source('coverage_func.R')
server <- function(input, output){
  output$countyPlot = renderPlot({
   
    cov.month <- coverage_func(as.Date(c(input$recruit.dates)))
    start.dates <- cov.month$start.vax.date
    end.dates <- cov.month$end.vax.date
    prop.rsv.season <- cov.month$prop.rsv.season
    
    
    rsv.dates <- cov.month$all.dates[cov.month$all.dates>=as.Date('2016-11-01') & cov.month$all.dates <=as.Date('2017-03-01')]
    prop.obs.rsv <- cov.month$prop.obs[cov.month$all.dates>=as.Date('2016-11-01') & cov.month$all.dates <=as.Date('2017-03-01')]
     plot(cov.month$all.dates,cov.month$prop.obs, type='l', 
          xlim=as.Date(c('2016-06-01', '2017-05-31')),
          main= paste0(round(cov.month$prop.rsv.season*100),'% of f.u. during RSV season'),
          sub=paste0('Vaccinate ' ,cov.month$start.vax.date,' - ', cov.month$end.vax.date))
     arrows(x0=as.Date('2016-11-01'), x1=as.Date('2017-03-01'), y0=0, col='red', lwd=3, length=0, ylab='N people under observaiton at each date')
     polygon( c(cov.month$all.dates, rev(cov.month$all.dates) ), 
              c(rep(0, times=length(cov.month$prop.obs)),rev(cov.month$prop.obs)), 
              col=rgb(0,0,1, alpha=0.2) , border=NA )
     polygon( c(rsv.dates, rev(rsv.dates) ), 
              c(rep(0, times=length(rsv.dates)),rev(prop.obs.rsv)) , 
              col=rgb(1,0,0, alpha=0.2) , border=NA ) 
     
  } ,
  
  width = "auto",
  height = "auto"
  
  )}

si <- shiny::selectInput

ui <- shiny::fluidPage(
  shiny::sidebarLayout(
    shiny::sidebarPanel(
       
      shiny::sliderInput(
        'recruit.dates',
        'Date range to vaccinate mothers',
        min=as.Date(c('2016-05-01')),
        max=as.Date(c('2016-12-01')),
        step=7,
        value=as.Date(c('2016-07-01', '2016-08-01'))
      )
      
       ),
    
    shiny::mainPanel(
      shiny::plotOutput("countyPlot"),
      shiny::column(
        8,
        align = 'justify',
        shiny::hr(),
        shiny::span(""),
        shiny::hr(),
        shiny::span("This app and package were developed by Dan Weinberger of The Public Health Modeling Unit at Yale School of Public Health.")
      )
    )
  )
  
)

shiny::shinyApp(ui, server)
