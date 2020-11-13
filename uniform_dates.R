#5-13 weeks is key period of protection
#--two curves of interest: age-dependent incidence and age-dependent
# Add option to select age distribution of risk (uniform, high income, low income)


#TODO: calculate % of follow up time that occurs during RSV season
source('./functions/coverage_func.R')

dates.test2 <- list(
  #2 month windows
  as.Date(c('2016-06-01','2016-07-31')),
  as.Date(c('2016-07-01','2016-08-31')), 
  as.Date(c('2016-08-01','2016-09-30')), 
  as.Date(c('2016-09-01','2016-10-31')), 
  as.Date(c('2016-10-01','2016-11-30')), 
  as.Date(c('2016-11-01','2016-12-31'))
)

dates.test3 <- list(
  #3 month windows
  as.Date(c('2016-06-01','2016-08-31')),
  as.Date(c('2016-07-01','2016-09-30')), 
  as.Date(c('2016-08-01','2016-10-31')), 
  as.Date(c('2016-09-01','2016-11-30')), 
  as.Date(c('2016-10-01','2016-12-31')) 
)

dates.test4 <- list(
  #4 month windows
  as.Date(c('2016-06-01','2016-09-30')),
  as.Date(c('2016-07-01','2016-10-31')), 
  as.Date(c('2016-08-01','2016-11-30')), 
  as.Date(c('2016-09-01','2016-12-31'))
)          
  #as.Date(c('2016-08-15','2016-09-01')),
  #as.Date(c('2016-05-15','2016-12-31')),
  #as.Date(c('2016-08-15','2016-12-31')),
  #as.Date(c('2016-06-01','2016-08-31'))



cov.2month <- lapply(dates.test2,coverage_func)
cov.3month <- lapply(dates.test3,coverage_func)
cov.4month <- lapply(dates.test4,coverage_func)
cov.all <- c(cov.2month,cov.3month,cov.4month)

start.dates <- as.Date(sapply(cov.all, '[[', 'start.vax.date'), origin = as.Date('1970-01-01'))
end.dates <- as.Date(sapply(cov.all, '[[', 'end.vax.date'), origin = as.Date('1970-01-01'))
prop.rsv.season <- sapply(cov.all, '[[', 'prop.rsv.season')

summary.table <- cbind.data.frame(start.dates,end.dates ,prop.rsv.season)
summary.table$month.recruitment <- round((summary.table$end.dates - summary.table$start.dates)/30.3) 
summary.table <- summary.table[order(-summary.table$prop.rsv.season),]



# plot(all.dates,prop.obs, type='l', 
#      xlim=as.Date(c('2016-06-01', '2017-05-31')),
#      main= paste0(round(prop.rsv.season*100),'% of f.u. during RSV season'),
#      sub=paste0('Vaccinate ' ,start.vax.date,' - ', end.vax.date))
# arrows(x0=as.Date('2016-11-01'), x1=as.Date('2017-03-01'), y0=0, col='red', lwd=3, length=0, ylab='N people under observaiton at each date')
# 


