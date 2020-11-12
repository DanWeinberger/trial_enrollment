coverage_func <- function(dates.test){
  start.vax.date <- dates.test[1]
  end.vax.date <- dates.test[2]
  length.vax.period <- as.numeric(end.vax.date - start.vax.date)
  
  date.vax <- start.vax.date + 
    sample(c(1:length.vax.period), size=1000, replace=T ) 
  
  #hist(date.vax, breaks=20)
  
  date.born <- date.vax + 45
  
  date.risk.start <- date.born + 7*5 #risk period starts 5 weeks after birth
  
  follow.period.end <- date.born + 7*13 #risk period lowers after 13 weeks
  
  date.df <- cbind.data.frame(date.risk.start,follow.period.end)
  date.df <- date.df[order(date.df$date.risk.start),]

  #to look at density, need to create a matrix with every day * N participants
  all.dates <- seq.Date(from=min(date.risk.start), length.out = 365, by='day')
  
  mat1 <- matrix(0,nrow=nrow(date.df), ncol=365)
  
  for(i in 1: nrow(mat1)){
    mat1[i,] <- all.dates >= date.df$date.risk.start[i] & all.dates <= date.df$follow.period.end[i]
  }
  prop.obs <- apply(mat1,2,sum)
  
  prop.rsv.season <- sum(prop.obs[all.dates>=as.Date('2016-11-01') & all.dates <= as.Date('2017-03-01')])/ sum(prop.obs)
  
  out.list <- list('all.dates'=all.dates,'start.vax.date'=start.vax.date,'end.vax.date'=end.vax.date,'prop.rsv.season'=prop.rsv.season,'prop.obs'=prop.obs )
  return(out.list)
  
}
