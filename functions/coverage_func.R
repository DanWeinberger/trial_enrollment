coverage_func <- function(dates.test){
  start.vax.date <- dates.test[1]
  end.vax.date <- dates.test[2]
  length.vax.period <- as.numeric(end.vax.date - start.vax.date)
  
  date.vax <- start.vax.date + 
    sample(c(1:length.vax.period), size=1000, replace=T ) 
  
  #hist(date.vax, breaks=20)
  
  date.born <- date.vax + 45
  
  #date.risk.start <- date.born + 7*5 #risk period starts 5 weeks after birth
  date.risk.start <- date.born
  
  #follow.period.end <- date.born + 7*13 #risk period lowers after 13 weeks
  follow.period.end <- date.born + 180 #follow kids for 180 days
  
    
  date.df <- cbind.data.frame(date.born,date.risk.start,follow.period.end)
  date.df <- date.df[order(date.df$date.risk.start),]

  #to look at density, need to create a matrix with every day * N participants
  all.dates <- seq.Date(from=min(date.born), length.out = 365, by='day')
  
  #scale age dist of risk
  agedist1$scale.risk <- agedist1$Age.Inc.Smooth/max(agedist1$Age.Inc.Smooth, na.rm=T)
  
  #mat1 captures individual's risk at each day of life udring follow up period
  mat1 <- matrix(0,nrow=nrow(date.df), ncol=365)
  
  for(i in 1: nrow(mat1)){
    aged.indv <- cbind.data.frame('aged'= as.numeric(all.dates - date.df$date.born[i] ))
    aged.indv$aged[aged.indv$aged<0 ] <- 0
    aged.indv <- merge(aged.indv, agedist1, all=T, by='aged')
    aged.indv <- aged.indv[aged.indv$aged>0 & aged.indv$aged <180 & !is.na(aged.indv$aged) , ]
    mat1[i,all.dates > date.df$date.born[i] & all.dates< date.df$follow.period.end[i]] <- aged.indv$scale.risk  #is kid under observation at the date?
  }
  

  sum.indiv.risk <- apply(mat1,2,sum)
  
  #Next need to multiply sum.indiv.risk  
  prop.rsv.season <- sum(sum.indiv.risk[all.dates>=as.Date('2016-11-01') & all.dates <= as.Date('2017-03-01')])/ sum(sum.indiv.risk)
  
  out.list <- list('all.dates'=all.dates,'start.vax.date'=start.vax.date,'end.vax.date'=end.vax.date,'prop.rsv.season'=prop.rsv.season,'sum.indiv.risk'=sum.indiv.risk, 'indiv.risk'=mat1 )
  return(out.list)
  
}
