coverage_func <- function(dates.test, agedist, rsv.smooth){
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
  
  rsv.smooth$smooth.rsv.scale <- rsv.smooth$smooth.rsv/max(rsv.smooth$smooth.rsv, na.rm=T)
  rsv.smooth <- rsv.smooth[order(rsv.smooth$date),]
  #pull out relevatn dates for RSv activity
  rsv.smooth2 <- rsv.smooth[rsv.smooth$date %in% all.dates,]
  rsv.smooth2 <- unique(rsv.smooth2)
  rsv.smooth2$rsv.smooth.scale <- rsv.smooth2$smooth.rsv/max(rsv.smooth2$smooth.rsv, na.rm=T)
  
  #scale age dist of risk
  agedist$scale.risk <- agedist$Age.Inc.Smooth/max(agedist$Age.Inc.Smooth, na.rm=T)
  
  #mat1 captures individual's risk at each day of life during follow up period
  mat1 <- matrix(0,nrow=nrow(date.df), ncol=365)
  
  for(i in 1: nrow(mat1)){
    aged.indv <- cbind.data.frame('aged'= as.numeric(all.dates - date.df$date.born[i] ))
    aged.indv$aged[aged.indv$aged<0 ] <- 0
    aged.indv <- merge(aged.indv, agedist, all=T, by='aged')
    aged.indv <- aged.indv[aged.indv$aged>0 & aged.indv$aged <180 & !is.na(aged.indv$aged) , ]
    mat1[i,all.dates > date.df$date.born[i] & all.dates< date.df$follow.period.end[i]] <- aged.indv$scale.risk  #is kid under observation at the date?
  }
  

  sum.indiv.risk <- apply(mat1,2,sum)
  sum.indiv.risk.scale <- sum.indiv.risk/max(sum.indiv.risk, na.rm=T)
  
  #Next need to multiply sum.indiv.risk  
  
  #what proportion of follow up occurs under the RSv curve?
  #diff1 <- sum.indiv.risk.scale[sum.indiv.risk.scale<rsv.smooth2$rsv.smooth.scale]
  prop.rsv.season <- sum(sum.indiv.risk.scale*(1-abs(rsv.smooth2$rsv.smooth.scale-sum.indiv.risk.scale)))/sum(sum.indiv.risk.scale)
  
  out.list <- list('all.dates'=all.dates,'start.vax.date'=start.vax.date,'end.vax.date'=end.vax.date,'prop.rsv.season'=prop.rsv.season,'sum.indiv.risk'=sum.indiv.risk, 'indiv.risk'=mat1,'rsv.smooth'=rsv.smooth )
  return(out.list)
  
}
