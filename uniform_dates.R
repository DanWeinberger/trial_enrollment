#TODO: calculate % of follow up time that occurs during RSV season

dates.test <- list(
  as.Date(c('2016-08-15','2016-09-01')),
  as.Date(c('2016-05-15','2016-12-31')),
  as.Date(c('2016-08-15','2016-12-31'))

)

for(k in 1:length(dates.test)){
start.vax.date <- dates.test[[k]][1]
end.vax.date <- dates.test[[k]][2]
length.vax.period <- as.numeric(end.vax.date - start.vax.date)

date.vax <- start.vax.date + 
  sample(c(1:length.vax.period), size=1000, replace=T ) 

hist(date.vax, breaks=20)

date.born <- date.vax + 45

follow.period.end <- date.born +180

date.df <- cbind.data.frame(date.born,follow.period.end)
date.df <- date.df[order(date.df$date.born),]
plot(y=1:length(date.df$date.born), x=date.df$date.born , col='white', xlim=c(min(date.df$date.born), min(date.df$date.born)+365 ))
arrows(x0=date.df$date.born, x1=date.df$follow.period.end, 
       y0=1:length(date.df$date.born), 
       length=0,
       col=rgb(0,0,0,alpha=0.1))

#to look at density, need to create a matrix with every day * N participants
all.dates <- seq.Date(from=min(date.born), length.out = 365, by='day')

mat1 <- matrix(0,nrow=nrow(date.df), ncol=365)

for(i in 1: nrow(mat1)){
  mat1[i,] <- all.dates >= date.df$date.born[i] & all.dates <= date.df$follow.period.end[i]
}
prop.obs <- apply(mat1,2,sum)

plot(all.dates,prop.obs, type='l', main='N people under observaiton at each date')
arrows(x0=as.Date('2016-11-01'), x1=as.Date('2017-03-01'), y0=0, col='red', lwd=3, length=0)
}