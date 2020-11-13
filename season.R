#https://www.eurosurveillance.org/content/10.2807/1560-7917.ES.2018.23.5.17-00284?crawler=true
library(mgcv)

#week 1 here corresponds to week 40, which starts oct 8 in 2016
d1 <- read.csv('./ave_season_nl.csv', header=F)
names(d1) <- c('week','rsv')
d1 <- rbind.data.frame(d1, cbind.data.frame('week'=0, 'rsv'=0) )

d1$day <- d1$week*7
d1$rsv[d1$rsv<10] <- 0

d1 <- d1[order(d1$week),]

d2 <- cbind.data.frame('day'=1:365,rsv=rep(NA, 365) )

interpol <-approx(x=d1$day, y = d1$rsv,  method="linear",  xout=d2$day,
                  rule = 1, f = 0, ties = mean)

mod1 <- gam( rsv ~ s(day) , data=d1)
pred1 <- predict(mod1, newdata=d2)

d3 <- cbind.data.frame('day'=interpol$x, 'smooth.rsv'=pred1)
d3$smooth.rsv[d3$smooth.rsv<5] <- 0
d3$date <- as.Date('2016-07-01')
plot(d3$day, d3$smooth.rsv)

d3$date <- d3$day +as.Date('2016-10-08')

d3b <- d3
d3b$date <- d3b$day + as.Date('2015-10-10') 

d3c <- d3
d3c$date <- d3c$day + as.Date('2017-10-07') 

d4 <- rbind.data.frame(d3, d3b, d3c)
d4 <- d4[,c('date','smooth.rsv')]

saveRDS(d4,'./functions/data/rsv.smooth.rds')
