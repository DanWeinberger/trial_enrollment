library(mgcv)
d1 <- read.csv('ave.age.TN.NY.OH.csv', header=F)
names(d1) <-c('agem','inc')
d1$agem <- round(d1$agem) + 0.5
d1$aged <- round((d1$agem)*30.4)
d1$aged2 <- d1$aged
d1$aged[d1$aged<106] <- NA
mod1 <- gam(inc ~ s(aged, bs='tp'), data=d1)


d2 <- cbind.data.frame('aged'=1:(365*2))


#Need to do a simple interpolation or first 3 months, ten the gam smooth for rest of data
d2$pred1<-predict(mod1, newdata=d2)

d2 <- merge(d2, d1, by='aged', all=T)
d2$aged2 <- d2$aged

train <- d1
zeros <- cbind.data.frame('agem'=0, 'aged2'=0, 'aged'=0,'inc'=0)
train <- rbind.data.frame(zeros,train )
train$inc[train$agem==3.5] <- d2$pred1[d2$agem==3.5 & !is.na(d2$agem)]

interpol <-approx(x=train$aged2, y = train$inc,  method="linear",  xout=d2$aged2,
                         rule = 1, f = 0, ties = mean)


d2$interpol <- interpol$y
#d2 <- d2[!is.na(d2$aged2),]
d2$Age.Inc.Smooth <- d2$interpol
d2$Age.Inc.Smooth[d2$aged >=106 & !is.na(d2$pred1)] <- d2$pred1[d2$aged >=106 & !is.na(d2$pred1)]

plot(d2$aged, d2$Age.Inc.Smooth, type='l', ylim=range(d1$inc))
points(d2$aged, d2$inc)


d3 <- d2[, c('aged','Age.Inc.Smooth')]

saveRDS(d3,'./data/smooth.age.dist.rds')


