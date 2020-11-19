library(readxl)
library(mgcv)
library(zoo)
library(EnvStats)

d1 <- readxl::read_excel('../DONTSYNC/RESCEUbc_RSVadmissions.xlsx')

names(d1) <- c('agew', 'active','passive','total')
plot(d1$agew, d1$total, type='l')

mystart <- list(shape=1, scale=10)
d.indiv <- readxl::read_excel('./DONTSYNC/RESCEUbc_RSVadmissions.xlsx', sheet='RESCEUbc_RSVadmissions')

ages <- d.indiv$`age_week (floored)`
ages <- ages[!is.na(ages)]

fw <- fitdist((ages+0.1), "weibull")
#fw.g <- fitdist((ages+0.1), "gamma")

#plot the distribution
curve(dweibull(x, shape=fw$estimate['shape'], scale = fw$estimate['scale']), from=1e-10, to=52)

x <- seq(0,104, by=1/7)
z<-dweibull(x,  shape=fw$estimate['shape'], scale = fw$estimate['scale']  )
z.g <- dgamma(x,  shape=fw.g$estimate['shape'], rate = fw.g$estimate['rate']  )
plot(z, type='l')
points(z.g, type='l', col='red')


z2 <- cbind.data.frame(aged=seq(1:length(z)),'Age.Inc.Smooth'=z )
saveRDS(z2,'./functions/data/smooth.age.dist.resceu.rds')
