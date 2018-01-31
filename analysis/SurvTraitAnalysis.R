##### Paper: water inundation sensitivity in dipterocarps
##### author: james margrove 
##### date: 29.01.2017


rm(list=ls())

data <- read.table("newdata.txt", header = T)
trait <- read.table("traits.txt", header = T)
mdden <- rep(NA,800)
for(i in 1:10) mdden[which(data$sp == trait$sp[i])] <- trait$dden[i]
data$mdden <- mdden