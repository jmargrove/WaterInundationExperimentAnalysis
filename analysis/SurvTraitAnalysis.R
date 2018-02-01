##### Paper: water inundation sensitivity in dipterocarps
##### author: james margrove 
##### date: 29.01.2017


rm(list=ls())

data <- read.table("./data/newdata.txt", header = T)
trait <- read.table("./data/traits.txt", header = T)
mdden <- rep(NA,800)
for(i in 1:10) mdden[which(data$sp == trait$sp[i])] <- trait$dden[i]
data$mdden <- mdden


require(ggplot2)
require(GGally)
ggpairs(data[,c("diameter","lTb","rTs","dden","slm","si")])
require(car)
vif(glm(surv ~ diameter + lTb + rTs + dden + slm + si + treat, family = "binomial", data))
# the three leaf traits lTb slm and si are colinear in the VIF 
vif(glm(surv ~ diameter + lTb + rTs + dden + si + treat, family = "binomial", data))
# remove slm and all is ok 
vif(glm(surv ~ diameter + rTs + dden + si + slm + treat, family = "binomial", data))
# not OK 
vif(glm(surv ~ diameter + scale(rTs) + scale(dden) + scale(lTb) + slm + treat, family = "binomial", data))
# according to the ltb and slm are co-linear 
ggplot(trait,aes(x=leafTbio,y=slm)) + geom_point() + stat_smooth(method = lm)
# so we find that lTb is a better variable, just remeber that is relates to slm as well 
