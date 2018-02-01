##### Paper: water inundation sensitivity in dipterocarps
##### author: james margrove 
##### date: 29.01.2017


rm(list=ls())

##### Packages required for this analysis 
require(ggplot2)
require(GGally)
require(car)
require(arm)
require(lme4)
require(doSNOW)
require(MuMIn)
source("./functions/newbinplot.R"); require(arm)
source(system.file("utils", "allFit.R", package = "lme4"))

##### Import data, mortality and trait data
data <- read.table("./data/newdata.txt", header = T)
trait <- read.table("./data/traits.txt", header = T)

# Add in the wood density variable 
mdden <- rep(NA,800) 
for(i in 1:10) mdden[which(data$sp == trait$sp[i])] <- trait$dden[i]
data$mdden <- mdden

# Investigate the covaiates for colinearity. 
ggpairs(data[,c("diameter","lTb","rTs","dden","slm","si")])
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

model <- glmer(surv ~ diameter + 
                 treat*si*lTb + treat*si*rTs + treat*si*dden + 
                 treat*lTb*rTs + treat*lTb*dden + 
                 treat*rTs*dden + 
                 (1|block) + (1|sp/mother), 
               data = data, 
               family = "binomial", 
               control = glmerControl(optimizer = "nlminbw"))

# model now converges but need to scale values 
scaled_data <- data.frame(surv = data$surv, 
                           block = data$block,
                           sp = data$sp,
                           mother = data$mother)

# Quick function to scale values 
scaler <- function(vec) {
  (vec - mean(vec, na.rm = T))/sd(vec)
}

# apply the function 
scaled_data$diameter <- scaler(data$diameter)
scaled_data$si <- scaler(data$si)
scaled_data$lTb <- scaler(data$lTb)
scaled_data$dden <- scaler(data$dden)
scaled_data$rTs <- scaler(data$rTs)
scaled_data$treat <- scaler(data$treat)


# Run model with scaled data
model2 <- glmer(surv ~ diameter + 
                 treat*si*lTb + treat*si*rTs + treat*si*dden + 
                 treat*lTb*rTs + treat*lTb*dden + 
                 treat*rTs*dden + 
                 (1|block) + (1|sp/mother), 
               data = scaled_data, 
               family = "binomial", 
               control = glmerControl(optimizer = "nlminbw"))

# traditional backwards model selection 
# removing variables by order of least significanse
model3 <- update(model2, . ~ . -treat:si:dden)
model4 <- update(model3, . ~ . -treat:si:lTb)
model5 <- update(model3, . ~ . -si:lTb)

summary(model4)
?update


AIC(model)
