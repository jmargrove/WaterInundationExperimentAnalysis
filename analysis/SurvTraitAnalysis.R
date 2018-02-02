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
source("./functions/ranNorm.R"); require(arm)

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
                 (1|block) + (1|sp/mother), 
               data = scaled_data, 
               family = "binomial", 
               na.action = 'na.fail',
               control = glmerControl(optimizer = "nlminbw"))

# traditional backwards model selection 
# removing variables by order of least significanse
model3 <- update(model2, . ~ . + treat*si*lTb + treat*si*rTs)
dredge(model3)

model4 <- update(model3, . ~ . -treat:si:lTb)
model5 <- update(model3, . ~ . -si:lTb)

model2 <- glmer(surv ~ diameter + 
                  treat*si*lTb + treat*si*rTs + treat*si*dden + 
                  treat*lTb*rTs + treat*lTb*dden + 
                  treat*rTs*dden + 
                  (1|block) + (1|sp/mother), 
                data = scaled_data, 
                family = "binomial", 
                na.action = "na.pass",
                control = glmerControl(optimizer = "nlminbw"))

## the model matix gets rank defincient and drops 2 perameters to be estimated.
## this needs to be looked at in more detail. Find out what they are, and re do
## analysis. 

## has dropped two coef 
dredged_model <- dredge(model2, rank = "AIC", trace = TRUE)
#save(dredged_model, file = "surv_trait_model.R")
load("surv_trait_model.R")
head(dredged_model, 5)
# the mode with the lowest used degrees of freedom, for the top models within 
# 2 AIC points of the best model has a two three way interactions with 
# treat:dden:rts and treat:lTb:si 
model <- glmer(surv ~ diameter + 
                 treat*si*lTb + 
                 treat*rTs*dden + 
                 (1|block) + (1|sp/mother), 
               data = scaled_data, 
               family = "binomial", 
               na.action = "na.pass",
               control = glmerControl(optimizer = "nlminbw"))

summary(model)


################################################################################
# Model evaluation 

# residuals & fitted values.
model_residuals <- resid(model, type =  "pearson");
model_fitted <- fitted(model)

# Some deviations from normallity but otherwise OK (for ecology data).
par(mfrow=c(1,3))
newbinplot(y = model_residuals, x = model_fitted);

# Random effects reasonably normally distributed
ranNorm("sp", slope = 1, model = model)
# species does nothing...
ranNorm("mother:sp", slope = 1, model = model)
# mother:sp fine
ranNorm("block", slope = 1, model = model)
# fine 

