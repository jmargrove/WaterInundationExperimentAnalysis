##### Paper: water inundation sensitivity in dipterocarps
##### author: james margrove 
##### date: 29.01.2017


##### remove clutter 
rm(list=ls())

##### imported packaged 
require(ggplot2)
require(lme4)
require(foreach)
library("lmerTest")
source("./functions/ranNorm.R")

################################################################################
data <- read.table("Photosynthesis.txt",
                   header = TRUE);
str(data);


##### exploration 
ggplot(data, aes(x = treat, y = A)) + geom_point() + stat_smooth()

##### model addressing the hyporthesis, do dipterocarps generally show a decline
## in carbon aquisition with respect to water inundation. 
photo_model <- lmer(A ~ dia + treat + 
                      (1 | block) + 
                      (1 | sp / mother) + 
                      (1 | date), 
                    data = data)
##### model1 - judging from the exploration, log of the water inundation 
## frequency may be a better fit. 

photo_model2 <- lmer(A ~ dia + log(treat+1) + 
                       (1 | block) + 
                       (1 | sp / mother) + 
                       (1 | date), 
                     data = data)
summary(photo_model2)
aic_m1_m2 <- AIC(photo_model2, photo_model)
diff(aic_m1_m2[,2])
## show a differance in AIC of 6.45, so go with photo_model2

##### Evaluate the models. 

## fitted vs. residuals 
plot(photo_model2)
## no patterns in the residuals

ranNorm("mother:sp", slope = 1, model = photo_model2)
ranNorm("sp", slope = 1, model = photo_model2)
ranNorm("date", slope = 1, model = photo_model2)
ranNorm("block", slope = 1, model = photo_model2)

## generally the randome effects are looking OK


################################################################################

##### predicting the data values 

photo_preds <- expand.grid(dia = mean(data$dia, na.rm = TRUE), 
                          treat = seq(from = 0, to = 21, length = 100))

photo_preds$A <-predict(photo_model2, 
                        photo_preds, 
                        type = "response", 
                        re.form = NA)


##### graphing the data with ggplot2 
ggplot(photo_preds, aes(x = treat, y = A)) + geom_line()

head(photo_preds)
tail(photo_preds)






