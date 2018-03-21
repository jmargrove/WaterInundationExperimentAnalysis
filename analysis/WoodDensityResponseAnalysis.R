##########################################################################################
#' @title Wood densities roll in the tollerance of species distributions 
#' @author James margrove 
#'

Sys.setenv(LANG = "en")
# Clear workspace 
rm(list=ls())
# Import packages 
require(ggplot2)
require(nlme)
require(AICcmodavg)

install.packages("AICcmodavg", dependencies = TRUE)



# Import models 
load("./models/photo_model4.R")
load("./models/surv_model3.R")
load("./models/growth_model2.R")
# Import functions 
source("./functions/slopy.R")
source("./functions/inty.R")
source("./design/colours.R")

sss <- function(vec, data){
  seq(min(data[, vec], na.rm = T), 
      max(data[, vec], na.rm = T), 
      length = 100)
}

# Import data 
trait <- read.table("./data/traits.txt", header = TRUE)
trait$slope_photo <- slopy(photo_model4)
trait$int_photo <- inty(photo_model4)
trait$slope_surv <- slopy(surv_model3)
trait$slope_growth <- slopy(growth_model2)

# Now some exploration on wood density | seedling 
ggplot(trait, aes(x = dden, y = slope_surv)) + geom_point()
ggplot(trait, aes(x = dden, y = slope_growth)) + geom_point()
ggplot(trait, aes(x = dden, y = slope_photo)) + geom_point()
ggplot(trait, aes(x = dden, y = int_photo)) + geom_point()

#interesting how adult wood density looks not to bad 
model_ds <- lm(slope_surv ~ dden, weights = 1/dden_sd, trait)
summary(model_ds)
plot(model_ds, which = 1) # quite clearly we have an increase in variance 
plot(model_ds, which = 2) # but is normally distributed 

# So 2 ways to model this - log or weighting 
model_ds2 <- gls(slope_surv ~ dden, weights = varExp(form = ~ dden * 1/dden_sd), trait)
summary(model_ds2)
plot(model_ds2) # much better 

# Prediction data frame 
pred_ds2 <- data.frame(dden = sss("dden", trait))
pred_ds2$surv <- predict(model_ds2, pred_ds2)
AICcmodavg::predictSE(model_ds2, pred_ds2, se.fit = TRUE)
?predict
# text positioning 
hj <- rep(-0.4, 10)
hj[which(trait$sp == "sxan")] <- 1.2

# Graph
p1 <- ggplot(pred_ds2, aes(x = dden, y = surv)) + 
  geom_line() + 
  geom_point(data = trait, aes(x = dden, y = slope_surv)) + 
  geom_hline(yintercept = 0, linetype = 2, color = cols[3]) + 
  theme_bw() + 
  geom_text(data = trait, aes(x = dden, y = slope_surv, label = sp),  hjust = hj)

p1

install.packages("AICcmodavg")

?geom_text
trait[8, "adden_sd"] <- max(trait$dden_sd) 
trait[3, "adden_sd"] <- max(trait$dden_sd)

summary(lm(int_photo ~  lmf, trait))
summary(lm(int_photo ~  slm, trait))
summary(lm(int_photo ~  si, trait))

summary(lm(slope_photo ~  lmf, trait))
summary(lm(slope_photo ~  slm, trait))
summary(lm(slope_photo ~  si, trait))

summary(lm(lmf ~  slm, trait))




model_pr <- (lm(log(slope_photo*-1) ~ log(rmf), trait))
pred_pr <- data.frame(rmf = seq(min(trait$rmf), 
                                max(trait$rmf), length = 100))
pred_pr$photo <- predict(model_pr, pred_pr, type = "response")
pred_pr$CI <- predict(model_pr, pred_pr, type = "response", se.fit = T)$se.fit * 1.96

ggplot(pred_pr, aes(x = rmf, y = (photo)*-1)) + geom_line() + 
  geom_ribbon(aes(ymin = (photo - CI)*-1, ymax = (photo + CI)*-1), alpha= 0.1) + 
  

ggplot(trait, aes(x = log(rmf), y = log(slope_photo*-1))) + geom_point() + 
  geom_text(aes(label = sp)) + 
  stat_smooth(method = lm) 

ggplot(trait, aes(x = dden, y = log(slope_surv+0.41))) + geom_point() + 
  stat_smooth(method = lm, size = 0.5) + 
  theme_bw()

# Wood density 
ggplot(trait, aes(x = dden, y = log(slope_surv+0.41))) + geom_point() + 
  stat_smooth(method = lm, size = 0.5) + 
  theme_bw()

log(trait$dden_sd)*-1

model_den_sur <- (lm(log(slope_surv+0.41) ~  dden, trait, weights = 1/dden_sd))
pred_ds <- data.frame(dden = seq(min(trait$dden), max(trait$dden), length = 100))
pred_ds$surv <- predict(model_den_sur, pred_ds, type = "response")
pred_ds$CI <- predict(model_den_sur, pred_ds, type = "response", se.fit = TRUE)$se.fit * 1.96

require(nlme)
model_dd <- gls(slope_ ~ dden, weights = varExp(form = ~ dden), trait)
summary(model_dd)
plot(model_dd)

pred_ds <- data.frame(dden = seq(min(trait$dden), max(trait$dden), length = 100))
pred_ds$surv <- predict(model_dd, pred_ds, type = "response")
pred_ds$CI <- predict(model_dd, pred_ds, type = "response", se.fit = TRUE)$se.fit * 1.96





ggplot(pred_ds, aes(x = dden, y = exp(surv)-0.41)) + 
  geom_ribbon(aes(ymin = exp(surv - CI) - 0.41, ymax = exp(surv + CI) - 0.41), 
              fill = cols[1], alpha = 0.65) + 
  geom_line() + 
  theme_bw() + 
  xlab("wood density g cm-3") + 
  geom_point(data = trait, aes(x = dden, y = slope_surv))


summary(lm(log(slope_surv+0.41) ~  si, trait, weights = 1/si_sd))

# And survival   
ggplot(trait, aes(x = slm, y = slope_surv)) + geom_point()
