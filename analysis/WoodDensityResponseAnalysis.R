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
# Import models 
load("./models/photo_model4.R")
load("./models/surv_model3.R")
load("./models/growth_model2.R")
# Import functions 
source("./functions/slopy.R")
source("./functions/inty.R")
source("./design/colours.R")
source("./functions/sss.R")
# Import data 
trait <- read.table("./data/traits.txt", header = TRUE)
trait$slope_photo <- slopy(photo_model4)
trait$int_photo <- inty(photo_model4)
trait$slope_surv <- slopy(surv_model3)
trait$slope_growth <- slopy(growth_model2)

# Now some exploration on wood density | seedling 
ggplot(trait, aes(x = dden, y = slope_surv)) + geom_point()
ggplot(trait, aes(x = dden, y = slope_growth)) + geom_point()
ggplot(trait, aes(x = log(dden), y = slope_growth)) + geom_point()
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

# Prediction dataframe 
pred_ds2 <- data.frame(dden = sss("dden", trait))
pred_ds2$surv <- predict(model_ds2, pred_ds2)
pred_ds2$CI <- predictSE(model_ds2, pred_ds2, se.fit = TRUE)$se.fit * 1.96
# text positioning 
hj <- rep(-0.4, 10)
hj[which(trait$sp == "sxan")] <- 1.2

# Graph
p1 <- ggplot(pred_ds2, aes(x = dden, y = surv)) + 
  geom_ribbon(aes(ymin = surv - CI, ymax = CI + surv), fill = cols[1], alpha = 0.65) + 
  geom_point(data = trait, aes(x = dden, y = slope_surv)) + 
  geom_hline(yintercept = 0, linetype = 2, color = cols[3]) + 
  theme_bw() + 
  geom_line() + 
  geom_text(data = trait, aes(x = dden, y = slope_surv, label = sp),  hjust = hj) + 
  ylab("Beta survival * species") + 
  xlab(bquote("Wood density g " ~ cm^-3))

p1

# Save graph 
ggsave(p1, file = "./graphs/surv_dden_gls.png", 
       width = 4, 
       height = 4)


#Growth analysis 
model_dg <- lm(slope_growth ~ dden, weights = 1/dden_sd, trait)
summary(model_dg)
plot(model_dg, which = 1)
plot(model_dg, which = 2)

# Photo analysis
model_dp <- lm(slope_photo ~ dden, weights = 1/dden_sd, trait)
summary(model_dp)
plot(model_dp, which = 1) # unequal variance again 
plot(model_dp, which = 2)

# gls model the unequal variance 
model_dp2 <- gls(slope_photo ~ dden, weights = varExp(form = ~ dden * 1/dden_sd), trait)
summary(model_dp2)
plot(model_ds2) # much better 

# And the background photosynthetic rate 
model_bpd <- lm(int_photo ~ dden, weights = 1/dden_sd, trait)
summary(model_bpd)
plot(model_bpd, which = 1)
plot(model_bpd, which = 2)

model_bpd2 <- gls(int_photo ~ dden, weights = varExp(form = ~ dden * 1/dden_sd), trait)
summary(model_bpd2)
