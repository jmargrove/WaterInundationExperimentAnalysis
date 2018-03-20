#####################################################################################
#' @title Pairing responses together  
#' @author James Margrove 

# Clear workspace 
rm(list=ls())
Sys.setenv(LANG = "en")

# Import data 
trait <- read.table("./data/traits.txt", header = TRUE)

# Import Model 
load("./models/growth_model2.R")
load("./models/surv_model3.R")
load("./models/photo_model4.R")

# Import functions and packages 
source("./functions/slopy.R")
source("./functions/inty.R")
source("./functions/aovPerVar.R")
source("./design/colours.R")

# construct data frame to investigate these variables 
data <- data.frame(sp = trait$sp)
data$slope_photo <- slopy(photo_model4)
data$slope_growth <- slopy(growth_model2)
data$slope_surv <- slopy(surv_model3)
data$int_photo <- inty(photo_model4)
data$int_photo <- inty(photo_model4)

# Model these data 
# Growth and Survival first 
model_sg <- (lm(slope_surv ~ slope_growth, data))
summary(model_sg)
plot(model_sg, which = 1)

# Prediction data frame 
pred_sg <- expand.grid(slope_growth = seq(min(data$slope_growth), 
                                          max(data$slope_growth), 
                                          length = 100))

# Predict 
pred_sg$surv <- predict(model_sg, pred_sg, type = "response")

# Adjusting the geom_text so that it is not overlapping 
hj <-  rep(-0.4, 10)
hj[which(data$sp == "ssem")] <- 1.2
hj[which(data$sp == "dry")] <- 1.2

# Graph 
p1 <- ggplot(data, aes(x = slope_growth, y = slope_surv)) + 
  geom_hline(yintercept = 0, color = "#C5283D", linetype = 2) + 
  geom_vline(xintercept = 0, color = "#C5283D", linetype = 2) +  
  geom_point() + 
  stat_smooth(method = lm, size = 0.2, color = "black", fill = "#255F85", alpha = 0.65) + 
  geom_line(data = pred_sg, inherit.aes = F, aes(x = slope_growth, y = surv)) + 
  theme_bw() + 
  xlab("Beta growth * species") + 
  ylab("Beta survival * species") + 
  geom_text(label = "ANOVA: 61.1", inherit.aes = F, aes(x = -0.015, y = 0.04)) + 
  geom_text(aes(label = sp, hjust = hj)) 

p1
# Save the plot 
ggsave(p1, file = "./graphs/slopes_surv_growth.png", 
       width = 4, height = 4)

# Model photosynthesis - exploration showed that it was important to account for 
# species differances in photosynthesis, :)
# Model 
model_spp <- lm(slope_surv ~ slope_photo + int_photo, data)
summary(model_spp)
aovPerVar(model_spp)
# Vadlid?
plot(model_spp, which = 2)

# Prediction of the Survival on the slope change - data frame 
pred_spp <- expand.grid(slope_photo = seq(min(data$slope_photo), 
                                          max(data$slope_photo), 
                                          length = 100),
                        int_photo = mean(data$int_photo))

# Predict
pred_spp$surv <- predict(model_spp, pred_spp, type = "response")  
pred_spp$CI <- predict(model_spp, pred_spp, type = "response", se.fit = T)$se.fit *1.96  

# adjust the text lables 
hj <- rep(-0.1, 10)

vj <- rep(-0.2, 10)
hj[which(trait$sp == "ssem")] <- 1.1
vj[which(trait$sp == "sjoh")] <- 0.4
vj[which(trait$sp == "sxan")] <- 0.7
vj[which(trait$sp == "ssem")] <- -0.4

# Graph the data 
p2 <- ggplot(pred_spp, aes(x = slope_photo, y = surv, fill = factor(int_photo))) + 
  geom_ribbon(aes(ymin = surv - CI, ymax = CI + surv), alpha = 0.65) + 
  geom_line() + 
  theme_bw() + 
  scale_fill_manual(values = cols) +  
  geom_point(data = data, inherit.aes = F, aes(x = slope_photo, y = slope_surv)) + 
  xlab("Beta photo_slope  * species") + 
  ylab("Beta survival * species") + 
  geom_text(label = "ANOVA: 48.5 %", aes(x = -0.75, y = 0.225)) +
  geom_text(data = data, inherit.aes = F, 
            aes(x = slope_photo, y = slope_surv, label = sp, hjust = hj, vjust = vj)) +
  geom_hline(yintercept = 0, color = "#C5283D", linetype = 2) + 
  geom_vline(xintercept = 0, color = "#C5283D", linetype = 2) +
  theme(legend.position = "none")

p2

ggsave(p2, file = "./graphs/slopes_photo_surv.png", 
       width = 4, height = 4)

# Model the growth rates as a function of the slope_photo and int_photo
model_gpp <- (lm(slope_growth ~ slope_photo + int_photo, data))
plot(model_gpp, which = 1)
summary(model_gpp)
car::Anova(model_gpp)
aovPerVar(model_gpp)

# Prediction of the growth on the slope change
pred_gpp <- expand.grid(slope_photo = seq(min(data$slope_photo), 
                                          max(data$slope_photo), 
                                          length = 100),
                        int_photo = mean(data$int_photo))

# Predict
pred_gpp$growth <- predict(model_gpp, pred_gpp, type = "response")  
pred_gpp$CI <- predict(model_gpp, pred_gpp, type = "response", se.fit = T)$se.fit *1.96  

# Graph the data 
p3 <- ggplot(pred_gpp, aes(x = slope_photo, y = growth, fill = factor(int_photo))) + 
  geom_ribbon(aes(ymin = growth - CI, ymax = CI + growth), alpha = 0.65) + 
  geom_line() + 
  theme_bw() + 
  scale_fill_manual(values = cols) +  
  geom_point(data = data, inherit.aes = F, aes(x = slope_photo, y = slope_growth)) + 
  xlab("Beta photo_slope  * species") + 
  ylab("Beta survival * species") + 
  geom_text(label = "ANOVA: 41.9 %", aes(x = -0.75, y = 0.007)) +
  geom_text(data = data, inherit.aes = F, 
            aes(x = slope_photo, y = slope_growth, label = sp, hjust = hj, vjust = vj)) +
  geom_hline(yintercept = 0, color = "#C5283D", linetype = 2) + 
  geom_vline(xintercept = 0, color = "#C5283D", linetype = 2) +
  theme(legend.position = "none")

p3

# Save the plot
ggsave(p3, file = "./graphs/slopes_photo_growth.png", 
       width = 4, height = 4)

