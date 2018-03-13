##########################################################################################
#' @title Root die back analysis 
#' @author James Margrove 
#' @description looking into the effect of water inundation specifically on the roots 

# Cear work space 
rm(list=ls())

# Import packages 
require(ggplot2)
require(lme4)
require(car)
require(arm)
require(MuMIn)

# predicting the mean biomass based on species 
BioData <- read.table("./data/initial_biomass.txt", header=T)
mrw_model <- lm(rootweight ~ sp, BioData)
initial_size <- read.table("./data/initial_plant_sizes.txt", header = TRUE)
final_biomass <- read.table("./data/final_biomass.txt", header = TRUE)
initial_size$initial_root <- (predict(mrw_model, initial_size, type = "response"))
data <- (initial_size)
data$Broot <- final_biomass$Broot

# Calculate the differenace in root biomass
data$Droot <- with(data, (log(Broot) - log(initial_root))/84)

# Import the scaled trait data from the mortality analysis
scaled_data <- read.table("./data/scaled_data.txt", header = T)
str(scaled_data)
scaled_data$Droot <- data$Droot

# Ok following the proceedure do any traits work 
scaled_data <- scaled_data[!is.na(data$Droot),]

# Full model with all interactions max tww traits 
model <- lmer(Droot ~ 
                diameter*treat*si*lTb + diameter*treat*si*rTs + diameter*treat*si*dden + 
                diameter*treat*lTb*rTs + diameter*treat*lTb*dden + 
                diameter*treat*rTs*dden + 
                (1|block) + (1|sp/mother),
                na.action = "na.pass",
                REML = F, 
                data = scaled_data)

# Dredge the model for interactions 
dredged_model <- dredge(model, rank = "AIC", trace = TRUE, 
                        fixed = ~ treat*si*lTb + treat*si*rTs + treat*si*dden + 
                          treat*lTb*rTs + treat*lTb*dden + 
                          treat*rTs*dden + diameter*si*lTb + diameter*si*rTs + diameter*si*dden + 
                          diameter*lTb*rTs + diameter*lTb*dden + 
                          diameter*rTs*dden)

# Comparing all the moddel with four way interactions 
model2 <- get.models(dredged_model1, subset = 1)[[1]]

dredged_model2 <- dredge(model2, rank = "AIC", trace = TRUE, 
                        fixed = ~ treat*diameter + treat*si + treat*dden + 
                          treat*lTb + treat*rTs + diameter*si + diameter*rTs + diameter*dden + 
                          diameter*lTb)

# The first round of dredging 
model3 <- get.models(dredged_model2, subset = 1)[[1]]
dredged_model3 <- dredge(model3, rank = "AIC", trace = TRUE)

# The second round of dredging 
model4 <- get.models(dredged_model3, subset = 1)[[1]]
head(dredged_model3)

# Yes it is the bets model in the top 4 AIC points 
model5 <- lmer(Droot ~ dden + diameter + si + treat + (1 | block) + (1 | sp/mother) +  
                 dden:diameter + dden:si + dden:treat + diameter:si + diameter:treat +  
                 si:treat + dden:diameter:si + dden:si:treat + diameter:si:treat, 
               data = scaled_data, REML = TRUE)

# Results 
summary(model5)
Anova(model5)

# OK, predict, the si:dden:treat interaction
preds1 <- expand.grid(diameter = quantile(scaled_data$diameter, c(0.025, 0.25, 0.5, 0.75, 0.975),  na.rm = T),
                      si = quantile(scaled_data$si, c(0.25, 0.75), na.rm = T), 
                      dden = quantile(scaled_data$dden, c(0.025, 0.25, 0.5, 0.75, 0.975), na.rm = T), 
                      treat = seq(min(scaled_data$treat, na.rm = T), max(scaled_data$treat, na.rm = T), length = 100))

preds1$Droot <- predict(model5, preds1, type = "response", re.form = NA)
source("./functions/booter.R")
CI <- booter(model = model5, 
             data = scaled_data, 
             preds = preds1, n = 100)

preds1$CI025 <- CI[1,]
preds1$CI975 <- CI[2,]
head(preds1)
cols <- rev(c("#8CB369", "#F4E285","#F4A259", "#4C8577", "#BC4B51"))
# Run the bets model 
p1 <- ggplot(preds1, aes(x = treat, y = Droot, fill = factor(dden))) + 
  facet_grid(si ~ diameter) + 
  geom_ribbon(aes(ymin = CI025, ymax = CI975), alpha = 1) + 
  geom_line() + 
  geom_hline(yintercept = 0, linetype = 2, color = "#BC4B51") + 
  theme_classic() + 
  scale_fill_manual(values = cols)


#####
ggsave(p1, file = "./graphs/RFM_growth_interaction.png", 
       width = 8, height = 6)

source("./functions/booter.R")
#### trying this as factors to make message clear
# Factor density 
r_dden <- range(scaled_data$dden)
scaled_data$fdden <- cut(scaled_data$dden, breaks = c((r_dden[1]-0.2), mean(scaled_data$dden), (r_dden[2]+0.2)), 
    labels = c("low wood density", "high wood density"))
# Factor SI
r_si <- range(scaled_data$si)
scaled_data$fsi <- cut(scaled_data$si, breaks = c((r_si[1]-0.2), mean(scaled_data$si), (r_si[2]+0.2)), 
                         labels = c("low", "high"))
# Factor treat
r_treat <- range(scaled_data$treat)
scaled_data$ftreat<- cut(scaled_data$treat, breaks = c((r_treat[1]-0.2), -1, mean(scaled_data$treat), (r_treat[2]+0.2)), 
                         labels = c("control", "lower",  "higher"))
# Factor diameter 
r_dia <- range(scaled_data$diameter)
scaled_data$fdia <- cut(scaled_data$diameter, breaks = c((r_dia[1]-0.2),  mean(scaled_data$diameter),  (r_dia[2]+0.2)), 
                         labels = c("small","large"))

head(scaled_data)
# Model full model 
model <- lmer(Droot ~ fdia * ftreat * fdden * fsi + 
                (1|block) + (1|sp/mother), 
              na.action = "na.pass",
              REML = F,
              data = scaled_data)

# Dreadge the full model
d <- dredge(model, rank = "AIC", trace = TRUE)
head(d)
# Extract best model 
model6 <- get.models(d, subset = 1)[[1]]
# switch back to REML
model7 <- lmer(Droot ~ fdden + fdia + fsi + ftreat + (1 | block) + (1 | sp/mother) +  
                  + fdden:fsi + fdden:ftreat + fdia:ftreat + fsi:ftreat +      
                 fdden:fsi:ftreat, 
               REML = F,
               data = scaled_data)
# Results 
summary(model7)
Anova(model7)

# Major significant interaction explaining root growth. 
preds2 <- expand.grid(fdden = c("low wood density", "high wood density"), 
                      fsi = c("high", "low"), 
                      ftreat = c("control", "lower", "higher"), 
                      fdia = c("small", "large"))

preds2$Droot <- predict(model7, preds2, type = "response", re.form = NA)

CI <- booter(model7, preds = preds2, data = scaled_data, n = 500)
preds2$CI025 <- CI[1,]
preds2$CI975 <- CI[2,]

# Graph 
p2 <- ggplot(preds2, aes(x = ftreat, y = Droot)) + 
  geom_errorbar(aes(ymin = CI025, ymax = CI975, width = 0.1), alpha = 0.5) + 
  facet_grid(fsi ~ fdden) + 
  geom_hline(yintercept = 0, linetype = 2, color= cols[1]) + 
  theme_classic() + 
  geom_line(aes(x = as.numeric(ftreat), y = Droot, color = fsi, fill = fdia)) + 
  geom_point(alpha = .8, aes(color = fsi, size =  fdia, shape = fdden)) + 
  scale_color_manual(values = c(cols[2], cols[3])) + 
  theme(legend.position = "top")
p2
# Save graphic 
ggsave(p2, file = "./graphs/Droot_fsi_fdia_ftreat_fdden.png", width = 8, height = 6)

# So the thing is this model still does not beat yesterdays three-way interaction
#scaled_data$ftreat <- factor(scaled_data$treat)
model8 <- lmer(Droot ~ fdden * fdia  * ftreat + (1 | block) + (1 | sp/mother), 
               REML = F,
               data = scaled_data)

# Calculate AIC
AIC(model7, model8)
#require(splines)

# Fit model with REML
source("./functions/booter.R")
model9 <- lmer(Droot ~ fdden * fdia  * ftreat + (1 | block) + (1 | sp/mother), 
               data = scaled_data)

Anova(model9)
# Prediction data frame 
preds3 <- expand.grid(fdden = levels(scaled_data$fdden), 
                     ftreat = levels(scaled_data$ftreat),
                     fdia = levels(scaled_data$fdia))

# Calculate the prediction values 
preds3$Droot <- predict(model9, preds3, type = "response", re.form = NA)
CI <- booter(model9, data = scaled_data, preds = preds3, n = 100)
preds3$CI025 <- CI[1,]
preds3$CI975 <- CI[2,]

# Rename the legend Wood density 
colnames(preds3)[1] <- "Wood density"
# Use good Colours 
cols <- rev(c("#8CB369", "#F4E285","#F4A259", "#4C8577", "#8CB369"))

# Create graph 
p1 <- ggplot(preds3, aes(x = ftreat, y = Droot, color = `Wood density`)) +
  facet_wrap(~ fdia) + 
  geom_errorbar(aes(ymin = CI025, ymax = CI975), width = 0.2, size = 1) + 
  geom_hline(yintercept = 0, linetype = 2, color = "#BC4B51") + 
  theme_classic() + 
  geom_point(size = 2) + 
  geom_line(aes(x = as.numeric(ftreat), y = Droot), linetype = 3) + 
  theme(legend.position = "top") + 
  ylab("Root growth g g-1 day-1") + 
  xlab("Treatment frequency") + 
  scale_color_manual(values = cols)

p1
# Save the graph 
ggsave(p1, file = "./graphs/root_dden_size_interactions.png", 
            width = 8, 
            height = 4.2)
