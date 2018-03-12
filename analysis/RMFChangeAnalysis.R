##########################################################################################
#' @title Root die back analysis 
#' @author James Margrove 
#' @description looking into the effect of water inundation specifically on the roots 


rm(list=ls())
require(ggplot2)
require(lme4)
require(car)
require(arm)
# predicting the mean biomass based on species 
BioData <- read.table("./data/initial_biomass.txt", header=T)
mrw_model <- lm(rootweight ~ sp, BioData)
AIC(mrw_model)
load("./models/rootbio_model.R")

initial_size <- read.table("./data/initial_plant_sizes.txt", header = TRUE)
final_biomass <- read.table("./data/final_biomass.txt", header = TRUE)
initial_size$initial_root <- predict(mrw_model, initial_size, type = "response")
data <- initial_size
data$Broot <- final_biomass$Broot

# Calculate the differenace in root biomass
data$Droot <- with(data, (log(Broot) - log(initial_root))/84)

# Import the scaled trait data from the mortality analysis
scaled_data <- read.table("./data/scaled_data.txt", header = T)
str(scaled_data)
scaled_data$Droot <- data$Droot

# Ok following the proceedure do any traits work 
scaled_data <- scaled_data[!is.na(data$Droot),]
#model <- lmer(Droot ~ diameter + 
                treat*si*lTb + treat*si*rTs + treat*si*dden + 
                treat*lTb*rTs + treat*lTb*dden + 
                treat*rTs*dden + 
                (1|block) + (1|sp/mother), 
                na.action = "na.pass",
               data = scaled_data)


summary(model)
require(MuMIn)
dredged_model <- dredge(model, rank = "AIC", trace = TRUE)
head(dredged_model, 20)

range(scaled_data$treat)
scaled_data$ftreat <- cut(scaled_data$treat, breaks = c(-2, 0, 2), labels = c("control", "treat"))

model <- lmer(Droot ~ diameter + treat +  dden + 
                (1|block) + (1|sp/mother), 
              na.action = "na.pass",
              data = scaled_data)

summary(model)

preds <- expand.grid(dden = seq(min(scaled_data$dden), max(scaled_data$dden), length = 100), 
                     treat = 0, 
                     diameter = mean(scaled_data$diameter))

preds$Droot <- predict(model, preds, type = "response", re.form = NA)

ggplot(preds, aes(x = dden, y = Droot)) + geom_line()

car::Anova(model)

r_dden <- range(scaled_data$dden)
scaled_data$fdden <- cut(scaled_data$dden, breaks = c((r_dden[1]-0.2), mean(scaled_data$dden), (r_dden[2]+0.2)), 
    labels = c("low", "high"))

r_treat <- range(scaled_data$treat)

scaled_data$ftreat<- cut(scaled_data$treat, breaks = c((r_treat[1]-0.2), -1.5, mean(scaled_data$treat),  (r_treat[2]+0.2)), 
                         labels = c("control", "lower", "higher"))

r_dia <- range(scaled_data$diameter)

scaled_data$fdia <- cut(scaled_data$diameter, breaks = c((r_dia[1]-0.2),  mean(scaled_data$diameter),  (r_dia[2]+0.2)), 
                         labels = c("small","large"))


model <- lmer(Droot ~ fdia * ftreat * fdden + 
                (1|block) + (1|sp/mother), 
              na.action = "na.pass",
              data = scaled_data)

summary(model)
car::Anova(model)
plot(model)

preds <- expand.grid(fdden = c("low", "high"), 
                     ftreat = c("control",  "lower", "higher") ,
                     fdia = c("small", "large"))


preds$Droot <- predict(model, preds, type = "response", re.form = NA)

ggplot(preds, aes(x = fdia, y = Droot, color = fdden)) + geom_line() + 
  facet_wrap(~ ftreat)

source("./functions/booter.R")
cols <- rev(c("#8CB369", "#F4E285","#F4A259", "#4C8577", "#8CB369"))
CI <- booter(model, data = scaled_data, preds = preds, n = 5000)
preds$CI025 <- CI[1,]
preds$CI975 <- CI[2,]
head(preds)
colnames(preds)[1] <- "wood density"
p1 <- ggplot(preds, aes(x = fdia, y = Droot, color = `wood density`)) +
  facet_wrap(~ ftreat) + 
  geom_errorbar(aes(ymin = CI025, ymax = CI975), width = 0.2, size = 1) + 
  geom_hline(yintercept = 0, linetype = 2, color = "#BC4B51") + 
  theme_classic() + 
  geom_point(size = 2) + 
  theme(legend.position = "top") + 
  ylab("root growth g g-1 day-1") + 
  xlab("seedling initial size") + 
  scale_color_manual(values = cols)

p1

ggsave(p1, file = "./graphs/root_dden_size_interactions.pdf", 
            width = 8, 
            height = 4.2)

