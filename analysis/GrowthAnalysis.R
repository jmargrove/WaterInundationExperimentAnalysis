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
data <- read.table("./data/Data_dia,height,leafnumber ofor biomass calculation.txt",
                   header = TRUE);
str(data);

##### function to calculate rgr 
Frgr <- function (t1, t2, t) ( (log(t2)) - (log(t1))) / t;

##### calculating the growth rates 
dia_byT <- matrix(data$dia, nrow = 800);
rgr_dia_mat <- foreach(i = 1:4,
                       .combine = cbind) %do%
  Frgr(t1 = dia_byT[, 0 + i], t2 = dia_byT[, 1 + i], t = 21);

Drgr <- as.vector(rgr_dia_mat);

##### converting to a data frame 
dt <- data[data$cenus != 4, ];
dt$Drgr <- Drgr * 30;

##### Modelling the data 
growth_model1 <- lmer(Drgr ~
                 dia +
                 treat +
                 (1 | cenus) +
                 (1 | sp / mother) +
                 (1 | block),
               data = dt)

summary(growth_model1)

# Some deviations from normallity but otherwise OK (for ecology data).
plot(growth_model1)

# Random effects reasonably normally distributed
ranNorm("mother", slope = 1, model = growth_model1)
ranNorm("block", slope = 1, model = growth_model1)
ranNorm("cenus", slope = 1, model = growth_model1)

##### predicting the growth rates across treatments 

growth_preds <- expand.grid(dia = mean(dt$dia, na.rm = TRUE),
                     treat = seq(from = 0, to = 21, length = 100))

growth_preds$rgr <- predict(growth_model1,
                     newdata = growth_preds,
                     type = "response",
                     re.form = NA)

ggplot(growth_preds, aes(x = treat, y = rgr)) + 
  geom_line() 

##### min growth rate, and max growth rate 
min(growth_preds$rgr)
max(growth_preds$rgr)

################################################################################
##### Growth preds for diameter 
grw_preds_dia <- expand.grid(dia = seq(from = min(dt$dia, na.rm = T),
                                      to = max(dt$dia, na.rm = T),
                                      length = 100),
                            treat = 9)
grw_preds_dia$rgr <- predict(growth_model1, 
                             newdata = grw_preds_dia,
                             type = "response",
                             re.form = NA)

ggplot(grw_preds_dia, aes(x = dia, y = rgr)) + geom_line()

################################################################################
# interaction between species and water inundation.
growth_model2 <- lmer(Drgr ~ sp * treat + dia + 
                        (1 | cenus) + 
                        (1 | mother) + 
                        (1 | block), 
                      data = dt)

# Some deviations from normallity but otherwise OK (for ecology data).
plot(growth_model2)

# Random effects reasonably normally distributed.
ranNorm("mother", slope = 1, model = growth_model2)
ranNorm("block", slope = 1, model = growth_model2)
ranNorm("cenus", slope = 1, model = growth_model2)
# summerise the model.
summary(growth_model2)
# Anova test the interaction.
car::Anova(growth_model2)
# Calculate the differenece in AIC. 
diff(AIC(growth_model1, growth_model2)[,2])

# extract coef from model.
coef <- fixef(growth_model2)

# Calculate fixed effect slopes for the species : treatment frequency.
slope_coef <- data.frame(sp = levels(dt$sp), 
                         rgr = c(coef[11], coef[11] + coef[13:length(coef)]))
# Remove row-names.
rownames(slope_coef) <- c()
# Print slopes to console.
slope_coef[order(-slope_coef$rgr), ]

