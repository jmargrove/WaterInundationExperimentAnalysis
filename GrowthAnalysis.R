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
################################################################################
data <- read.table("Data_dia,height,leafnumber ofor biomass calculation.txt",
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




log(5) - log(2.5)/ time 

