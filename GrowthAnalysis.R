##### Paper: water inundation sensitivity in dipterocarps
##### author: james margrove 
##### date: 29.01.2017

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
dt[1, ]# per 30 day month ;)

##### Modelling the data 

model1 <- lmer(Drgr ~
                 dia +
                 treat +
                 (1 | cenus) +
                 (1 | sp / mother) +
                 (1 | block),
               data = dt)
summary(model1)

##### predicting the growth rates across treatments 

preds <- expand.grid(dia = mean(dt$dia, na.rm = TRUE),
                     treat = seq(from = 0, to = 21, length = 100))
preds$rgr <- predict(model1,
                     newdata = preds,
                     type = "response",
                     re.form = NA)

conf <- predict(model1, newdata = preds, se.fit = TRUE, re.form = NA)
str(conf)

head(preds)
ggplot(preds, aes(x = treat, y = rgr)) + 
  geom_line() + 
  geom_ribbon(aes(ymin=rgr-conf, ymax = rgr + conf))
  

##### min growth rate, and max growth rate 
min(preds$rgr)
max(preds$rgr)
