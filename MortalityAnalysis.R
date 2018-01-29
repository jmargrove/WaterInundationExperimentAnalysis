##### Paper: water inundation sensitivity in dipterocarps
##### author: james margrove 
##### date: 26.01.2017

##### imported packaged 
require(ggplot2)
require(lme4)
source("./functions/newbinplot.R"); require(arm)
source("./functions/ranNorm.R")

data <- read.table("Experiment, mort, leafAB, dden, wden,sla.txt", header = TRUE);
str(data)

##### data exploration with ggplot. 
ggplot(data, aes(x=treat, y=surv)) + geom_point() + stat_smooth()

model1 <- glmer(surv ~ treat + dia + (1|sp/mother) + (1|block), data = data, family = "binomial")
summary(model1)

##### effect
# the effect of flooding overall on dipterocarps is negative. 
##### model validation.

##### residuals & fitted values 
residuals <- resid(model1, type =  "pearson");
fitted <- fitted(model1)

# Some deviations from normallity but otherwise OK 
newbinplot(residuals, fitted);

# Random effects reasonabbly normally distributed
ranNorm("mother:sp", slope = 1, model = model1)
ranNorm("sp", slope = 1, model = model1)
ranNorm("block", slope = 1, model = model1)

##### Graphing the analysis
preds <- expand.grid(dia = mean(data$dia), treat = seq(from = 0, to = 21, length = 100))
preds$p = predict(model1, type = "response", newdata = preds, re.form = NA)
head(preds)

##### Confidence intervals 
preds$CI <- predict(model1, newdata = preds, re.form = NA, se.fit = TRUE)$se.fit

ggplot(preds, aes(x=treat, y=p)) + geom_line() 

##### extremes of survival curve....
min(preds$p)
max(preds$p)
