##### Paper: water inundation sensitivity in dipterocarps
##### author: james margrove 
##### date: 29.01.2017
##### title: exploration of species reactions to flooding coeficents 

##### remove clutter 
rm(list=ls())

# import all the coef values from the species interaction section 
ASlopeCoef <- read.table("./ASlopeCoef.txt", header = TRUE)[, 2]
ESlopeCoeF <- read.table("./ESlopeCoef.txt", header = TRUE)[, 2]
psSlopeCoeF <- read.table("./psSlopeCoef.txt", header = TRUE)[, 2]
rgrSlopeCoeF <- read.table("./rgrSlopeCoef.txt", header = TRUE)[, 2]


data <- data.frame(A = ASlopeCoef, 
                   E = ESlopeCoeF, 
                   ps = psSlopeCoeF,
                   rgr = rgrSlopeCoeF)
# plot the data for these interactions. 
plot(data)

# Correlation between ps and rgr 
with(data, cor(ps, rgr))

# Linear model for survival and 
lm1 <- lm(ps ~ rgr, data = data)
summary(lm1)


### species that had their photosynthesis, and transperation reduced the most,
# where those that had theree

data$ASpInt <-read.table("./ASpIntCoef.txt", header = TRUE)[, 2]
data$ESpInt<- read.table("./ESpIntCoef.txt", header = TRUE)[, 2]

plot(data[,3:5])
# Photosynthesis linked to changes in growth or survival
lm2 <- lm(ps ~ ASpInt, data)
summary(lm2)
with(data, cor.test(ps, ASpInt))
lm3 <- lm(rgr ~ ASpInt, data)
summary(lm3)
with(data, cor.test(rgr, ASpInt))
?cor
# Transpiration linked to changes in growth or survival 
lm4 <- lm(ps ~ ESpInt, data)
summary(lm4)
lm5 <- lm(rgr ~ ESpInt, data)
summary(lm5)
