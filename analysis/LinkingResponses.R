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