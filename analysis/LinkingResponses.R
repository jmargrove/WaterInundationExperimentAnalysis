##### Paper: water inundation sensitivity in dipterocarps
##### author: james margrove 
##### date: 29.01.2017
##### title: exploration of species reactions to flooding coeficents 

##### remove clutter 
rm(list=ls())

# import all the coef values from the species interaction section 
ASlopeCoef <- read.table("./ASlopeCoef.txt", header = TRUE)[,2]
ESlopeCoeF <- read.table("./ESlopeCoef.txt", header = TRUE)[,2]
psSlopeCoeF <- read.table("./psSlopeCoef.txt", header = TRUE)[,2]
rgrSlopeCoeF <- read.table("./rgrSlopeCoef.txt", header = TRUE)[,2]


data <- data.frame(A = ASlopeCoef, 
                   E = ESlopeCoeF, 
                   ps = psSlopeCoeF,
                   rgr = rgrSlopeCoeF)
# plot the data for these interactions. 
plot(data)

