##########################################################################################
#' @title slopy_sd 
#' @author James Margrove
#' @description A function to calculate the standard deviation of the slope estimates 
#' 
#' 

source("./functions/slopy.R")
require(foreach)


# Calculate the photo_sd
slopy_sd <- function(model, n = 100){
  dt_p <- model@frame
  if(colnames(dt_p)[3] == "log(treat + 1)"){
    print("Unlogged!")
    colnames(dt_p)[3] <- "treat"
    dt_p$treat <- exp(dt_p$treat) - 1
  }
  booty <- function(){
    btm <- update(model, . ~ ., data = dt_p[sample(1:nrow(dt_p), replace = TRUE), ])
    as.vector(slopy(btm))
  }
  photo_sd_boots <- foreach(i = 1:n, .combine = cbind) %do% booty()
  apply(photo_sd_boots, 1, sd)
}
