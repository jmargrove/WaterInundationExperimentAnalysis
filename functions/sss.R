#####################################################################################
#' @author James Margrove
#' @title sss
#' @param vec vector from a data frame 
#' @param data dataframe variable 
#' 
#' @return a min to max vector for prediction 

sss <- function(vec, data){
  seq(min(data[, vec], na.rm = T), 
      max(data[, vec], na.rm = T), 
      length = 100)
}