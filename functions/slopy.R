#####################################################################################
#'@title slopy: function to get slopes from a MEM
#'@author James Margrove
#'
#'
#'@param model what model
#'
#'@return slopes for treatment 
#'

slopy <- function(model){
  coefs <- fixef(model)
  if(!is.na(coefs["treat"])){
    return(c(coefs["treat"], coefs["treat"] + coefs[(length(coefs)-8):length(coefs)]))
  } else {
    return(c(coefs["log(treat + 1)"], coefs["log(treat + 1)"] + coefs[(length(coefs)-8):length(coefs)]))
  }
}
