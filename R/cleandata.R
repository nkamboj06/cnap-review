##' @title Clean names 
##' @name cleandata
##' @param importdata imported data
##' @return
##' @author Aaron Conway
##' @export
##' @importFrom janitor clean_names
##' @importFrom stringr str_replace_all


cleandata <- function(importdata) {

  names(importdata) <- str_replace_all(names(importdata), "radial/femoral", "radialfemoral")
  importdata %>% 
    clean_names()

}
