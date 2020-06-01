##' @title Make a prisma diagram
##' @name make_prisma
##' @importFrom ggprisma ggprisma
##' @return
##' @export
make_prisma <- function() {

  ggprisma::ggprisma(retrieved = 1710, included = 19, 
                     duplicates = 858, full_text = 48, 
                     wrong_setting = 20, 
                     wrong_population= 7, 
                     insufficient_data = 2)
  
  
}