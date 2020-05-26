##' @title Make a prisma diagram
##' @name make_prisma
##' @importFrom ggprisma ggprisma
##' @return
##' @export
make_prisma <- function() {

  ggprisma::ggprisma(retrieved = 131, included = 16, 
                     duplicates = 35, full_text = 23, 
                     wrong_intervention = 5, 
                     wrong_comparator = 2, 
                     wrong_design = 1, 
                     awaiting_classification = 2)
  

}
