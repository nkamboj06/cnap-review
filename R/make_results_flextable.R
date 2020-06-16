##' @title Make a table of the meta-analyses
##' @name make_results_flextable
##' @param results_list list of meta-analysis results
##' @return
##' @author Aaron Conway
##' @export
#' @importFrom dplyr bind_rows rename
#' @importFrom flextable flextable compose as_paragraph 
#' as_sup footnote add_header_row theme_booktabs width fontsize
make_results_flextable <- function(results_list,
                               names) {

  ma <- results_list
  names(ma) <- c(names)
  df <- bind_rows(ma, .id="Comparison")
  
  df %>%
    rename(" " = Comparison, "Mean bias" = bias_mean,
           "Tau-squared" = tau_est, 
           "Lower 95% LoA" = LOA_L, 
           "Upper 95% LoA" = LOA_U, 
           "Outer CI for lower 95% LoA" = CI_L_rve,
           "Outer CI for upper 95% LoA" = CI_U_rve) %>% flextable() %>%
     compose(j=~sd2_est, part = "header",
            value = as_paragraph(
              "SD",
              as_sup("2")
            )) %>% 
    compose(j="Tau-squared", part = "header",
            value = as_paragraph(
              "τ",
              as_sup("2")
            ))  %>% 
    footnote(i = 1, j = 6:12,
             value = as_paragraph(
               "Units are mmHg"
             ),
             ref_symbols = "a",
             part = "header") %>% 
    footnote(i = 1, j = 7,
             value = as_paragraph(
               "Variance"
             ),
             ref_symbols = "b",
             part = "header") %>% 
    footnote(i = 1, j = 8,
             value = as_paragraph(
               "Measure of heterogeneity"
             ),
             ref_symbols = "c",
             part = "header") %>% 
    footnote(i = 1, j = 9:12,
             value = as_paragraph(
               "LoA = Limits of Agreement"
             ),
             ref_symbols = "d",
             part = "header") %>% 
    footnote(i = 1, j = 11:12,
             value = as_paragraph(
               "CI = Confidence Intervals"
             ),
             ref_symbols = "e",
             part = "header") %>% 
    add_header_row(values = c("","","","","","","","","","","Population LoA"),  colwidths = c(rep(1,10, by=1), 2))   %>%
    theme_booktabs() %>% 
    width(j=1,width = 1.19) %>%
    width(j=2,width = 0.69) %>%
    width(j=3,width = 1.06) %>%
    width(j=4,width = 0.94) %>%
    width(j=5,width = 1.19) %>%
    width(j=6,width = 0.56) %>%
    width(j=7,width = 0.56) %>%
    width(j=8,width = 0.6) %>%
    width(j=9,width = 0.59) %>%
    width(j=10,width = 0.75) %>%
    width(j=11,width = 0.94) %>%
    width(j=12,width = 0.94) %>% 
    fontsize(size = 10)
  

}
