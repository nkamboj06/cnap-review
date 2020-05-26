##' @title Data frame of meta-analysis results
##' @description Creates meta-anlysis results
##' @name meta_analysis
##' @param data Data frame of subsetted studies
##' @return
##' @export
meta_analysis <- function(data) {
  
    group <- data
    out <- loa_maker(group$meanbias,group$V_bias, group$logs2, group$V_logs2)
    out <- round(out, digits=2)
    out <- out %>% 
      dplyr::mutate(Participants = sum(group$participants, na.rm=T)) %>% 
      dplyr::mutate(Measurements = format(sum(group$measurements, na.rm = T), big.mark=",", scientific = FALSE)) %>% 
      dplyr::mutate(Studies = length(unique(group$study))) %>% 
      dplyr::select(Studies, m, Participants, Measurements, bias_mean, sd2_est, tau_est, LOA_L, LOA_U, CI_L_rve, CI_U_rve) %>% 
      dplyr::rename("Comparisons" = m)
    out
    

}
