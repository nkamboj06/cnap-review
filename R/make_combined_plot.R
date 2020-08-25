# @title Create a plot with results for sbp, dbp and map
#' @export
#' @importFrom patchwork plot_layout
#' @importFrom ggplot2 labs

make_combined_plot <- function(dist_plot_primary_sbp,
                           dist_plot_primary_dbp,
                           dist_plot_primary_map) {
  
    dist_plot_primary_sbp / dist_plot_primary_dbp / dist_plot_primary_map


}
