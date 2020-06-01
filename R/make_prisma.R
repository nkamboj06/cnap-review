##' @title Make a prisma diagram
##' @name make_prisma
##' @importFrom ggprisma ggprisma
##' @return
##' @export
make_prisma <- function(retrieved, duplicates, included, full_text,
                        wrong_setting,
                        wrong_population,
                        awaiting_classification,
                        line_size = 0.50, box_type = "round", box_lines = 0.5, 
                        box_colour = "#ff4f14", box_fill = "white", text_colour = "#ff4f14",
                        arrow_colour = "#ff4f14") {

    
    # Based on user input
    screened = {{ retrieved }}-{{ duplicates }} 
    screen_excluded = screened-{{ full_text }} - {{ awaiting_classification }}
    full_text_excluded = {{ full_text }} -{{ included }} 
    
    
    # Main flow y coordinates  
    import_y_max <- 100
    import_y_min <- 90
    screen_y_max <- 80
    screen_y_min <- 70
    full_y_max <- 60
    full_y_min <- 50
    included_y_max <- 40
    included_y_min <- 30
    
    # Scondary flow y coordinates  
    duplicates_y_max <- 90
    duplicates_y_min <- 80
    
    full_excluded_y_min <- 25
    full_excluded_y_max <- 45
    
    screen_excluded_y_min <- 65
    screen_excluded_y_max <- 75
    
    awaiting_classification_y_min <- 50
    awaiting_classification_y_max <- 60
    
    # x coordinates  
    main_x_min <- 20
    main_x_max <- 70
    
    second_x_min <- 75
    second_x_max <- 95
    
    arrow_x <- 45
    
    ###Plot space set-up####
    ggplot2::ggplot(data=NULL, ggplot2::aes()) +
      
      ########## Main box flow ###########
    
    # import box
    
    ggplot2::annotation_custom(grob = grid::roundrectGrob(gp = grid::gpar(col = box_colour, fill = box_fill)),
                               xmin = main_x_min, xmax=main_x_max, ymin=import_y_min, ymax=import_y_max)+
      ggfittext::geom_fit_text(ggplot2::aes(xmin = main_x_min, xmax=main_x_max, ymin=import_y_min, ymax=100),
                               label= glue::glue({{ retrieved }} , ' references identified'), 
                               reflow = TRUE, colour = text_colour, data = data.frame()) +
      
      # screen box
      ggplot2::annotation_custom(grob = grid::roundrectGrob(gp = 
                                                              grid::gpar(col = box_colour, fill = box_fill)),
                                 xmin = main_x_min, xmax=main_x_max, ymin=screen_y_min, 
                                 ymax=screen_y_max) +
      ggfittext::geom_fit_text(ggplot2::aes(xmin = main_x_min, xmax=main_x_max, ymin=screen_y_min, ymax=screen_y_max),
                               label= glue::glue(screened, ' titles and abstracts screened'), 
                               reflow = TRUE, colour = text_colour, data = data.frame()) +
      
      # full text box
      ggplot2::annotation_custom(grob = grid::roundrectGrob(gp = 
                                                              grid::gpar(col = box_colour, fill = box_fill)),
                                 xmin = main_x_min, xmax=main_x_max, ymin=full_y_min, ymax=full_y_max) +
      ggfittext::geom_fit_text(ggplot2::aes( xmin = main_x_min, xmax=main_x_max, ymin=full_y_min, ymax=full_y_max),
                               label= glue::glue({{ full_text }} , ' full-text articles assessed'), 
                               reflow = TRUE, colour = text_colour, data = data.frame()) +
      
      # included box 
      ggplot2::annotation_custom(grob = grid::roundrectGrob(gp = 
                                                              grid::gpar(col = box_colour, fill = box_fill)),
                                 xmin = main_x_min, xmax=main_x_max, 
                                 ymin=included_y_min, ymax=included_y_max) +
      ggfittext::geom_fit_text(ggplot2::aes(xmin = main_x_min, xmax=main_x_max, 
                                            ymin=included_y_min, ymax=included_y_max),
                               label= glue::glue({{ included }} , ' studies included'), 
                               reflow = TRUE, colour = text_colour, data = data.frame()) +
      
      ######### Secondary flow boxes ############
    
    
    ggplot2::annotation_custom(grob = grid::roundrectGrob(gp = 
                                                            grid::gpar(col = box_colour, fill = box_fill)),
                               xmin = second_x_min, xmax=second_x_max, 
                               ymin=duplicates_y_min, ymax=duplicates_y_max) +
      ggfittext::geom_fit_text(ggplot2::aes(xmin = second_x_min, xmax=second_x_max, 
                                            ymin=duplicates_y_min, ymax=duplicates_y_max),
                               label= glue::glue({{ duplicates }} , ' duplicates removed'),
                               reflow = TRUE, colour = text_colour, data = data.frame()) +
      
      
      # excluded box 
      ggplot2::annotation_custom(grob = 
                                   grid::roundrectGrob(gp = 
                                                         grid::gpar(col = box_colour, fill = box_fill)),
                                 xmin = second_x_min, xmax=second_x_max, 
                                 ymin=screen_excluded_y_min, ymax=screen_excluded_y_max) +
      ggfittext::geom_fit_text(ggplot2::aes(xmin = second_x_min, xmax=second_x_max, 
                                            ymin=screen_excluded_y_min, ymax=screen_excluded_y_max),
                               label= glue::glue(screen_excluded, ' studies excluded'),
                               reflow = TRUE, colour = text_colour, data = data.frame()) +
      
      # awaiting classifications box 
      ggplot2::annotation_custom(grob = 
                                   grid::roundrectGrob(gp = 
                                                         grid::gpar(col = box_colour, fill = box_fill)),
                                 xmin = second_x_min, xmax=second_x_max, 
                                 ymin=awaiting_classification_y_min, ymax=awaiting_classification_y_max) +
      ggfittext::geom_fit_text(ggplot2::aes(xmin = second_x_min, xmax=second_x_max, 
                                            ymin=awaiting_classification_y_min, ymax=awaiting_classification_y_max),
                               label= glue::glue({{ awaiting_classification}} , ' studies awaiting classification'),
                               reflow = TRUE, colour = text_colour, data = data.frame()) +
      
      # full text excluded box 
      ggplot2::annotation_custom(grob = 
                                   grid::roundrectGrob(gp = 
                                                         grid::gpar(col = box_colour, fill = box_fill)),
                                 xmin = second_x_min, xmax=second_x_max, 
                                 ymin=full_excluded_y_min, ymax=full_excluded_y_max) +
      ggfittext::geom_fit_text(ggplot2::aes(xmin = second_x_min, xmax=second_x_max,
                                            ymin=full_excluded_y_max-7, ymax=full_excluded_y_max),
                               label= glue::glue(full_text_excluded, ' studies excluded'),
                               reflow = TRUE, colour = text_colour, data = data.frame()) +
      ggfittext::geom_fit_text(ggplot2::aes(xmin = second_x_min, xmax=second_x_max,
                                            ymin=full_excluded_y_max-17, ymax=full_excluded_y_max-7),
                               label= glue::glue({{ wrong_setting }} , ' wrong setting', '\n',
                                                 {{ wrong_population }} , ' wrong population'),
                               colour = text_colour, data = data.frame()) +
      
      
      ####### Main flow arrows#######
    
    # Import - screen
    ggplot2::geom_segment(ggplot2::aes(
      x=arrow_x, xend = arrow_x, y = import_y_min, yend = screen_y_max+0.25), 
      size = line_size,  colour = arrow_colour,
      arrow = ggplot2::arrow(length = ggplot2::unit(2, "mm"), type= "closed")) +
      
      # Screen - full text
      ggplot2::geom_segment(ggplot2::aes(
        x=arrow_x, xend=arrow_x, y=screen_y_min, yend=full_y_max+0.25), 
        size = line_size,  colour = arrow_colour,
        arrow = ggplot2::arrow(length = ggplot2::unit(2, "mm"), type= "closed")) +
      
      # Full text - included
      
      ggplot2::geom_segment(ggplot2::aes(
        x=arrow_x, xend=arrow_x, y=full_y_min, yend=included_y_max+0.25), 
        size = line_size,  colour = arrow_colour,
        arrow = ggplot2::arrow(length = ggplot2::unit(2, "mm"), type= "closed")) +
      
      ####Secondary flow arrows########    
    
    # Import - duplicates  
    
    ggplot2::geom_curve(ggplot2::aes(
      x = arrow_x, xend = second_x_min, 
      y = duplicates_y_max,  yend = duplicates_y_max-5), linetype = "dashed",
      curvature = 0.1, colour = arrow_colour) +
      
      # Screen - excluded  
      
      ggplot2::geom_curve(ggplot2::aes(
        x = arrow_x, xend = second_x_min, 
        y = screen_y_min,  yend = screen_excluded_y_max-5), linetype = "dashed",
        curvature = 0.1, colour = arrow_colour) +
      
      # Screen - awaiting  
      
      ggplot2::geom_curve(ggplot2::aes(
        x = arrow_x, xend = second_x_min, 
        y = screen_y_min,  yend = awaiting_classification_y_max-4), linetype = "dashed",
        curvature = -0.15, colour = arrow_colour) +
      
      # Full text - excluded  
      
      ggplot2::geom_curve(ggplot2::aes(
        x = arrow_x, xend = second_x_min, 
        y = full_y_min, yend = full_excluded_y_max-8), linetype = "dashed",
        curvature = -0.15, colour = arrow_colour) +
      
      ggplot2::theme_void()
    
  
  
  
}