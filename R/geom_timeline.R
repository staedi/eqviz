library(ggplot2)
library(dplyr)

#' GeomTimeline: Geom for drawing earthquakes on the timeline
#'
#' This Geom take earthquake dataset and can show magnitudes and deaths by circles on a line.
#' Only x aes, representing date of earthquake is mandatory.
#' However, size and colour aeses can be used to represent magnitude of and deaths from the earthquakes, respectively.
#'
#' To make the customized range of minimum and maximum range of x axis, xmin and xmax aeses can be provided (datetime).
#' When not given, the minimum and maximum value of date object are calculated by default.
#'
#' @importFrom ggplot2 ggproto Geom aes draw_key_point alpha
#' @importFrom dplyr filter
#' @importFrom grid pointsGrob unit gpar gTree gList polylineGrob
#' @export
GeomTimeline <- ggplot2::ggproto("GeomTimeline", ggplot2::Geom,
                             required_aes = c("x"),
                             optional_aes = c("size","shape","colour","linesize","linetype","xmin","xmax"),
                             default_aes = ggplot2::aes(y = 0.1, shape = 19, size = 3, colour = "gray", alpha = 0.5, fill = NA, linesize = 2, linetype = 1),

                             draw_key = ggplot2::draw_key_point,

                             draw_group = function(data, panel_scales, coord) {
                               # Set default xmin and xmax
                               if (is.null(data$xmin)) data$xmin <- as.Date(min(data$x),origin='1970-01-01')
                               if (is.null(data$xmax)) data$xmax <- as.Date(max(data$x),origin='1970-01-01')

                               # Filtering data from xmin to xmax
                               data <- data %>%
                                 dplyr::filter(as.Date(x,origin='1970-01-01') >= xmin) %>%
                                 dplyr::filter(as.Date(x,origin='1970-01-01') <= xmax)

                               # Transform the data
                               coords <- coord$transform(data, panel_scales)
                               line_range = c(min(coords$x),max(coords$x))

                               # Construct a point grob
                               points <- grid::pointsGrob(
                                 x = coords$x,
                                 y = coords$y,
                                 pch = coords$shape,
                                 size = grid::unit(coords$size, "mm"),
                                 gp = grid::gpar(col = ggplot2::alpha(coords$colour,coords$alpha),
                                                 alpha = coords$alpha)
                               )

                               ys <- unique(coords$y)
                               rangex <- range(coords$x)

                               # Construct a segment grob
                               lines <- grid::polylineGrob(
                                 x = unit(c(rangex[1],rangex[2]),"npc"),
                                 y = unit(c(ys,ys),"npc"),
                                 gp = grid::gpar(
                                   col = "gray",
                                   lwd = grid::unit(coords$linesize[1],"mm"),
                                   lty = coords$linetype[1]
                                   )
                               )
                               grid::gTree(children = grid::gList(lines, points))
                               }

                             )

#' geom_timeline(): geom function to draw earthquake infos on a timeline
#'
#' @param mapping a set of aesthetic mappings
#' @param data data to be plotted
#' @param stat stat object (No custom version used here)
#' @param position position object (No custom version used here)
#' @param show.legend inclusion of the legend
#' @param na.rm treatment of missing values
#' @param inherit.aes inheriting aeses from default geom
#' @param ... additional parameters
#'
#' @return None
#' @export
#'
#' @examples
#' \dontrun{
#' x_min <- as.Date('2003-01-01',origin='1970-01-01')
#' x_max <- as.Date('2017-01-01',origin='1970-01-01')
#' eq_load_data('earthquakes.tsv.gz') %>%
#' eq_clean_data() %>%
#' filter(year >= 2000) %>%
#' filter(country %in% c("USA","MEXICO")) %>%
#' ggplot2::ggplot(ggplot2::aes(x=date,y=country,colour=deaths,size=eq_primary)) +
#' geom_timeline(aes(xmin=x_min, xmax=x_max))
#' }
geom_timeline <- function(mapping = NULL, data = NULL, stat = 'identity',
                      position = 'identity', show.legend = NA,
                      na.rm = FALSE,
                      inherit.aes = TRUE, ...) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomTimeline,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


#' theme_eq_custom(): Setting customized theme for simplifying unneeded elements
#'
#' This function specifies several graphical elements in a way in coordance with the guideline.
#' A few adjustments include placing the legend in the bottom, removing x and y axes grids, and changes colors to gray.
#'
#' @param ... No parameters
#'
#' @return None but adjusting the apperance of ggplot object
#'
#' @importFrom ggplot2 theme_minimal theme element_line element_blank
#' @export
#'
#' @examples
#' \dontrun{
#' x_min <- as.Date('2003-01-01',origin='1970-01-01')
#' x_max <- as.Date('2017-01-01',origin='1970-01-01')
#' eq_load_data('earthquakes.tsv.gz') %>%
#' eq_clean_data() %>%
#' filter(year >= 2000) %>%
#' filter(country %in% c("USA","MEXICO")) %>%
#' ggplot2::ggplot(ggplot2::aes(x=date,y=country,colour=deaths,size=eq_primary)) +
#' geom_timeline(aes(xmin=x_min, xmax=x_max)) +
#' geom_timeline_label(aes(xmin=x_min,xmax=x_max,label=location_name,n_max=5)) +
#' ggplot2::scale_x_date(name = "DATE") +
#' ggplot2::scale_size_continuous(name='Richter scale value') +
#' ggplot2::scale_color_continuous(name='# of deaths') +
#' theme_eq_custom()
#' }
theme_eq_custom <- function(...) {
  ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = 'bottom',
      axis.line.x = ggplot2::element_line(colour="gray"),
      axis.ticks.x = ggplot2::element_line(colour="gray"),
      axis.title.y = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank()
     )
    # ) +
    # ggplot2::theme(...)
}
