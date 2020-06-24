library(ggplot2)
library(dplyr)

#' GeomTimelineLabel: Geom for adding text labels of earthquake locations on the timeline
#'
#' This Geom is the optional addon for GeomTimeline Geom element.
#' This aims to give extra information of locations by adding text labels on top of the pointsGrob element.
#' The text is 45 degree slanted and takes three mandatory aeses, x, label, and n_max.
#' The aes x takes datetime type parameter, label takes the column which would be written, finally n_max is the number of allowed earthquake magnitudes.
#' In that way, this Geom takes top n_max earthquakes.
#'
#' To make the same range of xmin and xmax, same logics are applied inside the draw_group function.
#'
#' @importFrom ggplot2 ggproto Geom aes draw_key_polygon
#' @importFrom dplyr filter top_n
#' @importFrom grid textGrob gpar gTree gList
#' @export
GeomTimelineLabel <- ggplot2::ggproto("GeomTimelineLabel", ggplot2::Geom,
                                 required_aes = c("x","label","n_max"),
                                 optional_aes = c("size","shape","colour","linesize","linetype","stroke","xmin","xmax"),
                                 default_aes = ggplot2::aes(y = 0.1),

                                 draw_key = ggplot2::draw_key_polygon,

                                 draw_group = function(data, panel_scales, coord) {
                                   # Set default xmin and xmax
                                   if (is.null(data$xmin)) data$xmin <- as.Date(min(data$x),origin='1970-01-01')
                                   if (is.null(data$xmax)) data$xmax <- as.Date(max(data$x),origin='1970-01-01')

                                   # Filtering data from xmin to xmax
                                   data <- data %>%
                                     dplyr::filter(as.Date(x,origin='1970-01-01') >= xmin) %>%
                                     dplyr::filter(as.Date(x,origin='1970-01-01') <= xmax)

                                   # Labeling
                                   data <- data %>%
                                     dplyr::top_n(n = as.integer(n_max[1]),size)

                                   # Transform the data
                                   coords <- coord$transform(data, panel_scales)
                                   offset <- 0.1

                                   names <- grid::textGrob(
                                     label = coords$label,
                                     x = unit(coords$x, "npc"),
                                     y = unit(coords$y + offset, "npc"),
                                     just = c("left", "bottom"),
                                     gp = grid::gpar(fontsize = 10, col = "black"),
                                     rot = 45
                                   )

                                   # Construct a segment grob
                                   lines <- grid::polylineGrob(
                                     x = unit(c(coords$x,coords$x),"npc"),
                                     y = unit(c(coords$y,coords$y+offset),"npc"),
                                     id = rep(1:length(coords$x),2),
                                     gp = grid::gpar(col="darkgray")
                                   )

                                   grid::gTree(children = grid::gList(names,lines))
                                 }
)

#' geom_timeline_lable(): geom function to write label of location infos
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
#' geom_timeline(aes(xmin=x_min, xmax=x_max)) +
#' geom_timeline_label(aes(xmin=x_min,xmax=x_max,label=location_name,n_max=5))
#' }
geom_timeline_label <- function(mapping = NULL, data = NULL, stat = 'identity',
                          position = 'identity', show.legend = NA,
                          na.rm = FALSE,
                          inherit.aes = TRUE, ...) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomTimelineLabel,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm,
                  ...)
  )
}
