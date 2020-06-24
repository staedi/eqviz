library(ggplot2)
library(dplyr)

#' eq_create_label(): Create an annotated text to be used in leaflet pop-up
#'
#' This function creates a html tag including three lines of data, which pops up when the user clicks on a location.
#' The information includes location, magnitude, and total deaths.
#' Using <br> and <b> tags, the text is shown in three lines with subjects emphasized (made bold-faced).
#'
#' @param data Dataframe from which the function extracts necessary infos
#'
#' @return Created popup_text
#' @export
#'
#' @examples
#' \dontrun{
#' dataset$popup_text <- eq_load_data('earthquake.tsv.gz') %>%
#' eq_create_label()
#' }
eq_create_label <- function(data) {
  popup_text <- paste0(
    ifelse(is.na(data$location_name),"",paste0("<b>Location:</b> ",data$location_name)),
    ifelse(is.na(data$eq_primary),"",paste0("<br /><b>Magnitude:</b> ",data$eq_primary)),
    ifelse(is.na(data$total_deaths),"",paste0("<br /><b>Total Deaths:</b> ",data$total_deaths))
  )
  return(popup_text)
}

#' eq_map(): Build a leaflet map indicating earthquake locations
#'
#' This function builds an interactive leaflet map indicating locations of earthquakes indicated by points on the map.
#' It can also pull out information about the quake using user-provided columns.
#' Examples include data and popup_text columns, latter of which shows three-lined summary info.
#'
#' @param data Dataframe from which the map is drawn
#' @param annot_col Column to be used for popup info
#'
#' @return Leaflet map object to be drawn
#'
#' @importFrom leaflet leaflet addTiles addCircleMarkers
#' @export
#'
#' @examples
#' \dontrun{
#' dataset$popup_text <- eq_load_data('earthquake.tsv.gz') %>%
#' eq_map(date)
#'
#' dataset <- eq_create_label(dataset)
#' eq_map(dataset,'popup_text')
#' }
eq_map <- function(data, annot_col) {
  map <- leaflet::leaflet() %>%
    leaflet::addTiles() %>%
    leaflet::addCircleMarkers(data = data,
                              lng = ~ longitude,
                              lat = ~ latitude,
                              radius = ~ eq_primary,
                              weight = 1,
                              popup = ~ data[[annot_col]]
                              )
  map
}
