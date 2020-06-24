utils::globalVariables(c("country","day","deaths","eq_primary","id","latitude","location_name","longitude","month","total_deaths","year"))

library(readr)
library(dplyr)

#' eq_load_data(): Read raw NOAA datafile with readr::read_delim() function.
#'
#' This function accepts a filename and reads NOAA earthquake data file, thereby creating dataframe.
#' The function is capable of cheking if the file exists and prints error msg when it doesn't.
#'
#' @param filename String-type file name provided and results in dataframe
#'
#' @return This function returns a dataframe object
#'
#' @importFrom readr read_delim
#' @export
#'
#' @examples
#' \dontrun{
#' eq_load_data('earthquakes.tsv.gz')
#' }
eq_load_data <- function(filename) {
  # Set file pointers
  fp <- system.file('extdata',filename,package='eqviz')
  # Exception when file doesn't exist
  if (!file.exists(fp))
    stop("file '", filename, "' does not exist")

  # Set column names (lower-cased and simplified)
  colnames <- c("id","flag_tsunami","year","month","day","hour","minute","second","focal_depth","eq_primary",
                "eq_mag_mw","eq_mag_ms","eq_mag_mb","eq_mag_ml","eq_mag_mfa","eq_mag_unk","intensity","country","state","location_name",
                "latitude","longitude","region_code","deaths","deaths_desc","missing","missing_desc","injuries","injuries_desc","damage_mil_usd",
                "damage_desc","houses_destroyed","houses_destoryed_desc","houses_damaged","houses_damaged_desc","total_deaths","total_deaths_desc","total_missing","total_missing_desc","total_injuries",
                "total_injuries_desc","total_damage_mil_usd","total_damage_desc","total_houses_destroyed","total_houses_destoryed_desc","total_houses_damaged","total_houses_damaged_desc")

  # Read file (read_delim)
  dataset <- readr::read_delim(filename, delim='\t',
                                col_names = colnames,
                                skip = 1,
                                col_types = "icnnniinnnnnnnnnicccnniniiininiiiiiniiiiiniiiii"
                                )
  # dplyr::tbl_df(noaa_set)
  return(dataset)
}


#' helper_neg_dates(): Converts BC-dated character to datetime object
#'
#' This function mitigates the shortfall of lubridate::ymd() and as.Date() as they fail to convert BC dates.
#' The function accepts a variale of BC-dated character and mirror it with respect to origin of AD0.
#' Subraction of the date from that origin and add 1 day results in expected BC date.
#'
#' @param date A BC date to be converted
#'
#' @return Converted BC datetime object
#'
#' @importFrom lubridate ymd days
#' @export
#'
#' @examples
#' \dontrun{
#' helper_neg_dates('-2145-01-01')}
helper_neg_dates <- function(date) {
  origin_date <- as.numeric(lubridate::ymd('0000-1-1'))
  cur_date <- as.numeric(lubridate::ymd(date))
  neg_date <- as.Date(origin_date*2-cur_date,origin='1970-01-01')+lubridate::days(1)
  return(neg_date)
}

#' eq_location_clean(): Clean out location name
#' This function separates out country name from the location name including colon(:).
#' To achieve the objective, it employs regular expressions and finally converts the chracters to the title form.
#'
#' @param data_col location_name column to be simplified
#'
#' @return Simplified column
#'
#' @importFrom stringr str_remove str_replace_all str_squish str_to_title
#' @importFrom dplyr %>%
#' @export
#'
#' @examples
#' \dontrun{
#' eq_load_data('earthquakes.tsv.gz') %>%
#' eq_location_clean(location_name)
#' }
eq_location_clean <- function(data_col) {
  data_col %>%
    stringr::str_remove("^.*:") %>%
    stringr::str_replace_all("[,\\,;:](\\w)",", \\1") %>%
    stringr::str_squish() %>%
    stringr::str_to_title()
}

#' eq_clean_data(): Clean the dataset
#'
#' This function does three chores including conversion of datatypes, making date column, and drop unneeded columns.
#' More specifically, latitude and longitude columns are converted to numeric type.
#' Date column is made by combining year, month, and day column, and converted to datetime type.
#' Finally, only columns id, year, date, country, location_name, longitude, latitude, deaths, total_deaths, eq_primary are preserved.
#'
#' @param dataset Dataset to be kept tidy
#'
#' @return Simplified dataset
#'
#' @importFrom stringr str_pad
#' @importFrom dplyr mutate select %>% if_else
#' @importFrom lubridate ymd
#' @export
#'
#' @examples
#' \dontrun{
#' eq_load_data('earthquakes.tsv.gz') %>%
#' eq_clean_data()
#' }
eq_clean_data <- function(dataset) {
  year_len4 <- stringr::str_pad(as.character(abs(dataset$year)),width=4,side='left',pad='0')
  # origin_date <- as.numeric(lubridate::ymd('0000-1-1'))
  dataset %>%
    dplyr::mutate(latitude = as.numeric(latitude),
                  longitude = as.numeric(longitude),
                  year = ifelse(year<0,paste0('-',year_len4),year_len4),
                  month = ifelse(is.na(month),1,month),
                  day = ifelse(is.na(day),1,day),
                  date = paste(year,month,day,sep='-')) %>%

    dplyr::mutate(date = dplyr::if_else(nchar(year)==4,lubridate::ymd(date),helper_neg_dates(date)),
                  location_name = eq_location_clean(location_name)) %>%

    dplyr::select(id,year,date,country,location_name,longitude,latitude,deaths,total_deaths,eq_primary)
}
