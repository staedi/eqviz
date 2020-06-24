context('Leaflet drawing')

testthat::test_that('Empty data leaftet plotting error',{
  dataset <- data.frame()
  expect_that(eq_map(dataset,date),throws_error())
})

testthat::test_that('Test normal',{
  dataset <- data.frame(date = c(lubridate::ymd('2008-06-04'),
                                 lubridate::ymd('2010-06-07'),
                                 lubridate::ymd('2012-06-14'),
                                 lubridate::ymd('2023-06-18')),
                        eq_primary = c(6.4,4.5,5.9,7.4),
                        country = c('INDONESIA',
                                    'PERU',
                                    'TURKEY',
                                    'NEW ZEALAND'),
                        location_name = c('INDONESIA: NORTH MALUKU: MOROTAI',
                                          'PERU:  COAST:  CHIMBOTE',
                                          'TURKEY:  BINGOL',
                                          'KERMADEC ISLANDS:  S OF, RAOUL'),
                        latitude=c(2.923,-9.634,39.421,-33.294),
                        longitude=c(128.248,-78.591,40.697,-177.838),
                        total_deaths=c(10,4,5,15)
                        )
  dataset$location_name <- eq_location_clean(dataset$location_name)
  dataset$popup_text <- eq_create_label(dataset)
  expect_match(dataset$popup_text[1],paste0("<b>Location:</b> ",dataset$location_name[1]))
  expect_equal(class(eq_map(dataset,'popup_text')),c("leaflet","htmlwidget"))
})
