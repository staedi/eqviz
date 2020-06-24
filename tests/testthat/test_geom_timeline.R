context('geom_timeline drawing')

testthat::test_that('Omitting required aeses error',{
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
                        deaths=c(10,4,5,15)
  )
  dataset$location_name <- eq_location_clean(dataset$location_name)
  plot <- ggplot2::ggplot(dataset) +
    geom_timeline()
  expect_that(print(plot),throws_error())
})

testthat::test_that('Misspecified xmin and xmax',{
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
                        deaths=c(10,4,5,15)
  )
  dataset$location_name <- eq_location_clean(dataset$location_name)
  plot <- ggplot2::ggplot(dataset,aes(x=date,colour=deaths,size=eq_primary)) +
    geom_timeline(aes(xmin='2010-01-01',xmax='2015-01-01'))
  expect_that(print(plot),throws_error())
})

testthat::test_that('Compare default xmin and xmax',{
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
                        deaths=c(10,4,5,15)
  )
  dataset$location_name <- eq_location_clean(dataset$location_name)
  plot_custom <- ggplot2::ggplot(dataset,aes(x=date,colour=deaths,size=eq_primary)) +
    geom_timeline(aes(xmin=lubridate::ymd('2010-01-01'),xmax=lubridate::ymd('2015-01-01')))
  plot_default <- ggplot2::ggplot(dataset,aes(x=date,colour=deaths,size=eq_primary)) +
    geom_timeline(aes(xmin=min(date),xmax=max(date)))
  plot_omit <- ggplot2::ggplot(dataset,aes(x=date,colour=deaths,size=eq_primary)) +
    geom_timeline()
  plot_theme <- ggplot2::ggplot(dataset,aes(x=date,colour=deaths,size=eq_primary)) +
    geom_timeline() +
    ggplot2::scale_x_date(name = "DATE") +
    ggplot2::scale_size_continuous(name='Richter scale value') +
    ggplot2::scale_color_continuous(name='# of deaths') +
    theme_eq_custom()

  # Generate svgs
  # vdiffr::expect_doppelganger("Custom range",plot_custom)
  # vdiffr::expect_doppelganger("Default range",plot_default)
  # vdiffr::expect_doppelganger("Omitted range",plot_omit)
  # vdiffr::expect_doppelganger("Theme applied",plot_theme)

  vdiffr::expect_doppelganger("Default range",plot_omit)
  expect_that(vdiffr::expect_doppelganger("Custom range",plot_default),throws_error())
  expect_that(vdiffr::expect_doppelganger("Theme applied",plot_omit),throws_error())
})
