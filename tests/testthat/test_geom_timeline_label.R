context('geom_timeline_label drawing')

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
  plot <- ggplot2::ggplot(dataset,aes(x=date,colour=deaths,size=eq_primary)) +
    geom_timeline() +
    geom_timeline_label()
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
    geom_timeline(aes(xmin=lubridate::ymd('2010-01-01'),xmax=lubridate::ymd('2015-01-01'))) +
    geom_timeline_label(aes(xmin='2010-01-01',xmax='2015-01-01',label=location_name,n_max=5))
  expect_that(print(plot),throws_error())
})

testthat::test_that('Compare range of x with geom_timeline and geom_timeline_label',{
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
  plot_mismatch <- ggplot2::ggplot(dataset,aes(x=date,colour=deaths,size=eq_primary)) +
    geom_timeline(aes(xmin=lubridate::ymd('2010-01-01'),xmax=lubridate::ymd('2015-01-01'))) +
    geom_timeline_label(aes(label=location_name,n_max=5))
  plot_match <- ggplot2::ggplot(dataset,aes(x=date,colour=deaths,size=eq_primary)) +
    geom_timeline(aes(xmin=lubridate::ymd('2010-01-01'),xmax=lubridate::ymd('2015-01-01'))) +
    geom_timeline_label(aes(xmin=lubridate::ymd('2010-01-01'),xmax=lubridate::ymd('2015-01-01'),label=location_name,n_max=5))
  plot_theme <- ggplot2::ggplot(dataset,aes(x=date,colour=deaths,size=eq_primary)) +
    geom_timeline(aes(xmin=lubridate::ymd('2010-01-01'),xmax=lubridate::ymd('2015-01-01'))) +
    geom_timeline_label(aes(xmin=lubridate::ymd('2010-01-01'),xmax=lubridate::ymd('2015-01-01'),label=location_name,n_max=5)) +
    ggplot2::scale_x_date(name = "DATE") +
    ggplot2::scale_size_continuous(name='Richter scale value') +
    ggplot2::scale_color_continuous(name='# of deaths') +
    theme_eq_custom()

  # Generate svgs
  # vdiffr::expect_doppelganger("Mismatched range",plot_mismatch)
  # vdiffr::expect_doppelganger("Matched range",plot_match)
  # vdiffr::expect_doppelganger("Theme applied",plot_theme)

  expect_that(vdiffr::expect_doppelganger("Mismatched range",plot_match),throws_error())
  expect_that(vdiffr::expect_doppelganger("Theme applied",plot_match),throws_error())
})
