context('Data cleaning')

testthat::test_that('Load data',{
  expect_that(eq_load_data('a.txt'),throws_error())
})

testthat::test_that('Clean empty data error',{
  expect_that(eq_clean_data(data.frame()),throws_error())
})

testthat::test_that('Negative dates mismatch',{
  expect_that(helper_neg_dates('-213-01-01'),gives_warning())
})

testthat::test_that('Test normal',{
  dataset <- eq_load_data('earthquakes.tsv.gz')
  expect_that(dataset,is_a("data.frame"))
  dataset <- eq_clean_data(dataset)
  expect_that(dataset$latitude[1],is_a("numeric"))
  expect_that(dataset$longitude[1],is_a("numeric"))
  expect_that(dataset$date[1],is_a("Date"))
  expect_that(dataset$flag_tsunami,gives_warning())

  dataset_original <- dataset
  expect_that(dataset$location_name,equals(dataset_original$location_name))
})
