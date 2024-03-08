test_that("fars_read returns a tibble", {
  setwd(system.file("extdata", package = "FARSanalyzr"))
  file_name <- "accident_2013.csv.bz2"

  data <- fars_read(file_name)
  expect_s3_class(data, "tbl_df")
})

test_that("fars_read returns an error message", {
  setwd(system.file("extdata", package = "FARSanalyzr"))
  file_name <- "accident_2019.csv.bz2"

  # data <- fars_read(file_name)
  expect_error(fars_read(file_name), "file 'accident_2019.csv.bz2' does not exist")
})

test_that("make_filename generates correct filename", {
  # Test make_filename function with a single year
  years <- 2013
  expected_filename <- paste0("accident_", years, ".csv.bz2")
  actual_filename <- make_filename(years)
  expect_equal(actual_filename, expected_filename)

  # Test make_filename function with multiple years
  years <- c(2013, 2014)
  expected_filenames <- c("accident_2013.csv.bz2", "accident_2014.csv.bz2")
  actual_filenames <- make_filename(years)
  expect_equal(actual_filenames, expected_filenames)
})

test_that("fars_read_years returns a list of tibbles", {
  setwd(system.file("extdata", package = "FARSanalyzr"))
  years <- 2013
  data <- fars_read_years(years)
  expect_type(data, "list")
})

test_that("fars_read_years the objects in the list are tibbles", {
  setwd(system.file("extdata", package = "FARSanalyzr"))
  years <- 2013
  data <- fars_read_years(years)
  expect_true(all(sapply(data, inherits, "tbl_df")))
})

test_that("fars_read_years works with multiple years", {
  setwd(system.file("extdata", package = "FARSanalyzr"))
  years <- c(2013, 2014)
  data <- fars_read_years(years)
  expect_type(data, "list")
})

test_that("fars_summarise_years returns a tibble", {
  setwd(system.file("extdata", package = "FARSanalyzr"))
  years <- c(2013, 2014)
  data <- fars_summarize_years(years)
  expect_s3_class(data, "tbl_df")
})

test_that("fars_summarise_years returns the correct columns", {
  setwd(system.file("extdata", package = "FARSanalyzr"))
  years <- c(2013, 2014)
  data <- fars_summarize_years(years)
  expect_identical(colnames(data), c("MONTH", "2013", "2014"))
})

test_that("fars_map_state returns the correct error", {
  setwd(system.file("extdata", package = "FARSanalyzr"))
  years <- 2013
  state <- 60
  expect_error(fars_map_state(state, years), "invalid STATE number: 60")
})
