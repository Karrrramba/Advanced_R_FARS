test_that("fars_read returns a tibble", {
  setwd(system.file("extdata", package = "FARSanalyzr"))
  file_name <- "accident_2013.csv.bz2"

  data <- fars_read(file_name)
  expect_s3_class(data, "tbl_df")
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
  # current_dir <- getwd()
  setwd(system.file("extdata", package = "FARSanalyzr"))
  print(getwd())
  # print(list.files())

  # cat(list.files())
  years <- c(2013)

  data <- fars_read_years(years)
  # Test whether returned object is a list
  expect_type(data, "list")

  # setwd(current_dir)
})

test_that("fars_read_years the objects in the list are tibbles", {
  # current_dir <- getwd()
  setwd(system.file("extdata", package = "FARSanalyzr"))

  years <- 2013
  # file_name <- make_filename(years)
  #file_path <- paste0("", make_filename(years))
  # sample_data <- data.frame(MONTH = 1:3,
  #                           year = 2013)
  # write.csv(sample_data, file = file_name, row.names = FALSE)

  data <- fars_read_years(years)

  print(data[1])

  expect_true(all(sapply(data, inherits, "tbl_df")))
})

test_that("fars_read_years works with multiple years", {
  setwd(system.file("extdata", package = "FARSanalyzr"))
  years <- c(2013, 2014)
  data <- fars_read_years(years)
  # Test whether returned object is a list
  expect_type(data, "list")
  # Test whether objects within the list are tibbles
  expect_true(all(sapply(data, inherits, "tbl_df")))
})
