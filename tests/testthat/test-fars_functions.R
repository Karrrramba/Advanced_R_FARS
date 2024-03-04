test_that("fars_read returns a tibble", {
  # Create CSV temp file with sample data
  tmp_file <- tempfile("accident_2013", tmpdir = tempdir(check = TRUE))
  sample_data <- data.frame(A = 1:3, B = c("a", "b", "c"))
  write.csv(sample_data, file = tmp_file, row.names = FALSE)

  data <- fars_read(tmp_file)
  expect_s3_class(data, "tbl_df")
})

test_that("make_filename generates correct filename", {
  # Test make_filename function with a single year
  years <- 2013
  expected_filename <- "accident_2013.csv.bz2"
  actual_filename <- make_filename(years)
  expect_equal(actual_filename, expected_filename)

  # Test make_filename function with multiple years
  years <- c(2013, 2014)
  expected_filenames <- c("accident_2013.csv.bz2", "accident_2014.csv.bz2")
  actual_filenames <- make_filename(years)
  expect_equal(actual_filenames, expected_filenames)
})

test_that("fars_read_years returns a list of tibbles", {
  years <- 2013
  data <- fars_read_years(years)
  # Test whether returned object is a list
  expect_type(data, "list")
  # Test whether objects within the list are tibbles
  expect_true(all(sapply(data, inherits, "tbl_df")))
})

test_that("fars_read_years the objects in the list are tibbles", {

  expect_s3_class(all(sapply(data, inherits, "tbl_df")))
})


test_that("fars_read_years works with multiple years", {
  years <- c(2013, 2014)
  data <- fars_read_years(years)
  # Test whether returned object is a list
  expect_type(data, "list")
  # Test whether objects within the list are tibbles
  expect_true(all(sapply(data, inherits, "tbl_df")))
})
