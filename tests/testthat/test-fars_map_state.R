test_that("fars_map_state returns a map plot with points", {
  library(ggplot2)
  library(vdiffr)
  setwd(system.file("extdata", package = "FARSanalyzr"))
  year <- 2013
  state <- 6
  filename <- make_filename(year)
  ref_data <- fars_read(filename)
  state.num <- as.integer(state)

  ref_data_sub <- dplyr::filter(ref_data, STATE == state.num)
  cali <- subset(map_data("state"), region == "california")
  ref_plot <- ggplot(data = cali, mapping = aes(long, lat)) +
    geom_polygon(color = "black", fill = NA) +
    geom_point(data = ref_data_sub, aes(LONGITUD, LATITUDE)) +
    theme_minimal()
  expect_doppelganger("reference map", ref_plot)

  output_plot <- fars_map_state(state, year)
  expect_doppelganger("default map", output_plot)

})
