#' Read csv file
#'
#' @description
#' This function reads csv-files converts it into a tibble.
#'
#' @param filename A character string with the name of the csv file to be read.
#'
#' @returns Returns a tibble with the content of the specified file.
#'  It checks if the file specified exists and returns an error using
#'  the 'stop' function otherwise.
#'
#' @importFrom readr read_csv
#' @importFrom tibble as_tibble
#'
#' @examples
#' \dontrun{
#' fars_read("accidents_2013.csv")
#' }
#'
#' @export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  tibble::as_tibble(data)
}

#' Create file name for a given year
#'
#' @description
#' This function generates a filename for FARS data for the specified year.
#'
#' @param year A numeric or character vector specifying the year(s) of interest.
#'
#' @returns Returns a character string representing the generated filename.
#'  The character string is formatted as: "accident_YEAR.csv.bz2".
#'
#' @examples
#' \dontrun{
#'   make_filename(2013)
#'   make_filename(c(2013, 2014))
#' }
#'
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' Read FARS data for multiple years
#'
#' @description
#' This function reads data from the Fatality Analysis Reporting System (FARS) for
#' multiple years and returns a list of tibbles, each containing a 'MONTH' and
#' a 'year' column.
#'
#' @param years A vector of numeric values representing the years for which FARS
#'  data should be read.
#'
#' @returns A list of tibbles, each containing FARS data for a specific year
#'  with a 'MONTH' and a 'year' column.
#'  The function returns a warning if no FARS data set is available for
#'  the specified year.
#'
#' @details
#' This function takes the user input uses the 'make_filename' function to
#' create the filenames to search for.
#'
#' @importFrom dplyr mutate select
#' @importFrom utils warning
#' @importFrom methods tryCatch
#'
#' @examples
#' \dontrun{
#'   # Read FARS data for years 2013 and 2014
#'   fars_data <- fars_read_years(c(2013, 2014))
#' }
#'
#' @export
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}


#' Sumamrize FARS data for multiple years
#'
#' @description
#' This functions reads data for the specified years then combines it into
#' a single data frame and summarizes the number of accidents for each month and
#' year.
#'
#' @param years A vector of numeric values representing the years for which FARS
#'  data should be summarized.
#'
#' @returns A single tibble containing the number of accidents for each month and
#'  year.
#'
#' @details This function utilizes the 'fars_read_years' function to obtain
#'  the data for the specified years.
#'
#' @importFrom dplyr bind_rows group_by summarize
#' @importFrom tidyr spread
#'
#' @examples
#' \dontrun{
#'   # Summarize total accidents for each month for the years 2013 and 2014
#'   fars_summarize_years(c(2013, 2014))
#' }
#'
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}


#' Plot FARS data
#'
#' @description
#' This functions plots the locations of incidents recorded on a map of
#' the specified state for the specified year.
#'
#' @param state.num A numeric code representing the state.
#' @param year A numeric value specifying the year for which the data should
#'  be plotted.
#'
#' @returns A plot of the places of accidents (as points) based on latitude and
#'  longitude on the map of the specified state.
#'  Returns a warning if invalid state code and/or an error in case an
#'  invalid year is specified.
#'
#' @importFrom dplyr filter
#' @importFrom graphics points
#' @importFrom maps map
#'
#' @examples
#' \dontrun{
#'   # Map the accidents in the state of California in the year 2014.
#'   fars_map_state(6, 2014)
#' }
#'
#' @export

fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}
