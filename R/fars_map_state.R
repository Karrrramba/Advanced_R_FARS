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
