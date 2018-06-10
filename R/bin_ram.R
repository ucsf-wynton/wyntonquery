#' Round RAM Sizes to Nearest Possible RAM Size
#'
#' @param x A numeric vector of RAM sizes to round.
#'
#' @param map A numeric vector of RAM sizes to round.
#'
#' @return A numeric vector of length `length(x)` with
#' values in the `map` set.
#'
#' @examples
#' x <- c(14.5, 506.2, 46.2)
#' print(x)
#' x2 <- round_ram(x)
#' print(x2)
#'
#' @export
round_ram <- function(x, map = getOption("wyntonquery.round_ram.map", c(16, 32, 48, 64, 96, 128, 256, 384, 512, 768, 1024, 2048))) {
  map <- sort(map)
  bins <- c(c(map, Inf) + c(-Inf, map)) / 2
  map[findInterval(x, bins)]
}
