#' @export
available <- function(df, ...) UseMethod("available")


#' Gets Queues that are Available For Jobs
#'
#' @param df A `queues` data frame.
#'
#' @return
#' Return the subset of queues that are on available hosts with a
#' known load and where either the 'short.q' or the 'long.q' queue
#' is available for jobs (= not overloaded).
#'
#' @export
available.queues <- function(df, ...) {
  ## To please R CMD check
  load_avg <- disabled <- hostname <- alarm <- Alarm <- queue <- NULL

  ## Ignore queues whose nodes are disabled or without load
  df <- subset(df, !is.na(load_avg) & !disabled)
 ## Wynton specific: Ignore developer and test nodes
  df <- subset(df, !grepl("-(dev|int|test)", hostname))
  
  ## Wynton specific: Ignore hosts whose short.q or long.q queues
  ## are flagged with an alarm
  dfa <- subset(df, (alarm | Alarm))
  skip <- intersect(
    unique(subset(dfa, queue == "short.q")$hostname),
    unique(subset(dfa, queue == "long.q")$hostname)
  )

  ## Functioning queues
  subset(df, !hostname %in% skip)
}
