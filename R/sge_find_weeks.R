#' Index the SGE accounting file for ISO-8601 weeks
#'
#' @param file (character) The SGE \file{accounting} file to read.
#'
#' @param index (numeric vector) File byte offsets of job entries.
#'
#' @param until (character vector) If a week read that is part of this
#' set, the terminate the scanning.  The weeks should be specified
#' using `%GW%V` week format, e.g. `2019W01`.
#'
#' @param n_max (numeric) The maximum number of weeks to read.
#'
#' @param delta (numeric) Maximum number of rows to skip forward and
#' backword in the divide-and-conquer search strategy.
#'
#' @param debug (logical) If TRUE, display debug messages.
#'
#' @return A named list of file-byte offsets of the first job with
#' and end time in the corresponding week.  The entries are named
#' using `%GW%V` week format, e.g. `2021W48`.
#'
#' @importFrom progressr progressor
#' @export
sge_find_weeks <- function(file, index, until = NULL, n_max = Inf, delta = 100000, debug = FALSE) {
  ntry <- function(expr, envir = parent.frame(), tries = 5L, wait = 1.0) {
    expr <- substitute(expr)
    for (kk in seq_len(tries)) {
      value <- tryCatch({
        eval(expr, envir = envir)
      }, error = identity)
      if (!inherits(value, "error")) return(value)
      Sys.sleep(wait)
      tries <- tries - 1L
    }
    eval(expr, envir = envir)
  } # ntry()
  
  weeks <- list()
  delta_org <- delta

  by <- "end_time"
  
  ## Report on progress along 'index' every 1000:th entries
  p <- progressor(length(index) / 1000)
  
  pos <- 1

  ## Assume first read is the start of the first week
  last <- pos
  offset <- index[pos]
  job <- ntry(read_sge_accounting(file, offset = offset, n_max = 1L))
  week <- format(job[[by]], "%GW%V")
  weeks[[week]] <- offset
  last_week <- week
  p(week)

  ## Find the beginning of next week
  delta <- delta_org
  last_same <- last
  pos <- last + delta
  count <- 0L
  forward <- TRUE
  while (pos <= length(index)) {
    offset <- index[pos]
    job <- ntry(read_sge_accounting(file, offset = offset, n_max = 1L))
    week <- format(job[[by]], "%GW%V")
    if (debug) str(list(count = count, pos = pos, week  = week, last_week = last_week))
    if (identical(week, last_week)) {
      last_same <- pos
      if (debug) message("Move forward")
      if (!forward) delta <- max(floor(delta / 2), 1)
      pos <- pos + delta
      forward <- TRUE
    } else {
      ## Invalid entry?
      if (is.na(week)) {
        pos <- pos + if (forward) +1 else -1
	if (pos <= last_same) {
	  pos <- last_same + 1
	  forward <- TRUE
	} else if (pos >= length(index)) {
	  pos <- length(index) - 1
	  forward <- FALSE
	}
	if (pos <= last_same) {
	  attr(weeks, "terminated") <- "NA"
          break
	}
	next
      }
      
      if (delta == 1) {
        if (debug) message("Found next week")
        weeks[[week]] <- c(weeks[[week]], offset)
	amount <- (pos - last)/1000
	if (debug) message(sprintf("%s: amount = %.0f / %.0f", week, amount, length(index)/1000))
        p(week, amount = amount)
        last_week <- week
	delta <- delta_org
        last_same <- pos
	last <- pos
        pos <- pos + delta
        forward <- TRUE
        if (length(weeks) >= n_max) {
	  attr(weeks, "terminated") <- "n_max"
	  if (debug) message("n_max fulfilled: ", n_max)
	  break
	} else if (week %in% until) {
	  attr(weeks, "terminated") <- "until"
	  if (debug) message("until fulfilled: ", week)
	  break
	}
      }
      if (debug) message("Move back")
      if (forward) delta <- max(floor(delta / 2), 1)
      pos <- pos - delta
      forward <- FALSE
      if (pos <= last_same) {
        pos <- last_same + 1
	forward <- TRUE
      }
    }
    if (debug) str(list(count = count, pos = pos, last_same = last_same, delta = delta))
    stopifnot(pos > last_same)
    count <- count + 1L
  }
  p(step = length(index))
  weeks
}
