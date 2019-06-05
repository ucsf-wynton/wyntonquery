#' Evaluates an R Expression on One or More Hosts
#'
#' @param hostnames A character vector of hostnames.
#'
#' @param expr The \R expression to be evaluated on each host.
#'
#' @param \ldots (optional) Additional arguments passed to [future::future()].
#'
#' @param on_error A character string specifying how to handle errors.
#' If `"stop"` (default), then the error is relayed.
#' If `"asis"`, then the error (the [base::condition] object) is returned.
#'
#' @param cache If `TRUE`, cached results on file are considered.
#'
#' @return
#' A named list with names `hostnames`.
#'
#' @details
#' This function utilizes the \pkg{future} framework and specifically
#' the \pkg{future.batchtools} package to evaluate the expression on
#' a particular host by submitting it via the SGE scheduler.
#'
#' @examples
#' \donttest{\dontrun{
#' q <- available(queues())
#' hostnames <- sort(unique(q$hostname))
#' si <- on_hostname(hostnames, system_info())
#' print(si)
#' }}
#'
#' @importFrom R.cache loadCache saveCache
#' @importFrom future future value plan
#' @importFrom future.batchtools batchtools_sge
#' @export
on_hostname <- function(hostnames, expr, ..., on_error = c("stop", "asis"), cache = FALSE) {
  stopifnot(is.character(hostnames), !anyNA(hostnames))
  expr <- substitute(expr)
  on_error <- match.arg(on_error)
  
  template <- system.file(package = "wyntonquery",
                          "batchtools.sge.tmpl", mustWork = TRUE)
  oplan <- plan(batchtools_sge, template = template)
  on.exit(plan(oplan))

  dirs <- c("wyntonquery")

  fs <- list()
  for (h in hostnames) {
    key <- list(hostname = h)
    if (cache) {
      value <- loadCache(key = key, dirs = dirs)
      if (!is.null(value)) {
        fs[[h]] <- value
	next
      }
    }
    
    resources <- list(custom = c(
      sprintf("-l hostname=%s", h),
      "-l mem_free=1G",
      "-l h_rt=00:00:30"
    ))
    fs[[h]] <- future(expr, substitute = FALSE, ...,
                      resources = resources, label = h)
  }

  ## AD HOC: Using values(fs) would work as long all jobs
  ## return.  If not, we'll get a FutureError.  The below
  ## approach will allow us to fail for some jobs (=hosts)
  ## while still collecting data from the others.
  on_error <- switch(on_error,
    asis  = identity,
    relay = stop
  )
  lapply(names(fs), FUN = function(h) {
    key <- list(hostname = h)
    f <- fs[[h]]
    if (inherits(f, "Future")) {
      tryCatch({
        v <- value(f)
        if (cache) saveCache(v, key = key, dirs = dirs)
        v
      }, error = on_error)
    } else {
      f
    }
  })
}
