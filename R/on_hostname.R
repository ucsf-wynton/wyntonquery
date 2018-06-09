#' Evaluates an R Expression on One or More Hosts
#'
#' @param hostnames A character vector of hostnames.
#'
#' @param expr The \R expression to be evaluated on each host.
#'
#' @param \ldots (optional) Additional arguments passed to [future::future()].
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
#' q <- queues(filter = "available")
#' hostnames <- sort(unique(q$hostname))
#' si <- on_hostname(hostnames, system_info())
#' print(si)
#' }}
#'
#' @importFrom future future value values plan
#' @importFrom future.batchtools batchtools_sge
#' @export
on_hostname <- function(hostnames, expr, ...) {
  stopifnot(is.character(hostnames), !anyNA(hostnames))
  
  template <- system.file(package = "wyntonquery",
                          "batchtools.sge.tmpl", mustWork = TRUE)
  oplan <- plan(batchtools_sge, template = template)
  on.exit(plan(oplan))

  fs <- list()
  for (h in hostnames) {
    resources <- list(custom = c(
      sprintf("-l hostname=%s", h),
      "-l mem_free=1G",
      "-l h_rt=00:00:30"
    ))
    fs[[h]] <- future(expr, ..., resources = resources, label = h)
  }
  
  values(fs)
}

