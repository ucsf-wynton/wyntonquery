#' @examples
#' \donttest{\dontrun{
#' q <- available_queues()
#' q <- subset(q, !is.na(load_avg) & !disabled & !grepl("-(int|test)", hostname))
#' 
#' hostnames <- sort(unique(q$hostname))
#' qh <- read_qhost()
#' qh <- subset(qh, !is.na(load) & !grepl("-(int|test)[0-9]+$", hostname))
#' ci <- on_hostname(hostnames, read_cpuinfo())
#' }}
#'
#' @importFrom future future value values plan
#' @importFrom future.batchtools batchtools_sge
#' @export
on_hostname <- function(hostname, ...) {
  template <- system.file(package = "wyntonquery",
                          "batchtools.sge.tmpl", mustWork = TRUE)
  oplan <- plan(batchtools_sge, template = template)
  on.exit(plan(oplan))

  fs <- list()
  for (h in hostname) {
    resources <- list(custom = c(
      sprintf("-l hostname=%s", h),
      "-l mem_free=1G",
      "-l h_rt=00:00:30"
    ))
    fs[[h]] <- future(..., resources = resources, label = h)
  }
  
  values(fs)
}

