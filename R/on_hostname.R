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

