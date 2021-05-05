#' Summarize Jobs by Status and Process Priority
#'
#' @param jobs An 'sge_accounting' data.frame.
#'
#' @param \ldots Currently not used.
#'
#' @return A named list with two matrices `total` and `fraction`.
#'
#' @examples
#' \donttest{\dontrun{
#' ## Get all jobs that started during the last 30 days
#' jobs <- read_sge_accounting("accounting.csv", skip = 4L)
#' jobs <- subset(jobs, start_time >= Sys.time() - 30*24*3600)
#'
#' # Summarize by status and process priority
#' stats <- job_summary_by_status(jobs)
#' print(stats)
#' # $total
#' #                      all         0         10         19
#' # success       1546740016 362692738 3389595.99 1180657682
#' # fail           104934943  80500076 1541827.09   22893040
#' # exceeded_h_rt   66864152  61657974    1116.87    5205061
#' # 
#' # $fraction
#' #                      all         0           10          19
#' # success       0.93646756 0.8183633 0.6873464179 0.980978749
#' # fail          0.06353244 0.1816367 0.3126535821 0.019021251
#' # exceeded_h_rt 0.04048263 0.1391222 0.0002264803 0.004324755
#' }}
#'
#' @export
job_summary_by_status <- function(jobs, ...) {
  ## To please 'R CMD check'
  failed <- ru_wallclock <- priority <- NULL
  
  stopifnot(inherits(jobs, "sge_accounting"))

  ## Split up success, fail, and exceeded_h_rt groups
  jobsG <- list(
    success = subset(jobs, failed == 0L),
    fail    = subset(jobs, failed  > 0L)
  )
  resources <- parse_category(jobsG$fail)
  jobsG$exceeded_h_rt <- subset(jobsG$fail, failed == 37L & ru_wallclock >= resources$h_rt)

  ## Subgroup by priorities
  upriorities <- sort(unique(jobs$priority))
  total_cpu <- list()
  for (name in names(jobsG)) {
    res2 <- double(length(upriorities))
    names(res2) <- upriorities
    for (pp in seq_along(upriorities)) {
      t <- subset(jobsG[[name]], priority == upriorities[pp])
      res2[pp] <- sum(t$cpu, na.rm = TRUE)
    }
    total_cpu[[name]] <- res2
  }
  total_cpu <- do.call(rbind, total_cpu)
  total_cpu <- cbind(all = rowSums(total_cpu), total_cpu)

  ## Fraction
  fraction_cpu <- t(t(total_cpu) / colSums(total_cpu[1:2, , drop = TRUE]))

  list(
    total    = total_cpu,
    fraction = fraction_cpu
  )
}
