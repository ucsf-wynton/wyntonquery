#' @importFrom readr read_delim cols col_character col_double col_integer 
#' @export
read_raw_sge_accounting <- function(file, skip = 0L, ...) {
  ## Source: http://manpages.ubuntu.com/manpages/bionic/man5/sge_accounting.5.html
  col_types <- cols(
    qname           = col_character(), ## Name of the cluster queue in which the job has run
    hostname        = col_character(), ## Name of the execution host
    group           = col_character(), ## The effective group id of the job owner when executing the job
    owner           = col_character(), ## Owner of the Grid Engine job
    job_name        = col_character(), ## Job name
    job_number      = col_integer(),   ## Job identifier
    account         = col_character(), ## An account string as specified by the qsub(1) or qalter(1) -A option
    priority        = col_integer(),   ## Priority value assigned to the job
    submission_time = col_double(),    ## [epoch time] Submission time
    start_time      = col_double(),    ## [epoch time] Start time
    end_time        = col_double(),    ## [epoch time] End time 
    failed          = col_integer(),   ## Indicates the problem which occurred in case a job failed (at the system level, as opposed to the job script or binary having non-zero exit status)
    exit_status     = col_integer(),   ## Exit status of the job script (or Grid Engine-specific status in case of certain error conditions)
 
    ru_wallclock    = col_double(),    ## [POSIX time interval]
    
    ## struct rusage {
    ##    struct timeval ru_utime; /* user CPU time used */
    ##    struct timeval ru_stime; /* system CPU time used */
    ##    long   ru_maxrss;        /* maximum resident set size */
    ##    long   ru_ixrss;         /* integral shared memory size */
    ##    long   ru_idrss;         /* integral unshared data size */
    ##    long   ru_isrss;         /* integral unshared stack size */
    ##    long   ru_minflt;        /* page reclaims (soft page faults) */
    ##    long   ru_majflt;        /* page faults (hard page faults) */
    ##    long   ru_nswap;         /* swaps */
    ##    long   ru_inblock;       /* block input operations */
    ##    long   ru_oublock;       /* block output operations */
    ##    long   ru_msgsnd;        /* IPC messages sent */
    ##    long   ru_msgrcv;        /* IPC messages received */
    ##    long   ru_nsignals;      /* signals received */
    ##    long   ru_nvcsw;         /* voluntary context switches */
    ##    long   ru_nivcsw;        /* involuntary context switches */
    ## };
    ## Source: http://manpages.ubuntu.com/manpages/trusty/man2/getrusage.2.html
    ru_utime         = col_double(),   ## [seconds] Total amount of time spent executing in user mod
    ru_stime         = col_double(),   ## [seconds] Total amount of time spent executing in kernel mode
    ru_maxrss        = col_double(),   ## [kB] maximum resident set size
    ru_ixrss         = col_double(),   ## [kB] integral shared memory size (UNUSED)
    ru_ismrss        = col_double(),   ## [kB] integral ??? size
    ru_idrss         = col_double(),   ## [kB] integral unshared data size (UNUSED)
    ru_isrss         = col_double(),   ## [kB] integral unshared stack size (UNUSED)
    ru_minflt        = col_double(),   ## [count] page reclaims (soft page faults)
    ru_majflt        = col_double(),   ## [count] page faults (hard page faults)
    ru_nswap         = col_double(),   ## [] swaps (UNUSED)
    ru_inblock       = col_double(),   ## [count] block input operations
    ru_oublock       = col_double(),   ## [count] block output operations
    ru_msgsnd        = col_double(),   ## [] IPC messages sent (UNUSED)
    ru_msgrcv        = col_double(),   ## [] IPC messages received (UNUSED)
    ru_nsignals      = col_double(),   ## [] signals received (UNUSED)
    ru_nvcsw         = col_double(),   ## [count] voluntary context switches (number of times a context switch resulted due to a process voluntarily giving up the processor before its time slice was completed (usually to await availability of a resource)
    ru_nivcsw        = col_double(),   ## [count] involuntary context switches (number of times a context switch resulted due to a higher priority process becoming runnable or because the current process exceeded its time slice)
         
    project          = col_character(),
    department       = col_character(),  
         
    granted_pe       = col_character(),   ##        The parallel environment which was selected for the job
    slots            = col_integer(),     ##        The number of slots which were dispatched to the job by the scheduler
    task_number      = col_integer(),     ##        The CPU time usage in seconds
         
    cpu              = col_double(),      ## [s]    The CPU time usage in seconds
    mem              = col_double(),      ## [GB*s] The integral  memory  usage  in  Gbytes  seconds
    io               = col_double(),      ## [GB]   The  amount  of data transferred in input/output operations in GB (if available, otherwise 0)
         
    category         = col_character(),
         
    iow              = col_double(),      ## [seconds] The input/output wait time in seconds (if available, otherwise 0)
    pe_taskid        = col_character(),   ## If this identifier is not equal to NONE, the task was part of   parallel job, and was passed to Grid Engine via the qrsh -inherit interface
         
    maxvmem          = col_double(),      ## [bytes] The maximum vmem size in bytes
    arid             = col_integer(),     ## Advance reservation identifier
    ar_sub_time      = col_double()       ## [epoch time] Advance reservation submission time if the job uses the resources of an advance reservation; otherwise "0"
  )
  
  col_names <- names(col_types$cols)
  
  x <- read_delim(file = file, delim = ":", col_names = col_names, col_types = col_types, skip = skip, ...)

  class(x) <- c("raw_sge_accounting", class(x))

  x
}

#' @export
as_sge_accounting <- function(x, ...) UseMethod("as_sge_accounting")

#' @export
as_sge_accounting.raw_sge_accounting <- function(x, ...) {
  origin <- as.POSIXct("1970-01-01 00:00.00 UTC", tz = "GMT")
  
  ## POSIX timestamps
  for (name in c("submission_time", "start_time", "end_time")) {
    x[[name]] <- as.POSIXct(x[[name]], origin = origin)
  }
  
  ## Transfer 
  x$io <- x$io * 1024 * 1000^2              ## in bytes
  x$ru_maxrss <- x$ru_maxrss * (1000/1024)  
  x$mem <- x$mem * (1000/1024) * 1000^3     ## in bytes
  
  class(x) <- c("sge_accounting", setdiff(class(x), "raw_sge_accounting"))
  x
}

#' @export
sge_accounting <- function(x, cols = NULL) {
  if (!is.null(cols)) {
    t <- x[, cols]
    class(t) <- class(x)
    x <- t
  }
  class(x) <- c("sge_accounting", class(x))
  x
}

#' @importFrom prettyunits pretty_bytes pretty_sec
#' @export
print.sge_accounting <- function(x, format = c("pretty", "raw"), ...) {
  format <- match.arg(format)
  if (format == "pretty") {
    ## Time intervals (in seconds)
    for (name in c("ru_wallclock", "ru_utime", "ru_stime", "cpu")) {  # "iow"
      if (!name %in% names(x)) next
      x[[name]] <- pretty_sec(x[[name]])
    }
  
    ## Memory / Transfer (in bytes)
    for (name in c("ru_maxrss", "ru_ixrss", "ru_ismrss", "ru_idrss", "ru_isrss", "io")) {
      if (!name %in% names(x)) next
      x[[name]] <- pretty_bytes(x[[name]])
    }

    ## Memory / Transfer (in bytes)
    for (name in c("mem")) {
      if (!name %in% names(x)) next
      x[[name]] <- pretty_bytes(x[[name]])
    }
  }

  NextMethod()
## mem          8.389KBs B*s
## maxvmem      2.199MB
}
