#' Read a tab-delimited SGE accounting file (without parsing it)
#'
#' @param file (character) The SGE \file{accounting} file to read.
#'
#' @param skip (integer) Number of lines to skip before parsing file content.
#'
#' @param \ldots (optional) Addition arguments passed to [readr::read_delim()].
#'
#' @details
#' The SGE \file{accounting} file is typically located in a subfolder of
#' the folder \file{$SGE_ROOT/$SGE_CELL/}.  On Wynton HPC, the pathname
#' is given by `sge_accounting_file()`.
#'
#' @example incl/raw_sge_accounting.R
#'
#' @importFrom readr read_delim
#' @export
read_raw_sge_accounting <- function(file, skip = 4L, ...) {
  col_types <- sge_accounting_col_types()
  col_names <- names(col_types$cols)
  header <- if (skip > 0L) readLines(file, n = skip) else character(0L)
  x <- read_delim(file = file, delim = ":", col_names = col_names, col_types = col_types, skip = skip, ...)
  attr(x, "header") <- header
  class(x) <- c("raw_sge_accounting", class(x))
  x
}

#' @param x (raw_sge_accounting) An `tibble` data frame of class
#' `raw_sge_accounting`.
#'
#' @param header (character vector) Zero of more header lines to be written
#' at the top of the file.
#'
#' @rdname read_raw_sge_accounting
#' @importFrom readr write_delim
#' @export
write_raw_sge_accounting <- function(x, file, header = attr(x, "header"), ...) {
  stopifnot(inherits(x, "raw_sge_accounting"))

  ## AD HOC: Drop trailing zeros in doubles
  ## FIXME: Some double fields should not undergo this, at least
  ## if we look at the input data /HB 2021-05-04
  modes <- vapply(x, FUN = storage.mode, FUN.VALUE = NA_character_)
  dbl <- which(modes == "double")
  x[dbl] <- lapply(x[dbl], FUN = function(x) {
    gsub("[.]0*$", "", sprintf("%f", x))
  })

  has_header <- (length(header) > 0L)
  if (has_header) writeLines(header, con = file)
  
  write_delim(x, file = file, delim = ":", col_names = FALSE, ..., append = has_header)
}



#' @importFrom readr cols col_character col_double col_integer
sge_accounting_col_types <- function() {
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
 
    ru_wallclock    = col_double(),    ## [time interval] Difference between 'end_time' and 'start_time', except that if the job fails, it is zero.
    
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
    ru_utime         = col_double(),   ## [time interval] Total amount of time spent executing in user mod (seconds)
    ru_stime         = col_double(),   ## [time interval] Total amount of time spent executing in kernel mode (seconds)
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
         
    granted_pe       = col_character(),   ## The parallel environment which was selected for the job
    slots            = col_integer(),     ## The number of slots which were dispatched to the job by the scheduler
    task_number      = col_integer(),     ##        
         
    cpu              = col_double(),      ## [time interval] The CPU time usage in seconds
    mem              = col_double(),      ## [GB*s] The integral memory usage in Gbytes seconds
    io               = col_double(),      ## [GB] The  amount of data transferred in input/output operations in GB (if available, otherwise 0)
         
    category         = col_character(),
         
    iow              = col_double(),      ## [time interval] The input/output wait time in seconds (if available, otherwise 0)
    pe_taskid        = col_character(),   ## If this identifier is not equal to NONE, the task was part of parallel job, and was passed to Grid Engine via the qrsh -inherit interface
         
    maxvmem          = col_double(),      ## [bytes] The maximum vmem size in bytes
    arid             = col_integer(),     ## Advance reservation identifier
    ar_sub_time      = col_double()       ## [epoch time] Advance reservation submission time if the job uses the resources of an advance reservation; otherwise "0"
  )
}





#' Coerce to an 'sge_accounting' Object
#'
#' @param x The object to be coerced.
#' 
#' @param \ldots (optional) Addition arguments passed to the S3 methods.
#'
#' @export
as_sge_accounting <- function(x, ...) UseMethod("as_sge_accounting")

#' @export
as_sge_accounting.raw_sge_accounting <- function(x, ...) {
  origin <- as.POSIXct("1970-01-01 00:00.00 UTC", tz = "GMT")

  ## Setting missing values
  x$ru_wallclock[x$failed] <- NA_real_
  x$arid[x$arid == 0] <- NA_real_
  x$ar_sub_time[is.na(x$arid)] <- NA_real_
  for (name in c("granted_pe", "pe_taskid")) {
    value <- x[[name]]
    value[value == "NONE"] <- NA_character_
    x[[name]] <- value
  }
  
  ## epoch times
  for (name in c("submission_time", "start_time", "end_time", "ar_sub_time")) {
    value <- x[[name]]
    value[value == 0] <- NA_real_
    x[[name]] <- as.POSIXct(value, origin = origin)
  }

  ## time interval
  for (name in c("ru_wallclock", "ru_utime", "ru_stime", "iow", "cpu")) {
    value <- as.difftime(x[[name]], units = "secs")
    x[[name]] <- value
  }

  ## Convert kB to bytes (B)
  for (name in c("ru_maxrss", "ru_ixrss", "ru_ismrss", "ru_idrss", "ru_isrss")) {
    x[[name]] <- x[[name]] * 1000
  }

  ## Convert GB to bytes (B) (or GB*s to B*s)
  for (name in c("mem", "io")) {
    x[[name]] <- x[[name]] * 1000^3
  }
  
  attr(x, "spec") <- NULL
  class(x) <- c("sge_accounting", setdiff(class(x), "raw_sge_accounting"))
  x
}

#' @importFrom prettyunits pretty_bytes pretty_dt pretty_sec
#' @export
print.sge_accounting <- function(x, format = c("pretty", "raw"), ...) {
  format <- match.arg(format)
  if (format == "pretty") {
    ## Time intervals (in seconds)
    for (name in c("ru_wallclock", "ru_utime", "ru_stime", "cpu")) {  # "iow"
      if (!name %in% names(x)) next
      ## WORKAROUND: pretty_ms() does not support NA:s
      value <- x[[name]]
      ok <- which(!is.na(value))
      x[[name]][ok] <- pretty_dt(value[ok])
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
}


#' Locate the SGE Accounting File on the Current System
#'
#' @param filename (character string) The name of the accounting file.
#'
#' @param path (character string) The path to the accounting file.
#'
#' @return (character string) The pathname to the SGE accounting file.
#' If not found, and error is thrown.
#'
#' @importFrom utils file_test
#' @export
sge_accounting_file <- function(filename = "accounting", path = do.call(file.path, args = as.list(c(Sys.getenv(c("SGE_ROOT", "SGE_CELL")), "common")))) {
  stopifnot(file_test("-d", path))
  pathname <- file.path(path, filename)
  stopifnot(file_test("-f", pathname))
  pathname
}


#' Read an SGE accounting
#'
#' @param file (character) The SGE \file{accounting} file to read.
#'
#' @param skip (integer) The number of lines to skip at the top of the file.
#'
#' @param \ldots Additional arguments passed to [read_raw_sge_accounting()].
#'
#' @return A `tibble` data frame with columns:
#'
#'  * `qname` (character) - name of the cluster queue in which the job has run
#'
#'  * `hostname` (character) - name of the execution host
#'
#'  * `group` (character) - the effective group id of the job owner when
#'    executing the job
#'
#'  * `owner` (character) - owner of the Grid Engine job
#'
#'  * `job_name` (character) - job name
#'
#'  * `job_number` (integer) - job identifier
#'
#'  * `account` (character) - an account string as specified by the `qsub` or `qalter`
#'
#'  * `priority` (integer) - priority value assigned to the job
#'
#'  * `submission_time` (dttm) - submission time
#'
#'  * `start_time` (dttm) - start time
#'
#'  * `end_time` (dttm) - end time
#'
#'  * `failed` (integer) - indicates the problem which occurred in case a job
#'    failed (at the system level, as opposed to the job script or binary having
#'    non-zero exit status).  Indicates the problem which occurred in case a job
#'    could not be started on the execution host (e.g. because the owner of the 
#'    job did not have a valid account on that machine). If Sun Grid Engine
#'    tries to start a job multiple times, this may lead to multiple entries in
#'    the accounting file corresponding to the same job ID
#'
#'  * `exit_status` (integer) - exit status of the job script (or Grid
#'    Engine-specific status in case of certain error conditions).
#'    The exit status is determined by following the normal shell conventions.
#'    If the command terminates normally the value of the command is its exit
#'    status. However, in the case that the command exits abnormally, a value
#'    of 0200 (octal), 128 (decimal) is added to the value of the command to
#'    make up the exit status.
#'    For example: If a job dies through signal 9 (`SIGKILL`) - probably issued
#'    by Grid Engine through `qdel`, or because the job exceeded time or memory
#'    hard limits - then the exit status is 128 + 9 = 137.
#'
#'  * `ru_wallclock` (drtn) - Difference between 'end_time' and 'start_time'
#'    (time interval), except that if the job fails, it is zero.
#'
#'  * `ru_utime` (drtn) - user CPU time (in seconds) used, i.e. total amount of
#'    time spent executing in user mode
#'
#'  * `ru_stime` (drtn) - system CPU time (in seconds) used, i.e. total amount
#'    of time spent executing in kernel mode
#'
#'  * `ru_maxrss` (character) - maximum resident set size (in kB)
#'
#'  * `ru_ixrss` (character) - integral shared memory size (in kB) \[UNUSED\]
#'
#'  * `ru_ismrss` (character) - ???
#'
#'  * `ru_idrss` (character) - integral unshared data size (in kB) \[UNUSED\]
#'
#'  * `ru_isrss` (character) - integral unshared stack size (in kB) \[UNUSED\]
#'
#'  * `ru_minflt` (numeric) - page reclaims (soft page faults)
#'
#'  * `ru_majflt` (numeric) - page faults (hard page faults)
#'
#'  * `ru_nswap` (numeric) - number of swaps \[UNUSED\]
#'
#'  * `ru_inblock` (numeric) - number of block input operations
#'
#'  * `ru_oublock` (numeric) - number of block output operations
#'
#'  * `ru_msgsnd` (numeric) - number of IPC messages sent \[UNUSED\]
#'
#'  * `ru_msgrcv` (numeric) - number of IPC messages received \[UNUSED\]
#'
#'  * `ru_nsignals` (numeric) - number of signals received
#'
#'  * `ru_nvcsw` (numeric) - number of voluntary context switches (number of
#'    times a context switch resulted due to a process voluntarily giving up
#'    the processor before its time slice was completed (usually to await
#'    availability of a resource)
#'
#'  * `ru_nivcsw` (numeric) - number of involuntary context switches (number of
#'    times a context switch resulted due to a higher priority process becoming
#'    runnable or because the current process exceeded its time slice)
#'
#'  * `project` (character) -
#'
#'  * `department` (character) -
#'
#'  * `granted_pe` (character) - the parallel environment which was selected
#'    for the job
#'
#'  * `slots` (integer) - the number of slots which were dispatched to the job
#'    by the scheduler
#'
#'  * `task_number` (integer) -
#'
#'  * `cpu` (drtn) - The CPU time usage (in seconds)
#'
#'  * `mem` (character) - the integral memory usage (in GB seconds)
#'
#'  * `io` (character) - the amount of data transferred in input/output
#'    operations (in GB) if available, otherwise `0`
#'
#'  * `category` (character) -
#'
#'  * `iow` (drtn) - the input/output wait time (in seconds) if available,
#'    otherwise 0
#'
#'  * `pe_taskid` (character) - if this identifier is not equal to `NONE`, the
#'    task was part of parallel job, and was passed to Grid Engine via the
#'    `qrsh`-inherit interface
#'
#'  * `maxvmem` (numeric) - the maximum vmem size (in bytes)
#'
#'  * `arid` (numeric) - advance reservation identifier
#'
#'  * `ar_sub_time` (dttm) - advance reservation submission time, if the job
#'    uses the resources of an advance reservation, otherwise `0`
#'
#'
#' @section Failed code:
#' 
#' |Code | Description                                    | OK | Explanation                                             |
#' | --: | ---------------------------------------------- | -- | ------------------------------------------------------- |
#' |   0 | no failure                                     | Y  | ran and exited normally                                 |
#' |   1 | assumedly before job                           | N  | failed early in execd                                   |
#' |   3 | before writing config                          | N  | failed before execd set up local spool                  |
#' |   4 | before writing PID                             | N  | shepherd failed to record its pid - filesystem problem? |
#' |   6 | setting processor set                          | N  | failed setting up processor set (obsolete)              |
#' |   7 | before prolog                                  | N  | failed before prolog                                    |
#' |   8 | in prolog                                      | N  | failed in prolog                                        |
#' |   9 | before pestart                                 | N  | failed before starting PE                               |
#' |  10 | in pestart                                     | N  | failed in PE starter                                    |
#' |  11 | before job                                     | N  | failed in shepherd before starting job                  |
#' |  12 | before pestop                                  | Y  | ran, but failed before calling PE stop procedure        |
#' |  13 | in pestop                                      | Y  | ran, but PE stop procedure failed                       |
#' |  14 | before epilog                                  | Y  | ran, but failed before calling epilog                   |
#' |  15 | in epilog                                      | Y  | ran, but failed in epilog                               |
#' |  16 | releasing processor set                        | Y  | ran, but processor set could not be released (obsolete) |
#' |  17 | through signal                                 | Y  | job killed by signal (possibly qdel)                    |
#' |  18 | shepherd returned error                        | N  | shepherd died somehow                                   |
#' |  19 | before writing exit_status                     | N  | shepherd didn't write reports correctly - probably program or machine crash |
#' |  20 | found unexpected error file                    | ?  | shepherd encountered a problem                          |
#' |  21 | in recognizing job                             | N  | qmaster asked about an unknown job (not in accounting?) |
#' |  24 | migrating (checkpointing jobs)                 | Y  | ran, will be migrated                                   |
#' |  25 | rescheduling                                   | Y  | ran, will be rescheduled                                |
#' |  26 | opening output file                            | N  | failed opening stderr/stdout file                       |
#' |  27 | searching requested shell                      | N  | failed finding specified shell                          |
#' |  28 | changing to working directory                  | N  | failed changing to start directory                      |
#' |  29 | AFS setup                                      | N  | failed setting up AFS security                          |
#' |  30 | application error returned                     | Y  | ran and exited 100 - maybe re-scheduled                 |
#' |  36 | checking configured daemons                    | N  | failed because of configured remote startup daemon      |
#' |  38 | adding supplementary group                     | N  | failed adding supplementary gid to job                  |
#' |  37 | qmaster enforced h_rt, h_cpu, or h_vmem limit  | Y  | ran, but killed due to exceeding run time limit         |
#' | 100 | assumedly after job                            | Y  | ran, but killed by a signal (perhaps due to exceeding resources), task died, shepherd died (e.g. node crash), etc. |
#' 
#' 
#' The following failed codes are specific to MS Windows:
#' 
#' |Code | Description                                    | OK | Explanation                                             |
#' | --: | ---------------------------------------------- | -- | ------------------------------------------------------- |
#' |  31 | accessing sgepasswd file                       | N  | failed because sgepasswd not readable*                  |
#' |  32 | entry is missing in password file              | N  | failed because user not in sgepasswd*                   |
#' |  33 | wrong password                                 | N  | failed because of wrong password against sgepasswd*     |
#' |  34 | communicating with Grid Engine Helper Service  | N  | failed because of failure of helper service*            |
#' |  35 | before job in Grid Engine Helper Service       | N  | failed because of failure running helper service*       |
#' 
#' 
#' Source: `man sge_status`.
#' 
#' 
#' @section Common exit codes:
#'
#' |Code     | Description                                                 |
#' | --:     | ----------------------------------------------------------- |
#' |   0     | Success                                                     |
#' |   1     | Catchall for general errors                                 |
#' |   2     | Misuse of shell builtins (according to Bash documentation)  |
#' | 126     | Command invoked cannot execute, e.g. `/dev/null`            |
#' | 127     | "command not found"                                         |
#' | 128     | Invalid argument to exit, e.g. `exit 3.14`                  |
#' | 128 + n | Fatal error signal n                                        |
#' | 134     | 128 +   6 = 128 + `SIGABRT` - Abort signal from abort       |
#' | 135     | 128 +   7 = 128 + `SIGBUS`  - Bus error (bad memory access) |
#' | 136     | 128 +   8 = 128 + `SIGFPE`  - Floating-point exception      |
#' | 137     | 128 +   9 = 128 + `SIGKILL`                                 |
#' | 138     | 128 +  10 = 128 + `SIGUSR1`                                 |
#' | 140     | 128 +  12 = 128 + `SIGUSR2`                                 |
#' | 255     | 128 + 127 = Exit status out of range, e.g.`exit -1`         |
#'
#' Comment: `exit` only takes integers in \[0,255\]
#'
#'
#' @section Benchmarking:
#' The \file{accounting} on Wynton HPC took ~2 minutes to read when it
#' was 4.8 GB in size and ~6-8 minutes when it was 12 GB in size.
#'
#'
#' @example incl/sge_accounting.R
#'
#' @references
#' * `man accounting`
#' * `man sge_status`
#'
#' @export
read_sge_accounting <- function(file = sge_accounting_file(), skip = 4L, ...) {
  data <- read_raw_sge_accounting(file = file, skip = skip, ...)
  data <- as_sge_accounting(data)
  data
}


#' Parse SGE Accounting 'category' Field
#'
#' @param x An `sge_accounting` object.
#' 
#' @param \ldots (optional) Not used.
#'
#' @return A `tibble` data frame with columns corresponding to the requested
#' properties.
#'
#' @export
parse_category <- function(x, ...) {
  UseMethod("parse_category")
}

#' @param properties (character vector) The properties to extract.
#'
#' @importFrom tibble as_tibble
#' @export
parse_category.sge_accounting <- function(x, properties = c("h_rt", "s_rt", "mem_free"), ...) {
  properties <- match.arg(properties, several.ok = TRUE)

  category <- x$category
  n <- length(category)

  res <- list()
  for (field in properties) {
    if (field %in% c("h_rt", "s_rt")) {
      value <- rep(NA_integer_, times = n)
      pattern <- sprintf(".*%s=([[:digit:]]+).*", field)
      keep <- grep(pattern, category)
      if (length(keep) > 0L) {
        value[keep] <- as.integer(gsub(pattern, "\\1", category[keep]))
      }
      value <- .difftime(value, units = "secs")
    } else if (field == "mem_free") {
      value <- rep(NA_real_, times = n)
      pattern <- sprintf(".*%s=([[:digit:]]+[MG]).*", field)
      keep <- grep(pattern, category)
      if (length(keep) > 0L) {
        tmp <- gsub(pattern, "\\1", category[keep])
	tmp <- gsub("G$", "000M", tmp)
	tmp <- gsub("M$", "000K", tmp)
	tmp <- gsub("K$", "000", tmp)
        value[keep] <- tmp
      }
      value <- as.numeric(value)
    }
    res[[field]] <- value
  }

  as_tibble(res)
}
