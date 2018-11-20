library(readr)

#' @importFrom readr read_delim cols col_character col_double col_integer 
#' @export
read_sge_accounting <- function(file, skip = 0L, format = c("raw", "coerced")) {
  format <- match.arg(format)
  
  col_names <- c("qname", "hostname", "group", "owner", "jobname", "jobnumber", "account", "priority", "qsub_time", "start_time", "end_time", "failed", "exit_status", "ru_wallclock", "ru_utime", "ru_stime", "ru_maxrss", "ru_ixrss", "ru_ismrss", "ru_idrss", "ru_isrss", "ru_minflt", "ru_majflt", "ru_nswap", "ru_inblock", "ru_oublock", "ru_msgsnd", "ru_msgrcv", "ru_nsignals", "ru_nvcsw", "ru_nivcsw", "project", "department", "granted_pe", "slots", "taskid", "cpu", "mem", "io", "category", "iow", "maxvmem", "arid", "ar_sub_time", "unknown")

  col_types <- cols(
    .default = col_integer(),
    qname = col_character(),
    hostname = col_character(),
    group = col_character(),
    owner = col_character(),
    jobname = col_character(),
    account = col_character(),
    ru_utime = col_double(),
    ru_stime = col_double(),
    ru_maxrss = col_double(),
    ru_minflt = col_double(),
    ru_inblock = col_double(),
    ru_oublock = col_double(),
    project = col_character(),
    department = col_character(),
    granted_pe = col_character(),
    cpu = col_double(),
    mem = col_double(),
    io = col_double(),
    category = col_character(),
    iow = col_double(),
    maxvmem = col_character(),
    arid = col_double(),
    ar_sub_time = col_integer(),
    unknown = col_integer()
  )

  x <- read_delim(file = file, delim = ":", col_names = col_names, col_types = col_types, skip = skip)

  if (format == "raw") {
    class(x) <- c("raw_sge_accounting", class(x))
  } else if (format == "coercd") {
    origin <- as.POSIXct("1970-01-01 00:00.00 UTC", tz = "GMT")
  
    ## POSIX timestamps
    for (name in c("qsub_time", "start_time", "end_time")) {
      x[[name]] <- as.POSIXct(x[[name]], origin = origin)
    }
  
    ## Transfer 
    x$io <- x$io * 1024 * 1000^2              ## in bytes
    x$ru_maxrss <- x$ru_maxrss * (1000/1024)  
    x$mem <- x$mem * (1000/1024) * 1000^3     ## in bytes
    class(x) <- c("sge_accounting", class(x))
  }

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
