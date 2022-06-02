#' Query BeeGFS for Disk Quota
#'
#' @param which Subset of users or groups to be queried.
#' If NULL, quotas for all are queried.
#' If `<current>`, then the current user or group is used.
#' If 
#'
#' @param what Either `"user"` or `"group"`
#'
#' @param storagepool A storage-pool ID (integer) to be queried.
#'
#' @return
#' A data.frame (tibble)
#'
#' @details
#' This function queries `beegfs-ctl` on the system.
#'
#' @examples
#' \donttest{\dontrun{
#' ## The current user
#' quota <- beegfs_quota(what = "user", storagepool = 11L)
#' print(quota)
#'
#' ## The current group
#' quota <- beegfs_quota(what = "group", storagepool = 11L)
#' print(quota)
#'
#' ## All groups
#' quota <- beegfs_quota(which = NULL, what = "group", storagepool = 12L)
#' print(quota)
#' }}
#'
#' @importFrom readr read_csv cols col_character col_integer
#' @importFrom utils file_test
#' @export
beegfs_quota <- function(which = "<current>", storagepool, what = c("user", "group")) {
  what <- match.arg(what)
  
  if (!is.null(which)) {
    stopifnot(!anyNA(which), length(which) >= 1L, length(which) <= 2L)
    stopifnot(is.numeric(which) || is.character(which))
    if (is.character(which)) {
      stopifnot(length(which) == 1L)
      if (identical(which, "<current>")) {
        which <- switch(what, user = "<UID>", group = "<GID>")
      }
      if (identical(which, "<UID>")) {
        which <- as.integer(system2("id", args = "--user", stdout = TRUE))
      } else if (identical(which, "<GID>")) {
        which <- as.integer(system2("id", args = "--group", stdout = TRUE))
      }
      stopifnot(length(which) == 1L)
    }
  }
  
  storagepool <- as.integer(storagepool)
  stopifnot(
    length(storagepool) == 1L,
    is.integer(storagepool),
    !anyNA(storagepool),
    storagepool >= 0
  )

  bin <- Sys.which("beegfs-ctl")
  stopifnot(file_test("-f", bin), file_test("-x", bin))
  
  args <- c("--getquota")
  if (!is.null(storagepool)) {
    args <- c(args, sprintf("--storagepoolid=%d", storagepool))
  }
  args <- c(args, switch(what, user = "--uid", group = "--gid"))
  if (is.null(which)) {
    args <- c(args, "--all")
  } else {
    args <- c(args, which)
  }
  args <- paste(args, collapse = " ")

  bfr <- system2(bin, args = args, stdout = TRUE)
  status <- attr(bfr, "status")
  if (!is.null(status)) {
    stop(sprintf("Failed to query %s. Status code: %s",
                 paste(c(bin, args), collapse = " "), status))
  }

  ## Parse raw data
  ## (a) drop unwanted lines
  bfr <- bfr[nzchar(bfr)]
  bfr <- grep("^[-\\|]+$", bfr, invert = TRUE, value = TRUE)
  bfr <- grep("user/group.*size.*chunk files", bfr, invert = TRUE, value = TRUE)
  bfr <- grep("name.*id.*used.*hard.*used.*hard", bfr, invert = TRUE, value = TRUE)

  ## (b) parse storage pools, if given
  pattern <- "Quota information for storage pool ([[:alnum:]]+) [(]ID: ([[:digit:]]+)[)]:"
  bfr <- gsub(pattern, "storagepoolid=\\2,\\1", bfr)

  ## (c) prune rendered table
  bfr <- gsub("||", "|", bfr, fixed = TRUE)
  bfr <- gsub("(^[[:space:]]+|[[:space:]]+$)", "", bfr)
  bfr <- gsub("\\|[[:space:]]*", ",", bfr)
  
  ## (d) prune units
  bfr <- gsub("Byte", "B", bfr, fixed = TRUE)
  bfr <- gsub("unlimited", "NA", bfr, fixed = TRUE)

  ## (e) process storage pools
  pattern <- "^storagepoolid="
  idxs <- grep(pattern, bfr)
  if (length(idxs) > 0) {
    bfr <- paste(gsub(pattern, "", bfr[idxs]), bfr[idxs+1L], sep=",")
  } else {
    bfr <- sprintf("%d,NA,%s", storagepool, bfr)
  }
  
  ## (f) read
  col_types <- cols(
    storagepool   = col_integer(),
    label         = col_character(),
    name          = col_character(),
    id            = col_integer(),
    size          = col_character(),
    max_size      = col_character(),
    files         = col_integer(),
    max_files     = col_integer()
  )
  col_names <- names(col_types$cols)
  col_names[col_names == "name"] <- what
  col_names[col_names == "id"] <- switch(what, user = "uid", group = "gid")
  names(col_types$cols) <- col_names
  df <- read_csv(I(bfr), col_types = col_types, col_names = col_names)

  ## Parse unit sizes
  for (field in c("size", "max_size")) {
    value <- df[[field]]
    value <- gsub("iB", "", value)
    value <- gsub("T", "*1024G", value)
    value <- gsub("G", "*1024M", value)
    value <- gsub("M", "*1024K", value)
    value <- gsub("K", "*1024", value)
    value <- gsub("B", "*1", value)
    value <- vapply(value, USE.NAMES = FALSE, FUN.VALUE = NA_real_,
                    FUN = function(x) eval(parse(text=x)))
    df[[field]] <- value
  }

  df
}
