#' Report on File System Disk Usage
#'
#' @param mounts A character vector of disks (mount points) to query.
#' If `NULL`, all disks are queried.
#'
#' @param local If `TRUE`, only local disks are queried.
#'
#' @return
#' A data.frame (tibble) where each row corresponds to a unique disk.
#'
#' @details
#' This function queries `df -P` (POSIX output format) on the system.
#'
#' @examples
#' ## All disks
#' df <- disk_free()
#' print(df)
#'
#' ## All local disks
#' df <- disk_free(local = TRUE)
#' print(df)
#'
#' ## /tmp 
#' df <- disk_free(c("/tmp"))
#' print(df)
#'
#' @importFrom readr read_table cols col_character col_double
#' @export
disk_free <- function(mounts = NULL, local = FALSE) {
  col_types <- cols(
  Available = col_double(),
    Filesystem    = col_character(),
    `1024-blocks` = col_double(),
    Used          = col_double(),
    Available     = col_double(),
    Capacity      = col_character(),
    `Mounted on`  = col_character()
  )

  cmd <- "df -P"
  if (local) cmd <- paste(cmd, "-l")
  if (!is.null(mounts)) cmd <- paste(c(cmd, mounts), collapse = " ")
  
  df <- read_table(pipe(cmd), col_types = col_types)

  names <- tolower(colnames(df))
  names[names == "1024-blocks"] <- "blocks_1024"
  names <- gsub(" ", "_", names, fixed = TRUE)
  colnames(df) <- names

  ## Parse
  df[["capacity"]] <- as.numeric(sub("%", "", df[["capacity"]], fixed = TRUE)) / 100
  
  df
}

