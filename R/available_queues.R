#' List Available Queues and Their Properties
#'
#' @param parse If `TRUE`, `qhost -f` columns that contain multiple properties
#' (e.g. 'states') are further parsed for easy access.
#'
#' @return
#' A data.frame (tibble) where each row corresponds to a
#' unique queue on a particular host.
#'
#' @examples
#' \donttest{
#' q <- available_queues()
#' print(q)
#'
#' ## Ignore queues whose nodes are disabled, without load, or those
#' ## on developer and test nodes
#' q <- subset(q, !is.na(load_avg) & !disabled & !grepl("-(int|test)", hostname))
#' print(q)
#' }
#'
#' @details
#' This function queries SGE's `qstat -f` on the system.
#'
#' @importFrom readr read_table cols col_character col_double
#' @export
available_queues <- function(parse = TRUE) {
  col_types <- cols(
    queuename        = col_character(),
    qtype            = col_character(),
    `resv/used/tot.` = col_character(),
    load_avg         = col_double(),
    arch             = col_character(),
    states           = col_character()
  )
  
  df <- read_table(pipe("qstat -f | grep -vE '^---'"), col_types = col_types,
                   na = c("-NA-"))
  colnames(df) <- tolower(colnames(df))

  if (parse) {
    ## Parse: queuename -> (hostname, queue)
    parts <- strsplit(df[["queuename"]], split = "@", fixed = TRUE)
    df$queue    <- sapply(parts, FUN = `[`, 1L)
    df$hostname <- sapply(parts, FUN = `[`, 2L)
  
    ## Parse: qtype -> ...
    ## man qtype:
    ##   B(atch), I(nteractive), C(heckpointing), P(arallel),
    ##   combinations thereof, or N(one)
    parts <- strsplit(df[["qtype"]], split = "", fixed = TRUE)
    df$batch         <- sapply(parts, FUN = function(x) "B" %in% x)
    df$interactive   <- sapply(parts, FUN = function(x) "I" %in% x)
    df$checkpointing <- sapply(parts, FUN = function(x) "C" %in% x)
    df$parallel      <- sapply(parts, FUN = function(x) "P" %in% x)
    
    ## Parse: resv/used/tot. -> (reserved, used, total)
    parts <- strsplit(df[["resv/used/tot."]], split = "/", fixed = TRUE)
    df$reserved <- as.integer(sapply(parts, FUN = `[`, 1L))
    df$used     <- as.integer(sapply(parts, FUN = `[`, 2L))
    df$total    <- as.integer(sapply(parts, FUN = `[`, 3L))
    
    ## Parse: states -> (unknown, ..., Preempted)
    ## man qstat:
    ##   the state  of the queue - one of u(nknown), a(larm), A(larm),
    ##   C(alendar suspended), s(uspended), S(ubordinate), d(isabled),
    ##   D(isabled), E(rror), c(configuration ambiguous), o(rphaned),
    ##   P(reempted), or some combination thereof.
    parts <- strsplit(df[["states"]], split = "", fixed = TRUE)
    map <- c(u = "unknown",
             a = "alarm",
             A = "Alarm", 
             C = "Calendar_suspended",
             s = "suspended",
             S = "Subordinate",
             d = "disabled",
             D = "Disabled", 
             E = "Error",
             c = "configuration_ambiguous",
             o = "orphaned",
             P = "Preempted")
    ##   the state  of the queue - one of u(nknown), a(larm), A(larm),
    ##   C(alendar suspended), s(uspended), S(ubordinate), d(isabled),
    ##   D(isabled), E(rror), c(configuration ambiguous), o(rphaned),
    ##   P(reempted), or some combination thereof.
    for (kk in seq_along(map)) {
      state <- names(map)[kk]
      name <- map[kk]
      df[[name]] <- sapply(parts, FUN = function(x) state %in% x)
    }
  }

  df
}
