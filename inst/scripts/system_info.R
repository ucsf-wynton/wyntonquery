library(wyntonquery)

today <- format(Sys.time(), "%Y%m%d")
cat("Today's date: ", today, "\n", sep="")

hostnames <- Sys.getenv("R_WYNTONQUERY_INCLUDE", "")
if (hostnames == "") {
  ## All queues
  q <- queues()
  cat("\nAll known queues:\n")
  print(q)
  
  ## Ignore queues whose nodes are disabled, without load, or flagged as alarmed,
  ## or on developer and test nodes
  q <- available(q)
  cat("\nAll available queues:\n")
  print(q)
  
  ## Functioning nodes
  hostnames <- sort(unique(q$hostname))
}

## AD HOC: Drop nodes that should not have queues /HB 2018-09-27
hostnames <- setdiff(hostnames, "qb3-gpudev1")
cat("\nAll known hostnames:\n")
print(hostnames)

## Exclude additional hostnames?
excl <- Sys.getenv("R_WYNTONQUERY_EXCLUDE", "")
excl <- unlist(strsplit(excl, split = "[, \n]"), use.names = FALSE)
cat("\nHostnames to exclude:\n")
print(excl)

hostnames <- setdiff(hostnames, excl)
cat("\nHostnames to query:\n")
print(hostnames)

## Query nodes
raw <- on_hostname(hostnames, try(system_info()), on_error = "asis")
saveRDS(raw, file = sprintf("system_info,%s.rds", today))
print(raw)

## Filter out errors
is_error <- vapply(raw, FUN = inherits, c("error", "try-error"), FUN.VALUE = FALSE)
raw <- raw[!is_error]
print(raw)

## Combine
data <- Reduce(rbind, lapply(raw, FUN = host_info, qhost = qhost()))
saveRDS(data, file = sprintf("host_info,%s.rds", today))
print(data)

## Summarize static information
hosts <- host_table(data)
saveRDS(hosts, file = sprintf("host_table,%s.rds", today))
print(hosts)

## Write TSV file (for website)
readr::write_tsv(hosts, path = sprintf("host_table,%s.tsv", today))

print(sessionInfo())
