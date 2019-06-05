library(wyntonquery)

today <- format(Sys.time(), "%Y%m%d")
cat("Today's date: ", today, "\n", sep="")
cat("\n")

## All queues
q <- queues()
cat("\nAll known queues:\n")
print(q)

## Ignore queues whose nodes are disabled, without load, or flagged as alarmed,
## or on developer and test nodes
q <- available(q)
cat("All available queues:\n")
print(q)

## All available compute nodes
avail_hostnames <- sort(unique(q$hostname))
avail_hostnames <- setdiff(avail_hostnames, "qb3-gpudev1")
cat("All known available hostnames:\n")
print(avail_hostnames)

incl <- Sys.getenv("R_WYNTONQUERY_INCLUDE", "")
incl <- gsub("\"", "", incl)
cat("R_WYNTONQUERY_INCLUDE: ", sQuote(incl), "\n", sep = "")

if (incl == "") {
  hostnames <- avail_hostnames
} else {
  incl <- unlist(strsplit(incl, split = "[ ,\n]"), use.names = FALSE)
  incl <- incl[nzchar(incl)]
  hostnames <- incl
  cat("Hostnames to include:\n")
  print(hostnames)
}

## Exclude additional hostnames?
excl <- Sys.getenv("R_WYNTONQUERY_EXCLUDE", "")
excl <- unlist(strsplit(excl, split = "[, \n]"), use.names = FALSE)
excl <- excl[nzchar(excl)]
cat("\nHostnames to exclude:\n")
print(excl)

hostnames <- setdiff(hostnames, excl)
cat("All requested hostnames:\n")
print(hostnames)

hostnames <- intersect(hostnames, avail_hostnames)
cat("All requested hostnames after dropping non-available ones:\n")
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
