library(wyntonquery)
library(gtools)

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
raw <- on_hostname(hostnames, try(system_info()), on_error = "asis", cache = TRUE)
saveRDS(raw, file = sprintf("system_info,%s.rds", today))
str(raw)

## Filter out errors
is_error <- vapply(raw, FUN = inherits, c("error", "try-error"), FUN.VALUE = FALSE)
raw <- raw[!is_error]
str(raw)

## Combine
qhost <- subset(qhost(), !hostname %in% c("global"))
todo <- vapply(raw, FUN = inherits, "list", FUN.VALUE = FALSE)
raw2 <- raw
raw2[todo] <- lapply(raw[todo], FUN = host_info, qhost = qhost)
data <- Reduce(rbind, raw2)
data <- data[mixedorder(data$hostname), , drop = FALSE]
saveRDS(data, file = sprintf("host_info,%s.rds", today))
str(data)

## Summarize static information
host_table <- host_table(data)
host_table <- unique(host_table)
host_table <- host_table[, c("Node", "# Physical Cores", "RAM", "Local `/scratch`", "Local `/tmp`", "CPU")]
stopifnot(!anyDuplicated(host_table[[1]]))
cpu <- host_table[["CPU"]]
cpu <- gsub("(R)", "", cpu, fixed = TRUE)
cpu <- gsub("(tm)", "", cpu, fixed = TRUE)
cpu <- gsub(" @ ", " ", cpu, fixed = TRUE)
cpu <- gsub(" CPU ", " ", cpu, fixed = TRUE)
cpu <- gsub(" Processor ", " ", cpu, fixed = TRUE)
host_table[["CPU"]] <- cpu
saveRDS(host_table, file = sprintf("host_table,%s.rds", today))
print(host_table)

## Write TSV file (for website)
pathname <- sprintf("host_table,%s.tsv", today)
cat(sprintf("# Created by: Henrik Bengtsson\n"), file = pathname)
cat(sprintf("# Created on: %s\n", Sys.time()), file = pathname, append = TRUE)
cat(sprintf("# Number of hosts: %d\n", nrow(host_table)), file = pathname, append = TRUE)
cat(sprintf("# Number of cores: %d\n", sum(host_table[["# Physical Cores"]])), file = pathname, append = TRUE)
readr::write_tsv(host_table, path = pathname, col_names = TRUE, append = TRUE)

print(sessionInfo())
