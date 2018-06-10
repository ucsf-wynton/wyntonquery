library(wyntonquery)

today <- format(Sys.time(), "%Y%m%d")
print(today)

## Ignore queues whose nodes are disabled, without load, or flagged as alarmed,
## or on developer and test nodes
q <- queues(filter = "available")
print(q)

## Functioning nodes
hostnames <- sort(unique(q$hostname))
print(hostnames)

## Query nodes
raw <- on_hostname(hostnames, system_info())
saveRDS(raw, file = sprintf("system_info,%s.rds", today))
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
