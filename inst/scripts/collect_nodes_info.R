library(wyntonquery)

now <- format(Sys.time(), "%Y%m%d-%H%M%S")
print(now)

## Ignore queues whose nodes are disabled, without load, or flagged as alarmed,
## or on developer and test nodes
q <- queues(filter = "available")
print(q)

## Functioning nodes
hostnames <- sort(unique(q$hostname))
print(hostnames)

## Query nods
info <- on_hostname(hostnames, {
  info <- as.list(Sys.info())
  info <- info[!grepl("(user|login)", names(info))]
  info$cpu_info <- cpu_info()
})
print(info)

saveRDS(info, file = sprintf("host_info,%s.rds", now))

print(sessionInfo())
