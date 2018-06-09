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
ci <- on_hostname(hostnames, read_cpuinfo())
print(ci)

saveRDS(ci, file = sprintf("collect_nodes_info,%s.rds", now))

print(sessionInfo())
