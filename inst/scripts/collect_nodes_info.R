library(wyntonquery)

now <- format(Sys.time(), "%Y%m%d-%H%M%S")
print(now)

q <- available_queues()

## Ignore queues whose nodes are disabled, without load, or those
## on developer and test nodes
q <- subset(q, !is.na(load_avg) & !disabled & !grepl("-(int|test)", hostname))

## Queues flagged with an alarm
qa <- subset(q, (alarm | Alarm))
skip <- intersect(
  unique(subset(qa, queue == "short.q")$hostname),
  unique(subset(qa, queue == "long.q")$hostname)
)


## Functioning queues
q <- subset(q, !hostname %in% skip)

## Functioning nodes
hostnames <- sort(unique(q$hostname))

## Query nods
ci <- on_hostname(hostnames, read_cpuinfo())
print(ci)

saveRDS(ci, file = sprintf("collect_nodes_info,%s.rds", now))

print(sessionInfo())
