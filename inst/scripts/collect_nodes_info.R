library(wyntonquery)

now <- format(Sys.time(), "%Y%m%d-%H%M%S")
print(now)

qh <- read_qhost()
qh <- subset(qh, !is.na(load) & !grepl("-(int|test)[0-9]+$", hostname))
print(qh)

ci <- on_hostname(qh$hostname, read_cpuinfo())
print(ci)

saveRDS(ci, file = sprintf("collect_nodes_info,%d.rds", now))

print(sessionInfo())
