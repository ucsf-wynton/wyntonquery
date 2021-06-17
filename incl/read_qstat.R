pathname <- system.file("exdata", "qstat.xml", package = "wyntonquery")

qstat <- read_qstat(pathname)
print(qstat)

## Anonymize (although actually already anonymized)
qstat_anon <- anonymize(qstat)
print(qstat_anon)
