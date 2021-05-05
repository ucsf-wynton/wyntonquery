pathname <- system.file("exdata", "accounting", package = "wyntonquery")

jobs <- read_raw_sge_accounting(pathname)
print(jobs)

## Anonymize (although actually already anonymized)
jobs_anon <- anonymize_raw_sge_accounting(jobs)
print(jobs_anon)
