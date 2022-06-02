library(wyntonquery)

pathname <- sge_accounting_file()
pathname_index <- sprintf("%s.index_by_week.rds", basename(pathname))
week_index <- readRDS(pathname_index)

## Weeks of interest
week_index <- subset(week_index, week %in% c("2019W10", "2019W11"))

header <- readLines(pathname, n = 4L)
jobs <- read_raw_sge_accounting(pathname, subset = week_index)
jobs <- anonymize(jobs)
pp <- write_raw_sge_accounting(jobs, file = "accounting.2019W10-11", header = header)
