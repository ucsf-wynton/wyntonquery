pathname <- system.file("exdata", "accounting", package = "wyntonquery")
index <- read_file_index(sprintf("%s.index", pathname))
cat(sprintf("Number of jobs: %d\n", length(index)))

con <- open_file_at(pathname, position = index[1001])

## Get all jobs after the 1000:th job
jobs <- read_sge_accounting(con)
close(con)
print(jobs)
