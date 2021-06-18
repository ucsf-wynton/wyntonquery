pathname <- system.file("exdata", "accounting", package = "wyntonquery")
index <- read_file_index(sprintf("%s.index", pathname))
cat(sprintf("Number of jobs: %d\n", length(index)))

## Get all jobs *after* the 500:th job
jobs <- read_sge_accounting(pathname, offset = index[501])
print(jobs)

## Get jobs 301 to 350
jobs <- read_sge_accounting(pathname, offset = index[301], n_max = 50)
print(jobs)

