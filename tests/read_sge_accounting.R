library(wyntonquery)

pathname <- system.file("exdata", "accounting", package = "wyntonquery")
index <- read_file_index(sprintf("%s.index", pathname))
njobs <- length(index)
cat(sprintf("Number of jobs: %d\n", njobs))

## Read all jobs
jobs <- read_sge_accounting(pathname, lazy = TRUE)
print(jobs)
stopifnot(
  nrow(jobs) == njobs
)

## Get all jobs after the 500:th job
jobs_2 <- read_sge_accounting(pathname, offset = index[501], lazy = TRUE)
print(jobs_2)
stopifnot(
  all.equal(jobs_2, jobs[501:nrow(jobs), ], check.attributes = FALSE)
)

## Get jobs 301 to 350
jobs_3 <- read_sge_accounting(pathname, offset = index[301], n_max = 50, lazy = TRUE)
print(jobs_3)
stopifnot(
  nrow(jobs_3) == 50L,
  all.equal(jobs_3, jobs[301:350, ], check.attributes = FALSE)
)

## The same but read from a file connection
con <- file(pathname, open = "rb")
jobs_3b <- read_sge_accounting(con, offset = index[301], n_max = 50, lazy = TRUE)
close(con)
print(jobs_3b)
stopifnot(
  all.equal(jobs_3b, jobs_3)
)

