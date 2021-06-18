library(wyntonquery)

pathname <- system.file("exdata", "accounting", package = "wyntonquery")

bfr <- readLines(pathname)
nrows <- length(bfr)
cat(sprintf("Number of nrows: %d\n", nrows))

index <- make_file_index(pathname)
nindex <- length(index)
cat(sprintf("Number of row offsets: %d\n", nindex))
stopifnot(nindex == nrows)

index_jobs <- make_file_index(pathname, skip = 4L)
njobs <- length(index_jobs)
cat(sprintf("Number of row offsets for job entries: %d\n", njobs))
stopifnot(njobs == nindex - 4)

index_jobs_2 <- index[-seq_len(4)]
stopifnot(identical(index_jobs_2, index_jobs))

tf <- tempfile()
save_file_index(index_jobs, file = tf)

index_jobs_3 <- read_file_index(file = tf)
stopifnot(all.equal(index_jobs_3, index_jobs))

file.remove(tf)

