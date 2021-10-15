library(wyntonquery)

pathname <- system.file("exdata", "accounting", package = "wyntonquery")

bfr <- readLines(pathname)
nrows <- length(bfr)
cat(sprintf("Number of nrows: %d\n", nrows))

index_raw <- make_file_index(pathname)
nindex <- length(index_raw)
cat(sprintf("Number of row offsets: %d\n", nindex))
stopifnot(nindex == nrows)

index <- make_file_index(pathname, skip = 4L)
njobs <- length(index)
cat(sprintf("Number of row offsets for job entries: %d\n", njobs))
stopifnot(njobs == nindex - 4)

index_2 <- index_raw[-seq_len(4)]
stopifnot(identical(index_2, index))

tf <- tempfile()
save_file_index(index, file = tf)

index_3 <- read_file_index(file = tf)
stopifnot(all.equal(index_3, index))

file.remove(tf)

## Create a week index
week_index <- sge_make_week_index(pathname, index = index)
print(week_index)
