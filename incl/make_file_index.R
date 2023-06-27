## An SGE accounting file
pathname <- system.file("exdata", "accounting", package = "wyntonquery")

## The corresponding SGE accounting index file
pathname_index <- sprintf("%s.index", pathname)

## Scan SGE accounting file to identify job offset positions
index <- make_file_index(pathname)
cat(sprintf("Number of jobs: %d\n", length(index)))
str(index)

## Save index to file
tf <- tempfile(fileext = ".index")
save_file_index(index, file = tf)
cat(sprintf("Saved index file: %s (%d bytes)\n", pathname, file.size(tf)))

## Read index from file
index <- read_file_index(tf)
cat(sprintf("Number of jobs: %d\n", length(index)))
str(index)

## Read jobs 301 to 350
jobs <- read_sge_accounting(pathname, offset = index[301], n_max = 50L)
print(jobs)

## Read all jobs *after* the 500:th job
jobs <- read_sge_accounting(pathname, offset = index[501])
print(jobs)

## Cleanup
file.remove(tf)
