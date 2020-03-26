# R Package: wyntonquery - Query the UCSF Wynton Environment

An R package that provides utility functions for querying the UCSF Wynton environment, in order to gather system details used to populate the information on the [Wynton website](https://ucsf-hpc.github.io/wynton/about/specs.html).


## Install

This R package is only available on GitHub.  To install it, run the following in R:

```r
if (!requireNamespace("remotes")) install.packages("remotes")
remotes::install_github("UCSF-HPC/wyntonquery")
```




## Usages

### Read the SGE accounting file

As of March 2020, the Wynton HPC accounting file is ~12 GB and takes 6-8 minutes to read in:

```r
library(wyntonquery)
pathname <- sge_accounting_file()
cat(sprintf("File size: %.3g GB\n", file.size(pathname)/1024^3))

## Read all of the accounting file
jobs_all <- read_sge_accounting(pathname)

jobs <- jobs_all
period <- range(jobs$start_time, na.rm=TRUE)
cat(sprintf("Number of entries: %d\n", nrow(jobs)))
cat(sprintf("Period: %s/%s\n", period[1], period[2]))
## Number of entries: 9996
## Period: 2017-08-15 11:59:36/2017-10-26 16:49:25

## The last 30 days
jobs_recent <- subset(jobs, start_time >= period[2] - 30*24*3600)
jobs <- jobs_recent
period <- range(jobs$start_time, na.rm=TRUE)
cat(sprintf("Number of entries: %d\n", nrow(jobs)))
cat(sprintf("Period: %s/%s\n", period[1], period[2]))
## Number of entries: 9428
## Period: 2017-09-27 08:37:47/2017-10-26 16:49:25

## Get successful and failed jobs
jobs <- jobs_recent
jobs <- list(
  success = subset(jobs, failed == 0L),
  fail    = subset(jobs, failed > 0L)
)
print(sapply(jobs, nrow))
## success    fail 
##    9403      25
   
## CPU time consumed
cpu <- sapply(jobs, function(j) { d <- sum(j$cpu); units(d) <- "days"; d })
print(cpu)
##  success     fail 
## 136.1654 305.1815

## CPU-time fractions
print(cpu / sum(cpu))
##   success      fail
## 0.3085224 0.6914776
```

See `help("read_sge_accounting", package="wyntonquery")` for details on the parsed fields and their data types.


### Query compute node information via job submissions

To gather the information, in R, run:

```r
> source(system.file(package="wyntonquery", "scripts", "system_info.R"), echo = TRUE)
```

This will identify all compute nodes on the cluster (by querying `qhost`) and launch a job on each one that gathers details on that particular node.  Depending on the amount and type of jobs currently occupying the cluster nodes, this may take anywhere from a few minutes to hours to complete.

When completed, a tab-delimited file `.data/host_table,{date}.tsv` will be produced.  To deploy this to [Wynton website](https://ucsf-hpc.github.io/wynton/about/specs.html), copy this file to [`docs/assets/data/host_table.tsv`](https://github.com/UCSF-HPC/wynton/blob/master/docs/assets/data/host_table.tsv), then commit and push.


## Internal notes

### How to Rebuild Wynton HPC Specification Database

1. Query recently added nodes

```sh
cd .data
export R_WYNTONQUERY_EXCLUDE="$(cut -f 1 host_table.tsv | tail -n +4) qb3-hmid1"
qsub -v R_WYNTONQUERY_EXCLUDE="$R_WYNTONQUERY_EXCLUDE" ../inst/scripts/system_info.sge
```

2. Update current database
```sh
Rscript merge_all.R
```

3. Deploy
```sh
cp host_table.tsv ../../wynton/docs/assets/data/
```
