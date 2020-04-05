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
# File size: 0.5 GB

## Read all of the accounting file
jobs_all <- read_sge_accounting(pathname)

jobs <- jobs_all
period <- range(jobs$start_time, na.rm=TRUE)
cat(sprintf("Number of entries: %d\n", nrow(jobs)))
cat(sprintf("Period: %s/%s\n", period[1], period[2]))
# Number of entries: 1499996
# Period: 2020-02-25 13:22:10/2020-03-25 15:58:48

## The last 30 days
jobs_recent <- subset(jobs, start_time >= period[2] - 30*24*3600)
jobs <- jobs_recent
period <- range(jobs$start_time, na.rm=TRUE)
cat(sprintf("Number of entries: %d\n", nrow(jobs)))
cat(sprintf("Period: %s/%s\n", period[1], period[2]))
# Number of entries: 1489563
# Period: 2020-02-25 13:22:10/2020-03-25 15:58:48

## Get successful and failed jobs
jobs <- jobs_recent
jobs <- list(
  success = subset(jobs, failed == 0L),
  fail    = subset(jobs, failed > 0L)
)
print(sapply(jobs, nrow))
# success    fail 
# 1335219  154344
   
## CPU time consumed
cpu <- sapply(jobs, function(j) { d <- sum(j$cpu); units(d) <- "days"; d })
print(cpu)
#  success     fail 
# 82622.05 37785.55

## CPU-time fractions
print(cpu / sum(cpu))
#   success      fail 
# 0.6861863 0.3138137
```

From this, we see that during the last 30-day period, 31% of the CPU time was consumed by jobs that failed.  Among the failed jobs, the failure code was distributed as:

```r
print(table(jobs$fail$failed))
#    14     26     37    100 
#     1      9 104229  50105
```

From `help("read_sge_accounting", package="wyntonquery")` for details on the parsed fields and their data types, these description of these codes are:

 *  14: ran, but failed before calling epilog
 *  26: failed opening stderr/stdout file
 *  37: ran but killed because it exceeded the maximum run time (`h_rt`), maximum CPU time (`h_cpu`), or maximum virtual memory (`h_vmem`)
 * 100: ran, but killed by a signal (perhaps due to exceeding resources), task died, shepherd died (e.g. node crash), etc.

The jobs that exhausted their limits, consumed 28,600 days of CPU time;

```r
> d <- sum(subset(jobs$fail, failed == 37L)$cpu); units(d) <- "days"
> d
Time difference of 28597.34 days
```

which corresponds to 24% of all CPU time;

```r
> as.numeric(d/sum(cpu))
[1] 0.2375044
```



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
