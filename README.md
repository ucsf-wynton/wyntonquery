![R-CMD-check](https://github.com/ucsf-wynton/wyntonquery/workflows/R-CMD-check/badge.svg)


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

The SGE accounting file holds information on all submitted and _finished_ jobs submitted to the SGE scheduler.  Queued or currently running jobs are _not_ included in this file.  A job is considered "finished" if it completed successfully, terminated due to an error, or was cancelled.  The file is a colon-delimited text file with one job entry per row, with the most recently finished job appended at end.  Contrary to what one might expect, the file is not _perfectly_ ordered by the `end_time` of the jobs.  We might find some entries where the `end_time` of two consequentive entries might differ by a few seconds in the "wrong" order.  It's not clear to me why this is but it might be that the scheduler updates the SGE accounting file at regular intervals, say every few minutes, and when it does it goes through the jobs in order of job index.

As of June 2021, the Wynton HPC accounting file is ~54 GB with 160.5 million job entries (using `wc -l`).  In March 2020, it was ~12 GB and took 6-8 minutes to read in full.

```r
library(wyntonquery)

pathname <- sge_accounting_file()
cat(sprintf("File size: %.3g GB\n", file.size(pathname)/1024^3))
# File size: 53.9 GB

## Read the last 1,000,000 job entries of the accounting file,
## which takes 15-30 seconds to read
njobs <- 1e6
jobs_all <- read_sge_accounting(pipe(sprintf("tail -n %d %s", njobs+4, shQuote(pathname))))

## Sort by end time (it's only approximately so on file)
jobs_all <- jobs_all[order(jobs_all$end_time), ]

## Statistics of finished jobs
jobs <- jobs_all
period <- range(jobs$end_time, na.rm=TRUE)
cat(sprintf("Period: %s/%s\n", period[1], period[2]))
cat(sprintf("Number of jobs finished during this period: %d\n", nrow(jobs)))
# Period: 2021-06-13 18:15:30/2021-06-16 19:35:15
# Number of jobs finished during this period: 1000000

## Jobs ended during the last 24 hours
jobs <- subset(jobs_all, end_time >= period[2] - 24*3600)
period <- range(jobs$end_time, na.rm=TRUE)
cat(sprintf("Period: %s/%s\n", period[1], period[2]))
cat(sprintf("Number of jobs finished during this period: %d\n", nrow(jobs)))
Period: 2021-06-15 19:35:15/2021-06-16 19:35:15
Number of jobs finished during this period: 216375

## Get successful and failed jobs
jobs <- jobs_all
jobs <- list(
  success = subset(jobs, failed == 0L),
  fail    = subset(jobs, failed > 0L)
)
print(sapply(jobs, nrow))
# success    fail 
#  979905   20095

## CPU time consumed
cpu <- sapply(jobs, function(j) { d <- sum(j$cpu); units(d) <- "days"; d })
print(cpu)
#   success      fail 
# 17742.775  1390.293

## CPU-time fractions
print(cpu / sum(cpu))
#    success       fail 
# 0.92733561 0.07266439
```

From this, we see that during the last 30-day period, 31% of the CPU time was consumed by jobs that failed.  Among the failed jobs, the failure code was distributed as:

```r
print(table(jobs$fail$failed))
#     1     8    21    25    26    27    28    37   100 
#    48     1     1  6873     6     1    11  1639 11515 
```

From `help("read_sge_accounting", package="wyntonquery")` for details on the parsed fields and their data types, these description of some of these codes are:

 *  26: failed opening stderr/stdout file
 *  37: ran but killed because it exceeded the maximum run time (`h_rt`), maximum CPU time (`h_cpu`), or maximum virtual memory (`h_vmem`)
 * 100: ran, but killed by a signal (perhaps due to exceeding resources), task died, shepherd died (e.g. node crash), etc.

The jobs that exhausted their limits, consumed 369 days of CPU time;

```r
> d <- sum(subset(jobs$fail, failed == 37L)$cpu); units(d) <- "days"
> d
Time difference of 368.6784 days
```

which corresponds to ~2% of all CPU time;

```r
> as.numeric(d/sum(cpu))
[1] 0.01926917
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
