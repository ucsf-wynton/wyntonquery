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

As of September 2019, the Wynton HPC accounting file is ~5 GiB and takes roughly 2 minutes to read in:

```r
library(wyntonquery)
pathname <- sge_accounting_file()

## Read all of the accounting file
data <- read_sge_accounting(pathname, skip=4L)
print(data)

## Get all non-failed jobs of 'sali' group that requested a single slot
data2 <- subset(data, group == "sali" & slots == 1L & failed == 0)
print(data2)
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
