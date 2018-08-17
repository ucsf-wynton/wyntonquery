# R Package: wyntonquery - Query the UCSF Wynton Environment

An R package that provides utility functions for querying the UCSF Wynton environment, in order to gather system details used to populate the information on the [Wynton website](https://ucsf-hpc.github.io/wynton/about/specs.html).


## Usage

To gather the information, in R, run:

```r
> source(system.file(package="wyntonquery", "scripts", "system_info.R"), echo = TRUE)
```

This will identify all compute nodes on the cluster (by querying `qhost`) and launch a job on each one that gathers details on that particular node.  Depending on the amount and type of jobs currently occupying the cluster nodes, this may take anywhere from a few minutes to hours to complete.

When completed, a tab-delimited file `.data/host_table,{date}.tsv` will be produced.  To deploy this to [Wynton website](https://ucsf-hpc.github.io/wynton/about/specs.html), copy this file to [`docs/assets/data/host_table.tsv`](https://github.com/UCSF-HPC/wynton/blob/master/docs/assets/data/host_table.tsv), then commit and push.
