

<div id="badges"><!-- pkgdown markup -->
 <a href="https://github.com/ucsf-wynton/wyntonquery/actions?query=workflow%3AR-CMD-check"><img border="0" src="https://github.com/ucsf-wynton/wyntonquery/actions/workflows/R-CMD-check.yaml/badge.svg?branch=develop" alt="R CMD check status"/></a>     <a href="https://app.codecov.io/gh/ucsf-wynton/wyntonquery"><img border="0" src="https://codecov.io/gh/ucsf-wynton/wyntonquery/branch/develop/graph/badge.svg" alt="Coverage Status"/></a> 
</div>

# wyntonquery: Query the UCSF Wynton Environment 

An R package that provides utility functions for querying the UCSF Wynton environment, in order to gather system details used to populate the information on the [Wynton website](https://wynton.ucsf.edu/hpc/about/specs.html).


## Usages

### Query compute node information via job submissions

To gather the information, in R, run:

```r
> source(system.file(package="wyntonquery", "scripts", "system_info.R"), echo = TRUE)
```

This will identify all compute nodes on the cluster (by querying `qhost`) and launch a job on each one that gathers details on that particular node.  Depending on the amount and type of jobs currently occupying the cluster nodes, this may take anywhere from a few minutes to hours to complete.

When completed, a tab-delimited file `.data/host_table,{date}.tsv` will be produced.  To deploy this to [Wynton website](https://wynton.ucsf.edu/hpc/about/specs.html), copy this file to [`docs/assets/data/host_table.tsv`](https://github.com/ucsf-wynton/wynton-website-hpc/blob/master/docs/hpc/assets/data/host_table.tsv), then commit and push.


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

## Installation
R package wyntonquery is only available via [GitHub](https://github.com/ucsf-wynton/wyntonquery) and can be installed in R as:
```r
remotes::install_github("ucsf-wynton/wyntonquery", ref="master")
```


### Pre-release version

To install the pre-release version that is available in Git branch `develop` on GitHub, use:
```r
remotes::install_github("ucsf-wynton/wyntonquery", ref="develop")
```
This will install the package from source.  

<!-- pkgdown-drop-below -->

