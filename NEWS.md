# Version (development version)

 * ...


# Version 0.3.0 [2022-06-02]

## New Features

 * Add `beegfs_quota()`.
 
 * Now `read_raw_sge_accounting()` and `ead_sge_accounting()` can read
   the subset of data that is given by an
   `sge_accounting_index_by_week` week index table.  This is one by
   passing to to argument `offset`.

 * Now `read_raw_sge_accounting()` and `read_sge_accounting()` accepts
   also file connections as input.

 * Add `sge_failed_codes()`.

## Documentation

 * Updated SGE accounting vignette.

 * Updated example data for number of users over time.
 
## Bug Fixes

 * `disk_free()` failed because some columns were not recognized.

 * Field `nbr_of_jobs` returned by `sge_make_week_index()` was a
   cumulative sum, not number of jobs per week.

 * `open_file_at(..., auto_close = TRUE)` was not safe; it could end
   up closing the incorrect connection if the intended one had been
   closed and another one had been opened.  Dropped options for
   `auto_close` to be on safe side.

# Version 0.2.0 [2021-06-18]

## New Features

 * Add `sge_make_week_index()`.

 * Add `add_weeks()`.
 

# Version 0.1.0 [2021-06-18]

## New Features

 * Add `users_over_time()`.
 
 * Add generic function `anonymize()` with implementations for
   `data.frame`, `raw_sge_accounting`, and `qstat` objects.

 * Add `read_qstat()` for getting information on the jobs currently on
   the SGE queue.

 * Add `make_file_index()`, `save_file_index()`, and
   `qread_file_index()`.

 * Add `open_file_at()`.

 * `read_raw_sge_accounting()`, and therefore also
   `read_sge_accounting()`, gained argument `offset`.

## Documentation

 * Add vignettes 'SGE Accounting File' and 'Jobs on the SGE queue'.

## Bug Fixes

 * The `ru_wallclock` values returned by `read_sge_account()` were
   corrupted by "random" NAs.


# Version 0.0.0-9005 [2021-05-05]

## New Features

 * Add `write_raw_sge_accounting()`.

 * Add `anonymize_raw_sge_accounting()`.

 * Now `read_raw_sge_accounting()` records file header in attribute
   `header`.

 * Added an example SGE `accounting` file and examples how to process
   it.


# Version 0.0.0-9004 [2021-01-25]

## New Features

 * Add `job_summary_by_status()`.
 

# Version 0.0.0-9003 [2020-04-04]

## New Features

 * Add `parse_category()` for `sge_accounting` objects.
 
## Documentation

 * Expand example of `read_sge_accounting()` to display how to inspect
   how much CPU time was spent on successful and failed jobs.

## Bug Fixes

 * Package did not pass `R CMD check --as-cran` on systems without `qhost`.
 

# Version 0.0.0-9002 [2019-08-06]

## New Features

 * Add `sge_accounting_file()` and `read_sge_accounting()`.

 * Renamed `host_table()` column `# Physical Cores` to `Physical
   Cores` so that it does not conflict with `#`-comment lines in
   tab-delimited files.
   

# Version 0.0.0-9001 [2019-06-06]

## Significant Changes

 * The `system("scripts", "system_info.R", package="wyntonquery")`
   script is now robust against errors; if there's an error querying a
   particular host, then that host is dropped from the final out.

 * The `system("scripts", "system_info.R", package="wyntonquery")`
   script now caches results from individual compute nodes.

 * `host_info()` now reports on `cpu_model`, which corresponds to the
   `cpu_info()` field `model_name`, e.g. "Intel(R) Core(TM) i7-8650U
   CPU @ 1.90GHz".  Previously, it tried to report on the CPU speed
   but that fluctuated from run to run because it was incorrectly
   inferred from the `cpu_mhz` field.

## New Features

 * `on_hostname()` gained argument `cache` for controlling whether
   cached results should be considered or not.
   
 * It's now possible to specify which hosts to query via environment
   variable `R_WYNTONQUERY_INCLUDE`.

 * `on_hostname()` gained argument `on_error` for controlling how
   errors should be handled.
 
## Bug Fixes

 * `cpu_info()` did not handle upper-case letters in field names.
 

# Version 0.0.0-9000 [2018-06-10]

## New Features

 * Add `host_table()`.
 
 * Add `round_ram()`.
 
 * Add `lscpu()`.
 
 * Add `system_info()`.
 
 * Add `disk_free()`.
 
 * Add `queues()`.

 * Add `on_hostname()`.
 
 * Add `qhost()` and `cpu_info()`.
