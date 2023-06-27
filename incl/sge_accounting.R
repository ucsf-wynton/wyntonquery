pathname <- system.file("exdata", "accounting", package = "wyntonquery")

jobs <- read_sge_accounting(pathname)
print(jobs)
## # A tibble: 1,000 x 45
##    qname    hostname group  owner   job_name job_number account priority submission_time    
##    <chr>    <chr>    <chr>  <chr>   <chr>         <int> <chr>      <int> <dttm>             
##  1 member.q cin-id3  group4 owner09 sleep.sh          1 sge            0 2017-08-15 11:59:21
##  2 long.q   cc-id2   group4 owner09 sleep.sh          2 sge           19 2017-08-15 12:00:23
##  3 long.q   cc-id2   group4 owner09 sleep.sh          2 sge           19 2017-08-15 12:00:23
## ...

## Identify successful and failed jobs
jobs_success <- subset(jobs, failed == 0)
jobs_fail <- subset(jobs, failed > 0)

## CPU time consumed
t <- c(sum(jobs_success$cpu), sum(jobs_fail$cpu))
units(t) <- "days"
print(t)
## Time differences in days
## [1] 1283.6721  328.9508

## Fraction of successful and failed CPU time
u <- as.numeric(t)
u <- u / sum(u)
names(u) <- c("success", "failed")
print(u)
##   success    failed 
## 0.7960151 0.2039849
