library(wyntonquery)
library(dplyr)
library(future.apply)
library(progressr)

plan(multicore, workers = 8)

handlers(global = TRUE)
handlers(handler_progress(
  ":spin :current/:total (:message) [:bar] :percent in :elapsed ETA: :eta"
))

read_sge_accounting_by_weeks <- function(pathname, weeks, index = NULL, ...) {
  if (is.null(index)) {
    filename <- basename(pathname)
    filename_index <- sprintf("%s.index_by_week.rds", filename)
    index <- readRDS(filename_index)
  }

  index <- subset(index, week %in% weeks)
  stopifnot(nrow(index) > 0)
  
  offset <- index$file_offset[1]
  n_max <- sum(index$nbr_of_jobs)
  read_sge_accounting(pathname, offset = offset, n_max = n_max, ...)
}

pathname <- sge_accounting_file()
week_index <- readRDS("accounting.index_by_week.rds")

weeks <- sprintf("2021W%02d", 1:40)
message("Weeks: ", paste(weeks, collapse = ", "))
dts <- local({
  p <- progressor(along = weeks)
  future_lapply(weeks, FUN = function(week) {
    p(week, amount = 0)
    message("Reading week ", week)
    dt <- tryCatch({
      jobs <- read_sge_accounting_by_weeks(pathname, weeks = week, index = week_index)
      jobs <- jobs[, c("cpu", "slots")]
      ## Drop "invalid" entries
      jobs <- subset(jobs, slots > 0)
      jobs
    }, error = function(ex) ex)
    p()
    dt
  }, future.chunk.size = 1L)
})
names(dts) <- weeks

dts <- lapply(dts, FUN = mutate, cpu_per_slot = cpu/slots)

counts <- sapply(dts, FUN = nrow)
field <- "cpu_per_slot"
means <- sapply(dts, FUN = function(dt) mean(dt[[field]], na.rm = TRUE)/3600)
medians <- sapply(dts, FUN = function(dt) median(dt[[field]], na.rm = TRUE)/3600)
stats <- data.frame(week = names(means), nbr_of_jobs = counts, mean_hours = means, median_hours = medians)
rownames(stats) <- NULL
print(stats)
saveRDS(stats, file = "average_cpu_time_per_job_by_weeks.rds")

avg <- with(stats, weighted.mean(mean_hours, w = nbr_of_jobs))
message(sprintf("Average CPU time per slot and job: %.02f hours", avg))

