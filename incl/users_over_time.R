library(dplyr)

pathname <- system.file("exdata", "ldap_wynton_dates.txt", package = "wyntonquery")

signups <- users_over_time(pathname)
print(head(signups))
print(tail(signups))

## Summarize by year and month
signups <- mutate(signups, year = format(date, "%Y"))

## Signups per calendar year
signups <- mutate(signups, month = format(date, "%m"))
signups <- group_by(signups, year)
signups_per_year <- count(signups, name = "change")
signups_end_of_year <- filter(signups, date == max(date), total == max(total))
signups_per_year <- left_join(signups_per_year, signups_end_of_year)
signups_per_year <- select(signups_per_year, year, change, total, per = date)
print(signups_per_year, n = Inf)

## Signups per calendar month
signups <- group_by(signups, year, month)
signups_per_month <- count(signups, name = "change")
signups_end_of_month <- filter(signups, date == max(date), total == max(total))
signups_per_month <- left_join(signups_per_month, signups_end_of_month)
signups_per_month <- select(signups_per_month, year, month, change, total, per = date)
print(signups_per_month, n = Inf)


if (require("ggplot2", quietly = TRUE)) {
  gg <- ggplot(signups, aes(date, total)) + geom_line(linewidth = 2.0)
  gg <- gg + xlab("") + ylab("Number of users")
  gg <- gg + theme(text = element_text(size = 20))
  until <- max(signups$date, na.rm = TRUE)
  filename <- sprintf("users_over_time_%s.png", until)
  ggsave(filename, gg, width = 8.0, height = 6.0)
  print(gg)
}



