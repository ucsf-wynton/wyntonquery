library(dplyr)

pathname <- system.file("exdata", "ldap_wynton_dates.txt", package = "wyntonquery")

signups <- users_over_time(pathname)
print(head(signups))
print(tail(signups))

## Summarize by year and month
signups <- mutate(signups, year = format(date, "%Y"), month = format(date, "%m"))

## Signups per calendar year
signups <- group_by(signups, year)
signups_per_year <- count(signups)
print(signups_per_year, n = Inf)

## Signups per calendar month
signups <- group_by(signups, year, month)
signups_per_month <- count(signups)
print(signups_per_month, n = Inf)


if (require("ggplot2", quietly = TRUE)) {
  gg <- ggplot(signups, aes(date, count)) + geom_line(size = 2)
  gg <- gg + xlab("") + ylab("Number of users")
  gg <- gg + theme(text = element_text(size = 20))
  until <- max(signups$date, na.rm = TRUE)
  filename <- sprintf("users_over_time_%s.png", until)
  ## ggsave(filename, gg, width = 8, height = 6)
  print(gg)
}



