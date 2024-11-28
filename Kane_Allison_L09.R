# L09 - dates and time ----
# Stat 301-1

## load packages ----

library(tidyverse)
library(lubridate)
library(nycflights13)
library(skimr)

# data
data("flights")

tinder_data <- read_csv("data/tinder_data.csv")

# function to make datetime objects
make_datetime_100 <- function(year, month, day, time) {
  make_datetime(year, month, day, time %/% 100, time %% 100)
}


# change dep_time, arr_time, sched_dep_time and sched_arr_time to datetimes
flights_dt <- flights |> 
  # remove cancelled flights
  filter(!is.na(dep_time), !is.na(arr_time)) |> 
  # change to datetimes
  mutate(
    dep_time = make_datetime_100(year, month, day, dep_time),
    arr_time = make_datetime_100(year, month, day, arr_time),
    sched_dep_time = make_datetime_100(year, month, day, sched_dep_time),
    sched_arr_time = make_datetime_100(year, month, day, sched_arr_time)
  ) 


## Exercises ----

### Exercise 1 ----

# At the top of the document in the yaml header, define the `date` to 
# be the current date using `today`.

# see qmd

### Exercise 2----

# What happens if you try to parse a string that contains invalid dates, like this one?

# invalid string example
ymd(c("2010-10-10", "bananas"))

# Since bananas is not a valid date tring, this will return an error.

### Exercise 3----

# Use the appropriate `lubridate` function to parse each of the following dates:

# dates to parse
d1 <- "January 1, 2010"
mdy(d1)

d2 <- "2015-Mar-07"
ymd(d2)

d3 <- "06-Jun-2017"
dmy(d3)

d4 <- c("August 19 (2015)", "July 1 (2015)")
mdy(d4)

d5 <- "12/30/14" # Dec 30, 2014
mdy(d5)

### Exericse 4----

# Compare `dep_time`, `sched_dep_time` and `dep_delay`. Are they consistent? Explain your findings.

# dep_time - sched_dep_time = dep_delay

check_delay_calc <- flights_dt |> 
  mutate(
    computed_delay = dep_time - sched_dep_time,
    dep_delay = dminutes(dep_delay),
    consistent = if_else(computed_delay == dep_delay, TRUE, FALSE),
    .keep = "used"
  )

check_delay_calc <-check_delay_calc |> 
  filter(consistent == FALSE) |> 
  mutate(
    adj_dep_time = dep_time + ddays(1),
    computed_delay = as.duration(adj_dep_time - sched_dep_time),
    dep_delay = dep_delay,
    consistent = if_else(computed_delay == dep_delay, TRUE, FALSE)
  )

check_delay_calc |> 
  count(consistent) |> 
  mutate(prop = n / sum(n))

### Exercise 5----

# How does the average delay time change over the course of a day? 
# Should you use `dep_time` or `sched_dep_time` to assess this? Explain your choice.

dep_delay_by_hour <- flights_dt |> 
  filter(dep_delay > 0) |> 
  summarize(
    avg_dep_delay = mean(dep_delay),
    med_dep_delay = median(dep_delay),
    cv = sd(dep_delay) / mean(dep_delay),
    num_flights = n(),
    .by = hour
  )

ggplot(dep_delay_by_hour, aes(hour, avg_dep_delay)) +
  geom_line() 
  
ggplot(dep_delay_by_hour, aes(hour, cv)) +
  geom_line() 

### Exercise 6----

# On what day of the week should you leave if you want to minimize the chance of a delay?

flights_dt |> 
  mutate(
    day_of_week = wday(sched_dep_time, label = TRUE, abbr = FALSE),
    .before = 1
  ) |> 
  summarize(
    avg_dep_delay = mean(dep_delay),
    med_dep_delay = median(dep_delay),
    .by = day_of_week
  ) |> 
  arrange(avg_dep_delay)

### Exercise 7----

# Create a tibble with 3 variables containing the following:
  
# 1. every month in a year (i.e., `"January"`),

# 2. the date of the first day of every month (i.e., `"2022-01-01"`), and 
first_days <- seq(ymd("2022-01-01"), by = "1 month", length.out = 12)

# 3. the day of the week that each first day falls on (i.e., `"Tuesday"`). 
weekdays <- weekdays(first_days)

# tibble
tibble(
  month = c("January", "February", "March", "April", "May", "June", "July",
            "August", "September", "October", "November", "December"),
  first = first_days,
  day_of_week = weekdays
  )

### Case Study----

tinder_data <- tinder_data |> 
  mutate(
    age_current = floor((user_birth_date %--% today()) / years(1)),
    age_joined = floor((user_birth_date %--% user_create_date) / years(1)),
    .keep = "used"
  )

# 1. Describe the distribution of `age_current` on Tinder (calculated from the current date to their birth date). We typically express age in years, round down to the nearest whole number.

skim_without_charts(tinder_data$age_current)

ggplot(tinder_data, aes(x = age_current)) +
  geom_histogram()

ggplot(tinder_data, aes(x = age_current)) +
  geom_boxplot()

# 2. Describe the distribution of `age_joined` on Tinder (calculated from `user_create_date` and `user_birth_date`). We typically express age in years, round down to the nearest whole number.

skim_without_charts(tinder_data$age_joined)

ggplot(tinder_data, aes(x = age_joined)) +
  geom_histogram()

ggplot(tinder_data, aes(x = age_joined)) +
  geom_boxplot()

# 3. Which distribution do you believe is a more accurate representation of user age and why?

# `age_current` is a more accurate representation of user age, as it describes the user's age 
# relative to their date of birth and the current date, rather than their date of birth and 
# the date they created their tinder account 