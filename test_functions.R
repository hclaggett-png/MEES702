rm(list = ls())

library(ggplot2)

# Load the function
source("/Users/hannahclaggett/Desktop/MEES 702/MEES702/is_leap_year.R")

c(1:50) * 2
summary(iris)

# Tests
test_years <- c(1900, 2000, 2004, 2023, 2024, 2100)
is_leap_year(test_years) # press "ESCAPE" when stuck in + signs

is_leap_year(20)
is_leap_year(20.1)
is_leap_year(1900)
is_leap_year(2000)
is_leap_year(2004)
is_leap_year(2023)
is_leap_year("two thousand")

test_years <- c(1900, 2000, 2004, 2023, 2024, 2100)
is_leap_year(test_years) # press "ESCAPE" when stuck in + signs