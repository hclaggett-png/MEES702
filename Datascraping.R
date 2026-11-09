# Data Scraping
install.packages(c(
    # Core Data & Web Tools
    "tidyverse",        # dplyr, ggplot2, tibble, readr, purrr
    "rvest",            # Web scraping
    "httr2",            # REST API queries

    # NetCDF & Distance Processing
    "ncdf4",            # Low-level NetCDF array slicing
    "geosphere",        # Geodesic distance calculations (distGeo)

    # Spatial Mapping & Cartography
    "sf",               # Simple Features spatial vector handling
    "ggspatial",        # Scale bar (km) and north arrow
    "rnaturalearth",    # Public domain country/state vector boundaries
    "rnaturalearthdata" # Supporting dataset for rnaturalearth
))

library(rvest)
library(purrr) # for the keep() and pluck() functions
library(dplyr)

url <- "https://en.wikipedia.org/wiki/Washington,_D.C."
page <- read_html(url)

# Extract all html tables and find the population table
pop_table <- page |>
    html_elements("table") |>
    html_table() |>
    keep(~ any(grepl("Census", names(.x)))) |>
    pluck(1)

pop = pop_table |>
    # Rename variables
    rename(Year = Census,
           Population = Pop.,
           Change_pct = `%±`) |>
    # Remove last row and 3rd column
    slice(-n()) |>
    select(-3) |>
    # Replace commas with nothing or empty space
    mutate(Population = gsub(",", "", Population),
           Change_pct = gsub("%", "", Change_pct)) |>
    mutate(Change_pct = gsub("", "−", Change_pct)) |>
    mutate(Year = as.numeric(substr(Year, 1, 4)),
              Population = as.numeric(Population),
              Change_pct = as.numeric(Change_pct))
summary(pop)

write.csv(pop, "dataderived/popDC.csv", row.names = FALSE)
