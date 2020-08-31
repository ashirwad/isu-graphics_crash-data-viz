# Setup chunk
library(conflicted)
library(here)
library(tidyverse)
conflict_prefer("filter", "dplyr")
library(rio)
library(lubridate)
library(sf)

# Import Iowa crash data
crashes <- read_sf(
  "https://opendata.arcgis.com/datasets/104c99898c3c45d6b455bdf7fb594cd2_0.geojson"
)

# Extract five years (2015-19) worth of fatal crash data
fatal_crashes <- crashes %>%
  mutate(
    five_year_interval = interval(
      ymd_hms("2015-01-01 00:00:01"), ymd_hms("2019-12-31 23:59:59")
    )
  ) %>%
  filter(
    CRASH_DATE %within% five_year_interval,
    CSEV == 1 # crash severity 1 => fatal crashes
  ) %>%
  mutate(
    crash_date = ymd_hms(paste(date(CRASH_DATE), TIMESTR)),
    crash_year = year(crash_date),
    week_day = wday(crash_date, label = TRUE),
    is_weekend = week_day %in% c("Sat", "Sun")
  ) %>%
  select(
    crash_key = CRASH_KEY, crash_date, crash_year, week_day, is_weekend,
    geometry
  )

# Export fatal crash data
export(fatal_crashes, here("data", "ia-fatal-crashes-2015-19.rds"))
