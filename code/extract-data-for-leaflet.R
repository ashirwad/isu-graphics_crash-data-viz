## Getting setup

# Load packages
library(conflicted)
library(here)
library(tidyverse)
conflict_prefer("filter", "dplyr")
library(rio)
library(lubridate)
library(USAboundaries)
library(sf)

# Get Iowa state, county, and congressional district boundaries
state_boundary <- st_geometry(us_states(resolution = "high", states = "IA"))
county_boundaries <- us_counties(resolution = "high", states = "IA") %>%
  select(county_name = name)
cong_district_boundaries <- us_congressional(
  resolution = "high", states = "IA"
) %>%
  select(district_id = cd115fp)

# Import & process data
fatal_crashes_2019 <- import(here("data", "ia-fatal-crashes-2015-19.rds")) %>%
  filter(crash_year == 2019) %>%
  mutate(
    month_num = month(crash_date),
    season = if_else(month_num %in% c(5:10), "summer", "winter"),
    crash_month = format_ISO8601(crash_date, precision = "ym"),
    day_type = if_else(is_weekend == FALSE, "weekday", "weekend")
  ) %>%
  st_join(county_boundaries, st_within)

county_vmt_2019 <- import(
  # Data is obtained from here: https://iowadot.gov/maps/msp/vmt/countyvmt19.pdf
  here("data", "ia-county-vmt-2019.xlsx"),
  setclass = "tibble"
) %>%
  rename(county_name = `County Name`, total_vmt = Total) %>%
  mutate(county_name = str_to_title(county_name))


## Computation for each county

# Find centroids of each county
county_centroids <- st_centroid(county_boundaries) %>%
  nest(data = geometry) %>%
  mutate(coords = map(data, ~ as_tibble(st_coordinates(.x)))) %>%
  unnest(coords) %>%
  select(county_name, lng = X, lat = Y)

# Compute fatal crash counts
fatal_crash_counts_by_county_2019 <- list(
  # overall counts
  overall = st_drop_geometry(fatal_crashes_2019) %>%
    count(county_name, name = "n_crashes") %>%
    as_tibble(),

  # crash rate by vehicle miles traveled
  rate_by_vmt = st_drop_geometry(fatal_crashes_2019) %>%
    count(county_name, name = "n_crashes") %>%
    as_tibble() %>%
    left_join(county_vmt_2019, by = "county_name") %>%
    mutate(crash_rate = (n_crashes * 10^6) / total_vmt),

  # broken by season
  by_season = st_drop_geometry(fatal_crashes_2019) %>%
    count(county_name, season, name = "n_crashes") %>%
    pivot_wider(names_from = season, values_from = n_crashes),

  # broken by month
  by_month = st_drop_geometry(fatal_crashes_2019) %>%
    count(county_name, crash_month, name = "n_crashes") %>%
    as_tibble(),

  # broken by month & day type
  by_month_daytype = st_drop_geometry(fatal_crashes_2019) %>%
    count(county_name, crash_month, day_type, name = "n_crashes") %>%
    pivot_wider(names_from = day_type, values_from = n_crashes)
)


## Computation for each congressional district

# Find centroids of each congressional district
cong_district_centroids <- st_centroid(cong_district_boundaries) %>%
  nest(data = geometry) %>%
  mutate(coords = map(data, ~ as_tibble(st_coordinates(.x)))) %>%
  unnest(coords) %>%
  select(district_id, lng = X, lat = Y)

# Compute fatal crash counts
fatal_crash_counts_by_district_2019 <- fatal_crashes_2019 %>%
  mutate(quarter_num = quarter(crash_date)) %>%
  st_join(cong_district_boundaries, st_within) %>%
  count(district_id, quarter_num, name = "n_crashes") %>%
  st_drop_geometry()


## Create data for leaflet maps

# Data for choropleth map
choropleth_data <- fatal_crash_counts_by_county_2019 %>%
  pluck("rate_by_vmt") %>%
  right_join(county_vmt_2019, by = c("county_name", "total_vmt")) %>%
  replace_na(list(n_crashes = 0, crash_rate = 0))

export(choropleth_data, here("data", "leaflet-choropleth-data.rds"))

# Data for mini circles chart
mini_circles_data <- county_centroids %>%
  left_join(fatal_crash_counts_by_county_2019$overall, by = "county_name") %>%
  left_join(fatal_crash_counts_by_county_2019$by_season, by = "county_name") %>%
  replace_na(list(n_crashes = 0, summer = 0, winter = 0))

export(mini_circles_data, here("data", "leaflet-mini-circles-data.rds"))

# Data for mini bars chart
mini_bars_data <- cong_district_centroids %>%
  left_join(fatal_crash_counts_by_district_2019, by = "district_id") %>%
  mutate(quarter_name = str_c("Qtr", quarter_num, sep = "_")) %>%
  pivot_wider(names_from = quarter_name, values_from = n_crashes) %>%
  replace_na(list(Qtr_1 = 0, Qtr_2 = 0, Qtr_3 = 0, Qtr_4 = 0))

export(mini_bars_data, here("data", "leaflet-mini-bars-data.rds"))


## Create data for leaflet animated minicharts

# Create a data frame of 99 counties times 12 months
helper_data <- tibble(
  county_name = rep(unique(county_boundaries$county_name), each = 12),
  crash_month = rep(
    str_c(2019, str_pad(1:12, 2, pad = 0), sep = "-"), times = 99
  )
)

# Create data for generating mini circle charts animation
mini_circles_anim_data <- county_centroids %>%
  left_join(
    pluck(fatal_crash_counts_by_county_2019, "by_month"), by = "county_name"
  ) %>%
  right_join(helper_data, by = c("county_name", "crash_month")) %>%
  arrange(county_name, crash_month) %>%
  group_by(county_name) %>%
  fill(lng, lat, .direction = "updown") %>%
  ungroup() %>%
  replace_na(list(n_crashes = 0))

export(
  mini_circles_anim_data, here("data", "leaflet-mini-circles-anim-data.rds")
)

# Create data for generating mini pie charts animation
mini_pies_anim_data <- county_centroids %>%
  left_join(
    pluck(fatal_crash_counts_by_county_2019, "by_month"), by = "county_name"
  ) %>%
  left_join(
    pluck(fatal_crash_counts_by_county_2019, "by_month_daytype"),
    by = c("county_name", "crash_month")
  ) %>%
  right_join(helper_data, by = c("county_name", "crash_month")) %>%
  arrange(county_name, crash_month) %>%
  group_by(county_name) %>%
  fill(lng, lat, .direction = "updown") %>%
  ungroup() %>%
  replace_na(list(n_crashes = 0, weekday = 0, weekend = 0))

export(mini_pies_anim_data, here("data", "leaflet-mini-pies-anim-data.rds"))
