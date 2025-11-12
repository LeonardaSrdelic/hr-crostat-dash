suppressPackageStartupMessages({
  library(eurostat); library(dplyr); library(lubridate)
  library(ggplot2); library(scales); library(readr); library(janitor)
})

use_cache <- function() {
  options(eurostat_cache_dir = "data/.eurostat_cache",
          eurostat_cache = TRUE)
}

get_gdp_pps <- function(countries = c("HR","EU27_2020")) {
  use_cache()
  get_eurostat("nama_10_pc", time_format = "date") |>
    filter(na_item == "B1GQ", unit == "PPS_EU27_2020",
           geo %in% countries) |>
    mutate(year = lubridate::year(time)) |>
    group_by(geo, year) |>
    summarise(value = dplyr::first(values), .groups = "drop")
}

get_env_taxes_share <- function(countries = c("HR","EU27_2020")) {
  use_cache()
  get_eurostat("tec00127", time_format = "date") |>
    filter(geo %in% countries) |>
    mutate(year = lubridate::year(time))
}

plot_gdp <- function(df) {
  ggplot(df, aes(year, value, group = geo, color = geo)) +
    geom_line(linewidth = 1) +
    geom_point() +
    scale_x_continuous(breaks = scales::pretty_breaks()) +
    scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ",")) +
    labs(x = "Godina", y = "PPS",
         title = "BDP po stanovniku",
         subtitle = "PPS, EU27 referenca",
         caption = "Izvor: Eurostat nama_10_pc")
}

plot_tax <- function(df) {
  ggplot(df, aes(year, values, color = geo)) +
    geom_line(linewidth = 1) +
    geom_point() +
    scale_y_continuous(labels = label_percent(scale = 1, accuracy = 0.1, decimal.mark = ",")) +
    labs(x = "Godina", y = "Udio u BDP",
         title = "Okolišni porezi",
         caption = "Izvor: Eurostat tec00127")
}
