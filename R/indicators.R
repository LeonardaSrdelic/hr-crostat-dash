# =========================================================
# HR Crostat Dash — BDP grafovi s izvorom (bez naslova)
# =========================================================

suppressPackageStartupMessages({
  library(eurostat); library(dplyr); library(tidyr); library(readr)
  library(ggplot2);  library(scales); library(purrr); library(janitor)
  library(stringr)
})

# cache za eurostat
use_cache <- function() {
  options(
    eurostat_cache_dir = "data/.eurostat_cache",
    eurostat_cache = TRUE
  )
}

# konstante
dataset_id     <- "nama_10_gdp"
hr_code        <- "HR"
first_year     <- 2000
end_label_year <- 2025

eu27_codes   <- c("AT","BE","BG","HR","CY","CZ","DK","EE","FI","FR","DE","GR","HU","IE","IT","LV","LT","LU","MT","NL","PL","PT","RO","SK","SI","ES","SE")
ea20_members <- c("AT","BE","CY","DE","EE","ES","FI","FR","GR","IE","IT","LV","LT","LU","MT","NL","PT","SI","SK","HR")
cee_all      <- c("BG","CZ","EE","HU","LT","LV","PL","RO","SI","SK","HR")

groups <- list(
  EU27_exHR = setdiff(eu27_codes, hr_code),
  EA_exHR   = setdiff(ea20_members, hr_code),
  CEE_exHR  = setdiff(cee_all, hr_code)
)

label_map <- c(
  "Croatia"   = "Hrvatska",
  "EU27_exHR" = "EU 27 isklj. HR",
  "EA_exHR"   = "EA isklj. HR",
  "CEE_exHR"  = "CEE isklj. HR"
)

cols_named <- c(
  "Hrvatska"        = "#C00000",
  "EU 27 isklj. HR" = "#303030",
  "EA isklj. HR"    = "#E69F00",
  "CEE isklj. HR"   = "#009E73"
)

default_caption <- function(meta) {
  paste0("Izvor: Eurostat ", dataset_id, 
         ", jedinica ", meta$real_unit,
         " · © Leonarda Srdelić")
}


# pronalazak najnovije CLVxx_MEUR jedinice
latest_clv_unit <- function() {
  use_cache()
  candidate_units <- eurostat::get_eurostat(
    id = dataset_id,
    filters = list(na_item = "B1GQ", geo = c("EU27_2020","EA20","DE")),
    time_format = "num"
  ) |>
    distinct(unit) |>
    filter(str_detect(unit, "^CLV\\d{2}_MEUR$")) |>
    mutate(ref_year = readr::parse_number(unit)) |>
    arrange(desc(ref_year)) |>
    pull(unit) |>
    as.character()

  if (length(candidate_units) == 0) stop("Nije pronađena CLVxx_MEUR jedinica.")
  candidate_units[[1]]
}

# dohvat serija
fetch_gdp <- function(unit, geos) {
  use_cache()
  eurostat::get_eurostat(
    id = dataset_id,
    filters = list(na_item = "B1GQ", unit = unit, geo = geos),
    time_format = "num"
  ) |>
    rename(year = time, value = values)
}

# prosjek skupine po godinama
make_group_mean <- function(df, group_codes, group_name) {
  df |>
    filter(geo %in% group_codes) |>
    group_by(year) |>
    summarise(value = mean(value, na.rm = TRUE), .groups = "drop") |>
    mutate(group = group_name)
}

# indeks i yoy
index_and_yoy <- function(df, base_year) {
  df |>
    arrange(year) |>
    mutate(
      base_val   = value[year == base_year][1],
      index_2000 = 100 * value / base_val,
      yoy_pct    = 100 * (value / lag(value) - 1)
    )
}

# priprema podataka i meta
prepare_gdp_data <- function() {
  real_unit <- latest_clv_unit()

  gdp_nominal <- fetch_gdp("CP_MEUR", eu27_codes) |>
    filter(year >= first_year)

  gdp_real <- fetch_gdp(real_unit, eu27_codes) |>
    filter(year >= first_year)

  last_year <- min(max(gdp_nominal$year, na.rm = TRUE),
                   max(gdp_real$year, na.rm = TRUE))

  gdp_nominal <- gdp_nominal |> filter(year <= last_year)
  gdp_real    <- gdp_real    |> filter(year <= last_year)

  return(list(
    gdp_nominal = gdp_nominal,
    gdp_real    = gdp_real,
    last_year   = last_year,
    real_unit   = real_unit
  ))
}

# panel za realni BDP
build_panel_real <- function(gdp_real) {
  group_means <- imap_dfr(groups, ~ make_group_mean(gdp_real, .x, .y)) |>
    mutate(group = as.character(group))

  group_real <- group_means |>
    group_by(group) |>
    group_modify(~ index_and_yoy(.x, first_year)) |>
    ungroup()

  hr_real <- gdp_real |>
    filter(geo == hr_code) |>
    select(year, value) |>
    index_and_yoy(base_year = first_year) |>
    mutate(group = "Croatia")

  bind_rows(group_real, hr_real) |>
    mutate(group_label = recode(group, !!!label_map))
}

# grafovi — samo s captionom
plot_gdp_yoy <- function(panel_df, caption = NULL, meta = NULL) {
  caption <- caption %||% if (!is.null(meta)) default_caption(meta) else paste0("Izvor: Eurostat ", dataset_id)

  ggplot(panel_df, aes(year, yoy_pct, color = group_label)) +
    geom_hline(yintercept = 0, color = "grey50", linewidth = 0.4) +
    geom_line(linewidth = 1) +
    scale_color_manual(values = cols_named, name = NULL) +
    labs(x = NULL, y = "%", caption = caption) +
    scale_x_continuous(
      limits = c(first_year, end_label_year + 0.5),
      breaks = seq(first_year, end_label_year, by = 1),
      labels = function(x) paste0(x, "."),
      expand = expansion(mult = c(0, 0))
    ) +
    scale_y_continuous(labels = scales::label_number(decimal.mark = ",")) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      axis.title.y = element_text(vjust = 0.5, margin = margin(r = 8)),
      axis.text.y = element_text(vjust = 0.5, hjust = 0.5),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
      plot.caption = element_text(size = 9, hjust = 1, margin = margin(t = 8))
    )
}

plot_gdp_index <- function(panel_df, caption = NULL, meta = NULL) {
  caption <- caption %||% if (!is.null(meta)) default_caption(meta) else paste0("Izvor: Eurostat ", dataset_id)

  ggplot(panel_df, aes(year, index_2000, color = group_label)) +
    geom_line(linewidth = 1) +
    scale_color_manual(values = cols_named, name = NULL) +
    labs(x = NULL, y = "Indeks 2000 = 100", caption = caption) +
    scale_x_continuous(
      limits = c(first_year, end_label_year + 0.5),
      breaks = seq(first_year, end_label_year, by = 1),
      labels = function(x) paste0(x, "."),
      expand = expansion(mult = c(0, 0))
    ) +
    scale_y_continuous(labels = scales::label_number(decimal.mark = ",")) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      axis.text.y = element_text(vjust = 0.5, hjust = 0.5),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
      plot.caption = element_text(size = 9, hjust = 1, margin = margin(t = 8))
    )
}


# ---------------------------------------------------------
# Primjer upotrebe
# ---------------------------------------------------------
# if (interactive()) {
#   dat   <- prepare_gdp_data()
#   panel <- build_panel_real(dat$gdp_real)
#   print(plot_gdp_index(panel, meta = dat))
#   print(plot_gdp_yoy(panel, meta = dat))
# }
