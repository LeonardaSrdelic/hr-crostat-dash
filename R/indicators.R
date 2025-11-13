# ========================================================= 
# HR Crostat Dash — BDP grafovi s izvorom (bez naslova)
# =========================================================

suppressPackageStartupMessages({
  library(eurostat); library(dplyr); library(tidyr); library(readr)
  library(ggplot2);  library(scales); library(purrr); library(janitor)
  library(stringr);  library(lubridate)
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

eu27_codes   <- c("AT","BE","BG","HR","CY","CZ","DK","EE","FI","FR","DE",
                  "GR","HU","IE","IT","LV","LT","LU","MT","NL","PL","PT",
                  "RO","SK","SI","ES","SE")
ea20_members <- c("AT","BE","CY","DE","EE","ES","FI","FR","GR","IE","IT",
                  "LV","LT","LU","MT","NL","PT","SI","SK","HR")
cee_all      <- c("BG","CZ","EE","HU","LT","LV","PL","RO","SI","SK","HR")

groups <- list(
  EU27_exHR = setdiff(eu27_codes, hr_code),
  EA_exHR   = setdiff(ea20_members, hr_code),
  CEE_exHR  = setdiff(cee_all, hr_code)
)

label_map <- c(
  "Croatia"   = "HR",
  "EU27_exHR" = "EU 27 isklj. HR",
  "EA_exHR"   = "EA isklj. HR",
  "CEE_exHR"  = "CEE isklj. HR"
)

cols_named <- c(
  "HR"              = "#C00000",
  "EU 27 isklj. HR" = "#303030",
  "EA isklj. HR"    = "#E69F00",
  "CEE isklj. HR"   = "#009E73"
)

# pomoćne boje usklađene
col_hr    <- cols_named[["HR"]]
col_other <- "grey80"

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

  list(
    gdp_nominal = gdp_nominal,
    gdp_real    = gdp_real,
    last_year   = last_year,
    real_unit   = real_unit
  )
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

# =========================================================
# Osnovni grafovi (panel: godišnja stopa rasta i indeks)
# =========================================================

plot_gdp_yoy <- function(panel_df, caption = NULL, meta = NULL) {
  caption <- caption %||%
    if (!is.null(meta)) default_caption(meta) else
      paste0("Izvor: Eurostat ", dataset_id)

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
      axis.title.y = element_text(angle = 0, vjust = 0.5,
                                  margin = margin(r = 8)),
      axis.text.y  = element_text(angle = 0, vjust = 0.5, hjust = 0.5),
      axis.text.x  = element_text(angle = 90, vjust = 1, hjust = 1),
      plot.caption = element_text(size = 9, hjust = 0,
                                  margin = margin(t = 8))
    )
}

plot_gdp_index <- function(panel_df, caption = NULL, meta = NULL) {
  caption <- caption %||%
    if (!is.null(meta)) default_caption(meta) else
      paste0("Izvor: Eurostat ", dataset_id)

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
      axis.title.y = element_text(angle = 90, vjust = 0.5,
                                  margin = margin(r = 8)),
      axis.text.y  = element_text(angle = 0, vjust = 0.5, hjust = 0.5),
      axis.text.x  = element_text(angle = 90, vjust = 1, hjust = 1),
      plot.caption = element_text(size = 9, hjust = 0,
                                  margin = margin(t = 8))
    )
}


# =========================================================
# Distribucija po državama unutar grupa (CEE/EU27/EA)
# =========================================================

# =========================================================
# Distribucija po državama unutar grupa (CEE/EU27/EA)
# =========================================================

group_display_name <- function(key) {
  dplyr::recode(
    key,
    CEE_exHR  = "CEE isklj. HR",
    EU27_exHR = "EU 27 isklj. HR",
    EA_exHR   = "EA isklj. HR",
    .default  = key
  )
}

# realni BDP po zemljama (koristi stupac value iz gdp_real)
build_real_by_geo <- function(gdp_real) {
  gdp_real |>
    dplyr::group_by(geo) |>
    dplyr::arrange(year, .by_group = TRUE) |>
    dplyr::mutate(
      has_base   = any(year == first_year & !is.na(value)),
      index_2000 = dplyr::if_else(
        has_base,
        100 * value / value[year == first_year][1],
        NA_real_
      ),
      yoy_pct    = 100 * (value / dplyr::lag(value) - 1)
    ) |>
    dplyr::ungroup() |>
    dplyr::filter(has_base)
}

plot_distribution_core <- function(country_series,
                                   group_series,
                                   hr_series,
                                   yvar,
                                   grp_name,
                                   ylab_txt,
                                   percent  = FALSE,
                                   caption  = NULL,
                                   meta     = NULL) {

  caption <- caption %||%
    if (!is.null(meta)) default_caption(meta) else
      paste0("Izvor: Eurostat ", dataset_id)

  x_limits <- c(first_year, end_label_year + 0.5)

  # Raspon
  range_df <- country_series |>
    dplyr::group_by(year) |>
    dplyr::summarise(
      ymin = min(.data[[yvar]], na.rm = TRUE),
      ymax = max(.data[[yvar]], na.rm = TRUE),
      .groups = "drop"
    )

  # Zadnje vrijednosti HR i grupe
  hr_last <- hr_series |>
    dplyr::filter(year == max(year)) |>
    dplyr::mutate(
      lbl = formatC(.data[[yvar]], digits = 1,
                    format = "f", decimal.mark=",")
    )

  grp_last <- group_series |>
    dplyr::filter(year == max(year)) |>
    dplyr::mutate(
      lbl = formatC(.data[[yvar]], digits = 1,
                    format = "f", decimal.mark=",")
    )

  # BOJE: HR = crvena, grupa = antracit
  col_group <- "#303030"

  col_values <- c(cols_named[["HR"]], col_group)
  names(col_values) <- c("HR", grp_name)

  ggplot2::ggplot() +
    ggplot2::geom_ribbon(
      data = range_df,
      ggplot2::aes(x = year, ymin = ymin, ymax = ymax,
                   fill = "Raspon zemalja"),
      alpha = 0.4,
      show.legend = TRUE
    ) +
    ggplot2::geom_line(
      data = group_series,
      ggplot2::aes(x = year, y = .data[[yvar]], color = grp_name),
      linewidth = 1.4
    ) +
    ggplot2::geom_line(
      data = hr_series,
      ggplot2::aes(x = year, y = .data[[yvar]], color = "HR"),
      linewidth = 1.4
    ) +
    ggplot2::geom_text(
      data = hr_last,
      ggplot2::aes(x = year + 0.3, y = .data[[yvar]],
                   label = lbl, color="HR"),
      hjust = 0, vjust = 0.5,
      size = 3.5, show.legend = FALSE
    ) +
    ggplot2::geom_text(
      data = grp_last,
      ggplot2::aes(x = year + 0.3, y = .data[[yvar]],
                   label = lbl, color=grp_name),
      hjust = 0, vjust = 0.5,
      size = 3.5, show.legend = FALSE
    ) +
    ggplot2::scale_color_manual(
      name   = NULL,
      values = col_values,
      breaks = c("HR", grp_name)
    ) +
    ggplot2::scale_fill_manual(
      name   = NULL,
      values = c("Raspon zemalja" = "grey80")
    ) +
    ggplot2::guides(
      color = ggplot2::guide_legend(
        order = 1,
        override.aes = list(fill = NA, linewidth = 1.6)
      ),
      fill = ggplot2::guide_legend(
        order = 2,
        override.aes = list(linetype = 0, colour = NA)
      )
    ) +
    ggplot2::labs(
      x = NULL,
      y = ylab_txt,
      caption = caption
    ) +
    ggplot2::scale_x_continuous(
      limits = x_limits,
      breaks = seq(first_year, end_label_year, 1),
      labels = function(x) paste0(x, "."),
      expand = ggplot2::expansion(mult = c(0, 0))
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::label_number(decimal.mark=",")
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position  = "bottom",
      panel.grid.minor = ggplot2::element_blank(),
      axis.title.y     = ggplot2::element_text(angle=90, vjust=0.5),
      axis.text.y      = ggplot2::element_text(angle=0, hjust=0.5),
      axis.text.x      = ggplot2::element_text(angle=90, vjust=0.5, hjust=1),
      plot.caption     = ggplot2::element_text(
        size=9, hjust=1, margin=ggplot2::margin(t=8)
      )
    )
}

plot_gdp_index_distribution <- function(gdp_real,
                                        panel_df,
                                        group_key,
                                        caption = NULL,
                                        meta    = NULL) {
  grp_name    <- group_display_name(group_key)
  real_by_geo <- build_real_by_geo(gdp_real)

  country_idx <- real_by_geo |>
    dplyr::filter(geo %in% groups[[group_key]]) |>
    dplyr::select(geo, year, index_2000)

  grp_idx <- panel_df |>
    dplyr::filter(group == group_key) |>
    dplyr::select(year, index_2000)

  hr_idx <- panel_df |>
    dplyr::filter(group == "Croatia") |>
    dplyr::select(year, index_2000)

  plot_distribution_core(
    country_series = country_idx,
    group_series   = grp_idx,
    hr_series      = hr_idx,
    yvar           = "index_2000",
    grp_name       = grp_name,
    ylab_txt       = "Indeks 2000 = 100",
    percent        = FALSE,
    caption        = caption,
    meta           = meta
  )
}

plot_gdp_yoy_distribution <- function(gdp_real,
                                      panel_df,
                                      group_key,
                                      caption = NULL,
                                      meta    = NULL) {
  grp_name    <- group_display_name(group_key)
  real_by_geo <- build_real_by_geo(gdp_real)

  country_yoy <- real_by_geo |>
    dplyr::filter(geo %in% groups[[group_key]]) |>
    dplyr::select(geo, year, yoy_pct) |>
    dplyr::filter(!is.na(yoy_pct))

  grp_yoy <- panel_df |>
    dplyr::filter(group == group_key) |>
    dplyr::select(year, yoy_pct) |>
    dplyr::filter(!is.na(yoy_pct))

  hr_yoy <- panel_df |>
    dplyr::filter(group == "Croatia") |>
    dplyr::select(year, yoy_pct) |>
    dplyr::filter(!is.na(yoy_pct))

  plot_distribution_core(
    country_series = country_yoy,
    group_series   = grp_yoy,
    hr_series      = hr_yoy,
    yvar           = "yoy_pct",
    grp_name       = grp_name,
    ylab_txt       = "%",
    percent        = TRUE,
    caption        = caption,
    meta           = meta
  )
}


# ======================================================
# Hrvatska: realni BDP, kvartalne stope rasta (qoq)
# ======================================================

s_adj_try_q  <- c("SCA", "SA")
unit_try_q   <- c("CLV_MEUR", "CLV15_MEUR")

plot_gdp_qoq_hr <- function(start_cut = lubridate::yq("2019-Q1")) {
  hr_q   <- NULL
  s_used <- NA_character_
  u_used <- NA_character_

  for (s in s_adj_try_q) {
    for (u in unit_try_q) {
      dat <- tryCatch(
        eurostat::get_eurostat(
          id = "namq_10_gdp",
          filters = list(
            na_item = "B1GQ",
            s_adj   = s,
            unit    = u,
            geo     = hr_code
          ),
          time_format = "raw"
        ),
        error = function(e) NULL
      )

      if (!is.null(dat) && nrow(dat) > 0) {
        hr_q <- dat |>
          dplyr::rename(time_raw = time, value = values) |>
          dplyr::mutate(
            date = lubridate::yq(gsub("Q","-Q", time_raw))
          ) |>
          dplyr::arrange(date) |>
          dplyr::select(date, value)
        s_used <- s
        u_used <- u
        break
      }
    }
    if (!is.null(hr_q)) break
  }

  if (is.null(hr_q) || nrow(hr_q) == 0) {
    stop("Nema dostupnih podataka za kvartalni realni BDP Hrvatske")
  }

  hr_q <- hr_q |>
    dplyr::mutate(
      qoq  = 100 * (value / dplyr::lag(value) - 1),
      year = lubridate::year(date),
      qnum = lubridate::quarter(date),
      yq   = paste0(year, "-Q", qnum)
    ) |>
    dplyr::filter(date >= start_cut, !is.na(qoq)) |>
    dplyr::mutate(
      yq = factor(yq, levels = unique(yq)),
      label_qoq = dplyr::if_else(
        is.na(qoq),
        "",
        formatC(qoq, format = "f", digits = 1, decimal.mark = ",")
      )
    )

  caption <- paste0(
    "Izvor: Eurostat namq_10_gdp, B1GQ, s_adj = ",
    s_used,
    ", unit = ",
    u_used,
    ", geo = ",
    hr_code,
    " · © Leonarda Srdelić"
  )

  ggplot2::ggplot(hr_q, ggplot2::aes(x = yq, y = qoq)) +
    ggplot2::geom_col(
      fill  = col_hr,
      width = 0.72
    ) +
    ggplot2::geom_hline(yintercept = 0, color = "grey60") +
    ggplot2::geom_text(
      ggplot2::aes(label = label_qoq),
      vjust = ifelse(hr_q$qoq >= 0, -0.35, 1.2),
      size  = 3.0
    ) +
    ggplot2::scale_x_discrete(drop = FALSE) +
    ggplot2::scale_y_continuous(
      labels = scales::label_number(
        accuracy = 0.1,
        decimal.mark = ","
      )
    ) +
    ggplot2::labs(
      x = NULL,
      y = "%",
      caption = caption
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      axis.text.x  = ggplot2::element_text(
        angle = 90, vjust = 1, hjust = 1
      ),
      axis.text.y  = ggplot2::element_text(
        angle = 0, vjust = 0.5, hjust = 0.5
      ),
      axis.title.y = ggplot2::element_text(
        angle = 0,
        vjust = 0.5,
        margin = ggplot2::margin(r = 8)
      ),
      plot.caption = ggplot2::element_text(
        hjust = 0,
        size  = 9
      )
    )
}

# ======================================================
# BDP po stanovniku u PPS: CEE zemlje, zadnja godina
# ======================================================

plot_gdp_pc_cee_pps_last <- function(start_year = 2010) {
  hr_code   <- "HR"
  eu_code   <- "EU27_2020"
  cee_codes <- c("BG","CZ","EE","HU","LT","LV","PL","RO","SI","SK","HR")
  units_pps <- "CP_PPS_EU27_2020_HAB"

  col_hr    <- cols_named[["HR"]]
  col_other <- "grey80"

  pc_pps <- eurostat::get_eurostat(
    id = "nama_10_pc",
    filters = list(
      na_item = "B1GQ",
      unit    = units_pps,
      geo     = c(cee_codes, eu_code)
    ),
    time_format = "num"
  ) |>
    dplyr::rename(year = time, pps = values) |>
    dplyr::filter(year >= start_year)

  if (nrow(pc_pps) == 0L) {
    stop("Prazan skup za PPS per capita. Provjeri CP_PPS_EU27_2020_HAB.")
  }

  eu_pps <- pc_pps |>
    dplyr::filter(geo == eu_code) |>
    dplyr::select(year, eu_pps = pps)

  last_year <- max(pc_pps$year, na.rm = TRUE)

  last_df <- pc_pps |>
    dplyr::filter(year == last_year, geo %in% cee_codes) |>
    dplyr::left_join(
      eu_pps |>
        dplyr::filter(year == last_year),
      by = "year"
    ) |>
    dplyr::mutate(
      ratio = 100 * pps / eu_pps
    ) |>
    dplyr::arrange(ratio) |>
    dplyr::mutate(
      geo = factor(geo, levels = geo)
    )

  caption_txt <- paste0(
    "Izvor: Eurostat nama_10_pc, B1GQ, unit = ", units_pps,
    ", EU27_2020 = 100 · © Leonarda Srdelić"
  )

  ggplot2::ggplot(last_df, ggplot2::aes(x = geo, y = ratio, fill = geo == hr_code)) +
    ggplot2::geom_col(width = 0.7) +
    ggplot2::geom_text(
      ggplot2::aes(label = sprintf("%.0f", ratio)),
      vjust = -0.3,
      size  = 3.5
    ) +
    ggplot2::scale_fill_manual(
      values = c("TRUE" = col_hr, "FALSE" = col_other),
      guide  = "none"
    ) +
    ggplot2::labs(
      x       = NULL,
      y       = "EU27 = 100",
      caption = caption_txt
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      axis.text.x        = ggplot2::element_text(
        angle = 0, vjust = 1, hjust = 1
      ),
      axis.text.y        = ggplot2::element_text(
        angle = 0, vjust = 0.5, hjust = 0.5
      ),
      axis.title.y       = ggplot2::element_text(
        angle  = 90,
        vjust  = 0.5,
        margin = ggplot2::margin(r = 8)
      ),
      plot.caption       = ggplot2::element_text(
        hjust = 0,
        size  = 9
      )
    )
}

# ======================================================
# Nominalni BDP: stopa rasta 2023. u odnosu na 2022. (EU 27)
# ======================================================

plot_gdp_nominal_growth_eu27_2023_vs_2022 <- function() {
  use_cache()

  ngdp <- eurostat::get_eurostat(
    id = dataset_id,
    filters = list(
      na_item = "B1GQ",
      unit    = "CP_MEUR",
      geo     = eu27_codes
    ),
    time_format = "num"
  ) |>
    dplyr::rename(year = time, value = values) |>
    dplyr::filter(year %in% c(2022L, 2023L))

  if (nrow(ngdp) == 0L) {
    stop("Nema podataka za nominalni BDP (CP_MEUR) za 2022. i 2023.")
  }

  ngdp_clean <- ngdp |>
    dplyr::group_by(geo, year) |>
    dplyr::summarise(
      value = sum(value, na.rm = TRUE),
      .groups = "drop"
    )

  ngdp_wide <- ngdp_clean |>
    tidyr::pivot_wider(
      names_from  = year,
      values_from = value,
      names_prefix = "y"
    ) |>
    dplyr::mutate(
      stopa = 100 * (y2023 - y2022) / y2022
    )

  plot_df <- ngdp_wide |>
    dplyr::filter(!is.na(stopa)) |>
    dplyr::arrange(dplyr::desc(stopa)) |>
    dplyr::mutate(
      label = scales::number(
        stopa,
        accuracy     = 0.1,
        decimal.mark = ","
      ),
      geo   = factor(geo, levels = geo),
      is_hr = geo == hr_code
    )

  eu_avg <- mean(plot_df$stopa, na.rm = TRUE)

  caption_txt <- paste0(
    "Izvor: Eurostat ", dataset_id,
    ", B1GQ, CP_MEUR, stopa promjene 2023 u odnosu na 2022 · © Leonarda Srdelić"
  )

  ggplot2::ggplot(plot_df, ggplot2::aes(x = geo, y = stopa, fill = is_hr)) +
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::scale_fill_manual(
      values = c(
        "TRUE"  = cols_named[["HR"]],
        "FALSE" = "grey80"
      )
    ) +
    ggplot2::geom_hline(
      yintercept = eu_avg,
      colour     = "grey30",
      linewidth  = 0.6
    ) +
    ggplot2::annotate(
      "text",
      x     = Inf,
      y     = eu_avg,
      label = paste0(
        "Prosjek EU-27: ",
        scales::number(
          eu_avg,
          accuracy     = 0.1,
          decimal.mark = ","
        ),
        " %"
      ),
      hjust = 1.1,
      vjust = -0.3,
      size  = 3.5,
      colour = "grey30"
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = label),
      vjust = -0.5,
      size  = 3
    ) +
    ggplot2::scale_y_continuous(
      labels = function(x) scales::number(
        x,
        accuracy     = 0.1,
        decimal.mark = ","
      )
    ) +
    ggplot2::labs(
      x       = NULL,
      y       = "%",
      caption = caption_txt
    ) +
    ggplot2::expand_limits(
      y = max(plot_df$stopa, na.rm = TRUE) * 1.15
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      axis.text.x      = ggplot2::element_text(
        angle = 90,
        hjust = 1
      ),
      axis.text.y      = ggplot2::element_text(
        angle = 0, vjust = 0.5, hjust = 0.5
      ),
      axis.title.y     = ggplot2::element_text(
        angle  = 90,
        vjust  = 0.5,
        margin = ggplot2::margin(r = 8)
      ),
      plot.caption     = ggplot2::element_text(
        hjust = 0,
        size  = 9
      )
    )
}

# ======================================================
# EU-27: stope rasta nominalnog i realnog BDP-a (najnovije)
# ======================================================

plot_gdp_nominal_growth_eu27_latest <- function() {
  use_cache()

  ngdp <- eurostat::get_eurostat(
    id = dataset_id,
    filters = list(
      na_item = "B1GQ",
      unit    = "CP_MEUR",
      geo     = eu27_codes
    ),
    time_format = "num"
  ) |>
    dplyr::rename(year = time, value = values)

  if (nrow(ngdp) == 0L) {
    stop("Nema podataka za nominalni BDP (CP_MEUR) za zemlje EU-27.")
  }

  years_avail <- sort(unique(ngdp$year), na.last = NA)
  year_t      <- max(years_avail)
  year_tm1    <- max(years_avail[years_avail < year_t])

  ngdp_clean <- ngdp |>
    dplyr::filter(year %in% c(year_tm1, year_t)) |>
    dplyr::group_by(geo, year) |>
    dplyr::summarise(
      value = sum(value, na.rm = TRUE),
      .groups = "drop"
    )

  ngdp_wide <- ngdp_clean |>
    tidyr::pivot_wider(
      names_from  = year,
      values_from = value,
      names_prefix = "y"
    )

  yt   <- paste0("y", year_t)
  ytm1 <- paste0("y", year_tm1)

  plot_df <- ngdp_wide |>
    dplyr::mutate(
      stopa = 100 * (.data[[yt]] - .data[[ytm1]]) / .data[[ytm1]]
    ) |>
    dplyr::filter(!is.na(stopa)) |>
    dplyr::arrange(dplyr::desc(stopa)) |>
    dplyr::mutate(
      label = scales::number(
        stopa,
        accuracy     = 0.1,
        decimal.mark = ","
      ),
      geo   = factor(geo, levels = geo),
      is_hr = geo == hr_code
    )

  eu_avg <- mean(plot_df$stopa, na.rm = TRUE)

  caption_txt <- paste0(
    "Izvor: Eurostat ", dataset_id,
    ", B1GQ, CP_MEUR, stopa promjene ",
    year_t, " u odnosu na ", year_tm1,
    " · © Leonarda Srdelić"
  )

  ggplot2::ggplot(plot_df, ggplot2::aes(x = geo, y = stopa, fill = is_hr)) +
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::scale_fill_manual(
      values = c(
        "TRUE"  = cols_named[["HR"]],
        "FALSE" = "grey80"
      )
    ) +
    ggplot2::geom_hline(
      yintercept = eu_avg,
      colour     = "grey30",
      linewidth  = 0.6
    ) +
    ggplot2::annotate(
      "text",
      x     = Inf,
      y     = eu_avg,
      label = paste0(
        "Prosjek EU-27: ",
        scales::number(
          eu_avg,
          accuracy     = 0.1,
          decimal.mark = ","
        ),
        " %"
      ),
      hjust  = 1.1,
      vjust  = -0.3,
      size   = 3.5,
      colour = "grey30"
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = label),
      vjust = -0.5,
      size  = 3
    ) +
    ggplot2::scale_y_continuous(
      labels = function(x) scales::number(
        x,
        accuracy     = 0.1,
        decimal.mark = ","
      )
    ) +
    ggplot2::expand_limits(
      y = max(plot_df$stopa, na.rm = TRUE) * 1.15
    ) +
    ggplot2::labs(
      x       = NULL,
      y       = "%",
      caption = caption_txt
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      axis.text.x      = ggplot2::element_text(
        angle = 0,
        hjust = 1
      ),
      axis.text.y      = ggplot2::element_text(
        angle = 0, vjust = 0.5, hjust = 0.5
      ),
      axis.title.y     = ggplot2::element_text(
        angle  = 0,
        vjust  = 0.5,
        margin = ggplot2::margin(r = 8)
      ),
      plot.caption     = ggplot2::element_text(
        hjust = 0,
        size  = 9
      )
    )
}

plot_gdp_real_growth_eu27_latest <- function() {
  use_cache()

  real_unit <- latest_clv_unit()

  rgdp <- eurostat::get_eurostat(
    id = dataset_id,
    filters = list(
      na_item = "B1GQ",
      unit    = real_unit,
      geo     = eu27_codes
    ),
    time_format = "num"
  ) |>
    dplyr::rename(year = time, value = values)

  if (nrow(rgdp) == 0L) {
    stop("Nema podataka za realni BDP (", real_unit, ") za zemlje EU-27.")
  }

  years_avail <- sort(unique(rgdp$year), na.last = NA)
  year_t      <- max(years_avail)
  year_tm1    <- max(years_avail[years_avail < year_t])

  rgdp_clean <- rgdp |>
    dplyr::filter(year %in% c(year_tm1, year_t)) |>
    dplyr::group_by(geo, year) |>
    dplyr::summarise(
      value = sum(value, na.rm = TRUE),
      .groups = "drop"
    )

  rgdp_wide <- rgdp_clean |>
    tidyr::pivot_wider(
      names_from  = year,
      values_from = value,
      names_prefix = "y"
    )

  yt   <- paste0("y", year_t)
  ytm1 <- paste0("y", year_tm1)

  plot_df <- rgdp_wide |>
    dplyr::mutate(
      stopa = 100 * (.data[[yt]] - .data[[ytm1]]) / .data[[ytm1]]
    ) |>
    dplyr::filter(!is.na(stopa)) |>
    dplyr::arrange(dplyr::desc(stopa)) |>
    dplyr::mutate(
      label = scales::number(
        stopa,
        accuracy     = 0.1,
        decimal.mark = ","
      ),
      geo   = factor(geo, levels = geo),
      is_hr = geo == hr_code
    )

  eu_avg <- mean(plot_df$stopa, na.rm = TRUE)

  caption_txt <- paste0(
    "Izvor: Eurostat ", dataset_id,
    ", B1GQ, jedinica ", real_unit,
    ", stopa promjene ",
    year_t, " u odnosu na ", year_tm1,
    " · © Leonarda Srdelić"
  )

  ggplot2::ggplot(plot_df, ggplot2::aes(x = geo, y = stopa, fill = is_hr)) +
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::scale_fill_manual(
      values = c(
        "TRUE"  = cols_named[["HR"]],
        "FALSE" = "grey80"
      )
    ) +
    ggplot2::geom_hline(
      yintercept = eu_avg,
      colour     = "grey30",
      linewidth  = 0.6
    ) +
    ggplot2::annotate(
      "text",
      x     = Inf,
      y     = eu_avg,
      label = paste0(
        "Prosjek EU-27: ",
        scales::number(
          eu_avg,
          accuracy     = 0.1,
          decimal.mark = ","
        ),
        " %"
      ),
      hjust  = 1.1,
      vjust  = -0.3,
      size   = 3.5,
      colour = "grey30"
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = label),
      vjust = -0.5,
      size  = 3
    ) +
    ggplot2::scale_y_continuous(
      labels = function(x) scales::number(
        x,
        accuracy     = 0.1,
        decimal.mark = ","
      )
    ) +
    ggplot2::expand_limits(
      y = max(plot_df$stopa, na.rm = TRUE) * 1.15
    ) +
    ggplot2::labs(
      x       = NULL,
      y       = "%",
      caption = caption_txt
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      axis.text.x      = ggplot2::element_text(
        angle = 0,
        hjust = 1
      ),
      axis.text.y      = ggplot2::element_text(
        angle = 0, vjust = 0.5, hjust = 0.5
      ),
      axis.title.y     = ggplot2::element_text(
        angle  = 0,
        vjust  = 0.5,
        margin = ggplot2::margin(r = 8)
      ),
      plot.caption     = ggplot2::element_text(
        hjust = 0,
        size  = 9
      )
    )
}


# ======================================================
# EU 27: kvartalne godišnje stope rasta nominalnog BDP a
# ======================================================

plot_gdp_nominal_q_yoy_eu27_latest <- function() {
  use_cache()

  qdat <- NULL
  s_used <- NA_character_

  for (s in s_adj_try_q) {
    cand <- tryCatch(
      eurostat::get_eurostat(
        id = "namq_10_gdp",
        filters = list(
          na_item = "B1GQ",
          s_adj   = s,
          unit    = "CP_MEUR",
          geo     = eu27_codes
        ),
        time_format = "raw"
      ),
      error = function(e) NULL
    )

    if (!is.null(cand) && nrow(cand) > 0) {
      qdat <- cand
      s_used <- s
      break
    }
  }

  if (is.null(qdat) || nrow(qdat) == 0) {
    stop("Nema podataka za kvartalni nominalni BDP (CP_MEUR) za zemlje EU 27.")
  }

  qdat <- qdat |>
    dplyr::rename(time_raw = time, value = values) |>
    dplyr::mutate(
      date = lubridate::yq(gsub("Q", "-Q", time_raw))
    ) |>
    dplyr::arrange(geo, date) |>
    dplyr::group_by(geo) |>
    dplyr::mutate(
      yoy = 100 * (value / dplyr::lag(value, 4) - 1)
    ) |>
    dplyr::ungroup()

  last_date <- max(qdat$date[!is.na(qdat$yoy)], na.rm = TRUE)

  plot_df <- qdat |>
    dplyr::filter(date == last_date, !is.na(yoy)) |>
    dplyr::arrange(dplyr::desc(yoy)) |>
    dplyr::mutate(
      label = scales::number(
        yoy,
        accuracy     = 0.1,
        decimal.mark = ","
      ),
      geo   = factor(geo, levels = geo),
      is_hr = geo == hr_code
    )

  eu_avg <- mean(plot_df$yoy, na.rm = TRUE)

  caption_txt <- paste0(
    "Izvor: Eurostat namq_10_gdp, B1GQ, CP_MEUR, s_adj = ",
    s_used,
    ", stopa promjene u odnosu na isti kvartal prethodne godine, ",
    format(last_date, "%Y Q%q"),
    " · © Leonarda Srdelić"
  )

  ggplot2::ggplot(plot_df, ggplot2::aes(x = geo, y = yoy, fill = is_hr)) +
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::scale_fill_manual(
      values = c(
        "TRUE"  = cols_named[["HR"]],
        "FALSE" = "grey80"
      )
    ) +
    ggplot2::geom_hline(
      yintercept = eu_avg,
      colour     = "grey30",
      linewidth  = 0.6
    ) +
    ggplot2::annotate(
      "text",
      x     = Inf,
      y     = eu_avg,
      label = paste0(
        "Prosjek EU 27: ",
        scales::number(
          eu_avg,
          accuracy     = 0.1,
          decimal.mark = ","
        ),
        " %"
      ),
      hjust  = 1.1,
      vjust  = -0.3,
      size   = 3.5,
      colour = "grey30"
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = label),
      vjust = -0.5,
      size  = 3
    ) +
    ggplot2::scale_y_continuous(
      labels = function(x) scales::number(
        x,
        accuracy     = 0.1,
        decimal.mark = ","
      )
    ) +
    ggplot2::labs(
      x       = NULL,
      y       = "%",
      caption = caption_txt
    ) +
    ggplot2::expand_limits(
      y = max(plot_df$yoy, na.rm = TRUE) * 1.15
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      axis.text.x      = ggplot2::element_text(
        angle = 90,
        hjust = 1
      ),
      axis.text.y      = ggplot2::element_text(
        angle = 0, vjust = 0.5, hjust = 0.5
      ),
      axis.title.y     = ggplot2::element_text(
        angle  = 90,
        vjust  = 0.5,
        margin = ggplot2::margin(r = 8)
      ),
      plot.caption     = ggplot2::element_text(
        hjust = 0,
        size  = 9
      )
    )
}

# ======================================================
# EU 27: kvartalne godišnje stope rasta realnog BDP a
# ======================================================

# ======================================================
# EU 27: REALNI kvartalni BDP, yoy stope za t-1
# B1GQ, SCA, CLV20_MEUR
# ======================================================

plot_gdp_real_q_yoy_eu27_tminus1 <- function() {
  use_cache()

  # Dohvat podataka: realni BDP, sezonski prilagođen, CLV20_MEUR
  qdat <- eurostat::get_eurostat(
    id = "namq_10_gdp",
    filters = list(
      na_item = "B1GQ",
      s_adj   = "SCA",
      unit    = "CLV20_MEUR",
      geo     = eu27_codes
    ),
    time_format = "raw"
  ) |>
    dplyr::rename(time_raw = time, value = values) |>
    dplyr::mutate(
      date = lubridate::yq(gsub("Q", "-Q", time_raw))
    ) |>
    dplyr::arrange(geo, date) |>
    dplyr::group_by(geo) |>
    dplyr::mutate(
      yoy = 100 * (value / dplyr::lag(value, 4) - 1)
    ) |>
    dplyr::ungroup()

  if (nrow(qdat) == 0L) {
    stop("Nema podataka za namq_10_gdp, B1GQ, SCA, CLV20_MEUR za EU-27.")
  }

  # Zadnji dostupni kvartal uopće (t), pa t-1
  last_date_all <- max(qdat$date, na.rm = TRUE)
  t_minus1      <- last_date_all %m-% months(3)

  # Ako za t-1 nema izračunate yoy za neke zemlje, filtrira ih se van
  plot_df <- qdat |>
    dplyr::filter(date == t_minus1, !is.na(yoy)) |>
    dplyr::arrange(dplyr::desc(yoy)) |>
    dplyr::mutate(
      label = scales::number(
        yoy,
        accuracy     = 0.1,
        decimal.mark = ","
      ),
      geo   = factor(geo, levels = geo),
      is_hr = geo == hr_code
    )

  if (nrow(plot_df) == 0L) {
    stop("Za t-1 nema yoy stopa za nijednu zemlju.")
  }

  eu_avg <- mean(plot_df$yoy, na.rm = TRUE)

  year_q <- lubridate::year(t_minus1)
  q_q    <- lubridate::quarter(t_minus1)

  caption_txt <- paste0(
    "Izvor: Eurostat namq_10_gdp, B1GQ, s_adj = SCA, jedinica CLV20_MEUR, ",
    "stopa promjene u odnosu na isti kvartal prethodne godine, ",
    year_q, " Q", q_q,
    " · © Leonarda Srdelić"
  )

  ggplot2::ggplot(plot_df, ggplot2::aes(x = geo, y = yoy, fill = is_hr)) +
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::scale_fill_manual(
      values = c(
        "TRUE"  = cols_named[["HR"]],
        "FALSE" = "grey80"
      )
    ) +
    ggplot2::geom_hline(
      yintercept = eu_avg,
      colour     = "grey30",
      linewidth  = 0.6
    ) +
    ggplot2::annotate(
      "text",
      x     = Inf,
      y     = eu_avg,
      label = paste0(
        "Prosjek EU-27: ",
        scales::number(
          eu_avg,
          accuracy     = 0.1,
          decimal.mark = ","
        ),
        " %"
      ),
      hjust  = 1.1,
      vjust  = -0.3,
      size   = 3.5,
      colour = "grey30"
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = label),
      vjust = -0.5,
      size  = 3
    ) +
    ggplot2::scale_y_continuous(
      labels = function(x) scales::number(
        x,
        accuracy     = 0.1,
        decimal.mark = ","
      )
    ) +
    ggplot2::expand_limits(
      y = max(plot_df$yoy, na.rm = TRUE) * 1.15
    ) +
    ggplot2::labs(
      x       = NULL,
      y       = "%"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      axis.text.x      = ggplot2::element_text(
        angle = 90,
        hjust = 1
      ),
      axis.text.y      = ggplot2::element_text(
        angle = 0, vjust = 0.5, hjust = 0.5
      ),
      axis.title.y     = ggplot2::element_text(
        angle  = 0,
        vjust  = 0.5,
        margin = ggplot2::margin(r = 8)
      ),
      plot.caption     = ggplot2::element_text(
        hjust = 0,
        size  = 9
      )
    )
}



# ======================================================
# EU 27: NOMINALNI kvartalni BDP, yoy stope za t-1
# B1GQ, SCA, CP_MEUR
# ======================================================

plot_gdp_nominal_q_yoy_eu27_tminus1 <- function() {
  use_cache()

  qdat <- eurostat::get_eurostat(
    id = "namq_10_gdp",
    filters = list(
      na_item = "B1GQ",
      s_adj   = "SCA",
      unit    = "CP_MEUR",
      geo     = eu27_codes
    ),
    time_format = "raw"
  ) |>
    dplyr::rename(time_raw = time, value = values) |>
    dplyr::mutate(
      date = lubridate::yq(gsub("Q", "-Q", time_raw))
    ) |>
    dplyr::arrange(geo, date) |>
    dplyr::group_by(geo) |>
    dplyr::mutate(
      yoy = 100 * (value / dplyr::lag(value, 4) - 1)
    ) |>
    dplyr::ungroup()

  if (nrow(qdat) == 0L) {
    stop("Nema podataka za namq_10_gdp, B1GQ, SCA, CP_MEUR za EU 27.")
  }

  last_date_all <- max(qdat$date, na.rm = TRUE)
  t_minus1      <- last_date_all %m-% months(3)

  plot_df <- qdat |>
    dplyr::filter(date == t_minus1, !is.na(yoy)) |>
    dplyr::arrange(dplyr::desc(yoy)) |>
    dplyr::mutate(
      label = scales::number(
        yoy,
        accuracy     = 0.1,
        decimal.mark = ","
      ),
      geo   = factor(geo, levels = geo),
      is_hr = geo == hr_code
    )

  if (nrow(plot_df) == 0L) {
    stop("Za t minus 1 nema yoy stopa ni za jednu zemlju.")
  }

  eu_avg <- mean(plot_df$yoy, na.rm = TRUE)

  year_q <- lubridate::year(t_minus1)
  q_q    <- lubridate::quarter(t_minus1)

  caption_txt <- paste0(
    "Izvor: Eurostat namq_10_gdp, B1GQ, s_adj = SCA, jedinica CP_MEUR, ",
    "stopa promjene u odnosu na isti kvartal prethodne godine, ",
    year_q, " Q", q_q,
    " · © Leonarda Srdelić"
  )

  ggplot2::ggplot(plot_df, ggplot2::aes(x = geo, y = yoy, fill = is_hr)) +
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::scale_fill_manual(
      values = c(
        "TRUE"  = cols_named[["HR"]],
        "FALSE" = "grey80"
      )
    ) +
    ggplot2::geom_hline(
      yintercept = eu_avg,
      colour     = "grey30",
      linewidth  = 0.6
    ) +
    ggplot2::annotate(
      "text",
      x     = Inf,
      y     = eu_avg,
      label = paste0(
        "Prosjek EU 27: ",
        scales::number(
          eu_avg,
          accuracy     = 0.1,
          decimal.mark = ","
        ),
        " %"
      ),
      hjust  = 1.1,
      vjust  = -0.3,
      size   = 3.5,
      colour = "grey30"
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = label),
      vjust = -0.5,
      size  = 3
    ) +
    ggplot2::scale_y_continuous(
      labels = function(x) scales::number(
        x,
        accuracy     = 0.1,
        decimal.mark = ","
      )
    ) +
    ggplot2::expand_limits(
      y = max(plot_df$yoy, na.rm = TRUE) * 1.15
    ) +
    ggplot2::labs(
      x       = NULL,
      y       = "%"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      axis.text.x      = ggplot2::element_text(
        angle = 90,
        hjust = 1
      ),
      axis.text.y      = ggplot2::element_text(
        angle = 0, vjust = 0.5, hjust = 0.5
      ),
      axis.title.y     = ggplot2::element_text(
        angle  = 0,
        vjust  = 0.5,
        margin = ggplot2::margin(r = 8)
      ),
      plot.caption     = ggplot2::element_text(
        hjust = 0,
        size  = 9
      )
    )
}

# ======================================================
# EU 27: REALNI kvartalni BDP, qoq stope za t minus 1
# B1GQ, SCA, CLV20_MEUR
# ======================================================

plot_gdp_real_q_qoq_eu27_tminus1 <- function() {
  use_cache()

  qdat <- eurostat::get_eurostat(
    id = "namq_10_gdp",
    filters = list(
      na_item = "B1GQ",
      s_adj   = "SCA",
      unit    = "CLV20_MEUR",
      geo     = eu27_codes
    ),
    time_format = "raw"
  ) |>
    dplyr::rename(time_raw = time, value = values) |>
    dplyr::mutate(
      date = lubridate::yq(gsub("Q", "-Q", time_raw))
    ) |>
    dplyr::arrange(geo, date) |>
    dplyr::group_by(geo) |>
    dplyr::mutate(
      qoq = 100 * (value / dplyr::lag(value, 1) - 1)
    ) |>
    dplyr::ungroup()

  if (nrow(qdat) == 0L) {
    stop("Nema podataka za namq_10_gdp, B1GQ, SCA, CLV20_MEUR za EU 27.")
  }

  # zadnji dostupni kvartal = t, pa t minus 1
  last_date_all <- max(qdat$date, na.rm = TRUE)
  t_minus1      <- last_date_all %m-% months(3)

  plot_df <- qdat |>
    dplyr::filter(date == t_minus1, !is.na(qoq)) |>
    dplyr::arrange(dplyr::desc(qoq)) |>
    dplyr::mutate(
      label = scales::number(
        qoq,
        accuracy     = 0.1,
        decimal.mark = ","
      ),
      geo   = factor(geo, levels = geo),
      is_hr = geo == hr_code
    )

  if (nrow(plot_df) == 0L) {
    stop("Za t minus 1 nema qoq stopa ni za jednu zemlju.")
  }

  eu_avg <- mean(plot_df$qoq, na.rm = TRUE)

  year_q <- lubridate::year(t_minus1)
  q_q    <- lubridate::quarter(t_minus1)

  caption_txt <- paste0(
    "Izvor: Eurostat namq_10_gdp, B1GQ, s_adj = SCA, jedinica CLV20_MEUR, ",
    "stopa promjene u odnosu na prethodno tromjesečje, ",
    year_q, " Q", q_q,
    " · © Leonarda Srdelić"
  )

  ggplot2::ggplot(plot_df, ggplot2::aes(x = geo, y = qoq, fill = is_hr)) +
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::scale_fill_manual(
      values = c(
        "TRUE"  = cols_named[["HR"]],
        "FALSE" = "grey80"
      )
    ) +
    ggplot2::geom_hline(
      yintercept = eu_avg,
      colour     = "grey30",
      linewidth  = 0.6
    ) +
    ggplot2::annotate(
      "text",
      x     = Inf,
      y     = eu_avg,
      label = paste0(
        "Prosjek EU 27: ",
        scales::number(
          eu_avg,
          accuracy     = 0.1,
          decimal.mark = ","
        ),
        " %"
      ),
      hjust  = 1.1,
      vjust  = -0.3,
      size   = 3.5,
      colour = "grey30"
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = label),
      vjust = -0.5,
      size  = 3
    ) +
    ggplot2::scale_y_continuous(
      labels = function(x) scales::number(
        x,
        accuracy     = 0.1,
        decimal.mark = ","
      )
    ) +
    ggplot2::expand_limits(
      y = max(plot_df$qoq, na.rm = TRUE) * 1.15
    ) +
    ggplot2::labs(
      x       = NULL,
      y       = "%",
      caption = caption_txt
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      axis.text.x      = ggplot2::element_text(
        angle = 90,
        hjust = 1
      ),
      axis.text.y      = ggplot2::element_text(
        angle = 0, vjust = 0.5, hjust = 0.5
      ),
      axis.title.y     = ggplot2::element_text(
        angle  = 0,
        vjust  = 0.5,
        margin = ggplot2::margin(r = 8)
      ),
      plot.caption     = ggplot2::element_text(
        hjust = 0,
        size  = 9
      )
    )
}
