# ========================================================= 
# HR Crostat Dash — BDP grafovi s izvorom 
# Encoding: UTF-8
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
end_label_year <- lubridate::year(Sys.Date())

eu27_codes   <- c("AT","BE","BG","HR","CY","CZ","DK","EE","FI","FR","DE",
                  "GR","HU","IE","IT","LV","LT","LU","MT","NL","PL","PT",
                  "RO","SK","SI","ES","SE")
ea20_members <- c("AT","BE","CY","DE","EE","ES","FI","FR","GR","IE","IT",
                  "LV","LT","LU","MT","NL","PT","SI","SK","HR")
eu_aggregate_code <- "EU27_2020"
ea_aggregate_code <- "EA20"
cee_all      <- c("BG","CZ","EE","HU","LT","LV","PL","RO","SI","SK","HR")

groups <- list(
  EU27_exHR = setdiff(eu27_codes, hr_code),
  EA_exHR   = setdiff(ea20_members, hr_code),
  CEE_exHR  = setdiff(cee_all, hr_code)
)

aggregate_group_codes <- c(
  EU27_exHR = eu_aggregate_code,
  EA_exHR   = ea_aggregate_code
)

label_map <- c(
  "Croatia"   = "HR",
  "EU27_exHR" = "EU 27",
  "EA_exHR"   = "EA",
  "CEE_exHR"  = "CEE"
)

cols_named <- c(
  "HR"              = "#C00000",
  "EU 27" = "#000000",
  "EA"    = "#808080",
  "CEE"   = "#1f77b4"
)

`%||%` <- function(x, y) if (is.null(x) || identical(x, "")) y else x

# Godine kao string s točkom
fmt_year <- function(y) paste0(y, ".")

# Y-axis title layout helper: horizontal for % labels, vertical for word labels.
axis_name_props <- function(name) {
  rotate <- 0
  if (!is.null(name) && nzchar(name) && !grepl("%", name, fixed = TRUE)) {
    rotate <- 90
  }
  list(
    rotate = rotate,
    gap    = if (rotate == 0) 30 else 40
  )
}

# Helper to place caption/note lines below chart (left aligned).
add_caption_bottom <- function(e, caption, base_bottom = -4, step = 20) {
  if (is.null(caption) || identical(caption, "")) return(e)
  lines <- if (is.list(caption)) unlist(caption) else as.character(caption)
  lines <- trimws(lines)
  lines <- lines[nzchar(lines)]
  if (!length(lines)) return(e)

  bottoms <- base_bottom + step * rev(seq_along(lines) - 1)

  for (i in seq_along(lines)) {
    e <- e |>
      echarts4r::e_title(
        text      = lines[i],
        bottom    = bottoms[i],
        left      = "left",
        textStyle = list(
          fontSize   = 11,
          fontWeight = "normal",
          color      = "#555555"
        ),
        new_title = TRUE
      )
  }
  e
}

echarts_bar_tooltip <- htmlwidgets::JS("
  function(params){
    if (!params || !params.length) {
      return '';
    }
    for (var i = 0; i < params.length; i++) {
      var candidate = params[i];
      var raw = Array.isArray(candidate.value)
        ? candidate.value[candidate.value.length - 1]
        : candidate.value;
      if (raw !== null && raw !== undefined && !isNaN(raw)) {
        return candidate.name + ': ' + Number(raw).toFixed(1).replace('.', ',') + ' %';
      }
    }
    return '';
  }
")

resolve_echarts_bar_layout <- function(n_bars, axis_rotate = NULL) {
  rotate <- axis_rotate
  if (is.null(rotate) || is.na(rotate)) {
    rotate <- dplyr::case_when(
      n_bars <= 10 ~ 0,
      n_bars <= 18 ~ 35,
      n_bars <= 26 ~ 55,
      TRUE        ~ 65
    )
  }

  # Force showing every category label (even on small screens) instead of letting
  # ECharts auto-skip when many bars are present.
  axis_interval <- 0
  base_width    <- floor(260 / max(1, n_bars))
  bar_width <- dplyr::case_when(
    n_bars <= 8  ~ 28,
    n_bars <= 15 ~ 22,
    n_bars <= 25 ~ 16,
    TRUE         ~ max(8, base_width)
  )

  grid_bottom <- dplyr::case_when(
    rotate >= 70 ~ "20%",
    rotate >= 45 ~ "18%",
    rotate >= 20 ~ "16%",
    TRUE         ~ "12%"
  )

  list(
    bar_width     = bar_width,
    axis_rotate   = rotate,
    axis_interval = axis_interval,
    grid = list(
      top    = 60,
      bottom = grid_bottom,
      left   = if (n_bars > 20) "7%" else "6%",
      right  = if (n_bars > 20) "4%" else "3%"
    )
  )
}

build_echarts_bar_highlight <- function(df,
                                        value_col,
                                        y_axis_name = "%",
                                        title = NULL,
                                        caption = NULL,
                                        axis_rotate = NULL,
                                        avg_line = NULL,
                                        avg_label = NULL,
                                        axis_digits = 0) {
  axis_labels <- as.character(df$geo)
  values <- df[[value_col]]
  layout <- resolve_echarts_bar_layout(length(axis_labels), axis_rotate)
  name_cfg <- axis_name_props(y_axis_name)

  axis_label <- list(
    interval    = layout$axis_interval,
    rotate      = layout$axis_rotate,
    hideOverlap = FALSE,
    margin      = 8
  )

  df <- df |>
    dplyr::mutate(
      val_other = dplyr::if_else(is_hr, NA_real_, values),
      val_hr    = dplyr::if_else(is_hr, values, NA_real_)
    )

  chart <- df |>
    echarts4r::e_charts(geo) |>
    echarts4r::e_bar(
      val_other,
      name      = "Ostale zemlje",
      barWidth  = layout$bar_width,
      barGap    = "-100%",
      barCategoryGap = "0%",
      itemStyle = list(color = "#d9d9d9"),
      emphasis  = list(itemStyle = list(color = "#d9d9d9"))
    ) |>
    echarts4r::e_bar(
      val_hr,
      name      = "Hrvatska",
      barWidth  = layout$bar_width,
      barGap    = "-100%",
      barCategoryGap = "0%",
      itemStyle = list(color = cols_named[["HR"]]),
      emphasis  = list(itemStyle = list(color = cols_named[["HR"]]))
    ) |>
    echarts4r::e_x_axis(
      type = "category",
      data = axis_labels,
      axisLabel = axis_label,
      axisTick = list(alignWithLabel = TRUE)
    ) |>
    echarts4r::e_grid(
      top    = layout$grid$top,
      bottom = layout$grid$bottom,
      left   = layout$grid$left,
      right  = layout$grid$right
    ) |>
    echarts4r::e_y_axis(
      name         = "",
      nameLocation = "middle",
      nameGap      = name_cfg$gap,
      nameRotate   = name_cfg$rotate,
      axisLabel    = list(
        formatter = htmlwidgets::JS(
          sprintf("function(x){return x.toFixed(%d).replace('.', ',');}", axis_digits)
        )
      )
    ) |>
    echarts4r::e_tooltip(
      trigger     = "axis",
      axisPointer = list(type = "shadow"),
      formatter   = echarts_bar_tooltip
    ) |>
    echarts4r::e_legend(show = FALSE) |>
    echarts4r::e_title(
      text = title %||% ""
    ) |>
    add_caption_bottom(caption %||% "")

  if (!is.null(avg_line)) {
    label_cfg <- if (is.null(avg_label)) {
      list(show = FALSE)
    } else {
      list(
        show      = TRUE,
        formatter = avg_label,
        position  = "insideEndTop",
        color     = "#303030",
        padding   = c(0, 0, 4, 0)
      )
    }

    chart <- chart |>
      echarts4r::e_mark_line(
        data      = list(yAxis = avg_line, name = avg_label %||% ""),
        lineStyle = list(
          type  = "dashed",
          color = "#303030"
        ),
        label  = label_cfg,
        symbol = "none"
      )
  }

  chart
}

# pomoćne boje usklađene
col_hr    <- cols_named[["HR"]]
col_other <- "grey80"

default_caption <- function(meta) {
  paste0("Izvor: Eurostat ", dataset_id, 
         ", jedinica ", meta$real_unit,
         " © Leonarda Srdelić")
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

  gdp_real_aggregates <- fetch_gdp(real_unit, unique(aggregate_group_codes)) |>
    filter(year >= first_year)

  last_year <- min(max(gdp_nominal$year, na.rm = TRUE),
                   max(gdp_real$year, na.rm = TRUE))

  gdp_nominal <- gdp_nominal |> filter(year <= last_year)
  gdp_real    <- gdp_real    |> filter(year <= last_year)
  gdp_real_aggregates <- gdp_real_aggregates |> filter(year <= last_year)

  list(
    gdp_nominal = gdp_nominal,
    gdp_real    = gdp_real,
    gdp_real_aggregates = gdp_real_aggregates,
    last_year   = last_year,
    real_unit   = real_unit
  )
}

# panel za realni BDP
build_panel_real <- function(gdp_real, agg_real = NULL) {
  group_means <- imap_dfr(groups, ~ make_group_mean(gdp_real, .x, .y)) |>
    mutate(group = as.character(group))

  if (!is.null(agg_real)) {
    for (grp in names(aggregate_group_codes)) {
      agg_code <- aggregate_group_codes[[grp]]
      series <- agg_real |>
        filter(geo == agg_code) |>
        select(year, value)
      if (nrow(series)) {
        group_means <- group_means |>
          filter(group != grp) |>
          bind_rows(series |> mutate(group = grp))
      }
    }
  }

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

  level_order <- names(cols_named)
  panel_df <- panel_df |>
    dplyr::mutate(
      group_label = factor(as.character(group_label), levels = level_order)
    )

  color_keys <- levels(panel_df$group_label)
  color_keys <- color_keys[!is.na(color_keys)]
  color_vec  <- unname(cols_named[color_keys])

  chart <- panel_df |>
    dplyr::group_by(group_label) |>
    echarts4r::e_charts(year) |>
    echarts4r::e_line(yoy_pct, symbol = "circle", showSymbol = FALSE) |>
    echarts4r::e_color(color_vec) |>
    echarts4r::e_x_axis(
      min  = first_year,
      max  = end_label_year,
      type = "value",
      axisLabel = list(
        formatter = htmlwidgets::JS("function(x){return x + '.';}"),
        rotate    = 90
      ),
      axisPointer = list(
        label = list(
          formatter = htmlwidgets::JS("
            function(params){
              return Math.round(params.value) + '.';
            }
          ")
        )
      ),
      axisTick = list(alignWithLabel = TRUE)
    ) |>
    echarts4r::e_y_axis(
      name         = NULL,
      axisLabel    = list(
        formatter = htmlwidgets::JS(
          "function(x){return x.toFixed(0).replace('.', ',');}"
        )
      ),
      axisPointer = list(
        label = list(
          formatter = htmlwidgets::JS("
            function(params){
              var val = Number(params.value);
              if (isNaN(val)) { return params.value; }
              return val.toFixed(1).replace('.', ',') + ' %';
            }
          ")
        )
      )
    ) |>
    echarts4r::e_mark_line(
      data      = list(yAxis = 0),
      lineStyle = list(color = "grey60", type = "dashed"),
      label     = list(show = FALSE),
      symbol    = "none"
    ) |>
    echarts4r::e_tooltip(
      trigger     = "axis",
      axisPointer = list(
        type  = "cross",
        label = list(
          formatter = htmlwidgets::JS("
            function(params){
              if (params.axisDimension === 'x') {
                return Math.round(params.value) + '.';
              }
              var val = Number(params.value);
              if (isNaN(val)) { return params.value; }
              return val.toFixed(1).replace('.', ',') + ' %';
            }
          ")
        )
      ),
      formatter   = htmlwidgets::JS("
        function(params){
          if (!params || !params.length) { return ''; }
          var year = Math.round(params[0].axisValue);
          var lines = [];
          for (var i = 0; i < params.length; i++) {
            var p = params[i];
            if (p.data == null || isNaN(p.data[1])) { continue; }
            lines.push(p.seriesName + ': ' +
              Number(p.data[1]).toFixed(1).replace('.', ',') + ' %');
          }
          return year + '.<br/>' + lines.join('<br/>');
        }
      ")
    ) |>
    echarts4r::e_legend(bottom = 60, type = "scroll") |>
    echarts4r::e_title(
      text = "%"
    ) |>
    add_caption_bottom(caption) |>
    echarts4r::e_grid(
      top    = 70,
      bottom = 140,
      left   = 40,
      right  = 30
    )

  htmltools::tagList(
    chart,
    htmltools::div(style = "height: 20px;")
  )
}

plot_gdp_index <- function(panel_df, caption = NULL, meta = NULL) {
  caption <- caption %||%
    if (!is.null(meta)) default_caption(meta) else
      paste0("Izvor: Eurostat ", dataset_id)

  note <- paste(
    "Napomena: Zemlje srednje i istočne europe (CEE) uključuju BG, CZ, EE, HU, LT, LV, PL, RO, SI, SK."
  )
  caption <- c(note, caption)

  level_order <- names(cols_named)
  panel_df <- panel_df |>
    dplyr::mutate(
      group_label = factor(as.character(group_label), levels = level_order)
    )

  color_keys <- levels(panel_df$group_label)
  color_keys <- color_keys[!is.na(color_keys)]
  color_vec  <- unname(cols_named[color_keys])

  chart <- panel_df |>
    dplyr::group_by(group_label) |>
    echarts4r::e_charts(year) |>
    echarts4r::e_line(index_2000, symbol = "circle", showSymbol = FALSE) |>
    echarts4r::e_color(color_vec) |>
    echarts4r::e_x_axis(
      min  = first_year,
      max  = end_label_year,
      type = "value",
      axisLabel = list(
        formatter = htmlwidgets::JS("function(x){return x + '.';}"),
        rotate    = 90
      ),
      axisPointer = list(
        label = list(
          formatter = htmlwidgets::JS("
            function(params){
              return Math.round(params.value) + '.';
            }
          ")
        )
      ),
      axisTick = list(alignWithLabel = TRUE)
    ) |>
    echarts4r::e_y_axis(
      name         = NULL,
      axisLabel    = list(
        formatter = htmlwidgets::JS(
          "function(x){return x.toFixed(0).replace('.', ',');}"
        )
      ),
      axisPointer = list(
        label = list(
          formatter = htmlwidgets::JS("
            function(params){
              var val = Number(params.value);
              if (isNaN(val)) { return params.value; }
              return val.toFixed(1).replace('.', ',');
            }
          ")
        )
      )
    ) |>
    echarts4r::e_tooltip(
      trigger     = "axis",
      axisPointer = list(
        type  = "cross",
        label = list(
          formatter = htmlwidgets::JS("
            function(params){
              if (params.axisDimension === 'x') {
                return Math.round(params.value) + '.';
              }
              var val = Number(params.value);
              if (isNaN(val)) { return params.value; }
              return val.toFixed(1).replace('.', ',');
            }
          ")
        )
      ),
      formatter   = htmlwidgets::JS("
        function(params){
          if (!params || !params.length) { return ''; }
          var lines = [];
          var year  = Math.round(params[0].axisValue);
          for (var i = 0; i < params.length; i++) {
            var p = params[i];
            if (p.data == null || isNaN(p.data[1])) { continue; }
            lines.push(p.seriesName + ': ' +
              Number(p.data[1]).toFixed(1).replace('.', ','));
          }
          return year + '.<br/>' + lines.join('<br/>');
        }
      ")
    ) |>
    echarts4r::e_legend(
      bottom = 60,
      type   = "scroll"
    ) |>
    echarts4r::e_title(
      text = "Indeks 2000. = 100"
    ) |>
    add_caption_bottom(caption) |>
    echarts4r::e_grid(
      top    = 70,
      bottom = 140,
      left   = 40,
      right  = 30
    )

  htmltools::tagList(
    chart,
    htmltools::div(style = "height: 20px;")
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
    CEE_exHR  = "CEE",
    EU27_exHR = "EU 27",
    EA_exHR   = "EA",
    .default  = key
  )
}

group_note <- function(key) {
  dplyr::case_when(
    key == "CEE_exHR"  ~ "CEE: BG, CZ, EE, HU, LT, LV, PL, RO, SI, SK (bez HR)",
    key == "EU27_exHR" ~ "EU 27: sve članice EU-a bez HR",
    key == "EA_exHR"   ~ "EA: članice europodručja bez HR",
    TRUE               ~ ""
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

plot_distribution_core_echarts <- function(country_series,
                                           group_series,
                                           hr_series,
                                           yvar,
                                           grp_name,
                                           ylab_txt,
                                           percent  = FALSE,
                                           caption  = NULL,
                                           meta     = NULL) {

  base_caption <- caption %||%
    if (!is.null(meta)) default_caption(meta) else
      paste0("Izvor: Eurostat ", dataset_id)

  key_for_group <- names(groups)[match(grp_name, group_display_name(names(groups)))]
  note <- group_note(key_for_group)
  caption <- if (nzchar(note)) {
    paste("Napomena:", note, "\n", base_caption)
  } else {
    base_caption
  }

  range_df <- country_series |>
    dplyr::group_by(year) |>
    dplyr::summarise(
      y_min = min(.data[[yvar]], na.rm = TRUE),
      y_max = max(.data[[yvar]], na.rm = TRUE),
      .groups = "drop"
    )

  plot_df <- range_df |>
    dplyr::left_join(
      group_series |>
        dplyr::transmute(year, grp_value = .data[[yvar]]),
      by = "year"
    ) |>
    dplyr::left_join(
      hr_series |>
        dplyr::transmute(year, hr_value = .data[[yvar]]),
      by = "year"
    )

  col_group <- cols_named[[grp_name]] %||% "#303030"
  val_fmt   <- if (percent) " + ' %'" else ""
  digits    <- if (percent) 1 else 0

  tooltip_fmt <- htmlwidgets::JS(sprintf("
    function(params){
      if (!params || !params.length) { return ''; }
      var year = params[0].axisValue;
      var rows = [];
      params.forEach(function(p){
        var raw = Array.isArray(p.value) ? p.value[p.value.length - 1] : p.value;
        if (raw === null || raw === undefined || isNaN(raw)) { return; }
        var txt = Number(raw).toFixed(%d).replace('.', ',')%s;
        rows.push(p.seriesName + ': ' + txt);
      });
      return year + '.<br/>' + rows.join('<br/>');
    }
  ", digits, val_fmt))

  axis_fmt <- if (percent) {
    htmlwidgets::JS("function(x){return x.toFixed(0).replace('.', ',');}")
  } else {
    htmlwidgets::JS("function(x){return x.toFixed(0).replace('.', ',');}")
  }

  caption <- base_caption

  chart <- plot_df |>
    echarts4r::e_charts(year) |>
    echarts4r::e_band2(
      y_min,
      y_max,
      name      = "Raspon zemalja",
      color     = "rgba(160,160,160,0.25)",
      itemStyle = list(borderWidth = 0)
    ) |>
    echarts4r::e_line(
      grp_value,
      name       = grp_name,
      symbol     = "circle",
      showSymbol = FALSE,
      lineStyle  = list(width = 2),
      itemStyle  = list(color = col_group),
      emphasis   = list(itemStyle = list(color = col_group))
    ) |>
    echarts4r::e_line(
      hr_value,
      name       = "HR",
      symbol     = "circle",
      showSymbol = FALSE,
      lineStyle  = list(width = 2),
      itemStyle  = list(color = cols_named[["HR"]]),
      emphasis   = list(itemStyle = list(color = cols_named[["HR"]]))
    ) |>
    echarts4r::e_tooltip(
      trigger     = "axis",
      axisPointer = list(
        type  = "cross",
        label = list(
          formatter = htmlwidgets::JS(sprintf("
            function(params){
              var v = params.value;
              if (params.axisDimension === 'x') {
                return Math.round(v) + '.';
              }
              var num = Number(v);
              if (isNaN(num)) { return params.value; }
              return num.toFixed(%d).replace('.', ',')%s;
            }
          ", digits, val_fmt))
        )
      ),
      formatter   = tooltip_fmt
    ) |>
    echarts4r::e_x_axis(
      min  = first_year,
      max  = end_label_year,
      type = "value",
      axisLabel = list(
        formatter = htmlwidgets::JS("function(x){return x + '.';}"),
        rotate    = 90
      ),
      axisTick = list(alignWithLabel = TRUE)
    ) |>
    echarts4r::e_y_axis(
      name         = NULL,
      axisLabel    = list(
        formatter = axis_fmt
      )
    ) |>
    echarts4r::e_legend(bottom = 60) |>
    echarts4r::e_title(text = ylab_txt) |>
    add_caption_bottom(caption, base_bottom = -20) |>
    echarts4r::e_grid(
      top    = 70,
      bottom = 130,
      left   = 40,
      right  = 30
    )

  htmltools::tagList(
    chart,
    htmltools::div(
      style = "font-size: 0.85em; color: #555; text-align: left; margin-top: 4px; margin-bottom: 18px;",
      caption
    )
  )
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

  range_df <- country_series |>
    dplyr::group_by(year) |>
    dplyr::summarise(
      ymin = min(.data[[yvar]], na.rm = TRUE),
      ymax = max(.data[[yvar]], na.rm = TRUE),
      .groups = "drop"
    )

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
    ylab_txt       = "Indeks 2000. = 100",
    percent        = FALSE,
    caption        = caption,
    meta           = meta
  )
}

plot_gdp_index_distribution_echarts <- function(gdp_real,
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

  plot_distribution_core_echarts(
    country_series = country_idx,
    group_series   = grp_idx,
    hr_series      = hr_idx,
    yvar           = "index_2000",
    grp_name       = grp_name,
    ylab_txt       = "Indeks 2000. = 100",
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

plot_gdp_yoy_distribution_echarts <- function(gdp_real,
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

  plot_distribution_core_echarts(
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
    " © Leonarda Srdelić"
  )

  hr_q |>
    echarts4r::e_charts(yq) |>
    echarts4r::e_bar(
      qoq,
      name      = "Hrvatska",
      barWidth  = 14,
      itemStyle = list(color = col_hr),
      emphasis  = list(itemStyle = list(color = col_hr))
    ) |>
    echarts4r::e_x_axis(
      type = "category",
      data = as.character(hr_q$yq),
      axisLabel = list(
        interval = 0,
        rotate   = 90
      ),
      axisTick = list(alignWithLabel = TRUE)
    ) |>
    echarts4r::e_y_axis(
      name         = "",
      nameLocation = "middle",
      nameGap      = 30,
      nameRotate   = 0,
      axisLabel    = list(
        formatter = htmlwidgets::JS(
          "function(x){return x.toFixed(0).replace('.', ',');}"
        )
      )
    ) |>
    echarts4r::e_mark_line(
      data      = list(yAxis = 0),
      lineStyle = list(color = "grey70"),
      label     = list(show = FALSE),
      symbol    = "none"
    ) |>
    echarts4r::e_grid(
      top    = 60,
      bottom = 50,
      left   = 40,
      right  = 30
    ) |>
    # Make the layout responsive for narrow screens via JS hook (keeps desktop the same).
    htmlwidgets::onRender(
      "
      function(el, x) {
        var chart = echarts.getInstanceByDom(el);
        if (!chart) return;
        function tweak() {
          if (!chart) return;
          var w = el.clientWidth || 0;
          var opt = chart.getOption();
          if (w && w < 800) {
            (opt.series || []).forEach(function(s) { s.barWidth = 9; });
            if (opt.xAxis && opt.xAxis.length) {
              opt.xAxis[0].axisLabel = opt.xAxis[0].axisLabel || {};
              opt.xAxis[0].axisLabel.rotate = 60;
              opt.xAxis[0].axisLabel.fontSize = 9;
              opt.xAxis[0].axisLabel.interval = 0;
            }
            if (opt.grid && opt.grid.length) {
              opt.grid[0].bottom = '18%';
            }
            chart.setOption(opt, true);
          }
        }
        tweak();
        chart.on('resize', tweak);
      }
      "
    ) |>
    echarts4r::e_tooltip(
      trigger     = "axis",
      axisPointer = list(type = "shadow"),
      formatter   = echarts_bar_tooltip
    ) |>
    echarts4r::e_legend(show = FALSE) |>
    add_caption_bottom(caption)
}

# ======================================================
# Realni BDP, razina (zadnje tromjesečje), mlrd. EUR
# ======================================================

plot_gdp_real_q_level_eu27_latest_echarts <- function() {
  use_cache()

  qdat <- eurostat::get_eurostat(
    id = "namq_10_gdp",
    filters = list(
      na_item = "B1GQ",
      s_adj   = "SCA",
      unit    = "CLV20_MEUR",
      geo     = c(eu27_codes, "EU27_2020")
    ),
    time_format = "raw"
  ) |>
    dplyr::rename(time_raw = time, value = values) |>
    dplyr::mutate(
      date  = lubridate::yq(gsub("Q", "-Q", time_raw)),
      value = value / 1000 # mlrd. EUR
    )

  if (nrow(qdat) == 0L) {
    stop("Nema podataka za namq_10_gdp, B1GQ, SCA, CLV20_MEUR.")
  }

  last_date <- max(qdat$date, na.rm = TRUE)

  plot_df <- qdat |>
    dplyr::filter(
      date == last_date,
      geo != "EU27_2020"
    ) |>
    dplyr::mutate(
      geo_label = as.character(geo),
      is_hr     = geo_label == hr_code
    ) |>
    dplyr::arrange(dplyr::desc(value))

  if (nrow(plot_df) == 0L) {
    stop("Za posljednje tromjesečje nema podataka po zemljama.")
  }

  axis_labels <- plot_df$geo_label
  layout      <- resolve_echarts_bar_layout(length(axis_labels))

  plot_df <- plot_df |>
    dplyr::mutate(
      val_oth = dplyr::if_else(is_hr, NA_real_, value),
      val_hr  = dplyr::if_else(is_hr, value, NA_real_)
    )

  title_txt <- paste0(lubridate::year(last_date), "Q", lubridate::quarter(last_date))

  caption_txt <- list(
    "Izvor: Eurostat (namq_10_gdp, B1GQ, s_adj = SCA, CLV20_MEUR)"
  )

  plot_df |>
    echarts4r::e_charts(geo_label) |>
    echarts4r::e_bar(
      val_oth,
      name      = "Ostale zemlje",
      barWidth  = layout$bar_width,
      barGap    = "-100%",
      barCategoryGap = "0%",
      itemStyle = list(color = "#d9d9d9"),
      emphasis  = list(itemStyle = list(color = "#d9d9d9"))
    ) |>
    echarts4r::e_bar(
      val_hr,
      name      = "Hrvatska",
      barWidth  = layout$bar_width,
      barGap    = "-100%",
      barCategoryGap = "0%",
      itemStyle = list(color = cols_named[["HR"]]),
      emphasis  = list(itemStyle = list(color = cols_named[["HR"]]))
    ) |>
    echarts4r::e_x_axis(
      type = "category",
      data = axis_labels,
      axisLabel = list(
        interval    = layout$axis_interval,
        rotate      = layout$axis_rotate,
        hideOverlap = FALSE
      ),
      axisTick = list(alignWithLabel = TRUE)
    ) |>
    echarts4r::e_grid(
      top    = layout$grid$top,
      bottom = "12%",
      left   = layout$grid$left,
      right  = layout$grid$right
    ) |>
    echarts4r::e_y_axis(
      name         = "",
      nameLocation = "middle",
      nameGap      = 35,
      nameRotate   = 90,
      axisLabel    = list(
        formatter = htmlwidgets::JS(
          "function(x){return x.toFixed(0).replace('.', ',');}"
        )
      )
    ) |>
    echarts4r::e_tooltip(
      trigger     = "axis",
      axisPointer = list(type = "shadow"),
      formatter   = htmlwidgets::JS("
        function(params){
          if (!params || !params.length) { return ''; }
          var point = null;
          for (var i = 0; i < params.length; i++) {
            var cand = params[i];
            var raw = Array.isArray(cand.value)
              ? cand.value[cand.value.length - 1]
              : cand.value;
            if (raw !== null && raw !== undefined && !isNaN(raw)) {
              point = { name: cand.name, value: raw };
              break;
            }
          }
          if (!point) { return ''; }
          return point.name + ': ' +
            Number(point.value).toFixed(1).replace('.', ',') + ' mlrd. EUR';
        }
      ")
    ) |>
    echarts4r::e_title(text = title_txt) |>
    echarts4r::e_legend(show = FALSE) |>
    add_caption_bottom(caption_txt)
}

plot_gdp_real_q_level_eu27_tminus1_echarts <- function() {
  use_cache()

  qdat <- eurostat::get_eurostat(
    id = "namq_10_gdp",
    filters = list(
      na_item = "B1GQ",
      s_adj   = "SCA",
      unit    = "CLV20_MEUR",
      geo     = c(eu27_codes, "EU27_2020")
    ),
    time_format = "raw"
  ) |>
    dplyr::rename(time_raw = time, value = values) |>
    dplyr::mutate(
      date  = lubridate::yq(gsub("Q", "-Q", time_raw)),
      value = value / 1000 # mlrd. EUR
    )

  if (nrow(qdat) == 0L) {
    stop("Nema podataka za namq_10_gdp, B1GQ, SCA, CLV20_MEUR.")
  }

  last_date_all <- max(qdat$date, na.rm = TRUE)
  t_minus1      <- last_date_all %m-% months(3)

  plot_df <- qdat |>
    dplyr::filter(
      date == t_minus1,
      geo != "EU27_2020"
    ) |>
    dplyr::mutate(
      geo_label = as.character(geo),
      is_hr     = geo_label == hr_code
    ) |>
    dplyr::arrange(dplyr::desc(value))

  if (nrow(plot_df) == 0L) {
    stop("Za t-1 nema podataka po zemljama.")
  }

  axis_labels <- plot_df$geo_label
  layout      <- resolve_echarts_bar_layout(length(axis_labels))

  plot_df <- plot_df |>
    dplyr::mutate(
      val_oth = dplyr::if_else(is_hr, NA_real_, value),
      val_hr  = dplyr::if_else(is_hr, value, NA_real_)
    )

  title_txt <- paste0(lubridate::year(t_minus1), "Q", lubridate::quarter(t_minus1))

  caption_txt <- list(
    "Izvor: Eurostat namq_10_gdp, B1GQ, s_adj = SCA, CLV20_MEUR"
  )

  plot_df |>
    echarts4r::e_charts(geo_label) |>
    echarts4r::e_bar(
      val_oth,
      name      = "Ostale zemlje",
      barWidth  = layout$bar_width,
      barGap    = "-100%",
      barCategoryGap = "0%",
      itemStyle = list(color = "#d9d9d9"),
      emphasis  = list(itemStyle = list(color = "#d9d9d9"))
    ) |>
    echarts4r::e_bar(
      val_hr,
      name      = "Hrvatska",
      barWidth  = layout$bar_width,
      barGap    = "-100%",
      barCategoryGap = "0%",
      itemStyle = list(color = cols_named[["HR"]]),
      emphasis  = list(itemStyle = list(color = cols_named[["HR"]]))
    ) |>
    echarts4r::e_x_axis(
      type = "category",
      data = axis_labels,
      axisLabel = list(
        interval    = layout$axis_interval,
        rotate      = layout$axis_rotate,
        hideOverlap = FALSE
      ),
      axisTick = list(alignWithLabel = TRUE)
    ) |>
    echarts4r::e_grid(
      top    = layout$grid$top,
      bottom = "12%",
      left   = layout$grid$left,
      right  = layout$grid$right
    ) |>
    echarts4r::e_y_axis(
      name         = "",
      nameLocation = "middle",
      nameGap      = 35,
      nameRotate   = 90,
      axisLabel    = list(
        formatter = htmlwidgets::JS(
          "function(x){return x.toFixed(0).replace('.', ',');}"
        )
      )
    ) |>
    echarts4r::e_tooltip(
      trigger     = "axis",
      axisPointer = list(type = "shadow"),
      formatter   = htmlwidgets::JS("
        function(params){
          if (!params || !params.length) { return ''; }
          var point = null;
          for (var i = 0; i < params.length; i++) {
            var cand = params[i];
            var raw = Array.isArray(cand.value)
              ? cand.value[cand.value.length - 1]
              : cand.value;
            if (raw !== null && raw !== undefined && !isNaN(raw)) {
              point = { name: cand.name, value: raw };
              break;
            }
          }
          if (!point) { return ''; }
          return point.name + ': ' +
            Number(point.value).toFixed(1).replace('.', ',') + ' mlrd. EUR';
        }
      ")
    ) |>
    echarts4r::e_title(text = title_txt) |>
    echarts4r::e_legend(show = FALSE) |>
    add_caption_bottom(caption_txt)
}

# ======================================================
# BDP po stanovniku u PPS: CEE zemlje, zadnja godina
# ======================================================

plot_gdp_pc_cee_pps_last <- function(start_year = 2010) {
  hr_code   <- "HR"
  eu_code   <- "EU27_2020"
  cee_codes <- c("BG","CZ","EE","HU","LT","LV","PL","RO","SI","SK","HR")
  units_pps <- "CP_PPS_EU27_2020_HAB"

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
      geo   = factor(geo, levels = geo),
      is_hr = geo == hr_code
    )

  caption_txt <- paste0(
    "Izvor: Eurostat nama_10_pc, B1GQ, unit = ", units_pps,
    ", EU27_2020 = 100 © Leonarda Srdelić"
  )

  build_echarts_bar_highlight(
    df          = last_df,
    value_col   = "ratio",
    y_axis_name = "EU27 = 100",
    title       = paste0("CEE zemlje, ", last_year),
    caption     = caption_txt,
    axis_rotate = 0
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
    ", B1GQ, CP_MEUR, © Leonarda Srdelić"
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
    ", B1GQ, CP_MEUR",
    " © Leonarda Srdelić"
  )

  build_echarts_bar_highlight(
    df          = plot_df,
    value_col   = "stopa",
    y_axis_name = "%",
    title       = paste0(fmt_year(year_t), " / ", fmt_year(year_tm1)),
    caption     = caption_txt,
    avg_line    = eu_avg,
    avg_label   = paste0(
      "EU-27: ",
      scales::number(eu_avg, accuracy = 0.1, decimal.mark = ","),
      " %"
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
    " © Leonarda Srdelić"
  )

  build_echarts_bar_highlight(
    df          = plot_df,
    value_col   = "stopa",
    y_axis_name = "%",
    title       = paste0(fmt_year(year_t), " / ", fmt_year(year_tm1)),
    caption     = caption_txt,
    axis_rotate = 0,
    avg_line    = eu_avg,
    avg_label   = paste0(
      "EU-27: ",
      scales::number(eu_avg, accuracy = 0.1, decimal.mark = ","),
      " %"
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
    "Napomena: Prikazane su samo zemlje s dostupnim podatkom za najnovije tromjesečje.",
    "\n\nIzvor: Eurostat namq_10_gdp, B1GQ, CP_MEUR, s_adj = ",
    s_used,
    format(last_date, "%Y Q%q"),
    " © Leonarda Srdelić"
  )

  build_echarts_bar_highlight(
    df          = plot_df,
    value_col   = "yoy",
    y_axis_name = "%",
    title       = paste0("Realni BDP yoy (", format(last_date, "%Y Q%q"), ")"),
    caption     = caption_txt,
    axis_rotate = 90,
    avg_line    = eu_avg,
    avg_label   = paste0(
      "EU-27: ",
      scales::number(eu_avg, accuracy = 0.1, decimal.mark = ","),
      " %"
    )
  )
}

plot_gdp_nominal_q_yoy_eu27_tminus1 <- function() {
  plot_gdp_nominal_q_yoy_eu27_latest()
}

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
      geo     = c(eu27_codes, "EU27_2020")
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
    dplyr::filter(
      date == t_minus1,
      !is.na(yoy),
      geo != "EU27_2020"
    ) |>
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

  # Službeni EU-27 agregat (EU27_2020) za t-1
  eu_avg <- qdat |>
    dplyr::filter(
      date == t_minus1,
      geo == "EU27_2020"
    ) |>
    dplyr::pull(yoy)

  year_q <- lubridate::year(t_minus1)
  q_q    <- lubridate::quarter(t_minus1)

  prev_date <- t_minus1 %m-% months(12)
  prev_year <- lubridate::year(prev_date)
  prev_q    <- lubridate::quarter(prev_date)

caption_txt <- paste0(
  "Napomena: EU-27 je službeni ponderirani agregat Eurostata.",
  "\n\nIzvor: Eurostat namq_10_gdp, B1GQ, s_adj = SCA, jedinica CLV20_MEUR",
  " © Leonarda Srdelić"
)

  title_txt <- paste0(
    year_q, "Q", q_q, " / ",
    prev_year, "Q", prev_q
  )

  build_echarts_bar_highlight(
    df          = plot_df,
    value_col   = "yoy",
    y_axis_name = "%",
    title       = title_txt,
    caption     = caption_txt,
    avg_line    = eu_avg,
    avg_label   = paste0(
      "EU-27: ",
      scales::number(eu_avg, accuracy = 0.1, decimal.mark = ","),
      " %"
    )
  )
}

plot_gdp_real_q_yoy_eu27_latest <- function() {
  use_cache()

  qdat <- eurostat::get_eurostat(
    id = "namq_10_gdp",
    filters = list(
      na_item = "B1GQ",
      s_adj   = "SCA",
      unit    = "CLV20_MEUR",
      geo     = c(eu27_codes, "EU27_2020")
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

  latest_date <- max(qdat$date, na.rm = TRUE)

  plot_df <- qdat |>
    dplyr::filter(
      date == latest_date,
      !is.na(yoy),
      geo != "EU27_2020"
    ) |>
    dplyr::arrange(dplyr::desc(yoy)) |>
    dplyr::mutate(
      geo   = factor(geo, levels = geo),
      is_hr = geo == hr_code
    )

  if (nrow(plot_df) == 0L) {
    stop("Nema zemalja s dostupnim yoy stopama za posljednji kvartal.")
  }

  eu_avg <- qdat |>
    dplyr::filter(
      date == latest_date,
      geo == "EU27_2020"
    ) |>
    dplyr::pull(yoy)

  build_echarts_bar_highlight(
    df          = plot_df,
    value_col   = "yoy",
    y_axis_name = "%",
    title       = paste0(
      "Realni BDP yoy (", format(latest_date, "%Y Q%q"), ", posljednji dostupni)"
    ),
    caption = "Napomena: Prikazane su samo zemlje s objavljenim podatkom za najnovije tromjesečje.",
    axis_rotate = 90,
    avg_line    = eu_avg,
    avg_label   = paste0(
      "EU-27: ",
      scales::number(eu_avg, accuracy = 0.1, decimal.mark = ","),
      " %"
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
      geo     = c(eu27_codes, "EU27_2020")
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
    dplyr::filter(
      date == t_minus1,
      !is.na(qoq),
      geo != "EU27_2020"
    ) |>
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

  # Službeni EU-27 agregat (EU27_2020) za t-1
  eu_avg <- qdat |>
    dplyr::filter(
      date == t_minus1,
      geo == "EU27_2020"
    ) |>
    dplyr::pull(qoq)

  year_q <- lubridate::year(t_minus1)
  q_q    <- lubridate::quarter(t_minus1)

  prev_date <- t_minus1 %m-% months(3)
  prev_year <- lubridate::year(prev_date)
  prev_q    <- lubridate::quarter(prev_date)

  caption_txt <- paste0(
  "Napomena: EU-27 je službeni ponderirani agregat Eurostata.",
  "\n\nIzvor: Eurostat namq_10_gdp, B1GQ, s_adj = SCA, jedinica CLV20_MEUR ",
  " © Leonarda Srdelić"
)

  title_txt <- paste0(
    year_q, "Q", q_q,
    " / ",
    prev_year, "Q", prev_q
  )

    ggplot2::ggplot(
      plot_df,
      ggplot2::aes(
        x    = geo,
        y    = qoq,
        fill = is_hr,
        text = paste0(
          geo, ": ",
          scales::number(qoq, accuracy = 0.1, decimal.mark = ","),
          " %"
        )
      )
    ) +
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
        "EU-27: ",
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
      title   = title_txt,
      x       = NULL,
      y       = "%",
      caption = caption_txt
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(
      angle = 90,
      hjust = 0.5,
      vjust = 0.5
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

plot_gdp_real_q_qoq_eu27_tminus1_plotly <- function() {
  # pretpostavka: plot_gdp_real_q_qoq_eu27_tminus1 već postoji
  p <- plot_gdp_real_q_qoq_eu27_tminus1()

  plotly::ggplotly(
    p,
    tooltip = "text"  # koristi aes(text = ...) iz ggplota
  )
}

# Helpers for QoQ ECharts
get_qoq_dataset <- function() {
  use_cache()

  qdat <- eurostat::get_eurostat(
    id = "namq_10_gdp",
    filters = list(
      na_item = "B1GQ",
      s_adj   = "SCA",
      unit    = "CLV20_MEUR",
      geo     = c(eu27_codes, "EU27_2020")
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

  latest_date <- max(qdat$date, na.rm = TRUE)

  coverage <- qdat |>
    dplyr::filter(!is.na(qoq), geo %in% eu27_codes) |>
    dplyr::count(date, name = "n_countries")

  complete_date <- coverage |>
    dplyr::filter(n_countries == length(eu27_codes)) |>
    dplyr::summarise(max_date = max(date)) |>
    dplyr::pull(max_date)

  list(
    data          = qdat,
    latest_date   = latest_date,
    complete_date = complete_date
  )
}

build_qoq_chart <- function(qdat, target_date, title_text, caption_note, caption_source) {
  plot_df <- qdat |>
    dplyr::filter(
      date == target_date,
      !is.na(qoq),
      geo != "EU27_2020"
    ) |>
    dplyr::arrange(dplyr::desc(qoq)) |>
    dplyr::mutate(
      geo_label = as.character(geo),
      is_hr     = geo_label == hr_code
    )

  if (nrow(plot_df) == 0L) {
    return(NULL)
  }

  axis_labels <- plot_df$geo_label
  layout <- resolve_echarts_bar_layout(length(axis_labels))
  layout <- resolve_echarts_bar_layout(length(axis_labels))

  plot_df <- plot_df |>
    dplyr::mutate(
      qoq_oth = dplyr::if_else(is_hr, NA_real_, qoq),
      qoq_hr  = dplyr::if_else(is_hr, qoq, NA_real_)
    )

  eu_avg <- qdat |>
    dplyr::filter(
      date == target_date,
      geo == "EU27_2020"
    ) |>
    dplyr::pull(qoq)

  eu_label <- paste0(
    "EU-27: ",
    gsub("\\.", ",", sprintf("%.1f", eu_avg))
  )

  plot_df |>
    echarts4r::e_charts(geo_label) |>
    echarts4r::e_bar(
      qoq_oth,
      name      = "Ostale zemlje",
      barWidth  = layout$bar_width,
      barGap    = "-100%",
      barCategoryGap = "0%",
      itemStyle = list(color = "#d9d9d9"),
      emphasis  = list(itemStyle = list(color = "#d9d9d9"))
    ) |>
    echarts4r::e_bar(
      qoq_hr,
      name      = "Hrvatska",
      barWidth  = layout$bar_width,
      barGap    = "-100%",
      barCategoryGap = "0%",
      itemStyle = list(color = cols_named[["HR"]]),
      emphasis  = list(itemStyle = list(color = cols_named[["HR"]]))
    ) |>
    echarts4r::e_x_axis(
      type = "category",
      data = axis_labels,
      axisLabel = list(
        interval    = layout$axis_interval,
        rotate      = layout$axis_rotate,
        hideOverlap = FALSE
      ),
      axisTick = list(alignWithLabel = TRUE)
    ) |>
    echarts4r::e_grid(
      top    = layout$grid$top,
      bottom = layout$grid$bottom,
      left   = layout$grid$left,
      right  = layout$grid$right
    ) |>
    echarts4r::e_mark_line(
      data      = list(yAxis = eu_avg, name = eu_label),
      lineStyle = list(
        type  = "dashed",
        color = "#303030"
      ),
      label = list(
        show      = TRUE,
        formatter = paste0(eu_label, " %"),
        position  = "insideEndTop",
        color     = "#303030",
        padding   = c(0, 0, 4, 0)
      ),
      symbol = "none"
    ) |>
    echarts4r::e_title(
      text = title_text
    ) |>
    echarts4r::e_title(
      text      = caption_note,
      bottom    = 16,
      left      = "left",
      textStyle = list(
        fontSize   = 11,
        fontWeight = "normal",
        color      = "#555555"
      ),
      new_title = TRUE
    ) |>
    echarts4r::e_title(
      text      = caption_source,
      bottom    = -4,
      left      = "left",
      textStyle = list(
        fontSize   = 11,
        fontWeight = "normal",
        color      = "#555555"
      ),
      new_title = TRUE
    ) |>
    echarts4r::e_y_axis(
      name          = "",
      nameLocation  = "middle",
      nameGap       = 30,
      nameRotate    = 0,
      nameTextStyle = list(
        align   = "center",
        color   = "#555555",
        padding = c(0, 0, 0, -10)
      ),
      axisLabel = list(
        formatter = htmlwidgets::JS(
          "function(x){return x.toFixed(1).replace('.', ',');}"
        )
      )
    ) |>
    echarts4r::e_tooltip(
      trigger     = "axis",
      axisPointer = list(type = "shadow"),
      formatter   = htmlwidgets::JS("
        function(params){
          if (!params || !params.length) {
            return '';
          }
          var point = null;
          for (var i = 0; i < params.length; i++) {
            var candidate = params[i];
            var raw       = candidate.value;
            var val       = Array.isArray(raw) ? raw[raw.length - 1] : raw;
            if (val !== null && val !== undefined && !isNaN(val)) {
              point = { name: candidate.name, value: raw };
              break;
            }
          }
          if (!point) {
            return '';
          }
          var val = Number(point.value).toFixed(1).replace('.', ',');
          return point.name + ': ' + val + ' %';
        }
      ")
    ) |>
    echarts4r::e_legend(show = FALSE)
}

plot_gdp_real_q_qoq_eu27_tminus1_echarts <- function() {
  use_cache()

  qdat <- eurostat::get_eurostat(
    id = "namq_10_gdp",
    filters = list(
      na_item = "B1GQ",
      s_adj   = "SCA",
      unit    = "CLV20_MEUR",
      geo     = c(eu27_codes, "EU27_2020")
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
    dplyr::filter(
      date == t_minus1,
      !is.na(qoq),
      geo != "EU27_2020"
    ) |>
    dplyr::arrange(dplyr::desc(qoq)) |>
    dplyr::mutate(
      geo_label = as.character(geo),
      is_hr     = geo_label == hr_code
    )

  if (nrow(plot_df) == 0L) {
    stop("Za t minus 1 nema qoq stopa ni za jednu zemlju.")
  }

  # službeni EU-27 agregat za t-1
  eu_avg <- qdat |>
    dplyr::filter(
      date == t_minus1,
      geo == "EU27_2020"
    ) |>
    dplyr::pull(qoq)

  eu_label <- paste0(
    "EU-27: ",
    gsub("\\.", ",", sprintf("%.1f", eu_avg))
  )

  year_q <- lubridate::year(t_minus1)
  q_q    <- lubridate::quarter(t_minus1)

  prev_date <- t_minus1 %m-% months(3)
  prev_year <- lubridate::year(prev_date)
  prev_q    <- lubridate::quarter(prev_date)

  caption_txt <- list(
    note   = "Napomena: EU-27 je službeni ponderirani agregat Eurostata.",
    source = "Izvor: Eurostat namq_10_gdp, B1GQ, s_adj = SCA, CLV20_MEUR"
  )

  title_txt <- paste0(
    year_q, "Q", q_q,
    " / ",
    prev_year, "Q", prev_q
  )

  axis_labels <- plot_df$geo_label
  layout <- resolve_echarts_bar_layout(length(axis_labels))

  plot_df <- plot_df |>
    dplyr::mutate(
      qoq_oth = dplyr::if_else(is_hr, NA_real_, qoq),
      qoq_hr  = dplyr::if_else(is_hr, qoq, NA_real_)
    )

    plot_df |>
    echarts4r::e_charts(geo_label) |>
    echarts4r::e_bar(
      qoq_oth,
      name      = "Ostale zemlje",
      barWidth  = layout$bar_width,
      barGap    = "-100%",
      barCategoryGap = "0%",
      itemStyle = list(color = "#d9d9d9"),
      emphasis  = list(itemStyle = list(color = "#d9d9d9"))
    ) |>
    echarts4r::e_bar(
      qoq_hr,
      name      = "Hrvatska",
      barWidth  = layout$bar_width,
      barGap    = "-100%",
      barCategoryGap = "0%",
      itemStyle = list(color = cols_named[["HR"]]),
      emphasis  = list(itemStyle = list(color = cols_named[["HR"]]))
    ) |>
    echarts4r::e_x_axis(
      type = "category",
      data = axis_labels,
      axisLabel = list(
        interval    = layout$axis_interval,
        rotate      = layout$axis_rotate,
        hideOverlap = FALSE
      ),
      axisTick = list(alignWithLabel = TRUE)
    ) |>
    echarts4r::e_grid(
      top    = layout$grid$top,
      bottom = layout$grid$bottom,
      left   = layout$grid$left,
      right  = layout$grid$right
    ) |>
    echarts4r::e_mark_line(
      data      = list(yAxis = eu_avg, name = eu_label),
      lineStyle = list(
        type  = "dashed",
        color = "#303030"
      ),
      label = list(
        show      = TRUE,
        formatter = paste0(eu_label, " %"),
        position  = "insideEndTop",
        color     = "#303030",
        padding   = c(0, 0, 4, 0)
      ),
      symbol = "none"
    ) |>
    echarts4r::e_title(
      text = title_txt
    ) |>
    echarts4r::e_title(
      text      = caption_txt$note,
      bottom    = 16,
      left      = "left",
      textStyle = list(
        fontSize   = 11,
        fontWeight = "normal",
        color      = "#555555"
      ),
      new_title = TRUE
    ) |>
    echarts4r::e_title(
      text      = caption_txt$source,
      bottom    = -4,
      left      = "left",
      textStyle = list(
        fontSize   = 11,
        fontWeight = "normal",
        color      = "#555555"
      ),
      new_title = TRUE
    ) |>
    echarts4r::e_y_axis(
      name          = "",
      nameLocation  = "middle",
      nameGap       = 30,
      nameRotate    = 0,
      nameTextStyle = list(
        align   = "center",
        color   = "#555555",
        padding = c(0, 0, 0, -10)
      ),
      axisLabel = list(
        formatter = htmlwidgets::JS(
          "function(x){return x.toFixed(1).replace('.', ',');}"
        )
      )
    ) |>
    echarts4r::e_tooltip(
      trigger     = "axis",
      axisPointer = list(type = "shadow"),
      formatter   = htmlwidgets::JS("
        function(params){
          if (!params || !params.length) {
            return '';
          }
          var point = null;
          for (var i = 0; i < params.length; i++) {
            var candidate = params[i];
            var raw       = Array.isArray(candidate.value)
              ? candidate.value[candidate.value.length - 1]
              : candidate.value;
            if (raw !== null && raw !== undefined && !isNaN(raw)) {
              point = { name: candidate.name, value: raw };
              break;
            }
          }
          if (!point) {
            return '';
          }
          var val = Number(point.value).toFixed(1).replace('.', ',');
          return point.name + ': ' + val + ' %';
        }
      ")
    ) |>
    echarts4r::e_legend(show = FALSE)
}

plot_gdp_real_q_qoq_eu27_latest_echarts <- function() {
  use_cache()

  qdat <- eurostat::get_eurostat(
    id = "namq_10_gdp",
    filters = list(
      na_item = "B1GQ",
      s_adj   = "SCA",
      unit    = "CLV20_MEUR",
      geo     = c(eu27_codes, "EU27_2020")
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

  last_date_all <- max(qdat$date, na.rm = TRUE)

  plot_df <- qdat |>
    dplyr::filter(
      date == last_date_all,
      !is.na(qoq),
      geo != "EU27_2020"
    ) |>
    dplyr::arrange(dplyr::desc(qoq)) |>
    dplyr::mutate(
      geo_label = as.character(geo),
      is_hr     = geo_label == hr_code
    )

  if (nrow(plot_df) == 0L) {
    stop("Za t nema qoq stopa ni za jednu zemlju.")
  }

  eu_avg <- qdat |>
    dplyr::filter(
      date == last_date_all,
      geo == "EU27_2020"
    ) |>
    dplyr::pull(qoq)

  eu_label <- paste0(
    "EU-27: ",
    gsub("\\.", ",", sprintf("%.1f", eu_avg))
  )

  caption_txt <- list(
    note   = "Napomena: Prikazane su samo zemlje s dostupnim podatkom za najnovije tromjesečje.",
    source = "Izvor: Eurostat namq_10_gdp, B1GQ, s_adj = SCA, CLV20_MEUR"
  )

  title_txt <- paste0(
    lubridate::year(last_date_all), "Q", lubridate::quarter(last_date_all),
    " (posljednje dostupni podaci)"
  )

  axis_labels <- plot_df$geo_label
  layout <- resolve_echarts_bar_layout(length(axis_labels))

  plot_df <- plot_df |>
    dplyr::mutate(
      qoq_oth = dplyr::if_else(is_hr, NA_real_, qoq),
      qoq_hr  = dplyr::if_else(is_hr, qoq, NA_real_)
    )

    plot_df |>
    echarts4r::e_charts(geo_label) |>
    echarts4r::e_bar(
      qoq_oth,
      name      = "Ostale zemlje",
      barWidth  = layout$bar_width,
      barGap    = "-100%",
      barCategoryGap = "0%",
      itemStyle = list(color = "#d9d9d9"),
      emphasis  = list(itemStyle = list(color = "#d9d9d9"))
    ) |>
    echarts4r::e_bar(
      qoq_hr,
      name      = "Hrvatska",
      barWidth  = layout$bar_width,
      barGap    = "-100%",
      barCategoryGap = "0%",
      itemStyle = list(color = cols_named[["HR"]]),
      emphasis  = list(itemStyle = list(color = cols_named[["HR"]]))
    ) |>
    echarts4r::e_x_axis(
      type = "category",
      data = axis_labels,
      axisLabel = list(
        interval    = layout$axis_interval,
        rotate      = layout$axis_rotate,
        hideOverlap = FALSE
      ),
      axisTick = list(alignWithLabel = TRUE)
    ) |>
    echarts4r::e_grid(
      top    = layout$grid$top,
      bottom = layout$grid$bottom,
      left   = layout$grid$left,
      right  = layout$grid$right
    ) |>
    echarts4r::e_mark_line(
      data      = list(yAxis = eu_avg, name = eu_label),
      lineStyle = list(
        type  = "dashed",
        color = "#303030"
      ),
      label = list(
        show      = TRUE,
        formatter = paste0(eu_label, " %"),
        position  = "insideEndTop",
        color     = "#303030",
        padding   = c(0, 0, 4, 0)
      ),
      symbol = "none"
    ) |>
    echarts4r::e_title(
      text = title_txt
    ) |>
    echarts4r::e_title(
      text      = caption_txt$note,
      bottom    = 16,
      left      = "left",
      textStyle = list(
        fontSize   = 11,
        fontWeight = "normal",
        color      = "#555555"
      ),
      new_title = TRUE
    ) |>
    echarts4r::e_title(
      text      = caption_txt$source,
      bottom    = -4,
      left      = "left",
      textStyle = list(
        fontSize   = 11,
        fontWeight = "normal",
        color      = "#555555"
      ),
      new_title = TRUE
    ) |>
    echarts4r::e_y_axis(
      name          = "",
      nameLocation  = "middle",
      nameGap       = 30,
      nameRotate    = 0,
      nameTextStyle = list(
        align   = "center",
        color   = "#555555",
        padding = c(0, 0, 0, -10)
      ),
      axisLabel = list(
        formatter = htmlwidgets::JS(
          "function(x){return x.toFixed(1).replace('.', ',');}"
        )
      )
    ) |>
    echarts4r::e_tooltip(
      trigger     = "axis",
      axisPointer = list(type = "shadow"),
      formatter   = htmlwidgets::JS("
        function(params){
          if (!params || !params.length) {
            return '';
          }
          var point = null;
          for (var i = 0; i < params.length; i++) {
            var candidate = params[i];
            var raw       = Array.isArray(candidate.value)
              ? candidate.value[candidate.value.length - 1]
              : candidate.value;
            if (raw !== null && raw !== undefined && !isNaN(raw)) {
              point = { name: candidate.name, value: raw };
              break;
            }
          }
          if (!point) {
            return '';
          }
          var num = Number(point.value);
          if (isNaN(num)) {
            return '';
          }
          return point.name + ': ' + num.toFixed(1).replace('.', ',') + ' %';
        }
      ")
    ) |>
    echarts4r::e_legend(show = FALSE)
}

plot_gdp_real_q_qoq_eu27_t_echarts <- function() {
  use_cache()

  qdat <- eurostat::get_eurostat(
    id = "namq_10_gdp",
    filters = list(
      na_item = "B1GQ",
      s_adj   = "SCA",
      unit    = "CLV20_MEUR",
      geo     = c(eu27_codes, "EU27_2020")
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

  last_date_all <- max(qdat$date, na.rm = TRUE)

  plot_df <- qdat |>
    dplyr::filter(
      date == last_date_all,
      !is.na(qoq),
      geo != "EU27_2020"
    ) |>
    dplyr::arrange(dplyr::desc(qoq)) |>
    dplyr::mutate(
      geo_label = as.character(geo),
      is_hr     = geo_label == hr_code
    )

  if (nrow(plot_df) == 0L) {
    stop("Za t nema qoq stopa ni za jednu zemlju.")
  }

  eu_avg <- qdat |>
    dplyr::filter(
      date == last_date_all,
      geo == "EU27_2020"
    ) |>
    dplyr::pull(qoq)

  eu_label <- paste0(
    "EU-27: ",
    gsub("\\.", ",", sprintf("%.1f", eu_avg))
  )

  caption_txt <- list(
    note   = "Napomena: EU-27 je službeni ponderirani agregat Eurostata.",
    source = "Izvor: Eurostat namq_10_gdp, B1GQ, s_adj = SCA, CLV20_MEUR"
  )

  title_txt <- paste0(
    lubridate::year(last_date_all), "Q", lubridate::quarter(last_date_all),
    " (posljednji dostupni)"
  )

  axis_labels <- plot_df$geo_label

  plot_df <- plot_df |>
    dplyr::mutate(
      qoq_oth = dplyr::if_else(is_hr, NA_real_, qoq),
      qoq_hr  = dplyr::if_else(is_hr, qoq, NA_real_)
    )

    plot_df |>
    echarts4r::e_charts(geo_label) |>
    echarts4r::e_bar(
      qoq_oth,
      name      = "Ostale zemlje",
      barWidth  = layout$bar_width,
      barGap    = "-100%",
      barCategoryGap = "0%",
      itemStyle = list(color = "#d9d9d9"),
      emphasis  = list(itemStyle = list(color = "#d9d9d9"))
    ) |>
    echarts4r::e_bar(
      qoq_hr,
      name      = "Hrvatska",
      barWidth  = layout$bar_width,
      barGap    = "-100%",
      barCategoryGap = "0%",
      itemStyle = list(color = cols_named[["HR"]]),
      emphasis  = list(itemStyle = list(color = cols_named[["HR"]]))
    ) |>
    echarts4r::e_x_axis(
      type = "category",
      data = axis_labels,
      axisLabel = list(
        interval    = layout$axis_interval,
        rotate      = layout$axis_rotate,
        hideOverlap = FALSE
      ),
      axisTick = list(alignWithLabel = TRUE)
    ) |>
    echarts4r::e_grid(
      top    = layout$grid$top,
      bottom = layout$grid$bottom,
      left   = layout$grid$left,
      right  = layout$grid$right
    ) |>
    echarts4r::e_mark_line(
      data      = list(yAxis = eu_avg, name = eu_label),
      lineStyle = list(
        type  = "dashed",
        color = "#303030"
      ),
      label = list(
        show      = TRUE,
        formatter = paste0(eu_label, " %"),
        position  = "insideEndTop",
        color     = "#303030",
        padding   = c(0, 0, 4, 0)
      ),
      symbol = "none"
    ) |>
    echarts4r::e_title(
      text = title_txt
    ) |>
    echarts4r::e_title(
      text      = caption_txt$note,
      bottom    = 16,
      left      = "left",
      textStyle = list(
        fontSize   = 11,
        fontWeight = "normal",
        color      = "#555555"
      ),
      new_title = TRUE
    ) |>
    echarts4r::e_title(
      text      = caption_txt$source,
      bottom    = -4,
      left      = "left",
      textStyle = list(
        fontSize   = 11,
        fontWeight = "normal",
        color      = "#555555"
      ),
      new_title = TRUE
    ) |>
    echarts4r::e_y_axis(
      name          = "",
      nameLocation  = "middle",
      nameGap       = 30,
      nameRotate    = 0,
      nameTextStyle = list(
        align   = "center",
        color   = "#555555",
        padding = c(0, 0, 0, -10)
      ),
      axisLabel = list(
        formatter = htmlwidgets::JS(
          "function(x){return x.toFixed(1).replace('.', ',');}"
        )
      )
    ) |>
    echarts4r::e_tooltip(
      trigger     = "axis",
      axisPointer = list(type = "shadow"),
      formatter   = htmlwidgets::JS("
        function(params){
          if (!params || !params.length) {
            return '';
          }
          var point = null;
          for (var i = 0; i < params.length; i++) {
            var candidate = params[i];
            var raw       = Array.isArray(candidate.value)
              ? candidate.value[candidate.value.length - 1]
              : candidate.value;
            if (raw !== null && raw !== undefined && !isNaN(raw)) {
              point = { name: candidate.name, value: raw };
              break;
            }
          }
          if (!point) {
            return '';
          }
          var val = Number(point.value).toFixed(1).replace('.', ',');
          return point.name + ': ' + val + ' %';
        }
      ")
    ) |>
    echarts4r::e_legend(show = FALSE)
}
plot_gdp_real_q_qoq_eu27_t_echarts <- function() {
   plot_gdp_real_q_qoq_eu27_tminus1_echarts()
}
