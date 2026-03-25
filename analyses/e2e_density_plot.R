ensure_required_packages <- function(packages, repos = "https://cloud.r-project.org") {
  minor_major <- strsplit(R.version$minor, "\\.")[[1]][1]
  version_short <- paste(R.version$major, minor_major, sep = ".")
  local_app_data <- Sys.getenv("LOCALAPPDATA")
  if (!nzchar(local_app_data)) {
    local_app_data <- path.expand("~")
  }
  user_lib <- file.path(local_app_data, "R", "win-library", version_short)
  if (!dir.exists(user_lib)) {
    dir.create(user_lib, recursive = TRUE, showWarnings = FALSE)
  }
  if (!(user_lib %in% .libPaths())) {
    .libPaths(c(user_lib, .libPaths()))
  }

  installed <- rownames(installed.packages())
  missing <- setdiff(packages, installed)

  if (length(missing) > 0) {
    cat(sprintf(
      "Missing R packages: %s\nAutomatic installation in progress...\n",
      paste(missing, collapse = ", ")
    ))

    tryCatch(
      {
        install.packages(missing, repos = repos, lib = user_lib)
      },
      error = function(e) {
        stop(sprintf(
          "Automatic installation failed (%s). Please manually install the missing packages and try again.",
          conditionMessage(e)
        ))
      }
    )
  }

  still_missing <- setdiff(packages, rownames(installed.packages()))
  if (length(still_missing) > 0) {
    stop(sprintf(
      "Packages not available after installation: %s",
      paste(still_missing, collapse = ", ")
    ))
  }
}

ensure_required_packages(c("DBI", "RSQLite", "ggplot2", "gridExtra"))

suppressPackageStartupMessages({
  library(DBI)
  library(RSQLite)
  library(ggplot2)
  library(gridExtra)
  library(grid)
})

`%||%` <- function(a, b) {
  if (is.null(a)) b else a
}

CATEGORY_ORDER <- c("No-change", "Initial", "Improving", "Worsening")
CATEGORY_COLORS <- c(
  "No-change" = "#F8766D",
  "Initial" = "#7CAE00",
  "Improving" = "#00BFC4",
  "Worsening" = "#C77CFF"
)
SECONDS_PER_DAY <- 86400

get_script_dir <- function() {
  file_arg <- commandArgs(trailingOnly = FALSE)
  file_match <- grep("^--file=", file_arg, value = TRUE)
  if (length(file_match) > 0) {
    script_path <- sub("^--file=", "", file_match[[1]])
    return(normalizePath(dirname(script_path), winslash = "/", mustWork = FALSE))
  }
  normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}

SCRIPT_DIR <- get_script_dir()
PROJECT_ROOT <- normalizePath(file.path(SCRIPT_DIR, ".."), winslash = "/", mustWork = FALSE)

resolve_input_path <- function(path_value) {
  if (grepl("^[A-Za-z]:[/\\\\]", path_value) || startsWith(path_value, "/")) {
    return(normalizePath(path_value, winslash = "/", mustWork = FALSE))
  }

  candidates <- c(
    file.path(PROJECT_ROOT, path_value),
    file.path(SCRIPT_DIR, path_value),
    file.path(getwd(), path_value)
  )

  for (candidate in candidates) {
    if (file.exists(candidate)) {
      return(normalizePath(candidate, winslash = "/", mustWork = FALSE))
    }
  }

  normalizePath(file.path(PROJECT_ROOT, path_value), winslash = "/", mustWork = FALSE)
}

parse_iso_datetime <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  x[x == ""] <- NA_character_
  x[tolower(x) %in% c("na", "nan", "null", "none")] <- NA_character_

  # Normalize timezone
  x <- sub("Z$", "+0000", x)
  x <- sub("([+-][0-9]{2}):([0-9]{2})$", "\\1\\2", x)

  formats <- c(
    "%Y-%m-%dT%H:%M:%OS%z",
    "%Y-%m-%d %H:%M:%OS%z",
    "%Y-%m-%dT%H:%M:%OS",
    "%Y-%m-%d %H:%M:%OS",
    "%Y-%m-%d"
  )

  parse_one <- function(value) {
    if (is.na(value)) {
      return(as.POSIXct(NA, tz = "UTC"))
    }

    for (fmt in formats) {
      parsed <- suppressWarnings(as.POSIXct(value, tz = "UTC", format = fmt))
      if (!is.na(parsed)) {
        return(parsed)
      }
    }

    as.POSIXct(NA, tz = "UTC")
  }

  as.POSIXct(vapply(x, parse_one, FUN.VALUE = as.POSIXct(NA, tz = "UTC")), origin = "1970-01-01", tz = "UTC")
}

read_report_commits <- function(db_path) {
  db_resolved <- resolve_input_path(db_path)
  if (!file.exists(db_resolved)) {
    stop(sprintf("Database not found: %s", db_resolved))
  }

  con <- dbConnect(RSQLite::SQLite(), db_resolved)
  on.exit(dbDisconnect(con), add = TRUE)

  query <- "
    SELECT
      dataset,
      repository,
      file_name,
      commit_hash,
      date,
      nearest_future_release_date,
      nearest_previous_release_date,
      smells_count
    FROM report_commit_details
  "

  frame <- dbGetQuery(con, query)
  if (nrow(frame) == 0) {
    return(frame)
  }

  frame$date <- parse_iso_datetime(frame$date)
  frame$nearest_future_release_date <- parse_iso_datetime(frame$nearest_future_release_date)
  frame$nearest_previous_release_date <- parse_iso_datetime(frame$nearest_previous_release_date)
  frame$smells_count <- as.integer(ifelse(is.na(frame$smells_count), 0, frame$smells_count))
  frame <- frame[!is.na(frame$date), ]
  frame
}

closest_release_signed_days <- function(commit_date, prev_date, future_date) {
  candidates <- numeric(0)

  if (!is.na(prev_date)) {
    candidates <- c(candidates, as.numeric(difftime(commit_date, prev_date, units = "days")))
  }
  if (!is.na(future_date)) {
    candidates <- c(candidates, as.numeric(difftime(commit_date, future_date, units = "days")))
  }

  if (length(candidates) == 0) {
    return(NA_real_)
  }

  candidates[which.min(abs(candidates))]
}

classify_variation <- function(df) {
  if (nrow(df) == 0) return(df)

  # Sort by date and commit_hash (as in Python)
  o <- order(df$date, df$commit_hash)
  ordered <- df[o, ]

  # Shift smells_count down by 1, fill 0 (like fillna(0) in Python)
  prev <- c(0L, head(ordered$smells_count, -1))
  delta <- ordered$smells_count - prev

  variation <- ifelse(
    delta < 0,
    "Improving",
    ifelse(delta > 0, ifelse(prev == 0, "Initial", "Worsening"), "No-change")
  )

  ordered$variation_type <- variation
  ordered
}

build_plot_data <- function(frame) {
  if (nrow(frame) == 0) return(frame)

  split_key <- paste(frame$dataset, frame$repository, frame$file_name, sep = "||")
  groups <- split(frame, split_key)
  classified_list <- lapply(groups, classify_variation)
  classified <- do.call(rbind, classified_list)

  classified$signed_days <- mapply(
    closest_release_signed_days,
    classified$date,
    classified$nearest_previous_release_date,
    classified$nearest_future_release_date
  )

  classified <- classified[!is.na(classified$signed_days), ]
  classified$variation_type <- factor(classified$variation_type, levels = CATEGORY_ORDER)
  classified
}

plot_ridge_manual <- function(frame, output_png, figure_width, figure_height, bw_adjust, dpi) {
  if (nrow(frame) == 0) {
    stop("No valid data found to generate the plot.")
  }

  x_values <- frame$signed_days
  x_min <- floor(min(x_values, na.rm = TRUE) / 10) * 10 - 10
  x_max <- ceiling(max(x_values, na.rm = TRUE) / 10) * 10 + 10
  x_grid <- seq(x_min, x_max, length.out = 500)

  density_data <- data.frame()
  max_height <- 0.9

  for (i in seq_along(CATEGORY_ORDER)) {
    category <- CATEGORY_ORDER[i]
    baseline <- i - 1
    vals <- frame$signed_days[frame$variation_type == category]

    if (length(vals) == 0) {
      next
    }

    if (length(vals) == 1) {
      bandwidth <- 5.0 * max(bw_adjust, 0.1)
      y <- dnorm((x_grid - vals[[1]]) / bandwidth) / bandwidth
      dens <- list(x = x_grid, y = y)
    } else {
      dens <- density(vals, bw = "nrd0", adjust = max(bw_adjust, 0.1), n = 500, from = x_min, to = x_max)
    }
    y_scaled <- if (max(dens$y) > 0) (dens$y / max(dens$y)) * max_height else rep(0, length(dens$y))

    density_data <- rbind(
      density_data,
      data.frame(
        x = dens$x,
        y = baseline + y_scaled,
        baseline = baseline,
        variation_type = category,
        stringsAsFactors = FALSE
      )
    )
  }

  rug_data <- frame[, c("signed_days", "variation_type")]
  rug_data$ymin <- -0.18
  rug_data$ymax <- -0.04

  baselines_df <- data.frame(
    variation_type = CATEGORY_ORDER,
    baseline = seq_along(CATEGORY_ORDER) - 1
  )

  p <- ggplot() +
    geom_hline(data = baselines_df, aes(yintercept = baseline), color = "black", linewidth = 0.35) +
    geom_ribbon(
      data = density_data,
      aes(x = x, ymin = baseline, ymax = y, fill = variation_type),
      alpha = 0.6
    ) +
    geom_line(data = density_data, aes(x = x, y = y, group = variation_type), color = "black", linewidth = 0.55) +
    geom_segment(
      data = rug_data,
      aes(x = signed_days, xend = signed_days, y = ymin, yend = ymax, color = variation_type),
      linewidth = 0.5,
      alpha = 0.95
    ) +
    geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) +
    scale_fill_manual(values = CATEGORY_COLORS, breaks = CATEGORY_ORDER, drop = FALSE) +
    scale_color_manual(values = CATEGORY_COLORS, breaks = CATEGORY_ORDER, drop = FALSE) +
    scale_y_continuous(
      breaks = seq_along(CATEGORY_ORDER) - 1,
      labels = CATEGORY_ORDER,
      limits = c(-0.25, (length(CATEGORY_ORDER) - 1) + max_height + 0.05)
    ) +
    scale_x_continuous(limits = c(x_min, x_max)) +
    labs(
      x = "Days relative to closest release (negative = before, positive = after)",
      y = "Density"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.major = element_line(color = "#ffffff", linewidth = 0.5),
      panel.grid.minor = element_line(color = "#ffffff", linewidth = 0.25),
      panel.background = element_rect(fill = "#EBEBEB", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      legend.position = "none"
    )

  output_dir <- dirname(output_png)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  ggsave(output_png, p, width = figure_width, height = figure_height, dpi = dpi)
}

print_counts <- function(plot_data, label) {
  counts <- table(factor(plot_data$variation_type, levels = CATEGORY_ORDER))
  cat(sprintf("Ridge plot created (%s): %s\n", label, if (label == "JS") output_js else output_ts))
  cat(sprintf("Observations by variation type (%s):\n", label))
  for (name in CATEGORY_ORDER) {
    cat(sprintf("- %s: %d\n", name, as.integer(counts[[name]])))
  }
}

build_release_proximity_summary <- function(plot_data) {
  if (nrow(plot_data) == 0) {
    stop("No data available to build release-cycle proximity summary.")
  }

  col_labels <- c("No-change", "Introduc.", "Improv.", "Worsen.")
  thresholds <- c(7, 14, 30)

  values_by_category <- lapply(CATEGORY_ORDER, function(category) {
    abs(plot_data$signed_days[plot_data$variation_type == category])
  })
  names(values_by_category) <- col_labels

  format_int <- function(x) {
    if (is.na(x)) "NA" else sprintf("%d", as.integer(round(x)))
  }

  format_decimal <- function(x, digits = 1) {
    if (is.na(x)) "NA" else sprintf(paste0("%.", digits, "f"), x)
  }

  num_commits <- vapply(values_by_category, length, integer(1))

  within_rows <- lapply(thresholds, function(k) {
    vapply(values_by_category, function(v) {
      if (length(v) == 0) {
        return(NA_real_)
      }
      mean(v <= k) * 100
    }, numeric(1))
  })

  medians <- vapply(values_by_category, function(v) {
    if (length(v) == 0) {
      return(NA_real_)
    }
    as.numeric(median(v))
  }, numeric(1))

  iqrs <- vapply(values_by_category, function(v) {
    if (length(v) == 0) {
      return(NA_real_)
    }
    as.numeric(IQR(v))
  }, numeric(1))

  summary_table <- data.frame(
    Metric = c(
      "Num. of commits",
      "Within +/-7 days (%)",
      "Within +/-14 days (%)",
      "Within +/-30 days (%)",
      "Median |d|",
      "IQR |d|"
    ),
    "No-change" = c(
      format_int(num_commits[["No-change"]]),
      format_decimal(within_rows[[1]][["No-change"]], 1),
      format_decimal(within_rows[[2]][["No-change"]], 1),
      format_decimal(within_rows[[3]][["No-change"]], 1),
      format_int(medians[["No-change"]]),
      format_decimal(iqrs[["No-change"]], 1)
    ),
    "Introduc." = c(
      format_int(num_commits[["Introduc."]]),
      format_decimal(within_rows[[1]][["Introduc."]], 1),
      format_decimal(within_rows[[2]][["Introduc."]], 1),
      format_decimal(within_rows[[3]][["Introduc."]], 1),
      format_int(medians[["Introduc."]]),
      format_decimal(iqrs[["Introduc."]], 1)
    ),
    "Improv." = c(
      format_int(num_commits[["Improv."]]),
      format_decimal(within_rows[[1]][["Improv."]], 1),
      format_decimal(within_rows[[2]][["Improv."]], 1),
      format_decimal(within_rows[[3]][["Improv."]], 1),
      format_int(medians[["Improv."]]),
      format_decimal(iqrs[["Improv."]], 1)
    ),
    "Worsen." = c(
      format_int(num_commits[["Worsen."]]),
      format_decimal(within_rows[[1]][["Worsen."]], 1),
      format_decimal(within_rows[[2]][["Worsen."]], 1),
      format_decimal(within_rows[[3]][["Worsen."]], 1),
      format_int(medians[["Worsen."]]),
      format_decimal(iqrs[["Worsen."]], 1)
    ),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  summary_table
}

save_release_proximity_table_png <- function(summary_table, output_png, dataset_label) {
  output_dir <- dirname(output_png)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  title_text <- sprintf(
    "Release-cycle proximity summary by variation type (%s)",
    dataset_label
  )

  table_theme <- ttheme_minimal(
    base_size = 12,
    colhead = list(fg_params = list(fontface = "bold", cex = 1.1)),
    core = list(fg_params = list(cex = 1.0), padding = unit(c(5, 5), "mm"))
  )

  table_grob <- tableGrob(summary_table, rows = NULL, theme = table_theme)
  title_grob <- textGrob(
    title_text,
    x = 0,
    hjust = 0,
    gp = gpar(fontsize = 14, fontface = "bold")
  )

  combined <- arrangeGrob(
    title_grob,
    table_grob,
    ncol = 1,
    heights = unit.c(unit(1.2, "cm"), unit(1, "npc") - unit(1.2, "cm"))
  )

  png(output_png, width = 1600, height = 900, res = 160)
  grid.newpage()
  grid.draw(combined)
  dev.off()
}

save_release_proximity_summary <- function(plot_data, dataset_label, project_root) {
  summary_table <- build_release_proximity_summary(plot_data)

  csv_output <- normalizePath(
    file.path(project_root, "analyses", "reports", sprintf("release_cycle_proximity_summary_%s_R.csv", tolower(dataset_label))),
    winslash = "/",
    mustWork = FALSE
  )
  png_output <- normalizePath(
    file.path(project_root, "analyses", "plots", sprintf("release_cycle_proximity_summary_%s_R.png", tolower(dataset_label))),
    winslash = "/",
    mustWork = FALSE
  )

  csv_dir <- dirname(csv_output)
  if (!dir.exists(csv_dir)) {
    dir.create(csv_dir, recursive = TRUE)
  }

  write.csv(summary_table, csv_output, row.names = FALSE)
  save_release_proximity_table_png(summary_table, png_output, dataset_label)

  cat(sprintf("Release-cycle summary table saved (%s): %s\n", dataset_label, csv_output))
  cat(sprintf("Release-cycle summary image saved (%s): %s\n", dataset_label, png_output))

  # Remove the CSV file after creation
  if (file.exists(csv_output)) {
    file.remove(csv_output)
    cat(sprintf("Release-cycle summary CSV deleted (%s): %s\n", dataset_label, csv_output))
  }
}

args <- commandArgs(trailingOnly = TRUE)

js_db <- "historical_smellsJS.db"
ts_db <- "historical_smellsTS.db"
figure_width <- 11.5
figure_height <- 7.0
bw_adjust <- 1.0
dpi <- 220

if (length(args) > 0) {
  i <- 1
  while (i <= length(args)) {
    if (args[[i]] == "--js-db" && i + 1 <= length(args)) {
      js_db <- args[[i + 1]]
      i <- i + 2
    } else if (args[[i]] == "--ts-db" && i + 1 <= length(args)) {
      ts_db <- args[[i + 1]]
      i <- i + 2
    } else if (args[[i]] == "--figure-width" && i + 1 <= length(args)) {
      figure_width <- as.numeric(args[[i + 1]])
      i <- i + 2
    } else if (args[[i]] == "--figure-height" && i + 1 <= length(args)) {
      figure_height <- as.numeric(args[[i + 1]])
      i <- i + 2
    } else if (args[[i]] == "--bw-adjust" && i + 1 <= length(args)) {
      bw_adjust <- as.numeric(args[[i + 1]])
      i <- i + 2
    } else if (args[[i]] == "--dpi" && i + 1 <= length(args)) {
      dpi <- as.integer(args[[i + 1]])
      i <- i + 2
    } else {
      i <- i + 1
    }
  }
}

js_df <- read_report_commits(js_db)
ts_df <- read_report_commits(ts_db)

plot_data_js <- build_plot_data(js_df)
output_js <- normalizePath(file.path(PROJECT_ROOT, "analyses", "plots", "release_distance_ridge_js_R.png"), winslash = "/", mustWork = FALSE)
plot_ridge_manual(plot_data_js, output_js, figure_width, figure_height, bw_adjust, dpi)
print_counts(plot_data_js, "JS")
save_release_proximity_summary(plot_data_js, "JS", PROJECT_ROOT)

plot_data_ts <- build_plot_data(ts_df)
output_ts <- normalizePath(file.path(PROJECT_ROOT, "analyses", "plots", "release_distance_ridge_ts_R.png"), winslash = "/", mustWork = FALSE)
plot_ridge_manual(plot_data_ts, output_ts, figure_width, figure_height, bw_adjust, dpi)
print_counts(plot_data_ts, "TS")
save_release_proximity_summary(plot_data_ts, "TS", PROJECT_ROOT)
