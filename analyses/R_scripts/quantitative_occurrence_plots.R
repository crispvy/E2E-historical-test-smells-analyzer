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
    install.packages(missing, repos = repos, lib = user_lib)
  }

  still_missing <- setdiff(packages, rownames(installed.packages()))
  if (length(still_missing) > 0) {
    stop(sprintf(
      "Packages not available after installation: %s",
      paste(still_missing, collapse = ", ")
    ))
  }
}

ensure_required_packages(c("ggplot2", "dplyr", "readr", "scales", "forcats", "tidyr"))

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(readr)
  library(scales)
  library(forcats)
  library(tidyr)
})

LANG_COLORS <- c(
  "JavaScript" = "#E69F00",
  "TypeScript" = "#0072B2"
)

FRAMEWORK_COLORS <- c(
  "cypress" = "#2A9D8F",
  "playwright" = "#1D3557",
  "puppeteer" = "#F4A261",
  "selenium" = "#8D99AE"
)

get_script_dir <- function() {
  file_arg <- commandArgs(trailingOnly = FALSE)
  file_match <- grep("^--file=", file_arg, value = TRUE)
  if (length(file_match) > 0) {
    script_path <- sub("^--file=", "", file_match[[1]])
    return(normalizePath(dirname(script_path), winslash = "/", mustWork = FALSE))
  }
  normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}

resolve_input_path <- function(path_value, script_dir, project_root) {
  if (grepl("^[A-Za-z]:[/\\\\]", path_value) || startsWith(path_value, "/")) {
    return(normalizePath(path_value, winslash = "/", mustWork = FALSE))
  }

  candidates <- c(
    file.path(project_root, path_value),
    file.path(script_dir, path_value),
    file.path(getwd(), path_value)
  )

  for (candidate in candidates) {
    if (file.exists(candidate)) {
      return(normalizePath(candidate, winslash = "/", mustWork = TRUE))
    }
  }

  stop(sprintf("File not found: %s", path_value))
}

safe_filename <- function(value) {
  lower <- tolower(trimws(value))
  gsub("[^a-z0-9]+", "_", lower)
}

save_plot <- function(plot_obj, output_path, width = 14, height = 8, dpi = 320) {
  ggsave(
    filename = output_path,
    plot = plot_obj,
    width = width,
    height = height,
    dpi = dpi,
    bg = "white"
  )
  cat(sprintf("Saved: %s\n", output_path))
}

plot_occurrences_by_smell <- function(lang_df, language_label) {
  top_df <- lang_df %>%
    arrange(desc(occurrences_total)) %>%
    mutate(smell_type = fct_reorder(smell_type, occurrences_total))

  ggplot(top_df, aes(x = smell_type, y = occurrences_total)) +
    geom_col(fill = LANG_COLORS[[language_label]], alpha = 0.9) +
    geom_text(
      aes(label = comma(occurrences_total)),
      hjust = -0.08,
      size = 3.8,
      color = "#2B2D42"
    ) +
    coord_flip() +
    scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.15))) +
    labs(
      title = paste0(language_label, " - Occurrences by Smell Type"),
      subtitle = "Total smell occurrences across all analyzed tests",
      x = NULL,
      y = "Occurrences"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      axis.text.y = element_text(face = "bold")
    )
}

plot_distinct_tests_by_smell <- function(lang_df, language_label) {
  top_df <- lang_df %>%
    arrange(desc(distinct_tests_total)) %>%
    mutate(smell_type = fct_reorder(smell_type, distinct_tests_total))

  ggplot(top_df, aes(x = smell_type, y = distinct_tests_total)) +
    geom_col(fill = alpha(LANG_COLORS[[language_label]], 0.8)) +
    geom_text(
      aes(label = comma(distinct_tests_total)),
      hjust = -0.08,
      size = 3.8,
      color = "#2B2D42"
    ) +
    coord_flip() +
    scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.15))) +
    labs(
      title = paste0(language_label, " - Distinct Tests by Smell Type"),
      subtitle = "How many unique test files show each smell",
      x = NULL,
      y = "Distinct tests"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      axis.text.y = element_text(face = "bold")
    )
}

plot_framework_totals <- function(framework_df, language_label) {
  totals <- framework_df %>%
    group_by(framework) %>%
    summarise(
      total_occurrences = sum(occurrences, na.rm = TRUE),
      total_distinct_tests = sum(distinct_tests, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(total_occurrences)) %>%
    mutate(
      framework = fct_reorder(framework, total_occurrences),
      framework_color = FRAMEWORK_COLORS[as.character(framework)]
    )

  ggplot(totals, aes(x = framework, y = total_occurrences, fill = framework)) +
    geom_col(alpha = 0.95) +
    geom_text(
      aes(label = comma(total_occurrences)),
      vjust = -0.5,
      size = 4,
      color = "#2B2D42"
    ) +
    scale_fill_manual(values = FRAMEWORK_COLORS, guide = "none") +
    scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.12))) +
    labs(
      title = paste0(language_label, " - Total Occurrences by Framework"),
      subtitle = "Overall smell incidence volume per framework",
      x = NULL,
      y = "Occurrences"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(face = "bold")
    )
}

plot_framework_smell_heatmap <- function(framework_df, language_label, value_col, value_label) {
  aggregated <- framework_df %>%
    group_by(framework, smell_type) %>%
    summarise(metric = sum(.data[[value_col]], na.rm = TRUE), .groups = "drop") %>%
    mutate(smell_type = fct_reorder(smell_type, metric, .fun = sum, .desc = TRUE))

  max_metric <- suppressWarnings(max(aggregated$metric, na.rm = TRUE))
  if (!is.finite(max_metric) || max_metric <= 0) {
    aggregated <- aggregated %>% mutate(label_color = "#1F2937")
  } else {
    aggregated <- aggregated %>%
      mutate(
        label_color = ifelse(metric / max_metric >= 0.55, "#FFFFFF", "#1F2937")
      )
  }

  ggplot(aggregated, aes(x = framework, y = smell_type, fill = metric)) +
    geom_tile(color = "white", linewidth = 0.2) +
    geom_text(
      aes(label = ifelse(metric > 0, comma(metric), ""), color = label_color),
      size = 3,
      show.legend = FALSE
    ) +
    scale_color_identity() +
    scale_fill_gradient(low = "#E9F5DB", high = "#1B4332", labels = comma) +
    labs(
      title = paste0(language_label, " - Framework vs Smell Heatmap (", value_label, ")"),
      subtitle = paste0("Intensity of ", tolower(value_label), " by framework and smell type"),
      x = NULL,
      y = NULL,
      fill = value_label
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold"),
      panel.grid = element_blank(),
      axis.text.x = element_text(face = "bold"),
      axis.text.y = element_text(size = 9)
    )
}

run <- function() {
  script_dir <- get_script_dir()
  project_root <- normalizePath(file.path(script_dir, "..", ".."), winslash = "/", mustWork = FALSE)

  language_csv <- resolve_input_path(
    "analyses/reports/bad_smells_incidence_language_R.csv",
    script_dir,
    project_root
  )
  framework_csv <- resolve_input_path(
    "analyses/reports/bad_smells_incidence_framework_R.csv",
    script_dir,
    project_root
  )

  output_root <- file.path(project_root, "analyses", "plots", "quantitative_occurrence")
  dir.create(output_root, recursive = TRUE, showWarnings = FALSE)

  cat("[status] Loading selected data sources...\n")
  language_df <- read_csv(language_csv, show_col_types = FALSE)
  framework_df <- read_csv(framework_csv, show_col_types = FALSE)

  language_long <- language_df %>%
    select(
      smell_type,
      occurrences_javascript,
      distinct_tests_javascript,
      occurrences_typescript,
      distinct_tests_typescript
    ) %>%
    pivot_longer(
      cols = -smell_type,
      names_to = c("metric", "language"),
      names_pattern = "(occurrences|distinct_tests)_(javascript|typescript)",
      values_to = "value"
    ) %>%
    mutate(
      language = ifelse(language == "javascript", "JavaScript", "TypeScript")
    ) %>%
    pivot_wider(names_from = metric, values_from = value) %>%
    rename(
      occurrences_total = occurrences,
      distinct_tests_total = distinct_tests
    )

  languages <- c("JavaScript", "TypeScript")

  for (lang in languages) {
    cat(sprintf("[status] Generating plots for %s...\n", lang))

    lang_folder <- file.path(output_root, safe_filename(lang))
    dir.create(lang_folder, recursive = TRUE, showWarnings = FALSE)

    lang_data <- language_long %>% filter(language == lang)
    lang_framework_data <- framework_df %>% filter(language == lang)

    p1 <- plot_occurrences_by_smell(lang_data, lang)
    p2 <- plot_distinct_tests_by_smell(lang_data, lang)
    p3 <- plot_framework_totals(lang_framework_data, lang)
    p4 <- plot_framework_smell_heatmap(lang_framework_data, lang, "occurrences", "Occurrences")
    p5 <- plot_framework_smell_heatmap(lang_framework_data, lang, "distinct_tests", "Distinct tests")

    save_plot(p1, file.path(lang_folder, paste0("1_occurrences_by_smell_", safe_filename(lang), ".png")))
    save_plot(p2, file.path(lang_folder, paste0("2_distinct_tests_by_smell_", safe_filename(lang), ".png")))
    save_plot(p3, file.path(lang_folder, paste0("3_framework_total_occurrences_", safe_filename(lang), ".png")))
    save_plot(p4, file.path(lang_folder, paste0("4_framework_smell_heatmap_occurrences_", safe_filename(lang), ".png")), width = 16, height = 9)
    save_plot(p5, file.path(lang_folder, paste0("5_framework_smell_heatmap_distinct_tests_", safe_filename(lang), ".png")), width = 16, height = 9)
  }

  cat("[status] All quantitative occurrence plots generated successfully.\n")
  cat(sprintf("[status] Output folder: %s\n", output_root))
}

run()
