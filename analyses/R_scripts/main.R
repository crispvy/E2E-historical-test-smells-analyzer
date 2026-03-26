parse_args <- function(args) {
  out <- list(
    parallel = FALSE,
    workers = 2L,
    js_db = NULL,
    ts_db = NULL,
    only = NULL
  )

  i <- 1
  while (i <= length(args)) {
    if (args[[i]] == "--parallel") {
      out$parallel <- TRUE
      i <- i + 1
    } else if (args[[i]] == "--workers" && i + 1 <= length(args)) {
      out$workers <- as.integer(args[[i + 1]])
      i <- i + 2
    } else if (args[[i]] == "--js-db" && i + 1 <= length(args)) {
      out$js_db <- args[[i + 1]]
      i <- i + 2
    } else if (args[[i]] == "--ts-db" && i + 1 <= length(args)) {
      out$ts_db <- args[[i + 1]]
      i <- i + 2
    } else if (args[[i]] == "--only" && i + 1 <= length(args)) {
      out$only <- args[[i + 1]]
      i <- i + 2
    } else {
      i <- i + 1
    }
  }

  if (is.na(out$workers) || out$workers < 1L) {
    out$workers <- 1L
  }

  out
}

get_script_dir <- function() {
  file_arg <- commandArgs(trailingOnly = FALSE)
  file_match <- grep("^--file=", file_arg, value = TRUE)
  if (length(file_match) > 0) {
    script_path <- sub("^--file=", "", file_match[[1]])
    return(normalizePath(dirname(script_path), winslash = "/", mustWork = TRUE))
  }
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

build_common_args <- function(cfg) {
  args <- character(0)
  if (!is.null(cfg$js_db)) args <- c(args, "--js-db", cfg$js_db)
  if (!is.null(cfg$ts_db)) args <- c(args, "--ts-db", cfg$ts_db)
  args
}

run_job <- function(job_script, common_args) {
  cmd_args <- c(job_script, common_args)
  cat(sprintf("[RUN] %s\n", basename(job_script)))
  status <- system2("Rscript", cmd_args)
  if (!identical(status, 0L)) {
    stop(sprintf("Job failed (%s) with exit code %s", basename(job_script), as.character(status)))
  }
  cat(sprintf("[OK]  %s\n", basename(job_script)))
  TRUE
}

run_jobs_parallel <- function(job_scripts, common_args, workers) {
  suppressPackageStartupMessages(library(parallel))

  workers <- min(workers, length(job_scripts))
  cl <- makeCluster(workers)
  on.exit(stopCluster(cl), add = TRUE)

  clusterExport(cl, varlist = c("run_job"), envir = environment())
  clusterExport(cl, varlist = c("common_args"), envir = environment())

  results <- parLapply(cl, job_scripts, function(job_script) {
    tryCatch({
      run_job(job_script, common_args)
      list(ok = TRUE, job = basename(job_script), error = "")
    }, error = function(e) {
      list(ok = FALSE, job = basename(job_script), error = conditionMessage(e))
    })
  })

  failed <- vapply(results, function(x) !isTRUE(x$ok), logical(1))
  if (any(failed)) {
    msgs <- vapply(results[failed], function(x) sprintf("- %s: %s", x$job, x$error), character(1))
    stop(paste(c("One or more jobs failed:", msgs), collapse = "\n"))
  }

  TRUE
}

main <- function() {
  cfg <- parse_args(commandArgs(trailingOnly = TRUE))
  script_dir <- get_script_dir()

  job_dir <- normalizePath(file.path(script_dir, "jobs"), winslash = "/", mustWork = TRUE)
  all_jobs <- c(
    file.path(job_dir, "01_ridge_release.R"),
    file.path(job_dir, "02_smells_catalog.R"),
    file.path(job_dir, "03_startup_tables.R"),
    file.path(job_dir, "04_ownership_tables.R")
  )

  if (!is.null(cfg$only)) {
    key <- tolower(trimws(cfg$only))
    selected <- switch(
      key,
      ridge = all_jobs[[1]],
      smells = all_jobs[[2]],
      startup = all_jobs[[3]],
      ownership = all_jobs[[4]],
      stop("Invalid --only value. Use: ridge|smells|startup|ownership")
    )
    job_scripts <- c(selected)
  } else {
    job_scripts <- all_jobs
  }

  common_args <- build_common_args(cfg)

  start_time <- Sys.time()
  if (isTRUE(cfg$parallel) && length(job_scripts) > 1) {
    cat(sprintf("Executing %d jobs in parallel (workers=%d)\n", length(job_scripts), min(cfg$workers, length(job_scripts))))
    run_jobs_parallel(job_scripts, common_args, cfg$workers)
  } else {
    cat(sprintf("Executing %d jobs sequentially\n", length(job_scripts)))
    for (job in job_scripts) {
      run_job(job, common_args)
    }
  }
  elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  cat(sprintf("All jobs completed in %.1f seconds\n", elapsed))
}

main()
