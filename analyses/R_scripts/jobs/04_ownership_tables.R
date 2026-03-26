args <- commandArgs(trailingOnly = TRUE)

script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
job_path <- normalizePath(sub("^--file=", "", script_arg[[1]]), winslash = "/", mustWork = TRUE)
project_root <- normalizePath(file.path(dirname(job_path), "..", "..", ".."), winslash = "/", mustWork = TRUE)

script_path <- normalizePath(file.path(project_root, "analyses", "e2e_empirical_analyzer.R"), winslash = "/", mustWork = TRUE)
cmd_args <- c(script_path, "--ownership-tables-only", args)
old_wd <- getwd()
on.exit(setwd(old_wd), add = TRUE)
setwd(project_root)
status <- system2("Rscript", cmd_args)
quit(save = "no", status = status)
