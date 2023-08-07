get_metrics <- function(rfile_path) {
    # warn = FALSe to ignore
    lines <- readLines(rfile_path, warn = FALSE)

    list(nb_lines = length(lines), nb_loops = sum(str_detect(lines, "while|for|repeat")))
}


process_profile <- function(rfile_path, profile_dir, with_harness = FALSE, banned_passes = character(0), nb_outer_iter = 10, nb_inner_iter = 20, from_iteration = min(6, nb_outer_iter)) {
    current_dir <- getwd()

    cat("Profile ", rfile_path, " in ", current_dir, " with profile to be written in ", profile_dir, "with_harness=", with_harness, "banned_phases= ", banned_passes, "\n")

    # profile the file the "incomplete final line found" warning
    profile_path <- file.path(normalizePath(profile_dir), paste0("profile-phase-", basename(tools::file_path_sans_ext(rfile_path))))
    stdout_path <- file.path(normalizePath(profile_dir), paste0("stdout-", basename(tools::file_path_sans_ext(rfile_path))))
    passes_path <- file.path(normalizePath(profile_dir), paste0("passes-", basename(tools::file_path_sans_ext(rfile_path)), ".csv"))


    r_path <- file.path(r_dir, "R")

    rfile_name <- if (with_harness) {
        basename(tools::file_path_sans_ext(rfile_path))
    } else {
        basename(rfile_path)
    }

    # We set the current dir to where the R file is
    # in order for files it imports to be relative to it
    setwd(dirname(rfile_path))

    args <- if (with_harness) {
        c("-q", "-f harness.r", paste("--args ", rfile_name, nb_outer_iter, nb_inner_iter))
    } else {
        c("-q", paste0("-f ", rfile_name))
    }

    env <- c(
        "PIR_MEASURE_COMPILER=1", "PIR_MEASURE_COMPILER_BACKEND=1",
        paste0("PIR_MEASURING_LOGFILE=", profile_path),
        paste0("PIR_PASS_SCHEDULER_LOGFILE=", passes_path),
        paste0("PIR_OPT_LEVEL=", optLevel)
    )

    if (length(banned_passes) > 0) {
        env <- c(env, paste0("PIR_PASS_BLACKLIST=", paste0(banned_passes, collapse = "|")))
    }

    start_time <- Sys.time()
    res <- system2(r_path, args,
        stdout = stdout_path,
        env = env
    )
    end_time <- Sys.time()
    setwd(current_dir)

    if (res != 0) { # the command failed, just ignore
        cat("Error when profiling: ", rfile_path, "\n")
        return(tibble())
    }

    # get some metrics about the file
    metrics <- get_metrics(rfile_path)

    # Read the result of profiling
    lines <- readLines(profile_path)

    if (length(lines) < 7) {
        return(tibble())
    }

    parsed_line <- str_match(lines[[6]], "\\s*Timers \\((\\d+\\.?\\d*) (secs|min) [^\\)]*\\):")
    sec_convert <- if (parsed_line[, 3] == "min") {
        60
    } else {
        1
    }
    total_measured_time <- as.double(parsed_line[, 2]) * sec_convert

    # keep only lines with the phases
    lines <- lines[8:length(lines) - 1]

    df <- tibble(
        filename = rfile_path,
        nb_lines = metrics$nb_lines,
        nb_loops = metrics$nb_loops,
        execution_time = end_time - start_time,
        total_measured_time = total_measured_time
    )
    for (line in lines) {
        res <- str_match(line, "\\s*([^:\\s]+): ([^\\t]+)\\t(\\d+\\.?\\d*(?:e-\\d+)?)")

        phase_name <- str_replace_all(res[, 3], " ", "") # remove whitespaces to make it easier to handle later in R
        df[, phase_name] <- as.double(res[, 4])
        # percentages
        df[, paste0(phase_name, "-pc")] <- as.double(res[, 4]) / total_measured_time * 100
    }
    df
}
