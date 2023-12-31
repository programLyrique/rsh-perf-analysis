get_metrics <- function(rfile_path) {
    # warn = FALSe to ignore
    lines <- readLines(rfile_path, warn = FALSE)

    list(nb_lines = length(lines), nb_loops = sum(str_detect(lines, "while|for|repeat")))
}



get_peak_performance <- function(stdout_path, from_iteration = 6) {
    lines <- readLines(stdout_path)

    # the beginning of the output is the commands that were given to R
    # we need to reach `> run(commandArgs(trailingOnly=TRUE))`

    runCommand_start <- Position(function(x) {
        startsWith(x, "> run(commandArgs(trailingOnly=TRUE))")
    }, lines, right = TRUE, nomatch = 0)

    iterations_perf <- lines[(runCommand_start + 1):length(lines)]

    runtimes <- stringr::str_match_all(iterations_perf, "runtime: ([0-9]+)us")
    runtimes <- do.call(rbind, runtimes)
    runtimes <- as.integer(runtimes[, 2])

    if (from_iteration > length(runtimes)) {
        from_iteration <- 1
    }

    mean(runtimes[from_iteration:length(runtimes)])
}

get_peak_performance_from_file <- function(rfile_path, profile_dir, from_iteration = 6) {
    stdout_path <- file.path(normalizePath(profile_dir), paste0("stdout-", basename(tools::file_path_sans_ext(rfile_path))))
    get_peak_performance(stdout_path)
}

# subdir=TRUE if benchmarks are placed in subdirectories
process_profile <- function(rfile_path, profile_dir, with_harness = FALSE, banned_passes = character(0), nb_outer_iter = 10, nb_inner_iter = 20, from_iteration = max(nb_outer_iter, 6), subdir = FALSE) {
    current_dir <- getwd()

    cat("Profile ", rfile_path, " in ", current_dir, " with profile to be written in ", profile_dir, "with_harness=", with_harness, "banned_phases= ", banned_passes, "\n")

    # profile the file the "incomplete final line found" warning
    profile_path <- file.path(normalizePath(profile_dir), paste0("profile-phase-", basename(tools::file_path_sans_ext(rfile_path))))
    stdout_path <- file.path(normalizePath(profile_dir), paste0("stdout-", basename(tools::file_path_sans_ext(rfile_path))))
    passes_path <- file.path(normalizePath(profile_dir), paste0("passes-", basename(tools::file_path_sans_ext(rfile_path)), ".csv"))


    r_path <- file.path(r_dir, "R")

    rfile_name <- if (with_harness) {
        base <- basename(tools::file_path_sans_ext(rfile_path))
        if (subdir) {
            file.path(basename(dirname(rfile_path)), base)
        }
    } else {
        basename(rfile_path)
    }

    # We set the current dir to where the harness is
    # in order for files it imports to be relative to it
    setwd(if (subdir) {
        dirname(dirname(rfile_path))
    } else {
        dirname(rfile_path)
    })

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

    # get peak performances from the measurement of perf for each outer iteration
    peak_avg_exec_time <- get_peak_performance(stdout_path, 6) # take iterations 6 to 15

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
        total_measured_time = total_measured_time,
        peak_execution_time = peak_avg_exec_time
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
