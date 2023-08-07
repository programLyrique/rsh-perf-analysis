#! /usr/bin/env Rscript

library(stringr)
library(dplyr)
library(purrr)
library(readr)

browseOnce <- function() {
    old <- getOption("error")
    function() {
        options(error = old)
        browser()
    }
}
options(error = browseOnce())


passes <- c(
    "Baseline", # not a real pass so won't desactivate any pass
    "ForceDominance", "ScopeResolution", "DeadStoreRemoval",
    "OptimizeAssumptions",
    "Cleanup", "Constantfold", "GVN", "ElideEnvSpec",
    "DelayInstr", "Inline", "OptimizeVisibility",
    "OptimizeContexts", "ElideEnv", "HoistInstruction", "Overflow",
    "TypeInference", "LoadElision", "EagerCalls", "LoopInvariant",
    "CleanupCheckpoints", "DelayEnv", "DotDotDots", "MatchCallArgs",
    "InlineForcePromises", "TypefeedbackCleanup"
)

# ~/RBenchmarking/Benchmarks/RealThing/ ../banned-passes/ harness

args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 0) {
    stop("Provide a path to the files to profile")
}

profile_dir <- if (length(args) < 2) {
    "profiles"
} else {
    args[[2]]
}


rfiles_dir <- args[[1]]

harness <- if (length(args) >= 3) {
    args[[3]]
} else {
    ""
}


optLevel <- 3

r_dir <- Sys.getenv("R_DIR", unset = "../build/bin")


rfiles_path <- list.files(rfiles_dir, pattern = "*\\.(R|r)", full.names = TRUE)

source("functions.r")

files_to_run <- discard(rfiles_path, function(rfile) {
    basename(rfile) == "harness.r"
})


extract_profiles <- function(pass_name) {
    profile_pass_dir <- file.path(profile_dir, pass_name)
    dir.create(profile_pass_dir, showWarnings = FALSE)
    map(files_to_run, process_profile, profile_pass_dir, harness != "", banned_passes = if (pass_name == "Baseline") {
        character(0)
    } else {
        pass_name
    }, nb_inner_iter = 10, .progress = TRUE)
}

runs <- list()
for (i in seq_along(passes)) {
    extracted <- extract_profiles(passes[[i]])
    runs[[passes[[i]]]] <- bind_rows(extracted)
}


profiles <- bind_rows(runs, .id = "banned_pass")

csv_path <- file.path(profile_dir, "profiles.csv")
append <- file.exists(csv_path)
write_csv(profiles, csv_path, append = append)
