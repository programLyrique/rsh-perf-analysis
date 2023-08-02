#! /usr/bin/env Rscript

library(stringr)
library(dplyr)
library(purrr)
library(readr)



passes <- c(
    "ForceDominance", "ScopeResolution", "DeadStoreRemoval",
    "OptimizeAssumptions",
    "Cleanup", "Constantfold", "GVN", "ElideEnvSpec",
    "DelayInstr", "Inline", "OptimizeVisibility",
    "OptimizeContexts", "ElideEnv", "HoistInstruction", "Overflow",
    "TypeInference", "LoadElision", "EagerCalls", "LoopInvariant",
    "CleanupCheckpoints", "DelayEnv", "DotDotDots", "MatchCallArgs",
    "InlineForcePromises", "TypefeedbackCleanup"
)

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

runs <- list()
for (i in seq_along(passes)) {
    extracted <- map(files_to_run, process_profile, profile_dir, harness != "", banned_passes = passes[[i]], .progress = TRUE)

    runs[passes[[i]]] <- bind_rows(extracted)
}

profiles <- bind_rows(runs, .id = "banned_pass")

csv_path <- file.path(profile_dir, "profiles.csv")
append <- file.exists(csv_path)
write_csv(profiles, csv_path, append = append)
