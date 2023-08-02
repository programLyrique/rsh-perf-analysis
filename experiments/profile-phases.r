#! /usr/bin/env Rscript

library(stringr)
library(dplyr)
library(purrr)
library(readr)

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

extracted <- map(discard(rfiles_path, function(rfile) {
    basename(rfile) == "harness.r"
}), process_profile, profile_dir, harness != "", .progress = TRUE)


profiles <- bind_rows(extracted)


csv_path <- file.path(profile_dir, "profiles.csv")
append <- file.exists(csv_path)
write_csv(profiles, csv_path, append = append)
