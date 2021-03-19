#' ---
#' output: html_notebook
#' ---

#- setup, message = FALSE, results = "hide"
library(tidyverse)
pkgload::load_all()
knitr::opts_chunk$set(max.print = 1000)

summarize_ungroup <- function(.data, ...) {
  summarize(.data, ..., .groups = "drop")
}

#' # Arguments

# From analyze.R: ignore trivial area changes
IGNORE_MUTATIONS <- c(3293, 3432, 3932)

# From analyze.R:
# - nontrivial mutation happened in 2004
# - for now we're ignoring all years of 2004 and before
# - 2005 is fine as a baseline
# - we use only abolitions from 2005 or later
START_DATE <- as.Date("2005-01-01")

# Smaller working set, use NULL for all
#CANTON <- "LU"
CANTON <- NULL

#' # Preparation
#'
#' Base set:

mutations <-
  swc_get_mutations(canton = CANTON) %>%
  filter(mAbolitionDate >= !!START_DATE) %>%
  filter(!(mMutationNumber %in% !!IGNORE_MUTATIONS))

#' Sanity check: Each mutations maps only to one target commune.

mutations_check <-
  mutations %>%
  distinct(mMutationNumber, mId.y, mShortName.y) %>%
  add_count(mMutationNumber) %>%
  filter(n > 1)

stopifnot(nrow(mutations_check) == 0)


mutations_diff <-
  mutations %>%
  filter(mId.x != mId.y | mShortName.x != mShortName.y)

#' Summarize mutations:

mutations_diff %>%
  count(mAdmissionMode, mAbolitionMode, mMutationNumber) %>%
  select(-n) %>%
  count(mAbolitionMode, mAdmissionMode)

mutations_diff %>%
  count(mAbolitionMode, mAdmissionMode, mMutationNumber, name = "num_changed_municipalities") %>%
  count(mAbolitionMode, mAdmissionMode, num_changed_municipalities)

#' Restrict columns, aggregate by year:

mutations_base <-
  mutations_diff %>%
  select(mAdmissionDate, mMutationNumber, mId.x, mShortName.x, mId.y, mShortName.y) %>%
  mutate(year = as.integer(lubridate::year(mAdmissionDate))) %>%
  select(-mAdmissionDate, -mMutationNumber)

mutations_base

nested_mutations <-
  mutations_base %>%
  nest(x = -year)

#' Prototype for "no mutations":

mutation_init <-
  mutations_base[0, ] %>%
  select(-year)

#' All target years:

target_years <- seq2(min(nested_mutations$year - 1L), max(nested_mutations$year))

#' # Processing
#'
#' All valid year pairs:

year_pairs <-
  crossing(target_year = target_years, year = target_years) %>%
  filter(year < target_year)

year_pairs

#' The entire mapping table for all years is computed in this pipe.
#' It uses `accumulate_mappings()` and `compact_mapping()`, described below.

mapping_per_target_year <-
  year_pairs %>%
  left_join(nested_mutations, by = "year") %>%
  group_by(target_year) %>%
  summarize_ungroup(flat = list(
    accumulate_mappings(c(year, target_year[[1]]), x, mutation_init)
  )) %>%
  mutate(compact = map(flat, compact_mapping))

#' `accumulate_mappings()` does the heavy lifting, using `purrr::accumulate()`
#' in backward direction.
#' For e.g. target year 2007, this returns, in an efficient manner:
#'
#' - 2005+2006+2007
#' - 2006+2007
#' - 2007
#' - empty set

accumulate_mappings

#' The above function calls `combine_mapping()`, the purpose of this function
#' is to combine two mapping tables via `full_join()` and `coalesce()`.
#' We no longer need sparse matrices:

combine_mapping

#' For compaction, the `compact_mapping()` function splits sequences of years
#' into `year_from` and `year_to` pairs.

compact_mapping

#' `min_safe()` and `max_safe()` are simple wrappers that account for
#' the zero-length corner case.

min_safe

#' The overall result is a nested tibble, one sub-tibble per target year
#' with compact and explicit mappings:

mapping_per_target_year %>%
  mutate_at(vars(flat, compact), list(~ map_int(.x, nrow)))

#' Below are example mappings for three target years:
#'
#' ## 2006, flat

mapping_per_target_year %>%
  filter(target_year == 2006) %>%
  select(flat) %>%
  unnest(flat)

#'
#' ## 2006, compact

mapping_per_target_year %>%
  filter(target_year == 2006) %>%
  select(compact) %>%
  unnest(compact)

#' ## 2014

mapping_per_target_year %>%
  filter(target_year == 2014) %>%
  select(compact) %>%
  unnest(compact)

#' ## 2020

mapping_per_target_year %>%
  filter(target_year == 2020) %>%
  select(compact) %>%
  unnest(compact)

#' # Final flat table
#'
#' Unnesting the mapping table without filtering gives the final flat table:

mapping_per_target_year %>%
  select(target_year, compact) %>%
  unnest(compact)
