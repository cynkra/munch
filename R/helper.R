# returns all mun_hist_ids that belong to the municipality with the id `mun_id_to` (in year `year_to`)
#
# mun_id_to (integer): a municipality ID that is valid in `year_to`
# year_from (integer): beginning of time interval to consider
# year_to (integer): end of time interval to consider
get_mun_hist <- function(mun_id_to, year_from = 1960L, year_to = as.integer(format(Sys.Date(), "%Y")), all_mutations = NULL) {
  # starting from all mutations
  if (is_null(all_mutations)) all_mutations <- get_all_mutations_slim()
  mut_in_time_interval <- filter_time_interval(all_mutations, year_from, year_to)

  get_mun_hist_impl(mut_in_time_interval, mun_id_to, year_from, year_to)
}

filter_time_interval <- function(all_mutations, year_from, year_to) {
  all_mutations %>%
    filter(
      # If `mDateOfChange.y` is "1960-01-01", the state of the municipality has never changed since its first-time registration
      # ergo it's still valid, whatever `year_from` is.
      as.integer(format(mAdmissionDate, "%Y")) <= year_to,
      mDateOfChange.y == mAdmissionDate |
        as.integer(format(mDateOfChange.y, "%Y")) >= year_from
    )
}

get_mun_hist_impl <- function(mut_in_time_interval, mun_id_to, year_from, year_to) {
  # only if the historicized municipality ID corresponding to the municipality ID is an existing municipality ID in `year_to`
  # we will accept it
  m_hist_id <- filter(
    mut_in_time_interval,
    mId.y == mun_id_to,
    # for the last mutation of a municipality (so far) the `mDateOfChange.y` is set to the same date as `mAdmissionDate`
    # that means for us that it is still valid
    mAdmissionDate == mDateOfChange.y |
      as.integer(format(mDateOfChange.y, "%Y")) >= year_to
  ) %>%
    pull(mHistId.y) %>%
    # it can happen, that this results in more than 1 row, if a municipality merger leads to
    # the same `mHistId.y` for 2 or more municipalities
    unique()
  calc_all_hist_ids(mut_in_time_interval, m_hist_id)
}

calc_all_hist_ids <- function(mut_in_time_interval, m_hist_id) {
  new_hist_ids <- m_hist_id
  all_hist_ids <- integer()
  out <- FALSE
  while (!out) {
    all_hist_ids <- append(all_hist_ids, new_hist_ids)
    new_hist_ids <- get_former_hist_ids(mut_in_time_interval, new_hist_ids)
    if (is_empty(new_hist_ids)) out <- TRUE
  }
  all_hist_ids
}

get_former_hist_ids <- function(mut_in_time_interval, hist_ids) {
  hist_ids <- map(hist_ids, get_former_hist_id_from_one, mut_in_time_interval) %>%
    squash_int()
  hist_ids[hist_ids %in% mut_in_time_interval$mHistId.y]
}

get_former_hist_id_from_one <- function(hist_id, mut_in_time_interval) {
  filter(mut_in_time_interval, mHistId.y == hist_id) %>%
    pull(mHistId.x) %>%
    discard(is.na)
}
