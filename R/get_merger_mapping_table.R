#' Create mapping table for a certain time interval
#'
#' Produces a mapping table that can be joined to your data.
#' Municipalities that were merged to another municipality during the given time period
#' are mapped to that municipality.
#' Filtering by canton is supported.
#'
#' @param start_year First year of time interval (integer)
#' @param end_year Last year of time interval (integer)
#' @inheritParams swc_get_municipality_state
#' @param type Two options:
#'
#'     - "flat" (default) returns the table with one row per year per mapping
#'
#'     - "compact" returns a more compact table with one row per mapping,
#'   containing the time interval it is valid for
#'
#' @return Mapping table for the given time interval in the specified canton
#' @export
#'
#' @examples
#' swc_get_merger_mapping_table(2005, 2010)
#' swc_get_merger_mapping_table(2015, 2019, canton = "ZH", type = "compact")
swc_get_merger_mapping_table <- function(start_year, end_year, canton = NULL, type = "flat") {
  summarize_ungroup <- function(.data, ...) {
    summarize(.data, ..., .groups = "drop")
  }
  # From analyze.R: ignore trivial area changes
  IGNORE_MUTATIONS <- c(3293, 3432, 3932)

  START_DATE <- as.Date(paste0(start_year, "-01-01"))


  mutations <-
    swc_get_mutations(canton = canton) %>%
    filter(mAbolitionDate >= !!START_DATE) %>%
    filter(!(mMutationNumber %in% !!IGNORE_MUTATIONS))

  mutations_check <-
    mutations %>%
    distinct(mMutationNumber, mId.y, mShortName.y) %>%
    add_count(mMutationNumber) %>%
    filter(n > 1)

  stopifnot(nrow(mutations_check) == 0)

  mutations_diff <-
    mutations %>%
    filter(mId.x != mId.y | mShortName.x != mShortName.y)

  mutations_base <-
    mutations_diff %>%
    select(mAdmissionDate, mMutationNumber, mId.x, mShortName.x, mId.y, mShortName.y) %>%
    mutate(year = as.integer(lubridate::year(mAdmissionDate))) %>%
    select(-mAdmissionDate, -mMutationNumber)

  source_years <- seq2(start_year, end_year - 1)

  # Subtract one -- `year` refers to the last year where the state *before* the mutation
  nested_mutations <-
    mutations_base %>%
    nest(x = -year) %>%
    mutate(year = year - 1) %>%
    left_join(tibble(year = !!source_years), ., by = "year")

  mutation_init <-
    get_municipalities_identity_mapping(end_year, canton)

  flat <- accumulate_mappings(c(source_years, end_year), nested_mutations$x, mutation_init)

  if (type == "flat") {
    flat
  } else {
    compact_mapping(flat)
  }
}


get_municipalities_identity_mapping <- function(year, canton = NULL) {

  swc_get_municipality_state(year = year, canton = canton) %>%
    transmute(mId.x = mun_id, mShortName.x = short_name, mId.y = mun_id, mShortName.y = short_name)
}
