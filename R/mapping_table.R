combine_mapping <- function(earlier, later) {
  if (is.null(earlier)) {
    return(later)
  }
  if (is.null(later)) {
    return(earlier)
  }

  earlier_join <-
    earlier %>%
    rename(
      mId = mId.y,
      mShortName = mShortName.y
    )

  later_join <-
    later %>%
    rename(
      mId = mId.x,
      mShortName = mShortName.x
    )

  both <- full_join(earlier_join, later_join, by = c("mId", "mShortName"), suffix = c("", ""))

  out <-
    both %>%
    transmute(
      mId.x = coalesce(mId.x, mId),
      mId.y = coalesce(mId.y, mId),
      mShortName.x = coalesce(mShortName.x, mShortName),
      mShortName.y = coalesce(mShortName.y, mShortName)
    ) %>%
    arrange(mId.x, mId.y)

  stopifnot(!anyDuplicated(out$mId.x))
  stopifnot(!anyDuplicated(out$mShortName.x))

  out
}

is_compact <- function(x) {
  all(diff(x) == 1)
}

accumulate_mappings <- function(year, mappings, mapping_init) {
  data <- accumulate(
    mappings, combine_mapping,
    .init = mapping_init,
    .dir = "backward"
  )

  tibble(year, data) %>%
    unnest(data) %>%
    rename(
      mun_id_x = mId.x, short_name_x = mShortName.x,
      mun_id_y = mId.y, short_name_y = mShortName.y
    )
}

min_safe <- function(x) {
  if (length(x) == 0) x[NA_integer_] else min(x)
}

max_safe <- function(x) {
  if (length(x) == 0) x[NA_integer_] else max(x)
}

compact_mapping <- function(flat) {
  compact_check <-
    flat %>%
    group_by(mun_id_x, short_name_x, mun_id_y, short_name_y) %>%
    summarize(is_compact = all(diff(year) == 1)) %>%
    ungroup()

  stopifnot(compact_check$is_compact)

  out <-
    flat %>%
    group_by(mun_id_x, short_name_x, mun_id_y, short_name_y) %>%
    summarize(year_from = min_safe(year), year_to = max_safe(year)) %>%
    ungroup() %>%
    select(year_from, year_to, everything())

  out
}
