#' Create a list of municipality mutations
#'
#' Municipality mutation lists are required to compute matching tables for
#' municipalities between different points in time.
#'
#' The municipality dataset of the Swiss historicized communes data is a list
#' of snapshots of municipality states.  Each state is valid during a certain
#' period of time,
#' adjoining states are linked by admission and abolition numbers:  The
#' abolition number of a former state is the same as the admission number of a
#' subsequent state.  The states can be thought of as nodes in a graph, where
#' the edges are \emph{mutations} -- transformations from one state to the
#' next.
#'
#' This function performs a self-merge on the municipality data: Each abolition
#' number is matched with its corresponding
#' admission number (if available).  If no corresponding admission or
#' abolition number is found, the record is included with NAs instead of
#' matched values.  Records without admission or abolition number are
#' excluded.
#' The result is a list of mutations, i.e., a list of edges in the graph of
#' municipality state snapshots.
#'
#' @param mids A list of municipality id's (BFS-numbers) of which the mutations should be
#'   retrieved.
#'
#' @inheritParams swc_get_municipality_state
#'
#' @return A data frame that represents mutations.
#'
#' @examples
#' head(swc_get_mutations(), 20)
#' head(subset(swc_get_mutations(), !is.na(mHistId.x)), 20)
#' @export
swc_get_mutations <- function(mids = NULL, canton = NULL) {
  municipality_mutations <- swc_get_municipality_mutations()

  if (!is.null(canton)) {
    municipality_mutations <-
      municipality_mutations %>%
      filter(cAbbreviation %in% !!canton)
  }

  mun.mut <- merge(
    subset(
      municipality_mutations
      [, c(
          "cAbbreviation",
          "mHistId",
          "mId",
          "mShortName",
          "mAbolitionNumber",
          "mAbolitionMode",
          "mAbolitionDate",
          "mDateOfChange"
        )],
      !is.na(get("mAbolitionNumber"))
    ),
    subset(
      municipality_mutations
      [, c(
          "mHistId",
          "mId",
          "mShortName",
          "mAdmissionNumber",
          "mAdmissionMode",
          "mAdmissionDate",
          "mDateOfChange"
        )],
      !is.na(get("mAdmissionNumber"))
    ),
    by.x = "mAbolitionNumber", by.y = "mAdmissionNumber", all = T, incomparables = NA
  )
  names(mun.mut)[1] <- "mMutationNumber"

  stopifnot(mun.mut$mMutationNumber == sort(mun.mut$mMutationNumber, na.last = T))

  mun.mut$mMutationDate <- with(
    mun.mut,
    coalesce(mAdmissionDate, mAbolitionDate + 1)
  )
  stopifnot(!is.na(mun.mut$mMutationDate))

  # Special cases:
  # 1a. Casima and Monte have been merged to Castel San Pietro, not to Caneggio
  # 1b. A part of Caneggio has been moved to Castel San Pietro. Nothing has been moved
  #     the other way round
  cases.to.remove <- data.frame(
    mId.x = c(5248, 5256, 5249),
    mId.y = c(5246, 5246, 5246)
  )

  mun.mut <- subset(
    mun.mut,
    !(paste(get("mId.x"), get("mId.y")) %in%
      paste(cases.to.remove$mId.x, cases.to.remove$mId.y))
  )

  # Same mutation numbers should refer to the same date, but they not always do.
  # First check the "big" first mutation 1000...
  mun.mut.test <- mun.mut[, c("mMutationDate", "mMutationNumber")]
  mun.mut.test.first <- subset(mun.mut.test, get("mMutationNumber") == 1000)
  stopifnot(mun.mut.test.first$mMutationDate == mun.mut.test.first$mMutationDate[1])
  rm(mun.mut.test.first)
  # ...then the others, using a self-merge by mutation number.
  mun.mut.test <- subset(mun.mut.test, get("mMutationNumber") != 1000)
  mun.mut.test <- merge(mun.mut.test, mun.mut.test, by = "mMutationNumber")
  # A difference of one day is tolerated.
  stopifnot(with(mun.mut.test, abs(mMutationDate.x - mMutationDate.y) <= 1))
  # For those with identical dates, the minimum date is selected...
  mun.mut.fix.prep <- filter(mun.mut.test, mMutationDate.x != mMutationDate.y)
  mun.mut.fix <- if (nrow(mun.mut.fix.prep) == 0) {
    mun.mut.fix.prep
  } else {
    group_by(mun.mut.fix.prep, mMutationNumber) %>%
      summarize(mMutationDate = min(mMutationDate.x, na.rm = TRUE))
  }

  fix.match.pos <- match(mun.mut$mMutationNumber, mun.mut.fix$mMutationNumber)
  mun.mut$mMutationDate[!is.na(fix.match.pos)] <- mun.mut.fix$mMutationDate[fix.match.pos[!is.na(fix.match.pos)]]

  mun.mut <- arrange(
    mun.mut,
    mMutationDate, mMutationNumber,
    mId.x, mId.y
  )
  mun.mut$mMutationId <- factor(interaction(mun.mut$mMutationDate, mun.mut$mMutationNumber), ordered = T)
  # mun.mut$mMutationId <- factor(mun.mut$mMutationDate, ordered=T)

  # Remove Liechtenstein and lakes
  out <-
    mun.mut %>%
    as_tibble() %>%
    filter(is.na(mId.y) | mId.y < 7000)

  if (!is.null(mids)) {
    out <-
      out %>%
      filter(mId.x %in% !!mids | mId.y %in% !!mids)
  }

  out
}

get_all_mutations_slim <- function() {
  as_tibble(swc_get_mutations()) %>%
    select(mHistId.x, mHistId.y, mId.y, mShortName.y, mAdmissionDate, mDateOfChange.y)
}
