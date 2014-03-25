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
#' @template swc
#' @param municipalityIds A vector of municipality IDs of interest, default is
#'   to use all municipalities.
#' 
#' @return A data frame that represents mutations.
#' 
#' @examples
#' head(swcGetMutations(), 20)
#' head(subset(swcGetMutations(), !is.na(mHistId.x)), 20)
#' @export
swcGetMutations <- function(swc=swcGetData(), municipalityIds=NULL) {
  mun.mut <- merge(
    subset(
      swc$municipality[
        , c('mHistId',
            'mId',
            'mShortName',
            'mAbolitionNumber',
            'mAbolitionMode',
            'mAbolitionDate',
            'mDateOfChange')],
      !is.na(get("mAbolitionNumber"))),
    subset(
      swc$municipality[
        , c('mHistId',
            'mId',
            'mShortName',
            'mAdmissionNumber',
            'mAdmissionMode',
            'mAdmissionDate',
            'mDateOfChange')],
      !is.na(get("mAdmissionNumber"))),
    by.x='mAbolitionNumber', by.y='mAdmissionNumber', all=T, incomparables=NA)
  names(mun.mut)[1] <- 'mMutationNumber'
  
  stopifnot(mun.mut$mMutationNumber == sort(mun.mut$mMutationNumber, na.last=T))
  
  mun.mut$mMutationDate <- with(
    mun.mut,
    kimisc::coalesce.na(mAdmissionDate, mAbolitionDate + 1, replace=NA))
  stopifnot(!is.na(mun.mut$mMutationDate))
  
  # Special cases: 
  # 1a. Casima and Monte have been merged to Castel San Pietro, not to Caneggio
  # 1b. A part of Caneggio has been moved to Castel San Pietro. Nothing has been moved
  #     the other way round
  cases.to.remove <- data.frame(
    mId.x=c(5248, 5256, 5249),
    mId.y=c(5246, 5246, 5246)
  )
  
  mun.mut <- subset(
    mun.mut,
    !(paste(get("mId.x"), get("mId.y")) %in%
        paste(cases.to.remove$mId.x, cases.to.remove$mId.y)))
  
  # Same mutation numbers should refer to the same date, but they not always do.
  # First check the "big" first mutation 1000...
  mun.mut.test <- mun.mut[, c('mMutationDate', 'mMutationNumber')]
  mun.mut.test.first <- subset(mun.mut.test, get("mMutationNumber") == 1000)
  stopifnot(mun.mut.test.first$mMutationDate == mun.mut.test.first$mMutationDate[1])
  rm(mun.mut.test.first)
  # ...then the others, using a self-merge by mutation number.
  mun.mut.test <- subset(mun.mut.test, get("mMutationNumber") != 1000)
  mun.mut.test <- merge(mun.mut.test, mun.mut.test, by='mMutationNumber')
  # A difference of one day is tolerated.
  stopifnot(with(mun.mut.test, abs(mMutationDate.x - mMutationDate.y) <= 1))
  # For those with identical dates, the minimum date is selected...
  mun.mut.fix <- plyr::ddply(
    subset(mun.mut.test, get("mMutationDate.x") != get("mMutationDate.y")),
    "mMutationNumber",
    function(df)
      data.frame(mMutationNumber=df$mMutationNumber[1], mMutationDate=min(df$mMutationDate.x))
  )
  # ...and applied
  fix.match.pos <- match(mun.mut$mMutationNumber, mun.mut.fix$mMutationNumber)
  mun.mut$mMutationDate[!is.na(fix.match.pos)] <- mun.mut.fix$mMutationDate[fix.match.pos[!is.na(fix.match.pos)]]
  
  mun.mut <- plyr::arrange(mun.mut,
                           get("mMutationDate"), get("mMutationNumber"),
                           get("mId.x"), get("mId.y"))
  mun.mut$mMutationId <- factor(interaction(mun.mut$mMutationDate, mun.mut$mMutationNumber), ordered=T)
  #mun.mut$mMutationId <- factor(mun.mut$mMutationDate, ordered=T)
  
  #' Finally we exclude those records that do not fit our desired
  #' list of municipality IDs. We need to do this here because we
  #' want to keep the factor levels of mMutationId consistent.
  if (!is.null(municipalityIds)) {
    municipalityIds <- c(municipalityIds, NA)
    mId.x.in <- with(mun.mut, mId.x %in% municipalityIds)
    mId.y.in <- with(mun.mut, mId.y %in% municipalityIds)
    mId.in <- mId.x.in | mId.y.in
    mId.x.nin <- which(!mId.x.in & mId.y.in)
    mId.y.nin <- which(!mId.y.in & mId.x.in)
    
    for (n in c('mHistId.x', 'mId.x', 'mShortName.x', 'mAbolitionMode', 'mAbolitionDate', 'mDateOfChange.x'))
      mun.mut[mId.x.nin, n] <- NA
    for (n in c('mHistId.y', 'mId.y', 'mShortName.y', 'mAdmissionMode', 'mAdmissionDate', 'mDateOfChange.y'))
      mun.mut[mId.y.nin, n] <- NA
    
    mun.mut <- subset(mun.mut, mId.in)
  }
  
  mun.mut
}
