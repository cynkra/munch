#' Check internal assumptions on historic commune data
#' 
#' This functon checks several assumptions made for the historic commune data,
#'   mostly for internal purposes:
#' 
#' \itemize{
#'   \item Each admission number has less than 5 records (with the exception
#'     of the first-time registration where all communes share the same
#'     admission number)
#'   \item Admission numbers are roughly increasing by date, except for
#'     differences of one day
#'   \item The \code{mHist} column is a surrogate key
#' }
#' 
#' @template swc
#' 
#' @return Invisible named list with the following elements:
#' 
#' \describe{
#'   \item{\code{statesWithNonUniqueAdmissionNumbers}}{All municipality
#'     states where the admission number occurs at least in one other
#'     municipality state}
#'   \item{\code{stateSequencesWithDecreasingDate}}{State sequences
#'     where the date decreases}
#' }
#' 
#' @export
swcCheckData <- function(swc=swcGetData()) {
  admissionNumberCounts <- plyr::ddply(
    swc$municipality[, "mAdmissionNumber", drop = FALSE],
    "mAdmissionNumber",
    plyr::summarize,
    count=length(get("mAdmissionNumber"))
  )
  admissionNumberCounts <- admissionNumberCounts[-1, ]
  
  stopifnot(with(admissionNumberCounts, count < 5))
  # All entries with more than one municipality per admission number
  statesWithNonUniqueAdmissionNumbers <- merge(
    swc$municipality, subset(admissionNumberCounts,
                             kimisc::in.interval.ro(get("count"), 2L, 5L)))

  # Admission numbers are roughly increasing by date
  mutationsSortedByAdmissionNumber <- plyr::arrange(swc$municipality, get("mAdmissionNumber"))
  admissionDateDiff <- diff(mutationsSortedByAdmissionNumber$mAdmissionDate)
  admissionNumberJumps <- which(admissionDateDiff < 0)
  admissionNumberJumpsBig <- which(admissionDateDiff < -1)
  stopifnot(length(admissionNumberJumps) < 10)
  stopifnot(length(admissionNumberJumpsBig) == 0)

  mutationIndexesOfJumps <- unique(sort(c(admissionNumberJumps,
                                          admissionNumberJumps + 1)))
  stateSequencesWithDecreasingDate <- swc$municipality[mutationIndexesOfJumps, ]

  # mHistId is surrogate key
  stopifnot(swc$municipality.adm$mHistId == unique(swc$municipality.adm$mHistId))
  
  invisible(kimisc::nlist(statesWithNonUniqueAdmissionNumbers,
                          stateSequencesWithDecreasingDate))
}
