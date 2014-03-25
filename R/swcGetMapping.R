#' Compute a matching table between two lists of municipality IDs
#' 
#' For two lists of Swiss municipality IDs at any two points in time, this
#' function creates a data frame with two columns where each row represents
#' a match between municipality IDs.  This can be used as an intermediate
#' table for merging two data sets with municipality identifiers taken at
#' different, possibly unknown, points in time.
#' 
#' It is advisable to use factors as list of municipality IDs.  By that,
#' comparisons and merges for municipality IDs are automatically checked for
#' compatibility.
#' 
#' @template swc
#' @param ids.from A list of "source" municipality IDs, preferably a factor
#' @param ids.to A list of "target" municipality IDs, preferably a factor
#' 
#' @return A data frame with columns prefixed by \code{from.} and \code{to} that
#'   represents the computed match.  The municipality IDs are stored in the
#'   columns \code{from.mId} and \code{to.mId}.  The columns
#'   \code{from.MergeType} and \code{to.MergeType} contain \code{valid} if
#'   the municipality is contained in both the input and the mapping table,
#'   \code{missing} if the municipality is missing from the input, and
#'   \code{extra} if the municipality is in the input but not in the mapping
#'   table; most columns are \code{NA} for such rows.  In addition, the column
#'   \code{MergeType} offers a summary of the "from" and "to" status: Rows with
#'   values other than \code{"valid"} or \code{"missing"} should be examined.
#' 
#' @example example/swcGetMapping.R
#' @export
swcGetMapping <- function(swc=swcGetData(), ids.from, ids.to) {
  #' For two lists of municipalitys, we construct a mapping from the first list
  #' to the second.  First, the most probable mutation number in the
  #' "municipality mutations" data set is computed.
  tid <- function(ids) {
    if (is.factor(ids)) ids <- as.character(ids)
    ids <- as.integer(ids)
    ids
  }
  ids.from <- sort(unique(ids.from))
  ids.from.int <- tid(ids.from)
  mid.from <- getMostProbableMutationId(swc=swc, ids.from.int)
  hist.list.from <- getHistIdList(swc=swc, mid.from)
  
  ids.to <- sort(unique(ids.to))
  ids.to.int <- tid(ids.to)
  mid.to <- getMostProbableMutationId(swc=swc, ids.to.int)
  hist.list.to <- getHistIdList(swc=swc, mid.to)
  
  ret <- getMunicipalityMappingWorker(swc, hist.list.from, mid.from, hist.list.to, mid.to)
  
  mt <- function(c) factor(c, levels=c("valid", "missing", "extra"))
  cn <- function(name, n) paste(name, n, sep='.')

  resultTable <- function(histId, inId, name) {
    ret <- swc$municipality[match(histId, swc$municipality$mHistId),
                                 c('mHistId', 'cAbbreviation', 'mId', 'mLongName', 'mShortName')]
    if (is.factor(inId)) {
      ret$mIdAsNumber <- ret$mId
      ret$mId <- factor(ret$mId, levels=levels(inId))
    }

    ret$MatchType <- mt(ifelse(ret$mId %in% inId, "valid", "missing"))
    names(ret) <- cn(names(ret), name)
    ret
  }
  
  extraTable <- function(retId, inId, name) {
    mId <- sort(setdiff(inId, retId))
    if (length(mId) == 0) return(NULL)
    ret <- data.frame(mId=mId)
    ret <- transform(ret, MatchType=mt("extra"))
    names(ret) <- cn(names(ret), name)
    ret
  }
  
  ret.from <- resultTable(ret$from, ids.from, "from")
  ret.to <- resultTable(ret$to, ids.to, "to")
  ret <- cbind(ret.from, ret.to)
  ret <- plyr::rbind.fill(ret,
                          extraTable(ret.from$mId.from, ids.from, "from"),
                          extraTable(ret.to$mId.to, ids.to, "to"))
  dMatchType <- c(
    `valid.valid`="valid",
    `missing.missing`="missing",
    `missing.valid`="missing.from",
    `extra.NA`="extra.from",
    `valid.missing`="missing.to",
    `NA.extra`="extra.to"
  )
  ret$MatchType <- factor(
    dMatchType[paste(ret$MatchType.from, ret$MatchType.to, sep='.')],
    levels=dMatchType)
  ret
}

getMostProbableMutationId <- function(swc, municipalityIds) {
  fitness <- getMunicipalityIdFitness(swc=swc, municipalityIds)
  fitness.max <- which.max(fitness$fitness)
  fitness$mMutationId[fitness.max]
}

getMunicipalityIdFitness <- function(swc, municipalityIds) {
  mun.mut <- swcGetMutations(swc=swc, municipalityIds)
  fm <- computeFitnessAndMunList(mun.mut)
  fm$fitness
}

computeMunList <- function(mun.mut.m) {
  logging::loginfo("computeMunList")
  #' The .y values of the argument contain the newly added, the .x values
  #' the dropped column identifiers.  This function verifies that after each
  #' .y value there is a corresponding .x value, and vice versa, and that the
  #' mutation IDs are in nondecreasing order.  The return value is the list of
  #' municipality IDs after all transformations have been applied.
  #' 
  #' Work around performance issues in plyr:
  mun.mut.m$mMutationId <- with(mun.mut.m, as.integer(mMutationId))
  #' 
  #' Split input:
  y <- subset(mun.mut.m, grepl("[.]y$", get("variable")))
  x <- subset(mun.mut.m, grepl("[.]x$", get("variable")))
  stopifnot(nrow(x) + nrow(y) == nrow(mun.mut.m))
  #'
  #' Assign to each x even sequence values, to each y odd sequence values:
  xs <- plyr::ddply(x, "value", plyr::mutate, seq=seq_along(get("value")) * 2)
  ys <- plyr::ddply(y, "value", plyr::mutate, seq=seq_along(get("value")) * 2 - 1)
  #'
  #' Mingle, order by municipality (=value) and sequence number:
  xys <- plyr::arrange(plyr::rbind.fill(xs, ys), get("value"), get("seq"))
  #'
  #' Municipality groups: Compute "group change points" and end of group:
  xys.dvg <- diff(xys$value) > 0
  xys.endgroup <- c(xys.dvg, TRUE)
  #'
  #' Mutation IDs must be nondecreasing, and sequence numbers must be increasing
  #' by one in each group (i.e., everywhere except perhaps at group boundaries)
  xys.dMutationId <- diff(xys$mMutationId)
  xys.dseq <- diff(as.integer(xys$seq))
  stopifnot(xys.dMutationId[!xys.dvg] >= 0)
  stopifnot(xys.dseq[!xys.dvg] == 1)
  #'
  #' Return value: All municipalities that have not expired at the end of their
  #' group.
  subset(xys[xys.endgroup, c("seq", "value")], seq %% 2 == 1)$value
}

computeFitnessAndMunList <- function(mun.mut, hist=F) {
  logging::loginfo("computeFitnessAndMunList")
  #' The list of mutations is processed in the order of the mutation ID, which
  #' is composed of mutation date and mutation number. (Conversation with Ernst
  #' Oberholzer end of March 2013.)  All records with the same mutation ID
  #' form a mutation.  The abolished municipality numbers are removed, the admitted
  #' municipality numbers are added to the global list of municipalitys.  Consistency of
  #' all mutations is checked in computeMunList().
  measure.vars=paste0(if (hist) "mHistId" else "mId", ".", c("x", "y"))
  
  mun.mut.m <- reshape2::melt(
    mun.mut, id.vars="mMutationId",
    measure.vars=measure.vars,
    na.rm=TRUE)
  mun.mut.m <- plyr::arrange(mun.mut.m,
                             get("mMutationId"), get("variable"))
  mun.mut.m <- unique(mun.mut.m)
  
  mun.list <- computeMunList(mun.mut.m)
  
  mun.mut.c <- reshape2::dcast(data=mun.mut.m, formula=mMutationId~variable,
                               fun.aggregate=function(x) { length(unique(x)) })
  # delta <- y - x
  mun.mut.c$delta <- with(mun.mut.c, get(measure.vars[[2]]) - get(measure.vars[[1]]))
  mun.mut.c$fitness <- cumsum(mun.mut.c$delta)
  kimisc::nlist(fitness=mun.mut.c[, c("mMutationId", "fitness")], mun.list)
}

getHistIdList <- function(swc, mutationId) {
  mun.mut <- subset(
    swcGetMutations(swc=swc), get("mMutationId") <= mutationId)
  fm <- computeFitnessAndMunList(mun.mut, hist=T)
  fm$mun.list
}

getMunicipalityMappingWorker <- function(swc, hist.list.from, mid.from, hist.list.to, mid.to) {
  #' Here, we receive the most probable mutation number in the "municipality
  #' mutations" data set as parameter.
  
  #' We assume that the first list is older than the second list:
  stopifnot(mid.from <= mid.to)
  
  #' Then we iterate over all mutations.  We must cover all mutations,
  #' because a mutation A -> A' -> B would be lost otherwise if A'
  #' is not in either municipality list.
  mun.mut <- swcGetMutations(swc=swc)
  
  #' The mapping will be represented as a linear transformation, encoded
  #' as a sparse matrix with rows as "from" and columns as "to" municipalitys. 
  #' Each mutation is converted to such a matrix, the matrix product of
  #' all mutations will be the final mapping.  We start with a unit
  #' matrix (the identity transformation) of all "from" columns:
  
  f <- Matrix::sparseMatrix(i=seq_along(hist.list.from),
                            j=seq_along(hist.list.from),
                            x=1,
                            dimnames=list(hist.list.from, hist.list.from))
  
  #' Subsequently, this transformation is augmented with all mutations
  #' between "from" and "to"
  trans.list <- plyr::ddply(
    subset(mun.mut, kimisc::in.interval.lo(
      as.numeric(get("mMutationId")), as.numeric(mid.from), as.numeric(mid.to))),
    "mMutationId",
    function(m) {
      rn <- colnames(f)
      
      abolId <- unique(m$mHistId.x)
      admId <- unique(m$mHistId.y)
      abolId <- subset(abolId, !is.na(abolId))
      admId <- subset(admId, !is.na(admId))
      logging::logdebug('%s: +(%s), -(%s)', m$mMutationId[1], format(admId), format(abolId))
      
      removedId <- setdiff(abolId, admId)
      remainingId <- setdiff(abolId, removedId)
      addedId <- sort(setdiff(admId, abolId))
      logging::logdebug('%s: ++(%s), =(%s), --(%s)', m$mMutationId[1], format(addedId), format(remainingId), format(removedId))
      
      stopifnot(sort(remainingId) == sort(setdiff(admId, addedId)))
      
      cn <- setdiff(c(rn, addedId), removedId)
      
      idId <- setdiff(rn, abolId)
      idI <- match(idId, rn)
      idJ <- match(idId, cn)
      stopifnot(!is.na(idI))
      stopifnot(!is.na(idJ))
      
      logging::logdebug('%s: (%s)->(%s)', m$mMutationId[1], format(m$mHistId.x), format(m$mHistId.y))
      trI <- match(m$mHistId.x, rn)
      trJ <- match(m$mHistId.y, cn)
      
      stopifnot(!is.na(trI))
      stopifnot(!is.na(trJ))
      logging::logdebug('%s: ((%s))->((%s))', m$mMutationId[1], format(trI), format(trJ))
      logging::logdebug('%s: %s x %s', m$mMutationId[1], length(rn), length(cn))
      g <- Matrix::sparseMatrix(c(idI, trI), c(idJ, trJ), x=1, dimnames=list(rn, cn))
      
      logging::logdebug('%s: %s %%*%% %s', m$mMutationId[1], dim(f), dim(g))
      f <<- f %*% g
      logging::logdebug('%s: %s', m$mMutationId[1], dim(f))
      
      data.frame(rows=length(rn), columns=length(cn))
    }
  )
  
  ff <- Matrix::summary(f)
  ff$from <- as.numeric(rownames(f)[ff$i])
  ff$to <- as.numeric(colnames(f)[ff$j])
  ff <- plyr::arrange(ff, get("from"))
  ff$i <- NULL
  ff$j <- NULL
  ff$x <- NULL
  as.data.frame(ff)
}
