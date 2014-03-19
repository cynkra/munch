local({

getMunicipalityMutations <- function(municipalityIds=NULL) {
  #' Converts the list of "municipality states" in commhist.municipality
  #' to a list of mutations, for a given list of municipality IDs

  #' Self-merge: Each abolition number is matched with its corresponding 
  #' admission number (if available).  If no corresponding admission or 
  #' abolition number is found, the record is included with NAs instead of
  #' matched values.  Records without admission or abolition number are
  #' excluded.
  #' 
  #' The result is a list of mutations, as opposed to a list of
  #' snapshots of municipality states (edge list vs. node list).
  mun.mut <- merge(
    subset(
      commhist.municipality[
        , c('mHistId',
            'mId',
            'mShortName',
            'mAbolitionNumber',
            'mAbolitionMode',
            'mAbolitionDate',
            'mDateOfChange')],
      !is.na(mAbolitionNumber)),
    subset(
      commhist.municipality[
        , c('mHistId',
            'mId',
            'mShortName',
            'mAdmissionNumber',
            'mAdmissionMode',
            'mAdmissionDate',
            'mDateOfChange')],
      !is.na(mAdmissionNumber)),
    by.x='mAbolitionNumber', by.y='mAdmissionNumber', all=T, incomparables=NA)
  names(mun.mut)[1] <- 'mMutationNumber'
  
  stopifnot(mun.mut$mMutationNumber == sort(mun.mut$mMutationNumber, na.last=T))
  
  mun.mut$mMutationDate <- with(mun.mut, coalesce.na(mAdmissionDate, mAbolitionDate + 1, replace=NA))
  stopifnot(!is.na(mun.mut$mMutationDate))
  
  # Special cases: 
  # 1a. Casima and Monte have been merged to Castel San Pietro, not to Caneggio
  # 1b. A part of Caneggio has been moved to Castel San Pietro. Nothing has been moved
  #     the other way round
  cases.to.remove <- data.frame(
    mId.x=c(5248, 5256, 5249),
    mId.y=c(5246, 5246, 5246)
  )
  
  mun.mut <- subset(mun.mut, paste(mId.x, mId.y) %nin% paste(cases.to.remove$mId.x, cases.to.remove$mId.y))
  
  # Same mutation numbers should refer to the same date, but they not always do.
  # First check the "big" first mutation 1000...
  mun.mut.test <- mun.mut[, c('mMutationDate', 'mMutationNumber')]
  mun.mut.test.first <- subset(mun.mut.test, mMutationNumber == 1000)
  stopifnot(mun.mut.test.first$mMutationDate == mun.mut.test.first$mMutationDate[1])
  rm(mun.mut.test.first)
  # ...then the others, using a self-merge by mutation number.
  mun.mut.test <- subset(mun.mut.test, mMutationNumber != 1000)
  mun.mut.test <- merge(mun.mut.test, mun.mut.test, by='mMutationNumber')
  # A difference of one day is tolerated.
  stopifnot(with(mun.mut.test, abs(mMutationDate.x - mMutationDate.y) <= 1))
  # For those with identical dates, the minimum date is selected...
  mun.mut.fix <- ddply(
    subset(mun.mut.test, mMutationDate.x != mMutationDate.y),
    .(mMutationNumber),
    function(df)
      data.frame(mMutationNumber=df$mMutationNumber[1], mMutationDate=min(df$mMutationDate.x))
  )
  # ...and applied
  fix.match.pos <- match(mun.mut$mMutationNumber, mun.mut.fix$mMutationNumber)
  mun.mut$mMutationDate[!is.na(fix.match.pos)] <- mun.mut.fix$mMutationDate[fix.match.pos[!is.na(fix.match.pos)]]
  
  mun.mut <- arrange(mun.mut, mMutationDate, mMutationNumber, mId.x, mId.y)
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

computeFitnessAndMunList <- function(mun.mut, hist=F) {
  #' The list of mutations is processed in the order of the mutation ID, which
  #' is composed of mutation date and mutation number. (Conversation with Ernst
  #' Oberholzer end of March 2013.)  All records with the same mutation ID
  #' form a mutation.  The abolished municipality numbers are removed, the admitted
  #' municipality numbers are added to the global list of municipalitys.  Consistency of
  #' all mutations is checked along the way.
  mun.list <- c()
  fitness <- ddply(
    mun.mut,
    .(mMutationId),
    function(m) {
      if (!hist) {
        abolId <- unique(m$mId.x)
        admId <- unique(m$mId.y)
      } else {
        abolId <- unique(m$mHistId.x)
        admId <- unique(m$mHistId.y)
      }
      abolId <- subset(abolId, !is.na(abolId))
      admId <- subset(admId, !is.na(admId))
      logdebug('%s: +(%s), -(%s)', m$mMutationId[1], format(admId), format(abolId))
      
      stopifnot(abolId %in% mun.list)
      mun.list <<- setdiff(mun.list, abolId)
      stopifnot(admId %nin% mun.list)
      mun.list <<- c(mun.list, admId)

      data.frame(mMutationId=m$mMutationId, fitness=length(mun.list))
    }
  )
  fitness$mMutationId <- factor(fitness$mMutationId, levels=levels(mun.mut$mMutationId), ordered=T)
  list(fitness=fitness, mun.list=mun.list)
}

getMunicipalityIdFitness <- function(municipalityIds) {
  mun.mut <- getMunicipalityMutations(municipalityIds)
  fm <- computeFitnessAndMunList(mun.mut)
  fm$fitness
}

getMostProbableMutationId <- function(municipalityIds) {
  fitness <- getMunicipalityIdFitness(municipalityIds)
  fitness.max <- which.max(fitness$fitness)
  fitness$mMutationId[fitness.max]
}

getHistIdList <- function(mutationId) {
  mun.mut <- subset(
    getMunicipalityMutations(), mMutationId <= mutationId)
  fm <- computeFitnessAndMunList(mun.mut, hist=T)
  fm$mun.list
}

getMunicipalityMapping <- function(ids.from, ids.to) {
  #' For two lists of municipalitys, we construct a mapping from the first list
  #' to the second.  First, the most probable mutation number in the
  #' "municipality mutations" data set is computed.
  ids.from <- sort(unique(ids.from))
  mid.from <- getMostProbableMutationId(ids.from)
  hist.list.from <- getHistIdList(mid.from)

  ids.to <- sort(unique(ids.to))
  mid.to <- getMostProbableMutationId(ids.to)
  hist.list.to <- getHistIdList(mid.to)
  
  ret <- getMunicipalityMappingWorker(hist.list.from, mid.from, hist.list.to, mid.to)

  ret.from <- commhist.municipality[match(ret$from, commhist.municipality$mHistId),
                                    c('mHistId', 'cAbbreviation', 'mId', 'mLongName', 'mShortName')]
  names(ret.from) <- paste('from', names(ret.from), sep='.')
  ret.to <- commhist.municipality[match(ret$to, commhist.municipality$mHistId),
                                  c('mHistId', 'cAbbreviation', 'mId', 'mLongName', 'mShortName')]
  names(ret.to) <- paste('to', names(ret.to), sep='.')
  ret <- cbind(ret.from, ret.to)
  
  attr(ret, 'extra.mun.from') <- sort(setdiff(ids.from, ret$from.mId))
  attr(ret, 'missing.mun.from') <- sort(setdiff(unique(ret$from.mId), ids.from))
  attr(ret, 'extra.mun.to') <- sort(setdiff(ids.to, ret$to.mId))
  attr(ret, 'missing.mun.to') <- sort(setdiff(unique(ret$to.mId), ids.to))

  ret
}

getMunicipalityMappingWorker <- function(hist.list.from, mid.from, hist.list.to, mid.to) {
  #' Here, we receive the most probable mutation number in the "municipality
  #' mutations" data set as parameter.
  
  #' We assume that the first list is older than the second list:
  stopifnot(mid.from <= mid.to)
  
  #' Then we iterate over all mutations.  We must cover all mutations,
  #' because a mutation A -> A' -> B would be lost otherwise if A'
  #' is not in either municipality list.
  mun.mut <- getMunicipalityMutations()
  
  #' The mapping will be represented as a linear transformation, encoded
  #' as a sparse matrix with rows as "from" and columns as "to" municipalitys. 
  #' Each mutation is converted to such a matrix, the matrix product of
  #' all mutations will be the final mapping.  We start with a unit
  #' matrix (the identity transformation) of all "from" columns:
  
  f <- sparseMatrix(i=seq_along(hist.list.from), j=seq_along(hist.list.from), x=1,
                    dimnames=list(hist.list.from, hist.list.from))
  
  #' Subsequently, this transformation is augmented with all mutations
  #' between "from" and "to"
  trans.list <- ddply(
    subset(mun.mut, in.interval.lo(as.numeric(mMutationId), as.numeric(mid.from), as.numeric(mid.to))),
    .(mMutationId),
    function(m) {
      rn <- colnames(f)
      
      abolId <- unique(m$mHistId.x)
      admId <- unique(m$mHistId.y)
      abolId <- subset(abolId, !is.na(abolId))
      admId <- subset(admId, !is.na(admId))
      logdebug('%s: +(%s), -(%s)', m$mMutationId[1], format(admId), format(abolId))
      
      removedId <- setdiff(abolId, admId)
      remainingId <- setdiff(abolId, removedId)
      addedId <- sort(setdiff(admId, abolId))
      logdebug('%s: ++(%s), =(%s), --(%s)', m$mMutationId[1], format(addedId), format(remainingId), format(removedId))
      
      stopifnot(sort(remainingId) == sort(setdiff(admId, addedId)))
      
      cn <- setdiff(c(rn, addedId), removedId)
      
      idId <- setdiff(rn, abolId)
      idI <- match(idId, rn)
      idJ <- match(idId, cn)
      stopifnot(!is.na(idI))
      stopifnot(!is.na(idJ))
      
      logdebug('%s: (%s)->(%s)', m$mMutationId[1], format(m$mHistId.x), format(m$mHistId.y))
      trI <- match(m$mHistId.x, rn)
      trJ <- match(m$mHistId.y, cn)
      
      stopifnot(!is.na(trI))
      stopifnot(!is.na(trJ))
      logdebug('%s: ((%s))->((%s))', m$mMutationId[1], format(trI), format(trJ))
      logdebug('%s: %s x %s', m$mMutationId[1], length(rn), length(cn))
      g <- sparseMatrix(c(idI, trI), c(idJ, trJ), x=1, dimnames=list(rn, cn))
      
      logdebug('%s: %s %%*%% %s', m$mMutationId[1], dim(f), dim(g))
      f <<- f %*% g
      logdebug('%s: %s', m$mMutationId[1], dim(f))
      
      data.frame(rows=length(rn), columns=length(cn))
    }
  )
  
  ff <- summary(f)
  ff$from <- as.numeric(rownames(f)[ff$i])
  ff$to <- as.numeric(colnames(f)[ff$j])
  ff <- arrange(ff, from)
  ff$i <- NULL
  ff$j <- NULL
  ff$x <- NULL
  as.data.frame(ff)
}

getMunicipalityMappingForMutationIds <- function(from, to) {
  getMunicipalityMappingWorker(
    getMunicipalityIdList(from), from,
    getMunicipalityIdList(to), to
  )
}

})
