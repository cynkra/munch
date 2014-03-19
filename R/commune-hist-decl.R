local({

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
