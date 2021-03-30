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
#' Note that the "from" list must be from an earlier time than the "to" list.
#' Trying to compute the mapping the other way round results in an error.
#' This is intentional: As municipalities are usually merged, it makes sense
#' to use the most recent data set as target for the mapping.  This can also be
#' a file with suitable geometries to allow for visualization.
#'
#' @param ids_from A list of "source" municipality IDs, preferably a factor
#' @param ids_to A list of "target" municipality IDs, preferably a factor
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
#' @example example/swc_get_mapping.R
#' @export
swc_get_mapping <- function(ids_from, ids_to) {
  all.ids <- c(tid(ids_from), tid(ids_to))

  municipality_mutations <- swc_get_municipality_mutations() %>%
    mutate(
      mHistId = as.integer(mHistId),
      dHistId = as.integer(dHistId),
      mId = as.integer(mId)
      )

  mutations <- swc_get_mutations(mids = all.ids)

  #' @details
  #' For two lists of municipalities, we construct a mapping from the first list
  #' to the second.  First, the most probable mutation number in the
  #' "municipality mutations" data set is computed.
  ids_from <- sort(unique(ids_from))
  ids_from.int <- tid(ids_from)
  mid.from <- getMostProbableMutationId(mutations, ids_from.int)
  hist.list.from <- getHistIdList(mutations, mid.from)

  ids_to <- sort(unique(ids_to))
  ids_to.int <- tid(ids_to)
  mid.to <- getMostProbableMutationId(mutations, ids_to.int)
  hist.list.to <- getHistIdList(mutations, mid.to)

  ret <- getMunicipalityMappingWorker(mutations, hist.list.from, mid.from, hist.list.to, mid.to)

  mt <- function(c) factor(c, levels = c("valid", "missing", "extra"))
  cn <- function(name, n) paste(name, n, sep = ".")

  resultTable <- function(histId, inId, name) {
    ret <- municipality_mutations[
      match(histId, municipality_mutations$mHistId),
      c("mHistId", "cAbbreviation", "mId", "mLongName", "mShortName")
    ]
    ret$MatchType <- mt(ifelse(ret$mId %in% inId, "valid", "missing"))
    names(ret) <- cn(names(ret), name)
    ret
  }

  extraTable <- function(retId, inId, name) {
    mId <- sort(setdiff(inId, retId))
    if (length(mId) == 0) {
      return(NULL)
    }
    ret <- data.frame(mId = mId)
    ret <- transform(ret, MatchType = mt("extra"))
    names(ret) <- cn(names(ret), name)
    ret
  }

  fixFactor <- function(ret, inId, name) {
    nameId <- paste0("mId.", name)
    nameIdAsNumber <- paste0("mIdAsNumber.", name)
    if (is.factor(inId)) {
      ret[[nameIdAsNumber]] <- ret[[nameId]]
      ret[[nameId]] <- factor(ret[[nameId]], levels = levels(inId))
    }

    ret
  }

  ret.from <- resultTable(ret$from, ids_from.int, "from")
  ret.to <- resultTable(ret$to, ids_to.int, "to")
  ret <- cbind(ret.from, ret.to)
  ret <- bind_rows(
    ret,
    extraTable(ret.from$mId.from, ids_from.int, "from"),
    extraTable(ret.to$mId.to, ids_to.int, "to")
  )
  ret <- fixFactor(ret, ids_from, "from")
  ret <- fixFactor(ret, ids_to, "to")

  dMatchType <- c(
    `valid.valid` = "valid",
    `missing.missing` = "missing",
    `missing.valid` = "missing.from",
    `extra.NA` = "extra.from",
    `valid.missing` = "missing.to",
    `NA.extra` = "extra.to"
  )
  ret$MatchType <- factor(
    dMatchType[paste(ret$MatchType.from, ret$MatchType.to, sep = ".")],
    levels = dMatchType
  )
  ret
}

tid <- function(ids) {
  if (is.factor(ids)) ids <- as.character(ids)
  ids <- as.integer(ids)
  ids
}

getMostProbableMutationId <- function(mutations, municipalityIds) {
  fitness <- getMunicipalityIdFitness(mutations, municipalityIds)
  logging::logdebug("fitness:\n%s", head(arrange(fitness, desc(fitness))$fitness, 10))
  logging::logdebug("mutationId:\n%s", head(arrange(fitness, desc(fitness))$mMutationId, 10))
  fitness.max <- which.max(fitness$fitness)
  ret <- fitness$mMutationId[fitness.max]
  logging::logdebug("getMostProbableMutationId: %s", ret)
  ret
}

getMunicipalityIdFitness <- function(mutations, municipalityIds) {
  mun.mut <- mutations
  computeFitness(mun.mut, municipalityIds)
}

seq_by <- function(data, varname) {
  data <- arrange(data, !!ensym(varname))
  lengths <- rle(data[[varname]])$lengths
  var_seq <- unlist(lapply(lengths, seq_len))

  data[["seq"]] <- var_seq
  data
}

computeMunList <- function(mun.mut.m) {
  logging::loginfo("computeMunList")
  # The .y values of the argument contain the newly added, the .x values
  # the dropped column identifiers.  This function verifies that after each
  # .y value there is a corresponding .x value, and vice versa, and that the
  # mutation IDs are in nondecreasing order.  The return value is the list of
  # municipality IDs after all transformations have been applied.
  #
  # Work around performance issues in plyr:
  mun.mut.m$mMutationId <- with(mun.mut.m, as.integer(mMutationId))
  #
  # Split input:
  y <- subset(mun.mut.m, grepl("[.]y$", get("variable")))
  x <- subset(mun.mut.m, grepl("[.]x$", get("variable")))
  stopifnot(nrow(x) + nrow(y) == nrow(mun.mut.m))
  #
  # Assign to each x even sequence values, to each y odd sequence values:
  xs <- seq_by(x, "value")
  xs$seq <- xs$seq * 2L
  ys <- seq_by(y, "value")
  ys$seq <- ys$seq * 2L - 1L
  #
  # Mingle, order by municipality (=value) and sequence number:
  xys <- arrange(bind_rows(xs, ys), value, seq)
  #
  # Municipality groups: Compute "group change points" and end of group:
  xys.dvg <- diff(xys$value) > 0
  xys.endgroup <- c(xys.dvg, TRUE)
  #
  # Mutation IDs must be nondecreasing, and sequence numbers must be increasing
  # by one in each group (i.e., everywhere except perhaps at group boundaries)
  xys.dMutationId <- diff(xys$mMutationId)
  xys.dseq <- diff(as.integer(xys$seq))
  stopifnot(xys.dMutationId[!xys.dvg] >= 0)
  stopifnot(xys.dseq[!xys.dvg] == 1)
  #
  # Return value: All municipalities that have not expired at the end of their
  # group.
  subset(xys[xys.endgroup, c("seq", "value")], seq %% 2 == 1)$value
}

meltMutations <- function(mun.mut, hist) {
  logging::loginfo("meltMutations")
  # The list of mutations is processed in the order of the mutation ID, which
  # is composed of mutation date and mutation number. (Conversation with Ernst
  # Oberholzer end of March 2013.)  All records with the same mutation ID
  # form a mutation.
  measure.vars <- paste0(if (hist) "mHistId" else "mId", ".", c("x", "y"))

  mun.mut.m <- reshape2::melt(
    mun.mut,
    id.vars = "mMutationId",
    measure.vars = measure.vars,
    na.rm = TRUE
  )
  mun.mut.m <- arrange(
    mun.mut.m,
    mMutationId, variable
  )
  unique(mun.mut.m)
}

computeFitness <- function(mun.mut, municipalityIds) {
  mun.mut.m <- meltMutations(mun.mut, hist = F)

  mun.mut.m <- dplyr::mutate(
    mun.mut.m,
    dir = ifelse(grepl("[.]y$", variable), 1L, -1L),
    desired = ifelse(value %in% municipalityIds, 1L, -1L),
    delta = dir * desired
  )

  mun.mut.c <- reshape2::dcast(
    data = mun.mut.m, formula = mMutationId ~ .,
    fun.aggregate = sum,
    value.var = "delta"
  )

  mun.mut.c$fitness <- cumsum(mun.mut.c$.)
  mun.mut.c[, c("mMutationId", "fitness")]
}

getHistIdList <- function(mutations, mutationId) {
  mun.mut <- subset(
    mutations, get("mMutationId") <= mutationId
  )
  mun.mut.m <- meltMutations(mun.mut, hist = T)
  computeMunList(mun.mut.m)
}

getMunicipalityMappingWorker <- function(mutations, hist.list.from, mid.from, hist.list.to, mid.to) {
  # Here, we receive the most probable mutation number in the "municipality
  # mutations" data set as parameter.

  # We assume that the first list is older than the second list:
  stopifnot(mid.from <= mid.to)

  # Then we iterate over all mutations.  We must cover all mutations,
  # because a mutation A -> A' -> B would be lost otherwise if A'
  # is not in either municipality list.

  mun.mut <- mutations

  # The mapping will be represented as a linear transformation, encoded
  # as a sparse matrix with rows as "from" and columns as "to" municipalities.
  # Each mutation is converted to such a matrix, the matrix product of
  # all mutations will be the final mapping.  We start with a unit
  # matrix (the identity transformation) of all "from" columns:

  f <- Matrix::sparseMatrix(
    i = seq_along(hist.list.from),
    j = seq_along(hist.list.from),
    x = 1,
    dimnames = list(hist.list.from, hist.list.from)
  )

  # Subsequently, this transformation is augmented with all mutations
  # between "from" and "to"
  trans.list_prep <- dplyr::filter(
    mun.mut,
    as.numeric(mid.from) < as.numeric(mMutationId),
    as.numeric(mid.to) >= as.numeric(mMutationId)
  )

  stopifnot(!anyNA(trans.list_prep$mHistId.y))

  trans.list <- if (nrow(trans.list_prep) == 0) {
    trans.list_prep
  } else {
    group_split(trans.list_prep, mMutationId) %>%
      map_dfr(function(m) {
        stopifnot(!is.na(m$mHistId.y))
        rn <- colnames(f)

        abolId <- unique(m$mHistId.x)
        admId <- unique(m$mHistId.y)
        abolId <- subset(abolId, !is.na(abolId))
        admId <- subset(admId, !is.na(admId))
        logging::logdebug(
          "%s: +(%s), -(%s)",
          m$mMutationId[1],
          format(admId),
          format(abolId)
        )

        removedId <- setdiff(abolId, admId)
        remainingId <- setdiff(abolId, removedId)
        addedId <- sort(setdiff(admId, abolId))
        logging::logdebug(
          "%s: ++(%s), =(%s), --(%s)",
          m$mMutationId[1],
          format(addedId),
          format(remainingId),
          format(removedId)
        )

        stopifnot(sort(remainingId) == sort(setdiff(admId, addedId)))

        cn <- setdiff(c(rn, addedId), removedId)

        idId <- setdiff(rn, abolId)
        idI <- match(idId, rn)
        idJ <- match(idId, cn)
        stopifnot(!is.na(idI))
        stopifnot(!is.na(idJ))

        logging::logdebug(
          "%s: (%s)->(%s)",
          m$mMutationId[1],
          format(m$mHistId.x),
          format(m$mHistId.y)
        )
        trI <- match(m$mHistId.x, rn)
        trJ <- match(m$mHistId.y, cn)

        stopifnot(!is.na(trI))
        stopifnot(!is.na(trJ))
        logging::logdebug(
          "%s: ((%s))->((%s))",
          m$mMutationId[1],
          format(trI),
          format(trJ)
        )
        logging::logdebug("%s: %s x %s", m$mMutationId[1], length(rn), length(cn))
        g <-
          Matrix::sparseMatrix(c(idI, trI),
            c(idJ, trJ),
            x = 1,
            dimnames = list(rn, cn)
          )

        logging::logdebug("%s: %s %%*%% %s", m$mMutationId[1], dim(f), dim(g))
        f <<- f %*% g
        logging::logdebug("%s: %s", m$mMutationId[1], dim(f))
        data.frame(rows = length(rn), columns = length(cn))
      })
  }

  ff <- Matrix::summary(f)
  ff$from <- as.integer(rownames(f)[ff$i])
  ff$to <- as.integer(colnames(f)[ff$j])
  ff <- arrange(ff, from)
  ff$i <- NULL
  ff$j <- NULL
  ff$x <- NULL
  as.data.frame(ff)
}
