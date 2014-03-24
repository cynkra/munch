logging::basicConfig("INFO")

data(SwissPop)
mutIDPop <- plyr::dlply(
  SwissPop,
  c(), #"Jahr",
  function(piece) {
    SwissCommunes:::getMostProbableMutationId(swc=swcGetData(), as.character(piece$MunicipalityID))
  }
)

(mutIDPop <- unique(unlist(mutIDPop)))

data(SwissBirths)
mutIDBirths <- plyr::dlply(
  SwissBirths,
  c(), #"Year",
  function(piece)
    SwissCommunes:::getMostProbableMutationId(swc=swcGetData(), municipalityIds=piece$MunicipalityID),
  .progress="text"
)

mutIDBirths <- unique(unlist(mutIDBirths))


ids.from <- with(SwissPop, MunicipalityID)
ids.to <- with(SwissBirths, MunicipalityID)
setdiff(ids.from, ids.to)
setdiff(ids.to, ids.from)
mapping <- swcGetMapping(ids.from=ids.from, ids.to=ids.to)
with(mapping, sum(mapping$mId.from != mapping$mId.to))
subset(mapping, !(MatchType %in% c("valid", "missing")))
