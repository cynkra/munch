logging::basicConfig("INFO")

library(pxR)
vz <- read.px("/home/muelleki/Downloads/px-d-40-1A01.px")
vzd <- as.data.frame(vz)
vzdm <- subset(vzd, grepl('^[.][.][.][.][.][.]([0-9]+).*$', Region))
vzdp <- transform(vzdm, MunicipalityID=as.numeric(gsub('^[.][.][.][.][.][.]([0-9]+).*$', '\\1', Region)))
mutIDPop <- plyr::dlply(
  vzdp,
  .(), #"Jahr",
  function(piece) {
    SwissCommunes:::getMostProbableMutationId(swc=swcGetData(), piece$MunicipalityID)
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


ids.from <- with(vzdp, MunicipalityID)
ids.to <- with(SwissBirths, MunicipalityID)
setdiff(ids.from, ids.to)
setdiff(ids.to, ids.from)
mapping <- swcGetMapping(ids.from=ids.from, ids.to=ids.to)
with(mapping, sum(mapping$mId.from != mapping$mId.to))
subset(mapping, !(MatchType %in% c("valid", "missing")))
