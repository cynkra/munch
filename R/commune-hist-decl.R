local({

getMunicipalityMappingForMutationIds <- function(from, to) {
  getMunicipalityMappingWorker(
    getMunicipalityIdList(from), from,
    getMunicipalityIdList(to), to
  )
}

})
