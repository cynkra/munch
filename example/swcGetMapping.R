data(SwissPop)
data(SwissBirths)

# Show mismatch of municipality IDs:
ids.from <- with(SwissPop, MunicipalityID)
ids.to <- with(SwissBirths, MunicipalityID)
setdiff(ids.from, ids.to)
setdiff(ids.to, ids.from)

# Compute mapping and count non-matching municipality IDs:
mapping <- swcGetMapping(ids.from=ids.from, ids.to=ids.to)
with(mapping, sum(mapping$mIdAsNumber.from != mapping$mIdAsNumber.to))

# These should be looked at in some detail:
subset(mapping, !(MatchType %in% c("valid", "missing")))

# Test for injectivity. The result shows that the mapping is almost injective,
# only one "from" commune is mapped to more than one other "to" commune.
# This situation requires further examination.
valid.mapping <- subset(mapping, MatchType == "valid")
valid.mapping.dupes <- subset(valid.mapping, duplicated(mId.from))
(noninjective.mapping <- subset(valid.mapping, mId.from %in% valid.mapping.dupes$mId.from))
