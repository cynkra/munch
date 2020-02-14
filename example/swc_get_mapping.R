data(SwissPop)
data(SwissBirths)

# Show mismatch of municipality IDs:
ids.from <- with(SwissPop, MunicipalityID)
ids.to <- with(SwissBirths, MunicipalityID)
setdiff(ids.from, ids.to)
setdiff(ids.to, ids.from)

# Compute mapping and count non-matching municipality IDs:
mapping <- swc_get_mapping(ids.from=ids.from, ids.to=ids.to)
with(mapping, sum(mapping$mIdAsNumber.from != mapping$mIdAsNumber.to))

# Communes that are "missing" are mostly lakes and other special communes:
subset(mapping, MatchType == "missing")[, c("mIdAsNumber.from", "mShortName.from")]

# These should be looked at in some detail, and fixed manually:
subset(mapping, !(MatchType %in% c("valid", "missing")))

# Test for injectivity. The result shows that the mapping is almost injective,
# only one "from" commune is mapped to more than one other "to" commune.
# This situation requires further examination.
mapping.dupes <- subset(mapping, duplicated(mIdAsNumber.from))
(noninjective.mapping <- subset(
  mapping, mIdAsNumber.from %in% mapping.dupes$mIdAsNumber.from))

# Simple treatment (just for this example): Remove duplicates, and use only
# valid matches:
cleaned.mapping <- subset(mapping,
                          !duplicated(mIdAsNumber.from) & MatchType == "valid")

# Now merge the two datasets based on the mapping table:
SwissPop.1970 <- subset(SwissPop, Year == "1970")
SwissPopMapping.1970 <- merge(SwissPop.1970,
                              cleaned.mapping[, c("mId.from", "mId.to")],
                              by.x = "MunicipalityID", by.y = "mId.from")

# Datasets from the "from" table must be suitably aggregated.  For the given
# case of population totals we use the sum.
SwissPopMapping.1970.agg <- plyr::ddply(
  SwissPopMapping.1970, c("mId.to", "HouseholdSize"),
  plyr::summarize, Households=sum(Households))
with(SwissPopMapping.1970.agg, stopifnot(
  length(unique(mId.to)) * length(levels(HouseholdSize)) ==
    length(mId.to)))

# The aggregated "from" dataset now can be merged with the "to" dataset:
SwissBirths.1970 <- subset(SwissBirths, Year == "1970")
SwissPopBirths.1970 <- merge(SwissPopMapping.1970.agg, SwissBirths.1970,
                             by.x = "mId.to", by.y = "MunicipalityID")

# Some more communes are still missing from the 1970 statistics, although
# the matches are valid:
subset(mapping, mIdAsNumber.to %in% setdiff(
  SwissPopMapping.1970.agg$mId.to, SwissBirths.1970$MunicipalityID))[,
    c("mId.from", "mShortName.from", "MatchType")]

# The "from" list must be from an earlier time than the "to" list.
try(swc_get_mapping(ids.from=ids.to, ids.to=ids.from))
