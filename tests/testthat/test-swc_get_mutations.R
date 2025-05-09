# FIXME: Do we still need this function?

expected <- tibble(
  mMutationNumber = c(
    1000L, 1000L, 2300L, 2300L,
    2744L, 2744L, 3161L, 3161L, 3298L, 3298L, 3301L, 3432L,
    3432L, 3432L
  ),
  cAbbreviation = c(
    NA, NA, "LU", "LU", "LU", "LU",
    "LU", "LU", "LU", "LU", "LU", "LU", "LU", "LU"
  ),
  mHistId.x = c(
    NA, NA, 11864L, 12976L, 14501L,
    11939L, 12827L, 12706L, 14946L, 12513L, 15379L, 15519L,
    15519L, 15589L
  ),
  mId.x = c(
    NA, NA, 1081L, 1101L, 1081L,
    1087L, 1060L, 1061L, 1081L, 1092L, 1061L, 1081L, 1081L,
    1099L
  ),
  mShortName.x = c(
    NA, NA, "Beromünster",
    "Schwarzenbach", "Beromünster", "Gunzwil", "Littau", "Luzern",
    "Beromünster", "Neudorf", "Luzern", "Beromünster",
    "Beromünster", "Schenkon"
  ),
  mAbolitionMode = c(
    NA_character_, NA_character_,
    "Change of area", "Abolition", "Change of area",
    "Abolition", "Abolition", "Change of area",
    "Change of area", "Abolition",
    "Reassignment to other d/c", "Change of area", "Change of area",
    "Change of area"
  ),
  mAbolitionDate = as.Date(c(
    NA, NA, "2004-08-31",
    "2004-08-31", "2008-12-31", "2008-12-31", "2009-12-31",
    "2009-12-31", "2012-12-31", "2012-12-31", "2012-12-31",
    "2014-12-31", "2014-12-31", "2014-12-31"
  )),
  mDateOfChange.x = as.Date(c(
    NA, NA, "2004-08-31",
    "2004-08-31", "2008-12-31", "2008-12-31", "2009-12-31",
    "2009-12-31", "2012-12-31", "2012-12-31", "2012-12-31",
    "2014-12-31", "2014-12-31", "2014-12-31"
  )),
  mHistId.y = c(
    12706L, 11864L, 14501L, 14501L,
    14946L, 14946L, 15379L, 15379L, 15519L, 15519L, 15600L,
    15663L, 15664L, 15663L
  ),
  mId.y = c(
    1061L, 1081L, 1081L, 1081L,
    1081L, 1081L, 1061L, 1061L, 1081L, 1081L, 1061L, 1081L,
    1099L, 1081L
  ),
  mShortName.y = c(
    "Luzern", "Beromünster",
    "Beromünster", "Beromünster", "Beromünster", "Beromünster",
    "Luzern", "Luzern", "Beromünster", "Beromünster", "Luzern",
    "Beromünster", "Schenkon", "Beromünster"
  ),
  mAdmissionMode = c(
    "First-time registration", "First-time registration",
    "Change of area", "Change of area",
    "Change of area", "Change of area", "Change of area",
    "Change of area", "Change of area", "Change of area",
    "Reassignment to other d/c", "Change of area",
    "Change of area", "Change of area"
  ),
  mAdmissionDate = as.Date(c(
    "1960-01-01", "1960-01-01",
    "2004-09-01", "2004-09-01", "2009-01-01", "2009-01-01",
    "2010-01-01", "2010-01-01", "2013-01-01", "2013-01-01",
    "2013-01-01", "2015-01-01", "2015-01-01", "2015-01-01"
  )),
  mDateOfChange.y = as.Date(c(
    "2009-12-31", "2004-08-31",
    "2008-12-31", "2008-12-31", "2012-12-31", "2012-12-31",
    "2012-12-31", "2012-12-31", "2014-12-31", "2014-12-31",
    "2013-01-01", "2015-01-01", "2015-01-01", "2015-01-01"
  )),
  mMutationDate = as.Date(c(
    "1960-01-01", "1960-01-01",
    "2004-09-01", "2004-09-01", "2009-01-01", "2009-01-01",
    "2010-01-01", "2010-01-01", "2013-01-01", "2013-01-01",
    "2013-01-01", "2015-01-01", "2015-01-01", "2015-01-01"
  )),
  mMutationId = c(
    "1960-01-01.1000",
    "1960-01-01.1000", "2004-09-01.2300", "2004-09-01.2300",
    "2009-01-01.2744", "2009-01-01.2744", "2010-01-01.3161",
    "2010-01-01.3161", "2013-01-01.3298", "2013-01-01.3298",
    "2013-01-01.3301", "2015-01-01.3432", "2015-01-01.3432",
    "2015-01-01.3432"
  )
)

test_that("swc_get_mutations() works", {
  expect_identical(
    swc_get_mutations(c(1061, 1081), canton = "LU") %>%
      mutate(
        mMutationId = as.character(mMutationId),
        mAbolitionMode = as.character(mAbolitionMode),
        mAdmissionMode = as.character(mAdmissionMode)
      ),
    expected
  )
})
