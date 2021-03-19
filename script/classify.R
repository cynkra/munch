library(tidyverse)
library(here)

library(tidyverse)
pkgload::load_all()

mut <- swc_get_mutations()

mut %>%
  filter(is.na(mAbolitionMode), mAdmissionMode == "Change of area") %>%
  glimpse()
count(mMutationNumber, mAbolitionMode, mAdmissionMode) %>%
  arrange(mMutationNumber, mAbolitionMode, mAdmissionMode) %>%
  group_by(mMutationNumber) %>%
  summarize(
    mMode = paste0(mAdmissionMode, "+", mAbolitionMode, "-", format(n, width = 2), collapse = " | ")
  ) %>%
  ungroup() %>%
  count(mMode) %>%
  count(wt = n)

nest(data = -c(mMutationNumber, cAbbreviation, mAdmissionDate, mAbolitionDate)) %>%
  mutate(n = map_int(data, nrow))
