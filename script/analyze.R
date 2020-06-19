#' ---
#' output: html_notebook
#' ---

library(tidyverse)
pkgload::load_all()

#' ## Irreguläre Mutationen
irregular <-
  swc_get_mutations() %>%
  add_count(mMutationNumber, mId.x) %>%
  filter(between(n, 2, 10)) %>%
  group_by(mMutationNumber, cAbbreviation, mDateOfChange.x) %>%
  summarize(desc = paste(unique(c(mShortName.x, mShortName.y)), collapse = ", ")) %>%
  ungroup()

irregular

#' ## Details zu irregulären Mutationen
irregular %>%
  transmute(mMutationNumber, title = paste0(mDateOfChange.x, ": ", desc)) %>%
  left_join(swc_get_mutations(), by = "mMutationNumber") %>%
  count(mMutationNumber, title, mAbolitionMode, mAdmissionMode) %>%
  nest(x = -title) %>%
  deframe()

#' ## Irreguläre Mutationen, reine Flächenänderungen
#'
#' Ausschliesslich Umverteilungen von einer Gemeinde zur anderen.
#'
#' Frage: wie viel Fläche betrifft das?
#' Sind das alles insignifikante Weiler?
#' Oder gab es grössere Abspaltungen?

irregular %>%
  transmute(mMutationNumber, title = paste0(mDateOfChange.x, ": ", desc)) %>%
  left_join(swc_get_mutations(), by = "mMutationNumber") %>%
  group_by(mMutationNumber, title) %>%
  mutate(interesting = sum(mAbolitionMode != "Change of area") + sum(mAdmissionMode != "Change of area")) %>%
  ungroup() %>%
  filter(interesting == 0) %>%
  group_by(mMutationNumber, title) %>%
  summarize(
    nShortName.x = length(unique(mShortName.x)),
    nShortName.y = length(unique(mShortName.y))
  ) %>%
  ungroup()

#' ## Irreguläre Mutationen ohne reine Flächenänderungen
irregular %>%
  transmute(mMutationNumber, title = paste0(mDateOfChange.x, ": ", desc)) %>%
  left_join(swc_get_mutations(), by = "mMutationNumber") %>%
  group_by(mMutationNumber, title) %>%
  mutate(interesting = sum(mAbolitionMode != "Change of area") + sum(mAdmissionMode != "Change of area")) %>%
  ungroup() %>%
  filter(interesting > 0) %>%
  nest(x = -title) %>%
  deframe()

#' ## Irreguläre Mutationszeitpunkte pro Kanton

irregular %>%
  select(mMutationNumber, desc) %>%
  left_join(swc_get_mutations(), by = "mMutationNumber") %>%
  count(cAbbreviation, mDateOfChange.x, desc)

irregular %>%
  select(mMutationNumber, desc) %>%
  left_join(swc_get_mutations(), by = "mMutationNumber") %>%
  group_by(mMutationNumber, desc) %>%
  mutate(interesting = sum(mAbolitionMode != "Change of area") + sum(mAdmissionMode != "Change of area") > 0) %>%
  ungroup() %>%
  count(cAbbreviation, interesting, mDateOfChange.x, desc) %>%
  arrange(mDateOfChange.x)

irregular %>%
  select(mMutationNumber, desc) %>%
  left_join(swc_get_mutations(), by = "mMutationNumber") %>%
  group_by(mMutationNumber, desc) %>%
  mutate(interesting = sum(mAbolitionMode != "Change of area") + sum(mAdmissionMode != "Change of area") > 0) %>%
  ungroup() %>%
  count(cAbbreviation, interesting, mDateOfChange.x, mMutationNumber, desc) %>%
  group_by(cAbbreviation, interesting) %>%
  slice(n()) %>%
  ungroup() %>%
  arrange(mDateOfChange.x)

#' ## Mutationszeitpunkte pro Kanton
swc_get_mutations() %>%
  count(cAbbreviation, mDateOfChange.x)

#' ## Mutationstypen
swc_get_mutations() %>%
  count(mAdmissionMode, mAbolitionMode, sort = TRUE)

#' ## Bezirks- oder Kantonswechsel
swc_get_mutations() %>%
  filter(mAdmissionMode == "Reassignment to other d/c") %>%
  count(cAbbreviation, mDateOfChange.x)

#' ## Umnummerierungen
swc_get_mutations() %>%
  filter(mAdmissionMode == "Formal renumbering") %>%
  count(cAbbreviation, mDateOfChange.x)

swc_get_mutations() %>%
  filter(mAdmissionMode == "Formal renumbering") %>%
  filter(mId.x != mId.y)

#' Ist eine umnummerierte Gemeindenummer jemals wiederverwendet worden?

swc_get_mutations() %>%
  add_count(mMutationNumber) %>%
  filter(n == 1) %>%
  count(mAdmissionMode)

swc_get_mutations() %>%
  group_by(mMutationNumber) %>%
  summarize(new = list(setdiff(mId.y, mId.x))) %>%
  ungroup() %>%
  unnest(new) %>%
  pull() %>%
  anyDuplicated()

#' ## Gemeindestände
#'
#' Für alle Mutationen (ohne Flächenabtretungen) ist die Abbildung
#' Gemeindenummer -> Gemeindename eindeutig.
swc_get_mutations() %>%
  filter(mAdmissionMode != "Change of area") %>%
  distinct(mMutationNumber, mId.y, mShortName.y) %>%
  count(mMutationNumber, mId.y) %>%
  count(n)

#' Konsequenz: Für eine Gemeindenummer
#' definiert die zuletzt gültige Gemeindenummer und den Namen


#' Historisierte Gemeindenummern
mutations_filtered %>%
  filter(mHistId.x == mHistId.y)

