# The download via the BFS API with `load_bfs_mun_list()` only includes "real" municipalities
# The result of `swc_get_merger_mapping_table()` also includes 6 exceptions:
# 1. The only land area which is not part of a municipality: "Staatswald Galm" (BFS-Nr: 2391, https://de.wikipedia.org/wiki/Staatswald_Galm)
# 2. Up to 5 "Kommunanzen", areas which belong to more than one municipality (https://de.wikipedia.org/wiki/Kommunanz):
#    a) Kommunanz_Cadenazzo/Monteceneri, BFS-Nr: 5391, https://de.wikipedia.org/wiki/Kommunanz_Cadenazzo/Monteceneri
#    b) Kommunanz Capriasca/Lugano: BFS-Nr: 5394, https://de.wikipedia.org/wiki/Kommunanz_Capriasca/Lugano
#    c) Kommunanz_Bidogno/Capriasca/Corticiasca: until 2008-04-20, BFS-Nr: 5392, https://de.wikipedia.org/wiki/Kommunanz_Bidogno/Capriasca/Corticiasca
#    d) Kommunanz Capriasca/Lugaggia: until 2008-04-20, BFS-Nr: 5393, https://de.wikipedia.org/wiki/Kommunanz_Capriasca/Lugaggia
#    e) Kommunanz Reckingen-Gluringen/Grafschaft: until 2017-01-01, BFS-Nr: 6391, https://de.wikipedia.org/wiki/Kommunanz_Reckingen-Gluringen/Grafschaft
# Whereas the names of the "Kommunanzen" changed since 2005, the municipality IDs stayed the same.
# For Staatswald Galm both the name and the municipality ID have been constant.
#
# For the tests we remove the three exceptions from the result of `swc_get_merger_mapping_table()` and only then do the comparison

mapping_2005 <- swc_get_merger_mapping_table(2005, 2005) %>%
  filter(!(mun_id_y %in% c(2391, 5391, 5392, 5393, 5394, 6391)))
bfs_2005 <- load_bfs_mun_list(2005)

mapping_2012 <- swc_get_merger_mapping_table(2012, 2012) %>%
  filter(!(mun_id_y %in% c(2391, 5391, 5394, 6391)))
bfs_2012 <- load_bfs_mun_list(2012)

mapping_2020 <- swc_get_merger_mapping_table(2020, 2020) %>%
  filter(!(mun_id_y %in% c(2391, 5391, 5394)))
bfs_2020 <- load_bfs_mun_list(2020)


test_that("mapping tables' inventory of municipalities are complete", {

  # TEST 2005 DATA:
  expect_identical(
    sort(mapping_2005$mun_id_y),
    sort(bfs_2005$mun_id)
  )

  # TEST 2012 DATA:
  expect_identical(
    sort(mapping_2012$mun_id_y),
    sort(bfs_2012$mun_id)
  )

  # TEST 2020 DATA:
  expect_identical(
    sort(mapping_2020$mun_id_y),
    sort(bfs_2020$mun_id)
  )
})

test_that("swc_get_municipality_state() works", {
  expect_identical(
    swc_get_municipality_state(2005) %>%
      filter(!(mun_id %in% c(2391, 5391, 5392, 5393, 5394, 6391))) %>%
      pull(mun_id),
    bfs_2005 %>%
      pull(mun_id)
  )


  expect_identical(
    swc_get_municipality_state(2020) %>%
      filter(!(mun_id %in% c(2391, 5391, 5394))) %>%
      pull(mun_id),
    bfs_2020 %>%
      pull(mun_id)
  )
})

