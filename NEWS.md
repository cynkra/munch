0.0-4 (2014-03-25)
===

- New function `swcGetMapping()` that computes a mapping between two lists
  of municipality IDs, with runnable example

- New datasets `SwissBirths` and `SwissPop`

- New function `swcGetMutations()` that computes a list of municipality
  mutations from a list of municipality states

- New function `swcCheckData()` that checks the commune data for consistency

0.0-3 (2014-03-19)
===

- New function `swcGetData()` that reads commune data from
  the web only once and reuses cached results for forthcoming calls

- Rename function `read_historicized_commune_data()` to `swcReadData()`

0.0-2 (2014-03-18)
===

- New function `read_historicized_commune_data()` to download and import
  historicized commune data from the BfS server
