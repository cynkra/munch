# SwissCommunes 0.0.8.9000

- function 'get_mun_hist_lookup()': get historic municipalities contributing to modern municipalities on desired time-grid
- internal function 'get_mun_hist()' for collecting historicized municipality IDs
- Removed dependencies: {kimisc}, {plyr}; using {dplyr} instead


# SwissCommunes 0.0.8

- Renamed function `swcGetMapping()` -> `swc_get_mapping()` (old function soft-deprecated)
- Renamed function `swcGetMutations()` -> `swc_get_mutations()` (old function soft-deprecated)
- Added function `swc_get_mun_history()`
- Added function `swc_get_mun_merges()`


# SwissCommunes 0.0.7.9000

- Use tic.


# SwissCommunes 0.0-7 (2016-04-01)

- Use data from `SwissHistMunData` package.
- Remove `swcGetData()`, `swcCheckData()` and `swcReadData()`.
- Fix Travis-CI errors.


# SwissCommunes 0.0-6 (2015-11-05)

- Use integers (instead of numerics) for IDs. Factor input also works.
- Mapping is now computed slightly faster
- Data are reloaded from the web if current month or package version changes.

# SwissCommunes 0.0-5 (2014-09-22)

- Remove parameter municipalityIds to `swcGetMutations()`

- Improve computation of fitness to properly include abolished communes

- Adapt `swcReadData()` to new data format

# SwissCommunes 0.0-4 (2014-03-25)

- New function `swcGetMapping()` that computes a mapping between two lists
  of municipality IDs, with runnable example

- New datasets `SwissBirths` and `SwissPop`

- New function `swcGetMutations()` that computes a list of municipality
  mutations from a list of municipality states

- New function `swcCheckData()` that checks the commune data for consistency

# SwissCommunes 0.0-3 (2014-03-19)

- New function `swcGetData()` that reads commune data from
  the web only once and reuses cached results for forthcoming calls

- Rename function `read_historicized_commune_data()` to `swcReadData()`

# SwissCommunes 0.0-2 (2014-03-18)

- New function `read_historicized_commune_data()` to download and import
  historicized commune data from the BfS server
