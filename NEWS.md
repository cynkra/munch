<!-- NEWS.md is maintained by https://fledge.cynkra.com, contributors should not edit this file -->

# munch 0.0.8.9011 (2025-11-17)

## Continuous integration

- Install binaries from r-universe for dev workflow (#102).


# munch 0.0.8.9010 (2025-11-12)

## Continuous integration

- Fix reviewdog and add commenting workflow (#101).


# munch 0.0.8.9009 (2025-11-10)

## Continuous integration

- Use workflows for fledge (#100).

- Sync (#99).

- Use reviewdog for external PRs (#98).

- Cleanup and fix macOS (#97).

- Format with air, check detritus, better handling of `extra-packages` (#96).

- Enhance permissions for workflow (#95).

- Permissions, better tests for missing suggests, lints (#94).

- Only fail covr builds if token is given (#93).

- Always use `_R_CHECK_FORCE_SUGGESTS_=false` (#92).

- Correct installation of xml2 (#91).

- Explain (#90).

- Add xml2 for covr, print testthat results (#89).

- Fix (#88).

- Sync (#87).


# munch 0.0.8.9008 (2024-12-12)

## Chore

- Add Aviator configuration.

- Update maintainer e-mail address.

## Continuous integration

- Avoid failure in fledge workflow if no changes (#84).

- Fetch tags for fledge workflow to avoid unnecessary NEWS entries (#83).

- Use larger retry count for lock-threads workflow (#82).

- Ignore errors when removing pkg-config on macOS (#81).

- Explicit permissions (#80).

- Use styler from main branch (#79).

- Need to install R on Ubuntu 24.04 (#78).

- Use Ubuntu 24.04 and styler PR (#76).

- Correctly detect branch protection (#75).

- Use stable pak (#74).

- Trigger run (#73).

- Use pkgdown branch (#72).

- Install via R CMD INSTALL ., not pak (#71).

- Install local package for pkgdown builds.

- Improve support for protected branches with fledge.

- Improve support for protected branches, without fledge.

- Sync with latest developments.

- Use v2 instead of master.

- Import from actions-sync, check carefully.

- Import from actions-sync, check carefully.

## Documentation

- Set BS version explicitly for now.

  https://github.com/cynkra/cynkratemplate/issues/53

## Uncategorized

- Ci: Fix macOS (#16) (#77).

- Merge pull request #65 from cynkra/f-add-2023-data.

- Merge pull request #63 from cynkra/docs-pkgdown-cynkra.


# munch 0.0.8.9007 (2022-05-23)

- Automatic data update to 2022-05-01 (#58).
- Fix extraction from Zip archive.


# munch 0.0.8.9006 (2022-02-17)

- Automatically update mutation data on GitHub Actions (#56).
- Use most recent mutation data from 2022 (#56).
- Update tests to use testthat 3e (#51).
- Fix tests and compatibility with dev dplyr (#49).
- new internal function to download lists of municipalities by date via an API provided by BFS.
- Rename to munch (#21).
- Fix filter in `swc_get_mapping()` for correct results (#39).
- `swc_read_data()` now always fetches the most recent dataset (#20).


# SwissCommunes 0.0.8.9005 (2021-03-20)

- Full internal consistency (#23).


# SwissCommunes 0.0.8.9004 (2021-03-19)

- Improve internal consistency (#23).


# SwissCommunes 0.0.8.9003 (2021-03-19)

- Update data to 2021.


# SwissCommunes 0.0.8.9002 (2021-03-19)

- Fix off-by-one error that affects mutations in the middle of the year.
- Write compact and flat tables to main branch.
- The `cantons`, `district_mutations` and `municipality_mutations` datasets are no longer exported.
- Fix loss of municipality mappings in the presence of area changes.


# SwissCommunes 0.0.8.9001

- New `swc_get_municipality_state()` for retrieving the valid municipalities at a given year.
- New `swc_get_merger_mapping_table()` for computing mapping tables to a given target year.


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
