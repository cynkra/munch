#' Functions for working with the historicized list of municipalities of
#' Switzerland
#'
#' The mutations of Swiss municipalities are tracked in a machine-readable
#' mutation list.  This allows guessing the reference time for any given list of
#' municipality IDs, and to construct mappings between two lists of municipality
#' IDs.  The package provides functions to import and work with this mutation
#' list.
#'
#' \tabular{ll}{
#'   Package: \tab SwissCommunes\cr
#'   Type: \tab Package\cr
#'   Version: \tab 0.0-5\cr
#'   Date: \tab 2014-09-22\cr
#'   License: \tab GPL-3\cr
#' }
#'
#' @name SwissCommunes-package
#' @aliases SwissCommunes-package SwissCommunes
#' @docType package
#' @author Kirill Müller
#'
#' Maintainer: Kirill Müller <kirill.mueller@@ivt.baug.ethz.ch>
#'
#' URL: \url{https://github.com/krlmlr/SwissCommunes}
#'
#' Issue tracker: \url{https://github.com/krlmlr/SwissCommunes/issues}
#'
#' @references
#' \url{http://www.bfs.admin.ch/bfs/portal/de/index/infothek/nomenklaturen/blank/blank/gem_liste/02.html}
#'
#' @keywords Switzerland municipalities communes historicized
NULL

#' Total births per municipality in Switzerland between 1969 and 2012
#'
#' Each row contains the number of births for a municipality and
#'   a year.  Not all municipalities are present for each year due to
#'   changes in the definition of the municipalities.
#' @name SwissBirths
#' @docType data
#' @usage SwissBirths
#' @format A data frame with 105058 observations of  4 variables:
#' \preformatted{ $ Year            : Factor w/ 44 levels "1969","1970",..: 1 1 1 1 1 1 1 1 1 1 ...
#'  $ MunicipalityID  : Factor w/ 2485 levels "1","2","3","4",..: 1 2 3 4 5 6 7 8 9 10 ...
#'  $ MunicipalityName: Factor w/ 2485 levels "Aeugst am Albis",..: 1 2 3 4 5 6 7 8 9 10 ...
#'  $ Births          : num  13 174 22 36 20 13 17 4 32 48 ...}
#' @author Swiss Federal Statistical Office
#' @source STAT-TAB, section 01.2 "Bevölkerung/Bevölkerungsstand und -bewegung",
#'   item "Lebendgeburten nach institutionellen Gliederungen, Geschlecht und Staatsangehörigkeit des Kindes und nach Altersklasse der Mutter".
#'
#'   STAT-TAB URL: \url{http://www.pxweb.bfs.admin.ch/Database/German_01\%20-\%20Bev\%C3\%B6lkerung/01.2\%20-\%20Bev\%C3\%B6lkerungsstand\%20und\%20-bewegung/01.2\%20-\%20Bev\%C3\%B6lkerungsstand\%20und\%20-bewegung.asp?lang=1&prod=01&secprod=2&openChild=true}.
#'
#'   File link: \url{http://www.pxweb.bfs.admin.ch/Database/German_01\%20-\%20Bev\%C3\%B6lkerung/01.2\%20-\%20Bev\%C3\%B6lkerungsstand\%20und\%20-bewegung/px-d-01-2D02.px}.
NULL

#' Census population by household size per commune in Switzerland between 1970 and 2000
#'
#' Each row contains the number of households for a municipality,
#'   a year and a household size.
#' @author Swiss Federal Statistical Office
#' @source STAT-TAB, section 40.1 "Eidgenössische Volkszählung/1970--2000",
#'   item "Privathaushalte nach Haushaltsgrösse und Region".
#'
#'   STAT-TAB URL: \url{http://www.pxweb.bfs.admin.ch/Database/German_40\%20-\%20Eidgen\%C3\%B6ssische\%20Volksz\%C3\%A4hlung/40.1\%20-\%201970-2000/40.1\%20-\%201970-2000.asp?lang=1&prod=40&secprod=1&openChild=true}.
#'
#'   File link: \url{http://www.pxweb.bfs.admin.ch/Database/German_40\%20-\%20Eidgen\%C3\%B6ssische\%20Volksz\%C3\%A4hlung/40.1\%20-\%201970-2000/px-d-40-1A01.px}.
#' @name SwissPop
#' @docType data
#' @usage SwissPop
NULL
