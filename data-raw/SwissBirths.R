library(pxR)
vz <- read.px("orig-data/px-d-01-2D02.px")
vzd <- as.data.frame(vz)
vzds <- subset(vzd, Geschlecht.des.Kindes == "Total" & Staatsangehörigkeit.des.Kindes == "Total" & Altersklasse.der.Mutter == "Total")
vzdm <- subset(vzds, grepl('^[.][.][.][.][.][.]([0-9]+).*$', Kanton.......Gemeinde.........))
vzdp <- transform(vzdm,
                  MunicipalityID=as.numeric(gsub('^[.][.][.][.][.][.]([0-9]+) (.*)$', '\\1', Kanton.......Gemeinde.........)),
                  MunicipalityName=gsub('^[.][.][.][.][.][.]([0-9]+) (.*)$', '\\2', Kanton.......Gemeinde.........))
summary(vzdp)
SwissBirths <- with(vzdp, data.frame(
  Year = Jahr,
  MunicipalityID = factor(MunicipalityID, levels = unique(MunicipalityID)),
  MunicipalityName = factor(MunicipalityName, levels = unique(MunicipalityName)),
  Births = value)
  )
with(SwissBirths, stopifnot(as.numeric(MunicipalityID) == as.numeric(MunicipalityName)))
SwissBirths <- arrange(SwissBirths, Year, MunicipalityID)
save(SwissBirths, file="data/SwissBirths.rda", compress="xz")
summary(SwissBirths)
str(SwissBirths)
