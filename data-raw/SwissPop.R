library(pxR)
vz <- read.px("orig-data/px-d-40-1A01.px")
vzd <- as.data.frame(vz)
vzdm <- subset(vzd, grepl('^[.][.][.][.][.][.]([0-9]+).*$', Region))
vzdp <- transform(vzdm,
                  MunicipalityID=as.numeric(gsub('^[.][.][.][.][.][.]([0-9]+) (.*)$', '\\1', Region)),
                  MunicipalityName=gsub('^[.][.][.][.][.][.]([0-9]+) (.*)$', '\\2', Region))
vzdph <- subset(vzdp, Haushaltsgrösse != "Total")
vzdph$Haushaltsgrösse <- factor(vzdph$Haushaltsgrösse, levels=setdiff(levels(vzdph$Haushaltsgrösse), "Total"))
SwissPop <- with(vzdph, data.frame(
  Year=Jahr,
  MunicipalityID = factor(MunicipalityID, levels = unique(MunicipalityID)),
  MunicipalityName = factor(MunicipalityName, levels = unique(MunicipalityName)),
  HouseholdSize = Haushaltsgrösse,
  Households = value)
  )
with(SwissPop, stopifnot(as.numeric(MunicipalityID) == as.numeric(MunicipalityName)))
SwissPop <- arrange(SwissPop, Year, HouseholdSize, MunicipalityID)
save(SwissPop, file="data/SwissPop.rda", compress="xz")
summary(SwissPop)
str(SwissPop)
