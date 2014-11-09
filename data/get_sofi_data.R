########################################################################
## Title: This script gets the suite of indicator used in SOFI
## Date: 2014-11-05
########################################################################

meta.df = read.csv(file = "meta.csv")
requiredVariables = meta.df[meta.df$SOFI, "VARIABLE"]
requiredYear = 1995:2013

load(file = "final.RData")
fsiSubYear.df =
    fsi.df[fsi.df$Year %in% requiredYear,
           c("FAOST_CODE", "Year", requiredVariables)]
write.csv(fsiSubYear.df, file = "sofi_data.csv",
          row.names = FALSE, na = "")
