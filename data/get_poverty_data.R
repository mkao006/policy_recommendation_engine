########################################################################
## Title: This script gets the poverty related data from the world bank
## Date:  2014-11-05
########################################################################

pov.lst =
    getWDItoSYB(indicator = meta.dt[!SOFI & SOURCE == "raw",
                    VARIABLE])
pov.dt = data.table(merge(pov.lst$entity,
    FAOcountryProfile[, c("ISO2_WB_CODE", "FAOST_CODE")],
    all.x = TRUE))
pov.dt[, `:=`(ISO2_WB_CODE = NULL, Country = NULL)]
write.csv(pov.dt, file = "poverty_data.csv", row.names = FALSE,
          na = "")
