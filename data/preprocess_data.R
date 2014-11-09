library(data.table)
refreshData = FALSE
if(refreshData){
    source("get_sofi_data.R")
    source("get_poverty_data.R")
}

sofi.dt = data.table(read.csv(file = "sofi_data.csv"))
poverty.dt = data.table(read.csv(file = "poverty_data.csv"))

## Country name table
countryNamesTable =
    data.table(FAOcountryProfile[!is.na(FAOcountryProfile$FAOST_CODE),
                                 c("FAOST_CODE", "SHORT_NAME")])
countryNamesTable[FAOST_CODE == 357,
                    SHORT_NAME := "Taiwan and China"]
countryNamesTable[FAOST_CODE == 351,
                    SHORT_NAME := "China Aggregate"]
countryNamesTable[FAOST_CODE == 206,
                    SHORT_NAME := "former Sudan"]


## Merge all the data
processed.dt = merge(sofi.dt, poverty.dt, by = c("FAOST_CODE", "Year"))
setkeyv(processed.dt, c("FAOST_CODE", "Year"))
processed.dt[, Year := as.integer(Year)]
write.csv(processed.dt, file = "processed.csv", row.names = FALSE,
          na = "")
