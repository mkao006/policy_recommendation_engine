########################################################################
## Title: Data preparation for recommendation engine.
## Date: 2014-04-21
########################################################################

## NOTE (Michael): Download more indicator from the world bank for
##                 imputation and analysis.
##

## NOTE (Michael): Need to have a transformation table, so I can see
##                 the difference of imputation.

## NOTE (Michael): Next step is to examine the distributional
##                 difference between the country with problem and
##                 without.

library(car)
library(zoo)
library(data.table)
library(lattice)
library(FAOSTAT)
library(Amelia)
library(ggplot2)
library(lme4)
refreshPovData = FALSE

## Data preperation step
## ---------------------------------------------------------------------
## Load the data and remove future years
load(file = "final.RData")
fsiSubYear.df = fsi.df[fsi.df$Year %in% c(1995:2013), ]

## Read in the meta-data
meta.dt = data.table(read.csv(file = "meta.csv",
    stringsAsFactors = FALSE))

## Country name table
countryNamesTable =
    data.table(FAOcountryProfile[, c("FAOST_CODE", "FAO_TABLE_NAME")])
countryNamesTable[FAOST_CODE == 357,
                    FAO_TABLE_NAME := "Taiwan and China"]
countryNamesTable[FAOST_CODE == 351,
                    FAO_TABLE_NAME := "China Aggregate"]
countryNamesTable[FAOST_CODE == 107, FAO_TABLE_NAME := "Cote d'Ivoire"]
countryNamesTable[FAOST_CODE == 284, FAO_TABLE_NAME := "Aland Islands"]
countryNamesTable[FAOST_CODE == 279, FAO_TABLE_NAME := "Curacao"]
countryNamesTable[FAOST_CODE == 182, FAO_TABLE_NAME := "Reunion"]
countryNamesTable[FAOST_CODE == 282,
                    FAO_TABLE_NAME := "Saint Barthelemy"]
countryNamesTable[FAOST_CODE == 206,
                    FAO_TABLE_NAME := "former Sudan"]

## Download poverty and GINI data
if(refreshPovData){
    pov.lst =
        getWDItoSYB(indicator = meta.dt[!SOFI & SOURCE == "raw",
                        VARIABLE])
    pov.dt = data.table(merge(pov.lst$entity,
        FAOcountryProfile[, c("ISO2_WB_CODE", "FAOST_CODE")],
        all.x = TRUE))
    pov.dt[, `:=`(ISO2_WB_CODE = NULL, Country = NULL)]
    ## pov.df$ISO2_WB_CODE = NULL
    ## pov.df$Country = NULL
    save(pov.dt, file = "poverty.Rdata")
}

load("poverty.Rdata")
     
## Final unimputed data set
final.dt =
    data.table(fsiSubYear.df[, c("FAOST_CODE", "Year",
                                 meta.dt[SOFI & SOURCE == "raw",
                                         VARIABLE])])
final.dt = merge(final.dt, pov.dt, by = c("FAOST_CODE", "Year"))
setkeyv(final.dt, c("FAOST_CODE", "Year"))
final.dt[, Year := as.integer(Year)]

## Check the sparsity
sapply(final.dt, FUN = function(x) round(sum(is.na(x))/length(x) * 100))

## Check variable type
sapply(final.dt, FUN = function(x) range(x, na.rm = TRUE))

## Make correction to irrigation share of arable land.
final.dt[RL.AREA.EQIRR.HA.SHL >= 100, RL.AREA.EQIRR.HA.SHL := 100]

## Data transformation step:
## ---------------------------------------------------------------------

## Scale proportion data
scaleString = paste0("`:=`(",
    paste0(meta.dt[SCALE != 1, VARIABLE], " = ",
           meta.dt[SCALE != 1, VARIABLE], "/",
           meta.dt[SCALE != 1, SCALE], collapse = ", "), ")")
final.dt[, eval(parse(text = scaleString))]


## Some data transformation for normality and interpretation
invlogit = function(x){
    x = ifelse(x == 1, 1 - 1e-5,
        ifelse(x == 0, 0 + 1e-5, x))
    log(x/(1 - x))
}

transformString = paste0("`:=`(",
    paste0(meta.dt[!is.na(TRANSFORM) & SOURCE == "raw", VARIABLE], " = ",
           ifelse(meta.dt[!is.na(TRANSFORM) & SOURCE == "raw",
                          TRANSFORM] == "logit", "invlogit(", "log("),
           meta.dt[!is.na(TRANSFORM) & SOURCE == "raw", VARIABLE], ")",
           collapse = ", "), ")")
final.dt[, eval(parse(text = transformString))]

## Removing strange countries
final.dt = final.dt[FAOST_CODE != 164, ]
final.dt = final.dt[FAOST_CODE != 186, ]

## plotString =
##     paste0("~ ", paste0(colnames(final.dt)[-c(1, 2)], collapse = " + "))
## scatterplotMatrix(eval(parse(text = plotString)), data = final.dt,
##                   smoother = FALSE, use = "pairwise.complete.obs")


## Imputation Step
## ---------------------------------------------------------------------

## Create bounds for imputation

variableType = sapply(final.dt, typeof)
boundColumns = which(variableType == "double")
boundUpper = sapply(final.dt[, names(boundColumns), with = FALSE],
    max, na.rm = TRUE) +
         2 * sapply(final.dt[, names(boundColumns), with = FALSE], sd,
                    na.rm = TRUE)
boundLower = sapply(final.dt[, names(boundColumns), with = FALSE],
    max, na.rm = TRUE) -
         2 * sapply(final.dt[, names(boundColumns), with = FALSE], sd,
                    na.rm = TRUE)
boundsMatrix = matrix(c(boundColumns, boundLower, boundUpper),
    byrow = FALSE, nc = 3)
rownames(boundsMatrix) = names(boundColumns)

## lower bounds for untransformed variable
boundsMatrix[meta.dt[SOURCE == "raw" &
                     TRANSFORM == "" &
                     TYPE == "positive", VARIABLE],
             2] = 0


## linear interpolation for available data
myApprox = function(x){
    n = length(na.omit(x))
    if(n >= 2){
        tmp = na.approx(x, na.rm = FALSE)
    } else {
        tmp = x
    }
    tmp
}

for(i in colnames(final.dt)[-c(1:2)]){
    final.dt[, eval(parse(text = paste0(i, " := myApprox(", i, ")"))),
             by = "FAOST_CODE"]
}

## multiply impute the data set
imputed.am = amelia(final.dt, ts = "Year", cs = "FAOST_CODE",
    lag = c("ADESA", "POPULATION"), 
    p2s = 1, bounds = boundsMatrix, m = 10)

## Combine the multiple imputation by fitting loess
mi = Reduce(f = function(x, y) rbind(x, y), x = imputed.am$imputations)
combineString =
    paste0("list(", paste0(paste0(meta.dt[SOURCE == "raw", VARIABLE],
    "= predict(loess(", meta.dt[SOURCE == "raw", VARIABLE],
    " ~ Year, span = 0.85, loess.control(surface = 'direct')),",
    "newdata = data.frame(Year = 1995:2010))"),
    collapse = ", "), ")")
imputed.dt = mi[, eval(parse(text = combineString)), by = "FAOST_CODE"]
imputed.dt[, Year := rep(1995:2010, length(unique(FAOST_CODE)))]

## Plot the relationship of the result
## plotString =
##     paste0("~ ", paste0(colnames(imputed.dt)[colnames(imputed.dt) !=
##                                              c("FAOST_CODE", "Year")],
##                         collapse = " + "))
## scatterplotMatrix(eval(parse(text = plotString)), data = imputed.dt,
##                   smoother = FALSE, use = "pairwise.complete.obs")

## Merge with name table and set keys
imputed.dt = merge(imputed.dt, countryNamesTable, by = "FAOST_CODE")
imputed.dt = merge(imputed.dt,
    data.table(FAOregionProfile[, c("FAOST_CODE", "UNSD_MACRO_REG",
                                    "UNSD_SUB_REG")]), by = "FAOST_CODE")
setkeyv(imputed.dt, c("FAOST_CODE", "Year"))

## Post compute GDP per capita
imputed.dt[, BY.GDP.MKTP.PCAP :=
           log(exp(NY.GDP.MKTP.CD)/exp(POPULATION))]


## Merge country information to unimputed data for later comparison
final.dt = merge(final.dt, countryNamesTable, by = "FAOST_CODE")
final.dt = merge(final.dt,
    data.table(FAOregionProfile[, c("FAOST_CODE", "UNSD_MACRO_REG",
                                    "UNSD_SUB_REG")]), by = "FAOST_CODE")
final.dt[, BY.GDP.MKTP.PCAP := rep(NA, NROW(final.dt))]


## Back transform the data
## ---------------------------------------------------------------------

## logistic transformation for proportion
logit = function(x){
    1/(1 + exp(-x))
}


backTransformString = paste0("`:=`(",
    paste0(meta.dt[!is.na(TRANSFORM) & SOURCE == "raw", VARIABLE], " = ",
           ifelse(meta.dt[!is.na(TRANSFORM) & SOURCE == "raw",
                          TRANSFORM] == "logit", "logit(", "exp("),
           meta.dt[!is.na(TRANSFORM) & SOURCE == "raw", VARIABLE], ")",
           collapse = ", "), ")")
imputed.dt[, eval(parse(text = backTransformString))]
save(imputed.dt, file = "imputed.RData")
