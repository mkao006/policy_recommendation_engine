library(zoo)
library(data.table)
library(Amelia)
source("invlogit.R")
source("logit.R")
source("twoPointApprox.R")
processed.dt = data.table(read.csv(file = "processed.csv"))
keyVariable = c("FAOST_CODE", "Year")
indicatorVariable = setdiff(colnames(processed.dt), keyVariable)

setkeyv(processed.dt, c("FAOST_CODE", "Year"))


## Read in the meta-data
meta.dt =
    data.table(read.csv(file = "meta.csv", stringsAsFactors = FALSE))


## This is a temporary solution for irrigation area
processed.dt[, RL.AREA.EQIRR.HA.SHL :=
                 RL.AREA.EQIRR.HA.SHL/max(RL.AREA.EQIRR.HA.SHL,
                                          na.rm = TRUE) * 100]


## Data transformation for imputation
## ---------------------------------------------------------------------

## Scale proportion data
scaleString =
    with(meta.dt,
         paste0("`:=`(",
                paste0(VARIABLE, " = ", VARIABLE, "/", SCALE,
                       collapse = ", "), ")"))
processed.dt[, eval(parse(text = scaleString))]

## Transform data
transformString =
    with(meta.dt[TRANSFORM != "", ],
         paste0("`:=`(",
                paste0(VARIABLE, " = ", TRANSFORM, "(", VARIABLE, ")",
                       collapse = ", "), ")"))

processed.dt[, eval(parse(text = transformString))]



## Imputation Step
## ---------------------------------------------------------------------

## Create bounds for imputation

variableType = sapply(processed.dt, typeof)
boundColumns = which(variableType == "double")
boundUpper =
    sapply(processed.dt[, names(boundColumns), with = FALSE],
           max, na.rm = TRUE) +
    sapply(processed.dt[, names(boundColumns), with = FALSE],
           sd, na.rm = TRUE) * 3
boundLower =
    sapply(processed.dt[, names(boundColumns), with = FALSE],
           max, na.rm = TRUE) -
    sapply(processed.dt[, names(boundColumns), with = FALSE],
           sd, na.rm = TRUE) * 3
boundsMatrix = matrix(c(boundColumns, boundLower, boundUpper),
    byrow = FALSE, nc = 3)
rownames(boundsMatrix) = names(boundColumns)

## lower bounds for untransformed variable
boundsMatrix[meta.dt[TRANSFORM == "" & TYPE == "positive", VARIABLE],
             2] = 0
boundsMatrix[meta.dt[TRANSFORM == "" & TYPE == "proportion", VARIABLE],
             2] = -50
boundsMatrix[meta.dt[TRANSFORM == "" & TYPE == "proportion", VARIABLE],
             3] = 50


interpolated.dt = copy(processed.dt)
interpolated.dt[, `:=`(c(indicatorVariable),
                    lapply(indicatorVariable,
                           FUN = function(x)
                               twoPointApprox(get(x)))),
                by = "FAOST_CODE"]

cbind(
    sapply(processed.dt, function(x)
        round(sum(is.na(x))/length(x), 2) * 100),
    sapply(interpolated.dt, function(x)
        round(sum(is.na(x))/length(x), 2) * 100))

## multiply impute the data set
imputed.am = amelia(interpolated.dt, ts = "Year", cs = "FAOST_CODE",
    lag = c("ADESA", "POPULATION", "NY.GDP.MKTP.CD",
        "SP.RUR.TOTL.ZS", "SH.STA.ACSN"),
    lead = c("ADESA", "POPULATION", "NY.GDP.MKTP.CD", "POU",
        "SP.RUR.TOTL.ZS", "SH.STA.ACSN"),
    splinetime = 3,
    p2s = 1, bounds = boundsMatrix, m = 10)


mi.lst = imputed.am$imputations
for(i in 1:length(mi.lst))
    mi.lst[[i]]$imputation = i


## Combine the multiple imputation by fitting loess
mi = Reduce(f = function(x, y) rbind(x, y), x = mi.lst)

## Merge with name table and set keys
mi = merge(mi, countryNamesTable, by = "FAOST_CODE")
mi = merge(mi,
    data.table(FAOregionProfile[, c("FAOST_CODE", "UNSD_MACRO_REG",
                                    "UNSD_SUB_REG")]), by = "FAOST_CODE")
setkeyv(mi, c("FAOST_CODE", "Year"))

xyplot(SH.STA.WAST.ZS ~ Year|SHORT_NAME, data = mi, groups = imputation,
       auto.key = TRUE)

check1 = copy(processed.dt)
check1[, type := "processed"]
check2 = copy(interpolated.dt)
check2[, type := "interpolated"]
finalCheck = rbind(check1, check2)
xyplot(SH.STA.WAST.ZS ~ Year|FAOST_CODE, data = finalCheck, groups = type)


## combineString =
##     paste0("list(", paste0(paste0(meta.dt[SOURCE == "raw", VARIABLE],
##     "= predict(loess(", meta.dt[SOURCE == "raw", VARIABLE],
##     " ~ Year, span = 0.75, loess.control(surface = 'direct')),",
##     "newdata = data.frame(Year = 1995:2010))"),
##     collapse = ", "), ")")
## imputed.dt = mi[, eval(parse(text = combineString)), by = "FAOST_CODE"]
## imputed.dt[, Year := rep(1995:2010, length(unique(FAOST_CODE)))]

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


## Merge country information to unimputed data for later comparison
final.dt = merge(final.dt, countryNamesTable, by = "FAOST_CODE")
final.dt = merge(final.dt,
    data.table(FAOregionProfile[, c("FAOST_CODE", "UNSD_MACRO_REG",
                                    "UNSD_SUB_REG")]), by = "FAOST_CODE")
final.dt[, BY.GDP.MKTP.PCAP := rep(NA, NROW(final.dt))]


## Back transform the data
## ---------------------------------------------------------------------


backTransformString = paste0("`:=`(",
    paste0(meta.dt[!is.na(TRANSFORM) & SOURCE == "raw", VARIABLE], " = ",
           ifelse(meta.dt[!is.na(TRANSFORM) & SOURCE == "raw",
                          TRANSFORM] == "logit", "logit(", "exp("),
           meta.dt[!is.na(TRANSFORM) & SOURCE == "raw", VARIABLE], ")",
           collapse = ", "), ")")
imputed.dt[, eval(parse(text = backTransformString))]

## Post compute GDP per capita
imputed.dt[, BY.GDP.MKTP.PCAP :=
           NY.GDP.MKTP.CD/POPULATION]

## Reverse scale the data
scaleString = paste0("`:=`(",
    paste0(meta.dt[SCALE != 1, VARIABLE], " = ",
           meta.dt[SCALE != 1, VARIABLE], " * ",
           meta.dt[SCALE != 1, SCALE], collapse = ", "), ")")
imputed.dt[, eval(parse(text = scaleString))]
