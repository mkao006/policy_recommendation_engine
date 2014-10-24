library(data.table)
library(igraph)

load("../data/imputed.RData")


imputed.dt = na.omit(imputed.dt)
excludeCountry = c("Greenland", "United States Virgin Islands",
    "the Bahamas", "American Samoa", "Puerto Rico", "Barbados",
    "French Polynesia", "Turks and Caicos Islands",
    "the Marshall Islands", "Solomon Islands",
    "Northern Mariana Islands", "Faroe Islands", "Cayman Islands")
imputed.dt = imputed.dt[!FAO_TABLE_NAME%in% excludeCountry, ]
variableIndex = which(!grepl("_", colnames(imputed.dt)))
imputed.dt[, Year := (Year - min(Year))/max((Year - min(Year)))]
zeroHunger.dt =
    data.table(FAOST_CODE = 0, POU = 0,
               Year = unique(imputed.dt$Year),
               FAO_TABLE_NAME = "Zero Hunger",
               UNSD_MACRO_REG = "World", UNSD_SUB_REG = "World")
zeroHunger.dt[, c(setdiff(colnames(imputed.dt),
                          colnames(zeroHunger.dt))) := NA]
imputed.dt = rbind(imputed.dt, zeroHunger.dt)

## Should change this to rank
imputed.dt[, `:=`(colnames(imputed.dt)[variableIndex],
                  lapply(imputed.dt[,
                                    colnames(imputed.dt)[variableIndex],
                                    with = FALSE],
                         FUN = function(x){
                             if(is.numeric(x)){
                                 scaled = x/max(x, na.rm = TRUE)
                             } else {
                                 scaled = x
                             }
                             scaled
                         }))]

calculateDist = function(index, data, statistics){
    data[, statistics, with = FALSE] -
        data[index, statistic, with = FALSE]
}

dist2index = function(data, names, index){
    all = lapply(names,
        FUN = function(x) unlist(data[, x, with = FALSE]))
    one = lapply(names,
        FUN = function(x) unlist(data[index, x, with = FALSE]))
    rowSums(mapply(x = all, FUN = function(x, y) abs(x - y), y = one),
            na.rm = TRUE)
}


dist.lst =
    lapply(1:NROW(imputed.dt),
           FUN = function(x){
               print(x)
               dist =
                   dist2index(imputed.dt,
                              colnames(imputed.dt)[variableIndex], x)
               fromCountry = imputed.dt[x, FAO_TABLE_NAME]
               fromCountryCode = imputed.dt[x, FAOST_CODE]
               toCountry = imputed.dt[, FAO_TABLE_NAME]
               toCountryCode = imputed.dt[, FAOST_CODE]
               fromYear = imputed.dt[x, Year]
               toYear = imputed.dt[, Year]
               fullConnect = 
                   data.table(fromCountry = fromCountry,
                              fromCountryCode = fromCountryCode,
                              fromYear = fromYear,
                              toCountry = toCountry,
                              toCountryCode = toCountryCode,
                              toYear = toYear,
                              distance = dist)
               fullConnect =
                   fullConnect[order(distance, decreasing = FALSE)]
               fullConnect[, from :=
                               paste0(fromCountry, "(", fromYear, ")")]
               fullConnect[, to :=
                               paste0(toCountry, "(", toYear, ")")]
               fullConnect[!duplicated(toCountry), ]
           }
           )

fullNetwork = Reduce(rbind, dist.lst)
node.dt =
    data.table(name = unique(fullNetwork$fromCountry),
               id = 0:(length(unique(fullNetwork$fromCountry)) - 1))
setnames(node.dt, c("name", "id"), c("fromCountry", "fromID"))
fullNetwork = merge(fullNetwork, node.dt, by = "fromCountry")
setnames(node.dt, c("fromCountry", "fromID"), c("toCountry", "toID"))
fullNetwork = merge(fullNetwork, node.dt, by = "toCountry")

write.csv(fullNetwork, file = "../data/fullNetwork.csv",
          row.names = FALSE)
write.csv(imputed.dt, file = "../data/imputed.csv",
          row.names = FALSE)
fullNetwork = read.csv(file = "../data/fullNetwork.csv")

## library(RPostgreSQL)
## drv = dbDriver("PostgreSQL")
## con = dbConnect(drv, user = "mk", dbname = "fullNetwork")
## dbExistsTable(con, "full_network")
## dbWriteTable(con, name = "full_network", value = fullNetwork,
##              overwrite = TRUE)
## dbGetQuery(con, 'SELECT * FROM full_network LIMIT 10')



getSubYearNetwork = function(network, year){
    network[fromYear == year & toYear == year &
            fromCountry != toCountry, ]
}

library(data.table)
network2010 = getSubYearNetwork(data.table(fullNetwork), 1)

tmp = by(network2010, INDICES = network2010$fromCountry,
    FUN = function(x) head(x[order(distance, decreasing = FALSE)], 4))

edge2010 = Reduce(function(x, y) rbind(x, y), tmp)
edge2010 = edge2010[toCountryCode != 0, ]
## edge2010 = network2010[distance < 0.8 & distance > 0, ]

node2010 =
    unique.data.frame(network2010[, list(name = fromCountry,
                                         nameCode = fromCountryCode,
                                         idCode = fromID)])
library(FAOSTAT)
node2010 =
    merge(data.frame(node2010),
          FAOregionProfile[, c("FAOST_CODE", "UNSD_MACRO_REG",
                               "UNSD_SUB_REG", "UNSD_DVDDVG_REG")],
          by.x = "nameCode", by.y = "FAOST_CODE", all.x = TRUE)
node2010[node2010$nameCode == 0,
         c("UNSD_MACRO_REG", "UNSD_SUB_REG", "UNSD_DVDDVG_REG")] =
             rep("World", 3)
node2010 = node2010[order(node2010$idCode), ]

library(d3Network)
d3ForceNetwork(Links = data.frame(edge2010),
               Nodes = node2010,
               Source = "fromID", Target = "toID",
               Value = "distance", NodeID = "name",
               Group = "UNSD_MACRO_REG", zoom = TRUE,
               opacity = 0.9, fontsize = 15,
               linkDistance = "function(d){return d.value}",
               width = 1500, height = 1500, charge = -300,
               file = "network2010.html", d3Script = "d3.v3.min.js")

## Need to check how undernourishment of <5 is determined, I think
## it's based on development.

network2005 = getSubYearNetwork(data.table(fullNetwork), 2/3)

tmp = by(network2005, INDICES = network2005$fromCountry,
    FUN = function(x) head(x[order(distance, decreasing = FALSE)], 4))

edge2005 = Reduce(function(x, y) rbind(x, y), tmp)
edge2005 = edge2005[toCountryCode != 0, ]

node2005 =
    unique.data.frame(network2005[, list(name = fromCountry,
                                         nameCode = fromCountryCode,
                                         idCode = fromID)])
node2005 =
    merge(data.frame(node2005),
          FAOregionProfile[, c("FAOST_CODE", "UNSD_MACRO_REG",
                               "UNSD_SUB_REG", "UNSD_DVDDVG_REG")],
          by.x = "nameCode", by.y = "FAOST_CODE", all.x = TRUE)
node2005[node2005$nameCode == 0,
         c("UNSD_MACRO_REG", "UNSD_SUB_REG", "UNSD_DVDDVG_REG")] =
             rep("World", 3)
node2005 = node2005[order(node2005$idCode), ]

d3ForceNetwork(Links = data.frame(edge2005),
               Nodes = node2005,
               Source = "fromID", Target = "toID",
               Value = "distance", NodeID = "name",
               Group = "UNSD_MACRO_REG", zoom = TRUE,
               opacity = 0.9, fontsize = 15,
               linkDistance = "function(d){return d.value}",
               width = 1500, height = 1500, charge = -300,
               file = "network2005.html", d3Script = "d3.v3.min.js")





## Network for 1995
network1995 = getSubYearNetwork(data.table(fullNetwork), 0)

tmp = by(network1995, INDICES = network1995$fromCountry,
    FUN = function(x) head(x[order(distance, decreasing = FALSE)], 4))

edge1995 = Reduce(function(x, y) rbind(x, y), tmp)

## edge1995 = network1995[distance < 0.8 & distance > 0, ]
node1995 =
    unique.data.frame(network1995[, list(name = fromCountry,
                                         id = fromCountryCode)])

node1995 =
    merge(data.frame(node1995),
          FAOregionProfile[, c("FAOST_CODE", "UNSD_MACRO_REG",
                               "UNSD_SUB_REG")],
          by.x = "id", by.y = "FAOST_CODE")

d3ForceNetwork(Links = data.frame(edge1995),
               Nodes = node2010,
               Source = "fromID", Target = "toID",
               Value = "distance", NodeID = "name",
               Group = "UNSD_MACRO_REG", zoom = TRUE,
               opacity = 0.9, fontsize = 15,
               linkDistance = "function(d){return d.value*000}",
               width = 1500, height = 1500, charge = -300,
               file = "network1995.html")



## Network for 2000
network2000 = getSubYearNetwork(data.table(fullNetwork), 1/3)

tmp = by(network2000, INDICES = network2000$fromCountry,
    FUN = function(x) head(x[order(distance, decreasing = FALSE)], 4))

edge2000 = Reduce(function(x, y) rbind(x, y), tmp)

## edge2000 = network2000[distance < 0.8 & distance > 0, ]
node2000 =
    unique.data.frame(network2000[, list(name = fromCountry,
                                         id = fromCountryCode)])

node2000 =
    merge(data.frame(node2000),
          FAOregionProfile[, c("FAOST_CODE", "UNSD_MACRO_REG",
                               "UNSD_SUB_REG")],
          by.x = "id", by.y = "FAOST_CODE")

d3ForceNetwork(Links = data.frame(edge2000),
               Nodes = node2010,
               Source = "fromID", Target = "toID",
               Value = "distance", NodeID = "name",
               Group = "UNSD_MACRO_REG", zoom = TRUE,
               opacity = 0.9, fontsize = 15,
               linkDistance = "function(d){return d.value*000}",
               width = 1500, height = 1500, charge = -300,
               file = "network2000.html")




## Network for 2005
network2005 = getSubYearNetwork(data.table(fullNetwork), 2/3)

tmp = by(network2005, INDICES = network2005$fromCountry,
    FUN = function(x) head(x[order(distance, decreasing = FALSE)], 4))

edge2005 = Reduce(function(x, y) rbind(x, y), tmp)

## edge2005 = network2005[distance < 0.8 & distance > 0, ]
node2005 =
    unique.data.frame(network2005[, list(name = fromCountry,
                                         id = fromCountryCode)])

node2005 =
    merge(data.frame(node2005),
          FAOregionProfile[, c("FAOST_CODE", "UNSD_MACRO_REG",
                               "UNSD_SUB_REG")],
          by.x = "id", by.y = "FAOST_CODE")



d3ForceNetwork(Links = data.frame(edge2005),
               Nodes = node2010,
               Source = "fromID", Target = "toID",
               Value = "distance", NodeID = "name",
               Group = "UNSD_MACRO_REG", zoom = TRUE,
               opacity = 0.9, fontsize = 15,
               linkDistance = "function(d){return d.value*000}",
               width = 1500, height = 1500, charge = -300,
               file = "network2005.html")





networkTest = getSubYearNetwork(data.table(fullNetwork), 1)


tmp = by(networkTest, INDICES = networkTest$fromCountry,
    FUN = function(x) head(x[order(distance, decreasing = FALSE)], 2))

edgeTest = Reduce(function(x, y) rbind(x, y), tmp)

## edgeTest = networkTest[distance < 0.8 & distance > 0, ]
nodeTest =
    unique.data.frame(networkTest[, list(name = fromCountry,
                                         id = fromCountryCode)])
nodeTest =
    merge(data.frame(nodeTest),
          FAOregionProfile[, c("FAOST_CODE", "UNSD_MACRO_REG",
                               "UNSD_SUB_REG", "UNSD_DVDDVG_REG")],
          by.x = "id", by.y = "FAOST_CODE")



d3ForceNetwork(Links = data.frame(edgeTest),
               Nodes = nodeTest,
               Source = "fromID", Target = "toID",
               Value = "distance", NodeID = "name",
               Group = "UNSD_DVDDVG_REG", zoom = TRUE,
               opacity = 0.9, fontsize = 15,
               linkDistance = "function(d){return d.value*000}",
               width = 1500, height = 1500, charge = -300,
               file = "networkTest.html")



library(d3Network)
library(classInt)
test = dist.lst[[1248]][-1, list(from, to, distance)]
hist(test$distance, breaks = 100)
plot(classIntervals(test$distance, n=5, style="kmeans"),
     pal = pal1, main = "K-means")

plot(classIntervals(test$distance, n=7, style="bclust"),
     pal = pal1, main = "b-clust")

clust = classIntervals(test$distance, n=7, style="bclust")
attr(clust, "parameters")$cluster

table2hierachy = function(table){
    root = unique(table[, from])
    clust = classIntervals(table$distance, n = 7, style = "bclust")
    table[, class := attr(clust, "parameters")$cluster]
    splited = split(table, table$class)
    tmp = vector("list", length(splited))
    for(i in 1:length(splited)){
        print(i)
        tmp[[i]]$name = paste0("class_", unique(splited[[i]]$class))

        tmp2 = lapply(split(splited[[i]], splited[[i]]$to),
            FUN = function(x) list(name = x$to, size = x$distance))
        names(tmp2) = NULL
        tmp[[i]]$children =tmp2
    }
    list(name = root, children = tmp)
    ## for(i in 1:length(splited)){
    ##     tmp[[i]]$name = paste0("class ", i)
    ##     tmp[[i]]$children = splited
    ## list(name = root, child = 
}

check = table2hierachy(test)
d3Tree(List = check, fontsize = 8, file = "../playground/testTree.html",
       diameter = 500)


as.list(mapply(list, a = 1:5, b = 5:1))
lapply(FUN = function(x, y) list(x, y), x = 1:5, y = 5:1)

tmp = data.frame(t(data.frame(a = letters[1:5], b = rnorm(5))))
colnames(tmp) = letters[1:5]
as.list(tmp)


test = list(name = "root",
    children = list(list(name = "child1", dist = 5),
                    list(name = "child2", dist = 3)))
d3Tree(List = test, fontsize = 8, diameter = 500,
       file = "testTree.html")
        




########################################################################
## Testing codes
########################################################################

sorted.dt = imputed.dt[order(distFromIndex, decreasing = FALSE)]
filtered.dt = sorted.dt[!duplicated(sorted.dt$FAOST_CODE), ]

neighbour.dt = filtered.dt[1:15, ]


test.df = data.frame(from = rep(paste0(neighbour.dt[1, FAO_TABLE_NAME], "\n(", neighbour.dt[1, Year], ")"), NROW(neighbour.dt) - 1), to = paste0(neighbour.dt[-1, FAO_TABLE_NAME], "\n(", neighbour.dt[-1, Year], ")"), dist = neighbour.dt[-1, distFromIndex])
test.graph = graph.data.frame(d = test.df)

plot(test.graph,
     layout = layout.fruchterman.reingold(test.graph,
         weights = (1/(E(test.graph)$dist/max(E(test.graph)$dist))) ^5),
     vertex.label.cex = 0.9,
     vertex.size = 8,
     edge.label = round(E(test.graph)$dist, 2),
     edge.label.cex = 0.9)

compare.dt = imputed.dt[FAO_TABLE_NAME == "Indonesia" & Year >= 1996, ]
plot(compare.dt$Year, unlist(compare.dt[, 2, with = FALSE]), ylim = c(0, 1), type = "l")
for(i in 3:28){
    lines(compare.dt$Year, unlist(compare.dt[, i, with = FALSE]))
}
    



## Now assume the user chose Pakistan
difference = function(data, base, compare, statistics){
    tmp = sort(sapply(statistics, FUN = function(x) abs(unlist(data[FAO_TABLE_NAME == base, x, with = FALSE]) - unlist(data[FAO_TABLE_NAME == compare, x, with = FALSE]))), decreasing = TRUE)
    list(different = head(tmp, 5), similar = tail(tmp, 5))
    ## tmp
}

difference(data = neighbour.dt, "India", "Indonesia",
           colnames(imputed.dt)[2:28])

difference.df =
    data.frame(t(difference(data = neighbour.dt,
                            "India", "Pakistan",
                            colnames(imputed.dt)[2:28])))


pdf(file = "india_recommmendation.pdf", width = 10, height = 10)
plot(test.graph,
     layout = layout.fruchterman.reingold(test.graph,
         weights = (1/(E(test.graph)$dist/max(E(test.graph)$dist))) ^5),
     vertex.label.cex = 1.2,
     vertex.size = 8,
     edge.label = round(E(test.graph)$dist, 2),
     edge.label.cex = 1.2)
graphics.off()



## (1) Function to identify neighbouring countries

## (2) Function to compare the neighbouring country and the country of
## interset.

## (3) Function to illustrate the change of the neighbouring country
## in the 4 dimensions.
