library(data.table)
library(igraph)

load("../data/imputed.RData")


imputed.dt = na.omit(imputed.dt)
## Should change this to rank
imputed.dt[, `:=`(colnames(imputed.dt)[-c(1, 29)],
                  lapply(imputed.dt[, 3:ncol(imputed.dt), with = FALSE],
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
    rowSums(mapply(x = all, FUN = function(x, y) abs(x - y), y = one))
}

dist.lst =
    lapply(1:NROW(imputed.dt),
           FUN = function(x){
               print(x)
               dist =
                   dist2index(imputed.dt, colnames(imputed.dt)[2:28], x)
               fromCountry = imputed.dt[x, FAO_TABLE_NAME]
               toCountry = imputed.dt[, FAO_TABLE_NAME]
               fromYear = imputed.dt[x, Year]
               toYear = imputed.dt[, Year]
               fullConnect = 
                   data.table(fromCountry = fromCountry,
                              fromYear = fromYear,
                              toCountry = toCountry,
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

library(d3Network)
library(classInt)
test = dist.lst[[1248]][-1, list(from, to, distance)]
hist(test$distance, breaks = 100)
plot(classIntervals(test$distance, n=5, style="kmeans"), pal = pal1, main = "K-means")

plot(classIntervals(test$distance, n=7, style="bclust"), pal = pal1, main = "b-clust")

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

        tmp2 = lapply(split(splited[[i]], splited[[i]]$to), FUN = function(x) list(name = x$to, size = x$distance))
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
