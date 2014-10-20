library(data.table)
library(igraph)

load("../imputed.RData")


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

imputed.dt[, distFromIndex :=
               dist2index(imputed.dt, colnames(imputed.dt)[2:28], index = 1248)]

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
