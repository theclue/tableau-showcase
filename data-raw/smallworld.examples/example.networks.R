#################################
# EXAMPLE NETWORKS             #
# for educational purpoes       #
#################################
if (!require("pacman")) install.packages("pacman"); invisible(library(pacman))
tryCatch({
  p_load("tidyverse", "futile.logger","igraph", "sna", "intergraph", "RColorBrewer", "tsna", "visNetwork")
}, warning=function(w){
  stop(conditionMessage(w))
})

sapply(file.path("..", "..", "R", c("fit.power.law.R")),
       source,
       .GlobalEnv)

smallworld.example <- make_graph(directed = TRUE,
                                 edges = c(1,4,
                                           4,5,
                                           4,7,
                                           4,8,
                                           4,9,
                                           10,9,
                                           4,2,
                                           2,4,
                                           4,3,
                                           3,4,
                                           4,6,
                                           4,14,
                                           14,4,
                                           6,11,
                                           11,13,
                                           13,11,
                                           13,14,
                                           14,13,
                                           14,16,
                                           15,16,
                                           17,16,
                                           15,17,
                                           14,17,
                                           17,14,
                                           20,11,
                                           20,12,
                                           20,18,
                                           19,20,
                                           20,19,
                                           19,21,
                                           21,19,
                                           20,21,
                                           21,20,
                                           21,22,
                                           22,21,
                                           20,23,
                                           20,13,
                                           14,20
                                 ))

vis.x <- c(-0.93528883,
           -0.64370251,
           -0.25815814,
           -0.36911966,
           0.07582462, 
           0.09556118,
           -0.87784055,
           -0.59442750,
           -0.23924928,
           -0.16322217,
           0.33671840,
           0.25711923,
           0.11277473,
           -0.22382193,
           -1.00000000,
           -0.76775707,
           -0.57229733,
           0.97761498,
           0.59676092,
           0.45883745,
           0.71403774,
           0.91791276,
           1.00000000)

vis.y <- c(0.2524424,
           0.3249998,
           0.4378094,
           0.2731771,
           0.4498740,
           0.0591335,
           0.4633927,
           0.5685366,
           0.7012318,
           1.0000000,
           -0.2342028,
           -0.8151001,
           -0.3133818,
           -0.1651676,
           -0.4520687,
           -0.3141045,
           -0.3194687,
           -0.4543636,
           -0.6992878,
           -0.5285877,
           -0.7971433,
           -1.0000000,
           -0.6346642)


smallworld.sna <- intergraph::asNetwork(smallworld.example)

V(smallworld.example)$name <- as.matrix(V(smallworld.example))[,1]

smallworld.stats <- data.frame(name = V(smallworld.example)$name,
                               degree.std = sna::degree(smallworld.sna, gmode = "digraph"),
                               degree.norm = sna::degree(smallworld.sna, gmode = "digraph", rescale = TRUE),
                               degree.in = sna::degree(smallworld.sna, gmode = "digraph", cmode="indegree"),
                               degree.out = sna::degree(smallworld.sna, gmode = "digraph", cmode = "outdegree"),
                               betweeness.std = sna::betweenness(smallworld.sna, gmode = "graph", cmode = "undirected"),
                               betweeness.dir = sna::betweenness(smallworld.sna, gmode = "digraph", cmode = "directed"),
                               betweeness.linear = sna::betweenness(smallworld.sna, gmode = "graph", cmode = "linearscaled"),
                               betweeness.proximal = sna::betweenness(smallworld.sna, gmode = "graph", cmode = "proximalsrc"),
                               closeness.std = sna::closeness(smallworld.sna, gmode = "graph", cmode = "undirected"),
                               closeness.dir = sna::closeness(smallworld.sna, gmode = "digraph", cmode = "directed"),
                               closeness.gil = sna::closeness(smallworld.sna, gmode = "graph", cmode = "gil-schmidt"),
                               kcore.all = coreness(smallworld.example, mode = "all"),
                               kcore.in = coreness(smallworld.example, mode = "in"),
                               kcore.out = coreness(smallworld.example, mode = "out"),
                               eigenvector.std = sna::evcent(smallworld.sna, gmode = "graph"),
                               stringsAsFactors = FALSE)

brewer.pal(n = length(unique(smallworld.stats$kcore.all)), name = "Pastel2")

V(smallworld.example)$color <- factor(smallworld.stats$kcore.all, labels = brewer.pal(n = length(unique(smallworld.stats$kcore.all)), name = "Pastel2"))
#V(smallworld.example)$size <- smallworld.stats$degree.std*5
#V(smallworld.example)$size <- smallworld.stats$betweeness.std/2
V(smallworld.example)$size <- smallworld.stats$closeness.std*50

#V(smallworld.example)$name <- paste(as.matrix(V(smallworld.example))[,1], smallworld.stats$degree.std, sep = " - ")

vis.widget <- 
  #visNetwork(smallworld.data$nodes, smallworld.data$edges, width = "100%", height = "450px") %>%
  #visIgraphLayout(layout = "layout_with_fr") %>% 
  visIgraph(smallworld.example) %>% 
  visNodes(shadow = TRUE, 
           group = smallworld.stats$kcore.all,
           x = vis.x,
           y = vis.y,
           title = "don't care",
           scaling = list(min = 10, max = 50)) %>%
  visOptions(width = "100%",
             #height = "100%",
             highlightNearest = list(enabled = TRUE, algorithm = "hierarchical"),
             #selectedBy = list(variable = "group", style = ""L)
             ) %>%
  visInteraction(dragNodes = FALSE,
                 navigationButtons = TRUE)

# for some reasons visNodes positional parameters is not working so I'm using a hard-coded assignation
vis.widget$x$nodes$x <- vis.x
vis.widget$x$nodes$y <- vis.y

# same for sizingProperties
vis.widget$width = "100%"
vis.widget$height = "450px"
vis.widget$sizingPolicy$browser$padding <- "0px"

#vis.widget$x$nodes$title <- sprintf("<strong>Total Degree: %d</strong><br />In-Degree: %d<br />Out-Degree: %d", smallworld.stats$degree.std, smallworld.stats$degree.in, smallworld.stats$degree.out)
#vis.widget$x$nodes$title <- sprintf("Undirected Betweeness: %d<br />Directed Betweeness: %d", smallworld.stats$betweeness.std, smallworld.stats$betweeness.dir)
vis.widget$x$nodes$title <- sprintf("Closeness: %f", smallworld.stats$closeness.std)


vis.widget %>% htmlwidgets::saveWidget(file = "smallworld.degree.example.html", selfcontained = FALSE, libdir = "assets")

par(mfrow=c(1,2))

#######################
# ERDOS-RENYI NETWORK #
#######################
er.example <- sample_gnp(10000, p = .02, directed = FALSE, loops = FALSE)
plot.degree.distribution(er.example, xlab = "Degree (log)", ylab = "Probability (log)", col = 1, main = "Erdos-Renyi Degree Distribution", pch = 16, cex = .5)

######################
# SCALE-FREE NETWORK #
######################
scale.free.example <- sample_pa(10000, directed = FALSE, algorithm = "psumtree")
fit.power.law(scale.free.example, line.col = "red", xlab = "Degree (log)", ylab = "Probability (log)", 
              col = 1, main = "Scale-Free Degree Distribution", pch = 16, cex = .5)



###################
# TOY NETWORKS    #
###################

star.network <- make_star(8, mode = "undirected")
line.network <- make_lattice(length = 5, dim = 1) # %>% add_layout_(as_tree(root = 1))
lattice.network <- make_lattice(length = 6, dim = 1, circular = TRUE)

butterfly.network <- make_graph(directed = FALSE,
                                 edges = c(1,2,
                                           1,3,
                                           2,3,
                                           4,5,
                                           4,6,
                                           5,6,
                                           3,7,
                                           4,7)
)

line.layout <- line.network %>% layout_as_tree(root = 1)

plot(line.network, layout=-line.layout[, 2:1])
