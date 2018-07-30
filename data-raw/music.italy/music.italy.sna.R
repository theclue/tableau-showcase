######################
# REQUIREMENTS       #
######################
if (!require("pacman")) install.packages("pacman"); invisible(library(pacman))
tryCatch({
  p_load("tidyverse", "futile.logger", "Hmisc", "igraph", "sna", "poweRlaw", "intergraph", "RColorBrewer", "knitr", "tsna", "visNetwork")
}, warning=function(w){
  stop(conditionMessage(w))
})

sapply(file.path("..", "..", "R", c("fit.power.law.R", "giant.component.R")),
       source,
       .GlobalEnv)

#################################
# LOAD NETWORK DATA
# -
# 
##

italian.music.graph <- read_graph(file = "../../data/italian-music/italian-music.graphml", format = "graphml")

# Connected and giant component subgraphs
italian.connected.graph <- delete_vertices(italian.music.graph, V(italian.music.graph)[which(igraph::degree(italian.music.graph) == 0)])
italian.connected.sna <- intergraph::asNetwork(italian.connected.graph)

italian.giant.graph <- giant.component(italian.music.graph)
italian.giant.sna <- intergraph::asNetwork(italian.giant.graph)

##############
# Centrality
##

italian.music.sna <- intergraph::asNetwork(italian.music.graph)

italian.music.stats <- data.frame(name = V(italian.music.graph)$name,
                                  membership = igraph::components(italian.music.graph)$membership,
                                  degree.std = sna::degree(italian.music.sna, gmode = "digraph"),
                                  degree.norm = sna::degree(italian.music.sna, gmode = "digraph", rescale = TRUE),
                                  degree.in = sna::degree(italian.music.sna, gmode = "digraph", cmode="indegree"),
                                  degree.out = sna::degree(italian.music.sna, gmode = "digraph", cmode = "outdegree"),
                                  betweeness.std = sna::betweenness(italian.music.sna, gmode = "digraph", cmode = "undirected"),
                                  betweeness.linear = sna::betweenness(italian.music.sna, gmode = "digraph", cmode = "linearscaled"),
                                  betweeness.proximal = sna::betweenness(italian.music.sna, gmode = "digraph", cmode = "proximalsrc"),
                                  closeness.std = sna::closeness(italian.music.sna, gmode = "digraph", cmode = "directed", rescale = TRUE),
                                  closeness.gil = sna::closeness(italian.music.sna, gmode = "graph", cmode = "gil-schmidt", rescale = TRUE),
                                  closeness.igraph.in = igraph::closeness(italian.music.graph, mode = "in", normalized = TRUE),
                                  closeness.igraph.out = igraph::closeness(italian.music.graph, mode = "out", normalized = TRUE),
                                  core.all = coreness(italian.music.graph, mode = "all"),
                                  power.igraph = igraph::power_centrality(italian.music.graph, rescale = TRUE),
                                  #bonpow.std = sna::bonpow(italian.giant.sna),
                                  stringsAsFactors = FALSE)


#italian.music.brokerage <- brokerage(italian.music.sna, cl = V(italian.music.graph)$affiliation)

# the graph is strongly connected, there're no cutting vertices in it
italian.music.min_cut <- min_cut(italian.music.graph, value.only = FALSE)

# articulation points
italian.music.biconnected <- biconnected_components(italian.music.graph)

################
# Attribute enrichment
##
edge.colors <- brewer.pal(9, "Spectral")
names(edge.colors) <- unique(V(italian.music.graph)$affiliation)

V(italian.music.graph)$color <- edge.colors[V(italian.music.graph)$affiliation]

V(italian.music.graph)$id <- V(italian.music.graph)$label
V(italian.music.graph)$size <- italian.music.centrality$betweeness.std/6

V(italian.music.graph)$color[ends(italian.music.graph, es=E(italian.music.graph), names=F)[,1]]


################
# Network Plots
##

# cfr. http://kateto.net/network-visualization
italian.music.graph.layout <- layout_(italian.music.graph, with_lgl(root = "Harry Potter"))

V(italian.music.graph)$x <- italian.music.graph.layout[,1]
V(italian.music.graph)$y <- italian.music.graph.layout[,2]

plot(italian.music.graph,
     layout=italian.music.graph.layout,
     edge.width=E(italian.music.graph)$weight/8,
     edge.color=V(italian.music.graph)$color[ends(italian.music.graph, es=E(italian.music.graph), names=F)[,1]],
     vertex.frame.color="#ffffff",
     vertex.label= V(italian.music.graph)$name,
     vertex.label.color="black",
     vertex.size = 12)
legend(x=-1.5,
       y=-1.1,
       unique(V(italian.music.graph)$affiliation),
       pch=21,
       col="#777777",
       pt.bg=edge.colors,
       pt.cex=2,
       cex=.8,
       bty="n",
       ncol=3)

plot.degree.distribution(italian.music.graph.layout)

# it definitively look like a small world
fit.power.law(italian.music.graph)

write.graph(italian.music.graph, file = "../../data/harry-potter/harrypotter.01.layout.graphml", format=c("graphml"))
