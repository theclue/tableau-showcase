######################
# REQUIREMENTS       #
######################
if (!require("pacman")) install.packages("pacman"); invisible(library(pacman))
tryCatch({
  p_load("tidyverse", "futile.logger", "Hmisc", "igraph", "sna", "poweRlaw", "intergraph", "RColorBrewer")
}, warning=function(w){
  stop(conditionMessage(w))
})

sapply(file.path("..", "..", "R", c("fit.power.law.R")),
       source,
       .GlobalEnv)

#################################
# LOAD NETWORK DATA
# -
# 
##

hp.graph <- read_graph(file = "../../data/harry-potter/harrypotter.01.graphml", format = "graphml")

##############
# Centrality
##

hp.sna <- intergraph::asNetwork(hp.graph)

hp.centrality <- data.frame(name = V(hp.graph)$name,
                            betweeness.std = sna::betweenness(hp.sna, gmode = "graph", cmode = "undirected"),
                            betweeness.linear = sna::betweenness(hp.sna, gmode = "graph", cmode = "linearscaled"),
                            betweeness.proximal = sna::betweenness(hp.sna, gmode = "graph", cmode = "proximalsrc"),
                            closeness.std = sna::closeness(hp.sna, gmode = "graph", cmode = "undirected"),
                            closeness.gil = sna::closeness(hp.sna, gmode = "graph", cmode = "gil-schmidt"),
                            eigenvector.std = sna::evcent(hp.sna, gmode = "graph"),
                            harari.std = sna::graphcent(hp.sna, gmode = "graph"),
                            bonpow.std = sna::bonpow(hp.sna, g = 1, exponent = -2, tol = 1e-20, gmode = "graph"),
                            stringsAsFactors = FALSE)


ego <- ego.extract(hp.sna)

hp.brokerage <- brokerage(hp.sna, cl = V(hp.graph)$affiliation)

################
# Attribute enrichment
##
edge.colors <- brewer.pal(9, "Spectral")
names(edge.colors) <- unique(V(hp.graph)$affiliation)

V(hp.graph)$color <- edge.colors[V(hp.graph)$affiliation]

V(hp.graph)$id <- V(hp.graph)$label
V(hp.graph)$size <- hp.centrality$betweeness.std/6

V(hp.graph)$color[ends(hp.graph, es=E(hp.graph), names=F)[,1]]


################
# Network Plots
##

# cfr. http://kateto.net/network-visualization
hp.graph.layout <- layout_(hp.graph, with_lgl(root = "Harry Potter"))

V(hp.graph)$x <- hp.graph.layout[,1]
V(hp.graph)$y <- hp.graph.layout[,2]

plot(hp.graph,
     layout=hp.graph.layout,
     edge.width=E(hp.graph)$weight/8,
     edge.color=V(hp.graph)$color[ends(hp.graph, es=E(hp.graph), names=F)[,1]],
     vertex.frame.color="#ffffff",
     vertex.label= V(hp.graph)$name,
     vertex.label.color="black",
     vertex.size = 12)


legend(x=-1.5,
       y=-1.1,
       unique(V(hp.graph)$affiliation),
       pch=21,
       col="#777777",
       pt.bg=edge.colors,
       pt.cex=2,
       cex=.8,
       bty="n",
       ncol=3)

plot.degree.distribution(hp.graph.layout)


fit.power.law(hp.graph.layout)

write.graph(hp.graph, file = "../../data/harry-potter/harrypotter.01.layout.graphml", format=c("graphml"))
