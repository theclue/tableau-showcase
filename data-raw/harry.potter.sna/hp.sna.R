######################
# REQUIREMENTS       #
######################
if (!require("pacman")) install.packages("pacman"); invisible(library(pacman))
tryCatch({
  p_load("tidyverse",
         "futile.logger",
         "Hmisc", 
         "igraph",
         "sna",
         "poweRlaw",
         "intergraph",
         "RColorBrewer",
         "knitr")
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
hp.sna <- intergraph::asNetwork(hp.graph)

##############
# Basic Stats
##
hp.min_cut <- min_cut(hp.graph, value.only = FALSE)
hp.brokerage <- brokerage(hp.sna, cl = V(hp.graph)$affiliation)


# How to break the graph. Harry alone is not enough, but along with Ron or Hagrid he is.
is_separator(hp.graph, c("Albus Dumbledore"))

# articulation points
biconnected_components(hp.graph)

hp.stats <- data.frame(name = V(hp.graph)$name,
                       degree.std = sna::degree(hp.sna, gmode = "graph"),
                       betweeness.std = sna::betweenness(hp.sna, gmode = "graph", cmode = "undirected"),
                       betweeness.linear = sna::betweenness(hp.sna, gmode = "graph", cmode = "linearscaled"),
                       betweeness.proximal = sna::betweenness(hp.sna, gmode = "graph", cmode = "proximalsrc"),
                       closeness.std = sna::closeness(hp.sna, gmode = "graph", cmode = "undirected"),
                       closeness.gil = sna::closeness(hp.sna, gmode = "graph", cmode = "gil-schmidt"),
                       eigenvector.std = sna::evcent(hp.sna, gmode = "graph"),
                       harari.std = sna::graphcent(hp.sna, gmode = "graph"),
                       bonpow.std = sna::bonpow(hp.sna, g = 1, exponent = -2, tol = 1e-20, gmode = "graph"),
                       kcores.std = kcores(hp.sna, mode = "graph"),
                       is.separator = do.call(c,lapply(V(hp.graph)$id, function(x, g){
                         if(x=="Harry Potter") return(FALSE)
                         return(is_separator(g, c(x, "Harry Potter")))
                       }, g = hp.graph)),
                       stringsAsFactors = FALSE)

#######################
# Sociogram Enrichment
##

V(hp.graph)$kcore <- hp.stats$kcores.std

#######################
# igraph Layout Setup
##

node.colors <- brewer.pal(length(unique(hp.stats$kcores.std)), "Spectral")
names(node.colors) <- sort(unique(hp.stats$kcores.std))

V(hp.graph)$color <- node.colors[as.character(hp.stats$kcores.std)]

V(hp.graph)$id <- V(hp.graph)$label

V(hp.graph)$size <- hp.centrality$betweeness.std/50
#V(hp.graph)$size <- 6

hp.graph.layout <- layout_(hp.graph, with_fr(niter = 2000))

V(hp.graph)$x <- hp.graph.layout[,1]
V(hp.graph)$y <- hp.graph.layout[,2]


################
# Network Plots
##

# cfr. http://kateto.net/network-visualization

# Plot the egree distribution for our network:
deg.dist <- degree_distribution(hp.graph, cumulative=T, mode="all")
plot( x=0:max(igraph::degree(hp.graph)), y=1-deg.dist, pch=19, cex=1.2, col="orange", 
      xlab="Degree", ylab="Cumulative Frequency")

plot(hp.graph,
     layout=hp.graph.layout,
     edge.width=E(hp.graph)$weight/8,
     #edge.color=V(hp.graph)$color[ends(hp.graph, es=E(hp.graph), names=F)[,1]],
     edge.color= "grey",
     #vertex.frame.color="#ffffff",
     #vertex.label= V(hp.graph)$name,
     vertex.label = NA,
     #vertex.label.color="black",
     vertex.size = 6
)
legend(x=-1.5,
       y=-1.1,
       sort(unique(V(hp.graph)$kcore)),
       pch=21,
       col="#777777",
       pt.bg=edge.colors,
       pt.cex=1,
       cex=.8,
       bty="n",
       ncol=8)

#####################
# visNetwork Layouts
##

vis.nodes <- data.frame(id = V(hp.graph)$id,
                        label = V(hp.graph)$name,
                        group = V(hp.graph)$affiliation,
                        k.core = hp.stats$kcores.std,
                        value = hp.stats$betweeness.std,
                        #color = node.colors[as.character(hp.stats$kcores.std)],
                        title = V(hp.graph)$name,
                        #x = hp.graph.layout[,1],
                        #y = hp.graph.layout[,2],
                        stringsAsFactors = FALSE
                        )

vis.edges <- data.frame(from = as_edgelist(hp.graph)[,1],
                        to = as_edgelist(hp.graph)[,2],
                        value = E(hp.graph)$weight,
                        stringsAsFactors = FALSE
                        )

vis.n <- visNetwork(vis.nodes, vis.edges, width="100%", height="520px") %>%
  visNodes(shadow = TRUE, scaling = list(min=6, max=20), font = list(size = 10, face = "Verdana")) %>%
  visEdges(color = list(opacity = .5), smooth = FALSE, scaling = list(min=.2, max=4)) %>%
  visPhysics(solver = "forceAtlas2Based", forceAtlas2Based = list(gravitationalConstant = -30)) %>%
  visGroups(groupname = "Diagon Alley", color = "#4C362C") %>%
  visGroups(groupname = "Gringotts Wizarding Bank", color = "#4C362C") %>%
  visGroups(groupname = "Gryffindor", color = "#6F2848") %>%
  visGroups(groupname = "Hogwarts", color = "#FFEAB2") %>%
  visGroups(groupname = "Hufflepuff", color = "#FEB81C") %>%
  visGroups(groupname = "Muggle", color = "#DEDEDE") %>%
  visGroups(groupname = "Order of the Phoenix", color = "B22E22") %>%
  visGroups(groupname = "Ravenclaw", color = "#2478DB") %>%
  visGroups(groupname = "Slytherin", color = "#0F7B47") %>%
  visOptions(selectedBy = "group",
             highlightNearest = list(enabled = TRUE,
                                     degree = 1,
                                     labelOnly = TRUE,
                                     hover = TRUE)) %>%
  visInteraction(dragNodes = FALSE, dragView = TRUE, zoomView = TRUE, navigationButtons = TRUE)

vis.n$sizingPolicy$browser$padding <- 0
vis.n$x$byselection$style <- NULL
#vis.n$x$byselection$main <- "Gruppi"
vis.n$x$idselection$style <- NULL

vis.n %>% htmlwidgets::saveWidget(file = file.path("..", "..", "inst", "html-widgets", "hp.houses.html"), selfcontained = FALSE, libdir = "assets")

# browseURL(file.path("..", "..", "inst", "html-widgets", "hp.kcore.html"))

write.graph(hp.graph, file = "../../data/harry-potter/harrypotter.01.layout.graphml", format=c("graphml"))
