plot.degree.distribution = function(graph, ...) {
  # calculate degree
  d = igraph::degree(graph, mode = "all")
  dd = igraph::degree.distribution(graph, mode = "all", cumulative = FALSE)
  degree = 1:max(d)
  probability = dd[-1]
  # delete blank values
  nonzero.position = which(probability != 0)
  probability = probability[nonzero.position]
  degree = degree[nonzero.position]
  # plot
  plot(probability ~ degree, log = "xy", ...)
}
