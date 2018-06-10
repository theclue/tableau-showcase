plot.degree.distribution <- function(graph){
  
  hp.degrees <- igraph::degree(graph)
  
  # Let's count the frequencies of each degree
  hp.degree.histogram <- as.data.frame(table(hp.degrees))
  
  # Need to convert the first column to numbers, otherwise
  # the log-log thing will not work (that's fair...)
  hp.degree.histogram[,1] <- as.numeric(hp.degree.histogram[,1])
  
  hp.degree.histogram[,1] <- as.numeric(hp.degree.histogram[,1])
  
  # Now, plot it!
  ggplot(hp.degree.histogram, aes(x = hp.degrees, y = Freq)) +
    geom_point() +
    scale_x_continuous("Degree\n(nodes with this amount of connections)",
                       trans = "log10") +
    scale_y_continuous("Frequency\n(how many of them)",
                       breaks = 0:max(hp.degrees),
                       trans = "log10") +
    ggtitle("Degree Distribution (log-log)")
  
  
}