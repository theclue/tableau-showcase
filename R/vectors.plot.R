# Vectors to plot
a <- c(1,3)

m <- matrix(c(3, 0, 0, -2), 2, 2)

vectors.plot <- function(a, m){
  
  b <- m %*% a
  
  ## set global plot params
  par(xaxs = 'i',
      yaxs = 'i',
      mar=c(1, 1, 3, 3) + 0.1)
  
  # limits including zero
  xlim <- c(min(0, a[1], b[1]) - 0.4, max(0, a[1], b[1]) + 0.4)
  ylim <- c(min(0, a[2], b[2]) - 0.4, max(0, a[2], b[2]) + 0.2)
  
  ## draw plot
  plot(NA,
       xlim = xlim,
       ylim = ylim,
       axes = F,
       ann = F); ## set plot bounds, no default ornaments
  
  arrows(c(0, xlim[1]),
         c(ylim[1], 0),
         c(0, xlim[2]),
         c(ylim[2], 0), 0.05); ## draw custom axes
  
  mtext('y', 3, 1, at=0, las=1, cex=0.8, family='serif'); ## y label
  mtext('x', 4, 1, at=0, las=1, cex=0.8, family='serif'); ## x label
  
  # source vector
  arrows(0, 0, a[1], a[2], length = .1, lwd = 3, col = "gray6")
  arrows(0, 0,    0, a[2], length = .1, lwd = 2, col = "firebrick3")
  arrows(0, 0, a[1],    0, length = .1, lwd = 2, col = "forestgreen")
  segments(a[1], 0, a[1], a[2], length = .1, lwd = 1, col = "forestgreen", lty = 2)
  segments(0, a[2], a[1], a[2], length = .1, lwd = 1, col = "firebrick3", lty = 2)
  text(a[1] + .1, a[2] + .1, 'a', col = "gray6")
  text(-.1, a[2]/2, expression('a'['x']), col = "firebrick3")
  text(a[1]/2, 0.2, expression('a'['y']), col = "forestgreen")
  
  # destination vector
  arrows(0, 0, b[1], b[2], length = .1, lwd = 3, col = "deepskyblue3")
  arrows(0, 0,    0, b[2], length = .1, lwd = 2, col = "dodgerblue4")
  arrows(0, 0, b[1],    0, length = .1, lwd = 2, col = "darkslateblue")
  segments(b[1], 0, b[1], b[2], length = .1, lwd = 1, col = "darkslateblue", lty = 2)
  segments(0, b[2], b[1], b[2], length = .1, lwd = 1, col = "dodgerblue4", lty = 2)
  text(b[1] + .1, b[2] + .1, 'b', col = "deepskyblue3")
  text(-.1, b[2]/2, expression('b'['x']), col = "darkslateblue")
  text(b[1]/2, 0.2, expression('b'['y']), col = "dodgerblue4")
  
}