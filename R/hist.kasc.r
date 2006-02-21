"hist.kasc" <-
function (x, type = c("h", "l"), adjust = 1, col = "blue", ...)
{
  type <- match.arg(type)
  if (!inherits(x, "kasc")) 
    stop("should be an object of class \"kasc\"")
  old.par <- par(no.readonly = TRUE)
  on.exit(par(old.par))
  par(mar = c(0.5,0.5,2,0.5))

  tab <- x
  clas <- rep("", ncol(tab))
  for (j in 1:ncol(tab)) {
    w1 <- "q"
    if (is.factor(tab[, j])) 
      w1 <- "f"
    clas[j] <- w1
  }

  par(mfrow = rev(n2mfrow(ncol(tab))))

  f1 <- function(j) {
    tmpZ <- tab[,j]
    name <- names(tab)[j]
    if (clas[j] == "f") {
      par(mar = c(3,0.5,2,0.5))
      max <- max(table(tmpZ))
      max <- max + max/20
      ylim <- c(0, max)
      
      barplot(unclass(summary(tmpZ[!is.na(tmpZ)])), ylim = ylim, border = col, 
              main = name, ylab = NULL, axes = FALSE, ...)
      par(mar = c(0.5,0.5,2,0.5))
    }
    else {
      xrange <- range(tmpZ)
      G <- hist(tmpZ, plot = FALSE)
      plot(G, freq = FALSE, border = col, main = name, 
           xlab = NULL, ylab = NULL, axes = FALSE, ...)
    }
    box()
  }

  f2 <- function(j) {
    tmpZ <- tab[,j]
    name <- names(tab)[j]
    if (clas[j] == "f") {
      par(mar = c(3,0.5,2,0.5))
      max <- max(table(tmpZ))
      max <- max + max/20
      ylim <- c(0, max)
      barplot(unclass(summary(tmpZ[!is.na(tmpZ)])), ylim = ylim, border = col, 
              main = name, ylab = NULL, axes = FALSE, ...)
      par(mar = c(0.5,0.5,2,0.5))
    }
    else {
      dens <- density(tmpZ, adjust = adjust, na.rm = TRUE)
      plot(dens,  col = col, type = "l", lwd = 2, 
           main = name, xlab = NULL, ylab = "Density", 
           axes = FALSE, ...)
      mean <- mean(tmpZ, na.rm = TRUE)
      lines(rep(mean,2),
            c(0,dens$y[512-sum(dens$x>mean)]),
            col = col, lty = 2, lwd = 2)
    }
    box()
  }

  if (type == "h")
    lapply (1:ncol(tab), f1)
  if (type == "l") {
    if (any(clas == "f"))
      warning("Type = 'l' is not possible for factors, type = 'h' used instead.\n")
    lapply (1:ncol(tab), f2)
  }
  return(invisible(NULL))
}

