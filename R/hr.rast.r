"hr.rast" <-
function(mcp, w)
  {
    if (inherits(w, "asc"))
      w <- as.kasc(list(to=w))
    if (!inherits(w, "kasc"))
      stop("Non convenient data")
    if (!inherits(mcp, "area"))
      stop("mcp should be of class \"area\"")
    lpc<-split(mcp[,2:3], mcp[,1])
    output<-list()
    for (i in 1:length(lpc))
      output[[names(lpc)[i]]]<-mcp.rast(lpc[[i]], w)
    output<-as.kasc(output)
    return(output)
  }

