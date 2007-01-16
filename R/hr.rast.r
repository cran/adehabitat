"hr.rast" <- function(mcp, w)
{
    ## Verifications
    if (inherits(w, "asc"))
      w <- as.kasc(list(to=w))
    if (!inherits(w, "kasc"))
      stop("Non convenient data")
    if (!inherits(mcp, "area"))
      stop("mcp should be of class \"area\"")

    ## a list with one element = one polygon
    lpc<-split(mcp[,2:3], mcp[,1])
    output<-list()

    ## use of the function mcp.rast for each polygon
    for (i in 1:length(lpc))
      output[[names(lpc)[i]]]<-mcp.rast(lpc[[i]], w)

    ## the output:
    output<-as.kasc(output)
    return(output)
  }

