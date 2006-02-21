"getsahrlocs" <-
function(x, what=c("sa", "hr", "locs"))
  {
    what<-match.arg(what)
    sahr<-x
    rm(x)
    if (!inherits(sahr, "sahrlocs")) stop("non convenient data type")
    if (is.na(match(what, c("sa", "hr", "locs"))))
      stop("what should be either \"sa\", \"hr\", or \"locs\"")

    output<-sahr[[what]]
    attr(output, "nrow")<-attr(sahr, "nrow")
    attr(output, "ncol")<-attr(sahr, "ncol")
    attr(output, "xll")<-attr(sahr, "xll")
    attr(output, "yll")<-attr(sahr, "yll")
    attr(output, "cellsize")<-attr(sahr, "cellsize")
    class(output)<-c("kasc", "data.frame")

    return(output)
  }

