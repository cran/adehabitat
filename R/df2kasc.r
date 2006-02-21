"df2kasc" <-
function(df, index, x)
  {
    if (!inherits(df,"data.frame")) stop("non convenient data type")
    if ((!inherits(x,"kasc"))&(!inherits(x,"mapattr")))
      stop("non convenient data type")
    
    o<-x
    class(o)<-"data.frame"
    N<-attr(o, "nrow")*attr(o, "ncol")
    indw<-c(1:N)
    li<-df
    n1<-nrow(li)

    compl<-as.data.frame(matrix(NA, nrow=N-n1, ncol=ncol(li)))
    names(compl) <- names(li)
    output<-rbind.data.frame(li, compl)
    indcompl<-indw[is.na(match(indw, index))]
    indtot<-c(index, indcompl)
    output<-output[sort(indtot, index.return=TRUE)$ix,]
    class(output)<-c("kasc","data.frame")
    attr(output, "nrow")<-attr(x, "nrow")
    attr(output, "ncol")<-attr(x, "ncol")
    attr(output, "xll")<-attr(x, "xll")
    attr(output, "yll")<-attr(x, "yll")
    attr(output, "cellsize")<-attr(x, "cellsize")

    return(output)
  }

