"kasc2df" <-
function(x, var=names(x))
  {
    if (!inherits(x, "kasc")) stop("Non convenient data type")

    w<-data.frame(x[var])
    index<-c(1:nrow(w))
    abenner<-function(x){
      if (any(is.na(x))) {
        return(FALSE)
      } else {
        return(TRUE)
      }
    }
    cons<-apply(w, 1, abenner)
    indcons<-index[cons]
    wcons<-data.frame(w[cons,])
    output<-list(index=indcons, tab=wcons)
  }

