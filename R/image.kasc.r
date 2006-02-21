"image.kasc" <-
function(x,  var=names(x),
                     mar=if (length(var)>1) c(0,0,2,0) else c(5.1,4.1,4.1,2.1),
                     axes=(length(var) == 1),
                     clfac=NULL, col=gray((240:1)/256), mfrow=NULL,
                     ...)
  {
    w<-x
    if (!inherits(w,"kasc")) stop("object should be of class \"kasc\"")
    if (is.null(mfrow))
      mfrow=n2mfrow(length(var))
    opar<-par(mfrow=mfrow, mar=mar)
    on.exit(par(opar))
    for (i in var) {
      el<-getkasc(x, i)
      if (attr(el, "type")=="factor") {
        if (!is.null(clfac)) {
          clf<-clfac[[i]]
        } else {
          clf<-NULL
        }
        if (length(var)>1)
          image.asc(el, main=i, axes=axes, clfac=clf, ... )
        if (length(var)==1)
          image.asc(el, axes=axes, clfac=clf, ... )
      } else {
        if (length(var)>1)
          image.asc(el, main=i, axes=axes, col=col, ...)
        if (length(var)==1)
          image.asc(el, axes=axes, col=col, ... )
      }
    }
  }

