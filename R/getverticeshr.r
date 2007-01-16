"getverticeshr" <- function(x, lev=95)
  {
      ## Verifications
      if ((!inherits(x,"khr")))
          stop("non convenient data-type")
      if (inherits(x,"khrud"))
          x<-getvolumeUD(x)
      if (inherits(x,"kbbhrud"))
          x<-getvolumeUD(x)

      ## output list
      contour<-list()

      ## for each animal
      for (i in 1:length(x)) {

          ## gets the UD and keep areas upper than lev
          ud<-x[[i]]$UD
          ud[ud>lev]<-NA
          ud[!is.na(ud)]<-1

          ## gets the contour of the connected features
          contour[[i]]<-getcontour(ud)
      }
      ## output of class "kver"
      names(contour)<-names(x)
      class(contour) <- "kver"
      return(contour)
  }

