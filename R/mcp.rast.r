"mcp.rast" <- function(poly, w)
  {
      ## Verifications
      if (inherits(w, "asc"))
          w <- as.kasc(list(to=w))
      if (!inherits(w, "kasc")) stop("non convenient data")
      if (ncol(poly)!=2)
          stop("poly should have two columns")
      ## The first and last relocations should be the same (closed polygon)
      if (!all(poly[1,]==poly[nrow(poly),]))
          poly<-rbind(poly, poly[1,])

      ## prepares the data
      xy<-getXYcoords(w)
      huhu<-getkasc(w, names(w)[1])
      huhu[is.na(huhu)]<--9999

      ## Use of the C function "rastpolaire" itself calling the C
      ## function "rastpol" of the file "tests.c" of adehabitat
      toto<-.C("rastpolaire", as.double(poly[,1]), as.double(poly[,2]),
               as.double(xy$x), as.double(xy$y), as.double(t(huhu)),
               as.integer(nrow(huhu)), as.integer(ncol(huhu)),
               as.integer(nrow(poly)), PACKAGE="adehabitat")

      ## The output
      output<-matrix(toto[[5]], nrow = nrow(huhu), byrow = TRUE)
      output[output==0]<-NA

      attr(output, "xll")<-attr(w, "xll")
      attr(output, "yll")<-attr(w, "yll")
      attr(output, "cellsize")<-attr(w, "cellsize")
      attr(output, "type")<-"numeric"
      class(output)<-"asc"
      return(output)
  }

## Another name for this function
"area2asc" <- mcp.rast
