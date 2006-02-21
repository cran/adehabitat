"export.asc" <-
function(x, file)
  {

    if (!inherits(x, "asc")) stop("Non convenient data")    

### File header reading
    if (substr(file, nchar(file)-3, nchar(file))!=".asc")
      file<-paste(file, ".asc", sep="")
    
    file.create(file)
    zz<-file(file, "w")
    nc<-paste("ncols", "         ", nrow(x), sep="")
    nl<-paste("nrows", "         ", ncol(x), sep="")
    xll<-paste("xllcorner", "     ",
               attr(x, "xll")-attr(x, "cellsize")/2, sep="")
    yll<-paste("yllcorner", "     ",
               attr(x, "yll")-attr(x, "cellsize")/2, sep="")
    cs<-paste("cellsize", "      ", attr(x, "cellsize"), sep="")
    nas<-paste("NODATA_value", -9999, sep="  ")

    writeLines(nc, zz)
    writeLines(nl, zz)
    writeLines(xll, zz)
    writeLines(yll, zz)
    writeLines(cs, zz)
    writeLines(nas, zz)

    close(zz)
    x[is.na(x)]<--9999
    x<-x[,ncol(x):1]
    x<-rbind(x, rep("\n", ncol(x)))

    sink(file, append=TRUE)
    cat(x)
    sink()
    
  }

