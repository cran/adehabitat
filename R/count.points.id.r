"count.points.id" <-
function(xy, id, w)
  {
    x<-xy[,1]
    y<-xy[,2]
    id<-factor(id)
    lx<-split(x, id)
    ly<-split(y, id)
    output<-list()
    for (i in 1:length(levels(id))) 
      output[[levels(id)[i]]]<-count.points(cbind(lx[[i]], ly[[i]]), w)

    output<-as.kasc(output)
    }

