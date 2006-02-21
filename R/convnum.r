"convnum" <-
function(kasc) {
  if (!inherits(kasc, "kasc"))
    stop("should be of class kasc")
  litab<-kasc2df(kasc)
  dud<-dudi.mix(litab$tab, scannf=FALSE)
  toto <- dud$tab
  names(toto)
  cw<-dud$cw
  scores <- df2kasc(toto, litab$index, kasc)
  return(list(kasc=scores, weight=cw))
}

