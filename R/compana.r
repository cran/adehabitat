"compana" <-
function(used, avail, test = c("randomisation", "parametric"),
                  rnv = 0.01, nrep = 500, alpha=0.1)
  {
    ### 1. Vérifications
    test<-match.arg(test)
    used<-as.matrix(used)
    avail<-as.matrix(avail)
    if ((any(avail==0))&(test=="parametric")) {
      warning("parametric tests not suitable with 0 in avail; test has been set to \"randomisation\"")
      test<-"randomisation"
    }
    if (ncol(used)!=ncol(avail))
      stop("the two matrices should have the same dimensions")
    if (nrow(used)!=nrow(avail))
      stop("the two matrices should have the same dimensions")
    if (!all(colnames(used)==colnames(avail)))
      stop("the two matrices should have the same habitat names")
    if (is.null(colnames(used))) 
      colnames(used) <- paste("Habitat", 1:ncol(u), sep = "")
    if (is.null(colnames(avail))) 
        colnames(avail) <- paste("Habitat", 1:ncol(a), sep = "")

    ## 2. Bases
    nh<-ncol(used)
    na<-nrow(used)
    proj1<-matrix(1, nrow=nrow(used), ncol=nrow(used))*(1/nrow(used))
    proj2<-matrix(0, nrow=nrow(used), ncol=nrow(used))
    if (test=="parametric")
      nrep=1
    sorties<-list()

    ## 3. Première partie: test global
    toto<-.C("aclambda", as.double(t(used)), as.double(t(avail)),
             as.integer(na), as.integer(nh),  
             as.double(proj1), as.double(proj2), as.double(rnv),
             double(nrep), as.integer(nrep), double(nh), double(nh),
             PACKAGE="adehabitat")
    
    vrand<-toto[[8]]
    sorties$used<-used
    sorties$avail<-avail
    sorties$type.test<-test
    if (test=="randomisation") {
      sorties$random.res<-list(sim=vrand, obs=vrand[1])
      sorties$test<-c(vrand[1], length(vrand[vrand<=vrand[1]])/nrep)
      names(sorties$test)<-c("Lambda", "P")
    }
    else {
      sorties$test<-c(vrand[1], ncol(used)-1, 1-pchisq(-na*log(vrand[1]), ncol(used)-1))
      names(sorties$test)<-c("Lambda", "df", "P")
    }
    
    ## Deuxième partie: ranking matrix
    if (test=="randomisation") {
      toto<-.C("rankma", as.double(t(used)), as.double(t(avail)),
               double(nh**2), double(nh**2), double(nh**2),
               double(nh**2), as.integer(nh), as.integer(na),
               as.integer(nrep), as.double(rnv), PACKAGE="adehabitat")
      
      rmp<-t(matrix(toto[[3]]/nrep, nh, nh))
      rmm<-t(matrix(toto[[4]]/nrep, nh, nh))
      rmv<-t(matrix(toto[[5]], nh, nh))
      rmnb<-t(matrix(toto[[6]], nh, nh))
    }
    else {
      used[used==0]<-rnv
      rmv<-matrix(0, nh, nh)
      rmse<-matrix(0, nh, nh)
      rmm<-matrix(0, nh, nh)
      rmp<-matrix(0, nh, nh)
      rmnb<-matrix(0, nh, nh)

      for (i in 1:nh) {
        for (j in 1:nh) {
          dlr<-log(used[,i]/used[,j])-log(avail[,i]/avail[,j])
          rmv[i,j]<-mean(dlr)
          rmse[i,j]<-sqrt(var(dlr)/na)
          if (i!=j)
            rmv[i,j]<-rmv[i,j]/rmse[i,j]
          rmp[i,j]<-pt(rmv[i,j], na-1)
          rmm[i,j]<-1-rmp[i,j]
          rmnb[i,j]<-na
        }
      }
    }
    
    rm<-matrix("0", nh, nh)
    

    ## ranking matrix: juste les signes
    for (i in 1:nh) {
      for (j in 1:nh) {
        if (rmv[i,j]<0)
          rm[i,j]<-"-"
        if (rmv[i,j]>0)
          rm[i,j]<-"+"
      }
    }

    for (i in 1:nh) {
      for (j in 1:nh) {
        if (rmp[i,j] < (alpha/2)) {
          rm[i,j]<-"---"
        }
        if (rmm[i,j] < (alpha/2)) {
          rm[i,j]<-"+++"
        }
        if (i==j)
          rm[i,j]<-"0"
      }
    }
  


    rank<-rep(0, nh)
    for (j in 1:nh) {
      for (i in 1:nh) {
        if (rmv[j,i]>0)
          rank[j]<-rank[j]+1
      }
    }
    
    names(rank)<-colnames(avail)
    rownames(rm)<-colnames(avail)
    colnames(rm)<-colnames(avail)
    rownames(rmv)<-colnames(avail)
    colnames(rmv)<-colnames(avail)
    rownames(rmp)<-colnames(avail)
    colnames(rmp)<-colnames(avail)
    rownames(rmm)<-colnames(avail)
    colnames(rmm)<-colnames(avail)
    rownames(rmnb)<-colnames(avail)
    colnames(rmnb)<-colnames(avail)
    sorties$rmnb<-rmnb
    sorties$rank<-rank
    sorties$rm<-rm
    sorties$rmv<-rmv
    
    sorties$profile<-profilehab(rm, rank)
    class(sorties)<-"compana"
    return(sorties)
  }

