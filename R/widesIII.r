"widesIII" <-
function(u, a, avknown = TRUE, alpha = 0.05)
  {
    u<-as.matrix(u)
    a<-as.matrix(a)
    
    ## V�rifications de d�part
    if (nrow(u) != nrow(a)) 
      stop("available and used matrix should have the same number of animals")
    if (ncol(u) != ncol(a)) 
      stop("available and used matrix should have the same number of habitats")
    
    ## Les transfos de base
    sorties<-list()
    pij<-as.matrix(a)

    ## Calcul de la disponibilit� si pas fournie
    ## en pourcentages pij
    aip<-apply(a,2,sum)
    apj<-apply(a,1,sum)
    
    ## Calcul de l'utilisation (pourcentage)
    uij<-as.matrix(u)
    if (is.null(colnames(u))) 
      colnames(uij) <- paste("Habitat", 1:ncol(u), sep = "")
    if (is.null(colnames(a))) 
      colnames(pij) <- paste("Habitat", 1:ncol(a), sep = "")

    ## Les deux matrices
    pij<-as.matrix(a/apj)
    uij<-as.matrix(u)
    I<-ncol(uij)
    J<-nrow(uij)

    ## Calcul de l'IC de Bonferroni
    bonferroni <- alpha/(I * (I - 1)/2)
    upj<-apply(uij,1,sum)
    uip<-apply(uij,2,sum)
    wij<-uij/(upj*pij)
    wi<-uip/apply(pij*upj,2,sum)

    ## sorties
    sorties$used.prop <- t(t(uij)/uip)
    sorties$avail.prop <- pij
    sorties$wij<-wij
    sorties$wi<-wi

    ## Calcul des Khi2
    Khi2Lj<-matrix(0, nrow=J, ncol=3)
    colnames(Khi2Lj)<-c("Khi2Lj", "df", "pvalue")
    for (j in 1:J) {
      euij<-uij[j,]*log(uij[j,]/(upj[j]*pij[j,]))
      ddl<-length(euij[!is.na(euij)])-1
      euij<-euij[!is.na(euij)]
      Khi2Lj[j,1]<-sum(euij)
      Khi2Lj[j,2]<-ddl
      Khi2Lj[j,3]<-1 - pchisq(Khi2Lj[j,1], ddl)
    }

    rownames(Khi2Lj)<-rownames(u)
    
    sorties$Khi2Lj<-Khi2Lj
    Khi2L<-apply(Khi2Lj,2,sum)
    Khi2L[3]<-1 - pchisq(Khi2L[1],Khi2L[2])
    names(Khi2L)<-c("Khi2L", "df", "pvalue")
    sorties$Khi2L<-Khi2L

    ## Variance de wi
    vwi<-rep(0,I)
    for (i in 1:I) {
      yj<-uij[,i]
      xj<-pij[,i]*upj
      vwi[i]<-(sum((yj-wi[i]*xj)**2)/(J-1))*(1/(J*(mean(xj)**2)))
    }

    sewi<-sqrt(vwi)
    sorties$se.wi<-sewi
    sorties$ICwiupper<-round(wi+sewi*qnorm(1 - alpha/(2*I)), 4)
    sorties$ICwilower<-round(wi-sewi*qnorm(1 - alpha/(2*I)), 4)

    ## Diff�rences des selection ratios
    diffwi<-outer(wi,wi,"-")
    vardif<-matrix(0, I, I)
    ICdiffupper<-matrix(0, I, I)
    ICdifflower<-matrix(0, I, I)
    sig<-matrix("0", I, I)
    
    for (i in 1:I) {
      for (j in 1:I) {
        if (avknown) {
          ## dispo connue
          spi<-sum(pij[,i]*upj)
          spj<-sum(pij[,j]*upj)
          
          vardif[i,j]<-sum(((uij[,i]-wi[i]*upj)/spi +
                            (uij[,j]-wi[j]*upj)/spj )**2 )*(J/(J-1))
        }
        else {
          ## dispo inconnue
          dftmp<-data.frame(y1=uij[,i],y2=uij[,j],
                            x1=pij[,i]*upj, x2=pij[,j]*upj)
          vc<-var(dftmp)
          y1<-uip[i]
          y2<-uip[j]
          x1<-sum(pij[,i]*upj)
          x2<-sum(pij[,j]*upj)
          vardif[i,j]<-(1/(y1**2))*vc["x1","x1"] +
            ((x1**2)/(y1**4))*vc["y1","y1"] +
              (1/(y2**2))*vc["x2","x2"] +
                ((x2**2)/(y2**4))*vc["y2","y2"] -
                  2*(x1/(y1**3))*vc["x1","y1"] -
                    2*(1/(y1*y2))*vc["x1","x2"] +
                      2*(x2/(y1*(y2**2)))*vc["x1","y2"] +
                        2*(x1/(y2*(y1**2)))*vc["y1","x2"] -
                          2*((x1*x2)/((y1**2)*(y2**2)))*vc["y1","y2"] -
                            2*(x2/(y2**3))*vc["x2","y2"]
        }
        vardif[row(vardif)==col(vardif)]<-0

        ## calcul des ic...
        ICdiffupper[i, j] <- round(diffwi[i, j] +
                                   sqrt(vardif[i,j]) *
                                   qnorm(1 - bonferroni/2), 4)
        ICdifflower[i, j] <- round(diffwi[i, j] -
                                   sqrt(vardif[i,j]) *
                                   qnorm(1 - bonferroni/2), 4)
        sig[i, j] <- ifelse(diffwi[i, j] < 0, "-", "+")

        ## ... et de la signification  des diff�rences
        if (ICdiffupper[i, j] < 0)
          sig[i, j] <- "---"
        if (ICdifflower[i, j] > 0) 
          sig[i, j] <- "+++"
      }
    }
    
    rownames(diffwi) <- colnames(u)
    colnames(diffwi) <- colnames(u)
    rownames(ICdiffupper) <- colnames(u)
    colnames(ICdiffupper) <- colnames(u)
    rownames(ICdifflower) <- colnames(u)
    colnames(ICdifflower) <- colnames(u)
    rownames(sig) <- colnames(u)
    colnames(sig) <- colnames(u)
    sorties$avknown <- avknown
    sorties$comparisons$diffwi <- diffwi
    sorties$comparisons$ICdiffupper <- ICdiffupper
    sorties$comparisons$ICdifflower <- ICdifflower
    sorties$comparisons$signif <- sig
    sorties$profile <- profilehab(sig, wi)
    sorties$alpha <- alpha
    class(sorties) <- c("wiIII", "wi")
    return(sorties)
  }

