#include <math.h>
#include <time.h>
#include <string.h>
#include <stdlib.h>

void vecpermut (double *A, int *num, double *B);
double alea (void);
void aleapermutvec (double *a);
void trirapideintswap (int *v, int i, int j);
void trirapideint (int *x , int *num, int gauche, int droite);
void sqrvec (double *v1);
void getpermutation (int *numero, int repet);
void prodmatABC (double **a, double **b, double **c);
void prodmatAtAB (double **a, double **b);
void prodmatAtBC (double **a, double **b, double **c);
void prodmatAAtB (double **a, double **b);
void prodmatAtBrandomC (double **a, double **b, double **c, int*permut);
void taballoc (double ***tab, int l1, int c1);
void vecalloc (double **vec, int n);
void vecintalloc (int **vec, int n);
void freetab (double **tab);
void freevec (double *vec);
void freeintvec (int *vec);
void matcentrage (double **A, double *poili, char *typ);
void matmodiffc (double **tab, double *poili);
void matmodifcp (double **tab, double *poili);
void matmodifcs (double **tab, double *poili);
void matmodifcn (double **tab, double *poili);
void matmodifcm (double **tab, double *poili);
void DiagobgComp (int n0, double **w, double *d, int *rang);
void topoids(double *vec, int *n);
void multpoco(double **tab, double *poco);
void aleadistrivec(double *vec, double *no);
void randksel(int *fac, double *pu, int *nani, int *ni);
void rks(int *fac, double *pdsu, int *nani, int *nbani, int *nl);
void ksel(double *tab, int *fac, double *poidsut, int *nhab, 
	  int *nani, int *nloctot, double *ut, double *di,
	  double *marg, int *nombreani, double *eigenvp, double *poidsco, int *ewa);
void permutksel(double *tab, int *fac, double *poidsut, int *nhab,
		int *nani, int *nloctot, double *ut, double *di,
		double *marg, int *nombreani, int *npermut, 
		double *obseig, double *simeig, double *obsmarg,
		double *simmarg, double *eigenvp, double *simtout, double *poidsco,
		int *ewa);
void sahr2ksel(double *Usa, double *Uhr,  double *Ulo, int *nhab,
	       int *npix, int *nani, int *nlig, double *dud, 
	       int *fac, double *pu);
void nls2k(double *Usa, double *Uhr, int *nhab, 
	   int *npix, int *nani);
void rotxy(double *x, double *y, int k);
void shifthr(double **dispo, double **util, int *idl, int *idc);
void shr(double **carte, double **ze);
void sr(double *carter, double *zer, int *nlgr, int *ncgr);
void locrast(double *xgr, double *ygr, double *x, double *y,
	     double **carte);
void lr(double *xgri, double *ygri, double *xr, double *yr,
	double *carter, int *nco, int *nli, int *nlixy);
void getcarte(double **carte, double **kasc, int *indicecarte);
void gc(double *carter, double *kascr, int *nlgr, int *ncgr, int *nhab);
void comptePasNA(double **tab, int *nombre);
void videNA(double **entree, double **sortie, int *idcons);
void niche(double **X, double **Y, double *eig, double **mar);
void mvtfreeman(int *in, int *jn, int *dir, int *np);
void getcontour(double *grille, int *nlig, int *ncol, int *indicelig, 
		int *indicecol, int *lcont);
void lcontour(double *grille, int *nlig, int *ncol, int *lcont);
void levels(double *vec, double *lev, int *lvec);
void seqeticorr(double *grille, int *nlig, int *ncol);
void epa(double *X, double *Y, double *xl, double *yl, double *val, double *fen);
void kernelhr(double *grille, double *xgri, double *ygri, int *ncolgri,
	      int *nliggri, int *nloc, double *fen, double *xlo, double *ylo);
void CVmise(int *nloc, double *xlo, double *ylo,
	    double *hvec, double *CV, int *nhteste);
void calcvolume(double *grille, int *ncolgri, int *nliggri, double *cellsize);
void calcsim(double *pix, double **pts, double *rg, 
	     int *nvar, int *npts, double *similarite);
void fctdomain(double *kascmod, double *ptsmod, double *range, int *npts, int *npix,  
	       int *nvar, double *qualhab);
void wml(double **used, double **avail, double *wmla, int na, int nh,
	 double **proj1, double **proj2, double *nbassocie, int krep);
void aclambda(double *util, double *dispo, int *nani, int *nhab,  
	      double *xxtxxtmod1, double *xxtxxtmod2, double *rnv,
	      double *wmla, int *nrep, double *wm, double *nb);
void rankma(double *used, double *avail, double *rankmap, double *rankmam,
	    double *rankmav, double *rankmanb, int *nhab, int *nani, int *nrep, double *rnv);
void erodil(double *grille, int *nlig, int *ncol, int *ntour, int *oper);
void inout(double *x, double *y, double *xp, double *yp,
	   int *deds);
void inoutr(double *xr, double *yr, double *xpr, double *ypr,
	    int *dedsr, int *nxr, int *npr);
void rastpol(double *xp, double *yp, double *xg, double *yg,
	     double **carte);
void rastpolaire(double *xpr, double *ypr, double *xgr, double *ygr,
		 double *carter, int *nlg, int *ncg, int *nvp);
void calcniche(double **kasc, int *nvar, int *nlg, int *ncg,
	       double *margvar, double *tolvar, double **carte);
void calcnicher(double *kascr, int *nvar, int *nlg, int *ncg,
		double *margvar, double *tolvar, double *carter);
void randompol(double *xpr, double *ypr, double *kascr,
	       double *marg, double *tol, int *nvar,
	       double *xgr, double *ygr, int *nlr, 
	       int *ncr, int *nvpr, int *nrep);
void dedans(double *pts, double *xc, double *yc, double *na,
	    double cs, double **asc);
void dedansr(double *ptsr, double *xcr, double *ycr, double *na,
	     double *cs, double *ascr, int *nl, int *nc, int *nlocs);
void rpath(double **xp, double *rcx, double *rcy, double **asc, 
	   double **tabdist, double *dt, 
	   double *angles, double *xc, double *yc,
	   double *cs, int r);
void randpath(double *xpr, double *rcrx, double *rcry, double *ascr, 
	      double *xcr, double *ycr, double *csr,
	      double *tabdistr, double *dtr, double *anglesr, 
	      int *nlasc, int *ncasc, int *nltdr, int *nlocsr);
void joinkasc(double **xp, double **kasc, double **res, int nl, int nc,
	      double *xc, double *yc, double *cs);
void joinkascr(double *xpr, double *kascr, int *nlasc, int *ncasc,
	       double *xcr, double *ycr, double *cs, int *nlocs,
	       int *nvar, double *resr);
void randmargtol(double *xpr, double *rcrx, double *rcry, double *ascr, 
		 double *cwr, double *kascr, double *xcr, double *ycr,
		 double *csr,
		 double *tabdistr, double *dtr, double *anglesr, double *marr, 
		 double *tolr, int *nrepr, int *nlasc, 
		 int *ncasc, int *nvarr, int *nltdr, int *nlocsr);
void rpoint(double **xp, double *rcx, double *rcy, double **asc, 
	    double *xc, double *yc, double *cs);
void randmargtolpts(double *xpr, double *rcrx, double *rcry, double *ascr, 
		    double *cwr, double *kascr, double *xcr, double *ycr,
		    double *csr, double *marr, double *tolr, int *nrepr, int *nlasc, 
		    int *ncasc, int *nvarr, int *nlocsr);
void regroufacasc(double **asce, double **ascs, int *np,
		  int *nlev);
void regroufacascr(double *ascer, double *ascsr, int *npr,
		   int *nlevr, int *nle, int *nce, int *nls, 
		   int *ncs);
void regrouascnum(double **ascent, double **ascso);
void regrouascnumr(double *ascentr, double *ascsor, double *nler, double *ncer,
		   double *nlsr, double *ncsr);
void regroukasc(double *kascr, double *kascniou, int *nrow, 
		int *ncol, int *nvar, int *npix,
		int *typer, int *nrniou, int *ncniou);
void matmudemi(double **X, double **Y);
void matmudemir(double *Xr, double *Yr, int *ncr);
void enfa(double **Z, double *p, int *nvar, int *npix,
	  double *vp);
void enfar(double *Zr, double *pr, int *nvar, int *npix,
	   double *vpr);
void randenfa(double **Z, double *p, int *nrep, double *res);
void randenfar(double *Zr, double *pr, int *nvar, int *npix,
	       int *nrep, double *resr);




/*********************************************************************
 *********************************************************************
 *********                                                       *****
 *********               Les sources de ADE-4                    *****
 *********               --------------------                    *****
 *********************************************************************
 *********************************************************************
 */



/**************************/
double alea (void)
{
	double w;
	w = ((double) rand())/ (double)RAND_MAX;
	return (w);
}

/*************************/
void aleapermutvec (double *a)
{
  /* permute au hasard les ÚlÚments du vecteur a 
     Manly p. 42 Le vecteur est modifiÚ
     from Knuth 1981 p. 139 */
  int lig, i,j, k;
  double z;
  
  lig = a[0];
  for (i=1; i<=lig-1; i++) {
    j=lig-i+1;
    k = (int) (j*alea()+1);
    /* k = (int) (j*genrand()+1); */
    if (k>j) k=j;
		z = a[j];
		a[j]=a[k];
		a[k] = z;
	}
}


/*******************/	
void vecpermut (double *A, int *num, double *B)
{
/*---------------------------------------
* A est un vecteur n elements
* B est une vecteur n elements
* num est une permutation alŽatoire des n premiers entiers
* B contient en sortie les elements de A permutŽes
* ---------------------------------------*/

	int lig, lig1, lig2, i, k;
	
	lig = A[0];
	lig1 = B[0];
	lig2 = num[0];
	
	
	if ( (lig!=lig1) || (lig!=lig2) ) {
	  /* err_message ("Illegal parameters (vecpermut)");
	     closelisting(); */
	}
	
	for (i=1; i<=lig; i++) {
		k=num[i];
		B[i] = A[k];
	}
}

/*******************/	
void matcentrage (double **A, double *poili, char *typ)
{
	
	if (strcmp (typ,"nc") == 0) {
		return;
	} else if (strcmp (typ,"cm") == 0) {
		matmodifcm (A, poili);
		return;
	} else if (strcmp (typ,"cn") == 0) {
		matmodifcn (A, poili);
		return;
	} else if (strcmp (typ,"cp") == 0) {
		matmodifcp (A, poili);
		return;
	} else if (strcmp (typ,"cs") == 0) {
		matmodifcs (A, poili);
		return;
	} else if (strcmp (typ,"fc") == 0) {
		matmodiffc (A, poili);
		return;
	} else if (strcmp (typ,"fl") == 0) {
		matmodifcm (A, poili);
		return;
	}
}

/*********************/
void matmodifcm (double **tab, double *poili)
/*--------------------------------------------------
* tab est un tableau n lignes, m colonnes
* disjonctif complet
* poili est un vecteur n composantes
* la procedure retourne tab centre par colonne 
* pour la ponderation poili (somme=1)
* centrage type correspondances multiples
--------------------------------------------------*/
{
	double		poid;
	int 			i, j, l1, m1;
	double		*poimoda;
	double		x, z;

	l1 = tab[0][0];
	m1 = tab[1][0];
	vecalloc(&poimoda, m1);


	for (i=1;i<=l1;i++) {
		poid = poili[i];
		for (j=1;j<=m1;j++) {
			poimoda[j] = poimoda[j] + tab[i][j] * poid;
		}
	}
	
	for (j=1;j<=m1;j++) {
		x = poimoda[j];
		if (x==0) {
			for (i=1;i<=l1;i++) tab[i][j] = 0;
		} else {
		
			for (i=1;i<=l1;i++) {
				z = tab[i][j]/x - 1.0;
				tab[i][j] = z;
			}
		}
	}
	freevec (poimoda);
}

/*********************************************************/
void matmodifcn (double **tab, double *poili)
/*--------------------------------------------------
* tab est un tableau n lignes, p colonnes
* poili est un vecteur n composantes
* la procedure retourne tab norme par colonne 
* pour la ponderation poili (somme=1)
--------------------------------------------------*/
{
	double		poid, x, z, y, v2;
	int 			i, j, l1, c1;
	double		*moy, *var;

	l1 = tab[0][0];
	c1 = tab[1][0];

	vecalloc(&moy, c1);
	vecalloc(&var, c1);


/*--------------------------------------------------
* calcul du tableau centre/norme
--------------------------------------------------*/

	for (i=1;i<=l1;i++) {
		poid = poili[i];
		for (j=1;j<=c1;j++) {
			moy[j] = moy[j] + tab[i][j] * poid;
		}
	}
	
	for (i=1;i<=l1;i++) {
		poid=poili[i];
		for (j=1;j<=c1;j++) {
			x = tab[i][j] - moy[j];
			var[j] = var[j] + poid * x * x;
		}
	}
	
	for (j=1;j<=c1;j++) {
		v2 = var[j];
		if (v2<=0) v2 = 1;
		v2 = sqrt(v2);
		var[j] = v2;
	}
	
	for (i=1;i<=c1;i++) {
		x = moy[i];
		y = var[i];
		for (j=1;j<=l1;j++) {
			z = tab[j][i] - x;
			z = z / y;
			tab[j][i] = z;
		}
	}
	
	freevec(moy);
	freevec(var);
	
}

/*********************************************************/
void matmodifcs (double **tab, double *poili)
/*--------------------------------------------------
* tab est un tableau n lignes, p colonnes
* poili est un vecteur n composantes
* la procedure retourne tab standardise par colonne 
* pour la ponderation poili (somme=1)
--------------------------------------------------*/
{
	double		poid, x, z, y, v2;
	int 			i, j, l1, c1;
	double		*moy, *var;

	l1 = tab[0][0];
	c1 = tab[1][0];

	vecalloc(&var, c1);


/*--------------------------------------------------
* calcul du tableau standardise
--------------------------------------------------*/

	for (i=1;i<=l1;i++) {
		poid=poili[i];
		for (j=1;j<=c1;j++) {
			x = tab[i][j];
			var[j] = var[j] + poid * x * x;
		}
	}
	
	for (j=1;j<=c1;j++) {
		v2 = var[j];
		if (v2<=0) v2 = 1;
		v2 = sqrt(v2);
		var[j] = v2;
	}
	
	for (i=1;i<=c1;i++) {
		x = moy[i];
		y = var[i];
		for (j=1;j<=l1;j++) {
			z = tab[j][i];
			z = z / y;
			tab[j][i] = z;
		}
	}
	freevec(var);
}

/**********/
void matmodifcp (double **tab, double *poili)
/*--------------------------------------------------
* tab est un tableau n lignes, p colonnes
* poili est un vecteur n composantes
* la procedure retourne tab centre par colonne 
* pour la ponderation poili (somme=1)
--------------------------------------------------*/
{
	double		poid;
	int 			i, j, l1, c1;
	double		*moy, x, z;

	l1 = tab[0][0];
	c1 = tab[1][0];
	vecalloc(&moy, c1);


/*--------------------------------------------------
* calcul du tableau centre
--------------------------------------------------*/

	for (i=1;i<=l1;i++) {
		poid = poili[i];
		for (j=1;j<=c1;j++) {
			moy[j] = moy[j] + tab[i][j] * poid;
		}
	}
	
	
	for (i=1;i<=c1;i++) {
		x = moy[i];
		for (j=1;j<=l1;j++) {
			z = tab[j][i] - x;
			tab[j][i] = z;
		}
	}
	freevec(moy);
}

/*********************/
void matmodiffc (double **tab, double *poili)
/*--------------------------------------------------
* tab est un tableau n lignes, m colonnes
* de nombres positifs ou nuls
* poili est un vecteur n composantes
* la procedure retourne tab centre doublement 
* pour la ponderation poili (somme=1)
* centrage type correspondances simples
--------------------------------------------------*/
{
	double		poid;
	int 			i, j, l1, m1;
	double		*poimoda;
	double		x, z;

	l1 = tab[0][0];
	m1 = tab[1][0];
	vecalloc(&poimoda, m1);


	for (i=1;i<=l1;i++) {
		x = 0;
		for (j=1;j<=m1;j++) {
			x = x + tab[i][j];
		}
		if (x!=0) {
			for (j=1;j<=m1;j++) {
				tab[i][j] = tab[i][j]/x;
			}
		}	
	}

	for (i=1;i<=l1;i++) {
		poid = poili[i];
		for (j=1;j<=m1;j++) {
			poimoda[j] = poimoda[j] + tab[i][j] * poid;
		}
	}
	
	for (j=1;j<=m1;j++) {
		x = poimoda[j];
		if (x==0) {
		  /* err_message("column has a nul weight (matmodiffc)"); */
		}
		
		for (i=1;i<=l1;i++) {
			z = tab[i][j]/x - 1.0;
			tab[i][j] = z;
		}
	}
	freevec (poimoda);
}

/*****************/
void getpermutation (int *numero, int repet)
/*----------------------
* affectation d'une permutation alÚatoire des n premiers entiers 
* dans dans un vecteur d'entiers de dimension n
* vecintalloc prÚalable exigÚ
* *numero est un vecteur d'entier
* repet est un entier qui peut prendre une valeur arbitraire
* utilise dans le germe du generateur de nb pseudo-aleatoires
* si on l'incremente dans des appels repetes (e.g. simulation) garantit
* que deux appels donnent deux resultats distincts (seed=clock+repet)
------------------------*/
{
	int i, n, seed;
	int *alea;
	
	n=numero[0];
	vecintalloc (&alea,n);
	
	/*-------------
	* numerotation dans numero
	-----------*/
	for (i=1;i<=n;i++) {
		numero[i]=i;
	}
	
	/*-------------
	* affectation de nombres aleatoires dans alea
	----------------*/
	seed = clock();
	seed = seed + repet;
	srand(seed);
	for (i=1;i<=n;i++) {
		alea[i]=rand();
	}
	
	trirapideint (alea , numero, 1, n);
	freeintvec (alea);
}

/*****************************************/
void trirapideint (int *x , int *num, int gauche, int droite)
{
	int j, dernier, milieu, t;
	
	if ( (droite-gauche)<=0) return;
	
	milieu = (gauche+droite)/2;
	trirapideintswap (x, gauche, milieu);
	trirapideintswap (num, gauche, milieu);
	
	t=x[gauche];
	dernier=gauche;
	for (j = gauche+1; j<=droite; j++) {
		if (x[j] < t) {
			dernier = dernier + 1;
			trirapideintswap (x, dernier, j);	
			trirapideintswap (num, dernier, j);
		}
	}
	trirapideintswap (x, gauche, dernier);
	trirapideintswap (num, gauche, dernier);
	
	trirapideint (x, num, gauche, dernier-1);
	trirapideint (x, num, dernier+1, droite);
		
}

/**************************************/
void trirapideintswap (int *v, int i, int j)
{
	int provi;
	
	provi=v[i];
	v[i]=v[j];
	v[j]=provi;
}

/***********************************************************************/
void sqrvec (double *v1)
/*--------------------------------------------------
* Racine carree des elements d'un vecteur
--------------------------------------------------*/
{
	int i, c1;
	double v2;
	
	c1 = v1[0];
	
	for (i=1;i<=c1;i++) {
		v2 = v1[i];
		/* if (v2 < 0.0) err_message("Error: Square root of negative number (sqrvec)"); */
		v2 = sqrt(v2);
		v1[i] = v2;
	}
}

/***********************************************************************/
void DiagobgComp (int n0, double **w, double *d, int *rang)
/*--------------------------------------------------
* Diagonalisation
* T. FOUCART Analyse factorielle de tableaux multiples,
* Masson, Paris 1984,185p., p. 62. D'apr?s VPROP et TRIDI,
* de LEBART et coll.
--------------------------------------------------*/
{
	double			*s;
	double			a, b, c, x, xp, q, bp, ab, ep, h, t, u , v;
	double			dble;
	int				ni, i, i2, j, k, jk, ijk, ij, l, ix, m, m1, isnou;
	
	vecalloc(&s, n0);
	a = 0.000000001;
	ni = 100;
	if (n0 == 1) {
		d[1] = w[1][1];
		w[1][1] = 1.0;
		*rang = 1;
		freevec (s);
		return;
	}
	
	for (i2=2;i2<=n0;i2++) {
		
		b=0.0;
		c=0.0;
		i=n0-i2+2;
		k=i-1;
		if (k < 2) goto Et1;
		for (l=1;l<=k;l++) {
			c = c + fabs((double) w[i][l]);
		}
		if (c != 0.0) goto Et2;
		
Et1:	s[i] = w[i][k];
		goto Etc;
		
Et2:	for (l=1;l<=k;l++) {
			x = w[i][l] / c;
			w[i][l] = x;
			b = b + x * x;
		}
		xp = w[i][k];
		ix = 1;
		if (xp < 0.0) ix = -1;
		
/*		q = -sqrt(b) * ix; */
		dble = b;
		dble = -sqrt(dble);
		q = dble * ix;

		s[i] = c * q;
		b = b - xp * q;
		w[i][k] = xp - q;
		xp = 0;
		for (m=1;m<=k;m++) {
			w[m][i] = w[i][m] / b / c;
			q = 0;
			for (l=1;l<=m;l++) {
				q = q + w[m][l] * w[i][l];
			}
			m1 = m + 1;
			if (k < m1) goto Et3;
			for (l=m1;l<=k;l++) {
				q = q + w[l][m] * w[i][l];
			}
			
Et3:		s[m] = q / b;
			xp = xp + s[m] * w[i][m];
		}
		bp = xp * 0.5 / b;
		for (m=1;m<=k;m++) {
			xp = w[i][m];
			q = s[m] - bp * xp;
			s[m] = q;
			for (l=1;l<=m;l++) {
				w[m][l] = w[m][l] - xp * s[l] - q * w[i][l];
			}
		}
		for (l=1;l<=k;l++) {
			w[i][l] = c * w[i][l];
		}
		
Etc:	d[i] = b;
	} /* for (i2=2;i2<n0;i2++) */
	
	s[1] = 0.0;
	d[1] = 0.0;
	
	for (i=1;i<=n0;i++) {
		
		k = i - 1;
		if (d[i] == 0.0) goto Et4;
		for (m=1;m<=k;m++) {
			q = 0.0;
			for (l=1;l<=k;l++) {
				q = q + w[i][l] * w[l][m];
			}
			for (l=1;l<=k;l++) {
				w[l][m] = w[l][m] - q * w[l][i];
			}
		}
		
Et4:	d[i] = w[i][i];
		w[i][i] = 1.0;
		if (k < 1) goto Et5;
		for (m=1;m<=k;m++) {
			w[i][m] = 0.0;
			w[m][i] = 0.0;
		}

Et5:;
	}
	
	for (i=2;i<=n0;i++) {
		s[i-1] = s[i];
	}
	s[n0] = 0.0;
	
	for (k=1;k<=n0;k++) {

		m = 0;

Et6: 	for (j=k;j<=n0;j++) {
			if (j == n0) goto Et7;
			ab = fabs((double) s[j]);
			ep = a * (fabs((double) d[j]) + fabs((double) d[j+1]));
			if (ab < ep) goto Et7;
		}
	
Et7: 	isnou = 1;
		h = d[k];
		if (j == k) goto Eta;
		if (m < ni) goto Etd;
		
		/* err_message("Error: can't compute matrix eigenvalues"); */
		
Etd:	m = m + 1;
		q = (d[k+1]-h) * 0.5 / s[k];
		
/*		t = sqrt(q * q + 1.0); */
		dble = q * q + 1.0;
		dble = sqrt(dble);
		t = dble;
		
		if (q < 0.0) isnou = -1;
		q = d[j] - h + s[k] / (q + t * isnou);
		u = 1.0;
		v = 1.0;
		h = 0.0;
		jk = j-k;
		for (ijk=1;ijk<=jk;ijk++) {
			i = j - ijk;
			xp = u * s[i];
			b = v * s[i];
			if (fabs((double) xp) < fabs((double) q)) goto Et8;
			u = xp / q;
			
/*			t = sqrt(u * u + 1); */
			dble = u * u + 1.0;
			dble = sqrt(dble);
			t = dble;
			
			s[i+1] = q * t;
			v = 1 / t;
			u = u * v;
			goto Et9;

Et8:		v = q / xp;

/*			t = sqrt(1 + v * v); */
			dble = 1.0 + v * v;
			dble = sqrt(dble);
			t = dble;
			
			s[i+1] = t * xp;
			u = 1 / t;
			v = v * u;

Et9:
			q = d[i+1] - h;
			t = (d[i] - q) * u + 2.0 * v * b;
			h = u * t;
			d[i+1] = q + h;
			q = v * t - b;
			for (l=1;l<=n0;l++) {
				xp = w[l][i+1];
				w[l][i+1] = u * w[l][i] + v * xp;
				w[l][i] = v * w[l][i] - u * xp;
			}
		}
		d[k] = d[k] - h;
		s[k] = q;
		s[j] = 0.0;
		
		goto Et6;

Eta:;
	} /* for (k=1;k<=n0;k++) */
	
	for (ij=2;ij<=n0;ij++) {
		
		i = ij - 1;
		l = i;
		h = d[i];
		for (m=ij;m<=n0;m++) {
			if (d[m] >= h) {
				l = m;
				h = d[m];
			}
		}
		if (l == i) {
			goto Etb;
		} else {
			d[l] = d[i];
			d[i] = h;
		}
		for (m=1;m<=n0;m++) {
			h = w[m][i];
			w[m][i] = w[m][l];
			w[m][l] = h;
		}

Etb:;
	} /* for (ij=2;ij<=n0;ij++) */

	/* final:; */
	*rang = 0;
	for (i=1;i<=n0;i++) {
		/*
		if (d[i] / d[1] < 0.00001) d[i] = 0.0;
		if (d[i] != 0.0) *rang = *rang + 1;
		*/
		if (d[i] > 0.0) *rang = *rang + 1;
	}
	freevec(s);
} /* DiagoCompbg */

/***********************************************************************/
void prodmatABC (double **a, double **b, double **c)
/*--------------------------------------------------
* Produit matriciel AB
--------------------------------------------------*/
{
	int j, k, i, lig, col, col2;
	double s;
	
	lig = a[0][0];
	col = a[1][0];
	
	col2 = b[1][0];

	for (i=1;i<=lig;i++) {
		for (k=1;k<=col2;k++) {
			s = 0;
			for (j=1;j<=col;j++) {
				s = s + a[i][j] * b[j][k];
			}
		c[i][k] = s;
		}		
	}
}

/***********************************************************************/
void prodmatAtAB (double **a, double **b)
/*--------------------------------------------------
* Produit matriciel AtA
--------------------------------------------------*/
{
	int j, k, i, lig, col;
	double s;
	
	lig = a[0][0];
	col = a[1][0];

	for (j=1;j<=col;j++) {
		for (k=j;k<=col;k++) {
			s = 0;
			for (i=1;i<=lig;i++) {
				s = s + a[i][k] * a[i][j];
			}
		b[j][k] = s;
		b[k][j] = s;
		}		
	}
}

/***********************************************************************/
void prodmatAtBC (double **a, double **b, double **c)
/*--------------------------------------------------
* Produit matriciel AtB
--------------------------------------------------*/
{
	int j, k, i, lig, col, col2;
	double s;
	
	lig = a[0][0];
	col = a[1][0];
	
	col2 = b[1][0];

	for (j=1;j<=col;j++) {
		for (k=1;k<=col2;k++) {
			s = 0;
			for (i=1;i<=lig;i++) {
				s = s + a[i][j] * b[i][k];
			}
		c[j][k] = s;
		}		
	}
}


/***********************************************************************/
void prodmatAAtB (double **a, double **b)
/*--------------------------------------------------
* Produit matriciel B = AAt
--------------------------------------------------*/
{
	int j, k, i, lig, col;
	double s;
	
	lig = a[0][0];
	col = a[1][0];

	for (j=1;j<=lig;j++) {
		for (k=j;k<=lig;k++) {
			s = 0;
			for (i=1;i<=col;i++) {
				s = s + a[j][i] * a[k][i];
			}
		b[j][k] = s;
		b[k][j] = s;
		}		
	}
}

/*******************/
void prodmatAtBrandomC (double **a, double **b, double **c, int*permut)
/*--------------------------------------------------
* Produit matriciel AtB
* les lignes de B sont permutÚes par la permutation permut
--------------------------------------------------*/
{
	int j, k, i, i0, lig, col, col2;
	double s;
	
	lig = a[0][0];
	col = a[1][0];
	
	col2 = b[1][0];

	for (j=1;j<=col;j++) {
		for (k=1;k<=col2;k++) {
			s = 0;
			for (i=1;i<=lig;i++) {
				i0 = permut[i];
				s = s + a[i][j] * b[i0][k];
			}
		c[j][k] = s;
		}		
	}
}

/***********************************************************************/
void taballoc (double ***tab, int l1, int c1)
/*--------------------------------------------------
* Allocation de memoire dynamique pour un tableau (l1, c1)
--------------------------------------------------*/
{
	int i, j;
	
	if ( (*tab = (double **) calloc(l1+1, sizeof(double *))) != 0) {
		for (i=0;i<=l1;i++) {
			if ( (*(*tab+i)=(double *) calloc(c1+1, sizeof(double))) == 0 ) {
				return;
				for (j=0;j<i;j++) {
					free(*(*tab+j));
				}
			}
		}
	}

	**(*tab) = l1;
	**(*tab+1) = c1;
}

/***********************************************************************/
void vecalloc (double **vec, int n)
/*--------------------------------------------------
* Allocation de memoire pour un vecteur de longueur n
--------------------------------------------------*/
{
	if ( (*vec = (double *) calloc(n+1, sizeof(double))) != 0) {
		**vec = n;
		return;
	} else {
		return;
	}
}

/*****************/
void vecintalloc (int **vec, int n)
/*--------------------------------------------------
* Allocation de memoire pour un vecteur d'entiers de longueur n
--------------------------------------------------*/
{
  if ( (*vec = (int *) calloc(n+1, sizeof(int))) != NULL) {
    **vec = n;
    return;
  } else {
    return;
  }
}

/***********************************************************************/
void freetab (double **tab)
/*--------------------------------------------------
* Allocation de memoire dynamique pour un tableau (l1, c1)
--------------------------------------------------*/
{
	int 	i, n;
	
	n = *(*(tab));
	for (i=0;i<=n;i++) {
			free((char *) *(tab+i) );
	}
	free((char *) tab);
}

/***********************************************************************/
void freevec (double *vec)
/*--------------------------------------------------
* liberation de memoire pour un vecteur
--------------------------------------------------*/
{
	free((char *) vec);	
}

/***********************************************************************/
void freeintvec (int *vec)
/*--------------------------------------------------
* liberation de memoire pour un vecteur
--------------------------------------------------*/
{
	
	free((char *) vec);
	
}









/*********************************************************************
 *********************************************************************
 *************                               *************************
 *************         MES FONCTIONS         *************************
 *************         -------------         *************************
 *********************************************************************
 *********************************************************************
 */

/* Convertit un vecteur de façon à ce que la somme de tous 
   les éléments du vecteur fasse 1 */

void topoids(double *vec, int *n)
{
  int i;
  double somme;
  somme = 0;
  

    for (i=0; i<=(*n-1); i++){
      somme = somme + vec[i];
    }
  
    for (i=0; i<=(*n-1); i++){
      vec[i] = vec[i] / somme;
    }
    *n=somme;
    
}



/* Multiplie un tableau par la racine carrée du poids des colonnes */

void multpoco(double **tab, double *poco)
{
  int nc, nl, i, j;
  double k;
  
  nl = tab[0][0];
  nc = tab[1][0];
  
  for (i = 1; i <= nl; i++) {
    for (j = 1; j <= nc; j++) {
      k = poco[j];
      tab[i][j] = tab[i][j]*sqrt(k);
    }
  }
  
}

/* distribue no points aléatoirement dans un vecteur à p composantes */

void aleadistrivec(double *vec, double *no)
{
  /* Déclaration des variables */
  double tmp, i, j, n, lv;

  n = *no;
  lv = vec[0];
  
  
  for (i=1; i<=n; i++) {
    tmp = alea();
    for (j=1; j<=lv; j++) {
      if ((tmp >= (j-1)/lv)&&(tmp < j/lv))
	vec[(int) j]++;
    }
  }
}





/* *****************************************************
   Va faire de même, mais avec les structures de données
   utilisées par la K-select (i.e., par animal)
   ***************************************************** */

void randksel(int *fac, double *pu, int *nani, int *ni)
{
  /* Déclaration de variables locales */
  int i, j, k, l;
  double su, *tmp;
  
  l=1;
  
  for (k=1; k<=*nani; k++) {
    vecalloc(&tmp, ni[k]);
    su = 0;
    
    for (i=1; i<=ni[k];i++) {
      su = su+pu[l];
      l++;
    }
    
    aleadistrivec(tmp, &su);
    
    j=1;
    for (i=(l-ni[k]); i<l; i++) {
      pu[i]=tmp[j];
      j++;
    }

    freevec(tmp);
  }
 
}


/* *****************************************************
           Version interactive avec R de randksel
   ***************************************************** */

void rks(int *fac, double *pdsu, int *nani, int *nbani, int *nl)
{
  int i, *fa, *ni;
  double *pu;
  
  vecalloc(&pu, *nl);
  vecintalloc(&fa, *nl);
  vecintalloc(&ni, *nani);
  
  for (i=1; i<=*nl; i++) {
    fa[i] = fac[i-1];
  }
  
  for (i=1; i<=*nl; i++) {
    pu[i] = pdsu[i-1];
  }

  for (i=1; i<=*nani; i++) {
    ni[i] = nbani[i-1];
  }
  
  
  randksel(fa, pu, nani, ni);
  
  for (i=1; i<=*nl; i++) {
    pdsu[i-1] = pu[i];
  }
  
  freevec(pu);
  freeintvec(fa);
  freeintvec(ni);
    

}



/* ****************************************************************
   *                                                              *
   *              Analyse K-select                                *
   *                                                              *
   **************************************************************** */


void ksel(double *tab, int *fac, double *poidsut, int *nhab, 
	  int *nani, int *nloctot, double *ut, double *di,
	  double *marg, int *nombreani, double *eigenvp, double *poidsco, int *ewa)
{
  
  /* Déclaration des variables locales */
  int i,j,k, sommeloctot;
  double **ta, *pu, **use, **ava, **mar, *poidsani, **inertie, *valpro, *spu, *poco;
  int nh, na, nl, rang;
  int *fa, *ni;
  

  
  /* Allocation de mémoire pour les variables locales */
  nl = *nloctot; /* nombre total de pixels */
  na = *nani; /* nombre d'animaux suivis */
  nh = *nhab; /* nombre de variables */
  
  vecintalloc (&fa, nl); /* facteur avec un niveau par animal */
  vecalloc (&pu, nl); /* poids d'utilisation des pixels */
  vecalloc (&poidsani, na); /* poids associé à chaque animal (proportionnel au nb de locs) */
  vecalloc (&valpro, nh); /* valeurs propres de l'analyse */
  vecalloc (&spu, na); /* poids d'utilisations pondérés à 1 */
  vecalloc(&poco, nh); /* poids des colonnes dans l'analyse (les habitats, quoi) */
  taballoc (&ta, nl, nh); /* le tableau de départ */
  taballoc (&use, na, nh); /* le tableau des moyennes utilisées */
  taballoc (&ava, na, nh); /* le tableau des moyennes dispo */
  taballoc (&mar, na, nh); /* le tableau des marginalités */
  taballoc (&inertie, nh, nh); /* le tableau destiné à recevoir la matrice d'inertie */
  vecintalloc(&ni, na); /* le nombre de pixels dispo par animal */

  /* On recopie les objets R dans les variables C locales */
  k = 0;
  for (i=1; i<=nl; i++) {
    for (j=1; j<=nh; j++) {
      ta[i][j] = tab[k];
      k = k + 1;
    }
  }

  for (i=1; i<=nl; i++) {
    fa[i] = fac[i-1];
  }
  
  for (i=1; i<=nl; i++) {
    pu[i] = poidsut[i-1];
  }
  
  for (i=1; i<=nh; i++) {
    poco[i] = poidsco[i-1];
  }
  
  for (i=1; i<=na; i++) {
    spu[i] = 0;
  }
  
  
  for (i=1; i<=na; i++) {
    ni[i] = nombreani[i-1];
  }
  
  
  /* Calcul du nombre de locs par animal */
  for (i=1; i<=na; i++) {
    for (k=1; k<=nl; k++) {
      if (fa[k]==i) {
	spu[i] = spu[i] + pu[k];	
      }
    }
  }
  

  /* Calcul du nombre total de localisations */
  sommeloctot=0;
  for (i=1; i<=na; i++) {
    sommeloctot = sommeloctot + spu[i];	
  }
  
  
  /* Calcul de la moyenne utilisée et de la moyenne dispo */
  for (i=1; i<=na; i++) {
    for (j=1; j<=nh; j++) {
      for (k=1; k<=nl; k++) {
	if (fa[k]==i) {
	  ava[i][j]= ava[i][j] + ta[k][j]/ni[i];
	  use[i][j]=use[i][j] + (ta[k][j] * pu[k]);
	}
      }
    }
  }
  
  for (i=1; i<=na; i++) {
    for (j=1; j<=nh; j++) {
      use[i][j] = use[i][j] / spu[i];
      mar[i][j] = use[i][j] - ava[i][j];
    }
  }

  /* Que l'on recopie dans le pointeur vers objet R */
  k = 0;
  for (i=1; i<=na; i++) {
    for (j=1; j<=nh; j++) {
      ut[k] = use[i][j];
      di[k] = ava[i][j];
      marg[k] = mar[i][j];
      k = k + 1;
    }
  }

  /* ponderation des colonnes */
  multpoco(mar, poco);
  

  /* Calcul du poids de chaque animal */
  for (i=1; i<=na; i++) {
    if (*ewa==0)
      poidsani[i] = (double) spu[i] / sommeloctot;
    if(*ewa==1)
      poidsani[i] = (double) 1 / na;
  }

  sqrvec(poidsani);

  

  for (i=1; i<=na; i++) {
    for (j=1; j<=nh; j++) {
      mar[i][j] = mar[i][j] * poidsani[i];
    }
  }

  prodmatAtAB(mar, inertie);
  DiagobgComp(nh, inertie, valpro, &rang);
  
  /* Sorties vers R */

  for (i = 1; i<=rang; i++) {
    eigenvp[i-1] = valpro[i];
  }

  


  freeintvec (fa);
  freeintvec (ni);
  freevec (pu);
  freevec (poidsani);
  freevec(valpro);
  freevec (spu);
  freevec(poco);
  
  freetab (ta);
  freetab (use);
  freetab (ava);
  freetab (mar);
  freetab (inertie);
  
}






/* ****************************************************************
   *                                                              *
   *         Test de randomisation associé à la k-select          *
   *                                                              *
   **************************************************************** */

void permutksel(double *tab, int *fac, double *poidsut, int *nhab,
		int *nani, int *nloctot, double *ut, double *di,
		double *marg, int *nombreani, int *npermut, 
		double *obseig, double *simeig, double *obsmarg,
		double *simmarg, double *eigenvp, double *simtout, double *poidsco,
		int *ewa)
{
  /* Déclaration des variables*/
  double **ta, *pu, *obstout;
  int na, nh, nl, i, j, k, l, m, q, *ni, *fa, *numero, nbperm;
  
  /* Allocation de mémoire */
  na=*nani;
  nh=*nhab;
  nl=*nloctot;
  nbperm=*npermut;
    
  taballoc(&ta, nl, nh);
  vecintalloc(&fa, nl);
  vecintalloc(&numero, nbperm);
  vecalloc(&pu, nl);
  vecalloc(&obstout, na*nh);
  vecintalloc(&ni, na);
  
  
  /* On copie de R vers C (variables locales) */
  k = 0;
  for (i=1; i<=nl; i++) {
    for (j=1; j<=nh; j++) {
      ta[i][j] = tab[k];
      k = k + 1;
    }
  }
  
  for (i=1; i<=nl; i++) {
    fa[i] = fac[i-1];
  }
  
  for (i=1; i<=nl; i++) {
    pu[i] = poidsut[i-1];
  }
  
  for (i=1; i<=na; i++) {
    ni[i] = nombreani[i-1];
  }
  
  /* Calculs */
  /* kselect de base */
  ksel(tab,  fac, poidsut, nhab, 
       nani, nloctot, ut, di,
       marg, nombreani, eigenvp, poidsco, ewa);
  
  
  /* On place les valeurs observées dans les sorties */
  k=0;
  j=0;
  *obseig = eigenvp[0];
  for (i=0; i<(na*nh); i++) {
    obsmarg[j] = obsmarg[j] + (marg[i] * marg[i] * poidsco[k]);
    if (k==(nh-1)) {
      k=-1;
      j++;
    }
    k++;
  }
  
  /* Valeurs tout */
  for (i=1; i<=na*nh; i++) {
    obstout[i] = marg[i-1];
  }
  
  
  
  /* Les permutations */
  m=0; /* servira à voir où qu'on en est pour simmarg */
  q=0; /* servira à voir où qu'on en est pour simtout */
  
  for (k=1; k<=nbperm; k++) {

    /* On permute */
    randksel(fa, pu, nani, ni);
    
    /* On recopie les poids d'utilisation randomisés dans poidsut */
    for (i=0; i<nl; i++) {
      poidsut[i]=pu[i+1];
    }

    /* et zou */
    ksel(tab,  fac, poidsut, nhab, 
	 nani, nloctot, ut, di,
	 marg, nombreani, eigenvp, poidsco, ewa);
    
    
    /* On place les valeurs simulées dans les sorties */
    simeig[k-1] = eigenvp[0];
    l=0;

    for (i=0; i<(na*nh); i++) {
      simmarg[m] = simmarg[m] + (marg[i] * marg[i] * poidsco[l]);
      if (l==(nh-1)) {
	l=-1;
	m++;
      }
      l++;
    }
    
    /* que l'on place dans simtout */
    for (i=0; i<(na*nh); i++) {
      simtout[q] = marg[i];
      q++;
    }
  }
  
  /* Et on remet obstout dans marg */
    for (i=0; i<(na*nh); i++) {
      marg[i] = obstout[i+1];
    }
      
    
  /* Libération de mémoire */
  freetab(ta);
  freeintvec(fa);
  freevec(pu);
  freevec(obstout);
  freeintvec(ni);
  freeintvec(numero);
}








/* ****************************************************************
   *                                                              *
   *         convertit un sahrlocs en kselect                     *
   *                                                              *
   **************************************************************** */

/* Passer le nombre de lignes total en argument */
void sahr2ksel(double *Usa, double *Uhr,  double *Ulo, int *nhab,
	       int *npix, int *nani, int *nlig, double *dud, 
	       int *fac, double *pu)
{
  /* déclaration des variables */
  int i,j,k,l,na,nh,np, nl;
  double **SA, **HR, **LOCS, **sortie;
  int *idna;
  /* idna contiendra 1 pour les pixels non-NA sur SA */

  
  /* Allocation de mémoire */
  na = *nani;
  nh = *nhab;
  np = *npix;
  nl = *nlig;
  
  taballoc(&SA, np, nh);
  taballoc(&HR, np, na);
  taballoc(&LOCS, np, na);
  taballoc(&sortie, nl, nh);
  vecintalloc(&idna, np);  

  /* Recopiage dans les variables C locales */
  k = 0;
  for (i=1; i<=np; i++) {
    for (j=1; j<=nh; j++) {
      SA[i][j] = Usa[k];
      k = k + 1;
    }
  }

  k = 0;
  for (i=1; i<=np; i++) {
    for (j=1; j<=na; j++) {
      HR[i][j] = Uhr[k];
      LOCS[i][j] = Ulo[k];
      k = k + 1;
    }
  }

  
  /* Calculs */
  /* Calcul du nombre de lignes du tableau de sortie */
  for (i=1; i<=np; i++) {
    if (SA[i][1] != -9999) {
      idna[i] = 1; /* = 1 si non NA */
    }
  }
  
  /* Et enfin, tableau de sortie */
  l=0;
  
  for (i=1; i<=np; i++) {
    for (j=1; j<=na; j++) {
      if ((idna[i]==1)&&(HR[i][j]==1)) {
	l++;
	for (k=1; k<=nh; k++) {
	  sortie[l][k] = SA[i][k]; /* Que l'on passera à dud après */
	  fac[l-1] = j;
	  pu [l-1] = LOCS[i][j];
	}
      }
    }
  }

  /* On repasse sortie sous R */
  k = 0;
  for (i=1; i<=nl; i++) {
    for (j=1; j<=nh; j++) {
      dud[k] = sortie[i][j];
      k++;
    }
  }
  
  
  
  /* Libération de mémoire */
  freetab(SA);
  freetab(HR);
  freetab(LOCS);
  freetab(sortie);
  freeintvec(idna);


}


/* ****************************************************************
   *                                                              *
   *         Calcul du nombre de ligne du tableau de sorties      *
   *                                                              *
   **************************************************************** */

void nls2k(double *Usa, double *Uhr, int *nhab, 
	   int *npix, int *nani)
{
  /* déclaration des variables */
  int i,j,k,na,nh,np,nl;
  double **SA, **HR;
  int *ni; /* nombre de pixels pour chaque animal */
  int *idna;
  /* idna contiendra 1 pour les pixels non-NA sur SA */
  
  
  /* Allocation de mémoire */
  na = *nani;
  nh = *nhab;
  np = *npix;
  
  taballoc(&SA, np, nh);
  taballoc(&HR, np, na);
  vecintalloc(&ni, na);
  vecintalloc(&idna, np);  
  
  /* Recopiage dans les variables C locales */
  k = 0;
  for (i=1; i<=np; i++) {
    for (j=1; j<=nh; j++) {
      SA[i][j] = Usa[k];
      k = k + 1;
    }
    idna[i] = 1;
  }

  k=0;
  for (i=1; i<=np; i++) {
    for (j=1; j<=na; j++) {
      HR[i][j] = Uhr[k];
      k = k + 1;
    }
  }

  
  /* Calculs */
  /* Calcul du nombre de lignes du tableau de sortie */
  for (i=1; i<=np; i++) {
    if (SA[i][1] == -9999) {
      idna[i] = 0; /* = 1 si non NA */
    }
  }
  
  /* Calcul de ni */
  for (j=1; j<=na; j++) {
    ni[j] = 0;
  }

  for (i=1; i<=np; i++) {
    if (idna[i] == 1) {
      for (j=1; j<=na; j++) {
	if (HR[i][j]==1) {
	  ni[j] = ni[j]+1;
	}
      }
    }
  }
  
  
  /* Nombre de lignes total du tableau de sortie */
  nl=0;
  for (i=1; i<=na; i++) {
    nl = nl + ni[i];
  }
  *nani = nl;
  
  
  /* libération de la mémoire */
  freetab(SA);
  freetab(HR);
  freeintvec(ni);
  freeintvec(idna);
}




/* ****************************************************************
   *                                                              *
   * rotxy pour faire tourner de façon aléatoire un couple (x,y)  *
   *                                                              *
   **************************************************************** */

void rotxy(double *x, double *y, int k)
{
  /* Déclaration des variables */
  int i, n, *numero;
  double mx, my, *angle, *angleb, ang, co, si, xt, yt;
  
  /* Calcul de la moyenne */
  mx=0;
  my=0;
  n=x[0];
  
  vecalloc(&angle, 360);
  vecalloc(&angleb, 360);
  vecintalloc(&numero, 360);
  
  for (i=1; i<=n; i++) {
    mx = mx + x[i];
    my = my + y[i];
  }
  
  mx = mx / n;
  my = my / n;
  
  /* Centrage */
  for (i=1; i<=n; i++) {
    x[i] = x[i]-mx;
    y[i] = y[i]-my;
  }
  
  /* Tirage au sort d'un angle entre 0 et 2pi */
  for (i=1; i<=360; i++) {
    angle[i] = (((double) i)*3.14159265359)/180;
  }

  /* et zou */
  getpermutation(numero, k);
  vecpermut(angle, numero, angleb);
  ang = angleb[1];
  co = cos(ang);
  si = sin(ang);
  
  for (i=1; i<=n; i++) {
    xt = x[i];
    yt = y[i];
    
    x[i]= co * xt - si * yt + mx;
    y[i]= si * xt + co * yt + my;
  }
  
  /* libé de la mémoire */
  
  freevec(angle);
  freevec(angleb);
  freeintvec(numero);
}


/* ****************************************************************
   *                                                              *
   * shifthr pour placer un DV de facon aléatoire sur une zone.   *
   * entrée: asc qui décrit quoi dispo, et sous matrice           *
   * qui décrit cellules ou au moins une loc                      *
   *                                                              *
   **************************************************************** */

void shifthr(double **dispo, double **util, int *idl, int *idc)
{
  /* Déclaration variables locales */
  int i, j, l, ncgr, nlgr, ncpe, nlpe;
  int *idlgr, *idcgr, crand, lrand;
  
  /* Allocation de mémoire */
  nlgr = dispo[0][0];
  ncgr = dispo[1][0];
  nlpe = util[0][0];
  ncpe = util[1][0];

  vecintalloc(&idcgr, ncgr-ncpe+1);
  vecintalloc(&idlgr, nlgr-nlpe+1);
  
  /* ************** Tirage au sort x et y DV *********** 
     Deux conditions:
     1. carré DV tient dans ZE
     2. pas de 0 où locs
  */
  l=0;
  
  while (l==0) {
    getpermutation(idcgr, *idc); /* tirage au sort colonne */
    getpermutation(idlgr, *idl); /* tirage au sort ligne */
    crand = idcgr[1];
    lrand = idlgr[1];
    
    l=1;
    for (i=1; i<=nlpe; i++) {
      for (j=1; j<=ncpe; j++) {
	if (util[i][j]>0) {
	  if (dispo[i+lrand-1][j+crand-1]==-9999) {
	    l=0;
	  }
	}
      }
    }
  }
  
  *idl=lrand;
  *idc=crand;

  /* Libé de la mémoire */
  freeintvec(idcgr);
  freeintvec(idlgr);

}




/* ****************************************************************
   *                                                              *
   * shr pour placer un DV de facon aléatoire sur une zone.       *
   * entrée: asc qui décrit quoi dispo, et sous matrice           *
   * qui décrit cellules ou au moins une loc                      *
   *                                                              *
   **************************************************************** */

void shr(double **carte, double **ze)
{
  /* Déclaration des variables */
  int i, j, l, m, idci, idli, idls, idcs, *ligne, *colonne, nlsc, ncsc, nlg, ncg;
  double **souscar;
  
  idli = 0;
  idci = 0;
  idls = 0;
  idcs = 0;
  nlg = carte[0][0];
  ncg = carte[1][0];

  vecintalloc(&ligne, nlg);
  vecintalloc(&colonne, ncg);
  
  for (i=1; i<=nlg; i++) {
    for (j=1; j<=ncg; j++) {
      ligne[i] = ligne[i] + carte[i][j];
      colonne[j] = colonne[j] + carte[i][j];
    }
  }
  
  /* Puis on calcule l'indice inférieur et supérieur des lignes
     et des colonnes contenant les locs rastérisées */
  for (i=1; i<=nlg; i++) {
    if ((idli==0)&&(ligne[i]!=0)) idli = i;
  }
  for (i=nlg; i>=1; i--) {
    if ((idls==0)&&(ligne[i]!=0)) idls = i;
  }
  for (i=1; i<=ncg; i++) {
    if ((idci==0)&&(colonne[i]!=0)) idci = i;
  }
  for (i=ncg; i>=1; i--) {
    if ((idcs==0)&&(colonne[i]!=0)) idcs = i;
  }
  
  /* Enfin, calcul du nombre de lignes et de colonnes de souscar */
  nlsc = idls - idli + 1;
  ncsc = idcs - idci + 1;
  
  /* Allocation de mémoire pour souscar */
  taballoc(&souscar, nlsc, ncsc);
  
  /* attribution des valeurs aux cellules de souscar */
  l = 1;
  m = 1;
  for (i=idli; i<=idls; i++) {
    for (j=idci; j<=idcs; j++) {
      souscar[l][m] = carte[i][j];
      m++;
    }
    m = 1;
    l++;
  }
        
  /* Randomisation de la position des locs sur la ZE */
  shifthr(ze, souscar, &idli, &idci);
  /* idli et idci contiennent resp. les indices de lignes 
     et des colonnes pour la carte utilisée randomisée 
     
     Comme carte contient déjà les locs dont l'orientation est randomisée
     On va se servir de ze pour stocker la position randomisée des locs */
  for (i=1; i<=nlg; i++) {
    for (j=1; j<=ncg; j++) {
      ze[i][j] = 0;
    }
  }
      
  /* donc on recalcule la carte complete randomisée */
  l = 1;
  m = 1;
  for (i=idli; i<=(idli+nlsc-1); i++) {
    for (j=idci; j<=(idci+ncsc-1); j++) {
      ze[i][j] = souscar[l][m];
      m++;
    }
    m = 1;
    l++;
  }
  
  /* libé locale de la mémoire */
  freetab(souscar);
  freeintvec(ligne);
  freeintvec(colonne);
}




/* ****************************************************************
   *                                                              *
   *         sr = version interactive avec R de shr               *
   *                                                              *
   **************************************************************** */

void sr(double *carter, double *zer, int *nlgr, int *ncgr)
{
  double **carte, **ze;
  int i,j,k,nlg,ncg;
  nlg = *nlgr;
  ncg = *ncgr;
  taballoc(&carte,nlg,ncg);
  taballoc(&ze,nlg,ncg);
  
  k = 0;
  for (i=1; i<=nlg; i++) {
    for (j=1; j<=ncg; j++) {
      ze[i][j] = zer[k] ;
      carte[i][j] = carter[k];
      k++;
    }
  }

  shr(carte, ze);

  k = 0;
  for (i=1; i<=nlg; i++) {
    for (j=1; j<=ncg; j++) {
      zer[k] = ze[i][j];
      carter[k] = carte[i][j];
      k++;
    }
  }
  
  
  freetab(carte);
  freetab(ze);
  
}






/* ****************************************************************
   *                                                              *
   * locrast permet la rastérisation des locs                     *
   *                                                              *
   *                                                              *
   **************************************************************** */

void locrast(double *xgr, double *ygr, double *x, double *y,
	     double **carte)
{
  /* Déclaration de variables */
  int i, j, k, n, nc, nl;
  double res;
  
  /* allocation de mémoire */
  res = xgr[2]-xgr[1];
  n = x[0];
  nl = carte[0][0];
  nc = carte[1][0];
  
  /* carte */
  for (i=1; i<=nl; i++) {
    for (j=1; j<=nc; j++) {
      carte[i][j] = 0;
    }
  }
  
  
  /* rastérisation des locs */
  for (k=1; k<=n; k++) {
    for (i=1; i<=nl; i++) {
      if (((xgr[i]-(res / 2)) < x[k])&&(x[k]<= (xgr[i]+(res / 2)))) {
	for (j=1; j<=nc; j++) {
	  if (((ygr[j]-(res / 2)) < y[k])&&(y[k]<= (ygr[j]+(res / 2)))) {
	    carte[i][j]++;
	  }
	}
      }
    }
  }
}



/* ****************************************************************
   *                                                              *
   * lr = version interactive avec R de locrast                   *
   *                                                              *
   *                                                              *
   **************************************************************** */


void lr(double *xgri, double *ygri, double *xr, double *yr,
	double *carter, int *nco, int *nli, int *nlixy)
{
  int i,j,k, ncg, nlg, nlxy;
  double *xgr, *x, *y, *ygr, **carte;
  
  ncg = *nco;
  nlg = *nli;
  nlxy = *nlixy;

  vecalloc(&xgr, nlg);
  vecalloc(&ygr, ncg);
  vecalloc(&x, nlxy);
  vecalloc(&y, nlxy);
  taballoc(&carte, nlg, ncg);
  
  for (i=1; i<=nlxy; i++) {
    x[i] = xr[i-1];
    y[i] = yr[i-1];
  }
  
  for (i=1; i<=nlg; i++) {
    xgr[i] = xgri[i-1];
  }
  
  for (i=1; i<=ncg; i++) {
    ygr[i] = ygri[i-1];
  }
  
  
  locrast(xgr, ygr, x, y, carte);
  
  k=0;
  for (i=1; i<=nlg; i++) {
    for (j=1; j<=ncg; j++) {
      carter[k] = carte[i][j];
      k++;
    }
  }

  freetab(carte);
  freevec(xgr);
  freevec(ygr);
  freevec(x);
  freevec(y);
  
}



/* ****************************************************************
   *                                                              *
   * getcarte est l'équivalent C de getkasc                       *
   *                                                              *
   *                                                              *
   **************************************************************** */

void getcarte(double **carte, double **kasc, int *indicecarte)
{
  /* Définition des variables */
  int i,j,k, ic, lgr, cgr;
  
  /* Allocation de mémoire */
  lgr = carte[0][0];
  cgr = carte[1][0];
  ic = *indicecarte;
  
  k = 1;
  for (j=1; j<=cgr; j++) {
    for (i=1; i<=lgr; i++) {
      carte[i][j] = kasc[k][ic];
      k++;
    }
  }
}


/* ****************************************************************
   *                                                              *
   * gc pour test sous R                                          *
   *                                                              *
   *                                                              *
   **************************************************************** */


void gc(double *carter, double *kascr, int *nlgr, int *ncgr, int *nhab)
{
  int i,j,k, nlg, ncg, nh, nl;
  double **carte, **kasc;
  nlg = *nlgr;
  ncg = *ncgr;
  nh = *nhab;
  nl = nlg*ncg;
  
  taballoc(&carte, nlg, ncg);
  taballoc(&kasc, nl, nh);
  
  k=0;
  for (i=1; i<=nl; i++) {
    for (j=1; j<=nh; j++) {
      kasc[i][j] = kascr[k];
      k++;
    }
  }
  
  i=1;
  getcarte(carte, kasc, &i);
  
  k=0;
  for (i=1; i<=nlg; i++) {
    for (j=1; j<=ncg; j++) {
      carter[k] = carte[i][j];
      k++;
    }
  }
  
  
  freetab(carte);
  freetab(kasc);
  
}



/* ****************************************************************
   *                                                              *
   * comptepasNA compte le nombre de lignes d'un tableau pas NA   *
   *                                                              *
   *                                                              *
   **************************************************************** */

void comptePasNA(double **tab, int *nombre)
{
  int i,nb, nc,nl;
  nb = 0;
  nc = tab[1][0];
  nl = tab[0][0];

  for (i=1; i<=nl; i++) {
    if (tab[i][1]!=-9999) {
      nb++;
    }
  }
  
  *nombre = nb;
}


/* ****************************************************************
   *                                                              *
   * videNA supprime les lignes NA d'un tableau                   *
   *                                                              *
   *                                                              *
   **************************************************************** */

void videNA(double **entree, double **sortie, int *idcons)
{
  /* Déclaration des variables */
  int i,j,k, nc, nl;
  
  nl = entree[0][0];
  nc = entree[1][0];

  k=1;
  for (i=1; i<=nl; i++) {
    if (entree[i][1]!=-9999) {
      idcons[k] = i;
      for (j=1; j<=nc; j++) {
	sortie[k][j] = entree[i][j];
      }
      k++;
    }
  }
}






/* ****************************************************************
   *                                                              *
   * niche pour appliquer l'analyse de niche                      *
   *                                                              *
   **************************************************************** */

void niche(double **X, double **Y, double *eig, double **mar)
{
  /* Déclaration de variables */
  int i, j, k, nl, nh, na, rang;
  double **ut, **dis, *poidsli, *poidsani, solo, *ni, **inertie;
  
  /* Allocation de mémoire */
  nh = X[1][0];
  na = Y[1][0];
  nl = Y[0][0];
  
  taballoc(&ut, na, nh);
  taballoc(&dis, na, nh);
  taballoc(&inertie, nh, nh);
  vecalloc(&ni, na);
  vecalloc(&poidsli, nl);
  vecalloc(&poidsani, na);

  /* Centrage et réduction */
  for (i=1; i<=nl; i++) {
    poidsli[i] = (double) 1/nl;
  }
  matmodifcn(X, poidsli);
  
  /* calcul du nombre de locs par animal */
  for (i=1; i<=nl; i++) {
    for (j=1; j<=na; j++) {
      ni[j] = ni[j] + Y[i][j];
    }
  }
  
  /* Calcul de poidsani */
  solo = 0;
  for (j=1; j<=na; j++) {
    solo = ni[j] + solo; /* somme des locs */
  }
  for (j=1; j<=na; j++) {
    poidsani[j] = ni[j] / solo;
  }
  
  
  
  /* Calcul de la moyenne utilisée et dispo */
  for (k=1; k<=na; k++) {
    for (i=1; i<=nl; i++) {
      for (j=1; j<=nh; j++) {
	dis[k][j] = dis[k][j] + ((double) X[i][j]/nl);
	ut[k][j] = ut[k][j] + (X[i][j]*Y[i][k]/ni[k]);
      }
    }
  }

  /* Calcul de la marginalité */
  for (i=1; i<=na; i++) {
    for (j=1; j<=nh; j++) {
      mar[i][j] = ut[i][j] - dis[i][j];
    }
  }
  

  /* Calcul de l'inertie */
  sqrvec(poidsani);
  for (i=1; i<=na; i++) {
    for (j=1; j<=nh; j++) {
      mar[i][j] = mar[i][j] * poidsani[i];
    }
  }
  
  prodmatAtAB(mar, inertie);
  DiagobgComp(nh, inertie, eig, &rang);
  
  /* Pour les sorties: la marginalité */
  for (i=1; i<=na; i++) {
    for (j=1; j<=nh; j++) {
      mar[i][j] = mar[i][j] / poidsani[i];
    }
  }


  /* libé de mémoire */
  freetab(ut);
  freetab(dis);
  freetab(inertie);
  freevec(poidsli);
  freevec(poidsani);
}






/* ****************************************************************
   *                                                              *
   * mvtfreeman: on rentre indice ligne et indice colonne départ  *
   * (in et jn), la direction de freeman (dir), et on récupère    *
   * les indices lignes et colonnes (dans le vecteur np) après    *
   * mouvement.                                                   *
   *                                                              *
   **************************************************************** */


void mvtfreeman(int *in, int *jn, int *dir, int *np)
{
  int i,j;
  i=*in;
  j=*jn;
  
  if ((*dir == 0) | (*dir == 1) | (*dir == 7)) 
    i++;
  if ((*dir == 3) | (*dir == 4) | (*dir == 5)) 
    i--;
  if ((*dir == 1) | (*dir == 2) | (*dir == 3)) 
    j++;
  if ((*dir == 5) | (*dir == 6) | (*dir == 7)) 
    j--;

  np[1]=i;
  np[2]=j;
}


/* ****************************************************************
   *                                                              *
   * algorithme de suivi de contour pour récupe des coordonnées du*
   * polygone de contour                                          *
   *                                                              *
   **************************************************************** */


void getcontour(double *grille, int *nlig, int *ncol, int *indicelig, 
		int *indicecol, int *lcont)
{
  /* Déclaration des variables*/
  int i, j, k, nl, nc, *idlig, *idcol, *P0, *P1, fini, *np, dirprec, dir;
  int lidlig;
  double **x;
  
  nl=*nlig;
  nc=*ncol;
  vecintalloc(&P0,2);
  vecintalloc(&P1,2);
  vecintalloc(&np,2);
  taballoc(&x, nl,nc);
  vecintalloc(&idlig, *lcont);
  vecintalloc(&idcol, *lcont);
  
  
  k=0;
  for (i=1; i<=nl; i++) {
    for(j=1; j<=nc; j++) {
      x[i][j] = grille[k];
      k++;
    }
  }


  /* recherche des indices lignes et colonnes 
     du premiere cellule pas na */
  k=0;
  i=0;
  j=1;
  
  
  while (k==0) {
    if (i != nl) {
      i++;
    }
    else {
      i=1;
      j++;
    }
    k = (int) x[i][j];
  }
  

  idlig[1] = i;
  idcol[1] = j;
  lidlig = 1;
  P0[1] = i;
  P0[2] = j;
  dir = 4;

  fini = 0;
  k = 0;
  
  while (fini==0) {
    while (k==0) {
      dir = (dir + 1)%8;
      mvtfreeman(&i, &j, &dir, np);
      dirprec = (dir + 5)%8;
      k = (int) x[np[1]][np[2]];
    }
    if (lidlig == 1) {
      P1[1] = np[1];
      P1[2] = np[2];
    }
    else {
      if ((i==P0[1])&&(j==P0[2])&&(np[1]==P1[1])&&(np[2]==P1[2])) 
	fini =1;
    }
    
    if (fini==0) {
      lidlig++;
      idlig[lidlig] = np[1];
      idcol[lidlig] = np[2];
      i = np[1];
      j = np[2];
      mvtfreeman(&i, &j, &dirprec, np);
      k = (int) x[np[1]][np[2]];
      dir = dirprec;
    }
  }
  
  

  for (i=1; i<=lidlig; i++) {
    indicelig[i-1]=idlig[i];
    indicecol[i-1]=idcol[i];
  }
  
  
  freeintvec(idlig);
  freeintvec(idcol);
  freeintvec(P0);
  freeintvec(P1);
  freeintvec(np);
  freetab(x);
}



/* ****************************************************************
   *                                                              *
   * algorithme de suivi de contour pour récupe des coordonnées du*
   * polygone de contour: calcul du nombre de points de ce        *
   * polygone.                                                    *
   *                                                              *
   **************************************************************** */

void lcontour(double *grille, int *nlig, int *ncol, int *lcont)
{
  /* Déclaration des variables*/
  int i, j, k, l, m,n, nl, nc, *P0, *P1, fini, *np, dirprec, dir;
  int lidlig;
  double **x, **vois;
  
  nl=*nlig;
  nc=*ncol;
  vecintalloc(&P0,2);
  vecintalloc(&P1,2);
  vecintalloc(&np,2);
  taballoc(&vois, 3,3);
  taballoc(&x, nl,nc);
  
  
  k=0;
  for (i=1; i<=nl; i++) {
    for(j=1; j<=nc; j++) {
      x[i][j] = grille[k];
      k++;
    }
  }


  /* recherche des indices lignes et colonnes 
     du premiere cellule pas na */
  k=0;
  i=0;
  j=1;
  
  
  while (k==0) {
    if (i != nl) {
      i++;
    }
    else {
      i=1;
      j++;
    }
    k = (int) x[i][j];
  }
  
  
  lidlig = 1;
  P0[1] = i;
  P0[2] = j;
  dir = 4;
  
  m=1;
  n=1;
  l=1;

  /*  ca c'est juste au cas ou un seul pixel sur la carte */
  /*  m=1;
  
  for (k=i-1; k<=i+1; k++) {
    n=1;
    for (l=j-1; l<=j+1; l++) {
      vois[m][n] = x[k][l];
      n++;
    }
    m++;
  }
  
  vois[2][2] = 0;
  */
  fini = 0;
  k = 0;
  
  while (fini==0) {
    while (k==0) {
      dir = (dir + 1)%8;
      mvtfreeman(&i, &j, &dir, np);
      dirprec = (dir + 5)%8;
      k = (int) x[np[1]][np[2]];
    }
    if (lidlig == 1) {
      P1[1] = np[1];
      P1[2] = np[2];
    }
    else {
      if ((i==P0[1])&&(j==P0[2])&&(np[1]==P1[1])&&(np[2]==P1[2])) 
	fini = 1;
    }
    
    if (fini==0) {
      lidlig++;
      i = np[1];
      j = np[2];
      mvtfreeman(&i, &j, &dirprec, np);
      k = (int) x[np[1]][np[2]];
      dir = dirprec;
    }
  }
  
  
  *lcont = lidlig;
  freeintvec(P0);
  freeintvec(P1);
  freeintvec(np);
  freetab(vois);
  freetab(x);
}


/* ****************************************************************
   *                                                              *
   * récupération des niveaux d'un facteur                        *
   *                                                              *
   *                                                              *
   **************************************************************** */


void levels(double *vec, double *lev, int *lvec)
{
  int i,j,k,n, l;
  lev[1] = vec[1];
  k=1;
  n=*lvec;
  
  for (i=2; i<=n; i++) {
    l=0;
    for (j=1; j<=k; j++) {
      if (vec[i]==lev[j])
	l=1;
    }
    if (l==0) {
      k++;
      lev[k] = vec[i];
    }
  }
  *lvec = k;
}


/* ****************************************************************
   *                                                              *
   * algorithme d'étiquetage séquentiel des composantes connexes  *
   *                                                              *
   **************************************************************** */


void seqeticorr(double *grille, int *nlig, int *ncol)
  {
    int i, j, k, l, m, n, o, nl, nc, pr, beta, nniv, eticour;
    double **x, *Tc, *prec, *tmp, *tmp1, *tmp2, *etcons, *lf;
    
    nl=*nlig;
    nc=*ncol;
    taballoc(&x, nl, nc);
    vecalloc(&Tc, 1000);
    
    k=0;
    for (i=1; i<=nl; i++) {
      for (j=1; j<=nc; j++) {
	x[i][j]=grille[k];
	k++;
      }
    }
    
    Tc[1]=1;
    eticour=1;
    
    for (j=2; j<=nc; j++) {
      for (i=2; i<=nl; i++) {
        if (((int) x[i][j])!=0) {
	  vecalloc(&prec, 4);
          prec[1] = x[i-1][j-1];
	  prec[2] = x[i][j-1];
	  prec[3] = x[i+1][j-1];
	  prec[4] = x[i-1][j];
	  
	  k=0;
	  for (l=1; l<=4; l++) {
	    if (((int) prec[l])!=0)
	      k++;
	  }
	    
	  /* k contient le nombre de prédecesseurs non nuls */
          if (k!=0) {
	    vecalloc(&tmp, k); /* tmp contient les pred non nuls */
	    m=1;
	    for (l=1; l<=4; l++) {
	      if (((int) prec[l])>0) {
		tmp[m] = prec[l];
		m++;
	      }
	    }

	    freevec(prec);
	    vecalloc(&prec, k);
	    for (l=1; l<=k; l++)
	      prec[l] = tmp[l];
	    freevec(tmp);
	    /* Maintenant, c'est prec qui contient les pred non nuls */
	    
	    

	    /* Nombre de niveaux du facteur prec */
	    vecalloc(&tmp1, 4);
	    m=k;
	    levels(prec, tmp1, &m);
	    /* m contient le nombre de niveaux */
	    vecalloc(&tmp2, m);
	    /* tmp2 contient les niveaux de prec
	       equivalent de etiprec dans R */
	    for (l=1; l<=m; l++)
	      tmp2[l]=tmp1[l];
	    freevec(tmp1);

	    if (m == 1) {
              x[i][j] = tmp2[1];
	    } else {
	      /* calcul du niveau minimum et stockage dans xij */
              x[i][j] = tmp2[1];
	      for (l = 1; l <= m; l++) {
		if (tmp2[l]<x[i][j])
		  x[i][j] = tmp2[l];
	      }

	      /* etcons contiendra les étiquettes différentes de
		 xij */
	      vecalloc(&etcons, m-1);
	      n=1;
	      for (l=1; l<=m; l++) {
		if (x[i][j] != tmp2[l]) {
		  etcons[n] = tmp2[l];
		  n++;
		}
	      }
	      
	      /* boucle de remplissage de la table des correspondances */
	      for (l=1; l<=(m-1); l++) {
		pr = (int) etcons[l];
                beta = pr;
		while (((int) Tc[beta])!=beta) {
                  o = (int) Tc[beta];
		  Tc[beta] = Tc[(int) x[i][j]];
		  beta = o;
		}
                Tc[beta] = Tc[(int) x[i][j]];
	      }
	      freevec(prec);
	      freevec(tmp2);
	      freevec(etcons);
	    }
	  } else {
	    Tc[eticour] = eticour;
	    x[i][j]= eticour;
	    eticour++;
	  }
        }
      }
    }

    eticour--;
    
    /* Actualisation de la table */
    for (i=1; i<=eticour; i++) {
      j = i;
      while (((int) Tc[j])!=j)
        j = (int) Tc[j];
      Tc[i] = j;
    }
    j=eticour;
    vecalloc(&tmp1, j);
    vecalloc(&tmp2, eticour);
    for (i=1; i<=eticour; i++) {
      tmp2[i]=Tc[i];
    }
    
    levels(tmp2, tmp1, &j);
    freevec(tmp2);
    
    vecalloc(&lf, j);
    for (i=1; i<=j; i++)
      lf[i]=tmp1[i];
    freevec(tmp1);
    nniv=j;
    
    /* Deuxième passage */
    for (i=1; i<=nl; i++) {
      for (j=1; j<=nc; j++) {
        if (x[i][j]!=0) {
          x[i][j] = Tc[(int) x[i][j]];
	}
      }
    }

    /* Dernier passage: niveaux variant de 1 à p */
    k = 1;
    for (j=1; j<=nniv; j++) {
      i = (int) lf[j];
      if (i != k) {
	for (l = 1; l <= nl; l++) {
	  for (m = 1; m <= nc; m++) {
	    if (((int) x[l][m]) == i)
	      x[l][m]=k;
	  }
	}
      }
      k++;
    }

    /* grille */
    k=0;
    for (i=1; i<=nl; i++) {
      for (j=1; j<=nc; j++) {
	grille[k]=x[i][j];
	k++;
      }
    }

    freetab(x);
    freevec(Tc);
    
  }



/* ****************************************************************
   *                                                              *
   *   epa: bivariate normal kernel                               *
   *                                                              *
   **************************************************************** */

void epa(double *X, double *Y, double *xl, double *yl, double *val, double *fen)
{
  int k,nl;
  double *xy, kx, di2, h;
  
  nl = (int) xl[0];
  vecalloc(&xy, 2);
  *val = 0;
  h = *fen;
  kx = 0;
  
  for (k=1; k<=nl; k++) {
    xy[1] = (xl[k] - *X);
    xy[2] = (yl[k] - *Y);
    di2 = xy[1]*xy[1] + xy[2]*xy[2];
    kx = exp(-di2/(2*h*h));
    *val = *val + kx;
  }
  *val = *val * (1/(((double) nl)*h*h*2*3.14159265359));
  freevec(xy);
}



/* ****************************************************************
   *                                                              *
   *   estimation du DV par kernel                                *
   *                                                              *
   **************************************************************** */


void kernelhr(double *grille, double *xgri, double *ygri, int *ncolgri,
	      int *nliggri, int *nloc, double *fen, double *xlo, double *ylo)
{
  int i, j, k, ncg, nlg, nlo;
  double **gri, *xg, *yg, *xl, *yl, X, Y, tmp;
  
  /* Allocation de mémoire */
  ncg = *ncolgri;
  nlg = *nliggri;
  nlo = *nloc;
  tmp = 0;
  
  taballoc(&gri,nlg, ncg);
  vecalloc(&xg, nlg);
  vecalloc(&yg, ncg);
  vecalloc(&xl, nlo);
  vecalloc(&yl, nlo);
  
  /* passage de valeur aux variables C */
  
  for (i=1; i<=nlo; i++) {
    xl[i] = xlo[i-1];
    yl[i] = ylo[i-1];
  }
  
  for (i=1; i<=nlg; i++) {
    xg[i] = xgri[i-1];
  }
  
  for (i=1; i<=ncg; i++) {
    yg[i] = ygri[i-1];
  }
  
  /* boucle de calcul sur la grille */
  for (i=1; i<=nlg; i++) {
    for (j=1; j<=ncg; j++) {
      X = xg[i];
      Y = yg[j];
      epa(&X, &Y, xl, yl, &tmp, fen);
      gri[i][j] = tmp;
    }
  }
  
  /* retour vers R */
  k = 0;
  for (i=1; i<=nlg; i++) {
    for (j=1; j<=ncg; j++) {
      grille[k] = gri[i][j];
      k++;
    }
  }

  /* libération de la mémoire */
  freetab(gri);
  freevec(xg);
  freevec(yg);
  freevec(xl);
  freevec(yl);
}






/* ****************************************************************
   *                                                              *
   *   Minimisation de la LSCV                                    *
   *                                                              *
   **************************************************************** */


void CVmise(int *nloc, double *xlo, double *ylo,
	    double *hvec, double *CV, int *nhteste)
{
  int i, j, k, nlo, nh;
  double *xl, *yl, h, di2;
  
  /* Allocation de mémoire */
  nlo = *nloc;
  nh = *nhteste;
    
  vecalloc(&xl, nlo);
  vecalloc(&yl, nlo);
  
  /* passage de valeur aux variables C */
  
  for (i=1; i<=nlo; i++) {
    xl[i] = xlo[i-1];
    yl[i] = ylo[i-1];
  }
  
  /* boucle de calcul de la fenetre */
  for (k=1; k<=nh; k++) {
    h = hvec[k-1];
    CV[k-1] = 0;
    
    for (i=1; i<=nlo; i++) {
      for (j=1; j<=nlo; j++) {
	di2 = (xl[i]-xl[j])*(xl[i]-xl[j]) + (yl[i]-yl[j])*(yl[i]-yl[j]);
	CV[k-1] = CV[k-1] + (exp(-(di2/(4*h*h)))-4*exp(-di2/(2*h*h)));
      }
    }
    CV[k-1] = CV[k-1]*(1/(4*3.14159265359*h*h*((double) nlo)*((double) nlo)));
    CV[k-1] = CV[k-1] + (1/(3.14159265359*h*h*nlo));
    
  }
  freevec(xl);
  freevec(yl);
}

  



/* ****************************************************************
   *                                                              *
   *            Calcul du volume sous l'UD                        *
   *                                                              *
   **************************************************************** */


void calcvolume(double *grille, int *ncolgri, int *nliggri, double *cellsize)
{
  int i, j, k, nl, nc;
  double cs, **gri;
    
  /* Allocation de mémoire */
  nl = *nliggri;
  nc = *ncolgri;
  cs = *cellsize;
  
  taballoc(&gri, nl, nc);
  
  /* copie de la grille */
  k = 0;
  for (i = 1; i <= nl; i++) {
    for (j = 1; j <= nc; j++) {
      gri[i][j] = grille[k];
      k++;
    }
  }
  
  /* Calcul du volume de la grille */
  for (i = 1; i <= nl; i++) {
    for (j = 1; j <= nc; j++) {
      gri[i][j] = gri[i][j]*cs*cs;
    }
  }

  
  /* Retour vers R */
  k = 0;
  for (i = 1; i <= nl; i++) {
    for (j = 1; j <= nc; j++) {
      grille[k] = gri[i][j];
      k++;
    }
  }
  
  freetab(gri);
    
}





/* ****************************************************************
   *                                                              *
   *   DOMAIN: estimation de l'aire de répartition potentielle    *
   *                                                              *
   **************************************************************** */

void calcsim(double *pix, double **pts, double *rg, 
	     int *nvar, int *npts, double *similarite)
{
  /* Déclarations de variables */
  int no,nv, i, j;
  double *vecqual, *temp, nib;

  no = *npts;
  nv = *nvar;
  
  vecalloc(&vecqual, no);
  vecalloc(&temp, nv);
  
  /* Calcul de la similarité: boucle */
  for (i=1; i<=no; i++) {
    nib = 0;
    for (j=1; j<=nv; j++) {
      temp[j] = abs(pix[j]-pts[i][j])/rg[j];
      nib = nib + temp[j];
    }
    vecqual[i] = 1 - (1/((double) nv))*nib;
  }
  
  /* calcul de la qualité de l'habitat
     par le max de la similarité */
  *similarite = vecqual[1];
  
  for (i=2; i<=no; i++) {
    if (vecqual[i]>*similarite)
      *similarite = vecqual[i];
  }
  
  
  /* libération de la mémoire */
  freevec(vecqual);
  freevec(temp);
  
}


void fctdomain(double *kascmod, double *ptsmod, double *range, int *npts, int *npix,  
	       int *nvar, double *qualhab)
{
  /* Déclarations de variables */
  int no,np,nv, i, j, k;
  double **kasc, **pts, *rg, *pix, sim;
  
  /* Copie dans les variables locales */
  no = *npts;
  np = *npix;
  nv = *nvar;

  taballoc(&kasc, np, nv);
  taballoc(&pts, no, nv);
  vecalloc(&rg, nv);
  vecalloc(&pix, nv);
  
    
  k = 0;
  for (i = 1; i <= np; i++) {
    for (j = 1; j <= nv; j++) {
      kasc[i][j] = kascmod[k];
      k++;
    }
  }
  
  k = 0;
  for (i = 1; i <= no; i++) {
    for (j = 1; j <= nv; j++) {
      pts[i][j] = ptsmod[k];
      k++;
    }
  }

  for (i=1; i<=nv; i++) {
    rg[i] = range[i-1];
  }
  
  
  /* Le coeur de la fonction */
  for (i=1; i<=np; i++) {
    for (j=1; j<=nv; j++) {
      pix[j] = kasc[i][j];
    }
    calcsim(pix, pts, rg, &nv, &no, &sim);
    qualhab[i-1] = sim;
  }
  
  /* Libération de la mémoire */
  freetab(kasc);
  freetab(pts);
  freevec(pix);
  freevec(rg);

}





/**************************************************************************************
 **************                 L'analyse compositionelle                 *************
 *************************************************************************************/


/* Le weighted mean lambda: analyse compo sous R */

void wml(double **used, double **avail, double *wmla, int na, int nh,
	 double **proj1, double **proj2, double *nbassocie, int krep)
{
  /* Déclaration de variables */
  double **dlr, *moydlr, *nadlr, **dlrtmp, **mod1, **mod2, **res1, **res2;
  double **SCEres1, **SCEres2, *vp1, *vp2, det1, det2, *vecalea, *aleamu;
  int i, j, k, idcol, *vecindice, rg1, rg2;
  int jb;
  
  /* allocation de mémoire */
  taballoc(&dlr, na, (nh*(nh-1)));
  taballoc(&mod1, na, (nh-1));
  taballoc(&mod2, na, (nh-1));
  taballoc(&SCEres1, (nh-1), (nh-1));
  taballoc(&SCEres2, (nh-1), (nh-1));
  taballoc(&dlrtmp, na, (nh-1));
  taballoc(&res1, na, (nh-1));
  taballoc(&res2, na, (nh-1));
  vecintalloc(&vecindice, nh-1);
  vecalloc(&nadlr, nh -1);
  vecalloc(&moydlr, nh-1);
  vecalloc(&vp1, nh-1);
  vecalloc(&vp2, nh-1);
  vecalloc(&aleamu, 2);
  vecalloc(&vecalea, na);
  
  aleamu[1] = 1;
  aleamu[2] = -1;
  
  jb = 0;
  
  /* tirage au sort de la permutation pour chaque animal */
  for (i = 1; i <= na; i++) {
    aleapermutvec(aleamu);
    vecalea[i] = aleamu[1];
  }
  
  /* cas où krep ==1 : première répétition de la
     randomisation: on calcule le "vrai" lambda (pas
     randomisé) */
  if (krep == 1) {
    for (i = 1; i<=na; i++) {
      vecalea[i] = 1;
    }
  }
  
  
  /* vidage de nbassocie */
  for (i = 1; i <= nh; i++)
    nbassocie[i] = 0;

  
  /* boucle de remplissage des DLR */
  for (k = 1; k <= nh; k++) {
    i = 1;

    /* construction du vecteur d'indices */
    for (j = 1; j <= nh; j++) {
      if (j != k) {
	vecindice[i] = j;
	i++;
      }
    }

    /* remise à 0 de la moyenne  et du nombre devaleurs non manquantes */
    for (j = 1; j <= (nh-1); j++) {
      moydlr[j] = 0;
      nadlr[j] = 0;
    }

    /* premier remplissage des DLR */
    for (j = 1; j <= (nh-1); j++) {
      jb = vecindice[j];
      idcol = (nh - 1) * (k - 1) + j;
      for (i = 1; i <= na; i++) {
	if ((avail[i][jb]!=0)&&(avail[i][k]!=0)) {
	  dlr[i][idcol] = (log(used[i][jb] / used[i][k]) - 
	    log(avail[i][jb] / avail[i][k])) * vecalea[i];
	  
	  /* calcul de la moyenne */
	  moydlr[j] = moydlr[j] + dlr[i][idcol];
	  nadlr[j]++;
	}
      }
    }
    
    for (j = 1; j <= (nh-1); j++) {
      moydlr[j] = moydlr[j] / nadlr[j];
    }
    
    /* deuxième boucle: remplacement des valeurs manquantes */
    for (j = 1; j <= (nh-1); j++) {
      idcol = (nh - 1) * (k - 1) + j;
      jb = vecindice[j];
      for (i = 1; i <= na; i++) {
	if ((avail[i][jb]==0)||(avail[i][k]==0))
	  dlr[i][idcol] = moydlr[j];
      }
    }
    
    /* extraction de DLRtmp */
    for (i = 1; i <= na; i++) {
      for (j = 1; j <= (nh-1); j++) {
	idcol = (nh - 1) * (k - 1) + j;
	dlrtmp[i][j] = dlr[i][idcol];
      }
    }
    
    /* Calcul des modèles */
    prodmatABC(proj1, dlrtmp, mod1);
    prodmatABC(proj2, dlrtmp, mod2);
    
    /* Calcul des résidus */
    for (i = 1; i <= na; i++) {
      for (j = 1; j <= nh-1; j++) {
	res1[i][j] = dlrtmp[i][j] - mod1[i][j];
	res2[i][j] = dlrtmp[i][j] - mod2[i][j];
      }
    }
    
    /* calcul des sommes des carrés */
    prodmatAtAB(res1, SCEres1);
    prodmatAtAB(res2, SCEres2);
    
    /* calcul du déterminant */
    DiagobgComp(nh-1, SCEres1, vp1, &rg1);
    DiagobgComp(nh-1, SCEres2, vp2, &rg2);
    det1 = 1;
    det2 = 1;

    for (i = 1; i <= rg1; i++) {
      det1 = det1 * vp1[i];
    }

    for (i = 1; i <= rg2; i++) {
      det2 = det2 * vp2[i];
    }

    wmla[k] = det1 / det2;
    for (i = 1; i <= (nh-1); i++)
      nbassocie[k] = nbassocie[k] + nadlr[i];
  }

  /* libération de la mémoire */
  freetab(dlr);
  freetab(mod1);
  freetab(mod2);
  freetab(SCEres1);
  freetab(SCEres2);
  freetab(dlrtmp);
  freetab(res1);
  freetab(res2);
  freeintvec(vecindice);
  freevec(nadlr);
  freevec(moydlr);
  freevec(vp1);
  freevec(vp2);
  freevec(aleamu);
  freevec(vecalea);
  
}




/* aclambda permet le calcul de lambda dans l'analyse compositionnelle */

void aclambda(double *util, double *dispo, int *nani, int *nhab,  
	      double *xxtxxtmod1, double *xxtxxtmod2, double *rnv,
	      double *wmla, int *nrep, double *wm, double *nb)
{
  /* Déclarations de variables */
  int na, nh, i, j, k, nr;
  double **ut, **di, **proj1, **proj2, *lilamb, *linb, sumnb;

  /* allocation de mémoire */
  na = *nani;
  nr = *nrep;
  nh = *nhab;

  taballoc(&ut, na, nh);
  taballoc(&di, na, nh);
  taballoc(&proj1, na, na);
  taballoc(&proj2, na, na);
  vecalloc(&lilamb, nh);
  vecalloc(&linb, nh);
  
  
  /* Copie dans les variables locales */
  /* utilise */
  k = 0;
  for (i = 1; i <= na; i++) {
    for (j = 1; j <= nh; j++) {
      ut[i][j] = util[k];
      if (ut[i][j] == 0)
	ut[i][j] = *rnv;
      k++;
    }
  }

  /* disponible */
  k = 0;
  for (i = 1; i <= na; i++) {
    for (j = 1; j <= nh; j++) {
      di[i][j] = dispo[k];
      k++;
    }
  }
  
  /* projecteur 1 */
  k = 0;
  for (i = 1; i <= na; i++) {
    for (j = 1; j <= na; j++) {
      proj1[i][j] = xxtxxtmod1[k];
      k++;
    }
  }

  /* projecteur 2 */
  k = 0;
  for (i = 1; i <= na; i++) {
    for (j = 1; j <= na; j++) {
      proj2[i][j] = xxtxxtmod2[k];
      k++;
    }
  }
  
  /* Début de la boucle */
  for (k = 1; k <= nr; k++) {
    /* calcul du weighted mean lambda */
    wml(ut, di, lilamb, na, nh,
	proj1, proj2, linb, k);
    sumnb = 0;
    for (i = 1; i <= nh; i++)
      sumnb = sumnb + linb[i];
    for (i = 1; i <= nh; i++)
      wmla[k-1] = wmla[k-1] + ((lilamb[i] * linb[i]) / sumnb);
    
    /* retour des composantes de lambda et du 
       nombre de valeurs non manquantes */
    if (k == 1) {
      for (i = 1; i <= nh; i++) {
	wm[i-1] = lilamb[i];
	nb[i-1] = linb[i];
      }
    }
  }
  
      
  /* Libération de la mémoire */
  freetab(ut);
  freetab(di);
  freetab(proj1);
  freetab(proj2);
  freevec(lilamb);
  freevec(linb);

}


/* Calcul de la ranking matrix pour l'analyse compositionelle */

void rankma(double *used, double *avail, double *rankmap, double *rankmam,
	    double *rankmav, double *rankmanb, int *nhab, int *nani, int *nrep, double *rnv)
{
  /* Déclarations de variables */
  int i, j, k, nh, na, nr, r;
  double **u, **a, **rmp, **rmm, **rmv, **rmnb, *dlrtmp, *vecalea, pp, val, moy;
  double *aleamu, **tabani;
  

  /* Allocation de mémoire */
  nh = *nhab;
  na = *nani;
  nr = *nrep;
  r = 0;
  
  taballoc(&u, na, nh);
  taballoc(&a, na, nh);
  taballoc(&rmv, nh, nh);
  taballoc(&rmp, nh, nh);
  taballoc(&rmm, nh, nh);
  taballoc(&rmnb, nh, nh);
  vecalloc(&dlrtmp, na);
  vecalloc(&vecalea, nr);
  vecalloc(&aleamu, 2);
  taballoc(&tabani, nr, na);
  aleamu[1] = -1;
  aleamu[2] = 1;
  
  /* Remplissage des tableaux */
  k = 0;
  for (i = 1; i <= na; i++) {
    for (j = 1; j <= nh; j++) {
      u[i][j] = used[k];
      a[i][j] = avail[k];
      if (u[i][j] == 0)
	u[i][j] = *rnv;
      k++;
    }
  }

  /* Remplissage du tablea tabani */
  for (i = 1; i <= nr; i++) {
    for (j = 1; j <= na; j++) {
      aleapermutvec(aleamu);
      tabani[i][j] = aleamu[1];
    }
  }
  for (i = 1; i<=na; i++) {
    tabani[1][i] = 1;
  }

  /* Début de la boucle */
  for (k = 1; k <= nh; k++) {
    for (j = 1; j <= nh; j++) {
      for (r = 1; r <= nr; r++) {
	moy = 0;
	/* premier remplissage des dlr par ani */
	for (i = 1; i <= na; i++) {
	  if ((a[i][j]!=0)&&(a[i][k]!=0)) {
	    dlrtmp[i] = (log(u[i][j]/u[i][k]) - log(a[i][j]/a[i][k])) * tabani[r][i];
	    moy = moy + dlrtmp[i];
	    if (r == 1)
	      rmnb[j][k]++;
	  }
	}
	
	/* Calcul de la moyenne */
	moy = moy / rmnb[j][k];
	if (r==1)
	  rmv[j][k] = moy;
	vecalea[r] = moy;
      }
      
      /* Calcul de P */
      val = rmv[j][k];
      pp = 0;
      for (r = 1; r <= nr; r++) {
	if (val < vecalea[r])
	  rmm[j][k]++;
	if (val > vecalea[r])
	  rmp[j][k]++;
      }
    }
  }
  
  /* retour vers R */
  k = 0;
  for (i=1; i<=nh; i++) {
    for (j=1; j<=nh; j++) {
      rankmap[k] = rmp[i][j];
      rankmam[k] = rmm[i][j];
      rankmav[k] = rmv[i][j];
      rankmanb[k] = rmnb[i][j];
      k++;
    }
  }

  
  /* libération de la mémoire */  
  freetab(rmv);
  freetab(rmp);
  freetab(rmm);
  freetab(rmnb);
  freevec(dlrtmp);
  freetab(u);
  freetab(a);
  freevec(vecalea);
  freevec(aleamu);
  freetab(tabani);
}





/* ****************************************************************
   *                                                              *
   *   dilatation et érosion morphologique                        *
   *                                                              *
   **************************************************************** */

void erodil(double *grille, int *nlig, int *ncol, int *ntour, int *oper)
{
  int i,j,k,l,nl,nc, nt, etat0, etat1;
  double **x, **xm, *voisin;
  
  nl = *nlig;
  nc = *ncol;
  nt = *ntour;
  etat0 = 0;
  etat1 = 0;
  
  taballoc(&x,nl,nc);
  taballoc(&xm,nl,nc);
  vecalloc(&voisin, 9);
    
  k=0;
  for (i=1; i<=nl; i++) {
    for (j=1; j<=nc; j++) {
      x[i][j]=grille[k];
      k++;
    }
  }
  
  for (k=1; k<=nt; k++) {
    for (i=2; i<= (nl-1); i++) {
      for (j=2; j<= (nc-1); j++) {
	voisin[1] = x[i-1][j-1];
	voisin[2] = x[i-1][j];
	voisin[3] = x[i-1][j+1];
	voisin[4] = x[i][j-1];
	voisin[5] = x[i][j+1];
	voisin[6] = x[i+1][j-1];
	voisin[7] = x[i+1][j];
	voisin[8] = x[i+1][j+1];
	voisin[9] = x[i][j];
	for (l=1;l<=9; l++) {
	  if (voisin[l]==0) {
	    etat0 = etat0 + 1;
	  } else {
	    etat1 = etat1 + 1;
	  }
	}
	if (*oper==1) {
	  if (etat1 > 0)
	    xm[i][j] = 1;
	  if (etat1 == 0)
	    xm[i][j] = 0;
	} else {
	  if (etat0 == 0)
	    xm[i][j] =1;
	  if (etat0 > 0)
	    xm[i][j] =0;
	}
	etat1 = 0;
	etat0 = 0;
      }
    }
    
    
    for (i=1; i<=nl; i++) {
      for (j=1; j<=nc; j++) {
	x[i][j]=xm[i][j];
      }
    }
  }
  
  /* grille */
  k=0;
  for (i=1; i<=nl; i++) {
    for (j=1; j<=nc; j++) {
      grille[k]=xm[i][j];
      k++;
    }
  }
  
  freetab(x);
  freetab(xm);
  freevec(voisin);
  
}





/********************************************************
 x et y sont les coordonnées des points, xp et yp 
 les coordonnées des sommets du polygone (le premier 
 et le dernier sommet doivent être identiques). deds est
 un vecteur de même longueur que x et y: prend la valeur 1
 si le point est dans le polygone et 0 sinon.
********************************************************/

void inout(double *x, double *y, double *xp, double *yp,
	   int *deds)
{
  /* Déclaration des variables */
  int i, j, n, wm, np;
  double *xpc, *ypc, sig, a, b, x0;
  
  /* allocation d'espace et valeurs des variables */
  n = x[0];
  np = xp[0];

  vecalloc(&xpc, np);
  vecalloc(&ypc, np);
  
  for (i = 1; i <= n; i++) {
    deds[i] = 1;
  }
      
  for (j = 1; j <= n; j++) {

    /* Centrage autour du point */
    for (i = 1; i <= np; i++) {
      xpc[i] = xp[i] - x[j];
      ypc[i] = yp[i] - y[j];
    }
    
    /* mesure du nombre d'intersections avec l'axe des X , pour X >0 */
    wm = 0;
    for (i = 1; i <= (np-1); i++) {
      sig = ypc[i] * ypc[i+1];
      if (sig < 0) {
	/* calcul de la pente et ord ori */
	/* Cas 1: on n'a pas de pente infinie */
	if ((xpc[i+1] - xpc[i])!=0)
	  {
	    a = (ypc[i+1] - ypc[i]) / (xpc[i+1] - xpc[i]);
	    b = (ypc[i]- a * xpc[i]);
	    /* calcul de x à y = 0 */
	    /* ayant un sens seulement si a != 0 */
	    if (((ypc[i+1] - ypc[i])!=0)) {
	      x0 = - b / a;
	      if (x0 >= 0)
		wm = abs(wm - 1);
	    } 	    
	  }
	/* Cas 2: On a une pente infinie
	   il faut alors vérifier que à droite du point, soit
	   xi >0 */
	if (((xpc[i+1] - xpc[i])==0))
	  {
	    if (xpc[i] >= 0)
	      wm = abs(wm - 1);
	  }
      }
    }
    
    /* Si nombre pair: dehors, sinon, dedans */
    if (wm == 0)
      deds[j] = 0;
  }
  
  
  /* libération de la mémoire */
  freevec(xpc);
  freevec(ypc);
}

/* vérif de inout sous R */


void inoutr(double *xr, double *yr, double *xpr, double *ypr,
	    int *dedsr, int *nxr, int *npr)
{
  int i, nx, np, *deds;
  double *x, *y, *xp, *yp;
  
  /* allocation d'espace */
  nx = *nxr;
  np = *npr;
  vecalloc(&x, nx);
  vecalloc(&y, nx);
  vecalloc(&xp, np);
  vecalloc(&yp, np);
  vecintalloc(&deds, nx);

  for (i = 1; i <= nx; i++) {
    x[i] = xr[i-1];
    y[i] = yr[i-1];
  }

  for (i = 1; i <= np; i++) {
    xp[i] = xpr[i-1];
    yp[i] = ypr[i-1];
  }
  
  inout(x, y, xp, yp, deds);
  
  for (i=1; i<=nx; i++) {
    dedsr[i-1] = deds[i];
  }
  
  /* libération de la mémoire */
  freevec(x);
  freevec(y);
  freevec(xp);
  freevec(yp);
  freeintvec(deds);
}




/***********************************************************
  Rasterisation d'un polygone: xp et yp sont les coordonnées
  du polygone, xg et yg (pas la même longueur) sont les
  coordonnées des lignes et des colones de la grille, et
  carte est une matrice raster.
*************************************************************/


void rastpol(double *xp, double *yp, double *xg, double *yg,
	     double **carte)
{
  /* Déclaration des variables */
  int i, j, nl, nc, k, *deds;
  double *nxc, *nyc;
  
  /* allocation de mémoire */
  nl = xg[0];
  nc = yg[0];
  vecalloc(&nxc, nl*nc);
  vecalloc(&nyc, nl*nc);
  vecintalloc(&deds, nl*nc);
  
  /* Vidage de la carte */
  for (i = 1; i <= nl; i++) {
    for (j = 1; j <= nc; j++) {
      carte[i][j] = 0;
    }
  }
  
  /* Sortie des coordonnées des pixels de la grille */
  k = 1;
  for (i = 1; i <= nl; i++) {
    for (j = 1; j <= nc; j++) {
      nxc[k] = xg[i];
      nyc[k] = yg[j];
      k++;
    }
  }
  
  /* inout de ces pixels */
  inout(nxc, nyc, xp, yp, deds);
  
  /* et remplissage de la carte */
  k = 1;
  for (i = 1; i <= nl; i++) {
    for (j = 1; j <= nc; j++) {
      carte[i][j] = (double) deds[k];
      k++;
    }
  }
  
  /* libération de la mémoire */
  freevec(nxc);
  freevec(nyc);
  freeintvec(deds);
}



/* ****************************************************************
   *                                                              *
   *   Vérif de rastpol sous R                                    *
   *                                                              *
   **************************************************************** */

void rastpolaire(double *xpr, double *ypr, double *xgr, double *ygr,
		 double *carter, int *nlg, int *ncg, int *nvp)
{
  /* allocation de mémoire */
  int i, j, k, nl, nc, nv;
  double *xp, *yp, *xg, *yg, **carte;
  
  /* remplissage des tableaux */
  nl = *nlg;
  nc = *ncg;
  nv = *nvp;
  vecalloc(&xp, nv);
  vecalloc(&yp, nv);
  vecalloc(&xg, nl);
  vecalloc(&yg, nc);
  taballoc(&carte, nl, nc);
    
  for (i = 1; i <= nv; i++) {
    xp[i] = xpr[i-1];
    yp[i] = ypr[i-1];
  }

  for (i = 1; i <= nl; i++) {
    xg[i] = xgr[i-1];
  }

  for (i = 1; i <= nc; i++) {
    yg[i] = ygr[i-1];
  }
  
  k=0;
  for (i=1; i<=nl; i++) {
    for(j=1; j<=nc; j++) {
      carte[i][j] = carter[k];
      k++;
    }
  }
  
  rastpol(xp, yp, xg, yg, carte);

  k=0;
  for (i=1; i<=nl; i++) {
    for (j=1; j<=nc; j++) {
      carter[k] = carte[i][j];
      k++;
    }
  }
  
  /* libération de la mémoire */
  freevec(xp);
  freevec(yp);
  freevec(xg);
  freevec(yg);
  freetab(carte);
}




/* ****************************************************************
   *                                                              *
   *   Calcul de marginalité et tolérance (par variable)          *
   *                                                              *
   **************************************************************** */


void calcniche(double **kasc, int *nvar, int *nlg, int *ncg,
	       double *margvar, double *tolvar, double **carte)
{
  /* définition des variables */
  int i, j, l, np, nv, nc, nl, npixpol;
  double **cartevar;
  
  /* allocation de mémoire */
  nc = *ncg;
  nl = *nlg;
  nv = *nvar;
  np = nc*nl;
  npixpol = 0;
  
  taballoc(&cartevar, nl, nc);
  
  /* Marginalité et tolérance posées à 0 */
  for (l = 1; l <= nv; l++) {
    margvar[l] = 0;
    tolvar[l] = 0;
  }
  
  /* boucle pour chaque variable */
  for (l = 1; l <= nv; l++) {
    
    /* récupération de la carte */
    getcarte(cartevar, kasc, &l);
    npixpol = 0;
    
    /* calcul de la moyenne utilisée */
    for (i = 1; i <= nl; i++) {
      for (j = 1; j <= nc; j++) {
	if (carte[i][j] == 1) {
	  if (cartevar[i][j] != -9999) {
	    margvar[l] = margvar[l] + cartevar[i][j];
	    npixpol++;
	  }
	}
      }
    }
    margvar[l] = margvar[l] / ((double) npixpol);
    
    /* Calcul de la tolérance */
    for (i = 1; i <= nl; i++) {
      for (j = 1; j <= nc; j++) {
	if (carte[i][j] == 1) {
	  if (cartevar[i][j]!=-9999) {
	    tolvar[l] = tolvar[l] + (cartevar[i][j] - margvar[l])*(cartevar[i][j] - margvar[l]);
	  }
	}
      }
    }
    tolvar[l] = tolvar[l] / ((double) npixpol);
  }
  
  /* libération de la mémoire */
  freetab(cartevar);
}


/* ****************************************************************
   *                                                              *
   *   Vérification sous R                                        *
   *                                                              *
   **************************************************************** */

void calcnicher(double *kascr, int *nvar, int *nlg, int *ncg,
		double *margvar, double *tolvar, double *carter)
{
  /* Déclaration de variables */
  int i, j, k, nv, nl, nc;
  double *marg, *tol, **carte, **kasc;
  
  /* Allocation de mémoire */
  nv = *nvar;
  nl = *nlg;
  nc = *ncg;
  vecalloc(&marg, nv);
  vecalloc(&tol, nv);
  taballoc(&carte, nl,nc);
  taballoc(&kasc, nl*nc,nv);
  
  /* Remplissage des variables */
  k = 0;
  for (i = 1; i <= (nl*nc); i++) {
    for (j = 1; j<=nv; j++) {
      kasc[i][j] = kascr[k];
      k++;
    }
  }
  
  k = 0;
  for (i = 1; i <= nl; i++) {
    for (j = 1; j<=nc; j++) {
      carte[i][j] = carter[k];
      k++;
    }
  }
  
  
  /* Fonction C */
  calcniche(kasc, &nv, &nl, &nc, marg, tol, carte);
  
  /* Sorties vers R */
  for (i=1; i<=nv; i++) {
    margvar[i-1] = marg[i];
    tolvar[i-1] = tol[i];
  }
  
  /* libération de la mémoire */
  freevec(marg);
  freevec(tol);
  freetab(carte);
  freetab(kasc);
}






/* ****************************************************************
   *                                                              *
   *   Fonction permettant de randomiser l'orientation et         *
   *   la position d'un polygone (coordonnées xpr et ypr)         *
   *   sur la zone d'étude représentée par kascr                  *
   *                                                              *
   **************************************************************** */


void randompol(double *xpr, double *ypr, double *kascr,
	       double *marg, double *tol, int *nvar,
	       double *xgr, double *ygr, int *nlr, 
	       int *ncr, int *nvpr, int *nrep)
{
  /* définition des variables */
  int i, j, k, l, r, np, nv, nc, nl, nvp, nr;
  double *xp, *yp, *xr, *yr, **kasc, **carte, *xg, *yg, *moyut;
  double *tolvar, **margb, **tolb, **ze;
  
  /* allocation de mémoire */
  nc = *ncr;
  nl = *nlr;
  nv = *nvar;
  np = nc*nl;
  nvp = *nvpr;
  nr = *nrep;
  r=1;
  
  vecalloc(&xp, nvp);
  vecalloc(&yp, nvp);
  vecalloc(&xr, nvp);
  vecalloc(&yr, nvp);
  vecalloc(&xg, nl);
  vecalloc(&yg, nc);
  vecalloc(&moyut, nv);
  vecalloc(&tolvar, nv);
  taballoc(&kasc, np, nv);
  taballoc(&carte, nl, nc);
  taballoc(&ze, nl, nc);
  taballoc(&margb, (nr+1), nv);
  taballoc(&tolb, (nr+1), nv);
  
  /* remplissage des variables et tableaux C */
  for (i=1; i<=nvp; i++) {
    xp[i] = xpr[i-1];
    yp[i] = ypr[i-1];
  }
  for (i=1; i<=nl; i++) {
    xg[i] = xgr[i-1];
  }
  for (i=1; i<=nc; i++) {
    yg[i] = ygr[i-1];
  }
  k=0;
  for (i=1; i<=np; i++) {
    for(j=1; j<=nv; j++) {
      kasc[i][j] = kascr[k];
      k++;
    }
  }
  
  /* Rastérisation du polygone */
  rastpol(xp, yp, xg, yg, carte);
  
  /* Calcul de la moyenne utilisée et tolérance */
  calcniche(kasc, &nv, &nl, &nc, moyut, 
	    tolvar, carte);
  
  for (l = 1; l <= nv; l++) {
    margb[1][l] = moyut[l];
    tolb[1][l] = tolvar[l];
  }

  for (i = 1; i <= nvp; i++) {
    xr[i] = xp[i];
    yr[i] = yp[i];
  }
  
  for (r =1; r <= nr; r++) {
    
    rotxy(xr, yr, r);
    rastpol(xr, yr, xg, yg, carte);
    l=1;
    getcarte(ze, kasc, &l);
    
    shr(carte, ze);
    
    calcniche(kasc, &nv, &nl, &nc, moyut, 
	      tolvar, ze);

    for (l = 1; l <= nv; l++) {
      margb[r+1][l] = moyut[l];
      tolb[r+1][l] = tolvar[l];
    }
    
  }

  /* sortie des résultats sous R */
  k=0;
  for (i=1; i<=(nr+1); i++) {
    for (j=1; j<=nv; j++) {
      marg[k]=margb[i][j];
      tol[k]=tolb[i][j];
      k++;
    }
  }
  
  /* libération de la mémoire */
  freevec(xp);
  freevec(yp);
  freevec(xg);
  freevec(yg);
  freetab(kasc);
  freetab(carte);
  freevec(xr);
  freevec(yr);
  freevec(moyut);
  freevec(tolvar);
  freetab(ze);
  freetab(margb);
  freetab(tolb);
}



/* ****************************************************************
   *                                                              *
   *   On donne un vecteur point, un objet asc et les coordonnées *
   *   des lignes et des colonnes de la carte, et la taille de la *
   *   de la cellule, et on obtient dans na la valeur sous le     *
   *   point                                                      *
   *                                                              *
   **************************************************************** */


void dedans(double *pts, double *xc, double *yc, double *na,
	    double cs, double **asc)
{
  int nl, nc, i, ligne, colo;
  double x, y;
  
  x = pts[1];
  y = pts[2];
  
  nl = xc[0];
  nc = yc[0];
  
  ligne = 0;
  colo = 0;
  
  for (i = 1; i <= nl; i++) {
    if (((xc[i] - cs/2) <= x) && ((xc[i] + cs/2) > x))
      ligne = i;
  }
  
  for (i = 1; i <= nc; i++) {
    if (((yc[i] - cs/2) <= y) && ((yc[i] + cs/2) > y))
      colo = i;
  }
  *na = asc[ligne][colo]; 
}

/* dedans pour vérification sous R */

void dedansr(double *ptsr, double *xcr, double *ycr, double *na,
	     double *cs, double *ascr, int *nl, int *nc, int *nlocs)
{
  int i,j,k;
  double *pts, *xc, *yc, **asc;
  vecalloc(&pts, *nlocs);
  vecalloc(&xc, *nl);
  vecalloc(&yc, *nc);
  taballoc(&asc, *nl, *nc);
  
  pts[1] = ptsr[0];
  pts[2] = ptsr[1];

  for (i = 1; i <= *nl; i++) {
    xc[i] = xcr[i-1];
  }
  for (i = 1; i <= *nc; i++) {
    yc[i] = ycr[i-1];
  }
  k = 0;
  for (i = 1; i <= *nl; i++) {
    for (j = 1; j <= *nc; j++) {
      asc[i][j] = ascr[k];
      k++;
    }
  }
  dedans(pts, xc, yc, na, *cs, asc);
   
  freevec(pts);
  freevec(xc);
  freevec(yc);
  freetab(asc);
}


/* ****************************************************************
   *                                                              *
   *   Fonction permettant de randomiser un trajet, sur la base   *
   *   de distances interlocs, et de temps entre les locs,        *
   *   ainsi que d'angles tirés de façon indépendante             *
   *                                                              *
   **************************************************************** */

void rpath(double **xp, double *rcx, double *rcy, double **asc, 
	   double **tabdist, double *dt, 
	   double *angles, double *xc, double *yc,
	   double *cs, int r)
{
  int i, j, k, l, m, nsam, nltd, *index, *indangles, nlocs;
  double *pts, na, interv, *dobs, dech, anglech, ang;
  
  vecalloc(&pts, 2);
  nltd = tabdist[0][0];
  nlocs = xp[0][0];
  vecintalloc(&indangles, (nlocs-2));
  
  /* remplissage du vecteur indangle */
  for (i = 1; i <= (nlocs - 2); i++) {
    indangles[i] = i;
  }
    
  /* 1. Première localisation du trajet */
  k = 0;
  ang = 0;
  
  while (k==0) {
    
    /* Tirage au sort des coordonnées de la locs */
    xp[1][1] = (alea())*(rcx[2]-rcx[1]) + rcx[1];
    xp[1][2] = (alea())*(rcy[2]-rcy[1]) + rcy[1];
    
    pts[1] = xp[1][1];
    pts[2] = xp[1][2];
    
    /* Vérifie que la loc tombe bien dans la zone d'étude */
    dedans(pts, xc, yc, &na, *cs, asc);
    if (na != -9999)
      k = 1;
    
  }
  

  /* Boucle pour les localisations suivantes */
  for (i = 1; i <= (nlocs-1); i++) {
    interv = dt[i];
      
    /* combien y-a-t-il de distances pour l'intervalle observé ? */
    nsam = 0;
    for (j = 1; j <= nltd; j++) {
      if (tabdist[j][1]==interv)
	nsam++;
    }
      
    /* construction du tableau de distances */
    vecalloc(&dobs, nsam);
    
    /* le vecteur index servira à tirer une loc de façon aléatoire */
    vecintalloc(&index, nsam);
    for (l = 1; l <= nsam; l++) {
      index[l] = l;
    }
      
    /* mais dans un premier temps, 
       on récupère les distances correspondantes */
    m = 1;
    for (j = 1; j <= nltd; j++) {
      if (tabdist[j][1]==interv) {
	dobs[m] = tabdist[j][2];
	m++;
      }
    }
    
    k = 0;
    while (k == 0) {
      /* Distance échantillonnée */
      r = (int) (alea() * 100);
      getpermutation(index, j * r);
      dech = dobs[index[1]];
      
      /* Angles échantillonnés */
      getpermutation(indangles, j * r);
      anglech = angles[indangles[1]];
      
      /* mise à jour des angles */
      ang = (ang + anglech);
      
      /* calcul des nouvelles coordonnées */
      xp[i+1][1] = xp[i][1] + dech * cos(ang);
      xp[i+1][2] = xp[i][2] + dech * sin(ang);
      
      pts[1] = xp[i+1][1];
      pts[2] = xp[i+1][2];
      
      dedans(pts, xc, yc, &na, *cs, asc);
      if (na != -9999)
	k = 1;
    }
    freevec(dobs);
    freeintvec(index);
  }
  
  freeintvec(indangles);
  freevec(pts);
}




/* Vérification de rpath sous R */

void randpath(double *xpr, double *rcrx, double *rcry, double *ascr, 
	      double *xcr, double *ycr, double *csr,
	      double *tabdistr, double *dtr, double *anglesr, 
	      int *nlasc, int *ncasc, int *nltdr, int *nlocsr)
{
  /* déclaration de variables */
  int i, j, k, r, nlocs, nltd;
  double **xp, *rcx, *rcy, **asc, **tabdist, *dt, *angles;
  double *xc,*yc, cs;
  
  /* allocation de mémoire et définition des constantes */
  nlocs = *nlocsr;
  nltd = *nltdr;
  cs = *csr;
  
  taballoc(&xp, nlocs, 2);
  vecalloc(&rcx, 2);
  vecalloc(&rcy, 2);
  taballoc(&asc, *nlasc, *ncasc);
  vecalloc(&xc, *nlasc);
  vecalloc(&yc, *ncasc);
  taballoc(&tabdist, nltd, 2);
  vecalloc(&dt, nlocs-1);
  vecalloc(&angles, nlocs-2);
  
  /* remplissage des variables locales */
  k = 0;
  for (i = 1; i <= nlocs; i++) {
    for (j = 1; j <= 2; j++) {
      xp[i][j] = xpr[k];
      k++;
    }
  }
  rcx[1] = rcrx[0];
  rcx[2] = rcrx[1];
  rcy[1] = rcry[0];
  rcy[2] = rcry[1];
  
  k = 0;
  for (i = 1; i <= *nlasc; i++) {
    for (j = 1; j <= *ncasc; j++) {
      asc[i][j] = ascr[k];
      k++;
    }
  }
  
  k = 0;
  for (i = 1; i <= nltd; i++) {
    for (j = 1; j <= 2; j++) {
      tabdist[i][j] = tabdistr[k];
      k++;
    }
  }

  for (i = 1; i <= (nlocs - 1); i++) {
    dt[i] = dtr[i-1];
  }
  for (i = 1; i <= *nlasc; i++) {
    xc[i] = xcr[i-1];
  }
  for (i = 1; i <= *ncasc; i++) {
    yc[i] = ycr[i-1];
  }
  for (i = 1; i <= (nlocs - 2); i++) {
    angles[i] = anglesr[i-1];
  }

  r = 1;
  rpath(xp, rcx, rcy, asc, tabdist, dt, angles,
	xc, yc, &cs, r);
  
  /* sortie de xp vers r */
  k = 0;
  for (i = 1; i <= nlocs; i++) {
    for (j = 1; j <= 2; j++) {
      xpr[k] = xp[i][j];
      k++;
    }
  }
  
  /* vidage de la mémoire */
  freetab(xp);
  freevec(rcx);
  freevec(rcy);
  freetab(asc);
  freetab(tabdist);
  freevec(dt);
  freevec(angles);
  freevec(xc);
  freevec(yc);
}




/* ****************************************************************
   *                                                              *
   *   joinkasc est l'équivalent de join.kasc sous R.             *
   *   On donne un tableau de points, un objet kasc, et on        *
   *   obtient un tableau qui donne la composition des cartes     *
   *   sous chaque point (dans res).                              *
   *                                                              *
   **************************************************************** */


void joinkasc(double **xp, double **kasc, double **res, int nl, int nc,
	      double *xc, double *yc, double *cs)
{
  int i,j, nlocs, nvar;
  double **carte, *pts, na;
  
  taballoc(&carte, nl, nc);
  vecalloc(&pts, 2);
  nlocs = xp[0][0];
  nvar = kasc[1][0];
  
  for (j = 1; j <= nvar; j++) {
    getcarte(carte, kasc, &j);
    for (i = 1; i <= nlocs; i++) {
      pts[1] = xp[i][1];
      pts[2] = xp[i][2];
      dedans(pts, xc, yc, &na, *cs, carte);
      res[i][j] = na;
    }
  }
  freetab(carte);
  freevec(pts);
}


/* Vérification de joinkasc sous R */

void joinkascr(double *xpr, double *kascr, int *nlasc, int *ncasc,
	       double *xcr, double *ycr, double *cs, int *nlocs,
	       int *nvar, double *resr)
{
  int i,j,k;
  double **xp, **kasc, **res, *xc, *yc, cellsize;
  
  taballoc(&xp, *nlocs, 2);
  taballoc(&res, *nlocs, *nvar);
  taballoc(&kasc, (*nlasc) * (*ncasc), *nvar);
  vecalloc(&xc, *nlasc);
  vecalloc(&yc, *ncasc);
  cellsize = *cs;
  
  for (i = 1; i <= *nlasc; i++) {
    xc[i] = xcr[i-1];
  }
  for (i = 1; i <= *ncasc; i++) {
    yc[i] = ycr[i-1];
  }
  k = 0;
  for (i = 1; i <= ((*nlasc) * (*ncasc)); i++) {
    for (j = 1; j <= *nvar; j++) {
      kasc[i][j] = kascr[k];
      k++;
    }
  }
  k = 0;
  for (i = 1; i <= *nlocs; i++) {
    for (j = 1; j <= 2; j++) {
      xp[i][j] = xpr[k];
      k++;
    }
  }
  
  joinkasc(xp, kasc, res,  *nlasc, *ncasc,
	   xc, yc, &cellsize);
  
  k = 0;
  for (i = 1; i <= *nlocs; i++) {
    for (j = 1; j <= *nvar; j++) {
      resr[k] = res[i][j];
      k++;
    }
  }
  
  freetab(xp);
  freetab(res);
  freetab(kasc);
  freevec(xc);
  freevec(yc);
}



/* ****************************************************************
   *                                                              *
   *   Fonction permettant de randomiser un trajet, sur la base   *
   *   de distances interlocs, et de temps entre les locs,        *
   *   ainsi que d'angles tirés de façon indépendante.            *
   *   Randomisation dans l'espace écologique (retourne           *
   *   marginalité et tolérance.                                  *
   *                                                              *
   **************************************************************** */


void randmargtol(double *xpr, double *rcrx, double *rcry, double *ascr, 
		 double *cwr, double *kascr, double *xcr, double *ycr,
		 double *csr,
		 double *tabdistr, double *dtr, double *anglesr, double *marr, 
		 double *tolr, int *nrepr, int *nlasc, 
		 int *ncasc, int *nvarr, int *nltdr, int *nlocsr)
{
  /* déclaration de variables */
  int i, j, k, nr, r, nlocs, nltd, nvar;
  double **xp, *rcx, *rcy, **asc, **kasc, **tabdist, *dt, *angles;
  double *cw, *xc,*yc, cellsize, **res, *mar, *tol;
  
  /* allocation de mémoire et définition des constantes */
  nr = *nrepr;
  nlocs = *nlocsr;
  nltd = *nltdr;
  nvar = *nvarr;
  cellsize = *csr;
  
  taballoc(&xp, nlocs, 2);
  taballoc(&res, nlocs, nvar);
  vecalloc(&rcx, 2);
  vecalloc(&rcy, 2);
  vecalloc(&mar, nvar);
  vecalloc(&tol, nvar);
  taballoc(&asc, *nlasc, *ncasc);
  taballoc(&kasc, (*nlasc)*(*ncasc), nvar);
  vecalloc(&cw, nvar);
  vecalloc(&xc, *nlasc);
  vecalloc(&yc, *ncasc);
  taballoc(&tabdist, nltd, 2);
  vecalloc(&dt, nlocs-1);
  vecalloc(&angles, nlocs-2);
  
  /* remplissage des variables locales */
  k = 0;
  for (i = 1; i <= nlocs; i++) {
    for (j = 1; j <= 2; j++) {
      xp[i][j] = xpr[k];
      k++;
    }
  }
  rcx[1] = rcrx[0];
  rcx[2] = rcrx[1];
  rcy[1] = rcry[0];
  rcy[2] = rcry[1];
  
  k = 0;
  for (i = 1; i <= *nlasc; i++) {
    for (j = 1; j <= *ncasc; j++) {
      asc[i][j] = ascr[k];
      k++;
    }
  }
  
  k = 0;
  for (i = 1; i <= ((*nlasc)*(*ncasc)); i++) {
    for (j = 1; j <= nvar; j++) {
      kasc[i][j] = kascr[k];
      k++;
    }
  }
  
  k = 0;
  for (i = 1; i <= nltd; i++) {
    for (j = 1; j <= 2; j++) {
      tabdist[i][j] = tabdistr[k];
      k++;
    }
  }

  for (i = 1; i <= (nlocs - 1); i++) {
    dt[i] = dtr[i-1];
  }
  for (i = 1; i <= nvar; i++) {
    cw[i] = cwr[i-1];
  }
  for (i = 1; i <= *nlasc; i++) {
    xc[i] = xcr[i-1];
  }
  for (i = 1; i <= *ncasc; i++) {
    yc[i] = ycr[i-1];
  }
  for (i = 1; i <= (nlocs - 2); i++) {
    angles[i] = anglesr[i-1];
  }
  
  /* Calcul des valeurs observées pour la marginalité et la tolérance */
  /* on fait une jointure sur les cartes */
  joinkasc(xp, kasc, res,  *nlasc, *ncasc,
	   xc, yc, &cellsize);
  
  /* remise à 0 des vecteurs mar et tol */
  for (j = 1; j <= nvar; j++) {
    mar[j] = 0;
    tol[j] = 0;
  }
  
  /* 1. Calcul des moyennes */
  for (i = 1; i <= nlocs; i++) {
    for (j = 1; j <= nvar; j++) {
      mar[j] = mar[j] + (1 /((double) nlocs)) * res[i][j];
    }
  }
  /* 2. Centrage du tableau res */
  for (i = 1; i <= nlocs; i++) {
    for (j = 1; j <= nvar; j++) {
      res[i][j] = res[i][j] - mar[j];
    }
  }
  /* 3. Calcul des variances */
  for (i = 1; i <= nlocs; i++) {
    for (j = 1; j <= nvar; j++) {
      tol[j] = tol[j] + (1 /((double) nlocs)) * res[i][j] * res[i][j];
    }
  }
  /* 4. Calcul de la tolérance et de la marginalité */
  for (j = 1; j <= nvar; j++) {
    marr[0] = marr[0] + (mar[j] * mar[j] * cw[j]);
    tolr[0] = tolr[0] + (tol[j] * cw[j]);
  }
  
  /* Début de la boucle des répétitions */
  for (r = 2; r <= nr; r++) {
    /* on génère le trajet */
    rpath(xp, rcx, rcy, asc, tabdist, dt, angles, xc, yc, 
	  &cellsize, r);
    /* on fait une jointure sur les cartes */
    joinkasc(xp, kasc, res,  *nlasc, *ncasc,
	     xc, yc, &cellsize);
    
    /* remise à 0 des vecteurs mar et tol */
    for (j = 1; j <= nvar; j++) {
      mar[j] = 0;
      tol[j] = 0;
    }
    
    /* 1. Calcul des moyennes */
    for (i = 1; i <= nlocs; i++) {
      for (j = 1; j <= nvar; j++) {
	mar[j] = mar[j] + (1 /((double) nlocs)) * res[i][j];
      }
    }
    /* 2. Centrage du tableau res */
    for (i = 1; i <= nlocs; i++) {
      for (j = 1; j <= nvar; j++) {
	res[i][j] = res[i][j] - mar[j];
      }
    }
    /* 3. Calcul des variances */
    for (i = 1; i <= nlocs; i++) {
      for (j = 1; j <= nvar; j++) {
	tol[j] = tol[j] + (1 /((double) nlocs)) * res[i][j] * res[i][j];
      }
    }
    /* 4. Calcul de la tolérance et de la marginalité */
    for (j = 1; j <= nvar; j++) {
      marr[r-1] = marr[r-1] + (mar[j] * mar[j] * cw[j]);
      tolr[r-1] = tolr[r-1] + (tol[j] * cw[j]);
    }
  }
  
  
  

  /* vidage de la mémoire */
  freetab(xp);
  freevec(rcx);
  freevec(rcy);
  freevec(mar);
  freevec(tol);
  freetab(asc);
  freetab(kasc);
  freetab(tabdist);
  freevec(dt);
  freevec(angles);
  freevec(cw);
  freevec(xc);
  freevec(yc);
}



/* ****************************************************************
   *                                                              *
   *   Fonction permettant de randomiser la position de locs      *
   *   sur une zone de façon indépendantes                        *
   *                                                              *
   **************************************************************** */

void rpoint(double **xp, double *rcx, double *rcy, double **asc, 
	    double *xc, double *yc, double *cs)
{
  int i, k, nlocs;
  double *pts, na;
  
  vecalloc(&pts, 2);
  nlocs = xp[0][0];
  
  for (i = 1; i <= nlocs; i++) {
    k=0;
    while (k==0) {
      
      /* Tirage au sort des coordonnées de la locs */
      xp[i][1] = (alea())*(rcx[2]-rcx[1]) + rcx[1];
      xp[i][2] = (alea())*(rcy[2]-rcy[1]) + rcy[1];
      
      pts[1] = xp[i][1];
      pts[2] = xp[i][2];
      
      /* Vérifie que la loc tombe bien dans la zone d'étude */
      dedans(pts, xc, yc, &na, *cs, asc);
      if (na != -9999)
	k = 1;
      
    }
  }
    freevec(pts);
}




/* ****************************************************************
   *                                                              *
   *   Fonction identique à randmargtol, mais au lieu de          *
   *   randomiser des trajets, on randomise des locs.             *
   *                                                              *
   **************************************************************** */


void randmargtolpts(double *xpr, double *rcrx, double *rcry, double *ascr, 
		    double *cwr, double *kascr, double *xcr, double *ycr,
		    double *csr, double *marr, double *tolr, int *nrepr, int *nlasc, 
		    int *ncasc, int *nvarr, int *nlocsr)
{
  /* déclaration de variables */
  int i, j, k, nr, r, nlocs, nvar;
  double **xp, *rcx, *rcy, **asc, **kasc;
  double *cw, *xc,*yc, cellsize, **res, *mar, *tol;
  
  /* allocation de mémoire et définition des constantes */
  nr = *nrepr;
  nlocs = *nlocsr;
  nvar = *nvarr;
  cellsize = *csr;
  
  taballoc(&xp, nlocs, 2);
  taballoc(&res, nlocs, nvar);
  vecalloc(&rcx, 2);
  vecalloc(&rcy, 2);
  vecalloc(&mar, nvar);
  vecalloc(&tol, nvar);
  taballoc(&asc, *nlasc, *ncasc);
  taballoc(&kasc, (*nlasc)*(*ncasc), nvar);
  vecalloc(&cw, nvar);
  vecalloc(&xc, *nlasc);
  vecalloc(&yc, *ncasc);

  /* remplissage des variables locales */
  k = 0;
  for (i = 1; i <= nlocs; i++) {
    for (j = 1; j <= 2; j++) {
      xp[i][j] = xpr[k];
      k++;
    }
  }
  rcx[1] = rcrx[0];
  rcx[2] = rcrx[1];
  rcy[1] = rcry[0];
  rcy[2] = rcry[1];
  
  k = 0;
  for (i = 1; i <= *nlasc; i++) {
    for (j = 1; j <= *ncasc; j++) {
      asc[i][j] = ascr[k];
      k++;
    }
  }
  
  k = 0;
  for (i = 1; i <= ((*nlasc)*(*ncasc)); i++) {
    for (j = 1; j <= nvar; j++) {
      kasc[i][j] = kascr[k];
      k++;
    }
  }
  
  for (i = 1; i <= nvar; i++) {
    cw[i] = cwr[i-1];
  }
  for (i = 1; i <= *nlasc; i++) {
    xc[i] = xcr[i-1];
  }
  for (i = 1; i <= *ncasc; i++) {
    yc[i] = ycr[i-1];
  }
  
  /* Calcul des valeurs observées pour la marginalité et la tolérance */
  /* on fait une jointure sur les cartes */
  joinkasc(xp, kasc, res,  *nlasc, *ncasc,
	   xc, yc, &cellsize);
  
  /* remise à 0 des vecteurs mar et tol */
  for (j = 1; j <= nvar; j++) {
    mar[j] = 0;
    tol[j] = 0;
  }
  
  /* 1. Calcul des moyennes */
  for (i = 1; i <= nlocs; i++) {
    for (j = 1; j <= nvar; j++) {
      mar[j] = mar[j] + (1 /((double) nlocs)) * res[i][j];
    }
  }
  /* 2. Centrage du tableau res */
  for (i = 1; i <= nlocs; i++) {
    for (j = 1; j <= nvar; j++) {
      res[i][j] = res[i][j] - mar[j];
    }
  }
  /* 3. Calcul des variances */
  for (i = 1; i <= nlocs; i++) {
    for (j = 1; j <= nvar; j++) {
      tol[j] = tol[j] + (1 /((double) nlocs)) * res[i][j] * res[i][j];
    }
  }
  /* 4. Calcul de la tolérance et de la marginalité */
  for (j = 1; j <= nvar; j++) {
    marr[0] = marr[0] + (mar[j] * mar[j] * cw[j]);
    tolr[0] = tolr[0] + (tol[j] * cw[j]);
  }
  
  /* Début de la boucle des répétitions */
  for (r = 2; r <= nr; r++) {
    /* on génère le trajet */
    rpoint(xp, rcx, rcy, asc, xc, yc, &cellsize);
    /* on fait une jointure sur les cartes */
    joinkasc(xp, kasc, res,  *nlasc, *ncasc,
	     xc, yc, &cellsize);
    
    /* remise à 0 des vecteurs mar et tol */
    for (j = 1; j <= nvar; j++) {
      mar[j] = 0;
      tol[j] = 0;
    }
    
    /* 1. Calcul des moyennes */
    for (i = 1; i <= nlocs; i++) {
      for (j = 1; j <= nvar; j++) {
	mar[j] = mar[j] + (1 /((double) nlocs)) * res[i][j];
      }
    }
    /* 2. Centrage du tableau res */
    for (i = 1; i <= nlocs; i++) {
      for (j = 1; j <= nvar; j++) {
	res[i][j] = res[i][j] - mar[j];
      }
    }
    /* 3. Calcul des variances */
    for (i = 1; i <= nlocs; i++) {
      for (j = 1; j <= nvar; j++) {
	tol[j] = tol[j] + (1 /((double) nlocs)) * res[i][j] * res[i][j];
      }
    }
    /* 4. Calcul de la tolérance et de la marginalité */
    for (j = 1; j <= nvar; j++) {
      marr[r-1] = marr[r-1] + (mar[j] * mar[j] * cw[j]);
      tolr[r-1] = tolr[r-1] + (tol[j] * cw[j]);
    }
  }
  
  /* vidage de la mémoire */
  freetab(xp);
  freevec(rcx);
  freevec(rcy);
  freevec(mar);
  freevec(tol);
  freetab(asc);
  freetab(kasc);
  freevec(cw);
  freevec(xc);
  freevec(yc);
}



/* ********************************************************************
 *                                                                    *
 *            Diminution de la résolution d'une carte                 *
 *                                                                    *
 * ******************************************************************** */



/* Pour les cartes facteurs */

void regroufacasc(double **asce, double **ascs, int *np,
		  int *nlev)
{
  /* déclaration de variables */
  int i, j, k, l, m, dr, fr, dc, fc, nre, nrs, nce, ncs, nl, *ll, max, vm, na, *vecmax;
  nre = asce[0][0];
  nrs = ascs[0][0];
  nce = asce[1][0];
  ncs = ascs[1][0];
  nl = *nlev;
  vecintalloc(&ll, nl);
  
  /* boucle de gommage */
  for (i = 1; i <= nrs; i++) {
    for (j = 1; j <= ncs; j++) {
      
      /* extraction du sous 
	 tableau correspondant */
      dr = (i-1)*(*np) + 1;
      fr = i*(*np);
      dc = (j-1)*(*np) + 1;
      fc = j*(*np);
      
      /* On vide ll */
      for (m = 1; m <= nl; m++) {
	ll[m] = 0;
      }
      
      /* On compte le nombre de niveaux */
      na = 0;
      for (k = dr; k <= fr; k++) {
	for (l = dc; l <= fc; l++) {
	  if (asce[k][l] != -9999)
	    ll[(int) asce[k][l]]++;
	  if (asce[k][l] == -9999)
	    na++;
	}
      }
      
      if (na != (*np)*(*np)) {
	/* On calcule le nombre max */
	vm = ll[1];
	for (k = 2; k <= nl; k++) {
	  if (ll[k] >= vm) {
	    vm = ll[k];
	  }
	}
	
	/* ... et le nombre DE max */
	max = 0;
	for (k = 1; k <= nl; k++) {
	  if (ll[k] == vm) {
	    max++;
	  }
	}
      
	/* on isole les niveaux dont le nombre est max */
	vecintalloc(&vecmax, max);
	l = 1;
	for (k = 1; k<=nl; k++) {
	  if (ll[k] == vm) {
	    vecmax[1] = k;
	  }
	}
	
	/* Echantillonnage aléatoire 
	   des niveaux en cas d'égalité */
	if (max > 1) {
	  getpermutation(vecmax, i*j); /* tirage au sort ligne */
	}
	ascs[i][j] = (double) vecmax[1];
	freeintvec(vecmax);
      } else {
	ascs[i][j] = -9999;
      }
      
    }
  }
  freeintvec(ll);
}



/* Regroufacasc version R */

void regroufacascr(double *ascer, double *ascsr, int *npr,
		   int *nlevr, int *nle, int *nce, int *nls, 
		   int *ncs)
{
  /* Déclaration des variables */
  int i,j,k, np, nlev;
  double **asce, **ascs;
  
  /* Allocation de mémoire */
  np = *npr;
  nlev = *nlevr;
  taballoc(&asce, *nle, *nce);
  taballoc(&ascs, *nls, *ncs);
  
  /* remplissage des tableaux */
  k =0;
  for (i = 1; i <= *nle; i++) {
    for (j = 1; j <= *nce; j++) {
      asce[i][j] = ascer[k];
      k++;
    }
  }
  
  /* fonction */
  regroufacasc(asce, ascs, &np, &nlev);
  
  k =0;
  for (i = 1; i <= *nls; i++) {
    for (j = 1; j <= *ncs; j++) {
      ascsr[k] = ascs[i][j];
      k++;
    }
  }
  
  /* libération de la mémoire */
  freetab(asce);
  freetab(ascs);
}




/* regrouascnum pour les cartes numériques */

void regrouascnum(double **ascent, double **ascso)
{
  int i, j, k, l, n, nle, nls, nce, ncs, nreg;
  double moy, tmp;
  
  /* définition des variables */
  nle = ascent[0][0];
  nce = ascent[1][0];
  nls = ascso[0][0];
  ncs = ascso[1][0];
  nreg = nle/nls;
  
  for (i = 1; i <= nls; i++) {
    for (j = 1; j <= ncs; j++) {
      moy = 0;
      n = 0;
      for (k = 1; k <= nreg; k++) {
	for (l = 1; l <= nreg; l++) {
	  tmp = ascent[((i - 1) * nreg) + k][((j - 1) * nreg) + l];
	  if (tmp != -9999) {
	    moy = tmp + moy;
	  }
	  if (tmp == -9999) {
	    n++;
	  }
	}
      }
      ascso[i][j] = moy / ((double) (nreg * nreg));
      if (n == (nreg * nreg))
	ascso[i][j] = -9999;
    }
  }
  
}


/* Version pour R */

void regrouascnumr(double *ascentr, double *ascsor, double *nler, double *ncer,
		   double *nlsr, double *ncsr)
{
  /* Définition de variables */
  int i, j, k, nle, nce, nls, ncs;
  double **ascent, **ascso;
  
  /* Allocation de mémoire */
  nle = *nler;
  nce = *ncer;
  nls = *nlsr;
  ncs = *ncsr;
  
  taballoc(&ascent, nle, nce);
  taballoc(&ascso, nls, ncs);
  
  /* remplissage des variables locales C */
  k = 0;
  for (i = 1; i <= nle; i++) {
    for (j = 1; j <= nce; j++) {
      ascent[i][j] = ascentr[k];
      k++;
    }
  }
  
  k = 0;
  for (i = 1; i <= nls; i++) {
    for (j = 1; j <= ncs; j++) {
      ascso[i][j] = ascsor[k];
      k++;
    }
  }
  
  /* procédure C */
  regrouascnum(ascent, ascso);
  
  /* Retour vers R */
  k = 0;
  for (i = 1; i <= nls; i++) {
    for (j = 1; j <= ncs; j++) {
      ascsor[k] = ascso[i][j];
      k++;
    }
  }
  
  /* On vide la mémoire */
  freetab(ascso);
  freetab(ascent);
}







/* 
   Toutes les variables facteurs doivent 
   être étiquetées de 1 à n sans valeur manquantes 
   Version pour les objets kasc
*/


void regroukasc(double *kascr, double *kascniou, int *nrow, 
		int *ncol, int *nvar, int *npix,
		int *typer, int *nrniou, int *ncniou)
{
  /* déclaration de variables */
  double **kasc, **asc, **ascn, **kascn;
  int i, j, k, l, nr, nc, nv, *typ, np, nrn, ncn, nl;
  
  
  /* allocation de mémoire */
  nr = *nrow;
  nc = *ncol;
  nv = *nvar;
  np = *npix;
  nrn = *nrniou;
  ncn = *ncniou;

  taballoc(&kasc, nr*nc, nv);
  taballoc(&kascn, nrn*ncn, nv);
  taballoc(&asc, nr, nc);
  taballoc(&ascn, nrn, ncn);
  vecintalloc(&typ, nv);
  
  /* passage de R à C */
  for (i = 1; i<=nv; i++) {
    typ[i] = typer[i-1];
  }
  
  k = 0;
  for (i=1; i<= (nc*nr); i++) {
    for (j = 1; j<=nv; j++) {
      kasc[i][j]=kascr[k];
      k++;
    }
  }
  
  
  /* boucle pour chaque carte */
  for (k=1; k<=nv; k++) {
    getcarte(asc, kasc, &k);
    if (typ[k] == 0)
      regrouascnum(asc, ascn);
    nl = 0;
    if (typ[k] == 1) {
      nl = (int) asc[1][1];
      /* On compte le nombre de niveaux du facteur */
      for (i = 1; i <= nr; i++) {
	for (j = 1; j <= nc; j++) {
	  if (((int) asc[i][j]) > nl)
	    nl = (int) asc[i][j];
	}
      }
      
      regroufacasc(asc, ascn, &np, &nl);
    }
    
    
    l = 1;
    for (j = 1; j <= nc; j++) {
      for (i = 1; i <= nr; j++) {
	kascn[l][k] = ascn[i][j];
      }
    }
  }
  
  /* repassage sous R */
  k=0;
  for (i = 1; i <= (nrn*ncn); i++) {
    for (j = 1; j <= nv; j++) {
      kascniou[k] = kascn[i][j];
      k++;
    }
  }
  
  /* Libération de la mémoire */
  freetab(kasc);
  freetab(asc);
  freetab(kascn);
  freetab(ascn);
  freeintvec(typ);
}






/* *******************************************

   Transformation d'une matrice carrée 
   symétrique en matrice à la puissance -1/2

   ******************************************* */

void matmudemi(double **X, double **Y)
{
  /* Déclaration des variables */
  int i, j, nc, rg;
  double **U, **L, *lambda, **Ubis, **Uter;
  
  /* Allocation de mémoire */
  nc = X[0][0];
  taballoc(&U, nc, nc);
  taballoc(&Ubis, nc, nc);
  taballoc(&Uter, nc, nc);
  taballoc(&L, nc, nc);
  vecalloc(&lambda, nc);
  
  /* Remplissage de Xtmp */
  for (i = 1; i <= nc; i++) {
    for (j = 1; j <= nc; j++) {
      U[i][j] = X[i][j];
    }
  }
  
  /* Diagonalisation de X */
  DiagobgComp(nc, U, lambda, &rg);
  
  /* Calcul de la matrice lambda -1/2 */
  for (i = 1; i<=nc; i++) {
    L[i][i] = 1 / sqrt(lambda[i]);
  }
  
  /* Résultat */
  prodmatABC(U, L, Ubis);  
  for (i = 1; i <= nc; i++) {
    for (j = 1; j <= nc; j++) {
      Uter[i][j] = U[j][i];
    }
  }
  prodmatABC(Ubis, Uter, Y);
  
  freetab(U);
  freetab(Ubis);
  freetab(Uter);
  freetab(L);
  freevec(lambda);
}



/* La même, mais sous R */

void matmudemir(double *Xr, double *Yr, int *ncr)
{
  /* définition de variables et allocation de mémoire */
  int i, j, k, nc;
  double **X, **Y;
  nc = *ncr;
  taballoc(&X, nc, nc);
  taballoc(&Y, nc, nc);
  
  /* remplissage des variables */
  k = 0;
  for (i=1; i <= nc; i++) {
    for (j = 1; j <= nc; j++) {
      X[i][j] = Xr[k];
      k++;
    }
  }
  
  /* Inversion de matrice */
  matmudemi(X, Y);
  
  /* Retour vers R */
  k = 0;
  for (i=1; i <= nc; i++) {
    for (j = 1; j <= nc; j++) {
      Yr[k] = Y[i][j];
      k++;
    }
  }
  
  /* Libération de la mémoire */
  freetab(X);
  freetab(Y);
}





/* *******************************************

   Enfa sous C

   ****************************************** */

void enfa(double **Z, double *p, int *nvar, int *npix,
	  double *vp)
{
  /* déclaration de variables */
  double *m, *z, *y, **W, **Rs, **Rg, **Zbis, **Rsm12, norz;
  double **Wtmp, **H, **Iv, **yyt, **Ivmyyt, **Htmp;
  int i, j, nv, np, rg;
  
  /* allocation de mémoire */
  np = *npix;
  nv = *nvar;
  norz = 0;
  rg = 0;
    
  taballoc(&Zbis, np, nv);
  vecalloc(&m, nv);
  vecalloc(&z, nv);
  vecalloc(&y, nv);
  taballoc(&W, nv, nv);
  taballoc(&Iv, nv, nv);
  taballoc(&Ivmyyt, nv, nv);
  taballoc(&Htmp, nv, nv);
  taballoc(&yyt, nv, nv);
  taballoc(&H, nv, nv);
  taballoc(&Wtmp, nv, nv);
  taballoc(&Rg, nv, nv);
  taballoc(&Rs, nv, nv);
  taballoc(&Rsm12, nv, nv);
  
  /* Calcul de la marginalité */
  for (j = 1; j<=nv; j++) {
    for (i = 1; i <= np; i++){
      m[j] = m[j] + p[i] * Z[i][j];
    }
  }
  
  /* Calcul de Rs et Rg */
  for (i = 1; i<=np; i++) {
    for (j = 1; j <= nv; j++) {
      Zbis[i][j] = Z[i][j] * sqrt(p[i]);
    }
  }
  prodmatAtAB(Zbis, Rs);
  for (i = 1; i<=np; i++) {
    for (j = 1; j <= nv; j++) {
      Zbis[i][j] = Z[i][j] * sqrt((1/ ((double) np)));
    }
  }
  prodmatAtAB(Zbis, Rg);
  
  /* Calcul de Rs -1/2  */
  matmudemi(Rs,Rsm12);

  /* Calcul de z */
  for (i = 1; i <= nv; i++) {
    for (j = 1; j <= nv; j++) {
      z[i] = z[i] + Rsm12[i][j] * m[j];
    }
  }
  
  /* Calcul de la norme de z */
  for (i = 1; i <= nv; i++) {
    norz = norz + (z[i] * z[i]);
  }
  norz = sqrt(norz);
  
  /* Calcul de y */
  for (i = 1 ; i <= nv; i++) {
    y[i] = z[i] / norz;
  }
  
  /* Calcul de W */
  prodmatABC(Rsm12, Rg, Wtmp);
  prodmatABC(Wtmp, Rsm12, W);
  

  /* **************************** */
  /* Le gros morceau: calcul de H */
  /* **************************** */

  /* Calcul de yyt */
  
  for (i = 1; i<= nv; i++) {
    for (j = 1; j <= nv; j++) {
      yyt[i][j] = y[i] * y[j];
    }
  }
  

  /* Remplissage de Iv */
  
  for (i = 1; i <= nv; i++) {
    Iv[i][i] = 1;
  }
  

  /* Calcul de Ivmyyt */
  
  for (i = 1; i <= nv; i++) {
    for (j = 1; j <= nv; j++) {
      Ivmyyt[i][j] = Iv[i][j] - yyt[i][j];
    }
  }
  
  
  /* calcul de H */
  
  prodmatABC(Ivmyyt, W, Htmp);
  prodmatABC(Htmp, Ivmyyt, H);
    

  /* Diagonalisation de H */
  DiagobgComp(nv, H, vp, &rg);
  
    
  /* Libération de la mémoire */
  freevec(m);
  freevec(z);
  freevec(y);
  freetab(W);
  freetab(Iv);
  freetab(Ivmyyt);
  freetab(Htmp);
  freetab(yyt);
  freetab(H);
  freetab(Wtmp);
  freetab(Rg);
  freetab(Rs);
  freetab(Rsm12);
  freetab(Zbis);
}


/* *****************************************************

   Vérif de l'ENFA sous R

   ***************************************************** */

void enfar(double *Zr, double *pr, int *nvar, int *npix,
	   double *vpr)
{
  /* Déclaration de variables */
  int i, j, k, np, nv;
  double **Z, *p, *vp;
  
  /* Allocation de mémoire */
  taballoc(&Z, *npix, *nvar);
  vecalloc(&p, *npix);
  vecalloc(&vp, *nvar);

  np = *npix;
  nv = *nvar;
  
  /* remplissage des variables locales */
  k = 0;
  for (i=1; i <= np; i++) {
    for (j = 1; j <= nv; j++) {
      Z[i][j] = Zr[k];
      k++;
    }
  }

  for (i = 1; i <= np; i++) {
    p[i] = pr[i-1];
  }

  /* ENFA ...*/
  enfa(Z, p, &nv, &np, vp);

  
  /* ... Sorties vers R ... */
  for (i = 1; i <= nv; i++) {
    vpr[i-1] = vp[i];
  }
  
  /* ... et enfin libération de la mémoire */
  freetab(Z);
  freevec(p);
  freevec(vp);

}



/* ********************************************************
   
   Randomisation dans l'ENFA: test de la première valeur 
   propre de spécialisation

   ******************************************************** */

void randenfa(double **Z, double *p, int *nrep, double *res)
{
  /* Définition de variables */
  int i, j, k, nr, nv, np, ntot;
  double *psim, *vp;

  /* Allocation de mémoire */
  np = Z[0][0];
  nv = Z[1][0];
  ntot = 0;
  nr = *nrep;
  vecalloc(&psim, np);
  vecalloc(&vp, nv);
    
  /* Décompte du nombre total de localisations */
  for (i = 1; i <= np; i++) {
    ntot = ntot + p[i];
  }
  
  /* Début du processus de randomisation */
  for (k = 1; k <= *nrep; k++) {
    
    /* On commence par vider le vecteur psim */
    for (i = 1; i <= np; i++) {
      psim[i] = 0;
    }
    
    /* randomisation des locs dans le vecteur psim */
    for (i = 1; i <= ntot; i++) {
      j = (int) (np * alea());
      psim[j]++;
    }
    
    /* construction du vecteur de ponderation... */ 
    for (i = 1; i <= np; i++) {
      psim[i] = psim[i] / ((double) ntot);
    }
    
    /* ... et ENFA */
    enfa(Z, psim, &nv, &np, vp); 
    
    /* stockage dans res... */
    res[k] = vp[1];
    
    /* ... et fin de la boucle */
  }
  
  /* Et enfin, libération de la mémoire */
  freevec(psim);
  freevec(vp);
}


/* Le même, mais pour R */

void randenfar(double *Zr, double *pr, int *nvar, int *npix,
	       int *nrep, double *resr)
{
  /* Définition des variables locales */
  int i, j, k, nv, np, nr;
  double **Z, *p, *res;
  
  /* Allocation de mémoire */
  np = *npix;
  nv = *nvar;
  nr = *nrep;
  taballoc(&Z, np, nv);
  vecalloc(&p, np);
  vecalloc(&res, nr);
  
  /* remplissage des variables locales */
  k = 0;
  for (i=1; i <= np; i++) {
    for (j = 1; j <= nv; j++) {
      Z[i][j] = Zr[k];
      k++;
    }
  }
  
  for (i = 1; i <= np; i++) {
    p[i] = pr[i-1];
  }
  
  /* Fonction C */
  randenfa(Z, p, &nr, res); 
  
  /* Retour vers R */
  for (i = 1; i <= nr; i++) {
    resr[i-1] = res[i];
  }
  
  /* libération de la mémoire */
  freevec(p);
  freevec(res);
  freetab(Z);

}

