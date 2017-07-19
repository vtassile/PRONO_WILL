library(nlme)
library(lattice)
library(stats)
library(MASS)
library(Matrix)
library(splines)
library(car)
library(lme4)
library(nloptr)
library(MASS) 
library(lattice)
library(ggplot2)
library(plyr)
library(dplyr)
require(Hmisc)
library(xtable)


multi_random<-function (n = 1, mu, Sigma, tol = 1e-06, empirical = FALSE) 
{
  p <- length(mu)
  if (!all(dim(Sigma) == c(p, p))) 
    stop("incompatible arguments")
  eS <- eigen(Sigma, symmetric = TRUE, EISPACK = TRUE)
  ev <- eS$values
  if (!all(ev >= -tol * abs(ev[1L]))) 
    stop("'Sigma' is not positive definite")
  X <- matrix(rnorm(p * n), n)
  if (empirical) {
    X <- scale(X, TRUE, FALSE)
    X <- X %*% svd(X, nu = 0)$v
    X <- scale(X, FALSE, TRUE)
  }
  X <- drop(mu) + eS$vectors %*% diag(sqrt(pmax(ev, 0)), p) %*%t(X)
  nm <- names(mu)
  if (is.null(nm) && !is.null(dn <- dimnames(Sigma))) 
    nm <- dn[[1L]]
  dimnames(X) <- list(nm, NULL)
  if (n == 1) 
    drop(X)
  else t(X)
}

#### ACA OBTENEMOS LOS EFECTOS ALEATORIOS SIMULADOS POR NIVEL

OBTIENE_SIM<-function (MODELO_N,MODELO,NIVEL=1) {
if(MODELO_N=="AB_MOD21")
{   
 if(NIVEL==1)
 {  
   bb_1<-as.matrix(VarCorr(MODELO)[[5]])
   B_ALEA1<-rnorm (1,0,sqrt(bb_1))
   C_ALEA1<-0
   RETORNA<-c(B_ALEA1,C_ALEA1)
 }
 if(NIVEL==2)
 {
  aa_2<-as.numeric(VarCorr(MODELO)[[1]])
  bb_2<-as.numeric(VarCorr(MODELO)[[3]])
  cc_2<-as.numeric(VarCorr(MODELO)[[2]])
  A_ALEA2<-rnorm (1,0,sqrt(aa_2))
  B_ALEA2<-rnorm (1,0,sqrt(bb_2))
  C_ALEA2<-rnorm (1,0,sqrt(cc_2))
  RETORNA<-c(A_ALEA2,B_ALEA2,C_ALEA2)
 }
}  
  

if(MODELO_N=="AB_MOD24")
{   
  if(NIVEL==1)
  {  
    B_ALEA1<-0
    cc_1<-as.matrix(VarCorr(MODELO)[[6]])
    C_ALEA1<-rnorm (1,0,sqrt(cc_1))
    RETORNA<-c(B_ALEA1,C_ALEA1)    
  }
  if(NIVEL==2)
  {
    aa_2<-as.numeric(VarCorr(MODELO)[[1]])
    bb_2<-as.numeric(VarCorr(MODELO)[[3]])
    cc_2<-as.numeric(VarCorr(MODELO)[[2]])
    A_ALEA2<-rnorm (1,0,sqrt(aa_2))
    B_ALEA2<-rnorm (1,0,sqrt(bb_2))
    C_ALEA2<-rnorm (1,0,sqrt(cc_2))
    RETORNA<-c(A_ALEA2,B_ALEA2,C_ALEA2)
  }
}  

if(MODELO_N=="AB_MOD27")
{   
  if(NIVEL==1)
  {  
    B_ALEA1<-0
    C_ALEA1<-0
    RETORNA<-c(B_ALEA1,C_ALEA1) 
  }
  if(NIVEL==2)
  {
    aa_2<-as.numeric(VarCorr(MODELO)[[1]])
    bb_2<-as.numeric(VarCorr(MODELO)[[3]])
    cc_2<-as.numeric(VarCorr(MODELO)[[2]])
    A_ALEA2<-rnorm (1,0,sqrt(aa_2))
    B_ALEA2<-rnorm (1,0,sqrt(bb_2))
    C_ALEA2<-rnorm (1,0,sqrt(cc_2))
    RETORNA<-c(A_ALEA2,B_ALEA2,C_ALEA2)    
  }
} 

RETORNA
}



#### ACA OBTENEMOS LOS EFECTOS FIJOS
OBTIENE_FIJ<-function (MODELO_N,MODELO) {  
 
  if(MODELO_N=="AB_MOD21")
 {
  FIJOS<-fixef(MODELO)
  A<-FIJOS[1]+FIJOS[2] 
  C<-FIJOS[4]+FIJOS[5] 
  B<-FIJOS[3]   
 }
 if(MODELO_N=="AB_MOD24")
 {
  FIJOS<-fixef(MODELO)
  A<-FIJOS[1]+FIJOS[2] 
  B<-FIJOS[3]    
  C<-FIJOS[5]+FIJOS[6] 
  C<-C+FIJOS[4]*COVARI_C
 }  
 if(MODELO_N=="AB_MOD27")
 {
  FIJOS<-fixef(MODELO)
  A<-FIJOS[1]+FIJOS[2] 
  B<-FIJOS[3]+FIJOS[4]*COVARI_B 
  C<-FIJOS[6]+FIJOS[7] 
  C<-C+FIJOS[5]*COVARI_C
 }
 RETORNA<-c(A,B,C)
 RETORNA
}

MODELO, NOMBRE_FIJOS, DATOS, MODELO_N,TIPO

SIMU_CORR <-function (MODELO_N,SITIO2,NRO_SITIOS=1,NRO_FRUTOS=30,DDF_COS=106,MAX_N1=18,MAX_N2=10) {  
# DEFINICION DE LAS VARIABLES DE LA SIMULACION
  #NRO_SITIOS<-1
  #NRO_FRUTOS<-30
  #DDF_COS<-120
  #MAX_N1<-18
  #MAX_N2<-10
#MODELO_N<-"AB_MOD27"
  #MODELO<-AB_MOD27
  #COVARI_B<-78.1
  #COVARI_C<-55.35

PASA<-0
FIJOS<-OBTIENE_FIJ(MODELO, NOMBRE_FIJOS,DATOS,MODELO_N,TIPO)
NRO_FRUTO<-1
SIGMA<-summary(MODELO)$sigma
####################################
### GENERAMOS EL ARCHIVO ###########
####################################
for(aa in 1:NRO_SITIOS)
 {
  ALEA_N1<-OBTIENE_SIM(MODELO_N,MODELO,1)
  for(bb in 1:NRO_FRUTOS)
  {
    DDF_1<-30
    DDF_2<-35
    ALEA_N2<-OBTIENE_SIM(MODELO_N,MODELO,2)   
    for(cc in 1:MAX_N1)
    {
     DDF_2<-DDF_1+5      
     for(dd in 1:MAX_N2)
     {
      A_FOR<-FIJOS[1]+ALEA_N2[1]
      B_FOR<-FIJOS[2]+ALEA_N1[1]+ALEA_N2[2]
      C_FOR<-FIJOS[3]+ALEA_N1[2]+ALEA_N2[3]  

      A_FIJ<-FIJOS[1]
      B_FIJ<-FIJOS[2]+ALEA_N1[1]
      C_FIJ<-FIJOS[3]+ALEA_N1[2]
      
      ERROR1<-rnorm(1,0,SIGMA)      
      ERROR2<-rnorm (1,0,SIGMA) 
      DIAM_1<-(1/((A_FOR)*0.01+((B_FOR)*0.01)*(((C_FOR)*0.1)**(DDF_1))))+ERROR1
      DIAM_2<-(1/((A_FOR)*0.01+((B_FOR)*0.01)*(((C_FOR)*0.1)**(DDF_2))))+ERROR2
      DIAM_F1<-(1/((A_FIJ)*0.01+((B_FIJ)*0.01)*(((C_FIJ)*0.1)**(DDF_1))))
      DIAM_F2<-(1/((A_FIJ)*0.01+((B_FIJ)*0.01)*(((C_FIJ)*0.1)**(DDF_2))))
      ERR_1<-DIAM_1-DIAM_F1
      ERR_2<-DIAM_2-DIAM_F2    
      PARCIAL<-data.frame(SITIO=NRO_SITIOS,FRUTO1=NRO_FRUTO,FRUTO2=bb,
                          DDF_1=DDF_1,DDF_2=DDF_2,DIAM_1=DIAM_1,DIAM_2=DIAM_2,
                          ERR_1=ERR_1,ERR_2=ERR_2,ERROR1=ERROR1,ERROR2=ERROR2)
      if(PASA==0)
      {
       TOTAL<-PARCIAL 
      } 
      else
      {
      l <- list(TOTAL, PARCIAL)
      TOTAL<-do.call(rbind.fill, l)                       
     }
     PASA<-1
     DDF_2<-DDF_2+5
    }
    DDF_1<-DDF_1+5
   }  
   NRO_FRUTO<-NRO_FRUTO+1      
  }
 }
  
}


########################################
## CALCULAMOS LA CORRELACION PARA   ####
## CADA COMBINACION DE DDF_1 Y DDF_2 ###
########################################
               
TOTAL$LAG<-TOTAL$DDF_2-TOTAL$DDF_1
TOTAL2 <- tbl_df(TOTAL)
TOTAL2 <- group_by(TOTAL2,DDF_1,LAG)
COR_MARG<- summarise(TOTAL2,CORR =cor(ERR_1,ERR_2))

scatterplot3d(COR_MARG$DDF_1,COR_MARG$LAG, COR_MARG$CORR, highlight.3d = TRUE,
              type = "h", col.grid = "lightblue",
                     angle = 65, scale.y = 0.7, pch = 20, main = MODELO_N,
              xlab="DDF",ylab="LAG",zlab="Correlación")


