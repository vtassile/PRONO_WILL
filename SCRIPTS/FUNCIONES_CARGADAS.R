# funcion OBTIENE archivo: base de trabajo, variable: variable de
# respuesta,agrupa: variable de clasificacion operacion: operaci?n a
# realizar t_base: variable sobre la cual se tiene el criterio
# base(DIASAGO) base: criterio base a partir del cual se considera la
# variable de respuesta t_tope: variable sobre la cual se tiene el
# criterio tope(DDF) tope: criterio tope para considerar la variable de
# respuesta NOMBRE: Nombre de la variable a grabar lim.i: limite
# inferior para la obtencion de datos lim.s: limite superior para la
# obtencion de datos
# PROBAR CAMBIAR DDF POR DIASAGO EN LA FUNCION Obtiene Y VER PORQUE NO FUNCIONA
Obtiene <- function(archivo, variable, agrupa, operacion, t_base, base, 
    t_tope, tope, NOMBRE, lim.i = 0, lim.s = 99) {
    TEMPORAL <- cbind(archivo, RESP = 0)
    delta1 <- archivo[, variable] - lim.i
    archivo[, "RESP"] <- ifelse(delta1 > 0, delta1, 0)
    if (lim.s != 99) {
        rango <- lim.s - lim.i
        archivo[, "RESP"] <- ifelse(delta1 < rango, delta1, rango)
    }
    ELIGE1 <- subset(archivo, archivo[, t_base] >= base)
    ELIGE2 <- subset(ELIGE1, ELIGE1[, t_tope] <= tope)
    TATA <- aggregate(ELIGE2[, "RESP"] ~ ELIGE2[, agrupa], ELIGE2, FUN = operacion)
    colnames(TATA) <- c("AGRUPA", NOMBRE)
    RETORNA <- TATA
}

# funcion covariables archivo_agrupa: archivo con la configuracion de
# la conformacion de las covariables archivo: base de trabajo,
# variable: variable de respuesta,agrupa: variable de clasificacion
# variable: variable sobre la que se operan los procedimientos agrupa:
# criterio de agrupamiento
covariables <- function(archivo_agrupa, archivo, variable, agrupa) {
    c.max <- dim(archivo_agrupa)[1]
    maestro <- data.frame(levels(archivo$ANO))
    colnames(maestro) <- c("AGRUPA")
    mezcla <- maestro
    for (i in 1:c.max) {
        c.operacion <- as.character(archivo_agrupa[i, "operacion"])
        c.t_base <- as.character(archivo_agrupa[i, "t_base"])
        c.base <- archivo_agrupa[i, "base"]
        c.t_tope <- as.character(archivo_agrupa[i, "t_tope"])
        c.tope <- archivo_agrupa[i, "tope"]
        c.NOMBRE <- as.character(archivo_agrupa[i, "NOMBRE"])
        c.lim.i <- archivo_agrupa[i, "lim_i"]
        c.lim.s <- archivo_agrupa[i, "lim_s"]
        tempo <- Obtiene(archivo, as.character(variable), as.character(agrupa), 
            c.operacion, c.t_base, c.base, c.t_tope, c.tope, c.NOMBRE, 
            c.lim.i, c.lim.s)
        mezcla <- merge(mezcla, tempo, by.x = c("AGRUPA"), by.y = c("AGRUPA"), 
            all.x = TRUE, all.y = TRUE)
    }
    colnames(mezcla)[1] <- c(as.character(agrupa))
    devuelve <- mezcla
}

SUMA <- function(x) {
    RETORNA <- sum(x)
    RETORNA
}

# PREDICCION DE VARIABLES DE RESPUESTA NUEVAS #######
# ES NECESARIO AJUSTAR LA VARIABLE DEPENDIENTE EN ####
## CAR1- POR AHORA SOLO RECONOCE A DDF  #############
PREDICHO<-function(MODELO,EXPRESION,PP_FINAL,VAR_DEP_CORR="DDF",
                   METODO="BLUP",DDF_PRED,EXTIENDE=0,DERIVADAS)
{
  OBT_Z<-function(DATA,DERIVADAS) {
    ANCHO<-length(DERIVADAS)
    LARGO<-dim(DATA)[1]
    MARCO<-matrix(0,LARGO,ANCHO)
    for(i in 1:ANCHO) {
      EXPRESION<-DERIVADAS[[i]]
      MARCO[,i]<-eval(EXPRESION,DATA)
    }
    MARCO
  }
  
  OBT_B<-function(DATA,DERIVADAS) {
    ANCHO<-length(DERIVADAS)
    LARGO<-dim(DATA)[1]
    MARCO<-matrix(0,LARGO,ANCHO)
    for(i in 1:ANCHO) {
      NOMBRE<-names(DERIVADAS)[i]
      MARCO[,i]<-DATA[,NOMBRE]
    }
    MARCO
  }
  
  OBT_COR<-function(MODELO,DIF_DDF){
    APLICA<-rep(0,length(DIF_DDF))
    if(class(MODELO)[1]=="nlme"){
      T_CORR<-class(summary(MODELO)$modelStruct$corStruct)[1]
      if (T_CORR=="corAR1") {
        CORR_P<-MODELO$modelStruct$corStruct[1]
        CORR<-exp(CORR_P)
        CORR <- c((CORR - 1)/(CORR + 1))
        COEF_CORR<-CORR
        APLICA<-COEF_CORR^(DIF_DDF/7)
      }      
      # AHORA HAY QUE CONSIDERAR UNA FUNCION QUE PERMITA OBTENER UNA FUNCION   
      if (T_CORR=="corCAR1") {
        CORR_P<-MODELO$modelStruct$corStruct[1]
        CORR<-exp(CORR_P)
        CORR <- c((CORR )/(CORR + 1))
        COEF_CORR<-CORR
        CORR <- corCAR1(COEF_CORR)  
        #      APLICA<-COEF_CORR^(log(DIF_DDF))   
        APLICA<-COEF_CORR^(DIF_DDF) 
      }
    }
    APLICA  
  }    
  
  # AQUI COMIENZA LA FUNCION PREDICHO
  if(class(MODELO)[1]=="nlme"){
    VAR_DEP<-formula(MODELO)[[2]]
  }
  else
  {  
    VAR_DEP<-as.character(formula(MODELO)[[2]][[2]])
  }
  
  cat("Macro Predicho Ver 2.0.") 
  cat("Método:....",METODO,"\n") 
  # EXTRAEMOS LOS BLUPS CALIBRADOS
  BB<-OBT_B(PP_FINAL,DERIVADAS)     
  # SI EL METODO ES FO
  if (METODO=="ZERO") {
    for(xc in 1:length(DERIVADAS)) PP_FINAL[,names(DERIVADAS)[xc]]<-0
  }
  D_PREV<-PP_FINAL
  D_POST<-PP_FINAL
  if (DDF_PRED==0) D_POST$DDF<-D_POST$DDF_COS
  if (DDF_PRED>0) D_POST$DDF<-DDF_PRED
  
  D_PREV$Pred_1<-eval(EXPRESION,D_PREV)     
  D_PREV$Pred_2<-eval(EXPRESION,D_POST)         
  DIF<-D_PREV[,as.character(VAR_DEP)]-D_PREV[,"Pred_2"]
  Z1<-OBT_Z(D_PREV,DERIVADAS) 
  Z2<-OBT_Z(D_POST,DERIVADAS)   
  # SE ARMA COVA_R PARA QUE OBTENGA LA VARIABLE DEPENDIENTE TRANSFORMADA
  if(class(MODELO)[1]=="nlme")
  {
    PREVIO<-D_PREV$DDF
    POSTERIOR<-D_POST$DDF
    DIF_DDF<-POSTERIOR-PREVIO
    BCORR<-OBT_COR(MODELO,DIF_DDF)    
  }
  else
  {
    BCORR<-rep(0,dim(D_POST)[1])  
  }  
  TEMPO1<-rowSums(Z1*BB)
  TEMPO2<-rowSums(Z2*BB)
  if (BCORR[1]==0)
  { 
    if (METODO=="ZERO") { D_PREV$Pred_f<-D_PREV[,"Pred_2"]+TEMPO2 }  
    if (METODO=="BLUP"){  D_PREV$Pred_f<-D_PREV[,"Pred_2"]  }
  }
  if (BCORR[1]>0)
  {
    if (METODO=="ZERO"){ D_PREV$Pred_f<-D_PREV[,"Pred_2"]+BCORR*(DIF) }
    if (METODO=="BLUP"){ D_PREV$Pred_f<-D_PREV[,"Pred_2"]-TEMPO2+BCORR*(DIF+TEMPO1) }
  }  
  D_PREV$METODO2<-METODO 
  if (DDF_PRED==0) 
  {
    if(EXTIENDE==0)
    { 
      D_PREV$ER<-D_PREV[,"DIAM_COS"]-D_PREV[,"Pred_f"]
      D_PREV$ER_ABS<-abs(D_PREV[,"DIAM_COS"]-D_PREV[,"Pred_f"])
      D_PREV$ER_REL<-D_PREV$ER/D_PREV[,"DIAM_COS"]
      D_PREV$ER_REL_ABS<-D_PREV$ER_ABS/D_PREV[,"DIAM_COS"]
    }  
  }      
  D_PREV
}

##### ESTIMA Z    ################################
OBTIENE_Z<-function(VALIDA,LISTA,DERIVADAS,ALEATORIOS,NRO,MODELO,NOMBRE_FIJOS,MODELO_N,TIPO) {
  NIVELES=length(LISTA)
  # OBTIENE EL BLOQUE DEL PRIMER NIVEL
  NOMBRE_1<-as.character(names(LISTA)[1])
  ### ANCHO DEL BLOQUE
  ANCHO_1<-dim(LISTA[[1]])[1]
  V_N1<-unique(VALIDA[,NOMBRE_1])
  TAMIZ1<-VALIDA[VALIDA[,NOMBRE_1]==V_N1[NRO],]
  ### LONGITUD DEL BLOQUE
  LARGO_1<-dim(TAMIZ1)[1]
  ### CONSTRUYE EL PRIMER BLOQUE
  BLOQUE_1<-matrix(0,LARGO_1,ANCHO_1)
  CORRE<-1
  for(i in 1:ANCHO_1) {
    for(ii in 1:LARGO_1) {
      DATOS<-TAMIZ1[ii,]
      BLOQUE_1[ii,i]<-Z_2(DERIVADAS,CORRE,DATOS,MODELO,LISTA,
                          NOMBRE_FIJOS,ALEATORIOS,NRO,MODELO_N,TIPO,VALIDA)
      
    }
    CORRE<-CORRE+1
  }  
  RETORNA<-BLOQUE_1
  if(NIVELES>1) {
    # NIVEL 2 DE EFECTOS ALEATORIOS 
    NOMBRE_2<-as.character(names(LISTA)[2])
    ANCHO_2<-dim(LISTA[[2]])[1]
    V_N2<-unique(TAMIZ1[,NOMBRE_2])
    N2<-length(V_N2)   
    ### CONSTRUYE EL SEGUNDO BLOQUE
    for(i in 1:N2) {
      CORRE<-ANCHO_1+1
      TAMIZ2<-TAMIZ1[TAMIZ1[,NOMBRE_2]==V_N2[i],]
      ### LONGITUD DEL BLOQUE
      LARGO_2<-dim(TAMIZ2)[1]
      SUB_BLOQUE_2<-matrix(0,LARGO_2,ANCHO_2)
      for(ii in 1:ANCHO_2) {
        for(iii in 1:LARGO_2) {
          DATOS<-TAMIZ2[iii,]
          SUB_BLOQUE_2[iii,ii]<-Z_2(DERIVADAS,CORRE,DATOS,MODELO,LISTA,
                                    NOMBRE_FIJOS,ALEATORIOS,NRO,MODELO_N,TIPO,VALIDA)
        }
        CORRE<-CORRE+1
      }   
      if(i==1) BLOQUE_2<-SUB_BLOQUE_2
      else BLOQUE_2<-as.matrix(bdiag(BLOQUE_2,SUB_BLOQUE_2))
    }        
    RETORNA<-as.matrix(cbind(BLOQUE_1,BLOQUE_2))
    if(NIVELES>2) {
      # NIVEL 3 DE EFECTOS ALEATORIOS 
      NOMBRE_3<-as.character(names(LISTA)[3])
      ANCHO_3<-dim(LISTA[[3]])[1]
      ### CONSTRUYE EL TERCER BLOQUE
      for(i in 1:N2) {
        NOMBRE_3<-as.character(names(LISTA)[3])
        ANCHO_3<-dim(LISTA[[3]])[1]
        TAMIZ2<-TAMIZ1[TAMIZ1[,NOMBRE_2]==V_N2[i],]
        V_N3<-unique(TAMIZ2[,NOMBRE_3])
        N3<-length(V_N3)   
        for(v in 1:N3) {  
          TAMIZ3<-TAMIZ2[TAMIZ2[,NOMBRE_3]==V_N3[v],]
          ### LONGITUD DEL BLOQUE
          LARGO_3<-dim(TAMIZ3)[1]
          S_SUB_BLOQUE_3<-matrix(0,LARGO_3,ANCHO_3)
          CORRE<-ANCHO_1+ANCHO_2+1        
          for(ii in 1:ANCHO_3) {
            for(iii in 1:LARGO_3) {
              DATOS<-TAMIZ3[iii,]
              S_SUB_BLOQUE_3[iii,ii]<-Z_2(DERIVADAS,CORRE,DATOS,MODELO,LISTA,
                                          NOMBRE_FIJOS,ALEATORIOS,NRO,MODELO_N,TIPO,VALIDA)
            }
            CORRE<-CORRE+1
          }   
          if(v==1) S_BLOQUE_3<-S_SUB_BLOQUE_3
          else S_BLOQUE_3<-as.matrix(bdiag(S_BLOQUE_3,S_SUB_BLOQUE_3))
        }   
        if(i==1) BLOQUE_3<-S_BLOQUE_3
        else BLOQUE_3<-as.matrix(bdiag(BLOQUE_3,S_BLOQUE_3))
      }  
      RETORNA<-as.matrix(cbind(BLOQUE_1,BLOQUE_2,BLOQUE_3))
    }      
  } 
  RETORNA              
}        

Z_2<-function(DERIVADAS,EVALUA,DATOS,MODELO,LISTA,NOMBRE_FIJOS,ALEATORIOS,NRO,MODELO_N,TIPO,VALIDA)
{
  M_FIJOS<-OBTIENE_FIJOS(MODELO,NOMBRE_FIJOS,DATOS,MODELO_N,TIPO)      
  EXPRESION<-DERIVADAS[[EVALUA]]
  UU_LISTOS<-OBTIENE_UU(VALIDA,DERIVADAS,DATOS,LISTA,ALEATORIOS,NRO)  
  d <- data.frame((cbind(DATOS,M_FIJOS,UU_LISTOS)))
  Z2<-eval(EXPRESION,d)
  Z2
}

AJUSTADO<-function(VALIDA,LISTA,EXPRESION,DERIVADAS,ALEATORIOS,NRO,MODELO,NOMBRE_FIJOS,MODELO_N,TIPO)
{
  NOM_UU<-names(DERIVADAS)
  ANCHO_UU<-length(DERIVADAS)
  NOMBRE_1<-as.character(names(LISTA)[1])
  V_N1<-unique(VALIDA[,NOMBRE_1])
  TAMIZ1<-VALIDA[VALIDA[,NOMBRE_1]==V_N1[NRO],]
  LARGO_UU<-dim(TAMIZ1)[1]
  UU_LISTOS<-matrix(0,LARGO_UU,ANCHO_UU)
  M_FIJOS<-matrix(0,LARGO_UU,length(NOMBRE_FIJOS))
  colnames(UU_LISTOS)<-NOM_UU
  # CREA UN VECTOR LLAMADO UU_LISTOS 
  for(i in 1:LARGO_UU) {
    # ESTO SOLO FUNCIONA CON UNO O DOS NIVELES  
    M_FIJOS[i,]<-as.numeric(OBTIENE_FIJOS(MODELO,NOMBRE_FIJOS,TAMIZ1[i,],MODELO_N,TIPO))          
    UU_LISTOS[i,]<-OBTIENE_UU(VALIDA,DERIVADAS,TAMIZ1[i,],LISTA,ALEATORIOS,NRO)  
  }
  M_FIJOS<-as.data.frame(M_FIJOS)
  names(M_FIJOS)<-NOMBRE_FIJOS
  d <- data.frame((cbind(TAMIZ1,M_FIJOS,UU_LISTOS)))
  d$Pred<-eval(EXPRESION,d)
  d  
}

OBTIENE_UU<-function(VALIDA,DERIVADAS,DATOS,LISTA,ALEATORIOS,NRO,TOTAL=FALSE){  
  NIVELES=length(LISTA)
  ### DEFINE VECTOR DE EFECTOS ALEATORIOS
  NOM_UU<-names(DERIVADAS)  
  LARGO_UU<-length(DERIVADAS)
  UU_LIST<-matrix(0,1,LARGO_UU)
  colnames(UU_LIST)<-NOM_UU
  ##### DEFINICIONES DEL NIVEL 1
  NOMBRE_1<-as.character(names(LISTA)[1])
  ANCHO_1<-dim(LISTA[[1]])[1]
  V_N1<-unique(VALIDA[,NOMBRE_1])
  TAMIZ1<-VALIDA[VALIDA[,NOMBRE_1]==V_N1[NRO],]
  #### ASIGNA LOS EFECTOS ALEATORIOS DEL NIVEL 1 AL VECTOR
  for(i in 1:ANCHO_1) {
    UU_LIST[i]<-ALEATORIOS[i]
  }
  if(NIVELES>1) {
    ##### DEFINICIONES DEL NIVEL 2
    NOMBRE_2<-as.character(names(LISTA)[2])
    ANCHO_2<-dim(LISTA[[2]])[1]
    V_N2<-as.matrix(unique(TAMIZ1[,NOMBRE_2]))
    VALOR_N2<-as.character(DATOS[,NOMBRE_2])
    N2<-length(unique(TAMIZ1[,NOMBRE_2]))
    ###### BUSCA LA POSICION DEL EFECTO ALEATORIO ACTUAL EN LOS PRESENTES EN ESE NIVEL 
    POS_N2<-which(V_N2==VALOR_N2)
    for(i in 1:ANCHO_2) {
      POS_LIST<-ANCHO_1+i
      POS_ALEA<-ANCHO_1+ANCHO_2*(POS_N2-1)+i  
      UU_LIST[POS_LIST]<-ALEATORIOS[POS_ALEA]
    }
    if(NIVELES>2) {  
      ##### DEFINICIONES DEL NIVEL 3 
      NOMBRE_3<-as.character(names(LISTA)[3])
      ANCHO_3<-dim(LISTA[[3]])[1]    
      #  FRUTOS EN EL TAMAÑO
      N3<-0
      for(i in 1:N2) {
        TAMIZ2<-TAMIZ1[TAMIZ1[,NOMBRE_2]==V_N2[i],]
        #  FRUTOS EN EL TAMAÑO
        V_N3<-as.matrix(unique(TAMIZ2[,NOMBRE_3]))   
        N3_SUB<-length(V_N3)
        VALOR_N3<-as.character(DATOS[,NOMBRE_3])
        POS_N3<-match(VALOR_N3,V_N3)
        ifelse(is.na(POS_N3),N3<-N3+N3_SUB,break)
      }
      N3<-N3+POS_N3
      for(i in 1:ANCHO_3) {
        POS_LIST<-ANCHO_1+ANCHO_2+i
        POS_ALEA<-ANCHO_1+ANCHO_2+ANCHO_3*(N3-1) +i  
        UU_LIST[POS_LIST]<-ALEATORIOS[POS_ALEA]
      }
    }  
  }   
  UU_LIST        
}

### ESTIMA D  ######
OBTIENE_D<-function(LISTA,VALIDA,NRO) {
  NIVELES=length(LISTA)
  # OBTIENE EL BLOQUE DEL PRIMER NIVEL
  BLOQUE_1<-LISTA[[1]]
  NOMBRE_1<-as.character(names(LISTA)[1])
  ANCHO_1<-dim(LISTA[[1]])[1]
  LONGITUD<-ANCHO_1
  RETORNA<-BLOQUE_1
  if(NIVELES>1) {
    # NIVEL 2 DE EFECTOS ALEATORIOS 
    NOMBRE_2<-as.character(names(LISTA)[2])
    ANCHO_2<-dim(LISTA[[2]])[1]
    V_N1<-unique(VALIDA[,NOMBRE_1])
    TAMIZ1<-VALIDA[VALIDA[,NOMBRE_1]==V_N1[NRO],]
    V_N2<-unique(TAMIZ1[,NOMBRE_2])
    N2<-length(V_N2)   
    LONGITUD<-ANCHO_1+ANCHO_2*N2
    BLOQUE_2<-REPLICA_M(LISTA[[2]],N2)
    RETORNA<-as.matrix(bdiag(BLOQUE_1,BLOQUE_2))
    if(NIVELES>2) {
      # NIVEL 3 DE EFECTOS ALEATORIOS 
      NOMBRE_3<-as.character(names(LISTA)[3])
      ANCHO_3<-dim(LISTA[[3]])[1]    
      N3<-0
      for(i in 1:N2) {
        TAMIZ2<-TAMIZ1[TAMIZ1[,NOMBRE_2]==V_N2[i],]
        #  FRUTOS EN EL TAMAÑO
        V_N3<-unique(TAMIZ2[,NOMBRE_3])
        N3_SUB<-length(V_N3)
        N3<-N3+N3_SUB
      }
      LONGITUD<-ANCHO_1+ANCHO_2*N2+ANCHO_3*N3 
      BLOQUE_3<-REPLICA_M(LISTA[[3]],N3)
      RETORNA<-as.matrix(bdiag(BLOQUE_1,BLOQUE_2,BLOQUE_3))
    }   
  }
  RETORNA
}  

### REPLICA MATRICES DIAGONIZANDOLES
REPLICA_M<-function(MATRIZ,n) {
  ES<-MATRIZ
  if(n>1) {
    LIMITE<-n-1
    for(i in 1:LIMITE) {
      ES<-as.matrix(bdiag(MATRIZ,ES))
    }
  }
  ES
}

#### FUNCION QUE CALCULA LA LONGITUD DEL VECTOR DE BLUPS ESTIMADOS
LARGO_UU<-function(VALIDA,LISTA,NRO) {
  NIVELES=length(LISTA)
  # OBTIENE EL BLOQUE DEL PRIMER NIVEL
  NOMBRE_1<-as.character(names(LISTA)[1])
  ANCHO_1<-dim(LISTA[[1]])[1]
  V_N1<-unique(VALIDA[,NOMBRE_1])
  TAMIZ1<-VALIDA[VALIDA[,NOMBRE_1]==V_N1[NRO],]
  LONGITUD<-ANCHO_1
  if(NIVELES>1) {
    # NIVEL 2 DE EFECTOS ALEATORIOS 
    NOMBRE_2<-as.character(names(LISTA)[2])
    ANCHO_2<-dim(LISTA[[2]])[1]
    V_N2<-unique(TAMIZ1[,NOMBRE_2])
    N2<-length(V_N2)   
    LONGITUD<-ANCHO_1+ANCHO_2*N2
    if(NIVELES>2) {
      # NIVEL 3 DE EFECTOS ALEATORIOS 
      NOMBRE_3<-as.character(names(LISTA)[3])
      ANCHO_3<-dim(LISTA[[3]])[1]    
      N3<-0
      for(i in 1:N2) {
        TAMIZ2<-TAMIZ1[TAMIZ1[,NOMBRE_2]==V_N2[i],]
        #  FRUTOS EN EL TAMAÑO
        V_N3<-unique(TAMIZ2[,NOMBRE_3])
        N3_SUB<-length(V_N3)
        N3<-N3+N3_SUB
      }
      LONGITUD<-ANCHO_1+ANCHO_2*N2+ANCHO_3*N3 
    }   
  }
  LONGITUD
}

OBTIENE_R<-function(DATOS,LISTA,MODELO,PP,VAR_DEP_CORR="DDF",NRO) {
  NIVELES=length(LISTA)
  # VER SI EXISTE ESTRUCTURA DE: CORRELACION - DETECTAR EL COEF Y COVARIABLE
  #  / VARIANZAS CON VARIDENT - DETECTAR LA COVARIABLE
  #  / VARIANZAS CON FUNCION - DETECTAR EL COEF Y LA COVARIABLE
  # OBTIENE EL PRIMER NIVEL
  NOMBRE_1<-as.character(names(LISTA)[1])
  V_N1<-unique(PP[,NOMBRE_1])
  TAMIZ1<-PP[PP[,NOMBRE_1]==V_N1[NRO],]
  TAMAÑO_1<-dim(TAMIZ1)[1]
  if(NIVELES==1) {
    VAR_POND<-OBTIENE_TVAR(MODELO,TAMIZ1)
    VAR_CORR<-OBTIENE_CORR(MODELO,TAMIZ1,VAR_DEP_CORR)
    RR<-VAR_POND%*%VAR_CORR%*%VAR_POND
  }
  if(NIVELES==2) {
    # NIVEL 2 DE EFECTOS ALEATORIOS 
    NOMBRE_2<-as.character(names(LISTA)[2])
    ANCHO_2<-dim(LISTA[[2]])[1]
    V_N2<-unique(TAMIZ1[,NOMBRE_2])
    N2<-length(V_N2)   
    for(v in 1:N2) {
      TAMIZ2<-TAMIZ1[TAMIZ1[,NOMBRE_2]==V_N2[v],]
      # ESTE MATRIZ TIENE ELEMENTOS EN LA DIAGONAL
      # SE OBTIENEN LAS PONDERACIONES POR GRUPOS Y  
      # Y POR FUNCIONES DE POTENCIA Y EXPONENTE
      VAR_POND<-OBTIENE_TVAR(MODELO,TAMIZ2)
      VAR_CORR<-OBTIENE_CORR(MODELO,TAMIZ2,VAR_DEP_CORR)
      S_RR<-VAR_POND%*%VAR_CORR%*%VAR_POND
      if(v==1) RR<-S_RR
      else RR<-as.matrix(bdiag(RR,S_RR))
    }  
  }        
  if(NIVELES==3) {
    # NIVEL 3 DE EFECTOS ALEATORIOS 
    NOMBRE_2<-as.character(names(LISTA)[2])
    ANCHO_2<-dim(LISTA[[2]])[1]
    NOMBRE_3<-as.character(names(LISTA)[3])
    ANCHO_3<-dim(LISTA[[3]])[1]
    V_N2<-unique(TAMIZ1[,NOMBRE_2])
    N2<-length(V_N2)   
    for(v in 1:N2) {
      TAMIZ2<-TAMIZ1[TAMIZ1[,NOMBRE_2]==V_N2[v],]
      V_N3<-unique(TAMIZ2[,NOMBRE_3])
      N3<-length(V_N3) 
      for(w in 1:N3) {
        TAMIZ3<-TAMIZ2[TAMIZ2[,NOMBRE_3]==V_N3[w],]
        VAR_POND<-OBTIENE_TVAR(MODELO,TAMIZ2)
        VAR_CORR<-OBTIENE_CORR(MODELO,TAMIZ2,VAR_DEP_CORR)
        S_RR<-VAR_POND%*%VAR_CORR%*%VAR_POND
        if(w==1) S_RR<-S_S_RR
        else S_RR<-as.matrix(bdiag(S_RR,S_S_RR))
      }
      if(v==1) RR<-S_RR
      else RR<-as.matrix(bdiag(RR,S_RR))  
    }       
  }
  SIGMA<-summary(MODELO)$sigma      
  RR<-SIGMA*SIGMA*RR
  RR
} 

OBTIENE_TVAR<-function(MODELO,DATA){
  # esta es la forma de leer el tipo de estructura de varianza y correlacion del error
  RETORNA<-rep(1,dim(DATA)[1])
  if(class(MODELO)[1]=="nlme"){
    T_VAR<-class(summary(MODELO)$modelStruct$varStruct)[1]
    if(T_VAR=="varIdent")
    {   # con esto encuentro la covariable para varId 
      TEMPO1<-summary(MODELO)$modelStruct$varStruct
      (TEMPO2<-attr(TEMPO1,"formula"))
      COVA<-as.character(TEMPO2[[2]])[3]   
      GRUPOS<-attr(TEMPO1,"groupNames") 
      MARCO_VAR<-matrix(0,length(GRUPOS),2)
      MARCO_VAR<-as.data.frame(MARCO_VAR)   
      MARCO_VAR[1,2]<-1
      MARCO_VAR[1,1]<-GRUPOS[1]
      for(ww in 2:length(GRUPOS)) {
        MARCO_VAR[ww,1]<-GRUPOS[ww]
        MARCO_VAR[ww,2]<-exp(TEMPO1[ww-1])
      }
      DATA<-as.data.frame(DATA)
      RETORNA<-merge(DATA, MARCO_VAR, by.x = COVA, by.y = "V1", sort=F,all.x=T,all.y=F)[,"V2"]      
    }
    if(T_VAR=="varPower")
    {     # con esto encuentro la covariable para varPower
      TEMPO1<-summary(MODELO)$modelStruct$varStruct
      TEMPO2<-attr(TEMPO1,"formula")
      COVA<-as.character(TEMPO2[[2]])[1]
      COVA2<-ifelse(COVA=="fitted","Pred",COVA)
      COEF<-as.numeric(aa$varStruct)
      RETORNA<-(DATA[,COVA2]^COEF)  
    }  
    if(T_VAR=="varExp")
    {     # con esto encuentro la covariable para varPower
      TEMPO1<-summary(MODELO)$modelStruct$varStruct
      TEMPO2<-attr(TEMPO1,"formula")
      COVA<-as.character(TEMPO2[[2]])[1]
      COVA2<-ifelse(COVA=="fitted","Pred",COVA)
      COEF<-as.numeric(aa$varStruct)
      RETORNA<-exp(DATA[,COVA2]*COEF)  
    }  
  }
  RETORNA2<-diag(length(RETORNA))
  diag(RETORNA2)<-RETORNA
  RETORNA2
}

OBTIENE_CORR<-function(MODELO,DATA,VAR_DEP_CORR){
  RETORNA<-rep(1,dim(DATA)[1])    
  RETORNA2<-diag(length(RETORNA))
  diag(RETORNA2)<-RETORNA
  if(class(MODELO)[1]=="nlme"){
    T_CORR<-class(summary(MODELO)$modelStruct$corStruct)[1]
    # OBTIENE LA CORRELACION SI EXISTE Y CONSTRUYE LA MATRIZ
    if (T_CORR=="corAR1") {
      CORR_P<-MODELO$modelStruct$corStruct[1]
      CORR<-exp(CORR_P)
      CORR <- c((CORR - 1)/(CORR + 1))
      COEF_CORR<-CORR
      CORR <- corAR1(COEF_CORR)    
      #M_CORR<-corMatrix(CORR, covariate = 1:4)  
      # ARMO LA MATRIZ C DE LA CORRELACION
      LARGO<-length(DATA[,VAR_DEP_CORR])
      VECTOR_1<- DATA[1,VAR_DEP_CORR]
      UNOS<-rep(1,LARGO)
      CONSTANTE<-UNOS%*%t(VECTOR_1)
      VECTOR_2<- ((DATA[,VAR_DEP_CORR]-CONSTANTE)/7)+1
      MARCO_CORR<-matrix(0,LARGO,LARGO)
      for(i in 1:LARGO) {
        for(ii in 1:LARGO) {
          MARCO_CORR[i,ii]<-COEF_CORR^(abs(VECTOR_2[i]-VECTOR_2[ii])) 
        }
      }
      RETORNA2<-MARCO_CORR  
    }
    if (T_CORR=="corCAR1") {
      CORR_P<-MODELO$modelStruct$corStruct[1]
      CORR<-exp(CORR_P)
      CORR <- c((CORR )/(CORR + 1))
      COEF_CORR<-CORR
      CORR <- corCAR1(COEF_CORR)    
      # ARMO LA MATRIZ C DE LA CORRELACION
      LARGO<-length(DATA[,VAR_DEP_CORR])
      VECTOR_1<- DATA[1,VAR_DEP_CORR]
      UNOS<-rep(1,LARGO)
      CONSTANTE<-UNOS%*%t(VECTOR_1)
      VECTOR_2<- ((DATA[,VAR_DEP_CORR]-CONSTANTE))+1
      MARCO_CORR<-matrix(0,LARGO,LARGO)
      # SE ARMA COVA_R PARA QUE OBTENGA LA VARIABLE DEPENDIENTE TRANSFORMADA
      attach(DATA)    
      TEMPO1<-MODELO$modelStruct$corStruct
      TEMPO2<-attr(TEMPO1,"formula")
      TEMPO3<-TEMPO2[[2]][2]
      TEMPO4<-languageEl(TEMPO3, 1)
      COVA_R<-eval(TEMPO4)
      detach(DATA)      
      for(i in 1:LARGO) {
        for(ii in 1:LARGO) {
          MARCO_CORR[i,ii]<-COEF_CORR^(abs(COVA_R[i]-COVA_R[ii])) 
        }
      }
      RETORNA2<-MARCO_CORR  
    }
  }  
  RETORNA2
}

OBTIENE_FIJOS <- function(MODELO, NOMBRE_FIJOS, DATOS, MODELO_N,TIPO) {
    TIPO_M <- 0
    if (MODELO_N == "AB_MOD17") { TIPO_M <- 1     }
    if (MODELO_N == "AB_MOD21") { TIPO_M <- 1     }
    #### COVA CON TEMP TEMPRANAS EN C
    if (MODELO_N == "AB_MOD22") { TIPO_M <- 2     }
    if (MODELO_N == "AB_MOD23") { TIPO_M <- 2     }
    #### COVA CON TEMP TARDIAS EN C
    if (MODELO_N == "AB_MOD24") { TIPO_M <- 3     }
    if (MODELO_N == "AB_MOD25") { TIPO_M <- 3     }
    #### COVA CON TEMP TARDIAS EN C Y TEMP TEMPRANAS EN B
    if (MODELO_N == "AB_MOD26") { TIPO_M <- 4     }
    if (MODELO_N == "AB_MOD27") { TIPO_M <- 4     }
    if (MODELO_N == "AB_MOD30") { TIPO_M <- 4     }
    if (MODELO_N == "AB_MOD32") { TIPO_M <- 4     }
    if (TIPO_M == 1) {
        FIJOS <- fixef(MODELO)
        if (TIPO == "TRANS") {
            A <- FIJOS[1]
            C <- FIJOS[4]
        }
        if (TIPO == "LONGI") {
            A <- FIJOS[1] + FIJOS[2]
            C <- FIJOS[4] + FIJOS[5]
        }
        B <- FIJOS[3]
        AGRUPA <- c(A, B, C)
        RETORNA <- as.data.frame(t(AGRUPA))
    }
    if (TIPO_M == 2) {
        FIJOS <- fixef(MODELO)
        if (TIPO == "TRANS") {
            A <- FIJOS[1]
        }
        if (TIPO == "LONGI") {
            A <- FIJOS[1] + FIJOS[2]
        }
        B <- FIJOS[3]
        if (TIPO == "TRANS") {
            C <- FIJOS[5]
        }
        if (TIPO == "LONGI") {
            C <- FIJOS[5] + FIJOS[6]
        }
        C <- C + FIJOS[4] * DATOS$B.46.T.100.MIN.16
        AGRUPA <- c(A, B, C)
        RETORNA <- as.data.frame(t(AGRUPA))
    }
    if (TIPO_M == 3) {
        FIJOS <- fixef(MODELO)
        if (TIPO == "TRANS") {
            A <- FIJOS[1]
        }
        if (TIPO == "LONGI") {
            A <- FIJOS[1] + FIJOS[2]
        }
        B <- FIJOS[3]
        if (TIPO == "TRANS") {
            C <- FIJOS[5]
        }
        if (TIPO == "LONGI") {
            C <- FIJOS[5] + FIJOS[6]
        }
        C <- C + FIJOS[4] * DATOS$B.122.T.150.MIN.22
        AGRUPA <- c(A, B, C)
        RETORNA <- as.data.frame(t(AGRUPA))
    }
    if (TIPO_M == 4) {
        FIJOS <- fixef(MODELO)
        if (TIPO == "TRANS") {
            A <- FIJOS[1]
        }
        if (TIPO == "LONGI") {
            A <- FIJOS[1] + FIJOS[2]
        }
        B <- FIJOS[3] + FIJOS[4] * DATOS$B.60.T.100.MIN.17
        if (TIPO == "TRANS") {
            C <- FIJOS[6]
        }
        if (TIPO == "LONGI") {
            C <- FIJOS[6] + FIJOS[7]
        }
        C <- C + FIJOS[5] * DATOS$B.122.T.150.MIN.22
        AGRUPA <- c(A, B, C)
        RETORNA <- as.data.frame(t(AGRUPA))
    }
   names(RETORNA) <- NOMBRE_FIJOS
   RETORNA
}

# MEDIDA=1 PROMEDIO DE ERRORES, MEDIDA=2 PROMEDIO DE ERRORES ABSOLUTOS
# MEDIDA=3 PROMEDIO DE ERRORES RELATIVOS , MEDIDA=4 PROMEDIO DE ERRORES ABSOLUTOS RELATIVOS
# MEDIDA=5 VARIANZA DEL ERROR MEDIDA=6 RAIZ DEL CUADRADO MEDIO DEL ERROR
CAPACIDAD<-function(PP,OBSERVADO,PREDICHO,MEDIDA) {
  if(MEDIDA==1){ RETORNA<-mean(PP[,OBSERVADO]-PP[,PREDICHO])  }
  if(MEDIDA==2){ RETORNA<-mean(abs(PP[,OBSERVADO]-PP[,PREDICHO]))   }
  if(MEDIDA==3){ RETORNA<-mean((PP[,OBSERVADO]-PP[,PREDICHO])/PP[,OBSERVADO])*100 }
  if(MEDIDA==4){ RETORNA<-mean(abs(PP[,OBSERVADO]-PP[,PREDICHO])/PP[,OBSERVADO])*100 }
  if(MEDIDA==5){ RETORNA<-(var(PP[,OBSERVADO]-PP[,PREDICHO]))**0.5  }
  if(MEDIDA==6){
    RR1<- mean(PP[,OBSERVADO]-PP[,PREDICHO])   
    RR2<-var(PP[,OBSERVADO]-PP[,PREDICHO])    
    RETORNA<-(RR1**2+RR2)**0.5  
  }
  RETORNA
}

ol <- function(...) tag("ol", list(...))
li <- function(...) tag("li", list(...))

carousel_events <- function(id) {
  js <- sprintf("
                $('#%s').bind('slide.bs.carousel', function (e) { 
                $(e.relatedTarget).find('.shiny-bound-output').each(function(i) {
                $('#' + this.id).trigger('shown')
                });
                $(e.target).find('.shiny-bound-output').each(function(i) {
                $('#' + this.id).trigger('hidden')
                }); 
                });
                ", id)
  tag("script", gsub("\n", "", js))
}

#' Bootstrap Carousel
#' Create a bootstrap carousel for UI elements.
#' @param id character: The identifier for the carousel.
#' @param ... Shiny UI components.
#' @param height character: the height (in CSS format)
#' @param width character: the width (in CSS format)
#' @param interval numeric: milliseconds delay interval.
#'
#' @return A Shiny UI component
#' @export
carousel <- function(id, ..., height = "1024px", width = "100%", interval = 500) {
  # Capture the carousel content items
  dots <- list(...)
  # Prepare the items
  classes <- c("item active", rep("item", length(dots)))
  items <- lapply(seq_along(dots), function(i) div(class = classes[i], dots[[i]]))
  # Setup the inner carousel
  caro_inner <- div(class = "carousel-inner", role = "listbox")
  caro_inner$children <- items
  # Setup the indicator bullets
  indicators <- ol(class = "carousel-indicators")
  indicators$children <- lapply(seq_along(dots), function(i) {
    if (i == 1)
      li(`data-target` = paste0("#", id), `data-slide-to` = "0", class = "active")
    else
      li(`data-target` = paste0("#", id), `data-slide-to` = as.character(i - 1))
  })
  # Construct the actual carousel object
  caro <- div(id = id, class = "carousel slide", `data-ride` = "carousel",
              `data-interval` = as.character(interval),
              style = sprintf("height:%s; width:%s;",
                              height,
                              width))
  caro$children <- list(caro_inner, indicators)
  return(tagList(caro, carousel_events(id)))
}



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


