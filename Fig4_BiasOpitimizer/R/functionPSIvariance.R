
#---------------------------------------------
# FUNCTION TO DERIVE PSI VARIANCE MATRIX
#=============================================



PSImatrix<-function(PSI_rand, Z, scenario, SaveData, avgD){
  
  PSIVarMat=matrix(NA, length(Z), nrow(PSI_rand))
  # make matrix where all parameters of PSI function will be saved
  PSIparams=matrix(NA, nrow(PSI_rand),3)
  colnames(PSIparams)<-c("A","B","C")
  CTpsi=101.97    # Conversion factor between MPa and m H2O
  
  
  
  for( i in 1:nrow(PSI_rand)){
    reality=(PSI_rand[i,]/10000)*CTpsi
    
    # optimization function
    PSI_FUN <- function(Initparam){
      Apsi=Initparam[1];  Bpsi=Initparam[2];  Cpsi=Initparam[3]   # Derived via optimization !!!
      PSIest=((Apsi + Bpsi *log(avgD) - Cpsi*avgD^2)/10000)*CTpsi
      div=sum((abs(reality) - abs(PSIest))^2, na.rm=TRUE)
    }
    
    PSIoptim<-optim(c(200, 450, 250), PSI_FUN)
    PSIparams[i,1]<-PSIoptim$par[1]; PSIparams[i,2]<-PSIoptim$par[2]; PSIparams[i,3]<-PSIoptim$par[3];
    
    PSInew<- ((((PSIoptim$par[1] + PSIoptim$par[2] *log(Z) - PSIoptim$par[3]*Z^2)))/10000)*CTpsi
    
    # 2 conditions... 
    # if lower than saturation --> impossible = saturation
    PSIsat=-0.786;  # saturation of a silt loam soil, in m!  -------------------------------------------------------------------------
    PSInew[PSInew>=PSIsat]<- PSIsat
    
    # If you go deeper, we assume your delta is not increasing again!
    maxvalue=Z[which(PSInew==max(PSInew))]
    PSInew[which(Z>=maxvalue[1])]<-max(PSInew)
    
    PSIVarMat[,i] <- PSInew
    
  }
  # in case all params have been collected
  if(SaveData=='YES'){
    write.csv(PSIparams,paste0("csvFiles/",scenario,"/PSIparams.csv"))
    write.csv(PSIVarMat,paste0("csvFiles/",scenario,"/PSIProfiles.csv" ))
  }
  
  
  return(PSIVarMat)
}







#---------------------------------------------
# FUNCTION TO DERIVE D2Hsoil VARIANCE MATRIX
#=============================================

D2Hsoilmatrix<-function(Deut_rand=NULL,Z=NULL, scenario=NULL, SaveData=NULL, avgD=NULL){
  
  D2HsoilVarMat=matrix(NA, length(Z), nrow(Deut_rand))
  Deutparams=matrix(NA, nrow(Deut_rand),2)
  colnames(Deutparams)<-c("lsoil","msoil")
  
  for( i in 1:nrow(Deut_rand)){
    reality=Deut_rand[i,]
    soildev=0.001 # fixed
    
    # optimization function
    Deut_FUN <- function(Initparam){
      lsoil=Initparam[1];  msoil=Initparam[2]  # Derived via optimization !!!
      D2Hsoilest=lsoil * (avgD+soildev)^msoil
      div=sum((reality - D2Hsoilest)^2, na.rm=TRUE)
    }
    
    Deutoptim<-optim(c(-75, 0.15), Deut_FUN)
    Deutparams[i,1]<-Deutoptim$par[1]; Deutparams[i,2]<-Deutoptim$par[2];
    
    
    DEUTnew<- Deutoptim$par[1] * (Z+soildev)^Deutoptim$par[2]
    
    # If you go deeper, we assume your delta is not increasing again!
    minvalue=Z[which(DEUTnew==min(DEUTnew))]
    DEUTnew[which(Z>=minvalue[1])]<-min(DEUTnew)
    
    D2HsoilVarMat[,i] <- DEUTnew
    
  }
  # in case all params have been collected
  if(SaveData=='YES'){
    write.csv(Deutparams,paste0("csvFiles/",scenario,"/Fullrestricted_Deutparams.csv"))
    write.csv(D2HsoilVarMat,paste0("csvFiles/",scenario,"/Fullrestricted_DeutProfiles.csv"))
  }
  return(D2HsoilVarMat)
}






#---------------------------------------------
# FUNCTION TO DERIVE D18O-soil VARIANCE MATRIX
#=============================================

D18Osoilmatrix<-function(Oxy_rand=NULL,Z=NULL, scenario=NULL, SaveData=NULL, avgD = NULL){
  D18OsoilVarMat=matrix(NA, length(Z), nrow(Oxy_rand))
  Oxyparams=matrix(NA, nrow(Oxy_rand),2)
  colnames(Oxyparams)<-c("lsoil","msoil")
  
  for( i in 1:nrow(Oxy_rand)){
    reality=Oxy_rand[i,]
    soildev=0.001 # fixed
    
    # optimization function
    Oxy_FUN <- function(Initparam){
      lsoil=Initparam[1];  msoil=Initparam[2]  # Derived via optimization !!!
      D18Osoilest=lsoil * (avgD+soildev)^msoil
      div=sum((reality - D18Osoilest)^2, na.rm=TRUE)
    }
    
    Oxyoptim<-optim(c(-10, 0.15), Oxy_FUN)
    Oxyparams[i,1]<-Oxyoptim$par[1]; Oxyparams[i,2]<-Oxyoptim$par[2];
    
    
    OXYnew<- Oxyoptim$par[1] * (Z+soildev)^Oxyoptim$par[2]
    
    # If you go deeper, we assume your delta is not increasing again!
    minvalue=Z[which(OXYnew==min(OXYnew))]
    OXYnew[which(Z>=minvalue[1])]<-min(OXYnew)
    
    D18OsoilVarMat[,i] <- OXYnew
    
  }
  # in case all params have been collected
  if(SaveData=='YES'){
    write.csv(Oxyparams,paste0("csvFiles/",scenario,"/Fullrestricted_Oxyparams.csv"))
    write.csv(D18OsoilVarMat,paste0("csvFiles/",scenario,"/Fullrestricted_OxyProfiles.csv"))
  }
  
  return(D18OsoilVarMat)
}

