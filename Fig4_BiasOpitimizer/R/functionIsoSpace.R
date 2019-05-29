#
#         Function Generate Density Spaces
#
################################################################################


IsoSpace <- function(itterations=NULL, B=NULL, BioVars=NULL, PSImat=NULL, 
                     soilDeut=NULL, soilOxy=NULL, relSF=NULL, 
                     scenario=NULL, Z=NULL, dZ = NULL, TCOR = NULL, t=NULL,
                     tF=NULL, RunForWhichIsotope=NULL, beta=NULL, ndays=NULL){
  
  source("./R/SWIFT.R")
  # sinc repetition, we only need data for one day and then repeat it !!
  oneDay=1:(tF*24)
  
  Ax = BioVars[,'LA'] * BioVars[,'Asapwood'] 
  Tree_deut = Tree_oxy = rep(NA,itterations)
  
  # check of SWIFT RESTRICTION
  SWIFTrest<-ifelse( all(is.na(BioVars[,'tstud'])), 'YES', 'NO')
  
  # CREATE THE RESTRICTED POPULATION
  for (it in 1:itterations){
    SF<- relSF*BioVars[it,'SFdaily']
    k <- SoilRootCond(B, BioVars[it,'Kr'], PSImat[,it], Z, 'Silt Loam')
    
    devio=(BioVars[it,'beta']^(100*Z))*(1-((BioVars[it,'beta'])^(100*dZ)))
    ARi=BioVars[it,'ARtot']*devio/sum(devio)
    
    
    if(RunForWhichIsotope=='D2H'){
      SB_deut <- SWIFT_SB(ARi, soilDeut[,it], k,  
                          PSImat[,it],  SF[oneDay], 
                          t[oneDay], Z)}
    
    if(RunForWhichIsotope!='D2H'){
      SB_oxy  <- SWIFT_SB(ARi, soilOxy[,it], k,  
                          PSImat[,it], SF[oneDay], 
                          t[oneDay], Z)}
    
    if(RunForWhichIsotope=='Both'){
      SB_deut <- 8.2*SB_oxy+11.3
    }
    
    #-------- AT SPECIFIED HEIGHT AND TIME -----------------------------------------
    
    # CHECK IF SWIFT RESTRICTION ON SAMPLING WAS DONE -
    if(SWIFTrest == 'YES'){
      
      # if yes, then the maximum of the stem base should simply be samples
      if(RunForWhichIsotope!='D18O'){Tree_deut[it]<-max(SB_deut, na.rm=TRUE)}
      if(RunForWhichIsotope!='D2H'){Tree_oxy[it] <-max(SB_oxy, na.rm=TRUE)}
      
    }else{
      
      # calculate for the given random time
      if(RunForWhichIsotope!='D18O'){
        SWH <- SWIFT_H( Ax[it], rep(SB_deut, ndays), BioVars[it,'hom'],
                        SF, BioVars[it,'tstud'], tF)
        Tree_deut[it] <- SWH[,]
      }
      
      if(RunForWhichIsotope!='D2H'){
        SWH[,]  <- SWIFT_H( Ax[it], rep(SB_oxy, ndays),BioVars[it,'hom'],
                            SF, BioVars[it,'tstud'], tF)
        Tree_oxy[it] <- SWH[,]
        
      }
      
    }
    
    
    # indication to know how far in the process
    cat("\r","beta = ",BioVars[it,'beta']," | progress = ",
        format((it/itterations)*100,nsmall=2),"%")
  }
  
  
  # prepare dataset to send back
  if(RunForWhichIsotope=='Both'){
    DataSpace <- cbind(Tree_deut, Tree_oxy)
    colnames(DataSpace)<-c('D2H','D18O')}
  
  if(RunForWhichIsotope=='D2H'){
    DataSpace <- cbind(c(Tree_deut))
    colnames(DataSpace)<-c('D2H')}
  
  
  if(RunForWhichIsotope=='D18O'){
    DataSpace <- cbind(c(Tree_oxy))
    colnames(DataSpace)<-c('D18O')}
  
  
  
  return(cbind(BioVars,DataSpace))
}