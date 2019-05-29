#
#   Log Likelyhood optimization with scenario calculations
#
########################################################

# function without return of values for optimization
LogLik <- function( Beta=NULL, B=NULL,
                    FieldData=NULL, itterations=NULL,  Scenario=NULL,
                    RunForWhichIsotope = NULL,Z = NULL, relSF = NULL, dZ = NULL,
                    TCOR = NULL, t=NULL, tF = NULL , Meissner = NULL, ndays=NULL){
  
  # A. GENERATE ISOSCAPE FOR SPECIFIC BETA
  PAisospace<-RandomDataToIsospace(itterations, B, Scenario, Beta, RunForWhichIsotope, 
                                   Z, relSF,dZ, TCOR, t, tF, Meissner, ndays)   
  
  
  # Calculate the loglikelyhood
  # for when modelled for D2H and both
  if(RunForWhichIsotope != "D18O"){
    Kernel<-sm.density(
      PAisospace[,"D2H"], eval.points=FieldData[,"D2H"], display = "none"
    )
    PD2H=Kernel$estimate
    #PD2H[PD2H<=(1*10^(-5))]<-(1*10^(-5))
  }  
  # for when modelled for D18O and both
  if(RunForWhichIsotope != "D2H"){
    Kernel<-sm.density(
      PAisospace[,"D18O"], eval.points=FieldData[,"D18O"], display = "none"
    )
    PD18O=Kernel$estimate
    #PD18O[PD18O<=(1*10^(-5))]<-(1*10^(-5))
  }  
  
  # combined likelihood for when both are measured
  if(RunForWhichIsotope == "Both"){
    Kernel<-kde(
      PAisospace[,c("D2H","D18O")], eval.points=FieldData[,c("D2H","D18O")] )
    PDBoth=Kernel$estimate
    #PDBoth[PDboth<=(1*10^(-5))]<-(1*10^(-5))
  }
  
  
  
  
  # To small will cause errors
  if(RunForWhichIsotope == "D2H"){LogLik=-sum(log(PD2H))}
  if(RunForWhichIsotope == "D18O"){LogLik=-sum(log(PD18O))}
  if(RunForWhichIsotope == "Both"){LogLik=-sum(log(PDBoth))}

  # return the log likelihood
  return(LogLik)
}
