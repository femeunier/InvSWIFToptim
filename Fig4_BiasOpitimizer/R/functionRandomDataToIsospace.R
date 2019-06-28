#
#     ISO SPACE RESULTS
#
###############################################################################

RandomDataToIsospace <- function(itterations = NULL, B=NULL, scenario = NULL, 
                                 betaPA = NULL, RunForWhichIsotope = NULL,
                                 Z = NULL, relSF = NULL, dZ = NULL,
                                 TCOR = NULL,t=NULL, tF = NULL , 
                                 Meissner = NULL, ndays=NULL,
                                 param_sensitivity = NULL){
  
  # 1. Create (bio)physical variable matrix all random
  FieldVar <- VarMatrix(itterations, scenario, betaPA)
  
  if(!is.null(param_sensitivity)){
    FieldVar[,names(param_sensitivity)] <- param_sensitivity[[names(param_sensitivity)]]
  }
  
  # 2. Creat soil heterogeneity
  PSIprofiles <- SoilHeterogeneity(itterations, scenario, 
                                   DataType="PSI", Z, SaveData="NO", Meissner)
  
  soilDeuterium <- SoilHeterogeneity(itterations, scenario=scenario, 
                                     DataType="D2H", Z, SaveData="NO", Meissner)
  
  soilOxygen <- SoilHeterogeneity(itterations, scenario=scenario, 
                                  DataType="D18O", Z, SaveData="NO", Meissner)
  
  #3. create isospaces
  isospaces <- IsoSpace (itterations,B, FieldVar, 
                         PSIprofiles, soilDeuterium,
                         soilOxygen, relSF ,
                         scenario, Z, dZ, TCOR, t, tF, 
                         RunForWhichIsotope, beta, n)
  
  #4. add random error extraction errors
  if (RunForWhichIsotope == 'D2H'){
    isospaces[,'D2H'] <- isospaces[,'D2H']+rnorm(n = itterations, mean=0, sd = 1)}
  
  if (RunForWhichIsotope == 'D18O'){
    isospaces[,'D18O'] <- isospaces[,'D18O']+rnorm(n = itterations, mean=0, sd = 0.1)}
  
  if (RunForWhichIsotope == 'Both'){
    isospaces[,"D2H"] <- isospaces[,"D2H"]+rnorm(itterations, mean=0, sd = 1)
    isospaces[,"D18O"] <- isospaces[,"D18O"]+rnorm(itterations, mean=0, sd = 0.1)
  }
  
  # Return as matrix
  return(isospaces)
}