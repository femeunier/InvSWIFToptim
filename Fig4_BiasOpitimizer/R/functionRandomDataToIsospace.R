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

  
  # 2. Creat soil heterogeneity
  PSIprofiles <- SoilHeterogeneity(itterations, scenario, 
                                   DataType="PSI", Z, SaveData="NO", Meissner)
  
  soilDeuterium <- SoilHeterogeneity(itterations, scenario=scenario, 
                                     DataType="D2H", Z, SaveData="NO", Meissner)
  
  soilOxygen <- SoilHeterogeneity(itterations, scenario=scenario, 
                                  DataType="D18O", Z, SaveData="NO", Meissner)
  
  if(!is.null(param_sensitivity)){
    
    if (names(param_sensitivity) == "SoilHeterogeneity"){
      Meissner_mod <- Meissner
      Meissner_mod$sdPsi <- 0
      
      PSIprofiles <- SoilHeterogeneity(itterations, scenario, 
                                       DataType="PSI", Z, SaveData="NO", Meissner_mod)
      
      Xmin <-  matrix(rep(PSIprofiles[nrow(PSIprofiles),],length(Z)),nrow = length(Z))
      PSIprofiles <- Xmin + (PSIprofiles- Xmin)*matrix(rep(1+seq(param_sensitivity[[names(param_sensitivity)]],0,length.out = length(Z)),itterations),ncol=itterations)
      
    } else if (names(param_sensitivity) == "allGradients"){
      
      Meissner_mod <- Meissner
      Meissner_mod$sdPsi  <- Meissner_mod$sdDeuterium <- Meissner_mod$sdOxygen <-0
      
      PSIprofiles <- SoilHeterogeneity(itterations, scenario, 
                                       DataType="PSI", Z, SaveData="NO", Meissner_mod)
      
      soilDeuterium <- SoilHeterogeneity(itterations, scenario=scenario, 
                                         DataType="D2H", Z, SaveData="NO", Meissner_mod)

      soilOxygen <- SoilHeterogeneity(itterations, scenario=scenario, 
                                      DataType="D18O", Z, SaveData="NO", Meissner_mod)
      
      Xmin <-  matrix(rep(PSIprofiles[nrow(PSIprofiles),],length(Z)),nrow = length(Z))
      PSIprofiles <- Xmin + (PSIprofiles- Xmin)*matrix(rep(1+seq(param_sensitivity[[names(param_sensitivity)]],0,length.out = length(Z)),itterations),ncol=itterations)
      
      Xmin <-  matrix(rep(soilDeuterium[nrow(soilDeuterium),],length(Z)),nrow = length(Z))
      soilDeuterium <- Xmin + (soilDeuterium - Xmin)*matrix(rep(1+seq(param_sensitivity[[names(param_sensitivity)]],0,length.out = length(Z)),itterations),ncol=itterations)
    
      Xmin <-  matrix(rep(soilOxygen[nrow(soilOxygen),],length(Z)),nrow = length(Z))
      soilOxygen <- Xmin + (soilOxygen- Xmin)*matrix(rep(1+seq(param_sensitivity[[names(param_sensitivity)]],0,length.out = length(Z)),itterations),ncol=itterations)

    } else if (names(param_sensitivity) == "IsotopeGradients") {
      
      Meissner_mod <- Meissner
      Meissner_mod$sdDeuterium <- Meissner_mod$sdOxygen <-0
      
      soilDeuterium <- SoilHeterogeneity(itterations, scenario=scenario, 
                                         DataType="D2H", Z, SaveData="NO", Meissner_mod)
      
      soilOxygen <- SoilHeterogeneity(itterations, scenario=scenario, 
                                      DataType="D18O", Z, SaveData="NO", Meissner_mod)
      
      Xmin <-  matrix(rep(soilDeuterium[nrow(soilDeuterium),],length(Z)),nrow = length(Z))
      soilDeuterium <- Xmin + (soilDeuterium - Xmin)*matrix(rep(1+seq(param_sensitivity[[names(param_sensitivity)]],0,length.out = length(Z)),itterations),ncol=itterations)
      
      Xmin <-  matrix(rep(soilOxygen[nrow(soilOxygen),],length(Z)),nrow = length(Z))
      soilOxygen <- Xmin + (soilOxygen- Xmin)*matrix(rep(1+seq(param_sensitivity[[names(param_sensitivity)]],0,length.out = length(Z)),itterations),ncol=itterations)

      
    } else {
      FieldVar[,names(param_sensitivity)] <- param_sensitivity[[names(param_sensitivity)]] 
    }
  }
  
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
  
  if(!is.null(param_sensitivity)){
    if (names(param_sensitivity) == "SoilHeterogeneity"){
      isospaces <- cbind(isospaces,SoilHeterogeneity = param_sensitivity[[names(param_sensitivity)]])
    } else if (names(param_sensitivity) == "allGradients"){
      isospaces <- cbind(isospaces,allGradients = param_sensitivity[[names(param_sensitivity)]])
    } else if (names(param_sensitivity) == "IsotopeGradients"){
      isospaces <- cbind(isospaces,IsotopeGradients = param_sensitivity[[names(param_sensitivity)]])
    }
  }
  
  # Return as matrix
  return(isospaces)
}
