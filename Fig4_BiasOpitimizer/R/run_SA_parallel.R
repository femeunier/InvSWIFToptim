#
#     OPTIMIZATION USING THE GENSA FUNCTION
#
#############################################################
rm(list=ls())

library('lhs')
library("SWIFT")
library("GenSA")
library('sm')
library('ks')

# load standard model parameters

setwd(getwd())
maindir <- (getwd())

source('./R/dataprepStandardPara.R')
source('./R/dataprepRestrictionRanges.R')
source("./R/functionVarMatrix.R")
source("./R/functionIsoSpace.R")
source("./R/SWIFT.R")
source('./R/functionPSIvariance.R')
source('./R/functionRandomDataToIsospace.R')
source("./R/functionLogLik.R")
source("./R/functionOptim.R")
source("./R/create_Rscript_SWIFT.r")

###########################################################
## Parameters
# select a true beta value
Bs=10  # number of itterations over beta trues
Btrues= runif(Bs, min = 0.905, max = 0.995)
FDtotal=c(50)

# define/load the optimization function
itterations=20
RunForWhichIsotope='Both'

scenario_FD='Sc3'      # nature equals strongest scenario

# Submission parameters
args <- c('-l walltime=12:00:00','-l nodes=1:ppn=16')
run_per_nodes <- 100

###########################################################

# CREATION OF TRUE FIELD DATA SAMPLES
#-------------------------------------------------------------------------------

param <- 'allGradients'
values <- seq(0,1,length.out = 3)

compt <- 1
for (iBs in seq(Bs)){
  Btrue=Btrues[iBs]
  for (iFD in seq_along(FDtotal)){
    FDitter=FDtotal[iFD]   # number of samples to generate
    
    for (ivalue in seq_along(values)){
      
      param_sensitivity <- data.frame(values[ivalue])
      names(param_sensitivity) <- param
      
      # start.time <- Sys.time()
      FD<-RandomDataToIsospace(itterations = FDitter, B, scenario = scenario_FD,betaPA = Btrue,
                               RunForWhichIsotope="Both",Z, relSF,dZ, TCOR, t, tF, Meissner, n,param_sensitivity) 
          
      folder <- file.path(getwd(),'runs',paste0('run_',sprintf('%05i',compt)))
      dir.create(folder)
      write.csv(FD, file = file.path(folder,paste0( "FD_Btrue_",iBs,"_FD_",FDtotal[iFD],".csv")))
      
      script_file <- file.path(folder,paste0("script",'.R'))
      create_Rscript_SWIFT(script_file,itterations=itterations,RunForWhichIsotope=RunForWhichIsotope,scenario=NULL,Btrue = Btrue,run_id=compt,N_FD = FDitter,iBs = iBs,
                           param_sensitivity)
      
      compt <- compt + 1
    }
  }
}


# Creating Submission files
Nruns <- compt-1
folder_all <- seq(1,Nruns,run_per_nodes)

for (ifolder in seq(folder_all)){
  folder <- file.path(getwd(),'runs',paste0('run_',sprintf('%05i',folder_all[ifolder])))
  simus <- seq(folder_all[ifolder],min(Nruns,folder_all[ifolder]+run_per_nodes-1))
  
  job_list_file <- file.path(folder,'joblist.txt')
  writeLines(paste0("echo ",'"',"source('script.R')",'"',"| R --save"),con = job_list_file)
  
  cmd_l <- lapply(simus,function(sim){
    line <- file.path(dirname(folder),paste0('run_',sprintf('%05i',sim)))
    write(line,file=job_list_file,append=TRUE)})
  
  launcher_file <- file.path(folder,'launcher.sh')
  writeLines("#!/bin/bash",con = launcher_file)
  write("ml R/3.4.4-intel-2018a-X11-20180131",file=launcher_file,append=TRUE)
  write(paste("mpirun",file.path(maindir,"modellauncher","modellauncher"),job_list_file),file=launcher_file,append=TRUE)
}

# Submitting files
cmd <- "qsub"
for (ifolder in seq(folder_all)){
  folder <- file.path(getwd(),'runs',paste0('run_',sprintf('%05i',folder_all[ifolder])))
  launcher_file <- file.path(folder,'launcher.sh')
  out <- system2(cmd, c(args,launcher_file), stdout = TRUE, stderr = TRUE)
}

# runs
# maindir <- (getwd())
# isimu <- 3
# folder <- file.path(maindir,'runs',paste0('run_',sprintf('%05i',isimu)))
# setwd(folder)
# script_file <- file.path(paste0("script",'.R'))
# source(script_file)

# param <- 'ARtot'
# values <- seq(0.1,2,length.out = 3)*75
# biases <- rep(NA,length(values))
# Nsimus=3
# for (isimu in seq(Nsimus)){
#   results_file <-  file.path(maindir,'runs',paste0('run_',sprintf('%05i',isimu)),'results.csv')
#   results <- read.csv(file = results_file)
#   biases[isimu] <- results$Btrue - results$Bsc
# }
# 
# plot.new()
# plot(values,biases,type='l')

Nsimus <- 500
maindir <- (getwd())
biases <- param_v <- rep(NA,Nsimus)
param <- 'allGradients'
values <- seq(0.,2,length.out=20)
compt=1
for (isimu in seq(1,Nsimus)){
  
  current_dir <- file.path(maindir,'runs',paste0('run_',sprintf('%05i',isimu)))
  results_file <- file.path(current_dir,'results.csv')
  if (file.exists(results_file)){
    results <- read.csv(file = results_file)
    biases[compt] <- results$Btrue - results$Bsc
    
    input_files <- list.files(path = current_dir,pattern = "FD*")
    input_file <- grep(input_files,pattern='*scenario*', inv=T, value=T)
    input <- read.csv(file.path(current_dir,input_file[1]))
    if (length(input[[param]]>0)){
      param_v[compt] <- input[[param]][1]
      compt=compt+1
    }
  }
}

boxplot(bias ~ param,data = data.frame(param=param_v,bias=biases),xlab = 'Gradient heterogeneity',ylab='Bias')
abline(h=0)



