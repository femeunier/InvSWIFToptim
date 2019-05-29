#----------------------------------------------  
# LOGLIK OPTIMIZATION FUNCTION
#----------------------------------------------
# for beta plant collection
LogLikOptim <- function(x) {
  LogLik(Beta=x,B,FD, itterations, Scenario,
         RunForWhichIsotope ,Z , relSF, dZ ,
         TCOR ,t, tF , Meissner, n)
}

# LogLikOptim_solo <- function(x) {
#   LogLik(Beta=x, B,FDsolo, itterations, Scenario,
#          RunForWhichIsotope ,Z , relSF, dZ ,
#          TCOR ,t, tF , Meissner, n)
# }

