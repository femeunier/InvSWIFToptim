#
#   Make Figure of boxplot with bias per scenario
#
#########################################################
supp<-1:10
B5=B25=B50=NULL
for (i in 1:length(supp)){
  Bias5<-read.csv(paste0("./csvFiles/bias_5_",supp[i],".csv"))
    B5=rbind(B5,Bias5[c('Btrue',"Bsc1","Bsc2", "Bsc3","Bsc4")])
  Bias25<-read.csv(paste0("./csvFiles/bias_25_",supp[i],".csv"))
    B25=rbind(B25,Bias25[c('Btrue',"Bsc1","Bsc2", "Bsc3","Bsc4")])
  Bias50<-read.csv(paste0("./csvFiles/bias_50_",supp[i],".csv"))
    B50=rbind(B50,Bias50[c('Btrue',"Bsc1","Bsc2", "Bsc3","Bsc4")])
}




Bs5=c((B5$Bsc1-B5$Btrue)/B5$Btrue, (B5$Bsc2-B5$Btrue)/B5$Btrue,
      (B5$Bsc3-B5$Btrue)/B5$Btrue, (B5$Bsc4-B5$Btrue)/B5$Btrue)
Bs25=c((B25$Bsc1-B25$Btrue)/B25$Btrue, (B25$Bsc2-B25$Btrue)/B25$Btrue,
       (B25$Bsc3-B25$Btrue)/B25$Btrue,(B25$Bsc4-B25$Btrue)/B25$Btrue)
Bs50=c((B50$Bsc1-B50$Btrue)/B50$Btrue, (B50$Bsc2-B50$Btrue)/B50$Btrue,
       (B50$Bsc3-B50$Btrue)/B50$Btrue,(B50$Bsc4-B50$Btrue)/B50$Btrue)

g5=c(rep("TTC1", nrow(B5)),rep("TTC2", nrow(B5)),
        rep("TTC3", nrow(B5)),rep("TTC4", nrow(B5)))
g25=c(rep("TTC1", nrow(B25)),rep("TTC2", nrow(B25)),
     rep("TTC3", nrow(B25)),rep("TTC4", nrow(B25)))
g50=c(rep("TTC1", nrow(B50)),rep("TTC2", nrow(B50)),
     rep("TTC3", nrow(B50)),rep("TTC4", nrow(B50)))

Bstot=c(Bs5, Bs25, Bs50)
gtot=c(g5, g25, g50)
samp=c(rep(5,length(g5)), rep(25, length(g25)), rep(50, length(g50)))

width=7200
jpeg('./Fig/All_combined.jpeg',width = width, height = 2/3*width, units = "px", res = 800)

  boxplot(100*Bstot~samp*gtot,   
          col=(c("gold","lightskyblue","seagreen3")),
          las=2)
  abline(h=0,col="grey")
dev.off()




# INDIVIDUAL FIGURES
jpeg('./Fig/Single_5.jpeg')
  boxplot(Bs5~g5, col='orange', ylim=c(-0.05, 0.05),las = 1)
  abline(h=0, col='grey',  lwd=2)
dev.off()

jpeg('./Fig/Single_25.jpeg')
  boxplot(Bs25~g25, col='orange', ylim=c(-0.05, 0.05),las = 1)
  abline(h=0, col='grey',  lwd=2)
dev.off()


jpeg('./Fig/Single_50.jpeg')
  boxplot(Bs50~g50, col='orange', ylim=c(-0.05, 0.05),las = 1)
  abline(h=0, col='grey',  lwd=2)
dev.off()


