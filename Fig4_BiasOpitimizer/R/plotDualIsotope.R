#
#   RESTRICTED VERSUS NON RESTRICTED
#
################################################################################

res<-read.csv("csvFiles/Restricted")
nres<-read.csv("csvFiles/NonRestricted")

resD2H<-res$D2H+res$D2H_err
resD18O<-res$D18O+res$D18O_err

nresD2H<-nres$D2H+nres$D2H_err
nresD18O<-nres$D18O+nres$D18O_err

c1=which(res$beta==0.940)
c2=which(res$beta==0.960)
c3=which(res$beta==0.980)


plot(nresD18O[c1], nresD2H[c1], pch=20, col=rgb(1,0,0, alpha=0.1),
     xlim=c(-9,-6), ylim=c(-40,-70))
points(resD18O[c1], resD2H[c1], pch=20, col=rgb(1,0,0, alpha=1))
points(nresD18O[c2], nresD2H[c2], pch=20, col=rgb(0,1,0, alpha=0.1))
points(resD18O[c2], resD2H[c2], pch=20, col=rgb(0,1,0, alpha=1))
points(nresD18O[c3], nresD2H[c3], pch=20, col=rgb(0,0,1, alpha=0.1))
points(resD18O[c3], resD2H[c3], pch=20, col=rgb(0,0,1, alpha=1))