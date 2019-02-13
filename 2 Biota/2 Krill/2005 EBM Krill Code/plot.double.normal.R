function(x1=300,x2=700,sL=50,sR=100,fmax=1,xlo=0,xhi=1500,cex.multiplier=1,
         XYlabels=c("Age","Selectivity"),Line=1)
{

# left normal
Xval<-c(0:1000)/1000*(x1-xlo)+xlo
Yval<-dnorm(Xval,x1,sL)
Yval<-Yval/max(Yval)*fmax
Dist<-cbind(Xval,Yval)

# plateau
  if(x2>x1){
Xval<-c(0:1000)/1000*(x2-x1)+x1
Dist<-rbind(Dist,cbind(Xval,fmax))
}

# right normal
Xval<-c(0:1000)/1000*(xhi-x2)+x2
Yval<-dnorm(Xval,x2,sR)
Yval<-Yval/max(Yval)*fmax
Dist<-rbind(Dist,cbind(Xval,Yval))

plot (Dist[,1],Dist[,2],xlab=XYlabels[1],ylab=XYlabels[2],type="l",lty=Line)
}

