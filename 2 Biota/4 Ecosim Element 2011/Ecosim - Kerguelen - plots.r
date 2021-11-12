Xlab <-"Time"
Ylab <- "Log biomass"
PlotLogScale<-TRUE

GroupNames<-c("Marine Mammals & Birds","Pelagic Fish & squid","Benthic trophic groups","Production")
Groups<-list(G1 = c(1:5)
            ,G2 = c(6:10,13,14)
            ,G3 = c(11,12,15:17)
            ,G4 = c(18:23)
            ) # end list
            
PlotRegion<-list(
            P1   = c(0.2,0.5,0.6,0.9)
           ,P2   = c(0.6,0.9,0.6,0.9)
           ,P3   = c(0.2,0.5,0.15,0.45)
           ,P4   = c(0.6,0.9,0.15,0.45)
           ,Pall = c(0.2,0.9,0.15,0.9)
            )

# Data are in Res
# [1] "x"        "Time"     "XperStep" "fx"


# plot 1
PlotN<-1
X<-Res$Time
Y<-Res$XperStep[Groups[[PlotN]],]
if(PlotLogScale) Y<-log(Y)
par(plt=PlotRegion[[PlotN]]) # coordinates of plot region as fraction of figure region
plot(NA,NA,xlim=c(min(X),max(X)),ylim=c(min(Y),max(Y)),xlab="",ylab=Ylab,main=GroupNames[PlotN])
for(i in 1:nrow(Y)) lines(X,Y[i,],col=i)
# legend("topleft",SpNames[Groups[[PlotN]]],col=c(1:nrow(Y)))

# plot 2
par(new=TRUE)
PlotN<-2
X<-Res$Time
Y<-Res$XperStep[Groups[[PlotN]],]
if(PlotLogScale) Y<-log(Y)
par(plt=PlotRegion[[PlotN]]) # coordinates of plot region as fraction of figure region
plot(NA,NA,xlim=c(min(X),max(X)),ylim=c(min(Y),max(Y)),xlab="",ylab="",main=GroupNames[PlotN])
for(i in 1:nrow(Y)) lines(X,Y[i,],col=i)


# plot 3
par(new=TRUE)
PlotN<-3
X<-Res$Time
Y<-Res$XperStep[Groups[[PlotN]],]
if(PlotLogScale) Y<-log(Y)
par(plt=PlotRegion[[PlotN]]) # coordinates of plot region as fraction of figure region
plot(NA,NA,xlim=c(min(X),max(X)),ylim=c(min(Y),max(Y)),xlab=Xlab,ylab=Ylab,main=GroupNames[PlotN])
for(i in 1:nrow(Y)) lines(X,Y[i,],col=i)


# plot 4
par(new=TRUE)
PlotN<-4
X<-Res$Time
Y<-Res$XperStep[Groups[[PlotN]],]
if(PlotLogScale) Y<-log(Y)
par(plt=PlotRegion[[PlotN]]) # coordinates of plot region as fraction of figure region
plot(NA,NA,xlim=c(min(X),max(X)),ylim=c(min(Y),max(Y)),xlab=Xlab,ylab="",main=GroupNames[PlotN])
for(i in 1:nrow(Y)) lines(X,Y[i,],col=i)


# junk code
par(bg="white"   # background colour
   ,cex.axis= 1.0 # axis labels are 1.2 times ps (font size)
   ,cex.lab = 1.2  # tick labels are 1 times ps
   ,fin     = c(3.27,6.83) # figure dimensions
#   ,mfcol   = c(1,2)
   ,ps      = 8
   ) # end par


            par(plt=c(0.3,0.625,0.2,0.9)) # coordinates of plot region as fraction of figure region
            if (PlotHoriz)
                 barplot(ERsummary1,beside=TRUE,xlim=Ylim[[PlotNum]]*Yscale,xlab=Ylab[PlotNum],ylab=Xlab,legend.text=NULL,names.arg=NamesArg,horiz=PlotHoriz,col=LegendCols)
            else
                 barplot(ERsummary1,beside=TRUE,ylim=Ylim[[PlotNum]]*Yscale,ylab=Ylab[PlotNum],xlab=Xlab,legend.text=NULL,names.arg=NamesArg,horiz=PlotHoriz,col=LegendCols)


    # plot 2
    par(new=TRUE)
    PlotNum<-2

            par(plt=c(0.675,1.0,0.2,0.9)) # coordinates of plot region as fraction of figure region
            if (PlotHoriz)
                 barplot(ERsummary2,beside=TRUE,xlim=Ylim[[PlotNum]]*Yscale,xlab=Ylab[PlotNum],ylab=NULL,legend.text=NULL,names.arg=NULL,horiz=PlotHoriz,col=LegendCols)
            else
                 barplot(ERsummary2,beside=TRUE,ylim=Ylim[[PlotNum]]*Yscale,ylab=Ylab[PlotNum],xlab=NULL,legend.text=NULL,names.arg=NULL,horiz=PlotHoriz,col=LegendCols)
# plot legend
#          legend(x="bottom",y="right",legend=LegendText,fill=LegendCols)

