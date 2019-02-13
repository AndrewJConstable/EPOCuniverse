function (PGfile,Test)
{
SSBstat<-PGfile[(PGfile[,"Test"]==Test),]

 boxplot(SSB.Status~Year,SSBstat,ylim=c(0,2.5),xlab="Year",ylab="SSB Status")
}

