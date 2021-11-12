function (LongNum,Dec,Num)
# turn number, Num, into character string for FORTRAN F format with Fxx.yy
#   xx=LongNum
#   yy=Dec

# note there is no exponent
{
# allow for minus sign at beginning of number
NegNum<-FALSE
if(Num<0) NegNum<-TRUE
if(NegNum) {Num<-(-Num);LongNum<-LongNum-1}


# length of integer component allowed is LongNum minus the digits in Dec minus one for the decimal point
IntLength<-LongNum-Dec-1


# determine the integer component
ChIntLength<-nchar(as.character(floor(Num)))

ChNum<-as.character(Num)
ChNumLength<-nchar(ChNum)
DecPoint<-ChIntLength+1

ChInt<-substr(ChNum,1,(DecPoint-1))
ChIntLength<-nchar(ChInt)

ChDec<-ifelse(DecPoint<ChNumLength,substr(ChNum,DecPoint+1,ChNumLength),"")
ChDecLength<-nchar(ChDec)

if (ChDecLength>Dec) {
     # round off the last digit if nchar(ChDec)>Dec
    ChDec<-paste(substr(ChDec,1,(Dec-1)),as.character(round(as.numeric(substr(ChDec,Dec,(Dec+1)))/10,0)),sep="")
  } # end ChDeclength>Dec

 ChDecLength<-nchar(ChDec)

if(NegNum) {
    ChInt<-paste("-",ChInt,sep="")
    LongNum<-LongNum+1
  }
Blanks<-"                                  "

chstring<-paste(ChInt,".",ChDec,sep="")
if (nchar(chstring)<LongNum) chstring<-paste(chstring,substr(Blanks,1,LongNum-nchar(chstring)),sep="")
chstring

}

