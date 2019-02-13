function (LongNum,Dec,Num)
# turn number, Num, into character string for FORTRAN G format with Gxx.yy
#   xx=LongNum
#   yy=Dec

# note that 4 places are reserved for exponent e.g. E+04
{

IntLength<-LongNum-4-Dec-1


# allow for minus sign at beginning of number
NegNum<-FALSE
if(Num<0) NegNum<-TRUE
if(NegNum) {Num<-(-Num);LongNum<-LongNum-1}

ChNum<-as.character(Num)
ChNumLength<-nchar(ChNum)

Epresent<-FALSE

for (i in 1:ChNumLength){
  if(substr(ChNum,i,i)=="E" | substr(ChNum,i,i)=="e") {
    Epos<-i
    Epresent<-TRUE
  }
}

print(ChNum)

ExpVal<-0
if(Epresent) {
  ExpVal<-as.integer(substr(ChNum,(Epos+1),ChNumLength))
  ChNum<-substr(ChNum,1,(Epos-1))
  ChNumLength<-(Epos-1)
}

Dpresent<-FALSE
DecPoint<-0

for (i in 1:ChNumLength){
  if(substr(ChNum,i,i)==".") {
    DecPoint<-i
    Dpresent<-TRUE
  }
}

# determine the integer component and make scientific if too long
ChIntLength<-ifelse(Dpresent,(DecPoint-1),ChNumLength)

      if(!Dpresent) {
          DecPoint<-ChNumLength+1
          ChNum<-paste(ChNum,".0",sep="")
          ChNumLength<-nchar(ChNum)
       }

if(Epresent){
    if(ExpVal<0){
      NumExp<- (-ExpVal)

         for(i in 1:NumExp){
           if(DecPoint>=2) ChNum<-paste("0",substr(ChNum,1,(DecPoint-2)),".",substr(ChNum,(DecPoint-1),(DecPoint-1)),substr(ChNum,DecPoint+1,ChNumLength),sep="")
           if(DecPoint==1) {
             ChNum<-paste("0.0",substr(ChNum,(DecPoint-1),(DecPoint-1)),substr(ChNum,DecPoint+1,ChNumLength),sep="")
             DecPoint<-2
           }
              ChNumLength<-nchar(ChNum)
        }
    }

    if(ExpVal>0){
      NumExp<- ExpVal
         for(i in 1:NumExp){
          if(DecPoint>1) ChNum<-paste(substr(ChNum,1,(DecPoint-1)),substr(ChNum,(DecPoint+1),(DecPoint+1)),".",substr(ChNum,(DecPoint+2),ChNumLength),"0",sep="")
          DecPoint<-DecPoint+1
          ChNumLength<-nchar(ChNum)
          }
    }
}

print(ChNum)

NumExp<-0

ChInt<-substr(ChNum,1,(DecPoint-1))
ChIntLength<-nchar(ChInt)

ChDec<-ifelse(DecPoint<ChNumLength,substr(ChNum,DecPoint+1,ChNumLength),"")
ChDecLength<-nchar(ChDec)


print(c(ChNum,ChInt,ChDec))


if (ChIntLength>IntLength | ChDecLength>Dec) {


if(ChIntLength>IntLength) NumExp<-(ChIntLength-IntLength)
if (NumExp>0){
for (i in 1:NumExp) {
ChIntDigit<-substr(ChInt,ChIntLength,ChIntLength)
ChInt<-substr(ChInt,1,(ChIntLength-1))
ChIntLength<-ChIntLength-1
ChDec<-paste(ChIntDigit,substr(ChDec,1,ChDecLength),sep="")
ChDecLength<-ChDecLength+1
}
}


  if(Num<1){
        LeadZero<-ChDecLength-nchar(as.character(as.numeric(ChDec)))
        NumExp<-(-LeadZero-1)
        ChInt<-substr(ChDec,(LeadZero+1),(LeadZero+1))
        ChDec<-substr(ChDec,(LeadZero+2),ChDecLength)
        ChDecLength<-nchar(ChDec)
     }# end ChDec==0

     # round off the last digit if nchar(ChDec)>Dec
  if(ChDecLength>Dec) {
    ChDec<-paste(substr(ChDec,1,(Dec-1)),as.character(round(as.numeric(substr(ChDec,Dec,(Dec+1)))/10,0)),sep="")
  } # end ChDeclength>Dec

 ChDecLength<-nchar(ChDec)
} # end when either Int or Dec is longer than required

# if negative number - add minus sign and restore LongNum to its true length
if(NegNum) {
    ChInt<-paste("-",ChInt,sep="")
    LongNum<-LongNum+1
  }
Blanks<-"                                  "

if(NumExp>=0 & NumExp<10) chstring<-paste(ChInt,".",ChDec,"E+0",NumExp,sep="")
if(NumExp>=0 & NumExp>=10) chstring<-paste(ChInt,".",ChDec,"E+",NumExp,sep="")
if(NumExp<0 & NumExp>(-10)) chstring<-paste(ChInt,".",ChDec,"E-0",-NumExp,sep="")
if(NumExp<0 & NumExp<=(-10)) chstring<-paste(ChInt,".",ChDec,"E-",-NumExp,sep="")
if (nchar(chstring)<LongNum) chstring<-paste(chstring,substr(Blanks,1,LongNum-nchar(chstring)),sep="")
chstring
}

