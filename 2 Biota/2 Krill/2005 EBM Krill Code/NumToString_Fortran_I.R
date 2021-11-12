function (LongNum,Num)
# turn number, Num, into character string for FORTRAN F format with Ixx
#   xx=LongNum

# integer is rounded

{

NumInt<-round(Num,0)

Blanks<-"                                  "
chstring<-as.character(NumInt)
if (nchar(chstring)<LongNum) chstring<-paste(substr(Blanks,1,LongNum-nchar(chstring)),chstring,sep="")
chstring

}

