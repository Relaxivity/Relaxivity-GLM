trialnum<-readline(prompt = "Please input the number of points tested: ")#asking user how many trials they have
x=1:trialnum;#construct vector [1...x]
concentrationlist=0;#creating concentration vector
tilist=0;#creating t1 vector

for (vlaue in x) {
  concentration<-readline(prompt = "Please input concentration value in mM: ");#getting user concentration input
  concentrationlist<-append(concentrationlist,concentration);#adding using input to concentrationlist
  ti<-readline(prompt="Please input T1 or T2 value(ms) associated with your concentration: ");
  tilist<-append(tilist,ti);

}
concentrationlist<-concentrationlist[-1];#removing the initial 0 value set at beginning
tilist<-tilist[-1];

tilist<-type.convert(tilist,numericals=c("warn.loss"));

concentrationlist<-type.convert(concentrationlist,numericals=c("warn.loss"));
C=concentrationlist;
Ti=tilist/1000;#converting T1 list into seconds

Ri=1/Ti;

statmethod<-readline(prompt = "Please input codes for statistical methods you would like to use (Case sens):\n
 OLS\n
 WLS\n
 GGLM-INV\n
GGLM-ID\n
LNLS  \n")

T1orT2<-readline(prompt="(Case Sens) Enter T1 if you measured T1, T2 if you measured T2 ");

if(statmethod=="OLS")
{Tifit<-lm(Ri~C);

}else if(statmethod=="WLS")
{Tifit<-lm(Ri~C,weights=1/Ri^2)

}else if(statmethod=="LNLS")
{init<-lm(Ri~C);
Riwater0<-coef(init)[1];
relax0<-coef(init)[2];

LRi=log(Ri);

Tifit<-nls(LRi~log(Riw+r*C),start=list(Riw=Riwater0,r=relax0));

}else if(statmethod=="GGLM-INV")
{Tifit<-glm(Ti~C,family=Gamma(link=inverse));



}else
{Tifit<-glm(Ri~C,family=Gamma(link=identity));
statmethod<-"GGLM-ID";
}




Relaxivity<-coef(Tifit)[2]; #extracting coefficient
Intercept<-coef(Tifit)[1];
Error<-sqrt(vcov(Tifit)[4]); #extracting error

Relaxivity=round(Relaxivity,3);
Intercept=round(Intercept,3);
Error=round(Error,3);

if(T1orT2=="T1"){
plot(C,Ri,pch=19,main=expression(paste("R"[1]," ","Relaxivity Fit")),xlab="Concentration (mM)",ylab=expression(paste("R"[1]," ",(s^-1))))
abline(Tifit,col="darkgreen")
}else{
  
  plot(C,Ri,pch=19,main=expression(paste("R"[2]," ","Relaxivity Fit")),xlab="Concentration (mM)",ylab=expression(paste("R"[2]," ",(s^-1))))
  abline(Tifit,col="darkgreen")
}


Relaxivity.char=as.character(Relaxivity);
Error.char=as.character(Error);
Intercept.char=as.character(Intercept);

summary(Tifit) 
cat("Your relaxivity is",Relaxivity.char,"+-",Error.char,"With Intercept",Intercept.char,"Method",statmethod);
