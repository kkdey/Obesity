

#####################   Model  Fitting for Obesity  #######################

###  In this code we shall see some models that we shall use to determine
###  the effect of the two genes under study, namely the MC4R and the DGAT 
###  genes (booth of which are biallelic though for MC4R, we do not have AA
###  combination occurring in the data and it has been vrified that it is not 
###   a sex chromosome too. 


###   the first thing that seemed to make sense for modeling would be the 
###   LDL/HDL ratio. 

Data=read.csv("D:/Type II diabetes/Obesity.csv");
Data=Data[,-(15:16)];
colnames(Data)

index=which(Data$gender==3);

Data=Data[-index,];
attach(Data)
DGAT[which(DGAT=="CT")]="TC";



Lmodel_LDL.HDL.RATIO_prev= lm(LDL.HDL.RATIO ~ DGAT+MC4R+Age+gender); 
summ_Lmodel_LDL.HDL.RATIO_prev=summary(Lmodel_LDL.HDL.RATIO_prev);

plot(density(LDL.HDL.RATIO));
lines(density(LDL.HDL.RATIO[gender==1]),col="red");
lines(density(LDL.HDL.RATIO[gender==2]),col="green");


plot(density(LDL.HDL.RATIO));
lines(density(LDL.HDL.RATIO[DGAT=="TT" & MC4R=="G/G"]),col="red");
lines(density(LDL.HDL.RATIO[DGAT=="CC" & MC4R=="G/G"]),col="green");
lines(density(LDL.HDL.RATIO[DGAT=="CT" & MC4R=="G/G"| DGAT=="TC" &MC4R=="G/G"]),col="blue");

windows()
plot(density(LDL.HDL.RATIO));

lines(density(LDL.HDL.RATIO[DGAT=="TT" & MC4R=="G/A"]),col="red");
lines(density(LDL.HDL.RATIO[DGAT=="CC" & MC4R=="G/A"]),col="green");
lines(density(LDL.HDL.RATIO[DGAT=="CT" & MC4R=="G/A"| DGAT=="TC" &MC4R=="G/A"]),col="blue");





plot(density(LDL.HDL.RATIO));
lines(density(LDL.HDL.RATIO[MC4R=="G/A"]),col="red");
lines(density(LDL.HDL.RATIO[MC4R=="G/G"]),col="green");


####  the above graphs were plotted to see if one of the factors gender, MC4R
###  and DGAT expresses the mixture density we are seeing in the graph. None
###  very strongly expresses this difference in my opinion, though the 
###  strongest result seems to come from MC4R

boxcox(Lmodel_LDL.HDL.RATIO_prev)

Lmodel_LDL.HDL.RATIO= lm(LDL.HDL.RATIO ~ DGAT+MC4R+gender+Age); 
summ_Lmodel_LDL.HDL.RATIO=summary(Lmodel_LDL.HDL.RATIO);


plot(Lmodel_LDL.HDL.RATIO$fitted,residuals(Lmodel_LDL.HDL.RATIO),xlab="fitted values",
		ylab="residuals",main="Fitted values vs residuals plot for LDL.HDL ratio",
		col="red",pch=20,lwd=1)

plot(density(residuals(Lmodel_LDL.HDL.RATIO)),col="red",main="density plot of residuals",
		xlab="residuals",ylab="density")
########################  BMI modeling  ##################################

plot(BMI~LDL.HDL.RATIO)
plot(WHR~LDL.HDL.RATIO)
cor(BMI,LDL.HDL.RATIO)
cor(WHR,LDL.HDL.RATIO)


#################   Number of Risk alleles modeling ########################

xtabs(~Subject.type+DGAT);
xtabs(~Subject.type+MC4R);
DGAT_risk=array(0,length(DGAT));
DGAT_risk[which(DGAT=="TC")]=1;
DGAT_risk[which(DGAT=="TT")]=2;
DGAT_risk[which(DGAT=="CC")]=0;

MC4R_risk=array(0,length(MC4R));
MC4R_risk[which(MC4R=="G/A")]=1;
MC4R_risk[which(MC4R=="G/G")]=2;

gender=as.factor(gender);
attach(Data);



Lmodel_LDL.HDL.RATIO_risk_allele= lm(LDL.HDL.RATIO ~ DGAT_risk+MC4R_risk+gender+Age); 
summ_Lmodel_LDL.HDL.RATIO_risk_allele=summary(Lmodel_LDL.HDL.RATIO_risk_allele);

plot(Lmodel_LDL.HDL.RATIO_risk_allele$fitted,residuals(Lmodel_LDL.HDL.RATIO_risk_allele));

anova(Lmodel_LDL.HDL.RATIO_risk_allele,Lmodel_LDL.HDL.RATIO)

##########################  BMI modeling  ##############################

Lmodel_BMI_risk_allele= lm(BMI ~ DGAT_risk+MC4R_risk+gender); 
summ_Lmodel_BMI_risk_allele=summary(Lmodel_BMI_risk_allele);


Lmodel_BMI= lm(BMI ~ DGAT+MC4R+gender+Age); 
summ_Lmodel_BMI=summary(Lmodel_BMI);
library(MASS)

boxcox(Lmodel_BMI);

anova(Lmodel_BMI,Lmodel_BMI_risk_allele);

plot(BMI, LDL.HDL.RATIO, col="red",pch=20,lwd=1,main="Plot of BMI and the LDL.HDL.RATIO");
cor(BMI, LDL.HDL.RATIO)
#### ANOVA test not significant.
#### This indicates we can just work with the risk alleles data

###  this gives us a test for the validation of the fit as well 

plot(density(BMI),main="BMI")
lines(density(BMI[gender==1]),col="red");
lines(density(BMI[gender==2]),col="green");

plot(Lmodel_BMI$fitted,residuals(Lmodel_BMI),xlab="fitted values",
		ylab="residuals",main="Fitted values vs residuals plot for BMI data",col="red",pch=20,lwd=1)

plot(density(residuals(Lmodel_BMI)),col="red",main="density plot of residuals for BMI model",
		xlab="residuals",ylab="density")



########################  WHR  Ratio  ###############################

plot(density(WHR),main="WHR")
lines(density(WHR[gender==1]),col="red");
lines(density(WHR[gender==2]),col="green");

WHR_LDL.HDL.RATIO=cbind(WHR,LDL.HDL.RATIO);
sm.density(WHR_LDL.HDL.RATIO,xlab="WHR",ylab="LDL.HDL.Rat",zlab="");
title("WHR LDL_HDL density plot");


Lmodel_WHR_risk_allele= lm(1/WHR ~ DGAT_risk+MC4R_risk+gender); 
summ_Lmodel_WHR_risk_allele=summary(Lmodel_WHR_risk_allele);


Lmodel_WHR= lm(WHR ~ DGAT+MC4R+gender+Age); 
summ_Lmodel_WHR=summary(Lmodel_WHR);


plot(Lmodel_WHR$fitted[gender==1],residuals(Lmodel_WHR)[gender==1],xlab="fitted values",
		ylab="residuals",main="Fitted values vs residuals plot for WHR data",
			col="red",pch=20,lwd=1);

points(Lmodel_WHR$fitted[gender==2],residuals(Lmodel_WHR)[gender==2],xlab="fitted values",
		ylab="residuals",main="Fitted values vs residuals plot for WHR data",
			col="green",pch=20,lwd=1);

legend("topleft",fill=c("red","green"),c("male","female"));


plot(density(residuals(Lmodel_WHR)),col="red",main="density plot of residuals for WHR model",
		xlab="residuals",ylab="density")

plot(density(WHR[gender==1]),col="red",ylim=c(0,8),main="",xlab="",ylab="");
lines(density(WHR[gender==2]),col="green");
legend("topleft",fill=c("red","green"),c("male","female"));
title("WHR density plots for males and females");
abline(v=0.8,col="seagreen4")
abline(v=0.9,col="darkred")
title(xlab="WHR");
title(ylab="Density");



#################   Defining abnormal obesity  ########################

#######################  WHO standard  ##############################


WHO_obese_indicator=array(0,length(BMI));
WHO_obese_indicator_India=array(0,length(BMI));


WHO_obese_indicator[which(BMI>=30)]=1;

WHO_obese_indicator_India[which(BMI>=25)]=1;

Gmodel_obese=glm(as.factor(WHO_obese_indicator)~DGAT+MC4R+gender+Age,family=binomial(link="logit"));
summ_Gmodel_obese=summary(Gmodel_obese);

Gmodel_obese_risk_allele=glm(as.factor(WHO_obese_indicator)~DGAT_risk+MC4R_risk+gender,family=binomial(link="logit"));
summ_Gmodel_obese_risk_allele=summary(Gmodel_obese_risk_allele);

anova(Gmodel_obese,Gmodel_obese_risk_allele);

plot(density(BMI),col="red",main="Density plot of BMI",xlab="BMI",ylab="Density");
abline(v=25);
abline(v=30);


Gmodel_obese_India=glm(as.factor(WHO_obese_indicator_India)~DGAT+MC4R+gender+Age,family=binomial(link="logit"));
summ_Gmodel_obese_India=summary(Gmodel_obese_India);

Gmodel_obese_India_risk_allele=glm(as.factor(WHO_obese_indicator_India)~DGAT_risk+MC4R_risk+gender,family=binomial(link="logit"));
summ_Gmodel_obese_India_risk_allele=summary(Gmodel_obese_India_risk_allele);

anova(Gmodel_obese_India,Gmodel_obese_India_risk_allele);


###################   Abdominal Obesity  ############################

WHO_Ab_obese_indicator=array(0,length(WHR));

WHO_Ab_obese_indicator[which(WHR>=0.9 & gender==1 | WHR>=0.8 & gender==2)]=1;

Gmodel_Ab_obese=glm(as.factor(WHO_Ab_obese_indicator)~DGAT+MC4R+gender+Age,family=binomial(link="logit"));
summ_Gmodel_Ab_obese=summary(Gmodel_Ab_obese);

Gmodel_Ab_obese_risk_allele=glm(as.factor(WHO_Ab_obese_indicator)~DGAT_risk+MC4R_risk+gender,family=binomial(link="logit"));
summ_Gmodel_Ab_obese_risk_allele=summary(Gmodel_Ab_obese_risk_allele);

anova(Gmodel_Ab_obese,Gmodel_Ab_obese_risk_allele);










