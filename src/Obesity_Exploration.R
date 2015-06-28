
#################   Type II Diabetes (Obesity Data) #####################

Data=read.csv("D:/Type II diabetes/Obesity.csv");
Data=Data[,-(15:16)];
colnames(Data)
attach(Data)

BMI_split=split(BMI,as.factor(Subject.type))
boxplot(BMI_split)

library(vioplot)
vioplot(BMI_split$'0',BMI_split$'1',names=c("Non obese","Obese"))
title("Violin plot of BMI against Subject type");
title(ylab="BMI");

#############  BMI is basically the response (It is an effect of obesity) ####


####   degree of obesity can be measured by BMI of that person #################

Tab=xtabs(~as.factor(gender)+as.factor(Subject.type));
Tab=Tab[-3,];
pval_Tab=chisq.test(Tab)$p.value;


####  significance inn the pvalue (so there is an effect of gender)

###  may be the sex chromosome is also important somewhat for the obesity
Fac=LDL.HDL.RATIO
Fac_split=split(Fac,as.factor(Subject.type))
boxplot(Fac_split)

library(vioplot)
vioplot(Fac_split$'0',Fac_split$'1',names=c("Non obese","Obese"))
title("Violin plot of LDL/HDL against Subject type");
title(ylab="LDL/HDL");


#######   LDL/HDL ratio is also extremely high for obese persons compared 

####   to the non-obese persons. These seem to be important in deciding if 

####  a person is obese or not..



###################  Confusion about obesity  ##########################


table(split(MC4R,as.factor(Subject.type))$'0')
table(split(MC4R,as.factor(Subject.type))$'1')

table(split(DGAT,as.factor(Subject.type))$'0')
table(split(DGAT,as.factor(Subject.type))$'1')



DGAT[which(DGAT=="TC")]="CT";
DGAT=DGAT[DGAT!="TC"]

index=which(gender==3);
L1=glm(Subject.type[-index]~as.factor(MC4R[-index])+ as.factor(DGAT[-index])
			+ as.factor(gender[-index]),family=binomial(link="logit"));
summary(L1)

L2=glm(Subject.type[-index]~as.factor(DGAT[-index])
			+ as.factor(gender[-index]),family=binomial(link="logit"));
summary(L2)




