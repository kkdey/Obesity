

##########################  Why Obesity defined this way #####################


Data=read.csv("D:/Type II diabetes/Obesity.csv");
Data=Data[,-(15:16)];
colnames(Data)
attach(Data)


BMI_WHR_split_DGAT=split(data.frame(cbind(BMI,WHR)),as.factor(DGAT))
foo=BMI_WHR_split$'1';
foo[order(foo[,1]),]

#####  I wanted to make sure above whether obesity may be defined by an aggregation
####  of BMI and the WHR ratio..but that does not seem to be the case, so we 
####  leave it as it is and focus instead on the other analyses 


####  We have to redefine the obesity column?  

####  WHO defines it solely on the basis of the BMI column

library(MASS)
# den3d <- kde2d(BMI_WHR_split$'1'[,1],BMI_WHR_split$'1'[,2] )
# persp(den3d, box=FALSE)
# library(lattice)

library(sm)
library(ggplot2);

# fac=(c(rep(1,dim(BMI_WHR_split$'0')[1]),rep(2,dim(BMI_WHR_split$'1')[1])));

# BMI_WHR_split_pooled=rbind(BMI_WHR_split$'0',BMI_WHR_split$'1');

library(ggplot2)
sm.density(rbind(BMI_WHR_split_DGAT$TC,BMI_WHR_split_DGAT$CT),xlab="BMI",ylab="WHR",zlab="",xlim=c(15,40),ylim=c(0.65,1.2));
title("BMI-WHR density plot (TC)");
windows()
sm.density(BMI_WHR_split_DGAT$CC,xlab="BMI",ylab="WHR",zlab="",xlim=c(15,40),ylim=c(0.65,1.2));
title("BMI-WHR density plot (CC)");

windows()
sm.density(BMI_WHR_split_DGAT$TT,xlab="BMI",ylab="WHR",zlab="",xlim=c(15,40),ylim=c(0.65,1.2));
title("BMI-WHR density plot (TT)");


########################  MC4R  Analysis ###############################

BMI_WHR_split_MC4R=split(data.frame(cbind(BMI,WHR)),as.factor(MC4R))

library(ggplot2)
sm.density(BMI_WHR_split_MC4R$'G/A',xlab="BMI",ylab="WHR",zlab="",xlim=c(15,40),ylim=c(0.65,1.2));
title("BMI-WHR density plot (G/A) MC4R");
windows()
sm.density(BMI_WHR_split_MC4R$'G/G',xlab="BMI",ylab="WHR",zlab="",xlim=c(15,40),ylim=c(0.65,1.2));
title("BMI-WHR density plot (G/G) MC4R");



xtabs(~DGAT+gender)
xtabs(~MC4R+gender)


