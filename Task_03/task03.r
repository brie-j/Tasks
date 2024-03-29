trueMean1<-5
trueSD1<-5
population1<-rnorm(1e6, trueMean1, trueSD1)
population1
?rnorm
trueMean2<-4
trueSD2<-5
population2<- rnorm(1e6, trueMean2, trueSD2)
Size<-50
Sample1<-sample(population1,Size)
Sample2<-sample(population2,Size)
Sample1
Sample2
Sample1 == Sample2
population1 == population2
boxplot(Sample1,Sample2)
boxplot(population1,population2)
source("http://jonsmitchell.com/code/simFxn04.R")
MatGrandma <- makeFounder("grandma-mom")
MatGrandpa <- makeFounder("grandpa-mom")
PatGrandma <- makeFounder("grandma-da")
PatGrandpa <- makeFounder("grandpa-da")
head(MatGrandma)
head(MatGrandpa)
head(PatGrandma)
head(PatGrandpa)
nrow(MatGrandma)
nrow(MatGrandpa)
nrow(PatGrandma)
nrow(PatGrandpa)
Alan<-makeBaby(PatGrandma,PatGrandpa)
Brenda<-makeBaby(MatGrandma,MatGrandpa)
Alan
Bredna
Focus<-makeBaby(Brenda,Alan)
head(Focus)
?grep
head(Focus)
head(Brenda)
grep(Brenda,Focus)
length(Focus)
nrow(Focus)
ncol(Focus)
## Question1 the percent shared is 50%.
length(grep("mom",Focus))
ToMom<-length(grep("mom",Focus))/length(Focus)
ToMom
## Question2 These numbers could be anything as long as they add up to 100%.
ToMomMom<-length(grep("grandma-mom",Focus))/length(Focus)
ToMomDad<-length(grep("grandpa-mom",Focus))/length(Focus)
ToMomDad
ToMomMom
ToDad<-length(grep("da",Focus))/length(Focus)
ToDad
ToDadMom<-length(grep("grandma-da",Focus))/length(Focus)
ToDadDad<-length(grep("grandpa-da",Focus))/length(Focus)
ToDadDad
ToDadMom
ToDadDad+ToDadMom+ToMomDad+ToMomMom
## Question3 Focus related to Maternal Grandparents: 50%, Focus related to Paternal Grandparents: 50%, but not related to each grandparent equally as focus is 5% related to the Dadsdad and 34.9% related to Dadsmom, 29.2% related to Momsdad and 20.8% to Momsmom. Due to genetic changes and crossover, yes, this distribution could be expected.
## Pt.2 Avg relatedness: 25% 
Sibling_01<-makeBaby(Brenda, Alan)
ToSib<- length(intersect(Focus,Sibling_01))/length(Focus)
ToSib
## Question4 
ManySiblings<-replicate(1e3,length(intersect(Focus,makeBaby(Brenda,Alan)))/length(Focus))
ManySiblings
quantile(ManySiblings)
?quantile
mean(ManySiblings)
## Question5 Avg focus shared 49.2% of DNA in 1000 sibs.
plot(density(ManySiblings),main="",xlab="proportion shared genes")
## Question6 there is a range of values because there is no set value of DNA shared between focus and sibs. It is dependent on different factors such as crossover and recombination. 
HWE<-function(p) {
	aa<-p^2
	ab<-2*p*(1-p)
	bb<-(1-p)^2
	return(c(aa=aa,ab=ab,bb=bb))
}
HWE(0.5)

plot(1,1,type="n",xlim=c(0,1),ylim=c(0,1),xlab="freq. allele a",ylab="geno. freq")
p<-seq(from=0,to=1,by=0.01)
GenoFreq<-t(sapply(p,HWE))
p
GenoFreq
lines(p,GenoFreq[,"aa"],lwd=2,col="red")
lines(p,GenoFreq[,"ab"],lwd=2,col="purple")
lines(p,GenoFreq[,"bb"],lwd=2,col="blue") 
legend ("top",legend=c("aa","ab","bb"),col=c("red","purple","blue"),lty=1,lwd=2,bty="n")
Pop<-simPop(500)
Pop
??simPop
Pop1<-simPop(4)
Pop1
points(Pop[,"freqa"],Pop[,"Genotypes.aa"]/500,pch=21,bg="red")
## Question7 Yes
Pop<-simPop(50)
points(Pop[,"freqa"],Pop[,"Genotypes.aa"]/50,pch=22,bg="red")
## Question8 Fixation is reached when the population size is small. There is no fixation with large populations. 
## in a smaller population fixation is reached earlier.
install.packages("learnPopGen")
library(learnPopGen)
x<-genetic.drift(Ne=200,nrep=5,pause=0.01)
x<-genetic.drift(Ne=20,nrep=5,pause=0.01)
x<-genetic.drift(Ne=2,nrep=5,pause=0.01)
x<-genetic.drift(Ne=50,nrep=5,pause=0.01)
x<-genetic.drift(Ne=10,nrep=5,pause=0.01)

## Question9 As population size decreases the alleles are becoming fixed in less time.
PopSizes<-5:50
PopSizes
Samples<-rep(PopSizes,5)
Samples
tExt<-sapply(Samples,function(x)nrow(simPop(x,500)))
tExt
Line<-lm(tExt~Samples)
Line
summary(Line)
Line$coef
plot(Samples,tExt)
abline(Line,col="red")
Line2<-lm(tExt~Samples+0)
summary(Line2)
Line2$coef
plot(Samples,tExt)
abline(Line2,col="blue")

## Extra Credit
install.packages("lmtest")
library(lmtest)
bptest(Line)
## ECQ1- pvalue=0.0001353, indicating heteroscedasticity. 
install.packages("sandwich")
library(sandwich)
robust<-coeftest(Line, vcov=vcovHC)
install.packages("robustbase")
library(robustbase)
robustLine<-lmrob(tExt~Samples)
setwd('C:\\users\\Raza\\Desktop\\Evolution\\Tasks\\Task_03')
pdf("Heteroskedasticity.pdf", height=8, width=8)
plot(Samples, tExt, main="samples vs tExt")
abline(Line, col="blue")
abline(robustLine, col="red")
legend ("top", legend=c("Regression Linear","Robust"),col=c("blue","red"),lty=1,lwd=2,bty="n")
dev.off()
## ECQ2 The slope is steep compared to the linear regression. 