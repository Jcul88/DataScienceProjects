###R work 2015

citation()
2+2
myresult<-2+2
myresult
x <-c(2,4,8,16,32)
y <-c(1,2,4,8,16)
x
y
##plotting my data
plot(x,y)

##providing a regression line
abline(lm(y~x))

##First data analysis
firstdata<-read.table("Caterpillars.txt",header=T)
View(firstdata)
names(firstdata)
summary(firstdata)

##printing
print(POPDEN)
print(firstdata$POPDEN)

##printing sample size or means and sdeviation
mean(firstdata$POPDEN)
sd(firstdata$POPDEN)
length(firstdata$POPDEN)##Sample size

## Creating new variable
newvariable<-log(firstdata$POPDEN)

##Joining two data
seconddata<-cbind(firstdata,newvariable)
names(seconddata)
View(seconddata)

###Saving seconddata to working directory
write.table(seconddata,file="Caterpillars.txt",col.names=T,row.names=F,sep="\t")

##drawing plots to see if our data is normally distributed or not
qqnorm(seconddata$WGTGAIN)
qqline(seconddata$WGTGAIN)

##Drawing a histogram
hist(seconddata$WGTGAIN)

##Creating a scatter plot
plot(firstdata$POPDEN,firstdata$WGTGAIN)

##Bargraphs
bargraph.CI(HABITAT,WGTGAIN,legend=TRUE,ylab="Weight gain of caterpillars",
xlab="Habitat",lc=TRUE,uc=TRUE,cex.lab=1.5, cex.axis=1.2,
cex.leg=1, las=1, col="red",data=firstdata)

###########Univariate statistics worksheet3
oldf<-read.table("OldfieldsData.txt",header=T)
str(oldf)
modist<-lm(SpeciesRichness~Disturbance,data=oldf)
summary(modist)
anova(modist)

### Checking the parameter output
modist1<-lm(SpeciesRichness~Disturbance-1,data=oldf)
summary(modist1)

##Checking my model assumptions
qqnorm(modist$residuals)
qqline(modist$residuals)

##Boxplots for my model
boxplot(oldf$SpeciesRichness~oldf$Disturbance)

##Bargraph for my model
bargraph.CI(oldf$Disturbance,oldf$SpeciesRichness)

##Question 2
chick<-read.table("DataChickenweight.txt",header=T)
str(chick)
View(chick)
qqnorm(chick$Chickenweight)
qqline(chick$Chickenweight)

chickmod<-lm(Chickenweight~Food-1,data=chick)
summary(chickmod)
anova(chickmod)
##model assumptions
hist(chickmod$residuals)
qqnorm(chickmod$residuals)
qqline(chickmod$residuals)
length(chick$Chickenweight)
##sdeviation
sd(chick$Chickenweight)

##Q3.
melon<-read.table("ch01.txt",header=T)
str(melon)
variety<-as.factor(melon$VARIETY)
melmod<-lm(YIELDM~variety-1,data=melon)
summary(melmod)
anova(melmod)
bargraph.CI(variety,melon$YIELDM)


##Question 4
sex<-as.factor(melon$SEX)
flower<-lm(FLOWERS~sex,data=melon)
summary(flower)
anova(flower)
plot(sex,melon$FLOWERS, xlab="Sex of flowers",ylab="number of flowers")
bargraph.CI(sex,melon$FLOWERS, xlab="Sex of flowers",ylab="number of flowers")
axis(1,at=1:2,lab=c("males","females"))

##Worksheet Univariate 2
wei<-read.table("ch02.txt",header=T)
names(wei)
str(wei)

## Scatter plot Question 1.
plot(wei$WEIGHT,wei$FAT)
weimod<-lm(FAT~WEIGHT,data=wei)
summary(weimod)
anova(weimod)

x<-seq(20,100,0.1)
lines(x,0.021*x+26.89,col="red")

##Question 2
mod2<-lm(FLOWERS~DBH,data=wei)
summary(mod2)
anova(mod2)

boxplot(wei$DBH,wei$FLOWERS)
x<-seq(50,350,0.1)
lines(x,-481.16+4.51*x,col="blue")

###that slope is different from null hypothesis of 4 ##use s.e. of the slope
##slope (y)+,-(2*s.e.)
4.51+(2*0.4224)#lower mean

4.51-(2*0.4224)#upper mean

##worksheet LM3
droso<-read.table("Drosophila.txt",header=T)
str(droso)

plot(droso$LEGGRATE,droso$LLONGVTY)
drosomod<-lm(LLONGVTY~LEGGRATE,data=droso)
summary(drosomod)
anova(drosomod)
abline(drosomod)

drosomod2<-lm(LLONGVTY~LEGGRATE+LSIZE+LEGGRATE:LSIZE,data=droso)
summary(drosomod2)
Anova(drosomod2)

drosomod1<-lm(LLONGVTY~LEGGRATE+LSIZE,data=droso)
summary(drosomod1)
Anova(drosomod1)

plot(droso$LEGGRATE,droso$LLONGVTY)
## LONGVTY=x*size_slope + x*Eggrate_slope+C################################
x<-seq(min(droso$LEGGRATE),max(droso$LEGGRATE),0.01)
lines(x,-0.29*x+1.18*x+1.69)

##Question 2.
obes<-read.table("obesity.txt",header=T)
str(obes)

obesmod<-lm(FOREARM~HT,data=obes)
summary(obesmod)
anova(obesmod)

obesmod1<-lm(FOREARM~WT,data=obes)
summary(obesmod1)
anova(obesmod1)

obesmod2<-lm(FOREARM~WT+HT,data=obes)
summary(obesmod2)
Anova(obesmod2)
cor.test(obes$WT,obes$HT)

##Q3.
time<-read.table("AndraAtabDataBE2004.txt",header=T)
str(time)

patchmod<-lm(Time2~treat,data=time)
summary(patchmod)
anova(patchmod)

patchmod1<-lm(Time2~treat+Time1,data=time)
summary(patchmod1)
Anova(patchmod1)

##Worksheet Univariate 4 30/10/2015####Interactions
##Question 1.
flower<-read.table("ch01.txt",header=T)
str(flower)
sex<-as.factor(flower$SEX)
flowermod<-lm(FLOWERS~sex+DBH,data=flower)
summary(flowermod)
anova(flowermod)
plot(flower$DBH,flower$FLOWERS)

## fitting an interaction
flmod<-lm(FLOWERS~sex+DBH+sex:DBH,data=flower)
summary(flmod)
Anova(flmod)

plot(flower$DBH,flower$FLOWERS)
x<-seq(20,350,0.1)
lines(x,-440.7+3.54*x)
lines(x,-548.77+5.52*x,col="red")

##Question 2.
fatw<-read.table("ch07-WeightFatSex.txt",header=T)
str(fatw)
sexw<-as.factor(fatw$SEX)
##Does weight means Fat?
fatm<-lm(FAT~WEIGHT,data=fatw)
summary(fatm)
anova(fatm)##No

fatmd<-lm(FAT~WEIGHT+sexw+WEIGHT:sexw,data=fatw)
summary(fatmd)
Anova(fatmd)

plot(fatw$WEIGHT,fatw$FAT,xlab="Weight",ylab="Fat",las=1,main="Weight and Fat Relationship")
x<-seq(0,150,0.1)## alternative X<-seq(min(data$WEiGHT),max(data$WEIGHT),0.01)
lines(x,5.24+0.4*x,col="blue")##lines(data$WEIGHT),
lines(x,11.57+0.19*x,col="red")

##Question 3.
dro<-read.table("Drosophila.txt",header=T)
str(dro)
##Additive model
drom<-lm(LLONGVTY~LEGGRATE+LSIZE,data=dro)
summary(drom)
Anova(drom)

##multiplicative model
dromod<-lm(LLONGVTY~LEGGRATE*LSIZE,data=dro)
summary(dromod)
Anova(dromod)

##Question 4.
tulips<-read.table("ch07_Tulips.txt",header=T)
str(tulips)
names(tulips)
View(tulips)
bed<-as.factor(tulips$BED)
water<-as.factor(tulips$WATER)
shade<-as.factor(tulips$SHADE)
tulipmod<-lm(BLOOMS~bed+water+shade+water*shade,data=tulips)
summary(tulipmod)
Anova(tulipmod)#the effect of Water on flowers depends on shade regimes

##Bargraph for my model
bargraph.CI(water,BLOOMS,group=shade, data=tulips,
            legend=F, ylab="Blooms", xlab="Water treatment",
            lc=TRUE,uc=TRUE,cex.lab=1.2, cex.axis=1.2, cex.leg=1,
            las=1, col=c("blue","red","green"))

##Question 5.
patch<-read.table("AndraAtabDataBE2004.txt",header=T)
str(patch)

patchmod<-lm(LTime2~treat*LTime1*eggloadestim,data=patch)
summary(patchmod)
Anova(patchmod)
AIC(patchmod)

step(patchmod)###Step command for AIC model simplification

##MAM METHOD
patchmod1<-update(patchmod,~.-treat:LTime1:eggloadestim)
summary(patchmod1)
Anova(patchmod1)
AIC(patchmod1)

anova(patchmod1,patchmod)

patchmod2<-update(patchmod1,~.-LTime1:eggloadestim)
summary(patchmod2)
Anova(patchmod2)
AIC(patchmod2)

patchmod3<-update(patchmod1,~.-treat:eggloadestim)
summary(patchmod3)
Anova(patchmod3)
AIC(patchmod3)

patchmod4<-update(patchmod2,~.-treat:eggloadestim)
Anova(patchmod4)
anova(patchmod4,patchmod2)
AIC(patchmod4)

patchmod5<-update(patchmod4,~.-treat:LTime1)
summary(patchmod5)
Anova(patchmod5)
anova(patchmod5,patchmod4)
AIC(patchmod5)

patchmod6<-update(patchmod5,~.-eggloadestim)
summary(patchmod6)
Anova(patchmod6)######MAM
AIC(patchmod6)

patchmod7<-update(patchmod6,~.-treat)
Anova(patchmod7)
anova(patchmod6,patchmod7)

###Model simplification based on AIC
AIC(patchmod)
AIC(patchmod1)
AIC(patchmod2)
AIC(patchmod3)
AIC(patchmod4)
AIC(patchmod5)
AIC(patchmod6)
AIC(patchmod7)
AIC(patchmod8)

par(mfrow=c(2,2))
plot(patchmod6)
qqnorm(patchmod6$residuals)
qqline(patchmod6$residuals)

hist(patchmod6$residuals)

################Generalized Linear Model#####################
clutch<-read.table("LogLinearLogisitc_HardyEtAl_Data1.txt",header=T)##Question 1.
str(clutch)
View(clutch)

hist(clutch$Clutch_size)
mother_mating_status<-as.factor(clutch$Mothers_mating_status)

clutchmod<-glm(Clutch_size~Host_weight*mother_mating_status,family=quasipoisson(link="log"),data=clutch)
summary(clutchmod)
Anova(clutchmod,test="F")
AIC(clutchmod)

clutchmod1<-update(clutchmod,~.-Host_weight:mother_mating_status)
summary(clutchmod1)
Anova(clutchmod1)
AIC(clutchmod1)

anova(clutchmod,clutchmod1,test="Chi")

clutchmod2<-update(clutchmod1,~.-mother_mating_status)
summary(clutchmod2)
Anova(clutchmod2)###Best model using MAM
AIC(clutchmod2)
anova(clutchmod1,clutchmod2,test="Chi")

clutchmod3<-update(clutchmod1,~.-Host_weight)
Anova(clutchmod3,test="F")
anova(clutchmod1,clutchmod3,test="Chi")

###AIC method
AIC(clutchmod)
AIC(clutchmod1)##optimal model with AIC
AIC(clutchmod2)
AIC(clutchmod3)

step(clutchmod)##using step command

##scatter plot
plot(clutch$Host_weight,clutch$Clutch_size)
x<-seq(min(clutch$Host_weight),max(clutch$Host_weight),0.01)
lines(x,exp(1.39+0.027*x),col=3)

## Inspecting my residuals
par(mfrow=c(2,2))
plot(clutchmod2)

###1.4.########################### FITTED VALUES LINE COMMAND
clutchm<-glm(Clutch_size~Host_weight,family=poisson(link="log"),data=clutch)
summary(clutchm)
par(mfrow=c(1,1))
plot(clutch$Host_weight,clutch$Clutch_size)
x<-seq(min(clutch$Host_weight),max(clutch$Host_weight),0.01)
lines(clutch$Host_weight,clutchm$fitted.values,col="red")##Same as below
lines(x,exp(1.45+0.027*x),col="red")
abline(clutchm,col=("blue"))

###Question 2.
road<-read.table("RoadKills.txt",header=T)
str(road)
View(road)
hist(road$TOT.N)

rkillmod1<-glm(TOT.N~D.PARK,family=quasipoisson(link="log"),data=road)##correcting for ove
summary(rkillmod1)
Anova(rkillmod1,test="F")

plot(road$D.PARK,road$TOT.N)
x<-seq(min(road$D.PARK),max(road$D.PARK),0.01)
lines(x,exp(4.3-0.000106*x),col="blue")

###Question 3.1
egg<-read.table("egg_BI.txt",header=T)
View(egg)
hist(egg$load)
hist(egg$tot_load)

eggmod<-glm(load~age,family=quasipoisson(link="log"),data=egg)
summary(eggmod)

eggmod1<-glm(tot_load~age,family=quasipoisson(link="log"),data=egg)
summary(eggmod1)

##Q.3.2
R2<-(53919-26887)/53919
r2<-(29511-23318)/29511
r2
R2
##Q3.3
plot(egg$age,egg$tot_load)
x<-seq(min(egg$tot_load),max(egg$tot_load),0.01)
lines(x,exp(1.88+0.13*x),col="red")

####################Worksheet GLM Binomial Question 1.
dicta<-read.table("DictatorGameJUB.txt",header=T)
str(dicta)

failure<-(dicta$Totals-dicta$Money_Kept)
Y<-cbind(dicta$Money_Kept,failure)

dictamod<-glm(Y~TreatPlace*TreatAno,family=quasibinomial(link="logit"),data=dicta)
summary(dictamod)
Anova(dictamod,test="F")

dictamod1<-glm(Y~TreatPlace*TreatAno,family=binomial(link="logit"),data=dicta)
summary(dictamod1)
Anova(dictamod1,test="Chi")

dictamod2<-update(dictamod,~.-TreatPlace:TreatAno)
summary(dictamod2)
Anova(dictamod2,test="F")

dictamod3<-update(dictamod2,~.-TreatAno)
Anova(dictamod3,test="F")

dictamod4<-update(dictamod2,~.-TreatPlace)
Anova(dictamod4,test="F")

##Q1.2.#################using quasipoisson
dimod<-glm(Money_Kept~TreatPlace*TreatAno,family=quasipoisson(link="log"),data=dicta)
summary(dimod)
Anova(dimod,test="F")

dimod1<-update(dimod,~.-TreatPlace:TreatAno)
Anova(dimod1,test="F")

dimod2<-update(dimod1,~.-TreatAno)
Anova(dimod2,test="F")

#Q.1.3.
sdic<-glm(Fair~TreatPlace*TreatAno,family=quasibinomial(link=logit),data=dicta)##dont have to
summary(sdic)
Anova(sdic,test="F")

sdic1<-glm(Fair~TreatPlace*TreatAno,family=binomial(link=logit),data=dicta)
Anova(sdic1)

###AIC optimization
step(sdic1)

sdic2<-update(sdic1,~.-TreatPlace:TreatAno)
Anova(sdic2)

sdic3<-update(sdic2,~.-TreatAno)
Anova(sdic3)

##Question 2
pol<-read.table("polis.txt",header=T)
str(pol)
View(pol)
polmod<-glm(PA~RATIO,family=binomial(link="logit"),data=pol)
Anova(polmod)
summary(polmod)

plot(pol$RATIO,pol$PA)
x<-seq(min(pol$RATIO),max(pol$RATIO),0.01)
lines(x,1/(1+1/exp(3.60-0.2196*x)),lwd=2,col="red")

##Question 3.
aso<-read.table("Asobara_Irritability.txt",header=T)
str(aso)
View(aso)
asomod<-glm(Response~T_Irritat_prec,family=binomial(link="logit"),data=aso)
Anova(asomod)
summary(asomod)
#R squared
(679.15-534.86)/679.15

asomod1<-glm(Response~T_Irritat_prec*as.factor(Wasp2),family=binomial(link="logit"),data=aso)
Anova(asomod1)
(679.15-387.41)/679.15

asomod2<-glm(Response~T_Irritat_prec*Trial_No+as.factor(Wasp2),family=binomial(link="logit"),data=aso)
Anova(asomod2)##MAM
asomod3<-update(asomod2,.~.-T_Irritat_prec:Trial_No)
Anova(asomod3)

summary(asomod2)
###AIC
step(asomod2)

###Question 4.
para<-read.table("Katharina_DensDepParas.txt",header=T)
View(para)
str(para)
failure<-(para$Hosts-para$PAR)
Y<-cbind(para$PAR,failure)
parmod<-glm(Y~RateParas,family=quasibinomial(link="logit"),data=para)
Anova(parmod,test="F")
summary(parmod)

parmod1<-glm(Y~Hosts,family=quasibinomial(link="logit"),data=para)
Anova(parmod1,test="F")
summary(parmod1)

plot(para$Hosts,para$RateParas)
x<-seq(min(para$Hosts),max(para$Hosts),0.01)
lines(x,1/(1+1/exp(0.0604-0.00915*x)),lwd=2,col="red")

###Question 5
clutch<-read.table("BiClutch.txt",header=T)
names(clutch)
str(clutch)
View(clutch)

#fitting a model to my data not more than 2way interraction#### corectionssssssssssssss
#(variety), fruit ripeness (ripeness), fruit size(vol_ml), and egg load (load_tot)

clutchmod1<-glm(BinClutch~variety+load_tot+vol_ml+ripeness
                +variety:load_tot+variety:vol_ml+variety:ripeness
                +load_tot:vol_ml+load_tot:ripeness+vol_ml:ripeness
                ,family=binomial(link="logit"),data=clutch)######
summary(clutchmod1)
Anova(clutchmod1)

clutchmod2<-update(clutchmod1,~.-variety:load_tot)
Anova(clutchmod2)

clutchmod3<-update(clutchmod2,~.-load_tot:vol_ml)
Anova(clutchmod3)

clutchmod4<-update(clutchmod3,~.-variety:ripeness)
Anova(clutchmod4)

clutchmod5<-update(clutchmod4,~.-vol_ml:ripeness)
Anova(clutchmod5)

clutchmod6<-update(clutchmod5,~.-load_tot:ripeness)
Anova(clutchmod6)

clutchmod7<-update(clutchmod6,~.-variety:vol_ml)
Anova(clutchmod7)

clutchmod8<-update(clutchmod7,~.-variety)
Anova(clutchmod8)

clutchmod9<-update(clutchmod8,~.-vol_ml)
Anova(clutchmod9)##MAM using backwardstepwise elimination with capital anova
summary(clutchmod9)

clutchmod10<-glm(BinClutch~load_tot,family=binomial(link="logit"),data=clutch)
Anova(clutchmod10)
anova(clutchmod9,clutchmod10,test="Chi")

##Q5.2 AIC model
step(clutchmod1)## the models differ from MAM final model

plot(clutch$load_tot,clutch$BinClutch)###plots for 3 levels of ripeness
x<-seq(min(clutch$load_tot),max(clutch$load_tot),0.1)
lines(x,1/(1+1/exp(-1.829+0.042*x)),lwd=2,col="red")
lines(x,1/(1+1/exp(-2.479+0.042*x)),lwd=2,col="blue")
lines(x,1/(1+1/exp(-3.5271+0.042*x)),lwd=2,col="green")

par(mfrow=c(1,1))

### Worksheet for GEE and GLMs Q2.########################
andra<-read.table("AndraAtabDataBE2004.txt",header=T)
str(andra)
hist(andra$LTime2)
qqline(andra$LTime2)
timmod<-lm(LTime2~LTime1+treat,data=andra)
summary(timmod)
Anova(timmod)
plot(timmod)

timmod1<-glm(Time2~LTime1+treat,family=Gamma(link="inverse"),data=andra)
Anova(timmod1)
plot(timmod1)

timmod2<-glm(Time2~LTime1+treat,family=Gamma(link="log"),data=andra)
Anova(timmod2)
plot(timmod2)

timmod3<-glm(Time2~Time1+treat,family=Gamma(link="log"),data=andra)
Anova(timmod3)
plot(timmod3)
par(mfrow=c(2,2))

###Question 2.2.
para<-read.table("Paras_sexasex_ly.txt",header=T)
str(para)
View(para)
patc<-as.factor(para$patch)
mod1<-geeglm(patchtime~(patc+treatment+strain)^2,id=waspid,data=para,family=Gamma(link = "inverse"), corstr ="ar1")
anova(mod1,test="F")

##mod2 Drop treatment:strain
mod2<-update(mod1,.~.-treatment:strain)
anova(mod2,test="F")

hist(mod2$residuals)

qqnorm(mod1$residuals)
qqline(mod1$residuals)
qqnorm(residuals(mod1,"response"))
qqline(residuals(mod1,"response"))
step(mod1)

mod<-geeglm(n_ovis~(patc+treatment+strain)^2,id=waspid,data=para,family=poisson(link ="log"), corstr ="ar1")
anova(mod)

mo1<-update(mod,.~.-treatment:strain)
anova(mo1)

mo2<-update(mo1,.~.-patc:strain)
anova(mo2)
summary(mo2)

qqnorm(mod$residuals)
qqline(mod$residuals)
hist(mod$residuals)

par(mfrow=c(1,2))

###Question 3.
as<-read.table("Asobara_Irritability1.txt",header=T)
require(geepack)
str(as)
View(as)
was1<-as.factor(as$Wasp)
wasp2<-as.factor(as$Wasp2)
asmod<-geeglm(Response~T_Irritat_plan,id=wasp2,corstr="ar1",family=binomial(link="logit"),data=as)
anova(asmod)
summary(asmod)
par(mfrow=c(1,1))

plot(as$T_Irritat_plan,as$Response)
x<-seq(min(as$T_Irritat_plan),max(as$T_Irritat_plan),0.01)
lines(x,1/(1+(1/exp(-1.885+0.0030*x))),lwd=2,col="red")
min(as$T_Irritat_plan)
max(as$T_Irritat_plan)


####Scrips given for the worksheets from 6.11.2015

##EXAM EXCERSICES##15.11.2015
lice<-read.table("Lice.SIM.txt",header=T)
View(lice)
str(lice)
fpro_week<-(lice$Production_week)
hist(lice$Copepod)
qqnorm(lice$Copepod)
qqline(lice$Copepod)

lcopepod<-log(lice$Copepod)
hist(lcopepod)

### I opt for a Additive mixed effect model
licemod<-lm(Copepod~fpro_week*Station*Depth,data=lice)
summary(licemod)
Anova(licemod)

licemod<-glmer(Copepod~Depth+fpro_week + (1|Station),family=poisson(link="log"),data=lice)
summary(licemod)

licemod1<-glmer(Copepod~fpro_week + (1|Station),family=poisson(link="log"),data=lice)
summary(licemod1)
anova(licemod,licemod1)

licemod2<-glmer(Copepod~1 + (1|Station),family=poisson(link="log"),data=lice)
summary(licemod2)
anova(licemod1,licemod2)

##Model diagnostic tools
E<-resid(licemod,type="pearson")
Fit<-fitted(licemod)
op<-par(mfrow=c(1,2))
plot(x=Fit,y=E,xlab="Fitted values",ylab="Residuals",main="Residuals v Fitted")
identify(Fit,E)
hist(E,nclass=8)
hist(E, freq=F)
curve(expr=dnorm(x,mean=0,sd=sd(E)),add=T)


##Fitted values for the optimal model
boxplot(Copepod~Depth+fpro_week+Station,main="Model fit",data=lice)

##Normalized residuals v Depth
boxplot(E~1+Depth,random=~1+Depth|Station,main="Normalized v Depth",method="REML",data=lice)

(1.879629^2)/(1.879629^2+2.139525^2)##correlation btw copepod frq at d same stations




