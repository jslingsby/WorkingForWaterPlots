##############################################################################
######## Code to analyse Working for Water plots on the Cape Peninsula 
######## Data collected by Doug Euston-Brown, Susan Botha and Ryan Blanchard
##############################################################################
######## Compiled by Jasper Slingsby 2012-2014
######## Last edited: 4 March 2014
##############################################################################
######## 
###Steps:
###1) Get libraries and data
##############################################################################
########
###Notes:
###Want to analyse functional/compositional shifts... 
###- developing code to match traits (~line 58) - need to fix names!!!
##############################################################################

###Get libraries
library(gdata)
library(vegan)
library(picante)
library(MASS)
library(car)
library(MCMCglmm)

###Get data
#Get plot treatment info

treat<-read.xls("/Users/jasper/Documents/Dell/Jasper/Databases/Working for Water Peninsula plots/plot_treatments.xlsx", sheet=1, stringsAsFactors=FALSE)
ndvi=read.csv("/Users/jasper/Dropbox/SAEON/Projects/Cape Peninsula/WFW plots/plotNDVI.csv", stringsAsFactors=F)

#treat<-read.csv("/Users/jasper/Dropbox/SAEON/Projects/Cape Peninsula/WFW plots/plotNDVI.csv", stringsAsFactors=F)
#treat=treat[,c(5:8,26:28,32:34,4)]

#Get plant trait info
trait<-read.xls("/Users/jasper/Documents/Dell/Jasper/Databases/Working for Water Peninsula plots/Resampling Cape Peninsula/2002 Survey_Euston-Brown and Botha/Report/plants.xls", sheet=1, stringsAsFactors=FALSE)

#Get 1x1m subplot floristic data
dat2<-read.xls("/Users/jasper/Documents/Dell/Jasper/Databases/Working for Water Peninsula plots/Resampling Cape Peninsula/2008 Resurvey_Blanchard and Euston-Brown/2008 Resurvey data/1x1m 2000 plot data.xlsx", sheet=1, pattern="plot", stringsAsFactors=FALSE)

###Clean, fix and sort data [and fix funny values (non-numeric and non-integer)]
dat2<-dat2[,c(1,2,5,3)]
dat2[which(dat2[,3]=="0"),3]<-1
dat2[which(dat2[,3]=="0.500000000000000"),3]<-1
dat2[which(dat2[,3]=="*"),3]<-1
dat2[which(dat2[,3]=="&gt;15"),3]<-15
dat2[which(dat2[,3]=="&gt;25"),3]<-25
dat2[which(dat2[,3]==""),3]<-1
dat2[which(dat2[,3]=="s"),3]<-1
dat2[,3]<-as.numeric(dat2[,3])

dat8<-read.xls("/Users/jasper/Documents/Dell/Jasper/Databases/Working for Water Peninsula plots/Resampling Cape Peninsula/2008 Resurvey_Blanchard and Euston-Brown/2008 Resurvey data/meta data files 2008.xlsx", sheet=6, stringsAsFactors=FALSE)
dat8<-dat8[,c(2,4,7,5)]
dat8[which(dat8[,3]=="0.100000000000000"),3]<-1
dat8[which(dat8[,3]==""),3]<-1
dat8[which(dat8[,3]=="0"),3]<-1
dat8[which(dat8[,3]=="a"),3]<-1
dat8[,3]<-as.numeric(dat8[,3])

#Convert sample data into community data matrices
samp2<-sample2matrix(as.data.frame(dat2[,2:4]))
samp8<-sample2matrix(as.data.frame(dat8[,2:4]))

#################################################
###Development code
#################################################
##Match trait data to samp taxa
#length(which(colnames(samp2)%in%trait[,1]))
#length(which(colnames(samp8)%in%trait[,1]))
#colnames(samp8)[-which(colnames(samp8)%in%trait[,1])]

#Account for differences in density by calculating rarefied species richness by subplot (at 10 individuals)
spr2<-rarefy(samp2,sample=10)
spr8<-rarefy(samp8,10)

spd2<-rowSums(decostand(samp2,"pa"))
spd8<-rowSums(decostand(samp8,"pa"))

Ind2<-rowSums(samp2)
Ind8<-rowSums(samp8)

#Fix labels from each survey to match
pllab2<-substr(rownames(samp2),1,nchar(rownames(samp2))-1) #Plot numbers
splab2<-substr(rownames(samp2),nchar(rownames(samp2)),nchar(rownames(samp2))) #subplot letters
let<-c("a","b","c","d","e","f","g","h","i","j","k","l")
code<-c(1.1,1.2,2.3,2.4,3.5,3.6,4.7,4.8,5.9,"5.10",6.11,6.12)
for(i in 1:length(let)){splab2[which(splab2==let[i])]<-code[i]}
lab2<-paste(pllab2,splab2,sep=".")
names(spr2)<-lab2
names(spd2)<-lab2
names(Ind2)<-lab2

#Identify set of plots that match between surveys
comp<-intersect(names(spr2), names(spr8))

#Get plot numbers and match with treatment - make dataframes for further analyses
plt<-matrix(as.numeric(matrix(unlist(strsplit(comp,"\\.")),515,3,byrow=TRUE)[,1]))

treat<-treat[which(treat$Site%in%plt),]
treatL<-matrix(treat[which(treat$Site==plt[1]),1:4],1,4);colnames(treatL)<-colnames(treat[1:4])
for(i in 2:length(plt)){treatL<-rbind(treatL,treat[which(treat$Site==plt[i]),1:4])}
treatL<-cbind(plt,treatL)
colnames(treatL)[1]<-"Plot"

adat2<-cbind(treatL,spr2[comp],spd2[comp], rep("Young",dim(treatL)[1]), Ind2[comp])
adat8<-cbind(treatL,spr8[comp],spd8[comp], rep("Old",dim(treatL)[1]), Ind8[comp])
colnames(adat2)[6:9]<-c("Species_Richness", "Species_Density", "Survey", "No_Individuals")
colnames(adat8)[6:9]<-c("Species_Richness", "Species_Density", "Survey", "No_Individuals")
adat<-rbind(adat2,adat8)

adat[,1]<-as.factor(unlist(adat[,1]))
adat[,2]<-as.factor(unlist(adat[,2]))
adat[,3]<-as.factor(unlist(adat[,3]))
adat[,4]<-as.factor(unlist(adat[,4]))
adat[,5]<-as.factor(unlist(adat[,5]))
adat[,8]<-as.factor(unlist(adat[,8]))

TMS<-droplevels(adat[which(adat$Geology=="TMS"),])
Fynbos<-droplevels(adat[which(adat$Vegetation=="Fynbos"),])
Fynbos$Aliens<-relevel(Fynbos$Aliens,"No Aliens")

TMSfynbos<-droplevels(TMS[which(TMS$Vegetation=="Fynbos"),])
TMSfynbos$Aliens<-relevel(TMSfynbos$Aliens,"No Aliens")

TMSfynbos_old<-droplevels(TMSfynbos[which(TMSfynbos$Survey=="Old"),])

#Remove unnecessary objects
#rm(list=ls())

####################################################
###Examining Fynbos on Table Mountain Sandstone only
####################################################
attach(TMSfynbos)

#Examine difference in numbers of individuals per survey
png(file = "/Users/jasper/Dropbox/SAEON/Projects/Cape Peninsula/WFW plots/figures/num_indiv.png", width=9, height=5, units="in", res=300)
par(mfrow=c(1,2))
plot(Species_Density~No_Individuals, ylab="Species Density", xlab="Number of individuals")
boxplot(No_Individuals~Survey, xlab="Veld age", ylab="Number of individuals")
dev.off()

png(file = "/Users/jasper/Dropbox/SAEON/Projects/Cape Peninsula/WFW plots/figures/num_spp.png", width=9, height=5, units="in", res=300)
par(mfrow=c(1,2))
boxplot(Species_Density~Survey, xlab="Veld age", ylab="Species Density")
boxplot(Species_Richness~Survey, xlab="Veld age", ylab="Species Richness")
dev.off()

#Normality of response variables
hist(Species_Density)
hist(log(Species_Density))
hist(Species_Richness)

#Plot differences between treatments and response variables
par(mfrow=c(1,1))

levels(Survey)<-c("Y","O")

png(file = "/Users/jasper/Dropbox/SAEON/Projects/Cape Peninsula/WFW plots/figures/spp_dens.png", width=8, height=5, units="in", res=300)
boxplot(Species_Density~Survey/Aliens, main="Species density", ylab="Species Density", xlab="Treatment")
dev.off()

png(file = "/Users/jasper/Dropbox/SAEON/Projects/Cape Peninsula/WFW plots/figures/spp_rich.png", width=8, height=5, units="in", res=300)
boxplot(Species_Richness~Survey/Aliens, main="Species richness", ylab="Species Richness", xlab="Treatment")
dev.off()

png(file = "/Users/jasper/Dropbox/SAEON/Projects/Cape Peninsula/WFW plots/figures/ind_by_treatment.png", width=8, height=5, units="in", res=300)
boxplot(No_Individuals~Survey/Aliens, main="Number of individuals", xlab="Treatment", ylab="Number of individuals")
dev.off()

#Fit linear mixed-effects model (with plot as the random effect)
fitD1<-lme(Species_Density~Aliens*Survey, random=~1|Plot, method="ML")
fitS1<-lme(Species_Richness~Aliens*Survey, random=~1|Plot, method="ML")
summary(fitD1)
summary(fitS1)

#Check Assumptions
par(mfrow=c(1,2))
#Residuals normally distributed?
hist(fitD1$residuals, ylim=c(0,600))
hist(rnorm(length(fitD1$residuals), mean = 0, sd = 1), ylim=c(0,600))
shapiro.test(fitD1$residuals)
#Constant error variance?
plot(fitD1)

#For species richness?
hist(fitS1$residuals[,2])
shapiro.test(fitS1$residuals)
#plot(fitS1$residuals[,2])

detach(TMSfynbos)
 
### MCMCglmm models on "TMSfynbos"

prior<-list(R=list(V=1, nu=0.002), G=list(G1=list(V=1, nu=0.002)))

### LNC
MCmod1=MCMCglmm(Species_Density~Aliens*Survey, random=~Plot, data=TMSfynbos, family="poisson", prior=prior, verbose=TRUE, pr=TRUE)
summary(MCmod1)
MCmod2=MCMCglmm(No_Individuals~Aliens*Survey, random=~Plot, data=TMSfynbos, family="poisson", prior=prior, verbose=TRUE, pr=TRUE)
summary(MCmod2)
MCmod3=MCMCglmm(Species_Richness~Aliens*Survey, random=~Plot, data=Fynbos, family="gaussian", prior=prior, verbose=TRUE, pr=TRUE)
summary(MCmod3)

####################################################
###Examining Fynbos across 3 soil types (TMS, Granite, Sand)
####################################################
table(Fynbos[,c(3,5,8)])

attach(Fynbos)

levels(Survey)<-c("Y","O")

#Examine difference in numbers of individuals per survey
par(mfrow=c(1,1))
boxplot(No_Individuals~Survey)
boxplot(No_Individuals~Survey/Geology)
boxplot(Species_Density~Survey)
boxplot(Species_Density~Survey/Geology)
boxplot(Species_Richness~Survey)
boxplot(Species_Richness~Survey/Geology)

png(file = "/Users/jasper/Dropbox/SAEON/Projects/Cape Peninsula/WFW plots/figures/spdens_by_soilandtreatment.png", width=8, height=5, units="in", res=300)
boxplot(Species_Density~Survey/Geology, main="Species density", xlab="Treatment", ylab="Species density")
dev.off()

png(file = "/Users/jasper/Dropbox/SAEON/Projects/Cape Peninsula/WFW plots/figures/sprich_by_soilandtreatment.png", width=8, height=5, units="in", res=300)
boxplot(Species_Richness~Survey/Geology, main="Species richness", xlab="Treatment", ylab="Species richness")
dev.off()

png(file = "/Users/jasper/Dropbox/SAEON/Projects/Cape Peninsula/WFW plots/figures/No_ind_by_soilandtreatment.png", width=8, height=5, units="in", res=300)
boxplot(No_Individuals~Survey/Geology, main="Number of individuals", xlab="Treatment", ylab="Number of individuals")
dev.off()

levels(Aliens)=c("N", "C", "I")
levels(Geology)=c("G", "S", "T")

png(file = "/Users/jasper/Dropbox/SAEON/Projects/Cape Peninsula/WFW plots/figures/sprich_by_soilsurveyandtreatment.png", width=12, height=5, units="in", res=300)
boxplot(Species_Richness~Survey/Aliens/Geology, main="Species richness", xlab="Treatment", ylab="Species richness", cex.axis=.75)
dev.off()

png(file = "/Users/jasper/Dropbox/SAEON/Projects/Cape Peninsula/WFW plots/figures/spdens_by_soilsurveyandtreatment.png", width=12, height=5, units="in", res=300)
boxplot(Species_Density~Survey/Aliens/Geology, main="Species density", xlab="Treatment", ylab="Species density", cex.axis=.75)
dev.off()

png(file = "/Users/jasper/Dropbox/SAEON/Projects/Cape Peninsula/WFW plots/figures/No_ind_by_soilsurveyandtreatment.png", width=12, height=5, units="in", res=300)
boxplot(No_Individuals~Survey/Aliens/Geology, main="Number of individuals", xlab="Treatment", ylab="Number of individuals", cex.axis=.75)
dev.off()

#Normality of response variables
par(mfrow=c(1,2))
hist(Species_Density)
hist(log(Species_Density))
hist(Species_Richness)

#Plot differences between treatments and response variables
par(mfrow=c(1,2))
boxplot(Species_Density~Survey/Aliens, main="Species density")
boxplot(Species_Richness~Survey/Aliens, main="Species richness")

boxplot(Species_Density~Aliens/Geology, main="Species density")
boxplot(Species_Richness~Aliens/Geology, main="Species richness")

#Fit linear mixed-effects model (with plot as the random effect)
fitD1<-lme(Species_Density~Aliens*Survey, random=~1|Plot, method="ML")
fitS1<-lme(Species_Richness~Aliens*Survey, random=~1|Plot, method="ML")
summary(fitD1)
summary(fitS1)

#Fit linear mixed-effects model including geology
fitD2<-lme(log(Species_Density)~Aliens*Geology*Survey, random=~1|Plot, method="ML")
fitS2<-lme(Species_Richness~Aliens*Geology*Survey, random=~1|Plot, method="ML")
summary(fitD2)
summary(fitS2)

AIC(fitD1,fitD2)
AIC(fitS1,fitS2)

#Assumptions?
#Check Assumptions
par(mfrow=c(1,1))
#Residuals normally distributed?
hist(fitD2$residuals)
shapiro.test(fitD2$residuals)
#Constant error variance?
plot(fitD2)

#For species richness?
hist(fitS1$residuals[,2])
shapiro.test(fitS1$residuals)
plot(fitS1)

detach(Fynbos)

### MCMCglmm models on "Fynbos"

prior<-list(R=list(V=1, nu=0.002), G=list(G1=list(V=1, nu=0.002)))

### LNC
MCmod1=MCMCglmm(Species_Density~Aliens*Geology*Survey, random=~Plot, data=Fynbos, family="poisson", prior=prior, verbose=TRUE, pr=TRUE)
summary(MCmod1)
MCmod2=MCMCglmm(No_Individuals~Aliens*Geology*Survey, random=~Plot, data=Fynbos, family="poisson", prior=prior, verbose=TRUE, pr=TRUE)
summary(MCmod2)
MCmod3=MCMCglmm(Species_Richness~Aliens*Geology*Survey, random=~Plot, data=Fynbos, family="gaussian", prior=prior, verbose=TRUE, pr=TRUE)
summary(MCmod3)

############
###NDVI
############

png(file = "/Users/jasper/Dropbox/SAEON/Projects/Cape Peninsula/WFW plots/figures/ndvi2002_soil_treatment.png", width=12, height=5, units="in", res=300)
boxplot(ndvi_2002~Aliens/Geology, data=ndvi, main="NDVI 2002", xlab="Treatment", ylab="NDVI", cex.axis=.75)
dev.off()

png(file = "/Users/jasper/Dropbox/SAEON/Projects/Cape Peninsula/WFW plots/figures/ndvi2008_soil_treatment.png", width=12, height=5, units="in", res=300)
boxplot(ndvi_2008~Aliens/Geology, data=ndvi, main="NDVI 2008", xlab="Treatment", ylab="NDVI", cex.axis=.75)
dev.off()

### Linear models on "NDVI"
boxplot(ndvi_2002~Aliens, data=ndvi)
boxplot(ndvi_2002~Aliens/Geology, data=ndvi)
summary(lm(ndvi_2002~Aliens, data=ndvi))

### MCMCglmm models on "NDVI"

prior<-list(R=list(V=1, nu=0.002), G=list(G1=list(V=1, nu=0.002)))

### LNC
MCmod1=MCMCglmm(ndvi_2002~Aliens*Survey, random=~Plot, data=ndvi, family="gaussian", prior=prior, verbose=TRUE, pr=TRUE)
summary(MCmod1)
MCmod2=MCMCglmm(ndvi_2008~Aliens*Survey, random=~Plot, data=ndvi, family="gaussian", prior=prior, verbose=TRUE, pr=TRUE)
summary(MCmod2)

MCmod3=MCMCglmm(Species_Richness~Aliens*Survey, random=~Plot, data=ndvi, family="gaussian", prior=prior, verbose=TRUE, pr=TRUE)
summary(MCmod3)


#Attempts at finding some sort of regression coefficient...
#D-squared (Guisan & Zimmermann 2000) - the proportion of deviance explained by the fixed effects (over and above the random effects)
#D2=(Null deviance−Residual deviance)/Null deviance
#Guisan, A. & Zimmermann, N.E. (2000). Predictive habitat distribution models in ecology. Ecological Modelling 135: 147-186
#mean((MCmod3$Deviance-MCmod1$Deviance)/MCmod3$Deviance) 

###A gammy way of getting an idea of the R2 - NOT PUBLISHABLE!!!
summary(lm(log(mcdata$LNC)~predict(MCmod1, data=mcdata))) #Note that this is >D2 because it includes the random effects
summary(lm(log(mcdata$LNC)~predict(MCmod2, data=mcdata)))


    