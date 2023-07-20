#Master Code for White-Tailed Deer/ErM Project
#Kristy Ferraro
#Last Updated July 2023

#Setup----

library(lme4)
library(lmerTest)
library(arm)
library(ggplot2)
library(gridExtra)
library(ggrepel)
library(ggpubr)
library(dplyr)
library(tidyr)

r2.mixed<-function(mF){
  mFX<-model.matrix(mF)
  VarF <- var(as.vector(fixef(mF) %*% t(mFX)))
  VarR<-sum(as.numeric(VarCorr(mF))) 
  VarResid<-attr(VarCorr(mF), "sc")^2
  fR2<-VarF/(VarF + VarR + VarResid)
  rfR2<-(VarF + VarR)/(VarF + VarR + VarResid)
  list(fR2=fR2,rfR2=rfR2)
}

#..Plot Setup----
#make some plot themes 
Isotope.Theme<-theme(aspect.ratio = 1, panel.grid.minor = element_blank(), panel.grid.major = element_blank(), panel.background = element_rect(fill = "transparent", color=NA), plot.background = element_rect(fill = "transparent", color=NA),  axis.line = element_line(colour = "black"), plot.title = element_text(hjust = 0.5))
#legend.position = "none",

graph_points<- geom_point(aes(fill = Plant, color = Plant), size = 1, shape = 16, position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.7))
graph_text <-  geom_text(aes(label = Block), position = position_jitterdodge()) 
point_colors<-scale_color_manual(values = c("darkolivegreen3", "darkgreen"))
fill_colors<- scale_fill_manual(values=c("darkolivegreen3", "darkgreen"))

#..Data input----
setwd("~/My Drive/Scholarship/Yale/Projects/Parturition Projects/Myers Project/Data")
#Short Data
AllMyersData <- read.csv(file = 'Myers_All_Data.csv')
#Long Data

AllMyersData_NoControl<-subset(AllMyersData, Treatment!= "C")
AllMyersData_NoControl<-as.data.frame(AllMyersData_NoControl)
AllMyersData_NoControl$NForP<-as.factor(AllMyersData_NoControl$Placenta)
levels(AllMyersData_NoControl$NForP) <- c("NF", "P")


# = = = = = = = = = = = = = = = = = = = = = = = = = 
#BASELINE ANALYSIS ----
# = = = = = = = = = = = = = = = = = = = = = = = = = 
#Means for the PAper
AllMyersData_EcM<-subset(AllMyersData, Plant == "EcM")
AllMyersData_ErM<<-subset(AllMyersData, Plant!= "EcM")


mean(AllMyersData_EcM$Spring.Net.NO3)
mean(AllMyersData_ErM$Spring.Net.NO3)

mean(AllMyersData_EcM$Spring.Net.N.Min)
mean(AllMyersData_ErM$Spring.Net.N.Min)


#.Nutrient Cycles----
#...Net N Min -----
#Leaf % N - seem okay
hist(AllMyersData$Spring.Net.N.Min)
qqnorm(AllMyersData$Spring.Net.N.Min)
qqline(AllMyersData$Spring.Net.N.Min)

#NetN.baseline<- lmer(Spring.Net.N.Min ~ Natal.Fluid + ERM  +  (1|Block), data = AllMyersData)
NetN.plant<- lmer(Spring.Net.N.Min ~ ERM  + (1|Block), data = AllMyersData)
summary(NetN.plant)
r2.mixed(NetN.plant)
# = = = = = = = = = = = = = = = = = = = = = = = = = 
#...NO3 ----
hist(AllMyersData$Spring.Net.NO3)
qqnorm(AllMyersData$Spring.Net.NO3)
qqline(AllMyersData$Spring.Net.NO3)

#note some are neg, so create a new column and just add 1
AllMyersData$Spring.Net.NO3.Log<-AllMyersData$Spring.Net.NO3 + 1
hist(log(AllMyersData$Spring.Net.NO3.Log))
qqnorm(log(AllMyersData$Spring.Net.NO3.Log))
qqline(log(AllMyersData$Spring.Net.NO3.Log))

#this one we will log
NetN03.plant<- lmer(Spring.Net.NO3.Log ~ ERM  + (1|Block), data = AllMyersData)
NetN03.baseline<- lmer(Spring.Net.NO3.Log ~ Natal.Fluid + ERM  +  (1|Block), data = AllMyersData)
summary(NetN03.plant)
r2.mixed(NetN03.plant)

# = = = = = = = = = = = = = = = = = = = = = = = = = 
#...CMin ----
hist(AllMyersData$Spring.CMin.CO2.Hour)
qqnorm(AllMyersData$Spring.CMin.CO2.Hour)
qqline(AllMyersData$Spring.CMin.CO2.Hour)

CMin.plant<- lmer(Spring.CMin.CO2.Hour ~ ERM  + (1|Block), data = AllMyersData)
CMin.baseline<- lmer(Spring.CMin.CO2.Hour ~ Natal.Fluid + ERM  +  (1|Block), data = AllMyersData)
summary(CMin.plant)
r2.mixed(CMin.plant)


# = = = = = = = = = = = = = = = = = = = = = = = = = 
#...SIR ----
hist(AllMyersData$Spring.SIR.CO2.Hour)
qqnorm(AllMyersData$Spring.SIR.CO2.Hour)
qqline(AllMyersData$Spring.SIR.CO2.Hour)

SIR.plant<- lmer(Spring.SIR.CO2.Hour ~ ERM  + (1|Block), data = AllMyersData)
SIR.baseline<- lmer(Spring.SIR.CO2.Hour ~ Natal.Fluid + ERM  +  (1|Block), data = AllMyersData)
summary(SIR.plant)
r2.mixed(SIR.plant)


##.Nutrient Pools----
#This data I am just looking at whether or not plants were importnat drivers in the psring data
#...Leaf ========
hist(AllMyersData$Spring.Leaf.PrecN)
qqnorm(AllMyersData$Spring.Leaf.PrecN)
qqline(AllMyersData$Spring.Leaf.PrecN)
Spring.PrecN.Leaf<- lmer(Spring.Leaf.PrecN  ~  ERM + (1|Block), data = AllMyersData)
summary(Spring.PrecN.Leaf)
r2.mixed(Spring.PrecN.Leaf)


hist(AllMyersData$Spring.Leaf.PrecC)
qqnorm(AllMyersData$Spring.Leaf.PrecC)
qqline(AllMyersData$Spring.Leaf.PrecC)
Spring.PrecC.Leaf<- lmer(Spring.Leaf.PrecC  ~  ERM + (1|Block), data = AllMyersData)
summary(Spring.PrecC.Leaf)
r2.mixed(Spring.PrecC.Leaf)


hist(AllMyersData$Spring.Leaf.C.N)
qqnorm(AllMyersData$Spring.Leaf.C.N)
qqline(AllMyersData$Spring.Leaf.C.N)
Spring.CN.Leaf<- lmer(Spring.Leaf.C.N  ~  ERM + (1|Block), data = AllMyersData)
Spring.CN.Leaf.Log<- lmer(log(Spring.Leaf.C.N)  ~  ERM + (1|Block), data = AllMyersData)
summary(Spring.CN.Leaf)
r2.mixed(Spring.CN.Leaf)


#...Stem ========
hist(AllMyersData$Spring.Stem.PrecN)
qqnorm(AllMyersData$Spring.Stem.PrecN)
qqline(AllMyersData$Spring.Stem.PrecN)
Spring.PrecN.Stem<- lmer(Spring.Stem.PrecN  ~  ERM + (1|Block), data = AllMyersData)
summary(Spring.PrecN.Stem)
r2.mixed(Spring.PrecN.Stem)

hist(AllMyersData$Spring.Stem.PrecC)
qqnorm(AllMyersData$Spring.Stem.PrecC)
qqline(AllMyersData$Spring.Stem.PrecC)
Spring.PrecC.Stem<- lmer(Spring.Stem.PrecC  ~  ERM + (1|Block), data = AllMyersData)
summary(Spring.PrecC.Stem)
r2.mixed(Spring.PrecC.Stem)

hist(AllMyersData$Spring.Stem.C.N)
qqnorm(AllMyersData$Spring.Stem.C.N)
qqline(AllMyersData$Spring.Stem.C.N)
Spring.CN.Stem<- lmer(Spring.Stem.C.N  ~  ERM + (1|Block), data = AllMyersData)
Spring.CN.Stem.Log<- lmer(log(Spring.Stem.C.N)  ~  ERM + (1|Block), data = AllMyersData)
summary(Spring.CN.Stem.Log)
r2.mixed(Spring.CN.Stem.Log)

#...Coarse Root========
hist(AllMyersData$Spring.CoarseRoot.PrecN)
qqnorm(AllMyersData$Spring.CoarseRoot.PrecN)
qqline(AllMyersData$Spring.CoarseRoot.PrecN)
Spring.PrecN.CoarseRoot<- lmer(Spring.CoarseRoot.PrecN  ~  ERM + (1|Block), data = AllMyersData)
summary(Spring.PrecN.CoarseRoot)
r2.mixed(Spring.PrecN.CoarseRoot)

hist(AllMyersData$Spring.CoarseRoot.PrecC)
qqnorm(AllMyersData$Spring.CoarseRoot.PrecC)
qqline(AllMyersData$Spring.CoarseRoot.PrecC)
Spring.PrecC.CoarseRoot<- lmer(Spring.CoarseRoot.PrecC  ~  ERM + (1|Block), data = AllMyersData)
summary(Spring.PrecC.CoarseRoot)
r2.mixed(Spring.PrecC.CoarseRoot)

hist(AllMyersData$Spring.CoarseRoot.C.N)
qqnorm(AllMyersData$Spring.CoarseRoot.C.N)
qqline(AllMyersData$Spring.CoarseRoot.C.N)
Spring.CN.CoarseRoot<- lmer(Spring.CoarseRoot.C.N  ~  ERM + (1|Block), data = AllMyersData)
Spring.CN.CoarseRoot.Log<- lmer(log(Spring.CoarseRoot.C.N)  ~  ERM + (1|Block), data = AllMyersData)
summary(Spring.CN.CoarseRoot.Log)
r2.mixed(Spring.CN.CoarseRoot.Log)


#...Fine Root========
hist(AllMyersData$Spring.FineRoot.PrecN)
qqnorm(AllMyersData$Spring.FineRoot.PrecN)
qqline(AllMyersData$Spring.FineRoot.PrecN)
Spring.PrecN.FineRoot<- lmer(Spring.FineRoot.PrecN  ~  ERM + (1|Block), data = AllMyersData)
summary(Spring.PrecN.FineRoot)
r2.mixed(Spring.PrecN.FineRoot)

hist(AllMyersData$Spring.FineRoot.PrecC)
qqnorm(AllMyersData$Spring.FineRoot.PrecC)
qqline(AllMyersData$Spring.FineRoot.PrecC)
Spring.PrecC.FineRoot<- lmer(Spring.FineRoot.PrecC  ~  ERM + (1|Block), data = AllMyersData)
summary(Spring.PrecC.FineRoot)
r2.mixed(Spring.PrecC.FineRoot)

hist(AllMyersData$Spring.FineRoot.C.N)
qqnorm(AllMyersData$Spring.FineRoot.C.N)
qqline(AllMyersData$Spring.FineRoot.C.N)
Spring.CN.FineRoot<- lmer(Spring.FineRoot.C.N  ~  ERM + (1|Block), data = AllMyersData)
Spring.CN.FineRoot.Log<- lmer(log(Spring.FineRoot.C.N)  ~  ERM + (1|Block), data = AllMyersData)
summary(Spring.CN.FineRoot.Log)
r2.mixed(Spring.CN.FineRoot.Log)


#...POM========
hist(AllMyersData$Spring.POM.PrecN)
qqnorm(AllMyersData$Spring.POM.PrecN)
qqline(AllMyersData$Spring.POM.PrecN)
Spring.PrecN.POM<- lmer(Spring.POM.PrecN  ~  ERM + (1|Block), data = AllMyersData)
summary(Spring.PrecN.POM)
r2.mixed(Spring.PrecN.POM)

hist(AllMyersData$Spring.POM.PrecC)
qqnorm(AllMyersData$Spring.POM.PrecC)
qqline(AllMyersData$Spring.POM.PrecC)
Spring.PrecC.POM<- lmer(Spring.POM.PrecC  ~  ERM + (1|Block), data = AllMyersData)
summary(Spring.PrecC.POM)
r2.mixed(Spring.PrecC.POM)

hist(AllMyersData$Spring.POM.C.N)
qqnorm(AllMyersData$Spring.POM.C.N)
qqline(AllMyersData$Spring.POM.C.N)
Spring.CN.POM<- lmer(Spring.POM.C.N  ~  ERM + (1|Block), data = AllMyersData)
summary(Spring.CN.POM)
r2.mixed(Spring.CN.POM)



#...MAOM========
hist(AllMyersData$Spring.MAOM.PrecN)
qqnorm(AllMyersData$Spring.MAOM.PrecN)
qqline(AllMyersData$Spring.MAOM.PrecN)
Spring.PrecN.MAOM<- lmer(Spring.MAOM.PrecN  ~  ERM + (1|Block), data = AllMyersData)
summary(Spring.PrecN.MAOM)
r2.mixed(Spring.PrecN.MAOM)

hist(AllMyersData$Spring.MAOM.PrecC)
qqnorm(AllMyersData$Spring.MAOM.PrecC)
qqline(AllMyersData$Spring.MAOM.PrecC)
Spring.PrecC.MAOM<- lmer(Spring.MAOM.PrecC  ~  ERM + (1|Block), data = AllMyersData)
summary(Spring.PrecC.MAOM)
r2.mixed(Spring.PrecC.MAOM)

hist(AllMyersData$Spring.MAOM.C.N)
qqnorm(AllMyersData$Spring.MAOM.C.N)
qqline(AllMyersData$Spring.MAOM.C.N)
Spring.CN.MAOM<- lmer(Spring.MAOM.C.N  ~  ERM + (1|Block), data = AllMyersData)
Spring.CN.MAOM.Log<- lmer(log(Spring.MAOM.C.N)  ~  ERM + (1|Block), data = AllMyersData)
summary(Spring.CN.MAOM.Log)
r2.mixed(Spring.CN.MAOM.Log)


#...Soil Stocks========
AllMyersData$Baseline.C.Stock<-AllMyersData$Spring.POM.C.Stock.g.m2 + AllMyersData$Spring.MAOM.C.Stock.g.m2 
AllMyersData$Baseline.N.Stock<-AllMyersData$Spring.POM.N.Stock.g.m2 + AllMyersData$Spring.MAOM.N.Stock.g.m2

Spring.C.Stock<- lmer(Baseline.C.Stock  ~  ERM + (1|Block), data = AllMyersData)
summary(Spring.C.Stock)
Spring.N.Stock<- lmer(Baseline.N.Stock  ~  ERM + (1|Block), data = AllMyersData)
summary(Spring.N.Stock)




# = = = = = = = = = = = = = = = = = = = = = = = = = 
#FALL ANALYSIS NF v P ----
# = = = = = = = = = = = = = = = = = = = = = = = = = 
AllMyersData_NoControl<-subset(AllMyersData, Treatment!= "C")
AllMyersData_NoControl<-as.data.frame(AllMyersData_NoControl)
AllMyersData_NoControl$NForP<-as.factor(AllMyersData_NoControl$Placenta)
levels(AllMyersData_NoControl$NForP) <- c("NF", "P")

#.Nutrient Cycles----
#..Net N Min ----
hist(AllMyersData_NoControl$Fall.Net.N.Min)
qqnorm(AllMyersData_NoControl$Fall.Net.N.Min)
qqline(AllMyersData_NoControl$Fall.Net.N.Min)
P.NetN<- lmer(Fall.Net.N.Min~  Placenta + ERM + Placenta*ERM +(1|Block), data = AllMyersData_NoControl)
summary(P.NetN)
r2.mixed(P.NetN)

P.NetN.Simple<- lm(Fall.Net.N.Min~  Placenta + ERM + Placenta*ERM, data = AllMyersData_NoControl)
summary(P.NetN.Simple)


#...NO3 ----
hist(AllMyersData_NoControl$Fall.Net.NO3)
qqnorm(AllMyersData_NoControl$Fall.Net.NO3)
qqline(AllMyersData_NoControl$Fall.Net.NO3)
#try logging
hist(AllMyersData_NoControl$Fall.Net.NO3.Log)
qqnorm(AllMyersData_NoControl$Fall.Net.NO3.Log)
qqline(AllMyersData_NoControl$Fall.Net.NO3.Log)
#better, use log
P.NO3<- lmer(log(Fall.Net.NO3) ~  Placenta + ERM + Placenta*ERM +(1|Block), data = AllMyersData_NoControl)
summary(P.NO3)
r2.mixed(P.NO3)

P.NO3.Simple<- lm(log(Fall.Net.NO3) ~  Placenta + ERM + Placenta*ERM, data = AllMyersData_NoControl)
summary(P.NO3.Simple)



#...CMin ----
hist(AllMyersData_NoControl$Fall.CMin.CO2.Hour)
qqnorm(AllMyersData_NoControl$Fall.CMin.CO2.Hour)
qqline(AllMyersData_NoControl$Fall.CMin.CO2.Hour)
P.CMin<- lmer(Fall.CMin.CO2.Hour ~  Placenta + ERM + Placenta*ERM +(1|Block), data = AllMyersData_NoControl)
summary(P.CMin)
r2.mixed(P.CMin)

P.CMin.Simple<- lm(Fall.CMin.CO2.Hour ~  Placenta + ERM + Placenta*ERM, data = AllMyersData_NoControl)
summary(P.CMin.Simple)


#...SIR ----
hist(AllMyersData_NoControl$Fall.SIR.CO2.Hour)
qqnorm(AllMyersData_NoControl$Fall.SIR.CO2.Hour)
qqline(AllMyersData_NoControl$Fall.SIR.CO2.Hour)
hist(AllMyersData_NoControl$Fall.SIR.Log)
qqnorm(AllMyersData_NoControl$Fall.SIR.Log)
qqline(AllMyersData_NoControl$Fall.SIR.Log)
#not much better, lets stick with the original data 
P.SIR<- lmer(Fall.SIR.CO2.Hour ~  Placenta + ERM + Placenta*ERM +(1|Block), data = AllMyersData_NoControl)
summary(P.SIR)
r2.mixed(P.SIR)

P.SIR.Simple<- lm(Fall.SIR.CO2.Hour ~  Placenta + ERM + Placenta*ERM, data = AllMyersData_NoControl)
summary(P.SIR.Simple)

##.Nutrient Pools----
#...Leaf----
PrecN.Leaf.Placenta<- lmer(Fall.Leaf.PrecN  ~  Placenta + ERM + Placenta*ERM +(1|Block), data = AllMyersData_NoControl)
summary(PrecN.Leaf.Placenta)
r2.mixed(PrecN.Leaf.Placenta)

PrecC.Leaf.Placenta<- lmer(Fall.Leaf.PrecC ~  Placenta + ERM + Placenta*ERM +(1|Block), data = AllMyersData_NoControl)
summary(PrecC.Leaf.Placenta)
r2.mixed(PrecC.Leaf.Placenta)

CN.Leaf.Placenta<- lmer(Fall.Leaf.C.N  ~  Placenta + ERM + Placenta*ERM +(1|Block), data = AllMyersData_NoControl)
CN.Leaf.Placenta.Log<- lmer(log(Fall.Leaf.C.N)  ~  Placenta + ERM + Placenta*ERM +(1|Block), data = AllMyersData_NoControl)
summary(CN.Leaf.Placenta.Log)
r2.mixed(CN.Leaf.Placenta)

MassNRec.Leaf.Placenta<- lmer(Mass.Solution.Rec.Leaf ~  Placenta + ERM + Placenta*ERM +(1|Block), data = AllMyersData_NoControl)
summary(MassNRec.Leaf.Placenta)
r2.mixed(MassNRec.Leaf.Placenta)

PrecNRec.Leaf.Placenta<- lmer(Prec.Solution.Rec.Leaf ~ Placenta + ERM + Placenta*ERM +(1|Block), data = AllMyersData_NoControl)
summary(PrecNRec.Leaf.Placenta)
r2.mixed(PrecNRec.Leaf.Placenta)


#...Stem----
PrecN.Stem.Placenta<- lmer(Fall.Stem.PrecN ~ Placenta + ERM + Placenta*ERM +(1|Block), data = AllMyersData_NoControl)
summary(PrecN.Stem.Placenta)
r2.mixed(PrecN.Stem.Placenta)


#NOTE THERE IS AN INTERACTION HERE
PrecC.Stem.Placenta<- lmer(Fall.Stem.PrecC ~ Placenta + ERM + Placenta*ERM +(1|Block), data = AllMyersData_NoControl)
summary(PrecC.Stem.Placenta)
r2.mixed(PrecC.Stem.Placenta)

CN.Stem.Placenta<- lmer(Fall.Stem.C.N ~ Placenta + ERM + Placenta*ERM +(1|Block), data = AllMyersData_NoControl)
CN.Stem.Placenta.Log<- lmer(log(Fall.Stem.C.N) ~ Placenta + ERM + Placenta*ERM +(1|Block), data = AllMyersData_NoControl)
summary(CN.Stem.Placenta)
r2.mixed(CN.Stem.Placenta)

MassNRec.Stem.Placenta<- lmer(Mass.Solution.Rec.Stem ~ Placenta + ERM + Placenta*ERM +(1|Block), data = AllMyersData_NoControl)
summary(MassNRec.Stem.Placenta)
r2.mixed(MassNRec.Stem.Placenta)

PrecNRec.Stem.Placenta<- lmer(Prec.Solution.Rec.Stem ~ Placenta + ERM + Placenta*ERM +(1|Block), data = AllMyersData_NoControl)
summary(PrecNRec.Stem.Placenta)
r2.mixed(PrecNRec.Stem.Placenta)

#...Coarse Root----
PrecN.CoarseRoot.Placenta<- lmer(Fall.CoarseRoot.PrecN ~ Placenta + ERM + Placenta*ERM +(1|Block), data = AllMyersData_NoControl)
summary(PrecN.CoarseRoot.Placenta)
r2.mixed(PrecN.CoarseRoot.Placenta)

PrecC.CoarseRoot.Placenta<- lmer(Fall.CoarseRoot.PrecC ~ Placenta + ERM + Placenta*ERM +(1|Block), data = AllMyersData_NoControl)
summary(PrecC.CoarseRoot.Placenta)
r2.mixed(PrecC.CoarseRoot.Placenta)

CN.CoarseRoot.Placenta<- lmer(Fall.CoarseRoot.C.N ~ Placenta + ERM + Placenta*ERM +(1|Block), data = AllMyersData_NoControl)
CN.CoarseRoot.Placenta.Log<- lmer(log(Fall.CoarseRoot.C.N) ~ Placenta + ERM + Placenta*ERM +(1|Block), data = AllMyersData_NoControl)
summary(CN.CoarseRoot.Placenta)
r2.mixed(CN.CoarseRoot.Placenta)

MassNRec.CoarseRoot.Placenta<- lmer(Mass.Solution.Rec.CoarseRoot ~ Placenta + ERM + Placenta*ERM +(1|Block), data = AllMyersData_NoControl)
summary(MassNRec.CoarseRoot.Placenta)
r2.mixed(MassNRec.CoarseRoot.Placenta)

PrecNRec.CoarseRoot.Placenta<- lmer(Prec.Solution.Rec.CoarseRoot ~ Placenta + ERM + Placenta*ERM +(1|Block), data = AllMyersData_NoControl)
summary(PrecNRec.CoarseRoot.Placenta)
r2.mixed(PrecNRec.CoarseRoot.Placenta)


#...Fine Root----
PrecN.FineRoot.Placenta<- lmer(Fall.FineRoot.PrecN ~ Placenta + ERM + Placenta*ERM +(1|Block), data = AllMyersData_NoControl)
summary(PrecN.CoarseRoot.Placenta)
r2.mixed(PrecN.CoarseRoot.Placenta)

PrecC.FineRoot.Placenta<- lmer(Fall.FineRoot.PrecC ~ Placenta + ERM + Placenta*ERM +(1|Block), data = AllMyersData_NoControl)
summary(PrecC.FineRoot.Placenta)
r2.mixed(PrecC.FineRoot.Placenta)

CN.FineRoot.Placenta<- lmer(Fall.FineRoot.C.N ~ Placenta + ERM + Placenta*ERM +(1|Block), data = AllMyersData_NoControl)
CN.FineRoot.Placenta.Log<- lmer(log(Fall.FineRoot.C.N) ~ Placenta + ERM + Placenta*ERM +(1|Block), data = AllMyersData_NoControl)
summary(CN.FineRoot.Placenta)
r2.mixed(CN.FineRoot.Placenta)

MassNRec.FineRoot.Placenta<- lmer(Mass.Solution.Rec.FineRoot ~ Placenta + ERM + Placenta*ERM +(1|Block), data = AllMyersData_NoControl)
summary(MassNRec.FineRoot.Placenta)
r2.mixed(MassNRec.FineRoot.Placenta)

PrecNRec.FineRoot.Placenta<- lmer(Prec.Solution.Rec.FineRoot ~ Placenta + ERM + Placenta*ERM +(1|Block), data = AllMyersData_NoControl)
summary(PrecNRec.FineRoot.Placenta)
r2.mixed(PrecNRec.FineRoot.Placenta)



#...POM----
#has an interaction
MassNRec.POM.Placenta<- lmer(Mass.Solution.Rec.POM  ~ Placenta + ERM + Placenta*ERM +(1|Block), data = AllMyersData_NoControl)
summary(MassNRec.POM.Placenta)
r2.mixed(MassNRec.POM.Placenta)

#has an interaction
PrecNRec.POM.Placenta<- lmer(Prec.Solution.Rec.POM  ~ Placenta + ERM + Placenta*ERM +(1|Block), data = AllMyersData_NoControl)
summary(PrecNRec.POM.Placenta)
r2.mixed(PrecNRec.POM.Placenta)


#...MAOM----
#Significantly negative in placentas
MassNRec.MAOM.Placenta<- lmer(Mass.Solution.Rec.MAOM ~ Placenta + ERM + Placenta*ERM +(1|Block), data = AllMyersData_NoControl)
summary(MassNRec.MAOM.Placenta)
r2.mixed(MassNRec.MAOM.Placenta)


#Significantly negative in placentas
PrecNRec.MAOM.Placenta<- lmer(Prec.Solution.Rec.MAOM ~ Placenta + ERM + Placenta*ERM +(1|Block), data = AllMyersData_NoControl)
summary(PrecNRec.MAOM.Placenta)
r2.mixed(PrecNRec.MAOM.Placenta)

#.Ecosystem----
#...Mass N Recov----
P.Total.Mass.N.Rec.Model<- lmer(Total.Eco.N.Recovery ~ Placenta + ERM + Placenta*ERM + (1 | Block), data = AllMyersData_NoControl)
summary(P.Total.Mass.N.Rec.Model)
r2.mixed(P.Total.Mass.N.Rec.Model)

#Soil only
AllMyersData_NoControl$Mass.Solution.Rec.Soil<-AllMyersData_NoControl$Mass.Solution.Rec.POM + AllMyersData_NoControl$Mass.Solution.Rec.MAOM
P.Total.Soil.Mass.N.Rec.Model<- lmer(Mass.Solution.Rec.Soil ~ Placenta + ERM + Placenta*ERM + (1 | Block), data = AllMyersData_NoControl)
summary(P.Total.Soil.Mass.N.Rec.Model)
r2.mixed(P.Total.Soil.Mass.N.Rec.Model)

#Aboveground 
AllMyersData_NoControl$Mass.Solution.Rec.Aboveground<-AllMyersData_NoControl$Mass.Solution.Rec.Leaf + AllMyersData_NoControl$Mass.Solution.Rec.Stem
P.Total.Aboveground.Mass.N.Rec.Model<- lmer(Mass.Solution.Rec.Aboveground ~ Placenta + ERM + Placenta*ERM + (1 | Block), data = AllMyersData_NoControl)
summary(P.Total.Aboveground.Mass.N.Rec.Model)
r2.mixed(P.Total.Aboveground.Mass.N.Rec.Model)

#All plant 
AllMyersData_NoControl$Mass.Solution.Rec.Plant<-AllMyersData_NoControl$Mass.Solution.Rec.Aboveground + AllMyersData_NoControl$Mass.Solution.Rec.FineRoot + AllMyersData_NoControl$Mass.Solution.Rec.CoarseRoot
P.Total.Plant.Mass.N.Rec.Model<- lmer(Mass.Solution.Rec.Plant ~ Placenta + ERM + Placenta*ERM + (1 | Block), data = AllMyersData_NoControl)
summary(P.Total.Plant.Mass.N.Rec.Model)
r2.mixed(P.Total.Plant.Mass.N.Rec.Model)

#...Prec N Recov----
#Soil only
AllMyersData_NoControl$Prec.Solution.Rec.Soil<-AllMyersData_NoControl$Prec.Solution.Rec.POM + AllMyersData_NoControl$Prec.Solution.Rec.MAOM
P.Total.Soil.Prec.N.Rec.Model<- lmer(Prec.Solution.Rec.Soil ~ Placenta + ERM + Placenta*ERM + (1 | Block), data = AllMyersData_NoControl)
summary(P.Total.Soil.Prec.N.Rec.Model)
r2.mixed(P.Total.Soil.Prec.N.Rec.Model)

#Aboveground 
AllMyersData_NoControl$Prec.Solution.Rec.Aboveground<-AllMyersData_NoControl$Prec.Solution.Rec.Leaf + AllMyersData_NoControl$Mass.Solution.Rec.Stem
P.Total.Aboveground.Prec.N.Rec.Model<- lmer(Prec.Solution.Rec.Aboveground ~ Placenta + ERM + Placenta*ERM + (1 | Block), data = AllMyersData_NoControl)
summary(P.Total.Aboveground.Prec.N.Rec.Model)
r2.mixed(P.Total.Aboveground.Prec.N.Rec.Model)

#All plant 
AllMyersData_NoControl$Prec.Solution.Rec.Plant<-AllMyersData_NoControl$Prec.Solution.Rec.Aboveground + AllMyersData_NoControl$Mass.Solution.Rec.FineRoot + AllMyersData_NoControl$Prec.Solution.Rec.CoarseRoot
P.Total.Plant.Prec.N.Rec.Model<- lmer(Prec.Solution.Rec.Plant ~ Placenta + ERM + Placenta*ERM + (1 | Block), data = AllMyersData_NoControl)
summary(P.Total.Plant.Prec.N.Rec.Model)
r2.mixed(P.Total.Plant.Prec.N.Rec.Model)


Total.Prec.N.Rec.Model<- lmer(Prec.Eco.N.Recovery ~ Natal.Fluid + ERM + Natal.Fluid*ERM + (1 | Block), data = AllMyersData)
summary(Total.Prec.N.Rec.Model)
r2.mixed(Total.Prec.N.Rec.Model)



# = = = = = = = = = = = = = = = = = = = = = = = = = 
#FALL ANALYSIS NF+P ----
# = = = = = = = = = = = = = = = = = = = = = = = = = 
#.Nutrient Cycles----

#...Net N Min -----
hist(AllMyersData$Fall.Net.N.Min)
qqnorm(AllMyersData$Fall.Net.N.Min)
qqline(AllMyersData$Fall.Net.N.Min)
NetN<- lmer(Fall.Net.N.Min ~ Natal.Fluid + ERM  +  Natal.Fluid*ERM + (1|Block), data = AllMyersData)
summary(NetN)
r2.mixed(NetN)

NetN.Simple<- lm(Fall.Net.N.Min ~ Natal.Fluid + ERM  +  Natal.Fluid*ERM, data = AllMyersData)
summary(NetN.Simple)

NetN.Fixed<- lm(Fall.Net.N.Min ~ Natal.Fluid + ERM  +  Natal.Fluid*ERM + Block, data = AllMyersData)
summary(NetN.Fixed)


# = = = = = = = = = = = = = = = = = = = = = = = = = 
#...NO3 ----
hist(AllMyersData$Fall.Net.NO3)
qqnorm(AllMyersData$Fall.Net.NO3)
qqline(AllMyersData$Fall.Net.NO3)

hist(log(AllMyersData$Fall.Net.NO3))
qqnorm(log(AllMyersData$Fall.Net.NO3))
qqline(log(AllMyersData$Fall.Net.NO3))

#this one we will log
NetN03<- lmer(log(Fall.Net.NO3)  ~ Natal.Fluid + ERM  +  Natal.Fluid*ERM + (1|Block), data = AllMyersData)
summary(NetN03)
r2.mixed(NetN03)


NetN03.Simple<- lm(log(Fall.Net.NO3)  ~ Natal.Fluid + ERM  +  Natal.Fluid*ERM , data = AllMyersData)
summary(NetN03.Simple)

NetN03.Fixed<- lm(log(Fall.Net.NO3) ~ Natal.Fluid + ERM  +  Natal.Fluid*ERM + Block, data = AllMyersData)
summary(NetN03.Fixed)

# = = = = = = = = = = = = = = = = = = = = = = = = = 
#...CMin ----

hist(AllMyersData$Fall.CMin.CO2.Hour)
qqnorm(AllMyersData$Fall.CMin.CO2.Hour)
qqline(AllMyersData$Fall.CMin.CO2.Hour)
CMin<- lmer(Fall.CMin.CO2.Hour~ Natal.Fluid + ERM  +  Natal.Fluid*ERM + (1|Block), data = AllMyersData)
summary(CMin)
r2.mixed(CMin)

CMin.Simple<- lm(Fall.CMin.CO2.Hour~ Natal.Fluid + ERM  +  Natal.Fluid*ERM, data = AllMyersData)
summary(CMin.Simple)

CMin.Fixed<- lm(Fall.CMin.CO2.Hour ~ Natal.Fluid + ERM  +  Natal.Fluid*ERM + Block, data = AllMyersData)
summary(CMin.Fixed)


# = = = = = = = = = = = = = = = = = = = = = = = = = 
#...SIR ----
hist(AllMyersData$Fall.SIR.CO2.Hour)
qqnorm(AllMyersData$Fall.SIR.CO2.Hour)
qqline(AllMyersData$Fall.SIR.CO2.Hour)
SIR<- lmer(Fall.SIR.CO2.Hour ~ Natal.Fluid + ERM  +  Natal.Fluid*ERM + (1|Block), data = AllMyersData)
summary(SIR)
r2.mixed(SIR)

SIR.Simple<- lm(Fall.SIR.CO2.Hour ~ Natal.Fluid + ERM  +  Natal.Fluid*ERM, data = AllMyersData)
summary(SIR.Simple)

SIR.Fixed<- lm(Fall.SIR.CO2.Hour ~ Natal.Fluid + ERM  +  Natal.Fluid*ERM + Block, data = AllMyersData)
summary(SIR.Fixed)


##.Nutrient Pools----
#...Leaf----

hist(AllMyersData$Fall.Leaf.PrecN)
qqnorm(AllMyersData$Fall.Leaf.PrecN)
qqline(AllMyersData$Fall.Leaf.PrecN)
PrecN.Leaf.Model<- lmer(Fall.Leaf.PrecN ~ Natal.Fluid + ERM + Natal.Fluid*ERM + (1 | Block), data = AllMyersData)
summary(PrecN.Leaf.Model)
r2.mixed(PrecN.Leaf.Model)

PrecN.Leaf.Model.Simple<- lm(Fall.Leaf.PrecN ~ Natal.Fluid + ERM + Natal.Fluid*ERM, data = AllMyersData)
summary(PrecN.Leaf.Model.Simple)

hist(AllMyersData$Fall.Leaf.PrecC)
qqnorm(AllMyersData$Fall.Leaf.PrecC)
qqline(AllMyersData$Fall.Leaf.PrecC)
PrecC.Leaf.Model<- lmer(Fall.Leaf.PrecC ~ Natal.Fluid + ERM + Natal.Fluid*ERM + (1 | Block), data = AllMyersData)
summary(PrecC.Leaf.Model)
r2.mixed(PrecC.Leaf.Model)


hist(AllMyersData$Fall.Leaf.C.N)
qqnorm(AllMyersData$Fall.Leaf.C.N)
qqline(AllMyersData$Fall.Leaf.C.N)
C.N.Leaf.Model<- lmer(Fall.Leaf.C.N ~ Natal.Fluid + ERM + Natal.Fluid*ERM + (1 | Block), data = AllMyersData)
C.N.Leaf.Model.Log<- lmer(log(Fall.Leaf.C.N) ~ Natal.Fluid + ERM + Natal.Fluid*ERM + (1 | Block), data = AllMyersData)
summary(C.N.Leaf.Model)
r2.mixed(C.N.Leaf.Model)

N.Mass.Recov.Leaf.Model<- lmer(Mass.Solution.Rec.Leaf ~   Natal.Fluid + ERM + Natal.Fluid*ERM + (1 | Block), data = AllMyersData)
summary(N.Mass.Recov.Leaf.Model)
r2.mixed(N.Mass.Recov.Leaf.Model)
N.Mass.Recov.Leaf.Model.Simple<- lm(Mass.Solution.Rec.Leaf ~   Natal.Fluid + ERM + Natal.Fluid*ERM, data = AllMyersData)
summary(N.Mass.Recov.Leaf.Model.Simple)
N.Mass.Recov.Leaf.Model.Fixed<- lm(Mass.Solution.Rec.Leaf ~   Natal.Fluid + ERM + Natal.Fluid*ERM + Block, data = AllMyersData)
summary(N.Mass.Recov.Leaf.Model.Fixed)



N.Prec.Recov.Leaf.Model<- lmer(Prec.Solution.Rec.Leaf ~  Natal.Fluid + ERM + Natal.Fluid*ERM + (1 | Block), data = AllMyersData)
summary(N.Prec.Recov.Leaf.Model)
r2.mixed(N.Prec.Recov.Leaf.Model)
N.Prec.Recov.Leaf.Model.Simple<- lm(Prec.Solution.Rec.Leaf ~   Natal.Fluid + ERM + Natal.Fluid*ERM, data = AllMyersData)
summary(N.Prec.Recov.Leaf.Model.Simple)
N.Prec.Recov.Leaf.Model.Fixed<- lm(Prec.Solution.Rec.Leaf ~   Natal.Fluid + ERM + Natal.Fluid*ERM + Block, data = AllMyersData)
summary(N.Prec.Recov.Leaf.Model.Fixed)

#...Stem----
hist(AllMyersData$Fall.Stem.PrecN)
qqnorm(AllMyersData$Fall.Stem.PrecN)
qqline(AllMyersData$Fall.Stem.PrecN)
PrecN.Stem.Model<- lmer(Fall.Stem.PrecN ~ Natal.Fluid + ERM + Natal.Fluid*ERM + (1 | Block), data = AllMyersData)
summary(PrecN.Stem.Model)
r2.mixed(PrecN.Stem.Model)

PrecN.Stem.Model.Simple<- lm(Fall.Stem.PrecN ~ Natal.Fluid + ERM + Natal.Fluid*ERM, data = AllMyersData)
summary(PrecN.Stem.Model.Simple)


hist(AllMyersData$Fall.Stem.PrecC)
qqnorm(AllMyersData$Fall.Stem.PrecC)
qqline(AllMyersData$Fall.Stem.PrecC)
PrecC.Stem.Model<- lmer(Fall.Stem.PrecC ~ Natal.Fluid + ERM + Natal.Fluid*ERM + (1 | Block), data = AllMyersData)
summary(PrecC.Stem.Model)
r2.mixed(PrecC.Stem.Model)

hist(AllMyersData$Fall.Stem.C.N)
qqnorm(AllMyersData$Fall.Stem.C.N)
qqline(AllMyersData$Fall.Stem.C.N)
C.N.Stem.Model<- lmer(Fall.Stem.C.N ~ Natal.Fluid + ERM + Natal.Fluid*ERM + (1 | Block), data = AllMyersData)
C.N.Stem.Model.Log<- lmer(log(Fall.Stem.C.N) ~ Natal.Fluid + ERM + Natal.Fluid*ERM + (1 | Block), data = AllMyersData)
summary(C.N.Stem.Model)
r2.mixed(C.N.Stem.Model)

N.Mass.Recov.Stem.Model<- lmer(Mass.Solution.Rec.Stem ~   Natal.Fluid + ERM + Natal.Fluid*ERM + (1 | Block), data = AllMyersData)
summary(N.Mass.Recov.Stem.Model)
r2.mixed(N.Mass.Recov.Stem.Model)
N.Mass.Recov.Stem.Model.Simple<- lm(Mass.Solution.Rec.Stem ~   Natal.Fluid + ERM + Natal.Fluid*ERM , data = AllMyersData)
summary(N.Mass.Recov.Stem.Model.2)
N.Mass.Recov.Stem.Model.Fixed<- lm(Mass.Solution.Rec.Stem ~ Natal.Fluid + ERM + Natal.Fluid*ERM + Block, data = AllMyersData)
summary(N.Mass.Recov.Stem.Model.Fixed)


N.Prec.Recov.Stem.Model<- lmer(Prec.Solution.Rec.Stem ~  Natal.Fluid + ERM + Natal.Fluid*ERM + (1 | Block), data = AllMyersData)
summary(N.Prec.Recov.Stem.Model)
r2.mixed(N.Prec.Recov.Stem.Model)
N.Prec.Recov.Stem.Model.Simple<- lm(Prec.Solution.Rec.Stem ~   Natal.Fluid + ERM + Natal.Fluid*ERM , data = AllMyersData)
summary(N.Prec.Recov.Stem.Model.Simple)
N.Prec.Recov.Stem.Model.Fixed<- lm(Prec.Solution.Rec.Stem ~ Natal.Fluid + ERM + Natal.Fluid*ERM + Block, data = AllMyersData)
summary(N.Prec.Recov.Stem.Model.Fixed)

#...Coarse Root----
hist(AllMyersData$Fall.CoarseRoot.PrecN)
qqnorm(AllMyersData$Fall.CoarseRoot.PrecN)
qqline(AllMyersData$Fall.CoarseRoot.PrecN)
PrecN.CoarseRoot.Model<- lmer(Fall.CoarseRoot.PrecN ~ Natal.Fluid + ERM + Natal.Fluid*ERM + (1 | Block), data = AllMyersData)
summary(PrecN.CoarseRoot.Model)
r2.mixed(PrecN.CoarseRoot.Model)

hist(AllMyersData$Fall.CoarseRoot.PrecC)
qqnorm(AllMyersData$Fall.CoarseRoot.PrecC)
qqline(AllMyersData$Fall.CoarseRoot.PrecC)
PrecC.CoarseRoot.Model<- lmer(Fall.CoarseRoot.PrecC ~ Natal.Fluid + ERM + Natal.Fluid*ERM + (1 | Block), data = AllMyersData)
summary(PrecC.CoarseRoot.Model)
r2.mixed(PrecC.CoarseRoot.Model)

hist(AllMyersData$Fall.CoarseRoot.C.N)
qqnorm(AllMyersData$Fall.CoarseRoot.C.N)
qqline(AllMyersData$Fall.CoarseRoot.C.N)
C.N.CoarseRoot.Model<- lmer(Fall.CoarseRoot.C.N ~ Natal.Fluid + ERM + Natal.Fluid*ERM + (1 | Block), data = AllMyersData)
C.N.CoarseRoot.Model.Log<- lmer(log(Fall.CoarseRoot.C.N) ~ Natal.Fluid + ERM + Natal.Fluid*ERM + (1 | Block), data = AllMyersData)
summary(C.N.CoarseRoot.Model)
r2.mixed(C.N.CoarseRoot.Model)

N.Mass.Recov.CoarseRoot.Model<- lmer(Mass.Solution.Rec.CoarseRoot ~   Natal.Fluid + ERM + Natal.Fluid*ERM + (1 | Block), data = AllMyersData)
summary(N.Mass.Recov.CoarseRoot.Model)
r2.mixed(N.Mass.Recov.CoarseRoot.Model)
N.Mass.Recov.CoarseRoot.Model.Simple<- lm(Mass.Solution.Rec.CoarseRoot ~   Natal.Fluid + ERM + Natal.Fluid*ERM, data = AllMyersData)
summary(N.Mass.Recov.CoarseRoot.Model.Simple)
N.Mass.Recov.CoarseRoot.Model.Fixed<- lm(Mass.Solution.Rec.CoarseRoot ~   Natal.Fluid + ERM + Natal.Fluid*ERM + Block, data = AllMyersData)
summary(N.Mass.Recov.CoarseRoot.Model.Fixed)



N.Prec.Recov.CoarseRoot.Model<- lmer(Prec.Solution.Rec.CoarseRoot ~  Natal.Fluid + ERM + Natal.Fluid*ERM + (1 | Block), data = AllMyersData)
summary(N.Prec.Recov.CoarseRoot.Model)
r2.mixed(N.Prec.Recov.CoarseRoot.Model)
N.Prec.Recov.CoarseRoot.Model.Simple<- lm(Prec.Solution.Rec.CoarseRoot ~   Natal.Fluid + ERM + Natal.Fluid*ERM, data = AllMyersData)
summary(N.Prec.Recov.CoarseRoot.Model.Simple)
N.Prec.Recov.CoarseRoot.Model.Fixed<- lm(Prec.Solution.Rec.CoarseRoot ~   Natal.Fluid + ERM + Natal.Fluid*ERM + Block, data = AllMyersData)
summary(N.Prec.Recov.CoarseRoot.Model.Fixed)



#...Fine Root----
hist(AllMyersData$Fall.FineRoot.PrecN)
qqnorm(AllMyersData$Fall.FineRoot.PrecN)
qqline(AllMyersData$Fall.FineRoot.PrecN)
PrecN.FineRoot.Model<- lmer(Fall.FineRoot.PrecN ~ Natal.Fluid + ERM + Natal.Fluid*ERM + (1 | Block), data = AllMyersData)
summary(PrecN.FineRoot.Model)
r2.mixed(PrecN.FineRoot.Model)

hist(AllMyersData$Fall.FineRoot.PrecC)
qqnorm(AllMyersData$Fall.FineRoot.PrecC)
qqline(AllMyersData$Fall.FineRoot.PrecC)
PrecC.FineRoot.Model<- lmer(Fall.FineRoot.PrecC ~ Natal.Fluid + ERM + Natal.Fluid*ERM + (1 | Block), data = AllMyersData)
summary(PrecC.FineRoot.Model)
r2.mixed(PrecC.FineRoot.Model)

hist(AllMyersData$Fall.FineRoot.C.N)
qqnorm(AllMyersData$Fall.FineRoot.C.N)
qqline(AllMyersData$Fall.FineRoot.C.N)
C.N.FineRoot.Model<- lmer(Fall.FineRoot.C.N ~ Natal.Fluid + ERM + Natal.Fluid*ERM + (1 | Block), data = AllMyersData)
C.N.FineRoot.Model.Log<- lmer(log(Fall.FineRoot.C.N) ~ Natal.Fluid + ERM + Natal.Fluid*ERM + (1 | Block), data = AllMyersData)
summary(C.N.FineRoot.Model)
r2.mixed(C.N.FineRoot.Model)

N.Mass.Recov.FineRoot.Model<- lmer(Mass.Solution.Rec.FineRoot ~   Natal.Fluid + ERM + Natal.Fluid*ERM + (1 | Block), data = AllMyersData)
summary(N.Mass.Recov.FineRoot.Model)
r2.mixed(N.Mass.Recov.FineRoot.Model)
N.Mass.Recov.FineRoot.Model.Simple<- lm(Mass.Solution.Rec.FineRoot ~   Natal.Fluid + ERM + Natal.Fluid*ERM , data = AllMyersData)
summary(N.Mass.Recov.FineRoot.Model.Simple)
N.Mass.Recov.FineRoot.Model.Fixed<- lm(Mass.Solution.Rec.FineRoot ~   Natal.Fluid + ERM + Natal.Fluid*ERM + Block, data = AllMyersData)
summary(N.Mass.Recov.FineRoot.Model.Fixed)

N.Prec.Recov.FineRoot.Model<- lmer(Prec.Solution.Rec.FineRoot ~  Natal.Fluid + ERM + Natal.Fluid*ERM + (1 | Block), data = AllMyersData)
summary(N.Prec.Recov.FineRoot.Model)
r2.mixed(N.Prec.Recov.FineRoot.Model)


#...POM----
N.Mass.Recov.POM.Model<- lmer(Mass.Solution.Rec.POM ~   Natal.Fluid + ERM + Natal.Fluid*ERM + (1 | Block), data = AllMyersData)
summary(N.Mass.Recov.POM.Model)
r2.mixed(N.Mass.Recov.POM.Model)
N.Mass.Recov.POM.Model.Simple<- lm(Mass.Solution.Rec.POM ~  Natal.Fluid + ERM + Natal.Fluid*ERM , data = AllMyersData)
summary(N.Mass.Recov.POM.Model.Simple)
N.Mass.Recov.POM.Model.Fixed<- lm(Mass.Solution.Rec.POM ~  Natal.Fluid + ERM + Natal.Fluid*ERM + Block, data = AllMyersData)
summary(N.Mass.Recov.POM.Model.Fixed)


N.Prec.Recov.POM.Model<- lmer(Prec.Solution.Rec.POM ~  Natal.Fluid + ERM + Natal.Fluid*ERM + (1 | Block), data = AllMyersData)
summary(N.Prec.Recov.POM.Model)
r2.mixed(N.Prec.Recov.POM.Model)



#...MAOM----
N.Mass.Recov.MAOM.Model<- lmer(Mass.Solution.Rec.MAOM ~   Natal.Fluid + ERM + Natal.Fluid*ERM + (1 | Block), data = AllMyersData)
summary(N.Mass.Recov.MAOM.Model)
r2.mixed(N.Mass.Recov.MAOM.Model)

N.Prec.Recov.MAOM.Model<- lmer(Prec.Solution.Rec.MAOM ~  Natal.Fluid + ERM + Natal.Fluid*ERM + (1 | Block), data = AllMyersData)
summary(N.Prec.Recov.MAOM.Model)
r2.mixed(N.Prec.Recov.MAOM.Model)
N.Prec.Recov.MAOM.Model.Simple<- lm(Prec.Solution.Rec.MAOM ~  Natal.Fluid + ERM + Natal.Fluid*ERM , data = AllMyersData)
summary(N.Prec.Recov.MAOM.Model.Simple)
N.Prec.Recov.MAOM.Model.Fixed<- lm(Prec.Solution.Rec.MAOM ~  Natal.Fluid + ERM + Natal.Fluid*ERM + Block , data = AllMyersData)
summary(N.Prec.Recov.MAOM.Model.Fixed)

#.Ecosystem----
#..All Data----
#...Mass N Recov----
Total.Mass.N.Rec.Model<- lmer(Total.Eco.N.Recovery ~ Natal.Fluid + ERM + Natal.Fluid*ERM + (1 | Block), data = AllMyersData)
summary(Total.Mass.N.Rec.Model)
r2.mixed(Total.Mass.N.Rec.Model)
Total.Mass.N.Rec.Model.Simple<- lm(Total.Eco.N.Recovery ~ Natal.Fluid + ERM + Natal.Fluid*ERM , data = AllMyersData)
summary(Total.Mass.N.Rec.Model.Simple)
Total.Mass.N.Rec.Model.Fixed<- lm(Total.Eco.N.Recovery ~ Natal.Fluid + ERM + Natal.Fluid*ERM + Block, data = AllMyersData)
summary(Total.Mass.N.Rec.Model.Fixed)

#Soil only
AllMyersData$Mass.Solution.Rec.Soil<-AllMyersData$Mass.Solution.Rec.POM + AllMyersData$Mass.Solution.Rec.MAOM
Total.Soil.Mass.N.Rec.Model<- lmer(Mass.Solution.Rec.Soil ~ Natal.Fluid + ERM + Natal.Fluid*ERM + (1 | Block), data = AllMyersData)
summary(Total.Soil.Mass.N.Rec.Model)
r2.mixed(Total.Soil.Mass.N.Rec.Model)

#Aboveground 
AllMyersData$Mass.Solution.Rec.Aboveground<-AllMyersData$Mass.Solution.Rec.Leaf + AllMyersData$Mass.Solution.Rec.Stem
Total.Aboveground.Mass.N.Rec.Model<- lmer(Mass.Solution.Rec.Aboveground ~ Natal.Fluid + ERM + Natal.Fluid*ERM + (1 | Block), data = AllMyersData)
summary(Total.Aboveground.Mass.N.Rec.Model)
r2.mixed(Total.Aboveground.Mass.N.Rec.Model)

#All plant 
AllMyersData$Mass.Solution.Rec.Plant<-AllMyersData$Mass.Solution.Rec.Aboveground + AllMyersData$Mass.Solution.Rec.FineRoot + AllMyersData$Mass.Solution.Rec.CoarseRoot
Total.Plant.Mass.N.Rec.Model<- lmer(Mass.Solution.Rec.Plant ~ Natal.Fluid + ERM + Natal.Fluid*ERM + (1 | Block), data = AllMyersData)
summary(Total.Plant.Mass.N.Rec.Model)
r2.mixed(Total.Plant.Mass.N.Rec.Model)

#...Prec N Recov----
#Soil only
AllMyersData$Prec.Solution.Rec.Soil<-AllMyersData$Prec.Solution.Rec.POM + AllMyersData$Prec.Solution.Rec.MAOM
Total.Soil.Prec.N.Rec.Model<- lmer(Prec.Solution.Rec.Soil ~ Natal.Fluid + ERM + Natal.Fluid*ERM + (1 | Block), data = AllMyersData)
summary(Total.Soil.Prec.N.Rec.Model)
r2.mixed(Total.Soil.Prec.N.Rec.Model)

#Aboveground 
AllMyersData$Prec.Solution.Rec.Aboveground<-AllMyersData$Prec.Solution.Rec.Leaf + AllMyersData$Mass.Solution.Rec.Stem
Total.Aboveground.Prec.N.Rec.Model<- lmer(Prec.Solution.Rec.Aboveground ~ Natal.Fluid + ERM + Natal.Fluid*ERM + (1 | Block), data = AllMyersData)
summary(Total.Aboveground.Prec.N.Rec.Model)
r2.mixed(Total.Aboveground.Prec.N.Rec.Model)

#All plant 
AllMyersData$Prec.Solution.Rec.Plant<-AllMyersData$Prec.Solution.Rec.Aboveground + AllMyersData$Mass.Solution.Rec.FineRoot + AllMyersData$Prec.Solution.Rec.CoarseRoot
Total.Plant.Prec.N.Rec.Model<- lmer(Prec.Solution.Rec.Plant ~ Natal.Fluid + ERM + Natal.Fluid*ERM + (1 | Block), data = AllMyersData)
summary(Total.Plant.Prec.N.Rec.Model)
r2.mixed(Total.Plant.Prec.N.Rec.Model)


Total.Prec.N.Rec.Model<- lmer(Prec.Eco.N.Recovery ~ Natal.Fluid + ERM + Natal.Fluid*ERM + (1 | Block), data = AllMyersData)
summary(Total.Prec.N.Rec.Model)
r2.mixed(Total.Prec.N.Rec.Model)


#..Treatment Data----

#...Mass N Recov----
Treat.Total.Mass.N.Rec.Model<- lmer(Total.Eco.N.Recovery ~ ERM +  (1 | Block), data = AllMyersData_NoControl)
summary(Treat.Total.Mass.N.Rec.Model)
r2.mixed(Treat.Total.Mass.N.Rec.Model)

#Soil only
AllMyersData_NoControl$Mass.Solution.Rec.Soil<-AllMyersData_NoControl$Mass.Solution.Rec.POM + AllMyersData_NoControl$Mass.Solution.Rec.MAOM
Treat.Total.Soil.Mass.N.Rec.Model<- lmer(Mass.Solution.Rec.Soil ~ ERM + (1 | Block), data = AllMyersData_NoControl)
summary(Treat.Total.Soil.Mass.N.Rec.Model)
r2.mixed(Treat.Total.Soil.Mass.N.Rec.Model)

#Aboveground 
AllMyersData_NoControl$Mass.Solution.Rec.Aboveground<-AllMyersData_NoControl$Mass.Solution.Rec.Leaf + AllMyersData_NoControl$Mass.Solution.Rec.Stem
Treat.Total.Aboveground.Mass.N.Rec.Model<- lmer(Mass.Solution.Rec.Aboveground ~  ERM +  (1 | Block), data = AllMyersData_NoControl)
summary(Treat.Total.Aboveground.Mass.N.Rec.Model)
r2.mixed(Treat.Total.Aboveground.Mass.N.Rec.Model)

#All plant 
AllMyersData_NoControl$Mass.Solution.Rec.Plant<-AllMyersData_NoControl$Mass.Solution.Rec.Aboveground + AllMyersData_NoControl$Mass.Solution.Rec.FineRoot + AllMyersData_NoControl$Mass.Solution.Rec.CoarseRoot
Treat.Total.Plant.Mass.N.Rec.Model<- lmer(Mass.Solution.Rec.Plant ~ ERM  + (1 | Block), data = AllMyersData_NoControl)
summary(Treat.Total.Plant.Mass.N.Rec.Model)
r2.mixed(Treat.Total.Plant.Mass.N.Rec.Model)

#...Prec N Recov----
#Soil only
AllMyersData_NoControl$Prec.Solution.Rec.Soil<-AllMyersData_NoControl$Prec.Solution.Rec.POM + AllMyersData_NoControl$Prec.Solution.Rec.MAOM
Treat.Total.Soil.Prec.N.Rec.Model<- lmer(Prec.Solution.Rec.Soil ~  ERM  + (1 | Block), data = AllMyersData_NoControl)
summary(Treat.Total.Soil.Prec.N.Rec.Model)
r2.mixed(Treat.Total.Soil.Prec.N.Rec.Model)

#Aboveground 
AllMyersData_NoControl$Prec.Solution.Rec.Aboveground<-AllMyersData_NoControl$Prec.Solution.Rec.Leaf + AllMyersData_NoControl$Mass.Solution.Rec.Stem
Treat.Total.Aboveground.Prec.N.Rec.Model<- lmer(Prec.Solution.Rec.Aboveground ~ ERM + (1 | Block), data = AllMyersData_NoControl)
summary(Treat.Total.Aboveground.Prec.N.Rec.Model)
r2.mixed(Treat.Total.Aboveground.Prec.N.Rec.Model)

#All plant 
AllMyersData_NoControl$Prec.Solution.Rec.Plant<-AllMyersData_NoControl$Prec.Solution.Rec.Aboveground + AllMyersData_NoControl$Mass.Solution.Rec.FineRoot + AllMyersData_NoControl$Prec.Solution.Rec.CoarseRoot
Treat.Total.Plant.Prec.N.Rec.Model<- lmer(Prec.Solution.Rec.Plant ~ ERM +  (1 | Block), data = AllMyersData_NoControl)
summary(Treat.Total.Plant.Prec.N.Rec.Model)
r2.mixed(Treat.Total.Plant.Prec.N.Rec.Model)


Treat.Total.Prec.N.Rec.Model<- lmer(Prec.Eco.N.Recovery ~ ERM  + (1 | Block), data = AllMyersData)
summary(Treat.Total.Prec.N.Rec.Model)
r2.mixed(Treat.Total.Prec.N.Rec.Model)



#...Soil Stocks========
AllMyersData$Fall.C.Stock<-AllMyersData$Fall.POM.C.Stock.g.m2 + AllMyersData$Fall.MAOM.C.Stock.g.m2 
AllMyersData$Fall.N.Stock<-AllMyersData$Fall.POM.N.Stock.g.m2 + AllMyersData$Fall.MAOM.N.Stock.g.m2 

Fall.C.Stock.Model<- lmer(Fall.C.Stock ~   Natal.Fluid + ERM + Natal.Fluid*ERM + (1 | Block), data = AllMyersData)
summary(Fall.C.Stock.Model)
r2.mixed(Total.Prec.N.Rec.Model)

Fall.N.Stock.Model<- lmer(Fall.N.Stock ~   Natal.Fluid + ERM + Natal.Fluid*ERM + (1 | Block), data = AllMyersData)
summary(Fall.N.Stock.Model)
r2.mixed(Total.Prec.N.Rec.Model)

