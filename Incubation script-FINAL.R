data<-read.csv(file.choose(),header=T)

library(ggplot2)
library(tidyr)
library(dplyr)
library(cowplot)
library(nlme)

model<-lm(logCH4.prod~Depth*Treatment, data=data,na.action=na.omit)
anova(model)

model2<-lm(logCH4.prod~Depth*Site, data=data,na.action=na.omit)
anova(model2)

library(emmeans)
lsmeans(model,pairwise~Treatment,adjust="tukey")

lsmeans(model2,pairwise~Site*Depth,adjust="tukey")
lsmeans(model2, pairwise~Site, adjust="tukey")

model3<-lm(logCH4.prod~WT, data=data, na.action=na.omit)
anova(model3)

model4<-lm(logCH4.prod~WT*Site, data=data, na.action=na.omit)
anova(model4)

compare<-lstrends(model4,~as.factor(Site),var="WT",data=data)
compare
pairs(compare)

#ph 
library(emmeans)
lsmeans(model,pairwise~Treatment,adjust="tukey")

lsmeans(model2,pairwise~Site*Depth,adjust="tukey")
lsmeans(model2, pairwise~Site, adjust="tukey")

model5<-lm(logCH4.prod~pH, data=data, na.action=na.omit)
anova(model5)

model6<-lm(logCH4.prod~pH*Site, data=data, na.action=na.omit)
anova(model6)

compare<-lstrends(model6,~as.factor(Site),var="pH",data=data)
compare
pairs(compare)
#EC
library(emmeans)
lsmeans(model,pairwise~Treatment,adjust="tukey")

lsmeans(model2,pairwise~Site*Depth,adjust="tukey")
lsmeans(model2, pairwise~Site, adjust="tukey")

model7<-lm(logCH4.prod~EC, data=data, na.action=na.omit)
anova(model7)

model8<-lm(logCH4.prod~EC*Site, data=data, na.action=na.omit)
anova(model8)

compare<-lstrends(model8,~as.factor(Site),var="EC",data=data)
compare
pairs(compare)
#let's make some plots
#Create the boxplot
library(ggplot2)
fig1 <- ggplot(data, aes(Treatment, logCH4.prod, fill=Depth)) + 
  
  geom_boxplot()+
  labs(title = "Methane Production Control by Water Treatment", x = "Treatmemt", y = "Log CH4 Produtction")

fig1

fig2 <- ggplot(data, aes(WT, logCH4.prod)) + 
  
  geom_point(aes(fill=Depth, shape=Site, color=Depth), size=3) +
  labs(title = "Methane Production Control by Water Table", x = "Water Table(cm)", y = "Log CH4 Produtction")

fig2

fig3 <- ggplot(data, aes(pH, logCH4.prod)) + 
 
  geom_point(aes(fill=Depth, shape=Site, color=Depth), size=3)+
  labs(title = "Methane Production Control by pH Level", x = "pH level", y = "Log CH4 Produtction")

fig3

fig4 <- ggplot(data, aes(EC, logCH4.prod)) + 
  
  geom_point(aes(fill=Depth, shape=Site, color=Depth), size=3)+
  labs(title = "Methane Production Control by Electrical conductivity", x = "Electrical conductivity(mS/cm)", y = "Log CH4 Produtction")

fig4

