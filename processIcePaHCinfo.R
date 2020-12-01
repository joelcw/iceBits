#original: Friday 13th Nov 2020
#16 Nov 2020

library(tidyr)
library(tidyselect)
library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(lme4)
library(lmerTest)

#source(file="~/constantentropy/dormUido.R")
library(devtools)
source_url('https://raw.githubusercontent.com/joelcw/constantentropy/master/dormUido.R')

foo <- read.delim(file="~/iceBits/ovCodingTreeAndClauseFreq.tsv",header = F,sep="\t")

#drop empty columns and name good columns
foo <- foo[,1:14]
colnames(foo) <- c("OV","ObjType","SbjType","Clause","SbjWords","ObjWords","NodeString","TextId","Year","Genre", "TreeId","SentString","SentFreq","ClauseFreq")

foo$Year <- as.numeric(as.character(foo$Year))
foo$OV <- as.numeric(as.character(foo$OV))
foo$SbjWords <- as.numeric(as.character(foo$SbjWords))
foo$ObjWords <- as.numeric(as.character(foo$ObjWords))


#strip off extended labels from clause labels
foo$Clause <- str_extract(foo$Clause,"IP-[A-Z]{3}")

#strip response times of brackets, convert them to list of numbers by splitting
foo$ClauseFreq <- as.character(foo$ClauseFreq)
foo$SentFreq <- as.character(foo$SentFreq)
foo$ClauseFreq <- str_remove_all(foo$ClauseFreq, "[ \\[\\]]")
foo$SentFreq <- str_remove_all(foo$SentFreq, "[ \\[\\]]")
#Because R string functions are as unintuitive as humanly possible, in order to get this to actually return an indexable list of substrings
#you need to use simplify=T below, which returns a matrix, and then convert that into a vector or a list (list in this case because lists can be single
# elements in data frames but vectors can't for some reason), then make sure the vector/list is numeric, 
# and then you need to loop over every item so other dumb things don't happen...geez:
i = 1
for (i in 1:nrow(foo))
{
  foo$ClauseFreq[i] <- list(as.numeric(str_split(foo$ClauseFreq[i],",",simplify=T)))
  foo$SentFreq[i] <- list(as.numeric(str_split(foo$SentFreq[i],",",simplify=T)))
  
  #add 1 to all the frequencies to get rid of 0s, which won't be of any use to anyone
  foo$ClauseFreq[i][[1]] <- foo$ClauseFreq[i][[1]] + 1
  foo$SentFreq[i][[1]] <- foo$SentFreq[i][[1]] + 1
  
}

#Now, because each element has been coerced into a list, they need to be indexed in a really dumb way, e.g. fooNew$responseTimestamps[i][[1]]

#Convert to probability, and to bits. Then make columns for dorm, and dorm-uido:

foo$ClauseDorm <- 0
foo$ClauseDormUido <- 0
foo$SentDorm <- 0
foo$SentDormUido <- 0

i = 1
while (i <= nrow(foo))
  {
  
  foo$ClauseProb[i][[1]] <- foo$ClauseFreq[i][[1]]*(1/25000000)
  foo$ClauseInfo[i][[1]] <- log2(foo$ClauseProb[i][[1]])
  foo$ClauseDorm[i] <- dorm(foo$ClauseInfo[i][[1]], correct=TRUE)
  foo$ClauseDormUido[i] <- foo$ClauseDorm[i] - dorm(uido(foo$ClauseInfo[i][[1]]))
  
  foo$SentProb[i][[1]] <- (foo$SentFreq[i][[1]])*(1/25000000)
  foo$SentInfo[i][[1]] <- log2(foo$SentProb[i][[1]])
  foo$SentDorm[i] <- dorm(foo$SentInfo[i][[1]], correct=TRUE)
  foo$SentDormUido[i] <- foo$SentDorm[i] - dorm(uido(foo$SentInfo[i][[1]]))
  
  i = i+1
  
  }


####Some plots

#Plot with only one type of objects, sub clauses, sanity check
nomobj <- subset(foo,foo$ObjType == "dp" & foo$Clause == "IP-SUB")
nomobj <- subset(nomobj, nomobj$SbjType == "dp" | nomobj$SbjType == "pro")
nomobj <- droplevels(nomobj)
foo$Clause <- as.factor(foo$Clause)


ggplot(nomobj, aes(Year, OV, color=SbjType)) + 
  labs(y = "OV", x = "\nYear") +
  #  geom_line() +
  geom_point() +
  geom_smooth() +
  facet_wrap(~Genre) +
scale_color_brewer(palette = "Set1") + 
  theme_bw() + theme(panel.border = element_blank())

nomobjsbj <- subset(nomobj, nomobj$SbjType == "dp")
nomobjsbj <- droplevels(nomobjsbj)
nomobjsbj$OV <- as.factor(nomobjsbj$OV)

#Note that dormuido is stable over time, and Year is not significant in any model below.
ggplot(nomobjsbj, aes(Year, ClauseDormUido, color=OV)) + 
  labs(y = "ClauseDorm-ClauseUido", x = "\nYear") +
  #  geom_line() +
  geom_point() +
  geom_smooth() +
  facet_wrap(~SimpleGenre) +
  scale_color_brewer(palette = "Set1") + 
  theme_bw() + theme(panel.border = element_blank())


####Statistical models

##Assume interaction between Genre Year and OV, because we know the slope of decline of OV over time differs by Genre,
#but recode Genre into nar and other, so there's some statistical power

foo$SimpleGenre <- ifelse(foo$Genre == "nar", "nar", "other")

#no interactions with dorm
foo.fit.Sbj.Obj <- lmer(ClauseDormUido~(1|TextId)+Year+OV+Clause+Year+SimpleGenre+ObjType+SbjType, data=foo)
summary(foo.fit.Sbj.Obj)

#interaction between SbjType and OV affecting dormuido, and ObjType and OV, but no 3-way interaction affecting dormuido. Model comparison borderline on all measures.
foo.fit.SbjOV.ObjOV <- lmer(ClauseDormUido~(1|TextId)+Year+OV+Clause+Year+SimpleGenre+ObjType+SbjType+SbjType:OV+ObjType:OV, data=foo)
summary(foo.fit.SbjOV.ObjOV)

anova(foo.fit.SbjOV.ObjOV,foo.fit.Sbj.Obj, test="Chisq")
AIC(foo.fit.Sbj.Obj)
AIC(foo.fit.SbjOV.ObjOV)
BIC(foo.fit.Sbj.Obj)
BIC(foo.fit.SbjOV.ObjOV)

#Interaction between Sbj and Obj, but not with OV; model comparison shows this to be important.
foo.fit.SbjObj <- lmer(ClauseDormUido~(1|TextId)+Year+OV+Clause+Year+SimpleGenre+ObjType+SbjType+SbjType:ObjType, data=foo)
summary(foo.fit.SbjObj)
anova(foo.fit.SbjObj,foo.fit.Sbj.Obj, test="Chisq")
AIC(foo.fit.Sbj.Obj)
AIC(foo.fit.SbjObj)
BIC(foo.fit.Sbj.Obj)
BIC(foo.fit.SbjObj)


#3-way interaction between sbj and obj types and OV affecting dormuido, but no interaction with Clause yet
foo.fit.SbjObjOV <- lmer(ClauseDormUido~(1|TextId)+Year+OV+Clause+Year+SimpleGenre+ObjType+SbjType+SbjType*ObjType*OV, data=foo)
summary(foo.fit.SbjObjOV)
anova(foo.fit.SbjObj,foo.fit.SbjObjOV, test="Chisq")

#4-way interaction incl Clause affecting dormuido. Model comparison by Chisq and AIC does show this to be important, though BIC goes the other way. Still, I think we have a winner.  OV and Clause sig, and OV and SbjTypepro, and OV and SbjTypegapped.
foo.fit.SbjObjOVClause <- lmer(ClauseDormUido~(1|TextId)+Year+OV+Clause+Year+SimpleGenre+ObjType+SbjType+SbjType*ObjType*OV*Clause, data=foo)
summary(foo.fit.SbjObjOVClause)
anova(foo.fit.SbjObj,foo.fit.SbjObjOVClause, test="Chisq")
AIC(foo.fit.SbjObj)
AIC(foo.fit.SbjObjOVClause)
BIC(foo.fit.SbjObj)
BIC(foo.fit.SbjObjOVClause)


#just for the hell of it, see if the 3-way interaction OV:Clause:SbjType...and actually model comparison shows the 4 way is different (but BIC goes the other way)
foo.fit.SbjOVClause <- lmer(ClauseDormUido~(1|TextId)+Year+OV+Clause+Year+SimpleGenre+ObjType+SbjType+SbjType*OV*Clause, data=foo)
summary(foo.fit.SbjOVClause)
anova(foo.fit.SbjOVClause,foo.fit.SbjObjOVClause, test="Chisq")
AIC(foo.fit.SbjOVClause)
AIC(foo.fit.SbjObjOVClause)
BIC(foo.fit.SbjOVClause)
BIC(foo.fit.SbjObjOVClause)


#change contrasts so we can compare nominal objects to other types..and with "pro" it shows an interesting interaction bet/ dp and dp
foo$ObjTypeRelevel <- relevel(foo$ObjType, ref="pro")
foo$SbjTypeRelevel <- relevel(foo$SbjType, ref="pro")

foo.fit.SbjObjOVClause <- lmer(ClauseDormUido~(1|TextId)+Year+OV+Clause+Year+SimpleGenre+ObjTypeRelevel+SbjTypeRelevel+SbjTypeRelevel*ObjTypeRelevel*OV*Clause, data=foo)
summary(foo.fit.SbjObjOVClause)




#Model with SbjObj interaction, OVClause, OVSbjType interactions, but not 4-way. Model comparison prefers the 4-way, except for BIC, which prefers the SbjObj model.
foo.fit.SbjObj.OVSbj.OVClause <- lmer(ClauseDormUido~(1|TextId)+Year+OV+Clause+Year+SimpleGenre+ObjType+SbjType+SbjType*ObjType+OV*Clause+OV*SbjType, data=foo)
summary(foo.fit.SbjObj.OVSbj.OVClause)
anova(foo.fit.SbjObj,foo.fit.SbjObj.OVSbj.OVClause, test="Chisq")
anova(foo.fit.SbjObj.OVSbj.OVClause,foo.fit.SbjObjOVClause, test="Chisq")
AIC(foo.fit.SbjObj)
AIC(foo.fit.SbjObj.OVSbj.OVClause)
BIC(foo.fit.SbjObj)
BIC(foo.fit.SbjObj.OVSbj.OVClause)
AIC(foo.fit.SbjObj.OVSbj.OVClause)
AIC(foo.fit.SbjObjOVClause)
BIC(foo.fit.SbjObj.OVSbj.OVClause)
BIC(foo.fit.SbjObjOVClause)


##Same models with just narrative texts; but they all come out the same as above, more or less.
naronly <- subset(foo,foo$Genre == "nar")
naronly <- droplevels(naronly)

naronly.fit.Sbj.Obj <- lmer(ClauseDormUido~(1|TextId)+Year+OV+Clause+Year+ObjType+SbjType, data=naronly)
summary(naronly.fit.Sbj.Obj)

#Model comparison sig by Chisq and AIC, not BIC.
naronly.fit.SbjOV.ObjOV <- lmer(ClauseDormUido~(1|TextId)+Year+OV+Clause+Year+ObjType+SbjType+SbjType:OV+ObjType:OV, data=naronly)
summary(naronly.fit.SbjOV.ObjOV)
anova(naronly.fit.Sbj.Obj,naronly.fit.SbjOV.ObjOV, test="Chisq")
AIC(naronly.fit.Sbj.Obj)
AIC(naronly.fit.SbjOV.ObjOV)
BIC(naronly.fit.Sbj.Obj)
BIC(naronly.fit.SbjOV.ObjOV)

#Interaction between Sbj and Obj, but not with OV; Model comparison sig by Chisq and AIC, not BIC.
naronly.fit.SbjObj <- lmer(ClauseDormUido~(1|TextId)+Year+OV+Clause+Year+ObjType+SbjType+SbjType:ObjType, data=naronly)
summary(naronly.fit.SbjObj)
anova(naronly.fit.SbjObj,naronly.fit.Sbj.Obj, test="Chisq")
AIC(naronly.fit.Sbj.Obj)
AIC(naronly.fit.SbjObj)
BIC(naronly.fit.Sbj.Obj)
BIC(naronly.fit.SbjObj)


#4-way interaction incl Clause affecting dormuido. Model comparison by Chisq and mildly AIC shows this to be important, not BIC.
naronly.fit.SbjObjOVClause <- lmer(ClauseDormUido~(1|TextId)+Year+OV+Clause+Year+ObjType+SbjType+SbjType*ObjType*OV*Clause, data=naronly)
anova(naronly.fit.SbjObj,naronly.fit.SbjObjOVClause, test="Chisq")
AIC(naronly.fit.SbjObj)
AIC(naronly.fit.SbjObjOVClause)
BIC(naronly.fit.SbjObj)
BIC(naronly.fit.SbjObjOVClause)



##Same models, but with only pro and dp SbjType and ObjType
pronom <- subset(foo, foo$SbjType == "dp" | foo$SbjType == "pro")
pronom <- subset(pronom, pronom$ObjType == "dp" | pronom$ObjType == "pro")
pronom <- droplevels(pronom)

pronom.fit.Sbj.Obj <- lmer(ClauseDormUido~(1|TextId)+Year+OV+Clause+Year+SimpleGenre+ObjType+SbjType, data=pronom)
summary(pronom.fit.Sbj.Obj)

#interaction between SbjType and OV affecting dormuido, and ObjType and OV, but no 3-way interaction affecting dormuido. OV and SbjTypepro sig but borderline AIC, BIC.
pronom.fit.SbjOV.ObjOV <- lmer(ClauseDormUido~(1|TextId)+Year+OV+Clause+Year+SimpleGenre+ObjType+SbjType+SbjType:OV+ObjType:OV, data=pronom)
summary(pronom.fit.SbjOV.ObjOV)

anova(pronom.fit.SbjOV.ObjOV,pronom.fit.Sbj.Obj, test="Chisq")
AIC(pronom.fit.Sbj.Obj)
AIC(pronom.fit.SbjOV.ObjOV)
BIC(pronom.fit.Sbj.Obj)
BIC(pronom.fit.SbjOV.ObjOV)

#Interaction between Sbj and Obj, but not with OV; model comparison shows this to be important.
pronom.fit.SbjObj <- lmer(ClauseDormUido~(1|TextId)+Year+OV+Clause+Year+SimpleGenre+ObjType+SbjType+SbjType:ObjType, data=pronom)
summary(pronom.fit.SbjObj)
anova(pronom.fit.SbjObj,pronom.fit.Sbj.Obj, test="Chisq")
AIC(pronom.fit.Sbj.Obj)
AIC(pronom.fit.SbjObj)
BIC(pronom.fit.Sbj.Obj)
BIC(pronom.fit.SbjObj)


#3-way interaction between sbj and obj types and OV affecting dormuido, but no interaction with Clause yet. Model comparison around 0.05, OV and SbjTypepro is sig as above.
pronom.fit.SbjObjOV <- lmer(ClauseDormUido~(1|TextId)+Year+OV+Clause+Year+SimpleGenre+ObjType+SbjType+SbjType*ObjType*OV, data=pronom)
summary(pronom.fit.SbjObjOV)
anova(pronom.fit.SbjObj,pronom.fit.SbjObjOV, test="Chisq")

#4-way interaction incl Clause affecting dormuido. Model comparison by Chisq and AIC does show this to be important, though it's difficult to interpret, and BIC goes the other way. OV and Clause sig and OV and SbjTypepro.
pronom.fit.SbjObjOVClause <- lmer(ClauseDormUido~(1|TextId)+Year+OV+Clause+Year+SimpleGenre+ObjType+SbjType+SbjType*ObjType*OV*Clause, data=pronom)
summary(pronom.fit.SbjObjOVClause)
anova(pronom.fit.SbjObj,pronom.fit.SbjObjOVClause, test="Chisq")
AIC(pronom.fit.SbjObj)
AIC(pronom.fit.SbjObjOVClause)
BIC(pronom.fit.SbjObj)
BIC(pronom.fit.SbjObjOVClause)





##Now try for sentence level dormuido, subord clauses only, because sent and clause-level dormuido is often equivalent for matrix clauses
subonly <- subset(foo,foo$Clause == "IP-SUB")
subonly <- droplevels(subonly)

#no interactions with dorm
subonly.fit.Sbj.Obj <- lmer(SentDormUido~(1|TextId)+Year+OV+Year+SimpleGenre+ObjType+SbjType, data=subonly)
summary(subonly.fit.Sbj.Obj)

#Interaction between Sbj and Obj, but not with OV; not sig.
subonly.fit.SbjObj <- lmer(SentDormUido~(1|TextId)+Year+OV+Year+SimpleGenre+ObjType+SbjType+SbjType:ObjType, data=subonly)
summary(subonly.fit.SbjObj)
anova(subonly.fit.SbjObj,subonly.fit.Sbj.Obj, test="Chisq")
AIC(subonly.fit.Sbj.Obj)
AIC(subonly.fit.SbjObj)
BIC(subonly.fit.Sbj.Obj)
BIC(subonly.fit.SbjObj)


#3-way interaction between sbj and obj types and OV affecting dormuido; not sig.
subonly.fit.SbjObjOV <- lmer(SentDormUido~(1|TextId)+Year+OV+Year+SimpleGenre+ObjType+SbjType+SbjType*ObjType*OV, data=subonly)
summary(subonly.fit.SbjObjOV)
anova(subonly.fit.SbjObj,subonly.fit.SbjObjOV, test="Chisq")



####Modelling OV as a function of syntactic environment
foo.ovfit.SbjObjClause <- glmer(OV~(1|TextId)+Year+Clause+Year+SimpleGenre+ObjType+SbjType+SimpleGenre:Year+SbjType*ObjType*Clause, family=binomial, data=foo)
foo.ovfit.SbjObj <- glmer(OV~(1|TextId)+Year+Clause+Year+SimpleGenre+ObjType+SbjType+SimpleGenre:Year+SbjType*ObjType, family=binomial, data=foo)
foo.ovfit.Sbj.Obj <- glmer(OV~(1|TextId)+Year+Clause+Year+SimpleGenre+ObjType+SbjType+SimpleGenre:Year, family=binomial, data=foo)
summary()
anova(foo.ovfit.SbjObjClause,foo.ovfit.SbjObj, test="Chisq")
AIC(foo.ovfit.SbjObj)
AIC(foo.ovfit.SbjObjClause)
BIC(foo.ovfit.SbjObj)
BIC(foo.ovfit.SbjObjClause)