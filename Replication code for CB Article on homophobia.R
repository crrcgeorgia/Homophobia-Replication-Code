#CB Homophobia Article Replication Code
getwd()
library(descr)
library(haven)
library(psych)
library(MASS)
library(survey)
library(polycor)
setwd("C:/Users/keti/Desktop/ceu stats folder")
cb17<-read_dta("CB_2017_Regional_Only_Responses_22.02.18.dta")
geo17<-subset(cb17, cb17$COUNTRY==3)
designgeo17 <- svydesign(id=~PSU,weights=~INDWT, strat=~STRATUM, data=geo17)
freq(geo17$NEIGHBOR)
geo17$NEIGHBOR_r<-geo17$NEIGHBOR
geo17$NEIGHBOR_r[geo17$NEIGHBOR_r!=6]<-0
geo17$NEIGHBOR_r[geo17$NEIGHBOR_r==6]<-1
freq(geo17$NEIGHBOR_r, w=geo17$INDWT)
geo17$RESPSEX
geo17$children<-(geo17$V4-geo17$V5)
table(geo17$children)
geo17$RESPEDU_r<-geo17$RESPEDU
geo17$RESPEDU_r[geo17$RESPEDU_r<=-1]<-NA
geo17$RESPEDU_r[geo17$RESPEDU_r<=4]<-1
geo17$RESPEDU_r[geo17$RESPEDU_r>=6]<-3
geo17$RESPEDU_r[geo17$RESPEDU_r==5]<-2
table(geo17$ETHNIC)
geo17$ETHNIC_r<-geo17$ETHNIC
geo17$ETHNIC_r[geo17$ETHNIC_r!=3]<-1
geo17$ETHNIC_r[geo17$ETHNIC_r==3]<-0
table(geo17$ETHNIC_r)
geo17$NEIGHBOR_criminal<-geo17$NEIGHBOR
geo17$NEIGHBOR_criminal[geo17$NEIGHBOR_criminal!=8]<-0
geo17$NEIGHBOR_criminal[geo17$NEIGHBOR_criminal==8]<-1

svymean(geo17$NEIGHBOR_r, designgeo17)
(0.266+(1.96*0.0166))
(0.266-(1.96*0.0166))
(0.23333+(1.96*0.0154))
(0.23333-(1.96*0.0154))

table1<-svytable(~as.factor(NEIGHBOR_r)+as.factor(RELSERV_r), designgeo17)
table2<-svytable(~as.factor(NEIGHBOR_r)+as.factor(RELFAST_r), designgeo17)

geo17$OWNCOTV_r<-geo17$OWNCOTV
geo17$OWNCOTV_r[geo17$OWNCOTV_r<=-1]<-NA

geo17$OWNDIGC_r<-geo17$OWNDIGC
geo17$OWNDIGC_r[geo17$OWNDIGC_r<=-1]<-NA

geo17$OWNWASH_r<-geo17$OWNWASH
geo17$OWNWASH_r[geo17$OWNWASH<=-1]<-NA

geo17$OWNFRDG_r<-geo17$OWNFRDG
geo17$OWNFRDG_r[geo17$OWNFRDG<=-1]<-NA

geo17$OWNAIRC_r<-geo17$OWNAIRC
geo17$OWNAIRC_r[geo17$OWNAIRC_r<=-1]<-NA

geo17$OWNCARS_r<-geo17$OWNCARS
geo17$OWNCARS_r[geo17$OWNCARS_r<=-1]<-NA

geo17$OWNLNDP_r<-geo17$OWNLNDP
geo17$OWNLNDP_r[geo17$OWNLNDP_r<=-1]<-NA

geo17$OWNCELL_r<-geo17$OWNCELL
geo17$OWNCELL_r[geo17$OWNCELL_r<=-1]<-NA

geo17$OWNCOMP_r<-geo17$OWNCOMP
geo17$OWNCOMP_r[geo17$OWNCOMP_r<=-1]<-NA

geo17$assets<-(geo17$OWNCOTV_r+
                 geo17$OWNDIGC_r+
                 geo17$OWNWASH_r+
                 geo17$OWNFRDG_r+
                 geo17$OWNAIRC_r+
                 geo17$OWNCARS_r+
                 geo17$OWNLNDP_r+
                 geo17$OWNCELL_r+
                 geo17$OWNCOMP_r)
table(geo17$assets)


#religiousity
table(geo17$RELFAST)
geo17$RELFAST_r<-geo17$RELFAST
geo17$RELFAST_r[geo17$RELFAST_r<=-1]<-NA
geo17$RELFAST_r[geo17$RELFAST_r<=2]<-1
geo17$RELFAST_r[geo17$RELFAST_r>=3]<-0
table(geo17$RELSERV)
geo17$RELSERV_r<-geo17$RELSERV
geo17$RELSERV_r[geo17$RELSERV_r<=-1]<-NA
geo17$RELSERV_r[geo17$RELSERV_r<=3]<-1
geo17$RELSERV_r[geo17$RELSERV_r>=4]<-0
table(geo17$RELSERV_r)
table(geo17$RELGNEW)
geo17$RELGNEW[geo17$RELGNEW<=-1]<-NA
geo17$RELGNEW[geo17$RELGNEW!=3]<-0
geo17$RELGNEW[geo17$RELGNEW==3]<-1
geo17$children_dummy<-geo17$children
geo17$children_dummy[geo17$children_dummy>=1]<-1
freq(geo17$RELGNEW, w=geo17$INDWT)
geo17$agesex<-(geo17$AGE*geo17$RESPSEX)
designgeo17 <- svydesign(id=~PSU,weights=~INDWT, strat=~STRATUM, data=geo17)

model1homophobia<-svyglm(NEIGHBOR_r~as.factor(STRATUM)+
                           RESPSEX+AGE+
                           children_dummy+
                           RESPEDU_higher+
                           ETHNIC_r+
                           assets+
                           RELGNEW+
                           RELSERV_r+
                           RELFAST_r, design=designgeo17, family="binomial")
summary(model1homophobia)


newdata1<-with(geo17, data.frame(AGE=18:102,
                                 RESPSEX=2,
                                 STRATUM=1,
                                 STRATUM=2,
                                 STRATUM=3,
                                 STRATUM=4,
                                 children_dummy=mean(children_dummy, na.rm=TRUE),
                                 RESPEDU_higher=mean(RESPEDU_higher, na.rm=TRUE),
                                 ETHNIC_r=mean(ETHNIC_r, na.rm=TRUE),
                                 assets=mean(assets, na.rm=TRUE),
                                 RELGNEW=mean(RELGNEW, na.rm=TRUE),
                                 RELSERV_r=mean(RELSERV_r, na.rm=TRUE),
                                 RELFAST_r=mean(RELFAST_r,na.rm=TRUE)))

newdata1$preds<-predict(model1homophobia, newdata=newdata1, type = "resp")
plot(newdata1$preds)

newdata2<-with(geo17, data.frame(RESPSEX=1:2,
                                 AGE=mean(AGE, na.rm=TRUE),
                                 STRATUM=1,
                                 STRATUM=2,
                                 STRATUM=3,
                                 STRATUM=4,
                                 children_dummy=mean(children_dummy, na.rm=TRUE),
                                 RESPEDU_higher=mean(RESPEDU_higher, na.rm=TRUE),
                                 ETHNIC_r=mean(ETHNIC_r, na.rm=TRUE),
                                 assets=mean(assets, na.rm=TRUE),
                                 RELGNEW=mean(RELGNEW, na.rm=TRUE),
                                 RELSERV_r=mean(RELSERV_r, na.rm=TRUE),
                                 RELFAST_r=mean(RELFAST_r,na.rm=TRUE)))

preds<-predict(model1homophobia, newdata=newdata2, type = "resp", se.fit=TRUE)
plot(newdata2$preds)
newdata1$preds
newdata2$preds