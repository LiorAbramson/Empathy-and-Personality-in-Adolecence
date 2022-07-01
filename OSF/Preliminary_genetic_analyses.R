# This script performs all the preliminary analyses 
# before conducting the biometric cross-lagged analysis in Mx analysis 


#Preparations
  rm(list = ls()) # clean the global environment
  cat ("\014")    #clean the R console

#Packages
  packages <- c()
  lapply(packages, require, character.only = TRUE)

#Import the file (Dfinal_11_13 was created using 'the_empathic_personality_profiles.R' script)
  D <- read.csv ("~/Documents/projects/Empathy-Personality-Adolecence/OSF/data/Dfinal_11_13.csv")

#################################################################################################
####### Preliminary analyses  #####################################################################
#################################################################################################

#examining relations between sex and source and study variables
#equal variances between the groups ARE NOT assumed

library(broom)
library(purrr)

#sex
ttest_sexPredemo11 <- t.test(predicted_emotional_11~sex, data=D)
ttest_sexEmpemo11 <-t.test(EMPQ_emotional_11~sex, data=D)
ttest_sexPredcog11 <- t.test(predicted_cognitive_11~sex, data=D)
ttest_sexEmpcog11 <- t.test(EMPQ_cognitive_11~sex, data=D)

ttest_sexPredemo13 <- t.test(predicted_emotional_13~sex, data=D)
ttest_sexEmpemo13 <-t.test(EMPQ_emotional_13~sex, data=D)
ttest_sexPredcog13 <- t.test(predicted_cognitive_13~sex, data=D)
ttest_sexEmpcog13 <- t.test(EMPQ_cognitive_13~sex, data=D)

#girls are higher in all variables

ttest_sexTable <- map_df(list(ttest_sexPredemo11,ttest_sexEmpemo11,
                              ttest_sexPredemo13,ttest_sexEmpemo13,
                              ttest_sexPredcog11, ttest_sexEmpcog11,
                              ttest_sexPredcog13, ttest_sexEmpcog13), tidy)

ttest_sexTable_edit <- ttest_sexTable[c("estimate1", "estimate2","statistic","parameter","p.value")]
colnames(ttest_sexTable_edit) <- c("Mean boys","Mean girls", "t value","df","p-value")
ttest_sexTable_edit[,1:4] <- round(ttest_sexTable_edit[,1:4],2)
ttest_sexTable_edit[,5] <- round(ttest_sexTable_edit[,5],3)

#add effect size
ttest_sexTable_edit$cohenD <- NA
library(lsr)
ttest_sexTable_edit$cohenD[1] <- 0-round(cohensD(predicted_emotional_11~sex, data=D),2)
ttest_sexTable_edit$cohenD[2] <- 0-round(cohensD(EMPQ_emotional_11~sex, data=D),2)
ttest_sexTable_edit$cohenD[3] <- 0-round(cohensD(predicted_emotional_13~sex, data=D),2)
ttest_sexTable_edit$cohenD[4] <- 0-round(cohensD(EMPQ_emotional_13~sex, data=D),2)

ttest_sexTable_edit$cohenD[5] <- 0-round(cohensD(predicted_cognitive_11~sex, data=D),2)
ttest_sexTable_edit$cohenD[6] <- 0-round(cohensD(EMPQ_cognitive_11~sex, data=D),2)
ttest_sexTable_edit$cohenD[7] <- 0-round(cohensD(predicted_cognitive_13~sex, data=D),2)
ttest_sexTable_edit$cohenD[8] <- 0-round(cohensD(EMPQ_cognitive_13~sex, data=D),2)

#add sd
ttest_sexTable_edit$sdBoys <- NA
ttest_sexTable_edit$sdGirls <- NA

ttest_sexTable_edit$sdBoys[1] <- round((aggregate(D$predicted_emotional_11~D$sex, FUN = sd)[1,2]),2)
ttest_sexTable_edit$sdBoys[2] <- round((aggregate(D$EMPQ_emotional_11~D$sex, FUN = sd)[1,2]),2)
ttest_sexTable_edit$sdBoys[3] <- round((aggregate(D$predicted_emotional_13~D$sex, FUN = sd)[1,2]),2)
ttest_sexTable_edit$sdBoys[4] <- round((aggregate(D$EMPQ_emotional_13~D$sex, FUN = sd)[1,2]),2)

ttest_sexTable_edit$sdBoys[5] <- round((aggregate(D$predicted_cognitive_11~D$sex, FUN = sd)[1,2]),2)
ttest_sexTable_edit$sdBoys[6] <- round((aggregate(D$EMPQ_cognitive_11~D$sex, FUN = sd)[1,2]),2)
ttest_sexTable_edit$sdBoys[7] <- round((aggregate(D$predicted_cognitive_13~D$sex, FUN = sd)[1,2]),2)
ttest_sexTable_edit$sdBoys[8] <- round((aggregate(D$EMPQ_cognitive_13~D$sex, FUN = sd)[1,2]),2)

ttest_sexTable_edit$sdGirls[1] <- round((aggregate(D$predicted_emotional_11~D$sex, FUN = sd)[2,2]),2)
ttest_sexTable_edit$sdGirls[2] <- round((aggregate(D$EMPQ_emotional_11~D$sex, FUN = sd)[2,2]),2)
ttest_sexTable_edit$sdGirls[3] <- round((aggregate(D$predicted_emotional_13~D$sex, FUN = sd)[2,2]),2)
ttest_sexTable_edit$sdGirls[4] <- round((aggregate(D$EMPQ_emotional_13~D$sex, FUN = sd)[2,2]),2)

ttest_sexTable_edit$sdGirls[5] <- round((aggregate(D$predicted_cognitive_11~D$sex, FUN = sd)[2,2]),2)
ttest_sexTable_edit$sdGirls[6] <- round((aggregate(D$EMPQ_cognitive_11~D$sex, FUN = sd)[2,2]),2)
ttest_sexTable_edit$sdGirls[7] <- round((aggregate(D$predicted_cognitive_13~D$sex, FUN = sd)[2,2]),2)
ttest_sexTable_edit$sdGirls[8] <- round((aggregate(D$EMPQ_cognitive_13~D$sex, FUN = sd)[2,2]),2)

ttest_sexTable_edit$`Mean boys` <- paste(as.character(ttest_sexTable_edit$`Mean boys`),"(",
                                         as.character (ttest_sexTable_edit$sdBoys),")")
ttest_sexTable_edit$`Mean girls` <- paste(as.character(ttest_sexTable_edit$`Mean girls`),"(",
                                         as.character (ttest_sexTable_edit$sdGirls),")")
ttest_sexTable_edit$`t value` <- paste(as.character(ttest_sexTable_edit$`t value`),"(",
                                       as.character (ttest_sexTable_edit$df),")")

for (i in 1:8) { ifelse(ttest_sexTable_edit$`p-value`[i]<.0001,
      ttest_sexTable_edit$`t value`[i]<- paste0 (ttest_sexTable_edit$`t value`[i],"***"),
        ifelse(ttest_sexTable_edit$`p-value`[i]<.01,
               ttest_sexTable_edit$`t value`[i]<- paste0 (ttest_sexTable_edit$`t value`[i],"**"), 
                  ttest_sexTable_edit$`t value`[i]<- ttest_sexTable_edit$`t value`[i]))}

df_sex <- apply(ttest_sexTable_edit[,c(1,2,3,6)],2,as.character)

write.csv(df_sex, file= "ttest_sex.csv", row.names = F)

#source

#anova 3 groups age 13
oneway.test(EMPQ_emotional_13~OnlineManual13, data=D)
oneway.test(EMPQ_cognitive_13~OnlineManual13, data=D)
oneway.test(predicted_emotional_13~OnlineManual13, data=D)
oneway.test(predicted_cognitive_13~OnlineManual13, data=D)
#sex makes a difference but source doesn't (except for predicted cognitive at age 13). 

library(psych)
describeBy(data=D, predicted_cognitive_13~OnlineManual13)

#ttest only for the difference between the short and long online versions:
t.test(predicted_emotional_13~OnlineManual13, data=D[D$OnlineManual13<=1,])
t.test(EMPQ_emotional_13~OnlineManual13, data=D[D$OnlineManual13<=1,])
t.test(predicted_cognitive_13~OnlineManual13, data=D[D$OnlineManual13<=1,])
t.test(EMPQ_cognitive_13~OnlineManual13, data=D[D$OnlineManual13<=1,])
#no difference

#unite the two online groups
D$OnlineManual13_2G <- D$OnlineManual13
D$OnlineManual13_2G[D$OnlineManual13==0] <-"online"
D$OnlineManual13_2G[D$OnlineManual13==1] <-"online"
D$OnlineManual13_2G[D$OnlineManual13==2] <-"paper"

#source
ttest_sourcePredemo11 <- t.test(predicted_emotional_11~OnlineManual, data=D)
ttest_sourceEmpemo11 <-t.test(EMPQ_emotional_11~ OnlineManual, data=D)
ttest_sourcePredcog11 <- t.test(predicted_cognitive_11~ OnlineManual, data=D)
ttest_sourceEmpcog11 <- t.test(EMPQ_cognitive_11~ OnlineManual, data=D)

ttest_sourcePredemo13 <- t.test(predicted_emotional_13~ OnlineManual13_2G, data=D)
ttest_sourceEmpemo13 <-t.test(EMPQ_emotional_13~ OnlineManual13_2G, data=D)
ttest_sourcePredcog13 <- t.test(predicted_cognitive_13~ OnlineManual13_2G, data=D)
ttest_sourceEmpcog13 <- t.test(EMPQ_cognitive_13~ OnlineManual13_2G, data=D)


ttest_sourceTable <- map_df(list(ttest_sourcePredemo11,ttest_sourceEmpemo11,
                                 ttest_sourcePredemo13,ttest_sourceEmpemo13,
                                 ttest_sourcePredcog11, ttest_sourceEmpcog11,
                                 ttest_sourcePredcog13, ttest_sourceEmpcog13), tidy)

ttest_sourceTable_edit <- ttest_sourceTable[c("estimate1", "estimate2","statistic","parameter","p.value")]
colnames(ttest_sourceTable_edit) <- c("MeanOnline","MeanPaper", "t_value","df","p_value")
ttest_sourceTable_edit[,1:4] <- round(ttest_sourceTable_edit[,1:4],2)
ttest_sourceTable_edit[,5] <- round(ttest_sourceTable_edit[,5],3)


#add effect size
ttest_sourceTable_edit$cohenD <- NA
library(lsr)
ttest_sourceTable_edit$cohenD[1] <- round(cohensD(predicted_emotional_11~OnlineManual, data=D),2)
ttest_sourceTable_edit$cohenD[2] <- round(cohensD(EMPQ_emotional_11~OnlineManual, data=D),2)
ttest_sourceTable_edit$cohenD[3] <- round(cohensD(predicted_emotional_13~OnlineManual13_2G, data=D),2)
ttest_sourceTable_edit$cohenD[4] <- round(cohensD(EMPQ_emotional_13~OnlineManual13_2G, data=D),2)

ttest_sourceTable_edit$cohenD[5] <- round(cohensD(predicted_cognitive_11~OnlineManual, data=D),2)
ttest_sourceTable_edit$cohenD[6] <- round(cohensD(EMPQ_cognitive_11~OnlineManual, data=D),2)
ttest_sourceTable_edit$cohenD[7] <- round(cohensD(predicted_cognitive_13~OnlineManual13_2G, data=D),2)
ttest_sourceTable_edit$cohenD[8] <- round(cohensD(EMPQ_cognitive_13~OnlineManual13_2G, data=D),2)


#add sd
ttest_sourceTable_edit$sdOnline <- NA
ttest_sourceTable_edit$sdPaper <- NA

ttest_sourceTable_edit$sdOnline[1] <- round((aggregate(D$predicted_emotional_11~D$OnlineManual, FUN = sd)[1,2]),2)
ttest_sourceTable_edit$sdOnline[2] <- round((aggregate(D$EMPQ_emotional_11~D$OnlineManual, FUN = sd)[1,2]),2)
ttest_sourceTable_edit$sdOnline[3] <- round((aggregate(D$predicted_emotional_13~D$OnlineManual13_2G, FUN = sd)[1,2]),2)
ttest_sourceTable_edit$sdOnline[4] <- round((aggregate(D$EMPQ_emotional_13~D$OnlineManual13_2G, FUN = sd)[1,2]),2)

ttest_sourceTable_edit$sdOnline[5] <- round((aggregate(D$predicted_cognitive_11~D$OnlineManual, FUN = sd)[1,2]),2)
ttest_sourceTable_edit$sdOnline[6] <- round((aggregate(D$EMPQ_cognitive_11~D$OnlineManual, FUN = sd)[1,2]),2)
ttest_sourceTable_edit$sdOnline[7] <- round((aggregate(D$predicted_cognitive_13~D$OnlineManual13_2G, FUN = sd)[1,2]),2)
ttest_sourceTable_edit$sdOnline[8] <- round((aggregate(D$EMPQ_cognitive_13~D$OnlineManual13_2G, FUN = sd)[1,2]),2)

ttest_sourceTable_edit$sdPaper[1] <- round((aggregate(D$predicted_emotional_11~D$OnlineManual, FUN = sd)[2,2]),2)
ttest_sourceTable_edit$sdPaper[2] <- round((aggregate(D$EMPQ_emotional_11~D$OnlineManual, FUN = sd)[2,2]),2)
ttest_sourceTable_edit$sdPaper[3] <- round((aggregate(D$predicted_emotional_13~D$OnlineManual13_2G, FUN = sd)[2,2]),2)
ttest_sourceTable_edit$sdPaper[4] <- round((aggregate(D$EMPQ_emotional_13~D$OnlineManual13_2G, FUN = sd)[2,2]),2)

ttest_sourceTable_edit$sdPaper[5] <- round((aggregate(D$predicted_cognitive_11~D$OnlineManual, FUN = sd)[2,2]),2)
ttest_sourceTable_edit$sdPaper[6] <- round((aggregate(D$EMPQ_cognitive_11~D$OnlineManual, FUN = sd)[2,2]),2)
ttest_sourceTable_edit$sdPaper[7] <- round((aggregate(D$predicted_cognitive_13~D$OnlineManual13_2G, FUN = sd)[2,2]),2)
ttest_sourceTable_edit$sdPaper[8] <- round((aggregate(D$EMPQ_cognitive_13~D$OnlineManual13_2G, FUN = sd)[2,2]),2)

ttest_sourceTable_edit$MeanOnline <- paste(as.character(ttest_sourceTable_edit$MeanOnline),"(",
                                         as.character (ttest_sourceTable_edit$sdOnline),")")
ttest_sourceTable_edit$MeanPaper <- paste(as.character(ttest_sourceTable_edit$MeanPaper),"(",
                                          as.character (ttest_sourceTable_edit$sdPaper),")")
ttest_sourceTable_edit$t_value <- paste(as.character(ttest_sourceTable_edit$t_value),"(",
                                       as.character (ttest_sourceTable_edit$df),")")

for (i in 1:8) { ifelse(ttest_sourceTable_edit$p_value[i]<.001,
                        ttest_sourceTable_edit$t_value[i]<- paste0 (ttest_sourceTable_edit$t_value[i],"***"),
                        ifelse(ttest_sourceTable_edit$p_value[i]<.01,
                               ttest_sourceTable_edit$t_value[i]<- paste0 (ttest_sourceTable_edit$t_value[i],"**"),
                               ifelse(ttest_sourceTable_edit$p_value[i]<.05,
                                      ttest_sourceTable_edit$t_value[i]<- paste0 (ttest_sourceTable_edit$t_value[i],"*"),
                               ttest_sourceTable_edit$t_value[i]<- ttest_sourceTable_edit$t_value[i])))}

df_source <- apply(ttest_sourceTable_edit[,c(1,2,3,6)],2,as.character)

write.csv(df_source, file= "ttest_source.csv", row.names = F)


#examining correlations with age within each wave

setwd("C:/Users/ShaiW/Documents/Lior computer/empathy and puberty/data/Final data files")  
participants_ages11 <- read.csv ("Age11_participants_age.csv")
participants_ages13 <- read.csv ("Age13_participants_age.csv")

library(reshape)
participants_ages11 <- rename(participants_ages11, c(ï..ifam="ifam"))    # change weird variable names
participants_ages13 <- rename(participants_ages13, c(ï..ifam="ifam"))    # change weird variable names

D<- merge(x=D, y=participants_ages11, by=c("ifam","ID"), all.x=T, all.y=F )     
D<- merge(x=D, y=participants_ages13, by=c("ifam","ID"), all.x=T, all.y=F ) 

library(apaTables)
col=colnames(D)
relvar_agecor11 <- c(which (col=="Age11"),
                     which (col=="predicted_emotional_11"),which(col=="predicted_cognitive_11"),
                     which (col=="EMPQ_emotional_11"),which(col=="EMPQ_cognitive_11"))
                     
apa.cor.table(D[,relvar_agecor11])

relvar_agecor13 <- c(which (col=="Age13"),
                     which (col=="predicted_emotional_13"),which(col=="predicted_cognitive_13"),
                     which (col=="EMPQ_emotional_13"),which(col=="EMPQ_cognitive_13"))

apa.cor.table(D[,relvar_agecor13])


################################################################################################

#standartization according to sex-saving residuals cleaned from sex
#creating the matrix to store the data
col <- colnames(D)
relvar <- c(which (col=="EMPQ_emotional_11"),which(col=="EMPQ_cognitive_11"),
            which(col=="predicted_emotional_11"), which(col=="predicted_cognitive_11"),
            which (col=="EMPQ_emotional_13"),which(col=="EMPQ_cognitive_13"),
            which(col=="predicted_emotional_13"), which(col=="predicted_cognitive_13"),
            which(col=="EXTRAVERSION_11"), which(col=="AGREEABLE_11"),which(col=="CONSCIENTIOUS_11"),
            which(col=="NEUROTICISM_11"), which(col=="OPENESS_11"),
            which(col=="EXTRAVERSION_13"), which(col=="AGREEABLE_13"),which(col=="CONSCIENTIOUS_13"),
            which(col=="NEUROTICISM_13"), which(col=="OPENESS_13"))

residvec <- paste0(colnames(D[,relvar]),"r")
resid.matrix <- as.data.frame(matrix(data= NA, nrow =nrow(D),ncol =length(relvar)))
colnames(resid.matrix) <-residvec

#creating the residual scores
for (i in 1:length(relvar)) {
  reg <- lm(D[,relvar[i]]~sex, data=D, na.action=na.exclude)
  resid.matrix[,i] <-resid(reg)}
D <-cbind(D,resid.matrix)

#Converting dataset to wide format
library(reshape)
DWide <- reshape(D, timevar = "ID", idvar = "ifam", direction = "wide", 
                      v.names = colnames(D[-c(1,2,which(colnames(D)=="zygosity"))]))
 
newnames <- gsub(x=colnames(DWide),pattern="[.]", replacement="_")
newnames <- gsub(x=newnames,pattern="_4", replacement="_2")
newnames <- gsub(x=newnames,pattern="_11", replacement="11")
newnames <- gsub(x=newnames,pattern="_13", replacement="13")
colnames(DWide) <- newnames

#check raw correlations regardless of sex just for understanding
library("umx")
rawDescript<- umxSummarizeTwinData(DWide, 
                                   selVars=c("EMPQ_emotional11","EMPQ_cognitive11","predicted_emotional11","predicted_cognitive11",
                                             "EMPQ_emotional13","EMPQ_cognitive13","predicted_emotional13","predicted_cognitive13"),
                                   zyg="zygosity", 
                                   sep="_",MZ=1, DZ=2)

#check difference between DZS and DZO
#notice that I put the DZS in the MZ command and the DZO in the DZ command
rawDescript_DZS_DZO<- umxSummarizeTwinData(DWide, 
                                           selVars=c("EMPQ_emotional11","EMPQ_cognitive11","predicted_emotional11","predicted_cognitive11",
                                                     "EMPQ_emotional13","EMPQ_cognitive13","predicted_emotional13","predicted_cognitive13"),
                                           zyg="zygosity", 
                                           sep="_",MZ=2, DZ=3)

#change colnames
colnames(rawDescript_DZS_DZO)<- c("Var","Mean","SD", "rDZS","rDZO")


#create different datasets for the 3 zygosity groups (regardless of sex)
Dmz  = subset(DWide, zygosity == 1)
Ddzs = subset(DWide, zygosity == 2)
Ddzo = subset(DWide, zygosity == 3)

#computing ICC for dzs and dzo (almost sure I get ICC1 with this function. need to make sure)
col <- colnames(DWide)

library(irr)
icc_EMPemo_MZ11  <- icc(Dmz[,c(which(col=="EMPQ_emotional11_1"),which(col=="EMPQ_emotional11_2"))],model="twoway", unit="single")
icc_EMPemo_DZS11 <- icc(Ddzs[,c(which(col=="EMPQ_emotional11_1"),which(col=="EMPQ_emotional11_2"))],model="twoway", unit="single")
icc_EMPemo_DZO11 <- icc(Ddzo[,c(which(col=="EMPQ_emotional11_1"),which(col=="EMPQ_emotional11_2"))],model="twoway", unit="single")

icc_EMPcog_MZ11  <- icc(Dmz[,c(which(col=="EMPQ_cognitive11_1"),which(col=="EMPQ_cognitive11_2"))],model="twoway", unit="single")
icc_EMPcog_DZS11 <-icc(Ddzs[,c(which(col=="EMPQ_cognitive11_1"),which(col=="EMPQ_cognitive11_2"))],model="twoway", unit="single")
icc_EMPcog_DZO11 <-icc(Ddzo[,c(which(col=="EMPQ_cognitive11_1"),which(col=="EMPQ_cognitive11_2"))],model="twoway", unit="single")

icc_PREDemo_MZ11 <-icc(Dmz[,c(which(col=="predicted_emotional11_1"),which(col=="predicted_emotional11_2"))],model="twoway", unit="single")
icc_PREDemo_DZS11 <-icc(Ddzs[,c(which(col=="predicted_emotional11_1"),which(col=="predicted_emotional11_2"))],model="twoway", unit="single")
icc_PREDemo_DZO11 <-icc(Ddzo[,c(which(col=="predicted_emotional11_1"),which(col=="predicted_emotional11_2"))],model="twoway", unit="single")

icc_PREDcog_MZ11  <- icc(Dmz[,c(which(col=="predicted_cognitive11_1"),which(col=="predicted_cognitive11_2"))],model="twoway", unit="single")
icc_PREDcog_DZS11 <- icc(Ddzs[,c(which(col=="predicted_cognitive11_1"),which(col=="predicted_cognitive11_2"))],model="twoway", unit="single")
icc_PREDcog_DZO11 <- icc(Ddzo[,c(which(col=="predicted_cognitive11_1"),which(col=="predicted_cognitive11_2"))],model="twoway", unit="single")

library(psych)
#fisher Z comparison between DZS and DZo ICC
fishZ_empEmo11  <- r.test(n=icc_EMPemo_DZS11$subjects, n2=icc_EMPemo_DZO11$subjects,r12=icc_EMPemo_DZS11$value,  r34=icc_EMPemo_DZO11$value) #emotional empathy
fishZ_empCog11  <- r.test(n=icc_EMPcog_DZS11$subjects, n2=icc_EMPcog_DZO11$subjects,r12=icc_EMPcog_DZS11$value,  r34=icc_EMPcog_DZO11$value) #cognitive empathy
fishZ_predEmo11 <- r.test(n=icc_PREDemo_DZS11$subjects, n2=icc_PREDemo_DZO11$subjects,r12=icc_PREDemo_DZS11$value,  r34=icc_PREDemo_DZO11$value) #predicted emotional
fishZ_predCog11 <- r.test(n=icc_PREDcog_DZS11$subjects, n2=icc_PREDcog_DZO11$subjects,r12=icc_PREDcog_DZS11$value,  r34=icc_PREDcog_DZO11$value) #predicted cognitive

#age 13
icc_EMPemo_MZ13  <- icc(Dmz[,c(which(col=="EMPQ_emotional13_1"),which(col=="EMPQ_emotional13_2"))],model="twoway", unit="single")
icc_EMPemo_DZS13 <- icc(Ddzs[,c(which(col=="EMPQ_emotional13_1"),which(col=="EMPQ_emotional13_2"))],model="twoway", unit="single")
icc_EMPemo_DZO13 <- icc(Ddzo[,c(which(col=="EMPQ_emotional13_1"),which(col=="EMPQ_emotional13_2"))],model="twoway", unit="single")

icc_EMPcog_MZ13  <- icc(Dmz[,c(which(col=="EMPQ_cognitive13_1"),which(col=="EMPQ_cognitive13_2"))],model="twoway", unit="single")
icc_EMPcog_DZS13 <-icc(Ddzs[,c(which(col=="EMPQ_cognitive13_1"),which(col=="EMPQ_cognitive13_2"))],model="twoway", unit="single")
icc_EMPcog_DZO13 <-icc(Ddzo[,c(which(col=="EMPQ_cognitive13_1"),which(col=="EMPQ_cognitive13_2"))],model="twoway", unit="single")

icc_PREDemo_MZ13 <-icc(Dmz[,c(which(col=="predicted_emotional13_1"),which(col=="predicted_emotional13_2"))],model="twoway", unit="single")
icc_PREDemo_DZS13 <-icc(Ddzs[,c(which(col=="predicted_emotional13_1"),which(col=="predicted_emotional13_2"))],model="twoway", unit="single")
icc_PREDemo_DZO13 <-icc(Ddzo[,c(which(col=="predicted_emotional13_1"),which(col=="predicted_emotional13_2"))],model="twoway", unit="single")

icc_PREDcog_MZ13  <- icc(Dmz[,c(which(col=="predicted_cognitive13_1"),which(col=="predicted_cognitive13_2"))],model="twoway", unit="single")
icc_PREDcog_DZS13 <- icc(Ddzs[,c(which(col=="predicted_cognitive13_1"),which(col=="predicted_cognitive13_2"))],model="twoway", unit="single")
icc_PREDcog_DZO13 <- icc(Ddzo[,c(which(col=="predicted_cognitive13_1"),which(col=="predicted_cognitive13_2"))],model="twoway", unit="single")

#fisher Z comparison between DZS and DZo ICC
fishZ_empEmo13  <- r.test(n=icc_EMPemo_DZS13$subjects, n2=icc_EMPemo_DZO13$subjects,r12=icc_EMPemo_DZS13$value,  r34=icc_EMPemo_DZO13$value) #emotional empathy
fishZ_empCog13  <- r.test(n=icc_EMPcog_DZS13$subjects, n2=icc_EMPcog_DZO13$subjects,r12=icc_EMPcog_DZS13$value,  r34=icc_EMPcog_DZO13$value) #cognitive empathy
fishZ_predEmo13 <- r.test(n=icc_PREDemo_DZS13$subjects, n2=icc_PREDemo_DZO13$subjects,r12=icc_PREDemo_DZS13$value,  r34=icc_PREDemo_DZO13$value) #predicted emotional
fishZ_predCog13 <- r.test(n=icc_PREDcog_DZS13$subjects, n2=icc_PREDcog_DZO13$subjects,r12=icc_PREDcog_DZS13$value,  r34=icc_PREDcog_DZO13$value) #predicted cognitive

#except for emotional empathy in age 13, there is no significant difference between DZS and DZO.
#emotional empathy in age 13 also showed the highest mean differene between girls and boys
#on the other hand, in age 11 there is no difference between MZ and DZS either
#(in age 13 there is except for emotional empathy)
#this is probably a very restrictive test

#planting Fisher Z results in a table
fishZ_table <- matrix(nrow=8,ncol=4)
fishZ_table[,1] <- c("Emotional empathic personality disposition 11","Emotional empathy 11",
                     "Emotional empathic personality disposition 13","Emotional empathy 13",
                     "Cognitive empathic personality disposition 11","Cognitive empathy 11",
                     "Cognitive empathic personality disposition 13","Cognitive empathy 13")

fishZ_table[1,2] <- paste(as.character(round(icc_PREDemo_DZS11$value,2)),"[",
                          as.character(round(icc_PREDemo_DZS11$lbound,2)),"-",
                          as.character(round(icc_PREDemo_DZS11$ubound,2)),"]") 

fishZ_table[2,2] <- paste(as.character(round(icc_EMPemo_DZS11$value,2)),"[",
                          as.character(round(icc_EMPemo_DZS11$lbound,2)),"-",
                          as.character(round(icc_EMPemo_DZS11$ubound,2)),"]")

fishZ_table[3,2] <- paste(as.character(round(icc_PREDemo_DZS13$value,2)),"[",
                          as.character(round(icc_PREDemo_DZS13$lbound,2)),"-",
                          as.character(round(icc_PREDemo_DZS13$ubound,2)),"]") 

fishZ_table[4,2] <- paste(as.character(round(icc_EMPemo_DZS13$value,2)),"[",
                          as.character(round(icc_EMPemo_DZS13$lbound,2)),"-",
                          as.character(round(icc_EMPemo_DZS13$ubound,2)),"]")


fishZ_table[5,2] <- paste(as.character(round(icc_PREDcog_DZS11$value,2)),"[",
                          as.character(round(icc_PREDcog_DZS11$lbound,2)),"-",
                          as.character(round(icc_PREDcog_DZS11$ubound,2)),"]")

fishZ_table[6,2] <- paste(as.character(round(icc_EMPcog_DZS11$value,2)),"[",
                          as.character(round(icc_EMPcog_DZS11$lbound,2)),"-",
                          as.character(round(icc_EMPcog_DZS11$ubound,2)),"]")

fishZ_table[7,2] <- paste(as.character(round(icc_PREDcog_DZS13$value,2)),"[",
                          as.character(round(icc_PREDcog_DZS13$lbound,2)),"-",
                          as.character(round(icc_PREDcog_DZS13$ubound,2)),"]")

fishZ_table[8,2] <- paste(as.character(round(icc_EMPcog_DZS13$value,2)),"[",
                          as.character(round(icc_EMPcog_DZS13$lbound,2)),"-",
                          as.character(round(icc_EMPcog_DZS13$ubound,2)),"]")

fishZ_table[1,3] <- paste(as.character(round(icc_PREDemo_DZO11$value,2)),"[",
                          as.character(round(icc_PREDemo_DZO11$lbound,2)),"-",
                          as.character(round(icc_PREDemo_DZO11$ubound,2)),"]") 

fishZ_table[2,3] <- paste(as.character(round(icc_EMPemo_DZO11$value,2)),"[",
                          as.character(round(icc_EMPemo_DZO11$lbound,2)),"-",
                          as.character(round(icc_EMPemo_DZO11$ubound,2)),"]")

fishZ_table[3,3] <- paste(as.character(round(icc_PREDemo_DZO13$value,2)),"[",
                          as.character(round(icc_PREDemo_DZO13$lbound,2)),"-",
                          as.character(round(icc_PREDemo_DZO13$ubound,2)),"]") 

fishZ_table[4,3] <- paste(as.character(round(icc_EMPemo_DZO13$value,2)),"[",
                          as.character(round(icc_EMPemo_DZO13$lbound,2)),"-",
                          as.character(round(icc_EMPemo_DZO13$ubound,2)),"]")


fishZ_table[5,3] <- paste(as.character(round(icc_PREDcog_DZO11$value,2)),"[",
                          as.character(round(icc_PREDcog_DZO11$lbound,2)),"-",
                          as.character(round(icc_PREDcog_DZO11$ubound,2)),"]")

fishZ_table[6,3] <- paste(as.character(round(icc_EMPcog_DZO11$value,2)),"[",
                          as.character(round(icc_EMPcog_DZO11$lbound,2)),"-",
                          as.character(round(icc_EMPcog_DZO11$ubound,2)),"]")

fishZ_table[7,3] <- paste(as.character(round(icc_PREDcog_DZO13$value,2)),"[",
                          as.character(round(icc_PREDcog_DZO13$lbound,2)),"-",
                          as.character(round(icc_PREDcog_DZO13$ubound,2)),"]")

fishZ_table[8,3] <- paste(as.character(round(icc_EMPcog_DZO13$value,2)),"[",
                          as.character(round(icc_EMPcog_DZO13$lbound,2)),"-",
                          as.character(round(icc_EMPcog_DZO13$ubound,2)),"]")


fishZ_table[1,4] <- round(fishZ_predEmo11$z,2)
fishZ_table[2,4] <- round(fishZ_empEmo11$z,2)
fishZ_table[3,4] <- round(fishZ_predEmo13$z,2) 
fishZ_table[4,4] <- round(fishZ_empEmo13$z,2)
fishZ_table[5,4] <- round(fishZ_predCog11$z,2)
fishZ_table[6,4] <- round(fishZ_empCog11$z,2)
fishZ_table[7,4] <- round(fishZ_predCog13$z,2) 
fishZ_table[8,4] <- round(fishZ_empCog13$z,2)

colnames(fishZ_table) <- c("variable","DZ-same sex", "DZ-opposite sex","Z value")
write.csv(fishZ_table, file= "fisherZ_DZ.csv", row.names = F)

#checking again the raw correlations with the resid scores
rawDescript_r<- umxSummarizeTwinData(DWide, 
                                     selVars=c("EMPQ_emotional11r","EMPQ_cognitive11r","predicted_emotional11r","predicted_cognitive11r",
                                               "EMPQ_emotional13r","EMPQ_cognitive13r","predicted_emotional13r","predicted_cognitive13r"),
                                     zyg="zygosity", 
                                     sep="_",MZ=1, DZ=2)

#notice that I put the DZS in the MZ command and the DZO in the DZ command
rawDescript_DZS_DZO_r<- umxSummarizeTwinData(DWide, 
                                             selVars=c("EMPQ_emotional11r","EMPQ_cognitive11r","predicted_emotional11r","predicted_cognitive11r",
                                                       "EMPQ_emotional13r","EMPQ_cognitive13r","predicted_emotional13r","predicted_cognitive13r" ),
                                             zyg="zygosity", 
                                             sep="_",MZ=2, DZ=3)

#change colnames
colnames(rawDescript_DZS_DZO_r)<- c("Var","Mean","SD", "rDZS","rDZO)")

#uniting DZS and DZO
DWide$zygosity_2G <- DWide$zygosity
DWide$zygosity_2G[DWide$zygosity==1] <-"MZ"
DWide$zygosity_2G[DWide$zygosity==2] <-"DZ"
DWide$zygosity_2G[DWide$zygosity==3] <-"DZ"

#checking raw correlations
rawDescript_r_DZunite<- umxSummarizeTwinData(DWide, 
                                     selVars=c("EMPQ_emotional11r","EMPQ_cognitive11r","predicted_emotional11r","predicted_cognitive11r",
                                               "EMPQ_emotional13r","EMPQ_cognitive13r","predicted_emotional13r","predicted_cognitive13r" ),
                                     zyg="zygosity_2G", 
                                     sep="_",MZ="MZ", DZ="DZ")


#compute ICC of the new variables (NOT SCALED IN THE REVISED MANUSCRIPT)
col <- colnames(DWide)

icc_PREDemo_MZ11r  <- icc(DWide[DWide$zygosity_2G=="MZ",c(which(col=="predicted_emotional11r_1"),which(col=="predicted_emotional11r_2"))],model="twoway", unit="single")
icc_PREDemo_DZ11r  <- icc(DWide[DWide$zygosity_2G=="DZ",c(which(col=="predicted_emotional11r_1"),which(col=="predicted_emotional11r_2"))],model="twoway", unit="single")


icc_EMPemo_MZ11r  <- icc(DWide[DWide$zygosity_2G=="MZ",c(which(col=="EMPQ_emotional11r_1"),which(col=="EMPQ_emotional11r_2"))],model="twoway", unit="single")
icc_EMPemo_DZ11r  <- icc(DWide[DWide$zygosity_2G=="DZ",c(which(col=="EMPQ_emotional11r_1"),which(col=="EMPQ_emotional11r_2"))],model="twoway", unit="single")

icc_PREDemo_MZ13r  <- icc(DWide[DWide$zygosity_2G=="MZ",c(which(col=="predicted_emotional13r_1"),which(col=="predicted_emotional13r_2"))],model="twoway", unit="single")
icc_PREDemo_DZ13r  <- icc(DWide[DWide$zygosity_2G=="DZ",c(which(col=="predicted_emotional13r_1"),which(col=="predicted_emotional13r_2"))],model="twoway", unit="single")


icc_EMPemo_MZ13r  <- icc(DWide[DWide$zygosity_2G=="MZ",c(which(col=="EMPQ_emotional13r_1"),which(col=="EMPQ_emotional13r_2"))],model="twoway", unit="single")
icc_EMPemo_DZ13r  <- icc(DWide[DWide$zygosity_2G=="DZ",c(which(col=="EMPQ_emotional13r_1"),which(col=="EMPQ_emotional13r_2"))],model="twoway", unit="single")


icc_PREDcog_MZ11r  <- icc(DWide[DWide$zygosity_2G=="MZ",c(which(col=="predicted_cognitive11r_1"),which(col=="predicted_cognitive11r_2"))],model="twoway", unit="single")
icc_PREDcog_DZ11r  <- icc(DWide[DWide$zygosity_2G=="DZ",c(which(col=="predicted_cognitive11r_1"),which(col=="predicted_cognitive11r_2"))],model="twoway", unit="single")


icc_EMPcog_MZ11r  <- icc(DWide[DWide$zygosity_2G=="MZ",c(which(col=="EMPQ_cognitive11r_1"),which(col=="EMPQ_cognitive11r_2"))],model="twoway", unit="single")
icc_EMPcog_DZ11r  <- icc(DWide[DWide$zygosity_2G=="DZ",c(which(col=="EMPQ_cognitive11r_1"),which(col=="EMPQ_cognitive11r_2"))],model="twoway", unit="single")

icc_PREDcog_MZ13r  <- icc(DWide[DWide$zygosity_2G=="MZ",c(which(col=="predicted_cognitive13r_1"),which(col=="predicted_cognitive13r_2"))],model="twoway", unit="single")
icc_PREDcog_DZ13r  <- icc(DWide[DWide$zygosity_2G=="DZ",c(which(col=="predicted_cognitive13r_1"),which(col=="predicted_cognitive13r_2"))],model="twoway", unit="single")


icc_EMPcog_MZ13r  <- icc(DWide[DWide$zygosity_2G=="MZ",c(which(col=="EMPQ_cognitive13r_1"),which(col=="EMPQ_cognitive13r_2"))],model="twoway", unit="single")
icc_EMPcog_DZ13r  <- icc(DWide[DWide$zygosity_2G=="DZ",c(which(col=="EMPQ_cognitive13r_1"),which(col=="EMPQ_cognitive13r_2"))],model="twoway", unit="single")

#prepare a file only with the final variables for Mx biometric cross-lagged analysis
col <- colnames(DWide)
relvar_4mx <- c(which (col=="ifam"),which(col=="zygosity_2G"),which(col=="sex_2"),which(col=="sex_1"),
                which(col=="predicted_emotional11r_2"),which(col=="predicted_emotional13r_2"),
                which(col=="EMPQ_emotional11r_2"),which(col=="EMPQ_emotional13r_2"),
                which(col=="predicted_cognitive11r_2"),which(col=="predicted_cognitive13r_2"),
                which(col=="EMPQ_cognitive11r_2"),which(col=="EMPQ_cognitive13r_2"),
                which(col=="predicted_emotional11r_1"),which(col=="predicted_emotional13r_1"),
                which(col=="EMPQ_emotional11r_1"),which(col=="EMPQ_emotional13r_1"),
                which(col=="predicted_cognitive11r_1"),which(col=="predicted_cognitive13r_1"),
                which(col=="EMPQ_cognitive11r_1"),which(col=="EMPQ_cognitive13r_1"))

DWide_4Mx <- DWide[,relvar_4mx]
write.csv(DWide_4Mx, file= "DWide_4Mx.csv", row.names = F)                

#################################################################################################
#################################### Univariate models ##########################################
#################################################################################################

Univariate <- function (Dwide, selDVs, dzCr) {
  #dzCr=1 if ACE and .25 if ADE
  M <- umxACE(
    name = "M",
    selDVs,
    sep = "_",
    data = DWide,
    zyg = "zygosity_2G",
    type = "FIML",
    tryHard = "yes",
    dzAr = 0.5, dzCr = dzCr,
    equateMeans = TRUE,
    addStd = TRUE,
    addCI = TRUE)
  
  assign ("M",M, envir = .GlobalEnv)
  }


#predicted emotional-age 11
  Univariate( Dwide=Dwide, selDVs=c("predicted_emotional11r"), dzCr=.25)
  Uni_MpredEmo11 <- M
  umxSummaryACE(Uni_MpredEmo11, se=T, digits=2, showRg = F, CIs = T) #show standardized solution (Need to square for percentages)
  
  sUni_MpredEmo11<- summary(umxConfint(Uni_MpredEmo11, 
                     parm = c("top.a_std","top.c_std","top.e_std"), run=TRUE), verbose = T)
  
  Uni_MpredEmo11_AE <- umxModify(Uni_MpredEmo11, update = "c_r1c1",name = "M", comparison = T)
  sUni_MpredEmo11_AE<- summary(umxConfint(Uni_MpredEmo11_AE, 
                                       parm = c("top.a_std","top.c_std","top.e_std"), run=TRUE), verbose = T)
  
  Uni_MpredEmo11_DE <- umxModify(Uni_MpredEmo11, update = "a_r1c1",name = "M", comparison = T)
  sUni_MpredEmo11_DE<- summary(umxConfint(Uni_MpredEmo11_DE, 
                                          parm = c("top.a_std","top.c_std","top.e_std"), run=TRUE), verbose = T)
  
  Uni_MpredEmo11_E <- umxModify(Uni_MpredEmo11, update = c("a_r1c1","c_r1c1"),name = "M", comparison = T)
  sUni_MpredEmo11_E<- summary(umxConfint(Uni_MpredEmo11_E, 
                                          parm = c("top.a_std","top.c_std","top.e_std"), run=TRUE), verbose = T)
  
  comp_MpredEmo11 <- umxCompare(base=Uni_MpredEmo11, 
                                  comparison= c(Uni_MpredEmo11_AE, Uni_MpredEmo11_DE, Uni_MpredEmo11_E))


#emotional-empathy age 11
  Univariate( Dwide=Dwide, selDVs=c("EMPQ_emotional11r"), dzCr=.25)
  Uni_MempEmo11 <- M
  umxSummaryACE(Uni_MempEmo11, se=T, digits=2, showRg = F, CIs = T) #show standardized solution (Need to square for percentages)

  sUni_MempEmo11<- summary(umxConfint(Uni_MempEmo11, 
                                      parm = c("top.a_std","top.c_std","top.e_std"), run=TRUE), verbose = T)
  
  Uni_MempEmo11_AE <- umxModify(Uni_MempEmo11, update = "c_r1c1",name = "M", comparison = T)
  sUni_MempEmo11_AE<- summary(umxConfint(Uni_MempEmo11_AE, 
                                         parm = c("top.a_std","top.c_std","top.e_std"), run=TRUE), verbose = T)
  
  Uni_MempEmo11_DE <- umxModify(Uni_MempEmo11, update = "a_r1c1",name = "M", comparison = T)
  sUni_MempEmo11_DE<- summary(umxConfint(Uni_MempEmo11_DE, 
                                         parm = c("top.a_std","top.c_std","top.e_std"), run=TRUE), verbose = T)
  
  Uni_MempEmo11_E <- umxModify(Uni_MempEmo11, update = c("a_r1c1","c_r1c1"),name = "M", comparison = T)
  sUni_MempEmo11_E<- summary(umxConfint(Uni_MempEmo11_E, 
                                        parm = c("top.a_std","top.c_std","top.e_std"), run=TRUE), verbose = T)
  
  comp_MempEmo11 <- umxCompare(base=Uni_MempEmo11, 
                                 comparison= c(Uni_MempEmo11_AE, Uni_MempEmo11_DE, Uni_MempEmo11_E))
  

#predicted emotional-age 13
  Univariate( Dwide=Dwide, selDVs=c("predicted_emotional13r"), dzCr=.25)
  Uni_MpredEmo13 <- M

  umxSummaryACE(Uni_MpredEmo13, se=T, digits=2, showRg = F, CIs = T) #show standardized solution (Need to square for percentages)
  sUni_MpredEmo13<- summary(umxConfint(Uni_MpredEmo13, 
                                       parm = c("top.a_std","top.c_std","top.e_std"), run=TRUE), verbose = T)
  
  Uni_MpredEmo13_AE <- umxModify(Uni_MpredEmo13, update = "c_r1c1",name = "M", comparison = T)
  sUni_MpredEmo13_AE<- summary(umxConfint(Uni_MpredEmo13_AE, 
                                          parm = c("top.a_std","top.c_std","top.e_std"), run=TRUE), verbose = T)
  
  Uni_MpredEmo13_DE <- umxModify(Uni_MpredEmo13, update = "a_r1c1",name = "M", comparison = T)
  sUni_MpredEmo13_DE<- summary(umxConfint(Uni_MpredEmo13_DE, 
                                          parm = c("top.a_std","top.c_std","top.e_std"), run=TRUE), verbose = T)
  
  Uni_MpredEmo13_E <- umxModify(Uni_MpredEmo13, update = c("a_r1c1","c_r1c1"),name = "M", comparison = T)
  sUni_MpredEmo13_E<- summary(umxConfint(Uni_MpredEmo13_E, 
                                         parm = c("top.a_std","top.c_std","top.e_std"), run=TRUE), verbose = T)
  
  comp_MpredEmo13 <- umxCompare(base=Uni_MpredEmo13, 
                                  comparison= c(Uni_MpredEmo13_AE, Uni_MpredEmo13_DE, Uni_MpredEmo13_E))
  
  
#emotional-empathy age 13
  Univariate( Dwide=Dwide, selDVs=c("EMPQ_emotional13r"), dzCr=.25)
  Uni_MempEmo13 <- M
  
  umxSummaryACE(Uni_MempEmo13, se=T, digits=2, showRg = F, CIs = T) #show standardized solution (Need to square for percentages)
  sUni_MempEmo13<- summary(umxConfint(Uni_MempEmo13, 
                                      parm = c("top.a_std","top.c_std","top.e_std"), run=TRUE), verbose = T)
  
  Uni_MempEmo13_AE <- umxModify(Uni_MempEmo13, update = "c_r1c1",name = "M", comparison = T)
  sUni_MempEmo13_AE<- summary(umxConfint(Uni_MempEmo13_AE, 
                                         parm = c("top.a_std","top.c_std","top.e_std"), run=TRUE), verbose = T)
  
  Uni_MempEmo13_DE <- umxModify(Uni_MempEmo13, update = "a_r1c1",name = "M", comparison = T)
  sUni_MempEmo13_DE<- summary(umxConfint(Uni_MempEmo13_DE, 
                                         parm = c("top.a_std","top.c_std","top.e_std"), run=TRUE), verbose = T)
  
  Uni_MempEmo13_E <- umxModify(Uni_MempEmo13, update = c("a_r1c1","c_r1c1"),name = "M", comparison = T)
  sUni_MempEmo13_E<- summary(umxConfint(Uni_MempEmo13_E, 
                                        parm = c("top.a_std","top.c_std","top.e_std"), run=TRUE), verbose = T)
  
  comp_MempEmo13 <- umxCompare(base=Uni_MempEmo13, 
                                 comparison= c(Uni_MempEmo13_AE, Uni_MempEmo13_DE, Uni_MempEmo13_E))
  

#predicted cognitive-age 11
  Univariate( Dwide=Dwide, selDVs=c("predicted_cognitive11r"), dzCr=.25)
  Uni_MpredCog11 <- M
  
  umxSummaryACE(Uni_MpredCog11, se=T, digits=2, showRg = F, CIs = T) #show standardized solution (Need to square for percentages)
  sUni_MpredCog11<- summary(umxConfint(Uni_MpredCog11, 
                                       parm = c("top.a_std","top.c_std","top.e_std"), run=TRUE), verbose = T)
  
  Uni_MpredCog11_AE <- umxModify(Uni_MpredCog11, update = "c_r1c1",name = "M", comparison = T)
  sUni_MpredCog11_AE<- summary(umxConfint(Uni_MpredCog11_AE, 
                                          parm = c("top.a_std","top.c_std","top.e_std"), run=TRUE), verbose = T)
  
  Uni_MpredCog11_DE <- umxModify(Uni_MpredCog11, update = "a_r1c1",name = "M", comparison = T)
  sUni_MpredCog11_DE<- summary(umxConfint(Uni_MpredCog11_DE, 
                                          parm = c("top.a_std","top.c_std","top.e_std"), run=TRUE), verbose = T)
  
  Uni_MpredCog11_E <- umxModify(Uni_MpredCog11, update = c("a_r1c1","c_r1c1"),name = "M", comparison = T)
  sUni_MpredCog11_E<- summary(umxConfint(Uni_MpredCog11_E, 
                                         parm = c("top.a_std","top.c_std","top.e_std"), run=TRUE), verbose = T)
  
  comp_MpredCog11 <- umxCompare(base=Uni_MpredCog11, 
                                  comparison= c(Uni_MpredCog11_AE, Uni_MpredCog11_DE, Uni_MpredCog11_E))
  
  
#cognitive empathy age 11
  Univariate( Dwide=Dwide, selDVs=c("EMPQ_cognitive11r"), dzCr=.25)
  Uni_MempCog11 <- M
  
  umxSummaryACE(Uni_MempCog11, se=T, digits=2, showRg = F, CIs = T) #show standardized solution (Need to square for percentages)
  sUni_MempCog11<- summary(umxConfint(Uni_MempCog11, 
                                      parm = c("top.a_std","top.c_std","top.e_std"), run=TRUE), verbose = T)
  
  Uni_MempCog11_AE <- umxModify(Uni_MempCog11, update = "c_r1c1",name = "M", comparison = T)
  sUni_MempCog11_AE<- summary(umxConfint(Uni_MempCog11_AE, 
                                         parm = c("top.a_std","top.c_std","top.e_std"), run=TRUE), verbose = T)
  
  Uni_MempCog11_DE <- umxModify(Uni_MempCog11, update = "a_r1c1",name = "M", comparison = T)
  sUni_MempCog11_DE<- summary(umxConfint(Uni_MempCog11_DE, 
                                         parm = c("top.a_std","top.c_std","top.e_std"), run=TRUE), verbose = T)
  
  Uni_MempCog11_E <- umxModify(Uni_MempCog11, update = c("a_r1c1","c_r1c1"),name = "M", comparison = T)
  sUni_MempCog11_E<- summary(umxConfint(Uni_MempCog11_E, 
                                        parm = c("top.a_std","top.c_std","top.e_std"), run=TRUE), verbose = T)
  
  comp_MempCog11 <- umxCompare(base=Uni_MempCog11, 
                                 comparison= c(Uni_MempCog11_AE, Uni_MempCog11_DE, Uni_MempCog11_E))
  
  
#predicted cognitive-age 13
  Univariate( Dwide=Dwide, selDVs=c("predicted_cognitive13r"), dzCr=.25)
  Uni_MpredCog13 <- M
  
  umxSummaryACE(Uni_MpredCog13, se=T, digits=2, showRg = F, CIs = T) #show standardized solution (Need to square for percentages)
  sUni_MpredCog13<- summary(umxConfint(Uni_MpredCog13, 
                                       parm = c("top.a_std","top.c_std","top.e_std"), run=TRUE), verbose = T)
  
  Uni_MpredCog13_AE <- umxModify(Uni_MpredCog13, update = "c_r1c1",name = "M", comparison = T)
  sUni_MpredCog13_AE<- summary(umxConfint(Uni_MpredCog13_AE, 
                                          parm = c("top.a_std","top.c_std","top.e_std"), run=TRUE), verbose = T)
  
  Uni_MpredCog13_DE <- umxModify(Uni_MpredCog13, update = "a_r1c1",name = "M", comparison = T)
  sUni_MpredCog13_DE<- summary(umxConfint(Uni_MpredCog13_DE, 
                                          parm = c("top.a_std","top.c_std","top.e_std"), run=TRUE), verbose = T)
  
  Uni_MpredCog13_E <- umxModify(Uni_MpredCog13, update = c("a_r1c1","c_r1c1"),name = "M", comparison = T)
  sUni_MpredCog13_E<- summary(umxConfint(Uni_MpredCog13_E, 
                                         parm = c("top.a_std","top.c_std","top.e_std"), run=TRUE), verbose = T)
  
  comp_MpredCog13 <- umxCompare(base=Uni_MpredCog13, 
                                  comparison= c(Uni_MpredCog13_AE, Uni_MpredCog13_DE, Uni_MpredCog13_E))
  
  
#cognitive empathy age 13
  Univariate( Dwide=Dwide, selDVs=c("EMPQ_cognitive13r"), dzCr=.25)
  Uni_MempCog13 <- M
  
  umxSummaryACE(Uni_MempCog13, se=T, digits=2, showRg = F, CIs = T) #show standardized solution (Need to square for percentages)
  sUni_MempCog13<- summary(umxConfint(Uni_MempCog13, 
                                      parm = c("top.a_std","top.c_std","top.e_std"), run=TRUE), verbose = T)
  
  Uni_MempCog13_AE <- umxModify(Uni_MempCog13, update = "c_r1c1",name = "M", comparison = T)
  sUni_MempCog13_AE<- summary(umxConfint(Uni_MempCog13_AE, 
                                         parm = c("top.a_std","top.c_std","top.e_std"), run=TRUE), verbose = T)
  
  Uni_MempCog13_DE <- umxModify(Uni_MempCog13, update = "a_r1c1",name = "M", comparison = T)
  sUni_MempCog13_DE<- summary(umxConfint(Uni_MempCog13_DE, 
                                         parm = c("top.a_std","top.c_std","top.e_std"), run=TRUE), verbose = T)
  
  Uni_MempCog13_E <- umxModify(Uni_MempCog13, update = c("a_r1c1","c_r1c1"),name = "M", comparison = T)
  sUni_MempCog13_E<- summary(umxConfint(Uni_MempCog13_E, 
                                        parm = c("top.a_std","top.c_std","top.e_std"), run=TRUE), verbose = T)
  
  comp_MempCog13 <- umxCompare(base=Uni_MempCog13, 
                                 comparison= c(Uni_MempCog13_AE, Uni_MempCog13_DE, Uni_MempCog13_E))
  
  
#compare the best model according to AIC
umxWeightedAIC(c(Uni_MpredEmo11,Uni_MpredEmo11_AE,Uni_MpredEmo11_DE,Uni_MpredEmo11_E), digits =2)
umxWeightedAIC(c(Uni_MempEmo11, Uni_MempEmo11_AE, Uni_MempEmo11_DE, Uni_MempEmo11_E), digits =2) 
umxWeightedAIC(c(Uni_MpredEmo13,Uni_MpredEmo13_AE,Uni_MpredEmo13_DE,Uni_MpredEmo13_E), digits =2)  
umxWeightedAIC(c(Uni_MempEmo13, Uni_MempEmo13_AE, Uni_MempEmo13_DE, Uni_MempEmo13_E), digits =2)  

umxWeightedAIC(c(Uni_MpredCog11,Uni_MpredCog11_AE,Uni_MpredCog11_DE,Uni_MpredCog11_E), digits =2)
umxWeightedAIC(c(Uni_MempCog11,Uni_MempCog11_AE,Uni_MempCog11_DE,Uni_MempCog11_E), digits =2)  
umxWeightedAIC(c(Uni_MpredCog13,Uni_MpredCog13_AE,Uni_MpredCog13_DE,Uni_MpredCog13_E), digits =2)
umxWeightedAIC(c(Uni_MempCog13,Uni_MempCog13_AE,Uni_MempCog13_DE,Uni_MempCog13_E), digits =2) 


#plant CI function:
plantCIinTable <- function (CItable, sCI, row, col,estimate,lower,upper) {
  CItable[row,col] <- paste (as.character(round(sCI$CI$estimate[estimate],3)),"[",
                             as.character(round(sCI$CIdetail$value[lower],3)),"-",
                             as.character(round(sCI$CIdetail$value[upper],3)),"]")
  
  assign ("CItable",CItable,envir = .GlobalEnv)
}

#Plant CI of variance components (squared standardized paths)
plantCIinTable_squared <- function (CItable, sCI, row, col,estimate,lower,upper) {
  CItable[row,col] <- paste (as.character(round(sCI$CI$estimate[estimate]^2,2)),"[",
                             as.character(round(sCI$CIdetail$value[lower]^2,2)),"-",
                             as.character(round(sCI$CIdetail$value[upper]^2,2)),"]")
  
  assign ("CItable",CItable,envir = .GlobalEnv)
}


#function- put all vars in one table
plantUnivarinTable <- function (table,smodel, smodel_AE, smodel_DE, smodel_E,variableName, comparisons){
  
  table[1,1] <- variableName
  table[1,2] <- round(smodel$Minus2LogLikelihood,2)
  table[1,3] <- smodel$degreesOfFreedom
  plantCIinTable_squared (table,smodel, row=1,col=8,estimate=1,lower=1,upper=2); table <- CItable
  plantCIinTable_squared (table,smodel, row=1,col=9,estimate=2,lower=3,upper=4); table <- CItable
  plantCIinTable_squared (table,smodel, row=1,col=10,estimate=3,lower=5,upper=6); table <- CItable
  
  table[2,1] <- variableName
  table[2,2] <- round(smodel_AE$Minus2LogLikelihood,2)
  table[2,3] <- smodel_AE$degreesOfFreedom
  plantCIinTable_squared (table,smodel_AE, row=2,col=8,estimate=1,lower=1,upper=2); table <- CItable
  plantCIinTable_squared (table,smodel_AE, row=2,col=9,estimate=2,lower=3,upper=4); table <- CItable
  plantCIinTable_squared (table,smodel_AE, row=2,col=10,estimate=3,lower=5,upper=6); table <- CItable
  
  table[3,1] <- variableName
  table[3,2] <- round(smodel_DE$Minus2LogLikelihood,2)
  table[3,3] <- smodel_DE$degreesOfFreedom
  plantCIinTable_squared (table,smodel_DE, row=3,col=8,estimate=1,lower=1,upper=2); table <- CItable
  plantCIinTable_squared (table,smodel_DE, row=3,col=9,estimate=2,lower=3,upper=4); table <- CItable
  plantCIinTable_squared (table,smodel_DE, row=3,col=10,estimate=3,lower=5,upper=6); table <- CItable
  
  table[4,1] <- variableName
  table[4,2] <- round(smodel_E$Minus2LogLikelihood,2)
  table[4,3] <- smodel_E$degreesOfFreedom
  plantCIinTable_squared (table,smodel_E, row=4,col=8,estimate=1,lower=1,upper=2); table <- CItable
  plantCIinTable_squared (table,smodel_E, row=4,col=9,estimate=2,lower=3,upper=4); table <- CItable
  plantCIinTable_squared (table,smodel_E, row=4,col=10,estimate=3,lower=5,upper=6); table <- CItable
  
  table[,4:7] <-as.matrix(comparisons[,c(3:6)])
  table <- rbind(table,NA)
  
  assign ("table",table,envir = .GlobalEnv)  

}

UnivariateTable_predemo11 <- matrix(nrow = 4, ncol = 10)
plantUnivarinTable(UnivariateTable_predemo11,
                   sUni_MpredEmo11, sUni_MpredEmo11_AE, sUni_MpredEmo11_DE, sUni_MpredEmo11_E,
                   "EE personality disposition 11",comp_MpredEmo11); UnivariateTable_predemo11 <- table

UnivariateTable_empemo11 <- matrix(nrow = 4, ncol = 10)
plantUnivarinTable(UnivariateTable_empemo11,
                   sUni_MempEmo11, sUni_MempEmo11_AE, sUni_MempEmo11_DE, sUni_MempEmo11_E,
                   "EE 11",comp_MempEmo11); UnivariateTable_empemo11 <- table

UnivariateTable_predemo13 <- matrix(nrow = 4, ncol = 10)
plantUnivarinTable(UnivariateTable_predemo13,
                   sUni_MpredEmo13, sUni_MpredEmo13_AE, sUni_MpredEmo13_DE, sUni_MpredEmo13_E,
                   "EE personality disposition 13",comp_MpredEmo13); UnivariateTable_predemo13 <- table

UnivariateTable_empemo13 <- matrix(nrow = 4, ncol = 10)
plantUnivarinTable(UnivariateTable_empemo13,
                   sUni_MempEmo13, sUni_MempEmo13_AE, sUni_MempEmo13_DE, sUni_MempEmo13_E,
                   "EE 13",comp_MempEmo13); UnivariateTable_empemo13 <- table 



UnivariateTable_predcog11 <- matrix(nrow = 4, ncol = 10)
plantUnivarinTable(UnivariateTable_predcog11,
                   sUni_MpredCog11, sUni_MpredCog11_AE, sUni_MpredCog11_DE, sUni_MpredCog11_E,
                   "CE personality disposition 11",comp_MpredCog11); UnivariateTable_predcog11 <- table

UnivariateTable_empcog11 <- matrix(nrow = 4, ncol = 10)
plantUnivarinTable(UnivariateTable_empcog11,
                   sUni_MempCog11, sUni_MempCog11_AE, sUni_MempCog11_DE, sUni_MempCog11_E,
                   "CE 11",comp_MempCog11); UnivariateTable_empcog11 <- table


UnivariateTable_predcog13 <- matrix(nrow = 4, ncol = 10)
plantUnivarinTable(UnivariateTable_predcog13,
                   sUni_MpredCog13, sUni_MpredCog13_AE, sUni_MpredCog13_DE, sUni_MpredCog13_E,
                   "CE personality disposition 13",comp_MpredCog13); UnivariateTable_predcog13 <- table

UnivariateTable_empcog13 <- matrix(nrow = 4, ncol = 10)
plantUnivarinTable(UnivariateTable_empcog13,
                   sUni_MempCog13, sUni_MempCog13_AE, sUni_MempCog13_DE, sUni_MempCog13_E,
                   "CE 13",comp_MempCog13); UnivariateTable_empcog13 <- table

#unite all tables
UnivariateTable <- rbind (UnivariateTable_predemo11,UnivariateTable_empemo11,
                          UnivariateTable_predemo13,UnivariateTable_empemo13,
                          UnivariateTable_predcog11,UnivariateTable_empcog11,
                          UnivariateTable_predcog13,UnivariateTable_empcog13)

colnames(UnivariateTable) <- c("variable","-2LL", "df","dif LL","dif df",
                              "p-value","AIC","a2","d2","e2")

write.csv(UnivariateTable, file= "univariate.csv", row.names = F)

##################################################################################################
#Longitudinal genetic analyses 
##################################################################################################

#about warning code 5 (from openMx forum) 
#"5 means that the Hessian at the solution is not convex. 
#There is likely a better solution, but the optimizer is stuck in a region of 
#confusing geometry (like a saddle point).

#About negative path estimates: according to this thread: https://openmx.ssri.psu.edu/node/4377
#the sign of the paths doesn't matter. It is somehing that helps the model to be identified.
#I can look at the variance (i.e., squared path) regardless of the sign

#this thread: https://openmx.ssri.psu.edu/node/4615 says that while the negative sign of 
#paths may not be a problem, it may lead to confidence intervals for sign-indeterminate 
#parameters for which the lower and upper limits are the same (or nearly the same) in absolute
#magnitude, but opposite in sign. Such confidence intervals should not be interpreted as saying 
#that the parameter is not significantly different from zero.

#about significnace testing in cholesky: according to Verhulst et al (2019) it leads to type 1 
#error. They recommend using the Direct Symetric Approach (I think it means estimate the full
#model but I am not sure). Need to think if it helps. 


#about inconsistencies between standard eror based CIs (Wald-type CIs) and chi-square tests:
#in Friedman 2016 there is  an example and explantion for why Chi test do not always agree with
#the standard errors and why the chi test is better (I do it with log-likelihood but I guess it is the same)


###############################################################################################

#required functions:

#make a table with se Based CIs (Wald-type CIs)
plantSeBasedCI <- function(model){
  
  #extract the standardized estimates (by asking the confidence intervals)
  CI <- umxConfint(model,parm = c("top.a_std","top.c_std","top.e_std"),run = T)
  options(max.print=10000)
  sCI <- summary(CI, verbose = T)
  est <- sCI$CI$estimate
  dim(est) <- c(4,12)
  
  #SE's
  seA <- as.matrix(mxSE("top.a_std", model))
  seD <- as.matrix(mxSE("top.c_std", model))
  seE <- as.matrix(mxSE("top.e_std", model))
  se <- cbind(seA,seD,seE)
  
  seBaseCI <- matrix(nrow=4,ncol=12)
  
  for (i in 1:4){
    for (j in 1:12){
      seBaseCI[i,j] <- paste (round(est[i,j],2), "[",
                              round(est[i,j]-1.96*se[i,j],2),"-",
                              round(est[i,j]+1.96*se[i,j],2),"]")
    }
  }
  
  assign ("seBaseCI",seBaseCI, envir = .GlobalEnv)
}


################################################################################################

#Emotional empathy-personality first

selDVs <- c("predicted_emotional11r", "EMPQ_emotional11r", 
            "predicted_emotional13r", "EMPQ_emotional13r")


MPerEmpEmo <- umxACE(
  name = "Emotional empathy full model",
  selDVs,
  sep = "_",
  data = DWide,
  zyg = "zygosity_2G",
  type = "FIML",
  tryHard = "yes",
  dzAr = 0.5, dzCr = .25,
  equateMeans = TRUE,
  addStd = TRUE,
  addCI = TRUE)

umxSummary(MPerEmpEmo, se=T, digits=3, showRg = T) #standardized solution but not in percentages (need to square everything to get variance percentages)

#create the table
sMPerEmpEmo <- summary(MPerEmpEmo, verbose=T)
plantSeBasedCI(MPerEmpEmo)
seBaseCI_perempEmo <- seBaseCI

seBaseCI_perempEmo <- cbind (c("EE personality disposition 11","EE 11","EE personality disposition 13","EE 13"),
                               seBaseCI_perempEmo)
colnames(seBaseCI_perempEmo) <- c("Variable","A1","A2","A3","A4","D1","D2","D3","D4","E1","E2","E3","E4")
write.csv (seBaseCI_perempEmo, file= "seBaseCI_perempEmo.csv", row.names = F)


## Paths dropping -one path at a time and check its significance

MPerEmpEmo_t1 = umxModify(MPerEmpEmo, update = c("c_r4c4"),name = "Drop D4 on Empathy13", comparison = T) 
MPerEmpEmo_t2 = umxModify(MPerEmpEmo, update = c("c_r4c3"),name = "Drop D3 on Empathy13", comparison = T) 
MPerEmpEmo_t3 = umxModify(MPerEmpEmo, update = c("c_r4c2"),name = "Drop D2 on Empathy13", comparison = T) 
MPerEmpEmo_t4 = umxModify(MPerEmpEmo, update = c("c_r4c1"),name = "Drop D1 on Empathy13", comparison = T) 
MPerEmpEmo_t5 = umxModify(MPerEmpEmo, update = c("c_r3c3"),name = "Drop D3 on Personality13", comparison = T) 
MPerEmpEmo_t6 = umxModify(MPerEmpEmo, update = c("c_r3c2"),name = "Drop D2 on Personality13", comparison = T) 
MPerEmpEmo_t7 = umxModify(MPerEmpEmo, update = c("c_r2c2"),name = "Drop D2 on Empathy11", comparison = T) 
MPerEmpEmo_t8 = umxModify(MPerEmpEmo, update = c("c_r3c1"),name = "Drop D1 on Personality13", comparison = T) 
MPerEmpEmo_t9 = umxModify(MPerEmpEmo, update = c("c_r2c1"),name = "Drop D1 on Empathy11", comparison = T) 
MPerEmpEmo_t10= umxModify(MPerEmpEmo, update = c("c_r1c1"),name = "Drop D1 on Personality11", comparison = T) 

MPerEmpEmo_t11 = umxModify(MPerEmpEmo, update = c("a_r4c4"),name = "Drop A4 on Empathy13", comparison = T) 
MPerEmpEmo_t12 = umxModify(MPerEmpEmo, update = c("a_r4c3"),name = "Drop A3 on Empathy13", comparison = T) 
MPerEmpEmo_t13 = umxModify(MPerEmpEmo, update = c("a_r4c2"),name = "Drop A2 on Empathy13", comparison = T) 
MPerEmpEmo_t14 = umxModify(MPerEmpEmo, update = c("a_r4c1"),name = "Drop A1 on Empathy13", comparison = T) 
MPerEmpEmo_t15 = umxModify(MPerEmpEmo, update = c("a_r3c3"),name = "Drop A3 on Personality13", comparison = T) 
MPerEmpEmo_t16 = umxModify(MPerEmpEmo, update = c("a_r3c2"),name = "Drop A2 on Personality13", comparison = T) 
MPerEmpEmo_t17 = umxModify(MPerEmpEmo, update = c("a_r2c2"),name = "Drop A2 on Empathy11", comparison = T) 
MPerEmpEmo_t18 = umxModify(MPerEmpEmo, update = c("a_r3c1"),name = "Drop A1 on Personality13", comparison = T) 
MPerEmpEmo_t19 = umxModify(MPerEmpEmo, update = c("a_r2c1"),name = "Drop A1 on Empathy11", comparison = T) 
MPerEmpEmo_t20 = umxModify(MPerEmpEmo, update = c("a_r1c1"),name = "Drop A1 on Personality11", comparison = T) 

MPerEmpEmo_t21 = umxModify(MPerEmpEmo, update = c("e_r4c4"),name = "Drop E4 on Empathy13", comparison = T) 
MPerEmpEmo_t22 = umxModify(MPerEmpEmo, update = c("e_r4c3"),name = "Drop E3 on Empathy13", comparison = T) 
MPerEmpEmo_t23 = umxModify(MPerEmpEmo, update = c("e_r4c2"),name = "Drop E2 on Empathy13", comparison = T) 
MPerEmpEmo_t24 = umxModify(MPerEmpEmo, update = c("e_r4c1"),name = "Drop E1 on Empathy13", comparison = T) 
MPerEmpEmo_t25 = umxModify(MPerEmpEmo, update = c("e_r3c3"),name = "Drop E3 on Personality13", comparison = T) 
MPerEmpEmo_t26 = umxModify(MPerEmpEmo, update = c("e_r3c2"),name = "Drop E2 on Personality13", comparison = T) 
MPerEmpEmo_t27 = umxModify(MPerEmpEmo, update = c("e_r2c2"),name = "Drop E2 on Empathy11", comparison = T) 
MPerEmpEmo_t28 = umxModify(MPerEmpEmo, update = c("e_r3c1"),name = "Drop E1 on Personality13", comparison = T) 
MPerEmpEmo_t29 = umxModify(MPerEmpEmo, update = c("e_r2c1"),name = "Drop E1 on Empathy1", comparison = T) 
MPerEmpEmo_t30 = umxModify(MPerEmpEmo, update = c("e_r1c1"),name = "Drop E1 on Personality11", comparison = T) 

#Model comparisons
umxCompare(base = MPerEmpEmo, 
           comparison = c(MPerEmpEmo_t1,MPerEmpEmo_t2,MPerEmpEmo_t3,
                          MPerEmpEmo_t4,MPerEmpEmo_t5,MPerEmpEmo_t6,
                          MPerEmpEmo_t7,MPerEmpEmo_t8,MPerEmpEmo_t9,
                          MPerEmpEmo_t10, MPerEmpEmo_t11, MPerEmpEmo_t12,      
                          MPerEmpEmo_t13, MPerEmpEmo_t14, MPerEmpEmo_t15, 
                          MPerEmpEmo_t16, MPerEmpEmo_t17, MPerEmpEmo_t18, 
                          MPerEmpEmo_t19, MPerEmpEmo_t20, MPerEmpEmo_t21, 
                          MPerEmpEmo_t22, MPerEmpEmo_t23, MPerEmpEmo_t24, 
                          MPerEmpEmo_t25, MPerEmpEmo_t26, MPerEmpEmo_t27, 
                          MPerEmpEmo_t28, MPerEmpEmo_t29, MPerEmpEmo_t30),
           digits=3, report = "html")


#D: D1 is significant on all variables (almost significant on empathy 13)
#A: A is not significant at all, so due to theoretical reasons, I keep only the first A
#E: two paths are not significant and I drop only these two
#conclusion: one common A, one common D, and almost all E's

MPerEmpEmo_t = umxModify(MPerEmpEmo, update = c("c_r4c4",
                                                "c_r4c3","c_r3c3",
                                                "c_r4c2","c_r3c2","c_r2c2",
                                                "a_r4c4",
                                                "a_r4c3","a_r3c3",
                                                "a_r4c2","a_r3c2","a_r2c2",
                                                "e_r4c1","e_r3c2" ),
                        name = "M", comparison = T) 

#create the table
sMPerEmpEmo_t <- summary(MPerEmpEmo_t, verbose=T)
plantSeBasedCI(MPerEmpEmo_t)
seBaseCI_perempEmo_t <- seBaseCI

seBaseCI_perempEmo_t <- cbind (c("EE personality disposition 11","EE 11","EE personality disposition 13","EE 13"),
                               seBaseCI_perempEmo_t)
colnames(seBaseCI_perempEmo_t) <- c("Variable","A1","A2","A3","A4","D1","D2","D3","D4","E1","E2","E3","E4")
write.csv (seBaseCI_perempEmo_t, file= "seBaseCI_perempEmo_t.csv", row.names = F)

#examine paths significance in the final model
MPerEmpEmo_a11 <- umxModify(MPerEmpEmo_t, update = c("a_r1c1"),name = "Drop A1 on personality11", comparison = T) 
MPerEmpEmo_a21 <- umxModify(MPerEmpEmo_t, update = c("a_r2c1"),name = "Drop A1 on empathy11", comparison = T) 
MPerEmpEmo_a31 <- umxModify(MPerEmpEmo_t, update = c("a_r3c1"),name = "Drop A1 on personality13", comparison = T) 
MPerEmpEmo_a41 <- umxModify(MPerEmpEmo_t, update = c("a_r4c1"),name = "Drop A1 on empathy13", comparison = T) 

MPerEmpEmo_c11 <- umxModify(MPerEmpEmo_t, update = c("c_r1c1"),name = "Drop D1 on personality11", comparison = T) 
MPerEmpEmo_c21 <- umxModify(MPerEmpEmo_t, update = c("c_r2c1"),name = "Drop D1 on empathy11`", comparison = T) 
MPerEmpEmo_c31 <- umxModify(MPerEmpEmo_t, update = c("c_r3c1"),name = "Drop D1 on personality13", comparison = T) 
MPerEmpEmo_c41 <- umxModify(MPerEmpEmo_t, update = c("c_r4c1"),name = "Drop D1 on empathy13", comparison = T) 

MPerEmpEmo_e11 <- umxModify(MPerEmpEmo_t, update = c("e_r1c1"),name = "Drop E1 on personlity11", comparison = T) 
MPerEmpEmo_e21 <- umxModify(MPerEmpEmo_t, update = c("e_r2c1"),name = "Drop E1 on empathy11", comparison = T) 
MPerEmpEmo_e31 <- umxModify(MPerEmpEmo_t, update = c("e_r3c1"),name = "Drop E1 on personality13", comparison = T) 
#MPerEmpEmo_e41 <- umxModify(MPerEmpEmo_t, update = c("e_r4c1"),name = "Drop E1 on empathy13", comparison = T) 
MPerEmpEmo_e22 <- umxModify(MPerEmpEmo_t, update = c("e_r2c2"),name = "Drop E2 on empathy11", comparison = T) 
#MPerEmpEmo_e32 <- umxModify(MPerEmpEmo_t, update = c("e_r3c2"),name = "Drop E2 on personality13", comparison = T) 
MPerEmpEmo_e42 <- umxModify(MPerEmpEmo_t, update = c("e_r4c2"),name = "Drop E2 on empathy13", comparison = T) 
MPerEmpEmo_e33 <- umxModify(MPerEmpEmo_t, update = c("e_r3c3"),name = "Drop E3 on personality13", comparison = T) 
MPerEmpEmo_e43 <- umxModify(MPerEmpEmo_t, update = c("e_r4c3"),name = "Drop E3 on empathy13", comparison = T) 
MPerEmpEmo_e44 <- umxModify(MPerEmpEmo_t, update = c("e_r4c4"),name = "Drop E4 on empathy13", comparison = T) 


umxCompare(base = MPerEmpEmo_t, 
           comparison = c(MPerEmpEmo_a11,MPerEmpEmo_a21,MPerEmpEmo_a31,MPerEmpEmo_a41,
                          MPerEmpEmo_c11,MPerEmpEmo_c21,MPerEmpEmo_c31,MPerEmpEmo_c41,
                          MPerEmpEmo_e11,MPerEmpEmo_e21,MPerEmpEmo_e31,
                          MPerEmpEmo_e22,MPerEmpEmo_e42,
                          MPerEmpEmo_e33,MPerEmpEmo_e43,
                          MPerEmpEmo_e44),
           digits=3, report = "html")


###############################################################################################
#Emotional empathy-empathy first

selDVs <- c("EMPQ_emotional11r", "predicted_emotional11r", 
            "EMPQ_emotional13r", "predicted_emotional13r")

MEmpPerEmo <- umxACE(
  name = "Emotional empathy full model",
  selDVs,
  sep = "_",
  data = DWide,
  zyg = "zygosity_2G",
  type = "FIML",
  tryHard = "yes",
  dzAr = 0.5, dzCr = .25,
  equateMeans = TRUE,
  addStd = TRUE,
  addCI = TRUE)

umxSummary(MEmpPerEmo, se=T, digits=3, showRg = F) #standardized solution but not in percentages (need to square everything to get variance percentages)

## Paths dropping -one path at a time and check its significance
MEmpPerEmo_t1 = umxModify(MEmpPerEmo, update = c("c_r4c4"),name = "Drop D4 on Personality13", comparison = T) 
MEmpPerEmo_t2 = umxModify(MEmpPerEmo, update = c("c_r4c3"),name = "Drop D3 on Personality13", comparison = T) 
MEmpPerEmo_t3 = umxModify(MEmpPerEmo, update = c("c_r4c2"),name = "Drop D2 on Personality13", comparison = T) 
MEmpPerEmo_t4 = umxModify(MEmpPerEmo, update = c("c_r4c1"),name = "Drop D1 on Personality13", comparison = T) 
MEmpPerEmo_t5 = umxModify(MEmpPerEmo, update = c("c_r3c3"),name = "Drop D3 on Empathy13", comparison = T) 
MEmpPerEmo_t6 = umxModify(MEmpPerEmo, update = c("c_r3c2"),name = "Drop D2 on Empathy13", comparison = T) 
MEmpPerEmo_t7 = umxModify(MEmpPerEmo, update = c("c_r2c2"),name = "Drop D2 on Personality11", comparison = T) 
MEmpPerEmo_t8 = umxModify(MEmpPerEmo, update = c("c_r3c1"),name = "Drop D1 on Empathy13", comparison = T) 
MEmpPerEmo_t9 = umxModify(MEmpPerEmo, update = c("c_r2c1"),name = "Drop D1 on Personality11", comparison = T) 
MEmpPerEmo_t10= umxModify(MEmpPerEmo, update = c("c_r1c1"),name = "Drop D1 on Empathy11", comparison = T) 

MEmpPerEmo_t11 = umxModify(MEmpPerEmo, update = c("a_r4c4"),name = "Drop A4 on Personality13", comparison = T) 
MEmpPerEmo_t12 = umxModify(MEmpPerEmo, update = c("a_r4c3"),name = "Drop A3 on Personlity13", comparison = T) 
MEmpPerEmo_t13 = umxModify(MEmpPerEmo, update = c("a_r4c2"),name = "Drop A2 on Personality13", comparison = T) 
MEmpPerEmo_t14 = umxModify(MEmpPerEmo, update = c("a_r4c1"),name = "Drop A1 on Personality13", comparison = T) 
MEmpPerEmo_t15 = umxModify(MEmpPerEmo, update = c("a_r3c3"),name = "Drop A3 on Empathy13", comparison = T) 
MEmpPerEmo_t16 = umxModify(MEmpPerEmo, update = c("a_r3c2"),name = "Drop A2 on Empathy13", comparison = T) 
MEmpPerEmo_t17 = umxModify(MEmpPerEmo, update = c("a_r2c2"),name = "Drop A2 on Personality11", comparison = T) 
MEmpPerEmo_t18 = umxModify(MEmpPerEmo, update = c("a_r3c1"),name = "Drop A1 on Empathy13", comparison = T) 
MEmpPerEmo_t19 = umxModify(MEmpPerEmo, update = c("a_r2c1"),name = "Drop A1 on Personality11", comparison = T) 
MEmpPerEmo_t20 = umxModify(MEmpPerEmo, update = c("a_r1c1"),name = "Drop A1 on Empathy11", comparison = T) 

MEmpPerEmo_t21 = umxModify(MEmpPerEmo, update = c("e_r4c4"),name = "Drop E4 on Personality13", comparison = T) 
MEmpPerEmo_t22 = umxModify(MEmpPerEmo, update = c("e_r4c3"),name = "Drop E3 on Personality13", comparison = T) 
MEmpPerEmo_t23 = umxModify(MEmpPerEmo, update = c("e_r4c2"),name = "Drop E2 on Personality13", comparison = T) 
MEmpPerEmo_t24 = umxModify(MEmpPerEmo, update = c("e_r4c1"),name = "Drop E1 on Personality13", comparison = T) 
MEmpPerEmo_t25 = umxModify(MEmpPerEmo, update = c("e_r3c3"),name = "Drop E3 on Empathy13", comparison = T) 
MEmpPerEmo_t26 = umxModify(MEmpPerEmo, update = c("e_r3c2"),name = "Drop E2 on Empathy13", comparison = T) 
MEmpPerEmo_t27 = umxModify(MEmpPerEmo, update = c("e_r2c2"),name = "Drop E2 on Personality11", comparison = T) 
MEmpPerEmo_t28 = umxModify(MEmpPerEmo, update = c("e_r3c1"),name = "Drop E1 on Empathy13", comparison = T) 
MEmpPerEmo_t29 = umxModify(MEmpPerEmo, update = c("e_r2c1"),name = "Drop E1 on Personlity11", comparison = T) 
MEmpPerEmo_t30 = umxModify(MEmpPerEmo, update = c("e_r1c1"),name = "Drop E1 on Empathy11", comparison = T) 

#Model comparisons
umxCompare(base = MEmpPerEmo, 
           comparison = c(MEmpPerEmo_t1,MEmpPerEmo_t2,MEmpPerEmo_t3,
                          MEmpPerEmo_t4,MEmpPerEmo_t5,MEmpPerEmo_t6,
                          MEmpPerEmo_t7,MEmpPerEmo_t8,MEmpPerEmo_t9,
                          MEmpPerEmo_t10, MEmpPerEmo_t11, MEmpPerEmo_t12,      
                          MEmpPerEmo_t13, MEmpPerEmo_t14, MEmpPerEmo_t15, 
                          MEmpPerEmo_t16, MEmpPerEmo_t17, MEmpPerEmo_t18, 
                          MEmpPerEmo_t19, MEmpPerEmo_t20, MEmpPerEmo_t21, 
                          MEmpPerEmo_t22, MEmpPerEmo_t23, MEmpPerEmo_t24, 
                          MEmpPerEmo_t25, MEmpPerEmo_t26, MEmpPerEmo_t27, 
                          MEmpPerEmo_t28, MEmpPerEmo_t29, MEmpPerEmo_t30),
           digits=3, report = "html")


#D: D1 is significant on all variables (almost significant on empathy 13)
#A: A is not significant at all except for A1 on empathy 11. Due to theoretical reasons, 
#I keep the first A
#E: two paths are not significant and I drop only these two
#conclusion: one common A, one common D, and almost all E's


MEmpPerEmo_t = umxModify(MEmpPerEmo, update = c("c_r4c4",
                                                "c_r4c3","c_r3c3",
                                                "c_r4c2","c_r3c2","c_r2c2",
                                                "a_r4c4",
                                                "a_r4c3","a_r3c3",
                                                "a_r4c2","a_r3c2","a_r2c2",
                                                "e_r4c1","e_r3c2"),
                         name = "M", comparison = T) 

umxSummary(MEmpPerEmo_t, se=T, digits=3, showRg = F) 

#create the table
sMEmpPerEmo_t <- summary(MEmpPerEmo_t, verbose=T)
plantSeBasedCI(MEmpPerEmo_t)
seBaseCI_empperEmo_t <- seBaseCI

seBaseCI_empperEmo_t <- cbind (c("EE 11","EE personality disposition 11","EE 13", "EE personality disposition 13"),
                               seBaseCI_empperEmo_t)
colnames(seBaseCI_empperEmo_t) <- c("Variable","A1","A2","A3","A4","D1","D2","D3","D4","E1","E2","E3","E4")
write.csv (seBaseCI_empperEmo_t, file= "seBaseCI_empperEmo_t.csv", row.names = F)


#examine paths significance in the final model
MEmpPerEmo_a11 <- umxModify(MEmpPerEmo_t, update = c("a_r1c1"),name = "Drop A1 on empathy11", comparison = T) 
MEmpPerEmo_a21 <- umxModify(MEmpPerEmo_t, update = c("a_r2c1"),name = "Drop A1 on personality11", comparison = T) 
MEmpPerEmo_a31 <- umxModify(MEmpPerEmo_t, update = c("a_r3c1"),name = "Drop A1 on empathy13", comparison = T) 
MEmpPerEmo_a41 <- umxModify(MEmpPerEmo_t, update = c("a_r4c1"),name = "Drop A1 on personality13", comparison = T) 

MEmpPerEmo_c11 <- umxModify(MEmpPerEmo_t, update = c("c_r1c1"),name = "Drop D1 on empathy11", comparison = T) 
MEmpPerEmo_c21 <- umxModify(MEmpPerEmo_t, update = c("c_r2c1"),name = "Drop D1 on personality11`", comparison = T) 
MEmpPerEmo_c31 <- umxModify(MEmpPerEmo_t, update = c("c_r3c1"),name = "Drop D1 on empathy13", comparison = T) 
MEmpPerEmo_c41 <- umxModify(MEmpPerEmo_t, update = c("c_r4c1"),name = "Drop D1 on personality13", comparison = T) 

MEmpPerEmo_e11 <- umxModify(MEmpPerEmo_t, update = c("e_r1c1"),name = "Drop E1 on empathy11", comparison = T) 
MEmpPerEmo_e21 <- umxModify(MEmpPerEmo_t, update = c("e_r2c1"),name = "Drop E1 on personality11", comparison = T) 
MEmpPerEmo_e31 <- umxModify(MEmpPerEmo_t, update = c("e_r3c1"),name = "Drop E1 on empathy13", comparison = T) 
#MEmpPerEmo_e41 <- umxModify(MEmpPerEmo_t, update = c("e_r4c1"),name = "Drop E1 on personality13", comparison = T) 
MEmpPerEmo_e22 <- umxModify(MEmpPerEmo_t, update = c("e_r2c2"),name = "Drop E2 on personality11", comparison = T) 
#MEmpPerEmo_e32 <- umxModify(MEmpPerEmo_t, update = c("e_r3c2"),name = "Drop E2 on empathy13", comparison = T) 
MEmpPerEmo_e42 <- umxModify(MEmpPerEmo_t, update = c("e_r4c2"),name = "Drop E2 on personality13", comparison = T) 
MEmpPerEmo_e33 <- umxModify(MEmpPerEmo_t, update = c("e_r3c3"),name = "Drop E3 on empathy13", comparison = T) 
MEmpPerEmo_e43 <- umxModify(MEmpPerEmo_t, update = c("e_r4c3"),name = "Drop E3 on personality13", comparison = T) 
MEmpPerEmo_e44 <- umxModify(MEmpPerEmo_t, update = c("e_r4c4"),name = "Drop E4 on personality13", comparison = T)


umxCompare(base = MEmpPerEmo_t, 
           comparison = c(MEmpPerEmo_a11,MEmpPerEmo_a21,MEmpPerEmo_a31,MEmpPerEmo_a41,
                          MEmpPerEmo_c11,MEmpPerEmo_c21,MEmpPerEmo_c31,MEmpPerEmo_c41,
                          MEmpPerEmo_e11,MEmpPerEmo_e21,MEmpPerEmo_e31,
                          MEmpPerEmo_e22,MEmpPerEmo_e42,
                          MEmpPerEmo_e33,MEmpPerEmo_e43,
                          MEmpPerEmo_e44),
           digits=3, report = "html")


##############################Cognitive empathy######################################################

#cognitive empathy-personality first

selDVs <- c("predicted_cognitive11r", "EMPQ_cognitive11r", 
            "predicted_cognitive13r", "EMPQ_cognitive13r")

MPerEmpCog <- umxACE(
  name = "Cognitive empathy full model",
  selDVs,
  sep = "_",
  data = DWide,
  zyg = "zygosity_2G",
  type = "FIML",
  tryHard = "yes",
  dzAr = 0.5, dzCr = .25,
  equateMeans = TRUE,
  addStd = TRUE,
  addCI = TRUE)

umxSummary(MPerEmpCog, se=T, digits=3, showRg = F) #standardized solution but not in percentages (need to square everything to get variance percentages)

#create the table
sMPerEmpCog <- summary(MPerEmpCog, verbose=T)
plantSeBasedCI(MPerEmpCog)
seBaseCI_perempCog <- seBaseCI

seBaseCI_perempCog <- cbind (c("CE personality disposition 11","CE 11","CE personality disposition 13","CE 13"),
                               seBaseCI_perempCog)
colnames(seBaseCI_perempCog) <- c("Variable","A1","A2","A3","A4","D1","D2","D3","D4","E1","E2","E3","E4")
write.csv (seBaseCI_perempCog, file= "seBaseCI_perempCog.csv", row.names = F)


## Paths dropping -one path at a time and check its significance

MPerEmpCog_t1 = umxModify(MPerEmpCog, update = c("c_r4c4"),name = "Drop D4 on Empathy13", comparison = T) 
MPerEmpCog_t2 = umxModify(MPerEmpCog, update = c("c_r4c3"),name = "Drop D3 on Empathy13", comparison = T) 
MPerEmpCog_t3 = umxModify(MPerEmpCog, update = c("c_r4c2"),name = "Drop D2 on Empathy13", comparison = T) 
MPerEmpCog_t4 = umxModify(MPerEmpCog, update = c("c_r4c1"),name = "Drop D1 on Empathy13", comparison = T) 
MPerEmpCog_t5 = umxModify(MPerEmpCog, update = c("c_r3c3"),name = "Drop D3 on Personality13", comparison = T) 
MPerEmpCog_t6 = umxModify(MPerEmpCog, update = c("c_r3c2"),name = "Drop D2 on Personality13", comparison = T) 
MPerEmpCog_t7 = umxModify(MPerEmpCog, update = c("c_r2c2"),name = "Drop D2 on Empathy11", comparison = T) 
MPerEmpCog_t8 = umxModify(MPerEmpCog, update = c("c_r3c1"),name = "Drop D1 on Personality13", comparison = T) 
MPerEmpCog_t9 = umxModify(MPerEmpCog, update = c("c_r2c1"),name = "Drop D1 on Empathy11", comparison = T) 
MPerEmpCog_t10= umxModify(MPerEmpCog, update = c("c_r1c1"),name = "Drop D1 on Personality11", comparison = T) 

MPerEmpCog_t11 = umxModify(MPerEmpCog, update = c("a_r4c4"),name = "Drop A4 on Empathy13", comparison = T) 
MPerEmpCog_t12 = umxModify(MPerEmpCog, update = c("a_r4c3"),name = "Drop A3 on Empathy13", comparison = T) 
MPerEmpCog_t13 = umxModify(MPerEmpCog, update = c("a_r4c2"),name = "Drop A2 on Empathy13", comparison = T) 
MPerEmpCog_t14 = umxModify(MPerEmpCog, update = c("a_r4c1"),name = "Drop A1 on Empathy13", comparison = T) 
MPerEmpCog_t15 = umxModify(MPerEmpCog, update = c("a_r3c3"),name = "Drop A3 on Personality13", comparison = T) 
MPerEmpCog_t16 = umxModify(MPerEmpCog, update = c("a_r3c2"),name = "Drop A2 on Personality13", comparison = T) 
MPerEmpCog_t17 = umxModify(MPerEmpCog, update = c("a_r2c2"),name = "Drop A2 on Empathy11", comparison = T) 
MPerEmpCog_t18 = umxModify(MPerEmpCog, update = c("a_r3c1"),name = "Drop A1 on Personality13", comparison = T) 
MPerEmpCog_t19 = umxModify(MPerEmpCog, update = c("a_r2c1"),name = "Drop A1 on Empathy11", comparison = T) 
MPerEmpCog_t20 = umxModify(MPerEmpCog, update = c("a_r1c1"),name = "Drop A1 on Personality11", comparison = T) 

MPerEmpCog_t21 = umxModify(MPerEmpCog, update = c("e_r4c4"),name = "Drop E4 on Empathy13", comparison = T) 
MPerEmpCog_t22 = umxModify(MPerEmpCog, update = c("e_r4c3"),name = "Drop E3 on Empathy13", comparison = T) 
MPerEmpCog_t23 = umxModify(MPerEmpCog, update = c("e_r4c2"),name = "Drop E2 on Empathy13", comparison = T) 
MPerEmpCog_t24 = umxModify(MPerEmpCog, update = c("e_r4c1"),name = "Drop E1 on Empathy13", comparison = T) 
MPerEmpCog_t25 = umxModify(MPerEmpCog, update = c("e_r3c3"),name = "Drop E3 on Personality13", comparison = T) 
MPerEmpCog_t26 = umxModify(MPerEmpCog, update = c("e_r3c2"),name = "Drop E2 on Personality13", comparison = T) 
MPerEmpCog_t27 = umxModify(MPerEmpCog, update = c("e_r2c2"),name = "Drop E2 on Empathy11", comparison = T) 
MPerEmpCog_t28 = umxModify(MPerEmpCog, update = c("e_r3c1"),name = "Drop E1 on Personality13", comparison = T) 
MPerEmpCog_t29 = umxModify(MPerEmpCog, update = c("e_r2c1"),name = "Drop E1 on Empathy11", comparison = T) 
MPerEmpCog_t30 = umxModify(MPerEmpCog, update = c("e_r1c1"),name = "Drop E1 on Personality11", comparison = T) 

#Model comparisons
umxCompare(base = MPerEmpCog, 
           comparison = c(MPerEmpCog_t1,MPerEmpCog_t2,MPerEmpCog_t3,
                          MPerEmpCog_t4,MPerEmpCog_t5,MPerEmpCog_t6,
                          MPerEmpCog_t7,MPerEmpCog_t8,MPerEmpCog_t9,
                          MPerEmpCog_t10, MPerEmpCog_t11, MPerEmpCog_t12,      
                          MPerEmpCog_t13, MPerEmpCog_t14, MPerEmpCog_t15, 
                          MPerEmpCog_t16, MPerEmpCog_t17, MPerEmpCog_t18, 
                          MPerEmpCog_t19, MPerEmpCog_t20, MPerEmpCog_t21, 
                          MPerEmpCog_t22, MPerEmpCog_t23, MPerEmpCog_t24, 
                          MPerEmpCog_t25, MPerEmpCog_t26, MPerEmpCog_t27, 
                          MPerEmpCog_t28, MPerEmpCog_t29, MPerEmpCog_t30),
           digits=3, report = "html")


#D: D is not significant at all
#A: A1 is significant on the first three variables but not on cognitive empathy 13.
#I keep A1 on empathy 13 because it doesn't make sense that it has no heritability at all 
#I also keep one common D and unique D for age 13 because of the univarite results
#E: two paths are not significant and I drop only these two


MPerEmpCog_t = umxModify(MPerEmpCog, update = c("c_r4c4",
                                                "c_r4c2","c_r3c2","c_r2c2",
                                                "a_r4c4",
                                                "a_r4c3","a_r3c3",
                                                "a_r4c2","a_r3c2","a_r2c2",
                                                "e_r4c2", "e_r3c2" ),
                         name = "M", comparison = T) 


umxSummary(MPerEmpCog_t, se=T, digits=3, showRg = F) 


#create the table
sMPerEmpCog_t <- summary(MPerEmpCog_t, verbose=T)
plantSeBasedCI(MPerEmpCog_t)
seBaseCI_perempCog_t <- seBaseCI

seBaseCI_perempCog_t <- cbind (c("CE personality disposition 11","CE 11","CE personality disposition 13","CE 13"),
                               seBaseCI_perempCog_t)
colnames(seBaseCI_perempCog_t) <- c("Variable","A1","A2","A3","A4","D1","D2","D3","D4","E1","E2","E3","E4")
write.csv (seBaseCI_perempCog_t, file= "seBaseCI_perempCog_t.csv", row.names = F)

#examine paths significance in the final model
MPerEmpCog_a11 <- umxModify(MPerEmpCog_t, update = c("a_r1c1"),name = "Drop A1 on personality11", comparison = T) 
MPerEmpCog_a21 <- umxModify(MPerEmpCog_t, update = c("a_r2c1"),name = "Drop A1 on empathy11", comparison = T) 
MPerEmpCog_a31 <- umxModify(MPerEmpCog_t, update = c("a_r3c1"),name = "Drop A1 on personality13", comparison = T) 
MPerEmpCog_a41 <- umxModify(MPerEmpCog_t, update = c("a_r4c1"),name = "Drop A1 on empathy13", comparison = T) 

MPerEmpCog_c11 <- umxModify(MPerEmpCog_t, update = c("c_r1c1"),name = "Drop D1 on personlity11", comparison = T) 
MPerEmpCog_c21 <- umxModify(MPerEmpCog_t, update = c("c_r2c1"),name = "Drop D1 on empathy11`", comparison = T) 
MPerEmpCog_c31 <- umxModify(MPerEmpCog_t, update = c("c_r3c1"),name = "Drop D1 on personality13", comparison = T) 
MPerEmpCog_c41 <- umxModify(MPerEmpCog_t, update = c("c_r4c1"),name = "Drop D1 on empathy13", comparison = T) 
MPerEmpCog_c33 <- umxModify(MPerEmpCog_t, update = c("c_r3c3"),name = "Drop D3 on personality13", comparison = T) 
MPerEmpCog_c43 <- umxModify(MPerEmpCog_t, update = c("c_r4c3"),name = "Drop D3 on empathy13", comparison = T) 

MPerEmpCog_e11 <- umxModify(MPerEmpCog_t, update = c("e_r1c1"),name = "Drop E1 on personlity11", comparison = T) 
MPerEmpCog_e21 <- umxModify(MPerEmpCog_t, update = c("e_r2c1"),name = "Drop E1 on empathy11", comparison = T) 
MPerEmpCog_e31 <- umxModify(MPerEmpCog_t, update = c("e_r3c1"),name = "Drop E1 on personality13", comparison = T) 
MPerEmpCog_e41 <- umxModify(MPerEmpCog_t, update = c("e_r4c1"),name = "Drop E1 on empathy13", comparison = T) 
MPerEmpCog_e22 <- umxModify(MPerEmpCog_t, update = c("e_r2c2"),name = "Drop E2 on empathy11", comparison = T) 
#MPerEmpCog_e32 <- umxModify(MPerEmpCog_t, update = c("e_r3c2"),name = "Drop E2 on personality13", comparison = T) 
#MPerEmpCog_e42 <- umxModify(MPerEmpCog_t, update = c("e_r4c2"),name = "Drop E2 on empathy13", comparison = T) 
MPerEmpCog_e33 <- umxModify(MPerEmpCog_t, update = c("e_r3c3"),name = "Drop E3 on personality13", comparison = T) 
MPerEmpCog_e43 <- umxModify(MPerEmpCog_t, update = c("e_r4c3"),name = "Drop E3 on empathy13", comparison = T) 
MPerEmpCog_e44 <- umxModify(MPerEmpCog_t, update = c("e_r4c4"),name = "Drop E4 on empathy13", comparison = T) 


umxCompare(base = MPerEmpCog_t, 
           comparison = c(MPerEmpCog_a11,MPerEmpCog_a21,MPerEmpCog_a31,MPerEmpCog_a41,
                          MPerEmpCog_c11,MPerEmpCog_c21,MPerEmpCog_c31,MPerEmpCog_c41,
                          MPerEmpCog_c33,MPerEmpCog_c43,
                          MPerEmpCog_e11,MPerEmpCog_e21,MPerEmpCog_e31,MPerEmpCog_e41,
                          MPerEmpCog_e22,
                          MPerEmpCog_e33,MPerEmpCog_e43,
                          MPerEmpCog_e44),
           digits=3, report = "html")

##################################################################################################
# 2) Cognitive empathy- empathy first

selDVs <- c("EMPQ_cognitive11r","predicted_cognitive11r", 
            "EMPQ_cognitive13r","predicted_cognitive13r")

MEmpPerCog <- umxACE(
  name = "Cognitive empathy full model",
  selDVs,
  sep = "_",
  data = DWide,
  zyg = "zygosity_2G",
  type = "FIML",
  tryHard = "yes",
  dzAr = 0.5, dzCr = .25,
  equateMeans = TRUE,
  addStd = TRUE,
  addCI = TRUE)

umxSummary(MEmpPerCog, se=T, digits=3, showRg = F) #standardized solution but not in percentages (need to square everything to get variance percentages)


MEmpPerCog_t1 = umxModify(MEmpPerCog, update = c("c_r4c4"),name = "Drop D4 on Personality13", comparison = T) 
MEmpPerCog_t2 = umxModify(MEmpPerCog, update = c("c_r4c3"),name = "Drop D3 on Personality13", comparison = T) 
MEmpPerCog_t3 = umxModify(MEmpPerCog, update = c("c_r4c2"),name = "Drop D2 on Personality13", comparison = T) 
MEmpPerCog_t4 = umxModify(MEmpPerCog, update = c("c_r4c1"),name = "Drop D1 on Personality13", comparison = T) 
MEmpPerCog_t5 = umxModify(MEmpPerCog, update = c("c_r3c3"),name = "Drop D3 on Empathy13", comparison = T) 
MEmpPerCog_t6 = umxModify(MEmpPerCog, update = c("c_r3c2"),name = "Drop D2 on Empathy13", comparison = T) 
MEmpPerCog_t7 = umxModify(MEmpPerCog, update = c("c_r2c2"),name = "Drop D2 on Personality11", comparison = T) 
MEmpPerCog_t8 = umxModify(MEmpPerCog, update = c("c_r3c1"),name = "Drop D1 on Empathy13", comparison = T) 
MEmpPerCog_t9 = umxModify(MEmpPerCog, update = c("c_r2c1"),name = "Drop D1 on Personality11", comparison = T) 
MEmpPerCog_t10= umxModify(MEmpPerCog, update = c("c_r1c1"),name = "Drop D1 on Empathy11", comparison = T) 

MEmpPerCog_t11 = umxModify(MEmpPerCog, update = c("a_r4c4"),name = "Drop A4 on Personality13", comparison = T) 
MEmpPerCog_t12 = umxModify(MEmpPerCog, update = c("a_r4c3"),name = "Drop A3 on Personlity13", comparison = T) 
MEmpPerCog_t13 = umxModify(MEmpPerCog, update = c("a_r4c2"),name = "Drop A2 on Personality13", comparison = T) 
MEmpPerCog_t14 = umxModify(MEmpPerCog, update = c("a_r4c1"),name = "Drop A1 on Personality13", comparison = T) 
MEmpPerCog_t15 = umxModify(MEmpPerCog, update = c("a_r3c3"),name = "Drop A3 on Empathy13", comparison = T) 
MEmpPerCog_t16 = umxModify(MEmpPerCog, update = c("a_r3c2"),name = "Drop A2 on Empathy13", comparison = T) 
MEmpPerCog_t17 = umxModify(MEmpPerCog, update = c("a_r2c2"),name = "Drop A2 on Personality11", comparison = T) 
MEmpPerCog_t18 = umxModify(MEmpPerCog, update = c("a_r3c1"),name = "Drop A1 on Empathy13", comparison = T) 
MEmpPerCog_t19 = umxModify(MEmpPerCog, update = c("a_r2c1"),name = "Drop A1 on Personality11", comparison = T) 
MEmpPerCog_t20 = umxModify(MEmpPerCog, update = c("a_r1c1"),name = "Drop A1 on Empathy11", comparison = T) 

MEmpPerCog_t21 = umxModify(MEmpPerCog, update = c("e_r4c4"),name = "Drop E4 on Personality13", comparison = T) 
MEmpPerCog_t22 = umxModify(MEmpPerCog, update = c("e_r4c3"),name = "Drop E3 on Personality13", comparison = T) 
MEmpPerCog_t23 = umxModify(MEmpPerCog, update = c("e_r4c2"),name = "Drop E2 on Personality13", comparison = T) 
MEmpPerCog_t24 = umxModify(MEmpPerCog, update = c("e_r4c1"),name = "Drop E1 on Personality13", comparison = T) 
MEmpPerCog_t25 = umxModify(MEmpPerCog, update = c("e_r3c3"),name = "Drop E3 on Empathy13", comparison = T) 
MEmpPerCog_t26 = umxModify(MEmpPerCog, update = c("e_r3c2"),name = "Drop E2 on Empathy13", comparison = T) 
MEmpPerCog_t27 = umxModify(MEmpPerCog, update = c("e_r2c2"),name = "Drop E2 on Personality11", comparison = T) 
MEmpPerCog_t28 = umxModify(MEmpPerCog, update = c("e_r3c1"),name = "Drop E1 on Empathy13", comparison = T) 
MEmpPerCog_t29 = umxModify(MEmpPerCog, update = c("e_r2c1"),name = "Drop E1 on Personlity11", comparison = T) 
MEmpPerCog_t30 = umxModify(MEmpPerCog, update = c("e_r1c1"),name = "Drop E1 on Empathy11", comparison = T) 

#Model comparisons
umxCompare(base = MEmpPerCog, 
           comparison = c(MEmpPerCog_t1,MEmpPerCog_t2,MEmpPerCog_t3,
                          MEmpPerCog_t4,MEmpPerCog_t5,MEmpPerCog_t6,
                          MEmpPerCog_t7,MEmpPerCog_t8,MEmpPerCog_t9,
                          MEmpPerCog_t10, MEmpPerCog_t11, MEmpPerCog_t12,      
                          MEmpPerCog_t13, MEmpPerCog_t14, MEmpPerCog_t15, 
                          MEmpPerCog_t16, MEmpPerCog_t17, MEmpPerCog_t18, 
                          MEmpPerCog_t19, MEmpPerCog_t20, MEmpPerCog_t21, 
                          MEmpPerCog_t22, MEmpPerCog_t23, MEmpPerCog_t24, 
                          MEmpPerCog_t25, MEmpPerCog_t26, MEmpPerCog_t27, 
                          MEmpPerCog_t28, MEmpPerCog_t29, MEmpPerCog_t30),
           digits=3, report = "html")


#D: D is not significant at all
#A: A1 is significant on the first two variables but not on age 13
#I keep A1 on empathy 13 because it doesn't make sense that it has no heritability at all 
#I also keep one common D and unique D for age 13 because of the univarite results
#conclusion: one common A, common D and Unique D to age 13, and all E's

MEmpPerCog_t = umxModify(MEmpPerCog, update = c("c_r4c4",
                                                "c_r4c2","c_r3c2","c_r2c2",
                                                "a_r4c4",
                                                "a_r4c3","a_r3c3",
                                                "a_r4c2","a_r3c2","a_r2c2"),
                           name = "M", comparison = T) 

umxSummary(MEmpPerCog_t, se=T, digits=3, showRg = F) 


#create the table
sMEmpPerCog_t <- summary(MEmpPerCog_t, verbose=T)
plantSeBasedCI(MEmpPerCog_t)
seBaseCI_empperCog_t <- seBaseCI

seBaseCI_empperCog_t <- cbind (c("CE 11","CE personality disposition 11","CE 13", "CE personality disposition 13"),
                               seBaseCI_empperCog_t)
colnames(seBaseCI_empperCog_t) <- c("Variable","A1","A2","A3","A4","D1","D2","D3","D4","E1","E2","E3","E4")
write.csv (seBaseCI_empperCog_t, file= "seBaseCI_empperCog_t.csv", row.names = F)


#examine paths significance in the final model
MEmpPerCog_a11 <- umxModify(MEmpPerCog_t, update = c("a_r1c1"),name = "Drop A1 on empathy11", comparison = T) 
MEmpPerCog_a21 <- umxModify(MEmpPerCog_t, update = c("a_r2c1"),name = "Drop A1 on personality11", comparison = T) 
MEmpPerCog_a31 <- umxModify(MEmpPerCog_t, update = c("a_r3c1"),name = "Drop A1 on empathy13", comparison = T) 
MEmpPerCog_a41 <- umxModify(MEmpPerCog_t, update = c("a_r4c1"),name = "Drop A1 on personality13", comparison = T) 

MEmpPerCog_c11 <- umxModify(MEmpPerCog_t, update = c("c_r1c1"),name = "Drop D1 on empathy11", comparison = T) 
MEmpPerCog_c21 <- umxModify(MEmpPerCog_t, update = c("c_r2c1"),name = "Drop D1 on personality11`", comparison = T) 
MEmpPerCog_c31 <- umxModify(MEmpPerCog_t, update = c("c_r3c1"),name = "Drop D1 on empathy13", comparison = T) 
MEmpPerCog_c41 <- umxModify(MEmpPerCog_t, update = c("c_r4c1"),name = "Drop D1 on personality13", comparison = T) 
MEmpPerCog_c33 <- umxModify(MEmpPerCog_t, update = c("c_r3c3"),name = "Drop D3 on empathy13", comparison = T) 
MEmpPerCog_c43 <- umxModify(MEmpPerCog_t, update = c("c_r4c3"),name = "Drop D3 on personality13", comparison = T) 


MEmpPerCog_e11 <- umxModify(MEmpPerCog_t, update = c("e_r1c1"),name = "Drop E1 on empathy11", comparison = T) 
MEmpPerCog_e21 <- umxModify(MEmpPerCog_t, update = c("e_r2c1"),name = "Drop E1 on personality11", comparison = T) 
MEmpPerCog_e31 <- umxModify(MEmpPerCog_t, update = c("e_r3c1"),name = "Drop E1 on empathy13", comparison = T) 
MEmpPerCog_e41 <- umxModify(MEmpPerCog_t, update = c("e_r4c1"),name = "Drop E1 on personality13", comparison = T) 
MEmpPerCog_e22 <- umxModify(MEmpPerCog_t, update = c("e_r2c2"),name = "Drop E2 on personality11", comparison = T) 
MEmpPerCog_e32 <- umxModify(MEmpPerCog_t, update = c("e_r3c2"),name = "Drop E2 on empathy13", comparison = T) 
MEmpPerCog_e42 <- umxModify(MEmpPerCog_t, update = c("e_r4c2"),name = "Drop E2 on personality13", comparison = T) 
MEmpPerCog_e33 <- umxModify(MEmpPerCog_t, update = c("e_r3c3"),name = "Drop E3 on empathy13", comparison = T) 
MEmpPerCog_e43 <- umxModify(MEmpPerCog_t, update = c("e_r4c3"),name = "Drop E3 on personality13", comparison = T) 
MEmpPerCog_e44 <- umxModify(MEmpPerCog_t, update = c("e_r4c4"),name = "Drop E4 on personality13", comparison = T)


umxCompare(base = MEmpPerCog_t, 
           comparison = c(MEmpPerCog_a11,MEmpPerCog_a21,MEmpPerCog_a31,MEmpPerCog_a41,
                          MEmpPerCog_c11,MEmpPerCog_c21,MEmpPerCog_c31,MEmpPerCog_c41,
                          MEmpPerCog_c33,MEmpPerCog_c43,
                          MEmpPerCog_e11,MEmpPerCog_e21,MEmpPerCog_e31,MEmpPerCog_e41,
                          MEmpPerCog_e22,MEmpPerCog_e32,MEmpPerCog_e42,
                          MEmpPerCog_e33,MEmpPerCog_e43,
                          MEmpPerCog_e44),
           digits=3, report = "html")

#################################################################################################
#Obtaining variance components for the main figures: final models when personality is first
#I ask the standardized estimates by asking standardized CIs

#Emotional empathy
  CIs_MPerEmpEmo_t<- summary(umxConfint(MPerEmpEmo_t, 
                                       parm = c("top.a_std","top.c_std","top.e_std"), run=TRUE), verbose = T)
  
  est_MPerEmpEmo_t <- CIs_MPerEmpEmo_t$CI$estimate
  dim(est_MPerEmpEmo_t) <- c(4,12)
  
  varcomp_MPerEmpEmo_t <- round(est_MPerEmpEmo_t^2,2)
  

#Cognitive empathy
  CIs_MPerEmpCog_t<- summary(umxConfint(MPerEmpCog_t, 
                                        parm = c("top.a_std","top.c_std","top.e_std"), run=TRUE), verbose = T)
  
  est_MPerEmpCog_t <- CIs_MPerEmpCog_t$CI$estimate
  dim(est_MPerEmpCog_t) <- c(4,12)
  
  varcomp_MPerEmpCog_t <- round(est_MPerEmpCog_t^2,2)
  

##################################################################################################
#Checking associations with other, not self-reported measures
##################################################################################################

#Parent-reported empathy age 11
setwd("C:/Users/ShaiW/Documents/Lior computer/empathy and puberty/data/Final data files")  
DParentEmp <- read.csv ("Age11_ParentEmp_edited_united.csv")
library(reshape)
DParentEmp <- rename(DParentEmp, c(ï..ifam="ifam"))    # change weird variable names

#erasing invalid observations
DParentEmp <- DParentEmp[DParentEmp$whichParentOut!=3,]
col <- colnames(DParentEmp)
fathervars <- c(which(col=="Date.2"),which(col=="StartDate.2"),which(col=="FinishDate.2"),
                which(col=="EmpCon.2"),which(col=="EQSQ_empathy.2"))
DParentEmp[DParentEmp$whichParentOut==2,fathervars] <- NA

#merge datsets
relvar_parents <- c(which (col=="ifam"),which(col=="ID"),
                    which (col=="EmpCon.2"),which(col=="EmpCon.3"),
                    which (col=="EQSQ_empathy.2"),which(col=="EQSQ_empathy.3"))
DParentEmpShort <- DParentEmp[,relvar_parents]
D<- merge(x=D, y=DParentEmpShort, by=c("ifam","ID"), all.x=T, all.y=T )              

#Parent-reported empathy age 13
setwd("C:/Users/ShaiW/Documents/Lior computer/empathy and puberty/data/Final data files")  
D13ParentEmp <- read.csv ("Age13_ParentEmp_edited_united.csv")
library(reshape)
D13ParentEmp <- rename(D13ParentEmp, c(ï..ifam="ifam"))    # change weird variable names
D13ParentEmp <- D13ParentEmp[D13ParentEmp$whichParentOut==0,]

col <- colnames(D13ParentEmp)
relvar_parents13 <- c(which (col=="ifam"),which(col=="ID"),
                    which (col=="EmpCon.2"),which(col=="EmpCon.3"),
                    which (col=="EQSQ_empathy.2"),which(col=="EQSQ_empathy.3"))
D13ParentEmpShort <- D13ParentEmp[,relvar_parents13]

D13ParentEmpShort <- rename(D13ParentEmpShort, 
                            c(EmpCon.2="EmpCon_13.2", EmpCon.3="EmpCon_13.3",
                              EQSQ_empathy.2="EQSQ_empathy_13.2",EQSQ_empathy.3="EQSQ_empathy_13.3"))
D<- merge(x=D, y=D13ParentEmpShort, by=c("ifam","ID"), all.x=T, all.y=T )              


#subtle facial expressions emotion recognition
DEmoR <- read.csv ("subtle_facial_expressions_age13_edited.csv")
col <- colnames(DEmoR)
relvar_EmoR <- c(which (col=="ifam"),which(col=="ID"), which(col=="ER_c"))
DEmoRshort <- na.omit(DEmoR[,relvar_EmoR])

D <- merge(x=D, y=DEmoRshort, by=c("ifam","ID"), all.x=T, all.y=T ) 


#check N of available parents questionnaire
length(D$EQSQ_empathy.2[(!is.na(D$predicted_cognitive_11r)|!is.na(D$predicted_cognitive_13r)) & 
                          !is.na(D$EQSQ_empathy.2)])

length(D$EQSQ_empathy.3[(!is.na(D$predicted_cognitive_11r)|!is.na(D$predicted_cognitive_13r)) & 
                          !is.na(D$EQSQ_empathy.3)])

length(D$EQSQ_empathy_13.2[(!is.na(D$predicted_cognitive_11r)|!is.na(D$predicted_cognitive_13r)) & 
                          !is.na(D$EQSQ_empathy_13.2)])

length(D$EQSQ_empathy_13.3[(!is.na(D$predicted_cognitive_11r)|!is.na(D$predicted_cognitive_13r)) & 
                          !is.na(D$EQSQ_empathy_13.3)])


#check difference in sudy variables according to parental missingness
D$missmomques11 <- NA
D$missmomques11[!is.na(D$EQSQ_empathy.3)] <-0
D$missmomques11[is.na(D$EQSQ_empathy.3)] <-1

D$missdadques11 <- NA
D$missdadques11[!is.na(D$EQSQ_empathy.2)] <-0
D$missdadques11[is.na(D$EQSQ_empathy.2)] <-1

D$missmomques13 <- NA
D$missmomques13[!is.na(D$EQSQ_empathy_13.3)] <-0
D$missmomques13[is.na(D$EQSQ_empathy_13.3)] <-1

D$missdadques13 <- NA
D$missdadques13[!is.na(D$EQSQ_empathy_13.2)] <-0
D$missdadques13[is.na(D$EQSQ_empathy_13.2)] <-1

#ttests
t.test(predicted_emotional_11~missmomques11, data=D)
t.test(predicted_cognitive_11~missmomques11, data=D)
t.test(EMPQ_emotional_11~missmomques11, data=D)
t.test(EMPQ_cognitive_11~missmomques11, data=D)

t.test(predicted_emotional_11~missdadques11, data=D)
t.test(predicted_cognitive_11~missdadques11, data=D)
t.test(EMPQ_emotional_11~missdadques11, data=D)
t.test(EMPQ_cognitive_11~missdadques11, data=D)

t.test(predicted_emotional_13~missmomques13, data=D)
t.test(predicted_cognitive_13~missmomques13, data=D)
t.test(EMPQ_emotional_13~missmomques13, data=D)
t.test(EMPQ_cognitive_13~missmomques13, data=D)

t.test(predicted_emotional_13~missdadques13, data=D)
t.test(predicted_cognitive_13~missdadques13, data=D)
t.test(EMPQ_emotional_13~missdadques13, data=D)
t.test(EMPQ_cognitive_13~missdadques13, data=D)

#check difference between children who had no parent data and children who had at least one parent data
D$missparentdata11 <- 0
D$missparentdata11[D$missmomques11==1 & D$missdadques11] <-1
D$missparentdata13 <- 0
D$missparentdata13[D$missmomques13==1 & D$missdadques13] <-1

t.test(predicted_emotional_11~missparentdata11, data=D)
t.test(predicted_cognitive_11~missparentdata11, data=D)
t.test(EMPQ_emotional_11~missparentdata11, data=D)
t.test(EMPQ_cognitive_11~missparentdata11, data=D)

t.test(predicted_emotional_13~missparentdata13, data=D)
t.test(predicted_cognitive_13~missparentdata13, data=D)
t.test(EMPQ_emotional_13~missparentdata13, data=D)
t.test(EMPQ_cognitive_13~missparentdata13, data=D)


#check correlations-for the final analyses I present only EQSQ (and not EmpCon)
col <- colnames(D)
library(apaTables)
relvar <- c(which (col=="predicted_emotional_11"),which(col=="predicted_cognitive_11"),
            which (col=="EMPQ_emotional_11"),which(col=="EMPQ_cognitive_11"),
            which (col=="EQSQ_empathy.2"),which(col=="EQSQ_empathy.3"),
            which (col=="predicted_emotional_13"),which(col=="predicted_cognitive_13"),
            which (col=="EMPQ_emotional_13"),which(col=="EMPQ_cognitive_13"),
            which (col=="EQSQ_empathy_13.2"),which(col=="EQSQ_empathy_13.3"),
            which (col=="ER_c"))

setwd("C:/Users/ShaiW/Documents/Lior computer/empathy and puberty/writing")  
apa.cor.table(D[,relvar], filename = "EmpcorTable.doc")
#overall, it seems like the correlations between self and parent-reported empathy are
#similar to the correlation between the personality variables and parent-reported empathy

#try a different order of presentation (first self-reports, then all the rest)
col <- colnames(D)
library(apaTables)
relvar <- c(which (col=="predicted_emotional_11"),which(col=="predicted_cognitive_11"),
            which (col=="predicted_emotional_13"),which(col=="predicted_cognitive_13"),
            which (col=="EMPQ_emotional_11"),which(col=="EMPQ_cognitive_11"),
            which (col=="EMPQ_emotional_13"),which(col=="EMPQ_cognitive_13"),
            which (col=="EQSQ_empathy.2"),which(col=="EQSQ_empathy.3"),
            which (col=="EQSQ_empathy_13.2"),which(col=="EQSQ_empathy_13.3"),
            which (col=="ER_c"))

setwd("C:/Users/ShaiW/Documents/Lior computer/empathy and puberty/writing")  
apa.cor.table(D[,relvar], filename = "EmpcorTable.doc")
#overall, it seems like the correlations between self and parent-reported empathy are
#similar to the correlation between the personality variables and parent-reported empathy


############################################################################################
#Univariate analysis - emotion recognition

setwd("C:/Users/ShaiW/Documents/Lior computer/empathy and puberty/data/raw files")  
Managing <- read.csv ("ManagingFile_03_06_19_vars2cases.csv")

library(reshape)
Managing <- rename(Managing, c(ï..ifam="ifam"))    # change weird variable names

col <- colnames((Managing))
relvar_demographics <- c(which (col=="ifam"), which (col=="ID"),
                         which (col=="sex"), which (col=="zygosity")) 

col <-colnames(D)
DEmoRWide <- D[,c(which(col=="ifam"), which(col=="ID"),which(col=="ER_c"))]

DEmoRWide <- merge(DEmoRWide, Managing[,relvar_demographics],all.x=T, all.y=F)

library(reshape) 
DEmoRWide <- reshape(DEmoRWide, timevar = "ID", idvar = "ifam", direction = "wide", 
                 v.names = c("sex","ER_c"))

colnames(DEmoRWide) <- gsub(x=colnames(DEmoRWide),pattern=".1", replacement="_1")
colnames(DEmoRWide) <- gsub(x=colnames(DEmoRWide),pattern=".4", replacement="_2")


#checking raw correlations

#create different datasets for the 3 zygosity groups (regardless of sex)
DEmoRmz  = na.omit(subset(DEmoRWide, zygosity == 1))
DEmoRdzs = na.omit(subset(DEmoRWide, zygosity == 2))
DEmoRdzo = na.omit(subset(DEmoRWide, zygosity == 3))

rawDescript_DemoR<- umxSummarizeTwinData(na.omit(DEmoRWide), 
                                             selVars="ER_c",
                                             zyg="zygosity", 
                                             sep="_",MZ=2, DZ=3)

#it seems like there is a meaningful genetic difference (and perhaps sex differences)
#and no shared-environment. 
#On the other hand, the N of MZ is really small(42), so perhaps it is not so good 
#to do genetic analyses with that task


#######################################################################################################
############### Univariate analysis - Original Big-five scales ########################################
########################################################################################################

#THIS IS ACCORDING TO THE NON_SX_RESIDUAL VARIABlES. NEED TO C 

#checking raw correlations
rawDescript_r_DZunite<- umxSummarizeTwinData(DWide, 
                                             selVars=c("EXTRAVERSION11","AGREEABLE11","CONSCIENTIOUS11","NEUROTICISM11","OPENESS11",
                                                       "EXTRAVERSION13","AGREEABLE13","CONSCIENTIOUS13","NEUROTICISM13","OPENESS13" ),
                                             zyg="zygosity_2G", 
                                             sep="_",MZ="MZ", DZ="DZ")

#scale the variables according to umx reccomendations
DWide<- umx_scale(DWide, 
                  varsToScale =c(selVars=c("EXTRAVERSION11_1","AGREEABLE11_1","CONSCIENTIOUS11_1","NEUROTICISM11_1","OPENESS11_1",
                                           "EXTRAVERSION13_1","AGREEABLE13_1","CONSCIENTIOUS13_1","NEUROTICISM13_1","OPENESS13_1",
                                           "EXTRAVERSION11_2","AGREEABLE11_2","CONSCIENTIOUS11_2","NEUROTICISM11_2","OPENESS11_2",
                                           "EXTRAVERSION13_2","AGREEABLE13_2","CONSCIENTIOUS13_2","NEUROTICISM13_2","OPENESS13_2")))


#check that the scaling worked 
rawDescript_r_DZunite<- umxSummarizeTwinData(DWide, 
                                             selVars=c("EXTRAVERSION11","AGREEABLE11","CONSCIENTIOUS11","NEUROTICISM11","OPENESS11",
                                                       "EXTRAVERSION13","AGREEABLE13","CONSCIENTIOUS13","NEUROTICISM13","OPENESS13" ),
                                             zyg="zygosity_2G", 
                                             sep="_",MZ="MZ", DZ="DZ")



#now with sex-residualized variables

#scale the variables according to umx reccomendations
DWide<- umx_scale(DWide, 
                  varsToScale =c(selVars=c("EXTRAVERSION11r_1","AGREEABLE11r_1","CONSCIENTIOUS11r_1","NEUROTICISM11r_1","OPENESS11r_1",
                                           "EXTRAVERSION13r_1","AGREEABLE13r_1","CONSCIENTIOUS13r_1","NEUROTICISM13r_1","OPENESS13r_1",
                                           "EXTRAVERSION11r_2","AGREEABLE11r_2","CONSCIENTIOUS11r_2","NEUROTICISM11r_2","OPENESS11r_2",
                                           "EXTRAVERSION13r_2","AGREEABLE13r_2","CONSCIENTIOUS13r_2","NEUROTICISM13r_2","OPENESS13r_2")))


#check that the scaling worked 
rawDescript_r_DZunite<- umxSummarizeTwinData(DWide, 
                                             selVars=c("EXTRAVERSION11r","AGREEABLE11r","CONSCIENTIOUS11r","NEUROTICISM11r","OPENESS11r",
                                                       "EXTRAVERSION13r","AGREEABLE13r","CONSCIENTIOUS13r","NEUROTICISM13r","OPENESS13r" ),
                                             zyg="zygosity_2G", 
                                             sep="_",MZ="MZ", DZ="DZ")

##################################Univariate analyses################################################
#predicted emotional-age 11
Univariate( Dwide=Dwide, selDVs=c("EXTRAVERSION11r"), dzCr=.25)
Uni_EXTRAVSION <- M
umxSummaryACE(Uni_EXTRAVSION, se=T, digits=2, showRg = F, CIs = T) 

Univariate( Dwide=Dwide, selDVs=c("AGREEABLE11r"), dzCr=.25)
Uni_AGREEABLENESS <- M
umxSummaryACE(Uni_AGREEABLENESS, se=T, digits=2, showRg = F, CIs = T) 

Univariate( Dwide=Dwide, selDVs=c("CONSCIENTIOUS11r"), dzCr=.25)
Uni_CONSCIENTIOUS <- M
umxSummaryACE(Uni_CONSCIENTIOUS, se=T, digits=2, showRg = F, CIs = T) 
umxReduce(Uni_CONSCIENTIOUS)

Univariate( Dwide=Dwide, selDVs=c("NEUROTICISM11r"), dzCr=.25)
Uni_NEUROTICISM <- M
umxSummaryACE(Uni_NEUROTICISM, se=T, digits=2, showRg = F, CIs = T) 


Univariate( Dwide=Dwide, selDVs=c("OPENESS11r"), dzCr=.25)
Uni_OPENESS <- M
umxSummaryACE(Uni_OPENESS, se=T, digits=2, showRg = F, CIs = T) 
umxReduce(Uni_OPENESS)

