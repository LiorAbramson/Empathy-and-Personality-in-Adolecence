#################################################################################################
######### Attrition analysis ####################################################################
#################################################################################################

DSVRres_11_13$whatStage <- 0
DSVRres_11_13$whatStage[is.na(DSVRres_11_13$EMPQ_emotional_11)== F &is.na(DSVRres_11_13$EMPQ_emotional_13)== F] <-1   

ttest_whatStage_emo11 <- t.test(EMPQ_emotional_11~whatStage, data=DSVRres_11_13)
ttest_whatStage_cog11 <- t.test(EMPQ_cognitive_11~whatStage, data=DSVRres_11_13)
ttest_whatStage_predemo11 <- t.test(predicted_emotional_11~whatStage, data=DSVRres_11_13)
ttest_whatStage_predcog11 <- t.test(predicted_cognitive_11~whatStage, data=DSVRres_11_13)

ttest_whatStage_emo13 <- t.test(EMPQ_emotional_13~whatStage, data=DSVRres_11_13)
ttest_whatStage_cog13 <- t.test(EMPQ_cognitive_13~whatStage, data=DSVRres_11_13)
ttest_whatStage_predemo13 <- t.test(predicted_emotional_13~whatStage, data=DSVRres_11_13)
ttest_whatStage_predcog13 <- t.test(predicted_cognitive_13~whatStage, data=DSVRres_11_13)

library(broom)
library(purrr)

ttest_attritionTable <- map_df(list(ttest_whatStage_predemo11,ttest_whatStage_emo11,
                                    ttest_whatStage_predemo13,ttest_whatStage_emo13,
                                    ttest_whatStage_predcog11,ttest_whatStage_cog11,
                                    ttest_whatStage_predcog13,ttest_whatStage_cog13), tidy)

ttest_attritionTable_edit <- ttest_attritionTable[c("estimate1", "estimate2","statistic","parameter","p.value")]
colnames(ttest_attritionTable_edit) <- c("Mean one time-point","Mean both time-point", "t value","df","p-value")
ttest_attritionTable_edit[,1:4] <- round(ttest_attritionTable_edit[,1:4],2)
ttest_attritionTable_edit[,5]   <- round(ttest_attritionTable_edit[,5],3)

#add effect size
ttest_attritionTable_edit$cohenD <- NA
library(lsr)
ttest_attritionTable_edit$cohenD[1] <- 0-round(cohensD(predicted_emotional_11~whatStage, data=DSVRres_11_13),2)
ttest_attritionTable_edit$cohenD[2] <- 0-round(cohensD(EMPQ_emotional_11~whatStage, data=DSVRres_11_13),2)
ttest_attritionTable_edit$cohenD[3] <- 0-round(cohensD(predicted_emotional_13~whatStage, data=DSVRres_11_13),2)
ttest_attritionTable_edit$cohenD[4] <- 0-round(cohensD(EMPQ_emotional_13~whatStage, data=DSVRres_11_13),2)

ttest_attritionTable_edit$cohenD[5] <- 0-round(cohensD(predicted_cognitive_11~whatStage, data=DSVRres_11_13),2)
ttest_attritionTable_edit$cohenD[6] <- 0-round(cohensD(EMPQ_cognitive_11~whatStage, data=DSVRres_11_13),2)
ttest_attritionTable_edit$cohenD[7] <- 0-round(cohensD(predicted_cognitive_13~whatStage, data=DSVRres_11_13),2)
ttest_attritionTable_edit$cohenD[8] <- 0-round(cohensD(EMPQ_cognitive_13~whatStage, data=DSVRres_11_13),2)

#add sd
ttest_attritionTable_edit$sd1Stage <- NA
ttest_attritionTable_edit$sd2Stage <- NA

ttest_attritionTable_edit$sd1Stage[1] <- round((aggregate(DSVRres_11_13$predicted_emotional_11~DSVRres_11_13$whatStage, FUN = sd)[1,2]),2)
ttest_attritionTable_edit$sd1Stage[2] <- round((aggregate(DSVRres_11_13$EMPQ_emotional_11~DSVRres_11_13$whatStage, FUN = sd)[1,2]),2)
ttest_attritionTable_edit$sd1Stage[3] <- round((aggregate(DSVRres_11_13$predicted_emotional_13~DSVRres_11_13$whatStage, FUN = sd)[1,2]),2)
ttest_attritionTable_edit$sd1Stage[4] <- round((aggregate(DSVRres_11_13$EMPQ_emotional_13~DSVRres_11_13$whatStage, FUN = sd)[1,2]),2)

ttest_attritionTable_edit$sd1Stage[5] <- round((aggregate(DSVRres_11_13$predicted_cognitive_11~DSVRres_11_13$whatStage, FUN = sd)[1,2]),2)
ttest_attritionTable_edit$sd1Stage[6] <- round((aggregate(DSVRres_11_13$EMPQ_cognitive_11~DSVRres_11_13$whatStage, FUN = sd)[1,2]),2)
ttest_attritionTable_edit$sd1Stage[7] <- round((aggregate(DSVRres_11_13$predicted_cognitive_13~DSVRres_11_13$whatStage, FUN = sd)[1,2]),2)
ttest_attritionTable_edit$sd1Stage[8] <- round((aggregate(DSVRres_11_13$EMPQ_cognitive_13~DSVRres_11_13$whatStage, FUN = sd)[1,2]),2)

ttest_attritionTable_edit$sd2Stage[1] <- round((aggregate(DSVRres_11_13$predicted_emotional_11~DSVRres_11_13$whatStage, FUN = sd)[2,2]),2)
ttest_attritionTable_edit$sd2Stage[2] <- round((aggregate(DSVRres_11_13$EMPQ_emotional_11~DSVRres_11_13$whatStage, FUN = sd)[2,2]),2)
ttest_attritionTable_edit$sd2Stage[3] <- round((aggregate(DSVRres_11_13$predicted_emotional_13~DSVRres_11_13$whatStage, FUN = sd)[2,2]),2)
ttest_attritionTable_edit$sd2Stage[4] <- round((aggregate(DSVRres_11_13$EMPQ_emotional_13~DSVRres_11_13$whatStage, FUN = sd)[2,2]),2)

ttest_attritionTable_edit$sd2Stage[5] <- round((aggregate(DSVRres_11_13$predicted_cognitive_11~DSVRres_11_13$whatStage, FUN = sd)[2,2]),2)
ttest_attritionTable_edit$sd2Stage[6] <- round((aggregate(DSVRres_11_13$EMPQ_cognitive_11~DSVRres_11_13$whatStage, FUN = sd)[2,2]),2)
ttest_attritionTable_edit$sd2Stage[7] <- round((aggregate(DSVRres_11_13$predicted_cognitive_13~DSVRres_11_13$whatStage, FUN = sd)[2,2]),2)
ttest_attritionTable_edit$sd2Stage[8] <- round((aggregate(DSVRres_11_13$EMPQ_cognitive_13~DSVRres_11_13$whatStage, FUN = sd)[2,2]),2)

ttest_attritionTable_edit$`Mean one time-point` <- paste(as.character(ttest_attritionTable_edit$`Mean one time-point`),"(",
                                                         as.character (ttest_attritionTable_edit$sd1Stage),")")
ttest_attritionTable_edit$`Mean both time-point` <- paste(as.character(ttest_attritionTable_edit$`Mean both time-point`),"(",
                                                          as.character (ttest_attritionTable_edit$sd2Stage),")")
ttest_attritionTable_edit$`t value` <- paste(as.character(ttest_attritionTable_edit$`t value`),"(",
                                             as.character (ttest_attritionTable_edit$df),")")

df_attrition <- apply(ttest_attritionTable_edit[,c(1,2,3,6)],2,as.character)

write.csv(df_attrition, file= "ttest_attritionTable.csv", row.names = F)
