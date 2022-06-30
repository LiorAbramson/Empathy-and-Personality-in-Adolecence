#Preparations
rm(list = ls()) # clean the global environment
cat ("\014")    #clean the R console

#Preparing the file
setwd("/Users/liorabramson/Dropbox/empathy and puberty/SVR")  
D <- read.csv ("Age11_EmpPer_4SVR.csv")
library(reshape)
D <- rename(D, c(ï..ifam="ifam"))    # change weird variable names
D <- D[D$isOut==0,]                  #take out children that should not be included
D <- subset(D, !is.na(D$EMPQ_emotional)) #take out children with no empathy measures

#############################################################################################
######################### Data Preprocessing ################################################
#############################################################################################

#1) Handling missing values

#first count how many missing values in each var:
ind <- which(colnames(D)=="el_BFI1")
library(psych)
descD <- describe(D[,ind:(ind+43)])
missingBFI11 <- c()
  for (i in 1:44){
    eval(parse(text= paste0("missingBFI11[",i,"] <-as.numeric(descD$el_BFI",i,
                            "$counts[2])/ as.numeric(descD$el_BFI",i,"$counts[1])")))
    }

max(missingBFI11)

#Imputing missing values with pmm
library("Hmisc")

#Imputations of missing values for the Big5
#Big5
set.seed(12) #set the random vector to always be the same vector
Imp_B5 <- aregImpute (formula= ~el_BFI1+ el_BFI2+ el_BFI3+ el_BFI4+ el_BFI5+ el_BFI6+ el_BFI7+ el_BFI8+el_BFI9+ el_BFI10+
                        el_BFI11+ el_BFI12+ el_BFI13+ el_BFI14+ el_BFI15+ el_BFI16+ el_BFI17+ el_BFI18+el_BFI19+ el_BFI20+
                        el_BFI21+ el_BFI22+ el_BFI23+ el_BFI24+ el_BFI25+ el_BFI26+ el_BFI27+ el_BFI28+el_BFI29+ el_BFI30+
                        el_BFI31+ el_BFI32+ el_BFI33+ el_BFI34+ el_BFI35+ el_BFI36+ el_BFI37+ el_BFI38+el_BFI39+ el_BFI40+
                        el_BFI41+ el_BFI42+ el_BFI43+ el_BFI44, 
                      data=D, x=T,n.impute=5 , nk=0, type="pmm")


#Creating five data sets with different imputed values
DImp1_B5 <-as.data.frame(impute.transcan(Imp_B5,imputation=1,data=D,list.out=T,pr=F))
DImp2_B5 <-as.data.frame(impute.transcan(Imp_B5,imputation=2,data=D,list.out=T,pr=F))
DImp3_B5 <-as.data.frame(impute.transcan(Imp_B5,imputation=3,data=D,list.out=T,pr=F))
DImp4_B5 <-as.data.frame(impute.transcan(Imp_B5,imputation=4,data=D,list.out=T,pr=F))
DImp5_B5 <-as.data.frame(impute.transcan(Imp_B5,imputation=5,data=D,list.out=T,pr=F))

col <- colnames(D)
varsEmp <- c(which(col=="EMPQ_emotional"), which(col=="EMPQ_cognitive"), which(col=="EMPQ_IRI_motiv"))

DImp1 <-cbind.data.frame(D[,c(1,2)],DImp1_B5, D[,varsEmp])
DImp2 <-cbind.data.frame(D[,c(1,2)],DImp2_B5, D[,varsEmp])
DImp3 <-cbind.data.frame(D[,c(1,2)],DImp3_B5, D[,varsEmp])
DImp4 <-cbind.data.frame(D[,c(1,2)],DImp4_B5, D[,varsEmp])
DImp5 <-cbind.data.frame(D[,c(1,2)],DImp5_B5, D[,varsEmp])

#prepare also a dataset only with complete data (i.e., no missing values)
D_complete_data <- D[complete.cases(D[,ind:(ind+43)]),which(colnames(D) %in% colnames(DImp1))]

### To check another imputed dataset, all I need to do is to change the next row and enter
###another dataset to the CreateReverseItems_allData function

#2) reverse items-Activate the function first
CreateReverseItems_allData(D_complete_data)
DImp1.1 <- DImp

#defining the relevant variables
col <- colnames(DImp1.1)
relvar_emotional <- c(which (col=="EMPQ_emotional"),
                      which (col=="el_BFI1"), which (col=="el_BFI2_Rev"), which (col=="el_BFI3"),which (col=="el_BFI4"),
                      which (col=="el_BFI5"),which (col=="el_BFI6_Rev"),which (col=="el_BFI7"),which (col=="el_BFI8_Rev"),
                      which (col=="el_BFI9_Rev"),which (col=="el_BFI10"),which (col=="el_BFI11"),which (col=="el_BFI12_Rev"),
                      which (col=="el_BFI13"),which (col=="el_BFI14"),which (col=="el_BFI15"),which (col=="el_BFI16"),
                      which (col=="el_BFI17"),which (col=="el_BFI18_Rev"),which (col=="el_BFI19"),which (col=="el_BFI20"),
                      which (col=="el_BFI21_Rev"),which (col=="el_BFI22"),which (col=="el_BFI23_Rev"),which (col=="el_BFI24_Rev"),
                      which (col=="el_BFI25"),which (col=="el_BFI26"),which (col=="el_BFI27_Rev"),which (col=="el_BFI28"),
                      which (col=="el_BFI29"),which (col=="el_BFI30"),which (col=="el_BFI31_Rev"),which (col=="el_BFI32"),
                      which (col=="el_BFI33"),which (col=="el_BFI34_Rev"),which (col=="el_BFI35_Rev"),which (col=="el_BFI36"),
                      which (col=="el_BFI37_Rev"),which (col=="el_BFI38"),which (col=="el_BFI39"),which (col=="el_BFI40"),
                      which (col=="el_BFI41_Rev"),which (col=="el_BFI42"),which (col=="el_BFI43_Rev"),which (col=="el_BFI44"))

relvar_cognitive <- c(which(col=="EMPQ_cognitive"),
                      which (col=="el_BFI1"), which (col=="el_BFI2_Rev"), which (col=="el_BFI3"),which (col=="el_BFI4"),
                      which (col=="el_BFI5"),which (col=="el_BFI6_Rev"),which (col=="el_BFI7"),which (col=="el_BFI8_Rev"),
                      which (col=="el_BFI9_Rev"),which (col=="el_BFI10"),which (col=="el_BFI11"),which (col=="el_BFI12_Rev"),
                      which (col=="el_BFI13"),which (col=="el_BFI14"),which (col=="el_BFI15"),which (col=="el_BFI16"),
                      which (col=="el_BFI17"),which (col=="el_BFI18_Rev"),which (col=="el_BFI19"),which (col=="el_BFI20"),
                      which (col=="el_BFI21_Rev"),which (col=="el_BFI22"),which (col=="el_BFI23_Rev"),which (col=="el_BFI24_Rev"),
                      which (col=="el_BFI25"),which (col=="el_BFI26"),which (col=="el_BFI27_Rev"),which (col=="el_BFI28"),
                      which (col=="el_BFI29"),which (col=="el_BFI30"),which (col=="el_BFI31_Rev"),which (col=="el_BFI32"),
                      which (col=="el_BFI33"),which (col=="el_BFI34_Rev"),which (col=="el_BFI35_Rev"),which (col=="el_BFI36"),
                      which (col=="el_BFI37_Rev"),which (col=="el_BFI38"),which (col=="el_BFI39"),which (col=="el_BFI40"),
                      which (col=="el_BFI41_Rev"),which (col=="el_BFI42"),which (col=="el_BFI43_Rev"),which (col=="el_BFI44"))

#making the names of age 11 and age 13 the same
newnames <- gsub(x=colnames(DImp1.1[,relvar_emotional]),pattern="el_", replacement="")
colnames(DImp1.1)[relvar_emotional] <- newnames

##############################################################################################

#dividing the 6 folds so two twins from the same family will always be in the same test fold
unique_ifams <- unique(DImp1.1$ifam)
set.seed(32189)
unique_ifams <- sample(unique_ifams) 

remainder<-length(unique_ifams)%%6
nfold <- (length(unique_ifams)-remainder)/6
gfold <- c(rep(1, times=nfold), rep(2, times=nfold), rep(3, times=nfold),
           rep(4, times=nfold), rep(5, times=nfold), rep(6, times=nfold))
gfold <- c(gfold,1:remainder)
unique_ifams <- cbind(unique_ifams,gfold)
ifams<- as.data.frame(rbind(unique_ifams, unique_ifams))
colnames(ifams) <- c("ifam","gfold")
ifams$ID<- c(rep(1,times=nrow(unique_ifams)),rep(4,times=nrow(unique_ifams)))

DImp1.1<- merge(DImp1.1,ifams, by=c("ifam","ID"), all.x = T, all.y = F)

#####################################################################################################
#################### Ridge regression ####################################################################
######################################################################################################

library(glmnet)

#first do cross-validation within each age

lambdas <- 10^seq(3, -2, by = -.1) 

Ridge <- function (DImp,gfold,relvar,lambdas) {
  #find the best lambda- 
  #use either glmnet default range search by lambda=NULL or set search by lambda=lambdas  
  #alpha=0 means we use Ridge regression (and not lasso regression)
  set.seed(10000)
  cv_fit <- cv.glmnet(x=as.matrix(DImp[DImp$gfold != gfold,relvar[2:45]]),
                      y=DImp[DImp$gfold != gfold,relvar[1]],
                      alpha=0, lambda=NULL,nfolds=10)
  opt_lambda <- cv_fit$lambda.min
  opt_lambda_ind <- which(cv_fit$lambda==opt_lambda)
  
  #what are the coefficients when the lambda is optimal
  opt_coef <- as.matrix(cv_fit$glmnet.fit$beta[,opt_lambda_ind]) 
  
  #after finding the best lambda, train the entire train set with that lambda
  fit <- glmnet(x=as.matrix(DImp[DImp$gfold != gfold,relvar[2:45]]), 
                y=DImp[DImp$gfold != gfold,relvar[1]], 
                alpha = 0, lambda = opt_lambda)
  
  #now check the prediction on the test set
  y_pred <- predict(fit, s=opt_lambda, 
                    newx = as.matrix(DImp[DImp$gfold == gfold,relvar[2:45]]))
  
  mse <- mean((DImp[DImp$gfold == gfold,relvar[1]]-y_pred)^2)
  
  mean_train <- mean(DImp[DImp$gfold != gfold,relvar[1]])
  Rholdout_numenator <- sum((DImp[DImp$gfold == gfold,relvar[1]]-y_pred)^2) 
  Rholdout_denumenator <- sum((DImp[DImp$gfold == gfold,relvar[1]]-mean_train)^2) 
  Rholdout <- 1- (Rholdout_numenator/Rholdout_denumenator)
  
  assign ("fit",fit,envir = .GlobalEnv)
  assign ("opt_lambda",opt_lambda,envir = .GlobalEnv)
  assign ("opt_coef",opt_coef,envir = .GlobalEnv)
  assign ("y_pred",y_pred,envir = .GlobalEnv)
  assign ("mse", mse,envir = .GlobalEnv)
  assign ("Rholdout", Rholdout, envir = .GlobalEnv)
}

#scale all the BFI items so they all will mean=0 and SD=1
for (i in 2:45) {DImp1.1[,relvar_emotional[i]] <-
  scale(DImp1.1[,relvar_emotional[i]], scale=T)}


#doing Ridge regression on the folds 
#fold 1
Ridge(DImp=DImp1.1,gfold=1,relvar=relvar_emotional,lambdas=lambdas) 
fit_emo11_1 <- fit
opt_lambda_emo11_1 <- opt_lambda
opt_coef_emo11_1 <- opt_coef
y_pred_emo11_1 <- y_pred
mse_emo11_1 <- mse
Rholdout_emo11_1 <- Rholdout

#fold 2
Ridge(DImp=DImp1.1,gfold=2,relvar=relvar_emotional,lambdas=lambdas) 
fit_emo11_2 <- fit
opt_lambda_emo11_2 <- opt_lambda
opt_coef_emo11_2 <- opt_coef
y_pred_emo11_2 <- y_pred
mse_emo11_2 <- mse
Rholdout_emo11_2 <- Rholdout

#fold 3
Ridge(DImp=DImp1.1,gfold=3,relvar=relvar_emotional,lambdas=lambdas) 
fit_emo11_3 <- fit
opt_lambda_emo11_3 <- opt_lambda
opt_coef_emo11_3 <- opt_coef
y_pred_emo11_3 <- y_pred
mse_emo11_3 <- mse
Rholdout_emo11_3 <- Rholdout

#fold 4
Ridge(DImp=DImp1.1,gfold=4,relvar=relvar_emotional,lambdas=lambdas) 
fit_emo11_4 <- fit
opt_lambda_emo11_4 <- opt_lambda
opt_coef_emo11_4 <- opt_coef
y_pred_emo11_4 <- y_pred
mse_emo11_4 <- mse
Rholdout_emo11_4 <- Rholdout

#fold 5
Ridge(DImp=DImp1.1,gfold=5,relvar=relvar_emotional,lambdas=lambdas) 
fit_emo11_5 <- fit
opt_lambda_emo11_5 <- opt_lambda
opt_coef_emo11_5 <- opt_coef
y_pred_emo11_5 <- y_pred
mse_emo11_5 <- mse
Rholdout_emo11_5 <- Rholdout

#fold 6
Ridge(DImp=DImp1.1,gfold=6,relvar=relvar_emotional,lambdas=lambdas) 
fit_emo11_6 <- fit
opt_lambda_emo11_6 <- opt_lambda
opt_coef_emo11_6 <- opt_coef
y_pred_emo11_6 <- y_pred
mse_emo11_6 <- mse
Rholdout_emo11_6 <- Rholdout


#finding the mean correlation between outcome and predicted value across the folds
cor_emo11 <-1:6
for (i in 1:6) {
  cor_emo11[i] <- cor.test(DImp1.1$EMPQ_emotional[DImp1.1$gfold ==i],
                           eval(parse(text=paste0("y_pred_emo11_",i))))[4]}

cor_emo11 <- as.numeric(cor_emo11)
avecor_emo11 <- mean(cor_emo11)

#computing R^2
R2_emo11 <- as.numeric(cor_emo11^2)
aveR2_emo11 <- mean(R2_emo11)

#computing the mean mse across the folds
mse_emo11 <-1:6
for (i in 1:6) { mse_emo11[i] <- eval(parse(text=paste0("mse_emo11_",i)))}
avemse_emo11 <- mean(mse_emo11)


#############################################################################################
#Cognitive empathy 11

#doing Ridge regression on the folds 
#fold 1
Ridge(DImp=DImp1.1,gfold=1,relvar=relvar_cognitive,lambdas=lambdas) 
fit_cog11_1 <- fit
opt_lambda_cog11_1 <- opt_lambda
opt_coef_cog11_1 <- opt_coef
y_pred_cog11_1 <- y_pred
mse_cog11_1 <- mse
Rholdout_cog11_1 <- Rholdout

#fold 2
Ridge(DImp=DImp1.1,gfold=2,relvar=relvar_cognitive,lambdas=lambdas) 
fit_cog11_2 <- fit
opt_lambda_cog11_2 <- opt_lambda
opt_coef_cog11_2 <- opt_coef
y_pred_cog11_2 <- y_pred
mse_cog11_2 <- mse
Rholdout_cog11_2 <- Rholdout

#fold 3
Ridge(DImp=DImp1.1,gfold=3,relvar=relvar_cognitive,lambdas=lambdas) 
fit_cog11_3 <- fit
opt_lambda_cog11_3 <- opt_lambda
opt_coef_cog11_3 <- opt_coef
y_pred_cog11_3 <- y_pred
mse_cog11_3 <- mse
Rholdout_cog11_3 <- Rholdout

#fold 4
Ridge(DImp=DImp1.1,gfold=4,relvar=relvar_cognitive,lambdas=lambdas) 
fit_cog11_4 <- fit
opt_lambda_cog11_4 <- opt_lambda
opt_coef_cog11_4 <- opt_coef
y_pred_cog11_4 <- y_pred
mse_cog11_4 <- mse
Rholdout_cog11_4 <- Rholdout

#fold 5
Ridge(DImp=DImp1.1,gfold=5,relvar=relvar_cognitive,lambdas=lambdas) 
fit_cog11_5 <- fit
opt_lambda_cog11_5 <- opt_lambda
opt_coef_cog11_5 <- opt_coef
y_pred_cog11_5 <- y_pred
mse_cog11_5 <- mse
Rholdout_cog11_5 <- Rholdout

#fold 6
Ridge(DImp=DImp1.1,gfold=6,relvar=relvar_cognitive,lambdas=lambdas) 
fit_cog11_6 <- fit
opt_lambda_cog11_6 <- opt_lambda
opt_coef_cog11_6 <- opt_coef
y_pred_cog11_6 <- y_pred
mse_cog11_6 <- mse
Rholdout_cog11_6 <- Rholdout


#finding the mean correlation between outcome and predicted value across the folds
cor_cog11 <-1:6
for (i in 1:6) {
  cor_cog11[i] <- cor.test(DImp1.1$EMPQ_cognitive[DImp1.1$gfold ==i],
                           eval(parse(text=paste0("y_pred_cog11_",i))))[4]}

cor_cog11 <- as.numeric(cor_cog11)
avecor_cog11 <- mean(cor_cog11)

#computing R^2
R2_cog11 <- as.numeric(cor_cog11^2)
aveR2_cog11 <- mean(R2_cog11)

#computing the mean mse across the folds
mse_cog11 <-1:6
for (i in 1:6) { mse_cog11[i] <- eval(parse(text=paste0("mse_cog11_",i)))}
avemse_cog11 <- mean(mse_cog11)


###############################################################################################
####################### AGE 13 ################################################################
###############################################################################################

#preparing the file
setwd("/Users/liorabramson/Dropbox/empathy and puberty/SVR")  
D13 <- read.csv ("Age13_EmpPer_4SVR.csv")
library(reshape)
D13 <- rename(D13, c(ï..ifam="ifam"))    # change weird variable names
D13 <- D13[D13$isOut==0,]                  #take out children that should not be included
D13 <- subset(D13, !is.na(D13$EMPQ_emotional)) #take out children with no empathy measures


####################Data preprocessing##########################################################

# 1) Imputing missing values with pmm

#first count how many missing values in each var:
ind <- which(colnames(D13)=="tn_BFI1")
library(psych)
descD13 <- describe(D13[,ind:(ind+43)])
missingBFI13 <- c()
for (i in 1:44){
  eval(parse(text= paste0("missingBFI13[",i,"] <-as.numeric(descD13$tn_BFI",i,
                          "$counts[2])/ as.numeric(descD13$tn_BFI",i,"$counts[1])")))
}

max(missingBFI13)

library("Hmisc")

#Imputations of missing values for the Big5
#Big5
set.seed(12) #set the random vector to always be the same vector
Imp13_B5 <- aregImpute (formula= ~tn_BFI1+ tn_BFI2+ tn_BFI3+ tn_BFI4+ tn_BFI5+ tn_BFI6+ tn_BFI7+ tn_BFI8+tn_BFI9+ tn_BFI10+
                          tn_BFI11+ tn_BFI12+ tn_BFI13+ tn_BFI14+ tn_BFI15+ tn_BFI16+ tn_BFI17+ tn_BFI18+tn_BFI19+ tn_BFI20+
                          tn_BFI21+ tn_BFI22+ tn_BFI23+ tn_BFI24+ tn_BFI25+ tn_BFI26+ tn_BFI27+ tn_BFI28+tn_BFI29+ tn_BFI30+
                          tn_BFI31+ tn_BFI32+ tn_BFI33+ tn_BFI34+ tn_BFI35+ tn_BFI36+ tn_BFI37+ tn_BFI38+tn_BFI39+ tn_BFI40+
                          tn_BFI41+ tn_BFI42+ tn_BFI43+ tn_BFI44, 
                        data=D13, x=T,n.impute=5 , nk=0, type="pmm")


#Creating five data sets with different imputed values
D13Imp1_B5 <-as.data.frame(impute.transcan(Imp13_B5,imputation=1,data=D13,list.out=T,pr=F))
D13Imp2_B5 <-as.data.frame(impute.transcan(Imp13_B5,imputation=2,data=D13,list.out=T,pr=F))
D13Imp3_B5 <-as.data.frame(impute.transcan(Imp13_B5,imputation=3,data=D13,list.out=T,pr=F))
D13Imp4_B5 <-as.data.frame(impute.transcan(Imp13_B5,imputation=4,data=D13,list.out=T,pr=F))
D13Imp5_B5 <-as.data.frame(impute.transcan(Imp13_B5,imputation=5,data=D13,list.out=T,pr=F))

col <- colnames(D13)
varsEmp <- c(which(col=="EMPQ_emotional"), which(col=="EMPQ_cognitive"), which(col=="EMPQ_IRI_motiv"))

D13Imp1 <-cbind.data.frame(D13[,c(1,2)],D13Imp1_B5, D13[,varsEmp])
D13Imp2 <-cbind.data.frame(D13[,c(1,2)],D13Imp2_B5, D13[,varsEmp])
D13Imp3 <-cbind.data.frame(D13[,c(1,2)],D13Imp3_B5, D13[,varsEmp])
D13Imp4 <-cbind.data.frame(D13[,c(1,2)],D13Imp4_B5, D13[,varsEmp])
D13Imp5 <-cbind.data.frame(D13[,c(1,2)],D13Imp5_B5, D13[,varsEmp])

#prepare a dataset only with complete data (i.e., no missing values)
D13_complete_data <- D13[complete.cases(D13[,ind:(ind+43)]),which(colnames(D13) %in% colnames(D13Imp1))]


###To check the different imputed datasets all I need to do is change the dataset in 
###the CreateReverseItems_allDat_age13 command

#2) reverse items-activate funtion first
CreateReverseItems_allData_age13(D13_complete_data)
D13Imp1.1 <- DImp

#defining the relevant variables
col <- colnames(D13Imp1.1)
relvar_emotional_13 <- c(which (col=="EMPQ_emotional"),
                         which (col=="tn_BFI1"), which (col=="tn_BFI2_Rev"), which (col=="tn_BFI3"),which (col=="tn_BFI4"),
                         which (col=="tn_BFI5"),which (col=="tn_BFI6_Rev"),which (col=="tn_BFI7"),which (col=="tn_BFI8_Rev"),
                         which (col=="tn_BFI9_Rev"),which (col=="tn_BFI10"),which (col=="tn_BFI11"),which (col=="tn_BFI12_Rev"),
                         which (col=="tn_BFI13"),which (col=="tn_BFI14"),which (col=="tn_BFI15"),which (col=="tn_BFI16"),
                         which (col=="tn_BFI17"),which (col=="tn_BFI18_Rev"),which (col=="tn_BFI19"),which (col=="tn_BFI20"),
                         which (col=="tn_BFI21_Rev"),which (col=="tn_BFI22"),which (col=="tn_BFI23_Rev"),which (col=="tn_BFI24_Rev"),
                         which (col=="tn_BFI25"),which (col=="tn_BFI26"),which (col=="tn_BFI27_Rev"),which (col=="tn_BFI28"),
                         which (col=="tn_BFI29"),which (col=="tn_BFI30"),which (col=="tn_BFI31_Rev"),which (col=="tn_BFI32"),
                         which (col=="tn_BFI33"),which (col=="tn_BFI34_Rev"),which (col=="tn_BFI35_Rev"),which (col=="tn_BFI36"),
                         which (col=="tn_BFI37_Rev"),which (col=="tn_BFI38"),which (col=="tn_BFI39"),which (col=="tn_BFI40"),
                         which (col=="tn_BFI41_Rev"),which (col=="tn_BFI42"),which (col=="tn_BFI43_Rev"),which (col=="tn_BFI44"))

relvar_cognitive_13 <- c(which (col=="EMPQ_cognitive"),
                         which (col=="tn_BFI1"), which (col=="tn_BFI2_Rev"), which (col=="tn_BFI3"),which (col=="tn_BFI4"),
                         which (col=="tn_BFI5"),which (col=="tn_BFI6_Rev"),which (col=="tn_BFI7"),which (col=="tn_BFI8_Rev"),
                         which (col=="tn_BFI9_Rev"),which (col=="tn_BFI10"),which (col=="tn_BFI11"),which (col=="tn_BFI12_Rev"),
                         which (col=="tn_BFI13"),which (col=="tn_BFI14"),which (col=="tn_BFI15"),which (col=="tn_BFI16"),
                         which (col=="tn_BFI17"),which (col=="tn_BFI18_Rev"),which (col=="tn_BFI19"),which (col=="tn_BFI20"),
                         which (col=="tn_BFI21_Rev"),which (col=="tn_BFI22"),which (col=="tn_BFI23_Rev"),which (col=="tn_BFI24_Rev"),
                         which (col=="tn_BFI25"),which (col=="tn_BFI26"),which (col=="tn_BFI27_Rev"),which (col=="tn_BFI28"),
                         which (col=="tn_BFI29"),which (col=="tn_BFI30"),which (col=="tn_BFI31_Rev"),which (col=="tn_BFI32"),
                         which (col=="tn_BFI33"),which (col=="tn_BFI34_Rev"),which (col=="tn_BFI35_Rev"),which (col=="tn_BFI36"),
                         which (col=="tn_BFI37_Rev"),which (col=="tn_BFI38"),which (col=="tn_BFI39"),which (col=="tn_BFI40"),
                         which (col=="tn_BFI41_Rev"),which (col=="tn_BFI42"),which (col=="tn_BFI43_Rev"),which (col=="tn_BFI44"))

newnames <- gsub(x=colnames(D13Imp1.1[,relvar_emotional_13]),pattern="tn_", replacement="")
colnames(D13Imp1.1)[relvar_emotional_13] <- newnames

#dividing the 6 folds so two twins from the same family will always be in the same test fold
unique_ifams_13 <- unique(D13Imp1.1$ifam)
set.seed(32189)
unique_ifams_13 <- sample(unique_ifams_13) 

remainder<-length(unique_ifams_13)%%6
nfold <- (length(unique_ifams_13)-remainder)/6
gfold <- c(rep(1, times=nfold), rep(2, times=nfold), rep(3, times=nfold),
           rep(4, times=nfold), rep(5, times=nfold), rep(6, times=nfold))
gfold <- c(gfold,1:remainder)
unique_ifams_13 <- cbind(unique_ifams_13,gfold)
ifams_13<- as.data.frame(rbind(unique_ifams_13, unique_ifams_13))
colnames(ifams_13) <- c("ifam","gfold")
ifams_13$ID<- c(rep(1,times=nrow(unique_ifams_13)),rep(4,times=nrow(unique_ifams_13)))

D13Imp1.1<- merge(D13Imp1.1,ifams_13, by=c("ifam","ID"), all.x = T, all.y = F)

#scale all the BFI items so they all will have mean=0 and SD=1
for (i in 2:45) {D13Imp1.1[,relvar_emotional_13[i]] <-
  scale(D13Imp1.1[,relvar_emotional_13[i]], scale=T)}


##############################################################################################
#doing Ridge regression on the folds 

#emotional empathy 13

Ridge(DImp=D13Imp1.1,gfold=1,relvar=relvar_emotional_13,lambdas=lambdas) 
fit_emo13_1 <- fit
opt_lambda_emo13_1 <- opt_lambda
opt_coef_emo13_1 <- opt_coef
y_pred_emo13_1 <- y_pred
mse_emo13_1 <- mse

#fold 2
Ridge(DImp=D13Imp1.1,gfold=2,relvar=relvar_emotional_13,lambdas=lambdas) 
fit_emo13_2 <- fit
opt_lambda_emo13_2 <- opt_lambda
opt_coef_emo13_2 <- opt_coef
y_pred_emo13_2 <- y_pred
mse_emo13_2 <- mse

#fold 3
Ridge(DImp=D13Imp1.1,gfold=3,relvar=relvar_emotional_13,lambdas=lambdas) 
fit_emo13_3 <- fit
opt_lambda_emo13_3 <- opt_lambda
opt_coef_emo13_3 <- opt_coef
y_pred_emo13_3 <- y_pred
mse_emo13_3 <- mse

#fold 4
Ridge(DImp=D13Imp1.1,gfold=4,relvar=relvar_emotional_13,lambdas=lambdas) 
fit_emo13_4 <- fit
opt_lambda_emo13_4 <- opt_lambda
opt_coef_emo13_4 <- opt_coef
y_pred_emo13_4 <- y_pred
mse_emo13_4 <- mse

#fold 5
Ridge(DImp=D13Imp1.1,gfold=5,relvar=relvar_emotional_13,lambdas=lambdas) 
fit_emo13_5 <- fit
opt_lambda_emo13_5 <- opt_lambda
opt_coef_emo13_5 <- opt_coef
y_pred_emo13_5 <- y_pred
mse_emo13_5 <- mse

#fold 6
Ridge(DImp=D13Imp1.1,gfold=6,relvar=relvar_emotional_13,lambdas=lambdas) 
fit_emo13_6 <- fit
opt_lambda_emo13_6 <- opt_lambda
opt_coef_emo13_6 <- opt_coef
y_pred_emo13_6 <- y_pred
mse_emo13_6 <- mse

#finding the mean correlation between outcome and predicted value across the folds
cor_emo13 <-1:6
for (i in 1:6) {
  cor_emo13[i] <- cor.test(D13Imp1.1$EMPQ_emotional[D13Imp1.1$gfold ==i],
                           eval(parse(text=paste0("y_pred_emo13_",i))))[4]}

cor_emo13 <- as.numeric(cor_emo13)
avecor_emo13 <- mean(cor_emo13)

#computing R^2
R2_emo13 <- as.numeric(cor_emo13^2)
aveR2_emo13 <- mean(R2_emo13)


#computing the mean mse across the folds
mse_emo13 <-1:6
for (i in 1:6) { mse_emo13[i] <- eval(parse(text=paste0("mse_emo13_",i)))}
avemse_emo13 <- mean(mse_emo13)

###############################################################################################
#cognitive empathy 13

Ridge(DImp=D13Imp1.1,gfold=1,relvar=relvar_cognitive_13,lambdas=lambdas) 
fit_cog13_1 <- fit
opt_lambda_cog13_1 <- opt_lambda
opt_coef_cog13_1 <- opt_coef
y_pred_cog13_1 <- y_pred
mse_cog13_1 <- mse

#fold 2
Ridge(DImp=D13Imp1.1,gfold=2,relvar=relvar_cognitive_13,lambdas=lambdas) 
fit_cog13_2 <- fit
opt_lambda_cog13_2 <- opt_lambda
opt_coef_cog13_2 <- opt_coef
y_pred_cog13_2 <- y_pred
mse_cog13_2 <- mse

#fold 3
Ridge(DImp=D13Imp1.1,gfold=3,relvar=relvar_cognitive_13,lambdas=lambdas) 
fit_cog13_3 <- fit
opt_lambda_cog13_3 <- opt_lambda
opt_coef_cog13_3 <- opt_coef
y_pred_cog13_3 <- y_pred
mse_cog13_3 <- mse

#fold 4
Ridge(DImp=D13Imp1.1,gfold=4,relvar=relvar_cognitive_13,lambdas=lambdas) 
fit_cog13_4 <- fit
opt_lambda_cog13_4 <- opt_lambda
opt_coef_cog13_4 <- opt_coef
y_pred_cog13_4 <- y_pred
mse_cog13_4 <- mse

#fold 5
Ridge(DImp=D13Imp1.1,gfold=5,relvar=relvar_cognitive_13,lambdas=lambdas) 
fit_cog13_5 <- fit
opt_lambda_cog13_5 <- opt_lambda
opt_coef_cog13_5 <- opt_coef
y_pred_cog13_5 <- y_pred
mse_cog13_5 <- mse

#fold 6
Ridge(DImp=D13Imp1.1,gfold=6,relvar=relvar_cognitive_13,lambdas=lambdas) 
fit_cog13_6 <- fit
opt_lambda_cog13_6 <- opt_lambda
opt_coef_cog13_6 <- opt_coef
y_pred_cog13_6 <- y_pred
mse_cog13_6 <- mse

#finding the mean correlation between outcome and predicted value across the folds
cor_cog13 <-1:6
for (i in 1:6) {
  cor_cog13[i] <- cor.test(D13Imp1.1$EMPQ_cognitive[D13Imp1.1$gfold ==i],
                           eval(parse(text=paste0("y_pred_cog13_",i))))[4]}

cor_cog13 <- as.numeric(cor_cog13)
avecor_cog13 <- mean(cor_cog13)

#computing R^2
R2_cog13 <- as.numeric(cor_cog13^2)
aveR2_cog13 <- mean(R2_cog13)


#computing the mean mse across the folds
mse_cog13 <-1:6
for (i in 1:6) { mse_cog13[i] <- eval(parse(text=paste0("mse_cog13_",i)))}
avemse_cog13 <- mean(mse_cog13)


###########################################################################################
#predicting age 13 with 11 and vice versa- OPERATE ALL OF THIS AFTER CHANGING THE IMPUTED DATASETS

#creating folds in 11_13 dataset
ifams_1113 <- merge(DImp1.1[,c(1:2,which(colnames(DImp1.1)=="gfold"))],
                    D13Imp1.1[,c(1:2,which(colnames(D13Imp1.1)=="gfold"))],
                    by=c("ifam","ID"), all.x=T, all.y=T)

unique_ifams_1113 <- unique(ifams_1113$ifam)
set.seed(32189)
unique_ifams_1113 <- sample(unique_ifams_1113) 

remainder<-length(unique_ifams_1113)%%6
nfold <- (length(unique_ifams_1113)-remainder)/6
gfold <- c(rep(1, times=nfold), rep(2, times=nfold), rep(3, times=nfold),
           rep(4, times=nfold), rep(5, times=nfold), rep(6, times=nfold))
gfold <- c(gfold,1:remainder)
unique_ifams_1113 <- cbind(unique_ifams_1113,gfold)
ifams_1113<- as.data.frame(rbind(unique_ifams_1113, unique_ifams_1113))
colnames(ifams_1113) <- c("ifam","gfold1113")
ifams_1113$ID<- c(rep(1,times=nrow(unique_ifams_1113)),rep(4,times=nrow(unique_ifams_1113)))

DImp1.1<- merge(DImp1.1,ifams_1113, by=c("ifam","ID"), all.x = T, all.y = F)
D13Imp1.1<- merge(D13Imp1.1,ifams_1113, by=c("ifam","ID"), all.x = T, all.y = F)

#adapting the Ridge function
Ridge1113 <- function (DImp11,DImp13, gfold,relvar,lambdas) {
  #find the best lambda- 
  #use either glmnet default range search by lambda=NULL or set search by lambda=lambdas  
  set.seed(10000)
  cv_fit <- cv.glmnet(x=as.matrix(DImp11[DImp11$gfold1113 != gfold,relvar[2:45]]),
                      y=DImp11[DImp11$gfold1113 != gfold,relvar[1]],
                      alpha=0, lambda=NULL,nfolds=10)
  opt_lambda <- cv_fit$lambda.min
  opt_lambda_ind <- which(cv_fit$lambda==opt_lambda)
  
  #what are the coefficients when the lambda is optimal
  opt_coef <- as.matrix(cv_fit$glmnet.fit$beta[,opt_lambda_ind]) 
  
  #after finding the best lambda, train the entire train set with that lambda
  fit <- glmnet(x=as.matrix(DImp11[DImp11$gfold1113 != gfold,relvar[2:45]]), 
                y=DImp11[DImp11$gfold1113 != gfold,relvar[1]], 
                alpha = 0, lambda = opt_lambda)
  
  #now check the prediction on the test set
  y_pred <- predict(fit, s=opt_lambda, 
                    newx = as.matrix(DImp13[DImp13$gfold1113 == gfold,relvar[2:45]]))
  
  mse <- mean((DImp13[DImp13$gfold1113 == gfold,relvar[1]]-y_pred)^2)
  
  assign ("fit",fit,envir = .GlobalEnv)
  assign ("opt_lambda",opt_lambda,envir = .GlobalEnv)
  assign ("opt_coef",opt_coef,envir = .GlobalEnv)
  assign ("y_pred",y_pred,envir = .GlobalEnv)
  assign ("mse",mse,envir = .GlobalEnv)
}

#doing Ridge regression on the folds

Ridge1113(DImp11=DImp1.1,DImp13=D13Imp1.1, gfold=1,
          relvar=relvar_emotional,
          lambdas=lambdas) 
fit_emo_1 <- fit
opt_lambda_emo_1 <- opt_lambda
opt_coef_emo_1 <- opt_coef
y_pred_emo_1 <- y_pred
mse_emo_1 <- mse


Ridge1113(DImp11=DImp1.1,DImp13=D13Imp1.1, gfold=2,
          relvar=relvar_emotional,
          lambdas=lambdas) 
fit_emo_2 <- fit
opt_lambda_emo_2 <- opt_lambda
opt_coef_emo_2 <- opt_coef
y_pred_emo_2 <- y_pred
mse_emo_2 <- mse

Ridge1113(DImp11=DImp1.1,DImp13=D13Imp1.1, gfold=3,
          relvar=relvar_emotional,
          lambdas=lambdas) 
fit_emo_3 <- fit
opt_lambda_emo_3 <- opt_lambda
opt_coef_emo_3 <- opt_coef
y_pred_emo_3 <- y_pred
mse_emo_3 <- mse

Ridge1113(DImp11=DImp1.1,DImp13=D13Imp1.1, gfold=4,
          relvar=relvar_emotional,
          lambdas=lambdas) 
fit_emo_4 <- fit
opt_lambda_emo_4 <- opt_lambda
opt_coef_emo_4 <- opt_coef
y_pred_emo_4 <- y_pred
mse_emo_4 <- mse

Ridge1113(DImp11=DImp1.1,DImp13=D13Imp1.1, gfold=5,
          relvar=relvar_emotional,
          lambdas=lambdas) 
fit_emo_5 <- fit
opt_lambda_emo_5 <- opt_lambda
opt_coef_emo_5 <- opt_coef
y_pred_emo_5 <- y_pred
mse_emo_5 <- mse

Ridge1113(DImp11=DImp1.1,DImp13=D13Imp1.1, gfold=6,
          relvar=relvar_emotional,
          lambdas=lambdas) 
fit_emo_6 <- fit
opt_lambda_emo_6 <- opt_lambda
opt_coef_emo_6 <- opt_coef
y_pred_emo_6 <- y_pred
mse_emo_6 <- mse


#finding the mean correlation between outcome and predicted value across the folds
cor_emo <-1:6
for (i in 1:6) {
  cor_emo[i] <- cor.test(D13Imp1.1$EMPQ_emotional[D13Imp1.1$gfold1113 ==i],
                         eval(parse(text=paste0("y_pred_emo_",i))))[4]}

cor_emo <- as.numeric(cor_emo)
avecor_emo <- mean(cor_emo)

#computing R^2
R2_emo <- as.numeric(cor_emo^2)
aveR2_emo <- mean(R2_emo)


#computing the mean mse across the folds
mse_emo <-1:6
for (i in 1:6) { mse_emo[i] <- eval(parse(text=paste0("mse_emo_",i)))}
avemse_emo <- mean(mse_emo)


#cognitive empathy

#doing Ridge regression on the folds

Ridge1113(DImp11=DImp1.1,DImp13=D13Imp1.1, gfold=1,
          relvar=relvar_cognitive,
          lambdas=lambdas) 
fit_cog_1 <- fit
opt_lambda_cog_1 <- opt_lambda
opt_coef_cog_1 <- opt_coef
y_pred_cog_1 <- y_pred
mse_cog_1 <- mse


Ridge1113(DImp11=DImp1.1,DImp13=D13Imp1.1, gfold=2,
          relvar=relvar_cognitive,
          lambdas=lambdas) 
fit_cog_2 <- fit
opt_lambda_cog_2 <- opt_lambda
opt_coef_cog_2 <- opt_coef
y_pred_cog_2 <- y_pred
mse_cog_2 <- mse

Ridge1113(DImp11=DImp1.1,DImp13=D13Imp1.1, gfold=3,
          relvar=relvar_cognitive,
          lambdas=lambdas) 
fit_cog_3 <- fit
opt_lambda_cog_3 <- opt_lambda
opt_coef_cog_3 <- opt_coef
y_pred_cog_3 <- y_pred
mse_cog_3 <- mse

Ridge1113(DImp11=DImp1.1,DImp13=D13Imp1.1, gfold=4,
          relvar=relvar_cognitive,
          lambdas=lambdas) 
fit_cog_4 <- fit
opt_lambda_cog_4 <- opt_lambda
opt_coef_cog_4 <- opt_coef
y_pred_cog_4 <- y_pred
mse_cog_4 <- mse

Ridge1113(DImp11=DImp1.1,DImp13=D13Imp1.1, gfold=5,
          relvar=relvar_cognitive,
          lambdas=lambdas) 
fit_cog_5 <- fit
opt_lambda_cog_5 <- opt_lambda
opt_coef_cog_5 <- opt_coef
y_pred_cog_5 <- y_pred
mse_cog_5 <- mse

Ridge1113(DImp11=DImp1.1,DImp13=D13Imp1.1, gfold=6,
          relvar=relvar_cognitive,
          lambdas=lambdas) 
fit_cog_6 <- fit
opt_lambda_cog_6 <- opt_lambda
opt_coef_cog_6 <- opt_coef
y_pred_cog_6 <- y_pred
mse_cog_6 <- mse


#finding the mean correlation between outcome and predicted value across the folds
cor_cog <-1:6
for (i in 1:6) {
  cor_cog[i] <- cor.test(D13Imp1.1$EMPQ_cognitive[D13Imp1.1$gfold1113 ==i],
                         eval(parse(text=paste0("y_pred_cog_",i))))[4]}

cor_cog <- as.numeric(cor_cog)
avecor_cog <- mean(cor_cog)

#computing R^2
R2_cog <- as.numeric(cor_cog^2)
aveR2_cog <- mean(R2_cog)


#computing the mean mse across the folds
mse_cog <-1:6
for (i in 1:6) { mse_cog[i] <- eval(parse(text=paste0("mse_cog_",i)))}
avemse_cog <- mean(mse_cog)

