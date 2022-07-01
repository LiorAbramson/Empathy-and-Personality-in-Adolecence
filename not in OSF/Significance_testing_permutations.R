############################################################################################
###This code builds on the global environmente created in the script SVR_14_Ridge_regression
###The datasets DImp1.1 and D13Imp1.1 are prepared (and scaled there)
###The relevant variables (relvar_emotional etc.) are also defined there
###Thus, the whole script needs to run before I run this script
############################################################################################

library(glmnet)

#############################################################################################
#first build the Ridge function that will be inside the permutation function
#############################################################################################
RidgePermut <- function (DImpPerm,gfold,relvar,lambdas) {
    #find the best lambda- 
    #use either glmnet default range search by lambda=NULL or set search by lambda=lambdas  
    #alpha=0 means we use Ridge regression (and not lasso regression)
    set.seed(10000)
    cv_fit <- cv.glmnet(x=as.matrix(DImpPerm[DImpPerm$gfold != gfold,relvar[2:45]]),
                        y=DImpPerm[DImpPerm$gfold != gfold,relvar[1]],
                        alpha=0, lambda=NULL,nfolds=10)
    opt_lambda <- cv_fit$lambda.min
    opt_lambda_ind <- which(cv_fit$lambda==opt_lambda)
    
    #what are the coefficients when the lambda is optimal
    opt_coef <- as.matrix(cv_fit$glmnet.fit$beta[,opt_lambda_ind]) 
    
    #extract the highest coefficient (in absolute values)
    opt_coef_max <- max (abs(opt_coef))
    
    #after finding the best lambda, train the entire train set with that lambda
    fit <- glmnet(x=as.matrix(DImpPerm[DImpPerm$gfold != gfold,relvar[2:45]]), 
                  y=DImpPerm[DImpPerm$gfold != gfold,relvar[1]], 
                  alpha = 0, lambda = opt_lambda)
    
    #now check the prediction on the test set
    y_pred <- predict(fit, s=opt_lambda, 
                      newx = as.matrix(DImpPerm[DImpPerm$gfold == gfold,relvar[2:45]]))
    
    mse <- mean((DImpPerm[DImpPerm$gfold == gfold,relvar[1]]-y_pred)^2)
    
    assign ("fit",fit,envir = .GlobalEnv)
    assign ("opt_lambda",opt_lambda,envir = .GlobalEnv)
    assign ("y_pred",y_pred,envir = .GlobalEnv)
    assign ("mse", mse,envir = .GlobalEnv)
    assign ("opt_coef_max", opt_coef_max,envir = .GlobalEnv)
}

############################################################################################
#The same function adapted to predicting age 13 from age 11 data
############################################################################################

RidgePermut1113 <- function (DImpPerm11,DImpPerm13, gfold,relvar,lambdas) {
    #find the best lambda- 
    #use either glmnet default range search by lambda=NULL or set search by lambda=lambdas  
    set.seed(10000)
    cv_fit <- cv.glmnet(x=as.matrix(DImpPerm11[DImpPerm11$gfold1113 != gfold,relvar[2:45]]),
                        y=DImpPerm11[DImpPerm11$gfold1113 != gfold,relvar[1]],
                        alpha=0, lambda=NULL,nfolds=10)
    opt_lambda <- cv_fit$lambda.min
    opt_lambda_ind <- which(cv_fit$lambda==opt_lambda)
    
    #after finding the best lambda, train the entire train set with that lambda
    fit <- glmnet(x=as.matrix(DImpPerm11[DImpPerm11$gfold1113 != gfold,relvar[2:45]]), 
                  y=DImpPerm11[DImpPerm11$gfold1113 != gfold,relvar[1]], 
                  alpha = 0, lambda = opt_lambda)
    
    #now check the prediction on the test set
    y_pred <- predict(fit, s=opt_lambda, 
                      newx = as.matrix(DImpPerm13[DImpPerm13$gfold1113 == gfold,relvar[2:45]]))
    
    mse <- mean((DImpPerm13[DImpPerm13$gfold1113 == gfold,relvar[1]]-y_pred)^2)
    
    assign ("fit",fit,envir = .GlobalEnv)
    assign ("opt_lambda",opt_lambda,envir = .GlobalEnv)
    assign ("y_pred",y_pred,envir = .GlobalEnv)
    assign ("mse",mse,envir = .GlobalEnv)
}

############################################################################################
#Preparing the scrambled data for the permutation process
############################################################################################
#The folds (i.e., which family is in each fold) do not change, nor do the variables.
#the only thing that changes, inside the function, is the data (DImp). 
#Each time the empathy variable is scrambled so there won't be real relation between 
#personality and empathy (because the empathy measure does not really belong to the child)

ScrambleEmpathyVar <- function (DImp,relvar,yname){  
    #randomize the empathy variable- switch between families and not between individuals to keep
    #the dependency between twins
    library(reshape)
    DImpPerm <- DImp[,c(1:2,relvar, which (colnames(DImp)=="gfold"))]
    DImpPermWide <- reshape(DImpPerm, v.names = colnames(DImpPerm[3:(3+length(relvar)-1)]),
                            timevar = "ID", idvar = "ifam", direction="wide") 
    
    DImpPermWide_y <- DImpPermWide[,c(which(colnames(DImpPermWide)=="ifam"),
                                      which(colnames(DImpPermWide)==paste0(yname,".1")),
                                      which(colnames(DImpPermWide)==paste0(yname,".4")))]
    
    #to keep the proportion of missing twins , I scramble separtely families with both twins,
    #families with just twin A and  families with just twin B
    DImpPermWide_y_both <- na.omit(DImpPermWide_y)
    DImpPermWide_y_TA <- DImpPermWide_y[is.na(DImpPermWide_y[,3]),]
    DImpPermWide_y_TB <- DImpPermWide_y[is.na(DImpPermWide_y[,2]),] 
    
    #now scramble the ifam variable 
    DImpPermWide_y_both$ifam<- sample(DImpPermWide_y_both$ifam)
    DImpPermWide_y_TA$ifam<- sample(DImpPermWide_y_TA$ifam)
    DImpPermWide_y_TB$ifam<- sample(DImpPermWide_y_TB$ifam)
    
    DImpPermWide_y <- rbind (DImpPermWide_y_both,DImpPermWide_y_TA,DImpPermWide_y_TB)
    
    #change name of the empathy variable
    colnames(DImpPermWide_y)[2] <- "EMPQ_perm.1"
    colnames(DImpPermWide_y)[3] <- "EMPQ_perm.4"
    
    #merge the variables to the file. Now each family recieves the scores of another family
    DImpPermWide <- merge (DImpPermWide,DImpPermWide_y,by="ifam", all.x=T, all.y=T)
    DImpPerm <- reshape(DImpPermWide,
                        varying = colnames(DImpPermWide)[-c(which(colnames(DImpPermWide)=="ifam"),
                                           which(colnames(DImpPermWide)=="gfold"))],
                        timevar = "ID", idvar = "ifam",direction="long") 
    DImpPerm <- na.omit(DImpPerm)
    
    assign ("DImpPerm", DImpPerm,envir = .GlobalEnv)
}


############################################################################################
#Preparing the scrambled data for age 13
############################################################################################

#The only thing that changed is that I switch the order of the coloumns.
#This is because the first family have only twin B in that data (1000-4) 
#which makes Twin B come before twin A. I marked the rows that changed

ScrambleEmpathyVar13 <- function (DImp,relvar,yname){  
  #randomize the empathy variable- switch between families and not between individuals to keep
  #the dependency between twins
  library(reshape)
  DImpPerm <- DImp[,c(1:2,relvar, which (colnames(DImp)=="gfold"))]
  DImpPermWide <- reshape(DImpPerm, v.names = colnames(DImpPerm[3:(3+length(relvar)-1)]),
                          timevar = "ID", idvar = "ifam", direction="wide") 
  
  DImpPermWide_y <- DImpPermWide[,c(which(colnames(DImpPermWide)=="ifam"),
                                    which(colnames(DImpPermWide)==paste0(yname,".1")),
                                    which(colnames(DImpPermWide)==paste0(yname,".4")))]
  
  #to keep the proportion of missing twins , I scramble separtely families with both twins,
  #families with just twin A and families with just twin B
  DImpPermWide_y_both <- na.omit(DImpPermWide_y)
  DImpPermWide_y_TA <- DImpPermWide_y[is.na(DImpPermWide_y[,3]),]
  DImpPermWide_y_TB <- DImpPermWide_y[is.na(DImpPermWide_y[,2]),] 
  
  #now scramble the ifam variable 
  DImpPermWide_y_both$ifam<- sample(DImpPermWide_y_both$ifam)
  DImpPermWide_y_TA$ifam<- sample(DImpPermWide_y_TA$ifam)
  DImpPermWide_y_TB$ifam<- sample(DImpPermWide_y_TB$ifam)
  
  DImpPermWide_y <- rbind (DImpPermWide_y_both,DImpPermWide_y_TA,DImpPermWide_y_TB)
  
  #change name of the empathy variable
  colnames(DImpPermWide_y)[2] <- "EMPQ_perm.1"
  colnames(DImpPermWide_y)[3] <- "EMPQ_perm.4"
 
  ##THIS IS WHAT CHANGED FROM AGE11- change the order of the coloumns so twin B will come first
  DImpPermWide_y <- DImpPermWide_y[,c(1,3,2)]
  
  #merge the variables to the file. Now each family recieves the scores of another family
  DImpPermWide <- merge (DImpPermWide,DImpPermWide_y,by="ifam", all.x=T, all.y=T)
  DImpPerm <- reshape(DImpPermWide,
                      varying = colnames(DImpPermWide)[-c(which(colnames(DImpPermWide)=="ifam"),
                                                          which(colnames(DImpPermWide)=="gfold"))],
                      timevar = "ID", idvar = "ifam",direction="long") 
  DImpPerm <- na.omit(DImpPerm)
  
  assign ("DImpPerm", DImpPerm,envir = .GlobalEnv)
}


############################################################################################
#The permutation function that will be itterated n times:
############################################################################################

CrossValRidgePermut <- function (DImp,relvar,yname,lambdas,RidgePermut,ScrambleEmpathyVar){
  
  #call a new scrambled dataset (DImpPerm)
  set.seed(NULL) #make sure previous seeds are canceled so it will be random
  ScrambleEmpathyVar(DImp,relvar,yname)
  
  #defining the new relevant variables (relvar_perm)
  col <-colnames(DImpPerm)
  relvar_perm <- c(which (col=="EMPQ_perm"),
                   which (col=="BFI1"), which (col=="BFI2_Rev"), which (col=="BFI3"),which (col=="BFI4"),
                   which (col=="BFI5"),which (col=="BFI6_Rev"),which (col=="BFI7"),which (col=="BFI8_Rev"),
                   which (col=="BFI9_Rev"),which (col=="BFI10"),which (col=="BFI11"),which (col=="BFI12_Rev"),
                   which (col=="BFI13"),which (col=="BFI14"),which (col=="BFI15"),which (col=="BFI16"),
                   which (col=="BFI17"),which (col=="BFI18_Rev"),which (col=="BFI19"),which (col=="BFI20"),
                   which (col=="BFI21_Rev"),which (col=="BFI22"),which (col=="BFI23_Rev"),which (col=="BFI24_Rev"),
                   which (col=="BFI25"),which (col=="BFI26"),which (col=="BFI27_Rev"),which (col=="BFI28"),
                   which (col=="BFI29"),which (col=="BFI30"),which (col=="BFI31_Rev"),which (col=="BFI32"),
                   which (col=="BFI33"),which (col=="BFI34_Rev"),which (col=="BFI35_Rev"),which (col=="BFI36"),
                   which (col=="BFI37_Rev"),which (col=="BFI38"),which (col=="BFI39"),which (col=="BFI40"),
                   which (col=="BFI41_Rev"),which (col=="BFI42"),which (col=="BFI43_Rev"),which (col=="BFI44"))
  
  
  #doing Ridge regression on the folds 
  library(glmnet)
  #fold 1
  RidgePermut(DImpPerm=DImpPerm,gfold=1,relvar=relvar_perm,lambdas=lambdas) 
  fit_P_1 <- fit
  opt_lambda_P_1 <- opt_lambda
  y_pred_P_1 <- y_pred
  mse_P_1 <- mse
  opt_coef_max_1 <- opt_coef_max  
  
  #fold 2
  RidgePermut(DImpPerm=DImpPerm,gfold=2,relvar=relvar_perm,lambdas=lambdas) 
  fit_P_2 <- fit
  opt_lambda_P_2 <- opt_lambda
  y_pred_P_2 <- y_pred
  mse_P_2 <- mse
  opt_coef_max_2 <- opt_coef_max
  
  #fold 3
  RidgePermut(DImpPerm=DImpPerm,gfold=3,relvar=relvar_perm,lambdas=lambdas) 
  fit_P_3 <- fit
  opt_lambda_P_3 <- opt_lambda
  y_pred_P_3 <- y_pred
  mse_P_3 <- mse
  opt_coef_max_3 <- opt_coef_max
  
  #fold 4
  RidgePermut(DImpPerm=DImpPerm,gfold=4,relvar=relvar_perm,lambdas=lambdas) 
  fit_P_4 <- fit
  opt_lambda_P_4 <- opt_lambda
  y_pred_P_4 <- y_pred
  mse_P_4 <- mse
  opt_coef_max_4 <- opt_coef_max
  
  #fold 5
  RidgePermut(DImpPerm=DImpPerm,gfold=5,relvar=relvar_perm,lambdas=lambdas) 
  fit_P_5 <- fit
  opt_lambda_P_5 <- opt_lambda
  y_pred_P_5 <- y_pred
  mse_P_5 <- mse
  opt_coef_max_5 <- opt_coef_max
  
  #fold 6
  RidgePermut(DImpPerm=DImpPerm,gfold=6,relvar=relvar_perm,lambdas=lambdas) 
  fit_P_6 <- fit
  opt_lambda_P_6 <- opt_lambda
  y_pred_P_6 <- y_pred
  mse_P_6 <- mse
  opt_coef_max_6 <- opt_coef_max
  
  
  #finding the mean correlation between outcome and predicted value across the folds
  cor_P <-1:6
  for (i in 1:6) {
    cor_P[i] <- cor.test(DImpPerm$EMPQ_perm[DImpPerm$gfold ==i],
                         eval(parse(text=paste0("y_pred_P_",i))))[4]}
  
  cor_P <- as.numeric(cor_P)
  avecor_Permut <- mean(cor_P)
  
  #computing the mean mse across the folds
  mse_P <-1:6
  for (i in 1:6) { mse_P[i] <- eval(parse(text=paste0("mse_P_",i)))}
  avemse_Permut <- mean(mse_P)
  
  #computing the mean maximum coefficient
  avemaxcoef_Permut <- mean(c(opt_coef_max_1,opt_coef_max_2,opt_coef_max_3,
                              opt_coef_max_4,opt_coef_max_5,opt_coef_max_6))
  
  #take out the average correlation, average mse, and average maximal coefficient from the function
  AveCorMSEPermut <- c(avecor_Permut,avemse_Permut,avemaxcoef_Permut)
  assign ("AveCorMSEPermut",AveCorMSEPermut,envir = .GlobalEnv)
}


############################################################################################
#Preparing the scrambled data for the permutation process- 11 and 13 together
############################################################################################
#The only thing that changed from the regular function is that gfold is named gfold1113

ScrambleEmpathyVarBothAges <- function (DImp,relvar,yname){  
  #randomize the empathy variable- switch between families and not between individuals to keep
  #the dependency between twins
  library(reshape)
  DImpPerm <- DImp[,c(1:2,relvar, which (colnames(DImp)=="gfold1113"))]
  DImpPermWide <- reshape(DImpPerm, v.names = colnames(DImpPerm[3:(3+length(relvar)-1)]),
                          timevar = "ID", idvar = "ifam", direction="wide") 
  
  DImpPermWide_y <- DImpPermWide[,c(which(colnames(DImpPermWide)=="ifam"),
                                    which(colnames(DImpPermWide)==paste0(yname,".1")),
                                    which(colnames(DImpPermWide)==paste0(yname,".4")))]
  
  #to keep the proportion of missing twins , I scramble separtely families with both twins,
  #families with just twin A and  families with just twin B
  DImpPermWide_y_both <- na.omit(DImpPermWide_y)
  DImpPermWide_y_TA <- DImpPermWide_y[is.na(DImpPermWide_y[,3]),]
  DImpPermWide_y_TB <- DImpPermWide_y[is.na(DImpPermWide_y[,2]),] 
  
  #now scramble the ifam variable 
  DImpPermWide_y_both$ifam<- sample(DImpPermWide_y_both$ifam)
  DImpPermWide_y_TA$ifam<- sample(DImpPermWide_y_TA$ifam)
  DImpPermWide_y_TB$ifam<- sample(DImpPermWide_y_TB$ifam)
  
  DImpPermWide_y <- rbind (DImpPermWide_y_both,DImpPermWide_y_TA,DImpPermWide_y_TB)
  
  #change name of the empathy variable
  colnames(DImpPermWide_y)[2] <- "EMPQ_perm.1"
  colnames(DImpPermWide_y)[3] <- "EMPQ_perm.4"
  
  #merge the variables to the file. Now each family recieves the scores of another family
  DImpPermWide <- merge (DImpPermWide,DImpPermWide_y,by="ifam", all.x=T, all.y=T)
  DImpPerm <- reshape(DImpPermWide,
                      varying = colnames(DImpPermWide)[-c(which(colnames(DImpPermWide)=="ifam"),
                                                          which(colnames(DImpPermWide)=="gfold1113"))],
                      timevar = "ID", idvar = "ifam",direction="long") 
  DImpPerm <- na.omit(DImpPerm)
  
  assign ("DImpPerm", DImpPerm,envir = .GlobalEnv)
}


############################################################################################
#Preparing the scrambled data for age 13-for the analysis of age 11 and 13 together 
############################################################################################

ScrambleEmpathyVarBothAges13 <- function (DImp,relvar,yname){  
  #randomize the empathy variable- switch between families and not between individuals to keep
  #the dependency between twins
  library(reshape)
  DImpPerm <- DImp[,c(1:2,relvar, which (colnames(DImp)=="gfold1113"))]
  DImpPermWide <- reshape(DImpPerm, v.names = colnames(DImpPerm[3:(3+length(relvar)-1)]),
                          timevar = "ID", idvar = "ifam", direction="wide") 
  
  DImpPermWide_y <- DImpPermWide[,c(which(colnames(DImpPermWide)=="ifam"),
                                    which(colnames(DImpPermWide)==paste0(yname,".1")),
                                    which(colnames(DImpPermWide)==paste0(yname,".4")))]
  
  #to keep the proportion of missing twins , I scramble separtely families with both twins,
  #families with just twin A and families with just twin B
  DImpPermWide_y_both <- na.omit(DImpPermWide_y)
  DImpPermWide_y_TA <- DImpPermWide_y[is.na(DImpPermWide_y[,3]),]
  DImpPermWide_y_TB <- DImpPermWide_y[is.na(DImpPermWide_y[,2]),] 
  
  #now scramble the ifam variable 
  DImpPermWide_y_both$ifam<- sample(DImpPermWide_y_both$ifam)
  DImpPermWide_y_TA$ifam<- sample(DImpPermWide_y_TA$ifam)
  DImpPermWide_y_TB$ifam<- sample(DImpPermWide_y_TB$ifam)
  
  DImpPermWide_y <- rbind (DImpPermWide_y_both,DImpPermWide_y_TA,DImpPermWide_y_TB)
  
  #change name of the empathy variable
  colnames(DImpPermWide_y)[2] <- "EMPQ_perm.1"
  colnames(DImpPermWide_y)[3] <- "EMPQ_perm.4"
  
  ##THIS IS WHAT CHANGED FROM AGE11- change the order of the coloumns so twin B will come first
  DImpPermWide_y <- DImpPermWide_y[,c(1,3,2)]
  
  #merge the variables to the file. Now each family recieves the scores of another family
  DImpPermWide <- merge (DImpPermWide,DImpPermWide_y,by="ifam", all.x=T, all.y=T)
  DImpPerm <- reshape(DImpPermWide,
                      varying = colnames(DImpPermWide)[-c(which(colnames(DImpPermWide)=="ifam"),
                                                          which(colnames(DImpPermWide)=="gfold1113"))],
                      timevar = "ID", idvar = "ifam",direction="long") 
  DImpPerm <- na.omit(DImpPerm)
  
  assign ("DImpPerm", DImpPerm,envir = .GlobalEnv)
}


############################################################################################
#The permutation function that will be itterated n times- adapted to age 11&13 together
############################################################################################

CrossValRidgePermut1113 <- function (DImp11,DImp13, relvar,yname11,yname13, lambdas,
                                     RidgePermut1113, ScrambleEmpathyVar,ScrambleEmpathyVar13){
  
  #call a new scrambled dataset (DImpPerm11)
  set.seed(NULL) #make sure previous seeds are canceled so it will be random
  ScrambleEmpathyVarBothAges(DImp11,relvar,yname11)
  DImpPerm11 <- DImpPerm
  
  #call a new scrambled dataset for age 13(DImpPerm13)
  set.seed(NULL) #make sure previous seeds are canceled so it will be random
  ScrambleEmpathyVarBothAges13(DImp13,relvar,yname13)
  DImpPerm13 <- DImpPerm
  
  #defining the new relevant variables (relvar_perm)
  col <-colnames(DImpPerm11)
  relvar_perm <- c(which (col=="EMPQ_perm"),
                   which (col=="BFI1"), which (col=="BFI2_Rev"), which (col=="BFI3"),which (col=="BFI4"),
                   which (col=="BFI5"),which (col=="BFI6_Rev"),which (col=="BFI7"),which (col=="BFI8_Rev"),
                   which (col=="BFI9_Rev"),which (col=="BFI10"),which (col=="BFI11"),which (col=="BFI12_Rev"),
                   which (col=="BFI13"),which (col=="BFI14"),which (col=="BFI15"),which (col=="BFI16"),
                   which (col=="BFI17"),which (col=="BFI18_Rev"),which (col=="BFI19"),which (col=="BFI20"),
                   which (col=="BFI21_Rev"),which (col=="BFI22"),which (col=="BFI23_Rev"),which (col=="BFI24_Rev"),
                   which (col=="BFI25"),which (col=="BFI26"),which (col=="BFI27_Rev"),which (col=="BFI28"),
                   which (col=="BFI29"),which (col=="BFI30"),which (col=="BFI31_Rev"),which (col=="BFI32"),
                   which (col=="BFI33"),which (col=="BFI34_Rev"),which (col=="BFI35_Rev"),which (col=="BFI36"),
                   which (col=="BFI37_Rev"),which (col=="BFI38"),which (col=="BFI39"),which (col=="BFI40"),
                   which (col=="BFI41_Rev"),which (col=="BFI42"),which (col=="BFI43_Rev"),which (col=="BFI44"))
  
  
  #doing Ridge regression on the folds 
  library(glmnet)
  #fold 1
  RidgePermut1113(DImpPerm11=DImpPerm11,DImpPerm13=DImpPerm13, 
                  gfold=1,relvar=relvar_perm,lambdas=lambdas) 
  fit_P_1 <- fit
  opt_lambda_P_1 <- opt_lambda
  y_pred_P_1 <- y_pred
  mse_P_1 <- mse
  
  #fold 2
  RidgePermut1113(DImpPerm11=DImpPerm11,DImpPerm13=DImpPerm13, 
                  gfold=2,relvar=relvar_perm,lambdas=lambdas)  
  fit_P_2 <- fit
  opt_lambda_P_2 <- opt_lambda
  y_pred_P_2 <- y_pred
  mse_P_2 <- mse
  
  #fold 3
  RidgePermut1113(DImpPerm11=DImpPerm11,DImpPerm13=DImpPerm13, 
                  gfold=3,relvar=relvar_perm,lambdas=lambdas) 
  fit_P_3 <- fit
  opt_lambda_P_3 <- opt_lambda
  y_pred_P_3 <- y_pred
  mse_P_3 <- mse
  
  #fold 4
  RidgePermut1113(DImpPerm11=DImpPerm11,DImpPerm13=DImpPerm13, 
                  gfold=4,relvar=relvar_perm,lambdas=lambdas) 
  fit_P_4 <- fit
  opt_lambda_P_4 <- opt_lambda
  y_pred_P_4 <- y_pred
  mse_P_4 <- mse
  
  #fold 5
  RidgePermut1113(DImpPerm11=DImpPerm11,DImpPerm13=DImpPerm13, 
                  gfold=5,relvar=relvar_perm,lambdas=lambdas) 
  fit_P_5 <- fit
  opt_lambda_P_5 <- opt_lambda
  y_pred_P_5 <- y_pred
  mse_P_5 <- mse
  
  #fold 6
  RidgePermut1113(DImpPerm11=DImpPerm11,DImpPerm13=DImpPerm13, 
                  gfold=6,relvar=relvar_perm,lambdas=lambdas) 
  fit_P_6 <- fit
  opt_lambda_P_6 <- opt_lambda
  y_pred_P_6 <- y_pred
  mse_P_6 <- mse
  
  
  #finding the mean correlation between outcome and predicted value across the folds
  cor_P <-1:6
  for (i in 1:6) {
    cor_P[i] <- cor.test(DImpPerm13$EMPQ_perm[DImpPerm13$gfold1113 ==i],
                         eval(parse(text=paste0("y_pred_P_",i))))[4]}
  
  cor_P <- as.numeric(cor_P)
  avecor_Permut <- mean(cor_P)
  
  #computing the mean mse across the folds
  mse_P <-1:6
  for (i in 1:6) { mse_P[i] <- eval(parse(text=paste0("mse_P_",i)))}
  avemse_Permut <- mean(mse_P)
  
  #take out the average correlation and avverage mse from the function
  AveCorMSEPermut <- c(avecor_Permut,avemse_Permut)
  assign ("AveCorMSEPermut",AveCorMSEPermut,envir = .GlobalEnv)
}


###########################################################################################
#running the function
###########################################################################################

#Emotional empathy-11
PermutVector_emo11 <- replicate(n=10000,
                                expr=CrossValRidgePermut(
                                DImp=DImp1.1,relvar=relvar_emotional,
                                yname="EMPQ_emotional_11",lambdas=lambdas,
                                RidgePermut,ScrambleEmpathyVar))

hist (PermutVector_emo11[1,]) #see the distribution of the correlations
hist (PermutVector_emo11[2,]) #see the distribution of the mse

#now check- what is the proportion of cases in which the real correlation is higher than the
#correlations in the distribution, and the propotion of cases in which the real MSE is lower 
#than the MSEs in the distribution 

realCor_emo11 <- avecor_emo11
proportionCor_emo11 <- sum (realCor_emo11 > PermutVector_emo11[1,] )

realMSE_emo11 <- avemse_emo11
proportionMSE_emo11 <- sum (realMSE_emo11 < PermutVector_emo11[2,] )


#Cognitive empathy-11
PermutVector_cog11 <- replicate(n=10000,
                                expr=CrossValRidgePermut(
                                  DImp=DImp1.1,relvar=relvar_cognitive,
                                  yname="EMPQ_cognitive_11",lambdas=lambdas,
                                  RidgePermut,ScrambleEmpathyVar))



hist (PermutVector_cog11[1,]) #see the distribution of the correlations
hist (PermutVector_cog11[2,]) #see the distribution of the mse

#now check- what is the proportion of cases in which the real correlation is higher than the
#correlations in the distribution, and the propotion of cases in which the real MSE is lower 
#than the MSEs in the distribution 

realCor_cog11 <- avecor_cog11
proportionCor_cog11 <- sum (realCor_cog11 > PermutVector_cog11[1,] )

realMSE_cog11 <- avemse_cog11
proportionMSE_cog11 <- sum (realMSE_cog11 < PermutVector_cog11[2,] )


#Emotional empathy-13
PermutVector_emo13 <- replicate(n=10000,
                                expr=CrossValRidgePermut(
                                  DImp=D13Imp1.1,relvar=relvar_emotional_13,
                                  yname="EMPQ_emotional_13",lambdas=lambdas,
                                  RidgePermut,ScrambleEmpathyVar13))


hist (PermutVector_emo13[1,]) #see the distribution of the correlations
hist (PermutVector_emo13[2,]) #see the distribution of the mse

realCor_emo13 <- avecor_emo13
proportionCor_emo13 <- sum (realCor_emo13 > PermutVector_emo13[1,] )

realMSE_emo13 <- avemse_emo13
proportionMSE_emo13 <- sum (realMSE_emo13 < PermutVector_emo13[2,] )


#Cognitive empathy-13
PermutVector_cog13 <- replicate(n=10000,
                                expr=CrossValRidgePermut(
                                  DImp=D13Imp1.1,relvar=relvar_cognitive_13,
                                  yname="EMPQ_cognitive_13",lambdas=lambdas,
                                  RidgePermut,ScrambleEmpathyVar13))


hist (PermutVector_cog13[1,]) #see the distribution of the correlations
hist (PermutVector_cog13[2,]) #see the distribution of the mse

realCor_cog13 <- avecor_cog13
proportionCor_cog13 <- sum (realCor_cog13 > PermutVector_cog13[1,] )

realMSE_cog13 <- avemse_cog13
proportionMSE_cog13 <- sum (realMSE_cog13 < PermutVector_cog13[2,] )


#Emotional empathy-13 based on age 11 data
PermutVector_emo <- replicate(n=10000,
                                expr=CrossValRidgePermut1113(
                                  DImp11=DImp1.1,DImp13=D13Imp1.1,relvar=relvar_emotional,
                                  yname11="EMPQ_emotional_11",yname13="EMPQ_emotional_13",
                                  lambdas=lambdas,RidgePermut1113,
                                  ScrambleEmpathyVarBothAges, ScrambleEmpathyVarBothAges13))



hist (PermutVector_emo[1,]) #see the distribution of the correlations
hist (PermutVector_emo[2,]) #see the distribution of the mse

realCor_emo <- avecor_emo
proportionCor_emo <- sum (realCor_emo > PermutVector_emo[1,] )

realMSE_emo <- avemse_emo
proportionMSE_emo <- sum (realMSE_emo < PermutVector_emo[2,] )


#Cognitive empathy-13 based on age 11 data
PermutVector_cog <- replicate(n=10000,
                              expr=CrossValRidgePermut1113(
                                DImp11=DImp1.1,DImp13=D13Imp1.1,relvar=relvar_cognitive,
                                yname11="EMPQ_cognitive_11",yname13="EMPQ_cognitive_13",
                                lambdas=lambdas,RidgePermut1113,
                                ScrambleEmpathyVarBothAges, ScrambleEmpathyVarBothAges13))



hist (PermutVector_cog[1,]) #see the distribution of the correlations
hist (PermutVector_cog[2,]) #see the distribution of the mse

realCor_cog <- avecor_cog
proportionCor_cog <- sum (realCor_cog > PermutVector_cog[1,] )

realMSE_cog <- avemse_cog
proportionMSE_cog <- sum (realMSE_cog < PermutVector_cog[2,] )



#####################################################################################################
##With coefficients

#Emotional empathy-11
PermutVector_emo11_withCoef <- replicate(n=10000,
                                expr=CrossValRidgePermut(
                                  DImp=DImp1.1,relvar=relvar_emotional,
                                  yname="EMPQ_emotional_11",lambdas=lambdas,
                                  RidgePermut,ScrambleEmpathyVar))

hist (PermutVector_emo11_withCoef[3,]) #see the distribution of the maximal coefficients

#define the real coefficients (in absolute values)
realCoef_emo11 <- abs(opt_coef_emo11_matrix$aveCoef)

proportionCoef_emo11 <- matrix(nrow=1,ncol=44)
for (i in 1:44){
  proportionCoef_emo11[i] <- sum (realCoef_emo11[i] > PermutVector_emo11_withCoef[3,])}

#what coefficients are higher than the maximal value in 95% of the cases? 
proportionCoef_emo11_sig <- proportionCoef_emo11 > 9500
proportionCoef_emo11_sig01 <- proportionCoef_emo11 > 9900

#add the significance to the plots and adjust the graph limits
opt_coef_emo11_matrix$star <- ifelse(t(proportionCoef_emo11_sig01)==T,"**","")
plotemo11 + geom_text(data = opt_coef_emo11_matrix, 
                      label = opt_coef_emo11_matrix$star, nudge_y = 0)+ ylim(-.03, .13)
     


#FOR SUPPLEMENTRY- add the significance at p <.05 to the plots and adjust the graph limits
opt_coef_emo11_matrix$star05 <- ifelse(t(proportionCoef_emo11_sig01)==T,"**",
                                     ifelse (t(proportionCoef_emo11_sig)==T,"*",""))
plotemo11 + geom_text(data = opt_coef_emo11_matrix, 
                      label = opt_coef_emo11_matrix$star05, nudge_y = 0)+ ylim(-.03, .13)

#Cognitive empathy-11
PermutVector_cog11_withCoef <- replicate(n=10000,
                                         expr=CrossValRidgePermut(
                                           DImp=DImp1.1,relvar=relvar_cognitive,
                                           yname="EMPQ_cognitive_11",lambdas=lambdas,
                                           RidgePermut,ScrambleEmpathyVar))

hist (PermutVector_cog11_withCoef[3,]) #see the distribution of the maximal coefficients

#define the real coefficients (in absolute values)
realCoef_cog11 <- abs(opt_coef_cog11_matrix$aveCoef)

proportionCoef_cog11 <- matrix(nrow=1,ncol=44)
for (i in 1:44){
  proportionCoef_cog11[i] <- sum (realCoef_cog11[i] > PermutVector_cog11_withCoef[3,])}

#what coefficients are higher than the maximal value in 95% of the cases? 
proportionCoef_cog11_sig <- proportionCoef_cog11 > 9500
proportionCoef_cog11_sig01 <- proportionCoef_cog11 > 9900

#add the significance to the plots
opt_coef_cog11_matrix$star <- ifelse(t(proportionCoef_cog11_sig01)==T,"**","")
plotcog11 + geom_text(data = opt_coef_cog11_matrix, 
                      label = opt_coef_cog11_matrix$star, nudge_y = 0)+ ylim(-.03, .13)


#FOR SUPPLEMENTRY- add the significance at p <.05 to the plots and adjust the graph limits
opt_coef_cog11_matrix$star05 <- ifelse(t(proportionCoef_cog11_sig01)==T,"**",
                                       ifelse (t(proportionCoef_cog11_sig)==T,"*",""))
plotcog11 + geom_text(data = opt_coef_cog11_matrix, 
                      label = opt_coef_cog11_matrix$star05, nudge_y = 0)+ ylim(-.03, .13)


#Emotional empathy- 13
PermutVector_emo13_withCoef <- replicate(n=10000,
                                expr=CrossValRidgePermut(
                                  DImp=D13Imp1.1,relvar=relvar_emotional_13,
                                  yname="EMPQ_emotional_13",lambdas=lambdas,
                                  RidgePermut,ScrambleEmpathyVar13))


hist (PermutVector_emo13_withCoef[3,]) #see the distribution of the maximal coefficients

#define the real coefficients (in absolute values)
realCoef_emo13 <- abs(opt_coef_emo13_matrix$aveCoef)

proportionCoef_emo13 <- matrix(nrow=1,ncol=44)
for (i in 1:44){
  proportionCoef_emo13[i] <- sum (realCoef_emo13[i] > PermutVector_emo13_withCoef[3,])}

#what coefficients are higher than the maximal value in 95% of the cases? 
proportionCoef_emo13_sig <- proportionCoef_emo13 > 9500
proportionCoef_emo13_sig01 <- proportionCoef_emo13 > 9900

#add the significance to the plots
opt_coef_emo13_matrix$star <- ifelse(t(proportionCoef_emo13_sig01)==T,"**","")
plotemo13 + geom_text(data = opt_coef_emo13_matrix, 
                      label = opt_coef_emo13_matrix$star, nudge_y = 0)+ ylim(-.03, .13)


#FOR SUPPLEMENTRY- add the significance at p <.05 to the plots and adjust the graph limits
opt_coef_emo13_matrix$star05 <- ifelse(t(proportionCoef_emo13_sig01)==T,"**",
                                       ifelse (t(proportionCoef_emo13_sig)==T,"*",""))
plotemo13 + geom_text(data = opt_coef_emo13_matrix, 
                      label = opt_coef_emo13_matrix$star05, nudge_y = 0)+ ylim(-.03, .13)



#Cognitive empathy- 13
PermutVector_cog13_withCoef <- replicate(n=10000,
                                         expr=CrossValRidgePermut(
                                           DImp=D13Imp1.1,relvar=relvar_cognitive_13,
                                           yname="EMPQ_cognitive_13",lambdas=lambdas,
                                           RidgePermut,ScrambleEmpathyVar13))


hist (PermutVector_cog13_withCoef[3,]) #see the distribution of the maximal coefficients

#define the real coefficients (in absolute values)
realCoef_cog13 <- abs(opt_coef_cog13_matrix$aveCoef)

proportionCoef_cog13 <- matrix(nrow=1,ncol=44)
for (i in 1:44){
  proportionCoef_cog13[i] <- sum (realCoef_cog13[i] > PermutVector_cog13_withCoef[3,])}

#what coefficients are higher than the maximal value in 95% of the cases? 
proportionCoef_cog13_sig <- proportionCoef_cog13 > 9500
proportionCoef_cog13_sig01 <- proportionCoef_cog13 > 9900


#add the significance to the plots
opt_coef_cog13_matrix$star <- ifelse(t(proportionCoef_cog13_sig01)==T,"**","")
plotcog13 + geom_text(data = opt_coef_cog13_matrix, 
                      label = opt_coef_cog13_matrix$star, nudge_y = 0)+ ylim(-.03, .13)

#FOR SUPPLEMENTRY- add the significance at p <.05 to the plots and adjust the graph limits
opt_coef_cog13_matrix$star05 <- ifelse(t(proportionCoef_cog13_sig01)==T,"**",
                                       ifelse (t(proportionCoef_cog13_sig)==T,"*",""))
plotcog13 + geom_text(data = opt_coef_cog13_matrix, 
                      label = opt_coef_cog13_matrix$star05, nudge_y = 0)+ ylim(-.03, .13)


