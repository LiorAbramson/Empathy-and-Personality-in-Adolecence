# This script performs the ridge regression, predicting empathy from Big-Five nuanced items
# and creates the empathic personality profiles

#Preparations
  rm(list = ls()) # clean the global environment
  cat ("\014")    #clean the R console

#Packages
  packages <- c('psych', 'Hmisc', 'glmnet','ggplot2', 'reshape')
  lapply(packages, require, character.only = TRUE)

#Preparing the file
  D <- read.csv("~/Documents/projects/Empathy-Personality-Adolecence/OSF/data/Age11_EmpPer_anonymized.csv")
  D <- D[D$isOut==0,]                      #take out children that should not be included (see supplementary method)
  D <- subset(D, !is.na(D$EMPQ_emotional)) #take out children with no empathy measures

#############################################################################################
######################### Data Preprocessing - Age 11 #######################################
#############################################################################################

#Handling missing values

#first count how many missing values are in each var:
  ind <- which(colnames(D)=="el_BFI1")
  descD <- describe(D[,ind:(ind+43)])
  #calculate percentage of missing values for each item (descD$itemname[2])
  missingBFI11 <- c()
  for (i in 1:44){
    eval(parse(text= paste0("missingBFI11[",i,"] <-as.numeric(descD$el_BFI",i,
                            "$counts[2])/ nrow(D)")))
  }
  
  max(missingBFI11)           #find the maximum percentage of missing values

#Imputations of missing values for the Big5 (pmm)
  set.seed(12) #set the random vector to always be the same vector
  Imp_B5 <- aregImpute (formula= ~el_BFI1+ el_BFI2+ el_BFI3+ el_BFI4+ el_BFI5+ el_BFI6+ el_BFI7+ el_BFI8+el_BFI9+ el_BFI10+
                               el_BFI11+ el_BFI12+ el_BFI13+ el_BFI14+ el_BFI15+ el_BFI16+ el_BFI17+ el_BFI18+el_BFI19+ el_BFI20+
                               el_BFI21+ el_BFI22+ el_BFI23+ el_BFI24+ el_BFI25+ el_BFI26+ el_BFI27+ el_BFI28+el_BFI29+ el_BFI30+
                               el_BFI31+ el_BFI32+ el_BFI33+ el_BFI34+ el_BFI35+ el_BFI36+ el_BFI37+ el_BFI38+el_BFI39+ el_BFI40+
                               el_BFI41+ el_BFI42+ el_BFI43+ el_BFI44, 
                               data=D, x=T,n.impute=5 , nk=0, type="pmm")


#Creating five data sets with different imputed values
#DImp1 is the data frame reported in the paper. 
#The overall prediction of empathy from personality was examined on the other 4 datasets 
#as a robustness check (see appendix)
  DImp1_B5 <-as.data.frame(impute.transcan(Imp_B5,imputation=1,data=D,list.out=T,pr=F))
  DImp2_B5 <-as.data.frame(impute.transcan(Imp_B5,imputation=2,data=D,list.out=T,pr=F))
  DImp3_B5 <-as.data.frame(impute.transcan(Imp_B5,imputation=3,data=D,list.out=T,pr=F))
  DImp4_B5 <-as.data.frame(impute.transcan(Imp_B5,imputation=4,data=D,list.out=T,pr=F))
  DImp5_B5 <-as.data.frame(impute.transcan(Imp_B5,imputation=5,data=D,list.out=T,pr=F))

  col <- colnames(D)
  varsEmp <- c(which(col=="EMPQ_emotional"), which(col=="EMPQ_cognitive"))

  DImp1 <-cbind.data.frame(D[,c(1,2)],DImp1_B5, D[,varsEmp])
  DImp2 <-cbind.data.frame(D[,c(1,2)],DImp2_B5, D[,varsEmp])
  DImp3 <-cbind.data.frame(D[,c(1,2)],DImp3_B5, D[,varsEmp])
  DImp4 <-cbind.data.frame(D[,c(1,2)],DImp4_B5, D[,varsEmp])
  DImp5 <-cbind.data.frame(D[,c(1,2)],DImp5_B5, D[,varsEmp])


#reverse items  (items' content description will be added soon)
  CreateReverseItems_allData <-function(DImp) {
    DImp$el_BFI6_Rev <-  6-DImp$el_BFI6
    DImp$el_BFI21_Rev <- 6-DImp$el_BFI21
    DImp$el_BFI31_Rev <- 6-DImp$el_BFI31
    DImp$el_BFI2_Rev <-  6-DImp$el_BFI2
    DImp$el_BFI12_Rev <- 6-DImp$el_BFI12
    DImp$el_BFI27_Rev <- 6-DImp$el_BFI27
    DImp$el_BFI37_Rev <- 6-DImp$el_BFI37
    DImp$el_BFI8_Rev <-  6-DImp$el_BFI8
    DImp$el_BFI18_Rev <- 6-DImp$el_BFI18
    DImp$el_BFI23_Rev <- 6-DImp$el_BFI23
    DImp$el_BFI43_Rev <- 6-DImp$el_BFI43
    DImp$el_BFI9_Rev <-  6-DImp$el_BFI9
    DImp$el_BFI24_Rev <- 6-DImp$el_BFI24
    DImp$el_BFI34_Rev <- 6-DImp$el_BFI34
    DImp$el_BFI35_Rev <- 6-DImp$el_BFI35
    DImp$el_BFI41_Rev <- 6-DImp$el_BFI41
    
    assign ("DImp",DImp,envir = .GlobalEnv)
  }
 
CreateReverseItems_allData(DImp1)    #if you want to check another imputed dataset, enter it instead of DImp1
DImp1.1 <- DImp                  

#defining the relevant variables for the Ridge regression
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

#changing the names of age 11 and age 13 to be the same
  newnames <- gsub(x=colnames(DImp1.1[,relvar_emotional]),pattern="el_", replacement="")
  colnames(DImp1.1)[relvar_emotional] <- newnames

#dividing the 6 folds so two twins from the same family will always be in the same test fold
#gfold is the variable allocating family to a specific fold
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
  ifams$ID<- c(rep(1,times=nrow(unique_ifams)),rep(4,times=nrow(unique_ifams)))  # allocate ID for each twin (ID 1=older twin, ID 4= younger twin) 
  
  DImp1.1<- merge(DImp1.1,ifams, by=c("ifam","ID"), all.x = T, all.y = F)        #merge the gfold var with the data
  

#############################################################################################
######################### Ridge regression function #########################################
#############################################################################################

#create a vector of possible lambda values. These values will be examined in 
#a nested cross-validation procedure to find the optimal lambda that produces the lowest prediction error

  
#ridge regression function
  Ridge <- function (DImp,gfold,relvar) {
         #find the best lambda- 
         #use glmnet default range search by lambda=NULL 
         #alpha=0 means we use Ridge regression (as opposed to Lasso regression)
         #nfolds=10 means that the nested cross-validation procedure for finding the optimal lambda is performed on 10 folds 
         set.seed(10000)
         cv_fit <- cv.glmnet(x=as.matrix(DImp[DImp$gfold != gfold,relvar[2:45]]),
                             y=DImp[DImp$gfold != gfold,relvar[1]],
                             alpha=0, lambda=NULL,nfolds=10)
         
         opt_lambda <- cv_fit$lambda.min          #find the optimal lambda which produces the lowest prediction error
         opt_lambda_ind <- which(cv_fit$lambda==opt_lambda)   #find the optimal lambda index inside the vector
         
         #what are the coefficients when the lambda is optimal?
         opt_coef <- as.matrix(cv_fit$glmnet.fit$beta[,opt_lambda_ind]) 
         
         #after finding the best lambda, train the entire train set with that lambda
         fit <- glmnet(x=as.matrix(DImp[DImp$gfold != gfold,relvar[2:45]]), 
                       y=DImp[DImp$gfold != gfold,relvar[1]], 
                       alpha = 0, lambda = opt_lambda)
         
         #now check the prediction on the test set
         y_pred <- predict(fit, s=opt_lambda, 
                           newx = as.matrix(DImp[DImp$gfold == gfold,relvar[2:45]]))
         
         mse <- mean((DImp[DImp$gfold == gfold,relvar[1]]-y_pred)^2)     #MSE: mean of the squared differences between y and y predicted in the test set
         
         assign ("fit",fit,envir = .GlobalEnv)                           #glmnet ridge regression results
         assign ("opt_lambda",opt_lambda,envir = .GlobalEnv)             #optimal lambda value
         assign ("opt_coef",opt_coef,envir = .GlobalEnv)                 #items' coefficients when the lambda is optimal
         assign ("y_pred",y_pred,envir = .GlobalEnv)                     #y predicted for each participant
         assign ("mse", mse,envir = .GlobalEnv)                          #mse for the test set
}

#############################################################################################
######################### Ridge regression - Age 11 #########################################
#############################################################################################  

#scale all the BFI items so they all will mean=0 and SD=1 
  for (i in 2:45) {DImp1.1[,relvar_emotional[i]] <-
     scale(DImp1.1[,relvar_emotional[i]], scale=T)}

##Emotional empathy

#doing Ridge regression on the folds 
#fold 1
  Ridge(DImp=DImp1.1,gfold=1,relvar=relvar_emotional) 
  fit_emo11_1 <- fit
  opt_lambda_emo11_1 <- opt_lambda
  opt_coef_emo11_1 <- opt_coef
  y_pred_emo11_1 <- y_pred
  mse_emo11_1 <- mse

#fold 2
  Ridge(DImp=DImp1.1,gfold=2,relvar=relvar_emotional) 
  fit_emo11_2 <- fit
  opt_lambda_emo11_2 <- opt_lambda
  opt_coef_emo11_2 <- opt_coef
  y_pred_emo11_2 <- y_pred
  mse_emo11_2 <- mse

#fold 3
  Ridge(DImp=DImp1.1,gfold=3,relvar=relvar_emotional) 
  fit_emo11_3 <- fit
  opt_lambda_emo11_3 <- opt_lambda
  opt_coef_emo11_3 <- opt_coef
  y_pred_emo11_3 <- y_pred
  mse_emo11_3 <- mse

#fold 4
  Ridge(DImp=DImp1.1,gfold=4,relvar=relvar_emotional) 
  fit_emo11_4 <- fit
  opt_lambda_emo11_4 <- opt_lambda
  opt_coef_emo11_4 <- opt_coef
  y_pred_emo11_4 <- y_pred
  mse_emo11_4 <- mse

#fold 5
  Ridge(DImp=DImp1.1,gfold=5,relvar=relvar_emotional) 
  fit_emo11_5 <- fit
  opt_lambda_emo11_5 <- opt_lambda
  opt_coef_emo11_5 <- opt_coef
  y_pred_emo11_5 <- y_pred
  mse_emo11_5 <- mse

#fold 6
  Ridge(DImp=DImp1.1,gfold=6,relvar=relvar_emotional) 
  fit_emo11_6 <- fit
  opt_lambda_emo11_6 <- opt_lambda
  opt_coef_emo11_6 <- opt_coef
  y_pred_emo11_6 <- y_pred
  mse_emo11_6 <- mse
  

#computing the mean coefficients across the folds
  opt_coef_emo11_matrix <- as.data.frame(cbind (opt_coef_emo11_1,
                                  opt_coef_emo11_2,
                                  opt_coef_emo11_3,
                                  opt_coef_emo11_4,
                                  opt_coef_emo11_5,
                                  opt_coef_emo11_6))
  opt_coef_emo11_matrix$aveCoef <- rowMeans(opt_coef_emo11_matrix)

#finding the mean correlation between the outcome and the predicted value (y pred) across the folds
  cor_emo11 <-1:6
  #first do this for each test set
  for (i in 1:6) {
     cor_emo11[i] <- cor.test(DImp1.1$EMPQ_emotional[DImp1.1$gfold ==i],
                              eval(parse(text=paste0("y_pred_emo11_",i))))[4]}
  
  cor_emo11 <- as.numeric(cor_emo11)
  #now average across all the folds
  avecor_emo11 <- mean(cor_emo11)   #mean correlation
  aveR2_emo11  <- avecor_emo11^2    #mean R2

#computing the mean mse across the folds
  mse_emo11 <-1:6
  for (i in 1:6) { mse_emo11[i] <- eval(parse(text=paste0("mse_emo11_",i)))}
  avemse_emo11 <- mean(mse_emo11)


#Cognitive empathy
  
#doing Ridge regression on the folds
#fold 1
  Ridge(DImp=DImp1.1,gfold=1,relvar=relvar_cognitive) 
  fit_cog11_1 <- fit
  opt_lambda_cog11_1 <- opt_lambda
  opt_coef_cog11_1 <- opt_coef
  y_pred_cog11_1 <- y_pred
  mse_cog11_1 <- mse

#fold 2
  Ridge(DImp=DImp1.1,gfold=2,relvar=relvar_cognitive) 
  fit_cog11_2 <- fit
  opt_lambda_cog11_2 <- opt_lambda
  opt_coef_cog11_2 <- opt_coef
  y_pred_cog11_2 <- y_pred
  mse_cog11_2 <- mse

#fold 3
  Ridge(DImp=DImp1.1,gfold=3,relvar=relvar_cognitive) 
  fit_cog11_3 <- fit
  opt_lambda_cog11_3 <- opt_lambda
  opt_coef_cog11_3 <- opt_coef
  y_pred_cog11_3 <- y_pred
  mse_cog11_3 <- mse

#fold 4
  Ridge(DImp=DImp1.1,gfold=4,relvar=relvar_cognitive) 
  fit_cog11_4 <- fit
  opt_lambda_cog11_4 <- opt_lambda
  opt_coef_cog11_4 <- opt_coef
  y_pred_cog11_4 <- y_pred
  mse_cog11_4 <- mse

#fold 5
  Ridge(DImp=DImp1.1,gfold=5,relvar=relvar_cognitive) 
  fit_cog11_5 <- fit
  opt_lambda_cog11_5 <- opt_lambda
  opt_coef_cog11_5 <- opt_coef
  y_pred_cog11_5 <- y_pred
  mse_cog11_5 <- mse

#fold 6
  Ridge(DImp=DImp1.1,gfold=6,relvar=relvar_cognitive) 
  fit_cog11_6 <- fit
  opt_lambda_cog11_6 <- opt_lambda
  opt_coef_cog11_6 <- opt_coef
  y_pred_cog11_6 <- y_pred
  mse_cog11_6 <- mse

#computing the mean coefficients across the folds
  opt_coef_cog11_matrix <- as.data.frame(cbind (opt_coef_cog11_1,
                                              opt_coef_cog11_2,
                                              opt_coef_cog11_3,
                                              opt_coef_cog11_4,
                                              opt_coef_cog11_5,
                                              opt_coef_cog11_6))
  opt_coef_cog11_matrix$aveCoef <- rowMeans(opt_coef_cog11_matrix)


#finding the mean correlation between outcome and predicted value across the folds
  cor_cog11 <-1:6
  for (i in 1:6) {
     cor_cog11[i] <- cor.test(DImp1.1$EMPQ_cognitive[DImp1.1$gfold ==i],
                              eval(parse(text=paste0("y_pred_cog11_",i))))[4]}
  
  cor_cog11 <- as.numeric(cor_cog11)
  avecor_cog11 <- mean(cor_cog11)  #mean correlation
  aveR2_cog11  <- avecor_cog11^2    #mean R2
  

#computing the mean mse across the folds
  mse_cog11 <-1:6
  for (i in 1:6) { mse_cog11[i] <- eval(parse(text=paste0("mse_cog11_",i)))}
  avemse_cog11 <- mean(mse_cog11)

  
#############################################################################################
################## Specific items' coefficients visualization - Age 11 ######################
#############################################################################################  

#emotional empathy
    
#allocate items to their original Big-Five domains
  opt_coef_emo11_matrix$category <- NA
  
  #EXTRAVERSION
  for (i in c(1,11,16,26,36)){
    opt_coef_emo11_matrix$category[rownames(opt_coef_emo11_matrix)==paste0("BFI",i)] <- "E"}
  #reverse items
  for (i in c(6,21,31)){
    opt_coef_emo11_matrix$category[rownames(opt_coef_emo11_matrix)==paste0("BFI",i,"_Rev")] <- "E"}
  
  #AGREABELNESS
  for (i in c(7,17,22,32,42)){
    opt_coef_emo11_matrix$category[rownames(opt_coef_emo11_matrix)==paste0("BFI",i)] <- "A"}
  #reverse items
  for (i in c(2,12,27,37)){
    opt_coef_emo11_matrix$category[rownames(opt_coef_emo11_matrix)==paste0("BFI",i,"_Rev")] <- "A"}
  
  #OPENESS
  for (i in c(5,10,15,20,25,30,40,44)){
    opt_coef_emo11_matrix$category[rownames(opt_coef_emo11_matrix)==paste0("BFI",i)] <- "O"}
  #reverse items
  for (i in c(35,41)){
    opt_coef_emo11_matrix$category[rownames(opt_coef_emo11_matrix)==paste0("BFI",i,"_Rev")]<- "O"}
  
  #CONCIENCIOUSNESS
  for (i in c(3,13,28,33,38)){
    opt_coef_emo11_matrix$category[rownames(opt_coef_emo11_matrix)==paste0("BFI",i)] <- "C"}
  #reverse items
  for (i in c(8,18,23,43)){
    opt_coef_emo11_matrix$category[rownames(opt_coef_emo11_matrix)==paste0("BFI",i,"_Rev")]<- "C"}
  
  #NEUROTICISM
  for (i in c(4,14,19,29,39)){
    opt_coef_emo11_matrix$category[rownames(opt_coef_emo11_matrix)==paste0("BFI",i)] <- "N"}
  #reverse items
  for (i in c(9,24,34)){
    opt_coef_emo11_matrix$category[rownames(opt_coef_emo11_matrix)==paste0("BFI",i,"_Rev")]<-"N"}
  
  BFI_labels <- read.csv ("../OSF/data/BFI items.csv")
  
  opt_coef_emo11_matrix$item <- rownames(opt_coef_emo11_matrix)
  opt_coef_emo11_matrix <- cbind(opt_coef_emo11_matrix, BFI_labels)
  
  
  plotemo11 <- ggplot(data= opt_coef_emo11_matrix,
                      aes(reorder(x=opt_coef_emo11_matrix$label,opt_coef_emo11_matrix$aveCoef),
                          y=opt_coef_emo11_matrix$aveCoef, 
                          fill=opt_coef_emo11_matrix$category))+
                          geom_bar(stat="identity")+ coord_flip()+ 
                          ggtitle("Regression coefficients of  personality indicators")+
                          labs(x="Personality indicator", y="Regression coefficient", fill="Big5 scale")+
                          scale_fill_manual(values=c("#F8A6F8", "#F7563B", "#F59D3D", "#433FF3", "#5FD3D3"))+
                          theme_bw()+ theme(legend.position=c(0.95, 0.15), legend.title = element_text(size=12),
                          plot.title=element_text(size=16, face="bold", family="serif",hjust = 0.5),
                          axis.title =element_text(size=12, face="bold", family="serif"),
                          text=element_text(family="serif"))
  
#cognitive empathy
  
#allocate items to their original Big-Five domains
  opt_coef_cog11_matrix$category <- NA

  #EXTRAVERSION
  for (i in c(1,11,16,26,36)){
   opt_coef_cog11_matrix$category[rownames(opt_coef_cog11_matrix)==paste0("BFI",i)] <- "E"}
  #reverse items
  for (i in c(6,21,31)){
   opt_coef_cog11_matrix$category[rownames(opt_coef_cog11_matrix)==paste0("BFI",i,"_Rev")] <- "E"}

  #AGREABELNESS
  for (i in c(7,17,22,32,42)){
   opt_coef_cog11_matrix$category[rownames(opt_coef_cog11_matrix)==paste0("BFI",i)] <- "A"}
  #reverse items  
  for (i in c(2,12,27,37)){
   opt_coef_cog11_matrix$category[rownames(opt_coef_cog11_matrix)==paste0("BFI",i,"_Rev")] <- "A"}

  #OPENESS
  for (i in c(5,10,15,20,25,30,40,44)){
   opt_coef_cog11_matrix$category[rownames(opt_coef_cog11_matrix)==paste0("BFI",i)] <- "O"}
  #reverse items
  for (i in c(35,41)){
   opt_coef_cog11_matrix$category[rownames(opt_coef_cog11_matrix)==paste0("BFI",i,"_Rev")]<- "O"}

  #CONCIENCIOUSNESS
  for (i in c(3,13,28,33,38)){
   opt_coef_cog11_matrix$category[rownames(opt_coef_cog11_matrix)==paste0("BFI",i)] <- "C"}
  #reverse items
  for (i in c(8,18,23,43)){
   opt_coef_cog11_matrix$category[rownames(opt_coef_cog11_matrix)==paste0("BFI",i,"_Rev")]<- "C"}

  #NEUROTICISM
  for (i in c(4,14,19,29,39)){
   opt_coef_cog11_matrix$category[rownames(opt_coef_cog11_matrix)==paste0("BFI",i)] <- "N"}
  #reverse items
  for (i in c(9,24,34)){
   opt_coef_cog11_matrix$category[rownames(opt_coef_cog11_matrix)==paste0("BFI",i,"_Rev")]<-"N"}

  opt_coef_cog11_matrix$item <- rownames(opt_coef_cog11_matrix)
  opt_coef_cog11_matrix <- cbind(opt_coef_cog11_matrix, BFI_labels)

  plotcog11 <- ggplot(data= opt_coef_cog11_matrix,
                    aes(reorder(x=opt_coef_cog11_matrix$label,opt_coef_cog11_matrix$aveCoef),
                    y=opt_coef_cog11_matrix$aveCoef, 
                    fill=opt_coef_cog11_matrix$category))+
                    geom_bar(stat="identity")+ coord_flip()+ 
                    ggtitle("Regression coefficients of  personality indicators")+
                    labs(x="Personality indicator", y="Regression coefficient", fill="Big5 scale")+
                    scale_fill_manual(values=c("#F8A6F8", "#F7563B", "#F59D3D", "#433FF3", "#5FD3D3"))+
                    theme_bw()+ theme(legend.position=c(0.95, 0.15), legend.title = element_text(size=12),
                    plot.title=element_text(size=16, face="bold", family="serif",hjust = 0.5),
                    axis.title =element_text(size=12, face="bold", family="serif"),
                    text=element_text(family="serif"))
  

#################################################################################################
############################ Data preprocessing - AGE 13  #######################################
#################################################################################################

#preparing the file
  D13 <- read.csv ("~/Documents/projects/Empathy-Personality-Adolecence/OSF/data/Age13_EmpPer_anonymized.csv")
  D13 <- D13[D13$isOut==0,]                      #take out children that should not be included (see supplementary method)
  D13 <- subset(D13, !is.na(D13$EMPQ_emotional)) #take out children with no empathy measures

#Handling missing values
  
#first count how many missing values are in each var:
  ind <- which(colnames(D13)=="tn_BFI1")
  descD13 <- describe(D13[,ind:(ind+43)])
  #calculate percentage of missing values for each item (descD13$itemname[2])
  missingBFI13 <- c()
  for (i in 1:44){
    eval(parse(text= paste0("missingBFI13[",i,"] <-as.numeric(descD13$tn_BFI",i,
                            "$counts[2])/ nrow(D13)")))
  }
  max(missingBFI13)    #find the maximum percentage of missing values
  
  
#Imputations of missing values for the Big5 (pmm)
  set.seed(12) #set the random vector to always be the same vector
  Imp13_B5 <- aregImpute (formula= ~tn_BFI1+ tn_BFI2+ tn_BFI3+ tn_BFI4+ tn_BFI5+ tn_BFI6+ tn_BFI7+ tn_BFI8+tn_BFI9+ tn_BFI10+
                           tn_BFI11+ tn_BFI12+ tn_BFI13+ tn_BFI14+ tn_BFI15+ tn_BFI16+ tn_BFI17+ tn_BFI18+tn_BFI19+ tn_BFI20+
                           tn_BFI21+ tn_BFI22+ tn_BFI23+ tn_BFI24+ tn_BFI25+ tn_BFI26+ tn_BFI27+ tn_BFI28+tn_BFI29+ tn_BFI30+
                           tn_BFI31+ tn_BFI32+ tn_BFI33+ tn_BFI34+ tn_BFI35+ tn_BFI36+ tn_BFI37+ tn_BFI38+tn_BFI39+ tn_BFI40+
                           tn_BFI41+ tn_BFI42+ tn_BFI43+ tn_BFI44, 
                           data=D13, x=T,n.impute=5 , nk=0, type="pmm")


#Creating five data sets with different imputed values
#DImp1 is the data frame reported in the paper. 
#The overall prediction of empathy from personality was examined on the other 4 datasets as a robustness check
  D13Imp1_B5 <-as.data.frame(impute.transcan(Imp13_B5,imputation=1,data=D13,list.out=T,pr=F))
  D13Imp2_B5 <-as.data.frame(impute.transcan(Imp13_B5,imputation=2,data=D13,list.out=T,pr=F))
  D13Imp3_B5 <-as.data.frame(impute.transcan(Imp13_B5,imputation=3,data=D13,list.out=T,pr=F))
  D13Imp4_B5 <-as.data.frame(impute.transcan(Imp13_B5,imputation=4,data=D13,list.out=T,pr=F))
  D13Imp5_B5 <-as.data.frame(impute.transcan(Imp13_B5,imputation=5,data=D13,list.out=T,pr=F))

  col <- colnames(D13)
  varsEmp <- c(which(col=="EMPQ_emotional"), which(col=="EMPQ_cognitive"))

  D13Imp1 <-cbind.data.frame(D13[,c(1,2)],D13Imp1_B5, D13[,varsEmp])
  D13Imp2 <-cbind.data.frame(D13[,c(1,2)],D13Imp2_B5, D13[,varsEmp])
  D13Imp3 <-cbind.data.frame(D13[,c(1,2)],D13Imp3_B5, D13[,varsEmp])
  D13Imp4 <-cbind.data.frame(D13[,c(1,2)],D13Imp4_B5, D13[,varsEmp])
  D13Imp5 <-cbind.data.frame(D13[,c(1,2)],D13Imp5_B5, D13[,varsEmp])

#reverse items  (items' content description will be added soon)
  CreateReverseItems_allData_age13 <-function(DImp) {
    DImp$tn_BFI6_Rev <-  6-DImp$tn_BFI6
    DImp$tn_BFI21_Rev <- 6-DImp$tn_BFI21
    DImp$tn_BFI31_Rev <- 6-DImp$tn_BFI31
    DImp$tn_BFI2_Rev <-  6-DImp$tn_BFI2
    DImp$tn_BFI12_Rev <- 6-DImp$tn_BFI12
    DImp$tn_BFI27_Rev <- 6-DImp$tn_BFI27
    DImp$tn_BFI37_Rev <- 6-DImp$tn_BFI37
    DImp$tn_BFI8_Rev <-  6-DImp$tn_BFI8
    DImp$tn_BFI18_Rev <- 6-DImp$tn_BFI18
    DImp$tn_BFI23_Rev <- 6-DImp$tn_BFI23
    DImp$tn_BFI43_Rev <- 6-DImp$tn_BFI43
    DImp$tn_BFI9_Rev <-  6-DImp$tn_BFI9
    DImp$tn_BFI24_Rev <- 6-DImp$tn_BFI24
    DImp$tn_BFI34_Rev <- 6-DImp$tn_BFI34
    DImp$tn_BFI35_Rev <- 6-DImp$tn_BFI35
    DImp$tn_BFI41_Rev <- 6-DImp$tn_BFI41
    
    assign ("DImp",DImp,envir = .GlobalEnv)
  }
  
  CreateReverseItems_allData_age13(D13Imp1)   #if you want to check another imputed dataset, enter it instead of D13Imp1
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

#changing the names of age 11 and age 13 to be the same
  newnames <- gsub(x=colnames(D13Imp1.1[,relvar_emotional_13]),pattern="tn_", replacement="")
  colnames(D13Imp1.1)[relvar_emotional_13] <- newnames

#dividing the 6 folds so two twins from the same family will always be in the same test fold
#gfold is the variable allocating family to a specific fold
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
  ifams_13$ID<- c(rep(1,times=nrow(unique_ifams_13)),rep(4,times=nrow(unique_ifams_13)))  # allocate ID for each twin (ID 1=older twin, ID 4= younger twin) 

  D13Imp1.1<- merge(D13Imp1.1,ifams_13, by=c("ifam","ID"), all.x = T, all.y = F)          # merge the gfold var with the data

  
#############################################################################################
######################### Ridge regression - Age 13 #########################################
#############################################################################################  

#scale all the BFI items so they all will have mean=0 and SD=1
  for (i in 2:45) {D13Imp1.1[,relvar_emotional_13[i]] <-
       scale(D13Imp1.1[,relvar_emotional_13[i]], scale=T)}

#emotional empathy

#doing Ridge regression on the folds
#fold 1
  Ridge(DImp=D13Imp1.1,gfold=1,relvar=relvar_emotional_13) 
  fit_emo13_1 <- fit
  opt_lambda_emo13_1 <- opt_lambda
  opt_coef_emo13_1 <- opt_coef
  y_pred_emo13_1 <- y_pred
  mse_emo13_1 <- mse

#fold 2
  Ridge(DImp=D13Imp1.1,gfold=2,relvar=relvar_emotional_13) 
  fit_emo13_2 <- fit
  opt_lambda_emo13_2 <- opt_lambda
  opt_coef_emo13_2 <- opt_coef
  y_pred_emo13_2 <- y_pred
  mse_emo13_2 <- mse

#fold 3
  Ridge(DImp=D13Imp1.1,gfold=3,relvar=relvar_emotional_13) 
  fit_emo13_3 <- fit
  opt_lambda_emo13_3 <- opt_lambda
  opt_coef_emo13_3 <- opt_coef
  y_pred_emo13_3 <- y_pred
  mse_emo13_3 <- mse

#fold 4
  Ridge(DImp=D13Imp1.1,gfold=4,relvar=relvar_emotional_13) 
  fit_emo13_4 <- fit
  opt_lambda_emo13_4 <- opt_lambda
  opt_coef_emo13_4 <- opt_coef
  y_pred_emo13_4 <- y_pred
  mse_emo13_4 <- mse

#fold 5
  Ridge(DImp=D13Imp1.1,gfold=5,relvar=relvar_emotional_13) 
  fit_emo13_5 <- fit
  opt_lambda_emo13_5 <- opt_lambda
  opt_coef_emo13_5 <- opt_coef
  y_pred_emo13_5 <- y_pred
  mse_emo13_5 <- mse

#fold 6
  Ridge(DImp=D13Imp1.1,gfold=6,relvar=relvar_emotional_13) 
  fit_emo13_6 <- fit
  opt_lambda_emo13_6 <- opt_lambda
  opt_coef_emo13_6 <- opt_coef
  y_pred_emo13_6 <- y_pred
  mse_emo13_6 <- mse

#computing the mean coefficients across the folds
  opt_coef_emo13_matrix <- as.data.frame(cbind (opt_coef_emo13_1,
                                                opt_coef_emo13_2,
                                                opt_coef_emo13_3,
                                                opt_coef_emo13_4,
                                                opt_coef_emo13_5,
                                                opt_coef_emo13_6))
  opt_coef_emo13_matrix$aveCoef <- rowMeans(opt_coef_emo13_matrix)

#computing the mean correlation between outcome and predicted value across the folds
  cor_emo13 <-1:6
  for (i in 1:6) {
     cor_emo13[i] <- cor.test(D13Imp1.1$EMPQ_emotional[D13Imp1.1$gfold ==i],
                              eval(parse(text=paste0("y_pred_emo13_",i))))[4]}
  
  cor_emo13 <- as.numeric(cor_emo13)
  avecor_emo13 <- mean(cor_emo13)  #mean correlation
  aveR2_emo13  <- avecor_emo13^2   #mean R2
  
#computing the mean mse across the folds
  mse_emo13 <-1:6
  for (i in 1:6) { mse_emo13[i] <- eval(parse(text=paste0("mse_emo13_",i)))}
  avemse_emo13 <- mean(mse_emo13)

#check the correlation between age 11 and age 13 average items' coefficients
#this indicates on age consistency in terms of specific nuances’ ability to predict empathy
  cor.test(opt_coef_emo11_matrix$aveCoef,opt_coef_emo13_matrix$aveCoef)
  
  
  
#cognitive empathy
  
#doing Ridge regression on the folds
#fold 1
  Ridge(DImp=D13Imp1.1,gfold=1,relvar=relvar_cognitive_13) 
  fit_cog13_1 <- fit
  opt_lambda_cog13_1 <- opt_lambda
  opt_coef_cog13_1 <- opt_coef
  y_pred_cog13_1 <- y_pred
  mse_cog13_1 <- mse
  
#fold 2
  Ridge(DImp=D13Imp1.1,gfold=2,relvar=relvar_cognitive_13) 
  fit_cog13_2 <- fit
  opt_lambda_cog13_2 <- opt_lambda
  opt_coef_cog13_2 <- opt_coef
  y_pred_cog13_2 <- y_pred
  mse_cog13_2 <- mse
  
#fold 3
  Ridge(DImp=D13Imp1.1,gfold=3,relvar=relvar_cognitive_13) 
  fit_cog13_3 <- fit
  opt_lambda_cog13_3 <- opt_lambda
  opt_coef_cog13_3 <- opt_coef
  y_pred_cog13_3 <- y_pred
  mse_cog13_3 <- mse
  
#fold 4
  Ridge(DImp=D13Imp1.1,gfold=4,relvar=relvar_cognitive_13) 
  fit_cog13_4 <- fit
  opt_lambda_cog13_4 <- opt_lambda
  opt_coef_cog13_4 <- opt_coef
  y_pred_cog13_4 <- y_pred
  mse_cog13_4 <- mse
  
#fold 5
  Ridge(DImp=D13Imp1.1,gfold=5,relvar=relvar_cognitive_13) 
  fit_cog13_5 <- fit
  opt_lambda_cog13_5 <- opt_lambda
  opt_coef_cog13_5 <- opt_coef
  y_pred_cog13_5 <- y_pred
  mse_cog13_5 <- mse
  
#fold 6
  Ridge(DImp=D13Imp1.1,gfold=6,relvar=relvar_cognitive_13) 
  fit_cog13_6 <- fit
  opt_lambda_cog13_6 <- opt_lambda
  opt_coef_cog13_6 <- opt_coef
  y_pred_cog13_6 <- y_pred
  mse_cog13_6 <- mse
  
#computing the mean coefficients across the folds
  opt_coef_cog13_matrix <- as.data.frame(cbind (opt_coef_cog13_1,
                                                opt_coef_cog13_2,
                                                opt_coef_cog13_3,
                                                opt_coef_cog13_4,
                                                opt_coef_cog13_5,
                                                opt_coef_cog13_6))
  opt_coef_cog13_matrix$aveCoef <- rowMeans(opt_coef_cog13_matrix)
  
#computing the mean correlation between outcome and predicted value across the folds
  cor_cog13 <-1:6
  for (i in 1:6) {
    cor_cog13[i] <- cor.test(D13Imp1.1$EMPQ_cognitive[D13Imp1.1$gfold ==i],
                             eval(parse(text=paste0("y_pred_cog13_",i))))[4]}
  
  cor_cog13 <- as.numeric(cor_cog13)
  avecor_cog13 <- mean(cor_cog13)  #mean correlation
  aveR2_cog13  <- avecor_cog13^2     #mean R2
  
#computing the mean mse across the folds
  mse_cog13 <-1:6
  for (i in 1:6) { mse_cog13[i] <- eval(parse(text=paste0("mse_cog13_",i)))}
  avemse_cog13 <- mean(mse_cog13)
  
  
#check the correlation between age 11 and age 13 average items' coefficients
#this indicates on age consistency in terms of specific nuances’ ability to predict empathy
  cor.test(opt_coef_cog11_matrix$aveCoef,opt_coef_cog13_matrix$aveCoef)
  
  

  
#############################################################################################
################## Specific items' coefficients visualization - Age 13 ######################
#############################################################################################
  
#emotional empathy
  
#grouping according to the original scales
  opt_coef_emo13_matrix$category <- NA

#EXTRAVERSION
  for (i in c(1,11,16,26,36)){
   opt_coef_emo13_matrix$category[rownames(opt_coef_emo13_matrix)==paste0("BFI",i)] <- "E"}
#reverse items
  for (i in c(6,21,31)){
   opt_coef_emo13_matrix$category[rownames(opt_coef_emo13_matrix)==paste0("BFI",i,"_Rev")] <- "E"}

#AGREABELNESS 
  for (i in c(7,17,22,32,42)){
   opt_coef_emo13_matrix$category[rownames(opt_coef_emo13_matrix)==paste0("BFI",i)] <- "A"}
#reverse items
  for (i in c(2,12,27,37)){
   opt_coef_emo13_matrix$category[rownames(opt_coef_emo13_matrix)==paste0("BFI",i,"_Rev")] <- "A"}

#OPENESS
  for (i in c(5,10,15,20,25,30,40,44)){
   opt_coef_emo13_matrix$category[rownames(opt_coef_emo13_matrix)==paste0("BFI",i)] <- "O"}
#reverse items
  for (i in c(35,41)){
   opt_coef_emo13_matrix$category[rownames(opt_coef_emo13_matrix)==paste0("BFI",i,"_Rev")]<- "O"}

#CONCIENCIOUSNESS
  for (i in c(3,13,28,33,38)){
   opt_coef_emo13_matrix$category[rownames(opt_coef_emo13_matrix)==paste0("BFI",i)] <- "C"}
#reverse items
  for (i in c(8,18,23,43)){
   opt_coef_emo13_matrix$category[rownames(opt_coef_emo13_matrix)==paste0("BFI",i,"_Rev")]<- "C"}

#NEUROTICISM
  for (i in c(4,14,19,29,39)){
   opt_coef_emo13_matrix$category[rownames(opt_coef_emo13_matrix)==paste0("BFI",i)] <- "N"}
#reverse items
  for (i in c(9,24,34)){
   opt_coef_emo13_matrix$category[rownames(opt_coef_emo13_matrix)==paste0("BFI",i,"_Rev")]<-"N"}

  opt_coef_emo13_matrix$item <- rownames(opt_coef_emo13_matrix)
  opt_coef_emo13_matrix <- cbind(opt_coef_emo13_matrix, BFI_labels)

  plotemo13 <- ggplot(data= opt_coef_emo13_matrix,
                      aes(reorder(x=opt_coef_emo13_matrix$label,opt_coef_emo13_matrix$aveCoef),
                      y=opt_coef_emo13_matrix$aveCoef, 
                      fill=opt_coef_emo13_matrix$category))+
                      geom_bar(stat="identity")+ coord_flip()+ 
                      ggtitle("Regression coefficients of  personality indicators")+
                      labs(x="Personality indicator", y="Regression coefficient", fill="Big5 scale")+
                      scale_fill_manual(values=c("#F8A6F8", "#F7563B", "#F59D3D", "#433FF3", "#5FD3D3"))+
                      theme_bw()+ theme(legend.position=c(0.95, 0.15), legend.title = element_text(size=12),
                      plot.title=element_text(size=16, face="bold", family="serif",hjust = 0.5),
                      axis.title =element_text(size=12, face="bold", family="serif"),
                      text=element_text(family="serif"))



#cognitive empathy

#allocate items to their original Big-Five domains
  opt_coef_cog13_matrix$category <- NA

#EXTRAVERSION
  for (i in c(1,11,16,26,36)){
   opt_coef_cog13_matrix$category[rownames(opt_coef_cog13_matrix)==paste0("BFI",i)] <- "E"}
#reverse items
  for (i in c(6,21,31)){
   opt_coef_cog13_matrix$category[rownames(opt_coef_cog13_matrix)==paste0("BFI",i,"_Rev")] <- "E"}

#AGREABELNESS
  for (i in c(7,17,22,32,42)){
   opt_coef_cog13_matrix$category[rownames(opt_coef_cog13_matrix)==paste0("BFI",i)] <- "A"}
#reverse items
  for (i in c(2,12,27,37)){
   opt_coef_cog13_matrix$category[rownames(opt_coef_cog13_matrix)==paste0("BFI",i,"_Rev")] <- "A"}

#OPENESS
  for (i in c(5,10,15,20,25,30,40,44)){
   opt_coef_cog13_matrix$category[rownames(opt_coef_cog13_matrix)==paste0("BFI",i)] <- "O"}
#reverse items
  for (i in c(35,41)){
   opt_coef_cog13_matrix$category[rownames(opt_coef_cog13_matrix)==paste0("BFI",i,"_Rev")]<- "O"}

#CONCIENCIOUSNESS
  for (i in c(3,13,28,33,38)){
   opt_coef_cog13_matrix$category[rownames(opt_coef_cog13_matrix)==paste0("BFI",i)] <- "C"}
#reverse items
  for (i in c(8,18,23,43)){
   opt_coef_cog13_matrix$category[rownames(opt_coef_cog13_matrix)==paste0("BFI",i,"_Rev")]<- "C"}

#NEUROTICISM
  for (i in c(4,14,19,29,39)){
   opt_coef_cog13_matrix$category[rownames(opt_coef_cog13_matrix)==paste0("BFI",i)] <- "N"}
#reverse items
  for (i in c(9,24,34)){
   opt_coef_cog13_matrix$category[rownames(opt_coef_cog13_matrix)==paste0("BFI",i,"_Rev")]<-"N"}

  opt_coef_cog13_matrix$item <- rownames(opt_coef_cog13_matrix)
  opt_coef_cog13_matrix <- cbind(opt_coef_cog13_matrix, BFI_labels)

  plotcog13 <- ggplot(data= opt_coef_cog13_matrix,
                      aes(reorder(x=opt_coef_cog13_matrix$label,opt_coef_cog13_matrix$aveCoef),
                      y=opt_coef_cog13_matrix$aveCoef, 
                      fill=opt_coef_cog13_matrix$category))+
                      geom_bar(stat="identity")+ coord_flip()+ 
                      ggtitle("Regression coefficients of  personality indicators")+
                      labs(x="Personality indicator", y="Regression coefficient", fill="Big5 scale")+
                      scale_fill_manual(values=c("#F8A6F8", "#F7563B", "#F59D3D", "#433FF3", "#5FD3D3"))+
                      theme_bw()+ theme(legend.position=c(0.95, 0.15), legend.title = element_text(size=12),
                      plot.title=element_text(size=16, face="bold", family="serif",hjust = 0.5),
                      axis.title =element_text(size=12, face="bold", family="serif"),
                      text=element_text(family="serif"))


#############################################################################################
##################### Ridge regression - Training age 11 and testing Age 13 #################
#############################################################################################  

#creating folds in the combined 11_13 dataset
#gfold1113 indicates the twin pair's fold in this analysis
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
  

#adapting the Ridge function to the age 11-age 13 analysis
  Ridge1113 <- function (DImp11,DImp13, gfold,relvar) {
     #find the best lambda- 
     set.seed(10000)
     #train set comes from age 11
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
     
     #now check the prediction on the test set at age 13
     y_pred <- predict(fit, s=opt_lambda, 
                       newx = as.matrix(DImp13[DImp13$gfold1113 == gfold,relvar[2:45]]))
     
     mse <- mean((DImp13[DImp13$gfold1113 == gfold,relvar[1]]-y_pred)^2)
     
     assign ("fit",fit,envir = .GlobalEnv)
     assign ("opt_lambda",opt_lambda,envir = .GlobalEnv)
     assign ("opt_coef",opt_coef,envir = .GlobalEnv)
     assign ("y_pred",y_pred,envir = .GlobalEnv)
     assign ("mse",mse,envir = .GlobalEnv)
  }
  

#emotional empathy
  
#doing Ridge regression on the folds 
  
#fold 1
  Ridge1113(DImp11=DImp1.1,DImp13=D13Imp1.1, gfold=1,
            relvar=relvar_emotional) 
  fit_emo_1 <- fit
  opt_lambda_emo_1 <- opt_lambda
  opt_coef_emo_1 <- opt_coef
  y_pred_emo_1 <- y_pred
  mse_emo_1 <- mse

#fold 2
  Ridge1113(DImp11=DImp1.1,DImp13=D13Imp1.1, gfold=2,
            relvar=relvar_emotional) 
  fit_emo_2 <- fit
  opt_lambda_emo_2 <- opt_lambda
  opt_coef_emo_2 <- opt_coef
  y_pred_emo_2 <- y_pred
  mse_emo_2 <- mse

#fold 3
  Ridge1113(DImp11=DImp1.1,DImp13=D13Imp1.1, gfold=3,
          relvar=relvar_emotional) 
  fit_emo_3 <- fit
  opt_lambda_emo_3 <- opt_lambda
  opt_coef_emo_3 <- opt_coef
  y_pred_emo_3 <- y_pred
  mse_emo_3 <- mse

#fold 4
  Ridge1113(DImp11=DImp1.1,DImp13=D13Imp1.1, gfold=4,
            relvar=relvar_emotional) 
  fit_emo_4 <- fit
  opt_lambda_emo_4 <- opt_lambda
  opt_coef_emo_4 <- opt_coef
  y_pred_emo_4 <- y_pred
  mse_emo_4 <- mse

#fold 5
  Ridge1113(DImp11=DImp1.1,DImp13=D13Imp1.1, gfold=5,
          relvar=relvar_emotional) 
  fit_emo_5 <- fit
  opt_lambda_emo_5 <- opt_lambda
  opt_coef_emo_5 <- opt_coef
  y_pred_emo_5 <- y_pred
  mse_emo_5 <- mse

#fold 6
  Ridge1113(DImp11=DImp1.1,DImp13=D13Imp1.1, gfold=6,
          relvar=relvar_emotional) 
  fit_emo_6 <- fit
  opt_lambda_emo_6 <- opt_lambda
  opt_coef_emo_6 <- opt_coef
  y_pred_emo_6 <- y_pred
  mse_emo_6 <- mse

#computing the mean coefficients across the folds
  opt_coef_emo_matrix <- as.data.frame(cbind (opt_coef_emo_1,
                                              opt_coef_emo_2,
                                              opt_coef_emo_3,
                                              opt_coef_emo_4,
                                              opt_coef_emo_5,
                                              opt_coef_emo_6))
  opt_coef_emo_matrix$aveCoef <- rowMeans(opt_coef_emo_matrix)

#finding the mean correlation between the outcome and the predicted value across the folds
  cor_emo <-1:6
  for (i in 1:6) {
     cor_emo[i] <- cor.test(D13Imp1.1$EMPQ_emotional[D13Imp1.1$gfold1113 ==i],
                            eval(parse(text=paste0("y_pred_emo_",i))))[4]}
  
  cor_emo <- as.numeric(cor_emo)
  avecor_emo <- mean(cor_emo)   #mean correlation
  aveR2_emo  <- avecor_emo^2    #mean R2

#computing the mean mse across the folds
  mse_emo <-1:6
  for (i in 1:6) { mse_emo[i] <- eval(parse(text=paste0("mse_emo_",i)))}
  avemse_emo <- mean(mse_emo)


#cognitive empathy
  
#doing Ridge regression on the folds 

#fold 1
  Ridge1113(DImp11=DImp1.1,DImp13=D13Imp1.1, gfold=1,
            relvar=relvar_cognitive) 
  fit_cog_1 <- fit
  opt_lambda_cog_1 <- opt_lambda
  opt_coef_cog_1 <- opt_coef
  y_pred_cog_1 <- y_pred
  mse_cog_1 <- mse

#fold 2
  Ridge1113(DImp11=DImp1.1,DImp13=D13Imp1.1, gfold=2,
            relvar=relvar_cognitive) 
  fit_cog_2 <- fit
  opt_lambda_cog_2 <- opt_lambda
  opt_coef_cog_2 <- opt_coef
  y_pred_cog_2 <- y_pred
  mse_cog_2 <- mse

#fold 3
  Ridge1113(DImp11=DImp1.1,DImp13=D13Imp1.1, gfold=3,
            relvar=relvar_cognitive) 
  fit_cog_3 <- fit
  opt_lambda_cog_3 <- opt_lambda
  opt_coef_cog_3 <- opt_coef
  y_pred_cog_3 <- y_pred
  mse_cog_3 <- mse

#fold 4
  Ridge1113(DImp11=DImp1.1,DImp13=D13Imp1.1, gfold=4,
            relvar=relvar_cognitive) 
  fit_cog_4 <- fit
  opt_lambda_cog_4 <- opt_lambda
  opt_coef_cog_4 <- opt_coef
  y_pred_cog_4 <- y_pred
  mse_cog_4 <- mse

#fold 5
  Ridge1113(DImp11=DImp1.1,DImp13=D13Imp1.1, gfold=5,
          relvar=relvar_cognitive) 
  fit_cog_5 <- fit
  opt_lambda_cog_5 <- opt_lambda
  opt_coef_cog_5 <- opt_coef
  y_pred_cog_5 <- y_pred
  mse_cog_5 <- mse

#fold 6
  Ridge1113(DImp11=DImp1.1,DImp13=D13Imp1.1, gfold=6,
          relvar=relvar_cognitive) 
  fit_cog_6 <- fit
  opt_lambda_cog_6 <- opt_lambda
  opt_coef_cog_6 <- opt_coef
  y_pred_cog_6 <- y_pred
  mse_cog_6 <- mse

#computing the mean coefficients across the folds
  opt_coef_cog_matrix <- as.data.frame(cbind (opt_coef_cog_1,
                                              opt_coef_cog_2,
                                              opt_coef_cog_3,
                                              opt_coef_cog_4,
                                              opt_coef_cog_5,
                                              opt_coef_cog_6))
  opt_coef_cog_matrix$aveCoef <- rowMeans(opt_coef_cog_matrix)


#finding the mean correlation between outcome and predicted value across the folds
  cor_cog <-1:6
  for (i in 1:6) {
     cor_cog[i] <- cor.test(D13Imp1.1$EMPQ_cognitive[D13Imp1.1$gfold1113 ==i],
                            eval(parse(text=paste0("y_pred_cog_",i))))[4]}
  
     cor_cog <- as.numeric(cor_cog)
     avecor_cog <- mean(cor_cog)   #mean correlation 
     aveR2_cog  <- avecor_cog^2    #mean R2

#computing the mean mse across the folds
  mse_cog <-1:6
  for (i in 1:6) { mse_cog[i] <- eval(parse(text=paste0("mse_cog_",i)))}
  avemse_cog <- mean(mse_cog)

  

#what is the range of optimal lambda values across all age analyses 
#(reported in the Appendix 2)
  min(opt_lambda_cog11_1,opt_lambda_cog11_2,opt_lambda_cog11_3,opt_lambda_cog11_4,opt_lambda_cog11_5,opt_lambda_cog11_6,
      opt_lambda_cog13_1,opt_lambda_cog13_2,opt_lambda_cog13_3,opt_lambda_cog13_4,opt_lambda_cog13_5,opt_lambda_cog13_6,
      opt_lambda_emo11_1,opt_lambda_emo11_2,opt_lambda_emo11_3,opt_lambda_emo11_4,opt_lambda_emo11_5,opt_lambda_emo11_6,
      opt_lambda_emo13_1,opt_lambda_emo13_2,opt_lambda_emo13_3,opt_lambda_emo13_4,opt_lambda_emo13_5,opt_lambda_emo13_6,
      opt_lambda_cog_1,opt_lambda_cog_2,opt_lambda_cog_3,opt_lambda_cog_4,opt_lambda_cog_5,opt_lambda_cog_6,
      opt_lambda_emo_1,opt_lambda_emo_2,opt_lambda_emo_3,opt_lambda_emo_4,opt_lambda_emo_5,opt_lambda_emo_6)
  
  max(opt_lambda_cog11_1,opt_lambda_cog11_2,opt_lambda_cog11_3,opt_lambda_cog11_4,opt_lambda_cog11_5,opt_lambda_cog11_6,
      opt_lambda_cog13_1,opt_lambda_cog13_2,opt_lambda_cog13_3,opt_lambda_cog13_4,opt_lambda_cog13_5,opt_lambda_cog13_6,
      opt_lambda_emo11_1,opt_lambda_emo11_2,opt_lambda_emo11_3,opt_lambda_emo11_4,opt_lambda_emo11_5,opt_lambda_emo11_6,
      opt_lambda_emo13_1,opt_lambda_emo13_2,opt_lambda_emo13_3,opt_lambda_emo13_4,opt_lambda_emo13_5,opt_lambda_emo13_6,
      opt_lambda_cog_1,opt_lambda_cog_2,opt_lambda_cog_3,opt_lambda_cog_4,opt_lambda_cog_5,opt_lambda_cog_6,
      opt_lambda_emo_1,opt_lambda_emo_2,opt_lambda_emo_3,opt_lambda_emo_4,opt_lambda_emo_5,opt_lambda_emo_6)
  

############################################################################################
####### Building the new predictors on the entire dataset for further analyses #############
####### (i.e., no test set) ################################################################
############################################################################################

#age 11
#emotional empathy
  set.seed(10000)
  cv_fit_emo11 <- cv.glmnet(x=as.matrix(DImp1.1[,relvar_emotional[2:45]]),
                            y=DImp1.1[,relvar_emotional[1]],
                            alpha=0, lambda=NULL, nfolds=10)
  opt_lambda_emo11 <- cv_fit_emo11$lambda.min
  opt_lambda_ind_emo11 <- which(cv_fit_emo11$lambda==opt_lambda_emo11)

#what are the coefficients when the lambda is optimal
  opt_coef_emo11_all <- as.matrix(cv_fit_emo11$glmnet.fit$beta[,opt_lambda_ind_emo11]) 

#after finding the best lambda, train the entire set (train and test sets are the same)
  fit_emo11_all <- glmnet(x=as.matrix(DImp1.1[,relvar_emotional[2:45]]), 
                          y=DImp1.1[,relvar_emotional[1]], 
                          alpha = 0, lambda = opt_lambda_emo11)

#now check the prediction (notice that the training and testing sets are the same,
#so this is not exactly "prediction")
  y_pred_emo11_all <- as.numeric(predict(fit_emo11_all, s=opt_lambda_emo11, 
                                 newx = as.matrix(DImp1.1[,relvar_emotional[2:45]])))

  cor_emo11_all <- as.numeric (cor.test(DImp1.1$EMPQ_emotional, y_pred_emo11_all)[4])
  R2_emo11_all  <- cor_emo11_all^2
  mse_emo11_all <- mean((DImp1.1[,relvar_emotional[1]]-y_pred_emo11_all)^2)

#attaching the predicted values to the dataset
  DImp1.1$predicted_emotional_11 <- y_pred_emo11_all

#age 11
#cognitive empathy
  set.seed(10000)
  cv_fit_cog11 <- cv.glmnet(x=as.matrix(DImp1.1[,relvar_cognitive[2:45]]),
                            y=DImp1.1[,relvar_cognitive[1]],
                            alpha=0, lambda=NULL, nfolds=10)
  opt_lambda_cog11 <- cv_fit_cog11$lambda.min
  opt_lambda_ind_cog11 <- which(cv_fit_cog11$lambda==opt_lambda_cog11)

#what are the coefficients when the lambda is optimal
  opt_coef_cog11_all <- as.matrix(cv_fit_cog11$glmnet.fit$beta[,opt_lambda_ind_cog11]) 

#after finding the best lambda, train the entire set (train and test sets are the same)
  fit_cog11_all <- glmnet(x=as.matrix(DImp1.1[,relvar_cognitive[2:45]]), 
                        y=DImp1.1[,relvar_cognitive[1]], 
                        alpha = 0, lambda = opt_lambda_cog11)

#now check the prediction (notice that the training and testing sets are the same,
#so this is not exactly "prediction")
  y_pred_cog11_all <- as.numeric(predict(fit_cog11_all, s=opt_lambda_cog11, 
                            newx = as.matrix(DImp1.1[,relvar_cognitive[2:45]])))

  cor_cog11_all <- as.numeric (cor.test(DImp1.1$EMPQ_cognitive, y_pred_cog11_all)[4])
  R2_cog11_all  <- cor_cog11_all^2
  mse_cog11_all <- mean((DImp1.1[,relvar_cognitive[1]]-y_pred_cog11_all)^2)

#attaching the predicted values to the dataset
  DImp1.1$predicted_cognitive_11 <- y_pred_cog11_all

#age 13
#emotional empathy
  set.seed(10000)
  cv_fit_emo13 <- cv.glmnet(x=as.matrix(D13Imp1.1[,relvar_emotional_13[2:45]]),
                            y=D13Imp1.1[,relvar_emotional_13[1]],
                            alpha=0, lambda=NULL, nfolds=10)
  opt_lambda_emo13 <- cv_fit_emo13$lambda.min
  opt_lambda_ind_emo13 <- which(cv_fit_emo13$lambda==opt_lambda_emo13)

#what are the coefficients when the lambda is optimal
  opt_coef_emo13_all <- as.matrix(cv_fit_emo13$glmnet.fit$beta[,opt_lambda_ind_emo13]) 

#after finding the best lambda, train the entire set (train and test sets are the same)
  fit_emo13_all <- glmnet(x=as.matrix(D13Imp1.1[,relvar_emotional_13[2:45]]), 
                        y=D13Imp1.1[,relvar_emotional_13[1]], 
                        alpha = 0, lambda = opt_lambda_emo13)

#now check the prediction (notice that the training and testing sets are the same,
  #so this is not exactly "prediction")
  y_pred_emo13_all <- as.numeric(predict(fit_emo13_all, s=opt_lambda_emo13, 
                                 newx = as.matrix(D13Imp1.1[,relvar_emotional_13[2:45]])))

  cor_emo13_all <- as.numeric (cor.test(D13Imp1.1$EMPQ_emotional, y_pred_emo13_all)[4])
  R2_emo13_all  <- cor_emo13_all^2
  mse_emo13_all <- mean((D13Imp1.1[,relvar_emotional_13[1]]-y_pred_emo13_all)^2)
  
#attaching the predicted values to the dataset
  D13Imp1.1$predicted_emotional_13 <- y_pred_emo13_all

  
#cognitive empathy
  set.seed(10000)
  cv_fit_cog13 <- cv.glmnet(x=as.matrix(D13Imp1.1[,relvar_cognitive_13[2:45]]),
                            y=D13Imp1.1[,relvar_cognitive_13[1]],
                            alpha=0, lambda=NULL, nfolds=10)
  opt_lambda_cog13 <- cv_fit_cog13$lambda.min
  opt_lambda_ind_cog13 <- which(cv_fit_cog13$lambda==opt_lambda_cog13)

#what are the coefficients when the lambda is optimal
  opt_coef_cog13_all <- as.matrix(cv_fit_cog13$glmnet.fit$beta[,opt_lambda_ind_cog13]) 

#after finding the best lambda, train the entire set (train and test sets are the same)
  fit_cog13_all <- glmnet(x=as.matrix(D13Imp1.1[,relvar_cognitive_13[2:45]]), 
                        y=D13Imp1.1[,relvar_cognitive_13[1]], 
                        alpha = 0, lambda = opt_lambda_cog13)

#now check the prediction (notice that because the training and testing sets are the same 
#this is not exactly "prediction")
  y_pred_cog13_all <- as.numeric(predict(fit_cog13_all, s=opt_lambda_cog13, 
                                 newx = as.matrix(D13Imp1.1[,relvar_cognitive_13[2:45]])))

  cor_cog13_all <- as.numeric (cor.test(D13Imp1.1$EMPQ_cognitive, y_pred_cog13_all)[4])
  R2_cog13_all  <- cor_cog13_all^2
  mse_cog13_all <- mean((D13Imp1.1[,relvar_cognitive_13[1]]-y_pred_cog13_all)^2)
  
  
#attaching the predicted values to the dataset
  D13Imp1.1$predicted_cognitive_13 <- y_pred_cog13_all

  
#Examining the relation between the entire data analysis and the cross-validation analysis
#(reported in the supplementary materials)

#relation between items coefficients
  cor.test(opt_coef_emo11_all,opt_coef_emo11_matrix$aveCoef)
  cor.test(opt_coef_cog11_all,opt_coef_cog11_matrix$aveCoef)
  cor.test(opt_coef_emo13_all,opt_coef_emo13_matrix$aveCoef)
  cor.test(opt_coef_cog13_all,opt_coef_cog13_matrix$aveCoef)
#all > .99

#relation between predictive scores- emotional empathy age 11
#organize all the predicted values from the cross-validation analysis into one variable
  y_pred_emo11_1_mat <- cbind(DImp1.1[DImp1.1$gfold==1,1:2], y_pred_emo11_1)
  y_pred_emo11_2_mat <- cbind(DImp1.1[DImp1.1$gfold==2,1:2], y_pred_emo11_2)
  y_pred_emo11_3_mat <- cbind(DImp1.1[DImp1.1$gfold==3,1:2], y_pred_emo11_3)
  y_pred_emo11_4_mat <- cbind(DImp1.1[DImp1.1$gfold==4,1:2], y_pred_emo11_4)
  y_pred_emo11_5_mat <- cbind(DImp1.1[DImp1.1$gfold==5,1:2], y_pred_emo11_5)
  y_pred_emo11_6_mat <- cbind(DImp1.1[DImp1.1$gfold==6,1:2], y_pred_emo11_6)

  y_pred_emo11_mat <- rbind(y_pred_emo11_1_mat,
                            y_pred_emo11_2_mat,
                            y_pred_emo11_3_mat,
                            y_pred_emo11_4_mat,
                            y_pred_emo11_5_mat,
                            y_pred_emo11_6_mat)
  
  colnames(y_pred_emo11_mat)[3] <- "predictedKfold"
  y_pred_emo11_mat <- merge(y_pred_emo11_mat, 
                            DImp1.1[,c(1:2, which(colnames(DImp1.1)=="predicted_emotional_11"))],
                            all.x=T, all.y=T)
  #now check the correlation
  cor.test(y_pred_emo11_mat$predictedKfold,y_pred_emo11_mat$predicted_emotional_11)


#relation between predictive scores- cognitive empathy age 11
#organize all the predicted values from the cross-validation analysis into one variable
  y_pred_cog11_1_mat <- cbind(DImp1.1[DImp1.1$gfold==1,1:2], y_pred_cog11_1)
  y_pred_cog11_2_mat <- cbind(DImp1.1[DImp1.1$gfold==2,1:2], y_pred_cog11_2)
  y_pred_cog11_3_mat <- cbind(DImp1.1[DImp1.1$gfold==3,1:2], y_pred_cog11_3)
  y_pred_cog11_4_mat <- cbind(DImp1.1[DImp1.1$gfold==4,1:2], y_pred_cog11_4)
  y_pred_cog11_5_mat <- cbind(DImp1.1[DImp1.1$gfold==5,1:2], y_pred_cog11_5)
  y_pred_cog11_6_mat <- cbind(DImp1.1[DImp1.1$gfold==6,1:2], y_pred_cog11_6)

  y_pred_cog11_mat <- rbind(y_pred_cog11_1_mat,
                            y_pred_cog11_2_mat,
                            y_pred_cog11_3_mat,
                            y_pred_cog11_4_mat,
                            y_pred_cog11_5_mat,
                            y_pred_cog11_6_mat)
  colnames(y_pred_cog11_mat)[3] <- "predictedKfold"
  y_pred_cog11_mat <- merge(y_pred_cog11_mat, 
                            DImp1.1[,c(1:2, which(colnames(DImp1.1)=="predicted_cognitive_11"))],
                            all.x=T, all.y=T)
  #now check the correlation
  cor.test(y_pred_cog11_mat$predictedKfold,y_pred_cog11_mat$predicted_cognitive_11)


#relation between predictive scores- emotional empathy age 13
#organize all the predicted values from the cross-validation analysis into one variable
  y_pred_emo13_1_mat <- cbind(D13Imp1.1[D13Imp1.1$gfold==1,1:2], y_pred_emo13_1)
  y_pred_emo13_2_mat <- cbind(D13Imp1.1[D13Imp1.1$gfold==2,1:2], y_pred_emo13_2)
  y_pred_emo13_3_mat <- cbind(D13Imp1.1[D13Imp1.1$gfold==3,1:2], y_pred_emo13_3)
  y_pred_emo13_4_mat <- cbind(D13Imp1.1[D13Imp1.1$gfold==4,1:2], y_pred_emo13_4)
  y_pred_emo13_5_mat <- cbind(D13Imp1.1[D13Imp1.1$gfold==5,1:2], y_pred_emo13_5)
  y_pred_emo13_6_mat <- cbind(D13Imp1.1[D13Imp1.1$gfold==6,1:2], y_pred_emo13_6)

  y_pred_emo13_mat <- rbind(y_pred_emo13_1_mat,
                            y_pred_emo13_2_mat,
                            y_pred_emo13_3_mat,
                            y_pred_emo13_4_mat,
                            y_pred_emo13_5_mat,
                            y_pred_emo13_6_mat)
  colnames(y_pred_emo13_mat)[3] <- "predictedKfold"
  y_pred_emo13_mat <- merge(y_pred_emo13_mat, 
                            D13Imp1.1[,c(1:2, which(colnames(D13Imp1.1)=="predicted_emotional_13"))],
                            all.x=T, all.y=T)
  #now check the correlation
  cor.test(y_pred_emo13_mat$predictedKfold,y_pred_emo13_mat$predicted_emotional_13)


#relation between predictive scores- cognitive empathy age 13
#organize all the predicted values from the cross-validation analysis into one variable
  y_pred_cog13_1_mat <- cbind(D13Imp1.1[D13Imp1.1$gfold==1,1:2], y_pred_cog13_1)
  y_pred_cog13_2_mat <- cbind(D13Imp1.1[D13Imp1.1$gfold==2,1:2], y_pred_cog13_2)
  y_pred_cog13_3_mat <- cbind(D13Imp1.1[D13Imp1.1$gfold==3,1:2], y_pred_cog13_3)
  y_pred_cog13_4_mat <- cbind(D13Imp1.1[D13Imp1.1$gfold==4,1:2], y_pred_cog13_4)
  y_pred_cog13_5_mat <- cbind(D13Imp1.1[D13Imp1.1$gfold==5,1:2], y_pred_cog13_5)
  y_pred_cog13_6_mat <- cbind(D13Imp1.1[D13Imp1.1$gfold==6,1:2], y_pred_cog13_6)

  y_pred_cog13_mat <- rbind(y_pred_cog13_1_mat,
                            y_pred_cog13_2_mat,
                            y_pred_cog13_3_mat,
                            y_pred_cog13_4_mat,
                            y_pred_cog13_5_mat,
                            y_pred_cog13_6_mat)
 colnames(y_pred_cog13_mat)[3] <- "predictedKfold"
  y_pred_cog13_mat <- merge(y_pred_cog13_mat, 
                          D13Imp1.1[,c(1:2, which(colnames(D13Imp1.1)=="predicted_cognitive_13"))],
                          all.x=T, all.y=T)
  #now check the correlation
  cor.test(y_pred_cog13_mat$predictedKfold,y_pred_cog13_mat$predicted_cognitive_13)


#########################################################################################
###### Exporting the variables to csv for further analyses ##############################
#########################################################################################

#changing names to include age 
  DImp1.1 <- rename(DImp1.1, c(EMPQ_emotional="EMPQ_emotional_11",
                             EMPQ_cognitive="EMPQ_cognitive_11",
                             EMPQ_IRI_motiv="EMPQ_IRI_motiv_11"))


  D13Imp1.1 <- rename(D13Imp1.1, c(EMPQ_emotional="EMPQ_emotional_13",
                                 EMPQ_cognitive="EMPQ_cognitive_13",
                                 EMPQ_IRI_motiv="EMPQ_IRI_motiv_13"))


#import general unidentified participants details of the Longitudinal Israeli Study of Twins 
  participants_details <- read.csv ("~/Documents/projects/Empathy-Personality-Adolecence/OSF/data/participants_details.csv")

  col <- colnames((participants_details))
  relvar_participants_details <- c(which (col=="ifam"), which (col=="ID"),
                                   which (col=="sex"), which (col=="zygosity"),which (col=="zygoAcc3")) 

  col <- colnames((D))
  relvar_more_details_11 <- c(which (col=="ifam"), which (col=="ID"), which (col=="OnlineManual"))
                          
  col <- colnames((DImp1.1))
  relvar_D11 <-  c(which (col=="ifam"), which (col=="ID"), 
                  which(col=="EMPQ_emotional_11"), which(col=="EMPQ_cognitive_11"),
                  which(col=="predicted_emotional_11"), which(col=="predicted_cognitive_11"))

  col <- colnames((D13))
  relvar_more_details_13 <- c(which (col=="ifam"), which (col=="ID"), which (col=="OnlineManual13"))
                          
  col <- colnames((D13Imp1.1))
  relvar_D13 <-  c(which (col=="ifam"), which (col=="ID"), 
                  which(col=="EMPQ_emotional_13"), which(col=="EMPQ_cognitive_13"),
                  which(col=="predicted_emotional_13"), which(col=="predicted_cognitive_13"))

#merge final vars from all datasets
  Dfinal11 <- merge (D[,relvar_more_details_11],DImp1.1[,relvar_D11], by=c("ifam","ID"), all.y = T)
  Dfinal13 <- merge (D13[,relvar_more_details_13],D13Imp1.1[,relvar_D13], by=c("ifam","ID"), all.y = T)
  Dfinal_11_13 <- merge(Dfinal11,Dfinal13, by=c("ifam","ID"), all.x=T, all.y=T)
  Dfinal_11_13 <- merge(Dfinal_11_13, participants_details[,relvar_participants_details],
                         by=c("ifam","ID"), all.x=T, all.y=F)
  

#export to csv
  write.csv(Dfinal_11_13,row.names=F,"~/Documents/projects/Empathy-Personality-Adolecence/OSF/data/Dfinal_11_13.csv")


######################################################################################################
###### checking the model prediction on an independent, non-twin sample ##############################
######################################################################################################

#import data (called panel sample)
  DPan <- read.csv ("~/Documents/projects/Empathy-Personality-Adolecence/OSF/data/panel_results_PerEmp.csv")

#reversing items
  CreateReverseItems_allData_panel <-function(DPan) {
    
    DPan$BFI6_Rev <- 6-DPan$BFI6
    DPan$BFI21_Rev <- 6-DPan$BFI21
    DPan$BFI31_Rev <- 6-DPan$BFI31
    DPan$BFI2_Rev <- 6-DPan$BFI2
    DPan$BFI12_Rev <- 6-DPan$BFI12
    DPan$BFI27_Rev <- 6-DPan$BFI27
    DPan$BFI37_Rev <- 6-DPan$BFI37
    DPan$BFI8_Rev <- 6-DPan$BFI8
    DPan$BFI18_Rev <- 6-DPan$BFI18
    DPan$BFI23_Rev <- 6-DPan$BFI23
    DPan$BFI43_Rev <- 6-DPan$BFI43
    DPan$BFI9_Rev <- 6-DPan$BFI9
    DPan$BFI24_Rev <- 6-DPan$BFI24
    DPan$BFI34_Rev <- 6-DPan$BFI34
    DPan$BFI35_Rev <- 6-DPan$BFI35
    DPan$BFI41_Rev <- 6-DPan$BFI41
    
    assign ("DPan",DPan,envir = .GlobalEnv)
    
  }
  
  CreateReverseItems_allData_panel (DPan)

#remove children who are too young (elementary school)
  DPan <- DPan[DPan$isOut==0,]

  col <- colnames(DPan)
  relvar_emotional_panel <- c(which (col=="EMPQ_emotional"),
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

  relvar_cognitive_panel <- c(which(col=="EMPQ_cognitive"),
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


#scale all the BFI items so they all will have mean=0 and SD=1
  for (i in 2:45) {DPan[,relvar_emotional_panel[i]] <-
     scale(DPan[,relvar_emotional_panel[i]], scale=T)}


#check prediction of age 11 model- use the model that was done on the entire sample
#emotional empathy
  y_pred_emo_panel <- predict(fit_emo11_all, s=opt_lambda_emo11, 
                            newx = as.matrix(DPan[,relvar_emotional_panel[2:45]]))

  cor_panel_emo11<- cor.test(DPan$EMPQ_emotional, y_pred_emo_panel)
  R2_panel_emo11 <- as.numeric(cor_panel_emo11$estimate)^2
  mse_panel_emo11 <- mean((DPan[,relvar_emotional_panel[1]]-y_pred_emo_panel)^2)

#cognitive empathy
  y_pred_cog_panel <- predict(fit_cog11_all, s=opt_lambda_cog11, 
                            newx = as.matrix(DPan[,relvar_cognitive_panel[2:45]]))

  cor_panel_cog11 <- cor.test(DPan$EMPQ_cognitive, y_pred_cog_panel)
  R2_panel_cog11 <- as.numeric(cor_panel_cog11$estimate)^2
  mse_panel_cog11 <- mean((DPan[,relvar_cognitive_panel[1]]-y_pred_cog_panel)^2)


#check the prediction of the age 13 model - use the model that was done on the entire sample
#emotional empathy
  y_pred_emo13_panel <- predict(fit_emo13_all, s=opt_lambda_emo13, 
                                newx = as.matrix(DPan[,relvar_emotional_panel[2:45]]))
  
  cor_panel_emo13 <- cor.test(DPan$EMPQ_emotional, y_pred_emo13_panel)
  R2_panel_emo13  <- as.numeric(cor_panel_emo13$estimate)^2
  mse_panel_emo13 <- mean((DPan[,relvar_emotional_panel[1]]-y_pred_emo13_panel)^2)
  
#cognitive empathy
  y_pred_cog13_panel <- predict(fit_cog13_all, s=opt_lambda_cog13, 
                                newx = as.matrix(DPan[,relvar_cognitive_panel[2:45]]))
  
  cor_panel_cog13 <- cor.test(DPan$EMPQ_cognitive, y_pred_cog13_panel)
  R2_panel_cog13  <- as.numeric(cor_panel_cog13$estimate)^2
  mse_panel_cog13 <- mean((DPan[,relvar_cognitive_panel[1]]-y_pred_cog13_panel)^2)


#####################################################################################################
##################### Find out how many in each age before dropping invalid observations ############
#####################################################################################################
   
setwd("C:/Users/Lior Abramson/Dropbox/empathy and puberty/SVR")  
   age11 <- read.csv ("Age11_EmpPer_4SVR.csv")
   age11 <- age11[,1:2]
   library(reshape)
   age11 <- rename(age11, c(ï..ifam="ifam"))    # change weird variable names
   age11$was11 <-1

   
   age13 <- read.csv ("Age13_EmpPer_4SVR.csv")
   age13 <- age13[,1:2]
   library(reshape)
   age13 <- rename(age13, c(ï..ifam="ifam"))    # change weird variable names
   age13$was13 <-1

both_1113 <- merge (age11,age13, by=c("ifam","ID"), all.x=T, all.y = T)

sum(both_1113$was11==1 & both_1113$was13==1, na.rm=T)



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
