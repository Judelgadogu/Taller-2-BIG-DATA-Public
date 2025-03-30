rm(list = ls())
gc()
closeAllConnections()

# --------------------------
# 1. CONFIGURACIÓN DEL DIRECTORIO DE TRABAJO
# --------------------------

user <- Sys.getenv("USERNAME")

if (user == "judel") {
  path <- "C:/Users/judel/OneDrive/Documentos/ANDES/Semestre 2/Big data/segunda parte/Taller 2/input"
} else if (user == "---") {
  path <- "-----"
} else if (user == "-evaluador-") {
  path <- "-----"
} else {
  stop("Usuario no reconocido. Ajusta el 'path' manualmente.")
}

setwd(path)
pacman:: p_load(tidyverse,stargazer,caret,tidymodels,glmnet,parallel,doParallel,MLmetrics,themis,rattle,rlang,randomForest,mlr,rpart,rpart.plot,kableExtra)

set.seed(1234)
n_cores<-detectCores()
cl <- makePSOCKcluster(n_cores - 1) 

train_hogares <- readRDS("stores/train_hogares_full.Rds")


###################### ENTRENAMIENTO DE LOS MODELOS ############################
#hay que modificar esta con las nuevas variables 
Train<-model.matrix(object = ~ Ingtotugarr + Lp + Pobre+Npersug + Clase + P5010 + P5090 + P5100 + P5130 + P5140 + 
                      + Ingresos_AlquilerPensiones + + AyudasEco + Subsidios+
                      TGP+ tasa_desempleo + P_o + JH_Mujer + JH_Edad +JH_RSS_S + JH_NEduc +  JH_CotizaPension + 
                      JH_Oc +  JH_Ina-1,data=train_hogares)


Train_y<-Train[,c('Lp','Pobre','Ingtotugarr')]
Train_x<-Train[,!colnames(Train) %in% c('Lp','Pobre','Ingtotugarr')]


######################  DEFINICION DE METRICAS DE MEDICION MODELOS ############################


False_rate_reg <- function (data, lev = NULL, model = NULL) {
  
  FR <-function(pred,obs,Lp,P){#FR = FNR*0.75 + FPR*0.25 = (fn/(fn+tp)*0.75)+(fp/(fp+tn)*0.25)
    P_pred=ifelse(pred<=Lp,1,0)
    if(length(unique(P))==length(unique(P_pred))){ #FR
      ((table(P_pred,P)['0','1']/(table(P_pred,P)['0','1']+table(P_pred,P)['1','1']))*0.75)+
        ((table(P_pred,P)['1','0']/(table(P_pred,P)['1','0']+table(P_pred,P)['0','0']))*0.25)
    } else if (length(unique(P_pred))==2 & length(unique(P))==1){
      if (unique(P)==0){#FPR
        ((table(P_pred,P)['1','0']/(table(P_pred,P)['1','0']+table(P_pred,P)['0','0']))*0.25)
      } else if (unique(P)==1){#FNR
        ((table(P_pred,P)['0','1']/(table(P_pred,P)['0','1']+table(P_pred,P)['1','1']))*0.75)
      } else if (length(unique(P_pred))==1 & length(unique(P))==2){
        if (unique(P_pred)==0){#FNR
          (table(P_pred,P)['0','1']/(table(P_pred,P)['0','1'])*0.75)
        } else if (unique(P_pred)==1){#FPR
          (table(P_pred,P)['1','0']/(table(P_pred,P)['1','0'])*0.25)  
        }
      }
    }
  }
  
  FPR <- function (pred,obs,Lp,P){ #FPR = fp/(fp+tn)
    P_pred=ifelse(pred<=Lp,1,0)
    if(length(unique(P))==length(unique(P_pred))){ #FPR
      (table(P_pred,P)['1','0']/(table(P_pred,P)['1','0']+table(P_pred,P)['0','0']))
    } else if (length(unique(P_pred))==2 & length(unique(P))==1){
      if (unique(P)==0){#FPR
        (table(P_pred,P)['1','0']/(table(P_pred,P)['1','0']+table(P_pred,P)['0','0']))
      } else if (unique(P)==1){#FNR
      } else if (length(unique(P_pred))==1 & length(unique(P))==2){
        if (unique(P_pred)==0){#FNR
        } else if (unique(P_pred)==1){#FPR
          (table(P_pred,P)['1','0']/(table(P_pred,P)['1','0']))
        }
      }
    }
  }
  
  FNR <- function (pred,obs,Lp,P){ #FNR = fn/(fn+tp)
    P_pred=ifelse(pred<=Lp,1,0)
    if(length(unique(P))==length(unique(P_pred))){ #FNR
      table(P_pred,P)['0','1']/(table(P_pred,P)['0','1']+table(P_pred,P)['1','1'])
    } else if (length(unique(P_pred))==2 & length(unique(P))==1){
      if (unique(P)==0){#FPR
      } else if (unique(P)==1){#FNR
        ((table(P_pred,P)['0','1']/(table(P_pred,P)['0','1']+table(P_pred,P)['1','1']))*0.75)
      } else if (length(unique(P_pred))==1 & length(unique(P))==2){
        if (unique(P_pred)==0){#FNR
          table(P_pred,P)['0','1']/(table(P_pred,P)['0','1'])
        } else if (unique(P_pred)==1){#FPR
        }
      }
    }
  }
  
  w<-FR(pred = data$pred ,obs = data$obs ,Lp = data$Lp, P = data$Pobre)
  
  fp<-FPR(pred = data$pred ,obs = data$obs ,Lp = data$Lp, P = data$Pobre)
  
  fn<-FNR(pred = data$pred ,obs = data$obs ,Lp = data$Lp, P = data$Pobre)
  
  c(FR = w, FNR = fn, FPR = fp)
}

False_rate_class <- function (data, lev = c('NoPobre','Pobre'), model = NULL) {
  
  FR <-function(pred,P){#FR = FNR*0.75 + FPR*0.25 = (fn/(fn+tp)*0.75)+(fp/(fp+tn)*0.25)
    if(length(unique(P))==length(unique(pred))){ #FR
      ((table(pred,P)['NoPobre','Pobre']/(table(pred,P)['NoPobre','Pobre']+table(pred,P)['Pobre','Pobre']))*0.75)+
        ((table(pred,P)['Pobre','NoPobre']/(table(pred,P)['Pobre','NoPobre']+table(pred,P)['NoPobre','NoPobre']))*0.25)
    } else if (length(unique(pred))==2 & length(unique(P))==Pobre){
      if (unique(P)==0){#FPR
        ((table(pred,P)['Pobre','NoPobre']/(table(pred,P)['Pobre','NoPobre']+table(pred,P)['NoPobre','NoPobre']))*0.25)
      } else if (unique(P)==1){#FNR
        ((table(pred,P)['NoPobre','Pobre']/(table(pred,P)['NoPobre','Pobre']+table(pred,P)['Pobre','Pobre']))*0.75)
      } else if (length(unique(pred))==1 & length(unique(P))==2){
        if (unique(pred)==0){#FNR
          (table(pred,P)['NoPobre','Pobre']/(table(pred,P)['NoPobre','Pobre'])*0.75)
        } else if (unique(pred)==1){#FPR
          (table(pred,P)['Pobre','NoPobre']/(table(pred,P)['Pobre','NoPobre'])*0.25)  
        }
      }
    }
  }
  
  FPR <- function (pred,P){ #FPR = fp/(fp+tn)
    if(length(unique(P))==length(unique(pred))){ #FPR
      (table(pred,P)['Pobre','NoPobre']/(table(pred,P)['Pobre','NoPobre']+table(pred,P)['NoPobre','NoPobre']))
    } else if (length(unique(pred))==2 & length(unique(P))==1){
      if (unique(P)==0){#FPR
        (table(pred,P)['Pobre','NoPobre']/(table(pred,P)['Pobre','NoPobre']+table(pred,P)['NoPobre','NoPobre']))
      } else if (unique(P)==1){#FNR
      } else if (length(unique(pred))==1 & length(unique(P))==2){
        if (unique(pred)==0){#FNR
        } else if (unique(pred)==1){#FPR
          (table(pred,P)['Pobre','NoPobre']/(table(pred,P)['Pobre','NoPobre']))
        }
      }
    }
  }
  
  FNR <- function (pred,P){ #FNR = fn/(fn+tp)
    if(length(unique(P))==length(unique(pred))){ #FNR
      table(pred,P)['NoPobre','Pobre']/(table(pred,P)['NoPobre','Pobre']+table(pred,P)['Pobre','Pobre'])
    } else if (length(unique(pred))==2 & length(unique(P))==1){
      if (unique(P)==0){#FPR
      } else if (unique(P)==1){#FNR
        ((table(pred,P)['NoPobre','Pobre']/(table(pred,P)['NoPobre','Pobre']+table(pred,P)['Pobre','Pobre']))*0.75)
      } else if (length(unique(pred))==1 & length(unique(P))==2){
        if (unique(pred)==0){#FNR
          table(pred,P)['NoPobre','Pobre']/(table(pred,P)['NoPobre','Pobre'])
        } else if (unique(pred)==1){#FPR
        }
      }
    }
  }
  
  w<-FR(pred = data$pred, P = data$obs)
  
  fp<-FPR(pred = data$pred, P = data$obs)
  
  fn<-FNR(pred = data$pred, P = data$obs)
  
  c(FR = w, FNR = fn, FPR = fp)
}

########### PREDICCIÓN INGRESO ##########
train<-as.data.frame(Train)
Train_reg_recipe<- recipe(Ingtotugarr ~ ., data = train)%>%
  add_role(Lp, new_role = "performance var")%>%
  add_role(Pobre, new_role = "performance var")

##### Lasso #####
set.seed(1234)
registerDoParallel(cl)
lasso<-glmnet(x=Train_x,y=Train_y[,'Ingtotugarr'],alpha=1,nlambda=10,standarize=F)
lambdas<-lasso[["lambda"]]
Lasso_CV <-caret::train( Train_reg_recipe, train, method = "glmnet", trControl = trainControl(method = "cv", number = 10 ,savePredictions = 'none',verboseIter=T,summaryFunction = False_rate_reg),metric="FR",maximaize= FALSE, tuneGrid = expand.grid(alpha = 1,lambda=lambdas))


metricas_HyperP_l <- data.frame(Modelo = "Lasso", 
                                "lambda" = Lasso_CV[["bestTune"]][["lambda"]], 
                                "alpha" = Lasso_CV[["bestTune"]][["alpha"]],
                                "FNR" = mean(Lasso_CV[["results"]][["FNR"]]),
                                "FPR" = mean(Lasso_CV[["results"]][["FPR"]]),
                                "FR" = mean(Lasso_CV[["results"]][["FR"]])   )


#variables del modelo de lasso 
lasso<-glmnet(x=Train_x, y=Train_y[,'Ingtotugarr'], alpha=1, lambda= Lasso_CV[["bestTune"]][["lambda"]], standarize=F,intercept = F)
Betas<-coef(lasso,  exact = FALSE,x=x, y=y)
Betas<-Betas@Dimnames[[1]][which(Betas!= 0)]
Betas=Betas[Betas!='(Intercept)']

stargazer(metricas_HyperP_l,type="text",summary=F,out = "/stores/views/LassoReg.txt")


metricas_HyperP_l%>%
  kbl()%>%
  kable_styling(full_width = T)


betas <- sapply(Betas, function(i) { paste0(i, "+") })
betas <- paste(betas, collapse = '')
betas  <- substr(betas, 1, nchar(betas)-1)

model_Ing_lasso<-formula(paste0("Ingtotugarr~","Lp+Pobre+",betas))
Train_xlasso<-Train_x[,Betas]    
rm(betas,Betas,lambdas,lasso,Lasso_CV) 



##### Ridge #####
set.seed(1234)
Ridge<-glmnet(x=Train_x, y=Train_y[,'Ingtotugarr'],alpha=0,nlambda=100,standarize=F)
lambdas<-Ridge[["lambda"]]
Ridge_CV <-caret::train( Train_reg_recipe, train, method = "glmnet", trControl = trainControl(method = "cv", number = 10 ,savePredictions = 'none',verboseIter=T,summaryFunction = False_rate_reg),metric="FR",maximaize= FALSE, tuneGrid = expand.grid(alpha = 0,lambda=lambdas))

metricas_HyperP_r <- data.frame(Modelo = "Ridge", 
                                "lambda" = Ridge_CV[["bestTune"]][["lambda"]], 
                                "alpha" = Ridge_CV[["bestTune"]][["alpha"]],
                                "FNR" = mean(Ridge_CV[["results"]][["FNR"]]),
                                "FPR" = mean(Ridge_CV[["results"]][["FPR"]]),
                                "FR" = mean(Ridge_CV[["results"]][["FR"]])   )

stargazer(metricas_HyperP_r,type="text",summary=F,out = "views/RidgeReg.txt")

metricas_HyperP_r%>%
  kbl()%>%
  kable_styling(full_width = T)

metricas_HyperP<-rbind(metricas_HyperP,metricas_HyperP_r)

rm(lambdas,Ridge,Ridge_CV) 






##### Ridge con lasso variables #####
train<-as.data.frame(Train)
Train_lasso_reg_recipe<- recipe(model_Ing_lasso, data = train)%>%
  add_role(Lp, new_role = "performance var")%>%
  add_role(Pobre, new_role = "performance var")

set.seed(1234)
Ridge<-glmnet(x=Train_xlasso,y=Train_y[,'Ingtotugarr'],alpha=0,nlambda=100,standarize=F)
lambdas<-Ridge[["lambda"]]
Ridge_CV <-caret::train( Train_lasso_reg_recipe, train, method = "glmnet", trControl = trainControl(method = "cv", number = 10 ,savePredictions = 'none',verboseIter=T,summaryFunction = False_rate_reg),metric="FR",maximaize= FALSE, tuneGrid = expand.grid(alpha = 0,lambda=lambdas))

metricas_HyperP_rl <- data.frame(Modelo = "Ridge - lasso variables", 
                                 "lambda" = Ridge_CV[["bestTune"]][["lambda"]], 
                                 "alpha" = Ridge_CV[["bestTune"]][["alpha"]],
                                 "FNR" = mean(Ridge_CV[["results"]][["FNR"]]),
                                 "FPR" = mean(Ridge_CV[["results"]][["FPR"]]),
                                 "FR" = mean(Ridge_CV[["results"]][["FR"]])   )

stargazer(metricas_HyperP_rl,type="text",summary=F,out = "views/RidgeLasso_Reg.txt")

metricas_HyperP_r%>%
  kbl()%>%
  kable_styling(full_width = T)

metricas_HyperP<-rbind(metricas_HyperP,metricas_HyperP_rl)

rm(lambdas,Ridge,Ridge_CV) 


##### Elastic Net #####
set.seed(1234)
EN_CV    <-caret::train( Train_reg_recipe, train, method = "glmnet", trControl = trainControl(method = "cv", number = 10, savePredictions = 'none',verboseIter=T,summaryFunction = False_rate_reg),metric="FR",maximaize= FALSE, tuneLength = 25)

metricas_HyperP_en <- data.frame(Modelo = "Ridge", 
                                 "lambda" = EN_CV[["bestTune"]][["lambda"]], 
                                 "alpha" = EN_CV[["bestTune"]][["alpha"]],
                                 "FNR" = mean(EN_CV[["results"]][["FNR"]]),
                                 "FPR" = mean(EN_CV[["results"]][["FPR"]]),
                                 "FR" = mean(EN_CV[["results"]][["FR"]])   )

stargazer(metricas_HyperP_en,type="text",summary=F,out = "views/ElasticNetReg.txt")

metricas_HyperP_en%>%
  kbl()%>%
  kable_styling(full_width = T)

metricas_HyperP<-rbind(metricas_HyperP,metricas_HyperP_en)

rm(lambdas,EN_CV) 


###edada al cuadrado 

train$JH_Edad2<-(train$JH_Edad)^2

##### EN con variables recomendadas por literatura #####
set.seed(1234)

train_2<-train%>%
  select(Ingtotugarr,JH_Edad2,JH_Edad,Pobre,Clase,Lp,P_o,JH_RSS_S,P5100,P50902,P50903,P50904,P50905,P50906,CotizaPension)


Train_EN_li_reg_recipe<- recipe(Ingtotugarr ~ ., data = train_2)%>%
  add_role(Lp, new_role = "performance var")%>%
  add_role(Pobre, new_role = "performance var")


EN_li_CV <-caret::train( Train_EN_li_reg_recipe, train, method = "glmnet", trControl = trainControl(method = "cv", number = 5, savePredictions = 'none',verboseIter=T,summaryFunction = False_rate_reg),metric="FR",maximaize= FALSE, tuneLength = 25)

metricas_HyperP_enli <- data.frame(Modelo = "Ridge", 
                                   "lambda" = EN_li_CV[["bestTune"]][["lambda"]], 
                                   "alpha" = EN_li_CV[["bestTune"]][["alpha"]],
                                   "FNR" = mean(EN_li_CV[["results"]][["FNR"]]),
                                   "FPR" = mean(EN_li_CV[["results"]][["FPR"]]),
                                   "FR" = mean(EN_li_CV[["results"]][["FR"]])   )

stargazer(metricas_HyperP_enli,type="text",summary=F,out = "views/ElasticNet_lit_Reg.txt")

metricas_HyperP_enli%>%
  kbl()%>%
  kable_styling(full_width = T)

metricas_HyperP<-rbind(metricas_HyperP,metricas_HyperP_enli)

rm(lambdas,EN_CV) 

stopCluster(cl)


##### Random Forest #####
set.seed(1234)
control = trainControl(method = "cv",number = 10,  allowParallel = TRUE,verboseIter = TRUE,returnData = FALSE,summaryFunction = False_rate_reg)
tunegrid <- expand.grid(.mtry=sqrt(ncol(Train_x)))
randomForest <- train(Train_reg_recipe, train, method='rf', metric="FR",maximaize= FALSE,tuneGrid=tunegrid, trControl=control)

metricas_HyperP_RF <- data.frame(Modelo = "Ridge", 
                                 "lambda" = randomForest[["bestTune"]][["lambda"]], # cambiar por sus metricas
                                 "alpha" = randomForest[["bestTune"]][["alpha"]],#
                                 "FNR" = mean(randomForest[["results"]][["FNR"]]),
                                 "FPR" = mean(randomForest[["results"]][["FPR"]]),
                                 "FR" = mean(randomForest[["results"]][["FR"]])   )

stargazer(metricas_HyperP_RF,type="text",summary=F,out = "views/RandomForest_Reg.txt")

metricas_HyperP_RF%>%
  kbl()%>%
  kable_styling(full_width = T)
metricas_HyperP<-rbind(metricas_HyperP,metricas_HyperP_RF)
rm(control,tunegrid,randomForest) 


##### XGBoost #####
set.seed(1234)
control = trainControl(method = "cv",number = 10,  allowParallel = TRUE,verboseIter = TRUE,returnData = FALSE,summaryFunction = False_rate_reg)
xgbGrid <- expand.grid(nrounds = c(100,200), max_depth = c(3, 4, 6, 9),colsample_bytree = seq(0.5, 0.9, length.out = 5),eta = 0.1,gamma=0,min_child_weight = 1,subsample = 0.6)
xgb = train(Train_reg_recipe, train,trControl = xgb_trcontrol,tuneGrid = xgbGrid,method = "xgbTree",metric="FR",maximaize= FALSE)


metricas_HyperP_xgb <- data.frame(Modelo = "Ridge", 
                                  "lambda" = xgb[["bestTune"]][["lambda"]], # cambiar por sus metricas
                                  "alpha" = xgb[["bestTune"]][["alpha"]],#
                                  "FNR" = mean(xgb[["results"]][["FNR"]]),
                                  "FPR" = mean(xgb[["results"]][["FPR"]]),
                                  "FR" = mean(xgb[["results"]][["FR"]])   )

stargazer(metricas_HyperP_xgb,type="text",summary=F,out = "views/XGB_Reg.txt")

metricas_HyperP_xgb%>%
  kbl()%>%
  kable_styling(full_width = T)

metricas_HyperP<-rbind(metricas_HyperP,metricas_HyperP_xgb)

rm(control,xgb,xgbGrid) 

##### Regression Trees Bagging  #####
set.seed(1234)
cp_alpha<-seq(from = 0, to = 0.1, length = 10) set.seed(1410)
tree <- train(x=Train_x,y=Train_y[,'Ingtotugarr'], method = "rpart",trControl = ctrl, parms=list(split='Gini'),tuneGrid = expand.grid(cp = cp_alpha), tuneLength=200)








########### PREDICCIÓN POBREZA ##########

train_C<-as.data.frame(Train)
train_C$Pobre=as.factor(train_C$Pobre)
levels(train_C$Pobre)<-c('NoPobre','Pobre')

pred<-c(1,0,1,0,1,0,1,0,0,0)
p<-c(1,0,0,0,1,1,1,0,0,0)
data<-data.frame(pred,p)
data$pred=as.factor(data$pred)
data$p=as.factor(data$p)
levels(train_C)<-c('NoPobre','Pobre')

False_rate_class()

Train_clas_recipe<- recipe(Pobre ~ ., data = train_C)

##### LOGIT #####
set.seed(1234)
logit<-glmnet(x=Train_x,y=Train_y[,'Pobre'],alpha=1,nlambda=3,standarize=F,family = "binomial")
lambdas<-logit[["lambda"]]
#LOGIT_CV <-caret::train( Pobre ~ ., data = Train, method = "glmnet", trControl = trainControl(method = "cv", number = 2 ,savePredictions = 'final',verboseIter=T,summaryFunction = False_rate_class),metric="FR",maximaize= FALSE,family = "binomial", tuneGrid = expand.grid(alpha = 1,lambda=lambdas))
LOGIT_CV<-caret::train(Train_clas_recipe, train_C ,method='glmnet',trCLontrol=trainControl(method='none',number=1,summaryFunction = False_rate_class),metric='FR',tuneGrid = expand.grid(alpha = 1,lambda=lambdas))


##### Random Forest #####
set.seed(1234)
control = trainControl(method = "cv",number = 5,  allowParallel = TRUE,verboseIter = TRUE,returnData = FALSE,summaryFunction = False_rate_class)
tunegrid <- expand.grid(.mtry=sqrt(30))
randomForest<-caret::train(Train_clas_recipe,train_C, method='rf',metric='FR',tungeGrid=tunegrid,trControl=control)


metricas_HyperP_RF <- data.frame(Modelo = "Ridge", 
                                 "lambda" = randomForest[["bestTune"]][["lambda"]], # cambiar por sus metricas
                                 "alpha" = randomForest[["bestTune"]][["alpha"]],#
                                 "FNR" = mean(randomForest[["results"]][["FNR"]]),
                                 "FPR" = mean(randomForest[["results"]][["FPR"]]),
                                 "FR" = mean(randomForest[["results"]][["FR"]])   )

stargazer(metricas_HyperP_RF,type="text",summary=F,out = "views/RandomForest_Reg.txt")

metricas_HyperP_RF%>%
  kbl()%>%
  kable_styling(full_width = T)
metricas_HyperP<-rbind(metricas_HyperP,metricas_HyperP_RF)
rm(control,tunegrid,randomForest) 


##### XGBoost #####
set.seed(1234)
control = trainControl(method = "cv",number = 10,  allowParallel = TRUE,verboseIter = TRUE,returnData = FALSE,summaryFunction = False_rate_reg)
xgbGrid <- expand.grid(nrounds = c(100,200), max_depth = c(3, 4, 6, 9),colsample_bytree = seq(0.5, 0.9, length.out = 5),eta = 0.1,gamma=0,min_child_weight = 1,subsample = 0.6)
xgb = train(Train_reg_recipe, train,trControl = xgb_trcontrol,tuneGrid = xgbGrid,method = "xgbTree",metric="FR",maximaize= FALSE)


metricas_HyperP_xgb <- data.frame(Modelo = "Ridge", 
                                  "lambda" = xgb[["bestTune"]][["lambda"]], # cambiar por sus metricas
                                  "alpha" = xgb[["bestTune"]][["alpha"]],#
                                  "FNR" = mean(xgb[["results"]][["FNR"]]),
                                  "FPR" = mean(xgb[["results"]][["FPR"]]),
                                  "FR" = mean(xgb[["results"]][["FR"]])   )

stargazer(metricas_HyperP_xgb,type="text",summary=F,out = "views/XGB_Reg.txt")

metricas_HyperP_xgb%>%
  kbl()%>%
  kable_styling(full_width = T)

metricas_HyperP<-rbind(metricas_HyperP,metricas_HyperP_xgb)

rm(control,xgb,xgbGrid) 

##### Regression Trees Bagging  #####
cp_alpha<-seq(from = 0, to = 0.1, length = 10) set.seed(1410)
tree <- train(x=Train_x,y=Train_y[,'Ingtotugarr'], method = "rpart",trControl = ctrl, parms=list(split='Gini'),tuneGrid = expand.grid(cp = cp_alpha), tuneLength=200)















###################### REVISION DE CLASE DESBALANCEADA Y  AJUSTE ############################
prop.table(table(train_hogares$Pobre))
## Se aprecia que s�lo el 20% de la base es pobre


#Se separa el sample de train en dos
smp_size <- floor(0.7*nrow(train_hogares))
set.seed(666)
train_ind <- sample(1:nrow(train_hogares), size = smp_size)

train <- train_hogares[train_ind, ]
test <- train_hogares[-train_ind, ]



##OverSample
train_hogares$Pobre<- factor(train_hogares$Pobre)
#Se debe poner el modelo que se usa
train_h2 <- recipe(Pobre ~ P5000+P5010+SSalud+Trabajan+Estudiantes+CotizaPension+DeseaTrabajarMas+AyudasEco, data = train_hogares) %>%
  themis::step_smote(Pobre, over_ratio = 1) %>%
  prep() %>%
  bake(new_data = NULL)
#Corroboramos que ahora la mitad de la muestra sea pobre
prop.table(table(train_h2$Pobre))
#Aquí sabemos en qué porcentaje aumentó la muestra
(nrow(train_h2)-nrow(train_hogares))/nrow(train_hogares)*100


##UnderSample
train_h3 <- recipe(Pobre ~ P5000+P5010+SSalud+Trabajan+Estudiantes+CotizaPension+DeseaTrabajarMas+AyudasEco, data = train_hogares) %>%
  themis::step_downsample(Pobre) %>%
  prep() %>%
  bake(new_data = NULL)
#Corroboramos que ahora la mitad de la muestra sea pobre
prop.table(table(train_h3$Pobre))
nrow(train_h3)
nrow(train_hogares)

##Optimizar umbral de decisión


#Optimizador
thresholds <- seq(0.1, 0.9, length.out = 100)
opt_t<-dat.frame()
for (t in thresholds) {
  y_pred_t <- as.numeric(probs_outsample1 > t)
  f1_t <- F1_Score(y_true = train_hogares$Pobre, y_pred = y_pred_t,
                   positive = 1)
  fila <- data.frame(t = t, F1 = f1_t)
  opt_t <- bind_rows(opt_t, fila)
}

mejor_t <-  opt_t$t[which(opt_t$F1 == max(opt_t$F1, na.rm = T))]