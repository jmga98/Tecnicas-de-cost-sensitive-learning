####  :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::  ####
####                           ESTABLECER RUTAS                            ####

path_root <- 'C:/Users/jmche/OneDrive/Documentos/Universidad/TFG/Scripts R'
path_src  <- file.path(path_root, 'code/src')
path_data <- file.path(path_root, 'datacost')
memory.limit(size = 4000000)

####  :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::  ####
####                    CARGAR LIBRERÍAS Y FUNCIONES                       ####
library(data.table) # Los datos originales tienen clase data.table
library(caret)
library(ranger)
library(tidyverse)
library(pROC)
library(gridExtra)
library(latex2exp)

source(file.path(path_src, 'CodeManagement.R'))


####  :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::  ####
####                             CARGAR DATOS                              ####
dataENS.dt <- readRDS(file.path(path_data, "dataENS.dt.rds"))
str(dataENS.dt)

for (j in seq_len(ncol(dataENS.dt))){
  
  set(dataENS.dt, which(is.na(dataENS.dt[[j]])), j, '*')
  
}

dataENS.dt <- dataENS.dt[CNO_AS_true != '*']


CNO3_levels <- sort(union(unique(dataENS.dt$CNO_AS_ed), unique(dataENS.dt$CNO_AS_raw)))
CNO2_levels <- sort(union(unique(Code3toCode2(dataENS.dt$CNO_AS_ed)), unique(Code3toCode2(dataENS.dt$CNO_AS_raw))))
CNO1_levels <- sort(union(unique(CNO2toCNO1(Code3toCode2(dataENS.dt$CNO_AS_ed))), unique(CNO2toCNO1(Code3toCode2(dataENS.dt$CNO_AS_raw)))))
CS_levels   <- sort(union(unique(CNOtoCS(dataENS.dt$CNO_AS_raw)), unique(CNOtoCS(dataENS.dt$CNO_AS_true))))

CNAE3_levels <- sort(union(unique(dataENS.dt$CNAE_AS), unique(dataENS.dt$CNAE_AS)))
CNAE2_levels <- sort(union(unique(Code3toCode2(dataENS.dt$CNAE_AS)), unique(Code3toCode2(dataENS.dt$CNAE_AS))))
CNAE1_levels <- sort(union(unique(CNAE2toCNAE1(Code3toCode2(dataENS.dt$CNAE_AS))), unique(CNAE2toCNAE1(Code3toCode2(dataENS.dt$CNAE_AS)))))

dataENS.dt[
  , grupoEdad        := factor(ageGroup(EDADa))][
  , CNO1_AS_raw      := factor(CNO2toCNO1(Code3toCode2(CNO_AS_raw)), levels = CNO1_levels)][
  , CNO2_AS_raw      := factor(Code3toCode2(CNO_AS_raw), levels = CNO2_levels)][
  , CNO3_AS_raw      := factor(CNO_AS_raw, levels = CNO3_levels)][
  , CNO_AS_raw       := NULL][
  , CNO1_AS_true     := factor(CNO2toCNO1(Code3toCode2(CNO_AS_true)), levels = CNO1_levels)][
  , CNO2_AS_true     := factor(Code3toCode2(CNO_AS_true), levels = CNO2_levels)][
  , CNO3_AS_true     := factor(CNO_AS_true, levels = CNO3_levels)][
  , CNO_AS_true      := NULL][
  , claseSocial_raw  := factor(CNOtoCS(CNO3_AS_raw), levels = CS_levels)][
  , claseSocial_true := factor(CNOtoCS(CNO3_AS_true), levels = CS_levels)][  
  , error_CNO1       := factor(1L * (CNO1_AS_raw != CNO1_AS_true))][
  , error_CNO2       := factor(1L * (CNO2_AS_raw != CNO2_AS_true))][
  , error_CNO3       := factor(1L * (CNO3_AS_raw != CNO3_AS_true))][
  , error_claseSocial:= factor(1L * (claseSocial_raw != claseSocial_true))][
  , CNAE1_AS         := factor(CNAE2toCNAE1(Code3toCode2(CNAE_AS)), levels = CNAE1_levels)][
  , CNAE2_AS         := factor(Code3toCode2(CNAE_AS), levels = CNAE2_levels)][
  , CNAE3_AS         := factor(CNAE_AS, levels = CNAE3_levels)]

for (j in seq_len(ncol(dataENS.dt))){
  
  set(dataENS.dt, which(is.na(dataENS.dt[[j]])), j, '*')
  
}

####  :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::  ####
####                               ITERATIONS                              ####
nIter <- 30
seeds <- c(267, 137, 596, 738, 595,  50, 589, 174, 451, 
           603, 661, 572, 751, 725, 454, 699, 868,  57, 
           891, 411, 721, 106, 782, 232,  48,  16, 946, 
           816, 484, 269)
auc.dt <- data.table(
  iteration = integer(),
  model     = character(),
  rank      = character(),
  dataset   = character(),
  variable  = character(),
  auc       = numeric()
)

roc.list <- vector('list', nIter)

importance.dt <- data.table(
  iteration  = integer(),
  model      = character(),
  regressor  = character(),
  importance = character(),
  class      = character(),
  variable   = character()
)

CNO1_total_prob.dt <- data.table(
  iteration          = integer(),
  model              = character(),
  rank               = character(),
  CNO1_AS_running    = character(),
  ned                = numeric(),
  CNO1_running_total = numeric(),
  pseudobias         = numeric()
)

CNO1_total_score.dt <- CNO1_total_prob.dt

CNO2_total_prob.dt <- data.table(
  iteration          = integer(),
  model              = character(),
  rank               = character(),
  CNO2_AS_running    = character(),
  ned                = numeric(),
  CNO2_running_total = numeric(),
  pseudobias         = numeric()
)

CNO2_total_score.dt <- CNO2_total_prob.dt

CNO3_total_prob.dt <- data.table(
  iteration          = integer(),
  model              = character(),
  rank               = character(),
  CNO3_AS_running    = character(),
  ned                = numeric(),
  CNO3_running_total = numeric(),
  pseudobias         = numeric()
)

CNO3_total_score.dt <- CNO3_total_prob.dt

CS_total_prob.dt <- data.table(
  iteration          = integer(),
  model              = character(),
  rank               = character(),
  CS_AS_running      = character(),
  ned                = numeric(),
  CS_running_total   = numeric(),
  pseudobias         = numeric()
)

CS_total_score.dt <- CS_total_prob.dt


####  :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::  ####
####                       CONSTRUCCION DE FORMULAS                        ####

# Construyo las formulas. 
# Como regresores aparecen todas las variables salvo las variables relacionadas con el valor true o con el error.


target_CNO1     <- 'error_CNO1'
regressors_CNO1 <- c(
  'PROXY_0', 'A7_2a', 'SEXOa', 'grupoEdad', 'CCAA',
  'F7_2', 'F16a_2', 'F16m_2', 'F17a_2', 'F17m_2', 'F8_2', 'F18', 'F9',  
  'D28', 'A10_i',
  'ESTRATO',
  'CNAE1_AS', 'CNAE2_AS', 'CNAE3_AS', 'CNO1_AS_raw', 'CNO2_AS_raw', 'CNO3_AS_raw', 'claseSocial_raw')
formula_CNO1 <- as.formula(
  paste(target_CNO1, paste(regressors_CNO1, collapse = ' + '), sep = ' ~ '))

target_CNO2     <- 'error_CNO2'
regressors_CNO2 <- regressors_CNO1
formula_CNO2    <- as.formula(
  paste(target_CNO2, paste(regressors_CNO2, collapse = ' + '), sep = ' ~ '))

target_CNO3     <- 'error_CNO3'
regressors_CNO3 <- regressors_CNO1
formula_CNO3    <- as.formula(
  paste(target_CNO3, paste(regressors_CNO3, collapse = ' + '), sep = ' ~ '))

target_CS     <- 'error_claseSocial'
regressors_CS <- regressors_CNO1
formula_CS    <- as.formula(
  paste(target_CS, paste(regressors_CS, collapse = ' + '), sep = ' ~ '))

for (iter in 1:nIter){
  
  cat(paste0('Computing iteration ', iter, '\n'))
  cat('    Train-Test Division ...')
  ####  :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::  ####
  ####                    * DIVISIÓN ENTRENAMIENTO Y TEST                    ####
  
  set.seed(seeds[iter])
  train_index <- createDataPartition(
    dataENS.dt$CNO1_AS_raw, times=1, p=0.8, list=FALSE)  
  dataENS_train.dt <- dataENS.dt[train_index]
  dataENS_test.dt  <- dataENS.dt[-train_index]
  
  cat(' ok.\n')
  cat('    Computing cost-sensitive model CNO1...\n')
  ####  :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::  ####
  ####                      MODELO COST-SENSITIVE C = 1.4                    ####
  
  ## ..............................  CNO1  .................................. ####
  # ** Ajuste del modelo de cost-sensitive random forest con c = 1.4        ####
  
  set.seed(seeds[iter])
  normalRF_CNO1_c1 <- ranger(
    formula = formula_CNO1, 
    data    = dataENS_train.dt,
    num.trees = 500,
    mtry    = 8, 
    min.node.size = 9, 
    splitrule = 'gini',
    probability = TRUE,
    importance = 'impurity'
  )
  
  # ** Cálculo de las áreas bajo la curva AUC                                    ####
  cat('      Computing AUC...')
  roc_normalRF_prob_CNO1_train_c1 <- roc(dataENS_train.dt$error_CNO1, normalRF_CNO1_c1$predictions[, '1'])
  auc_normalRF_prob_CNO1_train_c1 <- auc(roc_normalRF_prob_CNO1_train_c1)
  
  pred_normalRF_prob_CNO1_c1      <- predict(normalRF_CNO1_c1, data = dataENS_test.dt)
  roc_normalRF_prob_CNO1_test_c1  <- roc(dataENS_test.dt$error_CNO1, pred_normalRF_prob_CNO1_c1$predictions[, '1'])
  auc_normalRF_prob_CNO1_test_c1  <- auc(roc_normalRF_prob_CNO1_test_c1)
  
  roc_normalRF_score_CNO1_train_c1 <- roc(dataENS_train.dt$error_CNO1, dataENS_train.dt$FACTORADULTO * normalRF_CNO1_c1$predictions[, '1'])
  auc_normalRF_score_CNO1_train_c1 <- auc(roc_normalRF_score_CNO1_train_c1)
  
  roc_normalRF_score_CNO1_test_c1  <- roc(dataENS_test.dt$error_CNO1, dataENS_test.dt$FACTORADULTO * pred_normalRF_prob_CNO1_c1$predictions[, '1'])
  auc_normalRF_score_CNO1_test_c1  <- auc(roc_normalRF_score_CNO1_test_c1)
  
  auc_table <- cbind(data = c("train", "test", "train", "test"), 
                     rank = c("prob", "prob", "score", "score"), 
                     AUC     = round(c(auc_normalRF_prob_CNO1_train_c1, 
                                       auc_normalRF_prob_CNO1_test_c1,
                                       auc_normalRF_score_CNO1_train_c1, 
                                       auc_normalRF_score_CNO1_test_c1), 3))
  tempAUC.dt <- data.table(
    iteration = rep(as.integer(iter), 4),
    model     = 'Cost c1',
    rank      = auc_table[, 'rank'],
    dataset   = auc_table[, 'data'],
    variable  = 'CNO1',
    auc       = auc_table[, 'AUC']
  )
  auc.dt <- rbindlist(list(auc.dt, tempAUC.dt))
  
  roc.list[[iter]] <- list(
    train_prob  = roc_normalRF_prob_CNO1_train_c1, 
    test_prob   = roc_normalRF_prob_CNO1_test_c1,
    train_score = roc_normalRF_score_CNO1_train_c1, 
    test_score  = roc_normalRF_score_CNO1_test_c1)
  cat('ok.\n')
  
  # ** Cálculo de la importancia                                 ####
  cat('      Computing importance...')
  importance_normalRF_CNO1_c1 <- importance(normalRF_CNO1_c1)
  importance_normalRF_CNO1_c1.dt <- data.table(
    iteration  = rep(as.integer(iter), length(importance_normalRF_CNO1_c1)),
    model      = 'Cost c1',
    regressor  = names(importance_normalRF_CNO1_c1),
    importance = importance_normalRF_CNO1_c1,
    class      = factor(sapply(dataENS.dt, class)[names(importance_normalRF_CNO1_c1)]),
    variable   = 'CNO1')
  
  importance.dt <- rbindlist(list(
    importance.dt, importance_normalRF_CNO1_c1.dt
  ))
  cat('ok.\n')
  
  pthresh = 1.4/2.4
  
  func_score = list()
  for (i in 1:length(pred_normalRF_prob_CNO1_c1$predictions[, '1'])){
    if(pred_normalRF_prob_CNO1_c1$predictions[i, '1'] > pthresh){
      func_score[i] <- dataENS_test.dt$FACTORADULTO[i] * pred_normalRF_prob_CNO1_c1$predictions[i, '1']
    } else {
      func_score[i] <- dataENS_test.dt$FACTORADULTO[i] * pred_normalRF_prob_CNO1_c1$predictions[i, '0'] * 1.4
    }
  }
  
  func_score <- as.numeric(func_score)
  
  # ** Cálculo del pseudosesgo                                   ####
  cat('      Computing pseudobias (prob) ...')
  pseudobias_eval_c1.dt <- data.table(
    factor       = dataENS_test.dt$FACTORADULTO,
    prob_error   = pred_normalRF_prob_CNO1_c1$predictions[, '1'],
    score        = func_score,
    CNO1_true = dataENS_test.dt$CNO1_true,
    CNO1_raw  = dataENS_test.dt$CNO1_raw)
  
  pseudobias_eval_prob_c1.dt <- copy(pseudobias_eval_c1.dt)[
    , prob.rank  := frank(-prob_error, ties.method = 'min')]
  setorder(pseudobias_eval_prob_c1.dt, prob.rank)
  CNO1_running_total_prob_c1.dt <- data.table(
    CNO1_AS_running = factor(CNO1_levels)[0],
    ned = numeric(0),
    CNO1_running_total = numeric(0))
  for (i in 1:nrow(pseudobias_eval_prob_c1.dt)){
    pseudobias_eval_prob_c1.dt[
      1:i, CNO1_AS_running := CNO1_true][
        is.na(CNO1_AS_running), CNO1_AS_running := CNO1_raw]
    CNO1_running_total_prob_c1.dt <- rbindlist(list(
      CNO1_running_total_prob_c1.dt, 
      pseudobias_eval_prob_c1.dt[, list(ned = i, CNO1_running_total = sum(factor)), by = 'CNO1_AS_running']))
  }
  n <- nrow(dataENS_test.dt)
  CNO1_running_total_prob_c1.dt[
    , iteration := as.integer(iter)][
    , model := 'Cost c1'][
    , rank := 'prob'][
    , pseudobias := abs(CNO1_running_total - CNO1_running_total[n])/CNO1_running_total[n], by = 'CNO1_AS_running']
  setcolorder(CNO1_running_total_prob_c1.dt, names(CNO1_total_prob.dt))
  
  CNO1_total_prob.dt <- rbindlist(list(
    CNO1_total_prob.dt, CNO1_running_total_prob_c1.dt))
  cat('ok.\n')
  
  cat('      Computing pseudobias (score) ...')
  pseudobias_eval_score_c1.dt <- copy(pseudobias_eval_c1.dt)[
    , score.rank  := frank(-score, ties.method = 'min')]
  setorder(pseudobias_eval_score_c1.dt, score.rank)
  CNO1_running_total_score_c1.dt <- data.table(
    CNO1_AS_running = factor(CNO1_levels)[0],
    ned = numeric(0),
    CNO1_running_total = numeric(0))
  for (i in 1:nrow(pseudobias_eval_score_c1.dt)){
    pseudobias_eval_score_c1.dt[
      1:i, CNO1_AS_running := CNO1_true][
        is.na(CNO1_AS_running), CNO1_AS_running := CNO1_raw]
    CNO1_running_total_score_c1.dt <- rbindlist(list(
      CNO1_running_total_score_c1.dt, 
      pseudobias_eval_score_c1.dt[, list(ned = i, CNO1_running_total = sum(factor)), by = 'CNO1_AS_running']))
  }
  n <- nrow(dataENS_test.dt)
  CNO1_running_total_score_c1.dt[
    , iteration := as.integer(iter)][
    , model := 'Cost c1'][
    , rank := 'score'][
    , pseudobias := abs(CNO1_running_total - CNO1_running_total[n])/CNO1_running_total[n], by = 'CNO1_AS_running']
  
  setcolorder(CNO1_running_total_score_c1.dt, names(CNO1_total_score.dt))
  
  CNO1_total_score.dt <- rbindlist(list(
    CNO1_total_score.dt, CNO1_running_total_score_c1.dt))
  cat('ok.\n')
  
  ####  :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::  ####
  ####                      MODELO COST-SENSITIVE C = 2.4                    ####
  
  ## ..............................  CNO1  ..................................   ####
  # ** Ajuste del modelo de cost-sensitive random forest con c = 2.4          ####
  
  set.seed(seeds[iter])
  normalRF_CNO1_c10 <- ranger(
    formula = formula_CNO1, 
    data    = dataENS_train.dt,
    num.trees = 500,
    mtry    = 8, 
    min.node.size = 9, 
    splitrule = 'gini',
    probability = TRUE,
    importance = 'impurity'
  )
  
  # ** Cálculo de las áreas bajo la curva AUC                                    ####
  cat('      Computing AUC...')
  roc_normalRF_prob_CNO1_train_c10 <- roc(dataENS_train.dt$error_CNO1, normalRF_CNO1_c10$predictions[, '1'])
  auc_normalRF_prob_CNO1_train_c10 <- auc(roc_normalRF_prob_CNO1_train_c10)
  
  pred_normalRF_prob_CNO1_c10      <- predict(normalRF_CNO1_c10, data = dataENS_test.dt)
  roc_normalRF_prob_CNO1_test_c10  <- roc(dataENS_test.dt$error_CNO1, pred_normalRF_prob_CNO1_c10$predictions[, '1'])
  auc_normalRF_prob_CNO1_test_c10  <- auc(roc_normalRF_prob_CNO1_test_c10)
  
  roc_normalRF_score_CNO1_train_c10 <- roc(dataENS_train.dt$error_CNO1, dataENS_train.dt$FACTORADULTO * normalRF_CNO1_c10$predictions[, '1'])
  auc_normalRF_score_CNO1_train_c10 <- auc(roc_normalRF_score_CNO1_train_c10)
  
  roc_normalRF_score_CNO1_test_c10  <- roc(dataENS_test.dt$error_CNO1, dataENS_test.dt$FACTORADULTO * pred_normalRF_prob_CNO1_c10$predictions[, '1'])
  auc_normalRF_score_CNO1_test_c10  <- auc(roc_normalRF_score_CNO1_test_c10)
  
  auc_table <- cbind(data = c("train", "test", "train", "test"), 
                     rank = c("prob", "prob", "score", "score"), 
                     AUC     = round(c(auc_normalRF_prob_CNO1_train_c10, 
                                       auc_normalRF_prob_CNO1_test_c10,
                                       auc_normalRF_score_CNO1_train_c10, 
                                       auc_normalRF_score_CNO1_test_c10), 3))
  tempAUC.dt <- data.table(
    iteration = rep(as.integer(iter), 4),
    model     = 'Cost c2',
    rank      = auc_table[, 'rank'],
    dataset   = auc_table[, 'data'],
    variable  = 'CNO1',
    auc       = auc_table[, 'AUC']
  )
  auc.dt <- rbindlist(list(auc.dt, tempAUC.dt))
  
  roc.list[[iter]] <- list(
    train_prob  = roc_normalRF_prob_CNO1_train_c10, 
    test_prob   = roc_normalRF_prob_CNO1_test_c10,
    train_score = roc_normalRF_score_CNO1_train_c10, 
    test_score  = roc_normalRF_score_CNO1_test_c10)
  cat('ok.\n')
  
  # ** Cálculo de la importancia                                 ####
  cat('      Computing importance...')
  importance_normalRF_CNO1_c10 <- importance(normalRF_CNO1_c10)
  importance_normalRF_CNO1_c10.dt <- data.table(
    iteration  = rep(as.integer(iter), length(importance_normalRF_CNO1_c10)),
    model      = 'Cost c2',
    regressor  = names(importance_normalRF_CNO1_c10),
    importance = importance_normalRF_CNO1_c10,
    class      = factor(sapply(dataENS.dt, class)[names(importance_normalRF_CNO1_c10)]),
    variable   = 'CNO1')
  
  importance.dt <- rbindlist(list(
    importance.dt, importance_normalRF_CNO1_c10.dt
  ))
  cat('ok.\n')
  
  pthresh = 2.4/3.4
  for (i in 1:length(pred_normalRF_prob_CNO1_c10$predictions[, '1'])){
    if(pred_normalRF_prob_CNO1_c10$predictions[i, '1'] > pthresh){
      func_score[i] = dataENS_test.dt$FACTORADULTO[i] * pred_normalRF_prob_CNO1_c10$predictions[i, '1']
    } else {
      func_score[i] = dataENS_test.dt$FACTORADULTO[i] * pred_normalRF_prob_CNO1_c10$predictions[i, '0'] * 2.4
    }
  }
  
  func_score <- as.numeric(func_score)
  
  # ** Cálculo del pseudosesgo                                   ####
  cat('      Computing pseudobias (prob) ...')
  pseudobias_eval_c10.dt <- data.table(
    factor       = dataENS_test.dt$FACTORADULTO,
    prob_error   = pred_normalRF_prob_CNO1_c10$predictions[, '1'],
    score        = func_score,
    CNO1_true = dataENS_test.dt$CNO1_true,
    CNO1_raw  = dataENS_test.dt$CNO1_raw)
  
  pseudobias_eval_prob_c10.dt <- copy(pseudobias_eval_c10.dt)[
    , prob.rank  := frank(-prob_error, ties.method = 'min')]
  setorder(pseudobias_eval_prob_c10.dt, prob.rank)
  CNO1_running_total_prob_c10.dt <- data.table(
    CNO1_AS_running = factor(CNO1_levels)[0],
    ned = numeric(0),
    CNO1_running_total = numeric(0))
  for (i in 1:nrow(pseudobias_eval_prob_c10.dt)){
    pseudobias_eval_prob_c10.dt[
      1:i, CNO1_AS_running := CNO1_true][
        is.na(CNO1_AS_running), CNO1_AS_running := CNO1_raw]
    CNO1_running_total_prob_c10.dt <- rbindlist(list(
      CNO1_running_total_prob_c10.dt, 
      pseudobias_eval_prob_c10.dt[, list(ned = i, CNO1_running_total = sum(factor)), by = 'CNO1_AS_running']))
  }
  n <- nrow(dataENS_test.dt)
  CNO1_running_total_prob_c10.dt[
    , iteration := as.integer(iter)][
    , model := 'Cost c2'][
    , rank := 'prob'][
    , pseudobias := abs(CNO1_running_total - CNO1_running_total[n])/CNO1_running_total[n], by = 'CNO1_AS_running']
  setcolorder(CNO1_running_total_prob_c10.dt, names(CNO1_total_prob.dt))
  
  CNO1_total_prob.dt <- rbindlist(list(
    CNO1_total_prob.dt, CNO1_running_total_prob_c10.dt))
  cat('ok.\n')
  
  cat('      Computing pseudobias (score) ...')
  pseudobias_eval_score_c10.dt <- copy(pseudobias_eval_c10.dt)[
    , score.rank  := frank(-score, ties.method = 'min')]
  setorder(pseudobias_eval_score_c10.dt, score.rank)
  CNO1_running_total_score_c10.dt <- data.table(
    CNO1_AS_running = factor(CNO1_levels)[0],
    ned = numeric(0),
    CNO1_running_total = numeric(0))
  for (i in 1:nrow(pseudobias_eval_score_c10.dt)){
    pseudobias_eval_score_c10.dt[
      1:i, CNO1_AS_running := CNO1_true][
        is.na(CNO1_AS_running), CNO1_AS_running := CNO1_raw]
    CNO1_running_total_score_c10.dt <- rbindlist(list(
      CNO1_running_total_score_c10.dt, 
      pseudobias_eval_score_c10.dt[, list(ned = i, CNO1_running_total = sum(factor)), by = 'CNO1_AS_running']))
  }
  n <- nrow(dataENS_test.dt)
  CNO1_running_total_score_c10.dt[
    , iteration := as.integer(iter)][
    , model := 'Cost c2'][
    , rank := 'score'][
    , pseudobias := abs(CNO1_running_total - CNO1_running_total[n])/CNO1_running_total[n], by = 'CNO1_AS_running']
  
  setcolorder(CNO1_running_total_score_c10.dt, names(CNO1_total_score.dt))
  
  CNO1_total_score.dt <- rbindlist(list(
    CNO1_total_score.dt, CNO1_running_total_score_c10.dt))
  cat('ok.\n')
  
  ####  :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::  ####
  ####                      MODELO COST-SENSITIVE C = 5.7                    ####
  
  ## ..............................  CNO1  ..................................  ####
  # ** Ajuste del modelo de cost-sensitive random forest con c = 5.7         ####
  
  set.seed(seeds[iter])
  normalRF_CNO1_c100 <- ranger(
    formula = formula_CNO1, 
    data    = dataENS_train.dt,
    num.trees = 500,
    mtry    = 8, 
    min.node.size = 9, 
    splitrule = 'gini',
    probability = TRUE,
    importance = 'impurity'
  )
  
  # ** Cálculo de las áreas bajo la curva AUC                                    ####
  cat('      Computing AUC...')
  roc_normalRF_prob_CNO1_train_c100 <- roc(dataENS_train.dt$error_CNO1, normalRF_CNO1_c100$predictions[, '1'])
  auc_normalRF_prob_CNO1_train_c100 <- auc(roc_normalRF_prob_CNO1_train_c100)
  
  pred_normalRF_prob_CNO1_c100      <- predict(normalRF_CNO1_c100, data = dataENS_test.dt)
  roc_normalRF_prob_CNO1_test_c100  <- roc(dataENS_test.dt$error_CNO1, pred_normalRF_prob_CNO1_c100$predictions[, '1'])
  auc_normalRF_prob_CNO1_test_c100  <- auc(roc_normalRF_prob_CNO1_test_c100)
  
  roc_normalRF_score_CNO1_train_c100 <- roc(dataENS_train.dt$error_CNO1, dataENS_train.dt$FACTORADULTO * normalRF_CNO1_c100$predictions[, '1'])
  auc_normalRF_score_CNO1_train_c100 <- auc(roc_normalRF_score_CNO1_train_c100)
  
  roc_normalRF_score_CNO1_test_c100  <- roc(dataENS_test.dt$error_CNO1, dataENS_test.dt$FACTORADULTO * pred_normalRF_prob_CNO1_c100$predictions[, '1'])
  auc_normalRF_score_CNO1_test_c100  <- auc(roc_normalRF_score_CNO1_test_c100)
  
  auc_table <- cbind(data = c("train", "test", "train", "test"), 
                     rank = c("prob", "prob", "score", "score"), 
                     AUC     = round(c(auc_normalRF_prob_CNO1_train_c100, 
                                       auc_normalRF_prob_CNO1_test_c100,
                                       auc_normalRF_score_CNO1_train_c100, 
                                       auc_normalRF_score_CNO1_test_c100), 3))
  tempAUC.dt <- data.table(
    iteration = rep(as.integer(iter), 4),
    model     = 'Cost c3',
    rank      = auc_table[, 'rank'],
    dataset   = auc_table[, 'data'],
    variable  = 'CNO1',
    auc       = auc_table[, 'AUC']
  )
  auc.dt <- rbindlist(list(auc.dt, tempAUC.dt))
  
  roc.list[[iter]] <- list(
    train_prob  = roc_normalRF_prob_CNO1_train_c100, 
    test_prob   = roc_normalRF_prob_CNO1_test_c100,
    train_score = roc_normalRF_score_CNO1_train_c100, 
    test_score  = roc_normalRF_score_CNO1_test_c100)
  cat('ok.\n')
  
  # ** Cálculo de la importancia                                 ####
  cat('      Computing importance...')
  importance_normalRF_CNO1_c100 <- importance(normalRF_CNO1_c100)
  importance_normalRF_CNO1_c100.dt <- data.table(
    iteration  = rep(as.integer(iter), length(importance_normalRF_CNO1_c100)),
    model      = 'Cost c3',
    regressor  = names(importance_normalRF_CNO1_c100),
    importance = importance_normalRF_CNO1_c100,
    class      = factor(sapply(dataENS.dt, class)[names(importance_normalRF_CNO1_c100)]),
    variable   = 'CNO1')
  
  importance.dt <- rbindlist(list(
    importance.dt, importance_normalRF_CNO1_c100.dt
  ))
  cat('ok.\n')
  
  pthresh = 5.7/6.7
  for (i in 1:length(pred_normalRF_prob_CNO1_c100$predictions[, '1'])){
    if(pred_normalRF_prob_CNO1_c100$predictions[i, '1'] > pthresh){
      func_score[i] = dataENS_test.dt$FACTORADULTO[i] * pred_normalRF_prob_CNO1_c100$predictions[i, '1']
    } else {
      func_score[i] = dataENS_test.dt$FACTORADULTO[i] * pred_normalRF_prob_CNO1_c100$predictions[i, '0'] * 5.7
    }
  }
  
  func_score <- as.numeric(func_score)
  
  # ** Cálculo del pseudosesgo                                   ####
  cat('      Computing pseudobias (prob) ...')
  pseudobias_eval_c100.dt <- data.table(
    factor       = dataENS_test.dt$FACTORADULTO,
    prob_error   = pred_normalRF_prob_CNO1_c100$predictions[, '1'],
    score        = func_score,
    CNO1_true = dataENS_test.dt$CNO1_true,
    CNO1_raw  = dataENS_test.dt$CNO1_raw)
  
  pseudobias_eval_prob_c100.dt <- copy(pseudobias_eval_c100.dt)[
    , prob.rank  := frank(-prob_error, ties.method = 'min')]
  setorder(pseudobias_eval_prob_c100.dt, prob.rank)
  CNO1_running_total_prob_c100.dt <- data.table(
    CNO1_AS_running = factor(CNO1_levels)[0],
    ned = numeric(0),
    CNO1_running_total = numeric(0))
  for (i in 1:nrow(pseudobias_eval_prob_c100.dt)){
    pseudobias_eval_prob_c100.dt[
      1:i, CNO1_AS_running := CNO1_true][
        is.na(CNO1_AS_running), CNO1_AS_running := CNO1_raw]
    CNO1_running_total_prob_c100.dt <- rbindlist(list(
      CNO1_running_total_prob_c100.dt, 
      pseudobias_eval_prob_c100.dt[, list(ned = i, CNO1_running_total = sum(factor)), by = 'CNO1_AS_running']))
  }
  n <- nrow(dataENS_test.dt)
  CNO1_running_total_prob_c100.dt[
    , iteration := as.integer(iter)][
    , model := 'Cost c3'][
    , rank := 'prob'][
    , pseudobias := abs(CNO1_running_total - CNO1_running_total[n])/CNO1_running_total[n], by = 'CNO1_AS_running']
  setcolorder(CNO1_running_total_prob_c100.dt, names(CNO1_total_prob.dt))
  
  CNO1_total_prob.dt <- rbindlist(list(
    CNO1_total_prob.dt, CNO1_running_total_prob_c100.dt))
  cat('ok.\n')
  
  cat('      Computing pseudobias (score) ...')
  pseudobias_eval_score_c100.dt <- copy(pseudobias_eval_c100.dt)[
    , score.rank  := frank(-score, ties.method = 'min')]
  setorder(pseudobias_eval_score_c100.dt, score.rank)
  CNO1_running_total_score_c100.dt <- data.table(
    CNO1_AS_running = factor(CNO1_levels)[0],
    ned = numeric(0),
    CNO1_running_total = numeric(0))
  for (i in 1:nrow(pseudobias_eval_score_c100.dt)){
    pseudobias_eval_score_c100.dt[
      1:i, CNO1_AS_running := CNO1_true][
        is.na(CNO1_AS_running), CNO1_AS_running := CNO1_raw]
    CNO1_running_total_score_c100.dt <- rbindlist(list(
      CNO1_running_total_score_c100.dt, 
      pseudobias_eval_score_c100.dt[, list(ned = i, CNO1_running_total = sum(factor)), by = 'CNO1_AS_running']))
  }
  n <- nrow(dataENS_test.dt)
  CNO1_running_total_score_c100.dt[
    , iteration := as.integer(iter)][
    , model := 'Cost c3'][
    , rank := 'score'][
    , pseudobias := abs(CNO1_running_total - CNO1_running_total[n])/CNO1_running_total[n], by = 'CNO1_AS_running']
  
  setcolorder(CNO1_running_total_score_c100.dt, names(CNO1_total_score.dt))
  
  CNO1_total_score.dt <- rbindlist(list(
    CNO1_total_score.dt, CNO1_running_total_score_c100.dt))
  cat('ok.\n')
  
  CNO1_total.dt <- rbindlist(list(
    CNO1_total_prob.dt, CNO1_total_score.dt))  
}


#saveRDS(auc.dt, file = file.path(path_data, 'auc.dt'))
#saveRDS(importance.dt, file = file.path(path_data, 'importance.dt'))
#saveRDS(CNO1_total_prob.dt, file = file.path(path_data, 'CNO1_total_prob.dt'))
#saveRDS(CNO1_total_score.dt, file = file.path(path_data, 'CNO1_total_score.dt'))
#saveRDS(CNO1_total.dt, file = file.path(path_data, 'CNO1_total.dt'))

for (iter in 1:nIter){
  
  cat(paste0('Computing iteration ', iter, '\n'))
  cat('    Train-Test Division ...')
  ####  :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::  ####
  ####                    * DIVISIÓN ENTRENAMIENTO Y TEST                    ####
  
  set.seed(seeds[iter])
  train_index <- createDataPartition(
    dataENS.dt$CNO1_AS_raw, times=1, p=0.8, list=FALSE)  
  dataENS_train.dt <- dataENS.dt[train_index]
  dataENS_test.dt  <- dataENS.dt[-train_index]
  
  cat(' ok.\n')
  cat('    Computing cost-sensitive model CNO2...\n')
  ####  :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::  ####
  ####                      MODELO COST-SENSITIVE C = 1.4                    ####
  
  ## ..............................  CNO2  .................................. ####
  # ** Ajuste del modelo de cost-sensitive random forest con c = 1.4        ####
  
  set.seed(seeds[iter])
  normalRF_CNO2_c1 <- ranger(
    formula = formula_CNO2, 
    data    = dataENS_train.dt,
    num.trees = 500,
    mtry    = 8, 
    min.node.size = 9, 
    splitrule = 'gini',
    probability = TRUE,
    importance = 'impurity'
  )
  
  # ** Cálculo de las áreas bajo la curva AUC                                    ####
  cat('      Computing AUC...')
  roc_normalRF_prob_CNO2_train_c1 <- roc(dataENS_train.dt$error_CNO2, normalRF_CNO2_c1$predictions[, '1'])
  auc_normalRF_prob_CNO2_train_c1 <- auc(roc_normalRF_prob_CNO2_train_c1)
  
  pred_normalRF_prob_CNO2_c1      <- predict(normalRF_CNO2_c1, data = dataENS_test.dt)
  roc_normalRF_prob_CNO2_test_c1  <- roc(dataENS_test.dt$error_CNO2, pred_normalRF_prob_CNO2_c1$predictions[, '1'])
  auc_normalRF_prob_CNO2_test_c1  <- auc(roc_normalRF_prob_CNO2_test_c1)
  
  roc_normalRF_score_CNO2_train_c1 <- roc(dataENS_train.dt$error_CNO2, dataENS_train.dt$FACTORADULTO * normalRF_CNO2_c1$predictions[, '1'])
  auc_normalRF_score_CNO2_train_c1 <- auc(roc_normalRF_score_CNO2_train_c1)
  
  roc_normalRF_score_CNO2_test_c1  <- roc(dataENS_test.dt$error_CNO2, dataENS_test.dt$FACTORADULTO * pred_normalRF_prob_CNO2_c1$predictions[, '1'])
  auc_normalRF_score_CNO2_test_c1  <- auc(roc_normalRF_score_CNO2_test_c1)
  
  auc_table <- cbind(data = c("train", "test", "train", "test"), 
                     rank = c("prob", "prob", "score", "score"), 
                     AUC     = round(c(auc_normalRF_prob_CNO2_train_c1, 
                                       auc_normalRF_prob_CNO2_test_c1,
                                       auc_normalRF_score_CNO2_train_c1, 
                                       auc_normalRF_score_CNO2_test_c1), 3))
  tempAUC.dt <- data.table(
    iteration = rep(as.integer(iter), 4),
    model     = 'Cost c1',
    rank      = auc_table[, 'rank'],
    dataset   = auc_table[, 'data'],
    variable  = 'CNO2',
    auc       = auc_table[, 'AUC']
  )
  auc.dt <- rbindlist(list(auc.dt, tempAUC.dt))
  
  roc.list[[iter]] <- list(
    train_prob  = roc_normalRF_prob_CNO2_train_c1, 
    test_prob   = roc_normalRF_prob_CNO2_test_c1,
    train_score = roc_normalRF_score_CNO2_train_c1, 
    test_score  = roc_normalRF_score_CNO2_test_c1)
  cat('ok.\n')
  
  # ** Cálculo de la importancia                                 ####
  cat('      Computing importance...')
  importance_normalRF_CNO2_c1 <- importance(normalRF_CNO2_c1)
  importance_normalRF_CNO2_c1.dt <- data.table(
    iteration  = rep(as.integer(iter), length(importance_normalRF_CNO2_c1)),
    model      = 'Cost c1',
    regressor  = names(importance_normalRF_CNO2_c1),
    importance = importance_normalRF_CNO2_c1,
    class      = factor(sapply(dataENS.dt, class)[names(importance_normalRF_CNO2_c1)]),
    variable   = 'CNO2')
  
  importance.dt <- rbindlist(list(
    importance.dt, importance_normalRF_CNO2_c1.dt
  ))
  cat('ok.\n')
  
  pthresh = 1.4/2.4
  
  func_score = list()
  for (i in 1:length(pred_normalRF_prob_CNO2_c1$predictions[, '1'])){
    if(pred_normalRF_prob_CNO2_c1$predictions[i, '1'] > pthresh){
      func_score[i] <- dataENS_test.dt$FACTORADULTO[i] * pred_normalRF_prob_CNO2_c1$predictions[i, '1']
    } else {
      func_score[i] <- dataENS_test.dt$FACTORADULTO[i] * pred_normalRF_prob_CNO2_c1$predictions[i, '0'] * 1.4
    }
  }
  
  func_score <- as.numeric(func_score)
  
  # ** Cálculo del pseudosesgo                                   ####
  cat('      Computing pseudobias (prob) ...')
  pseudobias_eval_c1.dt <- data.table(
    factor       = dataENS_test.dt$FACTORADULTO,
    prob_error   = pred_normalRF_prob_CNO2_c1$predictions[, '1'],
    score        = func_score,
    CNO2_true = dataENS_test.dt$CNO2_true,
    CNO2_raw  = dataENS_test.dt$CNO2_raw)
  
  pseudobias_eval_prob_c1.dt <- copy(pseudobias_eval_c1.dt)[
    , prob.rank  := frank(-prob_error, ties.method = 'min')]
  setorder(pseudobias_eval_prob_c1.dt, prob.rank)
  CNO2_running_total_prob_c1.dt <- data.table(
    CNO2_AS_running = factor(CNO2_levels)[0],
    ned = numeric(0),
    CNO2_running_total = numeric(0))
  for (i in 1:nrow(pseudobias_eval_prob_c1.dt)){
    pseudobias_eval_prob_c1.dt[
      1:i, CNO2_AS_running := CNO2_true][
        is.na(CNO2_AS_running), CNO2_AS_running := CNO2_raw]
    CNO2_running_total_prob_c1.dt <- rbindlist(list(
      CNO2_running_total_prob_c1.dt, 
      pseudobias_eval_prob_c1.dt[, list(ned = i, CNO2_running_total = sum(factor)), by = 'CNO2_AS_running']))
  }
  n <- nrow(dataENS_test.dt)
  CNO2_running_total_prob_c1.dt[
    , iteration := as.integer(iter)][
    , model := 'Cost c1'][
    , rank := 'prob'][
    , pseudobias := abs(CNO2_running_total - CNO2_running_total[n])/CNO2_running_total[n], by = 'CNO2_AS_running']
  setcolorder(CNO2_running_total_prob_c1.dt, names(CNO2_total_prob.dt))
  
  CNO2_total_prob.dt <- rbindlist(list(
    CNO2_total_prob.dt, CNO2_running_total_prob_c1.dt))
  cat('ok.\n')
  
  cat('      Computing pseudobias (score) ...')
  pseudobias_eval_score_c1.dt <- copy(pseudobias_eval_c1.dt)[
    , score.rank  := frank(-score, ties.method = 'min')]
  setorder(pseudobias_eval_score_c1.dt, score.rank)
  CNO2_running_total_score_c1.dt <- data.table(
    CNO2_AS_running = factor(CNO2_levels)[0],
    ned = numeric(0),
    CNO2_running_total = numeric(0))
  for (i in 1:nrow(pseudobias_eval_score_c1.dt)){
    pseudobias_eval_score_c1.dt[
      1:i, CNO2_AS_running := CNO2_true][
        is.na(CNO2_AS_running), CNO2_AS_running := CNO2_raw]
    CNO2_running_total_score_c1.dt <- rbindlist(list(
      CNO2_running_total_score_c1.dt, 
      pseudobias_eval_score_c1.dt[, list(ned = i, CNO2_running_total = sum(factor)), by = 'CNO2_AS_running']))
  }
  n <- nrow(dataENS_test.dt)
  CNO2_running_total_score_c1.dt[
    , iteration := as.integer(iter)][
    , model := 'Cost c1'][
    , rank := 'score'][
    , pseudobias := abs(CNO2_running_total - CNO2_running_total[n])/CNO2_running_total[n], by = 'CNO2_AS_running']
  
  setcolorder(CNO2_running_total_score_c1.dt, names(CNO2_total_score.dt))
  
  CNO2_total_score.dt <- rbindlist(list(
    CNO2_total_score.dt, CNO2_running_total_score_c1.dt))
  cat('ok.\n')
  
  ####  :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::  ####
  ####                      MODELO COST-SENSITIVE C = 2.4                    ####
  
  ## ..............................  CNO2  ..................................   ####
  # ** Ajuste del modelo de cost-sensitive random forest con c = 2.4          ####
  
  set.seed(seeds[iter])
  normalRF_CNO2_c10 <- ranger(
    formula = formula_CNO2, 
    data    = dataENS_train.dt,
    num.trees = 500,
    mtry    = 8, 
    min.node.size = 9, 
    splitrule = 'gini',
    probability = TRUE,
    importance = 'impurity'
  )
  
  # ** Cálculo de las áreas bajo la curva AUC                                    ####
  cat('      Computing AUC...')
  roc_normalRF_prob_CNO2_train_c10 <- roc(dataENS_train.dt$error_CNO2, normalRF_CNO2_c10$predictions[, '1'])
  auc_normalRF_prob_CNO2_train_c10 <- auc(roc_normalRF_prob_CNO2_train_c10)
  
  pred_normalRF_prob_CNO2_c10      <- predict(normalRF_CNO2_c10, data = dataENS_test.dt)
  roc_normalRF_prob_CNO2_test_c10  <- roc(dataENS_test.dt$error_CNO2, pred_normalRF_prob_CNO2_c10$predictions[, '1'])
  auc_normalRF_prob_CNO2_test_c10  <- auc(roc_normalRF_prob_CNO2_test_c10)
  
  roc_normalRF_score_CNO2_train_c10 <- roc(dataENS_train.dt$error_CNO2, dataENS_train.dt$FACTORADULTO * normalRF_CNO2_c10$predictions[, '1'])
  auc_normalRF_score_CNO2_train_c10 <- auc(roc_normalRF_score_CNO2_train_c10)
  
  roc_normalRF_score_CNO2_test_c10  <- roc(dataENS_test.dt$error_CNO2, dataENS_test.dt$FACTORADULTO * pred_normalRF_prob_CNO2_c10$predictions[, '1'])
  auc_normalRF_score_CNO2_test_c10  <- auc(roc_normalRF_score_CNO2_test_c10)
  
  auc_table <- cbind(data = c("train", "test", "train", "test"), 
                     rank = c("prob", "prob", "score", "score"), 
                     AUC     = round(c(auc_normalRF_prob_CNO2_train_c10, 
                                       auc_normalRF_prob_CNO2_test_c10,
                                       auc_normalRF_score_CNO2_train_c10, 
                                       auc_normalRF_score_CNO2_test_c10), 3))
  tempAUC.dt <- data.table(
    iteration = rep(as.integer(iter), 4),
    model     = 'Cost c2',
    rank      = auc_table[, 'rank'],
    dataset   = auc_table[, 'data'],
    variable  = 'CNO2',
    auc       = auc_table[, 'AUC']
  )
  auc.dt <- rbindlist(list(auc.dt, tempAUC.dt))
  
  roc.list[[iter]] <- list(
    train_prob  = roc_normalRF_prob_CNO2_train_c10, 
    test_prob   = roc_normalRF_prob_CNO2_test_c10,
    train_score = roc_normalRF_score_CNO2_train_c10, 
    test_score  = roc_normalRF_score_CNO2_test_c10)
  cat('ok.\n')
  
  # ** Cálculo de la importancia                                 ####
  cat('      Computing importance...')
  importance_normalRF_CNO2_c10 <- importance(normalRF_CNO2_c10)
  importance_normalRF_CNO2_c10.dt <- data.table(
    iteration  = rep(as.integer(iter), length(importance_normalRF_CNO2_c10)),
    model      = 'Cost c2',
    regressor  = names(importance_normalRF_CNO2_c10),
    importance = importance_normalRF_CNO2_c10,
    class      = factor(sapply(dataENS.dt, class)[names(importance_normalRF_CNO2_c10)]),
    variable   = 'CNO2')
  
  importance.dt <- rbindlist(list(
    importance.dt, importance_normalRF_CNO2_c10.dt
  ))
  cat('ok.\n')
  
  pthresh = 2.4/3.4
  for (i in 1:length(pred_normalRF_prob_CNO2_c10$predictions[, '1'])){
    if(pred_normalRF_prob_CNO2_c10$predictions[i, '1'] > pthresh){
      func_score[i] = dataENS_test.dt$FACTORADULTO[i] * pred_normalRF_prob_CNO2_c10$predictions[i, '1']
    } else {
      func_score[i] = dataENS_test.dt$FACTORADULTO[i] * pred_normalRF_prob_CNO2_c10$predictions[i, '0'] * 2.4
    }
  }
  
  func_score <- as.numeric(func_score)
  
  # ** Cálculo del pseudosesgo                                   ####
  cat('      Computing pseudobias (prob) ...')
  pseudobias_eval_c10.dt <- data.table(
    factor       = dataENS_test.dt$FACTORADULTO,
    prob_error   = pred_normalRF_prob_CNO2_c10$predictions[, '1'],
    score        = func_score,
    CNO2_true = dataENS_test.dt$CNO2_true,
    CNO2_raw  = dataENS_test.dt$CNO2_raw)
  
  pseudobias_eval_prob_c10.dt <- copy(pseudobias_eval_c10.dt)[
    , prob.rank  := frank(-prob_error, ties.method = 'min')]
  setorder(pseudobias_eval_prob_c10.dt, prob.rank)
  CNO2_running_total_prob_c10.dt <- data.table(
    CNO2_AS_running = factor(CNO2_levels)[0],
    ned = numeric(0),
    CNO2_running_total = numeric(0))
  for (i in 1:nrow(pseudobias_eval_prob_c10.dt)){
    pseudobias_eval_prob_c10.dt[
      1:i, CNO2_AS_running := CNO2_true][
        is.na(CNO2_AS_running), CNO2_AS_running := CNO2_raw]
    CNO2_running_total_prob_c10.dt <- rbindlist(list(
      CNO2_running_total_prob_c10.dt, 
      pseudobias_eval_prob_c10.dt[, list(ned = i, CNO2_running_total = sum(factor)), by = 'CNO2_AS_running']))
  }
  n <- nrow(dataENS_test.dt)
  CNO2_running_total_prob_c10.dt[
    , iteration := as.integer(iter)][
    , model := 'Cost c2'][
    , rank := 'prob'][
    , pseudobias := abs(CNO2_running_total - CNO2_running_total[n])/CNO2_running_total[n], by = 'CNO2_AS_running']
  setcolorder(CNO2_running_total_prob_c10.dt, names(CNO2_total_prob.dt))
  
  CNO2_total_prob.dt <- rbindlist(list(
    CNO2_total_prob.dt, CNO2_running_total_prob_c10.dt))
  cat('ok.\n')
  
  cat('      Computing pseudobias (score) ...')
  pseudobias_eval_score_c10.dt <- copy(pseudobias_eval_c10.dt)[
    , score.rank  := frank(-score, ties.method = 'min')]
  setorder(pseudobias_eval_score_c10.dt, score.rank)
  CNO2_running_total_score_c10.dt <- data.table(
    CNO2_AS_running = factor(CNO2_levels)[0],
    ned = numeric(0),
    CNO2_running_total = numeric(0))
  for (i in 1:nrow(pseudobias_eval_score_c10.dt)){
    pseudobias_eval_score_c10.dt[
      1:i, CNO2_AS_running := CNO2_true][
        is.na(CNO2_AS_running), CNO2_AS_running := CNO2_raw]
    CNO2_running_total_score_c10.dt <- rbindlist(list(
      CNO2_running_total_score_c10.dt, 
      pseudobias_eval_score_c10.dt[, list(ned = i, CNO2_running_total = sum(factor)), by = 'CNO2_AS_running']))
  }
  n <- nrow(dataENS_test.dt)
  CNO2_running_total_score_c10.dt[
    , iteration := as.integer(iter)][
    , model := 'Cost c2'][
    , rank := 'score'][
    , pseudobias := abs(CNO2_running_total - CNO2_running_total[n])/CNO2_running_total[n], by = 'CNO2_AS_running']
  
  setcolorder(CNO2_running_total_score_c10.dt, names(CNO2_total_score.dt))
  
  CNO2_total_score.dt <- rbindlist(list(
    CNO2_total_score.dt, CNO2_running_total_score_c10.dt))
  cat('ok.\n')
  
  ####  :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::  ####
  ####                      MODELO COST-SENSITIVE C = 5.7                    ####
  
  ## ..............................  CNO2  ..................................  ####
  # ** Ajuste del modelo de cost-sensitive random forest con c = 5.7         ####
  
  set.seed(seeds[iter])
  normalRF_CNO2_c100 <- ranger(
    formula = formula_CNO2, 
    data    = dataENS_train.dt,
    num.trees = 500,
    mtry    = 8, 
    min.node.size = 9, 
    splitrule = 'gini',
    probability = TRUE,
    importance = 'impurity'
  )
  
  # ** Cálculo de las áreas bajo la curva AUC                                    ####
  cat('      Computing AUC...')
  roc_normalRF_prob_CNO2_train_c100 <- roc(dataENS_train.dt$error_CNO2, normalRF_CNO2_c100$predictions[, '1'])
  auc_normalRF_prob_CNO2_train_c100 <- auc(roc_normalRF_prob_CNO2_train_c100)
  
  pred_normalRF_prob_CNO2_c100      <- predict(normalRF_CNO2_c100, data = dataENS_test.dt)
  roc_normalRF_prob_CNO2_test_c100  <- roc(dataENS_test.dt$error_CNO2, pred_normalRF_prob_CNO2_c100$predictions[, '1'])
  auc_normalRF_prob_CNO2_test_c100  <- auc(roc_normalRF_prob_CNO2_test_c100)
  
  roc_normalRF_score_CNO2_train_c100 <- roc(dataENS_train.dt$error_CNO2, dataENS_train.dt$FACTORADULTO * normalRF_CNO2_c100$predictions[, '1'])
  auc_normalRF_score_CNO2_train_c100 <- auc(roc_normalRF_score_CNO2_train_c100)
  
  roc_normalRF_score_CNO2_test_c100  <- roc(dataENS_test.dt$error_CNO2, dataENS_test.dt$FACTORADULTO * pred_normalRF_prob_CNO2_c100$predictions[, '1'])
  auc_normalRF_score_CNO2_test_c100  <- auc(roc_normalRF_score_CNO2_test_c100)
  
  auc_table <- cbind(data = c("train", "test", "train", "test"), 
                     rank = c("prob", "prob", "score", "score"), 
                     AUC     = round(c(auc_normalRF_prob_CNO2_train_c100, 
                                       auc_normalRF_prob_CNO2_test_c100,
                                       auc_normalRF_score_CNO2_train_c100, 
                                       auc_normalRF_score_CNO2_test_c100), 3))
  tempAUC.dt <- data.table(
    iteration = rep(as.integer(iter), 4),
    model     = 'Cost c3',
    rank      = auc_table[, 'rank'],
    dataset   = auc_table[, 'data'],
    variable  = 'CNO2',
    auc       = auc_table[, 'AUC']
  )
  auc.dt <- rbindlist(list(auc.dt, tempAUC.dt))
  
  roc.list[[iter]] <- list(
    train_prob  = roc_normalRF_prob_CNO2_train_c100, 
    test_prob   = roc_normalRF_prob_CNO2_test_c100,
    train_score = roc_normalRF_score_CNO2_train_c100, 
    test_score  = roc_normalRF_score_CNO2_test_c100)
  cat('ok.\n')
  
  # ** Cálculo de la importancia                                 ####
  cat('      Computing importance...')
  importance_normalRF_CNO2_c100 <- importance(normalRF_CNO2_c100)
  importance_normalRF_CNO2_c100.dt <- data.table(
    iteration  = rep(as.integer(iter), length(importance_normalRF_CNO2_c100)),
    model      = 'Cost c3',
    regressor  = names(importance_normalRF_CNO2_c100),
    importance = importance_normalRF_CNO2_c100,
    class      = factor(sapply(dataENS.dt, class)[names(importance_normalRF_CNO2_c100)]),
    variable   = 'CNO2')
  
  importance.dt <- rbindlist(list(
    importance.dt, importance_normalRF_CNO2_c100.dt
  ))
  cat('ok.\n')
  
  pthresh = 5.7/6.7
  for (i in 1:length(pred_normalRF_prob_CNO2_c100$predictions[, '1'])){
    if(pred_normalRF_prob_CNO2_c100$predictions[i, '1'] > pthresh){
      func_score[i] = dataENS_test.dt$FACTORADULTO[i] * pred_normalRF_prob_CNO2_c100$predictions[i, '1']
    } else {
      func_score[i] = dataENS_test.dt$FACTORADULTO[i] * pred_normalRF_prob_CNO2_c100$predictions[i, '0'] * 5.7
    }
  }
  
  func_score <- as.numeric(func_score)
  
  # ** Cálculo del pseudosesgo                                   ####
  cat('      Computing pseudobias (prob) ...')
  pseudobias_eval_c100.dt <- data.table(
    factor       = dataENS_test.dt$FACTORADULTO,
    prob_error   = pred_normalRF_prob_CNO2_c100$predictions[, '1'],
    score        = func_score,
    CNO2_true = dataENS_test.dt$CNO2_true,
    CNO2_raw  = dataENS_test.dt$CNO2_raw)
  
  pseudobias_eval_prob_c100.dt <- copy(pseudobias_eval_c100.dt)[
    , prob.rank  := frank(-prob_error, ties.method = 'min')]
  setorder(pseudobias_eval_prob_c100.dt, prob.rank)
  CNO2_running_total_prob_c100.dt <- data.table(
    CNO2_AS_running = factor(CNO2_levels)[0],
    ned = numeric(0),
    CNO2_running_total = numeric(0))
  for (i in 1:nrow(pseudobias_eval_prob_c100.dt)){
    pseudobias_eval_prob_c100.dt[
      1:i, CNO2_AS_running := CNO2_true][
        is.na(CNO2_AS_running), CNO2_AS_running := CNO2_raw]
    CNO2_running_total_prob_c100.dt <- rbindlist(list(
      CNO2_running_total_prob_c100.dt, 
      pseudobias_eval_prob_c100.dt[, list(ned = i, CNO2_running_total = sum(factor)), by = 'CNO2_AS_running']))
  }
  n <- nrow(dataENS_test.dt)
  CNO2_running_total_prob_c100.dt[
    , iteration := as.integer(iter)][
    , model := 'Cost c3'][
    , rank := 'prob'][
    , pseudobias := abs(CNO2_running_total - CNO2_running_total[n])/CNO2_running_total[n], by = 'CNO2_AS_running']
  setcolorder(CNO2_running_total_prob_c100.dt, names(CNO2_total_prob.dt))
  
  CNO2_total_prob.dt <- rbindlist(list(
    CNO2_total_prob.dt, CNO2_running_total_prob_c100.dt))
  cat('ok.\n')
  
  cat('      Computing pseudobias (score) ...')
  pseudobias_eval_score_c100.dt <- copy(pseudobias_eval_c100.dt)[
    , score.rank  := frank(-score, ties.method = 'min')]
  setorder(pseudobias_eval_score_c100.dt, score.rank)
  CNO2_running_total_score_c100.dt <- data.table(
    CNO2_AS_running = factor(CNO2_levels)[0],
    ned = numeric(0),
    CNO2_running_total = numeric(0))
  for (i in 1:nrow(pseudobias_eval_score_c100.dt)){
    pseudobias_eval_score_c100.dt[
      1:i, CNO2_AS_running := CNO2_true][
        is.na(CNO2_AS_running), CNO2_AS_running := CNO2_raw]
    CNO2_running_total_score_c100.dt <- rbindlist(list(
      CNO2_running_total_score_c100.dt, 
      pseudobias_eval_score_c100.dt[, list(ned = i, CNO2_running_total = sum(factor)), by = 'CNO2_AS_running']))
  }
  n <- nrow(dataENS_test.dt)
  CNO2_running_total_score_c100.dt[
    , iteration := as.integer(iter)][
    , model := 'Cost c3'][
    , rank := 'score'][
    , pseudobias := abs(CNO2_running_total - CNO2_running_total[n])/CNO2_running_total[n], by = 'CNO2_AS_running']
  
  setcolorder(CNO2_running_total_score_c100.dt, names(CNO2_total_score.dt))
  
  CNO2_total_score.dt <- rbindlist(list(
    CNO2_total_score.dt, CNO2_running_total_score_c100.dt))
  cat('ok.\n')
  
  CNO2_total.dt <- rbindlist(list(
    CNO2_total_prob.dt, CNO2_total_score.dt))  
}


#saveRDS(auc.dt, file = file.path(path_data, 'auc.dt'))
#saveRDS(importance.dt, file = file.path(path_data, 'importance.dt'))
#saveRDS(CNO2_total_prob.dt, file = file.path(path_data, 'CNO2_total_prob.dt'))
#saveRDS(CNO2_total_score.dt, file = file.path(path_data, 'CNO2_total_score.dt'))
#saveRDS(CNO2_total.dt, file = file.path(path_data, 'CNO2_total.dt'))


for (iter in 1:nIter){
  
  cat(paste0('Computing iteration ', iter, '\n'))
  cat('    Train-Test Division ...')
  ####  :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::  ####
  ####                    * DIVISIÓN ENTRENAMIENTO Y TEST                    ####
  
  set.seed(seeds[iter])
  train_index <- createDataPartition(
    dataENS.dt$CNO1_AS_raw, times=1, p=0.8, list=FALSE)  
  dataENS_train.dt <- dataENS.dt[train_index]
  dataENS_test.dt  <- dataENS.dt[-train_index]
  
  cat(' ok.\n')
  cat('    Computing cost-sensitive model CNO3...\n')
  ####  :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::  ####
  ####                      MODELO COST-SENSITIVE C = 1.4                    ####
  
  ## ..............................  CNO3  .................................. ####
  # ** Ajuste del modelo de cost-sensitive random forest con c = 1.4        ####
  
  set.seed(seeds[iter])
  normalRF_CNO3_c1 <- ranger(
    formula = formula_CNO3, 
    data    = dataENS_train.dt,
    num.trees = 500,
    mtry    = 8, 
    min.node.size = 9, 
    splitrule = 'gini',
    probability = TRUE,
    importance = 'impurity'
  )
  
  # ** Cálculo de las áreas bajo la curva AUC                                    ####
  cat('      Computing AUC...')
  roc_normalRF_prob_CNO3_train_c1 <- roc(dataENS_train.dt$error_CNO3, normalRF_CNO3_c1$predictions[, '1'])
  auc_normalRF_prob_CNO3_train_c1 <- auc(roc_normalRF_prob_CNO3_train_c1)
  
  pred_normalRF_prob_CNO3_c1      <- predict(normalRF_CNO3_c1, data = dataENS_test.dt)
  roc_normalRF_prob_CNO3_test_c1  <- roc(dataENS_test.dt$error_CNO3, pred_normalRF_prob_CNO3_c1$predictions[, '1'])
  auc_normalRF_prob_CNO3_test_c1  <- auc(roc_normalRF_prob_CNO3_test_c1)
  
  roc_normalRF_score_CNO3_train_c1 <- roc(dataENS_train.dt$error_CNO3, dataENS_train.dt$FACTORADULTO * normalRF_CNO3_c1$predictions[, '1'])
  auc_normalRF_score_CNO3_train_c1 <- auc(roc_normalRF_score_CNO3_train_c1)
  
  roc_normalRF_score_CNO3_test_c1  <- roc(dataENS_test.dt$error_CNO3, dataENS_test.dt$FACTORADULTO * pred_normalRF_prob_CNO3_c1$predictions[, '1'])
  auc_normalRF_score_CNO3_test_c1  <- auc(roc_normalRF_score_CNO3_test_c1)
  
  auc_table <- cbind(data = c("train", "test", "train", "test"), 
                     rank = c("prob", "prob", "score", "score"), 
                     AUC     = round(c(auc_normalRF_prob_CNO3_train_c1, 
                                       auc_normalRF_prob_CNO3_test_c1,
                                       auc_normalRF_score_CNO3_train_c1, 
                                       auc_normalRF_score_CNO3_test_c1), 3))
  tempAUC.dt <- data.table(
    iteration = rep(as.integer(iter), 4),
    model     = 'Cost c1',
    rank      = auc_table[, 'rank'],
    dataset   = auc_table[, 'data'],
    variable  = 'CNO3',
    auc       = auc_table[, 'AUC']
  )
  auc.dt <- rbindlist(list(auc.dt, tempAUC.dt))
  
  roc.list[[iter]] <- list(
    train_prob  = roc_normalRF_prob_CNO3_train_c1, 
    test_prob   = roc_normalRF_prob_CNO3_test_c1,
    train_score = roc_normalRF_score_CNO3_train_c1, 
    test_score  = roc_normalRF_score_CNO3_test_c1)
  cat('ok.\n')
  
  # ** Cálculo de la importancia                                 ####
  cat('      Computing importance...')
  importance_normalRF_CNO3_c1 <- importance(normalRF_CNO3_c1)
  importance_normalRF_CNO3_c1.dt <- data.table(
    iteration  = rep(as.integer(iter), length(importance_normalRF_CNO3_c1)),
    model      = 'Cost c1',
    regressor  = names(importance_normalRF_CNO3_c1),
    importance = importance_normalRF_CNO3_c1,
    class      = factor(sapply(dataENS.dt, class)[names(importance_normalRF_CNO3_c1)]),
    variable   = 'CNO3')
  
  importance.dt <- rbindlist(list(
    importance.dt, importance_normalRF_CNO3_c1.dt
  ))
  cat('ok.\n')
  
  pthresh = 1.4/2.4
  
  func_score = list()
  for (i in 1:length(pred_normalRF_prob_CNO3_c1$predictions[, '1'])){
    if(pred_normalRF_prob_CNO3_c1$predictions[i, '1'] > pthresh){
      func_score[i] <- dataENS_test.dt$FACTORADULTO[i] * pred_normalRF_prob_CNO3_c1$predictions[i, '1']
    } else {
      func_score[i] <- dataENS_test.dt$FACTORADULTO[i] * pred_normalRF_prob_CNO3_c1$predictions[i, '0'] * 1.4
    }
  }
  
  func_score <- as.numeric(func_score)
  
  # ** Cálculo del pseudosesgo                                   ####
  cat('      Computing pseudobias (prob) ...')
  pseudobias_eval_c1.dt <- data.table(
    factor       = dataENS_test.dt$FACTORADULTO,
    prob_error   = pred_normalRF_prob_CNO3_c1$predictions[, '1'],
    score        = func_score,
    CNO3_true = dataENS_test.dt$CNO3_true,
    CNO3_raw  = dataENS_test.dt$CNO3_raw)
  
  pseudobias_eval_prob_c1.dt <- copy(pseudobias_eval_c1.dt)[
    , prob.rank  := frank(-prob_error, ties.method = 'min')]
  setorder(pseudobias_eval_prob_c1.dt, prob.rank)
  CNO3_running_total_prob_c1.dt <- data.table(
    CNO3_AS_running = factor(CNO3_levels)[0],
    ned = numeric(0),
    CNO3_running_total = numeric(0))
  for (i in 1:nrow(pseudobias_eval_prob_c1.dt)){
    pseudobias_eval_prob_c1.dt[
      1:i, CNO3_AS_running := CNO3_true][
        is.na(CNO3_AS_running), CNO3_AS_running := CNO3_raw]
    CNO3_running_total_prob_c1.dt <- rbindlist(list(
      CNO3_running_total_prob_c1.dt, 
      pseudobias_eval_prob_c1.dt[, list(ned = i, CNO3_running_total = sum(factor)), by = 'CNO3_AS_running']))
  }
  n <- nrow(dataENS_test.dt)
  CNO3_running_total_prob_c1.dt[
    , iteration := as.integer(iter)][
    , model := 'Cost c1'][
    , rank := 'prob'][
    , pseudobias := abs(CNO3_running_total - CNO3_running_total[n])/CNO3_running_total[n], by = 'CNO3_AS_running']
  setcolorder(CNO3_running_total_prob_c1.dt, names(CNO3_total_prob.dt))
  
  CNO3_total_prob.dt <- rbindlist(list(
    CNO3_total_prob.dt, CNO3_running_total_prob_c1.dt))
  cat('ok.\n')
  
  cat('      Computing pseudobias (score) ...')
  pseudobias_eval_score_c1.dt <- copy(pseudobias_eval_c1.dt)[
    , score.rank  := frank(-score, ties.method = 'min')]
  setorder(pseudobias_eval_score_c1.dt, score.rank)
  CNO3_running_total_score_c1.dt <- data.table(
    CNO3_AS_running = factor(CNO3_levels)[0],
    ned = numeric(0),
    CNO3_running_total = numeric(0))
  for (i in 1:nrow(pseudobias_eval_score_c1.dt)){
    pseudobias_eval_score_c1.dt[
      1:i, CNO3_AS_running := CNO3_true][
        is.na(CNO3_AS_running), CNO3_AS_running := CNO3_raw]
    CNO3_running_total_score_c1.dt <- rbindlist(list(
      CNO3_running_total_score_c1.dt, 
      pseudobias_eval_score_c1.dt[, list(ned = i, CNO3_running_total = sum(factor)), by = 'CNO3_AS_running']))
  }
  n <- nrow(dataENS_test.dt)
  CNO3_running_total_score_c1.dt[
    , iteration := as.integer(iter)][
    , model := 'Cost c1'][
    , rank := 'score'][
    , pseudobias := abs(CNO3_running_total - CNO3_running_total[n])/CNO3_running_total[n], by = 'CNO3_AS_running']
  
  setcolorder(CNO3_running_total_score_c1.dt, names(CNO3_total_score.dt))
  
  CNO3_total_score.dt <- rbindlist(list(
    CNO3_total_score.dt, CNO3_running_total_score_c1.dt))
  cat('ok.\n')
  
  ####  :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::  ####
  ####                      MODELO COST-SENSITIVE C = 2.4                    ####
  
  ## ..............................  CNO3  ..................................   ####
  # ** Ajuste del modelo de cost-sensitive random forest con c = 2.4          ####
  
  set.seed(seeds[iter])
  normalRF_CNO3_c10 <- ranger(
    formula = formula_CNO3, 
    data    = dataENS_train.dt,
    num.trees = 500,
    mtry    = 8, 
    min.node.size = 9, 
    splitrule = 'gini',
    probability = TRUE,
    importance = 'impurity'
  )
  
  # ** Cálculo de las áreas bajo la curva AUC                                    ####
  cat('      Computing AUC...')
  roc_normalRF_prob_CNO3_train_c10 <- roc(dataENS_train.dt$error_CNO3, normalRF_CNO3_c10$predictions[, '1'])
  auc_normalRF_prob_CNO3_train_c10 <- auc(roc_normalRF_prob_CNO3_train_c10)
  
  pred_normalRF_prob_CNO3_c10      <- predict(normalRF_CNO3_c10, data = dataENS_test.dt)
  roc_normalRF_prob_CNO3_test_c10  <- roc(dataENS_test.dt$error_CNO3, pred_normalRF_prob_CNO3_c10$predictions[, '1'])
  auc_normalRF_prob_CNO3_test_c10  <- auc(roc_normalRF_prob_CNO3_test_c10)
  
  roc_normalRF_score_CNO3_train_c10 <- roc(dataENS_train.dt$error_CNO3, dataENS_train.dt$FACTORADULTO * normalRF_CNO3_c10$predictions[, '1'])
  auc_normalRF_score_CNO3_train_c10 <- auc(roc_normalRF_score_CNO3_train_c10)
  
  roc_normalRF_score_CNO3_test_c10  <- roc(dataENS_test.dt$error_CNO3, dataENS_test.dt$FACTORADULTO * pred_normalRF_prob_CNO3_c10$predictions[, '1'])
  auc_normalRF_score_CNO3_test_c10  <- auc(roc_normalRF_score_CNO3_test_c10)
  
  auc_table <- cbind(data = c("train", "test", "train", "test"), 
                     rank = c("prob", "prob", "score", "score"), 
                     AUC     = round(c(auc_normalRF_prob_CNO3_train_c10, 
                                       auc_normalRF_prob_CNO3_test_c10,
                                       auc_normalRF_score_CNO3_train_c10, 
                                       auc_normalRF_score_CNO3_test_c10), 3))
  tempAUC.dt <- data.table(
    iteration = rep(as.integer(iter), 4),
    model     = 'Cost c2',
    rank      = auc_table[, 'rank'],
    dataset   = auc_table[, 'data'],
    variable  = 'CNO3',
    auc       = auc_table[, 'AUC']
  )
  auc.dt <- rbindlist(list(auc.dt, tempAUC.dt))
  
  roc.list[[iter]] <- list(
    train_prob  = roc_normalRF_prob_CNO3_train_c10, 
    test_prob   = roc_normalRF_prob_CNO3_test_c10,
    train_score = roc_normalRF_score_CNO3_train_c10, 
    test_score  = roc_normalRF_score_CNO3_test_c10)
  cat('ok.\n')
  
  # ** Cálculo de la importancia                                 ####
  cat('      Computing importance...')
  importance_normalRF_CNO3_c10 <- importance(normalRF_CNO3_c10)
  importance_normalRF_CNO3_c10.dt <- data.table(
    iteration  = rep(as.integer(iter), length(importance_normalRF_CNO3_c10)),
    model      = 'Cost c2',
    regressor  = names(importance_normalRF_CNO3_c10),
    importance = importance_normalRF_CNO3_c10,
    class      = factor(sapply(dataENS.dt, class)[names(importance_normalRF_CNO3_c10)]),
    variable   = 'CNO3')
  
  importance.dt <- rbindlist(list(
    importance.dt, importance_normalRF_CNO3_c10.dt
  ))
  cat('ok.\n')
  
  pthresh = 2.4/3.4
  for (i in 1:length(pred_normalRF_prob_CNO3_c10$predictions[, '1'])){
    if(pred_normalRF_prob_CNO3_c10$predictions[i, '1'] > pthresh){
      func_score[i] = dataENS_test.dt$FACTORADULTO[i] * pred_normalRF_prob_CNO3_c10$predictions[i, '1']
    } else {
      func_score[i] = dataENS_test.dt$FACTORADULTO[i] * pred_normalRF_prob_CNO3_c10$predictions[i, '0'] * 2.4
    }
  }
  
  func_score <- as.numeric(func_score)
  
  # ** Cálculo del pseudosesgo                                   ####
  cat('      Computing pseudobias (prob) ...')
  pseudobias_eval_c10.dt <- data.table(
    factor       = dataENS_test.dt$FACTORADULTO,
    prob_error   = pred_normalRF_prob_CNO3_c10$predictions[, '1'],
    score        = func_score,
    CNO3_true = dataENS_test.dt$CNO3_true,
    CNO3_raw  = dataENS_test.dt$CNO3_raw)
  
  pseudobias_eval_prob_c10.dt <- copy(pseudobias_eval_c10.dt)[
    , prob.rank  := frank(-prob_error, ties.method = 'min')]
  setorder(pseudobias_eval_prob_c10.dt, prob.rank)
  CNO3_running_total_prob_c10.dt <- data.table(
    CNO3_AS_running = factor(CNO3_levels)[0],
    ned = numeric(0),
    CNO3_running_total = numeric(0))
  for (i in 1:nrow(pseudobias_eval_prob_c10.dt)){
    pseudobias_eval_prob_c10.dt[
      1:i, CNO3_AS_running := CNO3_true][
        is.na(CNO3_AS_running), CNO3_AS_running := CNO3_raw]
    CNO3_running_total_prob_c10.dt <- rbindlist(list(
      CNO3_running_total_prob_c10.dt, 
      pseudobias_eval_prob_c10.dt[, list(ned = i, CNO3_running_total = sum(factor)), by = 'CNO3_AS_running']))
  }
  n <- nrow(dataENS_test.dt)
  CNO3_running_total_prob_c10.dt[
    , iteration := as.integer(iter)][
    , model := 'Cost c2'][
    , rank := 'prob'][
    , pseudobias := abs(CNO3_running_total - CNO3_running_total[n])/CNO3_running_total[n], by = 'CNO3_AS_running']
  setcolorder(CNO3_running_total_prob_c10.dt, names(CNO3_total_prob.dt))
  
  CNO3_total_prob.dt <- rbindlist(list(
    CNO3_total_prob.dt, CNO3_running_total_prob_c10.dt))
  cat('ok.\n')
  
  cat('      Computing pseudobias (score) ...')
  pseudobias_eval_score_c10.dt <- copy(pseudobias_eval_c10.dt)[
    , score.rank  := frank(-score, ties.method = 'min')]
  setorder(pseudobias_eval_score_c10.dt, score.rank)
  CNO3_running_total_score_c10.dt <- data.table(
    CNO3_AS_running = factor(CNO3_levels)[0],
    ned = numeric(0),
    CNO3_running_total = numeric(0))
  for (i in 1:nrow(pseudobias_eval_score_c10.dt)){
    pseudobias_eval_score_c10.dt[
      1:i, CNO3_AS_running := CNO3_true][
        is.na(CNO3_AS_running), CNO3_AS_running := CNO3_raw]
    CNO3_running_total_score_c10.dt <- rbindlist(list(
      CNO3_running_total_score_c10.dt, 
      pseudobias_eval_score_c10.dt[, list(ned = i, CNO3_running_total = sum(factor)), by = 'CNO3_AS_running']))
  }
  n <- nrow(dataENS_test.dt)
  CNO3_running_total_score_c10.dt[
    , iteration := as.integer(iter)][
    , model := 'Cost c2'][
    , rank := 'score'][
    , pseudobias := abs(CNO3_running_total - CNO3_running_total[n])/CNO3_running_total[n], by = 'CNO3_AS_running']
  
  setcolorder(CNO3_running_total_score_c10.dt, names(CNO3_total_score.dt))
  
  CNO3_total_score.dt <- rbindlist(list(
    CNO3_total_score.dt, CNO3_running_total_score_c10.dt))
  cat('ok.\n')
  
  ####  :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::  ####
  ####                      MODELO COST-SENSITIVE C = 5.7                    ####
  
  ## ..............................  CNO3  ..................................  ####
  # ** Ajuste del modelo de cost-sensitive random forest con c = 5.7         ####
  
  set.seed(seeds[iter])
  normalRF_CNO3_c100 <- ranger(
    formula = formula_CNO3, 
    data    = dataENS_train.dt,
    num.trees = 500,
    mtry    = 8, 
    min.node.size = 9, 
    splitrule = 'gini',
    probability = TRUE,
    importance = 'impurity'
  )
  
  # ** Cálculo de las áreas bajo la curva AUC                                    ####
  cat('      Computing AUC...')
  roc_normalRF_prob_CNO3_train_c100 <- roc(dataENS_train.dt$error_CNO3, normalRF_CNO3_c100$predictions[, '1'])
  auc_normalRF_prob_CNO3_train_c100 <- auc(roc_normalRF_prob_CNO3_train_c100)
  
  pred_normalRF_prob_CNO3_c100      <- predict(normalRF_CNO3_c100, data = dataENS_test.dt)
  roc_normalRF_prob_CNO3_test_c100  <- roc(dataENS_test.dt$error_CNO3, pred_normalRF_prob_CNO3_c100$predictions[, '1'])
  auc_normalRF_prob_CNO3_test_c100  <- auc(roc_normalRF_prob_CNO3_test_c100)
  
  roc_normalRF_score_CNO3_train_c100 <- roc(dataENS_train.dt$error_CNO3, dataENS_train.dt$FACTORADULTO * normalRF_CNO3_c100$predictions[, '1'])
  auc_normalRF_score_CNO3_train_c100 <- auc(roc_normalRF_score_CNO3_train_c100)
  
  roc_normalRF_score_CNO3_test_c100  <- roc(dataENS_test.dt$error_CNO3, dataENS_test.dt$FACTORADULTO * pred_normalRF_prob_CNO3_c100$predictions[, '1'])
  auc_normalRF_score_CNO3_test_c100  <- auc(roc_normalRF_score_CNO3_test_c100)
  
  auc_table <- cbind(data = c("train", "test", "train", "test"), 
                     rank = c("prob", "prob", "score", "score"), 
                     AUC     = round(c(auc_normalRF_prob_CNO3_train_c100, 
                                       auc_normalRF_prob_CNO3_test_c100,
                                       auc_normalRF_score_CNO3_train_c100, 
                                       auc_normalRF_score_CNO3_test_c100), 3))
  tempAUC.dt <- data.table(
    iteration = rep(as.integer(iter), 4),
    model     = 'Cost c3',
    rank      = auc_table[, 'rank'],
    dataset   = auc_table[, 'data'],
    variable  = 'CNO3',
    auc       = auc_table[, 'AUC']
  )
  auc.dt <- rbindlist(list(auc.dt, tempAUC.dt))
  
  roc.list[[iter]] <- list(
    train_prob  = roc_normalRF_prob_CNO3_train_c100, 
    test_prob   = roc_normalRF_prob_CNO3_test_c100,
    train_score = roc_normalRF_score_CNO3_train_c100, 
    test_score  = roc_normalRF_score_CNO3_test_c100)
  cat('ok.\n')
  
  # ** Cálculo de la importancia                                 ####
  cat('      Computing importance...')
  importance_normalRF_CNO3_c100 <- importance(normalRF_CNO3_c100)
  importance_normalRF_CNO3_c100.dt <- data.table(
    iteration  = rep(as.integer(iter), length(importance_normalRF_CNO3_c100)),
    model      = 'Cost c3',
    regressor  = names(importance_normalRF_CNO3_c100),
    importance = importance_normalRF_CNO3_c100,
    class      = factor(sapply(dataENS.dt, class)[names(importance_normalRF_CNO3_c100)]),
    variable   = 'CNO3')
  
  importance.dt <- rbindlist(list(
    importance.dt, importance_normalRF_CNO3_c100.dt
  ))
  cat('ok.\n')
  
  pthresh = 5.7/6.7
  for (i in 1:length(pred_normalRF_prob_CNO3_c100$predictions[, '1'])){
    if(pred_normalRF_prob_CNO3_c100$predictions[i, '1'] > pthresh){
      func_score[i] = dataENS_test.dt$FACTORADULTO[i] * pred_normalRF_prob_CNO3_c100$predictions[i, '1']
    } else {
      func_score[i] = dataENS_test.dt$FACTORADULTO[i] * pred_normalRF_prob_CNO3_c100$predictions[i, '0'] * 5.7
    }
  }
  
  func_score <- as.numeric(func_score)
  
  # ** Cálculo del pseudosesgo                                   ####
  cat('      Computing pseudobias (prob) ...')
  pseudobias_eval_c100.dt <- data.table(
    factor       = dataENS_test.dt$FACTORADULTO,
    prob_error   = pred_normalRF_prob_CNO3_c100$predictions[, '1'],
    score        = func_score,
    CNO3_true = dataENS_test.dt$CNO3_true,
    CNO3_raw  = dataENS_test.dt$CNO3_raw)
  
  pseudobias_eval_prob_c100.dt <- copy(pseudobias_eval_c100.dt)[
    , prob.rank  := frank(-prob_error, ties.method = 'min')]
  setorder(pseudobias_eval_prob_c100.dt, prob.rank)
  CNO3_running_total_prob_c100.dt <- data.table(
    CNO3_AS_running = factor(CNO3_levels)[0],
    ned = numeric(0),
    CNO3_running_total = numeric(0))
  for (i in 1:nrow(pseudobias_eval_prob_c100.dt)){
    pseudobias_eval_prob_c100.dt[
      1:i, CNO3_AS_running := CNO3_true][
        is.na(CNO3_AS_running), CNO3_AS_running := CNO3_raw]
    CNO3_running_total_prob_c100.dt <- rbindlist(list(
      CNO3_running_total_prob_c100.dt, 
      pseudobias_eval_prob_c100.dt[, list(ned = i, CNO3_running_total = sum(factor)), by = 'CNO3_AS_running']))
  }
  n <- nrow(dataENS_test.dt)
  CNO3_running_total_prob_c100.dt[
    , iteration := as.integer(iter)][
    , model := 'Cost c3'][
    , rank := 'prob'][
    , pseudobias := abs(CNO3_running_total - CNO3_running_total[n])/CNO3_running_total[n], by = 'CNO3_AS_running']
  setcolorder(CNO3_running_total_prob_c100.dt, names(CNO3_total_prob.dt))
  
  CNO3_total_prob.dt <- rbindlist(list(
    CNO3_total_prob.dt, CNO3_running_total_prob_c100.dt))
  cat('ok.\n')
  
  cat('      Computing pseudobias (score) ...')
  pseudobias_eval_score_c100.dt <- copy(pseudobias_eval_c100.dt)[
    , score.rank  := frank(-score, ties.method = 'min')]
  setorder(pseudobias_eval_score_c100.dt, score.rank)
  CNO3_running_total_score_c100.dt <- data.table(
    CNO3_AS_running = factor(CNO3_levels)[0],
    ned = numeric(0),
    CNO3_running_total = numeric(0))
  for (i in 1:nrow(pseudobias_eval_score_c100.dt)){
    pseudobias_eval_score_c100.dt[
      1:i, CNO3_AS_running := CNO3_true][
        is.na(CNO3_AS_running), CNO3_AS_running := CNO3_raw]
    CNO3_running_total_score_c100.dt <- rbindlist(list(
      CNO3_running_total_score_c100.dt, 
      pseudobias_eval_score_c100.dt[, list(ned = i, CNO3_running_total = sum(factor)), by = 'CNO3_AS_running']))
  }
  n <- nrow(dataENS_test.dt)
  CNO3_running_total_score_c100.dt[
    , iteration := as.integer(iter)][
    , model := 'Cost c3'][
    , rank := 'score'][
    , pseudobias := abs(CNO3_running_total - CNO3_running_total[n])/CNO3_running_total[n], by = 'CNO3_AS_running']
  
  setcolorder(CNO3_running_total_score_c100.dt, names(CNO3_total_score.dt))
  
  CNO3_total_score.dt <- rbindlist(list(
    CNO3_total_score.dt, CNO3_running_total_score_c100.dt))
  cat('ok.\n')
  
  CNO3_total.dt <- rbindlist(list(
    CNO3_total_prob.dt, CNO3_total_score.dt))  
}


#saveRDS(auc.dt, file = file.path(path_data, 'auc.dt'))
#saveRDS(importance.dt, file = file.path(path_data, 'importance.dt'))
#saveRDS(CNO3_total_prob.dt, file = file.path(path_data, 'CNO3_total_prob.dt'))
#saveRDS(CNO3_total_score.dt, file = file.path(path_data, 'CNO3_total_score.dt'))
#saveRDS(CNO3_total.dt, file = file.path(path_data,'CNO3_total.dt'))


for (iter in 1:nIter){
  
  cat(paste0('Computing iteration ', iter, '\n'))
  cat('    Train-Test Division ...')
  ####  :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::  ####
  ####                    * DIVISIÓN ENTRENAMIENTO Y TEST                    ####
  
  set.seed(seeds[iter])
  train_index <- createDataPartition(
    dataENS.dt$CNO1_AS_raw, times=1, p=0.8, list=FALSE)  
  dataENS_train.dt <- dataENS.dt[train_index]
  dataENS_test.dt  <- dataENS.dt[-train_index]
  
  cat(' ok.\n')
  cat('    Computing cost-sensitive model CS...\n')
  ####  :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::  ####
  ####                      MODELO COST-SENSITIVE C = 1.4                    ####
  
  ## ..............................  CS  .................................. ####
  # ** Ajuste del modelo de cost-sensitive random forest con c = 1.4        ####
  
  set.seed(seeds[iter])
  normalRF_CS_c1 <- ranger(
    formula = formula_CS, 
    data    = dataENS_train.dt,
    num.trees = 500,
    mtry    = 8, 
    min.node.size = 9, 
    splitrule = 'gini',
    probability = TRUE,
    importance = 'impurity'
    )
  
  # ** Cálculo de las áreas bajo la curva AUC                                    ####
  cat('      Computing AUC...')
  roc_normalRF_prob_CS_train_c1 <- roc(dataENS_train.dt$error_claseSocial, normalRF_CS_c1$predictions[, '1'])
  auc_normalRF_prob_CS_train_c1 <- auc(roc_normalRF_prob_CS_train_c1)
  
  pred_normalRF_prob_CS_c1      <- predict(normalRF_CS_c1, data = dataENS_test.dt)
  roc_normalRF_prob_CS_test_c1  <- roc(dataENS_test.dt$error_claseSocial, pred_normalRF_prob_CS_c1$predictions[, '1'])
  auc_normalRF_prob_CS_test_c1  <- auc(roc_normalRF_prob_CS_test_c1)
  
  roc_normalRF_score_CS_train_c1 <- roc(dataENS_train.dt$error_claseSocial, dataENS_train.dt$FACTORADULTO * normalRF_CS_c1$predictions[, '1'])
  auc_normalRF_score_CS_train_c1 <- auc(roc_normalRF_score_CS_train_c1)
  
  roc_normalRF_score_CS_test_c1  <- roc(dataENS_test.dt$error_claseSocial, dataENS_test.dt$FACTORADULTO * pred_normalRF_prob_CS_c1$predictions[, '1'])
  auc_normalRF_score_CS_test_c1  <- auc(roc_normalRF_score_CS_test_c1)
  
  auc_table <- cbind(data = c("train", "test", "train", "test"), 
                     rank = c("prob", "prob", "score", "score"), 
                     AUC     = round(c(auc_normalRF_prob_CS_train_c1, 
                                       auc_normalRF_prob_CS_test_c1,
                                       auc_normalRF_score_CS_train_c1, 
                                       auc_normalRF_score_CS_test_c1), 3))
  tempAUC.dt <- data.table(
    iteration = rep(as.integer(iter), 4),
    model     = 'Cost c1',
    rank      = auc_table[, 'rank'],
    dataset   = auc_table[, 'data'],
    variable  = 'CS',
    auc       = auc_table[, 'AUC']
  )
  auc.dt <- rbindlist(list(auc.dt, tempAUC.dt))
  
  roc.list[[iter]] <- list(
    train_prob  = roc_normalRF_prob_CS_train_c1, 
    test_prob   = roc_normalRF_prob_CS_test_c1,
    train_score = roc_normalRF_score_CS_train_c1, 
    test_score  = roc_normalRF_score_CS_test_c1)
  cat('ok.\n')
  
  # ** Cálculo de la importancia                                 ####
  cat('      Computing importance...')
  importance_normalRF_CS_c1 <- importance(normalRF_CS_c1)
  importance_normalRF_CS_c1.dt <- data.table(
    iteration  = rep(as.integer(iter), length(importance_normalRF_CS_c1)),
    model      = 'Cost c1',
    regressor  = names(importance_normalRF_CS_c1),
    importance = importance_normalRF_CS_c1,
    class      = factor(sapply(dataENS.dt, class)[names(importance_normalRF_CS_c1)]),
    variable   = 'CS')
  
  importance.dt <- rbindlist(list(
    importance.dt, importance_normalRF_CS_c1.dt
  ))
  cat('ok.\n')

  pthresh = 1.4/2.4

  func_score = list()
  for (i in 1:length(pred_normalRF_prob_CS_c1$predictions[, '1'])){
    if(pred_normalRF_prob_CS_c1$predictions[i, '1'] > pthresh){
      func_score[i] <- dataENS_test.dt$FACTORADULTO[i] * pred_normalRF_prob_CS_c1$predictions[i, '1']
    } else {
     func_score[i] <- dataENS_test.dt$FACTORADULTO[i] * pred_normalRF_prob_CS_c1$predictions[i, '0'] * 1.4
    }
  }
  
  func_score <- as.numeric(func_score)

  # ** Cálculo del pseudosesgo                                   ####
  cat('      Computing pseudobias (prob) ...')
  pseudobias_eval_c1.dt <- data.table(
    factor       = dataENS_test.dt$FACTORADULTO,
    prob_error   = pred_normalRF_prob_CS_c1$predictions[, '1'],
    score        = func_score,
    claseSocial_true = dataENS_test.dt$claseSocial_true,
    claseSocial_raw  = dataENS_test.dt$claseSocial_raw)
  
  pseudobias_eval_prob_c1.dt <- copy(pseudobias_eval_c1.dt)[
    , prob.rank  := frank(-prob_error, ties.method = 'min')]
  setorder(pseudobias_eval_prob_c1.dt, prob.rank)
  CS_running_total_prob_c1.dt <- data.table(
    CS_AS_running = factor(CS_levels)[0],
    ned = numeric(0),
    CS_running_total = numeric(0))
  for (i in 1:nrow(pseudobias_eval_prob_c1.dt)){
    pseudobias_eval_prob_c1.dt[
      1:i, CS_AS_running := claseSocial_true][
        is.na(CS_AS_running), CS_AS_running := claseSocial_raw]
    CS_running_total_prob_c1.dt <- rbindlist(list(
      CS_running_total_prob_c1.dt, 
      pseudobias_eval_prob_c1.dt[, list(ned = i, CS_running_total = sum(factor)), by = 'CS_AS_running']))
  }
  n <- nrow(dataENS_test.dt)
  CS_running_total_prob_c1.dt[
    , iteration := as.integer(iter)][
    , model := 'Cost c1'][
    , rank := 'prob'][
    , pseudobias := abs(CS_running_total - CS_running_total[n])/CS_running_total[n], by = 'CS_AS_running']
  setcolorder(CS_running_total_prob_c1.dt, names(CS_total_prob.dt))
  
  CS_total_prob.dt <- rbindlist(list(
    CS_total_prob.dt, CS_running_total_prob_c1.dt))
  cat('ok.\n')
  
  cat('      Computing pseudobias (score) ...')
  pseudobias_eval_score_c1.dt <- copy(pseudobias_eval_c1.dt)[
    , score.rank  := frank(-score, ties.method = 'min')]
  setorder(pseudobias_eval_score_c1.dt, score.rank)
  CS_running_total_score_c1.dt <- data.table(
    CS_AS_running = factor(CS_levels)[0],
    ned = numeric(0),
    CS_running_total = numeric(0))
  for (i in 1:nrow(pseudobias_eval_score_c1.dt)){
    pseudobias_eval_score_c1.dt[
      1:i, CS_AS_running := claseSocial_true][
        is.na(CS_AS_running), CS_AS_running := claseSocial_raw]
    CS_running_total_score_c1.dt <- rbindlist(list(
      CS_running_total_score_c1.dt, 
      pseudobias_eval_score_c1.dt[, list(ned = i, CS_running_total = sum(factor)), by = 'CS_AS_running']))
  }
  n <- nrow(dataENS_test.dt)
  CS_running_total_score_c1.dt[
    , iteration := as.integer(iter)][
    , model := 'Cost c1'][
    , rank := 'score'][
    , pseudobias := abs(CS_running_total - CS_running_total[n])/CS_running_total[n], by = 'CS_AS_running']
  
  setcolorder(CS_running_total_score_c1.dt, names(CS_total_score.dt))
  
  CS_total_score.dt <- rbindlist(list(
    CS_total_score.dt, CS_running_total_score_c1.dt))
  cat('ok.\n')
  
  ####  :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::  ####
  ####                      MODELO COST-SENSITIVE C = 2.4                    ####

  ## ..............................  CS  ..................................   ####
  # ** Ajuste del modelo de cost-sensitive random forest con c = 2.4          ####
  
  set.seed(seeds[iter])
  normalRF_CS_c10 <- ranger(
    formula = formula_CS, 
    data    = dataENS_train.dt,
    num.trees = 500,
    mtry    = 8, 
    min.node.size = 9, 
    splitrule = 'gini',
    probability = TRUE,
    importance = 'impurity'
    )
  
  # ** Cálculo de las áreas bajo la curva AUC                                    ####
  cat('      Computing AUC...')
  roc_normalRF_prob_CS_train_c10 <- roc(dataENS_train.dt$error_claseSocial, normalRF_CS_c10$predictions[, '1'])
  auc_normalRF_prob_CS_train_c10 <- auc(roc_normalRF_prob_CS_train_c10)
  
  pred_normalRF_prob_CS_c10      <- predict(normalRF_CS_c10, data = dataENS_test.dt)
  roc_normalRF_prob_CS_test_c10  <- roc(dataENS_test.dt$error_claseSocial, pred_normalRF_prob_CS_c10$predictions[, '1'])
  auc_normalRF_prob_CS_test_c10  <- auc(roc_normalRF_prob_CS_test_c10)
  
  roc_normalRF_score_CS_train_c10 <- roc(dataENS_train.dt$error_claseSocial, dataENS_train.dt$FACTORADULTO * normalRF_CS_c10$predictions[, '1'])
  auc_normalRF_score_CS_train_c10 <- auc(roc_normalRF_score_CS_train_c10)
  
  roc_normalRF_score_CS_test_c10  <- roc(dataENS_test.dt$error_claseSocial, dataENS_test.dt$FACTORADULTO * pred_normalRF_prob_CS_c10$predictions[, '1'])
  auc_normalRF_score_CS_test_c10  <- auc(roc_normalRF_score_CS_test_c10)
  
  auc_table <- cbind(data = c("train", "test", "train", "test"), 
                     rank = c("prob", "prob", "score", "score"), 
                     AUC     = round(c(auc_normalRF_prob_CS_train_c10, 
                                       auc_normalRF_prob_CS_test_c10,
                                       auc_normalRF_score_CS_train_c10, 
                                       auc_normalRF_score_CS_test_c10), 3))
  tempAUC.dt <- data.table(
    iteration = rep(as.integer(iter), 4),
    model     = 'Cost c2',
    rank      = auc_table[, 'rank'],
    dataset   = auc_table[, 'data'],
    variable  = 'CS',
    auc       = auc_table[, 'AUC']
  )
  auc.dt <- rbindlist(list(auc.dt, tempAUC.dt))
  
  roc.list[[iter]] <- list(
    train_prob  = roc_normalRF_prob_CS_train_c10, 
    test_prob   = roc_normalRF_prob_CS_test_c10,
    train_score = roc_normalRF_score_CS_train_c10, 
    test_score  = roc_normalRF_score_CS_test_c10)
  cat('ok.\n')
  
  # ** Cálculo de la importancia                                 ####
  cat('      Computing importance...')
  importance_normalRF_CS_c10 <- importance(normalRF_CS_c10)
  importance_normalRF_CS_c10.dt <- data.table(
    iteration  = rep(as.integer(iter), length(importance_normalRF_CS_c10)),
    model      = 'Cost c2',
    regressor  = names(importance_normalRF_CS_c10),
    importance = importance_normalRF_CS_c10,
    class      = factor(sapply(dataENS.dt, class)[names(importance_normalRF_CS_c10)]),
    variable   = 'CS')
  
  importance.dt <- rbindlist(list(
    importance.dt, importance_normalRF_CS_c10.dt
  ))
  cat('ok.\n')
  
  pthresh = 2.4/3.4
  for (i in 1:length(pred_normalRF_prob_CS_c10$predictions[, '1'])){
    if(pred_normalRF_prob_CS_c10$predictions[i, '1'] > pthresh){
      func_score[i] = dataENS_test.dt$FACTORADULTO[i] * pred_normalRF_prob_CS_c10$predictions[i, '1']
    } else {
      func_score[i] = dataENS_test.dt$FACTORADULTO[i] * pred_normalRF_prob_CS_c10$predictions[i, '0'] * 2.4
    }
  }
  
  func_score <- as.numeric(func_score)
  
  # ** Cálculo del pseudosesgo                                   ####
  cat('      Computing pseudobias (prob) ...')
  pseudobias_eval_c10.dt <- data.table(
    factor       = dataENS_test.dt$FACTORADULTO,
    prob_error   = pred_normalRF_prob_CS_c10$predictions[, '1'],
    score        = func_score,
    claseSocial_true = dataENS_test.dt$claseSocial_true,
    claseSocial_raw  = dataENS_test.dt$claseSocial_raw)
  
  pseudobias_eval_prob_c10.dt <- copy(pseudobias_eval_c10.dt)[
    , prob.rank  := frank(-prob_error, ties.method = 'min')]
  setorder(pseudobias_eval_prob_c10.dt, prob.rank)
  CS_running_total_prob_c10.dt <- data.table(
    CS_AS_running = factor(CS_levels)[0],
    ned = numeric(0),
    CS_running_total = numeric(0))
  for (i in 1:nrow(pseudobias_eval_prob_c10.dt)){
    pseudobias_eval_prob_c10.dt[
      1:i, CS_AS_running := claseSocial_true][
        is.na(CS_AS_running), CS_AS_running := claseSocial_raw]
    CS_running_total_prob_c10.dt <- rbindlist(list(
      CS_running_total_prob_c10.dt, 
      pseudobias_eval_prob_c10.dt[, list(ned = i, CS_running_total = sum(factor)), by = 'CS_AS_running']))
  }
  n <- nrow(dataENS_test.dt)
  CS_running_total_prob_c10.dt[
    , iteration := as.integer(iter)][
    , model := 'Cost c2'][
    , rank := 'prob'][
    , pseudobias := abs(CS_running_total - CS_running_total[n])/CS_running_total[n], by = 'CS_AS_running']
  setcolorder(CS_running_total_prob_c10.dt, names(CS_total_prob.dt))
  
  CS_total_prob.dt <- rbindlist(list(
    CS_total_prob.dt, CS_running_total_prob_c10.dt))
  cat('ok.\n')
  
  cat('      Computing pseudobias (score) ...')
  pseudobias_eval_score_c10.dt <- copy(pseudobias_eval_c10.dt)[
    , score.rank  := frank(-score, ties.method = 'min')]
  setorder(pseudobias_eval_score_c10.dt, score.rank)
  CS_running_total_score_c10.dt <- data.table(
    CS_AS_running = factor(CS_levels)[0],
    ned = numeric(0),
    CS_running_total = numeric(0))
  for (i in 1:nrow(pseudobias_eval_score_c10.dt)){
    pseudobias_eval_score_c10.dt[
      1:i, CS_AS_running := claseSocial_true][
        is.na(CS_AS_running), CS_AS_running := claseSocial_raw]
    CS_running_total_score_c10.dt <- rbindlist(list(
      CS_running_total_score_c10.dt, 
      pseudobias_eval_score_c10.dt[, list(ned = i, CS_running_total = sum(factor)), by = 'CS_AS_running']))
  }
  n <- nrow(dataENS_test.dt)
  CS_running_total_score_c10.dt[
    , iteration := as.integer(iter)][
    , model := 'Cost c2'][
    , rank := 'score'][
    , pseudobias := abs(CS_running_total - CS_running_total[n])/CS_running_total[n], by = 'CS_AS_running']
  
  setcolorder(CS_running_total_score_c10.dt, names(CS_total_score.dt))
  
  CS_total_score.dt <- rbindlist(list(
    CS_total_score.dt, CS_running_total_score_c10.dt))
  cat('ok.\n')
  
  ####  :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::  ####
  ####                      MODELO COST-SENSITIVE C = 5.7                    ####

  ## ..............................  CS  ..................................  ####
  # ** Ajuste del modelo de cost-sensitive random forest con c = 5.7         ####
  
  set.seed(seeds[iter])
  normalRF_CS_c100 <- ranger(
    formula = formula_CS, 
    data    = dataENS_train.dt,
    num.trees = 500,
    mtry    = 8, 
    min.node.size = 9, 
    splitrule = 'gini',
    probability = TRUE,
    importance = 'impurity'
    )
  
  # ** Cálculo de las áreas bajo la curva AUC                                    ####
  cat('      Computing AUC...')
  roc_normalRF_prob_CS_train_c100 <- roc(dataENS_train.dt$error_claseSocial, normalRF_CS_c100$predictions[, '1'])
  auc_normalRF_prob_CS_train_c100 <- auc(roc_normalRF_prob_CS_train_c100)
  
  pred_normalRF_prob_CS_c100      <- predict(normalRF_CS_c100, data = dataENS_test.dt)
  roc_normalRF_prob_CS_test_c100  <- roc(dataENS_test.dt$error_claseSocial, pred_normalRF_prob_CS_c100$predictions[, '1'])
  auc_normalRF_prob_CS_test_c100  <- auc(roc_normalRF_prob_CS_test_c100)
  
  roc_normalRF_score_CS_train_c100 <- roc(dataENS_train.dt$error_claseSocial, dataENS_train.dt$FACTORADULTO * normalRF_CS_c100$predictions[, '1'])
  auc_normalRF_score_CS_train_c100 <- auc(roc_normalRF_score_CS_train_c100)
  
  roc_normalRF_score_CS_test_c100  <- roc(dataENS_test.dt$error_claseSocial, dataENS_test.dt$FACTORADULTO * pred_normalRF_prob_CS_c100$predictions[, '1'])
  auc_normalRF_score_CS_test_c100  <- auc(roc_normalRF_score_CS_test_c100)
  
  auc_table <- cbind(data = c("train", "test", "train", "test"), 
                     rank = c("prob", "prob", "score", "score"), 
                     AUC     = round(c(auc_normalRF_prob_CS_train_c100, 
                                       auc_normalRF_prob_CS_test_c100,
                                       auc_normalRF_score_CS_train_c100, 
                                       auc_normalRF_score_CS_test_c100), 3))
  tempAUC.dt <- data.table(
    iteration = rep(as.integer(iter), 4),
    model     = 'Cost c3',
    rank      = auc_table[, 'rank'],
    dataset   = auc_table[, 'data'],
    variable  = 'CS',
    auc       = auc_table[, 'AUC']
  )
  auc.dt <- rbindlist(list(auc.dt, tempAUC.dt))
  
  roc.list[[iter]] <- list(
    train_prob  = roc_normalRF_prob_CS_train_c100, 
    test_prob   = roc_normalRF_prob_CS_test_c100,
    train_score = roc_normalRF_score_CS_train_c100, 
    test_score  = roc_normalRF_score_CS_test_c100)
  cat('ok.\n')
  
  # ** Cálculo de la importancia                                 ####
  cat('      Computing importance...')
  importance_normalRF_CS_c100 <- importance(normalRF_CS_c100)
  importance_normalRF_CS_c100.dt <- data.table(
    iteration  = rep(as.integer(iter), length(importance_normalRF_CS_c100)),
    model      = 'Cost c3',
    regressor  = names(importance_normalRF_CS_c100),
    importance = importance_normalRF_CS_c100,
    class      = factor(sapply(dataENS.dt, class)[names(importance_normalRF_CS_c100)]),
    variable   = 'CS')
  
  importance.dt <- rbindlist(list(
    importance.dt, importance_normalRF_CS_c100.dt
  ))
  cat('ok.\n')
  
  pthresh = 5.7/6.7
  for (i in 1:length(pred_normalRF_prob_CS_c100$predictions[, '1'])){
    if(pred_normalRF_prob_CS_c100$predictions[i, '1'] > pthresh){
      func_score[i] = dataENS_test.dt$FACTORADULTO[i] * pred_normalRF_prob_CS_c100$predictions[i, '1']
    } else {
      func_score[i] = dataENS_test.dt$FACTORADULTO[i] * pred_normalRF_prob_CS_c100$predictions[i, '0'] * 5.7
    }
  }
  
  func_score <- as.numeric(func_score)
  
  # ** Cálculo del pseudosesgo                                   ####
  cat('      Computing pseudobias (prob) ...')
  pseudobias_eval_c100.dt <- data.table(
    factor       = dataENS_test.dt$FACTORADULTO,
    prob_error   = pred_normalRF_prob_CS_c100$predictions[, '1'],
    score        = func_score,
    claseSocial_true = dataENS_test.dt$claseSocial_true,
    claseSocial_raw  = dataENS_test.dt$claseSocial_raw)
  
  pseudobias_eval_prob_c100.dt <- copy(pseudobias_eval_c100.dt)[
    , prob.rank  := frank(-prob_error, ties.method = 'min')]
  setorder(pseudobias_eval_prob_c100.dt, prob.rank)
  CS_running_total_prob_c100.dt <- data.table(
    CS_AS_running = factor(CS_levels)[0],
    ned = numeric(0),
    CS_running_total = numeric(0))
  for (i in 1:nrow(pseudobias_eval_prob_c100.dt)){
    pseudobias_eval_prob_c100.dt[
      1:i, CS_AS_running := claseSocial_true][
        is.na(CS_AS_running), CS_AS_running := claseSocial_raw]
    CS_running_total_prob_c100.dt <- rbindlist(list(
      CS_running_total_prob_c100.dt, 
      pseudobias_eval_prob_c100.dt[, list(ned = i, CS_running_total = sum(factor)), by = 'CS_AS_running']))
  }
  n <- nrow(dataENS_test.dt)
  CS_running_total_prob_c100.dt[
    , iteration := as.integer(iter)][
    , model := 'Cost c3'][
    , rank := 'prob'][
    , pseudobias := abs(CS_running_total - CS_running_total[n])/CS_running_total[n], by = 'CS_AS_running']
  setcolorder(CS_running_total_prob_c100.dt, names(CS_total_prob.dt))
  
  CS_total_prob.dt <- rbindlist(list(
    CS_total_prob.dt, CS_running_total_prob_c100.dt))
  cat('ok.\n')
  
  cat('      Computing pseudobias (score) ...')
  pseudobias_eval_score_c100.dt <- copy(pseudobias_eval_c100.dt)[
    , score.rank  := frank(-score, ties.method = 'min')]
  setorder(pseudobias_eval_score_c100.dt, score.rank)
  CS_running_total_score_c100.dt <- data.table(
    CS_AS_running = factor(CS_levels)[0],
    ned = numeric(0),
    CS_running_total = numeric(0))
  for (i in 1:nrow(pseudobias_eval_score_c100.dt)){
    pseudobias_eval_score_c100.dt[
      1:i, CS_AS_running := claseSocial_true][
        is.na(CS_AS_running), CS_AS_running := claseSocial_raw]
    CS_running_total_score_c100.dt <- rbindlist(list(
      CS_running_total_score_c100.dt, 
      pseudobias_eval_score_c100.dt[, list(ned = i, CS_running_total = sum(factor)), by = 'CS_AS_running']))
  }
  n <- nrow(dataENS_test.dt)
  CS_running_total_score_c100.dt[
    , iteration := as.integer(iter)][
    , model := 'Cost c3'][
    , rank := 'score'][
    , pseudobias := abs(CS_running_total - CS_running_total[n])/CS_running_total[n], by = 'CS_AS_running']

  setcolorder(CS_running_total_score_c100.dt, names(CS_total_score.dt))
  
  CS_total_score.dt <- rbindlist(list(
    CS_total_score.dt, CS_running_total_score_c100.dt))
  cat('ok.\n')
  
  CS_total.dt <- rbindlist(list(
    CS_total_prob.dt, CS_total_score.dt))  
}


# saveRDS(auc.dt, file = file.path(path_data, 'auc.dt'))
# saveRDS(importance.dt, file = file.path(path_data, 'importance.dt'))
# saveRDS(CS_total_prob.dt, file = file.path(path_data, 'CS_total_prob.dt'))
# saveRDS(CS_total_score.dt, file = file.path(path_data, 'CS_total_score.dt'))
# saveRDS(CS_total.dt, file = file.path(path_data, 'CS_total.dt'))
