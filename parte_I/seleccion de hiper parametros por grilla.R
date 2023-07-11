diabetes <- readRDS("datos/diabetes.RDS")


# HACER SPLIT -------------------------------------------------------------
set.seed(2023)

diabetes_split <- initial_split(diabetes, prop=.75, strata = diabetes)

diab_train <- training(diabetes_split)
diab_test <- testing(diabetes_split)
diab_fold<- vfold_cv(diab_train,v=3)

# TUNE --------------------------------------------------------------------

#Busca identificar los parámetros que se quieren ajustar

boost_tune <- boost_tree(trees=100,
                         learn_rate = tune(),
                         tree_depth = tune(),
                         sample_size = tune())%>%
              set_mode("classification")%>%
              set_engine("xgboost")



# Generar grilla ----------------------------------------------------------
grilla_boost <- grid_regular(parameters(boost_tune),
                             levels = 2)

# Iterar sobre la grilla -----------------------------------------------------------
#Dentro de la función se declaran los folds
resultados_boost_tune <-tune_grid(boost_tune,
                                  diabetes~.,
                                  resamples=diab_fold,
                                  grid = grilla_boost,
                                  metrics=metric_set(roc_auc,accuracy,sensitivity)) 

collect_metrics(resultados_boost_tune)

autoplot(resultados_boost_tune)
# Seleccionar parámetros --------------------------------------------------


# Estimar el mejor modelo -------------------------------------------------
boost_final <- finalize_model(boost_tune,
                              select_best(resultados_boost_tune))%>%
                fit(diabetes ~.,
                    diab_train)

