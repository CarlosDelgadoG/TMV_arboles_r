
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
                                  outcome ~ mass + pregnant + glucose+age,
                                  resamples=vfold_cv(diab_train,v=3),
                                  grid = grilla_boost,
                                  metrics=metric_set(roc_auc,specificity,sensitivity)) 

collect_metrics(resultados_boost_tune)
# Seleccionar parámetros --------------------------------------------------


# Estimar el mejor modelo -------------------------------------------------
boost_final <- finalize_model(boost_tune,
                              select_best(resultados_boost_tune))%>%
                fit(outcome ~ mass + pregnant + glucose+age,
                    diab_train)

