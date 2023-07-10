library(tidymodels)
library(rpart.plot)
library(baguette)
library(xgboost)

set.seed(2023)

# HACER SPLIT -------------------------------------------------------------
diabetes_split <- initial_split(diabetes, prop=.75, strata = outcome)

diab_train <- training(diabetes_split)
diab_test <- testing(diabetes_split)



# Especificar el modelo ---------------------------------------------------



tree_spec <- decision_tree() %>% #Escoger una clase de modelo
  set_engine("rpart")%>% #Escoger un engine
  set_mode("classification") #Escoger un modo

modelo_arbol=tree_spec %>% 
  fit(formula=diabetes~.,
      data=diab_train)

#Visualizar arbol

print(modelo_arbol)
rpart.plot(modelo_arbol$fit)

# HACER PREDICCIONES ------------------------------------------------------
predicciones= predict(modelo_arbol,testing(diabetes_split))%>%
              mutate(outcome=testing(diabetes_split)$outcome)




# MATRIZ DE CONFUSION -----------------------------------------------------

caret::confusionMatrix(table(predicciones$.pred_class,predicciones$outcome))
# Accuracy: De todas las predicciones cuantas fueron correctas 
# Senstividad: De todos los valores positivos cuantos fueron correctamente identificados
# Espeficidad: De todos los valores negativos encontrados, cuantes fueron correctamente identificados
# AUC: 0,5 indica que el modelo no es mejor que el azar

# VALIDACION CRUZADA ------------------------------------------------------

set.seed(2022)

diab_fold <- vfold_cv(diab_train,3)


fits_cv= fit_resamples(tree_spec,
              outcome~mass,
              resamples=diab_fold,
              metrics = metric_set(accuracy,sensitivity,specificity))



collect_metrics(fits_cv)



# Grilla de parametros ----------------------------------------------------

### Crear especificacion con hiperparametros "libres"
arbol_untune <- decision_tree(tree_depth = tune(),
                              min_n = tune())%>%
                set_engine("rpart")%>%
                set_mode("classification")


grilla <- expand.grid(tree_depth=c(5,10,15),
                      min_n=c(10,20,30))



# Tune along the grid
tune_results <- tune_grid(arbol_untune, 
                          outcome~mass+pregnant,
                          resamples = diab_fold,
                          grid = grilla,
                          metrics = metric_set(roc_auc, sens, spec))



collect_metrics(tune_results) 




autoplot(tune_results)


mejor_modelo <- finalize_model(arbol_untune, select_best(tune_results))%>%
                fit(outcome~mass+pregnant,
                    diabetes)



rpart.plot(mejor_modelo$fit)



