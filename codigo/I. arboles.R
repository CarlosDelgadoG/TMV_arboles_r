library(tidymodels)
library(rpart.plot)
library(baguette)
library(xgboost)

set.seed(2023)

diabetes <- readRDS("datos/diabetes.RDS")
# HACER SPLIT -------------------------------------------------------------
diabetes_split <- initial_split(diabetes, prop=.75, strata = diabetes)

diab_train <- training(diabetes_split)
diab_test <- testing(diabetes_split)



# Especificar el modelo ---------------------------------------------------
tree_spec<- decision_tree() %>% #Escoger una clase de modelo
  set_engine("rpart")%>% #Escoger un engine
  set_mode("classification")


modelo_arbol%>% 
  fit(formula=diabetes~.,
      data=diab_train)

#Visualizar arbol

print(modelo_arbol)
rpart.plot(modelo_arbol$fit,extra=2)

# HACER PREDICCIONES ------------------------------------------------------
predicciones= list(predict(modelo_arbol,diab_train),predict(modelo_arbol,diab_train,type="prob"))%>%
              bind_cols(diab_train)


accuracy(predicciones,estimate=.pred_class,truth=diabetes)
sensitivity(predicciones,estimate=.pred_class,truth=diabetes)

roc_auc(predicciones,estimate=.pred_Positivo,truth=diabetes)

# MATRIZ DE CONFUSION -----------------------------------------------------

caret::confusionMatrix(table(predicciones$.pred_class,predicciones$diabetes))
# Accuracy: De todas las predicciones cuantas fueron correctas 
# Senstividad: De todos los valores positivos cuantos fueron correctamente identificados
# Espeficidad: De todos los valores negativos encontrados, cuantes fueron correctamente identificados
# AUC: 0,5 indica que el modelo no es mejor que el azar

# VALIDACION CRUZADA ------------------------------------------------------

set.seed(2022)

diab_fold <- vfold_cv(diab_train,3)


fits_cv= fit_resamples(tree_spec,
              diabetes~.,
              resamples=diab_fold,
              metrics = metric_set(roc_auc,accuracy, sensitivity))



collect_metrics(fits_cv)



# Grilla de parametros ----------------------------------------------------

### Crear especificacion con hiperparametros "libres"
arbol_untune <- decision_tree(tree_depth = tune(),
                              min_n = tune())%>%
                set_engine("rpart")%>%
                set_mode("classification")


grilla <- expand.grid(tree_depth=c(5,10,15),
                      min_n=c(10,20,30))




tune_results <- tune_grid(arbol_untune, 
                          diabetes~.,
                          resamples = diab_fold,
                          grid = grilla,
                          metrics = metric_set(roc_auc,accuracy,sensitivity))



collect_metrics(tune_results) 




autoplot(tune_results)


mejor_modelo <- finalize_model(arbol_untune, select_best(tune_results))%>%
                fit(outcome~mass+pregnant,
                    diabetes)



rpart.plot(mejor_modelo$fit)


