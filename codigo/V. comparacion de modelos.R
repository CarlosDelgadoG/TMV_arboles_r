diabetes <- readRDS("datos/diabetes.RDS")


# HACER SPLIT -------------------------------------------------------------
set.seed(2023)

diabetes_split <- initial_split(diabetes, prop=.75, strata = diabetes)

diab_train <- training(diabetes_split)
diab_test <- testing(diabetes_split)



# HACEMOS LAS ESPECIFICACIONES DE LOS MODELOS -----------------------------


tree_spec <- decision_tree()%>%
            set_mode("classification")%>%
            set_engine("rpart")

rf_spec <- rand_forest()%>%
            set_mode("classification")%>%
            set_engine("ranger")

boost_spec <-   boost_tree()%>%
  set_mode("classification")%>%
  set_engine("xgboost")



# Lista de especificaciones con nombre ------------------------------------



lista_specs <- list(tree_spec,rf_spec,boost_spec)
names(lista_specs)<- c("arbol","rf","boost")


# AJUSTE Y PREDICCION -----------------------------------------------------

# El ajuste mediante la función fit() se hace sobre el conjunto de entrenamiento
#Las predicciones se hacen para el conjunto de testeo

preds_modelos <-lapply(1:length(lista_specs),
       function(i){
#Iterar sobre cada especificacion
        preds=  lista_specs[[i]]%>%
#Ajustar el modelo
           fit(diabetes ~ ., data = diab_train) %>%
#Predecir el conjunto de testeo
           predict(new_data = diab_test, type = "prob")%>%
#Seleccionamos la columna de predicciones
          select(.pred_Positivo)
#Cambiamos el nombre de cada prediccion al modelo correspondiente        
names(preds)<-names(lista_specs)[i]
          
          return(preds)})%>%
#Combinamos las predicciones de los modelos con los valores reales obtenidos
  bind_cols(select(diab_test,diabetes))


lapply(1:length(lista_specs),function(i){
# Generamos el auc para cada modelo
  roc_auc(preds_modelos,
          truth = diabetes,
          estimate =!!rlang::sym(names(lista_specs)[i]) )%>%
    mutate(modelo=names(lista_specs)[i])})%>%
  bind_rows()


