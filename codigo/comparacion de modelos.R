diabetes <- readRDS("datos/diabetes.RDS")


# HACER SPLIT -------------------------------------------------------------
set.seed(2023)

diabetes_split <- initial_split(diabetes, prop=.75, strata = diabetes)

diab_train <- training(diabetes_split)
diab_test <- testing(diabetes_split)


tree_spec <- decision_tree()%>%
            set_mode("classification")%>%
            set_engine("rpart")

rf_spec <- rand_forest()%>%
            set_mode("classification")%>%
            set_engine("ranger")

boost_spec <-   boost_tree()%>%
  set_mode("classification")%>%
  set_engine("xgboost")

lista_specs <- list(tree_spec,rf_spec,boost_spec)
names(lista_specs)<- c("arbol","rf","boost")

preds_modelos <-lapply(1:length(lista_specs),
       function(i){
        preds=  lista_specs[[i]]%>%
           fit(diabetes ~ ., data = diab_train) %>%
           predict(new_data = diab_test, type = "prob")%>%
          select(.pred_pos)
        
        names(preds)<-names(lista_specs)[i]
          
          return(preds)})%>%
  bind_cols(select(diab_test,diabetes))


lapply(1:length(lista_specs),function(i){
  roc_auc(preds_modelos,
          truth = diabetes,
          estimate =!!rlang::sym(names(lista_specs)[i]) )%>%
    mutate(modelo=names(lista_specs)[i])})%>%
  bind_rows()


