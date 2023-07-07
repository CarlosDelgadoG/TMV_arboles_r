library(tidymodels)
library(mlbench)
library(rpart.plot)

data(PimaIndiansDiabetes2)
diabetes<- PimaIndiansDiabetes2%>%
  mutate(outcome= relevel(diabetes, ref = "pos"))



tree_spec <- decision_tree() %>% #Escoger una clase de modelo
  set_engine("rpart")%>% #Escoger un engine
  set_mode("classification") #Escoger un modo

tree_model_mass <- tree_spec %>% 
  fit(formula=outcome~mass,
      data=diabetes)


# HACER SPLIT -------------------------------------------------------------
set.seed(2023)

diabetes_split <- initial_split(diabetes, prop=.75, strata = outcome)

diab_train <- training(diabetes_split)
diab_test <- testing(diabetes_split)


modelo_arbol=tree_spec %>% 
  fit(formula=outcome~mass,
      data=diab_train)

#Visualizar arbol

print(modelo_arbol)
rpart.plot(modelo_arbol$fit)

# HACER PREDICCIONES ------------------------------------------------------
predicciones= predict(modelo_arbol,testing(diabetes_split))%>%
              mutate(outcome=testing(diabetes_split)$outcome)




# MATRIZ DE CONFUSION -----------------------------------------------------

caret::confusionMatrix(table(predicciones$outcome,predicciones$.pred_class))
# Accuracy: De todas las predicciones cuantas fueron correctas 
# Senstividad: De las predicciones positivas cuantas fueron correctas
# Especificidad: De las predicciones negativas cuantas fueron correctas