library(tidymodels)
library(mlbench)

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


tree_spec %>% 
  fit(formula=outcome~mass,
      data=diab_train)

