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