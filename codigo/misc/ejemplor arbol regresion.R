library(tidymodels)
library(mlbench)

data(PimaIndiansDiabetes2)
diabetes<- PimaIndiansDiabetes2



tree_fit  <- decision_tree() %>% #Escoger una clase de modelo
  set_engine("rpart")%>% #Escoger un engine
  set_mode("regression")%>%
  fit(mass~glucose)#Escoger un modo

# Parametros importantes:
# min_n: Número de observaciones en un nodo antes de ser partido
#tree_depth: Número máximo de particiones que son permitidas para un árbol