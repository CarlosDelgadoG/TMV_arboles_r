library(mlbench)

data(PimaIndiansDiabetes2)

diabetes<- PimaIndiansDiabetes2%>%
  mutate(diabetes= factor(case_match(diabetes, "pos" ~"Positivo",
                                     "neg"~"Negativo"),
                          levels=c("Positivo","Negativo")))%>%
  select(-c(triceps,insulin))%>%
  drop_na()

saveRDS(diabetes,file="datos/diabetes.RDS")