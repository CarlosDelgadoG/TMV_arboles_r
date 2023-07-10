library(mlbench)

data(PimaIndiansDiabetes2)

diabetes<- PimaIndiansDiabetes2%>%
  mutate(diabetes= relevel(diabetes, ref = "pos"))%>%
  select(-c(triceps,insulin))%>%
  drop_na()

saveRDS(diabetes,file="datos/diabetes.RDS")