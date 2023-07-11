library(tidyverse)

sjmisc::frq(diab_train$diabetes)

sjmisc::frq(filter(diab_train, glucose >=144)$diabetes)

sjmisc::frq(filter(diab_train, age  >=29)$diabetes)

sjmisc::frq(filter(diab_train, pedigree  >=.34)$diabetes)


sjmisc::frq(filter(diab_train, glucose >=144,pedigree >=.34)$diabetes)

sjmisc::frq(filter(diab_train, age <30)$diabetes)


