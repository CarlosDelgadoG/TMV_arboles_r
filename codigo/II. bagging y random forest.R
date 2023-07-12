diabetes <- readRDS("datos/diabetes.RDS")


# HACER SPLIT -------------------------------------------------------------
set.seed(2023)

diabetes_split <- initial_split(diabetes, prop=.75, strata = diabetes)

diab_train <- training(diabetes_split)
diab_test <- testing(diabetes_split)

diab_fold<-  vfold_cv(diab_train,3)



# BAGGING -----------------------------------------------------------------


spec_bagged <- bag_tree()%>%
  set_mode("classification")%>%
  set_engine("rpart",times=100)

model_bagged <- fit(spec_bagged,
                    diabetes~.,
                    diab_train)


cv_bagg <- fit_resamples(spec_bagged,
                         diabetes ~ ., 
                         resamples =diab_fold,
                         metrics = metric_set(roc_auc,accuracy,sensitivity))

# Collect metrics
collect_metrics(cv_bagg)



# RANDOM FOREST -----------------------------------------------------------

spec_forest <- rand_forest()%>%
  set_mode("classification")%>%
  set_engine("ranger")



cv_forest <- fit_resamples(spec_forest,
                           diabetes ~ ., 
                           resamples = vfold_cv(diab_train,3),
                           metrics =metric_set(roc_auc,accuracy,sensitivity),
                           control= control_resamples(save_pred = TRUE, save_workflow = TRUE))

collect_metrics(cv_forest)

