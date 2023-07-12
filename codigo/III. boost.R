diabetes <- readRDS("datos/diabetes.RDS")


# HACER SPLIT -------------------------------------------------------------
set.seed(2023)

diabetes_split <- initial_split(diabetes, prop=.75, strata = diabetes)

diab_train <- training(diabetes_split)
diab_test <- testing(diabetes_split)

diab_fold<-  vfold_cv(diab_train,3)


boost_spec <- boost_tree()%>%
  set_mode("classification")%>%
  set_engine("xgboost")

modelo_boost <- fit(boost_spec,
                    diabetes~.,
                    diab_train)

cv_boost <- fit_resamples(boost_spec,
                          diabetes ~ ., 
                          resamples =diab_fold,
                          metrics = metric_set(roc_auc,accuracy,sensitivity),
                          control= control_resamples(save_pred = TRUE, save_workflow = TRUE))

collect_metrics(cv_boost)