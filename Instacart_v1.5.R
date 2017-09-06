setwd("C:/Users/prave/Downloads/Praveen/UConn/Predictive modeling/My Learnings/Instacart")
getwd()

#====================================== Reading files and preparing prior products dataset =======================================
library(ff)
library(ffbase)
library(data.table)
library(dplyr)
library(caret)

aisle_dept_info = read.csv.ffdf(file="products.csv")

order_products_prior = read.csv.ffdf(file="order_products__prior.csv")
# View(order_products_prior[1:100,])

order_products__train = read.csv.ffdf(file="order_products__train.csv")


orders = read.csv.ffdf(file="orders.csv")

train_orders_index = ffwhich(orders,orders$eval_set=='train')
train_orders = orders[train_orders_index,]
# View(train_orders)

test_orders_index = ffwhich(orders,orders$eval_set=='test')
test_orders = orders[test_orders_index, ]

prior_orders_index = ffwhich(orders,orders$eval_set=='prior')
prior_orders = orders[prior_orders_index,]

# length(unique(products$product_id))  #total no.of products = 49,688 

# This large dataset (32 million records) contains both train and test user_ids, and their previously bought products
all_prior_prods = merge.ffdf(y=prior_orders, x=order_products_prior, by.x ="order_id", by.y ="order_id" )
# View(all_prior_prods[all_prior_prods$user_id %in% c(8,23),])

#sorting based on the below mentioned features
sorted_index = ffdforder(all_prior_prods[c("user_id","product_id","order_number")])
all_prior_prods = all_prior_prods[sorted_index,]

#============================================ Feature Engineering =================================================================

prior_prods_new_features = as.data.table(all_prior_prods)
prior_prods_new_features$days_since_prior_order = ifelse(is.na(prior_prods_new_features$days_since_prior_order),-1,prior_prods_new_features$days_since_prior_order)

# optimizing data.table functions for faster performance
options(datatable.optimize = 2L)

prior_prods_new_features = prior_prods_new_features[, total_no_of_orders_per_user:=as.numeric(max(order_number)),  by=(user_id)]

# prior_prods_new_features=prior_prods_new_features[prior_prods_new_features$user_id %in% c(8,23)]

gc()
memory.limit() 
# memory.limit(size=9000)


options(datatable.optimize = 2L)

# preparing few aggregated and new features
prior_prods_new_features= prior_prods_new_features[, ':=' (
  times_each_product_bought=as.numeric(.N),
  
  avg_hr_of_prd_bought=as.numeric(mean(order_hour_of_day)),

  avg_dow_prd_bought=as.numeric(mean(order_dow)), 

  avg_prd_cart_ord=as.numeric(mean(add_to_cart_order)), 
  
  avg_days_since_prior_ord=as.numeric(mean(days_since_prior_order)),
  
  days_since_last_order=as.numeric(last(days_since_prior_order)),
  
  num_back_to_back_buys=as.numeric(sum(diff(order_number)[diff(order_number)==1])),

  is_last_buy_bk_2_bk=as.numeric(ifelse(last(diff(order_number))==1,1,0 )),
  
  most_recent_buy=as.numeric(ifelse((max(order_number)/total_no_of_orders_per_user)==1,1,0))),
  
  by=list(user_id,product_id)]

# gc()
# memory.limit() 
# memory.limit(size=56000)# Assign more memory when needed
# memory.size() # memmory R is using



# options(java.parameters = "- Xmx9024m")

#write.csv(prior_prods_new_features,"prior_prods_new_features.csv")

prior_prods_new_features$is_last_buy_bk_2_bk = ifelse(is.na(prior_prods_new_features$is_last_buy_bk_2_bk),0,prior_prods_new_features$is_last_buy_bk_2_bk)

prior_prods_new_features$percent_chance_of_buying = round((prior_prods_new_features$times_each_product_bought/prior_prods_new_features$total_no_of_orders_per_user)*100,0)

prior_prods_new_features$avg_hr_of_prd_bought = round(prior_prods_new_features$avg_hr_of_prd_bought,0)

prior_prods_new_features$avg_dow_prd_bought = round(prior_prods_new_features$avg_dow_prd_bought,0)

prior_prods_new_features$avg_prd_cart_ord = round(prior_prods_new_features$avg_prd_cart_ord,0)

prior_prods_new_features$avg_days_since_prior_ord = round(prior_prods_new_features$avg_days_since_prior_ord,0)

prior_prods_new_features$percent_chance_of_buying = round(prior_prods_new_features$percent_chance_of_buying,0)

prior_prods_new_features_ffdf = as.ffdf(prior_prods_new_features[,-c(3,4,6:10)])
rm(prior_prods_new_features)

prior_prods_new_features_ffdf = merge.ffdf(x=prior_prods_new_features_ffdf,y=aisle_dept_info,by.x = "product_id",by.y = "product_id")
prior_prods_new_features_ffdf = as.ffdf(prior_prods_new_features_ffdf[,-c(15)])

# View(prior_prods_new_features[1:100,])

# write.csv.ffdf(prior_prods_new_features_ffdf,"prior_prods_new_features_ffdf.csv")

# View(prior_prods_new_features_ffdf[1:100,])
#================================ training and test set prep ======================================
#=============== training set prep===============================

# Extracting train set records from prior_prods_new_features_ffdf
train_set_prior_prods = prior_prods_new_features_ffdf[prior_prods_new_features_ffdf$user_id %in% train_orders$user_id ,] 
# View(train_set_prior_prods[1:100,])

# preparing distinct train set records
train_set_prior_prods = as.ffdf(train_set_prior_prods[,-c(1)] %>% as_tibble() %>% distinct())

#joining train orders with train set prior orders on user_id, now only reordered column is left out
train_orders_all = merge.ffdf(y=train_orders,x=train_set_prior_prods,
                              by.x = "user_id",by.y = "user_id")
# View(train_orders_all[1:100,])
#obtaining only reordered train data
reordered_train_prods = order_products__train[order_products__train$reordered==1,]
# class(train_reorders)
View(reordered_train_prods[1:100,])
#get user_id,prod_id, reordered info for train prods
train_reorders = merge.ffdf(y=train_orders, x=reordered_train_prods, by.x ="order_id", by.y ="order_id" )
train_reorders = as.ffdf(train_reorders[,c("user_id","product_id","reordered")])
# train_reorders = as.ffdf(train_reorders[train_reorders$user_id %in% c(8,23),])
# View(train_reorders[1:100,])

# class(train_orders_all)
# class(training_set)
training_set = merge.ffdf(y=train_reorders,x=train_orders_all,by=c("user_id","product_id"),all.x = T)
# View(training_set[1:100,])

# train_reorders[train_reorders$product_id==10326,]

training_set[,c("reordered")] = ifelse(is.na(training_set[,c("reordered")]),0,training_set[,c("reordered")])


# View(training_set[1:100,])
# Removing unnecessary columns
training_set = as.data.table(training_set[,-c(16:18)])
# training_set = as.data.table(training_set)
training_set$reordered = as.factor(training_set$reordered)

# View(subset(training_set,training_set$user_id %in% c(8,23)))

# training_set = as.data.frame(training_set)
# View(training_set[1:100,])

write.csv(training_set_ffdf,"training_set_ffdf.csv")
#

#================================================= Model prep ==================================

#split final train dataset into train and test datasets
library(caTools)
set.seed(12312)
split = sample.split(training_set$reordered, SplitRatio = 0.6)

ds_train = subset(training_set, split==T)

ds_test = subset(training_set, split==F)

# creating validation and test sets
split_2 = createFolds(ds_test$reordered, k=2, list=FALSE)
ds_validation = subset(ds_test, split_2 == 1)
ds_test = subset(ds_test, split_2 == 2)

# ====================================== Initial Rough Model ================================================

#============== XGBoost ============== 
library(xgboost)

params = list (
  subsample = 0.7, 
  stratified = T,
  # min_child_weight=2,
  colsample_bytree=0.9,
  lambda=5,
  # lambda_bias=1,
  eta=1, 
  gamma=2,
  # max_depth=3,
  max_delta_step=5,
  objective = "multi:softprob",             
  num_class=2,
  eval_metric = "merror"
)
  
xgb_cv = xgb.cv(data = as.matrix(ds_train[,-c("reordered")],with=F), params = params,nrounds = 15,
                nfold = 2, early_stopping_rounds = 3,label = as.matrix(ds_train$reordered))

# train error is very small, probably because of huge imbalance in the class (ie, in reordered variable)
# Hence, creating a customized evaluation error to calculate Fscore, which considers the huge 
# imbalance in the class and penalizes appropriately.

#xgb.train
xgb_train_ds= xgb.DMatrix(as.matrix(ds_train[,-c("reordered"),with=F]), label=as.matrix(ds_train[,c("reordered")]))
xgb_val_ds= xgb.DMatrix(as.matrix(ds_validation[,-c("reordered"),with=F]),label=as.matrix(ds_validation[,c("reordered")]))

# custom evaluation error function
Fscore_eval = function(pred,dtrain) {
  train_labels = getinfo(dtrain,"label");
  
  pred_matrix = matrix(pred, ncol = 2, byrow = T);
  
  pred_mod = ifelse(pred_matrix[,1]>=0.5,'0','1');
  
  # print(class(train_labels));
  # print(class(pred));
  # print(paste("length of train labels : ",length(train_labels)));
  # print(paste("length of pred : ",length(pred_mod)));
  cm = table(act=as.factor(as.character(train_labels)), pred=as.factor((pred_mod)));
  # print(cm)
  # precision = true pos/(all pred pos) 
  precision = cm[2,2]/(cm[1,2]+cm[2,2]);
  # recall =  true pos/(all actual pos)
  recall = cm[2,2]/(cm[2,1]+cm[2,2]);
  
  FScore = as.numeric(2*(precision*recall)/(precision + recall));
  return(list(metric = "error", value = FScore))
}

params = list (
  subsample = 0.7, 
  stratified = T,
  # min_child_weight=2,
  colsample_bytree=0.9,
  lambda=5,
  # lambda_bias=1,
  eta=1, 
  gamma=2,
  # max_depth=3,
  max_delta_step=5,
  objective = "multi:softprob", 
  num_class=2
  )

xgboost_model = xgb.train(params, xgb_train_ds, nrounds = 5, 
                            watchlist=list(train = xgb_train_ds, validation=xgb_val_ds),
                           #eval_metric = Fscore_eval)
                           feval = Fscore_eval, maximize = TRUE)
# Significant features
xgb_imp_mx = xgb.importance(names(ds_train)[-c(19)], model = xgboost_model)

(gg <- xgb.ggplot.importance(xgb_imp_mx, measure = "Frequency", rel_to_first = TRUE)) 
gg + ggplot2::ylab("Frequency")


# ******************** predictions on test set ********************************

y_pred_xgb_val = predict(xgboost_model, newdata = as.matrix(ds_test[,-c("reordered"),with=F]) )

y_pred_xgb_matrix_val = matrix(y_pred_xgb_val, ncol = 2, byrow = T)

y_pred_xgb_val_values = ifelse(y_pred_xgb_matrix_val[,1]>=0.5,'0','1')


#Validating results
cm = table(act=ds_test$reordered, pred=y_pred_xgb_val_values)
error_rate = (cm[1,2]+cm[2,1])/(cm[1,2]+cm[2,1] + cm[1,1]+cm[2,2])

# precision = true pos/(all pred pos) 
precision = cm[2,2]/(cm[1,2]+cm[2,2])
# recall =  true pos/(all actual pos)
recall = cm[2,2]/(cm[2,1]+cm[2,2])

FScore_test_set = 2*(precision*recall)/(precision + recall)

#Test Results
#1 train Fscore = 0.2708  validation Fscore = 0.2692
#2 test set Fscore = 0.2713


#========================================= Actual test set prep=========================================
# Extracting actual test set data   
test_set_prior_prods = prior_prods_new_features_ffdf[prior_prods_new_features_ffdf$user_id %in% test_orders$user_id, ] 
# test_set_prior_idx = ffwhich(prior_prods_new_features_ffdf,prior_prods_new_features_ffdf$user_id %in% test_orders$user_id)
# test_set_prior_prods = prior_prods_new_features_ffdf[test_set_prior_idx,-c(1,3,4,6:10)]

# View(test_set_prior_prods[1:100,])

# preparing distinct test dataset
test_set_prior_prods = as.ffdf(test_set_prior_prods[,-c(1)] %>% as_tibble() %>% distinct())

#joining test orders with test set prior orders on user_id
test_orders_all = merge.ffdf(y=test_orders,x=test_set_prior_prods,
                             by.x = "user_id",by.y = "user_id")
# View(test_orders_all[1:100,])

#removing eval_set columns(26,27)
test_orders_all = data.table(test_orders_all[,c("order_id")],test_orders_all[,-c(16,17)])


# colnames(ds_test)
# colnames(test_orders_all)


y_pred_xgb_final_test = predict(xgboost_model, newdata = as.matrix(test_orders_all) )

y_pred_xgb_matrix_final_test = matrix(y_pred_xgb_final_test, ncol = 2, byrow = T)

y_pred_xgb_values = ifelse(y_pred_xgb_matrix_final_test[,1]>=0.5,0,1)

pred_results_on_final_test_set = data.table(test_orders_all[,c(1:3)],pred=y_pred_xgb_values)
# View(pred_results_on_final_test_set[pred_results_on_final_test_set$pred==1,])

pred_results_on_final_test_set$pred_reordered = as.numeric(pred_results_on_final_test_set$pred)

#only predicted reordered test set
pred_results_on_final_test_set = pred_results_on_final_test_set[pred_results_on_final_test_set$pred==1,]
write.csv(pred_results_on_final_test_set,"final_pred_values.csv")

#============================== submission file prep ============================================
pred_results_on_final_test_set = pred_results_on_final_test_set %>% group_by(V1) %>% 
  summarise( products = paste(product_id, collapse = " ") )

pred_results_on_final_test_set = setNames(pred_results_on_final_test_set,c("order_id","products"))
pred_results_on_final_test_set = as.ffdf(pred_results_on_final_test_set)


# Remaining test orders that weren't reordered
missing_test_orders = test_orders[(test_orders$order_id %in% unique(as.ff(pred_results_on_final_test_set$order_id))) == F, ]
missing_test_orders = as.data.table(missing_test_orders)

#hardcoding missing test orders that weren't reordered
missing_products = rep_len("None",nrow(missing_test_orders))

# missing test orders with hardcoded products
missing_test_orders = data.table(missing_test_orders[,c("order_id")],products=(missing_products))

# length(missing_test_orders$order_id)
# View(pred_results_on_final_test_set[1:100,])

# class(pred_results_on_final_test_set)
submission_file = setNames(data.frame(rbind(pred_results_on_final_test_set,missing_test_orders)), c("order_id","products")) %>% arrange(order_id)

write.csv(submission_file,"submission_file_v0.1.csv")

