print("#######################################Running Modelling Steps###############################################################")

##Create a duplicate Dealer Code Variable(DC)

data$DC <- data$Dealer_Code

print("Dummy Encoding!")

names(data)

cols_to_dummy_encode <- "Dealer_Code"

data <- as.data.table(dummy.data.frame(data , names = cols_to_dummy_encode , sep = "_"))

names(data)

print("Dummy Encoding Done!")

names(data)

print("Creating flag variables from PL column")

dim(data)

##Creation of flags
head(data$C0_Date)
data$C0_Date <- as.Date(data$C0_Date , format = "%Y-%m-%d")
data$XL_flag <- ifelse(grepl(pattern = "XL",ignore.case = T,x = data$PL),1,0)
data$FBV_flag <- ifelse(grepl(pattern = "FBV",ignore.case = T,x = data$PL),1,0)
data$HT_flag <- ifelse(grepl(pattern = "HT",ignore.case = T,x = data$PL),1,0)
data$CC_207_flag <- ifelse(grepl(pattern = "207",ignore.case = T,x = data$PL),1,0)
data$CC_flag <- ifelse(grepl(pattern = "CC",ignore.case = T,x = data$PL),1,0)
data$CNG_flag <- ifelse(grepl(pattern = "CNG",ignore.case = T,x = data$PL),1,0)
data$DI_flag <- ifelse(grepl(pattern = "DI",ignore.case = T,x = data$PL),1,0)
data$EX_flag <- ifelse(grepl(pattern = "EX",ignore.case = T,x = data$PL),1,0)
data$RX_flag <- ifelse(grepl(pattern = "RX",ignore.case = T,x = data$PL),1,0)
data$yodha_flag <- ifelse(grepl(pattern = "yodha",ignore.case = T,x = data$PL),1,0)
data$xenon_flag <- ifelse(grepl(pattern = "xenon",ignore.case = T,x = data$PL),1,0)
data$mint_flag <- ifelse(grepl(pattern = "mint",ignore.case = T,x = data$PL),1,0)
data$gold_flag <- ifelse(grepl(pattern = "gold",ignore.case = T,x = data$PL),1,0)

##Binary encoding target variable
data$Status <- ifelse(data$Status %in% c("Converted"),1,0)

prop.table(table(data$Status))

print("Splitting into train , test and validation!")

##Specifying train,test and validation periods
train_start_period = 201807
train_end_period = 201906
validation_1_period = 201907
validation_2_period = 201908
train_test_split_ratio = 0.8

##Setting seed
set.seed(1)

data_1 <- data[data$year_month >= train_start_period & data$year_month <= train_end_period,]

split <- sample.split(data_1$DC , SplitRatio = train_test_split_ratio)

train <- data_1[split == TRUE,]
test <- data_1[split == FALSE,]

dim(train)
dim(test)

validation_1 <- data[data$year_month == validation_1_period,]
validation_2 <- data[data$year_month == validation_2_period,]

table(train$Status)
prop.table(table(train$Status))

table(test$Status)
prop.table(table(test$Status))

table(validation_1$Status)
prop.table(table(validation_1$Status))

table(validation_2$Status)
prop.table(table(validation_2$Status))

print("Getting Dealer Level train , test , validation event rates!")

DW_TR_TS_VAL <- do.call("bind_rows" , list(mutate(train , set = "train")
                                                  ,mutate(test , set = "test")
                                                  ,mutate(validation_1 , set = "validation_1")
                                                  ,mutate(validation_2 , set = "validation_2"))
)

DW_TR_TS_VAL_ER <- DW_TR_TS_VAL%>%mutate(train_flag = ifelse(set %in% c("train") , 1 , 0)
                         ,test_flag = ifelse(set %in% c("test") , 1 , 0)
                         ,validation_1_flag = ifelse(set %in% c("validation_1") , 1 , 0)
                         ,validation_2_flag = ifelse(set %in% c("validation_2") , 1 , 0)
                         ,train_event_flag = ifelse((set %in% c("train") & Status == 1) , 1 , 0)
                         ,test_event_flag = ifelse((set %in% c("test") & Status == 1) , 1 , 0)
                         ,validation_1_event_flag = ifelse((set %in% c("validation_1") & Status == 1) , 1 , 0)
                         ,validation_2_event_flag = ifelse((set %in% c("validation_2") & Status == 1) , 1 , 0))%>%
  group_by(DC)%>%summarise(train_leads = sum(train_flag) , test_leads = sum(test_flag) , val1_leads = sum(validation_1_flag)
                           ,val2_leads = sum(validation_2_flag) , train_events = sum(train_event_flag)
                           ,train_event_rate = sum(train_event_flag)/sum(train_flag)
                           ,test_event_rate = sum(test_event_flag)/sum(test_flag)
                           ,val1_event_rate = sum(validation_1_event_flag)/sum(validation_1_flag)
                           ,val2_event_rate = sum(validation_2_event_flag)/sum(validation_2_flag))%>%
  select(DC , train_leads , test_leads , val1_leads , val2_leads , train_events , train_event_rate , 
         test_event_rate ,val1_event_rate ,val2_event_rate)

print("Saving Dealer Wise train , test , validation event rates!")

write.xlsx(DW_TR_TS_VAL_ER , paste(str_to_upper(unique(train$Region)) , "_DW_TR_TS_VAL_ER.xlsx"  , sep = ""))

print("Getting Dealer Wise Category Wise train , test , validation event rates!")

DW_CAT_TR_TS_VAL_ER <- DW_TR_TS_VAL%>%group_by(DC , set , PPL_Groups , Vehicle_Application_Groups , Body_Type_Groups , Lead_Source_Groups
                                                      , Customer_Type_Groups)%>%summarise(leads = n() , events = sum(Status))%>%
  mutate(event_rate = events/leads)

print("Saving Dealer Wise Category Wise train , test , validation event rates!")

write.xlsx(DW_CAT_TR_TS_VAL_ER , paste(str_to_upper(unique(train$Region)), "_DW_CAT_ER.xlsx" , sep = ""))

print("Spitting into train,test and validation complete! Let's check the Conversion rate!")

##List of numerical variables

num_vars <- c(
  "DOM"
  ,"Quantity"
  )

var_pattern <- "Lead_Source_Groups_|Customer_Type_Groups_|Vehicle_Application_Groups_|PPL_Groups_|Body_Type_Groups_|_flag|Dealer_Code_"

indep_vars <- c(num_vars,grep(pattern = var_pattern,names(train),value = T))

sapply(train[,indep_vars,with=F], sum)

indep_vars <- indep_vars[!indep_vars %in% c(
                                            "Customer_Type_Groups_Existing_User"
                                            ,"Customer_Type_Groups_Existing_Customer"
                                            # ,"Customer_Type_Groups_Government_Existing"
                                            # ,"Customer_Type_Groups_Government_First_Time"
                                            ,"Customer_Type_Groups_Unspecified"
                                            ,"PPL_Groups_Super_Ace"
                                            # ,"Customer_Type_Groups_Key_Existing_Cust"
                                            ,"Customer_Type_Groups_Key_Competition_Customer"
                                            ,"Body_Type_Groups_FBV"
                                            # ,"Body_Type_Groups_Others"
                                            # ,"Lead_Source_Groups_Showroom_Walk_in"
                                            ,"Lead_Source_Groups_External_Leads"
                                            # ,"Lead_Source_Groups_Field_Enquiry"
                                            ,"Vehicle_Application_Groups_E_Commerce"
                                            # ,"CNG_flag"
                                            ,"CC_flag"
                                            # ,"DI_flag"
                                            # ,"EX_flag"
                                            # ,"RX_flag"
                                            # ,"mint_flag"
)]

table(train$Status[train$CC_flag==1])

dep_var <- "Status"

sapply(train[,indep_vars,with=F], sum)

table(sapply(train[,indep_vars,with=F], sum) < 10)

print("Saving independent variables!")

save(indep_vars , file = "independent variables.rda")

print("Fitting GLM Model!")

glm_model <- glm(Status ~ . , data = train[,c(indep_vars , dep_var) , with = F] , family = binomial)

summary(glm_model)

summary(glm_model$fitted.values)

#Stepwise and normal were same

stepwise_glm <- step(object = glm_model,direction = c("backward"))

glm_model <- stepwise_glm

glm_cutoff <- 0.5

confusionMatrix(data = as.factor(ifelse(glm_model$fitted.values > glm_cutoff,1,0)),reference = as.factor(train$Status)
                ,positive = "1")

names(glm_model$coefficients)

##Gain Chart
cuml.gains <- make.gains(score.values = glm_model$fitted.values , observed.values = train$Status,bins = 10)
cuml.gains <- cuml.gains$results.table
cuml.gains

# train_df <- cbind(train , model_score = glm_model$fitted.values)
# train_df <- train_df[train_df$year_month==201903,]
# 
# ##Gain Chart
# cuml.gains <- make.gains(score.values = train_df$model_score , observed.values = train_df$Status,bins = 10)
# cuml.gains <- cuml.gains$results.table
# cuml.gains

print("Predicting on test data!")

glm_test <- predict(glm_model , test[,indep_vars,with=F] , type="response")

summary(glm_test)

confusionMatrix(data = as.factor(ifelse(glm_test>glm_cutoff,1,0)),reference = as.factor(test$Status)
                ,positive = "1")

##Gain Chart
cuml.gains <- make.gains(score.values = glm_test , observed.values = test$Status,bins = 10)
cuml.gains <- cuml.gains$results.table
cuml.gains

print("Predicting on validation_1 data!")
glm_valid_1 <- predict(glm_model , validation_1[,indep_vars,with=F] , type="response")

summary(glm_valid_1)

confusionMatrix(data = as.factor(ifelse(glm_valid_1>glm_cutoff,1,0)),reference = as.factor(validation_1$Status)
                ,positive = "1")

##Gain Chart
cuml.gains <- make.gains(score.values = glm_valid_1 , observed.values = validation_1$Status,bins = 10)
cuml.gains <- cuml.gains$results.table
cuml.gains

print("Predicting on validation_2 data!")
glm_valid_2 <- predict(glm_model , validation_2[,indep_vars,with=F] , type="response")

summary(glm_valid_2)

confusionMatrix(data = as.factor(ifelse(glm_valid_2>glm_cutoff,1,0)),reference = as.factor(validation_2$Status)
                ,positive = "1")

##Gain Chart
cuml.gains <- make.gains(score.values = glm_valid_2 , observed.values = validation_2$Status,bins = 10)
cuml.gains <- cuml.gains$results.table
cuml.gains

print("Fitting RF Model!")

rf_model <- ranger(Status ~ . , data = mutate(train[,c(indep_vars , dep_var) , with = F],Status=as.factor(Status)) 
                   ,num.trees = 500, mtry = 5, importance = "impurity", min.node.size = 20, seed = 1
                   ,probability = T
                   # ,class.weights = c(1,20)
                   )

write.xlsx(names(sort(rf_model$variable.importance , decreasing = T)) , "RF_MODEL_VARIABLE_IMPORTANCE.xlsx")

length(indep_vars)

# imp_vars <- names(sort(rf_model$variable.importance , decreasing = T))[1:30]
# 
# print("Fitting RF Model with important variables only!")
# 
# rf_model <- ranger(Status ~ . , data = train[,c(imp_vars , dep_var) , with = F] ,num.trees = 100 , mtry = 5
#                    , importance = "impurity" ,min.node.size = 10,  seed = 1  , probability = T)

head(rf_model$predictions)
rf_train <- rf_model$predictions[,c('1')]
head(rf_train)
summary(rf_train)

get_auc(label=train$Status,probs = rf_train)

rf_cutoff <- 0.5

confusionMatrix(data = as.factor(ifelse(rf_train>rf_cutoff,1,0)),reference = as.factor(train$Status)
                ,positive = "1")

##Gain Chart
cuml.gains <- make.gains(score.values = rf_train , observed.values = train$Status,bins = 10)
cuml.gains <- cuml.gains$results.table
cuml.gains

# train_df <- cbind(train , model_score = rf_train)
# train_df <- train_df[train_df$year_month==201904,]
# 
# ##Gain Chart
# cuml.gains <- make.gains(score.values = train_df$model_score , observed.values = train_df$Status,bins = 10)
# cuml.gains <- cuml.gains$results.table
# cuml.gains

print("Predicting on test data!")
rf_test <- predict(rf_model , test[,c(indep_vars) , with=F])
rf_test <- rf_test$predictions
head(rf_test)
colnames(rf_test) <- colnames(rf_model$predictions)
head(rf_test)
rf_test <- rf_test[,c('1')]
head(rf_test)
summary(rf_test)

get_auc(label=test$Status,probs = rf_test)

confusionMatrix(data = as.factor(ifelse(rf_test>rf_cutoff,1,0)),reference = as.factor(test$Status)
                ,positive = "1")

##Gain Chart
cuml.gains <- make.gains(score.values = rf_test , observed.values = test$Status,bins = 10)
cuml.gains <- cuml.gains$results.table
cuml.gains

print("Predicting on validation_1 data!")
head(rf_model$predictions)
rf_valid_1 <- predict(rf_model , validation_1[,c(indep_vars) , with=F])
rf_valid_1 <- rf_valid_1$predictions
colnames(rf_valid_1) <- colnames(rf_model$predictions)
rf_valid_1 <- rf_valid_1[,c('1')]
head(rf_valid_1)
summary(rf_valid_1)

get_auc(label=validation_1$Status,probs = rf_valid_1)

confusionMatrix(data = as.factor(ifelse(rf_valid_1>rf_cutoff,1,0)),reference = as.factor(validation_1$Status)
                ,positive = "1")

##Gain Chart
cuml.gains <- make.gains(score.values = rf_valid_1 , observed.values = validation_1$Status,bins = 10)
cuml.gains <- cuml.gains$results.table
cuml.gains

print("Predicting on validation_2 data!")
rf_valid_2 <- predict(rf_model , validation_2[,c(indep_vars) , with=F])
rf_valid_2 <- rf_valid_2$predictions
colnames(rf_valid_2) <- colnames(rf_model$predictions)
rf_valid_2 <- rf_valid_2[,c('1')]

head(rf_valid_2)
summary(rf_valid_2)

get_auc(label=validation_2$Status,probs = rf_valid_2)

confusionMatrix(data = as.factor(ifelse(rf_valid_2>rf_cutoff,1,0)),reference = as.factor(validation_2$Status)
                ,positive = "1")

##Gain Chart
cuml.gains <- make.gains(score.values = rf_valid_2 , observed.values = validation_2$Status,bins = 10)
cuml.gains <- cuml.gains$results.table
cuml.gains

print("Fitting XGB Model!")

dat <- xgb.DMatrix(data = data.matrix(train[,indep_vars,with=F])
                   ,label = data.matrix(train[,dep_var,with=F]))

mat1 <- xgb.DMatrix(data = data.matrix(test[,indep_vars,with=F])
                    ,label= data.matrix(test[,dep_var,with=F]))

# df <- bind_rows(validation_1,validation_2)
# 
# mat2 <- xgb.DMatrix(data = data.matrix(df[,indep_vars,with=F])
#                     ,label= data.matrix(df[,dep_var,with=F]))

##XGB tree

xgb_model <- xgb.train(data = dat
                       ,objective = "binary:logistic" , nrounds = 2000 ,watchlist = list(
                         validation1 = dat,
                         validation2 = mat1
                         # ,validation3 = mat2
                         # ,validation4 = mat3
                       )
                       ,params = list(
                        booster = "gbtree",eta = 0.1,max_depth = 10,subsample = 0.75,colsample_by_tree = 0.75
                         ,min_child_weight = 5
                         # ,gamma = 1
                       ) , eval_metric = "auc" , print_every_n = 10,early_stopping_rounds = 10)

xgb.plot.importance(importance_matrix = xgb.importance(model=xgb_model,feature_names = indep_vars))

##XGB tree
# xgb_model <- xgboost(data = data.matrix(train[,c(indep_vars) , with = F]), label = data.matrix(train[,c(dep_var) , with = F])
#                      ,objective = "binary:logistic" ,nrounds = 1000,params = list(
#                        booster = "gbtree" , eta = 0.1,max_depth = 5,subsample = 0.8,colsample_by_tree = 0.5
#                        # ,gamma=10
#                        ),
#                      eval_metric = "auc" , print_every_n = 100)
##XGB Linear
# xgb_model <- xgb.train(data = dat
#                        ,objective = "binary:logistic" , nrounds = 1000 ,watchlist = list(
#                          validation1 = dat,
#                          validation2 = mat1
#                          ,validation3 = mat2
#                          # ,validation4 = mat3
#                        )
#                        ,params = list(
#                          booster = "gblinear"
#                          # ,gamma = 5
#                          # ,alpha=10
#                          ) 
#                        , eval_metric = "auc" , print_every_n = 10,early_stopping_rounds = 20)

##XGB Linear
# xgb_model <- xgboost(data = data.matrix(train[,c(indep_vars) , with = F]), label = data.matrix(train[,c(dep_var) , with = F])
#                      ,objective = "binary:logistic" ,nrounds = 494,params = list(
#                        booster = "gblinear"
#                        ,lambda = 1
#                        # ,alpha = 
#                          ) ,
#                      eval_metric = "auc" , print_every_n = 100)

print("Predicting on train data!")

xgb_train <- predict(xgb_model , data.matrix(train[,indep_vars,with=F]))

##Summary of training probabilities
summary(xgb_train)

get_auc(label=train$Status,probs = xgb_train)

##Classification Report
xgb_cutoff <- 0.50

confusionMatrix(data = as.factor(ifelse(xgb_train>xgb_cutoff,1,0)),reference = as.factor(train$Status)
                ,positive = "1")

##Gain Chart
cuml.gains <- make.gains(score.values = xgb_train , observed.values = train$Status,bins = 10)
cuml.gains <- cuml.gains$results.table
cuml.gains


# train_df <- cbind(train , model_score = xgb_train)
# train_df <- train_df[train_df$year_month==201901,]
# 
# ##Gain Chart
# cuml.gains <- make.gains(score.values = train_df$model_score , observed.values = train_df$Status,bins = 10)
# cuml.gains <- cuml.gains$results.table
# cuml.gains

print("Predicting on test data!")
xgb_test <- predict(xgb_model , data.matrix(test[,indep_vars , with=F]))

##Summary of testing probabilities
summary(xgb_test)

##Classification Report
confusionMatrix(data = as.factor(ifelse(xgb_test>xgb_cutoff,1,0)),reference = as.factor(test$Status)
                ,positive = "1")

get_auc(label = test$Status , probs = xgb_test)

##Gain Chart
cuml.gains <- make.gains(score.values = xgb_test , observed.values = test$Status,bins = 10)
cuml.gains <- cuml.gains$results.table
cuml.gains

print("Predicting on validation_1 data!")
xgb_valid_1 <- predict(xgb_model , data.matrix(validation_1[,indep_vars , with=F]))

summary(xgb_valid_1)

confusionMatrix(data = as.factor(ifelse(xgb_valid_1>xgb_cutoff,1,0)),reference = as.factor(validation_1$Status)
                ,positive = "1")

get_auc(label = validation_1$Status , probs = xgb_valid_1)

##Gain Chart
cuml.gains <- make.gains(score.values = xgb_valid_1 , observed.values = validation_1$Status,bins = 10)
cuml.gains <- cuml.gains$results.table
cuml.gains

print("Predicting on validation_2 data!")
xgb_valid_2 <- predict(xgb_model , data.matrix(validation_2[,indep_vars , with=F]))

summary(xgb_valid_2)

confusionMatrix(data = as.factor(ifelse(xgb_valid_2>xgb_cutoff,1,0)),reference = as.factor(validation_2$Status)
                ,positive = "1")

get_auc(label = validation_2$Status , probs = xgb_valid_2)

##Gain Chart
cuml.gains <- make.gains(score.values = xgb_valid_2 , observed.values = validation_2$Status,bins = 10)
cuml.gains <- cuml.gains$results.table
cuml.gains

print("Getting Dealer Level Gain Score!")

if(n_distinct(train$DC) > 1){
  
  
  print("GLM!")
  
  results <- list()
  
  glm_dealer_level <- dealer_level_results(df_train = train
                                           ,df_test = test
                                           ,df_valid_1 = validation_1
                                           ,df_valid_2 = validation_2
                                           ,score_train = glm_model$fitted.values
                                           ,score_test = glm_test
                                           ,score_valid_1 = glm_valid_1
                                           ,score_valid_2 = glm_valid_2
                                           ,observed_train = train$Status
                                           ,observed_test = test$Status
                                           ,observed_valid_1 = validation_1$Status
                                           ,observed_valid_2 = validation_2$Status
                                           )
  
  results[["DEALER_LEVEL_GLM"]] <- glm_dealer_level
  
  # results[["CLUSTER_LEVEL_GLM"]] <- do.call("bind_rows" , list(make.gains(glm_model$fitted.values, train$Status,bins = 10, sim = 500)$results.table%>%mutate(Data = "train") 
  #                                                     ,make.gains(glm_test, test$Status,bins = 10, sim = 500)$results.table%>%mutate(Data = "test") 
  #                                                     ,make.gains(glm_valid_1, validation_1$Status,bins = 10, sim = 500)$results.table%>%mutate(Data = "validation_1")
  #                                                     ,make.gains(glm_valid_2, validation_2$Status,bins = 10, sim = 500)$results.table%>%mutate(Data = "validation_2")
  # ))
  
  save(glm_model , file = "GLM_MODEL.rda")
  
  # glm_model <- NULL
  
  print("GLM Results Saved!")
  
  print("RF!")
  
  rf_dealer_level <- dealer_level_results(df_train = train, df_test = test, df_valid_1 = validation_1,df_valid_2 = validation_2
                                          ,score_train = rf_train ,score_test = rf_test
                                          ,score_valid_1 = rf_valid_1
                                          ,score_valid_2 = rf_valid_2
                                          ,observed_train = train$Status
                                          ,observed_test = test$Status
                                          ,observed_valid_1 = validation_1$Status
                                          ,observed_valid_2 = validation_2$Status
                                          )
  
  results[["DEALER_LEVEL_RF"]] <- rf_dealer_level
  
  # results[["CLUSTER_LEVEL_RF"]] <- do.call("bind_rows" , list(make.gains(rf_train, train$Status,bins = 10, sim = 500)$results.table%>%mutate(Data = "train") 
  #                                                    ,make.gains(rf_test, test$Status,bins = 10, sim = 500)$results.table%>%mutate(Data = "test") 
  #                                                    ,make.gains(rf_valid_1, validation_1$Status,bins = 10, sim = 500)$results.table%>%mutate(Data = "validation_1")
  #                                                    ,make.gains(rf_valid_2, validation_2$Status,bins = 10, sim = 500)$results.table%>%mutate(Data = "validation_2")
  # ))
  
  save(rf_model , file = "RF_MODEL.rda")
  
  print("RF Results Saved!")
  
  print("XGB!")
  
  xgb_dealer_level <- dealer_level_results(df_train = train, df_test = test
                                           ,df_valid_1 = validation_1,df_valid_2 = validation_2
                                           ,score_train = xgb_train
                                           ,score_test = xgb_test
                                           ,score_valid_1 = xgb_valid_1
                                           ,score_valid_2 = xgb_valid_2
                                           ,observed_train = train$Status
                                           ,observed_test = test$Status
                                           ,observed_valid_1 = validation_1$Status
                                           ,observed_valid_2 = validation_2$Status
                                           )
  
  results[["DEALER_LEVEL_XGB"]] <- xgb_dealer_level
  
  # results[["CLUSTER_LEVEL_XGB"]] <- do.call("bind_rows" , list(make.gains(xgb_train, train$Status,bins = 10, sim = 500)$results.table%>%mutate(Data = "train") 
  #                                                     ,make.gains(xgb_test, test$Status,bins = 10, sim = 500)$results.table%>%mutate(Data = "test") 
  #                                                     ,make.gains(xgb_valid_1, validation_1$Status,bins = 10, sim = 500)$results.table%>%mutate(Data = "validation_1")
  #                                                     ,make.gains(xgb_valid_2, validation_2$Status,bins = 10, sim = 500)$results.table%>%mutate(Data = "validation_2")
  # ))
  
  save(xgb_model , file = "XGB_MODEL.rda")
  
  print("XGB Results Saved!")
  
  write.xlsx(results, "DEALER_LEVEL_RESULTS.xlsx")
  
}else{
  
  print("GLM!")
  
  results <- list()
  
  results[["GLM"]] <- do.call("bind_rows" , list(make.gains(glm_model$fitted.values, train$Status,bins = 10, sim = 500)$results.table%>%mutate(Data = "train") 
                                                      ,make.gains(glm_test, test$Status,bins = 10, sim = 500)$results.table%>%mutate(Data = "test") 
                                                      ,make.gains(glm_valid_1, validation_1$Status,bins = 10, sim = 500)$results.table%>%mutate(Data = "validation_1")
                                                      ,make.gains(glm_valid_2, validation_2$Status,bins = 10, sim = 500)$results.table%>%mutate(Data = "validation_2")
  ))
  
  print("GLM Results Saved!")
  
  save(glm_model , file = "GLM_MODEL.rda")
  
  print("RF!")
  
  results[["RF"]] <- do.call("bind_rows" , list(make.gains(rf_train, train$Status,bins = 10, sim = 500)$results.table%>%mutate(Data = "train") 
                                                     ,make.gains(rf_test, test$Status,bins = 10, sim = 500)$results.table%>%mutate(Data = "test") 
                                                     ,make.gains(rf_valid_1, validation_1$Status,bins = 10, sim = 500)$results.table%>%mutate(Data = "validation_1")
                                                     ,make.gains(rf_valid_2, validation_2$Status,bins = 10, sim = 500)$results.table%>%mutate(Data = "validation_2")
  ))
  
  print("RF Results Saved!")
  
  save(rf_model , file = "RF_MODEL.rda")
  
  print("XGB!")
  
  results[["XGB"]] <- do.call("bind_rows" , list(make.gains(xgb_train, train$Status,bins = 10, sim = 500)$results.table%>%mutate(Data = "train") 
                                                      ,make.gains(xgb_test, test$Status,bins = 10, sim = 500)$results.table%>%mutate(Data = "test") 
                                                      ,make.gains(xgb_valid_1, validation_1$Status,bins = 10, sim = 500)$results.table%>%mutate(Data = "validation_1")
                                                      ,make.gains(xgb_valid_2, validation_2$Status,bins = 10, sim = 500)$results.table%>%mutate(Data = "validation_2")
  ))
  
  print("XGB Results Saved!")
  
  save(xgb_model , file = "XGB_MODEL.rda")
  
  write.xlsx(results, "DEALER_LEVEL_RESULTS.xlsx")
}
