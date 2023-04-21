## ---- load1 --------
library(here)
library(tidymodels)
library(ggpubr)
library(vip)
library(ranger)
library(tidyr)
library(gridExtra)

d1 <- readRDS(here::here('data','processed_data','processeddata.rds'))
d1$population1 <- factor(d1$population1, levels = c("small","medium","high"), ordered = TRUE)

d2 <- subset(d1, select = -c(county_name,newid) )

set.seed(123)

# Put 0.7 of the data into the training set 
data_split <- initial_split(d2, prop = 0.7,strata = `14 day case rate`)

# Create data frames for the two sets:
train_data <- training(data_split)
test_data  <- testing(data_split)

# Create 5-fold cross-validation, 5 times repeated
CV5 <- vfold_cv(train_data, v = 5, repeats = 5, strata = `14 day case rate`)

# Create a recipe
#is.ordered(d2$population1)

rec1 <- recipe(`14 day case rate`~.,data=train_data) %>%
  step_ordinalscore(population1)


## ---- Null --------
# Null model performance
null_setup <- null_model() %>%
  set_engine("parsnip") %>%
  set_mode("regression")

null_wf_train <- workflow() %>% 
  add_recipe(rec1)

null_model_train <- fit_resamples(null_wf_train %>% 
                                    add_model(null_setup), CV5,
                                  metrics = metric_set(rmse))


null_model_train %>% collect_metrics() # 87.5

## ---- spec --------
#Specification
#Decision tree
tune_spec <- 
  decision_tree(
    cost_complexity = tune(),
    tree_depth = tune()
  ) %>% 
  set_engine("rpart") %>% 
  set_mode("regression")
# LASSO
glm_spec <- 
  linear_reg(penalty = tune(), mixture = 1) %>% 
  set_engine("glmnet")

# Random forest
cores <- parallel::detectCores()
cores
rf_spec <- 
  rand_forest(mtry = tune(), min_n = tune(), trees = 1000) %>% 
  set_engine("ranger", num.threads = cores,importance = "impurity") %>% 
  set_mode("regression")


## ---- wf --------
#Work flow
#Decision tree
tree_wf <- workflow() %>%
  add_recipe(rec1) %>%
  add_model(tune_spec)
#LASSO
glm_wf=workflow()%>%
  add_recipe(rec1)%>%
  add_model(glm_spec)
#Random forest
rf_wf=workflow()%>%
  add_recipe(rec1)%>%
  add_model(rf_spec)


## ---- grid --------
#Decision tree
tree_grid <- grid_regular(cost_complexity(),
                          tree_depth(),
                          levels = 5)
tree_grid
#LASSO
glm_grid <- tibble(penalty = 10^seq(-4, -1, length.out = 30))
glm_grid %>% top_n(-5) # lowest penalty values
glm_grid %>% top_n(5)  # highest penalty values
#Random forest
extract_parameter_set_dials(rf_spec)


## ---- cv --------
#Decision tree
tree_res <- 
  tree_wf %>% 
  tune_grid(
    resamples = CV5,
    grid = tree_grid
  )
tree_res
#LASSO
glm_res <-glm_wf%>%
  tune_grid(
    resamples=CV5,
    grid=glm_grid,
    control=control_grid(save_pred = TRUE),
    metrics=metric_set(rmse)
  )
glm_res
#Random forest

rf_res <- rf_wf %>%
  tune_grid(
    resamples=CV5,
    grid=25,
    control=control_grid(save_pred=TRUE),
    metrics = metric_set(rmse))
rf_res

## ---- evaluation1 --------
#Decision tree
#plot the result
tree_res %>% autoplot()
#choose best tree
best_tree <- tree_res %>%
  select_best("rmse")
#finalizing the model
final_wf <- 
  tree_wf %>% 
  finalize_workflow(best_tree)
final_wf
#fit by train data
final_fit <- 
  final_wf %>%
  fit(train_data) 
#predict value
tree_predict <- final_fit %>%
  predict(train_data)
#actual vs predict
tree_plot <- as.data.frame(cbind(train_data$`14 day case rate`,tree_predict$`.pred`))
names(tree_plot)[1:2] <- c('actual','predict')

tree_plot1 <- pivot_longer(tree_plot,colnames(tree_plot)) 
plot(tree_plot,'actual','predict')
boxplot(tree_plot,'actual','predict')

#residual plot
residual_tree <- train_data$`14 day case rate`-tree_predict
plot(tree_predict$.pred,residual_tree$.pred)

## ----evalasso--------
#LASSO
# plot the result
glm_res%>%
  autoplot()
# Show the best
glm_res%>%
  show_best("rmse")
# Choose the best
glm_best <- 
  glm_res %>% 
  select_best(metric = "rmse")
glm_best
# Finalize the workflow
glm_final <- 
  glm_wf%>%
  finalize_workflow(glm_best)
#fit by train data
glm_fit <- 
  glm_final %>%
  fit(train_data) 
#predict value
glm_predict <- glm_fit%>%
  predict(train_data)

#actual vs predict
glm_plot <- as.data.frame(cbind(train_data$`14 day case rate`,glm_predict$`.pred`))
names(glm_plot)[1:2] <- c('actual','predict')

glm_plot1 <- pivot_longer(glm_plot,colnames(glm_plot)) 
ggboxplot(glm_plot1,x='name',y='value',add = "jitter")

plot(glm_plot,'actual','predict')

#residual plot
residual_glm <- train_data$`14 day case rate`-glm_predict
plot(glm_predict$.pred,residual_glm$.pred)
#Here residual shows funnel shape. If we decide use LASSO as our final model, we should take the logarithm of our outcome.
#Or we can try getting the lambda in Box-Cox transformation.


## ---- evarf --------
#random forest
# plot the result
rf_res%>%
  autoplot()
# Show the best
rf_res%>%
  show_best("rmse")
# Choose the best
rf_best <- 
  rf_res %>% 
  select_best(metric = "rmse")
rf_best
# Finalize the workflow
rf_final <- 
  rf_wf%>%
  finalize_workflow(rf_best)
#fit by train data
rf_fit <- 
  rf_final %>%
  fit(train_data) 
#predict value
rf_predict <- rf_fit%>%
  predict(train_data)

#actual vs predict
rf_plot <- as.data.frame(cbind(train_data$`14 day case rate`,rf_predict$`.pred`))
names(rf_plot)[1:2] <- c('actual','predict')

rf_plot1 <- pivot_longer(rf_plot,colnames(rf_plot)) 
prf1 <- ggboxplot(rf_plot1,x='name',y='value',add = "jitter")
prf1a <- ggboxplot(rf_plot1,x='name',y='value',add = "jitter")+ylim(0,210)

prf2 <- ggplot(rf_plot,aes(x = actual, y = predict))+
  geom_point(position = position_dodge(width = 1), size = 1, alpha = 0.2)+
  labs(x='Actual value for 14 day case rate',y='Predict in random forest model')

prf2a <- ggplot(rf_plot,aes(x = actual, y = predict))+
  geom_point(position = position_dodge(width = 1), size = 1, alpha = 0.2)+
  labs(x='Actual value for 14 day case rate',y='Predict in random forest model')+
  xlim(-10,270)+ylim(0,180)



prf3 <- grid.arrange(prf1,prf2, prf1a,prf2a, ncol=2,nrow=2)

figure_file = here("results","plot","4_rf_pre_act.png")
ggsave(filename = figure_file, plot=prf3) 



#residual plot
residual_rf <- train_data$`14 day case rate`-rf_predict
plot(rf_predict$.pred,residual_rf$.pred)

residual_rf_a <- cbind(rf_predict,residual_rf)
names(residual_rf_a) <- c('predict','residual')
rerf <- ggplot(data=residual_rf_a,aes(x = predict, y = residual))+
  geom_point(size = 1, alpha = 0.2)+
  labs(x='Predicted value',y='Residuals')
#zoom up
rerfa <- ggplot(data=residual_rf_a,aes(x = predict, y = residual))+
  geom_point(size = 2, alpha = 0.2)+
  labs(x='Predicted value',y='Residuals')+
  xlim(10,180)+ylim(-50,60)

rerf_all <- grid.arrange(rerf,rerfa,ncol=2)
figure_file = here("results","plot","5_rf_residual.png")
ggsave(filename = figure_file, plot=rerf_all) 
#residual looks fine here.

## ---- nullcom --------
# compare model performance
tree_rmse <- tree_res%>%
  show_best("rmse")%>%
  select(3:8)

glm_rmse <- glm_res%>%
  show_best("rmse")%>%
  select(2:7)

rf_rmse <- rf_res%>%
  show_best("rmse")%>%
  select(3:8)

null_rmse <- null_model_train %>% 
  show_best("rmse")

model <- c('null','tree','lasso','random forest')

comparision <- rbind(null_rmse[1,],tree_rmse[1,],glm_rmse[1,],rf_rmse[1,])

comparision1 <- cbind(comparision,model)


#comparision1

#random forest have lowest rmse.
n <- nrow(train_data)
upper_chisq <- qchisq(0.025, n, lower.tail=FALSE)
lower_chisq <- qchisq(0.025, n, lower.tail=TRUE)

#95CI for each model

comparision1$lower <- round(comparision1$mean*sqrt(n/upper_chisq),digit=3)
comparision1$upper <- round(comparision1$mean*sqrt(n/lower_chisq),digit=3)
comparision1$`95%CI` <- paste('(',comparision1$lower,',',comparision1$upper,')')
comparision1

comp1 <- comparision1 %>% select(c(1:5,7,10))

save_data_location <- here::here("results","rds","8_model_comp.rds")
saveRDS(comp1, file = save_data_location)

#so, 95% CI of random forest for random forest looks the best.





## ---- finaleval --------


#final evaluation

rf_last_fit <- rf_final%>%
  last_fit(data_split)
rf_result <- rf_last_fit%>%
  collect_metrics()
rf_result1 <- rf_result %>% select(1:3)

save_data_location <- here::here("results","rds","9_rf_perf.rds")
saveRDS(rf_result1, file = save_data_location)

rf_predict_final <- rf_last_fit%>%
  collect_predictions()
rf_last_fit_plot <- rf_last_fit%>%
  extract_fit_engine()%>%
  vip()

figure_file = here("results","plot","6_rf_variable.png")
ggsave(filename = figure_file, plot=rf_last_fit_plot) 


#performance RMSE=81.3 RSQ=0.462

#actual vs predict

names(rf_predict_final)[c(4,2)] <- c('actual','predict')

rf_final_plot1 <- pivot_longer(rf_predict_final,c('actual','predict')) 
ggboxplot(rf_final_plot1,x='name',y='value',add = "jitter")
plot(as.numeric(rf_predict_final$actual),as.numeric(rf_predict_final$predict))

#residual plot
residual_rf_final <- rf_predict_final$actual-rf_predict_final$predict
plot(rf_predict_final$predict,residual_rf_final)


