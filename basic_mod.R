library(MASS)
library(dplyr)
library(caret)
library(ROCR)
library(plotly)


#La ventaja de este dataset, es que prácticametne con la siguiente instrucción
#ya estaría casi listo para análisis


df <- biopsy %>%
  mutate(target = as.factor(ifelse(class == 'malignant', 1, 0))) %>%
  select(-ID, -class)

#Se hace a "lo bruto" una imputación a 0 de los valores missings

df[is.na(df)] <- 0

genera_roc_glm <- function(seed, df){
  
  set.seed(seed)
  
  index = createDataPartition(df$target, p = 0.70, list = FALSE)
  df_train = df[index, ]
  df_test = df[-index, ]
  
  #Se crea un modelo sencillo (para comlicados siempre cabe variar lo que aquí se tiene)
  
  mod <- train(target ~ V1, 
               method = "glm",
               family = "binomial", 
               data = df_train)
  
  summary(mod)
  
  p <- predict(mod, df_test, type = "prob")[2]
  roc_pred <- prediction(predictions = p  , labels = df_test$target)
  auc_ROCR <- performance(roc_pred, measure = "auc")
  return(auc_ROCR@y.values[[1]])
  
}


genera_roc_knn <- function(seed, df){
  
  set.seed(seed)
  
  index = createDataPartition(df$target, p = 0.70, list = FALSE)
  df_train = df[index, ]
  df_test = df[-index, ]
  
  #Se crea un modelo sencillo (para comlicados siempre cabe variar lo que aquí se tiene)
  
  mod <- train(target ~ V1, data = df_train,
               method = "knn")

  p <- predict(mod, df_test, type = 'prob')[2]
  roc_pred <- prediction(predictions = p  , labels = df_test$target)
  auc_ROCR <- performance(roc_pred, measure = "auc")
  return(auc_ROCR@y.values[[1]])
  
}

genera_roc_glm(1, df)
genera_roc_knn(1, df)


genera_roc_glm(10, df)
genera_roc_knn(10, df)

vector_glm = vector("numeric", 100)
vector_knn = vector("numeric", 100)


#Los bucles tardan unos 5 min máximo, esto se puede paralelizar, pero
#no lo he hecho ya que el objetivo no era un óptimo código, sino que se entendiese

for (i in c(1: 100)){
  
  vector_glm[i] = genera_roc_glm(i, df)
  
  
}

for (i in c(1: 100)){
  
  vector_knn[i] = genera_roc_knn(i, df)
  
  
}


plot_ly(alpha = 0.6) %>% 
  add_histogram(x = vector_glm, name = 'glm') %>% 
  add_histogram(x = vector_knn, name = 'knn') %>% 
  layout(barmode = "overlay")

mean(vector_glm)
sd(vector_glm)

mean(vector_knn)
sd(vector_knn)

max(vector_glm)
min(vector_glm)

max(vector_knn)
min(vector_knn)


#Se introduce una variable más y se repite lo anterior

genera_roc_glm2 <- function(seed, df){
  
  set.seed(seed)
  
  index = createDataPartition(df$target, p = 0.70, list = FALSE)
  df_train = df[index, ]
  df_test = df[-index, ]
  
  #Se crea un modelo sencillo (para comlicados siempre cabe variar lo que aquí se tiene)
  
  mod <- train(target ~ V1 + V2, 
               method = "glm",
               family = "binomial", 
               data = df_train)
  
  summary(mod)
  
  p <- predict(mod, df_test, type = "prob")[2]
  roc_pred <- prediction(predictions = p  , labels = df_test$target)
  auc_ROCR <- performance(roc_pred, measure = "auc")
  return(auc_ROCR@y.values[[1]])
  
}


genera_roc_knn2 <- function(seed, df){
  
  set.seed(seed)
  
  index = createDataPartition(df$target, p = 0.70, list = FALSE)
  df_train = df[index, ]
  df_test = df[-index, ]
  
  #Se crea un modelo sencillo (para comlicados siempre cabe variar lo que aquí se tiene)
  
  mod <- train(target ~ V1 + V2, data = df_train,
               method = "knn")
  
  p <- predict(mod, df_test, type = 'prob')[2]
  roc_pred <- prediction(predictions = p  , labels = df_test$target)
  auc_ROCR <- performance(roc_pred, measure = "auc")
  return(auc_ROCR@y.values[[1]])
  
}

genera_roc_glm2(1, df)
genera_roc_knn2(1, df)


genera_roc_glm2(10, df)
genera_roc_knn2(10, df)

vector_glm2 = vector("numeric", 100)
vector_knn2 = vector("numeric", 100)


#Los bucles tardan unos 5 min máximo, esto se puede paralelizar, pero
#no lo he hecho ya que el objetivo no era un óptimo código, sino que se entendiese

for (i in c(1: 100)){
  
  vector_glm2[i] = genera_roc_glm2(i, df)
  
  
}

for (i in c(1: 100)){
  
  vector_knn2[i] = genera_roc_knn2(i, df)
  
  
}


plot_ly(alpha = 0.6) %>% 
  add_histogram(x = vector_glm2, name = 'glm') %>% 
  add_histogram(x = vector_knn2, name = 'knn') %>% 
  layout(barmode = "overlay")

mean(vector_glm2)
sd(vector_glm2)

mean(vector_knn2)
sd(vector_knn2)

max(vector_glm2)
min(vector_glm2)

max(vector_knn2)
min(vector_knn2)

