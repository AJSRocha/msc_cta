grelha de parametros

```{r, echo = F}
grelha_nn<-expand.grid(
  units_x=c(16,32,64,128,256,512),
  batch_size_x=c(128,256,512),
  activation_1=c("relu","sigmoid","tanh","linear"),
  activation_2=c("relu","sigmoid","tanh","linear")
)

grelha_nn$loss<-NA
grelha_nn$mean_absolute_error<-NA
grelha_nn$val_loss<-NA
grelha_nn$val_mean_absolute_error<-NA
```

```{r keras_func, echo = F}
keras_search<-function(units_x,activation_1,activation_2,batch_size_x){
  model<-keras_model_sequential()
  
  # Topology
  model %>%
    layer_dense(input_shape = dim(inp)[2],units=units_x,activation = activation_1,name="H1") %>%
    layer_dense(units = 1, activation = activation_2,name="Output") 
  
  # loss, optimizer, metrics
  model %>% compile(loss = 'mse', 
                    optimizer = 'RMSprop', 
                    metrics = c('mean_absolute_error'))
  
  assign("history",(model %>% fit(
    inp, y_nn, 
    epochs = 30, batch_size = batch_size_x, 
    validation_split = 0.1)),
    envir = .GlobalEnv)}
```

```{r search, echo = F}
for(i in 1:nrow(grelha_nn)){
  keras_search(grelha_nn$units_x[i],grelha_nn$activation_1[i],grelha_nn$activation_2[i],grelha_nn$batch_size_x[i])
  grelha_nn$loss[i]<-history$metrics$loss[30]
  grelha_nn$mean_absolute_error[i]<-history$metrics$mean_absolute_error[30]
  grelha_nn$val_loss[i]<-history$metrics$val_loss[30]
  grelha_nn$val_mean_absolute_error[i]<-history$metrics$val_mean_absolute_error[30]
}

print(grelha_nn[which.min(grelha_nn$val_mean_absolute_error),])
```