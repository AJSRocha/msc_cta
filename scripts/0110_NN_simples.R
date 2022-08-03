
### Modelo ad hoc
model<-keras_model_sequential()

# Topology
model %>%
  layer_dense(input_shape = dim(inp)[2],units=90,name="H1",use_bias=T, activation = 'relu') %>%
  layer_dense(units = 45, use_bias =  T, activation = 'relu') %>%
  layer_dense(units = 1,name="Output") 

# loss, optimizer, metrics
model %>% keras::compile(loss = 'mse', 
                         optimizer = optimizer_adagrad(lr=0.125),
                         metrics = c('mean_absolute_error'))


history <- model %>%fit(
  inp, y_nn, 
  epochs = 30, batch_size = 128, 
  validation_data = list(x_test,y_test))

sum((model %>% predict(inp)))/sum(y_nn)

# tensorboard(action = "stop")

# , callbacks = callback_tensorboard("C://logs/run_a")
# para aceder ao Tensorboard, correr no terminal
## tensorboard --logdir C:\logs\run_a

plot(history) + theme_light()

data.frame(p.GUU=y_nn,
           p.GUU_hat=(model %>% predict(inp)),
           grupo=train[,"EESPECIE"]) %>%
  # filter(lota %in% c("SINES","OLHAO","VRSA")) %>%
  ggplot+
  geom_point(aes(x=p.GUU,y=p.GUU_hat,color=EESPECIE))+
  geom_abline(slope=1,intercept=0) +
  labs(x = "Quantidade real de GUU (kg)", y = "quantidade prevista de GUU (kg)", col = "") + 
  theme_light()
