
final.roc.plot=function(glm.model,training.data,test.data){

  default.variable.name=as.name(attributes(glm.model$terms)$variables[[2]])
  
  model.data.frame=data.frame(fit=fitted.values(glm.model), dif=glm.model$y)
  
  fit.test=predict(glm.model, newdata =test.data, type = "response")
  
  model.pred.data.frame<-data.frame(fit=fit.test, dif=test.data[,eval(default.variable.name)])
  
  
 #browser()
  model.data.frame$Uzorak<-"Razvojni"
  model.pred.data.frame$Uzorak<-"Validacioni"
  
  dataFrame=rbind(model.data.frame,model.pred.data.frame)
  dataFrame$Uzorak=as.factor(dataFrame$Uzorak)
  
  result.train = auc(model.data.frame$dif, model.data.frame$fit)
  
  annotation1 = paste("AUROC:",
                     round(ci(result.train)[[1]], 3),
                     ", ",
                     round(result.train, 3),
                     ", ",
                     round(ci(result.train)[[3]], 3))
  
  
  result.validation <- auc( model.pred.data.frame$dif, model.pred.data.frame$fit)
  
  annotation2 = paste("AUROC:",
                     round(ci(result.validation )[[1]], 3),
                     ", ",
                     round(result.validation , 3),
                     ", ",
                     round(ci(result.validation )[[3]], 3))
  
  
  
  ROC_plot <-
    ggplot(dataFrame, aes(m = fit, d = as.numeric(dif),color=Uzorak))+ 
    geom_roc(labelround=2)+
    geom_abline(intercept = 0,
                slope = 1) +
    annotate("text", x = .605, y = .25, 
               label = annotation1, color=hue_pal()(2)[1]) +
    annotate("text", x = .6, y = .35, 
             label = annotation2, color=hue_pal()(2)[2])+
    ggtitle("ROC krive sa dvostranim 90% intervalom poverenja")+
    style_roc()+
    theme(plot.title = element_text(family = "serif", 
                                    color="#666666", 
                                    size=12, hjust=0)
    )+
    theme(axis.title = element_text(family = "serif", 
                                    color="#666666", 
                                    size=10, hjust=0)
    )
    
  

  
  #ROC_plot.train <- ROC_plot.train + labs(title = a)+style_roc()
  hl.training=hoslem.test(model.data.frame$dif, model.data.frame$fit)
  
   
  #validation
  #ROC_plot.validation <-
    #ggplot( model.pred.data.frame, aes(m = fit, d = as.numeric(dif))) + geom_roc() +
    #geom_abline(intercept = 0,
               # slope = 1,
               # color = 'red')
  

  
  #ROC_plot.validation <- ROC_plot.validation + labs(title = a)+style_roc()
  
  hl.test=hoslem.test(as.numeric(model.pred.data.frame$dif),
                            as.numeric(model.pred.data.frame$fit))
  
  #results
  output=list()
  output$ROC_plot = ROC_plot
  #ROC_plot$ROC_plot.train = ROC_plot.train
  output$hl.training=hl.training
  output$hl.test=hl.test
  
  
  output

}
