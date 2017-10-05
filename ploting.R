ploting<-function(dataframe, predictor_col, default_column_col,n=3){
  
  #transformacija podataka 
  dataframe=as.data.frame(dataframe)
  data<-data.frame(predictor=dataframe[,predictor_col],
                   default_column=factor(dataframe[,default_column_col]))
  data<-na.omit(data)
  
  #density plot#####################
  density<-ggplot(data, aes(predictor, fill=factor( default_column))) + 
    geom_density(alpha=.5) + 
    scale_fill_manual(values = c('#999999','#E69F00')) + 
    theme(legend.position = "none")+xlim(quantile(data$predictor,c(0.05,0.95),na.rm = T))
  
  #boxplot############################
  box_plot<-ggplot(data, aes(y=predictor, x= default_column, fill=default_column))+
    geom_boxplot(alpha=0.5)
  # compute lower and upper whiskers
  ylim1 = boxplot.stats(data$predictor)$stats[c(1, 5)]
  # scale y limits based on ylim1
  box_plot = box_plot + coord_cartesian(ylim = ylim1*n)+ 
    scale_fill_manual(values = c('#999999','#E69F00'))
  
  #roc curve plot#######################
  ROC_plot <-
    ggplot(data, aes(m = predictor, d = as.numeric(default_column))) + geom_roc() +
    geom_abline(intercept = 0,
                slope = 1,
                color = 'red')
  
  result <- auc(data$default_column, data$predictor)
  
  a = paste("Area under the curve:",
            round(ci(result)[[1]], 3),
            ", ",
            round(result, 3),
            ", ",
            round(ci(result)[[3]], 3))
  
  ROC_plot <- ROC_plot + labs(title = a)
  
  #QQ plot
  
  qs = seq(0.001, 0.999, by=0.001)
  df.qs = data.frame(quantile.P = qs,
                     q.val.Normal = qnorm(qs,mean(data$predictor),sd(data$predictor)),
                     q.val.X = quantile(data$predictor,qs))
  QQ_plot=ggplot(df.qs, aes(q.val.Normal, q.val.X))+
    geom_point(col='#999999', cex=2)+
    geom_line(col='#999999', size=0.75)+
    geom_abline(position="identity")
  
  plot_grid(density,box_plot,ROC_plot,QQ_plot,align = "h", ncol = 4)
  
}
