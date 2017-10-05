corr_ellimination<-function(table1, ro=0.5){
  table=table1
  AUROC<-table[,1]
  table<-table[order(table[,1],decreasing = T),][,-1]
  table.temp=as.data.frame(table)
  for(i in 1:ncol(table)){#kolona
    for(j in 1:nrow(table)){#red
      #browser()
      if(abs(table[i,j])>ro & abs(table[i,j])<1){
        row.name=rownames(table)[i]
        if(sum(row.name==colnames(table.temp))==0) next
        col.name=colnames(table)[j]
        table.temp=table.temp[,col.name!=colnames(table.temp)]
        table.temp=table.temp[col.name!=row.names(table.temp),]
      }
    }
  }  
  AUROC=AUROC[rownames(table.temp)]
  
  table.temp=cbind(AUROC,table.temp)
  table.temp<-table.temp[AUROC>0.55,]
  table.temp<-table.temp[,rownames(table.temp)]
  
  table.temp[order(rownames(table.temp)),order(colnames(table.temp))]
  AUROC=AUROC[rownames(table.temp)]
  table.temp=cbind(AUROC,table.temp)
  
}