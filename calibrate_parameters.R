
#ovo je ujedno i prvi put da koristim listu u R-u
calibrate_parameters=function(tabela, varijabla, default.varijabla, 
                              broj_binova=8, 
                              outlier.quant=c(0.99,0.01)){
  tabela$id<-1:nrow(tabela)
  
  #provera da li je clasa tabele data.table objekat
  if(!is.data.table(tabela)) tabela=as.data.table(tabela)
  
  #mora ovako da bi se u funkciji pozvao naziv kasnije u eval funkciji, bag u data.table koji se ovako prevazilazi
  varijabla=as.name(varijabla)
  default.varijabla=as.name(default.varijabla)
  
  
  ############################################################################################
  #                              kontinualna transformacija                                  #
  ############################################################################################
  #################################Log odds transformacija##################################
  #kreiram kategoricku varijablu-kolonu u tabeli koja nam govori kojem quantilu, decilu.. (zavisno od broja binova) pripada data observacija varijable
  tabela[,varijabla_Bin:=quantcut(
    tabela[,eval(varijabla)],
    q = seq(0,1,by = 1/broj_binova)
  )
  ]
  
  #kreiram tabelu medijana sa odgovarajucim pdjevima
  temp_data<-na.omit(
    tabela[,.(prob=sum(eval(default.varijabla))/(.N),
              medians = median(eval(varijabla))),
           by = varijabla_Bin]
  )[order(medians)]
  
  
  #fitujem funkciju na medijane prema pdjevima
  loes_fit<-loess(prob~medians,data=temp_data )
  y<-tabela[,eval(varijabla)]
  
  #sredjujem autljere pre forecasta
  ubound<-quantile(x = y, probs = outlier.quant[1],na.rm = T)
  lbound<-quantile(x = y, probs = outlier.quant[2],na.rm = T)
  
  #sredi te autlajere bre
  y[y>ubound]<-ubound
  y[y<lbound]<-lbound
  
  #forkastujem komplet vrednosti varijable shodno dobijenim vrednostima fit funkcije
  
  median.H<-tabela[eval(default.varijabla)==0,median(eval(varijabla),na.rm = T)]
  median.D<-tabela[eval(default.varijabla)==1,median(eval(varijabla),na.rm = T)]
  
  y[is.na(y) & tabela[,eval(default.varijabla)]==1]<-median.D
  y[is.na(y) & tabela[,eval(default.varijabla)]==0]<-median.H
  
  p<-predict(loes_fit,y)
  
  #p[p<0]=0.000001 #za svaki slucaj
  # p[p>1]=0.999999
  #trebaju mi log odds a ne pdjevi
  
  tabela[,transformisana_varijabla:=log(p/(1-p))]
  
  ##########################################BOX COX transformacija######################### 
  
  # to find optimal lambda
  vector<-tabela[,eval(varijabla)]
  lambda = BoxCox.lambda( vector )
  # now to transform vector
  Box.cox.varijabla = BoxCox( vector, lambda)
  tabela[,Box.cox.varijabla:=Box.cox.varijabla]
  
  
  ############################################################################################
  #                              binovi                                                      #
  ############################################################################################
  
  ##############################log odds binovi#############################################
  
  #racunam pdjeve za svaki bin i dodeljujem ih pored stare vrednosti varijable
  #NA pretvaram u character da bih sracunao i za njega DF
  
  tabela[,varijabla_Bin_numeric:=as.numeric(varijabla_Bin)][
    ,varijabla_Bin_numeric:=as.character(varijabla_Bin_numeric)][
      is.na(varijabla_Bin_numeric),varijabla_Bin_numeric:="NA"]
  
  tabela[,prob:=sum(eval(default.varijabla))/(.N),by = varijabla_Bin_numeric]
  
  #record a plot
  p1=ggplot(data=tabela, aes(varijabla_Bin_numeric))+geom_bar(aes(weight=eval(default.varijabla)))
  
  #ali trebaju mi log odds naravno
  tabela[,odds:=(prob/(1-prob))]
  
  ###################################woe binovi###############################################
  
  data<-data.frame(
    default.varijabla=tabela[,eval(default.varijabla)],
    continualna_varijabla=tabela[,eval(varijabla)])
  
  WOE = create_infotables(data = as.data.frame(data),
                          y="default.varijabla", 
                          parallel=FALSE,
                          bins = broj_binova)
  
  woe =WOE$Tables$continualna_varijabla[,c(1,4)]
  
  #ako postoje missing values 
  if(woe[1,1]=="NA"){
    woe$"varijabla_Bin_numeric"=c("NA",1:broj_binova) 
  } else {
    woe$"varijabla_Bin_numeric"=as.character(1:broj_binova)
  }
  
  woe.plot<-plot_infotables(WOE,"continualna_varijabla")
  
  #prebaci kategorije u brojeve pa njih u character zbog na iz prethodne tabele, pa na u character jos jednom da bi mogao da ga vlookapuje
  
  
  
  tabela<-merge(x=tabela,y=woe, all.x = T)[order(id)][,id:=NULL]#varijabla_Bin_numeric je trazena varujabla
  
  
  
  ################################################################################################
  #                            AUC vrednosti                                                     #
  ################################################################################################
  
  #racunam auc vrednost, staru pa novu  noooovu 
  #stara
  auroc.s<-auc(
    as.numeric(tabela[,eval(default.varijabla)]),
    as.numeric(tabela[,eval(varijabla)]))
  auc.vrednost.pre<-as.numeric(auroc.s)
  #nova
  auroc.n<-auc(
    as.numeric(tabela[,eval(default.varijabla)]),
    as.numeric(tabela[,transformisana_varijabla]))
  auc.vrednost.posle<-as.numeric(auroc.n)
  
  ###############################################################################################
  #                            Ostali Grafici                                                   #
  ###############################################################################################
  
  col.num.default<-grep("default", colnames(tabela))
  col.num.varijabla<-which(colnames(tabela)==varijabla)
  
  ostali_plotovi_pre=ploting(tabela,col.num.varijabla,col.num.default)
  
  col.num.default<-grep("default", colnames(tabela))
  col.num.varijabla<-which(colnames(tabela)=="transformisana_varijabla")
  ostali_plotovi_posle=ploting(tabela,col.num.varijabla,col.num.default)
  
  col.num.varijabla<-which(colnames(tabela)=="Box.cox.varijabla")
  box.cox_plotovi<-ploting(tabela, col.num.varijabla, col.num.default)
  
  output<-list()
  output = list(monotonicity_graph = p1,
                ostali_plotovi_pre=ostali_plotovi_pre,
                ostali_plotovi_posle=ostali_plotovi_posle,
                woe.plot=woe.plot,
                auc.vrednost=data.frame(auc.vrednost.pre,auc.vrednost.posle),
                fit_funkcija_objekat = loes_fit)
  
  
  tabela$transformisana_varijabla->output[[7]]
  names(output)[7]<-paste(varijabla,".tr",sep = "")
  
  tabela$odds->output[[8]]
  names(output)[8]<-paste(varijabla,".odds",sep = "")
  
  tabela$WOE->output[[9]]
  names(output)[9]<-paste(varijabla,".WOE",sep = "")
  
  Box.cox.varijabla->output[[10]]
  names(output)[10]<-paste(varijabla,".Box.Cox",sep = "")
  
  box.cox_plotovi->output[[11]]
  names(output)[11]<-paste(varijabla,".Box.Cox.plot",sep = "")
  
  output
  
}