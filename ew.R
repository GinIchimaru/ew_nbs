#setting the working directory to the be this source file directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#sourcing nbs_data_prep_function for creating default indicator
source("nbs_data_prep_function.R")



a<-read_KA4_RK_data(KA4_url,RK_url,granica_gubitka=0.9)

