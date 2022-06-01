# percentage of eigenvalue drop
eigwords <- read.csv("eig_stem_bip_SJEDI.csv")
eigwords$X <- eigwords$final_eig_cent-eigwords$interim_eig_cent
negwords<-nrow(eigwords[eigwords$X<0,])
totwords<-nrow(eigwords)
centdrop<-negwords/totwords*100
eigSJEDI<-eigwords[eigwords$SJEDI == TRUE,]
negSJEDI<-nrow(eigSJEDI[eigSJEDI$X<0,])
totSJEDI<-nrow(eigSJEDI)
SJEDIdrop<-negSJEDI/totSJEDI*100
