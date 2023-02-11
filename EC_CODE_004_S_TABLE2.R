data<-fdatasetV2

#全国分省份====================================
data1<-data
a<-tapply(data1$death_world,INDEX = c(data1$pro.x),FUN = myfun_quan)
aa<-do.call(rbind,a)
aa<-as.data.frame(aa)
aa$minus<-aa$`90%`-aa$`10%`
aa$ratio<-aa$`90%`/aa$`10%`
aa
write.csv(aa,file = "全国分省区县百位数分析-2017.csv")
