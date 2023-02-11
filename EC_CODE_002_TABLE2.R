
da<-fdatasetV2
#定义y====================================
da$death1<-da$death_world
#单变量分析
#经济水平
da$gdp2004<-as.numeric(da$gdp2004)/10000
fdata<-da
m<-lm(death1~da$gdp2004, data = da)
a<-summary(m)
a1<-a$coefficients
a1<-as.data.frame(a1)
a1<-a1[2,]
a1$r.squared<-a$r.squared
aa<-rbind(aa,a1)
aa
#==========教育水平============
da$edu_all_2010<-as.numeric(da$edu_all_2010)
fdata<-da
m<-lm(death1~da$edu_all_2000, data = da)
a<-summary(m)
a1<-a$coefficients
a1<-as.data.frame(a1)
a1<-a1[2,]
a1$r.squared<-a$r.squared
aa<-rbind(aa,a1)
aa
#========
da$urban_rural2015<-as.numeric(da$urban_rural2015)
fdata<-da
m<-lm(death1~da$urban_rural2015, data = da)
a<-summary(m)
a1<-a$coefficients
a1<-as.data.frame(a1)
a1<-a1[2,]
a1$r.squared<-a$r.squared
aa<-rbind(aa,a1)
aa
write.csv(aa,file = "refactor.all.csv")

#多因素变量============================
m<-lm(da$death_world~da$edu_all_2010+da$gdp2004+da$urban_rural2015, data = da)
summary(m)
a<-summary(m)
a1<-a$coefficients
a1<-as.data.frame(a1)
a1$r.squared<-a$r.squared
a1$ajr.squared<-a$adj.r.squared
a1$term<-"overall"
aa<-a1

m<-lm(da$death_world.x~da$edu_all_2010+da$gdp2004+da$urban_rural2015, data = da)
summary(m)
a<-summary(m)
a1<-a$coefficients
a1<-as.data.frame(a1)
a1$r.squared<-a$r.squared
a1$ajr.squared<-a$adj.r.squared
a1$term<-"male"
aa<-rbind(aa,a1)


m<-lm(da$death_world.y~da$edu_all_2010+da$gdp2004+da$urban_rural2015, data = da)
summary(m)
a<-summary(m)
a1<-a$coefficients
a1<-as.data.frame(a1)
a1$r.squared<-a$r.squared
a1$ajr.squared<-a$adj.r.squared
a1$term<-"female"
aa<-rbind(aa,a1)
aa

aa$bci<-paste(round(aa$Estimate,2),"(",round((aa$Estimate-1.96*aa$`Std. Error`),2),"-",
              round((aa$Estimate+1.96*aa$`Std. Error`),2),")")
write.csv(aa,file = "refactor.mall.csv")
#=================================
