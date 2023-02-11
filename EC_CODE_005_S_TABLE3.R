da<-fdatasetV2
#定义y====================================
da$death1<-da$death_world

m<-lm(da$death_world~da$edu_all_2010+da$gdp2004+da$urban_rural2015+da$pro.x, data = da)
summary(m)
a<-summary(m)
a1<-a$coefficients
a1<-as.data.frame(a1)
a1$r.squared<-a$r.squared
a1$ajr.squared<-a$adj.r.squared
a1$term<-"overall"
aa<-a1

m<-lm(da$death_world.x~da$edu_all_2010+da$gdp2004+da$urban_rural2015+da$pro.x, data = da)
summary(m)
a<-summary(m)
a1<-a$coefficients
a1<-as.data.frame(a1)
a1$r.squared<-a$r.squared
a1$ajr.squared<-a$adj.r.squared
a1$term<-"male"
aa<-rbind(aa,a1)


m<-lm(da$death_world.y~da$edu_all_2010+da$gdp2004+da$urban_rural2015+da$pro.x, data = da)
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
write.csv(aa,file = "refactor.mallpro.csv")
#===============