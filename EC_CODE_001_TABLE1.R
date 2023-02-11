
#导入数据
data<-fdatasetV2
data<-data[!is.na(data$code15),]
data<-data[!duplicated(data$code15),]#994个点=================684个点；

#===994个点的名单
table1(~data$pro.x,data=data)
#第一次死因调查的粗率计算
a<-fdatasetV2[!duplicated(fdatasetV2$codefirst),]
data<-a
sum(data$fdeath_num)/sum(data$fpop)*100000
sum(data$fdeath_num)
sum(data$fdeath_num.x)
sum(data$fdeath_num.y)
sum(data$fdeath_num.x)/sum(data$fpop.x)*100000
sum(data$fdeath_num.y)/sum(data$fpop.y)*100000


#==========
da<-fdatasetV2
#提取粗率并重新计算code合计==================
load("s_nds01_cru.R")
data<-s_nds01_cru[s_nds01_cru$CODE%in%da$codefirst,]
d<-data[data$age_group%in%"all"&data$SEX%in%"0",]#820
data$CODE<-as.numeric(data$CODE)
data$age_group<-factor(data$age_group,labels=c(20,1:17))
data$age_group<-as.numeric(as.character(data$age_group))
data$KIND<-as.numeric(data$KIND)
data<-data[,-7]

a<-aggregate(data,by=list(data$age_group,data$SEX),FUN=sum,na.rm=TRUE)

a3<-a[,c(1:2,6:8)]

names(a3)[1:2]<-c("Agegroup","sex")
a3$CODE<-"all"
head(a3)
a3$death<-(a3$death_num/a3$pop)*100000
head(a3)
#导入标化人口
pop_stand<-read_excel("C:/Users/86131/Desktop/食管癌高低发区研究/食管癌人群数据/各种标准人口_第一次死因.xlsx")
#人群发病数据计算发病死亡率
head(pop_stand)
age_c<-pop_stand$age
data<-a3
data$age_group<-data$Agegroup
data$age<-factor(data$age_group,labels = age_c)
incidata01<-merge(data,pop_stand,by="age",all = TRUE)

#提取数据
table(incidata01$age)
incidata01<-incidata01[!incidata01$age%in%"合计",]
#标化发病率计算公式：年龄别实际发病率*年龄别标准人口=理论发病数
#理论发病数求和=理论发病总数；
#理论发病总数/标准总人口
incidata01$death_china_num<-incidata01$death*incidata01$ASRcn00
incidata01$death_world_num<-incidata01$death*incidata01$ASRwld85
head(incidata01)
#计算粗率
names(incidata01)
incidata3<-incidata01[,c("age_group","CODE","sex","pop",
                         "death_num",
                         "death_china_num","death_world_num",
                         "ASRcn00","ASRwld85")]

#求和
incidata3<-incidata3[,-2]
a<-aggregate(x = incidata3,by=list(incidata3$sex),
             FUN=sum,na.rm=TRUE)
a$death_china<-a$death_china_num/a$ASRcn00
a$death_world<-a$death_world_num/a$ASRwld85

#百分位数分析===================================
data<-fdatasetV2
#全国分性别
a<-tapply(data$fdeath_world,INDEX = c(data$SEX),FUN = myfun_quan)
aa<-do.call(rbind,a)
aa<-as.data.frame(aa)
aa$minus<-aa$`90%`-aa$`10%`
aa$ratio<-aa$`90%`/aa$`10%`
aa
b<-aa

a<-tapply(data$fdeath_world.x,INDEX = c(data$SEX),FUN = myfun_quan)
aa<-do.call(rbind,a)
aa<-as.data.frame(aa)
aa$minus<-aa$`90%`-aa$`10%`
aa$ratio<-aa$`90%`/aa$`10%`
aa
b<-rbind(b,aa)

a<-tapply(data$fdeath_world.y,INDEX = c(data$SEX),FUN = myfun_quan)
aa<-do.call(rbind,a)
aa<-as.data.frame(aa)

aa$minus<-aa$`90%`-aa$`10%`
aa$ratio<-aa$`90%`/aa$`10%`
aa
b<-rbind(b,aa)

a<-tapply(data$death_world,INDEX = c(data$SEX),FUN = myfun_quan)
aa<-do.call(rbind,a)
aa<-as.data.frame(aa)
aa$minus<-aa$`90%`-aa$`10%`
aa$ratio<-aa$`90%`/aa$`10%`
aa
b<-rbind(b,aa)


a<-tapply(data$death_world.x,INDEX = c(data$SEX),FUN = myfun_quan)
aa<-do.call(rbind,a)
aa<-as.data.frame(aa)
aa$minus<-aa$`90%`-aa$`10%`
aa$ratio<-aa$`90%`/aa$`10%`
aa
b<-rbind(b,aa)


a<-tapply(data$death_world.y,INDEX = c(data$SEX),FUN = myfun_quan)
aa<-do.call(rbind,a)
aa<-as.data.frame(aa)
aa$minus<-aa$`90%`-aa$`10%`
aa$ratio<-aa$`90%`/aa$`10%`
aa
b<-rbind(b,aa)
b
write.csv(b,file = "table1c.csv")
