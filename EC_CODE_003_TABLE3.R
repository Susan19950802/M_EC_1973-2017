
da<-data
#定义y====================================

calcOddsRatio <- function(mymatrix,alpha=0.05,referencerow=2,quiet=FALSE)
{
  numrow <- nrow(mymatrix)
  myrownames <- rownames(mymatrix)
  
  for (i in 1:numrow)
  {
    rowname <- myrownames[i]
    DiseaseUnexposed <- mymatrix[referencerow,1]
    ControlUnexposed <- mymatrix[referencerow,2]
    if (i != referencerow)
    {
      DiseaseExposed <- mymatrix[i,1]
      ControlExposed <- mymatrix[i,2]
      
      totExposed <- DiseaseExposed + ControlExposed
      totUnexposed <- DiseaseUnexposed + ControlUnexposed
      
      probDiseaseGivenExposed <- DiseaseExposed/totExposed
      probDiseaseGivenUnexposed <- DiseaseUnexposed/totUnexposed
      probControlGivenExposed <- ControlExposed/totExposed
      probControlGivenUnexposed <- ControlUnexposed/totUnexposed
      
      # calculate the odds ratio
      oddsRatio <- (probDiseaseGivenExposed*probControlGivenUnexposed)/
        (probControlGivenExposed*probDiseaseGivenUnexposed)
      if (quiet == FALSE)
      {
        print(paste("category =", rowname, ", odds ratio = ",oddsRatio))
      }
      
      # calculate a confidence interval
      confidenceLevel <- (1 - alpha)*100
      sigma <- sqrt((1/DiseaseExposed)+(1/ControlExposed)+
                      (1/DiseaseUnexposed)+(1/ControlUnexposed))
      # sigma is the standard error of our estimate of the log of the odds ratio
      z <- qnorm(1-(alpha/2))
      lowervalue <- oddsRatio * exp(-z * sigma)
      uppervalue <- oddsRatio * exp( z * sigma)
      if (quiet == FALSE)
      {
        print(paste("category =", rowname, ", ", confidenceLevel,
                    "% confidence interval = [",lowervalue,",",uppervalue,"]"))
      }
    }
  }
  if (quiet == TRUE && numrow == 2) # If there are just two treatments (exposed/nonexposed)
  {
    return(oddsRatio)
  }
}

> calcRelativeRisk <- function(mymatrix,alpha=0.05,referencerow=2)
{
  numrow <- nrow(mymatrix)
  myrownames <- rownames(mymatrix)
  for (i in 1:numrow)
  {
    rowname <- myrownames[i]
    DiseaseUnexposed <- mymatrix[referencerow,1]
    ControlUnexposed <- mymatrix[referencerow,2]
    if (i != referencerow)
    {
      DiseaseExposed <- mymatrix[i,1]
      ControlExposed <- mymatrix[i,2]
      totExposed <- DiseaseExposed + ControlExposed
      totUnexposed <- DiseaseUnexposed + ControlUnexposed
      probDiseaseGivenExposed <- DiseaseExposed/totExposed
      probDiseaseGivenUnexposed <- DiseaseUnexposed/totUnexposed
      # calculate the relative risk
      relativeRisk <- probDiseaseGivenExposed/probDiseaseGivenUnexposed
      print(paste("category =", rowname, ", relative risk = ",relativeRisk))
      # calculate a confidence interval
      confidenceLevel <- (1 - alpha)*100
      sigma <- sqrt((1/DiseaseExposed) - (1/totExposed)+
                      (1/DiseaseUnexposed) - (1/totUnexposed))
      # sigma is the standard error of estimate of log of relative risk
      z <- qnorm(1-(alpha/2))
      lowervalue <- relativeRisk * exp(-z * sigma)
      uppervalue <- relativeRisk * exp( z * sigma)
      print(paste("category =", rowname, ", ", confidenceLevel,
                  "% confidence interval = [",lowervalue,",",uppervalue,"]"))
    }
  }
}

mydata<-matrix(c(45,88,136,1459),nrow = 2,byrow = TRUE)
mydata
colnames(mydata)<-c("患病","未患病")
rownames(mydata)<-c("暴露","未暴露")
print(mydata)

calcRelativeRisk(mydata)

da<-data
a<-aggregate(cbind(da$death_num,da$pop)~da$edu_all_2010_g,data=data,mean)
a<-aggregate(cbind(da$death_num,da$pop)~da$gdp2004_g,data=data,mean)
a$inci<-(a$V1/a$V2)*100000
a
a[,c(2:3)]
a<-data.frame(a[,c(2:3)])

mydata<-as.matrix(a)

mydata<-t(mydata)
mydata

calcRelativeRisk(mydata, referencerow=1)

mymatrix <- matrix(c(575,8345990,480,5026241),nrow=2,byrow=TRUE)
mymatrix
colnames(mymatrix) <- c("Disease","Control")
rownames(mymatrix) <- c("Exposed","Unexposed")
print(mymatrix)


calcOddsRatio(mymatrix,alpha=0.05)
#[1] "category = Exposed , odds ratio =  0.160039091621751"
#[1] "category = Exposed ,  95 % confidence interval = [ 0.135460641900536 , 0.189077140693912 ]"


mymatrix <- matrix(c(30,24,76,241,82,509),nrow=3,byrow=TRUE)
colnames(mymatrix) <- c("Disease","Control")
rownames(mymatrix) <- c("Exposure1","Exposure2","Unexposed")
print(mymatrix)

calcOddsRatio(mymatrix, referencerow=3)
calcRelativeRisk(mymatrix, referencerow=3)