dataset<-read.dta("C:\\Users\\Neil\\Downloads\\2006 CCES replication.dta")

#Column 1
model<-glm(voted~age+income+as.factor(race)+divergence+educ+female+competitive+as.factor(st_id), family="binomial",data=dataset)


#Column 2
model<-glm(voted~divergence+competitive+age+educ+income+female+as.factor(race)+as.factor(st_id), family="binomial", data=dataset)