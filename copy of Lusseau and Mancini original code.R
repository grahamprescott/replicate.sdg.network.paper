############################################################################
### code from article published 1 March 2019 in Nature Sustainability    ###
### Lusseau D. & Mancini F. (2019) Income-based variation in Sustainable ###
### Development Goal interaction networks. Nature Sustainability         ###
### http://dx.doi.org/10.1038/s41893-019-0231-4                          ###
###                                                                      ###
### original preprint and updated version available on arxiv:            ###
### https://arxiv.org/abs/1804.09095                                     ###
############################################################################

##########################################################################
#####             code developed by David Lusseau                    #####
#####                  18 August 2017                                #####
#####           any questions email d.lusseau@abdn.ac.uk             ##### 
##########################################################################

##########################################################################
#####             thanks to World Bank and EMDAT.BE                  #####
##### for making data available in a relevant and easy to use format ##### 
##########################################################################


###############
##working with WDI from world bank
##first extract indicators identified to fit a SDG target

#####data available at: http://data.worldbank.org/data-catalog/sustainable-development-goals
##SDG target x indicator tableau not available so doing it by hand

###API 

##### code working from locally saved data 
##### make sure to add your own relevant path

ser<-read.csv("./SDGSeriesamended.csv",header=T)
data<-read.csv("./SDGData.csv",header=T)

##adding SDG and target to SDGSeries

data$SDG<-NA
data$target<-ser$target[1]
data$direction<-NA


for (i in 1:dim(data)[1]) {
  data$SDG[i]<-ser$SDG[ser$Series.Code==data$Indicator.Code[i]]
  data$target[i]<-ser$target[ser$Series.Code==data$Indicator.Code[i]]
  data$direction[i]<-ser$direction[ser$Series.Code==data$Indicator.Code[i]]
  
}

names(data)[1]<-"Country.Name"

save(ser,data,file="~//SDG.Rdata")


###QC

datas<-subset(data,Indicator.Code!="DT.ODA.ODAT.CD") 	##two code for one indicator .CD and .CD1; from visualisation in most instances 
##the same and when not .CD1>.CD, so chose .CD1

datas$Indicator.Code<-factor(datas$Indicator.Code)


countries<-data.frame(country=unique(datas[,1]),Country.Code=unique(datas[,2]))
indicators<-data.frame(indicator=unique(datas[,3]),Indicator.Code=unique(datas[,4]))  ##handy data frame to refer back to full text names

datasub<-datas[,-c(1,3,33)] #clear space to handle data frame


library(reshape2)

datamelt<-melt(datasub,id=c("Country.Code","Indicator.Code", "SDG","target","direction"))

datamelt$year<-as.numeric(sub("X","",datamelt$variable))

datamelt<-datamelt[,-6]

##subset(datamelt,Country.Code=="FRA" & Indicator.Code=="SI.POV.URHC")

nc<-dim(countries)[1]
ni<-dim(indicators)[1]


repi<-(ni*ni-ni)/2  ##upper triangle


#######SDG 13
### went back to emdat.be to grab the time series of disasters, will be using the sum of affected and deaths per year per country
###downloaded data (24 Aug 2017) is in disaster131.csv


disaster<-read.csv("~//disaster131.csv",header=T)

#QC
# missing<-unique(disaster$iso)[!unique(disaster$iso)%in%unique(data$Country.Code)]
# missing.name<-unique(disaster$country_name)[!unique(disaster$iso)%in%unique(data$Country.Code)]

disaster<-disaster[-c(5377:5380),]
disaster$year<-as.numeric(disaster$year)+1898  #go figure


disaster<-subset(disaster,year>1989)
disaster$iso<-factor(disaster$iso)
disaster$country_name<-factor(disaster$country_name)


######################
#####need to pad the dataset with countries that do not have EMBE data with NAs for 1990 to 2017 years
#######################


dis<-data.frame(Country.Code=disaster$iso, Indicator.Code=rep("EMBE",dim(disaster)[1]),SDG=rep("13",dim(disaster)[1]),target=rep("13.1",dim(disaster)[1]),direction=rep("-1",dim(disaster)[1]),value=disaster$Total.affected,year=disaster$year,valued=rep(NA,dim(disaster)[1]) )

###padding years for countries that are there
tt<-table(dis[dis$Indicator.Code=="EMBE",]$Country.Code,dis[dis$Indicator.Code=="EMBE",]$year)

yearmelt<-melt(tt)
yearmelt$value[yearmelt$value==0]<-NA
yearmelt<-subset(yearmelt,is.na(value)==TRUE)

extrapadding<-data.frame(Country.Code=yearmelt$Var1,Indicator.Code=rep("EMBE",dim(yearmelt)[1]),SDG=rep("13",dim(yearmelt)[1]),target=rep("13.1",dim(yearmelt)[1]),direction=rep(-1,dim(yearmelt)[1]),value=rep(NA,dim(yearmelt)[1]),year=yearmelt$Var2,valued=rep(NA,dim(yearmelt)[1]))

dis<-rbind(dis,extrapadding)



####countries
###
countryremove<-unique(dis$Country.Code)%in%unique(datamelt$Country.Code)
#countryremove.name<-unique(datamelt$Country.Code)[!countryremove]

countrygone<- unique(datamelt$Country.Code)%in%unique(dis$Country.Code)
#countrygone.name<-unique(datamelt$Country.Code)[countrygone]

togo<-unique(dis$Country.Code)[!countryremove]   ##countries in dis but not datamelt (to remove) dis has 217 countries and datamelt 263
missing<-unique(datamelt$Country.Code)[!countrygone] ##countries in datamelt but not dis (to add)



for (i in 1:length(togo)) {
  dis<-subset(dis,Country.Code!=as.character(togo[i]))
}

dis$Country.Code<-factor(dis$Country.Code)



years<-seq(1990,2017,1)
padding<-data.frame(Country.Code=rep(missing,length(years)),Indicator.Code=rep("EMBE",length(missing)*length(years)),SDG=rep("13",length(missing)*length(years)),target=rep("13.1",length(missing)*length(years)),direction=rep(-1,length(missing)*length(years)),value=rep(NA,length(missing)*length(years)),year=rep(years,each=length(missing)),valued=rep(NA,length(missing)*length(years)) )

dis<-rbind(dis,padding)

datamelt$direction<-as.numeric(datamelt$direction)
datamelt$valued<-datamelt$value*datamelt$direction


datamelt<-rbind(datamelt,dis)

datamelt$direction<-as.numeric(datamelt$direction)


save(datamelt,file="~//datamelt.Rdata")

###################
###udate loop counter
indicators<-data.frame(indicator=c(as.character(unique(datas[,3])),"number of people affected by disaster"),Indicator.Code=c(as.character(unique(datas[,4])),"EMBE"))  ##handy data frame to refer back to full text names
indicators$indicator<-as.factor(indicators$indicator)
indicators$Indicator.Code<-as.factor(indicators$Indicator.Code)
ni<-dim(indicators)[1]

repi<-(ni*ni-ni)/2  ##upper triangle


###first pass lme autocorrelation AR1 with random effect of country
##second pass debugged lme throwing warnings (mainly not enough samples (years sampled) for structure

library(nlme)

assoc<-data.frame(Indicator1=rep(indicators$Indicator.Code[1],repi), Indicator2=rep(indicators$Indicator.Code[1],repi), fixed=rep(0,repi), SE=rep(0,repi), df=rep(0,repi), tvalue=rep(0,repi), pvalue=rep(1,repi))


iter<-1


##################
for (i in 1:(ni-1)) {
  for (j in (i+1):ni) {
    
    
    #######################
    
    memdat<-subset(datamelt,Indicator.Code==indicators$Indicator.Code[i]|Indicator.Code==indicators$Indicator.Code[j])
    
    shaped_datamelt<-subset(memdat[,c(1,2,7,8)],Indicator.Code==indicators$Indicator.Code[i])
    colnames(shaped_datamelt)[4]<-as.character(indicators$Indicator.Code[i])
    shaped_datamelt$inter<-memdat[memdat$Indicator.Code==indicators$Indicator.Code[j],]$valued
    
    colnames(shaped_datamelt)[5]<-as.character(indicators$Indicator.Code[j])
    
    
    shaped_datamelt<-subset(shaped_datamelt,!is.na(shaped_datamelt[,4]) & !(is.na(shaped_datamelt[,5])))
    
    shaped_datamelt[,5]<-scale(shaped_datamelt[,5])
    shaped_datamelt[,4]<-scale(shaped_datamelt[,4])
    
    
    fmla <- as.formula(paste0(as.character(indicators$Indicator.Code[i]),"~",as.character(indicators$Indicator.Code[j])))
    
    lmeI1I2<-tryCatch(lme(fmla,data=shaped_datamelt,random=~1|Country.Code,correlation=corAR1(form=~year|Country.Code),na.action = na.omit),error=function(e) NULL, warning = function(w) NULL)
    
    assoc[iter,1]<-indicators$Indicator.Code[i]
    assoc[iter,2]<-indicators$Indicator.Code[j]
    
    if (is.null(lmeI1I2)) {
      assoc[iter,c(3:7)]<-rep(NA,5)
    } else {
      assoc[iter,c(3:7)]<-summary(lmeI1I2)$tTable[2,]
    }
    iter<-iter+1
    
  }###j
}###i

save(assoc,file="~//assocEMBE.Rdata")




##########################################################################################
##########################################################################################
#####parse the analysis to development categories to assess whether the challenges are the same

library(nlme)

load("~//datamelt.Rdata")

country<-read.csv("~//SDGCountry.csv",header=TRUE)


names(country)[1]<-"Country.Code"

indicators<-data.frame(indicator=unique(datamelt$Indicator.Code),Indicator.Code=unique(datamelt$Indicator.Code))

income<-unique(country$Income.Group)

ni<-dim(indicators)[1]

repi<-(ni*ni-ni)/2  ##upper triangle


for (m in 1:4) {
  selected<-country[country$Income.Group==income[m],]$Country.Code
  
  datamelts<-datamelt[datamelt$Country.Code %in% selected,]
  
  assoc<-data.frame(Indicator1=rep(indicators$Indicator.Code[1],repi), Indicator2=rep(indicators$Indicator.Code[1],repi), fixed=rep(0,repi), SE=rep(0,repi), df=rep(0,repi), tvalue=rep(0,repi), pvalue=rep(1,repi))
  
  
  iter<-1
  
  
  ##################
  for (i in 1:(ni-1)) {
    for (j in (i+1):ni) {
      
      
      #######################
      
      memdat<-subset(datamelts,Indicator.Code==indicators$Indicator.Code[i]|Indicator.Code==indicators$Indicator.Code[j])
      
      shaped_datamelt<-subset(memdat[,c(1,2,7,8)],Indicator.Code==indicators$Indicator.Code[i])
      colnames(shaped_datamelt)[4]<-as.character(indicators$Indicator.Code[i])
      shaped_datamelt$inter<-memdat[memdat$Indicator.Code==indicators$Indicator.Code[j],]$valued
      
      colnames(shaped_datamelt)[5]<-as.character(indicators$Indicator.Code[j])
      
      shaped_datamelt<-subset(shaped_datamelt,!is.na(shaped_datamelt[,4]) & !(is.na(shaped_datamelt[,5])))
      
      shaped_datamelt[,5]<-scale(shaped_datamelt[,5])
      shaped_datamelt[,4]<-scale(shaped_datamelt[,4])
      
      
      fmla <- as.formula(paste0(as.character(indicators$Indicator.Code[i]),"~",as.character(indicators$Indicator.Code[j])))
      
      lmeI1I2<-tryCatch(lme(fmla,data=shaped_datamelt,random=~1|Country.Code,correlation=corAR1(form=~year|Country.Code),na.action = na.omit),error=function(e) NULL, warning = function(w) NULL)
      
      assoc[iter,1]<-indicators$Indicator.Code[i]
      assoc[iter,2]<-indicators$Indicator.Code[j]
      
      if (is.null(lmeI1I2)) {
        assoc[iter,c(3:7)]<-rep(NA,5)
      } else {
        assoc[iter,c(3:7)]<-summary(lmeI1I2)$tTable[2,]
      }
      iter<-iter+1
      
    }###j
  }###i
  
  filename<-paste("~//assoc",income[m],".Rdata")
  save(assoc,file=filename)
  
}




########################################################
#######################################################

load("~//SDG.Rdata")
sers<-subset(ser,Series.Code!="DT.ODA.ODAT.CD") 	##two code for one indicator .CD and .CD1; from visualisation in most instances 
##the same and when not .CD1>.CD, so chose .CD1

sers$Series.Code<-factor(sers$Series.Code)

sers$Series.Code<-factor(sers$Series.Code,levels=c(levels(sers$Series.Code),"EMBE"))
sers<-rbind(sers,c("EMBE",NA,"disaster",13,"13.1",-1,rep(NA,17)))
sers$Series.Code<-factor(sers$Series.Code)



assoc$SDG1<-sers$SDG[1]
assoc$SDG2<-sers$SDG[1]
assoc$target1<-sers$target[1]
assoc$target2<-sers$target[1]


for (i in 1:dim(assoc)[1]) {
  
  assoc$SDG1[i]<-sers$SDG[sers$Series.Code==assoc$Indicator1[i]]
  assoc$SDG2[i]<-sers$SDG[sers$Series.Code==assoc$Indicator2[i]]
  assoc$target1[i]<-sers$target[sers$Series.Code==assoc$Indicator1[i]]
  assoc$target2[i]<-sers$target[sers$Series.Code==assoc$Indicator2[i]]
  
}


#############################################################
#############################################################
### meta analysis of multiple lmes for each pair of SDG   ### 
#############################################################
#############################################################

##############################################################
##############################################################
##### note here we have attempted a number of other         ## 
##### approaches which led to poor models                   ## 
##### (validation issues, poor fit, etc)                    ##
##### we think the more promising alternative is going to   ## 
##### be ICP, but there still a need for a few developments ##
##### of the methods to deal with the complexity of this    ##
##### data. but we think the inferential gains to using     ##
##### this alternative methodological approach will be      ##
##### substantial. Please have a try if tempted, it will    ##
##### be productive.                                        ##
##############################################################
#############################################################

library(metafor)


met.mat<-data.frame(SDG1=rep(seq(1,17,1),each=17),SDG2=rep(seq(1,17,1),17),beta=rep(0,(17*17)),se=rep(0,(17*17)),zval=rep(0,(17*17)),pval=rep(0,(17*17)),ci.lb=rep(0,(17*17)),ci.ub=rep(0,(17*17)))
iter<-1
for (i in 1:17) {
  for (j in 1:17) {
    assoc.trial<-subset(assoc,(SDG1==i&SDG2==j)|(SDG1==j&SDG2==i))
    assoc.trial<-subset(assoc.trial,pvalue<0.9 & df>9)
    if (sum(!is.na(assoc.trial$fixed))>1){
      #met<-rma(yi=assoc.trial$fixed,sei=assoc.trial$SE,weights=assoc.trial$df,control=list(tol=10^-60)) ##lowered tolerance to deal with the few instances when we have large SEs mixed with very small SEs
      met<-rma(yi=assoc.trial$fixed,sei=assoc.trial$SE,control=list(tol=10^-60,stepadj=0.5,maxiter=1000)) ##lowered tolerance to deal with the few instances when we have large SEs mixed with very small SEs
      met.mat$beta[iter]<-met$beta
      met.mat$se[iter]<-met$se
      met.mat$zval[iter]<-met$zval
      met.mat$pval[iter]<-met$pval
      met.mat$ci.lb[iter]<-met$ci.lb
      met.mat$ci.ub[iter]<-met$ci.ub
    }
    iter<-iter+1
  }}

met.mat$betas<-met.mat$beta
met.mat$betas[met.mat$pval>0.05]<-0
met.mat$source<-paste("SDG",met.mat$SDG1,sep="")
met.mat$target<-paste("SDG",met.mat$SDG2,sep="")

sdgmat<-matrix(met.mat$betas,17,17)
rownames(sdgmat)<-unique(met.mat$source)
colnames(sdgmat)<-unique(met.mat$target)

############################################
###########################################
##QC 

###################################
###target level
#####################################
### meta analytic approach
######################################
library(metafor)

targets<-unique(assoc$target1)
met.mat.tar<-data.frame(target1=rep(unique(assoc$target1),each=length(unique(assoc$target2))),SDG2=rep(unique(assoc$target2),length(unique(assoc$target1))),beta=rep(0,(71*71)),se=rep(0,(71*71)),zval=rep(0,(71*71)),pval=rep(1,(71*71)),ci.lb=rep(0,(71*71)),ci.ub=rep(0,(71*71)))
iter<-1
for (i in 1:71) {
  for (j in 1:71) {
    assoc.trial<-subset(assoc,(target1==targets[i]&target2==targets[j])|(target1==targets[j]&target2==targets[i]))
    assoc.trial<-subset(assoc.trial,pvalue<0.9 & df>9)
    if (sum(!is.na(assoc.trial$fixed))>1){
      #met<-rma(yi=assoc.trial$fixed,sei=assoc.trial$SE,weights=assoc.trial$df,control=list(tol=10^-80,stepadj=0.5,maxiter=1000)) ##lowered tolerance to deal with the few instances when we have large SEs mixed with very small SEs
      met<-rma(yi=assoc.trial$fixed,sei=assoc.trial$SE,control=list(tol=10^-80,stepadj=0.5,maxiter=1000))
      met.mat.tar$beta[iter]<-met$beta
      met.mat.tar$se[iter]<-met$se
      met.mat.tar$zval[iter]<-met$zval
      met.mat.tar$pval[iter]<-met$pval
      met.mat.tar$ci.lb[iter]<-met$ci.lb
      met.mat.tar$ci.ub[iter]<-met$ci.ub
    }
    iter<-iter+1
  }}


met.mat.tar$betas<-met.mat.tar$beta
met.mat.tar$betas[met.mat.tar$pval>0.05]<-0

sdgtarmat<-matrix(met.mat.tar$betas,71,71)
rownames(sdgtarmat)<-targets
colnames(sdgtarmat)<-targets



save(assoc,met.mat,sdgmat,met.mat.tar,sdgtarmat,file="~//metanalysedHIGHMID.Rdata")


library(igraph)
library(qgraph)

sustainome <- graph_from_adjacency_matrix(sdgtarmat,mode="undirected",weighted=TRUE)
eigen_sdgtar<-abs(eigen(sdgtarmat)$vectors[,1])

l <- layout.fruchterman.reingold(graph_from_adjacency_matrix((sdgtarmat),mode="undirected",weighted=TRUE))
plot.igraph(sustainome, layout=l,vertex.size=abs(eigen_sdgtar)*40, edge.curved=0.2,edge.width=abs(E(sustainome)$weight)*20, edge.color=ifelse(E(sustainome)$weight>0, "blue","red"))




filename<-list.files("~//icons")

###icons downloaded from UN SDG site, following the requirements for their use in figure development

library(jpeg)
library(qgraph) ##much better

qgraph(sdgmat,images=paste("~//",filename,sep="")[c(1,10,11,12,13,14,15,16,17,2,3,4,5,6,7,8,9)],labels=F,borders=F,curve=0.2,curveAll=T,edge.color=ifelse(sdgmat>0, "blue","red"),maximum=1,node.resolution=400,filetype="tiff",filename="C://Users//David//Dropbox//WDI_csv//debugged//sustainome")

##just dealing with idiosyncrasies of filenames


###############################################
###############################################
######################################
#####################################
######################################
#####################################

#################################################
##### stability analyses graph Laplacian approach

load("~//metanalysed.Rdata")
sdgmaster<-sdgmat
tarmaster<-sdgtarmat

rm(sdgmat,sdgtarmat)

load("~//metanalysedLOW.Rdata")
sdglow<-sdgmat
tarlow<-sdgtarmat

rm(sdgmat,sdgtarmat)

load("~//metanalysedHIGH.Rdata")
sdghigh<-sdgmat
tarhigh<-sdgtarmat

rm(sdgmat,sdgtarmat)

load("~//metanalysedHIGHMID.Rdata")
sdghighmid<-sdgmat
tarhighmid<-sdgtarmat

rm(sdgmat,sdgtarmat)

load("~//metanalysedLOWMID.Rdata")
sdglowmid<-sdgmat
tarlowmid<-sdgtarmat

rm(sdgmat,sdgtarmat)

##Laplacian
sdglowL<-sdglow-diag(rowSums(sdglow))  ##following Bronski & Deville 2014 SIAM Appl Math (signed graphs) contrary to the usual L=D-A
## and it works as sumRows(sdglowL)= array(0)
sum(which(eigen(sdglowL)$values>0))

sdglowmidL<-sdglowmid-diag(rowSums(sdglowmid)) 
sum(which(eigen(sdglowmidL)$values>0))

sdghighmidL<-sdghighmid-diag(rowSums(sdghighmid)) 
sum(which(eigen(sdghighmidL)$values>0))

sdghighL<-sdghigh-diag(rowSums(sdghigh)) 
sum(which(eigen(sdghighL)$values>0))

sdgmasterL<-sdgmaster-diag(rowSums(sdgmaster)) 
sum(which(eigen(sdgmasterL)$values>0))


##################################################################
#######################################target level

tarlowL<-tarlow-diag(rowSums(tarlow))  ##following Bronski & Deville 2014 SIAM Appl Math (signed graphs) contrary to the usual L=D-A
## and it works as sumRows(tarlowL)= array(0)
sum(which(eigen(tarlowL)$values>0))

tarlowmidL<-tarlowmid-diag(rowSums(tarlowmid)) 
sum(which(eigen(tarlowmidL)$values>0))

tarhighmidL<-tarhighmid-diag(rowSums(tarhighmid)) 
sum(which(eigen(tarhighmidL)$values>0))

tarhighL<-tarhigh-diag(rowSums(tarhigh)) 
sum(which(eigen(tarhighL)$values>0))

tarmasterL<-tarmaster-diag(rowSums(tarmaster)) 
sum(which(eigen(tarmasterL)$values>0))

##########################################################


#adjugate

minor <- function(A, i, j) det( A[-i,-j] )
cofactor <- function(A, i, j) (-1)^(i+j) * minor(A,i,j)

adjoint <- function(A) {
  n <- nrow(A)
  t(outer(1:n, 1:n, Vectorize(
    function(i,j) cofactor(A,i,j)
  )))
}


adjmasterL<-(adjoint(sdgmasterL)[,1]%*%matrix(1,1,17))
adjlowL<-(adjoint(sdglowL)[,1]%*%matrix(1,1,17))
adjlowmidL<-(adjoint(sdglowmidL)[,1]%*%matrix(1,1,17))
adjhighmidL<-(adjoint(sdghighmidL)[,1]%*%matrix(1,1,17))
adjhighL<-(adjoint(sdghighL)[,1]%*%matrix(1,1,17))

SSmaster<-adjmasterL%*%matrix(1,17,1)
SSlow<-adjlowL%*%matrix(1,17,1)
SSlowmid<-adjlowmidL%*%matrix(1,17,1)
SShighmid<-adjhighmidL%*%matrix(1,17,1)
SShigh<-adjhighL%*%matrix(1,17,1)


##########################
#### target Laplacian
##Laplacian
tarlowL<-tarlow-diag(rowSums(tarlow))  ##following Bronski & Deville 2014 SIAM Appl Math (signed graphs) contrary to the usual L=D-A
## and it works as sumRows(tarlowL)= array(0)
length(which(eigen(tarlowL)$values>0))

tarlowmidL<-tarlowmid-diag(rowSums(tarlowmid)) 
length(which(eigen(tarlowmidL)$values>0))

tarhighmidL<-tarhighmid-diag(rowSums(tarhighmid)) 
length(which(eigen(tarhighmidL)$values>0))

tarhighL<-tarhigh-diag(rowSums(tarhigh)) 
length(which(eigen(tarhighL)$values>0))

tarmasterL<-tarmaster-diag(rowSums(tarmaster)) 
length(which(eigen(tarmasterL)$values>0))


adjtarmasterL<-(adjoint(tarmasterL)[,1]%*%matrix(1,1,71))
adjtarlowL<-(adjoint(tarlowL)[,1]%*%matrix(1,1,71))
adjtarlowmidL<-(adjoint(tarlowmidL)[,1]%*%matrix(1,1,71))
adjtarhighmidL<-(adjoint(tarhighmidL)[,1]%*%matrix(1,1,71))
adjtarhighL<-(adjoint(tarhighL)[,1]%*%matrix(1,1,71))

SStarmaster<-adjtarmasterL%*%matrix(1,71,1)
SStarlow<-adjtarlowL%*%matrix(1,71,1)
SStarlowmid<-adjtarlowmidL%*%matrix(1,71,1)
SStarhighmid<-adjtarhighmidL%*%matrix(1,71,1)
SStarhigh<-adjtarhighL%*%matrix(1,71,1)

t(SStarmaster)
t(SStarlow)
t(SStarlowmid)
t(SStarhighmid)
t(SStarhigh)



fmf<-rowSums(sdgmat*(sdgmat>0))-abs(rowSums(sdgmat*(sdgmat<0)))
eee<-eigen(sdgmat)$vectors[,1]
plot(fmf~eee)

###############################################################################################################
###############################################################################################################
#####################       end of part I
###############################################################################################################
###############################################################################################################


###############################################################################################################
###############################################################################################################
#####################       part II: visualisation
###############################################################################################################
###############################################################################################################

library(grid)
library(gridExtra)
library(jpeg)
library(tiff)
library(qgraph)
library(png)
library(magick)
library(ggplot2)

###########################################
#####visualising sustainomes and strength portraits


load("~//metanalysed.Rdata")
sdgmaster<-sdgmat
tarmaster<-sdgtarmat

rm(sdgmat,sdgtarmat)

load("~//metanalysedLOW.Rdata")
sdglow<-sdgmat
tarlow<-sdgtarmat

rm(sdgmat,sdgtarmat)

load("~//metanalysedHIGH.Rdata")
sdghigh<-sdgmat
tarhigh<-sdgtarmat

rm(sdgmat,sdgtarmat)

load("~//metanalysedHIGHMID.Rdata")
sdghighmid<-sdgmat
tarhighmid<-sdgtarmat

rm(sdgmat,sdgtarmat)

load("~//metanalysedLOWMID.Rdata")
sdglowmid<-sdgmat
tarlowmid<-sdgtarmat

rm(sdgmat,sdgtarmat)


##############################################
##############################################
#####target level

sdgcolor<-data.frame(R=c(229,221,76,197,255,38,252,162,253,221,253,191,63,10,86,0,25),G=c(36,166,159,25,58,189,195,25,105,19,157,139,126,141,192,104,72),B=c(59,58,56,45,33,226,11,66,37,103,36,46,68,217,43,157,106))

####values taken from UN guide on SDG icons 

Dcolor<-rgb(sdgcolor,max=255)
colortar<-array(Dcolor[1],71)
for (i in 1:71) {
  colortar[i]<-Dcolor[as.numeric(assoc$SDG1[which(assoc$target1==colnames(tarlow)[i])[1]])]
}

targets<-data.frame(target=colnames(tarlow),color=colortar,sdg=1)

for (i in 1:71) {
  targets$sdg[i]<-as.numeric(assoc$SDG1[which(assoc$target1==targets$target[i])][1])
  
}
targets$sdgF<-as.factor(targets$sdg)

sdgtarlow.ord<-tarlow[order(targets$sdg,targets$target),order(targets$sdg,targets$target)]
sdgtarlowmid.ord<-tarlowmid[order(targets$sdg,targets$target),order(targets$sdg,targets$target)]
sdgtarhighmid.ord<-tarhighmid[order(targets$sdg,targets$target),order(targets$sdg,targets$target)]
sdgtarhigh.ord<-tarhigh[order(targets$sdg,targets$target),order(targets$sdg,targets$target)]



filename<-list.files("C://Users//David//Dropbox//WDI_csv//icons/icons 2019/icons 2019 png/")


##########################################################
##########################################################
##########################################################
#####Figure 1 2019
###########################################################
#############################################################

###tiff not allowed so going pdf
##get filename list first

sdgnsize<-abs(eigen(sdgmaster)$vectors[,1])

qgraph(sdgmaster,vsize=sdgnsize*30,images=paste("~//icons 2019 png//",filename,sep=""),labels=F,borders=F,curve=0.2,curveAll=T,edge.color=ifelse(sdgmaster>0, "blue","red"),maximum=1,node.resolution=400,filetype="pdf",filename="C://Users//David//Dropbox//WDI_csv//debugged//sustainome 2019")


#######
#######
#######
######
####figures for behind the paper piece


qgraph(sdghigh,vsize=8,images=paste("~//icons 2019 png//",filename,sep=""),labels=F,borders=F,curve=0.2,curveAll=T,edge.color=ifelse(sdghigh>0, "blue","red"),maximum=1,node.resolution=400,filetype="tiff",filename="~//sustainome high behind the paper 2019")

qgraph(sdglow,vsize=8,images=paste("~//icons 2019 png//",filename,sep=""),labels=F,borders=F,curve=0.2,curveAll=T,edge.color=ifelse(sdglow>0, "blue","red"),maximum=1,node.resolution=400,filetype="tiff",filename="~//sustainome low behind the paper 2019")
qgraph(sdghighmid,vsize=8,images=paste("~//icons 2019 png//",filename,sep=""),labels=F,borders=F,curve=0.2,curveAll=T,edge.color=ifelse(sdghighmid>0, "blue","red"),maximum=1,node.resolution=400,filetype="tiff",filename="~//sustainome highmid behind the paper 2019")
qgraph(sdglowmid,vsize=8,images=paste("~//icons 2019 png//",filename,sep=""),labels=F,borders=F,curve=0.2,curveAll=T,edge.color=ifelse(sdglowmid>0, "blue","red"),maximum=1,node.resolution=400,filetype="tiff",filename="~//sustainome lowmid behind the paper 2019")

p1<-rasterGrob(readTIFF("~//sustainome low behind the paper 2019.tiff"))
p2<-rasterGrob(readTIFF("~//sustainome lowmid behind the paper 2019.tiff"))
p3<-rasterGrob(readTIFF("~//sustainome highmid behind the paper 2019.tiff"))
p4<-rasterGrob(readTIFF("C://Users//David//Dropbox//WDI_csv//debugged//sustainome high behind the paper 2019.tiff"))


tiff(file="~//Figure behind the paper 2019.tiff",width=9,height=9,units ="in",res=400) 
grid.arrange(arrangeGrob(textGrob("low income countries",just="left",gp=gpar(fontsize=8)),p1,nrow=2,ncol=1,heights=c(.1,2)),arrangeGrob(textGrob("lower middle income countries",just="left",gp=gpar(fontsize=8)),p2,nrow=2,ncol=1,heights=c(.1,2)),arrangeGrob(textGrob("higher middle income countries",just="left",gp=gpar(fontsize=8)),p3,nrow=2,ncol=1,heights=c(.1,2)),arrangeGrob(textGrob("high income countries",just="left",gp=gpar(fontsize=8)),p4,nrow=2,ncol=1,heights=c(.1,2)),nrow=2,ncol=2)
dev.off()



jpeg(file="~//Figure behind the paper 2019.jpeg",quality=100,width=12,height=3,units ="in",res=400) 
grid.arrange(arrangeGrob(textGrob("low income countries",just="left",gp=gpar(fontsize=8)),p1,nrow=2,ncol=1,heights=c(.1,2)),arrangeGrob(textGrob("lower middle income countries",just="left",gp=gpar(fontsize=8)),p2,nrow=2,ncol=1,heights=c(.1,2)),arrangeGrob(textGrob("higher middle income countries",just="left",gp=gpar(fontsize=8)),p3,nrow=2,ncol=1,heights=c(.1,2)),arrangeGrob(textGrob("high income countries",just="left",gp=gpar(fontsize=8)),p4,nrow=2,ncol=1,heights=c(.1,2)),nrow=1,ncol=4)
dev.off()



#######
#######
#######
#######
#####target level

sdgcolor<-data.frame(R=c(229,221,76,197,255,38,252,162,253,221,253,191,63,10,86,0,25),G=c(36,166,159,25,58,189,195,25,105,19,157,139,126,141,192,104,72),B=c(59,58,56,45,33,226,11,66,37,103,36,46,68,217,43,157,106))
Dcolor<-rgb(sdgcolor,max=255)
colortar<-array(Dcolor[1],71)
for (i in 1:71) {
  colortar[i]<-Dcolor[as.numeric(assoc$SDG1[which(assoc$target1==colnames(tarlow)[i])[1]])]
}

targets<-data.frame(target=colnames(tarlow),color=colortar,sdg=1)

for (i in 1:71) {
  targets$sdg[i]<-as.numeric(assoc$SDG1[which(assoc$target1==targets$target[i])][1])
  
}
targets$sdgF<-as.factor(targets$sdg)

sdgtar.ord<-tarmaster[order(targets$sdg,targets$target),order(targets$sdg,targets$target)]

sdgnsize<-abs(eigen(sdgtar.ord)$vectors[,1])
qgraph(sdgtar.ord,vsize=sdgnsize*30,color=targets$color[order(targets$sdg,targets$target)],curve=0.2,curveAll=T,edge.color=ifelse(sdgtar.ord>0, "blue","red"),maximum=1,label.font=2,node.resolution=400,filetype="pdf",filename="~//sustainometargets 2019")

####################################
p1<-rasterGrob(image_read_pdf("~//sustainome 2019.pdf"))
p2<-rasterGrob(image_read_pdf("~//sustainometargets 2019.pdf"))

pdf(file="~//Figure1 2019.pdf",width=3.5,height=1.8,compress=FALSE) 
grid.arrange(arrangeGrob(textGrob("a",just="left",gp=gpar(fontsize=8)),p1,nrow=2,ncol=1,heights=c(.1,2)),arrangeGrob(textGrob("b",just="left",gp=gpar(fontsize=8)),p2,nrow=2,ncol=1,heights=c(.1,2)),nrow=1,ncol=2)
dev.off()

##########################################################
##########################################################


######################################################################################################
######################################################################################################
################   PART III: reactivity and income-specific visualisation                   ##########
######################################################################################################
######################################################################################################


########################################################
########################################################
###REACTIVITY
########################################################
########################################################
filenames<-c("~//metanalysedLOW.Rdata","~//metanalysedLOWMID.Rdata","~//metanalysedHIGHMID.Rdata","~//metanalysedHIGH.Rdata")

sdgmat.all<-array(0,dim=c(length(filenames),17,17))
tarmat.all<-array(0,dim=c(length(filenames),71,71))

react.sdgmat.all<-array(0,dim=c(length(filenames),17,17))
react.tarmat.all<-array(0,dim=c(length(filenames),71,71))

##let's make a tensor, will be easier

###declare index names
dimnames(sdgmat.all)[[1]]<-c("LOW","LOWMID","HIGHMID","HIGH")
dimnames(sdgmat.all)[[2]]<-colnames(sdgmaster) 
dimnames(sdgmat.all)[[3]]<-colnames(sdgmaster)

dimnames(tarmat.all)[[1]]<-c("LOW","LOWMID","HIGHMID","HIGH")
dimnames(tarmat.all)[[2]]<-colnames(tarmaster) 
dimnames(tarmat.all)[[3]]<-colnames(tarmaster)

dimnames(react.sdgmat.all)[[1]]<-c("LOW","LOWMID","HIGHMID","HIGH")
dimnames(react.sdgmat.all)[[2]]<-colnames(sdgmaster) 
dimnames(react.sdgmat.all)[[3]]<-colnames(sdgmaster)

dimnames(react.tarmat.all)[[1]]<-c("LOW","LOWMID","HIGHMID","HIGH")
dimnames(react.tarmat.all)[[2]]<-colnames(tarmaster) 
dimnames(react.tarmat.all)[[3]]<-colnames(tarmaster)

rm(sdgmat,sdgtarmat)
##########################################

for (f in 1:length(filenames)) {
  
  load(filenames[f])
  sdgmat.all[f,,]<-sdgmat
  tarmat.all[f,,]<-sdgtarmat
  
  U<-eigen(sdgmat)$vectors[,1]
  u<-abs(Re(U))
  uu<-eigen(sdgmat)$vectors
  v<-abs(Re(solve(Conj(uu))[1,]))
  react.sdgmat.all[f,,]<-v%o%u   #reactivity of lambda1
  
  tU<-eigen(sdgtarmat)$vectors[,1]
  tu<-abs(Re(tU))
  tuu<-eigen(sdgtarmat)$vectors
  tv<-abs(Re(solve(Conj(tuu))[1,]))
  react.tarmat.all[f,,]<-tv%o%tu   #reactivity of lambda1
  
  rm(sdgmat,sdgtarmat)
}

income<-c("low income countries","low middle income countries", "high middle income countries", "high income countries")

df<-data.frame(sdg=colnames(react.sdgmat.all[4,,]),react=colSums(react.sdgmat.all[4,,]))
o<-order(df$react)
dfo<-df[order(df$react),]
dfo$sdg <- factor(dfo$sdg, levels = c(as.character(df$sdg[o])))

library(gplots)

breakcolors = c(seq(-2,-.15,length=10),seq(-.1,0,length=2),seq(.05,0.15,length=2),seq(0.2,.8,length=2),seq(1,33,length=10))
pall<-colorRampPalette(c('#ca0020','#f4a582','#f7f7f7','#92c5de','#0571b0'))(n = 25)

####################
lmat <- rbind( c(5,3,4), c(2,1,4) )
lhei <- c(.5, 10)
lwid <- c(1, 10, .5)

library(SDMTools)

p3<-ggplot(data=dfo, aes(x=sdg, y=react)) +
  geom_bar(stat="identity", fill=Dcolor[o])+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90,size=5),axis.text.y=element_text(size=5),axis.title=element_text(size=8))+
  labs(y="Reactivity") +
  theme(axis.title.x=element_blank())

pnts = cbind(x =c(0,0.1,0.1,0), y =c(.7,.7,0.2,0.2))  
legend.gradient(pnts,cols=pall,limits=breakcolors,skip=4,title="Legend")


##########################################################
######Figure 3 high income

rects<-data.frame(x=1:length(breakcolors[-2]),color=pall,text= breakcolors[-2],text2=NA)
rects$text2[c(1,8,11,16,25)]<-round(rects$text[c(1,8,11,16,25)],1)


p4<-ggplot(rects,aes(x, y = 0, fill = color, label = text2)) +
  geom_tile(width = 1, height = 1) + # make square tiles
  geom_text(color = "black",size=2) + # add white text in the middle
  scale_fill_identity(guide = "none") + # color the tiles with the colors in the data frame
  coord_fixed() + # make sure tiles are square
  theme_void()


sdgnsize<-abs(eigen(sdghigh)$vectors[,1])
qgraph(sdghigh,vsize=sdgnsize*15,images=paste("~//icons 2019 png//",filename,sep=""),labels=F,borders=F,curve=0.2,curveAll=T,edge.color=ifelse(sdghigh>0, "blue","red"),maximum=1,node.resolution=400,filetype="pdf",filename="~//sustainomehigh 2019b")


p1<-rasterGrob(image_read_pdf("~//sustainomehigh 2019b.pdf"))
p2<-rasterGrob(readTIFF("~//Figure3sim.tif"))

pdf(file="~//Figure3 2019.pdf",width=7.2,height=4,compress=FALSE) 
grid.arrange(arrangeGrob(p1,top=grid.text("a")),arrangeGrob(arrangeGrob(p3,top=grid.text("b")),p4,arrangeGrob(p2,bottom=grid.text("c"),right=grid.text("SDG affected", rot=90,hjust=0,vjust=0,gp=gpar(fontsize=8))),nrow=3,heights=c(1,.1,2)),nrow=1,ncol=2,widths=c(2,1))
dev.off()

##########################################################
##########################################################


##########################################################
######Figure 4 low income

sdgnsize<-abs(eigen(sdglow)$vectors[,1])
qgraph(sdglow,vsize=sdgnsize*15,images=paste("~//icons 2019 png//",filename,sep=""),labels=F,borders=F,curve=0.2,curveAll=T,edge.color=ifelse(sdglow>0, "blue","red"),maximum=1,node.resolution=400,filetype="pdf",filename="~//sustainomelow 2019")

lmat <- rbind( c(5,3,4), c(2,1,4) )
lhei <- c(.5, 10)
lwid <- c(1, 10, .5)
breakcolors = c(seq(-1,-.1,length=1),seq(0,.1,length=1),seq(1,1,length=1))
pall<-colorRampPalette(c('#ca0020','#f7f7f7','#0571b0'))(n = 2)


p1<-rasterGrob(image_read_pdf("~//sustainomelow 2019.pdf"))
p2<-rasterGrob(readTIFF("~//Figure4simlow.tif"))



df<-data.frame(sdg=colnames(react.sdgmat.all[1,,]),react=colSums(react.sdgmat.all[1,,]))
o<-order(df$react)
dfo<-df[order(df$react),]
dfo$sdg <- factor(dfo$sdg, levels = c(as.character(df$sdg[o])))


p3<-ggplot(data=dfo, aes(x=sdg, y=react)) +
  geom_bar(stat="identity", fill=Dcolor[o])+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90,size=5),axis.text.y=element_text(size=5),axis.title=element_text(size=8))+
  labs(y="Reactivity") +
  theme(axis.title.x=element_blank())


breakcolors = c("-Inf",0,"+Inf")
pall<-colorRampPalette(c('#ca0020','#f7f7f7','#0571b0'))(n = 3)

rects<-data.frame(x=1:length(breakcolors),color=pall,text= breakcolors,text2=NA)


p4<-ggplot(rects,aes(x, y = 0, fill = color, label = text)) +
  geom_tile(width = 1, height = 1) + # make square tiles
  geom_text(color = "black",size=1) + # add white text in the middle
  scale_fill_identity(guide = "none") + # color the tiles with the colors in the data frame
  coord_fixed() + # make sure tiles are square
  theme_void()



pdf(file="~//Figure4 2019.pdf",width=7.2,height=4,compress=FALSE) 
grid.arrange(arrangeGrob(p1,top=grid.text("a")),arrangeGrob(arrangeGrob(p3,top=grid.text("b")),p4,arrangeGrob(p2,bottom=grid.text("c"),right=grid.text("SDG affected", rot=90,hjust=0,vjust=0,gp=gpar(fontsize=8))),nrow=3,heights=c(1,.1,2)),nrow=1,ncol=2,widths=c(2,1))
dev.off()

##########################################################
##########################################################


##########################################################
#####supplementary figure s1 low middle
sdgnsize<-abs(eigen(sdglowmid)$vectors[,1])
qgraph(sdglowmid,vsize=sdgnsize*30,images=paste("~//icons 2019 png//",filename,sep=""),labels=F,borders=F,curve=0.2,curveAll=T,edge.color=ifelse(sdglowmid>0, "blue","red"),maximum=1,node.resolution=400,filetype="pdf",filename="~//sustainomelowmid 2019")


p1<-rasterGrob(image_read_pdf("~//sustainomelowmid 2019.pdf"))
p2<-rasterGrob(readTIFF("~//Figsimlowmid.tif"))



df<-data.frame(sdg=colnames(react.sdgmat.all[2,,]),react=colSums(react.sdgmat.all[2,,]))
o<-order(df$react)
dfo<-df[order(df$react),]
dfo$sdg <- factor(dfo$sdg, levels = c(as.character(df$sdg[o])))


p3<-ggplot(data=dfo, aes(x=sdg, y=react)) +
  geom_bar(stat="identity", fill=Dcolor[o])+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90,size=5),axis.text.y=element_text(size=5),axis.title=element_text(size=8))+
  labs(y="Reactivity") +
  theme(axis.title.x=element_blank())


breakcolors = c("-Inf",0,"+Inf")
pall<-colorRampPalette(c('#ca0020','#f7f7f7','#0571b0'))(n = 3)

rects<-data.frame(x=1:length(breakcolors),color=pall,text= breakcolors,text2=NA)
#rects$text2[c(1,8,11,16,25)]<-round(rects$text[c(1,8,11,16,25)],1)


p4<-ggplot(rects,aes(x, y = 0, fill = color, label = text)) +
  geom_tile(width = 1, height = 1) + # make square tiles
  geom_text(color = "black",size=1) + # add white text in the middle
  scale_fill_identity(guide = "none") + # color the tiles with the colors in the data frame
  coord_fixed() + # make sure tiles are square
  theme_void()



tiff(file="~//supp fig s1.tif",units="mm",width=183,height=103,res=400) 
grid.arrange(arrangeGrob(p1,top=grid.text("a")),arrangeGrob(arrangeGrob(p3,top=grid.text("b")),p4,arrangeGrob(p2,bottom=grid.text("c"),right=grid.text("SDG affected", rot=90,hjust=0,vjust=0,gp=gpar(fontsize=8))),nrow=3,heights=c(1,.1,2)),nrow=1,ncol=2,widths=c(2,1))
dev.off()

##########################################################
##########################################################


##########################################################
#####supplementary figure s2 high middle

sdgnsize<-abs(eigen(sdghighmid)$vectors[,1])
qgraph(sdghighmid,vsize=sdgnsize*30,images=paste("~//icons 2019 png//",filename,sep=""),labels=F,borders=F,curve=0.2,curveAll=T,edge.color=ifelse(sdghighmid>0, "blue","red"),maximum=1,node.resolution=400,filetype="pdf",filename="~//sustainomehighmid 2019")


p1<-rasterGrob(image_read_pdf("~//sustainomehighmid 2019.pdf"))
p2<-rasterGrob(readTIFF("~//Figsimhighmid.tif"))



df<-data.frame(sdg=colnames(react.sdgmat.all[3,,]),react=colSums(react.sdgmat.all[3,,]))
o<-order(df$react)
dfo<-df[order(df$react),]
dfo$sdg <- factor(dfo$sdg, levels = c(as.character(df$sdg[o])))


p3<-ggplot(data=dfo, aes(x=sdg, y=react)) +
  geom_bar(stat="identity", fill=Dcolor[o])+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90,size=5),axis.text.y=element_text(size=5),axis.title=element_text(size=8))+
  labs(y="Reactivity") +
  theme(axis.title.x=element_blank())


breakcolors = c("-Inf",0,"+Inf")
pall<-colorRampPalette(c('#ca0020','#f7f7f7','#0571b0'))(n = 3)

rects<-data.frame(x=1:length(breakcolors),color=pall,text= breakcolors,text2=NA)
#rects$text2[c(1,8,11,16,25)]<-round(rects$text[c(1,8,11,16,25)],1)


p4<-ggplot(rects,aes(x, y = 0, fill = color, label = text)) +
  geom_tile(width = 1, height = 1) + # make square tiles
  geom_text(color = "black",size=1) + # add white text in the middle
  scale_fill_identity(guide = "none") + # color the tiles with the colors in the data frame
  coord_fixed() + # make sure tiles are square
  theme_void()



tiff(file="~//supp fig s2.tif",units="mm",width=183,height=103,res=400) 
grid.arrange(arrangeGrob(p1,top=grid.text("a")),arrangeGrob(arrangeGrob(p3,top=grid.text("b")),p4,arrangeGrob(p2,bottom=grid.text("c"),right=grid.text("SDG affected", rot=90,hjust=0,vjust=0,gp=gpar(fontsize=8))),nrow=3,heights=c(1,.1,2)),nrow=1,ncol=2,widths=c(2,1))
dev.off()

##########################################################
##########################################################



#######################################################################################################
#######################################################################################################
#######################   sustainomic simulations (outcomes needed for figures above)   ###############
#######################################################################################################
#######################################################################################################


####simulation of SDG change based on sustainome

################################################
####outcome across income groups

filenames<-c("~//metanalysedLOW.Rdata","~//metanalysedLOWMID.Rdata","~//metanalysedHIGHMID.Rdata","~//metanalysedHIGH.Rdata")

nudge<-c(0,.1)
sdgprog.out<-array(0,dim=c(length(filenames),17,17))
tarprog.out<-array(0,dim=c(length(filenames),71,71))


for (f in 1:length(filenames)) {
  
  load(filenames[f])
  iter<-1000
  progress<-array(0,dim=c(17,iter,17,length(nudge)))
  
  for (n in 1:2) {
    for (j in 1:17) {
      
      progress[1:17,1,j,n]<-array(1,17)
      #progress[j,1,j,n]<-progress[j,1,j,n]+nudge[n]
      
      for (i in 2:iter) {
        progress[j,(i-1),j,n]<-progress[j,(i-1),j,n]+nudge[n]
        progress[,i,j,n]<-sdgmat%*%progress[,(i-1),j,n]
        
      } #end i  
    } #end j
  } #end n
  
  sdgprog.out[f,,]<-progress[,iter,,2] ############DIFF HERE
  
  dimnames(sdgprog.out)[[1]]<-c("LOW","LOWMID","HIGHMID","HIGH")
  dimnames(sdgprog.out)[[2]]<-colnames(sdgmat) 
  dimnames(sdgprog.out)[[3]]<-colnames(sdgmat)
  
  
  iter<-200
  progress<-array(0,dim=c(71,iter,71,length(nudge)))
  
  for (n in 1:2) {
    for (j in 1:71) {
      
      progress[1:71,1,j,n]<-array(1,71)
      
      for (i in 2:iter) {
        progress[j,(i-1),j,n]<-progress[j,(i-1),j,n]+nudge[n]
        
        progress[,i,j,n]<-sdgtarmat%*%progress[,(i-1),j,n]
        
      } #end i  
    } #end j
  } #end n
  
  tarprog.out[f,,]<-progress[,iter,,2]    ##############DIFF HERE
  
  dimnames(tarprog.out)[[1]]<-c("LOW","LOWMID","HIGHMID","HIGH")
  dimnames(tarprog.out)[[2]]<-colnames(sdgtarmat) 
  dimnames(tarprog.out)[[3]]<-colnames(sdgtarmat)
  
  
} #end f


##############################################################

######################
##let's get the colours
##
load("~//metanalysedHIGH.Rdata") 
##

sdgcolor<-data.frame(R=c(229,221,76,197,255,38,252,162,253,221,253,191,63,10,86,0,25),G=c(36,166,159,25,58,189,195,25,105,19,157,139,126,141,192,104,72),B=c(59,58,56,45,33,226,11,66,37,103,36,46,68,217,43,157,106))
Dcolor<-rgb(sdgcolor,max=255)
colortar<-array(Dcolor[1],71)

for (i in 1:71) {
  colortar[i]<-Dcolor[as.numeric(assoc$SDG1[which(assoc$target1==colnames(tarprog.out)[i])[1]])]
}

targets<-data.frame(target=colnames(tarprog.out),color=colortar,sdg=1)

for (i in 1:71) {
  targets$sdg[i]<-as.numeric(assoc$SDG1[which(assoc$target1==targets$target[i])][1])
  
}
targets$sdgF<-as.factor(targets$sdg)

rm(assoc)




library(gplots)
heatmap.2(sign(tarprog.out[1,,]),dendrogram="none",Colv=TRUE,sepcolor="none",col=pall,key=FALSE,notecex=0,trace="none",colCol=targets$color)
heatmap.2(sign(sdgprog.out[1,,]),dendrogram="none",Colv=TRUE,sepcolor="none",col=pall,key=FALSE,notecex=0,trace="none",colCol=targets$color[!duplicated(targets$sdg)][order(targets$sdg[!duplicated(targets$sdg)])])



library(RColorBrewer)

breakcolors = c(seq(-2,-.15,length=10),seq(-.1,0,length=2),seq(.05,0.15,length=2),seq(0.2,.8,length=2),seq(1,33,length=10))
pall<-colorRampPalette(c('#ca0020','#f4a582','#f7f7f7','#92c5de','#0571b0'))(n = 25)
heatmap.2((sdgprog.out[4,,]),dendrogram="none",Colv=FALSE,sepcolor="none",breaks=breakcolors,col=pall,key=FLASE,notecex=0,trace="none",colCol=targets$color[!duplicated(targets$sdg)][order(targets$sdg[!duplicated(targets$sdg)])])

######################################################
#highjacking legend.gradient from library(SDMTools)

##### thanks SDMTools


legend.gradient<-function (pnts, cols = heat.colors(100), limits = limits, skip=1,title = "Legend", 
                           ...) 
{
  seqq<-seq(1,length(limits),skip)
  
  yvals = seq(min(pnts[, 2]), max(pnts[, 2]), length = length(limits))
  for (i in 1:length(cols)) {
    polygon(x = pnts[, 1], y = c(yvals[i], yvals[i], yvals[i + 
                                                             1], yvals[i + 1]), col = cols[i], border = F)
  }
  
  
  for (i in 1:length(seqq)){
    text(max(pnts[, 1]), yvals[seqq[i]], labels = round(limits[seqq[i]],1), 
         pos = 4, ...)
    
  }	
  text(min(pnts[, 1]), max(pnts[, 2]), labels = title, adj = c(0, 
                                                               -1), ...)
}



###############################################################
######special case for sdg high income
tiff(filename = "~//simulation high income.tif",units="in",width = 7, height = 7, res=100)

breakcolors = c(seq(-2,-.15,length=10),seq(-.1,0,length=2),seq(.05,0.15,length=2),seq(0.2,.8,length=2),seq(1,33,length=10))
pall<-colorRampPalette(c('#ca0020','#f4a582','#f7f7f7','#92c5de','#0571b0'))(n = 25)
heatmap.2((sdgprog.out[4,,]), xlab="SDG on which intervention takes place",ylab="SDG affected",dendrogram="none",Colv=FALSE,sepcolor="none",breaks=breakcolors,col=pall,key=FALSE,notecex=0,trace="none",colCol=Dcolor,colRow=Dcolor)

pnts = cbind(x =c(0,0.1,0.1,0), y =c(.7,.7,0.2,0.2))  

legend.gradient(pnts,cols=pall,limits=breakcolors,skip=4,title="Legend")

dev.off()

###############################################################
######other incomes
##low
tiff(filename = "~//simulation low income.tif",units="in",width = 7, height = 7, res=100)

breakcolors = c(seq(-1,-.1,length=1),seq(0,.1,length=1),seq(1,1,length=1))
pall<-colorRampPalette(c('#ca0020','#f7f7f7','#0571b0'))(n = 2)
heatmap.2(sign(sdgprog.out[1,,]), xlab="SDG on which intervention takes place",ylab="SDG affected",dendrogram="none",Colv=FALSE,sepcolor="none",breaks=breakcolors,col=pall,key=FALSE,notecex=0,trace="none",colCol=Dcolor,colRow=Dcolor)

pnts = cbind(x =c(0,0.1,0.1,0), y =c(.7,.7,0.2,0.2))  

legend.gradient(pnts,cols=pall,limits=breakcolors,skip=1,title="Legend")


dev.off()


##midlow
tiff(filename = "~//simulation midlow income.tif",units="in",width = 7, height = 7, res=100)

breakcolors = c(seq(-1,-.1,length=1),seq(0,.1,length=1),seq(1,1,length=1))
pall<-colorRampPalette(c('#ca0020','#f7f7f7','#0571b0'))(n = 2)
heatmap.2(sign(sdgprog.out[2,,]), xlab="SDG on which intervention takes place",ylab="SDG affected",dendrogram="none",Colv=FALSE,sepcolor="none",breaks=breakcolors,col=pall,key=FALSE,notecex=0,trace="none",colCol=Dcolor,colRow=Dcolor)

pnts = cbind(x =c(0,0.1,0.1,0), y =c(.7,.7,0.2,0.2))  

legend.gradient(pnts,cols=pall,limits=breakcolors,skip=1,title="Legend")


dev.off()


##midhigh
tiff(filename = "~//simulation midhigh income.tif",units="in",width = 7, height = 7, res=100)

breakcolors = c(seq(-1,-.1,length=1),seq(0,.1,length=1),seq(1,1,length=1))
pall<-colorRampPalette(c('#ca0020','#f7f7f7','#0571b0'))(n = 2)
heatmap.2(sign(sdgprog.out[3,,]), xlab="SDG on which intervention takes place",ylab="SDG affected",dendrogram="none",Colv=FALSE,sepcolor="none",breaks=breakcolors,col=pall,key=FALSE,notecex=0,trace="none",colCol=Dcolor,colRow=Dcolor)

pnts = cbind(x =c(0,0.1,0.1,0), y =c(.7,.7,0.2,0.2))  

legend.gradient(pnts,cols=pall,limits=breakcolors,skip=1,title="Legend")


dev.off()


#####################targets
##high
tiff(filename = "~//simulation target high income.tif",units="in",width = 7, height = 7, res=100)

breakcolors = c(seq(-1,-.1,length=1),seq(-.0001,.1,length=2),seq(1,1,length=1))
pall<-colorRampPalette(c('#ca0020','#f7f7f7','#0571b0'))(n = 3)
heatmap.2(sign(tarprog.out[4,,]),xlab="Target on which intervention takes place",ylab="Target affected",dendrogram="none",Colv=FALSE,sepcolor="none",breaks=breakcolors,col=pall,key=FALSE,notecex=0,trace="none",colCol=targets$color,colRow=targets$color)

pnts = cbind(x =c(0,0.1,0.1,0), y =c(.7,.7,0.2,0.2))  

legend.gradient(pnts,cols=pall,limits=breakcolors,skip=3,title="Legend")


dev.off()



##low
tiff(filename = "~//simulation target low income.tif",units="in",width = 7, height = 7, res=100)

breakcolors = c(seq(-1,-.1,length=1),seq(-.0001,.1,length=2),seq(1,1,length=1))
pall<-colorRampPalette(c('#ca0020','#f7f7f7','#0571b0'))(n = 3)
heatmap.2(sign(tarprog.out[1,,]), xlab="Target on which intervention takes place",ylab="Target affected",dendrogram="none",Colv=FALSE,sepcolor="none",breaks=breakcolors,col=pall,key=FALSE,notecex=0,trace="none",colCol=targets$color,colRow=targets$color)

pnts = cbind(x =c(0,0.1,0.1,0), y =c(.7,.7,0.2,0.2))  

legend.gradient(pnts,cols=pall,limits=breakcolors,skip=3,title="Legend")


dev.off()


##midlow
tiff(filename = "~//simulation target midlow income.tif",units="in",width = 7, height = 7, res=100)

breakcolors = c(seq(-1,-.1,length=1),seq(-.0001,.1,length=2),seq(1,1,length=1))
pall<-colorRampPalette(c('#ca0020','#f7f7f7','#0571b0'))(n = 3)
heatmap.2(sign(tarprog.out[2,,]), xlab="Target on which intervention takes place",ylab="Target affected",dendrogram="none",Colv=FALSE,sepcolor="none",breaks=breakcolors,col=pall,key=FALSE,notecex=0,trace="none",colCol=targets$color,colRow=targets$color)

pnts = cbind(x =c(0,0.1,0.1,0), y =c(.7,.7,0.2,0.2))  

legend.gradient(pnts,cols=pall,limits=breakcolors,skip=3,title="Legend")


dev.off()


##midhigh
tiff(filename = "~//simulation target midhigh income.tif",units="in",width = 7, height = 7, res=100)

breakcolors = c(seq(-1,-.1,length=1),seq(-.0001,.1,length=2),seq(1,1,length=1))
pall<-colorRampPalette(c('#ca0020','#f7f7f7','#0571b0'))(n = 3)
heatmap.2(sign(tarprog.out[3,,]), xlab="Target on which intervention takes place",ylab="Target affected",dendrogram="none",Colv=FALSE,sepcolor="none",breaks=breakcolors,col=pall,key=FALSE,notecex=0,trace="none",colCol=targets$color,colRow=targets$color)

pnts = cbind(x =c(0,0.1,0.1,0), y =c(.7,.7,0.2,0.2))  

legend.gradient(pnts,cols=pall,limits=breakcolors,skip=3,title="Legend")


dev.off()