# exploration
# read data

df<-read.csv(file="data/eandis/p0269_resultaat_finaal_20160126_-_2012.csv",sep=";",stringsAsFactors = F,dec=",")
df<-rbind(df,read.csv(file="data/eandis/p0269_resultaat_finaal_20160126_-_2013.csv",sep=";",stringsAsFactors = F,dec=","))
df<-rbind(df,read.csv(file="data/eandis/p0269_resultaat_finaal_20160126_-_2014.csv",sep=";",stringsAsFactors = F,dec=","))

df$Benaderend.Verbruik<-gsub(".","",dfCopy$Benaderend.Verbruik,fixed=T)
df$Benaderend.Verbruik<-gsub(",","",dfCopy$Benaderend.Verbruik,fixed=T)
df$Benaderend.Verbruik<-as.numeric(dfCopy$Benaderend.Verbruik)


#Elia data
df<-read.csv("app/data/e_martkaandeel_maandelijks_netbeheerders_h_plus_nh.csv",skip = 4,sep=",",quote="",header=FALSE)
df$V1<-NULL
df$V3<-NULL
df$Share<-paste(as.numeric(gsub("[^0-9]", "", df$V5)),".",as.numeric(gsub("[^0-9]", "", df$V6)),sep="")
df$V5<-NULL
df$V6<-NULL
df$Share<-as.numeric(df$Share)
df$Share[which(is.na(df$Share))]<-0.00
df$V4<-as.character(df$V4)
df$NewShare<-0
for(i in 1:length(unique(df$V4))){
  mysum<-sum(df$Share[df$V4==unique(df$V4)[i]])
  for(j in 1:length(unique(df$V2))){
    df$NewShare[df$V2==unique(df$V2)[j] & df$V4==unique(df$V4)[i]]<-df$Share[df$V2==unique(df$V2)[j] & df$V4==unique(df$V4)[i]]*100/mysum
  }
}
ggplot(na.omit(df),aes(x=as.factor(df$V4),y=NewShare,fill=as.factor(df$V2)))+geom_bar(stat="identity")+theme_bw()
colnames(df)[1:2]<-c("Company","Time")
df_e_elia<-df
df_e_elia$Company<-as.character(df_e_elia$Company)
save(df_e_elia,file="app/data/elia_e.RData")

df<-read.csv("app/data/g_martkaandeel_maandelijks_netbeheerders_h_plus_nh.csv",skip = 4,sep=",",quote="",header=FALSE)
df$V1<-NULL
df$V3<-NULL
df$Share<-paste(as.numeric(gsub("[^0-9]", "", df$V5)),".",as.numeric(gsub("[^0-9]", "", df$V6)),sep="")
df$V5<-NULL
df$V6<-NULL
df$Share<-as.numeric(df$Share)
df$Share[which(is.na(df$Share))]<-0.00
df$V4<-as.character(df$V4)
df$NewShare<-0
for(i in 1:length(unique(df$V4))){
  mysum<-sum(df$Share[df$V4==unique(df$V4)[i]])
  for(j in 1:length(unique(df$V2))){
    df$NewShare[df$V2==unique(df$V2)[j] & df$V4==unique(df$V4)[i]]<-df$Share[df$V2==unique(df$V2)[j] & df$V4==unique(df$V4)[i]]*100/mysum
  }
}
ggplot(na.omit(df),aes(x=as.factor(df$V4),y=NewShare,fill=as.factor(df$V2)))+geom_bar(stat="identity")+theme_bw()
colnames(df)[1:2]<-c("Company","Time")
df_g_elia<-df
df_g_elia$Company<-as.character(df_g_elia$Company)
save(df_g_elia,file="app/data/elia_g.RData")


#GEOCODING
coord<-gGeoCode(unique(df$Hoofdgemeente))
coord[coord$Hoofdgemeente=="DESTELBERGEN",1:2]<-c(51.060934,3.797765)

mdf<-merge(df,coord,by="Hoofdgemeente")
mdf$Sector<-toupper(mdf$Sector)
save(mdf,file="app/data/mydata.RData")

#get Geocodes
library(RCurl)
library(RJSONIO)

construct.geocode.url <- function(address, return.call = "json", sensor = "false") {
  root <- "http://maps.google.com/maps/api/geocode/"
  u <- paste(root, return.call, "?address=", address, "&sensor=", sensor, sep = "")
  return(URLencode(u))
}

gGeoCode <- function(address,verbose=FALSE) {
  require("plyr")
  if(verbose) cat(address,"\n")
  u <- sapply(address,function(x)construct.geocode.url(x))
  doc <- sapply(u,function(x)getURL(x))
  json <- sapply(doc,function(x)fromJSON(x,simplify = FALSE))
  coord = lapply(json,function(x) {
    if(x$status=="OK") {
      lat <- x$results[[1]]$geometry$location$lat
      lng <- x$results[[1]]$geometry$location$lng
      return(c(lat, lng))
    } else {
      return(c(NA,NA))
    }
  })
  coord<-as.data.frame(coord)
  coord<-t(coord)
  coord<-as.data.frame(coord)
  coord$Hoofdgemeente<-rownames(coord)
  
  if(length(address)>1) colnames(coord)[1:2]=c("lat","lng")
  else names(coord)=c("lat","lng")
  return(coord)
}
