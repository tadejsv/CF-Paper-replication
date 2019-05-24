rm(list=ls())
library(quantmod);library(readr);library(censusapi);library(reshape);library(dplyr);rename<-dplyr::rename;library(haven)
census<- read_csv("//bbking2/mbarrera/winprofile/desktop/Replication/bds_f_sic_release.csv")
census$a1_logN_bds = log(census$estabs)
aux<-aggregate(cbind(estabs,estabs_entry,estabs_exit)~year2,census,sum)
names(aux)<-c("year2","a_estabs","a_estabs_entry","a_estabs_exit")
census<-census%>%merge(aux,all=T)
census$a_entry_bds<-census$a_estabs_entry/census$a_estabs
census$a_exit_bds <-census$a_estabs_exit/census$a_estabs
census$a_logN_bds = log(census$a_estabs)
census<-census%>%rename(year=year2)
census<-census[, grepl("a_|year",names(census))]
census<-unique(census)
write.table(census, file = "\\\\bbking2/mbarrera/winprofile/desktop/Replication/Aggregate/census/entry_out.csv")
write_dta(census, "\\\\bbking2/mbarrera/winprofile/desktop/Replication/Aggregate/census/entry_out.dta")

#########2002-2012 Non Manufacturing#############################################

rm(list=ls())
library(quantmod);library(readr);library(censusapi);library(reshape);library(dplyr);rename<-dplyr::rename


years<-c(2002,2007,2012)
codes<-c(42,44,48,51,52,53,54,56,61,62,71,72,81)
for (y in years){
  for (c in codes){
    file<-paste0('//bbking2/mbarrera/winprofile/desktop/Replication/Aggregate/Census_raw/Census_conc_',y,'/ECN_',y,'_US_',c,'SSSZ6_with_ann.csv')
    d<-read_csv(file)[-1,]
    if (c%in%c(54,61,62,71,81)){d<-d[d$OPTAX.id=='T',]}
    else if (c==42&y==2002){d<-d[d$OPTAX.id=='00',]}
    else if (c==48&y==2007){d$NAICS.id<- gsub('\\(101\\)','',d$NAICS.id)}
    else if (c==48&y==2002){d$NAICS.id<- gsub('\\(001\\)','',d$NAICS.id)}
    if (y!=2002){d<-d%>%rename(VALPCT=VAL_PCT)}
    d<-d%>%rename(year=YEAR.id,concen=CONCENFI.id,pct_sales=VALPCT,naicsid=NAICS.id,rcptot=RCPTOT)
    d<-d%>%select(naicsid,concen,year,rcptot,pct_sales)
    d<-d%>%mutate(naicsid=as.integer(as.character(naicsid)),year=as.numeric(year),rcptot=as.numeric(rcptot),pct_sales=as.numeric(pct_sales))
    d$concen<-d$concen%>%recode('804'='4','808'='8','820'='20','850'='50','001'='All')
    d<-d%>%filter(concen!='All')
    if (c==42&y==2002){nonmfg_post97<-d}
    else{nonmfg_post97<-nonmfg_post97%>%rbind(d)}
  }
}

nonmfg_post97<-nonmfg_post97%>%arrange(year, naicsid)
write.table(nonmfg_post97, file = "\\\\bbking2/mbarrera/winprofile/desktop/Replication/Aggregate/census/nonmfg_post97.csv",row.names=F)

#### 1997 Non-manufacturing

for (c in codes){
    file<-paste0('//bbking2/mbarrera/winprofile/desktop/Replication/Aggregate/Census_raw/Census_conc_1997/NonMfg_data/E97',c,'S6.dat')
    d<-read.delim(file, header = TRUE, sep="|")
    d$naics<-d$naicsid
    if (c%in%c(54,61,62,71,81)){d<-d[d$OPTAX.id=='T',]}
    if (c==42){d<-d[d$OPTYPE==0,]}
    d<-d%>%rename(year=YEAR,concen=CONCENFI,pct_sales=VALPCT,naicsid=NAICS,rcptot=ECVALUE)
    d<-d%>%select(naicsid,concen,year,rcptot,pct_sales)
    d<-d%>%mutate(naicsid=as.integer(as.character(naicsid)),year=as.numeric(year),rcptot=as.numeric(rcptot),pct_sales=as.numeric(pct_sales))
    d$concen<-d$concen%>%recode('804'='4','808'='8','820'='20','850'='50','3'='All')
    d<-d%>%filter(concen!='All')
    if (c==42){nonmfg_97<-d}
    else{nonmfg_97<-nonmfg_97%>%rbind(d)}
}
nonmfg_97<-nonmfg_97%>%arrange(naicsid)
write.table(nonmfg_97, file = "\\\\bbking2/mbarrera/winprofile/desktop/Replication/Aggregate/census/nonmfg_97.csv",row.names=F)
#### post 1997 manufacturing
years<-c(2002,2007,2012)
for (y in years){
    file<-paste0('//bbking2/mbarrera/winprofile/desktop/Replication/Aggregate/Census_raw/Census_conc_',y,'/ECN_',y,'_US_31SR12_with_ann.csv')
    d<-read_csv(file)[-1,]
    d[d=='X']<-NA
    d<-d%>%rename(year=YEAR.id,concen=CONCENFI.id,pct_sales=CCORCPPCT,naicsid=NAICS.id,rcptot=RCPTOT,hhi=VSHERFI)
    d<-d%>%select(naicsid,concen,year,rcptot,pct_sales,hhi)
    d<-d%>%mutate(naicsid=as.integer(as.character(naicsid)),year=as.numeric(year),rcptot=as.numeric(rcptot),pct_sales=as.numeric(pct_sales))
    d$concen<-d$concen%>%recode('856'='4','857'='8','858'='20','859'='50','004'='All')
    aux<-d%>%group_by(naicsid)%>%summarize(hhi=max(as.numeric(hhi),na.rm=T),rcptot=max(rcptot,na.rm=T))
    d$hhi<-NULL; d$rcptot<-NULL
    d<-d%>%merge(aux,all.x=T)
    d<-d%>%filter(concen!='All')
    if (y==2002){mfg_post97<-d}
    else{mfg_post97<-mfg_post97%>%rbind(d)}
    mfg_post97$rcptot[mfg_post97=='X']<-NA
}
mfg_post97<-mfg_post97%>%arrange(naicsid,year)
write.table(mfg_post97, file = "\\\\bbking2/mbarrera/winprofile/desktop/Replication/Aggregate/census/mfg_post97.csv",row.names=F)

#### 1997 manufacturing
file<-paste0('//bbking2/mbarrera/winprofile/desktop/Replication/Aggregate/Census_raw/Census_conc_1997/Mfg/E9731R2_data.txt')
d<-read.delim(file, header = TRUE, sep="|")
d<-d%>%rename(year=YEAR,naicsid=NAICS,rcptot=ECVALUE,hhi=VSHERFI)
d<-d%>%select(naicsid,year,rcptot,VSTOP4,VSTOP8,VSTOP20,VSTOP50,hhi)
d<-d%>%melt(id=c("naicsid","year",'rcptot','hhi'))
d<-d%>%rename(pct_sales=value,concen=variable)
d$naicsid<-as.integer(as.character(d$naicsid))
d$concen<-d$concen%>%recode('VSTOP4'='4','VSTOP8'='8','VSTOP20'='20','VSTOP50'='50')
d<-d%>%select(naicsid,concen,year,rcptot,pct_sales,hhi)
d<-d%>%mutate(naicsid=as.integer(as.character(naicsid)),year=as.numeric(year),rcptot=as.numeric(rcptot),pct_sales=as.numeric(pct_sales))
mfg_97<-d
mfg_97<-mfg_97%>%arrange(naicsid)
write.table(mfg_97, file = "\\\\bbking2/mbarrera/winprofile/desktop/Replication/Aggregate/census/mfg_97.csv",row.names=F)

###################################
rm(list=ls())
library(quantmod);library(readr);library(censusapi);library(reshape);library(dplyr);rename<-dplyr::rename
mfg_97          <-read.table("\\\\bbking2/mbarrera/winprofile/desktop/Replication/Aggregate/census/mfg_97.csv",header=T)
mfg_post97      <-read.table("\\\\bbking2/mbarrera/winprofile/desktop/Replication/Aggregate/census/mfg_post97.csv",header=T)
nonmfg_97       <-read.table("\\\\bbking2/mbarrera/winprofile/desktop/Replication/Aggregate/census/nonmfg_97.csv",header=T)
nonmfg_post97   <-read.table("\\\\bbking2/mbarrera/winprofile/desktop/Replication/Aggregate/census/nonmfg_post97.csv",header=T)


nonmfg_post97$hhi<-NA
nonmfg_97$hhi<-NA
CenCon_naics<-nonmfg_post97
CenCon_naics<-CenCon_naics%>%rbind(nonmfg_97)
CenCon_naics<-CenCon_naics%>%rbind(mfg_post97)
CenCon_naics<-CenCon_naics%>%rbind(mfg_97)


naics<-unique(CenCon_naics$naicsid)
years<-unique(CenCon_naics$year)
aux<-data.frame(naicsid=rep(naics,each=4),year=rep(years,times=length(naics)))
aux<-aux%>%merge(CenCon_naics%>%group_by(naicsid,year)%>%summarize(hhi=mean(hhi),sales=max(rcptot)),all.x=T)
aux<-aux%>%merge(CenCon_naics%>%filter(concen==4)%>%select(naicsid,year,al_cenconc4=pct_sales),all.x=T)
aux<-aux%>%merge(CenCon_naics%>%filter(concen==8)%>%select(naicsid,year,al_cenconc8=pct_sales),all.x=T)
aux<-aux%>%merge(CenCon_naics%>%filter(concen==20)%>%select(naicsid,year,al_cenconc20=pct_sales),all.x=T)
aux<-aux%>%merge(CenCon_naics%>%filter(concen==50)%>%select(naicsid,year,al_cenconc50=pct_sales),all.x=T)
CenCon_naics<-aux
write.table(CenCon_naics, file = "\\\\bbking2/mbarrera/winprofile/desktop/Replication/Aggregate/census/CenCon_naics.csv")

############ CENCUSCONC MAP #####################
rm(list=ls())
rename<-dplyr::rename
d<-read.table(file = "\\\\bbking2/mbarrera/winprofile/desktop/Replication/Aggregate/census/CenCon_naics.csv")

d$al_cenconc4<-as.numeric(as.character(d$al_cenconc4))
d$al_cenconc8<-as.numeric(as.character(d$al_cenconc8))
d$al_cenconc20<-as.numeric(as.character(d$al_cenconc20))
d$al_cenconc50<-as.numeric(as.character(d$al_cenconc50))
d$hhi<-as.numeric(as.character(d$hhi))


d<-d%>%rename(naics=naicsid)
d$naics[d$naics=='44-45']<-440
d$naics[d$naics=='42']<-420
d$naics[d$naics=='810']<-81
d$length<-sapply(as.character(d$naics),nchar)
d<-d%>%filter(length==3)
d$naics<-as.numeric(as.character(d$naics))
d<-d%>%filter(!(naics%in%c(513:519))&year<2007)
d<-d%>%filter(!(naics%in%c(441:459,421:429,811:814)))


library(readxl)
NAICS2BEA <- read_excel("//bbking2/mbarrera/winprofile/desktop/Replication/Aggregate/census/NAICS2BEA.xlsx")
d<-d%>%merge(NAICS2BEA,all=T)
library(haven)
levelkey <- read_dta("//bbking2/mbarrera/winprofile/desktop/Replication/Aggregate/census/levelkey.dta")
d<-d%>%merge(levelkey,all=T)
d$indcode=d$ind_short



aux<-d%>%group_by(indcode, year)%>%summarize(i=weighted.mean(al_cenconc4,w=sales,na.rm=T))
d<-d%>%merge(aux,all=T)
d<-d%>%select(-al_cenconc4)
d<-d%>%rename(al_cenconc4=i)

aux<-d%>%group_by(indcode, year)%>%summarize(i=weighted.mean(al_cenconc8,w=sales,na.rm=T))
d<-d%>%merge(aux,all=T)
d<-d%>%select(-al_cenconc8)
d<-d%>%rename(al_cenconc8=i)

aux<-d%>%group_by(indcode, year)%>%summarize(i=weighted.mean(al_cenconc20,w=sales,na.rm=T))
d<-d%>%merge(aux,all=T)
d<-d%>%select(-al_cenconc20)
d<-d%>%rename(al_cenconc20=i)

aux<-d%>%group_by(indcode, year)%>%summarize(i=weighted.mean(al_cenconc50,w=sales,na.rm=T))
d<-d%>%merge(aux,all=T)
d<-d%>%select(-al_cenconc50)
d<-d%>%rename(al_cenconc50=i)

aux<-d%>%group_by(indcode, year)%>%summarize(i=weighted.mean(hhi,w=sales,na.rm=T))
d<-d%>%merge(aux,all=T)
d<-d%>%select(-hhi)

d<-d%>%rename(al_cenhhi=i)

d<-d[,grepl("al_|year|indcode",names(d))]
d<-d%>%arrange(indcode, year)

write.table(d, file = "\\\\bbking2/mbarrera/winprofile/desktop/Replication/Aggregate/census/cencon_out.csv")
write.dta(d, file = "\\\\bbking2/mbarrera/winprofile/desktop/Replication/Aggregate/census/cencon_out.dta")







# Add key to .Renviron
#Sys.setenv(CENSUS_KEY='c99a5a4e8e98a544dce46967064aa5f5f24dde28')
# Reload .Renviron
#readRenviron("~/.Renviron")
# Check to see that the expected key is output in your R console
#Sys.getenv("CENSUS_KEY")
#apis <- listCensusApis()
#View(apis)

#listCensusMetadata(name = "ewks", vintage=2007,type = "variables")
#listCensusMetadata(name = "ewks", vintage=2007,type = "geography")
#getCensus(name='ewks',vars='EMP',region='us',vintage=2007)
#getCensus(name='ewks',vars='EMP',region='us:*',vintage=2007)
