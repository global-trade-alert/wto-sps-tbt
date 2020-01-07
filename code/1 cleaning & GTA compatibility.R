library(stringr)
library(data.table)
library(splitstackshape)
library(gtalibrary)
rm(list = ls())


# loading the data.
gta_setwd()

project.path="8 data dumps/WTO SPS & TBT/"

load(paste0(project.path,"data/SPS search result table - interims - 2.Rdata")) ## the table data frame is split into addenda and notificaitons. No need to look at the table data frame. I just kept it.
load(paste0(project.path,"data/TBT search result table - interims - 2.Rdata")) ## the table data frame is split into notifications, addenda and revisions
load(paste0(project.path,"data/STC search result table - interims - 2.Rdata")) ## only one table here.


### (1) Try to combine the different data frames into one large data frame that you can work with.
# - intervention.id
notifications.sps$intervention.id=paste("sps.",1:nrow(notifications.sps), sep="")
notifications.tbt$intervention.id=paste("tbt.",1:nrow(notifications.tbt), sep="")
notifications.stc$intervention.id=paste("stc.",1:nrow(notifications.stc), sep="")

# - implementing.jurisdiction
setnames(notifications.sps, "Members", "implementing.jurisdiction")
setnames(notifications.tbt, "Notifying Member", "implementing.jurisdiction")
setnames(notifications.stc, "Member(s) subject to STC", "implementing.jurisdiction")

notifications.sps$implementing.jurisdiction=as.character(notifications.sps$implementing.jurisdiction)
notifications.tbt$implementing.jurisdiction=as.character(notifications.tbt$implementing.jurisdiction)
notifications.stc$implementing.jurisdiction=as.character(notifications.stc$implementing.jurisdiction)

# affected.jurisdiction
setnames(notifications.sps, "affected.trading.partners", "affected.jurisdiction")
notifications.tbt$affected.jurisdiction="All"
notifications.stc$affected.jurisdiction="All"

# - title

notifications.sps$title[notifications.sps$title.language!="English"]=paste(notifications.sps$objective[notifications.sps$title.language!="English"], ": ", gsub(";$","",notifications.sps$`Notification Keywords`[notifications.sps$title.language!="English"]), " (",notifications.sps$`Document type`[notifications.sps$title.language!="English"],")", sep="")
notifications.sps$title[notifications.sps$affected.trading.partners!="All" & notifications.sps$title.language!="English"]=paste(notifications.sps$objective[notifications.sps$affected.trading.partners!="All" & notifications.sps$title.language!="English"], " concerning imports from ",
                                                                                  notifications.sps$affected.trading.partners[notifications.sps$affected.trading.partners!="All" & notifications.sps$title.language!="English"],": ",
                                                                                  gsub(";$","",notifications.sps$`Notification Keywords`[notifications.sps$affected.trading.partners!="All" & notifications.sps$title.language!="English"]), " (",
                                                                                  notifications.sps$`Document type`[notifications.sps$affected.trading.partners!="All" & notifications.sps$title.language!="English"],")", sep="")

notifications.tbt=subset(notifications.tbt, is.na(objective)==F & is.na(Type)==F)
notifications.tbt$title[notifications.tbt$title.language!="English"]=paste(gsub(";$","",notifications.tbt$objective[notifications.tbt$title.language!="English"]), " (",notifications.tbt$Type[notifications.tbt$title.language!="English"],")", sep="")
setnames(notifications.stc, "Title", "title")



### DATES
# - date.announced
setnames(notifications.sps, "Date of distribution", "date.announced")
setnames(notifications.tbt, "Date of distribution", "date.announced")
setnames(notifications.stc, "First time raised", "date.announced")

notifications.sps$date.announced=as.Date(notifications.sps$date.announced, "%d/%m/%Y")
notifications.tbt$date.announced=as.Date(notifications.tbt$date.announced, "%d/%m/%Y")
notifications.stc$date.announced=as.Date(notifications.stc$date.announced, "%d/%m/%Y")

# - date.implemented

months="January|February|March|April|May|June|July|August|September|October|November|December"


## STC
notifications.stc$date.implemented=notifications.stc$date.announced


## TBT
### adoption date
notifications.tbt$date.adoption=as.Date(notifications.tbt$date.adoption, "%d/%m/%Y")


#### no date, but month mentioned
tbt.no.adoption.months=subset(notifications.tbt, is.na(date.adoption) & grepl(months, date.adoption.text, ignore.case=T))
tbt.no.adoption.no.months=subset(notifications.tbt, is.na(date.adoption) & grepl(months, date.adoption.text, ignore.case=T)==F)


#### cleaning formatting
tbt.no.adoption.months$date.adoption.text=gsub(paste0("(",months,") (\\d+),? (\\d{4})"),"\\2 \\1 \\3",tbt.no.adoption.months$date.adoption.text)
tbt.no.adoption.months$date.adoption.text=gsub(paste0("(\\d+) (",months,"),? (\\d{4})"),"\\1 \\2 \\3",tbt.no.adoption.months$date.adoption.text)
tbt.no.adoption.months$date.adoption.text=gsub(paste0("(",months,"),? (\\d{4})"),"\\1 \\2",tbt.no.adoption.months$date.adoption.text)


tbt.no.adoption.months$date.adoption.text=gsub(" ?January ","/01/", tbt.no.adoption.months$date.adoption.text)
tbt.no.adoption.months$date.adoption.text=gsub(" ?February ","/02/", tbt.no.adoption.months$date.adoption.text)
tbt.no.adoption.months$date.adoption.text=gsub(" ?March ","/03/", tbt.no.adoption.months$date.adoption.text)
tbt.no.adoption.months$date.adoption.text=gsub(" ?April ","/04/", tbt.no.adoption.months$date.adoption.text)
tbt.no.adoption.months$date.adoption.text=gsub(" ?May ","/05/", tbt.no.adoption.months$date.adoption.text)
tbt.no.adoption.months$date.adoption.text=gsub(" ?June ","/06/", tbt.no.adoption.months$date.adoption.text)
tbt.no.adoption.months$date.adoption.text=gsub(" ?July ","/07/", tbt.no.adoption.months$date.adoption.text)
tbt.no.adoption.months$date.adoption.text=gsub(" ?August ","/08/", tbt.no.adoption.months$date.adoption.text)
tbt.no.adoption.months$date.adoption.text=gsub(" ?September ","/09/", tbt.no.adoption.months$date.adoption.text)
tbt.no.adoption.months$date.adoption.text=gsub(" ?October ","/10/", tbt.no.adoption.months$date.adoption.text)
tbt.no.adoption.months$date.adoption.text=gsub(" ?November ","/11/", tbt.no.adoption.months$date.adoption.text)
tbt.no.adoption.months$date.adoption.text=gsub(" ?December ","/12/", tbt.no.adoption.months$date.adoption.text)

tbt.no.adoption.months$date.adoption.text=gsub("^/","01/", tbt.no.adoption.months$date.adoption.text)
tbt.no.adoption.months$date.adoption=as.Date(tbt.no.adoption.months$date.adoption.text, "%d/%m/%Y")


tbt.no.adoption.months$date.adoption.text[grepl("\\d+/\\d{1,2}/\\d{4}",tbt.no.adoption.months$date.adoption.text)==F]=gsub("(\\d{2}/\\d{4})","01/\\1", tbt.no.adoption.months$date.adoption.text[grepl("\\d+/\\d{1,2}/\\d{4}",tbt.no.adoption.months$date.adoption.text)==F])
tbt.no.adoption.months$date.adoption=as.Date(str_extract(tbt.no.adoption.months$date.adoption.text, "\\d{1,2}/\\d{1,2}/\\d{4}"), "%d/%m/%Y")
notifications.tbt=rbind(subset(notifications.tbt, is.na(date.adoption)==F),
                        tbt.no.adoption.months)

#### no date, no month mentioned

# adoption.text=as.data.frame(table(tbt.no.adoption.no.months$date.adoption.text))
# adoption.text$relative=as.numeric(grepl("notification|circulation", adoption.text$Var1, ignore.case = T))

tbt.no.adoption.no.months$add.days=NA
tbt.no.adoption.no.months$add.days[grepl("notification|circulation", tbt.no.adoption.no.months$date.adoption.text, ignore.case = T)]=as.numeric(str_extract(tbt.no.adoption.no.months$date.adoption.text[grepl("notification|circulation", tbt.no.adoption.no.months$date.adoption.text, ignore.case = T)],"\\d+"))
tbt.no.adoption.no.months$add.days[grepl(" day", tbt.no.adoption.no.months$date.adoption.text, ignore.case = T)]=1*tbt.no.adoption.no.months$add.days[grepl(" day", tbt.no.adoption.no.months$date.adoption.text, ignore.case = T)]
tbt.no.adoption.no.months$add.days[grepl(" week", tbt.no.adoption.no.months$date.adoption.text, ignore.case = T)]=7*tbt.no.adoption.no.months$add.days[grepl(" week", tbt.no.adoption.no.months$date.adoption.text, ignore.case = T)]
tbt.no.adoption.no.months$add.days[grepl(" month", tbt.no.adoption.no.months$date.adoption.text, ignore.case = T)]=30*tbt.no.adoption.no.months$add.days[grepl(" month", tbt.no.adoption.no.months$date.adoption.text, ignore.case = T)]
tbt.no.adoption.no.months$add.days[grepl(" quarter", tbt.no.adoption.no.months$date.adoption.text, ignore.case = T)]=90*tbt.no.adoption.no.months$add.days[grepl(" quarter", tbt.no.adoption.no.months$date.adoption.text, ignore.case = T)]
tbt.no.adoption.no.months$add.days[grepl(" year", tbt.no.adoption.no.months$date.adoption.text, ignore.case = T)]=365*tbt.no.adoption.no.months$add.days[grepl(" year", tbt.no.adoption.no.months$date.adoption.text, ignore.case = T)]

## corrections
tbt.no.adoption.no.months$add.days[grepl("news|gazette", tbt.no.adoption.no.months$date.adoption.text, ignore.case = T)]=NA

tbt.no.adoption.no.months$date.adoption[is.na(tbt.no.adoption.no.months$add.days)==F]=tbt.no.adoption.no.months$date.announced[is.na(tbt.no.adoption.no.months$add.days)==F]+tbt.no.adoption.no.months$add.days[is.na(tbt.no.adoption.no.months$add.days)==F]
tbt.no.adoption.no.months$add.days=NULL

notifications.tbt=rbind(notifications.tbt,
                        tbt.no.adoption.no.months)

rm(tbt.no.adoption.no.months, tbt.no.adoption.months)

## implementation date

notifications.tbt$date.inforce=as.Date(notifications.tbt$date.inforce, "%d/%m/%Y")


#### no date, but month mentioned
tbt.no.force.months=subset(notifications.tbt, is.na(date.inforce) & grepl(months, date.inforce.text, ignore.case=T))
tbt.no.force.no.months=subset(notifications.tbt, is.na(date.inforce) & grepl(months, date.inforce.text, ignore.case=T)==F)

#### cleaning formatting
tbt.no.force.months$date.inforce.text=gsub(paste0("(",months,") (\\d+),? (\\d{4})"),"\\2 \\1 \\3",tbt.no.force.months$date.inforce.text)
tbt.no.force.months$date.inforce.text=gsub(paste0("(\\d+) (",months,"),? (\\d{4})"),"\\1 \\2 \\3",tbt.no.force.months$date.inforce.text)
tbt.no.force.months$date.inforce.text=gsub(paste0("(",months,"),? (\\d{4})"),"\\1 \\2",tbt.no.force.months$date.inforce.text)


tbt.no.force.months$date.inforce.text=gsub(" ?January ","/01/", tbt.no.force.months$date.inforce.text)
tbt.no.force.months$date.inforce.text=gsub(" ?February ","/02/", tbt.no.force.months$date.inforce.text)
tbt.no.force.months$date.inforce.text=gsub(" ?March ","/03/", tbt.no.force.months$date.inforce.text)
tbt.no.force.months$date.inforce.text=gsub(" ?April ","/04/", tbt.no.force.months$date.inforce.text)
tbt.no.force.months$date.inforce.text=gsub(" ?May ","/05/", tbt.no.force.months$date.inforce.text)
tbt.no.force.months$date.inforce.text=gsub(" ?June ","/06/", tbt.no.force.months$date.inforce.text)
tbt.no.force.months$date.inforce.text=gsub(" ?July ","/07/", tbt.no.force.months$date.inforce.text)
tbt.no.force.months$date.inforce.text=gsub(" ?August ","/08/", tbt.no.force.months$date.inforce.text)
tbt.no.force.months$date.inforce.text=gsub(" ?September ","/09/", tbt.no.force.months$date.inforce.text)
tbt.no.force.months$date.inforce.text=gsub(" ?October ","/10/", tbt.no.force.months$date.inforce.text)
tbt.no.force.months$date.inforce.text=gsub(" ?November ","/11/", tbt.no.force.months$date.inforce.text)
tbt.no.force.months$date.inforce.text=gsub(" ?December ","/12/", tbt.no.force.months$date.inforce.text)

tbt.no.force.months$date.inforce.text[grepl("\\d+/\\d{1,2}/\\d{4}",tbt.no.force.months$date.inforce.text)==F]=gsub("(\\d{2}/\\d{4})","01/\\1", tbt.no.force.months$date.inforce.text[grepl("\\d+/\\d{1,2}/\\d{4}",tbt.no.force.months$date.inforce.text)==F])
tbt.no.force.months$date.inforce=as.Date(str_extract(tbt.no.force.months$date.inforce.text, "\\d{1,2}/\\d{1,2}/\\d{4}"), "%d/%m/%Y")
notifications.tbt=rbind(subset(notifications.tbt, is.na(date.inforce)==F),
                        tbt.no.force.months)

#### no date, no month mentioned
# 
# force.text=as.data.frame(table(tbt.no.force.no.months$date.inforce.text))
# force.text$relative=as.numeric(grepl("notification|circulation|adoption", force.text$Var1, ignore.case = T))

tbt.no.force.no.months$add.days=NA
tbt.no.force.no.months$add.days[grepl("notification|circulation", tbt.no.force.no.months$date.inforce.text, ignore.case = T)]=as.numeric(str_extract(tbt.no.force.no.months$date.inforce.text[grepl("notification|circulation", tbt.no.force.no.months$date.inforce.text, ignore.case = T)],"\\d+"))
tbt.no.force.no.months$add.days[grepl(" day", tbt.no.force.no.months$date.inforce.text, ignore.case = T)]=1*tbt.no.force.no.months$add.days[grepl(" day", tbt.no.force.no.months$date.inforce.text, ignore.case = T)]
tbt.no.force.no.months$add.days[grepl(" week", tbt.no.force.no.months$date.inforce.text, ignore.case = T)]=7*tbt.no.force.no.months$add.days[grepl(" week", tbt.no.force.no.months$date.inforce.text, ignore.case = T)]
tbt.no.force.no.months$add.days[grepl(" month", tbt.no.force.no.months$date.inforce.text, ignore.case = T)]=30*tbt.no.force.no.months$add.days[grepl(" month", tbt.no.force.no.months$date.inforce.text, ignore.case = T)]
tbt.no.force.no.months$add.days[grepl(" quarter", tbt.no.force.no.months$date.inforce.text, ignore.case = T)]=90*tbt.no.force.no.months$add.days[grepl(" quarter", tbt.no.force.no.months$date.inforce.text, ignore.case = T)]
tbt.no.force.no.months$add.days[grepl(" year", tbt.no.force.no.months$date.inforce.text, ignore.case = T)]=365*tbt.no.force.no.months$add.days[grepl(" year", tbt.no.force.no.months$date.inforce.text, ignore.case = T)]

## corrections
tbt.no.force.no.months$add.days[grepl("news|gazette", tbt.no.force.no.months$date.inforce.text, ignore.case = T)]=NA

tbt.no.force.no.months$date.inforce[is.na(tbt.no.force.no.months$add.days)==F]=tbt.no.force.no.months$date.announced[is.na(tbt.no.force.no.months$add.days)==F]+tbt.no.force.no.months$add.days[is.na(tbt.no.force.no.months$add.days)==F]
tbt.no.force.no.months$date.inforce[is.na(tbt.no.force.no.months$add.days)==F & grepl("adoption",tbt.no.force.no.months$date.inforce.text, ignore.case = T)]=tbt.no.force.no.months$date.adoption[is.na(tbt.no.force.no.months$add.days)==F  & grepl("adoption",tbt.no.force.no.months$date.inforce.text, ignore.case = T)]+tbt.no.force.no.months$add.days[is.na(tbt.no.force.no.months$add.days)==F  & grepl("adoption",tbt.no.force.no.months$date.inforce.text, ignore.case = T)]
tbt.no.force.no.months$add.days=NULL

notifications.tbt=rbind(notifications.tbt,
                        tbt.no.force.no.months)

rm(tbt.no.force.no.months, tbt.no.force.months)

## assumptions for adopted cases w/o implementation date.
notifications.tbt$date.inforce[is.na(notifications.tbt$date.inforce)==T & is.na(notifications.tbt$date.adoption)==F]=notifications.tbt$date.adoption[is.na(notifications.tbt$date.inforce)==T & is.na(notifications.tbt$date.adoption)==F]

setnames(notifications.tbt, "date.inforce", "date.implemented")


## SPS
notifications.sps$date.adoption=as.Date(notifications.sps$date.adoption, "%d/%m/%Y")
notifications.sps$date.inforce=as.Date(notifications.sps$date.inforce, "%d/%m/%Y")
notifications.sps$date.publication=as.Date(notifications.sps$date.publication, "%d/%m/%Y")
notifications.sps$date.inforce[is.na(notifications.sps$date.inforce)==T & is.na(notifications.sps$date.publication)==F & notifications.sps$date.inforce.6m==0]=notifications.sps$date.publication[is.na(notifications.sps$date.inforce)==T & is.na(notifications.sps$date.publication)==F & notifications.sps$date.inforce.6m==0]
notifications.sps$date.inforce[is.na(notifications.sps$date.inforce)==T & is.na(notifications.sps$date.publication)==F & notifications.sps$date.inforce.6m==1]=180+notifications.sps$date.publication[is.na(notifications.sps$date.inforce)==T & is.na(notifications.sps$date.publication)==F & notifications.sps$date.inforce.6m==1]
notifications.sps$date.inforce[is.na(notifications.sps$date.inforce)==T & is.na(notifications.sps$date.publication)==F & notifications.sps$date.inforce.6m==1]=notifications.sps$date.adoption
setnames(notifications.sps, "date.inforce", "date.implemented")






### HS & ICS PRODUCTS
notifications.sps$affected.product=apply(notifications.sps, 1, function(x) paste(gsub(" |\\|","",str_extract_all(x[which(colnames(notifications.sps)=="products.hs")], "(^\\d{2,})|\\|\\d{2,}")[[1]]), collapse=","))
notifications.tbt$affected.product=apply(notifications.tbt, 1, function(x) paste(gsub(" |\\|","",str_extract_all(x[which(colnames(notifications.tbt)=="products.hs")], "(^\\d{2,})|\\|\\d{2,}")[[1]]), collapse=","))
notifications.stc$affected.product=apply(notifications.stc, 1, function(x) paste(gsub(" |\\|","",str_extract_all(x[which(colnames(notifications.stc)=="products.hs")], "(^\\d{2,})|\\|\\d{2,}")[[1]]), collapse=","))




### intervention.type ("Technical Barrier to Trade", "TBT - Specific Trade Concern", "Sanitary and Phytosanitary Measure")
notifications.sps$intervention.type="Sanitary and Phytosanitary Measure"
notifications.tbt$intervention.type="Technical Barrier to Trade"
notifications.stc$intervention.type="TBT - Specific Trade Concern"


### url
setnames(notifications.sps, "doc.url", "url")
setnames(notifications.tbt, "doc.url", "url")
setnames(notifications.stc, "doc.url", "url")

### WTO symbol
setnames(notifications.sps, "Document symbol", "Symbol")
setnames(notifications.stc, "IMS ID", "Symbol")

notifications.sps$Symbol=as.character(notifications.sps$Symbol)
notifications.tbt$Symbol=as.character(notifications.tbt$Symbol)
notifications.stc$Symbol=as.character(notifications.stc$Symbol)


### joint cleaning
common.var=c("intervention.id", "Symbol","implementing.jurisdiction","affected.jurisdiction", "title","description", "description.language",  "affected.product", "date.announced", "date.implemented", "intervention.type", "url")
notifications=rbind(notifications.sps[,common.var],
                    notifications.tbt[,common.var],
                    notifications.stc[,common.var])


# be sure to take out measures that target specific countries but do not include Switzerland
notifications=subset(notifications, grepl("[Aa]ll|[Ss]witzerland|[Ss]uisa",affected.jurisdiction)==T)
notifications$affected.jurisdiction=NULL



### date removed or inforced based on addendum
# addenda include date information in two cases
cases=rbind(as.data.frame.table(table(addenda.tbt$reason.for.add)), as.data.frame.table(table(addenda.sps$concern)))
in.force=c("entry into force|enters in to force")
removed=c("Withdrawal|withdrawn")

addenda.sps$original.symbol=gsub(".Add.*", "", addenda.sps$`Document symbol`)
addenda.tbt$original.symbol=gsub(".Add.*", "", addenda.tbt$Symbol)

addenda.sps$`Date of distribution`=as.Date(addenda.sps$`Date of distribution`, "%d/%m/%Y")
addenda.tbt$`Date of distribution`=as.Date(addenda.tbt$`Date of distribution`, "%d/%m/%Y")

# removing withdrawn interventions
notifications=subset(notifications, ! Symbol %in% subset(addenda.sps, grepl(removed, concern))$original.symbol)
notifications=subset(notifications, ! Symbol %in% subset(addenda.tbt, grepl(removed, reason.for.add))$original.symbol)

# updating implementation dates
add.implemented=rbind(unique(subset(addenda.sps, grepl(in.force, concern))[,c("original.symbol", "Date of distribution")]), unique(subset(addenda.tbt, grepl(in.force, reason.for.add))[,c("original.symbol", "Date of distribution")]))
add.implemented=aggregate(`Date of distribution` ~ original.symbol, add.implemented, max)

setnames(add.implemented, "original.symbol","Symbol")
implemented=subset(notifications, Symbol %in% add.implemented$Symbol)
implemented=merge(implemented, add.implemented, by="Symbol", all.x=T)
implemented$date.implemented=implemented$`Date of distribution`
implemented$`Date of distribution`=NULL

notifications=rbind(subset(notifications, ! Symbol %in% add.implemented$Symbol), implemented)
rm(implemented)

setnames(notifications, "Symbol", "state.act.id")

### i.un
## add UN codes for the implementing and affected jurisdictions (treat the EU as if all member states impose this)
conversion=gtalibrary::country.names

notifications$implementing.jurisdiction=gsub("European Union; |; European Union","", notifications$implementing.jurisdiction)
notifications$implementing.jurisdiction=gsub("; ",";", notifications$implementing.jurisdiction)
notifications$implementing.jurisdiction[notifications$implementing.jurisdiction=="European Union"]=paste(subset(conversion, is.eu==1)$name, collapse=";")

notifications=cSplit(notifications, which(colnames(notifications)=="implementing.jurisdiction"), direction="long", sep=";")
countries=as.data.frame.table(table(notifications$implementing.jurisdiction))

setnames(conversion, "name", "implementing.jurisdiction")
setnames(conversion, "un_code", "i.un")
notifications=merge(notifications, unique(conversion[,c("implementing.jurisdiction","i.un")]), by="implementing.jurisdiction", all.x=T)

notifications$i.un[notifications$implementing.jurisdiction=="Bahrain, Kingdom of"]=48
notifications$i.un[notifications$implementing.jurisdiction=="Bolivia, Plurinational State of"]=68
notifications$i.un[notifications$implementing.jurisdiction=="Côte d'Ivoire"]=384
notifications$i.un[notifications$implementing.jurisdiction=="C?te d'Ivoire"]=384
notifications$i.un[notifications$implementing.jurisdiction=="Cabo Verde"]=132
notifications$i.un[notifications$implementing.jurisdiction=="Czech Republic"]=203
notifications$i.un[notifications$implementing.jurisdiction=="Democratic Republic of the Congo"]=180
notifications$i.un[notifications$implementing.jurisdiction=="Eswatini"]=748
notifications$i.un[notifications$implementing.jurisdiction=="Hong Kong, China"]=344
notifications$i.un[notifications$implementing.jurisdiction=="Korea, Republic of"]=410
notifications$i.un[notifications$implementing.jurisdiction=="Kuwait, the State of"]=414
notifications$i.un[notifications$implementing.jurisdiction=="Kyrgyz Republic"]=417
notifications$i.un[notifications$implementing.jurisdiction=="Lao People's Democratic Republic"]=418
notifications$i.un[notifications$implementing.jurisdiction=="Moldova, Republic of"]=498
notifications$i.un[notifications$implementing.jurisdiction=="Russian Federation"]=643
notifications$i.un[notifications$implementing.jurisdiction=="Saint Vincent and the Grenadines"]=670
notifications$i.un[notifications$implementing.jurisdiction=="Saudi Arabia, Kingdom of"]=682
notifications$i.un[notifications$implementing.jurisdiction=="Slovak Republic"]=703
notifications$i.un[notifications$implementing.jurisdiction=="The former Yugoslav Republic of Macedonia"]=807
notifications$i.un[notifications$implementing.jurisdiction=="The Gambia"]=270
notifications$i.un[notifications$implementing.jurisdiction=="Trinidad and Tobago"]=780
notifications$i.un[notifications$implementing.jurisdiction=="Venezuela, Bolivarian Republic of"]=862
notifications$i.un[notifications$implementing.jurisdiction=="Viet Nam"]=704

# should only be Macao, North Macedonia
unique(notifications$implementing.jurisdiction[is.na(notifications$i.un)])
notifications=subset(notifications, is.na(i.un)==F)

notifications$implementing.jurisdiction=NULL

notifications=merge(notifications, unique(conversion[,c("i.un", "implementing.jurisdiction")]), by="i.un", all.x=T)
unique(notifications$i.un[is.na(notifications$implementing.jurisdiction)])




# expanding HS codes
hs.codes=gtalibrary::hs.codes

hs.codes$hs2=substr(hs.codes$hs.code,1,2)
hs.codes$hs2[nchar(hs.codes$hs.code)==5]=substr(hs.codes$hs.code[nchar(hs.codes$hs.code)==5],1,1)

hs.codes$hs4=substr(hs.codes$hs.code,1,4)
hs.codes$hs4[nchar(hs.codes$hs.code)==5]=substr(hs.codes$hs.code[nchar(hs.codes$hs.code)==5],1,3)

hs.codes.2=unique((hs.codes[,c("hs2","hs.code")]))
names(hs.codes.2)=c("affected.product","hs6")


hs.codes.4=unique((hs.codes[,c("hs4","hs.code")]))
names(hs.codes.4)=c("affected.product","hs6")

hs.codes.6=unique((hs.codes[,c("hs.code","hs.code")]))
names(hs.codes.6)=c("affected.product","hs6")

hs.codes=unique(rbind(hs.codes.2, hs.codes.4, hs.codes.6))

hs.codes$affected.product=as.integer(hs.codes$affected.product)

hs.codes=aggregate(hs6 ~ affected.product, hs.codes, function(x) paste(unique(x), collapse=";"))



notifications=subset(notifications, nchar(affected.product)>0)
notifications=cSplit(notifications, which(colnames(notifications)=="affected.product"), direction="long", sep=",")
unique(nchar(notifications$affected.product))

notifications$affected.product[notifications$affected.product>=1061 & notifications$affected.product<=1063]=106

notifications=merge(notifications, hs.codes, by="affected.product", all.x=T)
notifications$hs6[is.na(notifications$hs6) & nchar(notifications$affected.product)>=5]=notifications$affected.product[is.na(notifications$hs6) & nchar(notifications$affected.product)>=5]

## have to convert those someday.
old.hs.vintages=subset(notifications, is.na(hs6))

notifications=subset(notifications, is.na(hs6)==F)
notifications$affected.product=notifications$hs6
notifications$hs6=NULL

notifications=cSplit(notifications, which(colnames(notifications)=="affected.product"), direction="long", sep=";")

save(notifications, file="data/WTO SPS & TBT/WTO SPS & TBT database.Rdata")
date.path=paste("data/WTO SPS & TBT/WTO SPS & TBT database - ",Sys.Date(),".Rdata", sep="")
save(notifications, file=date.path)

