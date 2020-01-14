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
notifications.stc$date.removed=NA
notifications.stc$date.adopted=NA
notifications.stc$date.published=NA



## TBT
notifications.tbt$date.adoption[nchar(notifications.tbt$date.adoption)<5]=NA
notifications.tbt$date.inforce[nchar(notifications.tbt$date.inforce)<5]=NA

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
setnames(notifications.tbt, "date.adoption", "date.adopted")
notifications.tbt$date.removed=NA
notifications.tbt$date.published=NA




## SPS
notifications.sps$date.adoption[nchar(notifications.sps$date.adoption)<5]=NA
notifications.sps$date.publication[nchar(notifications.sps$date.publication)<5]=NA
notifications.sps$date.inforce[nchar(notifications.sps$date.inforce)<5]=NA

notifications.sps$date.adoption.text[nchar(notifications.sps$date.adoption.text)<5]=NA
notifications.sps$date.publication.text[nchar(notifications.sps$date.publication.text)<5]=NA
notifications.sps$date.inforce.text[nchar(notifications.sps$date.inforce.text)<5]=NA

### adoption
notifications.sps$date.adoption=as.Date(notifications.sps$date.adoption, "%d/%m/%Y")

#### no date, but month mentioned
sps.no.adoption.months=subset(notifications.sps, is.na(date.adoption) & grepl(months, date.adoption.text, ignore.case=T))
sps.no.adoption.no.months=subset(notifications.sps, is.na(date.adoption) & grepl(months, date.adoption.text, ignore.case=T)==F)

#### cleaning formatting
sps.no.adoption.months$date.adoption.text=gsub(paste0("(",months,") (\\d+),? (\\d{4})"),"\\2 \\1 \\3",sps.no.adoption.months$date.adoption.text)
sps.no.adoption.months$date.adoption.text=gsub(paste0("(\\d+) (",months,"),? (\\d{4})"),"\\1 \\2 \\3",sps.no.adoption.months$date.adoption.text)
sps.no.adoption.months$date.adoption.text=gsub(paste0("(",months,"),? (\\d{4})"),"\\1 \\2",sps.no.adoption.months$date.adoption.text)


sps.no.adoption.months$date.adoption.text=gsub(" ?J?anuary ","/01/", sps.no.adoption.months$date.adoption.text)
sps.no.adoption.months$date.adoption.text=gsub(" ?F?ebruary ","/02/", sps.no.adoption.months$date.adoption.text)
sps.no.adoption.months$date.adoption.text=gsub(" ?M?arch ","/03/", sps.no.adoption.months$date.adoption.text)
sps.no.adoption.months$date.adoption.text=gsub(" ?A?pril ","/04/", sps.no.adoption.months$date.adoption.text)
sps.no.adoption.months$date.adoption.text=gsub(" ?M?ay ","/05/", sps.no.adoption.months$date.adoption.text)
sps.no.adoption.months$date.adoption.text=gsub(" ?J?une ","/06/", sps.no.adoption.months$date.adoption.text)
sps.no.adoption.months$date.adoption.text=gsub(" ?J?uly ","/07/", sps.no.adoption.months$date.adoption.text)
sps.no.adoption.months$date.adoption.text=gsub(" ?A?ugust ","/08/", sps.no.adoption.months$date.adoption.text)
sps.no.adoption.months$date.adoption.text=gsub(" ?S?eptember ","/09/", sps.no.adoption.months$date.adoption.text)
sps.no.adoption.months$date.adoption.text=gsub(" ?O?ctober ","/10/", sps.no.adoption.months$date.adoption.text)
sps.no.adoption.months$date.adoption.text=gsub(" ?N?ovember ","/11/", sps.no.adoption.months$date.adoption.text)
sps.no.adoption.months$date.adoption.text=gsub(" ?D?ecember ","/12/", sps.no.adoption.months$date.adoption.text)

sps.no.adoption.months$date.adoption.text=gsub("^/","01/", sps.no.adoption.months$date.adoption.text)
sps.no.adoption.months$date.adoption=as.Date(sps.no.adoption.months$date.adoption.text, "%d/%m/%Y")


sps.no.adoption.months$date.adoption.text[grepl("\\d+/\\d{1,2}/\\d{4}",sps.no.adoption.months$date.adoption.text)==F]=gsub("(\\d{2}/\\d{4})","01/\\1", sps.no.adoption.months$date.adoption.text[grepl("\\d+/\\d{1,2}/\\d{4}",sps.no.adoption.months$date.adoption.text)==F])
sps.no.adoption.months$date.adoption=as.Date(str_extract(sps.no.adoption.months$date.adoption.text, "\\d{1,2}/\\d{1,2}/\\d{4}"), "%d/%m/%Y")
notifications.sps=rbind(subset(notifications.sps, is.na(date.adoption)==F),
                        sps.no.adoption.months)



#### no date, no month mentioned

# adoption.text=as.data.frame(table(sps.no.adoption.no.months$date.adoption.text))
# adoption.text$relative=as.numeric(grepl("notification|circulation", adoption.text$Var1, ignore.case = T))

sps.no.adoption.no.months$add.days=NA
sps.no.adoption.no.months$add.days[grepl("notification|circulation", sps.no.adoption.no.months$date.adoption.text, ignore.case = T)]=as.numeric(str_extract(sps.no.adoption.no.months$date.adoption.text[grepl("notification|circulation", sps.no.adoption.no.months$date.adoption.text, ignore.case = T)],"\\d+"))
sps.no.adoption.no.months$add.days[grepl(" day", sps.no.adoption.no.months$date.adoption.text, ignore.case = T)]=1*sps.no.adoption.no.months$add.days[grepl(" day", sps.no.adoption.no.months$date.adoption.text, ignore.case = T)]
sps.no.adoption.no.months$add.days[grepl(" week", sps.no.adoption.no.months$date.adoption.text, ignore.case = T)]=7*sps.no.adoption.no.months$add.days[grepl(" week", sps.no.adoption.no.months$date.adoption.text, ignore.case = T)]
sps.no.adoption.no.months$add.days[grepl(" month", sps.no.adoption.no.months$date.adoption.text, ignore.case = T)]=30*sps.no.adoption.no.months$add.days[grepl(" month", sps.no.adoption.no.months$date.adoption.text, ignore.case = T)]
sps.no.adoption.no.months$add.days[grepl(" quarter", sps.no.adoption.no.months$date.adoption.text, ignore.case = T)]=90*sps.no.adoption.no.months$add.days[grepl(" quarter", sps.no.adoption.no.months$date.adoption.text, ignore.case = T)]
sps.no.adoption.no.months$add.days[grepl(" year", sps.no.adoption.no.months$date.adoption.text, ignore.case = T)]=365*sps.no.adoption.no.months$add.days[grepl(" year", sps.no.adoption.no.months$date.adoption.text, ignore.case = T)]

## corrections
sps.no.adoption.no.months$add.days[grepl("news|gazette", sps.no.adoption.no.months$date.adoption.text, ignore.case = T)]=NA

sps.no.adoption.no.months$date.adoption[is.na(sps.no.adoption.no.months$add.days)==F]=sps.no.adoption.no.months$date.announced[is.na(sps.no.adoption.no.months$add.days)==F]+sps.no.adoption.no.months$add.days[is.na(sps.no.adoption.no.months$add.days)==F]
sps.no.adoption.no.months$add.days=NULL

notifications.sps=rbind(notifications.sps,
                        sps.no.adoption.no.months)

notifications.sps$date.adoption[is.na(notifications.sps$date.adoption) & grepl("Immediately|soon as possible", notifications.sps$date.adoption.text, ignore.case = T)]=notifications.sps$date.announced[is.na(notifications.sps$date.adoption) & grepl("Immediately|soon as possible", notifications.sps$date.adoption.text, ignore.case = T)]

rm(sps.no.adoption.no.months, sps.no.adoption.months)



### publication
notifications.sps$date.publication=as.Date(notifications.sps$date.publication, "%d/%m/%Y")


### in force
notifications.sps$date.inforce=as.Date(notifications.sps$date.inforce, "%d/%m/%Y")

### emergencies
notifications.sps$emergency.start=as.Date(notifications.sps$emergency.start, "%d/%m/%Y")
notifications.sps$emergency.end=as.Date(notifications.sps$emergency.end, "%d/%m/%Y")
notifications.sps$date.inforce[is.na(notifications.sps$emergency.start)==F]=notifications.sps$emergency.start[is.na(notifications.sps$emergency.start)==F]
notifications.sps$date.removed=notifications.sps$emergency.end


### "six months after publication" box ticked with or without publication date specified.
notifications.sps$date.inforce[is.na(notifications.sps$date.inforce) & notifications.sps$date.inforce.6m==1 & is.na(notifications.sps$date.publication)==F]=180+notifications.sps$date.publication[is.na(notifications.sps$date.inforce) & notifications.sps$date.inforce.6m==1 & is.na(notifications.sps$date.publication)==F]
notifications.sps$date.inforce[is.na(notifications.sps$date.inforce) & notifications.sps$date.inforce.6m==1 & is.na(notifications.sps$date.adoption)==F]=180+notifications.sps$date.adoption[is.na(notifications.sps$date.inforce) & notifications.sps$date.inforce.6m==1 & is.na(notifications.sps$date.adoption)==F]

## somwhow the annoucnement date rewrite does not work in the same way, I thus go by identifying the rows.
anc=intersect(intersect(which(is.na(notifications.sps$date.inforce)), which(notifications.sps$date.inforce.6m==1)), which(is.na(notifications.sps$date.announced)==F))
notifications.sps$date.inforce[anc]=180+notifications.sps$date.announced[anc]


# assumptions for missing dates.
notifications.sps$date.inforce[is.na(notifications.sps$date.inforce) & is.na(notifications.sps$date.adoption)==F]=notifications.sps$date.adoption[is.na(notifications.sps$date.inforce) & is.na(notifications.sps$date.adoption)==F]
notifications.sps$date.inforce[is.na(notifications.sps$date.inforce) & is.na(notifications.sps$date.publication)==F]=notifications.sps$date.adoption[is.na(notifications.sps$date.inforce) & is.na(notifications.sps$date.publication)==F]


setnames(notifications.sps, "date.inforce", "date.implemented")
setnames(notifications.sps, "date.adoption", "date.adopted")
setnames(notifications.sps, "date.publication", "date.published")




### WTO symbol
setnames(notifications.sps, "Document symbol", "wto.id")
setnames(notifications.stc, "IMS ID", "wto.id")
setnames(notifications.tbt, "Symbol", "wto.id")

notifications.sps$wto.id=as.character(notifications.sps$wto.id)
notifications.tbt$wto.id=as.character(notifications.tbt$wto.id)
notifications.stc$wto.id=as.character(notifications.stc$wto.id)



### HS PRODUCTS
products=unique(rbind(notifications.sps[,c("wto.id", "products.hs")],
                      notifications.stc[,c("wto.id", "products.hs")],
                      notifications.tbt[,c("wto.id", "products.hs")]))

products=subset(products, grepl("\\d+",products.hs))

products.hs=data.frame()
for(i in 1:nrow(products)){
  
  
  products.hs=rbind(products.hs,
                    data.frame(wto.id=products$wto.id[i],
                                  hs.code=gsub("\\D","",unlist(str_extract_all(gsub("^\\D+","",products$products.hs[i]), "(^\\d{2,})|\\|\\d{2,}"))),
                                  stringsAsFactors = F))
  
  print(i)
  
}

rm(products)

### ICS PRODUCTS
products=unique(rbind(notifications.sps[,c("wto.id", "products.ics")],
                      notifications.stc[,c("wto.id", "products.ics")],
                      notifications.tbt[,c("wto.id", "products.ics")]))

products=subset(products, grepl("\\d+",products.ics))

products.ics=data.frame()
for(i in 1:nrow(products)){
  
  
  products.ics=rbind(products.ics,
                     data.frame(wto.id=products$wto.id[i],
                               ics.code=unlist(str_extract_all(products$products.ics[i], "\\d+(\\.?\\d+)*")),
                               stringsAsFactors = F))
  print(i)
}


rm(products)



### intervention.type ("Technical Barrier to Trade", "TBT - Specific Trade Concern", "Sanitary and Phytosanitary Measure")
notifications.sps$intervention.type="Sanitary and Phytosanitary Measure"
notifications.tbt$intervention.type="Technical Barrier to Trade"
notifications.stc$intervention.type="TBT - Specific Trade Concern"


### url
setnames(notifications.sps, "doc.url", "url")
setnames(notifications.tbt, "doc.url", "url")
setnames(notifications.stc, "doc.url", "url")


### joint cleaning
common.var=c("intervention.id", "wto.id","implementing.jurisdiction","affected.jurisdiction", "title","description", "description.language", "date.announced", "date.implemented","date.removed", "date.published","date.adopted", "intervention.type", "url")
notifications=rbind(notifications.sps[,common.var],
                    notifications.tbt[,common.var],
                    notifications.stc[,common.var])




### date removed or inforced based on addendum
# addenda include date information in two cases
cases=rbind(as.data.frame.table(table(addenda.tbt$reason.for.add)), as.data.frame.table(table(addenda.sps$concern)))
in.force=c("entry into force|enters in to force")
removed=c("Withdrawal|withdrawn")

addenda.sps$original.symbol=gsub(".Add.*", "", addenda.sps$`Document symbol`)
addenda.tbt$original.symbol=gsub(".Add.*", "", addenda.tbt$Symbol)

addenda.sps$`Date of distribution`=as.Date(addenda.sps$`Date of distribution`, "%d/%m/%Y")
addenda.tbt$`Date of distribution`=as.Date(addenda.tbt$`Date of distribution`, "%d/%m/%Y")

# updating implementation dates
add.implemented=rbind(unique(subset(addenda.sps, grepl(in.force, concern))[,c("original.symbol", "Date of distribution")]), unique(subset(addenda.tbt, grepl(in.force, reason.for.add))[,c("original.symbol", "Date of distribution")]))
add.implemented=aggregate(`Date of distribution` ~ original.symbol, add.implemented, max)

setnames(add.implemented, "original.symbol","wto.id")
implemented=subset(notifications, wto.id %in% add.implemented$wto.id)
implemented=merge(implemented, add.implemented, by="wto.id", all.x=T)
implemented$date.implemented=implemented$`Date of distribution`
implemented$`Date of distribution`=NULL

notifications=rbind(subset(notifications, ! wto.id %in% add.implemented$wto.id), implemented)
rm(implemented)


# updating removal dates
add.removed=rbind(unique(subset(addenda.sps, grepl(removed, concern))[,c("original.symbol", "Date of distribution")]), unique(subset(addenda.tbt, grepl(removed, reason.for.add))[,c("original.symbol", "Date of distribution")]))
add.removed=aggregate(`Date of distribution` ~ original.symbol, add.removed, max)

setnames(add.removed, "original.symbol","wto.id")
removed=subset(notifications, wto.id %in% add.removed$wto.id)
removed=merge(removed, add.removed, by="wto.id", all.x=T)
removed$date.removed=removed$`Date of distribution`
removed$`Date of distribution`=NULL

notifications=rbind(subset(notifications, ! wto.id %in% add.removed$wto.id), removed)
rm(removed)


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



## targeted jurisdictions
setnames(conversion, "implementing.jurisdiction", "targeted.jurisdiction")
setnames(conversion, "i.un", "a.un")

targeted.jurisdictions=unique(notifications[,c("wto.id","affected.jurisdiction")])
names(targeted.jurisdictions)=c("wto.id","targeted.jurisdiction")
notifications$affected.jurisdiction=NULL

targeted.jurisdictions$targeted.jurisdiction[nchar(targeted.jurisdictions$targeted.jurisdiction)<3]="All"
targeted.jurisdictions=subset(targeted.jurisdictions, targeted.jurisdiction !="All")


targeted.jurisdictions=cSplit(targeted.jurisdictions, which(names(targeted.jurisdictions)=="targeted.jurisdiction"), sep="|", direction="long")

tjs=unique(merge(targeted.jurisdictions, conversion[,c("targeted.jurisdiction","a.un")], by="targeted.jurisdiction", all.x=T)[,c("targeted.jurisdiction","a.un")])
tjs$a.un[tjs$affected.jurisdiction=="blabla"]=1234


targeted.jurisdictions=merge(targeted.jurisdictions, tjs, by="targeted.jurisdiction", all.x=T)
targeted.jurisdictions=cSplit(targeted.jurisdictions, which(names(targeted.jurisdictions)=="a.un"), sep=",", direction = "long")
rm(tjs)

rnd=0
rounds=length(unique(targeted.jurisdictions$wto.id))
for(symbl in unique(targeted.jurisdictions$wto.id)){
  
  ijs=notifications$i.un[notifications$wto.id==symbl]
  targeted.jurisdictions=rbind(subset(targeted.jurisdictions, wto.id!=symbl),
                               subset(targeted.jurisdictions, wto.id==symbl & ! a.un %in% ijs))
  rm(ijs)
  rnd=rnd+1
  print(rnd/rounds)
}




# expanding HS codes

for(hs in unique(subset(products.hs, nchar(hs.code)<5)$hs.code)){
  
  products.hs$hs.code[products.hs$hs.code==hs]=paste(gta_hs_code_check(as.integer(hs)), collapse=";")
  
}

products.hs=cSplit(products.hs, which(names(products.hs)=="hs.code"), sep=";", direction="long")
products.hs$hs.code=as.integer(products.hs$hs.code)
products.hs=unique(products.hs)


old.vintages=unique(products.hs$hs.code)
old.vintages=old.vintages[! old.vintages %in% gtalibrary::hs.codes$hs.code]

if(length(old.vintages)>0){
  
  
  for(hs in old.vintages){
    
    products.hs$hs.code[products.hs$hs.code==hs]=paste(gta_hs_vintage_converter(as.integer(hs)), collapse=";")
    
  }
  
  products.hs=cSplit(products.hs, which(names(products.hs)=="hs.code"), sep=";", direction="long")
  products.hs$hs.code=as.integer(products.hs$hs.code)
  products.hs=unique(products.hs)
  
}
products.hs=subset(products.hs, hs.code %in% hs.codes$hs.code)


# expanding ICS codes
ics.names=gtalibrary::ics.names

all.ics=unique(products.ics$ics.code)

for(ics in all.ics[!all.ics %in% ics.names$ics.code[ics.names$most.granular]]){
  
  products.ics$ics.code[products.ics$ics.code==ics]=paste(ics.names$ics.code[ics.names$most.granular & grepl(paste0("^",ics), ics.names$ics.code)], collapse=";")
  
}

products.ics=cSplit(products.ics, which(names(products.ics)=="ics.code"), sep=";", direction="long")

products.ics=subset(products.ics, ics.code %in% ics.names$ics.code[ics.names$most.granular])


save(notifications, targeted.jurisdictions, products.hs, products.ics, file="data/WTO SPS & TBT/WTO SPS & TBT database.Rdata")

