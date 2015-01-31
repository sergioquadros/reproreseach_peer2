---
title: 'Severe Weather Events across USA from 1996 to 2011: The Ten Worst, Fatalities,
  Injuries and Costs'
author: "Sérgio Quadros"
date: '2015-01-20'
output:
  html_document: default
---

## **Synopsis**

The annual costs of the ten worst weather events spread between 5.51 and 125 billions USD at 1996-2011 period and the yearly fatalities spread between 1687 and 11864 citizens.

The ten worst events that caused more deaths was heat, tornado, flash flood, lightning, rip current, flood, thunderstorm wind, high wind, avalanche and winter storms and those that more hurt was  tornado, heat, flood, thunderstorm wind, lightning,  flash flood, wild forest fire, hurricanes, winter storms and high wind.

The property damages were by event type almost ten times higher than those crops. Ten worst types for properties were flood, hurricane, storm surge, tornado, flash flood, hail, thunderstorm wind, wild forest fire, tropical storm and high wind. 

Other ten worst types for cropies were: drought, hurricane, flood, hail, flash flood, extreme cold, frost freeze, thunderstorm wind, rain and tropical storm.

Of course the most frequent events may not be neither the most fatal nor that bring higher losses and therefore not serve as a compass for government action.

The North American citizens should be prepared to meet these challenges and agencies of interest in this area should continue its efforts to standardize your records and make them public. This work will give climate predictions more accurate, saving lives and wealth.

## **Introduction**

Many severe weather events can result in fatalities, injuries, and property damage, and preventing such outcomes to the extent possible is a key concern and this project involves exploring the U.S. National Oceanic and Atmospheric Administration's (NOAA) storm database. 

The events in the database start in the year 1950 and end in November 2011, but in the earlier years of the database there are generally fewer events recorded, most likely due to a lack of good records. More recent years should be considered more complete: ["3. All Event Types (48 from Directive 10-1605): From 1996 to present, 48 event types are recorded as defined in NWS Directive 10-1605"](http://www.ncdc.noaa.gov/stormevents/details.jsp).

## **Data Processing**

Our data analysis must address the following questions:

*  Across the United States, which types of events (as indicated in the EVTYPE variable) are most harmful with respect to population health?

*  Across the United States, which types of events have the greatest economic consequences?

The document was produced with **R version 3.1.2 at a i686-pc-linux-gnu (32-bit) Ubuntu**, also we settled the local time -  _it shall be resettled at the end_ - and global options for knitr:  


```r
knitr::opts_chunk$set(echo=TRUE,cache=FALSE)
local <- Sys.getlocale(category = "LC_TIME")
Sys.setlocale("LC_TIME", "en_US.UTF-8")
```

We called the following libraries:  

```r
if(!library(RCurl, logical.return = TRUE)) install.packages("RCurl", dependencies=TRUE)
if(!library(knitr, logical.return = TRUE)) install.packages("knitr", dependencies=TRUE)
if(!library(dplyr, logical.return = TRUE)) install.packages("dplyr", dependencies=TRUE)
if(!library(tidyr, logical.return = TRUE)) install.packages("tidyr", dependencies=TRUE)
if(!library(magrittr, logical.return = TRUE)) install.packages("magrittr", dependencies=TRUE)
if(!library(lubridate, logical.return = TRUE)) install.packages("lubridate", dependencies=TRUE)
if(!library(stringr, logical.return = TRUE)) install.packages("stringr", dependencies=TRUE)
if(!library(ggplot2, logical.return = TRUE)) install.packages("ggplot2", dependencies=TRUE)
if(!library(gridExtra, logical.return = TRUE)) install.packages("gridExtra", dependencies=TRUE)

library(RCurl)
library(knitr)
library(dplyr)
library(tidyr)
library(magrittr)
library(lubridate)
library(stringr)
library(ggplot2)
library(gridExtra)
```

We downloaded and unziped the [Storm Data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2) file from the course web site:

There is also some documentation of the database available that we will find how some of the variables are constructed/defined.

*  National Weather Service [Storm Data Documentation](https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf).

*  National Climatic Data Center Storm Events [FAQ](https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2FNCDC%20Storm%20Events-FAQ%20Page.pdf).


```r
url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
spliturl   <- unlist(strsplit(url,"/|%2F"))
zipfile    <- spliturl[length(spliturl)]
if ( ! file.exists(zipfile) ) {
        download.file(url, zipfile, method="curl")
        }
bzfile(description=zipfile, open = "rb", encoding = getOption("encoding"),compression = 9)
datafile <- bzfile(zipfile)
data1 <- tbl_df(read.csv(datafile))
```

```
## Warning: encerrando conexão não utilizada 5 (StormData.csv.bz2)
```

```r
time <- format(Sys.time(),"%a %b %d %X %Y")
total_EVTYPE <- length(unique(data1$EVTYPE))
```

The current time is Sat Jan 31 04:52:40 PM 2015.  

We decided to chose the following variables and our assumptions in absence of data's code book were based on the NOOA's report:

*  DATE: transformed begin date from BGN_DATE variable with "_month/day/year 00:00:00_" to "year" format.

*  FATALITIES: number of people killed directly or indirectly by the event.

*  INJURIES: number of people injured directly or indirectly by the event.

*  EVTYPE: type of the meteorological event leading to fatalities, injuries, damages, etc.

*  PROPDMG: property damage estimates in US dollar. 

*  PROPDMGEXP: the characters mean powers of ten, the final values are product between PROPDMG and PROPDMGEXP.

*  CROPDMG: Crop damage estimates in US dollar amounts.

*  CROPDMGEXP: exponents are similar to PROPDMGEXP.

We assumed that damage to the property is "propdmg" variable and they are multiplied by powers of ten with transformed "propdmgexp" variable, as well with crop's damage in "cropdmg" and "cropdmgexp" variables in accordance with [page 12](http://www.ncdc.noaa.gov/stormevents/pd01016005curr.pdf) of NOAA's report.


```r
colnames(data1) %<>% sapply(tolower)
firstElement <- function(x){x[1]}
data1$bgn_date %<>% as.character
y <- strsplit(data1$bgn_date, " ") 
data1$date  <- sapply(y, firstElement)
data1$date %<>% mdy 
data1$date %<>% year

# notifications' policy in time line
data0 <- select(data1,date,evtype)
summary_holedata <- data0 %>%
        group_by(date) %>%
        summarise(sum_events=n(),sum_types=length(unique(evtype)))
#summary_holedata$date %<>% as.factor
        

# making well formated values for evtype variable with GSUB/GREP PROCEDURE by HAND for date>1995
# selecting our interest's variables
data1 %<>% filter(date > 1995)
data1 %<>%
        select(date,evtype,fatalities:cropdmgexp)
data1$propdmgexp %<>% as.character
data1$cropdmgexp %<>% as.character        
exchange <- data.frame(power_char=c("0", "+", "-", "?", "" , "1", "2", "H", "h", "3","k", "K", "4", "5", "6", "m", "M",  "7",  "8", "B"), power_int=c(0, 0, 0, 0, 0, 1, 2, 2, 2, 3, 3, 3, 4, 5, 6, 6, 6, 7, 8, 9))
data1 %<>% merge(exchange,by.x="propdmgexp",by.y="power_char") %>%
                mutate(propdmg=propdmg*10^power_int) 
data1 %<>% select(-propdmgexp,-power_int)
data1 %<>% merge(exchange,by.x="cropdmgexp",by.y="power_char") %>%
                mutate(cropdmg=cropdmg*10^power_int) 
data1 %<>% select(-cropdmgexp,-power_int)
data1$evtype %<>% tolower
old <- unique(data1$evtype)
new1 <- old
new_ev <- data.frame(old,new1)
new_ev$old %<>% as.character
new_ev$new1 %<>% as.character

no <- c("(/|-|  )","(abnormally|abnormal|agricultural|damaging|dense|early|excessively|excessive|extremely|extreme|gusty|hard|heavy|late|ligth|minor|moderate|monthly|prolonged|prolong|record|strong|unseasonable|unseasonably|unseasonal|unusually|unusual|very)","tstm","strm","wnd","mud slide","coastalstorm","(hurricane edouard|hurricane typhoon|typhoon|hurricane hurricane)","(cstl flood erosion|coastal flood erosion|coastalflood|erosion/cstl flood)","(wildfire|brush fire)","(wintery mix|wintry mix|winter weather mix)","(fld|flooding|floodg|flood flood)","(g40|g35|g45|\\(0.75\\|40|45|41|\\)|\\()") 
yes <- c(" ","","thunderstorm","stream","wind","mudslide","coastal storm","hurricane","coastal flood","wild forest fire","winter weather","flood","")
exchange2 <- data.frame(no,yes)
exchange2$no %<>% as.character
exchange2$yes %<>% as.character
for(h in 1:18){
        for(k in 1:length(no)){
                i <- grep(exchange2[k,1],new_ev$new1)
                new_ev$new1[i] <- gsub(exchange2[k,1],exchange2[k,2],new_ev$new1[i])
                }
        new_ev$new1 <- str_trim(new_ev$new1, side = "both")
        h <- h+1
        }
i <- grep("s$",new_ev$new1)
new_ev$new1[i] <- gsub("s$","",new_ev$new1[i])
new_ev$new1 <- str_trim(new_ev$new1, side = "both")
i <- grep(" g$",new_ev$new1)
new_ev$new1[i] <- gsub(" g$","",new_ev$new1[i])
new_ev$new1 <- str_trim(new_ev$new1, side = "both")
# length(no)=length(yes)=13
# problems with "[:digit:]" and "[:punct:]"
data1 %<>% merge(new_ev,by.x="evtype",by.y="old") %>%
        select(-evtype) %>%
                mutate(evtype=new1) %>%
                        select(-new1)
data1$evtype %<>% as.factor
evtype_number <- length(unique(new_ev$new1))
```

We looked at number of events and number of types along the data set' time which was from 1950 to 2011 to discover how the agencies' policies was changed:


```r
titulo_1 <- "Notifications by Year"
q1 <- ggplot(summary_holedata,aes(date,sum_events/1000))+geom_path()+
        geom_line(stats="identity")+ylab("Frequency(x 1000)")+ ggtitle(titulo_1)+
        theme_bw()+theme(axis.text.x = element_text(angle=90))
titulo_2 <- "Reported Types by Year"
q2 <- ggplot(summary_holedata,aes(date,sum_types))+geom_path()+
        geom_line(stats="identity")+xlab("Year")+ylab("Frequency")+ ggtitle(titulo_2)+
        theme_bw()+theme(axis.text.x = element_text(angle=90))
grid.arrange(q1,q2,ncol=1, main = "Reported Events and Their Types Yearly")
```

![plot of chunk figure_policy](figure/figure_policy-1.png) 

We emphasize that the yearly number of types of events spread between 1 and 387 and fell to 48 and this fall began at 1996. Also the annual total number of notifications ranged between 223 and 62174 and it has been growing fast and independently of subtle changes in weather and that reflects the new policies of government agencies.

Thus we selected the observations made from 1996 because new strategy of NOOA that proposed to standardize the types of events and increase the number of observations. Another filter was the _gsub procedure_ which got 310 types of events at 1996-2011 period that was compared with all 985 types at 1950-2011. The _gsub procedure_ selected types by elimination of some adjectives, adverbs and white spaces and some abbreviations that were undone among values of EVTYPE. We lost some information, but we joined what was really similar.


```r
# Costs(billions USD) and Fatalities by Year: 1996 to 2011
by_year <- data1 %>% 
        group_by(date) %>%
                summarise(deaths=sum(fatalities),hurts=sum(injuries),
                          property=sum(propdmg/10^9),crop=sum(cropdmg/10^9),
                          total_costs=sum((propdmg+cropdmg)/10^9))
# Cumulative Costs(billions USD) and Fatalities by Event Types
by_eventype <- data1 %>% 
        group_by(evtype) %>%
                summarise(frequency=n(),deaths=sum(fatalities),hurts=sum(injuries),
                  property=sum(propdmg/10^9),crop=sum(cropdmg/10^9),
                  total_costs=sum((propdmg+cropdmg)/10^9))
```

## **Results**

Initially we point out that the most frequent events in the sixteen years were not the ones who caused more deaths, injuries and losses. Below table shows the absolute cumulative frequency in these years according to the types of events and we can compare the following figure:


```r
# cumulative deaths, injuries, frequency, property damage, crop damage and
# damage' sum by event type at '1996-2011' period.
top_deathEV <- by_eventype %>% 
        select(evtype,deaths) %>% 
                arrange(desc(deaths)) %>% 
                        top_n(10,deaths)
top_hurtEV <- by_eventype %>% 
        select(evtype,hurts) %>% 
                arrange(desc(hurts)) %>% 
                        top_n(10,hurts)
top_freqEV <- by_eventype %>% 
        select(evtype,frequency) %>% 
                arrange(desc(frequency)) %>% 
                        top_n(10,frequency)
top_freqEV$frequency <- 100*(as.numeric(format(top_freqEV$frequency,digits=3)))/length(data1$evtype)
percent <- sum(top_freqEV$frequency)
         
top_propertyEV <- by_eventype %>% 
        select(evtype,property) %>% 
                arrange(desc(property)) %>% 
                        top_n(10,property)
top_cropEV <- by_eventype %>% 
        select(evtype,crop) %>% 
                arrange(desc(crop)) %>%
                        top_n(10,crop)
top_totalEV <- by_eventype %>% 
        select(evtype,total_costs) %>% 
                arrange(desc(total_costs)) %>%
                        top_n(10,total_costs)
```

Thus the ten most frequent types corresponds to 89.9% of notifications in 1996-2011 period: thunderstorm wind, hail, flash flood, flood, tornado, high wind, snow, lightning, marine thunderstorm and rain. They were not the ones who gave more damage and not those who caused more deaths - heat, tornado, flash flood, lightning, rip current, flood, thunderstorm wind, high wind, avalanche and winter storms - and those that more hurt was  tornado, heat, flood, thunderstorm wind, lightning,  flash flood, wild forest fire, hurricanes, winter storms and high wind as the following two graphs show in next panel. Tornadoes have injuried more than heat, but heat is more killer than tornadoes. See following panel:


```r
titulo1 <- "Event Types X Death" 
p1 <- ggplot(top_deathEV,aes(reorder(evtype,deaths),deaths))+ coord_flip()+
        geom_bar(stat="identity")+xlab("Event Types")+ylab("Deaths")+
        ggtitle(titulo1)+theme_bw()+theme(axis.text.x = element_text(angle=45))
titulo2 <- "Event Types X Hurts" 
p2 <- ggplot(top_hurtEV,aes(reorder(evtype,hurts),hurts))+ coord_flip()+
        geom_bar(stat="identity")+xlab("Event Types")+ylab("Hurts")+
        ggtitle(titulo2)+theme_bw()+theme(axis.text.x = element_text(angle=45))
titulo3 <- "Event Types X Frequency" 
p3 <- ggplot(top_freqEV,aes(reorder(evtype,frequency),frequency))+
        coord_flip()+geom_bar(stat="identity")+xlab("Event Types")+
        ylab("Events' Frequency(%)")+ggtitle(titulo3)+ theme_bw()+
        theme(axis.text.x = element_text(angle=45))
grid.arrange(p1,p2,p3,ncol=1, main = "Cumulative Fatalities and Injuries: 1996-2011")
```

![plot of chunk figure_1 by_eventype](figure/figure_1 by_eventype-1.png) 

Property and crop damages are seen in following panel by event type and property damage to almost ten times higher than those crops. Ten worst types for properties are flood, hurricane, storm surge, tornado, flash flood, hail, thunderstorm wind, wild forest fire, tropical storm and high wind. Other ten worst types for cropies are: drought, hurricane, flood, hail, flash flood, extreme cold, frost freeze, thunderstorm wind, rain and tropical storm.


```r
titulo4 <- "Type X Property" 
p4 <- ggplot(top_propertyEV,aes(reorder(evtype,property),property))+
        coord_flip()+geom_bar(stat="identity")+xlab("Event Types")+
        ylab("Property Damage (billions USD)")+ggtitle(titulo4)+
        theme_bw()+theme(axis.text.x = element_text(angle=45))
titulo5 <- "Type X Crop" 
p5 <- ggplot(top_cropEV,aes(reorder(evtype,crop),crop))+coord_flip()+
        geom_bar(stat="identity")+xlab("Event Types")+
        ylab("Crop Damage (billions USD)")+ggtitle(titulo5)+
        theme_bw()+theme(axis.text.x = element_text(angle=45))
titulo6 <- "Type X Total Costs" 
p6 <- ggplot(top_totalEV,aes(reorder(evtype,total_costs),total_costs))+
        coord_flip()+geom_bar(stat="identity")+xlab("Event Types")+
        ylab("Total Costs (billions USD)")+ggtitle(titulo6)+
        theme_bw()+theme(axis.text.x = element_text(angle=45))
grid.arrange(p4,p5,p6,ncol=1, main = "Cumulative Costs in 1996-2011 by Event Types")
```

![plot of chunk figure_2 by_eventype](figure/figure_2 by_eventype-1.png) 

At last we got yearly costs for crop and properties in 1996-2011 period - 2005 and 2006 are more expensive years - at folowing panel, also the yearly causalities are figured out:


```r
# f1 by_year: x=date, y=(deaths, hurts)
titulo7 <- "Fatalities X Year" 
f1 <- ggplot(by_year,aes(date))+geom_line(aes(y = deaths, colour = "Death"))+
        geom_line(aes(y = hurts, colour = "Injury"))+xlab("Year")+ 
        ylab("Deaths and Injuries")+ ggtitle(titulo7)+
        theme_bw()+theme(axis.text.x = element_text(angle=45))
# f2 by_year: x=date, y=(property, crop, total_costs, total_costs' model)
titulo8 <- "Damage X Year" 
f2 <- ggplot(by_year,aes(date))+
        geom_line(aes(y = property, colour = "Property"))+
        geom_line(aes(y = crop, colour = "Crop"))+
        geom_line(aes(y = total_costs, colour = "Total Costs"))+
        xlab("Year")+ylab("Damages (billions USD)")+ ggtitle(titulo8)+
        theme_bw()+theme(axis.text.x = element_text(angle=45))
grid.arrange(f1,f2,ncol=1, main = "Fatalities and Costs in 1996-2011 by Year")
```

![plot of chunk figure_3 by_year](figure/figure_3 by_year-1.png) 

So the annual costs spread between 5.51 and 125 billions USD and their mean cost was 25.1 $\pm $ 35.1 billions USD.

The annual fatalities spread between 1687 and 11864 citizens and their mean was 4169 $\pm $ 2661 citizens.


```r
Sys.setlocale("LC_TIME", local)
```
