---
title: "VISTA analyse"
author: "Maitreya Wagh"
date: "5/22/2018"
output:
  html_document:
    keep_md: yes
---



```r
library(ggplot2)
Work <- read.csv("../data/vista/2018-05-23-vista-2013-16/VISTA_2012_16_v1_SA1_CSV/JTW_VISTA12_16_SA1_V1.csv")
Work$ModeType[Work$JTWMODE=="Vehicle Driver"|Work$JTWMODE == "Vehicle Passenger"] <- "Private"
Work$ModeType[Work$JTWMODE=="Walking"|Work$JTWMODE == "Bicycle"] <- "Active"
Work$ModeType[Work$JTWMODE=="Public Bus"|Work$JTWMODE=="Tram"|Work$JTWMODE=="Train"|Work$JTWMODE=="Taxi"] <- "Public"
Work$ModeType[Work$JTWMODE=="Motorcycle"|Work$JTWMODE=="Other"]<-"Other"
Work$Isweekend <- ifelse(Work$TRAVDOW == "Sunday" | Work$TRAVDOW == "Saturday", "Weekend", "Weekday")
ActivWork <- subset(Work[Work$ModeType == "Active"&Work$Isweekend == "Weekday",])
ggplot(Work, aes(x=Work$ModeType, fill = Work$JTWMODE, weight = Work$CW_WDJTWWGT_LGA))+geom_bar()
```

![](Checking_files/figure-html/unnamed-chunk-1-1.png)<!-- -->

