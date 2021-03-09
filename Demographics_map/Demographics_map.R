
setwd("/Users/yikeshen/Desktop/ML_paper/ESP804")library(codebook)
library(foreign)
library(dplyr)
library(stringr)

filter <- dplyr::filter
select <- dplyr::select
qualtricsraw = read.spss("ESP804 S19.foodsafetyonly.sav", to.data.frame=TRUE)
qualtricsraw=as.data.frame(qualtricsraw)
qualtricsraw[] <- lapply(qualtricsraw, as.character)

#Tidying data, Only for the good complete (1 and 3) and good data
qualtricsprocessing <- qualtricsraw %>% filter(str_detect(gc, "3|1")) %>% 
  select(YOB,GENDER,RACEETH,MARITALSTATUS,EDUCATION,EMPLOYED,ZIP,RESIDENCETYPE,URBANRURAL,RESIDENCE_MALE18,
         RESIDENCE_FEMALE18,RESIDENCE_CHILDREN,RESIDENCE_INFANT,INCOME,Q94,Q95,Q96,Q97,Q97,Q97_5_TEXT,Q111,
         Q104,Q105,Q105_3_TEXT,Q98_1_1,Q98_1_2,Q98_1_3,Q98_1_4,Q98_1_5,Q98_1_6,Q98_1_7,Q98_1_8,Q98_1_9,Q98_2_1,Q98_2_2,
         Q98_2_3,Q98_2_4,Q98_2_5,Q98_2_6,Q98_2_7,Q98_2_8,Q98_2_9,Q103_1,Q103_2,Q103_3,Q103_4,Q103_5,Q102,Q107)
#install.packages("usa")
#install.packages('viridis')
#install.packages('ggmap')
#install.packages('maps')
#devtools::install_github("dkahle/ggmap")
library("usa")
library(plyr)
library(ggmap)
library(viridis)
library(maps)

zcs <- usa::zipcodes %>% as.data.frame()
register_google(key = "[#####yourkey]")
has_google_key()

Qual.ZIPCode<-aggregate(data.frame(count=qualtricsprocessing$ZIP),list(zip=qualtricsprocessing$ZIP),length)
Qual.ZIPCode <- Qual.ZIPCode %>% dplyr::rename(zipourdata=zip)
Qual.ZIPCode$zipourdata <- substr(Qual.ZIPCode$zipourdata, 1, 5)

zipmap<- merge(Qual.ZIPCode, zcs, by.x="zipourdata", by.y="zip")

us<-map_data('state')
ggplot(zipmap,aes(long,lat)) + 
  geom_polygon(data=us,aes(x=long,y=lat,group=group),color='gray',fill=NA,alpha=.35)+
  geom_point(aes(size = count))+
  xlim(-125,-65)+ylim(20,50)+
  theme_bw()+
  theme(axis.text=element_text(size=18),axis.title=element_text(size=18),
        plot.title = element_text(size=25, hjust=0.5),text = element_text(size = 18))+
  ggtitle("Survey Distribution")+xlab("Longitude") + ylab("Latitude")


#+ scale_colour_gradient(low = "dark gray", high = "black")
#,size=4,alpha=0.5
