library(readr)
library(dplyr)
library(lubridate)
library(maptools)
library(rgdal)
library(rgeos) 
library(choroplethr)
library(choroplethrMaps)
library(RColorBrewer)
library(plotly)
Taiwan_Crim_01_03 <- read_csv("Crim_10601_10603.csv", locale = locale(encoding = "BIG5"))
Taiwan_Crim_04_06 <- read_csv("Crim_10604_10606.csv", locale = locale(encoding = "BIG5"))
Taiwan_Crim_07_09 <- read_csv("Crim_10607_10609.csv", locale = locale(encoding = "BIG5"))
Taiwan_Crim_10_12 <- read_csv("Crim_10610_10612.csv", locale = locale(encoding = "BIG5"))
New_Taipei_Region <- readShapeSpatial("新北市區界")

Taiwan_crim_2017 <- Reduce(function(...) merge(..., all=TRUE), list(Taiwan_Crim_01_03, Taiwan_Crim_04_06, Taiwan_Crim_07_09, Taiwan_Crim_10_12))
Taiwan_crim_2017$發生日期 <- substr(Taiwan_crim_2017$發生日期, start = 4, stop = 7)
Taiwan_crim_2017$發生日期 <- paste0("2017", Taiwan_crim_2017$發生日期)
Taiwan_crim_2017$發生日期 <- ymd(Taiwan_crim_2017$發生日期)
Taiwan_crim_2017 <- filter(Taiwan_crim_2017, 發生地點 != "外國")
Taiwan_crim_2017$縣市 <- substr(Taiwan_crim_2017$發生地點, start = 1, stop = 3)
Taiwan_crim_2017$區域別 <- Taiwan_crim_2017$發生地點





New_Taipei_City_Crim <- filter(Taiwan_crim_2017, 縣市 == "新北市")
New_Taipei_City_Crim$id <- substr(New_Taipei_City_Crim$發生地點, start = 4, stop = 6)

New_Taipei_City_Crim_2 <- New_Taipei_City_Crim%>%
  group_by(id)%>%
  summarise(Crim_Number = n())%>%
  arrange(desc(Crim_Number))

New_Taipei_City_Crim_2 <- New_Taipei_City_Crim_2[-3,]


############
New_Taipei_City_Crim_2_2 <- New_Taipei_City_Crim%>%
  group_by(id,案類)%>%
  summarise(Crim_Number = n())%>%
  arrange(案類) 

New_Taipei_City_Crim_HSteal <- New_Taipei_City_Crim_2_2[1:27,] %>%
  arrange(desc(Crim_Number))%>%
  head(3)

New_Taipei_City_Crim_CSteal <- New_Taipei_City_Crim_2_2[28:47,] %>%
  arrange(desc(Crim_Number))%>%
  head(3)

New_Taipei_City_Crim_Drug <- New_Taipei_City_Crim_2_2[48:76,] %>%
  arrange(desc(Crim_Number))%>%
  head(3)

New_Taipei_City_Crim_Rape <- New_Taipei_City_Crim_2_2[77:91,] %>%
  arrange(desc(Crim_Number))%>%
  head(3)

New_Taipei_City_Crim_Robber <- New_Taipei_City_Crim_2_2[92:103,] %>%
  arrange(desc(Crim_Number))%>%
  head(3)

New_Taipei_City_Crim_Robber_2 <- New_Taipei_City_Crim_2_2[104:118,] %>%
  arrange(desc(Crim_Number))%>%
  head(3)

New_Taipei_City_Crim_SSteal <- New_Taipei_City_Crim_2_2[120:141,] %>%
  arrange(desc(Crim_Number))%>%
  head(3)


A <- New_Taipei_City_Crim_2_2 %>%
  group_by(案類) %>%
  head()
###############
New_Taipei_City_Crim_HSteal$id <- factor(New_Taipei_City_Crim_HSteal$id, levels = New_Taipei_City_Crim_HSteal$id[order(desc(New_Taipei_City_Crim_HSteal$Crim_Number))])
New_Taipei_City_Crim_HSteal_Plot_Bar <- ggplot() + 
  geom_bar(data = New_Taipei_City_Crim_HSteal, aes(x = id, y = Crim_Number), stat = "identity", width = 0.5, color="firebrick4", fill = "orange") + 
  labs(x = "地區", y = "住宅竊盜", title = "新北市住宅竊盜前三名")

New_Taipei_City_Crim_CSteal$id <- factor(New_Taipei_City_Crim_CSteal$id, levels = New_Taipei_City_Crim_CSteal$id[order(desc(New_Taipei_City_Crim_CSteal$Crim_Number))])
New_Taipei_City_Crim_CSteal_Plot_Bar <- ggplot() + 
  geom_bar(data = New_Taipei_City_Crim_CSteal, aes(x = id, y = Crim_Number), stat = "identity", width = 0.5, color="firebrick4", fill = "orange") + 
  labs(x = "地區", y = "汽車竊盜", title = "新北市汽車竊盜前三名")

New_Taipei_City_Crim_Drug$id <- factor(New_Taipei_City_Crim_Drug$id, levels = New_Taipei_City_Crim_Drug$id[order(desc(New_Taipei_City_Crim_Drug$Crim_Number))])
New_Taipei_City_Crim_Drug_Plot_Bar <- ggplot() + 
  geom_bar(data = New_Taipei_City_Crim_Drug, aes(x = id, y = Crim_Number), stat = "identity", width = 0.5, color="firebrick4", fill = "orange") + 
  labs(x = "地區", y = "毒品犯罪", title = "新北市毒品犯罪前三名")

New_Taipei_City_Crim_Rape$id <- factor(New_Taipei_City_Crim_Rape$id, levels = New_Taipei_City_Crim_Rape$id[order(desc(New_Taipei_City_Crim_Rape$Crim_Number))])
New_Taipei_City_Crim_Rape_Plot_Bar <- ggplot() + 
  geom_bar(data = New_Taipei_City_Crim_Rape, aes(x = id, y = Crim_Number), stat = "identity", width = 0.5, color="firebrick4", fill = "orange") + 
  labs(x = "地區", y = "強制性交", title = "新北市強制性交前三名")

New_Taipei_City_Crim_Robber$id <- factor(New_Taipei_City_Crim_Robber$id, levels = New_Taipei_City_Crim_Robber$id[order(desc(New_Taipei_City_Crim_Robber$Crim_Number))])
New_Taipei_City_Crim_Robber_Plot_Bar <- ggplot() + 
  geom_bar(data = New_Taipei_City_Crim_Robber, aes(x = id, y = Crim_Number), stat = "identity", width = 0.5, color="firebrick4", fill = "orange") + 
  labs(x = "地區", y = "強盜罪", title = "新北市強盜罪前三名")

New_Taipei_City_Crim_Robber_2$id <- factor(New_Taipei_City_Crim_Robber_2$id, levels = New_Taipei_City_Crim_Robber_2$id[order(desc(New_Taipei_City_Crim_Robber_2$Crim_Number))])
New_Taipei_City_Crim_Robber_2_Plot_Bar <- ggplot() + 
  geom_bar(data = New_Taipei_City_Crim_Robber_2, aes(x = id, y = Crim_Number), stat = "identity", width = 0.5, color="firebrick4", fill = "orange") + 
  labs(x = "地區", y = "搶奪罪", title = "新北市搶奪罪前三名")

New_Taipei_City_Crim_SSteal$id <- factor(New_Taipei_City_Crim_SSteal$id, levels = New_Taipei_City_Crim_SSteal$id[order(desc(New_Taipei_City_Crim_SSteal$Crim_Number))])
New_Taipei_City_Crim_SSteal_Plot_Bar <- ggplot() + 
  geom_bar(data = New_Taipei_City_Crim_SSteal, aes(x = id, y = Crim_Number), stat = "identity", width = 0.5, color="firebrick4", fill = "orange") + 
  labs(x = "地區", y = "機車竊盜", title = "新北市機車竊盜前三名")







##只有毒品
New_Taipei_City_Drug <- New_Taipei_City_Crim_2_2[grepl("毒品", New_Taipei_City_Crim_2_2$案類),]

################


New_Taipei_City_Crim_2_2 <- New_Taipei_City_Crim_2_2[-119,]


New_Taipei_Region <- fortify(New_Taipei_Region, region = "ADMIT")
New_Taipei_City_Crim_3 <- full_join(New_Taipei_City_Crim_2, New_Taipei_Region, by = "id")


New_Taipei_City_Crim_Plot<-ggplot() +
  geom_polygon(data = New_Taipei_City_Crim_3, 
               aes(x = long, y = lat, 
                   group = group, 
                   fill = Crim_Number), 
               color = "black", 
               size = 0.25) + 
  scale_fill_gradientn(
    colours = brewer.pal(9,"Reds"))+
  theme_void() +
  labs(title="新北市犯罪數量面量圖")

New_Taipei_City_Crim_2$id <- factor(New_Taipei_City_Crim_2$id, levels = New_Taipei_City_Crim_2$id[order(desc(New_Taipei_City_Crim_2$Crim_Number))])

New_Taipei_City_Crim_Plot_Bar <- ggplot() + 
  geom_bar(data = New_Taipei_City_Crim_2, 
           aes(x = id, y = Crim_Number), stat = "identity", width = 1, color="firebrick4", fill = "orange")+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.6, size = 8)) + 
  labs(x = "地區", y = "犯罪數", title = "新北市各區犯罪數柱狀圖")





####################################


New_Taipei_City_Crim_3_2 <- full_join(New_Taipei_City_Crim_2_2, New_Taipei_Region, by = "id")


##新北市學校資料

New_Taipei_City_School <- read_csv("New_Taipei_City_School.csv", 
                                   locale = locale(encoding = "BIG5"))

colnames(New_Taipei_City_School)[3] <- "id"

##只取國中
New_Taipei_City_School <- New_Taipei_City_School[grepl("國中|高職|高中", New_Taipei_City_School$types), ]

New_Taipei_City_NSchool <- group_by(New_Taipei_City_School, id) %>% summarise(School_Number = n())

A <- summarise(New_Taipei_City_NSchool, N = sum (School_Number))
  
New_Taipei_City_School_2 <- full_join(New_Taipei_City_NSchool, New_Taipei_Region, by = "id")

New_Taipei_City_School_Plot <- ggplot() +
  geom_polygon(data = New_Taipei_City_School_2, 
               aes(x = long, y = lat, 
                   group = group, 
                   fill = School_Number), 
                   color = "black", 
                   size = 0.25) + 
  scale_fill_gradientn(colours = brewer.pal(9,"Reds"))+
  theme_void()+
  labs(title="新北市國高中職數量面量圖")


#106年人口分布https://www.ca.ntpc.gov.tw/home.jsp?id=220&parentpath=0,2,43,219民政局
New_Taipei_Census_106 <- read_csv("New_Taipei_Census_106.csv", 
                                  locale = locale(encoding = "BIG5"), na = "empty")

New_Taipei_Census_106 <- New_Taipei_Census_106[-c(30:33),]


New_Taipei_Census_106 <- New_Taipei_Census_106 %>% 
  mutate(Population = 男 + 女)

colnames(New_Taipei_Census_106)[1] <- "id"
New_Taipei_Census_106_2 <- full_join(New_Taipei_Census_106, New_Taipei_Region, by = "id")

New_Taipei_Census_106_3 <- New_Taipei_Census_106[,c(1,7)]

New_Taipei_City_Population_Plot <- ggplot() +
  geom_polygon(data = New_Taipei_Census_106_2, 
               aes(x = long, y = lat, 
                   group = group, 
                   fill = Population), 
               color = "black", 
               size = 0.25) + 
  scale_fill_gradientn(colours = brewer.pal(9,"Reds"))+
  theme_void()+
  labs(title="新北市各區人口數量面量圖")

##106年教育程度

New_Taipei_City_Education_106 <- read_csv("New_Taipei_City_Education_106.csv", 
                                          locale = locale(encoding = "BIG5"), skip = 1)

##取大學以下(12年國教)
New_Taipei_City_Education_106 <- New_Taipei_City_Education_106[,c(1,2,24:45)]

New_Taipei_City_Education_106 <- New_Taipei_City_Education_106[grepl("新北市", New_Taipei_City_Education_106$區域別),]

New_Taipei_City_Education_106 <- New_Taipei_City_Education_106 %>% 
  group_by(區域別,村里名稱) %>%
 mutate(Total_Student = sum(高畢_男+高畢_女+高肄_男+高肄_女+職畢_男+職畢_女+職肄_男+職肄_女+前三肄_男+前三肄_女+
                               國畢_男+國畢_女+國肄_男+國肄_女+初畢_男+初畢_女+初肄_男+初肄_女+小畢_男+小畢_女+
                               小肄_男+小肄_女))
Drop_Out <- New_Taipei_City_Education_106[, grepl("肄", colnames(New_Taipei_City_Education_106))]
Drop_Out$區域別 <- New_Taipei_City_Education_106$區域別
Drop_Out$村里名稱 <- New_Taipei_City_Education_106$村里名稱
 
  
  
Drop_Out <- Drop_Out %>%
  group_by(區域別,村里名稱) %>%
  mutate(Total_Drop_Out = sum(高肄_男+高肄_女+職肄_男+職肄_女+前三肄_男+前三肄_女+
                              國肄_男+國肄_女+初肄_男+初肄_女+小肄_男+小肄_女))

New_Taipei_City_Education_106$Drop_Out <- Drop_Out$Total_Drop_Out

New_Taipei_City_Education_106$Drop_Out_Rate <- Drop_Out$Total_Drop_Out/New_Taipei_City_Education_106$Total_Student 

New_Taipei_City_Education_106 <- New_Taipei_City_Education_106 %>% group_by(區域別) %>%
  summarise(Drop_Out_Rate = sum(Drop_Out)/sum(Total_Student))

colnames(New_Taipei_City_Education_106)[1] <- "id"

New_Taipei_City_Education_106$id <- substr(New_Taipei_City_Education_106$id, start = 4, stop = 6)




  




remove(Drop_Out)
B <- New_Taipei_City_Education_106[, grepl("畢", colnames(New_Taipei_City_Education_106))]



sum(New_Taipei_City_Education_106[,3:24]))



##犯罪率#########

New_Taipei_City_Crim_Ratio <- full_join(New_Taipei_Census_106, New_Taipei_City_Crim_2, by = "id") %>%
  mutate(Crim_Ratio = Crim_Number/Population*100000) %>%
  arrange(desc(Crim_Ratio))

head(New_Taipei_City_Crim_Ratio)

New_Taipei_City_Crim_Ratio_2 <- full_join(New_Taipei_City_Crim_Ratio, New_Taipei_Region, by = "id")
New_Taipei_City_Crim_Ratio_3 <- New_Taipei_City_Crim_Ratio[,c(1,9)]

New_Taipei_City_Crim_Ratio_Plot <- ggplot()+
  geom_polygon(data = New_Taipei_City_Crim_Ratio_2, 
               aes(x = long, y = lat, 
                   group = group, 
                   fill = Crim_Ratio), 
               color = "black", 
               size = 0.25) + 
  scale_fill_gradientn(colours = brewer.pal(9,"Reds"))+
  theme_void()+
  labs(title="新北市各區犯罪率面量圖")

New_Taipei_City_Crim_Ratio$id <- factor(New_Taipei_City_Crim_Ratio$id, levels = New_Taipei_City_Crim_Ratio$id[order(desc(New_Taipei_City_Crim_Ratio$Crim_Ratio))])


New_Taipei_City_Crim_Ratio_Plot_Bar <- ggplot() + 
  geom_bar(data = New_Taipei_City_Crim_Ratio, 
           aes(x = id, y = Crim_Ratio),stat = "identity", width = 1, color="firebrick4", fill = "orange")+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.6, size = 8)) + 
  labs(x = "地區", y = "犯罪率", title = "新北市各區犯罪率柱狀圖")

##############









##新北市廟宇密度 新北市資料開放平台

New_Taipei_City_Temple <- read_csv("New_Taipei_City_Temple.csv", 
                                   locale = locale(encoding = "BIG5"))

New_Taipei_City_Temple_Num <- New_Taipei_City_Temple %>%
  group_by(id) %>%
  summarise(Temple_Number = n())

New_Taipei_City_Temple_Num <- New_Taipei_City_Temple_Num[-30,]


New_Taipei_City_Temple <- full_join(New_Taipei_City_Temple_Num, New_Taipei_Region, by = "id")


New_Taipei_City_Temple_Plot <- ggplot() +
  geom_polygon(data = New_Taipei_City_Temple, 
               aes(x = long, y = lat, 
                   group = group, 
                   fill = Temple_Number), 
               color = "black", 
               size = 0.25) + 
  scale_fill_gradientn(colours = brewer.pal(9,"Reds"))+
  theme_void()+
  labs(title="新北市各區廟宇數面量圖")


##離婚率(新北市各區粗離婚率(以千分率算))
library(tidyr)

New_Taipei_City_Divorce <- read_csv("New_Taipei_City_Divorce.csv", 
                                    locale = locale(encoding = "BIG5"), skip = 1)

New_Taipei_City_Divorce[1,2:30] <- as.numeric(New_Taipei_City_Divorce[1,2:30])
New_Taipei_City_Divorce[1,2:30] <- as.numeric(New_Taipei_City_Divorce[1,2:30]/1000)

New_Taipei_City_Divorce<-gather(New_Taipei_City_Divorce,
                                key=id,value=Divorce,
                                板橋區:萬里區)

New_Taipei_City_Divorce<- New_Taipei_City_Divorce[,-1] 

New_Taipei_City_Divorce_2 <- full_join(New_Taipei_City_Divorce, New_Taipei_Region, by = "id")

New_Taipei_City_Divorce_Plot <- ggplot() +
  geom_polygon(data = New_Taipei_City_Divorce_2, 
               aes(x = long, y = lat, 
                   group = group, 
                   fill = Divorce), 
               color = "black", 
               size = 0.25) + 
  scale_fill_gradientn(colours = brewer.pal(9,"Reds"))+
  theme_void()+
  labs(title="新北市各區離婚率面量圖")


##資料探勘

New_Taipei_City_Crim_2
New_Taipei_City_NSchool
New_Taipei_Census_106
New_Taipei_City_Temple_Num

New_Taipei_Data <- full_join(New_Taipei_City_Crim_Ratio_3, New_Taipei_City_NSchool, by = "id")
New_Taipei_Data <- full_join(New_Taipei_Data, New_Taipei_Census_106_3, by = "id")
New_Taipei_Data <- full_join(New_Taipei_Data, New_Taipei_City_Temple_Num, by = "id")
New_Taipei_Data <- full_join(New_Taipei_Data, New_Taipei_City_Education_106, by = "id")
New_Taipei_Data <- full_join(New_Taipei_Data, New_Taipei_City_Divorce, by = "id")


mode_1 <- glm(Crim_Ratio ~ School_Number + Population + Temple_Number + Drop_Out_Rate + Divorce,
              data = New_Taipei_Data)
sum<-summary(mode_1)
A <- sum$coefficients

library(MASS)

mode_2 <- 
  stepAIC(mode_1,
          direction = "both",
          trace = FALSE)

summary(mode_2)$coefficients



#毒品
New_Taipei_Data_2 <- full_join(New_Taipei_City_Drug, New_Taipei_City_NSchool, by = "id")
New_Taipei_Data_2 <- full_join(New_Taipei_Data_2, New_Taipei_Census_106_3, by = "id")
New_Taipei_Data_2 <- full_join(New_Taipei_Data_2, New_Taipei_City_Temple_Num, by = "id")
New_Taipei_Data_2 <- full_join(New_Taipei_Data_2, New_Taipei_City_Education_106, by = "id")

New_Taipei_Data_2 <- New_Taipei_Data_2 %>%
  mutate(Crim_Ratio = Crim_Number/Population*100000) %>%
  arrange(desc(Crim_Ratio))

mode_2 <- glm(Crim_Ratio ~ School_Number + Population + Temple_Number + Drop_Out_Rate,
              data = New_Taipei_Data_2)
sum<-summary(mode_2)





