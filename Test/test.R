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


New_Taipei_City_Crim_2_2 <- New_Taipei_City_Crim%>%
  group_by(id,案類)%>%
  summarise(Crim_Number = n())%>%
  arrange(desc(Crim_Number))

New_Taipei_City_Crim_2_2 <- New_Taipei_City_Crim_2_2[-2,]


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
  labs(title="New_Taipei_City_Crim")

####################################
New_Taipei_City_Crim_3_2 <- full_join(New_Taipei_City_Crim_2_2, New_Taipei_Region, by = "id")













ggplotly(id,twcmap)
?ggplotly
twcmap<-ggplotly() +
  geom_polygon(data = final.plot, 
               aes(x = long, y = lat, 
                   group = group, 
                   fill = prevalence), 
               color = "black", 
               size = 0.25) + 
  coord_map()+#維持地圖比例
  scale_fill_gradientn(
    colours = brewer.pal(9,"Reds"))+
  theme_void()+
  labs(title="Prevalence of X in Taiwan")

twcmap



library(readr)
library(choroplethr)
library(choroplethrMaps)
library(XML)
library(ggplot2) 
library(rgdal)
library(rgeos) 
library(maptools)
library(dplyr)
ggiris <- qplot(Petal.Width, Sepal.Length, data = iris, color = Species)
ggplotly(ggiris)

data(canada.cities, package = "maps")
viz <- ggplot(canada.cities, aes(long, lat)) +
  borders(regions = "canada") +
  coord_equal() +
  geom_point(aes(text = name, size = pop), colour = "red", alpha = 1/2)
ggplotly(viz, tooltip = c("text", "size"))

library(ggmap)
Taoyuan_map <- get_googlemap(center = c(lon=121.20,lat=25.00), 
                             zoom = 7,
                             language = "zh-TW")

Taoyuan_map_2 <- ggmap(Taoyuan_map,extent = "device") + 
  ggplot()+
  geom_polygon(data = Taipei_Data_2, 
               aes(x = long, y = lat, 
                   group = group, 
                   fill = 案件數), 
               color = "black", 
               size = 0.25)

##新北市學校資料

New_Taipei_City_School <- read_csv("New_Taipei_City_School.csv", 
                                   locale = locale(encoding = "BIG5"))

colnames(New_Taipei_City_School)[3] <- "id"

New_Taipei_City_NSchool <- group_by(New_Taipei_City_School, id) %>% summarise(School_Number = n())

New_Taipei_City_School_2 <- full_join(New_Taipei_City_NSchool, Taipei_Data_1, by = "id")

New_Taipei_City_School_Plot <- ggplot() +
  geom_polygon(data = New_Taipei_City_School_2, 
               aes(x = long, y = lat, 
                   group = group, 
                   fill = School_Number), 
                   color = "black", 
                   size = 0.25) + 
  scale_fill_gradientn(colours = brewer.pal(9,"Reds"))+
  theme_void()+
  labs(title="New_Taipei_City_School_Plot")


#106年人口分布https://www.ca.ntpc.gov.tw/home.jsp?id=220&parentpath=0,2,43,219民政局
New_Taipei_Census_106 <- read_csv("New_Taipei_Census_106.csv", 
                                  locale = locale(encoding = "BIG5"), na = "empty")

New_Taipei_Census_106 <- New_Taipei_Census_106[-c(30:33),]

New_Taipei_Census_106 <- New_Taipei_Census_106 %>% 
  mutate(Population = 男 + 女)

colnames(New_Taipei_Census_106)[1] <- "id"
New_Taipei_Census_106_2 <- full_join(New_Taipei_Census_106, Taipei_Data_1, by = "id")

New_Taipei_City_Population_Plot <- ggplot() +
  geom_polygon(data = New_Taipei_Census_106_2, 
               aes(x = long, y = lat, 
                   group = group, 
                   fill = Population), 
               color = "black", 
               size = 0.25) + 
  scale_fill_gradientn(colours = brewer.pal(9,"Reds"))+
  theme_void()+
  labs(title="New_Taipei_City_Population_Plot")

##新北市廟宇密度 新北市資料開放平台

New_Taipei_City_Temple <- read_csv("New_Taipei_City_Temple.csv", 
                                   locale = locale(encoding = "BIG5"))

New_Taipei_City_Temple_Num <- New_Taipei_City_Temple %>%
  group_by(id) %>%
  summarise(Temple_Number = n())

New_Taipei_City_Temple_Num <- New_Taipei_City_Temple_Num[-30,]


New_Taipei_City_Temple <- full_join(New_Taipei_City_Temple_Num, Taipei_Data_1, by = "id")


New_Taipei_City_Temple_Plot <- ggplot() +
  geom_polygon(data = New_Taipei_City_Temple, 
               aes(x = long, y = lat, 
                   group = group, 
                   fill = Temple_Number), 
               color = "black", 
               size = 0.25) + 
  scale_fill_gradientn(colours = brewer.pal(9,"Reds"))+
  theme_void()+
  labs(title="New_Taipei_City_Temple_Plot")


##資料探勘

New_Taipei_City_Crim_2
New_Taipei_City_NSchool
New_Taipei_Census_106
New_Taipei_City_Temple_Num

New_Taipei_Data <- full_join(New_Taipei_City_Crim_2, New_Taipei_City_NSchool, by = "id")
New_Taipei_Data <- full_join(New_Taipei_Data, New_Taipei_Census_106, by = "id")
New_Taipei_Data <- full_join(New_Taipei_Data, New_Taipei_City_Temple_Num, by = "id")
New_Taipei_Data <- New_Taipei_Data[,-c(4:8)]

mode_1 <- glm(Crim_Number ~ School_Number + Population + Temple_Number,
              data = New_Taipei_Data)
sum<-summary(mode_1)
sum$coefficients







