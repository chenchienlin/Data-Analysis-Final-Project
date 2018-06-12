Final
================

``` r
library(readr)
```

    ## Warning: package 'readr' was built under R version 3.4.4

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following object is masked from 'package:base':
    ## 
    ##     date

``` r
library(maptools)
```

    ## Warning: package 'maptools' was built under R version 3.4.4

    ## Loading required package: sp

    ## Warning: package 'sp' was built under R version 3.4.4

    ## Checking rgeos availability: TRUE

``` r
library(rgdal)
```

    ## Warning: package 'rgdal' was built under R version 3.4.4

    ## rgdal: version: 1.2-20, (SVN revision 725)
    ##  Geospatial Data Abstraction Library extensions to R successfully loaded
    ##  Loaded GDAL runtime: GDAL 2.2.3, released 2017/11/20
    ##  Path to GDAL shared files: C:/Users/Jack/Documents/R/win-library/3.4/rgdal/gdal
    ##  GDAL binary built with GEOS: TRUE 
    ##  Loaded PROJ.4 runtime: Rel. 4.9.3, 15 August 2016, [PJ_VERSION: 493]
    ##  Path to PROJ.4 shared files: C:/Users/Jack/Documents/R/win-library/3.4/rgdal/proj
    ##  Linking to sp version: 1.2-7

``` r
library(rgeos) 
```

    ## Warning: package 'rgeos' was built under R version 3.4.4

    ## rgeos version: 0.3-26, (SVN revision 560)
    ##  GEOS runtime version: 3.6.1-CAPI-1.10.1 r0 
    ##  Linking to sp version: 1.2-7 
    ##  Polygon checking: TRUE

``` r
library(choroplethr)
```

    ## Warning: package 'choroplethr' was built under R version 3.4.4

    ## Loading required package: acs

    ## Warning: package 'acs' was built under R version 3.4.4

    ## Loading required package: stringr

    ## Loading required package: XML

    ## 
    ## Attaching package: 'acs'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     combine

    ## The following object is masked from 'package:base':
    ## 
    ##     apply

``` r
library(choroplethrMaps)
```

    ## Warning: package 'choroplethrMaps' was built under R version 3.4.4

``` r
library(RColorBrewer)
library(plotly)
```

    ## Warning: package 'plotly' was built under R version 3.4.4

    ## Loading required package: ggplot2

    ## Warning: package 'ggplot2' was built under R version 3.4.4

    ## 
    ## Attaching package: 'plotly'

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     last_plot

    ## The following object is masked from 'package:stats':
    ## 
    ##     filter

    ## The following object is masked from 'package:graphics':
    ## 
    ##     layout

``` r
library(ggplot2)
Taiwan_Crim_01_03 <- read_csv("Crim_10601_10603.csv", locale = locale(encoding = "BIG5"))
```

    ## Parsed with column specification:
    ## cols(
    ##   案類 = col_character(),
    ##   發生日期 = col_integer(),
    ##   發生地點 = col_character()
    ## )

``` r
Taiwan_Crim_04_06 <- read_csv("Crim_10604_10606.csv", locale = locale(encoding = "BIG5"))
```

    ## Parsed with column specification:
    ## cols(
    ##   案類 = col_character(),
    ##   發生日期 = col_integer(),
    ##   發生地點 = col_character()
    ## )

``` r
Taiwan_Crim_07_09 <- read_csv("Crim_10607_10609.csv", locale = locale(encoding = "BIG5"))
```

    ## Parsed with column specification:
    ## cols(
    ##   案類 = col_character(),
    ##   發生日期 = col_integer(),
    ##   發生地點 = col_character()
    ## )

``` r
Taiwan_Crim_10_12 <- read_csv("Crim_10610_10612.csv", locale = locale(encoding = "BIG5"))
```

    ## Parsed with column specification:
    ## cols(
    ##   案類 = col_character(),
    ##   發生日期 = col_integer(),
    ##   發生地點 = col_character()
    ## )

``` r
New_Taipei_Region <- readShapeSpatial("新北市區界")
```

    ## Warning: use rgdal::readOGR or sf::st_read

    ## Warning: use rgdal::readOGR or sf::st_read

``` r1
Taiwan_crim_2017 <- Reduce(function(...) merge(..., all=TRUE), list(Taiwan_Crim_01_03, Taiwan_Crim_04_06, Taiwan_Crim_07_09, Taiwan_Crim_10_12))
Taiwan_crim_2017$發生日期 <- substr(Taiwan_crim_2017$發生日期, start = 4, stop = 7)
Taiwan_crim_2017$發生日期 <- paste0("2017", Taiwan_crim_2017$發生日期)
Taiwan_crim_2017$發生日期 <- ymd(Taiwan_crim_2017$發生日期)
Taiwan_crim_2017 <- filter(Taiwan_crim_2017, 發生地點 != "外國")
Taiwan_crim_2017$縣市 <- substr(Taiwan_crim_2017$發生地點, start = 1, stop = 3)
Taiwan_crim_2017$區域別 <- Taiwan_crim_2017$發生地點
```

``` r2
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
```

``` r3
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


New_Taipei_City_Crim_Plot
```
