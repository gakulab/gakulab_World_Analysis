¥123567890---
title: "World_Data_PortofolioVer1.0"
author: '@Kohsuke'
date: "5/27/2020"
output: html_document
---



# Section 1: Code Memo & Log

## (a) Reference:General
- <The tidyverse style guide by Hadly Wickam> [https://style.tidyverse.org/]
-　<探索的データ分析> [https://uribo.github.io/practical-ds/01/eda.html]

## (b) Reference:Section Specific 
<!-- Example 1 　- [Section ...  ]　××××××　 -->　
<!-- Example 2 　- [Section ×　Portfolio Simulation  ]　××××××　 -->　



## (c) Remarks/注意事項
<!-- Example 1 　** ** bold and Section number + reasons 　 -->　
###**Do NOT run these sections as default: defaultでは走らせないセクション:**
- saveRDSを無効化
 
## (d) Code log　/コードメモ
<!-- Example 1 　- [13 May 2020:gaku]石川レポートの初期版提出後にコード整理のために学志がVer.8.0を作成　 -->　

[29 May 2020: 5:59AM gaku] Add country profiles code from UN : https://unstats.un.org/unsd/methodology/m49/overview/
http://data.un.org/Explorer.aspx

- 


# 1 準備：
## 1-0 概要
-オリジナルデータから.rdsファイルの作成
-

## Library読み込む

```r
# 現在の環境にある変数の消去
rm(list = ls(all=TRUE))
#解析処理に関するセットアップ、パッケージの読み込みなど
library(tidyverse)
```

```
## ─ Attaching packages ────────────────────────────────────────────────── tidyverse 1.3.0 ─
```

```
## ✓ ggplot2 3.3.0     ✓ purrr   0.3.4
## ✓ tibble  3.0.1     ✓ dplyr   0.8.5
## ✓ tidyr   1.0.3     ✓ stringr 1.4.0
## ✓ readr   1.3.1     ✓ forcats 0.5.0
```

```
## ─ Conflicts ─────────────────────────────────────────────────── tidyverse_conflicts() ─
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()
```

```r
library(dplyr)
library(ggsci)
library(gdata)
```

```
## gdata: read.xls support for 'XLS' (Excel 97-2004) files ENABLED.
```

```
## 
```

```
## gdata: read.xls support for 'XLSX' (Excel 2007+) files ENABLED.
```

```
## 
## Attaching package: 'gdata'
```

```
## The following objects are masked from 'package:dplyr':
## 
##     combine, first, last
```

```
## The following object is masked from 'package:purrr':
## 
##     keep
```

```
## The following object is masked from 'package:stats':
## 
##     nobs
```

```
## The following object is masked from 'package:utils':
## 
##     object.size
```

```
## The following object is masked from 'package:base':
## 
##     startsWith
```

```r
library(DT)
library(xtable)
library(openxlsx)
library(ggrepel)
library(formattable)
```

```
## 
## Attaching package: 'formattable'
```

```
## The following object is masked from 'package:xtable':
## 
##     digits
```

```r
library(gridExtra)
```

```
## 
## Attaching package: 'gridExtra'
```

```
## The following object is masked from 'package:gdata':
## 
##     combine
```

```
## The following object is masked from 'package:dplyr':
## 
##     combine
```

```r
library(stringr)
```



## オリジナルデータの読み込み

```r
World_Data <- read.csv("Working_Data/World/World_Fisheries_Data.csv",
                       header = TRUE,fileEncoding = "CP932")

Country_List <- read.csv("Working_Data/World/Country_Name_29May20_gaku.csv",
                       header = TRUE,fileEncoding = "CP932")

UN_Country_List <- read.csv("Working_Data/UN_Country_List_koh_29May20.csv",
                       header = TRUE,fileEncoding = "CP932")

#Ver1.3においてcountryの修正を行ったデータでUN_Country_Listを作成して読み込んだ
```



#coutry_ListとWorld_Dataの結合

```r
Country_List_Int <- left_join(Country_List,UN_Country_List, by="Country.or.Area")

#重複している列の削除
Country_List_Int <- Country_List_Int[c(-5,-6)]

#World_Dataと結合させるために列名の変更列名の変更
names(Country_List_Int) <- c( "FishingEntityID","FishingEntityName","Country.or.Area","X","Region.Code",
                              "Region.Name","Region.Name_koh","Sub.region.Code","Sub.region.Name","Sub.region.Name_koh",
                              "Intermediate.Region.Code","Intermediate.Region.Name","M49.Code","ISO.alpha3.Code",
                              "Least.Developed.Countries..LDC.","Land.Locked.Developing.Countries..LLDC.",
                              "Small.Island.Developing.States..SIDS.","Developed...Developing.Countries" )



World_Data <- left_join(World_Data,Country_List_Int,by = "FishingEntityID")
```


UN_Country_Listにおいて"Region.Name_koh"と"Sub.region.Name_koh"がある。
これは、Region.NameとSub.region.Nameを個人では判断しにくいため、新たな列を自分で付け足した。
これにより、暫定的にだが、国ごとではなく、地域ごとの多様性指数の算出などが可能になる
UN_Country_Listにおける"Region.Name_koh"と"Sub.region.Name_koh"では自分ではどの地域に属するのかわからないFishingEntityNameがいくつかあったため、そこはNAのままとなっている。
誰か有識者に判断してほしい。


#データクリーニング

```r
# Sea Around US 価格の統合
World_Data <- World_Data %>% 
  mutate(all_price = (Price.DHC + Price.Reduc + WBCAvePrice))



##データクリーニング　Catchの０以下を除去
World_Data <- World_Data %>% 
  filter(!TotalCatch_Tonnes <= 0)

#FishingEntityIDとTaxonKeyをfactorにへんかん
World_Data$FishingEntityID <- as.factor(World_Data$FishingEntityID)
World_Data$TaxonKey <- as.factor(World_Data$TaxonKey)



#saveRDS(World_Data,file = "World_Data_cleaned_31May20_koh.rds")
```

オリジナルデータ：781,060
クリーニングデータ：672099

## データ数

```r
World_Data <- readRDS("Working_Data/World/World_Data_cleaned_31May20_koh.rds")


options(scipen=100000)　#指数表示を避けるコード

# クロス集計をおこないデータの概観の把握
# xtabs関数でモデル式で多次元分割表を作成
# https://teramonagi.hatenablog.com/entry/20150312/1426109245


#魚種ごとのデータ数
Cross_Table_0 <- World_Data %>%
  xtabs(~TaxonKey, .)
Cross_Table_0 <- as.data.frame(Cross_Table_0)
datatable(Cross_Table_0)
```

preserve90dc3a3189b4653d

```r
#年毎の各魚種のデータ数
Cross_Table_1 <- World_Data %>%
  xtabs(~TaxonKey+Year, .)
Cross_Table_1 <- as.data.frame(Cross_Table_1)
datatable(Cross_Table_1)
```

```
## Warning in instance$preRenderHook(instance): It seems your data is too big
## for client-side DataTables. You may consider server-side processing: https://
## rstudio.github.io/DT/server.html
```

preserve0ce0cddcefc60e71

```r
#年別のデータ数
Cross_Table_2 <- World_Data %>%
  xtabs(~Year, .) 
Cross_Table_2 <- as.data.frame(Cross_Table_2)
datatable(Cross_Table_2)
```

preservef2ee4d27822ad8ee

```r
#国別のデータ数
Cross_Table_4 <- World_Data %>%
  xtabs(~FishingEntityName, .) 
Cross_Table_4 <- as.data.frame(Cross_Table_4)
datatable(Cross_Table_4)
```

preserve05462d5861318ed6

魚種：1861
年　：1950~2010
国　：197


##  各国の漁獲割合

```r
country_Rate <- World_Data %>% 
  group_by(FishingEntityName ,Year) %>% 
  summarise(Annual_TotalCatch_Tonnes = sum(TotalCatch_Tonnes),
            Annual_all_price = sum(all_price)) %>%
    mutate(Percentage_Catch = (Annual_TotalCatch_Tonnes/sum(Annual_TotalCatch_Tonnes))*100,
           Percentage_Value = (Annual_all_price/sum(Annual_all_price))*100) %>% 
  arrange(Year)

g1 <- ggplot(country_Rate) +
  aes(x = Year,y =  Percentage_Catch,fill = FishingEntityName)+
  geom_bar(stat = "identity",position = "fill") +
  theme_gray(base_size = 15,base_family = "HiraKakuProN-W3") +
  labs(x = "Year",y = "Percentage_Catch") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_continuous(breaks = seq(1950,2010,5)) +
  scale_fill_hue(name = "country") 
g1
```

<img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-6-1.png" width="672" />

```r
g2 <- ggplot(country_Rate) +
  aes(x = Year,y =  Percentage_Value,fill = FishingEntityName)+
  geom_bar(stat = "identity",position = "fill") +
  theme_gray(base_size = 15,base_family = "HiraKakuProN-W3") +
  labs(x = "Year",y = "Percentage_Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_continuous(breaks = seq(1950,2010,5)) +
  scale_fill_hue(name = "country") 
g2
```

<img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-6-2.png" width="672" />


## 年毎のデータ頻度

```r
Yearly_Data = Cross_Table_2 



ggplot(Yearly_Data) +
  theme_light(base_size = 11,base_family = "HiraKakuProN-W3") +
  aes(x = Year,y = Freq,fill = Year) +
  geom_bar(stat = "identity") +
  labs(x = "Year",y = "data_number") +
  guides(fill = FALSE) +
  scale_x_discrete(breaks = seq(1950,2010,5)) +
  scale_y_continuous(breaks = seq(0,15000,3000)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  
```

<img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-7-1.png" width="672" />

```r
#年ごとのデータ数
```



## 各年の漁獲量・漁獲高

```r
source("World_Function.R")

#各年の漁獲量・漁獲高の推移
World_Catch_Value_Yearly <-fun_Catch_Value(World_Data,TotalCatch_Tonnes,all_price)
```

```
## Warning: group_by_() is deprecated. 
## Please use group_by() instead
## 
## The 'programming' vignette or the tidyeval book can help you
## to program with group_by() : https://tidyeval.tidyverse.org
## This warning is displayed once per session.
```

```r
g3 <- ggplot(World_Catch_Value_Yearly) +
  aes(x = Year,y = TotalCatch_Tonnes,fill = Year) +
  geom_bar(stat = "identity") +
  theme_light(base_size = 15,base_family = "HiraKakuProN-W3") +
  labs(x = "Year",y = "Total_Catch(ton)）") +
  guides(fill = FALSE) +
  scale_x_continuous(breaks = seq(1950,2010,5)) +
  scale_y_continuous(breaks = seq(0,120000000,20000000)) +
  expand_limits(y = 0) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
g3
```

<img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-8-1.png" width="672" />

```r
g4 <- ggplot(World_Catch_Value_Yearly) +
  aes(x = Year,y = all_price,fill = Year) +
  geom_bar(stat = "identity") +
  theme_light(base_size = 15,base_family = "HiraKakuProN-W3") +
  labs(x = "Year",y = "Total_Value(USD)") +
  guides(fill = FALSE) +
  scale_x_continuous(breaks = seq(1950,2010,5)) +
  scale_y_continuous(breaks = seq(0,50000000,10000000)) +
  expand_limits(y = 0) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
g4
```

<img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-8-2.png" width="672" />




## HHI 

```r
source("World_Function.R")

#年ごとのHHI

World_HHI_all_Yearly <- fun_HHI(World_Data,TotalCatch_Tonnes,all_price)
  
#列を見やすいように並び替える
World_HHI_all_Yearly <- World_HHI_all_Yearly[c(1,8,2,3,4,5,6,7)]
  
World_HHI_all_Yearly$Year <- as.integer(World_HHI_all_Yearly$Year)

EffectA=1.5

g5 <- ggplot(World_HHI_all_Yearly, 
             aes(x = HHI_Catch_Total,y = HHI_Value_Total,color = Year, label=(Year)) )+
  theme(text = element_text(family = "HiraKakuProN-W3"),panel.background=element_rect(fill="gray81"))+
  geom_point(aes(size = TotalCatch_Tonnes),alpha=.5) +
  scale_colour_gradient(name="年",low = "red", high = "blue",
                        breaks = seq(1950, 2010,by=5),
                        labels = seq(1950, 2010,by=5)) +
  geom_abline(intercept = 0,slope = 1,linetype = "dotted") +
  labs(x = "Catch_HHI",y = "Value_HHI",size="Total_Catch(ton)") +
  scale_x_continuous(limits = c(0,700)) +
  scale_y_continuous(limits = c(0,50))+
  theme( axis.title = element_text(size=15, face="bold"),
       axis.text= element_text(size=12, face="bold"),
       plot.title = element_text(size=15, face="bold"),
       legend.title = element_text(size=12, face="bold"),
       legend.text=element_text(face="bold"))+
  scale_radius(range=c(15/EffectA,20/EffectA))+ #  adjust scale effects 
  geom_text_repel(colour="black")
g5
```

<img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-9-1.png" width="672" />



## シンプソン指数λ

```r
source("World_Function.R")

#年ごとのλ
World_Simpson_all_Yearly <- fun_Simpson(World_Data,TotalCatch_Tonnes,all_price)

World_Simpson_all_Yearly <- World_Simpson_all_Yearly[c(1,8,2,3,4,5,6,7)]

World_Simpson_all_Yearly$Year <- as.integer(World_Simpson_all_Yearly$Year)

EffectA=1.5

g6 <- ggplot(World_Simpson_all_Yearly,
             aes(x = Simpson_Catch_Total,y = Simpson_Value_Total,color = Year, label=(Year)) )+
  theme(text = element_text(family = "HiraKakuProN-W3"),panel.background=element_rect(fill="gray81"))+
  geom_point(aes(size = TotalCatch_Tonnes),alpha=.5) +
  scale_colour_gradient(name="年",low = "red", high = "blue",
                        breaks = seq(1950, 2010,by=5),
                        labels = seq(1950, 2010,by=5)) +
  geom_abline(intercept = 0,slope = 1,linetype = "dotted") +
  labs(x = "Catch_λ",y = "Value_λ",size="Total_Catch(ton)") +
  scale_x_continuous(limits = c(0,70)) +
  scale_y_continuous(limits = c(0,400))+
  theme( axis.title = element_text(size=15, face="bold"),
       axis.text= element_text(size=12, face="bold"),
       plot.title = element_text(size=15, face="bold"),
       legend.title = element_text(size=12, face="bold"),
       legend.text=element_text(face="bold"))+
  scale_radius(range=c(15/EffectA,20/EffectA))+ #  adjust scale effects 
  geom_text_repel(colour="black")

g6
```

<img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-10-1.png" width="672" />



## シャノン・ウィナー指数H'

```r
source("World_Function.R")

#年ごとのH'
World_Shannon_all_Yearly = fun_Shannon(World_Data,TotalCatch_Tonnes,all_price = )

World_Shannon_all_Yearly <- World_Shannon_all_Yearly[c(1,8,2,3,4,5,6,7)]

World_Shannon_all_Yearly$Year <- as.integer(World_Shannon_all_Yearly$Year)

EffectA=1.5

g7 <- ggplot(World_Shannon_all_Yearly,
             aes(x = Shannon_Catch_Total,y = Shannon_Value_Total,color = Year, label=(Year)) )+
  theme(text = element_text(family = "HiraKakuProN-W3"),panel.background=element_rect(fill="gray81"))+
  geom_point(aes(size = TotalCatch_Tonnes),alpha=.5) +
  scale_colour_gradient(name="年",low = "red", high = "blue",
                        breaks = seq(1950, 2010,by=5),
                        labels = seq(1950, 2010,by=5)) +
  geom_abline(intercept = 0,slope = 1,linetype = "dotted") +
  labs(x = "Catch_H'",y = "Value_H'",size="Total_Catch(ton)") +
  scale_x_continuous(limits = c(6,7.5)) +
  scale_y_continuous(limits = c(8,10))+
  theme( axis.title = element_text(size=15, face="bold"),
       axis.text= element_text(size=12, face="bold"),
       plot.title = element_text(size=15, face="bold"),
       legend.title = element_text(size=12, face="bold"),
       legend.text=element_text(face="bold"))+
  scale_radius(range=c(15/EffectA,20/EffectA))+ #  adjust scale effects 
  geom_text_repel(colour="black")

g7
```

<img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-11-1.png" width="672" />









#2 各国
## HHI

```r
source("World_Function.R")

HHI_country_Data <- fun_HHI3(World_Data,TotalCatch_Tonnes,all_price)


HHI_country_Summary <- HHI_country_Data
  

#FishingEntityNameとNumberを紐付ける
#FishingEntityIDは連続した数字ではないために分析に使いにくい
HHI_rate <- as.data.frame(unique(HHI_country_Data$FishingEntityName))
  
HHI_rate <- HHI_rate %>% 
    mutate(Number = as.factor(1:197))
  
#列名の変更
names(HHI_rate) <- c("FishingEntityName","Number")

#HHI_country_DataとFishingEntityNameで組み合わせる
#FishingEntityNameに対応したNumberが付与され分析が容易になる
HHI_country_Data <-full_join(HHI_country_Data,HHI_rate,by = "FishingEntityName")
```

```
## Warning: Column `FishingEntityName` joining character vector and factor,
## coercing into character vector
```



## HHIのプロット1

```r
#FishingEntityNameの要素の長さを変数にする
for (i in 1:length(unique(HHI_rate$FishingEntityName))) {
 # i = 1
  
#HHI_country_DataからFishingEntityNameに対応した連続した値であるNumberでfilterをかける
HHI_loop <- HHI_country_Data %>% 
  filter(Number == i)

#各国におけるHHI_Total_Catch,HHI_Total_Valueの最小と最大を格納する
HHI_Max <- max(HHI_loop$HHI_Catch_Total,HHI_loop$HHI_Value_Total)
HHI_Min <- min(HHI_loop$HHI_Catch_Total,HHI_loop$HHI_Value_Total)


EffectA=1.5

g8 <- ggplot(HHI_loop) +
  aes(x = HHI_Catch_Total,y = HHI_Value_Total,color = Year, label=(Year))+
  theme(text = element_text(family = "HiraKakuProN-W3"),panel.background=element_rect(fill="gray81"))+
  geom_point(aes(size = TotalCatch_Tonnes),alpha=.5) +
  scale_colour_gradient(name="年",low = "red", high = "blue",
                        breaks = seq(1950, 2010,by=5),
                        labels = seq(1950, 2010,by=5)) +
  geom_abline(intercept = 0,slope = 1,linetype = "dotted") +
  labs(x = "Catch_HHI",y = "Value_HHI",size="Total_Catch(ton)",
       title = HHI_rate$FishingEntityName[i]) +　#タイトルはFishingEntityNameなので国を表す
  scale_x_continuous(limits = c(HHI_Min,HHI_Max)) +　　　　　　　　　　　
  scale_y_continuous(limits = c(HHI_Min,HHI_Max)) +
  theme( axis.title = element_text(size=15, face="bold"),
       axis.text= element_text(size=12, face="bold"),
       plot.title = element_text(size=15, face="bold"),
       legend.title = element_text(size=12, face="bold"),
       legend.text=element_text(face="bold"))+
  scale_radius(range=c(15/EffectA,20/EffectA))+ #  adjust scale effects 
  geom_text_repel(colour="black")

print(g8)

}
```

<img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-1.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-2.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-3.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-4.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-5.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-6.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-7.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-8.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-9.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-10.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-11.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-12.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-13.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-14.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-15.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-16.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-17.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-18.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-19.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-20.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-21.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-22.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-23.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-24.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-25.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-26.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-27.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-28.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-29.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-30.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-31.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-32.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-33.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-34.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-35.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-36.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-37.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-38.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-39.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-40.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-41.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-42.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-43.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-44.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-45.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-46.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-47.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-48.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-49.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-50.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-51.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-52.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-53.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-54.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-55.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-56.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-57.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-58.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-59.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-60.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-61.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-62.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-63.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-64.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-65.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-66.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-67.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-68.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-69.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-70.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-71.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-72.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-73.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-74.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-75.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-76.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-77.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-78.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-79.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-80.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-81.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-82.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-83.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-84.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-85.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-86.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-87.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-88.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-89.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-90.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-91.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-92.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-93.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-94.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-95.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-96.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-97.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-98.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-99.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-100.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-101.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-102.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-103.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-104.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-105.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-106.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-107.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-108.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-109.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-110.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-111.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-112.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-113.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-114.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-115.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-116.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-117.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-118.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-119.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-120.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-121.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-122.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-123.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-124.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-125.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-126.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-127.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-128.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-129.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-130.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-131.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-132.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-133.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-134.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-135.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-136.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-137.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-138.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-139.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-140.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-141.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-142.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-143.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-144.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-145.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-146.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-147.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-148.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-149.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-150.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-151.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-152.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-153.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-154.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-155.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-156.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-157.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-158.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-159.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-160.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-161.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-162.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-163.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-164.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-165.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-166.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-167.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-168.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-169.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-170.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-171.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-172.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-173.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-174.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-175.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-176.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-177.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-178.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-179.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-180.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-181.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-182.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-183.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-184.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-185.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-186.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-187.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-188.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-189.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-190.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-191.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-192.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-193.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-194.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-195.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-196.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-13-197.png" width="672" />





## HHIoのプロット2

```r
HHI_Max <- max(HHI_country_Data$HHI_Catch_Total,HHI_country_Data$HHI_Value_Total)
HHI_Min <- min(HHI_country_Data$HHI_Catch_Total,HHI_country_Data$HHI_Value_Total)


EffectA=1.5

p_1 <- ggplot(HHI_country_Data) +
  aes(x = HHI_Catch_Total,y = HHI_Value_Total,color = Year) +
  theme(text = element_text(family = "HiraKakuProN-W3"),panel.background=element_rect(fill="gray81"))+
  geom_point(aes(size = TotalCatch_Tonnes),alpha=.5) +
  scale_colour_gradient(name="年",low = "red", high = "blue",
                        breaks = seq(1950, 2010,by=5),
                        labels = seq(1950, 2010,by=5)) +
  geom_abline(intercept = 0,slope = 1,linetype = "dotted") +
  facet_wrap(~ FishingEntityName)　+
  labs(x = "Catch_HHI",y = "Value_HHI",size="Total_Catch(ton)") +　
  scale_x_continuous(limits = c(HHI_Min,HHI_Max)) +　　　　　　　　　　　
  scale_y_continuous(limits = c(HHI_Min,HHI_Max)) +
  theme( axis.title = element_text(size=15, face="bold"),
       axis.text= element_text(size=12, face="bold"),
       axis.text.x = element_text(angle = 45, hjust = 1),
       plot.title = element_text(size=15, face="bold"),
       legend.title = element_text(size=12, face="bold"),
       legend.text=element_text(face="bold"))+
  scale_radius(range=c(5/EffectA,10/EffectA))


print(p_1)
```

<img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-14-1.png" width="672" />




## Simpson

```r
source("World_Function.R")
Simpson_country_Data <- fun_Simpson2(World_Data,TotalCatch_Tonnes,all_price)

#FishingEntityNameとNumberを紐付ける
#FishingEntityIDは連続した数字ではないために分析に使いにくい
Simpson_rate <- as.data.frame(unique(Simpson_country_Data$FishingEntityName))
  
  Simpson_rate <- Simpson_rate %>% 
    mutate(Number = as.factor(1:197))
  
#列名の変更  
names(Simpson_rate) <- c("FishingEntityName","Number")

#Simpson_country_DataとFishingEntityNameで組み合わせる
#FishingEntityNameに対応したNumberが付与され分析が容易になる
Simpson_country_Data <-full_join(Simpson_country_Data,Simpson_rate,by = "FishingEntityName")
```

```
## Warning: Column `FishingEntityName` joining character vector and factor,
## coercing into character vector
```


## Simpsonのプロット

```r
#FishingEntityNameの要素の長さを変数にする
for (i in 1:length(unique(Simpson_rate$FishingEntityName))) {

  #Simpson_country_DataからFishingEntityNameに対応した連続した値であるNumberでfilterをかける
Simpson_loop <- Simpson_country_Data %>% 
  filter(Number == i)

#各国におけるSimpson_Catch_Total,Simpson_Value_Totalの最小と最大を格納する
Simpson_Max <- max(Simpson_loop$Simpson_Catch_Total,Simpson_loop$Simpson_Value_Total)
Simpson_Min <- min(Simpson_loop$Simpson_Catch_Total,Simpson_loop$Simpson_Value_Total)


EffectA=1.5

g9 <- ggplot(Simpson_loop) +
  aes(x = Simpson_Catch_Total,y = Simpson_Value_Total,color = Year, label=(Year))+
  theme(text = element_text(family = "HiraKakuProN-W3"),panel.background=element_rect(fill="gray81"))+
  geom_point(aes(size = TotalCatch_Tonnes),alpha=.5) +
  scale_colour_gradient(name="年",low = "red", high = "blue",
                        breaks = seq(1950, 2010,by=5),
                        labels = seq(1950, 2010,by=5)) +
  geom_abline(intercept = 0,slope = 1,linetype = "dotted") +
  labs(x = "Catch_λ",y = "Value_λ",size="Total_Catch(ton)",
       title = Simpson_rate$FishingEntityID[i]) +   #タイトルはFishingEntityNameなので国を表す
  scale_x_continuous(limits = c(Simpson_Min,Simpson_Max)) +
  scale_y_continuous(limits = c(Simpson_Min,Simpson_Max)) +
  theme( axis.title = element_text(size=15, face="bold"),
       axis.text= element_text(size=12, face="bold"),
       plot.title = element_text(size=15, face="bold"),
       legend.title = element_text(size=12, face="bold"),
       legend.text=element_text(face="bold"))+
  scale_radius(range=c(15/EffectA,20/EffectA))+ #  adjust scale effects 
  geom_text_repel(colour="black")


print(g9)

}
```

<img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-1.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-2.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-3.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-4.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-5.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-6.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-7.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-8.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-9.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-10.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-11.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-12.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-13.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-14.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-15.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-16.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-17.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-18.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-19.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-20.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-21.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-22.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-23.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-24.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-25.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-26.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-27.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-28.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-29.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-30.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-31.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-32.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-33.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-34.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-35.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-36.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-37.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-38.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-39.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-40.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-41.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-42.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-43.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-44.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-45.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-46.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-47.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-48.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-49.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-50.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-51.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-52.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-53.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-54.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-55.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-56.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-57.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-58.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-59.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-60.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-61.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-62.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-63.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-64.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-65.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-66.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-67.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-68.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-69.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-70.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-71.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-72.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-73.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-74.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-75.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-76.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-77.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-78.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-79.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-80.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-81.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-82.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-83.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-84.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-85.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-86.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-87.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-88.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-89.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-90.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-91.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-92.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-93.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-94.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-95.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-96.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-97.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-98.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-99.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-100.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-101.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-102.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-103.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-104.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-105.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-106.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-107.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-108.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-109.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-110.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-111.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-112.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-113.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-114.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-115.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-116.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-117.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-118.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-119.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-120.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-121.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-122.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-123.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-124.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-125.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-126.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-127.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-128.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-129.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-130.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-131.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-132.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-133.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-134.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-135.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-136.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-137.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-138.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-139.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-140.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-141.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-142.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-143.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-144.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-145.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-146.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-147.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-148.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-149.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-150.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-151.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-152.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-153.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-154.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-155.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-156.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-157.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-158.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-159.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-160.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-161.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-162.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-163.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-164.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-165.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-166.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-167.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-168.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-169.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-170.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-171.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-172.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-173.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-174.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-175.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-176.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-177.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-178.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-179.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-180.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-181.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-182.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-183.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-184.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-185.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-186.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-187.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-188.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-189.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-190.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-191.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-192.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-193.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-194.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-195.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-196.png" width="672" /><img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-16-197.png" width="672" />



## Simpsonのぷろっと2

```r
Simpson_Max <- max(Simpson_country_Data$Simpson_Catch_Total,
                   Simpson_country_Data$Simpson_Value_Total)

Simpson_Min <- min(Simpson_country_Data$Simpson_Catch_Total,
                   Simpson_country_Data$Simpson_Value_Total)

EffectA=1.5

p_2 <- ggplot(Simpson_country_Data) +
  aes(x = Simpson_Catch_Total,y = Simpson_Value_Total,color = Year, label=(FishingEntityName))+
  theme(text = element_text(family = "HiraKakuProN-W3"),panel.background=element_rect(fill="gray81"))+
  geom_point(aes(size = TotalCatch_Tonnes),alpha=.5) +
  scale_colour_gradient(name="年",low = "red", high = "blue",
                        breaks = seq(1950, 2010,by=5),
                        labels = seq(1950, 2010,by=5)) +
  geom_abline(intercept = 0,slope = 1,linetype = "dotted") +
  facet_wrap(~ FishingEntityName)　+
  labs(x = "Catch_λ",y = "Value_λ",size="Total_Catch(ton)") +   
 #scale_x_continuous(limits = c(Simpson_Min,Simpson_Max)) +
  #scale_y_continuous(limits = c(Simpson_Min,Simpson_Max)) +
  theme( axis.title = element_text(size=15, face="bold"),
       axis.text= element_text(size=12, face="bold"),
       plot.title = element_text(size=15, face="bold"),
       legend.title = element_text(size=12, face="bold"),
       legend.text=element_text(face="bold"))+
  scale_radius(range=c(5/EffectA,10/EffectA))

print(p_2)
```

<img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-17-1.png" width="672" />





## Shannon

```r
source("World_Function.R")
Shannon_country_Data <- fun_Shannon2(World_Data,TotalCatch_Tonnes,all_price)

#FishingEntityNameとNumberを紐付ける
#FishingEntityIDは連続した数字ではないために分析に使いにくい
Shannon_rate <- as.data.frame(unique(Shannon_country_Data$FishingEntityName))
  
  Shannon_rate <- Shannon_rate %>% 
    mutate(Number = as.factor(1:197))
 
#列名の変更   
names(Shannon_rate) <- c("FishingEntityName","Number")

#Shannon_country_DataとFishingEntityNameで組み合わせる
#FishingEntityNameに対応したNumberが付与され分析が容易になる
Shannon_country_Data <-full_join(Shannon_country_Data,Shannon_rate,by = "FishingEntityName")
```

```
## Warning: Column `FishingEntityName` joining character vector and factor,
## coercing into character vector
```


## Shanonnのプロット

```r
#FishingEntityNameの要素の長さを変数にする
for (i in 1:length(unique(Shannon_rate$FishingEntityName))) {

 #Shannon_country_DataからFishingEntityNameに対応した連続した値であるNumberでfilterをかける
Shannon_loop <- Shannon_country_Data %>% 
  filter(Number == i)

#各国におけるShannon_Catch_Total,Shannon_Value_Totalの最小と最大を格納する
Shannon_Max <- max(Shannon_loop$Shannon_Catch_Total,Shannon_loop$Shannon_Value_Total)
Shannon_Min <- min(Shannon_loop$Shannon_Catch_Total,Shannon_loop$Shannon_Value_Total)


EffectA=1.5

g10 <- ggplot(Shannon_loop) +
  aes(x = Shannon_Catch_Total,y = Shannon_Value_Total,color = Year, label=(Year))+
  theme(text = element_text(family = "HiraKakuProN-W3"),panel.background=element_rect(fill="gray81"))+
  geom_point(aes(size = TotalCatch_Tonnes),alpha=.5) +
  scale_colour_gradient(name="年",low = "red", high = "blue",
                        breaks = seq(1950, 2010,by=5),
                        labels = seq(1950, 2010,by=5)) +
  geom_abline(intercept = 0,slope = 1,linetype = "dotted") +
  labs(x = "Catch_λ",y = "Value_λ",size="Total_Catch(ton)",
       title = Shannon_rate$FishingEntityID[i]) +  #タイトルはFishingEntityNameなので国を表す
  scale_x_continuous(limits = c(Shannon_Min,Shannon_Max)) +
  scale_y_continuous(limits = c(Shannon_Min,Shannon_Max)) +
  theme( axis.title = element_text(size=15, face="bold"),
       axis.text= element_text(size=12, face="bold"),
       plot.title = element_text(size=15, face="bold"),
       legend.title = element_text(size=12, face="bold"),
       legend.text=element_text(face="bold"))+
  scale_radius(range=c(15/EffectA,20/EffectA))+ #  adjust scale effects 
  geom_text_repel(colour="black")

print(10)

}
```

```
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
## [1] 10
```



## Shannonpプロット2

```r
Shannon_Max <- max(Shannon_country_Data$Shannon_Catch_Total,Shannon_country_Data$Shannon_Value_Total)
Shannon_Min <- min(Shannon_country_Data$Shannon_Catch_Total,Shannon_country_Data$Shannon_Value_Total)


EffectA=1.5

p_3 <- ggplot(Shannon_country_Data) +
  aes(x = Shannon_Catch_Total,y = Shannon_Value_Total,color = Year, label=(Year))+
  theme(text = element_text(family = "HiraKakuProN-W3"),panel.background=element_rect(fill="gray81"))+
  geom_point(aes(size = TotalCatch_Tonnes),alpha=.5) +
  scale_colour_gradient(name="年",low = "red", high = "blue",
                        breaks = seq(1950, 2010,by=5),
                        labels = seq(1950, 2010,by=5)) +
  geom_abline(intercept = 0,slope = 1,linetype = "dotted") +
   facet_wrap(~ FishingEntityName)　+
  labs(x = "Catch_λ",y = "Value_λ",size="Total_Catch(ton)") +  
 # scale_x_continuous(limits = c(Shannon_Min,Shannon_Max)) +
#  scale_y_continuous(limits = c(Shannon_Min,Shannon_Max)) +
  theme( axis.title = element_text(size=15, face="bold"),
       axis.text= element_text(size=12, face="bold"),
       plot.title = element_text(size=15, face="bold"),
       legend.title = element_text(size=12, face="bold"),
       legend.text=element_text(face="bold"))+
  scale_radius(range=c(15/EffectA,20/EffectA))

print(p_3)
```

<img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-20-1.png" width="672" />


# 3 各地域 
## HHI

```r
source("World_Function.R")

Rerion_HHI <- fun_HHI4(World_Data,TotalCatch_Tonnes,all_price)

HHI_Max <- max(Rerion_HHI$HHI_Catch_Total,Rerion_HHI$HHI_Value_Total)
HHI_Min <- min(Rerion_HHI$HHI_Catch_Total,Rerion_HHI$HHI_Value_Total)


EffectA=1.5

p_4 <- ggplot(Rerion_HHI) +
  aes(x = HHI_Catch_Total,y = HHI_Value_Total,color = Year) +
  theme(text = element_text(family = "HiraKakuProN-W3"),panel.background=element_rect(fill="gray81"))+
  geom_point(aes(size = TotalCatch_Tonnes),alpha=.5) +
  scale_colour_gradient(name="年",low = "red", high = "blue",
                        breaks = seq(1950, 2010,by=10),
                        labels = seq(1950, 2010,by=10)) +
  geom_abline(intercept = 0,slope = 1,linetype = "dotted") +
  facet_wrap(~ Region.Name_koh)　+
  labs(x = "Catch_HHI",y = "Value_HHI",size="Total_Catch(ton)") +　
  scale_x_continuous(limits = c(HHI_Min,HHI_Max)) +　　　　　　　　　　　
  scale_y_continuous(limits = c(HHI_Min,HHI_Max)) +
  theme( axis.title = element_text(size=15, face="bold"),
       axis.text= element_text(size=12, face="bold"),
       axis.text.x = element_text(angle = 45, hjust = 1),
       plot.title = element_text(size=15, face="bold"),
       legend.title = element_text(size=12, face="bold"),
       legend.text=element_text(face="bold"))+
  scale_radius(range=c(10/EffectA,20/EffectA))


print(p_4)
```

<img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-21-1.png" width="672" />

##Simpson

```r
source("World_Function.R")

Rerion_Simpson <- fun_Simpson3(World_Data,TotalCatch_Tonnes,all_price)

Simpson_Max <- max(Rerion_Simpson$Simpson_Catch_Total,
                   Rerion_Simpson$Simpson_Value_Total)

Simpson_Min <- min(Rerion_Simpson$Simpson_Catch_Total,
                   Rerion_Simpson$Simpson_Value_Total)


EffectA=1.5

p_5 <- ggplot(Rerion_Simpson) +
  aes(x = Simpson_Catch_Total,y = Simpson_Value_Total,color = Year)+
  theme(text = element_text(family = "HiraKakuProN-W3"),panel.background=element_rect(fill="gray81"))+
  geom_point(aes(size = TotalCatch_Tonnes),alpha=.5) +
  scale_colour_gradient(name="年",low = "red", high = "blue",
                        breaks = seq(1950, 2010,by=10),
                        labels = seq(1950, 2010,by=10)) +
  geom_abline(intercept = 0,slope = 1,linetype = "dotted") +
  facet_wrap(~ Region.Name_koh)　+
  labs(x = "Catch_λ",y = "Value_λ",size="Total_Catch(ton)") +   
 #scale_x_continuous(limits = c(Simpson_Min,Simpson_Max)) +
  #scale_y_continuous(limits = c(Simpson_Min,Simpson_Max)) +
  theme( axis.title = element_text(size=15, face="bold"),
       axis.text= element_text(size=12, face="bold"),
       plot.title = element_text(size=15, face="bold"),
       legend.title = element_text(size=12, face="bold"),
       legend.text=element_text(face="bold"))+
  scale_radius(range=c(10/EffectA,20/EffectA))

print(p_5)
```

<img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-22-1.png" width="672" />


##Shannon

```r
source("World_Function.R")

Rerion_Shannon <- fun_Shannon3(World_Data,TotalCatch_Tonnes,all_price)

Shannon_Max <- max(Rerion_Shannon$Simpson_Catch_Total,
                   Rerion_Shannon$Simpson_Value_Total)
```

```
## Warning: Unknown or uninitialised column: `Simpson_Catch_Total`.
```

```
## Warning: Unknown or uninitialised column: `Simpson_Value_Total`.
```

```
## Warning in max(Rerion_Shannon$Simpson_Catch_Total,
## Rerion_Shannon$Simpson_Value_Total): max の引数に有限な値がありません: -Inf を返
## します
```

```r
Shannon_Min <- min(Rerion_Shannon$Simpson_Catch_Total,
                   Rerion_Shannon$Simpson_Value_Total)
```

```
## Warning: Unknown or uninitialised column: `Simpson_Catch_Total`.
```

```
## Warning: Unknown or uninitialised column: `Simpson_Value_Total`.
```

```
## Warning in min(Rerion_Shannon$Simpson_Catch_Total,
## Rerion_Shannon$Simpson_Value_Total): min の引数に有限な値がありません: Inf を返
## します
```

```r
EffectA=1.5

p_6 <- ggplot(Rerion_Shannon) +
  aes(x = Shannon_Catch_Total,y = Shannon_Value_Total,color = Year)+
  theme(text = element_text(family = "HiraKakuProN-W3"),panel.background=element_rect(fill="gray81"))+
  geom_point(aes(size = TotalCatch_Tonnes),alpha=.5) +
  scale_colour_gradient(name="年",low = "red", high = "blue",
                        breaks = seq(1950, 2010,by=10),
                        labels = seq(1950, 2010,by=10)) +
  geom_abline(intercept = 0,slope = 1,linetype = "dotted") +
   facet_wrap(~ Region.Name_koh)　+
  labs(x = "Catch_λ",y = "Value_λ",size="Total_Catch(ton)") +  
 # scale_x_continuous(limits = c(Shannon_Min,Shannon_Max)) +
#  scale_y_continuous(limits = c(Shannon_Min,Shannon_Max)) +
  theme( axis.title = element_text(size=15, face="bold"),
       axis.text= element_text(size=12, face="bold"),
       plot.title = element_text(size=15, face="bold"),
       legend.title = element_text(size=12, face="bold"),
       legend.text=element_text(face="bold"))+
  scale_radius(range=c(10/EffectA,20/EffectA))

print(p_6)
```

<img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-23-1.png" width="672" />






# 4 各地域 (サブ)
## Southern Europe

```r
Sub_region_Name <- as.data.frame(unique(World_Data$Sub.region.Name_koh))

Southern_Europe <- World_Data %>% 
  filter(Sub.region.Name_koh == "Southern Europe")
```




### HHI

```r
source("World_Function.R")

Southern_Europe_HHI_Cou <- fun_HHI5(Southern_Europe,TotalCatch_Tonnes,all_price)

Southern_Europe_HHI_Ave <- fun_HHI6(Southern_Europe,TotalCatch_Tonnes,all_price)

#rbindするために列をそろえる、順番も変える
Southern_Europe_HHI_Ave <- Southern_Europe_HHI_Ave[-2]
Southern_Europe_HHI_Ave <- Southern_Europe_HHI_Ave[c(1,6,2,3,4,5)]

#結合する
Southern_Europe_HHI <- rbind(Southern_Europe_HHI_Cou,Southern_Europe_HHI_Ave)

HHI_Max <- max(Southern_Europe_HHI$HHI_Catch_Total,Southern_Europe_HHI$HHI_Value_Total)
HHI_Min <- min(Southern_Europe_HHI$HHI_Catch_Total,Southern_Europe_HHI$HHI_Value_Total)


EffectA=1.5

p_7 <- ggplot(Southern_Europe_HHI) +
  aes(x = HHI_Catch_Total,y = HHI_Value_Total,color = Year) +
  theme(text = element_text(family = "HiraKakuProN-W3"),panel.background=element_rect(fill="gray81"))+
  geom_point(aes(size = TotalCatch_Tonnes),alpha=.5) +
  scale_colour_gradient(name="年",low = "red", high = "blue",
                        breaks = seq(1950, 2010,by=10),
                        labels = seq(1950, 2010,by=10)) +
  geom_abline(intercept = 0,slope = 1,linetype = "dotted") +
  facet_wrap(~ Country.or.Area)　+
  labs(x = "Catch_HHI",y = "Value_HHI",size="Total_Catch(ton)") +　
  scale_x_continuous(limits = c(HHI_Min,HHI_Max)) +　　　　　　　　　　　
  scale_y_continuous(limits = c(HHI_Min,HHI_Max)) +
  theme( axis.title = element_text(size=15, face="bold"),
       axis.text= element_text(size=12, face="bold"),
       axis.text.x = element_text(angle = 45, hjust = 1),
       plot.title = element_text(size=15, face="bold"),
       legend.title = element_text(size=12, face="bold"),
       legend.text=element_text(face="bold"))+
  scale_radius(range=c(5/EffectA,10/EffectA))


print(p_7)
```

<img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-25-1.png" width="672" />


### Simpson

```r
source("World_Function.R")

Southern_Europ_Simpson_Cou <- fun_Simpson4(Southern_Europe,TotalCatch_Tonnes,all_price)

Southern_Europe_Simpson_Ave <- fun_Simpson5(Southern_Europe,TotalCatch_Tonnes,all_price)

#rbindするために列をそろえる、順番も変える
Southern_Europe_Simpson_Ave <- Southern_Europe_Simpson_Ave[-2]
Southern_Europe_Simpson_Ave <- Southern_Europe_Simpson_Ave[c(1,6,2,3,4,5)]

#結合する
Southern_Europe_Simpson <- rbind(Southern_Europ_Simpson_Cou,Southern_Europe_Simpson_Ave)

Simpson_Max <- max(Southern_Europe_Simpson$Simpson_Catch_Total,
                   Southern_Europe_Simpson$Simpson_Value_Total)

Simpson_Min <- min(Southern_Europe_Simpson$Simpson_Catch_Total,
                   Southern_Europe_Simpson$Simpson_Value_Total)


EffectA=1.5

p_8 <- ggplot(Southern_Europe_Simpson) +
  aes(x = Simpson_Catch_Total,y = Simpson_Value_Total,color = Year)+
  theme(text = element_text(family = "HiraKakuProN-W3"),panel.background=element_rect(fill="gray81"))+
  geom_point(aes(size = TotalCatch_Tonnes),alpha=.5) +
  scale_colour_gradient(name="年",low = "red", high = "blue",
                        breaks = seq(1950, 2010,by=10),
                        labels = seq(1950, 2010,by=10)) +
  geom_abline(intercept = 0,slope = 1,linetype = "dotted") +
  facet_wrap(~ Country.or.Area)　+
  labs(x = "Catch_λ",y = "Value_λ",size="Total_Catch(ton)") +   
 scale_x_continuous(limits = c(Simpson_Min,Simpson_Max)) +
  scale_y_continuous(limits = c(Simpson_Min,Simpson_Max)) +
  theme( axis.title = element_text(size=15, face="bold"),
       axis.text= element_text(size=12, face="bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
       plot.title = element_text(size=15, face="bold"),
       legend.title = element_text(size=12, face="bold"),
       legend.text=element_text(face="bold"))+
  scale_radius(range=c(5/EffectA,10/EffectA))

print(p_8)
```

<img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-26-1.png" width="672" />



### Shannon

```r
source("World_Function.R")

Southern_Europ_Shannon_Cou <- fun_Shannon4(Southern_Europe,TotalCatch_Tonnes,all_price)

Southern_Europe_Shannon_Ave <- fun_Shannon5(Southern_Europe,TotalCatch_Tonnes,all_price)

#rbindするために列をそろえる、順番も変える
Southern_Europe_Shannon_Ave <- Southern_Europe_Shannon_Ave[-2]
Southern_Europe_Shannon_Ave <- Southern_Europe_Shannon_Ave[c(1,6,2,3,4,5)]

#結合する
Southern_Europe_Shannon <- rbind(Southern_Europ_Shannon_Cou,Southern_Europe_Shannon_Ave)

Shannon_Max <- max(Southern_Europe_Shannon$Shannon_Catch_Total,
                   Southern_Europe_Shannon$Shannon_Value_Total)

Shanon_Min <- min(Southern_Europe_Shannon$Shannon_Catch_Total,
                   Southern_Europe_Shannon$Shannon_Value_Total)


EffectA=1.5

p_9 <- ggplot(Southern_Europe_Shannon) +
  aes(x = Shannon_Catch_Total,y = Shannon_Value_Total,color = Year)+
  theme(text = element_text(family = "HiraKakuProN-W3"),panel.background=element_rect(fill="gray81"))+
  geom_point(aes(size = TotalCatch_Tonnes),alpha=.5) +
  scale_colour_gradient(name="年",low = "red", high = "blue",
                        breaks = seq(1950, 2010,by=10),
                        labels = seq(1950, 2010,by=10)) +
  geom_abline(intercept = 0,slope = 1,linetype = "dotted") +
  facet_wrap(~ Country.or.Area)　+
  labs(x = "Catch_H'",y = "Value_H'",size="Total_Catch(ton)") +   
 scale_x_continuous(limits = c(Shanon_Min,Shannon_Max)) +
  scale_y_continuous(limits = c(Shanon_Min,Shannon_Max)) +
  theme( axis.title = element_text(size=15, face="bold"),
       axis.text= element_text(size=12, face="bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
       plot.title = element_text(size=15, face="bold"),
       legend.title = element_text(size=12, face="bold"),
       legend.text=element_text(face="bold"))+
  scale_radius(range=c(5/EffectA,10/EffectA))

print(p_9)
```

<img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-27-1.png" width="672" />



## Northern Europe

```r
Northern_Europe <- World_Data %>% 
  filter(Sub.region.Name_koh == "Northern Europe")
```




### HHI

```r
source("World_Function.R")

Northern_Europe_HHI_Cou <- fun_HHI5(Northern_Europe,TotalCatch_Tonnes,all_price)

Northern_Europe_HHI_Ave <- fun_HHI6(Northern_Europe,TotalCatch_Tonnes,all_price)

#rbindするために列をそろえる、順番も変える
Northern_Europe_HHI_Ave <- Northern_Europe_HHI_Ave[-2]
Northern_Europe_HHI_Ave <- Northern_Europe_HHI_Ave[c(1,6,2,3,4,5)]

#結合する
Northern_Europe_HHI <- rbind(Northern_Europe_HHI_Cou,Northern_Europe_HHI_Ave)

HHI_Max <- max(Northern_Europe_HHI$HHI_Catch_Total,Northern_Europe_HHI$HHI_Value_Total)
HHI_Min <- min(Northern_Europe_HHI$HHI_Catch_Total,Northern_Europe_HHI$HHI_Value_Total)


EffectA=1.5

p_10 <- ggplot(Northern_Europe_HHI) +
  aes(x = HHI_Catch_Total,y = HHI_Value_Total,color = Year) +
  theme(text = element_text(family = "HiraKakuProN-W3"),panel.background=element_rect(fill="gray81"))+
  geom_point(aes(size = TotalCatch_Tonnes),alpha=.5) +
  scale_colour_gradient(name="年",low = "red", high = "blue",
                        breaks = seq(1950, 2010,by=10),
                        labels = seq(1950, 2010,by=10)) +
  geom_abline(intercept = 0,slope = 1,linetype = "dotted") +
  facet_wrap(~ Country.or.Area)　+
  labs(x = "Catch_HHI",y = "Value_HHI",size="Total_Catch(ton)") +　
  scale_x_continuous(limits = c(HHI_Min,HHI_Max)) +　　　　　　　　　　　
  scale_y_continuous(limits = c(HHI_Min,HHI_Max)) +
  theme( axis.title = element_text(size=15, face="bold"),
       axis.text= element_text(size=12, face="bold"),
       axis.text.x = element_text(angle = 45, hjust = 1),
       plot.title = element_text(size=15, face="bold"),
       legend.title = element_text(size=12, face="bold"),
       legend.text=element_text(face="bold"))+
  scale_radius(range=c(5/EffectA,10/EffectA))


print(p_10)
```

<img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-29-1.png" width="672" />


### Simpson

```r
source("World_Function.R")

Northern_Europe_Simpson_Cou <- fun_Simpson4(Northern_Europe,TotalCatch_Tonnes,all_price)

Northern_Europe_Simpson_Ave <- fun_Simpson5(Northern_Europe,TotalCatch_Tonnes,all_price)

#rbindするために列をそろえる、順番も変える
Northern_Europe_Simpson_Ave <- Northern_Europe_Simpson_Ave[-2]
Northern_Europe_Simpson_Ave <- Northern_Europe_Simpson_Ave[c(1,6,2,3,4,5)]

#結合する
Northern_Europe_Simpson <- rbind(Northern_Europe_Simpson_Cou,Northern_Europe_Simpson_Ave)

Simpson_Max <- max(Northern_Europe_Simpson$Simpson_Catch_Total,
                   Northern_Europe_Simpson$Simpson_Value_Total)

Simpson_Min <- min(Northern_Europe_Simpson$Simpson_Catch_Total,
                   Northern_Europe_Simpson$Simpson_Value_Total)


EffectA=1.5

p_11 <- ggplot(Northern_Europe_Simpson) +
  aes(x = Simpson_Catch_Total,y = Simpson_Value_Total,color = Year)+
  theme(text = element_text(family = "HiraKakuProN-W3"),panel.background=element_rect(fill="gray81"))+
  geom_point(aes(size = TotalCatch_Tonnes),alpha=.5) +
  scale_colour_gradient(name="年",low = "red", high = "blue",
                        breaks = seq(1950, 2010,by=10),
                        labels = seq(1950, 2010,by=10)) +
  geom_abline(intercept = 0,slope = 1,linetype = "dotted") +
  facet_wrap(~ Country.or.Area)　+
  labs(x = "Catch_λ",y = "Value_λ",size="Total_Catch(ton)") +   
 scale_x_continuous(limits = c(Simpson_Min,Simpson_Max)) +
  scale_y_continuous(limits = c(Simpson_Min,Simpson_Max)) +
  theme( axis.title = element_text(size=15, face="bold"),
       axis.text= element_text(size=12, face="bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
       plot.title = element_text(size=15, face="bold"),
       legend.title = element_text(size=12, face="bold"),
       legend.text=element_text(face="bold"))+
  scale_radius(range=c(5/EffectA,10/EffectA))

print(p_11)
```

<img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-30-1.png" width="672" />



### Shannon

```r
source("World_Function.R")

Northern_Europe_Shannon_Cou <- fun_Shannon4(Northern_Europe,TotalCatch_Tonnes,all_price)

Northern_Europe_Shannon_Ave <- fun_Shannon5(Northern_Europe,TotalCatch_Tonnes,all_price)

#rbindするために列をそろえる、順番も変える
Northern_Europe_Shannon_Ave <- Northern_Europe_Shannon_Ave[-2]
Northern_Europe_Shannon_Ave <- Northern_Europe_Shannon_Ave[c(1,6,2,3,4,5)]

#結合する
Northern_Europe_Shannon <- rbind(Northern_Europe_Shannon_Cou,Northern_Europe_Shannon_Ave)

Shannon_Max <- max(Northern_Europe_Shannon$Shannon_Catch_Total,
                   Northern_Europe_Shannon$Shannon_Value_Total)

Shanon_Min <- min(Northern_Europe_Shannon$Shannon_Catch_Total,
                   Northern_Europe_Shannon$Shannon_Value_Total)


EffectA=1.5

p_12 <- ggplot(Northern_Europe_Shannon) +
  aes(x = Shannon_Catch_Total,y = Shannon_Value_Total,color = Year)+
  theme(text = element_text(family = "HiraKakuProN-W3"),panel.background=element_rect(fill="gray81"))+
  geom_point(aes(size = TotalCatch_Tonnes),alpha=.5) +
  scale_colour_gradient(name="年",low = "red", high = "blue",
                        breaks = seq(1950, 2010,by=10),
                        labels = seq(1950, 2010,by=10)) +
  geom_abline(intercept = 0,slope = 1,linetype = "dotted") +
  facet_wrap(~ Country.or.Area)　+
  labs(x = "Catch_H'",y = "Value_H'",size="Total_Catch(ton)") +   
 scale_x_continuous(limits = c(Shanon_Min,Shannon_Max)) +
  scale_y_continuous(limits = c(Shanon_Min,Shannon_Max)) +
  theme( axis.title = element_text(size=15, face="bold"),
       axis.text= element_text(size=12, face="bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
       plot.title = element_text(size=15, face="bold"),
       legend.title = element_text(size=12, face="bold"),
       legend.text=element_text(face="bold"))+
  scale_radius(range=c(5/EffectA,10/EffectA))

print(p_12)
```

<img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-31-1.png" width="672" />



## Western Europe

```r
Western_Europe <- World_Data %>% 
  filter(Sub.region.Name_koh == "Western Europe")
```




### HHI

```r
source("World_Function.R")

Western_Europe_HHI_Cou <- fun_HHI5(Western_Europe,TotalCatch_Tonnes,all_price)

Western_Europe_HHI_Ave <- fun_HHI6(Western_Europe,TotalCatch_Tonnes,all_price)

#rbindするために列をそろえる、順番も変える
Western_Europe_HHI_Ave <- Western_Europe_HHI_Ave[-2]
Western_Europe_HHI_Ave <- Western_Europe_HHI_Ave[c(1,6,2,3,4,5)]

#結合する
Western_Europe_HHI <- rbind(Western_Europe_HHI_Cou,Western_Europe_HHI_Ave)

HHI_Max <- max(Western_Europe_HHI$HHI_Catch_Total,
               Western_Europe_HHI$HHI_Value_Total)
HHI_Min <- min(Western_Europe_HHI$HHI_Catch_Total,
               Western_Europe_HHI$HHI_Value_Total)


EffectA=1.5

p_13 <- ggplot(Western_Europe_HHI) +
  aes(x = HHI_Catch_Total,y = HHI_Value_Total,color = Year) +
  theme(text = element_text(family = "HiraKakuProN-W3"),panel.background=element_rect(fill="gray81"))+
  geom_point(aes(size = TotalCatch_Tonnes),alpha=.5) +
  scale_colour_gradient(name="年",low = "red", high = "blue",
                        breaks = seq(1950, 2010,by=10),
                        labels = seq(1950, 2010,by=10)) +
  geom_abline(intercept = 0,slope = 1,linetype = "dotted") +
  facet_wrap(~ Country.or.Area)　+
  labs(x = "Catch_HHI",y = "Value_HHI",size="Total_Catch(ton)") +　
  scale_x_continuous(limits = c(HHI_Min,HHI_Max)) +　　　　　　　　　　　
  scale_y_continuous(limits = c(HHI_Min,HHI_Max)) +
  theme( axis.title = element_text(size=15, face="bold"),
       axis.text= element_text(size=12, face="bold"),
       axis.text.x = element_text(angle = 45, hjust = 1),
       plot.title = element_text(size=15, face="bold"),
       legend.title = element_text(size=12, face="bold"),
       legend.text=element_text(face="bold"))+
  scale_radius(range=c(5/EffectA,10/EffectA))


print(p_13)
```

<img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-33-1.png" width="672" />


### Simpson

```r
source("World_Function.R")

Western_Europe_Simpson_Cou <- fun_Simpson4(Western_Europe,TotalCatch_Tonnes,all_price)

Western_Europe_Simpson_Ave <- fun_Simpson5(Western_Europe,TotalCatch_Tonnes,all_price)

#rbindするために列をそろえる、順番も変える
Western_Europe_Simpson_Ave <- Western_Europe_Simpson_Ave[-2]
Western_Europe_Simpson_Ave <- Western_Europe_Simpson_Ave[c(1,6,2,3,4,5)]

#結合する
Western_Europe_Simpson <- rbind(Western_Europe_Simpson_Cou,Western_Europe_Simpson_Ave)

Simpson_Max <- max(Western_Europe_Simpson$Simpson_Catch_Total,
                   Western_Europe_Simpson$Simpson_Value_Total)

Simpson_Min <- min(Western_Europe_Simpson$Simpson_Catch_Total,
                   Western_Europe_Simpson$Simpson_Value_Total)


EffectA=1.5

p_14 <- ggplot(Western_Europe_Simpson) +
  aes(x = Simpson_Catch_Total,y = Simpson_Value_Total,color = Year)+
  theme(text = element_text(family = "HiraKakuProN-W3"),panel.background=element_rect(fill="gray81"))+
  geom_point(aes(size = TotalCatch_Tonnes),alpha=.5) +
  scale_colour_gradient(name="年",low = "red", high = "blue",
                        breaks = seq(1950, 2010,by=10),
                        labels = seq(1950, 2010,by=10)) +
  geom_abline(intercept = 0,slope = 1,linetype = "dotted") +
  facet_wrap(~ Country.or.Area)　+
  labs(x = "Catch_λ",y = "Value_λ",size="Total_Catch(ton)") +   
 scale_x_continuous(limits = c(Simpson_Min,Simpson_Max)) +
  scale_y_continuous(limits = c(Simpson_Min,Simpson_Max)) +
  theme( axis.title = element_text(size=15, face="bold"),
       axis.text= element_text(size=12, face="bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
       plot.title = element_text(size=15, face="bold"),
       legend.title = element_text(size=12, face="bold"),
       legend.text=element_text(face="bold"))+
  scale_radius(range=c(5/EffectA,10/EffectA))

print(p_14)
```

<img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-34-1.png" width="672" />



### Shannon

```r
source("World_Function.R")

Western_Europe_Shannon_Cou <- fun_Shannon4(Western_Europe,TotalCatch_Tonnes,all_price)

Western_Europe_Shannon_Ave <- fun_Shannon5(Western_Europe,TotalCatch_Tonnes,all_price)

#rbindするために列をそろえる、順番も変える
Western_Europe_Shannon_Ave <- Western_Europe_Shannon_Ave[-2]
Western_Europe_Shannon_Ave <- Western_Europe_Shannon_Ave[c(1,6,2,3,4,5)]

#結合する
Western_Europe_Shannon <- rbind(Western_Europe_Shannon_Cou,Western_Europe_Shannon_Ave)

Shannon_Max <- max(Western_Europe_Shannon$Shannon_Catch_Total,
                   Western_Europe_Shannon$Shannon_Value_Total)

Shanon_Min <- min(Western_Europe_Shannon$Shannon_Catch_Total,
                   Western_Europe_Shannon$Shannon_Value_Total)


EffectA=1.5

p_15 <- ggplot(Western_Europe_Shannon) +
  aes(x = Shannon_Catch_Total,y = Shannon_Value_Total,color = Year)+
  theme(text = element_text(family = "HiraKakuProN-W3"),panel.background=element_rect(fill="gray81"))+
  geom_point(aes(size = TotalCatch_Tonnes),alpha=.5) +
  scale_colour_gradient(name="年",low = "red", high = "blue",
                        breaks = seq(1950, 2010,by=10),
                        labels = seq(1950, 2010,by=10)) +
  geom_abline(intercept = 0,slope = 1,linetype = "dotted") +
  facet_wrap(~ Country.or.Area)　+
  labs(x = "Catch_H'",y = "Value_H'",size="Total_Catch(ton)") +   
 scale_x_continuous(limits = c(Shanon_Min,Shannon_Max)) +
  scale_y_continuous(limits = c(Shanon_Min,Shannon_Max)) +
  theme( axis.title = element_text(size=15, face="bold"),
       axis.text= element_text(size=12, face="bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
       plot.title = element_text(size=15, face="bold"),
       legend.title = element_text(size=12, face="bold"),
       legend.text=element_text(face="bold"))+
  scale_radius(range=c(5/EffectA,10/EffectA))

print(p_15)
```

<img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-35-1.png" width="672" />



## 	Eastern Europe

```r
	Eastern_Europe <- World_Data %>% 
  filter(Sub.region.Name_koh == "Eastern Europe")
```




### HHI

```r
source("World_Function.R")

Eastern_Europe_HHI_Cou <- fun_HHI5(Eastern_Europe,TotalCatch_Tonnes,all_price)

Eastern_Europe_HHI_Ave <- fun_HHI6(Eastern_Europe,TotalCatch_Tonnes,all_price)

#rbindするために列をそろえる、順番も変える
Eastern_Europe_HHI_Ave <- Eastern_Europe_HHI_Ave[-2]
Eastern_Europe_HHI_Ave <- Eastern_Europe_HHI_Ave[c(1,6,2,3,4,5)]

#結合する
Eastern_Europe_HHI <- rbind(Eastern_Europe_HHI_Cou,Eastern_Europe_HHI_Ave)

HHI_Max <- max(Eastern_Europe_HHI$HHI_Catch_Total,
               Eastern_Europe_HHI$HHI_Value_Total)
HHI_Min <- min(Eastern_Europe_HHI$HHI_Catch_Total,
               Eastern_Europe_HHI$HHI_Value_Total)


EffectA=1.5

p_16 <- ggplot(Eastern_Europe_HHI) +
  aes(x = HHI_Catch_Total,y = HHI_Value_Total,color = Year) +
  theme(text = element_text(family = "HiraKakuProN-W3"),panel.background=element_rect(fill="gray81"))+
  geom_point(aes(size = TotalCatch_Tonnes),alpha=.5) +
  scale_colour_gradient(name="年",low = "red", high = "blue",
                        breaks = seq(1950, 2010,by=10),
                        labels = seq(1950, 2010,by=10)) +
  geom_abline(intercept = 0,slope = 1,linetype = "dotted") +
  facet_wrap(~ Country.or.Area)　+
  labs(x = "Catch_HHI",y = "Value_HHI",size="Total_Catch(ton)") +　
  scale_x_continuous(limits = c(HHI_Min,HHI_Max)) +　　　　　　　　　　　
  scale_y_continuous(limits = c(HHI_Min,HHI_Max)) +
  theme( axis.title = element_text(size=15, face="bold"),
       axis.text= element_text(size=12, face="bold"),
       axis.text.x = element_text(angle = 45, hjust = 1),
       plot.title = element_text(size=15, face="bold"),
       legend.title = element_text(size=12, face="bold"),
       legend.text=element_text(face="bold"))+
  scale_radius(range=c(5/EffectA,10/EffectA))


print(p_16)
```

<img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-37-1.png" width="672" />


### Simpson

```r
source("World_Function.R")

Eastern_Europe_Simpson_Cou <- fun_Simpson4(Eastern_Europe,TotalCatch_Tonnes,all_price)

Eastern_Europe_Simpson_Ave <- fun_Simpson5(Eastern_Europe,TotalCatch_Tonnes,all_price)

#rbindするために列をそろえる、順番も変える
Eastern_Europe_Simpson_Ave <- Eastern_Europe_Simpson_Ave[-2]
Eastern_Europe_Simpson_Ave <- Eastern_Europe_Simpson_Ave[c(1,6,2,3,4,5)]

#結合する
Eastern_Europe_Simpson <- rbind(Eastern_Europe_Simpson_Cou,Eastern_Europe_Simpson_Ave)

Simpson_Max <- max(Eastern_Europe_Simpson$Simpson_Catch_Total,
                   Eastern_Europe_Simpson$Simpson_Value_Total)

Simpson_Min <- min(Eastern_Europe_Simpson$Simpson_Catch_Total,
                   Eastern_Europe_Simpson$Simpson_Value_Total)


EffectA=1.5

p_17 <- ggplot(Eastern_Europe_Simpson) +
  aes(x = Simpson_Catch_Total,y = Simpson_Value_Total,color = Year)+
  theme(text = element_text(family = "HiraKakuProN-W3"),panel.background=element_rect(fill="gray81"))+
  geom_point(aes(size = TotalCatch_Tonnes),alpha=.5) +
  scale_colour_gradient(name="年",low = "red", high = "blue",
                        breaks = seq(1950, 2010,by=10),
                        labels = seq(1950, 2010,by=10)) +
  geom_abline(intercept = 0,slope = 1,linetype = "dotted") +
  facet_wrap(~ Country.or.Area)　+
  labs(x = "Catch_λ",y = "Value_λ",size="Total_Catch(ton)") +   
 scale_x_continuous(limits = c(Simpson_Min,Simpson_Max)) +
  scale_y_continuous(limits = c(Simpson_Min,Simpson_Max)) +
  theme( axis.title = element_text(size=15, face="bold"),
       axis.text= element_text(size=12, face="bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
       plot.title = element_text(size=15, face="bold"),
       legend.title = element_text(size=12, face="bold"),
       legend.text=element_text(face="bold"))+
  scale_radius(range=c(5/EffectA,10/EffectA))

print(p_17)
```

<img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-38-1.png" width="672" />



### Shannon

```r
source("World_Function.R")

Eastern_Europe_Shannon_Cou <- fun_Shannon4(Eastern_Europe,TotalCatch_Tonnes,all_price)

Eastern_Europe_Shannon_Ave <- fun_Shannon5(Eastern_Europe,TotalCatch_Tonnes,all_price)

#rbindするために列をそろえる、順番も変える
Eastern_Europe_Shannon_Ave <- Eastern_Europe_Shannon_Ave[-2]
Eastern_Europe_Shannon_Ave <- Eastern_Europe_Shannon_Ave[c(1,6,2,3,4,5)]

#結合する
Eastern_Europe_Shannon <- rbind(Eastern_Europe_Shannon_Cou,Eastern_Europe_Shannon_Ave)

Shannon_Max <- max(Eastern_Europe_Shannon$Shannon_Catch_Total,
                   Eastern_Europe_Shannon$Shannon_Value_Total)

Shanon_Min <- min(Eastern_Europe_Shannon$Shannon_Catch_Total,
                   Eastern_Europe_Shannon$Shannon_Value_Total)


EffectA=1.5

p_18 <- ggplot(Eastern_Europe_Shannon) +
  aes(x = Shannon_Catch_Total,y = Shannon_Value_Total,color = Year)+
  theme(text = element_text(family = "HiraKakuProN-W3"),panel.background=element_rect(fill="gray81"))+
  geom_point(aes(size = TotalCatch_Tonnes),alpha=.5) +
  scale_colour_gradient(name="年",low = "red", high = "blue",
                        breaks = seq(1950, 2010,by=10),
                        labels = seq(1950, 2010,by=10)) +
  geom_abline(intercept = 0,slope = 1,linetype = "dotted") +
  facet_wrap(~ Country.or.Area)　+
  labs(x = "Catch_H'",y = "Value_H'",size="Total_Catch(ton)") +   
 scale_x_continuous(limits = c(Shanon_Min,Shannon_Max)) +
  scale_y_continuous(limits = c(Shanon_Min,Shannon_Max)) +
  theme( axis.title = element_text(size=15, face="bold"),
       axis.text= element_text(size=12, face="bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
       plot.title = element_text(size=15, face="bold"),
       legend.title = element_text(size=12, face="bold"),
       legend.text=element_text(face="bold"))+
  scale_radius(range=c(5/EffectA,10/EffectA))

print(p_18)
```

<img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-39-1.png" width="672" />



## 	Northern Africa

```r
Northern_Africa <- World_Data %>% 
  filter(Sub.region.Name_koh == "Northern Africa")
```




### HHI

```r
source("World_Function.R")

Northern_Africa_HHI_Cou <- fun_HHI5(Northern_Africa,TotalCatch_Tonnes,all_price)

Northern_Africa_HHI_Ave <- fun_HHI6(Northern_Africa,TotalCatch_Tonnes,all_price)

#rbindするために列をそろえる、順番も変える
Northern_Africa_HHI_Ave <- Northern_Africa_HHI_Ave[-2]
Northern_Africa_HHI_Ave <- Northern_Africa_HHI_Ave[c(1,6,2,3,4,5)]

#結合する
Northern_Africa_HHI <- rbind(Northern_Africa_HHI_Cou,Northern_Africa_HHI_Ave)

HHI_Max <- max(Northern_Africa_HHI$HHI_Catch_Total,
               Northern_Africa_HHI$HHI_Value_Total)
HHI_Min <- min(Northern_Africa_HHI$HHI_Catch_Total,
               Northern_Africa_HHI$HHI_Value_Total)


EffectA=1.5

p_19 <- ggplot(Northern_Africa_HHI) +
  aes(x = HHI_Catch_Total,y = HHI_Value_Total,color = Year) +
  theme(text = element_text(family = "HiraKakuProN-W3"),panel.background=element_rect(fill="gray81"))+
  geom_point(aes(size = TotalCatch_Tonnes),alpha=.5) +
  scale_colour_gradient(name="年",low = "red", high = "blue",
                        breaks = seq(1950, 2010,by=10),
                        labels = seq(1950, 2010,by=10)) +
  geom_abline(intercept = 0,slope = 1,linetype = "dotted") +
  facet_wrap(~ Country.or.Area)　+
  labs(x = "Catch_HHI",y = "Value_HHI",size="Total_Catch(ton)") +　
  scale_x_continuous(limits = c(HHI_Min,HHI_Max)) +　　　　　　　　　　　
  scale_y_continuous(limits = c(HHI_Min,HHI_Max)) +
  theme( axis.title = element_text(size=15, face="bold"),
       axis.text= element_text(size=12, face="bold"),
       axis.text.x = element_text(angle = 45, hjust = 1),
       plot.title = element_text(size=15, face="bold"),
       legend.title = element_text(size=12, face="bold"),
       legend.text=element_text(face="bold"))+
  scale_radius(range=c(5/EffectA,10/EffectA))


print(p_19)
```

<img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-41-1.png" width="672" />


### Simpson

```r
source("World_Function.R")

Northern_Africa_Simpson_Cou <- fun_Simpson4(Northern_Africa,TotalCatch_Tonnes,all_price)

Northern_Africa_Simpson_Ave <- fun_Simpson5(Northern_Africa,TotalCatch_Tonnes,all_price)

#rbindするために列をそろえる、順番も変える
Northern_Africa_Simpson_Ave <- Northern_Africa_Simpson_Ave[-2]
Northern_Africa_Simpson_Ave <- Northern_Africa_Simpson_Ave[c(1,6,2,3,4,5)]

#結合する
Northern_Africa_Simpson <- rbind(Northern_Africa_Simpson_Cou,Northern_Africa_Simpson_Ave)

Simpson_Max <- max(Northern_Africa_Simpson$Simpson_Catch_Total,
                   Northern_Africa_Simpson$Simpson_Value_Total)

Simpson_Min <- min(Northern_Africa_Simpson$Simpson_Catch_Total,
                   Northern_Africa_Simpson$Simpson_Value_Total)


EffectA=1.5

p_20 <- ggplot(Northern_Africa_Simpson) +
  aes(x = Simpson_Catch_Total,y = Simpson_Value_Total,color = Year)+
  theme(text = element_text(family = "HiraKakuProN-W3"),panel.background=element_rect(fill="gray81"))+
  geom_point(aes(size = TotalCatch_Tonnes),alpha=.5) +
  scale_colour_gradient(name="年",low = "red", high = "blue",
                        breaks = seq(1950, 2010,by=10),
                        labels = seq(1950, 2010,by=10)) +
  geom_abline(intercept = 0,slope = 1,linetype = "dotted") +
  facet_wrap(~ Country.or.Area)　+
  labs(x = "Catch_λ",y = "Value_λ",size="Total_Catch(ton)") +   
 scale_x_continuous(limits = c(Simpson_Min,Simpson_Max)) +
  scale_y_continuous(limits = c(Simpson_Min,Simpson_Max)) +
  theme( axis.title = element_text(size=15, face="bold"),
       axis.text= element_text(size=12, face="bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
       plot.title = element_text(size=15, face="bold"),
       legend.title = element_text(size=12, face="bold"),
       legend.text=element_text(face="bold"))+
  scale_radius(range=c(5/EffectA,10/EffectA))

print(p_20)
```

<img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-42-1.png" width="672" />



### Shannon

```r
source("World_Function.R")

Northern_Africa_Shannon_Cou <- fun_Shannon4(Northern_Africa,TotalCatch_Tonnes,all_price)

Northern_Africa_Shannon_Ave <- fun_Shannon5(Northern_Africa,TotalCatch_Tonnes,all_price)

#rbindするために列をそろえる、順番も変える
Northern_Africa_Shannon_Ave <- Northern_Africa_Shannon_Ave[-2]
Northern_Africa_Shannon_Ave <- Northern_Africa_Shannon_Ave[c(1,6,2,3,4,5)]

#結合する
Northern_Africa_Shannon <- rbind(Northern_Africa_Shannon_Cou,Northern_Africa_Shannon_Ave)

Shannon_Max <- max(Northern_Africa_Shannon$Shannon_Catch_Total,
                   Northern_Africa_Shannon$Shannon_Value_Total)

Shanon_Min <- min(Northern_Africa_Shannon$Shannon_Catch_Total,
                   Northern_Africa_Shannon$Shannon_Value_Total)


EffectA=1.5

p_21 <- ggplot(Northern_Africa_Shannon) +
  aes(x = Shannon_Catch_Total,y = Shannon_Value_Total,color = Year)+
  theme(text = element_text(family = "HiraKakuProN-W3"),panel.background=element_rect(fill="gray81"))+
  geom_point(aes(size = TotalCatch_Tonnes),alpha=.5) +
  scale_colour_gradient(name="年",low = "red", high = "blue",
                        breaks = seq(1950, 2010,by=10),
                        labels = seq(1950, 2010,by=10)) +
  geom_abline(intercept = 0,slope = 1,linetype = "dotted") +
  facet_wrap(~ Country.or.Area)　+
  labs(x = "Catch_H'",y = "Value_H'",size="Total_Catch(ton)") +   
 scale_x_continuous(limits = c(Shanon_Min,Shannon_Max)) +
  scale_y_continuous(limits = c(Shanon_Min,Shannon_Max)) +
  theme( axis.title = element_text(size=15, face="bold"),
       axis.text= element_text(size=12, face="bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
       plot.title = element_text(size=15, face="bold"),
       legend.title = element_text(size=12, face="bold"),
       legend.text=element_text(face="bold"))+
  scale_radius(range=c(5/EffectA,10/EffectA))

print(p_21)
```

<img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-43-1.png" width="672" />



## 	Sub-Saharan Africa

```r
Sub_Saharan_Africa <- World_Data %>% 
  filter(Sub.region.Name_koh == "Sub-Saharan Africa")
```




### HHI

```r
source("World_Function.R")

Sub_Saharan_Africa_HHI_Cou <- fun_HHI5(Sub_Saharan_Africa,TotalCatch_Tonnes,all_price)

Sub_Saharan_Africa_HHI_Ave <- fun_HHI6(Sub_Saharan_Africa,TotalCatch_Tonnes,all_price)

#rbindするために列をそろえる、順番も変える
Sub_Saharan_Africa_HHI_Ave <- Sub_Saharan_Africa_HHI_Ave[-2]
Sub_Saharan_Africa_HHI_Ave <- Sub_Saharan_Africa_HHI_Ave[c(1,6,2,3,4,5)]

#結合する
Sub_Saharan_Africa_HHI <- rbind(Sub_Saharan_Africa_HHI_Cou,Sub_Saharan_Africa_HHI_Ave)

HHI_Max <- max(Sub_Saharan_Africa_HHI$HHI_Catch_Total,
               Sub_Saharan_Africa_HHI$HHI_Value_Total)
HHI_Min <- min(Sub_Saharan_Africa_HHI$HHI_Catch_Total,
               Sub_Saharan_Africa_HHI$HHI_Value_Total)


EffectA=1.5

p_22 <- ggplot(Sub_Saharan_Africa_HHI) +
  aes(x = HHI_Catch_Total,y = HHI_Value_Total,color = Year) +
  theme(text = element_text(family = "HiraKakuProN-W3"),panel.background=element_rect(fill="gray81"))+
  geom_point(aes(size = TotalCatch_Tonnes),alpha=.5) +
  scale_colour_gradient(name="年",low = "red", high = "blue",
                        breaks = seq(1950, 2010,by=10),
                        labels = seq(1950, 2010,by=10)) +
  geom_abline(intercept = 0,slope = 1,linetype = "dotted") +
  facet_wrap(~ Country.or.Area)　+
  labs(x = "Catch_HHI",y = "Value_HHI",size="Total_Catch(ton)") +　
  scale_x_continuous(limits = c(HHI_Min,HHI_Max)) +　　　　　　　　　　　
  scale_y_continuous(limits = c(HHI_Min,HHI_Max)) +
  theme( axis.title = element_text(size=15, face="bold"),
       axis.text= element_text(size=12, face="bold"),
       axis.text.x = element_text(angle = 45, hjust = 1),
       plot.title = element_text(size=15, face="bold"),
       legend.title = element_text(size=12, face="bold"),
       legend.text=element_text(face="bold"))+
  scale_radius(range=c(5/EffectA,10/EffectA))


print(p_22)
```

<img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-45-1.png" width="672" />


### Simpson

```r
source("World_Function.R")

Sub_Saharan_Africa_Simpson_Cou <- fun_Simpson4(Sub_Saharan_Africa,TotalCatch_Tonnes,all_price)

Sub_Saharan_Africa_Simpson_Ave <- fun_Simpson5(Sub_Saharan_Africa,TotalCatch_Tonnes,all_price)

#rbindするために列をそろえる、順番も変える
Sub_Saharan_Africa_Simpson_Ave <- Sub_Saharan_Africa_Simpson_Ave[-2]
Sub_Saharan_Africa_Simpson_Ave <- Sub_Saharan_Africa_Simpson_Ave[c(1,6,2,3,4,5)]

#結合する
Sub_Saharan_Africa_Simpson <- rbind(Sub_Saharan_Africa_Simpson_Cou,Sub_Saharan_Africa_Simpson_Ave)

Simpson_Max <- max(Sub_Saharan_Africa_Simpson$Simpson_Catch_Total,
                   Sub_Saharan_Africa_Simpson$Simpson_Value_Total)

Simpson_Min <- min(Sub_Saharan_Africa_Simpson$Simpson_Catch_Total,
                   Sub_Saharan_Africa_Simpson$Simpson_Value_Total)


EffectA=1.5

p_23 <- ggplot(Sub_Saharan_Africa_Simpson) +
  aes(x = Simpson_Catch_Total,y = Simpson_Value_Total,color = Year)+
  theme(text = element_text(family = "HiraKakuProN-W3"),panel.background=element_rect(fill="gray81"))+
  geom_point(aes(size = TotalCatch_Tonnes),alpha=.5) +
  scale_colour_gradient(name="年",low = "red", high = "blue",
                        breaks = seq(1950, 2010,by=10),
                        labels = seq(1950, 2010,by=10)) +
  geom_abline(intercept = 0,slope = 1,linetype = "dotted") +
  facet_wrap(~ Country.or.Area)　+
  labs(x = "Catch_λ",y = "Value_λ",size="Total_Catch(ton)") +   
 scale_x_continuous(limits = c(Simpson_Min,Simpson_Max)) +
  scale_y_continuous(limits = c(Simpson_Min,Simpson_Max)) +
  theme( axis.title = element_text(size=15, face="bold"),
       axis.text= element_text(size=12, face="bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
       plot.title = element_text(size=15, face="bold"),
       legend.title = element_text(size=12, face="bold"),
       legend.text=element_text(face="bold"))+
  scale_radius(range=c(5/EffectA,10/EffectA))

print(p_23)
```

<img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-46-1.png" width="672" />



### Shannon

```r
source("World_Function.R")

Sub_Saharan_Africa_Shannon_Cou <- fun_Shannon4(Sub_Saharan_Africa,TotalCatch_Tonnes,all_price)

Sub_Saharan_Africa_Shannon_Ave <- fun_Shannon5(Sub_Saharan_Africa,TotalCatch_Tonnes,all_price)

#rbindするために列をそろえる、順番も変える
Sub_Saharan_Africa_Shannon_Ave <- Sub_Saharan_Africa_Shannon_Ave[-2]
Sub_Saharan_Africa_Shannon_Ave <- Sub_Saharan_Africa_Shannon_Ave[c(1,6,2,3,4,5)]

#結合する
Sub_Saharan_Africa_Shannon <- rbind(Sub_Saharan_Africa_Shannon_Cou,Sub_Saharan_Africa_Shannon_Ave)

Shannon_Max <- max(Sub_Saharan_Africa_Shannon$Shannon_Catch_Total,
                   Sub_Saharan_Africa_Shannon$Shannon_Value_Total)

Shanon_Min <- min(Sub_Saharan_Africa_Shannon$Shannon_Catch_Total,
                   Sub_Saharan_Africa_Shannon$Shannon_Value_Total)


EffectA=1.5

p_24 <- ggplot(Sub_Saharan_Africa_Shannon) +
  aes(x = Shannon_Catch_Total,y = Shannon_Value_Total,color = Year)+
  theme(text = element_text(family = "HiraKakuProN-W3"),panel.background=element_rect(fill="gray81"))+
  geom_point(aes(size = TotalCatch_Tonnes),alpha=.5) +
  scale_colour_gradient(name="年",low = "red", high = "blue",
                        breaks = seq(1950, 2010,by=10),
                        labels = seq(1950, 2010,by=10)) +
  geom_abline(intercept = 0,slope = 1,linetype = "dotted") +
  facet_wrap(~ Country.or.Area)　+
  labs(x = "Catch_H'",y = "Value_H'",size="Total_Catch(ton)") +   
 scale_x_continuous(limits = c(Shanon_Min,Shannon_Max)) +
  scale_y_continuous(limits = c(Shanon_Min,Shannon_Max)) +
  theme( axis.title = element_text(size=15, face="bold"),
       axis.text= element_text(size=12, face="bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
       plot.title = element_text(size=15, face="bold"),
       legend.title = element_text(size=12, face="bold"),
       legend.text=element_text(face="bold"))+
  scale_radius(range=c(5/EffectA,10/EffectA))

print(p_24)
```

<img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-47-1.png" width="672" />




## 	Northern America

```r
Northern_America <- World_Data %>% 
  filter(Sub.region.Name_koh == "Northern America")
```




### HHI

```r
source("World_Function.R")

Northern_America_HHI_Cou <- fun_HHI5(Northern_America,TotalCatch_Tonnes,all_price)

Northern_America_HHI_Ave <- fun_HHI6(Northern_America,TotalCatch_Tonnes,all_price)

#rbindするために列をそろえる、順番も変える
Northern_America_HHI_Ave <- Northern_America_HHI_Ave[-2]
Northern_America_HHI_Ave <- Northern_America_HHI_Ave[c(1,6,2,3,4,5)]

#結合する
Northern_America_HHI <- rbind(Northern_America_HHI_Cou,Northern_America_HHI_Ave)

HHI_Max <- max(Northern_America_HHI$HHI_Catch_Total,
               Northern_America_HHI$HHI_Value_Total)
HHI_Min <- min(Northern_America_HHI$HHI_Catch_Total,
               Northern_America_HHI$HHI_Value_Total)


EffectA=1.5

p_25 <- ggplot(Northern_America_HHI) +
  aes(x = HHI_Catch_Total,y = HHI_Value_Total,color = Year) +
  theme(text = element_text(family = "HiraKakuProN-W3"),panel.background=element_rect(fill="gray81"))+
  geom_point(aes(size = TotalCatch_Tonnes),alpha=.5) +
  scale_colour_gradient(name="年",low = "red", high = "blue",
                        breaks = seq(1950, 2010,by=10),
                        labels = seq(1950, 2010,by=10)) +
  geom_abline(intercept = 0,slope = 1,linetype = "dotted") +
  facet_wrap(~ Country.or.Area)　+
  labs(x = "Catch_HHI",y = "Value_HHI",size="Total_Catch(ton)") +　
  scale_x_continuous(limits = c(HHI_Min,HHI_Max)) +　　　　　　　　　　　
  scale_y_continuous(limits = c(HHI_Min,HHI_Max)) +
  theme( axis.title = element_text(size=15, face="bold"),
       axis.text= element_text(size=12, face="bold"),
       axis.text.x = element_text(angle = 45, hjust = 1),
       plot.title = element_text(size=15, face="bold"),
       legend.title = element_text(size=12, face="bold"),
       legend.text=element_text(face="bold"))+
  scale_radius(range=c(5/EffectA,10/EffectA))


print(p_25)
```

<img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-49-1.png" width="672" />


### Simpson

```r
source("World_Function.R")

Northern_America_Simpson_Cou <- fun_Simpson4(Northern_America,TotalCatch_Tonnes,all_price)

Northern_America_Simpson_Ave <- fun_Simpson5(Northern_America,TotalCatch_Tonnes,all_price)

#rbindするために列をそろえる、順番も変える
Northern_America_Simpson_Ave <- Northern_America_Simpson_Ave[-2]
Northern_America_Simpson_Ave <- Northern_America_Simpson_Ave[c(1,6,2,3,4,5)]

#結合する
Northern_America_Simpson <- rbind(Northern_America_Simpson_Cou,Northern_America_Simpson_Ave)

Simpson_Max <- max(Northern_America_Simpson$Simpson_Catch_Total,
                   Northern_America_Simpson$Simpson_Value_Total)

Simpson_Min <- min(Northern_America_Simpson$Simpson_Catch_Total,
                   Northern_America_Simpson$Simpson_Value_Total)


EffectA=1.5

p_26 <- ggplot(Northern_America_Simpson) +
  aes(x = Simpson_Catch_Total,y = Simpson_Value_Total,color = Year)+
  theme(text = element_text(family = "HiraKakuProN-W3"),panel.background=element_rect(fill="gray81"))+
  geom_point(aes(size = TotalCatch_Tonnes),alpha=.5) +
  scale_colour_gradient(name="年",low = "red", high = "blue",
                        breaks = seq(1950, 2010,by=10),
                        labels = seq(1950, 2010,by=10)) +
  geom_abline(intercept = 0,slope = 1,linetype = "dotted") +
  facet_wrap(~ Country.or.Area)　+
  labs(x = "Catch_λ",y = "Value_λ",size="Total_Catch(ton)") +   
 scale_x_continuous(limits = c(Simpson_Min,Simpson_Max)) +
  scale_y_continuous(limits = c(Simpson_Min,Simpson_Max)) +
  theme( axis.title = element_text(size=15, face="bold"),
       axis.text= element_text(size=12, face="bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
       plot.title = element_text(size=15, face="bold"),
       legend.title = element_text(size=12, face="bold"),
       legend.text=element_text(face="bold"))+
  scale_radius(range=c(5/EffectA,10/EffectA))

print(p_26)
```

<img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-50-1.png" width="672" />



### Shannon

```r
source("World_Function.R")

Northern_America_Shannon_Cou <- fun_Shannon4(Northern_America,TotalCatch_Tonnes,all_price)

Northern_America_Shannon_Ave <- fun_Shannon5(Northern_America,TotalCatch_Tonnes,all_price)

#rbindするために列をそろえる、順番も変える
Northern_America_Shannon_Ave <- Northern_America_Shannon_Ave[-2]
Northern_America_Shannon_Ave <- Northern_America_Shannon_Ave[c(1,6,2,3,4,5)]

#結合する
Northern_America_Shannon <- rbind(Northern_America_Shannon_Cou,Northern_America_Shannon_Ave)

Shannon_Max <- max(Northern_America_Shannon$Shannon_Catch_Total,
                   Northern_America_Shannon$Shannon_Value_Total)

Shanon_Min <- min(Northern_America_Shannon$Shannon_Catch_Total,
                   Northern_America_Shannon$Shannon_Value_Total)


EffectA=1.5

p_27 <- ggplot(Northern_America_Shannon) +
  aes(x = Shannon_Catch_Total,y = Shannon_Value_Total,color = Year)+
  theme(text = element_text(family = "HiraKakuProN-W3"),panel.background=element_rect(fill="gray81"))+
  geom_point(aes(size = TotalCatch_Tonnes),alpha=.5) +
  scale_colour_gradient(name="年",low = "red", high = "blue",
                        breaks = seq(1950, 2010,by=10),
                        labels = seq(1950, 2010,by=10)) +
  geom_abline(intercept = 0,slope = 1,linetype = "dotted") +
  facet_wrap(~ Country.or.Area)　+
  labs(x = "Catch_H'",y = "Value_H'",size="Total_Catch(ton)") +   
 scale_x_continuous(limits = c(Shanon_Min,Shannon_Max)) +
  scale_y_continuous(limits = c(Shanon_Min,Shannon_Max)) +
  theme( axis.title = element_text(size=15, face="bold"),
       axis.text= element_text(size=12, face="bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
       plot.title = element_text(size=15, face="bold"),
       legend.title = element_text(size=12, face="bold"),
       legend.text=element_text(face="bold"))+
  scale_radius(range=c(5/EffectA,10/EffectA))

print(p_27)
```

<img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-51-1.png" width="672" />



## 		Latin America and the Caribbean

```r
	Latin_America_and_the_Caribbean <- World_Data %>% 
  filter(Sub.region.Name_koh == "Latin America and the Caribbean")
```


### HHI

```r
source("World_Function.R")

Latin_America_and_the_Caribbean_HHI_Cou <- fun_HHI5(Latin_America_and_the_Caribbean,TotalCatch_Tonnes,all_price)

Latin_America_and_the_Caribbean_HHI_Ave <- fun_HHI6(Latin_America_and_the_Caribbean,TotalCatch_Tonnes,all_price)

#rbindするために列をそろえる、順番も変える
Latin_America_and_the_Caribbean_HHI_Ave <- Latin_America_and_the_Caribbean_HHI_Ave[-2]
Latin_America_and_the_Caribbean_HHI_Ave <- Latin_America_and_the_Caribbean_HHI_Ave[c(1,6,2,3,4,5)]

#結合する
Latin_America_and_the_Caribbean_HHI <- rbind(Latin_America_and_the_Caribbean_HHI_Cou,Latin_America_and_the_Caribbean_HHI_Ave)

HHI_Max <- max(Latin_America_and_the_Caribbean_HHI$HHI_Catch_Total,
               Latin_America_and_the_Caribbean_HHI$HHI_Value_Total)
HHI_Min <- min(Latin_America_and_the_Caribbean_HHI$HHI_Catch_Total,
               Latin_America_and_the_Caribbean_HHI$HHI_Value_Total)


EffectA=1.5

p_28 <- ggplot(Latin_America_and_the_Caribbean_HHI) +
  aes(x = HHI_Catch_Total,y = HHI_Value_Total,color = Year) +
  theme(text = element_text(family = "HiraKakuProN-W3"),panel.background=element_rect(fill="gray81"))+
  geom_point(aes(size = TotalCatch_Tonnes),alpha=.5) +
  scale_colour_gradient(name="年",low = "red", high = "blue",
                        breaks = seq(1950, 2010,by=10),
                        labels = seq(1950, 2010,by=10)) +
  geom_abline(intercept = 0,slope = 1,linetype = "dotted") +
  facet_wrap(~ Country.or.Area)　+
  labs(x = "Catch_HHI",y = "Value_HHI",size="Total_Catch(ton)") +　
  scale_x_continuous(limits = c(HHI_Min,HHI_Max)) +　　　　　　　　　　　
  scale_y_continuous(limits = c(HHI_Min,HHI_Max)) +
  theme( axis.title = element_text(size=15, face="bold"),
       axis.text= element_text(size=12, face="bold"),
       axis.text.x = element_text(angle = 45, hjust = 1),
       plot.title = element_text(size=15, face="bold"),
       legend.title = element_text(size=12, face="bold"),
       legend.text=element_text(face="bold"))+
  scale_radius(range=c(5/EffectA,10/EffectA))


print(p_28)
```

<img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-53-1.png" width="672" />


### Simpson

```r
source("World_Function.R")

Latin_America_and_the_Caribbean_Simpson_Cou <- fun_Simpson4(Latin_America_and_the_Caribbean,TotalCatch_Tonnes,all_price)

Latin_America_and_the_Caribbean_Simpson_Ave <- fun_Simpson5(Latin_America_and_the_Caribbean,TotalCatch_Tonnes,all_price)

#rbindするために列をそろえる、順番も変える
Latin_America_and_the_Caribbean_Simpson_Ave <- Latin_America_and_the_Caribbean_Simpson_Ave[-2]
Latin_America_and_the_Caribbean_Simpson_Ave <- Latin_America_and_the_Caribbean_Simpson_Ave[c(1,6,2,3,4,5)]

#結合する
Latin_America_and_the_Caribbean_Simpson <- rbind(Latin_America_and_the_Caribbean_Simpson_Cou,Latin_America_and_the_Caribbean_Simpson_Ave)

Simpson_Max <- max(Latin_America_and_the_Caribbean_Simpson$Simpson_Catch_Total,
                   Latin_America_and_the_Caribbean_Simpson$Simpson_Value_Total)

Simpson_Min <- min(Latin_America_and_the_Caribbean_Simpson$Simpson_Catch_Total,
                   Latin_America_and_the_Caribbean_Simpson$Simpson_Value_Total)


EffectA=1.5

p_29 <- ggplot(Latin_America_and_the_Caribbean_Simpson) +
  aes(x = Simpson_Catch_Total,y = Simpson_Value_Total,color = Year)+
  theme(text = element_text(family = "HiraKakuProN-W3"),panel.background=element_rect(fill="gray81"))+
  geom_point(aes(size = TotalCatch_Tonnes),alpha=.5) +
  scale_colour_gradient(name="年",low = "red", high = "blue",
                        breaks = seq(1950, 2010,by=10),
                        labels = seq(1950, 2010,by=10)) +
  geom_abline(intercept = 0,slope = 1,linetype = "dotted") +
  facet_wrap(~ Country.or.Area)　+
  labs(x = "Catch_λ",y = "Value_λ",size="Total_Catch(ton)") +   
 scale_x_continuous(limits = c(Simpson_Min,Simpson_Max)) +
  scale_y_continuous(limits = c(Simpson_Min,Simpson_Max)) +
  theme( axis.title = element_text(size=15, face="bold"),
       axis.text= element_text(size=12, face="bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
       plot.title = element_text(size=15, face="bold"),
       legend.title = element_text(size=12, face="bold"),
       legend.text=element_text(face="bold"))+
  scale_radius(range=c(5/EffectA,10/EffectA))

print(p_29)
```

<img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-54-1.png" width="672" />



### Shannon

```r
source("World_Function.R")

Latin_America_and_the_Caribbean_Shannon_Cou <- fun_Shannon4(Latin_America_and_the_Caribbean,TotalCatch_Tonnes,all_price)

Latin_America_and_the_Caribbean_Shannon_Ave <- fun_Shannon5(Latin_America_and_the_Caribbean,TotalCatch_Tonnes,all_price)

#rbindするために列をそろえる、順番も変える
Latin_America_and_the_Caribbean_Shannon_Ave <- Latin_America_and_the_Caribbean_Shannon_Ave[-2]
Latin_America_and_the_Caribbean_Shannon_Ave <- Latin_America_and_the_Caribbean_Shannon_Ave[c(1,6,2,3,4,5)]

#結合する
Latin_America_and_the_Caribbean_Shannon <- rbind(Latin_America_and_the_Caribbean_Shannon_Cou,Latin_America_and_the_Caribbean_Shannon_Ave)

Shannon_Max <- max(Latin_America_and_the_Caribbean_Shannon$Shannon_Catch_Total,
                   Latin_America_and_the_Caribbean_Shannon$Shannon_Value_Total)

Shanon_Min <- min(Latin_America_and_the_Caribbean_Shannon$Shannon_Catch_Total,
                   Latin_America_and_the_Caribbean_Shannon$Shannon_Value_Total)


EffectA=1.5

p_30 <- ggplot(Latin_America_and_the_Caribbean_Shannon) +
  aes(x = Shannon_Catch_Total,y = Shannon_Value_Total,color = Year)+
  theme(text = element_text(family = "HiraKakuProN-W3"),panel.background=element_rect(fill="gray81"))+
  geom_point(aes(size = TotalCatch_Tonnes),alpha=.5) +
  scale_colour_gradient(name="年",low = "red", high = "blue",
                        breaks = seq(1950, 2010,by=10),
                        labels = seq(1950, 2010,by=10)) +
  geom_abline(intercept = 0,slope = 1,linetype = "dotted") +
  facet_wrap(~ Country.or.Area)　+
  labs(x = "Catch_H'",y = "Value_H'",size="Total_Catch(ton)") +   
 scale_x_continuous(limits = c(Shanon_Min,Shannon_Max)) +
  scale_y_continuous(limits = c(Shanon_Min,Shannon_Max)) +
  theme( axis.title = element_text(size=15, face="bold"),
       axis.text= element_text(size=12, face="bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
       plot.title = element_text(size=15, face="bold"),
       legend.title = element_text(size=12, face="bold"),
       legend.text=element_text(face="bold"))+
  scale_radius(range=c(5/EffectA,10/EffectA))

print(p_30)
```

<img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-55-1.png" width="672" />



## 		Micronesia

```r
	Micronesia <- World_Data %>% 
  filter(Sub.region.Name_koh == "Micronesia")
```


### HHI

```r
source("World_Function.R")

Micronesia_HHI_Cou <- fun_HHI5(Micronesia,TotalCatch_Tonnes,all_price)

Micronesia_HHI_Ave <- fun_HHI6(Micronesia,TotalCatch_Tonnes,all_price)

#rbindするために列をそろえる、順番も変える
Micronesia_HHI_Ave <- Micronesia_HHI_Ave[-2]
Micronesia_HHI_Ave <- Micronesia_HHI_Ave[c(1,6,2,3,4,5)]

#結合する
Micronesia_HHI <- rbind(Micronesia_HHI_Cou,Micronesia_HHI_Ave)

HHI_Max <- max(Micronesia_HHI$HHI_Catch_Total,
               Micronesia_HHI$HHI_Value_Total)
HHI_Min <- min(Micronesia_HHI$HHI_Catch_Total,
               Micronesia_HHI$HHI_Value_Total)


EffectA=1.5

p_31 <- ggplot(Micronesia_HHI) +
  aes(x = HHI_Catch_Total,y = HHI_Value_Total,color = Year) +
  theme(text = element_text(family = "HiraKakuProN-W3"),panel.background=element_rect(fill="gray81"))+
  geom_point(aes(size = TotalCatch_Tonnes),alpha=.5) +
  scale_colour_gradient(name="年",low = "red", high = "blue",
                        breaks = seq(1950, 2010,by=10),
                        labels = seq(1950, 2010,by=10)) +
  geom_abline(intercept = 0,slope = 1,linetype = "dotted") +
  facet_wrap(~ Country.or.Area)　+
  labs(x = "Catch_HHI",y = "Value_HHI",size="Total_Catch(ton)") +　
  scale_x_continuous(limits = c(HHI_Min,HHI_Max)) +　　　　　　　　　　　
  scale_y_continuous(limits = c(HHI_Min,HHI_Max)) +
  theme( axis.title = element_text(size=15, face="bold"),
       axis.text= element_text(size=12, face="bold"),
       axis.text.x = element_text(angle = 45, hjust = 1),
       plot.title = element_text(size=15, face="bold"),
       legend.title = element_text(size=12, face="bold"),
       legend.text=element_text(face="bold"))+
  scale_radius(range=c(5/EffectA,10/EffectA))


print(p_31)
```

<img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-57-1.png" width="672" />


### Simpson

```r
source("World_Function.R")

Micronesia_Simpson_Cou <- fun_Simpson4(Micronesia,TotalCatch_Tonnes,all_price)

Micronesia_Simpson_Ave <- fun_Simpson5(Micronesia,TotalCatch_Tonnes,all_price)

#rbindするために列をそろえる、順番も変える
Micronesia_Simpson_Ave <- Micronesia_Simpson_Ave[-2]
Micronesia_Simpson_Ave <- Micronesia_Simpson_Ave[c(1,6,2,3,4,5)]

#結合する
Micronesia_Simpson <- rbind(Micronesia_Simpson_Cou,Micronesia_Simpson_Ave)

Simpson_Max <- max(Micronesia_Simpson$Simpson_Catch_Total,
                   Micronesia_Simpson$Simpson_Value_Total)

Simpson_Min <- min(Micronesia_Simpson$Simpson_Catch_Total,
                   Micronesia_Simpson$Simpson_Value_Total)


EffectA=1.5

p_32 <- ggplot(Micronesia_Simpson) +
  aes(x = Simpson_Catch_Total,y = Simpson_Value_Total,color = Year)+
  theme(text = element_text(family = "HiraKakuProN-W3"),panel.background=element_rect(fill="gray81"))+
  geom_point(aes(size = TotalCatch_Tonnes),alpha=.5) +
  scale_colour_gradient(name="年",low = "red", high = "blue",
                        breaks = seq(1950, 2010,by=10),
                        labels = seq(1950, 2010,by=10)) +
  geom_abline(intercept = 0,slope = 1,linetype = "dotted") +
  facet_wrap(~ Country.or.Area)　+
  labs(x = "Catch_λ",y = "Value_λ",size="Total_Catch(ton)") +   
 scale_x_continuous(limits = c(Simpson_Min,Simpson_Max)) +
  scale_y_continuous(limits = c(Simpson_Min,Simpson_Max)) +
  theme( axis.title = element_text(size=15, face="bold"),
       axis.text= element_text(size=12, face="bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
       plot.title = element_text(size=15, face="bold"),
       legend.title = element_text(size=12, face="bold"),
       legend.text=element_text(face="bold"))+
  scale_radius(range=c(5/EffectA,10/EffectA))

print(p_32)
```

<img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-58-1.png" width="672" />



### Shannon

```r
source("World_Function.R")

Micronesia_Shannon_Cou <- fun_Shannon4(Micronesia,TotalCatch_Tonnes,all_price)

Micronesia_Shannon_Ave <- fun_Shannon5(Micronesia,TotalCatch_Tonnes,all_price)

#rbindするために列をそろえる、順番も変える
Micronesia_Shannon_Ave <- Micronesia_Shannon_Ave[-2]
Micronesia_Shannon_Ave <- Micronesia_Shannon_Ave[c(1,6,2,3,4,5)]

#結合する
Micronesia_Shannon <- rbind(Micronesia_Shannon_Cou,Micronesia_Shannon_Ave)

Shannon_Max <- max(Micronesia_Shannon$Shannon_Catch_Total,
                   Micronesia_Shannon$Shannon_Value_Total)

Shanon_Min <- min(Micronesia_Shannon$Shannon_Catch_Total,
                   Micronesia_Shannon$Shannon_Value_Total)


EffectA=1.5

p_33 <- ggplot(Micronesia_Shannon) +
  aes(x = Shannon_Catch_Total,y = Shannon_Value_Total,color = Year)+
  theme(text = element_text(family = "HiraKakuProN-W3"),panel.background=element_rect(fill="gray81"))+
  geom_point(aes(size = TotalCatch_Tonnes),alpha=.5) +
  scale_colour_gradient(name="年",low = "red", high = "blue",
                        breaks = seq(1950, 2010,by=10),
                        labels = seq(1950, 2010,by=10)) +
  geom_abline(intercept = 0,slope = 1,linetype = "dotted") +
  facet_wrap(~ Country.or.Area)　+
  labs(x = "Catch_H'",y = "Value_H'",size="Total_Catch(ton)") +   
 scale_x_continuous(limits = c(Shanon_Min,Shannon_Max)) +
  scale_y_continuous(limits = c(Shanon_Min,Shannon_Max)) +
  theme( axis.title = element_text(size=15, face="bold"),
       axis.text= element_text(size=12, face="bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
       plot.title = element_text(size=15, face="bold"),
       legend.title = element_text(size=12, face="bold"),
       legend.text=element_text(face="bold"))+
  scale_radius(range=c(5/EffectA,10/EffectA))

print(p_33)
```

<img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-59-1.png" width="672" />




## 		Melanesia

```r
	Melanesia <- World_Data %>% 
  filter(Sub.region.Name_koh == "Melanesia")
```


### HHI

```r
source("World_Function.R")

Melanesia_HHI_Cou <- fun_HHI5(Melanesia,TotalCatch_Tonnes,all_price)

Melanesia_HHI_Ave <- fun_HHI6(Melanesia,TotalCatch_Tonnes,all_price)

#rbindするために列をそろえる、順番も変える
Melanesia_HHI_Ave <- Melanesia_HHI_Ave[-2]
Melanesia_HHI_Ave <- Melanesia_HHI_Ave[c(1,6,2,3,4,5)]

#結合する
Melanesia_HHI <- rbind(Melanesia_HHI_Cou,Melanesia_HHI_Ave)

HHI_Max <- max(Melanesia_HHI$HHI_Catch_Total,
               Melanesia_HHI$HHI_Value_Total)
HHI_Min <- min(Melanesia_HHI$HHI_Catch_Total,
               Melanesia_HHI$HHI_Value_Total)


EffectA=1.5

p_34 <- ggplot(Melanesia_HHI) +
  aes(x = HHI_Catch_Total,y = HHI_Value_Total,color = Year) +
  theme(text = element_text(family = "HiraKakuProN-W3"),panel.background=element_rect(fill="gray81"))+
  geom_point(aes(size = TotalCatch_Tonnes),alpha=.5) +
  scale_colour_gradient(name="年",low = "red", high = "blue",
                        breaks = seq(1950, 2010,by=10),
                        labels = seq(1950, 2010,by=10)) +
  geom_abline(intercept = 0,slope = 1,linetype = "dotted") +
  facet_wrap(~ Country.or.Area)　+
  labs(x = "Catch_HHI",y = "Value_HHI",size="Total_Catch(ton)") +　
  scale_x_continuous(limits = c(HHI_Min,HHI_Max)) +　　　　　　　　　　　
  scale_y_continuous(limits = c(HHI_Min,HHI_Max)) +
  theme( axis.title = element_text(size=15, face="bold"),
       axis.text= element_text(size=12, face="bold"),
       axis.text.x = element_text(angle = 45, hjust = 1),
       plot.title = element_text(size=15, face="bold"),
       legend.title = element_text(size=12, face="bold"),
       legend.text=element_text(face="bold"))+
  scale_radius(range=c(5/EffectA,10/EffectA))


print(p_34)
```

<img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-61-1.png" width="672" />


### Simpson

```r
source("World_Function.R")

Melanesia_Simpson_Cou <- fun_Simpson4(Melanesia,TotalCatch_Tonnes,all_price)

Melanesia_Simpson_Ave <- fun_Simpson5(Melanesia,TotalCatch_Tonnes,all_price)

#rbindするために列をそろえる、順番も変える
Melanesia_Simpson_Ave <- Melanesia_Simpson_Ave[-2]
Melanesia_Simpson_Ave <- Melanesia_Simpson_Ave[c(1,6,2,3,4,5)]

#結合する
Melanesia_Simpson <- rbind(Melanesia_Simpson_Cou,Melanesia_Simpson_Ave)

Simpson_Max <- max(Melanesia_Simpson$Simpson_Catch_Total,
                   Melanesia_Simpson$Simpson_Value_Total)

Simpson_Min <- min(Melanesia_Simpson$Simpson_Catch_Total,
                   Melanesia_Simpson$Simpson_Value_Total)


EffectA=1.5

p_35 <- ggplot(Melanesia_Simpson) +
  aes(x = Simpson_Catch_Total,y = Simpson_Value_Total,color = Year)+
  theme(text = element_text(family = "HiraKakuProN-W3"),panel.background=element_rect(fill="gray81"))+
  geom_point(aes(size = TotalCatch_Tonnes),alpha=.5) +
  scale_colour_gradient(name="年",low = "red", high = "blue",
                        breaks = seq(1950, 2010,by=10),
                        labels = seq(1950, 2010,by=10)) +
  geom_abline(intercept = 0,slope = 1,linetype = "dotted") +
  facet_wrap(~ Country.or.Area)　+
  labs(x = "Catch_λ",y = "Value_λ",size="Total_Catch(ton)") +   
 scale_x_continuous(limits = c(Simpson_Min,Simpson_Max)) +
  scale_y_continuous(limits = c(Simpson_Min,Simpson_Max)) +
  theme( axis.title = element_text(size=15, face="bold"),
       axis.text= element_text(size=12, face="bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
       plot.title = element_text(size=15, face="bold"),
       legend.title = element_text(size=12, face="bold"),
       legend.text=element_text(face="bold"))+
  scale_radius(range=c(5/EffectA,10/EffectA))

print(p_35)
```

<img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-62-1.png" width="672" />



### Shannon

```r
source("World_Function.R")

Melanesia_Shannon_Cou <- fun_Shannon4(Melanesia,TotalCatch_Tonnes,all_price)

Melanesia_Shannon_Ave <- fun_Shannon5(Melanesia,TotalCatch_Tonnes,all_price)

#rbindするために列をそろえる、順番も変える
Melanesia_Shannon_Ave <- Melanesia_Shannon_Ave[-2]
Melanesia_Shannon_Ave <- Melanesia_Shannon_Ave[c(1,6,2,3,4,5)]

#結合する
Melanesia_Shannon <- rbind(Melanesia_Shannon_Cou,Melanesia_Shannon_Ave)

Shannon_Max <- max(Melanesia_Shannon$Shannon_Catch_Total,
                   Melanesia_Shannon$Shannon_Value_Total)

Shanon_Min <- min(Melanesia_Shannon$Shannon_Catch_Total,
                   Melanesia_Shannon$Shannon_Value_Total)


EffectA=1.5

p_36 <- ggplot(Melanesia_Shannon) +
  aes(x = Shannon_Catch_Total,y = Shannon_Value_Total,color = Year)+
  theme(text = element_text(family = "HiraKakuProN-W3"),panel.background=element_rect(fill="gray81"))+
  geom_point(aes(size = TotalCatch_Tonnes),alpha=.5) +
  scale_colour_gradient(name="年",low = "red", high = "blue",
                        breaks = seq(1950, 2010,by=10),
                        labels = seq(1950, 2010,by=10)) +
  geom_abline(intercept = 0,slope = 1,linetype = "dotted") +
  facet_wrap(~ Country.or.Area)　+
  labs(x = "Catch_H'",y = "Value_H'",size="Total_Catch(ton)") +   
 scale_x_continuous(limits = c(Shanon_Min,Shannon_Max)) +
  scale_y_continuous(limits = c(Shanon_Min,Shannon_Max)) +
  theme( axis.title = element_text(size=15, face="bold"),
       axis.text= element_text(size=12, face="bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
       plot.title = element_text(size=15, face="bold"),
       legend.title = element_text(size=12, face="bold"),
       legend.text=element_text(face="bold"))+
  scale_radius(range=c(5/EffectA,10/EffectA))

print(p_36)
```

<img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-63-1.png" width="672" />



## 		Polynesia

```r
	Polynesia <- World_Data %>% 
  filter(Sub.region.Name_koh == "Polynesia")
```


### HHI

```r
source("World_Function.R")

Polynesia_HHI_Cou <- fun_HHI5(Polynesia,TotalCatch_Tonnes,all_price)

Polynesia_HHI_Ave <- fun_HHI6(Polynesia,TotalCatch_Tonnes,all_price)

#rbindするために列をそろえる、順番も変える
Polynesia_HHI_Ave <- Polynesia_HHI_Ave[-2]
Polynesia_HHI_Ave <- Polynesia_HHI_Ave[c(1,6,2,3,4,5)]

#結合する
Polynesia_HHI <- rbind(Polynesia_HHI_Cou,Polynesia_HHI_Ave)

HHI_Max <- max(Polynesia_HHI$HHI_Catch_Total,
               Polynesia_HHI$HHI_Value_Total)
HHI_Min <- min(Polynesia_HHI$HHI_Catch_Total,
               Polynesia_HHI$HHI_Value_Total)


EffectA=1.5

p_37 <- ggplot(Polynesia_HHI) +
  aes(x = HHI_Catch_Total,y = HHI_Value_Total,color = Year) +
  theme(text = element_text(family = "HiraKakuProN-W3"),panel.background=element_rect(fill="gray81"))+
  geom_point(aes(size = TotalCatch_Tonnes),alpha=.5) +
  scale_colour_gradient(name="年",low = "red", high = "blue",
                        breaks = seq(1950, 2010,by=10),
                        labels = seq(1950, 2010,by=10)) +
  geom_abline(intercept = 0,slope = 1,linetype = "dotted") +
  facet_wrap(~ Country.or.Area)　+
  labs(x = "Catch_HHI",y = "Value_HHI",size="Total_Catch(ton)") +　
  scale_x_continuous(limits = c(HHI_Min,HHI_Max)) +　　　　　　　　　　　
  scale_y_continuous(limits = c(HHI_Min,HHI_Max)) +
  theme( axis.title = element_text(size=15, face="bold"),
       axis.text= element_text(size=12, face="bold"),
       axis.text.x = element_text(angle = 45, hjust = 1),
       plot.title = element_text(size=15, face="bold"),
       legend.title = element_text(size=12, face="bold"),
       legend.text=element_text(face="bold"))+
  scale_radius(range=c(5/EffectA,10/EffectA))


print(p_34)
```

<img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-65-1.png" width="672" />


### Simpson

```r
source("World_Function.R")

Polynesia_Simpson_Cou <- fun_Simpson4(Polynesia,TotalCatch_Tonnes,all_price)

Polynesia_Simpson_Ave <- fun_Simpson5(Polynesia,TotalCatch_Tonnes,all_price)

#rbindするために列をそろえる、順番も変える
Polynesia_Simpson_Ave <- Polynesia_Simpson_Ave[-2]
Polynesia_Simpson_Ave <- Polynesia_Simpson_Ave[c(1,6,2,3,4,5)]

#結合する
Polynesia_Simpson <- rbind(Polynesia_Simpson_Cou,Polynesia_Simpson_Ave)

Simpson_Max <- max(Polynesia_Simpson$Simpson_Catch_Total,
                   Polynesia_Simpson$Simpson_Value_Total)

Simpson_Min <- min(Polynesia_Simpson$Simpson_Catch_Total,
                   Polynesia_Simpson$Simpson_Value_Total)


EffectA=1.5

p_38 <- ggplot(Polynesia_Simpson) +
  aes(x = Simpson_Catch_Total,y = Simpson_Value_Total,color = Year)+
  theme(text = element_text(family = "HiraKakuProN-W3"),panel.background=element_rect(fill="gray81"))+
  geom_point(aes(size = TotalCatch_Tonnes),alpha=.5) +
  scale_colour_gradient(name="年",low = "red", high = "blue",
                        breaks = seq(1950, 2010,by=10),
                        labels = seq(1950, 2010,by=10)) +
  geom_abline(intercept = 0,slope = 1,linetype = "dotted") +
  facet_wrap(~ Country.or.Area)　+
  labs(x = "Catch_λ",y = "Value_λ",size="Total_Catch(ton)") +   
 scale_x_continuous(limits = c(Simpson_Min,Simpson_Max)) +
  scale_y_continuous(limits = c(Simpson_Min,Simpson_Max)) +
  theme( axis.title = element_text(size=15, face="bold"),
       axis.text= element_text(size=12, face="bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
       plot.title = element_text(size=15, face="bold"),
       legend.title = element_text(size=12, face="bold"),
       legend.text=element_text(face="bold"))+
  scale_radius(range=c(5/EffectA,10/EffectA))

print(p_38)
```

<img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-66-1.png" width="672" />



### Shannon

```r
source("World_Function.R")

Polynesia_Shannon_Cou <- fun_Shannon4(Polynesia,TotalCatch_Tonnes,all_price)

Polynesia_Shannon_Ave <- fun_Shannon5(Polynesia,TotalCatch_Tonnes,all_price)

#rbindするために列をそろえる、順番も変える
Polynesia_Shannon_Ave <- Polynesia_Shannon_Ave[-2]
Polynesia_Shannon_Ave <- Polynesia_Shannon_Ave[c(1,6,2,3,4,5)]

#結合する
Polynesia_Shannon <- rbind(Polynesia_Shannon_Cou,Polynesia_Shannon_Ave)

Shannon_Max <- max(Polynesia_Shannon$Shannon_Catch_Total,
                   Polynesia_Shannon$Shannon_Value_Total)

Shanon_Min <- min(Polynesia_Shannon$Shannon_Catch_Total,
                   Polynesia_Shannon$Shannon_Value_Total)


EffectA=1.5

p_39 <- ggplot(Polynesia_Shannon) +
  aes(x = Shannon_Catch_Total,y = Shannon_Value_Total,color = Year)+
  theme(text = element_text(family = "HiraKakuProN-W3"),panel.background=element_rect(fill="gray81"))+
  geom_point(aes(size = TotalCatch_Tonnes),alpha=.5) +
  scale_colour_gradient(name="年",low = "red", high = "blue",
                        breaks = seq(1950, 2010,by=10),
                        labels = seq(1950, 2010,by=10)) +
  geom_abline(intercept = 0,slope = 1,linetype = "dotted") +
  facet_wrap(~ Country.or.Area)　+
  labs(x = "Catch_H'",y = "Value_H'",size="Total_Catch(ton)") +   
 scale_x_continuous(limits = c(Shanon_Min,Shannon_Max)) +
  scale_y_continuous(limits = c(Shanon_Min,Shannon_Max)) +
  theme( axis.title = element_text(size=15, face="bold"),
       axis.text= element_text(size=12, face="bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
       plot.title = element_text(size=15, face="bold"),
       legend.title = element_text(size=12, face="bold"),
       legend.text=element_text(face="bold"))+
  scale_radius(range=c(5/EffectA,10/EffectA))

print(p_39)
```

<img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-67-1.png" width="672" />



## 		Australia and New Zealand

```r
	Australia_and_New_Zealand <- World_Data %>% 
  filter(Sub.region.Name_koh == "Australia and New Zealand")
```


### HHI

```r
source("World_Function.R")

Australia_and_New_Zealand_HHI_Cou <- fun_HHI5(Australia_and_New_Zealand,TotalCatch_Tonnes,all_price)

Australia_and_New_Zealand_HHI_Ave <- fun_HHI6(Australia_and_New_Zealand,TotalCatch_Tonnes,all_price)

#rbindするために列をそろえる、順番も変える
Australia_and_New_Zealand_HHI_Ave <- Australia_and_New_Zealand_HHI_Ave[-2]
Australia_and_New_Zealand_HHI_Ave <- Australia_and_New_Zealand_HHI_Ave[c(1,6,2,3,4,5)]

#結合する
Australia_and_New_Zealand_HHI <- rbind(Australia_and_New_Zealand_HHI_Cou,Australia_and_New_Zealand_HHI_Ave)

HHI_Max <- max(Australia_and_New_Zealand_HHI$HHI_Catch_Total,
               Australia_and_New_Zealand_HHI$HHI_Value_Total)
HHI_Min <- min(Australia_and_New_Zealand_HHI$HHI_Catch_Total,
               Australia_and_New_Zealand_HHI$HHI_Value_Total)


EffectA=1.5

p_40 <- ggplot(Australia_and_New_Zealand_HHI) +
  aes(x = HHI_Catch_Total,y = HHI_Value_Total,color = Year) +
  theme(text = element_text(family = "HiraKakuProN-W3"),panel.background=element_rect(fill="gray81"))+
  geom_point(aes(size = TotalCatch_Tonnes),alpha=.5) +
  scale_colour_gradient(name="年",low = "red", high = "blue",
                        breaks = seq(1950, 2010,by=10),
                        labels = seq(1950, 2010,by=10)) +
  geom_abline(intercept = 0,slope = 1,linetype = "dotted") +
  facet_wrap(~ Country.or.Area)　+
  labs(x = "Catch_HHI",y = "Value_HHI",size="Total_Catch(ton)") +　
  scale_x_continuous(limits = c(HHI_Min,HHI_Max)) +　　　　　　　　　　　
  scale_y_continuous(limits = c(HHI_Min,HHI_Max)) +
  theme( axis.title = element_text(size=15, face="bold"),
       axis.text= element_text(size=12, face="bold"),
       axis.text.x = element_text(angle = 45, hjust = 1),
       plot.title = element_text(size=15, face="bold"),
       legend.title = element_text(size=12, face="bold"),
       legend.text=element_text(face="bold"))+
  scale_radius(range=c(5/EffectA,10/EffectA))


print(p_40)
```

<img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-69-1.png" width="672" />


### Simpson

```r
source("World_Function.R")

Australia_and_New_Zealand_Simpson_Cou <- fun_Simpson4(Australia_and_New_Zealand,TotalCatch_Tonnes,all_price)

Australia_and_New_Zealand_Simpson_Ave <- fun_Simpson5(Australia_and_New_Zealand,TotalCatch_Tonnes,all_price)

#rbindするために列をそろえる、順番も変える
Australia_and_New_Zealand_Simpson_Ave <- Australia_and_New_Zealand_Simpson_Ave[-2]
Australia_and_New_Zealand_Simpson_Ave <- Australia_and_New_Zealand_Simpson_Ave[c(1,6,2,3,4,5)]

#結合する
Australia_and_New_Zealand_Simpson <- rbind(Australia_and_New_Zealand_Simpson_Cou,Australia_and_New_Zealand_Simpson_Ave)

Simpson_Max <- max(Australia_and_New_Zealand_Simpson$Simpson_Catch_Total,
                   Australia_and_New_Zealand_Simpson$Simpson_Value_Total)

Simpson_Min <- min(Australia_and_New_Zealand_Simpson$Simpson_Catch_Total,
                   Australia_and_New_Zealand_Simpson$Simpson_Value_Total)


EffectA=1.5

p_41 <- ggplot(Australia_and_New_Zealand_Simpson) +
  aes(x = Simpson_Catch_Total,y = Simpson_Value_Total,color = Year)+
  theme(text = element_text(family = "HiraKakuProN-W3"),panel.background=element_rect(fill="gray81"))+
  geom_point(aes(size = TotalCatch_Tonnes),alpha=.5) +
  scale_colour_gradient(name="年",low = "red", high = "blue",
                        breaks = seq(1950, 2010,by=10),
                        labels = seq(1950, 2010,by=10)) +
  geom_abline(intercept = 0,slope = 1,linetype = "dotted") +
  facet_wrap(~ Country.or.Area)　+
  labs(x = "Catch_λ",y = "Value_λ",size="Total_Catch(ton)") +   
 scale_x_continuous(limits = c(Simpson_Min,Simpson_Max)) +
  scale_y_continuous(limits = c(Simpson_Min,Simpson_Max)) +
  theme( axis.title = element_text(size=15, face="bold"),
       axis.text= element_text(size=12, face="bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
       plot.title = element_text(size=15, face="bold"),
       legend.title = element_text(size=12, face="bold"),
       legend.text=element_text(face="bold"))+
  scale_radius(range=c(5/EffectA,10/EffectA))

print(p_41)
```

<img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-70-1.png" width="672" />



### Shannon

```r
source("World_Function.R")

Australia_and_New_Zealand_Shannon_Cou <- fun_Shannon4(Australia_and_New_Zealand,TotalCatch_Tonnes,all_price)

Australia_and_New_Zealand_Shannon_Ave <- fun_Shannon5(Australia_and_New_Zealand,TotalCatch_Tonnes,all_price)

#rbindするために列をそろえる、順番も変える
Australia_and_New_Zealand_Shannon_Ave <- Australia_and_New_Zealand_Shannon_Ave[-2]
Australia_and_New_Zealand_Shannon_Ave <- Australia_and_New_Zealand_Shannon_Ave[c(1,6,2,3,4,5)]

#結合する
Australia_and_New_Zealand_Shannon <- rbind(Australia_and_New_Zealand_Shannon_Cou,Australia_and_New_Zealand_Shannon_Ave)

Shannon_Max <- max(Australia_and_New_Zealand_Shannon$Shannon_Catch_Total,
                   Australia_and_New_Zealand_Shannon$Shannon_Value_Total)

Shanon_Min <- min(Australia_and_New_Zealand_Shannon$Shannon_Catch_Total,
                   Australia_and_New_Zealand_Shannon$Shannon_Value_Total)


EffectA=1.5

p_42 <- ggplot(Australia_and_New_Zealand_Shannon) +
  aes(x = Shannon_Catch_Total,y = Shannon_Value_Total,color = Year)+
  theme(text = element_text(family = "HiraKakuProN-W3"),panel.background=element_rect(fill="gray81"))+
  geom_point(aes(size = TotalCatch_Tonnes),alpha=.5) +
  scale_colour_gradient(name="年",low = "red", high = "blue",
                        breaks = seq(1950, 2010,by=10),
                        labels = seq(1950, 2010,by=10)) +
  geom_abline(intercept = 0,slope = 1,linetype = "dotted") +
  facet_wrap(~ Country.or.Area)　+
  labs(x = "Catch_H'",y = "Value_H'",size="Total_Catch(ton)") +   
 scale_x_continuous(limits = c(Shanon_Min,Shannon_Max)) +
  scale_y_continuous(limits = c(Shanon_Min,Shannon_Max)) +
  theme( axis.title = element_text(size=15, face="bold"),
       axis.text= element_text(size=12, face="bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
       plot.title = element_text(size=15, face="bold"),
       legend.title = element_text(size=12, face="bold"),
       legend.text=element_text(face="bold"))+
  scale_radius(range=c(5/EffectA,10/EffectA))

print(p_42)
```

<img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-71-1.png" width="672" />




## 	Western Asia

```r
	Western_Asia <- World_Data %>% 
  filter(Sub.region.Name_koh == "Western Asia")
```


### HHI

```r
source("World_Function.R")

Western_Asia_HHI_Cou <- fun_HHI5(Western_Asia,TotalCatch_Tonnes,all_price)

Western_Asia_HHI_Ave <- fun_HHI6(Western_Asia,TotalCatch_Tonnes,all_price)

#rbindするために列をそろえる、順番も変える
Western_Asia_HHI_Ave <- Western_Asia_HHI_Ave[-2]
Western_Asia_HHI_Ave <- Western_Asia_HHI_Ave[c(1,6,2,3,4,5)]

#結合する
Western_Asia_HHI <- rbind(Western_Asia_HHI_Cou,Western_Asia_HHI_Ave)

HHI_Max <- max(Western_Asia_HHI$HHI_Catch_Total,
               Western_Asia_HHI$HHI_Value_Total)
HHI_Min <- min(Western_Asia_HHI$HHI_Catch_Total,
               Western_Asia_HHI$HHI_Value_Total)


EffectA=1.5

p_43 <- ggplot(Western_Asia_HHI) +
  aes(x = HHI_Catch_Total,y = HHI_Value_Total,color = Year) +
  theme(text = element_text(family = "HiraKakuProN-W3"),panel.background=element_rect(fill="gray81"))+
  geom_point(aes(size = TotalCatch_Tonnes),alpha=.5) +
  scale_colour_gradient(name="年",low = "red", high = "blue",
                        breaks = seq(1950, 2010,by=10),
                        labels = seq(1950, 2010,by=10)) +
  geom_abline(intercept = 0,slope = 1,linetype = "dotted") +
  facet_wrap(~ Country.or.Area)　+
  labs(x = "Catch_HHI",y = "Value_HHI",size="Total_Catch(ton)") +　
  scale_x_continuous(limits = c(HHI_Min,HHI_Max)) +　　　　　　　　　　　
  scale_y_continuous(limits = c(HHI_Min,HHI_Max)) +
  theme( axis.title = element_text(size=15, face="bold"),
       axis.text= element_text(size=12, face="bold"),
       axis.text.x = element_text(angle = 45, hjust = 1),
       plot.title = element_text(size=15, face="bold"),
       legend.title = element_text(size=12, face="bold"),
       legend.text=element_text(face="bold"))+
  scale_radius(range=c(5/EffectA,10/EffectA))


print(p_43)
```

<img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-73-1.png" width="672" />


### Simpson

```r
source("World_Function.R")

Western_Asia_Simpson_Cou <- fun_Simpson4(Western_Asia,TotalCatch_Tonnes,all_price)

Western_Asia_Simpson_Ave <- fun_Simpson5(Western_Asia,TotalCatch_Tonnes,all_price)

#rbindするために列をそろえる、順番も変える
Western_Asia_Simpson_Ave <- Western_Asia_Simpson_Ave[-2]
Western_Asia_Simpson_Ave <- Western_Asia_Simpson_Ave[c(1,6,2,3,4,5)]

#結合する
Western_Asia_Simpson <- rbind(Western_Asia_Simpson_Cou,Western_Asia_Simpson_Ave)

Simpson_Max <- max(Western_Asia_Simpson$Simpson_Catch_Total,
                   Western_Asia_Simpson$Simpson_Value_Total)

Simpson_Min <- min(Western_Asia_Simpson$Simpson_Catch_Total,
                   Western_Asia_Simpson$Simpson_Value_Total)


EffectA=1.5

p_44 <- ggplot(Western_Asia_Simpson) +
  aes(x = Simpson_Catch_Total,y = Simpson_Value_Total,color = Year)+
  theme(text = element_text(family = "HiraKakuProN-W3"),panel.background=element_rect(fill="gray81"))+
  geom_point(aes(size = TotalCatch_Tonnes),alpha=.5) +
  scale_colour_gradient(name="年",low = "red", high = "blue",
                        breaks = seq(1950, 2010,by=10),
                        labels = seq(1950, 2010,by=10)) +
  geom_abline(intercept = 0,slope = 1,linetype = "dotted") +
  facet_wrap(~ Country.or.Area)　+
  labs(x = "Catch_λ",y = "Value_λ",size="Total_Catch(ton)") +   
 scale_x_continuous(limits = c(Simpson_Min,Simpson_Max)) +
  scale_y_continuous(limits = c(Simpson_Min,Simpson_Max)) +
  theme( axis.title = element_text(size=15, face="bold"),
       axis.text= element_text(size=12, face="bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
       plot.title = element_text(size=15, face="bold"),
       legend.title = element_text(size=12, face="bold"),
       legend.text=element_text(face="bold"))+
  scale_radius(range=c(5/EffectA,10/EffectA))

print(p_44)
```

<img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-74-1.png" width="672" />



### Shannon

```r
source("World_Function.R")

Western_Asia_Shannon_Cou <- fun_Shannon4(Western_Asia,TotalCatch_Tonnes,all_price)

Western_Asia_Shannon_Ave <- fun_Shannon5(Western_Asia,TotalCatch_Tonnes,all_price)

#rbindするために列をそろえる、順番も変える
Western_Asia_Shannon_Ave <- Western_Asia_Shannon_Ave[-2]
Western_Asia_Shannon_Ave <- Western_Asia_Shannon_Ave[c(1,6,2,3,4,5)]

#結合する
Western_Asia_Shannon <- rbind(Western_Asia_Shannon_Cou,Western_Asia_Shannon_Ave)

Shannon_Max <- max(Western_Asia_Shannon$Shannon_Catch_Total,
                   Western_Asia_Shannon$Shannon_Value_Total)

Shanon_Min <- min(Western_Asia_Shannon$Shannon_Catch_Total,
                   Western_Asia_Shannon$Shannon_Value_Total)


EffectA=1.5

p_45 <- ggplot(Western_Asia_Shannon) +
  aes(x = Shannon_Catch_Total,y = Shannon_Value_Total,color = Year)+
  theme(text = element_text(family = "HiraKakuProN-W3"),panel.background=element_rect(fill="gray81"))+
  geom_point(aes(size = TotalCatch_Tonnes),alpha=.5) +
  scale_colour_gradient(name="年",low = "red", high = "blue",
                        breaks = seq(1950, 2010,by=10),
                        labels = seq(1950, 2010,by=10)) +
  geom_abline(intercept = 0,slope = 1,linetype = "dotted") +
  facet_wrap(~ Country.or.Area)　+
  labs(x = "Catch_H'",y = "Value_H'",size="Total_Catch(ton)") +   
 scale_x_continuous(limits = c(Shanon_Min,Shannon_Max)) +
  scale_y_continuous(limits = c(Shanon_Min,Shannon_Max)) +
  theme( axis.title = element_text(size=15, face="bold"),
       axis.text= element_text(size=12, face="bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
       plot.title = element_text(size=15, face="bold"),
       legend.title = element_text(size=12, face="bold"),
       legend.text=element_text(face="bold"))+
  scale_radius(range=c(5/EffectA,10/EffectA))

print(p_45)
```

<img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-75-1.png" width="672" />




## 	Southern Asia

```r
	Southern_Asia <- World_Data %>% 
  filter(Sub.region.Name_koh == "Southern Asia")
```


### HHI

```r
source("World_Function.R")

Southern_Asia_HHI_Cou <- fun_HHI5(Southern_Asia,TotalCatch_Tonnes,all_price)

Southern_Asia_HHI_Ave <- fun_HHI6(Southern_Asia,TotalCatch_Tonnes,all_price)

#rbindするために列をそろえる、順番も変える
Southern_Asia_HHI_Ave <- Southern_Asia_HHI_Ave[-2]
Southern_Asia_HHI_Ave <- Southern_Asia_HHI_Ave[c(1,6,2,3,4,5)]

#結合する
Southern_Asia_HHI <- rbind(Southern_Asia_HHI_Cou,Western_Asia_HHI_Ave)

HHI_Max <- max(Southern_Asia_HHI$HHI_Catch_Total,
               Southern_Asia_HHI$HHI_Value_Total)
HHI_Min <- min(Southern_Asia_HHI$HHI_Catch_Total,
               Southern_Asia_HHI$HHI_Value_Total)


EffectA=1.5

p_46 <- ggplot(Southern_Asia_HHI) +
  aes(x = HHI_Catch_Total,y = HHI_Value_Total,color = Year) +
  theme(text = element_text(family = "HiraKakuProN-W3"),panel.background=element_rect(fill="gray81"))+
  geom_point(aes(size = TotalCatch_Tonnes),alpha=.5) +
  scale_colour_gradient(name="年",low = "red", high = "blue",
                        breaks = seq(1950, 2010,by=10),
                        labels = seq(1950, 2010,by=10)) +
  geom_abline(intercept = 0,slope = 1,linetype = "dotted") +
  facet_wrap(~ Country.or.Area)　+
  labs(x = "Catch_HHI",y = "Value_HHI",size="Total_Catch(ton)") +　
  scale_x_continuous(limits = c(HHI_Min,HHI_Max)) +　　　　　　　　　　　
  scale_y_continuous(limits = c(HHI_Min,HHI_Max)) +
  theme( axis.title = element_text(size=15, face="bold"),
       axis.text= element_text(size=12, face="bold"),
       axis.text.x = element_text(angle = 45, hjust = 1),
       plot.title = element_text(size=15, face="bold"),
       legend.title = element_text(size=12, face="bold"),
       legend.text=element_text(face="bold"))+
  scale_radius(range=c(5/EffectA,10/EffectA))


print(p_46)
```

<img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-77-1.png" width="672" />


### Simpson

```r
source("World_Function.R")

Southern_Asia_Simpson_Cou <- fun_Simpson4(Southern_Asia,TotalCatch_Tonnes,all_price)

Southern_Asia_Simpson_Ave <- fun_Simpson5(Southern_Asia,TotalCatch_Tonnes,all_price)

#rbindするために列をそろえる、順番も変える
Southern_Asia_Simpson_Ave <- Southern_Asia_Simpson_Ave[-2]
Southern_Asia_Simpson_Ave <- Southern_Asia_Simpson_Ave[c(1,6,2,3,4,5)]

#結合する
Southern_Asia_Simpson <- rbind(Southern_Asia_Simpson_Cou,Southern_Asia_Simpson_Ave)

Simpson_Max <- max(Southern_Asia_Simpson$Simpson_Catch_Total,
                   Southern_Asia_Simpson$Simpson_Value_Total)

Simpson_Min <- min(Southern_Asia_Simpson$Simpson_Catch_Total,
                   Southern_Asia_Simpson$Simpson_Value_Total)


EffectA=1.5

p_47 <- ggplot(Southern_Asia_Simpson) +
  aes(x = Simpson_Catch_Total,y = Simpson_Value_Total,color = Year)+
  theme(text = element_text(family = "HiraKakuProN-W3"),panel.background=element_rect(fill="gray81"))+
  geom_point(aes(size = TotalCatch_Tonnes),alpha=.5) +
  scale_colour_gradient(name="年",low = "red", high = "blue",
                        breaks = seq(1950, 2010,by=10),
                        labels = seq(1950, 2010,by=10)) +
  geom_abline(intercept = 0,slope = 1,linetype = "dotted") +
  facet_wrap(~ Country.or.Area)　+
  labs(x = "Catch_λ",y = "Value_λ",size="Total_Catch(ton)") +   
 scale_x_continuous(limits = c(Simpson_Min,Simpson_Max)) +
  scale_y_continuous(limits = c(Simpson_Min,Simpson_Max)) +
  theme( axis.title = element_text(size=15, face="bold"),
       axis.text= element_text(size=12, face="bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
       plot.title = element_text(size=15, face="bold"),
       legend.title = element_text(size=12, face="bold"),
       legend.text=element_text(face="bold"))+
  scale_radius(range=c(5/EffectA,10/EffectA))

print(p_47)
```

<img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-78-1.png" width="672" />



### Shannon

```r
source("World_Function.R")

Southern_Asia_Shannon_Cou <- fun_Shannon4(Southern_Asia,TotalCatch_Tonnes,all_price)

Southern_Asia_Shannon_Ave <- fun_Shannon5(Southern_Asia,TotalCatch_Tonnes,all_price)

#rbindするために列をそろえる、順番も変える
Southern_Asia_Shannon_Ave <- Southern_Asia_Shannon_Ave[-2]
Southern_Asia_Shannon_Ave <- Southern_Asia_Shannon_Ave[c(1,6,2,3,4,5)]

#結合する
Southern_Asia_Shannon <- rbind(Western_Asia_Shannon_Cou,Western_Asia_Shannon_Ave)

Shannon_Max <- max(Southern_Asia_Shannon$Shannon_Catch_Total,
                   Southern_Asia_Shannon$Shannon_Value_Total)

Shanon_Min <- min(Southern_Asia_Shannon$Shannon_Catch_Total,
                   Southern_Asia_Shannon$Shannon_Value_Total)


EffectA=1.5

p_48 <- ggplot(Southern_Asia_Shannon) +
  aes(x = Shannon_Catch_Total,y = Shannon_Value_Total,color = Year)+
  theme(text = element_text(family = "HiraKakuProN-W3"),panel.background=element_rect(fill="gray81"))+
  geom_point(aes(size = TotalCatch_Tonnes),alpha=.5) +
  scale_colour_gradient(name="年",low = "red", high = "blue",
                        breaks = seq(1950, 2010,by=10),
                        labels = seq(1950, 2010,by=10)) +
  geom_abline(intercept = 0,slope = 1,linetype = "dotted") +
  facet_wrap(~ Country.or.Area)　+
  labs(x = "Catch_H'",y = "Value_H'",size="Total_Catch(ton)") +   
 scale_x_continuous(limits = c(Shanon_Min,Shannon_Max)) +
  scale_y_continuous(limits = c(Shanon_Min,Shannon_Max)) +
  theme( axis.title = element_text(size=15, face="bold"),
       axis.text= element_text(size=12, face="bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
       plot.title = element_text(size=15, face="bold"),
       legend.title = element_text(size=12, face="bold"),
       legend.text=element_text(face="bold"))+
  scale_radius(range=c(5/EffectA,10/EffectA))

print(p_48)
```

<img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-79-1.png" width="672" />



## 	South-eastern Asia

```r
	South_eastern_Asia <- World_Data %>% 
  filter(Sub.region.Name_koh == "South-eastern Asia")
```


### HHI

```r
source("World_Function.R")

South_eastern_Asia_HHI_Cou <- fun_HHI5(South_eastern_Asia,TotalCatch_Tonnes,all_price)

South_eastern_Asia_HHI_Ave <- fun_HHI6(South_eastern_Asia,TotalCatch_Tonnes,all_price)

#rbindするために列をそろえる、順番も変える
South_eastern_Asia_HHI_Ave <- South_eastern_Asia_HHI_Ave[-2]
South_eastern_Asia_HHI_Ave <- South_eastern_Asia_HHI_Ave[c(1,6,2,3,4,5)]

#結合する
South_eastern_Asia_HHI <- rbind(South_eastern_Asia_HHI_Cou,South_eastern_Asia_HHI_Ave)

HHI_Max <- max(South_eastern_Asia_HHI$HHI_Catch_Total,
               South_eastern_Asia_HHI$HHI_Value_Total)
HHI_Min <- min(South_eastern_Asia_HHI$HHI_Catch_Total,
               South_eastern_Asia_HHI$HHI_Value_Total)


EffectA=1.5

p_49 <- ggplot(South_eastern_Asia_HHI) +
  aes(x = HHI_Catch_Total,y = HHI_Value_Total,color = Year) +
  theme(text = element_text(family = "HiraKakuProN-W3"),panel.background=element_rect(fill="gray81"))+
  geom_point(aes(size = TotalCatch_Tonnes),alpha=.5) +
  scale_colour_gradient(name="年",low = "red", high = "blue",
                        breaks = seq(1950, 2010,by=10),
                        labels = seq(1950, 2010,by=10)) +
  geom_abline(intercept = 0,slope = 1,linetype = "dotted") +
  facet_wrap(~ Country.or.Area)　+
  labs(x = "Catch_HHI",y = "Value_HHI",size="Total_Catch(ton)") +　
  scale_x_continuous(limits = c(HHI_Min,HHI_Max)) +　　　　　　　　　　　
  scale_y_continuous(limits = c(HHI_Min,HHI_Max)) +
  theme( axis.title = element_text(size=15, face="bold"),
       axis.text= element_text(size=12, face="bold"),
       axis.text.x = element_text(angle = 45, hjust = 1),
       plot.title = element_text(size=15, face="bold"),
       legend.title = element_text(size=12, face="bold"),
       legend.text=element_text(face="bold"))+
  scale_radius(range=c(5/EffectA,10/EffectA))


print(p_49)
```

<img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-81-1.png" width="672" />


### Simpson

```r
source("World_Function.R")

South_eastern_Asia_Simpson_Cou <- fun_Simpson4(South_eastern_Asia,TotalCatch_Tonnes,all_price)

South_eastern_Asia_Simpson_Ave <- fun_Simpson5(South_eastern_Asia,TotalCatch_Tonnes,all_price)

#rbindするために列をそろえる、順番も変える
South_eastern_Asia_Simpson_Ave <- South_eastern_Asia_Simpson_Ave[-2]
South_eastern_Asia_Simpson_Ave <- South_eastern_Asia_Simpson_Ave[c(1,6,2,3,4,5)]

#結合する
South_eastern_Asia_Simpson <- rbind(South_eastern_Asia_Simpson_Cou,South_eastern_Asia_Simpson_Ave)

Simpson_Max <- max(South_eastern_Asia_Simpson$Simpson_Catch_Total,
                   South_eastern_Asia_Simpson$Simpson_Value_Total)

Simpson_Min <- min(South_eastern_Asia_Simpson$Simpson_Catch_Total,
                   South_eastern_Asia_Simpson$Simpson_Value_Total)


EffectA=1.5

p_50 <- ggplot(South_eastern_Asia_Simpson) +
  aes(x = Simpson_Catch_Total,y = Simpson_Value_Total,color = Year)+
  theme(text = element_text(family = "HiraKakuProN-W3"),panel.background=element_rect(fill="gray81"))+
  geom_point(aes(size = TotalCatch_Tonnes),alpha=.5) +
  scale_colour_gradient(name="年",low = "red", high = "blue",
                        breaks = seq(1950, 2010,by=10),
                        labels = seq(1950, 2010,by=10)) +
  geom_abline(intercept = 0,slope = 1,linetype = "dotted") +
  facet_wrap(~ Country.or.Area)　+
  labs(x = "Catch_λ",y = "Value_λ",size="Total_Catch(ton)") +   
 scale_x_continuous(limits = c(Simpson_Min,Simpson_Max)) +
  scale_y_continuous(limits = c(Simpson_Min,Simpson_Max)) +
  theme( axis.title = element_text(size=15, face="bold"),
       axis.text= element_text(size=12, face="bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
       plot.title = element_text(size=15, face="bold"),
       legend.title = element_text(size=12, face="bold"),
       legend.text=element_text(face="bold"))+
  scale_radius(range=c(5/EffectA,10/EffectA))

print(p_50)
```

<img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-82-1.png" width="672" />



### Shannon

```r
source("World_Function.R")

South_eastern_Asia_Shannon_Cou <- fun_Shannon4(South_eastern_Asia,TotalCatch_Tonnes,all_price)

South_eastern_Asia_Shannon_Ave <- fun_Shannon5(South_eastern_Asia,TotalCatch_Tonnes,all_price)

#rbindするために列をそろえる、順番も変える
South_eastern_Asia_Shannon_Ave <- South_eastern_Asia_Shannon_Ave[-2]
South_eastern_Asia_Shannon_Ave <- South_eastern_Asia_Shannon_Ave[c(1,6,2,3,4,5)]

#結合する
South_eastern_Asia_Shannon <- rbind(South_eastern_Asia_Shannon_Cou,South_eastern_Asia_Shannon_Ave)

Shannon_Max <- max(South_eastern_Asia_Shannon$Shannon_Catch_Total,
                   South_eastern_Asia_Shannon$Shannon_Value_Total)

Shanon_Min <- min(South_eastern_Asia_Shannon$Shannon_Catch_Total,
                   South_eastern_Asia_Shannon$Shannon_Value_Total)


EffectA=1.5

p_51 <- ggplot(South_eastern_Asia_Shannon) +
  aes(x = Shannon_Catch_Total,y = Shannon_Value_Total,color = Year)+
  theme(text = element_text(family = "HiraKakuProN-W3"),panel.background=element_rect(fill="gray81"))+
  geom_point(aes(size = TotalCatch_Tonnes),alpha=.5) +
  scale_colour_gradient(name="年",low = "red", high = "blue",
                        breaks = seq(1950, 2010,by=10),
                        labels = seq(1950, 2010,by=10)) +
  geom_abline(intercept = 0,slope = 1,linetype = "dotted") +
  facet_wrap(~ Country.or.Area)　+
  labs(x = "Catch_H'",y = "Value_H'",size="Total_Catch(ton)") +   
 scale_x_continuous(limits = c(Shanon_Min,Shannon_Max)) +
  scale_y_continuous(limits = c(Shanon_Min,Shannon_Max)) +
  theme( axis.title = element_text(size=15, face="bold"),
       axis.text= element_text(size=12, face="bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
       plot.title = element_text(size=15, face="bold"),
       legend.title = element_text(size=12, face="bold"),
       legend.text=element_text(face="bold"))+
  scale_radius(range=c(5/EffectA,10/EffectA))

print(p_51)
```

<img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-83-1.png" width="672" />



## 	Eastern Asia

```r
	Eastern_Asia <- World_Data %>% 
  filter(Sub.region.Name_koh == "Eastern Asia")
```


### HHI

```r
source("World_Function.R")

Eastern_Asia_HHI_Cou <- fun_HHI5(Eastern_Asia,TotalCatch_Tonnes,all_price)

Eastern_Asia_HHI_Ave <- fun_HHI6(Eastern_Asia,TotalCatch_Tonnes,all_price)

#rbindするために列をそろえる、順番も変える
Eastern_Asia_HHI_Ave <- Eastern_Asia_HHI_Ave[-2]
Eastern_Asia_HHI_Ave <- Eastern_Asia_HHI_Ave[c(1,6,2,3,4,5)]

#結合する
Eastern_Asia_HHI <- rbind(Eastern_Asia_HHI_Cou,Eastern_Asia_HHI_Ave)

HHI_Max <- max(Eastern_Asia_HHI$HHI_Catch_Total,
               Eastern_Asia_HHI$HHI_Value_Total)
HHI_Min <- min(Eastern_Asia_HHI$HHI_Catch_Total,
               Eastern_Asia_HHI$HHI_Value_Total)


EffectA=1.5

p_52 <- ggplot(Eastern_Asia_HHI) +
  aes(x = HHI_Catch_Total,y = HHI_Value_Total,color = Year) +
  theme(text = element_text(family = "HiraKakuProN-W3"),panel.background=element_rect(fill="gray81"))+
  geom_point(aes(size = TotalCatch_Tonnes),alpha=.5) +
  scale_colour_gradient(name="年",low = "red", high = "blue",
                        breaks = seq(1950, 2010,by=10),
                        labels = seq(1950, 2010,by=10)) +
  geom_abline(intercept = 0,slope = 1,linetype = "dotted") +
  facet_wrap(~ Country.or.Area)　+
  labs(x = "Catch_HHI",y = "Value_HHI",size="Total_Catch(ton)") +　
  scale_x_continuous(limits = c(HHI_Min,HHI_Max)) +　　　　　　　　　　　
  scale_y_continuous(limits = c(HHI_Min,HHI_Max)) +
  theme( axis.title = element_text(size=15, face="bold"),
       axis.text= element_text(size=12, face="bold"),
       axis.text.x = element_text(angle = 45, hjust = 1),
       plot.title = element_text(size=15, face="bold"),
       legend.title = element_text(size=12, face="bold"),
       legend.text=element_text(face="bold"))+
  scale_radius(range=c(5/EffectA,10/EffectA))


print(p_52)
```

<img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-85-1.png" width="672" />


### Simpson

```r
source("World_Function.R")

Eastern_Asia_Simpson_Cou <- fun_Simpson4(Eastern_Asia,TotalCatch_Tonnes,all_price)

Eastern_Asia_Simpson_Ave <- fun_Simpson5(Eastern_Asia,TotalCatch_Tonnes,all_price)

#rbindするために列をそろえる、順番も変える
Eastern_Asia_Simpson_Ave <- Eastern_Asia_Simpson_Ave[-2]
Eastern_Asia_Simpson_Ave <- Eastern_Asia_Simpson_Ave[c(1,6,2,3,4,5)]

#結合する
Eastern_Asia_Simpson <- rbind(Eastern_Asia_Simpson_Cou,Eastern_Asia_Simpson_Ave)

Simpson_Max <- max(Eastern_Asia_Simpson$Simpson_Catch_Total,
                   Eastern_Asia_Simpson$Simpson_Value_Total)

Simpson_Min <- min(Eastern_Asia_Simpson$Simpson_Catch_Total,
                   Eastern_Asia_Simpson$Simpson_Value_Total)


EffectA=1.5

p_53 <- ggplot(Eastern_Asia_Simpson) +
  aes(x = Simpson_Catch_Total,y = Simpson_Value_Total,color = Year)+
  theme(text = element_text(family = "HiraKakuProN-W3"),panel.background=element_rect(fill="gray81"))+
  geom_point(aes(size = TotalCatch_Tonnes),alpha=.5) +
  scale_colour_gradient(name="年",low = "red", high = "blue",
                        breaks = seq(1950, 2010,by=10),
                        labels = seq(1950, 2010,by=10)) +
  geom_abline(intercept = 0,slope = 1,linetype = "dotted") +
  facet_wrap(~ Country.or.Area)　+
  labs(x = "Catch_λ",y = "Value_λ",size="Total_Catch(ton)") +   
 scale_x_continuous(limits = c(Simpson_Min,Simpson_Max)) +
  scale_y_continuous(limits = c(Simpson_Min,Simpson_Max)) +
  theme( axis.title = element_text(size=15, face="bold"),
       axis.text= element_text(size=12, face="bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
       plot.title = element_text(size=15, face="bold"),
       legend.title = element_text(size=12, face="bold"),
       legend.text=element_text(face="bold"))+
  scale_radius(range=c(5/EffectA,10/EffectA))

print(p_53)
```

<img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-86-1.png" width="672" />



### Shannon

```r
source("World_Function.R")

Eastern_Asia_Shannon_Cou <- fun_Shannon4(Eastern_Asia,TotalCatch_Tonnes,all_price)

Eastern_Asia_Shannon_Ave <- fun_Shannon5(Eastern_Asia,TotalCatch_Tonnes,all_price)

#rbindするために列をそろえる、順番も変える
Eastern_Asia_Shannon_Ave <- Eastern_Asia_Shannon_Ave[-2]
Eastern_Asia_Shannon_Ave <- Eastern_Asia_Shannon_Ave[c(1,6,2,3,4,5)]

#結合する
Eastern_Asia_Shannon <- rbind(Eastern_Asia_Shannon_Cou,Eastern_Asia_Shannon_Ave)

Shannon_Max <- max(Eastern_Asia_Shannon$Shannon_Catch_Total,
                   Eastern_Asia_Shannon$Shannon_Value_Total)

Shanon_Min <- min(Eastern_Asia_Shannon$Shannon_Catch_Total,
                   Eastern_Asia_Shannon$Shannon_Value_Total)


EffectA=1.5

p_54 <- ggplot(Eastern_Asia_Shannon) +
  aes(x = Shannon_Catch_Total,y = Shannon_Value_Total,color = Year)+
  theme(text = element_text(family = "HiraKakuProN-W3"),panel.background=element_rect(fill="gray81"))+
  geom_point(aes(size = TotalCatch_Tonnes),alpha=.5) +
  scale_colour_gradient(name="年",low = "red", high = "blue",
                        breaks = seq(1950, 2010,by=10),
                        labels = seq(1950, 2010,by=10)) +
  geom_abline(intercept = 0,slope = 1,linetype = "dotted") +
  facet_wrap(~ Country.or.Area)　+
  labs(x = "Catch_H'",y = "Value_H'",size="Total_Catch(ton)") +   
 scale_x_continuous(limits = c(Shanon_Min,Shannon_Max)) +
  scale_y_continuous(limits = c(Shanon_Min,Shannon_Max)) +
  theme( axis.title = element_text(size=15, face="bold"),
       axis.text= element_text(size=12, face="bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
       plot.title = element_text(size=15, face="bold"),
       legend.title = element_text(size=12, face="bold"),
       legend.text=element_text(face="bold"))+
  scale_radius(range=c(5/EffectA,10/EffectA))

print(p_54)
```

<img src="World_Data_PortofolioVer1.4_koh_files/figure-html/unnamed-chunk-87-1.png" width="672" />












