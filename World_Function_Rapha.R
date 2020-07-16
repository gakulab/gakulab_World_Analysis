## 各年の漁獲量・漁獲高
fun_Catch_Value <- function(data,TotalCatch_Tonnes,Total_Value){
  data %>% 
    group_by_(.dots = lazyeval::lazy_dots(Year)) %>% 
    summarise(TotalCatch_Tonnes = sum(TotalCatch_Tonnes)/1000000,
              Total_Value = sum(Total_Value)/1000000) %>%
    arrange(Year)
}



## 各国の漁獲量・漁獲高の合計
fun_Catch_Value2 <- function(data,TotalCatch_Tonnes,Total_Value){
  data %>% 
    group_by_(.dots = lazyeval::lazy_dots(Country.or.Area)) %>% 
    summarise(TotalCatch_Tonnes = sum(TotalCatch_Tonnes)/1000000,
              Total_Value = sum(Total_Value)/1000000) %>%
    mutate(Catch_Ranking = min_rank(desc(TotalCatch_Tonnes)),
           Value_Ranking = min_rank(desc(Total_Value))) %>% 
    arrange(Country.or.Area)
}





##  年間漁獲魚種構成
#漁獲量上位１０魚種の漁獲量・漁獲高における漁獲構成
fun_Fish_Rate <- function(data,Catch,Value){
  data %>% 
    group_by_(.dots = lazyeval::lazy_dots(Year,FishName_EN)) %>% 
    summarise(Catch = ((sum(Catch))/1000),
              Value = (sum(Value))/10000000) %>%
    mutate(Percentage_Catch = (Catch/sum(Catch))*100,
           Percentage_Value = (Value/sum(Value))*100,
           UnitPrice = Value/Catch) %>%
    arrange(Year)
}

#漁獲高上位１０魚種の漁獲量・漁獲高における漁獲構成
fun_Fish_Rate2 <- function(data,Catch,Value){
  data %>% 
    group_by_(.dots = lazyeval::lazy_dots(Year,FishName_EN)) %>% 
    summarise(Catch = ((sum(Catch))/1000),
              Value = (sum(Value))/10000000) %>%
    mutate(Percentage_Catch = (Catch/sum(Catch))*100,
           Percentage_Value = (Value/sum(Value))*100,
           UnitPrice = Value/Catch) %>%
    arrange(Year)
}



#漁獲高上位１０魚種の漁獲量・漁獲高における漁獲構成
fun_Fish_Rate4 <- function(data,Catch,Value){
  data %>% 
    group_by_(.dots = lazyeval::lazy_dots(Month,FishName_EN)) %>% 
    summarise(Catch = ((sum(Catch))/1000),
              Value = (sum(Value))/10000000) %>%
    mutate(Percentage_Catch = (Catch/sum(Catch))*100,
           Percentage_Value = (Value/sum(Value))*100,
           UnitPrice = Value/Catch) %>%
    arrange(Month)
}

##  HHI
#年ごとのHHI
fun_HHI <- function(data,TotalCatch_Tonnes,Total_Value){
  data %>% 
    group_by_(.dots = lazyeval::lazy_dots(Year,TaxonKey)) %>% 
    summarise(TotalCatch_Tonnes = sum(TotalCatch_Tonnes)/1000000,
              Total_Value = sum(Total_Value)/1000000) %>%
    mutate(Percentage_Catch = (TotalCatch_Tonnes/sum(TotalCatch_Tonnes))*100,
           Percentage_Value = (Total_Value/sum(Total_Value))*100) %>%
    mutate(HHI_Catch = Percentage_Catch^2,
           HHI_Value = Percentage_Value^2) %>%
    ungroup() %>% 
    group_by_(.dots = lazyeval::lazy_dots(Year)) %>% 
    summarise(HHI_Catch_Total = sum(HHI_Catch),
              HHI_Value_Total = sum(HHI_Value),
              TotalCatch_Tonnes = sum(TotalCatch_Tonnes),
              Total_Value = sum(Total_Value)) %>%
    mutate(Percentage_Catch = (TotalCatch_Tonnes/sum(TotalCatch_Tonnes))*100,
           Percentage_Value = (Total_Value/sum(Total_Value))*100) %>%
    mutate(FisheryType = "World_All") %>% 
    arrange(Year)
}



## 3-7 シンプソン指数λ
#年ごとのλ
fun_Simpson <- function(data,TotalCatch_Tonnes,Total_Value){
  data %>% 
    group_by_(.dots = lazyeval::lazy_dots(Year,TaxonKey)) %>% 
    summarise(TotalCatch_Tonnes = sum(TotalCatch_Tonnes)/1000000,
              Total_Value = sum(Total_Value)/1000000) %>%
    mutate(Percentage_Catch = (TotalCatch_Tonnes/sum(TotalCatch_Tonnes)),
           Percentage_Value = (Total_Value/sum(Total_Value))) %>%
    mutate(Simpson_Catch = Percentage_Catch^2,
           Simpson_Value = Percentage_Value^2) %>%
    ungroup() %>% 
    group_by_(.dots = lazyeval::lazy_dots(Year)) %>% 
    summarise(Simpson_Catch_Total = 1/sum(Simpson_Catch),
              Simpson_Value_Total = 1/sum(Simpson_Value),
              TotalCatch_Tonnes = sum(TotalCatch_Tonnes),
              Total_Value = sum(Total_Value)) %>% 
    mutate(Percentage_Catch = (TotalCatch_Tonnes/sum(TotalCatch_Tonnes))*100,
           Percentage_Value = (Total_Value/sum(Total_Value))*100) %>%
    mutate(FisheryType = "World_All") %>% 
    arrange(Year)
}



## 3-8 シャノン・ウィナー指数H'
#年ごとのH'
fun_Shannon <- function(data,TotalCatch_Tonnes,Total_Value){
  data %>% 
    group_by_(.dots = lazyeval::lazy_dots(Year,TaxonKey)) %>% 
    summarise(TotalCatch_Tonnes = sum(TotalCatch_Tonnes)/1000000,
              Total_Value = sum(Total_Value)/1000000) %>%
    mutate(Percentage_Catch = (TotalCatch_Tonnes/sum(TotalCatch_Tonnes)),
           Percentage_Value = (Total_Value/sum(Total_Value))) %>%
    mutate(Shannon_Catch = (Percentage_Catch*log(Percentage_Catch)),
           Shannon_Value = (Percentage_Value*log(Percentage_Value))) %>%
    ungroup() %>% 
    group_by_(.dots = lazyeval::lazy_dots(Year)) %>% 
    summarise(Shannon_Catch_Total = exp(-sum(Shannon_Catch)),
              Shannon_Value_Total = exp(-sum(Shannon_Value)),
              TotalCatch_Tonnes = sum(TotalCatch_Tonnes),
              Total_Value = sum(Total_Value)) %>%
    mutate(Percentage_Catch = (TotalCatch_Tonnes/sum(TotalCatch_Tonnes))*100,
           Percentage_Value = (Total_Value/sum(Total_Value))*100) %>%
    mutate(FisheryType = "World_All") %>% 
    arrange(Year)
}




#国ごとのHHI
fun_HHI2 <- function(data,TotalCatch_Tonnes,Total_Value){
  data %>% 
    group_by_(.dots = lazyeval::lazy_dots(Year,FishingEntityID,TaxonKey)) %>% 
    summarise(TotalCatch_Tonnes = sum(TotalCatch_Tonnes)/1000000,
              Total_Value = sum(Total_Value)/1000000) %>%
    mutate(Percentage_Catch = (TotalCatch_Tonnes/sum(TotalCatch_Tonnes))*100,
           Percentage_Value = (Total_Value/sum(Total_Value))*100) %>%
    mutate(HHI_Catch = Percentage_Catch^2,
           HHI_Value = Percentage_Value^2) %>%
    ungroup() %>% 
    group_by_(.dots = lazyeval::lazy_dots(Year,FishingEntityID)) %>% 
    summarise(HHI_Catch_Total = sum(HHI_Catch),
              HHI_Value_Total = sum(HHI_Value),
              TotalCatch_Tonnes = sum(TotalCatch_Tonnes),
              Total_Value = sum(Total_Value)) %>%
    mutate(Percentage_Catch = (TotalCatch_Tonnes/sum(TotalCatch_Tonnes))*100,
           Percentage_Value = (Total_Value/sum(Total_Value))*100) %>%
    arrange(FishingEntityID)
}


#国ごとのHHI
fun_HHI3 <- function(data,TotalCatch_Tonnes,Total_Value){
  data %>% 
    group_by_(.dots = lazyeval::lazy_dots(Year,Country.or.Area,TaxonKey)) %>% 
    summarise(TotalCatch_Tonnes = sum(TotalCatch_Tonnes)/1000000,
              Total_Value = sum(Total_Value)/1000000) %>%
    mutate(Percentage_Catch = (TotalCatch_Tonnes/sum(TotalCatch_Tonnes))*100,
           Percentage_Value = (Total_Value/sum(Total_Value))*100) %>%
    mutate(HHI_Catch = Percentage_Catch^2,
           HHI_Value = Percentage_Value^2) %>%
    ungroup() %>% 
    group_by_(.dots = lazyeval::lazy_dots(Year,Country.or.Area)) %>% 
    summarise(HHI_Catch_Total = sum(HHI_Catch),
              HHI_Value_Total = sum(HHI_Value),
              TotalCatch_Tonnes = sum(TotalCatch_Tonnes),
              Total_Value = sum(Total_Value)) %>%
    mutate(Percentage_Catch = (TotalCatch_Tonnes/sum(TotalCatch_Tonnes))*100,
           Percentage_Value = (Total_Value/sum(Total_Value))*100) %>%
    arrange(Country.or.Area)
}


#国ごとのλ
fun_Simpson2 <- function(data,TotalCatch_Tonnes,Total_Value){
  data %>% 
    group_by_(.dots = lazyeval::lazy_dots(Year,Country.or.Area,TaxonKey)) %>% 
    summarise(TotalCatch_Tonnes = sum(TotalCatch_Tonnes)/1000000,
              Total_Value = sum(Total_Value)/1000000) %>%
    mutate(Percentage_Catch = (TotalCatch_Tonnes/sum(TotalCatch_Tonnes)),
           Percentage_Value = (Total_Value/sum(Total_Value))) %>%
    mutate(Simpson_Catch = Percentage_Catch^2,
           Simpson_Value = Percentage_Value^2) %>%
    ungroup() %>% 
    group_by_(.dots = lazyeval::lazy_dots(Year,Country.or.Area)) %>% 
    summarise(Simpson_Catch_Total = 1/sum(Simpson_Catch),
              Simpson_Value_Total = 1/sum(Simpson_Value),
              TotalCatch_Tonnes = sum(TotalCatch_Tonnes),
              Total_Value = sum(Total_Value)) %>% 
    mutate(Percentage_Catch = (TotalCatch_Tonnes/sum(TotalCatch_Tonnes))*100,
           Percentage_Value = (Total_Value/sum(Total_Value))*100) %>%
    arrange(Country.or.Area)
}


#国ごとのH’
fun_Shannon2 <- function(data,TotalCatch_Tonnes,Total_Value){
  data %>% 
    group_by_(.dots = lazyeval::lazy_dots(Year,Country.or.Area,TaxonKey)) %>% 
    summarise(TotalCatch_Tonnes = sum(TotalCatch_Tonnes)/1000000,
              Total_Value = sum(Total_Value)/1000000) %>%
    mutate(Percentage_Catch = (TotalCatch_Tonnes/sum(TotalCatch_Tonnes)),
           Percentage_Value = (Total_Value/sum(Total_Value))) %>%
    mutate(Shannon_Catch = (Percentage_Catch*log(Percentage_Catch)),
           Shannon_Value = (Percentage_Value*log(Percentage_Value))) %>%
    ungroup() %>% 
    group_by_(.dots = lazyeval::lazy_dots(Year,Country.or.Area)) %>% 
    summarise(Shannon_Catch_Total = exp(-sum(Shannon_Catch)),
              Shannon_Value_Total = exp(-sum(Shannon_Value)),
              TotalCatch_Tonnes = sum(TotalCatch_Tonnes),
              Total_Value = sum(Total_Value)) %>%
    mutate(Percentage_Catch = (TotalCatch_Tonnes/sum(TotalCatch_Tonnes))*100,
           Percentage_Value = (Total_Value/sum(Total_Value))*100) %>%
    arrange(Country.or.Area)
}



#地域ごとのHHI
fun_HHI4 <- function(data,TotalCatch_Tonnes,Total_Value){
  data %>% 
    group_by_(.dots = lazyeval::lazy_dots(Year,Region.Name_koh,TaxonKey)) %>% 
    summarise(TotalCatch_Tonnes = sum(TotalCatch_Tonnes)/1000000,
              Total_Value = sum(Total_Value)/1000000) %>%
    mutate(Percentage_Catch = (TotalCatch_Tonnes/sum(TotalCatch_Tonnes))*100,
           Percentage_Value = (Total_Value/sum(Total_Value))*100) %>%
    mutate(HHI_Catch = Percentage_Catch^2,
           HHI_Value = Percentage_Value^2) %>%
    ungroup() %>% 
    group_by_(.dots = lazyeval::lazy_dots(Year,Region.Name_koh)) %>% 
    summarise(HHI_Catch_Total = sum(HHI_Catch),
              HHI_Value_Total = sum(HHI_Value),
              TotalCatch_Tonnes = sum(TotalCatch_Tonnes),
              Total_Value = sum(Total_Value)) %>%
    mutate(Percentage_Catch = (TotalCatch_Tonnes/sum(TotalCatch_Tonnes))*100,
           Percentage_Value = (Total_Value/sum(Total_Value))*100) %>%
    na.omit() %>% 
    arrange(Region.Name_koh)
}


#地域ごとのλ
fun_Simpson3 <- function(data,TotalCatch_Tonnes,Total_Value){
  data %>% 
    group_by(.dots = lazyeval::lazy_dots(Year,Region.Name_koh,TaxonKey)) %>% 
    summarise(TotalCatch_Tonnes = sum(TotalCatch_Tonnes)/1000000,
              Total_Value = sum(Total_Value)/1000000) %>%
    mutate(Percentage_Catch = (TotalCatch_Tonnes/sum(TotalCatch_Tonnes)),
           Percentage_Value = (Total_Value/sum(Total_Value))) %>%
    mutate(Simpson_Catch = Percentage_Catch^2,
           Simpson_Value = Percentage_Value^2) %>%
    ungroup() %>% 
    group_by(.dots = lazyeval::lazy_dots(Year,Region.Name_koh)) %>% 
    summarise(Simpson_Catch_Total = 1/sum(Simpson_Catch),
              Simpson_Value_Total = 1/sum(Simpson_Value),
              TotalCatch_Tonnes = sum(TotalCatch_Tonnes),
              Total_Value = sum(Total_Value)) %>% 
    mutate(Percentage_Catch = (TotalCatch_Tonnes/sum(TotalCatch_Tonnes))*100,
           Percentage_Value = (Total_Value/sum(Total_Value))*100) %>%
    na.omit() %>% 
    arrange(Region.Name_koh)
}


#地域ごとのH’
fun_Shannon3 <- function(data,TotalCatch_Tonnes,Total_Value){
  data %>% 
    group_by_(.dots = lazyeval::lazy_dots(Year,Region.Name_koh,TaxonKey)) %>% 
    summarise(TotalCatch_Tonnes = sum(TotalCatch_Tonnes)/1000000,
              Total_Value = sum(Total_Value)/1000000) %>%
    mutate(Percentage_Catch = (TotalCatch_Tonnes/sum(TotalCatch_Tonnes)),
           Percentage_Value = (Total_Value/sum(Total_Value))) %>%
    mutate(Shannon_Catch = (Percentage_Catch*log(Percentage_Catch)),
           Shannon_Value = (Percentage_Value*log(Percentage_Value))) %>%
    ungroup() %>% 
    group_by_(.dots = lazyeval::lazy_dots(Year,Region.Name_koh)) %>% 
    summarise(Shannon_Catch_Total = exp(-sum(Shannon_Catch)),
              Shannon_Value_Total = exp(-sum(Shannon_Value)),
              TotalCatch_Tonnes = sum(TotalCatch_Tonnes),
              Total_Value = sum(Total_Value)) %>%
    mutate(Percentage_Catch = (TotalCatch_Tonnes/sum(TotalCatch_Tonnes))*100,
           Percentage_Value = (Total_Value/sum(Total_Value))*100) %>%
    na.omit() %>% 
    arrange(Region.Name_koh)
}



#地域(サブ)ごとのHHI
fun_HHI5 <- function(data,TotalCatch_Tonnes,Total_Value){
  data %>% 
    group_by_(.dots = lazyeval::lazy_dots(Year,Country.or.Area,TaxonKey)) %>% 
    summarise(TotalCatch_Tonnes = sum(TotalCatch_Tonnes)/1000000,
              Total_Value = sum(Total_Value)/1000000) %>%
    mutate(Percentage_Catch = (TotalCatch_Tonnes/sum(TotalCatch_Tonnes))*100,
           Percentage_Value = (Total_Value/sum(Total_Value))*100) %>%
    mutate(HHI_Catch = Percentage_Catch^2,
           HHI_Value = Percentage_Value^2) %>%
    ungroup() %>% 
    group_by_(.dots = lazyeval::lazy_dots(Year,Country.or.Area)) %>% 
    summarise(HHI_Catch_Total = sum(HHI_Catch),
              HHI_Value_Total = sum(HHI_Value),
              TotalCatch_Tonnes = sum(TotalCatch_Tonnes),
              Total_Value = sum(Total_Value)) %>%
    arrange(Country.or.Area)
}


#地域(サブ)平均のHHI
fun_HHI6 <- function(data,TotalCatch_Tonnes,Total_Value){
  data %>% 
    group_by_(.dots = lazyeval::lazy_dots(Year,Sub.region.Name_koh,Country.or.Area,TaxonKey)) %>% 
    summarise(TotalCatch_Tonnes = sum(TotalCatch_Tonnes)/1000000,
              Total_Value = sum(Total_Value)/1000000) %>%
    mutate(Percentage_Catch = (TotalCatch_Tonnes/sum(TotalCatch_Tonnes))*100,
           Percentage_Value = (Total_Value/sum(Total_Value))*100) %>%
    mutate(HHI_Catch = Percentage_Catch^2,
           HHI_Value = Percentage_Value^2) %>%
    ungroup() %>% 
    group_by_(.dots = lazyeval::lazy_dots(Year,Sub.region.Name_koh,Country.or.Area)) %>% 
    summarise(HHI_Catch_Total = sum(HHI_Catch),
               HHI_Value_Total = sum(HHI_Value),
              TotalCatch_Tonnes = sum(TotalCatch_Tonnes),
              Total_Value = sum(Total_Value)) %>% 
  ungroup() %>% 
    group_by_(.dots = lazyeval::lazy_dots(Year,Sub.region.Name_koh)) %>% 
    summarise(HHI_Catch_Total = mean(HHI_Catch_Total),
              HHI_Value_Total = mean(HHI_Value_Total),
              TotalCatch_Tonnes = sum(TotalCatch_Tonnes),
              Total_Value = sum(Total_Value)) %>%
    mutate(Country.or.Area = "Average")
}

#地域(サブ)TotalのHHI
fun_HHI7 <- function(data,TotalCatch_Tonnes,Total_Value){
  data %>% 
    group_by_(.dots = lazyeval::lazy_dots(Year,TaxonKey)) %>% 
    summarise(TotalCatch_Tonnes = sum(TotalCatch_Tonnes)/1000000,
              Total_Value = sum(Total_Value)/1000000) %>%
    mutate(Percentage_Catch = (TotalCatch_Tonnes/sum(TotalCatch_Tonnes))*100,
           Percentage_Value = (Total_Value/sum(Total_Value))*100) %>%
    mutate(HHI_Catch = Percentage_Catch^2,
           HHI_Value = Percentage_Value^2) %>%
    ungroup() %>% 
    group_by_(.dots = lazyeval::lazy_dots(Year)) %>% 
    summarise(HHI_Catch_Total = sum(HHI_Catch),
              HHI_Value_Total = sum(HHI_Value),
              TotalCatch_Tonnes = sum(TotalCatch_Tonnes),
              Total_Value = sum(Total_Value)) %>% 
    mutate(Country.or.Area = "Total")
}


#地域(サブ)加重平均のHHI
fun_HHI8 <- function(data,TotalCatch_Tonnes,Total_Value){
  data %>% 
    group_by_(.dots = lazyeval::lazy_dots(Year,Sub.region.Name_koh,Country.or.Area,TaxonKey)) %>% 
    summarise(TotalCatch_Tonnes = sum(TotalCatch_Tonnes)/1000000,
              Total_Value = sum(Total_Value)/1000000) %>%
    mutate(Percentage_Catch = (TotalCatch_Tonnes/sum(TotalCatch_Tonnes))*100,
           Percentage_Value = (Total_Value/sum(Total_Value))*100) %>%
    mutate(HHI_Catch = Percentage_Catch^2,
           HHI_Value = Percentage_Value^2) %>%
    ungroup() %>% 
    group_by_(.dots = lazyeval::lazy_dots(Year,Sub.region.Name_koh,Country.or.Area)) %>% 
    summarise(HHI_Catch_Total = sum(HHI_Catch),
              HHI_Value_Total = sum(HHI_Value),
              TotalCatch_Tonnes = sum(TotalCatch_Tonnes),
              Total_Value = sum(Total_Value)) %>% 
    ungroup() %>% 
    group_by_(.dots = lazyeval::lazy_dots(Year,Sub.region.Name_koh)) %>% 
    summarise(HHI_Catch_Total = weighted.mean(HHI_Catch_Total,TotalCatch_Tonnes),
              HHI_Value_Total = weighted.mean(HHI_Value_Total,Total_Value),
              TotalCatch_Tonnes = sum(TotalCatch_Tonnes),
              Total_Value = sum(Total_Value)) %>%
    mutate(Country.or.Area = "Weighted_Ave")
}




#地域(サブ)ごとのλ
fun_Simpson4 <- function(data,TotalCatch_Tonnes,Total_Value){
  data %>% 
    group_by_(.dots = lazyeval::lazy_dots(Year,Country.or.Area,TaxonKey)) %>% 
    summarise(TotalCatch_Tonnes = sum(TotalCatch_Tonnes)/1000000,
              Total_Value = sum(Total_Value)/1000000) %>%
    mutate(Percentage_Catch = (TotalCatch_Tonnes/sum(TotalCatch_Tonnes)),
           Percentage_Value = (Total_Value/sum(Total_Value))) %>%
    mutate(Simpson_Catch = Percentage_Catch^2,
           Simpson_Value = Percentage_Value^2) %>%
    ungroup() %>% 
    group_by_(.dots = lazyeval::lazy_dots(Year,Country.or.Area)) %>% 
    summarise(Simpson_Catch_Total = 1/sum(Simpson_Catch),
              Simpson_Value_Total = 1/sum(Simpson_Value),
              TotalCatch_Tonnes = sum(TotalCatch_Tonnes),
              Total_Value = sum(Total_Value)) %>% 
    arrange(Country.or.Area)
}


#地域(サブ)平均のλ
fun_Simpson5 <- function(data,TotalCatch_Tonnes,Total_Value){
  data %>% 
    group_by_(.dots = lazyeval::lazy_dots(Year,Sub.region.Name_koh,Country.or.Area,TaxonKey)) %>% 
    summarise(TotalCatch_Tonnes = sum(TotalCatch_Tonnes)/1000000,
              Total_Value = sum(Total_Value)/1000000) %>%
    mutate(Percentage_Catch = (TotalCatch_Tonnes/sum(TotalCatch_Tonnes)),
           Percentage_Value = (Total_Value/sum(Total_Value))) %>%
    mutate(Simpson_Catch = Percentage_Catch^2,
           Simpson_Value = Percentage_Value^2) %>%
    ungroup() %>% 
    group_by_(.dots = lazyeval::lazy_dots(Year,Sub.region.Name_koh,Country.or.Area)) %>% 
    summarise(Simpson_Catch_Total = 1/sum(Simpson_Catch),
              Simpson_Value_Total = 1/sum(Simpson_Value),
              TotalCatch_Tonnes = sum(TotalCatch_Tonnes),
              Total_Value = sum(Total_Value)) %>% 
    ungroup() %>% 
    group_by_(.dots = lazyeval::lazy_dots(Year,Sub.region.Name_koh)) %>% 
    summarise(Simpson_Catch_Total = mean(Simpson_Catch_Total),
              Simpson_Value_Total = mean(Simpson_Value_Total),
              TotalCatch_Tonnes = sum(TotalCatch_Tonnes),
              Total_Value = sum(Total_Value)) %>%
    mutate(Country.or.Area = "Average")
}

#地域(サブ)Totalのλ
fun_Simpson6 <- function(data,TotalCatch_Tonnes,Total_Value){
  data %>% 
    group_by_(.dots = lazyeval::lazy_dots(Year,TaxonKey)) %>% 
    summarise(TotalCatch_Tonnes = sum(TotalCatch_Tonnes)/1000000,
              Total_Value = sum(Total_Value)/1000000) %>%
    mutate(Percentage_Catch = (TotalCatch_Tonnes/sum(TotalCatch_Tonnes)),
           Percentage_Value = (Total_Value/sum(Total_Value))) %>%
    mutate(Simpson_Catch = Percentage_Catch^2,
           Simpson_Value = Percentage_Value^2) %>%
    ungroup() %>% 
    group_by_(.dots = lazyeval::lazy_dots(Year)) %>% 
    summarise(Simpson_Catch_Total = 1/sum(Simpson_Catch),
              Simpson_Value_Total = 1/sum(Simpson_Value),
              TotalCatch_Tonnes = sum(TotalCatch_Tonnes),
              Total_Value = sum(Total_Value)) %>% 
    mutate(Country.or.Area = "Total")
}


#地域(サブ)加重平均のλ
fun_Simpson7 <- function(data,TotalCatch_Tonnes,Total_Value){
  data %>% 
    group_by_(.dots = lazyeval::lazy_dots(Year,Sub.region.Name_koh,Country.or.Area,TaxonKey)) %>% 
    summarise(TotalCatch_Tonnes = sum(TotalCatch_Tonnes)/1000000,
              Total_Value = sum(Total_Value)/1000000) %>%
    mutate(Percentage_Catch = (TotalCatch_Tonnes/sum(TotalCatch_Tonnes)),
           Percentage_Value = (Total_Value/sum(Total_Value))) %>%
    mutate(Simpson_Catch = Percentage_Catch^2,
           Simpson_Value = Percentage_Value^2) %>%
    ungroup() %>% 
    group_by_(.dots = lazyeval::lazy_dots(Year,Sub.region.Name_koh,Country.or.Area)) %>% 
    summarise(Simpson_Catch_Total = 1/sum(Simpson_Catch),
              Simpson_Value_Total = 1/sum(Simpson_Value),
              TotalCatch_Tonnes = sum(TotalCatch_Tonnes),
              Total_Value = sum(Total_Value)) %>% 
    ungroup() %>% 
    group_by_(.dots = lazyeval::lazy_dots(Year,Sub.region.Name_koh)) %>% 
    summarise(Simpson_Catch_Total = weighted.mean(Simpson_Catch_Total,TotalCatch_Tonnes),
              Simpson_Value_Total = weighted.mean(Simpson_Value_Total,Total_Value),
              TotalCatch_Tonnes = sum(TotalCatch_Tonnes),
              Total_Value = sum(Total_Value)) %>%
    mutate(Country.or.Area = "Weighted_Ave")
}


#地域(サブ)ごとのH’
fun_Shannon4 <- function(data,TotalCatch_Tonnes,Total_Value){
  data %>% 
    group_by_(.dots = lazyeval::lazy_dots(Year,Country.or.Area,TaxonKey)) %>% 
    summarise(TotalCatch_Tonnes = sum(TotalCatch_Tonnes)/1000000,
              Total_Value = sum(Total_Value)/1000000) %>%
    mutate(Percentage_Catch = (TotalCatch_Tonnes/sum(TotalCatch_Tonnes)),
           Percentage_Value = (Total_Value/sum(Total_Value))) %>%
    mutate(Shannon_Catch = (Percentage_Catch*log(Percentage_Catch)),
           Shannon_Value = (Percentage_Value*log(Percentage_Value))) %>%
    ungroup() %>% 
    group_by_(.dots = lazyeval::lazy_dots(Year,Country.or.Area)) %>% 
    summarise(Shannon_Catch_Total = exp(-sum(Shannon_Catch)),
              Shannon_Value_Total = exp(-sum(Shannon_Value)),
              TotalCatch_Tonnes = sum(TotalCatch_Tonnes),
              Total_Value = sum(Total_Value)) %>%
    arrange(Country.or.Area)
}


#地域(サブ)平均のH’
fun_Shannon5 <- function(data,TotalCatch_Tonnes,Total_Value){
  data %>% 
    group_by_(.dots = lazyeval::lazy_dots(Year,Sub.region.Name_koh,Country.or.Area,TaxonKey)) %>% 
    summarise(TotalCatch_Tonnes = sum(TotalCatch_Tonnes)/1000000,
              Total_Value = sum(Total_Value)/1000000) %>%
    mutate(Percentage_Catch = (TotalCatch_Tonnes/sum(TotalCatch_Tonnes)),
           Percentage_Value = (Total_Value/sum(Total_Value))) %>%
    mutate(Shannon_Catch = (Percentage_Catch*log(Percentage_Catch)),
           Shannon_Value = (Percentage_Value*log(Percentage_Value))) %>%
    ungroup() %>% 
    group_by_(.dots = lazyeval::lazy_dots(Year,Sub.region.Name_koh,Country.or.Area)) %>% 
    summarise(Shannon_Catch_Total = exp(-sum(Shannon_Catch)),
              Shannon_Value_Total = exp(-sum(Shannon_Value)),
              TotalCatch_Tonnes = sum(TotalCatch_Tonnes),
              Total_Value = sum(Total_Value)) %>%
    ungroup() %>% 
    group_by_(.dots = lazyeval::lazy_dots(Year,Sub.region.Name_koh)) %>% 
    summarise(Shannon_Catch_Total = mean(Shannon_Catch_Total),
              Shannon_Value_Total = mean(Shannon_Value_Total),
              TotalCatch_Tonnes = sum(TotalCatch_Tonnes),
              Total_Value = sum(Total_Value) ) %>% 
  mutate(Country.or.Area = "Average")
}


#地域(サブ)TotalのH’
fun_Shannon6 <- function(data,TotalCatch_Tonnes,Total_Value){
  data %>% 
    group_by_(.dots = lazyeval::lazy_dots(Year,TaxonKey)) %>% 
    summarise(TotalCatch_Tonnes = sum(TotalCatch_Tonnes)/1000000,
              Total_Value = sum(Total_Value)/1000000) %>%
    mutate(Percentage_Catch = (TotalCatch_Tonnes/sum(TotalCatch_Tonnes)),
           Percentage_Value = (Total_Value/sum(Total_Value))) %>%
    mutate(Shannon_Catch = (Percentage_Catch*log(Percentage_Catch)),
           Shannon_Value = (Percentage_Value*log(Percentage_Value))) %>%
    ungroup() %>% 
    group_by_(.dots = lazyeval::lazy_dots(Year)) %>% 
    summarise(Shannon_Catch_Total = exp(-sum(Shannon_Catch)),
              Shannon_Value_Total = exp(-sum(Shannon_Value)),
              TotalCatch_Tonnes = sum(TotalCatch_Tonnes),
              Total_Value = sum(Total_Value)) %>%
    mutate(Country.or.Area = "Total")
}

#地域(サブ)加重平均のH’
fun_Shannon7 <- function(data,TotalCatch_Tonnes,Total_Value){
  data %>% 
    group_by_(.dots = lazyeval::lazy_dots(Year,Sub.region.Name_koh,Country.or.Area,TaxonKey)) %>% 
    summarise(TotalCatch_Tonnes = sum(TotalCatch_Tonnes)/1000000,
              Total_Value = sum(Total_Value)/1000000) %>%
    mutate(Percentage_Catch = (TotalCatch_Tonnes/sum(TotalCatch_Tonnes)),
           Percentage_Value = (Total_Value/sum(Total_Value))) %>%
    mutate(Shannon_Catch = (Percentage_Catch*log(Percentage_Catch)),
           Shannon_Value = (Percentage_Value*log(Percentage_Value))) %>%
    ungroup() %>% 
    group_by_(.dots = lazyeval::lazy_dots(Year,Sub.region.Name_koh,Country.or.Area)) %>% 
    summarise(Shannon_Catch_Total = exp(-sum(Shannon_Catch)),
              Shannon_Value_Total = exp(-sum(Shannon_Value)),
              TotalCatch_Tonnes = sum(TotalCatch_Tonnes),
              Total_Value = sum(Total_Value)) %>%
    ungroup() %>% 
    group_by_(.dots = lazyeval::lazy_dots(Year,Sub.region.Name_koh)) %>% 
    summarise(Shannon_Catch_Total = weighted.mean(Shannon_Catch_Total,TotalCatch_Tonnes),
              Shannon_Value_Total = weighted.mean(Shannon_Value_Total,Total_Value),
              TotalCatch_Tonnes = sum(TotalCatch_Tonnes),
              Total_Value = sum(Total_Value) ) %>% 
    mutate(Country.or.Area = "Weighted_Ave")
}





#年ごと
#CV_Region
fun_CV1 <- function(data,TotalCatch_Tonnes,Total_Value){
  data %>%
    group_by(.dots = lazyeval::lazy_dots(Year,Region.Name_koh)) %>%
    summarise(TotalCatch_Tonnes = sum(TotalCatch_Tonnes)/1000,
              Total_Value = sum(Total_Value)/1000000) %>%
    mutate(Percentage_Catch = (TotalCatch_Tonnes/sum(TotalCatch_Tonnes))*100,
           Percentage_Value = (Total_Value/sum(Total_Value))*100) %>%
    ungroup() %>% 
    group_by(.dots = lazyeval::lazy_dots(Region.Name_koh)) %>%
    summarise(CV_Catch = sd(TotalCatch_Tonnes)/mean(TotalCatch_Tonnes),
              CV_Value = sd(Total_Value)/mean(Total_Value)) %>% 
    na.omit()  %>% 
    arrange(Region.Name_koh)
}


#年ごと
#CV_Subregion
fun_CV2 <- function(data,TotalCatch_Tonnes,Total_Value){
  data %>%
    group_by(.dots = lazyeval::lazy_dots(Year,Sub.region.Name_koh)) %>%
    summarise(TotalCatch_Tonnes = sum(TotalCatch_Tonnes)/1000,
              Total_Value = sum(Total_Value)/1000000) %>%
    mutate(Percentage_Catch = (TotalCatch_Tonnes/sum(TotalCatch_Tonnes))*100,
           Percentage_Value = (Total_Value/sum(Total_Value))*100) %>%
    ungroup() %>% 
    group_by(.dots = lazyeval::lazy_dots(Sub.region.Name_koh)) %>%
    summarise(CV_Catch = sd(TotalCatch_Tonnes)/mean(TotalCatch_Tonnes),
              CV_Value = sd(Total_Value)/mean(Total_Value)) %>% 
    na.omit()  %>% 
    arrange(Sub.region.Name_koh)
}


#年ごと
#CV_Country
fun_CV3 <- function(data,TotalCatch_Tonnes,Total_Value){
  data %>%
    group_by(.dots = lazyeval::lazy_dots(Year,Country.or.Area)) %>%
    summarise(TotalCatch_Tonnes = sum(TotalCatch_Tonnes)/1000,
              Total_Value = sum(Total_Value)/1000000) %>%
    mutate(Percentage_Catch = (TotalCatch_Tonnes/sum(TotalCatch_Tonnes))*100,
           Percentage_Value = (Total_Value/sum(Total_Value))*100) %>%
    ungroup() %>% 
    group_by(.dots = lazyeval::lazy_dots(Country.or.Area)) %>%
    summarise(CV_Catch = sd(TotalCatch_Tonnes)/mean(TotalCatch_Tonnes),
              CV_Value = sd(Total_Value)/mean(Total_Value)) %>% 
    na.omit()  %>% 
    arrange(Country.or.Area)
}



#魚種ごと
#CV_Region
fun_CV4 <- function(data,TotalCatch_Tonnes,Total_Value){
  data %>%
    group_by(.dots = lazyeval::lazy_dots(Year,Region.Name_koh,TaxonKey)) %>%
    summarise(TotalCatch_Tonnes = sum(TotalCatch_Tonnes)/1000,
              Total_Value = sum(Total_Value)/1000000) %>%
    mutate(Percentage_Catch = (TotalCatch_Tonnes/sum(TotalCatch_Tonnes))*100,
           Percentage_Value = (Total_Value/sum(Total_Value))*100) %>%
    ungroup() %>% 
    group_by(.dots = lazyeval::lazy_dots(Region.Name_koh)) %>%
    summarise(CV_Catch = sd(TotalCatch_Tonnes)/mean(TotalCatch_Tonnes),
              CV_Value = sd(Total_Value)/mean(Total_Value)) %>% 
    na.omit()  %>% 
    arrange(Region.Name_koh)
}


#魚種ごと
#CV_Subregion
fun_CV5 <- function(data,TotalCatch_Tonnes,Total_Value){
  data %>%
    group_by(.dots = lazyeval::lazy_dots(Year,Sub.region.Name_koh,TaxonKey)) %>%
    summarise(TotalCatch_Tonnes = sum(TotalCatch_Tonnes)/1000,
              Total_Value = sum(Total_Value)/1000000) %>%
    mutate(Percentage_Catch = (TotalCatch_Tonnes/sum(TotalCatch_Tonnes))*100,
           Percentage_Value = (Total_Value/sum(Total_Value))*100) %>%
    ungroup() %>% 
    group_by(.dots = lazyeval::lazy_dots(Sub.region.Name_koh)) %>%
    summarise(CV_Catch = sd(TotalCatch_Tonnes)/mean(TotalCatch_Tonnes),
              CV_Value = sd(Total_Value)/mean(Total_Value)) %>% 
    na.omit()  %>% 
    arrange(Sub.region.Name_koh)
}


#魚種ごと
#CV_Country
fun_CV6 <- function(data,TotalCatch_Tonnes,Total_Value){
  data %>%
    group_by(.dots = lazyeval::lazy_dots(Year,Country.or.Area,TaxonKey)) %>%
    summarise(TotalCatch_Tonnes = sum(TotalCatch_Tonnes)/1000,
              Total_Value = sum(Total_Value)/1000000) %>%
    mutate(Percentage_Catch = (TotalCatch_Tonnes/sum(TotalCatch_Tonnes))*100,
           Percentage_Value = (Total_Value/sum(Total_Value))*100) %>%
    ungroup() %>% 
    group_by(.dots = lazyeval::lazy_dots(Country.or.Area)) %>%
    summarise(CV_Catch = sd(TotalCatch_Tonnes)/mean(TotalCatch_Tonnes),
              CV_Value = sd(Total_Value)/mean(Total_Value)) %>% 
    na.omit()  %>% 
    arrange(Country.or.Area)
}