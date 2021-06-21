###
#dataごとのλ  

fun_Simpson_Sim_gaku<- function(data,Catch_kg,Value_JPY,Fish_Name){
  data %>% 
    group_by((Fish_Name)) %>% 
    summarise(Catch = (sum(Catch_kg)/1000),
              Value = (sum(Value_JPY)/1000)) %>%
    mutate(Percentage_Catch = (Catch/sum(Catch)),
           Percentage_Value = (Value/sum(Value))) %>%
    mutate(Simpson_Catch = Percentage_Catch^2,
           Simpson_Value = Percentage_Value^2) %>%
    summarise(Simpson_Catch_Total = 1/sum(Simpson_Catch),
              Simpson_Value_Total = 1/sum(Simpson_Value),
              Catch = sum(Catch),
              Value = sum(Value)) %>% 
    mutate(Percentage_Catch = (Catch/sum(Catch))*100,
           Percentage_Value = (Value/sum(Value))*100) 
}




fun_Simpson_Sim_gaku2<- function(data,Catch_kg,Value_JPY){
  data %>% 
    group_by_(.dots = lazyeval::lazy_dots(Fish_Name_JP)) %>% 
    summarise(Catch = (sum(Catch_kg)/1000),
              Value = (sum(Value_JPY)/1000)) %>%
    mutate(Percentage_Catch = (Catch/sum(Catch)),
           Percentage_Value = (Value/sum(Value))) %>%
    mutate(Simpson_Catch = Percentage_Catch^2,
           Simpson_Value = Percentage_Value^2) %>%
    summarise(Simpson_Catch_Total = 1/sum(Simpson_Catch),
              Simpson_Value_Total = 1/sum(Simpson_Value),
              Catch = sum(Catch),
              Value = sum(Value)) %>% 
    mutate(Percentage_Catch = (Catch/sum(Catch))*100,
           Percentage_Value = (Value/sum(Value))*100) 
}




##  オリジナルファイルの読み込み

read_files <- function(x){
  df <- read.csv(x,
                 header = T,
                 fileEncoding = "CP932"
  )
  return(df)
}


## 各年の漁獲量・漁獲高
fun_Annual_Catch_Value <- function(data,Catch_kg,Value_JPY){
  data %>% 
    group_by_(.dots = lazyeval::lazy_dots(Year,Fishery_Type)) %>% 
    summarise(Catch = ((sum(Catch_kg))/1000),
              Value = (sum(Value_JPY))/10000000, .groups = "drop") %>%
    arrange(Year)
}

## 月ごとの漁獲量・漁獲高
fun_Monthly_Catch_Value <- function(data,Catch_kg,Value_JPY){
  data %>% 
    group_by_(.dots = lazyeval::lazy_dots(Month,Fishery_Type)) %>% 
    summarise(Catch = ((sum(Catch_kg))/1000),
              Value = (sum(Value_JPY))/10000000) %>%
    arrange(Month)
}


##  時系列データでの種毎の比率
Fish_Species_Composition <- function(data){
  data %>%
    group_by(Fish_Name_JP) %>% 
    summarise(Total_Catch = sum(Catch_kg),
              Total_Value = sum(Value_JPY)) %>% 
    mutate(Percentage_Catch = (Total_Catch/sum(Total_Catch))*100,
           Percentage_Value = (Total_Value/sum(Total_Value))*100) 
}

##  時系列データでの種毎の比率
Data_Summary <- function(data,key_1){
  data %>%
    group_by_(.dots = lazyeval::lazy_dots(key_1))
  summarize(
    Temp_Total_Catch=sum(Catch_kg),
    Temp_Total_Value=sum(Value_JPY))%>%
  summarize(
    ####
    Total_Catch=sum(Temp_Total_Catch),
    Total_Value=sum(Temp_Total_Value),
    ###
    Catch_AV=mean(Temp_Total_Catch),
    Catch_SD=sd(Temp_Total_Catch),
    Catch_CV=Catch_SD/Catch_AV,
    Value_AV=mean(Temp_Total_Value),
    Value_SD=sd(Temp_Total_Value),
    Value_CV=Value_SD/Value_AV
  )
}


#漁業種ごとの漁獲割合
fun_Fishery_Rate <- function(data,Catch_kg,Value_JPY){
  data %>% 
    group_by_(.dots = lazyeval::lazy_dots(Year,Fishery_Type)) %>% 
    summarise(Catch = ((sum(Catch_kg))/1000),
              Value = (sum(Value_JPY))/10000000) %>%
    mutate(Percentage_Catch = (Catch/sum(Catch))*100,
           Percentage_Value = (Value/sum(Value))*100,
           UnitPrice = Value/Catch) %>%
    arrange(Year)
}

## 3-3 年間漁獲魚種構成
#漁獲量上位１０魚種の漁獲量・漁獲高における漁獲構成
fun_Fish_Rate <- function(data,Catch_kg,Value_JPY){
  data %>% 
    group_by_(.dots = lazyeval::lazy_dots(Year,Fish_Name_JP)) %>% 
    summarise(Catch = ((sum(Catch_kg))/1000),
              Value = (sum(Value_JPY))/10000000) %>%
    mutate(Percentage_Catch = (Catch/sum(Catch))*100,
           Percentage_Value = (Value/sum(Value))*100,
           UnitPrice = Value/Catch) %>%
    arrange(Year)
}

#漁獲高上位１０魚種の漁獲量・漁獲高における漁獲構成
fun_Fish_Rate2 <- function(data,Catch_kg,Value_JPY){
  data %>% 
    group_by_(.dots = lazyeval::lazy_dots(Year,Fish_Name_JP)) %>% 
    summarise(Catch = ((sum(Catch_kg))/1000),
              Value = (sum(Value_JPY))/10000000) %>%
    mutate(Percentage_Catch = (Catch/sum(Catch))*100,
           Percentage_Value = (Value/sum(Value))*100,
           UnitPrice = Value/Catch) %>%
    arrange(Year)
}

## 3-5 月ごとの魚獲魚種構成
#漁獲量上位１０魚種の漁獲量・漁獲高における漁獲構成
fun_Fish_Rate3 <- function(data,Catch_kg,Value_JPY){
  data %>% 
    group_by_(.dots = lazyeval::lazy_dots(Month,Fish_Name_JP)) %>% 
    summarise(Catch = ((sum(Catch_kg))/1000),
              Value = (sum(Value_JPY))/10000000) %>%
    mutate(Percentage_Catch = (Catch/sum(Catch))*100,
           Percentage_Value = (Value/sum(Value))*100,
           UnitPrice = Value/Catch) %>%
    arrange(Month)
}

#漁獲高上位１０魚種の漁獲量・漁獲高における漁獲構成
fun_Fish_Rate4 <- function(data,Catch_kg,Value_JPY){
  data %>% 
    group_by_(.dots = lazyeval::lazy_dots(Month,Fish_Name_JP)) %>% 
    summarise(Catch = ((sum(Catch_kg))/1000),
              Value = (sum(Value_JPY))/10000000) %>%
    mutate(Percentage_Catch = (Catch/sum(Catch))*100,
           Percentage_Value = (Value/sum(Value))*100,
           UnitPrice = Value/Catch) %>%
    arrange(Month)
}

## 3-6 HHI
#年ごとのHHI
fun_HHI <- function(data,Catch_kg,Value_JPY){
  data %>% 
    group_by_(.dots = lazyeval::lazy_dots(Year,Fish_Name_JP)) %>% 
    summarise(Catch = sum(Catch_kg)/1000,
              Value = sum(Value_JPY)/10000000) %>%
    mutate(Percentage_Catch = (Catch/sum(Catch))*100,
           Percentage_Value = (Value/sum(Value))*100) %>%
    mutate(HHI_Catch = Percentage_Catch^2,
           HHI_Value = Percentage_Value^2) %>%
    group_by_(.dots = lazyeval::lazy_dots(Year)) %>% 
    summarise(HHI_Catch_Total = sum(HHI_Catch),
              HHI_Value_Total = sum(HHI_Value),
              Catch = sum(Catch),
              Value = sum(Value)) %>%
    mutate(Percentage_Catch = (Catch/sum(Catch))*100,
           Percentage_Value = (Value/sum(Value))*100) %>%
    mutate(FisheryType = "加賀支所全体") %>% 
    arrange(Year)
}

## 3-6 HHI
#年ごとのHHIfor simulation
fun_HHI_Sim <- function(data,Catch_kg,Value_JPY){
  data %>% 
    group_by_(.dots = lazyeval::lazy_dots(Year,Fish_Name_JP)) %>% 
    summarise(Catch = sum(Catch_kg)/1000,
              Value = sum(Value_JPY)/10000000) %>%
    mutate(Percentage_Catch = (Catch/sum(Catch))*100,
           Percentage_Value = (Value/sum(Value))*100) %>%
    mutate(HHI_Catch = Percentage_Catch^2,
           HHI_Value = Percentage_Value^2) %>%
    group_by_(.dots = lazyeval::lazy_dots(Year)) %>% 
    summarise(HHI_Catch_Total = sum(HHI_Catch),
              HHI_Value_Total = sum(HHI_Value),
              Catch = sum(Catch),
              Value = sum(Value)) %>%
    mutate(Percentage_Catch = (Catch/sum(Catch))*100,
           Percentage_Value = (Value/sum(Value))*100) %>%
    arrange(Year)
}

#年ごとのλ
fun_Simpson_Sim <- function(data,Catch_kg,Value_JPY){
  data %>% 
    group_by_(.dots = lazyeval::lazy_dots(Year,Fish_Name_JP)) %>% 
    summarise(Catch = (sum(Catch_kg)/1000),
              Value = (sum(Value_JPY)/1000)) %>%
    mutate(Percentage_Catch = (Catch/sum(Catch)),
           Percentage_Value = (Value/sum(Value))) %>%
    mutate(Simpson_Catch = Percentage_Catch^2,
           Simpson_Value = Percentage_Value^2) %>%
    group_by_(.dots = lazyeval::lazy_dots(Year)) %>% 
    summarise(Simpson_Catch_Total = 1/sum(Simpson_Catch),
              Simpson_Value_Total = 1/sum(Simpson_Value),
              Catch = sum(Catch),
              Value = sum(Value)) %>% 
    mutate(Percentage_Catch = (Catch/sum(Catch))*100,
           Percentage_Value = (Value/sum(Value))*100) %>%
    arrange(Year)
}


#年ごとのH'
fun_Shannon_Sim <- function(data,Catch_kg,Value_JPY){
  data %>% 
    group_by_(.dots = lazyeval::lazy_dots(Year,Fish_Name_JP)) %>% 
    summarise(Catch = (sum(Catch_kg)/1000),
              Value = (sum(Value_JPY)/1000)) %>%
    mutate(Percentage_Catch = (Catch/sum(Catch)),
           Percentage_Value = (Value/sum(Value))) %>%
    mutate(Shannon_Catch = -(Percentage_Catch*log2(Percentage_Catch)),
           Shannon_Value = -(Percentage_Value*log2(Percentage_Value))) %>%
    group_by_(.dots = lazyeval::lazy_dots(Year)) %>% 
    summarise(Shannon_Catch_Total = sum(Shannon_Catch),
              Shannon_Value_Total = sum(Shannon_Value),
              Catch = sum(Catch),
              Value = sum(Value)) %>%
    mutate(Percentage_Catch = (Catch/sum(Catch))*100,
           Percentage_Value = (Value/sum(Value))*100) %>%
    arrange(Year)
}





#月ごとのHHI
fun_HHI2 <- function(data,Catch_kg,Value_JPY){
  data %>% 
    group_by_(.dots = lazyeval::lazy_dots(Month,Fish_Name_JPY)) %>% 
    summarise(Catch = sum(Catch_kg)/1000,
              Value = sum(Value_JPY)/10000000) %>%
    mutate(Percentage_Catch = (Catch/sum(Catch))*100,
           Percentage_Value = (Value/sum(Value))*100) %>%
    mutate(HHI_Catch = Percentage_Catch^2,
           HHI_Value = Percentage_Value^2) %>%
    group_by_(.dots = lazyeval::lazy_dots(Month)) %>%
    summarise(HHI_Catch_Total = sum(HHI_Catch),
              HHI_Value_Total = sum(HHI_Value),Catch = sum(Catch),
              Value = sum(Value)) %>%
    arrange(Month)
}

## 3-7 シンプソン指数λ
#年ごとのλ
fun_Simpson <- function(data,Catch_kg,Value_JPY){
  data %>% 
    group_by_(.dots = lazyeval::lazy_dots(Year,Fish_Name_JPY)) %>% 
    summarise(Catch = (sum(Catch_kg)/1000),
              Value = (sum(Value_JPY)/1000)) %>%
    mutate(Percentage_Catch = (Catch/sum(Catch)),
           Percentage_Value = (Value/sum(Value))) %>%
    mutate(Simpson_Catch = Percentage_Catch^2,
           Simpson_Value = Percentage_Value^2) %>%
    group_by_(.dots = lazyeval::lazy_dots(Year)) %>% 
    summarise(Simpson_Catch_Total = 1/sum(Simpson_Catch),
              Simpson_Value_Total = 1/sum(Simpson_Value),
              Catch = sum(Catch),
              Value = sum(Value)) %>% 
    mutate(Percentage_Catch = (Catch/sum(Catch))*100,
           Percentage_Value = (Value/sum(Value))*100) %>%
    mutate(FisheryType = "加賀支所全体") %>% 
    arrange(Year)
}

#月ごとのλ
fun_Simpson2 <- function(data,Catch_kg,Value_JPY){
  data %>% 
    group_by_(.dots = lazyeval::lazy_dots(Month,Fish_Name_JP)) %>% 
    summarise(Catch = (sum(Catch_kg)/1000),
              Value = (sum(Value_JPY)/1000)) %>%
    mutate(Percentage_Catch = (Catch/sum(Catch)),
           Percentage_Value = (Value/sum(Value))) %>%
    mutate(Simpson_Catch = Percentage_Catch^2,
           Simpson_Value = Percentage_Value^2) %>%
    group_by_(.dots = lazyeval::lazy_dots(Month)) %>% 
    summarise(Simpson_Catch_Total = 1/sum(Simpson_Catch),
              Simpson_Value_Total = 1/sum(Simpson_Value),
              Catch = sum(Catch),
              Value = sum(Value))
}

## 3-8 シャノン・ウィナー指数H'
#年ごとのH'
fun_Shannon <- function(data,Catch_kg,Value_JPY){
  data %>% 
    group_by_(.dots = lazyeval::lazy_dots(Year,Fish_Name_JP)) %>% 
    summarise(Catch = (sum(Catch_kg)/1000),
              Value = (sum(Value_JPY)/1000)) %>%
    mutate(Percentage_Catch = (Catch/sum(Catch)),
           Percentage_Value = (Value/sum(Value))) %>%
    mutate(Shannon_Catch = -(Percentage_Catch*log2(Percentage_Catch)),
           Shannon_Value = -(Percentage_Value*log2(Percentage_Value))) %>%
    group_by_(.dots = lazyeval::lazy_dots(Year)) %>% 
    summarise(Shannon_Catch_Total = sum(Shannon_Catch),
              Shannon_Value_Total = sum(Shannon_Value),
              Catch = sum(Catch),
              Value = sum(Value)) %>%
    mutate(Percentage_Catch = (Catch/sum(Catch))*100,
           Percentage_Value = (Value/sum(Value))*100) %>%
    mutate(FisheryType = "加賀支所全体") %>% 
    arrange(Year)
}

#月ごとのH'
fun_Shannon2 <- function(data,Catch_kg,Value_JPY){
    group_by_(.dots = lazyeval::lazy_dots(Month,Fish_Name_JP)) %>% 
    summarise(Catch = (sum(Catch_kg)/1000),
              Value = (sum(Value_JPY)/1000)) %>%
    mutate(Percentage_Catch = (Catch/sum(Catch)),
           Percentage_Value = (Value/sum(Value))) %>%
    mutate(Shannon_Catch = -(Percentage_Catch*log2(Percentage_Catch)),
           Shannon_Value = -(Percentage_Value*log2(Percentage_Value))) %>%
    group_by_(.dots = lazyeval::lazy_dots(Month)) %>% 
    summarise(Shannon_Catch_Total = sum(Shannon_Catch),
              Shannon_Value_Total = sum(Shannon_Value),
              Catch = sum(Catch),
              Value = sum(Value)) %>%
    arrange(Month)
}

#CV
fun_CV <- function(data,Catch_kg,Value_JPY){
  data %>%
    group_by(.dots = lazyeval::lazy_dots(Year,Fish_Name_JP)) %>%
    summarise(Catch = sum(Catch_kg)/1000,
              Value = sum(Value_JPY)/1000000) %>%
    mutate(Percentage_Catch = (Catch/sum(Catch))*100,
           Percentage_Value = (Value/sum(Value))*100) %>%
   # group_by(.dots = lazyeval::lazy_dots(Year)) %>%
    summarise(CV_Catch = sd(Catch)/mean(Catch),
              CV_Value = sd(Value)/mean(Value))
}

#CV2
fun_CV_gaku <- function(data,Catch_kg,Value_JPY){
  data %>%
    group_by(.dots = lazyeval::lazy_dots(Year)) %>%
    summarise(Catch = sum(Catch_kg)/1000,
              Value = sum(Value_JPY)/1000000) %>%
    mutate(Percentage_Catch = (Catch/sum(Catch))*100,
           Percentage_Value = (Value/sum(Value))*100) %>%
    # group_by(.dots = lazyeval::lazy_dots(Year)) %>%
    summarise(CV_Catch = sd(Catch)/mean(Catch),
              CV_Value = sd(Value)/mean(Value))
}

#CV
fun_CV <- function(data,Catch_kg,Value_JPY){
  data %>%
    group_by(.dots = lazyeval::lazy_dots(Year)) %>%
    summarise(Catch = sum(Catch_kg)/1000,
              Value = sum(Value_JPY)/1000000) %>%
    mutate(Percentage_Catch = (Catch/sum(Catch))*100,
           Percentage_Value = (Value/sum(Value))*100) %>%
    ungroup() %>% 
    summarise(CV_Catch = sd(Catch)/mean(Catch),
              CV_Value = sd(Value)/mean(Value))
}




#漁業種ごとのHHI
fun_HHI3 <- function(data,Catch_kg,Value_JPY){
  data %>% 
    group_by_(.dots = lazyeval::lazy_dots(Year,FisheryType,Fish_Name_JP)) %>% 
    summarise(Catch = sum(Catch_kg)/1000,
              Value = sum(Value_JPY)/10000000) %>%
    mutate(Percentage_Catch = (Catch/sum(Catch))*100,
           Percentage_Value = (Value/sum(Value))*100) %>%
    mutate(HHI_Catch = Percentage_Catch^2,
           HHI_Value = Percentage_Value^2) %>%
    group_by_(.dots = lazyeval::lazy_dots(Year,FisheryType)) %>% 
    summarise(HHI_Catch_Total = sum(HHI_Catch),
              HHI_Value_Total = sum(HHI_Value),
              Catch = sum(Catch),
              Value = sum(Value)) %>%
    mutate(Percentage_Catch = (Catch/sum(Catch))*100,
           Percentage_Value = (Value/sum(Value))*100) %>%
    arrange(FisheryType)
}


#漁業種ごとのλ
fun_Simpson3 <- function(data,Catch_kg,Value_JPY){
  data %>% 
    group_by_(.dots = lazyeval::lazy_dots(Year,FisheryType,Fish_Name_JP)) %>% 
    summarise(Catch = (sum(Catch_kg)/1000),
              Value = (sum(Value_JPY)/1000)) %>%
    mutate(Percentage_Catch = (Catch/sum(Catch)),
           Percentage_Value = (Value/sum(Value))) %>%
    mutate(Simpson_Catch = Percentage_Catch^2,
           Simpson_Value = Percentage_Value^2) %>%
    group_by_(.dots = lazyeval::lazy_dots(Year,FisheryType)) %>% 
    summarise(Simpson_Catch_Total = 1/sum(Simpson_Catch),
              Simpson_Value_Total = 1/sum(Simpson_Value),
              Catch = sum(Catch),
              Value = sum(Value)) %>% 
    mutate(Percentage_Catch = (Catch/sum(Catch))*100,
           Percentage_Value = (Value/sum(Value))*100) %>%
    arrange(FisheryType)
}


#漁業種ごとのH’
fun_Shannon3 <- function(data,Catch_kg,Value_JPY){
  data %>% 
    group_by_(.dots = lazyeval::lazy_dots(Year,FisheryType,Fish_Name_JPY)) %>% 
    summarise(Catch = (sum(Catch_kg)/1000),
              Value = (sum(Value_JPY)/1000)) %>%
    mutate(Percentage_Catch = (Catch/sum(Catch)),
           Percentage_Value = (Value/sum(Value))) %>%
    mutate(Shannon_Catch = -(Percentage_Catch*log2(Percentage_Catch)),
           Shannon_Value = -(Percentage_Value*log2(Percentage_Value))) %>%
    group_by_(.dots = lazyeval::lazy_dots(Year,FisheryType)) %>% 
    summarise(Shannon_Catch_Total = sum(Shannon_Catch),
              Shannon_Value_Total = sum(Shannon_Value),
              Catch = sum(Catch),
              Value = sum(Value)) %>%
    mutate(Percentage_Catch = (Catch/sum(Catch))*100,
           Percentage_Value = (Value/sum(Value))*100) %>%
    arrange(FisheryType)
}





