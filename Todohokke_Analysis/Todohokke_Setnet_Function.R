#漁獲量・漁獲高の合計と割合の関数、年ごと
fun_Catch_Value <- function(data,Catch,Value){
  data %>% 
    select(Year,Catch,Value) %>% 
    group_by_(.dots = lazyeval::lazy_dots(Year)) %>% 
    summarise( Total_Catch = sum(Catch)/1000,
            Total_Value = sum(Value)/1000000)
}

#漁獲割合
fun_Fish_Rate <- function(data,Catch,Value){
  data %>% 
   group_by_(.dots = lazyeval::lazy_dots(Year,en)) %>% 
    summarise(Total_Catch = sum(Catch)/1000,
              Total_Value = sum(Value)/1000000) %>% 
    mutate( Percentage_Catch = (Total_Catch/sum(Total_Catch))*100,
            Percentage_Value = (Total_Value/sum(Total_Value))*100)
}

#上記の式は上位５魚種とその他の魚種に分けるコードである



#漁獲量・漁獲高の合計と割合、月ごと
fun_Catch_Value_2 <- function(data,Catch,Value){
  data %>% 
    group_by_(.dots = lazyeval::lazy_dots(Year,Month)) %>% 
    mutate( Total_Catch = sum(Catch)/1000,
            Total_Value = sum(Value)/1000000,
            Percentage_Catch = (Total_Catch/sum(Total_Catch))*100,
            Percentage_Value = (Total_Value/sum(Total_Value))*100)
}


#HHI
fun_HHI <- function(data,Catch,Value){
  data %>% 
    group_by_(.dots = lazyeval::lazy_dots(Year,en)) %>% 
    summarise(Catch = sum(Catch)/1000,
              Value = sum(Value)/1000000) %>% 
    mutate(Percentage_Catch = (Catch/sum(Catch))*100,
           Percentage_Value = (Value/sum(Value))*100) %>% 
    mutate(HHI_Catch = (Percentage_Catch^2),
           HHI_Value = (Percentage_Value^2),
           Catch = sum(Catch),
           Value = sum(Value)) %>%
    group_by_(.dots = lazyeval::lazy_dots(Year)) %>% 
    summarise(HHI_Catch = sum(HHI_Catch),
              HHI_Value = sum(HHI_Value),
              Total_Catch = sum(Catch),
              Total_Value = sum(Value)) 
}


#月ごとのHHI
fun_HHI2 <- function(data,Catch,Value){
  data %>% 
    group_by_(.dots = lazyeval::lazy_dots(Year,Month,en)) %>% 
    summarise(Catch = sum(Catch)/1000,
              Value = sum(Value)/1000000) %>% 
    mutate( Percentage_Catch = (Catch/sum(Catch))*100,
            Percentage_Value = (Value/sum(Value))*100) %>% 
    mutate(HHI_Catch = (Percentage_Catch^2),
           HHI_Value = (Percentage_Value^2)) %>%
    group_by_(.dots = lazyeval::lazy_dots(Year,Month)) %>% 
    summarise(HHI_Catch = sum(HHI_Catch),
              HHI_Value = sum(HHI_Value),
              Total_Catch = sum(Catch)) 
}

#魚種ごとの年別漁獲量・漁獲高の推移
fun_Fish <- function(data,Catch,Value){
  data %>%
    group_by(.dots = lazyeval::lazy_dots(Year)) %>%
    summarise(Catch = (sum(Catch)/1000),
              Value = (sum(Value)/1000000)) %>%
    arrange(Year)
}

#主要魚種を抜いた場合のHHI
fun_re_HHI <- function(data,Catch,Value){
  data %>% 
    group_by(.dots = lazyeval::lazy_dots(Year,en)) %>% 
    summarise(Catch = sum(Catch),
              Value = sum(Value)) %>% 
    mutate(Percentage_Catch = (Catch/sum(Catch))*100,
           Percentage_Value = (Value/sum(Value))*100) %>% 
    mutate(HHI_Catch = Percentage_Catch^2,
           HHI_Value = Percentage_Value^2) %>% 
    mutate(Percentage_HHI_Catch = (HHI_Catch/sum(HHI_Catch)),
           Percentage_HHI_Value = (HHI_Value/sum(HHI_Value))) %>%
    summarise(all_HHI_Catch = sum(HHI_Catch),all_HHI_Value = sum(HHI_Value)) %>% 
    arrange(Year) 
}


#Simpson
fun_Simpson <- function(data,Catch,Value){
  data %>% 
    group_by(.dots = lazyeval::lazy_dots(Year,en)) %>%
    summarise(Catch = (sum(Catch)/1000),
              Value = (sum(Value)/1000000)) %>%
    mutate(Percentage_Catch = (Catch/sum(Catch)),
           Percentage_Value = (Value/sum(Value))) %>%
    mutate(Simpson_Catch = Percentage_Catch^2,
           Simpson_Value = Percentage_Value^2) %>%
    group_by(Year) %>%
    summarise(Simpson_Catch = 1/sum(Simpson_Catch),
              Simpson_Value = 1/sum(Simpson_Value),
              Catch = sum(Catch),
              Value = sum(Value)) %>%
    arrange(Year)
}


#Shannon
fun_Shannon <- function(data,Catch,Value){
  Main %>% 
    group_by(.dots = lazyeval::lazy_dots(Year,en)) %>%
    summarise(Catch = (sum(Catch)/1000),
              Value = (sum(Value)/1000)) %>%
    mutate(Percentage_Catch = (Catch/sum(Catch)),
           Percentage_Value = (Value/sum(Value))) %>%
    mutate(Shannon_Catch = -(Percentage_Catch*log2(Percentage_Catch)),
           Shannon_Value = -(Percentage_Value*log2(Percentage_Value))) %>%
    group_by(Year) %>%
    summarise(Shannon_Catch = sum(Shannon_Catch),
              Shannon_Value = sum(Shannon_Value),
              Catch = sum(Catch),
              Value = sum(Value)) %>%
    arrange(Year)
}

#CV
fun_CV <- function(data,Catch,Value){
  data %>%
    select(Year,en,Catch,Value) %>%
    group_by(.dots = lazyeval::lazy_dots(Year,en)) %>%
    summarise(Catch = sum(Catch)/1000,
              Value = sum(Value)/1000000) %>%
    mutate(Percentage_Catch = (Catch/sum(Catch))*100,
           Percentage_Value = (Value/sum(Value))*100) %>%
    ungroup() %>%
    summarise(CV_Catch = sd(Catch)/mean(Catch),
              CV_Value = sd(Value)/mean(Value))
}
