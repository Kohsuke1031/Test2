##  gaku's fucntions 

## 1  縦書き用フォーマット
#h ttps://id.fnshr.info/2017/03/13/r-plot-tategaki/

tategaki <- function(x){
  x <- chartr("ー", "丨", x) # 長音符の処理
  x <- strsplit(split="", x)
  sapply(x, paste, collapse="\n")
}


#df_paths = list.files(path = "./gakuLab_with_Yasui_san/Ishikawa_Analysis/Ishikawa_Data",full.names = T)

## 2  縦書き用フォーマット
read_files <- function(x){
  df <- read.csv(x,
                 header = T,
                 fileEncoding = "CP932")
  return(df)
}



######  function to express y axis desits
ScientificNotation <- function(l) {
  l <- format(l, scientific = TRUE)
  l <- gsub("^(.*)e", "'\\1'e", l)
  l <- gsub("e\\+", "%*%10^", l)
  l[1] <- "0"
  parse(text = l)}
######