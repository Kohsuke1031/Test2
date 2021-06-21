##  縦書き用フォーマット
#h ttps://id.fnshr.info/2017/03/13/r-plot-tategaki/
tategaki <- function(x){
  x <- chartr("ー", "丨", x) # 長音符の処理
  x <- strsplit(split="", x)
  sapply(x, paste, collapse="\n")
}
