#csv, xls, xlsx形式を読み込む関数。
#使いまわすことを考えて単一のファイルに。

#ライブラリ読み込み
library(readxl)
library(dplyr)

#末尾に特定の文字を含むか判断する関数
#read.dataとexcel.sheetで使う
is.text <- function(text, file){
  #http://www.okadajp.org/RWiki/?R%20%E3%81%AB%E3%81%8A%E3%81%91%E3%82%8B%E6%AD%A3%E8%A6%8F%E8%A1%A8%E7%8F%BE
  grep.ret <- grep(paste0("\\", text, "$"), file, ignore.case = TRUE)
  if(length(grep.ret) > 0){return(TRUE)}else{return(FALSE)}
}

#エクセルのファイルか判断
is.excel <- function(file){
  if(is.text(".xls", file)){return(TRUE)}
  if(is.text(".xlsx", file)){return(TRUE)}
  return(FALSE)
}

#データ読み込み)
read.data <- function(file, sheet = NULL){
  #fileがNULLならNULLを返す
  if(is.null(file)){return((NULL))}
  
  #csv形式の場合
  if(is.text(".csv", file)){
    ret <- read.csv(file = file) %>% as_tibble()
    return(ret)
  }
  
  #xlsとxlsx形式の場合
  if(is.text(".xls", file) + is.text(".xlsx", file)){
    ret <- read_excel(file, sheet = sheet)
    return(ret)
  }
  
  #該当なしの場合
  stop("The extension is not csv, xls, xlsx.")
}

#excelファイルのsheet名を抜き出す
excel.sheet <- function(file){
  if(is.text(".xls", file) + is.text(".xlsx", file)){
    ret <- excel_sheets(file)
    return(ret)
  }else{
    stop("The extension is not xls, xlsx.")
  }
}

