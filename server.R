#ライブラリ読み込み
library(shiny)
library(readxl)
library(dplyr)
library(DT)

#距離の計算方法
name.vec.method <- c("Euclidean", "Maximum", "Manhattan", "Canberra", "Binary", "Minkowski")



#ファイルを読み込む関数
source("read.data.R")
source("ezclustering.R")



#Maximum upload size exceededを回避
#100MB設定
#https://github.com/rstudio/shiny-examples/blob/master/066-upload-file/server.R
#https://stackoverflow.com/questions/18037737/how-to-change-maximum-upload-size-exceeded-restriction-in-shiny-and-save-user
options(shiny.maxRequestSize = 100*1024^2)


#本体
shinyServer(function(input, output, session) {

  #ファイルを選択した場合
  observeEvent(input$file, {
    
    print(input$file$name)
    
    
    #ファイルがxlsかxlsxの場合
    if(is.excel(input$file$name)){
      #xlsかxlsxの場合、シートを選択する
      output$sheet <- renderUI({
        selectInput("sheet", "Excel Sheet",
                    choices = excel.sheet(file = input$file$name))
      })
    }
    
    #ファイル読み込み
    raw.data <- reactive({read.data(input$file$datapath, input$sheet)})
    
    #データテーブルを表示
    #https://github.com/rstudio/shinydashboard/issues/22
    output$data = DT::renderDataTable(raw.data(), options = list(autoWidth = TRUE))
    
    #数値データのみを抜き出す
    num.data <- reactive({raw.data() %>% select_if(is.numeric)})
    
    #クラスタリングに使用する変数を選択
    output$xdata <- renderUI({
      checkboxGroupInput("xdata", 
                         label = "Variable",
                         choices = colnames(num.data()),
                         selected = colnames(num.data()))
    })
    
    #クラスタリングにつけるラベルをどうするか？
    output$ydata <- renderUI({
      selectInput("ydata", label = "Label", 
                  choices = colnames(raw.data()))
    })
    
    #距離の計算方法を選択
    output$dist.method <- renderUI({
      selectInput("dist.method", "Distance Measure to be used",
                  choices = name.vec.method)
    })
      
    #スケーリングとセンタリングを行うか
    output$scale <- renderUI({
      checkboxInput("scale", "Scaling and Centering", value = TRUE)
    })
    
    #階層クラスタリングの手法選択
    output$hclust.method <- renderUI({
      selectInput("hclust.method", "Agglomeration method to be used",
                  choices = c("Ward", "Single", "Complete", "Average", 
                              "McQuitty", "Median", "Centroid"))
      
    })
    
    #クラスター数
    output$hclust.k <- renderUI({
      numericInput("hclust.k", "Clusters", 2, min = 2, max = nrow(raw.data()))
      
    })
    
    #変数を選択して距離を計算
    dist.data <- reactive({
      dist.calc(df = raw.data(),
                method = input$dist.method,
                xdata = input$xdata,
                ydata = input$ydata,
                scale = input$scale)
      
    })
    
    #階層クラスタリング
    hclust.data <- reactive({
      clust(dist.data(), method = input$hclust.method)
    })
    
    #デンドログラム
    output$hclust.plot <- renderPlot({
      #https://qiita.com/nozma/items/b6924a96f838f485f89b
      plot(as.dendrogram(hclust.data()))
      
      rect.hclust(
        hclust.data(), 
        k = input$hclust.k, 
        border = "red")
    })
    

    #変数重要度一覧のダウンロード
    output$downloadData = downloadHandler(
      filename = "clustering.csv",
      content = function(filename) {
        write.csv(
          data.frame(
            raw.data(),
            Cluster = cutree(hclust.data(), k = input$hclust.k)
            ), filename)
        }
    )
    
  })



})







