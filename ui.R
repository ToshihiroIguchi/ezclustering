#ライブラリ読み込み
library(shiny)

#読み込むファイルのタイプ
file.accept <- c(".csv", ".xls", ".xlsx")


shinyUI(fluidPage(

  # Application title
  titlePanel(h3("Easy Clustering")),

  sidebarLayout(
    sidebarPanel(
      
      #ファイル読み込み
      fileInput("file", "Data file (.csv, .xls, xlsx)", accept = file.accept),
      
      #シート選択
      htmlOutput("sheet"),
      
      #変数選択
      htmlOutput("xdata"),
      
      #ラベル選択
      htmlOutput("ydata"),
      
      
      #距離の計算
      htmlOutput("dist.method"),
      
      #スケーリングとセンタリングを行うか
      htmlOutput("scale"),
      
      #クラスタリングのアルゴリズム選択
      htmlOutput("hclust.method"),
      
      #クラスタ数
      htmlOutput("hclust.k")
    ),

    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(type = "tabs",
                  
                  #ファイルの内容をそのまま表示
                  tabPanel("Data",
                           #https://code.i-harness.com/ja/q/227d360
                           tags$head(tags$link(
                             rel = "stylesheet", type = "text/css", href = "my.css")),
                           DT::dataTableOutput("data")
                           ),
                  
                  tabPanel("Dendrogram",
                           plotOutput("hclust.plot"),
                           downloadButton('downloadData', 'Table download')
                           ),
                  tabPanel("References", includeMarkdown("reference.md"))
                  )
      )
    )
  )
)