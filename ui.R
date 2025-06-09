#ui.R
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
# Find out more about building applications with Shiny here:
#    http://shiny.rstudio.com/

library(shiny)
library(DT)
library(shinycssloaders)
# Define UI for application
navbarPage("SOP on Shiny 採種園の植栽配置の設計支援アプリ",
           tabPanel("使い方",
                    h4("簡易な使い方"),
                    br(""),
                    h4("1:「植栽クローン」タブに移動し、クローン特性表(testdata)を取り込む"),
                    h4("2:「シミュレーション」タブに移動し、採種園のサイズと植栽クローンと本数を入力する"),
                    h4("　　シミュレーション回数を選択し、「実行」する"),
                    h5("　　植栽本数（クローン数×ラメート数）が行数×列数より少ないと動きません"),
                    h5("　　ブラウザの再読み込みアイコン等から、プログラムをリロードしてください"),
                    h4("　　シミュレーション終了時後にResult.csvファイルを保存する"),
                    h4("3:「結果表示」タブに移動し、Result.csvファイルを読み込む"),
                    h4("　　配置タブに、近親交配を抑制した配置が出力される"),
                    h4("4:「配置をダウンロード」ボタンで配置をダウンロードする"),
                    br(""),
                    br(""),
                    h5("クローン特性表は一レコード（系統）、一行のフォーマットで整え、CSV形式で保存しておく"),
                    h5("列は左端から、系統名、通し番号、系統コード、母親系統コード、父親系統コード、形質1～5の順(testdataを参考に)"),
                    h5("現在のバージョンでは血縁関係のみに依存して配置を決定します。形質情報はなくても構いません"),
                    h5("最適配置の算出アルゴリズムは近日中に改良予定です")),
           tabPanel("1.植栽クローン",
    h4("植栽クローン特性情報の取り込みと可視化"),
           
# Input data with Slidebarlayout 
    sidebarLayout(
      sidebarPanel(
        fileInput("file","クローン特性表をアップロード", accept = (".csv")),
        br(""),
        selectInput("Choi1",h4("特性値を表示する形質"),choices=list("形質1"=6,"形質2"=7,"形質3"=8, "形質4"=9,"形質5"=10),selected = 1),
        h6("(右の特性値パネルに表示)"),
        selectInput("Choi2",h4("相関を表示する形質"),choices=list("形質1"=6,"形質2"=7,"形質3"=8, "形質4"=9,"形質5"=10),selected = 2),
        h6("(右の散布図パネルに表示)"),
        ),
      
 # Main panel
      mainPanel(
        tabsetPanel(type="tabs",
                    tabPanel("クローン特性表",dataTableOutput("table")),
                    tabPanel("特性値",plotOutput("plotBar01")),
                    tabPanel("散布図",plotOutput("plotCor01")),
                    tabPanel("クローン血縁行列",h4("0.00:血縁関係なし, 0.50:全兄弟or親子, 0.25:半兄弟"),verbatimTextOutput("ICM")),
                    tabPanel("クローン血縁Map",h4("読み込んだ系統の総当たり血縁マップ"),h4("血縁関係がない場合は黄色、血縁関係が濃いほど赤色に近づく"),plotOutput("ICM2")),
                    )))),
  tabPanel("2. シミュレーション",
           h2("シミュレーションの設定と実行"),
           sidebarLayout(
             sidebarPanel(
               sliderInput("R","行数", min = 1, max = 20, value = 3),
               sliderInput("C","列数", min = 1, max = 20, value = 3),
               sliderInput("NoTC","導入クローン数", min = 1, max = 40, value = 9),
               sliderInput("NoR","クローンあたりラメート数", min = 1, max = 20, value = 1),
               selectInput("NoS",h5("シミュレーション回数"),
                           choices = list(1,10,100,1000,10000,100000),selected = 10),
               actionButton("doSim",h4("シミュレーションを実行")),
               h5("シミュレーション終了時にダウンロードボタンが現れます"),
               withSpinner( #test
               downloadButton('download_data01','Download')
                          ,type=1),#test
              br(""),
               wellPanel(
                 h3("オプション設定  工事中"),
                 selectInput("Check1",h4("詳細な結果の出力"),choices=list("工事中"=1,"なし"=0),selected=0),
                 selectInput("Check2",h4("距離依存花粉飛散"),choices=list("あり"=1,"工事中"=0),selected = 1),
                 selectInput("Check3",h4("近交弱勢"),choices=list("あり"=1,"工事中"=0),selected = 1),
                 selectInput("Check4",h4("フェノロジ―重み付け"),choices=list("工事中"=1,"なし"=0),selected=0),
               )),
           
           # Main panel
           mainPanel(
                    tabsetPanel(type="tabs",
                                tabPanel("設定",
                                         h4("採種園サイズ"),plotOutput("plot03",heigh=300,width=300),
                                        h4(textOutput("OutTextBox1")),
                                        h4(verbatimTextOutput("CpoolTable")),
                                        h4(textOutput("OutTextBox3")),
                                        h4("苗木本数が植栽箇所数以上であることを確認してください"),
                                        br(""),
                                        h4(textOutput("OutTextBox5"))),
                           tabPanel("植栽系統",tableOutput("table2")),
                           tabPanel("各系統の種子親としての寄与割合",plotOutput("plot04",heigh=300,width=300),
                                    "採種園構成クローンを種子生産量順に並べてプロット。種子生産量の偏りを表示"),
                           ),))),
  tabPanel("3. 結果表示",
           h2("シミュレーションの結果"),
              sidebarLayout(
                sidebarPanel(
                  fileInput("file2","結果ファイルをアップロード", accept = (".csv")),
                  h5("シミュレーション上の最適配置をダウンロード"),
                  downloadButton('download_data02','Download')
              ),
                mainPanel(
                  tabsetPanel(type="tabs",
                              tabPanel("シミュレーション結果",DT::dataTableOutput("table3")),
                              tabPanel("配置",verbatimTextOutput("SOPlot01"),"シミュレーション中で最も近親交配が少ないと考えられる配置"),
                              tabPanel("系統使用回数",verbatimTextOutput("SOPlot02")),
                              tabPanel("評価値の頻度分布",plotOutput("plot05")),
                              tabPanel("評価値の時系列プロット",plotOutput("plot06")),
                  )
                )
           )
  ),

)
