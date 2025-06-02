#server.R
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
# Find out more about building applications with Shiny here:
#    http://shiny.rstudio.com/

library(shiny)
source("SOfunction01.R")
source("IBmatrix01.R")
options(shiny.reactlog=TRUE)

function(input, output,session) {
　#諸条件の設定
  vals<- reactiveValues()
  output$OutTextBox1 <- renderText({paste0("植栽箇所数:[",input$R*input$C,"] = 行数:[",input$R,"] x 列数:[",input$C,"]")})
  output$OutTextBox3 <- renderText({paste0("苗木本数:[",input$NoTC*input$NoR,"] = 導入クローン数 :[",input$NoTC,"] x ラメート数:[",input$NoR,"]")})
  output$CpoolTable <- renderPrint({summary(factor(rep(1:input$NoTC,input$NoR)))})#
  output$OutTextBox5 <- renderText({paste0("シミュレーション回数 :[",input$NoS,"]")})
  #植栽配置の形状を〇で確認
  output$plot03 <- renderPlot({
    x1<- rep(1:input$C,input$R)
    y1<- rep(1:input$R,each=input$C)
    plot(x1,y1,xlab="列",ylab="行",pch=1, bty="n", xaxt="n", yaxt="n")
  })
  #クローン特性表を読込、格納、表示
  observeEvent(input$file,{
    csv_file <- reactive(read.csv(input$file$datapath))
    D2 <- data.frame(csv_file())#読み込んだクローン特性表をD2として格納
    vals$BT02 <- D2#シミュレーション用データとしてBT02として格納
    output$table <- DT::renderDataTable(D2)#D2を表示するため、tableを準備
    output$table2 <- renderTable(D2[1:input$NoTC,])#シミュに使用するクローンのみのtable2を作成
    #各形質のBarPlot  
      output$plotBar01 <- renderPlot({
      barplot(D2[,as.numeric(input$Choi1)],xlab=colnames(D2[as.numeric(input$Choi1)]),names.arg=c(D2[,2]))
      })
      #2形質の散布図
      output$plotCor01 <- renderPlot({
        plot(D2[,as.numeric(input$Choi1)],D2[,as.numeric(input$Choi2)],xlab=colnames(D2[as.numeric(input$Choi1)]), ylab=colnames(D2[as.numeric(input$Choi2)]))
      })
        #血縁行列
  output$ICM <- renderPrint({
    IBfunc(D2$Clone,D2$Math,D2$Fath)
    })
  output$ICM2 <- renderPlot({
    heatmap(IBfunc(D2$Clone,D2$Math,D2$Fath),Colv = NA,Rowv = NA)
  })
  #各親系統の種子プールへの累積貢献割合
    output$plot04 <- renderPlot({
    x1<- D2[(1:input$NoTC),7]
    x2<- sort(x1,decreasing=T)
    x2<- x2/sum(x2)
    x2<- cumsum(x2)
    plot(x2,xlab="親　数", ylab="貢献割合",ylim=c(0,1),xlim=c(1,input$NoTC))
  })
  })
  #sim用のパラメータの準備と実行
  observeEvent(input$doSim,{
   R <- input$R
   C <- input$C
   RC = R*C
   NoTC <- as.numeric(input$NoTC)
   NoR <- as.numeric(input$NoR)
   NoS <- as.numeric(input$NoS)
   BT02 <- vals$BT02
  #6 シミュレーションに用いる植栽木データの格納データフレーム(DFFS:DataFrameForSimulation)の作成
  DFFS <- data.frame(PPID = 1:RC)#PPID: Planting Position ID 
     
  #7 DFFSに行列位置を追記（各個体の植栽位置番号と行列位置を対応させる）
    DFFS$R <- rep(1:R)
    DFFS$C <- rep(1:C,each=R)
      
      #8 植栽木を抽出するための抽出プール(Cpool)作成
      Cpool <- rep(1:NoTC,NoR)#Number of Total Clones x Number of Ramets
      
      #9 採種園サイズに応じた各植栽木間の距離行列の作成。
      #（各個体への周囲木からの距離を花粉親としての寄与率の算出に用いる）
      RD <- data.frame(matrix(NA_integer_, nrow=RC, ncol = RC))
      RD =  as.matrix( dist(DFFS[, c("R", "C")], diag=T, upper=T))
    #RD0 <- RD*0+1#個体間距離の影響を無視する場合は、全要素を1にする。デフォルトは距離の影響あり
      
      #10 距離行列を距離依存の減衰行列に変換（距離が２倍になると寄与が1/2になる様に減衰）
      RPEM1 =1/(2^(RD-1)) #Rerative pollen efficiency matrix,一本離れると花粉寄与が1/2
      diag(RPEM1) = 0#自分との自殖を花粉貢献に含めるか？ここでは含めない。
      #12 シミュレーション結果の格納庫の作成
     results <- c()
     results = matrix(NA_integer_, nrow = NoS, ncol = 2+RC)
      
      #13 シミュレーションの実行
      for( i in 1:NoS){
        results[i,] = one_sim( R, C, BT02 , DFFS, RPEM1, Cpool)
      }
      #14 シミュレーション結果の格納
      results = as.data.frame(results)
      results$SimID <- (1:NoS)
      colnames(results) = c("ACP","WACP1")
      #結果　ACP:集団同祖係数，WACP1距離重みづけ集団同祖係数1
      
      #15 結果をcsvに書き出し
      write.csv(results, file="Result.csv")
    }) 

#出力された結果を再度読みこんで、結果を表示
  observeEvent(input$file2,{
    csv_file2 <- reactive(read.csv(input$file2$datapath))
    output$table3 <- DT::renderDataTable(csv_file2())
     D3 <- data.frame(csv_file2())
     D31 <- D3[ order( D3$WACP1, decreasing = F), ]#近交係数で昇順
     SOPlot <- D31[1,4:(3+((input$R)*(input$C)))]#最良のクローン構成を抽出
     SOPlot <- matrix(SOPlot,nrow=input$R) 
    output$SOPlot01 <- renderPrint({SOPlot})#配置を行列として出力
    output$download_data01 = downloadHandler(filename="SOPlot.csv",
                                              content=function(file){
                                                write.csv(SOPlot,file)}) #ダウンロードデータの作成
    output$SOPlot02 <- renderPrint({summary(factor(as.numeric(SOPlot)))})#各系統の使用回数
    output$plot05 <- renderPlot({hist(D3$WACP1,xlab="近親交配の指標WACP",ylab="　頻　度　",main=(""))})#評価値の頻度分布
    output$plot06 <- renderPlot({plot(D3[,1],D3$WACP1,ylab="近親交配の指標WACP",xlab="シミュレーション回数")})#評価値の時系列プロット
  })
}