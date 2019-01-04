options(warn=-1)
# change the maximum size restriction
options(shiny.maxRequestSize=50*1024^2)

#SERVER
shinyServer(function(input,output,session){
  
  # Structure Prior
  RecP <- reactive({
    file <- input$inFile
    if(!is.null(file)){
      data <- read.csv(file$datapath, header = input$inHeader,colClasses = "factor")
      nodelist = colnames(data)
    }
    
    if(input$AddButtonP == n_AP + 1) {
      n_AP <<- n_AP + 1
      if(input$in_From == "All nodes" & input$in_To != "All nodes"){
        Pri_tab <<- rbind(Pri_tab,data.frame(From=nodelist,To=input$in_To,Type=input$BorW,stringsAsFactors=FALSE))
      }else if(input$in_From != "All nodes" & input$in_To == "All nodes"){
        Pri_tab <<- rbind(Pri_tab,data.frame(From=input$in_From,To=nodelist,Type=input$BorW,stringsAsFactors=FALSE))
      }else if(input$in_From != "All nodes" & input$in_To != "All nodes"){
        Pri_tab <<- rbind(Pri_tab,data.frame(From=input$in_From,To=input$in_To,Type=input$BorW,stringsAsFactors=FALSE))
      }
    }
    if(input$delButtonP == n_DP + 1) {
      n_DP <<- n_DP + 1
      if(input$in_From == "All nodes" & input$in_To != "All nodes"){
        indexP = which(Pri_tab$To == input$in_To & Pri_tab$Type == input$BorW)
      }else if(input$in_From != "All nodes" & input$in_To == "All nodes"){
        indexP = which(Pri_tab$From == input$in_From & Pri_tab$Type == input$BorW)
      }else if(input$in_From != "All nodes" & input$in_To != "All nodes"){
        indexP = which(Pri_tab$From == input$in_From & Pri_tab$To == input$in_To & Pri_tab$Type == input$BorW)
      }
      if(length(indexP)) Pri_tab <<- Pri_tab[-indexP,]
    }
    Pri_tab <<- Pri_tab[which(Pri_tab$From != Pri_tab$To),]
    Pri_tab <<- Pri_tab[!duplicated(data.frame(Pri_tab$From,Pri_tab$To,Pri_tab$Type), fromLast=TRUE), ]
    rownames(Pri_tab) <- NULL
    Pri_tab
  })
  output$Pri_table <- renderDataTable(RecP(),class="compact",rownames = FALSE,options=list(searching=F,
                                                                                           columnDefs=list(list(className = 'dt-center', targets = 1))))
  
  
  # Input the network in class bn.fit
  recFit <- reactive({
    bn_fit <- NULL
    if(input$inType=='R Object in R'){
      if(!is.null(input$inFit) & ! input$inFit == ""){
        inF1 <- unlist(strsplit(input$inFit,",",fixed=T))[1]
        inF2 <- unlist(strsplit(input$inFit,",",fixed=T))[2]
        if(exists(inF1,mode="list")){
          if("bn.fit" %in% class(get(inF1)) | "bn" %in% class(get(inF1))){
            bn_fit <- get(inF1)
            if(! is.na(inF2)){
              if(exists(inF2,mode="list")){
                if(! "data.frame" %in% class(get(inF2))) vals$Input_ERROR <- 1.4
              }else vals$Input_ERROR <- 1.3	    
            }
          }
          else{
            vals$Input_ERROR <- 1.2
            bn_fit <- NULL
          }
        }else vals$Input_ERROR <- 1.1
      }
    }
    else if(input$inType=='R Object(.Rdata)') {
      obj <- input$inObject
      if(! is.null(obj)){
        n_char <- nchar(obj$name)
        if(substr(obj$name,n_char-4,n_char) == "rdata"){
          a <- load(obj$datapath)
          load(obj$datapath)
          if("bn.fit" %in% class(get(a[1])) | "bn" %in% class(get(a[1])))  bn_fit <- get(a[1])
          else bn_fit <- get(a[2])
          if(! "bn.fit" %in% class(bn_fit) && ! "bn" %in% class(bn_fit)) bn_fit <- NULL
        }
        else vals$Input_ERROR <- 2
      }
    }
    else if(input$inType=='Raw Data(.csv)'){
      file_in <- input$inFile
      if(! is.null(file_in)){
        n_char <- nchar(file_in$name)
        if(substr(file_in$name,n_char-2,n_char) == "csv"){
          data <- read.csv(file_in$datapath, header = input$inHeader,colClasses = "factor")
          if(! is.null(data)){
            
            # Prior information
            if(input$GoButtonP == n_GP + 1){
              n_GP <<- n_GP + 1
              Prior <- RecP()
              colnames(Prior) <- c("from","to","type")
              white <<- Prior[Prior$type == "whitelist",]
              white <<- white[,-3]
              if(nrow(white) == 0) white <<- NULL
              
              black <<- Prior[Prior$type == "blacklist",]
              black <<- black[,-3]
              if(nrow(black) == 0) black <<- NULL
            }
            
            if(input$inLearnType == 'Constraint-Based Algorithms'){
              if(input$inLearn1 == 'Grow-Shrink') dag <- gs(data,blacklist = black,whitelist = white)
              else if(input$inLearn1 == 'Incremental Association') dag <- iamb(data,blacklist = black,whitelist = white)
              else if(input$inLearn1 == 'Fast Incremental Association') dag <- fast.iamb(data,blacklist = black,whitelist = white)
              else if(input$inLearn1 == 'Interleaved Incremental Association') dag <- inter.iamb(data,blacklist = black,whitelist = white)
              # else if(input$inLearn1 == 'Max-Min Parents and Children') dag <- mmpc(data,blacklist = black,whitelist = white)
              # else if(input$inLearn1 == 'Semi-Interleaved HITON-PC') dag <- si.hiton.pc(data,blacklist = black,whitelist = white)
            }
            else if(input$inLearnType == 'Score-Based Algorithms'){
              if(input$inLearn2 == 'hill-climbing') dag <- hc(data,score=input$inScore2,blacklist = black,whitelist = white)
              else if(input$inLearn2 == 'tabu search') dag <- tabu(data,score=input$inScore2,blacklist = black,whitelist = white)
            }
            else if(input$inLearnType == 'Hybrid Algorithms'){
              if(input$inLearn3 == 'Max-Min Hill Climbing') dag <- mmhc(data,blacklist = black,whitelist = white)
              else if(input$inLearn3 == '2-phase Restricted Maximization') dag <- rsmax2(data,blacklist = black,whitelist = white)
            }
            else if(input$inLearnType == 'Bootstrap'){
              if(input$inLearn4 %in% c('gs','iamb','fast.iamb','inter.iamb','mmhc','rsmax2')){
                boot = boot.strength(data=data,R = as.numeric(input$N_Boot),algorithm = input$inLearn4,
                                     algorithm.args = list(blacklist = black,whitelist = white))
              }else{
                boot = boot.strength(data=data,R = as.numeric(input$N_Boot),algorithm = input$inLearn4,
                                     algorithm.args = list(score=input$inScore4,blacklist = black,whitelist = white))
              }
              boot[(boot$strength > 0.85) & (boot$direction >=0.5),]
              dag = averaged.network(boot,threshold = 0.85)
            }
            if(nrow(undirected.arcs(dag))) vals$Input_ERROR <- 5
            bn_fit <- try(bn.fit(dag,data,method = input$inMethod))
            if(! "bn.fit" %in% class(bn_fit)) bn_fit <- NULL
          }
        }
        else vals$Input_ERROR <- 3
      }
    }
    bn_fit
  })
  
  #Read the data to compute edges strength
  recStrength <- reactive({
    bn_Strength <- NULL
    if(input$inType=='R Object in R'){
      if(!is.null(input$inFit)){
        inS <- unlist(strsplit(input$inFit,",",fixed=T))[1]
        if("bn.fit" %in% class(get(inS)) | "bn" %in% class(get(inS)))  bn_data <- get(unlist(strsplit(input$inFit,",",fixed=T))[2])
        else bn_data <- get(inS)
      }
    }
    else if(input$inType=='R object(.Rdata)') {
      obj <- input$inObject
      if(! is.null(obj)){
        a <- load(obj$datapath)
        load(obj$datapath)
        if("bn.fit" %in% class(get(a[1])) | "bn" %in% class(get(a[1])))  bn_data <- get(a[2])
        else bn_data <- get(a[1])
      }
    }
    else if(input$inType=='Raw Data(.csv)'){
      file <- input$inFile
      if(! is.null(file)){
        bn_data <- read.csv(file$datapath, header = input$inHeader,colClasses = "factor")
      }
    }
    if(is.data.frame(bn_data)){
      fit <- recFit()
      if(! is.null(fit)){
        if("bn.fit" %in% class(fit)) x <- bn.net(fit)
        else x <- fit
      } 
      if(input$IE_Criterion == 'Independence Test'){
        bn_Strength <- arc.strength(x,bn_data,criterion=input$IE_Independence)
        if(! "bn.strength" %in% class(bn_Strength)) bn_Strength <- NULL
        else bn_Strength$strength <- bn_Strength$strength*-1
      }else{
        bn_Strength <- arc.strength(x,bn_data,criterion=input$IE_Score)
        if(! "bn.strength" %in% class(bn_Strength)) bn_Strength <- NULL
      }
    }
    bn_Strength
  })
  
  #Structure input
  RecContinue <- reactive({
    in_file <- input$inContinue
    Continue <- NULL
    if(! is.null(in_file)){
      n_char <- nchar(in_file$name)
      if(substr(in_file$name,n_char-2,n_char) == "xls" | substr(in_file$name,n_char-3,n_char) == "xlsx"){
        Node <- read.xlsx2(in_file$datapath, header = T,sheetName = "Nodes",stringsAsFactors=F)
        Edge <- read.xlsx2(in_file$datapath, header = T,sheetName = "Edges",stringsAsFactors=F)
        if(! is.null(Node) & ! is.null(Edge)){
          Continue <- list(Node=Node,Edge=Edge)
        }
      }
      else vals$Input_ERROR <- 4
    }
    if(input$inType != "Structure in Excel") Continue <- NULL
    Continue
  })
  
  output$input_ERROR <- renderText({
    if(input$ClearLog == n_LogClear + 1){
      n_LogClear <<- n_LogClear + 1
      out_text <<- "Log:\n"
    }
    if(vals$Input_ERROR == 1.1) {
      inF1 <<- unlist(strsplit(input$inFit,",",fixed=T))[1]
      out_text <<- paste0(out_text,"\tError: object '",inF1,"' not found.\n")
      vals$Input_ERROR <- 0
    }
    if(vals$Input_ERROR == 1.2) {
      inF12 <- unlist(strsplit(input$inFit,",",fixed=T))[1]
      out_text <<- paste0(out_text,"\tError: object '",inF12,"' is not a 'bn' or 'bn.fit' class.\n")
      vals$Input_ERROR <- 0
    }
    if(vals$Input_ERROR == 1.3) {
      inF2 <- unlist(strsplit(input$inFit,",",fixed=T))[2]
      out_text <<- paste0(out_text,"\tError: object '",inF2,"' not found.\n")
      vals$Input_ERROR <- 0
    }
    if(vals$Input_ERROR == 1.4) {
      inF2 <- unlist(strsplit(input$inFit,",",fixed=T))[2]
      out_text <<- paste0(out_text,"\tError: object '",inF2,"' is not a data.frame.\n")
      vals$Input_ERROR <- 0
    }
    if(vals$Input_ERROR == 2) {
      out_text <<- paste0(out_text,"\tError: Please input a 'rdata' format file.\n")
      vals$Input_ERROR <- 0
    }
    if(vals$Input_ERROR == 3) {
      out_text <<- paste0(out_text,"\tError: Please input a 'csv' format file.\n")
      vals$Input_ERROR <- 0
    }
    if(vals$Input_ERROR == 4) {
      out_text <<- paste0(out_text,"\tError: Please input a Excel.\n")
      vals$Input_ERROR <- 0
    }
    out_text
  })
  #Reactive UI output
  output$evidence <- renderUI({
    fit <- recFit()
    if(! is.null(fit) & "bn.fit" %in% class(fit)){
      Nodelist = nodes(fit)
      selectInput("inEvidence","Select the Evidence nodes:",Nodelist)
    }
  })
  
  output$E_value <- renderUI({
    fit <- recFit()
    if(input$Q_Intype == "Click graph"){
      Click <- input$InClick
      if(length(Click)) tmp <- fit[[Click[length(Click)]]]
      else tmp = NULL
      valuelist <- rownames(tmp$prob)
    }else{
      if(! is.null(input$inEvidence)) tmp <- fit[[input$inEvidence]]
      else tmp <- NULL
      valuelist <- rownames(tmp$prob)
    }
    if(length(valuelist)) radioButtons("inEValue","Value of Evidence nodes:",choices=valuelist)
  })
  
  output$query <- renderUI({
    fit <- recFit()
    if(! is.null(fit) & "bn.fit" %in% class(fit)){
      Nodelist = nodes(fit)
      selectInput("inQuery","Select the Query nodes:",Nodelist)
    }
  })
  
  output$Q_value <- renderUI({
    fit <- recFit()
    if(! is.null(fit) & ! is.null(input$inQuery) & "bn.fit" %in% class(fit)){
      tmp = fit[[input$inQuery]]
      valuelist = rownames(tmp$prob)
      if(length(valuelist)) checkboxGroupInput("inQValue","Value of Query nodes:",choices=valuelist,selected=valuelist)
    }
  })
  
  output$from <- renderUI({
    file <- input$inFile
    if(! is.null(file)){
      data <- read.csv(file$datapath, header = input$inHeader,colClasses = "factor")
      Nodelist = c("All nodes",colnames(data))
      selectInput("in_From","From:",Nodelist)
    }
  })
  
  output$to <- renderUI({
    file <- input$inFile
    if(! is.null(file)){
      data <- read.csv(file$datapath, header = input$inHeader,colClasses = "factor")
      Nodelist = c("All nodes",colnames(data))
      selectInput("in_To","To:",Nodelist)
    }
  })
  
  output$N <- renderUI({
    fit <- recFit()
    Cont<- RecContinue()
    if(! is.null(fit) | ! is.null(Cont)){
      if(! is.null(fit)) Nodelist =nodes(fit)
      else Nodelist = Cont[["Node"]]$id
      selectInput("inNodes","Select the Nodes:", c(Nodelist,"Markov blanket of:"))
    }
  })
  
  output$N_mb <- renderUI({
    fit <- recFit()
    Cont<- RecContinue()
    if(! is.null(fit) | ! is.null(Cont)){
      if(! is.null(fit)) Nodelist =nodes(fit)
      else Nodelist = Cont[["Node"]]$id
      selectInput("inN_mb","Markov blanket of:", c(Nodelist))
    }
  })
  
  output$E <- renderUI({
    fit <- recFit()
    Cont<- RecContinue()
    if(! is.null(fit) | ! is.null(Cont)){
      if(! is.null(fit)){
        e = as.data.frame(arcs(fit))
        edgeslist = paste(e$from,"~",e$to,sep = '')
      }
      else edgeslist = paste(Cont[["Edge"]]$from,"~",Cont[["Edge"]]$to,sep = '')
      
      selectInput("inEdges","Select the Edges:",edgeslist)
    }
  })
  
  output$In_Nodetype <- renderUI({
    if(input$inType == "Structure in Excel") U <- radioButtons ("N_Intype","Select nodes by:",c("Click graph","Group in Excel","List"),inline = T)
    else U <- radioButtons ("N_Intype","Select nodes by:",c("Click graph","List"),inline = T)
    U
  })
  
  output$Node_Group <- renderUI({
    Cont<- RecContinue()
    if(! is.null(Cont))({
      group <- Cont[["Node"]]$group
      if(! all(group == "NA")) selectInput("In_Ngroup","Select group:",unique(group))
    })
  })
  
  output$Edge_Group <- renderUI({
    Cont<- RecContinue()
    if(! is.null(Cont))({
      group <- Cont[["Edge"]]$group
      if(! all(group == "NA")) selectInput("In_Egroup","Select group:",unique(group))
    })
  })
  
  output$E_Render <- renderUI({
    if(input$IE_size_type == 'Arc Strength') Renderlist <- c("Edge Color","Edge Type")
    else Renderlist <- c("Edge Color","Edge Type","Edge Width")
    selectInput("Edges_type","Select the Type:",Renderlist)
  })
  
  recSci_Pic <- reactive({
    colorlist <- NULL
    if(input$inType != "Structure in Excel"){
      if(input$IN_color_type == 'SCI-Style'){
        if(input$SCI_Name == 'NPG') colorlist <- substr(pal_npg("nrc")(10),1,7)
        else if(input$SCI_Name == 'Lancet') colorlist <- substr(pal_lancet("lanonc")(9),1,7)
        else if(input$SCI_Name == 'JAMA') colorlist <- substr(pal_jama("default")(7),1,7)
        else if(input$SCI_Name == 'NEJM') colorlist <- substr(pal_nejm("default")(8),1,7)
        else if(input$SCI_Name == 'JCO') colorlist <- substr(pal_jco("default")(10),1,7)
        else if(input$SCI_Name == 'AAAS') colorlist <- substr(pal_aaas("default")(10),1,7)
        else if(input$SCI_Name == 'D3') colorlist <- substr(pal_d3("category10")(10),1,7)
        else if(input$SCI_Name == 'UCSCGB') colorlist <- substr(pal_ucscgb("default")(11)[c(1:9,11)],1,7)
        else if(input$SCI_Name == 'LocusZoom') colorlist <- substr(pal_locuszoom("default")(7),1,7)
        else if(input$SCI_Name == 'Futurama') colorlist <- substr(pal_futurama("planetexpress")(9)[c(1:4,8:9)],1,7)
        else if(input$SCI_Name == 'Tron Legacy') colorlist <- substr(pal_tron("legacy")(7),1,7)
        else if(input$SCI_Name == 'Star Trek') colorlist <- substr(pal_startrek("uniform")(7),1,7)
        else if(input$SCI_Name == 'Google') colorlist <- c("#5380E4", "#E12A3C", "#FFBF03","#00B723")
        else if(input$SCI_Name == 'Twitter') colorlist <- c("#55ACEE", "#292f33", "#8899a6", "#e1e8ed")
        else if(input$SCI_Name == 'Facebook') colorlist <- c("#3b5998","#6d84b4", "#afbdd4", "#d8dfea")
        else if(input$SCI_Name == 'Airbnb') colorlist <- c("#FF5A5F","#FFB400", "#007A87", "#FFAA91", "#7B0051")
        else if(input$SCI_Name == 'Etsy') colorlist <- c("#F14000", "#67B6C3", "#F0DA47", "#EBEBE6", "#D0D0CB")
        else if(input$SCI_Name == '23andme') colorlist <- c("#3595D6", "#92C746", "#F2C100", "#FF6D19", "#6F3598")
      }else if(input$IN_color_type == 'Pic-Style'){
        if(! is.null(input$Pic_Name)){
          colorlist <- extract_colours(input$Pic_Name$datapath)
        }
      }
      colorlist
    }else{
      if(input$IN_color_type2 == 'SCI-Style'){
        if(input$SCI_Name2 == 'NPG') colorlist <- substr(pal_npg("nrc")(10),1,7)
        else if(input$SCI_Name2 == 'Lancet') colorlist <- substr(pal_lancet("lanonc")(9),1,7)
        else if(input$SCI_Name2 == 'JAMA') colorlist <- substr(pal_jama("default")(7),1,7)
        else if(input$SCI_Name2 == 'NEJM') colorlist <- substr(pal_nejm("default")(8),1,7)
        else if(input$SCI_Name2 == 'JCO') colorlist <- substr(pal_jco("default")(10),1,7)
        else if(input$SCI_Name2 == 'AAAS') colorlist <- substr(pal_aaas("default")(10),1,7)
        else if(input$SCI_Name2 == 'D3') colorlist <- substr(pal_d3("category10")(10),1,7)
        else if(input$SCI_Name2 == 'UCSCGB') colorlist <- substr(pal_ucscgb("default")(11)[c(1:9,11)],1,7)
        else if(input$SCI_Name2 == 'LocusZoom') colorlist <- substr(pal_locuszoom("default")(7),1,7)
        else if(input$SCI_Name2 == 'Futurama') colorlist <- substr(pal_futurama("planetexpress")(9)[c(1:4,8:9)],1,7)
        else if(input$SCI_Name2 == 'Tron Legacy') colorlist <- substr(pal_tron("legacy")(7),1,7)
        else if(input$SCI_Name2 == 'Star Trek') colorlist <- substr(pal_startrek("uniform")(7),1,7)
        else if(input$SCI_Name2 == 'Google') colorlist <- c("#5380E4", "#E12A3C", "#FFBF03","#00B723")
        else if(input$SCI_Name2 == 'Twitter') colorlist <- c("#55ACEE", "#292f33", "#8899a6", "#e1e8ed")
        else if(input$SCI_Name2 == 'Facebook') colorlist <- c("#3b5998","#6d84b4", "#afbdd4", "#d8dfea")
        else if(input$SCI_Name2 == 'Airbnb') colorlist <- c("#FF5A5F","#FFB400", "#007A87", "#FFAA91", "#7B0051")
        else if(input$SCI_Name2 == 'Etsy') colorlist <- c("#F14000", "#67B6C3", "#F0DA47", "#EBEBE6", "#D0D0CB")
        else if(input$SCI_Name2 == '23andme') colorlist <- c("#3595D6", "#92C746", "#F2C100", "#FF6D19", "#6F3598")
      }else if(input$IN_color_type2 == 'Pic-Style'){
        if(! is.null(input$Pic_Name2)){
          colorlist <- extract_colours(input$Pic_Name2$datapath)
        }
      }
      colorlist
    }
  })
  
  output$Sci_Pic_UI <- renderUI({
    selectInput("Sci_Pic_Color","Nodes Color:",recSci_Pic())
  })
  
  output$N_colorlist <- renderUI({
    if(input$inType != "Structure in Excel"){
      if(input$IN_color_type == 'Self-defined') color_list <- c("lightblue","red","orange","yellow","green","blue","Other")
      else color_list <- recSci_Pic()
    }else{
      if(input$IN_color_type2 == 'Self-defined') color_list <- c("lightblue","red","orange","yellow","green","blue","Other")
      else color_list <- recSci_Pic()
    }
    selectInput("N_color","Nodes Color:",color_list)
  })
  
  output$Main <- renderUI({
    if(input$inType == 'Structure in Excel'){
      ui <- column(width=12,
                   svgPanZoomOutput("outSVG",height = "650px",width = "100%"))
    }else{
      ui <- {
        column(width=12,
               column(width=7,
                      jqui_resizabled(
                        tabBox(side = "left", width = NULL,height = NULL,selected = "Network",
                               tabPanel("Network",
                                        svgPanZoomOutput("outSVG",height = "500px",width = "100%")),
                               tabPanel("Evidence",
                                        dataTableOutput("Evi_table")
                               )
                        ))),
               column(width=5,
                      jqui_resizabled(
                        tabBox(side = "left", width = NULL,height = NULL,selected = "Graph",
                               tabPanel("Graph",
                                        plotOutput("ResultPlot",height = "500px",width = "100%")),
                               tabPanel("Table",
                                        dataTableOutput("Result")),
                               tabPanel("Grade Color",
                                        column(width=12,
                                               column(width=10,strong("Select grade interval and color:")),
                                               column(width=2,checkboxInput("GC_TF","Yes?",FALSE))),
                                        conditionalPanel(
                                          condition = "input.GC_TF == true",
                                          column(width=6,textInput("GC_Interval",h4("Input the interval:"),"33,66")),
                                          column(width=6,textInput("GC_Color",h4("Input the color:"),"green,orange,red")),
                                          column(width=12,helpText("Notes: Separated by a comma.")),
                                          column(width=6,textInput("GC_Label",h4("Input the label:"),"Low,Middle,High")),
                                          column(width=6,sliderInput("RLegend_TextSize","Label Size:",min=8,max=16,value=10,step=0.5)),
                                          column(width=12,helpText("Notes: Separated by a comma. If not, please input NULL.")),
                                          column(width=12,
                                                 column(width=6,numericInput("RLegend_x","Input the X position:",90,min=0,max = 100)),
                                                 column(width=6,numericInput("RLegend_y","Input the Y position:",90,min=0,max = 100))
                                          ),
                                          column(width=12,helpText("Notes:For x: Number from 0 (left) to 100 (right); For y: Number from 0 (bottom) to 100 (top)."))
                                        )
                               )
                        ))))
      }
    }
    ui
  })
  
  output$Choose_download <- renderUI({
    if(input$shinyBN_choose == n_NetDownload + 2) n_NetDownload <<- n_NetDownload +2
    if(input$shinyBN_choose > n_NetDownload)
      box(width=12,
          downloadLink("shinyBN.html","Figure in HTML"),
          downloadLink("shinyBN.xlsx","Structure in Excel"))
  })
  
  # Add Nodes and Edges Render!
  recNode <- reactive({
    
    if(input$AddButtonNodes == n_ANode + 1 | input$DelButtonNodes == n_DNode + 1){
      if(input$N_Intype == "Click graph"){
        inNodes = input$InClick
      }
      else if(input$N_Intype == "Group in Excel"){
        Con <- RecContinue()[["Node"]]
        inNodes = Con[Con$group == input$In_Ngroup,"id"]
      }
      else{
        if(input$inNodes != "Markov blanket of:") inNodes = input$inNodes
        else{
          fit <- recFit()
          Cont<- RecContinue()
          if(! is.null(fit) | ! is.null(Cont)){
            if(! is.null(fit)) inNodes = mb(fit,input$inN_mb)
            else{
              Edges <- Cont[["Edge"]]
              Father <- Edges$from[Edges$to == input$inN_mb]
              children <- Edges$to[Edges$from == input$inN_mb]
              Co_Child <- Edges$from[Edges$to %in% children]
              inNodes <- unique(c(Father,children,Co_Child))
              inNodes <- inNodes[inNodes != input$inN_mb]
            }
          }
        }
      }
    }
    
    if(input$AddButtonNodes == n_ANode + 1) {
      n_ANode <<- n_ANode + 1
      
      if(! is.null(inNodes)){
        if(input$Nodes_type == 'Node Color'){
          if(input$N_color != "Other")
            Ncolorsize_tab <<- rbind(Ncolorsize_tab,data.frame(Nodes = inNodes,ColorSize = input$N_color,
                                                               Type = input$Nodes_type,stringsAsFactors=FALSE))
          else
            Ncolorsize_tab <<- rbind(Ncolorsize_tab,data.frame(Nodes = inNodes,ColorSize = input$N_Other_color,
                                                               Type = input$Nodes_type,stringsAsFactors=FALSE))
        }else if(input$Nodes_type == 'Label Size'){
          Ncolorsize_tab <<- rbind(Ncolorsize_tab,data.frame(Nodes = inNodes,ColorSize = input$N_Tsize,
                                                             Type = input$Nodes_type,stringsAsFactors=FALSE))
        }else if(input$Nodes_type == 'Label Color'){
          Ncolorsize_tab <<- rbind(Ncolorsize_tab,data.frame(Nodes = inNodes,ColorSize = as.character(input$N_Tcolor),
                                                             Type = input$Nodes_type,stringsAsFactors=FALSE))
        }else
          Ncolorsize_tab <<- rbind(Ncolorsize_tab,data.frame(Nodes = inNodes,ColorSize = input$N_Nshape,
                                                             Type = input$Nodes_type,stringsAsFactors=FALSE))
      }
    }
    
    if(input$DelButtonNodes == n_DNode + 1){
      n_DNode <<- n_DNode + 1
      
      indexNode = which(Ncolorsize_tab$Nodes %in% inNodes & Ncolorsize_tab$Type == input$Nodes_type)
      if(length(indexNode)) Ncolorsize_tab <<- Ncolorsize_tab[-indexNode,]
    }
    
    if(input$ClearNodes == n_ClearN + 1){
      n_ClearN <<- n_ClearN + 1
      Ncolorsize_tab <<- data.frame()
    }
    
    Ncolorsize_tab <<- Ncolorsize_tab[!duplicated(data.frame(Ncolorsize_tab$Nodes,Ncolorsize_tab$Type), fromLast=TRUE), ]
    rownames(Ncolorsize_tab) <- NULL
    Ncolorsize_tab
  })
  output$Ncolorsize_table <- renderDataTable(recNode(),class="compact",options=list(searching=F,pageLength=5,
                                                                                    columnDefs=list(list(className = 'dt-center', targets = 1:2))))
  
  recEdge <- reactive({
    
    if(input$AddButtonEdges == n_AEdge + 1 | input$DelButtonEdges == n_DEdge + 1){

      if(input$inType != 'Structure in Excel'){
        fit <- recFit()
        Cont<- RecContinue()
        if(! is.null(fit) | ! is.null(Cont)){
          inEdges <- input$inEdges
        }
      }else if(input$E_Intype == "Group in Excel"){
        e <- RecContinue()[["Edge"]]
        edges = paste(e$from,"~",e$to,sep = '')
        inEdges = edges[e$group == input$In_Egroup]
      }
    }
    
    if(input$AddButtonEdges == n_AEdge + 1){
      n_AEdge <<- n_AEdge + 1
      if(input$Edges_type == 'Edge Color'){
        if(input$E_color != "Other")
          Ecolorsize_tab <<- rbind(Ecolorsize_tab,data.frame(Edges = inEdges,ColorSize = input$E_color,
                                                             Type = input$Edges_type,stringsAsFactors=FALSE))
        else
          Ecolorsize_tab <<- rbind(Ecolorsize_tab,data.frame(Edges = inEdges,ColorSize = input$E_Other_color,
                                                             Type = input$Edges_type,stringsAsFactors=FALSE))
      }
      else if(input$Edges_type == 'Edge Width'){
        Ecolorsize_tab <<- rbind(Ecolorsize_tab,data.frame(Edges = inEdges,ColorSize = as.character(input$E_size),
                                                           Type = input$Edges_type,stringsAsFactors=FALSE))
      }
      else Ecolorsize_tab <<- rbind(Ecolorsize_tab,data.frame(Edges = inEdges,ColorSize = input$E_type,
                                                              Type = input$Edges_type,stringsAsFactors=FALSE))
    }
    
    if(input$DelButtonEdges == n_DEdge + 1){
      n_DEdge <<- n_DEdge + 1
      indexEdge = which(Ecolorsize_tab$Edges == inEdges & Ecolorsize_tab$Type == input$Edges_type)
      if(length(indexEdge)) Ecolorsize_tab <<- Ecolorsize_tab[-indexEdge,]
    }
    
    if(input$ClearEdges == n_ClearE + 1){
      n_ClearE <<- n_ClearE + 1
      Ecolorsize_tab <<- data.frame()
    }
    
    Ecolorsize_tab <<- Ecolorsize_tab[!duplicated(data.frame(Ecolorsize_tab$Edges,Ecolorsize_tab$Type), fromLast=TRUE), ]
    rownames(Ecolorsize_tab) <- NULL
    Ecolorsize_tab
  })
  output$Ecolorsize_table <- renderDataTable(recEdge(),class="compact",options=list(searching=F,pageLength=5,
                                                                                    columnDefs=list(list(className = 'dt-center', targets = 1:2))))
  
  recRendN <- reactive({
    fit <- recFit()
    Cont<- RecContinue()
    if(! is.null(fit) | ! is.null(Cont)){
      if(! is.null(fit)){
        if(input$inType == 'R Object in R' & input$inFit == "Stroke_bnfit"){
          Node_Excel <- read.xlsx2("data/shinyBN.xlsx", header = T,sheetName = "Nodes",stringsAsFactors=F)
          nodes <- Node_Excel$id
          Ncol <- Node_Excel$color
          Nshape <- Node_Excel$shape
          Tsize  = Node_Excel$font.size
          Tcolor = Node_Excel$font.color
        }else{
          nodes <- nodes(fit)
          
          # Default parameter
          if(input$IN_color_type == 'Self-defined'){
            if(input$IN_color == 'Other') Ncol <- rep(input$IN_Other_color,length(nodes))
            else Ncol <- rep(input$IN_color,length(nodes))
          }
          else{
            Ncol <- rep(input$Sci_Pic_Color,length(nodes))
          }
          
          Tsize  <- rep(input$IN_Tsize ,length(nodes))
          Tcolor <- rep(input$IN_Tcolor,length(nodes))
          Nshape <- rep(input$IN_Nshape,length(nodes))
        }
      }
      else{
        nodes  <- Cont[["Node"]]$id
        Ncol   <- Cont[["Node"]]$color
        Tcolor <- Cont[["Node"]]$font.color
        Tsize  <- Cont[["Node"]]$font.size
        Nshape <- Cont[["Node"]]$shape
      }
    }
    
    
    Ncolorsize_tab <<- recNode()
    
    if("Node Color" %in% Ncolorsize_tab$Type){
      r1 <- Ncolorsize_tab$Type=="Node Color"
      Ncol[match(Ncolorsize_tab$Nodes[r1],nodes)] <- Ncolorsize_tab$ColorSize[r1]
    }
    if("Label Size" %in% Ncolorsize_tab$Type){
      r2 <- Ncolorsize_tab$Type=="Label Size"
      Tsize[match(Ncolorsize_tab$Nodes[r2],nodes)] <- Ncolorsize_tab$ColorSize[r2]
    }
    if("Label Color" %in% Ncolorsize_tab$Type){
      r3 <- Ncolorsize_tab$Type=="Label Color"
      Tcolor[match(Ncolorsize_tab$Nodes[r3],nodes)] <- Ncolorsize_tab$ColorSize[r3]
    }
    if("Node Shape" %in% Ncolorsize_tab$Type){
      r4 <- Ncolorsize_tab$Type=="Node Shape"
      Nshape[match(Ncolorsize_tab$Nodes[r4],nodes)] <- Ncolorsize_tab$ColorSize[r4]
    }
    
    renderN <<- list(Ncol=Ncol,Tsize=Tsize,Tcolor=Tcolor,Nshape=Nshape)
    renderN
  })
  
  recRendE <- reactive({
    fit <- recFit()
    Cont<- RecContinue()
    if(! is.null(fit) | ! is.null(Cont)){
      if(! is.null(fit)){
        if(! is.null(fit)){
          if(input$inType == 'R Object in R' & input$inFit == "Stroke_bnfit"){
            Edge_Excel <- read.xlsx2("data/shinyBN.xlsx", header = T,sheetName = "Edges",stringsAsFactors=F)
            edges <- paste(Edge_Excel$from,"~",Edge_Excel$to,sep = '')
            Ecol <- Edge_Excel$color
            Elty <- Edge_Excel$linetype
            Elwd <- Edge_Excel$width
          }else{
            e = as.data.frame(arcs(fit))
            edges = paste(e$from,"~",e$to,sep = '')
            
            # Default parameter
            if(input$IE_color != 'Other') Ecol <- rep(input$IE_color,length(edges))
            else Ecol <- rep(input$IE_Other_color,length(edges))
            Elty <- rep(input$IE_type,length(edges))
            if(input$IE_size_type == "Self-defined") Elwd <- rep(input$IE_size,length(edges))
            else{
              a <- recStrength()$strength
              Elwd <- 5*(a-min(a))/(max(a)-min(a))+1
            }
          }
        }
      }
      else{
        edges = paste(f_id=Cont[["Edge"]]$from,"~",Cont[["Edge"]]$to,sep = '')
        
        # Default parameter
        Ecol <- Cont[["Edge"]]$color
        Elty <- Cont[["Edge"]]$linetype
        Elwd <- as.numeric(Cont[["Edge"]]$width)
      }
    }
    
    Ecolorsize_tab <- recEdge()
    
    if("Edge Color" %in% Ecolorsize_tab$Type){
      r1 <- Ecolorsize_tab$Type=="Edge Color"
      Ecol[match(Ecolorsize_tab$Edges[r1],edges)] <- Ecolorsize_tab$ColorSize[r1]
    }
    if("Edge Type" %in% Ecolorsize_tab$Type){
      r2 <- Ecolorsize_tab$Type=="Edge Type"
      Elty[match(Ecolorsize_tab$Edges[r2],edges)] <- Ecolorsize_tab$ColorSize[r2]
    }
    if("Edge Width" %in% Ecolorsize_tab$Type){
      r3 <- Ecolorsize_tab$Type=="Edge Width"
      Elwd[match(Ecolorsize_tab$Edges[r3],edges)] <- as.numeric(Ecolorsize_tab$ColorSize[r3])
    }
    
    renderE <- list(Ecol=Ecol,Elty=Elty,Elwd=Elwd)
    renderE
  })
  
  
  # Add Legend!
  output$Nleg_color <- renderUI({
    fit <- recFit()
    Cont<- RecContinue()
    if(! is.null(fit) | ! is.null(Cont)){
      N <- recRendN()
      NColor_list <- unique(N$Ncol)
      selectInput("NLegend_color","Color:",NColor_list)
    }
  })
  output$Nleg_shape <- renderUI({
    fit <- recFit()
    Cont<- RecContinue()
    if(! is.null(fit) | ! is.null(Cont)){
      N <- recRendN()
      NShape_list <- unique(N$Nshape)
      selectInput("NLegend_shape","Shape:",NShape_list)
    }
  })
  
  RecNL <- reactive({
    if(input$AddButtonNL == n_ALegeN + 1) {
      n_ALegeN <<- n_ALegeN + 1
      Nlegend_tab <<- rbind(Nlegend_tab,data.frame(color=input$NLegend_color,shape=input$NLegend_shape,label=input$NLegend_label,stringsAsFactors = F))
    }
    
    if(input$delButtonNL == n_DLegeN + 1) {
      n_DLegeN <<- n_DLegeN + 1
      indexNL = which(input$NLegend_color==Nlegend_tab$color & input$NLegend_shape==Nlegend_tab$shape)
      Nlegend_tab <<- Nlegend_tab[-indexNL,]
    }
    Nlegend_tab <<- Nlegend_tab[!duplicated(Nlegend_tab[,1:2], fromLast=TRUE), ]
    rownames(Nlegend_tab) <- NULL
    Nlegend_tab
  })
  output$N_legend_tab <- renderDataTable(RecNL(),class="compact",rownames = FALSE,options=list(searching=F,
                                                                                               columnDefs=list(list(className = 'dt-center', targets = 1))))
  
  output$Eleg_color <- renderUI({
    fit <- recFit()
    Cont<- RecContinue()
    if(! is.null(fit) | ! is.null(Cont)){
      E <- recRendE()
      EColor_list <- unique(E$Ecol)
      selectInput("ELegend_color","Color:",EColor_list)
    }
  })
  
  output$Eleg_dashed <- renderUI({
    fit <- recFit()
    Cont<- RecContinue()
    if(! is.null(fit) | ! is.null(Cont)){
      E <- recRendE()
      Edash_list <- unique(E$Elty)
      selectInput("ELegend_linetype","Color:",Edash_list)
    }
  })
  
  RecEL <- reactive({
    if(input$AddButtonEL == n_ALegeE + 1) {
      n_ALegeE <<- n_ALegeE + 1
      Elegend_tab <<- rbind(Elegend_tab,data.frame(color=input$ELegend_color,linetype=input$ELegend_linetype,label=input$ELegend_label,stringsAsFactors = F))
    }
    
    if(input$delButtonEL == n_DLegeE + 1) {
      n_DLegeE <<- n_DLegeE + 1
      indexEL = which(input$ELegend_color==Elegend_tab$color & input$ELegend_linetype==Elegend_tab$linetype)
      Elegend_tab <<- Elegend_tab[-indexEL,]
    }
    Elegend_tab <<- Elegend_tab[!duplicated(Elegend_tab[,1:2], fromLast=TRUE), ]
    rownames(Elegend_tab) <- NULL
    Elegend_tab
  })
  output$E_legend_tab <- renderDataTable(RecEL(),class="compact",rownames = FALSE,options=list(searching=F,
                                                                                               columnDefs=list(list(className = 'dt-center', targets = 1))))
  
  
  #Network Visualization
  output$outVis <- renderVisNetwork({
    fit <- recFit()
    Cont<- RecContinue()
    if(! is.null(fit) | ! is.null(Cont)){
      if(! is.null(fit)){
        if(input$inType == 'R Object in R' & input$inFit == "Stroke_bnfit"){
          Node_Excel <- read.xlsx2("data/shinyBN.xlsx", header = T,sheetName = "Nodes",stringsAsFactors=F)
          Edge_Excel <- read.xlsx2("data/shinyBN.xlsx", header = T,sheetName = "Edges",stringsAsFactors=F)
          nodes <- Node_Excel$id
          edges <- data.frame(from=Edge_Excel$from,to=Edge_Excel$to)
          label <- Node_Excel$label
          x <- Node_Excel$x
          y <- Node_Excel$y
          ncolor <- Node_Excel$color
          shape <- Node_Excel$shape
          font.size  = Node_Excel$font.size
          font.color = Node_Excel$font.color
          lcolor <- Edge_Excel$color
          lty <- Edge_Excel$linetype
          lwd <- Edge_Excel$width
        }else{
          nodes <- nodes(fit)
          edges <- as.data.frame(arcs(fit))
          label <- nodes
          names(edges)<-c("from","to")
          x <- NA
          y <- NA
          ncolor <- "lightblue"
          shape <- "ellipse"
          font.size  = "14"
          font.color = "black"
          lcolor <- "gray"
          lty <- "solid"
          lwd <- 1
        }
      }else if(input$inType == 'Structure in Excel'){
        nodes = Cont[["Node"]]$id
        edges = data.frame(from=Cont[["Edge"]]$from,to=Cont[["Edge"]]$to)
        label = Cont[["Node"]]$label
        x <<- Cont[["Node"]]$x
        y <<- Cont[["Node"]]$y
        ncolor <- Cont[["Node"]]$color
        shape <- Cont[["Node"]]$shape
        font.size  = Cont[["Node"]]$font.size
        font.color = Cont[["Node"]]$font.color
        lcolor <- Cont[["Edge"]]$color
        lty <- Cont[["Edge"]]$linetype
        lwd <- Cont[["Edge"]]$width
      }
      
      if(is.na(x) || toupper(x) == "NA"){
        nodes_tab <<- data.frame(id=nodes,label=label,color=ncolor,shape=shape,font.size=font.size,
                                 font.color=font.color,stringsAsFactors = F)
      }else{
      nodes_tab <<- data.frame(id=nodes,x=x,y=y,label=label,color=ncolor,shape=shape,font.size=font.size,
                               font.color=font.color,stringsAsFactors = F)
      }
      edges_tab <<- data.frame(id=1:nrow(edges),from=edges$from,to=edges$to,color=lcolor,width=lwd,dashes=(lty=="dashed"),
                              arrows="to",smooth=F,stringsAsFactors = F)
      
      node_Legend <- RecNL()
      if(nrow(node_Legend)) node_Legend <- data.frame(node_Legend,font.size=input$NLegend_KeySize)
      edge_Legend <- RecEL()
      edge_Legend$dashes <- edge_Legend$linetype == "dashed"
      edge_Legend <- edge_Legend[,c(1,3,4)]
      if(nrow(edge_Legend)) edge_Legend <- data.frame(edge_Legend,font.size=input$NLegend_KeySize,font.align="bottom")
      
      if(! input$E_TF){
        edge_Legend <- NULL
      }
      if(! input$N_TF){
        node_Legend <-NULL
        position <- input$ELegend_posion
      }else position <- input$NLegend_posion
      
      Network <- visNetwork(nodes_tab,edges_tab)%>%
        visPhysics(enabled = F)%>%
        visInteraction(navigationButtons=T,
                       selectConnectedEdges=F,
                       multiselect = T)%>%
        visLegend(addEdges = edge_Legend,addNodes = node_Legend,width=0.1,
                  position=position,useGroups=F,zoom=F)%>%
        visEvents(type="on", click = "function(propert){
            Shiny.onInputChange('InClick',propert.nodes);
            ;}")
      
      if(! is.na(x) && !toupper(x) == "NA"){
        Network <- Network%>%
          visLayout(randomSeed = 2018)
      }else{
        Network <- Network%>%
          visIgraphLayout(randomSeed=2018,layout=input$inLayout)
      }
    }
    Network
  })
  
  observe({
    fit <- recFit()
    Cont<- RecContinue()
    if(! is.null(fit) | ! is.null(Cont)){
      if(! is.null(fit)){
        nodes <- nodes(fit)
        label <- nodes
        edges <- as.data.frame(arcs(fit))
        names(edges)<-c("from","to")
      }
      else{
        nodes = Cont[["Node"]]$id
        edges = data.frame(from=Cont[["Edge"]]$from,to=Cont[["Edge"]]$to)
        label = Cont[["Node"]]$label
      }

      shape = recRendN()[["Nshape"]]
      ncolor<- recRendN()[["Ncol"]]
      Tsize = recRendN()[["Tsize"]]
      Tcolor= recRendN()[["Tcolor"]]
      if(!is.null(ncolor)) node_tab <- data.frame(id = nodes,label=label,color=ncolor,shape=shape,
                                                  font.color=Tcolor,font.size=Tsize,stringsAsFactors = F)

      lty   = recRendE()[["Elty"]]
      lcolor= recRendE()[["Ecol"]]
      E_size_strength = recRendE()[["Elwd"]]

      edge_tab <- data.frame(id=1:nrow(edges),arrows="to",color=lcolor,dashes=(lty=="dashed"),
                             width=E_size_strength,smooth=F)

      visNetworkProxy("outVis")%>%
        visUpdateNodes(nodes = node_tab)%>%
        visUpdateEdges(edges = edge_tab)
    }
  })
  
  vals <- reactiveValues(coords=NULL,Input_ERROR=0)
  observe({
    invalidateLater(1000)
    visNetworkProxy("outVis") %>% visGetPositions()
    vals$coords <- if (!is.null(input$outVis_positions)) 
      do.call(rbind, input$outVis_positions)
  })
  
  output$shiny_return <- renderPrint({
    input$InClick
  })
  
  output$Q_return <- renderPrint({
    input$InClick[length(input$InClick)]
  })
  
  # Structure save
  recStruct <- reactive({
    fit <- recFit()
    Cont<- RecContinue()
    if(! is.null(fit) | ! is.null(Cont)){
      if(! is.null(fit)){
        nodes <- nodes(fit)
        label <- nodes
        edges <- as.data.frame(arcs(fit))
        names(edges)<-c("from","to")
      }
      else{
        nodes = Cont[["Node"]]$id
        edges = data.frame(from=Cont[["Edge"]]$from,to=Cont[["Edge"]]$to)
        label = Cont[["Node"]]$label
      }
      
      cood <- as.data.frame(vals$coords)
      x <- as.numeric(cood$x)
      y <- as.numeric(cood$y)
      
      shape = recRendN()[["Nshape"]]
      ncolor= recRendN()[["Ncol"]]
      Tsize = recRendN()[["Tsize"]]
      Tcolor= recRendN()[["Tcolor"]]
      
      data_node <- data.frame(id = nodes,label=label,x=x,y=y,color=ncolor,shape=shape,
                              font.color=Tcolor,font.size=Tsize,stringsAsFactors = F)
      
      lty   = recRendE()[["Elty"]]
      lcolor= recRendE()[["Ecol"]]
      E_size_strength = recRendE()[["Elwd"]]
      
      data_edge <- data.frame(from=edges$from,to=edges$to,arrows="to",color=lcolor,linetype=lty,
                              width=E_size_strength,stringsAsFactors = F)
      
      stru <- List(node=data_node,edge=data_edge)
    }
  })
  
  # Network Download(Graph in PDF)
  output$shinyBN.html <- downloadHandler(
    filename = "shinyBN.html",
    content = function(file){
      
      node_Legend <- RecNL()
      if(nrow(node_Legend)) node_Legend <- data.frame(node_Legend,font.size=input$NLegend_KeySize)
      edge_Legend <- RecEL()
      edge_Legend$dashes <- edge_Legend$linetype == "dashed"
      edge_Legend <- edge_Legend[,c(1,3,4)]
      if(nrow(edge_Legend)) edge_Legend <- data.frame(edge_Legend,font.size=input$NLegend_KeySize,font.align="bottom")
      
      if(! input$E_TF){
        edge_Legend <- NULL
      }
      if(! input$N_TF){
        node_Legend <-NULL
        position <- input$ELegend_posion
      }else position <- input$NLegend_posion
      
      visNetwork(nodes = recStruct()[["node"]], edges = recStruct()[["edge"]], height = "800px") %>%
        visPhysics(enabled = FALSE)%>%
        visEdges(smooth = F)%>%
        visLegend(addEdges = edge_Legend,addNodes = node_Legend,width=0.1,
                  position=position,useGroups=F,zoom=F)%>%
        visExport(type = "pdf",label = paste0("Export as PDF")) %>%
        visSave(file)
      
    }, contentType = 'text/html'
  )
  
  # Network Download(Structure in Excel)
  output$shinyBN.xlsx <- downloadHandler(
    filename = "shinyBN.xlsx",
    content = function(file){
      write.xlsx2(recStruct()[["node"]],file,row.names = F,sheetName="Nodes")
      write.xlsx2(recStruct()[["edge"]],file,row.names = F,sheetName="Edges",append = T)
    }
  )
  
  # Set evidence & Query!
  RecE <- reactive({
    if(input$Q_Intype == "Click graph") inputE <- input$InClick[length(input$InClick)]
    else inputE <- input$inEvidence
    if(input$AddButtonE == n_AE + 1) {
      n_AE <<- n_AE + 1
      Evid_tab <<- rbind(Evid_tab,data.frame(Evidence = inputE,Value = input$inEValue,stringsAsFactors=FALSE))
    }
    
    if(input$delButtonE == n_DE + 1) {
      n_DE <<- n_DE + 1
      indexE = which(Evid_tab$Evidence != inputE)
      Evid_tab <<- Evid_tab[indexE,]
    }
    
    if(input$ClearButtonE == n_ClearEvi + 1){
      n_ClearEvi <<- n_ClearEvi + 1
      Evid_tab <<- data.frame(Evidence=character(),Value=character(),stringsAsFactors=FALSE)
    }
    
    Evid_tab <<- Evid_tab[!duplicated(Evid_tab$Evidence, fromLast=TRUE), ]
    rownames(Evid_tab) <- NULL
    Evid_tab
  })
  
  output$Evi_table <- renderDataTable(RecE(),rownames = FALSE,options=list(searching=T,
                                                                           columnDefs=list(list(className = 'dt-center', targets = 1))))
  
  RecQ <- reactive({
    fit <- recFit()
    if(input$delButtonQ == 0 & input$delButtonQ == 0 & input$ClearButtonQ == 0 & !is.null(fit) & "bn.fit" %in% class(fit)) {
      if(input$inType == 'R Object in R' & input$inFit == "Stroke_bnfit") node = "STROKE"
      else if(input$inFit %in% c("Asia_fit,Asia_data","Asia_fit")) node = "lung"
      else node <- nodes(fit)[length(fit)]
      tmp = fit[[node]]
      value = rownames(tmp$prob)
      Query_tab <<- data.frame(Query = node,Value = value)
    }
    if(input$AddButtonQ == n_AQ + 1 ) {
      n_AQ <<- n_AQ + 1
      Query_tab <<- rbind(Query_tab,data.frame(Query = input$inQuery,Value = input$inQValue,stringsAsFactors=FALSE))
      Query_tab <<- Query_tab[!duplicated(data.frame(Query_tab$Query,Query_tab$Value), fromLast=TRUE),,drop=F]
    }
    if(input$delButtonQ == n_DQ + 1) {
      n_DQ <<- n_DQ + 1
      index <- Query_tab$Query == input$inQuery & match(Query_tab$Value,input$inQValue)
      index[is.na(index)] <- F
      Query_tab <<- Query_tab[! index,]
    }
    if(input$ClearButtonQ == n_ClearQue + 1){
      n_ClearQue <<- n_ClearQue + 1
      Query_tab <<- data.frame(Query=character(),Value = character(),stringsAsFactors=FALSE)
    }
    rownames(Query_tab) <- NULL
    Query_tab
  })
  
  output$Query_table <- renderDataTable(RecQ(),class="compact",options=list(searching=F,
                                                                            columnDefs=list(list(className = 'dt-center', targets = 1))))
  
  RecR <- reactive({
    
    result <- data.frame()
    fit <- recFit()
    Q <- RecQ()
    if(nrow(Q) && ! is.null(fit) && "bn.fit" %in% class(fit)){
      E <- RecE()
      jtree = compile(as.grain(fit))
      jtree1 = setEvidence(jtree,E$Evidence,E$Value)
      q <- querygrain(jtree1,nodes=unique(Q$Query),type=input$Type)
      if(input$Type == "marginal"){
        Proability <- unlist2(q)
        Level <- names(Proability)
        n_level <- sapply(q,length)
        Variable <- rep(names(q),n_level)
        result_tmp <- data.frame(Variable,Level,Proability,stringsAsFactors = F)
        result <- sqldf("select Variable,Level,Proability from result_tmp,Q where Variable=Query and Level=Value")
      }
      if(input$Type == "joint"){
        result <- rbind(result,melt(q,varNames=dimnames(q),value.name = "Proability",as.is=T))
      }
    }
    result
  })
  
  # Query results in graph
  output$ResultPlot <- renderPlot({
    if(nrow(RecR())){
      data <- RecR()
      data$Proability <- round(data$Proability*100,2)
      
      Interval <- as.numeric(unlist(strsplit(input$GC_Interval,",",fixed=T)))
      Color <- unlist(strsplit(input$GC_Color,",",fixed=T))
      Label <- unlist(strsplit(input$GC_Label,",",fixed=T))
      n <- length(Interval) + 1
      if(length(Color) < n) Color[(n-length(Color)):n] <- "black"
      else Color <- Color[1:n]
      if(length(Label) < n) Label[(n-length(Label)):n] <- "NA"
      else Label <- Label[1:n]
      
      if(input$Type == "marginal"){
        x <- paste(data$Variable,":",data$Level,sep="")
        data_plot <- data.frame(x=x,p=data$Proability)
        g <<- ggplot(data=data_plot,aes(x=x,y=p))+
          geom_bar(stat = "identity",fill="lightblue")+
          geom_text(aes(x=x,y=p+4,label=p))+
          scale_y_continuous("Predict  Probability (%)",expand=c(0,0),lim=c(0,105))+
          scale_x_discrete(NULL)+
          theme(panel.background = element_rect(fill = "transparent",colour = NA),
                panel.grid.minor = element_blank(),
                panel.grid.major = element_blank(),
                plot.background = element_rect(fill = "transparent",colour = NA),
                axis.line = element_line(colour = "black"),
                axis.text.x = element_text(size=10),
                axis.title.y = element_text(angle=90,size = 14))
        if(input$GC_TF){
          index <- sapply(data$Proability,function(a){sum(a > Interval)})
          data_plot$col <- Label[index+1]
          g <<- ggplot(data=data_plot,aes(x=x,y=p,fill=col))+
            geom_bar(stat = "identity")+
            geom_text(aes(x=x,y=p+4,label=p))+
            scale_fill_manual(NULL,values=Color,limits = Label)+
            scale_y_continuous("Predict  Probability (%)",expand=c(0,0),lim=c(0,105))+
            scale_x_discrete(NULL)+
            theme(panel.background = element_rect(fill = "transparent",colour = NA),
                  panel.grid.minor = element_blank(),
                  panel.grid.major = element_blank(),
                  plot.background = element_rect(fill = "transparent",colour = NA),
                  axis.line = element_line(colour = "black"),
                  axis.text.x = element_text(size=10),
                  axis.title.y = element_text(angle=90,size = 14),
                  legend.position = c(input$RLegend_x/100,input$RLegend_y/100),
                  legend.text = element_text(size=input$RLegend_TextSize))
          if(input$GC_Label== 'NULL') g <<- g + theme(legend.position='none')
        }
        g
      }else{
        nc <- ncol(data) - 1
        nr <- nrow(data)
        data$IndexXxXxX <- letters[1:nr]
        data2 <- data.frame(lx=rep(data$IndexXxXxX,nc),ly=rep(1:nc*-4,each=nr),lab=unlist2(data[,1:nc]),stringsAsFactors=F)
        g <<- ggplot(data,aes(x=IndexXxXxX))+
          geom_bar(aes(weight=Proability),fill="lightblue")+
          geom_text(aes(x=IndexXxXxX,y=Proability+4,label=Proability))+
          geom_text(data = data2,aes(x = lx,y = ly,label=lab))+
          scale_x_discrete(NULL,breaks=NULL)+
          scale_y_continuous("Predict  Probability(%)",expand=c(0,0),lim=c(nc*-5,105),breaks=c(1:nc*-4,seq(20,100,20)),
                             labels=c(colnames(data)[1:nc],seq(20,100,20)))+
          geom_hline(aes(yintercept=0))+
          geom_line(data=data.frame(n=c(0,0),m=c(0,100)),aes(n,m),group=1,size=1.1)+
          theme(panel.background = element_rect(fill = "transparent",colour = NA),
                panel.grid.minor = element_blank(),
                panel.grid.major = element_blank(),
                plot.background = element_rect(fill = "transparent",colour = NA),
                axis.line = element_line(colour=NA),
                axis.ticks.x = element_line(colour = NA),
                axis.ticks.y = element_line(colour = NA),
                axis.title.y = element_text(angle=90,size = 14))
        
        if(input$GC_TF){
          index <- sapply(data$Proability,function(a){sum(a > Interval)})
          data$col <- Label[index+1]
          
          g <<- ggplot(data=data)+
            geom_bar(aes(x=IndexXxXxX,weight=Proability,fill=col))+
            geom_text(aes(x=IndexXxXxX,y=Proability+4,label=Proability))+
            geom_text(data = data2,aes(x = lx,y = ly,label=lab))+
            scale_fill_manual(NULL,values=Color,limits = Label)+
            geom_hline(aes(yintercept=0))+
            geom_line(data=data.frame(n=c(0,0),m=c(0,100)),aes(n,m),group=1,size=1.1)+
            scale_x_discrete(NULL,breaks=NULL)+
            scale_y_continuous("Predict  Probability(%)",expand=c(0,0),lim=c(nc*-5,105),breaks=c(1:nc*-4,seq(20,100,20)),
                               labels=c(colnames(data)[1:nc],seq(20,100,20)))+
            theme(panel.background = element_rect(fill = "transparent",colour = NA),
                  panel.grid.minor = element_blank(),
                  panel.grid.major = element_blank(),
                  plot.background = element_rect(fill = "transparent",colour = NA),
                  axis.text.x = element_text(size=10),
                  axis.ticks.x = element_line(colour = NA),
                  axis.ticks.y = element_line(colour = NA),
                  axis.title.y = element_text(angle=90,size = 14),
                  legend.position = c(input$RLegend_x/100,input$RLegend_y/100),
                  legend.text = element_text(size=input$RLegend_TextSize))
          
          if(input$GC_Label== 'NULL') g <<- g + theme(legend.position='none')
        }
        g
      }
    }else{
      fit <- recFit()
      if(! is.null(fit)){
        if("bn" %in% class(fit)) inlabel <- "Please fit the Parameter of Network !"
        else inlabel <- "Please choose your query nodes !"
      }
      
      g<<- ggplot(data.frame(x=400,y=400),aes(x,y))+
        geom_text(aes(label=inlabel),color="red",size=4.5)+
        theme(panel.background = element_rect(fill = "transparent",colour = NA),
              panel.grid.minor = element_blank(),
              panel.grid.major = element_blank(),
              plot.background = element_rect(fill = "transparent",colour = NA))+
        scale_x_continuous(NULL,breaks=NULL)+
        scale_y_continuous(NULL,breaks=NULL)
      g
    }
  })
  
  # Query results in table
  output$Result <- renderDT({
    if(nrow(RecR())) datatable(RecR(),rownames = FALSE,class="compact",options=list(searching=F,columnDefs=list(list(className = 'dt-center', targets = 1)))) %>% formatPercentage('Proability',2)
  })
  
  #Download Query results in graph
  output$Result.pdf <- downloadHandler(
    filename = paste("Result.pdf"),
    content = function(file){
      ggsave(file,g,width=as.numeric(input$Pwidth),height=as.numeric(input$Pheight))
    }, contentType = 'application/pdf'
  )
})
