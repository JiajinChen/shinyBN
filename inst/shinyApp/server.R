options(warn=-1)
# change the maximum size restriction
options(shiny.maxRequestSize=30*1024^2)

#SERVER
shinyServer(function(input,output,session){

  #Structure Prior
  RecP <- reactive({
    if(input$AddButtonP == n_AP + 1) {
      n_AP <<- n_AP + 1
      if(input$in_From != input$in_To) Pri_tab <<- rbind(Pri_tab,data.frame(From=input$in_From,To=input$in_To,Type=input$BorW,stringsAsFactors=FALSE))
    }
    if(input$delButtonP == n_DP + 1) {
      n_DP <<- n_DP + 1
      indexP = which(Pri_tab$From == input$in_From & Pri_tab$To == input$in_To & Pri_tab$Type == input$BorW)
      if(length(indexP)) Pri_tab <<- Pri_tab[-indexP,]
    }
    Pri_tab <<- Pri_tab[!duplicated(data.frame(Pri_tab$From,Pri_tab$To,Pri_tab$Type), fromLast=TRUE), ]
    rownames(Pri_tab) <- NULL
    Pri_tab
  })
  output$Pri_table <- renderDataTable(RecP(),class="compact",rownames = FALSE,options=list(searching=F,
                                                                                           columnDefs=list(list(className = 'dt-center', targets = 1))))

  recPrior <- reactive({

    Prior <- RecP()
    colnames(Prior) <- c("from","to","type")

    white <- Prior[Prior$type == "whitelist",]
    white <- white[,-3]
    if(nrow(white) == 0) white <- NULL
    black <- Prior[Prior$type == "blacklist",]
    black <- black[,-3]
    if(nrow(black) == 0) black <- NULL
    renderP <- list(black=black,white=white)
    renderP
  })

  #Input the network in class bn.fit
  recFit <- reactive({
    bn_fit <- NULL
    if(input$inType=='R Object in R'){
      if(!is.null(input$inFit) & ! input$inFit == ""){
        inF <- unlist(strsplit(input$inFit,",",fixed=T))[1]
        if("bn.fit" %in% class(get(inF)))  bn_fit <- get(inF)
        else bn_fit <- get(unlist(strsplit(input$inFit,",",fixed=T))[2])
        if(! "bn.fit" %in% class(bn_fit)) bn_fit <- NULL
      }
    }
    else if(input$inType=='R Object(.Rdata)') {
      obj <- input$inObject
      if(! is.null(obj)){
        a <- load(obj$datapath)
        load(obj$datapath)
        if("bn.fit" %in% class(get(a[1])))  bn_fit <- get(a[1])
        else bn_fit <- get(a[2])
        if(! "bn.fit" %in% class(bn_fit)) bn_fit <- NULL
      }
    }
    else if(input$inType=='Raw Data(.csv)'){
      file <- input$inFile
      if(! is.null(file)){
        data <- read.csv(file$datapath, header = input$inHeader,colClasses = "factor")
        if(! is.null(data)){
          if(input$inLearnType == 'Constraint-Based Algorithms'){
            if(input$inLearn1 == 'Grow-Shrink') dag <- gs(data,blacklist = recPrior()[["black"]],whitelist = recPrior()[["white"]])
            else if(input$inLearn1 == 'Incremental Association') dag <- iamb(data,blacklist = recPrior()[["black"]],whitelist = recPrior()[["white"]])
            else if(input$inLearn1 == 'Fast Incremental Association') dag <- fast.iamb(data,blacklist = recPrior()[["black"]],whitelist = recPrior()[["white"]])
            else if(input$inLearn1 == 'Interleaved Incremental Association') dag <- inter.iamb(data,blacklist = recPrior()[["black"]],whitelist = recPrior()[["white"]])
            else if(input$inLearn1 == 'Max-Min Parents and Children') dag <- mmpc(data,blacklist = recPrior()[["black"]],whitelist = recPrior()[["white"]])
            else if(input$inLearn1 == 'Semi-Interleaved HITON-PC') dag <- si.hiton.pc(data,blacklist = recPrior()[["black"]],whitelist = recPrior()[["white"]])
          }
          else if(input$inLearnType == 'Score-Based Algorithms'){
            if(input$inLearn2 == 'hill-climbing') dag <- hc(data,blacklist = recPrior()[["black"]],whitelist = recPrior()[["white"]])
            else if(input$inLearn2 == 'tabu search') dag <- tabu(data,blacklist = recPrior()[["black"]],whitelist = recPrior()[["white"]])
          }
          else if(input$inLearnType == 'Hybrid Algorithms'){
            if(input$inLearn3 == 'Max-Min Hill Climbing') dag <- mmhc(data,blacklist = recPrior()[["black"]],whitelist = recPrior()[["white"]])
            else if(input$inLearn3 == '2-phase Restricted Maximization') dag <- rsmax2(data,blacklist = recPrior()[["black"]],whitelist = recPrior()[["white"]])
          }
          bn_fit <- bn.fit(dag,data,method = input$inMethod)
          if(! "bn.fit" %in% class(bn_fit)) bn_fit <- NULL
        }
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
        if("bn.fit" %in% class(get(inS)))  bn_data <- get(unlist(strsplit(input$inFit,",",fixed=T))[2])
        else bn_data <- get(inS)
      }
    }
    else if(input$inType=='R object(.Rdata)') {
      obj <- input$inObject
      if(! is.null(obj)){
        a <- load(obj$datapath)
        load(obj$datapath)
        if("bn.fit" %in% class(get(a[1])))  bn_data <- get(a[2])
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
      if(! is.null(fit)) x <- as.bn(graphviz.plot(fit))
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
    file <- input$inContinue
    Continue <- NULL
    if(! is.null(file)){
      Node <- read.xlsx2(file$datapath, header = T,sheetName = "Nodes",stringsAsFactors=F)
      Edge <- read.xlsx2(file$datapath, header = T,sheetName = "Edges",stringsAsFactors=F)
      if(! is.null(Node) & ! is.null(Edge)){
        Continue <- list(Node=Node,Edge=Edge)
      }
    }
    if(input$inType != "Render Continue...") Continue <- NULL
    Continue
  })

  #Reactive UI output
  output$evidence <- renderUI({
    fit <- recFit()
    if(! is.null(fit)){
      Nodelist = nodes(fit)
      selectInput("inEvidence","Select the Evidence nodes:",Nodelist)
    }
  })

  output$E_value <- renderUI({
    fit <- recFit()
    if(! is.null(fit) & ! is.null(input$inEvidence)){
      tmp = fit[[input$inEvidence]]
      valuelist = rownames(tmp$prob)
      if(length(valuelist)) radioButtons("inEValue","Value of Evidence nodes:",choices=valuelist)
    }
  })

  output$query <- renderUI({
    fit <- recFit()
    if(! is.null(fit)){
      Nodelist = nodes(fit)
      selectInput("inQuery","Select the Query nodes:",Nodelist)
    }
  })

  output$Q_value <- renderUI({
    fit <- recFit()
    if(! is.null(fit) & ! is.null(input$inQuery)){
      tmp = fit[[input$inQuery]]
      valuelist = rownames(tmp$prob)
      if(length(valuelist)) checkboxGroupInput("inQValue","Value of Query nodes:",choices=valuelist,selected=valuelist)
    }
  })

  output$from <- renderUI({
    fit <- recFit()
    if(! is.null(fit)){
      Nodelist = nodes(fit)
      selectInput("in_From","From:",Nodelist)
    }
  })

  output$to <- renderUI({
    fit <- recFit()
    if(! is.null(fit)){
      Nodelist = nodes(fit)
      selectInput("in_To","To:",Nodelist)
    }
  })

  output$N <- renderUI({
    fit <- recFit()
    Cont<- RecContinue()
    if(! is.null(fit) | ! is.null(Cont)){
      if(! is.null(fit)) Nodelist = nodes(fit)
      else Nodelist = Cont[["Node"]]$node
      selectInput("inNodes","Select the Nodes:",Nodelist)
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

  output$E_Render <- renderUI({
    if(input$IE_size_type == 'Arc Strength') Renderlist <- c("Edge Color","Edge Type")
    else Renderlist <- c("Edge Color","Edge Type","Edge Width")
    selectInput("Edges_type","Select the Type:",Renderlist)
  })

  output$Main <- renderUI({
    if(input$inType == 'Render Continue...'){
      ui <- column(width=12,
                   svgPanZoomOutput("outSVG",height = "550px"))
    }else{
      ui <- {
        column(width=12,
               column(width=7,
                      jqui_resizabled(
                        tabBox(side = "left", width = NULL,height = NULL,selected = "Network",
                               tabPanel("Network",
                                        svgPanZoomOutput("outSVG")),
                               tabPanel("Evidence",
                                        dataTableOutput("Evi_table")
                               )
                        ))),
               column(width=5,
                      jqui_resizabled(
                        tabBox(side = "left", width = NULL,height = NULL,selected = "Graph",
                               tabPanel("Graph",
                                        plotOutput("ResultPlot")),
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
              downloadLink("shinyBN.pdf","Figure in PDF"),
              downloadLink("shinyBN.xlsx","Structure in Excel"))
  })

  # Add Nodes and Edges Render!
  recNode <- reactive({
    if(input$AddButtonNodes == n_ANode + 1) {
      n_ANode <<- n_ANode + 1
      if(input$Nodes_type == 'Node Color'){
        if(input$N_color != "Other")
          Ncolorsize_tab <<- rbind(Ncolorsize_tab,data.frame(Nodes = input$inNodes,ColorSize = input$N_color,
                                                             Type = input$Nodes_type,stringsAsFactors=FALSE))
        else
          Ncolorsize_tab <<- rbind(Ncolorsize_tab,data.frame(Nodes = input$inNodes,ColorSize = input$N_Other_color,
                                                             Type = input$Nodes_type,stringsAsFactors=FALSE))
      }
      else if(input$Nodes_type == 'Node Size'){
        Ncolorsize_tab <<- rbind(Ncolorsize_tab,data.frame(Nodes = input$inNodes,ColorSize = as.character(input$N_Nsize),
                                                           Type = input$Nodes_type,stringsAsFactors=FALSE))
      }
      else if(input$Nodes_type == 'Text Size'){
        Ncolorsize_tab <<- rbind(Ncolorsize_tab,data.frame(Nodes = input$inNodes,ColorSize = as.character(input$N_Tsize),
                                                           Type = input$Nodes_type,stringsAsFactors=FALSE))
      }else
        Ncolorsize_tab <<- rbind(Ncolorsize_tab,data.frame(Nodes = input$inNodes,ColorSize = as.character(input$N_Nshape),
                                                           Type = input$Nodes_type,stringsAsFactors=FALSE))
    }

    if(input$DelButtonNodes == n_DNode + 1){
      n_DNode <<- n_DNode + 1
      indexNode = which(Ncolorsize_tab$Nodes == input$inNodes & Ncolorsize_tab$Type == input$Nodes_type)
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
    if(input$AddButtonEdges == n_AEdge + 1){
      n_AEdge <<- n_AEdge + 1
      if(input$Edges_type == 'Edge Color'){
        if(input$E_color != "Other")
          Ecolorsize_tab <<- rbind(Ecolorsize_tab,data.frame(Edges = input$inEdges,ColorSize = input$E_color,
                                                             Type = input$Edges_type,stringsAsFactors=FALSE))
        else
          Ecolorsize_tab <<- rbind(Ecolorsize_tab,data.frame(Edges = input$inEdges,ColorSize = input$E_Other_color,
                                                             Type = input$Edges_type,stringsAsFactors=FALSE))
      }
      else if(input$Edges_type == 'Edge Width'){
        Ecolorsize_tab <<- rbind(Ecolorsize_tab,data.frame(Edges = input$inEdges,ColorSize = as.character(input$E_size),
                                                           Type = input$Edges_type,stringsAsFactors=FALSE))
      }
      else Ecolorsize_tab <<- rbind(Ecolorsize_tab,data.frame(Edges = input$inEdges,ColorSize = input$E_type,
                                                              Type = input$Edges_type,stringsAsFactors=FALSE))
    }

    if(input$DelButtonEdges == n_DEdge + 1){
      n_DEdge <<- n_DEdge + 1
      indexEdge = which(Ecolorsize_tab$Edges == input$inEdges & Ecolorsize_tab$Type == input$Edges_type)
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
        nodes <- nodes(fit)
        # Default parameter
        if(input$IN_color == 'Other') Ncol <- rep(as.character(input$IN_Other_color),length(nodes))
        else Ncol <- rep(as.character(input$IN_color),length(nodes))
        Nsize  <- rep(input$IN_Nsize,length(nodes))
        Tsize  <- rep(input$IN_Tsize,length(nodes))
        Nshape <- rep(input$IN_Nshape,length(nodes))
      }
      else{
        nodes  <- Cont[["Node"]]$node
        Ncol   <- Cont[["Node"]]$node_color
        Nsize  <- as.numeric(Cont[["Node"]]$node_size)
        Tsize  <- rep(input$IN_Tsize,length(nodes))
        Nshape <- Cont[["Node"]]$node_shape
      }
    }
    Nshape[Nshape=="Circle"]<-19;Nshape[Nshape=="Square"]<-15
    Nshape[Nshape=="Triangle"]<-17;Nshape[Nshape=="Rhombus"]<-18


    Ncolorsize_tab <- recNode()

    if("Node Color" %in% Ncolorsize_tab$Type){
      r1 <- Ncolorsize_tab$Type=="Node Color"
      Ncol[match(Ncolorsize_tab$Nodes[r1],nodes)] <- Ncolorsize_tab$ColorSize[r1]
    }
    if("Node Size" %in% Ncolorsize_tab$Type){
      r2 <- Ncolorsize_tab$Type=="Node Size"
      Nsize[match(Ncolorsize_tab$Nodes[r2],nodes)] <- as.numeric(Ncolorsize_tab$ColorSize[r2])
    }
    if("Text Size" %in% Ncolorsize_tab$Type){
      r3 <- Ncolorsize_tab$Type=="Text Size"
      Tsize[match(Ncolorsize_tab$Nodes[r3],nodes)] <- as.numeric(Ncolorsize_tab$ColorSize[r3])
    }
    if("Node Shape" %in% Ncolorsize_tab$Type){
      r4 <- Ncolorsize_tab$Type=="Node Shape"
      Nshape[match(Ncolorsize_tab$Nodes[r4],nodes)] <- Ncolorsize_tab$ColorSize[r4]
      Nshape[Nshape=="Circle"]<-19;Nshape[Nshape=="Square"]<-15
      Nshape[Nshape=="Triangle"]<-17;Nshape[Nshape=="Rhombus"]<-18
    }

    renderN <- list(Ncol=Ncol,Nsize=Nsize,Tsize=Tsize,Nshape=Nshape)
    renderN
  })

  recRendE <- reactive({
    fit <- recFit()
    Cont<- RecContinue()
    if(! is.null(fit) | ! is.null(Cont)){
      if(! is.null(fit)){
        e = as.data.frame(arcs(fit))
        edges = paste(e$from,"~",e$to,sep = '')

        # Default parameter
        if(input$IE_color != 'Other') Ecol <- rep(input$IE_color,length(edges))
        else Ecol <- rep(input$IE_Other_color,length(edges))
        Elty <- rep(input$IE_type,length(edges))
        if(input$IE_size_type == "Self-defined") Elwd <- rep(input$IE_size,length(edges))
        else{
          a <- recStrength()$strength
          Elwd <- 2*(a-min(a))/(max(a)-min(a))+1
        }
      }
      else{
        edges = paste(f_id=Cont[["Edge"]]$from,"~",Cont[["Edge"]]$to,sep = '')

        # Default parameter
        Ecol <- Cont[["Edge"]]$edge_color
        Elty <- Cont[["Edge"]]$edge_lty
        Elwd <- as.numeric(Cont[["Edge"]]$edge_lwd)
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
  output$NlegTab <- renderTable({
    fit <- recFit()
    Cont<- RecContinue()
    if(! is.null(fit) | ! is.null(Cont)){
      if(! is.null(fit)){
        NCol   = recRendN()[["Ncol"]]
        NShape = recRendN()[["Nshape"]]
      }
      else{
        NCol   = Cont[["Node"]]$node_color
        NShape = Cont[["Node"]]$node_shape
      }
      NShape[NShape==19]<-"Circle"
      NShape[NShape==15]<-"Square"
      NShape[NShape==17]<-"Triangle"
      NShape[NShape==18]<-"Rhombus"
      N_Leg <- data.frame(Node.Color = NCol,Node.Shape = NShape)
      N_Leg<-N_Leg[! duplicated(N_Leg),]
      N_Leg
    }
  })

  output$ElegTab <- renderTable({
    fit <- recFit()
    Cont<- RecContinue()
    if(! is.null(fit) | ! is.null(Cont)){
      if(! is.null(fit)){
        ECol   = recRendE()[["Ecol"]]
        Etype  = recRendE()[["Elty"]]
      }
      else{
        ECol   = Cont[["Edge"]]$edge_color
        Etype = Cont[["Edge"]]$edge_lty
      }
      E_Leg <- data.frame(Edge.Color = ECol,Edge.Type = Etype)
      E_Leg<-E_Leg[! duplicated(E_Leg),]
      E_Leg
    }
  })


  #Network Visualization
  output$outSVG <- renderSvgPanZoom({
    fit <- recFit()
    Cont<- RecContinue()
    if(! is.null(fit) | ! is.null(Cont)){
      if(! is.null(fit)){
        g<-graphviz.plot(fit, highlight = NULL, layout = input$inLayout,
                         shape = "circle",main = NULL, sub = NULL)
        nodes <- nodes(fit)
        edges <- as.data.frame(arcs(fit))
        names(edges)<-c("f_id","t_id")
        x_position = g@renderInfo@nodes$nodeX
        y_position = g@renderInfo@nodes$nodeY
      }
      else{
        nodes = Cont[["Node"]]$node
        edges = data.frame(f_id=Cont[["Edge"]]$from,t_id=Cont[["Edge"]]$to)

        x_position = as.numeric(Cont[["Node"]]$x)
        y_position = as.numeric(Cont[["Node"]]$y)
      }

      size <- recRendN()[["Nsize"]]
      shape = as.numeric(recRendN()[["Nshape"]])
      ncolor= recRendN()[["Ncol"]]
      Tsize = recRendN()[["Tsize"]]

      lty   = recRendE()[["Elty"]]
      lcolor= recRendE()[["Ecol"]]
      E_size_strength = recRendE()[["Elwd"]]

      node_xy <- data.frame(x_position,y_position,nodes)
      node_edge_xy <- sqldf("select f_id,t_id,x_position as x,y_position as y from edges,node_xy where f_id=nodes")
      node_edge_xy <- sqldf("select f_id,t_id,x,y,x_position as xend,y_position as yend from node_edge_xy,node_xy where t_id=nodes")
      node_edge_xy$d_x <- node_edge_xy$xend - node_edge_xy$x
      node_edge_xy$d_y <- node_edge_xy$yend - node_edge_xy$y
      node_edge_xy$d <- sqrt(node_edge_xy$d_x^2+node_edge_xy$d_y^2)
      names(size) <- nodes
      node_edge_xy$arrow_tx <- node_edge_xy$xend - node_edge_xy$d_x/node_edge_xy$d*size[as.character(node_edge_xy$t_id)]/0.4
      node_edge_xy$arrow_ty <- node_edge_xy$yend - node_edge_xy$d_y/node_edge_xy$d*size[as.character(node_edge_xy$t_id)]/0.4

      ltotal=paste(lcolor,lty,sep="")
      names(lty)=ltotal
      Eleg=data.frame(lty,lcolor)
      Eleg=Eleg[! duplicated(Eleg),]


      ntotal=paste(ncolor,shape,sep="")
      names(shape)=ntotal
      Nleg=data.frame(shape,ncolor)
      Nleg=Nleg[! duplicated(Nleg),]

      uni_l = unique(ltotal)
      nui_n = unique(ntotal)
      L_COLOR <- Eleg$lcolor[order(uni_l)]
      N_COLOR <- Nleg$ncolor[order(nui_n)]

      Nlabel = unlist(strsplit(input$N_Label,",",fixed=T))
      Elabel = unlist(strsplit(input$E_Label,",",fixed=T))
      if(length(Nlabel)==length(nui_n)) Nlabel <- Nlabel[order(nui_n)]
      else Nlabel[1] <- "Mismatched Nodes Label number"
      if(length(Elabel)==length(uni_l)) Elabel <- Elabel[order(uni_l)]
      else Elabel[1] <- "Mismatched Edges Label number"


      Gsvg <- ggplot(node_xy,aes(x=x_position,y=y_position))+
        geom_segment(data=node_edge_xy,aes(x = x,y = y,xend = arrow_tx,yend = arrow_ty,lty=ltotal,color=lcolor,size=E_size_strength),arrow=arrow(length=unit(E_size_strength+1.5,"mm"),type="closed"))+
        geom_point(aes(shape=ntotal,color=ncolor,size=size))+
        geom_text(aes(label=nodes),size=Tsize)+
        theme(panel.background = element_rect(fill = "transparent",colour = NA),
              panel.grid.minor = element_blank(),
              panel.grid.major = element_blank(),
              plot.background = element_rect(fill = "transparent",colour = NA),
              legend.background = element_rect(fill = "transparent",colour = NA))+
        coord_cartesian(xlim=c(min(x_position)-diff(range(x_position))/20,max(x_position)+diff(range(x_position))/20),
                        ylim=c(min(y_position)-diff(range(y_position))/20,max(y_position)+diff(range(y_position))/20))+
        scale_color_identity()+
        scale_size_identity()+
        scale_x_continuous(NULL,breaks=NULL)+
        scale_y_continuous(NULL,breaks=NULL)+
        scale_shape_manual(name="Nodes:",label=Nlabel,values=shape)+
        scale_linetype_manual(name="Edges:",label=Elabel,values=lty)

      if(input$N_TF | input$E_TF){
        if(! input$N_TF){
          Gsvg <- Gsvg + guides(shape=F)+
            guides(linetype = guide_legend(override.aes=list(color=L_COLOR,lwd=input$ELegend_LKeySize)))
          if(input$ELegend_posion == 'Other') Gsvg <- Gsvg + theme(legend.position=c(input$ELegend_x/100,input$ELegend_y/100))
          else {
            if(input$ELegend_posion == "right")
              Gsvg <- Gsvg + theme(legend.position=c(0.95,0.5))
            else if(input$ELegend_posion == "left")
              Gsvg <- Gsvg + theme(legend.position=c(0.05,0.5))
            else if(input$ELegend_posion == "top")
              Gsvg <- Gsvg + theme(legend.position=c(0.5,0.95))
            else if(input$ELegend_posion == "bottom")
              Gsvg <- Gsvg + theme(legend.position=c(0.5,0.05))
            else if(input$ELegend_posion == "topleft")
              Gsvg <- Gsvg + theme(legend.position=c(0.05,0.95))
            else if(input$ELegend_posion == "topright")
              Gsvg <- Gsvg + theme(legend.position=c(0.95,0.95))
            else if(input$ELegend_posion == "bottomleft")
              Gsvg <- Gsvg + theme(legend.position=c(0.05,0.05))
            else if(input$ELegend_posion == "bottomright")
              Gsvg <- Gsvg + theme(legend.position=c(0.95,0.05))
          }
          Gsvg <- Gsvg + theme(legend.direction=input$Hori_E,
                               legend.title=element_text(size=input$ELegend_TitleSize),
                               legend.text=element_text(size=input$ELegend_TextSize))
        }
        else{
          if(! input$E_TF)
            Gsvg <- Gsvg + guides(linetype=F)+
              guides(shape = guide_legend(override.aes=list(color=N_COLOR,size=input$NLegend_KeySize)))
          else Gsvg <- Gsvg + guides(shape = guide_legend(override.aes=list(color=N_COLOR,size=input$NLegend_KeySize)),
                                     linetype = guide_legend(override.aes=list(color=L_COLOR,lwd=input$ELegend_LKeySize)))

          if(input$NLegend_posion == 'Other') Gsvg <- Gsvg + theme(legend.position=c(input$NLegend_x/100,input$NLegend_y/100))
          else {
            if(input$NLegend_posion == "right")
              Gsvg <- Gsvg + theme(legend.position=c(0.95,0.5))
            else if(input$NLegend_posion == "left")
              Gsvg <- Gsvg + theme(legend.position=c(0.05,0.5))
            else if(input$NLegend_posion == "top")
              Gsvg <- Gsvg + theme(legend.position=c(0.5,0.95))
            else if(input$NLegend_posion == "bottom")
              Gsvg <- Gsvg + theme(legend.position=c(0.5,0.05))
            else if(input$NLegend_posion == "topleft")
              Gsvg <- Gsvg + theme(legend.position=c(0.05,0.95))
            else if(input$NLegend_posion == "topright")
              Gsvg <- Gsvg + theme(legend.position=c(0.95,0.95))
            else if(input$NLegend_posion == "bottomleft")
              Gsvg <- Gsvg + theme(legend.position=c(0.05,0.05))
            else if(input$NLegend_posion == "bottomright")
              Gsvg <- Gsvg + theme(legend.position=c(0.95,0.05))
          }
          Gsvg <- Gsvg + theme(legend.direction=input$Hori_N,
                               legend.title=element_text(size=input$NLegend_TitleSize),
                               legend.text=element_text(size=input$NLegend_TextSize))
        }
      }else Gsvg <- Gsvg + guides(shape=F,linetype=F)
      Gsvg <- Gsvg + theme(legend.key.width=unit(1, "cm"),
                           legend.key=element_blank(),
                           legend.margin=margin(1,1,1,1))
    }else{
      Gsvg <- ggplot(data.frame(x=400,y=400),aes(x,y))+
        geom_text(aes(label="Please import your R object with network, or raw data to build network!"),color="red",size=4.5)+
        theme(panel.background = element_rect(fill = "transparent",colour = NA),
              panel.grid.minor = element_blank(),
              panel.grid.major = element_blank(),
              plot.background = element_rect(fill = "transparent",colour = NA))+
        xlim(0,800)+ylim(0,800)+
        scale_x_continuous(NULL,breaks=NULL)+
        scale_y_continuous(NULL,breaks=NULL)
    }
    GSVG <<- Gsvg
    svgPanZoom(
      svgPlot({show(Gsvg)},height=6
      ), controlIconsEnabled = T,center=F
    )
  })

  #Network Download(Graph in PDF)
  output$shinyBN.pdf <- downloadHandler(
    filename = "shinyBN.pdf",
    content = function(file){
      ggsave(file,GSVG,width=as.numeric(input$Pwidth),height=as.numeric(input$Pheight))
    }, contentType = 'application/pdf'
  )

  #Structure save
  recStruct <- reactive({
    fit <- recFit()
    Cont<- RecContinue()
    if(! is.null(fit) | ! is.null(Cont)){
      if(! is.null(fit)){
        g<-graphviz.plot(fit, highlight = NULL, layout = input$inLayout,
                         shape = "circle",main = NULL, sub = NULL)
        N_list <- nodes(fit)
        E_list <- as.data.frame(arcs(fit))

        x_position = g@renderInfo@nodes$nodeX
        y_position = g@renderInfo@nodes$nodeY
      }
      else{
        N_list = Cont[["Node"]]$node
        E_list = data.frame(from=Cont[["Edge"]]$from,to=Cont[["Edge"]]$to)

        x_position = as.numeric(Cont[["Node"]]$x)
        y_position = as.numeric(Cont[["Node"]]$y)
      }

      N_shape <- recRendN()[["Nshape"]]
      N_color <- recRendN()[["Ncol"]]
      N_size  <- recRendN()[["Nsize"]]
      T_size  <- recRendN()[["Tsize"]]
      N_shape[N_shape==19]<-"Circle"
      N_shape[N_shape==15]<-"Square"
      N_shape[N_shape==17]<-"Triangle"
      N_shape[N_shape==18]<-"Rhombus"

      E_lty   <- recRendE()[["Elty"]]
      E_color <- recRendE()[["Ecol"]]
      E_lwd   <- recRendE()[["Elwd"]]

      data_node <- data.frame(node=N_list,x=x_position,y=y_position,node_color=N_color,node_shape=N_shape,node_size=N_size,label_size=T_size,stringsAsFactors = F)
      data_edge <- data.frame(from=E_list$from,to=E_list$to,edge_color=E_color,edge_lwd=E_lwd,edge_lty=E_lty,stringsAsFactors = F)
      structure <- List(node=data_node,edge=data_edge)
    }
  })

  # Network Download(Structure in CSV)
  output$shinyBN.xlsx <- downloadHandler(
    filename = "shinyBN.xlsx",
    content = function(file){
      write.xlsx2(recStruct()[["node"]],file,row.names = F,sheetName="Nodes")
      write.xlsx2(recStruct()[["edge"]],file,row.names = F,sheetName="Edges",append = T)
    }
  )

  # Set evidence & Query!
  RecE <- reactive({
    if(input$AddButtonE == n_AE + 1) {
      n_AE <<- n_AE + 1
      Evid_tab <<- rbind(Evid_tab,data.frame(Evidence = input$inEvidence,Value = input$inEValue,stringsAsFactors=FALSE))
    }

    if(input$delButtonE == n_DE + 1) {
      n_DE <<- n_DE + 1
      indexE = which(Evid_tab$Evidence != input$inEvidence)
      Evid_tab <<- Evid_tab[indexE,]
    }
    Evid_tab <<- Evid_tab[!duplicated(Evid_tab$Evidence, fromLast=TRUE), ]
    rownames(Evid_tab) <- NULL
    Evid_tab
  })
  output$Evi_table <- renderDataTable(RecE(),class="compact",rownames = FALSE,options=list(searching=F,
                                                                                           columnDefs=list(list(className = 'dt-center', targets = 1))))

  RecQ <- reactive({
    fit <- recFit()
    if(input$delButtonQ == 0 & !is.null(fit)) {
      node <- nodes(fit)[length(fit)]
      tmp = fit[[node]]
      value = rownames(tmp$prob)
      Query_tab <<- rbind(Query_tab,data.frame(Query = node,Value = value))
      Query_tab <<- Query_tab[!duplicated(data.frame(Query_tab$Query,Query_tab$Value), fromLast=TRUE),,drop=F]
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
    rownames(Query_tab) <- NULL
    Query_tab
  })

  output$Query_table <- renderDataTable(RecQ(),class="compact",options=list(searching=F,
                                                                            columnDefs=list(list(className = 'dt-center', targets = 1))))

  RecR <- reactive({
    result <<- data.frame()
    fit <- recFit()
    Q <- RecQ()
    if(nrow(Q) && ! is.null(fit)){
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
        result <<- sqldf("select Variable,Level,Proability from result_tmp,Q where Variable=Query and Level=Value")
      }
      if(input$Type == "joint"){
        result <<- rbind(result,melt(q,varNames=dimnames(q),value.name = "Proability",as.is=T))
      }
    }
    result
  })

  # Query results in graph
  output$ResultPlot <- renderPlot({
    if(nrow(RecR())){
      data <- RecR()
      data$Proability <- round(data$Proability*100,1)

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
        g <<- ggplot(data=data_plot,aes(x))+
          geom_bar(aes(weight=p),fill="lightblue")+
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
          data_plot$col <- Color[index+1]
          ucol <- unique(data_plot$col)
          names(ucol) <- ucol
          lab <- Label[index+1]
          lab <- unique(lab)
          lab <- lab[order(unique(ucol))]
          g <<- g + geom_bar(data= data_plot,aes(weight=p,fill=col))+
            scale_fill_manual(NULL,values=ucol,label = lab)+
            theme(legend.position = c(input$RLegend_x/100,input$RLegend_y/100))
          if(input$GC_Label== 'NULL') g <<- g + theme(legend.position='none')
        }
        g
      }else{
        nc <- ncol(data) - 1
        nr <- nrow(data)
        data$IndexXxXxX <- letters[1:nr]
        data2 <- data.frame(lx=rep(data$IndexXxXxX,nc),ly=rep(1:nc*-4,each=nr),lab=unlist2(data[,1:nc]),stringsAsFactors = F)
        g <<-ggplot(data,aes(x=IndexXxXxX))+
          geom_bar(aes(weight=Proability),fill="lightblue")+
          geom_text(aes(x=IndexXxXxX,y=Proability+4,label=Proability))+
          geom_text(data = data2,aes(x = lx,y = ly,label=lab))+
          scale_x_discrete(NULL,breaks=NULL)+
          scale_y_continuous("Predict  Probability(%)",expand=c(0,0),lim=c(nc*-5,105),breaks=c(1:nc*-4,seq(20,100,20)),
                             labels=c(colnames(data)[1:nc],seq(20,100,20)))+
          geom_hline(aes(yintercept=0))+
          geom_line(data=data.frame(n=c(0,0),m=c(0,100)),aes(n,m))+
          theme(panel.background = element_rect(fill = "transparent",colour = NA),
                panel.grid.minor = element_blank(),
                panel.grid.major = element_blank(),
                plot.background = element_rect(fill = "transparent",colour = NA),
                axis.ticks.y = element_line(colour = NA),
                axis.title.y = element_text(angle=90,size = 14))

        if(input$GC_TF){
          index <- sapply(data$Proability,function(a){sum(a > Interval)})
          data$col <- Color[index+1]
          ucol <- unique(data$col)
          names(ucol) <- ucol
          lab <- Label[index+1]
          lab <- unique(lab)
          lab <- lab[order(unique(ucol))]
          g <<- g + geom_bar(data=data,aes(x = IndexXxXxX,weight=Proability,fill=col))+
            scale_fill_manual(NULL,values=ucol,label = lab)+
            theme(legend.position = c(input$RLegend_x/100,input$RLegend_y/100))

          if(input$GC_Label== 'NULL') g <<- g + theme(legend.position='none')
        }
        g
      }
    }else{
      g<<- ggplot(data.frame(x=400,y=400),aes(x,y))+
        geom_text(aes(label="Please choose your query nodes!"),color="red",size=4.5)+
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
