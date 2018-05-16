options(warn=-1)
library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(DT)
library("bnlearn")
library(gRain)
library(Rgraphviz)
library(reshape2)
library(AnnotationDbi)
library(plotly)
library(svgPanZoom)
library(svglite)
library(SVGAnnotation)
library(gridSVG)
library(shinyjqui)
library(igraph)
library(sqldf)
Evid_tab<-data.frame(Evidence=character(),Value=character(),stringsAsFactors=FALSE)
Query_tab<-data.frame(Query=character(),stringsAsFactors=FALSE)
Ncolorsize_tab <- data.frame()
Ecolorsize_tab <- data.frame()
Nlegend_tab <- data.frame()
Elegend_tab <- data.frame()
result <- data.frame()

n_AE <- 0
n_DE <- 0
n_AQ <- 0
n_DQ <- 0
n_ANode  <- 0
n_DNode  <- 0
n_AEdge  <- 0
n_DEdge  <- 0
# n_AddLegendN <-0
# n_AddLegendE <-0
n_RdN <- 0
n_RdE <- 0

shinyUI(dashboardPage(
  dashboardHeader(title="Bayesian Network:"),
  dashboardSidebar(
    # h3("Bayesian Network:"),
    selectInput("inType","Select the type of Data",c("R Object in R","Row Data(.csv)","R object(.Rdata)")),
    conditionalPanel(
      condition = "input.inType == 'R Object in R'",
      textInput("inFit","Enter your Network:","fit")
    ),
    conditionalPanel(
      condition = "input.inType == 'Row Data(.csv)'",
      fileInput("inFile","Choose your Row Data(.csv):"),
      checkboxInput("inHeader","Header?",TRUE),
      selectInput("inLearnType","Select the type of Algorithm",c("Constraint-Based Algorithms","Score-Based Algorithms",
                                                                 "Hybrid Algorithms","Local Discovery Algorithms")),
      conditionalPanel(
        condition = "input.inLearnType == 'Constraint-Based Algorithms'",
        selectInput("inLearn1","Learning Algorithm:",c("Grow-Shrink","Incremental Association","Fast Incremental Association",
                                                       "Interleaved Incremental Association","Max-Min Parents and Children",
                                                       "Semi-Interleaved HITON-PC"))
      ),
      conditionalPanel(
        condition = "input.inLearnType == 'Score-Based Algorithms'",
        selectInput("inLearn2","Learning Algorithm:",c("hill-climbing","tabu search"))
      ),
      conditionalPanel(
        condition = "input.inLearnType == 'Hybrid Algorithms'",
        selectInput("inLearn3","Learning Algorithm:",c("Max-Min Hill Climbing","2-phase Restricted Maximization"))
      ),
      conditionalPanel(
        condition = "input.inLearnType == 'Local Discovery Algorithms'",
        selectInput("inLearn4","Learning Algorithm:",c("ARACNE","Chow-Liu"))
      ),
      selectInput("inMethod","Method for fit:",c("Maximum Likelihood parameter estimation"="mle",
                                                 "Bayesian parameter estimation"="bayes"))
    ),
    conditionalPanel(
      condition = "input.inType == 'R object(.Rdata)'",
      fileInput("inObject","Choose your R object(.Rdata):")
    ),
    uiOutput("evidence"),
    uiOutput("value"),
    column(width=12,
           column(width=6,actionButton("AddButtonE", "Add!",icon=icon("plus"),lib="glyphicon")),
           column(width=6,actionButton("delButtonE", "Delete!",icon=icon("trash"),lib="glyphicon"))),
    uiOutput("query"),
    column(width=12,
           column(width=6,actionButton("AddButtonQ", "Add!",icon=icon("plus"),lib="glyphicon")),
           column(width=6,actionButton("delButtonQ", "Delete!",icon=icon("trash"),lib="glyphicon"))),
    radioButtons("Type","Choose the type:",c("Marginal" = "marginal","Joint" = "joint"),inline=T)
    # actionButton("Go", "Go!",icon=icon("play-circle"),lib="glyphicon")
  ),
  dashboardBody(
    shinyDashboardThemes(
      theme = "blue_gradient"
    ),
    tabsetPanel(
      tabPanel("Main",
               fixedPanel(top = 60, right=70, width=400,height=15,draggable = F,
                          column(width=1,h5("W:",align="center")),
                          column(width=3,numericInput("Pwidth",NULL,8)),
                          column(width=1,h5("H:",align="center")),
                          column(width=3,numericInput("Pheight",NULL,8)),
                          column(width=2,downloadLink("shinyBN.pdf","Network Download")),
                          column(width=1,strong("")),
                          column(width=2,downloadLink("Result.pdf","Result Download"))),
               column(width=12,h1("")),
               column(width=6,
                      jqui_resizabled(
                        tabBox(side = "left", width = NULL,height = 500,selected = "Network",
                               tabPanel("Network",
                                        column(width=2,strong("Layout:")),
                                        column(width=4,selectInput("inLayout",NULL,c("dot","neato","twopi","circo","fdp"),selectize=F)),
                                        # column(width=1,strong("W:")),
                                        # column(width=2,numericInput("Pwidth",NULL,8)),
                                        # column(width=1,strong("H:")),
                                        # column(width=2,numericInput("Pheight",NULL,8)),
                                        # downloadLink("shinyBN.pdf",h4("PDF")),
                                        # downloadLink("shinyBN.png","PNG"),
                                        # downloadLink("shinyBN.svg","SVG"),
                                        # column(width=1,downloadLink("shinyBN.pdf","PDF")),
                                        # column(width=1,downloadLink("shinyBN.png","PNG")),
                                        # column(width=1,downloadLink("shinyBN.svg","SVG")),
                                        svgPanZoomOutput("outSVG")),
                               tabPanel("Evidence",
                                        dataTableOutput("Evi_table")
                               )
                        ))),
               column(width=6,
                      jqui_resizabled(
                        tabBox(side = "left", width = NULL,height = 500,selected = "Graph",
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
                                          column(width=6,textInput("GC_Interval",h4("Input the interval value:"),"33,66")),
                                          column(width=6,textInput("GC_Color",h4("Input the color:"),"green,orange,red")),
                                          helpText("Notes:Separated by a comma."),
                                          column(width=6,textInput("GC_Label",h4("Input the label:"),"low,middle,high")),
                                          column(width=12,helpText("Notes:Separated by a comma. If not, please input NULL.")),
                                          column(width=12,
                                                 column(width=6,numericInput("RLegend_x","Input the X position:",95,min=0,max = 100)),
                                                 column(width=6,numericInput("RLegend_y","Input the Y position:",80,min=0,max = 100))
                                          ),
                                          column(width=12,helpText("Notes:For x:Number from 0 (left) to 100 (right); For y:Number from 0 (bottom) to 100 (top)."))
                                        )
                               )
                        )))
      ),
      tabPanel("Nodes",
               column(width=12,h4("Please choose the initial parameters for each nodes:")),
               column(width=6,selectInput("IN_color","Nodes Color:",c("lightblue","red","orange","yellow","green","blue","Other"))),
               conditionalPanel(
                 condition = "input.IN_color == 'Other'",
                 column(width=6,textInput("IN_Other_color","Nodes Color:",""))
               ),
               column(width=6,selectInput("IN_Nshape","Nodes Shape:",c("Circle","Square","Triangle","Rhombus"))),
               column(width=6,sliderInput("IN_Nsize","Nodes Size:",min=1,max=25,value=8,step=1)),
               column(width=6,sliderInput("IN_Tsize","Text size:",min=1,max=10,value=3,step=1)),
               column(width=12,h4("Render your plot:")),
               column(width=6,uiOutput("N")),
               column(width=6,selectInput("Nodes_type","Select the Type:",
                                          c("Node Color","Node Shape","Node Size","Text Size"))),
               conditionalPanel(
                 condition = "input.Nodes_type == 'Node Color'",
                 column(width=6,selectInput("N_color","Select:",c("lightblue","red","orange","yellow","green","blue","Other"))),
                 conditionalPanel(
                   condition = "input.N_color == 'Other'",
                   column(width=6,textInput("N_Other_color","Input the color:",""))
                 )
               ),
               conditionalPanel(
                 condition = "input.Nodes_type == 'Node Shape'",
                 column(width=6,selectInput("N_Nshape","Select:",c("Circle","Square","Triangle","Rhombus")))
               ),
               conditionalPanel(
                 condition = "input.Nodes_type == 'Node Size'",
                 column(width=6,sliderInput("N_Nsize","Select:",min=1,max=25,value=8,step=1))
               ),
               conditionalPanel(
                 condition = "input.Nodes_type == 'Text Size'",
                 column(width=6,sliderInput("N_Tsize","Select:",min=1,max=10,value=3,step=1))
               ),
               column(width=12,
                      column(width=4,actionButton("AddButtonNodes", "Add!",icon=icon("plus"),lib="glyphicon")),
                      column(width=4,actionButton("DelButtonNodes", "Delete!",icon=icon("trash"),lib="glyphicon")),
                      column(width=4,actionButton("RenderNodes", "Render!",icon=icon("refresh"),lib="glyphicon"))),
               column(width=12,h5("")),
               column(width=12,dataTableOutput("Ncolorsize_table")),

               column(width=12,h4("Legend:")),
               column(width=6,checkboxInput("N_TF","Add Node Legend?",FALSE)),
               conditionalPanel(
                 condition = "input.N_TF == true",
                 column(width = 12,
                        column(width=8,tableOutput("NlegTab"))),
                 column(width=12,strong("Input the Label:")),
                 column(width=12,textInput("N_Label",NULL)),
                 column(width=12,helpText("Notes:From top to bottom,Separated by a comma.")),
                 column(width=6,selectInput("Hori_N","Direction?",c("vertical","horizontal"))),
                 column(width=6,selectInput("NLegend_posion","Select the position",c("right","left","top","bottom","topleft","topright","bottomleft","bottomright","Other"))),
                 conditionalPanel(
                   condition = "input.NLegend_posion == 'Other'",
                   column(width=12,
                          column(width=6,numericInput("NLegend_x","Input the X position:",1,min=0,max = 100)),
                          column(width=6,numericInput("NLegend_y","Input the Y position:",1,min=0,max = 100)))
                 ),
                 column(width=12,helpText("Notes:For x:Number from 0 (left) to 100 (right); For y:Number from 0 (bottom) to 100 (top)."))
               )
      ),
      tabPanel("Edges",
               column(width=12,h4("Please choose the initial parameters for each edges:")),
               column(width=6,selectInput("IE_color","Edges Color:",c("gray","red","orange","yellow","green","blue","Other"))),
               conditionalPanel(
                 condition = "input.IE_color == 'Other'",
                 column(width=6,textInput("IE_Other_color","Input the color:",""))
               ),
               column(width=6,selectInput("IE_type","Edges Type:",c("solid","dashed","dotted","dotdash","longdash","twodash"))),
               column(width=6,sliderInput("IE_size","Edges Width:",min=1,max=10,value=1)),
               column(width=12,h4("Render your plot:")),
               column(width=6,uiOutput("E")),
               column(width=6,selectInput("Edges_type","Select the Type:",
                                          c("Edge Color","Edge Type","Edge Width"))),
               conditionalPanel(
                 condition = "input.Edges_type == 'Edge Color'",
                 column(width=6,selectInput("E_color","Select:",c("gray","red","orange","yellow","green","blue","Other"))),
                 conditionalPanel(
                   condition = "input.E_color == 'Other'",
                   column(width=6,textInput("E_Other_color","Input the color:",""))
                 )
               ),
               conditionalPanel(
                 condition = "input.Edges_type == 'Edge Type'",
                 column(width=6,selectInput("E_type","Select:",c("solid","dashed","dotted","dotdash","longdash","twodash")))
               ),
               conditionalPanel(
                 condition = "input.Edges_type == 'Edge Width'",
                 column(width=6,sliderInput("E_size","Select:",min=1,max=10,value=2))
               ),
               h1(""),
               column(width=12,
                      column(width=4,actionButton("AddButtonEdges", "Add!",icon=icon("plus"),lib="glyphicon")),
                      column(width=4,actionButton("DelButtonEdges", "Delete!",icon=icon("trash"),lib="glyphicon")),
                      column(width=4,actionButton("RenderEdges", "Render!",icon=icon("refresh"),lib="glyphicon"))),
               column(width=12,h5("")),
               column(width=12,dataTableOutput("Ecolorsize_table")),

               column(width=12,h4("Legend:")),
               column(width=6,checkboxInput("E_TF","Add Edge Legend?",FALSE)),
               conditionalPanel(
                 condition = "input.E_TF == true",
                 column(width = 12,
                        column(width=8,tableOutput("ElegTab"))),
                 column(width=12,strong("Input the Label:")),
                 column(width=12,textInput("E_Label",NULL)),
                 column(width=12,helpText("Notes:From top to bottom,Separated by a comma.")),
                 conditionalPanel(
                   condition = "input.N_TF == false",
                   column(width=6,selectInput("Hori_E","Direction?",c("vertical","horizontal"))),
                   column(width=6,selectInput("ELegend_posion","Select the position",c("right","left","top","bottom","topleft","topright","bottomleft","bottomright","Other"))),
                   conditionalPanel(
                     condition = "input.ELegend_posion == 'Other'",
                     column(width=12,
                            column(width=6,numericInput("ELegend_x","Input the X position:",1,min=0,max = 100)),
                            column(width=6,numericInput("ELegend_y","Input the Y position:",1,min=0,max = 100)))
                   ),
                   column(width=12,helpText("Notes:For x:Number from 0 (left) to 100 (right); For y:Number from 0 (bottom) to 100 (top)."))
                 )
               )
      )
    )
  )
))
