options(warn=-1)

# UI
shinyUI(dashboardPage(

  # Header
  dashboardHeader(title="Bayesian Network:",
                  dropdownMenu(type="notifications",badgeStatus = "primary",icon=icon("question-sign",lib="glyphicon"),
                               notificationItem(icon=icon("question"),"Click to get help",href = "https://github.com/JiajinChen/shinyBN"))),

  # Sidebar
  dashboardSidebar(
    selectInput("inType","Select the type of Data:",c("R Object in R","R Object(.Rdata)","Raw Data(.csv)")),
    conditionalPanel(
      condition = "input.inType == 'R Object in R'",
      textInput("inFit","Enter your Network&Data:(Separated by a comma.)","Asia_fit,Asia_data")
    ),
    conditionalPanel(
      condition = "input.inType == 'Raw Data(.csv)'",
      fileInput("inFile","Choose your Raw Data(.csv):"),
      checkboxInput("inHeader","Header?",TRUE),
      selectInput("inLearnType","Type of Structure Algorithm:",c("Score-Based Algorithms","Constraint-Based Algorithms",
                                                                 "Hybrid Algorithms")),
      conditionalPanel(
        condition = "input.inLearnType == 'Constraint-Based Algorithms'",
        selectInput("inLearn1","Structure Algorithm:",c("Grow-Shrink","Incremental Association","Fast Incremental Association",
                                                       "Interleaved Incremental Association","Max-Min Parents and Children",
                                                       "Semi-Interleaved HITON-PC"))
      ),
      conditionalPanel(
        condition = "input.inLearnType == 'Score-Based Algorithms'",
        selectInput("inLearn2","Structure Algorithm:",c("hill-climbing","tabu search"))
      ),
      conditionalPanel(
        condition = "input.inLearnType == 'Hybrid Algorithms'",
        selectInput("inLearn3","Structure Algorithm:",c("Max-Min Hill Climbing","2-phase Restricted Maximization"))
      ),
      selectInput("inMethod","Parameter Algorithm:",c("Maximum Likelihood parameter estimation"="mle",
                                                 "Bayesian parameter estimation"="bayes")),
      column(width=6,checkboxInput("prior_TF", "Prior?", F)),
      column(width=6,checkboxInput("prior_hide", "Hide?", F)),
      conditionalPanel(
        condition = "input.prior_TF",
        uiOutput("from"),
        uiOutput("to"),
        conditionalPanel(
          condition = "output.from",
          radioButtons("BorW",NULL,c("blacklist","whitelist"),inline = T),
          column(width=6,actionButton("AddButtonP", "Add!",icon=icon("plus"),lib="glyphicon")),
          column(width=6,actionButton("delButtonP", "Delete!",icon=icon("trash"),lib="glyphicon"))
        )
      )
    ),
    conditionalPanel(
      condition = "input.inType == 'R Object(.Rdata)'",
      fileInput("inObject","Choose your R Object(.Rdata):")
    ),
    uiOutput("evidence"),
    uiOutput("E_value"),
    conditionalPanel(
      condition = "output.evidence",
      column(width=6,actionButton("AddButtonE", "Add!",icon=icon("plus"),lib="glyphicon")),
      column(width=6,actionButton("delButtonE", "Delete!",icon=icon("trash"),lib="glyphicon"))
    ),
    uiOutput("query"),
    conditionalPanel(
      condition = "input.Type == 'marginal'",
      uiOutput("Q_value")
    ),
    conditionalPanel(
      condition = "output.query",
      column(width=6,actionButton("AddButtonQ", "Add!",icon=icon("plus"),lib="glyphicon")),
      column(width=6,actionButton("delButtonQ", "Delete!",icon=icon("trash"),lib="glyphicon")),
      radioButtons("Type","Choose the type:",c("Marginal" = "marginal","Joint" = "joint"),inline=T)
    )
  ),

  # Body
  dashboardBody(
    shinyDashboardThemes(
      theme = "blue_gradient"
      # theme = "onenote"
    ),
    tabsetPanel(
      # Main Panel
      tabPanel("Main",
               br(),
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
                        ))),
               conditionalPanel(
                 condition = "input.prior_TF &  ! input.prior_hide",
                 column(width=6,dataTableOutput("Pri_table"))
               ),
               fixedPanel(top = 65, right=80, width=430,height=15,draggable = F,
                          column(width=3,selectInput("inLayout",NULL,c("dot","neato","twopi","circo","fdp"))),
                          column(width=1,h5("W:",align="center")),
                          column(width=2,textInput("Pwidth",NULL,8)),
                          column(width=1,h5("H:",align="center")),
                          column(width=2,textInput("Pheight",NULL,8)),
                          column(width=1,downloadLink("shinyBN.pdf",shiny::img(src="Network_download.png",height=30,width=30))),
                          column(width=1,NULL),
                          column(width=1,downloadLink("Result.pdf",shiny::img(src="Result_download.png",height=30,width=30))))
      ),
      # Nodes Panel
      tabPanel("Nodes",
               column(width=12,h4("Please choose the initial parameters for all nodes:")),
               column(width=6,selectInput("IN_color","Nodes Color:",c("lightblue","red","orange","yellow","green","blue","Other"))),
               conditionalPanel(
                 condition = "input.IN_color == 'Other'",
                 column(width=6,textInput("IN_Other_color","Input Node Color:",""))
               ),
               column(width=6,selectInput("IN_Nshape","Nodes Shape:",c("Circle","Square","Triangle","Rhombus"))),
               column(width=6,sliderInput("IN_Nsize","Nodes Size:",min=1,max=25,value=12,step=1)),
               column(width=6,sliderInput("IN_Tsize","Text size:",min=1,max=10,value=5,step=1)),
               column(width=12,h4("Change individual nodes:")),
               column(width=6,uiOutput("N")),
               column(width=6,selectInput("Nodes_type","Select the Type:",
                                          c("Node Color","Node Shape","Node Size","Text Size"))),
               conditionalPanel(
                 condition = "input.Nodes_type == 'Node Color'",
                 column(width=6,selectInput("N_color","Node Color:",c("lightblue","red","orange","yellow","green","blue","Other"))),
                 conditionalPanel(
                   condition = "input.N_color == 'Other'",
                   column(width=6,textInput("N_Other_color","Input Node color:",""))
                 )
               ),
               conditionalPanel(
                 condition = "input.Nodes_type == 'Node Shape'",
                 column(width=6,selectInput("N_Nshape","Node Shape:",c("Circle","Square","Triangle","Rhombus")))
               ),
               conditionalPanel(
                 condition = "input.Nodes_type == 'Node Size'",
                 column(width=6,sliderInput("N_Nsize","Node Size:",min=1,max=25,value=8,step=1))
               ),
               conditionalPanel(
                 condition = "input.Nodes_type == 'Text Size'",
                 column(width=6,sliderInput("N_Tsize","Text Size:",min=1,max=10,value=3,step=1))
               ),
               column(width=12,
                      column(width=4,actionButton("AddButtonNodes", "Add!",icon=icon("plus"),lib="glyphicon")),
                      column(width=4,actionButton("DelButtonNodes", "Delete!",icon=icon("trash"),lib="glyphicon")),
                      column(width=4,actionButton("RenderNodes", "Render!",icon=icon("refresh"),lib="glyphicon"))),
               br(),
               column(width=12,dataTableOutput("Ncolorsize_table")),

               column(width=12,h4("Legend:")),
               column(width=6,checkboxInput("N_TF","Add Node Legend?",FALSE)),
               conditionalPanel(
                 condition = "input.N_TF == true",
                 column(width = 12,
                        column(width=8,tableOutput("NlegTab"))),
                 column(width=12,strong("Input the Label:")),
                 column(width=6,textInput("N_Label",NULL)),
                 column(width=12,helpText("Notes: From top to bottom,Separated by a comma.")),
                 column(width=6,selectInput("Hori_N","Direction?",c("vertical","horizontal"))),
                 column(width=6,selectInput("NLegend_posion","Select the position:",c("right","left","top","bottom","topleft","topright","bottomleft","bottomright","Other"))),
                 conditionalPanel(
                   condition = "input.NLegend_posion == 'Other'",
                   column(width=12,
                          column(width=6,numericInput("NLegend_x","Input the X position:",1,min=0,max = 100)),
                          column(width=6,numericInput("NLegend_y","Input the Y position:",1,min=0,max = 100))),
                   column(width=12,helpText("Notes:For x: Number from 0 (left) to 100 (right); For y: Number from 0 (bottom) to 100 (top)."))
                 )
               )
      ),
      # Edges Panel
      tabPanel("Edges",
               column(width=12,h4("Please choose the initial parameters for all edges:")),
               column(width=6,selectInput("IE_color","Edges Color:",c("gray","red","orange","yellow","green","blue","Other"))),
               conditionalPanel(
                 condition = "input.IE_color == 'Other'",
                 column(width=6,textInput("IE_Other_color","Input Edges color:",""))
               ),
               column(width=6,selectInput("IE_type","Edges Type:",c("solid","dashed","dotted","dotdash","longdash","twodash"))),
               column(width=3,radioButtons("IE_size_type","Edges Width:",c("Self-defined","Arc Strength"))),
               conditionalPanel(
                 condition = "input.IE_size_type == 'Self-defined'",
                 column(width=3,""),
                 column(width=6,sliderInput("IE_size","Edges Width:",min=0,max=5,step=0.5,value=1))
               ),
               conditionalPanel(
                 condition = "input.IE_size_type == 'Arc Strength'",
                 column(width=3,radioButtons("IE_Criterion","Criterion?",c("Independence Test","Score Function"))),
                 conditionalPanel(
                   condition = "input.IE_Criterion == 'Independence Test'",
                   column(width=6,selectInput("IE_Independence","Tests:",list(`Mutual information` = c("Asymptotic chi-square test"="mi", "Adjusted degrees of freedom"="mi-adf",  "Monte Carlo permutation test"="mc-mi",
                                                                                                 "sequential Monte Carlo permutation test"="smc-mi","Semiparametric test"="sp-mi"),
                                                                        `Shrinkage estimator for the mutual information` = c("Shrinkage estimator for the mutual information"="mi-sh"),
                                                                        `Pearson's X^2` = c("Asymptotic chi-square test"="x2", "Adjusted degrees of freedom"="x2-adf",  "Monte Carlo permutation test"="mc-x2",
                                                                                            "sequential Monte Carlo permutation test"="smc-x2","Semiparametric test"="sp-x2"),
                                                                        `Jonckheere-Terpstra` = c("Asymptotic normal test"="jt","Monte Carlo permutation test"="mc-jt","sequential Monte Carlo permutation test"="smc-jt"))))
                 ),
                 conditionalPanel(
                   condition = "input.IE_Criterion == 'Score Function'",
                   column(width=6,selectInput("IE_Score","Scores:",c("Multinomial log-likelihood score"="loglik","Akaike Information Criterion score"="aic","Bayesian Information Criterion score"="bic",
                                                                     "Bayesian Dirichlet equivalent score"="bde","Bayesian Dirichlet sparse score"="bds","Modified Bayesian Dirichlet equivalent score"="mbde",
                                                                     "Locally averaged Bayesian Dirichlet score"="bdla","K2 score"="k2")))
                 )
               ),
               column(width=12,h4("Change individual edges:")),
               column(width=6,uiOutput("E")),
               column(width=6,uiOutput("E_Render")),
               conditionalPanel(
                 condition = "input.Edges_type == 'Edge Color'",
                 column(width=6,selectInput("E_color","Edge Color:",c("gray","red","orange","yellow","green","blue","Other"))),
                 conditionalPanel(
                   condition = "input.E_color == 'Other'",
                   column(width=6,textInput("E_Other_color","Input Edge color:",""))
                 )
               ),
               conditionalPanel(
                 condition = "input.Edges_type == 'Edge Type'",
                 column(width=6,selectInput("E_type","Edge Type:",c("solid","dashed","dotted","dotdash","longdash","twodash")))
               ),
               conditionalPanel(
                 condition = "input.Edges_type == 'Edge Width'",
                 column(width=6,sliderInput("E_size","Edge Width:",min=0,max=5,step=0.5,value=1))
               ),
               br(),
               column(width=12,
                      column(width=4,actionButton("AddButtonEdges", "Add!",icon=icon("plus"),lib="glyphicon")),
                      column(width=4,actionButton("DelButtonEdges", "Delete!",icon=icon("trash"),lib="glyphicon")),
                      column(width=4,actionButton("RenderEdges", "Render!",icon=icon("refresh"),lib="glyphicon"))),
               br(),
               column(width=12,dataTableOutput("Ecolorsize_table")),

               column(width=12,h4("Legend:")),
               column(width=6,checkboxInput("E_TF","Add Edge Legend?",FALSE)),
               conditionalPanel(
                 condition = "input.E_TF == true",
                 column(width = 12,
                        column(width=8,tableOutput("ElegTab"))),
                 column(width=12,strong("Input the Label:")),
                 column(width=12,textInput("E_Label",NULL)),
                 column(width=12,helpText("Notes: From top to bottom,Separated by a comma.")),
                 conditionalPanel(
                   condition = "input.N_TF == false",
                   column(width=6,selectInput("Hori_E","Direction?",c("vertical","horizontal"))),
                   column(width=6,selectInput("ELegend_posion","Select the position:",c("right","left","top","bottom","topleft","topright","bottomleft","bottomright","Other"))),
                   conditionalPanel(
                     condition = "input.ELegend_posion == 'Other'",
                     column(width=12,
                            column(width=6,numericInput("ELegend_x","Input the X position:",1,min=0,max = 100)),
                            column(width=6,numericInput("ELegend_y","Input the Y position:",1,min=0,max = 100))),
                     column(width=12,helpText("Notes:For x: Number from 0 (left) to 100 (right); For y: Number from 0 (bottom) to 100 (top)."))
                   )
                 )
               )
      )
    )
  )
))
