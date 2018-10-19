options(warn=-1)

# UI
shinyUI(dashboardPage(

  # Header
  dashboardHeader(title="Bayesian Network",
                  dropdownMenu(type="notifications",badgeStatus = "primary",icon=icon("question-sign",lib="glyphicon"),
                               notificationItem(icon=icon("question"),"Click to get help",href = "https://github.com/JiajinChen/shinyBN"))),

  # Sidebar
  dashboardSidebar(
    selectInput("inType","Select the type of Data:",c("R Object in R","R Object(.Rdata)","Raw Data(.csv)","Render Continue...")),
    conditionalPanel(
      condition = "input.inType == 'R Object in R'",
      textInput("inFit","Enter your Network&Data:(Separated by a comma.)","Asia_fit,Asia_data")
    ),
    conditionalPanel(
      condition = "input.inType == 'Raw Data(.csv)'",
      fileInput("inFile","Choose your Raw Data(.csv):"),
      checkboxInput("inHeader","Header?",TRUE),
      selectInput("inLearnType","Type of Structure Algorithm:",c("Score-Based Algorithms","Constraint-Based Algorithms",
                                                                 "Hybrid Algorithms","Bootstrap")),
      conditionalPanel(
        condition = "input.inLearnType == 'Constraint-Based Algorithms'",
        selectInput("inLearn1","Structure Algorithm:",c("Grow-Shrink","Incremental Association","Fast Incremental Association",
                                                       "Interleaved Incremental Association"))
      ),
      conditionalPanel(
        condition = "input.inLearnType == 'Score-Based Algorithms'",
        selectInput("inLearn2","Structure Algorithm:",c("hill-climbing","tabu search")),
        selectInput("inScore2","Scores:",c("Multinomial log-likelihood score"="loglik","Akaike Information Criterion score"="aic","Bayesian Information Criterion score"="bic",
                                          "Bayesian Dirichlet equivalent score"="bde","Bayesian Dirichlet sparse score"="bds","Modified Bayesian Dirichlet equivalent score"="mbde",
                                          "Locally averaged Bayesian Dirichlet score"="bdla","K2 score"="k2"))
      ),
      conditionalPanel(
        condition = "input.inLearnType == 'Hybrid Algorithms'",
        selectInput("inLearn3","Structure Algorithm:",c("Max-Min Hill Climbing","2-phase Restricted Maximization"))
      ),
      conditionalPanel(
        condition = "input.inLearnType == 'Bootstrap'",
        selectInput("inLearn4","Structure Algorithm:",c("hill-climbing"="hc","tabu search"='tabu',"Grow-Shrink"='gs',"Incremental Association"='iamb',
                                                        "Fast Incremental Association"='fast.iamb',"Interleaved Incremental Association"='inter.iamb',
                                                        "Max-Min Hill Climbing"='mmhc',"2-phase Restricted Maximization"='rsmax2')),
        conditionalPanel(
          condition = "input.inLearn4 =='hc' || input.inLearn4 =='tabu'",
          selectInput("inScore4","Score:",c("Multinomial log-likelihood score"="loglik","Akaike Information Criterion score"="aic","Bayesian Information Criterion score"="bic",
                                          "Bayesian Dirichlet equivalent score"="bde","Bayesian Dirichlet sparse score"="bds","Modified Bayesian Dirichlet equivalent score"="mbde",
                                          "Locally averaged Bayesian Dirichlet score"="bdla","K2 score"="k2"))),
        radioButtons("N_Boot","Num of Boot:",c(10,500,1000),inline=T)
      ),
      column(width=6,checkboxInput("prior_TF", "Prior?", F)),
      column(width=6,checkboxInput("prior_hide", "Hide?", F)),
      conditionalPanel(
        condition = "input.prior_TF",
        uiOutput("from"),
        uiOutput("to"),
        radioButtons("BorW",NULL,c("blacklist","whitelist"),inline = T),
        column(width=4,actionButton("AddButtonP", "Add",icon=icon("plus"),lib="glyphicon")),
        # column(width=1,NULL),
        column(width=4,actionButton("delButtonP", "Del",icon=icon("trash"),lib="glyphicon")),
        # column(width=1,NULL),
        column(width=4,actionButton("GoButtonP", "Go",icon=icon("play"),lib="glyphicon"))
      ),
      selectInput("inMethod","Parameter Algorithm:",c("Maximum Likelihood parameter estimation"="mle",
                                                      "Bayesian parameter estimation"="bayes"))
    ),
    conditionalPanel(
      condition = "input.inType == 'R Object(.Rdata)'",
      fileInput("inObject","Choose your R Object(.Rdata):")
    ),
    conditionalPanel(
      condition = "input.inType == 'Render Continue...'",
      fileInput("inContinue","Choose your Structure File(.xlsx):")

    ),
    uiOutput("evidence"),
    uiOutput("E_value"),
    conditionalPanel(
      condition = "output.evidence",
      column(width=6,actionButton("AddButtonE", "Add",icon=icon("plus"),lib="glyphicon")),
      column(width=6,actionButton("delButtonE", "Delete",icon=icon("trash"),lib="glyphicon"))
    ),
    uiOutput("query"),
    conditionalPanel(
      condition = "input.Type == 'marginal'",
      uiOutput("Q_value")
    ),
    conditionalPanel(
      condition = "output.query",
      column(width=6,actionButton("AddButtonQ", "Add",icon=icon("plus"),lib="glyphicon")),
      column(width=6,actionButton("delButtonQ", "Delete",icon=icon("trash"),lib="glyphicon")),
      radioButtons("Type","Choose the type:",c("Marginal" = "marginal","Joint" = "joint"),inline=T)
    )
  ),

  # Body
  dashboardBody(

    shinyDashboardThemeDIY(

      # 蓝色 RGB: "rgb(51,105,232)"  "#4486F4"
      # 红色 RGB: "rgb(213,15,37)"   "#EA4335"
      # 绿色 RGB: "rgb(0,153,37)"    "#35A755"
      # 黄色 RGB: "rgb(238,178,17)"  "#FBBB04"
      ### general
      appFontFamily = "Arial"
      ,appFontColor = "rgb(0,0,0)"
      ,bodyBackColor = "rgb(248,248,248)"

      ### header
      ,logoBackColor = "rgb(51,105,232)"  #"#4486F4"
      # cssGradientThreeColors(
      #   direction = "right"
      #   ,colorStart = "rgba(44,222,235,1)"
      #   ,colorMiddle = "rgba(44,222,235,1)"
      #   ,colorEnd = "rgba(0,255,213,1)"
      #   ,colorStartPos = 0
      #   ,colorMiddlePos = 30
      #   ,colorEndPos = 100
      # )
      # "rgb(51,105,232)" #"rgb(23,103,124)"     #左header

      ,headerButtonBackColor = "rgb(238,238,238)" #header的按钮背景
      ,headerButtonIconColor = "rgb(75,75,75)"    #header的按钮颜色
      ,headerButtonBackColorHover = "rgb(210,210,210)" #header的鼠标hover按钮背景
      ,headerButtonIconColorHover = "rgb(0,0,0)"       #header的鼠标hover按钮颜色

      ,headerBackColor = "rgb(238,238,238)"  #"rgb(51,105,232)"  #右header
      ,headerBoxShadowColor = "#aaaaaa"      #右header下面一小条阴影
      ,headerBoxShadowSize = "2px 2px 2px"   #右header下面一小条阴影 大小

      ### sidebar
      ,sidebarBackColor = cssGradientThreeColors(   #sidebar渐变色
        direction = "down"
        ,colorStart = "rgb(213,15,37)"   #"rgb(20,97,117)"
        ,colorMiddle = "rgb(238,178,17)" #"rgb(56,161,187)"
        ,colorEnd = "rgb(0,153,37)"      #"rgb(3,22,56)"
        ,colorStartPos = 0
        ,colorMiddlePos = 50
        ,colorEndPos = 100
      )
      ,sidebarPadding = 0   #sidebar边缘大小

      ,sidebarMenuBackColor = "transparent"
      ,sidebarMenuPadding = 0
      ,sidebarMenuBorderRadius = 0

      ,sidebarShadowRadius = "3px 5px 5px"
      ,sidebarShadowColor = "#aaaaaa"

      ,sidebarUserTextColor = "rgb(255,255,255)"

      ,sidebarSearchBackColor = "rgb(55,72,80)"
      ,sidebarSearchIconColor = "rgb(153,153,153)"
      ,sidebarSearchBorderColor = "rgb(55,72,80)"

      ,sidebarTabTextColor = "rgb(255,255,255)"
      ,sidebarTabTextSize = 13
      ,sidebarTabBorderStyle = "none none solid none"
      ,sidebarTabBorderColor = "rgb(35,106,135)"
      ,sidebarTabBorderWidth = 100

      ,sidebarTabBackColorSelected = cssGradientThreeColors(
        direction = "right"
        ,colorStart = "rgba(44,222,235,1)"
        ,colorMiddle = "rgba(44,222,235,1)"
        ,colorEnd = "rgba(0,255,213,1)"
        ,colorStartPos = 0
        ,colorMiddlePos = 30
        ,colorEndPos = 100
      )
      ,sidebarTabTextColorSelected = "rgb(0,0,0)"
      ,sidebarTabRadiusSelected = "0px 20px 20px 0px"

      ,sidebarTabBackColorHover = cssGradientThreeColors(
        direction = "right"
        ,colorStart = "rgba(44,222,235,1)"
        ,colorMiddle = "rgba(44,222,235,1)"
        ,colorEnd = "rgba(0,255,213,1)"
        ,colorStartPos = 0
        ,colorMiddlePos = 30
        ,colorEndPos = 100
      )
      ,sidebarTabTextColorHover = "rgb(50,50,50)"
      ,sidebarTabBorderStyleHover = "none none solid none"
      ,sidebarTabBorderColorHover = "rgb(75,126,151)"
      ,sidebarTabBorderWidthHover = 1
      ,sidebarTabRadiusHover = "0px 20px 20px 0px"

      ### boxes
      ,boxBackColor = "rgb(255,255,255)"
      ,boxBorderRadius = 5
      ,boxShadowSize = "0px 1px 1px"
      ,boxShadowColor = "rgba(0,0,0,.1)"
      ,boxTitleSize = 16
      ,boxDefaultColor = "rgb(210,214,220)"
      ,boxPrimaryColor = "rgba(44,222,235,1)"
      ,boxSuccessColor = "rgba(0,255,213,1)"
      ,boxWarningColor = "rgb(244,156,104)"
      ,boxDangerColor = "rgb(255,88,55)"

      ,tabBoxTabColor = "rgb(255,255,255)"
      ,tabBoxTabTextSize = 14
      ,tabBoxTabTextColor = "rgb(0,0,0)"
      ,tabBoxTabTextColorSelected = "rgb(0,0,0)"
      ,tabBoxBackColor = "rgb(255,255,255)"
      ,tabBoxHighlightColor = "rgba(44,222,235,1)"
      ,tabBoxBorderRadius = 5

      ### inputs
      ,buttonBackColor = "rgb(245,245,245)"
      ,buttonTextColor = "rgb(0,0,0)"
      ,buttonBorderColor = "rgb(200,200,200)"
      ,buttonBorderRadius = 5

      ,buttonBackColorHover = "rgb(235,235,235)"
      ,buttonTextColorHover = "rgb(100,100,100)"
      ,buttonBorderColorHover = "rgb(200,200,200)"

      ,textboxBackColor = "rgb(255,255,255)"
      ,textboxBorderColor = "rgb(200,200,200)"
      ,textboxBorderRadius = 5
      ,textboxBackColorSelect = "rgb(245,245,245)"
      ,textboxBorderColorSelect = "rgb(200,200,200)"

      ### tables
      ,tableBackColor = "rgb(255,255,255)"
      ,tableBorderColor = "rgb(240,240,240)"
      ,tableBorderTopSize = 1
      ,tableBorderRowSize = 1

    ),

    # theme_self,
    # shinyDashboardThemes(
    #   theme = "blue_gradient"
    #   # theme = "onenote"
    # ),
    tabsetPanel(
      # Main Panel
      tabPanel("Main",
               br(),
               uiOutput("Main"),
               conditionalPanel(
                 condition = "input.inType == 'Raw Data(.csv)' & input.prior_TF &  ! input.prior_hide",
                 column(width=6,dataTableOutput("Pri_table"))
               ),
               fixedPanel(top = 65, right=80, width=430,height=15,draggable = F,
                          conditionalPanel(
                            condition = "input.inType != 'Render Continue...'",
                            column(width=3,selectInput("inLayout",NULL,c("dot","neato","twopi","circo","fdp")))
                          ),
                          column(width=1,h5("W:",align="center")),
                          column(width=2,textInput("Pwidth",NULL,8)),
                          column(width=1,h5("H:",align="center")),
                          column(width=2,textInput("Pheight",NULL,8)),
                          column(width=1,actionLink("shinyBN_choose",shiny::img(src="Network_download.png",height=30,width=30))),
                          column(width=1,NULL),
                          conditionalPanel(
                            condition = "input.inType != 'Render Continue...'",
                            column(width=1,downloadLink("Result.pdf",shiny::img(src="Result_download.png",height=30,width=30))))
                          ),
               fixedPanel(top = 105,right=100,width=160,draggable = F,
                          uiOutput("Choose_download")),
               fixedPanel(top = 104,right=127,width=4,draggable = F,
                          uiOutput("Close"))
      ),
      # Nodes Panel
      tabPanel("Nodes",
               conditionalPanel(
                 condition = "input.inType != 'Render Continue...'",
                 column(width=12,h4("Please choose the initial parameters for all nodes:")),
                 column(width=12,
                        column(width=4,radioButtons("IN_color_type","Nodes Color:",c("Self-defined","SCI-Style","Pic-Style"))),
                        conditionalPanel(
                          condition = "input.IN_color_type == 'Self-defined'",
                          column(width=4,selectInput("IN_color","Nodes Color:",c("lightblue","red","orange","yellow","green","blue","Other"))),
                          conditionalPanel(
                            condition = "input.IN_color == 'Other'",
                            column(width=4,textInput("IN_Other_color","Input Node Color:",""))
                          )),
                        conditionalPanel(
                          condition = "input.IN_color_type == 'SCI-Style'",
                          column(width=4,selectInput("SCI_Name","Style:",list(`Journal` = c("NPG","Lancet","JAMA","NEJM","JCO","AAAS"),
                                                                              `Technology` = c("Google","Twitter","Facebook","Airbnb","Etsy","23andme"),
                                                                              `Other`   = c("D3","LocusZoom","Futurama","UCSCGB","Tron Legacy","Star Trek"))))
                        ),
                        conditionalPanel(
                          condition = "input.IN_color_type == 'Pic-Style'",
                          column(width=4,fileInput("Pic_Name","Picture:",accept=c('image/png',
                                                                                  'image/jpeg',
                                                                                  'image/jpg')))),
                        conditionalPanel(
                          condition = "input.IN_color_type == 'SCI-Style' | input.IN_color_type == 'Pic-Style'",
                          column(width=4,uiOutput("Sci_Pic_UI"))
                        )),
                 column(width=12,
                        column(width=4,selectInput("IN_Nshape","Nodes Shape:",c("Circle","Square","Triangle","Rhombus"))),
                        column(width=4,sliderInput("IN_Nsize","Nodes Size:",min=1,max=25,value=12,step=1)),
                        column(width=4,sliderInput("IN_Tsize","Text size:",min=1,max=10,value=5,step=0.5)))
               ),
               column(width=12,h4("Change individual nodes:")),
               column(width=6,uiOutput("N")),
               column(width=6,selectInput("Nodes_type","Select the Type:",
                                          c("Node Color","Node Shape","Node Size","Text Size"))),
               conditionalPanel(
                 condition = "input.Nodes_type == 'Node Color'",
                 conditionalPanel(
                   condition = "input.inType == 'Render Continue...'",
                   column(width=4,radioButtons("IN_color_type2","Nodes Color:",c("Self-defined","SCI-Style","Pic-Style"))),
                   conditionalPanel(
                     condition = "input.IN_color_type2 == 'SCI-Style'",
                     column(width=4,selectInput("SCI_Name2","Style:",list(`Journal` = c("NPG","Lancet","JAMA","NEJM","JCO","AAAS"),
                                                                          `Technology` = c("Google","Twitter","Facebook","Airbnb","Etsy","23andme"),
                                                                          `Other`   = c("D3","LocusZoom","Futurama","UCSCGB","Tron Legacy","Star Trek"))))
                   ),
                   conditionalPanel(
                     condition = "input.IN_color_type2 == 'Pic-Style'",
                     column(width=4,fileInput("Pic_Name2","Picture:",accept=c('image/png',
                                                                              'image/jpeg',
                                                                              'image/jpg'))))
                 ),
                 column(width=4,uiOutput("N_colorlist")),
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
                 column(width=6,sliderInput("N_Nsize","Node Size:",min=1,max=25,value=12,step=1))
               ),
               conditionalPanel(
                 condition = "input.Nodes_type == 'Text Size'",
                 column(width=6,sliderInput("N_Tsize","Text Size:",min=1,max=10,value=5,step=1))
               ),
               column(width=12,
                      column(width=4,actionButton("AddButtonNodes", "Add",icon=icon("plus"),lib="glyphicon")),
                      column(width=4,actionButton("DelButtonNodes", "Delete",icon=icon("trash"),lib="glyphicon")),
                      column(width=4,actionButton("ClearNodes", "Clear",icon=icon("refresh"),lib="glyphicon"))),
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
                 ),
                 column(width=4,sliderInput("NLegend_TitleSize","Title Size:",min=8,max=16,value=12,step=0.5)),
                 column(width=4,sliderInput("NLegend_TextSize","Text Size:",min=8,max=16,value=10,step=0.5)),
                 column(width=4,sliderInput("NLegend_KeySize","Nodes Key Size:",min=0.5,max=5,value=1.5,step=0.5))
               )
      ),
      # Edges Panel
      tabPanel("Edges",
               conditionalPanel(
                 condition = "input.inType != 'Render Continue...'",
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
                      column(width=4,actionButton("AddButtonEdges", "Add",icon=icon("plus"),lib="glyphicon")),
                      column(width=4,actionButton("DelButtonEdges", "Delete",icon=icon("trash"),lib="glyphicon")),
                      column(width=4,actionButton("ClearEdges", "Clear",icon=icon("refresh"),lib="glyphicon"))),
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
                   ),
                   column(width=4,sliderInput("ELegend_TitleSize","Title Size:",min=8,max=16,value=12,step=0.5)),
                   column(width=4,sliderInput("ELegend_TextSize","Text Size:",min=8,max=16,value=10,step=0.5))
                 ),
                 column(width=4,sliderInput("ELegend_LKeySize","Edges Key Size:",min=0.5,max=5,value=0.5,step=0.5))
               )
      )
    )
  )
))
