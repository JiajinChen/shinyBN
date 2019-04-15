
shinyUI(
  dashboardPagePlus(
    header = dashboardHeaderPlus(
      fixed = TRUE,
      enable_rightsidebar = TRUE,
      rightSidebarIcon = "bars",
      titleWidth = 200,
      title = tagList(
        span(class = "logo-lg", img(src = "Logo.png",height=40,width=60)), 
        img(src = "Logo1.png",height=30,width=30))
    ),
    sidebar = dashboardSidebar(
      width=200,
      sidebarMenu(
        menuItem(
          text = "Introduction", 
          tabName = "introduction",
          icon = icon("home",lib="glyphicon")
        ),
        menuItem(
          text = "Network input", 
          tabName = "input",
          icon = icon("folder-open")
        ),
        menuItem(
          text = "Render Graph", 
          tabName = "render",
          icon = icon("project-diagram")
        ),
        menuItem(
          text = "Inference", 
          tabName = "inference",
          icon = icon("stats",lib="glyphicon")
        ),
        br(),
        br(),
        menuItem(
          text = "Help & Source code", 
          href = "https://github.com/JiajinChen/shinyBN",
          icon = icon("question-circle")
        )
      )
    ),
    rightsidebar = rightSidebar(
      background = "light",
      width = 355,
      rightSidebarTabContent(
        id = 1,
        title = "Nodes:",
        icon = "circle",
        active = T,
        conditionalPanel(
          condition = "input.inType == 'Raw Data(.csv)' | input.inType == 'R Object(.Rdata)' | (input.inType == 'R Object in R' & input.inFit != 'Stroke_bnfit')",
          h4("Initial parameters for all nodes:"),
          column(width=12,
                 column(width=6,radioButtons("IN_color_type","Nodes Color:",c("Self-defined","SCI-Style","Pic-Style"))),
                 conditionalPanel(
                   condition = "input.IN_color_type == 'Self-defined'",
                   column(width=6,selectInput("IN_color","Nodes Color:",c("lightblue","red","orange","yellow","green","Other"))),
                   conditionalPanel(
                     condition = "input.IN_color == 'Other'",
                     column(width=6,textInput("IN_Other_color","Input Node Color:",""))
                   )),
                 conditionalPanel(
                   condition = "input.IN_color_type == 'SCI-Style'",
                   column(width=5,selectInput("SCI_Name","Style:",list(`Journal` = c("NPG","Lancet","JAMA","NEJM","JCO","AAAS"),
                                                                       `Technology` = c("Google","Twitter","Facebook","Airbnb","Etsy","23andme"),
                                                                       `Other`   = c("D3","LocusZoom","Futurama","UCSCGB","Tron Legacy","Star Trek"))))
                 ),
                 conditionalPanel(
                   condition = "input.IN_color_type == 'Pic-Style'",
                   column(width=5,fileInput("Pic_Name","Upload picture",accept=c('image/png',
                                                                           'image/jpeg',
                                                                           'image/jpg',
                                                                           'image/svg')))),
                 conditionalPanel(
                   condition = "input.IN_color_type == 'SCI-Style' | input.IN_color_type == 'Pic-Style'",
                   column(width=6,uiOutput("Sci_Pic_UI"))
                 )),
          column(width=12,
                 column(width=6,selectInput("IN_Nshape","Nodes Shape:",c("ellipse","circle","database","box"))),
                 column(width=6,selectInput("IN_Tcolor","Label Color:",c("black","white","red","gray"))),
                 column(width=12,sliderInput("IN_Tsize","Label Size:",min=5,max=25,value=16,step=1)))
        ),
        h4("Change partial nodes:"),
        column(width=12,
               column(width=12,uiOutput("In_Nodetype")),
               conditionalPanel(
                 condition = "input.N_Intype == 'Click graph'",
                 column(width=12,verbatimTextOutput("shiny_return"))
               ),
               conditionalPanel(
                 condition = "input.N_Intype == 'Group in Excel'",
                 column(width=6,uiOutput("Node_Group"))
               ),
               conditionalPanel(
                 condition = "input.N_Intype == 'List'",
                 column(width=6,uiOutput("N")),
                 conditionalPanel(
                   condition = "input.inNodes == 'Markov blanket of:'",
                   column(width=6,uiOutput("N_mb"))
                 )
               ),
               column(width=6,selectInput("Nodes_type","Select the Type:",
                                          c("Node Color","Node Shape","Label Color","Label Size"))),
               conditionalPanel(
                 condition = "input.Nodes_type == 'Node Color'",
                 conditionalPanel(
                   condition = "input.inType == 'Structure in Excel'",
                   column(width=6,radioButtons("IN_color_type2","Nodes Color:",c("Self-defined","SCI-Style","Pic-Style"))),
                   conditionalPanel(
                     condition = "input.IN_color_type2 == 'SCI-Style'",
                     column(width=6,selectInput("SCI_Name2","Style:",list(`Journal` = c("NPG","Lancet","JAMA","NEJM","JCO","AAAS"),
                                                                          `Technology` = c("Google","Twitter","Facebook","Airbnb","Etsy","23andme"),
                                                                          `Other`   = c("D3","LocusZoom","Futurama","UCSCGB","Tron Legacy","Star Trek"))))
                   ),
                   conditionalPanel(
                     condition = "input.IN_color_type2 == 'Pic-Style'",
                     column(width=6,fileInput("Pic_Name2","Picture:",accept=c('image/png',
                                                                              'image/jpeg',
                                                                              'image/jpg',
                                                                              'image/svg'))))
                 ),
                 column(width=6,uiOutput("N_colorlist")),
                 conditionalPanel(
                   condition = "input.N_color == 'Other'",
                   column(width=6,textInput("N_Other_color","Input Node color:",""))
                 ))),
        column(width=12,
               conditionalPanel(
                 condition = "input.Nodes_type == 'Node Shape'",
                 column(width=6,selectInput("N_Nshape","Node Shape:",c("ellipse","circle","database","box")))
               ),
               conditionalPanel(
                 condition = "input.Nodes_type == 'Label Size'",
                 column(width=6,sliderInput("N_Tsize","Label Size:",min=5,max=25,value=14,step=1))
               ),
               conditionalPanel(
                 condition = "input.Nodes_type == 'Label Color'",
                 column(width=6,selectInput("N_Tcolor","Label Color:",c("black","white","red","gray")))
               )),
        column(width=12,
               column(width=4,actionButton("AddButtonNodes", "Add",icon=icon("plus"),lib="glyphicon")),
               column(width=4,actionButton("DelButtonNodes", "Delete",icon=icon("trash"),lib="glyphicon")),
               column(width=4,actionButton("ClearNodes", "Clear",icon=icon("refresh"),lib="glyphicon"))),
        br(),
        column(width=12,dataTableOutput("Ncolorsize_table")),
        h4("Legend:"),
        column(width=6,checkboxInput("N_TF","Add Node Legend?",FALSE)),
        conditionalPanel(
          condition = "input.N_TF == true",
          column(width=12,
                 column(width=6,uiOutput("Nleg_color")),
                 column(width=6,uiOutput("Nleg_shape")),
                 column(width=6,textInput("NLegend_label","Label:",""))),
          column(width=12,
                 column(width=6,actionButton("AddButtonNL", "Add",icon=icon("plus"),lib="glyphicon")),
                 column(width=6,actionButton("delButtonNL", "Delete",icon=icon("trash"),lib="glyphicon"))),
          column(width=12,br()),
          column(width=12,
                 dataTableOutput("N_legend_tab")),
          column(width=12,br()),
          column(width=12,
                 column(width=6,sliderInput("NLegend_KeySize","Key Size:",min=5,max=15,value=10,step=1)),
                 column(width=6,selectInput("NLegend_posion","Select position:",c("right","left"))))
        )
      ),
      rightSidebarTabContent(
        id = 2,
        title = "Edges:",
        icon = "long-arrow-alt-right",
        conditionalPanel(
          condition = "input.inType == 'Raw Data(.csv)' | input.inType == 'R Object(.Rdata)' | (input.inType == 'R Object in R' & input.inFit != 'Stroke_bnfit')",
          h4("Initial parameters for all edges:"),
          column(width=12,
                 column(width=6,selectInput("IE_color","Edges Color:",c("gray","red","orange","yellow","green","Other"))),
                 conditionalPanel(
                   condition = "input.IE_color == 'Other'",
                   column(width=6,textInput("IE_Other_color","Input Edges color:",""))
                 ),
                 column(width=6,selectInput("IE_type","Edges Type:",c("solid","dashed"))),
                 column(width=6,radioButtons("IE_size_type","Edges Width:",c("Self-defined","Arc Strength"))),
                 conditionalPanel(
                   condition = "input.IE_size_type == 'Self-defined'",
                   column(width=6,sliderInput("IE_size","Edges Width:",min=0,max=5,step=0.2,value=1))),
                 conditionalPanel(
                   condition = "input.IE_size_type == 'Arc Strength'",
                   column(width=6,radioButtons("IE_Criterion","Criterion:",c("Independence Test","Score Function"))),
                   conditionalPanel(
                     condition = "input.IE_Criterion == 'Independence Test'",
                     column(width=12,selectInput("IE_Independence","Tests:",list(`Mutual information` = c("Asymptotic chi-square test"="mi", "Adjusted degrees of freedom"="mi-adf",  "Monte Carlo permutation test"="mc-mi",
                                                                                                          "sequential Monte Carlo permutation test"="smc-mi","Semiparametric test"="sp-mi"),
                                                                                 `Shrinkage estimator for the mutual information` = c("Shrinkage estimator for the mutual information"="mi-sh"),
                                                                                 `Pearson's X^2` = c("Asymptotic chi-square test"="x2", "Adjusted degrees of freedom"="x2-adf",  "Monte Carlo permutation test"="mc-x2",
                                                                                                     "sequential Monte Carlo permutation test"="smc-x2","Semiparametric test"="sp-x2"),
                                                                                 `Jonckheere-Terpstra` = c("Asymptotic normal test"="jt","Monte Carlo permutation test"="mc-jt","sequential Monte Carlo permutation test"="smc-jt"))))
                   )),
            conditionalPanel(
              condition = "input.IE_Criterion == 'Score Function'",
              column(width=12,selectInput("IE_Score","Scores:",c("Bayesian Information Criterion score"="bic","Akaike Information Criterion score"="aic","Multinomial log-likelihood score"="loglik",
                                                                "Bayesian Dirichlet equivalent score"="bde","Bayesian Dirichlet sparse score"="bds","Modified Bayesian Dirichlet equivalent score"="mbde",
                                                                "Locally averaged Bayesian Dirichlet score"="bdla","K2 score"="k2")))
            )
          )
        ),
        h4("Change partial edges:"),
        column(width=12,
               conditionalPanel(
                 condition = "input.inType == 'Structure in Excel'",
                 column(width=12,radioButtons ("E_Intype","Select nodes by:",c("Group in Excel","List"),inline = T))
               ),
               conditionalPanel(
                 condition = "input.E_Intype == 'List'  | input.inType != 'Structure in Excel'",
                 column(width=12,uiOutput("E"))
               ),
               conditionalPanel(
                 condition = "input.E_Intype == 'Group in Excel'",
                 column(width=12,uiOutput("Edge_Group"))
               ),
               column(width=6,uiOutput("E_Render")),
               conditionalPanel(
                 condition = "input.Edges_type == 'Edge Color'",
                 column(width=6,selectInput("E_color","Edge Color:",c("gray","red","orange","yellow","green","Other"))),
                 conditionalPanel(
                   condition = "input.E_color == 'Other'",
                   column(width=6,textInput("E_Other_color","Input Edge color:",""))
                 )),
               conditionalPanel(
                 condition = "input.Edges_type == 'Edge Type'",
                 column(width=6,selectInput("E_type","Edge Type:",c("solid","dashed")))
               ),
               conditionalPanel(
                 condition = "input.Edges_type == 'Edge Width'",
                 column(width=6,sliderInput("E_size","Edge Width:",min=0,max=5,step=0.5,value=1))
               )),
        column(width=12,
               column(width=4,actionButton("AddButtonEdges", "Add",icon=icon("plus"),lib="glyphicon")),
               column(width=4,actionButton("DelButtonEdges", "Delete",icon=icon("trash"),lib="glyphicon")),
               column(width=4,actionButton("ClearEdges", "Clear",icon=icon("refresh"),lib="glyphicon"))),
        br(),
        column(width=12,dataTableOutput("Ecolorsize_table")),
        
        h4("Legend:"),
        column(width=6,checkboxInput("E_TF","Add Edge Legend?",FALSE)),
        conditionalPanel(
          condition = "input.E_TF == true",
          column(width=12,
                 column(width=6,uiOutput("Eleg_color")),
                 column(width=6,uiOutput("Eleg_dashed")),
                 column(width=6,textInput("ELegend_label","Label:",""))),
          column(width=12,
                 column(width=6,actionButton("AddButtonEL", "Add",icon=icon("plus"),lib="glyphicon")),
                 column(width=6,actionButton("delButtonEL", "Delete",icon=icon("trash"),lib="glyphicon"))),
          column(width=12,br()),
          column(width=12,
                 dataTableOutput("E_legend_tab")),
          column(width=12,br()),
          column(width=12,
                 column(width=6,sliderInput("ELegend_KeySize","Key Size:",min=0.5,max=5,value=1.5,step=0.5)),
                 conditionalPanel(
                   condition = "input.N_TF == false",
                   column(width=6,selectInput("ELegend_posion","Select position:",c("right","left")))
                 )))
      ),
      rightSidebarTabContent(
        id = 3,
        icon = "chart-bar",
        title = "Inference:",
        radioButtons("Infer_type",NULL,c("Single Prediction","Validation Set"),inline=T),
        conditionalPanel(
          condition = "input.Infer_type == 'Single Prediction'",
          radioButtons("Q_Intype","Select nodes by:",c("List","Click graph"),inline = T),
          conditionalPanel(
            condition = "input.Q_Intype == 'Click graph'",
            verbatimTextOutput("Q_return")
          ),
          conditionalPanel(
            condition = "input.Q_Intype == 'List'",
            uiOutput("evidence")
          ),
          uiOutput("E_value"),
          fluidRow(
            column(width=4,actionButton("AddButtonE", "Add",icon=icon("plus"),lib="glyphicon")),
            column(width=4,actionButton("delButtonE", "Delete",icon=icon("trash"),lib="glyphicon")),
            column(width=4,actionButton("ClearButtonE", "Clear",icon=icon("refresh"),lib="glyphicon"))
          ),
          column(width=12,br("")),
          uiOutput("query"),
          conditionalPanel(
            condition = "input.Type == 'marginal'",
            uiOutput("Q_value")
          ),
          fluidRow(
            column(width=4,actionButton("AddButtonQ", "Add",icon=icon("plus"),lib="glyphicon")),
            column(width=4,actionButton("delButtonQ", "Delete",icon=icon("trash"),lib="glyphicon")),
            column(width=4,actionButton("ClearButtonQ", "Clear",icon=icon("refresh"),lib="glyphicon"))
          ),
          column(width=12,br("")),
          radioButtons("Type","Choose the type:",c("Marginal" = "marginal","Joint" = "joint"),inline=T)
        ),
        conditionalPanel(
          condition = "input.Infer_type == 'Validation Set'",
          conditionalPanel(
            condition = "input.inType == 'Raw Data(.csv)' & input.YNsplit == 'yes'",
            radioButtons("Valid_Sample","Using:",c("Upload dataset","Split Sample"),inline=T)
          ),
          conditionalPanel(
            condition = "input.inType != 'Raw Data(.csv)' | (input.inType == 'Raw Data(.csv)' & input.YNsplit == 'yes' & input.Valid_Sample == 'Upload dataset') | (input.inType == 'Raw Data(.csv)' & input.YNsplit == 'no')",
            fileInput("ValidSet","Upload validation Set:(csv)"),
            column(width=3,checkboxInput("ValidHeader","Header?",TRUE)),
            column(width=9,radioButtons("ValidSep","Separator:",c("Comma"=",","Tabs"="\t","Spaces"=" "),inline = T)),
            helpText("WARNING: The variable should have the same names and values with Network.")
          ),
          uiOutput("ValidVarUI"),   #input$ValidVar
          uiOutput("ValidValueUI"), #input$ValidValue
          sliderInput("Case_Prob","Threshold of Probability to ",0,1,0.5,step=0.01),
          radioButtons("ROCorDCA","Plot ROC or DCA:",c("ROC","DCA"),inline = T)
        )
      )
    ),
    body = dashboardBody(
      tabItems(
        introduction_page,
        input_page,
        render_page,
        inference_page
      )
    )
  ) 
)