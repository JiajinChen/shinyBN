input_page <- tabItem(
  tabName = "input",
  br(),
  br(),
  
  radioButtons("inType",h4(strong("Select the type of Input:")),c("R Object in R","R Object(.Rdata)","Raw Data(.csv)","Structure in Excel"),inline = T,width="100%"),
  conditionalPanel(
    condition = "input.inType == 'R Object in R'",
    br(),
    column(width=6,textInput("inFit","Enter your Network and Data (if need):","Stroke_bnfit",width=500)),
    column(width=12,helpText("Separated by a comma.")),
    column(width=12,helpText("To change default network, please input 'Stroke_bnfit' (For Stroke Network) or 'Asia_fit,Asia_data' (For Asia Network)."))
  ),
  conditionalPanel(
    condition = "input.inType == 'Raw Data(.csv)'",
    br(),
    h4(strong("Structure learning:")),
    column(width=12,
           column(width=4,fileInput("inFile","Choose your Raw Data(.csv):")),
           column(width=4,checkboxInput("inHeader","Header?",TRUE))),
    column(width=12,
           column(width=4,selectInput("inLearnType","Type of Structure Algorithm:",c("Score-Based Algorithms","Constraint-Based Algorithms",
                                                                                     "Hybrid Algorithms","Bootstrap"))),
           conditionalPanel(
             condition = "input.inLearnType == 'Constraint-Based Algorithms'",
             column(width=4,selectInput("inLearn1","Structure Algorithm:",c("Grow-Shrink","Incremental Association","Fast Incremental Association",
                                                                            "Interleaved Incremental Association")))),
           conditionalPanel(
             condition = "input.inLearnType == 'Score-Based Algorithms'",
             column(width=4,selectInput("inLearn2","Structure Algorithm:",c("hill-climbing","tabu search"))),
             column(width=4,selectInput("inScore2","Scores:",c("Bayesian Information Criterion score"="bic","Akaike Information Criterion score"="aic","Multinomial log-likelihood score"="loglik",
                                                "Bayesian Dirichlet equivalent score"="bde","Bayesian Dirichlet sparse score"="bds","Modified Bayesian Dirichlet equivalent score"="mbde",
                                                "Locally averaged Bayesian Dirichlet score"="bdla","K2 score"="k2")))),
           conditionalPanel(
             condition = "input.inLearnType == 'Hybrid Algorithms'",
             column(width=4,selectInput("inLearn3","Structure Algorithm:",c("Max-Min Hill Climbing","2-phase Restricted Maximization")))),
           conditionalPanel(
             condition = "input.inLearnType == 'Bootstrap'",
             column(width=4,selectInput("inLearn4","Structure Algorithm:",c("hill-climbing"="hc","tabu search"='tabu',"Grow-Shrink"='gs',"Incremental Association"='iamb',
                                                             "Fast Incremental Association"='fast.iamb',"Interleaved Incremental Association"='inter.iamb',
                                                             "Max-Min Hill Climbing"='mmhc',"2-phase Restricted Maximization"='rsmax2'))),
             conditionalPanel(
               condition = "input.inLearn4 =='hc' || input.inLearn4 =='tabu'",
               column(width=4,selectInput("inScore4","Score:",c("Bayesian Information Criterion score"="bic","Akaike Information Criterion score"="aic","Multinomial log-likelihood score"="loglik",
                                                 "Bayesian Dirichlet equivalent score"="bde","Bayesian Dirichlet sparse score"="bds","Modified Bayesian Dirichlet equivalent score"="mbde",
                                                 "Locally averaged Bayesian Dirichlet score"="bdla","K2 score"="k2")))),
             column(width=7,radioButtons("N_Boot","Num of Boot:",c(10,500,1000,2000),inline=T))
           )),
    
    column(width=12,
           column(width=2,checkboxInput("prior_TF", "Prior?", F)),
           column(width=2,checkboxInput("prior_hide", "Hide?", F))),
    conditionalPanel(
      condition = "input.prior_TF",
      column(width=12,
             column(width=6,uiOutput("from")),
             column(width=6,uiOutput("to"))),
      radioButtons("BorW",NULL,c("blacklist","whitelist"),inline = T),
      column(width=4,actionButton("AddButtonP", "Add!",icon=icon("plus"),lib="glyphicon")),
      column(width=4,actionButton("delButtonP", "Delete!",icon=icon("trash"),lib="glyphicon")),
      column(width=4,actionButton("GoButtonP", "Go!",icon=icon("play"),lib="glyphicon"))
    ),
    conditionalPanel(
      condition = "input.inType == 'Raw Data(.csv)' & input.prior_TF &  ! input.prior_hide",
      column(width=12,dataTableOutput("Pri_table"))
    ),
    br(),
    h4(strong("Parameter learning:")),
    column(width=12,selectInput("inMethod","Parameter Algorithm:",c("Maximum Likelihood parameter estimation"="mle",
                                                    "Bayesian parameter estimation"="bayes")))
  ),
  conditionalPanel(
    condition = "input.inType == 'R Object(.Rdata)'",
    br(),
    column(width=12,fileInput("inObject","Choose your R Object(.Rdata):"))
  ),
  conditionalPanel(
    condition = "input.inType == 'Structure in Excel'",
    br(),
    column(width=12,fileInput("inContinue","Choose your Structure File(.xlsx):"))
  ),
  column(width=12,
         br(),
         actionButton("ClearLog","Clear log!",icon=icon("trash"),lib="glyphicon"),
         verbatimTextOutput("input_ERROR"))
)