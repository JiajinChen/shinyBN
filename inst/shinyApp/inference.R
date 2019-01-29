inference_page <- tabItem(
  tabName = "inference",
  br(),
  br(),
  br(),
  br(),
  
  conditionalPanel(
    condition = "input.Infer_type == 'Single Prediction'",
    column(width=12,
           column(width=6,dataTableOutput("Evi_table")),
           column(width=6,
                  jqui_resizable(
                    tabBox(side = "left", width = NULL,height = "550px",selected = "Graph",
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
                                      column(width=6,numericInput("RLegend_x","Input the X position:",90,min=0,max = 100)),
                                      column(width=6,numericInput("RLegend_y","Input the Y position:",90,min=0,max = 100)),
                                      column(width=12,helpText("Notes:For x: Number from 0 (left) to 100 (right);  For y: Number from 0 (bottom) to 100 (top)."))
                                    )
                           )
                    )))),
    fixedPanel(top = 57, right=0, width=380,height=15,draggable = F,
               conditionalPanel(
                 condition = "input.inType != 'Structure in Excel'",
                 column(width=4,h5()),
                 column(width=1,h5(strong("W:"),align="center")),
                 column(width=2,textInput("Pwidth",NULL,8)),
                 column(width=1,h5(strong("H:"),align="center")),
                 column(width=2,textInput("Pheight",NULL,8)),
                 column(width=1,downloadLink("Result.pdf",shiny::img(src="Result_download.png",height=30,width=30)))
               )
    )
  ),
  conditionalPanel(
    condition = "input.Infer_type == 'Validation Set'",
    column(width=12,
           column(width=6,verbatimTextOutput("index")),
           # column(width=6,dataTableOutput("Index_table")),
           column(width=6,
                  tabBox(side= "left", width = NULL,height = "550px",selected = "Graph",
                         tabPanel("Graph",
                                  plotOutput("ROCDCA",height = "500px",width = "100%")),
                         tabPanel("Options",
                                  conditionalPanel(
                                    condition = "input.ROCorDCA == 'ROC'",
                                    column(width=2,checkboxInput("ROC_smooth","Smooth?",F)),
                                    column(width=3,textInput("ROC_color","ROC Color:","black")),
                                    column(width=3,selectizeInput("ROC_linetype","ROC line type:",c("solid","dashed","dotted"))),
                                    column(width=4,sliderInput("ROC_lwd","ROC width",0.5,3,value=1,step=0.5)),
                                    column(width=12,checkboxInput("Threshold","Best Cutoff?",T)),
                                    conditionalPanel(
                                      condition = "input.Threshold",
                                      column(width=4,textInput("Threshold_color","Cutoff Color:","black")),
                                      column(width=4,sliderInput("Threshold_size","Cutoff Size:",0.5,3,value=1.5,step=0.5))
                                    ),
                                    column(width=12,checkboxInput("AUC","AUC?",T)),
                                    conditionalPanel(
                                      condition = "input.AUC",
                                      column(width=4,checkboxInput("AUC_CI","CI of AUC?",T)),
                                      column(width=4,textInput("AUC_color","AUC Color:","black")),
                                      column(width=4,sliderInput("AUC_size","AUC Size:",0.5,2,value=1.5,step=0.2))
                                    ),
                                    column(width=12,checkboxInput("polygon","Display the area as a polygon?",F)),
                                    conditionalPanel(
                                      condition = "input.polygon",
                                      column(width=4,textInput("polygon_color","Polygon Color:","lightblue"))
                                    ),
                                    column(width=12,checkboxInput("grid","Add grid line?",F)),
                                    conditionalPanel(
                                      condition = "input.grid",
                                      column(width=4,textInput("grid_color","Grid line Color:","gray")),
                                      column(width=4,selectizeInput("grid_linetype","Grid line type:",c("solid","dashed","dotted"))),
                                      column(width=4,sliderInput("grid_lwd","Grid line width",0.5,2,value=0.5,step=0.5))
                                    )
                                  ),
                                  conditionalPanel(
                                    condition = "input.ROCorDCA == 'DCA'",
                                    column(width=4,checkboxInput("DCA_stand","standardized net benefit?",T)),
                                    column(width=4,textInput("DCA_name","DCA name:","DCA model")),
                                    column(width=4,selectInput("DCA_leg.posi","Legend position:",c("none","topright","topleft","bottomright","bottomleft",
                                                                                                   "top","bottom","right","left"))),
                                    column(width=4,textInput("DCA_col","Line Color:","darkred,black,black")),
                                    column(width=4,textInput("DCA_lty","Line Linetype:","solid,dashed,solid")),
                                    column(width=4,textInput("DCA_lwd","Line Width:","2,2,2")),
                                    helpText("Notes: Separated by a comma. The first element correspond to the curves provided.
                                             The next two elements correspond to the attributes of the 'all' and 'none' curves."),
                                    column(width=12,textInput("DCA_xlab","Label of x-axis:","Threshold probability for treatment")),
                                    column(width=4,textInput("DCA_xlim","Min, Max of x-axis:","0,1")),
                                    column(width=4,textInput("DCA_ylim","Min, Max of y-axis:","0,1"))
                                  )
                         )
                  )
           )
    ),
  fixedPanel(top = 57, right=0, width=380,height=15,draggable = F,
             conditionalPanel(
               condition = "input.inType != 'Structure in Excel'",
               column(width=2,h5()),
               column(width=1,h5(strong("W:"),align="center")),
               column(width=2,textInput("Pwidth",NULL,8)),
               column(width=1,h5(strong("H:"),align="center")),
               column(width=2,textInput("Pheight",NULL,8)),
               conditionalPanel(
                 condition = "input.ROCorDCA == 'ROC'",
                 column(width=1,downloadLink("ROC_download.pdf",shiny::img(src="ROC_download.png",height=30,width=30)))
               ),
               conditionalPanel(
                 condition = "input.ROCorDCA == 'DCA'",
                 column(width=1,downloadLink("DCA_download.pdf",shiny::img(src="DCA_download.png",height=30,width=30)))
               ),
               column(width=1,h5()),
               column(width=1,downloadLink("Validation.csv",shiny::img(src="Valid_download.png",height=30,width=30)))
             )
  )
  ),
  column(width=12,
         actionButton("ClearLogv","Clear log!",icon=icon("trash"),lib="glyphicon"),
         verbatimTextOutput("valid_ERROR"))
)
