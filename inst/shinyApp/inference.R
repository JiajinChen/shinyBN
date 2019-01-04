inference_page <- tabItem(
  tabName = "inference",
  br(),
  br(),
  br(),
  br(),
  
  
  column(width=12,
         column(width=6,dataTableOutput("Evi_table")),
         column(width=6,
                jqui_resizable(
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
)