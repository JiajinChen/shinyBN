render_page <- tabItem(
  tabName = "render",
  br(),
  br(),
  
  jqui_resizable(visNetworkOutput("outVis",height = "750",width = "100%")),
  fixedPanel(top = 57, right=0, width=380,height=15,draggable = F,
             column(width=6,h5()),
             column(width=4,selectInput("inLayout",NULL,c("Layer"="layout_with_sugiyama","Circle"="layout_in_circle",
                                                          "Star"="layout_as_star","Tree"="layout_as_tree","Grid"="layout_on_grid"))),
             column(width=1,actionLink("shinyBN_choose",shiny::img(src="Network_download.png",height=30,width=30)))
  ),
  fixedPanel(top = 95,right=0,width=160,draggable = F,
             uiOutput("Choose_download")),
  column(width=12,
         actionButton("ClearLogRender","Clear log!",icon=icon("trash"),lib="glyphicon"),
         verbatimTextOutput("Render_ERROR"))
)