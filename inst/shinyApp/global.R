require(shiny)
require(shinydashboard)
require(devtools)
require(dashboardthemes)
require(DT)
require(bnlearn)
require(gRain)
require(Rgraphviz)
require(ggplot2)
require(reshape2)
require(AnnotationDbi)
require(svgPanZoom)
require(gridSVG)
require(svglite)
require(SVGAnnotation)
require(shinyjqui)
require(igraph)
require(sqldf)
require(xlsx)

data(asia,package = "bnlearn")
names(asia) <- c("Asia","smoke","tub","lung","bronc","either","xray","dysp")
Asia_data <- asia
dag = model2network("[Asia][smoke][tub|Asia][lung|smoke][bronc|smoke][dysp|bronc:either][either|tub:lung][xray|either]")
Asia_fit <- bn.fit(dag,Asia_data)

Evid_tab<-data.frame(Evidence=character(),Value=character(),stringsAsFactors=FALSE)
Pri_tab<-data.frame(From=character(),To=character(),Type=character(),stringsAsFactors=FALSE)
Query_tab<-data.frame(Query=character(),Value = character(),stringsAsFactors=FALSE)
Ncolorsize_tab <- data.frame()
Ecolorsize_tab <- data.frame()
Nlegend_tab <- data.frame()
Elegend_tab <- data.frame()
result <- data.frame()

n_AE <- 0
n_DE <- 0
n_AQ <- 0
n_DQ <- 0
n_AP <- 0
n_DP <- 0
n_ANode  <- 0
n_DNode  <- 0
n_AEdge  <- 0
n_DEdge  <- 0
n_ClearN <- 0
n_ClearE <- 0
n_NetDownload <- 0
