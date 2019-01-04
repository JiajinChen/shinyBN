require(shiny)
require(shinydashboard)
require(visNetwork)
require(shinydashboardPlus)
require(DT)
require(bnlearn)
require(gRain)
require(ggplot2)
require(ggsci)
require(rPlotter)
require(reshape2)
require(AnnotationDbi)
require(shinyjqui)
require(igraph)
require(sqldf)
require(xlsx)
require(knitr)

Evid_tab<-data.frame(Evidence=character(),Value=character(),stringsAsFactors=FALSE)
Query_tab<-data.frame(Query=character(),Value = character(),stringsAsFactors=FALSE)
Pri_tab<-data.frame(From=character(),To=character(),Type=character(),stringsAsFactors=FALSE)
Ncolorsize_tab <- data.frame(Nodes=character(),ColorSize=character(),Type=character(),stringsAsFactors = F)
Ecolorsize_tab <- data.frame(Edges=character(),ColorSize=character(),Type=character(),stringsAsFactors = F)
Nlegend_tab <- data.frame(color=character(),shape=character(),label=character())
Elegend_tab <- data.frame(color=character(),linetype=character(),label=character())
result <- data.frame()

data(asia,package = "bnlearn")
names(asia) <- c("Asia","smoke","tub","lung","bronc","either","xray","dysp")
Asia_data <- asia
dag = model2network("[Asia][smoke][tub|Asia][lung|smoke][bronc|smoke][dysp|bronc:either][either|tub:lung][xray|either]")
Asia_fit <- bn.fit(dag,Asia_data)
load("data/Stroke_bn.rdata")

source("introduction.R")
source("input.R")
source("render.R")
source("inference.R")

out_text="Log:\n"
n_AE <- 0
n_DE <- 0
n_AQ <- 0
n_DQ <- 0
n_AP <- 0
n_DP <- 0
n_GP <- 0
white <- NULL
black <- NULL
n_ANode  <- 0
n_DNode  <- 0
n_AEdge  <- 0
n_DEdge  <- 0
n_ClearN <- 0
n_ClearE <- 0
n_ALegeN <- 0
n_DLegeN <- 0
n_ALegeE <- 0
n_DLegeE <- 0
n_ClearEvi <- 0
n_ClearQue <- 0
n_LogClear <- 0
n_NetDownload <- 0