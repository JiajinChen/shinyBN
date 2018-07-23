if(! require(shiny)){
  print("Trying to install package shiny")
  install.packages("shiny")
  require(shiny)
}
if(! require(shinydashboard)){
  print("Trying to install package shinydashboard")
  install.packages("shinydashboard")
  require(shinydashboard)
}
if(! require(devtools)){
  print("Trying to install package devtools")
  install.packages("devtools")
  require(devtools)
}
if(! require(dashboardthemes)){
  print("Trying to install package dashboardthemes")
  devtools::install_github("nik01010/dashboardthemes")
  require(dashboardthemes)
}
if(! require(DT)){
  print("Trying to install package DT")
  install.packages("DT")
  require(DT)
}
if(! require(bnlearn)){
  print("Trying to install package bnlearn")
  install.packages("bnlearn")
  require(bnlearn)
}
if(! require(gRain)){
  print("Trying to install package gRain")
  install.packages("gRain")
  require(gRain)
}
if(! require(Rgraphviz)){
  print("Trying to install package Rgraphviz")
  source("https://bioconductor.org/biocLite.R")
  biocLite("Rgraphviz")
  require(Rgraphviz)
}
if(! require(ggplot2)){
  print("Trying to install package ggplot2")
  install.packages("ggplot2")
  require(ggplot2)
}
if(! require(reshape2)){
  print("Trying to install package reshape2")
  install.packages("reshape2")
  require(reshape2)
}
if(! require(AnnotationDbi)){
  print("Trying to install package AnnotationDbi")
  source("https://bioconductor.org/biocLite.R")
  biocLite("AnnotationDbi")
  require(AnnotationDbi)
}
if(! require(svgPanZoom)){
  print("Trying to install package svgPanZoom")
  install.packages("svgPanZoom")
  require(svgPanZoom)
}
if(! require(SVGAnnotation)){
  print("Trying to install package SVGAnnotation")
  devtools::install_github("duncantl/SVGAnnotation")
  require(SVGAnnotation)
}
if(! require(shinyjqui)){
  print("Trying to install package shinyjqui")
  install.packages("shinyjqui")
  require(shinyjqui)
}
if(! require(igraph)){
  print("Trying to install package igraph")
  install.packages("igraph")
  require(igraph)
}
if(! require(sqldf)){
  print("Trying to install package sqldf")
  install.packages("sqldf")
  require(sqldf)
}

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
n_RdN <- 0
n_RdE <- 0
