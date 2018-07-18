---
title: "shinyBN"
author: "Jiajin Chen,Yongyue Wei"
date: "2018/05/14"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Introduction
___
shinyBN is a Shiny application for interactive construction, inference and visualization of Bayesian Network, which provide friendly GUI for users lacking of programming skills. It's based on bnlearn (A core R package for construction and inference of Bayesian Network) and Rgraphviz (An R package commonly used for Bayesian network visualization), and wrapped by Shiny, a framework to build interactive web apps straight from R. 

## Get Start
___
To install shinyBN from CRAN:
```{r,eval = FALSE}
install.packages("shinyBN")
```

To install shinyBN from Github for the latest developmental version:
```{r,eval = FALSE}
devtools::install_github('JiajinChen/shnyBN')
```

To launch the APP in R:
```{r,eval = FALSE}
shinyBN:run_shinyBN()
```

To lauch the APP through browser:

Please visit: [https://jiajin.shinyapps.io/shinyBN/](https://jiajin.shinyapps.io/shinyBN/)

## Main Page
___

<img src="https://github.com/JiajinChen/shinyBN/blob/master/inst/images/Main%20Page.png?raw=true"/>


## How to use
___
##### **Step 1: Input your row data/ R object!**

Here, we provide three type of data input:

For web platform users, you can choose a csv-format **discretized** row data or an R object save as Rdata. 

For R platform users, besides that, you can also directly input an R object existing in your R environment.

  <img src="https://github.com/JiajinChen/shinyBN/blob/master/inst/images/Step%201.png?raw=true"/>
   
##### **Step 2: Render your Network!**

Once your BN is inputed, the plot would present automatically with default parameters. If you are not satisfied with your graphic appearance or you want to highlight which nodes or edges. We provide tackles for you to render the plot in your style. We provide nodes color, nodes shape, nodes size, label size for nodes. As for edges, you can choose your favoriate line width, line color, line type. As for line width, we provide two different method: Self-defined and Arc strength, users can render line width corresponding to the strength of the probabilistic relationships. In addition, you're allowed to add legend to index the feature of plot. For instance, you might want to index the baseline nodes and the outcome nodes. High-quality images in PDF output interface also offered by shinyBN.

  + Nodes:
  <img src="https://github.com/JiajinChen/shinyBN/blob/master/inst/images/Node%20Render.png?raw=true"/>
  
  + Edges:
  <img src="https://github.com/JiajinChen/shinyBN/blob/master/inst/images/Edge%20Render.png?raw=true"/>
  
  + An Example:
  ![grab-landing-page](https://github.com/JiajinChen/shinyBN/blob/master/inst/GIF/Render1280.gif?v=9ad8eed7)
  
##### **Step 3: Inference!**
 
