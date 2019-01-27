# Introduction
___
*shinyBN* is an `R/Shiny` application for interactive construction, inference and visualization of Bayesian Network, which provide friendly GUI for users lacking of programming skills. It's mainly based on five R packages: `bnlearn` for structure learning, parameter training, `gRain` for network inference, and `visNetwork` for network visualization, `pROC` and `rmda` for receiver operating characteristic (ROC) curve and decision curves analysis (DCA) , respectively, which was further wrapped by `Shiny`, a framework to build interactive web application straight by R.

# Get Start
___
***Run APP in R:***

Install dependencies:
```{r,eval = FALSE}
install.packages("devtools")
library(devtools)

# Packages on CRAN
install.packages(c("shiny","shinydashboard","shinydashboardPlus","sqldf","writexl","readxl","reshape2","DT","bnlearn","ggsci","shinyjqui","ggplot2","visNetwork","pROC","rmda","knitr"))

# Packages on Bioconductor
source("http://bioconductor.org/biocLite.R")
biocLite(c("gRain","igraph","AnnotationDbi","EBImage"))

# Packages on Github
install_github(c("ramnathv/rblocks","woobe/rPlotter"))
```

Install *shinyBN* from Github:
```{r,eval = FALSE}
devtools::install_github('JiajinChen/shinyBN')
```

Launch the APP in R:
```{r,eval = FALSE}
shinyBN::run_shinyBN()
```


***Lauch the APP through browser:***

Please visit: [https://jiajin.shinyapps.io/shinyBN/](https://jiajin.shinyapps.io/shinyBN/)

# Main Page
___

<img src="https://github.com/JiajinChen/shinyBN/blob/master/inst/images/Main%20Page.png?raw=true"/>


# How to use
___
### **Step 1: Input your Network!**

Here, we provide Four type of data input:

+ **R Object in R :** If you trigger *shinyBN* in R, you can directly upload your Network exist in R environment.
+ **R Object(.Rdata) :** Upload your Network that save as rdata format.
+ **Raw Data(.csv) :** Upload raw data and perform structure learning, parameter training in *shinyBN*.
+ **Structure in Excel :** Upload a Excel with Network information (see [Example](https://github.com/JiajinChen/shinyBN/blob/master/inst/shinyApp/data/shinyBN.xlsx)).

  <img src="https://github.com/JiajinChen/shinyBN/blob/master/inst/images/Input.png?raw=true"/>
   
### **Step 2: Render your Network!**

Once your BN is inputed, the plot would present automatically with default parameters. If you are not satisfied with your graphic appearance, you can render your plot with corresponding settings. Additionally, network layout and legend can be set flexibly. Finally, *shinyBN* provides high-quality images download in HTML output and Network information in Excel.Because the network plot is based on canvas, it's difficult to get SVG. You can get the high-resolution figure by the following step:

+ **Step1 :** Download the network in HTML from shinyBN.
+ **Step2 :** Use any text editor to open the HTML formatted file and replace the text '888px' to the pixel you want, such as '5000px'. Save the change.
+ **Step3 :** Open the HTML file with your browser and adjust the network to the maximum. Then right click to view the image. Finally, right click to save the image.

  <img src="https://github.com/JiajinChen/shinyBN/blob/master/inst/images/Render.png?raw=true"/>
  
  + An Example:
  
  ![grab-landing-page](https://github.com/JiajinChen/shinyBN/blob/master/inst/GIF/Render.gif?v=9ad8eed7)
  
### **Step 3: Inference!**

One of the major functions of Bayesian network is outcome prediction. You can query the probability of interested nodes given the values of a set of instantiated nodes. *shinyBN* allowed users to set multiple instantiated nodes and both marginal probability and joint probability are supported, the prediction results will be displayed in bar plot or probabilistic table. Users can set different color representing different threshold to distinguish different levels of outcome probability. In addition, you can download the result through a PDF output interface for High-quality images.


  <img src="https://github.com/JiajinChen/shinyBN/blob/master/inst/images/Inference.png?raw=true"/>
  
  + An Example of single prediction:
  
  ![grab-landing-page](https://github.com/JiajinChen/shinyBN/blob/master/inst/GIF/Single%20inference.gif?v=9ad8eed7)

*shinyBN* also allowed user to upload a validation set for batch prediction. If your validation set contains outcome information, you can get the receiver operating characteristic (ROC) curve plot and decision curves analysis (DCA) plot. The same, both the plot in high-resolution images and batch prediction result in tables are supported.

  + An Example of batch prediction:
  
  ![grab-landing-page](https://github.com/JiajinChen/shinyBN/blob/master/inst/GIF/Batch%20inference.gif?v=9ad8eed7)

# Reference
shinyBN: An online application for Bayesian network interactive inference and visualization

# Source code

*shinyBN* is an open source project, and the source code and its manual is freely available at https://github.com/JiajinChen/shinyBN.

# Contact us

If you have any problem or other inquiries you can also email us at ywei@njmu.edu.cn .
