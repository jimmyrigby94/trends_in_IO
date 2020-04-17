if(!require(shiny)){
  install.packages("shiny")
}

if(!require(tidyverse)){
  install.packages('tidyverse')
}
if(!require(tidytext)){
  install.packages("tidytext")
}
if(!require(plotly)){
  install.packages("plotly")
}
if(!require(knitr)){
  install.packages("knitr")
}

if(!require(shinydashboard)){
  install.packages('shinydashboard')
}

if(!require(DT)){
  install.packages('DT')
}

if(!require(rintrojs)){
  install.packages("rintrojs")
}

if(!require(shinycssloaders)){
  install.packages("shinycssloaders")
}

library(shiny)
library(tidyverse)
library(tidytext)
library(plotly)
library(knitr)
library(shinydashboard)
library(rintrojs)
library(shinycssloaders)