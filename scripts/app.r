library(shiny)
library(shinydashboard)
library(tidyverse)
library(data.table)
library(lubridate)
library(R.utils)
library(fst)
library(ggprism)
library(plotly)


source("../scripts/server.r")
source("../scripts/ui.r")


shinyApp(ui, server)

