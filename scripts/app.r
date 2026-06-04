library(shiny)
library(shinydashboard)
library(tidyverse)
library(data.table)
library(lubridate)
library(R.utils)
library(fst)
library(ggprism)
library(plotly)


scripts_dir <- if (file.exists("server.r")) "." else "scripts"
source(file.path(scripts_dir, "server.r"))
source(file.path(scripts_dir, "ui.r"))


shinyApp(ui, server)
