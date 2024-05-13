# Copyright (C) 2023 Gwen Beebe
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published
# by the Free Software Foundation, either version 3 of the License, or
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU Affero General Public License for more details at
# <https://www.gnu.org/licenses/>. 

library(Hmisc)
library(sysfonts)
library(showtext)
library(hrbrthemes)
library(bslib)
library(shiny)
library(shinydashboard)
library(tidyverse)
library(readxl)
library(DT)
library(shinyWidgets)
library(shinythemes)
library(janitor)
library(ggpubr)
library(ggplot2)
library(dashboardthemes)
# require(openxlsx)

options(shiny.maxRequestSize = 30*1024^2)

ICF_theme <- bslib::bs_theme(
  bg = "#ffffff", fg = "#000000", primary = "#0785F2",
  secondary = "#031D40", success = "#30F298", info = "#5BCBF5",
  warning = "#FFC628", danger = "#414042", base_font = font_google("DM Sans"),
  code_font = font_google("DM Mono"), heading_font = "DM Sans Black",
  `enable-shadows` = TRUE
  , preset = "spacelab"
)

ICF_TitlePanel <- function (title, windowTitle = title, color = "#031D40") {
  css <- paste(paste0("background-color:", color),
               "color: white",
               # "margin-left: 15px",
               "margin-top: -15px",
               "margin-left: -30px",
               "margin-right: -12px",
               "border-radius: 25px",
               "padding-top: 17px",
               "padding-left: 25px",
               "padding-bottom: 5px",
               sep = ";")
  tagList(tags$head(tags$title(windowTitle)), 
          h1(title, style = css))
}

sysfonts::font_add_google(name = "DM Sans")
showtext::showtext_auto()