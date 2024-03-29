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

library(shiny)
library(shinydashboard)
library(tidyverse)
library(readxl)
library(DT)
library(shinyWidgets)
library(shinythemes)
library(janitor)
# require(openxlsx)

options(shiny.maxRequestSize = 30*1024^2)