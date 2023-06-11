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

fluidPage(
  theme = shinytheme("paper"),
  fluidRow(
    box(
      solidHeader = TRUE,
      title = "Upload Assessment Data",
      fileInput("assessment_data", "Click below to upload assessment data. This should include the demographics you are using in your analysis and the numeric columns you are interested in. \nCommonly used numeric columns include points for specific questions, scores on assessment sub-sections, or even overall scores.",
                accept = c(".csv", ".xls", ".xlsx")),
      status = "warning",
      width = 12
    )
  ),
  fluidRow(
    box(uiOutput("demographic_split"), width = 6),
    box(uiOutput("analysis_selections"), width = 6)
  ),
  fluidRow(
    dataTableOutput("data")
  ),
  fluidRow(
    # infoBoxOutput("file_people", width = 6),
    # infoBoxOutput("file_arrests", width = 6)
  )
)


# show what the disparities are
# hide columns that are all null
# swap order to show confidence interval instead of p-value