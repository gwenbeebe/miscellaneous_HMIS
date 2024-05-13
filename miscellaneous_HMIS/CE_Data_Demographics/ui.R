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



dashboard_body <- fluidPage(
  theme = ICF_theme,
  ICF_TitlePanel("Simple Equity Evaluator"),
  fluidRow(
    box(
      solidHeader = TRUE,
      title = "Upload Assessment Data",
      "Click below to upload assessment data. This should include the demographics you are using in your analysis and the numeric columns you are interested in. \nCommonly used numeric columns include points for specific questions, scores on assessment sub-sections, or even overall scores.",
      status = "warning",
      width = 12
    )
  ),  fluidRow(
    box(
      fileInput("assessment_data", " ",
                accept = c(".csv", ".xls", ".xlsx")),
      width = 12
    )
  ),
  fluidRow(
    # box(uiOutput("demographic_split"), width = 6),
    # box(uiOutput("target_group"), width = 6),
    # box(uiOutput("analysis_selections"), width = 6)
    box(
      status = "warning",
      "After data is uploaded, population averages are displayed in the table below. The bottom row contains the p-value for a statistical test (Tukey's honest significant difference, if you're curious) comparing these values. If the number in the bottom row is highlighted green, the difference in means is statistically significant (there is at most a 5% chance that the difference is coincidence). If the number in the bottom row is highlighted yellow, it means that there is a 5-20% chance that the difference is coincidence.",
      width = 12)
),
  fluidRow(
    dataTableOutput("averages")
  ),
  fluidRow(
    box(
      status = "warning",
      "After data is uploaded, variable distributions are shown in the violin charts below. These show the distribution for each group to help visualize the difference in responses. Each distribution is relevant only to itself; comparisons between variables should be understood only in terms of distribution across the possible range for each variable. The white dot indicates the median of the distribution, and the white lines extend from the 25th percentile to the 75th percentile. In other words, approximately 50% of all rows fall within the range of the white line.",
      width = 12)
  ),
  fluidRow(
    plotOutput("distribution_plot")
  ),
  # ),
  fluidRow(
  )
)

card(
  full_screen = TRUE,
  layout_sidebar(
    sidebar = sidebar(
      position = "right",
      uiOutput("demographic_split"),
      uiOutput("target_group"),
      uiOutput("analysis_selections") #alphabetize
    ),
    dashboard_body
  )
)

# show what the disparities are
# hide columns that are all null
# swap order to show confidence interval instead of p-value