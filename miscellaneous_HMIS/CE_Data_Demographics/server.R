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

function(input, output, session) {

  uploaded_data <- reactive({
    if (is.null(input$assessment_data)) {
      return ()
    }
    if (grepl("csv", tolower(input$assessment_data$name))) {
      data <- read.csv(input$assessment_data$datapath)
    } else {
      data <- readxl::read_excel(input$assessment_data$datapath)
    }
    remove_empty(data)
  })
  
  output$data_display <- renderDataTable({
    if (is.null(input$assessment_data)) {
      return ()
    }
    
    DT::datatable(
      uploaded_data(),
      options = list(
        pageLength = 50),
      selection = "single",
      rownames = FALSE
    )
  })
  
  output$demographic_split <- renderUI({
    if (is.null(input$assessment_data)) {
      return ()
    }
    choices <- colnames(select_if(uploaded_data(), negate(is.numeric)))
    radioButtons("demographic_split",
                       "What groups would you like to compare?",
                       choices = choices,
                       selected = choices[1])
  })
  
  output$analysis_selections <- renderUI({
    if (is.null(input$assessment_data)) {
      return ()
    }
    choices <- colnames(select_if(uploaded_data(), is.numeric))
    checkboxGroupInput("factor_checkboxes", 
                       "Which factors would you like to analyze?", 
                       choices = choices,
                       selected = choices)
  })
  
  small_dataframe <- reactive({
    if (is.null(input$assessment_data)) {
      return ()
    }
    uploaded_data() %>%
      select(c(all_of(input$demographic_split),
               all_of(input$factor_checkboxes)))
  })
  
  calculated_table <- reactive({
    if (is.null(input$assessment_data)) {
      return ()
    }
    
    first_factor <- input$factor_checkboxes[1]
    demographic <- input$demographic_split
    
    for (factor in input$factor_checkboxes) {
      hold <- small_dataframe() %>%
        mutate(factor_of_interest := get(factor),
               demographic := get(demographic)) %>%
        filter(!is.na(factor_of_interest))
      
      res.aov <- aov(factor_of_interest ~ demographic,
                     data = hold)
      
      significance <- as.data.frame(TukeyHSD(res.aov)[[1]]) %>%
        mutate(Demographic = rownames(.)) %>%
        select(Demographic, `p adj`) %>%
        rename(!!quo_name(factor) := `p adj`)
      
      if (factor != first_factor) {
        all_significances <- all_significances %>%
          left_join(significance, by = "Demographic")
      } else {
        all_significances <- significance
      }
    }
    all_significances
  })
  
  output$data <- renderDataTable({
    if (is.null(input$assessment_data)) {
      return ()
    }
    
    DT::datatable(
      calculated_table(),
      selection = "single",
      rownames = FALSE,
      options = list(pageLength = 50)) %>%
      formatRound(input$factor_checkboxes, 2) %>%
      formatStyle(
        input$factor_checkboxes, 
        backgroundColor = styleInterval(c(.05, .2), 
                                        c('#adc9b1', '#f0f0cc', 'white'))
      )
  })

  
}
