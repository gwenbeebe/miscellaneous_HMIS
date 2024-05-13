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
      data <- as.data.frame(readxl::read_excel(input$assessment_data$datapath))
    }
    data <- data %>%
      remove_empty(.,
                 which = c("rows", "cols")) %>%
      mutate(across(where(is.character), ~ replace_na(., "null")))
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
    selected_option <- ifelse(!is.na(which(str_detect(tolower(choices), "race"))[1]),
                              choices[which(str_detect(tolower(choices), "race"))[1]],
                              choices[1])
    
    radioButtons("demographic_split",
                       "What group type would you like to compare?",
                       choices = choices,
                       selected = selected_option)
  })
  
  possible_targets <- reactive({
    if (is.null(input$assessment_data)) {
      return ()
    }
    as.vector(unique(
      uploaded_data()[, which(colnames(uploaded_data()) == input$demographic_split)]))
  })
  
  output$target_group <- renderUI({
    if (is.null(input$demographic_split)) {
      return ()
    }
    choices <- possible_targets()
    names(choices) <- possible_targets()
    
    checkboxGroupInput("target_group",
                 "Please select your target group/s.",
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
    
    group_label <- if(length(input$target_group) == 1) 
      {input$target_group} else { "Selected Groups"}
    
    uploaded_data() %>%
      mutate(Group = factor(
        if_else(
          get(input$demographic_split) %in% input$target_group,
          group_label, "Other"),
        levels = c(group_label, "Other"))) %>%
      select(Group,
             all_of(input$factor_checkboxes)) %>% 
      select(order(colnames(.))) %>%
      relocate(Group)
  })
  
  output$averages <- renderDataTable({
    if (is.null(input$target_group)) {
      return ()
    }
    
    DT::datatable(
      small_dataframe() %>%
        group_by(Group) %>%
        summarise_all(mean, na.rm = TRUE) %>% 
        map_df(rev) %>%
        arrange(Group) %>%
        union(calculated_table()  %>% 
                mutate(Group = "P Value") %>%
                select(order(colnames(.))) %>%
                relocate(Group)) %>%
        select(c(Group, input$factor_checkboxes)),
      selection = "single",
      rownames = FALSE,
      options = list(
                     dom = 't',
                     ordering = FALSE)) %>%
      formatRound(input$factor_checkboxes, 2) %>%
      formatStyle(
        input$factor_checkboxes,
        backgroundColor = styleInterval(c(.05, .2),
                                        c('#c0fbe0', '#ffedbe', 'white'))
      ) %>% 
      formatStyle(colnames(small_dataframe()), target = "cell", 
                  backgroundColor = styleRow(1:2, 'white')) 
  })
  
  calculated_table <- reactive({
    if (is.null(input$assessment_data)) {
      return ()
    }
    
    first_factor <- input$factor_checkboxes[1]
    
    for (factor in input$factor_checkboxes) {
      hold <- small_dataframe() %>%
        mutate(factor_of_interest := get(factor)) %>%
        filter(!is.na(factor_of_interest))
      
      res.aov <- aov(factor_of_interest ~ Group,
                     data = hold)
      
      significance <- as.data.frame(TukeyHSD(res.aov)[[1]]) %>%
        mutate(Group = rownames(.)) %>%
        select(Group, `p adj`) %>%
        rename(!!quo_name(factor) := `p adj`)
      
      if (factor != first_factor) {
        all_significances <- all_significances %>%
          left_join(significance, by = "Group")
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
      calculated_table()  %>% 
        select(order(colnames(.))) %>%
        select(-Group),
      selection = "single",
      rownames = FALSE,
      options = list(pageLength = 50,
                     paging = FALSE,
                     searching = FALSE)) %>%
      formatRound(input$factor_checkboxes, 2) %>%
      formatStyle(
        input$factor_checkboxes,
        backgroundColor = styleInterval(c(.05, .2),
                                        c('#adc9b1', '#f0f0cc', 'white'))
      )
  })
  
  output$distribution_plot <- renderPlot({
    if (is.null(input$target_group)) {
      return ()
    }
    min_max_norm <- function(x) {
      (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
    }
    numeric_cols <- input$factor_checkboxes
    
    distributions <- small_dataframe() %>%
      mutate(across(
        all_of(numeric_cols),
        ~ min_max_norm(.))) %>%
      select(c(Group, all_of(numeric_cols))) %>%
      pivot_longer(-Group, names_to = "variables", values_to = "value") %>%
      filter(!is.na(value)) %>%
      mutate(variables = factor(variables, levels = numeric_cols)) %>%
      arrange(variables)
    
    violins <- distributions %>%
      ggplot(aes(fill=Group, y=value, x=variables)) + 
      geom_violin(position = position_dodge(width = 0.5), alpha = 0.8) +
      # theme_ipsum() +
      xlab("Variable") +
      ylab("Distribution")  +
      theme(text = element_text(family = "DM Sans",face="plain"),
            axis.ticks.y=element_blank()) + 
      scale_fill_manual(values=c("#0785F2", "#414042"))
    
    add_summary(violins, "median_q1q3", group = "Group", color = "white",
                position = position_dodge(width = 0.5), shape = 1, size = 0.3)
  })

  
}
