library(shiny)
library(dplyr)

ui <- basicPage(
  fluidRow(
    uiOutput("ui"),
    sliderInput("slider", "Sepal Width", min = 0, max=20, value = 1:2),
    tableOutput("data")
  )
)

server <- function(input, output){
  get_width_filter <- reactive({
    iris %>% filter(between(Sepal.Width, input$slider[1], input$slider[2]))
  })
  
  get_ovl_filter <- reactive({
    get_width_filter() %>%
      filter(Species %in% input$occurence_checkboxes)
  })
  
  output$data <- renderTable({
    get_ovl_filter()
  })
  
  output$ui <- renderUI({
    choices <-  get_width_filter() %>% pull(Species) %>% unique()
    checkboxGroupInput("occurence_checkboxes", 
                       "Species", 
                       choices = choices, 
                       selected = choices)
  })
}

shinyApp(ui, server)