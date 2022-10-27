pacman::p_load(shiny, tidyverse, tidytable, data.table, googlesheets4)

# setting user interface
ui <- navbarPage(
  
  title = "Daily Monitoring",
  
    # questionare  -------------------------------------------------------------
    tabPanel( "Questionare", 
             fluidRow(
               column(9, align = 'left',
                      textInput(inputId = "name", 
                                label = "Name (Last, First):", 
                                value = ""),
                      dateInput(inputId = "q_date",
                                label = "Date:",
                                min = "2022-01-01",
                                weekstart = 1),
                      selectizeInput(inputId = "sleep", 
                                     label = "Sleep (1-good, 5-bad):",
                                     choices = seq(1:5)), 
                      selectizeInput(inputId = "soreness",
                                     label = "Soreness (1-good, 5-bad):",
                                     choices = seq(1:5)), 
                      selectizeInput(inputId = "morale", 
                                     label = "Morale (1-good, 5-bad):",
                                     choices = seq(1:5))
                      )
             )
             ),
    # RPE ----------------------------------------------------------------------
    tabPanel("RPE", 
             fluidRow(
               column(9, align = 'left',
                      textInput(inputId = "name", 
                                label = "Name (Last, First):", 
                                value = ""),
                      dateInput(inputId = "rpe_date",
                                label = "Date:",
                                min = "2022-01-01",
                                weekstart = 1), 
                      selectizeInput(inputId = "rpe", 
                                     label = "RPE:", 
                                     choices = seq(1:10)), 
                      numericInput(inputId = "rpe_duration", 
                                   label = "Session Duration:", 
                                   value = 60,
                                   min = 1, 
                                   max = 360)
                      )
             )
             )
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    
}

# Run the application 
shinyApp(ui = ui, server = server)
