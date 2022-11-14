pacman::p_load(shiny, tidyverse, tidytable, data.table, lubridate, runner, roll, 
               thematic, shinythemes, htmlwidgets, RCurl, httr, rsconnect, shinydashboard)

##########
########## themes ---------------------------------------------------------
##########

# windowsFonts(a = windowsFont("Times New Roman"))
# 
# fletch_theme <- function(){
#   theme_fletch <- list(theme(title = element_text(size = 20, face = 'bold', family = "TT Times New Roman"), 
#         axis.title.x = element_text(size = 15, family = "TT Times New Roman"),
#         axis.title.y = element_text(size = 15, family = "TT Times New Roman"),
#         axis.text = element_text(size = 12, family = "TT Times New Roman")))
#   
#   return(theme_fletch)
# }

##########
########## data import ---------------------------------------------------------
##########

phys_data <- fread("physiological_cycles.csv") %>%  
  janitor::clean_names() %>%  
  arrange.(cycle_start_time) %>% 
  drop_na.(recovery_score_percent) %>%
  mutate.(cycle_start_time = as.POSIXct(cycle_start_time)) %>%  
  drop_na.(cycle_end_time) %>% 
  mutate.(date = as.Date(cycle_start_time), .before = cycle_start_time) %>% 
  mutate.(date = case_when.(as.ITime(cycle_start_time) < as.ITime("05:00:00") ~ date - days(1), 
                            TRUE ~ date)) %>% 
  select.(-c(contains("cycle"), contains("onset"))) %>% 
  # mutate.(across.(where(is.numeric), ~roll_scale(.x, width = 7, min_obs = 2), 
                  # .names = "{.col}_scale_roll"))# |> 
  pivot_longer.(-date) %>% 
  mutate.(z = c(scale(value)), 
          .by = name) %>% 
  arrange.(date) %>% 
  tidytable::dt(, roll_sd := runner(value, function(x) last(sd(x)), k = 7), by = name) %>% 
  tidytable::dt(, roll_z := runner(value, function (x) last(scale(x)), k = 7), by = name) %>% 
  mutate.(raw_color = case_when.(value < value - (roll_sd * 1.5) ~ "#CC79A7",
                             value > value + (roll_sd * 1.5) ~ "#E69F00",
                             TRUE ~ "sky blue"), 
          z_color = case_when.(z < roll_z - 1.5 ~ "#CC79A7",
                               z > roll_z + 1.5 ~ "#E69F00",
                               TRUE ~ "sky blue"), 
          .by = name) %>% 
  mutate.(name = str_replace_all(name, "_", " "))

# work_data <- fread("workouts.csv") %>% 
#   janitor::clean_names() %>% 
#   mutate.(date = as.Date(workout_start_time), 
#           .before = cycle_start_time) %>% 
#   arrange.(date) %>% 
#   drop_na.(activity_strain)


# sleep_data <- fread("sleeps.csv") %>% 
#   janitor::clean_names()

# phys_data_join <- left_join.(phys_data, strain)
##########
########## setting user interface ----------------------------------------------
##########

header <- dashboardHeader(title = HTML("Whoop Tracking"),
                            disable = FALSE, 
                            titleWidth = 200)
  
sidebar <- 
  dashboardSidebar(
    width = 200, 
    sidebarMenu(
      id = 'sidebar', 
      style = "position: relative;
               overflow: visible;", 
      menuItem("Activity", tabName = "activity"), 
      menuItem("Sleep", tabName = "sleep"), 
      menuItem("Recovery", tabName = "recovery")))
  
body <- dashboardBody(
  tabItems(
    # WHOOP Daily Activity  ----------------------------------------------------
  tabItem(tabName = "activity", 
          tags$style(".selectize-dropdown-content{
                     text-size = 50px;
                     background-color: #FFFFFF;
                     }"),
          tags$style('.input-daterange {
                     font-size: 40px; 
                     }'),
          selectizeInput(inputId = "raw_activity", 
                         label = "Activity:",
                         choices = c("day strain", "energy burned cal", 
                                     "max hr bpm", "average hr bpm")), 
          dateRangeInput(inputId = "date_range",
                         label = "Date Range:",
                         start = min(phys_data$date), 
                         end = max(phys_data$date)), 
         br(),
         plotOutput("activity_plot"),
         br(),
         p(em("Raw data ribbon is set at Value +/- (Value SD * 1.5)")),
         plotOutput("activity_z_plot"), 
         br(),
         p(em("Z-score data ribbon is set at Rolling Value Z-score +/- 1.5"))
         ),

    # WHOOP Sleep Metrics ------------------------------------------------------
    tabItem(tabName = "sleep",
            tags$style(".selectize-dropdown-content{
                       text-size = 50px;
                       background-color: #FFFFFF;
                       }"),
            selectizeInput(inputId = "raw_sleep",
                           label = "Sleep:",
                           choices = c("sleep performance percent", "respiratory rate rpm",
                                       "asleep duration min", "in bed duration min", "light sleep duration min",
                                       "deep sws duration min", "rem duration min", "awake duration min",
                                       "sleep need min", "sleep debt min", "sleep efficiency percent")), 
            dateRangeInput(inputId = "date_range",
                           label = "Date Range:",
                           start = min(phys_data$date), 
                           end = max(phys_data$date)), 
            br(),
            plotOutput("sleep_plot"),
            br(),
            p(em("Raw data ribbon is set at Value +/- (Value SD * 1.5)")),
            br(),
            plotOutput("sleep_z_plot"),
            br(),
            p(em("Z-score data ribbon is set at Rolling Value Z-score +/- 1.5"))
    ),
    # # WHOOP Recovery Metrics ---------------------------------------------------
    tabItem(tabName = "recovery",
            tags$style(".selectize-dropdown-content{
                       text-size = 50px;
                       background-color: #FFFFFF;
                       }"),
            selectizeInput(inputId = "raw_recovery",
                           label = "Recovery:",
                           choices = c("recovery score percent", "resting heart rate bpm",
                                       "heart rate variability ms", "respiratory rate rpm")), 
            dateRangeInput(inputId = "date_range",
                           label = "Date Range:",
                           start = min(phys_data$date), 
                           end = max(phys_data$date)),
            br(),
            plotOutput("recovery"),
            br(),
            p(em("Raw data ribbon is set at Value +/- (Value SD * 1.5)")),
            br(),
            plotOutput("recovery_z_plot"),
            br(),
            p(em("Z-score data ribbon is set at Rolling Value Z-score +/- 1.5"))
            )
  ))
ui <- dashboardPage(header, sidebar, body)

# Define server logic required to draw a histogram
server <- function(input, output) {

activity_react <- reactive({
   
   phys_data %>% 
     filter.(name %in% input$raw_activity, 
             between.(date,input$date_range[1], input$date_range[2]))
   
   })

sleep_react <- reactive({
  
  phys_data %>% 
    filter.(name %in% input$raw_sleep, 
            between.(date,input$date_range[1], input$date_range[2]))
  
})

recov_react <- reactive({
  
  phys_data %>% 
    filter.(name %in% input$raw_recovery, 
            between.(date,input$date_range[1], input$date_range[2]))
  
})
 
  
  output$activity_plot <- renderPlot({
  
    ggplot(activity_react()) +
      geom_ribbon(aes(x = date, 
                      ymin = value - (roll_sd * 1.5), 
                      ymax = value + (roll_sd * 1.5))) +
      geom_line(aes(date, value), color = "sky blue") +
      geom_point(aes(date, value, color = raw_color), size = 3) +
      ylab(input$raw_activity) +
      scale_color_identity() +
      labs(title = str_to_upper(paste(input$raw_activity, "Raw Data Across Time")))+
      theme(title = element_text(size = 20, face = 'bold'),
            axis.title.x = element_text(size = 15),
            axis.title.y = element_text(size = 15),
            axis.text = element_text(size = 12))
  })
 
 
 
  output$activity_z_plot<- renderPlot({
    
    ggplot(activity_react()) +
      geom_ribbon(aes(x = date, 
                      ymin = roll_z - 1.5, 
                      ymax = roll_z + 1.5)) +
      geom_line(aes(date, z), color = "sky blue") +
      geom_point(aes(date, z, color = z_color), size = 3) +
      ylab(input$raw_activity) +
      scale_color_identity() +
      labs(title = str_to_upper(paste(input$raw_activity, "Z-Score Across Time"))) +
      theme(title = element_text(size = 20, face = 'bold'), 
            axis.title.x = element_text(size = 15),
            axis.title.y = element_text(size = 15),
            axis.text = element_text(size = 12))
  })

   
  output$sleep_plot <- renderPlot({
    
    ggplot(sleep_react()) + 
      geom_ribbon(aes(x = date, 
                      ymin = value - (roll_sd * 1.5), 
                      ymax = value + (roll_sd * 1.5))) +
      geom_line(aes(date, value), color = "sky blue") +
      geom_point(aes(date, value, color = raw_color), size = 3) +
      ylab(paste(input$raw_sleep, "Z-Score")) +
      scale_color_identity() +
      labs(title = str_to_upper(paste(input$raw_sleep, "Raw Data Across Time"))) +
      theme(title = element_text(size = 20, face = 'bold'), 
            axis.title.x = element_text(size = 15),
            axis.title.y = element_text(size = 15),
            axis.text = element_text(size = 12))
    
  })
  
  output$sleep_z_plot <- renderPlot({
    
    ggplot(sleep_react()) + 
      geom_ribbon(aes(x = date, 
                      ymin = roll_z - 1.5, 
                      ymax = roll_z + 1.5)) +
      geom_line(aes(date, z), color = "sky blue") +
      geom_point(aes(date, z, color = z_color), size = 3) +
      ylab(paste(input$raw_sleep, "Z-Score")) +
      labs(title = str_to_upper(paste(input$raw_sleep, "Z-Score Across Time"))) +
      scale_color_identity() +
      theme(title = element_text(size = 20, face = 'bold'), 
            axis.title.x = element_text(size = 15),
            axis.title.y = element_text(size = 15),
            axis.text = element_text(size = 12))
    
  })
  
  output$recovery <- renderPlot({
    
    ggplot(recov_react()) + 
      geom_ribbon(aes(x = date, 
                      ymin = value - (roll_sd * 1.5), 
                      ymax = value + (roll_sd * 1.5))) +
      geom_line(aes(date, value), color = "sky blue") +
      geom_point(aes(date, value, color = raw_color), size = 3) +
      ylab(input$raw_recovery) +
      scale_color_identity() +
      labs(title = str_to_upper(paste(input$raw_recovery, "Raw Data Across Time"))) +
      theme(title = element_text(size = 20, face = 'bold'), 
            axis.title.x = element_text(size = 15),
            axis.title.y = element_text(size = 15),
            axis.text = element_text(size = 12))
    
  })
  
  output$recovery_z_plot <- renderPlot({

    ggplot(recov_react()) +
      geom_ribbon(aes(x = date,
                      ymin = roll_z - 1.5,
                      ymax = roll_z + 1.5)) +
      geom_line(aes(date, z), color = "sky blue") +
      geom_point(aes(date, z, color = z_color), size = 3) +
      ylab(paste(input$raw_recovery, "Z-Score")) +
      scale_color_identity() +
      labs(title = str_to_upper(paste(input$raw_recovery, "Z-Score Across Time"))) +
      theme(title = element_text(size = 20, face = 'bold'), 
            axis.title.x = element_text(size = 15),
            axis.title.y = element_text(size = 15),
            axis.text = element_text(size = 12))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


