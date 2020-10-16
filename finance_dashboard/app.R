#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Very please to have https://juanitorduz.github.io/dockerize-a-shinyapp/ show me how to Dockerize
#
#
#
# This app takes a file called "Retirement" with the following data structure:
#   - Investments: The investment name
#   - Rate: Morningstar rating of the fund
#   - Fee: Fee paid to maintain the fund
#   - Location: Bank of firm holding the fund
#   - Description: What type of fund (stock, bond, cash, etc)
#   - Shares: How many individual shares owned
#   - Date columns, by month: Displays the value of the investment for that month
#
#
# This app returns:
#   - Pie chart of investment type by value for past month
#   - Pie chart of investment area by value for past month
#   - Line graph of investment value over time for all individual investments
#   - Line graph of investment value over time for investment types
#   - Bar chart of morningstar ratings
#   - Scatterplot of investment fees
#   - Datatable displaying information
#
#
# The data can be filtered by almost any column, except rate and fee, which contain null values

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(tidyverse)
library(readxl)
library(janitor)
library(highcharter)
library(DT)

money <- read_xlsx('data/Retirement.xlsx') %>% 
    clean_names()

money_long <- money %>% 
    pivot_longer(!c(investments, rate, fee, location, description, shares), names_to = "date", values_to = "value") %>% 
    mutate(date = str_remove(date, 'x')) %>% 
    mutate(date = as.Date(as.numeric(date), origin = "1899-12-30")) %>% 
    mutate(description_simple = str_split(description, "-")) %>% 
    separate(description, into = "description_simple", remove = F, sep = "-") %>% 
    mutate(description_simple = trimws(description_simple)) %>% 
    mutate(value_description = ifelse(value < 10000, "<10k",
                                      ifelse(value <50000, "10k-50k",
                                             ifelse(value < 100000, "50k-100k",
                                                    ">100k"))))

year_sum <- money_long %>% 
    filter(date == max(date)) %>% 
    summarize(sum(value, na.rm = T)) %>% 
    as.numeric()

last_month <- money_long %>% 
    filter(date == max(date)) %>% 
    mutate(value_perc = value*100/year_sum)

ui <- dashboardPage(
    
    ############# Create the dashboard header #############
    dashboardHeader(title = "Finance Dashboard"),
    
    ############# Create the filters #############
    dashboardSidebar(
        
        ############# Filter by date #############
        dateRangeInput(
            inputId = "date",
            label = "Date:",
            start = min(money_long$date),
            end = max(money_long$date)
        ),
        
        ############# Filter by type (stock, bond, etc) #############
        pickerInput(
            inputId = "type", 
            label = "Type:",
            choices = unique(money_long$description_simple),
            multiple = T,
            selected = unique(money_long$description_simple),
            options = list(`actions-box` = TRUE)
        ),
        
        ############# Filter by value #############
        pickerInput(
            inputId = "value",
            label = "Value:",
            choices = c("NA", "<10k", "10k-50k", '50k-100k', '>100k'),
            multiple = T,
            selected = unique(money_long$value_description),
            options = list(`actions-box` = TRUE)
        ),
        
        ############# Filter by location #############
        pickerInput(
            inputId = "location",
            label = "Location:",
            choices = sort(unique(money_long$location)),
            multiple = T,
            selected = unique(money_long$location),
            options = list(`actions-box` = TRUE)
        ),
        
        ############# Filter by rating #############
        #pickerInput(
        #  inputId = "rating",
        #  label = "Morningstar Rating:",
        #  choices = unique(money_long$rate),
        #  multiple = T,
        #  selected = unique(money_long$rate),
        #  options = list(`actions-box` = TRUE)
        #),
        
        ############# Filter by fee #############
        #sliderInput(
        #  inputId = "fee",
        #  label = "Fee:",
        #  min = min(money_long$fee, na.rm = T),
        #  max = max(money_long$fee, na.rm = T),
        #  value = c(min(money_long$fee, na.rm = T), max(money_long$fee, na.rm = T))
        #),
        
        ############# Filter by specific investment #############
        pickerInput(
            inputId = "investment_name",
            label = "Investment:",
            choices = sort(unique(money_long$investments)),
            multiple = T,
            selected = unique(money_long$investments),
            options = list(`actions-box` = TRUE,
                           `live-search`=TRUE)
        )
    ),
    
    ############# Create the UI for the body, just boxes next to each other #############
    dashboardBody(
        fluidRow(box(highchartOutput("type_by_value_pie")),
                 box(highchartOutput("specific_type_by_value_pie"))),
        fluidRow(box(highchartOutput("type_value_over_time")),
                 box(highchartOutput("investment_over_time"))),
        fluidRow(box(highchartOutput("morningstar_rating")),
                 box(highchartOutput("associated_fee"))),
        fluidRow(DTOutput("data_table"))
    )
)

############# APP
server <- function(input, output) {
    
    last_month_reactive <- reactive({
        last_month %>% 
            filter(description_simple %in% input$type) %>% 
            filter(value_description %in% input$value) %>% 
            filter(location %in% input$location) %>% 
            filter(investments %in% input$investment_name) 
        #filter(fee >= input$fee[1] & fee <= input$fee[2]) %>% 
        #filter(rate %in% input$rating)
    })
    
    money_long_reactive <- reactive({
        money_long %>% 
            filter(date >= input$date[1] & date <= input$date[2]) %>% 
            filter(description_simple %in% input$type) %>% 
            filter(value_description %in% input$value) %>% 
            filter(location %in% input$location) %>% 
            filter(investments %in% input$investment_name)
        #filter(fee >= input$fee[1] & fee <= input$fee[2]) %>%
        #filter(rate %in% input$rating)
    })
    
    ############# Pie graph of investment value by type #############
    output$type_by_value_pie <- renderHighchart({
        hchart(
            last_month_reactive() %>%
                group_by(description_simple) %>% 
                summarise(total_perc = sum(value_perc, na.rm = T)),
            type = "pie",
            hcaes(x= description_simple, y = total_perc)) %>% 
            hc_title(text = "Investment Type by Value in Past Month") %>% 
            hc_subtitle(text = paste0("Month Ending ", unique(last_month$date)))
    })
    
    ############# Pie graph of investment value by area #############
    output$specific_type_by_value_pie <- renderHighchart({
        hchart(
            last_month_reactive() %>%
                group_by(description) %>% 
                summarise(total_perc = sum(value_perc, na.rm = T)),
            type = "pie",
            hcaes(x = description, y = total_perc)) %>% 
            hc_title(text = "Investment Area by Value in Past Month") %>% 
            hc_subtitle(text = paste0("Month Ending ", unique(last_month$date)))
    })
    
    ############# Line graph of investment value over time by type #############
    output$investment_over_time <- renderHighchart({
        hchart(
            money_long_reactive() %>% 
                group_by(date, description_simple) %>% 
                summarise(sum_value = sum(value, na.rm = T)),
            type = "line",
            hcaes(x = date, y = sum_value, group = description_simple)) %>% 
            hc_title(text = "Investment Type Value over Time")
    })
    
    ############# Line graph of investment value over time by specific investment #############
    output$type_value_over_time <- renderHighchart({
        hchart(
            money_long_reactive() %>% 
                group_by(date, investments, description_simple) %>% 
                summarise(sum_value = sum(value, na.rm = T)),
            type = "line",
            hcaes(x = date, y = sum_value, group = investments)) %>% 
            hc_title(text = "Investment Value over Time") %>% 
            hc_legend(enabled = F)
    })
    
    ############# Bar graph of morningstar rating #############
    output$morningstar_rating <- renderHighchart({
        hchart(
            money_long_reactive() %>%  
                group_by(rate) %>% 
                count(name = 'count'), 
            type = 'bar',
            hcaes(x = rate, y = count)) %>% 
            hc_title(text = "Morningstar Rating")
    })
    
    
    ############# Scatterplot of fees #############
    output$associated_fee <- renderHighchart({
        hchart(
            money_long_reactive() %>%  
                group_by(fee) %>% 
                count(name = 'count'), 
            type = 'scatter',
            hcaes(x = fee, y = count)) %>% 
            hc_title(text = "Investment Fee")
    })
    
    ############# Data table of information #############  
    output$data_table <- renderDT({
        money_long_reactive() %>% 
            pivot_wider(names_from = date, values_from = value) %>% 
            arrange(desc(.[ncol(.)]))
    })
    
}

# Run the app
shinyApp(ui, server)