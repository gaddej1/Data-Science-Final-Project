#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

options(stringsAsFactors = FALSE)
options(scipen = 999)
library(shiny)
library(shinyWidgets)
library(readr)
library(tidyverse)

myData <- read_csv(url(urlfile))

# UI
ui <- fluidPage(
    setBackgroundColor(
        color = c("#AFEEEE", "#98FB98"), 
        gradient = "linear", 
        direction = "bottom"
    ),

    # Application title
    titlePanel("Do Majors With More Men Than Women Tend to Have Higher Salaries?"),
    
    #Graph 1
    splitLayout(
        sliderInput("head",
                    "Number of Highest Paying Majors:",
                    min = 5,
                    max = 20,
                    value = 10), 
        plotOutput("hpm")
    ), 
    #Table 1
    splitLayout(
        sliderInput("rows", 
                   "Number of Majors with Highest Percentage of Women", 
                   min = 5, 
                   max = 20, 
                   value = 10), 
        tableOutput("table1")
    ), 
    #Table 2
    splitLayout(
        sliderInput("rows2", 
                    "Number of Majors with Highest Percentage of Men", 
                    min = 5, 
                    max = 20, 
                    value = 10),
        tableOutput("table2")
    ), 
    #Graph 2
    splitLayout(
        radioButtons("percentile", label = h3("Percentile"),
                     choices = list("25th Percentile" = 1, "Median" = 2, "75th Percentile" = 3), 
                     selected = 1),
        plotOutput("salary")
    )
)

# Server
server <- function(input, output) {
    
    #Graph 1
    output$hpm <- renderPlot({
        myData <- mutate(myData, majWom = ifelse(myData$Women > myData$Men, T, F))
        top <- head(myData, input$head)
        ggplot(data = top) + 
            geom_bar(mapping = aes(x = Major, y = Median, fill = majWom), stat = "identity") +
            theme(axis.text.x=element_text(angle = 60, hjust = 1)) +
            labs(x = "Highest Paying Majors", y = "Median Salary", 
            fill = "Majority Women?",
            title = "Average Salaries of Highest Paying Majors")
    })
    
    #Graph 2
    output$salary <- renderPlot({
        if (input$percentile == 1){
            ggplot(data = myData, mapping = aes(x = ShareWomen, y = P25th)) +
                geom_point(color = "mediumorchid2") +
                geom_smooth(method = "lm", color = "black") + 
                labs(x = "Percentage of Women", y = "25th Percentile Salary", 
                     title = "25th Percentile Salaries v.s. Percentage of Women in Each Major")
        }
        else if (input$percentile == 2){
            ggplot(data = myData, mapping = aes(x = ShareWomen, y = Median)) +
                geom_point(color = "mediumorchid2") +
                geom_smooth(method = "lm", color = "black") + 
                labs(x = "Percentage of Women", y = "Median Salary", 
                     title = "Median Salaries v.s. Percentage of Women in Each Major")
        }
        else if (input$percentile == 3){
            ggplot(data = myData, mapping = aes(x = ShareWomen, y = P75th)) +
                geom_point(color = "mediumorchid2") +
                geom_smooth(method = "lm", color = "black") + 
                labs(x = "Percentage of Women", y = "75th Percentile Salary", 
                     title = "75th Percentile Salaries v.s. Percentage of Women in Each Major")
        }
    })
    
    #Table 1
    output$table1 <- renderTable({
        dat <- mutate(myData, PercentWomen = ShareWomen * 100) %>%
            select(Major, Median, PercentWomen) %>%
            arrange(desc(PercentWomen))
        head(dat, input$rows)
    })
    
    #Table 2
    output$table2 <- renderTable({
        dat <- mutate(myData, PercentMen = 100 - (ShareWomen * 100)) %>%
            select(Major, Median, PercentMen) %>%
            arrange(desc(PercentMen))
        head(dat, input$rows2)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
