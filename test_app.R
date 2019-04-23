# load the required packages
library(shiny)
require(shinydashboard)
library(ggplot2)
library(dplyr)
library(r2d3)
library(nycflights13)


#####################################
## Setup                           ##
#####################################

recommendation <- read.csv('C:\\Users\\Chun Lam\\Git\\clones\\shiny\\recommendation.csv',stringsAsFactors = F,header=T)
head(recommendation)

signal_list <- c('s_bull_stick', 's_bear_stick', 's_bull_engulf', 's_bear_engulf', 's_bull_harami', 's_bear_harami', 's_2day_reverse_good', 's_2day_reverse_bad', 's_bull_pierce', 's_bear_pierce', 's_hammer', 's_shooting_star')
signal_list_chinese <- c('大陽燭', '大陰燭', '向好吞噬', '向淡吞噬', '向好身懷六甲', '向淡身懷六甲', '向好雙日轉向', '向淡雙日轉向', '曙光初現', '烏雲蓋頂', '鎚頭', '射擊之星')
signal_radioButton_list <- as.list(setNames(c("none", signal_list), c("None",signal_list_chinese)))



#####################################
## Build UI                        ##
#####################################


header <- dashboardHeader(title = "Stock analysis")


sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Dashboard", tabName = "tab_dashboard", icon = icon("dashboard")),
        menuItem("Signal"   , tabName = "tab_signal"   , icon = icon("cog", lib = "glyphicon")),
        menuItem("Stock"    , tabName = "tab_stock"    , icon = icon("bar-chart-o")),
        menuItem("Option"   , tabName = "tab_option"   , icon = icon("list-alt"))
    )
)

row_top <- fluidRow(
    valueBoxOutput("box_success"),
    valueBoxOutput("box_fail"),
    valueBoxOutput("box_score")
)

row_bottom <- fluidRow(

    box(
        title = "Control", width = 4, solidHeader = TRUE, status = "primary",

        textInput("dash_input_code", "Stock Number", placeholder = "Stock Number"),

        dateRangeInput("dash_input_date",
                       "Date range:",
                       format = "yyyy-mm-dd",
                       start = "2017-01-01",
                       end = "2018-12-31",
                       min = "2010-01-01",
                       max = "2018-12-31"),

        selectInput(
            inputId = "dash_input_signal",
            label = "Signal",
            choices = signal_radioButton_list,
            selected = "none",
            size = length(signal_radioButton_list),
            selectize = FALSE
        )
    ),

    box(
        title       = "Output",
        status      = "primary",
        width       = 8,
        solidHeader = TRUE,
        collapsible = TRUE,
        #plotOutput("dash_output_graph", height = "600px")
        d3Output("test_group_totals")
    )


)

row_debug <- fluidRow(
    box(
        title = "Debug", width = 8, solidHeader = TRUE, status = "primary",

        verbatimTextOutput("dash_output_debug")
    )
)

ui <- dashboardPage(

        dashboardHeader(
            title = "Stock analysis",
            titleWidth = 200
        ),

        dashboardSidebar(
            sidebarMenu(
                menuItem("Dashboard", tabName = "tab_dashboard", icon = icon("dashboard")),
                menuItem("Signal"   , tabName = "tab_signal"   , icon = icon("cog", lib = "glyphicon")),
                menuItem("Stock"    , tabName = "tab_stock"    , icon = icon("bar-chart-o")),
                menuItem("Option"   , tabName = "tab_option"   , icon = icon("list-alt"))
            )
        ),

        # dashboardBody(row_top, row_bottom, row_debug)
        dashboardBody(
            tabsetPanel(
                id = "main_tabs",
                tabPanel(
                    title = "Main Dashboard",
                    value = "tab_dashboard",
                    fluidRow(
                        valueBoxOutput("box_success"),
                        valueBoxOutput("box_fail"),
                        valueBoxOutput("box_score")
                    ),
                    fluidRow(),
                    row_bottom,

                    fluidRow(
                        box(
                            title       = "Details",
                            status      = "primary",
                            width       = 12,
                            solidHeader = TRUE,
                            collapsible = TRUE,
                            height      = 400,
                            verbatimTextOutput("test_text", placeholder = TRUE)
                        )
                    ),

                    row_debug
                )
            )
        )

)
# create the server functions for the dashboard
server <- function(input, output, session) {

    #####################################
    ## Get Data                        ##
    #####################################
    base_data <- reactive({
        res <- iris
        res
    })

    #####################################
    ## Value box at top                ##
    #####################################

    output$box_success <- renderValueBox({
        valueBox(
            formatC(23, format="d", big.mark=','),
            "Success",
            icon = icon("thumbs-up"),
            color = "green")
    })

    output$box_fail <- renderValueBox({

        valueBox(
            formatC(12, format="d", big.mark=','),
            'Fail',
            icon = icon("thumbs-down"),
            color = "red")

    })

    output$box_score <- renderValueBox({

        valueBox(
            formatC(123, format="d", big.mark=','),
            "Signal Score",
            icon = icon("star"),
            color = "yellow")

    })

    #####################################
    ## Testing Components            ##
    #####################################

    # Test 1 - Test D3 bar chart
    output$test_group_totals <- renderD3({

        res <- data.table::data.table(x = c(1:10),
                                      # y = round(rnorm(10, mean = 0, sd = 5)/100, 2),
                                      y = c(2:11),
                                      z = c(3:12))

        r2d3(res, "col_plot.js")
    })

    # Test 2 - Output text
    output$test_text <- renderText({

        #head(iris)
        some_input = input$column_clicked
        sprintf("You have selected this %s", some_input)
    })



}


shinyApp(ui, server)
