# load the required packages
library(shiny)
require(shinydashboard)
library(ggplot2)
library(dplyr)


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

        radioButtons("dash_input_signal",
                     label    = h3("Signal"),
                     choices  = signal_radioButton_list,
                     selected = "none")
    ),

    box(
        title       = "Output",
        status      = "primary",
        width       = 8,
        solidHeader = TRUE,
        collapsible = TRUE,
        plotOutput("dash_output_graph", height = "600px")
    )

)

row_debug <- fluidRow(
    box(
        title = "Debug", width = 8, solidHeader = TRUE, status = "primary",

        verbatimTextOutput("dash_output_debug")
    )
)

# combine the two fluid rows to make the body
body <- dashboardBody(row_top, row_bottom, row_debug)

#completing the ui part with dashboardPage
ui <- dashboardPage(title = 'Stock analysis dashboard', header, sidebar, body, skin='red')

# create the server functions for the dashboard
server <- function(input, output, session) {

    #some data manipulation to derive the values of KPI boxes
    total.revenue <- sum(recommendation$Revenue)
    sales.account <- recommendation %>% group_by(Account) %>% summarise(value = sum(Revenue)) %>% filter(value==max(value))
    prof.prod <- recommendation %>% group_by(Product) %>% summarise(value = sum(Revenue)) %>% filter(value==max(value))


    #creating the valueBoxOutput content
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

    #creating the plotOutput content

    output$revenuebyPrd <- renderPlot({
        ggplot(data = recommendation,
               aes(x=Product, y=Revenue, fill=factor(Region))) +
            geom_bar(position = "dodge", stat = "identity") + ylab("Revenue (in Euros)") +
            xlab("Product") + theme(legend.position="bottom"
                                    ,plot.title = element_text(size=15, face="bold")) +
            ggtitle("Revenue by Product") + labs(fill = "Region")
    })


    output$revenuebyRegion <- renderPlot({
        ggplot(data = recommendation,
               aes(x=Account, y=Revenue, fill=factor(Region))) +
            geom_bar(position = "dodge", stat = "identity") + ylab("Revenue (in Euros)") +
            xlab("Account") + theme(legend.position="bottom"
                                    ,plot.title = element_text(size=15, face="bold")) +
            ggtitle("Revenue by Region") + labs(fill = "Region")
    })



}


shinyApp(ui, server)
