library(shiny)
library(shinydashboard)
library(DT)
library(shinydashboardPlus)
library(ggplot2)
library(dplyr)
library(lubridate)

# Define UI for app
ui <- dashboardPage(
  
  # Define app header
  dashboardHeader(title = "Point of Sale App"),
  
  # Define app sidebar
  dashboardSidebar(
    sidebarMenu(
      menuItem("Transactions", tabName = "transactions", icon = icon("credit-card")),
      menuItem("Sales Data", tabName = "sales_data", icon = icon("chart-bar")),
      menuItem("Sales Charts", tabName = "sales_charts", icon = icon("chart-pie"))
    )
  ),
  
  # Define app body
  dashboardBody(
    
    # Define tab items
    tabItems(
      
      # Define "Transactions" tab
      tabItem(tabName = "transactions",
              
              # Define input widgets
              fluidRow(
                box(title = "Item" ,selectInput("vendor", "Vendor", 
                                      choices = c("Vendor A", "Vendor B", "Vendor C")),
                      selectInput("category","Category",
                                      choices = c("Electronics", "Furniture", "Toys")),
                    textInput("item", "Item Description"),
                    numericInput("price", "Price", value = NULL),
                    actionButton("add_item", "Add Item"),
                    actionButton("remove_item", "Remove Item")),
                
                
                
                        infoBoxOutput("total"),
                       
                       actionButton("checkout", "Checkout"),
                
              
              box(dataTableOutput("receipt_table"))
              # Define receipt table
             
              )     
              
      ),
      
      # Define "Sales Data" tab
      tabItem(tabName = "sales_data",
              
              # Define input widgets
              fluidRow(
                column(3, selectInput("filter_vendor", "Vendor",
                                      choices = c("All", "Vendor A", "Vendor B", "Vendor C"))),
                column(3, dateRangeInput("filter_date", "Date Range")),
                column(3, actionButton("filter", "Filter"))
              ),
              
              # Define sales data table
              br(),
              dataTableOutput("sales_table")
            
      ),
      
      #define the graph tab page
      tabItem(tabName = "sales_charts",
              fluidRow(
                box(width = 12, height = 500,
                    plotOutput("sales_chart"))
              )
              
                
              )
      
    )
    
  )
  
)

# Define server for app
server <- function(input, output, session) {
  
  # Define reactive receipt data frame
  receipt_data <- reactiveValues(items = data.frame(item = character(), 
                                                    price = numeric(), 
                                                    stringsAsFactors = FALSE))
  
  
  # Add an item to the receipt
  observeEvent(input$add_item, {
    new_item <- data.frame(item = input$item, price = input$price)
    receipt_data$items <- rbind(receipt_data$items, new_item)
    updateTextInput(session, "item", value = "")
    updateNumericInput(session, "price", value = 0)
  })
  
  
  # Remove an item from the receipt
  observeEvent(input$remove_item, {
    if (nrow(receipt_data$items) > 0) {
      receipt_data$items <- receipt_data$items[-nrow(receipt_data$items), ]
    }
  })
  
  # Define receipt table output
  output$receipt_table <- renderDataTable({
    receipt_data$items
  }, options = list(searching = FALSE, lengthChange = FALSE))
  
  # Define total widget output
  output$total <- renderInfoBox({
    total <- sum(receipt_data$items$price) * 1.07
    
    infoBox(
      "Total",
      paste0("$", total),
      icon = icon("dollar-sign"),
      color = "teal",
      tags$style("font-size: 36px;")
      #paste0("$", total), "Total", color = "teal", icon = icon("dollar-sign")
    )
  })
  
  observeEvent(input$checkout, {
    # Create sales.csv file if it doesn't exist
    if (!file.exists("sales.csv")) {
      write.csv(data.frame(date = character(), vendor = character(),
                           catagory = character(),
                           item = character(), price = numeric(),
                           stringsAsFactors = FALSE),
                "sales.csv", row.names = FALSE)
    }
    # Append transaction details to sales.csv
    datetime <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    transaction_data <- cbind(datetime, vendor = input$vendor, category = input$category,
                              receipt_data$items)
    write.table(transaction_data, "sales.csv", sep = ",",
                col.names = FALSE, row.names = FALSE,
                append = TRUE)
    # Clear receipt
    receipt_data$items <- data.frame(item = character(), price = numeric(),
                                     stringsAsFactors = FALSE)
  })
  sales_data_subset <- reactive({
    # Read in sales.csv
    sales_data <- read.csv("sales.csv", stringsAsFactors = FALSE)
    
  sales <- reactive({
      sales <- read.csv("sales.csv", stringsAsFactors = FALSE)
      sales$date <- lubridate::ymd_hms(sales$date)
      sales
    })
    
    output$sales_chart <- renderPlot({
      sales_data <- sales()
      sales_data <- sales_data %>% filter(year(date) == year(Sys.Date()), month(date) == month(Sys.Date()))
      sales_data <- sales_data %>% group_by(vendor) %>% summarize(month_sales = sum(price))
      ggplot(sales_data, aes(x=vendor, y=month_sales, fill=vendor)) +
        geom_col() +
        ggtitle("Monthly Sales by Vendor") +
        xlab("Vendor") +
        ylab("Month-to-Date Sales")
    })
    
    
    # Filter sales data based on input widgets
    if (input$filter_vendor != "All") {
      sales_data_subset <- sales_data[sales_data$vendor == input$filter_vendor, ]
    } else {
      sales_data_subset <- sales_data
    }
    if (!is.null(input$filter_date)) {
      sales_data_subset <- sales_data_subset[sales_data_subset$date >= input$filter_date[1] &
                                               sales_data_subset$date <= input$filter_date[2], ]
    }
    sales_data_subset
  })
  output$sales_table <- renderDataTable({
    sales_data_subset()
  })
  
}
shinyApp(ui, server)
