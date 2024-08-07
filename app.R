library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Argus Code Checker v1"),
  
  # Sidebar with file inputs and text input
  sidebarLayout(
    sidebarPanel(
      fileInput("yardiInput", "Upload Yardi Extract"),
      fileInput("argusInput", "Upload Argus Extract"),
      textInput("fund","Fund Name"),
      actionButton("process", "Process Files")
    ),
    
    # Show a download button for processed file
    mainPanel(
      downloadButton("download", "Download Processed File")
    )
  )
)

# Define server logic
server <- function(input, output) {
  processedData <- eventReactive(input$process, {
    yardi_tenancy <- readxl::read_excel(input$yardiInput$datapath, sheet = "TenancySchedule")
    yardi_vacancy <- readxl::read_excel(input$yardiInput$datapath, sheet = "Vacancy")
    ten_cols <- c("PropertyCode","PropertyName","UnitCode","ContractCode","Contract