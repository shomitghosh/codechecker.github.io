library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Argus Code Checker v1"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            fileInput("yardiInput", "Upload Yardi Extract"),
            fileInput("argusInput", "Upload Argus Extract"),
            textInput("fund","Fund Name"),
            actionButton("process", "Process Files")
        ),

        # Show a plot of the generated distribution
        mainPanel(
          downloadButton("download", "Download Processed File")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  processedData <- eventReactive(input$process, {
    yardi_tenancy <- readxl::read_excel(input$yardiInput$datapath, sheet = "TenancySchedule")
    yardi_vacancy <- readxl::read_excel(input$yardiInput$datapath, sheet = "Vacancy")
    ten_cols <- c("PropertyCode","PropertyName","UnitCode","ContractCode","ContractName")
    vac_cols <- c("PropertyCode","PropertyName","UnitCode")
    yardi_tenClean <- yardi_tenancy[,ten_cols]
    yardi_vacClean <- yardi_vacancy[,vac_cols]
    
    yardi_vacClean$ContractCode <- yardi_vacClean$UnitCode
    yardi_vacClean$ContractName <- "VACANT"
    yardi <- rbind(yardi_tenClean,yardi_vacClean)
    
    remove_leading_zeros <- function(x) {
      # Check if string contains only digits
      if (grepl("^[0-9]+$", x)) {
        # Remove leading zeros using sub
        return(sub("^0*", "", x))
      } else {
        # Return original string if it contains non-digits
        return(x)
      }
    }
    
    yardi$ContractCode <- lapply(yardi$ContractCode, remove_leading_zeros)
    yardi$yardiPropLease <- paste(yardi$PropertyCode, yardi$ContractCode,sep="")
    
    
    argus_raw <- readxl::read_excel(input$argusInput$datapath)
    arg_cols <- c("Property Name", "External ID", "Lease ID", "Tenant Name", "Rental Value (ERV)")
    argusClean <- argus_raw[,arg_cols]
    argusClean$argPropLease <- paste(argusClean$`External ID`, argusClean$`Lease ID`, sep = "")
    checker <- merge(argusClean, yardi, by.x = "argPropLease", by.y = "yardiPropLease", all.x = TRUE)
    
    yardi$yardiPropUnit <- paste(yardi$PropertyCode, yardi$UnitCode, sep="")
    checker2 <- merge(argusClean, yardi, by.x = "argPropLease", by.y = "yardiPropUnit", all.x = TRUE)
    tempChecker <- checker[is.na(checker$PropertyCode),]
    
    checker3 <- merge(checker, checker2, by.x = "argPropLease", by.y = "argPropLease",all.x = TRUE)
    
    checker4 <- subset(checker3, subset = is.na(PropertyCode.x) & is.na(PropertyCode.y))
    
    checker4 <- checker4[,c(1:6)]
    
    colnames(checker4) <- c("ArgusPropLease","Property Name","Property Code","Lease ID","Tenant Name","Rental Value (ERV)")
    
    checker4 <- unique(checker4)
    checker4$err <- round(nrow(checker4)/nrow(checker2)*100,2)
    return(checker4)
  })
  
  output$download <- downloadHandler(
    filename = function() {
      paste(input$fund,"_mismatch.xlsx",sep="")
    },
    content = function(file) {
      # Write the processed data to Excel file
      writexl::write_xlsx(processedData(), path = file)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
