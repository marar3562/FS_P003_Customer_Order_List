# original code for upload:  https://shiny.rstudio.com/gallery/file-upload.html

# original code for download:  https://shiny.rstudio.com/articles/download.html

# Case study similar to what we want: https://mastering-shiny.org/action-transfer.html#case-study-transfer


library(shiny)
library(vroom)
library(tidyverse)
library(tools)

# Define UI for data upload app ----
ui_upload <- sidebarLayout(
    sidebarPanel(
        fileInput("file", "Data", buttonLabel = "Upload...") # ,
        # textInput("delim", "Delimiter (leave blank to guess)", ""),
        # numericInput("skip", "Rows to skip", 0, min = 0),
        # numericInput("rows", "Rows to preview", 10, min = 1)
    ),
    mainPanel(
        h3("Raw data"),
        tableOutput("preview1")
    )
)

ui_clean <- sidebarLayout(
    sidebarPanel(
        checkboxInput("clean_d", "Would you like the data cleansed?") # ,
        # checkboxInput("constant", "Remove constant columns?"),
        # checkboxInput("empty", "Remove empty cols?")
    ),
    mainPanel(
        h3("Cleaner data"),
        tableOutput("preview2")
    )
)

ui_download <- fluidRow(
    column(width = 12, downloadButton("download", class = "btn-block"))
)

ui <- fluidPage(
    ui_upload,
    ui_clean,
    ui_download
)

# ui <- fluidPage(
#     
#     # App title ----
#     titlePanel("Uploading Files"),
#     
#     # Sidebar layout with input and output definitions ----
#     sidebarLayout(
#         
#         # Sidebar panel for inputs ----
#         sidebarPanel(
#             
#             # Input: Select a file ----
#             fileInput("file1", "Choose CSV File",
#                       multiple = FALSE,
#                       accept = ".csv"
#                                 # c("text/csv",
#                                 #  "text/comma-separated-values,text/plain",
#                                 #  ".csv")
#                                 ),
#             
#             # Horizontal line ----
#             tags$hr(),
#             
#             # Input: Checkbox if file has header ----
#             # Commented out: assume that headers are present
#             # checkboxInput("header", "Header", TRUE), #
#             
#             # Input: Select separator ----
#             # Commented out: assume comma separated
#             # radioButtons("sep", "Separator",
#             #              choices = c(Comma = ",",
#             #                          Semicolon = ";",
#             #                          Tab = "\t"),
#             #              selected = ","),
#             
#             # Input: Select quotes ----
#             # Commented out: assume no quotes
#             # radioButtons("quote", "Quote",
#             #              choices = c(None = "",
#             #                          "Double Quote" = '"',
#             #                          "Single Quote" = "'"),
#             #              selected = '"'),
#             
#             # Horizontal line ----
#             tags$hr(),
#             
#             # Input: Select number of rows to display ----
#             # radioButtons("disp", "Display",
#             #              choices = c(Head = "head",
#             #                          All = "all"),
#             #              selected = "head"),
#             
#             # Horizontal line ----
#             tags$hr(),
#             
#             actionButton("transform", "Transform Data"),
#             
#             # Horizontal line ----
#             tags$hr(),
#             
#             
#             # Button
#             downloadButton("downloadData", "Download")
#             
#         ),
#         
#         # Main panel for displaying outputs ----
#         mainPanel(
#             
#             # Output: Data file ----
#             tableOutput("contents")
#             
#         )
#         
#     )
# )

# Define server logic to read selected file ----
server <- function(input, output) {
    
    # Upload ---------------------------------------------------------
    raw <- reactive({
        req(input$file)
        vroom::vroom(input$file$datapath, delim = ",")
    })
    output$preview1 <- renderTable(head(raw(), 20))
    
    
    # init_data <- reactive({
    #     
    #     # input$file1 will be NULL initially. After the user selects
    #     # and uploads a file, head of that data file by default,
    #     # or all rows if selected, will be shown.
    #     
    #     req(input$file1)
    #     
    #     # when reading semicolon separated files,
    #     # having a comma separator causes `read.csv` to error
    #     tryCatch(
    #         {
    #             df <- read.csv(input$file1$datapath,
    #                            # header = input$header,
    #                            # sep = input$sep,
    #                            # quote = input$quote
    #                            )
    # 
    #         },
    #         error = function(e) {
    #             # return a safeError if a parsing error occurs
    #             stop(safeError(e))
    #         }
    #     )
    #     
    #     # ext <- tools::file_ext(input$file1$name)
    #     # switch(ext,
    #     #        csv = vroom::vroom(input$file1$datapath, delim = ","),
    #     #        # tsv = vroom::vroom(input$file$datapath, delim = "\t"),
    #     #        validate("Invalid file; Please upload a .csv file")
    #     # )
    #     
    #     # if(input$disp == "head") {
    #     #     return(head(df))
    #     # }
    #     # else {
    #     #     return(df)
    #     # }
    #     
    # })
    
    
    # Clean ----------------------------------------------------------
    tidied <- reactive({
        out <- raw() %>% dplyr::rename_all(list(~make.names(.)))
        if (input$clean_d) {
            out <- out %>% 
                mutate(Order = strsplit(as.character(Order), ",")) %>%
                unnest(Order) %>% 
                mutate(pickuptime = ifelse(str_detect(Order, "Pickup Time"), TRUE, FALSE)) %>% 
                group_by(Pickup.Site, First.Name, Last.Name) %>% 
                arrange(Pickup.Site, First.Name, Last.Name) %>% 
                mutate(rank_v = rank(row_number()),
                       group = ifelse(rank_v == 1, TRUE, FALSE),
                       rows= max(rank_v)
                )
        }
        
        out
    })
    output$preview2 <- renderTable(head(tidied(), 20))
    
    #
    # transform_members = observeEvent(input$transform, {
    # 
    #     
    #     df_n = init_data() %>% 
    #         mutate(Order = strsplit(as.character(Order), ",")) %>%
    #         unnest(Order) %>% 
    #         mutate(pickuptime = ifelse(str_detect(Order, "Pickup Time"), TRUE, FALSE)) %>% 
    #         group_by(Pickup.Site, First.Name, Last.Name) %>% 
    #         arrange(Pickup.Site, First.Name, Last.Name) %>% 
    #         mutate(rank_v = rank(row_number()),
    #                group = ifelse(rank_v == 1, TRUE, FALSE),
    #                rows= max(rank_v)
    #         )
    #     
    #     return(head(df_n))
    # })
    # 
    # output$contents = renderTable(head(transform_members()))
    
    
    # Downloadable csv of selected dataset ----
    # This App works on desktop but when downloading not sure how to get file. Still need to remedy issue.
    output$download <- downloadHandler(
        filename = function() {
            paste("test",".csv", sep = "")
        },
        content = function(file) {
            #vroom::vroom_write(tidied(), file)
            write.csv(as.data.frame(tidied()), file, row.names = FALSE)
        })
    
}

# Create Shiny app ----
shinyApp(ui, server)