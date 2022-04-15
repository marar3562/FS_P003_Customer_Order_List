#
# This is the final Dashboad that Fair Shares employees will use to create the customer order printouts
#


library(shiny)
library(tidyverse)
library(DT)
library(lubridate)
library(googlesheets4)
library(openxlsx)
library(emoji)
library(emo)


## load settings data
gs4_auth(cache = ".secrets", email = TRUE, use_oob = TRUE) #use in shinyapps prod to connect to data
sh_shares = range_read('1pEEGA3mawQWmgDtIjJmpBMPws8_-EyEqPtQlucGVlNQ'
                       ,sheet = 'shares'
                       ,col_types = 'ccccc--'
                       ,skip = 1
)
sh_delete = range_read('1pEEGA3mawQWmgDtIjJmpBMPws8_-EyEqPtQlucGVlNQ'
                       ,sheet = 'delete'
                       ,col_types = 'c'
)
sr = range_read("1xs8TAMrSsJuL_gou4y0DBH3IkaTH0eBn_pdboCGWFTI"
                ,sheet = 'share_rotation'
                ,col_types = 'Diciccc'
)

sr_wed = sr |> 
  select(date) |> 
  distinct()

sr_thu = sr_wed |> 
  mutate(date_all = date + 1)

wed_thu_dates = sr_wed |> 
  mutate(date_all = date) |> 
  rbind(sr_thu) |> 
  arrange() 

all_date = data.frame(all_dates = as_date(min(wed_thu_dates$date_all)
                                          :max(wed_thu_dates$date_all))) |> 
  filter(!all_dates %in% wed_thu_dates$date_all)

date_df = wed_thu_dates |> 
  left_join(sr |> 
              select(date, week, season) |> 
              distinct()
            , by = c('date')
            ) 

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Test"),
    
    tabsetPanel(
      tabPanel("Step 1 - Load Data",

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
      sidebarPanel(
        h4("Step 1a"),
        dateInput("date_value","Enter Pickup Date (used in printout)"
                  ,ymd(str_sub(lubridate::now(),1, 10))
                  ,datesdisabled = all_date$all_dates
                  ),
        actionButton("p_file","Step 1a. Process Date Selected"),
        h4("Step 1b"),
        uiOutput("file_rui"),
        uiOutput("button_1b_rui"),
        h4("Step 1c"),
        uiOutput("button_1c_rui")
        
        ),
      mainPanel(
        dataTableOutput("preview1")
      )
      )
    )#,
    # tabPanel(
    #   sidebarLayout(
    #     sidebarPanel(
    #     ),
    #     mainPanel(
    #     )
    #   )
    # )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  ## Step 1 (Load Data)
  
  ### Load Data
  observeEvent(input$p_file,{
    if(input$date_value %in% wed_thu_dates$date_all) {
      output$file_rui <- renderUI(
        fileInput("file_input", "Choose your file in csv"
                  , multiple = FALSE
                  , accept = c(".csv"))
      )
      output$button_1b_rui <- renderUI(
        actionButton("pp_file","Step 1b. Check File Format"),
      )
    } else {
      showModal(modalDialog(
        title = "Warning",
        "Need to select a Wednesday or Thursday date on Fair Shares calendar.",
        easyClose = TRUE
      ))
    }
    
  })
  
  df_format = reactiveValues(data = NULL)
  
  observeEvent(input$pp_file,{
    if(is.null(input$file_input$datapath)){
      showModal(modalDialog(
        title = "Warning",
        "Please upload a csv file",
        easyClose = TRUE
      ))
    } else{
      input$file_input$datapath %>% 
        read_csv() -> df
      
      if(paste0(colnames(df), collapse = ",") == 'Pickup Site,First Name,Last Name,Order'){
        output$button_1c_rui <- renderUI(
          actionButton("ppp_file","Step 1c. Process File"),
        )
      } else {
        showModal(modalDialog(
          title = "Warning",
          "File is not of correct format. 
          Make sure the file has four columns (Pickup Site, First Name, Last Name, Order)",
          easyClose = TRUE
        ))
      }
      
    }
    df_format$data = df
    output$preview1 <- renderDataTable(datatable(df
                                                 , caption = htmltools::tags$caption(
                                                   style = 'caption-side: top; text-align: left;'
                                                   ,'Raw Data from CSV file'
                                                 )
                                                 ,options = list(
                                                   lengthMenu = c(5, 30, 50)
                                                   , pageLength = 5
                                                   )
                                                 )
                                      )
  })
  
  
  observeEvent(input$ppp_file,{
    df_orig = df_format$data |> 
      rename_all(tolower) |> 
      select_all(funs(gsub(" ", ".", .))) %>%
      rename(group_name = pickup.site, firstname = first.name, lastname = last.name) |> 
      mutate(id = row_number())
    
    ## Group Check
    
    group_check = df_orig |>
      mutate(check = ifelse(group_name %in% (sh_shares)$group_name, TRUE, FALSE)) |>
      filter(check == FALSE) |> 
      select(-check, -order, -id) 

    if (dim(group_check |> select(group_name) |> distinct())[1] > 0) {
      output$preview1 <- renderDataTable(datatable(group_check))
      showModal(modalDialog(
        title = "Warning",
        HTML("There are Fair Shares groups in this file that do not match the Standard Setup file. 
        Please see the chart for problem Groups and member names. 
        Before rerunning this process, you either need to:<br>
        1) Update the Farmigo group for these members<br>
        2) Update the Standard Setup file"),
        easyClose = TRUE
      ))
    } else {
      output$preview1 <- renderDataTable(datatable(df_orig))
    }
    
  })
  
  ######ALL THREE OF THESE BELOW STILL NEED TO BE BUILT INTO THE APP ABOVE!!!
  #### Member Count - Start
  
  #### Make data long
  
  #### Pickup Times found
  
  
  
  
  ## Step 2 (Standard Shares)
  
  ### Standard Share Members
  
  ## Step 3 (Delete Items)
  
  ### Delete List
  
  ### Actions
  
  #### Plant Print Out (Optional)
  
  #### Optimize orders to two columns
  
  ## Step 4 (Item Styling)
  
  ### adding in symbols or emojis
  
  ### Highlight Share / customer names (via Excel)
  
  ### Action
  
  #### Final view of data seen
  
  #### Member Count - End
  
  ## Step 5 (Final checks / Print)
  

}

# Run the application 
shinyApp(ui = ui, server = server)
