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


order_only_list = c('1 No Share - Order Only (even wk)',
                    '1 No Share - Order Only (odd wk)',
                    '1 No Produce - Order Only (even wk)',
                    '1 No Produce - Order Only (odd wk)')



# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Customer Order Printout"),
    
    tabsetPanel(
      tabPanel("Step 1 - Load Data",

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
      sidebarPanel(
        h4(textOutput("step1a")),
        dateInput("date_value","Enter Pickup Date (used in printout)"
                  ,ymd(str_sub(lubridate::now(),1, 10))
                  ,datesdisabled = all_date$all_dates
                  ),
        actionButton("p_file","Step 1a. Process Date Selected"),
        uiOutput("week_value"),
        h4(textOutput("step1b")),
        uiOutput("file_rui"),
        uiOutput("button_1b_rui"),
        h4(textOutput("step1c")),
        uiOutput("button_1c_rui"),
        h3(uiOutput("step1")),
        h3(uiOutput("step1_"))
        
        ),
      mainPanel(
        # tableOutput("mult_pickup_times_tbl"),
        splitLayout(tableOutput("member_start_tbl")
                    ,tableOutput("pickup_time_counts_tbl")),
        dataTableOutput("preview1")
      )
      )
    ),
    tabPanel("Step 2 - Standard Shares",
      sidebarLayout(
        sidebarPanel(
          h4("Step 2a")
        ),
        mainPanel(
        )
      )
    )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  ## Step 1 (Load Data)
  
  output$step1a = renderText(paste0("Step 1a ",emo::ji('black_circle')))
  output$step1b = renderText(paste0("Step 1b ",emo::ji('black_circle')))
  output$step1c = renderText(paste0("Step 1c ",emo::ji('black_circle')))
  
  ### Load Data
  observeEvent(input$p_file,{
    if(input$date_value %in% wed_thu_dates$date_all) {
      date_df_fltr = date_df |> 
        filter(date_all == input$date_value) |> 
        select(season, week, date_all) |> 
        distinct()
      
      output$week_value <- renderUI(
        h6(paste0("Week Number: ",date_df_fltr$week," Season: ",date_df_fltr$season))
      )
      output$file_rui <- renderUI(
        fileInput("file_input", "Choose your file in csv"
                  , multiple = FALSE
                  , accept = c(".csv"))
      )
      output$button_1b_rui <- renderUI(
        actionButton("pp_file","Step 1b. Check File Format"),
      )
      output$step1a = renderText(paste0("Step 1a ",emo::ji('heavy_check_mark')))
    } else {
      showModal(modalDialog(
        title = "Warning",
        "Need to select a Wednesday or Thursday date on Fair Shares calendar.",
        easyClose = TRUE
      ))
      output$step1a = renderText(paste0("Step 1a ",emo::ji('cross_mark')))
    }
    
  })
  
  df_format = reactiveValues(data = NULL)
  member_start = reactiveValues(data = NULL) #use later for member numbers before and after
  df_long_tm = reactiveValues(data = NULL)   #passed on as data set for steps
  
  observeEvent(input$pp_file,{
    if(is.null(input$file_input$datapath)){
      showModal(modalDialog(
        title = "Warning",
        "Please upload a csv file",
        easyClose = TRUE
      ))
      output$step1b = renderText(paste0("Step 1b ",emo::ji('cross_mark')))
    } else{
      input$file_input$datapath %>% 
        read_csv() -> df
      
      if(paste0(colnames(df), collapse = ",") == 'Pickup Site,First Name,Last Name,Order'){
        output$button_1c_rui <- renderUI(
          actionButton("ppp_file","Step 1c. Process File"),
        )
        output$step1b = renderText(paste0("Step 1b ",emo::ji('heavy_check_mark')))
      } else {
        showModal(modalDialog(
          title = "Warning",
          "File is not of correct format. 
          Make sure the file has four columns (Pickup Site, First Name, Last Name, Order)",
          easyClose = TRUE
        ))
        output$step1b = renderText(paste0("Step 1b ",emo::ji('cross_mark')))
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
      output$step1c = renderText(paste0("Step 1c ",emo::ji('cross_mark')))
    } else {
      
      #### Member Count - Start
      df_orig_sh = df_orig  |>  
        left_join(sh_shares |> #bringing in short group name
                    select(group_name, group_name_short) |> 
                    distinct()
                  , by = c('group_name')
        ) |> 
        mutate(group_name = ifelse(is.na(group_name_short) 
                                   | group_name_short == '', group_name, group_name_short)
        ) |>
        select(-group_name_short)
      
      sh_shares_shrt = sh_shares |> 
        mutate(group_name = ifelse(is.na(group_name_short) 
                                   | group_name_short == '', group_name, group_name_short)
        ) |>
        select(-group_name_short)
      
      member_start$data = df_orig_sh |> 
        select(group_name, firstname, lastname) |> 
        distinct() |> 
        group_by(group_name) |> 
        summarise(n_start = n())
      
      member_start_view = member_start$data |> 
        rename('Group Name' = group_name, 'Member Count' = n_start)
      
      # remove("preview1") #struggling to find how to easily remove the first table 
      output$member_start_tbl <- renderTable(member_start_view)
      
      #### Make data long
      
      df_orig_long = df_orig_sh  |> 
        mutate(order = strsplit(as.character(order), ",")) |>
        unnest(order) |> 
        mutate(pickuptime = ifelse(str_detect(order, "Pickup Time"), 2, NA),
               order = trimws(order),
               item_original = order
        ) |> 
        separate(order, into = c('quantity','item'), sep = 'x', extra = "merge") |> 
        mutate(item = ifelse(is.na(item) & !is.na(quantity), quantity, item)
               ,quantity = ifelse(item == quantity, NA, quantity)
               ,group_bool = ifelse(is.na(quantity) & !is.na(item) & !item %in% order_only_list, 1, NA)
               ,rank = ifelse(!is.na(group_bool), group_bool,
                              ifelse(!is.na(pickuptime), pickuptime, 3))
               ,name_long = paste0(lastname,', ',firstname,' [',id,']')
        ) |> 
        select(-group_bool, -pickuptime) |> 
        rbind(df_orig_sh |> #bringing in a new row for each group_name
                select(group_name) |> 
                distinct() |> 
                mutate(firstname = '0'
                       ,lastname = '0'
                       ,quantity = NA
                       ,item = group_name
                       ,id = 0
                       ,item_original = group_name
                       ,rank = -1
                       ,name_long = paste0(lastname,', ',firstname,' [',id,']')
                )
        ) |> 
        rbind(df_orig_sh |> #bringing in a new row for each member name
                select(group_name, firstname, lastname, id) |> 
                distinct() |> 
                mutate(quantity = NA
                       ,item = paste0(lastname,', ',firstname)
                       ,rank = 0
                       ,item_original = item
                       ,name_long = paste0(lastname,', ',firstname,' [',id,']')
                ) |> 
                select(group_name, firstname, lastname, quantity, item, id, item_original, rank, name_long)
        ) |> 
        rbind(df_orig_sh |> #bringing in a new spacer row for each member name
                select(group_name, firstname, lastname, id) |> 
                distinct() |> 
                mutate(quantity = NA
                       ,item = NA
                       ,rank = 4
                       ,item_original = NA
                       ,name_long = paste0(lastname,', ',firstname,' [',id,']')
                ) |> 
                select(group_name, firstname, lastname, quantity, item, id, item_original, rank, name_long)
        ) |> 
        arrange(group_name, lastname, firstname, id, rank) 
      
      #### Pickup Times found
      
      pickup_time = df_orig_long |> 
        filter(rank == 2) |> 
        group_by(Time = item) |> 
        summarise(count = n()) |> 
        ungroup() |> 
        mutate(val = Time) |> 
        separate(val, into = c('q','time','description'), sep = ' ', extra = "merge") |> 
        arrange(description, time, q) 
      
      pickup_time_counts = pickup_time |> 
        select('Pickup Time' = Time, 'Member Count' = count)
      
      df_long_tm_df = df_orig_long |> 
        left_join(
          df_orig_long |> 
            filter(rank == 2) |> 
            select(id, item) |> 
            distinct() |> 
            left_join(pickup_time |> 
                        select(item = Time, description, time)
                      , by = c('item')
            ) |>                                #accounts for if a member accidentally puts in two pickup times
            arrange(id, description, time) |>   # look at first based on description and pickup time
            group_by(id) |> 
            mutate(count = rank(row_number())) |> 
            filter(count == 1) |> 
            rename(time_descr = item) |> 
            select(-count) |> 
            ungroup()
          , by = c('id')
        ) |> 
        mutate(description = ifelse(rank == -1, '.....', description)) |> 
        arrange(group_name, description, time, lastname, firstname, id, rank) 
      
      # mult_pickup_times = df_long_tm_df |> 
      #   filter(rank == 2) |> 
      #   group_by(name_long) |> 
      #   mutate(count = sum(n())) |> 
      #   filter(count > 1) |> 
      #   select(name_long, item, count) |> 
      #   distinct()
      
      df_long_tm_view = df_long_tm_df |>
        select('Group Name' = group_name, 'First Name' = firstname
               , 'Last Name' = lastname, 'Description' = item_original)
      
      df_long_tm$data = df_long_tm_df
      
      # if (dim(mult_pickup_times)[1] > 0) {
      #   output$mult_pickup_times_tbl <- renderTable(mult_pickup_times)
      #   output$pickup_time_counts_tbl <- renderTable(pickup_time_counts)
      #   output$preview1 <- renderDataTable(datatable(df_long_tm_view))
      #   
      # } else {
        output$pickup_time_counts_tbl <- renderTable(pickup_time_counts)
        output$preview1 <- renderDataTable(datatable(df_long_tm_view))
      # }
      
      output$step1c = renderText(paste0("Step 1c ",emo::ji('heavy_check_mark')))
      
      output$step1 <- renderUI(
        renderText(paste0(emo::ji('heavy_check_mark'), " Step 1 is Complete! ",emo::ji('party_popper')))
      )
      output$step1_ <- renderUI(
        renderText(paste0(emo::ji('index_pointing_up'),
                          emo::ji('index_pointing_up'),
                          "   GO TO STEP 2 TAB! "
                          ,emo::ji('index_pointing_up')
                          ,emo::ji('index_pointing_up')))
      )
    }
    
  })

  ####### IN STEP 1 STILL NEED TO PROVIDE DIRECTIONS ON HOW TO GET FARMIGO CSV FILE
  
  #########START ON STEP 2 / TAB 2!!!
  
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
