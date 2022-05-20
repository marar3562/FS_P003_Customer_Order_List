#
# This is the final Dashboard that Fair Shares employees will use to create the customer order printouts
#

library(shiny)
library(tidyverse)
library(DT)
library(lubridate)
library(googlesheets4)
library(openxlsx)      #used for creating/customizing Excel file in easy way 
library(emoji)         #used for emoji creation/reading
library(emo)
library(readxl)        #necessary for writing/reading csv/excel files for test files
library(colourpicker)  #used for color picking in formatting step
library(bslib)         #used for dark theme
library(thematic)

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
) |> arrange(item)
sr = range_read("1xs8TAMrSsJuL_gou4y0DBH3IkaTH0eBn_pdboCGWFTI"
                ,sheet = 'share_rotation'
                ,col_types = 'Diciccc'
)

sr_wed = sr |> 
  select(date) |> 
  distinct()

sr_thu = sr_wed |> 
  mutate(date_all = date + 1)

sr_fri = sr_wed |> 
  mutate(date_all = date + 2)

wed_thu_dates = sr_wed |> 
  mutate(date_all = date) |> 
  rbind(sr_thu) |> 
  rbind(sr_fri) |> 
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

sh_shares_shrt = sh_shares |> 
  mutate(group_name = ifelse(is.na(group_name_short) 
                             | group_name_short == '', group_name, group_name_short)
  ) |>
  select(-group_name_short)

order_only_list = c('1 No Share - Order Only (even wk)',
                    '1 No Share - Order Only (odd wk)',
                    '1 No Produce - Order Only (even wk)',
                    '1 No Produce - Order Only (odd wk)')

food_list_group_name = c('A','H','C','M','U')

group_a_stndrd = NA
group_h_stndrd = NA
group_c_stndrd = NA
group_m_stndrd = NA
group_u_stndrd = NA

group_a_keep = NA
group_h_keep = NA
group_c_keep = NA
group_m_keep = NA
group_u_keep = NA

pot_plant_text = 'Pot Plant'

df_symbol_inputs_milk = data.frame(match_name = c('contains', 'contains', 'contains'
                                                  , 'contains', 'contains', 'contains'
                                                  , 'contains', 'contains', 'contains'
                                                  , 'contains', 'contains', 'contains')
                                   ,text_name = c('Milk - 2%','Milk - Chocolate','Milk - Creamline'
                                                  , 'Milk - Skim','Milk - Whole', '2% MILK'
                                                  , 'Chocolate MILK', 'Creamline MILK', 'Skim MILK'
                                                  , 'Whole MILK', 'Whole Milk - o'
                                                  , '64 Oz. Heavy Cream')
                                   ,freeform_text = c('[*]', '[*]', '[*]', '[*]', '[*]'
                                                      , '[*]', '[*]', '[*]', '[*]', '[*]'
                                                      , '[*]', '[*]')
                                   ,emoji_name = c(emo::ji('cow2'), emo::ji('cow2'), emo::ji('cow2')
                                                   , emo::ji('cow2'), emo::ji('cow2'), emo::ji('cow2')
                                                   , emo::ji('cow2'), emo::ji('cow2'), emo::ji('cow2')
                                                   , emo::ji('cow2'), emo::ji('cow2'), emo::ji('cow2'))
)

df_symbol_inputs_pizza = data.frame(match_name = c('contains', 'contains', 'contains'
                                                   , 'contains', 'contains', 'contains'
                                                   , 'contains', 'contains')
                                    ,text_name = c('Pizza - Cheese','Pizza - Deluxe'
                                                   ,'Pizza - Four Meat','Pizza - Hot Chicken'
                                                   ,'Pizza - Pepperoni','Pizza - Sausage Pepperoni'
                                                   ,'Pizza - Tomato Basil Garlic','Pizza - Veggie')
                                    ,freeform_text = c('[**]', '[**]', '[**]', '[**]'
                                                       , '[**]', '[**]', '[**]', '[**]')
                                    ,emoji_name = c(emo::ji('pizza'), emo::ji('pizza'), emo::ji('pizza')
                                                    , emo::ji('pizza'), emo::ji('pizza'), emo::ji('pizza')
                                                    , emo::ji('pizza'), emo::ji('pizza'))
)

df_symbol_inputs_other = data.frame(match_name = c('contains', 'contains','contains'
                                                   ,'contains','contains','contains')
                                    ,text_name = c('FF Dish - Creole Shrimp & Okra'
                                                   , 'Pork - Andouille links','Raw Milk Aged Cheddar'
                                                   , 'Pickup Time','Schlafly Beer'
                                                   , 'Pre-ordered Plants')
                                    ,freeform_text = c('[**]', '[andouille]', '[1]',NA, NA,NA)
                                    ,emoji_name = c(emo::ji('fried_shrimp'), NA, emo::ji('cheese')
                                                    , emo::ji('clock3'), emo::ji('beer')
                                                    , emo::ji('seedling'))
)
df_symbol_inputs_orig = df_symbol_inputs_milk |> 
  rbind(df_symbol_inputs_pizza) |> 
  rbind(df_symbol_inputs_other) |>  
  mutate(row_num = row_number())

match_name_list = c('contains','equal','ends with','starts with')

emoji_df_name = data.frame(emoji::emoji_name)
emoji_df_name = emoji_df_name |> rownames_to_column()
colnames(emoji_df_name) = c('name', 'emoji')

keywords <- emo::ji_keyword
emojis <- purrr::map_chr(keywords, function(x) paste0(x, collapse = ","))
emoji_df_key = data.frame(keywords = names(emojis)
                   , emojis)
colnames(emoji_df_key) = c('keyword', 'name_list')
emoji_df_key_long = emoji_df_key |>
  mutate(name = strsplit(as.character(name_list), ",")) |>
  unnest(name) |>
  select(keyword, name) |>
  distinct()

emoji_df_comb = emoji_df_name |>
  left_join(emoji_df_key_long
            , by = c('name')
  ) |>
  arrange(name, keyword) |>
  group_by(name, emoji) |>
  mutate(keyword_list = paste0(keyword, collapse = ",")) |>
  ungroup() |>
  select(-keyword) |>
  distinct()

fs_emoji_df = emoji_df_comb |> 
  filter(str_detect(keyword_list,'beer')
         | str_detect(keyword_list,'plant')
         | str_detect(keyword_list,'pizza')
         | str_detect(keyword_list,'food')
         | str_detect(keyword_list,'vege')
         | str_detect(keyword_list,'cow')
         | str_detect(keyword_list,'chicken')
         | str_detect(keyword_list,'fish')
         | str_detect(keyword_list,'shrimp')
         | str_detect(keyword_list,'cheese')
  )

emoji_name_list = fs_emoji_df$emoji


format_list = c('italic','bold')

df_format_inputs_orig = data.frame(match_name = c('equal', 'contains')
                                   ,text_name = c('STANDARD SHARE ITEMS', 'Pickup Time')
                                   ,format_name = c('bold', 'italic')
                                   ,color_name = c(NA, NA)
                                  ) |>  
  mutate(row_num = row_number())


##Theme Setup
# Setting Theme (https://shiny.rstudio.com/app-stories/weather-lookup-bslib.html)
my_theme <- bs_theme(bootswatch = "slate",
                     base_font = font_google("Roboto"))
# Let thematic know to update the fonts, too
thematic_shiny(font = "auto")

# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = my_theme,

    # Application title
    # titlePanel("Customer Order Printout"),
    
    # Application title
    div(id = "page-top",
        fluidRow(img(src="logo.png", height="5%", width="5%")
                 ,column(3, radioButtons("current_theme", "App Theme:", c("Dark" = "slate", "Light" = "flatly"), inline = TRUE))
        )
    ),
    div(
      id = "app-title",
      titlePanel(title="Customer Order Printouts"
                 ,tags$head(tags$link(rel = "icon", type = "image/png", href = "logo.png"),
                            tags$title("FS Customer Orders"))
      )
    ),
    
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
        uiOutput("help_1b_rui"),
        uiOutput("file_rui"),
        uiOutput("example_upload"),
        uiOutput("example_share_list"),
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
          h4(textOutput("step2a")),
          selectInput("group_2a","Select Food List Group:"
                      , sort(food_list_group_name)
                      , multiple = FALSE),
          uiOutput("members_2a_rui"),
          selectInput("keep_2a","Leave Member in Final Printout?"
                      , c('Keep','Remove')
                      , multiple = FALSE),
          uiOutput("update_table_2a_rui"),
          uiOutput("remove_member_2a_rui"),
          uiOutput("item_list_2a_rui"),
          uiOutput("add_item_2a_rui"),
          uiOutput("item_list_remove_2a_rui"),
          uiOutput("remove_item_2a_rui"),
          uiOutput("process_changes_2a_rui"),
          h3(uiOutput("step2")),
          h3(uiOutput("step2_"))
          
        ),
        mainPanel(
          splitLayout(tableOutput("preview2_name")
                      ,dataTableOutput("preview2_item")),
          dataTableOutput("standard_members"),
          h4(uiOutput("filter_2a_test_rui")),
          splitLayout(uiOutput("filter1_item_2a_rui")
                      ,uiOutput("filter2_item_2a_rui")
                      ,uiOutput("filter3_item_2a_rui")),
          uiOutput("preview2a")
        )
      )
    ),
    tabPanel("Step 3 - Delete Items",
             sidebarLayout(
               sidebarPanel(
                 h4(textOutput("step3a")),
                 uiOutput("delete_gs_list_rui"),
                 actionButton("remove_perm_item","Remove Permanently Selected Item"),
                 uiOutput("new_item_3a_rui"),
                 uiOutput("new_item_temp_3a_rui"),
                 uiOutput("new_item_perm_3a_rui"),
                 uiOutput("remove_item_temp_select_3a_rui"),
                 uiOutput("remove_item_temp_3a_rui"),
                 uiOutput("process_3a_rui"),
                 h4(uiOutput("download_plants_text_rui")),
                 uiOutput("download_plants_rui"),
                 h4(uiOutput("step3b")),
                 h5(uiOutput("step3b_text")),
                 uiOutput("step3b_process"),
                 h3(uiOutput("step3")),
                 h3(uiOutput("step3_"))
                 
               ),
               mainPanel(
                 splitLayout(dataTableOutput("delete_list_view")
                             ,dataTableOutput("delete_list_temp_view")
                             ),
                 splitLayout(dataTableOutput("delete_list_table")
                             ,dataTableOutput("delete_list_temp_table")
                             ),
                 splitLayout(dataTableOutput("delete_plant_table")
                             ,dataTableOutput("order_only_table")
                             ),
                 dataTableOutput("preview3")
               )
             )
    ),
    tabPanel("Step 4 - Item Styling",
             sidebarLayout(
               sidebarPanel(
                 h4(textOutput("step4a")),
                 uiOutput("new_item_4a_match_rui"),
                 uiOutput("new_item_4a_text_search_rui"),
                 uiOutput("new_item_4a_text_add_rui"),
                 uiOutput("emoji_help_4a_rui"),
                 uiOutput("new_item_4a_emoji_rui"),
                 uiOutput("new_item_4a_add_rui"),
                 uiOutput("new_item_4a_row_rui"),
                 uiOutput("new_item_4a_remove_rui"),
                 uiOutput("process_4a_rui"),
                 h4(uiOutput("step4b")),
                 h5(uiOutput("step4b_text")),
                 uiOutput("step4b_process"),
                 h3(uiOutput("step4")),
                 h3(uiOutput("step4_"))
                 
               ),
               mainPanel(
                 dataTableOutput("preview4a"),
                 dataTableOutput("preview4b")
               )
             )
             
    ),
    tabPanel("Step 5 - Check In Sheet",
             sidebarLayout(
               sidebarPanel(
                 h4(textOutput("step5a")),
                 uiOutput("new_item_5a_match_rui"),
                 uiOutput("new_item_5a_text_search_rui"),
                 uiOutput("new_item_5a_format_rui"),
                 uiOutput("color_help_5a_rui"),
                 uiOutput("new_item_5a_color_rui"),
                 uiOutput("new_item_5a_add_rui"),
                 uiOutput("new_item_5a_row_rui"),
                 uiOutput("new_item_5a_remove_rui"),
                 uiOutput("process_5a_rui"),
                 uiOutput("download_5a_excel"),
                 uiOutput("download_5a_openo"),
                 
                 #h4(uiOutput("step5b")),
                 h5(uiOutput("step5b_text")),
                 uiOutput("step5b_process"),
                 h3(uiOutput("step5")),
                 h3(uiOutput("step5_"))
                 
               ),
               mainPanel(
                 dataTableOutput("preview5a"),
                 tableOutput("preview5b")
               )
             )
    )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  ############## Step 1 (Load Data)  ##############
  #####################################################################
  ###### ENHANCEMENT LIST FOR STEP:
    ##Play with Side bar % if other steps also change this to fit charts better
  
  output$step1a = renderText(paste0("Step 1a ",emo::ji('black_circle')))
  output$step1b = renderText(paste0("Step 1b ",emo::ji('black_circle')))
  output$step1c = renderText(paste0("Step 1c ",emo::ji('black_circle')))
  
  week_number = reactiveValues(data = NA)
  season_number = reactiveValues(data = NA)
  
  ### Load Data
  #####################################################################
  observeEvent(input$p_file,{
    if(input$date_value %in% wed_thu_dates$date_all) {
      date_df_fltr = date_df |> 
        filter(date_all == input$date_value) |> 
        select(season, week, date_all) |> 
        distinct() |> 
        mutate(week = ifelse(str_length(week) == 1, paste0('0',week), week))
      
      week_number$data = date_df_fltr$week
      season_number$data = date_df_fltr$season
      
      output$week_value <- renderUI(
        h6(paste0("Week Number: ",week_number$data," Season: ",season_number$data))
      )
      output$help_1b_rui <- renderUI(
        actionButton("help_1b",paste0(emo::ji('information')," Farmigo File Instructions")),
      )
      output$file_rui <- renderUI(
        fileInput("file_input", "Choose your file in csv"
                  , multiple = FALSE
                  , accept = c(".csv"))
      )
      
      output$example_upload <- renderUI({
        output$download_example_upload = downloadHandler(
          filename = function() {
            paste0('example_upload.csv')
          },
          content = function(file) {
            test_upload = read.csv('Member Pick-up Details-shiny_test.csv'
                                   )
            colnames(test_upload) = c('Pickup Site','First Name','Last Name','Order')
            readr::write_excel_csv(test_upload, file
                      # , col_names = c('Pickup Site','First Name','Last Name','Order')
                      )
          }
        )
        
        downloadButton("download_example_upload", label = "Download Example Upload", class = "btn-block")
      })
      
      output$example_share_list <- renderUI({
        output$download_example_share_list = downloadHandler(
          filename = function() {
            paste0('example_share_list.xlsx')
          },
          content = function(file) {
            test_shares = read_excel('Member Pick-up Details-shiny_test.xlsx', 'shares')
            test_members = read_excel('Member Pick-up Details-shiny_test.xlsx', 'members')
            wb_share <- createWorkbook(title = 'example_share_list')
            addWorksheet(wb_share, "standard_shares")
            writeData(wb_share, "standard_shares", test_shares)
            addWorksheet(wb_share, "member_orders")
            writeData(wb_share, "member_orders", test_members)
            
            saveWorkbook(wb_share, file, overwrite = TRUE)
          }
        )
        
        downloadButton("download_example_share_list", label = "Download Example Share", class = "btn-block")
      })
      
      
      
      output$button_1b_rui <- renderUI(
        actionButton("pp_file","Step 1b. Check File Format"),
      )
      output$step1a = renderText(paste0("Step 1a ",emo::ji('heavy_check_mark')))
    } else {
      showModal(modalDialog(
        title = paste0(emo::ji('stop_sign')," Error"),
        "Need to select a Wednesday or Thursday date on Fair Shares calendar.",
        easyClose = TRUE
      ))
      output$step1a = renderText(paste0("Step 1a ",emo::ji('cross_mark')))
    }
    
  })
  
  observeEvent(input$help_1b,{
    showModal(modalDialog(
      title = paste0(emo::ji('information')," Help"),
      HTML("To Download the Farmigo Customer CSV File: <br>
      1. Navigate to Farmigo website (<a href='https://csa.farmigo.com/dashboard/fairsharesccsa/members'>Farmigo Weblink</a>) <br>
      2. Click on 'Reports' <br>
      3. Click on 'Member Pick Up Details' <br>
      4. Select the 'Delivery date' of interest <br>
      5. All 'Routes' should be selected <br>
      6. 'Store Orders and Subscriptions' should be selected under 'Items Type'<br>
      7. Do NOT include 'Contact details'<br>
      8. Click 'Download CSV' <br>
      9. Once downloaded you can click 'Dismiss' on this message and 'Browse' for this new file in the dashboard
      "),
      easyClose = TRUE
    ))
  })
  
  df_format = reactiveValues(data = NULL)
  member_start = reactiveValues(data = NULL) #use later for member numbers before and after
  df_long_tm = reactiveValues(data = NULL)   #passed on as data set for steps
  
  #used in step 2
  df_stndrds = reactiveValues(data = data.frame(food_list_group_name
                                                , name_long = c(group_a_stndrd, group_h_stndrd, group_c_stndrd
                                                                , group_m_stndrd, group_u_stndrd)
                                                , keep_remove = c(group_a_keep, group_h_keep, group_c_keep
                                                                  , group_m_keep, group_u_keep)) |> 
                                mutate(name_long = as.character(name_long)
                                       ,keep_remove = as.logical(keep_remove))
  )
  
  df_stndrds_item = reactiveValues(data = data.frame(food_list_group_name
                                              , item_original = c(NA, NA, NA, NA, NA)) |> 
                                              filter(!is.na(item_original))
  )
  
  
  observeEvent(input$pp_file,{
    if(is.null(input$file_input$datapath)){
      showModal(modalDialog(
        title = paste0(emo::ji('stop_sign')," Error"),
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
          title = paste0(emo::ji('stop_sign')," Error"),
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
    #####################################################################
    
    group_check = df_orig |>
      mutate(check = ifelse(group_name %in% (sh_shares)$group_name, TRUE, FALSE)) |>
      filter(check == FALSE) |> 
      select(-check, -order, -id) 

    if (dim(group_check |> select(group_name) |> distinct())[1] > 0) {
      output$preview1 <- renderDataTable(datatable(group_check))
      showModal(modalDialog(
        title = paste0(emo::ji('stop_sign')," Error"),
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
      #####################################################################
      
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
      #####################################################################
      
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
      #####################################################################
      
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
      
      df_stndrds_vw = df_stndrds$data |> 
        arrange(food_list_group_name) |> 
        rename('Group' = food_list_group_name
               , 'Name and Id' = name_long
               , 'In Printout' = keep_remove)
      output$preview2_name <- renderTable(df_stndrds_vw)
      
      df_stndrds_item_vw = df_stndrds_item$data |> 
        arrange(food_list_group_name) |> 
        rename('Group' = food_list_group_name
               , 'Item Description' = item_original)
      output$preview2_item <- renderDataTable(datatable(df_stndrds_item_vw
                                                        , options = list(dom = 'tipr'
                                                                         , pageLength = 5)))
      member_react()

    }
    
  })
  
  ############## Step 2 (Standard Shares)  ##############
  #############################################################################################
  ###### ENHANCEMENT LIST FOR STEP:
    ##NEED TO PLAY WITH THE FORMATTING AND POTENTIAL NEW EMOJIS
    ## TAB OVER THE FILTERS AND ADD MEMBER BUTTONS
    ## ADD AN EMOJI TO THE ADD MEMBER ACTION BUTTON
    ## PLAY WITH MAIN PAGE FORMATTING SO ALL CHARTS AND FILTER LOOK NICE
  
  
  ## Step 2 - Inputs
  
  ### Standard Share Members - Setup / Search
  #####################################################################
  
  output$step2a = renderText(paste0("Step 2a ",emo::ji('black_circle')))
   
  member_react = reactive({

    output$members_2a_rui <- renderUI({
      member_list = df_long_tm$data |> 
        filter(id > 0) |> 
        inner_join(sh_shares_shrt |> 
                     filter(food_list_group_name %in% input$group_2a) |> 
                     select(group_name, food_list_group_name)
                   , by = c('group_name')
        ) |> 
        select(name_long) |> 
        distinct() |> 
        arrange(name_long) |> 
        pull()
      
      selectInput("member_2a","Select Member Name:", member_list, multiple = FALSE)
      
    })
    
    output$remove_member_2a_rui <- renderUI({
      actionButton("remove_member_2a","Clear Member Name")
    })
    
    output$item_list_2a_rui <- renderUI({
      item_list_2a = df_long_tm$data |> 
        filter(rank == 3) |> 
        inner_join(sh_shares_shrt |> 
                     filter(food_list_group_name %in% input$group_2a) |> 
                     select(group_name, food_list_group_name)
                   , by = c('group_name')
        ) |> 
        select(item_original, item) |> 
        distinct() |> 
        arrange(item) |> 
        select(item_original) |> 
        pull()
      
      selectInput("item_2a","Select Item:"
                  , c(NA, item_list_2a)
                  , multiple = FALSE)
    })
    
    output$add_item_2a_rui <- renderUI({
      actionButton("add_item_2a","Add Item to List")
    })
    
    output$item_list_remove_2a_rui <- renderUI({
      item_remove = df_stndrds_item$data |> 
        mutate(row_num = row_number()) |> 
        select(row_num) |> 
        distinct() |> 
        pull()
      selectInput("item_list_remove_2a","Select Item Row Number:", c(NA, item_remove), multiple = FALSE)
    })
    
    output$remove_item_2a_rui <- renderUI({
      actionButton("remove_item_2a","Remove Item from List")
    })

    output$filter_2a_test_rui <- renderUI({
      renderText("Search member's carts which containing text (not case sensitive):")
    })
    
    output$filter1_item_2a_rui <- renderUI({ 
      textInput("standard_item_search1"
                , "1. Search term:"
                , value = '')
    })
    
    output$filter2_item_2a_rui <- renderUI({ 
      textInput("standard_item_search2"
                , "2. Search term:"
                , value = '')
    })
    
    output$filter3_item_2a_rui <- renderUI({ 
      textInput("standard_item_search3"
                , "3. Search term:"
                , value = '')
    })
    
    output$preview2a <- renderUI({
     filter_df = df_long_tm$data |>
        filter(id > 0) |>
        inner_join(sh_shares_shrt |>
                     filter(food_list_group_name %in% input$group_2a) |>
                     select(group_name, food_list_group_name)
                   , by = c('group_name')
        )
     
     if (input$standard_item_search1 != '' 
          & input$standard_item_search2 != ''
          & input$standard_item_search3 != '') {
       standard_share_search = filter_df |>
         inner_join(filter_df |> 
                      filter(str_detect(tolower(item_original), tolower(input$standard_item_search1))) |> 
                      select(name_long) |> 
                      distinct()
                    , by = c('name_long')
         ) |> 
         inner_join(filter_df |> 
                      filter(str_detect(tolower(item_original), tolower(input$standard_item_search2))) |> 
                      select(name_long) |> 
                      distinct()
                    , by = c('name_long')
         ) |> 
         inner_join(filter_df |> 
                      filter(str_detect(tolower(item_original), tolower(input$standard_item_search3))) |> 
                      select(name_long) |> 
                      distinct()
                    , by = c('name_long')
         ) |>
         select('Group Name' = group_name
                , 'Name and Id' = name_long
                , Description = item_original)
     } else if (input$standard_item_search1 != '' 
                & input$standard_item_search2 != '') {
       standard_share_search = filter_df |>
         inner_join(filter_df |> 
                      filter(str_detect(tolower(item_original), tolower(input$standard_item_search1))) |> 
                      select(name_long) |> 
                      distinct()
                    , by = c('name_long')
         ) |> 
         inner_join(filter_df |> 
                      filter(str_detect(tolower(item_original), tolower(input$standard_item_search2))) |> 
                      select(name_long) |> 
                      distinct()
                    , by = c('name_long')
         ) |>
         select('Group Name' = group_name
                , 'Name and Id' = name_long
                , Description = item_original)
     } else if (input$standard_item_search1 != '' 
                & input$standard_item_search3 != '') {
       standard_share_search = filter_df |>
         inner_join(filter_df |> 
                      filter(str_detect(tolower(item_original), tolower(input$standard_item_search1))) |> 
                      select(name_long) |> 
                      distinct()
                    , by = c('name_long')
         ) |> 
         inner_join(filter_df |> 
                      filter(str_detect(tolower(item_original), tolower(input$standard_item_search3))) |> 
                      select(name_long) |> 
                      distinct()
                    , by = c('name_long')
         ) |>
         select('Group Name' = group_name
                , 'Name and Id' = name_long
                , Description = item_original)
     } else if (input$standard_item_search2 != ''
                & input$standard_item_search3 != '') {
       standard_share_search = filter_df |>
         inner_join(filter_df |> 
                      filter(str_detect(tolower(item_original), tolower(input$standard_item_search2))) |> 
                      select(name_long) |> 
                      distinct()
                    , by = c('name_long')
         ) |> 
         inner_join(filter_df |> 
                      filter(str_detect(tolower(item_original), tolower(input$standard_item_search3))) |> 
                      select(name_long) |> 
                      distinct()
                    , by = c('name_long')
         ) |>
         select('Group Name' = group_name
                , 'Name and Id' = name_long
                , Description = item_original)
     } else if (input$standard_item_search1 != '') {
       standard_share_search = filter_df |>
         inner_join(filter_df |> 
                      filter(str_detect(tolower(item_original), tolower(input$standard_item_search1))) |> 
                      select(name_long) |> 
                      distinct()
                    , by = c('name_long')
         ) |> 
         select('Group Name' = group_name
                , 'Name and Id' = name_long
                , Description = item_original)
     } else if (input$standard_item_search2 != '') {
       standard_share_search = filter_df |>
         inner_join(filter_df |> 
                      filter(str_detect(tolower(item_original), tolower(input$standard_item_search2))) |> 
                      select(name_long) |> 
                      distinct()
                    , by = c('name_long')
         ) |> 
         select('Group Name' = group_name
                , 'Name and Id' = name_long
                , Description = item_original)
     } else if (input$standard_item_search3 != '') {
       standard_share_search = filter_df |>
         inner_join(filter_df |> 
                      filter(str_detect(tolower(item_original), tolower(input$standard_item_search3))) |> 
                      select(name_long) |> 
                      distinct()
                    , by = c('name_long')
         ) |> 
         select('Group Name' = group_name
                , 'Name and Id' = name_long
                , Description = item_original)
     } else {
       standard_share_search = filter_df |>
         select('Group Name' = group_name
                , 'Name and Id' = name_long
                , Description = item_original)
     }
    
      renderDataTable(datatable(standard_share_search, options = list(dom = 'ltipr', pageLength = 100)))
    })
    
    output$update_table_2a_rui <- renderUI({ 
      actionButton("add_member_2a", "Add Member to Standard Item List")
    })
    output$process_changes_2a_rui <- renderUI({
      actionButton("process_2a", "2a. Process Member List")
    })
  
  }) 
  
  ### Standard Share Members - Adding members to the dataframe
  #####################################################################
  observeEvent(input$add_member_2a,{
    df_stndrds$data = df_stndrds$data |> 
      filter(food_list_group_name == input$group_2a) |> 
      mutate(name_long = input$member_2a
             , keep_remove = ifelse(input$keep_2a == 'Keep', TRUE, FALSE)
             ) |> 
      rbind(df_stndrds$data |> 
              filter(food_list_group_name != input$group_2a)
            )
    
    df_stndrds_update = df_stndrds$data |> 
      arrange(food_list_group_name) |> 
      rename('Group' = food_list_group_name
             , 'Name and Id' = name_long
             , 'In Printout' = keep_remove)
      
    output$preview2_name <- renderTable(df_stndrds_update)
  })
  
  ### Standard Share Members - Remove members from the dataframe
  #####################################################################
  observeEvent(input$remove_member_2a,{
    df_stndrds$data = df_stndrds$data |> 
      filter(food_list_group_name == input$group_2a) |> 
      mutate(name_long = NA
             , keep_remove = NA
      ) |> 
      rbind(df_stndrds$data |> 
              filter(food_list_group_name != input$group_2a)
      )
    
    df_stndrds_update = df_stndrds$data |> 
      arrange(food_list_group_name) |> 
      rename('Group' = food_list_group_name
             , 'Name and Id' = name_long
             , 'In Printout' = keep_remove)
    
    output$preview2_name <- renderTable(df_stndrds_update)
  })
  
  ### Standard Share Items - Adding items to the dataframe
  #####################################################################
  observeEvent(input$add_item_2a,{
    if (input$item_2a == 'NA') {
      showModal(modalDialog(
        title = paste0(emo::ji('stop_sign')," Error"),
        HTML("Before adding a new Standard Share Item the <b>Select Item</b> field needs to not show as <u>NA</u>."),
        easyClose = TRUE
      ))
    } else {
      df_stndrds_item$data = data.frame(food_list_group_name = c(input$group_2a)
                                        ,item_original = c(input$item_2a)
      ) |> 
        rbind(df_stndrds_item$data)
      
      df_stndrds_item_update = df_stndrds_item$data |>
        arrange(food_list_group_name, item_original) |>
        rename('Group' = food_list_group_name
               , 'Item Description' = item_original)
      
      output$preview2_item <- renderDataTable(datatable(df_stndrds_item_update
                                                        , options = list(dom = 'tipr'
                                                                         , pageLength = 5)))
      
      item_remove = df_stndrds_item$data |> 
        mutate(row_num = row_number()) |> 
        select(row_num) |> 
        distinct() |> 
        pull()
      selectInput("item_list_remove_2a","Select Item Row Number:", c(NA, item_remove), multiple = FALSE)
    }
    
  })
  
  ### Standard Share Items - Remove items from the dataframe
  #####################################################################
  observeEvent(input$remove_item_2a,{
    if (input$item_list_remove_2a == 'NA') {
      showModal(modalDialog(
        title = paste0(emo::ji('stop_sign')," Error"),
        HTML("Before remvoing a Standard Share Item the <b>Select Item Row Number</b> field needs to not show as <u>NA</u>."),
        easyClose = TRUE
      ))
    } else {
      df_stndrds_item$data = df_stndrds_item$data |> 
        mutate(row_num = row_number()) |> 
        filter(row_num != input$item_list_remove_2a) |> 
        select(-row_num)
      
      df_stndrds_item_update = df_stndrds_item$data |>
        arrange(food_list_group_name, item_original) |>
        rename('Group' = food_list_group_name
               , 'Item Description' = item_original)
      
      output$preview2_item <- renderDataTable(datatable(df_stndrds_item_update
                                                        , options = list(dom = 'tipr'
                                                                         , pageLength = 5)))
      
      item_remove = df_stndrds_item$data |> 
        mutate(row_num = row_number()) |> 
        select(row_num) |> 
        distinct() |> 
        pull()
      selectInput("item_list_remove_2a","Select Item Row Number:", c(NA, item_remove), multiple = FALSE)
    }
    
  })
  
  ### Standard Share Members - Action (Process results)
  #####################################################################
  df_long_def = reactiveValues(data = NULL)   #passed on as data set for steps
  
  observeEvent(input$process_2a,{
    
    df_stndrd_comb = df_stndrds$data |> 
      mutate(item_original = NA) |> 
      rbind(df_stndrds_item$data |> 
              mutate(name_long = NA
                     , keep_remove = NA) |> 
              select(food_list_group_name
                     , name_long
                     , keep_remove
                     , item_original)
      )
    
    if (dim(df_stndrd_comb
        |> mutate(name_cnt = ifelse(!is.na(name_long), 1, 0)
                  , item_cnt = ifelse(!is.na(item_original), 1, 0)) 
        |> group_by(food_list_group_name)
        |> summarise(name_cnt = sum(name_cnt)
                     , item_cnt = sum(item_cnt))
        |> filter(name_cnt > 0 & item_cnt > 0)
    )[1] > 0) {
      showModal(modalDialog(
        title = paste0(emo::ji('stop_sign')," Error"),
        HTML("Cannot move on until either:  <br>
             1. Each Group only contains a single member name and NO Item Description for that same group <br>
             2. Items for that Group are populated and NO Member name is associated as the standard share"),
        easyClose = TRUE
      ))
    } else {
      if (dim(df_stndrd_comb 
              |> filter(!is.na(name_long) | !is.na(item_original)) 
              |> select(food_list_group_name)
              |> distinct()
              )[1] < length(food_list_group_name) ) {
        showModal(modalDialog(
          title = paste0(emo::ji('warning')," Warning"),
          HTML("There is at least one group that does not have a member selected for the standard share item list.<br> 
          This list will be used to reduce customer orders who have all standard share items to a single line in the printout sheet. <br>
          You can either:<br>
          1. Leave this if its expected. There may not be a member that has all standard share items with no additions for the week.<br>
          2. Add a member to the item standard share list above to reduce printout results.
          "),
          easyClose = TRUE
        ))
      } 
      
      #standard share item list
      df_stndrds_mem_item = df_long_tm$data |> 
        inner_join(df_stndrd_comb |> 
                     filter(!is.na(name_long)) |> 
                     select(-item_original)
                   , by = c('name_long')
        ) |> 
        filter(rank == 3) 
      
      #bringing in the Food List Group Name
      df_stndrds_item_grp = df_stndrds_mem_item |>
        select(food_list_group_name, item) |>
        rbind(df_stndrd_comb |>
                filter(!is.na(item_original)) |>
                mutate(order = item_original) |>
                separate(order, into = c('quantity','item'), sep = 'x', extra = "merge") |>
                select(food_list_group_name, item)
              ) |>
        distinct() |> 
        left_join(sh_shares_shrt |>
                    select(group_name, food_list_group_name)
                  , by = c('food_list_group_name')
        ) |>
        select(-food_list_group_name) |>
        mutate(standard_item = 1)

      stndrd_customers_all = df_long_tm$data |>
        left_join(df_stndrds_mem_item |>
                    select(food_list_group_name, item_original) |>
                    rbind(df_stndrd_comb |>
                            filter(!is.na(item_original)) |>
                            select(food_list_group_name, item_original)
                    ) |>
                    distinct()|>
                    group_by(food_list_group_name) |>
                    mutate(rank_v = rank(row_number()),
                           stndrd_rows= max(rank_v)
                    ) |>
                    ungroup() |>
                    left_join(sh_shares_shrt |>
                                select(group_name, food_list_group_name)
                              , by = c('food_list_group_name')
                    ) |>
                    select( -rank_v, -food_list_group_name)
                  , by = c('group_name', 'item_original')
        ) |>
        filter(rank == 3) |>
        select(id, name_long, group_name, item_original, stndrd_rows) |>
        distinct() |>
        group_by(id) |>
        mutate(rank_v = sum(ifelse(!is.na(stndrd_rows), 1, 0)),
               customer_rows = max(rank_v),
               rank_n_full = ifelse(!is.na(stndrd_rows), rank(row_number()), 0),
               customer_n_full_rows = max(rank_n_full),
               n_full_rows = sum(ifelse(rank_n_full == 0, 1, 0))
        ) |>
        ungroup()

      #100% match to the standard share list (Pickup Time does not count)
      stndrd_customers_full_match = stndrd_customers_all |>
        filter(stndrd_rows == customer_rows & n_full_rows == 0) |>
        left_join(df_stndrds$data |>
                    filter(!is.na(name_long))
                  , by = c('name_long')
        ) |>
        mutate(keep_remove = ifelse(is.na(keep_remove), TRUE, keep_remove)) |>
        select(id, keep_remove) |>
        distinct()

      #Some members have the standard share but with additional items on top (milk, etc.)
      stndrd_customers_match_w_extras = stndrd_customers_all |>
        filter(stndrd_rows == (customer_n_full_rows - n_full_rows) & n_full_rows > 0) |>
        select(id) |>
        distinct() |>
        left_join(stndrd_customers_all |>
                    filter(is.na(stndrd_rows)) |>
                    select(id, item_original)
                  , by = c('id')
        ) |>
        mutate(keep_remove =  TRUE) |>
        select(id, item_original, keep_remove) |>
        distinct()

      #combine all data to output to be used in next step
      df_long_def$data = df_long_tm$data |> #bring in stndrd trading member names
        filter(id %in% (stndrd_customers_full_match |> filter(keep_remove == TRUE))$id
               | id %in% (stndrd_customers_match_w_extras |> filter(keep_remove == TRUE))$id
        ) |>
        filter(rank <= 2) |>
        select(-item_original) |>
        rbind(df_long_tm$data |>  #bring in stndrd trading item row
                filter(id %in% (stndrd_customers_full_match |> filter(keep_remove == TRUE))$id
                       | id %in% (stndrd_customers_match_w_extras |> filter(keep_remove == TRUE))$id
                ) |>
                filter(rank >= 3) |>
                mutate(quantity = NA,
                       item = ifelse(rank == 3, 'STANDARD SHARE ITEMS', item)
                ) |>
                select(-item_original) |>
                distinct()
        ) |>
        rbind(df_long_tm$data |>  #bring in all members not in stndrd member list
                select(-item_original) |>
                filter(!id %in% (stndrd_customers_full_match |> filter(keep_remove == TRUE))$id
                       & !id %in% (stndrd_customers_match_w_extras |> filter(keep_remove == TRUE))$id
                )
        ) |>
        rbind(df_long_tm$data |>  #bring in items that have a stndrd item match but have extra sales
                inner_join(stndrd_customers_match_w_extras |>
                             select(-keep_remove)
                           , by = c('id','item_original')
                ) |>
                select(-item_original)
        ) |>
        arrange(group_name, description, time, lastname, firstname, id, rank) |>
        left_join(df_stndrds_item_grp
                  , by = c('item','group_name'))

      standard_summary = df_long_def$data |>
        inner_join(stndrd_customers_match_w_extras |>
                     select(-item_original) |>
                     mutate(extras = TRUE) |>
                     distinct() |>
                     rbind(stndrd_customers_full_match |>
                             mutate(extras = FALSE))
                   , by = c('id')
        ) |>
        select('Group Name' = group_name
               , 'Name and Id' = name_long
               , 'In Printout' = keep_remove
               , 'Extra Items' = extras) |>
        distinct()

      output$standard_members <- renderDataTable(datatable(standard_summary
                                                           , options = list(dom = 'ltip'
                                                                            , pageLength = 5
                                                                            , lengthMenu = c(5, 10, 15, 20)
                                                                            )
                                                           ))

      output$step2a = renderText(paste0("Step 2a ",emo::ji('heavy_check_mark')))

      output$step2 <- renderUI(
        renderText(paste0(emo::ji('heavy_check_mark'), " Step 2 is Complete! ",emo::ji('party_popper')))
      )
      output$step2_ <- renderUI(
        renderText(paste0(emo::ji('index_pointing_up'),
                          emo::ji('index_pointing_up'),
                          "   GO TO STEP 3 TAB! "
                          ,emo::ji('index_pointing_up')
                          ,emo::ji('index_pointing_up')))
      )

      #all below are used in Step 3
      output$new_item_3a_rui <- renderUI(
        selectInput("new_item_3a", "Select Item to Add to Delete List:"
                    ,c(NA, df_long_def$data |>
                         filter(rank == 3) |>
                         filter(!str_detect(item, pot_plant_text)) |>
                         select(item) |>
                         distinct() |>
                         pull())
                    )
      )

      output$new_item_temp_3a_rui <- renderUI(
        actionButton("add_temp_item","Add Item Temporarily")
      )

      output$new_item_perm_3a_rui <- renderUI(
        actionButton("add_perm_item","Add Item Permanently")
      )

      output$process_3a_rui <- renderUI(
        actionButton("process_3a","Step 3a. Process Delete List")
      )
      
    }
  })
  
  ############## Step 3 (Delete Items)  ##############
  #############################################################################################
  ###### ENHANCEMENT LIST FOR STEP:
    ##Formatting to be able to view all dataframes easier
  
  output$step3a = renderText(paste0("Step 3a ",emo::ji('black_circle')))
  
  delete_list = reactiveValues(data = sh_delete$item)
  delete_df = reactiveValues(data = sh_delete)
  df_long_del = reactiveValues(data = NA)
  
  output$delete_gs_list_rui <- renderUI({
    
    selectInput("delete_list_2a","Select Permanent Item to Remove:"
                                      , c(NA, delete_list$data)
                                      , multiple = FALSE)
  })
  
  output$delete_list_view = renderDataTable(datatable(delete_df$data
                                                      , options = list(dom = 'ltip'
                                                                       , pageLength = 5
                                                                      , lengthMenu = c(5, 10, 15, 20) 
                                                                      )))
  
  ### Delete - Remove A Permament Item from Google Sheets List
  #####################################################################
  observeEvent(input$remove_perm_item,{
    
    if (input$delete_list_2a == 'NA') {
      showModal(modalDialog(
        title = paste0(emo::ji('warning')," Warning"),
        HTML("A non-NA item needs to be selected for this to remove an item permanently.
        "),
        easyClose = TRUE
      ))
    } else {
      range_delete('1pEEGA3mawQWmgDtIjJmpBMPws8_-EyEqPtQlucGVlNQ'
                   , sheet = 'delete'
                   , range = "A:A")
      range_write('1pEEGA3mawQWmgDtIjJmpBMPws8_-EyEqPtQlucGVlNQ'
                 , data = delete_df$data |> filter(!is.na(item)) |> filter(item != input$delete_list_2a) |> distinct()
                 , sheet = 'delete'
                 , col_names = TRUE
                 , range = "A:A"
      ) 
      Sys.sleep(2)
      sh_delete_new = range_read('1pEEGA3mawQWmgDtIjJmpBMPws8_-EyEqPtQlucGVlNQ'
                             ,sheet = 'delete'
                             ,col_types = 'c'
      ) |> arrange(item)
      
      output$delete_list_view = renderDataTable(datatable(sh_delete_new
                                                          , options = list(dom = 'ltip'
                                                                           , pageLength = 5
                                                                           , lengthMenu = c(5, 10, 15, 20) 
                                                                           )))
      
      output$delete_gs_list_rui <- renderUI({
        
        selectInput("delete_list_2a","Select Permanent Item to Remove:"
                    , c(NA, sh_delete_new$item)
                    , multiple = FALSE)
      })
      delete_list$data = sh_delete_new$item
      delete_df$data = sh_delete_new
    }
    })
  
  ### Delete - Add/Remove a Temporary Item to List
  #####################################################################
  delete_temp = reactiveValues(data = data.frame(item = c(NA)))
  
  observeEvent(input$add_temp_item,{
    
    if (input$new_item_3a == 'NA') {
      showModal(modalDialog(
        title = paste0(emo::ji('warning')," Warning"),
        HTML("A non-NA item needs to be selected for this to add an item temporarily.
        "),
        easyClose = TRUE
      ))
    } else {
      delete_temp$data = delete_temp$data |> 
        rbind(data.frame(item = input$new_item_3a)) |> 
        filter(!is.na(item))
      
      output$delete_list_temp_view = renderDataTable(datatable(delete_temp$data
                                                               , options = list(dom = 'ltip'
                                                                                , pageLength = 5
                                                                                , lengthMenu = c(5, 10, 15, 20) 
                                                              )))
      
      output$remove_item_temp_select_3a_rui <- renderUI(
        selectInput("remove_temp_item_select", "Select Temporary Item to Remove:"
                    ,c(NA, delete_temp$data |> 
                         select(item) |> 
                         distinct() |> 
                         pull())
        )
      )
      
      output$remove_item_temp_3a_rui <- renderUI(
        actionButton("remove_temp_item","Remove Item Temporarily")
      )
      
      
    }
  })
  
  observeEvent(input$remove_temp_item,{
    
    if (input$remove_temp_item_select == 'NA') {
      showModal(modalDialog(
        title = paste0(emo::ji('warning')," Warning"),
        HTML("A non-NA item needs to be selected for this to remove an item.
        "),
        easyClose = TRUE
      ))
    } else {
      delete_temp$data = delete_temp$data |> 
        filter(!is.na(item)) |> 
        filter(item != input$remove_temp_item_select)
      
      output$delete_list_temp_view = renderDataTable(datatable(delete_temp$data
                                                               , options = list(dom = 'ltip'
                                                                                , pageLength = 5
                                                                                , lengthMenu = c(5, 10, 15, 20) 
                                                                                )))
      
      output$remove_item_temp_select_3a_rui <- renderUI(
        selectInput("remove_temp_item_select", "Select Temporary Item to Remove:"
                    ,c(NA, delete_temp$data |> 
                         select(item) |> 
                         distinct() |> 
                         pull()
                       )
        )
      )

    }
  })
  
  ### Delete - Add a Permanent Item to Google Sheets List
  #####################################################################
  observeEvent(input$add_perm_item,{
    
    if (input$new_item_3a == 'NA') {
      showModal(modalDialog(
        title = paste0(emo::ji('warning')," Warning"),
        HTML("A non-NA item needs to be selected for this to add an item permanently.
        "),
        easyClose = TRUE
      ))
    } else {
      range_write('1pEEGA3mawQWmgDtIjJmpBMPws8_-EyEqPtQlucGVlNQ'
                  , data = delete_df$data |> 
                            rbind(data.frame(item = input$new_item_3a)) |> 
                            distinct()
                  , sheet = 'delete'
                  , col_names = TRUE
                  , range = "A:A"
      ) 
      # Sys.sleep(2)
      sh_delete_new = range_read('1pEEGA3mawQWmgDtIjJmpBMPws8_-EyEqPtQlucGVlNQ'
                                 ,sheet = 'delete'
                                 ,col_types = 'c'
      ) |> arrange(item)
      
      output$delete_list_view = renderDataTable(datatable(sh_delete_new
                                                          , options = list(dom = 'ltip'
                                                                           , pageLength = 5
                                                                           , lengthMenu = c(5, 10, 15, 20) 
                                                                           )))
      
      output$delete_gs_list_rui <- renderUI({
        
        selectInput("delete_list_2a","Select Permanent Item to Remove:"
                    , c(NA, sh_delete_new$item)
                    , multiple = FALSE)
      })
      delete_list$data = sh_delete_new$item
      delete_df$data = sh_delete_new
      
    }
  })
  
  ### Delete - Process Results
  #####################################################################
  observeEvent(input$process_3a,{
    
    #permanent delete list
    delete_list_items = df_long_def$data |>
      filter(item %in% (delete_df$data |>
                          filter(!is.na(item)) |> 
                          select(item) |>
                          distinct() |>
                          pull())
             ) |>
      group_by(item) |>
      summarise(Count = n())

    output$delete_list_table = renderDataTable(datatable(delete_list_items
                                                         , options = list(dom = 'ltip'
                                                                          , pageLength = 5
                                                                          , lengthMenu = c(5, 10, 15, 20) 
                                                                          )))

    #temporary delete list
    delete_list_temp_items = df_long_def$data |>
      filter(item %in% (delete_temp$data |>
                          filter(!is.na(item)) |> 
                          select(item) |>
                          distinct() |>
                          pull())
             ) |>
      group_by(item) |>
      summarise(Count = n())

    output$delete_list_temp_table = renderDataTable(datatable(delete_list_temp_items
                                                              , options = list(dom = 'ltip'
                                                                               , pageLength = 5
                                                                               , lengthMenu = c(5, 10, 15, 20) 
                                                                               )))

    #pot plant to remove
    delete_plants = df_long_def$data |>
      filter(str_detect(item, 'Pot Plant')) |>
      # filter(item %in% delete_list_plants) |>
      group_by(item) |>
      summarise(Count = n())

    output$delete_plant_table = renderDataTable(datatable(delete_plants
                                                          , options = list(dom = 'ltip'
                                                                           , pageLength = 5
                                                                           , lengthMenu = c(5, 10, 15, 20) 
                                                                           )))

    #Order Only members removed (if necessary)
    delete_no_share = df_long_def$data |>
      inner_join(df_long_def$data |>
                   filter(item %in% order_only_list
                   ) |>
                   select(id) |>
                   distinct()
                 , by = c('id')
      ) |>
      filter(rank == 3) |>
      group_by(id) |>
      mutate(rank_v = rank(row_number()),
             rows= max(rank_v)
      ) |>
      ungroup() |>
      filter(rows == 1) |>
      select(id) |>
      distinct()

    output$order_only_table = renderDataTable(datatable(df_long_def$data |>
                                                          filter(id %in% delete_no_share$id) |>
                                                          select('Group Name' = group_name
                                                                 , 'Name and Id' = name_long
                                                                 , 'Quanity' = quantity
                                                                 , 'Description' = item
                                                                 ) |>
                                                          distinct()
                                                        , options = list(dom = 'ltip'
                                                                         , pageLength = 5
                                                                         , lengthMenu = c(5, 10, 15, 20) 
                                                          )))

    #final data set
    df_long_del$data = df_long_def$data |>
      filter(!item %in% (delete_df$data |>
                           filter(!is.na(item)) |> 
                           select(item) |>
                           distinct() |>
                           pull())
             ) |>
      filter(!item %in% (delete_temp$data |>
                           filter(!is.na(item)) |> 
                           select(item) |>
                           distinct() |>
                           pull())
             ) |>
      filter(!id %in% delete_no_share$id) |>
      filter(!item %in% delete_plants$item)

    output$preview3 = renderDataTable(datatable(df_long_del$data |>
                                                  select('Group Name' = group_name
                                                         , 'Name and Id' = name_long
                                                         , 'Quanity' = quantity
                                                         , 'Description' = item
                                                  )
                                                , options = list(dom = 'ltip'
                                                                 , pageLength = 5
                                                                 , lengthMenu = c(5, 10, 15, 20) 
                                                               )))
    
    #### Plant Print Out (Optional Download Button)
    ##################################################################### 
    plant_output = df_long_def$data |> 
      filter(item %in% delete_plants$item) |>
      select(group_name, lastname, firstname, item, quantity)
    
    if (dim(plant_output)[1] > 0) {
      output$download_plants_text_rui <- renderUI({
        renderText(paste0(emo::ji('seedling'),emo::ji('seedling'),
                          "Plants Found - Order!",
                          emo::ji('seedling'),emo::ji('seedling')
                          )
                   )
      })
      
      output$download_plants_rui <- renderUI({
        downloadHandler(
          filename = function() {
            paste0('PotPlant_File_',input$date_value,'.xlsx')
          },
          content = function(file) {
            wb_p <- createWorkbook(title = 'PotPlant_File')
            addWorksheet(wb_p, "sheet1")
            writeData(wb_p, "sheet1", plant_output)
            saveWorkbook(wb_p, file, overwrite = TRUE)
          })
      })
    }
    output$step3a = renderText(paste0("Step 3a ",emo::ji('heavy_check_mark')))
    
    output$step3b <- renderUI({
      renderText(paste0("Step 3b ",emo::ji('black_circle')))
    })
    
    output$step3b_text <- renderUI({
      HTML("Check Deletion Results.<br>
           <ul>
            <li>If issues redue Step 3a.
            <li>If no issues Process Step 3b.
           <ul>")
    })
    output$step3b_process <- renderUI({
      actionButton("process_3b","Step 3b. Process")
    })
    })
  
  ### Optimize orders to two columns
  #####################################################################
  column_print_comb_excel = reactiveValues(data = NA)
  column_print_comb_openo = reactiveValues(data = NA)
  # column_print_comb_sheets = reactiveValues(data = NA)
  
  df_symbol_inputs = reactiveValues(data = df_symbol_inputs_orig) # Used in Step 4)
  df_format_inputs = reactiveValues(data = df_format_inputs_orig) # Used in Step 5)
  
  observeEvent(input$process_3b,{
    
    optimize_columns = function(data, max_rows) {
      opt_group = data |> 
        ungroup() |> 
        filter(rank >= 0) |> 
        group_by(group_name, description, time, lastname, firstname, id) |> 
        summarise(n = n()) |>  
        ungroup() |> 
        arrange(group_name, description, time, lastname, firstname, desc(n))
      
      group_names = data |>  
        ungroup() |> 
        select(group_name) |> 
        distinct()
      
      column1 = as.data.frame(NULL)
      column2 = as.data.frame(NULL)
      row_max = max_rows - 1 ##one minus the max amount of rows per sheet in print process
      g_name = NULL
      
      
      for (g in 1:dim(group_names)[1]) {
        g_name = group_names$group_name[g]
        opt_grouped = opt_group |> 
          filter(group_name == g_name)
        page = 1
        c1_rows = 0
        c2_rows = 0
        c1_cust = 0
        c2_cust = 0
        name = NA
        for (i in 1:dim(opt_grouped)[1]) {
          name = opt_grouped[i,]
          if (dim(column1)[1] == 0 & dim(column2)[1] == 0 & i == 1) {
            column1 = column1 %>% 
              rbind(opt_grouped[i,] %>% 
                      mutate(page_n = page
                      )
              )
            c1_rows = column1$n[dim(column1)[1]]
            c1_cust = 1
          } else {
            if (floor((opt_grouped$n[i] + c1_rows)/row_max) > 0 & floor((opt_grouped$n[i] + c2_rows)/row_max) > 0) {
              page = page+1
              column1 = column1 %>% 
                rbind(opt_grouped[i,] %>% 
                        mutate(page_n = page
                        )
                )
              c1_rows = column1$n[dim(column1)[1]]
              c2_rows = 0
              c1_cust = 1
              c2_cust = 0
            } else if (floor((opt_grouped$n[i] + c1_rows)/row_max) > 0 & floor((opt_grouped$n[i] + c2_rows)/row_max) == 0) {
              column2 = column2 %>% 
                rbind(opt_grouped[i,] %>% 
                        mutate(page_n = page
                        )
                )
              c2_rows = c2_rows + column2$n[dim(column2)[1]]
              c2_cust = c2_cust + 1
            }  else if (floor((opt_grouped$n[i] + c1_rows)/row_max) == 0 & floor((opt_grouped$n[i] + c2_rows)/row_max) > 0) {
              column1 = column1 %>%
                rbind(opt_grouped[i,] %>%
                        mutate(page_n = page
                        )
                )
              c1_rows = c1_rows + column1$n[dim(column1)[1]]
              c1_cust = c1_cust + 1
            } else if (c1_cust > c2_cust) {
              column2 = column2 %>% 
                rbind(opt_grouped[i,] %>% 
                        mutate(page_n = page
                        )
                )
              c2_rows = c2_rows + column2$n[dim(column2)[1]]
              c2_cust = c2_cust + 1
            } else {
              column1 = column1 %>% 
                rbind(opt_grouped[i,] %>% 
                        mutate(page_n = page
                        )
                )
              c1_rows = c1_rows + column1$n[dim(column1)[1]]
              c1_cust = c1_cust + 1
            }
          }
          # print(opt_grouped$n[i])
        }
      }
      # }
      # 
      # debug(test)
      
      group_name_additions_c1 = data %>% 
        ungroup() |> 
        filter(rank < 0) |> 
        mutate(n = 1) |> 
        select(group_name, description, time, lastname, firstname, id, n) |> 
        left_join(column1 |> 
                    select(group_name, page_n) |> 
                    distinct()
                  , by = c('group_name')
        ) |> 
        rbind(column1) |> 
        arrange(group_name, page_n, description, time, lastname, firstname, desc(n))
      
      group_name_additions_c2 = data %>% 
        ungroup() |> 
        filter(rank < 0) |> 
        mutate(n = 1) |> 
        select(group_name, description, time, lastname, firstname, id, n) |> 
        left_join(column2 |> 
                    select(group_name, page_n) |> 
                    distinct()
                  , by = c('group_name')
        ) |> 
        rbind(column2) |> 
        arrange(group_name, page_n, description, time, lastname, firstname, desc(n))
      
      row_df = data.frame(row_n = 0:row_max+1)
      print_full = column1 |> 
        select(group_name, page_n) |> 
        distinct() |> 
        full_join(row_df, by = character()
        )
      
      column1_w_groups = group_name_additions_c1 |> 
        select(id, n, page_n, group_name_col = group_name) |> 
        left_join(data |> 
                    ungroup() |> 
                    filter(rank >= 0)
                  , by = c('id')
        ) |> 
        mutate(item = ifelse(is.na(rank), group_name_col, item),
               group_name = ifelse(is.na(rank), group_name_col, group_name),
               rank = ifelse(is.na(rank), -1, rank)
        ) |> 
        select(-group_name_col) |> 
        group_by(group_name, page_n) |> 
        mutate(row_n = rank(row_number()))
      
      column2_w_groups = group_name_additions_c2 |> 
        select(id, n, page_n, group_name_col = group_name) |> 
        left_join(data |> 
                    ungroup() |> 
                    filter(rank >= 0)
                  , by = c('id')
        ) |> 
        mutate(item = ifelse(is.na(rank), group_name_col, item),
               group_name = ifelse(is.na(rank), group_name_col, group_name),
               rank = ifelse(is.na(rank), -1, rank)
        ) |> 
        select(-group_name_col) |> 
        group_by(group_name, page_n) |> 
        mutate(row_n = rank(row_number()))
      
      stopifnot(column1_w_groups |> 
                  filter(row_n > row_max+1) |> 
                  summarise(n=n()) |> 
                  pull() == 0)
      
      stopifnot(column2_w_groups |> 
                  filter(row_n > row_max+1) |> 
                  summarise(n=n()) |> 
                  pull() == 0)
      
      column1_print = print_full |> 
        left_join(column1_w_groups
                  , by = c('group_name', 'page_n', 'row_n')
        )
      
      column2_print = print_full |> 
        left_join(column2_w_groups
                  , by = c('group_name', 'page_n', 'row_n')
        )
      
      column_print_combined = column1_print |> 
        mutate(column = 1) |> 
        rbind(column2_print |> 
                mutate(column = 2)
        )
      
      return(column_print_combined)
    }
    
    column_print_comb_excel$data = optimize_columns(df_long_del$data, 46)
    column_print_comb_openo$data = optimize_columns(df_long_del$data, 46)
    # column_print_comb_sheets = optimize_columns(df_long_del$data, 47)
    
    output$preview3 = renderDataTable(datatable(column_print_comb_excel$data |>
                                                  filter(column == 1) |> 
                                                  select('Description' = item) |> 
                                                  cbind(column_print_comb_excel$data |>
                                                          filter(column == 2) |> 
                                                          select('Description' = item))
                                                , options = list(dom = 'ltip'
                                                                 , pageLength = 5
                                                                 , lengthMenu = c(5, 10, 15, 20) 
                                                )))
    
    output$step3b = renderUI(
      renderText(paste0("Step 3b ",emo::ji('heavy_check_mark')))
    )
    
    output$step3 <- renderUI(
      renderText(paste0(emo::ji('heavy_check_mark'), " Step 3 is Complete! ",emo::ji('party_popper')))
    )
    output$step3_ <- renderUI(
      renderText(paste0(emo::ji('index_pointing_up'),
                        emo::ji('index_pointing_up'),
                        "   GO TO STEP 4 TAB! "
                        ,emo::ji('index_pointing_up')
                        ,emo::ji('index_pointing_up')))
    )
    
    
    ##### Step 4 Variables Created
    
    output$new_item_4a_match_rui <- renderUI(
      selectInput("new_item_4a_match", "Select Match Type (Required):"
                  , c(NA, match_name_list)
                  , multiple = FALSE
      )
    )
    
    output$new_item_4a_text_search_rui <- renderUI(
      textInput("new_item_4a_text_search", "Text to Search for (Required - Case Sensitive!):"
      )
    )
    
    output$new_item_4a_text_add_rui <- renderUI(
      textInput("new_item_4a_text_add", "Text to Add (Optional):"
      )
    )
    
    output$emoji_help_4a_rui <- renderUI(
      actionButton("emoji_help_4a", paste0(emo::ji('information'),"Click for Emoji Details"))
    )
    
    output$new_item_4a_emoji_rui <- renderUI(
      selectInput("new_item_4a_emoji", "Select Emoji (Optional):"
                  , c(NA, emoji_name_list)
                  , multiple = FALSE
      )
    )
    
    output$new_item_4a_add_rui <- renderUI(
      actionButton("new_item_4a_add", "Add Item Text Change")
    )
  
    output$new_item_4a_row_rui <- renderUI(
      selectInput("new_item_4a_row", "Select Search Row Number to Remove"
                  , c(NA, df_symbol_inputs$data |> 
                          select(row_num) |> 
                          pull()
                       )
                  , multiple = FALSE
      )
    )
    
    output$new_item_4a_remove_rui <- renderUI(
      actionButton("new_item_4a_remove", "Remove Item Text Change")
    )
    
    output$process_4a_rui <- renderUI(
      actionButton("process_4a", "Step 4a. Process")
    )
    
    output$preview4a = renderDataTable(datatable(df_symbol_inputs$data |> 
                                                   select('Match Type' = match_name
                                                          , 'Searched Text' = text_name
                                                          , 'Added Text' = freeform_text
                                                          , Emoji = emoji_name)
                                                 , options = list(dom = 'ltip'
                                                                  , pageLength = 5
                                                                  , lengthMenu = c(5, 10, 15, 20) 
                                                 )))
    
  })

  ############## Step 4 (Add Text/Emojis)  ##############
  #############################################################################################
  ###### ENHANCEMENT LIST FOR STEP:
     ##Add in to the searched text the Row Count found after processed
  
  
  output$step4a = renderText(paste0("Step 4a ",emo::ji('black_circle')))
  
  df_long_symbol_excel = reactiveValues(data = NA)
  df_long_symbol_openo = reactiveValues(data = NA)
  
  ### Emoji Help
  #####################################################################
  observeEvent(input$emoji_help_4a,{
    showModal(modalDialog(
      title = paste0(emo::ji('information')," Help"),
      HTML("The Emoji list provided in this dashboard is not comprehensive.<br>
           If you need more resource options you can first look at all available options <a href='https://github.com/hadley/emo'>here</a><br>
           If a new Emoji is found that you want in the dashboard let the <p>&#129412;</p> know which one they should add."),
      easyClose = TRUE
    ))
  })
  
  
  ### Add Item Search  
  #####################################################################
  observeEvent(input$new_item_4a_add,{
    if (input$new_item_4a_match == 'NA') {
      showModal(modalDialog(
        title = paste0(emo::ji('stop_sign')," Error"),
        HTML("Before adding a new text search to change the <b>Match Type</b> field needs to not show as <u>NA</u>."),
        easyClose = TRUE
      ))
    } else if (input$new_item_4a_text_search == '') {
      showModal(modalDialog(
        title = paste0(emo::ji('stop_sign')," Error"),
        HTML("Before adding a new text search to change the <b>Text to Search</b> field needs to not show as <u>blank</u>."),
        easyClose = TRUE
      ))
    } else if (input$new_item_4a_text_add == '' & input$new_item_4a_emoji == 'NA') {
      showModal(modalDialog(
        title = paste0(emo::ji('stop_sign')," Error"),
        HTML("Before adding a new text search to change the <b>Text to Add</b> field needs to not show as <u>blank</u><br>
             OR<br>
             The <b>Emoji</b> dropdown needs to not show as <u>NA</u>"),
        easyClose = TRUE
      ))
    } else {
      ## add data
      if (input$new_item_4a_emoji == 'NA') {
        df_symbol_in1 = df_symbol_inputs$data |> 
          select(-row_num) |> 
          rbind(
            data.frame(match_name = c(input$new_item_4a_match)
                       ,text_name = c(input$new_item_4a_text_search)
                       ,freeform_text = c(input$new_item_4a_text_add)
                       ,emoji_name = c('')
            ) 
            
          ) |>  
          distinct() |> 
          mutate(row_num = row_number())
      } else {
        df_symbol_in1 = df_symbol_inputs$data |> 
          select(-row_num) |> 
          rbind(
            data.frame(match_name = c(input$new_item_4a_match)
                       ,text_name = c(input$new_item_4a_text_search)
                       ,freeform_text = c(input$new_item_4a_text_add)
                       ,emoji_name = c(input$new_item_4a_emoji)
            )
          ) |>  
          distinct() |> 
          mutate(row_num = row_number())
      }
      #update drop down row # for remove
      output$new_item_4a_row_rui <- renderUI(
        selectInput("new_item_4a_row", "Select Search Row Number to Remove"
                    , c(NA, df_symbol_in1 |> 
                         select(row_num) |> 
                         pull()
                    )
                    , multiple = FALSE
        )
      )
      #update chart
      output$preview4a = renderDataTable(datatable(df_symbol_in1 |> 
                                                     select('Match Type' = match_name
                                                            , 'Searched Text' = text_name
                                                            , 'Added Text' = freeform_text
                                                            , Emoji = emoji_name)
                                                   , options = list(dom = 'ltip'
                                                                    , pageLength = 5
                                                                    , lengthMenu = c(5, 10, 15, 20) 
                                                   )))
      df_symbol_inputs$data = df_symbol_in1
    }

  })
  
  ### Remove Item Search
  #####################################################################
  observeEvent(input$new_item_4a_remove,{
    if (input$new_item_4a_row == 'NA') {
      showModal(modalDialog(
        title = paste0(emo::ji('stop_sign')," Error"),
        HTML("Before removing a text search line the <b>Select Search Row Number to Remove</b> field needs to not show as <u>NA</u>."),
        easyClose = TRUE
      ))
    } else {
      df_symbol_in2 = df_symbol_inputs$data |> 
        filter(row_num != input$new_item_4a_row) |> 
        select(-row_num) |> 
        distinct() |> 
        mutate(row_num = row_number())
        
      #update drop down row # for remove
      output$new_item_4a_row_rui <- renderUI(
         selectInput("new_item_4a_row", "Select Search Row Number to Remove"
                      , c(NA, df_symbol_in2 |> 
                           select(row_num) |> 
                           pull()
                      )
                      , multiple = FALSE
          )
        )
        
      #update chart
      output$preview4a = renderDataTable(datatable(df_symbol_in2 |> 
                                                       select('Match Type' = match_name
                                                              , 'Searched Text' = text_name
                                                              , 'Added Text' = freeform_text
                                                              , Emoji = emoji_name)
                                                     , options = list(dom = 'ltip'
                                                                      , pageLength = 5
                                                                      , lengthMenu = c(5, 10, 15, 20) 
                                                     )))
      df_symbol_inputs$data = df_symbol_in2
    }
    
  })   
  
  ### Process Search and Add new Text/Emojis
  #####################################################################
  observeEvent(input$process_4a,{
    symbol_addition = function(item_n, match_n, text_n, ff_n, em_n) {
      ifelse((!is.na(ff_n)) & (!is.na(em_n)),
             ifelse(match_n == 'equal', ifelse(item_n == text_n, paste0(em_n, ff_n), NA),
                    ifelse(match_n == 'contains', ifelse(str_detect(item_n, text_n), paste0(em_n, ff_n), NA),
                           ifelse(match_n == 'starts with', ifelse(startsWith(item_n, text_n), paste0(em_n, ff_n), NA),
                                  ifelse(match_n == 'ends with', ifelse(endsWith(item_n, text_n), paste0(em_n, ff_n), NA)
                                         , NA))))
             ,ifelse((!is.na(ff_n)), 
                     ifelse(match_n == 'equal', ifelse(item_n == text_n, paste0(ff_n), NA),
                            ifelse(match_n == 'contains', ifelse(str_detect(item_n, text_n), paste0(ff_n), NA),
                                   ifelse(match_n == 'starts with', ifelse(startsWith(item_n, text_n), paste0(ff_n), NA),
                                          ifelse(match_n == 'ends with', ifelse(endsWith(item_n, text_n), paste0(ff_n), NA)
                                                 , NA))))
                     ,ifelse((!is.na(em_n)) , 
                             ifelse(match_n == 'equal', ifelse(item_n == text_n, paste0(em_n), NA),
                                    ifelse(match_n == 'contains', ifelse(str_detect(item_n, text_n), paste0(em_n), NA),
                                           ifelse(match_n == 'starts with', ifelse(startsWith(item_n, text_n), paste0(em_n), NA),
                                                  ifelse(match_n == 'ends with', ifelse(endsWith(item_n, text_n), paste0(em_n), NA)
                                                         , NA))))   
                             , NA)))
    }
    
    
    add_item_styling = function(data) {
      symbol_search = data |> 
        select(item) |> 
        distinct() |> 
        full_join(df_symbol_inputs$data
                  , by = character()
        ) |> 
        mutate(symbol_text = symbol_addition(item, match_name, text_name, freeform_text, emoji_name)) |> 
        filter(!is.na(symbol_text)) |> 
        select(item, symbol_text) |> 
        arrange(item, symbol_text) |> 
        group_by(item) |> 
        mutate(symbol_text = paste0(symbol_text, collapse = "")) |> 
        ungroup() |> 
        distinct()
      
      df_long_symbol = data |> 
        left_join(symbol_search
                  , by = c('item')
        ) |> 
        mutate(quantity = ifelse(is.na(quantity), '', as.character(quantity))
               , item = ifelse(is.na(item), '', item)
               , item_symbol = ifelse(quantity == '', item, paste0(quantity,'x',item))
               , qty_symbol = ifelse(quantity > 1, emo::ji('plus'), '')
               , item_symbol = ifelse(is.na(symbol_text)
                                      , paste0(qty_symbol, item_symbol)
                                      , paste0(qty_symbol, symbol_text, item_symbol))
        )
      return(df_long_symbol)
    }
    
    df_long_symbol_excel$data = add_item_styling(column_print_comb_excel$data)
    df_long_symbol_openo$data = add_item_styling(column_print_comb_openo$data)

    output$preview4b = renderDataTable(datatable(df_long_symbol_excel$data |>
                                                  filter(column == 1) |> 
                                                  select('Description' = item_symbol) |> 
                                                  cbind(df_long_symbol_excel$data |>
                                                          filter(column == 2) |> 
                                                          select('Description' = item_symbol)
                                                        )
                                                , options = list(dom = 'ltip'
                                                                 , pageLength = 5
                                                                 , lengthMenu = c(5, 10, 15, 20) 
                                                )))
    
    output$step4a = renderText(paste0("Step 4a ",emo::ji('heavy_check_mark')))
    
    output$step4b <- renderUI({
      renderText(paste0("Step 4b ",emo::ji('black_circle')))
    })
    
    output$step4b_text <- renderUI({
      HTML("Check Emoji/Text Results.<br>
           <ul>
            <li>If issues redue Step 4a.
            <li>If no issues Process Step 4b.
           <ul>")
    })
    output$step4b_process <- renderUI({
      actionButton("process_4b","Step 4b. Process")
    })
  })
  
  observeEvent(input$process_4b,{
    output$step4b = renderUI(
      renderText(paste0("Step 4b ",emo::ji('heavy_check_mark')))
    )
    
    output$step4 <- renderUI(
      renderText(paste0(emo::ji('heavy_check_mark'), " Step 4 is Complete! ",emo::ji('party_popper')))
    )
    output$step4_ <- renderUI(
      renderText(paste0(emo::ji('index_pointing_up'),
                        emo::ji('index_pointing_up'),
                        "   GO TO STEP 5 TAB! "
                        ,emo::ji('index_pointing_up')
                        ,emo::ji('index_pointing_up')))
    )
    
    # Step 5 setup
    
    output$new_item_5a_match_rui <- renderUI(
      selectInput("new_item_5a_match", "Select Match Type (Required):"
                  , c(NA, match_name_list)
                  , multiple = FALSE
      )
    )
    
    output$new_item_5a_text_search_rui <- renderUI(
      textInput("new_item_5a_text_search", "Text to Search for (Required - Case Sensitive!):"
      )
    )
    
    output$new_item_5a_format_rui <- renderUI(
      selectInput("new_item_5a_format", "Select Formatting Type (Optional):"
                  , c(NA, format_list)
                  , multiple = FALSE
      )
    )
    
    output$color_help_5a_rui <- renderUI(
      actionButton("color_help_5a", paste0(emo::ji('information'),"Click for Color Details"))
    )

    output$new_item_5a_color_rui <- renderUI(
      # selectInput("new_item_5a_color", "Select Color (Optional):"
      #             , c(NA, '#ffff00')
      #             , multiple = FALSE
      # )
      colourInput(
        "new_item_5a_color", NULL, "#E5E5E5",
        palette = "limited")
    )
    
    output$new_item_5a_add_rui <- renderUI(
      actionButton("new_item_5a_add", "Add Item Format Change")
    )
    
    output$new_item_5a_row_rui <- renderUI(
      selectInput("new_item_5a_row", "Select Search Row Number to Remove"
                  , c(NA, df_format_inputs$data |> 
                        select(row_num) |> 
                        pull()
                  )
                  , multiple = FALSE
      )
    )
    
    output$new_item_5a_remove_rui <- renderUI(
      actionButton("new_item_5a_remove", "Remove Item Format Change")
    )
    
    output$process_5a_rui <- renderUI(
      actionButton("process_5a", "Step 5a. Process")
    )
    
    output$preview5a = renderDataTable(datatable(df_format_inputs$data |> 
                                                   select('Match Type' = match_name
                                                         , 'Searched Text' = text_name
                                                         , 'Format Type' = format_name
                                                         , 'Color' = color_name)
                                                 , options = list(dom = 'ltip'
                                                                  , pageLength = 5
                                                                  , lengthMenu = c(5, 10, 15, 20) 
                                                 )))
  })
  
  ############## Step 5 (Color / Text Format)  ##############
  #############################################################################################
  ###### ENHANCEMENT LIST FOR STEP:
     ##SHARE FINAL VIEW OF EXCEL WITH FORMATTING CHANGES
     ## ADD THE DATA FRAME SHOWING MEMBER REDUCTION TOTALS TO SHOW SOMETHING IF RESULT ABOVE FAILS
  
  output$step5a = renderText(paste0("Step 5a ",emo::ji('black_circle')))
  
  wb_excel = reactiveValues(data = NA)
  wb_openo = reactiveValues(data = NA)
  
  ### Color Help
  #####################################################################
  observeEvent(input$color_help_5a,{
    showModal(modalDialog(
      title = paste0(emo::ji('information')," Help"),
      HTML("The Color list provided in this dashboard is not comprehensive.<br>
           If you need more resource options you can.... <br>
           If a new Color is found that you want in the dashboard let the <p>&#129412;</p> know which one they should add."),
      easyClose = TRUE
    ))
  })

  ### Add Item Format  
  #####################################################################
  observeEvent(input$new_item_5a_add,{
    if (input$new_item_5a_match == 'NA') {
      showModal(modalDialog(
        title = paste0(emo::ji('stop_sign')," Error"),
        HTML("Before adding a new text Format to change the <b>Match Type</b> field needs to not show as <u>NA</u>."),
        easyClose = TRUE
      ))
    } else if (input$new_item_5a_text_search == '') {
      showModal(modalDialog(
        title = paste0(emo::ji('stop_sign')," Error"),
        HTML("Before adding a new text Format to change the <b>Text to Search</b> field needs to not show as <u>blank</u>."),
        easyClose = TRUE
      ))
    } else if (input$new_item_5a_format == 'NA' & input$new_item_5a_color == 'NA') {
      showModal(modalDialog(
        title = paste0(emo::ji('stop_sign')," Error"),
        HTML("Before adding a new text Format to change the <b>Format Type</b> field needs to not show as <u>NA</u><br>
             OR<br>
             The <b>Color</b> dropdown needs to not show as <u>NA</u>"),
        easyClose = TRUE
      ))
    } else {
      ## add data
      if (input$new_item_5a_format == 'NA') {
        df_format_in1 = df_format_inputs$data |> 
          select(-row_num) |> 
          rbind(
            data.frame(match_name = c(input$new_item_5a_match)
                       ,text_name = c(input$new_item_5a_text_search)
                       ,format_name = c(NA)
                       ,color_name = c(input$new_item_5a_color)
            ) 
          ) |>  
          distinct() |> 
          mutate(row_num = row_number())
      } else if (input$new_item_5a_color == 'NA') {
        df_format_in1 = df_format_inputs$data |> 
          select(-row_num) |> 
          rbind(
            data.frame(match_name = c(input$new_item_5a_match)
                       ,text_name = c(input$new_item_5a_text_search)
                       ,format_name = c(input$new_item_5a_format)
                       ,color_name = c(NA)
            ) 
          ) |>  
          distinct() |> 
          mutate(row_num = row_number())
      } else {
        df_format_in1 = df_format_inputs$data |> 
          select(-row_num) |> 
          rbind(
            data.frame(match_name = c(input$new_item_5a_match)
                       ,text_name = c(input$new_item_5a_text_search)
                       ,format_name = c(input$new_item_5a_format)
                       ,color_name = c(input$new_item_5a_color)
            )
          ) |>  
          distinct() |> 
          mutate(row_num = row_number())
      }
      #update drop down row # for remove
      output$new_item_5a_row_rui <- renderUI(
        selectInput("new_item_5a_row", "Select Search Row Number to Remove"
                    , c(NA, df_format_in1 |> 
                          select(row_num) |> 
                          pull()
                    )
                    , multiple = FALSE
        )
      )
      #update chart
      output$preview5a = renderDataTable(datatable(df_format_in1 |> 
                                                     select('Match Type' = match_name
                                                            , 'Searched Text' = text_name
                                                            , 'Format Type' = format_name
                                                            , 'Color' = color_name)
                                                   , options = list(dom = 'ltip'
                                                                    , pageLength = 5
                                                                    , lengthMenu = c(5, 10, 15, 20) 
                                                   )))
      df_format_inputs$data = df_format_in1
    }
    
  })
  
  ### Remove Item Format
  #####################################################################
  observeEvent(input$new_item_5a_remove,{
    if (input$new_item_5a_row == 'NA') {
      showModal(modalDialog(
        title = paste0(emo::ji('stop_sign')," Error"),
        HTML("Before removing a text search line the <b>Select Search Row Number to Remove</b> field needs to not show as <u>NA</u>."),
        easyClose = TRUE
      ))
    } else {
      df_format_in2 = df_format_inputs$data |> 
        filter(row_num != input$new_item_5a_row) |> 
        select(-row_num) |> 
        distinct() |> 
        mutate(row_num = row_number())
      
      #update drop down row # for remove
      output$new_item_5a_row_rui <- renderUI(
        selectInput("new_item_5a_row", "Select Format Row Number to Remove"
                    , c(NA, df_format_in2 |> 
                          select(row_num) |> 
                          pull()
                    )
                    , multiple = FALSE
        )
      )
      
      #update chart
      output$preview5a = renderDataTable(datatable(df_format_in2 |> 
                                                     select('Match Type' = match_name
                                                            , 'Searched Text' = text_name
                                                            , 'Format Type' = format_name
                                                            , 'Color' = color_name)
                                                   , options = list(dom = 'ltip'
                                                                    , pageLength = 5
                                                                    , lengthMenu = c(5, 10, 15, 20) 
                                                   )))
      df_format_inputs$data = df_format_in2
    }
    
  })   
  
  ### Process Color Background / Change Text Format
  #####################################################################
  observeEvent(input$process_5a,{
    ## Create function to do color or text change based on search criteria in item
    workbook_created = function(data, width, height) {
      df_long_sh_color_c1 = data |> 
        filter(column == 1) |> 
        left_join(sh_shares_shrt
                  , by = c('group_name')
        ) |> 
        mutate(highlight_color_hex = ifelse(rank <= 0, highlight_color_hex, NA))
      
      df_long_sh_color_c2 = data |> 
        filter(column == 2) |> 
        left_join(sh_shares_shrt
                  , by = c('group_name')
        ) |> 
        mutate(highlight_color_hex = ifelse(rank <= 0, highlight_color_hex, NA))
      
      
      write_df = df_long_sh_color_c1 |>
        mutate(item_symbol = ifelse(rank == -1
                                    , paste0(item_symbol, ' (',season_number$data,'w',week_number$data, ' - ', input$date_value,')')
                                    , item_symbol)
        ) |> 
        select(item_1 = item_symbol) |>
        cbind(
          df_long_sh_color_c2 |>
            mutate(item_symbol = ifelse(rank == -1
                                        , paste0(item_symbol, ' (',season_number$data,'w',week_number$data, ' - ', input$date_value,')')
                                        , item_symbol)
            ) |> 
            select(item_2 = item_symbol)
        )
      
      wb <- createWorkbook(title = 'TestFile')
      addWorksheet(wb, "testing")
      writeData(wb, "testing", write_df, colNames = FALSE)
      
      setColWidths(wb, "testing", cols = 1:2, widths = width)
      setRowHeights(wb, "testing", rows = (dim(data)[1]/2), heights = height)
      modifyBaseFont(wb, fontSize = 12, fontColour = "black", fontName = "Calibri")
      
      ## Coloring each Group and Member names based on Google Sheet 
      
      #### Column 1
      test_color_c1 = df_long_sh_color_c1 |> 
        mutate(ss_cell = row_number()) |> 
        filter(!is.na(highlight_color_hex)) 
      
      withProgress(message = 'Color Groups - Column 1', value = 0, {
        # Number of times we'll go through the loop
        n <- (test_color_c1 |> select(highlight_color_hex) |> distinct() |> summarise(n = n()) |> pull())
        for (i in (test_color_c1 |> select(highlight_color_hex) |> distinct() |> pull())) {
          print(i)
          bhfill_style <- createStyle(bgFill = i, textDecoration = "bold")
          
          for (j in (test_color_c1 |> filter(highlight_color_hex == i) |> select(ss_cell) |> distinct() |> pull())) {
            conditionalFormatting(wb, "testing", rule = "!=0", cols = 1, rows = j, style = bhfill_style)
            addStyle(wb, "testing", cols = 1, rows = j, style = createStyle(halign = "center"))
          }
          
          # Increment the progress bar, and update the detail text.
          incProgress(1/n, detail = paste("Painting", i))
        }
      })
      
      #### Column 2
      test_color_c2 = df_long_sh_color_c2 |> 
        mutate(ss_cell = row_number()) |> 
        filter(!is.na(highlight_color_hex)) 
      
      withProgress(message = 'Color Groups - Column 2', value = 0, {
        # Number of times we'll go through the loop
        n <- (test_color_c2 |> select(highlight_color_hex) |> distinct() |> summarise(n = n()) |> pull())
        for (i in (test_color_c2 |> select(highlight_color_hex) |> distinct() |> pull())) {
          print(i)
          bhfill_style <- createStyle(bgFill = i, textDecoration = "bold")
          
          for (j in (test_color_c2 |> filter(highlight_color_hex == i) |> select(ss_cell) |> distinct() |> pull())) {
            conditionalFormatting(wb, "testing", rule = "!=0", cols = 2, rows = j, style = bhfill_style)
            addStyle(wb, "testing", cols = 2, rows = j, style = createStyle(halign = "center"))
          }
          # Increment the progress bar, and update the detail text.
          incProgress(1/n, detail = paste("Painting", i))
        }
      })
      
      ## Bold each item if its in the Standard Item List for that group
      
      bold_stndrd_item_c1 = df_long_sh_color_c1 |> 
        mutate(ss_cell = row_number()) |> 
        filter(!is.na(standard_item)) 
      
      withProgress(message = 'Bold Standard Share Items - Column 1', value = 0, {
        # Number of times we'll go through the loop
        n <- (bold_stndrd_item_c1 |> select(ss_cell) |> distinct() |> summarise(n = n()) |> pull())
        for (j in (bold_stndrd_item_c1 |> select(ss_cell) |> distinct() |> pull())) {
          bhfill_style <- createStyle(textDecoration = "bold")
          conditionalFormatting(wb, "testing", rule = "!=0", cols = 1, rows = j, style = bhfill_style)
          # Increment the progress bar, and update the detail text.
          incProgress(1/n, detail = paste("Painting", i))
        }
      })
      
      bold_stndrd_item_c2 = df_long_sh_color_c2 |> 
        mutate(ss_cell = row_number()) |> 
        filter(!is.na(standard_item)) 
      
      withProgress(message = 'Bold Standard Share Items - Column 2', value = 0, {
        # Number of times we'll go through the loop
        n <- (bold_stndrd_item_c2 |> select(ss_cell) |> distinct() |> summarise(n = n()) |> pull())
        for (j in (bold_stndrd_item_c2 |> select(ss_cell) |> distinct() |> pull())) {
          bhfill_style <- createStyle(textDecoration = "bold")
          conditionalFormatting(wb, "testing", rule = "!=0", cols = 2, rows = j, style = bhfill_style)
          # Increment the progress bar, and update the detail text.
          incProgress(1/n, detail = paste("Painting", i))
        }
      })
      
      style_xlsx = function(wb_n, sheet, col_n, match, text, colorhex, text_dec_name) {
        if (!is.na(colorhex) & !is.na(text_dec_name)) {  #HIGHLIGHT BACKGROUND AND DECORATE TEXT
          if (match == 'equal') {
            addStyle(wb_n, sheet
                     , cols = col_n
                     , rows = which(df_long_sh_color$item == text)
                     , style = createStyle(fgFill = colorhex
                                           ,textDecoration = text_dec_name)
            )
          } else if (match == 'contains') {
            addStyle(wb_n, sheet
                     , cols = col_n
                     , rows = which(str_detect(df_long_sh_color$item, pattern = text))
                     , style = createStyle(fgFill = colorhex
                                           ,textDecoration = text_dec_name)
            )
          } else if (match == 'starts with') {
            addStyle(wb_n, sheet
                     , cols = col_n
                     , rows = which(startsWith(df_long_sh_color$item, text))
                     , style = createStyle(fgFill = colorhex
                                           ,textDecoration = text_dec_name)
            )
          } else if (match == 'ends with') {
            addStyle(wb_n, sheet
                     , cols = col_n
                     , rows = which(endsWith(df_long_sh_color$item, text))
                     , style = createStyle(fgFill = colorhex
                                           ,textDecoration = text_dec_name)
            )
          } else {
            print('valid match type not determined')
          }
        } else if (!is.na(colorhex)) {    #HIGHLIGHT BACKGROUND ONLY
          if (match == 'equal') {
            addStyle(wb_n, sheet
                     , cols = col_n
                     , rows = which(df_long_sh_color$item == text)
                     , style = createStyle(fgFill = colorhex)
            )
          } else if (match == 'contains') {
            addStyle(wb_n, sheet
                     , cols = col_n
                     , rows = which(str_detect(df_long_sh_color$item, pattern = text))
                     , style = createStyle(fgFill = colorhex)
            )
          } else if (match == 'starts with') {
            addStyle(wb_n, sheet
                     , cols = col_n
                     , rows = which(startsWith(df_long_sh_color$item, text))
                     , style = createStyle(fgFill = colorhex)
            )
          } else if (match == 'ends with') {
            addStyle(wb_n, sheet
                     , cols = col_n
                     , rows = which(endsWith(df_long_sh_color$item, text))
                     , style = createStyle(fgFill = colorhex)
            )
          } else {
            print('valid match type not determined')
          }
        } else if (!is.na(text_dec_name)) {   #DECORATE TEXT ONLY
          if (match == 'equal') {
            addStyle(wb_n, sheet
                     , cols = col_n
                     , rows = which(df_long_sh_color$item == text)
                     , style = createStyle(textDecoration = text_dec_name)
            )
          } else if (match == 'contains') {
            addStyle(wb_n, sheet
                     , cols = col_n
                     , rows = which(str_detect(df_long_sh_color$item, pattern = text))
                     , style = createStyle(textDecoration = text_dec_name)
            )
          } else if (match == 'starts with') {
            addStyle(wb_n, sheet
                     , cols = col_n
                     , rows = which(startsWith(df_long_sh_color$item, text))
                     , style = createStyle(textDecoration = text_dec_name)
            )
          } else if (match == 'ends with') {
            addStyle(wb_n, sheet
                     , cols = col_n
                     , rows = which(endsWith(df_long_sh_color$item, text))
                     , style = createStyle(textDecoration = text_dec_name)
            )
          } else {
            print('valid match type not determined')
          }
        } else {
          print('Need to selet a color or text format type')
        }
        
      }
      
      df_long_sh_color = df_long_sh_color_c1
      withProgress(message = 'Custom Formatting - Column 1', value = 0, {
        # Number of times we'll go through the loop
        n <- (dim(df_format_inputs$data)[1])
        for (i in 1:dim(df_format_inputs$data)[1]) {
          df_l = df_format_inputs$data
          style_xlsx(wb, "testing", 1, df_l$match_name[i], df_l$text_name[i], df_l$color_name[i], df_l$format_name[i])
          # Increment the progress bar, and update the detail text.
          incProgress(1/n, detail = paste("Painting", i))
        }
      })
      
      df_long_sh_color = df_long_sh_color_c2
      withProgress(message = 'Custom Formatting - Column 2', value = 0, {
        # Number of times we'll go through the loop
        n <- (dim(df_format_inputs$data)[1])
        for (i in 1:dim(df_format_inputs$data)[1]) {
          df_l = df_format_inputs$data
          style_xlsx(wb, "testing", 2, df_l$match_name[i], df_l$text_name[i], df_l$color_name[i], df_l$format_name[i]) 
          # Increment the progress bar, and update the detail text.
          incProgress(1/n, detail = paste("Painting", i))
        }
      })

      return(wb)
    }
    
    wb_excel$data = workbook_created(df_long_symbol_excel$data, 39, 24)
    wb_openo$data = workbook_created(df_long_symbol_openo$data, 39, 30)
    
    output$download_5a_excel <- renderUI({
      output$download_excel = downloadHandler(
        filename = function() {
          paste0('pull_sheet_excel_',input$date_value,'.xlsx')
        },
        content = function(file) {
          saveWorkbook(wb_excel$data, file, overwrite = TRUE)
        }
      )
      
      downloadButton("download_excel", label = "Excel Download", class = "btn-block")
    })
    
    output$download_5a_openo <- renderUI({
      output$download_openo = downloadHandler(
        filename = function() {
          paste0('pull_sheet_openo_',input$date_value,'.xlsx')
        },
        content = function(file) {
          saveWorkbook(wb_openo$data, file, overwrite = TRUE)
        }
      )
      
      downloadButton("download_openo", label = "Open Office Download", class = "btn-block")
    })
    
    output$step5b_text <- renderUI({
      HTML("Download and Check Format Results.<br>
           <ul>
            <li>If issues redue Step 5a.
            <li>If no issues you can print. 
           <ul>")
    })
    
    output$step5a = renderText(paste0("Step 5a ",emo::ji('heavy_check_mark')))
    
    output$step5 <- renderUI(
      renderText(paste0(emo::ji('heavy_check_mark'), " Step 5 is Complete! ",emo::ji('party_popper')))
    )
    output$step5_ <- renderUI(
      renderText(paste0(emo::ji('partying_face'),
                        emo::ji('partying_face'),
                        " DASHBOARD DONE! "
                        ,emo::ji('partying_face')
                        ,emo::ji('partying_face')))
    )
    
    
    member_end = df_long_del$data |> 
      filter(rank >= 0) |> 
      select(group_name, firstname, lastname) |> 
      distinct() |> 
      group_by(group_name) |> 
      summarise(n_end = n())
    
    output$preview5b = renderTable(member_start$data |> 
                                      full_join(member_end
                                                , by = c('group_name')) |> 
                                      mutate(difference = n_end - n_start) |> 
                                      select(Group = 'group_name'
                                             , 'Member Count - Start' = n_start
                                             , 'Member Count - End' = n_end
                                             , Change = difference)
    )
    
  })
  
  ##Move this up (Delete step) or remove entirely???
  #### Member Count - End
  
  observe({
    # Make sure theme is kept current with desired
    session$setCurrentTheme(
      bs_theme_update(my_theme, bootswatch = input$current_theme)
    )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
