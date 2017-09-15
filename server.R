## loading the required libraries
library(shiny)
library(rdrop2)
library(dplyr)
library(lubridate)
library(formattable)
library(stringr)

# Define server logic for listing dashboard
function(input, output) {
  dataIn <- reactive({
    ## saved application token to access dropbox through R 
    token <- readRDS("droptoken.rds")
    ##  accessing dropbox acount
    drop_acc(dtoken = token)
    
    ## reading in file from dropbox (type_ 'todays date'.csv)
    filename_mother <- paste("mother_comb_", Sys.Date(), ".csv", sep = "")
    filename_child <- paste("child_comb_", Sys.Date(), ".csv", sep = "")
    filename_hl <- paste("hl_comb_", Sys.Date(), ".csv", sep = "")
    filename_hc <- paste("hc_comb_", Sys.Date(), ".csv", sep = "")
        
    mother <- drop_read_csv(paste("PHS II Data/", filename_mother, sep = ""), sep = ",", dtoken = token)
    child <- drop_read_csv(paste("PHS II Data/", filename_child, sep = ""), sep = ",", dtoken = token)
    
    # mother$district <- as.character(cluster_districts$NAME.OF.DISTRICT[mother$hh1])
    # child$district <- as.character(cluster_districts$NAME.OF.DISTRICT[child$hh1])
    
    hc <- drop_read_csv(paste("PHS II Data/", filename_hc, sep = ""), sep = ",", dtoken = token, na.strings = c("Lat N/A", "Lng N/A"))
    hl <- drop_read_csv(paste("PHS II Data/", filename_hl, sep = ""), sep = ",", dtoken = token)
    
    hl$hh1 <- as.integer(hl$hh1)
    hc$hh1 <- as.integer(hc$hh1)
    mother$hh1 <- as.integer(mother$hh1)
    child$hh1 <- as.integer(child$hh1)
    
    hl$hh2 <- as.integer(hl$hh2)
    hc$hh2 <- as.integer(hc$hh2)
    mother$hh2 <- as.integer(mother$hh2)
    child$hh2 <- as.integer(child$hh2)
    
    ## filter out mock entries
    hl <- hl[!(grepl(pattern = "CHECK", x = hl$hl2)), ]
    hc <- hc[!(grepl(pattern = "ATIF", x = hc$hh3name)), ]
    mother <- mother[!(grepl(pattern = "MOTHER", x = mother$wm1)), ]
    child <- child[!(grepl(pattern = "CHECK", x = hl$hl2)), ]
    
    ## removing all values with NA values in hh1
    hl <- hl[!(is.na(hl$hh1)), ]
    hc <- hc[!(is.na(hc$hh1)), ]
    child <- child[!(is.na(child$hh1)), ]
    mother <- mother[!(is.na(mother$hh1)), ]
    
    return(list(mother = mother, child = child, hc = hc, hl = hl))
  })
  
  misMatch <- reactive({
    missing_cases <- identify_missing(hl = dataIn()$hl, hc = dataIn()$hc, mother = dataIn()$mother, child = dataIn()$child)
    return(missing_cases)
  })
  
  create_summary_ <- reactive({
    summary_table <- create_summary(hl_data = dataIn()$hl, 
                                    hc_data = dataIn()$hc, 
                                    mother_data = dataIn()$mother, 
                                    child_data = dataIn()$child)
    return(summary_table)
  })
  
  create_indicators_table <- reactive({
    switch(input$select_ind_type,
      "BF" = {
        split_mother <- time_value_split(var = "mn25", data_set = dataIn()$mother, remove_orig = T)
        indic <- as.data.frame(compute_BF_indic(split_mother, dataIn()$child, "district"))
      },
      "AN" = {
        indic <- as.data.frame(compute_AN_indic(mother_data = dataIn()$mother, type = "district"))
      },
      "D" = {
        indic <- as.data.frame(compute_delivery_indic(dataIn()$mother, type = "district"))
      },
      "PN" = {
        mother_time_split <- time_value_split(var = "pn2", data_set = dataIn()$mother, remove_orig = T)
        mother_time_split <- time_value_split(var = "pn12a", data_set = mother_time_split, remove_orig = T)
        mother_time_split <- time_value_split(var = "pn12b", data_set = mother_time_split, remove_orig = T)
        mother_time_split <- time_value_split(var = "pn21a", data_set = mother_time_split, remove_orig = T)
        mother_time_split <- time_value_split(var = "pn21b", data_set = mother_time_split, remove_orig = T)
        
        indic <- as.data.frame(compute_PN_indic(mother_data = mother_time_split, type = "district"))
      }, 
      "IM" = {
        indic <- compute_immun_district(mother_data = dataIn()$mother, child_data = dataIn()$child)
      }
    )
    return(list(indic = indic, col_names = names(indic)))
  })
  
  output$indicators_table <- DT::renderDataTable(DT::datatable(create_indicators_table()[['indic']], 
                                                               options = list(pageLength = 50)) %>% 
                                                   DT::formatPercentage(create_indicators_table()[['col_names']], 2))
  output$summary_table <- DT::renderDataTable(DT::datatable(create_summary_() %>%
                                                              arrange(desc(HHs)), 
                                                            filter = 'top', options = list(pageLength = 50)))
  output$mismatch_table <- DT::renderDataTable({
    options_ <- c("hl_moth", "hl_child", "HH_moth", "moth_child")
    selected <- options_[as.numeric(input$select_miss_type)]
    DT::datatable(misMatch()[[selected]], filter = 'top')})
  
  output$SummaryDwnld <- downloadHandler(
    filename = function() {
      paste('summary_', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(create_summary_(), con)
    }
  )
  
  output$hc_mother <- downloadHandler(
    filename = function() {
      paste('hc_mother_', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(complete_interviews(mother_data = dataIn()$mother, 
                                    children_data = dataIn()$child, 
                                    hc_data = dataIn()$hc)[['hc_mother']], 
                row.names = F, 
                con)
    }
  )
  
  output$incon_summary <- DT::renderDataTable({
    if (input$select_incon_type == 1){
      DT::datatable(elect_sum(hc_data = dataIn()$hc), options = list(pageLength = 50))
    }
    else if (input$select_incon_type == 3){
      DT::datatable(filter_incon(mother_data = dataIn()$mother, hc_data = dataIn()$hc)$summary, 
                    options = list(pageLength = 50))
    }
    else {
      DT::datatable(non_elect_sum(hc_data = dataIn()$hc,
                                  mother_data = dataIn()$mother,
                                  child_data = dataIn()$child))
    }
  })
  output$without_elec <- DT::renderDataTable(DT::datatable({
    if (input$incon_det_type == 1){
      without_electric(dataIn()$hc, var2 = input$asset)  
    }
    else {
      switch(input$incon_det_type,
             "2" = {
               cattle_incon(hc_data = dataIn()$hc)
             },
             "3" = {
               hosp_notest(mother_data = dataIn()$mother, hc_data = dataIn()$hc)
             },
             "4" = {
               AF_incon(mother_data = dataIn()$mother, hc_data = dataIn()$hc)$glasses
             },
             "5" = {
               AF_incon(mother_data = dataIn()$mother, hc_data = dataIn()$hc)$h_aid
             },
             "6" = {
               immun_incon(child_data = dataIn()$child, hc_data = dataIn()$hc)$card_seen_ND
             },
             "7" = {
               ADWS<- immun_incon(child_data = dataIn()$child, hc_data = dataIn()$hc)$all_Dates_WS
               ## removing the NA rows 
               ADWS <- ADWS[!(is.na(ADWS$hh1)), ]
             },
             "8" = {
               immun_incon(child_data = dataIn()$child, hc_data = dataIn()$hc)$all_but_BCG
             },
             "9" = {
               filter_incon(mother_data = dataIn()$mother, hc_data = dataIn()$hc)$filter
             },
             "10" = {
               id_neg_age(child_data = dataIn()$child, hc_data = dataIn()$hc)
             }
      )
      
    }
    }, options = list(pageLength = 50)))
  ## output$check <-  renderPrint(summary(immun_incon(child_data = dataIn()$child, hc_data = dataIn()$hc)$all_Dates_WS))
}


# mother_drop <- drop_read_csv('PHS II Data/mother_comb_2017-09-10.csv')
# child_drop <- drop_read_csv('PHS II Data/child_comb_2017-09-10.csv')
# 
# mother_drop$district <- cluster_districts$NAME.OF.DISTRICT[mother_drop$hh1]
# child_drop$district <- cluster_districts$NAME.OF.DISTRICT[child_drop$hh1]
# 
# unique(child_drop$district)
# unique(mother_drop$district)
