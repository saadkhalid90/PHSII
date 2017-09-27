## loading required libraries
library(shiny)

## Define UI for PHS II app
fluidPage(
  ## choosing a theme for shiny app
  ##theme = shinytheme("cerulean"),
  ## Application title
  titlePanel("PHS II Dashboard"),
  
  ## UI for main panel (Tabs at the top)
  mainPanel(
    tabsetPanel(type = "tabs", 
                ## tabs on the top and their contents
                ## tab for summary of field activity 
                tabPanel("Summary",
                         hr(),
                         downloadButton("SummaryDwnld", label = "Download summary as .csv"),
                         hr(),
                         DT::dataTableOutput("summary_table")),
                ## tab for finding mismatches across forms 
                tabPanel("Mismatch",
                         selectInput("select_miss_type", label = h3("Mismatch type:"),
                                     choices = list("HL - Mother" = 1, "HL - Child" = 2,
                                                    "HC - Mother" = 3, "Mother - Child" = 4),
                                     selected = 1),
                         hr(),
                         DT::dataTableOutput("mismatch_table")),
                ## tab for major indicators
                tabPanel("Indicators",
                         hr(),
                         selectInput("select_ind_type", label = h3("Indicator Type:"),
                                     choices = list("Breastfeeding" = "BF", "Antenatal" = "AN",
                                                    "Delivery" = "D", "Postnatal" = "PN", "Immunization" = "IM",
                                                    "All-Punjab Level" = "glob"),
                                     selected = 1),
                         conditionalPanel("input.select_ind_type == 'IM'",
                                          radioButtons("age_group", label = h4("Select age group:"),
                                                       choices = list("1 - 11 months" = 1, 
                                                                      "12 - 23 months" = 2))),
                         DT::dataTableOutput("indicators_table"),
                         verbatimTextOutput("test")),
                ## tab for downloading combined data
                tabPanel("Complete Data", 
                         hr(),
                         downloadButton("hc_mother", label = "HC_Mother")),
                ## tab for internal inconsistencies in data (SUMMARY)
                tabPanel("Inconsistent",
                         hr(),
                         selectInput("select_incon_type", label = h3("Select Category:"),
                                     choices = list("Electricity/ Assets" = 1, "Others" = 2, "Filter" = 3)),
                         DT::dataTableOutput("incon_summary"),
                         verbatimTextOutput("check")),
                ## tab for detailed inconsistencies
                tabPanel("Incon. Details", 
                         hr(),
                         selectInput("incon_det_type", label = h3("Select Category:"),
                                     choices = list("Electricity/ Assets" = 1, 
                                                    "Cattle" = "2",
                                                    "ANC Hospital-No test" = "3",
                                                    "AF-Glasses" = "4",
                                                    "AF-Hearing Aid" = "5",
                                                    "Card seen - No dates" = "6",
                                                    "All Dates-Wrong skip" = "7",
                                                    "All but BCG/ OPV0" = "8",
                                                    "Filters" = "9",
                                                    "Negative age" = "10",
                                                    "Missing dates - IM4 = Yes" = "11")),
                         conditionalPanel("input.incon_det_type == 1",
                                          radioButtons("asset", label = h3("Select Asset"),
                                                       choices = list("Television" = "hc8_opt3", "Fridge" = "hc8_opt5", "Computer" = "hc8_opt7", 
                                                                      "AC" = "hc8_opt8", "Washing Machine" = "hc8_opt9", "Fan" = "hc8_opt10", "Iron" = "hc8_opt13",
                                                                      "Dunky Pump" = "hc8_opt15", "Electricity fuel" = "hc6"), 
                                                       selected = "hc8_opt3")
                         ),
                         DT::dataTableOutput("without_elec")),
                ## tab for checking enumerator progress
                tabPanel("Enum. Prog", 
                         hr(),
                         downloadButton("EnumProgDwnld", label = "Download Progress"),
                         radioButtons("prog_type", label = h4("Summary type:"),
                                      choices = list("Date-wise" = "date_wise",
                                                     "Consolidated" = "consol")),
                         DT::dataTableOutput("Enum_Prog"))
    )
  )
)