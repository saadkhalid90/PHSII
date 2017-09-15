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
                tabPanel("Summary",
                         hr(),
                         downloadButton("SummaryDwnld", label = "Download summary as .csv"),
                         hr(),
                         DT::dataTableOutput("summary_table")),
                tabPanel("Mismatch",
                         selectInput("select_miss_type", label = h3("Mismatch type:"),
                                     choices = list("HL - Mother" = 1, "HL - Child" = 2,
                                                    "HC - Mother" = 3, "Mother - Child" = 4),
                                     selected = 1),
                         hr(),
                         DT::dataTableOutput("mismatch_table")),
                tabPanel("Indicators",
                         hr(),
                         selectInput("select_ind_type", label = h3("Indicator Type:"),
                                     choices = list("Breastfeeding" = "BF", "Antenatal" = "AN",
                                                    "Delivery" = "D", "Postnatal" = "PN",
                                                    "Immunization" = "IM"),
                                     selected = 1),
                         DT::dataTableOutput("indicators_table"),
                         verbatimTextOutput("test")),
                tabPanel("Complete Data", 
                         hr(),
                         downloadButton("hc_mother", label = "HC_Mother")),
                tabPanel("Inconsistent",
                         hr(),
                         selectInput("select_incon_type", label = h3("Select Category:"),
                                     choices = list("Electricity/ Assets" = 1, "Others" = 2, "Filter" = 3)),
                         DT::dataTableOutput("incon_summary"),
                         verbatimTextOutput("check")),
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
                                                    "Negative age" = "10")),
                         conditionalPanel("input.incon_det_type == 1",
                                          radioButtons("asset", label = h3("Select Asset"),
                                                       choices = list("Television" = "hc8_opt3", "Fridge" = "hc8_opt5", "Computer" = "hc8_opt7", 
                                                                      "AC" = "hc8_opt8", "Washing Machine" = "hc8_opt9", "Fan" = "hc8_opt10", "Iron" = "hc8_opt13",
                                                                      "Dunky Pump" = "hc8_opt15", "Electricity fuel" = "hc6"), 
                                                       selected = "hc8_opt3")
                         ),
                         DT::dataTableOutput("without_elec"))
    )
  )
)