library(shiny)
library(shinyjs)

ui <- fluidPage(
  # Reference the custom CSS file
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$style(HTML("
      .custom-plot {
        background: transparent !important;
      }
      .custom-plot-container {
        background: transparent !important;
      }
    "))
  ),
  tags$div(
    class = "logo-container",
    tags$img(src = "logo.png", alt = "Logo")
  ),
  # Title and subtitle for the Shiny app
  fluidRow(
    column(12,
           h1("TeenGrowth"),
           h4("Individualized expected body weights for young people with eating disorders"))
  ),
  useShinyjs(),  # Initialize shinyjs
  tabsetPanel(
    id = "main_tabs",
    tabPanel("Data Input",
             sidebarLayout(
               sidebarPanel(
                 class = "container-background",
                 radioButtons("data_source", "Data Input:",
                              choices = list("Use Demo Data" = "demo", "Upload Your Own Data" = "upload"),
                              selected = "demo"),
                 conditionalPanel(
                   condition = "input.data_source == 'upload'",
                   fileInput("file1", "Upload Excel or CSV File with Growth Chart Data",
                             accept = c(".xlsx", ".xls", ".csv"))
                 ),
                 conditionalPanel(
                   condition = "input.data_source == 'upload'",
                   radioButtons("data_type", "Is the data for one person or multiple people?",
                                choices = list("One person" = "one", "Multiple people" = "multiple"),
                                selected = "one")
                 ),
                 conditionalPanel(
                   condition = "input.data_source == 'upload'",
                   tagList(
                     h4("Check Available Data Columns:"),
                     h5("Demographics"),
                     checkboxGroupInput("demographics_columns", NULL,
                                        choices = list("ID (Required if n > 1)" = "id",
                                                       "Sex (for reference charts; defaults to Female If NA)" = "sex",
                                                       "Adult Height (Optional)" = "adult_height",
                                                       "Age at Adult Height (Optional)" = "age_adult_height",
                                                       "Eating Disorder Age of Onset (Optional)" = "ed_age_onset")
                     ),
                     h5("Age at Assessment (1 Required)"),
                     checkboxGroupInput("age_columns", NULL,
                                        choices = list("Age" = "age",
                                                       "Date of Birth + Date of Assessment" = "Dates")
                     ),
                     h5("Anthropometric (1 Required)"),
                     checkboxGroupInput("anthropometric_columns", NULL,
                                        choices = list("BMI" = "bmi",
                                                       "BMI z-score" = "bmi_z",
                                                       "BMI percentile" = "bmi_percentile",
                                                       "Height + Weight" = "Ht + Wt")
                     )
                   )
                 ),
                 actionButton("next_button_data_input", "Next")
               ),
               mainPanel(
                 h3("Raw Data"),
                 p("Note. Only the first 30 rows are displayed."),
                 tableOutput("rawdata"),
               )
             )
    ),  # End of Data Input Tab

    # Data Specification Tab
    tabPanel("Data Specification",
             sidebarLayout(
               sidebarPanel(
                 class = "container-background",
                 uiOutput("conditional_inputs"),
                 actionButton("submit_button", "Clean Data"),
                 actionButton("next_button_data_spec", "Next"),
                 actionButton("back_button_data_spec", "Back")
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Cleaned Data",
                            conditionalPanel(
                              condition = "output.data_cleaned == false",
                              h4("Clean Data Required Before Modeling", style = "color: #1A4F66;")
                            ),
                            uiOutput("clean_data_messages"),
                            p("Note: Only the first 30 rows of the table are displayed."),
                            tableOutput("cleaned_data")),
                   tabPanel("Raw Data", tableOutput("rawdata_spec"))
                 )
               )
             )
    ),  # End of Data Specification Tab

    # Model Selection Tab
    tabPanel("Model Selection",
             sidebarLayout(
               sidebarPanel(
                 class = "container-background",
                 h3("Model Selection"),
                 selectInput("person_id", "ID to model:", choices = NULL),
                 selectInput("model_type", "Select BMIz Point for Prediction",
                             choices = c("Most Recent (Prior to ED)" = "most_recent",
                                         "Mean (Prior to ED)" = "mean",
                                         "Maximum (Prior to ED)" = "max",
                                         "Most Recent and Mean Equally Weighted" = "mean+most_recent"
                             )),
                 selectInput("confidence_interval", "Select the Prediction Interval:",
                             choices = c("BMIz Window = 1" = "User-Defined",
                                         "95%" = 95,
                                         "99%" = 99)),
                 conditionalPanel(
                   condition = "output.ed_aoo_needed == true",
                   numericInput("ed_aoo", "Eating Disorder Age of Onset (Years):", value = 12, min = 1)
                 ),
                 conditionalPanel(
                   condition = "output.adult_height_needed == true",
                   numericInput("adult_height_in", "Adult Height (in):", value = NULL, min = 1)
                 ),
                 conditionalPanel(
                   condition = "output.age_adult_height_needed == true",
                   numericInput("age_adult_height", "Age at Adult Height (years):", value = NULL, min = 1)
                 ),
                 actionButton("run_model", "Run / Update Model")
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Expected BMI",
                            div(class = "custom-plot", plotOutput("BMIPlot")),
                            tableOutput("forecast_output_1")),
                   tabPanel("Expected Weight",
                            div(class = "custom-plot", plotOutput("WtPlot")),
                            tableOutput("forecast_output_2"),
                            p("Note. Expected weight calculated only for ages after reaching adult height. Use expected BMI and height at a specific age to calculate expected weight prior to reaching adult height.")),
                   tabPanel("Summary", uiOutput("summary"))
                 )
               )
             )
    ),  # End of Model Selection Tab

    # New Weight Restoration Planning Tab
    tabPanel("Weight Restoration Planning",
             h3("Weight Restoration Planning"),
             p("Under construction")
    ),

    # New Background and FAQ Tab
    tabPanel("Background and FAQ",
             h3("Background"),
             p("The TeenGrowth app is designed to provide individualized expected body weights for young people with eating disorders. It leverages individuals growth chart data to make accurate predictions and help in planning weight restoration."),
             h3("FAQ"),
             h4("How do I use this app?"),
             p("To use TeenGrowth, start by inputting data in the 'Data Input' tab. You can use demo data or upload your own data. Next, specify the data columns in the 'Data Specification' tab. Once your data is cleaned, proceed to the 'Model Selection' tab to run and view the models."),
             h4("What kind of data can I upload?"),
             p("You can upload data in CSV or Excel format. Ensure your data contains the necessary information in columns such as age, anthropometric indicators, and  other relevant information as specified in the 'Data Input' tab."),
             h4("What is the purpose of this app?"),
             p("The purpose of the TeenGrowth app is to assist healthcare providers, researchers, and families in understanding and planning weight restoration for young individuals with eating disorders. It provides a scientific basis for setting realistic and individualized weight goals.")
    )
  )  # End of tabsetPanel
)  # End of fluidPage

