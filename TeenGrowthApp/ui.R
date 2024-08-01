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
  # Header container with flexbox for positioning logos and title
  tags$div(
    class = "header-container",
    tags$div(class = "logo-container-left",
             tags$img(src = "TeenGrowth.png", alt = "TeenGrowth Logo", style = "width: 150px; height: auto;")
    ),
    tags$div(
      class = "title-container",
      h4("Individualized expected body weights for young people with eating disorders")
    ),
    tags$div(class = "logo-container-right",
             tags$img(src = "logo.png", alt = "Logo", style = "width: 80px; height: auto;")
    )
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
                   fileInput("file1", "Upload Excel (.xlsx) or CSV File with Growth Chart Data",
                             accept = c(".xlsx", ".csv"))
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
                                                       "Eating Disorder Age of Onset (Optional)" = "ed_age_onset")
                     ),
                     h5("Age at Assessment (Choose One)"),
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
                 tableOutput("rawdata")
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
                                         "80%" = 80,
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
                 actionButton("run_model", "Run / Update Model"),
                 actionButton("next_button_model_selection", "Next"),
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

    # Weight Restoration Planning Tab
    tabPanel("Weight Restoration Planning",
             sidebarLayout(
               sidebarPanel(
                 class = "container-background",
                 h3("Weight Restoration Planning"),
                 uiOutput("conditional_weight_restoration_inputs"),
                 actionButton("plot_weight_restoration", "Plot Weight Restoration Plan"),
                 actionButton("back_button_weight_restoration", "Back")
               ),
               mainPanel(
                 div(class = "custom-plot", plotOutput("wt_restore_graph"))
               )
             )
    ),  # End of Weight Restoration Planning Tab

    # Background and FAQ Tab
    tabPanel("Background and FAQ",
             h3("Background"),
             p("The TeenGrowth app is designed to provide individualized expected body weights for young people with eating disorders. It leverages individuals' growth chart data to make accurate predictions and help in planning weight restoration."),
             h3("FAQ"),
             h4("What is the purpose of this app?"),
             p("The purpose of the TeenGrowth app is to assist healthcare providers and researchers in understanding and planning weight restoration for young individuals with eating disorders. It provides individualized weight goals, based on prior growth data."),
             h4("How do I use this app?"),
             p("To use TeenGrowth, start by inputting data in the 'Data Input' tab. You can use demo data or upload your own data. Next, specify the data columns in the 'Data Specification' tab. Once your data is cleaned, proceed to the 'Model Selection' tab to run and view the models. Finally, you can plan for weight restoration in the 'Weight Restoration Planning' tab. Steps must proceed in order for everything to work appropriately."),
             h4("What kind of data can I upload?"),
             p("You can upload data in .csv or .xlsx format. Ensure your data contains the necessary information in columns such as age,
               anthropometric indicators, and other relevant information as specified in the 'Data Input' tab.
               In order for models to work properly, you must have at least TWO datapoints for each participant after age 2.1 years and prior to ED onst to use 80%, 95% and 99% models, and ONE datapoint prior to ED onset to use the BMIz window = 1 models."),
             h4("How far out can I predict?"),
             p("The TeenGrowth app can predict expected BMIz and weight up to 10 years after the most recent data point PRIOR to ED onset. For instance, if the most recent datapoint prior to ED onset was at age 11, prediction will go to age 21."),
             h4("Will this work for young adults?"),
             p("The TeenGrowth app is designed for individuals with ED onset prior to age 21, and can be used used up to age 36.
               Age 21 BMIz reference values are used to forecast from age 21-36 and currently assume stability after age 20."),
             h4("I'm getting stuck! My models won't run"),
             p("Sparse data (< 4 prediction points) will occasionally produce obvious modeling errors with 80%, 95%, and 99% prediction intervals -- try +/- 0.5 BMIz models if errors emerge with other models. The TeenGrowth Package is still in very early phases of development and there are likely to be unanticipated bugs and issues -- particularly with tricky data formats like dates. Dr. Schaumberg (kschaumberg@wisc.edu) is happy to help troubleshoot any issues you may be having. Please reach out to her with any questions or concerns."),
             h4("Do I have to specify sex for trans and non-binary individuals?"),
             p("The TeenGrowth app currently relies on CDC and WHO growth chart reference data,
             which are sex-specific, and therefore a binary sex specification is required."),
             h3("Package and Preprint Details"),
             p("Find information about the", a("R package", href = "https://embark-lab.github.io/TeenGrowth/index.html")),
             p("Find detailed information about the development and use of the TeenGrowth app in the following", a("preprint", href = "https://osf.io/preprints/psyarxiv/vwj7s?view_only="))
    )
  )  # End of Background and FAQ Tab

  # Close the tabsetPanel
)  # End of fluidPage
