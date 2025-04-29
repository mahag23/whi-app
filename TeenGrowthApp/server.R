library(dplyr)
library(lubridate)
library(TeenGrowth)
library(readxl)


server <- function(input, output, session) {
  demo_data <- TeenGrowth::demo

#### 0. Functions ####


  show_error_modal <- function(message) {
    showModal(modalDialog(
      title = "Error",
      message,
      footer = modalButton("OK")
    ))
  }

  read_uploaded_data <- function(file) {
    ext <- tools::file_ext(file$name)
    df <- switch(ext,
                 "csv" = read.csv(file$datapath, stringsAsFactors = FALSE),
                 "xlsx" = readxl::read_excel(file$datapath),
                 "xls" = readxl::read_excel(file$datapath),
                 stop("Unsupported file type"))
  }

  parse_and_format_dates <- function(df, date_columns) {
    for (col in date_columns) {
      if (!is.null(col) && col %in% names(df)) {
        print(paste("Parsing column:", col))
        print(paste("Original values:", paste(df[[col]], collapse = ", ")))
        if (is.numeric(df[[col]])) {
          df[[col]] <- as.Date(df[[col]], origin = "1899-12-30")
        } else if (is.character(df[[col]])) {
          tryCatch({
            df[[col]] <- parse_date_time(df[[col]], orders = c("mdy", "ymd", "dmy"))
            df[[col]] <- format(df[[col]], "%Y-%m-%d")  # Convert to ISO format
            df[[col]] <- as.Date(df[[col]])  # Ensure column is Date class

          }, error = function(e) {
            print(paste("Error parsing column:", col, "with message:", e$message))
            df[[col]] <- NA  # Set problematic values to NA
          })
        }
      }}
    return(df)
  }

  # Helper function to set default values based on data source
  get_default_value <- function(column, default_demo_value) {
    if (input$data_source == "demo") {
      return(default_demo_value)
    } else {
      return(NULL)
    }
  }

#### 1. Data Ingestion ####

  ##### 1.1 Demo vs. Upload #####

  data <- reactive({
    if (input$data_source == "upload") {
      req(input$file1)
      read_uploaded_data(input$file1)
      updateCheckboxGroupInput(session, "demographics_columns", selected = character(0))
      updateCheckboxGroupInput(session, "age_columns", selected = character(0))
      updateCheckboxGroupInput(session, "anthropometric_columns", selected = character(0))
      read_uploaded_data(input$file1)
    } else {
      demo_data
    }
  })

    ##### 1.2 Prep data #####

      ###### 1.2.1 Demo Prep ######
tx_start_date_for_demo_participants <- reactive({
  if (input$data_source == 'demo') {
    switch(as.character(input$person_id),
           "1" = Sys.Date() - lubridate::days(120), # Example: Tx start date for participant 1
           "2" = Sys.Date() - lubridate::days(150), # Example: Tx start date for participant 2
           "3" = Sys.Date() - lubridate::days(180), # Example: Tx start date for participant 3
           NULL
    )
  } else {
    Sys.Date()
  }
})

  dob_for_demo_participants <- reactive({
    if (input$data_source == 'demo') {
      switch(as.character(input$person_id),
             "1" = Sys.Date() - lubridate::duration(years = 16.3),
             "2" = Sys.Date() - lubridate::duration(years = 15.4),
             "3" = Sys.Date() - lubridate::duration(years = 15.8),
             NULL
      )
    } else {
      NULL
    }
  })

      ###### 1.2.2 Parse Dates ######
  parsed_data <- reactive({
    df <- data()
    date_columns <- c(input$dob_column, input$assessment_date_column)  # Get the date columns from the user inputs
    df <- parse_and_format_dates(df, date_columns)
    return(df)
  })


  # Reactive to check if DOB is missing
  dob_missing <- reactive({
    cleaned_df <- cleaned_data()
    "dob" %in% input$age_columns && !("dob" %in% colnames(cleaned_df))
  })

#### 2. Data Input  ####

  ##### 2.1 Identify checkboxes to include or select #####
  observeEvent(input$data_source, {
    if (input$data_source == "demo") {
      updateCheckboxGroupInput(session, "demographics_columns", selected = c("id", "sex", "adult_height", "ed_age_onset"))
      updateCheckboxGroupInput(session, "age_columns", selected = c("age"))
      updateCheckboxGroupInput(session, "anthropometric_columns", selected = c("Ht + Wt"))
    } else if (input$data_source == "upload") {
      updateCheckboxGroupInput(session, "demographics_columns", selected = NULL)
      updateCheckboxGroupInput(session, "age_columns", selected = NULL)
      updateCheckboxGroupInput(session, "anthropometric_columns", selected = NULL)
    }

    cleaned_data(NULL)
    cleaned_data_status(FALSE)
    clean_data_messages(NULL)
  })

  observeEvent(input$file1, {
    cleaned_data(NULL)
    cleaned_data_status(FALSE)
    clean_data_messages(NULL)
  })

  ed_aoo_present <- reactive({
    "ed_age_onset" %in% colnames(data())
  })


  ##### 2.2 Data specification tab inputs #####
  output$conditional_inputs <- renderUI({
    req(input$data_source == 'upload' || input$data_source == 'demo')

    inputs <- list(
      selectInput("height_unit", "Height Unit", choices = c("in", "cm"), selected = get_default_value("height_unit", "in")),
      selectInput("weight_unit", "Weight Unit", choices = c("lb", "kg"), selected = get_default_value("weight_unit", "lb")),
      selectInput("age_unit", "Age Unit", choices = c("years", "months", "days"), selected = get_default_value("age_unit", "years"))
    )

    columns_list <- list(
      "id" = list(textInput("id_column", "Name of the id column:", value = get_default_value("id_column", "participant"))),
      "age" = list(textInput("age_column", "Name of the age column:", value = get_default_value("age_column", "age"))),
      "sex" = list(textInput("sex_column", "Name of the sex column (for growth chart reference):", value = get_default_value("sex_column", "sex"))),
      "adult_height" = list(textInput("adult_height_column", "Name of the adult height column:", value = get_default_value("adult_height_column", "adult_height_in"))),
      "age_adult_height" = list(textInput("age_adult_height_column", "Name of the age at adult height column:", value = get_default_value("age_adult_height_column", "agemos_adult_ht"))),
      "dob" = list(textInput("dob_column", "Name of the date of birth column:")),
      "assessment_date" = list(textInput("assessment_date_column", "Name of the date of assessment column:")),
      "bmi" = list(textInput("bmi_column", "Name of the BMI column:")),
      "bmi_z" = list(textInput("bmi_z_column", "Name of the BMI z-score column:", value = get_default_value("bmi_z_column", "bmiz"))),
      "bmi_percentile" = list(textInput("bmi_percentile_column", "Name of the BMI percentile column:")),
      "Ht + Wt" = list(
        textInput("height_value_column", "Name of the height column:", value = get_default_value("height_value_column", "height")),
        textInput("weight_column", "Name of the weight column:", value = get_default_value("weight_column", "weight"))
      ),
      "ed_age_onset" = list(textInput("aao_column", "Name of the eating disorder age of onset column:", value = get_default_value("aao_column", "ed_aoo")))
    )


    for (col in c(input$demographics_columns, input$age_columns, input$anthropometric_columns)) {
      if (col %in% names(columns_list)) {
        inputs <- append(inputs, columns_list[[col]])
      }
    }

    if ("Dates" %in% input$age_columns) {
      inputs <- append(inputs, columns_list[["dob"]])
      inputs <- append(inputs, columns_list[["assessment_date"]])
    }

      if (input$data_type == "one") {
        if (!("Dates" %in% input$age_columns) && !("age" %in% input$age_columns)) {
          inputs <- append(inputs, list(dateInput("dob", "What is the individual's date of birth?")))
        }
        # add an optional age at adult height input
          if (!("age_adult_height" %in% input$demographics_columns)) {
            inputs <- append(inputs, list(
              numericInput("aheight_age", "What is the individual's age at adult height (Optional)?", value = NULL, min = 1, max = 25)
            ))
          }
        if (!("sex" %in% input$demographics_columns)) {
          inputs <- append(inputs, list(selectInput("sex", "What is the individual's sex at birth?", choices = c("Female", "Male"))))
          }
    }

    do.call(tagList, inputs)
  })

  cleaned_data_status <- reactiveVal(FALSE)
  cleaned_data <- reactiveVal()
  clean_data_messages <- reactiveVal(NULL)



#### 3. Clean Data ####
  ##### 3.1 Ensures all data is included before data cleaning #####
  observeEvent(input$next_button_data_input, {
    if (input$data_source == "upload" && (
      !("id" %in% input$demographics_columns) && input$data_type == "multiple" ||
      !("age" %in% input$age_columns) && !("Dates" %in% input$age_columns) ||
      !any(c("bmi", "bmi_z", "bmi_percentile", "Ht + Wt") %in% input$anthropometric_columns)
    )) {
      showModal(modalDialog(
        title = "Missing Required Data",
        "Please ensure that all required fields are selected before proceeding.",
        footer = modalButton("OK")
      ))
    } else {
      updateTabsetPanel(session, "main_tabs", selected = "Data Specification")
    }
  })

  ##### 3.2 Things that happen when submit button is pushed to clean data #####

  observeEvent(input$submit_button, {
    df <- parsed_data()
    print("Initial data:")
    print(head(df))

    # Add constants if uploading single participant
    if (input$data_source != "demo" && input$data_type == "one") {
      if (!("dob" %in% colnames(df)) && !is.null(input$dob)) {
        df <- df %>% mutate(dob = input$dob)
        updateTextInput(session, "dob_column", value = "dob")
      }
      if (!("assessment_date" %in% colnames(df)) && !is.null(input$assessment_date)) {
        df <- df %>% mutate(assessment_date = input$assessment_date)
        updateTextInput(session, "assessment_date_column", value = "assessment_date")
      }
      if (!("sex" %in% colnames(df)) && !is.null(input$sex)) {
        df <- df %>% mutate(sex = input$sex)
      }
      # Decide whether we truly have an "age_adult_height" column
      if ("age_adult_height" %in% names(df)) {            # comes from the file
        age_adult_ht_col <- "age_adult_height"

      } else if (!is.null(input$aheight_age) &&            # typed in the UI
                 input$aheight_age != "") {

        df <- df %>% mutate(age_adult_height = as.numeric(input$aheight_age))
        age_adult_ht_col <- "age_adult_height"

      } else {                                             # optional & missing
        age_adult_ht_col <- NULL
      }

      # add the age-at-adult-height column (years) if the user supplied one
      if (!"age_adult_height" %in% names(df) && !is.null(input$aheight_age)) {

        df <- df %>%
          mutate(age_adult_height = as.numeric(input$aheight_age))   # <── force numeric

        updateTextInput(session,
                        inputId = "age_adult_height_column",
                        value   = "age_adult_height")
      }
      if (!("ed_age_onset" %in% colnames(df)) && !is.null(input$symptoms)) {
        df <- df %>% mutate(ed_age_onset = input$symptoms)
        updateTextInput(session, "aao_column", value = "ed_age_onset")
      }
    }

    df <- df %>%
      mutate(across(all_of(c(input$dob_column, input$assessment_date_column)), as.Date))

    print("Updated data:")
    print(head(df))

    required_columns <- c(
      if ("id" %in% input$demographics_columns) input$id_column else NULL,
      if ("age" %in% input$age_columns) input$age_column else NULL,
      if ("sex" %in% input$demographics_columns) input$sex_column else NULL,
      if ("adult_height" %in% input$demographics_columns) input$adult_height_column else NULL,
      if ("age_adult_height" %in% input$demographics_columns) input$age_adult_height_column else NULL,
      if ("Dates" %in% input$age_columns) input$dob_column else NULL,
      if ("Dates" %in% input$age_columns) input$assessment_date_column else NULL,
      if ("bmi" %in% input$anthropometric_columns) input$bmi_column else NULL,
      if ("bmi_z" %in% input$anthropometric_columns) input$bmi_z_column else NULL,
      if ("bmi_percentile" %in% input$anthropometric_columns) input$bmi_percentile_column else NULL,
      if ("Ht + Wt" %in% input$anthropometric_columns) input$height_value_column else NULL,
      if ("Ht + Wt" %in% input$anthropometric_columns) input$weight_column else NULL,
      if ("ed_age_onset" %in% input$demographics_columns) input$aao_column else NULL
    )

    missing_columns <- setdiff(required_columns, colnames(df))

    if (length(missing_columns) > 0) {
      showModal(modalDialog(
        title = "Missing Required Columns",
        paste("The following columns are missing in the dataset:", paste(missing_columns, collapse = ", ")),
        footer = modalButton("OK")
      ))
      return(NULL)
    }

    messages <- capture.output({
      cleaned <- TeenGrowth::clean_data(
        df,
        id_col_name = if ("id" %in% input$demographics_columns) input$id_column else NULL,
        age_col_name = if ("age" %in% input$age_columns) input$age_column else NULL,
        dob_col_name = if ("Dates" %in% input$age_columns) input$dob_column else if (!is.null(input$dob)) "dob" else NULL,
        date_assessed_col_name = if ("Dates" %in% input$age_columns) input$assessment_date_column else if (!is.null(input$assessment_date)) 'assessment_date' else NULL,
        age_unit = input$age_unit,
        sex_col_name = if ("sex" %in% input$demographics_columns) input$sex_column else if (!is.null(input$sex)) "sex" else NULL,
        ht_col_name = if ("Ht + Wt" %in% input$anthropometric_columns) input$height_value_column else NULL,
        ht_unit = input$height_unit,
        wt_col_name = if ("Ht + Wt" %in% input$anthropometric_columns) input$weight_column else NULL,
        wt_unit = input$weight_unit,
        bmi_col_name = if ("bmi" %in% input$anthropometric_columns) input$bmi_column else NULL,
        bmiz_col_name = if ("bmi_z" %in% input$anthropometric_columns) input$bmi_z_column else NULL,
        pct_col_name = if ("bmi_percentile" %in% input$anthropometric_columns) input$bmi_percentile_column else NULL,
        data_source = 'cdc',
        adult_ht_col_name = if (!is.null(input$adult_height_column)) input$adult_height_column else NULL,
        age_adult_ht_col_name = age_adult_ht_col,
        ed_aoo_col_name = if ("ed_age_onset" %in% input$demographics_columns) input$aao_column else if (!is.null(input$symptoms)) 'ed_age_onset' else NULL
      )

      # Reattach DOB if needed
      original_df <- parsed_data()
      dob_column_name <- input$dob_column
      id_column_name <- input$id_column

      if (!is.null(dob_column_name) && dob_column_name != "" && dob_column_name %in% names(original_df)) {
        if (!is.null(input$data_type) && input$data_type == "multiple" &&
            !is.null(id_column_name) && id_column_name %in% names(original_df) && id_column_name %in% names(cleaned)) {
          dob_map <- original_df %>% select(!!sym(id_column_name), !!sym(dob_column_name))
          cleaned <- cleaned %>%
            left_join(dob_map, by = id_column_name)
          names(cleaned)[names(cleaned) == dob_column_name] <- "dob"
        } else {
          cleaned$dob <- original_df[[dob_column_name]][1]
        }
      }

      cleaned_data(cleaned)
    }, type = "message")

    clean_data_messages(paste(messages, collapse = "<br>"))
    cleaned_data_status(TRUE)

    updateSelectInput(session, "person_id", choices = unique(cleaned_data()[["id"]]))
  })

  ##### 3.4 Output Cleaned and raw data #####
  output$cleaned_data <- renderTable({
    req(cleaned_data())
    head(cleaned_data(), 30)
  })

  output$clean_data_messages <- renderUI({
    req(clean_data_messages())
    HTML(clean_data_messages())
  })

  output$rawdata <- renderTable({
    req(data())
    head(data(), 30)
  })

  output$rawdata_spec <- renderTable({
    req(data())
    head(data(), 30)
  })

#### 4. Model Data ####
  ##### 4.1. Runs Model when 'Run Model' is selected in Model Selection tab #####
  observe({
    req(cleaned_data())
    n_ids <- length(unique(cleaned_data()$id))

    if (n_ids <= 1) {
      shinyjs::hide("person_id")
    } else {
      shinyjs::show("person_id")
    }
  })

  model_data <- eventReactive(input$run_model, {
    req(cleaned_data(), input$person_id, input$model_type, input$confidence_interval)

    ed_aoo_input <- as.numeric(age_in_months(input$ed_aoo, age_unit = input$age_unit))
    cleaned_df <- cleaned_data()

if (input$data_type == "multiple" && "id" %in% names(cleaned_df)) {
  # Only filter by ID if the user uploaded multiple participants
  selected_data <- cleaned_df[cleaned_df[["id"]] == input$person_id, ]
} else {
  # Otherwise, for a single participant, use all rows
  selected_data <- cleaned_df
}

    selected_data <- cleaned_df[cleaned_df[["id"]] == input$person_id, ]

    if (all(is.na(selected_data$adult_height_in))) {
      selected_data$adult_height_in <- input$adult_height_in
    }
    if (all(is.na(selected_data$agemos_adult_ht))) {
      selected_data$agemos_adult_ht <- (input$age_adult_height * 12)
    }
    if (all(is.na(selected_data$agemos_ed_onset))) {
      selected_data$agemos_ed_onset <- (input$ed_aoo * 12)
    }

    selected_data <- selected_data %>%
      mutate(agemos_ed_onset = ifelse(!is.na(agemos_ed_onset), agemos_ed_onset, ed_aoo_input * 12))

    # QC check: Print selected_data after applying ED onset logic
    print("Final selected_data for forecast input after ED onset logic:")
    print(head(selected_data))


    forecast_input <- if (!all(is.na(selected_data$agemos_ed_onset))) {
      selected_data %>% filter(agemos < agemos_ed_onset[1])
    } else {
      selected_data
    }

    # Check if forecast_input has 0 rows
    if (nrow(forecast_input) == 0) {
      show_error_modal("No data available for prediction.
                       Please ensure there is growth data available after age 2.1 and prior to the age of eating disorder onset.")
      return(NULL)
    }
    # Check if forecast_input has only 1 row and confidence interval is not 'User-Defined'
    if (nrow(forecast_input) == 1 && input$confidence_interval != 'User-Defined') {
      show_error_modal("Prediction interval cannot be computed with only
                       1 datapoint available prior to ED onset -- use BMIz window = 1 for prediction window")
      return(NULL)
    }
    forecast_data <- if (input$confidence_interval == 'User-Defined') {
      TeenGrowth::forecast_bmi(
        data = forecast_input,
        central_value = input$model_type,
        ci = input$confidence_interval,
        lower_margin = 0.5,
        upper_margin = 0.5
      )
    } else {
      TeenGrowth::forecast_bmi(
        data = forecast_input,
        central_value = input$model_type,
        ci = as.numeric(input$confidence_interval)
      )
    }

    list(
      forecast_input = forecast_input,
      forecast_data = forecast_data,
      ed_aoo_input = ed_aoo_input,
      selected_data = selected_data
    )
  })

  observeEvent(input$run_model, {
    model_data()
  })

  observeEvent(input$main_tabs, {
    if (input$main_tabs == "Model Selection") {
      if (!cleaned_data_status()) {
        showModal(modalDialog(
          title = "Clean Data Required",
          "Please go back to the Data Specification tab and clean the data before proceeding to model selection.",
          footer = modalButton("OK")
        ))
      }
    }
  })

  ##### 4.2 Expected Weight and BMI plot outputs #####
    ###### 4.2.1 Expected Weight Plot ######
  output$WtPlot <- renderPlot({
    req(model_data())
    data <- model_data()

    if (all(is.na(data$selected_data$adult_height_in)) || all(is.na(data$selected_data$agemos_adult_ht))) {
      showModal(modalDialog(
        title = "Additional Information Required",
        "Please provide Adult Height and Age at Adult Height to plot the weight.",
        footer = modalButton("OK")
      ))
    } else {
      tryCatch({
        TeenGrowth::plot_weight(
          clean_data = data$selected_data,
          forecast_data = data$forecast_data,
          px = input$person_id,
          agemos_ed_onset = data$selected_data$agemos_ed_onset[1]
        )
      }, error = function(e) {
        cat("Error in plotting Weight:", e$message, "\n")
      })
    }
  }, bg = "transparent")
    ###### 4.2.2 Expected Weight Table ######
  output$forecast_output_2 <- renderTable({
    req(model_data())
    data <- model_data()

    forecast_output <- TeenGrowth::clean_forecast_data(
      data = data$forecast_data,
      model = input$model_type,
      px = input$person_id
    )
    forecast_output <- forecast_output |>
      select(c(Age, `Expected Weight (lbs)`, `Expected Weight (kgs)`)) |>
      filter(`Expected Weight (lbs)` != "NA (NA, NA)")

    head(forecast_output, 30)
  })
    ###### 4.2.3 Expected BMI Plot ######
  output$BMIPlot <- renderPlot({
    req(model_data())
    data <- model_data()

    tryCatch({
      TeenGrowth::plot_eBMI(
        data$selected_data,
        data$forecast_data,
        px = input$person_id,
        agemos_ed_onset = data$selected_data$agemos_ed_onset[1]
      )
    }, error = function(e) {
      cat("Error in plotting BMI:", e$message, "\n")
    })
  }, bg = "transparent")
    ###### 4.2.4 Expected BMI Table ######
  output$forecast_output_1 <- renderTable({
    req(model_data())
    data <- model_data()

    forecast_output <- TeenGrowth::clean_forecast_data(
      data = data$forecast_data,
      model = input$model_type,
      px = input$person_id
    )
    forecast_output <- forecast_output |>
      select(c(Age, `Expected BMI`))

    head(forecast_output, 30)
  })
  ##### 4.4 Output for Model Summary Tab #####
  output$summary <- renderUI({
    req(model_data())
    data <- model_data()
    ed_aoo_input <- data$ed_aoo_input

    model_type_names <- c(
      "most_recent" = "Most Recent (Prior to ED)",
      "mean" = "Mean (Prior to ED)",
      "max" = "Maximum (Prior to ED)",
      "mean+most_recent" = "Most Recent and Mean Equally Weighted"
    )
    model_type_friendly <- model_type_names[input$model_type]

    Prediction_interval_names <- c(
      "99" = "99%",
      "95" = "95%",
      "80" = "80%",
      "User-Defined" = "BMIz Window = 1 (+/- 0.5 BMIz from the central value of the prediction)"
    )
    Prediction_interval_friendly <- Prediction_interval_names[input$confidence_interval]

    forecast_data <- data$forecast_data
    eBMI_pct <- pnorm(mean(forecast_data$eBMIz, na.rm = TRUE))*100
    upper_eBMI_pct <- pnorm(mean(forecast_data$upper_eBMIz, na.rm = TRUE))*100
    lower_eBMI_pct <- pnorm(mean(forecast_data$lower_eBMIz, na.rm = TRUE))*100

    selected_data <- data$selected_data
    ed_aoo <- selected_data$agemos_ed_onset[1]
    adult_ht_in <- selected_data$adult_height_in[1]
    adult_ht_cm <- selected_data$adult_height_cm[1]
    age_adult_ht <- selected_data$agemos_adult_ht[1]

    most_recent <- selected_data %>% filter(agemos == max(agemos))
    current_age <- most_recent$agemos[1] / 12
    current_height_in <- most_recent$height_in[1]
    current_height_cm <- most_recent$height_cm[1]
    current_weight_lb <- most_recent$weight_lb[1]
    current_weight_kg <- most_recent$weight_kg[1]
    current_bmi <- most_recent$bmi[1]
    current_bmi_pct <- pnorm(most_recent$bmiz[1])*100


    one_year_future <- (most_recent$agemos[1] + 12)
    eWeight_one_year <- data$forecast_data %>%
      filter(agemos == one_year_future) %>%
      select(`eBMI`, `lower_eBMI`, `upper_eBMI`, `eWeight`, `lower_eWeight`, `upper_eWeight`) %>%
      mutate(eWeight_kgs = eWeight * 0.453592,
             lower_eWeight_kgs = lower_eWeight * 0.453592,
             upper_eWeight_kgs = upper_eWeight * 0.453592) %>%
      mutate_all(round, 1) %>%
      mutate(`eBMI` = paste0(eBMI, ' (', lower_eBMI, ', ', upper_eBMI, ')')) %>%
      mutate(`eWt_lbs` = paste0(eWeight, ' (', lower_eWeight, ', ', upper_eWeight, ')')) %>%
      mutate(`eWt_kgs` = paste0(eWeight_kgs, ' (', lower_eWeight_kgs, ', ', upper_eWeight_kgs, ')')) %>%
      slice(1)

    expected_bmi_one_year <- ifelse(nrow(eWeight_one_year) > 0, eWeight_one_year$eBMI[1], "Not Provided")
    expected_weight_lbs_one_year <- ifelse(nrow(eWeight_one_year) > 0, eWeight_one_year$eWt_lbs[1], "Not Provided")
    expected_weight_kgs_one_year <- ifelse(nrow(eWeight_one_year) > 0, eWeight_one_year$eWt_kgs[1], "Not Provided")

    five_year_future <- (most_recent$agemos[1] + 60)
    eWeight_five_year <- data$forecast_data %>%
      filter(agemos == five_year_future) %>%
      select(`eBMI`, `lower_eBMI`, `upper_eBMI`, `eWeight`, `lower_eWeight`, `upper_eWeight`) %>%
      mutate(eWeight_kgs = eWeight * 0.453592,
             lower_eWeight_kgs = lower_eWeight * 0.453592,
             upper_eWeight_kgs = upper_eWeight * 0.453592) %>%
      mutate_all(round, 1) %>%
      mutate(`eBMI` = paste0(eBMI, ' (', lower_eBMI, ', ', upper_eBMI, ')')) %>%
      mutate(`eWt_lbs` = paste0(eWeight, ' (', lower_eWeight, ', ', upper_eWeight, ')')) %>%
      mutate(`eWt_kgs` = paste0(eWeight_kgs, ' (', lower_eWeight_kgs, ', ', upper_eWeight_kgs, ')')) %>%
      slice(1)

    expected_bmi_five_year <- ifelse(nrow(eWeight_five_year) > 0, eWeight_five_year$eBMI[1], "Not Provided")
    expected_weight_lbs_five_year <- ifelse(nrow(eWeight_five_year) > 0, eWeight_five_year$eWt_lbs[1], "Not Provided")
    expected_weight_kgs_five_year <- ifelse(nrow(eWeight_five_year) > 0, eWeight_five_year$eWt_kgs[1], "Not Provided")

    HTML(paste0(
      "<h4>Summary</h4>",
      "<p><b>Person ID:</b> ", input$person_id, "</p>",
      "<p><b>Growth Chart Reference Sex:</b> ", ifelse(selected_data$sex[1] == 2, 'Female', "Male"), "</p>",
      "<p><b>Central BMIz Point:</b> ", model_type_friendly, "</p>",
      "<p><b>Prediction Interval:</b> ", Prediction_interval_friendly, "</p>",
      "<p><b>Eating Disorder Age of Onset:</b> ", case_when(
        !is.na(ed_aoo) ~ sprintf("%.1f years", ed_aoo / 12),
        !is.na(ed_aoo_input) ~ sprintf("%.1f years", ed_aoo_input / 12),
        TRUE ~ "Not Provided"
      ), "</p>",
      "<p><b>Adult Height:</b> ", ifelse(
        !is.na(adult_ht_in),
        paste0(round(adult_ht_in, 0), " in / ", round(adult_ht_cm, 0), " cm"),
        "Not Provided"
      ), "</p>",
      "<p><b>Age at Adult Height:</b> ", ifelse(
        !is.na(age_adult_ht),
        sprintf("%.1f years", age_adult_ht / 12),
        "Not Provided"
      ), "</p>",
      "<p><b>Most Recent Age with Available Data:</b> ", sprintf("%.1f years", current_age), "</p>",
      "<p><b>Most Recent Height:</b> ", ifelse(
        !is.na(current_height_in),
        paste0(round(current_height_in, 1), " in / ", round(current_height_cm, 0), " cm"),
        "Not Provided"
      ), "</p>",
      "<p><b>Most Recent Weight:</b> ", ifelse(
        !is.na(current_weight_lb),
        paste0(round(current_weight_lb, 1), " lbs / ", round(current_weight_kg, 1), " kg"),
        "Not Provided"
      ), "</p>",
      "<p><b>Most Recent BMI:</b> ", ifelse(
        !is.na(current_bmi),
        round(current_bmi, 1),
        "Not Provided"
      ), "</p>",
      "<p><b>Most Recent BMI Percentile:</b> ", ifelse(
        !is.na(current_bmi_pct),
        round(current_bmi_pct, 0),
        "Not Provided"
      ), "</p>",
      "<p><b>Expected BMI Percentile:</b> ", paste0(round(eBMI_pct, 0), " (", round(lower_eBMI_pct, 0), ", ", round(upper_eBMI_pct, 0), ")"), "</p>",
      "<p><b>Expected BMI (+1 Year):</b> ", expected_bmi_one_year, "</p>",
      "<p><b>Expected Weight (+1 Year):</b> ", ifelse(
        expected_weight_lbs_one_year != "NA (NA, NA)",
        paste0(expected_weight_lbs_one_year, " lbs / ", expected_weight_kgs_one_year, " kg"),
        "Not Provided"
      ), "</p>",
      "<p><b>Expected BMI (+5 Years):</b> ", expected_bmi_five_year, "</p>",
      "<p><b>Expected Weight (+5 Years):</b> ", ifelse(
        expected_weight_lbs_five_year != "NA (NA, NA)",
        paste0(expected_weight_lbs_five_year, " lbs / ", expected_weight_kgs_five_year, " kg"),
        "Not Provided"
      ), "</p>"
    ))
  })

  # Function to check if model is selected and summary is rendered
  check_model_and_summary <- reactive({
    cleaned_data_status() && !is.null(model_data()) && input$model_type != "" && input$confidence_interval != ""
  })
#### 5. Weight Restoration Planning #####
  ##### 5.1 Weight restoration Inputs #####
  output$conditional_weight_restoration_inputs <- renderUI({
    req(input$person_id)
    cleaned_df <- cleaned_data()
    selected_data <- cleaned_df[cleaned_df[["id"]] == input$person_id, ]

    inputs <- list()

    if (TRUE) {  # Always needed
      inputs <- append(inputs, list(dateInput("tx_start_date", "Treatment Start Date:", value = tx_start_date_for_demo_participants())))
    }

    if (is.null(input$adult_height_in) & !("adult_height" %in%(input$demographic_columns))) {  # Need only if no adult height
      inputs <- append(inputs, list(numericInput("current_height", "Current Height (in):", value = NULL, min = 1)))
    }
    if (is.null(input$adult_height_in) & !("adult_height" %in%(input$demographic_columns))) {  # Always needed
      inputs <- append(inputs, list(numericInput("age_current_height", "Age at Current Height (years):", value = NULL, min = 1)))
    }
    # Use the reactive dob_for_demo_participants to set the value for demo participants
    dob_value <- dob_for_demo_participants()

    if (is.null(selected_data$dob) || all(is.na(selected_data$dob))) {
      if (is.null(dob_value)) {
        inputs <- append(inputs, list(dateInput("dob", "Date of Birth:", value = Sys.Date() - lubridate::years(16))))
      } else {
        inputs <- append(inputs, list(dateInput("dob", "Date of Birth:", value = dob_value)))
      }
    }


    # Add the slider input for Rate of Weight Restoration
    inputs <- append(inputs, list(
      sliderInput("weight_restoration_rate", "Rate of Weight Restoration (lbs per week):", min = 0.5, max = 5, value = 1, step = 0.5)
    ))


    do.call(tagList, inputs)
  })

  ##### 5.2 Weight Restoration Plot #####
  observeEvent(input$plot_weight_restoration, {
    if (is.null(input$tx_start_date) || !check_model_and_summary()) {
      showModal(modalDialog(
        title = "Missing Information",
        if (is.null(input$tx_start_date)) {
          "Please input the Treatment Start Date."
        } else {
          "Please go back to the Model Selection tab and ensure a model is selected and summary is rendered."
        },
        footer = modalButton("OK")
      ))
    } else {
      output$wt_restore_graph <- renderPlot({
        req(model_data())
        req(data())
        data <- model_data()
        # Retrieve the data
        raw_data <- data()

        # Conditionally filter the data only if id_column is not NULL
        if (!is.null(input$id_column)) {
          raw_data <- raw_data %>%
            filter(!!sym(input$id_column) == input$person_id)
        }
        cleaned_data <- data$selected_data

        dob <- if ("dob" %in% input$age_columns) {
          dob_colname <- input$dob_column
          if (!is.null(input$id_column)) {
            id_colname <- input$id_column
            dob_value <- raw_data %>%
              filter(!!sym(id_colname) == input$person_id) %>%
              pull(!!sym(dob_colname))
            as.Date(dob_value[1])
          } else {
            dob_value <- raw_data %>%
              pull(!!sym(dob_colname))
            as.Date(dob_value[1])
          }
        } else if (!is.null(input$dob)) {
          as.Date(input$dob, origin = "1970-01-01")
        } else {
          NA
        }

        sex <- if ("sex" %in% input$demographics_columns) {
          cleaned_data$sex[1]
        } else {
          input$sex
        }
        cat("Selected participant's sex:", sex, "\n")

        current_ht <- if (!is.null(input$height_value_column) && input$height_value_column %in% colnames(cleaned_data)) {
          cleaned_data[[input$height_value_column]]
        } else {
          input$current_height
        }
        age_current_ht <- if ("age_adult_height" %in% input$demographics_columns) {
          input$age_adult_height
        } else {
          input$age_current_height
        }
        adult_ht <- if ("adult_height" %in% input$demographics_columns) {
          adult_ht_col <- input$adult_height_column
          id_colname <- input$id_column
          adult_ht_value <- raw_data %>%
            filter(!!sym(id_colname) == input$person_id) %>%
            pull(!!sym(adult_ht_col))
          adult_ht_value[1]
        } else if (!is.null(input$adult_height_in)) {
          input$adult_height_in
        } else {
          NA
        }
        age_adult_ht <- if ("age_adult_height" %in% input$demographics_columns || "adult_height " %in% input$demographics_columns) {
          agemos_adult_ht <- cleaned_data %>%
            pull(agemos_adult_ht)
          agemos_adult_ht <- agemos_adult_ht[1]
          if (input$age_unit == 'years') {
            agemos_adult_ht / 12
          } else if (input$age_unit == 'months') {
            agemos_adult_ht
          } else if (input$age_unit == 'days') {
            agemos_adult_ht / 365.25
          } else {
            NA
          }
        } else if (!is.null(input$age_adult_height)) {
          input$age_adult_height
        } else {
          NA
        }
        ed_aoo <- if ("ed_age_onset" %in% input$demographics_columns) {
          ed_aoo_vec <- cleaned_data %>%
            pull(agemos_ed_onset)
          agemos_ed_aoo <- ed_aoo_vec[1]
          if (input$age_unit == 'years') {
            agemos_ed_aoo / 12
          } else if (input$age_unit == 'months') {
            agemos_ed_aoo
          } else if (input$age_unit == 'days') {
            agemos_ed_aoo / 365.25
          } else {
            NA
          }
        } else if (!is.null(input$ed_aoo)) {
          input$ed_aoo
        } else {
          NA
        }
        tx_start_date <- if (!is.null(input$tx_start_date) && !is.na(input$tx_start_date)) {
          as.Date(input$tx_start_date, origin = "1970-01-01")
        } else {
          NA
        }
        intake_wt <- if (!is.null(input$intake_wt) && !is.na(input$intake_wt)) {
          input$intake_wt
        } else {
          NULL
        }

        # Debugging statements
        cat("dob:", dob, "\n")
        cat("sex:", sex, "\n")
        cat("current_ht:", current_ht, "\n")
        cat("age_current_ht:", age_current_ht, "\n")
        cat("adult_ht:", adult_ht, "\n")
        cat("age_adult_ht:", age_adult_ht, "\n")
        cat("ed_aoo:", ed_aoo, "\n")
        cat("tx_start_date:", tx_start_date, "\n")
        cat("intake_wt:", intake_wt, "\n")
        cat("ht_unit:", input$height_unit, "\n")
        cat("age_col_name", input$age_column, "\n")
        cat("wt_col_name: ", input$weight_column, "\n")

        filtered_data <- tx_plot_clean(
          raw_data,
          age_col_name = if(!is.null(input$age_column)) input$age_column else NULL,
          date_assessed_col_name = input$assessment_date_column,
          ht_col_name = input$height_value_column,
          wt_col_name = if (!is.null(input$weight_column)) input$weight_column else NULL,
          age_unit = if(!is.null(input$age_unit)) input$age_unit else 'years',
          ht_unit = if(!is.null(input$height_unit)) input$height_unit else 'in',
          wt_unit = if(!is.null(input$weight_unit)) input$weight_unit else 'lb',
          bmi_col_name = if (!is.null(input$bmi_column)) input$bmi_column else NULL,
          bmiz_col_name = if (!is.null(input$bmi_z_column)) input$bmi_z_column else NULL,
          pct_col_name = if (!is.null(input$bmi_percentile_column)) input$bmi_percentile_column else NULL,
          data_source = 'cdc',
          dob = dob,
          sex = sex,
          current_ht = if (!is.null(current_ht)) current_ht else NULL,
          age_current_ht = if (!is.null(age_current_ht)) age_current_ht else NULL,
          adult_ht = adult_ht,
          age_adult_ht = age_adult_ht,
          ed_aoo = ed_aoo,
          tx_start_date = tx_start_date,
          intake_wt = intake_wt
        )

        # Debugging statement for filtered_data
        cat("filtered_data:\n")
        print(head(filtered_data))

        forecast_data <- data$forecast_data %>%
          filter(id == input$person_id)

        tryCatch({
          TeenGrowth::Wt_Restore_Plot(
            filtered_data,
            forecast_data = forecast_data,
            slope_per_week = input$weight_restoration_rate)
        }, error = function(e) {
          showModal(modalDialog(
            title = "Error",
            "Error making weight restoration graph -- return to Model Selection and ensure a model has been run for a single participant prior to making a weight restoration graph. Also ensure all necessary inputs are filled out in the Model Selection tab. Also consider dates or that the participant may not have enough data to make a weight restoration graph.",
            footer = modalButton("OK")
          ))
          cat("Error in plotting Weight restoration:", e$message, "\n")
        })
      }, bg = "transparent")
    }
  })
  ##### 5.3 BMI Percentile Restoration Plot #####
  observeEvent(input$plot_pct_restore, {
    if (is.null(input$tx_start_date) || !check_model_and_summary()) {
      showModal(modalDialog(
        title = "Missing Information",
        if (is.null(input$tx_start_date)) {
          "Please input the Treatment Start Date."
        } else {
          "Please go back to the Model Selection tab and ensure a model is selected and summary is rendered."
        },
        footer = modalButton("OK")
      ))
    } else {
      output$pct_restore_graph <- renderPlot({
        req(model_data())
        req(data())
        data <- model_data()
        raw_data <- data()

        if (!is.null(input$id_column)) {
          raw_data <- raw_data %>%
            filter(!!sym(input$id_column) == input$person_id)
        }

        cleaned_data <- data$selected_data

        # Define 'dob' and other variables before using them
        dob <- if ("dob" %in% input$age_columns) {
          dob_colname <- input$dob_column
          if (!is.null(input$id_column)) {
            id_colname <- input$id_column
            dob_value <- raw_data %>%
              filter(!!sym(id_colname) == input$person_id) %>%
              pull(!!sym(dob_colname))
            as.Date(dob_value[1])
          } else {
            dob_value <- raw_data %>%
              pull(!!sym(dob_colname))
            as.Date(dob_value[1])
          }
        } else if (!is.null(input$dob)) {
          as.Date(input$dob, origin = "1970-01-01")
        } else {
          NA
        }

        sex <- if ("sex" %in% input$demographics_columns) {
          cleaned_data$sex[1]
        } else {
          input$sex
        }

        current_ht <- if (!is.null(input$height_value_column) && input$height_value_column %in% colnames(cleaned_data)) {
          cleaned_data[[input$height_value_column]]
        } else {
          input$current_height
        }

        age_current_ht <- if ("age_adult_height" %in% input$demographics_columns) {
          input$age_adult_height
        } else {
          input$age_current_height
        }

        adult_ht <- if ("adult_height" %in% input$demographics_columns) {
          adult_ht_col <- input$adult_height_column
          id_colname <- input$id_column
          adult_ht_value <- raw_data %>%
            filter(!!sym(id_colname) == input$person_id) %>%
            pull(!!sym(adult_ht_col))
          adult_ht_value[1]
        } else if (!is.null(input$adult_height_in)) {
          input$adult_height_in
        } else {
          NA
        }

        age_adult_ht <- if ("age_adult_height" %in% input$demographics_columns || "adult_height " %in% input$demographics_columns) {
          agemos_adult_ht <- cleaned_data %>%
            pull(agemos_adult_ht)
          agemos_adult_ht <- agemos_adult_ht[1]
          if (input$age_unit == 'years') {
            agemos_adult_ht / 12
          } else if (input$age_unit == 'months') {
            agemos_adult_ht
          } else if (input$age_unit == 'days') {
            agemos_adult_ht / 365.25
          } else {
            NA
          }
        } else if (!is.null(input$age_adult_height)) {
          input$age_adult_height
        } else {
          NA
        }

        ed_aoo <- if ("ed_age_onset" %in% input$demographics_columns) {
          ed_aoo_vec <- cleaned_data %>%
            pull(agemos_ed_onset)
          agemos_ed_aoo <- ed_aoo_vec[1]
          if (input$age_unit == 'years') {
            agemos_ed_aoo / 12
          } else if (input$age_unit == 'months') {
            agemos_ed_aoo
          } else if (input$age_unit == 'days') {
            agemos_ed_aoo / 365.25
          } else {
            NA
          }
        } else if (!is.null(input$ed_aoo)) {
          input$ed_aoo
        } else {
          NA
        }

        tx_start_date <- if (!is.null(input$tx_start_date) && !is.na(input$tx_start_date)) {
          as.Date(input$tx_start_date, origin = "1970-01-01")
        } else {
          NA
        }

        intake_wt <- if (!is.null(input$intake_wt) && !is.na(input$intake_wt)) {
          input$intake_wt
        } else {
          NULL
        }

        # Now that variables are defined, proceed to use them
        filtered_data <- pct_plot_clean(
          raw_data,
          age_col_name = input$age_column,
          date_assessed_col_name = input$assessment_date_column,
          ht_col_name = input$height_value_column,
          wt_col_name = input$weight_column,
          age_unit = input$age_unit,
          ht_unit = input$height_unit,
          wt_unit = input$weight_unit,
          bmi_col_name = input$bmi_column,
          bmiz_col_name = input$bmi_z_column,
          pct_col_name = input$bmi_percentile_column,
          data_source = 'cdc',
          dob = dob,
          sex = "M",
          current_ht = current_ht,
          age_current_ht = age_current_ht,
          adult_ht = adult_ht,
          age_adult_ht = age_adult_ht,
          ed_aoo = ed_aoo,
          tx_start_date = tx_start_date,
          intake_wt = intake_wt
        )

        # Proceed with plotting
        forecast_data <- data$forecast_data %>%
          filter(id == input$person_id)

        # print head of forecast data
        tryCatch({
          TeenGrowth::Pct_Restore_Plot(
            clean_data = filtered_data,
            forecast_data = forecast_data
          )
        }, error = function(e) {
          showModal(modalDialog(
            title = "Error",
            paste("Error generating the BMI percentile restoration plot:", e$message),
            footer = modalButton("OK")
          ))
          cat("Error in plotting BMI percentile restoration:", e$message, "\n")
        })
      }, bg = "transparent")
    }
  })
#### 6. Navigation ####
  observeEvent(input$next_button_data_spec, {
    updateTabsetPanel(session, "main_tabs", selected = "Model Selection")
  })
  observeEvent(input$back_button_data_spec, {
    updateTabsetPanel(session, "main_tabs", selected = "Data Input")
  })
  observeEvent(input$next_button_model_selection, {
    updateTabsetPanel(session, "main_tabs", selected = "Weight Restoration")
  })
  observeEvent(input$back_button_weight_restoration, {
    updateTabsetPanel(session, "main_tabs", selected = "Model Selection")
  })
  observeEvent(input$next_button_data_input, {
    updateTabsetPanel(session, "main_tabs", selected = "Data Specification")
  })
  observeEvent(input$next_button_data_spec, {
    updateTabsetPanel(session, "main_tabs", selected = "Model Selection")
  })
  observeEvent(input$back_button_data_spec, {
    updateTabsetPanel(session, "main_tabs", selected = "Data Input")
  })
  observeEvent(input$next_button_model_selection, {
    updateTabsetPanel(session, "main_tabs", selected = "Weight Restoration Planning")
  })
  observeEvent(input$back_button_weight_restoration, {
    updateTabsetPanel(session, "main_tabs", selected = "Model Selection")
  })
  observeEvent(c(input$model_type, input$confidence_interval, input$person_id), {
    output$wt_restore_graph <- renderPlot(NULL)
  })

}
