library(shiny)
library(shinyjs)
library(readxl)
library(TeenGrowth)
library(dplyr)
library(embarktools)

server <- function(input, output, session) {
  demo_data <- TeenGrowth::demo

  # Helper function to read uploaded data
  read_uploaded_data <- function(file) {
    ext <- tools::file_ext(file$name)
    switch(ext,
           "csv" = read.csv(file$datapath),
           "xlsx" = readxl::read_excel(file$datapath),
           "xls" = readxl::read_excel(file$datapath),
           stop("Unsupported file type"))
  }

  data <- reactive({
    if (input$data_source == "upload") {
      req(input$file1)
      read_uploaded_data(input$file1)
    } else {
      demo_data
    }
  })

  observeEvent(input$data_source, {
    if (input$data_source == "demo") {
      updateCheckboxGroupInput(session, "demographics_columns", selected = c("id", "sex", "adult_height", "ed_age_onset"))
      updateCheckboxGroupInput(session, "age_columns", selected = c("age"))
      updateCheckboxGroupInput(session, "anthropometric_columns", selected = c("Ht + Wt"))
    } else {
      updateCheckboxGroupInput(session, "demographics_columns", selected = NULL)
      updateCheckboxGroupInput(session, "age_columns", selected = NULL)
      updateCheckboxGroupInput(session, "anthropometric_columns", selected = NULL)
    }

    # Reset cleaned data
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

  output$conditional_inputs <- renderUI({
    req(input$data_source == 'upload' || input$data_source == 'demo')

    inputs <- list(
      selectInput("height_unit", "Height Unit", choices = c("in", "cm"), selected = if (input$data_source == "demo") "in" else NULL),
      selectInput("weight_unit", "Weight Unit", choices = c("lb", "kg"), selected = if (input$data_source == "demo") "lb" else NULL),
      selectInput("age_unit", "Age Unit", choices = c("years", "months", "days"), selected = if (input$data_source == "demo") "years" else NULL)
    )

    columns_list <- list(
      "id" = list(textInput("id_column", "Name of the id column:", value = if (input$data_source == "demo") "participant" else NULL)),
      "age" = list(textInput("age_column", "Name of the age column:", value = if (input$data_source == "demo") "age" else NULL)),
      "sex" = list(textInput("sex_column", "Name of the sex column (for growth chart reference):", value = if (input$data_source == "demo") "sex" else NULL)),
      "adult_height" = list(textInput("adult_height_column", "Name of the adult height column:", value = if (input$data_source == "demo") "adult_height_in" else NULL)),
      "dob" = list(textInput("dob_column", "Name of the date of birth column:")),
      "assessment_date" = list(textInput("assessment_date_column", "Name of the date of assessment column:")),
      "bmi" = list(textInput("bmi_column", "Name of the BMI column:")),
      "bmi_z" = list(textInput("bmi_z_column", "Name of the BMI z-score column:", value = if (input$data_source == "demo") "bmiz" else NULL)),
      "bmi_percentile" = list(textInput("bmi_percentile_column", "Name of the BMI percentile column:")),
      "Ht + Wt" = list(
        textInput("height_value_column", "Name of the height column:", value = if (input$data_source == "demo") "height" else NULL),
        textInput("weight_column", "Name of the weight column:", value = if (input$data_source == "demo") "weight" else NULL)
      ),
      "ed_age_onset" = list(textInput("aao_column", "Name of the eating disorder age of onset column:", value = if (input$data_source == "demo") "ed_aoo" else NULL))
    )

    for (col in c(input$demographics_columns, input$age_columns, input$anthropometric_columns)) {
      if (col %in% names(columns_list)) {
        inputs <- append(inputs, columns_list[[col]])
      }
    }

    if (input$data_source != "demo") {
      if (input$data_type == "one") {
        if (!("dob" %in% input$age_columns) && !("age" %in% input$age_columns)) {
          inputs <- append(inputs, list(dateInput("dob", "What is the individual's date of birth?")))
        }
        if (!("sex" %in% input$demographics_columns)) {
          inputs <- append(inputs, list(selectInput("sex", "What is the individual's sex at birth?", choices = c("Male", "Female"))))
        }
        if (!("adult_height" %in% input$demographics_columns)) {
          inputs <- append(inputs, list(numericInput("aheight", "What is the individual's adult height?", value = NULL, min = 55, max = 200)))
        }
        if (!("age_adult_height" %in% input$demographics_columns)) {
          inputs <- append(inputs, list(numericInput("aheight_age", "At what age did the individual reach their adult height?", value = NULL, min = 1, max = 25)))
        }
        if (!("ed_age_onset" %in% input$demographics_columns)) {
          inputs <- append(inputs, list(numericInput("symptoms", "At what age did the individual first experience significant eating disorder symptoms? (Optional)", value = NULL, min = 1)))
        }
      }
    }

    do.call(tagList, inputs)
  })

  cleaned_data_status <- reactiveVal(FALSE)
  cleaned_data <- reactiveVal()
  clean_data_messages <- reactiveVal(NULL)

  observeEvent(input$submit_button, {
    df <- data()

    required_columns <- c(
      if ("id" %in% input$demographics_columns) input$id_column else NULL,
      if ("age" %in% input$age_columns) input$age_column else NULL,
      if ("sex" %in% input$demographics_columns) input$sex_column else NULL,
      if ("adult_height" %in% input$demographics_columns) input$adult_height_column else NULL,
      if ("dob" %in% input$age_columns) input$dob_column else NULL,
      if ("assessment_date" %in% input$age_columns) input$assessment_date_column else NULL,
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

    # Capture messages from the clean_data function
    messages <- capture.output({
      cleaned <- TeenGrowth::clean_data(
        df,
        id_col_name = if ("id" %in% input$demographics_columns) input$id_column else NULL,
        age_col_name = if ("age" %in% input$age_columns) input$age_column else NULL,
        dob_col_name = if ("dob" %in% input$age_columns) input$dob_column else NULL,
        date_assessed_col_name = if ("assessment_date" %in% input$age_columns) input$assessment_date_column else NULL,
        age_unit = input$age_unit,
        sex_col_name = if ("sex" %in% input$demographics_columns) input$sex_column else NULL,
        ht_col_name = if ("Ht + Wt" %in% input$anthropometric_columns) input$height_value_column else NULL,
        ht_unit = input$height_unit,
        wt_col_name = if ("Ht + Wt" %in% input$anthropometric_columns) input$weight_column else NULL,
        wt_unit = input$weight_unit,
        bmi_col_name = if ("bmi" %in% input$anthropometric_columns) input$bmi_column else NULL,
        bmiz_col_name = if ("bmi_z" %in% input$anthropometric_columns) input$bmi_z_column else NULL,
        pct_col_name = if ("bmi_percentile" %in% input$anthropometric_columns) input$bmi_percentile_column else NULL,
        data_source = 'cdc',
        adult_ht_col_name = if ("adult_height" %in% input$demographics_columns) input$adult_height_column else NULL,
        ed_aoo_col_name = if ("ed_age_onset" %in% input$demographics_columns) input$aao_column else NULL
      )
      cleaned_data(cleaned)
    }, type = "message")

    clean_data_messages(paste(messages, collapse = "<br>"))
    cleaned_data_status(TRUE)
    updateSelectInput(session, "person_id", choices = unique(cleaned_data()[["id"]]))
  })

  observe({
    req(input$person_id)
    cleaned_df <- cleaned_data()
    selected_data <- cleaned_df[cleaned_df[["id"]] == input$person_id, ]

    adult_height_needed <- all(is.na(selected_data$adult_height_in))
    age_adult_height_needed <- all(is.na(selected_data$agemos_adult_ht))
    ed_aoo_needed <- all(is.na(selected_data$agemos_ed_onset))

    output$adult_height_needed <- reactive({ adult_height_needed })
    output$age_adult_height_needed <- reactive({ age_adult_height_needed })
    output$ed_aoo_needed <- reactive({ ed_aoo_needed })

    outputOptions(output, "adult_height_needed", suspendWhenHidden = FALSE)
    outputOptions(output, "age_adult_height_needed", suspendWhenHidden = FALSE)
    outputOptions(output, "ed_aoo_needed", suspendWhenHidden = FALSE)
  })

  output$conditional_model_inputs <- renderUI({
    req(input$person_id)
    cleaned_df <- cleaned_data()
    selected_data <- cleaned_df[cleaned_df[["id"]] == input$person_id, ]

    inputs <- list()

    if (all(is.na(selected_data$agemos_ed_onset))) {
      inputs <- append(inputs, list(numericInput("ed_aoo", "Eating Disorder Age of Onset (Years):", value = 12, min = 1)))
    }

    do.call(tagList, inputs)
  })

  model_data <- eventReactive(input$run_model, {
    req(cleaned_data(), input$person_id, input$model_type, input$confidence_interval)

    ed_aoo_input <- as.numeric(age_in_months(input$ed_aoo, age_unit = input$age_unit))
    cleaned_df <- cleaned_data()
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

    forecast_input <- if (!all(is.na(selected_data$agemos_ed_onset))) {
      selected_data %>% filter(agemos < agemos_ed_onset[1])
    } else {
      selected_data
    }

    forecast_data <- if (input$confidence_interval == 'User-Defined') {
      TeenGrowth::make_full_bmi_df(
        data = forecast_input,
        central_value = input$model_type,
        ci = input$confidence_interval,
        lower_margin = 0.5,
        upper_margin = 0.5
      )
    } else {
      TeenGrowth::make_full_bmi_df(
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

  observeEvent(input$next_button_data_spec, {
    updateTabsetPanel(session, "main_tabs", selected = "Model Selection")
  })

  observeEvent(input$back_button_data_spec, {
    updateTabsetPanel(session, "main_tabs", selected = "Data Input")
  })

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
          agemos_onset_ed = data$selected_data$agemos_ed_onset[1]
        )
      }, error = function(e) {
        cat("Error in plotting Weight:", e$message, "\n")
      })
    }
  }, bg = "transparent")

  output$BMIPlot <- renderPlot({
    req(model_data())
    data <- model_data()

    tryCatch({
      TeenGrowth::plot_eBMI(
        data$selected_data,
        data$forecast_data,
        px = input$person_id,
        agemos_onset_ed = data$selected_data$agemos_ed_onset[1]
      )
    }, error = function(e) {
      cat("Error in plotting BMI:", e$message, "\n")
    })
  }, bg = "transparent")

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
      "90" = "90%",
      "95" = "95%",
      "User-Defined" = "BMIz Window = 1 (+/- 0.5 BMIz from the central value of the prediction)"
    )
    Prediction_interval_friendly <- Prediction_interval_names[input$confidence_interval]

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
        paste0(round(current_height_in, 1), " in / ", round(current_height_cm, 1), " cm"),
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

  output$wt_restore_graph <-  renderPlot({
    req(model_data())
    data <- model_data()

  tx_plot_clean <- tx_plot_clean(
      filtered_data,
      age_col_name = NULL,
      date_assessed_col_name = NULL,
      ht_col_name = NULL,
      wt_col_name = NULL,
      age_unit = NULL,
      ht_unit = 'cm',
      wt_unit = 'kg',
      bmi_col_name = NULL,
      bmiz_col_name = NULL,
      pct_col_name = NULL,
      data_source = 'cdc',
      dob = NULL,
      sex = NULL,
      current_ht = NULL,
      age_current_ht = NULL,
      adult_ht = NULL,
      age_adult_ht = NULL,
      ed_aoo = NULL,
      tx_start_date = NULL,
      intake_wt = NULL
    )

    tryCatch({
      TeenGrowth::Wt_Restore_Plot(
        data$selected_data,
        data$forecast_data,
        px = input$person_id,
        agemos_onset_ed = data$selected_data$agemos_ed_onset[1]
      )
    }, error = function(e) {
      cat("Error in plotting BMI:", e$message, "\n")
    })
  }, bg = "transparent")



  output$data_cleaned <- reactive({
    cleaned_data_status()
  })

  outputOptions(output, "data_cleaned", suspendWhenHidden = FALSE)
}
