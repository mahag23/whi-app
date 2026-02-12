library(dplyr)
library(lubridate)
library(TeenGrowth)
library(readxl)
library(timevis)
library(ggplot2)
library(embarktools)
library(readxl)
library(scales)      # For pretty_breaks on axes
library(plotly)      # For interactive plots


# Load LMS once at the top of your server.R
lms_df <- read.csv("bmi-age-2022.csv")
lms_df <- lms_df %>%
 # Convert growth chart age to months
  arrange(agemos) %>%
  distinct(agemos, .keep_all = TRUE)

# Interpolation functions
get_L <- function(agemos) approx(lms_df$agemos, lms_df$L, xout = agemos, rule = 2)$y
get_M <- function(agemos) approx(lms_df$agemos, lms_df$M, xout = agemos, rule = 2)$y
get_S <- function(agemos) approx(lms_df$agemos, lms_df$S, xout = agemos, rule = 2)$y

server <- function(input, output, session) {

demo_data <- TeenGrowth::demo
observeEvent(input$to_fasting, {
  updateTabsetPanel(session, "behav_tabs", selected = "Fasting")
})

observeEvent(input$back_to_restrict, {
  updateTabsetPanel(session, "behav_tabs", selected = "Restriction")
})

observeEvent(input$to_uwl, {
  updateTabsetPanel(session, "behav_tabs", selected = "Unintended Weightloss")
})

observeEvent(input$back_to_uwl, {
  updateTabsetPanel(session, "behav_tabs", selected = "Unintended Weightloss")
})

observeEvent(input$to_vomit, {
  updateTabsetPanel(session, "behav_tabs", selected = "Vomiting")
})

observeEvent(input$back_to_vomit, {
  updateTabsetPanel(session, "behav_tabs", selected = "Vomiting")
})

observeEvent(input$to_lax, {
  updateTabsetPanel(session, "behav_tabs", selected = "Laxative")
})

observeEvent(input$back_to_lax, {
  updateTabsetPanel(session, "behav_tabs", selected = "Laxative")
})

observeEvent(input$to_diur, {
  updateTabsetPanel(session, "behav_tabs", selected = "Diuretic")
})

observeEvent(input$back_to_diur, {
  updateTabsetPanel(session, "behav_tabs", selected = "Diuretic")
})

observeEvent(input$to_binge, {
  updateTabsetPanel(session, "behav_tabs", selected = "Binge")
})

observeEvent(input$back_to_binge, {
  updateTabsetPanel(session, "behav_tabs", selected = "Binge")
})

observeEvent(input$to_exercise, {
  updateTabsetPanel(session, "behav_tabs", selected = "Exercise")
})

observeEvent(input$back_to_exercise, {
  updateTabsetPanel(session, "behav_tabs", selected = "Exercise")
})


observeEvent(input$to_edtabs, {
  updateTabsetPanel(session, "ed_behaviors", selected = "Pt.2 ED Behaviors")
})
####medical history#####

observeEvent(input$open_ques, {
  runjs("
      var refWindow = window.open('', 'InterviewWindow', 'width=600,height=600,resizable=yes,scrollbars=yes');
      refWindow.document.write('<h3>PT 2. Interview Questions</h3>');
      refWindow.document.write ('<p> Has there ever been a time when you a) tried to limit the amount of food you ate to influence your weight, b) tried to exclude foods from your diet to influence your weight, and/or c) tried to follow rules (such as following a calorie limit) to influence your weight?</p>');


      refWindow.document.close();
    ")
})
  # JS to open a new window and write some content
  observeEvent(input$open_ref, {
    runjs("
      var refWindow = window.open('', 'ReferenceWindow', 'width=600,height=600,resizable=yes,scrollbars=yes');
      refWindow.document.write('<h3>Reference Sheet</h3>');
      refWindow.document.write('<p><strong>Key:</strong> 0 - No Restriction, 1 - Subclinical, 2 - Clinical</p>');
      refWindow.document.write('<p><strong>For interviewers:</strong> For each behavior, get participants age to the nearest six months (14.5 vs 14). Help them recall by asking participants what grade/year in school they were. It may be helpful for them to recall what time of year (season, month) they were in while engaging in these behaviors. Create a new graph for each behavior to show participants each behavior to help them with recall of information.</p>');
      refWindow.document.write('<p><strong>FOR RESTRICTION:</strong> Mark on the graph based on their level of restriction. Do not tell participants what the numbers mean.</p>');
      refWindow.document.write('<p>1. Did you deliberately try to limit the overall amount of food you ate?<br>[No = the participant denies attempting to decrease the amount of food they were eating]<br>[Subclinical = the participant attempted to reduce the amount of food/calories they were eating for a reason other than weight loss, was following a medically-prescribed eating plan in which caloric or food restriction was not drastic, or reduced the amount of food or calories they were eating, but the reduction was not drastic (< 300 calorie reduction from baseline)]<br>[Clinical = the participant attempted to reduce the amount of food/calories they were eating drastically (> 300 calorie reduction from baseline)]</p>');
      refWindow.document.write('<p>2. Try to exclude food that you liked from your diet?<br>[No = the participant denies excluding any food they liked from their diet]<br>[Subclinical = the participant stopped eating 1-2 foods or excluded foods for reasons other than weight loss]<br>[Clinical = the participant stopped eating more than 2 foods or excluded entire food groups with the goal of losing weight]</p>');
      refWindow.document.write('<p>3. Try to follow rules related to food, such as calorie counting or cutting out food groups?<br>[No = the participant denies following rules related to food]<br>[Subclinical = the participant read food labels, considered the calories in food before eating it, or followed rules for medical reasons rather than weight loss]<br>[Clinical = the participant followed a calorie limit, tracked food in an app, or followed rules that significantly limit the amount or types of food they could consume for the purpose of losing weight]</p>');
      refWindow.document.write('<p>4. Skip meals?<br>[No = the participant denies skipping meals (i.e., consistently ate 3 meals per day whenever possible)]<br>[Subclinical = the participant skipped meals for reasons other than weight loss (not hungry in the morning, lack of time), skipped meals less than twice per week, or skipped meals to save calories for other meals]<br>[Clinical = the participant skipped meals more than 3 times per week for the purposes of weight loss]</p>');
      refWindow.document.close();
    ")
  })


rv <- reactiveValues(
  step = 1,
  responses = list(),

  # counts (never NULL)
  n_surgeries = 0,
  n_hosp = 0,
  ed_hospital_times = 0,
  n_injury = 0,
  medical_conditions = 0,
  mental_disorder_dx = 0,
  n_meds = 0,

  # indices (never NULL)
  surgery_index = 1,
  hosp_index = 1,
  ed_hosp_index = 1,
  injury_index = 1,
  med_index = 1,
  mh_index = 1,
  meds_index = 1
)


rv3 <- reactiveValues(
  step = 1,
  uwl_index = 1,
  n_episodes = 1
)

output$uwl_questions <- renderUI({
  switch(as.character(rv3$step),

         # STEP 1: Yes/No
         "1" = tagList(
           h4("Unintentional Weight Loss"),
           radioButtons("uwl_yn",
                        "Has there ever been a time when weight was lost unintentionally?",
                        choices = c("No" = 0, "Yes" = 1),
                        inline = TRUE),
           conditionalPanel(
             condition = "input.uwl_yn == 1",
             numericInput(
               "uwl_index",
               "Number of periods of UWL",
               value = 1,
               min = 1
             )
           )
         ),

         # STEP 2: Episode details
         "2" = tagList(
           h4(paste("Episode", rv3$uwl_index)),
           textInput("uwl_reason", "Reason for weight loss"),
           dateInput("uwl_start", "Start date"),
           dateInput("uwl_end", "End date"),
           numericInput("uwl_lost", "Amount of weight lost", value = NA),
           numericInput("uwl_duration", "How long at this lower weight (months)", value = NA),
           numericInput("uwl_regain", "Amount of weight regained", value = NA),
           textAreaInput("uwl_notes", "Notes")
         )
  )
})

observeEvent(input$uwl_next, {

  # STEP 1 — Yes/No
  if (rv3$step == 1) {
    req(input$uwl_yn)

    if (input$uwl_yn == 1) {
      rv3$n_episodes <- input$uwl_index
      rv3$uwl_index <- 1
      rv3$step <- 2
    } else {
      # Go to next tab if "No"
      updateTabsetPanel(session, "behav_tabs", selected = "Vomiting")
    }
  }

  # STEP 2 — Episode details
  else if (rv3$step == 2) {
    req(input$uwl_reason, input$uwl_start)

    # Here you could save input for this episode
    # e.g., store in a reactiveValues list or dataframe

    # Move to next episode or finish
    if (rv3$uwl_index < rv3$n_episodes) {
      rv3$uwl_index <- rv3$uwl_index + 1
      # Clear inputs for next episode
      updateTextInput(session, "uwl_reason", value = "")
      updateDateInput(session, "uwl_start", value = NA)
      updateDateInput(session, "uwl_end", value = NA)
      updateNumericInput(session, "uwl_lost", value = NA)
      updateNumericInput(session, "uwl_duration", value = NA)
      updateNumericInput(session, "uwl_regain", value = NA)
      updateTextAreaInput(session, "uwl_notes", value = "")
    } else {
      # Finished all episodes
      updateTabsetPanel(session, "behav_tabs", selected = "Vomiting")
    }
  }

})





output$main_ui <- renderUI({

  switch(
    as.character(rv$step),

    # -------------------------------
    # 1. Intro######
    # -------------------------------
    "1" = tagList(
      h4("PART 1: Medical History Overview"),
      p("I will begin the interview by asking you about your medical and mental health history at various points in your life.")
    ),

    # -------------------------------
    # 2–3. Surgeries#####
    # -------------------------------
    "2" = tagList(
      h4("Surgeries"),
      numericInput("n_surgeries", "How many surgeries have you had?", 0, min = 0)
    ),

    "3" = tagList(
      h4(paste("Surgery", rv$surgery_index)),
      textInput("med_hx_surgery", "What was the surgery?"),
      dateInput("surg_date", "Date of surgery", value = NULL),
      textAreaInput("surg_eat", "Impact on eating (N/A if none)", rows = 3),
      dateInput("surg_eat_normal", "Date eating returned to normal (N/A if none)", value = NULL),
      textAreaInput("surg_act", "Impact on activity (N/A if none)", rows = 3),
      dateInput("surg_act_normal", "Date activity returned to normal (N/A if none)", value = NULL),
      textAreaInput("surg_comp", "Complications (N/A if none)", rows = 3)
    ),

    # -------------------------------
    # 4–5. General Hospitalizations#####
    # -------------------------------
    "4" = tagList(
      h4("Hospitalizations"),
      numericInput("n_hosp", "How many times have you been hospitalized?", 0, min = 0)
    ),

    "5" = tagList(
      h4(paste("Hospitalization", rv$hosp_index)),
      textInput("med_hx_hospital", "Reason for hospitalization"),
      dateInput("hospital_admit_", "Date of admission", value = NULL),
      dateInput("hospital_discharge_", "Date of discharge", value = NULL),
      textAreaInput("hosp_eat", "Impact on eating (N/A if none)", rows = 3),
      dateInput("hosp_eat_normal", "Date eating returned to normal (N/A if none)", value = NULL),
      textAreaInput("hosp_act", "Impact on activity (N/A if none)", rows = 3),
      dateInput("hosp_act_normal", "Date activity returned to normal (N/A if none)", value = NULL)
    ),

    # -------------------------------
    # 6–7. ED Hospitalizations#####
    # -------------------------------
    "6" = tagList(
      h4("Eating Disorder–Related Hospitalizations"),
      radioButtons(
        "ed_hosp_yn",
        "Have you ever been hospitalized for an eating disorder (IOP, PHP, residential, or inpatient)?",
        choices = c("No" = 0, "Yes" = 1),
        inline = TRUE
      ),
      conditionalPanel(
        condition = "input.ed_hosp_yn == 1",
        numericInput(
          "ed_hospital_times",
          "How many times have you been hospitalized for an eating disorder?",
          value = 1,
          min = 1
        )
      )
    ),


    "7" = tagList(
      h4(paste("ED Hospitalization", rv$ed_hosp_index)),
      selectInput(
        "ed_hospital_loc",
        "Level of care",
        choices = c(
          "Intensive Outpatient Program (IOP)",
          "Partial Hospitalization Program (PHP)",
          "Residential",
          "Inpatient"
        )
      ),
      dateInput("ed_hospital_admit_", "Date admitted", value = NULL),
      dateInput("ed_hospital_discharge_", "Date discharged", value = NULL),
      textAreaInput(
        "ed_notes",
        "Any additional information about this admission?",
        rows = 3
      )
    ),

    # -------------------------------
    # 8–9. Injuries#####
    # -------------------------------
    "8" = tagList(
      h4("Injuries"),
      numericInput("n_injury", "How many injuries have you had?", 0, min = 0)
    ),

    "9" = tagList(
      h4(paste("Injury", rv$injury_index)),
      textInput("med_hx_injury", "Type of injury"),
      dateInput("med_hx_when_injury", "Date of injury", value = NULL),
      textAreaInput("injury_eat", "Impact on eating (N/A if none)", rows = 3),
      dateInput("injury_eat_normal", "Date eating returned to normal (N/A if none)", value = NULL),
      textAreaInput("injury_act", "Impact on activity (N/A if none)", rows = 3),
      dateInput("injury_act_normal", "Date activity returned to normal (N/A if none)", value = NULL),
      textAreaInput("injury_treat", "Treatment received (N/A if none)", rows = 3),
      dateInput("injury_treat_start", "Treatment start date (N/A if none)", value = NULL),
      dateInput("injury_treat_end", "Treatment end date (N/A if none)", value = NULL)
    ),

    # -------------------------------
    # 10–11. Medical Conditions######
    # -------------------------------
    "10" = tagList(
      h4("Medical Conditions"),
      numericInput(
        "medical_conditions",
        "How many medical conditions have you been diagnosed with?",
        0,
        min = 0
      )
    ),

    "11" = tagList(
      h4(paste("Medical Condition", rv$med_index)),
      textInput("medical_condition_name_", "Condition name"),
      dateInput("medical_conditon_diagnosis_", "Date of diagnosis", value = NULL),
      dateInput("symptom_onset_", "Date of symptom onsent", value = NULL),
      textAreaInput("med_notes", "Additional details (optional)", rows = 3)
    ),

    # -------------------------------
    # 12–13. Mental Health####
    # -------------------------------
    "12" = tagList(
      h4("Mental Health History"),
      numericInput(
        "mental_disorder_dx",
        "How many mental health diagnoses have you had?",
        0,
        min = 0
      )
    ),

    "13" = tagList(
      h4(paste("Mental Health Diagnosis", rv$mh_index)),
      textInput("mental_condition_name_", "Diagnosis"),
      dateInput("mental_dx_", "Date of diagnosis", value = NULL),
      textAreaInput("mh_notes", "Additional details (optional)", rows = 3)
    ),

    # -------------------------------
    # 14–15. Medications######
    # -------------------------------
    "14" = tagList(
      h4("Medications (Taken ≥ 3 months)"),
      numericInput("medication_no", "How many medications?", 0, min = 0)
    ),

    "15" = tagList(
      h4(paste("Medication", rv$meds_index)),
      textInput("med_name_", "Medication name"),
      textInput("med_dosage_", "Dosage"),
      dateInput("med_start_date_", "Start date", value = NULL),
      dateInput("med_end_date_", "End date (leave blank if ongoing)", value = NULL),
      radioButtons(
        "meds_yn",
        "Taken Consistently?",
        choices = c("No" = 0, "Yes" = 1),
        inline = TRUE
      ),

      textAreaInput("meds_reason", "Notes", rows = 2)
    )
  )
})

# Initialize reactive values

# Observe Next button
observeEvent(input$next_btn, {

  # ----------------------
  # STEP 1: Intro / Step 1
  # ----------------------
  if (rv$step == 1) {
    rv$step <- 2
  }

  # ----------------------
  # STEP 2: Surgeries
  # ----------------------
  else if (rv$step == 2) {
    rv$n_surgeries <- ifelse(is.na(input$n_surgeries), 0, input$n_surgeries)

    rv$surgery_index <- 1
    rv$step <- ifelse(rv$n_surgeries > 0, 3, 4)
  }

  # ----------------------
  # STEP 3: Surgery details
  # ----------------------
  else if (rv$step == 3) {
    rv$responses[[paste0("surgery_", rv$surgery_index)]] <- list(
      name = input$med_hx_surgery,
      date = input$surg_date,
      eating = input$surg_eat,
      activity = input$surg_act,
      complications = input$surg_comp
    )
    rv$surgery_index <- rv$surgery_index + 1
    if (rv$surgery_index > rv$n_surgeries) rv$step <- 4
  }

  # ----------------------
  # STEP 4: Hospitalizations
  # ----------------------
  else if (rv$step == 4) {
    rv$n_hosp <- ifelse(is.na(input$n_hosp), 0, input$n_hosp)

    rv$hosp_index <- 1
    rv$step <- ifelse(rv$n_hosp > 0, 5, 6)
  }

  # ----------------------
  # STEP 5: Hospitalization details
  # ----------------------
  else if (rv$step == 5) {
    rv$responses[[paste0("hospital_", rv$hosp_index)]] <- list(
      reason = input$med_hx_hospital,
      admit = input$hospital_admit_,
      discharge = input$hospital_discharge_,
      eating = input$hosp_eat,
      activity = input$hosp_act
    )
    rv$hosp_index <- rv$hosp_index + 1
    if (rv$hosp_index > rv$n_hosp) rv$step <- 6
  }

  # ----------------------
  # STEP 6: ED hospitalizations Y/N
  # ----------------------
  else if (rv$step == 6) {
    req(input$ed_hosp_yn)

    if (input$ed_hosp_yn == 1) {
      rv$ed_hospital_times <- ifelse(is.na(input$ed_hospital_times), 0, input$ed_hospital_times)

      rv$ed_hosp_index <- 1
      rv$step <- 7
    } else {
      rv$step <- 8
    }
  }

  # ----------------------
  # STEP 7: ED hospitalization details
  # ----------------------
  else if (rv$step == 7) {
    rv$responses[[paste0("ed_hospitalization_", rv$ed_hosp_index)]] <- list(
      level_of_care = input$ed_hospital_loc,
      admit_date = input$ed_hospital_admit_,
      discharge_date = input$ed_hospital_discharge_,
      notes = input$ed_notes
    )
    rv$ed_hosp_index <- rv$ed_hosp_index + 1
    if (rv$ed_hosp_index > rv$ed_hospital_times) rv$step <- 8
  }

  # ----------------------
  # STEP 8: Injuries
  # ----------------------
  else if (rv$step == 8) {
    rv$n_injury <- ifelse(is.na(input$n_injury), 0, input$n_injury)

    rv$injury_index <- 1
    rv$step <- ifelse(rv$n_injury > 0, 9, 10)
  }

  # ----------------------
  # STEP 9: Injury details
  # ----------------------
  else if (rv$step == 9) {
    rv$responses[[paste0("injury_", rv$injury_index)]] <- list(
      type = input$med_hx_injury,
      date = input$med_hx_when_injury,
      treatment = input$injury_treat,
      treatmentstart = input$injury_treat_start,
      treatmentend = input$injury_treat_end,
      eating = input$injury_eat,
      activity = input$injury_act,
      eatingnormal = input$injury_eat_normal,
      activitynormal = input$injury_act_normal
    )
    rv$injury_index <- rv$injury_index + 1
    if (rv$injury_index > rv$n_injury) rv$step <- 10
  }

  # ----------------------
  # STEP 10: Medical conditions
  # ----------------------
  else if (rv$step == 10) {
    rv$medical_conditions <- ifelse(is.na(input$medical_conditions), 0, input$medical_conditions)
    rv$med_index <- 1
    rv$step <- ifelse(rv$medical_conditions > 0, 11, 12)
  }

  else if (rv$step == 11) {
    rv$responses[[paste0("medical_", rv$med_index)]] <- list(
      name = input$medical_condition_name_,
      dx = input$medical_conditon_diagnosis_,
      symonset = input$symptom_onset_,
      notes = input$med_notes
    )
    rv$med_index <- rv$med_index + 1
    if (rv$med_index > rv$medical_conditions) rv$step <- 12
  }

  # ----------------------
  # STEP 12: Mental health
  # ----------------------
  else if (rv$step == 12) {
    rv$mental_disorder_dx <- ifelse(is.na(input$mental_disorder_dx), 0, input$mental_disorder_dx)

    rv$mh_index <- 1
    rv$step <- ifelse(rv$mental_disorder_dx > 0, 13, 14)
  }

  else if (rv$step == 13) {
    rv$responses[[paste0("mh_", rv$mh_index)]] <- list(
      diagnosis = input$mental_condition_name_,
      dx = input$mental_dx_,
      notes = input$mh_notes
    )
    rv$mh_index <- rv$mh_index + 1
    if (rv$mh_index > rv$mental_disorder_dx) rv$step <- 14
  }

  # ----------------------
  # STEP 14: Medications
  # ----------------------
  else if (rv$step == 14) {
    rv$medication_no <- ifelse(is.na(input$medication_no), 0, input$medication_no)
    rv$meds_index <- 1
    rv$step <- ifelse(rv$medication_no > 0, 15, 16)
  }

  else if (rv$step == 15) {
    rv$responses[[paste0("med_", rv$meds_index)]] <- list(
      name = input$med_name_,
      start = input$meds_start_date_,
      end = input$meds_end_date_,
      dosage = input$med_dosage_,
      consistent = input$meds_yn,
      reason = input$meds_reason
    )
    rv$meds_index <- rv$meds_index + 1
    if (rv$meds_index > rv$medication_no) {
      rv$step <- 16  # final step complete
      updateTabsetPanel(session, "ed_behaviors", selected = "Pt.2 ED Behaviors")
    }
  }
  })

###done go to next tab
  # ----------------------
  # FINAL STEP: Move to ED behaviors


rv1 <- reactiveValues(
  step = 1,
  responses = list(),
  stress_index = 1
)
####SOCIAL HISTORY PT. 3 #####
output$social_ui <- renderUI({

  switch(
    as.character(rv$step),

    # -------------------------------
    # 1. Intro######
    # -------------------------------
    "1" = tagList(
      h4("PART 3: Social History"),
      p("I will begin the interview by asking you about your medical and mental health history at various points in your life.")
    ),

    # -------------------------------
    # 2–3. Food insecurity
    # -------------------------------
    "2" = tagList(
      h4("Food Insecurity"),
      radioButtons(
        "food_yn",
        "18. Not able to afford food?",
        choices = c("No" = 0, "Yes" = 1),
        inline = TRUE
      ),
      conditionalPanel(
        condition = "input.food_yn == 1",
        dateInput("food_start", "Start date", value = NULL),
        dateInput("food_end", "End date (N/A if ongoing)", value = NULL)

      )
    ),
    "3" = tagList(
      h4("Stress outside of ED"),
      radioButtons(
        "stress_yn",
        "19. Changes of eating outside of eating disorder (e.g., family issues, school issues, bullying)?",
        choices = c("No" = 0, "Yes" = 1),
        inline = TRUE
      ),
      conditionalPanel(
        condition = "input.stress_yn == 1",
        numericInput("n_stress", "How many periods of stress?", 0, min = 0)

      )
    ),
    "4" = tagList(
      h4(paste("Period of Stress", rv1$stress_index)),
      dateInput("stress_start", "Start date", value = NULL),
      dateInput("stress_end", "End date", value = NULL),
      textAreaInput("stress_notes", "Notes (optional)", rows = 3)

  )
  )
})


##### interview ######
  output$combinedGraph <- renderPlot({
    df <- combined_data$data
    req(nrow(df) > 0)

    ggplot(df, aes(x = Age, y = Frequency, color = Behavior, group = Behavior)) +
      geom_line(data = subset(df, !is.na(Frequency)), aes(group = Behavior), size = 1) +
      geom_point(data = subset(df, !is.na(Frequency)), size = 3) +
      labs(x = "Age", y = "Frequency", color = "Behavior", title = "Combined Behavior Frequencies") +
      scale_color_manual(values = c(
        "Restriction" = "#ef6c45",
        "Fasting" = "#56b4e9",
        "Binge" = "#f1c40f",
        "Vomiting" = "#2ca02c",
        "Laxative" = "#d95f02",
        "Diuretic" = "#7570b3",
        "WLMED" = "#1b9e77",
        "Exercise" = "#e7298a"
      )) + theme(
        panel.background = element_blank(),
        plot.background  = element_blank(),
        axis.title.x = element_text(family = "Avenir", size = 16, face = "bold", color = "#1A4F66"),
        axis.title.y = element_text(family = "Avenir", size = 16, face = "bold", color = "#1A4F66"),
        axis.text.x  = element_text(family = "Avenir", size = 14, color = "#1A4F66"),
        axis.text.y  = element_text(family = "Avenir", size = 14, color = "#1A4F66"),
        plot.title   = element_text(family = "Avenir", size = 20, face = "bold", color = "#1A4F66"))
  })

  output$combinedGraph1 <- renderPlot({
    df <- combined_data$data
    req(nrow(df) > 0)

    ggplot(df, aes(x = Age, y = Frequency, color = Behavior, group = Behavior)) +
      geom_line(data = subset(df, !is.na(Frequency)), aes(group = Behavior), size = 1) +
      geom_point(data = subset(df, !is.na(Frequency)), size = 3) +
      labs(x = "Age", y = "Frequency (Per Month)", color = "Behavior", title = "Combined Behavior Frequencies") +
      scale_color_manual(values = c(
        "Restriction" = "#ef6c45",
        "Fasting" = "#56b4e9",
        "Binge" = "#f1c40f",
        "Vomiting" = "#2ca02c",
        "Laxative" = "#d95f02",
        "Diuretic" = "#7570b3",
        "WLMED" = "#1b9e77",
        "Exercise" = "#e7298a"
      )) + theme(
        panel.background = element_blank(),
        plot.background  = element_blank(),
        axis.title.x = element_text(family = "Avenir", size = 16, face = "bold", color = "#1A4F66"),
        axis.title.y = element_text(family = "Avenir", size = 16, face = "bold", color = "#1A4F66"),
        axis.text.x  = element_text(family = "Avenir", size = 14, color = "#1A4F66"),
        axis.text.y  = element_text(family = "Avenir", size = 14, color = "#1A4F66"),
        plot.title   = element_text(family = "Avenir", size = 20, face = "bold", color = "#1A4F66"))
  })
  output$combinedGraph2 <- renderPlot({
    df <- combined_data$data
    req(nrow(df) > 0)

    ggplot(df, aes(x = Age, y = Frequency, color = Behavior, group = Behavior)) +
      geom_line(data = subset(df, !is.na(Frequency)), aes(group = Behavior), size = 1) +
      geom_point(data = subset(df, !is.na(Frequency)), size = 3) +
      labs(x = "Age", y = "Frequency", color = "Behavior", title = "Combined Behavior Frequencies") +
      scale_color_manual(values = c(
        "Restriction" = "#ef6c45",
        "Fasting" = "#56b4e9",
        "Binge" = "#f1c40f",
        "Vomiting" = "#2ca02c",
        "Laxative" = "#d95f02",
        "Diuretic" = "#7570b3",
        "WLMED" = "#1b9e77",
        "Exercise" = "#e7298a"
      ))+ theme(
        panel.background = element_blank(),
        plot.background  = element_blank(),
        axis.title.x = element_text(family = "Avenir", size = 16, face = "bold", color = "#1A4F66"),
        axis.title.y = element_text(family = "Avenir", size = 16, face = "bold", color = "#1A4F66"),
        axis.text.x  = element_text(family = "Avenir", size = 14, color = "#1A4F66"),
        axis.text.y  = element_text(family = "Avenir", size = 14, color = "#1A4F66"),
        plot.title   = element_text(family = "Avenir", size = 20, face = "bold", color = "#1A4F66"))
  })
  output$combinedGraph3 <- renderPlot({
    df <- combined_data$data
    req(nrow(df) > 0)

    ggplot(df, aes(x = Age, y = Frequency, color = Behavior, group = Behavior)) +
      geom_line(data = subset(df, !is.na(Frequency)), aes(group = Behavior), size = 1) +
      geom_point(data = subset(df, !is.na(Frequency)), size = 3) +
      labs(x = "Age", y = "Frequency", color = "Behavior", title = "Combined Behavior Frequencies") +
      scale_color_manual(values = c(
        "Restriction" = "#ef6c45",
        "Fasting" = "#56b4e9",
        "Binge" = "#f1c40f",
        "Vomiting" = "#2ca02c",
        "Laxative" = "#d95f02",
        "Diuretic" = "#7570b3",
        "WLMED" = "#1b9e77",
        "Exercise" = "#e7298a"
      )) + theme(
        panel.background = element_blank(),
        plot.background  = element_blank(),
        axis.title.x = element_text(family = "Avenir", size = 16, face = "bold", color = "#1A4F66"),
        axis.title.y = element_text(family = "Avenir", size = 16, face = "bold", color = "#1A4F66"),
        axis.text.x  = element_text(family = "Avenir", size = 14, color = "#1A4F66"),
        axis.text.y  = element_text(family = "Avenir", size = 14, color = "#1A4F66"),
        plot.title   = element_text(family = "Avenir", size = 20, face = "bold", color = "#1A4F66"))
  })

  output$combinedGraph4 <- renderPlot({
    df <- combined_data$data
    req(nrow(df) > 0)

    ggplot(df, aes(x = Age, y = Frequency, color = Behavior, group = Behavior)) +
      geom_line(data = subset(df, !is.na(Frequency)), aes(group = Behavior), size = 1) +
      geom_point(data = subset(df, !is.na(Frequency)), size = 3) +
      labs(x = "Age", y = "Frequency", color = "Behavior", title = "Combined Behavior Frequencies") +
      scale_color_manual(values = c(
        "Restriction" = "#ef6c45",
        "Fasting" = "#56b4e9",
        "Binge" = "#f1c40f",
        "Vomiting" = "#2ca02c",
        "Laxative" = "#d95f02",
        "Diuretic" = "#7570b3",
        "WLMED" = "#1b9e77",
        "Exercise" = "#e7298a"
      )) + theme(
        panel.background = element_blank(),
        plot.background  = element_blank(),
        axis.title.x = element_text(family = "Avenir", size = 16, face = "bold", color = "#1A4F66"),
        axis.title.y = element_text(family = "Avenir", size = 16, face = "bold", color = "#1A4F66"),
        axis.text.x  = element_text(family = "Avenir", size = 14, color = "#1A4F66"),
        axis.text.y  = element_text(family = "Avenir", size = 14, color = "#1A4F66"),
        plot.title   = element_text(family = "Avenir", size = 20, face = "bold", color = "#1A4F66")
      )
  })

  output$combinedGraph5 <- renderPlot({
    df <- combined_data$data
    req(nrow(df) > 0)

    ggplot(df, aes(x = Age, y = Frequency, color = Behavior, group = Behavior)) +
      geom_line(data = subset(df, !is.na(Frequency)), aes(group = Behavior), size = 1) +
      geom_point(data = subset(df, !is.na(Frequency)), size = 3) +
      labs(x = "Age", y = "Frequency", color = "Behavior", title = "Combined Behavior Frequencies") +
      scale_color_manual(values = c(
        "Restriction" = "#ef6c45",
        "Fasting" = "#56b4e9",
        "Binge" = "#f1c40f",
        "Vomiting" = "#2ca02c",
        "Laxative" = "#d95f02",
        "Diuretic" = "#7570b3",
        "WLMED" = "#1b9e77",
        "Exercise" = "#e7298a"
      )) + theme(
        panel.background = element_blank(),
        plot.background  = element_blank(),
        axis.title.x = element_text(family = "Avenir", size = 16, face = "bold", color = "#1A4F66"),
        axis.title.y = element_text(family = "Avenir", size = 16, face = "bold", color = "#1A4F66"),
        axis.text.x  = element_text(family = "Avenir", size = 14, color = "#1A4F66"),
        axis.text.y  = element_text(family = "Avenir", size = 14, color = "#1A4F66"),
        plot.title   = element_text(family = "Avenir", size = 20, face = "bold", color = "#1A4F66"))
  })
  ###wh tab
  output$combinedGraph7 <- renderPlot({
    df <- combined_data$data
    req(nrow(df) > 0)

    ggplot(df, aes(x = Age, y = Frequency, color = Behavior, group = Behavior)) +
      geom_line(data = subset(df, !is.na(Frequency)), aes(group = Behavior), size = 1) +
      geom_point(data = subset(df, !is.na(Frequency)), size = 3) +
      scale_x_continuous(limits = c(7.5, 20.5), breaks = seq(8, 20, by = 2))  +

      labs(x = "Age", y = "Frequency (Per Month)", color = "Behavior", title = "Combined Behavior Frequencies") +
      scale_color_manual(values = c(
        "Restriction" = "#ef6c45",
        "Fasting" = "#56b4e9",
        "Binge" = "#f1c40f",
        "Vomiting" = "#2ca02c",
        "Laxative" = "#d95f02",
        "Diuretic" = "#7570b3",
        "WLMED" = "#1b9e77",
        "Exercise" = "#e7298a"
      ))  +
      theme(
        panel.background = element_blank(),
        plot.background  = element_blank(),
        axis.title.x = element_text(family = "Avenir", size = 16, face = "bold", color = "#1A4F66"),
        axis.title.y = element_text(family = "Avenir", size = 16, face = "bold", color = "#1A4F66"),
        axis.text.x  = element_text(family = "Avenir", size = 16, face= "bold", margin = margin(t = 1), color = "#1A4F66"),
        axis.text.y  = element_text(family = "Avenir", size = 14, face= "bold", color = "#1A4F66"),
        plot.title   = element_text(family = "Avenir", size = 20, face = "bold", color = "#1A4F66"),
        legend.title = element_text(family = "Avenir", size = 24, face = "bold", color = "#1A4F66"),
        legend.text  = element_text(family = "Avenir", size = 24, color = "#1A4F66"),
        legend.key.size = unit(3, "lines"),   # makes legend keys (color boxes) larger
        legend.position = "right"               # or "bottom", "top", etc.
      )
  })


  shared_data <- reactiveValues(
    current_date = NULL,
    birthdate = NULL,
    graduation_year = NULL,
    restriction_data = NULL,
    fasting_data = NULL,
    binge_data = NULL
  )
  output$combinedGraph6 <- renderPlot({
    df <- combined_data$data
    req(nrow(df) > 0)

    ggplot(df, aes(x = Age, y = Frequency, color = Behavior, group = Behavior)) +
      geom_line(data = subset(df, !is.na(Frequency)), aes(group = Behavior), size = 1) +
      geom_point(data = subset(df, !is.na(Frequency)), size = 3) +
      labs(x = "Age", y = "Frequency", color = "Behavior", title = "Combined Behavior Frequencies") +
      scale_color_manual(values = c(
        "Restriction" = "#ef6c45",
        "Fasting" = "#56b4e9",
        "Binge" = "#f1c40f",
        "Vomiting" = "#2ca02c",
        "Laxative" = "#d95f02",
        "Diuretic" = "#7570b3",
        "WLMED" = "#1b9e77",
        "Exercise" = "#e7298a"
      )) + theme(
        panel.background = element_blank(),
        plot.background  = element_blank(),
        axis.title.x = element_text(family = "Avenir", size = 16, face = "bold", color = "#1A4F66"),
        axis.title.y = element_text(family = "Avenir", size = 16, face = "bold", color = "#1A4F66"),
        axis.text.x  = element_text(family = "Avenir", size = 14, color = "#1A4F66"),
        axis.text.y  = element_text(family = "Avenir", size = 14, color = "#1A4F66"),
        plot.title   = element_text(family = "Avenir", size = 20, face = "bold", color = "#1A4F66")
      )
  })

  shared_data <- reactiveValues(
    current_date = NULL,
    birthdate = NULL,
    graduation_year = NULL,
    restriction_data = NULL,
    fasting_data = NULL,
    binge_data = NULL
  )


  combined_data <- reactiveValues(
    data = data.frame(Age = numeric(), Frequency = numeric(), Behavior = character())
  )

  add_to_combined_data <- function(new_data, behavior_name) {
    new_data$Behavior <- behavior_name

    # Remove ANY old version of this behavior
    combined_data$data <- combined_data$data[combined_data$data$Behavior != behavior_name, ]

    # Add the new version
    combined_data$data <- rbind(combined_data$data, new_data)
  }


  observeEvent(input$proceed, {
    shared_data$current_date <- input$current_date
    shared_data$birthdate <- input$birthdate
    shared_data$graduation_year <- input$graduation_year
    showNotification("Participant information saved! You can now use the other tabs.")
  })


  ages_restriction <- reactive({
    req(input$age_start1, input$age_end1)
    seq(input$age_start1, input$age_end1, by = 0.5)
  })
  ages_fasting <- reactive({
    req(input$age_start2, input$age_end2)
    seq(input$age_start2, input$age_end2, by = 0.5)
  })
  ages_binge <- reactive({
    req(input$age_start3, input$age_end3)
    seq(input$age_start3, input$age_end3, by = 0.5)
  })

  ages_vomit <- reactive({
    req(input$age_start4, input$age_end4)
    seq(input$age_start4, input$age_end4, by = 0.5)
  })
  ages_laxative <- reactive({
    req(input$age_start5, input$age_end5)
    seq(input$age_start5, input$age_end5, by = 0.5)
  })
  ages_diuretic <- reactive({
    req(input$age_start6, input$age_end6)
    seq(input$age_start6, input$age_end6, by = 0.5)
  })
  ages_wlmed <- reactive({
    req(input$age_start7, input$age_end7)
    seq(input$age_start7, input$age_end7, by = 0.5)
  })
  ages_exercise <- reactive({
    req(input$age_start8, input$age_end8)
    seq(input$age_start8, input$age_end8, by = 0.5)
  })
  ### ---------------- RESTRICTION INPUT UI ----------------
  output$behavior_input_ui1 <- renderUI({
    lapply(ages_restriction(), function(age) {
      numericInput(
        inputId = paste0("freq1_", age),
        label   = paste("Frequency at Age", age),
        value   = 0,
        min     = 0
      )
    })
  })

  output$behavior_input_ui2 <- renderUI({
    lapply(ages_fasting(), function(age) {
      numericInput(inputId = paste0("freq2_", age),
                   label = paste("Frequency at Age", age),
                   value = 0,
                   min = 0)
    })
  })
  output$behavior_input_ui3 <- renderUI({
    lapply(ages_binge(), function(age) {
      numericInput(inputId = paste0("freq3_", age),
                   label = paste("Frequency at Age", age),
                   value = 0,
                   min = 0)
    })
  })
  output$behavior_input_ui4 <- renderUI({
    lapply(ages_vomit(), function(age) {
      numericInput(inputId = paste0("freq4_", age),
                   label = paste("Frequency at Age", age),
                   value = 0,
                   min = 0)
    })
  })
  output$behavior_input_ui5 <- renderUI({
    lapply(ages_laxative(), function(age) {
      numericInput(inputId = paste0("freq5_", age),
                   label = paste("Frequency at Age", age),
                   value = 0,
                   min = 0)
    })
  })
  output$behavior_input_ui6 <- renderUI({
    lapply(ages_diuretic(), function(age) {
      numericInput(inputId = paste0("freq6_", age),
                   label = paste("Frequency at Age", age),
                   value = 0,
                   min = 0)
    })
  })
  output$behavior_input_ui7 <- renderUI({
    lapply(ages_wlmed(), function(age) {
      numericInput(inputId = paste0("freq7_", age),
                   label = paste("Frequency at Age", age),
                   value = 0,
                   min = 0)
    })
  })
  output$behavior_input_ui8 <- renderUI({
    lapply(ages_exercise(), function(age) {
      numericInput(inputId = paste0("freq8_", age),
                   label = paste("Frequency at Age", age),
                   value = 0,
                   min = 0)
    })
  })
  #Restriction Calculations######

  timeline_data1 <- reactive({
    req(shared_data$birthdate, shared_data$graduation_year)

    birthdate1 <- as.Date(shared_data$birthdate)
    kindergarten_year1 <- shared_data$graduation_year - 13

    calculate_grade <- function(age) {
      age_date <- birthdate1 + (age * 365.25)
      year <- as.numeric(format(age_date, "%Y"))
      sept1 <- as.Date(paste0(year, "-09-01"))
      school_year <- ifelse(age_date < sept1, year - 1, year)
      grade <- school_year - kindergarten_year1
      max(0, min(16, grade))
    }

    grade_to_label <- function(grade) {
      if (grade <= 12) {
        if (grade == 0) return("Kindergarten")
        return(paste("Grade", grade))
      }
      switch(as.character(grade),
             "13"="College Freshman",
             "14"="College Sophomore",
             "15"="College Junior",
             "16"="College Senior")
    }

    get_grade_label <- function(age) {
      date <- birthdate1 + (age * 365.25)
      month <- as.numeric(format(date, "%m"))
      grade <- calculate_grade(age)

      if (month %in% 6:8) {
        if (grade == 16) return("Summer after College")
        return(paste("Summer before", grade_to_label(min(16, grade+1))))
      }
      grade_to_label(grade)
    }

    current_age <- as.numeric(difftime(Sys.Date(), birthdate1, units = "days")) / 365.25
    base_ages <- seq(8, floor(current_age), by = 0.5)

    data.frame(
      Age = base_ages,
      Date = birthdate1 + (base_ages * 365.25),
      Label = sapply(base_ages, get_grade_label)
    )
  })


    observeEvent(input$restrict_ref_ui, {
      showModal(
        modalDialog(
          title = "Restriction – Reference",

          p("Criteria:
Did you...

1. Deliberately try to limit the overall amount of food you ate?
[No = the participant denies attempting to decrease the amount of food they were eating
Subclinical = the participant attempted to reduce the amount of food/calories they were eating for a reason other than weight loss, was following a medically-prescribed eating plan in which caloric or food restriction was not drastic, or reduced the amount of food or calories they were eating, but the reduction was not drastic (< 300 calorie reduction from baseline)
Clinical = the participant attempted to reduce the amount of food/calories they were eating drastically (> than 300 calorie reduction from baseline)]

2. Try to exclude food that you liked from your diet?
[No = the participant denies excluding any food they liked from their diet
Subclinical = the participant stopped eating 1-2 foods or excluded foods for reasons other than weight loss
Clinical = the participant stopped eating more than 2 foods or excluded entire food groups with the goal of losing weight]

3. Try to follow rules related to food, such as calorie counting or cutting out food groups?
[No = the participant denies following rules related to food
Subclinical = the participant read food labels, considered the calories in food before eating it, or followed rules for medical reasons rather than weight loss
Clinical = the participant followed a calorie limit, tracked food in an app, or followed rules that significantly limit the amount or types of food they could consume for the purpose of losing weight]

4. Skip meals?
[No = the participant denies skipping meals (i.e., consistently ate 3 meals per day whenever possible)
Subclinical = the participant skipped meals for reasons other than weight loss (not hungry in the morning, lack of time), skipped meals less than twice per week, or skipped meals to save calories for other meals]
Clinical = the participant skipped meals more than 3 times per week for the purposes of weight loss]."),

          easyClose = TRUE,
          footer = modalButton("Close")
        )
      )
    })
    # --- CONVERT NUMERIC GRADES TO LABELS ---
    output$timeline1 <- renderTimevis({
      df <- timeline_data1()

      timevis(data.frame(
        id = 1:nrow(df),
        start = df$Date,
        content = paste0("Age: ", df$Age, "<br>", df$Label)
      )) %>%
        setOptions(list(editable = FALSE, align = "center"))
    })

    observeEvent(input$submit1, {
      req(shared_data$birthdate)

      entered_ages <- ages_restriction()
      if (length(entered_ages) == 0) return()

      birthdate1 <- as.Date(shared_data$birthdate)

      start_date <- birthdate1 + min(entered_ages) * 365.25
      end_date   <- birthdate1 + max(entered_ages) * 365.25


    })

    restriction_data <- reactive({
      ages <- ages_restriction()

      data.frame(
        Age = ages,
        Frequency = sapply(ages, function(age)
          input[[paste0("freq1_", age)]] %||% NA_real_
        )
      )
    })


    observeEvent(input$submit1, {
      add_to_combined_data(restriction_data(), "Restriction")
    })

    output$behaviorPlot1 <- renderPlotly({

      plot_ly(
        restriction_data(),
        x = ~Age,
        y = ~Frequency,
        type = "scatter",
        mode = "lines+markers",
        line = list(color = "#ef6c45", width = 3),
        marker = list(size = 8, color = "#1a4f66")
      ) %>%
        layout(
          title = list(text = "", font = list(size = 20, color = "#1A4F66")),
          xaxis = list(
            title = "Age",
            tickmode = "linear",
            tick0 = 0,     # starting age
            dtick = 0.5    # half-year steps
          ),
          yaxis = list(title = "Frequency",
          tick0 = 0),
          plot_bgcolor = "white",
          paper_bgcolor = "rgba(0,0,0,0)"
        )
    })

    observeEvent(input$submit1, {
      add_to_combined_data(restriction_data(), "Restriction")
    })

  # Fasting Calculations ####
    fasting_timeline <- reactive({
      req(shared_data$birthdate, shared_data$graduation_year)

      birthdate <- as.Date(shared_data$birthdate)
      kindergarten_year <- shared_data$graduation_year - 13

      # --- Grade calculation ---
      calculate_grade <- function(age) {
        age_date <- birthdate + (age * 365.25)
        year <- as.numeric(format(age_date, "%Y"))
        sept1 <- as.Date(paste0(year, "-09-01"))
        school_year <- ifelse(age_date < sept1, year - 1, year)
        grade <- school_year - kindergarten_year
        max(0, min(16, grade))
      }

      grade_to_label <- function(grade) {
        if (grade <= 12) {
          if (grade == 0) return("Kindergarten")
          return(paste("Grade", grade))
        }
        switch(as.character(grade),
               "13"="College Freshman",
               "14"="College Sophomore",
               "15"="College Junior",
               "16"="College Senior")
      }

      get_grade_label <- function(age) {
        date <- birthdate + (age * 365.25)
        month <- as.numeric(format(date, "%m"))
        grade <- calculate_grade(age)

        if (month %in% 6:8) {
          if (grade == 16) return("Summer after College")
          return(paste("Summer before", grade_to_label(min(16, grade + 1))))
        }
        grade_to_label(grade)
      }

      # --- All ages from 8 to current age, by 0.5 years ---
      current_age <- as.numeric(difftime(Sys.Date(), birthdate, units = "days")) / 365.25
      base_ages <- seq(8, floor(current_age), by = 0.5)

      data.frame(
        Age = base_ages,
        Date = birthdate + (base_ages * 365.25),
        Label = sapply(base_ages, get_grade_label)
      )
    })




    output$timeline2 <- renderTimevis({
      df <- fasting_timeline()
      req(nrow(df) > 0)

      timevis(data.frame(
        id = 1:nrow(df),
        start = df$Date,
        content = paste0("Age: ", df$Age, "<br>", df$Label)
      )) %>%
        setOptions(list(editable = FALSE, align = "center"))
    })


    fasting_data <- reactive({
      ages <- ages_fasting()  # only ages with user-entered inputs
      req(length(ages) > 0)

      data.frame(
        Age = ages,
        Frequency = sapply(ages, function(age) input[[paste0("freq2_", age)]] %||% NA_real_)
      )
    })

    output$behaviorPlot2 <- renderPlotly({
      df <- fasting_data()
      req(nrow(df) > 0)

      plot_ly(
        df,
        x = ~Age,
        y = ~Frequency,
        type = "scatter",
        mode = "lines+markers",
        line = list(color = "#ef6c45", width = 3),
        marker = list(size = 8, color = "#1a4f66")
      ) %>%
        layout(
          title = list(text = "", font = list(size = 20, color = "#1A4F66")),
          xaxis = list(
            title = "Age",
            tickmode = "linear",
            tick0 = 0,
            dtick = 0.5
          ),
          yaxis = list(title = "Frequency", tick0 = 0),
          plot_bgcolor = "white",
          paper_bgcolor = "rgba(0,0,0,0)"
        )
    })


  #Vomiting Calculations ####
    # ===============================
    # VOMITING (match FASTING logic)
    # ===============================

    # --- Full timeline: ages 8 -> current age by 0.5 ---
    vomit_timeline <- reactive({
      req(shared_data$birthdate, shared_data$graduation_year)

      birthdate <- as.Date(shared_data$birthdate)
      kindergarten_year <- shared_data$graduation_year - 13

      calculate_grade <- function(age) {
        age_date <- birthdate + (age * 365.25)
        year <- as.numeric(format(age_date, "%Y"))
        sept1 <- as.Date(paste0(year, "-09-01"))
        school_year <- ifelse(age_date < sept1, year - 1, year)
        grade <- school_year - kindergarten_year
        max(0, min(16, grade))
      }

      grade_to_label <- function(grade) {
        if (grade <= 12) {
          if (grade == 0) return("Kindergarten")
          return(paste("Grade", grade))
        }
        switch(as.character(grade),
               "13"="College Freshman",
               "14"="College Sophomore",
               "15"="College Junior",
               "16"="College Senior")
      }

      get_grade_label <- function(age) {
        date <- birthdate + (age * 365.25)
        month <- as.numeric(format(date, "%m"))
        grade <- calculate_grade(age)

        if (month %in% 6:8) {
          if (grade == 16) return("Summer after College")
          return(paste("Summer before", grade_to_label(min(16, grade + 1))))
        }
        grade_to_label(grade)
      }

      current_age <- as.numeric(difftime(Sys.Date(), birthdate, units = "days")) / 365.25
      base_ages <- seq(8, floor(current_age), by = 0.5)

      data.frame(
        Age   = base_ages,
        Date  = birthdate + (base_ages * 365.25),
        Label = sapply(base_ages, get_grade_label)
      )
    })

    output$timeline4 <- renderTimevis({
      df <- vomit_timeline()
      req(nrow(df) > 0)

      timevis(data.frame(
        id = 1:nrow(df),
        start = df$Date,
        content = paste0("Age: ", df$Age, "<br>", df$Label)
      )) %>%
        setOptions(list(editable = FALSE, align = "center"))
    })


    # --- Plotly data: only user-entered ages ---
    vomit_data <- reactive({
      ages <- ages_vomit()         # ONLY ages with user-entered inputs
      req(length(ages) > 0)

      data.frame(
        Age = ages,
        Frequency = sapply(ages, function(age) input[[paste0("freq4_", age)]] %||% NA_real_)
      )
    })

    output$behaviorPlot4 <- renderPlotly({
      df <- vomit_data()
      req(nrow(df) > 0)

      plot_ly(
        df,
        x = ~Age,
        y = ~Frequency,
        type = "scatter",
        mode = "lines+markers",
        line = list(color = "#ef6c45", width = 3),
        marker = list(size = 8, color = "#1a4f66")
      ) %>%
        layout(
          title = list(text = "", font = list(size = 20, color = "#1A4F66")),
          xaxis = list(
            title = "Age",
            tickmode = "linear",
            tick0 = 0,
            dtick = 0.5
          ),
          yaxis = list(title = "Frequency", tick0 = 0),
          plot_bgcolor = "white",
          paper_bgcolor = "rgba(0,0,0,0)"
        )
    })


    # --- If you only want to add to combined data on submit (like a "save" button) ---
    observeEvent(input$submit4, {
      req(shared_data$birthdate, shared_data$graduation_year)})


  # Laxative Calculation ####
    # ===============================
    # LAXATIVE (match FASTING logic)
    # ===============================

    # --- Full timeline: ages 8 -> current age by 0.5 ---
    laxative_timeline <- reactive({
      req(shared_data$birthdate, shared_data$graduation_year)

      birthdate <- as.Date(shared_data$birthdate)
      kindergarten_year <- shared_data$graduation_year - 13

      calculate_grade <- function(age) {
        age_date <- birthdate + (age * 365.25)
        year <- as.numeric(format(age_date, "%Y"))
        sept1 <- as.Date(paste0(year, "-09-01"))
        school_year <- ifelse(age_date < sept1, year - 1, year)
        grade <- school_year - kindergarten_year
        max(0, min(16, grade))
      }

      grade_to_label <- function(grade) {
        if (grade <= 12) {
          if (grade == 0) return("Kindergarten")
          return(paste("Grade", grade))
        }
        switch(as.character(grade),
               "13"="College Freshman",
               "14"="College Sophomore",
               "15"="College Junior",
               "16"="College Senior")
      }

      get_grade_label <- function(age) {
        date <- birthdate + (age * 365.25)
        month <- as.numeric(format(date, "%m"))
        grade <- calculate_grade(age)

        if (month %in% 6:8) {
          if (grade == 16) return("Summer after College")
          return(paste("Summer before", grade_to_label(min(16, grade + 1))))
        }
        grade_to_label(grade)
      }

      current_age <- as.numeric(difftime(Sys.Date(), birthdate, units = "days")) / 365.25
      base_ages <- seq(8, floor(current_age), by = 0.5)

      data.frame(
        Age   = base_ages,
        Date  = birthdate + (base_ages * 365.25),
        Label = sapply(base_ages, get_grade_label)
      )
    })

    output$timeline5 <- renderTimevis({
      df <- laxative_timeline()
      req(nrow(df) > 0)

      timevis(data.frame(
        id = 1:nrow(df),
        start = df$Date,
        content = paste0("Age: ", df$Age, "<br>", df$Label)
      )) %>%
        setOptions(list(editable = FALSE, align = "center"))
    })


    # --- Plotly data: only user-entered ages ---
    laxative_data <- reactive({
      ages <- ages_laxative()
      req(length(ages) > 0)

      data.frame(
        Age = ages,
        Frequency = sapply(ages, function(age) input[[paste0("freq5_", age)]] %||% NA_real_)
      )
    })

    output$behaviorPlot5 <- renderPlotly({
      df <- laxative_data()
      req(nrow(df) > 0)

      plot_ly(
        df,
        x = ~Age,
        y = ~Frequency,
        type = "scatter",
        mode = "lines+markers",
        line = list(color = "#ef6c45", width = 3),
        marker = list(size = 8, color = "#1a4f66")
      ) %>%
        layout(
          title = list(text = "", font = list(size = 20, color = "#1A4F66")),
          xaxis = list(
            title = "Age",
            tickmode = "linear",
            tick0 = 0,
            dtick = 0.5
          ),
          yaxis = list(title = "Frequency", tick0 = 0),
          plot_bgcolor = "white",
          paper_bgcolor = "rgba(0,0,0,0)"
        )
    })


    # --- Save to combined data only when they click submit5 ---
    observeEvent(input$submit4, {
      req(shared_data$birthdate, shared_data$graduation_year)})

  # Fasting Calculations ####
  observeEvent(input$submit2, {

    birthdate2 <- as.Date(shared_data$birthdate)
    kindergarten_year2 <- shared_data$graduation_year - 13   # backwards calculation

    # --- GRADE CALCULATION 0–16 ---
    calculate_grade <- function(age) {
      age_date <- birthdate2 + (age * 365.25)

      year <- as.numeric(format(age_date, "%Y"))
      sept1 <- as.Date(paste0(year, "-09-01"))

      # before Sept 1 → previous school year
      school_year <- ifelse(age_date < sept1, year - 1, year)

      grade <- school_year - kindergarten_year2
      grade <- max(0, min(16, grade))   # includes college years
      return(grade)
    }

    # --- CONVERT NUMERIC GRADE TO LABEL ---
    grade_to_label <- function(grade) {
      if (grade <= 12) {
        if (grade == 0) return("Kindergarten")
        return(paste("Grade", grade))
      }
      switch(
        as.character(grade),
        "13" = "College Freshman",
        "14" = "College Sophomore",
        "15" = "College Junior",
        "16" = "College Senior"
      )
    }

    # --- SCHOOL-YEAR vs SUMMER LABELS ---
    get_grade_label <- function(age) {
      date <- birthdate2 + (age * 365.25)
      month <- as.numeric(format(date, "%m"))

      grade <- calculate_grade(age)

      # summer months (June–August)
      if (month %in% 6:8) {

        # summer after last college year
        if (grade == 16) return("Summer after College")

        next_grade <- min(16, grade + 1)
        next_label <- grade_to_label(next_grade)

        return(paste("Summer before", next_label))
      }

      # regular school-year label
      return(grade_to_label(grade))
    }

    # --- BUILD TIMELINE DATA ---
    timeline_data2 <- data.frame(
      Age = ages_fasting(),
      Date = birthdate2 + (ages_fasting() * 365.25),
      Label = sapply(ages_fasting(), get_grade_label)
    )

    # --- RENDER TIMELINE ---
    output$timeline2 <- renderTimevis({
      timevis(data.frame(
        id = 1:nrow(timeline_data2),
        start = timeline_data2$Date,
        content = paste0(
          "Age: ", timeline_data2$Age,
          "<br>", timeline_data2$Label
        )
      )) %>%
        setOptions(list(editable = FALSE, align = "center"))
    })


    fasting_data <- data.frame(
      Age = ages_fasting(),
      Frequency = sapply(ages_fasting(), function(age) input[[paste0("freq2_", age)]] %||% 0)
    )

    add_to_combined_data(fasting_data, "Fasting")

    output$behaviorPlot2 <- renderPlot({
      ggplot(fasting_data, aes(x = Age, y = Frequency)) +
        geom_line(color = "#33a02c", size = 1) +
        geom_point(size = 3, color = "#1a4f66") +
        labs(x = "Age", y = "Frequency", title = "Fasting") + theme(
          panel.background = element_blank(),
          plot.background  = element_blank(),
          axis.title.x = element_text(family = "Avenir", size = 16, face = "bold", color = "#1A4F66"),
          axis.title.y = element_text(family = "Avenir", size = 16, face = "bold", color = "#1A4F66"),
          axis.text.x  = element_text(family = "Avenir", size = 14, color = "#1A4F66"),
          axis.text.y  = element_text(family = "Avenir", size = 14, color = "#1A4F66"),
          plot.title   = element_text(family = "Avenir", size = 20, face = "bold", color = "#1A4F66")
        )
    })
  })
  #Vomiting Calculations ####
  observeEvent(input$submit4, {

    birthdate4 <- as.Date(shared_data$birthdate)
    kindergarten_year4 <- shared_data$graduation_year - 13

    # --- GRADE CALCULATION 0–16 ---
    calculate_grade <- function(age) {
      age_date <- birthdate4 + (age * 365.25)

      year <- as.numeric(format(age_date, "%Y"))
      sept1 <- as.Date(paste0(year, "-09-01"))

      # Before Sept 1 → still in last school year
      school_year <- ifelse(age_date < sept1, year - 1, year)

      grade <- school_year - kindergarten_year4
      grade <- max(0, min(16, grade))
      return(grade)
    }

    # --- MAP NUMERIC GRADES TO LABELS ---
    grade_to_label <- function(grade) {
      if (grade <= 12) {
        if (grade == 0) return("Kindergarten")
        return(paste("Grade", grade))
      }
      switch(
        as.character(grade),
        "13" = "College Freshman",
        "14" = "College Sophomore",
        "15" = "College Junior",
        "16" = "College Senior"
      )
    }

    # --- DETERMINE SUMMER vs SCHOOL YEAR ---
    get_grade_label <- function(age) {
      date <- birthdate4 + (age * 365.25)
      m <- as.numeric(format(date, "%m"))

      grade <- calculate_grade(age)

      # June, July, August → summer
      if (m %in% 6:8) {

        # After senior year of college
        if (grade == 16) {
          return("Summer after College")
        }

        next_grade <- min(16, grade + 1)
        next_label <- grade_to_label(next_grade)

        return(paste("Summer before", next_label))
      }

      # In school year
      return(grade_to_label(grade))
    }

    # --- BUILD TIMELINE DATA ---
    timeline_data4 <- data.frame(
      Age = ages_vomit(),
      Date = birthdate4 + (ages_vomit() * 365.25),
      Label = sapply(ages_vomit(), get_grade_label)
    )

    # --- RENDER TIMEVIS TIMELINE ---
    output$timeline4 <- renderTimevis({
      timevis(data.frame(
        id = 1:nrow(timeline_data4),
        start = timeline_data4$Date,
        content = paste0(
          "Age: ", timeline_data4$Age,
          "<br>", timeline_data4$Label
        )
      )) %>%
        setOptions(list(editable = FALSE, align = "center"))
    })

    vomit_data <- data.frame(
      Age = ages_vomit(),
      Frequency = sapply(ages_vomit(), function(age) input[[paste0("freq4_", age)]] %||% 0)
    )

    add_to_combined_data(vomit_data, "Vomiting")

    output$behaviorPlot4 <- renderPlotly({
      ggplot(vomit_data, aes(x = Age, y = Frequency)) +
        geom_line(color = "#6a3d9a", size = 1) +
        geom_point(size = 3, color = "#1a4f66") +
        labs(x = "Age", y = "Frequency", title = "Vomit") + theme(
          panel.background = element_blank(),
          plot.background  = element_blank(),
          axis.title.x = element_text(family = "Avenir", size = 16, face = "bold", color = "#1A4F66"),
          axis.title.y = element_text(family = "Avenir", size = 16, face = "bold", color = "#1A4F66"),
          axis.text.x  = element_text(family = "Avenir", size = 14, color = "#1A4F66"),
          axis.text.y  = element_text(family = "Avenir", size = 14, color = "#1A4F66"),
          plot.title   = element_text(family = "Avenir", size = 20, face = "bold", color = "#1A4F66")
        )

    })
  })
  # Laxative Calculation ####
  observeEvent(input$submit5, {
    birthdate5 <- as.Date(shared_data$birthdate)
    kindergarten_year5 <- shared_data$graduation_year - 13

    # grade → label conversion
    grade_label <- function(grade_numeric) {
      if (grade_numeric <= 12) return(as.character(grade_numeric))
      switch(
        as.character(grade_numeric),
        "13" = "Freshman",
        "14" = "Sophomore",
        "15" = "Junior",
        "16" = "Senior",
        as.character(grade_numeric)
      )
    }

    # calculate numeric grade (0–16)
    calculate_grade <- function(age) {
      age_date <- birthdate5 + (age * 365.25)
      age_year <- as.numeric(format(age_date, "%Y"))
      grade <- age_year - kindergarten_year5
      max(0, min(16, grade))  # clamp 0–16
    }

    ages <- ages_laxative()

    timeline_data5 <- data.frame(
      Age = ages,
      Grade_numeric = sapply(ages, calculate_grade),
      Date = birthdate5 + (ages * 365.25)
    )

    timeline_data5$Grade_label <- sapply(timeline_data5$Grade_numeric, grade_label)

    output$timeline5 <- renderTimevis({
      timevis(
        data.frame(
          id = 1:nrow(timeline_data5),
          start = timeline_data5$Date,
          content = paste0(
            "Age: ", timeline_data5$Age,
            "<br> Grade: ", timeline_data5$Grade_label
          )
        )
      ) %>%
        setOptions(list(editable = FALSE, align = "center"))
    })

    laxative_data <- data.frame(
      Age = ages_laxative(),
      Frequency = sapply(ages_laxative(), function(age) input[[paste0("freq5_", age)]] %||% 0)
    )

    add_to_combined_data(laxative_data(), "Laxative")

    output$behaviorPlot5 <- renderPlot({
      ggplot(laxative_data, aes(x = Age, y = Frequency)) +
        geom_line(color = "#e31a1c", size = 1) +
        geom_point(size = 3, color = "#1a4f66") +
        labs(x = "Age", y = "Frequency", title = "Laxative") + theme(
          panel.background = element_blank(),
          plot.background  = element_blank(),
          axis.title.x = element_text(family = "Avenir", size = 16, face = "bold", color = "#1A4F66"),
          axis.title.y = element_text(family = "Avenir", size = 16, face = "bold", color = "#1A4F66"),
          axis.text.x  = element_text(family = "Avenir", size = 14, color = "#1A4F66"),
          axis.text.y  = element_text(family = "Avenir", size = 14, color = "#1A4F66"),
          plot.title   = element_text(family = "Avenir", size = 20, face = "bold", color = "#1A4F66")
        )
    })

  })

  # Diuretic Calculation ####
    # --- DIURETIC TIMELINE (8 -> current age) ---
    diuretic_timeline <- reactive({
      req(shared_data$birthdate, shared_data$graduation_year)

      birthdate <- as.Date(shared_data$birthdate)
      kindergarten_year <- shared_data$graduation_year - 13

      calculate_grade <- function(age) {
        age_date <- birthdate + (age * 365.25)
        year <- as.numeric(format(age_date, "%Y"))
        sept1 <- as.Date(paste0(year, "-09-01"))
        school_year <- ifelse(age_date < sept1, year - 1, year)
        grade <- school_year - kindergarten_year
        max(0, min(16, grade))
      }

      grade_to_label <- function(grade) {
        if (grade <= 12) {
          if (grade == 0) return("Kindergarten")
          return(paste("Grade", grade))
        }
        switch(as.character(grade),
               "13"="College Freshman",
               "14"="College Sophomore",
               "15"="College Junior",
               "16"="College Senior")
      }

      get_grade_label <- function(age) {
        date <- birthdate + (age * 365.25)
        month <- as.numeric(format(date, "%m"))
        grade <- calculate_grade(age)

        if (month %in% 6:8) {
          if (grade == 16) return("Summer after College")
          return(paste("Summer before", grade_to_label(min(16, grade + 1))))
        }
        grade_to_label(grade)
      }

      current_age <- as.numeric(difftime(Sys.Date(), birthdate, units = "days")) / 365.25
      base_ages <- seq(8, floor(current_age), by = 0.5)

      data.frame(
        Age = base_ages,
        Date = birthdate + (base_ages * 365.25),
        Label = sapply(base_ages, get_grade_label)
      )
    })

    output$timeline6 <- renderTimevis({
      df <- diuretic_timeline()
      req(nrow(df) > 0)

      timevis(data.frame(
        id = 1:nrow(df),
        start = df$Date,
        content = paste0("Age: ", df$Age, "<br>", df$Label)
      )) %>%
        setOptions(list(editable = FALSE, align = "center"))
    })

    # --- DIURETIC DATA (only entered ages) ---
    diuretic_data <- reactive({
      ages <- ages_diuretic()
      req(length(ages) > 0)

      data.frame(
        Age = ages,
        Frequency = sapply(ages, function(age) input[[paste0("freq6_", age)]] %||% NA_real_)
      )
    })

    output$behaviorPlot6 <- renderPlotly({
      df <- diuretic_data()
      req(nrow(df) > 0)

      plot_ly(
        df,
        x = ~Age,
        y = ~Frequency,
        type = "scatter",
        mode = "lines+markers",
        line = list(color = "#ef6c45", width = 3),
        marker = list(size = 8, color = "#1a4f66")
      ) %>%
        layout(
          title = list(text = "", font = list(size = 20, color = "#1A4F66")),
          xaxis = list(title = "Age", tickmode = "linear", tick0 = 0, dtick = 0.5),
          yaxis = list(title = "Frequency", tick0 = 0),
          plot_bgcolor = "white",
          paper_bgcolor = "rgba(0,0,0,0)"
        )
    })

    observeEvent(input$submit6, {
      req(shared_data$birthdate, shared_data$graduation_year)
      add_to_combined_data ((diuretic_data()), "Diuretic")
    })


    # --- WLMED TIMELINE (8 -> current age) ---
    wlmed_timeline <- reactive({
      req(shared_data$birthdate, shared_data$graduation_year)

      birthdate <- as.Date(shared_data$birthdate)
      kindergarten_year <- shared_data$graduation_year - 13

      calculate_grade <- function(age) {
        age_date <- birthdate + (age * 365.25)
        year <- as.numeric(format(age_date, "%Y"))
        sept1 <- as.Date(paste0(year, "-09-01"))
        school_year <- ifelse(age_date < sept1, year - 1, year)
        grade <- school_year - kindergarten_year
        max(0, min(16, grade))
      }

      grade_to_label <- function(grade) {
        if (grade <= 12) {
          if (grade == 0) return("Kindergarten")
          return(paste("Grade", grade))
        }
        switch(as.character(grade),
               "13"="College Freshman",
               "14"="College Sophomore",
               "15"="College Junior",
               "16"="College Senior")
      }

      get_grade_label <- function(age) {
        date <- birthdate + (age * 365.25)
        month <- as.numeric(format(date, "%m"))
        grade <- calculate_grade(age)

        if (month %in% 6:8) {
          if (grade == 16) return("Summer after College")
          return(paste("Summer before", grade_to_label(min(16, grade + 1))))
        }
        grade_to_label(grade)
      }

      current_age <- as.numeric(difftime(Sys.Date(), birthdate, units = "days")) / 365.25
      base_ages <- seq(8, floor(current_age), by = 0.5)

      data.frame(
        Age = base_ages,
        Date = birthdate + (base_ages * 365.25),
        Label = sapply(base_ages, get_grade_label)
      )
    })

    output$timeline7 <- renderTimevis({
      df <- wlmed_timeline()
      req(nrow(df) > 0)

      timevis(data.frame(
        id = 1:nrow(df),
        start = df$Date,
        content = paste0("Age: ", df$Age, "<br>", df$Label)
      )) %>%
        setOptions(list(editable = FALSE, align = "center"))
    })

    # --- WLMED DATA (only entered ages) ---
    wlmed_data <- reactive({
      ages <- ages_wlmed()
      req(length(ages) > 0)

      data.frame(
        Age = ages,
        Frequency = sapply(ages, function(age) input[[paste0("freq7_", age)]] %||% NA_real_)
      )
    })

    output$behaviorPlot7 <- renderPlotly({
      df <- wlmed_data()
      req(nrow(df) > 0)

      plot_ly(
        df,
        x = ~Age,
        y = ~Frequency,
        type = "scatter",
        mode = "lines+markers",
        line = list(color = "#ef6c45", width = 3),
        marker = list(size = 8, color = "#1a4f66")
      ) %>%
        layout(
          title = list(text = "", font = list(size = 20, color = "#1A4F66")),
          xaxis = list(title = "Age", tickmode = "linear", tick0 = 0, dtick = 0.5),
          yaxis = list(title = "Frequency", tick0 = 0),
          plot_bgcolor = "white",
          paper_bgcolor = "rgba(0,0,0,0)"
        )
    })

    observeEvent(input$submit7, {
      req(shared_data$birthdate, shared_data$graduation_year)
      add_to_combined_data((wlmed_data()), "WLMED")
    })
    # --- EXERCISE TIMELINE (8 -> current age) ---
    exercise_timeline <- reactive({
      req(shared_data$birthdate, shared_data$graduation_year)

      birthdate <- as.Date(shared_data$birthdate)
      kindergarten_year <- shared_data$graduation_year - 13

      calculate_grade <- function(age) {
        age_date <- birthdate + (age * 365.25)
        year <- as.numeric(format(age_date, "%Y"))
        sept1 <- as.Date(paste0(year, "-09-01"))
        school_year <- ifelse(age_date < sept1, year - 1, year)
        grade <- school_year - kindergarten_year
        max(0, min(16, grade))
      }

      grade_to_label <- function(grade) {
        if (grade <= 12) {
          if (grade == 0) return("Kindergarten")
          return(paste("Grade", grade))
        }
        switch(as.character(grade),
               "13"="College Freshman",
               "14"="College Sophomore",
               "15"="College Junior",
               "16"="College Senior")
      }

      get_grade_label <- function(age) {
        date <- birthdate + (age * 365.25)
        month <- as.numeric(format(date, "%m"))
        grade <- calculate_grade(age)

        if (month %in% 6:8) {
          if (grade == 16) return("Summer after College")
          return(paste("Summer before", grade_to_label(min(16, grade + 1))))
        }
        grade_to_label(grade)
      }

      current_age <- as.numeric(difftime(Sys.Date(), birthdate, units = "days")) / 365.25
      base_ages <- seq(8, floor(current_age), by = 0.5)

      data.frame(
        Age = base_ages,
        Date = birthdate + (base_ages * 365.25),
        Label = sapply(base_ages, get_grade_label)
      )
    })

    output$timeline8 <- renderTimevis({
      df <- exercise_timeline()
      req(nrow(df) > 0)

      timevis(data.frame(
        id = 1:nrow(df),
        start = df$Date,
        content = paste0("Age: ", df$Age, "<br>", df$Label)
      )) %>%
        setOptions(list(editable = FALSE, align = "center"))
    })

    # --- EXERCISE DATA (only entered ages) ---
    exercise_data <- reactive({
      ages <- ages_exercise()
      req(length(ages) > 0)

      data.frame(
        Age = ages,
        Frequency = sapply(ages, function(age) input[[paste0("freq8_", age)]] %||% NA_real_)
      )
    })

    output$behaviorPlot8 <- renderPlotly({
      df <- exercise_data()
      req(nrow(df) > 0)

      plot_ly(
        df,
        x = ~Age,
        y = ~Frequency,
        type = "scatter",
        mode = "lines+markers",
        line = list(color = "#ef6c45", width = 3),
        marker = list(size = 8, color = "#1a4f66")
      ) %>%
        layout(
          title = list(text = "", font = list(size = 20, color = "#1A4F66")),
          xaxis = list(title = "Age", tickmode = "linear", tick0 = 0, dtick = 0.5),
          yaxis = list(title = "Frequency", tick0 = 0),
          plot_bgcolor = "white",
          paper_bgcolor = "rgba(0,0,0,0)"
        )
    })

    observeEvent(input$submit8, {
      req(shared_data$birthdate, shared_data$graduation_year)
      add_to_combined_data((exercise_data()), "Exercise")
    })
    # --- BINGE TIMELINE (8 -> current age) ---
    binge_timeline <- reactive({
      req(shared_data$birthdate, shared_data$graduation_year)

      birthdate <- as.Date(shared_data$birthdate)
      kindergarten_year <- shared_data$graduation_year - 13

      calculate_grade <- function(age) {
        age_date <- birthdate + (age * 365.25)
        year <- as.numeric(format(age_date, "%Y"))
        sept1 <- as.Date(paste0(year, "-09-01"))
        school_year <- ifelse(age_date < sept1, year - 1, year)
        grade <- school_year - kindergarten_year
        max(0, min(16, grade))
      }

      grade_to_label <- function(grade) {
        if (grade <= 12) {
          if (grade == 0) return("Kindergarten")
          return(paste("Grade", grade))
        }
        switch(as.character(grade),
               "13"="College Freshman",
               "14"="College Sophomore",
               "15"="College Junior",
               "16"="College Senior")
      }

      get_grade_label <- function(age) {
        date <- birthdate + (age * 365.25)
        month <- as.numeric(format(date, "%m"))
        grade <- calculate_grade(age)

        if (month %in% 6:8) {
          if (grade == 16) return("Summer after College")
          return(paste("Summer before", grade_to_label(min(16, grade + 1))))
        }
        grade_to_label(grade)
      }

      current_age <- as.numeric(difftime(Sys.Date(), birthdate, units = "days")) / 365.25
      base_ages <- seq(8, floor(current_age), by = 0.5)

      data.frame(
        Age = base_ages,
        Date = birthdate + (base_ages * 365.25),
        Label = sapply(base_ages, get_grade_label)
      )
    })

    output$timeline3 <- renderTimevis({
      df <- binge_timeline()
      req(nrow(df) > 0)

      timevis(data.frame(
        id = 1:nrow(df),
        start = df$Date,
        content = paste0("Age: ", df$Age, "<br>", df$Label)
      )) %>%
        setOptions(list(editable = FALSE, align = "center"))
    })

    # --- BINGE DATA (only entered ages) ---
    binge_data <- reactive({
      ages <- ages_binge()
      req(length(ages) > 0)

      data.frame(
        Age = ages,
        Frequency = sapply(ages, function(age) input[[paste0("freq3_", age)]] %||% NA_real_)
      )
    })

    output$behaviorPlot3 <- renderPlotly({
      df <- binge_data()
      req(nrow(df) > 0)

      plot_ly(
        df,
        x = ~Age,
        y = ~Frequency,
        type = "scatter",
        mode = "lines+markers",
        line = list(color = "#ef6c45", width = 3),
        marker = list(size = 8, color = "#1a4f66")
      ) %>%
        layout(
          title = list(text = "", font = list(size = 20, color = "#1A4F66")),
          xaxis = list(title = "Age", tickmode = "linear", tick0 = 0, dtick = 0.5),
          yaxis = list(title = "Frequency", tick0 = 0),
          plot_bgcolor = "white",
          paper_bgcolor = "rgba(0,0,0,0)"
        )
    })

    observeEvent(input$submit3, {
      req(shared_data$birthdate, shared_data$graduation_year)
      add_to_combined_data((binge_data()), "Binge")
    })


  # Weight Chart Tab: Excel Data & Additional Markers with Interactive Tooltip and Legend #####
  weight_data <- reactive({
    req(input$weightFile)
    tryCatch({
      read_excel(input$weightFile$datapath)
    }, error = function(e) {
      showNotification("Error reading Excel file. Please ensure it has the correct format.", type = "error")
      return(NULL)
    })
  })


  point_exists <- function(age_val, weight_val, data) {
    tol_age <- 0.1
    tol_weight <- 0.1
    any(abs(data$Age - age_val) < tol_age & abs(data$Weight - weight_val) < tol_weight)
  }

  output$weightChartPlot <- renderPlotly({

    req(weight_data())
    df <- weight_data()
    req(df)

    if (!all(c("Age","Weight","Height") %in% names(df))) {
      showNotification("Data must include columns: Age, Weight, Height", type = "error")
      return(NULL)
    }

    df <- df %>%
      filter(!is.na(Age) & !is.na(Weight) & !is.na(Height)) %>%
      mutate(
        agemos = Age * 12,
        L = get_L(agemos),
        M = get_M(agemos),
        S = get_S(agemos),
        BMI = Weight / ((Height / 100)^2),
        BMIz = ((BMI / M)^L - 1) / (L * S)
      )

    # ----- Puberty marker -----
    puberty_age <- ifelse(!is.na(input$firstPeriodAge),
                          input$firstPeriodAge + 1,
                          NA)

    # ----- Pre-puberty subset for BMIz shading -----
    df_prepub <- df
    if (!is.na(puberty_age)) {
      df_prepub <- df %>% filter(Age < puberty_age)
    }

    # ----- Identify BMIz extremes (pre-puberty only) -----
    bmi_shading <- df_prepub %>%
      mutate(
        BMIz_group = case_when(
          BMIz <= quantile(BMIz, 0.1, na.rm = TRUE) ~ "Lowest BMIz",
          BMIz >= quantile(BMIz, 0.9, na.rm = TRUE) ~ "Highest BMIz",
          TRUE ~ NA_character_
        )
      ) %>%
      filter(!is.na(BMIz_group)) %>%
      group_by(BMIz_group) %>%
      summarize(
        xmin = min(Age),
        xmax = max(Age),
        .groups = "drop"
      )

    # ----- Behavior ranges from UI -----
    behavior_ranges <- data.frame(
      Behavior = c("Restriction","Fasting","Vomit","Laxative","Diuretic","WLMED","Exercise","Binge"),
      xmin = c(input$age_start1, input$age_start2, input$age_start4, input$age_start5,
               input$age_start6, input$age_start7, input$age_start8, input$age_start3),
      xmax = c(input$age_end1, input$age_end2, input$age_end4, input$age_end5,
               input$age_end6, input$age_end7, input$age_end8, input$age_end3),
      stringsAsFactors = FALSE
    )

    purge_related <- subset(behavior_ranges,
                            Behavior %in% c("Vomit","Laxative","Diuretic","WLMED"))

    if (nrow(purge_related) > 0) {
      combined_purge <- data.frame(
        Behavior = "Purge",
        xmin = min(purge_related$xmin, na.rm = TRUE),
        xmax = max(purge_related$xmax, na.rm = TRUE)
      )

      behavior_ranges <- behavior_ranges %>%
        filter(!Behavior %in% c("Vomit","Laxative","Diuretic","WLMED")) %>%
        bind_rows(combined_purge)
    }

    valid_ranges <- behavior_ranges %>%
      filter(!is.na(xmin) & !is.na(xmax) & xmin < xmax)

    # ----- Base plot -----
    p <- ggplot(df, aes(x = Age, y = Weight)) +
      geom_line(aes(color = "Weight Trend"), size = 1) +
      geom_point(aes(color = "Weight Data"), size = 3)

    # ----- Behavior shading -----
    if (nrow(valid_ranges) > 0) {

      yr <- range(df$Weight, na.rm = TRUE)
      pad <- diff(yr) * 0.03

      rect_data <- valid_ranges %>%
        mutate(ymin = yr[1] - pad, ymax = yr[2] + pad)

      p <- p +
        geom_rect(
          data = rect_data,
          aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = Behavior),
          inherit.aes = FALSE,
          alpha = 0.25
        ) +
        scale_fill_manual(values = c(
          "Restriction" = "#ef6c45",
          "Fasting"     = "#56b4e9",
          "Binge"       = "#f1c40f",
          "Purge"       = "#2ca02c",
          "Exercise"    = "#e7298a",
          "Lowest BMIz" = "#6baed6",
          "Highest BMIz"= "#fb6a4a"
        ))
    }

    # ----- BMIz shading (pre-puberty only) -----
    if (nrow(bmi_shading) > 0) {

      yr <- range(df$Weight, na.rm = TRUE)
      pad <- diff(yr) * 0.03

      bmi_rects <- bmi_shading %>%
        mutate(ymin = yr[1] - pad, ymax = yr[2] + pad)

      p <- p +
        geom_rect(
          data = bmi_rects,
          aes(
            xmin = xmin,
            xmax = xmax,
            ymin = ymin,
            ymax = ymax,
            fill = BMIz_group
          ),
          inherit.aes = FALSE,
          alpha = 0.18
        )
    }

    # ----- Puberty marker -----
    if (!is.na(puberty_age)) {
      p <- p +
        geom_vline(xintercept = puberty_age,
                   color = "red", linetype = "dashed", size = 1) +
        annotate(
          "text",
          x = puberty_age + 0.1,
          y = max(df$Weight, na.rm = TRUE),
          label = "Puberty Marker",
          angle = 90,
          vjust = -0.5,
          color = "red"
        )
    }

    # ----- Hallmark points -----
    hallmark_df <- data.frame(
      Age = c(input$preLowAge, input$preHighAge,
              input$postLowAge, input$postHighAge),
      Weight = c(input$preLowWeight, input$preHighWeight,
                 input$postLowWeight, input$postHighWeight)
    ) %>% filter(!is.na(Age) & !is.na(Weight))

    p <- p +
      geom_point(
        data = hallmark_df,
        aes(x = Age, y = Weight, color = "Hallmark Weights"),
        size = 6, shape = 21, fill = "lightblue"
      )

    # ----- Axes & theme -----
    p <- p +
      scale_x_continuous(limits = c(7.5, 20.5),
                         breaks = seq(8, 20, by = 2)) +
      labs(x = "Age", y = "Weight", title = "Weight Over Time") +
      theme_minimal(base_size = 14) +
      theme(
        legend.position = "bottom",
        legend.title = element_blank()
      ) +
      scale_color_manual(values = c(
        "Weight Trend" = "#2c3e50",
        "Weight Data" = "#e74c3c",
        "Hallmark Weights" = "blue"
      ))

    # ----- Convert to Plotly -----
    gp <- ggplotly(p, tooltip = c("x","y","colour","fill"))
    gp$x$layout$legend$title$text <- NULL

    # ----- Legend cleanup -----
    wanted <- c(
      "Restriction","Fasting","Purge","Exercise","Binge",
      "Lowest BMIz","Highest BMIz",
      "Weight Trend","Weight Data","Hallmark Weights"
    )

    seen <- character(0)

    for (i in seq_along(gp$x$data)) {
      nm <- gp$x$data[[i]]$name
      if (!is.null(nm)) {
        nm <- sub("^\\((.*)\\)$", "\\1", nm)
        gp$x$data[[i]]$name <- nm
        gp$x$data[[i]]$legendgroup <- nm
        gp$x$data[[i]]$showlegend <- nm %in% wanted && !(nm %in% seen)
        if (nm %in% wanted) seen <- c(seen, nm)
      }
    }

    gp
  })


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
      "age_adult_height" = list(textInput("age_adult_height_column", "Name of the age at adult height column:", value = get_default_value("age_adult_height_column", "age_adult_height"))),
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

      if (input$data_type == "one" && input$data_source != "demo") {
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
        age_adult_ht_col_name = if (!is.null(input$age_adult_height_column)) input$age_adult_height_column else NULL,
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
