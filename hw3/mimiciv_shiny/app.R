library(shiny)
library(dplyr)
library(ggplot2)

icu_cohort = readRDS('icu_cohort.rds')
ui <- fluidPage(
    titlePanel("ICU Cohort Data"),
    sidebarPanel(selectInput("variable", "Choose a variable:",
                             choices = c("First Care Unit", 
                                         "Last Care Unit", 
                                         "In Time",
                                         'Out Time',
                                         'Length of Stay',
                                         'Admission Time',
                                         'Discharge Time',
                                         'Death Time',
                                         'Admission Type',
                                         'Admission Location',
                                         'Discharge Location',
                                         'Insurance',
                                         'Language',
                                         'Martial Status',
                                         'Ethnicity',
                                         'Edregtime',
                                         'Edouttime',
                                         'Hospital Expired Flag',
                                         'Gender',
                                         'Anchor Age',
                                         'Anchor Year',
                                         'Anchor Year Group',
                                         'DOD',
                                         'Age at Admission',
                                         'Bicarbonate',
                                         'Calcium',
                                         'Chloride',
                                         'Creatinine',
                                         'Glucose',
                                         'Magnesium',
                                         'Potassium',
                                         'Sodium',
                                         'Hematocrite',
                                         'WBC',
                                         'Lactate',
                                         'Heart rate',
                                         'Non-invasive Blood Pressure Systolic',
                                         'Non-invasive Blood Pressure Mean',
                                         'Respiratory Rate',
                                         'Temperature Fahrenheit',
                                         'Arterial Blood Pressure Systolic',
                                         'Arterial Blood Pressure Mean',
                                         'Dead in 30 days'))),
    sliderInput("binnum", label = "Bin Number:", min = 1, max = 100, value = 20),
    textOutput('sum'),
    plotOutput('hist'))

server <- function(input, output){
    output$sum = renderPrint({
        datasetInput <- switch(input$variable,
                               "First Care Unit" = icu_cohort$first_careunit, 
                               "Last Care Unit" = icu_cohort$last_careunit, 
                               "In Time" = icu_cohort$intime,
                               'Out Time' = icu_cohort$outtime,
                               'Length of Stay' = icu_cohort$los,
                               'Admission Time' = icu_cohort$admittime,
                               'Discharge Time' = icu_cohort$dischtime,
                               'Death Time' = icu_cohort$deathtime,
                               'Admission Type' = icu_cohort$admission_type,
                               'Admission Location' = 
                                   icu_cohort$admission_location,
                               'Discharge Location' = 
                                   icu_cohort$discharge_location,
                               'Insurance' = icu_cohort$insurance,
                               'Language' = icu_cohort$language,
                               'Martial Status' = icu_cohort$marital_status,
                               'Ethnicity' = icu_cohort$ethnicity,
                               'Edregtime' = icu_cohort$edregtime,
                               'Edouttime' = icu_cohort$edouttime,
                               'Hospital Expired Flag' = 
                                   icu_cohort$hospital_expire_flag,
                               'Gender' = icu_cohort$gender,
                               'Anchor Age' = icu_cohort$anchor_age,
                               'Anchor Year' = icu_cohort$anchor_year,
                               'Anchor Year Group' = 
                                   icu_cohort$anchor_year_group,
                               'DOD' = icu_cohort$dod,
                               'Age at Admission' = icu_cohort$age_at_adm,
                               'Bicarbonate' = icu_cohort$bicarbonate,
                               'Calcium' = icu_cohort$calcium,
                               'Chloride' = icu_cohort$chloride,
                               'Creatinine' = icu_cohort$creatinine,
                               'Glucose' = icu_cohort$glucose,
                               'Magnesium' = icu_cohort$magnesium,
                               'Potassium' = icu_cohort$potassium,
                               'Sodium' = icu_cohort$sodium,
                               'Hematocrite' = icu_cohort$hematocrit,
                               'WBC' = icu_cohort$wbc,
                               'Lactate' = icu_cohort$lactate,
                               'Heart rate' = icu_cohort$heart_rate,
                               'Non-invasive Blood Pressure Systolic' = 
                                   icu_cohort$non_invasive_blood_pressure_systolic,
                               'Non-invasive Blood Pressure Mean' = 
                                   icu_cohort$non_invasive_blood_pressure_mean,
                               'Respiratory Rate' = icu_cohort$respiratory_rate,
                               'Temperature Fahrenheit' = 
                                   icu_cohort$temperature_fahrenheit,
                               'Arterial Blood Pressure Systolic' = 
                                   icu_cohort$arterial_blood_pressure_systolic,
                               'Arterial Blood Pressure Mean' = 
                                   icu_cohort$arterial_blood_pressure_mean,
                               'Dead in 30 days' = icu_cohort$de30)
        summary(datasetInput%>%as.data.frame())})
    output$hist = renderPlot({
        datasetInput <- switch(input$variable,
                               "First Care Unit" = icu_cohort$first_careunit, 
                               "Last Care Unit" = icu_cohort$last_careunit, 
                               "In Time" = icu_cohort$intime,
                               'Out Time' = icu_cohort$outtime,
                               'Length of Stay' = icu_cohort$los,
                               'Admission Time' = icu_cohort$admittime,
                               'Discharge Time' = icu_cohort$dischtime,
                               'Death Time' = icu_cohort$deathtime,
                               'Admission Type' = icu_cohort$admission_type,
                               'Admission Location' = 
                                   icu_cohort$admission_location,
                               'Discharge Location' = 
                                   icu_cohort$discharge_location,
                               'Insurance' = icu_cohort$insurance,
                               'Language' = icu_cohort$language,
                               'Martial Status' = icu_cohort$marital_status,
                               'Ethnicity' = icu_cohort$ethnicity,
                               'Edregtime' = icu_cohort$edregtime,
                               'Edouttime' = icu_cohort$edouttime,
                               'Hospital Expired Flag' = 
                                   icu_cohort$hospital_expire_flag,
                               'Gender' = icu_cohort$gender,
                               'Anchor Age' = icu_cohort$anchor_age,
                               'Anchor Year' = icu_cohort$anchor_year,
                               'Anchor Year Group' = 
                                   icu_cohort$anchor_year_group,
                               'DOD' = icu_cohort$dod,
                               'Age at Admission' = icu_cohort$age_at_adm,
                               'Bicarbonate' = icu_cohort$bicarbonate,
                               'Calcium' = icu_cohort$calcium,
                               'Chloride' = icu_cohort$chloride,
                               'Creatinine' = icu_cohort$creatinine,
                               '. Length:50048 Class :character Mode :character
Glucose' = icu_cohort$glucose,
                               'Magnesium' = icu_cohort$magnesium,
                               'Potassium' = icu_cohort$potassium,
                               'Sodium' = icu_cohort$sodium,
                               'Hematocrite' = icu_cohort$hematocrit,
                               'WBC' = icu_cohort$wbc,
                               'Lactate' = icu_cohort$lactate,
                               'Heart rate' = icu_cohort$heart_rate,
                               'Non-invasive Blood Pressure Systolic' = 
                                   icu_cohort$non_invasive_blood_pressure_systolic,
                               'Non-invasive Blood Pressure Mean' = 
                                   icu_cohort$non_invasive_blood_pressure_mean,
                               'Respiratory Rate' = icu_cohort$respiratory_rate,
                               'Temperature Fahrenheit' = 
                                   icu_cohort$temperature_fahrenheit,
                               'Arterial Blood Pressure Systolic' = 
                                   icu_cohort$arterial_blood_pressure_systolic,
                               'Arterial Blood Pressure Mean' = 
                                   icu_cohort$arterial_blood_pressure_mean,
                               'Dead in 30 days' = icu_cohort$de30)
        if (is.numeric(datasetInput)){
            hist(datasetInput, breaks = input$binnum, main=input$variable,
                 xlab =input$variable)
            }
        if(is.character(datasetInput)){
            ggplot() +
                geom_bar(mapping = aes(x = datasetInput),) +
                labs(x = input$variable) +
                coord_flip()
            }
    })
    }

# Run the application 
shinyApp(ui = ui, server = server)
