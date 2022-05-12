#' Roxygen style comments
#' This structure supports later package documentation

# Load Libraries
library(shiny)
library(tidyverse)
library(ggplot2)
library(openxlsx)
library(lubridate)
library(shinythemes)
library(AICcmodavg)



# Code Chunk

# Fix Date Function

fix_date <- function(dataframe) {

  dataframe$Date <- as.Date(as.numeric(dataframe$Date), "1899-12-30")

  return(dataframe)
}


# Load Data
# Population

# Population by Race
# Load Data

pop_race <- readr::read_csv("../data-raw/DC Population by Ward by Race - Census 2020.csv")

# Clean Data
pop_race_pivot <- pop_race %>%
                    pivot_longer(-Ward,
                                 names_to = "Race",
                                 values_to = "Count") %>%
                    mutate(Race = str_replace_all(Race, "\\.", " "))

# Write Population by Race data to file
utils::write.csv(pop_race_pivot, "../data/DC Population by Ward by Race - Census 2020.csv",
                 row.names = FALSE)



# Population by Ethnicity
# Load Data
pop_ethn <- readr::read_csv("../data-raw/DC Population by Ward by Ethnicity - Census 2020.csv")

# Clean Data
pop_ethn_pivot <- pop_ethn %>%
                    pivot_longer(-Ward,
                                 names_to = "Ethnicity",
                                 values_to = "Count") %>%
                    mutate(Ethnicity = str_replace_all(Ethnicity, "\\.", " "))


# Write Population by Ethnicity data to file
utils::write.csv(pop_ethn_pivot, "../data/DC Population by Ward by Ethnicity - Census 2020.csv",
                 row.names = FALSE)



# By Ward

# Lives Lost by Ward
# Load Data
lives_lost_ward <- openxlsx::read.xlsx("../data-raw/DC-COVID-19-Lives Lost by Ward.xlsx")

# Clean Data
lives_lost_ward_pivot <- lives_lost_ward %>%
  pivot_longer(-Ward,
               names_to = "Date",
               values_to = "Lives Lost")


lives_lost_ward_pivot <- fix_date(lives_lost_ward_pivot) %>%
      filter(Date >= "2020-04-19" & Date <= "2021-12-02") %>%
      pivot_wider(names_from = Ward,
                  values_from = `Lives Lost`) %>%
      # merged Unknown and Experienced Homelessness groups
      mutate(Unknown = `Unknown` + `Experienced Homelessness`) %>%
      select(-`Experienced Homelessness`) %>%
      pivot_longer(-Date,
               names_to = "Ward",
               values_to = "Lives Lost") %>%
      select(Ward, Date, `Lives Lost`)

# Write Lives Lost by Ward data to file
openxlsx::write.xlsx(lives_lost_ward_pivot, "../data/DC-COVID-19-Lives_Lost_Ward.xlsx", overwrite = TRUE)


# Total Cases by Ward
# Load Data
total_cases_ward <- openxlsx::read.xlsx("../data-raw/DC-COVID-19-Total Cases by Ward.xlsx")

# Clean Data
total_cases_ward_pivot <- total_cases_ward %>%
  pivot_longer(-Ward,
               names_to = "Date",
               values_to = "Total Cases")

total_cases_ward_pivot <- fix_date(total_cases_ward_pivot) %>%
      filter(Date >= "2020-04-19" & Date <= "2021-12-02") %>%
      pivot_wider(names_from = Ward,
                  values_from = `Total Cases`) %>%
      mutate(All = select(., `Ward 1`:Unknown) %>% rowSums()) %>%
      pivot_longer(-Date,
                    names_to = "Ward",
                    values_to = "Total Cases") %>%
      select(Ward, Date, `Total Cases`)

# Write Lives Lost by Ward data to file
openxlsx::write.xlsx(total_cases_ward_pivot, "../data/DC-COVID-19-Total_Cases_Ward.xlsx", overwrite = TRUE)


# DC Covid 19 by Ward
# Join Data
dc_covid19_ward <- lives_lost_ward_pivot %>%
  full_join(total_cases_ward_pivot,
            by = c("Ward", "Date")) %>%
  mutate(`Lives Lost` = replace_na(`Lives Lost`, 0)) %>%
  arrange(Date, Ward)

# Write Ward data to file
openxlsx::write.xlsx(dc_covid19_ward, "../data/DC-COVID-19-Ward.xlsx", overwrite = TRUE)

# By Race

# Lives Lost by Race
# Load Data
lives_lost_race <- read.xlsx("../data-raw/DC-COVID-19-Lives Lost by Race.xlsx")

# Clean Data
lives_lost_race_pivot <- lives_lost_race %>%
  pivot_longer(-Race,
               names_to = "Date",
               values_to = "Lives Lost")

lives_lost_race_pivot <- fix_date(lives_lost_race_pivot) %>%
  filter(Date >= "2020-04-19" & Date <= "2021-12-02")

# Write Lives Lost by Race data to file
openxlsx::write.xlsx(lives_lost_race_pivot, "../data/DC-COVID-19-Lives_Lost_Race.xlsx", overwrite = TRUE)


# By Ethnicity

# Lives Lost by Ethnicity
# Load Data
lives_lost_ethn <- read.xlsx("../data-raw/DC-COVID-19-Lives Lost by Ethnicity.xlsx")

# Clean Data
lives_lost_ethn_pivot <- lives_lost_ethn %>%
  pivot_longer(-Ethnicity,
               names_to = "Date",
               values_to = "Lives Lost")

lives_lost_ethn_pivot <- fix_date(lives_lost_ethn_pivot) %>%
  filter(Date >= "2020-04-19" & Date <= "2021-12-02") %>%
  select(Ethnicity, Date, `Lives Lost`)

# Write Lives Lost by Ethnicity data to file
openxlsx::write.xlsx(lives_lost_ethn_pivot, "../data/DC-COVID-19-Lives_Lost_Ethnicity.xlsx", overwrite = TRUE)


# By Sex

# Lives Lost by Sex
# Load Data
lives_lost_sex <- openxlsx::read.xlsx("../data-raw/DC-COVID-19-Lives Lost by Sex.xlsx")

# Clean Data
lives_lost_sex_pivot <- lives_lost_sex %>%
  pivot_longer(-Sex,
               names_to = "Date",
               values_to = "Lives Lost")

lives_lost_sex_pivot <- fix_date(lives_lost_sex_pivot) %>%
  filter(Date >= "2020-04-19" & Date <= "2021-12-02") %>%
  select(Sex, Date, `Lives Lost`)


# Write Lives Lost by Sex data to file
openxlsx::write.xlsx(lives_lost_sex_pivot,
                     "../data/DC-COVID-19-Lives_Lost_Sex.xlsx",
                      overwrite = TRUE)



# By Age

#Lives Lost by Age
# Load Data
lives_lost_age <- openxlsx::read.xlsx("../data-raw/DC-COVID-19-Lives Lost by Age.xlsx")

# Clean Data
lives_lost_age_pivot <- lives_lost_age %>%
  pivot_longer(-Age,
               names_to = "Date",
               values_to = "Lives Lost")

lives_lost_age_pivot <- fix_date(lives_lost_age_pivot) %>%
  filter(Date >= "2020-04-19" & Date <= "2021-12-02") %>%
  select(Age, Date, `Lives Lost`)

# Write Lives Lost by Age data to file
openxlsx::write.xlsx(lives_lost_age_pivot, "../data/DC-COVID-19-Lives_Lost_Age.xlsx",
           overwrite = TRUE)



#Vaccination in DC

# Load Data
vax_dc <- readr::read_csv("../data-raw/covid19_vaccinations_in_the_united_states-dc.csv")

# Clean Data

# Rename first column name to remove special character
names(vax_dc)[1] <- "Date"

# Clean Data
vax_dc <- vax_dc %>%
            filter(date_type == "Admin") %>%
            mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>%
            filter(Date <= "2021-12-02") %>%
            select(Date, MMWR_week, Location, Administered_Daily,
                   Administered_Cumulative, Admin_Dose_1_Daily,
                   Admin_Dose_1_Cumulative, Series_Complete_Daily,
                   Series_Complete_Cumulative, Booster_Daily,
                   Booster_Cumulative, date_type)
# Write Vaccination data to file
utils::write.csv(vax_dc, "../data/DC-COVID-19-Vaccination.csv",
                 row.names = FALSE)



# DC Vaccination by Ward
# Load Data
dc_vax_ward <- read.xlsx("../data-raw/DC-Vaccination by Ward.xlsx")

# Write DC Vaccination by Ward data to file
openxlsx::write.xlsx(dc_vax_ward, "../data/DC-Vaccination by Ward.xlsx",
                     overwrite = TRUE)


# DC Vaccination by Race
# Load Data
dc_vax_race <- read.xlsx("../data-raw/DC-Vaccination by Race.xlsx")

# Write DC Vaccination by Race data to file
openxlsx::write.xlsx(dc_vax_race, "../data/DC-Vaccination by Race.xlsx",
                     overwrite = TRUE)

# DC Vaccination by Ethnicity
# Load Data
dc_vax_ethn <- read.xlsx("../data-raw/DC-Vaccination by Ethnicity.xlsx")

# Write DC Vaccination by Ethnicity data to file
openxlsx::write.xlsx(dc_vax_ethn, "../data/DC-Vaccination by Ethnicity.xlsx",
                     overwrite = TRUE)


# DC Neighborhood

# Load Data
dc_neighborhood <- readr::read_csv("../data-raw/dc_neighborhood.csv")

# Write DC Neighborhood data to file
utils::write.csv(dc_neighborhood, "../data/dc_neighborhood.csv",
                 row.names = FALSE)

# Load data from data folder
pop_race_pivot <- readr::read_csv("../data/DC Population by Ward by Race - Census 2020.csv")
pop_ethn_pivot <- readr::read_csv("../data/DC Population by Ward by Ethnicity - Census 2020.csv")

lives_lost_ward <- openxlsx::read.xlsx("../data/DC-COVID-19-Lives_Lost_Ward.xlsx")
total_cases_ward <- openxlsx::read.xlsx("../data/DC-COVID-19-Total_Cases_Ward.xlsx")
dc_covid_19_ward <- openxlsx::read.xlsx("../data/DC-COVID-19-Ward.xlsx")


lives_lost_race <- openxlsx::read.xlsx("../data/DC-COVID-19-Lives_Lost_Race.xlsx")

lives_lost_ethn <- openxlsx::read.xlsx("../data/DC-COVID-19-Lives_Lost_Ethnicity.xlsx")
  
lives_lost_sex <- openxlsx::read.xlsx("../data/DC-COVID-19-Lives_Lost_Sex.xlsx")
lives_lost_age <- openxlsx::read.xlsx("../data/DC-COVID-19-Lives_Lost_Age.xlsx")

dc_vax_admin <- readr::read_csv("../data/DC-COVID-19-Vaccination.csv")
dc_vax_ward <- openxlsx::read.xlsx("../data/DC-Vaccination by Ward.xlsx")
dc_vax_race <- openxlsx::read.xlsx("../data/DC-Vaccination by Race.xlsx")
dc_vax_ethn <- openxlsx::read.xlsx("../data/DC-Vaccination by Ethnicity.xlsx")

dc_neighborhood <- readr::read_csv("../data/dc_neighborhood.csv")


ui <- fluidPage(
  theme = shinytheme("yeti"),
  titlePanel("COVID-19 in DC"),
  h4("by Yuxi Duan"),
  h5("The data was collected from the DC Government Coronavirus Website,
  , the DC Government Office of Planning Website, and the CDC"),
  dataTableOutput("pop_race_pivot"),


  tabsetPanel( type = "tabs",
   
   
    tabPanel("DC Demographic",
     sidebarLayout(
             sidebarPanel(
               img(src = "dc_ward_map.png", height = 200, width = 200),
               h4("DC Neighborhood"),
               tableOutput("DCNeighborhood1")
             ), # end of sidebarPanel
             mainPanel(
               h3("Population by Race"),
               tableOutput("PopRace"),
               h3("Population by Ethnicity"),
               tableOutput("PopEthn")
             ) # end of mainPanel
     ) # end of sidebarLayout
    ), # tabPanel #1
    tabPanel("Vaccine Inequality by Ward",
      sidebarLayout(

          sidebarPanel(
            img(src = "dc_ward_map.png", height = 200, width = 200),
            h4("DC Neighborhood"),
            tableOutput("DCNeighborhood2")
          ), # end of sidebarPanel
          mainPanel(
            h3("Vaccination by Ward"),
            selectInput("wardVaxCat", "Select Vaccination Status",
                         unique(dc_vax_ward$Vaccination.Status),
                         selected = "Fully Vaccinated"),
            tableOutput("VaccinationWard"),
            h3("Vaccination by Race"),
            selectInput("raceVaxCat", "Select Vaccination Status",
                        unique(dc_vax_race$Vaccination.Status),
                        selected = "Fully Vaccinated"),
            tableOutput("VaccinationRace"),
            h3("Vaccination by Ethnicity"),
            selectInput("ethnVaxCat", "Select Vaccination Status",
                        unique(dc_vax_ethn$Vaccination.Status),
                        selected = "Fully Vaccinated"),
            tableOutput("VaccinationEthn")
          ) # end of mainPanel
      ) # end of sidebarLayout
    ), # end of tabPanel #2


    tabPanel("Total Cases and Lives Lost",
      sidebarLayout(
        sidebarPanel(
          dateInput("date",
                    h3("Date Input"),
                    value = "2021-12-02",
                    min = "2021-04-19",
                    max = "2021-12-02"),
          checkboxGroupInput("dataCheckBox",
                             "Select dataset(s)",
                             choices = list("Total Cases by Ward" = "TCWard",
                                            "Lives Lost by Ward" = "LLWard",
                                            "Lives Lost by Race" = "LLRace",
                                            "Lives Lost by Ethnicity" = "LLEthn",
                                            "Lives Lost by Sex" = "LLSex",
                                            "Lives Lost by Age" = "LLAge"),
                             selected = "TCWard")
      
    
        ), # end of sidebarPanel
        mainPanel(
          uiOutput("TotalWardTitle"),
          tableOutput("TotalWard"),
          uiOutput("LostWardTitle"),
          tableOutput("LostWard"),
          uiOutput("LostRaceTitle"),
          tableOutput("LostRace"),
          uiOutput("LostEthnTitle"),
          tableOutput("LostEthn"),
          uiOutput("LostSexTitle"),
          tableOutput("LostSex"),
          uiOutput("LostAgeTitle"),
          tableOutput("LostAge")
      
      
        ) # end of mainPanel
      ) # end of sidebarLayout
       
       
       
    ), # end of tabPanel #3


    tabPanel("Plot Outputs",
             sidebarLayout(
               sidebarPanel(
                 dateRangeInput("dateRange", "Date Range",
                                start = "2020-04-19",
                                end = "2021-12-02",
                                min = "2020-04-19",
                                max = "2021-12-02"),
             
                 radioButtons("dataRadioButton",
                                    "Select dataset(s)",
                                    choices = list("Total Cases by Ward" = "TCWardPlot",
                                                   "Lives Lost by Ward" = "LLWardPlot",
                                                   "Lives Lost by Race" = "LLRacePlot",
                                                   "Lives Lost by Ethnicity" = "LLEthnPlot",
                                                   "Lives Lost by Sex" = "LLSexPlot",
                                                   "Lives Lost by Age" = "LLAgePlot"),
                                    selected = "TCWardPlot"),
               ), # end sidebarPanel
               mainPanel(plotOutput("plot1"))
            ) # end of sidebarLayout
    ), # endof tabPanel #4



    tabPanel("Statistical Model",
             
               mainPanel(tableOutput("vaxdata"),
                         h4("dc_vax_admin.model_total <- lm(Booster_Daily~ .-Date, data=dc_vax_admin)"),
                         h4("dc_vax_admin.model <- lm(Booster_Daily~ Admin_Dose_1_Daily + Series_Complete_Daily+ MMWR_week, data=dc_vax_admin)"),
                         verbatimTextOutput("AIC"),
                         verbatimTextOutput("statmodel")
               )
             
    ) # end of tabPanel # 5


  ) # end tabsetPanel



) # end fluidPlage



server <- function(input, output, session) {
  # DC Demographics
  output$DCNeighborhood1 <- renderTable({
    dc_neighborhood
  })

  output$PopRace <- renderTable({
    pop_race
  })

  output$PopEthn <- renderTable({
    pop_ethn
  })


  # Vaccine Inequality by Ward

  output$DCNeighborhood2 <- renderTable({
    dc_neighborhood
  })


  output$VaccinationWard <- renderTable({
    subset(dc_vax_ward[, 1:10], Vaccination.Status == input$wardVaxCat)
   })

  output$VaccinationRace <- renderTable({
    subset(dc_vax_race[, 1:12], Vaccination.Status == input$raceVaxCat)
  })

  output$VaccinationEthn <- renderTable({
    subset(dc_vax_ethn[, 1:12], Vaccination.Status == input$ethnVaxCat)
  })

  # Total Cases and Lives Lost


  # Total Cases by Ward
  output$TotalWardTitle <- renderUI({
    if ("TCWard" %in% input$dataCheckBox) {
      h3("Total Cases by Ward")
    }
  })

  output$TotalWard <- renderTable({
    if ("TCWard" %in% input$dataCheckBox) {

      fix_date(total_cases_ward) %>%
        filter(Date == input$date) %>%
        select(-Date)
    }
  })

  # Lives Lost by Ward
  output$LostWardTitle <- renderUI({
    if ("LLWard" %in% input$dataCheckBox) {
      h3("Lives Lost by Ward")
    }
  })

  output$LostWard <- renderTable({
    if ("LLWard" %in% input$dataCheckBox) {
      fix_date(lives_lost_ward) %>%
        filter(Date == input$date) %>%
        select(-Date)
    }
  })

  # Lives Lost by Race
  output$LostRaceTitle <- renderUI({
    if ("LLRace" %in% input$dataCheckBox) {
      h3("Lives Lost by Race")
    }
  })

  output$LostRace <- renderTable({
    if ("LLRace" %in% input$dataCheckBox) {
      fix_date(lives_lost_race) %>%
        filter(Date == input$date) %>%
        select(-Date)
    }
  })

  # Lives Lost by Ethnicity
  output$LostEthnTitle <- renderUI({
    if ("LLEthn" %in% input$dataCheckBox) {
      h3("Lives Lost by Ethnicity")
    }
  })

  output$LostEthn <- renderTable({
    if ("LLEthn" %in% input$dataCheckBox) {
      fix_date(lives_lost_ethn) %>%
        filter(Date == input$date) %>%
        select(-Date)
    }
  })

  # Lives Lost by Sex
  output$LostSexTitle <- renderUI({
    if ("LLSex" %in% input$dataCheckBox) {
      h3("Lives Lost by Sex")
    }
  })

  output$LostSex <- renderTable({
    if ("LLSex" %in% input$dataCheckBox) {
      fix_date(lives_lost_sex) %>%
        filter(Date == input$date) %>%
        select(-Date)
    }
  })

    # Lives Lost by Age
  output$LostAgeTitle <- renderUI({
    if ("LLAge" %in% input$dataCheckBox) {
      h3("Lives Lost by Age")
    }
  })

  output$LostAge <- renderTable({
    if ("LLAge" %in% input$dataCheckBox) {
      fix_date(lives_lost_age) %>%
        filter(Date == input$date) %>%
        select(-Date)
    }
  })




  # Cases and Lives Lost

  output$plot1 <- renderPlot({
    # Total Cases by Ward
    if (input$dataRadioButton == "TCWardPlot") {
      fix_date(total_cases_ward) %>%
        filter(str_detect(Ward, "Ward")) %>%
        filter(Date >= input$dateRange[1] & Date <= input$dateRange[2]) %>%
        ggplot(aes(x = Date, y = `Total.Cases`,  color = Ward)) +
        geom_line()
    }

    # Lives Lost by Ward
    else if (input$dataRadioButton == "LLWardPlot") {
      fix_date(lives_lost_ward) %>%
        filter(str_detect(Ward, "Ward")) %>%
        filter(Date >= input$dateRange[1] & Date <= input$dateRange[2]) %>%
        ggplot(aes(x = Date, y = `Lives.Lost`,  color = Ward)) +
        geom_line()
    }

    # Lives Lost by Race
    else if (input$dataRadioButton == "LLRacePlot") {
      fix_date(lives_lost_race) %>%
        filter(Race != "All") %>%
        filter(Date >= input$dateRange[1] & Date <= input$dateRange[2]) %>%
        ggplot(aes(x = Date, y = `Lives.Lost`,  color = Race)) +
        geom_line()
    }

    # Lives Lost by Ethnicity
    else if (input$dataRadioButton == "LLEthnPlot") {
      fix_date(lives_lost_ethn) %>%
        filter(Date >= input$dateRange[1] & Date <= input$dateRange[2]) %>%
        ggplot(aes(x = Date, y = `Lives.Lost`,  color = Ethnicity)) +
        geom_line()
    }

    # Lives Lost by Sex
    else if (input$dataRadioButton == "LLSexPlot") {
      fix_date(lives_lost_sex) %>%
        filter(Sex != "All") %>%
        filter(Date >= input$dateRange[1] & Date <= input$dateRange[2]) %>%
        ggplot(aes(x = Date, y = `Lives.Lost`,  color = Sex)) +
        geom_line()
    }

    # Lives Lost by Age
    else if (input$dataRadioButton == "LLAgePlot") {
      fix_date(lives_lost_age) %>%
        filter(Age != "All") %>%
        filter(Date >= input$dateRange[1] & Date <= input$dateRange[2]) %>%
        ggplot(aes(x = Date, y = `Lives.Lost`,  color = Age)) +
        geom_line()
    }

  })
  
  # Statistical Model
  dc_vax_admin<- dc_vax_admin[,-3]
  dc_vax_admin<- dc_vax_admin[,-11]
  dc_vax_admin <- as.data.frame(dc_vax_admin)
  
  output$vaxdata <- renderTable({
    dc_vax_admin <- fix(dc_vax_admin)
    head(dc_vax_admin)
  })
  
  dc_vax_admin.model_total <- lm(Booster_Daily~ .-Date, data=dc_vax_admin)
  dc_vax_admin.model <- lm(Booster_Daily~ Admin_Dose_1_Daily + Series_Complete_Daily+ MMWR_week, data=dc_vax_admin)
  #define list of models
  models <- list(dc_vax_admin.model_total, dc_vax_admin.model)
  
  #specify model names
  mod.names <- c('dc_vax_admin.model_total', 'dc_vax_admin.model')
  
  #calculate AIC of each model
  output$AIC <- renderPrint({
  aictab(cand.set = models, modnames = mod.names)
  })
    
  # print model result
  output$statmodel <- renderPrint({
    summary(dc_vax_admin.model_total)
  })
  
}


shinyApp(ui = ui, server = server)