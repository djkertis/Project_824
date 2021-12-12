#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(lubridate)
library(scales)
library(readxl)
library(shinythemes)

working_dir <- "~/data_vis/assignments/git-viz-repo/Project_824"

use_local <- TRUE

### Death by cause per week
if (!use_local) {
  vaccine_status_url <-
    "https://data.cdc.gov/api/views/unsk-b7fc/rows.csv?accessType=DOWNLOAD"
  death_by_cause_hist_url <-
    "https://data.cdc.gov/api/views/3yf8-kanr/rows.csv?accessType=DOWNLOAD"
  death_by_cause_url <-
    "https://data.cdc.gov/api/views/muzy-jte6/rows.csv?accessType=DOWNLOAD"
} else {
  vaccine_status_url <-
    "./data/COVID-19_Vaccinations_in_the_United_States_Jurisdiction.csv"
  death_by_cause_url <-
    "./data/Weekly_Provisional_Counts_of_Deaths_by_State_and_Select_Causes__2020-2021.csv"
  death_by_cause_hist_url <-
    "./data/Weekly_Counts_of_Deaths_by_State_and_Select_Causes__2014-2019.csv"
}
# https://www.census.gov/data/tables/time-series/dec/popchange-data-text.html
pop_url <- "./data/pop-decennial.xls"

# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel("COVID Tracker"),
  
  # Sidebar with a slider input for number of bins
  bootstrapPage(
    sidebarLayout(
      sidebarPanel(
        sliderInput(
          "year",
          "Year:",
          min = 2014,
          max = 2022,
          value = c(2014, 2022),
          sep = "",
          step = 1
        ),
        selectInput("location", "Location", c()),
        checkboxInput("showMult", "Show COVID.19.Multiple", FALSE),
        checkboxInput("showVacc", "Show Vaccine Administered", FALSE),
        width = 3
      ),
      mainPanel(plotOutput("distPlot"))
    ),
    
    hr(),
    plotOutput("heatPlot"),
    theme = shinytheme("cerulean")
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  setwd(working_dir)
  
  # reactive so we can tell the shape routines when the data arrives
  dataInput <- reactive({
    death_by_cause <- read.csv(death_by_cause_url)
    death_by_cause_hist <- read.csv(death_by_cause_hist_url)
    vaccine_status <- read.csv(vaccine_status_url)
    pop_inp_df <- read_xls(pop_url, sheet = "States", skip = 3)
    
    updateSelectInput(session ,
                      "location",
                      choices = unique(death_by_cause$Jurisdiction.of.Occurrence))
    list(
      dbc = death_by_cause,
      dbc_h = death_by_cause_hist,
      vs = vaccine_status,
      pop = pop_inp_df
    )
  })
  
  dataSets <- reactive({
    location = input$location
    start_year <- ymd(input$year[1] * 10000 + 101)
    end_year <- ymd(input$year[2] * 10000 + 101)
    
    death_curr <- dataInput()[["dbc"]]
    death_hist <- dataInput()[["dbc_h"]]
    vs <- dataInput()[["vs"]]
    pop_inp_df <- dataInput()[["pop"]]
    
    dbc <- shape_death_data(death_hist, death_curr, location, start_year, end_year)
    
    vs_df <- shape_vaccine_data(vs, location, start_year, end_year)
    
    pop_df <- shape_heatmap_data(death_hist, death_curr, vs, pop_inp_df)
    
    list(dbc = dbc, vs = vs_df, pop = pop_df)
  })
  
  shape_death_data <- function(death_hist, death_curr, location, start_year, end_year) {
    dbc_h_u <- death_hist %>%
      filter(Jurisdiction.of.Occurrence == location) %>%
      mutate(
        Date = as.Date(Week.Ending.Date, "%m/%d/%Y"),
        Total = All..Cause,
        Septicemia = Septicemia..A40.A41.,
        Malignant.neoplasms = Malignant.neoplasms..C00.C97.,
        Diabetes = Diabetes.mellitus..E10.E14.,
        Alzheimers = Alzheimer.disease..G30.,
        Influenza.and.Pneumonia = Influenza.and.pneumonia..J10.J18.,
        Chronic.lower.respiratory = Chronic.lower.respiratory.diseases..J40.J47.,
        Other.respiratory.system = Other.diseases.of.respiratory.system..J00.J06.J30.J39.J67.J70.J98.,
        Nephritis = Nephritis..nephrotic.syndrome.and.nephrosis..N00.N07.N17.N19.N25.N27.,
        Not.elsewhere.classified = Symptoms..signs.and.abnormal.clinical.and.laboratory.findings..not.elsewhere.classified..R00.R99.,
        Diseases.of.heart = Diseases.of.heart..I00.I09.I11.I13.I20.I51.,
        Cerebrovascular = Cerebrovascular.diseases..I60.I69.,
        COVID.19.Multiple = 0,
        COVID.19.Underlying = 0
      ) %>%
      select(Date:COVID.19.Underlying) %>%
      mutate (
        TotalNoCovid = Total - COVID.19.Underlying
        ,
        TotalCauses = select(., Septicemia:COVID.19.Underlying) %>%
          rowSums(na.rm = TRUE)
      )
    
    
    dbc_u <- death_curr %>%
      filter(Jurisdiction.of.Occurrence == location) %>%
      mutate(
        Date = as.Date(Week.Ending.Date, "%Y-%m-%d"),
        Total = All.Cause,
        Septicemia = Septicemia..A40.A41.,
        Malignant.neoplasms = Malignant.neoplasms..C00.C97.,
        Diabetes = Diabetes.mellitus..E10.E14.,
        Alzheimers = Alzheimer.disease..G30.,
        Influenza.and.Pneumonia = Influenza.and.pneumonia..J09.J18.,
        Chronic.lower.respiratory = Chronic.lower.respiratory.diseases..J40.J47.,
        Other.respiratory.system = Other.diseases.of.respiratory.system..J00.J06.J30.J39.J67.J70.J98.,
        Nephritis = Nephritis..nephrotic.syndrome.and.nephrosis..N00.N07.N17.N19.N25.N27.,
        Not.elsewhere.classified = Symptoms..signs.and.abnormal.clinical.and.laboratory.findings..not.elsewhere.classified..R00.R99.,
        Diseases.of.heart = Diseases.of.heart..I00.I09.I11.I13.I20.I51.,
        Cerebrovascular = Cerebrovascular.diseases..I60.I69.,
        COVID.19.Multiple = COVID.19..U071..Multiple.Cause.of.Death.,
        COVID.19.Underlying = COVID.19..U071..Underlying.Cause.of.Death.
      ) %>%
      select(Date:COVID.19.Underlying) %>%
      mutate (
        TotalNoCovid = Total - COVID.19.Underlying,
        TotalCauses = select(., Septicemia:COVID.19.Underlying) %>%
          rowSums(na.rm = TRUE)
      )
    
    dbc <- dbc_h_u %>% union(dbc_u) %>%
      filter(Date >= start_year, Date <= end_year)
    
    dbc
  }
  
  shape_heatmap_data <-
    function(death_by_cause_hist,
             death_by_cause,
             vaccine_status,
             pop_inp_df) {
      pop_df <- pop_inp_df %>% select(state = Areaname, pop = `2020`)
      
      
      ## Shape the death percentage data
      death_df <- death_by_cause %>% filter(MMWR.Year == 2021) %>%
        select(count = All.Cause,
               location = Jurisdiction.of.Occurrence) %>%
        group_by(location) %>%
        summarise(total = sum(count))
      
      ## Add New York City to New York
      death_df <- death_df %>%
        mutate(count = ifelse(location == "New York", total + death_df[death_df$location ==
                                                                         "New York City", 2] %>% pull , total))
      
      death_df_2019 <- death_by_cause_hist %>% filter(MMWR.Year == 2019) %>%
        select(count = All..Cause,
               location = Jurisdiction.of.Occurrence) %>%
        group_by(location) %>%
        summarise(total = sum(count))
      
      ## Add New York City to New York
      death_df_2019 <- death_df_2019 %>%
        mutate(total_2019 = ifelse(location == "New York", 
                                   total + death_df[death_df_2019$location ==
                                  "New York City", 2] %>% pull , total)) #%>%
      df_2019 <- death_df_2019 %>% inner_join(pop_df, by = c("location" = "state"))
      res_2019 <- df_2019 %>% mutate (perc = total_2019 / pop)
      res_2019$scaled2019DeathPerc <- scale(res_2019$perc)
      
      dat_2019 <- res_2019 %>% select(location, scaled2019DeathPerc) 

      
      # percDeath = Total deaths / Total population
      df <-
        death_df %>% inner_join(pop_df, by = c("location" = "state"))
      
      res <- df %>% mutate (perc = count / pop)
      # Standardize percDeath from -3 to 3
      res$scaledDeathPerc = scale(res$perc)
      
      ## Shape vaccination percentage
      vs <- vaccine_status %>% group_by(Location) %>%
        summarise(vaccPerc = max(Series_Complete_Pop_Pct))
      
      vs$scaledVaccPerc = scale(vs$vaccPerc)
      vs$location <- state.name[match(vs$Location, state.abb)]
      
      # Combine the two data sets and filter out states not in both data sets
      all_dat <-
        res %>% inner_join(vs, by = c("location" = "location")) %>%
        inner_join(res_2019, by = c("location" = "location"))
      
      
      
    }
  
  shape_vaccine_data <-
    function (vs_df, location, start_year, end_year) {
      if (location == "United States") {
        location = "US"
      }
      else {
        location <- state.abb[match(location, state.name)]
      }

      df <- vs_df %>%
        filter(Location == location) %>%
        mutate(date = as.Date(Date, "%m/%d/%Y"),
               year = as.numeric(format(date, format = "%Y")),) %>%
        select(
          year,
          MMWR_week,
          date,
          Location,
          
          Administered,
          Admin_Per_100K,
          Administered_Moderna,
          Administered_Janssen,
          Administered_Pfizer,
          Administered_Unk_Manuf,
          
          Administered_Dose1_Recip,
          Administered_Dose1_Pop_Pct,
          
          Series_Complete_Yes,
          Series_Complete_Pop_Pct,
          Series_Complete_Moderna,
          Series_Complete_Pfizer,
          Series_Complete_Unk_Manuf,
          Series_Complete_Janssen,
          
          Additional_Doses,
          Additional_Doses_Vax_Pct,
          Additional_Doses_Janssen,
          Additional_Doses_Moderna,
          Additional_Doses_Pfizer,
          Additional_Doses_Unk_Manuf
        )
      
      vsd_date <- df %>%
        group_by(year, MMWR_week) %>%
        summarise(date = max(date),)
      
      vsd_df <- df %>%
        inner_join(vsd_date, by = c("date")) %>%
        arrange(date)
      
      vsd_df
    }
  
  ### Heatmap plot
  output$heatPlot <- renderPlot({
    all_dat <- dataSets()[["pop"]]
    
    
    all_dat <- all_dat %>% mutate (scaledDeathPerc = -scaledDeathPerc,
                                        scaled2019DeathPerc = -scaled2019DeathPerc)
    
    
    
    all_dat <- rename(all_dat,
                      `2019 Death % (total)` = scaled2019DeathPerc,
                      `Death % (total)` = scaledDeathPerc,
                      `Vaccination %` =  scaledVaccPerc)
    
    long_dat <-
      all_dat %>% pivot_longer(c(`2019 Death % (total)`, `Death % (total)`, `Vaccination %`))
    
    dat <-
      long_dat %>% mutate(catvar = as.factor(name), catfill = as.double(value)) %>%
      select(location, catvar, catfill)
    
    dat %>% ggplot(aes(state.abb[match(location, state.name)], catvar, fill =
                         catfill)) +
      geom_tile() +
      labs(fill = "SD") +
      scale_fill_gradient2(low = "red",
                           mid = "white",
                           high = "green") +
      xlab("State") + ylab("") + ggtitle("2021 Vaccination/Death Percentage Heatmap by State")
    
  }, height = 120)
  
  
  ### Area plot
  output$distPlot <- renderPlot({
    vacc_df <- dataSets()[["vs"]]
    death_df <- dataSets()[["dbc"]]
    showMult <- input$showMult
    showVacc <- input$showVacc
    
    df <-
      death_df %>% tidyr::pivot_longer(Septicemia:COVID.19.Underlying)
    
    if (!showMult) {
      df <- df %>% filter(name != "COVID.19.Multiple")
    }
    df$name <-
      fct_relevel(df$name, "COVID.19.Multiple", "COVID.19.Underlying",)
    
    
    plt <- df %>% ggplot(aes(x = Date, y = value)) +
      geom_area(aes(fill = name)) +
      geom_line(aes(y = Total)) +
      geom_line(aes(y = TotalNoCovid), color = "blue") +
      geom_line(aes(y = TotalCauses), color = "red") +
      ylab("Deaths/Week") +
      theme(legend.position = "bottom") +
      scale_fill_brewer(palette = "RdYlBu") +
      scale_y_continuous(labels = comma)
    
    maxval <- layer_scales(plt)$y$get_limits()[[2]] * .01
    if (showVacc) {
      plt <- plt + scale_y_continuous(sec.axis = sec_axis(~ . / maxval,
                                                          name = "Admin",)) +
        geom_segment(data = vacc_df,
                     aes(
                       x = date,
                       y = Administered_Dose1_Pop_Pct * maxval ,
                       xend = date,
                       yend = 0
                     )) +
        geom_point(data = vacc_df,
                   aes(x = date, y = Administered_Dose1_Pop_Pct * maxval))
      
    }
    plt
  })
  
  
}

# Run the application
shinyApp(ui = ui, server = server)
