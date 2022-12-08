# ----------------------- Sta 404 - Fall 2022 ---------------------#
#                 Group Project - Shiny app                        #
# Credits:        Thy Nguyen - Gender/Race Data                    #
#                 Anh Vo     - Population/Expenditures Dataa       #
#------------------------------------------------------------------#

# Packages

library(plotly)
library(tidyverse)
library(shiny)
library(writexl)
library(readxl)
library(maps)
library(tidyverse)
library(stringr)
library(lubridate)
library(ggthemes)
library(shinythemes)
library(DT)
library(scales)
library(formattable)

# Data load

setwd("/Users/anh.vo/Downloads/STA 404/Final Draft/Data/Clean data/RData file")

load("race_income.RData")
load("gender_income.RData")
load("population.RData")
load("expenditures.RData")
load("sum_ex_adj.RData")
load("sum_ex.RData")
 

state_map_data <- map_data("state")
state_map_data$region<-str_to_title(state_map_data$region)

state_map_all <- left_join(state_map_data,pop,by=c("region"="State"))
state_map_all2 <- left_join(state_map_data,expenditures,by=c("region"="State"))


setwd("/Users/anh.vo/Downloads/STA 404/Final Draft/")
## Choices of inputs

education <- c("Less than High School" = "`Less than High School`", 
               "High School" = "`High School`",
               "College - No degree" = "`College - No degree`",
               "Associate" = "Associate",
               "Bachelor" = "Bachelor", 
               "Master" = "Master")


gender <- c("Male" = "Male",
            "Female" = "Female")


popfull <- c("Total High School Graduates"="Total_graduates",
  "Total population in thousands"="Total_population",
  "Total school-aged population in thousands"="School_population")


popratio <- c("High School Graduates:Total Population"= "hs_pop_ratio",
              "School-age Population:Total Population"="school_pop_ratio",
              "High School Graduates:School-age Population"= "hs_school_ratio")


plot2year <- c("2000" = "2000",
               "2010" = "2010",
               "2015" =  "2015" ,
               "2016"= "2016",
               "2017" = "2017",
               "2018" = "2018",
               "2019" = "2019",
               "2020"= "2020")
  
plot3year <- c("1999-2000" = "1999-2000",
               "2009-2010" = "2009-2010",
               "2014-2015" =  "2014-2015",
               "2017-2018" = "2017-2018",
               "2018-2019" = "2018-2019")

inst <- c("Private Post-secondary" = "Private Post-secondary",
          "Public Secondary"  =  "Public Secondary")

org <- c("Non-profit" = "Non-profit",
         "For-profit"= "For-profit")

sort <- c("Increasing Average",
          "Decreasing Average")


## (2) Build the shiny app
ui <- 
  navbarPage("Digest of Education", collapsible = TRUE, inverse = TRUE, theme = shinytheme("spacelab"),

    tabPanel("Median Earnings", 
        sidebarLayout(
          
          sidebarPanel(width = 4,
            
            selectInput(inputId = "Edu",
                        label = "Choose the education level:",
                        choices = education,
                        selected = "Less than High School"),
            
            selectInput(inputId = "Sex",
                        label = "Choose the gender:",
                        choices = gender,
                        selected = "Male")
            ),
            
          mainPanel(
            tabsetPanel(
              tabPanel("By Education",
                       br(), br(), 
                       p("(plot might not appear immediately because it is trying its best to generate. Please be patient! :)"),
                       br(), br(), 
                       h3(strong("Median Annual Earnings of Selected Educational Level")),
                       p(
                       "This plot shows the Median Annual Earnings of full-time year-round workers from", strong("25 to 34 years old
                       by Race/Ethnicity"),"and", strong("Highest Education Attainment (Selected years from 1995 to 2000)"), ". Please
                       choose the level of education you want to know."),
                       plotlyOutput(outputId = "timeplot")),
              
              
              tabPanel("By Gender", 
                       br(), br(), 
                       p("(plot might not appear immediately because it is trying its best to generate. Please be patient! :)"),
                       
                       br(), br(),
                       h3(strong("Median Annual Earnings of Selected Gender")),
                       p(
                         "This plot shows the Median Annual Earnings of full-time year-round workers from", strong("25 to 34 years old
                       by Race/Ethnicity"),"and", strong("Highest Education Attainment (Selected years from 1995 to 2000)"), ". Please
                       choose the gender you want to know."),
                       plotlyOutput(outputId = "boxplot"))
            )
      
          )
          
        
          
         ) ## end sidebarLayout
    ), ## end tabpanel 1


 tabPanel("Population", 
          
          sidebarLayout(
            sidebarPanel(
              helpText("Create demographic maps based on population or ratios, selected years"),
              
              selectInput(
                inputId = "yearselected",
                label = "Select year",
                choices = plot2year,
                selected = "2020"
              ),
              
              selectInput(
                inputId = "pop_type",
                label = "Select the type of population",
                choices = popfull,
                selected = "Total High School Graduates"
              ),
              
              br(),
              
              h4("Select to display population ratio"),
          
              br(),
              
              checkboxInput(inputId = "ratio",
                            label = "Plot by ratios",
                            value = FALSE),
              helpText("‎ "),
              radioButtons(
                inputId = "ratio_type",
                label = "Select the desired ratio",
                choices = popratio,
                selected = "hs_pop_ratio"
              ),
              
              
              helpText("HSGrad:Population with 0.2 ratio meaning for 10 people there are 2 high school graduates ")
            ),
          
            
            mainPanel(
              tabsetPanel(
                tabPanel("Map plot",     
                         br(), br(),  
                         p("(plot might not appear immediately because it is trying its best to generate. Please be patient! :)"),
                         br(),
                         h3(strong("US Map for population AND/OR population ratio")),
                        p("This plot is interactive and can be zoomed in/out for visualization purposes."),
                        p("The plot includes all", strong("50 US states"), ". First, choose the desired", strong("year"), "and", 
                        strong("type of population (Total, School-aged, HS Graduates)"), 
                        "to look at. You can also choose to display the", strong("population ratio map"),
                        "by checking the box. If population ratio is desired, then click the different buttons of types of ratio to look at."),
                         p("Hover the mouse to different states to get the number of people in the population selected. If ratio is selected, hover the mouse to different states to get the ratio of desired populations."),
                         plotlyOutput(outputId = "popplot")), 
                tabPanel("Display Table", 
                         br(), br(), 
                         p("(table might not appear immediately because it is trying its best to generate. Please be patient! :)"),
                         br(),
                         h3(strong("Display Table For Selected Population/Ratio")),
                         p("Observe each of the selected population's statistics for a selected year. If statistics for",
                         strong("ratio"), "is desired, simply check the ratio box. If you want a quick search for a state, hover to the search bar of the table to quickly search for a single state."),
                         DT::dataTableOutput(outputId = "sumtablepop")))

                )
            
  
            
          ) ## end sidebarLayout
  ),  ## end tab panel2
 
 
 tabPanel("Expenditures", 
          
          sidebarLayout(
            sidebarPanel(
              helpText("Create demographic maps based on expenditures, selected years."),
              helpText("Offers current or 2021 constant (adjusted for inflation) amount"),
              
              br(),
              
              checkboxInput(inputId = "adj",
                            label = "Adjusted for inflation",
                            value = FALSE),
              
              br(),
              
              selectInput(
                inputId = "year2selected",
                label = "Select year",
                choices = plot3year,
                selected = "2017-2018"
              ),
              
          
              selectInput(
                inputId = "institution_type",
                label = "Select the type of Institution",
                choices = inst,
                selected = "Private Post-secondary"
              ),
              selectInput(
                inputId = "org_type",
                label = "Select the type of Organization",
                choices = org,
                selected = "Non-profit"
              ),
              
              br(),
              br(),
              
              h4("FOR SUMMARY TABLE:"),
              br(),
              
              radioButtons(
                inputId = "full_or_partial",
                label = "Select FULL or INTERACTIVE table",
                choices = c("Full Summary Statistics",
                            "User-input summary statistics"),
                selected = "Full Summary Statistics"
              ),
              
              selectInput(inputId="stateexp", label="Select State(s) for Summary Statistics",
                          choices=expenditures$State,multiple = TRUE),
              
              br(),

              helpText("Note: Montana and Wyoming contains many missing data"),
              helpText("Note: Public Secondary school are not for profit institution.
                       Selecting Public Secondary - For-profit would result in a blank plot"),
            ),
            
            mainPanel(
              tabsetPanel(
                tabPanel("Map plot",  
                         br(), br(), 
                         p("(plot might not appear immediately because it is trying its best to generate. Please be patient! :)"),
                         
                  br(),
                  
                  h3(strong("US Map for Expenditures")),
                  p("This plot is interactive and can be zoomed in/out for visualization purposes."),
                  p("The plot includes all", strong("50 US states"), "Choose if you want to look at current dollars (of that year), or the constant dollars (2021, adjusted by inflation",
                    ". Then, choose the desired", strong("year"), 
                    strong(", type of institution (Private Post-secondary or Public Secondary)"),
                  "or", strong("type of organization (Non-profit or For-profit)"), "to look at."),
                  p("Hover the mouse to different states to get the number of people in the population selected. If ratio is selected, hover the mouse to different states to get the ratio of desired populations."),
                  br(), 
                  p(strong("NOTE:"), "Public Secondary school are not for profit institution. Selecting Public Secondary - For-profit would result in a blank plot"),
                  
                
                  
                  plotlyOutput(outputId = "expplot")),
                tabPanel("Summary Statistics",
                         br(), br(), 
                         
                         p("(table might not appear immediately because it is trying its best to generate. Please be patient! :)"),
                         
                         br(),
                         
                         p("For the summary table, select if you want a", strong("full"),"table of five-number summary statistics, or a ",
                           strong("you-selected"), "model, where you can choose the States that you want to look at. If you want a quick search, hover to the search bar of the table to quickly search for a single state."),
                         
                         
                         
                         DT::dataTableOutput(outputId = "sumtableexp"))
              ) ,
              textOutput('text1'))
          ) ## end sidebarLayout
 ), ## end tab panel 3
 
 
 
 tabPanel(
   title = "About",
   titlePanel("About"),
   "Created with R Shiny",
   
   sidebarLayout(
     sidebarPanel(width = 3,# Input() functions
       br(),
       helpText("Authors: Anh Vo, Thy Nguyen"),
       helpText("Data Science and Statistics majors at Miami University, Oxford, OH")
   ##   img(src = "miami-logo.jpeg")

     ),
     mainPanel(# Output() functions
       h4("Data Source: "), uiOutput("tab"),
       h4("Data are taken from the National Center for Education Statistics "),
       h5("The Digest of Education Statistics contains a set of tables covering the broad field of American education from prekindergarten through graduate school."),
       h5("Tables are taken directly from the websites, and are included for other users to run inside the zip files."),
       br(),
       br(),
       h5("Thank you for taking a look at our applications. We are striving to improve this app in the future!")
       
     )
   )
   
 )  ## end tab panel 4

 
 
)

server <- function(input, output) {
  
  url <- a("NCES", href="https://nces.ed.gov/programs/digest/current_tables.asp")
  
  output$tab <- renderUI({
    tagList("URL link:", url)
  })
  
  output$timeplot <- renderPlotly({
    ggplotly(
        ggplot(race_income)+
          geom_line(aes_string(x = "Year", y = input$Edu, color = "Race"))+
          labs(x = "Year", y = "Median Annual Earnings", 
               title = paste("Median Annual Earnings of people with", 
                             names(education)[education == input$Edu], "by Race/Ethnicity"), 
               color = "Race/Ethnicity")+
          theme(plot.title = element_text(hjust = 0.5, vjust = 0.5), legend.position = "bottom")
    )}
  )
  
  output$boxplot <- renderPlotly({
    ggplotly(
      ggplot(gender_income)+
        geom_boxplot(aes_string(x = "Education", y = input$Sex, fill = "Education"))+
        labs(x = "Education", y = "Median Annual Earnings", 
             title = paste("Median Annual Earnings of", 
                           names(gender)[gender == input$Sex], "by Educational Attainment"))+
        theme(plot.title = element_text(hjust = 0.5, vjust = 0.5))
    )}
  )
  
  
  output$popplot <- renderPlotly({
    
    
    state_map <- state_map_all[which(state_map_all$Year == input$yearselected), ]
    
    if(input$ratio==TRUE){
      ggplotly(
        ggplot()+
          geom_polygon(aes_string(x="long",y="lat",group="group",fill=input$ratio_type),
                       data=state_map) +
          scale_fill_gradient(low = "#56B1F7", high = "#132B43") +
          labs(x = "", y = "", 
               title = paste("US Ratio Map for the", 
                             names(popratio)[popratio == input$ratio_type], 
                             "for the year", names(plot2year)[plot2year == input$yearselected]),
               fill = names(popratio)[popratio == input$ratio_type])+
          theme(plot.title = element_text(hjust = 0.5, vjust = 0.5)) +
          coord_map() +
          theme_map()
      ) 
    } else {
      require(scales)
      ggplotly(
        ggplot()+
          geom_polygon(aes_string(x="long",y="lat",group="group",fill=input$pop_type),
                       data=state_map) +
          scale_fill_gradient(low = "#56B1F7", high = "#132B43") +
          labs(x = "", y = "", 
               title = paste("US Map for the", 
                             names(popfull)[popfull == input$pop_type], 
                             "for the year", names(plot2year)[plot2year == input$yearselected]),
               fill = names(popfull)[popfull == input$pop_type])+
          scale_fill_continuous(labels=comma) +
          theme(plot.title = element_text(hjust = 0.5, vjust = 0.5)) +
          coord_map() +
          theme_map()
          
      ) 
    }

    
  }
  )
  warning_text <- reactive({
    ifelse(TRUE %in% (input$org_type == "For-profit" && input$institution_type == "Public Secondary"),
           'You have chosen For-profit Public Secondary Institution - which does not exist',
           '') 
  })
  output$text1 <- renderText(warning_text()) 
  
  output$expplot <- renderPlotly({
        
  
        state_map <- state_map_all2[which(state_map_all2$Year == input$year2selected), ]
        state_map <- state_map[which(state_map$Institution_Type == input$institution_type), ]
        state_map <- state_map[which(state_map$Organization_type == input$org_type), ]

        if(input$adj==TRUE){  ### Constant value
          ggplotly(
            ggplot()+
              geom_polygon(aes_string(x="long",y="lat",group="group",fill="Constant"),
                           data=state_map) +
              scale_fill_gradient(low = "#56B1F7", high = "#132B43") +
              labs(x = "", y = "", 
                   title = paste("US Map for", 
                                 names(org)[org == input$org_type], 
                                 names(inst)[inst == input$institution_type], 
                                 "for the year", names(plot3year)[plot3year == input$year2selected]),
                   fill = "Constant (2021) Expenditures")+
              scale_fill_continuous(labels=comma) +
              theme(plot.title = element_text(hjust = 0.5, vjust = 0.5)) +
              coord_map() +
              theme_map()
          ) 
        } else {     ### Current value
          require(scales)
          ggplotly(
            ggplot()+
              geom_polygon(aes_string(x="long",y="lat",group="group",fill="Current"),
                           data=state_map) +
              scale_fill_gradient(low = "#56B1F7", high = "#132B43") +
              labs(x = "", y = "", 
                   title = paste("US Map for", 
                                 names(org)[org == input$org_type], 
                                 names(inst)[inst == input$institution_type], 
                                 "for the year", names(plot3year)[plot3year == input$year2selected]),
                   fill = "Current Expenditures")+
              scale_fill_continuous(labels=comma) +
              theme(plot.title = element_text(hjust = 0.5, vjust = 0.5)) +
              coord_map() +
              theme_map()
            
          ) 
        }
      }
      )


  output$sumtablepop <- DT::renderDT({
    pop <- pop %>%
     filter(Year %in% input$yearselected)

    if(input$ratio==TRUE){  ### Constant value
      if (input$ratio_type == "hs_pop_ratio") {
        ret <- pop[c("Year", "State", input$ratio_type, "Total_graduates", "Total_population")]
        names(ret) <- c("Year", "State", names(popratio)[popratio == input$ratio_type], "High School Graduates", "Total Population")
      } else if (input$ratio_type == "hs_school_ratio") {
        ret <- pop[c("Year", "State", input$ratio_type, "Total_graduates", "School_population")]
        names(ret) <- c("Year", "State", names(popratio)[popratio == input$ratio_type], "High School Graduates", "School-aged Population")
      } else{
        ret <- pop[c("Year", "State", input$ratio_type, "School_population", "Total_population")]
        names(ret) <- c("Year", "State", names(popratio)[popratio == input$ratio_type], "School-aged Population", "Total Population")
        
      }
      
      ret

    }

    else{
      ret <- pop[c("Year", "State", input$pop_type)]
      names(ret) <- c("Year", "State", names(popfull)[popfull == input$pop_type])
      ret

    }
  })

  
  output$sumtableexp <- DT::renderDT({
    
    if (input$full_or_partial == "Full Summary Statistics") {
      if(input$adj==TRUE){
        sum_ex_adj
      } else {
        sum_ex
      }
    }

    else {
    if(input$adj==TRUE){  ### Constant value
      sum_ex_adj <- expenditures %>%
        filter(State %in% input$stateexp) %>%
        group_by(State) %>%
        summarize(Mean = mean(Constant, na.rm=T),
                  Median = median(Constant, na.rm=T),
                  Standard_Deviation = sd(Constant, na.rm=T),
                  Lower_Quartile = quantile(Constant, 0.25, na.rm=T),
                  Upper_Quartile = quantile(Constant, 0.75, na.rm=T)) %>%
        dplyr::select(State, Lower_Quartile, Standard_Deviation, Median, Mean, Upper_Quartile)
      sum_ex_adj
    }
    else{
      sum_ex <- expenditures %>%
        filter(State %in% input$stateexp) %>%
        group_by(State) %>%
        summarize(Mean = mean(Current, na.rm=T),
                  Median = median(Current, na.rm=T),
                  Standard_Deviation = sd(Current, na.rm=T),
                  Lower_Quartile = quantile(Current, 0.25, na.rm=T),
                  Upper_Quartile = quantile(Current, 0.75, na.rm=T)) %>%
        dplyr::select(State, Lower_Quartile, Standard_Deviation, Median, Mean, Upper_Quartile)
      sum_ex
    }
      
    }
  })
  
  
  observe(print({input$ratio_type}))
  
}

shinyApp(ui = ui, server = server)
