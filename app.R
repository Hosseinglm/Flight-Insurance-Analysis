
library(shiny)
library(shinydashboard)
library(rsconnect)
library(readr)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(rvest)
library(summarytools)

#getwd()
#setwd("URL Address")

dataIns<- read_csv("AirAsia_insurance.csv")
#head(dataIns, header=T)


shinyUI(fluidPage(
# Define UI for application that draws a histogram
ui <- fluidPage(

  ui <- dashboardPage(
    skin = "red",
   
    
    dashboardHeader(title = "Menu"),
    
    dashboardSidebar(width = 340,
      sidebarMenu(
        HTML(paste0(
          "<br>",
          "<a href='https://www.airasia.com/en/gb' target='_blank'><img style = 'display: block; margin-left: auto; margin-right: auto;' src='https://static.airasia.com/design/logos/airasiacom_logo.svg' width = '186'></a>",
          "<br>",
          
          "<br>"
        )),
        menuItem("Insurance Analysis", tabName = "Insurance_Analysis"),
        menuItem("Charts", startExpanded = TRUE,
                 menuSubItem("Insurance Flag Based on Days", tabName = "subitem1"),
                 menuSubItem("Insurance Flag Based on Trip Type", tabName = "subitem2"),
                 menuSubItem("Insurance Flag Based on Sales Channel", tabName = "subitem3"),
                 menuSubItem("Insurance Flag Based on Flight Duration", tabName = "subitem4"),
                 menuSubItem("Insurance Flag Based on FNB Category", tabName = "subitem5"),
                 menuSubItem("Insurance Flag Based on Seat Category", tabName = "subitem6"),
                 menuSubItem("Interactive Chart Based on Various Factors", tabName = "subitem7")
        ),
        menuItem("Our Team", tabName = "Team_Members"),
        menuItem("Air Asia Website", href = "https://www.airasia.com/en/gb")
      )
    ),
    
    dashboardBody(

      tabItems(
        # First tab content
        tabItem(tabName = "Insurance_Analysis",
                
                h1(align = "center", strong("Air Asia Insurance")),
                br(),
                h3(strong("Problem Statement")),
                p("The utility of travel insurance as a risk reduction scheme is one useful indicator on how modern socially sensitive society views their quality of life. Travel insurance covers the financial risk against unforeseen mishaps that travellers are exposed to while on their holiday. Depending on the policy coverage, such risks include loss of baggage, loss of travel deposit, trip cancellation, medical repatriation or evacuation, physical injury, bankruptcy of travel service providers, personal liability against tort committed to citizen of host country, legal expenses cover and may cover almost any unforeseen situations that is not covered by typical policies such as terrorism and denied boarding. However, the desirability to purchase travel insurance by consumers is still uncertain. 
"),
                br(),
                h3(strong("Common Consumer Pattern of Purchasing Insurance")),
                p("Based on our analysis, we observed that consumers that travel in a short travel duration are more likely to purchase insurance, compared to a consumer who travels on a flight that has a long duration of travel. This can be because, In some developed countries, several legal measures were introduced to broaden the types of protection mechanism for outbound travellers on top of reliance on regulated travel insurance. Therefore, they may not feel the need to purchase travel insurance especially for overseas traveling. In Malaysia, outbound travellers are protected, to a large extent, through licensing regulations of travel agencies. However, these protections are only focused on overseas travelling. Therefore, consumers tend to purchase travel insurance when on short travel. The possibilities of unexpected incidents and derails to occur is high, even when in short travel. With the many things that can go wrong while traveling, it’s worth considering some form of travel insurance. Let’s take an example of a Malaysian airline incident, flight MH17, that crashed after being hit by a Russian-made Buk missile over eastern Ukraine. A total of 283 passengers, including 80 children, and 15 crew members that were on board are dead. "),
                br(),
                tags$img(src="Travel.png", height = '150', width = '300', alt = 'Travellers', deleteFils= FALSE),
                br(),
                tags$img(src="Insure.png", height = '400', width = '650', alt = 'Insurance', deleteFils= FALSE),
                
                
        ),
      
        # Second tab content
        
        tabItem(tabName = "subitem1", 
                br(),
                fluidRow(
                box(
                  titlePanel("Insurance Flag Based on Days"),
                  p("From the analysis of the histogram, we can conclude that the number of people buying insurance on different days is significantly higher on Monday and Wednesday than other weeks, so there is a greater chance of buying insurance on Monday and Wednesday. Airline marketing departments can consider introducing more ticket insurance packages on Mondays and Wednesdays to stimulate people's desire to buy insurance and increase the sales of insurance. Airline merchandise management and front end departments can create pop-up posters on the web on Mondays and Wednesdays to synchronize insurance promotions from marketing operations."),
                  br(),br(),
                  plotOutput("dayplot", height = 400),
                  
                  width = 12,
                  ),
                ),
                ),
        
        tabItem(tabName = "subitem2", 
                br(),
                fluidRow(
                  box(
                    titlePanel("Insurance Flag Based on Trip Type"),
                    p("It can be seen intuitively from this histogram by travel type that passengers who buy round-trip tickets are more likely to buy insurance than those who buy one-way tickets and Circle Tripped. Therefore, the front-end technology department can make a pop-up poster, and when the user chooses a round-trip ticket, the pop-up asks whether they need to buy insurance, so that the probability of successful promotion will be greater."),
                    br(),br(),
                    plotOutput("tripplot", height = 400),
                    width = 12,
                  ),
                ),
        ),
        tabItem(tabName = "subitem3", 
                br(),
                fluidRow(
                  box(
                    titlePanel("Insurance Flag Based on Sales Channel"),
                    p("This is a histogram of Insurance Flag Based on Sales Channel, which analyses the statistics of Insurance purchase by users from different purchasing channels. It is easy to see that users who buy Insurance through websites have a higher probability of purchasing Insurance, perhaps because the computer screen is large, and it is easier to operate with a mouse. Another possibility is that web pages are more personal than app pages. Therefore, the operation department and the front management department should make more adjustments on the APP to optimize products, so that mobile phone users can also improve the probability of buying insurance."),
                    br(),br(),
                    plotOutput("salesplot", height = 400),
                    width = 12,
                  ),
                ),
        ),
       
        
        tabItem(tabName = "subitem4",
                br(),
                fluidRow(
                  box(
                    titlePanel("Insurance Flag Based on Flight Duration"),
                    p("It’s depicted in the graph that passengers for shorter flight duration tend to purchase insurance. This can be related to cost, since AirAsia is considered a low budget airline and log duration flights are more expensive, hence it can be inferred that purchasing insurance might be out of customers' budget."),
                    br(),br(),
                    plotOutput("flightDurationplot", height = 400),
                    width = 12,

                  ),

                ),
        ),
        
         tabItem(tabName = "subitem5",
                 br(),
                 fluidRow(
                   box(
                     titlePanel("Insurance Flag Based on FNB Category"),
                     #p("Lorem Ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry's standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book. It has survived not only five centuries, but also the leap into electronic typesetting, remaining essentially unchanged. It was popularised in the 1960s with the release of Letraset sheets containing Lorem Ipsum passages, and more recently with desktop publishing software like Aldus PageMaker including versions of Lorem Ipsum."),
                     br(),br(),
                     plotOutput("fnbplot", height = 400),
                     width = 12,
        
                   ),
        
                 ),
        ),
        
        tabItem(tabName = "subitem6",
                br(),
                fluidRow(
                  box(
                    titlePanel("Insurance Flag Based on Seat Category"),
                    p("Based on Seat Category, we also made an analysis, because we all know that Airasia needs to pay more money to select seats. If you want to get a Seat near the front or choose a spacious Seat, you need to pay more money. Therefore, we made an analysis according to whether we pay more money to select seats and obtained the following chart. Customers who didn't pay for a seat were more likely to have insurance than those who did."),
                    br(),br(),
                    plotOutput("seatplot", height = 400),
                    width = 12,

                  ),

                ),
        ),
        
        tabItem(tabName = "subitem7",
                br(),
                titlePanel("Interactive Chart"),
                p("Based on the analysis, passengers with flights in early hours of the day are less likely to buy insurance. which seems reasonable as early flights are cheaper and more convenient for low budget travellers, hence purchasing insurance might be out of their budget."),
            
                p("We have also noticed that when the tickets purchased in groups of more than 5 are less likely to buy insurance. this category might refer to travel agencies and tour companies, in their case travel insurance will be purchased separately by travellers."),
                br(),br(),
                sidebarLayout(
                  sidebarPanel(h3("Input"), 
                               sliderInput("flightHour", "Flight Hour", min = 1, max = 24, step = 1, round = TRUE, value = 20),
                               radioButtons("insFlag", "Insurance Flag",choices = c(0,1), selected = 0),
                               selectInput("paxCount", "Pax Count",choices = c(3,4,5,6,7,8,9), selected = 7)),
                  mainPanel(
                    
                    plotOutput("plot"),
                    br(),br(),
                    tableOutput("results")
                    
                    
                  )
                )
                
            
        ),
       
        tabItem(tabName = "Team_Members",
                br(),
                #box(
                  titlePanel("Our Team Members"),
                  h3("Group Name: Flight Insurance Analysis (Insurance Crew!)"),
                br(),br(),
                  tags$ul(
                    tags$li("ID: S2031256 || Name: Kiruthika Lexmi Sundram"), 
                    tags$li("ID: 17141946 || Name: Cheng Ziyu"), 
                    tags$li("ID: S2126753 || Name: Nazanin Ahmadian"), 
                    tags$li("ID: S2148678 || Name: Yang Hao"), 
                    tags$li("ID: S2129915 || Name: Pushpanjali Nagarajan(Anjali)")
                  ),
                
               
        )
       
      )
    )
  )

)
))
# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$dayplot <- renderPlot({
    
      dataIns %>%
      group_by(flightDay) %>%
      summarise(numOfFlag = sum(insFlag)) %>%
      arrange(desc(numOfFlag)) %>%
      ggplot(aes(x=flightDay,y=numOfFlag))+ geom_col() + labs(x = 'Days', y = 'Number of Insurance', title = 'Number of Insurance Based on Days')
    
  })

  output$tripplot <- renderPlot({

      dataIns %>%
      group_by(tripTypeDesc) %>%
      summarise(numOfFlag = sum(insFlag)) %>%
      arrange(desc(numOfFlag)) %>%
      ggplot(aes(x = tripTypeDesc, y = numOfFlag)) + geom_col() + labs(x = 'Trip Type', y = 'Number of Insurance', title = 'Number of Insurance Based on Trip Type')

  })
  output$salesplot <- renderPlot({
      dataIns %>%
      group_by(salesChannel) %>%
      summarise(numOfFlag = sum(insFlag)) %>%
      arrange(desc(numOfFlag)) %>%
      ggplot(aes(x = salesChannel, y = numOfFlag)) + geom_col() + labs(x = 'Sales Channel', y = 'Number of Insurance', title = 'Number of Insurance Based on Sales Channel')

  })

  output$flightDurationplot <- renderPlot({
    dataIns %>%
    ggplot( aes(x = insFlagT, y = flightDurationHour)) + geom_col() + labs(x = ' Insurance Condition', y = 'Count of Flight Duration Hour', title = 'Insurance - Flight Duration Hour')

  })
  
  output$fnbplot <- renderPlot({
    dataIns %>%
      group_by(fnbCategoryT) %>%
      summarise(numOfFlag = sum(insFlag)) %>%
      arrange(desc(numOfFlag)) %>%
      ggplot(aes(x = fnbCategoryT, y = numOfFlag)) + geom_col() + labs(x = 'FNB Category', y = 'Number of Insurance', title = 'Number of Insurance Based on FNB Condition')

  })
  output$seatplot <- renderPlot({
    dataIns %>%
      group_by(seatCategoryT) %>%
      summarise(numOfFlag = sum(insFlag)) %>%
      arrange(desc(numOfFlag)) %>%
      ggplot(aes(x = seatCategoryT, y = numOfFlag)) + geom_col() + labs(x = 'Seat Category', y = 'Number of Insurance', title = 'Number of Insurance Based on Seat Condition')

  })
  output$plot <- renderPlot({
    filtered_data <- dataIns[dataIns$flightHour <= input$flightHour &
                               dataIns$insFlag == input$insFlag &
                               dataIns$paxCount == input$paxCount,]
    
    ggplot(data = filtered_data, aes(x = flightHour, y = lengthOfStay)) +
      geom_point()
   
  })
  output$results <- renderTable({
    
    filtered_data <- dataIns[dataIns$flightHour <= input$flightHour &
                               dataIns$insFlag == input$insFlag &
                               dataIns$paxCount == input$paxCount,]
    
    filtered_data
  })

}

# Run the application 
shinyApp(ui = ui, server = server)
