library(shinydashboard)
library(maps)
library(shiny)
library(leaflet)
library(tidyverse)
library(dplyr)
library(packcircles)
library(ggplot2)
library(tidyverse)
library(shinyjs)
library(plotly)
library(dygraphs)
library(shinyWidgets)
library(leaflet.extras)
library(xts)
library(RColorBrewer)
library(formattable)
library(dplyr)
library(repmis)
library(highcharter)
library(packcircles)
library(ggplot2)
library(viridis)
library(ggiraph)
#reading the file

jobsdata <- read.csv('seek_australia.csv')
cityName <- c('ACT','NSW','NT','QLD','SA','TAS','VIC','WA')
states = c('Australian Capital territory(ACT)',
           'New South Wales(NSW)',
           'Northern Territory(NT)',
           'Queensland(QLD)',
           'Southern Australia(SA)',
           'Tasmania(TAS)',
           'Victoria(VIC)',
           'Western Australia(WA)'
)

category1 <- c('Information & Communication Technology', 'Manufacturing, Transport & Logistics', 'Administration & Office Support', 'Banking & Financial Services', 'Healthcare & Medical')
months <- c('April','June','July','August','September','October','November')
type1 <- c( 'Casual/Vacation','Contract/Temp','Full Time','Part Time','All')
cat <- unique(jobsdata$category)
typeclass <- unique(jobsdata$job_type)


#grouping the data
grouped_data <-jobsdata  %>%
  group_by(state,lat,long)  %>%
  summarise(total = n_distinct(pageurl))

donut_data <-jobsdata  %>%
  group_by(state,job_type)  %>%
  summarise(count = n_distinct(pageurl))

category_data = jobsdata %>% 
  group_by(category) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count)) 
  
#creating a donut charts for all the states
nswdata <- filter(donut_data,state == 'NSW')
vicdata <- filter(donut_data,state == 'VIC')
tasdata <- filter(donut_data,state == 'TAS')
actdata <- filter(donut_data,state == 'ACT')
sadata <- filter(donut_data,state == 'SA')
wadata <- filter(donut_data,state == 'WA')
ntdata <- filter(donut_data,state == 'NT')
qlddata <- filter(donut_data,state == 'QLD')

#############################donut data ###################################

nswdata$fraction = nswdata$count / sum(nswdata$count)
nswdata$ymax = cumsum(nswdata$fraction)
nswdata$ymin = c(0,head(nswdata$ymax,n=-1))

vicdata$fraction = vicdata$count / sum(vicdata$count)
vicdata$ymax = cumsum(vicdata$fraction)
vicdata$ymin = c(0,head(vicdata$ymax,n=-1))
                 
actdata$fraction = actdata$count / sum(actdata$count)
actdata$ymax = cumsum(actdata$fraction)
actdata$ymin = c(0,head(actdata$ymax,n=-1))

ntdata$fraction = ntdata$count / sum(ntdata$count)
ntdata$ymax = cumsum(ntdata$fraction)
ntdata$ymin = c(0,head(ntdata$ymax,n=-1))

qlddata$fraction = qlddata$count / sum(qlddata$count)
qlddata$ymax = cumsum(qlddata$fraction)
qlddata$ymin = c(0,head(qlddata$ymax,n=-1))

sadata$fraction = sadata$count / sum(sadata$count)
sadata$ymax = cumsum(sadata$fraction)
sadata$ymin = c(0,head(sadata$ymax,n=-1))

tasdata$fraction = tasdata$count / sum(tasdata$count)
tasdata$ymax = cumsum(tasdata$fraction)
tasdata$ymin = c(0,head(tasdata$ymax,n=-1))

wadata$fraction = wadata$count / sum(wadata$count)
wadata$ymax = cumsum(wadata$fraction)
wadata$ymin = c(0,head(wadata$ymax,n=-1))





####################################### Pages of the RShiny APP#######################################




ui <- dashboardPage(
    dashboardHeader(title = "JOB LISTINGS"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Home", tabName = "Home", icon = icon("home")),
            menuItem("Places", tabName = "Places", icon = icon("map-marked")),
            menuItem("Categories", tabName = "Categories", icon = icon("door-open")),
            menuItem("Trends", tabName = "trends", icon = icon("line-chart"))
        )
    ),
    dashboardBody(
        tags$head(tags$style(HTML('
                          .box {margin: 5px;}'
        ))),
    tabItems(
        tabItem(tabName = "Home",
                fluidRow(
                    box(title=strong("Project Introduction"),width = 12,
                        p(style="text-align: justify;" , 'The Austalian jobs market project is done based on the open data set from Kaggle,
                          which consists of 20031 rows and 13 columns.The data consists of job postings in Australia of the year 2018 which includes 
                          information such as job category, the occupational place, job type and the expected salary range.The main objective of my 
                          visualization project is to allow user to search for a desired job type based on certain parameters like area selection,
                          or selecting an interested job category. He may also see the trends in job postings to know the 
                          rise and fall in the job recruiting market.These visualizations let the user know where they can search for a job,
                          what are new oppurtunities in the market and working in which places may be beneficial to them based on the 
                          average salary.While we try to put the numerous options in front on him, it gives the user a visual advantage 
                          on which basic conditions he is looking for a new job.The visualizations are not only helpful for full - time workers 
                          but also contractors and students who are looking for casual and part time oppurtunities.')
                    )
                    
                ),
              column(3,offset = 4,titlePanel(title = div("Australian Job Market"))),
              column(6,offset = 3, img(height = 550, width = 670, src = "job1.jpg"))
        ),
        #Places Tab
        
        tabItem(tabName = "Places",
                h1("Map Visualizations Based on a City Selected"),
                fluidRow(
                  box(title=strong("Information"),width = 12,
                      p(style="text-align: justify;" , 'This page allows the users to know the popular job locations in Australia. Job opputunities are 
                        spread across the Australian sub-continent and every state has their own fair share of job types depending maybe on the population 
                        or the number of companies that are available during the particular period.A user has to select a desired state from the drop down menu 
                        .Upon selection the user can view the categorization of the jobs in that state and also the percentage contribution.The selected state will be 
                        highlighted in the map for making it easy for the user to view the loaction and also compare the total on a whole with the other states.'),
                      p(style="text-align: justify;" , 'The job types consist of 4 main types. They are part time, full time, casual and contract jobs. A person may want to 
                        know where there is a higher demand of a particular type of jobs. Upon selecting the desired category , the user will geta a donut chart upon hovering 
                        shows the percentage compostion of its type in the total jobs present in the particular area.The place selected will be highlighted on the map 
                        and the other places tuen opaque inorder to clearly highlight the selected place.' ),
                  )
                ),
                fluidRow(
                    box(title =HTML("<font color='#800080', size = '5px'>Select a Region</font>"),icon=icon('home'),
                        solidHeader = F,
                        width = 6,
                        collapsible = T,
                        selectInput(
                            label = '',
                            inputId = "place", 
                            choices = states
                        ))
                ),
                fluidRow(
                    box( which='plot',lty='solid',
                         width = 6,
                         leafletOutput('placesMap',height = 600),
                         hr() ),
                    box( which='plot',lty='solid',
                         width = 6,
                         plotlyOutput("selected_var"),
                         p(style="text-align: justify;", 'The above donut chart indicates what is the distribution of different types of categories
                           present in the selected state. upon hovering the user gets a tool tip showing the percentage contribution in 
                           the number of jobs and the type of job it is.'))  
                )),
        tabItem(tabName = "Categories",
                h1("Categorical Visualizations of jobs"),
                fluidRow(
                  box(title =HTML("<font color='#800080', size = '5px'>Select a Job type</font>"),icon=icon('door-open'),
                      solidHeader = F,
                      width = 6,
                      collapsible = T,
                      pickerInput(
                        label = '',
                        inputId = "type", 
                        choices = type1
                      )),
                  box(title =HTML("<font color='#800080', size = '5px'>Select a Job Category</font>"),icon=icon('door-open'),
                      solidHeader = F,
                      width = 6,
                      collapsible = T,
                      pickerInput(
                        label = '',
                        inputId = "category", 
                        choices = category1
                      )
                  ),
                  
                  box(width = 100, collapsed = T, solidHeader = F,
                      p(style="text-align: justify;",'The below high chart which is an interactive bar chart shows the average salary value over different states considering all job titles, job types from a selected job category type in the above
                        drop down button')
                      
                  ),
                  
                  box(which = 'plot',
                      solidHeader = F,
                      width = 12,
                      
                      highchartOutput("high_chart",height = "500px")
                  ),
                  
                  box(width = 100, collapsed = T,solidHeader = F,
                      p(style="text-align: justify;",'The below circular bubble chart or packed bubble chart shows the highest postings job titles based on selected job type and job category from the above drop
                          down button. The size of bubble chart is based on the number of job postings occured on selection of job type and category.
                          On hovering on each bubble, the total number of job postings occured will be displayed. By default an error is shown as soon as the page is opened and the default first values are not being taken and the output is not being obtained for this part because of some issue in library girafe and has updated in version 7.0. But on
                          selection of any values in job type, job category the below graph changes.')
                      
                  ),
                  
                  box(which = 'plot',
                      solidHeader = F,
                      width = 12,
                      
                      girafeOutput("plot", width = "100%")
                      
                  )
                  
                )
        ),
        tabItem(tabName = "trends",
                h1("Trends in the job market"),
                fluidRow(
                  box(title =strong("Information"),width = 12,
                      p(style="text-align: justify;",'The trends page allows the user to know the trend in the job postings through various visualizations that
                        are interactive through selecting a particular area. the most important aspect of this page is to make the user understand that there
                        is a particular time in Australia to apply jobs as there are more job listings .'),
                      p(style="text-align: justify;",'If we analyse the listings carefully we get to know that jobs are mostly posted in the period between April and 
                        November.October is the peak time to post and Apply for jobs comparitively as the peak value is high.Through research I have observed that due
                        to the christmas season there are many job openings that are beneficial to the user'))
                ),
                fluidRow(
                  box(title =HTML("<font color='#800080', size = '5px'>Select a Region</font>"),icon=icon('home'),
                      solidHeader = F,
                      width = 4,
                      collapsible = T,
                      selectInput(
                        label = '',
                        inputId = "place1", 
                        choices = states
                      )),
                  box(which = 'plot',
                      solidHeader = F,
                      width = 8,
                      plotlyOutput("time_graph"),
                      p(style="text-align: justify;", 'The above line chart shows the user the trend in the job postings across various regions in Australia.')
                  )
                ),
                fluidRow(
                  box( which='plot',lty='solid',
                       width = 12,
                       plotlyOutput("pie_output"),
                       p(style="text-align: justify;", 'The above pie chart shows the user the distribution of job type postings in a particular area
                         over the year. This visualization is mainly helpful in making the user understand the magnitude and the frequency of job postings across various 
                         regions in Australia.')) 
                  
                )
             )
        
    )
    )
  )

    
############################### Server side input of the R application #####################################################

server <- function(input, output){
  
  
  
  
  grouped_data_ACT = grouped_data[grouped_data$state == "ACT", ]
  grouped_data_NSW = grouped_data[grouped_data$state == "NSW", ]
  grouped_data_NT = grouped_data[grouped_data$state == "NT", ]
  grouped_data_QLD = grouped_data[grouped_data$state == "QLD", ]
  grouped_data_SA = grouped_data[grouped_data$state == "SA", ]
  grouped_data_TAS = grouped_data[grouped_data$state == "TAS", ]
  grouped_data_VIC = grouped_data[grouped_data$state == "VIC", ]
  grouped_data_WA = grouped_data[grouped_data$state == "WA", ]
  
  map1 <- leaflet(data = grouped_data_ACT) %>% addTiles() %>%
    addCircleMarkers(~long, ~lat, radius = ~sqrt(total)*0.5,popup = ~as.character(state))  %>%
    setView(lng = 133.7751, lat = -25.2744, zoom = 4)
  
  map2 <- leaflet(data = grouped_data_NSW) %>% addTiles() %>%
    addCircleMarkers(~long, ~lat, radius = ~sqrt(total)*0.5,popup = ~as.character(state))  %>%
    setView(lng = 133.7751, lat = -25.2744, zoom = 4)
  
  map3 <- leaflet(data = grouped_data_NT) %>% addTiles() %>%
    addCircleMarkers(~long, ~lat, radius = ~sqrt(total)*0.5,popup = ~as.character(state))  %>%
    setView(lng = 133.7751, lat = -25.2744, zoom = 4)
  
  map4 <- leaflet(data = grouped_data_QLD) %>% addTiles() %>%
    addCircleMarkers(~long, ~lat, radius = ~sqrt(total)*0.5,popup = ~as.character(state))  %>%
    setView(lng = 133.7751, lat = -25.2744, zoom = 4)
  
  map5 <- leaflet(data = grouped_data_SA) %>% addTiles() %>%
    addCircleMarkers(~long, ~lat, radius = ~sqrt(total)*0.5,popup = ~as.character(state))  %>%
    setView(lng = 133.7751, lat = -25.2744, zoom = 4)
  
  map6 <- leaflet(data = grouped_data_TAS) %>% addTiles() %>%
    addCircleMarkers(~long, ~lat, radius = ~sqrt(total)*0.5,popup = ~as.character(state))  %>%
    setView(lng = 133.7751, lat = -25.2744, zoom = 4)
  
  map7 <- leaflet(data = grouped_data_VIC) %>% addTiles() %>%
    addCircleMarkers(~long, ~lat, radius = ~sqrt(total)*0.5,popup = ~as.character(state))  %>%
    setView(lng = 133.7751, lat = -25.2744, zoom = 4)
  
  map8 <- leaflet(data = grouped_data_WA) %>% addTiles() %>%
    addCircleMarkers(~long, ~lat, radius = ~sqrt(total)*0.5,popup = ~as.character(state))  %>%
    setView(lng = 133.7751, lat = -25.2744, zoom = 4)
  
  
  
#Map Leaflet output
  
  output$placesMap <- renderLeaflet({
    
    if((input$place)== 'Australian Capital territory(ACT)') 
    {
      
      map1
      
    }
    else if((input$place)== 'New South Wales(NSW)')
    {
      
      map2
      
    }
    else if((input$place)== 'Northern Territory(NT)')
    {
      
      map3
      
    }
    else if((input$place)== 'Queensland(QLD)')
    {
      
      map4
      
    }
    else if((input$place)== 'Southern Australia(SA)')
    {
      
      map5
      
    }
    else if((input$place)== 'Tasmania(TAS)')
    {
     
      map6
      
    }
    else if((input$place)== 'Victoria(VIC)')
    {
      
      map7
      
    }
    else if((input$place)== 'Western Australia(WA)')
    {
      
      map8
      
    } 
    
    
  })
  
###################### donut chart #############################
  
  
  nsw_donut_chart <- nswdata %>% plot_ly(labels = ~job_type, values = ~count)
  nsw_donut_chart <- nsw_donut_chart %>% add_pie(hole = 0.6)
  nsw_donut_chart <- nsw_donut_chart %>% layout(title = "Donut chart showing the division of job types in NSW",  showlegend = F,
                        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  
  
  nsw_donut_chart
  formulaText <- reactive({
    
    paste("nsw_donut_chart ~", input$place)
    
  })
  
  vic_donut_chart <- vicdata %>% plot_ly(labels = ~job_type, values = ~count)
  vic_donut_chart <- vic_donut_chart %>% add_pie(hole = 0.6)
  vic_donut_chart <- vic_donut_chart %>% layout(title = "Donut chart showing the division of job types in VIC",  showlegend = F,
                                                xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  
  
  vic_donut_chart
  
  formulaText <- reactive({
    
    paste("vic_donut_chart ~", input$place)
    
  })
  
  act_donut_chart <- actdata %>% plot_ly(labels = ~job_type, values = ~count)
  act_donut_chart <- act_donut_chart %>% add_pie(hole = 0.6)
  act_donut_chart <- act_donut_chart %>% layout(title = "Donut chart showing the division of job types in ACT",  showlegend = F,
                                                xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  
  
  act_donut_chart
  formulaText <- reactive({
    
    paste("act_donut_chart ~", input$place)
    
  })
  
  nt_donut_chart <- ntdata %>% plot_ly(labels = ~job_type, values = ~count)
  nt_donut_chart <- nt_donut_chart %>% add_pie(hole = 0.6)
  nt_donut_chart <- nt_donut_chart %>% layout(title = "Donut chart showing the division of job types in NT",  showlegend = F,
                                                xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  
  
  nt_donut_chart
  formulaText <- reactive({
    
    paste("nt_donut_chart ~", input$place)
    
  })
  
  
  qld_donut_chart <- qlddata %>% plot_ly(labels = ~job_type, values = ~count)
  qld_donut_chart <- qld_donut_chart %>% add_pie(hole = 0.6)
  qld_donut_chart <- qld_donut_chart %>% layout(title = "Donut chart showing the division of job types in QLD",  showlegend = F,
                                              xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                              yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  
  
  qld_donut_chart
  formulaText <- reactive({
    
    paste("qld_donut_chart ~", input$place)
    
  })
  
  sa_donut_chart <- sadata %>% plot_ly(labels = ~job_type, values = ~count)
  sa_donut_chart <- sa_donut_chart %>% add_pie(hole = 0.6)
  sa_donut_chart <- sa_donut_chart %>% layout(title = "Donut chart showing the division of job types in SA",  showlegend = F,
                                              xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                              yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  
  
  sa_donut_chart
  formulaText <- reactive({
    
    paste("sa_donut_chart ~", input$place)
    
  })
  
  tas_donut_chart <- tasdata %>% plot_ly(labels = ~job_type, values = ~count)
  tas_donut_chart <- tas_donut_chart %>% add_pie(hole = 0.6)
  tas_donut_chart <- tas_donut_chart %>% layout(title = "Donut chart showing the division of job types in TAS",  showlegend = F,
                                              xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                              yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  
  
  tas_donut_chart
  formulaText <- reactive({
    
    paste("tas_donut_chart ~", input$place)
    
  })
  
  
  wa_donut_chart <- wadata %>% plot_ly(labels = ~job_type, values = ~count)
  wa_donut_chart <- wa_donut_chart %>% add_pie(hole = 0.6)
  wa_donut_chart <- wa_donut_chart %>% layout(title = "Donut chart showing the division of job types in WA",  showlegend = F,
                                              xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                              yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  
  
  wa_donut_chart
  formulaText <- reactive({
    
    paste("wa_donut_chart ~", input$place)
    
  })
  
  
  output$selected_var <- renderPlotly({
    if(input$place == "Australian Capital territory(ACT)"){
      
      act_donut_chart
      
    }
    else if(input$place == "New South Wales(NSW)")
    {
      nsw_donut_chart
    }
    else if(input$place == "Northern Territory(NT)")
    {
      nt_donut_chart
    }
    else if(input$place == "Queensland(QLD)")
    {
      qld_donut_chart
    }
    else if(input$place == "Southern Australia(SA)")
    {
      sa_donut_chart
    }
    else if(input$place == "Tasmania(TAS)")
    {
      tas_donut_chart
    }
    else if(input$place == "Victoria(VIC)")
    {
      vic_donut_chart
    }
    else if(input$place == "Western Australia(WA)")
    {
      wa_donut_chart
    }
    })
  
  ########code for the dygraph###############
  
  priceTrend <- jobsdata %>% 
    group_by(state,post_month) %>% 
    summarise(count = n_distinct(pageurl))
  
  priceTrend_ACT = priceTrend[priceTrend$state == "ACT", ]
  priceTrend_NSW = priceTrend[priceTrend$state == "NSW", ]
  priceTrend_NT = priceTrend[priceTrend$state == "NT", ]
  priceTrend_QLD = priceTrend[priceTrend$state == "QLD", ]
  priceTrend_SA = priceTrend[priceTrend$state == "SA", ]
  priceTrend_TAS = priceTrend[priceTrend$state == "TAS", ]
  priceTrend_VIC = priceTrend[priceTrend$state == "VIC", ]
  priceTrend_WA = priceTrend[priceTrend$state == "WA", ]
  
  
  df1 = subset(priceTrend_ACT, select = -c(state) )
  df2 = subset(priceTrend_NSW, select = -c(state) )
  df3 = subset(priceTrend_NT, select = -c(state) )
  df4 = subset(priceTrend_QLD, select = -c(state) )
  df5 = subset(priceTrend_SA, select = -c(state) )
  df6 = subset(priceTrend_TAS, select = -c(state) )
  df7 = subset(priceTrend_VIC, select = -c(state) )
  df8 = subset(priceTrend_WA, select = -c(state) )
  
  
  fig1 <- plot_ly(df1, x = ~post_month, y = ~count, type = 'scatter', mode = 'lines+markers')
  fig2 <- plot_ly(df2, x = ~post_month, y = ~count, type = 'scatter', mode = 'lines+markers')
  fig3 <- plot_ly(df3, x = ~post_month, y = ~count, type = 'scatter', mode = 'lines+markers')
  fig4 <- plot_ly(df4, x = ~post_month, y = ~count, type = 'scatter', mode = 'lines+markers')
  fig5 <- plot_ly(df5, x = ~post_month, y = ~count, type = 'scatter', mode = 'lines+markers')
  fig6 <- plot_ly(df6, x = ~post_month, y = ~count, type = 'scatter', mode = 'lines+markers')
  fig7 <- plot_ly(df7, x = ~post_month, y = ~count, type = 'scatter', mode = 'lines+markers')
  fig8 <- plot_ly(df8, x = ~post_month, y = ~count, type = 'scatter', mode = 'lines+markers')
  
  
  formulaText <- reactive({
    
    paste("fig1 ~", input$place1)
    
  })

  
  formulaText <- reactive({
    
    paste("fig2 ~", input$place1)
    
  })
  
  
  formulaText <- reactive({
    
    paste("fig3 ~", input$place1)
    
  })
  
  
  formulaText <- reactive({
    
    paste("fig4 ~", input$place1)
    
  })
  
  
  formulaText <- reactive({
    
    paste("fig5 ~", input$place1)
    
  })
  
  
  formulaText <- reactive({
    
    paste("fig6 ~", input$place1)
    
  })
  
  
  formulaText <- reactive({
    
    paste("fig7 ~", input$place1)
    
  })
  
  
  formulaText <- reactive({
    
    paste("fig8 ~", input$place1)
    
  })
  
  
  
  output$time_graph <- renderPlotly({
    
    if(input$place1 == "Australian Capital territory(ACT)"){
      
      fig1
      
    }
    else if(input$place1 == "New South Wales(NSW)")
    {
      fig2
    }
    else if(input$place1 == "Northern Territory(NT)")
    {
      fig3
    }
    else if(input$place1 == "Queensland(QLD)")
    {
      fig4
    }
    else if(input$place1 == "Southern Australia(SA)")
    {
      fig5
    }
    else if(input$place1 == "Tasmania(TAS)")
    {
      fig6
    }
    else if(input$place1 == "Victoria(VIC)")
    {
      fig7
    }
    else if(input$place1 == "Western Australia(WA)")
    {
      fig8
    }
   
   
  })
 
  
####################### trends #########################
  
  time_data <-jobsdata  %>%
    group_by(post_month,state)  %>%
    summarise(count = n_distinct(pageurl))
  
  t1 <- filter(time_data,state == 'ACT')
  
  t2 <- filter(time_data,state == 'NSW')
  
  t3 <- filter(time_data,state == 'NT')
  
  t4 <- filter(time_data,state == 'QLD')
  
  t5 <- filter(time_data,state == 'SA')
  
  t6 <- filter(time_data,state == 'TAS')
  
  t7 <- filter(time_data,state == 'VIC')
  
  t8 <- filter(time_data,state == 'WA')
  
  
  
  fig_ACT <- plot_ly(t1, labels = ~post_month, values = ~count, type = 'pie')
  fig_ACT <- fig_ACT %>% layout(title = 'Job postings in ACT',
                                xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  
  fig_NSW <- plot_ly(t2, labels = ~post_month, values = ~count, type = 'pie')
  fig_NSW <- fig_NSW %>% layout(title = 'Job postings in NSW',
                                xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  
  
  fig_NT <- plot_ly(t3, labels = ~post_month, values = ~count, type = 'pie')
  fig_NT <- fig_NT %>% layout(title = 'Job postings in NT',
                                xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  
  
  fig_QLD <- plot_ly(t4, labels = ~post_month, values = ~count, type = 'pie')
  fig_QLD <- fig_QLD %>% layout(title = 'Job postings in QLD',
                                xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  
  
  fig_SA <- plot_ly(t5, labels = ~post_month, values = ~count, type = 'pie')
  fig_SA <- fig_SA %>% layout(title = 'Job postings in SA',
                                xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  
  fig_TAS <- plot_ly(t6, labels = ~post_month, values = ~count, type = 'pie')
  fig_TAS <- fig_TAS %>% layout(title = 'Job postings in TAS',
                                xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  
  fig_VIC <- plot_ly(t7, labels = ~post_month, values = ~count, type = 'pie')
  fig_VIC <- fig_VIC %>% layout(title = 'Job postings in VIC',
                                xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  
  
  
  fig_WA <- plot_ly(t8, labels = ~post_month, values = ~count, type = 'pie')
  fig_WA <- fig_WA %>% layout(title = 'Job postings in WA',
                                xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  
  
  formulaText <- reactive({
    
    paste("fig_ACT ~", input$place1)
    
  })
  
  
  formulaText <- reactive({
    
    paste("fig_NSW ~", input$place1)
    
  })
  
  formulaText <- reactive({
    
    paste("fig_NT ~", input$place1)
    
  })
  
  formulaText <- reactive({
    
    paste("fig_QLD ~", input$place1)
    
  })
  
  formulaText <- reactive({
    
    paste("fig_SA ~", input$place1)
    
  })
  
  
  formulaText <- reactive({
    
    paste("fig_TAS ~", input$place1)
    
  })
  
  formulaText <- reactive({
    
    paste("fig_VIC ~", input$place1)
    
  })
  
  formulaText <- reactive({
    
    paste("fig_WA ~", input$place1)
    
  })
  
  output$pie_output <- renderPlotly({
    if(input$place1 == "Australian Capital territory(ACT)"){
      
      fig_ACT
      
    }
    else if(input$place1 == "New South Wales(NSW)")
    {
      fig_NSW
    }
    else if(input$place1 == "Northern Territory(NT)")
    {
      fig_NT
    }
    else if(input$place1 == "Queensland(QLD)")
    {
      fig_QLD
    }
    else if(input$place1 == "Southern Australia(SA)")
    {
      fig_SA
    }
    else if(input$place1 == "Tasmania(TAS)")
    {
      fig_TAS
    }
    else if(input$place1 == "Victoria(VIC)")
    {
      fig_VIC
    }
    else if(input$place == "Western Australia(WA)")
    {
      fig_WA
    }
  })
  
  ##########################################code for page two###########################
  
  job_type_datau = jobsdata[jobsdata$job_type == "Full Time" , ]
  job_type_category_datau = job_type_datau[job_type_datau$category == "Information & Communication Technology" , ]
  job_type_category_data_countu = job_type_category_datau %>%
    group_by(job_title) %>%
    summarize(count = n()) %>%
    arrange(desc(count))
  job_type_category_data_countu = job_type_category_data_countu[1:10, ]
  print(job_type_category_data_countu)
  job_type_category_data_countu$text <- paste("count: ",job_type_category_data_countu$count)
  packingu <- circleProgressiveLayout(job_type_category_data_countu$count, sizetype='area')
  job_type_category_data_countu <- cbind(job_type_category_data_countu, packingu)
  datu.gg <- circleLayoutVertices(packingu, npoints=50)
  
  plot1 <-  ggplot() + 
    geom_polygon_interactive(data = datu.gg, aes(x, y, group = id, fill=id, tooltip = job_type_category_data_countu$text[id], data_id = id), colour = "black", alpha = 0.6) +
    scale_fill_viridis() +
    geom_text(data = job_type_category_data_countu, aes(x, y,label = job_title), size = 3.5, color="black") +
    theme_void() + 
    theme(legend.position="none", plot.margin=unit(c(0,0,0,0),"cm") ) + 
    coord_equal() 
  
  
  
  job_type_data1 = jobsdata[jobsdata$job_type == "Full Time" , ]
  job_type_category_data1 = job_type_data1[job_type_data1$category == "Manufacturing, Transport & Logistics" , ]
  job_type_category_data_count1 = job_type_category_data1 %>%
    group_by(job_title) %>%
    summarize(count = n()) %>%
    arrange(desc(count))
  job_type_category_data_count1 = job_type_category_data_count1[1:10, ]
  print(job_type_category_data_count1)
  job_type_category_data_count1$text <- paste("count: ",job_type_category_data_count1$count)
  packing11 <- circleProgressiveLayout(job_type_category_data_count1$count, sizetype='area')
  job_type_category_data_count1 <- cbind(job_type_category_data_count1, packing11)
  dat11.gg <- circleLayoutVertices(packing11, npoints=50)
  
  plot11 <-  ggplot() + 
    geom_polygon_interactive(data = dat11.gg, aes(x, y, group = id, fill=id, tooltip = job_type_category_data_count1$text[id], data_id = id), colour = "black", alpha = 0.6) +
    scale_fill_viridis() +
    geom_text(data = job_type_category_data_count1, aes(x, y,label = job_title), size = 3.5, color="black") +
    theme_void() + 
    theme(legend.position="none", plot.margin=unit(c(0,0,0,0),"cm") ) + 
    coord_equal()
  
  job_type_data1a = jobsdata[jobsdata$job_type == "Full Time" , ]
  job_type_category_data1a = job_type_data1[job_type_data1a$category == "Administration & Office Support" , ]
  job_type_category_data_count1a = job_type_category_data1a %>%
    group_by(job_title) %>%
    summarize(count = n()) %>%
    arrange(desc(count))
  job_type_category_data_count1a = job_type_category_data_count1a[1:10, ]
  print(job_type_category_data_count1a)
  job_type_category_data_count1$text <- paste("count: ",job_type_category_data_count1a$count)
  packing1a <- circleProgressiveLayout(job_type_category_data_count1a$count, sizetype='area')
  job_type_category_data_count1a <- cbind(job_type_category_data_count1a, packing1a)
  dat11a.gg <- circleLayoutVertices(packing1a, npoints=50)
  plot11a <-  ggplot() + 
    geom_polygon_interactive(data = dat11a.gg, aes(x, y, group = id, fill=id, tooltip = job_type_category_data_count1a$text[id], data_id = id), colour = "black", alpha = 0.6) +
    scale_fill_viridis() +
    geom_text(data = job_type_category_data_count1a, aes(x, y,label = job_title), size = 3.5, color="black") +
    theme_void() + 
    theme(legend.position="none", plot.margin=unit(c(0,0,0,0),"cm") ) + 
    coord_equal()
  
  job_type_data1f = jobsdata[jobsdata$job_type == "Full Time" , ]
  job_type_category_data1f = job_type_data1f[job_type_data1f$category == "Banking & Financial Services" , ]
  job_type_category_data_count1f = job_type_category_data1f %>%
    group_by(job_title) %>%
    summarize(count = n()) %>%
    arrange(desc(count))
  job_type_category_data_count1f = job_type_category_data_count1f[1:10, ]
  print(job_type_category_data_count1f)
  job_type_category_data_count1f$text <- paste("count: ",job_type_category_data_count1f$count)
  packing1f <- circleProgressiveLayout(job_type_category_data_count1f$count, sizetype='area')
  job_type_category_data_count1f <- cbind(job_type_category_data_count1f, packing1f)
  dat11f.gg <- circleLayoutVertices(packing1f, npoints=50)
  
  plot11f <-  ggplot() + 
    geom_polygon_interactive(data = dat11f.gg, aes(x, y, group = id, fill=id, tooltip = job_type_category_data_count1f$text[id], data_id = id), colour = "black", alpha = 0.6) +
    scale_fill_viridis() +
    geom_text(data = job_type_category_data_count1f, aes(x, y,label = job_title), size = 3.5, color="black") +
    theme_void() + 
    theme(legend.position="none", plot.margin=unit(c(0,0,0,0),"cm") ) + 
    coord_equal()
  
  job_type_data1m = jobsdata[jobsdata$job_type == "Full Time" , ]
  job_type_category_data1m = job_type_data1m[job_type_data1m$category == "Healthcare & Medical" , ]
  job_type_category_data_count1m = job_type_category_data1m %>%
    group_by(job_title) %>%
    summarize(count = n()) %>%
    arrange(desc(count))
  job_type_category_data_count1m = job_type_category_data_count1m[1:10, ]
  print(job_type_category_data_count1m)
  job_type_category_data_count1m$text <- paste("count: ",job_type_category_data_count1m$count)
  packing1m <- circleProgressiveLayout(job_type_category_data_count1m$count, sizetype='area')
  job_type_category_data_count1m <- cbind(job_type_category_data_count1m, packing1m)
  dat11m.gg <- circleLayoutVertices(packing1m, npoints=50)
  
  plot11m <-  ggplot() + 
    geom_polygon_interactive(data = dat11m.gg, aes(x, y, group = id, fill=id, tooltip = job_type_category_data_count1m$text[id], data_id = id), colour = "black", alpha = 0.6) +
    scale_fill_viridis() +
    geom_text(data = job_type_category_data_count1m, aes(x, y,label = job_title), size = 3.5, color="black") +
    theme_void() + 
    theme(legend.position="none", plot.margin=unit(c(0,0,0,0),"cm") ) + 
    coord_equal()
  
  job_type_data_casuals = jobsdata[jobsdata$job_type == "Casual/Vacation" , ]
  job_type_category_data_casuals = job_type_data_casuals[job_type_data_casuals$category == "Information & Communication Technology" , ]
  job_type_category_data_count_casuals = job_type_category_data_casuals %>%
    group_by(job_title) %>%
    summarize(count = n()) %>%
    arrange(desc(count))
  job_type_category_data_count_casuals = job_type_category_data_count_casuals[1:10, ]
  print(job_type_category_data_count_casuals)
  job_type_category_data_count_casuals$text <- paste("count: ",job_type_category_data_count_casuals$count)
  packing1s <- circleProgressiveLayout(job_type_category_data_count_casuals$count, sizetype='area')
  job_type_category_data_count_casuals <- cbind(job_type_category_data_count_casuals, packing1s)
  dat1s.gg <- circleLayoutVertices(packing1s, npoints=50)
  
  plot2s <-  ggplot() + 
    geom_polygon_interactive(data = dat1s.gg, aes(x, y, group = id, fill=id, tooltip = job_type_category_data_count_casuals$text[id], data_id = id), colour = "black", alpha = 0.6) +
    scale_fill_viridis() +
    geom_text(data = job_type_category_data_count_casuals, aes(x, y,label = job_title), size = 3.5, color="black") +
    theme_void() + 
    theme(legend.position="none", plot.margin=unit(c(0,0,0,0),"cm") ) + 
    coord_equal()
  
  job_type_data_casual1 = jobsdata[jobsdata$job_type == "Casual/Vacation" , ]
  job_type_category_data_casual1 = job_type_data_casual1[job_type_data_casual1$category == "Manufacturing, Transport & Logistics" , ]
  job_type_category_data_count_casual1 = job_type_category_data_casual1 %>%
    group_by(job_title) %>%
    summarize(count = n()) %>%
    arrange(desc(count))
  job_type_category_data_count_casual1 = job_type_category_data_count_casual1[1:10, ]
  print(job_type_category_data_count_casual1)
  job_type_category_data_count_casual1$text <- paste("count: ",job_type_category_data_count_casual1$count)
  packing12 <- circleProgressiveLayout(job_type_category_data_count_casual1$count, sizetype='area')
  job_type_category_data_count_casual1 <- cbind(job_type_category_data_count_casual1, packing12)
  dat22.gg <- circleLayoutVertices(packing12, npoints=50)
  
  plot22 <-  ggplot() + 
    geom_polygon_interactive(data = dat22.gg, aes(x, y, group = id, fill=id, tooltip = job_type_category_data_count_casual1$text[id], data_id = id), colour = "black", alpha = 0.6) +
    scale_fill_viridis() +
    geom_text(data = job_type_category_data_count_casual1, aes(x, y,label = job_title), size = 3.5, color="black") +
    theme_void() + 
    theme(legend.position="none", plot.margin=unit(c(0,0,0,0),"cm") ) + 
    coord_equal()
  
  job_type_data_casual1c = jobsdata[jobsdata$job_type == "Casual/Vacation" , ]
  job_type_category_data_casual1c = job_type_data_casual1c[job_type_data_casual1c$category == "Administration & Office Support" , ]
  job_type_category_data_count_casual1c = job_type_category_data_casual1c %>%
    group_by(job_title) %>%
    summarize(count = n()) %>%
    arrange(desc(count))
  job_type_category_data_count_casual1c = job_type_category_data_count_casual1c[1:10, ]
  print(job_type_category_data_count_casual1c)
  job_type_category_data_count_casual1c$text <- paste("count: ",job_type_category_data_count_casual1c$count)
  packing12c <- circleProgressiveLayout(job_type_category_data_count_casual1c$count, sizetype='area')
  job_type_category_data_count_casual1c <- cbind(job_type_category_data_count_casual1c, packing12c)
  dat22c.gg <- circleLayoutVertices(packing12c, npoints=50)
  
  plot22c <-  ggplot() + 
    geom_polygon_interactive(data = dat22c.gg, aes(x, y, group = id, fill=id, tooltip = job_type_category_data_count_casual1c$text[id], data_id = id), colour = "black", alpha = 0.6) +
    scale_fill_viridis() +
    geom_text(data = job_type_category_data_count_casual1c, aes(x, y,label = job_title), size = 3.5, color="black") +
    theme_void() + 
    theme(legend.position="none", plot.margin=unit(c(0,0,0,0),"cm") ) + 
    coord_equal()
  
  job_type_data_casual1g = jobsdata[jobsdata$job_type == "Casual/Vacation" , ]
  job_type_category_data_casual1g = job_type_data_casual1g[job_type_data_casual1g$category == "Banking & Financial Services" , ]
  job_type_category_data_count_casual1g = job_type_category_data_casual1g %>%
    group_by(job_title) %>%
    summarize(count = n()) %>%
    arrange(desc(count))
  job_type_category_data_count_casual1g = job_type_category_data_count_casual1g[1:10, ]
  print(job_type_category_data_count_casual1g)
  job_type_category_data_count_casual1g$text <- paste("count: ",job_type_category_data_count_casual1g$count)
  packing12g <- circleProgressiveLayout(job_type_category_data_count_casual1g$count, sizetype='area')
  job_type_category_data_count_casual1g <- cbind(job_type_category_data_count_casual1g, packing12g)
  dat22g.gg <- circleLayoutVertices(packing12g, npoints=50)
  
  plot22g <-  ggplot() + 
    geom_polygon_interactive(data = dat22g.gg, aes(x, y, group = id, fill=id, tooltip = job_type_category_data_count_casual1g$text[id], data_id = id), colour = "black", alpha = 0.6) +
    scale_fill_viridis() +
    geom_text(data = job_type_category_data_count_casual1g, aes(x, y,label = job_title), size = 3.5, color="black") +
    theme_void() + 
    theme(legend.position="none", plot.margin=unit(c(0,0,0,0),"cm") ) + 
    coord_equal()
  
  
  job_type_data_casual1n = jobsdata[jobsdata$job_type == "Casual/Vacation" , ]
  job_type_category_data_casual1n = job_type_data_casual1n[job_type_data_casual1n$category == "Healthcare & Medical" , ]
  job_type_category_data_count_casual1n = job_type_category_data_casual1n %>%
    group_by(job_title) %>%
    summarize(count = n()) %>%
    arrange(desc(count))
  job_type_category_data_count_casual1n = job_type_category_data_count_casual1n[1:10, ]
  print(job_type_category_data_count_casual1n)
  job_type_category_data_count_casual1n$text <- paste("count: ",job_type_category_data_count_casual1n$count)
  packing12n <- circleProgressiveLayout(job_type_category_data_count_casual1n$count, sizetype='area')
  job_type_category_data_count_casual1n <- cbind(job_type_category_data_count_casual1n, packing12n)
  dat22n.gg <- circleLayoutVertices(packing12n, npoints=50)
  
  plot22n <-  ggplot() + 
    geom_polygon_interactive(data = dat22n.gg, aes(x, y, group = id, fill=id, tooltip = job_type_category_data_count_casual1n$text[id], data_id = id), colour = "black", alpha = 0.6) +
    scale_fill_viridis() +
    geom_text(data = job_type_category_data_count_casual1n, aes(x, y,label = job_title), size = 3.5, color="black") +
    theme_void() + 
    theme(legend.position="none", plot.margin=unit(c(0,0,0,0),"cm") ) + 
    coord_equal()
  
  job_type_data_contract = jobsdata[jobsdata$job_type == "Contract/Temp" , ]
  job_type_category_data_contract = job_type_data_contract[job_type_data_contract$category == "Information & Communication Technology" , ]
  job_type_category_data_count_contract = job_type_category_data_contract %>%
    group_by(job_title) %>%
    summarize(count = n()) %>%
    arrange(desc(count))
  job_type_category_data_count_contract = job_type_category_data_count_contract[1:10, ]
  print(job_type_category_data_count_contract)
  job_type_category_data_count_contract$text <- paste("count: ",job_type_category_data_count_contract$count)
  packing2 <- circleProgressiveLayout(job_type_category_data_count_contract$count, sizetype='area')
  job_type_category_data_count_contract <- cbind(job_type_category_data_count_contract, packing2)
  dat2.gg <- circleLayoutVertices(packing2, npoints=50)
  
  plot3 <-  ggplot() + 
    geom_polygon_interactive(data = dat2.gg, aes(x, y, group = id, fill=id, tooltip = job_type_category_data_count_contract$text[id], data_id = id), colour = "black", alpha = 0.6) +
    scale_fill_viridis() +
    geom_text(data = job_type_category_data_count_contract, aes(x, y,label = job_title), size = 3.5, color="black") +
    theme_void() + 
    theme(legend.position="none", plot.margin=unit(c(0,0,0,0),"cm") ) + 
    coord_equal()
  
  job_type_data_contract1 = jobsdata[jobsdata$job_type == "Contract/Temp" , ]
  job_type_category_data_contract1 = job_type_data_contract1[job_type_data_contract1$category == "Manufacturing, Transport & Logistics" , ]
  job_type_category_data_count_contract1 = job_type_category_data_contract1 %>%
    group_by(job_title) %>%
    summarize(count = n()) %>%
    arrange(desc(count))
  job_type_category_data_count_contract1 = job_type_category_data_count_contract1[1:10, ]
  print(job_type_category_data_count_contract1)
  job_type_category_data_count_contract1$text <- paste("count: ",job_type_category_data_count_contract1$count)
  packing21 <- circleProgressiveLayout(job_type_category_data_count_contract1$count, sizetype='area')
  job_type_category_data_count_contract1 <- cbind(job_type_category_data_count_contract1, packing21)
  dat23.gg <- circleLayoutVertices(packing21, npoints=50)
  
  plot32 <-  ggplot() + 
    geom_polygon_interactive(data = dat23.gg, aes(x, y, group = id, fill=id, tooltip = job_type_category_data_count_contract1$text[id], data_id = id), colour = "black", alpha = 0.6) +
    scale_fill_viridis() +
    geom_text(data = job_type_category_data_count_contract1, aes(x, y,label = job_title), size = 3.5, color="black") +
    theme_void() + 
    theme(legend.position="none", plot.margin=unit(c(0,0,0,0),"cm") ) + 
    coord_equal()
  
  job_type_data_contract1b = jobsdata[jobsdata$job_type == "Contract/Temp" , ]
  job_type_category_data_contract1b = job_type_data_contract1[job_type_data_contract1b$category == "Administration & Office Support" , ]
  job_type_category_data_count_contract1b = job_type_category_data_contract1b %>%
    group_by(job_title) %>%
    summarize(count = n()) %>%
    arrange(desc(count))
  job_type_category_data_count_contract1b = job_type_category_data_count_contract1b[1:10, ]
  print(job_type_category_data_count_contract1b)
  job_type_category_data_count_contract1b$text <- paste("count: ",job_type_category_data_count_contract1b$count)
  packing21b <- circleProgressiveLayout(job_type_category_data_count_contract1b$count, sizetype='area')
  job_type_category_data_count_contract1b <- cbind(job_type_category_data_count_contract1b, packing21b)
  dat23b.gg <- circleLayoutVertices(packing21b, npoints=50)
  
  plot32b <-  ggplot() + 
    geom_polygon_interactive(data = dat23b.gg, aes(x, y, group = id, fill=id, tooltip = job_type_category_data_count_contract1b$text[id], data_id = id), colour = "black", alpha = 0.6) +
    scale_fill_viridis() +
    geom_text(data = job_type_category_data_count_contract1b, aes(x, y,label = job_title), size = 3.5, color="black") +
    theme_void() + 
    theme(legend.position="none", plot.margin=unit(c(0,0,0,0),"cm") ) + 
    coord_equal()
  
  job_type_data_contract1h = jobsdata[jobsdata$job_type == "Contract/Temp" , ]
  job_type_category_data_contract1h = job_type_data_contract1h[job_type_data_contract1h$category == "Banking & Financial Services" , ]
  job_type_category_data_count_contract1h = job_type_category_data_contract1h %>%
    group_by(job_title) %>%
    summarize(count = n()) %>%
    arrange(desc(count))
  job_type_category_data_count_contract1h = job_type_category_data_count_contract1h[1:10, ]
  print(job_type_category_data_count_contract1h)
  job_type_category_data_count_contract1h$text <- paste("count: ",job_type_category_data_count_contract1h$count)
  packing21h <- circleProgressiveLayout(job_type_category_data_count_contract1h$count, sizetype='area')
  job_type_category_data_count_contract1h <- cbind(job_type_category_data_count_contract1h, packing21h)
  dat23h.gg <- circleLayoutVertices(packing21h, npoints=50)
  
  plot32h <-  ggplot() + 
    geom_polygon_interactive(data = dat23h.gg, aes(x, y, group = id, fill=id, tooltip = job_type_category_data_count_contract1h$text[id], data_id = id), colour = "black", alpha = 0.6) +
    scale_fill_viridis() +
    geom_text(data = job_type_category_data_count_contract1h, aes(x, y,label = job_title), size = 3.5, color="black") +
    theme_void() + 
    theme(legend.position="none", plot.margin=unit(c(0,0,0,0),"cm") ) + 
    coord_equal()
  
  job_type_data_contract1p = jobsdata[jobsdata$job_type == "Contract/Temp" , ]
  job_type_category_data_contract1p = job_type_data_contract1p[job_type_data_contract1p$category == "Healthcare & Medical" , ]
  job_type_category_data_count_contract1p = job_type_category_data_contract1p %>%
    group_by(job_title) %>%
    summarize(count = n()) %>%
    arrange(desc(count))
  job_type_category_data_count_contract1p = job_type_category_data_count_contract1p[1:10, ]
  print(job_type_category_data_count_contract1p)
  job_type_category_data_count_contract1p$text <- paste("count: ",job_type_category_data_count_contract1p$count)
  packing21p <- circleProgressiveLayout(job_type_category_data_count_contract1p$count, sizetype='area')
  job_type_category_data_count_contract1p <- cbind(job_type_category_data_count_contract1p, packing21p)
  dat23p.gg <- circleLayoutVertices(packing21p, npoints=50)
  
  plot32p <-  ggplot() + 
    geom_polygon_interactive(data = dat23p.gg, aes(x, y, group = id, fill=id, tooltip = job_type_category_data_count_contract1p$text[id], data_id = id), colour = "black", alpha = 0.6) +
    scale_fill_viridis() +
    geom_text(data = job_type_category_data_count_contract1p, aes(x, y,label = job_title), size = 3.5, color="black") +
    theme_void() + 
    theme(legend.position="none", plot.margin=unit(c(0,0,0,0),"cm") ) + 
    coord_equal()
  
  job_type_data_parttime = jobsdata[jobsdata$job_type == "Part Time" , ]
  job_type_category_data_parttime = job_type_data_contract[job_type_data_parttime$category == "Information & Communication Technology" , ]
  job_type_category_data_count_parttime = job_type_category_data_parttime %>%
    group_by(job_title) %>%
    summarize(count = n()) %>%
    arrange(desc(count))
  job_type_category_data_count_parttime = job_type_category_data_count_parttime[1:10, ]
  print(job_type_category_data_count_parttime)
  job_type_category_data_count_parttime$text <- paste("count: ",job_type_category_data_count_parttime$count)
  packing3 <- circleProgressiveLayout(job_type_category_data_count_parttime$count, sizetype='area')
  job_type_category_data_count_parttime <- cbind(job_type_category_data_count_parttime, packing3)
  dat2.gg <- circleLayoutVertices(packing3, npoints=50)
  
  plot4 <-  ggplot() + 
    geom_polygon_interactive(data = dat2.gg, aes(x, y, group = id, fill=id, tooltip = job_type_category_data_count_parttime$text[id], data_id = id), colour = "black", alpha = 0.6) +
    scale_fill_viridis() +
    geom_text(data = job_type_category_data_count_parttime, aes(x, y,label = job_title), size = 3.5, color="black") +
    theme_void() + 
    theme(legend.position="none", plot.margin=unit(c(0,0,0,0),"cm") ) + 
    coord_equal()
  
  job_type_data_parttime1 = jobsdata[jobsdata$job_type == "Part Time" , ]
  job_type_category_data_parttime1 = job_type_data_parttime1 [job_type_data_parttime1$category == "Manufacturing, Transport & Logistics" , ]
  job_type_category_data_count_parttime1 = job_type_category_data_parttime1 %>%
    group_by(job_title) %>%
    summarize(count = n()) %>%
    arrange(desc(count))
  job_type_category_data_count_parttime1 = job_type_category_data_count_parttime1[1:10, ]
  print(job_type_category_data_count_parttime1)
  job_type_category_data_count_parttime1$text <- paste("count: ",job_type_category_data_count_parttime1$count)
  packing33 <- circleProgressiveLayout(job_type_category_data_count_parttime1$count, sizetype='area')
  job_type_category_data_count_parttime1 <- cbind(job_type_category_data_count_parttime1, packing33)
  dat32.gg <- circleLayoutVertices(packing33, npoints=50)
  
  plot42 <-  ggplot() + 
    geom_polygon_interactive(data = dat32.gg, aes(x, y, group = id, fill=id, tooltip = job_type_category_data_count_parttime1$text[id], data_id = id), colour = "black", alpha = 0.6) +
    scale_fill_viridis() +
    geom_text(data = job_type_category_data_count_parttime1, aes(x, y,label = job_title), size = 3.5, color="black") +
    theme_void() + 
    theme(legend.position="none", plot.margin=unit(c(0,0,0,0),"cm") ) + 
    coord_equal()
  
  job_type_data_parttime1d = jobsdata[jobsdata$job_type == "Part Time" , ]
  job_type_category_data_parttime1d = job_type_data_parttime1d [job_type_data_parttime1$category == "Administration & Office Support" , ]
  job_type_category_data_count_parttime1d = job_type_category_data_parttime1d %>%
    group_by(job_title) %>%
    summarize(count = n()) %>%
    arrange(desc(count))
  job_type_category_data_count_parttime1d = job_type_category_data_count_parttime1d[1:10, ]
  print(job_type_category_data_count_parttime1d)
  job_type_category_data_count_parttime1d$text <- paste("count: ",job_type_category_data_count_parttime1d$count)
  packing33d <- circleProgressiveLayout(job_type_category_data_count_parttime1d$count, sizetype='area')
  job_type_category_data_count_parttime1d <- cbind(job_type_category_data_count_parttime1d, packing33d)
  dat32d.gg <- circleLayoutVertices(packing33d, npoints=50)
  
  plot42d <-  ggplot() + 
    geom_polygon_interactive(data = dat32d.gg, aes(x, y, group = id, fill=id, tooltip = job_type_category_data_count_parttime1d$text[id], data_id = id), colour = "black", alpha = 0.6) +
    scale_fill_viridis() +
    geom_text(data = job_type_category_data_count_parttime1d, aes(x, y,label = job_title), size = 3.5, color="black") +
    theme_void() + 
    theme(legend.position="none", plot.margin=unit(c(0,0,0,0),"cm") ) + 
    coord_equal()
  
  job_type_data_parttime1i = jobsdata[jobsdata$job_type == "Part Time" , ]
  job_type_category_data_parttime1i = job_type_data_parttime1i [job_type_data_parttime1i$category == "Banking & Financial Services" , ]
  job_type_category_data_count_parttime1i = job_type_category_data_parttime1i %>%
    group_by(job_title) %>%
    summarize(count = n()) %>%
    arrange(desc(count))
  job_type_category_data_count_parttime1i = job_type_category_data_count_parttime1i[1:10, ]
  print(job_type_category_data_count_parttime1i)
  job_type_category_data_count_parttime1i$text <- paste("count: ",job_type_category_data_count_parttime1i$count)
  packing33i <- circleProgressiveLayout(job_type_category_data_count_parttime1i$count, sizetype='area')
  job_type_category_data_count_parttime1i <- cbind(job_type_category_data_count_parttime1i, packing33i)
  dat32i.gg <- circleLayoutVertices(packing33i, npoints=50)
  
  plot42i <-  ggplot() + 
    geom_polygon_interactive(data = dat32i.gg, aes(x, y, group = id, fill=id, tooltip = job_type_category_data_count_parttime1i$text[id], data_id = id), colour = "black", alpha = 0.6) +
    scale_fill_viridis() +
    geom_text(data = job_type_category_data_count_parttime1i, aes(x, y,label = job_title), size = 3.5, color="black") +
    theme_void() + 
    theme(legend.position="none", plot.margin=unit(c(0,0,0,0),"cm") ) + 
    coord_equal()
  
  
  job_type_data_parttime1q = jobsdata[jobsdata$job_type == "Part Time" , ]
  job_type_category_data_parttime1q = job_type_data_parttime1q[job_type_data_parttime1q$category == "Healthcare & Medical" , ]
  job_type_category_data_count_parttime1q = job_type_category_data_parttime1q %>%
    group_by(job_title) %>%
    summarize(count = n()) %>%
    arrange(desc(count))
  job_type_category_data_count_parttime1q = job_type_category_data_count_parttime1q[1:10, ]
  print(job_type_category_data_count_parttime1q)
  job_type_category_data_count_parttime1q$text <- paste("count: ",job_type_category_data_count_parttime1q$count)
  packing33q <- circleProgressiveLayout(job_type_category_data_count_parttime1q$count, sizetype='area')
  job_type_category_data_count_parttime1q <- cbind(job_type_category_data_count_parttime1q, packing33q)
  dat32q.gg <- circleLayoutVertices(packing33q, npoints=50)
  
  plot42q <-  ggplot() + 
    geom_polygon_interactive(data = dat32q.gg, aes(x, y, group = id, fill=id, tooltip = job_type_category_data_count_parttime1q$text[id], data_id = id), colour = "black", alpha = 0.6) +
    scale_fill_viridis() +
    geom_text(data = job_type_category_data_count_parttime1q, aes(x, y,label = job_title), size = 3.5, color="black") +
    theme_void() + 
    theme(legend.position="none", plot.margin=unit(c(0,0,0,0),"cm") ) + 
    coord_equal()
  
  
  job_type_category_data_all = jobsdata[jobsdata$category == "Information & Communication Technology" , ]
  job_type_category_data_count_all = job_type_category_data_all %>%
    group_by(job_title) %>%
    summarize(count = n()) %>%
    arrange(desc(count))
  job_type_category_data_count_all = job_type_category_data_count_all[1:10, ]
  print(job_type_category_data_count_all)
  job_type_category_data_count_all$text <- paste("count: ",job_type_category_data_count_all$count)
  packing4 <- circleProgressiveLayout(job_type_category_data_count_all$count, sizetype='area')
  job_type_category_data_count_all <- cbind(job_type_category_data_count_all, packing4)
  dat3.gg <- circleLayoutVertices(packing4, npoints=50)
  
  plot5 <-  ggplot() + 
    geom_polygon_interactive(data = dat3.gg, aes(x, y, group = id, fill=id, tooltip = job_type_category_data_count_all$text[id], data_id = id), colour = "black", alpha = 0.6) +
    scale_fill_viridis() +
    geom_text(data = job_type_category_data_count_all, aes(x, y,label = job_title), size = 3.5, color="black") +
    theme_void() + 
    theme(legend.position="none", plot.margin=unit(c(0,0,0,0),"cm") ) + 
    coord_equal()
  
  
  job_type_category_data_all1 = jobsdata[jobsdata$category == "Manufacturing, Transport & Logistics" , ]
  job_type_category_data_count_all1 = job_type_category_data_all1 %>%
    group_by(job_title) %>%
    summarize(count = n()) %>%
    arrange(desc(count))
  job_type_category_data_count_all1 = job_type_category_data_count_all1[1:10, ]
  print(job_type_category_data_count_all1)
  job_type_category_data_count_all1$text <- paste("count: ",job_type_category_data_count_all1$count)
  packing44 <- circleProgressiveLayout(job_type_category_data_count_all1$count, sizetype='area')
  job_type_category_data_count_all1 <- cbind(job_type_category_data_count_all1, packing44)
  dat34.gg <- circleLayoutVertices(packing44, npoints=50)
  
  plot52 <-  ggplot() + 
    geom_polygon_interactive(data = dat34.gg, aes(x, y, group = id, fill=id, tooltip = job_type_category_data_count_all1$text[id], data_id = id), colour = "black", alpha = 0.6) +
    scale_fill_viridis() +
    geom_text(data = job_type_category_data_count_all1, aes(x, y,label = job_title), size = 3.5, color="black") +
    theme_void() + 
    theme(legend.position="none", plot.margin=unit(c(0,0,0,0),"cm") ) + 
    coord_equal()
  
  
  job_type_category_data_all1e = jobsdata[jobsdata$category == "Administration & Office Support" , ]
  job_type_category_data_count_all1e = job_type_category_data_all1e %>%
    group_by(job_title) %>%
    summarize(count = n()) %>%
    arrange(desc(count))
  job_type_category_data_count_all1e = job_type_category_data_count_all1e[1:10, ]
  print(job_type_category_data_count_all1e)
  job_type_category_data_count_all1e$text <- paste("count: ",job_type_category_data_count_all1e$count)
  packing44e <- circleProgressiveLayout(job_type_category_data_count_all1e$count, sizetype='area')
  job_type_category_data_count_all1e <- cbind(job_type_category_data_count_all1e, packing44e)
  dat34e.gg <- circleLayoutVertices(packing44e, npoints=50)
  
  plot52e <-  ggplot() + 
    geom_polygon_interactive(data = dat34e.gg, aes(x, y, group = id, fill=id, tooltip = job_type_category_data_count_all1e$text[id], data_id = id), colour = "black", alpha = 0.6) +
    scale_fill_viridis() +
    geom_text(data = job_type_category_data_count_all1e, aes(x, y,label = job_title), size = 3.5, color="black") +
    theme_void() + 
    theme(legend.position="none", plot.margin=unit(c(0,0,0,0),"cm") ) + 
    coord_equal()
  
  
  job_type_category_data_all1j = jobsdata[jobsdata$category == "Banking & Financial Services" , ]
  job_type_category_data_count_all1j = job_type_category_data_all1j %>%
    group_by(job_title) %>%
    summarize(count = n()) %>%
    arrange(desc(count))
  job_type_category_data_count_all1j = job_type_category_data_count_all1j[1:10, ]
  print(job_type_category_data_count_all1j)
  job_type_category_data_count_all1j$text <- paste("count: ",job_type_category_data_count_all1j$count)
  packing44j <- circleProgressiveLayout(job_type_category_data_count_all1j$count, sizetype='area')
  job_type_category_data_count_all1j <- cbind(job_type_category_data_count_all1j, packing44j)
  dat34j.gg <- circleLayoutVertices(packing44j, npoints=50)
  
  plot52j <-  ggplot() + 
    geom_polygon_interactive(data = dat34j.gg, aes(x, y, group = id, fill=id, tooltip = job_type_category_data_count_all1j$text[id], data_id = id), colour = "black", alpha = 0.6) +
    scale_fill_viridis() +
    geom_text(data = job_type_category_data_count_all1j, aes(x, y,label = job_title), size = 3.5, color="black") +
    theme_void() + 
    theme(legend.position="none", plot.margin=unit(c(0,0,0,0),"cm") ) + 
    coord_equal()
  
  job_type_category_data_all1r = jobsdata[jobsdata$category == "Healthcare & Medical" , ]
  job_type_category_data_count_all1r = job_type_category_data_all1r %>%
    group_by(job_title) %>%
    summarize(count = n()) %>%
    arrange(desc(count))
  job_type_category_data_count_all1r = job_type_category_data_count_all1r[1:10, ]
  print(job_type_category_data_count_all1r)
  job_type_category_data_count_all1r$text <- paste("count: ",job_type_category_data_count_all1r$count)
  packing44r <- circleProgressiveLayout(job_type_category_data_count_all1r$count, sizetype='area')
  job_type_category_data_count_all1r <- cbind(job_type_category_data_count_all1r, packing44r)
  dat34r.gg <- circleLayoutVertices(packing44r, npoints=50)
  
  plot52r <-  ggplot() + 
    geom_polygon_interactive(data = dat34r.gg, aes(x, y, group = id, fill=id, tooltip = job_type_category_data_count_all1r$text[id], data_id = id), colour = "black", alpha = 0.6) +
    scale_fill_viridis() +
    geom_text(data = job_type_category_data_count_all1r, aes(x, y,label = job_title), size = 3.5, color="black") +
    theme_void() + 
    theme(legend.position="none", plot.margin=unit(c(0,0,0,0),"cm") ) + 
    coord_equal()
  
  output$plot <- renderGirafe({
    
    if((input$type == "Full Time") & (input$category == "Information & Communication Technology")){
      
      widg <- girafe(ggobj = plot1)
      widg
    }
    
    else if((input$type == "Full Time") & (input$category == "Manufacturing, Transport & Logistics")){
      widg1 <- girafe(ggobj = plot11)
      widg1
    }
    
    else if((input$type == "Full Time") & (input$category == "Administration & Office Support")){
      widg2 <- girafe(ggobj = plot11a)
      widg2
    }
    
    else if((input$type == "Full Time") & (input$category == "Banking & Financial Services")){
      widg3 <- girafe(ggobj = plot11f)
      widg3
      
    }
    
    else if((input$type == "Full Time") & (input$category == "Healthcare & Medical")){
      widg4 <- girafe(ggobj = plot11m)
      widg4
    }
    
    else if((input$type == "Casual/Vacation") & (input$category == "Information & Communication Technology")){
      widg5 <- girafe(ggobj = plot2s)
      widg5
      
    }
    
    else if((input$type == "Casual/Vacation") & (input$category == "Manufacturing, Transport & Logistics")){
      widg6 <- girafe(ggobj = plot22)
      widg6
    }
    
    else if((input$type == "Casual/Vacation") & (input$category == "Administration & Office Support")){
      widg7 <- girafe(ggobj = plot22c)
      widg7
    }
    
    else if((input$type == "Casual/Vacation") & (input$category == "Banking & Financial Services")){
      widg8 <- girafe(ggobj = plot22g)
      widg8
      
    }
    
    else if((input$type == "Casual/Vacation") & (input$category == "Healthcare & Medical")){
      widg8 <- girafe(ggobj = plot22n)
      widg8
      
    }
    
    else if((input$type == "Contract/Temp") & (input$category == "Information & Communication Technology")){
      widg8 <- girafe(ggobj = plot3)
      widg8
      
    }
    
    else if((input$type == "Contract/Temp") & (input$category == "Manufacturing, Transport & Logistics")){
      widg8 <- girafe(ggobj = plot32)
      widg8
      
    }
    
    else if((input$type == "Contract/Temp") & (input$category == "Administration & Office Support")){
      widg8 <- girafe(ggobj = plot32b)
      widg8
      
    }
    
    else if((input$type == "Contract/Temp") & (input$category == "Banking & Financial Services")){
      widg8 <- girafe(ggobj = plot32h)
      widg8
      
    }
    
    else if((input$type == "Contract/Temp") & (input$category == "Healthcare & Medical")){
      widg8 <- girafe(ggobj = plot32p)
      widg8
      
    }
    
    else if ((input$type == "Part Time") & (input$category == "Information & Communication Technology")){
      widg8 <- girafe(ggobj = plot4)
      widg8
      
    } 
    
    else if((input$type == "Part Time") & (input$category == "Manufacturing, Transport & Logistics")){
      widg8 <- girafe(ggobj = plot42)
      widg8
      
    }
    
    else if((input$type == "Part Time") & (input$category == "Administration & Office Support")){
      widg8 <- girafe(ggobj = plot42d)
      widg8
      
    }
    
    else if((input$type == "Part Time") & (input$category == "Banking & Financial Services")){
      widg8 <- girafe(ggobj = plot42i)
      widg8
      plot42i
    }
    
    else if((input$type == "Part Time") & (input$category == "Healthcare & Medical")){
      widg8 <- girafe(ggobj = plot42q)
      widg8
      
    }
    
    else if ((input$type == "All") & (input$category == "Information & Communication Technology")){
      widg8 <- girafe(ggobj = plot5)
      widg8
      
    } 
    
    else if ((input$type == "All") & (input$category == "Manufacturing, Transport & Logistics")){
      widg8 <- girafe(ggobj = plot52)
      widg8
      
    }
    
    else if ((input$type == "All") & (input$category == "Administration & Office Support")){
      widg8 <- girafe(ggobj = plot52e)
      widg8
      
    }
    
    else if ((input$type == "All") & (input$category == "Banking & Financial Services")){
      widg8 <- girafe(ggobj = plot52j)
      widg8
      
    }
    
    else if ((input$type == "All") & (input$category == "Healthcare & Medical")){
      widg8 <- girafe(ggobj = plot52r)
      widg8
      
    }
    
  })
  
  job_type_category_data_it = job_type_category_data_all %>%
    group_by(state)%>%
    summarize(avg_salaries = round(mean(as.numeric(Mean_Salary)),2))
  
  job_type_category_data_man = job_type_category_data_all1 %>%
    group_by(state)%>%
    summarize(avg_salaries = round(mean(as.numeric(Mean_Salary)),2))
  
  job_type_category_data_adm = job_type_category_data_all1e %>%
    group_by(state)%>%
    summarize(avg_salaries = round(mean(as.numeric(Mean_Salary)),2))
  
  job_type_category_data_ban = job_type_category_data_all1j %>%
    group_by(state)%>%
    summarize(avg_salaries = round(mean(as.numeric(Mean_Salary)),2))
  
  job_type_category_data_med = job_type_category_data_all1r %>%
    group_by(state)%>%
    summarize(avg_salaries = round(mean(as.numeric(Mean_Salary)),2))
  
  highchartit <- highchart() %>% 
    hc_title(text = "The bar chart showing the average salaries count over different states of different job titles of all job types in Information Technology") %>%
    hc_subtitle(text = paste0("Average Salaries are shown in dollars")) %>% 
    
    hc_add_series(job_type_category_data_it$avg_salaries, type = "column",name = " ", colorByPoint = FALSE,center = c('35%', '10%'),
                  size = 50,tooltip = list(pointFormat = "average salaries: {point.y}")) %>%
    hc_yAxis(title = list(text = 'Average Salaries Count'), categories = job_type_category_data_it$avg_salaries) %>% 
    hc_xAxis(title = list(text = 'States'),categories = job_type_category_data_it$state)
  
  highchartman <- highchart() %>% 
    hc_title(text = "The bar chart showing the average salaries count over different states of different job titles of all job types in Manufacturing, Transport & Logistics ") %>%
    hc_subtitle(text = paste0("Average Salaries are shown in dollars")) %>% 
    
    hc_add_series(job_type_category_data_man$avg_salaries, type = "column",name = " ", colorByPoint = FALSE,center = c('35%', '10%'),
                  size = 50,tooltip = list(pointFormat = "average salaries: {point.y}")) %>%
    hc_yAxis(title = list(text = 'Average Salaries Count'), categories = job_type_category_data_man$avg_salaries) %>% 
    hc_xAxis(title = list(text = 'States'),categories = job_type_category_data_man$state)
  
  highchartadm <- highchart() %>% 
    hc_title(text = "The bar chart showing the average salaries count over different states of different job titles of all job types in Administration & Office Support") %>%
    hc_subtitle(text = paste0("Average Salaries are shown in dollars")) %>% 
    
    hc_add_series(job_type_category_data_adm$avg_salaries, type = "column",name = " ", colorByPoint = FALSE,center = c('35%', '10%'),
                  size = 50,tooltip = list(pointFormat = "average salaries: {point.y}")) %>%
    hc_yAxis(title = list(text = 'Average Salaries Count'), categories = job_type_category_data_adm$avg_salaries) %>% 
    hc_xAxis(title = list(text = 'States'),categories = job_type_category_data_adm$state)
  
  highchartban <- highchart() %>% 
    hc_title(text = "The bar chart showing the average salaries count over different states of different job titles of all job types in Banking & Financial Services") %>%
    hc_subtitle(text = paste0("Average Salaries are shown in dollars")) %>% 
    
    hc_add_series(job_type_category_data_ban$avg_salaries, type = "column",name = " ", colorByPoint = FALSE,center = c('35%', '10%'),
                  size = 50,tooltip = list(pointFormat = "average salaries: {point.y}")) %>%
    hc_yAxis(title = list(text = 'Average Salaries Count'), categories = job_type_category_data_ban$avg_salaries) %>% 
    hc_xAxis(title = list(text = 'States'),categories = job_type_category_data_ban$state)
  
  highchartmed <- highchart() %>% 
    hc_title(text = "The bar chart showing the average salaries count over different states of different job titles of all job types in Healthcare & Medical") %>%
    hc_subtitle(text = paste0("Average Salaries are shown in dollars")) %>% 
    
    hc_add_series(job_type_category_data_med$avg_salaries, type = "column",name = " ", colorByPoint = FALSE,center = c('35%', '10%'),
                  size = 50,tooltip = list(pointFormat = "average salaries: {point.y}")) %>%
    hc_yAxis(title = list(text = 'Average Salaries Count'), categories = job_type_category_data_med$avg_salaries) %>% 
    hc_xAxis(title = list(text = 'States'),categories = job_type_category_data_med$state)
  
  output$high_chart <- renderHighchart({
    
    if(input$category == "Information & Communication Technology"){
      
      highchartit
      
    }
    else if(input$category == "Manufacturing, Transport & Logistics"){
      
      highchartman
      
    }
    else if(input$category == "Administration & Office Support"){
      
      highchartadm
      
    }
    else if(input$category == "Banking & Financial Services"){
      
      highchartban
      
    }
    else if(input$category == "Healthcare & Medical"){
      
      highchartmed
      
    }
  })
  
  
}

shinyApp(ui, server)