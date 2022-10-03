#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(plotly)
library(ggplot2)
library(ggtext)
library(ggcorrplot)
library(openxlsx)
library(maps)
library(shinycssloaders)

options(scipen=999)
#USA_States <- read.xlsx("US_States_Crimes.xlsx")
USA_States <- read.xlsx("States_Combined.xlsx")
#my_data <- subset(USA_States, select = -c(Area,X4))
my_data <- USA_States
my_data$State <- trimws(my_data$State, which = c("both"))
my_data$Population[my_data$X4 == "Rate per 100,000 inhabitants"] = 100000


c1 <- my_data %>% 
  #select(colnames(my_data[c(5,6,7,8,9,10,11,12,13,14)])) %>%
  select(-"State", -"X1",-"Area",-"X4",-"Year",-"identity",-"US_State") %>% 
  names()

c2 = my_data %>% 
  select(-"State", -"X1",-"Area",-"X4",-"Year",-"identity",-"US_State") %>% 
  names()


state_map = map_data("state")
my_data2 <- my_data %>% 
  mutate(State = tolower(State))
merged = right_join(my_data2, state_map, by = c("State" = "region"))
st= data.frame(abb = state.abb, stname=tolower(state.name), x=state.center$x, y=state.center$y)
new_join = left_join(merged,st, by= c("State"="stname"))
new_join$x[is.na(new_join$x)] = -77.03196
new_join$y[is.na(new_join$y)] = 38.89037
new_join$abb[is.na(new_join$abb)] = "DC"
# Define UI for application that draws a histogram
ui <- fluidPage(
  
  
  dashboardPage(
    dashboardHeader(title = "US Statistics across States from 2010-2019", titleWidth = 650),
    dashboardSidebar(
      sidebarMenu(
      id = "sidebar",
      #First MenuItem
      menuItem("Dataset", tabName = "data", icon = icon("database")),
      menuItem(text = "Visualization", tabName = "viz",icon = icon("chart-line")),
      conditionalPanel("input.sidebar == 'viz' && input.t2 == 'distro'",selectizeInput(inputId = "var1", label = "select the category", choices = c1, selected = "Rape")),
      conditionalPanel("input.sidebar == 'viz' && input.t2 == 'distro'",selectizeInput("State", "Select a State", choices = NULL)),
      conditionalPanel("input.sidebar == 'viz' && input.t2 == 'trends'",selectizeInput(inputId = "var4", label = "select the category", choices = c1, selected = "Rape")),
      conditionalPanel("input.sidebar == 'viz' && input.t2 == 'trends'",selectizeInput("Year1", "Select a Year", choices = NULL)),
      conditionalPanel("input.sidebar == 'viz' && input.t2 == 'Scatters'",selectizeInput("USA_ID", "Select a State", choices = NULL)),
      conditionalPanel("input.sidebar == 'viz' && input.t2 == 'Scatters'",selectizeInput(inputId = "var2", label = "select the Y category", choices = c1, selected = "Population")),
      conditionalPanel("input.sidebar == 'viz' && input.t2 == 'Scatters'",selectizeInput(inputId = "var3", label = "select the X category", choices = c1, selected = "Rape")),
      menuItem(text = "Choropleth Map", tabName = "map", icon = icon("map"))
      
      )
    ),
    dashboardBody(
      tabItems(
        #tabitem
        tabItem(tabName = "data",
                tabBox(id = "t1", width = 12,
                       tabPanel("about", icon = icon("address-card"), fluidRow(
                         column(width = 8, tags$img(src="crimes_usa.png", width=450, height = 300),
                                tags$br(),
                                tags$a(""), align = "center"),
                         column(width = 4, tags$br(),
                                tags$p("This data set comes along with base R and contains statistics, trends, distributions for the crimes  per 100,000 residents in each of 50 US states, Provided linear regression model between population to the crimes to determine how well the states are performing over the years. Finally visual representation of US map for different crimes across from 2010-2019"))
                       )),
                       tabPanel(title = "Data", icon = icon("address-card"),
                                
                                fluidRow(
                                  column(width = 4, selectizeInput("Year", "Select a Year", choices = NULL)),
                                  column(width = 8, selectizeInput("Category", "Select a Category", choices = c1, selected = "Rape"))),
                                  withSpinner(dataTableOutput("Data"))),
                       
                       tabPanel(title = "Structure", icon = icon("address-card"), verbatimTextOutput("Structure")),
                       tabPanel(title = "Summary", icon = icon("address-card"), verbatimTextOutput("Summary"))
                       )
                ),
        
        #second tabname = "viz"
        tabItem(tabName = "viz",
                tabBox(id = "t2", width = 12,
                       tabPanel(title = "Crime Trends by State",value = "trends",
                                fluidRow(tags$div(align="center", box(tableOutput("top5"), title = textOutput("head1"),collapsible = TRUE, status = "primary", collapsed = TRUE, solidHeader = TRUE)),
                                         tags$div(align="center", box(tableOutput("low5"), title = textOutput("head2"),collapsible = TRUE, status = "primary", collapsed = TRUE, solidHeader = TRUE))),
                                radioButtons(inputId = "Display", label = "Select the option:", choices = c("All Data","For 100,000 Residents"), selected = "For 100,000 Residents", inline = TRUE),
                                withSpinner(plotlyOutput("bar"))
                                  
                                ),
                       tabPanel(title = "Distribution", value = "distro", 
                                withSpinner(plotlyOutput("barlineplot"))),
                       tabPanel(title = "Relationship", value = "Scatters",
                                radioButtons(inputId = "fit", label = "Select Smooth method", choices = c("loess","lm"), selected = "lm", inline = TRUE),
                                withSpinner(plotlyOutput("relationship"))
                                )
                )
        ),
        
        #third taItem
        tabItem(tabName = "map",
                box(selectInput("Crimetype", "Select Crime Type:", choices = c2, selected = "Rape"),
                    selectizeInput("Year2", "Select a Year", choices = NULL)),
                withSpinner(plotOutput("map_plot")), width = 450, height = 500
                )
       )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  updateSelectizeInput(session, 'Year1', choices = my_data$Year, server = TRUE)
  updateSelectizeInput(session, 'State', choices = my_data$State, server = TRUE)
  updateSelectizeInput(session, 'USA_ID', choices = my_data$State, server = TRUE)
  updateSelectizeInput(session, 'Year', choices = my_data$Year, server = TRUE)
  updateSelectizeInput(session, 'Year2', choices = my_data$Year, server = TRUE)
  
  
  #Structure
  output$Structure <- renderPrint({
    #Structure of the data

    my_data %>% 
      filter(X4 == "0") %>% 
      str()
  })
  
  #Summary
  output$Summary <- renderPrint({
    my_data %>% 
      filter(X4 == "0") %>% 
      summary()
  })
  
  #DataTable
  output$Data <- renderDataTable({
    subset_mydata<- my_data %>% 
      filter(Year == input$Year &
              X4 == "0")
    
    subset_mydata <- subset_mydata[ ,c("State","Year",input$Category)]
    
  })
  
  output$barlineplot <- renderPlotly({
    
    p1 <- my_data %>% 
      filter(State == input$State &
               X4 == "0") %>% 
      plot_ly() %>% 
      add_bars(x = ~Year , y = ~get(input$var1)) %>% 
      layout(xaxis = list(title = input$var1))
    
    
    p2 <- my_data %>% 
      filter(State == input$State &
             X4 == "0") %>% 
      plot_ly() %>% 
      add_lines(x =~Year, y =~get(input$var1)) %>% 
      layout(xaxis = list(showticklabels = F))
  
    
    #stacking
    subplot(p2,p1, nrows = 2) %>% hide_legend() %>% 
      layout(title = "Distribution Chart - Bar chart and Line chart")
  
  })  
    
  output$relationship <- renderPlotly({
    
    p = my_data %>% 
      filter(State == input$USA_ID &
               X4 == "0") %>% 
      ggplot(aes(x=get(input$var3), y=get(input$var2)),colour = trans)+
      geom_point() +
      geom_smooth(method = get(input$fit)) +
      labs(title = paste("Relationship between",input$var2, "and", input$var3, "Categories over the years"),
           x =  input$var3,
           y =  input$var2)+
      theme(plot.title = element_textbox_simple(size = 10,halign = 0.5))+
      scale_color_discrete(labels = labels)
    
      
  
    ggplotly(p)
  })

  output$bar <- renderPlotly({
    
    
    if (input$Display == "For 100,000 Residents"){
      my_data %>% 
        filter(X4 == "Rate per 100,000 inhabitants" &
                 Year == input$Year1) %>% 
        plot_ly() %>% 
        add_bars(y=~get(input$var4), x=~State) %>% 
        layout(title  = paste("Statewise Details - " , input$var4),
            #xaxis = list(categoryorder = "total ascending"),
             yaxis = list(title = paste(input$var4, "per 100,000 residents")),
             xaxis = list(title = "State",las = 2,
                          cex.names = 1))
    }else{
      
      my_data %>% 
        filter(X4 == "0" &
                 Year == input$Year1) %>% 
        plot_ly() %>% 
        add_bars(y=~get(input$var4), x=~State) %>% 
        layout(title  = paste("Statewise Details - " , input$var4),
               #xaxis = list(categoryorder = "total ascending"),
               yaxis = list(title = paste("Total number of ",input$var4)),
               xaxis = list(title = "State",las = 2,
                            cex.names = 1))
    }
  })
  
  
  output$head1 <- renderText(
    paste("5 states with high rate of", input$var4)
  )
  
  
  
  output$top5 <- renderTable({
    
    if (input$Display == "For 100,000 Residents"){
      my_data %>% 
        filter(X4 == "Rate per 100,000 inhabitants" &
                 Year == input$Year1) %>% 
        select(State, input$var4) %>% 
        arrange(desc(get(input$var4))) %>% 
        head(5)
      
    }else{
      
      my_data %>% 
        filter(X4 == "0" &
                 Year == input$Year1) %>% 
        select(State, input$var4) %>% 
        arrange(desc(get(input$var4))) %>% 
        head(5)
    }
    
  })
  
  output$head2 <- renderText(
    paste("5 states with low rate of", input$var4)
  )
  
  
  
  output$low5 <- renderTable({
    
    if (input$Display == "For 100,000 Residents"){
      my_data %>% 
        filter(X4 == "Rate per 100,000 inhabitants" &
                 Year == input$Year1) %>% 
        select(State, input$var4) %>% 
        arrange(get(input$var4)) %>% 
        head(5)
      
    }else{
      
      my_data %>% 
        filter(X4 == "0" &
                 Year == input$Year1) %>% 
        select(State, input$var4) %>% 
        arrange(get(input$var4)) %>% 
        head(5)
    }
    
  })
  
  #choropleth map
  output$map_plot <- renderPlot({
    
    new_join %>% 
      filter(X4 == "Rate per 100,000 inhabitants" &
                          Year == input$Year2) %>% 
      ggplot(aes(x=long, y= lat, fill=get(input$Crimetype), group = group)) +
      geom_polygon(color = "gray90", size = 0.1) +
      coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
      scale_fill_continuous(type = "viridis")+
      scale_x_continuous(limits = c(-125, -67))+
      scale_y_continuous(limits = c(25, 50))+
      labs(title = paste("U.S State Crime rate for 100,000 residents For ", input$Crimetype), fill = input$Crimetype)+
      theme(legend.position="right",
            axis.line=element_blank(),
            axis.text=element_blank(),
            axis.ticks=element_blank(),
            axis.title=element_blank(),
            panel.background=element_blank(),
            panel.border=element_blank(),
            panel.grid=element_blank())+
      geom_text(aes(x=x, y=y, label=US_State), size=4, color="white")
  })

}

# Run the application 
shinyApp(ui = ui, server = server)