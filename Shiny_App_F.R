library(leaflet)
library(plotly)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(shiny)
install.packages("shinythemes")
library(shinythemes)
#importing required files
#please note if any pop-up is displayed that says updating loaded packages when we run our app please press cancel and the app will then run smoothly
#please note when importing data please choose Heading as yes to show the heading of the data files instead of V1,V2 etc.
data_1<-read.csv("data_1.csv")
map_data<-read.csv("map_data.csv")
map_data_f<-read.csv("map_data_f.csv")
employement_data<-read.csv("employement_data.csv")
map_data<-map_data_f
#using theme selector for the user to change themes
#using select input for the user to change data based on location
# the first select input is for leaflet map and second select input is for plot2
#main panel consisting of the summary and tabs of each individual plots and their description
ui<-fluidPage(
  shinythemes::themeSelector(),
  headerPanel(title = "Covid-19 Pandemic:Poverty across the globe"),
  sidebarLayout(
    sidebarPanel(
      selectInput("loc","select a location", choices = unique(map_data$Location),selected = 'Location',multiple = T),
      selectInput("Loc","select a country", choices = unique(data_1$location)
      )),
    
    mainPanel(
      tabsetPanel(type = "tab",
                  tabPanel("Summary", h4(strong("Overview")),p(style = "text-allign:justify; font-size = 25px","The recent covid_19 pandemic has had significant impact on the lives of individuals, causing
economic despair across the globe.Globally, the increase in poverty that occured in 2020 due to COVID-19 still lingers."),p(style = "text-allign:justify;font-size = 25px","Let's try and understand more on this topic by exploring these questions: ",br()),p(style = "text-allign:justify;font-size = 25px","a)How has the covid-19 pandemic affected global economy and how are the factors inter-connected?",br(),"b)How did the pandemic affect individuals acroos the globe and increased poverty?")),
                  tabPanel("map",leafletOutput("map"),p(style = "text-allign:justify; font-size = 25px","The map shows us the intensity of effect of covid-19 across countries.The red markers indicate that the cases in those countries were considerably high than average cases across while green indicates that they were low.")),
                  tabPanel("plot1", h4(strong("How are the GDP and HDI linked amongst each other?")),plotlyOutput("plot1"),h4(strong("Description")),p(style = "text-allign:justify; font-size = 25px","The following graph shows that with increase in GDP, HDI also increases and vice-vera.It also shows us that the countries in particular continents with high population has considerably low HDI and hence low GDP. These factors were affected massively due to COVID-19 pandemic let's see how in the upcoming tab.")),
                  tabPanel("plot2", h4(strong("How did the pandemic affect the GDP.",br(),"Note:The data below showcasts data from year 2020")),plotOutput("plot2"),h4(strong("Description")),p(style = "text-allign:justify; font-size = 25px","In this plot if we compare the GDP for each country we can notice that as the pandemic hit during Mar'20, the GDP for each country started decreasing as we reach month 12 i.e December, the GDP for most of the countries decreased massively indicating that economy across the globe decreased and increased poverty across the globe.")),
                  tabPanel("plot3",h4(strong("How did the COVID-19 pandemic impact individuals and increase poverty?")),plotlyOutput("plot3"),h4(strong("Project description")),p(style = "text-allign:justify; font-size = 25px","The pandemic impacted individuals across the globe as we can witness here individuals from across the globe indicated that they had lost considerable working hours and hence their pay rate aslo reduced leading to earning less money than the would normally do before pandemic."))
      )
      
    )
  )
)
#plotting the data in shiny server
server<-function(input,output){
  pres.dat.sel<-reactive({
    data.subset<-map_data[map_data$Location == input$loc,]
    return(data.subset)
  })
  pres.data<-reactive({
    req(input$Loc)
    data<-data_1 %>% filter(location %in% input$Loc)%>%
      group_by(Month)%>%summarise(gdp_per_capita = sum(gdp_per_capita))
  })
  
  
  output$map<-renderLeaflet({
    leaflet()%>%addProviderTiles(providers$OpenStreetMap)%>%
      addCircleMarkers(data = pres.dat.sel(),color = ~Color)%>%
      addScaleBar()%>%
      addLegend(colors = c("green","red"),labels = c("Low","High"),title = "Data Source")
    
  })
  
  
  
  
  output$plot1<-renderPlotly({
    p<-ggplot(data_1,aes(x=human_development_index,y=gdp_per_capita))
    p<-p+geom_point(aes(color=iso_code,size=population,alpha=0.5))
    p<-p+labs(x="HDI",
              y="gdp_per_capita")
    ggplotly(p)
  })
  
  
  
  output$plot2<-renderPlot({
    ggplot(pres.data(),aes(x=Month,y=gdp_per_capita))+
      geom_col(stat = sum)+
      theme_minimal(base_size = 14)+
      labs(title="Affect of covid-19 on GDP")
  })
  
  output$plot3<-renderPlotly({
    p<-ggplot(employement_data,aes(x=Country,y=Percentage.Of.Working.Hrs.Lost))
    p<-p+geom_col(color = "red")
    p<-p+theme(axis.text.x=element_blank(),axis.ticks.x = element_blank())
    p<-p+labs(title="Affect on working hours")
    p<-p+labs(y = "Weekly hours lost")
    ggplotly(p)
  })
}

shinyApp(ui = ui, server = server)


