library(shiny)
library(leaflet)
library(ggplot2)
library(googleVis)
library(plotly)
load("data/mydata.RData")
load("data/elia_g.RData")
load("data/elia_e.RData")
# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Welcome to the Belgian Energy Market app!"),
  titlePanel(h5("Please select below whether you are an energy client or supplier. If you are a client,
                you can view the energy (gas or electricity) across all selected municipalities for a certain sector and view
the differences between the different municipalities. If you are a supplier, you can view and compare your market share
                for gas and electricity with your peers.")),
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      radioButtons("scen1",label="Select below: I am an energy...",choices=list("Client","Supplier"),selected="Client"),
      conditionalPanel(condition="input.scen1 == 'Client'",selectInput("sector", label="Choose Sector:", choices = unique(mdf$Sector),selected="TERTIAIRE SECTOR"),
      selectizeInput("municipality", label="Choose Municipality:", choices = unique(mdf$Hoofdgemeente),multiple=T,selected="AALST")
    ),conditionalPanel(condition="input.scen1 == 'Supplier'",selectInput("company_g",label="Select gas company:",choices=unique(df_g_elia$Company),selectize=F,multiple=T,selected=unique(df_g_elia$Company)),
                       selectInput("company_e",label="Select electricity company:",choices=unique(df_e_elia$Company),selectize=F,multiple=T,selected=unique(df_e_elia$Company)))
    ),
    # Show a plot of the generated distribution
    mainPanel(
      conditionalPanel("input.scen1 == 'Client'",
                       tabsetPanel(tabPanel("Electricity",leafletOutput("map_e","100%",300),plotlyOutput("mygraph_e"),plotlyOutput("mygraph_e_individual"),
                                            htmlOutput("mytable_e")),
                                   tabPanel("Gas", leafletOutput("map_g","100%",300),plotlyOutput("mygraph_g"),plotlyOutput("mygraph_g_individual"),
                                            htmlOutput("mytable_g"))
          )
      ),
      conditionalPanel("input.scen1 == 'Supplier'","Gas",
                       plotlyOutput("elia_g"),"Electricity",plotlyOutput("elia_e")
                       )
      )
  )
))

