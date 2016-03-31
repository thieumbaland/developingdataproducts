library(shiny)
library(leaflet)
library(ggplot2)
library(dplyr)
library(plotly)
library(googleVis)

load("data/mydata.RData")
load("data/elia_g.RData")
load("data/elia_e.RData")

shinyServer(function(input, output) {
  my_e_data<-reactive({
    mdf[mdf$Sector==input$sector & mdf$Hoofdgemeente%in%input$municipality & mdf$Energie=="Elektriciteit" & mdf$Injectie.afname=="Afname",]
  })
  mydata<-reactive({
    mdf[mdf$Sector==input$sector & mdf$Hoofdgemeente%in%input$municipality & mdf$Injectie.afname=="Afname",]
  })
  my_g_data<-reactive({
    mdf[mdf$Sector==input$sector & mdf$Hoofdgemeente%in%input$municipality & mdf$Energie=="Aardgas" & mdf$Injectie.afname=="Afname",]
  })
  elia_g_data<-reactive({
    df_g_elia$Time<-factor(df_g_elia$Time,levels=c("jan 2015","feb 2015","mrt 2015","apr 2015","mei 2015","jun 2015","jul 2015","aug 2015","sep 2015","okt 2015","nov 2015","dec 2015"))
    df_g_elia[df_g_elia$Company%in%input$company_g,]
  })
  elia_e_data<-reactive({
    df_e_elia$Time<-factor(df_e_elia$Time,levels=c("jan 2015","feb 2015","mrt 2015","apr 2015","mei 2015","jun 2015","jul 2015","aug 2015","sep 2015","okt 2015","nov 2015","dec 2015"))
    df_e_elia[df_e_elia$Company%in%input$company_e,]
  })
  output$map_g<-renderLeaflet({
    leaflet() %>%
      addTiles %>%
      addMarkers(data=my_g_data(),popup=paste("Gas consumption in ",my_g_data()$Hoofdgemeente,": ",my_g_data()$Benaderend.Verbruik,sep=""))
  })
  output$map_e<-renderLeaflet({
    leaflet() %>%
      addTiles %>%
      addMarkers(data=my_e_data(),popup=paste("Electricity consumption in ",my_e_data()$Hoofdgemeente,": ",my_e_data()$Benaderend.Verbruik,sep=""))
  })
  output$mygraph_e<-renderPlotly({
    plot_data<-tbl_df(my_e_data())
    plot_data<-group_by(plot_data,Subsector,Verbruiksjaar)
    plot_data<-summarise(plot_data,Benaderend.Verbruik=sum(Benaderend.Verbruik))
    g<-ggplot(plot_data,aes(x=Subsector,y=Benaderend.Verbruik,fill=as.factor(Verbruiksjaar)))+theme_bw()+geom_bar(stat="identity",position="dodge")+coord_flip()+ylab("Consumption")+scale_fill_discrete(name="Year")
    p<-ggplotly(g)
    p
    })
  output$mygraph_g<-renderPlotly({
    plot_data<-tbl_df(my_g_data())
    plot_data<-group_by(plot_data,Subsector,Verbruiksjaar)
    plot_data<-summarise(plot_data,Benaderend.Verbruik=sum(Benaderend.Verbruik))
    g<-ggplot(plot_data,aes(x=Subsector,y=Benaderend.Verbruik,fill=as.factor(Verbruiksjaar)))+theme_bw()+geom_bar(stat="identity",position="dodge")+coord_flip()+ylab("Consumption")+scale_fill_discrete(name="Year")
    p<-ggplotly(g)
    p
    })
  output$mygraph_e_individual<-renderPlotly({
    g<-ggplot(my_e_data(),aes(x=Subsector,y=Benaderend.Verbruik,fill=as.factor(Verbruiksjaar)))+theme_bw()+geom_bar(stat="identity",position="dodge")+coord_flip()+ylab("Consumption")+facet_grid(. ~ Hoofdgemeente)+scale_fill_discrete(name="Year")
    p<-ggplotly(g)
    p
    })
  output$mygraph_g_individual<-renderPlotly({
    g<-ggplot(my_g_data(),aes(x=Subsector,y=Benaderend.Verbruik,fill=as.factor(Verbruiksjaar)))+theme_bw()+geom_bar(stat="identity",position="dodge")+coord_flip()+ylab("Consumption")+facet_grid(. ~ Hoofdgemeente)+scale_fill_discrete(name="Year")
    p<-ggplotly(g)
    p
    })
  output$mytable_e<-renderGvis({
    tmp<-my_e_data()
    tmp$lat<-NULL;tmp$lng<-NULL;tmp$Energie<-NULL;tmp$Injectie.afname<-NULL;
    gvisTable(tmp,options = list(page='enable'))
  })
  output$mytable_g<-renderGvis({
    tmp<-my_g_data()
    tmp$lat<-NULL;tmp$lng<-NULL;tmp$Energie<-NULL;tmp$Injectie.afname<-NULL;
    gvisTable(tmp,options=list(page='enable'))
  })
  output$elia_e<-renderPlotly({
    g<-ggplot(elia_e_data(),aes(x=as.factor(Time),y=NewShare,fill=as.factor(Company)))+geom_bar(stat="identity")+theme_bw()+xlab("Time")+ylab("Market Share")+scale_fill_discrete(name="Company")
    p<-ggplotly(g)
    p
  })
  output$elia_g<-renderPlotly({
    g<-ggplot(elia_g_data(),aes(x=as.factor(Time),y=NewShare,fill=as.factor(Company)))+geom_bar(stat="identity")+theme_bw()+xlab("Time")+ylab("Market Share")+scale_fill_discrete(name="Company")
    p<-ggplotly(g)
    p
    })
})
