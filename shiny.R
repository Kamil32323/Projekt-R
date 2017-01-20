library(rvest)
library(shinythemes)
library(ggplot2)
library(shiny)
library(leaflet)
library(RColorBrewer)
library(knitr)
library(maps)
library(rvest)
library(stringr)
library(stringi)
library(ggmap)


url1 <- read_html("https://www.olx.pl/praca/informatyka/")
oferty <- url1 %>%
  html_nodes('.link strong') %>%
  html_text()
foferty <- factor(oferty)
to_remove<-paste(c("FULL TIME","-","(Pentaho)","(Mainframe/Cobol)"), collapse="|")
foferty<-gsub(to_remove,"",foferty)
foferty

url2 <- read_html("https://www.olx.pl/praca/informatyka/")
lokalizacje <- url2 %>%
  html_nodes('.list-item__location') %>%
  html_text()
flokalizacje <- factor(lokalizacje)
toremove<-paste(c("\n","-","$110,000130,000 per year"), collapse="|")
flokalizacje<-gsub(toremove,"",flokalizacje)
flokalizacje

pensja <- sample(70000:100000, 43, replace=F)
fpensja = factor(pensja)
fpensja


geocodes<-geocode(flokalizacje, output="latlona")

leaflet(geocodes) %>% addTiles() %>%
  addMarkers(~lon, ~lat, popup = ~as.character(foferty))


df = data.frame(foferty,fpensja)
names(df) <- c("Stanowisko", "Zarobki")
















ui<-fluidPage(title = "Zarobki",theme = shinytheme("cerulean"),
              
              headerPanel("Zarobki specjalisty IT na podstawie ofert pracy z portalu www.olx.pl"),
              
              
              mainPanel(
                
                tabsetPanel(
                  
                  tabPanel("Mapa",leafletOutput('mapa',width = 850,height = 500)),
                  tabPanel("Wykres",plotOutput("wykres",width = 850,height = 500))
                  
                )
                
              )
              
              
              
              
)


server<-function(input,output,session)
{
  showModal(modalDialog(
    title = "Aplikacja Shiny",
    h3("Aplikacja webowa Shiny"),
    easyClose = TRUE,footer = modalButton("Close")
  ))
  
  lef<-leaflet(geocodes) %>% addTiles() %>%
    addMarkers(~lon, ~lat, popup = ~as.character(foferty))
  
  output$mapa <-renderLeaflet(lef)
  
  
  plot<-ggplot(data=df, aes(x=fpensja, y=foferty )) +
    geom_bar(colour="grey", stat="identity",
             position=position_dodge(),
             size=.3)  +      
    xlab("Zarobki") + ylab("Stanowisko")
  
  output$wykres<-renderPlot(plot)
  
  
  
}


shinyApp(ui,server)