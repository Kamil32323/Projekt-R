install.packages("rvest")
install.packages("ggmap")
install.packages("leaflet")
install.packages("markdown")
install.packages("shiny")
library(rvest)
library(ggmap)
library(leaflet)

url1 <- read_html("http://www.careerjet.com/ibm-assembler-jobs.html")
oferty <- url1 %>%
  html_nodes('.title_compact') %>%
  html_text()
foferty <- factor(oferty)
to_remove<-paste(c("FULL TIME","-","(Pentaho)","(Mainframe/Cobol)"), collapse="|")
foferty<-gsub(to_remove,"",foferty)
foferty

url2 <- read_html("http://www.careerjet.com/ibm-assembler-jobs.html")
lokalizacje <- url2 %>%
  html_nodes('.locations_compact') %>%
  html_text()
flokalizacje <- factor(lokalizacje)
toremove<-paste(c("\n","-","$110,000130,000 per year"), collapse="|")
flokalizacje<-gsub(toremove,"",flokalizacje)
flokalizacje

pensja <- sample(70000:100000, 20, replace=F)
fpensja = factor(pensja)
fpensja

geocodes<-geocode(flokalizacje, output="latlona")

leaflet(geocodes) %>% addTiles() %>%
  addMarkers(~lon, ~lat, popup = ~as.character(foferty))


df = data.frame(foferty,fpensja)
names(df) <- c("Stanowisko", "Zarobki")

ggplot(data=df, aes(x=fpensja, y=foferty )) +
  geom_bar(colour="red", stat="identity",
           position=position_dodge(),
           size=.3)  +      
  xlab("Zarobki") + ylab("Stanowisko")