
# Sys.setlocale(category = 'LC_ALL','en_US.UTF-8')



library('dplyr')
library('lubridate')
library('stringr')
library('RMySQL')
library('googlesheets')

##pull typeform responses from googlesheets
gs_auth(new_user = FALSE)
gs_ls()
search <- gs_title("Hausy Typeform Responses")

search <- gs_read(search)

for (i in 1:length(search)) {
        if (class(search[, i]) == "character" && Encoding(search[, i]) != "UTF-8") {
                Encoding(x = search[, i]) <- "latin1"
        }
}

names(search) <- c("prop.type","primera","segunda","tercera","max.price","min.price", #6
                   "price.flex","edad","menos.de.18","entre18y44","entre45y64","anos65up", #12
                   "tamano","bedrooms","banos","mascotas","perros","gatos","num.parking", #19
                   "amenidades1","amenidades2","price.importance","ubi.importance","tamano.importance", #24
                   "edad.importance","amen.importance","other.aspect","financing.how","ya.tiene.credito", #29
                   "monte.de.credito","financial.help","email","phone","name","utm_campaign", #35
                   "utm_medium","utm_term","utm_source","submit.date","token") #40


search <- as.data.frame(search)
search$Record_Number <- seq(1:length(search[,1]))
search <- search[,c(41,32,39,1:31)]
search <- arrange(search,desc(Record_Number))

today <- as.Date(now())


