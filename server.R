
# Sys.setlocale(category = 'LC_ALL','en_US.UTF-8')


library('shiny')
library('dplyr')
library('lubridate')
library('stringr')
library('RMySQL')
library('googlesheets')

scrape <- read.csv("scrape_1.csv",stringsAsFactors = FALSE,encoding = "UTF8")

for (i in 1:length(scrape)) {
        if (class(scrape[, i]) == "character" && Encoding(scrape[, i]) != "UTF-8") {
                Encoding(x = scrape[, i]) <- "latin1"
        }
}

scrape$date_published2 <- as.Date(scrape$date_published2)

col_del <- unique(select(scrape,colonia,delegacion))



shinyServer(function(input, output) {
        
        
        # table of all typeform responses
        output$search_tbl <- renderTable({
                search
        })
        
        
        
        
        # original typeform
        output$tf1 <- renderTable({
                search[which(search$Record_Number==input$record),]
        })
        
        
        
        tf2 <- reactive({
                
                tf2 <- search[which(search$Record_Number==input$record),]
                prim <- ifelse(input$primera == "",tf2$primera,input$primera)
                tf2$primera <- prim
                
                seg <- ifelse(input$segunda == "",tf2$segunda,input$segunda)
                tf2$segunda <- seg
                
                terc <- ifelse(input$tercera == "",tf2$tercera,input$tercera)
                tf2$tercera <- terc
                
                maxP <- ifelse(input$max.price == "",tf2$max.price,input$max.price)
                tf2$max.price <- maxP
                
                minP <- ifelse(input$min.price == "",tf2$min.price,input$min.price)
                tf2$min.price <- minP
                
                return(tf2)
        })
        
        
        output$tf2 <- renderTable({
                input$goButton
                isolate(tf2())
        })
        
        #in case you want to look at head of scrape
        output$scrape <- renderTable({
                head(scrape)
        })
        
        #in case you want to look at all colonies and delegations
        output$coldel <- renderTable({
                col_del
        })
        
        
        ##run prep and cleaning code on typeform prior to scoring
        ##includes getting the deleg/col choices. 
        tf3 <- eventReactive(
                input$prepareButton, isolate({
                        tf3 <- tf2()
                        #####################################################
                        #######tidy price
                        #####################################################
                        tf3$max.price <- gsub("\\.00","",tf3$max.price)
                        tf3$max.price <- gsub(",","",tf3$max.price)
                        tf3$max.price <- gsub("\\.","",tf3$max.price)
                        tf3$max.price <- gsub("\\$","",tf3$max.price)
                        tf3$max.price <- gsub("[A-Za-z ]","",tf3$max.price)
                        tf3$max.price <- format(as.numeric(tf3$max.price),scientific = FALSE)
                        #######max price
                        tf3$min.price <- gsub("\\.00","",tf3$min.price)
                        tf3$min.price <- gsub(",","",tf3$min.price)
                        tf3$min.price <- gsub("\\.","",tf3$min.price)
                        tf3$min.price <- gsub("\\$","",tf3$min.price)
                        tf3$min.price <- gsub("[A-Za-z ]","",tf3$min.price)
                        tf3$min.price <- format(as.numeric(tf3$min.price),scientific = FALSE)
                        #####################################################
                        #######prep age
                        #####################################################
                        tf3$age.fix <- 1
                        if(grepl("50",tf3$edad)) {tf3$age.fix <- 50} else {
                                if(grepl("30",tf3$edad)) {tf3$age.fix <- 30} else {
                                        if(grepl("20",tf3$edad)) {tf3$age.fix <- 20} else {
                                                if(grepl("10",tf3$edad)) {tf3$age.fix <- 10} else {
                                                        if(grepl("5",tf3$edad)) {tf3$age.fix <- 5} else {
                                                                if(grepl("Cualquier",tf3$edad)) {tf3$age.fix <- 500} 
                                                        }
                                                }
                                        }
                                }
                        }
                        #######tidy size
                        #####################################################
                        if(tf3$tamano==0 | trimws(tf3$tamano)=="" | is.na(tf3$tamano) | grepl("^[A-Za-z ].+[A-Za-z]$",tf3$tamano)) {
                                tf3$size1 <- ""
                                tf3$size2 <- ""
                        } else {
                                if(grepl("-",tf3$tamano)==FALSE) {
                                        tf3$size1 <- tf3$tamano 
                                        tf3$size1 <- gsub("m.+ cuadrados","",tf3$size1)
                                        tf3$size1 <- gsub("[Mm]|[Mm]2|[Mm]3","",tf3$size1)
                                        tf3$size1 <- trimws(tf3$size1)
                                        tf3$size1 <- as.numeric(tf3$size1)
                                        tf3$size2 <- tf3$size1
                                        tf3$size.interval <- 0
                                } else {
                                        #tf3$size1 <- tf3$tamano "90-150 m2"
                                        tf3$tamano <- gsub("m.+ cuadrados","",tf3$tamano)
                                        tf3$tamano <- gsub("m2","",tf3$tamano) #"90-150 "
                                        tf3$tamano <- trimws(tf3$tamano) #"90-150"
                                        tf3$size1 <- unlist(str_extract(tf3$tamano,"[0-9].+-"))
                                        tf3$size1 <- gsub("-","",tf3$size1)
                                        tf3$size1 <- as.numeric(tf3$size1)
                                        
                                        tf3$size2 <- unlist(str_extract(tf3$tamano,"-[0-9].+"))
                                        tf3$size2 <- gsub("-","",tf3$size2)
                                        tf3$size2 <- as.numeric(tf3$size2)
                                        
                                        tf3$size.interval <- 1
                                } 
                        }
                        #####################################################
                        ##tidy locations and produce interrim results.
                        #####################################################
                        tf3$primera <- enc2utf8(gsub('\\n',',',tf3$primera))
                        tf3$primera <- enc2utf8(gsub('^ +','',tf3$primera))
                        tf3$primera <- enc2utf8(gsub(' +$','',tf3$primera))
                        tf3$primera <- enc2utf8(gsub(', ',',',tf3$primera))
                        tf3$primera <- enc2utf8(gsub(' ,',',',tf3$primera))
                        
                        tf3$segunda <- enc2native(gsub('\\n',',',tf3$segunda))
                        tf3$segunda <- enc2native(gsub('^ +','',tf3$segunda))
                        tf3$segunda <- enc2native(gsub(' +$','',tf3$segunda))
                        tf3$segunda <- enc2native(gsub(', ',',',tf3$segunda))
                        tf3$segunda <- enc2native(gsub(' ,',',',tf3$segunda))
                        
                        tf3$tercera <- gsub('\\n',',',tf3$tercera)
                        tf3$tercera <- gsub('^ +','',tf3$tercera)
                        tf3$tercera <- gsub(' +$','',tf3$tercera)
                        tf3$tercera <- gsub(', ',',',tf3$tercera)
                        tf3$tercera <- gsub(' ,',',',tf3$tercera)
                        ############
                        #make lomas == lomas de chapultapec
                        if(!is.na(tf3[1,5]) & tf3[1,5] == "lomas") {tf3[1,5] <- "lomas de chapultepec"}
                        if(!is.na(tf3[1,6]) & tf3[1,6] == "lomas") {tf3[1,6] <- "lomas de chapultepec"}
                        if(!is.na(tf3[1,7]) & tf3[1,7] == "lomas") {tf3[1,7] <- "lomas de chapultepec"}
                        #other cleaning steps
                        for(i in 5:7) {
                                if(!is.na(tf3[1,i]) & grepl("^sur|^zona sur",tf3[1,i],ignore.case = TRUE)) {
                                        tf3[1,i] <- "tlalpan,xochimilco,magdalena contreras,coyoacan"}
                                
                                if(!is.na(tf3[1,i]) & grepl("roma",tf3[1,i],ignore.case = TRUE)) {
                                        tf3[1,i] <- "roma norte,roma sur"}
                                
                                if(!is.na(tf3[1,i]) & grepl("portales",tf3[1,i],ignore.case = TRUE)) {
                                        tf3[1,i] <- "portales norte,portales sur"}
                                
                                if(!is.na(tf3[1,i]) & grepl("lindavista",tf3[1,i],ignore.case = TRUE)) {
                                        tf3[1,i] <- "lindavista norte,lindavista sur,lindavista vallejo i sección"}
                                
                                if(!is.na(tf3[1,i]) & grepl("gustavo madero",tf3[1,i],ignore.case = TRUE)) {
                                        tf3[1,i] <- "gustavo a. madero"}
                        }
                        for(i in 5:7) {
                                if(!is.na(tf3[1,i]) & grepl("santa fe",tf3[1,i],ignore.case = TRUE)) {
                                        tf3[1,i] <- "santa fe centro ciudad,santa fe cuajimalpa,santa fe imss,santa fe la loma,santa fe peña blanca"}
                        }
                        
                        tf3$primera <- tolower(tf3$primera)
                        tf3$primera <- gsub("[aá]lvaro obr[ae]g[oó]n","alvaro obregon",tf3$primera,ignore.case = TRUE)
                        tf3$primera <- gsub("álamos","alamos",tf3$primera,ignore.case = TRUE)
                        tf3$primera <- gsub("Cuahut[ée]moc","Cuauhtémoc",tf3$primera,ignore.case = TRUE)
                        
                        
                        tf3$segunda <- tolower(tf3$segunda)
                        tf3$segunda <- gsub("[aá]lvaro obr[ae]g[oó]n","alvaro obregon",tf3$segunda,ignore.case = TRUE)
                        tf3$segunda <- gsub("álamos","alamos",tf3$segunda,ignore.case = TRUE)
                        tf3$segunda <- gsub("Cuahut[ée]moc","Cuauhtémoc",tf3$segunda,ignore.case = TRUE)
                        
                        tf3$tercera <- tolower(tf3$tercera)
                        tf3$tercera <- gsub("[aá]lvaro obr[ae]g[oó]n","alvaro obregon",tf3$tercera,ignore.case = TRUE)
                        tf3$tercera <- gsub("álamos","alamos",tf3$tercera,ignore.case = TRUE)
                        tf3$tercera <- gsub("Cuahut[ée]moc","Cuauhtémoc",tf3$tercera,ignore.case = TRUE)
                        
                        
                        #####################################################
                        #Is the location a delegacion?
                        #####################################################
                        deleg_choices <- rep("",times=3)
                        del1 <- character()
                        del2 <- character()
                        del3 <- character()
                        
                        primera.list <- strsplit(tf3$primera,",")
                        for(i in 1:length(primera.list[[1]])) {
                                primera.matches <- unique(agrep(primera.list[[1]][i],col_del$delegacion,ignore.case = TRUE,value = TRUE))
                                if(all(!is.na(primera.matches)) & length(primera.matches)>0) {
                                        primera.dists <- adist(primera.list[[1]][i],unique(agrep(primera.list[[1]][i],col_del$delegacion,ignore.case = TRUE,value = TRUE,fixed=TRUE)))
                                        choose <- which(primera.dists ==min(primera.dists))
                                        del1[i] <- primera.matches[choose]
                                } else {del1[i] <- ""}
                        }
                        
                        segunda.list <- strsplit(tf3$segunda,",")
                        for(i in 1:length(segunda.list[[1]])) {
                                segunda.matches <- unique(agrep(segunda.list[[1]][i],col_del$delegacion,ignore.case = TRUE,value = TRUE))
                                if(all(!is.na(segunda.matches)) & length(segunda.matches)>0) {
                                        segunda.dists <- adist(segunda.list[[1]][i],unique(agrep(segunda.list[[1]][i],col_del$delegacion,ignore.case = TRUE,value = TRUE,fixed=TRUE)))
                                        choose <- which(segunda.dists ==min(segunda.dists))
                                        del2[i] <- segunda.matches[choose]
                                } else {del2[i] <- ""}
                        }
                        
                        tercera.list <- strsplit(tf3$tercera,",")
                        for(i in 1:length(tercera.list[[1]])) {
                                tercera.matches <- unique(agrep(tercera.list[[1]][i],col_del$delegacion,ignore.case = TRUE,value = TRUE))
                                if(all(!is.na(tercera.matches)) & length(tercera.matches)>0) {
                                        tercera.dists <- adist(tercera.list[[1]][i],unique(agrep(tercera.list[[1]][i],col_del$delegacion,ignore.case = TRUE,value = TRUE,fixed=TRUE)))
                                        choose <- which(tercera.dists ==min(tercera.dists))
                                        del3[i] <- tercera.matches[choose]
                                } else {del3[i] <- ""}
                        }
                        
                        if(length(del1)==0 | all(del1=="")) {deleg_choices[1] <- "Not Found"} else {
                                if(length(del1)==1 & all(!is.na(del1))) {deleg_choices[1] <- del1} else {
                                        if(length(del1)>1 & any(del1=="")) {deleg_choices[1] <- "Mix"} else {
                                                if(length(del1)>1 & all(del1!="")) {deleg_choices[1] <- "Multiple"}
                                        }
                                }
                        }
                        
                        if(is.na(tf3$segunda)) {deleg_choices[2] <- "No selection"} else {
                                if(length(del2)==0 | all(del2=="")) {deleg_choices[2] <- "Not Found"} else {       
                                        if(length(del2)==1) {deleg_choices[2] <- del2} else {
                                                if(length(del2)>1 & any(del2=="")) {deleg_choices[2] <- "Mix"} else {
                                                        if(length(del2)>1 & all(del2!="")) {deleg_choices[2] <- "Multiple"} 
                                                }
                                        }  
                                }
                        }
                        
                        if(is.na(tf3$tercera)) {deleg_choices[3] <- "No selection"} else {
                                if(length(del3)==0 | all(del3=="")) {deleg_choices[3] <- "Not Found"} else {       
                                        if(length(del3)==1) {deleg_choices[3] <- del3} else {
                                                if(length(del3)>1 & any(del3=="")) {deleg_choices[3] <- "Mix"} else {
                                                        if(length(del3)>1 & all(del3!="")) {deleg_choices[3] <- "Multiple"} 
                                                }
                                        }  
                                }
                        }
                        
                        #####################################################
                        #Is the location a colonia?
                        #####################################################
                        colonia_choices <- rep("",times=3)
                        
                        col1 <- character()
                        col2 <- character()
                        col3 <- character()
                        
                        for(i in 1:length(primera.list[[1]])) {
                                primera.matches <- unique(agrep(primera.list[[1]][i],col_del$colonia,ignore.case = TRUE,value = TRUE))
                                if(all(!is.na(primera.matches)) & length(primera.matches)>0) {
                                        primera.dists <- adist(primera.list[[1]][i],unique(agrep(primera.list[[1]][i],col_del$colonia,ignore.case = TRUE,value = TRUE,fixed=TRUE)))
                                        choose <- which(primera.dists ==min(primera.dists))
                                        col1[i] <- primera.matches[choose]
                                } else {col1[i] <- ""}
                        } 
                        
                        for(i in 1:length(segunda.list[[1]])) {
                                segunda.matches <- unique(agrep(segunda.list[[1]][i],col_del$colonia,ignore.case = TRUE,value = TRUE))
                                if(all(!is.na(segunda.matches)) &length(segunda.matches)>0) {
                                        segunda.dists <- adist(segunda.list[[1]][i],unique(agrep(segunda.list[[1]][i],col_del$colonia,ignore.case = TRUE,value = TRUE,fixed=TRUE)))
                                        choose <- which(segunda.dists ==min(segunda.dists))
                                        col2[i] <- segunda.matches[choose]
                                } else {col2[i] <- ""}
                        }
                        
                        for(i in 1:length(tercera.list[[1]])) {
                                tercera.matches <- unique(agrep(tercera.list[[1]][i],col_del$colonia,ignore.case = TRUE,value = TRUE))
                                if(all(!is.na(tercera.matches)) &length(tercera.matches)>0) {
                                        tercera.dists <- adist(tercera.list[[1]][i],unique(agrep(tercera.list[[1]][i],col_del$colonia,ignore.case = TRUE,value = TRUE,fixed=TRUE)))
                                        choose <- which(tercera.dists ==min(tercera.dists))
                                        col3[i] <- tercera.matches[choose]
                                } else {col3[i] <- ""}
                        }
                        
                        if(!(deleg_choices[1] %in% c("No selection","Not Found","Mix")) | deleg_choices[1] == "Multiple") {
                                colonia_choices[1] <- "Using as Delegacion"} else {
                                        if(length(col1)==1 & all(!is.na(col1))) {
                                                colonia_choices[1] <- col1} else {
                                                        if(length(col1)==0) {
                                                                colonia_choices[1] <- "Not Found"}
                                                        else {
                                                                if(length(col1)>1 & deleg_choices[1] != "Mix") {
                                                                        colonia_choices[1] <- "Multiple"    
                                                                } else {
                                                                        if(deleg_choices[1] == "Mix") {colonia_choices[1] <- "Mix" 
                                                                        }
                                                                }   
                                                        }
                                                }}
                        
                        if(deleg_choices[2] =="No selection") {colonia_choices[2] <- "No selection"} else {
                                if(!(deleg_choices[2] %in% c("No selection","Not Found","Mix")) | deleg_choices[2] == "Multiple") {
                                        colonia_choices[2] <- "Using as Delegacion"} else {
                                                if(length(col2)==1 & all(!is.na(col2))) {
                                                        colonia_choices[2] <- col2} else {
                                                                if(length(col2)==0) {
                                                                        colonia_choices[2] <- "Not Found"} else {
                                                                                if(length(col2)>1 & deleg_choices[2] != "Mix") {
                                                                                        colonia_choices[2] <- "Multiple"} else {
                                                                                                if(deleg_choices[2] == "Mix") {colonia_choices[2] <- "Mix"     
                                                                                                }
                                                                                        }
                                                                        }
                                                        }   
                                        }}
                        
                        if(deleg_choices[3] =="No selection") {colonia_choices[3] <- "No selection"} else {
                                if(!(deleg_choices[3] %in% c("No selection","Not Found","Mix")) | deleg_choices[3] == "Multiple") {
                                        colonia_choices[3] <- "Using as Delegacion"} else {
                                                if(length(col3)==1 & all(!is.na(col3))) {
                                                        colonia_choices[3] <- col3} else {
                                                                if(length(col3)==0) {
                                                                        colonia_choices[3] <- "Not Found"} else {
                                                                                if(length(col3)>1 & deleg_choices[3] != "Mix") {
                                                                                        colonia_choices[3] <- "Multiple"} else {
                                                                                                if(deleg_choices[3] == "Mix") {colonia_choices[3] <- "Mix"     
                                                                                                }
                                                                                        }
                                                                        }
                                                        }   
                                        }}
                        #####################################################
                        ###CHECK RESULTS
                        #####################################################    
                        tf3$deleg_choices <- paste0(deleg_choices,collapse=",")
                        tf3$colonia_choices <- paste0(colonia_choices,collapse=",")
                        
                        tf3$del1 <- paste0(del1,collapse=",")
                        tf3$del2 <- paste0(del2,collapse=",")
                        tf3$del3 <- paste0(del3,collapse=",")
                        
                        tf3$col1 <- paste0(col1,collapse=",")
                        tf3$col2 <- paste0(col2,collapse=",")
                        tf3$col3 <- paste0(col3,collapse=",")
                        
                        tf3$primera <- tf3$primera
                        tf3$segunda <- tf3$segunda
                        tf3$tercera <- tf3$tercera
                        
                        
                        #####################################################
                        ###Tidy Other Inputs
                        #####################################################
                        tf3$price.flex <- as.integer(tf3$price.flex)
                        
                        price.flex.mult <- c(.025,.05,.1,.15,.2)
                        
                        tf3$price.flex <- as.numeric(tf3$price.flex)
                        flex1 <- as.numeric(tf3$price.flex)
                        flex2 <- as.numeric(price.flex.mult[flex1])
                        max <- as.numeric(tf3$max.price)
                        min <- as.numeric(tf3$min.price)
                        
                        if(tf3$max.price == tf3$min.price) {
                                tf3$max.price2 <- max * (1 + flex2)
                                tf3$min.price2 <- min * (1 - flex2)
                        } else {
                                tf3$max.price2 <- tf3$max.price
                                tf3$min.price2 <- tf3$min.price
                        }
                        tf3$max.price2 <- as.numeric(tf3$max.price2)
                        tf3$min.price2 <- as.numeric(tf3$min.price2)
                        
                        tf3$price25 <- tf3$min.price2 + ((tf3$max.price2-tf3$min.price2) / 4)
                        tf3$price50 <- tf3$min.price2 + ((tf3$max.price2-tf3$min.price2) / 2)
                        tf3$price75 <- tf3$max.price2 - ((tf3$max.price2-tf3$min.price2) / 4)
                        
                        # #quick clean for bedrooms
                        if(tf3$bedrooms == "0 (Estudio)") {
                                tf3$bedrooms <- 0
                        }
                        tf3$bedrooms <- as.numeric(tf3$bedrooms)
                        
                        #amenities to lower case
                        tf3$amenidades1 <- tolower(tf3$amenidades1)
                        tf3$amenidades2 <- tolower(tf3$amenidades2)
                        
                        # #combine into one col
                        tf3$amenidades.all <- paste0(tf3$amenidades1,",",tf3$amenidades2)
                        tf3$amenidades.all <- gsub(", ",",",tf3$amenidades.all)
                        #####################################################
                        ###Return tf3
                        #####################################################
                        
                        return(tf3)
                        
                })
        )
        
        output$tf3 <- renderTable({
                
                tf3()
        })
        
        output$deleg <- renderText({
                tf3()$deleg_choices
        })
        output$dels <- renderText({
                paste(tf3()$del1,", ",tf3()$del2,", ",tf3()$del3)
        })
        output$colonia <- renderText({
                tf3()$colonia_choices
        })
        output$cols <- renderText({
                paste(tf3()$col1,", ",tf3()$col2,", ",tf3()$col3)
        })
        
        ##run score code
        
        scores <- eventReactive(
                input$RunCode, isolate({
                        scores <- data.frame(type = rep(0, times = length(scrape[,1]))) 
                        ###############################################################
                        ##PROPERTY TYPE
                        ###############################################################
                        
                        #two cols for property type
                        scores$type <- scrape$property_type
                        scores$type.score <- rep(0, times = length(scrape[,1]))
                        
                        #prop.switch <- input$property
                        
                        scores$type.score[which(scrape$property_type2 != tf3()$prop.type)] <- ifelse(
                                input$property == "Selected Type",-100,0)
                        
                        
                        
                        if(tf3()$prop.type=="Casa" & input$property == "Selected Type") {
                                scores$type.score[which(scrape$property_type == "Casa en condominio")] <- -5
                        }
                        
                        ###############################################################
                        ##LOCATIONS
                        ###############################################################
                        deleg_choices <- unlist(strsplit(tf3()$deleg_choices,split=","))
                        colonia_choices <- unlist(strsplit(tf3()$deleg_choices,split=","))
                        del1 <- unlist(strsplit(tf3()$del1,split=","))
                        del2 <- unlist(strsplit(tf3()$del2,split=","))
                        del3 <- unlist(strsplit(tf3()$del3,split=","))
                        col1 <- unlist(strsplit(tf3()$col1,split=","))
                        col2 <- unlist(strsplit(tf3()$col2,split=","))
                        col3 <- unlist(strsplit(tf3()$col3,split=","))
                        
                        
                        scores$location <- "No Exact Match"
                        scores$location.score <- tf3()$ubi.importance * -20
                        scores$property.location <- ""
                        
                        
                        if(deleg_choices[1]=="Mix") {del1.nondels <- which(del1=="")} else {
                                del1.nondels <- which(!is.na(col1))
                        }
                        
                        if(deleg_choices[2]=="Mix") {del2.nondels <- which(del2=="")} else {
                                del2.nondels <- which(!is.na(col2))
                        }
                        
                        if(deleg_choices[3]=="Mix") {del3.nondels <- which(del3=="")} else {
                                del3.nondels <- which(!is.na(col3))
                        }
                        
                        for(i in 1:length(scrape[,1])) {
                                
                                scores$property.location[i] <- paste0(scrape[i,"colonia"],", ",scrape[i,"delegacion"])
                                
                                
                                if(deleg_choices[1] != "Not Found" & sum(grepl(scrape[i,"delegacion"],del1))>0) {
                                        scores$location.score[i] <- 0
                                        scores$location[i] <- "Primera Match"
                                } else {
                                        if(!(colonia_choices[1] %in% c("Not Found","Using as Delegacion","")) & sum(grepl(scrape[i,"colonia"],col1[del1.nondels]))>0) {
                                                scores$location.score[i] <- 0
                                                scores$location[i] <- "Primera Match"
                                        } else {
                                                if(deleg_choices[2] == "No selection") {next} else {
                                                        
                                                        if(deleg_choices[2] != "Not Found" & sum(grepl(scrape[i,"delegacion"],del2))>0) {
                                                                scores$location.score[i] <- tf3()$ubi.importance * -.25
                                                                scores$location[i] <- "Segunda Match"
                                                        } else {
                                                                if(!(colonia_choices[2] %in% c("Not Found","Using as Delegacion","")) & sum(grepl(scrape[i,"colonia"],col2[del2.nondels]))>0) {
                                                                        scores$location.score[i] <- tf3()$ubi.importance * -.25
                                                                        scores$location[i] <- "Segunda Match"
                                                                } else {
                                                                        if(deleg_choices[3] != "Not Found" & sum(grepl(scrape[i,"delegacion"],del3))>0) {
                                                                                scores$location.score[i] <- tf3()$ubi.importance * -.50
                                                                                scores$location[i] <- "Tercera Match"
                                                                        } else {
                                                                                if(!(colonia_choices[3] %in% c("Not Found","Using as Delegacion")) & sum(grepl(scrape[i,"colonia"],col3[del3.nondels]))>0) {
                                                                                        scores$location.score[i] <- tf3()$ubi.importance * -.50
                                                                                        scores$location[i] <- "Tercera Match"
                                                                                }
                                                                                
                                                                        }
                                                                }
                                                        }
                                                }
                                        }
                                }
                        }
                        
                        ##############################################################
                        ##PRICE
                        ###############################################################
                        
                        scores$price <- ""
                        scores$price.score <- 0
                        
                        price.flex.mult <- c(.025,.05,.1,.15,.2)
                        
                        for(i in 1:length(scrape[,1])) {
                                
                                if(is.na(scrape[i,"price2"])) {next}
                                
                                if(scrape[i,"price2"] >= tf3()$price25 & scrape[i,"price2"] < tf3()$price50) {
                                        scores$price.score[i] <- tf3()$price.importance * -.25
                                } else {
                                        if(scrape[i,"price2"] >= tf3()$price50 & scrape[i,"price2"] < tf3()$price75) {
                                                scores$price.score[i] <- tf3()$price.importance * -.50
                                        } else {
                                                if(scrape[i,"price2"] >= tf3()$price75 & scrape[i,"price2"] <= tf3()$max.price2) {
                                                        scores$price.score[i] <- tf3()$price.importance * -.75
                                                } else {
                                                        if(scrape[i,"price2"] > tf3()$max.price2) {  #price > max
                                                                diff <- scrape[i,"price2"] - tf3()$max.price2 #difference in prices
                                                                mult.amount <- price.flex.mult[tf3()$price.flex] * tf3()$max.price2 #this is n% of max price
                                                                # this is HOW MANY n% multiples (an integer) over max price the diff is
                                                                #MULTIPLIED by the price.importance var [1-5]
                                                                scores$price.score[i] <- -(round(diff/mult.amount,1) * tf3()$price.importance)
                                                        } else {
                                                                if(scrape[i,"price2"] < tf3()$min.price2) { #price < min
                                                                        diff <- tf3()$min.price2 - scrape[i,"price2"] #difference in prices
                                                                        mult.amount <- .2 * tf3()$min.price2 #20% of min price
                                                                        # this is HOW MANY 20% multiples (an integer) under price the diff is
                                                                        #MULTIPLIED by the price.importance var [1-5]
                                                                        scores$price.score[i] <- -(round(diff/mult.amount,1))
                                                                }
                                                        }
                                                }
                                        }
                                        
                                }
                                
                                
                        }
                        
                        #now fill in the price column with amount over/under minimum
                        
                        for(i in 1:length(scrape[,1])) {
                                
                                if(is.na(scrape[i,"price2"])) {scores$price[i] <- "Precio Desconocido";next}
                                
                                if(scrape[i,"price2"] > tf3()$min.price2 & scrape[i,"price2"] <= tf3()$max.price2) {
                                        scores$price[i] <- paste0("$",format(scrape[i,"price2"] - tf3()$min.price2,big.mark = ",",scientific = FALSE)," over min / ",
                                                                  "$",format(tf3()$max.price2 - scrape[i,"price2"],big.mark = ",",scientific = FALSE)," below max")
                                } else {
                                        if(scrape[i,"price2"] > tf3()$max.price2) {
                                                scores$price[i] <- paste0("$",format(scrape[i,"price2"] - tf3()$max.price2,big.mark = ",",scientific = FALSE)," over max")
                                        } else {
                                                if(scrape[i,"price2"] < tf3()$min.price2) {
                                                        scores$price[i] <- paste0("$",format(tf3()$min.price2 - scrape[i,"price2"],big.mark = ",",scientific = FALSE)," below minimum")
                                                } else {
                                                        scores$price[i] <- "Igual o cerca del mínimo"
                                                }
                                        }
                                        
                                }
                        }
                        
                        ###############################################################
                        ##SIZE
                        ###############################################################
                        
                        ##two columns for tamano in m2
                        
                        scores$size <- ""
                        scores$size.score <- 0
                        
                        if(tf3()$size1=="") {scores$size <- "Solicitud no proporcionada"}
                        
                        if(tf3()$size1!="") {
                                
                                for(i in 1:length(scrape[,1])) {
                                        if(is.na(scrape[i,"area_m2_of_construction"])) {
                                                scores$size[i] <- "Tamaño desconocido"
                                        } else {
                                                if(scrape[i,"area_m2_of_construction"] >= tf3()$size1 & scrape[i,"area_m2_of_construction"] <= tf3()$size2) {
                                                        scores$size[i] <- "Concuerda"
                                                } else {
                                                        if(scrape[i,"area_m2_of_construction"] > tf3()$size2) {
                                                                diff <- scrape[i,"area_m2_of_construction"] - tf3()$size2 #difference in sizes
                                                                mult.amount <- 20 #looking for multiples of 20m2
                                                                # this is HOW MANY n multiples (an integer) below size request the diff is
                                                                #MULTIPLIED by the size.importance var [1-5]
                                                                scores$size.score[i] <- -(round(diff/mult.amount,1))
                                                                scores$size[i] <- paste0(diff,"m2 over requested size")
                                                                
                                                        } else {
                                                                
                                                                diff <- tf3()$size1 - scrape[i,"area_m2_of_construction"] #difference in sizes
                                                                mult.amount <- 20 #looking for multiples of 20m2
                                                                # this is HOW MANY n multiples (an integer) below size request the diff is
                                                                #MULTIPLIED by the size.importance var [1-5]
                                                                scores$size.score[i] <- -(round(diff/mult.amount,1) * tf3()$tamano.importance)
                                                                scores$size[i] <- paste0(diff,"m2 below requested size")
                                                        }
                                                }
                                                
                                        }
                                }
                                
                        }
                        
                        ###############################################################
                        ##BEDS & BATHS
                        ###############################################################
                        
                        #two columns for num.bedrooms
                        
                        scores$beds <- ""
                        scores$beds.score <- 0
                        
                        for(i in 1:length(scrape[,1])) {
                                if(is.na(scrape[i,"num_bedrooms"])) {
                                        scores$beds[i] <- "Numero de recámaras desconocido"
                                        next
                                } else {
                                        diff <- tf3()$bedrooms - scrape[i,"num_bedrooms"]
                                        if(diff==0) {scores$beds[i] <- "Match"} else {
                                                if(diff == -1) {
                                                        scores$beds.score[i] <- diff
                                                        scores$beds[i] <- paste0(abs(diff)," over request")
                                                } else {
                                                        if(diff < -1) {
                                                                scores$beds.score[i] <- (diff * 5)
                                                                scores$beds[i] <- paste0(abs(diff)," over request")
                                                        } else {
                                                                scores$beds.score[i] <- -(tf3()$tamano.importance * diff * 5)
                                                                scores$beds[i] <- paste0(abs(diff)," below request")
                                                        }
                                                        
                                                }
                                        }
                                }
                                
                        }
                        
                        #two columns for num.bedrooms
                        
                        scores$baths <- ""
                        scores$baths.score <- 0
                        
                        for(i in 1:length(scrape[,1])) {
                                if(is.na(scrape[i,"total_bathrooms"])) {
                                        scores$baths[i] <- "Numero de baños desconocido"
                                        next
                                } else {
                                        diff <- tf3()$banos - scrape[i,"total_bathrooms"]
                                        if(diff==0) {scores$baths[i] <- "Match"} else {
                                                if(diff %in% c(-1,-1.5)) {
                                                        scores$baths.score[i] <- diff
                                                        scores$baths[i] <- paste0(abs(diff)," over request")
                                                } else {
                                                        if(diff < -1.5) {
                                                                scores$baths.score[i] <- diff * 2.5
                                                                scores$baths[i] <- paste0(abs(diff)," over request")
                                                        } else {
                                                                scores$baths.score[i] <- -(tf3()$tamano.importance * diff * 2.5)
                                                                scores$baths[i] <- paste0(abs(diff)," below request")
                                                        }
                                                        
                                                }
                                        }
                                }
                                
                        }
                        
                        ###############################################################
                        ##PARKING
                        ###############################################################
                        
                        #two cols for parking
                        
                        scores$parking <- ""
                        scores$parking.score <- 0
                        
                        for(i in 1:length(scrape[,1])) {
                                if(is.na(scrape[i,"num_parking_spaces"])) {
                                        scores$parking[i] <- "Numero de estacionamientos desconocido"
                                        
                                } else {
                                        diff <- tf3()$num.parking - scrape[i,"num_parking_spaces"]
                                        if(diff==0) {scores$parking[i] <- "Match"} else {
                                                if(diff ==-1) {scores$parking[i] <- paste0(abs(diff)," over request")
                                                } else {
                                                        if(diff < -1) {
                                                                scores$parking.score[i] <- diff
                                                                scores$parking[i] <- paste0(abs(diff)," over request") ## 33
                                                        } else {
                                                                scores$parking.score[i] <- -(diff * 5)
                                                                scores$parking[i] <- paste0(abs(diff)," below request")
                                                        }
                                                        
                                                }
                                        }
                                }
                                
                        }
                        
                        ###############################################################
                        ##AMENITIES
                        ###############################################################
                        #two cols for amens
                        
                        scores$missing.amenities <- ""
                        scores$amenity.score <- 0
                        
                        amen.string <- unlist(strsplit(tf3()$amenidades.all,split=","))
                        #remove actual or text-based "NA"
                        amen.string <- amen.string[which(!is.na(amen.string) & amen.string!= "NA")]
                        
                        for(i in 1:length(scrape[,1])) {
                                for(j in 1:length(amen.string)) {
                                        if(grepl(amen.string[j],scrape[i,"amenities"])==TRUE) {next} else {
                                                scores$missing.amenities[i] <- paste0(scores$missing.amenities[i],amen.string[j],", ")
                                                scores$amenity.score[i] <- scores$amenity.score[i] - tf3()$amen.importance
                                        }
                                }
                                scores$missing.amenities[i] <- gsub(", $","",scores$missing.amenities[i])
                        }
                        
                        ###############################################################
                        ##PETS
                        ###############################################################
                        ##Property-specific pet scores. Allows for re-running tf for all prop types.
                        
                        scores$pets <- ""
                        scores$pets.score <- 0
                        
                        if(tf3()$mascotas == "No") {
                                scores$pets <- "Client does not have pets"} else {
                                        
                                        for(i in 1:length(scores[,1])) {
                                                
                                                if(scores$type[i] == "Casa") {scores$pets[i] <- "Property type is House"} else {
                                                        
                                                        if(grepl("mascotas permitidas",scrape[i,"amenities"])) {
                                                                scores$pets[i] <- "Mascotas Permitidas"
                                                        } else {
                                                                if(grepl("no se aceptan mascotas",scrape[i,"amenities"])) {
                                                                        scores$pets[i] <- "No se aceptan mascotas"
                                                                        scores$pets.score[i] <- -50
                                                                } else {
                                                                        scores$pets[i] <- "Desconocido"
                                                                }
                                                        }
                                                }
                                        }
                                }
                        
                        ##############################################################
                        #DATE PUBLISHED
                        ##############################################################
                        ##date_published
                        scores$date_published <- as.character(scrape$date_published2)
                        scores$date_published.score <- 0
                        #diff <- as.numeric(as.Date(today) - as.Date(scrape$date_published2[1])) ##remove this
                        for(i in 1:length(scrape[,1])) {
                                #scores$date_published[i] <- as.character(scrape$date_published2[i])
                                
                                diff <- as.numeric(today - scrape$date_published2[i]) #<-- AND HERE... MAKE SURE EVERTHING IS A DATE...
                                
                                if(diff >= 365*3) {
                                        scores$date_published.score[i] <- -30
                                } else {
                                        if(diff >= 365 *2) {
                                                scores$date_published.score[i] <- -20
                                        } else {
                                                if(diff >= 365) {
                                                        scores$date_published.score[i] <- -10
                                                } else {next}
                                        }
                                }
                        }
                        
                        # ###############################################################
                        # ##AGE
                        ###############################################################
                        #two cols for age
                        
                        scores$age <- ""
                        scores$age.score <- 0
                        
                        edad <- as.character(tf3()$edad)
                        
                        age.buckets <- c(1,5,10,20,30,50,1000)
                        af <- tf3()$age.fix
                        next.age <- age.buckets[which(age.buckets==af)+1]  #this gets the next upper age limit
                        
                        
                        if(edad=="Cualquier edad") {
                                scores$age <- "Client answered 'Cualquier edad'"
                        }
                        else {
                                for(i in 1:length(scrape[,1])) {
                                        
                                        year <- year(now()) - tf3()$age.fix
                                        
                                        if(scrape[i,"year_built2"]==0) {
                                                scores$age[i] <- "Edad desconocido [Blank]"
                                        } else {
                                                if(scrape[i,"year_built2"]=="FIX") {
                                                        scores$age[i] <- paste0("Edad desconocido ['",scrape[i,"year_built"],"']")
                                                } else {
                                                        if(scrape[i,"year_built2"] == 1) {
                                                                scores$age[i] <- "Match [A estrenar]"
                                                        } else {
                                                                if(scrape[i,"year_built2"] == -1) {
                                                                        scores$age[i] <- "Match [En construcción]"
                                                                } else {
                                                                        if(scrape[i,"year_built2"] >= year) {
                                                                                scores$age[i] <- paste0("Match [",scrape[i,"year_built2"],"]")
                                                                        } else {
                                                                                diff <- year(now()) - as.numeric(scrape[i,"year_built2"])
                                                                                scores$age[i] <- paste0(diff," years old")
                                                                                if(diff >= tf3()$age.fix) {
                                                                                        request <- which(age.buckets==af) #this is index number of age bucket (requested)
                                                                                        actual <- min(which(diff < age.buckets)) #this is index number of age bucket (actual)
                                                                                        
                                                                                        scores$age.score[i] <-  -((actual - request) * tf3()$edad.importance)
                                                                                }
                                                                                
                                                                        }
                                                                }
                                                        }
                                                }
                                        }
                                        
                                        
                                }
                        }
                        
                        ##############################################################
                        #PREP FOR SCORES
                        ##############################################################
                        
                        ##flags!! 2 cols
                        
                        col3 <- which(names(scores)=="type.score")
                        col4 <- which(names(scores)=="age.score")
                        
                        scores$missing_property_data <- "Missing Property Data: "
                        for(i in 1:length(scores[,1])) {
                                for(j in col3:col4) {
                                        if(grepl("[D|d]esconocido",scores[i,j])) {
                                                scores$missing_property_data[i] <- paste(scores$missing_property_data[i],names(scores)[j],", ")
                                        }
                                }
                                scores$missing_property_data[i] <- gsub(", $","",scores$missing_property_data[i])
                        }
                        
                        scores$missing_property_data[which(scores$missing_property_data=="Missing Property Data: ")] <- ""
                        
                        scores$EB_ID_Number <- ""
                        scores$URL <- ""
                        
                        for(i in 1:length(scores[,1])) {
                                scores$EB_ID_Number[i] <- scrape$id_number[i]
                                scores$URL[i] <- scrape$property_url[i]
                        }
                        
                        scores$long.lat <- ""
                        scores$realtor_company <- ""
                        scores$property_price <- 0
                        
                        for(i in 1:length(scrape[,1])) {
                                scores$long.lat[i] <- paste0(scrape$location_longitude[i],",",scrape$location_latitude[i])
                                scores$realtor_company[i] <- scrape$realtor_company[i]
                                scores$property_price[i] <- scrape$price2[i]
                        }
                        
                        
                        if(tf3()$min.price==tf3()$max.price) {
                                for(i in 1:length(scores[,1])) {
                                        if(scores$property_price[i]==tf3()$min.price) {
                                                scores$price[i] <- "Exact match to unmodified price request"
                                                scores$price.score[i] <- 0
                                        }
                                }
                        }
                        
                        scores$property_price <- round(scores$property_price,0)
                        scores$property_price <- format(scores$property_price,scientific = FALSE,big.mark = ",",nsmall=0)
                        
                        
                        ###############################################################
                        ##SCORES, RANKS and FLAGS
                        ###############################################################
                        score.cols <- which(grepl("score",names(scores)))
                        score.cols <- score.cols[score.cols <= col4]
                        score.cols.no.age <- score.cols[-length(score.cols)]
                        
                        scores$score.with.age <- 0
                        scores$score.without.age <- 0
                        
                        for(i in 1:length(scores[,1])) {
                                scores$score.with.age[i] <- round(sum(scores[i,score.cols]),1)
                        }
                        
                        
                        for(i in 1:length(scores[,1])) {
                                scores$score.without.age[i] <- round(sum(scores[i,score.cols.no.age]),1)
                        }
                        
                        scores$score.without.age.inverse <- 100 + scores$score.without.age
                        scores$score.with.age.inverse <- 100 + scores$score.with.age
                        
                        
                        
                        ################# <----------------------come back to this
                        ##tiny cleaning step.  This removes properties in delegacion "la paz"
                        ##when the selected colony is 'santa fe' or 'centro'
                        
                        # if(any(tf[1,5:7]==santafe)) {
                        #         scores <- scores[which(!(grepl(", la paz$",scores$property.location))),]
                        # }
                        #
                        # if(any(tf[1,5:7]=="centro"|any(tf[1,2:4]=="Centro"))) {
                        #         scores <- scores[which(!(grepl(", la paz$",scores$property.location))),]
                        # }
                        
                        
                        
                        scores <- arrange(scores,desc(score.without.age.inverse),date_published)
                        
                        scores$rank.without.age <- 1
                        for(i in 2:length(scores[,1])) {
                                if(scores$score.without.age.inverse[i] == scores$score.without.age.inverse[i-1]) {
                                        scores$rank.without.age[i] <- scores$rank.without.age[i-1]
                                } else {
                                        scores$rank.without.age[i] <- scores$rank.without.age[i-1] + 1
                                }
                        }
                        
                        scores$rank.with.age <- 1
                        for(i in 2:length(scores[,1])) {
                                if(scores$score.with.age.inverse[i] == scores$score.with.age.inverse[i-1]) {
                                        scores$rank.with.age[i] <- scores$rank.with.age[i-1]
                                } else {
                                        scores$rank.with.age[i] <- scores$rank.with.age[i-1] + 1
                                }
                        }
                        
                        
                        scores2 <- scores[1:100,]
                        #names(scores2)
                        
                        scores2 <- select(scores2,25,32,33,24,26,28,29,1:23,30,31,34,35,26,27)
                        
                        ##check for possible/probable duplicates
                        
                        scores2$duplicates <- ""
                        for(i in 1:50) {
                                second <- i+(i-1)
                                first <- second - 1
                                third <- second + 1
                                if(i==1) {match1 <- 0} else {
                                        match1 <- sum(scores2[first,2:35]==scores2[second,2:35])
                                }
                                
                                match2 <- sum(scores2[second,2:35]==scores2[third,2:35])
                                
                                if(match1 == 33) {scores2$duplicates[first:second] <- "Probable Duplicates"} else {
                                        if(match1 >= 31) {scores2$duplicates[first:second] <- "Possible Duplicates"}
                                }
                                if(match2 == 33) {scores2$duplicates[second:third] <- "Probable Duplicates"} else {
                                        if(match2 >= 31) {scores2$duplicates[second:third] <- "Possible Duplicates"}
                                }
                                
                        }
                        
                        
                        
                        ##check for key word in property descx to check for scams.
                        ##FLAG records with "contado", "inversion", y "inversionista(s)"
                        
                        scores2$possible.ad_or_scam <- ""
                        
                        for(i in 1:length(scores2[,1])) {
                                desc <- scrape[which(scrape$id_number==scores2$EB_ID_Number[i]),"description"]
                                
                                if(length(desc)==0) {next} else {
                                        if(grepl("contado|inversi[óo]n|inversionista",desc,ignore.case = T)) {
                                                scores2$possible.ad_or_scam[i] <- "FLAG: Possible ad or scam"
                                        }
                                }
                        }
                        
                        #
                        
                        scores2 <- select(scores2,1,37,36,2:35)
                        
                        
                        scores2$email <- rep(tf3()$email[1],times = length(scores2[,1]))
                        scores2$typeform.date <- rep(tf3()$submit.date[1],times = length(scores2[,1]))
                        scores2$results.date <- rep(as.character(today),times = length(scores2[,1]))
                        ###############################################################
                        ##Return final score df to be displayed
                        ###############################################################
                        
                        scores <- scores2
                        
                        return(scores)
                        
                })
        )
        
        
        
        output$scores <- renderTable({
                head(scores(),n=50)
        })
        
        
        output$sheetname <- reactive({tf3()$email})
        
        
        
        
        # Downloadable csv of selected dataset ----
        output$downloadData <- downloadHandler(
                
                filename <-  paste0(gsub(" |@|:|/","_",paste0(tf3()$email,"_",tf3()$submit.date,"_app")),".csv",sep=""),
                
                content = function(file) {
                        write.csv(scores(), file, row.names = FALSE)
                })
        
})



