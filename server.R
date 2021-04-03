library(shiny)

# Define server logic required to draw a histogram
function(input,output){
    
    output$plot_rate <- renderPlotly({
        suicide_rate_edit <- suicide_rate %>%
            drop_na(population)
        suicide_rate_edit <- suicide_rate_edit %>%
            mutate(country = as.factor(country),
                   sex = as.factor(sex),
                   age = as.factor(age))
        
        plot_rate <- suicide_rate_edit %>%
            filter(country == input$Negara,
                   age == input$Kategori) %>%
            ggplot(aes(x = year, 
                       y = population, 
                       group = sex, 
                       color = sex,
                       text = glue("Gender: {sex}
                          Jumlah kasus: {population}
                          Tahun: {year}"))) +
            geom_line(show.legend = F) +
            geom_point(show.legend = F) +
            labs(title = "Plot Laju Kasus Bunuh Diri di Suatu Negara Berdasarkan Gender",
                 x = "Tahun",
                 y = "Jumlah kasus bunuh diri",
                 caption = "Source: WHO") +
            theme_algoritma
        
        ggplotly(plot_rate, tooltip = "text")
    })           
    
    
    
    
    
    output$plot_jumlah <- renderPlotly({
        options(scipen = 999)
        jumlah_suicide <- suicide_rate_edit %>%
            filter(year == input$tahun,
                   sex == input$jkel) %>%
            group_by(sex, age, year) %>%
            summarise(jumlah = sum(population))
        
        
        plot_jumlah <- jumlah_suicide %>%
            ggplot(aes(x = jumlah,
                       y = reorder(age, jumlah),
                       fill = age,
                       text = glue("{age}
                         Kasus suicide: {jumlah}"))) +
            geom_col(aes(fill=jumlah), show.legend = F) +
            scale_fill_gradient(low = "#1f0101", high = "#fc0303") +
            labs(title = "Jumlah kasus suicide per kategori umur",
                 x = "Jumlah kasus suicide",
                 y = "kategori umur",
                 caption = "Source: WHO") +
            theme_algoritma
        
        ggplotly(plot_jumlah, tooltip = "text")
        
    })
    
    output$plot_persentasi <- renderPlotly({
        jumlah_suicide_negara <-suicide_rate_edit %>%
            filter(country == input$negara, year == input$Tahun) %>%
            group_by(country, age, year) %>%
            summarise(banyaknya = sum(population))
        
        plot_persentasi <- 
            plot_ly(type='pie', labels=jumlah_suicide_negara$age, values=jumlah_suicide_negara$banyaknya, 
                    textinfo='label+percent',
                    insidetextorientation='radial') %>% 
            layout(title = 'Persentase Kasus Bunuh Diri per Kategori Umur')
        
    }
    
    )
    
    output$plot_top <- renderPlotly({
        kasus <- suicide_rate_edit %>%
            filter(year == input$yr) %>%
            group_by(age, year, country) %>%
            summarise(jumlah = sum(population))
        kasus
        
        kasus_1 <- kasus[order(kasus$jumlah, decreasing = T),]
        
        kasus_1_umur <- kasus_1 %>%
            filter(age == input$ag)
        
        plot_top <- ggplot(data = kasus_1_umur[1:input$banyaknya,],
                           aes(x = jumlah,
                               y = reorder(country, jumlah),
                               text = glue("Jumlah Kasus: {jumlah}"))) +
            geom_col(aes(fill = jumlah), show.legend = F) +
            labs(title = "Negara dengan Kasus Bunuh Diri Tertinggi",
                 x = "Jumlah Kasus",
                 y = "Nama Negara") +
            theme_algoritma
        
        ggplotly(plot_top, tooltip = "text")
        
    }
    
    )
    
    
    output$topneg <- renderValueBox({
        kasus <- suicide_rate_edit %>%
            filter(year == input$yr) %>%
            group_by(age, year, country) %>%
            summarise(jumlah = sum(population))
        kasus
        
        kasus_1 <- kasus[order(kasus$jumlah, decreasing = T),]
        
        kasus_1_umur <- kasus_1 %>%
            filter(age == input$ag)
        
        valueBox(value = kasus_1_umur$country[1],
                 subtitle = "Negara",
                 color = "maroon",
                 icon = icon("globe-americas"))
        
    }
    
    )
    
    
    output$jum <- renderValueBox({
        kasus <- suicide_rate_edit %>%
            filter(year == input$yr) %>%
            group_by(age, year, country) %>%
            summarise(jumlah = sum(population))
        kasus
        
        kasus_1 <- kasus[order(kasus$jumlah, decreasing = T),]
        
        kasus_1_umur <- kasus_1 %>%
            filter(age == input$ag)
        
        valueBox(value = kasus_1_umur$jumlah[1],
                 subtitle = "Frekuensi",
                 color = "maroon",
                 icon = icon("sort-amount-down"))
        
    }
    
    )
    
    
    output$data_suicides <- renderDataTable({
        DT::datatable(data = suicide_rate_edit, options = list(scrollX = T))
        
        
    })
    
    
    output$plot_sentimen <- renderPlotly({
        
        inFile <- input$file1
        
        if (is.null(inFile))
            return(NULL)
        
        depression_test <- read.csv(inFile$datapath, header = input$header)
        
        test <-  as.data.frame(depression_test %>%
                                         mutate(text = text %>%
                                                    str_to_lower() %>% # transform menjadi huruf kecil
                                                    replace_url()  %>% 
                                                    replace_html() %>% 
                                                    replace_contraction() %>%
                                                    replace_word_elongation() %>% 
                                                    replace_internet_slang() %>% 
                                                    replace_emoji(.) %>% 
                                                    replace_emoticon(.) %>% 
                                                    str_remove_all(pattern = "[[:digit:]]") %>% # remove number
                                                    str_remove_all(pattern = "[[:punct:]]") %>% 
                                                    str_remove_all(pattern = "%") %>%
                                                    str_remove_all(pattern = "\\$") %>% # remove dollar sign
                                                    str_remove_all(pattern = "\\|") %>%
                                                    str_remove_all(pattern = "\\=") %>%
                                                    str_remove_all('[\\&]+') %>% 
                                                    str_remove_all('[\\"]+') %>% 
                                                    str_remove_all(pattern = "\\+") %>%
                                                    str_remove_all(pattern = "\\>") %>%
                                                    str_remove_all(pattern = "\\<") %>%
                                                    str_squish()
                                         ))
        
        
        depresi.test.dtm <- DocumentTermMatrix(VCorpus(VectorSource(test$text)))
        
        not_include <- depresi.train.dtm$dimnames$Terms[!depresi.train.dtm$dimnames$Terms %in% depresi.test.dtm$dimnames$Terms]
        
        dummy_empty <- train_rf %>% 
            select(not_include) %>% 
            head(1) %>% 
            mutate_all(function(x) 0)
        
        test_table <- as.data.frame(as.matrix(depresi.test.dtm)) %>% 
            bind_cols(dummy_empty)
        
        forest_class <- predict(depression_forest, test_table, type = "raw")
        
        
        prediksi <- data.frame(forest_class) %>% `colnames<-`("predict")
        
        test_predict <- data.frame("text" = test, "predict" = prediksi)
        
        freq_pred <- prediksi %>% group_by(predict) %>% summarise(total = n())
        
        plot_sentiment <- ggplot(data = freq_pred ,mapping = aes(x = predict, y = total, text = glue(
            "jumlah: {total}"))) +
            geom_col(aes(fill = predict))
        
        ggplotly(plot_sentiment, tooltip = "text")
        
        
        
    })
    
    output$plot_word <- renderPlot({
        
        inFile <- input$file1
        
        if (is.null(inFile))
            return(NULL)
        
        depression_test <- read.csv(inFile$datapath, header = input$header)
        
        test <-  as.data.frame(depression_test %>%
                                   mutate(text = text %>%
                                              str_to_lower() %>% # transform menjadi huruf kecil
                                              replace_url()  %>% 
                                              replace_html() %>% 
                                              replace_contraction() %>%
                                              replace_word_elongation() %>% 
                                              replace_internet_slang() %>% 
                                              replace_emoji(.) %>% 
                                              replace_emoticon(.) %>% 
                                              str_remove_all(pattern = "[[:digit:]]") %>% # remove number
                                              str_remove_all(pattern = "[[:punct:]]") %>% 
                                              str_remove_all(pattern = "%") %>%
                                              str_remove_all(pattern = "\\$") %>% # remove dollar sign
                                              str_remove_all(pattern = "\\|") %>%
                                              str_remove_all(pattern = "\\=") %>%
                                              str_remove_all('[\\&]+') %>% 
                                              str_remove_all('[\\"]+') %>% 
                                              str_remove_all(pattern = "\\+") %>%
                                              str_remove_all(pattern = "\\>") %>%
                                              str_remove_all(pattern = "\\<") %>%
                                              str_squish()
                                   ))
        
        
        depresi.test.dtm <- DocumentTermMatrix(VCorpus(VectorSource(test$text)))
        
        not_include <- depresi.train.dtm$dimnames$Terms[!depresi.train.dtm$dimnames$Terms %in% depresi.test.dtm$dimnames$Terms]
        
        dummy_empty <- train_rf %>% 
            select(not_include) %>% 
            head(1) %>% 
            mutate_all(function(x) 0)
        
        test_table <- as.data.frame(as.matrix(depresi.test.dtm)) %>% 
            bind_cols(dummy_empty)
        
        forest_class <- predict(depression_forest, test_table, type = "raw")
        
        
        prediksi <- data.frame(forest_class) %>% `colnames<-`("predict")
        
        test_predict <- data.frame("text" = test, "predict" = prediksi)
        
        word_aa <- test_predict %>%
            unnest_tokens(word, text.text)%>%
            mutate(word = textstem::lemmatize_words(word)) %>%
            anti_join(stop_words) %>%
            count(word, predict, sort = T) %>%
            group_by(predict) %>%
            top_n(1000)
        
        
        library(ggthemes)
        
        ggplot(head(word_aa %>% filter(predict == input$kondisi), input$berapa), aes(label = word)) +
            ggwordcloud::geom_text_wordcloud(aes(size=n), color = "#8B0000") +
            scale_size_area(max_size = 12) +
            labs(title = "Suicidal's Wordcloud") +
            theme_minimal() 
        
        
        
        
        
        
    })
    
}

