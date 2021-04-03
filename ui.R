library(shiny)

# Define UI for application that draws a histogram
header <- dashboardHeader(title = "Suicide Rates Analysis")

sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem(text = "Business Case", 
                 tabName = "rate", 
                 icon = icon("globe")),
        menuItem(text = "Plot", 
                 tabName = "jumlah", 
                 icon = icon("chart-line")),
        menuItem(text = "Prediksi", 
                 tabName = "predict", 
                 icon = icon("hand-holding-medical")),
        menuItem(text = "Dataset", 
                 tabName = "data", 
                 icon = icon("database"))
    )
)

body <- dashboardBody(
    tabItems(
        
        
        tabItem(tabName = "jumlah", 
                fluidRow(
                    tabBox(id="tabchart1", width = 12, height = "1000px", 
                           tabPanel("SuicideRate",
                                    box(title = "Input",
                                        background = "black",
                                        width = 4,
                                        selectInput(inputId = "Kategori", 
                                                    label = "Pilih Kategori Umur",
                                                    choices = unique(suicide_rate_edit$age)),
                                        selectInput(inputId = "Negara", 
                                                    label = "Pilih Negara",
                                                    choices = unique(suicide_rate_edit$country))
                                        
                                    ),
                                    box(title = "Plot",
                                        background = "maroon",
                                        width = 10,
                                        solidHeader = TRUE,
                                        plotlyOutput(outputId = "plot_rate"))),
                           tabPanel("Jumlah Kasus",
                                    selectInput(inputId = "tahun", 
                                                label = "Select Year",
                                                choices = unique(suicide_rate_edit$year)),
                                    checkboxGroupInput(inputId = "jkel",
                                                       label = "Jenis Kelamin",
                                                       choices = unique(suicide_rate_edit$sex),
                                                       selected = unique(suicide_rate_edit$sex)),
                                    plotlyOutput(outputId = "plot_jumlah")),
                           tabPanel("Persentase Kasus", 
                                    selectInput(inputId = "Tahun", 
                                                label = "Select Year",
                                                choices = unique(suicide_rate_edit$year)),
                                    selectInput(inputId = "negara", 
                                                label = "Select Country",
                                                choices = unique(suicide_rate_edit$country)),
                                    plotlyOutput(outputId = "plot_persentasi")
                           ),
                           tabPanel("Negara dengan Kasus Tertinggi", 
                                    selectInput(inputId = "yr", 
                                                label = "Pilih Tahun",
                                                choices = unique(suicide_rate_edit$year)),
                                    selectInput(inputId = "ag", 
                                                label = "Pilih Kategori Umur",
                                                choices = unique(suicide_rate_edit$age)),
                                    numericInput(inputId = "banyaknya", 
                                                 label = "Pilih Top n", 
                                                 value = 15,
                                                 min = 10, 
                                                 max = 50, 
                                                 step = 1),
                                    plotlyOutput(outputId = "plot_top"),
                                    valueBoxOutput("topneg",
                                                   width = 8),
                                    valueBoxOutput("jum")
                                    
                           )
                    )
                )
                
        ),
        tabItem(tabName = "predict", 
                h1("Sentiment Dashboard", align = "center"),
                fluidRow(
                    box(title = "Input",
                        background = "black",
                        width = 4,
                        fileInput("file1", "Choose CSV File",
                                  accept = c(
                                      "text/csv",
                                      "text/comma-separated-values,text/plain",
                                      ".csv")
                        ),
                        tags$hr(),
                        checkboxInput("header", "Header", TRUE)
                    )
                    
                    
                    ,
                    box(title = "Plot",
                        background = "maroon",
                        width = 8,
                        solidHeader = TRUE,
                        plotlyOutput(outputId = "plot_sentimen")),
                    
                    box(title = "Word",
                        background = "blue",
                        width = 10,
                        solidHeader = TRUE,
                        selectInput(inputId = "kondisi", 
                                    label = "Pilih Kategori",
                                    choices = unique(depresi$class)),
                        sliderInput(inputId = "berapa",
                                    "Frequency:",
                                    min = 1,  max = 40, value = 25),
                        plotOutput(outputId = "plot_word"))
                )
        ),
        tabItem(tabName = "data", 
                dataTableOutput(outputId = "data_suicides"))
        
        
    )
)

dashboardPage(
    header = header,
    body = body,
    sidebar = sidebar, 
    skin = "blue"
)
