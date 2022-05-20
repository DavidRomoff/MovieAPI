
library(shiny)
library(shinythemes)
library(RCurl)
library(RJSONIO)
library(stringr)

str = as.character(Sys.time())
str = str_replace_all(str,'-','_')
str = str_replace_all(str,' ','_')
str = str_replace_all(str,':','_')
savename = str
savename_script = paste0(str,'_script.R')

mymovies = c(
  'Superman',
  'Batman',
  'Thor',
  'Captain America',
  'Iron Man'
)

mymovie = sample(mymovies,1)

ui <- fluidPage(
  theme = shinytheme("cyborg"),
  shiny::HTML(text = '<br>'),
  sidebarLayout(
    sidebarPanel(
      textInput(inputId = 'moviename',label = 'Movie Name',value = mymovie),
      textInput(inputId = 'apikey',label = 'API Key',value = ''),
      actionButton(inputId = 'action_query',label = 'Query!'),
      shiny::HTML(text = '<br>'),
      shiny::HTML(text = '<br>'),
      a("Get your own Free API key", href="http://www.omdbapi.com/apikey.aspx")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel('Query',
                 verbatimTextOutput(outputId = "url_query")),
        tabPanel('Raw',
                 verbatimTextOutput(outputId = "data_web")),
        tabPanel('JSON',
                 verbatimTextOutput(outputId = "data_json")),
        tabPanel('Lookup',
                 selectInput(inputId = 'field',label = 'Select Field',choices = '',selected = ''),
                 verbatimTextOutput(outputId = 'field_data')),
        tabPanel('Poster',
                 textInput(inputId = 'postersavename',label = 'Save Name',value = ''),
                 actionButton(inputId = 'action_posterdownload',label = 'Download!'),
                 imageOutput(outputId = 'image_poster')),
        tabPanel('Code',
                 shiny::HTML(text = '<br>'),
                 shiny::HTML(text = '<br>'),
                 verbatimTextOutput(outputId = 'code'),
                 textInput(inputId = 'savename_code',label = 'Save name',value = savename_script),
                 downloadButton("download_script")
        ),
        tabPanel('Info',
                 shiny::HTML(text = '<br>'),
                 shiny::HTML(text = '<br>'),
                 fluidRow(align="center",uiOutput("tab"))),
                 
      )
    )
  )

)

server <- function(input, output, session) {
  
  URL_Query = eventReactive(input$action_query,{
    movie_name = input$moviename
    api_key = input$apikey
    url_0 = URLencode(paste0("www.omdbapi.com/?t=", movie_name, '&type=movie&tomatoes=true'))
    url_1 = paste0(url_0,'&apikey=',api_key)
    url_1
  })
  
  Data_Web = reactive({
    data_web  = getURL(URL_Query())
    data_web
  })
  
  Data_JSON = reactive({
    data_json = fromJSON(Data_Web())
    data_json
  })
  
  observeEvent(input$action_query,{
    obj_list = Data_JSON()
    fields = names(obj_list)
    updateSelectInput(session = session,inputId = 'field',choices = fields,selected = fields[1])
    
    poster_savename = obj_list[['Title']]
    poster_savename = stringr::str_replace_all(string = poster_savename,pattern = "[[:punct:]]", "")
    poster_savename = stringr::str_replace_all(string = poster_savename,pattern = ' ',replacement = '_')
    updateTextInput(session = session,inputId = 'postersavename',value = poster_savename)
  })
  
  output$field_data = renderPrint({
    obj_list = Data_JSON()
    field = input$field
    textout = obj_list[[field]]
    cat(textout)
  })
  
  observeEvent(input$action_posterdownload,{
    obj_list = Data_JSON()
    poster_url = obj_list[['Poster']]
    savename = input$postersavename
    download.file(url = poster_url,destfile = paste0(savename,'.jpg'))
  })
  
  
  
  output$image_poster = renderImage({
    input$action_posterdownload
    savename = input$postersavename
    list(
      src = paste0(savename,'.jpg'),
      contentType = "jpg"
    )
  })
  
  output$url_query = renderPrint(URL_Query())
  output$data_web = renderPrint(Data_Web())
  output$data_json = renderPrint(Data_JSON())
  
  Obj_code = reactive({
    paste0("
library(RCurl)
library(RJSONIO)
library(stringr)

movie_name = '",input$moviename,"'
api_key = '",input$apikey,"'

url_0 = URLencode(paste0('www.omdbapi.com/?t=', movie_name, '&type=movie&tomatoes=true'))
url_1 = paste0(url_0,'&apikey=',api_key)

data_web  = getURL(url_1)
data_json = fromJSON(data_web)

poster_savename = data_json[['Title']]
poster_savename = stringr::str_replace_all(string = poster_savename,pattern = '[[:punct:]]', '')
poster_savename = stringr::str_replace_all(string = poster_savename,pattern = ' ',replacement = '_')
poster_savename = paste0(poster_savename,'.jpg')
poster_url = data_json[['Poster']]
download.file(url = poster_url,destfile = poster_savename)


",sep='')
    
  })
  output$download_script <- downloadHandler(
    filename = function() {
      input$savename_code
    },
    content = function(file) {
      capture.output(cat(Obj_code()),file = file)
    }
  )
  
  output$code = renderPrint(cat(Obj_code()))
  
  url <- a("OMDb API", href="http://www.omdbapi.com/")
  output$tab <- renderUI({
    tagList("URL link:", url)
  })
}

shinyApp(ui, server)