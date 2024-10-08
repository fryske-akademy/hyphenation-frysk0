library(shiny)
library(shinyjs)
library(shinyWidgets)
library(shinysky)
library(shinyAce)
library(readr)
library(readtext)
library(xml2)
library(rvest)
library(openxlsx)
library(stringr)
library(udpipe)
library(ipa)
library(ggplot2)

################################################################################

ui <- tagList(
  useShinyjs(),

  includeCSS("www/styles.css"), extendShinyjs(script = "extend.js", functions = "clearUpload"),
  titlePanel(title = HTML("<div class='title'>G2P Frysk<div>"), windowTitle = "G2P Frysk"),

  tags$head(
    tags$link(rel="icon", href="FA2.png"),

    tags$meta(charset="UTF-8"),
    tags$meta(name   ="description", content="Hyphenation Frysk is a web app for hyphenating words in Frisian text."),
  ),

  navbarPage
  (
    title=NULL, id = "navBar", collapsible = TRUE,

    tabPanel
    (
      title = HTML("<span class='glyphicon glyphicon-cog' style='font-size: 100%'></span>&nbsp;Run"),
      value = "run",

      fluidPage
      (
        style = "border: 1px solid silver; min-height: calc(100vh - 203px); background-color: #eeeefe",

        br(),

        fluidPage(
          align="left",
          style='max-width: 880px;',

          p("This service is freely available. If you use this service, you agree that data obtained by us during such use can be used for further improvements of this system."),
        ),

        align="center",
        br(),

        HTML("<p style='font-weight: bold;'>Select Input:</p>"),
        br(),

        icon(NULL),

        radioGroupButtons(
          inputId    = "selInput",
          label      = NULL,
          choices    = c("<i class='glyphicon glyphicon-font' ></i> Text" = "Text" ,
                         "<i class='glyphicon glyphicon-file' ></i> File" = "File" ,
                         "<i class='glyphicon glyphicon-globe'></i> Web"  = "Web" ),
          individual = TRUE,
          selected   = "Text"
        ),

        br(),
        uiOutput("getInput"),
        br(),

        shiny::actionButton("clearButton", HTML("<span class='glyphicon glyphicon-erase' style='font-size: 90%'></span>&nbsp;Clear")),

        br(), br(), br(),
        busyIndicator(text = NULL, wait = 1000),
        uiOutput("showResults"),
        br(), br(), br(), br()
      ),

      br()
    ),

    tabPanel
    (
      title = HTML("<span class='glyphicon glyphicon-info-sign' style='font-size: 100%'></span>&nbsp;About"),
      value = "about",

      fluidPage
      (
        style = "border: 1px solid silver; min-height: calc(100vh - 203px); background-color: #eeeefe",

        br(),

        h5(strong("About")),
        p("Hyphenation Frysk is a web app for converting Frisian text to phonetic IPA transcriptions. The following people were involved in the development of Hyphenation Frysk: Wilbert Heeringa (Fryske Akademy, implementation G2P Frysk), Eduard Drenth (data). Comments are welcome and can be sent to", img(src = 'email.png', height = 19, align = "center"),"."),
        br(),

        h5(strong("System requirements")),
        p("Hyphenation Frysk runs best on a computer with a monitor with a minimum resolution of 1370 x 870 (width x height). The use of Chrome, Chromium, Firefox or Opera as a web browser is to be preferred."),
        br(),

        h5(strong("Implementation")),
        p("The model underlying this app is trained on the basis of the Foarkarswurdlist. The model was trained using the", a("Python wrapper", href = "https://pypi.org/project/phonetisaurus/", target = "_blank"), "of the ", a("Phonetisaurus", href = "https://github.com/AdolfVonKleist/Phonetisaurus", target = "_blank"), "grapheme to phoneme tool. In the app itself the model is used by Phonetisaurus in order to convert words without hyphenation into words with hyphenation. The interface of Hyphenation Frysk is implemented as a Shiny app. Shiny was developed by RStudio. This app uses the following R packages:"),
        br(),

        tags$div(tags$ul
        (
          tags$li(tags$span(HTML("<span style='color:blue'>base</span>"), p("R Core Team (2017). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. https://www.R-project.org/"))),
          tags$li(tags$span(HTML("<span style='color:blue'>shiny</span>"), p("Winston Chang, Joe Cheng, J.J. Allaire, Yihui Xie and Jonathan McPherson (2017). shiny: Web Application Framework for R. R package version 1.0.0. https://CRAN.R-project.org/package=shiny"))),
          tags$li(tags$span(HTML("<span style='color:blue'>shinyjs</span>"), p("Dean Attali (2018). shinyjs: Easily Improve the User Experience of Your Shiny Apps in Seconds. R package version 1.0. https://CRAN.R-project.org/package=shinyjs"))),
          tags$li(tags$span(HTML("<span style='color:blue'>shinyWidgets</span>"), p("Victor Perrier, Fanny Meyer and David Granjon (2019). shinyWidgets: Custom Inputs Widgets for Shiny. R package version 0.5.0. https://CRAN.R-project.org/package=shinyWidgets"))),
          tags$li(tags$span(HTML("<span style='color:blue'>shinysky</span>"), p("Dai ZJ (2019). shinysky: A Set of Shiny Components and Widgets. R package version 0.1.3. https://github.com/AnalytixWare/ShinySky"))),
          tags$li(tags$span(HTML("<span style='color:blue'>shinyAce</span>"), p("Vincent Nijs, Forest Fang, Trestle Technology, LLC and Jeff Allen (2019). shinyAce: Ace Editor Bindings for Shiny. R package version 0.4.1. https://CRAN.R-project.org/package=shinyAce"))),
          tags$li(tags$span(HTML("<span style='color:blue'>readr</span>"), p("Hadley Wickham, Jim Hester and Romain Francois (2018). readr: Read Rectangular Text Data. R package version 1.3.1. https://CRAN.R-project.org/package=readr"))),
          tags$li(tags$span(HTML("<span style='color:blue'>readtext</span>"), p("Benoit K, Obeng A (2021). _readtext: Import and Handling for Plain and Formatted Text Files_. R package version 0.81, https://CRAN.R-project.org/package=readtext"))),
          tags$li(tags$span(HTML("<span style='color:blue'>xml2</span>"), p("Hadley Wickham, Jim Hester and Jeroen Ooms (2021). xml2: Parse XML. R package version 1.3.3. https://CRAN.R-project.org/package=xml2"))),
          tags$li(tags$span(HTML("<span style='color:blue'>rvest</span>"), p("Hadley Wickham (2021). rvest: Easily Harvest (Scrape) Web Pages. R package version 1.0.2. https://CRAN.R-project.org/package=rvest"))),
          tags$li(tags$span(HTML("<span style='color:blue'>openxlsx</span>"), p("Philipp Schauberger and Alexander Walker (2020). openxlsx: Read, Write and Edit xlsx Files. R package version 4.2.3. https://CRAN.R-project.org/package=openxlsx"))),
          tags$li(tags$span(HTML("<span style='color:blue'>stringr</span>"), p("Hadley Wickham (2019). stringr: Simple, Consistent Wrappers for Common String Operations. R package version 1.4.0. https://CRAN.R-project.org/package=stringr"))),
          tags$li(tags$span(HTML("<span style='color:blue'>tokenizers</span>"), p('Lincoln A. Mullen et al., "Fast, Consistent Tokenization of Natural Language Text," Journal of Open Source Software 3, no. 23 (2018): 655, https://doi.org/10.21105/joss.00655.'))),
          tags$li(tags$span(HTML("<span style='color:blue'>ipa</span>"), p("Alexander Rossell Hayes (2020). ipa: convert between phonetic alphabets. R package version 0.1.0. https://github.com/rossellhayes/ipa"))),
          tags$li(tags$span(HTML("<span style='color:blue'>ggplot2</span>"), p("H. Wickham. ggplot2: Elegant Graphics for Data Analysis. Springer-Verlag New York, 2016.")))
        )),

        br(),
        p("The icons used in this app are glyphs taken from the set of", a("Bootstrap Glyphicons", href = "https://getbootstrap.com/docs/3.3/components/", target = "_blank"), "which includes over 250 glyphs from the", a("Glyphicon", href = "https://glyphicons.com/", target = "_blank"), "Halflings set."),
        br(),
        br()
      ),

      br()
    ),

    tabPanel
    (
      title = HTML("<span class='glyphicon glyphicon-exclamation-sign' style='font-size: 100%'></span>&nbsp;Disclaimer"),
      value = "disclaimer",

      fluidPage
      (
        style = "border: 1px solid silver; min-height: calc(100vh - 203px); background-color: #eeeefe",

        br(),
        h5(strong("Liability")),
        p("This app is provided 'as is' without warranty of any kind, either express or implied, including, but not limited to, the implied warranties of fitness for a purpose, or the warranty of non-infringement. Without limiting the foregoing, the Fryske Akademy makes no warranty that: 1) the app will meet your requirements, 2) the app will be uninterrupted, timely, secure or error-free, 3) the results that may be obtained from the use of the app will be effective, accurate or reliable, 4) the quality of the app will meet your expectations, 5) any errors in the app will be corrected."),
        br(),
        p("The app and its documentation could include technical or other mistakes, inaccuracies or typographical errors. The Fryske Akademy may make changes to the app or documentation made available on its web site. The app and its documentation may be out of date, and the Fryske Akademy makes no commitment to update such materials."),
        br(),
        p("The Fryske Akademy assumes no responsibility for errors or ommissions in the app or documentation available from its web site."),
        br(),
        p("In no event shall the Fryske Akademy be liable to you or any third parties for any special, punitive, incidental, indirect or consequential damages of any kind, or any damages whatsoever, including, without limitation, those resulting from loss of use, data or profits, whether or not the Fryske Akademy has been advised of the possibility of such damages, and on any theory of liability, arising out of or in connection with the use of this software."),
        br(),
        p("The use of the app is done at your own discretion and risk and with agreement that you will be solely responsible for any damage to your computer system or loss of data that results from such activities. No advice or information, whether oral or written, obtained by you from the Fryske Akademy shall create any warranty for the software."),
        br(),
        h5(strong("Other")),
        p("The disclaimer may be changed from time to time."),
        br()
      ),

      br()
    )
  ),

  tags$footer
  (
    tags$table(style = "width:100%",
               tags$tr
               (
                 tags$td(tags$a(tags$img(src="FA1.png", style = "height: 35px; margin-top: 9px; margin-left : 22px;"),
                                href    = "https://www.fryske-akademy.nl/en/"),
                                style   = "width: 40%; text-align: left;",
                                class   = "balk",
                                onclick = "window.open('https://www.fryske-akademy.nl/en/', '_blank'); return false;",
                                target  = "_blank")
               )
    )
  )
)

################################################################################

server <- function(input, output, session)
{
  observeEvent(input$navBar,
  {
    if (getUrlHash() == paste0("#", input$navBar)) return()
    updateQueryString(paste0("#", input$navBar), mode = "push")
  })

  observeEvent(getUrlHash(),
  {
    Hash <- getUrlHash()
    if (Hash == paste0("#", input$navBar)) return()
    Hash <- gsub("#", "", Hash)
    updateNavbarPage(session, "navBar", selected=Hash)
  })

  ##############################################################################

  global <- reactiveValues(firstFY=TRUE, firstNL=TRUE, firstEN=TRUE, UDfy=NULL, UDnl=NULL, UDen=NULL, model=NULL)

  global$model <- udpipe_load_model(file = paste0("www/frisian_frysk-ud-1.00-240313.udpipe"))
  
  output$aceEditor <- renderUI(
  {
    aceEditor(
      outputId            = "ace",
      value               = "",
      placeholder         = "Enter your text here.",
      mode                = "plain_text",
      height              = "200px",
      fontSize            = 17,
      wordWrap            = TRUE,
      showLineNumbers     = FALSE,
      highlightActiveLine = FALSE,
      showPrintMargin     = FALSE
    )
  })

  fileContent <- reactive(
  {
    req(input$fileInput)
    return(readtext(file = input$fileInput$datapath, encoding = "UTF-8"))
  })

  webContent <- reactive(
  {
    req(input$webURL)

    site <- NULL

    tryCatch(
      site <- read_html(input$webURL),
      error   = function(something) {},
      warning = function(something) {}
    )

    if (length(site)>0)
    {
      text <- html_text(html_nodes(site, 'p'))
      text <- gsub("\n", "", text)
      text <- gsub("([)[0-9]+(]))", "", text)

      text <- data.frame(text)
      text <- subset(text, str_count(text, "\\w+") > 1)
      text <- subset(text, grepl("[A-Z|a-z]", text))

      if (nrow(text) > 0)
        return(paste(text$text, sep = "", collapse = "\n\n"))
      else
        return(NULL)
    }
    else
      return(NULL)
  })

  output$getInput <- renderUI(
  {
    req(input$selInput)

    if (input$selInput=="Text")
      return(div(uiOutput("aceEditor"), class='format'))
    if (input$selInput=="File")
      return(div(fileInput('fileInput', NULL, accept = c(".txt", ".docx", ".html"), placeholder='Browse or drop your file here (.txt, .docx, .html)', width="100%"), class='format'))
    if (input$selInput=="Web")
      return(div(HTML("<input type='text' id='webURL' class='web' placeholder='Enter web address'><div style='height: 40px'></div>"), class='format'))
  })

  observeEvent(input$clearButton,
  {
    if (input$selInput=="Text")
      updateAceEditor(session, "ace", value = 1)
    if (input$selInput=="File")
      js$clearUpload()
    if (input$selInput=="Web")
      updateTextInput(session, "webURL", value = "")
  })

  checkText <- reactive(
  {
    req(input$selInput)

    if (input$selInput=="Text")
      s <- input$ace
    if (input$selInput=="File")
      s <- fileContent()
    if (input$selInput=="Web" )
      s <- webContent()

    if (!is.null(s) && (trimws(s) == ""))
      return(NULL)
    
    return(s)
  })

  graph2phon <- function(ud)
  {
    p <- gsub("'" , "\\'", ud$token, fixed = TRUE)
    p <- gsub("^-", "", p)
    p <- gsub("[0-9]+", "digits", p)
    p <- tolower(paste(p, collapse = " "))
    p <- unlist(system(command = paste0("phonetisaurus predict --model www/hyph.fst --casing ignore ", p), intern = TRUE))
   
    if (length(p) == nrow(ud))
    {
      df <- data.frame()
      
      for (i in 1:length(p))
      {
        sep <- str_locate(p[i], " ")[1]
        
        gi <- substr(p[i], 1, sep-1)
        
        pi <- substr(p[i], sep+1, nchar(p[i]))
        pi <- gsub(" ", "", pi)

        df <- rbind(df, data.frame(graphemic=gi, hyphenated=pi))
      }
      
      df$graphemic <- ud$token
      df$lemma     <- ud$lemma
      df$upos      <- ud$upos

      return(df)
    }
    else
    {
      showNotification("The text likely contains one or more words composed exclusively of unknown tokens!", type = "error")
      return(NULL)
    }
  }

  annotateUD <- function(s)
  {
    s <- str_replace_all(s, "[^[:alnum:][:space:]’]", "")
    s <- str_replace_all(s, "^\\’(?=([:alpha:][:alpha:]))", "")
    s <- str_replace_all(s, "(?<=([:space:]|[:punct:]))\\’(?=([:alpha:][:alpha:]))", " ")
    s <- str_replace_all(s, "\\’ ", " ")
    s <- str_replace_all(s, "\\’$", "")

    result <- as.data.frame(udpipe(x = s, object=global$model))
    result$term_id <- NULL
  
    return(result)
  }

  resultUD <- reactive(
  {
    req(checkText())
    return(graph2phon(annotateUD(checkText())))
  })

  output$resultUD <- DT::renderDataTable(
    resultUD(),
    options = list(scrollX = TRUE, searching = FALSE, pageLength = 500, lengthChange = FALSE)
  )

  output$showResults <- renderUI(
  {
    req(checkText())

    tagList(
      HTML("<p style='font-weight: bold;'>Output Table:</p>"),
      div(DT::dataTableOutput('resultUD'), class='format'),

      br(), br(),

      splitLayout
      (
        cellWidths = c("270px", "150px"),

        pickerInput('selFormat', label = "Format: ", c("Tab-delimited text", "Microsoft Excel"), selected="Microsoft Excel", multiple=FALSE, width="150px", options = pickerOptions(title = "Format", dropupAuto = F, container = 'body')),
        downloadButton("downloadTable", "Download Table")
      )
    )
  })

  fileName <- function()
  {
    if (input$selFormat=="Tab-delimited text")
      ext <- "tsv"

    if (input$selFormat=="Microsoft Excel")
      ext <- "xlsx"

    return(paste0("G2P.", ext))
  }

  output$downloadTable <- downloadHandler(filename = fileName, content = function(file)
  {
    if (input$selFormat=="Tab-delimited text")
      write.table(resultUD(), file, sep = "\t", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE)

    if (input$selFormat=="Microsoft Excel")
      write.xlsx (resultUD(), file, sheetName = "table", headerStyle = createStyle(textDecoration = "BOLD"), rowNames=FALSE, colNames=TRUE, na.string = "", firstRow = TRUE)
  })
}

################################################################################

options(shiny.sanitize.errors = TRUE)
options(shiny.usecairo=FALSE)
options(shiny.maxRequestSize=20*1024^2)

shinyApp(ui = ui, server = server)

################################################################################
