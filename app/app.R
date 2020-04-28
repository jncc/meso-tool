modules::import(shiny)
modules::import(shinydashboard)
modules::import(shinydashboardPlus)
modules::import(shinycssloaders)
modules::import(shinyjs)

modules::import(bnlearn)
modules::import(visNetwork)
modules::import(RColorBrewer)
modules::import(plotly)
modules::import(openxlsx)
modules::import(zip)
modules::import(DT)
modules::import(plyr)

parser <- modules::use("Parses.R")


addResourcePath("js", "./www/js")


layers <- c("Pressures to Bio-Assemblages", "Bio-Assemblages to Output Processes", "Output Processes to Ecosystem services")
transitions <- c("Pressures to Bio-Assemblages", "Pressures to Output Processes", "Pressures to Ecosystem services")
impacts <- c("Very High", ">= High", ">= Medium", ">= Low", "All")
thresholds <- c(0.97, 0.9, 0.45, 0.17, 0)
impLabels <- c("Very High", "High", "Medium", "Low", "Very Low")


ui <- dashboardPage(
      dashboardHeader(title = "JNCC MESO online",
        tags$li(
          id = "dropdownHelp",
          class = "dropdown",
          tags$head(
            tags$script(
              paste0(
                "$(document).ready(function(){",
                "  $('#dropdownHelp')",
                "  .find('ul')",
                "  .click(function(e) { e.stopPropagation(); });",
                "});"
              )
            )
          ),
          tags$a(
            href = "javascript:void(0);",
            class = "dropdown-toggle",
            `data-toggle` = "dropdown",
            icon("question")
          ),
          tags$ul(
            class = "dropdown-menu",
            style = "left: auto; right: 0; min-width: 200px",
            tags$li(
              tags$div(
                style = "margin-left: auto; margin-right: auto; width: 90%;",
                tags$a(
                  href = "Manual.pdf",
                  target = "_BLANK",
                  "Open user guide in tab"
                )
              )
            ),
            tags$li(
              tags$div(
                style = "margin-left: auto; margin-right: auto; width: 90%;",
                tags$a(
                  href = "Report.pdf",
                  target = "_BLANK",
                  "Open Final Report in tab"
                )
              )
            )
          )
        )
      ),
      dashboardSidebar(
        sidebarMenu(id = "tabs",
            menuItem("Introduction", tabName = "1", icon = icon("arrow-down")),
            menuItem("Pressure Test", tabName = "2", icon = icon("arrow-down")),
            menuItem("Bayesian Network", tabName = "3", icon = icon("atom")),
            #menuItem("Habitats", tabName = "3", icon = icon("atlas")),
            #menuItem("Ingestion", tabName = "3", icon = icon("utensils")),
            selectInput("modelSelect", "Select MESO model", choices = c(""), selected = NULL, multiple = FALSE),
            #downloadButton("download", "", icon = icon("download")),
            uiOutput("pressureList")
        )
      ),
      dashboardBody(
        tabItems(
          tabItem(
            tabName = "1", h2("Introduction"),
            tags$p(
              style = "font-size: 12pt",
              "This website is provided for the Joint Nature Conservation Committee (JNCC)  and is provided by",
              tags$a(href = "https://avsdev.uk", "AVS Developments", target = "_BLANK"),
              ", working under contract to ",
              tags$a(href = "https://www.mba.ac.uk", "the Marine Biology Association.", target = "_BLANK")
            ),
            tags$p(
              style = "font-size: 12pt",
              "This website provides a Proof of Concept visualisation tool to assist in understanding the probabilitic impact that
              Anthropogenic Pressures (i.e. human activities) has on the habitats of sub-littoral areas of the United Kingdom."
            ),
            tags$p(
              style = "font-size: 12pt",
              "The tool provides a mapping using a Continuous Gaussian Bayesian Belief Network from the
               Anthropogenic Pressures through the biotopes and to the output processes and ultimately the
              Ecosystem services, to which the habitat supports."
            ),
            tags$p(
              style = "font-size: 12pt",
              "By selecting combinations of pressures on the left hand side bar, the impact on biotopes and functions of the
                     habitat can be estimated on the graphs shown on the Pressure test page.
              The Bayesian Network page shows the structure of the Bayesian Network itself.              "
            ),
            tags$p(
              style = "font-size: 12pt",
              "Five substrate types have been modelled (coarse sediment, mixed sediment, mud, rock and sand)."
            ),
            tags$p(
              style = "font-size: 12pt",
              "Impact of pressures are as defined in ",
              tags$a(href = "https://www.marlin.ac.uk/sensitivity/sensitivity_rationale",
                     "the Marine Evidence based Sensitivity Assessment (MarESA).", target = "_BLANK")
            ),
            tags$p(
              style = "margin-top: 150px; font-size: 12pt",
              "Further information on the rationalale and supporting information can be found in the Studiy's Final Report
              available as a download from the Help pages selectable from the Question Mark logo on the
              top right hand side of the website."
            ),
            tags$p(
              style = "margin-top: 150px; font-size: 10pt",
              "GDPR Notice: This website only uses cookies to provide core functionality. No personal data cookies are used."
            ),
            tags$p(
              style = "font-size: 10pt",
              "Copyright Notice: All images, logos and sources are property and copyright of their respected owners"
            )
          ),
          tabItem(tabName = "2", h2("Impact Distribution"),
            fluidRow(
              column(
                width = 6,
                h4("Effect on bio-assemblage")
              ),
              column(
                width = 1,
                actionButton("layer1Slider", "1", icon = icon("sliders-h"))
              ),
              column(
                width = 2,
                p("Custom sense weighting")
              ),
              column(
                width = 1,
                downloadButton("download", "", icon = icon("download"))
              ),
              column(
                width = 2,
                p("Download results as Excel workbook")
              )
            ),
            plotlyOutput("layer1", height = "270px") %>% withSpinner(),
            h4("Effect on Output Processes"),
            plotlyOutput("layer2", height = "270px") %>% withSpinner(),
            h4("Effect on Ecosystem services"),
            plotlyOutput("layer3", height = "270px") %>% withSpinner()
          ),
          tabItem(tabName = "3",h2("Bayesian Network"),
            fluidPage(
              p("Graphical output of the Bayesian Network.  Note: The graph will only draw if pressures are applied!"),
              fluidRow(
                column(
                  width = 4,
                  checkboxInput("bbnDisplayNames", "Display Node names", value = FALSE)
                ),
                column(
                  width = 4,
                  checkboxInput("bbnDisplayEdges", "Display edge status", value = FALSE)
                ),
                column(
                  width = 4,
                  selectInput("bbnImpactSelect", "Impact Threshold", choices = impacts, selected = "All")
                )
              ),
              fluidRow(
                visNetworkOutput("bbnGraphPlot", width = "100%", height = "1000px")
              ),
              fluidRow(
               column(
                 width = 6,
                 h4("Ecoservice nodes"),
                 DT::dataTableOutput("nodeTable")
               ),
               column(
                 width = 6,
                 h4("Ecoservice influences"),
                 DT::dataTableOutput("edgeTable")
               )
              )
            )
          )
        )
      )
)

server <- function(input, output, session) {
  #SERVER Constants

  print("Loading data")

  dataStorage <- "data/"

  palette <- c("firebrick", "coral", "rosybrown", "tan", "salmon",  "olivedrab", "seagreen", "aquamarine",  "darkcyan",  "dodgerblue",  "steelblue",   "royalblue")


  models <- NULL
  pressures <- NULL

  .loadStatus <- reactiveValues(
    valid = c(p = FALSE, ba = FALSE, op = FALSE, es = FALSE),
    msgs = NULL
  )

  .likelihoods <- reactiveValues(
    p_es = NULL
  )

  setPressures <- function(newPressures) {
    pressures <<- newPressures
  }


  .resistanceScores <- c(
    ins = -0.01,
    hr = -0.2,
    mr = -0.75,
    lr = -0.95,
    nr = -0.99,
    ssgr = 0,
    pressSD = 1.0
  )

  .selections <- reactiveValues(
    model = 1,
    #runOnce = FALSE,
    bbnImpact = 1,
    bbnNames = FALSE,
    bbnEdges = FALSE,
    pressStatus = NULL
  )

  getImpact <- function(v) {
    if ((v == "INS") || (v == "IV")) return(.resistanceScores[1])
    if ((v == "HR") || (v == "III")) return(.resistanceScores[2])
    if ((v == "MR") || (v == "II")) return(.resistanceScores[3])
    if ((v == "LR") || (v == "I")) return(.resistanceScores[4])
    if (v == "NR") return(.resistanceScores[5])
    as.numeric(v)
  }

  getAvailableModels <- function() {
    fileList <- list.files(dataStorage, pattern = ".xlsx")

    modelList <- list()
    cnt <- 1

    for (idx in 1:length(fileList)) {
      print(paste("attempting to load", paste0(dataStorage, fileList[idx])))

      wb <- parser$parseSheet(paste0(dataStorage, fileList[idx]))
      #print(tmp)

      wb$p_es$edges$values <- sapply(wb$p_es$edges$impact, getImpact)

      if (!is.null(wb)) {
        modelList[[cnt]] <- wb
        models <<- c(models, substr(fileList[idx], 1, (nchar(fileList[idx])-5)))
        print(paste("Model file successfully loaded", fileList[idx]))
        #save(tmp, file = "tmp.RData")
        cnt <- cnt+1
      }
    }
    updateSelectInput(session, "modelSelect", choices = models)
    return(modelList)
  }

  #parse on load sheets in the input sheet folder - replace with R Data
  modelList <- getAvailableModels()


  calcLikelihood <- function(layer, pressStatus, forPlotly) {

    isolate({

      modelList[[.selections$model]]$p_es$edges$values <<- sapply(modelList[[.selections$model]]$p_es$edges$impact, getImpact)
      modelList[[.selections$model]]$p_es$nodes$growth <<- .resistanceScores["ssgr"]
      modelList[[.selections$model]]$p_es$nodes$confidence <<- .resistanceScores["pressSD"]

      thisModel <- modelList[[.selections$model]]


      MEANPOS <- 1
      MEANNEG <- 0

      expr <- "list("
      for (p in 1:nrow(pressStatus)) {
        if (pressStatus$status[p] == "On") {
          threshold <- MEANPOS
        } else {
          threshold <- MEANNEG
        }

        expr <- paste0(expr, "\"", pressStatus$code[p], "\" = ", threshold, ", ")
      }
      expr <- substr(expr, 1, nchar(expr)-2)
      expr <- paste0(expr, ")")

      thisNet <- parser$buildGraph(thisModel$p_es, desc = list(inputCode = "p", outputCodes = c("ba", "op", "es")))

      sampleDists <- cpdist(
        fitted = thisNet$cfit,
        nodes = bnlearn::nodes(thisNet$cfit),
        evidence = eval(parse(text = expr)),
        method = "lw",
        n = 10000,
        debug = FALSE
      )
    })

    #print(sampleDists)

    #displayCols <- match(nodeCodes, colnames(sampleDists))
    sampleDists <- sampleDists[,match(thisModel$p_es$nodes$code, colnames(sampleDists))]

    means <- apply(sampleDists, 2, mean)
    stdDev <- apply(sampleDists, 2, sd)
    #quantiles <- t(apply(sampleDists, 2, quantile, c(0.01, 0.25, 0.5, 0.75, 0.99)))
    quantiles <- t(apply(sampleDists, 2, quantile, c(0.01, 0.25, 0.5, 0.75, 0.99)))
    print(paste("Building likelihoods from model, sample dists", length(thisModel$p_es$nodes$name), length(sampleDists)))
    #str(quantiles)
    
    if (forPlotly) {
      return(data.frame(
        name = thisModel$p_es$nodes$name,
        code = thisModel$p_es$nodes$code,
        layer = thisModel$p_es$nodes$layer,
        range = c(
           #apply(sampleDists, 2, min),
           quantiles[,1],
           quantiles[,2],
           quantiles[,2],
           quantiles[,3],
           quantiles[,4],
           quantiles[,4],
           quantiles[,5]
        ),
        stringsAsFactors = FALSE
      ))
    } else {

      return(data.frame(
        name = thisModel$p_es$nodes$name,
        code = thisModel$p_es$nodes$code,
        layer = thisModel$p_es$nodes$layer,
        means = means,
        stdDev = stdDev,
        mins = apply(sampleDists, 2, min),
        maxes = apply(sampleDists, 2, max),
        stringsAsFactors = FALSE
      ))
      
    }
  }


  observeEvent(input$modelSelect, {
    .selections$model <<- match(input$modelSelect, models)
    #.selections$runOnce <<- TRUE
  })

  observeEvent(reactiveValuesToList(input), {
    isolate(myList <- reactiveValuesToList(input))
    matches <- match(pressures$code,  names(myList))

    if (length(matches) > 0) {
      status <- NULL
      for (n in 1:length(matches)) {
        status[n] <- myList[[matches[n]]]
      }

      newStatus <- data.frame(code = pressures$code, status = status, stringsAsFactors = FALSE)

      if (!identical(newStatus, .selections$pressStatus)) { #} || .selections$runOnce) {
        #.selections$runOnce = FALSE
        print("Running calc")
        .likelihoods$p_es <<- calcLikelihood(0, newStatus, TRUE)
       
        .selections$pressStatus <<- newStatus
      }

    }
  })

  makeRadioButtons <- function(row) {
    radioButtons(row["code"], row["name"], choices = c("Off", "On"), selected = "Off", inline = TRUE)
  }

  output$pressureList <- renderUI({
    #isolate({
    if (!is.null(modelList[[.selections$model]]$p_es$nodes)) {
      pressCodes <- which(startsWith(modelList[[.selections$model]]$p_es$nodes$code, "p"))

      #if (is.null(.selections$pressStatus)) status <- rep("Off", length(pressCodes)) else status <- .selections$pressStatus$status
      pressures <- data.frame(
        code = modelList[[.selections$model]]$p_es$nodes$code[pressCodes],
        name = modelList[[.selections$model]]$p_es$nodes$name[pressCodes],
        #status = status,
        stringsAsFactors = FALSE
      )
     
      #This assumes all pressures are the same...

      setPressures(pressures)
      btnList <- apply(pressures, 1, makeRadioButtons)
    }
  })

  observeEvent(input$bbnImpactSelect, {
    #filter nodes and edges to
    .selections$bbnImpact <- thresholds[match(input$bbnImpactSelect, impacts)]
  })

  observeEvent(input$bbnDisplayNames, {
    .selections$bbnNames <- input$bbnDisplayNames
  })

  observeEvent(input$bbnDisplayEdges, {
    .selections$bbnEdges <- input$bbnDisplayEdges

  })


  observeEvent(input$layer1Slider, {
    showModal(
      modalDialog({
        tagList(
          sliderInput("l1VL", "Insensitive", 0.01, 0.2, abs(.resistanceScores[1]), step = 0.01),
          sliderInput("l1L", "Low Sensitivity/High resistance", 0.15, 0.5, abs(.resistanceScores[2]), step = 0.01),
          sliderInput("l1M", "Medium Sensitivity/Med resistance", 0.5, 0.75, abs(.resistanceScores[3]), step = 0.01),
          sliderInput("l1H", "High Sensitivity/Low resistance", 0.75, 1.0, abs(.resistanceScores[4]), step = 0.01),
          sliderInput("l1VH", "Very High Sensitivity/No resistance", 0.9, 1.0, abs(.resistanceScores[5]), step = 0.01),
          sliderInput("ssgr", "Zero intercept", -0.1, 0.1,.resistanceScores[6], step = 0.01),
          sliderInput("l1PressSD", "Std Dev", 0.1, 1.0, .resistanceScores[7], step = 0.01)
        )
      },
      title = "Layer 1 controls",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("modalOK", "OK")
      ),
      size = "s")
    )
  })

  observeEvent(input$modalOK, {
    

    .resistanceScores["nr"] <<- -input$l1VH
    .resistanceScores["lr"] <<- -input$l1H
    .resistanceScores["mr"] <<- -input$l1M
    .resistanceScores["hr"] <<- -input$l1L
    .resistanceScores["ins"] <<- -input$l1VL
    .resistanceScores["ssgr"] <<- input$ssgr
    .resistanceScores["pressSD"] <<- input$l1PressSD

    
    .likelihoods$p_es <<- calcLikelihood(0, .selections$pressStatus, TRUE)
    removeModal()

  })


  output$nodeTable <- DT::renderDataTable(
    modelList[[.selections$model]]$p_es$nodes,
    selection = "single",
    server = TRUE,
    escape = FALSE,
    rownames = TRUE,
    options = list(searching = TRUE, pageLength = 10, editable = TRUE)
  )

  output$edgeTable <- DT::renderDataTable(
    modelList[[.selections$model]]$p_es$edges,
    selection = "single",
    server = TRUE,
    escape = FALSE,
    rownames = TRUE,
    options = list(searching = TRUE, pageLength = 10, editable = TRUE)
  )

  getLabel <- function(value) {
    sign <- ifelse(value < 0, "-", "+")
    idx <- min(which((abs(value) >= thresholds) == TRUE))
    return(paste0(sign, impLabels[idx]))
  }

  makeBbnGraph <- function(model) {
    nodes <- model$p_es$nodes

    if (.selections$bbnEdges) {
      labels <- sapply(model$p_es$edges$values, getLabel)
    } else {
      labels <- rep("", nrow(model$p_es$edges))
    }

    edges <- data.frame(
      id = rownames(model$p_es$edges),
      from = match(model$p_es$edges$input, nodes$code),
      to = match(model$p_es$edges$output, nodes$code),
      values = model$p_es$edges$values,
      label = labels,
      arrows = "to",
      stringsAsFactors = FALSE
    )
    if (.selections$bbnNames) {
      labels <- nodes$name
    } else {
      labels <- nodes$code
    }

    nodeSpacing <- ifelse(.selections$bbnNames, 600, 150)


    nodes <- data.frame(
      id = rownames(nodes),
      label = labels,
      level = nodes$layer,
      group = nodes$layer,
      color = palette[as.integer(nodes$layer)],
      code = nodes$code,
      stringsAsFactors = FALSE
    )

    edges <- edges[(abs(edges$values) >= .selections$bbnImpact),]

    nodeNet <- nodes[(nodes$code %in% .selections$pressStatus$code[.selections$pressStatus$status %in% c("On")]),]

    #save(nodes, edges, nodeNet, file = "tmp.RData")

    if (nrow(nodeNet) > 0) {
      #do pressures
      edgeNet <- edges[edges$from %in% nodeNet$id, ]
      idx <- 1
      repeat {
        nodesToAdd <- nodes[nodes$id %in% edgeNet$to, ]
        nodesToAdd <- nodesToAdd[!(nodesToAdd$id %in% nodeNet$id),]

        edgesToAdd <- edges[edges$from %in% nodesToAdd$id, ]
        edgesToAdd <- edgesToAdd[!(edgesToAdd$id %in% edgeNet$id),]

        idx <- idx + 1
        if ((idx > 20) || ((nrow(nodesToAdd) == 0) && (nrow(edgesToAdd) == 0))) break
        nodeNet <- rbind(nodeNet, nodesToAdd)
        edgeNet <- rbind(edgeNet, edgesToAdd)

      } #until finished
    } else {
      edgeNet <- edges
    }
    
    print(paste(nrow(model$legend), length(palette)))

    legendDF <- data.frame(
      id = 1:nrow(model$legend),
      label = model$legend,
      color = palette[1:nrow(model$legend)],
      stringsAsFactors = FALSE
    )

    visNetwork(nodeNet, edgeNet, width = "100%", main = "Bayesian Belief Network", submain = input$modelSelect) %>%
      visExport() %>%
      visLegend(useGroups = FALSE, addNodes = legendDF) %>%
      visHierarchicalLayout(nodeSpacing = nodeSpacing, direction = "LR") %>%
      visOptions(highlightNearest = TRUE) #%>%
      #visInteraction(navigationButtons = TRUE, dragNodes = TRUE, dragView = TRUE, zoomView = TRUE)
  }

  output$bbnGraphPlot <- renderVisNetwork({
    makeBbnGraph(modelList[[.selections$model]])
  })

  #observe({
  #  visNetworkProxy("bbnGraphPlot") %>%
  #    visStabilize(iterations = 10)
  #})

  getModelName <- function() {
    paste0("data/", input$modelSelect, ".xlsx")
  }

  genPlot <- function(boxPlot, title, paletteLength) {
    if (nrow(boxPlot) > 0) {

      #print(paste('Palette length', paletteLength))

      #palette <- brewer.pal(paletteLength,  "Set3")

      #palette <- c("red", "sienna3", "plum2", "rosybrown4",  "sandybrown", "yellow", "seashell3", "palegreen", "springgreen4",  "steelblue",   "azure")

      names(palette) <- 1:length(palette)

      #print(paste("Box plot, colours", nrow(boxPlot), length(colours)))
      #cat(colours)
      xform <- list(categoryorder = "array",
                    categoryarray = boxPlot[,1],
                    zerolinewidth = 10)
      #
      plot_ly(boxPlot, x = boxPlot[,1], y = ~Range, color = as.character(boxPlot$Group), colors = palette, type = "box") %>%
        layout(xaxis = xform, showlegend = FALSE, title = title)

    }
  }

  prepPlot <- function(code = "ba", name = "Bio-Assemblage") {
    if (!is.null(.likelihoods$p_es)) {
      inScope <- startsWith(.likelihoods$p_es$code, code)
      thisPlot <- .likelihoods$p_es[inScope, c(1,3,4)]
      colnames(thisPlot) <- c(name, "Group", "Range")
      title <- paste(input$modelSelect, name, "Box Plot")
      paletteLength <- nrow(modelList[[.selections$model]]$legend)
      #print(paste('prep plot palette', paletteLength))
      genPlot(thisPlot, title, paletteLength)
    }
  }

  output$layer1 <- renderPlotly({
    prepPlot("ba", "Bio-Assemblage")
  })

  output$layer2 <- renderPlotly({
    prepPlot("op", "Output Processes")
  })

  output$layer3 <- renderPlotly({
    prepPlot("es", "Ecosystem Services")
  })


  isAbsolutePath = function( path ){
      if( path == "~" )
          return(TRUE);
      if( grepl("^~/", path) )
          return(TRUE);
      if( grepl("^.:(/|\\\\)", path) )
          return(TRUE);
      if( grepl("^(/|\\\\)", path) )
          return(TRUE);
      return(FALSE);
  }

  output$linkBackgroundData <- downloadHandler(
    filename = getModelName(),
    content = function(file) {
      file.copy(getModelName(), file)
    },
    contentType = "application/xlsx"
  )
  
  makeLikelihoods <- function() {
    
    
    likeliTab <- as.data.frame(
      cbind(
        .likelihoods$p_es, codeVal = sapply(
          .likelihoods$p_es$code, function(str) {
            if (startsWith(str, 'p')) as.numeric(substring(str, 2, nchar(str)))
            else as.numeric(substring(str, 3, nchar(str)))
          }
        )),
      stringsAsFactors=FALSE
    )
    
    likeliTab <- arrange(likeliTab, layer, codeVal)
    
    outputRows <- trunc(nrow(likeliTab)/7)
    outputTab <- NULL
    
    for (idx in 1:outputRows) {
      elementRow <- (idx - 1) * 7 + 1
    
      tabRow <-c(
        name = likeliTab$name[elementRow],
        code = likeliTab$code[elementRow],
        layer = likeliTab$layer[elementRow],
        min=likeliTab$range[elementRow],
        q1 =likeliTab$range[elementRow+2],
        median =likeliTab$range[elementRow+3],
        q3 =likeliTab$range[elementRow+4],
        max =likeliTab$range[elementRow+6]
      )
      outputTab <- rbind(outputTab, tabRow)
      
    }
    
    likelihoods <- data.frame(
      name = outputTab[,1],
      code = outputTab[,2],
      layer = as.numeric(outputTab[,3]),
      max =as.numeric(outputTab[,8]),
      q3 =as.numeric(outputTab[,7]),
      median =as.numeric(outputTab[,6]),
      q1 =as.numeric(outputTab[,5]),
      min=as.numeric(outputTab[,4]),
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  }

  output$download <- downloadHandler(
    
    filename = function() { paste0("MESO-", format(Sys.time(), "%m%d_%H%M"), ".xlsx") },
    content = function(file) {
    
      showModal(
        modalDialog(
          fluidRow(
            column(width = 12) %>% withSpinner(type = 5, proxy.height = "200px")
          ),
          footer=div()
        )
      )

      oldDir <- getwd()
      
      tmp <- tempfile("")
      dir.create(tmp)
      setwd(tmp)
      
      
      
      l <- list(
        pressures = .selections$pressStatus,
        nodes = modelList[[.selections$model]]$p_es$nodes,
        edges = modelList[[.selections$model]]$p_es$edges,
        settings = as.data.frame(cbind(names(.resistanceScores), .resistanceScores), stringsAsFactors = FALSE),
        likelihoods = makeLikelihoods()
      )
      xl <- write.xlsx(l,  "dataset.xlsx")

      #zipFile <- zipr(file, c("dataset.xlsx"))
      
      file.copy("dataset.xlsx", file)

      #print(paste("zip file complete", zipFile))

      setwd(oldDir)
      unlink(tmp)

      removeModal()
    },
    contentType = "application/xlsx"
  )


}

shinyApp(ui, server)
