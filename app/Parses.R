
modules::import(bnlearn)
modules::import(openxlsx)
modules::import(stringr)
modules::import(stats)


#Improvements needed: make the selection of first row/column of nodes programmatic
FIRST_NODE_COL <- 3

mappings <- c("TestScenario", "Map_P_BA", "Map_BA_OP", "Map_OP_ES", "Legend")
nodeTypes <- c("Input.Nodes", "Internal.Nodes", "Published.Nodes")
states <- c("impact", "confidence", "growth", "recovery", "layer")
refs <- c(1:length(mappings))

setEmpties <- function(val) {
  if (is.na(val)) {
    return(0)
  } else {
    return(val)
  }
}

readXL <- function(fName, sheetN, startRow = 1) {
  xl <- read.xlsx(fName, sheet = sheetN, startRow)    #, rowNames = import)
  return(data.frame(xl, stringsAsFactors = FALSE, row.names = NULL))
}

delNA <- function(vec) {
  return(vec[!is.na(vec)])
}

buildExpr <- function(pressStatus) {
  #pressStatus is a two column DF of name of pressure and status Ii.e. on or off)
  MEANPRESS <- 0
  expr <- "("
  for (p in 1:nrow(pressStatus)) {
    if (pressStatus$status[p] == "On") {
      symbol <- ">="
    } else {
      symbol <- "<="
    }

    expr <- paste0(expr, "(\"", pressStatus$code[p], "\"", symbol, MEANPRESS, ") & ")
  }
  expr <- substr(expr, 1, nchar(expr) - 2)
  expr <- paste0(expr, ")")

  return(expr)
}

parseScenario <- function(press, prefix = "p") {
  pressNames <- colnames(press)[2:length(colnames(press))]
  coefs <- matrix(
    data = NA,
    nrow = length(pressNames),
    ncol = 3,
    dimnames = list(NULL, c("growth", "confidence", "layer"))
  )
  for (col in 2:ncol(press)) {
    coefs[col-1,] <-  as.numeric(split(press[1, col]))[match(c("growth", "confidence", "layer"), states)]
  }
  press[is.na(press)] <- 0
  if (sum(duplicated(pressNames)) > 0) {
    cat("Duplicated pressure node names found")
    print(pressNodes[duplicated(pressNames)])
  }

  return(list(
    timeSeq = press,
    nodes = data.frame(
      name = pressNames,
      code = paste0(prefix, seq(1:length(pressNames))),
      growth = coefs[,"growth"],
      confidence = coefs[,"confidence"],
      layer = coefs[,"layer"],
      stringsAsFactors = FALSE
    ),
    edges = data.frame(input = NULL, output = NULL, impact = NULL)
  ))
}

getInitial <- function(string, letter) {
  return(tolower(substr(string, start = 1, stop = 1)))
}

split <- function(cell) {
  
  params <- unlist(strsplit(cell, ","))
  values <- rep(0, length(states))

  for (n in 1:length(params)) {
     kvp <- unlist(strsplit(params[n], "="))
     ref <- match(getInitial(trimws(kvp[1])), getInitial(states))

     if ((ref > 0) & (ref <= length(values))) {
       values[ref] <- kvp[2]
     } else {
       print(paste("Unrecognised parameter(s):",params[n]))
     }
  }

  return(values)
}

cleanTitles <- function(titleV) {
  return(str_replace_all(titleV, c(" " = ".", "-" = "")))
}

getOutNodes <- function(codes, codeList) {
  v <- vector(mode = "logical", length = length(codes))

  for (idx in 1:length(codes)) {
    v[idx] <- (sum(startsWith(codes[idx], codeList)) > 0)
  }

  return(v)
}

buildGraph <- function(model, desc) {

  #model contains the following
  # node table, edge table

  #descriptor (desc) contains:
  #inputCode - the top layer of the model
  #outputCodes - all subsequent layers to be included in the model

  inputNodes <- model$nodes$code[which(startsWith(model$nodes$code, desc$inputCode))]
  inputText <- paste0("[", inputNodes, "]", collapse = "")

  #do the internal nodes
  edges <- ""

  outNodes <- model$nodes$code[getOutNodes(model$nodes$code, desc$outputCodes)]
  outDist <- vector(mode = "list", length = length(outNodes))

  for (idx in 1:length(outNodes)) {
    nodeRef <- match(outNodes[idx], model$nodes$code)

    rows <- which(model$edges$output == outNodes[idx])
    inputsStr <- paste0(model$edges$input[which(model$edges$output == outNodes[idx])], sep = ":", collapse = "")
    edges <- paste0(edges, paste0("[", outNodes[idx], "|", substr(inputsStr, start = 1, stop = (nchar(inputsStr)-1)), "]"))

    #Make the coefficient of the distribution
    coefVal <- setNames(
      c(model$nodes$growth[nodeRef], model$edges$values[rows]),
      c("(Intercept)", model$edges$input[rows])
    )
    #str(coefVal)
    outDist[[idx]] <- list(coef = coefVal, sd = model$nodes$confidence[nodeRef])
  }

  print("about to build network")
  print(paste0(inputText, edges))

  net <- model2network(paste0(inputText, edges), debug = FALSE)

  print("network build successful")

  inDist <- vector(mode = "list", length = length(inputNodes))

  for (idx in 1:length(inputNodes)) {
    inRef <- match(inputNodes[idx], model$nodes$code)
    coefVal <- setNames(model$nodes$growth[inRef], "(Intercept)")
    inDist[[idx]] <- list(coef = coefVal, sd = model$nodes$confidence[inRef])
  }

  allDists <- as.list(setNames(c(inDist, outDist), c(inputNodes, outNodes)))
  cfit <- custom.fit(net, allDists)

  cat("about to calculate sample distributions")
  #print(outNodes)

  sampleDists <- cpdist(cfit, nodes = outNodes, evidence = TRUE, n = 10000, method = "lw")
  summDists <- summary(sampleDists)
  #stdDev <- sd(sampleDists)

  print("sample distribution build successful")

  model$edges$input <- model$nodes$name[match(model$edges$input, model$nodes$code)]
  model$edges$output <- model$nodes$name[match(model$edges$output, model$nodes$code)]

  return(
    list(
      nodes = model$nodes,
      edges = model$edges,
      net = net,
      cfit = cfit,
      allDists = allDists,
      summDists = summDists
    )
  )
}


getValidNodes <- function(mapping, prevOutputs, prefix) {

  #Find row id for input nodes, internal and published
  inputNodes <- mapping[2:nrow(mapping),1]

  #check that all input nodes are in the previous table
  inputNodes <- delNA(mapping[mapping[,"Node.Type"] == "input", "Nodes"])
  if (length(inputNodes) > 0) {
    if (sum(inputNodes %in% prevOutputs$name) < length(inputNodes)) {
      cat("Missing entries for input nodes in previous output columns")
      print(inputNodes[!inputNodes %in% prevOutputs$name])
    }
  } else {
    print("Invalid sheet - table must have at least one input row containing names from previous table")
  }


  #Check the row headings concur with previous names
  validInputs <- delNA(inputNodes[which(unique(inputNodes) %in% prevOutputs$name)])
  if (length(validInputs) == 0) {
    print("Invalid sheet - table must have at least one input row containing names from previous table")
  }


  inputInts <- delNA(inputNodes[mapping$Node.Type != "link"])

  if (sum(duplicated(inputInts))>0) {
    cat("Duplicated input node names found")
    print(inputNodes[duplicated(inputNodes)])
  }

  outNodes <- delNA(colnames(mapping)[FIRST_NODE_COL:ncol(mapping)])
  if (sum(duplicated(outNodes)) > 0) {
    cat("Duplicated output node names found")
    print(outNodes[duplicated(outNodes)])
  }


  #check that all internal nodes are in the columns
  intNodes <- delNA(mapping[mapping[,"Node.Type"] == "internal", "Nodes"])
  if (length(intNodes) > 0) {
    if (sum(intNodes %in% outNodes)<length(intNodes)) {
      cat("Missing entries for internal nodes in output columns")
      print(intNodes[!(intNodes %in% outNodes)])
    }
  }

  coefs <- matrix(data = NA, nrow = length(outNodes), ncol = 3, dimnames = list(NULL, c("growth", "confidence", "layer")))
  for (idx in 1:length(outNodes)) {
    col <- match(outNodes[idx], colnames(mapping))
    coefs[idx,] <-  as.numeric(split(mapping[1, col]))[match(c("growth", "confidence", "layer"), states)]
  }

  return(data.frame(
    code = c(prevOutputs$code, paste0(prefix, seq(1:length(outNodes)))),
    name = c(prevOutputs$name, outNodes),
    growth = c(prevOutputs$growth, coefs[,"growth"]),
    confidence = c(prevOutputs$confidence, coefs[,"confidence"]),
    layer = c(prevOutputs$layer, coefs[,"layer"]),
    stringsAsFactors = FALSE
  ))
}

getCode <- function(name, nodeDF) {
  nodeDF$code[match(name, nodeDF$name)]
}

getValidEdges <- function(mapping, nodeDF, prevEdge = NULL, prefix) {
  #utils::str(nodeDF)

  edgeCols <- c("inputNode", "outputNode", "impact")
  edgeM <- matrix(data = NA, nrow = 0, ncol = length(edgeCols), dimnames = list(NULL, edgeCols))

  #to start let just get the statements and print them out....
  for (col in FIRST_NODE_COL:ncol(mapping)) {
    count <- 0

    for (row in 2:nrow(mapping)) {

      if (!is.na(mapping[row, col])) {
        edgeM <- rbind(edgeM,
          c(getCode(mapping[row, 1], nodeDF),
            getCode(colnames(mapping)[col], nodeDF),
            split(mapping[row,col])[match("impact", states)]
          )
        )
        count <- count + 1
      }
      #if (count == 0) print(paste("No edges found for output", colnames(mapping)[col]))
    }
  }
  if (is.null(prevEdge)) {
    return (data.frame(
      input = edgeM[,"inputNode"],
      output = edgeM[,"outputNode"],
      impact = edgeM[,"impact"],
      stringsAsFactors = FALSE
    ))
  } else {
    return (data.frame(
      input = c(prevEdge$input, edgeM[,"inputNode"]),
      output = c(prevEdge$output, edgeM[,"outputNode"]),
      impact = c(prevEdge$impact, edgeM[,"impact"]),
      stringsAsFactors = FALSE
    ))
  }
}

parseMapping <- function(mapping, prevOutputs, prefix) {
  mapping <- mapping[,-1]
  mapping[,1] <- cleanTitles(mapping[,1])

  nodeDF <- getValidNodes(mapping, prevOutputs$nodes, prefix)
  edgeDF <- getValidEdges(mapping, nodeDF, prevEdge = prevOutputs$edges, prefix)

  return(list(
    #New structure
    nodes = nodeDF,
    edges = edgeDF
  ))
}

parseSheet <- function(fName) {
  #get sheet names

  print(paste("starting sheet load", fName))

  if (file.exists(fName)) {
    names <- openxlsx::getSheetNames(fName)

    if (length(names) > 0) {

      sheets <- sort(delNA(match(names, mappings)))

      cat("starting sheet parse")
      print(sheets)

      if (sum(sheets == refs) == length(refs)) {
        #read all mapping tables
        scenario <- parseScenario(readXL(fName,mappings[1], startRow = 1), prefix = "p")
        p_ba <- parseMapping(readXL(fName,mappings[2], startRow = 1), scenario, prefix = "ba")
        p_op <- parseMapping(readXL(fName,mappings[3], startRow = 1), p_ba, prefix = "op")
        p_es <- parseMapping(readXL(fName,mappings[4], startRow = 1), p_op, prefix = "es")
        legend <- readXL(fName,mappings[5], startRow = 1)
        
        #print("building graphs")

        #p_baNet <- buildGraph(p_ba, desc = list(inputCode = "p", outputCodes = "ba"))
        #p_opNet <- buildGraph(p_op, desc = list(inputCode = "p", outputCodes = c("ba", "op")))
        #p_esNet <- buildGraph(p_es, desc = list(inputCode = "p", outputCodes = c("ba", "op", "es")))

        print("sheet load completed")
        return(
          list(
            p_esMap = p_es,
            legend = legend
          )
        )

      } else {
        print(paste("Sheets found include", mappings[sheets]))
        cat("Missing sheets are:")
        print(refs[-sheets])
      }
    }
  }
}
