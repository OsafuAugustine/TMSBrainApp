library(shiny)
library(shinyBS)
library(shinyWidgets)
library(shinydashboard)
library(readxl)
library(tidyverse)
# library(plyr)
library(plotly)
library(colorRamps)
library(lme4)
library(lmerTest)

# INLA REQUIREf
library(foreach)
library(sp)
library(splancs)
library(rgdal)
library(INLA)
library(INLABMA)
library(geosphere)
library(igraph)
library(rmarkdown)
# Define server logic required to draw a histogram

shinyServer(function(input, output, session) {
  
  dados = reactiveValues(dados = NULL, prd.m = NULL, output = NULL)
  
  observeEvent(input$FileInput, {
    #
    arquivo = input$FileInput
    dados$RawData = read_excel(arquivo$datapath)#, sheet = 'RawData')
    #dados$TRawData = read_excel(arquivo$datapath, sheet = 'AusPV002')
    # dados$testq = as.data.frame(dados$dados[-c(1:23),])
    #
    dados.names = names(dados$RawData)
    #
    EMG <- NULL
    EMGpos <- which(dados.names %in% 'EMG_Data')
    EMG.n <- 1:length(dados.names)
    #
    updateSelectInput(session = session,
                      inputId = 'RandomEffects',
                      choices = dados.names[EMG.n<EMGpos],
                      selected = c('x','y'))
    #
    updateSelectInput(session = session,
                      inputId = 'FixedEffects',
                      choices = dados.names[EMG.n<EMGpos],
                      selected = NULL)
    #
  })
  
  observeEvent(input$Model,{
    #
    req(dados$RawData)
    #
    dados.names = names(dados$RawData)
    #
    EMG <- NULL
    EMGpos <- which(dados.names %in% 'EMG_Data')
    EMG.n <- 1:length(dados.names)
    #
    updateSelectInput(session = session,
                      inputId = 'RandomEffects',
                      choices = dados.names[EMG.n<EMGpos],
                      selected = c('x','y'))
    #
    updateSelectInput(session = session,
                      inputId = 'FixedEffects',
                      choices = dados.names[EMG.n<EMGpos],
                      selected = NULL)
    #
  })
  
  observeEvent(c(input$Model,input$FileInput,input$SummaryResponse,input$FixedEffects,input$RandomEffects,
                 input$lambda.LEROUX, input$CAR.shape, input$CAR.scale, input$BYM.shape, input$BYM.scale), {
                   #
                   # Resetando objetos
                   dados$prd.m = NULL
                   dados$output = NULL
                   #
                 })
  
  observeEvent(input$RandomEffects, {
    #
    # browser()
    aux = input$RandomEffects
    if ( !all( c('x','y','Rep') %in% aux ) ) {
      updateSelectInput(session = session,
                        inputId = 'RandomEffects',
                        selected = c('x','y','Rep',aux))
    }
    #
  })
  
  observeEvent(c(input$SummaryResponse,input$FileInput,dados$RawData), {
    #
    req(dados$RawData)
    #
    # browser()
    aux = input$SummaryResponse
    RawData = dados$RawData
    names.RawData = names(RawData)
    #
    EMG <- NULL
    EMGpos <- which(names.RawData %in% 'EMG_Data')
    EMG.n <- 1:length(names.RawData)
    #
    testq = RawData %>% select(names.RawData[EMG.n<EMGpos]) %>% mutate( `Assoc. Target` = gsub(x = ID, pattern = '.*\\(', replacement = '(') )
    signal = RawData %>% select(names.RawData[EMG.n>=EMGpos])
    switch(
      aux,
      'Mean'={ testq = testq %>% bind_cols(data.frame(r = signal %>% apply(MARGIN = 1, FUN = mean) ) ) },
      'Median'={ testq = testq %>% bind_cols(data.frame(r = signal %>% apply(MARGIN = 1, FUN = median) ) ) },
      'Range'={ testq = testq %>% bind_cols(data.frame(r = signal %>% apply(MARGIN = 1, FUN = function(x)max(x)-min(x)) ) ) },
      'Standard Deviation'={ testq = testq %>% bind_cols(data.frame(r = signal %>% apply(MARGIN = 1, FUN = sd) ) ) }
    )
    dados$testq = testq
    #
  })
  
  output$ParModels <- renderUI({
    #
    req(input$Model)
    #
    # browser()
    aux = list()
    switch (
      input$Model,
      'SPDE' = {
        aux[[1]] =
          column(
            width = 6,
            style = 'padding-left: 0px; padding-right: 10px;',
            selectInput(
              inputId = 'FixedEffects',
              label = 'Fixed Effects',
              choices = list(''),
              selected = '',
              multiple = T
            )
          )
        aux[[2]] =
          column(
            width = 6,
            style = 'padding-left: 10px; padding-right: 0px;',
            selectInput(
              inputId = 'RandomEffects',
              label = 'Random Effects',
              choices = list(''),
              selected = '',
              multiple = T
            )
          )
      },
      'LEROUX' = {
        aux[[1]] =
          column(
            width = 6,
            style = 'padding-left: 0px; padding-right: 10px;',
            selectInput(
              inputId = 'FixedEffects',
              label = 'Fixed Effects',
              choices = list(''),
              selected = '',
              multiple = T
            )
          )
        aux[[2]] =
          column(
            width = 6,
            style = 'padding-left: 10px; padding-right: 0px;',
            selectInput(
              inputId = 'RandomEffects',
              label = 'Random Effects',
              choices = list(''),
              selected = '',
              multiple = T
            )
          )
        aux[[3]] =
          column(
            width = 6,
            style = 'padding-left: 0px; padding-right: 10px;',
            numericInput(
              inputId = 'LEROUX.shape',
              label = 'shape',
              value = 1,
              min = 0.01,
              max = 2.0,
              step = 0.01,
              width = '100%'
            )
          )
        aux[[4]] =
          column(
            width = 6,
            style = 'padding-left: 10px; padding-right: 0px;',
            numericInput(
              inputId = 'LEROUX.scale',
              label = 'scale',
              value = 0.001,
              min = 0.0005,
              max = 1.0,
              step = 0.0005,
              width = '100%'
            )
          )
      },
      'ICAR' = {
        aux[[1]] =
          column(
            width = 6,
            style = 'padding-left: 0px; padding-right: 10px;',
            selectInput(
              inputId = 'FixedEffects',
              label = 'Fixed Effects',
              choices = list(''),
              selected = '',
              multiple = T
            )
          )
        aux[[2]] =
          column(
            width = 6,
            style = 'padding-left: 10px; padding-right: 0px;',
            selectInput(
              inputId = 'RandomEffects',
              label = 'Random Effects',
              choices = list(''),
              selected = '',
              multiple = T
            )
          )
        aux[[3]] =
          column(
            width = 6,
            style = 'padding-left: 0px; padding-right: 10px;',
            numericInput(
              inputId = 'CAR.shape',
              label = 'shape',
              value = 1,
              min = 0.01,
              max = 2.0,
              step = 0.01,
              width = '100%'
            )
          )
        aux[[4]] =
          column(
            width = 6,
            style = 'padding-left: 10px; padding-right: 0px;',
            numericInput(
              inputId = 'CAR.scale',
              label = 'scale',
              value = 0.001,
              min = 0.0005,
              max = 1.0,
              step = 0.0005,
              width = '100%'
            )
          )
      },
      'BYM' = {
        aux[[1]] =
          column(
            width = 6,
            style = 'padding-left: 0px; padding-right: 10px;',
            selectInput(
              inputId = 'FixedEffects',
              label = 'Fixed Effects',
              choices = list(''),
              selected = '',
              multiple = T
            )
          )
        aux[[2]] =
          column(
            width = 6,
            style = 'padding-left: 10px; padding-right: 0px;',
            selectInput(
              inputId = 'RandomEffects',
              label = 'Random Effects',
              choices = list(''),
              selected = '',
              multiple = T
            )
          )
        aux[[3]] =
          column(
            width = 6,
            style = 'padding-left: 0px; padding-right: 10px;',
            numericInput(
              inputId = 'BYM.shape',
              label = 'shape',
              value = 1,
              min = 0.01,
              max = 2.0,
              step = 0.01,
              width = '100%'
            )
          )
        aux[[4]] =
          column(
            width = 6,
            style = 'padding-left: 10px; padding-right: 0px;',
            numericInput(
              inputId = 'BYM.scale',
              label = 'scale',
              value = 0.001,
              min = 0.0005,
              max = 1.0,
              step = 0.0005,
              width = '100%'
            )
          )
      }
    )
    #
    aux
    #
  })
  
  Calculate <- eventReactive(input$Calculate, {
    #
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    updateProgress <<- function(value = NULL, detail = NULL) {
      if (is.null(value)) {
        value <- progress$getValue()
      }
      progress$set(value = value, detail = detail)
    }
    #
    switch (
      input$Model,
      'SPDE' = SPDE(),
      'LEROUX' = LEROUX(),
      'ICAR' = CAR(),
      'BYM' = BYM()
    )
    #
  })
  
  observeEvent(input$SampleData, {
    #
    # browser()
    dados$RawData = NULL
    #
    dados$RawData = read_excel('www/PV002pt - Copia.xls', sheet = 'RawData')
    #
    dados.names = names(dados$RawData)
    #
    EMGpos <- which(dados.names %in% 'EMG_Data')
    EMG.n <- 1:length(dados.names)
    #
    updateSelectInput(session = session,
                      inputId = 'RandomEffects',
                      choices = dados.names[EMG.n < EMGpos],
                      selected = c('x','y'))
    #
    updateSelectInput(session = session,
                      inputId = 'FixedEffects',
                      choices = dados.names[EMG.n < EMGpos],
                      selected = NULL)
    #
    if ( !is.null(dados$RawData) ) {
      shinyalert(
        title = 'Success',
        text = 'Sample data has been loaded.',
        type = 'success'
      )
    }
    #
  })
  observeEvent(input$Calculate, {
    #
    req(
      dados$RawData
      , dados$testq
      , input$Model
      , input$SummaryResponse
      # 
      # 
    )
    #
    Calculate()
    #
    
  })
  
  idd <- reactive({
    #
    req(dados$RawData, dados$testq) # input$LEROUX.lambda
    #
    testq = dados$testq
    # 
    id <- testq %>% dplyr::select(matches('Target')) %>% pull()
  })
  
  idd2 <- reactive({
    #
    if (is.null(idd()))
      return(NULL)
    #
    id2 = vector("numeric",length(idd()))
    id = idd()
    clust = levels(as.factor(id))
    #
    for ( i in 1:length(clust) ) {
      id2[which(id==clust[i])] = i
    }
    return(id2)
    #
  })
  
  adj <-  reactive({
    if (is.null(idd()))
      return(NULL)
    
    id <- idd()
    clust <- levels(as.factor(id))
    
    M <- data.frame(long=as.numeric(gsub(",","",substring( clust,2,3))),lat=as.numeric(gsub(",","",substring( clust,4,(nchar(clust)-1)))))
    L1 <-NULL
    L2 <- NULL
    neiglength <- 2:5
    n=length(clust)
    for(i in 1:n){
      f <- function(j) distm(c(M[i,1], M[i,2]), c(M[j,1], M[j,2]), fun = distHaversine)
      c <- lapply(1:n,f )
      d<- data.frame(id =1:n,dist=unlist(c))
      d1 <- d[order(d$dist),]
      d2 <- d1[,1][neiglength]#d1$id[neiglength]
      L1 <- c(L1,rep(i,4))
      L2 <- c(L2,d2)
    }
    mat <- data.frame(L1,L2)
    g=get.adjacency(graph.edgelist(as.matrix(mat), directed=FALSE))
    return(g)
  })
  
  SPDE <- reactive({
    #
    req(dados$RawData, dados$testq)
    #
    updateProgress(detail = '\n Calculation of the SPDE model started')
    # browser()
    testq = dados$testq
    A.coords = cbind(testq$x,testq$y)
    #
    bnd = inla.nonconvex.hull(as.matrix(A.coords))
    mesh = inla.mesh.2d(boundary = bnd, max.edge = c(8,8)) #, boundary=bnd,
    #
    # Linear combination matrix
    A.est1 = inla.spde.make.A(mesh=mesh, loc=A.coords, cutoff=0.3)
    # SPDE model
    B.spde = inla.spde2.matern(mesh=mesh, alpha=2)
    #
    s.index = inla.spde.make.index(name="field", n.spde=B.spde$n.spde)
    # Data Stack
    RandomEffects = input$RandomEffects
    RandomEffects = RandomEffects[!(RandomEffects %in% c('x','y'))]
    FixedEffects = input$FixedEffects
    
    # browser()
    eval(
      parse(
        text = paste('
                B.stack.est = inla.stack(data = list(y=testq$r),
                A = ',
                     ifelse(
                       length(RandomEffects)>0 | length(FixedEffects)>0,
                       'list(A.est1, 1)',
                       'list(A.est1)'
                     ),',
                effects = list(
                c(s.index, list(Intercept=1))',
                     ifelse(
                       length(RandomEffects)>0 | length(FixedEffects)>0,
                       paste0(', list(',
                              paste0(na.omit(
                                c(ifelse(length(RandomEffects[!(RandomEffects%in%FixedEffects)])>0,
                                         paste0(RandomEffects, '=testq$', RandomEffects, collapse = ', '),NA),
                                  ifelse(length(FixedEffects)>0, paste0(FixedEffects, '=testq$', FixedEffects, collapse = ', '),NA))),
                                collapse = ', '),')'),
                       ''),
                     '), tag = "est")'
        )
      )
    )
    
    # formula and estimation
    eval(
      parse(
        text = paste0('
                    formula = y ~ -1 + f(field, model=spde)',
                      ifelse(
                        length(FixedEffects)>0,
                        paste0(' + ', paste0(FixedEffects, collapse = ' + ')),
                        ''
                      ),
                      ifelse(
                        length(RandomEffects)>0,
                        paste0(' + ',  paste0('f(',RandomEffects,', model = "rw1")', collapse = ' + ')),
                        ''
                      ),'
                ')
      )
    )
    
    # 
    #
    output = inla(formula,
                  data=inla.stack.data(B.stack.est, spde=B.spde),
                  family="gaussian",
                  control.predictor=list(A=inla.stack.A(B.stack.est), compute=TRUE),
                  control.compute=list(cpo=TRUE, dic=TRUE),
                  verbose = T)
    updateProgress(detail = '\n Calculation of the SPDE model finished')
    updateProgress(detail = '\n Organizing results')
    #
    P = inla.spde.make.A(mesh, loc=A.coords)
    dados$dados$Spde = drop(P%*%output$summary.random$field$mean)
    dados$dados$Spde.sig = drop(P%*%output$summary.random$field$sd)
    #
    projection = inla.mesh.projector(mesh) # projects the mesh onto a square grid
    #
    prd.m = inla.mesh.project(projection, output$summary.random$field$mean) # Projects the spatial effect mean onto the grid
    prd.s = inla.mesh.project(projection, output$summary.random$field$sd) # Projects the spatial effect SD onto the grid
    #
    colnames(prd.m) = projection$y
    rownames(prd.m) = projection$x
    prd.m.aux =
      cbind(expand.grid(dimnames(prd.m)), value = as.vector(prd.m)) %>%
      mutate(Var1 = as.numeric(levels(Var1)[Var1]),
             Var2 = as.numeric(levels(Var2)[Var2]))
    #
    dados$prd.m = prd.m.aux
    dados$output = output
    #
  })
  
  LEROUX <- reactive({
    #
    req(dados$RawData, dados$testq, input$LEROUX.shape, input$LEROUX.scale)
    #
    updateProgress(detail = '\n Calculation of the LEROUX model started')
    # browser()
    testq = dados$testq %>% mutate(idx=idd2())
    #
    # Data Stack
    RandomEffects = input$RandomEffects
    # RandomEffects 
    FixedEffects = input$FixedEffects
    
    # formula and estimation
    eval(
      parse(
        text = paste0('
                    formula = r ~ f(idx, model="besagproper2",graph=paste(getwd(),"/graph.graph",sep = ""),
                      
                      hyper = list(list(prior="loggamma",param=c(',input$LEROUX.shape,',',input$LEROUX.scale,'))))',
                      ifelse(
                        length(FixedEffects)>0,
                        paste0(' + ', paste0(FixedEffects, collapse = ' + ')),
                        ''
                      ),
                      ifelse(
                        length(RandomEffects)>0,
                        paste0(' + ',  paste0('f(',RandomEffects,', model = "rw1")', collapse = ' + ')),
                        ''
                      ),'
                ')
      )
    )
    #
    inla.write.graph(adj(), filename = "graph.graph")
    output <- inla(formula = formula,
                   control.predictor = list(compute = TRUE),
                   control.compute=list(cpo=TRUE, dic=TRUE),
                   data = testq) # Default non-informative priors
    #
    updateProgress(detail = '\n Calculation of the ICAR model finished')
    updateProgress(detail = '\n Organizing results')
    
    #
    output.aux =
      testq %>%
      bind_cols(output$summary.fitted.values) %>%
      group_by(`Assoc. Target`, idx) %>%
      summarise(x=mean(x), y=mean(y), value=mean(mean)) %>%
      rename('Var1'='x', 'Var2'='y')
    #
    
    #
    x = output.aux$Var1; y = output.aux$Var2; value = output.aux$value
    data = data.frame( x=x, y=y, z=value )
    data.loess = loess(z~x*y, data=data)
    data.fit = expand.grid(list(x=seq(min(x),max(x),l=50), y=seq(min(y),max(y),l=50)))
    value.aux = predict(data.loess, newdata=data.fit)
    #
    value.aux2 = as.data.frame(value.aux)
    #
    value.aux3 = rownames_to_column(value.aux2, var='Var1')
    value.aux4 = value.aux3 %>%
      gather(key='Var2', value='value', -Var1) %>%
      mutate(Var1=as.numeric(gsub('x=','',Var1)), Var2=as.numeric(gsub('y=','',Var2)))
    #
    dados$output = output
    dados$prd.m = value.aux4
    #
    
    #
  })
  
  CAR <- reactive({
    #
    req(dados$RawData, dados$testq, input$CAR.shape, input$CAR.scale)
    #
    updateProgress(detail = '\n Calculation of the ICAR model started')
    # browser()
    testq = dados$testq %>% mutate(idx=idd2())
    #
    # Data Stack
    RandomEffects = input$RandomEffects
    # RandomEffects 
    FixedEffects = input$FixedEffects
    
    # formula and estimation
    eval(
      parse(
        text = paste0('
                    formula = r ~ f(idx, model="besag",graph=paste(getwd(),"/graph.graph",sep = ""),
                      scale.model=TRUE,
                      hyper = list(list(prior="loggamma",param=c(',input$CAR.shape,',',input$CAR.scale,'))))',
                      ifelse(
                        length(FixedEffects)>0,
                        paste0(' + ', paste0(FixedEffects, collapse = ' + ')),
                        ''
                      ),
                      ifelse(
                        length(RandomEffects)>0,
                        paste0(' + ',  paste0('f(',RandomEffects,', model = "rw1")', collapse = ' + ')),
                        ''
                      ),'
                ')
      )
    )
    #
    inla.write.graph(adj(), filename = "graph.graph")
    output <- inla(formula = formula,
                   control.predictor = list(compute = TRUE),
                   control.compute=list(cpo=TRUE, dic=TRUE),
                   data = testq) # Default non-informative priors
    #
    updateProgress(detail = '\n Calculation of the ICAR model finished')
    updateProgress(detail = '\n Organizing results')
    
    #
    output.aux =
      testq %>%
      bind_cols(output$summary.fitted.values) %>%
      group_by(`Assoc. Target`, idx) %>%
      summarise(x=mean(x), y=mean(y), value=mean(mean)) %>%
      rename('Var1'='x', 'Var2'='y')
    #
    
    #
    x = output.aux$Var1; y = output.aux$Var2; value = output.aux$value
    data = data.frame( x=x, y=y, z=value )
    data.loess = loess(z~x*y, data=data)
    data.fit = expand.grid(list(x=seq(min(x),max(x),l=50), y=seq(min(y),max(y),l=50)))
    value.aux = predict(data.loess, newdata=data.fit)
    #
    value.aux2 = as.data.frame(value.aux)
    #
    value.aux3 = rownames_to_column(value.aux2, var='Var1')
    value.aux4 = value.aux3 %>%
      gather(key='Var2', value='value', -Var1) %>%
      mutate(Var1=as.numeric(gsub('x=','',Var1)), Var2=as.numeric(gsub('y=','',Var2)))
    #
    dados$output = output
    dados$prd.m = value.aux4
    #
  })
  
  BYM <- reactive({
    #
    req(dados$RawData, dados$testq, input$BYM.shape, input$BYM.scale)
    #
    updateProgress(detail = '\n Calculation of the BYM model started')
    # browser()
    testq = dados$testq %>% mutate(idx=idd2())
    #
    # Data Stack
    RandomEffects = input$RandomEffects
    # 
    FixedEffects = input$FixedEffects
    
    
    # formula and estimation
    eval(
      parse(
        text = paste0('
                    formula = r ~ f(idx, model="bym",graph=paste(getwd(),"/graph.graph",sep = ""),
                      scale.model=TRUE,
                              hyper = list(
                                        prec.unstruct=
                                            list(prior="loggamma",param=c(',input$BYM.shape,',',input$BYM.scale,')),
                                        prec.spatial=list(
                                            prior="loggamma",param=c(',input$BYM.shape,',',input$BYM.scale,')))
                                )',
                      ifelse(
                        length(FixedEffects)>0,
                        paste0(' + ', paste0(FixedEffects, collapse = ' + ')),
                        ''
                      ),
                      ifelse(
                        length(RandomEffects)>0,
                        paste0(' + ',  paste0('f(',RandomEffects,', model = "rw1")', collapse = ' + ')),
                        ''
                      ),'
                ')
      )
    )
    #
    inla.write.graph(adj(), filename = "graph.graph")
    output <- inla(formula = formula,
                   control.predictor = list(compute = TRUE),
                   control.compute=list(cpo=TRUE, dic=TRUE),
                   data = testq) # Default non-informative priors
    #
    updateProgress(detail = '\n Calculation of the BYM model finished')
    updateProgress(detail = '\n Organizing results')
    # 
    # 
    #
    output.aux =
      testq %>%
      bind_cols(output$summary.fitted.values) %>%
      group_by(`Assoc. Target`, idx) %>%
      summarise(x=mean(x), y=mean(y), value=mean(mean)) %>%
      rename('Var1'='x', 'Var2'='y')
    #
    
    #
    x = output.aux$Var1; y = output.aux$Var2; value = output.aux$value
    data = data.frame( x=x, y=y, z=value )
    data.loess = loess(z~x*y, data=data)
    data.fit = expand.grid(list(x=seq(min(x),max(x),l=50), y=seq(min(y),max(y),l=50)))
    value.aux = predict(data.loess, newdata=data.fit)
    #
    value.aux2 = as.data.frame(value.aux)
    #
    value.aux3 = rownames_to_column(value.aux2, var='Var1')
    value.aux4 = value.aux3 %>%
      gather(key='Var2', value='value', -Var1) %>%
      mutate(Var1=as.numeric(gsub('x=','',Var1)), Var2=as.numeric(gsub('y=','',Var2)))
    #
    dados$output = output
    dados$prd.m = value.aux4
    #
  })
  
  
  # =====
  # CARD
  output$CARD <- renderValueBox({
    #
    # req(dados$prd.m)
    #
    # browser()
    if ( !is.null(dados$output) ) {
      X = dados$prd.m %>% filter(value == max(value, na.rm=T)) %>% pull(Var1)
      Y = dados$prd.m %>% filter(value == max(value, na.rm=T)) %>% pull(Var2)
      Z = dados$prd.m %>% filter(value == max(value, na.rm=T)) %>% pull(value)
      valueBox(
        subtitle = h5(HTML('HOTSPOT COORDINATE')),
        value = tags$p(style = "font-size: 18px; font-weight: bold;",
                       HTML(paste0('X = ',round(X,4),'<br> Y = ',round(Y,4)))), # ,'<br> Z = ',round(Z,2)
        color = 'light-blue',
        width = '100%',
        icon = icon("list")
      )
    } else {
      valueBox(
        subtitle = h5(HTML('HOTSPOT COORDINATE')),
        value = tags$p(style = "font-size: 18px; font-weight: bold;", HTML(paste0(''))),
        color = 'light-blue',
        width = '100%',
        icon = icon("list")
      )
    }
    #
  })
  probability <- eventReactive(input$Probability, {
    #
    if(!is.null(dados$output)& !is.null(input$quantile))
    {
      aux = dados$output
      max.index = which.max(aux$summary.fitted.values$mean)
      t.marg = inla.tmarginal(function(x)x,aux$marginals.fitted.values[[max.index]])
      probb = mean(inla.rmarginal(10^4,t.marg)>=input$quantile)
      dados$probb = probb
    }
  })
  
  observeEvent(c(input$Probability),{
    
    req(dados$output,input$quantile)
    probability()
    
  })
  
  observeEvent(c(input$Calculate),{
    
    updateNumericInput(session,
                       inputId ='quantile' ,
                       label   = 'MEP',
                       value = NA
    )
    dados$probb=NULL
  })
  
  
  output$CARD2 <- renderValueBox({
    #
    
    if ( !is.null(dados$output)& is.null(dados$probb) ) {
      aux = dados$output
      max.index = which.max(aux$summary.fitted.values$mean)
      t.marg = inla.tmarginal(function(x)x,aux$marginals.fitted.values[[max.index]])
      hpd = inla.hpdmarginal(0.95,t.marg)
      valueBox(
        subtitle = h5(HTML('HOTSPOT CREDIBLE INTERVAL')),
        value = tags$p(style = "font-size: 18px; font-weight: bold;",
                       HTML(paste0('Low = ',round(hpd[1],4),'<br> High = ',round(hpd[2],4)))),
        color = 'light-blue',
        width = '100%',
        icon = icon("list")
      )
    } else if( !is.null(dados$output)& !is.null(dados$probb) ){
      probb =  dados$probb
      valueBox(
        subtitle = h5(HTML('TAIL PROBABILITY')),
        value = tags$p(style = "font-size: 18px; font-weight: bold;",
                       HTML(paste0(round(probb ,4)))),
        color = 'light-blue',
        width = '100%',
        icon = icon("list")
      )
    }else {
      valueBox(
        subtitle = h5(HTML('HOTSPOT CREDIBLE INTERVAL')),
        value = tags$p(style = "font-size: 18px; font-weight: bold;", HTML(paste0(''))),
        color = 'light-blue',
        width = '100%',
        icon = icon("list")
      )
    }
    
  })
  
  # =====
  # TMS BRAIN MAPPING
  output$TMS.BRAIN.MAPPING <- renderPlot({
    #
    req(dados$testq)
    #
    testq = dados$testq
    #
    # browser()
    x.min = min(dados$testq$x); x.max = max(dados$testq$x); x.range = abs(x.max-x.min)
    y.min = min(dados$testq$y); y.max = max(dados$testq$y); y.range = abs(y.max-y.min)
    h = 0.25
    #
    x.range = c(floor( x.min-h*x.range ), ceiling( x.max+h*x.range ))
    y.range = c(floor( y.min-h*y.range ), ceiling( y.max+h*y.range ))
    #
    # browser()
    TMS = testq %>%
      mutate(x = round(x, 0), y = round(y, 0)) %>%
      group_by(x, y) %>%
      summarise(mean = mean(r)) %>%
      ggplot(aes(x, y, z = mean)) + stat_summary_2d() +
      # 
      scale_fill_gradientn(colours=matlab.like(20), na.value=NA) +
      labs(fill=NULL) +
      scale_x_continuous(breaks = seq(0,60,by=15)) +
      scale_y_continuous(breaks = seq(-40,20,by=15)) +
      coord_cartesian(xlim = x.range, ylim = y.range) +
      coord_fixed() +
      # ggtitle("TMS BRAIN MAPPING | MEAN PER X-Y COORDINATES") +
      theme_minimal() +
      theme(text = element_text(size=16),
            legend.position = 'bottom',
            legend.key.width = unit(2.0,"cm"))
    # x11();
    TMS
    
  })
  
  # =====
  # TESTING ORDERS
  output$TESTING.ORDERS <- renderPlot({
    #
    req(dados$testq, input$Rep)
    #
    # browser()
    DB = dados$testq
    #
    if ( input$Rep1 ) {
      REP = unique(c(1,input$Rep))
    } else {
      REP = input$Rep
    }
    # 
    #
    DB.aux =
      # 
      tibble(i=REP) %>%
      rowwise() %>%
      mutate(
        rep = list(c(1:i)),
        data = list(
          DB %>%
            filter(Rep %in% rep) %>%
            group_by(`Assoc. Target`) %>%
            summarise(r = mean(r))
        )
      ) %>%
      select(-rep) %>%
      unnest(cols = data)
    #
    TO = DB %>%
      ggplot(aes(x=`Assoc. Target`,y=r)) +
      geom_boxplot(outlier.shape = NA) +
      geom_point(data = DB.aux, aes(colour = factor(i)), size = 3) +
      labs(colour = 'Replication', y = 'Summary Signal') +
      theme_minimal() +
      theme(
        legend.position = 'bottom',
        text = element_text(size=16),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    TO
    #
  })
  
  # 
  
  output$HOTSPOT <- renderPlot({
    #
    req(dados$testq, dados$prd.m)
    #
    testq = dados$testq
    #
    x.min = min(testq$x); x.max = max(testq$x); x.range = abs(x.max-x.min)
    y.min = min(testq$y); y.max = max(testq$y); y.range = abs(y.max-y.min)
    h = 0.25
    #
    x.range = c(floor( x.min-h*x.range ), ceiling( x.max+h*x.range ))
    y.range = c(floor( y.min-h*y.range ), ceiling( y.max+h*y.range ))
    #
    # browser()
    G.HOTSPOT =
      dados$prd.m %>%
      ggplot() +
      geom_raster(aes(x=Var1, y=Var2, fill=value)) +
      # 
      # 
      scale_fill_gradientn(colours=matlab.like(20), na.value=NA) +
      labs(x='x', y='y', fill=NULL) +
      scale_x_continuous(breaks = seq(0,60,by=15)) +
      scale_y_continuous(breaks = seq(-40,20,by=15)) +
      coord_cartesian(xlim = x.range, ylim = y.range) +
      coord_fixed() +
      theme_minimal() +
      theme(text = element_text(size=16),
            legend.position  ='none',
            legend.key.width = unit(2.0,"cm"))
    G.HOTSPOT
    #
  })
  
  output$REPLICATION <- renderPlot({
    #
    req(dados$testq, dados$output, dados$output$summary.random$Rep)
    #
    # browser()
    # Replication effect #####
    G.REPLICATION =
      dados$output$summary.random$Rep %>%
      ggplot() +
      geom_line(aes(x=ID, y=mean), size=1) +
      geom_hline(yintercept=c(0), colour='black', linetype='dashed') +
      labs(x='Replication', y='Effect') +
      theme_minimal() +
      theme(text = element_text(size=16))
    G.REPLICATION
    #
  })
  
  
  output$PLOT3D <- renderPlotly({
    #
    req(dados$RawData, dados$prd.m)
    #
    # browser()
    prd.m = dados$prd.m
    testq = dados$testq
    #
    prd.m =
      prd.m %>%
      mutate(x.round = round(prd.m$Var1,0),
             y.round = round(prd.m$Var2,0))
    #
    testq =
      testq %>%
      mutate(x.round = round(testq$x,0),
             y.round = round(testq$y,0))
    #
    testq =
      testq %>%
      left_join( prd.m %>% select(value,x.round,y.round) , by = c('x.round','y.round') )
    #
    
    # browser()
    PLOT.3D = plot_ly(testq, x = ~x, y = ~y, z = ~z, color = ~value, colors = matlab.like(20)) %>%
      add_markers(size=3, showlegend = FALSE)
    PLOT.3D
    #
  })
  
  output$SUMMARY.OUTPUT <- renderPrint({
    #
    req(dados$output)
    #
    summary(dados$output)
    #
  })
  
  output$datatable <- renderDataTable(dados$RawData)
  
  output$downloadReport <- downloadHandler(
    filename = function() {
      
      paste('TMS-report', sep = '.', 'html')
    },
    content = function(file) {
      #src <- system.file("report.Rmd",package="TMSBrainApp")#normalizePath('report.Rmd')
      src <- paste0(getwd(),'/report.Rmd')
      
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'report.Rmd', overwrite = TRUE)
      
      out <- rmarkdown::render('report.Rmd',
                               params = list(plo=dados,Rep=input$Rep),
                               #switch(input$format,
                               #      PDF = pdf_document(),
                               #      HTML = html_document()
                               html_document()
      )
      file.rename(out, file)
    }
  )
  
}

)

