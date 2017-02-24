shinyServer(


  function(input, output, session) {
  
    ####################Getting all the uploaded data####################

    
#' makeList
#'
#' @param data - the initial uploaded data
#'
#' @return a list in the format needed required by the MARSS package
#' @export
#'
#' @examples
    makeList<- function(data){
      l<-list()
      for ( i in 1:length(data)){
        l[[toString(data[i])]]<-data[i]
      }
      return(l)
    }
    
    
    
    #' data
    #' reactive function, updates all the ui components according to the loaded data
    #' @param inFile - data loaded into the application 
    #' 
    #'
    data<-reactive({
      inFile<- input$data
      req(inFile)
      table<-read.csv(inFile$datapath)
      vars<-names(table)
      vars1<-makeList(vars)
      
      #Updating the ui with all the parameters availiable from the table
      updateSelectInput(session, "paramsAutoArima","Select Columns:", choices = vars)
      updateSelectInput(session, "paramsState","Select Columns:", choices = vars)
      updateSelectInput(session, "paramsDlm","Select Columns:", choices = vars)
      updateSelectInput(session, "explainDlm","Choose explanatory variable:", choices = vars)
      updateSelectInput(session, "paramsArima", "Select data:", choices = vars)
      updateCheckboxGroupInput(session, inputId="xregParamsArimax", choices = vars, selected=NULL)
      updateCheckboxGroupInput(session, inputId="paramsMARSS", choices = vars, selected=vars[1])
      updateCheckboxGroupInput(session, inputId="paramsData", choices = vars, selected=vars)
      table
    })
    
    #######################Outputting the uploaded table##########################

    #' output$contents
    #' 
    #' reactive function
    #' outputs the data table with updated parameters choice
    #' @inherit updatedData()
    #' @examples
    output$contents <- DT::renderDataTable({  
      DT::datatable(updatedData())
    })
    
    
    #' updatedData
    #'
    #' updates the data.frame selection according to the user input 
    #'
    #' @inherit input$paramsData
    #' @examples
    updatedData<- reactive({
      data()[,input$paramsData]
    })
    
    
    #' output$dataPlot
    #'
    #' Plots all the input data as a Dygraph
    #' @param updatedData()
    #' 
    #' @examples
    output$dataPlot<- renderDygraph({
      dygraph(updatedData()) %>%
        dyRangeSelector()
    })


    ####################### Arima ###############################
    
    
    ###################Fitting an Arima model#####################
    #' arimaFit()
    #' reactive function, fit the given data to an arima model
    #' according to the provided parameters
    #' 
    #' @param input$arimaModel, "Manual" or "Auto" 
    #' @param input$paramsArima, data columns to use as observation variable
    #' @param input$arimaOrder, order of the arima model if "Auto"
    #' @param xreg() reactive function that takes parameters chosen to be predictors
    #'
    #' @return an object class arima()
    #' 
    #' @examples
    arimaFit<-reactive({
      
      inFile<- data()
      if (input$arimaModel == "Manual"){
        params<-input$paramsArima
        data1<-inFile[, params]
        x<-input$arimaOrder
        x<-as.numeric(unlist(strsplit(x, ",")))
        if (is.null(xreg())){
          model<-Arima(data1, order = x)
        } else {
          model<- Arima(data1, order = x, xreg = inFile[, xreg()])
        }
      } else {
        data1<-inFile[, input$paramsAutoArima]
        if (is.null(xreg())){
          model<- auto.arima(data1)
        } else {
          model<- auto.arima(data1, xreg=inFile[, xreg()])
        }
      }
      model
    })
    
    
    
    #################### Forecasting the data with Arima###################
    #' arimaForecast()
    #' a reactive function that forecasts data according to the fitted model
    #' 
    #' @param arimaFit() model fitted, object of class arima
    #' @param input$arimaPeriod period for prediction, 10 by default 
    #' @param xreg() reactive function that takes parameters chosen to be predictors
    #'
    #' @return an object of class forecast

    #' @examples
    arimaForecast<- reactive({
      if (is.null(xreg())){
        
        f<- forecast(arimaFit(), h=input$arimaPeriod)
      } else{
        f<-forecast(arimaFit(), h=input$arimaPeriod, xreg=data()[,xreg()])
      }
      f
      
    })
    
    
    #' logTextArima
    #' function to capture console output in R to display in the UI
    #'
    #' @param arimaFit() 
    #'
    #' @return output for the console
    #' @examples
    logTextArima <- reactive({
      capture.output(arimaFit())
    })
    
    #' xreg
    #' reactive function, takes the list of parameters to beused as explanatory variables
    #'
    #' @param input$xregParamsArimax list of parameters 
    #' @examples
    xreg<- reactive({
      input$xregParamsArimax
    })
    
    ########################Construction forecast output######################
    
    observeEvent(input$analyseArima, {
      #' output$console
      #'
      #' @param logTextArima() - concole text from R session 
      #' 
      #' @return console output in the UI
      #' @examples
      output$console<- renderPrint({
        logTextArima()
      })
      
      
      #' output$arimaForecast
      #'
      #' reactive function, construct a DT:data table for the UI
      #' @param arimaForecast() - an object of class forecast 
      #' 
      #' @return a DT: data.frame in the UI
      #' 
      #' @examples
      output$arimaForecast<- DT::renderDataTable({
          table<- data.frame(arimaForecast())
          colnames(table)<-c("Forecast", "Low 80", "High 80", "Low 95", "High 95")
          DT::datatable(table)
      })
      
      
      #' output$arimaPlot
      #'
      #' a reactive function renders the fitted values plot 
      #' 
      #' @examples
      output$arimaPlot<-renderPlot({
        plot(arimaFit()$x)
      })
      
      
      #' output$arimaForecastPlot
      #'
      #' a reactive function, renders the forecast plot for arima models
      #' 
      #' @examples
      output$arimaForecastPlot<-renderPlot({
        plot(arimaForecast())
      })
    })
    
    
    ####################Warnings handler####################
    
    output$warn <- reactive({
      return(length(warning())>1)
    })
    
    outputOptions(output, 'warn', suspendWhenHidden=FALSE)

    
  ##################################Space-State modeling#########################
  
    #' state
    #'
    #' a reactive function, constructs a Structural type of state models 
    #' 
    #' @examples
  state<-reactive({
    inFile<- data()
    data1<-inFile[, input$paramsState]
    type<-input$StateType
    StructTS(data1, type=type)
  })
  
  
  #################### Linear regression space state model with constant coefficients #################
#' Title
#'
#' @param v 
#'
#' @return
#' @export
#'
#' @examples
  buildModRegConst<-function(v){
    ##########################
    ###TODO: 4 parameters input handler
    ############################
    dV<- exp(v[1])
    dW<- c(exp(v[2]), 0)
    m0<- v[3:4]
    data<-data()
    x<- data[, input$explainDlm]
    dlmModReg(x, dV= dV, dW=dW, m0=m0)
    
  } 
  
  #_______ Linear regression space state model with time varying coefficients
    
    #' Title
    #'
    #' @param v 
    #'
    #' @return
    #' @export
    #'
    #' @examples
  buildModRegVariant<-function(v){
    ##########################
    ###TODO: 4 parameters input handler
    ############################
    dV<- exp(v[1])
    dW<- c(exp(v[2:3]))
    m0<- v[4:5]
    data<- data()
    x<- data[, input$explainDlm]
    dlmModReg(x, dV= dV, dW=dW, m0=m0)
    
  }  
  
  
  #_________Constructing manual dlm_________________
  
    #' Title
    #'
    #' @param v 
    #'
    #' @return
    #' @export
    #'
    #' @examples
  buildDlmPoly<- function(p){
    #n <- input$dlmPolyOrder
    n<- input$dlmPolyOrder
    rw <- dlmModPoly(n, dV=p[1], dW=c(rep(p[2], n - 1), 1), C0 = p[3]*diag(nrow = n), rep(p[4], n))
    return(rw)
  }
  
    
    #' Title
    #'
    #' @param v 
    #'
    #' @return
    #' @export
    #'
    #' @examples
  buildDlmSeas<- reactive({
    if (input$seasType=="Seasonal"){
      dlmModSeas( frequency= input$seasFreq, dV=input$seasVarNoise, dW=input$seasVarSys)
    } else if (input$seasType=="Fourier form"){
      dlmModTrig(q=input$trigHarmonics, tau=input$trigPeriod, dV=input$trigVarNoise ,dW=input$trigVarSys)
    }
  })
  
    
    
#' Title
#'
#' @param p 
#' @param data 
#'
#' @return
#' @export
#'
#' @examples
  fitDlmPoly<- function(p, data){
    mod<- dlmMLE(data, parm = p, build = buildDlmPoly )
    return(mod)
  }
  
  
  
  #____________Calculating startign values for the optim in dlm()
    #' Title
    #'
    #' @param v 
    #'
    #' @return
    #' @export
    #'
    #' @examples
  initGuessParams<- reactive({
    inFile<- data()
    data<-inFile[, input$paramsDlm]
    varGuess<- var(diff(data), na.rm=TRUE)
    mu0Guess<-data[1]
    lambdaGuess<-mean(diff(data), na.rm=TRUE)
    if (input$typeDlm=="Time-varying coefficients"){
      params<-c(log(varGuess), log(varGuess/5), log(varGuess/5), mu0Guess, lambdaGuess)
      mle<- dlmMLE(data, parm = params, build = buildModRegVariant,  method = "Nelder-Mead") 
    } else if (input$typeDlm=="Manual"){
      params<- c(log(varGuess), log(varGuess/5), mu0Guess, lambdaGuess)
      mle<- dlmMLE(data, parm = params, build = buildDlmPoly,  method = "Nelder-Mead")
    } else {
      params<- c(log(varGuess), log(varGuess/5), mu0Guess, lambdaGuess)
      mle<- dlmMLE(data, parm = params, build = buildModRegConst,  method = "Nelder-Mead")
    }
    mle
  })
  
  
  #__________Constructing a dlm model with the paramteres
    #' Title
    #'
    #' @param v 
    #'
    #' @return
    #' @export
    #'
    #' @examples
  dlm<-reactive({
    dv<-as.numeric(input$dlmParamsDv)
    dw<- input$dlmParamsDw
    dw<-as.numeric(unlist(strsplit(dw, ",")))
    m0<-input$dlmParamsM0
    m0<-as.numeric(unlist(strsplit(m0, ",")))
    c0<-input$dlmParamsC0
    c0<-as.numeric(unlist(strsplit(c0, ",")))
    init<-initGuessParams()
    inFile<- data()
    data<-inFile[, input$paramsDlm]
    p<- c(dv,dw, c0, m0)
    if (input$typeDlm=="Time-varying coefficients"){
      dlm<- buildModRegVariant(p)
    } else if (input$typeDlm=="Manual"){
      
      if (input$seasType=="No seasonality"){
        pars<- fitDlmPoly(p, data)
        dlm<- buildDlmPoly(pars$par)
      } else {
        pars<- fitDlmPoly(p, data)
        dlm<-buildDlmPoly(pars$par)+ buildDlmSeas() 
      }
    } else {
      dlm<-buildModRegConst(p)
    }
    dlm
    
  })
  
  
  #__________Smoothign the data usign the dlm model
    #' Title
    #'
    #' @param v 
    #'
    #' @return
    #' @export
    #'
    #' @examples
  smoothDlm<- reactive({
    inFile<- data()
    data<-inFile[, input$paramsDlm]
    model<- dlm()
    smooth<-dlmSmooth(data, model)
    smooth
  })
  
  
  #___________Kalman Filtering for the dlm model
    #' Title
    #'
    #' @param v 
    #'
    #' @return
    #' @export
    #'
    #' @examples
  filterDlm<-reactive({
    inFile<- data()
    data<-inFile[, input$paramsDlm]
    model<- dlm()
    fil<-dlmFilter(data, model)
    fil
    
  })
  
    #__________MARSS handler__________
    
    #' Title
    #'
    #' @param v 
    #'
    #' @return
    #' @export
    #'
    #' @examples
    MARSSHandler<-reactive({
      inFile<- input$data
      req(inFile)
      table<-read.csv(inFile$datapath)
      dat<-table[, input$paramsMARSS]
      dat<-sapply(dat, as.numeric)
      dat<-as.matrix(dat)
      dat<-t(dat)
      dat
    })
    
    
    #' Title
    #'
    #' @param v 
    #'
    #' @return
    #' @export
    #'
    #' @examples
    marss<-reactive({
      dat<-MARSSHandler()
      mars<-MARSS(dat)
      mars
    })
  
    
    #' Title
    #'
    #' @param v 
    #'
    #' @return
    #' @export
    #'
    #' @examples
  marssSmooth<- reactive({
    model<-marss()
    s<-MARSSkfss(model)$xtT
    s
  })
  
    
    #' Title
    #'
    #' @param v 
    #'
    #' @return
    #' @export
    #'
    #' @examples
  marssKalman<- reactive({
    model<-marss()
    k<-MARSSkfss(model)$xtt
    k
    })
  
    
    #' Title
    #'
    #' @param v 
    #'
    #' @return
    #' @export
    #'
    #' @examples
  marssForecast<- reactive({
    f<-stateForecast()
    forec<-data.frame(f$sim.data)
    forec2<-data.frame(f$sim.states)
    a<-data.frame(t(forec))
    b<-data.frame(t(forec2))
    table<- data.frame(a,b)
    table
  })

  
    observeEvent(input$analyseState,{
      #' Title
      #'
      #' @param v 
      #'
      #' @return
      #' @export
      #'
      #' @examples
      modelState<- reactive({
        model<- switch(input$StateModel,
                       "dlm"={
                         dlm()
                       },
                       "Structural"={
                         state()
                       },
                       "Autoregressive"={
                         marss()
                       })
        model
      })
      
      
      #' Title
      #'
      #' @param v 
      #'
      #' @return
      #' @export
      #'
      #' @examples
      output$stateModel<- renderPrint({
        model<- modelState()
        capture.output(model)
      })
      
      
      #' Title
      #'
      #' @param v 
      #'
      #' @return
      #' @export
      #'
      #' @examples
      logTextState <- reactive({
        capture.output(modelState())
      })
      
      
      #' Title
      #'
      #' @param v 
      #'
      #' @return
      #' @export
      #'
      #' @examples
      output$consoleState<- renderPrint({
        logTextState()
      })
      
      
      #' Title
      #'
      #' @param v 
      #'
      #' @return
      #' @export
      #'
      #' @examples
      output$stateDiag<-renderPlot({
        
        if ( input$StateModel=='Structural'){
          tsdiag(modelState())
        } else if (input$StateModel=='dlm'){
          tsdiag(filterDlm())
        } else {
          NULL
        }
      })
      
      
      #' Title
      #'
      #' @param v 
      #'
      #' @return
      #' @export
      #'
      #' @examples
      dlmPredict<- reactive({
        data<- filterDlm()
        if (input$typeDlm=='Manual'){
          fore<- dlmForecast(data, nAhead = input$statePeriod, sampleNew=100)
          ciTheory20 <- (outer(sapply(fore$Q, FUN=function(x) sqrt(diag(x))), qnorm(c(0.2,0.8))) +
                         as.vector(t(fore$f)))
          ciTheory5 <- (outer(sapply(fore$Q, FUN=function(x) sqrt(diag(x))), qnorm(c(0.05,0.95))) +
                           as.vector(t(fore$f)))
          Forecast<-cbind(fore$f[,1], ciTheory20, ciTheory5)
          Forecast
        } else {
        }
      })
      
      
      #' Title
      #'
      #' @param v 
      #'
      #' @return
      #' @export
      #'
      #' @examples
      stateForecast<- reactive({
        if ( input$StateModel=="Structural"){
          data<-state()
           f<-forecast(data, h= input$statePeriod)
        } else if (input$StateModel=="dlm"){
          model<- filterDlm()
          fore<- dlmForecast(model, nAhead = input$statePeriod, sampleNew=100)
          ciTheory20 <- (outer(sapply(fore$Q, FUN=function(x) sqrt(diag(x))), qnorm(c(0.2,0.8))) +
                           as.vector(t(fore$f)))
          ciTheory5 <- (outer(sapply(fore$Q, FUN=function(x) sqrt(diag(x))), qnorm(c(0.05,0.95))) +
                          as.vector(t(fore$f)))
          f<-data.frame(fore$f[,1], ciTheory20, ciTheory5)
          
        } else {
          model<- marss()
          data<- MARSSHandler()[1,]
          f<-predict(model,n.ahead=input$statePeriod,t.start=length(data)+1)
        }
        f
      })
      
      
      #' Title
      #'
      #' @param v 
      #'
      #' @return
      #' @export
      #'
      #' @examples
      MARSSforecastTable<- reactive({
        f<-stateForecast()
        a<-t(f$E.y)
        tab<- data.frame(a)
        colnames(tab)<-input$paramsMARSS
        tab
      })
      
      
      #' Title
      #'
      #' @param v 
      #'
      #' @return
      #' @export
      #'
      #' @examples
      output$stateForecast<- DT::renderDataTable({
        
        if (input$StateModel=="Structural"){
          table<- data.frame(stateForecast())
          colnames(table)<-c("Forecast", "Low 80", "High 80", "Low 95", "High 95") 
        } else if (input$StateModel=="Autoregressive"){
          table<-MARSSforecastTable()
        } else {
          data<-stateForecast()
          table<-data.frame(data)
          colnames(table)<-c("Forecast", "Low 80", "High 80", "Low 95", "High 95") 
        }
        DT::datatable(table)
      })
      
      
      #' Title
      #'
      #' @param v 
      #'
      #' @return
      #' @export
      #'
      #' @examples
      dlmPlot<- reactive({
        data1<- data()
        data<-data1[, input$paramsDlm]
        filtered<-filterDlm()
        smooth<-smoothDlm()
        if (input$typeDlm=="Manual"){
          if ( input$seasType=='No seasonality' && input$dlmPolyOrder==1){
            filt<-dropFirst(filtered$m)
            smoothed<-dropFirst(smooth$s)
          } else {
            filt<-dropFirst(filtered$m[,1])
            smoothed<-dropFirst(smooth$s[,1]) 
          }
        } else {
          x<- data1[, input$explainDlm]
          filt<- filtered$m[-1,1]+ x* filtered$m[-1,2]
          smoothed<- smooth$s[-1,1]+ x* smooth$s[-1,2]
        }
        plot(data, type="o", pch=19, bg="black")
        lines(filt ,lty = "dashed", lwd = 2, col="red")
        lines(smoothed,lty = "dotted", lwd = 2, col="blue")
        legend("bottomright", legend = c("Data", "Filtered","Smoothed"), lty = c(1,2,3), pch=c(19, NA, NA), col=c("black", "red", "blue"), lwd=c(1,2,2))
        
      })
      
      
      #' Title
      #'
      #' @param v 
      #'
      #' @return
      #' @export
      #'
      #' @examples
      structuralPlot<- reactive({
        data<-state()
        data1<- data()
        data1<-data1[, input$paramsState]
        plot(data1, type="o", pch=19, bg="black")
        lines(fitted(data)[,1],lty = "dashed", lwd = 2, col="red")
        lines(tsSmooth(data)[,1],lty = "dotted", lwd = 2, col="blue")
        legend("bottomright", legend = c("Data", "Filtered","Smoothed"), lty = c(1,2,3), pch=c(19, NA, NA), col=c("black", "red", "blue"), lwd=c(1,2,2))
        
      })
      
      
      #' Title
      #'
      #' @param v 
      #'
      #' @return
      #' @export
      #'
      #' @examples
      output$MARSSplotData <- renderUI({
        plot_output_list <- lapply(1:length(input$paramsMARSS), function(i) {
          plotname <- paste("plot", i, sep="")
          plotOutput(plotname)
        })
        # Convert the list to a tagList - this is necessary for the list of items
        # to display properly.
        do.call(tagList, plot_output_list)
      })
      
      observe({ 
        for (m in 1:length(input$paramsMARSS)) {
          local({ 
            i<-m
            plotname <- paste("plot", i, sep="")
            output[[plotname]] <- renderPlot({
              plot(MARSSHandler()[i,], type="o", pch=19, bg="black", main=input$paramsMARSS[i])
              lines(marssKalman()[i,],lty = "dashed", lwd = 2, col="red")
              lines(marssSmooth()[i,],lty = "dotted", lwd = 2, col="blue")
              legend("bottomright", legend = c("Data", "Filtered","Smoothed"), lty = c(1,2,3), pch=c(19, NA, NA), col=c("black", "red", "blue"), lwd=c(1,2,2))
              
            })
          })#endlocal
        }
        
      })
      
      
      #' Title
      #'
      #' @param v 
      #'
      #' @return
      #' @export
      #'
      #' @examples
      output$stateFittedPlot<- renderPlot({
        if (input$StateModel=="dlm"){
          dlmPlot()
         } else if (input$StateModel=="Structural"){
           structuralPlot()
         }})
      
      
      #Function creating a data.frame in the form required by ggplot in dlmForecastPlot()
      
      
      #' Title
      #'
      #' @param v 
      #'
      #' @return
      #' @export
      #'
      #' @examples
      stateForecastPlotDataHandler<- reactive({
        data1<- data()
        data<-data.frame(data1[, input$paramsDlm])
        colnames(data)<- c("data")
        data$x<- c(1:length(data$data))
        fore<-stateForecast()
        fore<-data.frame(fore)
        colnames(fore)<-c("mean","min80", "max80", "min95", "max95")
        m1<- length(data$x)+1
        m2<- length(data$x)+ length(fore$mean)
        fore$x<-c(m1:m2)
        f<-full_join(data, fore, by = "x")
        f
      })
      
      
      #ggplot functionality like in arime.plot()
      
      
      #' Title
      #'
      #' @param v 
      #'
      #' @return
      #' @export
      #'
      #' @examples
      dlmForecastPlot<- reactive({
        f<- stateForecastPlotDataHandler()
        g<- ggplot(f, aes(x=x, y=data)) + geom_line() + geom_line(aes(x=x, y=mean), colour = "blue")
        g<- g + geom_ribbon(aes(x=x, ymin=min80, ymax=max80),  fill = "lightsteelblue4",  alpha=0.3)
        g<- g + geom_ribbon(aes(x=x, ymin=min95, ymax=max95),  fill = "grey50",  alpha=0.3)
        g
      })
      
      
      #' Title
      #'
      #' @param v 
      #'
      #' @return
      #' @export
      #'
      #' @examples
      marssForecastPlot<-reactive({
        data<- MARSSHandler()[1,]
        f<-stateForecast()
        a<-data.frame(f$sim.data)
        forecast<-t(a)
        plot(forecast[,1], type = 'o', lwd = 2, pch = 16, ylab = "Data")
        lines(data,type = 'o')
        #abline(v = length(data)+0.5, lty = "dashed")
      })
      
      
      #' Title
      #'
      #' @param v 
      #'
      #' @return
      #' @export
      #'
      #' @examples
      output$MARSSForecastPlot<- renderUI({
        plot_output_list1 <- lapply(1:length(input$paramsMARSS), function(i) {
          plotname1 <- paste("plot1", i, sep="")
          plotOutput(plotname1)
        })
        # Convert the list to a tagList - this is necessary for the list of items
        # to display properly.
        do.call(tagList, plot_output_list1)
      })
      
      observe({ 
        for (m in 1:length(input$paramsMARSS)) {
          local({ 
            i<-m
            plotname1 <- paste("plot1", i, sep="")
            output[[plotname1]] <- renderPlot({
              d<-MARSSHandler()[i,]
              f1<-MARSSforecastTable()[,i]
              f<-MARSSForecastPlotDataHandler(d,f1)
              g<- ggplot(f, aes(x=x, y=data)) + geom_line() + geom_line(aes(x=x, y=forecast), colour = "blue")
              g<- g + geom_ribbon(aes(x=x, ymin=forecast-0.2*forecast, ymax=forecast+0.2*forecast),  fill = "lightsteelblue4",  alpha=0.3)
              g<- g + geom_ribbon(aes(x=x, ymin=forecast-0.05*forecast, ymax=forecast+0.05*forecast),  fill = "grey50",  alpha=0.3)
              g+ ggtitle(paste(input$paramsMARSS[i]))
              g
            })
          })#endlocal
        }
      })
      
      
#' Title
#'
#' @param d 
#' @param forecast 
#'
#' @return
#' @export
#'
#' @examples
      MARSSForecastPlotDataHandler<- function(d, forecast){
        data<-data.frame(d)
        data$x<- c(1:length(d))
        m1<- length(data$x)+1
        m2<- length(data$x)+ length(forecast)
        forecast<-data.frame(forecast)
        forecast$x<-c(m1:m2)
        f<-full_join(data, forecast, by = "x")
        colnames(f)<- c("data", "x", "forecast")
        return(f)
      }
      
      
      #' Title
      #'
      #' @param v 
      #'
      #' @return
      #' @export
      #'
      #' @examples
      output$stateForecastPlot<-renderPlot({
        if (input$StateModel=="dlm"){
          dlmForecastPlot()
        } else if (input$StateModel=="Autoregressive"){
          marssForecastPlot()
        } else {
          plot(stateForecast()) 
        }
      })
    })
  })


