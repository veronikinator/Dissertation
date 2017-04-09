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
    #' @description reactive function, updates all the ui components according to the loaded data
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
      updateSelectInput(session, inputId="paramsArima", "Select data:", choices = vars)
      updateCheckboxGroupInput(session, inputId="xregParamsArimax", choices = vars, selected=NULL)
      updateCheckboxGroupInput(session, inputId="xregParamsAutoArimax", choices = vars, selected=NULL)
      updateCheckboxGroupInput(session, inputId="paramsMARSS", choices = vars, selected=vars[1])
      updateCheckboxGroupInput(session, inputId="paramsData", label="Choose columns", choices = vars, selected=vars)
      table
    })
    
    
    output$DataParams<-renderUI({
      checkboxGroupInput("paramsData", label="", choices = NULL)
    })
    
    #######################Outputting the uploaded table##########################

    #' output$contents
    #' 
    #' @description reactive function, outputs the data table with updated parameters choice
    #' @inherit updatedData()
    #' @examples
    output$contents <- DT::renderDataTable({  
      DT::datatable(updatedData())
    })
    
    #' updatedData
    #'
    #' @description Updates the data.frame selection according to the user input 
    #'
    #' @inherit input$paramsData
    #' @examples
    updatedData<- reactive({
      data()[,input$paramsData]
    })
    
    
    #' output$dataPlot
    #'
    #' @description Plots all the input data as a Dygraph
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
    #' @description reactive function, fit the given data to an arima model 
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
    #' @description a reactive function that forecasts data according to the fitted model
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
    #' @description function to capture console output in R to display in the UI
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
      #' @description reactive function, construct a DT:data table for the UI
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
      #'  @description a reactive function renders the fitted values plot 
      #' 
      #' @examples
      output$arimaPlot<-renderPlot({
        plot(arimaFit()$x)
      })
      
      
      #' output$arimaForecastPlot
      #'
      #' @description a reactive function, renders the forecast plot for arima models
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
    #' @description a reactive function, constructs a Structural type of state models 
    #' 
    #' @examples
  state<-reactive({
    inFile<- data()
    data1<-inFile[, input$paramsState]
    type<-input$StateType
    StructTS(data1, type=type)
  })
  
  
  #################### Linear regression space state model with constant coefficients #################
#' buildModRegConst
#' @description Fitting function for the manual dlm regression
#'
#' @param v -  list of initial values for the parameters [dV, dW, m0] 
#'
#' @return an object class dlm with estimated parameters
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
    
    #' buildModRegVariant
    #' @description Fitting function for the manual dlm with time variant coefficients regression
    #' @param v -  list of initial values for the parameters [dV, dW, m0]  
    #'
    #' @return an object class dlm with estimated parameters
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
  
    #' buildDlmPoly
    #' 
    #' @description Constructing function for the manual dlm with fixed coefficients
    #' @param v -  list of initial values for the parameters [dV, dW, m0]  
    #'
    #' @return an object class dlm with estimated parameters
    #' @export
    #'
    #' @examples
  buildDlmPoly<- function(p){
    #n <- input$dlmPolyOrder
    n<- input$dlmPolyOrder
    rw <- dlmModPoly(n, dV=p[1], dW=c(rep(p[2], n - 1), 1), C0 = p[3]*diag(nrow = n), rep(p[4], n))
    return(rw)
  }
  
    
    #' buildDlmSeas
    #' @description A reactive function, constructs the seasonal part of a dlm
    #'
    #'
  buildDlmSeas<- reactive({
    if (input$seasType=="Seasonal"){
      dlmModSeas( frequency= input$seasFreq, dV=input$seasVarNoise, dW=input$seasVarSys)
    } else if (input$seasType=="Fourier form"){
      dlmModTrig(q=input$trigHarmonics, tau=input$trigPeriod, dV=input$trigVarNoise ,dW=input$trigVarSys)
    }
  })
  
    
    
#' fitDlmPoly
#' @description The function returns the MLE of unknown parameters in the specification of a state space model
#'
#' @param p - vector of inital values for the parameters
#' @param data time series data to be fitted
#'
#' @return The function dlmMLE returns the value returned by optim.
#' @export
#'
#' @examples
  fitDlmPoly<- function(p, data){
    mod<- dlmMLE(data, parm = p, build = buildDlmPoly )
    return(mod)
  }
  
  
  #__________Constructing a dlm model with the paramteres
    #' dlm
    #' 
    #' @description A reactive function, constructs the dlm model with fitted parameters
    #'
    #' @return returns an object of class dlm
  dlm<-reactive({
    dv<-as.numeric(input$dlmParamsDv)
    dw<- input$dlmParamsDw
    dw<-as.numeric(unlist(strsplit(dw, ",")))
    m0<-input$dlmParamsM0
    m0<-as.numeric(unlist(strsplit(m0, ",")))
    c0<-input$dlmParamsC0
    c0<-as.numeric(unlist(strsplit(c0, ",")))
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
    #' smoothDlm
    #' @description A reactive function, applies Kalman smoother to the dlm model to compute 
    #' smoothed values of the state vectors, together with their variance/covariance matrices.
    #' 
    #' @return Time series (or matrix) of smoothed values of the state vectors
  smoothDlm<- reactive({
    inFile<- data()
    data<-inFile[, input$paramsDlm]
    model<- dlm()
    smooth<-dlmSmooth(data, model)
    smooth
  })
  
  
  #___________Kalman Filtering for the dlm model
    #' filterDlm
    #' @description A reactive function, applies Kalman filter to the dlm model to compute filtered values of the state vectors.
    #' 
    #' @return Time series (or matrix) of filtered values of the state vectors
    #' 
  filterDlm<-reactive({
    inFile<- data()
    data<-inFile[, input$paramsDlm]
    model<- dlm()
    fil<-dlmFilter(data, model)
    fil
    
  })
  
    #__________MARSS handler__________
    
    #' MARSSHandler
    #' @description A reactive function, transform the data frame into matrix required by MARSS package
    #' 
    #' @return a transposed matrix with the data
    #' 
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
    
    
    #' marss
    #' @description A reactive function, uses optim to estimate the parameters for the model for the given data set 
    #' 
    #' @return an object class marssMLE
    #' 
    marss<-reactive({
      dat<-MARSSHandler()
      mars<-MARSS(dat)
      mars
    })
  
    
    #' marssSmooth
    #' @description A reactive function, applies Kalman smoother to the marss model to compute 
    #' smoothed values of the state vectors, together with their variance/covariance matrices.
    #' 
    #' @return State first moment conditioned on y(1:T): E[x(t) | y(1:T)] (m x T matrix). Kalman smoother output.
    #' 
  marssSmooth<- reactive({
    model<-marss()
    s<-MARSSkfss(model)$xtT
    s
  })
  
    
    #' marssKalman
    #' @description A reactive function, applies Kalman filter to the marss model to compute filtered values of the state vectors.
    #' 
    #' @return State first moment conditioned on y(1:t): E[x(t) | y(1:t)] (m x T). Kalman filter output. (S&S eqn 6.17 with s=t)
    #' 
  marssKalman<- reactive({
    model<-marss()
    k<-MARSSkfss(model)$xtt
    k
    })
  
    
    #' marssForecast
    #' @description A reactive forecasting function for the marss models.
    #'
    #' @return data - data frame with the forecasting ( simulated) values. 
    #' 
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
      #' modelState
      #' @description A reactive function, switches between different Space-State models according to the user input.
      #' 
      #' @return a fitted model
      #' 
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
      
      
      #' logTextState
      #' @description A reactive function, constructs the console output for the Space State models
      #' 
      logTextState <- reactive({
        a<-capture.output(modelState())
        if (input$StateModel=="Structural"){
          a<-paste(toString(a),"LogLik", capture.output(modelState()$loglik), sep=" ")
        }
        if (input$StateModel=="dlm"){
          inFile<- data()
          data<-inFile[, input$paramsDlm]
          a <- paste(toString(a),"LogLik", capture.output(dlmLL(data, dlm())))
        }
        a
      })
      
      
      #' output$consoleState
      #' @description A reactive output function, outputs the console data for the Space-State models
      #' 
      output$consoleState<- renderPrint({
        logTextState()
      })
      
      
      #' output$stateDiag
      #' @description A rendering reactive function, constructs the diagnostics plot for Space State models
      #' 
      output$stateDiag<-renderPlot({
        
        if ( input$StateModel=='Structural'){
          tsdiag(modelState())
        } else if (input$StateModel=='dlm'){
          tsdiag(filterDlm())
        } else {
          NULL
        }
      })
      
      
      #' stateForecast
      #' @description A reactive function, constructs forecast for different 
      #' Space State models according the user input
      #'
      #'
      #' @return a data frame with forecasted values  and CI
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
      
      
      #' MARSSforecastTable
      #' @description A reactive function, constructs forecasting table for the MARSS momdels
      #'
      #' 
      #' @return a data.frame to be outputed in the ui for the MARSS model
      #'
      MARSSforecastTable<- reactive({
        f<-stateForecast()
        a<-t(f$E.y)
        tab<- data.frame(a)
        colnames(tab)<-input$paramsMARSS
        tab
      })
      
      
      #' output$stateForecast
      #' @description a reactive rendering function, outputs the forecast table for the SPace State models
      #'
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
      
      
      #' dlmPlot
      #' @description A reactive function, constructs the data plot with smoothed and filtered lines for dlm models
      #'
      #' @return a dlm plot
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
      
      
      #' structuralPlot
      #' @description A reactive function, constructs the data plot with smoothed and filtered lines for Structural models
      #'
      structuralPlot<- reactive({
        data<-state()
        data1<- data()
        data1<-data1[, input$paramsState]
        plot(data1, type="o", pch=19, bg="black")
        lines(fitted(data)[,1],lty = "dashed", lwd = 2, col="red")
        lines(tsSmooth(data)[,1],lty = "dotted", lwd = 2, col="blue")
        legend("bottomright", legend = c("Data", "Filtered","Smoothed"), lty = c(1,2,3), pch=c(19, NA, NA), col=c("black", "red", "blue"), lwd=c(1,2,2))
        
      })
      
      
      #' output$MARSSplotData
      #' @description A reactive rendering function, constructs data plots with filtered and smoothed lines for 
      #' MARSS models, a plot per parameters chosen
      #'
      #' @return A list of plots for MARSS
      #' 
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
          #Needed local for proper work of the ui
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
      
      
      #' output$stateFittedPlot
      #' @description A reactive rendering function, switches between data and fit plot for Space state models
      #' according to the user input, ignores MARSS construction
      #'
      output$stateFittedPlot<- renderPlot({
        if (input$StateModel=="dlm"){
          dlmPlot()
         } else if (input$StateModel=="Structural"){
           structuralPlot()
         }})
      
      
     
      #' stateForecastPlotDataHandler
      #' @description A reactive function, creating a data.frame in the form required by ggplot in dlmForecastPlot()
      #'
      #' @return a data.frame in the required format
      #' 
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
      

      #' dlmForecastPlot
      #' @description A reactive function, reconstructs ggplot functionality like in arime.plot() for Space State
      #'
      dlmForecastPlot<- reactive({
        f<- stateForecastPlotDataHandler()
        g<- ggplot(f, aes(x=x, y=data)) + geom_line() + geom_line(aes(x=x, y=mean), colour = "blue")
        g<- g + geom_ribbon(aes(x=x, ymin=min80, ymax=max80),  fill = "lightsteelblue4",  alpha=0.3)
        g<- g + geom_ribbon(aes(x=x, ymin=min95, ymax=max95),  fill = "grey50",  alpha=0.3)
        g
      })
      
      
      #' marssForecastPlot
      #' @description A reactive function, constructs the forecast plot for MARSS models
      #'
      marssForecastPlot<-reactive({
        data<- MARSSHandler()[1,]
        f<-stateForecast()
        a<-data.frame(f$sim.data)
        forecast<-t(a)
        plot(forecast[,1], type = 'o', lwd = 2, pch = 16, ylab = "Data")
        lines(data,type = 'o')
        #abline(v = length(data)+0.5, lty = "dashed")
      })
      
      
      #' output$MARSSForecastPlot
      #' @description A reactive rendering function, constructs forecasting plots for each of the parameters for MARSS model
      #'
      #' @return List of forecast plots
      #'
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
      
      
#' MARSSForecastPlotDataHandler
#' @description A plot data handler for MARSS models
#'
#' @param d - original data 
#' @param forecast - the forecasted data
#'
#' @return a data frame in the format neede for ggplot
#' 
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
      
      
      #' output$stateForecastPlot
      #' @description A reactive rendering function, switches the Space State Forecast plot according to the chosen model type
      #'
      #' @return A Space State forecast plot in the UI
      #' 
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


