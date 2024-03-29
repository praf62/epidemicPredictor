getSourceMode <<- function(isToDebug=FALSE){
  retFunction = NULL
  if(isToDebug){
    retFunction = function(file, echo=TRUE){
      ret = debugSource(file=file, echo=echo)
      return(ret)
    }
  }
  else {
    retFunction = function(file){
      ret = source(file=file)
      return(ret)
    }
  }
  return(retFunction)
}
todayDate <<- as.character(Sys.Date())
isToDebug <<- F#TRUE
sourceMode <<- getSourceMode(isToDebug)
tuningParameters  <<- NULL
setParametersTuning = function(pTrainingPercentual = 1
                               , pForecastingTimeHorizon = 1000
                               , pGenSA = list(max.call = 3E+04
                                               , max.time=6, maxit = 5e+3
                                               , temperature = 1e+8, nb.stop.improvement = 20) 
                               , pDATA_LABELS
                               , pROOT = "./"
                                   , pEPIDEMICS_CODES_PATH = pROOT
                                   , pDATA_PATH = paste(pEPIDEMICS_CODES_PATH, "Data/", todayDate, "/", sep="")
                                   , pRESULTS_PATH = paste(pEPIDEMICS_CODES_PATH, "Results/", todayDate, "/", sep="")
                                   , pDATA_NAMES=NULL){
  options(digits = 4, scipen = -2)
  GSA <<- pGenSA
  libPaths = .libPaths(); destDir = libPaths[1]#length(libPaths)];
  #JAIR_ROOT = "C:/Users/jairp/Dropbox/UFCA/UFCA - Mestrado - PRODER/Disserta??o/Projetos/Project_Qual/ANNextra/DSIJ.xlsx"
  #PRAF_ROOT <<- "G:/Meu Drive/UFCA/Pesquisa/MESOR/Codes/EPIDEMICS/"
  ROOT <<- pROOT#exghange if necessary
  EPIDEMICS_CODES_PATH <<- pEPIDEMICS_CODES_PATH
  if(is.null(EPIDEMICS_CODES_PATH)){
    EPIDEMICS_CODES_PATH = ROOT
  }
  DATA_PATH <<- pDATA_PATH
  if(is.null(DATA_PATH)){
    DATA_PATH = paste(ROOT, "Data/", sep="")
  }
  RESULTS_PATH <<- pRESULTS_PATH
  if(is.null(RESULTS_PATH)){
    RESULTS_PATH <<- paste(ROOT, "Results/", sep="")
  }
  DATA_LABELS <<- pDATA_LABELS #c("DEF")
  DATA_NAMES <<- pDATA_NAMES
  if(is.null(DATA_NAMES)){DATA_NAMES <<-pDATA_LABELS}
  
  #FUNCTIONS DECLARATION 
  trainingPercentual <<- pTrainingPercentual;
  Optimum <<- list()#optAnnObjs, optArimaObjs, optEtsObjs, optGarchObjs, optMvObjs, optCAnnObjs, optCbObjs, optCacoullosObjs)
  tuningParameters <<- list()
  nCases = length(pDATA_LABELS)
  for(i in 1:nCases){
    dataLabel_i = pDATA_LABELS[i]
    tuningParameters[[dataLabel_i]]$trainingPercentual <<- pTrainingPercentual
    tuningParameters[[dataLabel_i]]$forecastingTimeHorizon <<- pForecastingTimeHorizon
    tuningParameters[[dataLabel_i]]$GSA <<- pGenSA
    tuningParameters[[dataLabel_i]]$ROOT <<- pROOT
    tuningParameters[[dataLabel_i]]$EPIDEMICS_CODES_PATH <<- pEPIDEMICS_CODES_PATH
    tuningParameters[[dataLabel_i]]$DATA_PATH <<- pDATA_PATH
    tuningParameters[[dataLabel_i]]$RESULTS_PATH <<- pRESULTS_PATH
    tuningParameters[[dataLabel_i]]$DATA_LABELS <<- pDATA_LABELS
    tuningParameters[[dataLabel_i]]$DATA_NAMES <<- pDATA_NAMES
  }
  
  #LOADING PACKAGES
  sourceMode(file=paste(pEPIDEMICS_CODES_PATH, "Auxiliar.R", sep=""))#, echo=TRUE)
  sourceMode(file=paste(pEPIDEMICS_CODES_PATH, "OptimalModel.R", sep=""))#, echo=TRUE)
  #sourceMode(file=paste(pROOT, "/Auxiliary/PerformanceMetrics.R", sep=""))#, echo=TRUE)
  loadPackages()#c("neuralnet","forecast", "GenSA", "nortest", "copula", "moments", "distr","fGarch", "tdata"), echo=TRUE)
}

getTS = function(URL = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv" 
                          , DATA_LABELS =
                             c("Argentina", "Brazil", "China", "Germany", "India", "Iran", "Italy", "Japan", "France"
                               , "Korea, South", "Spain","United Kingdom", "US")){
  URL_names = list(COUNTRIES = "COUNTRIES", BRAZIL = "BRAZIL")
  jhu_url = URL
  saveCountryData = function(contry_i){
    data_i = data[data$`Country/Region`==country_i,]; #View(data_i, title = country_i)
    cumulative_i = as.integer(lapply(data_i[, col_names[5:nCols]], sum))
    target_i = c(cumulative_i[1]
                 , cumulative_i[2:seriesSize] - cumulative_i[1:(seriesSize-1)])
    start_i = which(target_i>0)[1]
    target_i = target_i[start_i:(nCols-4)]
    date_i = date[start_i:(nCols-4)]
    data_to_save_i = cbind(date=as.character(date_i), target= target_i)
    #View(rbind(data_i[,5:nCols], target_i))
    #View(data_to_save_i)
    write.table(x = data_to_save_i, file = paste(DATA_PATH, "/covid_19_dailyConfirmedCases_", country_i, ".csv", sep=""), dec=".", sep=";", row.names = FALSE);#, header = TRUE); 
  }
  getDataFromUrl = function(str_URL){
    # my_url = url(str_URL)#, encoding = "UTF-8")
    # data <- read.csv (my_url, sep=",",fileEncoding = "UTF-8",  encoding = "UTF-8", stringsAsFactors=FALSE) 
    # mySink("Start getDataFromUrl OK!")
    data <- as.data.frame(fread(str_URL, sep=",",  encoding = "UTF-8", stringsAsFactors=FALSE))#;View(data))#;View(data)
    if(ncol(data)<2){
      # close(my_url)
      data <- as.data.frame(fread(str_URL, sep=";",  encoding = "UTF-8", stringsAsFactors=FALSE))#;View(data))#;View(data)
    }
    #View(data)
    # mySink("getDataFromUrl OK!")
    return(data)
  }
  downloadAndSaveData = function(){
    str_URLs = list()
    str_URLs[[URL_names$COUNTRIES]] = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
    str_URLs[[URL_names$BRAZIL]] = "https://data.brasil.io/dataset/covid19/caso.csv.gz"
    str_URLs[[paste(URL_names$COUNTRIES, "DEATHS", sep="_")]] = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
    str_URLs[[paste(URL_names$BRAZIL, "DEATHS", sep="_")]] = "https://data.brasil.io/dataset/covid19/obito_cartorio.csv.gz"
    nmsURLs = names(str_URLs)
    nURLs = length(nmsURLs)
    EPIDEMICS_DATA_PATH = paste(MyROOT, "Epidemics/Data/", sep="")
    cl = getParallelComputingConfiguration(coresProp = 0.8)
    foreach(i = 1:nURLs
            , .export = c('getDataFromUrl', 'URL_names')
            , .packages = c("data.table"))%dopar%{#i=2
              nm_i = nmsURLs[i]
              data_i <- getDataFromUrl(str_URL = str_URLs[[i]])#View(data_i, nmsURLs[i])   
              if(nm_i==URL_names$BRAZIL){
                data_i = data_i[, c('date', 'state', 'city', 'place_type', 'confirmed')]
                # dates = as.character(data_i$date)
                # data_i = data[data$state==state_i & data$place_type=="state",]; #View(data_i, title = state_i)
                # dates = as.character(data_i$date)
                setorder(x = data_i, state, city, date); 
                View(data_i, title = paste(":", nm_i))
              }
              path_i = paste(EPIDEMICS_DATA_PATH, nm_i, ".csv", sep="")
              fwrite(x = data_i, file = path_i, append = FALSE)
            }
    stopCluster(cl$cluster)#Stopping your cluster
    print(paste(Sys.time(), ":", paste(URL_names, collapse = " and "), "OK!"))
  }
  ##verifying if the case has been studied today...
  nCountries = length(DATA_LABELS)
  #today_DATA_PATH = paste(DATA_PATH, todayDate, sep="")
  folder = paste(DATA_PATH)
  folderExists = file.exists(folder)
  data = NULL; col_names = NULL; dates = NULL; seriesSize = NULL; nCols = NULL
  if(folderExists==FALSE){
    dir.create(folder)
    dir.create(RESULTS_PATH)
    loadPackages("data.table")
    # url = getURL(jhu_url)
    data <- getDataFromUrl(jhu_url)#read.csv (text = url)
    #Country.Region_s = names(table(data$Country.Region)); print(cbind(Country.Region_s))
    col_names = names(data); nCols = length(col_names)
    dates = col_names[5:nCols]
    seriesSize = length(dates)
    # dates = paste(dates, collapse =  "")
    # loadPackages("stringr")
    # dates = str_split(string = dates, pattern = "X")
    # dates = dates[[1]][-1]
    date = as.Date(dates, "%m.%d.%y")
    if(length(which(is.na(date)))==seriesSize){
      date = as.Date(dates, "%m/%d/%y")
    }
    for(i in 1:nCountries){#i=1;i=3
      country_i = DATA_LABELS[i]
      saveCountryData(country_i)
    }
  }
  else{
    for(i in 1:nCountries){#i=1;i=3
      country_i = DATA_LABELS[i]
      fileName = paste(DATA_PATH, "/covid_19_dailyConfirmedCases_", country_i, ".csv", sep="")
      fileExists = file.exists(fileName)
      if(fileExists == FALSE){
        if(is.null(data)){
          loadPackages("RCurl")
          url = getURL(jhu_url)
          data <- read.csv (text = url)
          col_names = names(data); nCols = length(col_names)
          dates = col_names[5:nCols]
          seriesSize = length(dates)
          dates = paste(dates, collapse =  "")
          loadPackages("stringr")
          dates = str_split(string = dates, pattern = "X")
          dates = dates[[1]][-1]
          date = as.Date(dates, "%m.%d.%y")
        }
        saveCountryData(country_i)
      }
    }
  }
}
fitModel = function(pDATA_LABELS = DATA_LABELS, ns=rep(NA, length(pDATA_LABELS))) {
  DATA_LABELS = pDATA_LABELS
  dataNm_i <<- NULL
  nCases = length(DATA_LABELS)#; i=1
  #alldataset <- read.csv(paste(DATA_PATH, "time-series-19-covid-combined.csv", sep=""), dec=".", sep=";", header = TRUE); 
  #View(alldataset, title = "covid")
  #sort(names(table(alldataset$Country.Region)))
  for (i in 1:nCases){#i=1
    Optimum_i = list()
    dataNm_i <<- DATA_LABELS[i]
    dataNm_i_2 = dataNm_i
    print(paste(i, "> ==============", dataNm_i, "=============="))
    dataset_i <- read.csv(paste(DATA_PATH, "covid_19_dailyConfirmedCases_", dataNm_i, ".csv", sep=""), dec=".", sep=";", header = TRUE); 
    #dataset_i = alldataset[alldataset$Country.Region==dataNm_i,]; 
    #View(dataset_i, title = dataNm_i)
    n_m = nrow(dataset_i)
    p_n = ns[i]
    if(is.na(p_n)){
      n =  round(tuningParameters[[dataNm_i]]$trainingPercentual*n_m)#traininc size
      #n =  min((n_m-1), n)#traininc size
    }else{
      n = p_n
      dataNm_i_2 = paste(dataNm_i, "_n_", n, sep="")
      tuningParameters[[dataNm_i_2]]$trainingPercentual <<- n/n_m
    }
    forecastingTimeHorizon= tuningParameters[[dataNm_i_2]]$forecastingTimeHorizon
    Optimum_i$trainingSize = n
    m = n_m - n #test size
    
    target.all = dataset_i$target

    optNCB_i = getOptimalModel(data = dataset_i, dataName = dataNm_i_2, training.series.size = n)
    forecasts = getModelForecasts(optNCB_i, data = dataset_i)
    optNCB_i$model$RMSE.Test = forecasts$RMSE.Test
    optNCB_i$model$MAE.Test = forecasts$MAE.Test
    optNCB_i$forecastingTime = forecasts$forecastingTime
    forecasts = forecasts$forecasts
    
    forecastingSeriesLength = length(forecasts)
    diffs = forecastingSeriesLength - n_m
    target = target.all
    date = as.character(dataset_i$date)
    if(diffs<0){
      forecasts = c(forecasts, rep(NA, diffs))
    } 
    else if(diffs > 0){
      target = c(target, rep(NA, diffs))
      inc = 1:diffs
      start_date = as.Date(as.character(date[n_m], "%y-%m-%d"))
      new_dates = start_date+inc
      date = c(date, as.character(new_dates))
    }
    
    optNCB_i$modelParameters$start.incidence.date = as.character(date[1])
    modeIndexes = which(forecasts==max(forecasts, na.rm=TRUE))
    optNCB_i$modelParameters$mode.incidence.date = paste(as.character(date[modeIndexes]), collapse=", ")
    vanishe.incidence.date.indexes = which(forecasts==0)
    end.incidence.date.index = vanishe.incidence.date.indexes[vanishe.incidence.date.indexes>modeIndexes][1]
    aux.index = end.incidence.date.index
    if(is.na(end.incidence.date.index)){
      aux.index = forecastingTimeHorizon
      optNCB_i$modelParameters$end.incidence.date = as.character(paste(">", date[aux.index]))
    } 
    else {
      optNCB_i$modelParameters$end.incidence.date = as.character(date[end.incidence.date.index])
    }
    
    Optimum_i$optNCB = optNCB_i
    #single.forecasts = cbind(single.forecasts, forecasts)#ANN, ARIMA, ETS)

    all.data = cbind(target=target[1:aux.index], forecasts=forecasts[1:aux.index])#View(all.data)
    nCombinators_i = 0

    Optimum_i$nCombinators = nCombinators_i
    
    aux = NULL
    aux[["date"]] = date[1:aux.index]
    aux = as.data.frame(aux, stringsAsFactors =FALSE)
    all.data = as.data.frame(cbind(aux, all.data), stringsAsFactors =FALSE)
    #all.data = as.data.frame(all.data)#; View(all.data)
    Optimum_i$all.data = all.data
    Optimum[[dataNm_i_2]] <<- Optimum_i
    #trash=1
    #View(all.data.norm)
    #sink()
  }
  saveObjects()
}
computeModelsResults = function(DATA_LABELS, isToExportForecastsTable=FALSE
                                , isToComputePlots=TRUE, isToComputeStructures=TRUE, isToComputePerformances=FALSE){#){
  dataNames = DATA_LABELS
  nCases = length(dataNames)
  all.data.list = list()
  Optimum <<- list()
  tuningParameters <<- list()
  BACKUP_RESULTS_PATH <<- RESULTS_PATH
  BACKUP_tuningParameters = list()
  nDec = 2
  for(i in 1:nCases){
    dataNm_i <<- dataNames[i]
    RESULTS_PATH <<- paste(BACKUP_RESULTS_PATH, dataNm_i, "/", sep="")
    file = paste(RESULTS_PATH, dataNm_i, "_tuningParameters.rds", sep="")
    if(file.exists(file)){
      BACKUP_tuningParameters[[dataNm_i]] = readRDS(file = file)[[dataNm_i]]
    }
    ObjModels_i = readRDS(file = paste(RESULTS_PATH, dataNm_i, ".rds", sep=""))
    Optimum[[dataNm_i]] <<- ObjModels_i
    all.data.list[[dataNm_i]] = ObjModels_i$all.data#[,-c(2)] #View(all.data.list[[dataNm_i]], title=dataNm_i)
    if(isToExportForecastsTable){
      write.table(x = all.data.list[[dataNm_i]]
                  , file = paste(RESULTS_PATH, dataNm_i, "_data.csv", sep="")
                  , sep="\t")#, row.names = TRUE)
    }
  }
  computePlots = function(){
    for (i in 1:nCases){#i=1
      dataNm_i = dataNames[i]
      RESULTS_PATH <<- paste(BACKUP_RESULTS_PATH, dataNm_i, "/", sep="")
      tuningParameters <<-  BACKUP_tuningParameters[[dataNm_i]]
      #isToRun <<- tuningParameters$isToRun
      trainingPercentual <<-tuningParameters$trainingPercentual
      print(paste(i, "> ==============", dataNm_i, "Plot Results =============="))
      all.data <- all.data.list[[dataNm_i]]; #View(all.data, title = dataNm_i)
      all.data$target = round(all.data$target)
      n_m = nrow(all.data)
      n = Optimum[[dataNm_i]]$trainingSize#round(trainingPercentual*n_m)#traininc size
      #v = round(.25*n_m) # validation size
      m = n_m - (n ) #test size
      Optimum_i = Optimum[[dataNm_i]]
      nCombinators_i = Optimum_i$nCombinators
      #FIGURES
      # studyCorrespondencesAndResidualsAllModels( series = all.data[,-c(1)], from = (1), to=n
      #                                           , phaseLabel = "Training", seriesName = dataNm_i
      #                                           , nCombinators = nCombinators_i)
      # studyCorrespondencesAndResidualsAllModels(series = all.data[,-c(1)], from = (n+1), to=n_m
      #                                           , phaseLabel = "Test", seriesName = dataNm_i
      #                                           , nCombinators = nCombinators_i)
      N = round(n/trainingPercentual)
      generateTimeSeriesGraphic(all.data[1:N,-c(3)], n, v=0, m = (N-n)
                                , seriesName =paste("_", dataNm_i, sep=""), nCombinators = nCombinators_i, ylab= "daily incidence")
      generateTimeSeriesGraphic(all.data, n, v=0, m, seriesName =dataNm_i
                                , nCombinators = nCombinators_i, ylab= "daily incidence")
      write.table(x = all.data, file = paste(BACKUP_RESULTS_PATH, dataNm_i,"/",dataNm_i ,"_timeSeries.csv", sep="")
                  , dec=".", sep="\t", row.names = FALSE);#, header = TRUE);
    }
  }
  computePerformanceTables = function(){
    sourceMode(file=paste(ROOT, "/Forecasting/PerformanceMetrics.R", sep=""))#, echo=TRUE)
    srtCases = paste(dataNames, collapse = "_")
    print(paste("> ==============", "Performance Results =============="))
    sink(file = paste(BACKUP_RESULTS_PATH, srtCases, "_PerformanceMetrics_latexTable.txt", sep="")
         , append = FALSE, type = "output",split = FALSE)
    agregateTb.train = NULL
    agregateTb.test = NULL
    for (i in 1:nCases){#i=1
      dataNm_i <<- dataNames[i]
      RESULTS_PATH <<- paste(BACKUP_RESULTS_PATH, dataNm_i, "/", sep="")
      tuningParameters <<-  BACKUP_tuningParameters[[dataNm_i]]
      #isToRun <<- tuningParameters$isToRun
      trainingPercentual <<- tuningParameters$trainingPercentual
      print(paste(i, "> ==============", dataNm_i, "Performance Results =============="))
      all.data <- all.data.list[[dataNm_i]]; #View(all.data, title = dataNm_i)
      all.data = all.data[,-c(1)]; #View(all.data, title = dataNm_i)
      n_m = nrow(all.data)
      n = Optimum[[dataNm_i]]$trainingSize#round(trainingPercentual*n_m)#traininc size
      #v = round(.25*n_m) # validation size
      m = n_m - (n ) #test size
      #PERFORMANCE TABLES
      modelsNms = paste(names((all.data)), collapse = ", ")
      modelsNms = paste("(", modelsNms, ")", sep="")
      phaseLabel = "Training"
      tb = computePerformanceMetrics(all.data, from = 1, to=n, phaseLabel = phaseLabel
                                     , seriesName =  dataNm_i, withNormalization = TRUE)
      n.MeanIndex = which(tb[["Metric"]]=="n.Mean")
      agregateTb.train = rbind(agregateTb.train, tb[n.MeanIndex, ])
      align = c(rep("l|", ncol(tb)), "l")
      latex_tb = xtable(tb, caption=
                          paste("Performance of the EPIDEMICS models ", modelsNms, " when predicting "
                                , dataNm_i, " time data (", phaseLabel, " phase).", sep="")
                        , label=paste("tab:", "performanceMetrics_", phaseLabel, "_", dataNm_i, sep="")
                        , align = align, digits = 3)
      print(latex_tb, type = "latex", include.rownames = FALSE)
      
      phaseLabel = "Test"
      tb = computePerformanceMetrics(all.data, from = (n+1), to=n_m, phaseLabel = phaseLabel
                                     , seriesName = dataNm_i, withNormalization = TRUE)
      agregateTb.test = rbind(agregateTb.test, tb[n.MeanIndex, ])
      align = c(rep("l|", ncol(tb)), "l")
      latex_tb = xtable(tb, caption=
                          paste("Performance of the EPIDEMICS models ", modelsNms, " when predicting"
                                , dataNm_i, " time data (", phaseLabel, " phase).", sep="")
                        , label=paste("tab:", "performanceMetrics_", phaseLabel, "_", dataNm_i, sep="")
                        , align = align, digits = 3)
      print(latex_tb, type = "latex", include.rownames = FALSE)
    }
    dataNms_ = paste(dataNames, collapse = "_")
    dataNmsComma = paste(dataNames, collapse =  ", ")
    computeAggregatePerformanceTable = function(tb, phaseLabel){
      tb = tb[-c(1)]
      tb = as.data.frame(cbind(data=dataNames, tb))
      colNames = colnames(tb)
      nCols = length(colNames)
      tb[,4:nCols]=round(tb[,4:nCols], nDec)
      summaryRow = NULL
      for(i in 4:nCols){
        colName = colNames[i]
        mean_i = mean(tb[[colName]], na.rm=TRUE)
        summaryRow[[colName]]= mean_i
      }
      summaryRow = as.data.frame(t(summaryRow))
      rankRow = rank(summaryRow, ties.method = "min")
      summaryRow[1,] = paste(summaryRow, " (", rankRow, ")", sep="")
      modelsNames = colnames(summaryRow)
      Worst = modelsNames[which(rankRow==max(rankRow, na.rm=TRUE))]
      Worst = paste(Worst, collapse = ", ")
      Best = modelsNames[which(rankRow==1)]
      Best = paste(Best, collapse = ", ")
      summaryRow$Worst = Worst
      summaryRow$Best = Best
      # for(i in 2:3){
      #   colName = colNames[i]
      #   freq = table(tb[[colName]])
      #   mode = names(freq[which(freq==max(freq, na.rm=TRUE))])
      #   mode = paste(mode, sep=", ")
      #   summaryRow[[colName]]= mode
      # }
      
      summaryRow[["data"]]="aggregate"
      tb = rbind(tb, aggregate = summaryRow)
      View(tb, title=paste("aggregate", phaseLabel, sep="_"))
      resultName = paste(dataNms_, "AggregateNormPerformance", phaseLabel, sep="_")
      write.table(x = tb, file = paste(RESULTS_PATH, resultName, ".csv", sep="")
                  , sep="\t")#, row.names = TRUE)
      align = c(rep("l|", ncol(tb)), "l")
      latex_tb = xtable(tb, caption=
                          paste("Aggregate mean normalised performance of the EPIDEMICS models ", modelsNms, " when predicting "
                                , dataNmsComma, " time data (", phaseLabel, " phase). The rank of each model is in parentheses, in the last line. ", sep="")
                        , label=paste("tab:", "aggregateNormalisedPerformanceMetrics_", dataNms_, "_", dataNm_i, sep="")
                        , align = align, digits = 3)
      print(latex_tb, type = "latex", include.rownames = FALSE)
    }
    computeAggregatePerformanceTable(tb = agregateTb.train, phaseLabel="Training")
    computeAggregatePerformanceTable(tb = agregateTb.test, phaseLabel="Test")
    sink()
  }
  computeModelsStructure = function(){
    library(xtable)
    # labels = c(#"Distribution", "meanlog", "sdlog", "mean", "sd", "skewness", "shape", "scale", 
    #              "shape1", "shape2", "ncp"
    #            , "totalIncidencePrediction", "training.series.totalCases", "training.series.size", "forecastingTimeHorizon"
    #            , "start.incidence.date", "mode.incidence.date", "end.incidence.date"
    #            , "RMSE", "modelling.time", "forecastingTime"
    # )
    labels = c(#"Distribution", "meanlog", "sdlog", "mean", "sd", "skewness", "shape", "scale", 
      "n", "Cum.n", "date.0" 
      , "TIP", "nNCBs", "weight", "alpha", "beta", "lambda"
      , "date.m", "date.end"
      , "RMSE.Training"
      , "RMSE.Test"
      , "MAE.Training"
      , "MAE.Test"
      , "modellingTime"
      , "forecastingTime"
    )
    tb = cbind(labels)
    #tuningParameters_labels = names(BACKUP_tuningParameters[[1]])
    tuningParameters_tb = NULL
    for (i in 1:nCases){#i=1
      dataNm_i <<- dataNames[i]
      print(paste(i, "> ==============", dataNm_i, "Models Structure =============="))
      RESULTS_PATH <<- paste(BACKUP_RESULTS_PATH, dataNm_i, "/", sep="")
      tuningParameters <<-  BACKUP_tuningParameters[[dataNm_i]]
      #TUNING PARAMETERS LATEX TABLE
      aux = as.data.frame(tuningParameters)
      aux = aux[which(aux$DATA_LABELS==dataNm_i), ]
      if(is.null(tuningParameters_tb)){
        tuningParameters_tb = as.data.frame(cbind(names(aux), t(aux)), row.names = NULL)#;View(tuningParameters_tb)
      }
      else{
        tuningParameters_tb = cbind(tuningParameters_tb, t(aux))#;View(tuningParameters_tb)
      }
      name="tuningParameter"
      sink(file = paste(RESULTS_PATH, dataNm_i, "_modelsStructure_latexTable.txt", sep="")
           , append = FALSE, type = "output",split = FALSE)
      align = paste(c(rep("l|", (ncol(aux))), "l"), sep="")
      aux_tb = xtable(aux
                      , caption=
                        paste("Tuning parameters of the COVID-19 daily incidence models of ", dataNm_i, ".", sep="")
                      , label=paste("tab:", dataNm_i, "_", name, "_tb", sep="")
                      , align = align, digits = 3)
      print(aux_tb, type = "latex", include.rownames = FALSE)
      #sink()
      trainingPercentual <<- tuningParameters$trainingPercentual
      Optimum_i = Optimum[[dataNm_i]]
      #MODELS STRUCUTRE
      opt_i = Optimum_i$optNCB
      mp = list()
      mp[["n"]] =round(opt_i$modelParameters[["training.series.size"]], 0)
      mp[["Cum.n"]]=round(opt_i$modelParameters[["training.series.totalCases"]], 0)
      mp[["date.0"]]=opt_i$modelParameters[["start.incidence.date"]]
      mp[["TIP"]]=round(opt_i$modelParameters[["totalIncidencePrediction"]], 0)
      mp[["nNCBs"]]=opt_i$modelParameters[["nNCBs"]]
      mp[["weights"]]=paste(round(opt_i$modelParameters[["weights"]], nDec), collapse = ", ")
      mp[["alpha"]]=paste(round(opt_i$modelParameters[["shape1s"]], nDec), collapse = ", ")
      mp[["beta"]]=paste(round(opt_i$modelParameters[["shape2s"]], nDec), collapse = ", ")
      mp[["lambda"]]=paste(round(opt_i$modelParameters[["ncps"]], nDec), collapse = ", ")      
      mp[["date.m"]]=opt_i$modelParameters[["mode.incidence.date"]]
      mp[["date.end"]]=opt_i$modelParameters[["end.incidence.date"]]
      mp[["RMSE.Training"]] = round(opt_i$model$RMSE.Training, nDec)
      mp[["RMSE.Test"]] = ifelse(is.na(opt_i$model$RMSE.Test), NA, round(opt_i$model$RMSE.Test, nDec))
      mp[["MAE.Training"]] = round(opt_i$model$MAE.Training, nDec)
      mp[["MAE.Test"]] = ifelse(is.na(opt_i$model$MAE.Test), NA, round(opt_i$model$MAE.Test, nDec))
      mp[["modellingTime"]] = round(opt_i$modellingTime, nDec)
      mp[["forecastingTime"]] = round(opt_i$forecastingTime, nDec)
      tb = cbind(tb, unlist(mp, use.names = FALSE))
        
        #SINGLE MODEL LATEX TABLE
        aux = rbind(characteristic = names(mp), value = unlist(mp, use.names = FALSE))
        aux = cbind(c("characteristic", "value"), aux)
        modelName="model"
        align = paste(c(rep("l|", (ncol(aux))), "l"), sep="")
        aux_tb = xtable(aux
                        , caption=
                          paste("Architecture of the near-optimal ", modelName 
                                , " fitted to COVID-19 daily incidence of ", dataNm_i, " via \texttt{GenSA} package of \texttt{R}."
                                , " The modelling and forecasting times are given in seconds.", sep="")
                        , label=paste("tab:", dataNm_i, "_", modelName, "_tb", sep="")
                        , align = align, digits = 3)
        print(aux_tb, type = "latex", include.rownames = FALSE)
        #sink()
      sink()
    }
    #PRINTING AGGREGATE LaTex TABLE
    srtCases = paste(dataNames, collapse = "_")
    align = c(rep("l|", (nCases+1)), "l")
    sink(file = paste(BACKUP_RESULTS_PATH, srtCases, "_ModelsStructure_latexTables.txt", sep="")
         , append = FALSE, type = "output",split = FALSE)
    srtCases = paste(dataNames, collapse = ", ")
    srtCases = paste("(", srtCases, ")", sep="")
    
    colnames(tuningParameters_tb) <- c("characteristic", dataNames); 
    latex_tuningParameters_tb = xtable(tuningParameters_tb
                                       , caption=
                                         paste("Tuning parameters of the COVID-19 daily incidence models for each country taken into account "
                                               , srtCases, ".", sep="")
                                       , label="tab:tuningParameters_tb"
                                       , align = align, digits = 3)
    print(latex_tuningParameters_tb, type = "latex", include.rownames = TRUE)
    colnames(tb) <- c("characteristic", dataNames); 
    latex_tb = xtable(tb
                         , caption=
                           paste("Architecture of the GenSA-based near-optimal  
                                 model for each COVID-19 daily incidence time data taken into account ", srtCases, ".", " The modelling and forecasting times are given in seconds.", sep="")
                         , label="tab:architecture_performance_tb"
                         , align = align, digits = 3)
    print(latex_tb, type = "latex", include.rownames = FALSE)
    sink()
    write.table(x = tb, file = paste(BACKUP_RESULTS_PATH, srtCases, "_ModelsStructure.csv", sep="")
                , dec=".", sep=";", row.names = FALSE);#, header = TRUE); 
                
  }
  if(isToComputePlots) computePlots()
  if(isToComputeStructures) computeModelsStructure()
  if(isToComputePerformances) computePerformanceTables()
  RESULTS_PATH <<- BACKUP_RESULTS_PATH
  tuningParameters <<- BACKUP_tuningParameters
}

saveObjects = function(){
  nObjects = length(Optimum)
  nmObjects = names(Optimum)
  for (j in 1:nObjects){#j=1
    Obj_j = Optimum[[j]]
    nameObj_j = nmObjects[j]
    path = paste(RESULTS_PATH, nameObj_j, sep="")
    if(!dir.exists(path)){
      dir.create(path)
    }
    file = paste(path, "/", nameObj_j, "_tuningParameters.rds", sep="")
    saveRDS(object = tuningParameters, file = file)
    file = paste(path, "/", nameObj_j, ".rds", sep="")
    saveRDS(object = Obj_j, file = file)
  }
}
getSavedObjects = function(DATA_LABELS){
  nObjects = length(DATA_LABELS)
  Optimums = list()
  tuningParameters <<- list()
  for (j in 1:nObjects){#j=1
    nameObj_j = DATA_LABELS[j]
    path = paste(RESULTS_PATH, nameObj_j, "/", sep="")
    file = paste(path, nameObj_j, "_tuningParameters.rds", sep="")
    if(file.exists(file)){
      tuningParameters[[nameObj_j]] <<- readRDS(file = file)
    }
    file = paste(path, nameObj_j, ".rds", sep="")
    if(file.exists(file)){
      Obj_j = readRDS(file=file)
      Optimums[[nameObj_j]]=Obj_j
    }
  }
  return(Optimums)
}
removeSavedObjects = function(DATA_LABELS){
  nObjects = length(DATA_LABELS)
  for (j in 1:nObjects){#j=1
    nameObj_j = DATA_LABELS[j]
    path = paste(RESULTS_PATH, nameObj_j, "/", sep="")
    file = paste(path, nameObj_j, "_tuningParameters.rds", sep="")
    if(file.exists(file)){
      file.remove(file)
    }
    file = paste(path, nameObj_j, ".rds", sep="")
    if(file.exists(file)){
      file.remove(file)
    }
  }
}
getNewForecasts = function(DATA_LABELS){
  Optimums = getSavedObjects(DATA_LABELS)
  BACKUP_RESULTS_PATH = RESULTS_PATH
  BACKUP_tuningParameters = tuningParameters
  # isToRun <<- tuningParameters$isToRun
  nObjects = length(Optimums)
  nmObjs = names(Optimums)
  all.data.list = NULL
  for (j in 1:nObjects){#j=1
    nameObj_j = nmObjs[j]
    data = read.csv(file = paste(DATA_PATH, "NEW/", nameObj_j, ".csv", sep="")
                      , header = TRUE, sep = ";", stringsAsFactors = FALSE)#;View(data)
    RESULTS_PATH <<- paste(BACKUP_RESULTS_PATH, nameObj_j, "/", sep="")
    tuningParameters <<-  BACKUP_tuningParameters[[nameObj_j]][[nameObj_j]]
    isToRun <<- tuningParameters$isToRun
    n = nrow(data)
    m = 1
    Obj_j = Optimums[[nameObj_j]]#readRDS(file = paste(RESULTS_PATH, nameObj_j, ".rds", sep=""))
    new_forecasts = NULL
    new_forecastsData = NULL
    new_fullRelevantIndexes = NULL
    new_lagIndexes = NULL
    new_c.forecasts = NULL
    minIndex = Inf
    #SINGLE MODELS FORECASTS
    if(isToRun$ANN){
      forecasts = getAnnNewForecasts(ModelsObjs = Obj_j, data = data)
      new_forecasts$ANN = c(forecasts$forecasts)
      new_forecastsData$ANN = forecasts$forecastsData
      new_fullRelevantIndexes$ANN = forecasts$fullRelevantIndexes
      new_lagIndexes$ANN = forecasts$lagIndexes
      minRelevantIndex = min(forecasts$fullRelevantIndexes)
      if(minRelevantIndex < minIndex){
        minIndex = minRelevantIndex
      }
    }
    if(isToRun$ARIMA){
      forecasts = getArimaNewForecasts(ModelsObjs = Obj_j, data = data)
      new_forecasts$ARIMA = c(forecasts$forecasts)
      new_forecastsData$ARIMA = forecasts$forecastsData
      new_fullRelevantIndexes$ARIMA = forecasts$fullRelevantIndexes
      new_lagIndexes$ARIMA = forecasts$lagIndexes
      minRelevantIndex = min(forecasts$fullRelevantIndexes)
      if(minRelevantIndex < minIndex){
        minIndex = minRelevantIndex
      }
    }
    if(isToRun$ETS){
      forecasts = getEtsNewForecasts(ModelsObjs = Obj_j, data = data)
      new_forecasts$ETS = c(forecasts$forecasts)
    }
    if(isToRun$SVM){
      forecasts = getSvmNewForecasts(ModelsObjs = Obj_j, data = data)
      new_forecasts$SVM = c(forecasts$forecasts)
      new_forecastsData$SVM = forecasts$forecastsData
      new_fullRelevantIndexes$SVM = forecasts$fullRelevantIndexes
      new_lagIndexes$SVM = forecasts$lagIndexes
      minRelevantIndex = min(forecasts$fullRelevantIndexes)
      if(minRelevantIndex < minIndex){
        minIndex = minRelevantIndex
      }
    }
    new_forecasts = as.data.frame(new_forecasts)#;View(new_forecasts)
    nSingleModels = ncol(new_forecasts)
    nNAsRows = (n-minIndex+1)
    nNAs = rep(NA, nSingleModels*nNAsRows)
    complemMatrix = matrix(data=nNAs, ncol=nSingleModels)
    dimnames(complemMatrix)[[2]] = as.list(colnames(new_forecasts))
    new_forecasts = rbind(complemMatrix, new_forecasts)
    #COMBINATORS FORECASTS
    nCombinators_i = Obj_j$nCombinators
    if(nCombinators_i > 0){
      if(isToRun$cANN){
        forecasts = get_cAnnNewForecasts(ModelsObjs = Obj_j, singleForecasts = new_forecasts[(nNAsRows+1),])
        new_c.forecasts$cANN = c(rep(NA, nNAsRows), forecasts$forecasts)
        
      }
      if(isToRun$cCB){
        forecasts = get_cCbNewForecasts(ModelsObjs = Obj_j, singleForecasts = new_forecasts[(nNAsRows+1),])
        new_c.forecasts$cCB = c(rep(NA, nNAsRows), forecasts$forecasts)
        
      }
      if(isToRun$cMV){
        forecasts = get_cMvNewForecasts(ModelsObjs = Obj_j, singleForecasts = new_forecasts[(nNAsRows+1),])
        new_c.forecasts$cMV = c(rep(NA, nNAsRows), forecasts$forecasts)
      }
      if(isToRun$cSA){
        forecasts = get_cSaNewForecasts(ModelsObjs = Obj_j, singleForecasts = new_forecasts[(nNAsRows+1),])
        new_c.forecasts$cSA = c(rep(NA, nNAsRows), forecasts$forecasts)
      }
      if(isToRun$cSM){
        forecasts = get_cSmNewForecasts(ModelsObjs = Obj_j, singleForecasts = new_forecasts[(nNAsRows+1),])
        new_c.forecasts$cSM = c(rep(NA, nNAsRows), forecasts$forecasts)
      }
      if(isToRun$cSVM){
        forecasts = get_cSvmNewForecasts(ModelsObjs = Obj_j, singleForecasts = new_forecasts[(nNAsRows+1),])
        new_c.forecasts$cANN = c(rep(NA, nNAsRows), forecasts$forecasts)
        
      }
    }
    #GENERATING FORECASTS PLOT
    #nModels = length(new_forecasts)
    all.data = NULL
    all.data[["date"]] = c(data[["date"]][minIndex:n], "next_date")
    all.data[["target"]] = c(data[["target"]][minIndex:n], NA)
    all.data = as.data.frame(all.data)
    all.data = cbind(all.data, new_forecasts)#;View(all.data)
    if(!is.null(new_c.forecasts)){
      new_c.forecasts = as.data.frame(new_c.forecasts)
      all.data = cbind(all.data, new_c.forecasts)#;View(all.data)
    }
    library(xtable)
    sink(file = paste(RESULTS_PATH, nameObj_j, "_oneStepAheadForecast_latexTable.txt", sep="")
         , append = FALSE, type = "output",split = FALSE)
    modelsIndexes = which(isToRun==TRUE)
    modelsNames = names(isToRun)[modelsIndexes]
    modelsNames = paste(modelsNames, collapse=", ")
    align = paste(rep("l|", ncol(all.data)), sep="")
    align = paste(c("l|", align), sep="")
    latex_tb = xtable(all.data
                      , caption= paste("Previous relevant observations of ", nameObj_j
                                       , " and forecasts of its next value (via "
                                       , modelsNames, ").", sep="")
                      , label="tab:oneStepAheadForecast"
                      , align = align, digits = 3)
    print(latex_tb, type = "latex", include.rownames = FALSE)
    sink()
    generateTimeSeriesGraphic(all.data, nNAsRows, v=0, m, dataName=paste("NEXT", nameObj_j, sep="_")
                              , nCombinators = nCombinators_i)
    all.data.list[[nameObj_j]] = all.data
  }
  RESULTS_PATH <<- BACKUP_RESULTS_PATH
  tuningParameters <<- BACKUP_tuningParameters
  return(all.data.list)
}
computeAlternativeCases = function(pDATA_LABELS=DATA_LABELS, ns = c(27, 34)){
  DATA_LABELS = NULL
  CASE_DATA_LABELS = NULL
  for(i in 1:length(pDATA_LABELS)){
    casei_i = pDATA_LABELS[i]
    CASE_DATA_LABELS = c(CASE_DATA_LABELS, rep(casei_i, length(ns)+1))
    casei_i = c(paste(casei_i, "_n_", ns, sep=""), casei_i)
    DATA_LABELS = c(DATA_LABELS, casei_i) 
  }
  ns = c(ns, NA); ns = rep(ns, length(pDATA_LABELS))
  loadModellingParameters(
    pROOT = "G:/Meu Drive/UFCA/Pesquisa/MESOR/Codes/"       #PRAF
    , pEPIDEMICS_CODES_PATH = paste(ROOT, "Epidemics/", sep="")
    , pDATA_PATH = paste(EPIDEMICS_CODES_PATH, "Data/", todayDate, "/", sep="")
    , pRESULTS_PATH = paste(EPIDEMICS_CODES_PATH, "Results/", todayDate, "/", sep="")
    , pDATA_LABELS =DATA_LABELS)
  saveCovidSeries()
  performModelling(pDATA_LABELS= CASE_DATA_LABELS, ns = ns)#;c("China", "Korea, South"), p_n = n)
  computeModelsResults(DATA_LABELS = DATA_LABELS)#
}
# #FUNCTIONS USAGE
DATA_LABELS = c("US", "Argentina")#, "Brazil", "China", "Canada")
setParametersTuning(pDATA_LABELS = DATA_LABELS)
getTS(DATA_LABELS = DATA_LABELS)
fitModel(pDATA_LABELS = DATA_LABELS)
computeModelsResults(DATA_LABELS = DATA_LABELS )#
# computeAlternativeCases(pDATA_LABELS = c("China", "Korea, South"))
