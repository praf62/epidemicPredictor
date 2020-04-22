tryCatch(library("GenSA"), error = function(e){install.packages("GenSA"); library("GenSA")}, finally=paste(":( :( :( :( Error when installing", "GenSA"))
loadPackages(list("rmutil", "fGarch", "moments","MASS"))

DistributionsLabels = c("lnorm", "weibull", "snorm", "beta")
#TypeLabels = c("eps-regression", "C-classification", "nu-classification", "nu-regression")
#criar matriz para alimentar o algoritmo model
getModel = function(modelParameters, series){
  model = NULL; 
  training.series.size = modelParameters$training.series.size
  totalIncidencePrediction = modelParameters$totalIncidencePrediction
  days = series$normalizedDays[1:training.series.size]
  training.series.totalCases = modelParameters$training.series.totalCases
  # if(modelParameters$Distribution=="lnorm"){
  #   meanlog = modelParameters$mean
  #   sdlog = modelParameters$sd
  #   PDFs = dlnorm(x=days, meanlog = meanlog, sdlog = sdlog)
  #   model$PDFs = c(PDFs)
  #   model$PDF = function(x){
  #     dlnorm(x, meanlog = meanlog, sdlog = sdlog)
  #   }
  # }
  # else if(modelParameters$Distribution=="weibull"){
  #   shape = modelParameters$shape
  #   scale  = modelParameters$scale 
  #   PDFs = dweibull(x=days, shape = shape, scale  = scale)
  #   model$PDFs = c(PDFs)
  #   model$PDF = function(x){
  #     dweibull(x, shape = shape, scale  = scale)
  #   }
  # }
  # else if(modelParameters$Distribution=="snorm"){
  #   mean = modelParameters$mean
  #   sd = modelParameters$sd
  #   xi = modelParameters$skewness 
  #   model$PDFs = dsnorm(x=c(days), mean = mean, sd = sd, xi = xi)
  #   model$PDF = function(x){
  #     dsnorm(x, mean = mean, sd = sd, xi = xi)
  #   }
  # }
  # else if(modelParameters$Distribution=="beta"){
    shape1 = modelParameters$shape1
    shape2 = modelParameters$shape2
    ncp = modelParameters$ncp 
    model$PDFs = dbeta(x=c(days), shape1 = shape1, shape2 = shape2, ncp = ncp)
    model$PDF = function(x){
      dbeta(x, shape1 = shape1, shape2 = shape2, ncp = ncp)
    }
  # }
  probabilities = model$PDFs[1:(training.series.size)]*(days[2]-days[1])# in the day- model$PDFs[1:training.series.size]
  expectations = round(probabilities*totalIncidencePrediction); 
  model$expectations = expectations
  #plot(expectations, series$target);plot(expectations); plot(series$target);  
  target = series$target[1:training.series.size]
  model$RMSE = sqrt(mean((expectations-target)^2, na.rm = TRUE))
  if(!is.finite(model$RMSE)){
    model$RMSE = Inf
  }
  notZeroindexes = which(target>0)
  nzTarget = target[notZeroindexes]
  nzExpectations = expectations[notZeroindexes]
  diff = (nzExpectations-nzTarget)
  model$MAE = mean(abs(diff), na.rm = TRUE)
  if(!is.finite(model$MAE)){
    model$MAE = Inf
  }
  model$MAPE = mean(abs(diff/nzTarget), na.rm = TRUE)
  if(!is.finite(model$MAPE)){
    model$MAPE = Inf
  }
  model$ChiSquared = sum(diff^2/nzTarget, na.rm = TRUE)
  if(!is.finite(model$ChiSquared)){
    model$ChiSquared = Inf
  }
  return (model)
}
getOptimalModel = function(data = dataset_i, dataName = dataNm_i, training.series.size=n){#training.series.totalCases, forecastingTimeHorizon = forecastingTimeHorizon){
  print(paste("***", dataName, ": Model ***"))
  pGSA <<- tuningParameters[[dataName]]$GSA
  forecastingTimeHorizon = tuningParameters[[dataName]]$forecastingTimeHorizon
  modellingTime = Sys.time()
  calls <<- 0; optimalModel <<- list()
  optimalModel$PerformanceMetric = Inf
  optimalModel$model = NULL
  optimalModel$parameters = NULL
  target.all = data$target
  target.train = target.all[1:training.series.size]
  training.series.totalCases = sum(target.train, na.rm = TRUE)
  
  series = NULL
  series$target = data$target
  series$normalizedDays = getNormalizedSeries(1:forecastingTimeHorizon, 1, forecastingTimeHorizon)

  fitness=function(parameters){# = c(2,2,12,3,.5,2)){
    calls <<- calls+1;
    # print(paste("#calls =", calls))
    # if(calls >= 13){
    #   g=1
    # }
    modelParameters  = list()
    i=1
    # index = floor(parameters[i]); i = i+1
    # modelParameters$Distribution = DistributionsLabels[index]
    # modelParameters$meanlog =   (parameters[i]); i = i+1
    # modelParameters$sdlog =   (parameters[i]); i = i+1
    # modelParameters$mean =   (parameters[i]); i = i+1
    # modelParameters$sd =   (parameters[i]); i = i+1
    # modelParameters$skewness  =   (parameters[i]); i = i+1
    # modelParameters$shape =   (parameters[i]); i = i+1
    # modelParameters$scale =   (parameters[i]); i = i+1
    modelParameters$shape1 =   (parameters[i]); i = i+1
    modelParameters$shape2 =   (parameters[i]); i = i+1
    modelParameters$ncp =   (parameters[i]); i = i+1
    modelParameters$totalIncidencePrediction =   round(parameters[i]); i = i+1
    modelParameters$training.series.totalCases = training.series.totalCases
    modelParameters$training.series.size = training.series.size
    modelParameters$forecastingTimeHorizon = forecastingTimeHorizon
    model = getModel(modelParameters, series)
    #PerformanceMetric = getPerformanceMetric (model, modelParameters)
    PerformanceMetric = model$RMSE
    if(PerformanceMetric < optimalModel$PerformanceMetric){
      optimalModel$PerformanceMetric <<- PerformanceMetric
      optimalModel$model <<- model
      optimalModel$modelParameters <<- modelParameters
      #print(paste(dataName, "Model, #calls =", calls, "PerformanceMetric=", PerformanceMetric));
      #print(modelParameters);
    }
    return(PerformanceMetric)
  }
  trash = function(){
    days=1:600; days_norm = days/max(days)
    dbeta = dbeta(x=days_norm, shape1=10, shape2=100, ncp = 100); plot(days_norm, dbeta)
    # dlnorm = dlnorm(x=days_norm, meanlog = log(10), sdlog = log(10)); plot(days_norm, dlnorm)
    # dsnorm = dsnorm(x=c(days_norm), mean = 1, sd = 1, xi = 2); plot(days_norm, dsnorm)
    # dweibull = dweibull(x=days_norm, shape = 5, scale  = 1); plot(days_norm, dweibull)
  }
  set.seed(0); #definir semente 
  n = length(series) #n ? o tamanho da s?rie
  # min_meanlog = log(1e-1); min_sdlog = log(1.1); 
  # min_mean = 0.1; min_sd = .1;  min_skewness = .1; 
  # min_shape = 1.1; min_scale = 1e-1; 
  min_shape1 = 1; min_shape2 = 1; min_ncp = 0
  min_N = training.series.totalCases
  # max_meanlog = log(10); max_sdlog = log(10); 
  # max_mean = 1; max_sd = 1;  max_skewness = 2; 
  # max_shape = 5; max_scale = 1; 
  max_shape1 = 100; max_shape2 = 100; max_ncp = 100
  max_N = 1e+7
  #max_Distributions = (length(DistributionsLabels)+1-1e-5)
  lowers <- c(#1                 , min_meanlog, min_sdlog, min_mean, min_sd, min_skewness, min_shape, min_scale, 
              min_shape1, min_shape2, min_ncp, min_N)
  uppers <- c(#max_Distributions, max_meanlog, max_sdlog, max_mean, max_sd, max_skewness, max_shape, max_scale, 
              max_shape1, max_shape2, max_ncp, max_N)
  starts = c(min_shape1+1, min_shape2+1, min_ncp+1, 1.5*min_N)
  tol <- 1e-3
  out = 
    tryCatch({
      expr = GenSA(lower = lowers, upper = uppers, fn = fitness 
                   , par = starts
                   , control=list(max.call = pGSA$max.call, max.time=pGSA$max.time
                                  , maxit = pGSA$maxit, verbose = FALSE, smooth = FALSE
                                  , seed=-1, nb.stop.improvement = pGSA$nb.stop.improvement
                                  , temperature = pGSA$temperature))
    }, 
    error = function(e){
      message(paste("Error in GenSA.", e)); 
      return(return(NA))
    } 
    #, finally={message(paste("Error: ginv(covMatrix)")); return(NA)}
    )
  #plot(optimalModel$model$expectations, series$target);
  #plot(series$target); points(optimalModel$model$expectations, col="blue");  

  modellingTime = Sys.time() - modellingTime#time in seconds
  modellingTime = as.numeric(modellingTime) 
  print(paste(dataName, "Model, #calls =", calls, "PerformanceMetric=", optimalModel$PerformanceMetric));
  print(optimalModel$modelParameters);
  
  optimal = list()
  optimal$modellingTime = modellingTime
  optimal$forecastingTime = NA
  optimal$model = optimalModel$model
  optimal$GenSA_output = out
  optimal$modelParameters = optimalModel$modelParameters
  optimal$data = series
  return(optimal)
} 
getModelForecasts = function(model=optModel_i, data = dataset_i){
  # Model one-step forecast  (training and test set)
  #plot(series)
  forecastingTime = Sys.time()
  series = data$target
  seriesSize = length(series)

  training.series.size = model$modelParameters$training.series.size
  training.series.totalCases = model$modelParameters$training.series.totalCases
  N = model$modelParameters$totalIncidencePrediction
  forecasting.horizon = model$modelParameters$forecastingTimeHorizon#days
  days = 1:forecasting.horizon
  forecasts = model$model$expectations
  norm_days = model$data$normalizedDays
  newDays = norm_days[(training.series.size+1):forecasting.horizon]
  newPDFs = c(model$model$PDF(newDays))
  newLegth = length(newPDFs)
  probabilities = newPDFs[1:(newLegth)]*(newDays[2]-newDays[1])# - newPDFs[1:newLegth]
  expectations = round(probabilities*N)
  forecasts = c(forecasts, expectations)
  forecastingTime = Sys.time() - forecastingTime
  ret = list()
  ret$forecastingTime = as.numeric(forecastingTime)
  ret$forecasts = as.numeric(forecasts)
  return (ret)
}
