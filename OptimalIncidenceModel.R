tryCatch(library("GenSA"), error = function(e){install.packages("GenSA"); library("GenSA")}, finally=paste(":( :( :( :( Error when installing", "GenSA"))
#loadPackages(list("rmutil", "fGarch", "moments","MASS"))

getModel = function(modelParameters, series){
  model = NULL; 
  training.series.size = modelParameters$training.series.size
  totalIncidencePrediction = modelParameters$totalIncidencePrediction
  days = series$normalizedDays[1:training.series.size]
  training.series.totalCases = modelParameters$training.series.totalCases
    shape1 = modelParameters$shape1
    shape2 = modelParameters$shape2
    ncp = modelParameters$ncp 
    model$PDFs = dbeta(x=c(days), shape1 = shape1, shape2 = shape2, ncp = ncp)
    model$PDF = function(x){
      dbeta(x, shape1 = shape1, shape2 = shape2, ncp = ncp)
    }
  probabilities = model$PDFs[1:(training.series.size)]*(days[2]-days[1])# in the day- model$PDFs[1:training.series.size]
  expectations = round(probabilities*totalIncidencePrediction); 
  model$expectations = expectations
  #plot(expectations, series$target);plot(expectations); plot(series$target);  
  target = series$target[1:training.series.size]
  model$RMSE = sqrt(mean((expectations-target)^2, na.rm = TRUE))
  if(!is.finite(model$RMSE)){
    model$RMSE = Inf
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
    modelParameters  = list()
    i=1
    modelParameters$shape1 =   (parameters[i]); i = i+1
    modelParameters$shape2 =   (parameters[i]); i = i+1
    modelParameters$ncp =   (parameters[i]); i = i+1
    modelParameters$totalIncidencePrediction =   round(parameters[i]); i = i+1
    modelParameters$training.series.totalCases = training.series.totalCases
    modelParameters$training.series.size = training.series.size
    modelParameters$forecastingTimeHorizon = forecastingTimeHorizon
    model = getModel(modelParameters, series)
    PerformanceMetric = model$RMSE
    if(PerformanceMetric < optimalModel$PerformanceMetric){
      optimalModel$PerformanceMetric <<- PerformanceMetric
      optimalModel$model <<- model
      optimalModel$modelParameters <<- modelParameters
    }
    return(PerformanceMetric)
  }
  set.seed(0); #definir semente 
  n = length(series) #n ? o tamanho da s?rie
  min_shape1 = 1; min_shape2 = 1; min_ncp = 0
  min_N = training.series.totalCases
  max_shape1 = 100; max_shape2 = 100; max_ncp = 100
  max_N = 1e+7
  lowers <- c(min_shape1, min_shape2, min_ncp, min_N)
  uppers <- c(max_shape1, max_shape2, max_ncp, max_N)
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
    )

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
