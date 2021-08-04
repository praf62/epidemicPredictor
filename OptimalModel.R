tryCatch(library("GenSA"), error = function(e){install.packages("GenSA"); library("GenSA")}, finally=paste(":( :( :( :( Error when installing", "GenSA"))
#loadPackages(list("rmutil", "fGarch", "moments","MASS"))
loadPackages("doParallel")
getModel_old = function(modelParameters, series){
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
  #plot(expectations, series$target[1:(training.series.size)]);plot(expectations, col="blue"); points(series$target, type="l");  
  target = series$target[1:training.series.size]
  diff0 = expectations-target
  model$MSE.Training = mean(diff0^2, na.rm = TRUE)
  if(!is.finite(model$MSE.Training)){
    model$MSE.Training = Inf
  }
  model$RMSE.Training = sqrt(model$MSE.Training)
  model$MAE.Training = mean(abs(diff0), na.rm = TRUE)
  if(!is.finite(model$MAE.Training)){
    model$MAE.Training = Inf
  }
  notZeroindexes = which(target>0)
  nzTarget = target[notZeroindexes]
  nzExpectations = expectations[notZeroindexes]
  diff = (nzExpectations-nzTarget)
  model$MAPE = mean(abs(diff/nzTarget), na.rm = TRUE)
  if(!is.finite(model$MAPE)){
    model$MAPE = Inf
  }
  model$ChiSquared.Training = sum(diff^2/nzTarget, na.rm = TRUE)
  if(!is.finite(model$ChiSquared.Training)){
    model$ChiSquared.Training = Inf
  }
  return (model)
}
getModel = function(modelParameters, series){
  model = NULL; 
  training.series.size = modelParameters$training.series.size
  days = series$normalizedDays[1:training.series.size]
  training.series.totalCases = modelParameters$training.series.totalCases
  shape1s = modelParameters$shape1s
  shape2s = modelParameters$shape2s
  ncps = modelParameters$ncps 
  nNCBs = modelParameters$nNCBs
  w = modelParameters$weights
  model$PDF = function(x){
    pdfs=
      foreach(i = 1:nNCBs, .combine = cbind
              , .export = c('w', 'shape1s', 'shape2s', 'ncps'))%do%{#i=1
                pdf_i = w[i]*dbeta(x, shape1 = shape1s[i], shape2 = shape2s[i], ncp = ncps[i])
                # plot(days_norm, pdf_i, type="o", col = "blue")
                # pdfs = pdfs + pdf_i
                return(pdf_i)
              }
    # pdfs = ifelse(is.matrix(pdfs), rowSums(pdfs, na.rm = TRUE), pdfs)
    pdfs = rowSums(as.matrix(pdfs), na.rm = TRUE)
    # plot(x, pdfs, type="l")
    return(pdfs)
  }
  model$PDFs = model$PDF(x= days)
  probabilities = model$PDFs[1:training.series.size]*(days[2]-days[1])# in the day- model$PDFs[1:training.series.size]
  cum.Probs = sum(probabilities, na.rm = TRUE)
  totalIncidencePrediction = round(training.series.totalCases/cum.Probs)#modelParameters$totalIncidencePrediction
  totalIncidencePrediction = min(c(totalIncidencePrediction, populationSize), na.rm = TRUE)
  expectations = round(probabilities*totalIncidencePrediction); 
  model$expectations = expectations
  #plot(expectations, series$target[1:(training.series.size)]);plot(expectations, col="blue"); points(series$target, type="l");  
  target = series$target[1:training.series.size]
  diff0 = expectations-target
  
  log_mle = dnorm(x = diff0, mean = mean(diff0, na.rm=TRUE), sd = sd(diff0, na.rm=TRUE), log = TRUE)
  log_mle = sum(log_mle, na.rm = TRUE)
  k = nNCBs*4#the number of parameters of the mixed NCBs model
  n = training.series.size
  BIC = -2*log_mle + k*log(n)
  model$BIC = BIC
  if(!is.finite(model$BIC)){
    model$BIC = Inf
  }
  
  model$MSWE = mean((days*diff0)^2, na.rm = TRUE)
  if(!is.finite(model$MSWE)){
    model$MSWE = Inf
  }
  
  model$MSE.Training = mean(diff0^2, na.rm = TRUE)
  if(!is.finite(model$MSE.Training)){
    model$MSE.Training = Inf
  }
  model$RMSE.Training = sqrt(model$MSE.Training)
  model$MAE.Training = mean(abs(diff0), na.rm = TRUE)
  if(!is.finite(model$MAE.Training)){
    model$MAE.Training = Inf
  }
  notZeroindexes = which(target>0)
  nzTarget = target[notZeroindexes]
  nzExpectations = expectations[notZeroindexes]
  diff = (nzExpectations-nzTarget)
  model$MAPE = mean(abs(diff/nzTarget), na.rm = TRUE)
  if(!is.finite(model$MAPE)){
    model$MAPE = Inf
  }
  model$ChiSquared.Training = sum(diff^2/nzTarget, na.rm = TRUE)
  if(!is.finite(model$ChiSquared.Training)){
    model$ChiSquared.Training = Inf
  }
  model$totalIncidencePrediction = totalIncidencePrediction
  return (model)
}

getOptimalModel_old = function(data = dataset_i, dataName = dataNm_i, training.series.size=n){#training.series.totalCases, forecastingTimeHorizon = forecastingTimeHorizon){
  print(paste("***", dataName, ": NCB Model ***"))
  pGSA <<- tuningParameters[[dataName]]$GSA
  forecastingTimeHorizon = tuningParameters[[dataName]]$forecastingTimeHorizon
  modellingTime = proc.time()[[3]]
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
    PerformanceMetric = model$MSE.Training# + model$ChiSquared.Training
    if(PerformanceMetric < optimalModel$PerformanceMetric){
      optimalModel$PerformanceMetric <<- PerformanceMetric
      optimalModel$model <<- model
      optimalModel$modelParameters <<- modelParameters
      print(paste(dataName, "Best Model, #calls =", calls, "PerformanceMetric=", PerformanceMetric));
      #print(modelParameters);
    }
    return(PerformanceMetric)
  }
  trash = function(){
    days=1:600; days_norm = days/max(days)
    dbeta = dbeta(x=days_norm, shape1=4, shape2=4, ncp = .01); plot(days_norm, dbeta)
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
  max_shape1 = 100; max_shape2 = 1000; max_ncp = 100
  max_N = 1e+7
  #max_Distributions = (length(DistributionsLabels)+1-1e-5)
  lowers <- c(#1                 , min_meanlog, min_sdlog, min_mean, min_sd, min_skewness, min_shape, min_scale, 
    min_shape1, min_shape2, min_ncp, min_N)
  uppers <- c(#max_Distributions, max_meanlog, max_sdlog, max_mean, max_sd, max_skewness, max_shape, max_scale, 
    max_shape1, max_shape2, max_ncp, max_N)
  starts = c(4, 4, .01, min_N+0.5*(max_N-min_N))
  tol <- 1e-3
  out = 
    tryCatch({
      expr = GenSA(lower = lowers, upper = uppers, fn = fitness 
                   , par = starts
                   , control=list(max.call = pGSA$max.call, max.time=pGSA$max.time
                                  , maxit = pGSA$maxit, verbose = FALSE, smooth = FALSE
                                  , seed=1, nb.stop.improvement = pGSA$nb.stop.improvement
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
  
  modellingTime = proc.time()[[3]] - modellingTime#time in seconds
  modellingTime = as.numeric(modellingTime) 
  print(paste(dataName, "NCB Model, #calls =", calls, "PerformanceMetric=", optimalModel$PerformanceMetric));
  print(optimalModel$modelParameters);
  print(paste("$modellingTime=", modellingTime));
  
  optimal = list()
  optimal$modellingTime = modellingTime
  optimal$forecastingTime = NA
  optimal$model = optimalModel$model
  optimal$GenSA_output = out
  optimal$modelParameters = optimalModel$modelParameters
  optimal$data = series
  return(optimal)
} 
getOptimalModel = function(data = dataset_i, dataName = dataNm_i, training.series.size=n, max_nNCBs = 50){
  print(paste("***", dataName, ": MIXED NCBs Model ***"))
  pGSA <<- tuningParameters[[dataName]]$GSA
  forecastingTimeHorizon = tuningParameters[[dataName]]$forecastingTimeHorizon
  modellingTime = proc.time()[[3]]
  calls <<- 0; optimalModel <<- list()
  optimalModel$PerformanceMetric = Inf
  optimalModel$model = NULL
  optimalModel$parameters = NULL
  target.all = data$target
  target.train = target.all[1:training.series.size]
  training.series.totalCases = sum(target.train, na.rm = TRUE)
  populationSize <<- NULL#populations$populaction[populations$date==2020 & populations$territory_label==dataName]
  # populationSize <<- 0.9*populationSize
  if(length(populationSize)==0){
    populationSize <<- 1e+8
  }
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
    nNCBs = floor(parameters[1]);
    modelParameters$nNCBs =   nNCBs
    startIndex = 2; 
    modelParameters$shape1s =   parameters[startIndex:(startIndex+nNCBs-1)]
    startIndex = startIndex+max_nNCBs; 
    modelParameters$shape2s =   parameters[startIndex:(startIndex+nNCBs-1)]
    startIndex = startIndex+max_nNCBs; 
    modelParameters$ncps =   parameters[startIndex:(startIndex+nNCBs-1)]
    startIndex = startIndex+max_nNCBs; 
    modelParameters$weights =   parameters[startIndex:(startIndex+nNCBs-1)]
    modelParameters$weights = modelParameters$weights/sum(modelParameters$weights, na.rm=TRUE)
    
    modelParameters$training.series.totalCases = training.series.totalCases
    modelParameters$training.series.size = training.series.size
    modelParameters$forecastingTimeHorizon = forecastingTimeHorizon
    model = getModel(modelParameters, series)
    PerformanceMetric = model$MSE.Training#model$MSWE#model$BIC# + model$ChiSquared.Training
    if(PerformanceMetric < optimalModel$PerformanceMetric){
      optimalModel$PerformanceMetric <<- PerformanceMetric
      optimalModel$model <<- model
      modelParameters$totalIncidencePrediction =   model$totalIncidencePrediction
      optimalModel$modelParameters <<- modelParameters
      print(paste(dataName, "Best Model, #calls =", calls, "PerformanceMetric=", PerformanceMetric));
      # print(modelParameters);
      print(paste("nNCBs =", modelParameters$nNCBs));
    }
    return(PerformanceMetric)
  }
  trash = function(){
    days=1:600; days_norm = days/max(days)
    shape1s = c(500, 50)
    shape2s = c(50, 500)
    ncps = c(0, 0)
    w = c(0.1, 0.9)
    nNCBs = length(w)
    pdfs = 0
    for(i in 1:nNCBs){
      pdf_i = w[i]*dbeta(days_norm, shape1 = shape1s[i], shape2 = shape2s[i], ncp = ncps[i])
      # plot(days_norm, pdf_i, type="o", col = "blue")
      pdfs = pdfs + pdf_i
    }
    plot(days_norm, pdfs, type="l")
  }
  set.seed(0); #definir semente 
  n = length(series) #n ? o tamanho da s?rie
  min_shape1 = 1;   min_shape2 = 1;    min_ncp = 0;  min_weight = 0;  min_nNCBs = .9
  max_shape1 = 100; max_shape2 = 1000; max_ncp = 100; max_weight = 1; 
  lowers <- c(min_nNCBs
              , rep(min_shape1, max_nNCBs)
              , rep(min_shape2, max_nNCBs)
              , rep(min_ncp, max_nNCBs)
              , rep(min_weight, max_nNCBs))
  uppers <- c(max_nNCBs + 1 - 1e-10
              , rep(max_shape1, max_nNCBs)
              , rep(max_shape2, max_nNCBs)
              , rep(max_ncp, max_nNCBs)
              , rep(max_weight, max_nNCBs))
  starts = NULL
  # file_GenSA_output = paste(path_GenSA_output, "/", dataName, "_MNCB_GenSA_output.rds", sep="")
  # if(file.exists(file_GenSA_output)){
  #   GenSA_output = readRDS(file = file_GenSA_output)
  #   starts = GenSA_output$par#[1:3]
  # }
  # else{
    starts = #just one NCB with shape1 = 4, shape2=4, ncb = 0.01, weight=1
      c(1
        , c(4, rep(0, max_nNCBs-1))
        , c(4, rep(0, max_nNCBs-1))
        , c(.01, rep(0, max_nNCBs-1))
        , c(1, rep(0, max_nNCBs-1)))
  # }
  
  tol <- 1e-3
  out = 
    tryCatch({
      #For GA (Genetic Algorithms) package
      # expr = ga(type = 'real-valued', parallel = TRUE, monitor = TRUE, seed = 1
      #           , lower = lowers, upper = uppers, fitness = function(x) {-fitness(x)}
      #           , suggestions = matrix(starts, byrow = TRUE, nrow = 1)
      #           , maxiter = pGSA$maxiter, popSize = pGSA$popSize
      #           )
      #For GSA (Generalized Simulated Annealing) package
      expr = GenSA(lower = lowers, upper = uppers, fn = fitness
                   , par = starts
                   , control=list(max.call = pGSA$max.call, max.time=pGSA$max.time
                                  , maxit = pGSA$maxit, verbose = FALSE, smooth = FALSE
                                  , seed=1, nb.stop.improvement = pGSA$nb.stop.improvement
                                  , temperature = pGSA$temperature))
    }, 
    error = function(e){
      message(paste("Error in getOptimalModel GenSA.", e)); 
      return(return(NA))
    } 
    #, finally={message(paste("Error: ginv(covMatrix)")); return(NA)}
    )
  #plot(optimalModel$model$expectations, series$target);
  #plot(series$target); points(optimalModel$model$expectations, col="blue");  
  
  modellingTime = proc.time()[[3]] - modellingTime#time in seconds
  modellingTime = as.numeric(modellingTime) 
  print(paste(dataName, "MNCB Model, #calls =", calls, "PerformanceMetric=", optimalModel$PerformanceMetric));
  print(optimalModel$modelParameters);
  print(paste(">$modellingTime=", modellingTime));
  
  optimal = list()
  optimal$modellingTime = modellingTime
  optimal$forecastingTime = NA
  optimal$model = optimalModel$model
  optimal$GenSA_output = out
  optimal$modelParameters = optimalModel$modelParameters
  optimal$data = series
  return(optimal)
} 

getModelForecasts_old = function(model=optModel_i, data = dataset_i){
  #plot(series)
  forecastingTime = proc.time()[[3]]
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
  newExpectations = round(probabilities*N)
  
  target = series[(training.series.size+1):forecasting.horizon]
  diff = newExpectations - target
  RMSE.Test = sqrt(mean(diff^2, na.rm = TRUE))
  if(!is.finite(RMSE.Test)){
    RMSE.Test = NA
  }
  MAE.Test = mean(abs(diff), na.rm = TRUE)
  if(!is.finite(MAE.Test)){
    MAE.Test = NA
  }
  
  forecasts = c(forecasts, newExpectations)
  forecastingTime = proc.time()[[3]] - forecastingTime
  ret = list()
  ret$forecastingTime = as.numeric(forecastingTime)
  ret$forecasts = as.numeric(forecasts)
  ret$RMSE.Test = RMSE.Test
  ret$MAE.Test = MAE.Test
  return (ret)
}
getModelForecasts = function(model=optModel_i, data = dataset_i){
  #plot(series)
  forecastingTime = proc.time()[[3]]
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
  newExpectations = round(probabilities*N)
  
  target = series[(training.series.size+1):forecasting.horizon]
  diff = newExpectations - target
  RMSE.Test = sqrt(mean(diff^2, na.rm = TRUE))
  if(!is.finite(RMSE.Test)){
    RMSE.Test = NA
  }
  MAE.Test = mean(abs(diff), na.rm = TRUE)
  if(!is.finite(MAE.Test)){
    MAE.Test = NA
  }
  
  forecasts = c(forecasts, newExpectations)
  forecastingTime = proc.time()[[3]] - forecastingTime
  ret = list()
  ret$forecastingTime = as.numeric(forecastingTime)
  ret$forecasts = as.numeric(forecasts)
  ret$RMSE.Test = RMSE.Test
  ret$MAE.Test = MAE.Test
  return (ret)
}

