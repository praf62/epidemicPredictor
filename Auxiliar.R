loadPackages<-function(...) {
  libs<-unlist(list(...))
  req<-unlist(lapply(libs,require,character.only=TRUE))
  need<-libs[req==FALSE]
  if(length(need)>0){ 
    install.packages(need)
    lapply(need,require,character.only=TRUE)
  }
}

generateTimeSeriesGraphic =  function(all.series, n, v=0, m, seriesName=seriesNm_i, nCombinators = 5, ylab = NULL){
  n_m = nrow(all.series)
  dateColName = "date"
  dates = as.character(all.series[[dateColName]])
  if(length(dates)==0){
    dates = 1:n_m
  }
  else{
    nmCols = colnames(all.series)
    dateIndex = which(nmCols==dateColName)
    all.series = all.series[,-c(dateIndex)]
  }
  min_y = min(all.series, na.rm = TRUE)
  max_y = max(all.series, na.rm = TRUE)
  max_y = max_y #+ 0.2*(max_y - min_y)
  nSeries = ncol(all.series)
  nSingleModels = nSeries - 1 - nCombinators
  all.colors = colors()
  aux = max(1, nSeries)
  plot_colors <- all.colors[seq(from=24, by=4, length.out = aux)]
  # pch <- c(NA,rep(1,aux),seq(from=2, by=2, length.out = nCombinators))#rep(NA, nSeries)# an integer code for one of a set of graphics symbols
  # lty <- c(1,rep(3,aux), rep(3, nCombinators))#line type. Line types can either be specified as an integer (0=blank, 1=solid (default), 2=dashed, 3=dotted, 4=dotdash, 5=longdash, 6=twodash) 
  pch_seq = rep(2:6, (aux+nCombinators))[1:(aux+nCombinators)]
  pch <- c(NA,1:(aux+nCombinators))#rep(NA, nSeries)# an integer code for one of a set of graphics symbols
  lty <- c(1, pch_seq)#line type. Line types can either be specified as an integer (0=blank, 1=solid (default), 2=dashed, 3=dotted, 4=dotdash, 5=longdash, 6=twodash) 
  lwd <- c(3,rep(1,aux), rep(1, nCombinators))# line width
  types = c("l", rep("l",aux), rep("b", nCombinators))#1-character string giving the type of plot desired. The following values are possible, for details, see plot: "p" for points, "l" for lines,...
  
  plotForecasts = function(modelsType = "SingleModels", seriesIndexes) {
    png(filename = paste(RESULTS_PATH, seriesName, "_", modelsType, "_Series.png", sep=""), width = 2.7*480, height = 1.25*480)
    #png(filename = paste(RESULTS_PATH, seriesName, "_Series.png", sep=""), width = 2.7*480, height = 1.25*480)
    # Ploting the plot box
    mai = c(3,2.5,0,0); mar = c(1,1,1,1); mgp = c(3,1,0); par(family="serif", mar=mar, mgp=mgp, mai=mai)
    plot(x=c(1,(n+v+m)), y=c(min_y, 1.2*max_y), type="n", ylim=c(min_y,max_y), axes=FALSE, ann=FALSE)#, lwd=lwd[i], lty=lty[i], pch=NA, col=plot_colors[i])
    by_x = max(round(0.1*n_m), 1); at_x = sort(c(n, (n_m-1), n_m, seq(from=1, to=n_m, by = by_x)))
    by_y = round((max_y - min_y)/5, 2)
    par(family="serif", mar=mar, mgp=mgp, mai=mai)
    axis(1, at=at_x, lab=dates[at_x], las=2
         , cex.lab=1, cex.axis = 3, cex=2)#, las=2)#lab=seq(from=1, to=seriesSize, by = by_x))
    axis(2, at=seq(from=round(min_y,2), to=round(max_y, 2), by = by_y), las=1
         , cex.lab=2, cex.axis = 3, cex=1)#, las=2)
    # Create box around plot
    box()
    title(xlab="date", col.lab=rgb(0,0,0)
          , cex.lab=3, cex.axis = 3, cex=1, line = 14)#, las=2)
    if(is.null(ylab)){ylab=seriesName}
    title(ylab=ylab, col.lab=rgb(0,0,0)
          , cex.lab=3, cex.axis = 3, cex=1, line = 10)#, las=2)
    mar = c(10,10,1,1); mgp = c(4,1,0); par(family="serif", mar=mar, mgp=mgp, mai=mai)
    # blocking training, validation, and test sets
    #  if(from==1){
    #points(x=c(n+1:(n+v+m)), y=all.series$target[n_treinamento+1:N], type="l", lty=1, col="black", lwd=lwd+2)
    # Insere a linha de divide a parte a ser prevista da de treinamento
    lines(x=c(n,n),c(min_y,max_y), col="gray", lwd=2, lty=2)
    #lines(x=c((n+v),(n+v)),c(min_y,max_y), col="orange", lwd=2, lty=1)
    #  }
    
    #Plot the series
    legend = NULL
    seriesNm = dimnames(all.series)[[2]]
    nSeries = length(seriesIndexes)
    if(nSeries==1){
      i = 1
      lines(x=(1:(n+v+m)), y=all.series, type=types[i], pch=pch[i], col=plot_colors[i], ylim=c(min_y,max_y), ann=FALSE, lwd=lwd[i], lty=lty[i])#, axes=FALSE
      legend = c(legend, seriesNm[i])
    }
    else{
      for (j in 1:nSeries){
        i = seriesIndexes[j]
        lines(x=(1:(n+v+m)), y=all.series[,i], type=types[i], pch=pch[i], col=plot_colors[i], ylim=c(min_y,max_y), ann=FALSE, lwd=lwd[i], lty=lty[i])#, axes=FALSE
        legend = c(legend, seriesNm[i])
      }
    }
    # Create a title with a red, bold/italic font
    title(main="", col.main="red", font.main=4)
    # title(xlab= "time index", col.lab=rgb(0,0,0))
    # title(ylab= seriesName, col.lab=rgb(0,0,0))
    
    # Create a legend
    #  legend <- c(expression(u[t]), expression(hat(u)[t]), expression(x[paste(t,1)]), expression(x[paste(t,2)]), expression(x[paste(t,3)]), expression(SA))
    legend[1] = seriesName
    legend(x=max(.1*n, 1), y=max_y, legend=legend, cex=3, col=plot_colors[seriesIndexes]
           , pch=pch[seriesIndexes], lty=lty[seriesIndexes], lwd = 1*lwd[seriesIndexes], ncol=5, bty="n")
    dev.off()
  }
  singleSeriesIndexes = 1
  if(!is.null(nSeries)) {
    singleSeriesIndexes = 1:(nSingleModels+1)
  }
  plotForecasts(modelsType = "SingleModels", seriesIndexes = singleSeriesIndexes)
  if(nCombinators>0){
    combinedSeriesIndexes = c(1, (nSingleModels+2):nSeries)
    plotForecasts(modelsType = "CombinedModels", seriesIndexes = combinedSeriesIndexes)
  }
}
getNormalizedSeries = function(series, min, max){
  dataset.norm = (series-min)/(max-min)
  return(dataset.norm)
}
getDenormalizedSeries = function(dataset.norm, min, max){
  dataset = dataset.norm*(max-min) + min
  return(dataset)
}
studyCorrespondencesAndResiduals = function(targets, forecasts, modelName, seriesName, optCbObj=NULL , nCombinators){
  plotCorrespondenceBetweenForecastsAndTargets = function(targets, forecasts, modelName, seriesName){
    min_y = min(c(targets, forecasts), na.rm = TRUE)
    max_y = max(c(targets, forecasts), na.rm = TRUE)

    png(filename = paste(RESULTS_PATH, seriesName, "_correspondence_", modelName, ".png", sep=""), width = 1.25*480, height = 1.25*480)
    plot(forecasts, targets, xlab = paste(modelName, "forecasts"), ylab = seriesName, ylim=c(min_y,max_y), xlim=c(min_y,max_y))
    f = function(x){
      x
    }
    curve(expr = f, from=min_y, to=max_y, add=TRUE)
    dev.off()
  }
  studyResiduals = function(targets, forecasts, modelName, seriesName, toMakeIndividualStudy=FALSE){
    #CB RESIDUALS
    #sink(file = paste(RESULTS_PATH, seriesName, ".residuals.", modelName, ".txt", sep=""))
    #print(paste(modelName, seriesName), sep=":")
    residuals = targets - forecasts
    if(!toMakeIndividualStudy){
      pairsFigure(data = residuals, seriesName)
    } else {
      png(filename = paste(RESULTS_PATH, seriesName, "_histogram_", modelName, ".png", sep=""), width = 2.7*480, height = 1.25*480)
      hist(residuals, main = paste(modelName, seriesName, sep="_"), freq = FALSE)
      dev.off()
      print(lillie.test((residuals)))
      print(Box.test((residuals)))
      
      #SINGLE MODELS RESIDUALS
      modelsNames = names(optCbObj$MPDs)
      nModels = length(modelsNames)
      residualsMatrix = NULL
      for (i in 1:nModels){
        modelName = modelsNames[i]
        print(paste(modelName, seriesName), sep=":")
        residuals = optCbObj$MPDs[[modelName]]$residuals
        residualsMatrix = cbind(residualsMatrix, residuals)
        print(summary(residuals))
        png(filename = paste(RESULTS_PATH, seriesName, "_histogram_", modelName, ".png", sep=""), width = 2.7*480, height = 1.25*480)
        h = hist(residuals, main = paste(modelName, seriesName, sep="_"), freq = FALSE)
        min_ = min(residuals, na.rm = TRUE)
        max_ = max(residuals, na.rm = TRUE)
        max_f = max(h$density)
        f = optCbObj$MPDs[[modelName]]$pdf
        c = curve(f, from=min_, to=max_, add = TRUE, col = "red")
        max_c = max(c$y)
        # text(x=min_, y=.6*max(max_c, max_f), cex=1, pos=4,col="black",
        #       paste("p*=", format(pValues$Lilli, scientific = TRUE, digits=2)))
        dev.off()
      }
      #sink()
      dimnames(residualsMatrix)[[2]] = modelsNames; #View(CDFs)
      png(filename = paste(RESULTS_PATH, seriesName, "_residuals_", modelName, ".png", sep=""), width = 2.7*480, height = 1.25*480)
      pairs(residualsMatrix)#, main = "Residuals correspondence")
      dev.off()
    }
  }  
  plotCorrespondenceBetweenForecastsAndTargets(targets, forecasts, modelName, seriesName)
  # if(!is.null(optCbObj)){
  #   phase = unlist(strsplit(seriesName, "[.]"))[2]
    # if(phase=="validation"){
     # lastSIngleModelIndex = ncol(forecasts) - nCombinators + 1
      #studyResiduals(targets, forecasts[, 2:lastSIngleModelIndex], modelName, seriesName)
  #   }
  # }
}
pairsFigure = function(data, seriesName, randomData=NULL, phaseLabel="Training"){
  panel.hist <- function(x, ...){
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5) )
    h <- hist(x, plot = FALSE)
    breaks <- h$breaks; nB <- length(breaks)
    y <- h$counts; y <- y/max(y)
    rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
  }
  nCol = ncol(data)
  if(!is.null(nCol)){
    png(filename = paste(RESULTS_PATH, seriesName, "_", phaseLabel, "_residualsRelationship", ".png", sep=""), width = 2.7*480, height = 1.25*480)
    pairs(data[,1:nCol], panel = panel.smooth,
          #cex = 1.5, pch = 24, bg = "light blue",
          diag.panel = panel.hist, cex.labels = 2, font.labels = 2)
    # myFormula = paste(names(data), collapse = "+")
    # myFormula = paste("~", myFormula, sep="")
    # myFormula = as.formula(myFormula)
    # pairs(formula=myFormula, data=data, main = seriesName)
    dev.off()
  }
}
pairsRealAndRandomSampleFigure = function(data, seriesName){
  nCol = ncol(data)
  png(filename = paste(RESULTS_PATH, seriesName, "_withoutOutliersAndWithRandomSample", ".png", sep=""), width = 2.7*480, height = 1.25*480)
  pairs(data[1:(nCol-1)], main = seriesName,
        pch = 21, bg = c("red", "green3")[unclass(data$label)])
  dev.off()
}

histFigure = function(data, MPD, modelName, seriesName){
  png(filename = paste(RESULTS_PATH, seriesName, "_residualsHistogram_", modelName, ".png", sep=""), width = 2.7*480, height = 1.25*480)
  hist(data, freq = FALSE, main="", xlab="residual", ylab="density"); curve(MPD$pdf(x), from = min(data, na.rm = TRUE), to=max(data, na.rm = TRUE), add = TRUE, col="red", type = "p")
  dev.off()
}
getOutliersIndexes = function(data){
  bp = boxplot(data, plot=FALSE)
  indexes = which(data %in% bp$out)
  #data[indexes]=NA
  #return(data)
  return(indexes)
}
#3d plot
z_mapp = function(x1, x2, f, ...){
  n1 = length(x1)#Pega a quantidade de elementos contidas em x1
  n2 = length(x2)
  ret = matrix(nrow=n1, ncol=n2)#cria uma matrix 50x50
  for(i in 1:n1){
    for(j in 1:n2){
      tryCatch({
        ret[i,j]=f(x1[i], x2[j])#x1 e x2 s?o seq_x e seq_y
      }, warning = function(war){
        print(paste("z_mapp_WARNING:  ",war, " (x1,x2)=(", x1[i], ", ",x2[j], ")"))
      }, error = function(err){
        print(paste("z_mapp_ERROR:  ",err, " (x1,x2)=(", x1[i], ", ",x2[j], ")"))
      }
      )
    }
  }
  ret
}

#correspondence
studyCorrespondencesAndResidualsAllModels = function(series, from = (n+1), to=n_m, phaseLabel = "Test", seriesName, nCombinators = nCombinators_i){
  series = series[from:to, ]
  target = series[["target"]]
  nSeries = ncol(series)
  modelsNames = names(series)
  for (i in 2:nSeries){
    modelName = modelsNames[i]
    studyCorrespondencesAndResiduals(targets = target
                                     , forecasts = series[,i]
                                     , modelName = modelName
                                     , seriesName = paste(seriesName, phaseLabel, sep="_")
                                     , nCombinators = nCombinators_i)
  }
  lastSingleModelIndex = nSeries - nCombinators
  if(lastSingleModelIndex>2){
    forecasts = series[, 2:lastSingleModelIndex]
    residuals = target - forecasts
    pairsFigure (data = residuals, seriesName=seriesName, phaseLabel = phaseLabel)
  }
}
