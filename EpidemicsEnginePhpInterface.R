g=1
# args <- commandArgs(trailingOnly = TRUE); ROOT = "C:/xampp/htdocs/mesor/login/visao/covid19/"#valid for UFCA- server
LEVEL = list(COUNTRIES="PAÍSES", BRAZILIAN_STATES="ESTADOS BRASILEIROS")
ROOT = "G:/Meu Drive/UFCA/Pesquisa/MESOR/Codes/";#valid for praf notebook
# args = c(LEVEL$COUNTRIES, "@", "Argentina", "::", "Brazil", "::", "China", "::", "Germany", "::", "India", "::", "Iran", "::", "Italy", "::", "Japan", "::", "France", "::", "Korea,", "South", "::", "Spain", "::", "United", "Kingdom", "::", "US", "::", "Peru", "::", "Bolivia", "::", "Chile", "::", "Kosovo" )##PRAF
args =  c(LEVEL$BRAZILIAN_STATES, "@", "PE", "::", "CE", "::", "SP", "::", "RJ", "::", "BA", "::", "MA", "::", "AM", "::", "DF", "::", "MG", "::", "AC", "::", "RR", "::", "RO", "::", "PR", "::", "SC")

isToDebug <<- TRUE
source(paste(ROOT, "Auxiliary/Auxiliar.R", sep=""))
sourceMode <<- getSourceMode(isToDebug)
sourceMode(paste(ROOT, "Epidemics/MainEpidemics.R", sep=""))
DATA_LABELS = NULL
computeEpidemicCases = function(DATA_LABELS ){
  todayDate <<- as.character(Sys.Date())
  yesterdayDateNumber = NULL
  # yesterdayDateNumber = paste0(yesterdayDateNumber, collapse = "")
  
  loadModellingParameters(
    pTrainingPercentual = 1.0
    , pROOT = ROOT       #PRAF
    , pEPIDEMICS_CODES_PATH = paste(ROOT, "Epidemics/", sep="")
    , pDATA_PATH = paste(EPIDEMICS_CODES_PATH, "Data/", todayDate, "/", sep="")
    , pRESULTS_PATH = paste(EPIDEMICS_CODES_PATH, "Results/", todayDate, "/", sep="")
    , pDATA_LABELS = DATA_LABELS)
  
  todayFolder_DATA_PATH = paste(EPIDEMICS_CODES_PATH, "Data/", todayDate, sep="")
  DATA_LABELS_TO_COMPUTE = NULL; data = NULL; col_names = NULL; dates = NULL; seriesSize=NULL
  folder = paste(todayFolder_DATA_PATH)
  folderExists = file.exists(folder)
  jhu_url = NULL
  computeCountriesCases = function(){
    saveCountryData = function(contry_i){
      data_i = data[data$Country.Region==country_i,]; #View(data_i, title = country_i)
      cumulative_i = as.integer(lapply(data_i[, col_names[5:nCols]], sum))
      target_i = c(cumulative_i[1]
                   , cumulative_i[2:seriesSize] - cumulative_i[1:(seriesSize-1)])
      start_i = which(target_i>0)[1]
      if(is.na(start_i)){
        start_i = 1
      }
      target_i = target_i[start_i:(nCols-4)]
      date_i = date[start_i:(nCols-4)]
      data_to_save_i = cbind(date=as.character(date_i), target= target_i)
      #View(rbind(data_i[,5:nCols], target_i))
      #View(data_to_save_i, title=contry_i)
      write.table(x = data_to_save_i, file = paste(todayFolder_DATA_PATH, "/covid_19_dailyConfirmedCases_", country_i, ".csv", sep=""), dec=".", sep=";", row.names = FALSE);#, header = TRUE); 
    }
    ##verifying if the case has been studied today...
    nCountries = length(DATA_LABELS)
    seriesSize = NULL; nCols = NULL
    if(folderExists==FALSE){
      DATA_LABELS_TO_COMPUTE <<- DATA_LABELS
      dir.create(folder)
      dir.create(paste(EPIDEMICS_CODES_PATH, "Results/", todayDate, sep=""))
      
      loadPackages("RCurl")
      url = getURL(jhu_url)
      data <- read.csv (text = url)
      #Country.Region_s = names(table(data$Country.Region)); print(cbind(Country.Region_s))
      col_names = names(data); nCols = length(col_names)
      dates = col_names[5:nCols]
      seriesSize = length(dates)
      dates = paste(dates, collapse =  "")
      loadPackages("stringr")
      dates = str_split(string = dates, pattern = "X")
      dates = dates[[1]][-1]
      date = as.Date(dates, "%m.%d.%y")
      for(i in 1:nCountries){#i=1;i=3
        country_i = DATA_LABELS[i]
        saveCountryData(country_i)
      }
    }
    else{
      for(i in 1:nCountries){#i=1;i=3
        country_i = DATA_LABELS[i]
        fileName = paste(todayFolder_DATA_PATH, "/covid_19_dailyConfirmedCases_", country_i, ".csv", sep="")
        fileExists = file.exists(fileName)
        if(fileExists == FALSE){
          DATA_LABELS_TO_COMPUTE <<- c(DATA_LABELS_TO_COMPUTE, country_i)
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
  
  computeBrazilianStatesCases = function(){
    saveStateData = function(state_i){
      data_i = data[data$estado==state_i,]; #View(data_i, title = state_i)
      #cumulative_i = as.integer(lapply(data_i[, col_names[5:nCols]], sum))
      target_i = data_i$casosNovos #c(cumulative_i[1] , cumulative_i[2:seriesSize] - cumulative_i[1:(seriesSize-1)])
      start_i = which(target_i>0)[1]
      if(is.na(start_i)){
        start_i = 1
      }
      target_i = target_i[start_i:seriesSize]
      date_i = date[start_i:seriesSize]
      data_to_save_i = cbind(date=as.character(date_i), target= target_i)
      #View(rbind(data_i[,5:nCols], target_i))
      #View(data_to_save_i, title=state_i)
      write.table(x = data_to_save_i, file = paste(todayFolder_DATA_PATH, "/covid_19_dailyConfirmedCases_", state_i, ".csv", sep=""), dec=".", sep=";", row.names = FALSE);#, header = TRUE); 
    }
    ##verifying if the case has been studied today...
    nStates = length(DATA_LABELS)
    data = NULL; #col_names = NULL; dates = NULL; seriesSize = NULL; nCols = NULL
    if(folderExists==FALSE){
      DATA_LABELS_TO_COMPUTE <<- DATA_LABELS
      dir.create(folder)
      dir.create(paste(EPIDEMICS_CODES_PATH, "Results/", todayDate, sep=""))
      
      loadPackages("RCurl")
      url = getURL(jhu_url)
      data <- read.csv (text = url, sep=";")#;View(data)
      #col_names = names(data); nCols = length(col_names)
      state1 = names(table(data$estado))[1]
      dates = as.character(data$data[data$estado==state1])
      seriesSize = length(dates)
      date = as.Date(dates, "%d/%m/%y")
      if(length(which(is.na(date)))==seriesSize){
        date = as.Date(dates)#, "%y-%m-%d")
      }
      for(i in 1:nStates){#i=1;i=3
        state_i = DATA_LABELS[i]
        saveStateData(state_i)
      }
    }
    else{
      for(i in 1:nStates){#i=1;i=3
        state_i = DATA_LABELS[i]
        fileName = paste(todayFolder_DATA_PATH, "/covid_19_dailyConfirmedCases_", state_i, ".csv", sep="")
        fileExists = file.exists(fileName)
        if(fileExists == FALSE){
          DATA_LABELS_TO_COMPUTE <<- c(DATA_LABELS_TO_COMPUTE, state_i)
          if(is.null(data)){
            loadPackages("RCurl")
            url = getURL(jhu_url)
            data <- read.csv (text = url, sep=";")#;View(data)
            #col_names = names(data); nCols = length(col_names)
            state1 = names(table(data$estado))[1]
            dates = as.character(data$data[data$estado==state1])
            seriesSize = length(dates)
            date = as.Date(dates, "%d/%m/%y")
            if(length(which(is.na(date)))==seriesSize){
              date = as.Date(dates)#, "%y-%m-%d")
            }
          }
          saveStateData(state_i)
        }
      }
    }
  }

  if(level == LEVEL$COUNTRIES) {
    jhu_url <- paste("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv", sep = "")
    computeCountriesCases()
  } 
  else if (level == LEVEL$BRAZILIAN_STATES) {
    # jhu_url_aux = "https://mobileapps.saude.gov.br/esus-vepi/files/unAFkcaNDeXajurGB7LChj8SgQYS2ptm/dfe64e164c58c05c77afdd5ecbe8c689_Download_COVID19_"
    # yesterdayDateNumber = strsplit(x=as.character(Sys.Date()-1), split = "-")[[1]]
    # yesterdayDateNumber = paste0(yesterdayDateNumber, collapse = "")
    # jhu_url=  paste(jhu_url_aux, yesterdayDateNumber, ".csv", sep = "")
    # jhu_url=  "https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-states.csv"
    jhu_url=  "https://raw.githubusercontent.com/praf62/covid_19_brazil/master/Covid_19_BrazilianStates.csv"
    # loadPackages("RCurl"); url = getURL(jhu_url); data <- read.csv (text = url, sep=";"); View(data); cbind(names(table(data$estado)))
    computeBrazilianStatesCases()
  }
  
  if(!is.null(DATA_LABELS_TO_COMPUTE)){
    performModelling(pDATA_LABELS = DATA_LABELS_TO_COMPUTE)
    computeModelsResults(DATA_LABELS = DATA_LABELS_TO_COMPUTE, isToComputePlots=TRUE, isToComputeStructures=FALSE)
  }
  computeModelsResults(DATA_LABELS = DATA_LABELS, isToComputePlots=FALSE, isToComputeStructures=TRUE)
  #View(data)
  #View(alldataset)
}
# computeEpidemicCases(c("Argentina", "Brazil", "China", "Germany", "India", "Iran", "Italy", "Japan", "France"
#                          , "Korea, South", "Spain","United Kingdom", "US"))
# computeEpidemicCases("Angola");  computeEpidemicCases("Australia"); # computeEpidemicCases("Afghanistan")
convertStringToConcatenation = function(args) {
  nTerms =length(args)
  if(nTerms>1){
    args = paste0(args, collapse =" ")
  }
  aux = strsplit(x=args, split = " @ ")[[1]]
  level <<- aux[1]
  c_args = strsplit(x=aux[2], split = " :: ")[[1]]
  return(c_args)
}
c_args = convertStringToConcatenation(args) 
computeEpidemicCases(c_args)
# computeEpidemicCases(args)
#computeEpidemicCases(c("Aruba", "Angola", "Australia", "Brazil", "US", "Christmas Island"))