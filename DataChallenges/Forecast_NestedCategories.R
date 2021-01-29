library('lattice')
library('ggplot2')
library('forecast')
library('tseries')
library('mice')
library('gvlma')
library('dplyr')


### Read data
dailyData = read.csv("QS_Datos Limpios de Ventas.csv", header = TRUE, stringsAsFactors = FALSE)
mydateformat = "%d/%m/%Y"
dailyData$Fecha = as.Date(dailyData$Fecha, mydateformat)
maxDate = max(dailyData$Fecha, na.rm = TRUE)
future_dates = Sys.Date()+1:90
dailyData$Volumen = gsub(",", "", dailyData$Volumen)
dailyData$Volumen = as.numeric(dailyData$Volumen)

### Split data by Facility- Material combinations
##dailyData$Facility_Material <- paste(dailyData$Material, dailyData$Facility, sep = " - ")
##dailyData_split <- split(dailyData, dailyData$Facility_Material)

## 3. Split data by Facility-Material combinations
dailyData$Material_Facility <- paste(dailyData$Material, dailyData$Facility,  sep = " - ")
dailyData_split <- split(dailyData, dailyData$Material_Facility)
all_dates <- seq.Date(as.Date("2016-01-01"), as.Date("2018-12-31"), by = "day")

## Prepare data for analysis
prepared_data_split <- lapply(1:length(dailyData_split), function(i) {
  
  data_s = dailyData_split[[i]]
  time_series = data_s[c("Fecha", "Volumen")]
  time_series = time_series[time_series$Fecha %in% all_dates, ]
  time_series = rbind(time_series,
                      data.frame(Fecha = as.Date(setdiff(all_dates, data_s$Fecha), origin = "1970-01-01"),
                                 Volumen = NA))
  time_series = time_series[order(time_series$Fecha), ]
  time_series$nrow = 1:nrow(time_series)
  
##Clean ts (optional) 
##time_series$Clean_Vol = tsclean(time_series$Volumen, replace.missing = TRUE)
  
  ts_ma = NULL
  deseasonal_vol = NULL
  
### Decompose & deseasonalize data
  tryCatch({
    
    if (sum(is.na(time_series$Volumen)) > 0) {
      imp = mice(time_series[, c("Volumen", "nrow")])
      temp = complete(imp)
      time_series$Volumen = temp$Volumen
    }
    
    ts_ma = ts(time_series$Volumen, start = c(2016, 1), frequency = 7) #time series object for 365 periods (days)
    decomp = stl(ts_ma, s.window = "periodic")
    deseasonal_vol = seasadj(decomp)
    
  }, error = function(err) {
    
    print(err)
    
  })
  
  if (!is.null(ts_ma)) {
    write.csv(time_series, 
              paste0("DailySalesVols ", names(dailyData_split)[i], ".csv"), 
              row.names = FALSE, quote = FALSE)
  }
  
  deseasonal_vol
  
})

## Run analysis

result_arima <- lapply(prepared_data_split, function(vol_data) {
  
  result = NULL
  
  if (!is.null(vol_data)) {
    
    log_vol_data = log(vol_data)
    exp_forecast_arima = FALSE
    
    tryCatch({
      
      fit_arima = auto.arima(vol_data, seasonal = FALSE)
      test_arima = Box.test(fit_arima$residuals, type = "Ljung-Box")
      if (test_arima$p.value <= 0.05) {
        fit_arima = auto.arima(log_vol_data, seasonal = FALSE)
        exp_forecast_arima = TRUE
      }
      accuracy_arima = accuracy(fit_arima)
      accuracy_arima = accuracy_arima[colnames(accuracy_arima) == "MAPE"]
      
      result = as.data.frame(forecast(fit_arima, 90))
      if (exp_forecast_arima) result = exp(result)
      
      result = list(forecast = result, accuracy = accuracy_arima)
      
    }, error = function(err) {
      print(err)
    })
    
  }
  
  result
  
})

ok <- !sapply(result_arima, is.null)
result_arima <- result_arima[ok]
names_arima <- names(dailyData_split)[ok]

forecast_arima <- lapply(result_arima, `[[`, 1)
names_forecast_arima <- lapply(1:length(forecast_arima), function(i) {
  paste(names_arima[i], colnames(forecast_arima[[i]]), sep = " - ")
})
forecast_arima <- do.call(cbind, forecast_arima)
forecast_arima <- data.frame(Date = future_dates, forecast_arima)
colnames(forecast_arima) = c("Date", do.call(c, names_forecast_arima))

accuracy_arima <- lapply(result_arima, `[[`, 2)
accuracy_arima <- do.call(rbind, accuracy_arima)
accuracy_arima <- data.frame(combination = names_arima, MAPE = accuracy_arima, stringsAsFactors = FALSE)


result_arima2 <- lapply(prepared_data_split, function(vol_data) {
  
  result = NULL
  
  if (!is.null(vol_data)) {
    
    log_vol_data = log(vol_data)
    exp_forecast_arima2 = FALSE
    
    tryCatch({
      
        fit_arima2 = arima(vol_data, order = c(2, 1, 7))
        test_arima2 = Box.test(fit_arima2$residuals, type = "Ljung-Box")
        if (test_arima2$p.value <= 0.05) {
          fit_arima2 = arima(log_vol_data, order = c(2, 1, 7))
          exp_forecast_arima2 = TRUE
        }
        accuracy_arima2 = accuracy(fit_arima2)
        accuracy_arima2 = accuracy_arima2[colnames(accuracy_arima2) == "MAPE"]
        
        result = as.data.frame(forecast(fit_arima2, 90))
        if (exp_forecast_arima2) result = exp(result)
        
        result = list(forecast = result, accuracy = accuracy_arima2)
      
    }, error = function(err) {
      print(err)
    })
    
  }
  
  result
  
})

ok <- !sapply(result_arima2, is.null)
result_arima2 <- result_arima2[ok]
names_arima2 <- names(dailyData_split)[ok]

forecast_arima2 <- lapply(result_arima2, `[[`, 1)
names_forecast_arima2 <- lapply(1:length(forecast_arima2), function(i) {
  paste(names_arima2[i], colnames(forecast_arima2[[i]]), sep = " - ")
})
forecast_arima2 <- do.call(cbind, forecast_arima2)
forecast_arima2 <- data.frame(Date = future_dates, forecast_arima2)
colnames(forecast_arima2) = c("Date", do.call(c, names_forecast_arima2))

accuracy_arima2 <- lapply(result_arima2, `[[`, 2)
accuracy_arima2 <- do.call(rbind, accuracy_arima2)
accuracy_arima2 <- data.frame(combination = names_arima2, MAPE = accuracy_arima2, stringsAsFactors = FALSE)


result_lm <- lapply(prepared_data_split, function(vol_data) {
  
  result = NULL
  
  if (!is.null(vol_data)) {
    
    log_vol_data = log(vol_data)
    exp_forecast_lm = FALSE
    
    tryCatch({
      
        data_lm = data.frame(value = vol_data, date = time(vol_data))
        fit_lm = lm(value ~ date, data = data_lm)
        test_lm = gvlma(fit_lm)
        if (test_lm$GlobalTest$GlobalStat4$pvalue <= 0.05) {
          data_lm = data.frame(value = log_vol_data, date = time(log_vol_data))
          fit_lm = lm(value ~ date, data = data_lm)
          exp_forecast_lm = TRUE
        }
        accuracy_lm = accuracy(fit_lm)
        accuracy_lm = accuracy_lm[colnames(accuracy_lm) == "MAPE"]
        
        result = as.data.frame(forecast(fit_lm, newdata = data.frame(date = time(vol_data)[length(vol_data)]+(1:90)*(time(vol_data)[length(vol_data)]-time(vol_data)[length(vol_data)-1]))))
        if (exp_forecast_lm) result = exp(result)
        
        result = list(forecast = result, accuracy = accuracy_lm)
      
    }, error = function(err) {
      print(err)
    })
    
  }
  
  result
  
})

ok <- !sapply(result_lm, is.null)
result_lm <- result_lm[ok]
names_lm <- names(dailyData_split)[ok]

forecast_lm <- lapply(result_lm, `[[`, 1)
names_forecast_lm <- lapply(1:length(forecast_lm), function(i) {
  paste(names_lm[i], colnames(forecast_lm[[i]]), sep = " - ")
})
forecast_lm <- do.call(cbind, forecast_lm)
forecast_lm <- data.frame(Date = future_dates, forecast_lm)
colnames(forecast_lm) = c("Date", do.call(c, names_forecast_lm))

accuracy_lm <- lapply(result_lm, `[[`, 2)
accuracy_lm <- do.call(rbind, accuracy_lm)
accuracy_lm <- data.frame(combination = names_lm, MAPE = accuracy_lm, stringsAsFactors = FALSE)


result_HW <- lapply(prepared_data_split, function(vol_data) {
  
  result = NULL
  
  if (!is.null(vol_data)) {
    
    log_vol_data = log(vol_data)
    exp_forecast_HW = FALSE
    
    tryCatch({
      
        fit_HW = ets(vol_data, model = "ZZZ")
        accuracy_HW = accuracy(fit_HW)
        accuracy_HW = accuracy_HW[colnames(accuracy_HW) == "MAPE"]
        
        result = as.data.frame(forecast(fit_HW, 90))
        if (exp_forecast_HW) result = exp(result)
        
        result = list(forecast = result, accuracy = accuracy_HW)
      
    }, error = function(err) {
      print(err)
    })
    
  }
  
  result
  
})

ok <- !sapply(result_HW, is.null)
result_HW <- result_HW[ok]
names_HW <- names(dailyData_split)[ok]

forecast_HW <- lapply(result_HW, `[[`, 1)
names_forecast_HW <- lapply(1:length(forecast_HW), function(i) {
  paste(names_HW[i], colnames(forecast_HW[[i]]), sep = " - ")
})
forecast_HW <- do.call(cbind, forecast_HW)
forecast_HW <- data.frame(Date = future_dates, forecast_HW)
colnames(forecast_HW) = c("Date", do.call(c, names_forecast_HW))

accuracy_HW <- lapply(result_HW, `[[`, 2)
accuracy_HW <- do.call(rbind, accuracy_HW)
accuracy_HW <- data.frame(combination = names_HW, MAPE = accuracy_HW, stringsAsFactors = FALSE)


#merge accuracies and choose best
combinations <- intersect(intersect(accuracy_arima$combination, accuracy_arima2$combination),
                          intersect(accuracy_lm$combination, accuracy_HW$combination))
accuracy <- accuracy_arima %>% filter(combination %in% combinations) %>% rename(MAPE_arima = MAPE) %>% 
  left_join(accuracy_arima2 %>% filter(combination %in% combinations) %>% rename(MAPE_arima2 = MAPE)) %>% 
  left_join(accuracy_lm %>% filter(combination %in% combinations) %>% rename(MAPE_lm = MAPE)) %>% 
  left_join(accuracy_HW %>% filter(combination %in% combinations) %>% rename(MAPE_HW = MAPE))

accuracy$best_accuracy = apply(accuracy[, 2:5], 1, min, na.rm = TRUE)
accuracy$best_model = c("arima", "arima2", "lm", "HW")[apply(accuracy[, 2:5], 1, which.min)]


forecast <- lapply(1:nrow(accuracy), function(i) {
  
  comb <- paste0(accuracy$combination[i], " - ")
  res <- switch(accuracy$best_model[i],
                "arima" = forecast_arima[, grepl(comb, colnames(forecast_arima))],
                "arima2" = forecast_arima2[, grepl(comb, colnames(forecast_arima2))],
                "lm" = forecast_lm[, grepl(comb, colnames(forecast_lm))],
                "HW" = forecast_HW[, grepl(comb, colnames(forecast_HW))])
  res
  
})

forecast <- do.call(cbind, forecast)
forecast <- data.frame(Date = future_dates, forecast, check.names = FALSE)


##Saving output to file
write.csv(forecast_arima, "forecast_arima.csv", row.names = FALSE, quote = FALSE)
#write.csv(accuracy_arima, "accuracy_arima.csv", row.names = FALSE, quote = FALSE)
write.csv(forecast_arima2, "forecast_arima2.csv", row.names = FALSE, quote = FALSE)
#write.csv(accuracy_arima2, "accuracy_arima2.csv", row.names = FALSE, quote = FALSE)
write.csv(forecast_lm, "forecast_lm.csv", row.names = FALSE, quote = FALSE)
#write.csv(accuracy_lm, "accuracy_lm.csv", row.names = FALSE, quote = FALSE)
write.csv(forecast_HW, "forecast_HW.csv", row.names = FALSE, quote = FALSE)
#write.csv(accuracy_HW, "accuracy_HW.csv", row.names = FALSE, quote = FALSE)

write.csv(forecast, "task2_forecast.csv", row.names = FALSE, quote = FALSE)
write.csv(accuracy, "task2_accuracy.csv", row.names = FALSE, quote = FALSE)
