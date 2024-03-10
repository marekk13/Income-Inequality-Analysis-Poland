library(readxl)


transform_data <- function(data){
  if ( !all(sapply(data, is.numeric) == TRUE) ){
    stop("Argument funkcji musi być listą z wektorami liczb rzeczywistych!")
  }
  added_vec <- c()
  for (i in 1:length(data)){
    temp <- rep(data[[i]][1], times=data[[i]][2])
    added_vec <- c(added_vec, temp)
  }
  sorted_data <- sort(added_vec)
  return(sorted_data)
}

calculate_lorenz_curve <- function(data) {
  data_new <- transform_data(data)
  
  cumulative_share <- cumsum(data_new) / sum(data_new)
  lorenz_points <- c(0, cumulative_share)
  
  return(lorenz_points)
}

calculate_gini_coefficient <- function(data) {
  data_new <- transform_data(data)
  n <- length(data_new)
  gini_coefficient <- (2 * sum(data_new * seq_along(data_new)) / (n * sum(data_new))) - (n + 1) / n
  return(gini_coefficient)
}

plot_lorenz_curve <- function(lorenz_points){
  plot(c(0, 1), c(0, 1), type = "n", col = "white", xlab = "Kumulacyjny udział populacji", ylab = "Kumulacyjny udział np. w dochodach", main = "Krzywa Lorenza")
  abline(v = seq(0, 1, 0.1), h = seq(0, 1, 0.1), col = "lightgray")
  
  lines(seq_along(lorenz_points)/length(lorenz_points), lorenz_points, type = "l", col = "blue", lwd = 2)
  
  abline(0, 1, col = "red", lwd = 2)
  
  legend("bottomright", legend = c("Linia doskonałej równości", "Krzywa Lorenza"), col = c("red", "blue"), lwd = 2)
}


# Przykładowy wektor par liczb rzeczywistych
data <- list(c(10,2), c(15,5), c(2,9), c(7,11), c(11,9), c(12,6), c(5, 6), c(1,3))

lorenz_curve <- calculate_lorenz_curve(data)
gini_coefficient <- calculate_gini_coefficient(data)

print("Punkty krzywej Lorenza dla przykładowych danych:")
print(lorenz_curve)

print("Współczynnik Giniego dla przykładowych danych:")
print(gini_coefficient)

plot_lorenz_curve(lorenz_curve)


#dane dotyczące zarobków PolakóW - przyjmujemy środek przedziału z pliku tablice_struktura_wynagrodzen_wedlug_zawodow_w_pazdzierniku_2020_r-1
#aby jako xi były wartości liczbowe - dla najniższego przedziału przyjmujemy 2000, dla ostatniego przedziału - 25000
#(xi, y1) - (środek przedziału, pracownicy ogółem w tym przedziale w tysiącach)
zarobki <- list(c(2000, 642), c(2737, 454), c(3348, 1677), c(4081, 742), c(5028, 1857), c(6467, 1163), c(7904, 632), c(9340, 341), c(10778, 194), c(12215, 122), c(13651, 85), c(15088, 64), c(16525, 46), c(18622, 60), c(25000, 105))
lorenz_curve1 <- calculate_lorenz_curve(zarobki)
gini_coefficient1 <- calculate_gini_coefficient(zarobki)

print("Punkty krzywej Lorenza dla danych o zarobkach Polaków:")
print(lorenz_curve1)

print("Współczynnik Giniego dla danych o zarobkach Polaków:")
print(gini_coefficient1)

plot_lorenz_curve(lorenz_curve1)


#ponizej dane z Gini Coeffficients różnych państw ze strony worldbank
#obliczony współczynnik Giniego niewiele się różni od tego z pliku - 0.297 do 0.288
dane <- read_excel('API_SI.POV.GINI_DS2_en_excel_v2_5728952.xls')
dane <- na.omit(dane[, c('Data Source', '...64')])[-1,] #bierzemy kolumnę z rokiem 2019 (ostatni rok z danymi dla Polski), pomijamy NA oraz wiersz z rokiem (2019)
dane$'...64' <- as.numeric(dane$'...64')
wartosc <- as.numeric(dane[dane['Data Source'] == 'Poland', '...64'])

procentyl <- sum(dane$'...64' > wartosc) / length(dane$'...64')
#Polska ma lepszy współczynnik Giniego od około 83% badanych państw w roku 2019, co jest bardzo dobrym wynikiem.

