# Na codzień zaowodowo programuje narzędzia dla inwestorów kryptowalut a także sam inwestuje i interesuję się rynkiem.
# Z tego też powodu, będę chciał przeprowadzić analizę zależności kilki wskaźników na cenę Bitcoina.
# Skupię się głownie na wskaźnikach presji zakupowej i sprzedażowej (wskaźniki BSP).
# Z racji, iż są to dane, które noramlnie są dostępne po zakupie dostępu, prosiłbym o nie rozpowszechnianie danych (chocaż wiem, że w samym pliku HTML ich nie ma).

# Wczytanie paczek
library(dplyr)
library(ggplot2)
library(jsonlite)
library(lubridate)
library(zoo)

# Wczytywanie pliku JSON
#data <- fromJSON("data-crypto-format2.json")
data <- fromJSON("data-crypto-format2-short.json")

# Sprawdzenie struktury danych
head(data)
#str(data)
#summary(data)

# Usunięcie brakujących danych (jeśli są)
data_clean <- na.omit(data)

# Dodanie nowej kolumny bsp_avg
data_clean <- data_clean %>%
  mutate(bsp_avg = rowMeans(select(., starts_with("ob_10_p_")), na.rm = TRUE))

head(data_clean)


# Obliczanie skali przekształcającej dane BSP na zakres cen
scaleFactor <- (max(data_clean$h, na.rm = TRUE) - min(data_clean$l, na.rm = TRUE)) / 2

# Tworzenie wykresu
ggplot(data_clean, aes(x = ts)) +
  # Linia dla BSP Avg
  geom_line(aes(y = bsp_avg, color = "Metryka Order Book")) +
  # Linia dla ceny wysokiej, przeskalowanej
  geom_line(aes(y = (h - min(data_clean$l, na.rm = TRUE)) / scaleFactor, color = "Najwyższa cena"), linetype = "dashed") +
  # Linia dla ceny niskiej, przeskalowanej
  geom_line(aes(y = (l - min(data_clean$l, na.rm = TRUE)) / scaleFactor, color = "Najniższa cena"), linetype = "dashed") +
  scale_color_manual(values = c("Metryka Order Book" = "blue", "Najwyższa cena" = "green", "Najniższa cena" = "red")) +
  # Ustawienie pierwszej osi Y dla BSP Avg
  scale_y_continuous(
    name = "Metryka Order Book",
    limits = c(-0.5, 2),
    sec.axis = sec_axis(
      trans = ~ . * scaleFactor + min(data_clean$l, na.rm = TRUE),
      name = "Cena"
    )
  ) +
  labs(title = "Metryka Order Book, najwyższa i najniższa cena  w danym interwale", x = "Timestamp") +
  theme(
    axis.title.y.left = element_text(color = "blue"),
    axis.text.y.left = element_text(color = "blue"),
    axis.title.y.right = element_text(color = "green"),
    axis.text.y.right = element_text(color = "green"),
    legend.position = "bottom"
  )

# Ustawienie zakresu (np. 100 minut wstecz)
X <- 1000

# Wyliczenie min_bsp z uwzględnieniem niepełnych okien
data_clean <- data_clean %>%
  mutate(min_bsp = rollapply(
    ob_10_p_l, 
    width = X, 
    FUN = min, 
    align = "right", 
    partial = TRUE, # Uwzględnianie niepełnych okien
    na.rm = TRUE    # Ignorowanie braków danych
  ))

# Podgląd danych
head(data_clean)

# Wykres metryki BSP i minimum
ggplot(data_clean, aes(x = ts)) +
  geom_line(aes(y = ob_10_p_l, color = "Metryka BSP (ob_10_p_l)")) +
  geom_line(aes(y = min_bsp, color = "Wartości minimum (min_bsp)"), linetype = "dashed") +
  scale_color_manual(values = c("Metryka BSP (ob_10_p_l)" = "blue", "Wartości minimum (min_bsp)" = "red")) +
  labs(
    title = "Metryka BSP i wartości minimum",
    x = "Timestamp",
    y = "Wartość"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Zaktualizowany wykres
ggplot(data_clean, aes(x = ts)) +
  # Linia dla BSP Avg
  geom_line(aes(y = bsp_avg, color = "Metryka Order Book (BSP Avg)")) +
  # Linia dla ceny wysokiej, przeskalowanej
  geom_line(aes(y = (h - min(data_clean$l, na.rm = TRUE)) / scaleFactor, color = "Najwyższa cena"), linetype = "dashed") +
  # Linia dla ceny niskiej, przeskalowanej
  geom_line(aes(y = (l - min(data_clean$l, na.rm = TRUE)) / scaleFactor, color = "Najniższa cena"), linetype = "dashed") +
  # Linia dla minimum BSP
  geom_line(aes(y = min_bsp, color = "Wartości minimum BSP"), linetype = "dotted") +
  scale_color_manual(values = c(
    "Metryka Order Book (BSP Avg)" = "blue", 
    "Najwyższa cena" = "green", 
    "Najniższa cena" = "red", 
    "Wartości minimum BSP" = "purple"
  )) +
  # Ustawienie pierwszej osi Y dla BSP Avg
  scale_y_continuous(
    name = "Metryka Order Book",
    limits = c(-0.5, 2),
    sec.axis = sec_axis(
      trans = ~ . * scaleFactor + min(data_clean$l, na.rm = TRUE),
      name = "Cena"
    )
  ) +
  labs(
    title = "Porównanie metryki Order Book, wartości minimum i ceny",
    x = "Timestamp"
  ) +
  theme(
    axis.title.y.left = element_text(color = "blue"),
    axis.text.y.left = element_text(color = "blue"),
    axis.title.y.right = element_text(color = "green"),
    axis.text.y.right = element_text(color = "green"),
    legend.position = "bottom"
  )

# Wykrycie punktów, w których ob_10_p_l < min_bsp
data_clean <- data_clean %>%
  mutate(below_min = ifelse(ob_10_p_l <= min_bsp, TRUE, FALSE))

# Wyodrębnienie timestampów i wartości dla punktów, gdzie below_min = TRUE
points_below_min <- data_clean %>%
  filter(below_min) %>%
  select(ts, ob_10_p_l)

points_below_min
nrow(points_below_min)

# Wykres BSP z punktami przecięcia
ggplot(data_clean, aes(x = ts)) +
  geom_line(aes(y = ob_10_p_l, color = "Metryka BSP (ob_10_p_l)")) +
  geom_line(aes(y = min_bsp, color = "Wartości minimum (min_bsp)"), linetype = "dashed") +
  geom_point(data = points_below_min, aes(x = ts, y = ob_10_p_l, color = "Punkty przecięcia"), size = 2) +
  scale_color_manual(values = c(
    "Metryka BSP (ob_10_p_l)" = "blue",
    "Wartości minimum (min_bsp)" = "red",
    "Punkty przecięcia" = "black"
  )) +
  labs(
    title = "Punkty przecięcia metryki BSP z wartościami minimum",
    x = "Timestamp",
    y = "Wartość"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Funkcja do filtrowania punktów na podstawie minimalnego odstępu czasowego
filter_points <- function(data, min_gap) {
  # Dodanie kolumny oznaczającej wybrane punkty
  data <- data %>%
    mutate(selected = FALSE)
  
  # Przechowywanie czasu ostatniego wybranego punktu
  last_valid_time <- -Inf
  
  for (i in seq_len(nrow(data))) {
    if (data$below_min[i]) {
      # Sprawdzenie odstępu czasowego
      if ((data$ts[i] - last_valid_time) >= min_gap * 60) {
        data$selected[i] <- TRUE
        last_valid_time <- data$ts[i] # Aktualizacja ostatniego wybranego punktu
      }
    }
  }
  
  # Zwracanie tylko wybranych punktów
  data %>% filter(selected)
}

# Parametr minimalnego odstępu czasowego (w minutach)
min_gap <- 2 # Możesz zmieniać ten parametr

# Filtrowanie punktów
filtered_points <- filter_points(data_clean, min_gap)

# Liczba wybranych punktów
cat("Liczba wybranych punktów po filtracji:", nrow(filtered_points), "\n")

# Aktualizacja wykresu P z wybranymi punktami
ggplot(data_clean, aes(x = ts)) +
  # Linia dla metryki ob_10_p_l
  geom_line(aes(y = ob_10_p_l, color = "ob_10_p_l")) +
  # Linia dla min_bsp
  geom_line(aes(y = min_bsp, color = "min_bsp")) +
  # Wybrane punkty po filtracji
  geom_point(
    data = filtered_points,
    aes(x = ts, y = ob_10_p_l),
    color = "green", size = 2, shape = 21, fill = "yellow"
  ) +
  scale_color_manual(values = c("ob_10_p_l" = "blue", "min_bsp" = "red")) +
  labs(
    title = "Metryka ob_10_p_l z zaznaczonymi punktami po filtracji",
    x = "Timestamp", y = "Wartość"
  ) +
  theme_minimal()

# Zaktualizowany wykres z filtracją punktów
ggplot(data_clean, aes(x = ts)) +
  # Linia dla BSP Avg
  geom_line(aes(y = bsp_avg, color = "Metryka Order Book (BSP Avg)")) +
  # Linia dla ceny wysokiej, przeskalowanej
  geom_line(aes(y = (h - min(data_clean$l, na.rm = TRUE)) / scaleFactor, color = "Najwyższa cena"), linetype = "dashed") +
  # Linia dla ceny niskiej, przeskalowanej
  geom_line(aes(y = (l - min(data_clean$l, na.rm = TRUE)) / scaleFactor, color = "Najniższa cena"), linetype = "dashed") +
  # Linia dla minimum BSP
  geom_line(aes(y = min_bsp, color = "Wartości minimum BSP"), linetype = "dotted") +
  # Dodanie punktów wybranych po filtracji
  geom_point(
    data = filtered_points,
    aes(x = ts, y = ob_10_p_l),
    color = "green", size = 2, shape = 21, fill = "yellow"
  ) +
  scale_color_manual(values = c(
    "Metryka Order Book (BSP Avg)" = "blue", 
    "Najwyższa cena" = "green", 
    "Najniższa cena" = "red", 
    "Wartości minimum BSP" = "purple"
  )) +
  # Ustawienie pierwszej osi Y dla BSP Avg
  scale_y_continuous(
    name = "Metryka Order Book",
    limits = c(-0.5, 2),
    sec.axis = sec_axis(
      trans = ~ . * scaleFactor + min(data_clean$l, na.rm = TRUE),
      name = "Cena"
    )
  ) +
  labs(
    title = "Porównanie metryki Order Book, wartości minimum i ceny",
    x = "Timestamp"
  ) +
  theme(
    axis.title.y.left = element_text(color = "blue"),
    axis.text.y.left = element_text(color = "blue"),
    axis.title.y.right = element_text(color = "green"),
    axis.text.y.right = element_text(color = "green"),
    legend.position = "bottom"
  )

# Parametry
X <- 20  # Czas w minutach dla obliczenia procentowej zmiany ceny
Y_trail <- 2  # Procent spadku do wyjścia (Trailing stop loss)
Y_fixed <- 10  # Procent wzrostu do wyjścia (Fixed target)
timestamp_col <- "ts"
price_col_low <- "l"  # Cena wejścia na podstawie 'low'
price_col_high <- "h"  # Cena do analizy wzrostu/spadku na podstawie 'high'

# Utworzenie nowej tabeli do przechowywania transakcji
transactions <- data.frame(
  ts_entry = integer(),
  price_entry = numeric(),
  price_change_percent = numeric(),
  ts_exit_trail = integer(),
  price_exit_trail = numeric(),
  gain_loss_trail = numeric(),
  ts_exit_fixed = integer(),
  price_exit_fixed = numeric(),
  gain_loss_fixed = numeric(),
  stringsAsFactors = FALSE
)

# Funkcja do obliczenia procentowej zmiany ceny
calculate_percentage_change <- function(current_price, past_price) {
  return((current_price - past_price) / past_price * 100)
}

# Funkcja do trailing stop loss
calculate_trailing_exit <- function(entry_price, prices) {
  max_price <- entry_price
  for (price in prices) {
      print(paste("Current Price:", price, "Max Price:", max_price))
      if (price > max_price) {
        max_price <- price
      }
    if (price <= max_price * (1 - Y_trail / 100)) {
      return(price)
    }
  }
  return(NA)  # Jeśli nie osiągnięto warunków trailing stop loss
}

# Funkcja do wyjścia przy osiągnięciu wzrostu
calculate_fixed_exit <- function(entry_price, prices) {
  for (price in prices) {
    if (price >= entry_price * (1 + Y_fixed / 100)) {
      return(price)
    }
  }
  return(NA)  # Jeśli nie osiągnięto wzrostu
}

# Iteracja przez momenty kupna
for (i in 1:nrow(points_below_min)) {
  entry_ts <- points_below_min$ts[i]
  entry_price <- points_below_min$l[i]  # Używamy ceny niskiej (low) do wejścia
  
  # Procentowa zmiana ceny
  past_ts <- entry_ts - X * 60  # X minut wstecz
  past_price <- data_clean$l[which.min(abs(data_clean$ts - past_ts))]  # Cena 'low' z X minut wstecz
  price_change_percent <- calculate_percentage_change(entry_price, past_price)
  
  # Znalezienie momentu wyjścia (Trailing Stop Loss)
  prices_after_entry <- data_clean$h[data_clean$ts >= entry_ts]  # Analizujemy ceny 'high' po wejściu
  exit_trail <- calculate_trailing_exit(entry_price, prices_after_entry)
  gain_loss_trail <- if (!is.na(exit_trail)) calculate_percentage_change(exit_trail, entry_price) else NA
  
  # Znalezienie momentu wyjścia (Fixed Target)
  exit_fixed <- calculate_fixed_exit(entry_price, prices_after_entry)
  gain_loss_fixed <- if (!is.na(exit_fixed)) calculate_percentage_change(exit_fixed, entry_price) else NA
  
  # Dodanie nowej transakcji do tabeli
  transactions <- rbind(transactions, data.frame(
    ts_entry = entry_ts,
    price_entry = entry_price,
    price_change_percent = price_change_percent,
    ts_exit_trail = ifelse(is.na(exit_trail), NA, entry_ts + which.min(abs(prices_after_entry - exit_trail)) * 60),
    price_exit_trail = exit_trail,
    gain_loss_trail = gain_loss_trail,
    ts_exit_fixed = ifelse(is.na(exit_fixed), NA, entry_ts + which.min(abs(prices_after_entry - exit_fixed)) * 60),
    price_exit_fixed = exit_fixed,
    gain_loss_fixed = gain_loss_fixed
  ))
}

# Wyświetlenie wyników
print(transactions)

