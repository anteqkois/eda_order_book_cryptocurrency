# Na codzień zaowodowo programuje narzędzia dla inwestorów kryptowalut a także sam inwestuje i interesuję się rynkiem.
# Z tego też powodu, będę chciał przeprowadzić analizę zależności kilki wskaźników na cenę Bitcoina.
# Skupię się głownie na wskaźnikach presji zakupowej i sprzedażowej (wskaźniki BSP).
# Z racji, iż są to dane, które noramlnie są dostępne po zakupie dostępu, prosiłbym o nie rozpowszechnianie danych (chocaż wiem, że w samym pliku HTML ich nie ma).

# Wczytanie paczek
library(dplyr)
library(ggplot2)
library(jsonlite)

# Wczytywanie pliku JSON
data <- fromJSON("data-crypto-format2.json")

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
  geom_line(aes(y = bsp_avg, color = "BSP Avg")) +
  # Linia dla ceny wysokiej, przeskalowanej
  geom_line(aes(y = (h - min(data_clean$l, na.rm = TRUE)) / scaleFactor, color = "High Price"), linetype = "dashed") +
  # Linia dla ceny niskiej, przeskalowanej
  geom_line(aes(y = (l - min(data_clean$l, na.rm = TRUE)) / scaleFactor, color = "Low Price"), linetype = "dashed") +
  scale_color_manual(values = c("BSP Avg" = "blue", "High Price" = "green", "Low Price" = "red")) +
  # Ustawienie pierwszej osi Y dla BSP Avg
  scale_y_continuous(
    name = "BSP Avg",
    limits = c(-0.5, 2),
    sec.axis = sec_axis(
      trans = ~ . * scaleFactor + min(data_clean$l, na.rm = TRUE),
      name = "Price (High / Low)"
    )
  ) +
  labs(title = "BSP Avg vs Price (High and Low)", x = "Timestamp") +
  theme(
    axis.title.y.left = element_text(color = "blue"),
    axis.text.y.left = element_text(color = "blue"),
    axis.title.y.right = element_text(color = "green"),
    axis.text.y.right = element_text(color = "green"),
    legend.position = "bottom"
  )

