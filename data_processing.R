library(xlsx)
library(lfl)
library(dplyr)
library(ggplot2)

dane_kwestionariusz = read.xlsx(file = "raw_data/dane_kwestionariuszowe.xlsx", sheetIndex = 1, header = T)
dane_obserwacyjne = data.table::fread( "raw_data/dane_obserwacyjne.csv", data.table = FALSE )

sredni_czas_eksponatu = dane_obserwacyjne %>%
  group_by(ekspot) %>%
  summarise(sr_cz_eksponatu = mean(czas_w_sek))



dane_obserwacyjne = dane_obserwacyjne %>%
  left_join(sredni_czas_eksponatu) %>%
  mutate(roznica_czasu = czas_w_sek - sr_cz_eksponatu, wzgl_roznica_czasu = (czas_w_sek - sr_cz_eksponatu)/sr_cz_eksponatu)

sredni_czas_spedzania_wzgledny = dane_obserwacyjne %>%
  group_by(ID) %>%
  summarise(srednia_wzgl_roznica = mean(wzgl_roznica_czasu))

typy_interakcji = dane_obserwacyjne %>%
  group_by(ID) %>%
  summarise(patrzenie = sum(zach >= 1, na.rm = T),
            dotykanie = sum(zach >= 2, na.rm = T),
            uzywanie = sum(zach >= 3, na.rm = T),
            eksperymentowanie = sum(zach >= 4, na.rm = T))

dane_dzieci = dane_kwestionariusz %>%
  left_join(sredni_czas_spedzania_wzgledny) %>%
  left_join(typy_interakcji)


plot(dane_dzieci$studiaM, dane_dzieci$srednia_wzgl_roznica)

ggplot(dane_dzieci, aes(x = as.factor(NR_szkoły), y = srednia_wzgl_roznica)) +
  geom_boxplot()
# tu widać że szkoła ma wpływ na średni czas spedzony przy eksponatach


