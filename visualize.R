library(ggplot2)
library(dplyr)
library(tidyr)

# Données fictives (mais cohérentes) — Tu peux remplacer par des données réelles si dispo
equipes <- c("Arsenal", "Aston Villa", "Bournemouth", "Brentford", "Brighton", "Chelsea",
             "Crystal Palace", "Everton", "Fulham", "Ipswich Town", "Leicester City",
             "Liverpool", "Manchester City", "Manchester Utd", "Newcastle Utd",
             "Nott'ham Forest", "Southampton", "Tottenham", "West Ham", "Wolves")

buts_dom <- c(211, 181, 181, 176, 168, 171, 169, 157, 161, 138, 142, 189, 162, 150, 145, 138, 155, 161, 150, 157)
buts_ext <- c(181, 176, 181, 176, 168, 171, 169, 157, 161, 138, 142, 189, 162, 150, 145, 138, 155, 161, 150, 157)
buts_encaisses <- c(120, 150, 180, 160, 165, 170, 140, 155, 158, 142, 130, 125, 110, 145, 135, 168, 160, 150, 155, 140)

# Créer un data frame
df <- data.frame(Équipe = equipes,
                 Buts_dom = buts_dom,
                 Buts_ext = buts_ext,
                 Buts_encaissés = buts_encaisses)

# -------- Premier graphique : barres buts dom/ext --------
df_long <- df %>%
  pivot_longer(cols = c(Buts_dom, Buts_ext), names_to = "Lieu", values_to = "Buts")

p1 <- ggplot(df_long, aes(x = reorder(Équipe, -Buts), y = Buts, fill = Lieu)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Buts marqués à domicile et à l'extérieur par équipe",
       x = "Équipe", y = "Nombre de buts") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# -------- Deuxième graphique : scatter plot attaque/défense --------
df <- df %>%
  mutate(Total_buts = Buts_dom + Buts_ext)

p2 <- ggplot(df, aes(x = Total_buts, y = Buts_encaissés, label = Équipe)) +
  geom_point(aes(color = Total_buts - Buts_encaissés), size = 4) +
  geom_text(vjust = -1, size = 3) +
  scale_color_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0) +
  labs(title = "Comparaison attaque/défense : Buts marqués vs encaissés",
       x = "Total des buts marqués",
       y = "Buts encaissés",
       color = "Diff. buts") +
  theme_minimal()

# -------- Afficher les deux graphiques --------
# Besoin de gridExtra pour afficher les deux ensemble
install.packages("gridExtra")
library(gridExtra)

grid.arrange(p1, p2, nrow = 2)