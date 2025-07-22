import pandas as pd
import requests
from bs4 import BeautifulSoup
import time

# URL de base des matchs de la saison 2023-2024
base_url = "https://www.basketball-reference.com"
schedule_url = "https://www.basketball-reference.com/leagues/NBA_2024_games-{}.html"

# Mois de la saison
months = ["october", "november", "december", "january", "february", "march", "april"]

# Liste pour stocker les liens des matchs
game_links = []

# Étape 1 : Récupérer tous les liens des matchs de la saison
for month in months:
    url = schedule_url.format(month)
    response = requests.get(url)
    soup = BeautifulSoup(response.content, "html.parser")
    
    # Trouver le tableau des matchs
    table = soup.find("table", {"id": "schedule"})
    if table:
        rows = table.find_all("tr")
        for row in rows:
            link = row.find("a", text="Box Score")
            if link:
                game_links.append(base_url + link["href"])

# Liste pour stocker toutes les stats des matchs
all_games_data = []

# Étape 2 : Scraper les stats pour chaque match
for game_url in game_links:
    response = requests.get(game_url)
    soup = BeautifulSoup(response.content, "html.parser")

    # Trouver le titre du match (ex: "Denver Nuggets vs. Los Angeles Lakers")
    title = soup.find("h1").text.strip()

    # Récupérer la date du match
    date = soup.find("div", {"class": "scorebox"}).find("div").text.strip()

    # Trouver les tableaux des joueurs
    for table in soup.find_all("table"):
        if "basic" in table["id"]:  # Stats des joueurs (pas des équipes)
            df = pd.read_html(str(table))[0]

            # Ajouter des infos au DataFrame
            df["Match"] = title
            df["Date"] = date

            # Ajouter au dataset final
            all_games_data.append(df)

    # Pause pour éviter d'être bloqué par le site
    time.sleep(2)

# Fusionner tous les matchs en un seul DataFrame
df_all = pd.concat(all_games_data, ignore_index=True)

# Sauvegarde en CSV
df_all.to_csv("/Users/icesim/Desktop/nba_2023_2024_match_stats.csv", index=False)

print("Données récupérées et sauvegardées dans nba_2023_2024_match_stats.csv")
