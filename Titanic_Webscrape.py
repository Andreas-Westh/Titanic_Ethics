import os
import browser_cookie3
import requests
import pandas as pd
from bs4 import BeautifulSoup
from dotenv import load_dotenv

# load .env file
load_dotenv(".env")
username = os.getenv("titanica_username")
password = os.getenv("titanica_password")

# Get cookies from my Zen Browser
cookies = browser_cookie3.firefox()

# Url
url = "https://www.encyclopedia-titanica.org/titanic-passenger-list/"

# Headers
headers = {
    "User-Agent": "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/133.0.0.0 Safari/537.36"
}

# Send GET
response = requests.get(url, headers=headers,cookies=cookies)

# parse HTML
soup = BeautifulSoup(response.text, "html.parser")

# Part 1: Scrape names, links and type (victim or survivor)
data = []  # List to store passenger data
proceed = True  # Control variable

while proceed:  # Always True since there is only one page, but follows guide structure
    print("Scraping Titanic passenger list...")

    url = "https://www.encyclopedia-titanica.org/titanic-passenger-list/"
    page = requests.get(url, headers=headers, cookies=cookies)  # Send request

    soup = BeautifulSoup(page.text, "html.parser")

    all_passengers = soup.find_all("tr")  # Find all table rows (each passenger)

    for passenger in all_passengers:
        item = {}  # Create a dictionary for each passenger

        # Extract name from <span class="fn">
        name_tag = passenger.select_one("span.fn")
        item["Name"] = name_tag.get_text(strip=True) if name_tag else "Unknown"

        # Extract profile link from <a itemprop="url">
        link_tag = passenger.select_one("a[itemprop='url']")
        if link_tag:
            relative_link = link_tag.get("href")
            item["Link"] = f"https://www.encyclopedia-titanica.org{relative_link}"
            # Determine type based on link
            item["Type"] = "titanic-victim" if "/titanic-victim/" in relative_link else "titanic-survivor"
        else:
            item["Link"] = None
            item["Type"] = None

        data.append(item)  # Append passenger data

    proceed = False  # Since there is only one page, stop after first iteration

print("Scraping complete.")
# Convert to df
df = pd.DataFrame(data)
print(df.head)
# Save it
df.to_csv("titanic_passengers.csv")
