import requests
from bs4 import BeautifulSoup
import pandas as pd

# Trying to webscrape in Python for the first time, currently not related to Titanic


# stop at last page (here it gives a page with title: "404 Not Found")
current_page = 1

data = []

proceed = True

while(proceed): # Same as proceed == True
    print("Currently scraping page: "+str(current_page))
    
    url = "https://books.toscrape.com/catalogue/page-"+str(current_page)+".html"

    page = requests.get(url)

    soup = BeautifulSoup(page.text, "html.parser")
    
    if soup.title.text == "404 Not Found":
        proceed = False
    else:
        all_books = soup.find_all("li",class_="col-xs-6 col-sm-4 col-md-3 col-lg-3")
    
        for book in all_books:
            item = {}  # creates a dictionary
            
            item['Title'] = book.find("img").attrs["alt"] # Where the title of the book can be found
            
            item['Link'] = "https://books.toscrape.com/catalogue/"+book.find("a").attrs["href"] # Book link
            
            item['Price'] = book.find("p", class_="price_color").text[2:]# with .text you only get the contents of the element
            # [2:] skips the first 2 charecters 
            item['Stock'] = book.find("p", class_="instock availability").text.strip()
            
            data.append(item) # appends the dictionary
    
    current_page += 1
    
df = pd.DataFrame(data)
df.to_csv("books.csv")