import os
import csv
from os import listdir
from os.path import isfile, join
import urllib.request
from bs4 import BeautifulSoup
from helpers import *
import re
import requests

allLinks = []

mypath = 'VSS_2001-2019/' #set path to folder containing files
files = [f for f in listdir(mypath) if isfile(join(mypath, f))]
if ".DS_Store" in files:
	files.remove(".DS_Store")
   
# url = "http://jov.arvojournals.org/article.aspx?articleid=2119682"
# print(getHTMLdynamic(url))

# leaving off where file broke
starthere = True
filestart = "2142239"

with open('vss_abstracts.csv', 'w') as csv_file:
    fieldnames = ['year', 'authors', 'title', 'abstract', 'html_link']
    writer = csv.DictWriter(csv_file, fieldnames=fieldnames)
    writer.writeheader()

    for f in files:
        vss = open(mypath+f, 'rb')
        soup = BeautifulSoup(vss, 'html.parser')

        # extracts publication year from file name
        year = re.search(r'VSS(.*?)\.htm', f).group(1)
        print(year)

        for link in soup.find_all('a'):
            newlink = link.get('href')

            if not starthere and newlink is not None and filestart in newlink:
            	starthere = True
            
            if starthere and newlink is not None and 'articleid=' in newlink and newlink not in allLinks:
            	allLinks.append(newlink)

            	page = getHTMLdynamic(newlink)
            	lsoup = BeautifulSoup(page, 'html.parser')
            	# lsoup.prettify()

            	# extracts title
            	title = lsoup.find('div', attrs={'class':'article-title-main'}).get_text()
            	# print(title)

            	# extracts authors
            	authorList = []
            	authors = ""
            	for auth in lsoup.find_all("meta", attrs={'name':'citation_author'}):
            		authorList.append(auth['content'])
            	authors = ", ".join(authorList)
            	#print(authors)

            	abstract = lsoup.find('p', attrs={'class':'para'}).get_text()
            	#print(abstract)

            	writer.writerow({'year': year, 'authors':authors, 'title': title, 'abstract': abstract, 'html_link': newlink})
    csv_file.close()





