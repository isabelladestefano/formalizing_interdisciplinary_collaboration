# -*- coding: utf-8 -*-
"""
Spyder Editor

"""

import sys
reload(sys)  
sys.setdefaultencoding('utf8')
sys.path.append("/usr/local/lib/python2.7/site-packages/")
sys.setrecursionlimit(1500)
from pdfminer.pdfinterp import PDFResourceManager, PDFPageInterpreter
from pdfminer.converter import TextConverter
from pdfminer.layout import LAParams
from pdfminer.pdfpage import PDFPage
from pdfminer.pdfparser import PDFParser
from pdfminer.pdfdocument import PDFDocument
from StringIO import StringIO

import os
import csv
from os import listdir
from os.path import isfile, join
import urllib2
from urllib2 import Request
from bs4 import BeautifulSoup
import re

allYears = dict()
allLinks = dict()


def pdf_from_url_to_txt(url):
    rsrcmgr = PDFResourceManager()
    retstr = StringIO()
    codec = 'utf-8'
    laparams = LAParams()
    device = TextConverter(rsrcmgr, retstr, codec=codec, laparams=laparams)
    # Open the url provided as an argument to the function and read the content
    f = urllib2.urlopen(urllib2.Request(url)).read()
    # Cast to StringIO object
    fp = StringIO(f)
    interpreter = PDFPageInterpreter(rsrcmgr, device)
    password = ""
    maxpages = 0
    caching = True
    pagenos = set()
    for page in PDFPage.get_pages(fp,
                                  pagenos,
                                  maxpages=maxpages,
                                  password=password,
                                  caching=caching,
                                  check_extractable=True):
        interpreter.process_page(page)
    fp.close()
    device.close()
    str = retstr.getvalue()
    retstr.close()
    return str

def checkLink(url):
	try:
		urllib2.urlopen(url)
		return True
	except Exception:
		return False

mypath = 'htm/' #set path to folder containing files
files = [f for f in listdir(mypath) if isfile(join(mypath, f))]

with open('cogsci_papers.csv', 'w') as csv_file:
    fieldnames = ['year', 'authors', 'title', 'abstract', 'html_link', 'pdf_link', 'full_text']
    writer = csv.DictWriter(csv_file, fieldnames=fieldnames)
    writer.writeheader()


    #2000-2014
    pagevol = "https://escholarship.org/uc/cognitivesciencesociety/"
    for y in range(22, 37):
	    yearpage = urllib2.urlopen(pagevol + str(y) + "/" + str(y))
	    soup = BeautifulSoup(yearpage, 'html.parser')
	    for link in soup.find("main").find_all('a'):
	        newlink = 'https://escholarship.org' + link.get('href')
	        if "/uc/item/" in str(link) and newlink not in allLinks:
	            allLinks[newlink] = [newlink]
	            page = urllib2.urlopen(newlink)
	            lsoup = BeautifulSoup(page, 'html.parser')
	            
	            year = lsoup.find("meta", attrs={'name':'citation_publication_date'})['content']
	            if year not in allYears:
	            	allYears[year] = [year]
	            	print(year)
	            
	            title = lsoup.find("title").get_text()
	            if len(allLinks) % 100 == 1:
	            	print(str(len(allLinks) - 1) + ": " + title)
	            #print(year + ": " + title)
	            
	            authorList = []
	            for auth in lsoup.find_all("meta", attrs={'name':'citation_author'}):
	                authorList.append(auth['content'])
	            authors = ', '.join(authorList)

	            abstract = 'NA'
	            
	            pdf_link = lsoup.find("meta", attrs={'name':'citation_pdf_url'})['content']

	            full_text = 'NA'
	            try:
	            	if pdf_link != 'NA' and checkLink(pdf_link):
	            		full_text = pdf_from_url_to_txt(pdf_link)
	            except Exception, e:
	            	pass
	            except urllib2.HTTPError:
	            	pass

	            writer.writerow({'year': year, 'authors':authors, 'title': title, 'abstract': abstract, 'html_link': newlink, 'pdf_link': pdf_link, 'full_text': full_text})

    # 2015-2018
    for f in files:
    	if f != '.DS_Store':
	        cogsci = open('htm/'+f, 'rb')
	        soup = BeautifulSoup(cogsci, 'html.parser')
	        
	        for link in soup.find_all('a'):
	            newlink = link.get('href')
	            if 'papers' in newlink and newlink not in allLinks: #checks for redundant papers
	                allLinks[newlink] = [newlink]
	                
	                # extracts publication year from file name
	                year = re.search(r'CogSci(.*?)\.htm', f).group(1)
	                #print(year)
	                if year not in allYears:
		            	allYears[year] = [year]
		            	print(year)
	                
	                page = urllib2.urlopen(newlink)
	                lsoup = BeautifulSoup(page, 'html.parser')
	                     
	                # extracts title
	                title = lsoup.find('h1').get_text().encode('utf-8')
	                if len(allLinks) % 100 == 1:
	            		print(str(len(allLinks) - 1) + ": " + title)
	                #print(year + ": " + title)

	                # extracts authors, separated by a comma
	                authors = 'NA'
	                if lsoup.find('div', attrs={'id':'pagebody'}) != None:
	                    li = lsoup.find('div', attrs={'id':'pagebody'}).find('li')
	                    authors = re.sub(",.*?\n", ", ", li.get_text().encode('utf-8'))
	                elif lsoup.find('ul', attrs={'class':'subAuthorList'}) != None:
	                    authorList = []
	                    for a in lsoup.find('ul', attrs={'class':'subAuthorList'}).find_all('span', attrs={'class':'subAuthorName'}):
	                        authorList.append(a.get_text().encode('utf-8'))
	                    authors = ', '.join(authorList)
	                    
	                # extracts abstract
	                abstract = 'NA'
	                if lsoup.find('p', attrs={'id':'abstract'}) != None:
	                    abstract = lsoup.find('p', attrs={'id':'abstract'}).get_text().encode('utf-8')
	                elif lsoup.find('span', attrs={'class':'subAbstract'}) != None:
	                    abstractHtml = lsoup.find('span', attrs={'class':'subAbstract'})
	                    b = abstractHtml.find('b')
	                    b.extract()
	                    abstract = abstractHtml.get_text().encode('utf-8')
	                
	                # extracts full pdf link
	                pdf_link = 'NA'
	                if lsoup.find('li', attrs={'id':'files'}) != None:
	                    pdf_link = re.sub("index.html", "", newlink)
	                    pdf_link = pdf_link + lsoup.find('li', attrs={'id':'files'}).find('a').get('href')
	                elif lsoup.find('span', attrs={'class':'subFile'}) != None:
	                    pdf_link = re.sub("index.html", "", newlink)
	                    pdf_link = pdf_link + lsoup.find('span', attrs={'class':'subFile'}).find('a').get('href')

	                full_text = 'NA'
	                try:
	                	if pdf_link != 'NA' and checkLink(pdf_link):
	                		full_text = pdf_from_url_to_txt(pdf_link)
	                except Exception, e:
	            		pass
	            	except urllib2.HTTPError:
	            		pass

	                writer.writerow({'year': year, 'authors':authors, 'title': title, 'abstract': abstract, 'html_link': newlink, 'pdf_link': pdf_link, 'full_text': full_text})
    		cogsci.close()
    csv_file.close()









