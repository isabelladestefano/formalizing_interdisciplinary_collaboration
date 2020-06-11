#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Aug 16 12:16:15 2018



@author: evul
"""
import re
import requests
import time
import warnings
from math import sqrt
import random
from selenium import webdriver
# from selenium.webdriver.support.ui import WebDriverWait
# from selenium.webdriver.support import expected_conditions as EC
# from selenium.webdriver.common.by import By

# TODO: automate closing this?
wd = webdriver.Firefox(executable_path='geckodriver')
#wd.get("http://jov.arvojournals.org/article.aspx?articleid=2119682")

# firefox_profile = webdriver.FirefoxProfile()
# firefox_profile.set_preference("browser.privatebrowsing.autostart", True)

# wd = webdriver.Firefox(firefox_profile=firefox_profile)


def getTLD(URL):
    return('.'.join(re.search('//([A-Za-z\.]+)/', URL).group(1).split('.')[-2:]))
    

def cleanString(string):
    if string is not None:
        for ch in [',', '"', "'"]:
            string = string.replace(ch,' ')
        return(string)
    else:
        return(None)


def getUserAgent():
#    user_agent_list=[
#    #Chrome
#    'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/60.0.3112.113 Safari/537.36',
#    'Mozilla/5.0 (Windows NT 6.1; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/60.0.3112.90 Safari/537.36',
#    'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/44.0.2403.157 Safari/537.36',
#    'Mozilla/5.0 (Windows NT 5.1; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/60.0.3112.90 Safari/537.36',
#    'Mozilla/5.0 (Windows NT 6.2; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/60.0.3112.90 Safari/537.36',
#    #Safari
#    'Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.21 (KHTML, like Gecko) Mwendo/1.1.5 Safari/537.21',
#    'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_6) AppleWebKit/601.7.7 (KHTML, like Gecko) Version/9.1.2 Safari/601.7.7',
#    'Mozilla/5.0 (iPad; CPU OS 9_3_2 like Mac OS X) AppleWebKit/601.1.46 (KHTML, like Gecko) Version/9.0 Mobile/13F69 Safari/601.1',
#    #'Mozilla/5.0 (iPhone; CPU iPhone OS 9_1 like Mac OS X) AppleWebKit/601.1.46 (KHTML, like Gecko) Version/9.0 Mobile/13B143 Safari/601.1',
#    'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_6) AppleWebKit/603.3.8 (KHTML, like Gecko) Version/10.1.2 Safari/603.3.8',
#    #Linux
#    'Mozilla/5.0 (X11; Linux x86_64; rv:45.0) Gecko/20100101 Thunderbird/45.3.0',
#    'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/44.0.2403.157 Safari/537.36',
#    'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/42.0.2311.135 Safari/537.36',
#    'Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:15.0) Gecko/20100101 Firefox/15.0',
#    'Mozilla/5.0 (Unknown; Linux) AppleWebKit/538.1 (KHTML, like Gecko) Chrome/v1.0.0 Safari/538.1',
#    ]
#    return(random.choice(user_agent_list))
    # seems to work better with a fixed, appropriate, user-agent string
    return('Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/61.0.3163.100 Safari/537.36')

def getHTMLstatic(URL):
    user_agent=getUserAgent()
    HTML=requests.get(URL, headers={"User-Agent":user_agent})
    requestscount = 0
    time.sleep(random.randint(1, 5))
    while HTML.status_code !=200:
        if requestscount > 10:
            warnings.warn('Requests were exceeded.')
            break;
        else:
            time.sleep(random.randint(3, 60)) # (sqrt(requestscount))*10, sqrt(requestscount+1)*10)
            HTML=requests.get(URL, headers={'User-Agent':user_agent})
            requestscount +=1
    if HTML.status_code == 200:
        return(HTML.text)
    else:
        return(None)


def getHTMLdynamic(URL):
    try:
        wd.get(URL)
    except:
        return('')
    
    # Wait for the dynamically loaded elements to show up
    # this works better than trying to use webdriver conditional statements, 
    # since its hard to identify what thr right conditions ought to be
    # time.sleep(2)
    # try:
    #     # this hack closes modal popup on one particular site.  general solution will be trickier.
    #     wd.execute_script("$('.close-link').click()")
    # except:
    #     pass
    # for _ in range(0, 2):
    #     # scroll up, then down, to load pages that dynamically load more when at bottom
    #     wd.execute_script('window.scrollTo(0,0);')
    #     time.sleep(0.25)
    #     wd.execute_script('window.scrollTo(0,document.body.scrollHeight);')
    #     time.sleep(1.5)
    
    # time.sleep(2)
    # And grab the page HTML source
    html = wd.page_source
    return(html)
