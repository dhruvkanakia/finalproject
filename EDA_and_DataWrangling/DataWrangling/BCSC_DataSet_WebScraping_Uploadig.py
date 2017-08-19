
# coding: utf-8

# In[2]:

import requests
from lxml import html
import json
from zipfile import ZipFile
import zipfile
import pandas as pd
import os
import glob
import mechanicalsoup as ms
from azure.storage.blob import BlockBlobService
from azure.storage.blob import ContentSettings


# In[3]:

with open('config.json') as data:
    payload=json.load(data)


# In[4]:

block_blob_service = BlockBlobService(account_name='finalprojectsummer2017', account_key=payload['Key'])


# In[5]:

final='Data/'


# In[6]:

url = "http://www.bcsc-research.org/rfdataset/app2/protected/Logon.aspx?ReturnUrl=%2frfdataset%2fapp2%2fprotected%2frisk_dataset.zip"
url2 = "http://www.bcsc-research.org/rfdataset/app2/protected/risk_dataset.zip"
filename='zipfile.zip'
s = requests.Session()
browser = ms.Browser(session = s)                                  #Creating session
login_page = browser.get(url)
login_form = login_page.soup.find("form",{"id":"form1"})
#print(login_form)
login_form.find("input", {"name":"UserEmail"})["value"] = 'bcsclogin'     #Inputting username
login_form.find("input", {"name":"UserPass"})["value"] = 'bcsc_pass'      #Inputing password
response = browser.submit(login_form, login_page.url)                     
login_page2 = browser.get(url2)
with open(os.path.join(final,filename), 'wb') as f:
#                 print(link.text)
        for chunk in login_page2.iter_content(chunk_size=1024):
            if chunk: # filter out keep-alive new chunks
                f.write(chunk)
                #print('zip file created')


# In[7]:

names=["menopaus","agegrp","density","race","Hispanic","bmi","agefirst",
                "noOfFirstDegRelativesCan","prevcanProc","lastMamm","surgMeno","hrt","invasive"
                ,"cancerStatus","type","count"]


# In[8]:

import zipfile
for files in glob.glob(os.path.join(final, '*.zip')):       
        with zipfile.ZipFile(files) as zip_ref:
            df = pd.read_csv(zip_ref.open('risk.txt'),delim_whitespace=True,names=names)
            


# In[9]:

a=df.iloc[:, [1,3,4,5,6,7,8,9,10,12]]
a = a.replace(9, '', regex=True)
df.iloc[:, [1,3,4,5,6,7,8,9,10,12]]=a


# In[10]:

df.to_csv(final+'Risk.csv')
print('Risk csv ready')


# In[11]:

from azure.storage.blob import ContentSettings
temp=[]
generator = block_blob_service.list_blobs('data')
for blob in generator:
    a=blob.name
    temp.append(a)
    


# In[12]:

temp


# In[ ]:




# In[13]:

from os import walk

f = []
for files in glob.glob(os.path.join(final, '*.csv')): 
    f.append(files.rsplit('\\', 1)[-1])


# In[14]:

f


# In[15]:

for x in f:
    if x in temp:
        print(x+'  exists')
        
    else:
        
        block_blob_service.create_blob_from_path(
        'data',
        x,
        final+x,
        content_settings=ContentSettings(content_type='csv')
        )


# In[ ]:



