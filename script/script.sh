#!/bin/bash

#La pagina: https://annali.regione.umbria.it/download/ contiene la lista di tutti i file .csv
#che riguardano ciascuna stazione. Avendo questa lista di file csv si possono facilmente scaricare
#tutti i corrispondenti file .csv. Questa e' la cosa piu' semplice da fare perche' con RSelenium
#scaricare i dati sembra abbastanza complesso

# 1) https://annali.regione.umbria.it/download/ da questa pagina scarichiamo il file "Index of download.html"

#2) Dal file "Index of download.html" mediante lo script seguente ricaviamo la lista dei file .csv
#da scaricare

#Questo script trova i tag con gli indirizzi dei file csv e fa in modo di delimitare tali indirizzi
#mediante ";". In questo modo il file listaDownloads.csv pu essere facilmente aperto in Excel
#per generare un file .csv (tabellaDownload.csv) che serve da input per il file scaricaDati.R

 grep -E "https://annali.regione.umbria.it/download/" Index\ of\ _download.html \
| sed -e 's/href="/;/g' \
|sed -e 's/csv"/csv;/g' > listaDownloads.csv
