#!/bin/bash
#script per elaborare i dati di precipitazione del 2020


pdfseparate ${1} temp%d.pdf


for dd in $(ls temp*.pdf); do

	pdftotext -layout ${dd}


	rm -rf $dd

done
