# -*- coding: utf-8 -*-
"""
Created on Wed Feb  8 15:17:24 2023

@author: kclab

Convert PDF to text for NGO annual reports
"""

import os
import glob
import pytesseract

from pdf2image import convert_from_path

# Define working directory, set PATH to Tesseract in this process
os.chdir("C:/Users/kclab/OneDrive/ASU/Scripts/R/ConservationBuzz/data/corpora/preprocessed/ngo/")
os.environ['PATH'] = os.environ['PATH'] + ";C:\Program Files (x86)\Tesseract-OCR"


# Use glob to find all files that end with .pdf in a directory
folder = "203_ci"
to_search = [folder, "/*.pdf"]
separator = ""

files = glob.glob(separator.join(to_search))


for file in files:

    images = convert_from_path(file)
    ocr_text = ""
    for image in images:
        ocr_text += pytesseract.image_to_string(image)

    new_file = [file[:-4], "_ocr", ".txt"]
        
    with open(separator.join(new_file), 'wb') as i:
        i.write(ocr_text.encode("utf-8"))
