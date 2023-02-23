# -*- coding: utf-8 -*-
"""
Created on Wed Feb  8 15:13:55 2023

@author: kclab

Convert PDF to text for NGO annual reports

"""


import glob
import PyPDF2


# Use glob to find all files that end with .pdf in a directory
folder = "213_wcs"
to_search = [folder, "/*.pdf"]
separator = ""


files = glob.glob(separator.join(to_search))


# Loop through each file
for file in files:
    # Open the PDF file
    with open(file, 'rb') as f:
        pdf_reader = PyPDF2.PdfFileReader(f)

        # Initialize a variable to store the text from all pages
        all_text = ""

        # Loop through each page of the PDF
        for page_num in range(pdf_reader.numPages):
            page = pdf_reader.getPage(page_num)
            page_text = page.extractText()

            # Add the text from the current page to the text from all pages
            all_text += page_text

        text_forexport = all_text.replace("\u25aa", "*")
        # Do something with the text from all pages
        
        new_file = [file[:-4], "_preprocess", ".txt"]
        
        with open(separator.join(new_file), 'wb') as i:
            i.write(text_forexport.encode("utf-8"))
        
        
