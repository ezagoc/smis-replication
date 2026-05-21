#!python3.8

import re
import spacy
import pandas as pd
import numpy as np
import nltk

from nltk.corpus import stopwords
from unicodedata import normalize

def remove_stop_words(dataframe, target_column_name, new_column_name):
    dataframe[new_column_name] = dataframe[target_column_name].apply(lambda x: ' '.join([item for item in x.split() if item not in stopwords.words('english')]))
    return dataframe

def remove_punctuations(dataframe, target_column_name, new_column_name):
    dataframe[new_column_name] = dataframe[target_column_name].apply(lambda x: "".join([char for char in x if char not in string.punctuation]))
    return dataframe

def stem_text(dataframe, target_column_name, new_column_name):
    dataframe[new_column_name] = dataframe[target_column_name].apply(lambda x:ps.stem(word) for word in x)
    return dataframe

def clean_text(sentence, *nlp, remove_stopwords=False, lemmatize=False):
    """Clean string

    * Removes:
    ** html
    ** http links
    ** accents
    ** hasthtags
    ** mentions
    ** punctuation
    ** numbers
    """
    try: 
        tag_re = re.compile(
    
        r'<[^>]+>'
    
        )
        sentence = tag_re.sub(
    
        '', sentence
    
        )
        sentence = re.sub(
        r'https?:\/\/.[^\n ]*',
        '',
        sentence
        )
        sentence = re.sub(
            r"([^n\u0300-\u036f]|n(?!\u0303(?![\u0300-\u036f])))[\u0300-\u036f]+", r"\1",
            normalize( "NFD", sentence), 0, re.I)
    
        sentence = sentence.lower()
        sentence = re.sub(
        r'\@\S+',
        ' ',
        sentence
        )
        sentence = re.sub(r'\#\S+', ' ', sentence)
        sentence = re.sub('[^a-zA-Z]', ' ', sentence)
    
        if remove_stopwords:
            sentence = remove_stopwords(sentence)
    
        sentence = re.sub(r"\s+[a-zA-Z]\s+", ' ', sentence)
        sentence = re.sub(r"^[a-zA-Z]\s+", ' ', sentence)
        sentence = re.sub(r'\s+', ' ', sentence)
    
        if lemmatize:
            sentence = lemmatize_text(sentence, *nlp)
            #sentence = remove_stopwords(sentence)
        sentence = sentence.strip()
    except:
        pass
    return sentence
