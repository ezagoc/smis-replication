# python3


import numpy as np
import pandas as pd
import textstat
import re
import collections as coll
import math
import scipy as sc
import nltk

from nltk.tokenize import word_tokenize, sent_tokenize
from nltk import ngrams
from keras.preprocessing.text import text_to_word_sequence
from sentiment_analysis_spanish import sentiment_analysis
from collections import Counter
from spacy.lang.es import Spanish


def sentiment_metrics(text, sentiments, sentiment_score=True, polarity_score=False):
    """Computes sentiment score.

    Parameters
    ----------
    text : str
        String containing text to analyse
    sentiments : object
        Instantiate sentiment object
    sentiment_score : bol
        Defines if sentiment score will be computed (defaults to True)
    polarity_score : bol
        Defines if polarity score will be computed (defaults to False)

    Returns
    -------
    sentimiento : float
        Sentiment score
    polarity : float
        Polarity score
    """
    try:
        if sentiment_score:
            sentimiento = sentiments.sentiment(text)
        else:
            sentimiento = np.nan
        if polarity_score:
            positive, negative = [], []
            tokens = text_to_word_sequence(text)
            for i in tokens:
                score = sentiments.sentiment(i)
                if score < 0.5:
                    negative.append(1)
                elif score > 0.5:
                    positive.append(1)
                else:
                    pass
            polarity = (sum(positive)*(1) + sum(negative)*(-1))/(len(tokens))
        else:
            polarity = np.nan
    except:
        sentimiento, polarity = np.nan, np.nan


    return sentimiento, polarity


def load_fakenews(type_data, bd):
    """Load fakenews dataset

    Parameters
    ----------
    type_data : str
        String to specify if we load news pieces or social media set
    bd : str
        Path to the fake news database
    Returns
    -------
    texto : list
        List of clean fake news text
    """

    if type_data=='newspaper':
        df = pd.read_excel(bd +'fakenews_newspapers.xlsx')
    else:
        df = pd.read_excel(bd+'fakenews_socialmedia.xlsx')
    try:
        df = df.drop(columns = ['Unnamed: 0']).drop_duplicates()
    except:
        pass
    df = df[df['label_desinformacion'] == 'fake']
    df = df[~df['texto_desinformacion'].isna()]
    df = df.replace(
        {'...': np.nan, "[": np.nan, '?': np.nan, "]": np.nan})
    texto = list(df.clean_texto)


    return texto


def clean_list(list):
    """Clean text in list

    Parameters
    ----------
    list : list
        A list containing text
    Returns
    -------
    list : list
        A clean list containing text
    """
    try:
        list = [x for x in list if x != '...']
        list = [x for x in list if x != "["]
        list = [x for x in list if x != '?']
        list = [x for x in list if x != "]"]
        list = [x for x in list if x != '"']
        list = [x for x in list if x != "'"]
        list = [x for x in list if x != '<']
        list = [x for x in list if x != '>']
        list = [x for x in list if x != '--']
    except:
        pass


    return list

def count_unigrams(type_data, bd):
    """Count unigrams

    Parameters
    ----------
    type_data : str
        String to specify if we load news pieces or social media set
    bd : str
        Path to the fake news database
    Returns
    -------
    counts : tuple
        A tuple of unigram and count
    """
    all_fake = ' '.join(load_fakenews(type_data, bd))
    fake = re.sub('[ ]y[ ]|[ ]a[ ]|[ ]o[ ]|[ ]e[ ]', '', all_fake)
    split_it = fake.split()
    split_it = clean_list(split_it)
    counts = Counter(split_it)

    return counts


def common_unigrams(text, counts_unigrams):
    """Common unigrams

    Returns a 1 or 0 depending on whether input text contains
    at least one unigram of the 25/50/100 most common unigrams
    in misinformations

    Parameters
    ----------
    counts_unigrams : tuple
        A tuple of unigrams and counts
    text : str
        A text

    Returns
    -------
    values : list
        A list containing 1 or 0 depending on whether input text contains
        at least one unigram of the 25/50/100 most common unigrams
        in misinformations
    """
    try:
        text = clean_list(text.split())
        values = []
        for n in [25, 50, 100]:
            most_occur=[key for key, _ in counts_unigrams.most_common(n)]
            n_grams = []
            for word in text:
                if word in most_occur:
                    n_grams.append(1)
            value = sum(n_grams)
            if value >= 1:
                value=1
            else:
                value=0
            values.append(value)
    except:
        values = [np.nan, np.nan, np.nan]
    values_list = values[0], values[1], values[2]

    return values_list


def count_bigrams(type_data, bd):
    """Count bigrams

    Parameters
    ----------
    type_data : str
        String to specify if we load news pieces or social media set
    bd : str
        Path to the fake news database
    Returns
    -------
    counts : tuple
        A tuple of bigrams and count
    """
    all_fake = ' '.join(load_fakenews(type_data, bd))
    fake = re.sub('[ ]y[ ]|[ ]a[ ]|[ ]o[ ]|[ ]e[ ]', '', all_fake)
    bigrams_fake = [b for l in [fake] for b in zip(l.split(" ")[:-1], l.split(" ")[1:])]
    counts = Counter(bigrams_fake)

    return counts


def common_bigrams(text, count_bigrams):
    """Common bigrams

    Returns a 1 or 0 depending on whether input text contains
    at least one bigram of the most common bigrams
    in misinformations

    Parameters
    ----------
    counts_bigrams : tuple
        A tuple of bigrams and counts
    text_list : str
        A text

    Returns
    -------
    values : list
        A list containing 1 or 0 depending on whether input text contains
        at least one bigram of the 25/50/100 most common unigrams
        in misinformations
    """
    try:
        bigrams_x = ngrams(text.split(), 2)
        values=[]
        for i in [25, 50, 100]:
            most_occur = [key for key, _ in count_bigrams.most_common(i)]
            n_bigrams=[]
            for bigram_x in bigrams_x:
                if bigram_x in most_occur:
                    n_bigrams.append(1)
            value = sum(n_bigrams)
            if value >= 1:
                value=1
            else:
                value=0
            values.append(value)
    except:
        values=[np.nan, np.nan, np.nan]

    return values


def word_count(text):

    try:
        value = len(text_to_word_sequence(text))
    except:
        value = np.nan

    return value


def word_density(text):

    try:
        keyword_density = []
        collection = Counter(text_to_word_sequence(text))
        for ele in collection:
            density = collection[ele]/len(text_to_word_sequence(ele))
            keyword_density.append(density)
        keyword_density = np.mean(keyword_density)
    except:
        keyword_density = np.nan

    return keyword_density


def upper_case(text):

    try:
        value = len(re.findall('[A-Z]', text))
        return value
    except:
        value = np.nan

    return value


def character(text):

    try:
        value = len(text)
    except:
        value = np.nan
    return value


def hashtag(text):

    try:
        value = len(re.findall('[#]', text))
    except:
        value = np.nan

    return value


def find_url(text):
    try:
        regex = r"(?i)\b((?:https?://|www\d{0,3}[.]|[a-z0-9.\-]+[.][a-z]{2,4}/)(?:[^\s()<>]+|\(([^\s()<>]+|(\([^\s()<>]+\)))*\))+(?:\(([^\s()<>]+|(\([^\s()<>]+\)))*\)|[^\s`!()\[\]{};:'\".,<>?«»“”‘’]))"
        url = re.findall(regex,text)
        value = len([text[0] for text in url])
    except:
        value = 0

    return value


def sentence_count(text, nlp):

    try:
        doc = nlp(str(text))
        sentences_list = []
        for sent in doc.sents:
            sentences_list.append(1)
        value = sum(sentences_list)
    except:
        value = np.nan

    return value


def punctuation_count(text):
    """
    Conteo de la cantidad
    de signos de puntuación
    en un texto determinado.
    """
    try:
        puntuacion = [",", ".", "'", "!", '"', ";", "?", ":", ";", '¡', '¿']
        conteo = 0
        for character in text:
            if (character in puntuacion):
                conteo = conteo + 1
    except:
        conteo = np.nan

    return conteo


def syllable_count(text):

    try:
        word = text.lower()
        count = 0
        vowels = "aeiouy"
        if word[0] in vowels:
            count += 1
        for index in range(1, len(word)):
            if word[index] in vowels and word[index - 1] not in vowels:
                count += 1
        if word.endswith("e"):
            count -= 1
        if count == 0:
            count += 1
        value = count
    except:
        value = 0

    return value


def emoticon_count(text):

    try:
        value = len(re.findall(u'[\U0001f600-\U0001f650]', text))
    except:
        value = np.nan

    return value


def readability_index(text):

    try:
        ari = textstat.automated_readability_index(text)
    except:
        ari = np.nan

    return ari


def unique_words(text):

    unique = []
    words = text_to_word_sequence(text)
    for h in words:
        if h not in unique:
            unique.append(h)
        else:
            continue
    unique_words = len(unique)

    return unique_words


def type_token_ratio(text):

    try:
        total_unique = unique_words(text)
        total_words = len(text_to_word_sequence(text))
        value = total_unique/total_words
    except:
        value= np.nan

    return value


def flesch_reading(text):

    try:
        value = textstat.flesch_reading_ease(text)
    except:
        value = np.nan

    return value


def remover_caracteres(texto):
    try:
        texto = word_tokenize(texto)
        st = [",", ".", "'", "!", '"', "#", "$", "%", "&", "(", ")", "*", "+", "-", ".", "/", ":", ";", "<", "=", '>', "?",
              "@", "[", "\\", "]", "^", "_", '`', "{", "|", "}", '~', '\t', '\n',"¡", "¿"]

        palabras = [palabra for palabra in texto if palabra not in st]
    except:
        palabras = np.nan

    return palabras

def simpsons_index(text):
    """Indice de diversidad medido de la siguiente manera:

    * 1 - (sigma(n(n - 1))/N(N-1)
    N is total number of words
    n is the number of each type of word
    """
    try:
        words = remover_caracteres(text)
        freqs = coll.Counter()
        freqs.update(words)
        N = len(words)
        n = sum([1.0 * i * (i - 1) for i in freqs.values()])
        D = 1 - (n / (N * (N - 1)))
    except:
        D = np.nan

    return D


def brunets_measure_w(text):
    """
    # logW = V-a/log(N)
    # N = total words , V = vocabulary richness
    (unique words) ,  a=0.17
    # we can convert into log because we are
    only comparing different texts
    """
    try:
        words = remover_caracteres(text)
        a = 0.17
        V = float(len(set(words)))
        N = len(words)
        B = (V - a) / (math.log(N))
    except:
        B = np.nan

    return B


def yules_characteristic_K(text):
    """La K caraceterística de Yules

    * K  10,000 * (M - N) / N**2
    Donde M es Sigma i**2 * Vi.
    """
    try:
        words = remover_caracteres(text)
        N = len(words)
        freqs = coll.Counter()
        freqs.update(words)
        vi = coll.Counter()
        vi.update(freqs.values())
        M = sum([(value * value) * vi[value] for key, value in freqs.items()])
        K = 10000 * (M - N) / math.pow(N, 2)
    except:
        K = np.nan

    return K


def shannon_entropy(text):
    """
    Índice de diversidad medido de la siguiente manera:
    * -1*sigma(pi*lnpi)
    """
    try:
        words = remover_caracteres(text)
        lenght = len(words)
        freqs = coll.Counter()
        freqs.update(words)
        arr = np.array(list(freqs.values()))
        distribution = 1. * arr
        distribution /= max(1, lenght)
        H = sc.stats.entropy(distribution, base=2)
    except:
        H = np.nan

    return H


def fernandez_huerta(text):
    try:
        lecturabilidad = textstat.fernandez_huerta(text)
    except:
        lecturabilidad = np.nan

    return lecturabilidad


def reading_time(text):
    try:
        reading_time = textstat.reading_time(text)
    except:
        reading_time = np.nan

    return reading_time


def gutierrez_polini(text):
    try:
        understandability = textstat.gutierrez_polini(text)
    except:
        understandability = np.nan

    return understandability


def szigriszt_pazos(text):
    try:
        szigriszt_pazos = textstat.szigriszt_pazos(text)
    except:
        szigriszt_pazos = np.nan

    return szigriszt_pazos


def crawford(text):
    try:
        schooling = textstat.crawford(text)
    except:
        schooling = np.nan

    return schooling


def punctuation_percentage(text):
    """
    Porcentaje del texto que es
    algún signo de puntuación
    """
    try:
        puntuacion = [",", ".", "'", "!", '"', ";", "?", ":", ";", '¡', '¿']
        conteo = 0
        for m in text:
            if (m in puntuacion):
                conteo = conteo + 1
        porcentaje = (float(conteo) / float(len(text)))*100
    except:
        porcentaje = np.nan

    return porcentaje


def remove_special_characters(text):
    """Remove special characters


    """
    try:
        entry = word_tokenize(text)
        st = [",", ".", "'", "!", '"', "#", "$", "%", "&", "(", ")", "*", "+", "-", ".", "/", ":", ";", "<", "=", '>', "?",
              "@", "[", "\\", "]", "^", "_", '`', "{", "|", "}", '~', '\t', '\n', '¡', '¿']
        palabras = [palabra for palabra in entry if palabra not in st]
        palabras = " ".join(palabras).strip()
    except:
        palabras = np.nan

    return palabras


def functional_words_count(text, palabras_funcionales):
    """Conteo de palabras funcionales en el texto


    """
    try:
        entradas = remove_special_characters(text)
        entradas = word_tokenize(entradas)
        conteo_palabrasfuncionales = []
        for entrada in entradas:
            conteo = 0
            for i in palabras_funcionales:
                if i in entrada[0].split():
                    conteo += 1
            cantidad = conteo / len(entrada[0].split())
            conteo_palabrasfuncionales.append(cantidad)
        suma = sum(conteo_palabrasfuncionales)
    except:
        suma = np.nan

    return suma


def hapaxdislegomena(text):

    try:
        words = remover_caracteres(text)
        count = 0
        freqs = coll.Counter()
        freqs.update(words)
        for word in freqs:
            if freqs[word] == 2:
                count += 1

        h = count / float(len(words))
        S = count / float(len(set(words)))
    except:
        h, S = np.nan, np.nan

    return S, h


def hapaxlegomenon(texto):
    """
    Detecta una palabra que solo
    aparece una vez dentro del texto
    y el ratio de honoré
    Entrada:
    --texto
    Salida:
    --R: ratio de Honoré
    --hapax
    """
    try:
        palabras = remover_caracteres(texto)
        V1 = 0
        freqs = {key: 0 for key in palabras}
        for palabra in palabras:
            freqs[palabra] += 1
        for palabra in freqs:
            if freqs[palabra] == 1:
                V1 += 1
        N = len(palabras)
        V = float(len(set(palabras)))
        R = 100 * math.log(N) / max(1, (1 - (V1 / V)))
        hapax = V1 / N
    except:
        hapax, R = np.nan, np.nan

    return R, hapax
