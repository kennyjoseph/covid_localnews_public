''''
Given a csv formated and preprocessed for the STM, use a basic LDA model to find the best k.
''''

import sqlite3
from sklearn.feature_extraction.text import CountVectorizer
import numpy as np
from sklearn.decomposition import LatentDirichletAllocation as LDA
import os
from pyLDAvis import sklearn as sklearn_lda
import pyLDAvis

def test_topic_ks(text, ck = 80): #text is a list of documents

    count_vectorizer = CountVectorizer(stop_words='english')
    count_data = count_vectorizer.fit_transform(text)

    print("testing Ks...")
    cks = range(ck)
    candidate_ks = cks[40:]
    for number_topics in candidate_ks:
        print("K =", number_topics)
        lda = LDA(n_components=number_topics, n_jobs=-1)
        lda.fit(count_data)

        # Log Likelihood: Higher the better
        print("---> Log Likelihood: ", lda.score(count_data))

        # Perplexity: Lower the better. Perplexity = exp(-1. * log-likelihood per word)
        print("---> Perplexity: ", lda.perplexity(count_data))

#main
fn = "article_per_daysource_alldata.csv"
print("reading in text data...")
with open(fn) as data:
    text = [line.strip().split(",")[-1] for line in data]

test_topic_ks(text)
