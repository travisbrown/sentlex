#!/usr/bin/env python

"""
Implemention of the WordNet propagation algorithm of 

@inproceedings{Blair-Goldensohn-etal08,
	Address = {Beijing, China},
	Author = {Blair-Goldensohn, Sasha and Hannan, Kerry and McDonald, Ryan and Neylon, Tyler and Reis, George A. and Reynar, Jeff},
	Booktitle = {{WWW} Workshop on {NLP} in the Information Explosion Era (NLPIX)},
	Title = {Building a Sentiment Summarizer for Local Service Reviews},
	Year = {2008}}

Christopher Potts.  Last update: 2010-03-01.
"""

from dicts import DefaultDict
import numpy
from nltk.corpus import wordnet as wn
from nltk.corpus import WordNetCorpusReader
from operator import itemgetter

wn_root = "/Volumes/CHRIS/Applications/WordNet-3.0/dict/"

def pos(lemma):
    return lemma.synset.pos

class SentimentLexicon:
    """ Inputs:
      positive (list): the positive seed set
      negative (list): the negative seed set
      neutral (list): the neutral seed set
      pos (str): WodNet pos value: a, v, r, n, s
      start (int): first word in the vocabulary to compute
      finish: (int or None): final word in the vocabulary to compute
      weight (float): the biasing weight used by the algorithm. 
    """
    def __init__(self, positive, negative, neutral, pos, start=0, finish=None, weight=0.2):
        self.positive = positive
        self.negative = negative
        self.neutral = neutral
        self.pos = pos
        self.weight = weight
        self.s = {}
        self.s0 = {}
        self.initialize_s()
        self.lemmas = sorted(self.s.keys())
        self.lemma_count = len(self.lemmas)
        self.start = start
        self.finish = finish
        if self.finish == None or self.finish > self.lemma_count:
            self.finish = self.lemma_count
        self.a = DefaultDict(DefaultDict(0.0))
        self.initialize_a()
    
    def iterate(self, runs=5):
        for i in range(0,runs):
            self.multiply_matrices()
        sentiment = {}
        for lemma,score in self.s.items():
            if self.a[lemma]:
                sentiment[lemma.name] = self.rescale_score(score)
        return sentiment

    def rescale_score(self, score):
        """Logarithmic rescaling of scores, using the method of Blair-Goldensohn-etal08."""
        if abs(score) <= 1:
            return 0.0
        else:
            return numpy.log(abs(score)) * numpy.sign(score)

    def initialize_s(self):
        """Builds the vectors s, as a dictionary mapping words to
        reals. The domain of the dictionary is the full vocabulary."""
        synsets = list(WordNetCorpusReader(wn_root).all_synsets(pos=self.pos))
        for synset in synsets:
            for lemma in synset.lemmas:
                if (lemma.name, synset.pos) in self.positive:
                    self.s[lemma]  = 1.0
                    self.s0[lemma] = 1.0
                elif (lemma.name, synset.pos) in self.negative:
                    self.s[lemma]  = -1.0
                    self.s0[lemma] = -1.0
                else:
                    self.s[lemma]  = 0.0
                    self.s0[lemma] = 0.0

    def initialize_a(self):
        """ Builds the matrix a.  This matrix is too big to have in
        memory all at once.  Thus, we build it instead as a dictionary
        over a limited limited set of words (self.start to
        self.finish). This is the most resource intensive part of the
        program. When it is done, the matrix multiplications are fast.
        """
        
        for i in range(self.start, self.finish):        
            lemma1 = self.lemmas[i]
            for lemma2 in self.lemmas:
                if lemma1 == lemma2:
                    self.a[lemma1][lemma2] = 1 + self.weight
                elif pos(lemma1) == pos(lemma2) and (lemma1.name, pos(lemma1)) not in self.neutral:
                    if lemma1.synset in wn.synsets(lemma2.name):
                        self.a[lemma1][lemma2] = self.weight
                    elif lemma1 in lemma2.antonyms():
                        self.a[lemma1][lemma2] = - self.weight
    
    def multiply_matrices(self):
        for lemma1 in self.lemmas:
            if self.a[lemma1]:
                colsum = 0.0           
                for lemma2 in self.lemmas:
                    colsum += self.s[lemma1] * self.a[lemma1][lemma2]
                self.s[lemma1] = self.sign_correct(lemma1, colsum)

    def sign_correct(self, lemma, colsum):
        """ Sign correction, per Blair-Goldensohn-etal08's instructions."""
        if numpy.sign(self.s0[lemma]) != numpy.sign(colsum):
            return -colsum
        else:
            return colsum


