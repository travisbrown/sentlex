#!/usr/bin/env python

from dicts import DefaultDict
import numpy
from nltk.corpus import wordnet as wn
from nltk.corpus import WordNetCorpusReader
from operator import itemgetter
import pickle
import yaml
from sentimentlexicon import SentimentLexicon
import pp

wn_root = "/Volumes/CHRIS/Applications/WordNet-3.0/dict/"

def parse_seedfile(filename):
    return [tuple(line.split(",")) for line in open(filename).read().splitlines()]

def run_pos_single(specs):
    positive = parse_seedfile(specs["positive_filename"])
    negative = parse_seedfile(specs["negative_filename"])
    neutral  = parse_seedfile(specs["neutral_filename"])
    lex = SentimentLexicon(positive, negative, neutral, specs["pos"], start=0, finish=None, weight=0.2)
    sentiment = lex.iterate()
    yaml.dump(sentiment, file("wn.%s.%s.yaml" % (specs["classification"], specs["pos"]), "w"))

def run_pos_parallel(specs, job_length):
    
    def sentiment_chunk(specs, start, finish):
        positive = [tuple(line.split(",")) for line in open(specs["positive_filename"]).read().splitlines()]
        negative = [tuple(line.split(",")) for line in open(specs["negative_filename"]).read().splitlines()]
        neutral  = [tuple(line.split(",")) for line in open(specs["neutral_filename"]).read().splitlines()]
        lex = sentimentlexicon.SentimentLexicon(positive, negative, neutral, specs["pos"], start=start, finish=finish, weight=0.2)
        sentiment = lex.iterate()
        output_filename = "wn.%s.%s%s-%s.yaml" % (specs["classification"], specs["pos"], start, finish)
        yaml.dump(sentiment, file(output_filename, "w"))
        return sentiment

    # Get the overall size, lc.
    synsets = list(WordNetCorpusReader(wn_root).all_synsets(pos=specs["pos"]))
    lc = {}
    for synset in synsets:
        for lemma in synset.lemmas:
            lc[lemma] = True
    lc = len(lc.keys())
    print "Lemma count", lc
    # Build the jobs.
    start = 0
    finish = job_length
    jobs = []
    ppservers = ()
    job_server = pp.Server(ppservers=ppservers)
    while start < lc:
        jobs.append(job_server.submit(sentiment_chunk, (specs, start, finish), (), ("sentimentlexicon","yaml")))    
        start = finish
        finish += job_length
        if finish > lc: finish = lc
    print len(jobs), "jobs created; now running ..."
    all_sentiment = {}
    for job in jobs:
        sentiment = job()
        for key,val in sentiment.items():
            all_sentiment[key] = val
    job_server.print_stats()
    output_filename = "wn.%s.%s.yaml" % (specs["classification"], specs["pos"])
    yaml.dump(all_sentiment, file(output_filename, "w"))


######################################################################
    
hedges_r = {
    "positive_filename": "seeds/hedges/certain.r.txt",
    "negative_filename": "seeds/hedges/uncertain.r.txt",
    "neutral_filename":  "seeds/hedges/neutral.r.txt",
    "pos": "r",
    "classification": "hedges"}

#run_pos_single(hedges_r)
#run_pos_parallel(hedges_r, 500)
