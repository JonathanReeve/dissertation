#!/usr/bin/env python3


import spacy
from glob import glob
import json
import os

nlp = spacy.load('./out/model-best/')

nlp.max_length = 10000000

files = glob('../count-objects/pg-text2/*')

for book in sorted(files):
    basename = book.split('/')[-1]
    outFilename = f"results/{basename}.json"
    if os.path.isfile(outFilename):
        print(f"Skipping {book} since we already have it.")
        continue
    print(f"Loading {book}...")
    with open(book) as f:
        raw = f.read()
    print(f"Processing {book}...")
    doc = nlp(raw)
    data = []
    for sent in doc.sents:
        for ent in sent.ents:
            data.append({"text": ent.text, "lemma": ent.lemma_,
                        "start_char": ent.start_char,
                        "end_char": ent.end_char,
                        "sentence": str(ent.sent).replace('\n', ' ')})
    basename = book.split('/')[-1]
    outFilename = f"results/{basename}.json"
    print(f"Writing to {outFilename}...")
    with open(outFilename, 'w') as f:
          json.dump(data, f)
