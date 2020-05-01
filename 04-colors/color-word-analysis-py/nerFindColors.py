"""
Use the custom language model previously trained to find
colors.
"""

import spacy
import sqlite3
import json
from colorMaps import xkcdMap, masterMap

# print('Loading model')
nlp = spacy.load('/home/jon/Code/prodigy/colorModel2')

nlp.max_length = 8000000


def lookup(color):
    # return xkcdMap.get(color)
    xkcdHex = xkcdMap.get(color)
    if xkcdHex is not None:
        return xkcdHex
    else:
        return masterMap.get(color)

ids = open('/run/media/jon/Sekurkopioj/Corpora/ids').read().split()
conn = sqlite3.connect('/run/media/jon/Sekurkopioj/Corpora/pg-text-7.db')
c = conn.cursor()

results = open('pg-results.jsonl', 'a')

for bookId in ids:
    print("Now analyzing book: ", bookId)
    c.execute('SELECT text FROM text where id=?', [bookId])
    text = c.fetchone()[0]

    doc = nlp(text)

    entDict = {ent.lemma_: [] for ent in doc.ents}

    for ent in doc.ents:
        entDict[ent.lemma_].append((ent.start_char, ent.end_char))

    hexes = {ent: lookup(ent) for ent in entDict
             if lookup(ent) is not None}

    data = {bookId: {"length": len(text),
                     "totalNColors": len(doc.ents),
                     "colorCounts": {ent: len(entDict[ent]) for ent in hexes},
                     "colorHexes": hexes,
                     "colorLocs": {ent: locs for ent, locs in entDict.items()
                                   if locs is not None}
                     }
            }

    results.write(json.dumps(data))
