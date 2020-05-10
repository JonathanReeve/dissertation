"""
Use the custom language model previously trained to find
colors.
"""

import spacy
import sqlite3
import json
import re
from colorMaps import xkcdMap, masterMap

# print('Loading model')
nlp = spacy.load('../color-ner/colorModel')

nlp.max_length = 8000000


def lookup(color):
    # return xkcdMap.get(color)
    xkcdHex = xkcdMap.get(color)
    if xkcdHex is not None:
        return xkcdHex
    else:
        return masterMap.get(color)

conn = sqlite3.connect('/media/jon/Sekurkopioj/Corpora/pg-text-7.db')
c = conn.cursor()

# Get only those books with Library of Congress Category "PR"
# (British Literature), and which are written in English.
# c.execute('select id from meta where LCC like "%PR%" and languages like "%en%";')
c.execute(
    """select id from meta
    where LCC like "%PR%"
    and languages like "%en%"
    and (
        gr_pubDate like "188%"
        or gr_pubDate like "189%"
        or gr_pubDate like "190%"
        or gr_pubDate like "191%"
        or gr_pubDate like "192%"
    ) order by gr_pubDate;""")
idList = [item[0] for item in c.fetchall()]

with open('pg-results.jsonl') as f:
    existingData = f.read()
    existingDataLines = existingData.split('\n')
    existingKeys = [line.split(':')[0][2:-1] for line in existingDataLines]

results = open('pg-results.jsonl', 'a')

for bookId in idList:
    if bookId in existingKeys:
        print(f"Book {bookId} already analyzed. Skipping.")
        continue
    print(f"Now analyzing book {bookId}, {idList.index(bookId)} of {len(idList)}")
    c.execute('SELECT text FROM text where id=?', [bookId])
    try:
        text = c.fetchone()[0]
    except TypeError:
        print(f"Error on text {bookId}, skipping")
        continue

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

results.close()
