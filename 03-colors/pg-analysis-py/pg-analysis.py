
from glob import glob
import sqlite3
import os
import argparse
import logging

dataLocation = '/media/jon/Sekurkopioj/Corpora'
logging.basicConfig(format='%(asctime)s %(message)s', level=logging.DEBUG)

parser = argparse.ArgumentParser(
    description='Run a color word analysis over a text database.')
parser.add_argument('--dbPath', type=str,
                    help='Path to the database location')
parser.add_argument('--programPath', type=str,
                    help='Path to the program location')
args = parser.parse_args()

conn = sqlite3.connect(f"{dataLocation}/pg-text-7.db")
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

chunkSize = 40
idChunks = [idList[x: x+chunkSize] for x in range(0, len(idList), chunkSize)]


def slugify(text):
    return "".join(x for x in text if x.isalnum())


def getResult(default):
    result = c.fetchone()
    if result is not None:
        return result[0]
    else:
        return default

for idChunk in idChunks[31:]:
    filenames = []
    for bookId in sorted(idChunk):
        bookIdSanitized = str(float(bookId))
        c.execute('SELECT text from text where id=?;', [bookId])
        text = getResult("")
        if text == "":
            continue  # Skip it if there's no text.
        c.execute('SELECT title from meta where id=?;', [bookIdSanitized])
        title = getResult(bookId)
        if len(title) > 50:
            title = title[:51]  # Truncate titles
        c.execute('SELECT gr_pubDate from meta where id=?;', [bookIdSanitized])
        pubDate = getResult(bookId)
        filename = f"{slugify(pubDate)}-{slugify(title)}-{bookIdSanitized}"
        filenames.append(filename)
        open(f'/tmp/{filename}', 'w').write(text)
    filesToAnalyze = list(set(sorted([f"/tmp/{filename}" for filename in filenames])))
    filesStr = " ".join(filesToAnalyze)
    chunkIndex = idChunks.index(idChunk)
    command = f"../color-word-analyzer/color-word-analyzer-cli --colorMap=XKCD {filesStr} > results-{chunkIndex}.html"
    logging.info(f'Analyzing {len(filesToAnalyze)} files in batch {chunkIndex} of {len(idChunks)}')
    os.system(command)
    for bookFile in filesToAnalyze:
        try:
            os.remove(bookFile)
        except:
            continue
