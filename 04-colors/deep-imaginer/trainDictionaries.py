import spacy
from collections import Counter
import re
import logging
import json

logging.basicConfig(level=logging.INFO)


nutallRaw = open('../data/dict/pg12342.txt').read()

baseColors = ['black', 'grey', 'brown', 'white', 'red', 'orange', 'yellow', 'green', 'blue']

def processNutall(text):
    paras = text.split('\n\n')
    definitions = {}
    for para in paras:
        para = para.replace('\n', ' ')
        result = re.match('([A-Z `,.]{2,}), (.*)', para)
        if result is not None:
            word = result.group(1).strip()
            definition = result.group(2)
            definitions[word] = definition
    return definitions

nutallDict = processNutall(nutallRaw)

with open('../data/dict/pg29765.txt') as f:
    websterRaw = f.read()

def processWebster(websterRaw):
    splitString = re.compile("\n([A-Z-]+)\n").split(websterRaw)
    # Now we should have a list with some items like "AARD-VARK"
    # and then the definitions
    websterDict = {}
    for i, item in enumerate(splitString):
        if item.replace('-', '').isupper():
            nextItem = splitString[i+1]
            if 'Defn:' in nextItem:
                definition = nextItem.split('Defn:')[-1]
                websterDict[item.lower()] = definition.strip().replace('\n', ' ')
    return websterDict

# websterDict = processWebster(websterRaw)

logging.info('Loading language model...')
nlp = spacy.load('en_core_web_lg')
logging.info('done.')

def vectorizeDict(nutallDict):
    """
    Given a dictionary of word: definition,
    score words for color content by giving a point whenever a color word
    appears in a word's definition.
    """
    colorWords = {}
    for word, definition in nutallDict.items():
        #print(word, definition)
        defDoc = nlp(definition)
        for w in defDoc:
            wStr = str(w.lemma_)
            if wStr in baseColors:
                #print(wStr)
                if wStr in colorWords:
                    colorWords[wStr].append(word)
                else:
                    colorWords[wStr] = [word]
    return colorWords

def processDictionaryWord(word):
    # Transform "GENEVA, LAKE OF" to "lake of geneva"
    if ',' in word:
        parts = word.split(',')
        parts.reverse()
        word = ' '.join(parts)
    # Lowercase it
    return word.lower().strip()

newModel = vectorizeDict(nutallDict)

# Should now be { 'blue': ['arthur seat', 'canopus', ...] }

# Merge with existing model.

# websterModel = vectorizeDict(websterDict)

chambersIds = [ 37683, 38538, 38699, 38700 ]
chambersTexts = [ open(f'../data/dict/pg{fn}.txt').read()
                for fn in chambersIds ]

chambersModels = []
for chambersText in chambersTexts:
    processed = processNutall(chambersText)
    chambersModel = vectorizeDict(processed)
    chambersModels.append(chambersModel)

with open('model.json') as f:
    model = json.load(f)

def mergeModels(model, newModel):
  for color, wordList in newModel.items():
      for word in wordList:
          word = processDictionaryWord(word)
          if color in model:
              if word in model[color]:
                  # Add one to the score
                  model[color][word] += 1
              else:
                  model[color][word] = 1
          else:
              model[color] = {word: 1}

mergeModels(model, newModel)

# mergeModels(model, websterModel)

for chambersModel in chambersModels:
    mergeModels(model, chambersModel)


# Sort again
model = {color: {k: v for k, v in
                 sorted(stats.items(),
                        key=lambda item: item[1], reverse=True)}
         for color, stats in model.items()}

with open('model.json', 'w') as f:
    json.dump(model, f, ensure_ascii=False, indent=4)

