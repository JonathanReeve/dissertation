import spacy
from collections import Counter
import re
import logging
import json

logging.basicConfig(level=logging.INFO)

def main(modelFn='dictionaryModel.json'):

    baseColors = loadBaseColors()

    model = loadModel()

    with open('../../data/dict/pg')
    websterDict =

    logging.info('Vectorizing Webster...')
    websterModel = vectorizeDict(websterDict, langModel=nlp, baseColors=baseColors)
    logging.info(f'Webster model preview: {str(websterModel)[:30]}')

    logging.info('Processing Nutall...')
    with open('../../data/dict/pg12342.txt') as f:
        nutallRaw = f.read()
    nutallDict = processNutall(nutallRaw)

    logging.info('Loading language model...')
    nlp = spacy.load('en_core_web_lg')
    logging.info('done.')

    logging.info('Vectorizing Nutall...')
    nutallModel = vectorizeDict(nutallDict, langModel=nlp, baseColors=baseColors)
    logging.info(f'Nutall model preview: {str(nutallModel)[:30]}')

    logging.info('Vectorizing Nutall...')
    nutallModel = vectorizeDict(nutallDict, langModel=nlp, baseColors=baseColors)
    logging.info(f'Nutall model preview: {str(nutallModel)[:30]}')

    logging.info('Vectorizing Chambers...')
    chambersIds = [ 37683, 38538, 38699, 38700 ]
    chambersTexts = [ open(f'../../data/dict/pg{fn}.txt').read() for fn in chambersIds ]
    chambersModel = {}
    for chambersText in chambersTexts:
        logging.info(f'Processing chambers text number {chambersTexts.index(chambersText)}')
        processed = processNutall(chambersText)
        chambersVecs = vectorizeDict(processed, langModel=nlp, baseColors=baseColors)
        chambersModel = mergeModels(chambersModel, chambersVecs)

    logging.info(f'Chambers model preview: {str(chambersModel)[:30]}')
    masterModel = mergeModels(nutallModel, chambersModel)
    logging.info(f'Merged model preview: {str(chambersModel)[:30]}')

    # Sort again
    model = {color: {k: v for k, v in
                    sorted(stats.items(),
                            key=lambda item: item[1], reverse=True)}
            for color, stats in masterModel.items()}

    with open(modelFn, 'w') as f:
        json.dump(model, f, ensure_ascii=False, indent=4)

def loadBaseColors():
    with open('../../data/maps/xkcd/rgb-termsonly.txt') as f:
        xkcdColors = f.readlines()
        xkcdColors = [line.strip() for line in xkcdColors]
    return xkcdColors

def loadModel(modelFn='dictionaryModel.json'):
    try:
        with open(modelFn) as f:
            try:
                model = json.load(f)
                return model
            except json.decoder.JSONDecodeError:
                # Make an empty model
                model = {}
                return model
    except FileNotFoundError:
        # Create the file, since it doesn't exist
        open(modelFn, 'a').close()
    return {}

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

def vectorizeDict(nutallDict, langModel, baseColors):
    """
    Given a dictionary of word: definition,
    score words for color content by giving a point whenever a color word
    appears in a word's definition.
    """
    colorWords = {}
    for word, definition in nutallDict.items():
        #print(word, definition)
        word = word.lower()
        defDoc = langModel(definition)
        for w in defDoc:
            wStr = str(w.lemma_)
            if wStr in baseColors:
                #print(wStr)
                color = wStr
                if color in colorWords:
                    if word in colorWords[color]:
                        colorWords[color][word] += 1
                    else:
                        colorWords[color][word] = 1
                else:
                    colorWords[color] = {word: 1}
    return colorWords

def processDictionaryWord(word):
    # Transform "GENEVA, LAKE OF" to "lake of geneva"
    if ',' in word:
        parts = word.split(',')
        parts.reverse()
        word = ' '.join(parts)
    # Lowercase it
    return word.lower().strip()

def mergeModels(model, newModel):
    logging.info('Merging models.')
    logging.info(f'Model type: {type(model)}, newModel type: {type(newModel)}')
    for color, wordList in newModel.items():
        for word in wordList:
            word = processDictionaryWord(word)
            try:
                if color in model:
                    if word in model[color]:
                        # Add one to the score
                        model[color][word] += 1
                    else:
                        model[color][word] = 1
                else:
                    model[color] = {word: 1}
            except:
                continue
    return model

if __name__ == '__main__':
    main()
