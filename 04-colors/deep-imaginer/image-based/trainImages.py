"""
Given a text, parse it with Spacy, then get nouns and imagine them
using images.
"""

import logging
import spacy
import argparse
import json
from time import sleep
from glob import glob

import downloadImages
import processImages

imgLocation = "../models/img"

def train(fn, langModel):
    nlp = langModel
    model = {}
    logging.info(f'Reading text {fn}')
    with open(fn) as f:
        rawText = f.read()
    logging.info(f'Text length: {len(rawText)}')
    if len(rawText) > 4000000:
        textParts = [rawText[i:i + n] for i in range(0, len(rawText), n)]
        logging.info(f'Breaking text into {len(textParts)} parts.')
    else:
        textParts = [rawText]
    for textPart in textParts:
        logging.info(f"Processing part {textParts.index(textPart) + 1} of {len(textParts)}") 
        textDoc = nlp(textPart)
        logging.info(f"Part has {len(textDoc)} words.")
        nouns = list(set([str(w.lemma_) for w in textDoc if w.tag_ == "NN" and w.is_alpha]))[:200]
        logging.info(f"About to query image service for these nouns: {nouns}")
        # exit()
        alreadyHaveIt = [fn.split('/')[-1] for fn in glob(f'{imgLocation}/*')]
        for noun in nouns:
            if noun in alreadyHaveIt:
                continue
            logging.info(f"Quering image service for word: {noun} number {nouns.index(noun)} of {len(nouns)}")
            result = downloadImages.main(noun)
            if result is not None:
              sleep(1)
              processImages.main(noun, 3)
    return nouns # I could use this later

if __name__ == '__main__':
    logging.basicConfig(level=logging.INFO)

    parser = argparse.ArgumentParser(description='Train a model on new color vectors from images.')
    parser.add_argument('--model', help='the existing model, if it exists')
    parser.add_argument('--langModel', help='the spacy language model')
    parser.add_argument('--colorList', help='the color list to use')
    parser.add_argument('files', nargs='+', help='the files to train on')

    args = parser.parse_args()

    if args.langModel is None:
        logging.info('Loading language model...')
        nlp = spacy.load('en_core_web_lg')
        logging.info('done.')
    else:
        nlp = args.langModel

    if not args.model:
        model = "model.json"
    else:
        model = args.model

    logging.info(f'Training model {model} using language model {nlp} and files {args.files}')

    for f in args.files:
        # Grab the most recent nouns just so we can make an example
        nouns = train(f, nlp)

    # Write out example
    wordDict = {}
    jsonFiles = glob(f'../models/json/*.json')
    for noun in nouns:
        try:
            with open(f'../models/json/{noun}.json') as f:
                nounJson = json.load(f)
                wordDict[noun] = nounJson[noun]['secondary-mixed']
        except FileNotFoundError:
            logging.info(f'{noun} not in ../models/json')

    with open(model, 'w') as f:
        json.dump(wordDict, f)


    
    # logging.info(f'Updating model {model}')

    # with open(model) as f:
    #     decodedModel = json.load(f)


    # with open(model, 'wb') as f:
    #     json.dump(updatedModel, f)

