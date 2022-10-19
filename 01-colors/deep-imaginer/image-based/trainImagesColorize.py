"""
Given a text, parse it with Spacy, then get nouns and imagine them
using images.
"""

import logging
import spacy
import argparse
from time import sleep
from glob import glob
import json
from collections import Counter

import processImages
import colorizeImages

def train(fn, langModel, model, n):
    nlp = langModel
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
        nounsCounter = list(Counter(list(set([str(w.lemma_) for w in textDoc if w.tag_ == "NN" and w.is_alpha]))).most_common(200))
        nouns = [pair[0] for pair in nounsCounter]
        logging.info(f"About to query Colorize for these nouns: {nouns}")
        # exit()
        for noun in nouns:
            if noun in model:
                continue
            logging.info(f"Querying Colorize for word: {noun}, {nouns.index(noun)} of {len(nouns)}")
            result = colorizeImages.main(noun)
            if result is not None:
                model[noun] = result
    return model

if __name__ == '__main__':
    logging.basicConfig(level=logging.INFO)

    parser = argparse.ArgumentParser(description='Train a model on new color vectors from images.')
    parser.add_argument('--model', help='the existing model, if it exists')
    parser.add_argument('--langModel', help='the spacy language model')
    parser.add_argument('--colorList', help='the color list to use')
    parser.add_argument('--n', help='the number of nouns to get')
    parser.add_argument('files', nargs='+', help='the files to train on')

    args = parser.parse_args()

    if args.langModel is None:
        logging.info('Loading language model...')
        nlp = spacy.load('en_core_web_lg')
        logging.info('done.')
    else:
        nlp = args.langModel

    if args.n is None:
        n = 100
    else:
        n = int(args.n)

    defaultResultsFilename = 'colorize-results.json'

    if args.model is None:
        with open(defaultResultsFilename) as f:
            model = json.load(f)
    else:
        with open(args.model) as f:
            model = json.load(f)

    logging.info(f'Training model {model} using language model {nlp} and files {args.files}')

    for f in args.files:
        results = train(f, nlp, model, n)

    with open(defaultResultsFilename, 'w') as f:
        outText = json.dumps(model)
        f.write(outText)
