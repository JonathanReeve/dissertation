"""
Given a text, parse it with Spacy, then get nouns and imagine them
using images from Wikimedia Commons.

TODO: merge this with other models 
"""

import logging
import spacy
import argparse
from time import sleep
from glob import glob
import json
import os
from itertools import zip_longest

from commonsdownloader import commonsdownloader

import processImages
import colorizeImages

def train(fn, langModel, model=None):
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
        nouns = list(set([str(w.lemma_) for w in textDoc if w.tag_ == "NN" and w.is_alpha]))
        logging.info(f"About to query Wikimedia Commons for these nouns: {nouns}")
        # exit()
        existingWords = glob('commons-img/*')
        for noun in nouns:
            if noun in existingWords:
                continue
            logging.info(f"Downloading images from Wikimedia Commons for word: {noun}")
            os.system(f'mkdir -p commons-img/{noun}')
            outPath = f'commons-img/{noun}/'
            fileNames = commonsdownloader.get_category_files_from_api(noun)
            fileNames = list(fileNames)
            if len(fileNames) > 5:
                fileNames = fileNames[:5]
            logging.info(f"Downloading images: {fileNames}")
            filesToDownload = zip_longest(fileNames, [], fillvalue=100)
            commonsdownloader.download_files_if_not_in_manifest(filesToDownload, outPath)

if __name__ == '__main__':
    logging.basicConfig(level=logging.INFO)

    parser = argparse.ArgumentParser(description='Train a model on new color vectors from images.')
    # parser.add_argument('--model', help='the existing model, if it exists')
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

    # defaultResultsFilename = 'colorize-results.json'

    # if args.model is None:
    #     with open(defaultResultsFilename) as f:
    #         model = json.load(f)
    # else:
    #     with open(args.model) as f:
    #         model = json.load(f)

    # logging.info(f'Training model {model} using language model {nlp} and files {args.files}')

    for f in args.files:
        results = train(f, nlp, model=None)

    with open(defaultResultsFilename) as f:
        f.write(results)
