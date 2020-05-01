"""
Given a text, parse it with Spacy, then get nouns and imagine them
using images.
"""

import logging
import spacy
import argparse
from time import sleep
from glob import glob

import unsplashImages
import processImages

def train(fn, langModel):
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
        nouns = list(set([str(w.lemma_) for w in textDoc if w.tag_ == "NN" and w.is_alpha]))[200:300]
        logging.info(f"About to query unsplash for these nouns: {nouns}")
        # exit()
        alreadyHaveIt = glob('img/*')
        for noun in nouns:
            if noun in alreadyHaveIt:
                continue
            logging.info(f"Quering Unsplash for word: {noun}")
            result = unsplashImages.main(noun)
            if result is not None:
              sleep(1)
              processImages.main(noun, 3)

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

    logging.info(f'Training model {args.model} using language model {nlp} and files {args.files}')

    for f in args.files:
        train(f, nlp)
