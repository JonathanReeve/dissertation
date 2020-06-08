"""
Train a model of colors to their associated words, with scores,
using this as a module, or via CLI.

Example: python train.py --model=model.json \
                         --colorList=../data/maps/xkcd/rgb-termsonly.txt \
                         dalloway.txt
"""

import spacy
from collections import Counter
import re
import logging
import argparse
import json

# Window of words to search to, backwards and forwards
n = 6

posList = ['NN', 'JJ', 'JJR', 'JJR', 'NNS']


def getAllChildren(w):
    lemmas = []
    for child in w.children:
        lemmas.append(child.lemma_)
        if child.children:
            for grandchild in child.children:
                lemmas.append(grandchild.lemma_)
    return lemmas


def getAllParents(w):
    """
    Walk up the dependency tree for a given word, and find all other words
    that are parents or grandparents. So, given "white," this tries to find
    what the word "white" is modifying.

    Returns a tuple of (word, 1) if parent,
    Or (word, 2) if grandparent. """
    words = []
    for ancestor in w.ancestors:
        # If parent, give it a score of 1
        words.append((ancestor, 2))
        if ancestor.ancestors:
            for grandparent in ancestor.ancestors:
                # If grandparent, give it a score of 1/2.
                words.append((grandparent, 1))
    return words


def computeDependentDistances(textDoc, baseColors, posList):
    colorNeighbors = {}
    for w in textDoc:
        if str(w.lemma_) in baseColors:
            neighbors = []
            parents = getAllParents(w)
            # logging.info(f'Parents: {parents}')
            for parent in parents:
                if parent[0].tag_ in posList and parent[0].is_alpha:
                    neighbors.append((str(parent[0].lemma_), parent[1]))
            # logging.info(f'Neighbors: {neighbors}')
            if len(neighbors) > 0:
                wStr = str(w.lemma_)
                if wStr in colorNeighbors:
                    colorNeighbors[wStr] += neighbors
                else:
                    colorNeighbors[wStr] = neighbors
        else:
            continue
    # logging.info(f'Colorneighbors: {colorNeighbors}')
    return colorNeighbors


def computeRawDistances(textDoc, baseColors, posList):
    colorNeighbors = {}
    for w in textDoc:
        if str(w.lemma_) in baseColors:
            neighbors = []
            for wordIndex in range(1, n+1):
                score = 1 / wordIndex
                try:
                    backWord = textDoc[w.i - wordIndex]
                    backWordStr = str(backWord.lemma_)
                    if backWord.tag_ in posList and backWord.is_alpha:
                        neighbors.append((backWordStr, score))
                    frontWord = textDoc[w.i + wordIndex]
                    frontWordStr = str(frontWord.lemma_)
                    if frontWord.tag_ in posList and frontWord.is_alpha:
                        neighbors.append((frontWordStr, score))
                except IndexError:
                    continue
        else:
            continue
        if len(neighbors) > 0:
            wStr = str(w.lemma_)
            if wStr in colorNeighbors:
                colorNeighbors[wStr] += neighbors
            else:
                colorNeighbors[wStr] = neighbors
    return colorNeighbors


def mergeNeighbors(dictA, dictB):
    """
    Merge two dictionaries that look like:
    {"colorWord": [("word", score)]}
    """
    for key in dictA:
        dictA[key] += dictB[key]
    return dictA


def train(model, langModel, colorList, files):
    if langModel is None:
        logging.info('Loading language model...')
        nlp = spacy.load('en_core_web_lg')
        nlp.max_length = 4000000
        logging.info('done.')
    else:
        nlp = langModel

    logging.info(f'Training {model} using language model {langModel}, color list {colorList}, and files {files}')

    if not colorList:
        baseColors = ['black', 'grey', 'brown', 'white', 'red', 'orange', 'yellow', 'green', 'blue']
    else:
        baseColors = open(colorList).readlines()
        baseColors = [color.strip() for color in baseColors]

    texts = files

    for text in texts:
        logging.info(f'Reading text {text}')
        rawText = open(text).read()
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
            # logging.debug(f"Text preview: {textDoc[200:250]} words.")
            colorNeighborsDependent = computeDependentDistances(textDoc, baseColors, posList)
            colorNeighborsRaw = computeRawDistances(textDoc, baseColors, posList)

    colorNeighbors = mergeNeighbors(colorNeighborsDependent, colorNeighborsRaw)

    logging.info(f"Color neighbors preview: {str(colorNeighbors)[:40]}")

    colorNeighborsSums = {}
    for color, scoreList in colorNeighbors.items():
        combinedScores = {}
        for wordAndScore in scoreList:
            word, score = wordAndScore
            if word in combinedScores:
                combinedScores[word] += score
            else:
                combinedScores[word] = score
        colorNeighborsSums[color] = combinedScores

    colorNeighborsSumsSorted = {color: {k: v for k, v in sorted(stats.items(), key=lambda item: item[1], reverse=True)} for color, stats in colorNeighborsSums.items()}

    newColors = colorNeighborsSumsSorted

    logging.info(f"Dictionary preview: {str(newColors)[:40]}")

    # Merge with model
    if model is not None:
        with open(model) as f:
            logging.info('Loading model: %s' % model)
            model = json.load(f)
        for color, stats in newColors.items():
            for word, score in stats.items():
                if color in model:
                    if word in model[color]:
                        model[color][word] += newColors[color][word]
                    else:
                        model[color][word] = newColors[color][word]
                else:
                    model[color] = stats
    else:
        model = colorNeighborsSumsSorted
    # print(colorNeighborsSumsSorted)

    logging.info('Writing model...')
    with open('model.json', 'w', encoding='utf-8') as f:
        json.dump(model, f, ensure_ascii=False, indent=4)


if __name__ == "__main__":
    # We are running this as CLI, so:
    logging.basicConfig(level=logging.INFO)

    parser = argparse.ArgumentParser(description='Train a model on new color vectors.')
    parser.add_argument('--model', help='the existing model, if it exists')
    parser.add_argument('--langModel', help='the spacy language model')
    parser.add_argument('--colorList', help='the color list to use')
    parser.add_argument('files', nargs='+', help='the files to train on')

    args = parser.parse_args()

    logging.info('Training model %s with files: %s' % (args.model, args.files))

    train(model=args.model, langModel=args.langModel, colorList=args.colorList, files=args.files)
