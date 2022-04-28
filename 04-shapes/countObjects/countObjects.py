import pywsd
from nltk import sent_tokenize
from nltk.corpus import wordnet as wn
from collections import Counter
import argparse
import logging
import json

logging.basicConfig(level=logging.INFO)

def wsd(sents):
    return [pywsd.disambiguate(sent, algorithm=pywsd.lesk.adapted_lesk)
                 for sent in sents]

def isArtifact(lemma):
    return artifact in [item[0] for item in lemma.hypernym_distances()]

def getArtifacts(disambiguated):
    logging.info(f'Extracting artifacts...')
    artifacts = []
    nSents = len(disambiguated)
    for i, sent in enumerate(disambiguated):
        for word, syn in sent:
            if syn is not None:
                if isArtifact(syn):
                    artifacts.append((i, nSents, word, syn))
    return artifacts


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Count the number of artifacts in a text, according to their WordNet categories.")
    parser.add_argument('filename')
    parser.add_argument('--verbose', action='store_true', help="Print out everything")
    args = parser.parse_args()
    fn = args.filename
    logging.info(f"Reading file {fn}")
    with open(fn) as f:
        raw = f.read()
    sents = sent_tokenize(raw)
    nSents = len(sents)

    logging.info(f'Disambiguating {nSents} sentences...')
    disambiguated = wsd(sents)
    # print(disambiguated)
    artifact = wn.synsets('artifact')[0]
    # print(artifact)
    artifacts = getArtifacts(disambiguated)

    if args.verbose:
        print(artifacts)

    artifactSyns = [item[3].name() for item in artifacts]
    counter = Counter(artifactSyns).most_common(40)
    logging.info(f"Total artifacts: {len(artifacts)}")
    out = json.dumps({fn: dict(counter)})
    print(out)
