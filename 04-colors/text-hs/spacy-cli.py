import spacy
import argparse
import logging

logger = logging.getLogger('spacy')

logger.info('Loading...')
nlp = spacy.load('en_core_web_md')

parser = argparse.ArgumentParser(description='Parse a document with SpaCy')

parser.add_argument('files', metavar='files', type=open, nargs='+',
                    help='input text files')

args = parser.parse_args()
logger.info('Files: ', args.files)

for f in args.files:
    logger.info('Parsing file: %s' % f.name)
    doc = nlp(f.read())
    print(doc.to_json())
