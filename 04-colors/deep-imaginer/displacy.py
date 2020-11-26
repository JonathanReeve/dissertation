import spacy

nlp = spacy.load('en_core_web_lg')

doc = nlp('slopes of bright green grassland lay above them')

spacy.displacy.serve(doc, style='dep')
