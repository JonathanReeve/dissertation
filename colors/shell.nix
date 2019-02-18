with import <nixpkgs> {};

((
let pandas = pandas.overrideAttrs(oldAttrs: { doCheck = false; });
in python37.withPackages (ps: with ps; [
  matplotlib
  gensim
  spacy
  ftfy
  spacy_models.en_core_web_lg
  jupyter
  scikitlearn
  six
  nltk
])).override({ignoreCollisions=true; })).env
