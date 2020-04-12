FROM jupyter/datascience-notebook
COPY prodigy-1.9.9-cp36.cp37.cp38-cp36m.cp37m.cp38-linux_x86_64.whl /home/jovyan
RUN pip install prodigy-1.9.9-cp36.cp37.cp38-cp36m.cp37m.cp38-linux_x86_64.whl
RUN pip install spacy
RUN python -m spacy download en_core_web_lg
