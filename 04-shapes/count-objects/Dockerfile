FROM jupyter/datascience-notebook
RUN pip install torch -f https://download.pytorch.org/whl/cpu/torch-1.11.0%2Bcpu-cp38-cp38-linux_x86_64.whl
RUN pip install torchvision torch-scatter torch-sparse -f https://pytorch-geometric.com/whl/torch-1.11.0+cpu.html
RUN git clone https://github.com/pytorch/fairseq
RUN cd fairseq && pip install --editable ./
RUN git clone https://github.com/SapienzaNLP/ewiser.git
RUN cd ewiser && pip install -r requirements.txt && pip install -e .
RUN pip install spacy
RUN python -m spacy download en_core_web_trf
COPY ewiser.semcor+wngt.pt ewiser/bin
COPY test.txt ewiser/bin
RUN python -c "import nltk; nltk.download('wordnet'); nltk.download('omw-1.4')"
RUN cd ewiser/bin && python annotate.py -c ewiser.semcor+wngt.pt -s en_core_web_trf test.txt
COPY bin/myannotate.py ewiser/bin
RUN cd ewiser/bin && python myannotate.py
