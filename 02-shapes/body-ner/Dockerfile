# Usage:
# docker build . --build-arg KEY=$KEY
# And make sure Prodigy license key is set to the KEY env var. 
FROM python
ARG KEY
RUN pip install prodigy -f https://$KEY@download.prodi.gy
RUN python -m spacy download en_core_web_lg
ENV PRODIGY_CONFIG_OVERRIDES '{"host": "0.0.0.0", "db": "sqlite", "db_settings": {"name":"prodigy.db", "path":"/mnt/prodigy.db"}}'
ENV PRODIGY_HOME /mnt
RUN pip install jupyter jupyterlab 
WORKDIR /mnt
EXPOSE 8080
