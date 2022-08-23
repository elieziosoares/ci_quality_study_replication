FROM postgres:11
LABEL maintainer="Anonymous for ICSE submission"


ENV POSTGRES_USER=ci_quality
ENV POSTGRES_PASSWORD=ci_quality

RUN  apt-get update \
  && apt-get install -y wget \
  && rm -rf /var/lib/apt/lists/*
RUN wget --load-cookies /tmp/cookies.txt "https://docs.google.com/uc?export=download&confirm=$(wget --quiet --save-cookies /tmp/cookies.txt --keep-session-cookies --no-check-certificate 'https://docs.google.com/uc?export=download&id=19XlRFEZtWbnEJsB-aMZ1V4HkW7zrqGzJ' -O- | sed -rn 's/.*confirm=([0-9A-Za-z_]+).*/\1\n/p')&id=19XlRFEZtWbnEJsB-aMZ1V4HkW7zrqGzJ" -O backup.sql && rm -rf /tmp/cookies.txt

COPY restore_database.sh /docker-entrypoint-initdb.d/restore_database.sh
RUN sed -i 's/\r$//g' /docker-entrypoint-initdb.d/restore_database.sh
RUN chmod 777 /docker-entrypoint-initdb.d/restore_database.sh

EXPOSE 5432