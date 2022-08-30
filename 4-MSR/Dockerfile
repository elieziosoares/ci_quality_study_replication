FROM jupyter/base-notebook:latest
LABEL maintainer="Anonymous for ICSE submission"

USER root
#RUN conda install --quiet --yes \
#    'matplotlib-base' \
#    'scipy' && \
#    conda clean --all -f -y

#RUN useradd study && \
#    mkdir /notebooks && \
#    chown study /notebooks

#USER study

COPY *.ipynb /home/jovyan/work/
COPY psql_pwd.txt /home
#COPY ../4-MSR /notebooks

WORKDIR /home/jovyan/work/
EXPOSE 8889