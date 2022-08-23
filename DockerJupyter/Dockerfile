FROM jupyter/datascience-notebook:0fd03d9356de
LABEL maintainer="Anonymous for ICSE submission"

USER root
RUN conda install --quiet --yes \
    'matplotlib-base' \
    'scipy' && \
    conda clean --all -f -y
USER 1001


RUN useradd study && \
    mkdir /notebooks && \
    chown 1001 /notebooks

COPY ../4-MSR/* /notebooks/

WORKDIR /notebooks
EXPOSE 8000