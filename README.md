# CI-Quality Study REPLICATION PACKAGE
Replication package of Continuous Integration and Software Quality: A Causal Explanatory Study

For MSR replication in the step 4, we provide a docker infrastructure to provide the correct environment for replication.
This may use aproximately 15GB of your disc.


## Replication Instructions
The methodology is organizazed in 7 steps. Below we present instructions for each step:

#### 1- Literature Survey
  - "1-Literature Survey/Literature Survey.ods"
    - File containing literature claims and its references.
    - The last three sheets contain a summary of associations.



#### 2- General Causal DAG
  - Tool to model causal DAGs: http://dagitty.net/
    - This tool allows us to draw causal DAGs and point out the minimal adjustment set, as well the conditional independencies implication (to test statistically). Therefore, this tool makes the job easier and faster (It is possible to use the dagitty R package to the same functionalities).

  - General Model with bugs extratified:
    - "2-General DAG/1-model_ci_bugs_extratified.pdf"
  - Tests - Bugs associations:
    - "2-General DAG/2-model_tests_bugs.pdf"
  - Build attributes - Bugs Associations:
    - "2-General DAG/3-model_build_bugs.pdf"
  - Unified DAG
    - "2-General DAG/4-model_Unified_General.pdf
  - Unified Dag cleaned into CI (Unified Dag with automated tests, time to fix, build health, integration frequency represented inside CI).
    - "2-General DAG/5-model_Unified_General_CIsimplified.pdf"


#### 3- Variable Cleaning and Bias Identification
  - Causal DAG cleaned on variables of interest.
    - "3-Causal DAG-Cleaned/1-model_Causal_Literature.pdf"
  - Causal DAG with Age variable
    - "3-Causal DAG-Cleaned/2-model_Causal_Literature_Age.pdf"
  
#### 4- Mining Software Repositories (MSR)
  - "4-MSR/..."
    - This folder contains python notebooks (Jupyter) with scripts to mine the repositories.
  - INSTRUCTIONS TO ACCESS DATA AND NOTEBOOKS:
    - You need to have Docker installed in your machine.
    - (These docker containers instantiation may use aproximately 15GB of your disc.)

  - FOR DATABASE INSTANTIATION:
    - Open Terminal into repository root folder.
    - Run the docker commands below IN THE FIRST USAGE:
      - docker image build -t study-db .
      - docker container run -it -p 5433:5432 --name study-db-server study-db
    
    - To use after first instantiation:
      - docker container start -ai study-db-server  
      
    - Ready! A postgres instance is listening on http://localhost:5433

  - FOR JUPYTER NOTEBOOKS INSTANTIATION:
    - Open Terminal into DockerJupyter folder.
    - Run the docker commands below IN THE FIRST USAGE:
      - docker image build -t study-notebooks 4-MSR/Dockerfile
      - docker run -d --name jupyterserver -p 8889:8888 -e JUPYTER_ENABLE_LAB=yes study-notebooks start-notebook.sh --NotebookApp.password='' --NotebookApp.token=''
      
    - To use after first instantiation:
      - docker container start -ai jupyterserver  

    - Ready! A Jupyter Notebook server instance is listening on http://localhost:8889

        
#### 5- Data-Driven DAG Evaluation
  - R file to run (un)conditional independencies tests:
    - "5-Data-Driven_Evaluation/dcov-KCI_tests_data-theory_v6.R"
  

#### (*) Database Dump
  - If you desire download the database dump without use docker, you may access through the link below:
  - https://drive.google.com/file/d/1wYBZ3ytDi3SB6cBJ8uZ5I7uxBfp3PVcd/view?usp=drive_link

