library(DBI)
library(RODBC)
library(RPostgreSQL)


#create connection object
con <- dbConnect(drv =PostgreSQL(), 
                 user="commitminer", 
                 password="commitminer",
                 host="localhost", 
                 port=5432, 
                 dbname="Causal_CI_Quality_v4")



##########################################################################
# To use the "pcalg" package, we need two auxilliary packages "RBGL"
# and "graph", which are not available on CRAN but on BioConductor
##########################################################################
library(kpcalg)
library(CondIndTests)
library(energy)

#install.packages('dHSIC')
library(dHSIC)


###############################################################
################### QUERIES TO OBTAIN DATA ####################
###############################################################

ci <- dbGetQuery(con, "SELECT CASE WHEN CI=TRUE THEN 1 WHEN CI=FALSE THEN 0 END CI FROM METRICS_RELEASES where repo_name IN (select repo_name from projects where rq1_included is true) ORDER BY REPO_NAME,created_at asc")
bugs <- dbGetQuery(con, "SELECT bugs FROM METRICS_RELEASES where repo_name IN (select repo_name from projects where rq1_included is true) ORDER BY REPO_NAME,created_at asc")
#commit_s <- dbGetQuery(con, "SELECT commit_size_mean commit_size FROM METRICS_RELEASES where repo_name IN (select repo_name from projects where rq1_included is true) ORDER BY REPO_NAME,created_at asc")
#commit frequency as commit_size
commit_s <- dbGetQuery(con, "SELECT qty_commits commit_size FROM METRICS_RELEASES where repo_name IN (select repo_name from projects where rq1_included is true) ORDER BY REPO_NAME,created_at asc")
#merge_c <- dbGetQuery(con, "SELECT merge_conflicts Merge_Conf FROM METRICS_RELEASES where repo_name IN (select repo_name from projects where rq1_included is true) ORDER BY REPO_NAME,created_at asc")
#merge_c <- dbGetQuery(con, "SELECT merge_conflicts_git Merge_Conf FROM METRICS_RELEASES where repo_name IN (select repo_name from projects where rq1_included is true) ORDER BY REPO_NAME,created_at asc")
merge_c <- dbGetQuery(con, "SELECT merge_conflicts_gitv2 Merge_Conf FROM METRICS_RELEASES where repo_name IN (select repo_name from projects where rq1_included is true) ORDER BY REPO_NAME,created_at asc")
communication <- dbGetQuery(con, "SELECT communication_mean Communication FROM METRICS_RELEASES where repo_name IN (select repo_name from projects where rq1_included is true) ORDER BY REPO_NAME,created_at asc")
test_v <- dbGetQuery(con, "SELECT test_volume_proportional test_vol FROM METRICS_RELEASES where repo_name IN (select repo_name from projects where rq1_included is true) ORDER BY REPO_NAME,created_at asc")
age <- dbGetQuery(con, "SELECT age FROM METRICS_RELEASES where repo_name IN (select repo_name from projects where rq1_included is true) ORDER BY REPO_NAME,created_at asc")



df <- data.frame(ci,bugs,commit_s,merge_c,communication,test_v,age)#,issue_type)
df[is.na(df)]=0
df





#Verify normality with Anderson-Darling normality test
library(nortest)
ad.test(df$ci)
shapiro.test(df$merge_c)
ad.test(df$bugs)
ad.test(df$commit_size)
ad.test(df$merge_c)
ad.test(df$communication)
ad.test(df$test_v)
ad.test(df$age)
#ad.test(df$issue_type)



#1-ci
#2-bugs
#3-commit_s
#4-merge_c
#5-communication
#6-test_v
#7-age
#####################################################
##  Literature-driven Model 
##  http://dagitty.net/meZB6lx
## dcov R < 1    - independent
## KCI  p > 0.05 - independent
#####################################################

#H1	Age ⊥ Commit_Frequency | Continuous_Integration                                                                               1.41219e-07
CondIndTest(df$age,df$commit_size,df$ci, method = "KCI")

#H2	Age ⊥ Tests_Volume | Continuous_Integration                                                                                   2.395195e-12
CondIndTest(df$age,df$test_vol,df$ci, method = "KCI")

#H3	Age ⊥ Communication | Continuous_Integration                                                                                  1.162458e-06
CondIndTest(df$age,df$communication,df$ci, method = "KCI")

#H4	Age ⊥ Merge_Conflicts | Continuous_Integration                                                                                0.2549253
CondIndTest(df$age,df$merge_conf,df$ci, method = "KCI")

#H5	Bug_Report ⊥ Commit_Frequency | Communication, Continuous_Integration, Tests_Volume                                           0
dfa <- data.frame(ci,communication,test_v)
dfa[is.na(dfa)]=0
CondIndTest(df$commit_size,df$bugs,dfa, method = "KCI")

#H6	Bug_Report ⊥ Merge_Conflicts | Commit_Frequency, Continuous_Integration                                                       0.3245861
dfa <- data.frame(ci,commit_s)
dfa[is.na(dfa)]=0
CondIndTest(df$bugs,df$merge_conf,dfa, method = "KCI")

#H7	Bug_Report ⊥ Merge_Conflicts | Communication, Continuous_Integration, Tests_Volume                                          0.344638
dfa <- data.frame(age,communication,ci,test_v)
dfa[is.na(dfa)]=0
CondIndTest(df$bugs,df$merge_conf,dfa, method = "KCI")

#H8	Communication ⊥ Merge_Conflicts | Commit_Frequency, Continuous_Integration                                                    0.4865323
dfa <- data.frame(commit_s,ci)
dfa[is.na(dfa)]=0
CondIndTest(df$communication,df$merge_conf,dfa, method = "KCI")

#H9	Communication ⊥ Tests_Volume | Continuous_Integration                                                                       0
CondIndTest(df$communication,df$test_vol,df$ci, method = "KCI")

#H10	Merge_Conflicts ⊥ Tests_Volume | Commit_Frequency, Continuous_Integration                                                 0.2465839
dfa <- data.frame(commit_s,ci)
dfa[is.na(dfa)]=0
CondIndTest(df$merge_conf,df$test_vol,dfa, method = "KCI")




#####################################################
##  Literature-driven Model - Failed Hypothesis
## dcov R < 1    - independent
## KCI  p > 0.05 - independent
#####################################################

#H1	Age ⊥ Commit_Frequency | Continuous_Integration
CondIndTest(df$age,df$commit_size,df$ci, method = "KCI")

  # Add an edge between Age -> Commit Frequency
  dcov.test(df$age,df$commit_size,R=200)                        #49.5566 
  CondIndTest(df$age,df$commit_size,df$bugs, method = "KCI")    #0
  
  dfa <- data.frame(bugs,ci)
  dfa[is.na(dfa)]=0
  CondIndTest(df$age,df$commit_size,dfa, method = "KCI")        #2.576492e-08

  
#H2	Age ⊥ Tests_Volume | Continuous_Integration
CondIndTest(df$age,df$test_vol,df$ci, method = "KCI")
  
  #Add an edge between age --> tests_volume
  dfa <- data.frame(bugs,ci)
  dfa[is.na(dfa)]=0
  CondIndTest(df$age,df$test_vol,dfa, method = "KCI")        #8.518427e-10
  
  CondIndTest(df$age,df$test_vol,df$bugs, method = "KCI")        #0
  
  dcov.test(df$age,df$test_vol,R=200)                        #4.308763
  
#H3	Age ⊥ Communication | Continuous_Integration
CondIndTest(df$age,df$communication,df$ci, method = "KCI")

  #Add an edge between age --> communication
  dfa <- data.frame(bugs,ci)
  dfa[is.na(dfa)]=0
  CondIndTest(df$age,df$communication,dfa, method = "KCI")        #1.251478e-05

  CondIndTest(df$age,df$communication,df$bugs, method = "KCI")
  
  dcov.test(df$age,df$communication,R=200)                        #11.86268

#H5	Bug_Report ⊥ Commit_Frequency | Communication, Continuous_Integration, Tests_Volume
dfa <- data.frame(ci,communication,test_v)
dfa[is.na(dfa)]=0
CondIndTest(df$commit_size,df$bugs,dfa, method = "KCI")

    #Add an edge between commit_frequency -> Bug reports
    dcov.test(df$bugs,df$commit_size,R=200)                     #6.009902 
    
#H9	Communication ⊥ Tests_Volume | Continuous_Integration
    CondIndTest(df$ci,df$test_vol,df$commit_size, method = "KCI") #0
    
    #Reorient the edge between Commit_frequency -> issue_type
    #We tested if adjusting for commit_frequency the association is interrupted
    #this suggests that the edge between commit_frequency and issue_type must be reoriented
    dfa <- data.frame(ci,commit_s)
    dfa[is.na(dfa)]=0
    CondIndTest(df$ci,df$test_vol,dfa, method = "KCI")          #0.8623259
    
    
    

#####################################################
## theory_data_v12_1
## http://dagitty.net/mQCW3V9
## dcov R < 1    - independent
## KCI  p > 0.05 - independent
#####################################################

    
#H1	Age ⊥ Merge_Conflicts | Commit_Frequency, Continuous_Integration                        #0.2696356 / 0.2685542
dfa <- data.frame(ci,commit_s)
dfa[is.na(dfa)]=0
CondIndTest(df$age,df$merge_conf,dfa, method = "KCI")

#H2	Bug_Report ⊥ Merge_Conflicts | Commit_Frequency, Continuous_Integration                 #0.3213383 / 0.3175822
dfa <- data.frame(commit_s,ci)
dfa[is.na(dfa)]=0
CondIndTest(df$bugs,df$merge_conf,dfa, method = "KCI")

#H3	Communication ⊥ Merge_Conflicts | Commit_Frequency, Continuous_Integration              #0.4865323 / 0.3262914
dfa <- data.frame(commit_s,ci)
dfa[is.na(dfa)]=0
CondIndTest(df$communication,df$merge_conf,dfa, method = "KCI")

#H4	Communication ⊥ Tests_Volume | Age, Commit_Frequency, Continuous_Integration                # 2.047917e-12
dfa <- data.frame(age,commit_s,ci)
dfa[is.na(dfa)]=0
CondIndTest(df$communication,df$test_vol,dfa, method = "KCI")

#H5	Merge_Conflicts ⊥ Tests_Volume | Commit_Frequency, Continuous_Integration               #0.384515 / 0.2465839
dfa <- data.frame(commit_s,ci)
dfa[is.na(dfa)]=0
CondIndTest(df$merge_conf,df$test_vol,dfa, method = "KCI")
    

#####################################################
##  theory_data_v12_1 - Failed Hypothesis
## dcov R < 1    - independent
## KCI  p > 0.05 - independent
#####################################################

#H4	Communication ⊥ Tests_Volume | Age, Commit_Frequency, Continuous_Integration                # 2.047917e-12
dfa <- data.frame(age,commit_s,ci)
dfa[is.na(dfa)]=0
CondIndTest(df$communication,df$test_vol,dfa, method = "KCI")
  
      #If the edge communication -> bug was the opposite?
      #Then when conditioning by communication the flows would be openned
      #let's try to test without communication
      CondIndTest(df$communication,df$test_vol,df$bugs, method = "KCI")           #0
      
      # Add an edge
