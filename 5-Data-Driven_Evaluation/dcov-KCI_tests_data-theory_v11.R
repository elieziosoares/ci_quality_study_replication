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
merge_c <- dbGetQuery(con, "SELECT merge_conflicts_git Merge_Conf FROM METRICS_RELEASES where repo_name IN (select repo_name from projects where rq1_included is true) ORDER BY REPO_NAME,created_at asc")
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
##  http://dagitty.net/mpX_p5l
## dcov R < 1    - independent
## KCI  p > 0.05 - independent
#####################################################

#Age ⊥ Commit_Size                                                                                  49.5566 DEPENDENT
dcov.test(df$commit_s,df$age,R=200)

#Age ⊥ Tests_Volume                                                                                 4.308763 DEPENDENT
dcov.test(df$test_vol,df$age,R=200)

#Age ⊥ Communication | Commit_Size, Continuous_Integration                                          0.0001035991 DEPENDENT
dfa <- data.frame(ci,commit_s)
dfa[is.na(dfa)]=0
CondIndTest(df$age,df$communication,dfa, method = "KCI")

#Age ⊥ Merge_Conflicts | Commit_Size, Continuous_Integration                                        0.2696356 
dfa <- data.frame(ci,commit_s)
dfa[is.na(dfa)]=0
CondIndTest(df$age,df$merge_conf,dfa, method = "KCI")

#Bug_Report ⊥ Commit_Size | Age, Communication, Continuous_Integration, Tests_Volume               0  
dfa <- data.frame(ci,communication,test_v,age)
dfa[is.na(dfa)]=0
CondIndTest(df$commit_size,df$bugs,dfa, method = "KCI")

#Bug_Report ⊥ Merge_Conflicts | Commit_Size, Continuous_Integration                               0.3245861  
dfa <- data.frame(ci,commit_s)
dfa[is.na(dfa)]=0
CondIndTest(df$bugs,df$merge_conf,dfa, method = "KCI")

#Bug_Report ⊥ Merge_Conflicts | Age, Communication, Continuous_Integration, Tests_Volume            0.3633449
dfa <- data.frame(age,communication,ci,test_v)
dfa[is.na(dfa)]=0
CondIndTest(df$bugs,df$merge_conf,dfa, method = "KCI")

#Communication ⊥ Merge_Conflicts | Commit_Size, Continuous_Integration                              0.4865323
dfa <- data.frame(commit_s,ci)
dfa[is.na(dfa)]=0
CondIndTest(df$communication,df$merge_conf,dfa, method = "KCI")

#Continuous_Integration ⊥ Tests_Volume | Commit_Size                                                3.463896e-14
CondIndTest(df$ci,df$test_vol,df$commit_size, method = "KCI")

#Merge_Conflicts ⊥ Tests_Volume | Commit_Size                                                       0.5421594
CondIndTest(df$merge_conf,df$test_vol,df$commit_size, method = "KCI")





#####################################################
##  Literature-driven Model - Failed Hypothesis
## dcov R < 1    - independent
## KCI  p > 0.05 - independent
#####################################################

#H1 - Age ⊥ Commit_frequency
dcov.test(df$age,df$commit_size,R=200)    #49.5566 
# Age and Commit_frequency  should be independent because the collider on CI.

      # ADDING AN EDGE AGE -> Commit_frequency
      # We tested if the collider is the problem, and it's not. 
      # The edges orientation could be wrong, in this case, when conditioning on CI, they should be independent
      # Even conditioning by CI, they remain dependents.
      CondIndTest(df$age,df$commit_size,df$ci, method = "KCI")   # 1.41219e-07 Dependent    
      # Therefore, we add a direct edge from Age to Commit_frequency
    
#H2 - Age ⊥ Tests_Volume
      dcov.test(df$age,df$test_vol,R=200) #4.308763
      
      #ADDING EDGE AGE -> TEST VOLUME
      #Even conditioning by CI    (22.395195e-12)
      #Even conditioning by Commit_frequency (0)
      #and CI+Commit_frequency, the variables AGE and TEST VOLUME remains dependent  (1.068682e-08)
      #Without conditioning, they are dependent (4.308763)
      CondIndTest(df$age,df$test_vol,df$ci, method = "KCI")   #2.395195e-12  DEPENDENT
      CondIndTest(df$age,df$test_vol,df$commit_size, method = "KCI")   #0  DEPENDENT
      
      dfa <- data.frame(ci,commit_s)
      dfa[is.na(dfa)]=0
      CondIndTest(df$age,df$test_vol,dfa, method = "KCI")   #1.37973e-11  DEPENDENT
      

      
#H3 - Age ⊥ Communication | Commit_frequency, Continuous_Integration        ******************************
      dfa <- data.frame(ci,commit_s)
      dfa[is.na(dfa)]=0
      CondIndTest(df$age,df$communication,dfa, method = "KCI")  #0.0001035991
      CondIndTest(df$age,df$communication,df$ci, method = "KCI")  #1.162458e-06
      
      # ADDING AGE -> COMMUNICATION
      # Even conditioning by CI and Commit_frequency size, the variables remains dependent
      # without conditioning, they are dependent  ()
      dcov.test(df$age,df$communication,R=200)  #11.86268

      
#H5	- Bug_Report ⊥ Commit_frequency | Age, Communication, Continuous_Integration, Tests_Volume 
      dfa <- data.frame(ci,communication,test_v,age)
      dfa[is.na(dfa)]=0
      CondIndTest(df$commit_size,df$bugs,dfa, method = "KCI")         #0
      
      # ADDING  Commit_frequency -> Bug_Report
      # Even conditioning by ci,communication,test_v,age, the variables remains dependent
      # Even conditioning by ci,communication,test_v, the variables remains dependent
      # without conditioning, they are dependent  ()
      dfa <- data.frame(ci,communication,test_v)
      dfa[is.na(dfa)]=0
      CondIndTest(df$commit_size,df$bugs,dfa, method = "KCI")       #0
      
      dcov.test(df$bugs,df$commit_size,R=200)                       #6.009902
      
      
      
#H9 - Continuous_Integration ⊥ Tests_Volume | Commit_frequency
    CondIndTest(df$test_vol,df$ci,df$commit_size, method = "KCI")   #6.739054e-14
    
    # INVERTING THE EDGE CI -> COMMIT SIZE 
    # Testing the independence under the conditioning, they are dependent
    # Testing without conditioning, they are independent
    # Thus, we suppose they form a collider on Commit Size, then when conditioning on it, the flow open...
    dcov.test(df$test_vol,df$ci,R=200)  #0.1128856  independent

    dcov.test(df$communication,df$test_vol,R=200)  #0.1128856  independent

  
  
#####################################################
## theory_data_v11_1
## http://dagitty.net/mT_4dhW
## dcov R < 1    - independent
## KCI  p > 0.05 - independent
#####################################################

#H1	Age ⊥ Merge_Conflicts | Commit_Frequency, Continuous_Integration                        #0.2696356
    dfa <- data.frame(ci,commit_s)
    dfa[is.na(dfa)]=0
    CondIndTest(df$age,df$merge_conf,dfa, method = "KCI")

#H2	Bug_Report ⊥ Merge_Conflicts | Commit_Frequency, Continuous_Integration                 #0.3213383
    dfa <- data.frame(commit_s,ci)
    dfa[is.na(dfa)]=0
    CondIndTest(df$bugs,df$merge_conf,dfa, method = "KCI")
    
#H3	Communication ⊥ Merge_Conflicts | Commit_Frequency, Continuous_Integration              #0.4865323
    dfa <- data.frame(commit_s,ci)
    dfa[is.na(dfa)]=0
    CondIndTest(df$communication,df$merge_conf,dfa, method = "KCI")
    
#H4	Communication ⊥ Tests_Volume | Age                                                      # 0
    CondIndTest(df$communication,df$test_vol,df$age, method = "KCI")
    
#H5	Continuous_Integration ⊥ Tests_Volume | Age                                             #0.7855025
    CondIndTest(df$ci,df$test_vol,df$age, method = "KCI")
    
    
    dhsic(df$communication,df$test_vol,kernel=c("gaussian","discrete"))$dHSIC
    
#H6	Merge_Conflicts ⊥ Tests_Volume | Commit_Frequency, Continuous_Integration               #0.384515
    dfa <- data.frame(commit_s,ci)
    dfa[is.na(dfa)]=0
    CondIndTest(df$merge_conf,df$test_vol,dfa, method = "KCI")
    
    
    
#####################################################
## theory_data_v11_1 - Failed Hypothesis
## http://dagitty.net/mT_4dhW
## dcov R < 1    - independent
## KCI  p > 0.05 - independent
#####################################################
    
  
#H4	Communication ⊥ Tests_Volume | Age
    
    #Add an edge from Tests_Volume to Communication
    
    
    
    
    
    
    
    
    
###################################################################
  
  t.test(df$merge_conf[df$ci==0],df$merge_conf[df$ci==1])
  t.test(df$commit_size[df$ci==0],df$commit_size[df$ci==1])
  t.test(df$age[df$ci==0],df$age[df$ci==1])
  t.test(df$bugs[df$ci==0],df$bugs[df$ci==1])
 
  res <- cor.test(df$age, df$bugs, method = c("pearson"))
  res
  res <- cor.test(df$communication, df$bugs, method = c("pearson"))
  res
     