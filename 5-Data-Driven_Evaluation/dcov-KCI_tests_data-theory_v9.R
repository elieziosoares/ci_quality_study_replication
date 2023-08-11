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


###############################################################
################### QUERIES TO OBTAIN DATA ####################
###############################################################

ci <- dbGetQuery(con, "SELECT CASE WHEN CI=TRUE THEN 1 WHEN CI=FALSE THEN 0 END CI FROM METRICS_RELEASES where repo_name IN (select repo_name from projects where rq1_included is true) ORDER BY REPO_NAME,created_at asc")
bugs <- dbGetQuery(con, "SELECT bugs FROM METRICS_RELEASES where repo_name IN (select repo_name from projects where rq1_included is true) ORDER BY REPO_NAME,created_at asc")
commit_s <- dbGetQuery(con, "SELECT commit_size_mean commit_size FROM METRICS_RELEASES where repo_name IN (select repo_name from projects where rq1_included is true) ORDER BY REPO_NAME,created_at asc")
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
##  Literature-driven Model (EXCEPT FOR INVERSION OF CI-> MERGE CONF)
##  http://dagitty.net/mruVHSK
#####################################################

#Age ⊥ Commit_Size                                                                                  60.95021 DEPENDENT
dcov.test(df$commit_s,df$age,R=200)

#Age ⊥ Tests_Volume                                                                                 4.308763 DEPENDENT
dcov.test(df$test_vol,df$age,R=200)

#Age ⊥ Communication | Commit_Size, Continuous_Integration                                          0.000004072992 DEPENDENT
dfa <- data.frame(ci,commit_s)
dfa[is.na(dfa)]=0
CondIndTest(df$age,df$communication,dfa, method = "KCI")

#Age ⊥ Merge_Conflicts | Commit_Size, Continuous_Integration                                        0.270616 
dfa <- data.frame(ci,commit_s)
dfa[is.na(dfa)]=0
CondIndTest(df$age,df$merge_conf,dfa, method = "KCI")

#Bug_Report ⊥ Commit_Size | Age, Communication, Continuous_Integration, Tests_Volume               0.3405027  
dfa <- data.frame(ci,communication,test_v,age)
dfa[is.na(dfa)]=0
CondIndTest(df$commit_size,df$bugs,dfa, method = "KCI")

#Bug_Report ⊥ Merge_Conflicts | Commit_Size, Continuous_Integration                                 0.3977972
dfa <- data.frame(ci,commit_s)
dfa[is.na(dfa)]=0
CondIndTest(df$bugs,df$merge_conf,dfa, method = "KCI")

#Bug_Report ⊥ Merge_Conflicts | Age, Communication, Continuous_Integration, Tests_Volume            0.3633449
dfa <- data.frame(age,communication,ci,test_v)
dfa[is.na(dfa)]=0
CondIndTest(df$bugs,df$merge_conf,dfa, method = "KCI")

#Communication ⊥ Merge_Conflicts | Commit_Size, Continuous_Integration                              0.2296125
dfa <- data.frame(commit_s,ci)
dfa[is.na(dfa)]=0
CondIndTest(df$communication,df$merge_conf,dfa, method = "KCI")

#Continuous_Integration ⊥ Tests_Volume | Commit_Size                                                0
CondIndTest(df$ci,df$test_vol,df$commit_size, method = "KCI")

#Merge_Conflicts ⊥ Tests_Volume | Commit_Size                                                       0.3988595
CondIndTest(df$merge_conf,df$test_vol,df$commit_size, method = "KCI")





#####################################################
##  Literature-driven Model - Failed Hypothesis
## dcov R < 1    - independent
## KCI  p > 0.05 - independent
#####################################################

#H1 - Age ⊥ Commit_Size
dcov.test(df$age,df$commit_size,R=200)
      
      # REORIENT AN EDGE COMMIT SIZE -> CI
      # Age and commit size  should be independent because the collider on CI.
      # The edges orientation could be wrong, in this case, when conditioning on CI, they should be independent
      CondIndTest(df$age,df$commit_size,df$ci, method = "KCI")   # 0.4096049 Independent
      # We confirm this, therefore we reorient the edge Commit_size -> CI to CI -> commit_size
    
#H2 - Age ⊥ Tests_Volume
      dcov.test(df$age,df$test_vol,R=200) #4.308763
      
      #ADDING EDGE AGE -> TEST VOLUME
      #Even conditioning by CI    (2.395195e-12)
      #and CI+commit size, the variables AGE and TEST VOLUME remains dependent  (1.068682e-08)
      #Without conditioning, they are dependent (4.308763)
      CondIndTest(df$age,df$test_vol,df$ci, method = "KCI")   #2.395195e-12  DEPENDENT
      
      dfa <- data.frame(ci,commit_s)
      dfa[is.na(dfa)]=0
      CondIndTest(df$age,df$test_vol,dfa, method = "KCI")   #1.068682e-08  DEPENDENT
      

      
#H3 - Age ⊥ Communication | Commit_Size, Continuous_Integration
      dfa <- data.frame(ci,commit_s)
      dfa[is.na(dfa)]=0
      CondIndTest(df$age,df$communication,dfa, method = "KCI")
      
      # ADDING AGE -> COMMUNICATION
      # Even conditioning by CI and Commit size, the variables remains dependent
      # without conditioning, they are dependent  ()
      dcov.test(df$age,df$communication,R=200)  #11.86268
      
      
#H9 - Continuous_Integration ⊥ Tests_Volume | Commit_Size
    CondIndTest(df$test_vol,df$ci,df$commit_size, method = "KCI")
    
    # INVERTING THE EDGE CI -> COMMIT SIZE (CONFIRMING WHAT OCCURRED IN H2)
    # Testing the independence under the conditioning, they are dependent
    # Testing without conditioning, they are independent
    # Thus, we suppose they form a collider on Commit Size, then when conditioning on it, the flow open...
    dcov.test(df$test_vol,df$ci,R=200)  #0.1128856  independent



  
  
#####################################################
## theory_data_v7_0
## http://dagitty.net/mTmzESK
## dcov R < 1    - independent
## KCI  p > 0.05 - independent
#####################################################

#Age ⊥ Commit_Size | Tests_Volume                                                               0.7184668
CondIndTest(df$age,df$commit_size,df$test_vol, method = "KCI")
    
#Age ⊥ Merge_Conflicts | Commit_Size, Continuous_Integration                                    0.270616
dfa <- data.frame(ci,commit_s)
dfa[is.na(dfa)]=0
CondIndTest(df$age,df$merge_conf,dfa, method = "KCI")

#Bug_Report ⊥ Commit_Size | Age, Communication, Continuous_Integration, Tests_Volume            0.4529254
dfa <- data.frame(age,communication,ci,test_v)
dfa[is.na(dfa)]=0
CondIndTest(df$bugs,df$commit_size,dfa, method = "KCI")

#Bug_Report ⊥ Merge_Conflicts | Commit_Size, Continuous_Integration                             0.3977978
dfa <- data.frame(commit_s,ci)
dfa[is.na(dfa)]=0
CondIndTest(df$bugs,df$merge_conf,dfa, method = "KCI")

#Bug_Report ⊥ Merge_Conflicts | Age, Communication, Continuous_Integration, Tests_Volume        0.3662549
dfa <- data.frame(age,communication,ci,test_v)
dfa[is.na(dfa)]=0
CondIndTest(df$bugs,df$merge_conf,dfa, method = "KCI")

#Communication ⊥ Merge_Conflicts | Commit_Size, Continuous_Integration                          0.2294564
dfa <- data.frame(commit_s,ci)
dfa[is.na(dfa)]=0
CondIndTest(df$communication,df$merge_conf,dfa, method = "KCI")

#Continuous_Integration ⊥ Tests_Volume | Age, Commit_Size                                       0.7235629
dfa <- data.frame(age,commit_s)
dfa[is.na(dfa)]=0
CondIndTest(df$ci,df$test_vol,dfa, method = "KCI")

#Merge_Conflicts ⊥ Tests_Volume | Age, Commit_Size                                              0.3371355
dfa <- data.frame(age,commit_s)
dfa[is.na(dfa)]=0
CondIndTest(df$merge_conf,df$test_vol,dfa, method = "KCI")

#Merge_Conflicts ⊥ Tests_Volume | Commit_Size, Continuous_Integration                           0.3099187
dfa <- data.frame(commit_s,ci)
dfa[is.na(dfa)]=0
CondIndTest(df$merge_conf,df$test_vol,dfa, method = "KCI")

    
    
    
    
    
  
###################################################################
  
  t.test(df$merge_conf[df$ci==0],df$merge_conf[df$ci==1])
  t.test(df$commit_size[df$ci==0],df$commit_size[df$ci==1])
  t.test(df$age[df$ci==0],df$age[df$ci==1])
  t.test(df$bugs[df$ci==0],df$bugs[df$ci==1])
 
  res <- cor.test(df$age, df$bugs, method = c("pearson"))
  res
  res <- cor.test(df$communication, df$bugs, method = c("pearson"))
  res
     