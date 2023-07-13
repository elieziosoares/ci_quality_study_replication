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
merge_c <- dbGetQuery(con, "SELECT merge_conflicts Merge_Conf FROM METRICS_RELEASES where repo_name IN (select repo_name from projects where rq1_included is true) ORDER BY REPO_NAME,created_at asc")
#merge_c <- dbGetQuery(con, "SELECT merge_conflicts_git Merge_Conf FROM METRICS_RELEASES where repo_name IN (select repo_name from projects where rq1_included is true) ORDER BY REPO_NAME,created_at asc")
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
##  http://dagitty.net/mruVHSK
#####################################################

##Age ⊥ Communication | Commit Size, Continuous Integration                                  #0.001457542 #############              
dfa <- data.frame(ci,commit_s)
dfa[is.na(dfa)]=0
CondIndTest(df$age,df$communication,dfa, method = "KCI")


##Tests Volume ⊥ Age                                                                       #0.4137644
dcov.test(df$test_vol,df$age,R=200)

##Age ⊥ Communication | Commit Size, Continuous Integration                                  #0.001457542 #############              
dfa <- data.frame(ci,commit_s)
dfa[is.na(dfa)]=0
CondIndTest(df$age,df$communication,dfa, method = "KCI")

##Communication ⊥ Merge_Conflicts | Commit_Size, Continuous_Integration                      #0.001661873  #############              
dfa <- data.frame(ci,commit_s)
dfa[is.na(dfa)]=0
CondIndTest(df$communication,df$merge_conf,dfa, method = "KCI")

##Merge Conflicts ⊥ Tests Volume | Commit Size                                                 0.3256462
CondIndTest(df$merge_conf,df$test_vol,df$commit_size, method = "KCI")

##Merge_Conflicts ⊥ Bug_Report | Age, Communication, Continuous_Integration, Tests_Volume      0.832849
dfa <- data.frame(ci,communication,test_v,age)
dfa[is.na(dfa)]=0
CondIndTest(df$merge_conf,df$bugs,dfa, method = "KCI")

##Merge_Conflicts ⊥ Bug_Report | Age, Commit_Size, Continuous_Integration                      0.6429821
dfa <- data.frame(ci,commit_s,age)
dfa[is.na(dfa)]=0
CondIndTest(df$merge_conf,df$bugs,dfa, method = "KCI")

##Tests_Volume ⊥ Continuous_Integration | Commit_Size                                          0.02211318     ###########
CondIndTest(df$test_vol,df$ci,df$commit_size, method = "KCI")

##Commit_Size ⊥ Bug_Report | Age, Communication, Continuous_Integration, Tests_Volume        #0.2281599
dfa <- data.frame(ci,communication,test_v,age)
dfa[is.na(dfa)]=0
CondIndTest(df$commit_size,df$bugs,dfa, method = "KCI")
#kernelCItest(x=2,y=3, S=c(7,5,1,6), suffStat) #  
#hsic.clust(df$commit_size,df$bugs,dfa)












#####################################################
##  Literature-driven Model - Failed Hypothesis
#####################################################

#H1 - Age ⊥ Merge_Conflicts                                                                    #1.593328     #########
dcov.test(df$age,df$merge_conf,R=200)

  # INVERTING THE EDGE MERGE -> CI
  # The independence between Age and Merge expected because the collider on CI do not exist on the data.
  # Thus, we test for independence when conditioning on CI.
  CondIndTest(df$age,df$merge_conf,df$ci, method = "KCI")   #0.2794397 Independent
  # They become independent, then we confirm that they are not in a colllider.


#H2 - Age ⊥ Commit_Size                                                                        #19.20786    #######               
dcov.test(df$age,df$commit_size,R=200)
  
  # REORIENT AN EDGE COMMIT SIZE -> CI
  # Age and commit size  should be independent because the collider on CI.
  # The edges orientation could be wrong, in this case, when conditioning on CI, they should be independent
  CondIndTest(df$age,df$commit_size,df$ci, method = "KCI")   #0.1484211 Independent
  # We confirm this, therefore we reorient the edge Commit_size -> CI to CI -> commit_size

  
##H4 - Age ⊥ Communication | Commit Size, Continuous Integration                                  #0.001457542 #############              
dfa <- data.frame(ci,commit_s)
dfa[is.na(dfa)]=0
CondIndTest(df$age,df$communication,dfa, method = "KCI")

  # ADDING AGE -> COMMUNICATION
  # Even conditioning by CI and Commit size, the variables remains dependent
  # without conditioning, they are dependent
  dcov.test(df$age,df$communication,R=200)  #8.737601
  
  
##H5 - Communication ⊥ Merge_Conflicts | Commit_Size, Continuous_Integration                      #0.001661873  #############              
dfa <- data.frame(ci)
dfa[is.na(dfa)]=0
CondIndTest(df$communication,df$merge_conf,dfa, method = "KCI")
  
  # ADDING MERGE_CONFLICTS -> COMMUNICATION
  # Even conditioning by CI and Commit size, the variables remains dependent
  # without conditioning, they are dependent
  dcov.test(df$merge_conf,df$communication,R=200)  #0.1599613
  

##H9 - Tests_Volume ⊥ Continuous_Integration | Commit_Size                                          0.02211318     ###########
CondIndTest(df$test_vol,df$ci,df$commit_size, method = "KCI")
  
  # INVERTING THE EDGE CI -> COMMIT SIZE (CONFIRMING WHAT OCCURRED IN H2)
  # Testing the independence under the conditioning, they are dependent
  # Testing without conditioning, they are independent
  # Thus, we suppose they form a collider on Commit Size, then when conditioning on it, the flow open...
  dcov.test(df$test_vol,df$ci,R=200)  #0.02357966

  
  
#####################################################
## theory_data_v5_0
## http://dagitty.net/mllzCj-
#####################################################
#Age ⊥ Tests_Volume                                                                           0.4137644 indep
  dcov.test(df$age,df$test_vol,R=200)

#Continuous_Integration ⊥ Tests_Volume                                                        0.02357966 INDEP
  dcov.test(df$ci,df$test_vol,R=200)
  
#Age ⊥ Merge_Conflicts | Continuous_Integration                                               0.2794397 INDEP   
  CondIndTest(df$age,df$merge_conf,df$ci, method = "KCI")
  
#Age ⊥ Commit_Size | Continuous_Integration                                                   0.1484211 INDEP
  CondIndTest(df$age,df$commit_size,df$ci, method = "KCI")
  
#Bug_Report ⊥ Commit_Size | Age, Communication, Continuous_Integration, Tests_Volume          0.09372402 INDEP
  dfa <- data.frame(age,ci,communication,test_v)
  dfa[is.na(dfa)]=0
  CondIndTest(df$bugs,df$commit_size,dfa, method = "KCI")
  
#Bug_Report ⊥ Merge_Conflicts | Age, Communication, Continuous_Integration, Tests_Volume      0.7762824 INDEP
  dfa <- data.frame(age,ci,communication,test_v)
  dfa[is.na(dfa)]=0
  CondIndTest(df$bugs,df$merge_conf,dfa, method = "KCI")
  
  
#Merge_Conflicts ⊥ Tests_Volume | Commit_Size, Continuous_Integration                         0.3699588 INDEP
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
     