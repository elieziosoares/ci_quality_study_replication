select I.issue_number, I.repo_name FROM ISSUE I 
                INNER JOIN ISSUELABEL IL  ON IL.issue_number = I.issue_number AND IL.repo_name = I.repo_name
                INNER JOIN LABEL L ON IL.idlabel = L.idlabel
                where (name ilike '%bug%' AND (name not like '%not a bug%' AND name not like '%notabug%' AND name not like '%not-bug%' AND name NOT ilike '%bug fix%' 
                            AND name NOT ilike '%bug-fix%' AND name NOT ilike '%bugfix%' AND name NOT ilike '%bugsnag%' AND name NOT ilike '%debug%'))
                    OR name ilike '%please verify%' OR name ilike '%waiting for fix%' OR name ilike '%awaiting fix%' OR name ilike '%reproduc%'
                    OR name ilike '%fixme%'
                    OR NAME like 'issue' OR NAME like 'type: issue'
    OR (name ilike '%known issue%' and I.repo_name like 'Azure/azure-event-hubs-spark')
    OR (name ilike '%defect%' AND I.repo_name not like 'top-think/framework')
    OR (name ilike '%confirmed%' AND (name not ilike '%unconfirmed%' AND I.repo_name not like 'Dogfalo/materialize' AND I.repo_name not like 'particle-iot/device-os'))
    OR (I.repo_name LIKE 'jquery/jquery-mobile' AND (name ilike '%Priority: Blocker%' or name ilike '%Priority: High%'))
    OR I.repo_name LIKE 'jruby/activerecord-jdbc-adapter'
    OR I.repo_name LIKE 'microsoft/malmo'
    OR (I.repo_name LIKE 'Atmosphere/atmosphere' AND name NOT in ('Documentation','Enhancement','not a bug','sample'))
    OR (I.repo_name LIKE 'intel-analytics/BigDL' AND name NOT in ('document','dataset','example','question','test'))
    OR (I.repo_name LIKE 'rubinius/rubinius' AND name NOT in ('Community','Documentation','C-API Compatibility','Build & Packaging','test','Feature Request'))
    OR (I.repo_name LIKE 'jshint/jshint' AND name IN ('P1','P2'))


   --88.621

   
-- Identified BUGS    
-- 90159
 select count(I.issue_number) from issue I where isbug is true

-- Projects with no classified bugs 53
-- Projects with classified bugs 485
 SELECT count(repo_name) From PROJECTS 
 WHERE PRS_SELECTED IS TRUE AND ISSUES_SELECTED IS TRUE AND repo_name  in (
 	select distinct repo_name from issue where isbug is true
 )
 
-- Projects with merge conflicts classified 551
SELECT count(repo_name) From PROJECTS 
    WHERE merge_conflict_mined is TRUE


-- Pullrequests with identifies merge conflicts 7423
SELECT COUNT(PR_NUMBER) FROM PULLREQUESTS WHERE mergeconflict is true