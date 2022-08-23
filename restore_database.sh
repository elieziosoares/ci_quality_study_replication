#echo "Drop database Causal_CI_Quality_v3"
#dropdb -U $POSTGRES_USER Causal_CI_Quality_v3

echo "Create database Causal_CI_Quality_v3"
psql -U $POSTGRES_USER -c "CREATE DATABASE \"Causal_CI_Quality_v3\""

echo "Restoring database Causal_CI_Quality_v3"
pg_restore -v -U $POSTGRES_USER -d Causal_CI_Quality_v3 /backup.sql
#pg_restore -U $POSTGRES_USER -v -f /backup.sql

#echo "Setting privileges to $POSTGRES_USER"
#psql -U postgres -c "GRANT ALL PRIVILEGES ON DATABASE Causal_CI_Quality_v3 TO $POSTGRES_USER"
echo "Database restored successfully"

#echo "Removing backup file"
#rm -f /backup.sql