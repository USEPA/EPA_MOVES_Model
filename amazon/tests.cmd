echo off
rem s3 parameters:
rem --------------------------
rem login accesskey <keyvalue> secretkey <keyvalue>
rem logout
rem
rem put bucket <bucketname> nameinbucket <nameinbucket> file <filenameandpath>
rem     Note: put creates the bucket if it does not exist and does not transfer
rem           if <nameinbucket> already exists.
rem get bucket <bucketname> nameinbucket <nameinbucket> file <filenameandpath>
rem     Note: get does not transfer data if <filenameandpath> already exists.
rem delete bucket <bucketname> nameinbucket <nameinbucket>
rem     Note: delete reports warnings if the bucket or the named object do not exist.
rem list bucket <bucketname>
rem
rem job parameters:
rem --------------------------
rem createqueue queue <queuename> timeoutminutes <defaulttimeoutminutes>
rem listqueues
rem addstatus queue <queuename> status <statusmessage>
rem getstatus queue <queuename> command <commandtorun>
rem     Execute <commandtorun> with MOVES_STATUS environment variable.
rem getjob queue <queuename> command <commandtorun>
rem     Execute <commandtorun> with environment variables:
rem     MOVES_JOBID
rem     MOVES_DATABASEBUCKET
rem     MOVES_DATABASENAME
rem     MOVES_CODEBUCKET
rem     MOVES_CODENAME
rem     MOVES_JOBBUCKET
rem     MOVES_JOBNAME
rem 	MOVES_STATUSQUEUE
rem completejob queue <queuename> jobid <jobid>
rem addjob jobqueue <jobqueuename> statusqueue <statusqueuename>
rem     databasebucket <dbbucket> database <dbname>
rem     codebucket <codebucket> code <codename>
rem     jobbucket <jobbucket> job <jobjarname>

call ant -Dcmd="login accesskey ACCESSKEY secretkey SECRETKEY" s3

call ant -Dcmd="list bucket moves-experiment-1" s3
call ant -Dcmd="createqueue queue epa-moves-jobs timeoutminutes 1440" job
call ant -Dcmd="createqueue queue epa-moves-stats timeoutminutes 10" job

call ant -Dcmd="listqueues" job

rem call ant -Dcmd="addstatus queue epa-moves-stats status \"This is a full message One\"" job
rem call ant -Dcmd="addstatus queue epa-moves-stats status \"This is a full message Two\"" job
rem call ant -Dcmd="addstatus queue epa-moves-stats status \"This is a full message Three\"" job
rem call ant -Dcmd="addstatus queue epa-moves-stats status \"This is a full message Four\"" job

rem call ant -Dcmd="getstatus queue epa-moves-stats command statuscallback.cmd" job

rem call ant -Dcmd="logout" s3
