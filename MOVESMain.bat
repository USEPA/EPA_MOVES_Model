@echo off
call setenv.bat

if exist sharedwork\*.* erase /Q /S /F sharedwork\*.*
if exist WorkerFolder\*.* erase /Q /S /F WorkerFolder\*.*

set HR=%time:~0,2%
set HR=%Hr: =0% 
set HR=%HR: =%
set logfile=moveslog_archive_%date:~10,4%-%date:~4,2%-%date:~7,2%_%HR%%time:~3,2%.txt

if exist moveslog.txt REN "moveslog.txt" "%logfile%"
if exist %logfile% (
	if not exist moveslog.zip ( 
		jar cMf moveslog.zip "%logfile%" && del "%logfile%"
	) else ( 
		jar uMf moveslog.zip "%logfile%" && del "%logfile%"
	)
)

ant rungui




