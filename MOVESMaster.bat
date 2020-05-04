@echo off
if exist sharedwork\*.* erase /Q /S /F sharedwork\*.*
if exist WorkerFolder\*.* erase /Q /S /F WorkerFolder\*.*
if exist moveslog_old.txt erase moveslog_old.txt
if exist moveslog.txt rename moveslog.txt moveslog_old.txt
call setenv.bat
ant rungui