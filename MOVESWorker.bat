@echo off
if exist WorkerFolder\*.* erase /Q /S /F WorkerFolder\*.*
call setenv.bat
ant runworker