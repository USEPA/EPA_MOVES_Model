@echo off
@echo Setting minimal environment to run MOVES workers via ant.
set ANT_HOME=ant
set JAVA_HOME=C:\Program Files\Java\jdk1.8.0_181
set JRE_HOME=C:\Program Files\Java\jre1.8.0_181
set PATH=%JRE_HOME%\bin;%JAVA_HOME%\bin;%ANT_HOME%\bin;%PATH%
set JAVA_TOOL_OPTIONS=-Dfile.encoding=UTF-8
