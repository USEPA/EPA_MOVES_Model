set >statusenv.txt
del statustemp.txt
del statuslog_before.txt
echo %MOVES_STATUS% >statustemp.txt
ren statuslog.txt statuslog_before.txt
copy statuslog_before.txt+statustemp.txt statuslog.txt
