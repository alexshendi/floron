set SCHEME=a:\geneva\bin\pcs.exe
rem set SCHEME=c:\bin\mscm_l.exe	
set LOADUP=loadup1.scm

del  .\blog\index.htm 
del .\blog\hello\index.htm
del .\blog\post2\index.htm 
del .\blog\post3\index.htm
del .\blog\rss.xml
rem date < crlf.txt >> date.txt
copy sxml08.scm sxml.scm
time < crlf.txt >> LOG.TXT
%SCHEME%  %LOADUP%
time < crlf.txt >> LOG.TXT

