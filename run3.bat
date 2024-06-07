rem set SCHEME=..\geneva\bin\pcs.exe
set SCHEME=c:\bin\mscm_l.exe	
set LOADUP=loadup.scm

del  .\blog\index.htm 
del .\blog\hello\index.htm
del .\blog\post2\index.htm 
del .\blog\post3\index.htm
del .\blog\rss.xml
rem date < crlf.txt >> date.txt
copy sxml08.scm sxml.scm
time < crlf.txt >> LOG.TXT
%SCHEME%  <  %LOADUP% >> LOG.TXT
time < crlf.txt >> LOG.TXT

