rem set SCHEME=..\geneva\bin\pcs.exe
set SCHEME=c:\bin\mscm_l.exe	
set LOADUP=loadup.scm

copy sxml09.scm sxml.scm
del  .\blog\index.htm 
del .\blog\hello\index.htm
del .\blog\post2\index.htm 
del .\blog\post3\index.htm
del .\blog\rss.xml
rem date < crlf.txt >> date.txt
time < crlf.txt >> TIME.TXT
%SCHEME%  <  %LOADUP%
time < crlf.txt >> TIME.TXT


del  .\blog\index.htm 
del .\blog\hello\index.htm
del .\blog\post2\index.htm 
del .\blog\post3\index.htm
del .\blog\rss.xml
rem date < crlf.txt >> date.txt
copy sxml08.scm sxml.scm
time < crlf.txt >> TIME.TXT
%SCHEME%  <  %LOADUP%
time < crlf.txt >> TIME.TXT

