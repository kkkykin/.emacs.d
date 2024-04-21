@echo off

set GIT_PAGER=NUL & set EDITOR=emacsclient
set emacs_bin_dir="%emacs_dir%\usr\bin"
md %emacs_bin_dir% & echo @cd /d %* > %emacs_bin_dir%\cdd.bat
set emacs_bin_dir=

rem Local Variables:
rem mode: bat
rem End:
echo on
