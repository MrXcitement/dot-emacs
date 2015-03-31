@echo off

rem emacs.cmd
rem Launch the Emacs editor.

rem Mike Barker <mike@thebarkers.com>
rem October 18, 2012

rem Copyright (c) 2012 Mike Barker

rem Change log:
rem 2012.10.18
rem * First release.

set EMACS_PATH=c:\tools\emacs\bin
%EMACS_PATH%\runemacs %*
