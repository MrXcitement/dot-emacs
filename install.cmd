@echo off
rem install.cmd -- Install the .emacs.d directory in the users home directory
rem Use powershell to run a process elevated and then use mklink to create
rem a symlink to the emacs.d directory in the directory that this file is in.

rem Mike Barker <mike@thebarkers.com>
rem October 10, 2014

rem Copyright (c) 2014 by Mike Barker

rem Change log:
rem 2014.10.10
rem * First release.

rem powershell "start-process cmd.exe -argumentlist '/c %command%' -verb 'runass'"
mklink /d %userprofile%\.emacs.d %~dp0emacs.d
