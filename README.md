#My Emacs Configuration
This is my constantly evolving Emacs configuration.

#Requirements
Emacs 24 or greater.

#Installation
To install the configuration on your system, clone this repository and then link the configuration direcotory (emacs.d) into your home directory. Detailed directions are listed below for Unix and Windows systems.

##For Unix style OS: (Linux/Mac OS X/etc.)
Make sure to backup your emacs configuration before you install this new one.

###Clone the repository
    $ cd ~
    $ git clone https://github.com/mrxcitement/dot-emacs.git ~/src/dot-emacs

###Install the config files
    $ cd ~/src/dot-emacs
    $ sh ./install.sh

##For windows based OS: (Windows XP or greater with MKLINK available)

###Clone the repository from github.
    > cd /d %userprofile%
    > git clone https://github.com/mrxcitement/dot-emacs.git %userprofile%/src/dot-emacs

###Link the emacs configuration directory.
(Note: To use MKLINK you need to be in a command prompt "Run As" an Administrator.)

    > cd /d %userprofile%/git/dot-emacs
    > .\install.cmd
