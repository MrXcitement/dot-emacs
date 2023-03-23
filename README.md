# Personal Emacs Configuration
This is my constantly evolving Emacs configuration.

# Summary

## File editing
- Use [Evil](https://www.emacswiki.org/emacs/Evil) mode to provide an editing environment that mimimcks the [Vim](https://www.vim.org/) editor.

## Input Completion
- Use [ido]() and [icomplete]() modes to handle completing items in the minibuffer

## Text Completion
- *TODO!*

## Programming Language Support
- *TODO!*

# Installation
To install the configuration on your system, clone this repository and then link the configuration directory `./home/.emacs.d` into your home directory.
Directions are listed below for Unix and Windows systems.

## Requirements
Emacs 24 or greater.
*Recommend* Emacs 27+

## For Linux, macOS, wsl
Backup any existing Emacs configuration before you install this new one.

### Clone the repository

```sh
$ cd ~
$ git clone https://github.com/mrxcitement/dot-emacs.git ~/src/dot-emacs
```

### Install the config files

```sh
$ cd ~/src/dot-emacs
$ ./install
```

## For Windows OS: (Windows 10 or greater with powershell installed)

### Clone the repository from github.

```powershell
> cd /d %userprofile%
> git clone https://github.com/mrxcitement/dot-emacs.git %userprofile%/src/dot-emacs
```

### Install the config files
(Note: To LINK the files you need to "Run As" an Administrator.)

```powershell
> cd /d %userprofile%/git/dot-emacs
> .\install.ps1
```
