ass-mode.el
==============================================

Emacs major mode for editing (Advanced) SubStation Alpha (SSA/ASS) subtitles

##Installation:


```
(add-to-list 'load-path "~/.emacs.d/ass-mode/")
(require 'ass-mode)
```
Or something like this.

##Usage:
###(aka 'what we can do now')
* C-c C-o Run mplayer for event under point
* C-c C-s Shifts timestamp of event under point by amount of seconds
* C-c C-f Calculate new timestamp for event under point considering change in framerate
