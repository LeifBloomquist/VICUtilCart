@del cart*.prg
@del *.bak
@tools\dasm vic-diskcart.asm -ocart7-a000.prg
@pause
@C:\Leif\C64\WinVICE-1.15\xvic.exe -warp cart7-a000.prg