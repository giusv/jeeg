﻿#NoEnv  ; Recommended for performance and compatibility with future AutoHotkey releases.
; #Warn  ; Enable warnings to assist with detecting common errors.
SendMode Input  ; Recommended for new scripts due to its superior speed and reliability.
SetWorkingDir %A_ScriptDir%  ; Ensures a consistent starting directory.

#Hotstring EndChars 
SetTitleMatchMode, 2

^w::
    WinActivate, GHCi
Return

^q::
    Send :load HList.MainGhcGeneric1{Enter}
Return

^r::
    WinActivate, Foxit
Return

^a::
    Send main{Enter}
Return

^b::
    Send :load jeeg{Enter}
Return

^o:: 
    Send main{Enter}
Return

^d::
    Send :cd D:\Dati\Profili\M026980\Documents\programmi\jeeg{Enter}
    Sleep, 200
    ; Send :set -fglasgow-exts -fallow-overlapping-instances -fallow-undecidable-instances{Enter}
    ; Sleep, 200
    Send :set -iHList{Enter}
Return 

^n:: 
    WinActivate, Notepad
Return

^j:: 
    WinActivate, JBoss
Return
