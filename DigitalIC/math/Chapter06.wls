#!/usr/bin/env wolframscript
(* ::Package:: *)

tp=Log[2](R C n (m+1)/2)+(n/m-1)tbuf
tpdm=D[tp,m]
Solve[tpdm==0,m]



