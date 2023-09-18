#!/usr/bin/env wolframscript
(* ::Package:: *)

Req=1/(VDD/2) Integrate[V/(IDSAT(1+lambda V)),{V,VDD/2,VDD},GenerateConditions->False]


Series[Req,{VDD,0,2}]
