#!/usr/bin/env wolframscript
(* ::Package:: *)

Req=1/(VDD/2) Integrate[V/(IDSAT(1+lambda V)),{V,VDD/2,VDD},GenerateConditions->False]


Series[Req,{VDD,0,2}]


tp=Function[{beta},(Log[2]/2)((1+beta)(Cdn1+Cgn2)+Cw)Reqn(1+r/beta)]
FullSimplify[D[tp[beta],beta]]
Solve[D[tp[beta],beta]==0,beta]


D[F^(1/n),{n}]


EDP=Function[{VDD},alpha (VDD^3CL^2)/(2(VDD-VT-VDSAT/2))]
EDPx=Function[{VDD},D[EDP[a],a]/.{a->VDD}]
Solve[EDPx[VDD]==0,VDD]


(*-*)
