#!/usr/bin/env wolframscript
(* ::Package:: *)

ClearAll["Global`*"]


SetDirectory[NotebookDirectory[]];
Needs["MaTeX`"];


nbFileName="Chapter05A_02.fig.wls";
nbPDFName="build/"<>StringReplace[nbFileName,"wls"->"pdf"];
nbPDFNameList=(StringReplace[nbPDFName,".fig"->#<>".fig"]&)/@Alphabet[];
Export[nbPDFName,""]


xLaTeX=(MaTeX[
	#1,Magnification->#2,
	"Preamble"->{"\\usepackage{siunitx}","\\usepackage{physics}","\\usepackage{upgreek}"}])&;


xLabelFont={FontColor->Black,FontFamily->"Latin Modern",FontSize->8}


xFigConfig={
	Frame->True,GridLines->Automatic,ImageSize->Automatic->300,
	FrameStyle->BlackFrame,PlotStyle->ColorData[10],BaseStyle->xLabelFont};
xFigTicks=(FrameTicks->{
	Table[{x,xLaTeX[ToString[PaddedForm[x,{#2[[1]],#2[[2]]}]],0.5]},{x,#1[[1]],#1[[2]],#1[[3]]}],
	Table[{x,xLaTeX[ToString[PaddedForm[x,{#4[[1]],#4[[2]]}]],0.5]},{x,#3[[1]],#3[[2]],#3[[3]]}]})&;
xFigLabel=(FrameLabel->{xLaTeX[#1,0.8],xLaTeX[#2,0.8]})&;


xIn=Function[{Vin,Vout},(+kn((Vin-VTn)Vmin-Vmin^2/2))];
xIp=Function[{Vin,Vout},(-kp((Vin-VDD-VTp)Vmax-Vmax^2/2))];


s1=Function[{Vin},Solve[Evaluate[xIn[Vin,Vout]==xIp[Vin,Vout]/.{Vmin->(Vin-VTn),Vmax->(Vout-VDD)}],Vout]];
s2=Function[{Vin},Solve[Evaluate[xIn[Vin,Vout]==xIp[Vin,Vout]/.{Vmin->(VDSATn),Vmax->(Vout-VDD)}],Vout]];
sM=Solve[Evaluate[xIn[VM,VM]==xIp[VM,VM]/.{Vmin->(VDSATn),Vmax->(VDSATp)}],VM];
s3=Function[{Vin},Solve[Evaluate[xIn[Vin,Vout]==xIp[Vin,Vout]/.{Vmin->(Vout),Vmax->(VDSATp)}],Vout]];
s4=Function[{Vin},Solve[Evaluate[xIn[Vin,Vout]==xIp[Vin,Vout]/.{Vmin->(Vout),Vmax->(Vin-VDD-VTp)}],Vout]];


xVout=Function[{Vin},Piecewise[{
{VDD,0<=Vin<VTn},
{Vout/.s1[Vin][[1]],VTn<=Vin<Min[VM/.sM[[1]],VTn+VDSATn]},
{Vout/.s2[Vin][[2]],VTn+VDSATn<=Vin<(VM/.sM[[1]])},
{Vout/.s3[Vin][[2]],(VM/.sM[[1]])<Vin<=VDD+VDSATp+VTp},
{Vout/.s4[Vin][[2]],VDD+VDSATp+VTp<Vin<=VDD+VTp},
{0,VDD+VTp<Vin<=VDD}
}]]


val={VTn->+0.4,VDSATn->+0.6,VTp->-0.4,VDSATp->-0.6,VDD->2.5,kn->1,kp->-1}

valListA={
{VTn->+0.4,VDSATn->+0.6,VTp->-0.4,VDSATp->-0.6,VDD->2.5,kn->1,kp->-1},
{VTn->+0.4,VDSATn->+0.6,VTp->-0.4,VDSATp->-0.6,VDD->2.5,kn->2,kp->-1},
{VTn->+0.4,VDSATn->+0.6,VTp->-0.4,VDSATp->-0.6,VDD->2.5,kn->1,kp->-2}
}

valListB={
{VTn->+0.4,VDSATn->+0.6,VTp->-0.4,VDSATp->-0.6,VDD->2.5,kn->1,kp->-1},
{VTn->+0.4,VDSATn->+0.6,VTp->-0.4,VDSATp->-0.6,VDD->2.0,kn->1,kp->-1},
{VTn->+0.4,VDSATn->+0.6,VTp->-0.4,VDSATp->-0.6,VDD->1.5,kn->1,kp->-1},
{VTn->+0.4,VDSATn->+0.6,VTp->-0.4,VDSATp->-0.6,VDD->1.0,kn->1,kp->-1}
}


Plot[Evaluate[(xVout[Vin])/.val],{Vin,0,2.5},Evaluate@xFigConfig,Exclusions->None]
Plot[Evaluate[(D[xVout[Vin],Vin])/.val],{Vin,0,2.5},Evaluate@xFigConfig,Exclusions->None]


Plot[Evaluate[(xVout[Vin])/.valListA],{Vin,0,2.5},Evaluate@xFigConfig,Exclusions->None]
Plot[Evaluate[(xVout[Vin])/.valListB],{Vin,0,2.5},Evaluate@xFigConfig,Exclusions->None]



