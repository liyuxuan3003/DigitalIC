#!/usr/bin/env wolframscript
(* ::Package:: *)

ClearAll["Global`*"]


SetDirectory[NotebookDirectory[]];
Needs["MaTeX`"];


nbFileName="Chapter05A_03.fig.wls";
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


xVM=((VTn+VDSATn/2)+r(VDD+VTp+VDSATp/2))/(1+r)
xr=(kp0 VDSATp)/(kn0 VDSATn) WLpn


val={VTn->+0.43,VTp->-0.40,VDSATn->+0.63,VDSATp->-1.00,kn0->+115 10^(-6),kp0->-30 10^(-6),VDD->2.5}
LogLinearPlot[VM/.{VM->xVM}/.{r->xr}/.val,{WLpn,10^(-2),10^(+2)},Evaluate@xFigConfig]



