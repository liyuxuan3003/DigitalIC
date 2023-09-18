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


xLabelFont={FontColor->Black,FontFamily->"Latin Modern",FontSize->5}


xFigConfig={
	Frame->True,GridLines->Automatic,ImageSize->Automatic->300,
	FrameStyle->BlackFrame,PlotStyle->ColorData[10],BaseStyle->xLabelFont};
xFigTicks=(FrameTicks->{
	Table[{x,xLaTeX[ToString[PaddedForm[x,{#2[[1]],#2[[2]]}]],0.5]},{x,#1[[1]],#1[[2]],#1[[3]]}],
	Table[{x,xLaTeX[ToString[PaddedForm[x,{#4[[1]],#4[[2]]}]],0.5]},{x,#3[[1]],#3[[2]],#3[[3]]}]})&;
xFigLabel=(FrameLabel->{xLaTeX[#1,0.8],xLaTeX[#2,0.8]})&;


xVM=((VTn+VDSATn/2)+r(VDD+VTp+VDSATp/2))/(1+r)
xVMa=(r VDD)/(1+r)
xr=(kp0 VDSATp)/(kn0 VDSATn) WLpn


val={VTn->+0.43,VTp->-0.40,VDSATn->+0.63,VDSATp->-1.00,kn0->+115 10^(-6),kp0->-30 10^(-6),VDD->2.5}
fig1=LogLinearPlot[Evaluate[{VM,VMa}/.{VM->xVM,VMa->xVMa}/.{r->xr}/.val],{WLpn,10^(-2),10^(+2)},
	PlotStyle->{{Black},{Gray,Thickness[0.002],Dashed}},
	PlotLegends->Placed[
	LineLegend[
		{Style["\:7cbe\:786e\:503c",FontSize->8,FontFamily->"Noto Sans Mono CJK SC"],
		Style["\:8fd1\:4f3c\:503c",FontSize->8,FontFamily->"Noto Sans Mono CJK SC"]},
		LegendFunction->"Panel",
		LegendLayout -> {"Row",2}],{Left,Top}],
	Evaluate@xFigLabel["(W/L)_p/(W/L)_n","V_M~(\\si{V})"],
	Evaluate@xFigConfig]


fig={fig1}


figWithName=Transpose[{fig,nbPDFNameList[[1;;Length[fig]]]}];
ReleaseHold[(Hold[Export[#[[2]],#[[1]],ImageResolution->2000]]&)/@figWithName]
