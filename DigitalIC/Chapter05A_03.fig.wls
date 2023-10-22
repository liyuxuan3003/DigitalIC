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
	"Preamble"->{"\\usepackage{siunitx}","\\usepackage{physics}","\\usepackage{upgreek}","\\usepackage{xcolor}"}])&;


xLabelFont={FontColor->Black,FontFamily->"Latin Modern",FontSize->5}


xFigConfig={
	Frame->True,GridLines->Automatic,ImageSize->Automatic->300,
	FrameStyle->BlackFrame,PlotStyle->ColorData[10],BaseStyle->xLabelFont};
xFigTicks=(FrameTicks->{
	Table[{x,xLaTeX[ToString[PaddedForm[x,{#2[[1]],#2[[2]]}]],0.5]},{x,#1[[1]],#1[[2]],#1[[3]]}],
	Table[{x,xLaTeX[ToString[PaddedForm[x,{#4[[1]],#4[[2]]}]],0.5]},{x,#3[[1]],#3[[2]],#3[[3]]}]})&;
xFigLabel=(FrameLabel->{xLaTeX[#1,0.8],xLaTeX[#2,0.8]})&;


xTicks=(Table[{x,xLaTeX[ToString[PaddedForm[x,{#2[[1]],#2[[2]]}]],0.5]},{x,#1[[1]],#1[[2]],#1[[3]]}])&;
xTicksLog=(Table[{10^x,xLaTeX["10^{"<>ToString[x]<>"}",0.5]},{x,#1[[1]],#1[[2]],#1[[3]]}])&;
xTicksLog1=(Table[{10^x,xLaTeX["1\\times 10^{"<>ToString[x]<>"}",0.5]},{x,#1[[1]],#1[[2]],#1[[3]]}])&;
xTicksLog2=(Table[{2 10^x,xLaTeX["2\\times 10^{"<>ToString[x]<>"}",0.5]},{x,#1[[1]],#1[[2]],#1[[3]]}])&;
xTicksLog5=(Table[{5 10^x,xLaTeX["5\\times 10^{"<>ToString[x]<>"}",0.5]},{x,#1[[1]],#1[[2]],#1[[3]]}])&;


xGrids=(Table[x,{x,#1[[1]],#1[[2]],#1[[3]]}])&;
xGridsLog=(Flatten[Table[Table[{If[#2[[1]]<=(n 10^x)<=#2[[2]],(n 10^x),{}]},{n,1,9,1}],{x,#1[[1]],#1[[2]],#1[[3]]}]])&;


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
	Epilog->{Red,Point[{{Log[3.5]//.val,1.25}}],Text[xLaTeX["\\textcolor{red}{\\beta=3.5}",0.7],{Log[3.5],1.1}]},
	FrameTicks->{xTicksLog[{-2,2,1}],xTicks[{0.0,2.5,0.25},{3,2}]},
	GridLines->{xGridsLog[{-2,2,1},{0.01,100}],xGrids[{0.0,2.5,0.25}]},
	Evaluate@xFigLabel["\\beta=(W/L)_p/(W/L)_n","V_M~(\\si{V})"],
	Evaluate@xFigConfig]


fig={fig1}


figWithName=Transpose[{fig,nbPDFNameList[[1;;Length[fig]]]}];
ReleaseHold[(Hold[Export[#[[2]],#[[1]],ImageResolution->2000]]&)/@figWithName]
