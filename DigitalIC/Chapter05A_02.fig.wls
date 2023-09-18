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


xBText=(Text[Style[#1,Black,FontSize->7,FontFamily->"Noto Sans Mono CJK SC"],#2])&;
xBTextPairs=({xBText[#1,{#3[[1]],#3[[2]]+0.1}],xBText[#2,{#3[[1]],#3[[2]]-0.1}]})&;


fig1=Plot[Evaluate[(xVout[Vin])/.val],{Vin,0,2.5},
	PlotStyle->{Black},
	Prolog->{
		Dashed,Gray,
		Line[{{0.4,-1},{0.4,3.5}}],
		Line[{{1.25,-1},{1.25,3.5}}],
		Line[{{2.1,-1},{2.1,3.5}}],
		Dashed,LightGray,
		Line[{{1.0,-1},{1.0,3.5}}],
		Line[{{1.5,-1},{1.5,3.5}}]
	},
	Epilog->{
		Red,
		Point[{0.4,2.5}],
		Point[{1.0,2.32}],
		Point[{1.25,1.91}],
		Point[{1.25,1.25}],
		Point[{1.25,0.61}],
		Point[{1.5,0.18}],
		Point[{2.1,0.00}],
		xBTextPairs["N\:622a\:6b62\:533a","P\:7ebf\:6027\:533a",{0.18,1.25}],
		xBTextPairs["N\:9971\:548c\:533a","P\:7ebf\:6027\:533a",{0.70,1.25}],
		xBTextPairs["N\:7ebf\:6027\:533a","P\:9971\:548c\:533a",{1.80,1.25}],
		xBTextPairs["N\:7ebf\:6027\:533a","P\:622a\:6b62\:533a",{2.32,1.25}],
		xBText["N\:5939\:65ad",{0.9,1.75}],
		xBText["N\:901f\:5ea6",{1.1,1.75}],
		xBText["P\:5939\:65ad",{1.6,0.75}],
		xBText["P\:901f\:5ea6",{1.4,0.75}],
		Gray,
		Arrow[{{1.25,0.25},{0.85,0.25}}],
		Arrow[{{1.25,2.25},{1.66,2.25}}],
		Text["PMOS\:6c9f\:9053\:5bfc\:901a",{0.75,0.38}],
		Text["NMOS\:6c9f\:9053\:5bfc\:901a",{1.75,2.12}],
		Text[xLaTeX["V_M",0.8],{1.35,1.25}],
		Arrowheads[{-0.02,0.02}],Thin,Black,
		Arrow[{{0.0,2.25},{0.4,2.25}}],
		Arrow[{{0.4,2.25},{1.0,2.25}}],
		Arrow[{{2.5,0.25},{2.1,0.25}}],
		Arrow[{{2.1,0.25},{1.5,0.25}}],
		Text[xLaTeX["V_{Tn}",0.8],{0.2,2.15}],
		Text[xLaTeX["V_{DSATn}",0.8],{0.7,2.15}],
		Text[xLaTeX["V_{DSATp}",0.8],{1.8,0.35}],
		Text[xLaTeX["V_{Tp}",0.8],{2.3,0.35}]
	},
	Evaluate@xFigTicks[{0,2.5,0.5},{2,1},{0,2.5,0.5},{2,1}],
	Evaluate@xFigLabel["V_{in}","V_{out}"],
	Evaluate@xFigConfig,Exclusions->None]


fig2=Plot[Evaluate[(xVout[Vin])/.val],{Vin,0,2.5},
	Evaluate@xFigTicks[{0,2.5,0.5},{2,1},{0,2.5,0.5},{2,1}],
	PlotStyle->Black,
	Evaluate@xFigLabel["V_{in}","V_{out}"],Evaluate@xFigConfig,Exclusions->None]
fig3=Plot[Evaluate[(D[xVout[Vin],Vin])/.val],{Vin,0,2.5},PlotRange->{-3.2,0.2},PlotPoints->500,
	Evaluate@xFigTicks[{0,2.5,0.5},{2,1},{-3,0,1},{2,1}],
	PlotStyle->Black,
	Evaluate@xFigLabel["V_{in}","\\dv*{V_{in}}{V_{out}}"],Evaluate@xFigConfig,Exclusions->None]


fig4=Plot[Evaluate[(xVout[Vin])/.valListA],{Vin,0,2.5},
	PlotStyle->{Black,Blue,Red},
	PlotLegends->Placed[
	LineLegend[
		{Style["\:6807\:51c6",FontSize->8,FontFamily->"Noto Sans Mono CJK SC"],
		Style["\:597d\:7684NMOS \:5dee\:7684PMOS",FontSize->8,FontFamily->"Noto Sans Mono CJK SC"],
		Style["\:597d\:7684PMOS \:5dee\:7684NMOS",FontSize->8,FontFamily->"Noto Sans Mono CJK SC"]},
		LegendFunction->"Panel",
		LegendLayout -> {"Row",3}],{Right,Top}],
	Evaluate@xFigTicks[{0,2.5,0.5},{2,1},{0,2.5,0.5},{2,1}],
	Evaluate@xFigLabel["V_{in}","V_{out}"],Evaluate@xFigConfig,Exclusions->None]
fig5=Plot[Evaluate[(xVout[Vin])/.valListB],{Vin,0,2.5},
	PlotLegends->Placed[LineLegend[
		{
			xLaTeX["V_{DD}=2.5\\si{V}",0.7],
			xLaTeX["V_{DD}=2.0\\si{V}",0.7],
			xLaTeX["V_{DD}=1.5\\si{V}",0.7],
			xLaTeX["V_{DD}=1.0\\si{V}",0.7]
		},
		LegendFunction->"Panel",
		LegendLayout -> {"Row",4}],{Right,Top}],
	Evaluate@xFigTicks[{0,2.5,0.5},{2,1},{0,2.5,0.5},{2,1}],
	Evaluate@xFigLabel["V_{in}","V_{out}"],Evaluate@xFigConfig,Exclusions->None]


fig={fig1,fig2,fig3,fig4,fig5}


figWithName=Transpose[{fig,nbPDFNameList[[1;;Length[fig]]]}];
ReleaseHold[(Hold[Export[#[[2]],#[[1]],ImageResolution->2000]]&)/@figWithName]


(*-*)
