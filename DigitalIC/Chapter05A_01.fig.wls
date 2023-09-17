#!/usr/bin/env wolframscript
(* ::Package:: *)

ClearAll["Global`*"]


SetDirectory[NotebookDirectory[]];
Needs["MaTeX`"];


nbFileName="Chapter05A_01.fig.wls";
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


xFig3DConfig={
	ViewPoint->{-100,-80,65},PlotPoints -> 50,MaxRecursion -> 5,
	ExclusionsStyle->{None,{Black,Thin}},ColorFunction->ColorData["DarkRainbow"],
	BaseStyle->xLabelFont};
xFig3DTicks=(Ticks->{
	Table[{x,xLaTeX[ToString[PaddedForm[x,{#2[[1]],#2[[2]]}]],0.5]},{x,#1[[1]],#1[[2]],#1[[3]]}],
	Table[{x,xLaTeX[ToString[PaddedForm[x,{#4[[1]],#4[[2]]}]],0.5]},{x,#3[[1]],#3[[2]],#3[[3]]}],
	Table[{x,xLaTeX[ToString[PaddedForm[x,{#6[[1]],#6[[2]]}]],0.5]},{x,#5[[1]],#5[[2]],#5[[3]]}]})&;
xFig3DLabel=(AxesLabel->{xLaTeX[#1,0.8],xLaTeX[#2,0.8],xLaTeX[#3,0.8]})&;


xFigCTConfig={
	PlotLegends->Automatic,
	ColorFunction->ColorData["DarkRainbow"],
	BaseStyle->{FontFamily->"Latin Modern Roman",FontSize->8},
	ImageSize->Automatic->300,
	PlotPoints->50,
	MaxRecursion->5,
	ExclusionsStyle->{None,{Red,Thin}}};
xFigCTFast={};
(*xFigCTFast={PlotPoints->5,MaxRecursion->1};*)


xVNT0=+0.43;
xVPT0=-0.40;

xVNDSAT=+0.63;
xVPDSAT=-1.00;

xNlambda=+0.06;
xPlambda=-0.10;

xVT=0.40;
xVDSAT=0.60;

xVGT=Function[{VGS},VGS-VT];
xVMin=Function[{VDS,VGS},Min[VDS,VGT,VDSAT]/.{VGT->xVGT[VGS]}];
xVMax=Function[{VDS,VGS},Max[VDS,VGT,VDSAT]/.{VGT->xVGT[VGS]}];

xID=Function[{VDS,VGS},(VGT Vm-Vm^2/2)(1+lambda VDS)/.{VGT->xVGT[VGS]}];
xIDNMOS=Function[{VDS,VGS},Piecewise[{{0,VGT<=0},{0,VGT>0 && VDS<=0},{+xID[VDS,VGS],VGT>0 && VDS>0}}]/.{Vm->xVMin[VDS,VGS],VGT->xVGT[VGS]}];
xIDPMOS=Function[{VDS,VGS},Piecewise[{{0,VGT>=0},{0,VGT<0 && VDS>=0},{-xID[VDS,VGS],VGT<0 && VDS<0}}]/.{Vm->xVMax[VDS,VGS],VGT->xVGT[VGS]}];


xVDD=2.5;
xGND=0.0;

transNMOS={VDS->(Vout-GND),VGS->(Vin-GND)};
transPMOS={VDS->(Vout-VDD),VGS->(Vin-VDD)};

argNMOS={VT->+xVT,VDSAT->+xVDSAT,lambda->0,GND->xGND,VDD->xVDD};
argPMOS={VT->-xVT,VDSAT->-xVDSAT,lambda->0,GND->xGND,VDD->xVDD};

xIoutNMOS=Function[{Vout,Vin},Iout/.{Iout->+xIDNMOS[VDS,VGS]}/.transNMOS/.argNMOS];
xIoutPMOS=Function[{Vout,Vin},Iout/.{Iout->-xIDPMOS[VDS,VGS]}/.transPMOS/.argPMOS];


exclusionNMOS={{VGT==0},{VDS==VDSAT,VGT>=VDSAT},{VDS==VGT,VDS<=VDSAT},{VGT==VDSAT,VDS>=VDSAT}}/.{VGT->xVGT[VGS]};
exclusionPMOS={{VGT==0},{VDS==VDSAT,VGT<=VDSAT},{VDS==VGT,VDS>=VDSAT},{VGT==VDSAT,VDS<=VDSAT}}/.{VGT->xVGT[VGS]};


fig2a=Plot3D[ID/.{ID->xIoutNMOS[Vout,Vin]},{Vin,0,xVDD},{Vout,0,xVDD},
	Exclusions->(exclusionNMOS/.transNMOS/.argNMOS),
	Evaluate@xFig3DConfig,Evaluate@xFig3DLabel["V_{in}","V_{out}","I_D/k_n"],
	Evaluate@xFig3DTicks[{0,2.5,0.5},{2,1},{0,2.5,0.5},{2,1},{0,1,0.5},{2,1}],
	Mesh->25]
fig2b=Plot3D[ID/.{ID->xIoutNMOS[Vout,Vin]},{Vin,0,xVDD},{Vout,0,xVDD},
	Exclusions->(exclusionNMOS/.transNMOS/.argNMOS),
	Evaluate@xFig3DConfig,Evaluate@xFig3DLabel["V_{in}","V_{out}","I_D/k_n"],
	Evaluate@xFig3DTicks[{0,2.5,0.5},{2,1},{0,2.5,0.5},{2,1},{0,1,0.5},{2,1}],
	MeshFunctions->{#3&},
	Mesh->30]


xWText=(Text[Style[#1,White,FontSize->6,FontFamily->"Noto Sans Mono CJK SC"],#2])&;
xGText=(Text[Style[#1,Black,FontSize->6,FontFamily->"Noto Sans Mono CJK SC"],#2])&;


fig3a=ContourPlot[ID/.{ID->xIoutNMOS[Vout,Vin]},{Vin,0,xVDD},{Vout,0,xVDD},
	(*Exclusions->Evaluate@(exclusionNMOS/.transNMOS/.argNMOS),*)
	Contours->Table[x,{x,0,1.1,0.05}],
	PlotLegends->Placed[BarLegend[Automatic,
		LegendLayout->"Row",LabelStyle->xLabelFont,
		LegendFunction->"Panel",LegendMargins->{{-12,0},{0,0}},LegendMarkerSize->250,
		LegendLabel->xLaTeX["~~~(I_{Dn}/k)",0.8]],{0.53,-0.05}],
	Epilog->({
		xWText["N\:622a\:6b62\:533a",{VT/2,xVDD/2}],
		xWText["N\:5939\:65ad\:9971\:548c\:533a",{VT+VDSAT/2,xVDD/2}],
		xWText["N\:901f\:5ea6\:9971\:548c\:533a",{xVDD-(xVDD-(VT+VDSAT))/2,xVDD/2}],
		xWText["N\:7ebf\:6027\:533a",{xVDD-(xVDD-(VT+VDSAT))/2,xVDSAT/2}],
		LightBlue,
		Line[{{0.4,0.0},{0.4,2.5}}],
		Line[{{1.0,0.6},{1.0,2.5}}],
		Line[{{1.0,0.6},{2.5,0.6}}],
		Line[{{1.0,0.6},{0.4,0.0}}]
	}/.argNMOS),
	Evaluate@xFigCTFast,
	Evaluate@xFigCTConfig,
	Evaluate@xFigTicks[{0,2.5,0.5},{2,1},{0,2.5,0.5},{2,1}],
	Evaluate@xFigLabel["V_{in}","V_{out}"]]
fig3b=ContourPlot[ID/.{ID->xIoutPMOS[Vout,Vin]},{Vin,0,xVDD},{Vout,0,xVDD},
	(*Exclusions->Evaluate@(exclusionPMOS/.transPMOS/.argPMOS),*)
	Contours->Table[x,{x,0,1.1,0.05}],
	PlotLegends->Placed[BarLegend[Automatic,
		LegendLayout->"Row",LabelStyle->xLabelFont,
		LegendFunction->"Panel",LegendMargins->{{-12,0},{0,0}},LegendMarkerSize->250,
		LegendLabel->xLaTeX["~~~(I_{Dp}/k)",0.8]],{0.53,-0.05}],
	Epilog->({
		xWText["P\:622a\:6b62\:533a",{xVDD+VT/2,xVDD/2}],
		xWText["P\:5939\:65ad\:9971\:548c\:533a",{xVDD+VT+VDSAT/2,xVDD/2}],
		xWText["P\:901f\:5ea6\:9971\:548c\:533a",{xVDD-(xVDD-(VT+VDSAT))/2,xVDD/2}],
		xWText["P\:7ebf\:6027\:533a",{xVDD-(xVDD-(VT+VDSAT))/2,xVDD-xVDSAT/2}],
		LightRed,
		Line[{{2.1,0.0},{2.1,2.5}}],
		Line[{{1.5,1.9},{1.5,0.0}}],
		Line[{{1.5,1.9},{0.0,1.9}}],
		Line[{{1.5,1.9},{2.1,2.5}}]
	}/.argPMOS),
	Evaluate@xFigCTFast,
	Evaluate@xFigCTConfig,
	Evaluate@xFigTicks[{0,2.5,0.5},{2,1},{0,2.5,0.5},{2,1}],
	Evaluate@xFigLabel["V_{in}","V_{out}"]]


xWTextPairs=({xWText[#1,{#3[[1]],#3[[2]]+0.05}],xWText[#2,{#3[[1]],#3[[2]]-0.05}]})&;
xGTextPairs=({xGText[#1,{#3[[1]],#3[[2]]+0.05}],xGText[#2,{#3[[1]],#3[[2]]-0.05}]})&;


fig4=ContourPlot[ID1-ID2/.{ID1->xIoutNMOS[Vout,Vin],ID2->xIoutPMOS[Vout,Vin]},{Vin,0,xVDD},{Vout,0,xVDD},
	(*Exclusions->Join[exclusionPMOS/.transPMOS/.argPMOS,exclusionNMOS/.transNMOS/.argNMOS],*)
	Contours->Table[x,{x,-1,1,0.2}],
	PlotLegends->Placed[BarLegend[{"DarkRainbow",{-1.2,1.2}},Table[x,{x,-1,1,0.2}],
		LegendLayout->"Row",LabelStyle->xLabelFont,
		LegendFunction->"Panel",LegendMargins->{{-12,0},{0,0}},LegendMarkerSize->250,
		LegendLabel->xLaTeX["~~~~~~(I_{Dn}-I_{Dp})/k",0.8]],{0.53,-0.05}],
	Epilog->({
		xWTextPairs["N\:622a\:6b62\:533a","P\:7ebf\:6027\:533a",{0.20,2.2}],
		xWTextPairs["N\:5939\:65ad\:9971\:548c\:533a","P\:7ebf\:6027\:533a",{0.70,2.2}],
		xWTextPairs["N\:901f\:5ea6\:9971\:548c\:533a","P\:7ebf\:6027\:533a",{1.25,2.2}],
		xWTextPairs["N\:901f\:5ea6\:9971\:548c\:533a","P\:901f\:5ea6\:9971\:548c\:533a",{1.25,1.25}],
		xWTextPairs["N\:7ebf\:6027\:533a","P\:901f\:5ea6\:9971\:548c\:533a",{1.25,0.3}],
		xWTextPairs["N\:7ebf\:6027\:533a","P\:5939\:65ad\:9971\:548c\:533a",{1.80,0.3}],
		xWTextPairs["N\:7ebf\:6027\:533a","P\:622a\:6b62\:533a",{2.30,0.3}],
		xGTextPairs["N\:622a\:6b62\:533a","P\:901f\:5ea6\:9971\:548c\:533a",{0.20,1.25}],
		xGTextPairs["N\:5939\:65ad\:9971\:548c\:533a","P\:901f\:5ea6\:9971\:548c\:533a",{0.70,1.25}],
		xGTextPairs["N\:901f\:5ea6\:9971\:548c\:533a","P\:5939\:65ad\:9971\:548c\:533a",{1.80,1.25}],
		xGTextPairs["N\:901f\:5ea6\:9971\:548c\:533a","P\:622a\:6b62\:533a",{2.30,1.25}],
		LightBlue,
		Line[{{0.4,0.0},{0.4,2.5}}],
		Line[{{1.0,0.6},{1.0,2.5}}],
		Line[{{1.0,0.6},{2.5,0.6}}],
		Line[{{1.0,0.6},{0.4,0.0}}],
		LightRed,
		Line[{{2.1,0.0},{2.1,2.5}}],
		Line[{{1.5,1.9},{1.5,0.0}}],
		Line[{{1.5,1.9},{0.0,1.9}}],
		Line[{{1.5,1.9},{2.1,2.5}}]
	}/.argPMOS),
	Evaluate@xFigCTFast,
	Evaluate@xFigCTConfig,
	Evaluate@xFigTicks[{0,2.5,0.5},{2,1},{0,2.5,0.5},{2,1}],
	Evaluate@xFigLabel["V_{in}","V_{out}"]]


s[Vin_]:=Solve[ID1==ID2/.{ID1->xIoutNMOS[Vout,Vin],ID2->xIoutPMOS[Vout,Vin]},Vout]
fig1=Plot[Vout/.s[Vin],{Vin,0,xVDD},Evaluate@xFigConfig]


fig={fig2a,fig2b,fig3a,fig3b,fig4}


figWithName=Transpose[{fig,nbPDFNameList[[1;;Length[fig]]]}];
ReleaseHold[(Hold[Export[#[[2]],#[[1]],ImageResolution->2000]]&)/@figWithName]


(* *)
