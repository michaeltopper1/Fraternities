var gvjs_nW="ABCDEFGHIJKLMNOPQRSTUVWXYZ",gvjs_oW="allValuesSuffix",gvjs_pW="annotationsContainer",gvjs_qW="annotationsFilterContainer",gvjs_rW="background-color",gvjs_sW="chartContainer",gvjs_tW="containerTable",gvjs_uW="dateFormat",gvjs_vW="displayAnnotations",gvjs_wW="displayDateBarSeparator",gvjs_xW="displayLegendDots",gvjs_yW="displayLegendValues",gvjs_zW="displayRangeSelector",gvjs_AW="displayZoomButtons",gvjs_BW="numberFormats",gvjs_CW="outerChartContainer",gvjs_DW="zoomButtons.";
function gvjs_jka(a){return void 0!==a.lastElementChild?a.lastElementChild:gvjs_Ah(a.lastChild,!1)}function gvjs_EW(a,b,c){var d=a.Y(c);if(d!==gvjs_Pb&&d!==gvjs_Qb)throw Error(gvjs_za+c+" must be of type date or datetime, but is "+(d+"."));return a.getValue(b,c)}
var gvjs_kka={annotationsWidth:25,annotationsFilter:"off",scaleValues:null,dateFormat:"MMMM dd, yyyy",displayRangeSelector:!0,displayAnnotations:!0,displayAnnotationsFilter:!1,displayZoomButtons:!0,zoomButtons:{"1-second":{label:"1s",offset:[0,0,1]},"5-seconds":{label:"5s",offset:[0,0,5]},"10-seconds":{label:"10s",offset:[0,0,10]},"15-seconds":{label:"15s",offset:[0,0,15]},"1-minute":{label:"1min",offset:[0,1,0]},"5-minutes":{label:"5min",offset:[0,5,0]},"10-minutes":{label:"10min",offset:[0,10,0]},
"15-minutes":{label:"15min",offset:[0,15,0]},"1-hour":{label:"1h",offset:[1,0,0]},"6-hours":{label:"6h",offset:[6,0,0]},"1-day":{label:"1d",offset:[1,0,0,0,0]},"5-days":{label:"5d",offset:[5,0,0,0,0]},"1-week":{label:"1w",offset:[7,0,0,0,0]},"1-month":{label:"1m",offset:[1,0,0,0,0,0]},"3-months":{label:"3m",offset:[3,0,0,0,0,0]},"6-months":{label:"6m",offset:[6,0,0,0,0,0]},"1-year":{label:"1y",offset:[1,0,0,0,0,0,0]},max:{label:gvjs_Fv,range:{start:null,end:null}}},zoomButtonsOrder:["1-hour","1-day",
"5-days","1-week","1-month","3-months","6-months","1-year",gvjs_Fv],displayLegendDots:!0,displayDateBarSeparator:!0,displayExactValues:!1,fill:0,lclt:0,labelColors:null,allowHtml:!1},gvjs_lka={annotations:{textStyle:{fontSize:10,auraColor:gvjs_e},boxStyle:{stroke:"#888888",strokeWidth:.5,rx:2,ry:2,gradient:{color1:"#eeeeee",color2:"#dddddd",x1:gvjs_Lo,y1:gvjs_Lo,x2:gvjs_Lo,y2:gvjs_Mo,useObjectBoundingBoxUnits:!0}}},backgroundColor:gvjs_ea,chartArea:{width:gvjs_Mo,backgroundColor:gvjs_ea},height:200,
width:gvjs_Mo,hAxis:{baselineColor:gvjs_e,slantedText:!1,maxAlternation:1,gridlines:{count:-1},textStyle:{fontSize:9}},vAxis:{baselineColor:"#ababab",gridlines:{count:-1,color:"#ECECF7"},textPosition:gvjs_Fp,inTextPosition:"high",viewWindowMode:gvjs_Kv},legend:{position:gvjs_e,alignment:gvjs_T},colors:gvjs_EF,targetAxisIndex:1,focusTarget:gvjs_It,tooltip:{trigger:gvjs_e},candlestick:{hollowIsRising:!0},forceIFrame:!1,animation:{duration:0}},gvjs_mka={filterColumnIndex:0,ui:{chartType:gvjs_qa,chartOptions:{chartArea:{width:gvjs_Mo,
height:gvjs_Mo,backgroundColor:{fill:gvjs_ea,stroke:"#ababab",strokeWidth:.5}},height:40,width:gvjs_Mo,backgroundColor:gvjs_ea,areaOpacity:.1,lineWidth:1,forceIFrame:!1,hAxis:{baselineColor:gvjs_e,slantedText:!1,maxAlternation:1,gridlines:{},textPosition:gvjs_Fp,textStyle:{fontSize:9}},vAxis:{baselineColor:gvjs_e,scaleType:gvjs_Uv}},sideScreenColor:{fill:"#f2f2f2",fillOpacity:.75},zoomAroundSelection:!0,chartView:{columns:[0,1]}}},gvjs_nka={width:gvjs_Mo,height:gvjs_Mo,allowHtml:!0,sortAscending:!1,
sortColumn:0};function gvjs_FW(a){gvjs_O.call(this,a);this.gb=null;this.DH=!0;this.qa=null;this.wi=0;this.DM=this.qV=this.Iu=this.bj=this.Cx=this.Hu=this.Fh=this.R=this.mG=this.um=this.Jc=this.J=this.m=this.xu=null;this.CI={};this.gr=this.SN=this.OE=this.pf=null;this.aa=new gvjs_Jq;this.Sd=new gvjs_TJ(this);this.yZ=!1;this.G=gvjs_rl()}gvjs_r(gvjs_FW,gvjs_O);gvjs_=gvjs_FW.prototype;
gvjs_.nf=function(){this.aa.removeAll();gvjs_J(this.Sd);this.Sd=new gvjs_TJ(this);gvjs_J(this.aa);this.aa=new gvjs_Jq;this.yZ=!1;this.bj&&this.bj.clear();this.Fh&&this.Fh.clear();this.R&&this.R.clear();this.container&&this.G.Ac(this.container)};gvjs_.vv=function(){return this.R.vf()};gvjs_.getSelection=function(){var a=this;return this.vv().getSelection().map(function(b){b=gvjs_GW(a,b.row,b.column);return{row:b.row,column:b.column}})};
gvjs_.setSelection=function(a){var b=this,c=this.vv();null==a||0===a.length?c.setSelection([]):(a=a.map(function(d){d=gvjs_HW(b,d.row,d.column);return{row:d.row,column:d.column}}),c.setSelection(a))};gvjs_.o$=function(){return this.Fh.getState().range};gvjs_.setVisibleChartRange=function(a,b,c){this.Fh.setState({range:{start:a,end:b}});this.Fh.draw();gvjs_IW(this,null==c?!0:c)};gvjs_.qaa=function(a){gvjs_JW(this,a,!0)};gvjs_.yfa=function(a){gvjs_JW(this,a,!1)};
function gvjs_JW(a,b,c){Array.isArray(b)||(b=[b]);for(var d=0;d<b.length;d++)a.CI[b[d]]=c;a.Vt()}gvjs_.Vt=function(){this.draw(this.qa,this.xu,this.J)};
gvjs_.hg=function(a,b,c,d){this.xu=c||{};this.J=d||{};this.VL(b);this.UZ(b);gvjs_oka(this);a=this.Zi=new gvjs_L(this.qa);a.wp(this.pf);b=a.it([{column:this.wi,test:function(e){return null!=e}}]);a.Ap(b);b=a.un(this.wi);a.Ap(b);b=gvjs_EW(this.Zi,0,this.wi);c=gvjs_EW(this.Zi,this.Zi.ca()-1,this.wi);this.UN={min:b,max:c};this.DO();gvjs_pka(this);gvjs_KW(this);gvjs_qka(this);gvjs_rka(this);gvjs_ska(this);gvjs_tka(this);gvjs_uka(this);gvjs_vka(this);this.R.eh(a);this.Fh.eh(a);this.Fh.draw();gvjs_IW(this,
!1)};gvjs_.UZ=function(a){this.qa=a;this.pp()};gvjs_.VL=function(a){if(!a)throw Error(gvjs_js);var b=a.Z();if(2>b)throw Error(gvjs_is);if(a.Y(0)!=gvjs_Pb&&a.Y(0)!=gvjs_Qb)throw Error("First column must contain date, or date and time.");for(var c,d,e=!1,f=0,g=1;g<b&&!e;g++)c=a.Y(g),c==gvjs_f?f=1:0==f?(e=!0,d=g):1==f?c==gvjs_j?f=2:(e=!0,d=g):2==f&&(c==gvjs_j?f=0:(e=!0,d=g));if(e)throw Error("Each values column may be followed by one or two annotation columns. column number "+d+gvjs_vr+c);};
function gvjs_pka(a){var b=a.gb;b&&b.gS&&gvjs_H(b.gS,gvjs_pu,gvjs_e);var c=a.getHeight(a.m),d=a.Oa(a.m);if(b){gvjs_Eq(b.gS,d,c);gvjs_H(b.gS,gvjs_pu,"");var e=gvjs_Kk(b.GG);a.Pa=c-(e.top+e.bottom);a.da=d-(e.left+e.right);gvjs_Eq(b.GG,a.da,a.Pa)}else{b=a.gb={};var f=a.G;f.Ac(a.container);gvjs_YB(a.container,["google-visualization-atl","container"]);var g=a.container.id+"_AnnotationChart_",h=function(l,m,n){var p=f.createElement(gvjs_Sb);l&&(p.id=g+l);p.className=m;n.appendChild(p);return p},k=b.gS=
h("","",a.container);gvjs_H(k,gvjs_zd,gvjs_Dd);gvjs_Eq(k,d,c);k=h("","",k);gvjs_H(k,gvjs_zd,gvjs_R);gvjs_Cq(k,0,0);gvjs_Eq(k,gvjs_Mo,gvjs_Mo);k=b.GG=h("borderDiv","border",k);k=b.hCa=h(gvjs_tW,gvjs_tW,k);e=gvjs_Kk(b.GG);a.Pa=c-(e.top+e.bottom);a.da=d-(e.left+e.right);gvjs_Eq(b.GG,a.da,a.Pa);a=b.bCa=h("chartTd","td chartTdContainer",k);c=b.rV=h("annotationsTd","td annotationsTdContainer",k);d=b.dDa=h(gvjs_CW,gvjs_CW,a);k=b.ola=h("chartControlsContainer","chartControls",d);b.RF=h("zoomControlContainer",
"zoomControls",k);b.Csa=h("legendContainer",gvjs_tv,k);b.mla=h(gvjs_sW,gvjs_sW,d);b.S1=h("rangeControlContainer","rangeControl",a);b.nG=h(gvjs_qW,gvjs_qW,c);b.pV=b.pV=h(gvjs_pW,gvjs_pW,c)}}
function gvjs_ska(a){var b=1+gvjs_xi(a.Jc,["series.0.pointSize",gvjs_jw],6),c=gvjs_Hk(a.gb.ola).height,d=gvjs_E(a.m,gvjs_zW)?gvjs_F(a.um,"ui.chartOptions.height"):0;c=a.Pa-(c+d);var e=Math.max(b,gvjs_F(a.Jc,gvjs_Tt,0)),f=Math.max(b,2+2*gvjs_F(a.Jc,"hAxis.textStyle.fontSize"));f=Math.max(f,gvjs_F(a.Jc,gvjs_Pt,0));gvjs_kq(a.Jc,1,{height:c,chartArea:{top:e,bottom:f,height:c-(e+f)}});gvjs_kq(a.um,1,{ui:{chartOptions:{chartArea:{height:d}}}});d=0;a.DH?(d=a.mG.ea(gvjs_Zd,gvjs_yb),gvjs_Dq(a.gb.rV,d),d=gvjs_ey(a.gb.rV).width,
d=Math.max(0,d)):gvjs_Dq(a.gb.rV,0);d=a.da-d;c=Math.max(b,gvjs_F(a.Jc,gvjs_Rt,0));b=Math.max(b,gvjs_F(a.Jc,gvjs_St,0));e=d-(c+b);gvjs_kq(a.Jc,1,{width:d,chartArea:{left:c,right:b,width:e}});gvjs_kq(a.um,1,{ui:{chartOptions:{width:d,chartArea:{left:c+1,right:b+2,width:e-3}}}})}
function gvjs_tka(a){a.R||(a.R=new gvjs_Q({chartType:gvjs_Ba,container:a.gb.mla}));a.Fh||(a.Fh=new gvjs_P({controlType:"ChartRangeFilter",container:a.gb.S1}),gvjs_1m(a.Fh,gvjs_g,gvjs_q(function(){gvjs_Ym(this.Fh,gvjs_Kd,gvjs_q(this.Aqa,this))},a)));if(a.DH){var b=gvjs_E(a.m,"displayAnnotationsFilter"),c=gvjs_q(function(){var d=this.DH&&b?gvjs_ey(this.gb.nG).height:0;this.gb.pV.style.height=gvjs_Bq(this.Pa-d+gvjs_U,!0)},a);a.oG||(a.oG=new gvjs_P({controlType:"StringFilter",container:a.gb.nG,options:{filterColumnLabel:"Text",
matchType:"any",useFormattedValue:!0,ui:{label:"Filter",labelSeparator:": "}}}));gvjs_H(a.gb.nG,gvjs_pu,b?"":gvjs_e);a.bj||(a.bj=new gvjs_Q({chartType:gvjs_hb,container:a.gb.pV}),gvjs_1m(a.bj,gvjs_g,gvjs_q(function(){gvjs_Ym(this.bj,gvjs_i,gvjs_q(this.Jqa,this))},a)));a.Hu?c():(a.Hu=new gvjs_lo(a.gb.nG),a.Hu.bind(a.oG,a.bj),gvjs_1m(a.Hu,gvjs_g,c))}else gvjs_Dg(a.gb.nG,gvjs_yg),a.bj&&(a.bj.clear(),a.bj=null),a.oG&&(a.oG.clear(),a.oG=null),a.Hu&&(a.Hu.clear(),a.Hu=null);a.yZ||(a.yZ=!0,gvjs_Ym(a.R,gvjs_g,
gvjs_q(a.R_,a)),gvjs_1m(a.R,gvjs_g,gvjs_q(function(){var d=this.vv();gvjs_Ym(d,gvjs_i,gvjs_q(this.eqa,this));gvjs_Ym(d,gvjs_5v,gvjs_q(this.gqa,this));gvjs_Ym(d,gvjs_4v,gvjs_q(this.fqa,this))},a)))}gvjs_.R_=function(){this.Sd.dispatchEvent(gvjs_g)};gvjs_.Aqa=function(){var a=this.Fh.getState();gvjs_IW(this,!1);this.Sd.dispatchEvent(gvjs_tw,a.range)};
function gvjs_IW(a,b){var c=a.Fh.getState();a.R.$("hAxis.viewWindow",{min:c.range.start,max:c.range.end});c=a.R.getOption(gvjs_0s);b&&0===c&&a.R.$(gvjs_0s,150);a.R.draw();a.R.$(gvjs_0s,c)}
function gvjs_oka(a){a.pf=[{label:"Datetime",type:a.qa.Y(a.wi),calc:gvjs_q(function(g,h){return{v:g.getValue(h,this.wi),p:{dataRow:h}}},a)}];a.OE=[];for(var b=0,c=a.wi+1,d=a.qa.Z();c<d;){var e=c,f={label:a.qa.Ca(c)+"  ",sourceColumn:c,properties:{dataColumn:e,viewColumn:a.pf.length}};a.CI[b]&&(f.type=gvjs_f,f.calc=function(){return null});a.pf.push(f);a.OE.push(f);c++;c<d&&a.qa.Y(c)===gvjs_j&&(c++,c<d&&a.qa.Y(c)===gvjs_j&&c++,a.CI[b]||a.pf.push({calc:function(g){return gvjs_q(function(h,k){return(h=
gvjs_LW(this,k,g))?h.key:null},this)}.call(a,e),type:gvjs_j,role:gvjs_2s,properties:{dataColumn:e}}));b++}for(c=b=0;c<a.OE.length;c++)if(!a.CI[c]){b=c;break}gvjs_kq(a.um,1,{ui:{chartView:{columns:[0,a.OE[b].properties.viewColumn]}}});gvjs_kq(a.um,1,{ui:{chartOptions:{colors:[a.Mb[b%a.Mb.length]]}}})}
gvjs_.DO=function(){function a(){return("      "+b(f++)).substr(-e)}function b(n){return 26<=n?b(Math.floor(n/26)-1)+gvjs_nW[n%26]:gvjs_nW[n]}var c=this.qa.Z(),d=this.qa.ca();this.Iu=[];this.qV={};for(var e=Math.ceil(Math.log(d)/Math.log(26)),f=0,g=0;g<d;g++)for(var h=this.wi+1;h<c;){var k=h;h++;if(h<c&&this.qa.Y(h)===gvjs_j){var l=h,m=null;h++;h<c&&this.qa.Y(h)===gvjs_j&&(m=h,h++);(k=gvjs_wka(this,this.Iu.length,g,k,l,m,a))&&gvjs_xka(this,k)}}this.DH=gvjs_E(this.m,gvjs_vW)&&0<this.Iu.length};
function gvjs_wka(a,b,c,d,e,f,g){e=e&&a.qa.getValue(c,e);f=f&&a.qa.getValue(c,f);return e||f?(g=g(),a=a.qa.getValue(c,a.wi),{idx:b,key:g,title:e,text:f,date:a,eX:c,Ks:d}):null}
function gvjs_yka(a){a.Cx=new gvjs_G;a.Cx.oe(gvjs_j,"Key");a.Cx.oe(gvjs_j,"Text");a.DM=[];for(var b=[],c=0;c<a.Iu.length;c++){var d=a.Iu[c],e='<button type="button" class="key">'+gvjs_Fg(d.key)+"</button>",f=d.date;a.SN&&(f=gvjs_Fg(a.SN.$b(d.date)));var g=d.title||"";d=d.text||"";gvjs_E(a.m,gvjs_Zs)||(g=gvjs_Fg(g),d=gvjs_Fg(d));b.push([e,'<span class="title">'+g+'</span> <span class="description">'+d+'</span> <span class="date">'+f+"</span> "])}a.Cx.Pp(b);var h=a.Cx.ca();b.forEach(function(k,l){a.Cx.pfa(l,
"idx",h-l-1);a.DM[h-l-1]=l})}function gvjs_uka(a){if(a.DH){gvjs_yka(a);var b=gvjs_Fy(a.mG);b.width=gvjs_Mo;b.height=gvjs_Mo;a.bj.setOptions(b);a.Hu.draw(a.Cx)}}
gvjs_.pp=function(){this.m=new gvjs_ji([this.xu,gvjs_kka]);this.Jc=new gvjs_ji([gvjs_Gy(this.m,gvjs_Fb),gvjs_lka]);this.um=new gvjs_ji([gvjs_Gy(this.m,gvjs_sw),gvjs_mka]);this.mG=new gvjs_ji([gvjs_Gy(this.m,gvjs_Od),gvjs_nka]);var a=this.m.mb(gvjs_kt);a&&(gvjs_H(this.gb.GG,gvjs_rW,a),gvjs_kq(this.Jc,1,{backgroundColor:a}),gvjs_kq(this.um,1,{ui:{chartOptions:{backgroundColor:a}}}));gvjs_kq(this.Jc,1,{colors:this.m.ea(gvjs_7t)});this.Mb=this.Jc.ea(gvjs_7t);gvjs_kq(this.um,1,{ui:{chartOptions:{colors:[this.Mb[0]]}}});
typeof this.m.ea(gvjs_lp)===gvjs_f&&gvjs_kq(this.Jc,1,{areaOpacity:gvjs_F(this.m,gvjs_lp)/100,seriesType:gvjs_et});a=gvjs_E(this.m,gvjs_jv,!1);gvjs_kq(this.Jc,1,{interpolateNulls:a});gvjs_kq(this.um,1,{ui:{chartOptions:{interpolateNulls:a}}});gvjs_kq(this.Jc,1,{vAxis:{maxValue:this.m.ea(gvjs_Fv),minValue:this.m.ea(gvjs_Nv)}});if(a=this.m.ea("scaleColumns"))if(0===a.length)gvjs_kq(this.Jc,1,{vAxis:{textPosition:gvjs_e}});else{for(var b=1===a.length,c={},d=0;d<a.length;d++)c[a[d]]=b?{targetAxisIndex:1}:
{targetAxisIndex:d};gvjs_kq(this.Jc,1,{series:c})}else gvjs_kq(this.Jc,1,{vAxis:{targetAxisIndex:1}});this.m.ea(gvjs_Ew)&&(this.m.ea(gvjs_Ew)===gvjs_Bu||"allfixed"===this.m.ea(gvjs_Ew))&&gvjs_kq(this.Jc,1,{vAxis:{baseline:0}});this.qa.Y(this.wi)===gvjs_Qb?gvjs_kq(this.m,1,{minDisplaySeconds:60,dateFormat:"HH:mm MMMM dd, yyyy",minTimelineGranularity:0}):gvjs_kq(this.m,1,{minDisplaySeconds:86400,dateFormat:"MMMM dd, yyyy",minTimelineGranularity:86400});a=this.m.ea(gvjs_uW);a=typeof a===gvjs_j?{pattern:gvjs_D(this.m,
gvjs_uW)}:gvjs_Gy(this.m,gvjs_uW);this.SN=new gvjs_Oi(a);a=this.m.ea(gvjs_BW);if(typeof a===gvjs_j)a={pattern:gvjs_D(this.m,gvjs_BW)},this.gr=new gvjs_bj(a);else if(gvjs_p(a)){var e=[];this.gr=e;gvjs_v(gvjs_Gy(this.m,gvjs_BW),function(f,g){f={pattern:f};g=parseInt(g,10);e[g]=new gvjs_bj(f)})}else gvjs_E(this.m,"displayExactValues")?this.gr=null:(a={pattern:"#.##"},this.gr=new gvjs_bj(a));a=gvjs_D(this.m,"scaleFormat","#.########")+gvjs_D(this.m,gvjs_oW,"");gvjs_kq(this.Jc,1,{vAxis:{format:a}});a=
this.m.Da("thickness");gvjs_kq(this.Jc,1,{lineWidth:null!=a?Math.max(1,a):1});a=this.m.ea("annotationsWidth");typeof a===gvjs_f?gvjs_kq(this.mG,1,{width:a+"%"}):a&&gvjs_kq(this.mG,1,{width:a});this.nla="newRow"===gvjs_D(this.m,"legendPosition","sameRow")?"twoRows":"oneRow";gvjs_kq(this.um,1,{ui:{chartOptions:{width:this.Jc.ea(gvjs_Zd)}}});gvjs_kq(this.um,1,{ui:{chartOptions:{chartArea:{width:this.Jc.ea(gvjs_Ut)}}}});this.m.ea("async",!1)&&(gvjs_kq(this.Jc,1,{async:!0}),gvjs_kq(this.um,1,{ui:{chartOptions:{async:!0}}}))};
function gvjs_vka(a){var b=a.Fh.getState()||{};b.range=b.range||{};var c=a.m.ea("zoomEndTime");null!=c&&(b.range.end=c);c=a.m.ea("zoomStartTime");null!=c&&(b.range.start=c);a.Fh.setState(b);gvjs_E(a.m,gvjs_zW)?a.gb.S1.style.display="":a.gb.S1.style.display=gvjs_e;b=gvjs_Fy(a.Jc);a.R.setOptions(b);b=gvjs_Fy(a.um);a.Fh.setOptions(b)}
function gvjs_qka(a){if(gvjs_E(a.m,"displayAggregationButtons")){var b=[{id:"daily",label:"Daily",modCalc:function(l){if(null!=l)return new Date(l.getFullYear(),l.getMonth(),l.getDate(),0)}},{id:"weekly",label:"Weekly",modCalc:function(l){if(null!=l)return new Date(l.getFullYear(),l.getMonth(),l.getDate()-l.getDay())}},{id:"monthly",label:"Monthly",modCalc:function(l){if(null!=l)return new Date(l.getFullYear(),l.getMonth(),1)}},{id:"year",label:"Yearly",modCalc:function(l){if(null!=l)return new Date(l.getFullYear(),
0,1)}}],c=a.gb.RF.id;var d="Aggregate: ";for(var e=0;e<b.length;e++){var f=b[e],g=c+"_"+f.id;d+=' <button type="button" id="'+g+'" href="#">'+gvjs_Fg(f.label)+"</button>"}d+="<br />";e=function(l){for(var m=0;m<l.length;m++)if(null!==l[m])return l[m]};var h=[];f=a.qa.Z();for(g=a.wi+1;g<f;)h.push({column:g,type:gvjs_f,label:a.qa.Ca(g),aggregation:gvjs_wo}),g++,g<f&&(h.push({column:g,type:gvjs_j,label:a.qa.Ca(g),aggregation:e}),g++,g<f&&(h.push({column:g,type:gvjs_j,label:a.qa.Ca(g),aggregation:e}),
g++));var k=a.qa.Ca(a.wi);for(e=0;e<b.length;e++)f=b[e],d=gvjs_q(function(l){return gvjs_q(function(){gvjs_xo(this.qa,[{column:this.wi,type:gvjs_Pb,label:k,modifier:l}],h)},this)},a),g=c+"_"+f.id,a.aa.D(a.G.j(g),gvjs_1t,d(f.modCalc))}}
function gvjs_rka(a){var b=a.gb.RF.id,c=gvjs_E(a.m,gvjs_AW),d=gvjs_oi(a.m,gvjs_ri,[],"zoomButtonsOrder",void 0,void 0),e=a.G;e.Ac(a.gb.RF);if(c){e.append(a.gb.RF,"Zoom: ");c={};for(var f=0;f<d.length;c={ZF:c.ZF,KU:c.KU},f++){var g=d[f];if(!a.m.ob(gvjs_DW+g))break;var h=gvjs_D(a.m,gvjs_DW+g+".className","zoomButton"),k=gvjs_D(a.m,gvjs_DW+g+".label",g);h=gvjs_vg(gvjs_Ct,{type:gvjs_Ct,id:b+"_"+g,"class":h,href:"#"},k);c.ZF=gvjs_uy(e.jd,h);e.append(a.gb.RF,c.ZF);c.ZF=gvjs_jka(a.gb.RF);c.KU=gvjs_q(gvjs_zka(a,
g),a);gvjs_1m(a,gvjs_g,function(l){return function(){a.aa.D(l.ZF,gvjs_1t,l.KU)}}(c))}}}
function gvjs_zka(a,b){var c=a.m.ea(gvjs_DW+b+".offset");if(c)var d=gvjs_ff(c);var e=a.m.ob(gvjs_DW+b+".range");return gvjs_q(function(){var f=this.Fh.getState();var g=f.range.end;null==g&&(g=this.UN.max);if(c&&null!=g){g=gvjs_9z(g,d);var h=this.UN.min;g=g.getTime()>h.getTime()?g:h;h={start:g}}else e&&(h=e);void 0!==h.start&&(f.range.start=h.start);void 0!==h.end&&(f.range.end=h.end);this.Fh.setState(f);this.Fh.draw();gvjs_IW(this,!0)},a)}
gvjs_.fqa=function(a){a=gvjs_GW(this,a.row,a.column);this.Sd.dispatchEvent(gvjs_4v,{row:a.row,column:a.column});var b;a=this.getSelection();0<a.length&&(b=a[0].row);gvjs_KW(this,b||void 0)};gvjs_.gqa=function(a){var b=gvjs_GW(this,a.row,a.column);this.Sd.dispatchEvent(gvjs_5v,{row:b.row,column:b.column});a=this.R.Xa().getProperty(a.row,this.wi,"dataRow");gvjs_KW(this,a)};
function gvjs_KW(a,b){for(var c=gvjs_yg,d=0;d<a.OE.length;d++)if(!a.CI[d]){var e=a.OE[d],f=e.properties.dataColumn,g=a.Mb[d%a.Mb.length];g&&g.color&&(g=g.color);var h=gvjs_E(a.m,gvjs_xW)?gvjs_vg(gvjs_Sb,{"class":"legend-dot",style:gvjs_7f({"background-color":g})}):gvjs_yg;c=gvjs_zg(c,gvjs_vg(gvjs__w,{style:gvjs_7f({color:g})},gvjs_zg(h,e.label)));null!=b&&gvjs_E(a.m,gvjs_yW,!0)&&(f=a.qa.getValue(b,f),null!==f&&(e=f,g=null,a.gr&&(g=Array.isArray(a.gr)?a.gr[d]:a.gr),g&&(e=g.$b(f)),(f=a.m.mb(gvjs_oW))&&
(e+=f),c=gvjs_zg(c,": ",e)));c=gvjs_zg(c,gvjs_lA("&nbsp;&nbsp;"))}if(null!=b){var k=a.qa.getValue(b,a.wi);k=a.SN.$b(k)}"oneRow"===a.nla?(b=a.m.ea(gvjs_wW),b=!0===b?"|&nbsp;":!1===b?"&nbsp; &nbsp;":b.toString(),c=k?gvjs_zg(c,gvjs_lA(b),k):c):c=gvjs_zg(k?k:"",gvjs_lA("&nbsp;&nbsp;"),gvjs_vg("br"),c);gvjs_Dg(a.gb.Csa,c)}function gvjs_GW(a,b,c){var d=a.R.Xa();return{row:null!=b?d.getProperty(b,a.wi,"dataRow"):null,column:null!=c?d.Xe(c,"dataColumn"):null}}
function gvjs_HW(a,b,c){return{row:null==b?null:a.Zi.aZ(b),column:null==c?null:a.Zi.l$(c)}}gvjs_.eqa=function(){this.Sd.dispatchEvent(gvjs_i);if(this.bj&&!this.eaa){var a=this.bj.vf();if(a){var b;if((b=this.vv().getSelection())&&b.length){b=b[0];var c=b.column;b=!c||1>c?null:b}else b=null;b?(b=gvjs_GW(this,b.row,b.column),(b=gvjs_LW(this,b.row,b.column))?(c=this.bj.Xa(),b=c.ca()-c.aZ(this.DM[b.idx])-1,a.setSelection([{row:b}])):a.setSelection([])):a.setSelection([])}}};
function gvjs_LW(a,b,c){b=[String(b),String(c)].join();return a.Iu[a.qV[b]]}function gvjs_xka(a,b){var c=b.eX+","+b.Ks,d=b.idx;a.Iu.push(b);a.qV[c]=d}
gvjs_.Jqa=function(){var a=this.vv(),b=this.bj.vf().getSelection()[0];if(b){this.eaa=!0;var c=this.bj.Xa();b=c.ca()-c.nj(b.row)-1;var d=this.Iu[this.DM[b]];b=gvjs_q(function(){var e=this.R.cd().hAxis.viewWindow,f=e&&e.min&&e.min.getTime()||-Infinity;e=e&&e.max&&e.max.getTime()||Infinity;var g=d.date.getTime(),h=gvjs_HW(this,d.eX,d.Ks);return g>=f&&g<=e?(a.setSelection([{row:h.row,column:h.column+1}]),this.eaa=!1,!0):!1},this);b()||(c=gvjs_g,gvjs_xi(this.Jc,gvjs_0s)&&(c=gvjs_1s),gvjs_1m(a,c,b),gvjs_Aka(this,
d.eX))}else a.setSelection([])};function gvjs_Aka(a,b){var c=a.UN.min.getTime(),d=a.UN.max.getTime(),e=a.R.cd().hAxis.viewWindow,f=e&&e.min&&e.min.getTime()||c;e=e&&e.max&&e.max.getTime()||d;b=a.qa.getValue(b,a.wi).getTime();f=e-f;b=Math.max(c,b-Math.round(f/2));d=Math.min(d,b+f);b=Math.max(c,d-f);a.setVisibleChartRange(new Date(b),new Date(d),!0)};gvjs_o(gvjs_7b,gvjs_FW,void 0);gvjs_FW.prototype.clearChart=gvjs_FW.prototype.Lb;gvjs_FW.prototype.draw=gvjs_FW.prototype.draw;gvjs_FW.prototype.getContainer=gvjs_FW.prototype.getContainer;gvjs_FW.prototype.getSelection=gvjs_FW.prototype.getSelection;gvjs_FW.prototype.getVisibleChartRange=gvjs_FW.prototype.o$;gvjs_FW.prototype.setVisibleChartRange=gvjs_FW.prototype.setVisibleChartRange;gvjs_FW.prototype.showDataColumns=gvjs_FW.prototype.yfa;gvjs_FW.prototype.hideDataColumns=gvjs_FW.prototype.qaa;