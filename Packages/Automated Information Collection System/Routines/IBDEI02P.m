IBDEI02P ; ; 12-MAY-2016
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;MAY 12, 2016
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,2252,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,2252,1,2,0)
 ;;=2^93295
 ;;^UTILITY(U,$J,358.3,2252,1,3,0)
 ;;=3^ICD Device Interrogate Remote
 ;;^UTILITY(U,$J,358.3,2253,0)
 ;;=93283^^15^170^10^^^^1
 ;;^UTILITY(U,$J,358.3,2253,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,2253,1,2,0)
 ;;=2^93283
 ;;^UTILITY(U,$J,358.3,2253,1,3,0)
 ;;=3^ICD Device Progr Eval,Dual
 ;;^UTILITY(U,$J,358.3,2254,0)
 ;;=93284^^15^170^11^^^^1
 ;;^UTILITY(U,$J,358.3,2254,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,2254,1,2,0)
 ;;=2^93284
 ;;^UTILITY(U,$J,358.3,2254,1,3,0)
 ;;=3^ICD Device Progr Eval,Multi
 ;;^UTILITY(U,$J,358.3,2255,0)
 ;;=93281^^15^170^33^^^^1
 ;;^UTILITY(U,$J,358.3,2255,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,2255,1,2,0)
 ;;=2^93281
 ;;^UTILITY(U,$J,358.3,2255,1,3,0)
 ;;=3^PM Device Progr Eval,Multi
 ;;^UTILITY(U,$J,358.3,2256,0)
 ;;=33227^^15^170^50^^^^1
 ;;^UTILITY(U,$J,358.3,2256,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,2256,1,2,0)
 ;;=2^33227
 ;;^UTILITY(U,$J,358.3,2256,1,3,0)
 ;;=3^Remove PM Pulse Gen w/Replc PM Gen,Single
 ;;^UTILITY(U,$J,358.3,2257,0)
 ;;=33228^^15^170^48^^^^1
 ;;^UTILITY(U,$J,358.3,2257,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,2257,1,2,0)
 ;;=2^33228
 ;;^UTILITY(U,$J,358.3,2257,1,3,0)
 ;;=3^Remove PM Pulse Gen w/Replc PM Gen,Dual
 ;;^UTILITY(U,$J,358.3,2258,0)
 ;;=33229^^15^170^49^^^^1
 ;;^UTILITY(U,$J,358.3,2258,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,2258,1,2,0)
 ;;=2^33229
 ;;^UTILITY(U,$J,358.3,2258,1,3,0)
 ;;=3^Remove PM Pulse Gen w/Replc PM Gen,Mult
 ;;^UTILITY(U,$J,358.3,2259,0)
 ;;=33230^^15^170^18^^^^1
 ;;^UTILITY(U,$J,358.3,2259,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,2259,1,2,0)
 ;;=2^33230
 ;;^UTILITY(U,$J,358.3,2259,1,3,0)
 ;;=3^Insert ICD Gen Only,Existing Single
 ;;^UTILITY(U,$J,358.3,2260,0)
 ;;=33231^^15^170^17^^^^1
 ;;^UTILITY(U,$J,358.3,2260,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,2260,1,2,0)
 ;;=2^33231
 ;;^UTILITY(U,$J,358.3,2260,1,3,0)
 ;;=3^Insert ICD Gen Only,Existing Mult
 ;;^UTILITY(U,$J,358.3,2261,0)
 ;;=33233^^15^170^44^^^^1
 ;;^UTILITY(U,$J,358.3,2261,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,2261,1,2,0)
 ;;=2^33233
 ;;^UTILITY(U,$J,358.3,2261,1,3,0)
 ;;=3^Removal of PM Generator Only
 ;;^UTILITY(U,$J,358.3,2262,0)
 ;;=33262^^15^170^54^^^^1
 ;;^UTILITY(U,$J,358.3,2262,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,2262,1,2,0)
 ;;=2^33262
 ;;^UTILITY(U,$J,358.3,2262,1,3,0)
 ;;=3^Remv ICD Gen w/Replc PM Gen,Single
 ;;^UTILITY(U,$J,358.3,2263,0)
 ;;=33263^^15^170^52^^^^1
 ;;^UTILITY(U,$J,358.3,2263,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,2263,1,2,0)
 ;;=2^33263
 ;;^UTILITY(U,$J,358.3,2263,1,3,0)
 ;;=3^Remv ICD Gen w/Replc PM Gen,Dual
 ;;^UTILITY(U,$J,358.3,2264,0)
 ;;=33264^^15^170^53^^^^1
 ;;^UTILITY(U,$J,358.3,2264,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,2264,1,2,0)
 ;;=2^33264
 ;;^UTILITY(U,$J,358.3,2264,1,3,0)
 ;;=3^Remv ICD Gen w/Replc PM Gen,Multiple
 ;;^UTILITY(U,$J,358.3,2265,0)
 ;;=93286^^15^170^40^^^^1
 ;;^UTILITY(U,$J,358.3,2265,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,2265,1,2,0)
 ;;=2^93286
 ;;^UTILITY(U,$J,358.3,2265,1,3,0)
 ;;=3^Pre-Op PM Device Eval w/Review&Rpt
 ;;^UTILITY(U,$J,358.3,2266,0)
 ;;=93287^^15^170^39^^^^1
 ;;^UTILITY(U,$J,358.3,2266,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,2266,1,2,0)
 ;;=2^93287
 ;;^UTILITY(U,$J,358.3,2266,1,3,0)
 ;;=3^Pre-Op ICD Device Eval
 ;;^UTILITY(U,$J,358.3,2267,0)
 ;;=93290^^15^170^12^^^^1
 ;;^UTILITY(U,$J,358.3,2267,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,2267,1,2,0)
 ;;=2^93290
 ;;^UTILITY(U,$J,358.3,2267,1,3,0)
 ;;=3^ICM Device Eval
 ;;^UTILITY(U,$J,358.3,2268,0)
 ;;=93293^^15^170^35^^^^1
 ;;^UTILITY(U,$J,358.3,2268,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,2268,1,2,0)
 ;;=2^93293
 ;;^UTILITY(U,$J,358.3,2268,1,3,0)
 ;;=3^PM Phone R-Strip Device Eval
 ;;^UTILITY(U,$J,358.3,2269,0)
 ;;=33223^^15^170^59^^^^1
 ;;^UTILITY(U,$J,358.3,2269,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,2269,1,2,0)
 ;;=2^33223
 ;;^UTILITY(U,$J,358.3,2269,1,3,0)
 ;;=3^Revision Skin Pckt, ICD
 ;;^UTILITY(U,$J,358.3,2270,0)
 ;;=33224^^15^170^25^^^^1
 ;;^UTILITY(U,$J,358.3,2270,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,2270,1,2,0)
 ;;=2^33224
 ;;^UTILITY(U,$J,358.3,2270,1,3,0)
 ;;=3^Insertion of Pacing Electrode
 ;;^UTILITY(U,$J,358.3,2271,0)
 ;;=33214^^15^170^65^^^^1
 ;;^UTILITY(U,$J,358.3,2271,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,2271,1,2,0)
 ;;=2^33214
 ;;^UTILITY(U,$J,358.3,2271,1,3,0)
 ;;=3^Upgrade Sng to Dual PM System
 ;;^UTILITY(U,$J,358.3,2272,0)
 ;;=33215^^15^170^57^^^^1
 ;;^UTILITY(U,$J,358.3,2272,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,2272,1,2,0)
 ;;=2^33215
 ;;^UTILITY(U,$J,358.3,2272,1,3,0)
 ;;=3^Reposition Transvenous PM/ICD Lead
 ;;^UTILITY(U,$J,358.3,2273,0)
 ;;=33221^^15^170^29^^^^1
 ;;^UTILITY(U,$J,358.3,2273,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,2273,1,2,0)
 ;;=2^33221
 ;;^UTILITY(U,$J,358.3,2273,1,3,0)
 ;;=3^New Pacemaker Attached to Old Leads
 ;;^UTILITY(U,$J,358.3,2274,0)
 ;;=33225^^15^170^3^^^^1
 ;;^UTILITY(U,$J,358.3,2274,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,2274,1,2,0)
 ;;=2^33225
 ;;^UTILITY(U,$J,358.3,2274,1,3,0)
 ;;=3^CS Lead Implt at time of New Implt/Upgd
 ;;^UTILITY(U,$J,358.3,2275,0)
 ;;=33284^^15^170^27^^^^1
 ;;^UTILITY(U,$J,358.3,2275,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,2275,1,2,0)
 ;;=2^33284
 ;;^UTILITY(U,$J,358.3,2275,1,3,0)
 ;;=3^Monitor Explant
 ;;^UTILITY(U,$J,358.3,2276,0)
 ;;=33282^^15^170^28^^^^1
 ;;^UTILITY(U,$J,358.3,2276,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,2276,1,2,0)
 ;;=2^33282
 ;;^UTILITY(U,$J,358.3,2276,1,3,0)
 ;;=3^Monitor Implant
 ;;^UTILITY(U,$J,358.3,2277,0)
 ;;=33226^^15^170^4^^^^1
 ;;^UTILITY(U,$J,358.3,2277,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,2277,1,2,0)
 ;;=2^33226
 ;;^UTILITY(U,$J,358.3,2277,1,3,0)
 ;;=3^CS Lead Revision
 ;;^UTILITY(U,$J,358.3,2278,0)
 ;;=92961^^15^170^6^^^^1
 ;;^UTILITY(U,$J,358.3,2278,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,2278,1,2,0)
 ;;=2^92961
 ;;^UTILITY(U,$J,358.3,2278,1,3,0)
 ;;=3^Cardioversion,Internal
 ;;^UTILITY(U,$J,358.3,2279,0)
 ;;=93260^^15^170^41^^^^1
 ;;^UTILITY(U,$J,358.3,2279,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,2279,1,2,0)
 ;;=2^93260
 ;;^UTILITY(U,$J,358.3,2279,1,3,0)
 ;;=3^Prgrmg Dev Eval Impltbl Sys
 ;;^UTILITY(U,$J,358.3,2280,0)
 ;;=93261^^15^170^26^^^^1
 ;;^UTILITY(U,$J,358.3,2280,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,2280,1,2,0)
 ;;=2^93261
 ;;^UTILITY(U,$J,358.3,2280,1,3,0)
 ;;=3^Interrogate Subq Defib
 ;;^UTILITY(U,$J,358.3,2281,0)
 ;;=93298^^15^170^14^^^^1
 ;;^UTILITY(U,$J,358.3,2281,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,2281,1,2,0)
 ;;=2^93298
 ;;^UTILITY(U,$J,358.3,2281,1,3,0)
 ;;=3^ILR Device Interrogat Remote
 ;;^UTILITY(U,$J,358.3,2282,0)
 ;;=93724^^15^170^1^^^^1
 ;;^UTILITY(U,$J,358.3,2282,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,2282,1,2,0)
 ;;=2^93724
 ;;^UTILITY(U,$J,358.3,2282,1,3,0)
 ;;=3^ANALYZE PACEMAKER SYSTEM
 ;;^UTILITY(U,$J,358.3,2283,0)
 ;;=33967^^15^170^16^^^^1
 ;;^UTILITY(U,$J,358.3,2283,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,2283,1,2,0)
 ;;=2^33967
 ;;^UTILITY(U,$J,358.3,2283,1,3,0)
 ;;=3^Insert IA Percut Device
 ;;^UTILITY(U,$J,358.3,2284,0)
 ;;=33236^^15^170^46^^^^1
 ;;^UTILITY(U,$J,358.3,2284,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,2284,1,2,0)
 ;;=2^33236
 ;;^UTILITY(U,$J,358.3,2284,1,3,0)
 ;;=3^Remove Epi Electrode/Thoracotomy
 ;;^UTILITY(U,$J,358.3,2285,0)
 ;;=33237^^15^170^45^^^^1
 ;;^UTILITY(U,$J,358.3,2285,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,2285,1,2,0)
 ;;=2^33237
 ;;^UTILITY(U,$J,358.3,2285,1,3,0)
 ;;=3^Remove Electrode/Thoracotomy Dual
 ;;^UTILITY(U,$J,358.3,2286,0)
 ;;=33249^^15^170^47^^^^1
 ;;^UTILITY(U,$J,358.3,2286,1,0)
 ;;=^358.31IA^3^2
