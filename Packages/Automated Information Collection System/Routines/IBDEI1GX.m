IBDEI1GX ; ; 19-NOV-2015
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;JUN 29, 2015
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,24538,1,4,0)
 ;;=4^J13.
 ;;^UTILITY(U,$J,358.3,24538,2)
 ;;=^5008170
 ;;^UTILITY(U,$J,358.3,24539,0)
 ;;=J15.7^^121^1221^179
 ;;^UTILITY(U,$J,358.3,24539,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,24539,1,3,0)
 ;;=3^Pneumonia, Mycoplasma
 ;;^UTILITY(U,$J,358.3,24539,1,4,0)
 ;;=4^J15.7
 ;;^UTILITY(U,$J,358.3,24539,2)
 ;;=^5008177
 ;;^UTILITY(U,$J,358.3,24540,0)
 ;;=J16.0^^121^1221^178
 ;;^UTILITY(U,$J,358.3,24540,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,24540,1,3,0)
 ;;=3^Pneumonia, Chlamydia/Chlamydophila
 ;;^UTILITY(U,$J,358.3,24540,1,4,0)
 ;;=4^J16.0
 ;;^UTILITY(U,$J,358.3,24540,2)
 ;;=^5008179
 ;;^UTILITY(U,$J,358.3,24541,0)
 ;;=R76.11^^121^1221^184
 ;;^UTILITY(U,$J,358.3,24541,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,24541,1,3,0)
 ;;=3^Positive PPD/Tuberculin skin test
 ;;^UTILITY(U,$J,358.3,24541,1,4,0)
 ;;=4^R76.11
 ;;^UTILITY(U,$J,358.3,24541,2)
 ;;=^5019570
 ;;^UTILITY(U,$J,358.3,24542,0)
 ;;=R76.12^^121^1221^183
 ;;^UTILITY(U,$J,358.3,24542,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,24542,1,3,0)
 ;;=3^Positive IGRA test for tuberculosis
 ;;^UTILITY(U,$J,358.3,24542,1,4,0)
 ;;=4^R76.12
 ;;^UTILITY(U,$J,358.3,24542,2)
 ;;=^5019571
 ;;^UTILITY(U,$J,358.3,24543,0)
 ;;=R76.8^^121^1221^185
 ;;^UTILITY(U,$J,358.3,24543,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,24543,1,3,0)
 ;;=3^Positive RPR
 ;;^UTILITY(U,$J,358.3,24543,1,4,0)
 ;;=4^R76.8
 ;;^UTILITY(U,$J,358.3,24543,2)
 ;;=^5019572
 ;;^UTILITY(U,$J,358.3,24544,0)
 ;;=N41.9^^121^1221^186
 ;;^UTILITY(U,$J,358.3,24544,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,24544,1,3,0)
 ;;=3^Prostatitis
 ;;^UTILITY(U,$J,358.3,24544,1,4,0)
 ;;=4^N41.9
 ;;^UTILITY(U,$J,358.3,24544,2)
 ;;=^5015694
 ;;^UTILITY(U,$J,358.3,24545,0)
 ;;=T84.52XA^^121^1221^188
 ;;^UTILITY(U,$J,358.3,24545,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,24545,1,3,0)
 ;;=3^Prosthetic Left Hip Joint Infection,Init Encntr
 ;;^UTILITY(U,$J,358.3,24545,1,4,0)
 ;;=4^T84.52XA
 ;;^UTILITY(U,$J,358.3,24545,2)
 ;;=^5055388
 ;;^UTILITY(U,$J,358.3,24546,0)
 ;;=T84.51XA^^121^1221^190
 ;;^UTILITY(U,$J,358.3,24546,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,24546,1,3,0)
 ;;=3^Prosthetic Right Hip Joint Infection,Init Encntr
 ;;^UTILITY(U,$J,358.3,24546,1,4,0)
 ;;=4^T84.51XA
 ;;^UTILITY(U,$J,358.3,24546,2)
 ;;=^5055385
 ;;^UTILITY(U,$J,358.3,24547,0)
 ;;=T84.54XA^^121^1221^189
 ;;^UTILITY(U,$J,358.3,24547,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,24547,1,3,0)
 ;;=3^Prosthetic Left Knee Joint Infection,Init Encntr
 ;;^UTILITY(U,$J,358.3,24547,1,4,0)
 ;;=4^T84.54XA
 ;;^UTILITY(U,$J,358.3,24547,2)
 ;;=^5055394
 ;;^UTILITY(U,$J,358.3,24548,0)
 ;;=T84.53XA^^121^1221^191
 ;;^UTILITY(U,$J,358.3,24548,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,24548,1,3,0)
 ;;=3^Prosthetic Right Knee Joint Infection,Init Encntr
 ;;^UTILITY(U,$J,358.3,24548,1,4,0)
 ;;=4^T84.53XA
 ;;^UTILITY(U,$J,358.3,24548,2)
 ;;=^5055391
 ;;^UTILITY(U,$J,358.3,24549,0)
 ;;=T84.59XA^^121^1221^192
 ;;^UTILITY(U,$J,358.3,24549,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,24549,1,3,0)
 ;;=3^Prosthetic Shoulder Joint Infection,Init Encntr
 ;;^UTILITY(U,$J,358.3,24549,1,4,0)
 ;;=4^T84.59XA
 ;;^UTILITY(U,$J,358.3,24549,2)
 ;;=^5055397
 ;;^UTILITY(U,$J,358.3,24550,0)
 ;;=T82.6XXA^^121^1221^187
 ;;^UTILITY(U,$J,358.3,24550,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,24550,1,3,0)
 ;;=3^Prosthetic Heart Valve Infection, initial encounter
 ;;^UTILITY(U,$J,358.3,24550,1,4,0)
 ;;=4^T82.6XXA
 ;;^UTILITY(U,$J,358.3,24550,2)
 ;;=^5054908
 ;;^UTILITY(U,$J,358.3,24551,0)
 ;;=N12.^^121^1221^195
 ;;^UTILITY(U,$J,358.3,24551,1,0)
 ;;=^358.31IA^4^2
