IBDEI22W ; ; 19-NOV-2015
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;JUN 29, 2015
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,34867,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,34867,1,2,0)
 ;;=2^90471
 ;;^UTILITY(U,$J,358.3,34867,1,3,0)
 ;;=3^IMMUNIZATION ADMIN,1 INJECTION
 ;;^UTILITY(U,$J,358.3,34868,0)
 ;;=90472^^162^1775^2^^^^1
 ;;^UTILITY(U,$J,358.3,34868,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,34868,1,2,0)
 ;;=2^90472
 ;;^UTILITY(U,$J,358.3,34868,1,3,0)
 ;;=3^IMMUN ADMIN, EACH ADDITIONAL
 ;;^UTILITY(U,$J,358.3,34869,0)
 ;;=51702^^162^1776^1^^^^1
 ;;^UTILITY(U,$J,358.3,34869,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,34869,1,2,0)
 ;;=2^51702
 ;;^UTILITY(U,$J,358.3,34869,1,3,0)
 ;;=3^FOLEY CATHETER/SIMPLE
 ;;^UTILITY(U,$J,358.3,34870,0)
 ;;=51703^^162^1776^2^^^^1
 ;;^UTILITY(U,$J,358.3,34870,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,34870,1,2,0)
 ;;=2^51703
 ;;^UTILITY(U,$J,358.3,34870,1,3,0)
 ;;=3^FOLEY CATHETER/COMPLICATED
 ;;^UTILITY(U,$J,358.3,34871,0)
 ;;=51701^^162^1776^3^^^^1
 ;;^UTILITY(U,$J,358.3,34871,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,34871,1,2,0)
 ;;=2^51701
 ;;^UTILITY(U,$J,358.3,34871,1,3,0)
 ;;=3^INSERT STRAIGHT CATH
 ;;^UTILITY(U,$J,358.3,34872,0)
 ;;=J1645^^162^1777^4^^^^1
 ;;^UTILITY(U,$J,358.3,34872,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,34872,1,2,0)
 ;;=2^J1645
 ;;^UTILITY(U,$J,358.3,34872,1,3,0)
 ;;=3^HEPARIN (FOR FLUSHING)
 ;;^UTILITY(U,$J,358.3,34873,0)
 ;;=36593^^162^1777^3^^^^1
 ;;^UTILITY(U,$J,358.3,34873,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,34873,1,2,0)
 ;;=2^36593
 ;;^UTILITY(U,$J,358.3,34873,1,3,0)
 ;;=3^DECLOT VASCULAR DEVICE
 ;;^UTILITY(U,$J,358.3,34874,0)
 ;;=96523^^162^1777^2^^^^1
 ;;^UTILITY(U,$J,358.3,34874,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,34874,1,2,0)
 ;;=2^96523
 ;;^UTILITY(U,$J,358.3,34874,1,3,0)
 ;;=3^CATH FLUSH
 ;;^UTILITY(U,$J,358.3,34875,0)
 ;;=J1642^^162^1777^5^^^^1
 ;;^UTILITY(U,$J,358.3,34875,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,34875,1,2,0)
 ;;=2^J1642
 ;;^UTILITY(U,$J,358.3,34875,1,3,0)
 ;;=3^HEPARIN SODIUM per 10 Units
 ;;^UTILITY(U,$J,358.3,34876,0)
 ;;=J7042^^162^1777^1^^^^1
 ;;^UTILITY(U,$J,358.3,34876,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,34876,1,2,0)
 ;;=2^J7042
 ;;^UTILITY(U,$J,358.3,34876,1,3,0)
 ;;=3^5% DEXTROSE/NORMAL SALINE
 ;;^UTILITY(U,$J,358.3,34877,0)
 ;;=94150^^162^1778^1^^^^1
 ;;^UTILITY(U,$J,358.3,34877,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,34877,1,2,0)
 ;;=2^94150
 ;;^UTILITY(U,$J,358.3,34877,1,3,0)
 ;;=3^PFT TEST
 ;;^UTILITY(U,$J,358.3,34878,0)
 ;;=93005^^162^1779^1^^^^1
 ;;^UTILITY(U,$J,358.3,34878,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,34878,1,2,0)
 ;;=2^93005
 ;;^UTILITY(U,$J,358.3,34878,1,3,0)
 ;;=3^EKG, TRACING ONLY
 ;;^UTILITY(U,$J,358.3,34879,0)
 ;;=93225^^162^1779^2^^^^1
 ;;^UTILITY(U,$J,358.3,34879,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,34879,1,2,0)
 ;;=2^93225
 ;;^UTILITY(U,$J,358.3,34879,1,3,0)
 ;;=3^HOLTER MONITOR PLACEMENT/REMVL
 ;;^UTILITY(U,$J,358.3,34880,0)
 ;;=90732^^162^1780^30^^^^1
 ;;^UTILITY(U,$J,358.3,34880,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,34880,1,2,0)
 ;;=2^90732
 ;;^UTILITY(U,$J,358.3,34880,1,3,0)
 ;;=3^PNEUMOCOCCAL VACCINE
 ;;^UTILITY(U,$J,358.3,34881,0)
 ;;=90658^^162^1780^11^^^^1
 ;;^UTILITY(U,$J,358.3,34881,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,34881,1,2,0)
 ;;=2^90658
 ;;^UTILITY(U,$J,358.3,34881,1,3,0)
 ;;=3^FLU VACCINE MULTI DOSE VIAL (AFLURIA)
 ;;^UTILITY(U,$J,358.3,34882,0)
 ;;=90585^^162^1780^1^^^^1
 ;;^UTILITY(U,$J,358.3,34882,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,34882,1,2,0)
 ;;=2^90585
 ;;^UTILITY(U,$J,358.3,34882,1,3,0)
 ;;=3^BCG VACCINE, PERCUT
 ;;^UTILITY(U,$J,358.3,34883,0)
 ;;=90632^^162^1780^14^^^^1
