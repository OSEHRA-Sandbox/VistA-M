IBDEI19J ; ; 17-FEB-2016
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;JUN 29, 2015
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,21471,1,2,0)
 ;;=2^97602
 ;;^UTILITY(U,$J,358.3,21471,1,3,0)
 ;;=3^Wound Care, non-selective debridement
 ;;^UTILITY(U,$J,358.3,21472,0)
 ;;=51701^^86^957^10^^^^1
 ;;^UTILITY(U,$J,358.3,21472,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,21472,1,2,0)
 ;;=2^51701
 ;;^UTILITY(U,$J,358.3,21472,1,3,0)
 ;;=3^Insert Catheter for Residual Urine
 ;;^UTILITY(U,$J,358.3,21473,0)
 ;;=51702^^86^957^11^^^^1
 ;;^UTILITY(U,$J,358.3,21473,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,21473,1,2,0)
 ;;=2^51702
 ;;^UTILITY(U,$J,358.3,21473,1,3,0)
 ;;=3^Insert Foley Cath
 ;;^UTILITY(U,$J,358.3,21474,0)
 ;;=51798^^86^957^16^^^^1
 ;;^UTILITY(U,$J,358.3,21474,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,21474,1,2,0)
 ;;=2^51798
 ;;^UTILITY(U,$J,358.3,21474,1,3,0)
 ;;=3^Ultrasound for Resid Urine
 ;;^UTILITY(U,$J,358.3,21475,0)
 ;;=96360^^86^957^7^^^^1
 ;;^UTILITY(U,$J,358.3,21475,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,21475,1,2,0)
 ;;=2^96360
 ;;^UTILITY(U,$J,358.3,21475,1,3,0)
 ;;=3^Hydration,IV,first hour
 ;;^UTILITY(U,$J,358.3,21476,0)
 ;;=96361^^86^957^6^^^^1
 ;;^UTILITY(U,$J,358.3,21476,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,21476,1,2,0)
 ;;=2^96361
 ;;^UTILITY(U,$J,358.3,21476,1,3,0)
 ;;=3^Hydration,IV,e addl hour
 ;;^UTILITY(U,$J,358.3,21477,0)
 ;;=96365^^86^957^9^^^^1
 ;;^UTILITY(U,$J,358.3,21477,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,21477,1,2,0)
 ;;=2^96365
 ;;^UTILITY(U,$J,358.3,21477,1,3,0)
 ;;=3^Infusion,IV up to 1 hour
 ;;^UTILITY(U,$J,358.3,21478,0)
 ;;=96366^^86^957^8^^^^1
 ;;^UTILITY(U,$J,358.3,21478,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,21478,1,2,0)
 ;;=2^96366
 ;;^UTILITY(U,$J,358.3,21478,1,3,0)
 ;;=3^Infusion,IV e addl hour
 ;;^UTILITY(U,$J,358.3,21479,0)
 ;;=94664^^86^957^4^^^^1
 ;;^UTILITY(U,$J,358.3,21479,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,21479,1,2,0)
 ;;=2^94664
 ;;^UTILITY(U,$J,358.3,21479,1,3,0)
 ;;=3^Evaluate Pt Use of Inhaler
 ;;^UTILITY(U,$J,358.3,21480,0)
 ;;=99195^^86^957^13^^^^1
 ;;^UTILITY(U,$J,358.3,21480,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,21480,1,2,0)
 ;;=2^99195
 ;;^UTILITY(U,$J,358.3,21480,1,3,0)
 ;;=3^Phlebotomy
 ;;^UTILITY(U,$J,358.3,21481,0)
 ;;=94760^^86^957^2^^^^1
 ;;^UTILITY(U,$J,358.3,21481,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,21481,1,2,0)
 ;;=2^94760
 ;;^UTILITY(U,$J,358.3,21481,1,3,0)
 ;;=3^Blood Oxygen Level
 ;;^UTILITY(U,$J,358.3,21482,0)
 ;;=17000^^86^958^10^^^^1
 ;;^UTILITY(U,$J,358.3,21482,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,21482,1,2,0)
 ;;=2^17000
 ;;^UTILITY(U,$J,358.3,21482,1,3,0)
 ;;=3^Destr ben les, any method, 1st les
 ;;^UTILITY(U,$J,358.3,21483,0)
 ;;=17003^^86^958^11^^^^1
 ;;^UTILITY(U,$J,358.3,21483,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,21483,1,2,0)
 ;;=2^17003
 ;;^UTILITY(U,$J,358.3,21483,1,3,0)
 ;;=3^Destr ben les, any method, addt'l les, ea (use with 17000)
 ;;^UTILITY(U,$J,358.3,21484,0)
 ;;=17004^^86^958^12^^^^1
 ;;^UTILITY(U,$J,358.3,21484,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,21484,1,2,0)
 ;;=2^17004
 ;;^UTILITY(U,$J,358.3,21484,1,3,0)
 ;;=3^Destr ben les, over 15
 ;;^UTILITY(U,$J,358.3,21485,0)
 ;;=10060^^86^958^18^^^^1
 ;;^UTILITY(U,$J,358.3,21485,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,21485,1,2,0)
 ;;=2^10060
 ;;^UTILITY(U,$J,358.3,21485,1,3,0)
 ;;=3^I&D abscess
 ;;^UTILITY(U,$J,358.3,21486,0)
 ;;=10061^^86^958^17^^^^1
 ;;^UTILITY(U,$J,358.3,21486,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,21486,1,2,0)
 ;;=2^10061
 ;;^UTILITY(U,$J,358.3,21486,1,3,0)
 ;;=3^I&D Complicated Abscess
 ;;^UTILITY(U,$J,358.3,21487,0)
 ;;=11042^^86^958^9^^^^1
