IBDEI158 ; ; 19-NOV-2015
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;JUN 29, 2015
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,19096,2)
 ;;=^5063616
 ;;^UTILITY(U,$J,358.3,19097,0)
 ;;=Z91.19^^94^917^70
 ;;^UTILITY(U,$J,358.3,19097,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,19097,1,3,0)
 ;;=3^Noncompliance w/ Medical Treatment & Regimen
 ;;^UTILITY(U,$J,358.3,19097,1,4,0)
 ;;=4^Z91.19
 ;;^UTILITY(U,$J,358.3,19097,2)
 ;;=^5063618
 ;;^UTILITY(U,$J,358.3,19098,0)
 ;;=Z93.1^^94^917^52
 ;;^UTILITY(U,$J,358.3,19098,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,19098,1,3,0)
 ;;=3^Gastrostomy Status
 ;;^UTILITY(U,$J,358.3,19098,1,4,0)
 ;;=4^Z93.1
 ;;^UTILITY(U,$J,358.3,19098,2)
 ;;=^5063643
 ;;^UTILITY(U,$J,358.3,19099,0)
 ;;=Z93.2^^94^917^58
 ;;^UTILITY(U,$J,358.3,19099,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,19099,1,3,0)
 ;;=3^Ileostomy Status
 ;;^UTILITY(U,$J,358.3,19099,1,4,0)
 ;;=4^Z93.2
 ;;^UTILITY(U,$J,358.3,19099,2)
 ;;=^5063644
 ;;^UTILITY(U,$J,358.3,19100,0)
 ;;=Z93.3^^94^917^15
 ;;^UTILITY(U,$J,358.3,19100,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,19100,1,3,0)
 ;;=3^Colostomy Status
 ;;^UTILITY(U,$J,358.3,19100,1,4,0)
 ;;=4^Z93.3
 ;;^UTILITY(U,$J,358.3,19100,2)
 ;;=^5063645
 ;;^UTILITY(U,$J,358.3,19101,0)
 ;;=Z94.0^^94^917^63
 ;;^UTILITY(U,$J,358.3,19101,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,19101,1,3,0)
 ;;=3^Kidney Transplant Status
 ;;^UTILITY(U,$J,358.3,19101,1,4,0)
 ;;=4^Z94.0
 ;;^UTILITY(U,$J,358.3,19101,2)
 ;;=^5063654
 ;;^UTILITY(U,$J,358.3,19102,0)
 ;;=Z94.1^^94^917^56
 ;;^UTILITY(U,$J,358.3,19102,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,19102,1,3,0)
 ;;=3^Heart Transplant Status
 ;;^UTILITY(U,$J,358.3,19102,1,4,0)
 ;;=4^Z94.1
 ;;^UTILITY(U,$J,358.3,19102,2)
 ;;=^5063655
 ;;^UTILITY(U,$J,358.3,19103,0)
 ;;=Z94.2^^94^917^66
 ;;^UTILITY(U,$J,358.3,19103,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,19103,1,3,0)
 ;;=3^Lung Transplant Status
 ;;^UTILITY(U,$J,358.3,19103,1,4,0)
 ;;=4^Z94.2
 ;;^UTILITY(U,$J,358.3,19103,2)
 ;;=^5063656
 ;;^UTILITY(U,$J,358.3,19104,0)
 ;;=Z94.3^^94^917^55
 ;;^UTILITY(U,$J,358.3,19104,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,19104,1,3,0)
 ;;=3^Heart & Lungs Transplant Status
 ;;^UTILITY(U,$J,358.3,19104,1,4,0)
 ;;=4^Z94.3
 ;;^UTILITY(U,$J,358.3,19104,2)
 ;;=^5063657
 ;;^UTILITY(U,$J,358.3,19105,0)
 ;;=Z94.4^^94^917^65
 ;;^UTILITY(U,$J,358.3,19105,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,19105,1,3,0)
 ;;=3^Liver Transplant Status
 ;;^UTILITY(U,$J,358.3,19105,1,4,0)
 ;;=4^Z94.4
 ;;^UTILITY(U,$J,358.3,19105,2)
 ;;=^5063658
 ;;^UTILITY(U,$J,358.3,19106,0)
 ;;=Z94.84^^94^917^143
 ;;^UTILITY(U,$J,358.3,19106,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,19106,1,3,0)
 ;;=3^Stem Cell Transplant Status
 ;;^UTILITY(U,$J,358.3,19106,1,4,0)
 ;;=4^Z94.84
 ;;^UTILITY(U,$J,358.3,19106,2)
 ;;=^5063665
 ;;^UTILITY(U,$J,358.3,19107,0)
 ;;=Z96.21^^94^917^14
 ;;^UTILITY(U,$J,358.3,19107,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,19107,1,3,0)
 ;;=3^Cochlear Implant Status
 ;;^UTILITY(U,$J,358.3,19107,1,4,0)
 ;;=4^Z96.21
 ;;^UTILITY(U,$J,358.3,19107,2)
 ;;=^5063684
 ;;^UTILITY(U,$J,358.3,19108,0)
 ;;=Z96.41^^94^917^126
 ;;^UTILITY(U,$J,358.3,19108,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,19108,1,3,0)
 ;;=3^Presence of Insulin Pump
 ;;^UTILITY(U,$J,358.3,19108,1,4,0)
 ;;=4^Z96.41
 ;;^UTILITY(U,$J,358.3,19108,2)
 ;;=^5063688
 ;;^UTILITY(U,$J,358.3,19109,0)
 ;;=Z96.641^^94^917^134
 ;;^UTILITY(U,$J,358.3,19109,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,19109,1,3,0)
 ;;=3^Presence of Right Artificial Hip Jt
 ;;^UTILITY(U,$J,358.3,19109,1,4,0)
 ;;=4^Z96.641
 ;;^UTILITY(U,$J,358.3,19109,2)
 ;;=^5063701
 ;;^UTILITY(U,$J,358.3,19110,0)
 ;;=Z96.642^^94^917^129
