IBDEI1T8 ; ; 19-NOV-2015
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;JUN 29, 2015
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,30302,0)
 ;;=N73.5^^135^1374^64
 ;;^UTILITY(U,$J,358.3,30302,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,30302,1,3,0)
 ;;=3^Peritonitis Female Pelvic,Unspec
 ;;^UTILITY(U,$J,358.3,30302,1,4,0)
 ;;=4^N73.5
 ;;^UTILITY(U,$J,358.3,30302,2)
 ;;=^5015817
 ;;^UTILITY(U,$J,358.3,30303,0)
 ;;=N72.^^135^1374^35
 ;;^UTILITY(U,$J,358.3,30303,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,30303,1,3,0)
 ;;=3^Inflammatory Disease Cervix Uteri
 ;;^UTILITY(U,$J,358.3,30303,1,4,0)
 ;;=4^N72.
 ;;^UTILITY(U,$J,358.3,30303,2)
 ;;=^5015812
 ;;^UTILITY(U,$J,358.3,30304,0)
 ;;=N76.3^^135^1374^92
 ;;^UTILITY(U,$J,358.3,30304,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,30304,1,3,0)
 ;;=3^Vulvitis,Subacute & Chronic
 ;;^UTILITY(U,$J,358.3,30304,1,4,0)
 ;;=4^N76.3
 ;;^UTILITY(U,$J,358.3,30304,2)
 ;;=^5015829
 ;;^UTILITY(U,$J,358.3,30305,0)
 ;;=N76.1^^135^1374^88
 ;;^UTILITY(U,$J,358.3,30305,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,30305,1,3,0)
 ;;=3^Vaginitis,Subacute & Chronic
 ;;^UTILITY(U,$J,358.3,30305,1,4,0)
 ;;=4^N76.1
 ;;^UTILITY(U,$J,358.3,30305,2)
 ;;=^5015827
 ;;^UTILITY(U,$J,358.3,30306,0)
 ;;=N76.2^^135^1374^91
 ;;^UTILITY(U,$J,358.3,30306,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,30306,1,3,0)
 ;;=3^Vulvitis,Acute
 ;;^UTILITY(U,$J,358.3,30306,1,4,0)
 ;;=4^N76.2
 ;;^UTILITY(U,$J,358.3,30306,2)
 ;;=^5015828
 ;;^UTILITY(U,$J,358.3,30307,0)
 ;;=N76.0^^135^1374^87
 ;;^UTILITY(U,$J,358.3,30307,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,30307,1,3,0)
 ;;=3^Vaginitis,Acute
 ;;^UTILITY(U,$J,358.3,30307,1,4,0)
 ;;=4^N76.0
 ;;^UTILITY(U,$J,358.3,30307,2)
 ;;=^5015826
 ;;^UTILITY(U,$J,358.3,30308,0)
 ;;=M00.20^^135^1374^3
 ;;^UTILITY(U,$J,358.3,30308,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,30308,1,3,0)
 ;;=3^Arthritis Streptococcal,Unspec Joint
 ;;^UTILITY(U,$J,358.3,30308,1,4,0)
 ;;=4^M00.20
 ;;^UTILITY(U,$J,358.3,30308,2)
 ;;=^5009645
 ;;^UTILITY(U,$J,358.3,30309,0)
 ;;=M00.80^^135^1374^4
 ;;^UTILITY(U,$J,358.3,30309,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,30309,1,3,0)
 ;;=3^Arthritis d/t Bacteria,Unspec Joint
 ;;^UTILITY(U,$J,358.3,30309,1,4,0)
 ;;=4^M00.80
 ;;^UTILITY(U,$J,358.3,30309,2)
 ;;=^5009669
 ;;^UTILITY(U,$J,358.3,30310,0)
 ;;=M00.9^^135^1374^72
 ;;^UTILITY(U,$J,358.3,30310,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,30310,1,3,0)
 ;;=3^Pyogenic Arthritis,Unspec
 ;;^UTILITY(U,$J,358.3,30310,1,4,0)
 ;;=4^M00.9
 ;;^UTILITY(U,$J,358.3,30310,2)
 ;;=^5009693
 ;;^UTILITY(U,$J,358.3,30311,0)
 ;;=M00.00^^135^1374^78
 ;;^UTILITY(U,$J,358.3,30311,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,30311,1,3,0)
 ;;=3^Staphylococcal Arthritis,Unspec Joint
 ;;^UTILITY(U,$J,358.3,30311,1,4,0)
 ;;=4^M00.00
 ;;^UTILITY(U,$J,358.3,30311,2)
 ;;=^5009597
 ;;^UTILITY(U,$J,358.3,30312,0)
 ;;=M00.10^^135^1374^65
 ;;^UTILITY(U,$J,358.3,30312,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,30312,1,3,0)
 ;;=3^Pneumococcal Arthritis,Unspec Joint
 ;;^UTILITY(U,$J,358.3,30312,1,4,0)
 ;;=4^M00.10
 ;;^UTILITY(U,$J,358.3,30312,2)
 ;;=^5009621
 ;;^UTILITY(U,$J,358.3,30313,0)
 ;;=M86.20^^135^1374^62
 ;;^UTILITY(U,$J,358.3,30313,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,30313,1,3,0)
 ;;=3^Osteomyelitis,Subacute,Unspec Site
 ;;^UTILITY(U,$J,358.3,30313,1,4,0)
 ;;=4^M86.20
 ;;^UTILITY(U,$J,358.3,30313,2)
 ;;=^5014535
 ;;^UTILITY(U,$J,358.3,30314,0)
 ;;=M86.10^^135^1374^58
 ;;^UTILITY(U,$J,358.3,30314,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,30314,1,3,0)
 ;;=3^Osteomyelitis,Acute,Unspec Site
 ;;^UTILITY(U,$J,358.3,30314,1,4,0)
 ;;=4^M86.10
 ;;^UTILITY(U,$J,358.3,30314,2)
 ;;=^5014521
 ;;^UTILITY(U,$J,358.3,30315,0)
 ;;=M86.00^^135^1374^57
