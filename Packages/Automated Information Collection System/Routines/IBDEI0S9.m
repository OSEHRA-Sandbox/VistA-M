IBDEI0S9 ; ; 19-NOV-2015
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;JUN 29, 2015
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,13001,1,4,0)
 ;;=4^H10.502
 ;;^UTILITY(U,$J,358.3,13001,2)
 ;;=^5004698
 ;;^UTILITY(U,$J,358.3,13002,0)
 ;;=H11.011^^80^753^4
 ;;^UTILITY(U,$J,358.3,13002,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,13002,1,3,0)
 ;;=3^Amyloid Pterygium,Right Eye
 ;;^UTILITY(U,$J,358.3,13002,1,4,0)
 ;;=4^H11.011
 ;;^UTILITY(U,$J,358.3,13002,2)
 ;;=^5004720
 ;;^UTILITY(U,$J,358.3,13003,0)
 ;;=H11.012^^80^753^3
 ;;^UTILITY(U,$J,358.3,13003,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,13003,1,3,0)
 ;;=3^Amyloid Pterygium,Left Eye
 ;;^UTILITY(U,$J,358.3,13003,1,4,0)
 ;;=4^H11.012
 ;;^UTILITY(U,$J,358.3,13003,2)
 ;;=^5004721
 ;;^UTILITY(U,$J,358.3,13004,0)
 ;;=H11.002^^80^753^140
 ;;^UTILITY(U,$J,358.3,13004,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,13004,1,3,0)
 ;;=3^Pterygium,Left Eye,Unspec
 ;;^UTILITY(U,$J,358.3,13004,1,4,0)
 ;;=4^H11.002
 ;;^UTILITY(U,$J,358.3,13004,2)
 ;;=^5004718
 ;;^UTILITY(U,$J,358.3,13005,0)
 ;;=H11.001^^80^753^141
 ;;^UTILITY(U,$J,358.3,13005,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,13005,1,3,0)
 ;;=3^Pterygium,Right Eye,Unspec
 ;;^UTILITY(U,$J,358.3,13005,1,4,0)
 ;;=4^H11.001
 ;;^UTILITY(U,$J,358.3,13005,2)
 ;;=^5004717
 ;;^UTILITY(U,$J,358.3,13006,0)
 ;;=H11.151^^80^753^132
 ;;^UTILITY(U,$J,358.3,13006,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,13006,1,3,0)
 ;;=3^Pinguecula,Right Eye
 ;;^UTILITY(U,$J,358.3,13006,1,4,0)
 ;;=4^H11.151
 ;;^UTILITY(U,$J,358.3,13006,2)
 ;;=^5004761
 ;;^UTILITY(U,$J,358.3,13007,0)
 ;;=H11.152^^80^753^131
 ;;^UTILITY(U,$J,358.3,13007,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,13007,1,3,0)
 ;;=3^Pinguecula,Left Eye
 ;;^UTILITY(U,$J,358.3,13007,1,4,0)
 ;;=4^H11.152
 ;;^UTILITY(U,$J,358.3,13007,2)
 ;;=^5004762
 ;;^UTILITY(U,$J,358.3,13008,0)
 ;;=H11.121^^80^753^34
 ;;^UTILITY(U,$J,358.3,13008,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,13008,1,3,0)
 ;;=3^Conjunctival Concretions,Right Eye
 ;;^UTILITY(U,$J,358.3,13008,1,4,0)
 ;;=4^H11.121
 ;;^UTILITY(U,$J,358.3,13008,2)
 ;;=^5004749
 ;;^UTILITY(U,$J,358.3,13009,0)
 ;;=H11.122^^80^753^33
 ;;^UTILITY(U,$J,358.3,13009,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,13009,1,3,0)
 ;;=3^Conjunctival Concretions,Left Eye
 ;;^UTILITY(U,$J,358.3,13009,1,4,0)
 ;;=4^H11.122
 ;;^UTILITY(U,$J,358.3,13009,2)
 ;;=^5004750
 ;;^UTILITY(U,$J,358.3,13010,0)
 ;;=H11.231^^80^753^156
 ;;^UTILITY(U,$J,358.3,13010,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,13010,1,3,0)
 ;;=3^Symblepharon,Right Eye
 ;;^UTILITY(U,$J,358.3,13010,1,4,0)
 ;;=4^H11.231
 ;;^UTILITY(U,$J,358.3,13010,2)
 ;;=^5004773
 ;;^UTILITY(U,$J,358.3,13011,0)
 ;;=H11.232^^80^753^155
 ;;^UTILITY(U,$J,358.3,13011,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,13011,1,3,0)
 ;;=3^Symblepharon,Left Eye
 ;;^UTILITY(U,$J,358.3,13011,1,4,0)
 ;;=4^H11.232
 ;;^UTILITY(U,$J,358.3,13011,2)
 ;;=^5004774
 ;;^UTILITY(U,$J,358.3,13012,0)
 ;;=H11.31^^80^753^40
 ;;^UTILITY(U,$J,358.3,13012,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,13012,1,3,0)
 ;;=3^Conjunctival Hemorrhage,Right Eye
 ;;^UTILITY(U,$J,358.3,13012,1,4,0)
 ;;=4^H11.31
 ;;^UTILITY(U,$J,358.3,13012,2)
 ;;=^5004782
 ;;^UTILITY(U,$J,358.3,13013,0)
 ;;=H11.32^^80^753^39
 ;;^UTILITY(U,$J,358.3,13013,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,13013,1,3,0)
 ;;=3^Conjunctival Hemorrhage,Left Eye
 ;;^UTILITY(U,$J,358.3,13013,1,4,0)
 ;;=4^H11.32
 ;;^UTILITY(U,$J,358.3,13013,2)
 ;;=^5004783
 ;;^UTILITY(U,$J,358.3,13014,0)
 ;;=H11.441^^80^753^37
 ;;^UTILITY(U,$J,358.3,13014,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,13014,1,3,0)
 ;;=3^Conjunctival Cysts,Right Eye
 ;;^UTILITY(U,$J,358.3,13014,1,4,0)
 ;;=4^H11.441
