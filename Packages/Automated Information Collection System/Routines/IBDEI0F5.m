IBDEI0F5 ; ; 19-NOV-2015
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;JUN 29, 2015
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,6659,1,3,0)
 ;;=3^Dermabrasion,Total Face
 ;;^UTILITY(U,$J,358.3,6660,0)
 ;;=15781^^45^433^2^^^^1
 ;;^UTILITY(U,$J,358.3,6660,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,6660,1,2,0)
 ;;=2^15781
 ;;^UTILITY(U,$J,358.3,6660,1,3,0)
 ;;=3^Dermabrasion,Segmental,Face
 ;;^UTILITY(U,$J,358.3,6661,0)
 ;;=15782^^45^433^1^^^^1
 ;;^UTILITY(U,$J,358.3,6661,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,6661,1,2,0)
 ;;=2^15782
 ;;^UTILITY(U,$J,358.3,6661,1,3,0)
 ;;=3^Dermabrasion,Regional,Not Face
 ;;^UTILITY(U,$J,358.3,6662,0)
 ;;=15783^^45^433^5^^^^1
 ;;^UTILITY(U,$J,358.3,6662,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,6662,1,2,0)
 ;;=2^15783
 ;;^UTILITY(U,$J,358.3,6662,1,3,0)
 ;;=3^Dermabrasion,Superficial
 ;;^UTILITY(U,$J,358.3,6663,0)
 ;;=15786^^45^433^3^^^^1
 ;;^UTILITY(U,$J,358.3,6663,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,6663,1,2,0)
 ;;=2^15786
 ;;^UTILITY(U,$J,358.3,6663,1,3,0)
 ;;=3^Dermabrasion,Single Lesion
 ;;^UTILITY(U,$J,358.3,6664,0)
 ;;=15787^^45^433^4^^^^1
 ;;^UTILITY(U,$J,358.3,6664,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,6664,1,2,0)
 ;;=2^15787
 ;;^UTILITY(U,$J,358.3,6664,1,3,0)
 ;;=3^Dermabrasion,4 Addl Lesions
 ;;^UTILITY(U,$J,358.3,6665,0)
 ;;=96900^^45^434^7^^^^1
 ;;^UTILITY(U,$J,358.3,6665,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,6665,1,2,0)
 ;;=2^96900
 ;;^UTILITY(U,$J,358.3,6665,1,3,0)
 ;;=3^Ultraviolet Light Therapy
 ;;^UTILITY(U,$J,358.3,6666,0)
 ;;=96910^^45^434^5^^^^1
 ;;^UTILITY(U,$J,358.3,6666,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,6666,1,2,0)
 ;;=2^96910
 ;;^UTILITY(U,$J,358.3,6666,1,3,0)
 ;;=3^Photochemotherapy w/ UV-B
 ;;^UTILITY(U,$J,358.3,6667,0)
 ;;=96912^^45^434^4^^^^1
 ;;^UTILITY(U,$J,358.3,6667,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,6667,1,2,0)
 ;;=2^96912
 ;;^UTILITY(U,$J,358.3,6667,1,3,0)
 ;;=3^Photochemotherapy w/ UV-A
 ;;^UTILITY(U,$J,358.3,6668,0)
 ;;=96567^^45^434^6^^^^1
 ;;^UTILITY(U,$J,358.3,6668,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,6668,1,2,0)
 ;;=2^96567
 ;;^UTILITY(U,$J,358.3,6668,1,3,0)
 ;;=3^Photodynamic Tx Skin
 ;;^UTILITY(U,$J,358.3,6669,0)
 ;;=96920^^45^434^1^^^^1
 ;;^UTILITY(U,$J,358.3,6669,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,6669,1,2,0)
 ;;=2^96920
 ;;^UTILITY(U,$J,358.3,6669,1,3,0)
 ;;=3^Laser Tx Skin < 250 sq cm
 ;;^UTILITY(U,$J,358.3,6670,0)
 ;;=96921^^45^434^2^^^^1
 ;;^UTILITY(U,$J,358.3,6670,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,6670,1,2,0)
 ;;=2^96921
 ;;^UTILITY(U,$J,358.3,6670,1,3,0)
 ;;=3^Laser Tx Skin 250-500 sq cm
 ;;^UTILITY(U,$J,358.3,6671,0)
 ;;=96922^^45^434^3^^^^1
 ;;^UTILITY(U,$J,358.3,6671,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,6671,1,2,0)
 ;;=2^96922
 ;;^UTILITY(U,$J,358.3,6671,1,3,0)
 ;;=3^Laser Tx Skin > 500 sq cm
 ;;^UTILITY(U,$J,358.3,6672,0)
 ;;=13151^^45^435^1^^^^1
 ;;^UTILITY(U,$J,358.3,6672,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,6672,1,2,0)
 ;;=2^13151
 ;;^UTILITY(U,$J,358.3,6672,1,3,0)
 ;;=3^Complex Repair Face;1.1 to 2.5 cm
 ;;^UTILITY(U,$J,358.3,6673,0)
 ;;=13152^^45^435^2^^^^1
 ;;^UTILITY(U,$J,358.3,6673,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,6673,1,2,0)
 ;;=2^13152
 ;;^UTILITY(U,$J,358.3,6673,1,3,0)
 ;;=3^Complex Repair Face;2.6 to 7.5 cm
 ;;^UTILITY(U,$J,358.3,6674,0)
 ;;=13153^^45^435^3^^^^1
 ;;^UTILITY(U,$J,358.3,6674,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,6674,1,2,0)
 ;;=2^13153
 ;;^UTILITY(U,$J,358.3,6674,1,3,0)
 ;;=3^Complex Repair Face;Ea Addl 5 cm
 ;;^UTILITY(U,$J,358.3,6675,0)
 ;;=13131^^45^436^1^^^^1
 ;;^UTILITY(U,$J,358.3,6675,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,6675,1,2,0)
 ;;=2^13131
 ;;^UTILITY(U,$J,358.3,6675,1,3,0)
 ;;=3^Complex Repair Nk/Hd/Ft;1.1 to 2.5 cm
