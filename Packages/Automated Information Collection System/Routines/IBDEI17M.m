IBDEI17M ; ; 19-NOV-2015
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;JUN 29, 2015
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,20213,0)
 ;;=43760^^96^949^1^^^^1
 ;;^UTILITY(U,$J,358.3,20213,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,20213,1,1,0)
 ;;=1^43760
 ;;^UTILITY(U,$J,358.3,20213,1,2,0)
 ;;=2^Change Gastrostomy Tube
 ;;^UTILITY(U,$J,358.3,20214,0)
 ;;=97602^^96^949^2^^^^1
 ;;^UTILITY(U,$J,358.3,20214,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,20214,1,1,0)
 ;;=1^97602
 ;;^UTILITY(U,$J,358.3,20214,1,2,0)
 ;;=2^Wound(S) Care And Instruction
 ;;^UTILITY(U,$J,358.3,20215,0)
 ;;=51701^^96^950^1^^^^1
 ;;^UTILITY(U,$J,358.3,20215,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,20215,1,1,0)
 ;;=1^51701
 ;;^UTILITY(U,$J,358.3,20215,1,2,0)
 ;;=2^Insert Bladder Catheter
 ;;^UTILITY(U,$J,358.3,20216,0)
 ;;=51702^^96^950^2^^^^1
 ;;^UTILITY(U,$J,358.3,20216,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,20216,1,1,0)
 ;;=1^51702
 ;;^UTILITY(U,$J,358.3,20216,1,2,0)
 ;;=2^Insert Foley Catheter
 ;;^UTILITY(U,$J,358.3,20217,0)
 ;;=86580^^96^951^1^^^^1
 ;;^UTILITY(U,$J,358.3,20217,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,20217,1,1,0)
 ;;=1^86580
 ;;^UTILITY(U,$J,358.3,20217,1,2,0)
 ;;=2^TB Intradermal Test
 ;;^UTILITY(U,$J,358.3,20218,0)
 ;;=99001^^96^952^1^^^^1
 ;;^UTILITY(U,$J,358.3,20218,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,20218,1,1,0)
 ;;=1^99001
 ;;^UTILITY(U,$J,358.3,20218,1,2,0)
 ;;=2^Specimen Handling
 ;;^UTILITY(U,$J,358.3,20219,0)
 ;;=99366^^96^953^1^^^^1
 ;;^UTILITY(U,$J,358.3,20219,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,20219,1,1,0)
 ;;=1^99366
 ;;^UTILITY(U,$J,358.3,20219,1,2,0)
 ;;=2^Team Conf w/ Pt By HC Pro,30+ Min
 ;;^UTILITY(U,$J,358.3,20220,0)
 ;;=99368^^96^953^2^^^^1
 ;;^UTILITY(U,$J,358.3,20220,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,20220,1,1,0)
 ;;=1^99368
 ;;^UTILITY(U,$J,358.3,20220,1,2,0)
 ;;=2^Team Conf w/o Pt by HC Pro,30+ Min
 ;;^UTILITY(U,$J,358.3,20221,0)
 ;;=99503^^96^954^5^^^^1
 ;;^UTILITY(U,$J,358.3,20221,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,20221,1,1,0)
 ;;=1^99503
 ;;^UTILITY(U,$J,358.3,20221,1,2,0)
 ;;=2^Home Visit-Respiratory Tx Care
 ;;^UTILITY(U,$J,358.3,20222,0)
 ;;=99504^^96^954^4^^^^1
 ;;^UTILITY(U,$J,358.3,20222,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,20222,1,1,0)
 ;;=1^99504
 ;;^UTILITY(U,$J,358.3,20222,1,2,0)
 ;;=2^Home Visit-Mech Ventilator Care
 ;;^UTILITY(U,$J,358.3,20223,0)
 ;;=99507^^96^954^1^^^^1
 ;;^UTILITY(U,$J,358.3,20223,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,20223,1,1,0)
 ;;=1^99507
 ;;^UTILITY(U,$J,358.3,20223,1,2,0)
 ;;=2^Home Visit-Cath Maintenance
 ;;^UTILITY(U,$J,358.3,20224,0)
 ;;=99511^^96^954^2^^^^1
 ;;^UTILITY(U,$J,358.3,20224,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,20224,1,1,0)
 ;;=1^99511
 ;;^UTILITY(U,$J,358.3,20224,1,2,0)
 ;;=2^Home Visit-Fecal/Enema Mgmt
 ;;^UTILITY(U,$J,358.3,20225,0)
 ;;=99505^^96^954^6^^^^1
 ;;^UTILITY(U,$J,358.3,20225,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,20225,1,1,0)
 ;;=1^99505
 ;;^UTILITY(U,$J,358.3,20225,1,2,0)
 ;;=2^Home Visit-Stoma Care
 ;;^UTILITY(U,$J,358.3,20226,0)
 ;;=99512^^96^954^3^^^^1
 ;;^UTILITY(U,$J,358.3,20226,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,20226,1,1,0)
 ;;=1^99512
 ;;^UTILITY(U,$J,358.3,20226,1,2,0)
 ;;=2^Home Visit-Hemodialysis
 ;;^UTILITY(U,$J,358.3,20227,0)
 ;;=99601^^96^955^1^^^^1
 ;;^UTILITY(U,$J,358.3,20227,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,20227,1,1,0)
 ;;=1^99601
 ;;^UTILITY(U,$J,358.3,20227,1,2,0)
 ;;=2^Home Infsn,Up to 2 hrs
 ;;^UTILITY(U,$J,358.3,20228,0)
 ;;=99602^^96^955^2^^^^1
 ;;^UTILITY(U,$J,358.3,20228,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,20228,1,1,0)
 ;;=1^99602
 ;;^UTILITY(U,$J,358.3,20228,1,2,0)
 ;;=2^Home Infsn,Ea Addl Hr
