IBDEI16X ; ; 12-AUG-2014
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;MAY 15, 2014
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,21308,2)
 ;;=^87311
 ;;^UTILITY(U,$J,358.3,21309,0)
 ;;=211.3^^133^1314^19
 ;;^UTILITY(U,$J,358.3,21309,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,21309,1,4,0)
 ;;=4^211.3
 ;;^UTILITY(U,$J,358.3,21309,1,5,0)
 ;;=5^Colon Polyps (Current)
 ;;^UTILITY(U,$J,358.3,21309,2)
 ;;=Colon Polyps (current)^13295
 ;;^UTILITY(U,$J,358.3,21310,0)
 ;;=V12.72^^133^1314^20
 ;;^UTILITY(U,$J,358.3,21310,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,21310,1,4,0)
 ;;=4^V12.72
 ;;^UTILITY(U,$J,358.3,21310,1,5,0)
 ;;=5^Colon Polyps (Removed)
 ;;^UTILITY(U,$J,358.3,21310,2)
 ;;=Colon Polyps (removed)^303401
 ;;^UTILITY(U,$J,358.3,21311,0)
 ;;=789.01^^133^1314^6
 ;;^UTILITY(U,$J,358.3,21311,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,21311,1,4,0)
 ;;=4^789.01
 ;;^UTILITY(U,$J,358.3,21311,1,5,0)
 ;;=5^Abdominal Pain,RUQ
 ;;^UTILITY(U,$J,358.3,21311,2)
 ;;=^303318
 ;;^UTILITY(U,$J,358.3,21312,0)
 ;;=789.02^^133^1314^4
 ;;^UTILITY(U,$J,358.3,21312,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,21312,1,4,0)
 ;;=4^789.02
 ;;^UTILITY(U,$J,358.3,21312,1,5,0)
 ;;=5^Abdominal Pain,LUQ
 ;;^UTILITY(U,$J,358.3,21312,2)
 ;;=^303319
 ;;^UTILITY(U,$J,358.3,21313,0)
 ;;=789.03^^133^1314^5
 ;;^UTILITY(U,$J,358.3,21313,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,21313,1,4,0)
 ;;=4^789.03
 ;;^UTILITY(U,$J,358.3,21313,1,5,0)
 ;;=5^Abdominal Pain,RLQ
 ;;^UTILITY(U,$J,358.3,21313,2)
 ;;=^303320
 ;;^UTILITY(U,$J,358.3,21314,0)
 ;;=789.04^^133^1314^3
 ;;^UTILITY(U,$J,358.3,21314,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,21314,1,4,0)
 ;;=4^789.04
 ;;^UTILITY(U,$J,358.3,21314,1,5,0)
 ;;=5^Abdominal Pain,LLQ
 ;;^UTILITY(U,$J,358.3,21314,2)
 ;;=^303321
 ;;^UTILITY(U,$J,358.3,21315,0)
 ;;=789.05^^133^1314^83
 ;;^UTILITY(U,$J,358.3,21315,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,21315,1,4,0)
 ;;=4^789.05
 ;;^UTILITY(U,$J,358.3,21315,1,5,0)
 ;;=5^Periumbilical Pain
 ;;^UTILITY(U,$J,358.3,21315,2)
 ;;=^303322
 ;;^UTILITY(U,$J,358.3,21316,0)
 ;;=789.06^^133^1314^38
 ;;^UTILITY(U,$J,358.3,21316,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,21316,1,4,0)
 ;;=4^789.06
 ;;^UTILITY(U,$J,358.3,21316,1,5,0)
 ;;=5^Epigastric Pain
 ;;^UTILITY(U,$J,358.3,21316,2)
 ;;=^303323
 ;;^UTILITY(U,$J,358.3,21317,0)
 ;;=789.61^^133^1314^10
 ;;^UTILITY(U,$J,358.3,21317,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,21317,1,4,0)
 ;;=4^789.61
 ;;^UTILITY(U,$J,358.3,21317,1,5,0)
 ;;=5^Abdominal Tenderness,RUQ
 ;;^UTILITY(U,$J,358.3,21317,2)
 ;;=^303343
 ;;^UTILITY(U,$J,358.3,21318,0)
 ;;=789.62^^133^1314^8
 ;;^UTILITY(U,$J,358.3,21318,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,21318,1,4,0)
 ;;=4^789.62
 ;;^UTILITY(U,$J,358.3,21318,1,5,0)
 ;;=5^Abdominal Tenderness,LUQ
 ;;^UTILITY(U,$J,358.3,21318,2)
 ;;=^303344
 ;;^UTILITY(U,$J,358.3,21319,0)
 ;;=789.63^^133^1314^9
 ;;^UTILITY(U,$J,358.3,21319,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,21319,1,4,0)
 ;;=4^789.63
 ;;^UTILITY(U,$J,358.3,21319,1,5,0)
 ;;=5^Abdominal Tenderness,RLQ
 ;;^UTILITY(U,$J,358.3,21319,2)
 ;;=^303345
 ;;^UTILITY(U,$J,358.3,21320,0)
 ;;=789.64^^133^1314^7
 ;;^UTILITY(U,$J,358.3,21320,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,21320,1,4,0)
 ;;=4^789.64
 ;;^UTILITY(U,$J,358.3,21320,1,5,0)
 ;;=5^Abdominal Tenderness,LLQ
 ;;^UTILITY(U,$J,358.3,21320,2)
 ;;=^303346
 ;;^UTILITY(U,$J,358.3,21321,0)
 ;;=789.65^^133^1314^84
 ;;^UTILITY(U,$J,358.3,21321,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,21321,1,4,0)
 ;;=4^789.65
 ;;^UTILITY(U,$J,358.3,21321,1,5,0)
 ;;=5^Periumbilical Tenderness
 ;;^UTILITY(U,$J,358.3,21321,2)
 ;;=^303347
 ;;^UTILITY(U,$J,358.3,21322,0)
 ;;=789.66^^133^1314^39
