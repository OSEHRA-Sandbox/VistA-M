IBDEI1GH ; ; 19-NOV-2015
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;JUN 29, 2015
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,24320,0)
 ;;=Z71.7^^118^1205^2
 ;;^UTILITY(U,$J,358.3,24320,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,24320,1,3,0)
 ;;=3^HIV Counseling
 ;;^UTILITY(U,$J,358.3,24320,1,4,0)
 ;;=4^Z71.7
 ;;^UTILITY(U,$J,358.3,24320,2)
 ;;=^5063251
 ;;^UTILITY(U,$J,358.3,24321,0)
 ;;=Z76.89^^118^1205^4
 ;;^UTILITY(U,$J,358.3,24321,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,24321,1,3,0)
 ;;=3^Health Services in Oth Circumstances
 ;;^UTILITY(U,$J,358.3,24321,1,4,0)
 ;;=4^Z76.89
 ;;^UTILITY(U,$J,358.3,24321,2)
 ;;=^5063304
 ;;^UTILITY(U,$J,358.3,24322,0)
 ;;=Z56.82^^118^1205^7
 ;;^UTILITY(U,$J,358.3,24322,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,24322,1,3,0)
 ;;=3^Military Deployment Status
 ;;^UTILITY(U,$J,358.3,24322,1,4,0)
 ;;=4^Z56.82
 ;;^UTILITY(U,$J,358.3,24322,2)
 ;;=^5063115
 ;;^UTILITY(U,$J,358.3,24323,0)
 ;;=Z91.82^^118^1205^8
 ;;^UTILITY(U,$J,358.3,24323,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,24323,1,3,0)
 ;;=3^Personal Hx of Military Deployment
 ;;^UTILITY(U,$J,358.3,24323,1,4,0)
 ;;=4^Z91.82
 ;;^UTILITY(U,$J,358.3,24323,2)
 ;;=^5063626
 ;;^UTILITY(U,$J,358.3,24324,0)
 ;;=Z72.51^^118^1206^2
 ;;^UTILITY(U,$J,358.3,24324,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,24324,1,3,0)
 ;;=3^High Risk Heterosexual Behavior
 ;;^UTILITY(U,$J,358.3,24324,1,4,0)
 ;;=4^Z72.51
 ;;^UTILITY(U,$J,358.3,24324,2)
 ;;=^5063258
 ;;^UTILITY(U,$J,358.3,24325,0)
 ;;=Z72.6^^118^1206^1
 ;;^UTILITY(U,$J,358.3,24325,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,24325,1,3,0)
 ;;=3^Gambling/Betting
 ;;^UTILITY(U,$J,358.3,24325,1,4,0)
 ;;=4^Z72.6
 ;;^UTILITY(U,$J,358.3,24325,2)
 ;;=^5063261
 ;;^UTILITY(U,$J,358.3,24326,0)
 ;;=Z72.3^^118^1206^4
 ;;^UTILITY(U,$J,358.3,24326,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,24326,1,3,0)
 ;;=3^Lack of Physical Exercise
 ;;^UTILITY(U,$J,358.3,24326,1,4,0)
 ;;=4^Z72.3
 ;;^UTILITY(U,$J,358.3,24326,2)
 ;;=^5063256
 ;;^UTILITY(U,$J,358.3,24327,0)
 ;;=Z72.4^^118^1206^3
 ;;^UTILITY(U,$J,358.3,24327,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,24327,1,3,0)
 ;;=3^Inappropriate Diet/Eating Habits
 ;;^UTILITY(U,$J,358.3,24327,1,4,0)
 ;;=4^Z72.4
 ;;^UTILITY(U,$J,358.3,24327,2)
 ;;=^5063257
 ;;^UTILITY(U,$J,358.3,24328,0)
 ;;=Z72.820^^118^1206^6
 ;;^UTILITY(U,$J,358.3,24328,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,24328,1,3,0)
 ;;=3^Sleep Deprivation
 ;;^UTILITY(U,$J,358.3,24328,1,4,0)
 ;;=4^Z72.820
 ;;^UTILITY(U,$J,358.3,24328,2)
 ;;=^5063264
 ;;^UTILITY(U,$J,358.3,24329,0)
 ;;=Z72.9^^118^1206^5
 ;;^UTILITY(U,$J,358.3,24329,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,24329,1,3,0)
 ;;=3^Lifestyle Related Problems,Unspec
 ;;^UTILITY(U,$J,358.3,24329,1,4,0)
 ;;=4^Z72.9
 ;;^UTILITY(U,$J,358.3,24329,2)
 ;;=^5063267
 ;;^UTILITY(U,$J,358.3,24330,0)
 ;;=Z13.89^^118^1207^1
 ;;^UTILITY(U,$J,358.3,24330,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,24330,1,3,0)
 ;;=3^Screening for Other Disorders
 ;;^UTILITY(U,$J,358.3,24330,1,4,0)
 ;;=4^Z13.89
 ;;^UTILITY(U,$J,358.3,24330,2)
 ;;=^5062720
 ;;^UTILITY(U,$J,358.3,24331,0)
 ;;=Z65.8^^118^1208^3
 ;;^UTILITY(U,$J,358.3,24331,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,24331,1,3,0)
 ;;=3^Psychosocial Related Problems
 ;;^UTILITY(U,$J,358.3,24331,1,4,0)
 ;;=4^Z65.8
 ;;^UTILITY(U,$J,358.3,24331,2)
 ;;=^5063185
 ;;^UTILITY(U,$J,358.3,24332,0)
 ;;=Z86.51^^118^1208^1
 ;;^UTILITY(U,$J,358.3,24332,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,24332,1,3,0)
 ;;=3^Personal Hx of Combat/Operational Stress Reaction
 ;;^UTILITY(U,$J,358.3,24332,1,4,0)
 ;;=4^Z86.51
 ;;^UTILITY(U,$J,358.3,24332,2)
 ;;=^5063470
 ;;^UTILITY(U,$J,358.3,24333,0)
 ;;=Z86.59^^118^1208^2
