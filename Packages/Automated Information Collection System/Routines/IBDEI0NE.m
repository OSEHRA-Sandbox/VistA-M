IBDEI0NE ; ; 17-FEB-2016
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;JUN 29, 2015
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,10919,1,3,0)
 ;;=3^Skin Changes,Unspec
 ;;^UTILITY(U,$J,358.3,10919,1,4,0)
 ;;=4^R23.9
 ;;^UTILITY(U,$J,358.3,10919,2)
 ;;=^5019298
 ;;^UTILITY(U,$J,358.3,10920,0)
 ;;=M79.9^^47^522^108
 ;;^UTILITY(U,$J,358.3,10920,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,10920,1,3,0)
 ;;=3^Soft Tissue Disorder,Unspec
 ;;^UTILITY(U,$J,358.3,10920,1,4,0)
 ;;=4^M79.9
 ;;^UTILITY(U,$J,358.3,10920,2)
 ;;=^5013358
 ;;^UTILITY(U,$J,358.3,10921,0)
 ;;=L55.9^^47^522^109
 ;;^UTILITY(U,$J,358.3,10921,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,10921,1,3,0)
 ;;=3^Sunburn,Unspec
 ;;^UTILITY(U,$J,358.3,10921,1,4,0)
 ;;=4^L55.9
 ;;^UTILITY(U,$J,358.3,10921,2)
 ;;=^5009213
 ;;^UTILITY(U,$J,358.3,10922,0)
 ;;=L75.9^^47^522^110
 ;;^UTILITY(U,$J,358.3,10922,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,10922,1,3,0)
 ;;=3^Sweat Disorder,Apocrine,Unspec
 ;;^UTILITY(U,$J,358.3,10922,1,4,0)
 ;;=4^L75.9
 ;;^UTILITY(U,$J,358.3,10922,2)
 ;;=^5009301
 ;;^UTILITY(U,$J,358.3,10923,0)
 ;;=L74.9^^47^522^111
 ;;^UTILITY(U,$J,358.3,10923,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,10923,1,3,0)
 ;;=3^Sweat Disorder,Eccrine,Unspec
 ;;^UTILITY(U,$J,358.3,10923,1,4,0)
 ;;=4^L74.9
 ;;^UTILITY(U,$J,358.3,10923,2)
 ;;=^5009296
 ;;^UTILITY(U,$J,358.3,10924,0)
 ;;=R22.9^^47^522^112
 ;;^UTILITY(U,$J,358.3,10924,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,10924,1,3,0)
 ;;=3^Swelling,Mass & Lump,Localized,Unspec
 ;;^UTILITY(U,$J,358.3,10924,1,4,0)
 ;;=4^R22.9
 ;;^UTILITY(U,$J,358.3,10924,2)
 ;;=^5019292
 ;;^UTILITY(U,$J,358.3,10925,0)
 ;;=L50.9^^47^522^113
 ;;^UTILITY(U,$J,358.3,10925,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,10925,1,3,0)
 ;;=3^Urticaria,Unspec
 ;;^UTILITY(U,$J,358.3,10925,1,4,0)
 ;;=4^L50.9
 ;;^UTILITY(U,$J,358.3,10925,2)
 ;;=^5009204
 ;;^UTILITY(U,$J,358.3,10926,0)
 ;;=L95.9^^47^522^114
 ;;^UTILITY(U,$J,358.3,10926,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,10926,1,3,0)
 ;;=3^Vasculitis,Skin,Unspec
 ;;^UTILITY(U,$J,358.3,10926,1,4,0)
 ;;=4^L95.9
 ;;^UTILITY(U,$J,358.3,10926,2)
 ;;=^5009479
 ;;^UTILITY(U,$J,358.3,10927,0)
 ;;=L80.^^47^522^115
 ;;^UTILITY(U,$J,358.3,10927,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,10927,1,3,0)
 ;;=3^Vitiligo
 ;;^UTILITY(U,$J,358.3,10927,1,4,0)
 ;;=4^L80.
 ;;^UTILITY(U,$J,358.3,10927,2)
 ;;=^127071
 ;;^UTILITY(U,$J,358.3,10928,0)
 ;;=B07.9^^47^522^116
 ;;^UTILITY(U,$J,358.3,10928,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,10928,1,3,0)
 ;;=3^Warts,Viral,Unspec
 ;;^UTILITY(U,$J,358.3,10928,1,4,0)
 ;;=4^B07.9
 ;;^UTILITY(U,$J,358.3,10928,2)
 ;;=^5000519
 ;;^UTILITY(U,$J,358.3,10929,0)
 ;;=L85.3^^47^522^117
 ;;^UTILITY(U,$J,358.3,10929,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,10929,1,3,0)
 ;;=3^Xerosis Cutis
 ;;^UTILITY(U,$J,358.3,10929,1,4,0)
 ;;=4^L85.3
 ;;^UTILITY(U,$J,358.3,10929,2)
 ;;=^5009323
 ;;^UTILITY(U,$J,358.3,10930,0)
 ;;=C21.0^^47^523^3
 ;;^UTILITY(U,$J,358.3,10930,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,10930,1,3,0)
 ;;=3^Cancer,Anus,Unspec Type & Site
 ;;^UTILITY(U,$J,358.3,10930,1,4,0)
 ;;=4^C21.0
 ;;^UTILITY(U,$J,358.3,10930,2)
 ;;=^5000930
 ;;^UTILITY(U,$J,358.3,10931,0)
 ;;=C41.9^^47^523^4
 ;;^UTILITY(U,$J,358.3,10931,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,10931,1,3,0)
 ;;=3^Cancer,Bone & Articular Cartilage,Unspec Site
 ;;^UTILITY(U,$J,358.3,10931,1,4,0)
 ;;=4^C41.9
 ;;^UTILITY(U,$J,358.3,10931,2)
 ;;=^5000993
 ;;^UTILITY(U,$J,358.3,10932,0)
 ;;=C50.911^^47^523^6
 ;;^UTILITY(U,$J,358.3,10932,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,10932,1,3,0)
 ;;=3^Cancer,Breast,Female,Right
 ;;^UTILITY(U,$J,358.3,10932,1,4,0)
 ;;=4^C50.911
 ;;^UTILITY(U,$J,358.3,10932,2)
 ;;=^5001195
