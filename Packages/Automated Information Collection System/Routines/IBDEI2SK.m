IBDEI2SK ; ; 19-NOV-2015
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;JUN 29, 2015
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,46862,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,46862,1,3,0)
 ;;=3^Major Depressive Disorder,Single Episoide,Unspec
 ;;^UTILITY(U,$J,358.3,46862,1,4,0)
 ;;=4^F32.9
 ;;^UTILITY(U,$J,358.3,46862,2)
 ;;=^5003528
 ;;^UTILITY(U,$J,358.3,46863,0)
 ;;=F17.221^^206^2312^4
 ;;^UTILITY(U,$J,358.3,46863,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,46863,1,3,0)
 ;;=3^Nicotine Dependence,Chewing Tobacco,In Remission
 ;;^UTILITY(U,$J,358.3,46863,1,4,0)
 ;;=4^F17.221
 ;;^UTILITY(U,$J,358.3,46863,2)
 ;;=^5003371
 ;;^UTILITY(U,$J,358.3,46864,0)
 ;;=F17.220^^206^2312^5
 ;;^UTILITY(U,$J,358.3,46864,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,46864,1,3,0)
 ;;=3^Nicotine Dependence,Chewing Tobacco,Uncomplicated
 ;;^UTILITY(U,$J,358.3,46864,1,4,0)
 ;;=4^F17.220
 ;;^UTILITY(U,$J,358.3,46864,2)
 ;;=^5003370
 ;;^UTILITY(U,$J,358.3,46865,0)
 ;;=F17.211^^206^2312^6
 ;;^UTILITY(U,$J,358.3,46865,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,46865,1,3,0)
 ;;=3^Nicotine Dependence,Cigarettes,In Remission
 ;;^UTILITY(U,$J,358.3,46865,1,4,0)
 ;;=4^F17.211
 ;;^UTILITY(U,$J,358.3,46865,2)
 ;;=^5003366
 ;;^UTILITY(U,$J,358.3,46866,0)
 ;;=F17.210^^206^2312^7
 ;;^UTILITY(U,$J,358.3,46866,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,46866,1,3,0)
 ;;=3^Nicotine Dependence,Cigarettes,Uncomplicated
 ;;^UTILITY(U,$J,358.3,46866,1,4,0)
 ;;=4^F17.210
 ;;^UTILITY(U,$J,358.3,46866,2)
 ;;=^5003365
 ;;^UTILITY(U,$J,358.3,46867,0)
 ;;=F17.291^^206^2312^8
 ;;^UTILITY(U,$J,358.3,46867,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,46867,1,3,0)
 ;;=3^Nicotine Dependence,Oth Tobacco Product,In Remission
 ;;^UTILITY(U,$J,358.3,46867,1,4,0)
 ;;=4^F17.291
 ;;^UTILITY(U,$J,358.3,46867,2)
 ;;=^5003376
 ;;^UTILITY(U,$J,358.3,46868,0)
 ;;=F17.290^^206^2312^9
 ;;^UTILITY(U,$J,358.3,46868,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,46868,1,3,0)
 ;;=3^Nicotine Dependence,Oth Tobacco Product,Uncomplicated
 ;;^UTILITY(U,$J,358.3,46868,1,4,0)
 ;;=4^F17.290
 ;;^UTILITY(U,$J,358.3,46868,2)
 ;;=^5003375
 ;;^UTILITY(U,$J,358.3,46869,0)
 ;;=F17.201^^206^2312^10
 ;;^UTILITY(U,$J,358.3,46869,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,46869,1,3,0)
 ;;=3^Nicotine Dependence,Unspec,In Remission
 ;;^UTILITY(U,$J,358.3,46869,1,4,0)
 ;;=4^F17.201
 ;;^UTILITY(U,$J,358.3,46869,2)
 ;;=^5003361
 ;;^UTILITY(U,$J,358.3,46870,0)
 ;;=F17.200^^206^2312^11
 ;;^UTILITY(U,$J,358.3,46870,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,46870,1,3,0)
 ;;=3^Nicotine Dependence,Unspec,Uncomplicated
 ;;^UTILITY(U,$J,358.3,46870,1,4,0)
 ;;=4^F17.200
 ;;^UTILITY(U,$J,358.3,46870,2)
 ;;=^5003360
 ;;^UTILITY(U,$J,358.3,46871,0)
 ;;=F19.20^^206^2312^15
 ;;^UTILITY(U,$J,358.3,46871,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,46871,1,3,0)
 ;;=3^Psycoactive Substance Dependence,Uncomplicated
 ;;^UTILITY(U,$J,358.3,46871,1,4,0)
 ;;=4^F19.20
 ;;^UTILITY(U,$J,358.3,46871,2)
 ;;=^5003431
 ;;^UTILITY(U,$J,358.3,46872,0)
 ;;=F43.11^^206^2312^12
 ;;^UTILITY(U,$J,358.3,46872,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,46872,1,3,0)
 ;;=3^PTSD,Acute
 ;;^UTILITY(U,$J,358.3,46872,1,4,0)
 ;;=4^F43.11
 ;;^UTILITY(U,$J,358.3,46872,2)
 ;;=^5003571
 ;;^UTILITY(U,$J,358.3,46873,0)
 ;;=F43.12^^206^2312^13
 ;;^UTILITY(U,$J,358.3,46873,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,46873,1,3,0)
 ;;=3^PTSD,Chronic
 ;;^UTILITY(U,$J,358.3,46873,1,4,0)
 ;;=4^F43.12
 ;;^UTILITY(U,$J,358.3,46873,2)
 ;;=^5003572
 ;;^UTILITY(U,$J,358.3,46874,0)
 ;;=F43.10^^206^2312^14
 ;;^UTILITY(U,$J,358.3,46874,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,46874,1,3,0)
 ;;=3^PTSD,Unspec
 ;;^UTILITY(U,$J,358.3,46874,1,4,0)
 ;;=4^F43.10
