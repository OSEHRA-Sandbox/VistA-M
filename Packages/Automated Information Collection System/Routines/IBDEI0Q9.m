IBDEI0Q9 ; ; 12-MAY-2016
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;MAY 12, 2016
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,26405,1,4,0)
 ;;=4^R41.9
 ;;^UTILITY(U,$J,358.3,26405,2)
 ;;=^5019449
 ;;^UTILITY(U,$J,358.3,26406,0)
 ;;=F06.31^^100^1267^1
 ;;^UTILITY(U,$J,358.3,26406,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,26406,1,3,0)
 ;;=3^Depressive Disorder d/t Another Med Cond w/ Depressive Features
 ;;^UTILITY(U,$J,358.3,26406,1,4,0)
 ;;=4^F06.31
 ;;^UTILITY(U,$J,358.3,26406,2)
 ;;=^5003057
 ;;^UTILITY(U,$J,358.3,26407,0)
 ;;=F06.32^^100^1267^2
 ;;^UTILITY(U,$J,358.3,26407,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,26407,1,3,0)
 ;;=3^Depressive Disorder d/t Another Med Cond w/ Major Depressive-Like Episode
 ;;^UTILITY(U,$J,358.3,26407,1,4,0)
 ;;=4^F06.32
 ;;^UTILITY(U,$J,358.3,26407,2)
 ;;=^5003058
 ;;^UTILITY(U,$J,358.3,26408,0)
 ;;=F32.9^^100^1267^20
 ;;^UTILITY(U,$J,358.3,26408,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,26408,1,3,0)
 ;;=3^MDD,Single Episode,Unspec
 ;;^UTILITY(U,$J,358.3,26408,1,4,0)
 ;;=4^F32.9
 ;;^UTILITY(U,$J,358.3,26408,2)
 ;;=^5003528
 ;;^UTILITY(U,$J,358.3,26409,0)
 ;;=F32.0^^100^1267^17
 ;;^UTILITY(U,$J,358.3,26409,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,26409,1,3,0)
 ;;=3^MDD,Single Episode,Mild
 ;;^UTILITY(U,$J,358.3,26409,1,4,0)
 ;;=4^F32.0
 ;;^UTILITY(U,$J,358.3,26409,2)
 ;;=^5003521
 ;;^UTILITY(U,$J,358.3,26410,0)
 ;;=F32.1^^100^1267^18
 ;;^UTILITY(U,$J,358.3,26410,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,26410,1,3,0)
 ;;=3^MDD,Single Episode,Moderate
 ;;^UTILITY(U,$J,358.3,26410,1,4,0)
 ;;=4^F32.1
 ;;^UTILITY(U,$J,358.3,26410,2)
 ;;=^5003522
 ;;^UTILITY(U,$J,358.3,26411,0)
 ;;=F32.2^^100^1267^19
 ;;^UTILITY(U,$J,358.3,26411,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,26411,1,3,0)
 ;;=3^MDD,Single Episode,Severe
 ;;^UTILITY(U,$J,358.3,26411,1,4,0)
 ;;=4^F32.2
 ;;^UTILITY(U,$J,358.3,26411,2)
 ;;=^5003523
 ;;^UTILITY(U,$J,358.3,26412,0)
 ;;=F32.3^^100^1267^14
 ;;^UTILITY(U,$J,358.3,26412,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,26412,1,3,0)
 ;;=3^MDD,Single Episode w Psychotic Features
 ;;^UTILITY(U,$J,358.3,26412,1,4,0)
 ;;=4^F32.3
 ;;^UTILITY(U,$J,358.3,26412,2)
 ;;=^5003524
 ;;^UTILITY(U,$J,358.3,26413,0)
 ;;=F32.4^^100^1267^16
 ;;^UTILITY(U,$J,358.3,26413,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,26413,1,3,0)
 ;;=3^MDD,Single Episode,In Partial Remission
 ;;^UTILITY(U,$J,358.3,26413,1,4,0)
 ;;=4^F32.4
 ;;^UTILITY(U,$J,358.3,26413,2)
 ;;=^5003525
 ;;^UTILITY(U,$J,358.3,26414,0)
 ;;=F32.5^^100^1267^15
 ;;^UTILITY(U,$J,358.3,26414,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,26414,1,3,0)
 ;;=3^MDD,Single Episode,In Full Remission
 ;;^UTILITY(U,$J,358.3,26414,1,4,0)
 ;;=4^F32.5
 ;;^UTILITY(U,$J,358.3,26414,2)
 ;;=^5003526
 ;;^UTILITY(U,$J,358.3,26415,0)
 ;;=F33.9^^100^1267^13
 ;;^UTILITY(U,$J,358.3,26415,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,26415,1,3,0)
 ;;=3^MDD,Recurrent Episode,Unspec
 ;;^UTILITY(U,$J,358.3,26415,1,4,0)
 ;;=4^F33.9
 ;;^UTILITY(U,$J,358.3,26415,2)
 ;;=^5003537
 ;;^UTILITY(U,$J,358.3,26416,0)
 ;;=F33.0^^100^1267^10
 ;;^UTILITY(U,$J,358.3,26416,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,26416,1,3,0)
 ;;=3^MDD,Recurrent Episode,Mild
 ;;^UTILITY(U,$J,358.3,26416,1,4,0)
 ;;=4^F33.0
 ;;^UTILITY(U,$J,358.3,26416,2)
 ;;=^5003529
 ;;^UTILITY(U,$J,358.3,26417,0)
 ;;=F33.1^^100^1267^11
 ;;^UTILITY(U,$J,358.3,26417,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,26417,1,3,0)
 ;;=3^MDD,Recurrent Episode,Moderate
 ;;^UTILITY(U,$J,358.3,26417,1,4,0)
 ;;=4^F33.1
 ;;^UTILITY(U,$J,358.3,26417,2)
 ;;=^5003530
 ;;^UTILITY(U,$J,358.3,26418,0)
 ;;=F33.2^^100^1267^12
 ;;^UTILITY(U,$J,358.3,26418,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,26418,1,3,0)
 ;;=3^MDD,Recurrent Episode,Severe
 ;;^UTILITY(U,$J,358.3,26418,1,4,0)
 ;;=4^F33.2
 ;;^UTILITY(U,$J,358.3,26418,2)
 ;;=^5003531
 ;;^UTILITY(U,$J,358.3,26419,0)
 ;;=F33.3^^100^1267^7
 ;;^UTILITY(U,$J,358.3,26419,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,26419,1,3,0)
 ;;=3^MDD,Recurrent Episode w/ Psychotic Features
 ;;^UTILITY(U,$J,358.3,26419,1,4,0)
 ;;=4^F33.3
 ;;^UTILITY(U,$J,358.3,26419,2)
 ;;=^5003532
 ;;^UTILITY(U,$J,358.3,26420,0)
 ;;=F33.41^^100^1267^9
 ;;^UTILITY(U,$J,358.3,26420,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,26420,1,3,0)
 ;;=3^MDD,Recurrent Episode,In Partial Remission
 ;;^UTILITY(U,$J,358.3,26420,1,4,0)
 ;;=4^F33.41
 ;;^UTILITY(U,$J,358.3,26420,2)
 ;;=^5003534
 ;;^UTILITY(U,$J,358.3,26421,0)
 ;;=F33.42^^100^1267^8
 ;;^UTILITY(U,$J,358.3,26421,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,26421,1,3,0)
 ;;=3^MDD,Recurrent Episode,In Full Remission
 ;;^UTILITY(U,$J,358.3,26421,1,4,0)
 ;;=4^F33.42
 ;;^UTILITY(U,$J,358.3,26421,2)
 ;;=^5003535
 ;;^UTILITY(U,$J,358.3,26422,0)
 ;;=F34.8^^100^1267^6
 ;;^UTILITY(U,$J,358.3,26422,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,26422,1,3,0)
 ;;=3^Disruptive Mood Dysregulation Disorder
 ;;^UTILITY(U,$J,358.3,26422,1,4,0)
 ;;=4^F34.8
 ;;^UTILITY(U,$J,358.3,26422,2)
 ;;=^5003539
 ;;^UTILITY(U,$J,358.3,26423,0)
 ;;=F34.1^^100^1267^21
 ;;^UTILITY(U,$J,358.3,26423,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,26423,1,3,0)
 ;;=3^Persistent Depressive Disorder (Dysthmia)
 ;;^UTILITY(U,$J,358.3,26423,1,4,0)
 ;;=4^F34.1
 ;;^UTILITY(U,$J,358.3,26423,2)
 ;;=^331913
 ;;^UTILITY(U,$J,358.3,26424,0)
 ;;=N94.3^^100^1267^22
 ;;^UTILITY(U,$J,358.3,26424,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,26424,1,3,0)
 ;;=3^Premenstrual Dysphoric Disorder
 ;;^UTILITY(U,$J,358.3,26424,1,4,0)
 ;;=4^N94.3
 ;;^UTILITY(U,$J,358.3,26424,2)
 ;;=^5015919
 ;;^UTILITY(U,$J,358.3,26425,0)
 ;;=F06.34^^100^1267^3
 ;;^UTILITY(U,$J,358.3,26425,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,26425,1,3,0)
 ;;=3^Depressive Disorder d/t Another Med Cond w/ Mixed Features,Unsp
 ;;^UTILITY(U,$J,358.3,26425,1,4,0)
 ;;=4^F06.34
 ;;^UTILITY(U,$J,358.3,26425,2)
 ;;=^5003060
 ;;^UTILITY(U,$J,358.3,26426,0)
 ;;=F32.8^^100^1267^4
 ;;^UTILITY(U,$J,358.3,26426,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,26426,1,3,0)
 ;;=3^Depressive Disorder,Other Specified
 ;;^UTILITY(U,$J,358.3,26426,1,4,0)
 ;;=4^F32.8
 ;;^UTILITY(U,$J,358.3,26426,2)
 ;;=^5003527
 ;;^UTILITY(U,$J,358.3,26427,0)
 ;;=F32.9^^100^1267^5
 ;;^UTILITY(U,$J,358.3,26427,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,26427,1,3,0)
 ;;=3^Depressive Disorder,Unsp
 ;;^UTILITY(U,$J,358.3,26427,1,4,0)
 ;;=4^F32.9
 ;;^UTILITY(U,$J,358.3,26427,2)
 ;;=^5003528
 ;;^UTILITY(U,$J,358.3,26428,0)
 ;;=F44.81^^100^1268^6
 ;;^UTILITY(U,$J,358.3,26428,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,26428,1,3,0)
 ;;=3^Dissociative Identity Disorder
 ;;^UTILITY(U,$J,358.3,26428,1,4,0)
 ;;=4^F44.81
 ;;^UTILITY(U,$J,358.3,26428,2)
 ;;=^331909
 ;;^UTILITY(U,$J,358.3,26429,0)
 ;;=F44.9^^100^1268^5
 ;;^UTILITY(U,$J,358.3,26429,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,26429,1,3,0)
 ;;=3^Dissociative Disorder,Unspec
 ;;^UTILITY(U,$J,358.3,26429,1,4,0)
 ;;=4^F44.9
 ;;^UTILITY(U,$J,358.3,26429,2)
 ;;=^5003584
 ;;^UTILITY(U,$J,358.3,26430,0)
 ;;=F44.0^^100^1268^2
 ;;^UTILITY(U,$J,358.3,26430,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,26430,1,3,0)
 ;;=3^Dissociative Amnesia
 ;;^UTILITY(U,$J,358.3,26430,1,4,0)
 ;;=4^F44.0
 ;;^UTILITY(U,$J,358.3,26430,2)
 ;;=^5003577
 ;;^UTILITY(U,$J,358.3,26431,0)
 ;;=F48.1^^100^1268^1
 ;;^UTILITY(U,$J,358.3,26431,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,26431,1,3,0)
 ;;=3^Depersonalization/Derealization Disorder
 ;;^UTILITY(U,$J,358.3,26431,1,4,0)
 ;;=4^F48.1
 ;;^UTILITY(U,$J,358.3,26431,2)
 ;;=^5003593
 ;;^UTILITY(U,$J,358.3,26432,0)
 ;;=F44.89^^100^1268^4
 ;;^UTILITY(U,$J,358.3,26432,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,26432,1,3,0)
 ;;=3^Dissociative Disorder,Other Specified
 ;;^UTILITY(U,$J,358.3,26432,1,4,0)
 ;;=4^F44.89
 ;;^UTILITY(U,$J,358.3,26432,2)
 ;;=^5003583
 ;;^UTILITY(U,$J,358.3,26433,0)
 ;;=F44.1^^100^1268^3
 ;;^UTILITY(U,$J,358.3,26433,1,0)
 ;;=^358.31IA^4^2
