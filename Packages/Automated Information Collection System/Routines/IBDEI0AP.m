IBDEI0AP ; ; 06-AUG-2013
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;AUG 06, 2013
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,14281,1,1,0)
 ;;=1^206.00
 ;;^UTILITY(U,$J,358.3,14281,1,8,0)
 ;;=8^Acute Monocytic Leuk w/o Remission
 ;;^UTILITY(U,$J,358.3,14281,2)
 ;;=^336861
 ;;^UTILITY(U,$J,358.3,14282,0)
 ;;=206.01^^87^776^5
 ;;^UTILITY(U,$J,358.3,14282,1,0)
 ;;=^358.31IA^8^2
 ;;^UTILITY(U,$J,358.3,14282,1,1,0)
 ;;=1^206.01
 ;;^UTILITY(U,$J,358.3,14282,1,8,0)
 ;;=8^Acute Monocytic Leuk in Remission
 ;;^UTILITY(U,$J,358.3,14282,2)
 ;;=^267544
 ;;^UTILITY(U,$J,358.3,14283,0)
 ;;=206.02^^87^776^4
 ;;^UTILITY(U,$J,358.3,14283,1,0)
 ;;=^358.31IA^8^2
 ;;^UTILITY(U,$J,358.3,14283,1,1,0)
 ;;=1^206.02
 ;;^UTILITY(U,$J,358.3,14283,1,8,0)
 ;;=8^Acute Monocytic Leuk in Relapse
 ;;^UTILITY(U,$J,358.3,14283,2)
 ;;=^336476
 ;;^UTILITY(U,$J,358.3,14284,0)
 ;;=206.10^^87^776^15
 ;;^UTILITY(U,$J,358.3,14284,1,0)
 ;;=^358.31IA^8^2
 ;;^UTILITY(U,$J,358.3,14284,1,1,0)
 ;;=1^206.10
 ;;^UTILITY(U,$J,358.3,14284,1,8,0)
 ;;=8^Chr Monocytic Leuk w/o Remission
 ;;^UTILITY(U,$J,358.3,14284,2)
 ;;=^336862
 ;;^UTILITY(U,$J,358.3,14285,0)
 ;;=206.11^^87^776^14
 ;;^UTILITY(U,$J,358.3,14285,1,0)
 ;;=^358.31IA^8^2
 ;;^UTILITY(U,$J,358.3,14285,1,1,0)
 ;;=1^206.11
 ;;^UTILITY(U,$J,358.3,14285,1,8,0)
 ;;=8^Chr Monocytic Leuk in Remission
 ;;^UTILITY(U,$J,358.3,14285,2)
 ;;=^267546
 ;;^UTILITY(U,$J,358.3,14286,0)
 ;;=206.12^^87^776^13
 ;;^UTILITY(U,$J,358.3,14286,1,0)
 ;;=^358.31IA^8^2
 ;;^UTILITY(U,$J,358.3,14286,1,1,0)
 ;;=1^206.12
 ;;^UTILITY(U,$J,358.3,14286,1,8,0)
 ;;=8^Chr Monocytic Leuk in Relapse
 ;;^UTILITY(U,$J,358.3,14286,2)
 ;;=^336477
 ;;^UTILITY(U,$J,358.3,14287,0)
 ;;=206.20^^87^776^55
 ;;^UTILITY(U,$J,358.3,14287,1,0)
 ;;=^358.31IA^8^2
 ;;^UTILITY(U,$J,358.3,14287,1,1,0)
 ;;=1^206.20
 ;;^UTILITY(U,$J,358.3,14287,1,8,0)
 ;;=8^Subacute Monocytic Leuk w/o Remiss
 ;;^UTILITY(U,$J,358.3,14287,2)
 ;;=^336863
 ;;^UTILITY(U,$J,358.3,14288,0)
 ;;=206.21^^87^776^54
 ;;^UTILITY(U,$J,358.3,14288,1,0)
 ;;=^358.31IA^8^2
 ;;^UTILITY(U,$J,358.3,14288,1,1,0)
 ;;=1^206.21
 ;;^UTILITY(U,$J,358.3,14288,1,8,0)
 ;;=8^Subacute Monocytic Leuk in Relapse
 ;;^UTILITY(U,$J,358.3,14288,2)
 ;;=^267548
 ;;^UTILITY(U,$J,358.3,14289,0)
 ;;=206.22^^87^776^56
 ;;^UTILITY(U,$J,358.3,14289,1,0)
 ;;=^358.31IA^8^2
 ;;^UTILITY(U,$J,358.3,14289,1,1,0)
 ;;=1^206.22
 ;;^UTILITY(U,$J,358.3,14289,1,8,0)
 ;;=8^Subacute Monocytic Leuk w/o Remiss
 ;;^UTILITY(U,$J,358.3,14289,2)
 ;;=^336478
 ;;^UTILITY(U,$J,358.3,14290,0)
 ;;=206.80^^87^776^41
 ;;^UTILITY(U,$J,358.3,14290,1,0)
 ;;=^358.31IA^8^2
 ;;^UTILITY(U,$J,358.3,14290,1,1,0)
 ;;=1^206.80
 ;;^UTILITY(U,$J,358.3,14290,1,8,0)
 ;;=8^Oth Monocytic Leuk w/ Remission
 ;;^UTILITY(U,$J,358.3,14290,2)
 ;;=^336864
 ;;^UTILITY(U,$J,358.3,14291,0)
 ;;=206.81^^87^776^40
 ;;^UTILITY(U,$J,358.3,14291,1,0)
 ;;=^358.31IA^8^2
 ;;^UTILITY(U,$J,358.3,14291,1,1,0)
 ;;=1^206.81
 ;;^UTILITY(U,$J,358.3,14291,1,8,0)
 ;;=8^Oth Monocytic Leuk w/ Remiss
 ;;^UTILITY(U,$J,358.3,14291,2)
 ;;=^267551
 ;;^UTILITY(U,$J,358.3,14292,0)
 ;;=206.82^^87^776^39
 ;;^UTILITY(U,$J,358.3,14292,1,0)
 ;;=^358.31IA^8^2
 ;;^UTILITY(U,$J,358.3,14292,1,1,0)
 ;;=1^206.82
 ;;^UTILITY(U,$J,358.3,14292,1,8,0)
 ;;=8^Oth Monocytic Leuk in Relapse
 ;;^UTILITY(U,$J,358.3,14292,2)
 ;;=^336479
 ;;^UTILITY(U,$J,358.3,14293,0)
 ;;=206.90^^87^776^62
 ;;^UTILITY(U,$J,358.3,14293,1,0)
 ;;=^358.31IA^8^2
 ;;^UTILITY(U,$J,358.3,14293,1,1,0)
 ;;=1^206.90
 ;;^UTILITY(U,$J,358.3,14293,1,8,0)
 ;;=8^Unspec Monocytic Leuk w/o Remiss
 ;;^UTILITY(U,$J,358.3,14293,2)
 ;;=^336865
 ;;^UTILITY(U,$J,358.3,14294,0)
 ;;=206.91^^87^776^61
 ;;^UTILITY(U,$J,358.3,14294,1,0)
 ;;=^358.31IA^8^2
 ;;^UTILITY(U,$J,358.3,14294,1,1,0)
 ;;=1^206.91
 ;;^UTILITY(U,$J,358.3,14294,1,8,0)
 ;;=8^Unspec Monocytic Leuk in Remiss
 ;;^UTILITY(U,$J,358.3,14294,2)
 ;;=^267554
 ;;^UTILITY(U,$J,358.3,14295,0)
 ;;=206.92^^87^776^60
 ;;^UTILITY(U,$J,358.3,14295,1,0)
 ;;=^358.31IA^8^2
 ;;^UTILITY(U,$J,358.3,14295,1,1,0)
 ;;=1^206.92
 ;;^UTILITY(U,$J,358.3,14295,1,8,0)
 ;;=8^Unspec Monocytic Leuk in Relapse
 ;;^UTILITY(U,$J,358.3,14295,2)
 ;;=^336480
 ;;^UTILITY(U,$J,358.3,14296,0)
 ;;=207.00^^87^776^3
 ;;^UTILITY(U,$J,358.3,14296,1,0)
 ;;=^358.31IA^8^2
 ;;^UTILITY(U,$J,358.3,14296,1,1,0)
 ;;=1^207.00
 ;;^UTILITY(U,$J,358.3,14296,1,8,0)
 ;;=8^AC Eryth & Erythroleuk w/o Remiss
 ;;^UTILITY(U,$J,358.3,14296,2)
 ;;=^336866
 ;;^UTILITY(U,$J,358.3,14297,0)
 ;;=207.01^^87^776^1
 ;;^UTILITY(U,$J,358.3,14297,1,0)
 ;;=^358.31IA^8^2
 ;;^UTILITY(U,$J,358.3,14297,1,1,0)
 ;;=1^207.01
 ;;^UTILITY(U,$J,358.3,14297,1,8,0)
 ;;=8^AC Eryth & Erythroleuk in Relapse
 ;;^UTILITY(U,$J,358.3,14297,2)
 ;;=^267556
 ;;^UTILITY(U,$J,358.3,14298,0)
 ;;=207.02^^87^776^2
 ;;^UTILITY(U,$J,358.3,14298,1,0)
 ;;=^358.31IA^8^2
 ;;^UTILITY(U,$J,358.3,14298,1,1,0)
 ;;=1^207.02
 ;;^UTILITY(U,$J,358.3,14298,1,8,0)
 ;;=8^AC Eryth & Erythroleuk in Relapse
 ;;^UTILITY(U,$J,358.3,14298,2)
 ;;=^336481
 ;;^UTILITY(U,$J,358.3,14299,0)
 ;;=207.10^^87^776^9
 ;;^UTILITY(U,$J,358.3,14299,1,0)
 ;;=^358.31IA^8^2
 ;;^UTILITY(U,$J,358.3,14299,1,1,0)
 ;;=1^207.10
 ;;^UTILITY(U,$J,358.3,14299,1,8,0)
 ;;=8^Chr Erythremia w/o Remission
 ;;^UTILITY(U,$J,358.3,14299,2)
 ;;=^336867
 ;;^UTILITY(U,$J,358.3,14300,0)
 ;;=207.11^^87^776^8
 ;;^UTILITY(U,$J,358.3,14300,1,0)
 ;;=^358.31IA^8^2
 ;;^UTILITY(U,$J,358.3,14300,1,1,0)
 ;;=1^207.11
 ;;^UTILITY(U,$J,358.3,14300,1,8,0)
 ;;=8^Chr Erythremia in Remission
 ;;^UTILITY(U,$J,358.3,14300,2)
 ;;=^267558
 ;;^UTILITY(U,$J,358.3,14301,0)
 ;;=207.12^^87^776^7
 ;;^UTILITY(U,$J,358.3,14301,1,0)
 ;;=^358.31IA^8^2
 ;;^UTILITY(U,$J,358.3,14301,1,1,0)
 ;;=1^207.12
 ;;^UTILITY(U,$J,358.3,14301,1,8,0)
 ;;=8^Chr Erythremia in Relapse
 ;;^UTILITY(U,$J,358.3,14301,2)
 ;;=^336482
 ;;^UTILITY(U,$J,358.3,14302,0)
 ;;=207.20^^87^776^28
 ;;^UTILITY(U,$J,358.3,14302,1,0)
 ;;=^358.31IA^8^2
 ;;^UTILITY(U,$J,358.3,14302,1,1,0)
 ;;=1^207.20
 ;;^UTILITY(U,$J,358.3,14302,1,8,0)
 ;;=8^Megakaryocytic Leuk w/o Remission
 ;;^UTILITY(U,$J,358.3,14302,2)
 ;;=^336868
 ;;^UTILITY(U,$J,358.3,14303,0)
 ;;=207.21^^87^776^27
 ;;^UTILITY(U,$J,358.3,14303,1,0)
 ;;=^358.31IA^8^2
 ;;^UTILITY(U,$J,358.3,14303,1,1,0)
 ;;=1^207.21
 ;;^UTILITY(U,$J,358.3,14303,1,8,0)
 ;;=8^Megakaryocytic Leuk in Remission
 ;;^UTILITY(U,$J,358.3,14303,2)
 ;;=^267560
 ;;^UTILITY(U,$J,358.3,14304,0)
 ;;=207.22^^87^776^26
 ;;^UTILITY(U,$J,358.3,14304,1,0)
 ;;=^358.31IA^8^2
 ;;^UTILITY(U,$J,358.3,14304,1,1,0)
 ;;=1^207.22
 ;;^UTILITY(U,$J,358.3,14304,1,8,0)
 ;;=8^Megakaryocytic Leuk in Relapse
 ;;^UTILITY(U,$J,358.3,14304,2)
 ;;=^336483
 ;;^UTILITY(U,$J,358.3,14305,0)
 ;;=207.80^^87^776^47
 ;;^UTILITY(U,$J,358.3,14305,1,0)
 ;;=^358.31IA^8^2
 ;;^UTILITY(U,$J,358.3,14305,1,1,0)
 ;;=1^207.80
 ;;^UTILITY(U,$J,358.3,14305,1,8,0)
 ;;=8^Oth Spec Leukemia w/o Remission
 ;;^UTILITY(U,$J,358.3,14305,2)
 ;;=^336869
 ;;^UTILITY(U,$J,358.3,14306,0)
 ;;=207.81^^87^776^46
 ;;^UTILITY(U,$J,358.3,14306,1,0)
 ;;=^358.31IA^8^2
 ;;^UTILITY(U,$J,358.3,14306,1,1,0)
 ;;=1^207.81
 ;;^UTILITY(U,$J,358.3,14306,1,8,0)
 ;;=8^Oth Spec Leukemia in Remission
 ;;^UTILITY(U,$J,358.3,14306,2)
 ;;=^267562
 ;;^UTILITY(U,$J,358.3,14307,0)
 ;;=207.82^^87^776^45
 ;;^UTILITY(U,$J,358.3,14307,1,0)
 ;;=^358.31IA^8^2
 ;;^UTILITY(U,$J,358.3,14307,1,1,0)
 ;;=1^207.82
 ;;^UTILITY(U,$J,358.3,14307,1,8,0)
 ;;=8^Oth Spec Leukemia in Relapse
 ;;^UTILITY(U,$J,358.3,14307,2)
 ;;=^336484
 ;;^UTILITY(U,$J,358.3,14308,0)
 ;;=208.00^^87^776^21
 ;;^UTILITY(U,$J,358.3,14308,1,0)
 ;;=^358.31IA^8^2
 ;;^UTILITY(U,$J,358.3,14308,1,1,0)
 ;;=1^208.00
 ;;^UTILITY(U,$J,358.3,14308,1,8,0)
 ;;=8^Leukemia Unspec w/o Remission
 ;;^UTILITY(U,$J,358.3,14308,2)
 ;;=^336870
 ;;^UTILITY(U,$J,358.3,14309,0)
 ;;=208.01^^87^776^20
 ;;^UTILITY(U,$J,358.3,14309,1,0)
 ;;=^358.31IA^8^2
 ;;^UTILITY(U,$J,358.3,14309,1,1,0)
 ;;=1^208.01
 ;;^UTILITY(U,$J,358.3,14309,1,8,0)
 ;;=8^Leukemia Unspec in Remission
 ;;^UTILITY(U,$J,358.3,14309,2)
 ;;=^267566
 ;;^UTILITY(U,$J,358.3,14310,0)
 ;;=208.02^^87^776^19
 ;;^UTILITY(U,$J,358.3,14310,1,0)
 ;;=^358.31IA^8^2
 ;;^UTILITY(U,$J,358.3,14310,1,1,0)
 ;;=1^208.02
 ;;^UTILITY(U,$J,358.3,14310,1,8,0)
 ;;=8^Leukemia Unspec in Relapse
 ;;^UTILITY(U,$J,358.3,14310,2)
 ;;=^336485
 ;;^UTILITY(U,$J,358.3,14311,0)
 ;;=208.10^^87^776^12
 ;;^UTILITY(U,$J,358.3,14311,1,0)
 ;;=^358.31IA^8^2
 ;;^UTILITY(U,$J,358.3,14311,1,1,0)
 ;;=1^208.10
 ;;^UTILITY(U,$J,358.3,14311,1,8,0)
 ;;=8^Chr Leukemia w/o Remission
 ;;^UTILITY(U,$J,358.3,14311,2)
 ;;=^336871
 ;;^UTILITY(U,$J,358.3,14312,0)
 ;;=208.11^^87^776^11
 ;;^UTILITY(U,$J,358.3,14312,1,0)
 ;;=^358.31IA^8^2
 ;;^UTILITY(U,$J,358.3,14312,1,1,0)
 ;;=1^208.11
 ;;^UTILITY(U,$J,358.3,14312,1,8,0)
 ;;=8^Chr Leukemia in Remission
 ;;^UTILITY(U,$J,358.3,14312,2)
 ;;=^267569
 ;;^UTILITY(U,$J,358.3,14313,0)
 ;;=208.12^^87^776^10
 ;;^UTILITY(U,$J,358.3,14313,1,0)
 ;;=^358.31IA^8^2
 ;;^UTILITY(U,$J,358.3,14313,1,1,0)
 ;;=1^208.12
 ;;^UTILITY(U,$J,358.3,14313,1,8,0)
 ;;=8^Chr Leukemia in Relapse
 ;;^UTILITY(U,$J,358.3,14313,2)
 ;;=^336486
 ;;^UTILITY(U,$J,358.3,14314,0)
 ;;=208.20^^87^776^53
 ;;^UTILITY(U,$J,358.3,14314,1,0)
 ;;=^358.31IA^8^2
 ;;^UTILITY(U,$J,358.3,14314,1,1,0)
 ;;=1^208.20
 ;;^UTILITY(U,$J,358.3,14314,1,8,0)
 ;;=8^Subacute Leukemia w/o Remission
 ;;^UTILITY(U,$J,358.3,14314,2)
 ;;=^336872
 ;;^UTILITY(U,$J,358.3,14315,0)
 ;;=208.21^^87^776^52
 ;;^UTILITY(U,$J,358.3,14315,1,0)
 ;;=^358.31IA^8^2
 ;;^UTILITY(U,$J,358.3,14315,1,1,0)
 ;;=1^208.21
 ;;^UTILITY(U,$J,358.3,14315,1,8,0)
 ;;=8^Subacute Leukemia in Remission
 ;;^UTILITY(U,$J,358.3,14315,2)
 ;;=^267572
 ;;^UTILITY(U,$J,358.3,14316,0)
 ;;=208.22^^87^776^51
 ;;^UTILITY(U,$J,358.3,14316,1,0)
 ;;=^358.31IA^8^2
 ;;^UTILITY(U,$J,358.3,14316,1,1,0)
 ;;=1^208.22
 ;;^UTILITY(U,$J,358.3,14316,1,8,0)
 ;;=8^Subacute Leukemia in Relapse
 ;;^UTILITY(U,$J,358.3,14316,2)
 ;;=^336487
 ;;^UTILITY(U,$J,358.3,14317,0)
 ;;=208.80^^87^776^38
 ;;^UTILITY(U,$J,358.3,14317,1,0)
 ;;=^358.31IA^8^2
 ;;^UTILITY(U,$J,358.3,14317,1,1,0)
 ;;=1^208.80
