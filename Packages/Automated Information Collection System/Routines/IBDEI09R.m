IBDEI09R ; ; 06-AUG-2013
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;AUG 06, 2013
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,12978,1,5,0)
 ;;=5^Thrombophlebitis 
 ;;^UTILITY(U,$J,358.3,12978,2)
 ;;=^93357
 ;;^UTILITY(U,$J,358.3,12979,0)
 ;;=446.6^^80^701^139
 ;;^UTILITY(U,$J,358.3,12979,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,12979,1,4,0)
 ;;=4^446.6
 ;;^UTILITY(U,$J,358.3,12979,1,5,0)
 ;;=5^Thrombotic Thrombocytopenic Purpura(TTP)
 ;;^UTILITY(U,$J,358.3,12979,2)
 ;;=^119061
 ;;^UTILITY(U,$J,358.3,12980,0)
 ;;=286.4^^80^701^143
 ;;^UTILITY(U,$J,358.3,12980,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,12980,1,4,0)
 ;;=4^286.4
 ;;^UTILITY(U,$J,358.3,12980,1,5,0)
 ;;=5^Von Willebrand's Disease
 ;;^UTILITY(U,$J,358.3,12980,2)
 ;;=^127267
 ;;^UTILITY(U,$J,358.3,12981,0)
 ;;=204.00^^80^701^17
 ;;^UTILITY(U,$J,358.3,12981,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,12981,1,4,0)
 ;;=4^204.00
 ;;^UTILITY(U,$J,358.3,12981,1,5,0)
 ;;=5^All, W/O Remission
 ;;^UTILITY(U,$J,358.3,12981,2)
 ;;=^267521
 ;;^UTILITY(U,$J,358.3,12982,0)
 ;;=204.01^^80^701^16
 ;;^UTILITY(U,$J,358.3,12982,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,12982,1,4,0)
 ;;=4^204.01
 ;;^UTILITY(U,$J,358.3,12982,1,5,0)
 ;;=5^All, In Remission
 ;;^UTILITY(U,$J,358.3,12982,2)
 ;;=^267522
 ;;^UTILITY(U,$J,358.3,12983,0)
 ;;=204.10^^80^701^61
 ;;^UTILITY(U,$J,358.3,12983,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,12983,1,4,0)
 ;;=4^204.10
 ;;^UTILITY(U,$J,358.3,12983,1,5,0)
 ;;=5^Cll, W/O Remission
 ;;^UTILITY(U,$J,358.3,12983,2)
 ;;=^267523
 ;;^UTILITY(U,$J,358.3,12984,0)
 ;;=204.11^^80^701^60
 ;;^UTILITY(U,$J,358.3,12984,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,12984,1,4,0)
 ;;=4^204.11
 ;;^UTILITY(U,$J,358.3,12984,1,5,0)
 ;;=5^Cll, In Remission
 ;;^UTILITY(U,$J,358.3,12984,2)
 ;;=^267524
 ;;^UTILITY(U,$J,358.3,12985,0)
 ;;=202.41^^80^701^87
 ;;^UTILITY(U,$J,358.3,12985,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,12985,1,4,0)
 ;;=4^202.41
 ;;^UTILITY(U,$J,358.3,12985,1,5,0)
 ;;=5^Hairy Cell Leukemia,Unspec Site
 ;;^UTILITY(U,$J,358.3,12985,2)
 ;;=^267472
 ;;^UTILITY(U,$J,358.3,12986,0)
 ;;=201.90^^80^701^94
 ;;^UTILITY(U,$J,358.3,12986,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,12986,1,4,0)
 ;;=4^201.90
 ;;^UTILITY(U,$J,358.3,12986,1,5,0)
 ;;=5^Hodgkin's Lymphoma,Unspec Type & Site
 ;;^UTILITY(U,$J,358.3,12986,2)
 ;;=^267430
 ;;^UTILITY(U,$J,358.3,12987,0)
 ;;=785.6^^80^701^101
 ;;^UTILITY(U,$J,358.3,12987,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,12987,1,4,0)
 ;;=4^785.6
 ;;^UTILITY(U,$J,358.3,12987,1,5,0)
 ;;=5^Lymphadenopathy
 ;;^UTILITY(U,$J,358.3,12987,2)
 ;;=^72368
 ;;^UTILITY(U,$J,358.3,12988,0)
 ;;=200.20^^80^701^102
 ;;^UTILITY(U,$J,358.3,12988,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,12988,1,4,0)
 ;;=4^200.20
 ;;^UTILITY(U,$J,358.3,12988,1,5,0)
 ;;=5^Lymphoma,Burkett's,Unspecified Sites
 ;;^UTILITY(U,$J,358.3,12988,2)
 ;;=^17529
 ;;^UTILITY(U,$J,358.3,12989,0)
 ;;=202.00^^80^701^104
 ;;^UTILITY(U,$J,358.3,12989,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,12989,1,4,0)
 ;;=4^202.00
 ;;^UTILITY(U,$J,358.3,12989,1,5,0)
 ;;=5^Lymphoma, Low-Grade, Unspec Site
 ;;^UTILITY(U,$J,358.3,12989,2)
 ;;=^72606
 ;;^UTILITY(U,$J,358.3,12990,0)
 ;;=200.10^^80^701^103
 ;;^UTILITY(U,$J,358.3,12990,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,12990,1,4,0)
 ;;=4^200.10
 ;;^UTILITY(U,$J,358.3,12990,1,5,0)
 ;;=5^Lymphoma, Int Or High Grade, Unspec Site
 ;;^UTILITY(U,$J,358.3,12990,2)
 ;;=^175886
 ;;^UTILITY(U,$J,358.3,12991,0)
 ;;=273.3^^80^701^105
 ;;^UTILITY(U,$J,358.3,12991,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,12991,1,4,0)
 ;;=4^273.3
 ;;^UTILITY(U,$J,358.3,12991,1,5,0)
 ;;=5^Macroglobulinemia
 ;;^UTILITY(U,$J,358.3,12991,2)
 ;;=^73013
 ;;^UTILITY(U,$J,358.3,12992,0)
 ;;=203.00^^80^701^115
 ;;^UTILITY(U,$J,358.3,12992,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,12992,1,4,0)
 ;;=4^203.00
 ;;^UTILITY(U,$J,358.3,12992,1,5,0)
 ;;=5^Multiple Myeloma W/O Rem
 ;;^UTILITY(U,$J,358.3,12992,2)
 ;;=^267514
 ;;^UTILITY(U,$J,358.3,12993,0)
 ;;=203.01^^80^701^114
 ;;^UTILITY(U,$J,358.3,12993,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,12993,1,4,0)
 ;;=4^203.01
 ;;^UTILITY(U,$J,358.3,12993,1,5,0)
 ;;=5^Multiple Myeloma In Remission
 ;;^UTILITY(U,$J,358.3,12993,2)
 ;;=^267515
 ;;^UTILITY(U,$J,358.3,12994,0)
 ;;=238.6^^80^701^118
 ;;^UTILITY(U,$J,358.3,12994,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,12994,1,4,0)
 ;;=4^238.6
 ;;^UTILITY(U,$J,358.3,12994,1,5,0)
 ;;=5^Plasmacytoma Nos
 ;;^UTILITY(U,$J,358.3,12994,2)
 ;;=^81973
 ;;^UTILITY(U,$J,358.3,12995,0)
 ;;=205.00^^80^701^19
 ;;^UTILITY(U,$J,358.3,12995,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,12995,1,4,0)
 ;;=4^205.00
 ;;^UTILITY(U,$J,358.3,12995,1,5,0)
 ;;=5^Aml, W/O Remission
 ;;^UTILITY(U,$J,358.3,12995,2)
 ;;=^267531
 ;;^UTILITY(U,$J,358.3,12996,0)
 ;;=205.01^^80^701^18
 ;;^UTILITY(U,$J,358.3,12996,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,12996,1,4,0)
 ;;=4^205.01
 ;;^UTILITY(U,$J,358.3,12996,1,5,0)
 ;;=5^Aml, In Remission
 ;;^UTILITY(U,$J,358.3,12996,2)
 ;;=^267532
 ;;^UTILITY(U,$J,358.3,12997,0)
 ;;=205.10^^80^701^63
 ;;^UTILITY(U,$J,358.3,12997,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,12997,1,4,0)
 ;;=4^205.10
 ;;^UTILITY(U,$J,358.3,12997,1,5,0)
 ;;=5^Cml, W/O Remission
 ;;^UTILITY(U,$J,358.3,12997,2)
 ;;=^267533
 ;;^UTILITY(U,$J,358.3,12998,0)
 ;;=205.11^^80^701^62
 ;;^UTILITY(U,$J,358.3,12998,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,12998,1,4,0)
 ;;=4^205.11
 ;;^UTILITY(U,$J,358.3,12998,1,5,0)
 ;;=5^Cml, In Remission
 ;;^UTILITY(U,$J,358.3,12998,2)
 ;;=^267534
 ;;^UTILITY(U,$J,358.3,12999,0)
 ;;=289.0^^80^701^65
 ;;^UTILITY(U,$J,358.3,12999,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,12999,1,4,0)
 ;;=4^289.0
 ;;^UTILITY(U,$J,358.3,12999,1,5,0)
 ;;=5^Erthryocytosis, Secondary
 ;;^UTILITY(U,$J,358.3,12999,2)
 ;;=^186856
 ;;^UTILITY(U,$J,358.3,13000,0)
 ;;=238.4^^80^701^119
 ;;^UTILITY(U,$J,358.3,13000,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13000,1,4,0)
 ;;=4^238.4
 ;;^UTILITY(U,$J,358.3,13000,1,5,0)
 ;;=5^Polycytheria Rubra Vera
 ;;^UTILITY(U,$J,358.3,13000,2)
 ;;=^96105
 ;;^UTILITY(U,$J,358.3,13001,0)
 ;;=V58.61^^80^701^144
 ;;^UTILITY(U,$J,358.3,13001,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13001,1,4,0)
 ;;=4^V58.61
 ;;^UTILITY(U,$J,358.3,13001,1,5,0)
 ;;=5^Warfarin/Coumadin Use
 ;;^UTILITY(U,$J,358.3,13001,2)
 ;;=Warfarin/Coumadin Use^303459
 ;;^UTILITY(U,$J,358.3,13002,0)
 ;;=282.49^^80^701^134
 ;;^UTILITY(U,$J,358.3,13002,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13002,1,4,0)
 ;;=4^282.49
 ;;^UTILITY(U,$J,358.3,13002,1,5,0)
 ;;=5^Thalassemia Nec
 ;;^UTILITY(U,$J,358.3,13002,2)
 ;;=^329910
 ;;^UTILITY(U,$J,358.3,13003,0)
 ;;=289.89^^80^701^29
 ;;^UTILITY(U,$J,358.3,13003,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13003,1,4,0)
 ;;=4^289.89
 ;;^UTILITY(U,$J,358.3,13003,1,5,0)
 ;;=5^Blood Diseases Nec
 ;;^UTILITY(U,$J,358.3,13003,2)
 ;;=^329887
 ;;^UTILITY(U,$J,358.3,13004,0)
 ;;=238.79^^80^701^100
 ;;^UTILITY(U,$J,358.3,13004,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13004,1,4,0)
 ;;=4^238.79
 ;;^UTILITY(U,$J,358.3,13004,1,5,0)
 ;;=5^Lymph/Hematpoitc Tis Nec
 ;;^UTILITY(U,$J,358.3,13004,2)
 ;;=^334033
 ;;^UTILITY(U,$J,358.3,13005,0)
 ;;=287.30^^80^701^120
 ;;^UTILITY(U,$J,358.3,13005,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13005,1,4,0)
 ;;=4^287.30
 ;;^UTILITY(U,$J,358.3,13005,1,5,0)
 ;;=5^Primary Thrombocytopenia
 ;;^UTILITY(U,$J,358.3,13005,2)
 ;;=^332841
 ;;^UTILITY(U,$J,358.3,13006,0)
 ;;=288.09^^80^701^15
 ;;^UTILITY(U,$J,358.3,13006,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13006,1,4,0)
 ;;=4^288.09
 ;;^UTILITY(U,$J,358.3,13006,1,5,0)
 ;;=5^Agranulocytosis
 ;;^UTILITY(U,$J,358.3,13006,2)
 ;;=^334042
 ;;^UTILITY(U,$J,358.3,13007,0)
 ;;=284.81^^80^701^26
 ;;^UTILITY(U,$J,358.3,13007,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13007,1,4,0)
 ;;=4^284.81
 ;;^UTILITY(U,$J,358.3,13007,1,5,0)
 ;;=5^Aplastic Due To Drugs
 ;;^UTILITY(U,$J,358.3,13007,2)
 ;;=^335245
 ;;^UTILITY(U,$J,358.3,13008,0)
 ;;=284.89^^80^701^25
 ;;^UTILITY(U,$J,358.3,13008,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13008,1,4,0)
 ;;=4^284.89
 ;;^UTILITY(U,$J,358.3,13008,1,5,0)
 ;;=5^Aplastic Anemia due to Chr disease
 ;;^UTILITY(U,$J,358.3,13008,2)
 ;;=^87880
 ;;^UTILITY(U,$J,358.3,13009,0)
 ;;=284.9^^80^701^23
 ;;^UTILITY(U,$J,358.3,13009,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13009,1,4,0)
 ;;=4^284.9
 ;;^UTILITY(U,$J,358.3,13009,1,5,0)
 ;;=5^Aplastic Anemia NOS
 ;;^UTILITY(U,$J,358.3,13009,2)
 ;;=^7020
 ;;^UTILITY(U,$J,358.3,13010,0)
 ;;=289.84^^80^701^136
 ;;^UTILITY(U,$J,358.3,13010,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13010,1,4,0)
 ;;=4^289.84
 ;;^UTILITY(U,$J,358.3,13010,1,5,0)
 ;;=5^Thrombocytopenia,Heparin Induced
 ;;^UTILITY(U,$J,358.3,13010,2)
 ;;=^336542
 ;;^UTILITY(U,$J,358.3,13011,0)
 ;;=204.02^^80^701^12
 ;;^UTILITY(U,$J,358.3,13011,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13011,1,4,0)
 ;;=4^204.02
 ;;^UTILITY(U,$J,358.3,13011,1,5,0)
 ;;=5^ALL, In Relapse
 ;;^UTILITY(U,$J,358.3,13011,2)
 ;;=^336465
 ;;^UTILITY(U,$J,358.3,13012,0)
 ;;=204.12^^80^701^35
 ;;^UTILITY(U,$J,358.3,13012,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13012,1,4,0)
 ;;=4^204.12
 ;;^UTILITY(U,$J,358.3,13012,1,5,0)
 ;;=5^CLL, In Remission
 ;;^UTILITY(U,$J,358.3,13012,2)
 ;;=^336466
 ;;^UTILITY(U,$J,358.3,13013,0)
 ;;=204.21^^80^701^129
 ;;^UTILITY(U,$J,358.3,13013,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13013,1,4,0)
 ;;=4^204.21
 ;;^UTILITY(U,$J,358.3,13013,1,5,0)
 ;;=5^Subacute LL in Remission
 ;;^UTILITY(U,$J,358.3,13013,2)
 ;;=^267526
 ;;^UTILITY(U,$J,358.3,13014,0)
 ;;=204.20^^80^701^130
 ;;^UTILITY(U,$J,358.3,13014,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13014,1,4,0)
 ;;=4^204.20
 ;;^UTILITY(U,$J,358.3,13014,1,5,0)
 ;;=5^Subacute LL w/o Remission
 ;;^UTILITY(U,$J,358.3,13014,2)
 ;;=^336852
 ;;^UTILITY(U,$J,358.3,13015,0)
 ;;=204.22^^80^701^131
 ;;^UTILITY(U,$J,358.3,13015,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,13015,1,4,0)
 ;;=4^204.22
