IBDEI2OM ; ; 19-NOV-2015
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;JUN 29, 2015
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,45006,2)
 ;;=^5003776
 ;;^UTILITY(U,$J,358.3,45007,0)
 ;;=G20.^^200^2235^32
 ;;^UTILITY(U,$J,358.3,45007,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,45007,1,3,0)
 ;;=3^Parkinson's Disease
 ;;^UTILITY(U,$J,358.3,45007,1,4,0)
 ;;=4^G20.
 ;;^UTILITY(U,$J,358.3,45007,2)
 ;;=^5003770
 ;;^UTILITY(U,$J,358.3,45008,0)
 ;;=G25.0^^200^2235^15
 ;;^UTILITY(U,$J,358.3,45008,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,45008,1,3,0)
 ;;=3^Essential Tremor
 ;;^UTILITY(U,$J,358.3,45008,1,4,0)
 ;;=4^G25.0
 ;;^UTILITY(U,$J,358.3,45008,2)
 ;;=^5003791
 ;;^UTILITY(U,$J,358.3,45009,0)
 ;;=G25.2^^200^2235^39
 ;;^UTILITY(U,$J,358.3,45009,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,45009,1,3,0)
 ;;=3^Tremor,Other Spec Forms
 ;;^UTILITY(U,$J,358.3,45009,1,4,0)
 ;;=4^G25.2
 ;;^UTILITY(U,$J,358.3,45009,2)
 ;;=^5003793
 ;;^UTILITY(U,$J,358.3,45010,0)
 ;;=G25.1^^200^2235^38
 ;;^UTILITY(U,$J,358.3,45010,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,45010,1,3,0)
 ;;=3^Tremor,Drug-Induced
 ;;^UTILITY(U,$J,358.3,45010,1,4,0)
 ;;=4^G25.1
 ;;^UTILITY(U,$J,358.3,45010,2)
 ;;=^5003792
 ;;^UTILITY(U,$J,358.3,45011,0)
 ;;=G25.81^^200^2235^36
 ;;^UTILITY(U,$J,358.3,45011,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,45011,1,3,0)
 ;;=3^Restless Legs Syndrome
 ;;^UTILITY(U,$J,358.3,45011,1,4,0)
 ;;=4^G25.81
 ;;^UTILITY(U,$J,358.3,45011,2)
 ;;=^5003801
 ;;^UTILITY(U,$J,358.3,45012,0)
 ;;=G35.^^200^2235^29
 ;;^UTILITY(U,$J,358.3,45012,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,45012,1,3,0)
 ;;=3^Multiple Sclerosis
 ;;^UTILITY(U,$J,358.3,45012,1,4,0)
 ;;=4^G35.
 ;;^UTILITY(U,$J,358.3,45012,2)
 ;;=^79761
 ;;^UTILITY(U,$J,358.3,45013,0)
 ;;=G40.901^^200^2235^13
 ;;^UTILITY(U,$J,358.3,45013,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,45013,1,3,0)
 ;;=3^Epilepsy,Not Intractable w/ Status Epilepticus,Unspec
 ;;^UTILITY(U,$J,358.3,45013,1,4,0)
 ;;=4^G40.901
 ;;^UTILITY(U,$J,358.3,45013,2)
 ;;=^5003864
 ;;^UTILITY(U,$J,358.3,45014,0)
 ;;=G40.909^^200^2235^14
 ;;^UTILITY(U,$J,358.3,45014,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,45014,1,3,0)
 ;;=3^Epilepsy,Not Intractable w/o Status Epilepticus,Unspec
 ;;^UTILITY(U,$J,358.3,45014,1,4,0)
 ;;=4^G40.909
 ;;^UTILITY(U,$J,358.3,45014,2)
 ;;=^5003865
 ;;^UTILITY(U,$J,358.3,45015,0)
 ;;=G43.809^^200^2235^26
 ;;^UTILITY(U,$J,358.3,45015,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,45015,1,3,0)
 ;;=3^Migraine,Not Intractable w/o Status Migrainosus,Other
 ;;^UTILITY(U,$J,358.3,45015,1,4,0)
 ;;=4^G43.809
 ;;^UTILITY(U,$J,358.3,45015,2)
 ;;=^5003901
 ;;^UTILITY(U,$J,358.3,45016,0)
 ;;=G43.B0^^200^2235^28
 ;;^UTILITY(U,$J,358.3,45016,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,45016,1,3,0)
 ;;=3^Migraine,Ophthalmoplegic Not Intractable
 ;;^UTILITY(U,$J,358.3,45016,1,4,0)
 ;;=4^G43.B0
 ;;^UTILITY(U,$J,358.3,45016,2)
 ;;=^5003914
 ;;^UTILITY(U,$J,358.3,45017,0)
 ;;=G43.C0^^200^2235^17
 ;;^UTILITY(U,$J,358.3,45017,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,45017,1,3,0)
 ;;=3^Headache Syndromes,Periodic Not Intractable
 ;;^UTILITY(U,$J,358.3,45017,1,4,0)
 ;;=4^G43.C0
 ;;^UTILITY(U,$J,358.3,45017,2)
 ;;=^5003916
 ;;^UTILITY(U,$J,358.3,45018,0)
 ;;=G43.A0^^200^2235^8
 ;;^UTILITY(U,$J,358.3,45018,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,45018,1,3,0)
 ;;=3^Cyclical Vomiting Not Intractable
 ;;^UTILITY(U,$J,358.3,45018,1,4,0)
 ;;=4^G43.A0
 ;;^UTILITY(U,$J,358.3,45018,2)
 ;;=^5003912
 ;;^UTILITY(U,$J,358.3,45019,0)
 ;;=G43.C1^^200^2235^16
 ;;^UTILITY(U,$J,358.3,45019,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,45019,1,3,0)
 ;;=3^Headache Syndromes,Periodic Intractable
 ;;^UTILITY(U,$J,358.3,45019,1,4,0)
 ;;=4^G43.C1
