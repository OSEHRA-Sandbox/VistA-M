IBDEI0PJ ; ; 17-FEB-2016
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;JUN 29, 2015
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,11938,1,3,0)
 ;;=3^F/U Exam After Treatment Encounter
 ;;^UTILITY(U,$J,358.3,11938,1,4,0)
 ;;=4^Z09.
 ;;^UTILITY(U,$J,358.3,11938,2)
 ;;=^5062668
 ;;^UTILITY(U,$J,358.3,11939,0)
 ;;=Z00.01^^47^538^60
 ;;^UTILITY(U,$J,358.3,11939,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,11939,1,3,0)
 ;;=3^General Medical Exam w/ Abnormal Findings
 ;;^UTILITY(U,$J,358.3,11939,1,4,0)
 ;;=4^Z00.01
 ;;^UTILITY(U,$J,358.3,11939,2)
 ;;=^5062600
 ;;^UTILITY(U,$J,358.3,11940,0)
 ;;=Z00.00^^47^538^61
 ;;^UTILITY(U,$J,358.3,11940,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,11940,1,3,0)
 ;;=3^General Medical Exam w/o Abnormal Findings
 ;;^UTILITY(U,$J,358.3,11940,1,4,0)
 ;;=4^Z00.00
 ;;^UTILITY(U,$J,358.3,11940,2)
 ;;=^5062599
 ;;^UTILITY(U,$J,358.3,11941,0)
 ;;=Z23.^^47^538^69
 ;;^UTILITY(U,$J,358.3,11941,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,11941,1,3,0)
 ;;=3^Immunization Encounter
 ;;^UTILITY(U,$J,358.3,11941,1,4,0)
 ;;=4^Z23.
 ;;^UTILITY(U,$J,358.3,11941,2)
 ;;=^5062795
 ;;^UTILITY(U,$J,358.3,11942,0)
 ;;=Z03.89^^47^538^81
 ;;^UTILITY(U,$J,358.3,11942,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,11942,1,3,0)
 ;;=3^Observation for Suspected Diseases/Ruled Out Conditions
 ;;^UTILITY(U,$J,358.3,11942,1,4,0)
 ;;=4^Z03.89
 ;;^UTILITY(U,$J,358.3,11942,2)
 ;;=^5062656
 ;;^UTILITY(U,$J,358.3,11943,0)
 ;;=Z04.9^^47^538^82
 ;;^UTILITY(U,$J,358.3,11943,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,11943,1,3,0)
 ;;=3^Observation/Exam,Unspec Reason
 ;;^UTILITY(U,$J,358.3,11943,1,4,0)
 ;;=4^Z04.9
 ;;^UTILITY(U,$J,358.3,11943,2)
 ;;=^5062666
 ;;^UTILITY(U,$J,358.3,11944,0)
 ;;=Z51.5^^47^538^94
 ;;^UTILITY(U,$J,358.3,11944,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,11944,1,3,0)
 ;;=3^Palliative Care Encounter
 ;;^UTILITY(U,$J,358.3,11944,1,4,0)
 ;;=4^Z51.5
 ;;^UTILITY(U,$J,358.3,11944,2)
 ;;=^5063063
 ;;^UTILITY(U,$J,358.3,11945,0)
 ;;=Z12.9^^47^538^162
 ;;^UTILITY(U,$J,358.3,11945,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,11945,1,3,0)
 ;;=3^Screening for Malig Neop,Unspec Site
 ;;^UTILITY(U,$J,358.3,11945,1,4,0)
 ;;=4^Z12.9
 ;;^UTILITY(U,$J,358.3,11945,2)
 ;;=^5062698
 ;;^UTILITY(U,$J,358.3,11946,0)
 ;;=Z13.9^^47^538^163
 ;;^UTILITY(U,$J,358.3,11946,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,11946,1,3,0)
 ;;=3^Screening,Unspec
 ;;^UTILITY(U,$J,358.3,11946,1,4,0)
 ;;=4^Z13.9
 ;;^UTILITY(U,$J,358.3,11946,2)
 ;;=^5062721
 ;;^UTILITY(U,$J,358.3,11947,0)
 ;;=Z48.89^^47^538^164
 ;;^UTILITY(U,$J,358.3,11947,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,11947,1,3,0)
 ;;=3^Surgical Aftercare Encounter,Other Spec
 ;;^UTILITY(U,$J,358.3,11947,1,4,0)
 ;;=4^Z48.89
 ;;^UTILITY(U,$J,358.3,11947,2)
 ;;=^5063055
 ;;^UTILITY(U,$J,358.3,11948,0)
 ;;=Z51.81^^47^538^167
 ;;^UTILITY(U,$J,358.3,11948,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,11948,1,3,0)
 ;;=3^Therapeutic Drug Level Monitoring
 ;;^UTILITY(U,$J,358.3,11948,1,4,0)
 ;;=4^Z51.81
 ;;^UTILITY(U,$J,358.3,11948,2)
 ;;=^5063064
 ;;^UTILITY(U,$J,358.3,11949,0)
 ;;=Z77.9^^47^538^55
 ;;^UTILITY(U,$J,358.3,11949,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,11949,1,3,0)
 ;;=3^Exposures/Contact with Hazardous Substances
 ;;^UTILITY(U,$J,358.3,11949,1,4,0)
 ;;=4^Z77.9
 ;;^UTILITY(U,$J,358.3,11949,2)
 ;;=^5063326
 ;;^UTILITY(U,$J,358.3,11950,0)
 ;;=R50.9^^47^538^57
 ;;^UTILITY(U,$J,358.3,11950,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,11950,1,3,0)
 ;;=3^Fever,Unspec
 ;;^UTILITY(U,$J,358.3,11950,1,4,0)
 ;;=4^R50.9
 ;;^UTILITY(U,$J,358.3,11950,2)
 ;;=^5019512
 ;;^UTILITY(U,$J,358.3,11951,0)
 ;;=Z96.9^^47^538^58
 ;;^UTILITY(U,$J,358.3,11951,1,0)
 ;;=^358.31IA^4^2
