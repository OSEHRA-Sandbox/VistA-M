IBDEI0DT ; ; 06-AUG-2013
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;AUG 06, 2013
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,18570,1,5,0)
 ;;=5^Fall From Ladder
 ;;^UTILITY(U,$J,358.3,18570,2)
 ;;=Fall From Ladder^294644
 ;;^UTILITY(U,$J,358.3,18571,0)
 ;;=E882.^^116^1013^3
 ;;^UTILITY(U,$J,358.3,18571,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,18571,1,4,0)
 ;;=4^E882.
 ;;^UTILITY(U,$J,358.3,18571,1,5,0)
 ;;=5^Fall From Building
 ;;^UTILITY(U,$J,358.3,18571,2)
 ;;=Fall From Building^294646
 ;;^UTILITY(U,$J,358.3,18572,0)
 ;;=E883.9^^116^1013^13
 ;;^UTILITY(U,$J,358.3,18572,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,18572,1,4,0)
 ;;=4^E883.9
 ;;^UTILITY(U,$J,358.3,18572,1,5,0)
 ;;=5^Fall Into Hole
 ;;^UTILITY(U,$J,358.3,18572,2)
 ;;=Fall Into Hole^294650
 ;;^UTILITY(U,$J,358.3,18573,0)
 ;;=E884.2^^116^1013^4
 ;;^UTILITY(U,$J,358.3,18573,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,18573,1,4,0)
 ;;=4^E884.2
 ;;^UTILITY(U,$J,358.3,18573,1,5,0)
 ;;=5^Fall From Chair
 ;;^UTILITY(U,$J,358.3,18573,2)
 ;;=Fall From Chair^294653
 ;;^UTILITY(U,$J,358.3,18574,0)
 ;;=E884.3^^116^1013^11
 ;;^UTILITY(U,$J,358.3,18574,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,18574,1,4,0)
 ;;=4^E884.3
 ;;^UTILITY(U,$J,358.3,18574,1,5,0)
 ;;=5^Fall From Wheelchair
 ;;^UTILITY(U,$J,358.3,18574,2)
 ;;=Fall From Wheelchair^303368
 ;;^UTILITY(U,$J,358.3,18575,0)
 ;;=E884.4^^116^1013^2
 ;;^UTILITY(U,$J,358.3,18575,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,18575,1,4,0)
 ;;=4^E884.4
 ;;^UTILITY(U,$J,358.3,18575,1,5,0)
 ;;=5^Fall From Bed
 ;;^UTILITY(U,$J,358.3,18575,2)
 ;;=Fall From Bed^303369
 ;;^UTILITY(U,$J,358.3,18576,0)
 ;;=E884.6^^116^1013^5
 ;;^UTILITY(U,$J,358.3,18576,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,18576,1,4,0)
 ;;=4^E884.6
 ;;^UTILITY(U,$J,358.3,18576,1,5,0)
 ;;=5^Fall From Commode
 ;;^UTILITY(U,$J,358.3,18576,2)
 ;;=Fall from Commode^303371
 ;;^UTILITY(U,$J,358.3,18577,0)
 ;;=E884.9^^116^1013^18
 ;;^UTILITY(U,$J,358.3,18577,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,18577,1,4,0)
 ;;=4^E884.9
 ;;^UTILITY(U,$J,358.3,18577,1,5,0)
 ;;=5^Other Fall, One Level To Another
 ;;^UTILITY(U,$J,358.3,18577,2)
 ;;=Other Fall^294654
 ;;^UTILITY(U,$J,358.3,18578,0)
 ;;=E885.1^^116^1013^7
 ;;^UTILITY(U,$J,358.3,18578,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,18578,1,4,0)
 ;;=4^E885.1
 ;;^UTILITY(U,$J,358.3,18578,1,5,0)
 ;;=5^Fall From Roller Skates
 ;;^UTILITY(U,$J,358.3,18578,2)
 ;;=Fall from Roller Skates^322100
 ;;^UTILITY(U,$J,358.3,18579,0)
 ;;=E885.2^^116^1013^8
 ;;^UTILITY(U,$J,358.3,18579,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,18579,1,4,0)
 ;;=4^E885.2
 ;;^UTILITY(U,$J,358.3,18579,1,5,0)
 ;;=5^Fall From Skateboard
 ;;^UTILITY(U,$J,358.3,18579,2)
 ;;=Fall from Skateboard^322102
 ;;^UTILITY(U,$J,358.3,18580,0)
 ;;=E885.3^^116^1013^9
 ;;^UTILITY(U,$J,358.3,18580,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,18580,1,4,0)
 ;;=4^E885.3
 ;;^UTILITY(U,$J,358.3,18580,1,5,0)
 ;;=5^Fall From Skis
 ;;^UTILITY(U,$J,358.3,18580,2)
 ;;=Fall from Skis^322103
 ;;^UTILITY(U,$J,358.3,18581,0)
 ;;=E885.4^^116^1013^10
 ;;^UTILITY(U,$J,358.3,18581,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,18581,1,4,0)
 ;;=4^E885.4
 ;;^UTILITY(U,$J,358.3,18581,1,5,0)
 ;;=5^Fall From Snowboard
 ;;^UTILITY(U,$J,358.3,18581,2)
 ;;=Fall from Snowboard^322104
 ;;^UTILITY(U,$J,358.3,18582,0)
 ;;=E885.9^^116^1013^1
 ;;^UTILITY(U,$J,358.3,18582,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,18582,1,4,0)
 ;;=4^E885.9
 ;;^UTILITY(U,$J,358.3,18582,1,5,0)
 ;;=5^Fall After Tripping Or Slipping
 ;;^UTILITY(U,$J,358.3,18582,2)
 ;;=Fall after tripping or slipping^322105
 ;;^UTILITY(U,$J,358.3,18583,0)
 ;;=E886.0^^116^1013^12
 ;;^UTILITY(U,$J,358.3,18583,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,18583,1,4,0)
 ;;=4^E886.0
 ;;^UTILITY(U,$J,358.3,18583,1,5,0)
 ;;=5^Fall In Sports
 ;;^UTILITY(U,$J,358.3,18583,2)
 ;;=Fall in Sports^294656
 ;;^UTILITY(U,$J,358.3,18584,0)
 ;;=E886.9^^116^1013^15
 ;;^UTILITY(U,$J,358.3,18584,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,18584,1,4,0)
 ;;=4^E886.9
 ;;^UTILITY(U,$J,358.3,18584,1,5,0)
 ;;=5^Fall, Collision With Another Person
 ;;^UTILITY(U,$J,358.3,18584,2)
 ;;=Fall, Collision with another person^294657
 ;;^UTILITY(U,$J,358.3,18585,0)
 ;;=E888.9^^116^1013^16
 ;;^UTILITY(U,$J,358.3,18585,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,18585,1,4,0)
 ;;=4^E888.9
 ;;^UTILITY(U,$J,358.3,18585,1,5,0)
 ;;=5^Fall, Not Specified
 ;;^UTILITY(U,$J,358.3,18585,2)
 ;;=Fall, Not Specified^323639
 ;;^UTILITY(U,$J,358.3,18586,0)
 ;;=E819.0^^116^1013^19
 ;;^UTILITY(U,$J,358.3,18586,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,18586,1,4,0)
 ;;=4^E819.0
 ;;^UTILITY(U,$J,358.3,18586,1,5,0)
 ;;=5^Traffic Accident, Driver
 ;;^UTILITY(U,$J,358.3,18586,2)
 ;;=Traffic Accident, Driver^294215
 ;;^UTILITY(U,$J,358.3,18587,0)
 ;;=E819.1^^116^1013^20
 ;;^UTILITY(U,$J,358.3,18587,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,18587,1,4,0)
 ;;=4^E819.1
 ;;^UTILITY(U,$J,358.3,18587,1,5,0)
 ;;=5^Traffic Accident, Passenger
 ;;^UTILITY(U,$J,358.3,18587,2)
 ;;=^294216
 ;;^UTILITY(U,$J,358.3,18588,0)
 ;;=E891.9^^116^1013^17
 ;;^UTILITY(U,$J,358.3,18588,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,18588,1,4,0)
 ;;=4^E891.9
 ;;^UTILITY(U,$J,358.3,18588,1,5,0)
 ;;=5^Fire In Building
 ;;^UTILITY(U,$J,358.3,18588,2)
 ;;=Fire in Building^294669
 ;;^UTILITY(U,$J,358.3,18589,0)
 ;;=0^1^116^1013^0^CODE THE CONDITION OR SYMPTOM FIRST
 ;;^UTILITY(U,$J,358.3,18589,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,18589,1,4,0)
 ;;=4
 ;;^UTILITY(U,$J,358.3,18589,1,5,0)
 ;;=5
 ;;^UTILITY(U,$J,358.3,18590,0)
 ;;=S9446^^117^1014^2^^^^1
 ;;^UTILITY(U,$J,358.3,18590,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,18590,1,2,0)
 ;;=2^Pt/Leisure Ed,Grp NOS
 ;;^UTILITY(U,$J,358.3,18590,1,3,0)
 ;;=3^S9446
 ;;^UTILITY(U,$J,358.3,18591,0)
 ;;=S9451^^117^1014^3^^^^1
 ;;^UTILITY(U,$J,358.3,18591,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,18591,1,2,0)
 ;;=2^Exercise Class,per session
 ;;^UTILITY(U,$J,358.3,18591,1,3,0)
 ;;=3^S9451
 ;;^UTILITY(U,$J,358.3,18592,0)
 ;;=S9454^^117^1014^4^^^^1
 ;;^UTILITY(U,$J,358.3,18592,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,18592,1,2,0)
 ;;=2^Stress Mgmt Class,per session
 ;;^UTILITY(U,$J,358.3,18592,1,3,0)
 ;;=3^S9454
 ;;^UTILITY(U,$J,358.3,18593,0)
 ;;=98961^^117^1014^5^^^^1
 ;;^UTILITY(U,$J,358.3,18593,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,18593,1,2,0)
 ;;=2^Educ,Stand Curric,2-4,30min
 ;;^UTILITY(U,$J,358.3,18593,1,3,0)
 ;;=3^98961
 ;;^UTILITY(U,$J,358.3,18594,0)
 ;;=98962^^117^1014^6^^^^1
 ;;^UTILITY(U,$J,358.3,18594,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,18594,1,2,0)
 ;;=2^Educ,Stand Curric,5-8,30min
 ;;^UTILITY(U,$J,358.3,18594,1,3,0)
 ;;=3^98962
 ;;^UTILITY(U,$J,358.3,18595,0)
 ;;=96153^^117^1014^7^^^^1
 ;;^UTILITY(U,$J,358.3,18595,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,18595,1,2,0)
 ;;=2^Group Train/Support,ea 15min
 ;;^UTILITY(U,$J,358.3,18595,1,3,0)
 ;;=3^96153
 ;;^UTILITY(U,$J,358.3,18596,0)
 ;;=97150^^117^1015^1^^^^1
 ;;^UTILITY(U,$J,358.3,18596,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,18596,1,2,0)
 ;;=2^Grp Therapeutic Procedures,ea 15min
 ;;^UTILITY(U,$J,358.3,18596,1,3,0)
 ;;=3^97150
 ;;^UTILITY(U,$J,358.3,18597,0)
 ;;=97139^^117^1015^2^^^^1
 ;;^UTILITY(U,$J,358.3,18597,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,18597,1,2,0)
 ;;=2^Unlisted Thera Procedure (define)
 ;;^UTILITY(U,$J,358.3,18597,1,3,0)
 ;;=3^97139
 ;;^UTILITY(U,$J,358.3,18598,0)
 ;;=T2001^^117^1015^3^^^^1
 ;;^UTILITY(U,$J,358.3,18598,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,18598,1,2,0)
 ;;=2^Escort Patient to Activities
 ;;^UTILITY(U,$J,358.3,18598,1,3,0)
 ;;=3^T2001
 ;;^UTILITY(U,$J,358.3,18599,0)
 ;;=97150^^117^1016^3^^^^1
 ;;^UTILITY(U,$J,358.3,18599,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,18599,1,2,0)
 ;;=2^Creative Arts Grp,ea 15min
 ;;^UTILITY(U,$J,358.3,18599,1,3,0)
 ;;=3^97150
 ;;^UTILITY(U,$J,358.3,18600,0)
 ;;=97150^^117^1016^8^^^^1
 ;;^UTILITY(U,$J,358.3,18600,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,18600,1,2,0)
 ;;=2^Rec. Therapy Grp,ea 15min
 ;;^UTILITY(U,$J,358.3,18600,1,3,0)
 ;;=3^97150
 ;;^UTILITY(U,$J,358.3,18601,0)
 ;;=97150^^117^1016^2^^^^1
 ;;^UTILITY(U,$J,358.3,18601,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,18601,1,2,0)
 ;;=2^Art Therapy Grp,ea 15min
 ;;^UTILITY(U,$J,358.3,18601,1,3,0)
 ;;=3^97150
 ;;^UTILITY(U,$J,358.3,18602,0)
 ;;=97150^^117^1016^4^^^^1
 ;;^UTILITY(U,$J,358.3,18602,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,18602,1,2,0)
 ;;=2^Dance Therapy Grp,ea 15min
 ;;^UTILITY(U,$J,358.3,18602,1,3,0)
 ;;=3^97150
 ;;^UTILITY(U,$J,358.3,18603,0)
 ;;=97150^^117^1016^5^^^^1
 ;;^UTILITY(U,$J,358.3,18603,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,18603,1,2,0)
 ;;=2^Drama Therapy Grp,ea 15min
 ;;^UTILITY(U,$J,358.3,18603,1,3,0)
 ;;=3^97150
 ;;^UTILITY(U,$J,358.3,18604,0)
 ;;=97150^^117^1016^6^^^^1
 ;;^UTILITY(U,$J,358.3,18604,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,18604,1,2,0)
 ;;=2^Music Therapy Grp,ea 15min
 ;;^UTILITY(U,$J,358.3,18604,1,3,0)
 ;;=3^97150
 ;;^UTILITY(U,$J,358.3,18605,0)
 ;;=97150^^117^1016^1^^^^1
 ;;^UTILITY(U,$J,358.3,18605,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,18605,1,2,0)
 ;;=2^Aquatic Therapy Grp,ea 15min
 ;;^UTILITY(U,$J,358.3,18605,1,3,0)
 ;;=3^97150
 ;;^UTILITY(U,$J,358.3,18606,0)
 ;;=97150^^117^1016^9^^^^1
 ;;^UTILITY(U,$J,358.3,18606,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,18606,1,2,0)
 ;;=2^Sports Therapy Grp,ea 15min
 ;;^UTILITY(U,$J,358.3,18606,1,3,0)
 ;;=3^97150
 ;;^UTILITY(U,$J,358.3,18607,0)
 ;;=97150^^117^1016^7^^^^1
 ;;^UTILITY(U,$J,358.3,18607,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,18607,1,2,0)
 ;;=2^Oth Rec Ther,Grp,ea 15min
 ;;^UTILITY(U,$J,358.3,18607,1,3,0)
 ;;=3^97150
 ;;^UTILITY(U,$J,358.3,18608,0)
 ;;=96154^^117^1017^1^^^^1
 ;;^UTILITY(U,$J,358.3,18608,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,18608,1,2,0)
 ;;=2^Train/Support w/Pt/Fam,ea 15min
 ;;^UTILITY(U,$J,358.3,18608,1,3,0)
 ;;=3^96154
 ;;^UTILITY(U,$J,358.3,18609,0)
 ;;=96155^^117^1017^2^^^^1
