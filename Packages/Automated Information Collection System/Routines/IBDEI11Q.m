IBDEI11Q ; ; 12-MAY-2016
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;MAY 12, 2016
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,37950,1,3,0)
 ;;=3^Issue of Repeat Prescription
 ;;^UTILITY(U,$J,358.3,37950,1,4,0)
 ;;=4^Z76.0
 ;;^UTILITY(U,$J,358.3,37950,2)
 ;;=^5063297
 ;;^UTILITY(U,$J,358.3,37951,0)
 ;;=Z01.818^^142^1886^23
 ;;^UTILITY(U,$J,358.3,37951,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,37951,1,3,0)
 ;;=3^Pre-Op Call
 ;;^UTILITY(U,$J,358.3,37951,1,4,0)
 ;;=4^Z01.818
 ;;^UTILITY(U,$J,358.3,37951,2)
 ;;=^5062628
 ;;^UTILITY(U,$J,358.3,37952,0)
 ;;=Z51.81^^142^1886^24
 ;;^UTILITY(U,$J,358.3,37952,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,37952,1,3,0)
 ;;=3^Therapeutic Drug Level Monitoring
 ;;^UTILITY(U,$J,358.3,37952,1,4,0)
 ;;=4^Z51.81
 ;;^UTILITY(U,$J,358.3,37952,2)
 ;;=^5063064
 ;;^UTILITY(U,$J,358.3,37953,0)
 ;;=I10.^^142^1886^17
 ;;^UTILITY(U,$J,358.3,37953,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,37953,1,3,0)
 ;;=3^Hypertension
 ;;^UTILITY(U,$J,358.3,37953,1,4,0)
 ;;=4^I10.
 ;;^UTILITY(U,$J,358.3,37953,2)
 ;;=^5007062
 ;;^UTILITY(U,$J,358.3,37954,0)
 ;;=Z79.01^^142^1886^19
 ;;^UTILITY(U,$J,358.3,37954,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,37954,1,3,0)
 ;;=3^Long Term (Current) Use of Anticoagulants
 ;;^UTILITY(U,$J,358.3,37954,1,4,0)
 ;;=4^Z79.01
 ;;^UTILITY(U,$J,358.3,37954,2)
 ;;=^5063330
 ;;^UTILITY(U,$J,358.3,37955,0)
 ;;=R73.09^^142^1886^3
 ;;^UTILITY(U,$J,358.3,37955,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,37955,1,3,0)
 ;;=3^Abnormal Glucose
 ;;^UTILITY(U,$J,358.3,37955,1,4,0)
 ;;=4^R73.09
 ;;^UTILITY(U,$J,358.3,37955,2)
 ;;=^5019563
 ;;^UTILITY(U,$J,358.3,37956,0)
 ;;=R68.89^^142^1886^14
 ;;^UTILITY(U,$J,358.3,37956,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,37956,1,3,0)
 ;;=3^General Symptoms & Signs
 ;;^UTILITY(U,$J,358.3,37956,1,4,0)
 ;;=4^R68.89
 ;;^UTILITY(U,$J,358.3,37956,2)
 ;;=^5019557
 ;;^UTILITY(U,$J,358.3,37957,0)
 ;;=R79.89^^142^1886^1
 ;;^UTILITY(U,$J,358.3,37957,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,37957,1,3,0)
 ;;=3^Abnormal Findings of Blood Chemistry
 ;;^UTILITY(U,$J,358.3,37957,1,4,0)
 ;;=4^R79.89
 ;;^UTILITY(U,$J,358.3,37957,2)
 ;;=^5019593
 ;;^UTILITY(U,$J,358.3,37958,0)
 ;;=Z71.89^^142^1886^8
 ;;^UTILITY(U,$J,358.3,37958,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,37958,1,3,0)
 ;;=3^Counseling,Oth Specified
 ;;^UTILITY(U,$J,358.3,37958,1,4,0)
 ;;=4^Z71.89
 ;;^UTILITY(U,$J,358.3,37958,2)
 ;;=^5063253
 ;;^UTILITY(U,$J,358.3,37959,0)
 ;;=Z98.89^^142^1886^22
 ;;^UTILITY(U,$J,358.3,37959,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,37959,1,3,0)
 ;;=3^Post-Procedural Call
 ;;^UTILITY(U,$J,358.3,37959,1,4,0)
 ;;=4^Z98.89
 ;;^UTILITY(U,$J,358.3,37959,2)
 ;;=^5063754
 ;;^UTILITY(U,$J,358.3,37960,0)
 ;;=Z71.2^^142^1886^13
 ;;^UTILITY(U,$J,358.3,37960,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,37960,1,3,0)
 ;;=3^Explanation of Exam or Test Finding
 ;;^UTILITY(U,$J,358.3,37960,1,4,0)
 ;;=4^Z71.2
 ;;^UTILITY(U,$J,358.3,37960,2)
 ;;=^5063244
 ;;^UTILITY(U,$J,358.3,37961,0)
 ;;=Z86.51^^142^1886^20
 ;;^UTILITY(U,$J,358.3,37961,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,37961,1,3,0)
 ;;=3^Personal Hx of Combat & Operational Stress Reaction
 ;;^UTILITY(U,$J,358.3,37961,1,4,0)
 ;;=4^Z86.51
 ;;^UTILITY(U,$J,358.3,37961,2)
 ;;=^5063470
 ;;^UTILITY(U,$J,358.3,37962,0)
 ;;=Z76.89^^142^1886^15
 ;;^UTILITY(U,$J,358.3,37962,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,37962,1,3,0)
 ;;=3^Health Services in Other Specified Circumstances
 ;;^UTILITY(U,$J,358.3,37962,1,4,0)
 ;;=4^Z76.89
 ;;^UTILITY(U,$J,358.3,37962,2)
 ;;=^5063304
 ;;^UTILITY(U,$J,358.3,37963,0)
 ;;=E10.9^^142^1886^11
 ;;^UTILITY(U,$J,358.3,37963,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,37963,1,3,0)
 ;;=3^Diabetes Type 1 w/o Complications
 ;;^UTILITY(U,$J,358.3,37963,1,4,0)
 ;;=4^E10.9
 ;;^UTILITY(U,$J,358.3,37963,2)
 ;;=^5002626
 ;;^UTILITY(U,$J,358.3,37964,0)
 ;;=E11.9^^142^1886^12
 ;;^UTILITY(U,$J,358.3,37964,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,37964,1,3,0)
 ;;=3^Diabetes Type 2 w/o Complications
 ;;^UTILITY(U,$J,358.3,37964,1,4,0)
 ;;=4^E11.9
 ;;^UTILITY(U,$J,358.3,37964,2)
 ;;=^5002666
 ;;^UTILITY(U,$J,358.3,37965,0)
 ;;=Z51.89^^142^1886^6
 ;;^UTILITY(U,$J,358.3,37965,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,37965,1,3,0)
 ;;=3^Aftercare,Oth Specified
 ;;^UTILITY(U,$J,358.3,37965,1,4,0)
 ;;=4^Z51.89
 ;;^UTILITY(U,$J,358.3,37965,2)
 ;;=^5063065
 ;;^UTILITY(U,$J,358.3,37966,0)
 ;;=Z56.6^^142^1886^21
 ;;^UTILITY(U,$J,358.3,37966,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,37966,1,3,0)
 ;;=3^Physical & Mental Strain Related to Work
 ;;^UTILITY(U,$J,358.3,37966,1,4,0)
 ;;=4^Z56.6
 ;;^UTILITY(U,$J,358.3,37966,2)
 ;;=^5063113
 ;;^UTILITY(U,$J,358.3,37967,0)
 ;;=Z59.9^^142^1886^16
 ;;^UTILITY(U,$J,358.3,37967,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,37967,1,3,0)
 ;;=3^Housing & Economic Circumstance Problems,Unspec
 ;;^UTILITY(U,$J,358.3,37967,1,4,0)
 ;;=4^Z59.9
 ;;^UTILITY(U,$J,358.3,37967,2)
 ;;=^5063138
 ;;^UTILITY(U,$J,358.3,37968,0)
 ;;=Z71.9^^142^1886^9
 ;;^UTILITY(U,$J,358.3,37968,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,37968,1,3,0)
 ;;=3^Counseling,Unspec
 ;;^UTILITY(U,$J,358.3,37968,1,4,0)
 ;;=4^Z71.9
 ;;^UTILITY(U,$J,358.3,37968,2)
 ;;=^5063254
 ;;^UTILITY(U,$J,358.3,37969,0)
 ;;=19260^^143^1887^6^^^^1
 ;;^UTILITY(U,$J,358.3,37969,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,37969,1,2,0)
 ;;=2^19260
 ;;^UTILITY(U,$J,358.3,37969,1,3,0)
 ;;=3^Excision CW Tumor incl Ribs
 ;;^UTILITY(U,$J,358.3,37970,0)
 ;;=19271^^143^1887^7^^^^1
 ;;^UTILITY(U,$J,358.3,37970,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,37970,1,2,0)
 ;;=2^19271
 ;;^UTILITY(U,$J,358.3,37970,1,3,0)
 ;;=3^Excision CW Tumor w/ Reconstruction
 ;;^UTILITY(U,$J,358.3,37971,0)
 ;;=21555^^143^1887^4^^^^1
 ;;^UTILITY(U,$J,358.3,37971,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,37971,1,2,0)
 ;;=2^21555
 ;;^UTILITY(U,$J,358.3,37971,1,3,0)
 ;;=3^Exc Tumor,Neck/Thorax,SQ < 3cm
 ;;^UTILITY(U,$J,358.3,37972,0)
 ;;=21556^^143^1887^5^^^^1
 ;;^UTILITY(U,$J,358.3,37972,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,37972,1,2,0)
 ;;=2^21556
 ;;^UTILITY(U,$J,358.3,37972,1,3,0)
 ;;=3^Exc Tumor,Neck/Thorax;Deep < 5cm
 ;;^UTILITY(U,$J,358.3,37973,0)
 ;;=21600^^143^1887^8^^^^1
 ;;^UTILITY(U,$J,358.3,37973,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,37973,1,2,0)
 ;;=2^21600
 ;;^UTILITY(U,$J,358.3,37973,1,3,0)
 ;;=3^Excision of Rib,Partial
 ;;^UTILITY(U,$J,358.3,37974,0)
 ;;=21825^^143^1887^10^^^^1
 ;;^UTILITY(U,$J,358.3,37974,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,37974,1,2,0)
 ;;=2^21825
 ;;^UTILITY(U,$J,358.3,37974,1,3,0)
 ;;=3^Open Tx Sternum Fx w/ or w/o Fixation
 ;;^UTILITY(U,$J,358.3,37975,0)
 ;;=21557^^143^1887^11^^^^1
 ;;^UTILITY(U,$J,358.3,37975,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,37975,1,2,0)
 ;;=2^21557
 ;;^UTILITY(U,$J,358.3,37975,1,3,0)
 ;;=3^Rad Resect Neck/Thorax Tumor
 ;;^UTILITY(U,$J,358.3,37976,0)
 ;;=21630^^143^1887^13^^^^1
 ;;^UTILITY(U,$J,358.3,37976,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,37976,1,2,0)
 ;;=2^21630
 ;;^UTILITY(U,$J,358.3,37976,1,3,0)
 ;;=3^Rad Resect,Sternum
 ;;^UTILITY(U,$J,358.3,37977,0)
 ;;=21632^^143^1887^12^^^^1
 ;;^UTILITY(U,$J,358.3,37977,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,37977,1,2,0)
 ;;=2^21632
 ;;^UTILITY(U,$J,358.3,37977,1,3,0)
 ;;=3^Rad Resect Sternum w/ Media Lymphadenect
 ;;^UTILITY(U,$J,358.3,37978,0)
 ;;=21740^^143^1887^16^^^^1
 ;;^UTILITY(U,$J,358.3,37978,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,37978,1,2,0)
 ;;=2^21740
 ;;^UTILITY(U,$J,358.3,37978,1,3,0)
 ;;=3^Recons Repair Pect Excavat/Carinat;Opn
 ;;^UTILITY(U,$J,358.3,37979,0)
 ;;=21742^^143^1887^15^^^^1
 ;;^UTILITY(U,$J,358.3,37979,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,37979,1,2,0)
 ;;=2^21742
 ;;^UTILITY(U,$J,358.3,37979,1,3,0)
 ;;=3^Recons Rep Pect,Nuss Proc w/o Thoracosc
 ;;^UTILITY(U,$J,358.3,37980,0)
 ;;=21743^^143^1887^14^^^^1
