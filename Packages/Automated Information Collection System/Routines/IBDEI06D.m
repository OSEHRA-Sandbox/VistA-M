IBDEI06D ; ; 06-AUG-2013
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;AUG 06, 2013
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,8312,0)
 ;;=11057^^61^507^35^^^^1
 ;;^UTILITY(U,$J,358.3,8312,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,8312,1,2,0)
 ;;=2^TRIM CORN/CALLOUS,5 OR MORE
 ;;^UTILITY(U,$J,358.3,8312,1,3,0)
 ;;=3^11057
 ;;^UTILITY(U,$J,358.3,8313,0)
 ;;=12001^^61^507^28^^^^1
 ;;^UTILITY(U,$J,358.3,8313,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,8313,1,2,0)
 ;;=2^REPAIR SUPREFICIAL WOUND < 2.5cm
 ;;^UTILITY(U,$J,358.3,8313,1,3,0)
 ;;=3^12001
 ;;^UTILITY(U,$J,358.3,8314,0)
 ;;=12002^^61^507^29^^^^1
 ;;^UTILITY(U,$J,358.3,8314,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,8314,1,2,0)
 ;;=2^REPAIR SUPERFICIAL WND 2.6-7.5cm
 ;;^UTILITY(U,$J,358.3,8314,1,3,0)
 ;;=3^12002
 ;;^UTILITY(U,$J,358.3,8315,0)
 ;;=12011^^61^507^32^^^^1
 ;;^UTILITY(U,$J,358.3,8315,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,8315,1,2,0)
 ;;=2^SUTURE SIMPLE WND,FACE < 2.6cm
 ;;^UTILITY(U,$J,358.3,8315,1,3,0)
 ;;=3^12011
 ;;^UTILITY(U,$J,358.3,8316,0)
 ;;=12021^^61^507^6^^^^1
 ;;^UTILITY(U,$J,358.3,8316,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,8316,1,2,0)
 ;;=2^CLOSURE OF SPLIT WOUND
 ;;^UTILITY(U,$J,358.3,8316,1,3,0)
 ;;=3^12021
 ;;^UTILITY(U,$J,358.3,8317,0)
 ;;=16000^^61^507^1^^^^1
 ;;^UTILITY(U,$J,358.3,8317,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,8317,1,2,0)
 ;;=2^BURNS,1st DEGREE,INIT TX
 ;;^UTILITY(U,$J,358.3,8317,1,3,0)
 ;;=3^16000
 ;;^UTILITY(U,$J,358.3,8318,0)
 ;;=16020^^61^507^4^^^^1
 ;;^UTILITY(U,$J,358.3,8318,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,8318,1,2,0)
 ;;=2^BURNS,SMALL
 ;;^UTILITY(U,$J,358.3,8318,1,3,0)
 ;;=3^16020
 ;;^UTILITY(U,$J,358.3,8319,0)
 ;;=16030^^61^507^2^^^^1
 ;;^UTILITY(U,$J,358.3,8319,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,8319,1,2,0)
 ;;=2^BURNS,LARGE
 ;;^UTILITY(U,$J,358.3,8319,1,3,0)
 ;;=3^16030
 ;;^UTILITY(U,$J,358.3,8320,0)
 ;;=16025^^61^507^3^^^^1
 ;;^UTILITY(U,$J,358.3,8320,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,8320,1,2,0)
 ;;=2^BURNS,MEDUIM
 ;;^UTILITY(U,$J,358.3,8320,1,3,0)
 ;;=3^16025
 ;;^UTILITY(U,$J,358.3,8321,0)
 ;;=17250^^61^507^5^^^^1
 ;;^UTILITY(U,$J,358.3,8321,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,8321,1,2,0)
 ;;=2^CHEMICAL CAUTERY
 ;;^UTILITY(U,$J,358.3,8321,1,3,0)
 ;;=3^17250
 ;;^UTILITY(U,$J,358.3,8322,0)
 ;;=20005^^61^507^20^^^^1
 ;;^UTILITY(U,$J,358.3,8322,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,8322,1,2,0)
 ;;=2^INCISION OF ABSCESS
 ;;^UTILITY(U,$J,358.3,8322,1,3,0)
 ;;=3^20005
 ;;^UTILITY(U,$J,358.3,8323,0)
 ;;=21555^^61^507^27^^^^1
 ;;^UTILITY(U,$J,358.3,8323,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,8323,1,2,0)
 ;;=2^REMOVE LESION NECK/CHEST
 ;;^UTILITY(U,$J,358.3,8323,1,3,0)
 ;;=3^21555
 ;;^UTILITY(U,$J,358.3,8324,0)
 ;;=20550^^61^507^22^^^^1
 ;;^UTILITY(U,$J,358.3,8324,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,8324,1,2,0)
 ;;=2^INJ TENDON/LIGAMENT/CYST
 ;;^UTILITY(U,$J,358.3,8324,1,3,0)
 ;;=3^20550
 ;;^UTILITY(U,$J,358.3,8325,0)
 ;;=20551^^61^507^21^^^^1
 ;;^UTILITY(U,$J,358.3,8325,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,8325,1,2,0)
 ;;=2^INJ TENDON ORIGIN/INSERTION
 ;;^UTILITY(U,$J,358.3,8325,1,3,0)
 ;;=3^20551
 ;;^UTILITY(U,$J,358.3,8326,0)
 ;;=20552^^61^507^23^^^^1
 ;;^UTILITY(U,$J,358.3,8326,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,8326,1,2,0)
 ;;=2^INJ TRIGGER POINT 1-2 MUSCLE
 ;;^UTILITY(U,$J,358.3,8326,1,3,0)
 ;;=3^20552
 ;;^UTILITY(U,$J,358.3,8327,0)
 ;;=36410^^61^507^24^^^^1
 ;;^UTILITY(U,$J,358.3,8327,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,8327,1,2,0)
 ;;=2^NON ROUTINE BLOOD DRAW
 ;;^UTILITY(U,$J,358.3,8327,1,3,0)
 ;;=3^36410
 ;;^UTILITY(U,$J,358.3,8328,0)
 ;;=36415^^61^507^30^^^^1
 ;;^UTILITY(U,$J,358.3,8328,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,8328,1,2,0)
 ;;=2^ROUTINE VENIPUNCTURE
 ;;^UTILITY(U,$J,358.3,8328,1,3,0)
 ;;=3^36415
 ;;^UTILITY(U,$J,358.3,8329,0)
 ;;=81002^^61^507^36^^^^1
 ;;^UTILITY(U,$J,358.3,8329,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,8329,1,2,0)
 ;;=2^URINALYSIS NONAUTO W/O SCOPE
 ;;^UTILITY(U,$J,358.3,8329,1,3,0)
 ;;=3^81002
 ;;^UTILITY(U,$J,358.3,8330,0)
 ;;=82948^^61^507^25^^^^1
 ;;^UTILITY(U,$J,358.3,8330,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,8330,1,2,0)
 ;;=2^REAGENT STRIP/BLOOD GLUCOSE
 ;;^UTILITY(U,$J,358.3,8330,1,3,0)
 ;;=3^82948
 ;;^UTILITY(U,$J,358.3,8331,0)
 ;;=97597^^61^507^7^^^^1
 ;;^UTILITY(U,$J,358.3,8331,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,8331,1,2,0)
 ;;=2^DEBRIDE OPEN WND 20cm/<
 ;;^UTILITY(U,$J,358.3,8331,1,3,0)
 ;;=3^97597
 ;;^UTILITY(U,$J,358.3,8332,0)
 ;;=97598^^61^507^8^^^^1
 ;;^UTILITY(U,$J,358.3,8332,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,8332,1,2,0)
 ;;=2^DEBRIDE OPEN WND EA ADDL 20cm
 ;;^UTILITY(U,$J,358.3,8332,1,3,0)
 ;;=3^97598
 ;;^UTILITY(U,$J,358.3,8333,0)
 ;;=G0109^^61^508^1^^^^1
 ;;^UTILITY(U,$J,358.3,8333,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,8333,1,2,0)
 ;;=2^DIAB MANAGE TRN IND/GROUP
 ;;^UTILITY(U,$J,358.3,8333,1,3,0)
 ;;=3^G0109
 ;;^UTILITY(U,$J,358.3,8334,0)
 ;;=98960^^61^508^2^^^^1
 ;;^UTILITY(U,$J,358.3,8334,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,8334,1,2,0)
 ;;=2^SELF-MGMT EDUC & TRAIN 1 PT
 ;;^UTILITY(U,$J,358.3,8334,1,3,0)
 ;;=3^98960
 ;;^UTILITY(U,$J,358.3,8335,0)
 ;;=90471^^61^509^1^^^^1
 ;;^UTILITY(U,$J,358.3,8335,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,8335,1,2,0)
 ;;=2^IMMUNIZATION ADMIN
 ;;^UTILITY(U,$J,358.3,8335,1,3,0)
 ;;=3^90471
 ;;^UTILITY(U,$J,358.3,8336,0)
 ;;=90472^^61^509^2^^^^1
 ;;^UTILITY(U,$J,358.3,8336,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,8336,1,2,0)
 ;;=2^IMMUNIZATION ADMIN,EA ADDL
 ;;^UTILITY(U,$J,358.3,8336,1,3,0)
 ;;=3^90472
 ;;^UTILITY(U,$J,358.3,8337,0)
 ;;=90658^^61^509^3^^^^1
 ;;^UTILITY(U,$J,358.3,8337,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,8337,1,2,0)
 ;;=2^FLU VACCINE
 ;;^UTILITY(U,$J,358.3,8337,1,3,0)
 ;;=3^90658
 ;;^UTILITY(U,$J,358.3,8338,0)
 ;;=90703^^61^509^7^^^^1
 ;;^UTILITY(U,$J,358.3,8338,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,8338,1,2,0)
 ;;=2^TETANUS VACCINE
 ;;^UTILITY(U,$J,358.3,8338,1,3,0)
 ;;=3^90703
 ;;^UTILITY(U,$J,358.3,8339,0)
 ;;=90707^^61^509^4^^^^1
 ;;^UTILITY(U,$J,358.3,8339,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,8339,1,2,0)
 ;;=2^MMR VACCINE
 ;;^UTILITY(U,$J,358.3,8339,1,3,0)
 ;;=3^90707
 ;;^UTILITY(U,$J,358.3,8340,0)
 ;;=90714^^61^509^6^^^^1
 ;;^UTILITY(U,$J,358.3,8340,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,8340,1,2,0)
 ;;=2^TD VACCINE
 ;;^UTILITY(U,$J,358.3,8340,1,3,0)
 ;;=3^90714
 ;;^UTILITY(U,$J,358.3,8341,0)
 ;;=90732^^61^509^5^^^^1
 ;;^UTILITY(U,$J,358.3,8341,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,8341,1,2,0)
 ;;=2^PNEUMOCOCCAL VACCINE
 ;;^UTILITY(U,$J,358.3,8341,1,3,0)
 ;;=3^90732
 ;;^UTILITY(U,$J,358.3,8342,0)
 ;;=99420^^61^510^1^^^^1
 ;;^UTILITY(U,$J,358.3,8342,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,8342,1,2,0)
 ;;=2^HEALTH RISK ASSESSMENT TEST
 ;;^UTILITY(U,$J,358.3,8342,1,3,0)
 ;;=3^99420
 ;;^UTILITY(U,$J,358.3,8343,0)
 ;;=99429^^61^510^2^^^^1
 ;;^UTILITY(U,$J,358.3,8343,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,8343,1,2,0)
 ;;=2^UNLISTED PREVENTIVE SERVICE
 ;;^UTILITY(U,$J,358.3,8343,1,3,0)
 ;;=3^99429
 ;;^UTILITY(U,$J,358.3,8344,0)
 ;;=86485^^61^511^1^^^^1
 ;;^UTILITY(U,$J,358.3,8344,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,8344,1,2,0)
 ;;=2^CANDIDA
 ;;^UTILITY(U,$J,358.3,8344,1,3,0)
 ;;=3^86485
 ;;^UTILITY(U,$J,358.3,8345,0)
 ;;=86486^^61^511^4^^^^1
 ;;^UTILITY(U,$J,358.3,8345,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,8345,1,2,0)
 ;;=2^MUMPS
 ;;^UTILITY(U,$J,358.3,8345,1,3,0)
 ;;=3^86486
 ;;^UTILITY(U,$J,358.3,8346,0)
 ;;=86580^^61^511^5^^^^1
 ;;^UTILITY(U,$J,358.3,8346,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,8346,1,2,0)
 ;;=2^TB
 ;;^UTILITY(U,$J,358.3,8346,1,3,0)
 ;;=3^86580
 ;;^UTILITY(U,$J,358.3,8347,0)
 ;;=95024^^61^511^3^^^^1
 ;;^UTILITY(U,$J,358.3,8347,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,8347,1,2,0)
 ;;=2^INTRADERMALS (EACH)
 ;;^UTILITY(U,$J,358.3,8347,1,3,0)
 ;;=3^95024
 ;;^UTILITY(U,$J,358.3,8348,0)
 ;;=86510^^61^511^2^^^^1
 ;;^UTILITY(U,$J,358.3,8348,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,8348,1,2,0)
 ;;=2^HISTOPLASMOSIS
 ;;^UTILITY(U,$J,358.3,8348,1,3,0)
 ;;=3^86510
 ;;^UTILITY(U,$J,358.3,8349,0)
 ;;=414.01^^62^512^12
 ;;^UTILITY(U,$J,358.3,8349,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,8349,1,4,0)
 ;;=4^414.01
 ;;^UTILITY(U,$J,358.3,8349,1,5,0)
 ;;=5^Atherosclerosis, native coronary
 ;;^UTILITY(U,$J,358.3,8349,2)
 ;;=CAD, Native Vessel^303281
 ;;^UTILITY(U,$J,358.3,8350,0)
 ;;=413.9^^62^512^2
 ;;^UTILITY(U,$J,358.3,8350,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,8350,1,4,0)
 ;;=4^413.9
 ;;^UTILITY(U,$J,358.3,8350,1,5,0)
 ;;=5^Angina Pectoris
 ;;^UTILITY(U,$J,358.3,8350,2)
 ;;=Angina Pectoris^87258
 ;;^UTILITY(U,$J,358.3,8351,0)
 ;;=413.0^^62^512^3
 ;;^UTILITY(U,$J,358.3,8351,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,8351,1,4,0)
 ;;=4^413.0
 ;;^UTILITY(U,$J,358.3,8351,1,5,0)
 ;;=5^Angina at Rest
 ;;^UTILITY(U,$J,358.3,8351,2)
 ;;=Angina at Rest^265313
 ;;^UTILITY(U,$J,358.3,8352,0)
 ;;=411.1^^62^512^5
 ;;^UTILITY(U,$J,358.3,8352,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,8352,1,4,0)
 ;;=4^411.1
 ;;^UTILITY(U,$J,358.3,8352,1,5,0)
 ;;=5^Angina, Unstable
 ;;^UTILITY(U,$J,358.3,8352,2)
 ;;=Angina, Unstable^7455
 ;;^UTILITY(U,$J,358.3,8353,0)
 ;;=413.1^^62^512^4
 ;;^UTILITY(U,$J,358.3,8353,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,8353,1,4,0)
 ;;=4^413.1
 ;;^UTILITY(U,$J,358.3,8353,1,5,0)
 ;;=5^Angina, Prinzmetal
 ;;^UTILITY(U,$J,358.3,8353,2)
 ;;=^7448
 ;;^UTILITY(U,$J,358.3,8354,0)
 ;;=V58.61^^62^512^6
 ;;^UTILITY(U,$J,358.3,8354,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,8354,1,4,0)
 ;;=4^V58.61
 ;;^UTILITY(U,$J,358.3,8354,1,5,0)
 ;;=5^Anticoag Rx, chronic
 ;;^UTILITY(U,$J,358.3,8354,2)
 ;;=^303459
 ;;^UTILITY(U,$J,358.3,8355,0)
 ;;=441.4^^62^512^7
 ;;^UTILITY(U,$J,358.3,8355,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,8355,1,4,0)
 ;;=4^441.4
 ;;^UTILITY(U,$J,358.3,8355,1,5,0)
 ;;=5^Aortic Aneursym, abdominal
