IBDEI3F2 ; ; 19-NOV-2015
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;JUN 29, 2015
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,57470,1,4,0)
 ;;=4^N81.3
 ;;^UTILITY(U,$J,358.3,57470,2)
 ;;=^5015840
 ;;^UTILITY(U,$J,358.3,57471,0)
 ;;=N81.2^^267^2878^60
 ;;^UTILITY(U,$J,358.3,57471,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,57471,1,3,0)
 ;;=3^Uterovaginal Prolapse,Incomplete
 ;;^UTILITY(U,$J,358.3,57471,1,4,0)
 ;;=4^N81.2
 ;;^UTILITY(U,$J,358.3,57471,2)
 ;;=^5015839
 ;;^UTILITY(U,$J,358.3,57472,0)
 ;;=N81.4^^267^2878^61
 ;;^UTILITY(U,$J,358.3,57472,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,57472,1,3,0)
 ;;=3^Uterovaginal Prolapse,Unspec
 ;;^UTILITY(U,$J,358.3,57472,1,4,0)
 ;;=4^N81.4
 ;;^UTILITY(U,$J,358.3,57472,2)
 ;;=^5015841
 ;;^UTILITY(U,$J,358.3,57473,0)
 ;;=N87.9^^267^2878^27
 ;;^UTILITY(U,$J,358.3,57473,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,57473,1,3,0)
 ;;=3^Dysplasia of Cervix Uteri,Unspec
 ;;^UTILITY(U,$J,358.3,57473,1,4,0)
 ;;=4^N87.9
 ;;^UTILITY(U,$J,358.3,57473,2)
 ;;=^5015877
 ;;^UTILITY(U,$J,358.3,57474,0)
 ;;=N87.0^^267^2878^13
 ;;^UTILITY(U,$J,358.3,57474,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,57474,1,3,0)
 ;;=3^Cervical Dysplasia,Mild
 ;;^UTILITY(U,$J,358.3,57474,1,4,0)
 ;;=4^N87.0
 ;;^UTILITY(U,$J,358.3,57474,2)
 ;;=^5015875
 ;;^UTILITY(U,$J,358.3,57475,0)
 ;;=N87.1^^267^2878^14
 ;;^UTILITY(U,$J,358.3,57475,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,57475,1,3,0)
 ;;=3^Cervical Dysplasia,Moderate
 ;;^UTILITY(U,$J,358.3,57475,1,4,0)
 ;;=4^N87.1
 ;;^UTILITY(U,$J,358.3,57475,2)
 ;;=^5015876
 ;;^UTILITY(U,$J,358.3,57476,0)
 ;;=N94.1^^267^2878^26
 ;;^UTILITY(U,$J,358.3,57476,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,57476,1,3,0)
 ;;=3^Dyspareunia
 ;;^UTILITY(U,$J,358.3,57476,1,4,0)
 ;;=4^N94.1
 ;;^UTILITY(U,$J,358.3,57476,2)
 ;;=^5015917
 ;;^UTILITY(U,$J,358.3,57477,0)
 ;;=N94.6^^267^2878^25
 ;;^UTILITY(U,$J,358.3,57477,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,57477,1,3,0)
 ;;=3^Dysmenorrhea,Unspec
 ;;^UTILITY(U,$J,358.3,57477,1,4,0)
 ;;=4^N94.6
 ;;^UTILITY(U,$J,358.3,57477,2)
 ;;=^5015922
 ;;^UTILITY(U,$J,358.3,57478,0)
 ;;=R10.2^^267^2878^46
 ;;^UTILITY(U,$J,358.3,57478,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,57478,1,3,0)
 ;;=3^Pelvic & Perineal Pain
 ;;^UTILITY(U,$J,358.3,57478,1,4,0)
 ;;=4^R10.2
 ;;^UTILITY(U,$J,358.3,57478,2)
 ;;=^5019209
 ;;^UTILITY(U,$J,358.3,57479,0)
 ;;=N91.2^^267^2878^6
 ;;^UTILITY(U,$J,358.3,57479,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,57479,1,3,0)
 ;;=3^Amenorrhea,Unspec
 ;;^UTILITY(U,$J,358.3,57479,1,4,0)
 ;;=4^N91.2
 ;;^UTILITY(U,$J,358.3,57479,2)
 ;;=^5015904
 ;;^UTILITY(U,$J,358.3,57480,0)
 ;;=N93.8^^267^2878^58
 ;;^UTILITY(U,$J,358.3,57480,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,57480,1,3,0)
 ;;=3^Uterine & Vaginal Bleeding,Abnormal
 ;;^UTILITY(U,$J,358.3,57480,1,4,0)
 ;;=4^N93.8
 ;;^UTILITY(U,$J,358.3,57480,2)
 ;;=^5015915
 ;;^UTILITY(U,$J,358.3,57481,0)
 ;;=N95.0^^267^2878^49
 ;;^UTILITY(U,$J,358.3,57481,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,57481,1,3,0)
 ;;=3^Postmenopausal Bleeding
 ;;^UTILITY(U,$J,358.3,57481,1,4,0)
 ;;=4^N95.0
 ;;^UTILITY(U,$J,358.3,57481,2)
 ;;=^97040
 ;;^UTILITY(U,$J,358.3,57482,0)
 ;;=N95.1^^267^2878^44
 ;;^UTILITY(U,$J,358.3,57482,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,57482,1,3,0)
 ;;=3^Menopausal & Female Climacteric States
 ;;^UTILITY(U,$J,358.3,57482,1,4,0)
 ;;=4^N95.1
 ;;^UTILITY(U,$J,358.3,57482,2)
 ;;=^5015927
 ;;^UTILITY(U,$J,358.3,57483,0)
 ;;=N95.8^^267^2878^45
 ;;^UTILITY(U,$J,358.3,57483,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,57483,1,3,0)
 ;;=3^Menopausal & Perimenopausal Disorders NEC
 ;;^UTILITY(U,$J,358.3,57483,1,4,0)
 ;;=4^N95.8
 ;;^UTILITY(U,$J,358.3,57483,2)
 ;;=^5015928
