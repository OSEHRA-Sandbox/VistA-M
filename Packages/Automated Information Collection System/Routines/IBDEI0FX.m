IBDEI0FX ; ; 06-AUG-2013
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;AUG 06, 2013
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,21505,1,5,0)
 ;;=5^ULCER,PEPTIC
 ;;^UTILITY(U,$J,358.3,21505,2)
 ;;=^93051
 ;;^UTILITY(U,$J,358.3,21506,0)
 ;;=707.10^^146^1331^8
 ;;^UTILITY(U,$J,358.3,21506,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,21506,1,2,0)
 ;;=2^707.10
 ;;^UTILITY(U,$J,358.3,21506,1,5,0)
 ;;=5^ULCER,LOWER LIMB
 ;;^UTILITY(U,$J,358.3,21506,2)
 ;;=^322142
 ;;^UTILITY(U,$J,358.3,21507,0)
 ;;=707.9^^146^1331^10
 ;;^UTILITY(U,$J,358.3,21507,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,21507,1,2,0)
 ;;=2^707.9
 ;;^UTILITY(U,$J,358.3,21507,1,5,0)
 ;;=5^ULCER,SKIN NOS
 ;;^UTILITY(U,$J,358.3,21507,2)
 ;;=^24439
 ;;^UTILITY(U,$J,358.3,21508,0)
 ;;=Q3014^^147^1332^1^^^^1
 ;;^UTILITY(U,$J,358.3,21508,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,21508,1,2,0)
 ;;=2^Q3014
 ;;^UTILITY(U,$J,358.3,21508,1,3,0)
 ;;=3^Telehealth Fac Fee-Pt Site
 ;;^UTILITY(U,$J,358.3,21509,0)
 ;;=441.4^^148^1333^8
 ;;^UTILITY(U,$J,358.3,21509,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,21509,1,4,0)
 ;;=4^441.4
 ;;^UTILITY(U,$J,358.3,21509,1,5,0)
 ;;=5^Aortic Aneursym, abdominal
 ;;^UTILITY(U,$J,358.3,21509,2)
 ;;=^269769
 ;;^UTILITY(U,$J,358.3,21510,0)
 ;;=441.2^^148^1333^9
 ;;^UTILITY(U,$J,358.3,21510,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,21510,1,4,0)
 ;;=4^441.2
 ;;^UTILITY(U,$J,358.3,21510,1,5,0)
 ;;=5^Aortic Aneursym, thoracic
 ;;^UTILITY(U,$J,358.3,21510,2)
 ;;=^269765
 ;;^UTILITY(U,$J,358.3,21511,0)
 ;;=428.0^^148^1333^13
 ;;^UTILITY(U,$J,358.3,21511,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,21511,1,4,0)
 ;;=4^428.0
 ;;^UTILITY(U,$J,358.3,21511,1,5,0)
 ;;=5^CHF
 ;;^UTILITY(U,$J,358.3,21511,2)
 ;;=^54758
 ;;^UTILITY(U,$J,358.3,21512,0)
 ;;=433.10^^148^1333^14
 ;;^UTILITY(U,$J,358.3,21512,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,21512,1,4,0)
 ;;=4^433.10
 ;;^UTILITY(U,$J,358.3,21512,1,5,0)
 ;;=5^Carotid Artery disease
 ;;^UTILITY(U,$J,358.3,21512,2)
 ;;=^295801
 ;;^UTILITY(U,$J,358.3,21513,0)
 ;;=443.9^^148^1333^31
 ;;^UTILITY(U,$J,358.3,21513,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,21513,1,4,0)
 ;;=4^443.9
 ;;^UTILITY(U,$J,358.3,21513,1,5,0)
 ;;=5^PVD
 ;;^UTILITY(U,$J,358.3,21513,2)
 ;;=^184182
 ;;^UTILITY(U,$J,358.3,21514,0)
 ;;=440.21^^148^1333^32
 ;;^UTILITY(U,$J,358.3,21514,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,21514,1,4,0)
 ;;=4^440.21
 ;;^UTILITY(U,$J,358.3,21514,1,5,0)
 ;;=5^PVD w/ intermittent claudication
 ;;^UTILITY(U,$J,358.3,21514,2)
 ;;=^293885
 ;;^UTILITY(U,$J,358.3,21515,0)
 ;;=414.00^^148^1333^11
 ;;^UTILITY(U,$J,358.3,21515,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,21515,1,4,0)
 ;;=4^414.00
 ;;^UTILITY(U,$J,358.3,21515,1,5,0)
 ;;=5^CAD
 ;;^UTILITY(U,$J,358.3,21515,2)
 ;;=^28512
 ;;^UTILITY(U,$J,358.3,21516,0)
 ;;=433.30^^148^1333^29
 ;;^UTILITY(U,$J,358.3,21516,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,21516,1,4,0)
 ;;=4^433.30
 ;;^UTILITY(U,$J,358.3,21516,1,5,0)
 ;;=5^Occ & Sten Mult/Bil Art w/o CRB
 ;;^UTILITY(U,$J,358.3,21516,2)
 ;;=^295803
 ;;^UTILITY(U,$J,358.3,21517,0)
 ;;=433.80^^148^1333^30
 ;;^UTILITY(U,$J,358.3,21517,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,21517,1,4,0)
 ;;=4^433.80
 ;;^UTILITY(U,$J,358.3,21517,1,5,0)
 ;;=5^Occ & Sten Precereb Art
 ;;^UTILITY(U,$J,358.3,21517,2)
 ;;=^295804
 ;;^UTILITY(U,$J,358.3,21518,0)
 ;;=444.21^^148^1333^19
 ;;^UTILITY(U,$J,358.3,21518,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,21518,1,4,0)
 ;;=4^444.21
 ;;^UTILITY(U,$J,358.3,21518,1,5,0)
 ;;=5^Embolism/Thrombosis UE
 ;;^UTILITY(U,$J,358.3,21518,2)
 ;;=^269789
 ;;^UTILITY(U,$J,358.3,21519,0)
 ;;=444.22^^148^1333^18
 ;;^UTILITY(U,$J,358.3,21519,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,21519,1,4,0)
 ;;=4^444.22
 ;;^UTILITY(U,$J,358.3,21519,1,5,0)
 ;;=5^Embolism/Thrombosis LE
 ;;^UTILITY(U,$J,358.3,21519,2)
 ;;=^269790
 ;;^UTILITY(U,$J,358.3,21520,0)
 ;;=454.0^^148^1333^40
 ;;^UTILITY(U,$J,358.3,21520,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,21520,1,4,0)
 ;;=4^454.0
 ;;^UTILITY(U,$J,358.3,21520,1,5,0)
 ;;=5^Varicose Vein w/ Ulcer
 ;;^UTILITY(U,$J,358.3,21520,2)
 ;;=^125410
 ;;^UTILITY(U,$J,358.3,21521,0)
 ;;=454.9^^148^1333^39
 ;;^UTILITY(U,$J,358.3,21521,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,21521,1,4,0)
 ;;=4^454.9
 ;;^UTILITY(U,$J,358.3,21521,1,5,0)
 ;;=5^Varicose Vein
 ;;^UTILITY(U,$J,358.3,21521,2)
 ;;=^328758
 ;;^UTILITY(U,$J,358.3,21522,0)
 ;;=729.5^^148^1333^22
 ;;^UTILITY(U,$J,358.3,21522,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,21522,1,4,0)
 ;;=4^729.5
 ;;^UTILITY(U,$J,358.3,21522,1,5,0)
 ;;=5^Leg Pain
 ;;^UTILITY(U,$J,358.3,21522,2)
 ;;=^89086
 ;;^UTILITY(U,$J,358.3,21523,0)
 ;;=V81.2^^148^1333^34
 ;;^UTILITY(U,$J,358.3,21523,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,21523,1,4,0)
 ;;=4^V81.2
 ;;^UTILITY(U,$J,358.3,21523,1,5,0)
 ;;=5^Screen-Cardiovasc NEC
 ;;^UTILITY(U,$J,358.3,21523,2)
 ;;=^295689
 ;;^UTILITY(U,$J,358.3,21524,0)
 ;;=424.0^^148^1333^23
 ;;^UTILITY(U,$J,358.3,21524,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,21524,1,4,0)
 ;;=4^424.0
 ;;^UTILITY(U,$J,358.3,21524,1,5,0)
 ;;=5^Mitral Valve Disorders
 ;;^UTILITY(U,$J,358.3,21524,2)
 ;;=^78367
 ;;^UTILITY(U,$J,358.3,21525,0)
 ;;=424.1^^148^1333^10
 ;;^UTILITY(U,$J,358.3,21525,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,21525,1,4,0)
 ;;=4^424.1
 ;;^UTILITY(U,$J,358.3,21525,1,5,0)
 ;;=5^Aortic Valve Disorder
 ;;^UTILITY(U,$J,358.3,21525,2)
 ;;=^9330
 ;;^UTILITY(U,$J,358.3,21526,0)
 ;;=424.2^^148^1333^37
 ;;^UTILITY(U,$J,358.3,21526,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,21526,1,4,0)
 ;;=4^424.2
 ;;^UTILITY(U,$J,358.3,21526,1,5,0)
 ;;=5^Tricuspid Valve Disorders,Nonrheumatic
 ;;^UTILITY(U,$J,358.3,21526,2)
 ;;=^269715
 ;;^UTILITY(U,$J,358.3,21527,0)
 ;;=424.3^^148^1333^33
 ;;^UTILITY(U,$J,358.3,21527,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,21527,1,4,0)
 ;;=4^424.3
 ;;^UTILITY(U,$J,358.3,21527,1,5,0)
 ;;=5^Pulmonary Valve Disorder
 ;;^UTILITY(U,$J,358.3,21527,2)
 ;;=^101164
 ;;^UTILITY(U,$J,358.3,21528,0)
 ;;=425.11^^148^1333^20
 ;;^UTILITY(U,$J,358.3,21528,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,21528,1,4,0)
 ;;=4^425.11
 ;;^UTILITY(U,$J,358.3,21528,1,5,0)
 ;;=5^Hypertrophic Obs Cardiomyopathy
 ;;^UTILITY(U,$J,358.3,21528,2)
 ;;=^340520
 ;;^UTILITY(U,$J,358.3,21529,0)
 ;;=428.1^^148^1333^21
 ;;^UTILITY(U,$J,358.3,21529,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,21529,1,4,0)
 ;;=4^428.1
 ;;^UTILITY(U,$J,358.3,21529,1,5,0)
 ;;=5^Left Heart Failure
 ;;^UTILITY(U,$J,358.3,21529,2)
 ;;=^68721
 ;;^UTILITY(U,$J,358.3,21530,0)
 ;;=428.21^^148^1333^7
 ;;^UTILITY(U,$J,358.3,21530,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,21530,1,4,0)
 ;;=4^428.21
 ;;^UTILITY(U,$J,358.3,21530,1,5,0)
 ;;=5^Acute Systolic Ht Failure
 ;;^UTILITY(U,$J,358.3,21530,2)
 ;;=^328494
 ;;^UTILITY(U,$J,358.3,21531,0)
 ;;=428.22^^148^1333^17
 ;;^UTILITY(U,$J,358.3,21531,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,21531,1,4,0)
 ;;=4^428.22
 ;;^UTILITY(U,$J,358.3,21531,1,5,0)
 ;;=5^Chr Systolic Ht Failure
 ;;^UTILITY(U,$J,358.3,21531,2)
 ;;=^328495
 ;;^UTILITY(U,$J,358.3,21532,0)
 ;;=428.23^^148^1333^5
 ;;^UTILITY(U,$J,358.3,21532,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,21532,1,4,0)
 ;;=4^428.23
 ;;^UTILITY(U,$J,358.3,21532,1,5,0)
 ;;=5^Ac on Chr Sys Ht Failure
 ;;^UTILITY(U,$J,358.3,21532,2)
 ;;=^328496
 ;;^UTILITY(U,$J,358.3,21533,0)
 ;;=428.31^^148^1333^6
 ;;^UTILITY(U,$J,358.3,21533,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,21533,1,4,0)
 ;;=4^428.31
 ;;^UTILITY(U,$J,358.3,21533,1,5,0)
 ;;=5^Acute Diastolic Ht Failure
 ;;^UTILITY(U,$J,358.3,21533,2)
 ;;=^328497
 ;;^UTILITY(U,$J,358.3,21534,0)
 ;;=428.32^^148^1333^16
 ;;^UTILITY(U,$J,358.3,21534,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,21534,1,4,0)
 ;;=4^428.32
 ;;^UTILITY(U,$J,358.3,21534,1,5,0)
 ;;=5^Chr Diastolic Ht Failure
 ;;^UTILITY(U,$J,358.3,21534,2)
 ;;=^328498
 ;;^UTILITY(U,$J,358.3,21535,0)
 ;;=428.33^^148^1333^4
 ;;^UTILITY(U,$J,358.3,21535,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,21535,1,4,0)
 ;;=4^428.33
 ;;^UTILITY(U,$J,358.3,21535,1,5,0)
 ;;=5^Ac on Chr Diastolic Ht Failure
 ;;^UTILITY(U,$J,358.3,21535,2)
 ;;=^328499
 ;;^UTILITY(U,$J,358.3,21536,0)
 ;;=428.41^^148^1333^2
 ;;^UTILITY(U,$J,358.3,21536,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,21536,1,4,0)
 ;;=4^428.41
 ;;^UTILITY(U,$J,358.3,21536,1,5,0)
 ;;=5^Ac Combined Sys & Diast Ht Fail
 ;;^UTILITY(U,$J,358.3,21536,2)
 ;;=^328500
 ;;^UTILITY(U,$J,358.3,21537,0)
 ;;=428.42^^148^1333^15
 ;;^UTILITY(U,$J,358.3,21537,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,21537,1,4,0)
 ;;=4^428.42
 ;;^UTILITY(U,$J,358.3,21537,1,5,0)
 ;;=5^Chr Combined Sys & Dias Ht Fail
 ;;^UTILITY(U,$J,358.3,21537,2)
 ;;=^328501
 ;;^UTILITY(U,$J,358.3,21538,0)
 ;;=428.43^^148^1333^3
 ;;^UTILITY(U,$J,358.3,21538,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,21538,1,4,0)
 ;;=4^428.43
 ;;^UTILITY(U,$J,358.3,21538,1,5,0)
 ;;=5^Ac on Chr Combined Ht Failure
 ;;^UTILITY(U,$J,358.3,21538,2)
 ;;=^328502
 ;;^UTILITY(U,$J,358.3,21539,0)
 ;;=441.01^^148^1333^35
 ;;^UTILITY(U,$J,358.3,21539,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,21539,1,4,0)
 ;;=4^441.01
 ;;^UTILITY(U,$J,358.3,21539,1,5,0)
 ;;=5^Thoracic Aorta Aneurysm Dissection
 ;;^UTILITY(U,$J,358.3,21539,2)
 ;;=^303289
 ;;^UTILITY(U,$J,358.3,21540,0)
 ;;=441.02^^148^1333^1
 ;;^UTILITY(U,$J,358.3,21540,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,21540,1,4,0)
 ;;=4^441.02
 ;;^UTILITY(U,$J,358.3,21540,1,5,0)
 ;;=5^Abdominal Aortic Aneurysm Dissection
 ;;^UTILITY(U,$J,358.3,21540,2)
 ;;=^303290
 ;;^UTILITY(U,$J,358.3,21541,0)
 ;;=441.03^^148^1333^36
 ;;^UTILITY(U,$J,358.3,21541,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,21541,1,4,0)
 ;;=4^441.03
 ;;^UTILITY(U,$J,358.3,21541,1,5,0)
 ;;=5^Thoracoabdominal Aortic Aneurysm Dissection
 ;;^UTILITY(U,$J,358.3,21541,2)
 ;;=^303291
 ;;^UTILITY(U,$J,358.3,21542,0)
 ;;=396.0^^148^1333^27
 ;;^UTILITY(U,$J,358.3,21542,1,0)
 ;;=^358.31IA^5^2
 ;;^UTILITY(U,$J,358.3,21542,1,4,0)
 ;;=4^396.0
 ;;^UTILITY(U,$J,358.3,21542,1,5,0)
 ;;=5^Mitral/Aortic Valve Stenosis
