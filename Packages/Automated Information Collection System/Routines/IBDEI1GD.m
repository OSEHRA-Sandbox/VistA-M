IBDEI1GD ; ; 12-AUG-2014
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;MAY 15, 2014
 Q:'DIFQR(358.5)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.5,192,0)
 ;;=Patient name^237^3
 ;;^UTILITY(U,$J,358.5,192,2,0)
 ;;=^358.52^1^1
 ;;^UTILITY(U,$J,358.5,192,2,1,0)
 ;;=Name:^^^1^1^1^7^30^1
 ;;^UTILITY(U,$J,358.5,193,0)
 ;;=SSN^237^4
 ;;^UTILITY(U,$J,358.5,193,2,0)
 ;;=^358.52^1^1
 ;;^UTILITY(U,$J,358.5,193,2,1,0)
 ;;=SSN:^^^39^1^1^44^15^1
 ;;^UTILITY(U,$J,358.5,194,0)
 ;;=Age^237^5
 ;;^UTILITY(U,$J,358.5,194,2,0)
 ;;=^358.52^2^2
 ;;^UTILITY(U,$J,358.5,194,2,1,0)
 ;;=DOB:^^^60^1^1^65^12^1
 ;;^UTILITY(U,$J,358.5,194,2,2,0)
 ;;=Age:^^^78^1^1^83^3^2
 ;;^UTILITY(U,$J,358.5,195,0)
 ;;=HEADER^240
 ;;^UTILITY(U,$J,358.5,195,2,0)
 ;;=^358.52^1^1
 ;;^UTILITY(U,$J,358.5,195,2,1,0)
 ;;=NATIONAL TBI^^^0^0
 ;;^UTILITY(U,$J,358.5,196,0)
 ;;=Patient name^241^3
 ;;^UTILITY(U,$J,358.5,196,2,0)
 ;;=^358.52^1^1
 ;;^UTILITY(U,$J,358.5,196,2,1,0)
 ;;=Name:^^^1^1^1^7^30^1
 ;;^UTILITY(U,$J,358.5,197,0)
 ;;=SSN^241^4
 ;;^UTILITY(U,$J,358.5,197,2,0)
 ;;=^358.52^1^1
 ;;^UTILITY(U,$J,358.5,197,2,1,0)
 ;;=SSN:^^^39^1^1^44^15^1
 ;;^UTILITY(U,$J,358.5,198,0)
 ;;=Age^241^5
 ;;^UTILITY(U,$J,358.5,198,2,0)
 ;;=^358.52^2^2
 ;;^UTILITY(U,$J,358.5,198,2,1,0)
 ;;=DOB:^^^60^1^1^65^12^1
 ;;^UTILITY(U,$J,358.5,198,2,2,0)
 ;;=Age:^^^78^1^1^83^3^2
 ;;^UTILITY(U,$J,358.5,199,0)
 ;;=HEADER^245
 ;;^UTILITY(U,$J,358.5,199,2,0)
 ;;=^358.52^1^1
 ;;^UTILITY(U,$J,358.5,199,2,1,0)
 ;;=NATIONAL TELEDERM^^^0^0
 ;;^UTILITY(U,$J,358.5,200,0)
 ;;=Patient name^246^3
 ;;^UTILITY(U,$J,358.5,200,2,0)
 ;;=^358.52^1^1
 ;;^UTILITY(U,$J,358.5,200,2,1,0)
 ;;=Name:^^^1^1^1^7^30^1
 ;;^UTILITY(U,$J,358.5,201,0)
 ;;=SSN^246^4
 ;;^UTILITY(U,$J,358.5,201,2,0)
 ;;=^358.52^1^1
 ;;^UTILITY(U,$J,358.5,201,2,1,0)
 ;;=SSN:^^^39^1^1^44^15^1
 ;;^UTILITY(U,$J,358.5,202,0)
 ;;=Age^246^5
 ;;^UTILITY(U,$J,358.5,202,2,0)
 ;;=^358.52^2^2
 ;;^UTILITY(U,$J,358.5,202,2,1,0)
 ;;=DOB:^^^60^1^1^65^12^1
 ;;^UTILITY(U,$J,358.5,202,2,2,0)
 ;;=Age:^^^78^1^1^83^3^2
 ;;^UTILITY(U,$J,358.5,203,0)
 ;;=HEADER^249
 ;;^UTILITY(U,$J,358.5,203,2,0)
 ;;=^358.52^1^1
 ;;^UTILITY(U,$J,358.5,203,2,1,0)
 ;;=NATIONAL TELEPHONE CLINICS^^^0^0
 ;;^UTILITY(U,$J,358.5,204,0)
 ;;=Patient name^251^3
 ;;^UTILITY(U,$J,358.5,204,2,0)
 ;;=^358.52^1^1
 ;;^UTILITY(U,$J,358.5,204,2,1,0)
 ;;=Name:^^^1^1^1^7^30^1
 ;;^UTILITY(U,$J,358.5,205,0)
 ;;=SSN^251^4
 ;;^UTILITY(U,$J,358.5,205,2,0)
 ;;=^358.52^1^1
 ;;^UTILITY(U,$J,358.5,205,2,1,0)
 ;;=SSN:^^^39^1^1^44^15^1
 ;;^UTILITY(U,$J,358.5,206,0)
 ;;=Age^251^5
 ;;^UTILITY(U,$J,358.5,206,2,0)
 ;;=^358.52^2^2
 ;;^UTILITY(U,$J,358.5,206,2,1,0)
 ;;=DOB:^^^60^1^1^65^12^1
 ;;^UTILITY(U,$J,358.5,206,2,2,0)
 ;;=Age:^^^78^1^1^83^3^2
 ;;^UTILITY(U,$J,358.5,207,0)
 ;;=HEADER^255
 ;;^UTILITY(U,$J,358.5,207,2,0)
 ;;=^358.52^1^1
 ;;^UTILITY(U,$J,358.5,207,2,1,0)
 ;;=NATIONAL UROLOGY EF^^^0^0
 ;;^UTILITY(U,$J,358.5,208,0)
 ;;=Patient name^256^3
 ;;^UTILITY(U,$J,358.5,208,2,0)
 ;;=^358.52^1^1
 ;;^UTILITY(U,$J,358.5,208,2,1,0)
 ;;=Name:^^^1^1^1^7^30^1
 ;;^UTILITY(U,$J,358.5,209,0)
 ;;=SSN^256^4
 ;;^UTILITY(U,$J,358.5,209,2,0)
 ;;=^358.52^1^1
 ;;^UTILITY(U,$J,358.5,209,2,1,0)
 ;;=SSN:^^^39^1^1^44^15^1
 ;;^UTILITY(U,$J,358.5,210,0)
 ;;=Age^256^5
 ;;^UTILITY(U,$J,358.5,210,2,0)
 ;;=^358.52^2^2
 ;;^UTILITY(U,$J,358.5,210,2,1,0)
 ;;=DOB:^^^60^1^1^65^12^1
 ;;^UTILITY(U,$J,358.5,210,2,2,0)
 ;;=Age:^^^78^1^1^83^3^2
 ;;^UTILITY(U,$J,358.5,211,0)
 ;;=HEADER^259
 ;;^UTILITY(U,$J,358.5,211,2,0)
 ;;=^358.52^1^1
 ;;^UTILITY(U,$J,358.5,211,2,1,0)
 ;;=NATIONAL VASCULAR SURGERY^^^0^0
 ;;^UTILITY(U,$J,358.5,212,0)
 ;;=Patient name^260^3
 ;;^UTILITY(U,$J,358.5,212,2,0)
 ;;=^358.52^1^1
 ;;^UTILITY(U,$J,358.5,212,2,1,0)
 ;;=Name:^^^1^1^1^7^30^1
 ;;^UTILITY(U,$J,358.5,213,0)
 ;;=SSN^260^4
