IBDEI11S ; ; 12-MAY-2016
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;MAY 12, 2016
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,38013,1,2,0)
 ;;=2^32673
 ;;^UTILITY(U,$J,358.3,38013,1,3,0)
 ;;=3^Thoracosc w/ Thymus Resect
 ;;^UTILITY(U,$J,358.3,38014,0)
 ;;=32997^^143^1889^1^^^^1
 ;;^UTILITY(U,$J,358.3,38014,1,0)
 ;;=^358.31IA^3^2
 ;;^UTILITY(U,$J,358.3,38014,1,2,0)
 ;;=2^32997
 ;;^UTILITY(U,$J,358.3,38014,1,3,0)
 ;;=3^Total Lung Lavage
 ;;^UTILITY(U,$J,358.3,38015,0)
 ;;=99201^^144^1890^1
 ;;^UTILITY(U,$J,358.3,38015,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,38015,1,1,0)
 ;;=1^Problem Focus
 ;;^UTILITY(U,$J,358.3,38015,1,2,0)
 ;;=2^99201
 ;;^UTILITY(U,$J,358.3,38016,0)
 ;;=99202^^144^1890^2
 ;;^UTILITY(U,$J,358.3,38016,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,38016,1,1,0)
 ;;=1^Expanded Problem Focus
 ;;^UTILITY(U,$J,358.3,38016,1,2,0)
 ;;=2^99202
 ;;^UTILITY(U,$J,358.3,38017,0)
 ;;=99203^^144^1890^3
 ;;^UTILITY(U,$J,358.3,38017,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,38017,1,1,0)
 ;;=1^Detailed
 ;;^UTILITY(U,$J,358.3,38017,1,2,0)
 ;;=2^99203
 ;;^UTILITY(U,$J,358.3,38018,0)
 ;;=99204^^144^1890^4
 ;;^UTILITY(U,$J,358.3,38018,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,38018,1,1,0)
 ;;=1^Comprehensive, Moderate
 ;;^UTILITY(U,$J,358.3,38018,1,2,0)
 ;;=2^99204
 ;;^UTILITY(U,$J,358.3,38019,0)
 ;;=99205^^144^1890^5
 ;;^UTILITY(U,$J,358.3,38019,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,38019,1,1,0)
 ;;=1^Comprehensive, High
 ;;^UTILITY(U,$J,358.3,38019,1,2,0)
 ;;=2^99205
 ;;^UTILITY(U,$J,358.3,38020,0)
 ;;=99211^^144^1891^1
 ;;^UTILITY(U,$J,358.3,38020,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,38020,1,1,0)
 ;;=1^Nursing Visit/Brief MD Visit
 ;;^UTILITY(U,$J,358.3,38020,1,2,0)
 ;;=2^99211
 ;;^UTILITY(U,$J,358.3,38021,0)
 ;;=99212^^144^1891^2
 ;;^UTILITY(U,$J,358.3,38021,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,38021,1,1,0)
 ;;=1^Problem Focused
 ;;^UTILITY(U,$J,358.3,38021,1,2,0)
 ;;=2^99212
 ;;^UTILITY(U,$J,358.3,38022,0)
 ;;=99213^^144^1891^3
 ;;^UTILITY(U,$J,358.3,38022,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,38022,1,1,0)
 ;;=1^Expanded Problem Focus
 ;;^UTILITY(U,$J,358.3,38022,1,2,0)
 ;;=2^99213
 ;;^UTILITY(U,$J,358.3,38023,0)
 ;;=99214^^144^1891^4
 ;;^UTILITY(U,$J,358.3,38023,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,38023,1,1,0)
 ;;=1^Detailed
 ;;^UTILITY(U,$J,358.3,38023,1,2,0)
 ;;=2^99214
 ;;^UTILITY(U,$J,358.3,38024,0)
 ;;=99215^^144^1891^5
 ;;^UTILITY(U,$J,358.3,38024,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,38024,1,1,0)
 ;;=1^Comprehensive
 ;;^UTILITY(U,$J,358.3,38024,1,2,0)
 ;;=2^99215
 ;;^UTILITY(U,$J,358.3,38025,0)
 ;;=99241^^144^1892^1
 ;;^UTILITY(U,$J,358.3,38025,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,38025,1,1,0)
 ;;=1^Problem Focused
 ;;^UTILITY(U,$J,358.3,38025,1,2,0)
 ;;=2^99241
 ;;^UTILITY(U,$J,358.3,38026,0)
 ;;=99242^^144^1892^2
 ;;^UTILITY(U,$J,358.3,38026,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,38026,1,1,0)
 ;;=1^Expanded Problem Focus
 ;;^UTILITY(U,$J,358.3,38026,1,2,0)
 ;;=2^99242
 ;;^UTILITY(U,$J,358.3,38027,0)
 ;;=99243^^144^1892^3
 ;;^UTILITY(U,$J,358.3,38027,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,38027,1,1,0)
 ;;=1^Detailed
 ;;^UTILITY(U,$J,358.3,38027,1,2,0)
 ;;=2^99243
 ;;^UTILITY(U,$J,358.3,38028,0)
 ;;=99244^^144^1892^4
 ;;^UTILITY(U,$J,358.3,38028,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,38028,1,1,0)
 ;;=1^Comprehensive, Moderate
 ;;^UTILITY(U,$J,358.3,38028,1,2,0)
 ;;=2^99244
 ;;^UTILITY(U,$J,358.3,38029,0)
 ;;=99245^^144^1892^5
 ;;^UTILITY(U,$J,358.3,38029,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,38029,1,1,0)
 ;;=1^Comprehensive, High
 ;;^UTILITY(U,$J,358.3,38029,1,2,0)
 ;;=2^99245
 ;;^UTILITY(U,$J,358.3,38030,0)
 ;;=99024^^144^1893^1
 ;;^UTILITY(U,$J,358.3,38030,1,0)
 ;;=^358.31IA^2^2
 ;;^UTILITY(U,$J,358.3,38030,1,1,0)
 ;;=1^Post-Op Follow-up Visit
 ;;^UTILITY(U,$J,358.3,38030,1,2,0)
 ;;=2^99024
 ;;^UTILITY(U,$J,358.3,38031,0)
 ;;=E11.9^^145^1894^14
 ;;^UTILITY(U,$J,358.3,38031,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,38031,1,3,0)
 ;;=3^Diabetes Type 2 w/o Complications
 ;;^UTILITY(U,$J,358.3,38031,1,4,0)
 ;;=4^E11.9
 ;;^UTILITY(U,$J,358.3,38031,2)
 ;;=^5002666
 ;;^UTILITY(U,$J,358.3,38032,0)
 ;;=E10.9^^145^1894^13
 ;;^UTILITY(U,$J,358.3,38032,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,38032,1,3,0)
 ;;=3^Diabetes Type 1 w/o Complications
 ;;^UTILITY(U,$J,358.3,38032,1,4,0)
 ;;=4^E10.9
 ;;^UTILITY(U,$J,358.3,38032,2)
 ;;=^5002626
 ;;^UTILITY(U,$J,358.3,38033,0)
 ;;=N18.9^^145^1894^10
 ;;^UTILITY(U,$J,358.3,38033,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,38033,1,3,0)
 ;;=3^Chronic kidney disease, unspecified
 ;;^UTILITY(U,$J,358.3,38033,1,4,0)
 ;;=4^N18.9
 ;;^UTILITY(U,$J,358.3,38033,2)
 ;;=^332812
 ;;^UTILITY(U,$J,358.3,38034,0)
 ;;=D50.0^^145^1894^26
 ;;^UTILITY(U,$J,358.3,38034,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,38034,1,3,0)
 ;;=3^Iron def anemia d/t blood loss (chr)
 ;;^UTILITY(U,$J,358.3,38034,1,4,0)
 ;;=4^D50.0
 ;;^UTILITY(U,$J,358.3,38034,2)
 ;;=^267971
 ;;^UTILITY(U,$J,358.3,38035,0)
 ;;=D50.9^^145^1894^27
 ;;^UTILITY(U,$J,358.3,38035,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,38035,1,3,0)
 ;;=3^Iron def anemia, unsp
 ;;^UTILITY(U,$J,358.3,38035,1,4,0)
 ;;=4^D50.9
 ;;^UTILITY(U,$J,358.3,38035,2)
 ;;=^5002283
 ;;^UTILITY(U,$J,358.3,38036,0)
 ;;=D62.^^145^1894^4
 ;;^UTILITY(U,$J,358.3,38036,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,38036,1,3,0)
 ;;=3^Acute posthemorrhagic anemia
 ;;^UTILITY(U,$J,358.3,38036,1,4,0)
 ;;=4^D62.
 ;;^UTILITY(U,$J,358.3,38036,2)
 ;;=^267986
 ;;^UTILITY(U,$J,358.3,38037,0)
 ;;=D64.9^^145^1894^5
 ;;^UTILITY(U,$J,358.3,38037,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,38037,1,3,0)
 ;;=3^Anemia, unspecified
 ;;^UTILITY(U,$J,358.3,38037,1,4,0)
 ;;=4^D64.9
 ;;^UTILITY(U,$J,358.3,38037,2)
 ;;=^5002351
 ;;^UTILITY(U,$J,358.3,38038,0)
 ;;=F03.90^^145^1894^12
 ;;^UTILITY(U,$J,358.3,38038,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,38038,1,3,0)
 ;;=3^Dementia w/o behavioral disturb,Unsp
 ;;^UTILITY(U,$J,358.3,38038,1,4,0)
 ;;=4^F03.90
 ;;^UTILITY(U,$J,358.3,38038,2)
 ;;=^5003050
 ;;^UTILITY(U,$J,358.3,38039,0)
 ;;=I10.^^145^1894^24
 ;;^UTILITY(U,$J,358.3,38039,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,38039,1,3,0)
 ;;=3^Hypertension,Essential
 ;;^UTILITY(U,$J,358.3,38039,1,4,0)
 ;;=4^I10.
 ;;^UTILITY(U,$J,358.3,38039,2)
 ;;=^5007062
 ;;^UTILITY(U,$J,358.3,38040,0)
 ;;=G45.9^^145^1894^43
 ;;^UTILITY(U,$J,358.3,38040,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,38040,1,3,0)
 ;;=3^Transient cerebral ischemic attack, unsp
 ;;^UTILITY(U,$J,358.3,38040,1,4,0)
 ;;=4^G45.9
 ;;^UTILITY(U,$J,358.3,38040,2)
 ;;=^5003959
 ;;^UTILITY(U,$J,358.3,38041,0)
 ;;=N18.4^^145^1894^9
 ;;^UTILITY(U,$J,358.3,38041,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,38041,1,3,0)
 ;;=3^Chronic kidney disease, stage 4 (severe)
 ;;^UTILITY(U,$J,358.3,38041,1,4,0)
 ;;=4^N18.4
 ;;^UTILITY(U,$J,358.3,38041,2)
 ;;=^5015605
 ;;^UTILITY(U,$J,358.3,38042,0)
 ;;=R44.3^^145^1894^20
 ;;^UTILITY(U,$J,358.3,38042,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,38042,1,3,0)
 ;;=3^Hallucinations, unspecified
 ;;^UTILITY(U,$J,358.3,38042,1,4,0)
 ;;=4^R44.3
 ;;^UTILITY(U,$J,358.3,38042,2)
 ;;=^5019458
 ;;^UTILITY(U,$J,358.3,38043,0)
 ;;=R55.^^145^1894^41
 ;;^UTILITY(U,$J,358.3,38043,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,38043,1,3,0)
 ;;=3^Syncope and collapse
 ;;^UTILITY(U,$J,358.3,38043,1,4,0)
 ;;=4^R55.
 ;;^UTILITY(U,$J,358.3,38043,2)
 ;;=^116707
 ;;^UTILITY(U,$J,358.3,38044,0)
 ;;=R42.^^145^1894^16
 ;;^UTILITY(U,$J,358.3,38044,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,38044,1,3,0)
 ;;=3^Dizziness and giddiness
 ;;^UTILITY(U,$J,358.3,38044,1,4,0)
 ;;=4^R42.
 ;;^UTILITY(U,$J,358.3,38044,2)
 ;;=^5019450
 ;;^UTILITY(U,$J,358.3,38045,0)
 ;;=G47.9^^145^1894^39
 ;;^UTILITY(U,$J,358.3,38045,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,38045,1,3,0)
 ;;=3^Sleep disorder, unspecified
