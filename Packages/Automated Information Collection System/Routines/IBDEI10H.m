IBDEI10H ; ; 12-MAY-2016
 ;;3.0;IB ENCOUNTER FORM IMP/EXP;;MAY 12, 2016
 Q:'DIFQR(358.3)  F I=1:2 S X=$T(Q+I) Q:X=""  S Y=$E($T(Q+I+1),4,999),X=$E(X,4,999) S:$A(Y)=126 I=I+1,Y=$E(Y,2,999)_$E($T(Q+I+1),5,99) S:$A(Y)=61 Y=$E(Y,2,999) X NO E  S @X=Y
Q Q
 ;;^UTILITY(U,$J,358.3,36706,2)
 ;;=^5063241
 ;;^UTILITY(U,$J,358.3,36707,0)
 ;;=Z71.9^^135^1810^1
 ;;^UTILITY(U,$J,358.3,36707,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,36707,1,3,0)
 ;;=3^Counseling or Consultation,Other
 ;;^UTILITY(U,$J,358.3,36707,1,4,0)
 ;;=4^Z71.9
 ;;^UTILITY(U,$J,358.3,36707,2)
 ;;=^5063254
 ;;^UTILITY(U,$J,358.3,36708,0)
 ;;=Z60.0^^135^1811^2
 ;;^UTILITY(U,$J,358.3,36708,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,36708,1,3,0)
 ;;=3^Phase of Life Problem
 ;;^UTILITY(U,$J,358.3,36708,1,4,0)
 ;;=4^Z60.0
 ;;^UTILITY(U,$J,358.3,36708,2)
 ;;=^5063139
 ;;^UTILITY(U,$J,358.3,36709,0)
 ;;=Z60.2^^135^1811^3
 ;;^UTILITY(U,$J,358.3,36709,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,36709,1,3,0)
 ;;=3^Problem Related to Living Alone
 ;;^UTILITY(U,$J,358.3,36709,1,4,0)
 ;;=4^Z60.2
 ;;^UTILITY(U,$J,358.3,36709,2)
 ;;=^5063140
 ;;^UTILITY(U,$J,358.3,36710,0)
 ;;=Z60.3^^135^1811^1
 ;;^UTILITY(U,$J,358.3,36710,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,36710,1,3,0)
 ;;=3^Acculturation Difficulty
 ;;^UTILITY(U,$J,358.3,36710,1,4,0)
 ;;=4^Z60.3
 ;;^UTILITY(U,$J,358.3,36710,2)
 ;;=^5063141
 ;;^UTILITY(U,$J,358.3,36711,0)
 ;;=Z60.4^^135^1811^5
 ;;^UTILITY(U,$J,358.3,36711,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,36711,1,3,0)
 ;;=3^Social Exclusion or Rejection
 ;;^UTILITY(U,$J,358.3,36711,1,4,0)
 ;;=4^Z60.4
 ;;^UTILITY(U,$J,358.3,36711,2)
 ;;=^5063142
 ;;^UTILITY(U,$J,358.3,36712,0)
 ;;=Z60.5^^135^1811^6
 ;;^UTILITY(U,$J,358.3,36712,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,36712,1,3,0)
 ;;=3^Target of (Perceived) Adverse Discrimination or Persecution
 ;;^UTILITY(U,$J,358.3,36712,1,4,0)
 ;;=4^Z60.5
 ;;^UTILITY(U,$J,358.3,36712,2)
 ;;=^5063143
 ;;^UTILITY(U,$J,358.3,36713,0)
 ;;=Z60.9^^135^1811^4
 ;;^UTILITY(U,$J,358.3,36713,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,36713,1,3,0)
 ;;=3^Problem Related to Social Environment,Unspec
 ;;^UTILITY(U,$J,358.3,36713,1,4,0)
 ;;=4^Z60.9
 ;;^UTILITY(U,$J,358.3,36713,2)
 ;;=^5063145
 ;;^UTILITY(U,$J,358.3,36714,0)
 ;;=F65.4^^135^1812^6
 ;;^UTILITY(U,$J,358.3,36714,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,36714,1,3,0)
 ;;=3^Pedophilica Disorder
 ;;^UTILITY(U,$J,358.3,36714,1,4,0)
 ;;=4^F65.4
 ;;^UTILITY(U,$J,358.3,36714,2)
 ;;=^5003655
 ;;^UTILITY(U,$J,358.3,36715,0)
 ;;=F65.2^^135^1812^1
 ;;^UTILITY(U,$J,358.3,36715,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,36715,1,3,0)
 ;;=3^Exhibitionistic Disorder
 ;;^UTILITY(U,$J,358.3,36715,1,4,0)
 ;;=4^F65.2
 ;;^UTILITY(U,$J,358.3,36715,2)
 ;;=^5003653
 ;;^UTILITY(U,$J,358.3,36716,0)
 ;;=F65.3^^135^1812^10
 ;;^UTILITY(U,$J,358.3,36716,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,36716,1,3,0)
 ;;=3^Voyeuristic Disorder
 ;;^UTILITY(U,$J,358.3,36716,1,4,0)
 ;;=4^F65.3
 ;;^UTILITY(U,$J,358.3,36716,2)
 ;;=^5003654
 ;;^UTILITY(U,$J,358.3,36717,0)
 ;;=F65.81^^135^1812^3
 ;;^UTILITY(U,$J,358.3,36717,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,36717,1,3,0)
 ;;=3^Frotteuristic Disorder
 ;;^UTILITY(U,$J,358.3,36717,1,4,0)
 ;;=4^F65.81
 ;;^UTILITY(U,$J,358.3,36717,2)
 ;;=^5003659
 ;;^UTILITY(U,$J,358.3,36718,0)
 ;;=F65.51^^135^1812^7
 ;;^UTILITY(U,$J,358.3,36718,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,36718,1,3,0)
 ;;=3^Sexual Masochism Disorder
 ;;^UTILITY(U,$J,358.3,36718,1,4,0)
 ;;=4^F65.51
 ;;^UTILITY(U,$J,358.3,36718,2)
 ;;=^5003657
 ;;^UTILITY(U,$J,358.3,36719,0)
 ;;=F65.52^^135^1812^8
 ;;^UTILITY(U,$J,358.3,36719,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,36719,1,3,0)
 ;;=3^Sexual Sadism Disorder
 ;;^UTILITY(U,$J,358.3,36719,1,4,0)
 ;;=4^F65.52
 ;;^UTILITY(U,$J,358.3,36719,2)
 ;;=^5003658
 ;;^UTILITY(U,$J,358.3,36720,0)
 ;;=F65.0^^135^1812^2
 ;;^UTILITY(U,$J,358.3,36720,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,36720,1,3,0)
 ;;=3^Fetishistic Disorder
 ;;^UTILITY(U,$J,358.3,36720,1,4,0)
 ;;=4^F65.0
 ;;^UTILITY(U,$J,358.3,36720,2)
 ;;=^5003651
 ;;^UTILITY(U,$J,358.3,36721,0)
 ;;=F65.1^^135^1812^9
 ;;^UTILITY(U,$J,358.3,36721,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,36721,1,3,0)
 ;;=3^Transvestic Disorder
 ;;^UTILITY(U,$J,358.3,36721,1,4,0)
 ;;=4^F65.1
 ;;^UTILITY(U,$J,358.3,36721,2)
 ;;=^5003652
 ;;^UTILITY(U,$J,358.3,36722,0)
 ;;=F65.89^^135^1812^4
 ;;^UTILITY(U,$J,358.3,36722,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,36722,1,3,0)
 ;;=3^Paraphilic Disorder,Other Specified
 ;;^UTILITY(U,$J,358.3,36722,1,4,0)
 ;;=4^F65.89
 ;;^UTILITY(U,$J,358.3,36722,2)
 ;;=^5003660
 ;;^UTILITY(U,$J,358.3,36723,0)
 ;;=F65.9^^135^1812^5
 ;;^UTILITY(U,$J,358.3,36723,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,36723,1,3,0)
 ;;=3^Paraphilic Disorder,Unspec
 ;;^UTILITY(U,$J,358.3,36723,1,4,0)
 ;;=4^F65.9
 ;;^UTILITY(U,$J,358.3,36723,2)
 ;;=^5003661
 ;;^UTILITY(U,$J,358.3,36724,0)
 ;;=F60.0^^135^1813^8
 ;;^UTILITY(U,$J,358.3,36724,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,36724,1,3,0)
 ;;=3^Paranoid Personality Disorder
 ;;^UTILITY(U,$J,358.3,36724,1,4,0)
 ;;=4^F60.0
 ;;^UTILITY(U,$J,358.3,36724,2)
 ;;=^5003635
 ;;^UTILITY(U,$J,358.3,36725,0)
 ;;=F60.1^^135^1813^12
 ;;^UTILITY(U,$J,358.3,36725,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,36725,1,3,0)
 ;;=3^Schizoid Personality Disorder
 ;;^UTILITY(U,$J,358.3,36725,1,4,0)
 ;;=4^F60.1
 ;;^UTILITY(U,$J,358.3,36725,2)
 ;;=^108271
 ;;^UTILITY(U,$J,358.3,36726,0)
 ;;=F21.^^135^1813^13
 ;;^UTILITY(U,$J,358.3,36726,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,36726,1,3,0)
 ;;=3^Schizotypal Personality Disorder
 ;;^UTILITY(U,$J,358.3,36726,1,4,0)
 ;;=4^F21.
 ;;^UTILITY(U,$J,358.3,36726,2)
 ;;=^5003477
 ;;^UTILITY(U,$J,358.3,36727,0)
 ;;=F60.5^^135^1813^7
 ;;^UTILITY(U,$J,358.3,36727,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,36727,1,3,0)
 ;;=3^Obsessive-Compulsive Personality Disorder
 ;;^UTILITY(U,$J,358.3,36727,1,4,0)
 ;;=4^F60.5
 ;;^UTILITY(U,$J,358.3,36727,2)
 ;;=^331918
 ;;^UTILITY(U,$J,358.3,36728,0)
 ;;=F60.4^^135^1813^5
 ;;^UTILITY(U,$J,358.3,36728,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,36728,1,3,0)
 ;;=3^Histrionic Personality Disorder
 ;;^UTILITY(U,$J,358.3,36728,1,4,0)
 ;;=4^F60.4
 ;;^UTILITY(U,$J,358.3,36728,2)
 ;;=^5003636
 ;;^UTILITY(U,$J,358.3,36729,0)
 ;;=F60.7^^135^1813^4
 ;;^UTILITY(U,$J,358.3,36729,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,36729,1,3,0)
 ;;=3^Dependent Personality Disorder
 ;;^UTILITY(U,$J,358.3,36729,1,4,0)
 ;;=4^F60.7
 ;;^UTILITY(U,$J,358.3,36729,2)
 ;;=^5003637
 ;;^UTILITY(U,$J,358.3,36730,0)
 ;;=F60.2^^135^1813^1
 ;;^UTILITY(U,$J,358.3,36730,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,36730,1,3,0)
 ;;=3^Antisocial Personality Disorder
 ;;^UTILITY(U,$J,358.3,36730,1,4,0)
 ;;=4^F60.2
 ;;^UTILITY(U,$J,358.3,36730,2)
 ;;=^9066
 ;;^UTILITY(U,$J,358.3,36731,0)
 ;;=F60.81^^135^1813^6
 ;;^UTILITY(U,$J,358.3,36731,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,36731,1,3,0)
 ;;=3^Narcissistic Personality Disorder
 ;;^UTILITY(U,$J,358.3,36731,1,4,0)
 ;;=4^F60.81
 ;;^UTILITY(U,$J,358.3,36731,2)
 ;;=^331919
 ;;^UTILITY(U,$J,358.3,36732,0)
 ;;=F60.6^^135^1813^2
 ;;^UTILITY(U,$J,358.3,36732,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,36732,1,3,0)
 ;;=3^Avoidant Personality Disorder
 ;;^UTILITY(U,$J,358.3,36732,1,4,0)
 ;;=4^F60.6
 ;;^UTILITY(U,$J,358.3,36732,2)
 ;;=^331920
 ;;^UTILITY(U,$J,358.3,36733,0)
 ;;=F60.3^^135^1813^3
 ;;^UTILITY(U,$J,358.3,36733,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,36733,1,3,0)
 ;;=3^Borderline Personality Disorder
 ;;^UTILITY(U,$J,358.3,36733,1,4,0)
 ;;=4^F60.3
 ;;^UTILITY(U,$J,358.3,36733,2)
 ;;=^331921
 ;;^UTILITY(U,$J,358.3,36734,0)
 ;;=F60.89^^135^1813^10
 ;;^UTILITY(U,$J,358.3,36734,1,0)
 ;;=^358.31IA^4^2
 ;;^UTILITY(U,$J,358.3,36734,1,3,0)
 ;;=3^Personality Disorder,Other
 ;;^UTILITY(U,$J,358.3,36734,1,4,0)
 ;;=4^F60.89
 ;;^UTILITY(U,$J,358.3,36734,2)
 ;;=^5003638
 ;;^UTILITY(U,$J,358.3,36735,0)
 ;;=F60.9^^135^1813^11
 ;;^UTILITY(U,$J,358.3,36735,1,0)
 ;;=^358.31IA^4^2
