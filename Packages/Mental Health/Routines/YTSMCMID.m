YTSMCMID ;BAL/KTL- MCMI4 ANSWERS SPECIAL HANDLING - INI ; 9/14/18 3:19pm
 ;;5.01;MENTAL HEALTH;**151**;Dec 30, 1994;Build 92
 ;
 ; MCMI4 Scoring
 ;
 ; Initialize arrays for scoring MCMI4
 ; Percentile Ranking (INIPRS)
 ; 
INIPRS ;
 ;Initialize arrays and variables to calculate Percentile Ranking Scores
 ;Piece Number Indexed by Adjusted Base Rate
 S SCALCOD="1 Schizoid"
 S YPRS(SCALCOD,"STR")="3^7^7^8^8^8^9^9^9^10^12^12^12^12^13^13^13^16^19^20^20^20^21^21^21^23^25^26^26^26"
 S YPRS(SCALCOD,"STR")=YPRS(SCALCOD,"STR")_"^26^26^26^26^29^31^31^32^32^32^32^32^32^35^38^38^38^38^39^39^39^39^41^44^44^44^44"
 S YPRS(SCALCOD,"STR")=YPRS(SCALCOD,"STR")_"^44^45^45^47^50^53^55^58^61^64^67^68^70^73^76^78^80^82^84^87^89^91^93^94^95^97^"
 S YPRS(SCALCOD,"STR")=YPRS(SCALCOD,"STR")_"98^98^99^99^99^99^99^99^99^99^99^99^99^99^99^99^99^99^99^99^99^99^99^99^99^99^99"
 S YPRS(SCALCOD,"STR")=YPRS(SCALCOD,"STR")_"^99^99^99^99^99^99"
 S SCALCOD="2A Avoidant"
 S YPRS(SCALCOD,"STR")="3^7^7^7^8^8^8^8^9^11^14^14^15^15^16^16^17^19^21^21^22^22^23^23^24^27^28^29^29^29"
 S YPRS(SCALCOD,"STR")=YPRS(SCALCOD,"STR")_"^30^30^30^31^33^34^35^35^35^35^35^36^37^39^40^40^40^40^40^41^41^43^44^45^45^45^46"
 S YPRS(SCALCOD,"STR")=YPRS(SCALCOD,"STR")_"^46^47^48^49^50^51^51^52^53^54^54^55^56^58^59^60^61^63^67^71^75^78^81^84^86^88^"
 S YPRS(SCALCOD,"STR")=YPRS(SCALCOD,"STR")_"89^90^91^91^92^92^93^93^94^94^94^95^95^95^96^96^96^96^97^97^97^98^98^98^99^99^99"
 S YPRS(SCALCOD,"STR")=YPRS(SCALCOD,"STR")_"^99^99^99^99^99^99"
 S SCALCOD="2B Melancholic"
 S YPRS(SCALCOD,"STR")="3^6^7^8^8^8^8^9^10^13^13^13^14^15^16^18^20^21^21^21^22^22^24^26^26^27^27^27^28^30"
 S YPRS(SCALCOD,"STR")=YPRS(SCALCOD,"STR")_"^31^32^32^32^32^33^33^35^36^37^37^37^38^38^39^40^41^41^41^42^42^43^44^45^46^46^"
 S YPRS(SCALCOD,"STR")=YPRS(SCALCOD,"STR")_"47^47^47^48^49^50^50^51^52^53^53^54^54^55^57^59^60^63^67^71^76^80^84^86^88^89^90"
 S YPRS(SCALCOD,"STR")=YPRS(SCALCOD,"STR")_"^90^91^91^92^92^93^93^93^94^94^94^95^95^95^96^96^96^97^97^97^98^98^98^99^99^99^99"
 S YPRS(SCALCOD,"STR")=YPRS(SCALCOD,"STR")_"^99^99^99^99^99^99"
 S SCALCOD="3 Dependent"
 S YPRS(SCALCOD,"STR")="3^6^7^7^8^8^8^8^8^8^11^13^14^14^14^15^15^15^15^15^19^23^24^24^24^24^24^24^24^24^27"
 S YPRS(SCALCOD,"STR")=YPRS(SCALCOD,"STR")_"^31^31^32^32^32^32^32^32^32^36^40^40^40^40^40^40^40^40^40^43^46^46^46^46^46^46"
 S YPRS(SCALCOD,"STR")=YPRS(SCALCOD,"STR")_"^47^47^47^50^52^53^53^56^59^60^60^63^65^66^66^69^71^72^74^77^79^82^85^87^88^90^92"
 S YPRS(SCALCOD,"STR")=YPRS(SCALCOD,"STR")_"^92^93^94^94^94^94^95^95^95^96^96^96^97^97^97^97^98^98^98^98^99^99^99^99^99^99^"
 S YPRS(SCALCOD,"STR")=YPRS(SCALCOD,"STR")_"99^99^99^99^99^99"
 S SCALCOD="4A Histrionic"
 S YPRS(SCALCOD,"STR")="1^2^2^2^2^2^3^4^4^4^4^6^8^8^8^8^10^12^12^12^12^14^16^16^16^17^18^20^21^21^21^23^"
 S YPRS(SCALCOD,"STR")=YPRS(SCALCOD,"STR")_"25^25^25^26^26^28^31^31^31^31^31^33^34^35^35^35^35^37^39^39^39^40^42^44^44^45^45"
 S YPRS(SCALCOD,"STR")=YPRS(SCALCOD,"STR")_"^45^48^50^53^55^55^58^60^63^65^69^72^73^76^79^79^81^83^85^87^88^90^92^92^94^95^96"
 S YPRS(SCALCOD,"STR")=YPRS(SCALCOD,"STR")_"^97^97^97^97^98^99^99^99^99^99^99^99^99^99^99"
 S SCALCOD="4B Turbulent"
 S YPRS(SCALCOD,"STR")="2^4^4^4^4^4^6^8^8^8^8^9^11^12^13^13^13^13^15^17^18^18^18^18^20^22^22^22^22^23^25"
 S YPRS(SCALCOD,"STR")=YPRS(SCALCOD,"STR")_"^26^26^27^27^27^28^30^30^30^31^31^33^35^36^36^36^36^38^40^40^40^41^41^42^44^44^44"
 S YPRS(SCALCOD,"STR")=YPRS(SCALCOD,"STR")_"^45^45^47^50^52^55^57^59^61^63^66^70^72^74^77^79^83^87^88^88^90^91^91^92^93^95^"
 S YPRS(SCALCOD,"STR")=YPRS(SCALCOD,"STR")_"95^96^97^98^98^98^98^98^98^98^99^99^99^99^99^99^99"
 S SCALCOD="5 Narcissistic"
 S YPRS(SCALCOD,"STR")="3^7^8^8^9^9^9^9^9^9^9^9^12^14^14^14^14^14^14^15^15^15^15^15^19^24^24^25^25^25^26"
 S YPRS(SCALCOD,"STR")=YPRS(SCALCOD,"STR")_"^26^26^26^26^26^30^33^33^34^34^34^34^34^34^34^34^34^38^43^43^43^44^44^44^44^44^44"
 S YPRS(SCALCOD,"STR")=YPRS(SCALCOD,"STR")_"^44^45^48^52^55^59^59^63^66^69^72^74^77^78^80^82^82^84^86^88^90^91^92^94^94^95^"
 S YPRS(SCALCOD,"STR")=YPRS(SCALCOD,"STR")_"96^96^97^97^97^98^98^98^98^98^99^99^99^99^99^99^99^99^99^99^99^99^99^99^99^99^99"
 S YPRS(SCALCOD,"STR")=YPRS(SCALCOD,"STR")_"^99^99^99^99^99"
 S SCALCOD="6A Antisocial"
 S YPRS(SCALCOD,"STR")="6^12^12^13^13^14^14^14^14^14^14^14^14^14^14^14^14^14^14^14^19^24^25^25^26^26^26^"
 S YPRS(SCALCOD,"STR")=YPRS(SCALCOD,"STR")_"27^27^27^27^27^27^27^27^27^27^27^27^28^32^36^37^37^37^38^38^38^38^38^38^38^38^38"
 S YPRS(SCALCOD,"STR")=YPRS(SCALCOD,"STR")_"^38^38^38^38^38^39^43^47^51^55^58^62^65^68^69^72^75^78^81^84^86^87^89^90^92^93^94"
 S YPRS(SCALCOD,"STR")=YPRS(SCALCOD,"STR")_"^94^95^96^96^97^97^97^97^98^98^99^99^99^99^99^99^99^99^99^99^99^99^99^99^99^99^"
 S YPRS(SCALCOD,"STR")=YPRS(SCALCOD,"STR")_"99^99^99^99^99^99^99^99^99"
 S SCALCOD="6B Sadistic"
 S YPRS(SCALCOD,"STR")="5^11^13^14^15^16^16^16^16^16^17^17^17^17^17^19^22^22^22^22^22^22^22^22^22^22^22^"
 S YPRS(SCALCOD,"STR")=YPRS(SCALCOD,"STR")_"22^22^22^28^33^33^34^34^34^34^34^34^34^34^34^34^34^34^38^41^42^42^42^42^42^42^42"
 S YPRS(SCALCOD,"STR")=YPRS(SCALCOD,"STR")_"^42^42^42^42^42^43^47^54^57^61^64^67^70^72^75^77^79^81^83^86^89^91^92^93^95^96^97"
 S YPRS(SCALCOD,"STR")=YPRS(SCALCOD,"STR")_"^97^98^98^99^99^99^99^99^99^99^99^99^99^99^99^99^99^99^99^99^99^99^99^99^99^99^"
 S YPRS(SCALCOD,"STR")=YPRS(SCALCOD,"STR")_"99^99^99^99^99^99^99^99^99"
 S SCALCOD="7 Compulsive"
 S YPRS(SCALCOD,"STR")="1^1^1^1^1^1^1^1^1^1^1^1^1^1^2^2^2^2^2^3^3^3^3^3^3^4^4^5^6^6^7^9^9^9^10^12^12^12^"
 S YPRS(SCALCOD,"STR")=YPRS(SCALCOD,"STR")_"12^14^16^17^17^19^21^21^21^24^26^27^27^28^30^32^32^33^36^39^40^41^44^46^51^55^56"
 S YPRS(SCALCOD,"STR")=YPRS(SCALCOD,"STR")_"^58^61^65^70^72^74^74^79^83^84^85^87^87^87^87^91^94^94^95^95^95^96^96^96^96^96^96"
 S YPRS(SCALCOD,"STR")=YPRS(SCALCOD,"STR")_"^96^96^96^96^96^96^96^96^98"
 S SCALCOD="8A Negativistic"
 S YPRS(SCALCOD,"STR")="3^8^9^10^11^11^12^12^12^12^15^17^17^18^18^18^18^18^18^18^21^24^25^25^25^26^26^26"
 S YPRS(SCALCOD,"STR")=YPRS(SCALCOD,"STR")_"^26^26^28^31^31^31^31^31^31^31^31^31^34^37^37^37^37^37^37^37^37^37^40^43^43^43^43"
 S YPRS(SCALCOD,"STR")=YPRS(SCALCOD,"STR")_"^43^43^43^43^43^45^47^50^53^53^55^58^60^62^65^67^67^69^71^72^74^78^82^85^89^91^"
 S YPRS(SCALCOD,"STR")=YPRS(SCALCOD,"STR")_"94^95^97^97^98^98^98^98^98^98^98^99^99^99^99^99^99^99^99^99^99^99^99^99^99^99^99"
 S YPRS(SCALCOD,"STR")=YPRS(SCALCOD,"STR")_"^99^99^99^99^99^99^99^99"
 S SCALCOD="8B Masochistic"
 S YPRS(SCALCOD,"STR")="4^9^10^10^11^11^11^11^11^11^12^13^16^19^19^20^20^20^20^21^21^21^22^22^25^27^27^28"
 S YPRS(SCALCOD,"STR")=YPRS(SCALCOD,"STR")_"^28^28^29^29^29^30^31^32^34^35^36^36^36^36^36^36^37^37^38^39^40^42^42^42^42^42^"
 S YPRS(SCALCOD,"STR")=YPRS(SCALCOD,"STR")_"42^43^43^44^45^47^50^52^54^56^58^60^62^64^66^68^70^72^74^78^82^85^88^91^93^95^96"
 S YPRS(SCALCOD,"STR")=YPRS(SCALCOD,"STR")_"^97^97^97^97^97^98^98^98^98^98^98^98^98^99^99^99^99^99^99^99^99^99^99^99^99^99^99"
 S YPRS(SCALCOD,"STR")=YPRS(SCALCOD,"STR")_"^99^99^99^99^99^99^99^99"
 S SCALCOD="S Schizotypal"
 S YPRS(SCALCOD,"STR")="4^10^11^13^13^13^13^13^13^13^14^14^17^20^21^21^21^21^21^21^21^21^21^22^25^28^28^"
 S YPRS(SCALCOD,"STR")=YPRS(SCALCOD,"STR")_"28^28^29^29^29^29^29^29^30^33^35^35^35^36^36^36^36^36^36^37^38^40^41^41^41^41^41"
 S YPRS(SCALCOD,"STR")=YPRS(SCALCOD,"STR")_"^41^41^42^42^43^45^49^53^55^58^62^65^70^73^76^78^80^83^85^88^90^91^93^95^96^97^98"
 S YPRS(SCALCOD,"STR")=YPRS(SCALCOD,"STR")_"^98^98^98^98^98^99^99^99^99^99^99^99^99^99^99^99^99^99^99^99^99^99^99^99^99^99^"
 S YPRS(SCALCOD,"STR")=YPRS(SCALCOD,"STR")_"99^99^99^99^99^99^99^99^99"
 S SCALCOD="C Borderline"
 S YPRS(SCALCOD,"STR")="6^13^15^16^17^17^17^17^18^18^21^24^24^25^25^25^25^25^25^26^28^31^31^31^31^31^32^"
 S YPRS(SCALCOD,"STR")=YPRS(SCALCOD,"STR")_"32^32^33^35^36^36^36^36^36^37^37^38^39^41^42^42^42^42^42^43^43^44^44^46^47^47^47"
 S YPRS(SCALCOD,"STR")=YPRS(SCALCOD,"STR")_"^47^47^48^48^49^50^52^54^55^57^58^60^61^63^65^66^68^70^72^75^79^82^85^88^90^92^93"
 S YPRS(SCALCOD,"STR")=YPRS(SCALCOD,"STR")_"^94^94^95^95^96^96^97^97^97^97^97^98^98^99^99^99^99^99^99^99^99^99^99^99^99^99^"
 S YPRS(SCALCOD,"STR")=YPRS(SCALCOD,"STR")_"99^99^99^99^99^99^99^99^99"
 S SCALCOD="P Paranoid"
 S YPRS(SCALCOD,"STR")="7^16^19^20^21^21^21^21^21^21^21^21^21^21^21^24^27^27^27^27^27^27^27^27^27^27^27^"
 S YPRS(SCALCOD,"STR")=YPRS(SCALCOD,"STR")_"27^27^27^33^38^38^39^39^39^39^39^39^39^39^39^39^39^39^41^44^44^44^44^44^44^44^44"
 S YPRS(SCALCOD,"STR")=YPRS(SCALCOD,"STR")_"^44^44^44^44^44^44^47^51^54^58^61^64^67^69^70^71^73^75^77^79^81^83^86^87^89^91^93"
 S YPRS(SCALCOD,"STR")=YPRS(SCALCOD,"STR")_"^95^96^97^98^98^98^98^98^99^99^99^99^99^99^99^99^99^99^99^99^99^99^99^99^99^99^"
 S YPRS(SCALCOD,"STR")=YPRS(SCALCOD,"STR")_"99^99^99^99^99^99^99^99^99"
 S SCALCOD="A Generalized Anxiety"
 S YPRS(SCALCOD,"STR")="5^10^12^13^13^13^13^13^13^13^13^13^13^13^13^16^20^21^21^21^21^21^21^21^21^21^21^"
 S YPRS(SCALCOD,"STR")=YPRS(SCALCOD,"STR")_"21^21^21^25^29^29^30^30^30^30^30^30^30^30^30^30^30^30^33^37^37^37^37^37^37^37^37"
 S YPRS(SCALCOD,"STR")=YPRS(SCALCOD,"STR")_"^37^37^37^37^37^37^40^44^44^44^44^44^44^44^44^44^44^44^44^44^44^48^52^55^57^58^61"
 S YPRS(SCALCOD,"STR")=YPRS(SCALCOD,"STR")_"^64^64^67^70^73^74^75^77^79^80^81^83^84^86^87^88^90^91^91^92^94^94^95^96^97^97^"
 S YPRS(SCALCOD,"STR")=YPRS(SCALCOD,"STR")_"98^98^98^99^99^99^99^99^99"
 S SCALCOD="H Somatic Symptom"
 S YPRS(SCALCOD,"STR")="7^15^16^17^17^17^17^17^17^17^19^22^23^24^24^24^24^24^24^24^27^30^30^31^31^31^31^"
 S YPRS(SCALCOD,"STR")=YPRS(SCALCOD,"STR")_"31^31^31^34^37^37^37^37^37^38^38^38^38^40^42^42^42^42^42^42^42^42^42^44^46^47^47"
 S YPRS(SCALCOD,"STR")=YPRS(SCALCOD,"STR")_"^47^47^47^47^47^47^50^53^53^53^56^59^59^59^62^65^66^66^69^72^73^76^78^79^83^85^86"
 S YPRS(SCALCOD,"STR")=YPRS(SCALCOD,"STR")_"^87^89^92^93^95^96^96^96^96^96^96^96^96^96^96^96^96^96^97^98^98^98^98^98^98^98^"
 S YPRS(SCALCOD,"STR")=YPRS(SCALCOD,"STR")_"98^98^98^98^98^98^99^99^99"
 S SCALCOD="N Bipolar Spectrum"
 S YPRS(SCALCOD,"STR")="1^3^4^4^5^5^5^5^5^5^5^5^8^11^11^12^12^12^12^12^12^12^12^12^15^19^20^20^20^21^21^"
 S YPRS(SCALCOD,"STR")=YPRS(SCALCOD,"STR")_"21^21^21^21^21^25^29^30^30^30^30^30^30^30^30^30^30^34^37^37^38^38^38^38^38^38^38"
 S YPRS(SCALCOD,"STR")=YPRS(SCALCOD,"STR")_"^38^38^43^48^49^53^56^56^59^63^63^66^70^70^73^76^77^79^81^81^84^85^86^86^88^89^90"
 S YPRS(SCALCOD,"STR")=YPRS(SCALCOD,"STR")_"^91^92^92^92^93^94^95^95^95^96^96^97^97^97^97^98^98^98^98^98^99^99^99^99^99^99^"
 S YPRS(SCALCOD,"STR")=YPRS(SCALCOD,"STR")_"99^99^99^99^99"
 S SCALCOD="D Persistent Depression"
 S YPRS(SCALCOD,"STR")="4^8^9^10^10^10^10^12^15^16^17^17^17^17^20^22^23^23^23^23^25^27^27^27^27^27^29^31"
 S YPRS(SCALCOD,"STR")=YPRS(SCALCOD,"STR")_"^31^31^31^31^31^32^34^34^34^34^34^34^36^38^38^38^38^38^38^41^43^43^43^43^43^43^45"
 S YPRS(SCALCOD,"STR")=YPRS(SCALCOD,"STR")_"^46^46^46^46^46^48^52^54^55^57^58^59^61^62^64^65^66^68^70^73^75^77^78^79^79^81^"
 S YPRS(SCALCOD,"STR")=YPRS(SCALCOD,"STR")_"82^83^84^85^87^88^88^88^88^89^90^91^91^91^91^92^93^94^94^94^94^95^96^96^96^96^97"
 S YPRS(SCALCOD,"STR")=YPRS(SCALCOD,"STR")_"^97^98^98^98^98^99^99^99"
 S SCALCOD="B Alcohol Use"
 S YPRS(SCALCOD,"STR")="14^29^32^33^34^34^34^34^34^34^34^34^34^34^34^34^34^34^34^34^34^34^34^34^34^34^34"
 S YPRS(SCALCOD,"STR")=YPRS(SCALCOD,"STR")_"^34^34^34^34^34^34^34^34^34^34^34^34^34^34^34^34^34^34^34^34^34^34^34^34^34^34^34"
 S YPRS(SCALCOD,"STR")=YPRS(SCALCOD,"STR")_"^34^34^34^35^35^36^48^60^61^61^61^61^62^63^70^76^76^76^76^77^78^81^84^85^87^89^"
 S YPRS(SCALCOD,"STR")=YPRS(SCALCOD,"STR")_"89^90^91^92^92^93^93^94^94^95^95^95^95^95^96^96^96^97^97^97^97^98^98^98^98^99^99"
 S YPRS(SCALCOD,"STR")=YPRS(SCALCOD,"STR")_"^99^99^99^99^99^99^99^99^99"
 S SCALCOD="T Drug Use"
 S YPRS(SCALCOD,"STR")="10^22^23^24^24^24^24^24^24^24^24^24^24^24^24^24^24^24^24^24^24^24^24^24^24^24^24"
 S YPRS(SCALCOD,"STR")=YPRS(SCALCOD,"STR")_"^24^24^24^24^24^24^24^24^24^24^24^24^24^24^24^24^24^24^24^24^24^24^24^24^24^24^24"
 S YPRS(SCALCOD,"STR")=YPRS(SCALCOD,"STR")_"^24^24^24^24^24^26^36^48^56^63^64^68^72^74^77^78^80^80^81^82^82^83^84^85^86^86^"
 S YPRS(SCALCOD,"STR")=YPRS(SCALCOD,"STR")_"87^88^88^89^90^90^91^91^91^92^92^92^92^93^93^94^94^94^95^95^95^95^96^97^97^97^98"
 S YPRS(SCALCOD,"STR")=YPRS(SCALCOD,"STR")_"^98^98^99^99^99^99^99^99^99"
 S SCALCOD="R Post-Traumatic Stress"
 S YPRS(SCALCOD,"STR")="8^16^19^20^20^21^21^21^21^21^21^21^21^21^21^21^21^21^21^21^26^32^33^33^33^33^33^"
 S YPRS(SCALCOD,"STR")=YPRS(SCALCOD,"STR")_"33^33^33^33^33^33^33^33^33^33^33^33^33^38^43^43^43^43^43^43^43^43^43^43^43^43^43"
 S YPRS(SCALCOD,"STR")=YPRS(SCALCOD,"STR")_"^43^43^43^43^43^44^48^52^56^59^61^64^66^68^69^70^72^74^76^78^80^81^82^82^83^84^84"
 S YPRS(SCALCOD,"STR")=YPRS(SCALCOD,"STR")_"^84^86^87^87^89^90^90^90^90^91^92^92^92^92^93^94^94^95^95^96^97^97^97^98^98^99^"
 S YPRS(SCALCOD,"STR")=YPRS(SCALCOD,"STR")_"99^99^99^99^99^99^99^99^99"
 S SCALCOD="SS Schizophrenic Spectrum"
 S YPRS(SCALCOD,"STR")="3^7^9^10^10^11^11^11^11^13^16^17^17^17^18^18^18^20^23^23^23^23^23^23^23^26^29^29"
 S YPRS(SCALCOD,"STR")=YPRS(SCALCOD,"STR")_"^29^29^29^29^29^29^31^34^34^34^34^34^34^34^34^38^41^42^42^42^42^42^42^42^44^47^47"
 S YPRS(SCALCOD,"STR")=YPRS(SCALCOD,"STR")_"^47^47^47^47^47^50^56^59^62^68^72^77^81^84^87^88^90^91^92^94^95^96^96^97^97^97^"
 S YPRS(SCALCOD,"STR")=YPRS(SCALCOD,"STR")_"98^98^98^99^99^99^99^99^99^99^99^99^99^99^99^99^99^99^99^99^99^99^99^99^99^99^99"
 S YPRS(SCALCOD,"STR")=YPRS(SCALCOD,"STR")_"^99^99^99^99^99^99^99^99"
 S SCALCOD="CC Major Depression"
 S YPRS(SCALCOD,"STR")="7^15^17^18^18^18^18^18^18^18^18^18^21^25^25^25^25^25^25^25^25^25^25^25^28^32^32^"
 S YPRS(SCALCOD,"STR")=YPRS(SCALCOD,"STR")_"32^32^32^32^32^32^32^32^32^35^38^38^38^38^38^38^38^38^38^38^38^41^43^43^43^43^43"
 S YPRS(SCALCOD,"STR")=YPRS(SCALCOD,"STR")_"^44^44^44^44^44^44^46^48^48^49^51^53^53^53^55^56^56^56^58^60^60^61^63^65^66^68^71"
 S YPRS(SCALCOD,"STR")=YPRS(SCALCOD,"STR")_"^73^75^76^77^78^79^80^81^82^82^83^84^85^86^87^88^89^89^91^92^92^93^94^94^95^96^"
 S YPRS(SCALCOD,"STR")=YPRS(SCALCOD,"STR")_"97^98^98^99^99^99^99^99^99"
 S SCALCOD="PP Delusional"
 S YPRS(SCALCOD,"STR")="13^28^31^33^33^34^34^34^34^34^34^34^34^34^34^34^34^34^34^34^34^34^34^34^34^34^34"
 S YPRS(SCALCOD,"STR")=YPRS(SCALCOD,"STR")_"^34^34^34^34^34^34^34^34^34^34^34^34^34^34^34^34^34^34^34^34^34^34^34^34^34^34^34"
 S YPRS(SCALCOD,"STR")=YPRS(SCALCOD,"STR")_"^34^34^34^34^34^34^45^56^63^70^74^79^81^84^85^87^89^90^91^92^94^95^96^96^97^98^"
 S YPRS(SCALCOD,"STR")=YPRS(SCALCOD,"STR")_"98^99^99^99^99^99^99^99^99^99^99^99^99^99^99^99^99^99^99^99^99^99^99^99^99^99^99"
 S YPRS(SCALCOD,"STR")=YPRS(SCALCOD,"STR")_"^99^99^99^99^99^99^99^99^99"
 D INIPRS^YTSMCMIE
 Q
