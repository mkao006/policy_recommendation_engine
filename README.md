# Policy Recommendation Engine

This is the repository for the policy recommendation engine. The aim
is to assist countries lacking statistical and policy analysts to
identify and analyse policies available that may help.


<iframe src="http://bl.ocks.org/Kalimaha/53c4d2c8945236232495" width="600" height="400" marginwidth="0" marginheight="0" scrolling="no"></iframe>

<style>
.link {
stroke: #666;
opacity: 0.9;
stroke-width: 1.5px;
}
.node circle {
stroke: #fff;
opacity: 0.9;
stroke-width: 1.5px;
}
.node:not(:hover) .nodetext {
display: none;
}
text {
font: 15px serif;
opacity: 0.9;
pointer-events: none;
}
</style>

<script src=http://d3js.org/d3.v3.min.js></script>

<script> var links = [ { "source" : 1, "target" : 131, "value" :
 1.31452729451742 }, { "source" : 1, "target" : 82, "value" :
 1.48780804393006 }, { "source" : 1, "target" : 111, "value" :
 1.53525937177258 }, { "source" : 2, "target" : 146, "value" :
 0.920880072265894 }, { "source" : 2, "target" : 78, "value" :
 0.940957645654329 }, { "source" : 2, "target" : 102, "value" :
 0.960545297345756 }, { "source" : 3, "target" : 168, "value" :
 0.953653479210874 }, { "source" : 3, "target" : 152, "value" :
 1.0220078017862 }, { "source" : 3, "target" : 47, "value" :
 1.02234392589866 }, { "source" : 4, "target" : 116, "value" :
 0.974043813062297 }, { "source" : 4, "target" : 112, "value" :
 1.00861994554911 }, { "source" : 4, "target" : 89, "value" :
 1.04323962864693 }, { "source" : 5, "target" : 32, "value" :
 1.44955239332356 }, { "source" : 5, "target" : 14, "value" :
 1.55694107513401 }, { "source" : 5, "target" : 117, "value" :
 1.63821920099526 }, { "source" : 6, "target" : 182, "value" :
 1.19119444160882 }, { "source" : 6, "target" : 59, "value" :
 1.20319220151253 }, { "source" : 6, "target" : 78, "value" :
 1.37655213152321 }, { "source" : 7, "target" : 46, "value" :
 1.37045931512834 }, { "source" : 7, "target" : 20, "value" :
 1.65668766498696 }, { "source" : 7, "target" : 174, "value" :
 1.71018383342883 }, { "source" : 0, "target" : 2, "value" :
 1.20637844221834 }, { "source" : 0, "target" : 71, "value" :
 1.217090120178 }, { "source" : 0, "target" : 47, "value" :
 1.24330899923766 }, { "source" : 17, "target" : 45, "value" :
 1.01820681828437 }, { "source" : 17, "target" : 146, "value" :
 1.10137862487542 }, { "source" : 17, "target" : 141, "value" :
 1.2985203201483 }, { "source" : 8, "target" : 24, "value" :
 0.838362577073292 }, { "source" : 8, "target" : 158, "value" :
 1.09438576978989 }, { "source" : 8, "target" : 48, "value" :
 1.09961749350012 }, { "source" : 9, "target" : 120, "value" :
 0.602232655441791 }, { "source" : 9, "target" : 54, "value" :
 0.639259804146382 }, { "source" : 9, "target" : 186, "value" :
 0.768040477294505 }, { "source" : 36, "target" : 2, "value" :
 1.19611020180288 }, { "source" : 36, "target" : 168, "value" :
 1.42990851080529 }, { "source" : 36, "target" : 102, "value" :
 1.4479413745007 }, { "source" : 10, "target" : 170, "value" :
 1.56855749452816 }, { "source" : 10, "target" : 4, "value" :
 1.876723903123 }, { "source" : 10, "target" : 45, "value" :
 2.05825722589473 }, { "source" : 11, "target" : 121, "value" :
 1.4425791028605 }, { "source" : 11, "target" : 111, "value" :
 1.49996277790847 }, { "source" : 11, "target" : 183, "value" :
 1.97834755666622 }, { "source" : 41, "target" : 190, "value" :
 0.789337479044317 }, { "source" : 41, "target" : 67, "value" :
 0.82726644887886 }, { "source" : 41, "target" : 128, "value" :
 0.868810763776866 }, { "source" : 186, "target" : 38, "value" :
 0.71495414307359 }, { "source" : 186, "target" : 48, "value" :
 0.733536216264423 }, { "source" : 186, "target" : 9, "value" :
 0.768040477294505 }, { "source" : 18, "target" : 109, "value" :
 0.905488077232075 }, { "source" : 18, "target" : 191, "value" :
 0.928990869486456 }, { "source" : 18, "target" : 47, "value" :
 1.06682789338007 }, { "source" : 37, "target" : 56, "value" :
 1.09893293326321 }, { "source" : 37, "target" : 119, "value" :
 1.29737835818427 }, { "source" : 37, "target" : 130, "value" :
 1.43547592970923 }, { "source" : 12, "target" : 150, "value" :
 1.20614068109613 }, { "source" : 12, "target" : 134, "value" :
 2.30662598084721 }, { "source" : 12, "target" : 142, "value" :
 2.35832869698108 }, { "source" : 13, "target" : 192, "value" :
 1.29940663294756 }, { "source" : 13, "target" : 109, "value" :
 1.32392687443911 }, { "source" : 13, "target" : 108, "value" :
 1.39988619910808 }, { "source" : 14, "target" : 117, "value" :
 0.682738067393721 }, { "source" : 14, "target" : 23, "value" :
 1.30069178321288 }, { "source" : 14, "target" : 105, "value" :
 1.32361192170884 }, { "source" : 55, "target" : 189, "value" :
 0.614750722993733 }, { "source" : 55, "target" : 67, "value" :
 0.642608633750824 }, { "source" : 55, "target" : 87, "value" :
 0.852964315454793 }, { "source" : 15, "target" : 110, "value" :
 0.887991137474196 }, { "source" : 15, "target" : 44, "value" :
 1.42298756227974 }, { "source" : 15, "target" : 125, "value" :
 1.47799137800987 }, { "source" : 16, "target" : 138, "value" :
 1.42252709160877 }, { "source" : 16, "target" : 152, "value" :
 1.56673225573877 }, { "source" : 16, "target" : 122, "value" :
 1.69997913169409 }, { "source" : 19, "target" : 89, "value" :
 0.96003446478593 }, { "source" : 19, "target" : 129, "value" :
 1.03369836065786 }, { "source" : 19, "target" : 4, "value" :
 1.12078429317492 }, { "source" : 20, "target" : 189, "value" :
 0.85064892082221 }, { "source" : 20, "target" : 190, "value" :
 0.971612887849814 }, { "source" : 20, "target" : 67, "value" :
 1.05298685631974 }, { "source" : 176, "target" : 118, "value" :
 1.21414677752 }, { "source" : 176, "target" : 28, "value" :
 1.5685368954702 }, { "source" : 176, "target" : 76, "value" :
 1.60913249855578 }, { "source" : 22, "target" : 132, "value" :
 1.92102096765661 }, { "source" : 22, "target" : 137, "value" :
 2.06817242151767 }, { "source" : 22, "target" : 31, "value" :
 2.29105373318966 }, { "source" : 25, "target" : 44, "value" :
 0.910834630900881 }, { "source" : 25, "target" : 122, "value" :
 1.15635610494343 }, { "source" : 25, "target" : 126, "value" :
 1.46094334309018 }, { "source" : 83, "target" : 90, "value" :
 1.56419009848881 }, { "source" : 83, "target" : 21, "value" :
 1.5924438438337 }, { "source" : 83, "target" : 124, "value" :
 1.60434653131104 }, { "source" : 23, "target" : 117, "value" :
 0.975511109512529 }, { "source" : 23, "target" : 90, "value" :
 1.21847324468831 }, { "source" : 23, "target" : 145, "value" :
 1.22410275311965 }, { "source" : 24, "target" : 8, "value" :
 0.838362577073292 }, { "source" : 24, "target" : 48, "value" :
 0.898813888700985 }, { "source" : 24, "target" : 123, "value" :
 0.931861117429435 }, { "source" : 26, "target" : 135, "value" :
 1.49461162528901 }, { "source" : 26, "target" : 147, "value" :
 1.55181563212396 }, { "source" : 26, "target" : 91, "value" :
 1.55959206999486 }, { "source" : 28, "target" : 162, "value" :
 1.02167171095672 }, { "source" : 28, "target" : 95, "value" :
 1.24572404427195 }, { "source" : 28, "target" : 107, "value" :
 1.26741367173486 }, { "source" : 29, "target" : 113, "value" :
 0.956794162296048 }, { "source" : 29, "target" : 167, "value" :
 0.973029189538259 }, { "source" : 29, "target" : 89, "value" :
 1.48697729248095 }, { "source" : 30, "target" : 122, "value" :
 1.06793098914228 }, { "source" : 30, "target" : 40, "value" :
 1.14865142976247 }, { "source" : 30, "target" : 42, "value" :
 1.16193537350911 }, { "source" : 32, "target" : 5, "value" :
 1.44955239332356 }, { "source" : 32, "target" : 14, "value" :
 1.55385925388808 }, { "source" : 32, "target" : 23, "value" :
 1.58864646875774 }, { "source" : 33, "target" : 114, "value" :
 1.01225021776878 }, { "source" : 33, "target" : 146, "value" :
 1.07114556956056 }, { "source" : 33, "target" : 18, "value" :
 1.21497089869963 }, { "source" : 76, "target" : 62, "value" :
 1.08365560669245 }, { "source" : 76, "target" : 130, "value" :
 1.11302717762523 }, { "source" : 76, "target" : 37, "value" :
 1.1415370310435 }, { "source" : 67, "target" : 87, "value" :
 0.617144820469032 }, { "source" : 67, "target" : 55, "value" :
 0.642608633750824 }, { "source" : 67, "target" : 190, "value" :
 0.723500346667688 }, { "source" : 34, "target" : 168, "value" :
 0.761877422241889 }, { "source" : 34, "target" : 92, "value" :
 0.830995625266431 }, { "source" : 34, "target" : 39, "value" :
 0.906599367281589 }, { "source" : 193, "target" : 195, "value" :
 0.455929887515982 }, { "source" : 193, "target" : 194, "value" :
 0.683170301495052 }, { "source" : 193, "target" : 114, "value" :
 1.11673495056999 }, { "source" : 35, "target" : 4, "value" :
 1.1969376870027 }, { "source" : 35, "target" : 89, "value" :
 1.31770432237712 }, { "source" : 35, "target" : 159, "value" :
 1.34296384156125 }, { "source" : 84, "target" : 88, "value" :
 1.65774601940357 }, { "source" : 84, "target" : 196, "value" :
 1.70471414477474 }, { "source" : 84, "target" : 156, "value" :
 1.71242533467083 }, { "source" : 38, "target" : 158, "value" :
 0.536166159724376 }, { "source" : 38, "target" : 48, "value" :
 0.589148245646065 }, { "source" : 38, "target" : 159, "value" :
 0.611330889530299 }, { "source" : 50, "target" : 143, "value" :
 2.33336973079323 }, { "source" : 50, "target" : 131, "value" :
 2.49969943381133 }, { "source" : 50, "target" : 121, "value" :
 2.6049461907643 }, { "source" : 39, "target" : 46, "value" :
 0.83838831939105 }, { "source" : 39, "target" : 34, "value" :
 0.906599367281589 }, { "source" : 39, "target" : 141, "value" :
 0.968679401888891 }, { "source" : 42, "target" : 40, "value" :
 0.936967400239652 }, { "source" : 42, "target" : 30, "value" :
 1.16193537350911 }, { "source" : 42, "target" : 126, "value" :
 1.46727036027847 }, { "source" : 43, "target" : 161, "value" :
 1.66691338986215 }, { "source" : 43, "target" : 81, "value" :
 1.73191858419284 }, { "source" : 43, "target" : 178, "value" :
 1.9201654795029 }, { "source" : 44, "target" : 122, "value" :
 0.887691060162472 }, { "source" : 44, "target" : 25, "value" :
 0.910834630900881 }, { "source" : 44, "target" : 126, "value" :
 0.955996918890767 }, { "source" : 45, "target" : 17, "value" :
 1.01820681828437 }, { "source" : 45, "target" : 166, "value" :
 1.20145988677156 }, { "source" : 45, "target" : 146, "value" :
 1.20640154717973 }, { "source" : 132, "target" : 22, "value" :
 1.92102096765661 }, { "source" : 132, "target" : 1, "value" :
 1.93839214146552 }, { "source" : 132, "target" : 64, "value" :
 1.96395471248129 }, { "source" : 46, "target" : 87, "value" :
 0.525514245923718 }, { "source" : 46, "target" : 67, "value" :
 0.77164737193497 }, { "source" : 46, "target" : 39, "value" :
 0.83838831939105 }, { "source" : 181, "target" : 28, "value" :
 1.55604385856314 }, { "source" : 181, "target" : 1, "value" :
 1.56965338936259 }, { "source" : 181, "target" : 171, "value" :
 1.69660451982878 }, { "source" : 47, "target" : 152, "value" :
 0.854577826233589 }, { "source" : 47, "target" : 114, "value" :
 0.9266323619282 }, { "source" : 47, "target" : 3, "value" :
 1.02234392589866 }, { "source" : 48, "target" : 158, "value" :
 0.265362734477278 }, { "source" : 48, "target" : 120, "value" :
 0.556333578536645 }, { "source" : 48, "target" : 38, "value" :
 0.589148245646065 }, { "source" : 154, "target" : 82, "value" :
 1.14527302730041 }, { "source" : 154, "target" : 76, "value" :
 1.17097721571362 }, { "source" : 154, "target" : 162, "value" :
 1.26802520157892 }, { "source" : 49, "target" : 54, "value" :
 0.649027731293726 }, { "source" : 49, "target" : 38, "value" :
 0.780850102942669 }, { "source" : 49, "target" : 173, "value" :
 0.81124028367136 }, { "source" : 52, "target" : 122, "value" :
 1.3379778773384 }, { "source" : 52, "target" : 179, "value" :
 1.49005509960485 }, { "source" : 52, "target" : 44, "value" :
 1.57348662287399 }, { "source" : 51, "target" : 155, "value" :
 1.31915603278569 }, { "source" : 51, "target" : 161, "value" :
 1.4660251038037 }, { "source" : 51, "target" : 178, "value" :
 1.58491533851279 }, { "source" : 54, "target" : 173, "value" :
 0.535879012442302 }, { "source" : 54, "target" : 9, "value" :
 0.639259804146382 }, { "source" : 54, "target" : 49, "value" :
 0.649027731293726 }, { "source" : 56, "target" : 37, "value" :
 1.09893293326321 }, { "source" : 56, "target" : 23, "value" :
 1.27306803971252 }, { "source" : 56, "target" : 145, "value" :
 1.76979282137191 }, { "source" : 58, "target" : 73, "value" :
 0.61753062769693 }, { "source" : 58, "target" : 75, "value" :
 1.00578459301855 }, { "source" : 58, "target" : 74, "value" :
 1.15679258314774 }, { "source" : 59, "target" : 139, "value" :
 1.18623448906104 }, { "source" : 59, "target" : 6, "value" :
 1.20319220151253 }, { "source" : 59, "target" : 172, "value" :
 1.22357088466087 }, { "source" : 60, "target" : 195, "value" :
 1.07140346151954 }, { "source" : 60, "target" : 193, "value" :
 1.13121231635874 }, { "source" : 60, "target" : 146, "value" :
 1.36170737960538 }, { "source" : 61, "target" : 157, "value" :
 1.32267459949486 }, { "source" : 61, "target" : 65, "value" :
 1.44062721268049 }, { "source" : 61, "target" : 117, "value" :
 1.80769520809045 }, { "source" : 62, "target" : 130, "value" :
 0.973569176575288 }, { "source" : 62, "target" : 76, "value" :
 1.08365560669245 }, { "source" : 62, "target" : 164, "value" :
 1.18811484239607 }, { "source" : 130, "target" : 62, "value" :
 0.973569176575288 }, { "source" : 130, "target" : 76, "value" :
 1.11302717762523 }, { "source" : 130, "target" : 164, "value" :
 1.2886006488654 }, { "source" : 63, "target" : 191, "value" :
 1.02932883580549 }, { "source" : 63, "target" : 163, "value" :
 1.12113424547918 }, { "source" : 63, "target" : 18, "value" :
 1.26189973373278 }, { "source" : 64, "target" : 135, "value" :
 1.51541573130194 }, { "source" : 64, "target" : 185, "value" :
 1.62720597037579 }, { "source" : 64, "target" : 31, "value" :
 1.68348821786941 }, { "source" : 65, "target" : 117, "value" :
 1.21996159391605 }, { "source" : 65, "target" : 14, "value" :
 1.34788804810519 }, { "source" : 65, "target" : 23, "value" :
 1.43367622150495 }, { "source" : 66, "target" : 123, "value" :
 1.01959459924331 }, { "source" : 66, "target" : 41, "value" :
 1.22421240339144 }, { "source" : 66, "target" : 8, "value" :
 1.25014002394532 }, { "source" : 68, "target" : 38, "value" :
 0.978449363913232 }, { "source" : 68, "target" : 158, "value" :
 1.03703630242829 }, { "source" : 68, "target" : 48, "value" :
 1.05021090349244 }, { "source" : 69, "target" : 127, "value" :
 3.23342416696383 }, { "source" : 69, "target" : 11, "value" :
 3.26320505674659 }, { "source" : 69, "target" : 21, "value" :
 3.36150506432041 }, { "source" : 70, "target" : 127, "value" :
 1.62061103837861 }, { "source" : 70, "target" : 180, "value" :
 1.89587456765023 }, { "source" : 70, "target" : 27, "value" :
 2.06063336461274 }, { "source" : 71, "target" : 0, "value" :
 1.217090120178 }, { "source" : 71, "target" : 92, "value" :
 1.26272934069319 }, { "source" : 71, "target" : 168, "value" :
 1.29666888154406 }, { "source" : 72, "target" : 42, "value" :
 2.05388545240779 }, { "source" : 72, "target" : 27, "value" :
 2.20278286020565 }, { "source" : 72, "target" : 180, "value" :
 2.30557748096039 }, { "source" : 73, "target" : 129, "value" :
 0.612358248600967 }, { "source" : 73, "target" : 58, "value" :
 0.61753062769693 }, { "source" : 73, "target" : 75, "value" :
 1.02251661277558 }, { "source" : 188, "target" : 194, "value" :
 1.03089321012129 }, { "source" : 188, "target" : 2, "value" :
 1.07237271026171 }, { "source" : 188, "target" : 193, "value" :
 1.16319525241664 }, { "source" : 74, "target" : 129, "value" :
 1.03378320012459 }, { "source" : 74, "target" : 187, "value" :
 1.1094673454879 }, { "source" : 74, "target" : 58, "value" :
 1.15679258314774 }, { "source" : 75, "target" : 49, "value" :
 0.838637128701236 }, { "source" : 75, "target" : 129, "value" :
 0.870953330955876 }, { "source" : 75, "target" : 153, "value" :
 0.908790881363595 }, { "source" : 78, "target" : 114, "value" :
 0.577326771587172 }, { "source" : 78, "target" : 141, "value" :
 0.866273656709917 }, { "source" : 78, "target" : 102, "value" :
 0.86866830585887 }, { "source" : 79, "target" : 35, "value" :
 1.42656875685969 }, { "source" : 79, "target" : 153, "value" :
 1.4489012818169 }, { "source" : 79, "target" : 173, "value" :
 1.49455192559332 }, { "source" : 80, "target" : 144, "value" :
 0.887281278594123 }, { "source" : 80, "target" : 92, "value" :
 0.965296178506917 }, { "source" : 80, "target" : 34, "value" :
 0.980718740002864 }, { "source" : 77, "target" : 136, "value" :
 0.621515478016677 }, { "source" : 77, "target" : 174, "value" :
 0.928038715026315 }, { "source" : 77, "target" : 94, "value" :
 1.0012295017037 }, { "source" : 82, "target" : 162, "value" :
 1.16365377943373 }, { "source" : 82, "target" : 171, "value" :
 1.28093280512416 }, { "source" : 82, "target" : 1, "value" :
 1.48780804393006 }, { "source" : 57, "target" : 101, "value" :
 1.69082279848271 }, { "source" : 57, "target" : 65, "value" :
 1.69798167687027 }, { "source" : 57, "target" : 143, "value" :
 1.78241540974505 }, { "source" : 86, "target" : 144, "value" :
 1.69849085139365 }, { "source" : 86, "target" : 167, "value" :
 1.80677510354074 }, { "source" : 86, "target" : 170, "value" :
 1.83490735850556 }, { "source" : 81, "target" : 178, "value" :
 1.23794776855576 }, { "source" : 81, "target" : 188, "value" :
 1.32991794855375 }, { "source" : 81, "target" : 63, "value" :
 1.41972493824014 }, { "source" : 87, "target" : 46, "value" :
 0.525514245923718 }, { "source" : 87, "target" : 67, "value" :
 0.617144820469032 }, { "source" : 87, "target" : 190, "value" :
 0.657721693638066 }, { "source" : 89, "target" : 19, "value" :
 0.96003446478593 }, { "source" : 89, "target" : 4, "value" :
 1.04323962864693 }, { "source" : 89, "target" : 35, "value" :
 1.31770432237712 }, { "source" : 90, "target" : 145, "value" :
 1.29556473186106 }, { "source" : 90, "target" : 88, "value" :
 1.43793605202866 }, { "source" : 90, "target" : 21, "value" :
 1.45745067303454 }, { "source" : 91, "target" : 147, "value" :
 0.872263227671935 }, { "source" : 91, "target" : 95, "value" :
 1.29940782523615 }, { "source" : 91, "target" : 164, "value" :
 1.36845093078949 }, { "source" : 92, "target" : 168, "value" :
 0.891693280866144 }, { "source" : 92, "target" : 80, "value" :
 0.965296178506917 }, { "source" : 92, "target" : 3, "value" :
 0.980596813500818 }, { "source" : 93, "target" : 166, "value" :
 1.32215390772501 }, { "source" : 93, "target" : 45, "value" :
 1.49876888445967 }, { "source" : 93, "target" : 35, "value" :
 1.50067519330793 }, { "source" : 94, "target" : 77, "value" :
 1.0012295017037 }, { "source" : 94, "target" : 87, "value" :
 1.02154413719956 }, { "source" : 94, "target" : 67, "value" :
 1.05504419113869 }, { "source" : 187, "target" : 74, "value" :
 1.1094673454879 }, { "source" : 187, "target" : 112, "value" :
 1.14438859643746 }, { "source" : 187, "target" : 38, "value" :
 1.18095138624967 }, { "source" : 95, "target" : 28, "value" :
 1.24572404427195 }, { "source" : 95, "target" : 91, "value" :
 1.29940782523615 }, { "source" : 95, "target" : 147, "value" :
 1.34482555410403 }, { "source" : 96, "target" : 137, "value" :
 1.32822702959422 }, { "source" : 96, "target" : 162, "value" :
 1.38533108035212 }, { "source" : 96, "target" : 107, "value" :
 1.49662865947539 }, { "source" : 97, "target" : 190, "value" :
 1.00634891249526 }, { "source" : 97, "target" : 174, "value" :
 1.03329555435976 }, { "source" : 97, "target" : 55, "value" :
 1.05031189243427 }, { "source" : 98, "target" : 2, "value" :
 1.03887077942638 }, { "source" : 98, "target" : 146, "value" :
 1.18023524376724 }, { "source" : 98, "target" : 141, "value" :
 1.20228260131143 }, { "source" : 99, "target" : 176, "value" :
 1.19797479406283 }, { "source" : 99, "target" : 76, "value" :
 1.27133696320388 }, { "source" : 99, "target" : 62, "value" :
 1.50760392233255 }, { "source" : 100, "target" : 186, "value" :
 1.18594405843695 }, { "source" : 100, "target" : 68, "value" :
 1.2145355550819 }, { "source" : 100, "target" : 120, "value" :
 1.23733968431916 }, { "source" : 101, "target" : 145, "value" :
 1.59853163583269 }, { "source" : 101, "target" : 90, "value" :
 1.71247449794562 }, { "source" : 101, "target" : 88, "value" :
 1.81186749206525 }, { "source" : 102, "target" : 114, "value" :
 0.846377635188045 }, { "source" : 102, "target" : 78, "value" :
 0.86866830585887 }, { "source" : 102, "target" : 2, "value" :
 0.960545297345756 }, { "source" : 103, "target" : 92, "value" :
 1.27136862302378 }, { "source" : 103, "target" : 179, "value" :
 1.48121391237927 }, { "source" : 103, "target" : 34, "value" :
 1.51520652859041 }, { "source" : 108, "target" : 13, "value" :
 1.39988619910808 }, { "source" : 108, "target" : 191, "value" :
 1.41951743564977 }, { "source" : 108, "target" : 115, "value" :
 1.48989298763721 }, { "source" : 104, "target" : 150, "value" :
 3.17433357180874 }, { "source" : 104, "target" : 12, "value" :
 3.34346393734169 }, { "source" : 104, "target" : 86, "value" :
 3.7132464438663 }, { "source" : 105, "target" : 14, "value" :
 1.32361192170884 }, { "source" : 105, "target" : 117, "value" :
 1.49435811762957 }, { "source" : 105, "target" : 44, "value" :
 1.59466498541963 }, { "source" : 190, "target" : 87, "value" :
 0.657721693638066 }, { "source" : 190, "target" : 67, "value" :
 0.723500346667688 }, { "source" : 190, "target" : 189, "value" :
 0.848215195900464 }, { "source" : 106, "target" : 3, "value" :
 1.09867908870044 }, { "source" : 106, "target" : 152, "value" :
 1.23096252452761 }, { "source" : 106, "target" : 160, "value" :
 1.28585821480392 }, { "source" : 107, "target" : 28, "value" :
 1.26741367173486 }, { "source" : 107, "target" : 154, "value" :
 1.44347014594506 }, { "source" : 107, "target" : 96, "value" :
 1.49662865947539 }, { "source" : 21, "target" : 88, "value" :
 0.781590948262505 }, { "source" : 21, "target" : 145, "value" :
 1.28986956879597 }, { "source" : 21, "target" : 90, "value" :
 1.45745067303454 }, { "source" : 110, "target" : 15, "value" :
 0.887991137474196 }, { "source" : 110, "target" : 14, "value" :
 1.41008103511339 }, { "source" : 110, "target" : 13, "value" :
 1.55531597989108 }, { "source" : 111, "target" : 11, "value" :
 1.49996277790847 }, { "source" : 111, "target" : 1, "value" :
 1.53525937177258 }, { "source" : 111, "target" : 83, "value" :
 1.65075719758189 }, { "source" : 113, "target" : 167, "value" :
 0.946340803920534 }, { "source" : 113, "target" : 29, "value" :
 0.956794162296048 }, { "source" : 113, "target" : 134, "value" :
 1.10563198261429 }, { "source" : 116, "target" : 4, "value" :
 0.974043813062297 }, { "source" : 116, "target" : 89, "value" :
 1.33016024214415 }, { "source" : 116, "target" : 187, "value" :
 1.46303723401943 }, { "source" : 117, "target" : 14, "value" :
 0.682738067393721 }, { "source" : 117, "target" : 23, "value" :
 0.975511109512529 }, { "source" : 117, "target" : 65, "value" :
 1.21996159391605 }, { "source" : 119, "target" : 37, "value" :
 1.29737835818427 }, { "source" : 119, "target" : 99, "value" :
 1.60585586344905 }, { "source" : 119, "target" : 76, "value" :
 1.61391321068953 }, { "source" : 120, "target" : 48, "value" :
 0.556333578536645 }, { "source" : 120, "target" : 158, "value" :
 0.576353243510418 }, { "source" : 120, "target" : 9, "value" :
 0.602232655441791 }, { "source" : 196, "target" : 40, "value" :
 1.08808160837882 }, { "source" : 196, "target" : 42, "value" :
 1.63086060386294 }, { "source" : 196, "target" : 44, "value" :
 1.63284506971359 }, { "source" : 167, "target" : 113, "value" :
 0.946340803920534 }, { "source" : 167, "target" : 29, "value" :
 0.973029189538259 }, { "source" : 167, "target" : 161, "value" :
 1.26170575068203 }, { "source" : 121, "target" : 11, "value" :
 1.4425791028605 }, { "source" : 121, "target" : 111, "value" :
 1.8229691499831 }, { "source" : 121, "target" : 21, "value" :
 2.36748093430187 }, { "source" : 134, "target" : 113, "value" :
 1.10563198261429 }, { "source" : 134, "target" : 29, "value" :
 1.51111142679662 }, { "source" : 134, "target" : 142, "value" :
 1.54102098656998 }, { "source" : 122, "target" : 44, "value" :
 0.887691060162472 }, { "source" : 122, "target" : 126, "value" :
 0.926033334725504 }, { "source" : 122, "target" : 30, "value" :
 1.06793098914228 }, { "source" : 124, "target" : 154, "value" :
 1.3994288208261 }, { "source" : 124, "target" : 82, "value" :
 1.53299698713778 }, { "source" : 124, "target" : 37, "value" :
 1.59149388548455 }, { "source" : 125, "target" : 44, "value" :
 1.26950992658186 }, { "source" : 125, "target" : 122, "value" :
 1.37388837799296 }, { "source" : 125, "target" : 40, "value" :
 1.37621257436654 }, { "source" : 126, "target" : 44, "value" :
 0.955996918890767 }, { "source" : 126, "target" : 30, "value" :
 1.17023648864064 }, { "source" : 126, "target" : 40, "value" :
 1.19813563320751 }, { "source" : 128, "target" : 148, "value" :
 0.696288899442868 }, { "source" : 128, "target" : 123, "value" :
 0.79919742595811 }, { "source" : 128, "target" : 67, "value" :
 0.810137647057435 }, { "source" : 129, "target" : 58, "value" :
 0.499109295817574 }, { "source" : 129, "target" : 73, "value" :
 0.612358248600967 }, { "source" : 129, "target" : 75, "value" :
 0.870953330955876 }, { "source" : 133, "target" : 10, "value" :
 1.49789773506322 }, { "source" : 133, "target" : 86, "value" :
 1.88801956975405 }, { "source" : 133, "target" : 116, "value" :
 1.97415188294617 }, { "source" : 85, "target" : 144, "value" :
 0.980673203144786 }, { "source" : 85, "target" : 34, "value" :
 1.0719148246239 }, { "source" : 85, "target" : 128, "value" :
 1.28831023475477 }, { "source" : 109, "target" : 18, "value" :
 0.905488077232075 }, { "source" : 109, "target" : 191, "value" :
 1.11314258139784 }, { "source" : 109, "target" : 13, "value" :
 1.32392687443911 }, { "source" : 136, "target" : 77, "value" :
 0.621515478016677 }, { "source" : 136, "target" : 174, "value" :
 1.12794551332438 }, { "source" : 136, "target" : 94, "value" :
 1.19089859746761 }, { "source" : 137, "target" : 96, "value" :
 1.32822702959422 }, { "source" : 137, "target" : 107, "value" :
 1.52732045355541 }, { "source" : 137, "target" : 171, "value" :
 1.52919521498377 }, { "source" : 139, "target" : 6, "value" :
 1.11017779278581 }, { "source" : 139, "target" : 59, "value" :
 1.18623448906104 }, { "source" : 139, "target" : 166, "value" :
 1.22725042025008 }, { "source" : 140, "target" : 115, "value" :
 1.69609168005874 }, { "source" : 140, "target" : 155, "value" :
 1.89268438165769 }, { "source" : 140, "target" : 166, "value" :
 1.9175324570393 }, { "source" : 195, "target" : 194, "value" :
 0.499680983888029 }, { "source" : 195, "target" : 60, "value" :
 1.07140346151954 }, { "source" : 195, "target" : 188, "value" :
 1.18594760661578 }, { "source" : 141, "target" : 146, "value" :
 0.758592707185517 }, { "source" : 141, "target" : 114, "value" :
 0.773123762237194 }, { "source" : 141, "target" : 78, "value" :
 0.866273656709917 }, { "source" : 182, "target" : 165, "value" :
 0.880973993738239 }, { "source" : 182, "target" : 102, "value" :
 1.07277012655727 }, { "source" : 182, "target" : 78, "value" :
 1.12004379331132 }, { "source" : 142, "target" : 4, "value" :
 1.10523638540958 }, { "source" : 142, "target" : 19, "value" :
 1.34603687670483 }, { "source" : 142, "target" : 89, "value" :
 1.42675789157226 }, { "source" : 143, "target" : 57, "value" :
 1.78241540974505 }, { "source" : 143, "target" : 50, "value" :
 2.33336973079323 }, { "source" : 143, "target" : 155, "value" :
 2.35920288841408 }, { "source" : 144, "target" : 80, "value" :
 0.887281278594123 }, { "source" : 144, "target" : 92, "value" :
 1.10720413640354 }, { "source" : 144, "target" : 19, "value" :
 1.25169011397607 }, { "source" : 145, "target" : 88, "value" :
 1.2098976801696 }, { "source" : 145, "target" : 23, "value" :
 1.22410275311965 }, { "source" : 145, "target" : 21, "value" :
 1.28986956879597 }, { "source" : 189, "target" : 55, "value" :
 0.614750722993733 }, { "source" : 189, "target" : 20, "value" :
 0.85064892082221 }, { "source" : 189, "target" : 114, "value" :
 0.882656129914939 }, { "source" : 146, "target" : 141, "value" :
 0.758592707185517 }, { "source" : 146, "target" : 2, "value" :
 0.920880072265894 }, { "source" : 146, "target" : 114, "value" :
 0.969125381887772 }, { "source" : 147, "target" : 91, "value" :
 0.872263227671935 }, { "source" : 147, "target" : 164, "value" :
 1.07211733625236 }, { "source" : 147, "target" : 162, "value" :
 1.31643258134908 }, { "source" : 150, "target" : 12, "value" :
 1.20614068109613 }, { "source" : 150, "target" : 133, "value" :
 2.46232475615298 }, { "source" : 150, "target" : 113, "value" :
 2.51682907306793 }, { "source" : 194, "target" : 195, "value" :
 0.499680983888029 }, { "source" : 194, "target" : 193, "value" :
 0.683170301495052 }, { "source" : 194, "target" : 188, "value" :
 1.03089321012129 }, { "source" : 149, "target" : 67, "value" :
 0.981292051964712 }, { "source" : 149, "target" : 189, "value" :
 0.996252984592464 }, { "source" : 149, "target" : 190, "value" :
 1.04949711887004 }, { "source" : 148, "target" : 123, "value" :
 0.649791012616204 }, { "source" : 148, "target" : 128, "value" :
 0.696288899442868 }, { "source" : 148, "target" : 67, "value" :
 0.824270670985038 }, { "source" : 151, "target" : 107, "value" :
 1.85294556846627 }, { "source" : 151, "target" : 147, "value" :
 1.95004894353807 }, { "source" : 151, "target" : 184, "value" :
 1.99370051363542 }, { "source" : 152, "target" : 47, "value" :
 0.854577826233589 }, { "source" : 152, "target" : 3, "value" :
 1.0220078017862 }, { "source" : 152, "target" : 92, "value" :
 1.1897702426502 }, { "source" : 192, "target" : 13, "value" :
 1.29940663294756 }, { "source" : 192, "target" : 90, "value" :
 1.33596778479383 }, { "source" : 192, "target" : 23, "value" :
 1.52972970295811 }, { "source" : 153, "target" : 49, "value" :
 0.835263639555275 }, { "source" : 153, "target" : 75, "value" :
 0.908790881363595 }, { "source" : 153, "target" : 38, "value" :
 0.912412166681162 }, { "source" : 27, "target" : 108, "value" :
 1.64161276123444 }, { "source" : 27, "target" : 180, "value" :
 1.76187494692584 }, { "source" : 27, "target" : 163, "value" :
 1.87956179907146 }, { "source" : 155, "target" : 51, "value" :
 1.31915603278569 }, { "source" : 155, "target" : 113, "value" :
 1.33774803133268 }, { "source" : 155, "target" : 161, "value" :
 1.37888245401744 }, { "source" : 157, "target" : 61, "value" :
 1.32267459949486 }, { "source" : 157, "target" : 14, "value" :
 1.39622045276397 }, { "source" : 157, "target" : 88, "value" :
 1.40458084695958 }, { "source" : 158, "target" : 48, "value" :
 0.265362734477278 }, { "source" : 158, "target" : 38, "value" :
 0.536166159724376 }, { "source" : 158, "target" : 120, "value" :
 0.576353243510418 }, { "source" : 159, "target" : 120, "value" :
 0.603097865432138 }, { "source" : 159, "target" : 158, "value" :
 0.60817230594848 }, { "source" : 159, "target" : 38, "value" :
 0.611330889530299 }, { "source" : 156, "target" : 84, "value" :
 1.71242533467083 }, { "source" : 156, "target" : 88, "value" :
 1.85116285041494 }, { "source" : 156, "target" : 183, "value" :
 2.08956035708908 }, { "source" : 163, "target" : 191, "value" :
 0.952862179117558 }, { "source" : 163, "target" : 63, "value" :
 1.12113424547918 }, { "source" : 163, "target" : 102, "value" :
 1.24588772727019 }, { "source" : 31, "target" : 64, "value" :
 1.68348821786941 }, { "source" : 31, "target" : 132, "value" :
 2.10187357136505 }, { "source" : 31, "target" : 90, "value" :
 2.11642129320112 }, { "source" : 123, "target" : 148, "value" :
 0.649791012616204 }, { "source" : 123, "target" : 128, "value" :
 0.79919742595811 }, { "source" : 123, "target" : 41, "value" :
 0.878727812385485 }, { "source" : 184, "target" : 95, "value" :
 1.78865645454068 }, { "source" : 184, "target" : 151, "value" :
 1.99370051363542 }, { "source" : 184, "target" : 91, "value" :
 2.04480495202114 }, { "source" : 40, "target" : 42, "value" :
 0.936967400239652 }, { "source" : 40, "target" : 196, "value" :
 1.08808160837882 }, { "source" : 40, "target" : 30, "value" :
 1.14865142976247 }, { "source" : 114, "target" : 78, "value" :
 0.577326771587172 }, { "source" : 114, "target" : 141, "value" :
 0.773123762237194 }, { "source" : 114, "target" : 102, "value" :
 0.846377635188045 }, { "source" : 53, "target" : 145, "value" :
 1.55789877041342 }, { "source" : 53, "target" : 23, "value" :
 1.6242598455313 }, { "source" : 53, "target" : 117, "value" :
 1.74357045421812 }, { "source" : 88, "target" : 21, "value" :
 0.781590948262505 }, { "source" : 88, "target" : 145, "value" :
 1.2098976801696 }, { "source" : 88, "target" : 135, "value" :
 1.39759102690315 }, { "source" : 112, "target" : 159, "value" :
 0.881066125783051 }, { "source" : 112, "target" : 158, "value" :
 0.886730703944127 }, { "source" : 112, "target" : 38, "value" :
 0.894002726286493 }, { "source" : 118, "target" : 176, "value" :
 1.21414677752 }, { "source" : 118, "target" : 28, "value" :
 1.31503503303524 }, { "source" : 118, "target" : 162, "value" :
 1.75942329485976 }, { "source" : 127, "target" : 21, "value" :
 1.49854768083235 }, { "source" : 127, "target" : 70, "value" :
 1.62061103837861 }, { "source" : 127, "target" : 88, "value" :
 1.63547051661808 }, { "source" : 138, "target" : 46, "value" :
 1.14091993548707 }, { "source" : 138, "target" : 97, "value" :
 1.24067227192795 }, { "source" : 138, "target" : 169, "value" :
 1.27746378215018 }, { "source" : 191, "target" : 18, "value" :
 0.928990869486456 }, { "source" : 191, "target" : 163, "value" :
 0.952862179117558 }, { "source" : 191, "target" : 63, "value" :
 1.02932883580549 }, { "source" : 160, "target" : 106, "value" :
 1.28585821480392 }, { "source" : 160, "target" : 47, "value" :
 1.41755747604485 }, { "source" : 160, "target" : 3, "value" :
 1.41986555864412 }, { "source" : 170, "target" : 167, "value" :
 1.33780159043309 }, { "source" : 170, "target" : 10, "value" :
 1.56855749452816 }, { "source" : 170, "target" : 29, "value" :
 1.61485804285238 }, { "source" : 173, "target" : 54, "value" :
 0.535879012442302 }, { "source" : 173, "target" : 49, "value" :
 0.81124028367136 }, { "source" : 173, "target" : 48, "value" :
 0.814018021168644 }, { "source" : 162, "target" : 107, "value" :
 0.690892872108048 }, { "source" : 162, "target" : 28, "value" :
 1.02167171095672 }, { "source" : 162, "target" : 82, "value" :
 1.16365377943373 }, { "source" : 175, "target" : 49, "value" :
 2.38596468651895 }, { "source" : 175, "target" : 24, "value" :
 2.40298737951784 }, { "source" : 175, "target" : 54, "value" :
 2.43319665696516 }, { "source" : 131, "target" : 1, "value" :
 1.31452729451742 }, { "source" : 131, "target" : 135, "value" :
 1.70918636088312 }, { "source" : 131, "target" : 82, "value" :
 1.88194097651288 }, { "source" : 164, "target" : 147, "value" :
 1.07211733625236 }, { "source" : 164, "target" : 62, "value" :
 1.18811484239607 }, { "source" : 164, "target" : 130, "value" :
 1.2886006488654 }, { "source" : 165, "target" : 182, "value" :
 0.880973993738239 }, { "source" : 165, "target" : 78, "value" :
 0.979060486001503 }, { "source" : 165, "target" : 2, "value" :
 1.17045010019216 }, { "source" : 166, "target" : 146, "value" :
 1.18007015839305 }, { "source" : 166, "target" : 165, "value" :
 1.19060092425939 }, { "source" : 166, "target" : 45, "value" :
 1.20145988677156 }, { "source" : 168, "target" : 34, "value" :
 0.761877422241889 }, { "source" : 168, "target" : 169, "value" :
 0.844297495249255 }, { "source" : 168, "target" : 92, "value" :
 0.891693280866144 }, { "source" : 169, "target" : 168, "value" :
 0.844297495249255 }, { "source" : 169, "target" : 34, "value" :
 1.2227460955028 }, { "source" : 169, "target" : 138, "value" :
 1.27746378215018 }, { "source" : 161, "target" : 167, "value" :
 1.26170575068203 }, { "source" : 161, "target" : 155, "value" :
 1.37888245401744 }, { "source" : 161, "target" : 113, "value" :
 1.43199498384078 }, { "source" : 172, "target" : 59, "value" :
 1.22357088466087 }, { "source" : 172, "target" : 141, "value" :
 1.59670506410588 }, { "source" : 172, "target" : 146, "value" :
 1.63643162601456 }, { "source" : 171, "target" : 82, "value" :
 1.28093280512416 }, { "source" : 171, "target" : 162, "value" :
 1.43251080893642 }, { "source" : 171, "target" : 137, "value" :
 1.52919521498377 }, { "source" : 174, "target" : 77, "value" :
 0.928038715026315 }, { "source" : 174, "target" : 128, "value" :
 1.02596729712391 }, { "source" : 174, "target" : 97, "value" :
 1.03329555435976 }, { "source" : 177, "target" : 33, "value" :
 1.2340054164185 }, { "source" : 177, "target" : 7, "value" :
 1.27080637442328 }, { "source" : 177, "target" : 34, "value" :
 1.42592530414 }, { "source" : 178, "target" : 81, "value" :
 1.23794776855576 }, { "source" : 178, "target" : 51, "value" :
 1.58491533851279 }, { "source" : 178, "target" : 161, "value" :
 1.64589471386379 }, { "source" : 115, "target" : 63, "value" :
 1.37332650631947 }, { "source" : 115, "target" : 191, "value" :
 1.42681654243901 }, { "source" : 115, "target" : 108, "value" :
 1.48989298763721 }, { "source" : 179, "target" : 122, "value" :
 1.34599036234862 }, { "source" : 179, "target" : 60, "value" :
 1.37915176216964 }, { "source" : 179, "target" : 3, "value" :
 1.39836172007607 }, { "source" : 180, "target" : 115, "value" :
 1.66742904753805 }, { "source" : 180, "target" : 81, "value" :
 1.7156539940767 }, { "source" : 180, "target" : 27, "value" :
 1.76187494692584 }, { "source" : 183, "target" : 131, "value" :
 1.42652956194683 }, { "source" : 183, "target" : 1, "value" :
 1.65188484677928 }, { "source" : 183, "target" : 88, "value" :
 1.78946930184687 }, { "source" : 185, "target" : 137, "value" :
 1.5451713094435 }, { "source" : 185, "target" : 64, "value" :
 1.62720597037579 }, { "source" : 185, "target" : 107, "value" :
 1.68220810515777 }, { "source" : 197, "target" : 74, "value" :
 0.00087366264492041 }, { "source" : 197, "target" : 9, "value" :
 0.00120162971200672 }, { "source" : 197, "target" : 186, "value" :
 0.00166639927834256 }, { "source" : 197, "target" : 175, "value" :
 0.00188429419957239 }, { "source" : 135, "target" : 88, "value" :
 1.39759102690315 }, { "source" : 135, "target" : 26, "value" :
 1.49461162528901 }, { "source" : 135, "target" : 157, "value" :
 1.49990831454331 } ] ; var nodes = [ { "name" : "Armenia", "group" :
 "Asia" }, { "name" : "Afghanistan", "group" : "Asia" }, { "name" :
 "Albania", "group" : "Europe" }, { "name" : "Algeria", "group" :
 "Africa" }, { "name" : "Andorra", "group" : "Europe" }, { "name" :
 "Angola", "group" : "Africa" }, { "name" : "Antigua and Barbuda",
 "group" : "Americas" }, { "name" : "Argentina", "group" : "Americas"
 }, { "name" : "Australia", "group" : "Oceania" }, { "name" :
 "Austria", "group" : "Europe" }, { "name" : "Bahrain", "group" :
 "Asia" }, { "name" : "Bangladesh", "group" : "Asia" }, { "name" :
 "Bermuda", "group" : "Americas" }, { "name" : "Bhutan", "group" :
 "Asia" }, { "name" : "Bolivia (Plurinational State of)", "group" :
 "Americas" }, { "name" : "Botswana", "group" : "Africa" }, { "name" :
 "Brazil", "group" : "Americas" }, { "name" : "Aruba", "group" :
 "Americas" }, { "name" : "Belize", "group" : "Americas" }, { "name" :
 "Brunei Darussalam", "group" : "Asia" }, { "name" : "Bulgaria",
 "group" : "Europe" }, { "name" : "Myanmar", "group" : "Asia" }, {
 "name" : "Burundi", "group" : "Africa" }, { "name" : "Cameroon",
 "group" : "Africa" }, { "name" : "Canada", "group" : "Americas" }, {
 "name" : "Cabo Verde", "group" : "Africa" }, { "name" : "Central
 African Republic", "group" : "Africa" }, { "name" : "Sri Lanka",
 "group" : "Asia" }, { "name" : "Chad", "group" : "Africa" }, { "name"
 : "Chile", "group" : "Americas" }, { "name" : "Colombia", "group" :
 "Americas" }, { "name" : "the Comoros", "group" : "Africa" }, {
 "name" : "Congo", "group" : "Africa" }, { "name" : "Costa Rica",
 "group" : "Americas" }, { "name" : "Cuba", "group" : "Americas" }, {
 "name" : "Cyprus", "group" : "Asia" }, { "name" : "Azerbaijan",
 "group" : "Asia" }, { "name" : "Benin", "group" : "Africa" }, {
 "name" : "Denmark", "group" : "Europe" }, { "name" : "Dominica",
 "group" : "Americas" }, { "name" : "the Dominican Republic", "group"
 : "Americas" }, { "name" : "Belarus", "group" : "Europe" }, { "name"
 : "Ecuador", "group" : "Americas" }, { "name" : "Egypt", "group" :
 "Africa" }, { "name" : "El Salvador", "group" : "Americas" }, {
 "name" : "Equatorial Guinea", "group" : "Africa" }, { "name" :
 "Estonia", "group" : "Europe" }, { "name" : "Fiji", "group" :
 "Oceania" }, { "name" : "Finland", "group" : "Europe" }, { "name" :
 "France", "group" : "Europe" }, { "name" : "Djibouti", "group" :
 "Africa" }, { "name" : "Georgia", "group" : "Asia" }, { "name" :
 "Gabon", "group" : "Africa" }, { "name" : "the Gambia", "group" :
 "Africa" }, { "name" : "Germany", "group" : "Europe" }, { "name" :
 "Bosnia and Herzegovina", "group" : "Europe" }, { "name" : "Ghana",
 "group" : "Africa" }, { "name" : "Kiribati", "group" : "Oceania" }, {
 "name" : "Greece", "group" : "Europe" }, { "name" : "Grenada",
 "group" : "Americas" }, { "name" : "Guam", "group" : "Oceania" }, {
 "name" : "Guatemala", "group" : "Americas" }, { "name" : "Guinea",
 "group" : "Africa" }, { "name" : "Guyana", "group" : "Americas" }, {
 "name" : "Haiti", "group" : "Americas" }, { "name" : "Honduras",
 "group" : "Americas" }, { "name" : "Hungary", "group" : "Europe" }, {
 "name" : "Croatia", "group" : "Europe" }, { "name" : "Iceland",
 "group" : "Europe" }, { "name" : "India", "group" : "Asia" }, {
 "name" : "Indonesia", "group" : "Asia" }, { "name" : "Iran (Islamic
 Republic of)", "group" : "Asia" }, { "name" : "Iraq", "group" :
 "Asia" }, { "name" : "Ireland", "group" : "Europe" }, { "name" :
 "Israel", "group" : "Asia" }, { "name" : "Italy", "group" : "Europe"
 }, { "name" : "Cote d'Ivoire", "group" : "Africa" }, { "name" :
 "Kazakhstan", "group" : "Asia" }, { "name" : "Jamaica", "group" :
 "Americas" }, { "name" : "Japan", "group" : "Asia" }, { "name" :
 "Jordan", "group" : "Asia" }, { "name" : "Kyrgyzstan", "group" :
 "Asia" }, { "name" : "Kenya", "group" : "Africa" }, { "name" :
 "Cambodia", "group" : "Asia" }, { "name" : "Democratic People's
 Republic of Korea", "group" : "Asia" }, { "name" : "Republic of
 Korea", "group" : "Asia" }, { "name" : "Kuwait", "group" : "Asia" },
 { "name" : "Latvia", "group" : "Europe" }, { "name" : "the Lao
 People's Democratic Republic", "group" : "Asia" }, { "name" :
 "Lebanon", "group" : "Asia" }, { "name" : "Lesotho", "group" :
 "Africa" }, { "name" : "Liberia", "group" : "Africa" }, { "name" :
 "Libya", "group" : "Africa" }, { "name" : "Liechtenstein", "group" :
 "Europe" }, { "name" : "Lithuania", "group" : "Europe" }, { "name" :
 "Madagascar", "group" : "Africa" }, { "name" : "Malawi", "group" :
 "Africa" }, { "name" : "Malaysia", "group" : "Asia" }, { "name" :
 "Maldives", "group" : "Asia" }, { "name" : "Mali", "group" : "Africa"
 }, { "name" : "Malta", "group" : "Europe" }, { "name" : "Mauritania",
 "group" : "Africa" }, { "name" : "Mauritius", "group" : "Africa" }, {
 "name" : "Mexico", "group" : "Americas" }, { "name" : "Monaco",
 "group" : "Europe" }, { "name" : "Mongolia", "group" : "Asia" }, {
 "name" : "Morocco", "group" : "Africa" }, { "name" : "Mozambique",
 "group" : "Africa" }, { "name" : "Micronesia (Federated States of)",
 "group" : "Oceania" }, { "name" : "Republic of Moldova", "group" :
 "Europe" }, { "name" : "Namibia", "group" : "Africa" }, { "name" :
 "Nepal", "group" : "Asia" }, { "name" : "the Netherlands", "group" :
 "Europe" }, { "name" : "New Caledonia", "group" : "Oceania" }, {
 "name" : "The former Yugoslav Republic of Macedonia", "group" :
 "Europe" }, { "name" : "Vanuatu", "group" : "Oceania" }, { "name" :
 "New Zealand", "group" : "Oceania" }, { "name" : "Nicaragua", "group"
 : "Americas" }, { "name" : "the Niger", "group" : "Africa" }, {
 "name" : "Nigeria", "group" : "Africa" }, { "name" : "Norway",
 "group" : "Europe" }, { "name" : "Pakistan", "group" : "Asia" }, {
 "name" : "Panama", "group" : "Americas" }, { "name" : "the Czech
 Republic", "group" : "Europe" }, { "name" : "Papua New Guinea",
 "group" : "Oceania" }, { "name" : "Paraguay", "group" : "Americas" },
 { "name" : "Peru", "group" : "Americas" }, { "name" : "the
 Philippines", "group" : "Asia" }, { "name" : "Poland", "group" :
 "Europe" }, { "name" : "Portugal", "group" : "Europe" }, { "name" :
 "Guinea-Bissau", "group" : "Africa" }, { "name" : "Timor-Leste",
 "group" : "Asia" }, { "name" : "Eritrea", "group" : "Africa" }, {
 "name" : "Qatar", "group" : "Asia" }, { "name" : "Palau", "group" :
 "Oceania" }, { "name" : "Zimbabwe", "group" : "Africa" }, { "name" :
 "Romania", "group" : "Europe" }, { "name" : "Rwanda", "group" :
 "Africa" }, { "name" : "the Russian Federation", "group" : "Europe"
 }, { "name" : "Saint Kitts and Nevis", "group" : "Americas" }, {
 "name" : "Saint Lucia", "group" : "Americas" }, { "name" : "Saint
 Vincent and the Grenadines", "group" : "Americas" }, { "name" : "San
 Marino", "group" : "Europe" }, { "name" : "Sao Tome and Principe",
 "group" : "Africa" }, { "name" : "Saudi Arabia", "group" : "Asia" },
 { "name" : "Senegal", "group" : "Africa" }, { "name" : "Seychelles",
 "group" : "Africa" }, { "name" : "Sierra Leone", "group" : "Africa"
 }, { "name" : "Slovenia", "group" : "Europe" }, { "name" :
 "Slovakia", "group" : "Europe" }, { "name" : "Singapore", "group" :
 "Asia" }, { "name" : "Somalia", "group" : "Africa" }, { "name" :
 "South Africa", "group" : "Africa" }, { "name" : "Spain", "group" :
 "Europe" }, { "name" : "former Sudan", "group" : "Africa" }, { "name"
 : "Suriname", "group" : "Americas" }, { "name" : "Tajikistan",
 "group" : "Asia" }, { "name" : "Swaziland", "group" : "Africa" }, {
 "name" : "Sweden", "group" : "Europe" }, { "name" : "Switzerland",
 "group" : "Europe" }, { "name" : "the Syrian Arab Republic", "group"
 : "Asia" }, { "name" : "Turkmenistan", "group" : "Asia" }, { "name" :
 "the United Republic of Tanzania", "group" : "Africa" }, { "name" :
 "Thailand", "group" : "Asia" }, { "name" : "Togo", "group" : "Africa"
 }, { "name" : "Tonga", "group" : "Oceania" }, { "name" : "Trinidad
 and Tobago", "group" : "Americas" }, { "name" : "Oman", "group" :
 "Asia" }, { "name" : "Tunisia", "group" : "Africa" }, { "name" :
 "Turkey", "group" : "Asia" }, { "name" : "the United Arab Emirates",
 "group" : "Asia" }, { "name" : "Uganda", "group" : "Africa" }, {
 "name" : "Tuvalu", "group" : "Oceania" }, { "name" : "the United
 Kingdom of Great Britain and Northern Ireland", "group" : "Europe" },
 { "name" : "Ukraine", "group" : "Europe" }, { "name" : "the United
 States of America", "group" : "Americas" }, { "name" : "Burkina
 Faso", "group" : "Africa" }, { "name" : "Uruguay", "group" :
 "Americas" }, { "name" : "Uzbekistan", "group" : "Asia" }, { "name" :
 "Venezuela (Bolivarian Republic of)", "group" : "Americas" }, {
 "name" : "Viet Nam", "group" : "Asia" }, { "name" : "Ethiopia",
 "group" : "Africa" }, { "name" : "Samoa", "group" : "Oceania" }, {
 "name" : "Yemen", "group" : "Asia" }, { "name" : "the Democratic
 Republic of the Congo", "group" : "Africa" }, { "name" : "Zambia",
 "group" : "Africa" }, { "name" : "Belgium", "group" : "Europe" }, {
 "name" : "Luxembourg", "group" : "Europe" }, { "name" : "Isle of
 Man", "group" : "Europe" }, { "name" : "Serbia", "group" : "Europe"
 }, { "name" : "Montenegro", "group" : "Europe" }, { "name" : "the
 Sudan", "group" : "Africa" }, { "name" : "South Sudan", "group" :
 "Africa" }, { "name" : "Curacao", "group" : "Americas" }, { "name" :
 "Sint Maarten (Dutch Part)", "group" : "Americas" }, { "name" :
 "Saint-Martin (French Part)", "group" : "Americas" }, { "name" :
 "Occupied Palestinian Territory", "group" : "Asia" }, { "name" :
 "Zero Hunger", "group" : "World" } ] ; var width = 1500 height =
 1500;

var color = d3.scale.category20();

var force = d3.layout.force()
.nodes(d3.values(nodes))
.links(links)
.size([width, height])
.linkDistance(function(d){return d.value})
.charge(-300)
.on("tick", tick)
.start();

var svg = d3.select("body").append("svg")
.attr("width", width)
.attr("height", height)
.attr("pointer-events", "all")
.call(d3.behavior.zoom().on("zoom", redraw));

var vis = svg
.append("svg:g");

vis.append("svg:rect")
.attr("width", width)
.attr("height", height)
.attr("fill", 'white');

function redraw() {
vis.attr("transform",
"translate(" + d3.event.translate + ")"
+ " scale(" + d3.event.scale + ")");
}

var link = vis.selectAll(".link")
.data(force.links())
.enter().append("line")
.attr("class", "link")
.style("stroke-width", function(d) { return Math.sqrt(d.value); });

var node = vis.selectAll(".node")
.data(force.nodes())
.enter().append("g")
.attr("class", "node")
.style("fill", function(d) { return color(d.group); })
.style("opacity", 0.9)
.on("mouseover", mouseover)
.on("mouseout", mouseout)
.call(force.drag);

node.append("circle")
.attr("r", 6)

node.append("svg:text")
.attr("class", "nodetext")
.attr("dx", 12)
.attr("dy", ".35em")
.text(function(d) { return d.name });

function tick() {
link
.attr("x1", function(d) { return d.source.x; })
.attr("y1", function(d) { return d.source.y; })
.attr("x2", function(d) { return d.target.x; })
.attr("y2", function(d) { return d.target.y; });

node.attr("transform", function(d) { return "translate(" + d.x + "," + d.y + ")"; });
}

function mouseover() {
d3.select(this).select("circle").transition()
.duration(750)
.attr("r", 16);
d3.select(this).select("text").transition()
.duration(750)
.attr("x", 13)
.style("stroke-width", ".5px")
.style("font", "37.5px serif")
.style("opacity", 1);
}

function mouseout() {
d3.select(this).select("circle").transition()
.duration(750)
.attr("r", 8);
}

</script>
