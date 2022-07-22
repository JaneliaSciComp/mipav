package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;

/**
/ M. Ullner and M. Lund, 2012-2016

Copyright notice
----------------

~~~~
Copyright (c) 1991-95 Paul J Turner, Portland, OR
Copyright (c) 1996-98 ACE/gr Development Team

Currently maintained by Evgeny Stambulchik, Rehovot, Israel

                             All Rights Reserved

Permission  to  use, copy, modify, and  distribute  this software  and  its
documentation  for any purpose and  without fee is hereby granted, provided
that  the above copyright notice  appear in  all copies and  that both that
copyright  notice  and   this  permission  notice   appear  in   supporting
documentation.

PAUL J TURNER AND OTHER CONTRIBUTORS DISCLAIM ALL WARRANTIES WITH REGARD TO
THIS SOFTWARE, INCLUDING,  BUT  NOT LIMITED  TO, ALL  IMPLIED WARRANTIES OF
MERCHANTABILITY  AND  FITNESS. IN NO EVENT SHALL PAUL J TURNER  OR  CURRENT
MAINTAINER  BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER
IN AN ACTION OF CONTRACT, NEGLIGENCE OR  OTHER TORTUOUS ACTION, ARISING OUT
OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
*/

/**
 * Correct values for Cephes functions are taken from hcephes-master/test/src/hcephes.c provided under the MIT license
 * The MIT License (MIT)
=====================

Copyright (c) 2018 Danilo Horta

Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
of the Software, and to permit persons to whom the Software is furnished to do
so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
 *  */

/**
 * Correct values for ndtri are taken from scipy-main/scipy/special/tests/test_ndtr.py under the BSD-3 license:
 * Copyright (c) 2001-2002 Enthought, Inc. 2003-2022, SciPy Developers.
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:

1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above
   copyright notice, this list of conditions and the following
   disclaimer in the documentation and/or other materials provided
   with the distribution.

3. Neither the name of the copyright holder nor the names of its
   contributors may be used to endorse or promote products derived
   from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 */

public class Cephes {
	
	public final static int BETA = 1;
	public final static int CHBEVL = 2;
	public final static int CHDTR = 3;
	public final static int CHDTRC = 4;
	public final static int CHDTRI = 5;
	public final static int DAWSN = 6;
	public final static int ELLIE = 7;
	public final static int ELLIK = 8;
	public final static int ELLPE = 9;
	public final static int ELLPK = 10;
	public final static int ERF = 11;
	public final static int ERFC = 12;
	public final static int EXPN = 13;
	public final static int FAC = 14;
	public final static int FDTR = 15;
	public final static int FDTRC = 16;
	public final static int FDTRI = 17;
	public final static int FRESNL = 18;
	public final static int GDTR = 19;
	public final static int GDTRC = 20;
	public final static int HYP2F1 = 21;
	public final static int IGAM = 22;
	public final static int IGAMI = 23;
	public final static int IGAMC = 24;
	public final static int INCBET = 25;
	public final static int INCBI = 26;
	public final static int LBETA = 27;
	public final static int LGAM = 28;
	public final static int NDTR = 29;
	public final static int NDTRI = 30;
	public final static int POLEVL = 31;
	public final static int P1EVL = 32;
	public final static int PSI = 33;
	public final static int STIRF = 34;
	public final static int STRUVE = 35;
	public final static int TRUE_GAMMA = 36;
	public final static int ZETA = 37;
	public final static int ZETAC = 38;
	// For IEEE arithmetic (IBMPC):
    private final static double MACHEP =  1.11022302462515654042E-16; // 2**-53
    private final static double MAXLOG =  7.09782712893383996843E2;   // log(2**1024)
    private final static double MINLOG = -7.08396418532264106224E2;   // log(2**-1022)
    private final static double MAXNUM =  Double.MAX_VALUE; // 1.7976931348623158E308 2**1024
    private final static double MAXGAM  = 171.624376956302725;
    private final static double MAXLGM = 2.556348e305;
    private final static double big = 4.503599627370496e15;
	private final static double biginv =  2.22044604925031308085e-16;
	private final static double BIG  = 1.44115188075855872E+17;
	private final static double EUL = 0.57721566490153286060;
	/* sqrt(2pi) */
	private final static double s2pi = 2.50662827463100050242E0;
	private final static double PIO2   =  1.57079632679489661923; // pi/2
	private final static double LOGPI = 1.14472988584940017414;
	/* log( sqrt( 2*pi ) ) */
	private final static double LS2PI  =  0.91893853320467274178;
	private final static double SQRTH  =  7.07106781186547524401E-1; // sqrt(2)/2
	private final static int MAXL2 = 127;
	private final static double MAXSTIR = 143.01608;
	private final static double SQTPI = 2.50662827463100050242E0;
	private final static double stop = 1.37e-17;
	private final static int MAXFAC = 170;
	private final static double EPS = 1.0e-13;
	private final static double EPS2 = 1.0e-10;
	private final static double ETHRESH = 1.0e-12;
	private final static int MAX_ITERATIONS = 10000;

	private final boolean DEBUG = false;
	private int sgngam;
	// For ndtr:
	private final static double P[] = new double[]{
		 2.46196981473530512524E-10,
		 5.64189564831068821977E-1,
		 7.46321056442269912687E0,
		 4.86371970985681366614E1,
		 1.96520832956077098242E2,
		 5.26445194995477358631E2,
		 9.34528527171957607540E2,
		 1.02755188689515710272E3,
		 5.57535335369399327526E2
		};
	private final static double Q[] = new double[]{
			/* 1.00000000000000000000E0,*/
		 1.32281951154744992508E1,
		 8.67072140885989742329E1,
		 3.54937778887819891062E2,
		 9.75708501743205489753E2,
		 1.82390916687909736289E3,
		 2.24633760818710981792E3,
		 1.65666309194161350182E3,
		 5.57535340817727675546E2
		};
    private final static double R[] = new double[]{
		 5.64189583547755073984E-1,
		 1.27536670759978104416E0,
		 5.01905042251180477414E0,
		 6.16021097993053585195E0,
		 7.40974269950448939160E0,
		 2.97886665372100240670E0
		};
	private final static double S[] = new double[]{
		/* 1.00000000000000000000E0,*/
		 2.26052863220117276590E0,
		 9.39603524938001434673E0,
		 1.20489539808096656605E1,
		 1.70814450747565897222E1,
		 9.60896809063285878198E0,
		 3.36907645100081516050E0
		};
	private final static double T[] = new double[]{
		 9.60497373987051638749E0,
		 9.00260197203842689217E1,
		 2.23200534594684319226E3,
		 7.00332514112805075473E3,
		 5.55923013010394962768E4
		};
	private final static double U[] = new double[]{
		/* 1.00000000000000000000E0,*/
		 3.35617141647503099647E1,
		 5.21357949780152679795E2,
		 4.59432382970980127987E3,
		 2.26290000613890934246E4,
		 4.92673942608635921086E4
		};
	// P and Q approximations for ndtri
	/* approximation for 0 <= |y - 0.5| <= 3/8 */
	private final static double P0[] = new double[]{
	-5.99633501014107895267E1,
	 9.80010754185999661536E1,
	-5.66762857469070293439E1,
	 1.39312609387279679503E1,
	-1.23916583867381258016E0,
	};
	private final static double Q0[] = new double[]{
	/* 1.00000000000000000000E0,*/
	 1.95448858338141759834E0,
	 4.67627912898881538453E0,
	 8.63602421390890590575E1,
	-2.25462687854119370527E2,
	 2.00260212380060660359E2,
	-8.20372256168333339912E1,
	 1.59056225126211695515E1,
	-1.18331621121330003142E0,
	};
	
	/* Approximation for interval z = sqrt(-2 log y ) between 2 and 8
	 * i.e., y between exp(-2) = .135 and exp(-32) = 1.27e-14.
	 */
	private final static double P1[] = new double[]{
	 4.05544892305962419923E0,
	 3.15251094599893866154E1,
	 5.71628192246421288162E1,
	 4.40805073893200834700E1,
	 1.46849561928858024014E1,
	 2.18663306850790267539E0,
	-1.40256079171354495875E-1,
	-3.50424626827848203418E-2,
	-8.57456785154685413611E-4,
	};
	private final static double Q1[] = new double[]{
	/*  1.00000000000000000000E0,*/
	 1.57799883256466749731E1,
	 4.53907635128879210584E1,
	 4.13172038254672030440E1,
	 1.50425385692907503408E1,
	 2.50464946208309415979E0,
	-1.42182922854787788574E-1,
	-3.80806407691578277194E-2,
	-9.33259480895457427372E-4,
	};
	
	
	/* Approximation for interval z = sqrt(-2 log y ) between 8 and 64
	 * i.e., y between exp(-32) = 1.27e-14 and exp(-2048) = 3.67e-890.
	 */

	private final static double P2[] = new double[]{
	  3.23774891776946035970E0,
	  6.91522889068984211695E0,
	  3.93881025292474443415E0,
	  1.33303460815807542389E0,
	  2.01485389549179081538E-1,
	  1.23716634817820021358E-2,
	  3.01581553508235416007E-4,
	  2.65806974686737550832E-6,
	  6.23974539184983293730E-9,
	};
	private final static double Q2[] = new double[]{
	/*  1.00000000000000000000E0,*/
	  6.02427039364742014255E0,
	  3.67983563856160859403E0,
	  1.37702099489081330271E0,
	  2.16236993594496635890E-1,
	  1.34204006088543189037E-2,
	  3.28014464682127739104E-4,
	  2.89247864745380683936E-6,
	  6.79019408009981274425E-9,
	};
	
	// For ellpe:
	private final static double PELLPE[] = new double[]{
		  1.53552577301013293365E-4,
		  2.50888492163602060990E-3,
		  8.68786816565889628429E-3,
		  1.07350949056076193403E-2,
		  7.77395492516787092951E-3,
		  7.58395289413514708519E-3,
		  1.15688436810574127319E-2,
		  2.18317996015557253103E-2,
		  5.68051945617860553470E-2,
		  4.43147180560990850618E-1,
		  1.00000000000000000299E0
		};
	private final static double QELLPE[] = new double[]{
		  3.27954898576485872656E-5,
		  1.00962792679356715133E-3,
		  6.50609489976927491433E-3,
		  1.68862163993311317300E-2,
		  2.61769742454493659583E-2,
		  3.34833904888224918614E-2,
		  4.27180926518931511717E-2,
		  5.85936634471101055642E-2,
		  9.37499997197644278445E-2,
		  2.49999999999888314361E-1
		};
	
	// For ellpk:
	private final static double C1 = 1.3862943611198906188E0; /* log(4) */
	private final static double PELLPK[] = new double[]{
		 1.37982864606273237150E-4,
		 2.28025724005875567385E-3,
		 7.97404013220415179367E-3,
		 9.85821379021226008714E-3,
		 6.87489687449949877925E-3,
		 6.18901033637687613229E-3,
		 8.79078273952743772254E-3,
		 1.49380448916805252718E-2,
		 3.08851465246711995998E-2,
		 9.65735902811690126535E-2,
		 1.38629436111989062502E0
		};

	private final static double QELLPK[] = new double[]{
		 2.94078955048598507511E-5,
		 9.14184723865917226571E-4,
		 5.94058303753167793257E-3,
		 1.54850516649762399335E-2,
		 2.39089602715924892727E-2,
		 3.01204715227604046988E-2,
		 3.73774314173823228969E-2,
		 4.88280347570998239232E-2,
		 7.03124996963957469739E-2,
		 1.24999999999870820058E-1,
		 4.99999999999999999821E-1
		};
	
	/* Expansion coefficients
	 * for Euler-Maclaurin summation formula
	 * (2k)! / B2k
	 * where B2k are Bernoulli numbers
	 */
	private final static double AZETA[] = new double[]{
	12.0,
	-720.0,
	30240.0,
	-1209600.0,
	47900160.0,
	-1.8924375803183791606e9, /*1.307674368e12/691*/
	7.47242496e10,
	-2.950130727918164224e12, /*1.067062284288e16/3617*/
	1.1646782814350067249e14, /*5.109094217170944e18/43867*/
	-4.5979787224074726105e15, /*8.028576626982912e20/174611*/
	1.8152105401943546773e17, /*1.5511210043330985984e23/854513*/
	-7.1661652561756670113e18 /*1.6938241367317436694528e27/236364091*/
	};
	
	
	private final static double azetac[] = new double[]{
			-1.50000000000000000000E0,
			 Double.POSITIVE_INFINITY, /* infinity. */
			 6.44934066848226436472E-1,
			 2.02056903159594285400E-1,
			 8.23232337111381915160E-2,
			 3.69277551433699263314E-2,
			 1.73430619844491397145E-2,
			 8.34927738192282683980E-3,
			 4.07735619794433937869E-3,
			 2.00839282608221441785E-3,
			 9.94575127818085337146E-4,
			 4.94188604119464558702E-4,
			 2.46086553308048298638E-4,
			 1.22713347578489146752E-4,
			 6.12481350587048292585E-5,
			 3.05882363070204935517E-5,
			 1.52822594086518717326E-5,
			 7.63719763789976227360E-6,
			 3.81729326499983985646E-6,
			 1.90821271655393892566E-6,
			 9.53962033872796113152E-7,
			 4.76932986787806463117E-7,
			 2.38450502727732990004E-7,
			 1.19219925965311073068E-7,
			 5.96081890512594796124E-8,
			 2.98035035146522801861E-8,
			 1.49015548283650412347E-8,
			 7.45071178983542949198E-9,
			 3.72533402478845705482E-9,
			 1.86265972351304900640E-9,
			 9.31327432419668182872E-10
			};
	/* 2**x (1 - 1/x) (zeta(x) - 1) = P(1/x)/Q(1/x), 1 <= x <= 10 */
	private final static double PZETAC[] = new double[]{
			  5.85746514569725319540E11,
			  2.57534127756102572888E11,
			  4.87781159567948256438E10,
			  5.15399538023885770696E9,
			  3.41646073514754094281E8,
			  1.60837006880656492731E7,
			  5.92785467342109522998E5,
			  1.51129169964938823117E4,
			  2.01822444485997955865E2,
			};
	private final static double QZETAC[] = new double[]{
			/*  1.00000000000000000000E0,*/
			  3.90497676373371157516E11,
			  5.22858235368272161797E10,
			  5.64451517271280543351E9,
			  3.39006746015350418834E8,
			  1.79410371500126453702E7,
			  5.66666825131384797029E5,
			  1.60382976810944131506E4,
			  1.96436237223387314144E2,
			};
	/* log(zeta(x) - 1 - 2**-x), 10 <= x <= 50 */
	private final static double AZETAC[] = new double[]{
			 8.70728567484590192539E6,
			 1.76506865670346462757E8,
			 2.60889506707483264896E10,
			 5.29806374009894791647E11,
			 2.26888156119238241487E13,
			 3.31884402932705083599E14,
			 5.13778997975868230192E15,
			-1.98123688133907171455E15,
			-9.92763810039983572356E16,
			 7.82905376180870586444E16,
			 9.26786275768927717187E16,
			};
	private final static double BZETAC[] = new double[]{
			/* 1.00000000000000000000E0,*/
			-7.92625410563741062861E6,
			-1.60529969932920229676E8,
			-2.37669260975543221788E10,
			-4.80319584350455169857E11,
			-2.07820961754173320170E13,
			-2.96075404507272223680E14,
			-4.86299103694609136686E15,
			 5.34589509675789930199E15,
			 5.71464111092297631292E16,
			-1.79915597658676556828E16,
			};
	/* (1-x) (zeta(x) - 1), 0 <= x <= 1 */
	private final static double RZETAC[] = new double[]{
			-3.28717474506562731748E-1,
			 1.55162528742623950834E1,
			-2.48762831680821954401E2,
			 1.01050368053237678329E3,
			 1.26726061410235149405E4,
			-1.11578094770515181334E5,
			};
	private final static double SZETAC[] = new double[]{
			/* 1.00000000000000000000E0,*/
			 1.95107674914060531512E1,
			 3.17710311750646984099E2,
			 3.03835500874445748734E3,
			 2.03665876435770579345E4,
			 7.43853965136767874343E4,
			};
	
	/* Stirling's formula for the gamma function */
	private final static double STIR[] = new double[]{
	 7.87311395793093628397E-4,
	-2.29549961613378126380E-4,
	-2.68132617805781232825E-3,
	 3.47222221605458667310E-3,
	 8.33333333333482257126E-2,
	};
	
	private final static double PGAMMA[] = new double[]{
			  1.60119522476751861407E-4,
			  1.19135147006586384913E-3,
			  1.04213797561761569935E-2,
			  4.76367800457137231464E-2,
			  2.07448227648435975150E-1,
			  4.94214826801497100753E-1,
			  9.99999999999999996796E-1
			};
	private final static double QGAMMA[] = new double[]{
			-2.31581873324120129819E-5,
			 5.39605580493303397842E-4,
			-4.45641913851797240494E-3,
			 1.18139785222060435552E-2,
			 3.58236398605498653373E-2,
			-2.34591795718243348568E-1,
			 7.14304917030273074085E-2,
			 1.00000000000000000320E0
			};
	
	/* A[]: Stirling's formula expansion of log gamma
	 * B[], C[]: log gamma function between 2 and 3
	 */
	private final static double ALGAM[] = new double[]{
	 8.11614167470508450300E-4,
	-5.95061904284301438324E-4,
	 7.93650340457716943945E-4,
	-2.77777777730099687205E-3,
	 8.33333333333331927722E-2
	};
	private final static double BLGAM[] = new double[]{
	-1.37825152569120859100E3,
	-3.88016315134637840924E4,
	-3.31612992738871184744E5,
	-1.16237097492762307383E6,
	-1.72173700820839662146E6,
	-8.53555664245765465627E5
	};
	private final static double CLGAM[] = new double[]{
	/* 1.00000000000000000000E0, */
	-3.51815701436523470549E2,
	-1.70642106651881159223E4,
	-2.20528590553854454839E5,
	-1.13933444367982507207E6,
	-2.53252307177582951285E6,
	-2.01889141433532773231E6
	};
	
	/* Dawson's integral, interval 0 to 3.25 */
	private final static double AN[] = new double[] {
	 1.13681498971755972054E-11,
	 8.49262267667473811108E-10,
	 1.94434204175553054283E-8,
	 9.53151741254484363489E-7,
	 3.07828309874913200438E-6,
	 3.52513368520288738649E-4,
	-8.50149846724410912031E-4,
	 4.22618223005546594270E-2,
	-9.17480371773452345351E-2,
	 9.99999999999999994612E-1,
	};
	private final static double AD[] = new double[] {
	 2.40372073066762605484E-11,
	 1.48864681368493396752E-9,
	 5.21265281010541664570E-8,
	 1.27258478273186970203E-6,
	 2.32490249820789513991E-5,
	 3.25524741826057911661E-4,
	 3.48805814657162590916E-3,
	 2.79448531198828973716E-2,
	 1.58874241960120565368E-1,
	 5.74918629489320327824E-1,
	 1.00000000000000000539E0,
	};
	/* interval 3.25 to 6.25 */
	private final static double BN[] = new double[] {
	 5.08955156417900903354E-1,
	-2.44754418142697847934E-1,
	 9.41512335303534411857E-2,
	-2.18711255142039025206E-2,
	 3.66207612329569181322E-3,
	-4.23209114460388756528E-4,
	 3.59641304793896631888E-5,
	-2.14640351719968974225E-6,
	 9.10010780076391431042E-8,
	-2.40274520828250956942E-9,
	 3.59233385440928410398E-11,
	};
	private final static double BD[] = new double[] {
	/*  1.00000000000000000000E0,*/
	-6.31839869873368190192E-1,
	 2.36706788228248691528E-1,
	-5.31806367003223277662E-2,
	 8.48041718586295374409E-3,
	-9.47996768486665330168E-4,
	 7.81025592944552338085E-5,
	-4.55875153252442634831E-6,
	 1.89100358111421846170E-7,
	-4.91324691331920606875E-9,
	 7.18466403235734541950E-11,
	};
	/* 6.25 to infinity */
	private final static double CN[] = new double[]{
	-5.90592860534773254987E-1,
	 6.29235242724368800674E-1,
	-1.72858975380388136411E-1,
	 1.64837047825189632310E-2,
	-4.86827613020462700845E-4,
	};
	private final static double CD[] = new double[] {
	/* 1.00000000000000000000E0,*/
	-2.69820057197544900361E0,
	 1.73270799045947845857E0,
	-3.93708582281939493482E-1,
	 3.44278924041233391079E-2,
	-9.73655226040941223894E-4,
	};
	
	/* Factorials of integers from 0 through 33 */
	private final static double factbl[] = new double[]{
	  1.00000000000000000000E0,
	  1.00000000000000000000E0,
	  2.00000000000000000000E0,
	  6.00000000000000000000E0,
	  2.40000000000000000000E1,
	  1.20000000000000000000E2,
	  7.20000000000000000000E2,
	  5.04000000000000000000E3,
	  4.03200000000000000000E4,
	  3.62880000000000000000E5,
	  3.62880000000000000000E6,
	  3.99168000000000000000E7,
	  4.79001600000000000000E8,
	  6.22702080000000000000E9,
	  8.71782912000000000000E10,
	  1.30767436800000000000E12,
	  2.09227898880000000000E13,
	  3.55687428096000000000E14,
	  6.40237370572800000000E15,
	  1.21645100408832000000E17,
	  2.43290200817664000000E18,
	  5.10909421717094400000E19,
	  1.12400072777760768000E21,
	  2.58520167388849766400E22,
	  6.20448401733239439360E23,
	  1.55112100433309859840E25,
	  4.03291461126605635584E26,
	  1.0888869450418352160768E28,
	  3.04888344611713860501504E29,
	  8.841761993739701954543616E30,
	  2.6525285981219105863630848E32,
	  8.22283865417792281772556288E33,
	  2.6313083693369353016721801216E35,
	  8.68331761881188649551819440128E36
	};
	
	/* S(x) for small x */
	private final static double sn[] = new double[] {
	-2.99181919401019853726E3,
	 7.08840045257738576863E5,
	-6.29741486205862506537E7,
	 2.54890880573376359104E9,
	-4.42979518059697779103E10,
	 3.18016297876567817986E11,
	};
	private final static double sd[] = new double[] {
	/* 1.00000000000000000000E0,*/
	 2.81376268889994315696E2,
	 4.55847810806532581675E4,
	 5.17343888770096400730E6,
	 4.19320245898111231129E8,
	 2.24411795645340920940E10,
	 6.07366389490084639049E11,
	};
	
	/* C(x) for small x */
	private final static double cn[] = new double[] {
	-4.98843114573573548651E-8,
	 9.50428062829859605134E-6,
	-6.45191435683965050962E-4,
	 1.88843319396703850064E-2,
	-2.05525900955013891793E-1,
	 9.99999999999999998822E-1,
	};
	private final static double cd[] = new double[] {
	 3.99982968972495980367E-12,
	 9.15439215774657478799E-10,
	 1.25001862479598821474E-7,
	 1.22262789024179030997E-5,
	 8.68029542941784300606E-4,
	 4.12142090722199792936E-2,
	 1.00000000000000000118E0,
	};
	
	/* Auxiliary function f(x) */
	private final static double fn[] = new double[] {
	  4.21543555043677546506E-1,
	  1.43407919780758885261E-1,
	  1.15220955073585758835E-2,
	  3.45017939782574027900E-4,
	  4.63613749287867322088E-6,
	  3.05568983790257605827E-8,
	  1.02304514164907233465E-10,
	  1.72010743268161828879E-13,
	  1.34283276233062758925E-16,
	  3.76329711269987889006E-20,
	};
	private final static double fd[] = new double[] {
	/*  1.00000000000000000000E0,*/
	  7.51586398353378947175E-1,
	  1.16888925859191382142E-1,
	  6.44051526508858611005E-3,
	  1.55934409164153020873E-4,
	  1.84627567348930545870E-6,
	  1.12699224763999035261E-8,
	  3.60140029589371370404E-11,
	  5.88754533621578410010E-14,
	  4.52001434074129701496E-17,
	  1.25443237090011264384E-20,
	};
	
	/* Auxiliary function g(x) */
	private final static double gn[] = new double[] {
	  5.04442073643383265887E-1,
	  1.97102833525523411709E-1,
	  1.87648584092575249293E-2,
	  6.84079380915393090172E-4,
	  1.15138826111884280931E-5,
	  9.82852443688422223854E-8,
	  4.45344415861750144738E-10,
	  1.08268041139020870318E-12,
	  1.37555460633261799868E-15,
	  8.36354435630677421531E-19,
	  1.86958710162783235106E-22,
	};
	private final static double gd[] = new double[] {
	/*  1.00000000000000000000E0,*/
	  1.47495759925128324529E0,
	  3.37748989120019970451E-1,
	  2.53603741420338795122E-2,
	  8.14679107184306179049E-4,
	  1.27545075667729118702E-5,
	  1.04314589657571990585E-7,
	  4.60680728146520428211E-10,
	  1.10273215066240270757E-12,
	  1.38796531259578871258E-15,
	  8.39158816283118707363E-19,
	  1.86958710162783236342E-22,
	};
	
	private final static double APSI[] = new double[] {
		 8.33333333333333333333E-2,
		-2.10927960927960927961E-2,
		 7.57575757575757575758E-3,
		-4.16666666666666666667E-3,
		 3.96825396825396825397E-3,
		-8.33333333333333333333E-3,
		 8.33333333333333333333E-2
		};
	
	private double result[];
	
	private int version;
	
	private double par1;
	
	private double par2;
	
	private double[] par3;
	
	private int par4;
	
	private double par5;
	
	private int par6;
	
	private double par7;
	
	private double ssa[];
	
	private double cca[];
	
	public void testCephes() {
		// The test for beta(6.3,2.9) passed
		// The test for chdtr(4,5) passed
		// The test for chdtrc(4,5) passed
		// The test for chdtri(4,0.3) passed
		// The test for dawsn(0.0) passed
		// The test for dawsn(1.0) passed
		// The test for dawsn(2.0) passed
		// The test for ellie(-5.3, 0.12) passed
		// The test for ellik(-5.3, 0.12) passed
        // The test for ellpe(0.0) passed
        // The test for ellpe(0.12) passed
        // The test for ellpe(0.50) passed
        // The test for ellpk(0.0) passed
        // The test for ellpk(0.12) passed
        // The test for ellpk(0.50) passed
		// The test for erf(0) passed
		// The test for erf(0.4) passed
		// The test for erf(0.9) passed
		// The test for erf(0.9) passed
		// The test for expn(1, 0.1) passed
		// The test for expn(2, 0.0) passed
		// The test for expn(2, 3.0) passed
		// The test for expn(20, 1.5) passed
		// The test for expn(20, 100.0) passed
		// The test for fac(40) passed
		// The test for fac(100) passed
		// The test for fdtr(4, 5, 0.3) passed
		// The test for fdtrc(4, 5, 0.3) passed
		// The test for fdtri(4, 5, 0.3) passed
		// The test for fresnl(0.0, ssa, cca) passed
		// The test for fresnl(0.2, ssa, cca) passed
		// The test for fresnl(10.0, ssa, cca) passed
		// The test for fresnl(500.0, ssa, cca) passed
		// The test for gdtr(1, 2, 0.1) passed
		// The test for gdtrc(1, 2, 0.1) passed
		// The test for hyp2f1(0.2, 1.1, 0.3, -1) passed
		// The test for igam(0.5,0) passed
		// The test for igam(1,2) passed
		// lowerIncompleteGamma = 23.297935486152934
		// upperIncompleteGamma = 0.7020645138470673
		// regularizedGammaP = 0.9707473119230389
		// The test for igam(5,10) passed
		// lowerIncompleteGamma = 4.790423305785542E155
		// upperIncompleteGamma = 4.542198238608868E155
		// regularizedGammaP = 0.5132987856625221
		// The test for igam(100,100) passed
		// The test for igmac(2,1) passed
		// The test for igami(2,0.3) passed
		// The test for incbet(1.0, 3.0, 0.3) passed
		// The test for incbi(1.0, 3.0, 0.3) passed
		// The test for lbeta(10.0,3.0) passed
		// The test for lgam(3.4) passed
		// The test for ndtr(0.0) passed
		// The test for ndtr(0.3) passed
		// The test for ndtr(1) passed
		// The test for ndtri(0.5) passed
		// The test for ndtri(0.6) passed
		// The test for psi(-4.9) passed
		// The test for psi(-0.1) passed
		// The test for psi(0.1) passed
		// The test for psi(1.0) passed
		// The test for psi(4.5) passed
		// The test for struve(0.0,0.0) passed
		// The test for struve(0.0,5.0) passed
		// The test for struve(1.0,0.0) passed
		// The test for struve(1.0,5.0) passed
		// The test for zetac(-3.0) passed
		// The test for zetac(-2.0) passed
		// The test for zetac(-1.0) passed
		// The test for zetac(0.0) passed
		// The test for zetac(1.0) passed
		result = new double[1];
		
		result[0] = beta(6.3,2.9);
		if (Math.abs(result[0] - 0.005947104834350) < 1.0E-7) {
	    	System.out.println("The test for beta(6.3,2.9) passed");
	    }
	    else {
	    	System.out.println("The test for beta(6.3,2.9) failed");
	    	System.out.println("Implemented beta gave " + result[0]);
	    	System.out.println("Correct answer is 0.005947104834350");
	    }
		
		result[0] = chdtr(4,5);
		if (Math.abs(result[0] - 0.7127025048163542) < 1.0E-7) {
	    	System.out.println("The test for chdtr(4,5) passed");
	    }
	    else {
	    	System.out.println("The test for chdtr(4,5) failed");
	    	System.out.println("Implemented chdtr gave " + result[0]);
	    	System.out.println("Correct answer is 0.7127025048163542");
	    }
		
		result[0] = chdtr(4,5);
		if (Math.abs(result[0] - 0.7127025048163542) < 1.0E-7) {
	    	System.out.println("The test for chdtr(4,5) passed");
	    }
	    else {
	    	System.out.println("The test for chdtr(4,5) failed");
	    	System.out.println("Implemented chdtr gave " + result[0]);
	    	System.out.println("Correct answer is 0.7127025048163542");
	    }
		
		result[0] = chdtrc(4,5);
		if (Math.abs(result[0] - 0.2872974951836458) < 1.0E-7) {
	    	System.out.println("The test for chdtrc(4,5) passed");
	    }
	    else {
	    	System.out.println("The test for chdtrc(4,5) failed");
	    	System.out.println("Implemented chdtrc gave " + result[0]);
	    	System.out.println("Correct answer is 0.2872974951836458");
	    }
		
	    result[0] = chdtri(4,0.3);
	    if (Math.abs(result[0] - 4.8784329665604087) < 1.0E-7) {
	    	System.out.println("The test for chdtri(4,0.3) passed");
	    }
	    else {
	    	System.out.println("The test for chdtri(4,0.3) failed");
	    	System.out.println("Implemented chdtri gave " + result[0]);
	    	System.out.println("Correct answer is 4.8784329665604087");
	    }
	    
	    result[0] = dawsn(0.0);
	    if (Math.abs(result[0]) < 1.0E-7) {
	    	System.out.println("The test for dawsn(0.0) passed");
	    }
	    else {
	    	System.out.println("The test for dawsn(0.0) failed");
	    	System.out.println("Implemented dawsn gave " + result[0]);
	    	System.out.println("Correct answer is 0.0");
	    }
	    
	    result[0] = dawsn(1.0);
	    if (Math.abs(result[0] - 0.5380795069) < 1.0E-7) {
	    	System.out.println("The test for dawsn(1.0) passed");
	    }
	    else {
	    	System.out.println("The test for dawsn(1.0) failed");
	    	System.out.println("Implemented dawsn gave " + result[0]);
	    	System.out.println("Correct answer is 0.5380795069");
	    }
	    
	    result[0] = dawsn(2.0);
	    if (Math.abs(result[0] - 0.3013403889) < 1.0E-7) {
	    	System.out.println("The test for dawsn(2.0) passed");
	    }
	    else {
	    	System.out.println("The test for dawsn(2.0) failed");
	    	System.out.println("Implemented dawsn gave " + result[0]);
	    	System.out.println("Correct answer is 0.3013403889");
	    }
	    
	    result[0] = ellie(-5.3, 0.12);
	    if (Math.abs(result[0] + 5.12290521194) < 1.0E-7) {
	    	System.out.println("The test for ellie(-5.3, 0.12) passed");
	    }
	    else {
	    	System.out.println("The test for ellie(-5.3, 0.12) failed");
	    	System.out.println("Implemented ellie gave " + result[0]);
	    	System.out.println("Correct answer is -5.12290521194");
	    }
	    
	    result[0] = ellik(-5.3, 0.12);
	    if (Math.abs(result[0] + 5.48607395126) < 1.0E-7) {
	    	System.out.println("The test for ellik(-5.3, 0.12) passed");
	    }
	    else {
	    	System.out.println("The test for ellik(-5.3, 0.12) failed");
	    	System.out.println("Implemented ellik gave " + result[0]);
	    	System.out.println("Correct answer is -5.48607395126");
	    }
	    
	    // ellpe and ellpk answers here from hcephes versions which
	    // start with a line not present in cephes version
	    // line added by Danilo x = 1.0 - x;
	    result[0] = ellpe(0.0);
	    if (Math.abs(result[0] - 1.570796327) < 1.0E-7) {
	    	System.out.println("The test for ellpe(0.0) passed");
	    }
	    else {
	    	System.out.println("The test for ellpe(0.0) failed");
	    	System.out.println("Implemented ellpe gave " + result[0]);
	    	System.out.println("Correct answer is 1.570796327");
	    }
	    
	    result[0] = ellpe(0.12);
	    if (Math.abs(result[0] - 1.522555369217904) < 1.0E-7) {
	    	System.out.println("The test for ellpe(0.12) passed");
	    }
	    else {
	    	System.out.println("The test for ellpe(0.12) failed");
	    	System.out.println("Implemented ellpe gave " + result[0]);
	    	System.out.println("Correct answer is 1.522555369217904");
	    }
	    
	    result[0] = ellpe(0.50);
	    if (Math.abs(result[0] - 1.350643881) < 1.0E-7) {
	    	System.out.println("The test for ellpe(0.50) passed");
	    }
	    else {
	    	System.out.println("The test for ellpe(0.50) failed");
	    	System.out.println("Implemented ellpe gave " + result[0]);
	    	System.out.println("Correct answer is 1.350643881");
	    }
	    
	    result[0] = ellpk(0.0);
	    if (Math.abs(result[0] - 1.570796327) < 1.0E-7) {
	    	System.out.println("The test for ellpk(0.0) passed");
	    }
	    else {
	    	System.out.println("The test for ellpk(0.0) failed");
	    	System.out.println("Implemented ellpk gave " + result[0]);
	    	System.out.println("Correct answer is 1.570796327");
	    }
	    
	    result[0] = ellpk(0.12);
	    if (Math.abs(result[0] - 1.621393137980658) < 1.0E-7) {
	    	System.out.println("The test for ellpk(0.12) passed");
	    }
	    else {
	    	System.out.println("The test for ellpk(0.12) failed");
	    	System.out.println("Implemented ellpk gave " + result[0]);
	    	System.out.println("Correct answer is 1.621393137980658");
	    }
	    
	    result[0] = ellpk(0.50);
	    if (Math.abs(result[0] - 1.8540746773) < 1.0E-7) {
	    	System.out.println("The test for ellpk(0.50) passed");
	    }
	    else {
	    	System.out.println("The test for ellpk(0.50) failed");
	    	System.out.println("Implemented ellpk gave " + result[0]);
	    	System.out.println("Correct answer is 1.8540746773");
	    }
	    
	    result[0] = erf(0);
	    if (Math.abs(result[0]) < 1.0E-7) {
	    	System.out.println("The test for erf(0) passed");
	    }
	    else {
	    	System.out.println("The test for erf(0) failed");
	    	System.out.println("Implemented erf gave " + result[0]);
	    	System.out.println("Correct answer is 0.0");
	    }
	    
	    result[0] = erf(0.4);
	    if (Math.abs(result[0] - 0.4283923550) < 1.0E-7) {
	    	System.out.println("The test for erf(0.4) passed");
	    }
	    else {
	    	System.out.println("The test for erf(0.4) failed");
	    	System.out.println("Implemented erf gave " + result[0]);
	    	System.out.println("Correct answer is 0.4283923550");
	    }
	    
	    result[0] = erf(0.9);
	    if (Math.abs(result[0] - 0.7969082124) < 1.0E-7) {
	    	System.out.println("The test for erf(0.9) passed");
	    }
	    else {
	    	System.out.println("The test for erf(0.9) failed");
	    	System.out.println("Implemented erf gave " + result[0]);
	    	System.out.println("Correct answer is 0.7969082124");
	    }
	    
	    result[0] = erf(1.5);
	    if (Math.abs(result[0] - 0.9661051465) < 1.0E-7) {
	    	System.out.println("The test for erf(0.9) passed");
	    }
	    else {
	    	System.out.println("The test for erf(1.5) failed");
	    	System.out.println("Implemented erf gave " + result[0]);
	    	System.out.println("Correct answer is 0.9661051465");
	    }
	    
	    result[0] = expn(1, 0.1);
	    if (Math.abs(result[0] - 1.8229239) < 1.0E-7) {
	    	System.out.println("The test for expn(1, 0.1) passed");
	    }
	    else {
	    	System.out.println("The test for expn(1, 0.1) failed");
	    	System.out.println("Implemented expn gave " + result[0]);
	    	System.out.println("Correct answer is 1.8229239");
	    }
	    
	    result[0] = expn(2, 0.0);
	    if (Math.abs(result[0] - 1.0000000) < 1.0E-7) {
	    	System.out.println("The test for expn(2, 0.0) passed");
	    }
	    else {
	    	System.out.println("The test for expn(2, 0.0) failed");
	    	System.out.println("Implemented expn gave " + result[0]);
	    	System.out.println("Correct answer is 1.0000000");
	    }
	    
	    result[0] = expn(2, 3.0);
	    if (Math.abs(result[0] - 1.0641925E-2) < 1.0E-9) {
	    	System.out.println("The test for expn(2, 3.0) passed");
	    }
	    else {
	    	System.out.println("The test for expn(2, 3.0) failed");
	    	System.out.println("Implemented expn gave " + result[0]);
	    	System.out.println("Correct answer is 1.0641925E-2");
	    }
	    
	    result[0] = expn(20, 1.5);
	    if (Math.abs(result[0] - 1.0844039E-2) < 1.0E-9) {
	    	System.out.println("The test for expn(20, 1.5) passed");
	    }
	    else {
	    	System.out.println("The test for expn(20, 1.5) failed");
	    	System.out.println("Implemented expn gave " + result[0]);
	    	System.out.println("Correct answer is 1.0844039E-2");
	    }
	    
	    result[0] = expn(20, 100.0);
	    if (Math.abs(result[0] - 3.1043160E-46) < 1.0E-53) {
	    	System.out.println("The test for expn(20, 100.0) passed");
	    }
	    else {
	    	System.out.println("The test for expn(20, 100.0) failed");
	    	System.out.println("Implemented expn gave " + result[0]);
	    	System.out.println("Correct answer is 3.1043160E-46");
	    }
	    
	    result[0] = fac(40);
	    if (Math.abs(result[0] - 8.1591528325E47) < 1.0E40) {
	    	System.out.println("The test for fac(40) passed");
	    }
	    else {
	    	System.out.println("The test for fac(40) failed");
	    	System.out.println("Implemented fac gave " + result[0]);
	    	System.out.println("Correct answer is 8.1591528325E47");
	    }
	    
	    result[0] = fac(100);
	    if (Math.abs(result[0] - 9.3326215444E157) < 1.0E150) {
	    	System.out.println("The test for fac(100) passed");
	    }
	    else {
	    	System.out.println("The test for fac(100) failed");
	    	System.out.println("Implemented fac gave " + result[0]);
	    	System.out.println("Correct answer is 9.3326215444E157");
	    }
	    
	    result[0] = fdtr(4, 5, 0.3);
	    if (Math.abs(result[0] - 0.1333536247071635) < 1.0E-7) {
	    	System.out.println("The test for fdtr(4, 5, 0.3) passed");
	    }
	    else {
	    	System.out.println("The test for fdtr(4, 5, 0.3) failed");
	    	System.out.println("Implemented fdtr gave " + result[0]);
	    	System.out.println("Correct answer is 0.1333536247071635");
	    }
	    
	    result[0] = fdtrc(4, 5, 0.3);
	    if (Math.abs(result[0] - 0.8666463752928364) < 1.0E-7) {
	    	System.out.println("The test for fdtrc(4, 5, 0.3) passed");
	    }
	    else {
	    	System.out.println("The test for fdtrc(4, 5, 0.3) failed");
	    	System.out.println("Implemented fdtrc gave " + result[0]);
	    	System.out.println("Correct answer is 0.8666463752928364");
	    }
	    
	    result[0] = fdtri(4, 5, 0.3);
	    if (Math.abs(result[0] - 0.56493190151185757) < 1.0E-7) {
	    	System.out.println("The test for fdtri(4, 5, 0.3) passed");
	    }
	    else {
	    	System.out.println("The test for fdtri(4, 5, 0.3) failed");
	    	System.out.println("Implemented fdtri gave " + result[0]);
	    	System.out.println("Correct answer is 0.56493190151185757");
	    }
	    
	    ssa = new double[1];
	    cca = new double[1];
	    fresnl(0.0, ssa, cca);
	    if ((Math.abs(ssa[0]) < 1.0E-7) && (Math.abs(cca[0]) < 1.0E-7)) {
	    	System.out.println("The test for fresnl(0.0, ssa, cca) passed");
	    }
	    else {
	    	System.out.println("The test for fresnl(0.0, ssa, cca) failed");
	    	System.out.println("Implemented fresnl gave ssa[0] = " + ssa[0] + " cca[0] = " + cca[0]);
	    	System.out.println("Correct answer is ssa[0] = 0.0 cca[0] = 0.0");
	    }
	    
	    fresnl(0.2, ssa, cca);
	    if ((Math.abs(ssa[0] - 0.00418761) < 1.0E-7) && (Math.abs(cca[0] - 0.19992106) < 1.0E-7)) {
	    	System.out.println("The test for fresnl(0.2, ssa, cca) passed");
	    }
	    else {
	    	System.out.println("The test for fresnl(0.2, ssa, cca) failed");
	    	System.out.println("Implemented fresnl gave ssa[0] = " + ssa[0] + " cca[0] = " + cca[0]);
	    	System.out.println("Correct answer is ssa[0] = 0.00418761 cca[0] = 0.19992106");
	    }
	    
	    fresnl(10.0, ssa, cca);
	    if ((Math.abs(ssa[0] - 0.46816998) < 1.0E-7) && (Math.abs(cca[0] - 0.49989869) < 1.0E-7)) {
	    	System.out.println("The test for fresnl(10.0, ssa, cca) passed");
	    }
	    else {
	    	System.out.println("The test for fresnl(10.0, ssa, cca) failed");
	    	System.out.println("Implemented fresnl gave ssa[0] = " + ssa[0] + " cca[0] = " + cca[0]);
	    	System.out.println("Correct answer is ssa[0] = 0.46816998 cca[0] = 0.49989869");
	    }
	    
	    fresnl(500.0, ssa, cca);
	    if ((Math.abs(ssa[0] - 0.49936338) < 1.0E-7) && (Math.abs(cca[0] - 0.5000000) < 1.0E-7)) {
	    	System.out.println("The test for fresnl(500.0, ssa, cca) passed");
	    }
	    else {
	    	System.out.println("The test for fresnl(500.0, ssa, cca) failed");
	    	System.out.println("Implemented fresnl gave ssa[0] = " + ssa[0] + " cca[0] = " + cca[0]);
	    	System.out.println("Correct answer is ssa[0] = 0.49936338 cca[0] = 0.5000000");
	    }
	    
	    result[0] = gdtr(1, 2, 0.1);
	    if (Math.abs(result[0] -  0.0046788401604445) < 1.0E-7) {
	    	System.out.println("The test for gdtr(1, 2, 0.1) passed");
	    }
	    else {
	    	System.out.println("The test for gdtr(1, 2, 0.1) failed");
	    	System.out.println("Implemented gdtr gave " + result[0]);
	    	System.out.println("Correct answer is 0.0046788401604445");
	    }
	    
	    result[0] = gdtrc(1, 2, 0.1);
	    if (Math.abs(result[0] -  0.9953211598395555) < 1.0E-7) {
	    	System.out.println("The test for gdtrc(1, 2, 0.1) passed");
	    }
	    else {
	    	System.out.println("The test for gdtrc(1, 2, 0.1) failed");
	    	System.out.println("Implemented gdtrc gave " + result[0]);
	    	System.out.println("Correct answer is 0.9953211598395555");
	    }
	    
	    result[0] = hyp2f1(0.2, 1.1, 0.3, -1);
	    if (Math.abs(result[0] -  0.62482831198989075) < 1.0E-7) {
	    	System.out.println("The test for hyp2f1(0.2, 1.1, 0.3, -1) passed");
	    }
	    else {
	    	System.out.println("The test for hyp2f1(0.2, 1.1, 0.3, -1) failed");
	    	System.out.println("Implemented hyp2f1 gave " + result[0]);
	    	System.out.println("Correct answer is 0.62482831198989075");
	    }
	    
	    double lowerIncompleteGamma[] = new double[1];
	    double upperIncompleteGamma[] = new double[1];
	    double regularizedGammaP[] = new double[1];
	    Gamma gam = new Gamma(0.5, 0, lowerIncompleteGamma, upperIncompleteGamma, regularizedGammaP);
	    gam.run();
	    result[0] = igam(0.5,0);
	    if (Math.abs(result[0] - regularizedGammaP[0]) < 1.0E-7) {
	    	System.out.println("The test for igam(0.5,0) passed");
	    }
	    else {
	    	System.out.println("The test for igam(0.5,0) failed");
	    	System.out.println("Implemented igam gave " + result[0]);
	    	System.out.println("Correct answer is " + regularizedGammaP[0]);
	    }
	    
	    gam = new Gamma(1, 2, lowerIncompleteGamma, upperIncompleteGamma, regularizedGammaP);
	    gam.run();
	    result[0] = igam(1,2);
	    if (Math.abs(result[0] - regularizedGammaP[0]) < 1.0E-7) {
	    	System.out.println("The test for igam(1,2) passed");
	    }
	    else {
	    	System.out.println("The test for igam(1,2) failed");
	    	System.out.println("Implemented igam gave " + result[0]);
	    	System.out.println("Correct answer is " + regularizedGammaP[0]);
	    }
	    
	    gam = new Gamma(5, 10, lowerIncompleteGamma, upperIncompleteGamma, regularizedGammaP);
	    gam.run();
	    System.out.println("lowerIncompleteGamma = " + lowerIncompleteGamma[0]);
	    System.out.println("upperIncompleteGamma = " + upperIncompleteGamma[0]);
	    System.out.println("regularizedGammaP = " + regularizedGammaP[0]);
	    result[0] = igam(5,10);
	    if (Math.abs(result[0] - regularizedGammaP[0]) < 1.0E-7) {
	    	System.out.println("The test for igam(5,10) passed");
	    }
	    else {
	    	System.out.println("The test for igam(5,10) failed");
	    	System.out.println("Implemented igam gave " + result[0]);
	    	System.out.println("Correct answer is " + regularizedGammaP[0]);
	    }
	    
	    gam = new Gamma(100, 100, lowerIncompleteGamma, upperIncompleteGamma, regularizedGammaP);
	    gam.run();
	    System.out.println("lowerIncompleteGamma = " + lowerIncompleteGamma[0]);
	    System.out.println("upperIncompleteGamma = " + upperIncompleteGamma[0]);
	    System.out.println("regularizedGammaP = " + regularizedGammaP[0]);
	    result[0] = igam(100,100);
	    if (Math.abs(result[0] - regularizedGammaP[0]) < 1.0E-7) {
	    	System.out.println("The test for igam(100,100) passed");
	    }
	    else {
	    	System.out.println("The test for igam(100,100) failed");
	    	System.out.println("Implemented igam gave " + result[0]);
	    	System.out.println("Correct answer is " + regularizedGammaP[0]);
	    }
	    
	    result[0] = igamc(2,1);
	    if (Math.abs(result[0] - 0.7357588823428847) < 1.0E-7) {
	    	System.out.println("The test for igmac(2,1) passed");
	    }
	    else {
	    	System.out.println("The test for igmac(2,1) failed");
	    	System.out.println("Implemented igmac gave " + result[0]);
	    	System.out.println("Correct answer is 0.7357588823428847");
	    }
	    
	    result[0] = igami(2,0.3);
	    if (Math.abs(result[0] - 2.439216483280204) < 1.0E-7) {
	    	System.out.println("The test for igami(2,0.3) passed");
	    }
	    else {
	    	System.out.println("The test for igami(2,0.3) failed");
	    	System.out.println("Implemented igami gave " + result[0]);
	    	System.out.println("Correct answer is 2.439216483280204");
	    }
	    
	    result[0] = incbet(1.0, 3.0, 0.3);
	    if (Math.abs(result[0] - 0.65699999999999992) < 1.0E-7) {
	    	System.out.println("The test for incbet(1.0, 3.0, 0.3) passed");
	    }
	    else {
	    	System.out.println("The test for incbet(1.0, 3.0, 0.3) failed");
	    	System.out.println("Implemented incbet gave " + result[0]);
	    	System.out.println("Correct answer is 0.65699999999999992");
	    }
	    
	    result[0] = incbi(1.0, 3.0, 0.3);
	    if (Math.abs(result[0] - 0.1120959982573993) < 1.0E-7) {
	    	System.out.println("The test for incbi(1.0, 3.0, 0.3) passed");
	    }
	    else {
	    	System.out.println("The test for incbi(1.0, 3.0, 0.3) failed");
	    	System.out.println("Implemented incbi gave " + result[0]);
	    	System.out.println("Correct answer is 0.1120959982573993");
	    }
	    
	    result[0] = lbeta(10.0,3.0);
		if (Math.abs(result[0] + 6.4922398350204711) < 1.0E-7) {
	    	System.out.println("The test for lbeta(10.0,3.0) passed");
	    }
	    else {
	    	System.out.println("The test for lbeta(10.0,3.0) failed");
	    	System.out.println("Implemented lbeta gave " + result[0]);
	    	System.out.println("Correct answer is -6.4922398350204711");
	    }
		
		result[0] = lgam(3.4);
		if (Math.abs(result[0] - 1.0923280598027414) < 1.0E-7) {
	    	System.out.println("The test for lgam(3.4) passed");
	    }
	    else {
	    	System.out.println("The test for lgam(3.4) failed");
	    	System.out.println("Implemented lgam gave " + result[0]);
	    	System.out.println("Correct answer is 1.0923280598027414");
	    }
	    
	    result[0] = ndtr(0.0);
	    if (result[0] == 0.5) {
	    	System.out.println("The test for ndtr(0.0) passed");
	    }
	    else {
	    	System.out.println("The test for ndtr(0.0) failed");
	    	System.out.println("Implemented ndtr gave " + result[0]);
	    	System.out.println("Correct answer is 0.5");
	    }
	    
	    result[0] = ndtr(0.3);
	    if (Math.abs(result[0] - 0.61791142218895256) < 1.0E-7) {
	    	System.out.println("The test for ndtr(0.3) passed");
	    }
	    else {
	    	System.out.println("The test for ndtr(0.3) failed");
	    	System.out.println("Implemented ndtr gave " + result[0]);
	    	System.out.println("Correct answer is 0.61791142218895256");
	    }
	    
	    result[0] = ndtr(1);
	    if (Math.abs(result[0] - 0.8413447460685429) < 1.0E-7) {
	    	System.out.println("The test for ndtr(1) passed");
	    }
	    else {
	    	System.out.println("The test for ndtr(1) failed");
	    	System.out.println("Implemented ndtr gave " + result[0]);
	    	System.out.println("Correct answer is 0.8413447460685429");
	    }
	    
	    result[0] = ndtri(0.5);
	    if (result[0] == 0.0) {
	    	System.out.println("The test for ndtri(0.5) passed");
	    }
	    else {
	    	System.out.println("The test for ndtri(0.5) failed");
	    	System.out.println("Implemented ndtri gave " + result[0]);
	    	System.out.println("Correct answer is 0.0");
	    }
	    
	    result[0] = ndtri(0.6);
	    if (Math.abs(result[0] - 0.25334710313579972) < 1.0E-7) {
	    	System.out.println("The test for ndtri(0.6) passed");
	    }
	    else {
	    	System.out.println("The test for ndtri(0.6) failed");
	    	System.out.println("Implemented ndtri gave " + result[0]);
	    	System.out.println("Correct answer is 0.25334710313579972");
	    }
	    
	    result[0] = psi(-4.9);
	    if (Math.abs(result[0] + 7.9810086) < 1.0E-7) {
	    	System.out.println("The test for psi(-4.9) passed");
	    }
	    else {
	    	System.out.println("The test for psi(-4.9) failed");
	    	System.out.println("Implemented psi gave " + result[0]);
	    	System.out.println("Correct answer is -7.9810086");
	    }
	    
	    result[0] = psi(-0.1);
	    if (Math.abs(result[0] - 9.2450731) < 1.0E-7) {
	    	System.out.println("The test for psi(-0.1) passed");
	    }
	    else {
	    	System.out.println("The test for psi(-0.1) failed");
	    	System.out.println("Implemented psi gave " + result[0]);
	    	System.out.println("Correct answer is 9.2450731");
	    }
	    
	    result[0] = psi(0.1);
	    if (Math.abs(result[0] + 10.4237549) < 1.0E-7) {
	    	System.out.println("The test for psi(0.1) passed");
	    }
	    else {
	    	System.out.println("The test for psi(0.1) failed");
	    	System.out.println("Implemented psi gave " + result[0]);
	    	System.out.println("Correct answer is -10.4237549");
	    }
	    
	    result[0] = psi(1.0);
	    if (Math.abs(result[0] + 0.57721566) < 1.0E-7) {
	    	System.out.println("The test for psi(1.0) passed");
	    }
	    else {
	    	System.out.println("The test for psi(1.0) failed");
	    	System.out.println("Implemented psi gave " + result[0]);
	    	System.out.println("Correct answer is -0.57721566");
	    }
	    
	    result[0] = psi(4.5);
	    if (Math.abs(result[0] - 1.38887093) < 1.0E-7) {
	    	System.out.println("The test for psi(4.5) passed");
	    }
	    else {
	    	System.out.println("The test for psi(4.5) failed");
	    	System.out.println("Implemented psi gave " + result[0]);
	    	System.out.println("Correct answer is 1.38887093");
	    }
	    
	    result[0] = struve(0.0,0.0);
	    if (Math.abs(result[0]) < 1.0E-7) {
	    	System.out.println("The test for struve(0.0,0.0) passed");
	    }
	    else {
	    	System.out.println("The test for struve(0.0,0.0) failed");
	    	System.out.println("Implemented struve gave " + result[0]);
	    	System.out.println("Correct answer is 0.0");
	    }
	    
	    result[0] = struve(0.0,5.0);
	    if (Math.abs(result[0] + 0.1852168) < 1.0E-7) {
	    	System.out.println("The test for struve(0.0,5.0) passed");
	    }
	    else {
	    	System.out.println("The test for struve(0.0,5.0) failed");
	    	System.out.println("Implemented struve gave " + result[0]);
	    	System.out.println("Correct answer is -0.1852168");
	    }
	    
	    result[0] = struve(1.0,0.0);
	    if (Math.abs(result[0]) < 1.0E-7) {
	    	System.out.println("The test for struve(1.0,0.0) passed");
	    }
	    else {
	    	System.out.println("The test for struve(1.0,0.0) failed");
	    	System.out.println("Implemented struve gave " + result[0]);
	    	System.out.println("Correct answer is 0.0");
	    }
	    
	    result[0] = struve(1.0,5.0);
	    if (Math.abs(result[0] - 0.8078119) < 1.0E-7) {
	    	System.out.println("The test for struve(1.0,5.0) passed");
	    }
	    else {
	    	System.out.println("The test for struve(1.0,5.0) failed");
	    	System.out.println("Implemented struve gave " + result[0]);
	    	System.out.println("Correct answer is 0.8078119");
	    }
	    
	    result[0] = zetac(-3.0);
	    if (Math.abs(result[0] + (119.0/120.0)) < 1.0E-7) {
	    	System.out.println("The test for zetac(-3.0) passed");
	    }
	    else {
	    	System.out.println("The test for zetac(-3.0) failed");
	    	System.out.println("Implemented zetac gave " + result[0]);
	    	System.out.println("Correct answer is " + (-119.0/120.0));
	    }
	    
	    result[0] = zetac(-2.0);
	    if (Math.abs(result[0] + 1.0) < 1.0E-7) {
	    	System.out.println("The test for zetac(-2.0) passed");
	    }
	    else {
	    	System.out.println("The test for zetac(-2.0) failed");
	    	System.out.println("Implemented zetac gave " + result[0]);
	    	System.out.println("Correct answer is -1.0");
	    }
	    
	    result[0] = zetac(-1.0);
	    if (Math.abs(result[0] + (13.0/12.0)) < 1.0E-7) {
	    	System.out.println("The test for zetac(-1.0) passed");
	    }
	    else {
	    	System.out.println("The test for zetac(-1.0) failed");
	    	System.out.println("Implemented zetac gave " + result[0]);
	    	System.out.println("Correct answer is " + (-13.0/12.0));
	    }
	    
	    result[0] = zetac(0.0);
	    if (Math.abs(result[0] + 1.5) < 1.0E-7) {
	    	System.out.println("The test for zetac(0.0) passed");
	    }
	    else {
	    	System.out.println("The test for zetac(0.0) failed");
	    	System.out.println("Implemented zetac gave " + result[0]);
	    	System.out.println("Correct answer is -1.5");
	    }
	    
	    result[0] = zetac(1.0);
	    if (Double.isInfinite(result[0])) {
	    	System.out.println("The test for zetac(1.0) passed");
	    }
	    else {
	    	System.out.println("The test for zetac(1.0) failed");
	    	System.out.println("Implemented zetac gave " + result[0]);
	    	System.out.println("Correct answer is Double.POSITIVE_INFINITY");
	    }
	    
	}
	
	public Cephes() {
		
	}
	
	public Cephes(double par1, int version, double result[]) {
		this.par1 = par1;
		this.version = version;
		this.result = result;
	}
	
	
	public Cephes(double par1, int version, double ssa[], double cca[]) {
		this.par1 = par1;
		this.version = version;
		this.ssa = ssa;
		this.cca = cca;
	}
	
	public Cephes(double par1, double par2, int version, double result[]) {
		this.par1 = par1;
		this.par2 = par2;
		this.version = version;
		this.result = result;
	}
	
	public Cephes(double par1, double par2, double par5, int version, double result[]) {
		this.par1 = par1;
		this.par2 = par2;
		this.par5 = par5;
		this.version = version;
		this.result = result;
	}
	
	public Cephes(int par4, int version, double result[]) {
		this.par4 = par4;
		this.version = version;
		this.result = result;
	}
	
	public Cephes(int par4, double par1, int version, double result[]) {
		this.par4 = par4;
		this.par1 = par1;
		this.version = version;
		this.result = result;
	}
	
	public Cephes(int par4, int par6, double par1, int version, double result[]) {
		this.par4 = par4;
		this.par6 = par6;
		this.par1 = par1;
		this.version = version;
		this.result = result;
	}
	
	public Cephes(double par1, double par3[], int par4, int version, double result[]) {
		this.par1 = par1;
		this.par3 = par3;
		this.par4 = par4;
		this.version = version;
		this.result = result;
	}
	
	public void run() {
		if (version == BETA) {
			result[0] = beta(par1, par2);
		}
		else if (version == CHBEVL) {
			result[0] = chbevl(par1, par3, par4);
		}
		else if (version == CHDTR) {
			result[0] = chdtr(par1, par2);
		}
	    else if (version == CHDTRC) {
	    	result[0] = chdtrc(par1, par2);
		}
	    else if (version == CHDTRI) {
	    	result[0] = chdtri(par1, par2);
		}
	    else if (version == DAWSN) {
	    	result[0] = dawsn(par1);
	    }
	    else if (version == ELLIE) {
	    	result[0] = ellie(par1, par2);
	    }
	    else if (version == ELLIK) {
	    	result[0] = ellik(par1, par2);
	    }
	    else if (version == ELLPE) {
	    	result[0] = ellpe(par1);
	    }
	    else if (version == ELLPK)
	    	result[0] = ellpk(par1);
	    else if (version == ERF) {
	    	result[0] = erf(par1);
	    }
	    else if (version == ERFC) {
	    	result[0] = erfc(par1);
	    }
	    else if (version == EXPN) {
	    	result[0] = expn(par4, par1);
	    }
	    else if (version == FAC) {
	    	result[0] = fac(par4);
	    }
	    else if (version == FDTR) {
	    	result[0] = fdtr(par4, par6, par1);
	    }
	    else if (version == FDTRC) {
	    	result[0] = fdtrc(par4, par6, par1);
	    }
	    else if (version == FDTRI) {
	    	result[0] = fdtri(par4, par6, par1);
	    }
	    else if (version == FRESNL) {
	    	fresnl(par1, ssa, cca);
	    }
	    else if (version == GDTR) {
	    	result[0] = gdtr(par1, par2, par5);
	    }
	    else if (version == GDTRC) {
	    	result[0] = gdtrc(par1, par2, par5);
	    }
	    else if (version == HYP2F1) {
	    	result[0] = hyp2f1(par1, par2, par5, par7);
	    }
		else if (version == IGAMI) {
			result[0] = igami(par1, par2);
		}
		else if (version == IGAMC) {
			result[0] = igamc(par1, par2);
		}
		else if (version == IGAM) {
			result[0] = igam(par1,par2);
		}
		else if (version == INCBET) {
			result[0] = incbet(par1, par2, par5);
		}
		else if (version == INCBI) {
			result[0] = incbi(par1, par2, par5);
		}
		else if (version == LBETA) {
			result[0] = lbeta(par1,par2);
		}
		else if (version == LGAM) {
			result[0] = lgam(par1);
		}
		else if (version == NDTR) {
			result[0] = ndtr(par1);
		}
		else if (version == NDTRI) {
			result[0] = ndtri(par1);
		}
		else if (version == POLEVL) {
			result[0] = polevl(par1, par3, par4);
		}
		else if (version == P1EVL) {
			result[0] = p1evl(par1, par3, par4);
		}
		else if (version == PSI) {
			result[0] = psi(par1);
		}
		else if (version == STIRF) {
			result[0] = stirf(par1);
		}
		else if (version == STRUVE) {
			result[0] = struve(par1, par2);
		}
		else if (version == TRUE_GAMMA) {
			result[0] = true_gamma(par1);
		}
		else if (version == ZETA) {
			result[0] = zeta(par1, par2);
		}
		else if (version == ZETAC) {
			result[0] = zetac(par1);
		}
	}
	
	/*							chdtr.c
	 *
	 *	Chi-square distribution
	 *
	 *
	 *
	 * SYNOPSIS:
	 *
	 * double df, x, y, chdtr();
	 *
	 * y = chdtr( df, x );
	 *
	 *
	 *
	 * DESCRIPTION:
	 *
	 * Returns the area under the left hand tail (from 0 to x)
	 * of the Chi square probability density function with
	 * v degrees of freedom.
	 *
	 *
	 *                                  inf.
	 *                                    -
	 *                        1          | |  v/2-1  -t/2
	 *  P( x | v )   =   -----------     |   t      e     dt
	 *                    v/2  -       | |
	 *                   2    | (v/2)   -
	 *                                   x
	 *
	 * where x is the Chi-square variable.
 *
 * The incomplete gamma integral is used, according to the
 * formula
 *
 *	y = chdtr( v, x ) = igam( v/2.0, x/2.0 ).
 *
 *
 * The arguments must both be positive.
 *
 *
 *
 * ACCURACY:
 *
 * See igam().
 *
 * ERROR MESSAGES:
 *
 *   message         condition      value returned
 * chdtr domain   x < 0 or v < 1        0.0
 */
	
	public double chdtr(double df, double x) {

	if( (x < 0.0) || (df < 1.0) )
		{
		    MipavUtil.displayError("Domain error in chdtr()");
		    return(0.0);
		}
        return(igam( df/2.0, x/2.0 ));
	}
	
	/*							chdtrc()
	 *
	 *	Complemented Chi-square distribution
	 *
	 *
	 *
	 * SYNOPSIS:
	 *
	 * double v, x, y, chdtrc();
	 *
	 * y = chdtrc( v, x );
	 *
	 *
	 *
	 * DESCRIPTION:
	 *
	 * Returns the area under the right hand tail (from x to
	 * infinity) of the Chi square probability density function
	 * with v degrees of freedom:
	 *
	 *
	 *                                  inf.
	 *                                    -
	 *                    v/2  -       | |
	 *                   2    | (v/2)   -
	 *                                   x
	 *
	 * where x is the Chi-square variable.
	 *
	 * The incomplete gamma integral is used, according to the
	 * formula
	 *
	 *	y = chdtr( v, x ) = igamc( v/2.0, x/2.0 ).
	 *
	 *
	 * The arguments must both be positive.
	 *
	 *
	 *
	 * ACCURACY:
	 *
	 * See igamc().
	 *
	 * ERROR MESSAGES:
	 *
	 *   message         condition      value returned
	 * chdtrc domain  x < 0 or v < 1        0.0
	 */
	public double chdtrc(double df, double x)
	{

	if( (x < 0.0) || (df < 1.0) )
		{
		MipavUtil.displayError("Domain error in chdtrc()");
		return(0.0);
		}
	    return(igamc( df/2.0, x/2.0 ));
	}
	
	/*							chdtri()
	 *
	 *	Inverse of complemented Chi-square distribution
	 *
	 *
	 *
	 * SYNOPSIS:
	 *
	 * double df, x, y, chdtri();
	 *
	 * x = chdtri( df, y );
	 *
	 *
	 *
	 *
	 * DESCRIPTION:
	 *
	 * Finds the Chi-square argument x such that the integral
	 * from x to infinity of the Chi-square density is equal
	 * to the given cumulative probability y.
	 *
	 * This is accomplished using the inverse gamma integral
	 * function and the relation
	 *
	 *    x/2 = igami( df/2, y );
	 *
	 *
	 *
	 *
	 * ACCURACY:
	 * 
	 * See igami.c.
	 *
	 * ERROR MESSAGES:
	 *
	 *   message         condition      value returned
	 * chdtri domain   y < 0 or y > 1        0.0
	 *                     v < 1
	 *
	 */
	
	/*
	Cephes Math Library Release 2.0:  April, 1987
	Copyright 1984, 1987 by Stephen L. Moshier
	Direct inquiries to 30 Frost Street, Cambridge, MA 02140
	*/
	public double chdtri(double df, double y) {
	    double x;
	    if ((y < 0) || ( y > 1.0) || (df < 1.0)) {
	    	MipavUtil.displayError("Domain error in chdtri()");
	    	return (0.0);
	    }
	    
	    x = igami( 0.5 * df, y );
	    return (2.0 * x );
	    
	}
	
	/*							erf.c
	 *
	 *	Error function
	 *
	 *
	 *
	 * SYNOPSIS:
	 *
	 * double x, y, erf();
	 *
	 * y = erf( x );
	 *
	 *
	 *
	 * DESCRIPTION:
	 *
	 * The integral is
	 *
	 *                           x 
	 *                            -
	 *                 2         | |          2
	 *   erf(x)  =  --------     |    exp( - t  ) dt.
	 *              sqrt(pi)   | |
	 *                          -
	 *                           0
	 *
	 * The magnitude of x is limited to 9.231948545 for DEC
	 * arithmetic; 1 or -1 is returned outside this range.
	 *
	 * For 0 <= |x| < 1, erf(x) = x * P4(x**2)/Q5(x**2); otherwise
	 * erf(x) = 1 - erfc(x).
	 *
	 *
	 *
	 * ACCURACY:
	 *
	 *                      Relative error:
	 * arithmetic   domain     # trials      peak         rms
	 *    DEC       0,1         14000       4.7e-17     1.5e-17
	 *    IEEE      0,1         30000       3.7e-16     1.0e-16
	 *
	 */
	public double erf(double x)
	{
	double z;

	if( Math.abs(x) > 1.0 ) {
		return (1.0 - erfc(x));
	}
	z = x * x;
	
	return (x * polevl(z, T, 4) /p1evl(z, U, 5)) ;
	}
	
	/*							erfc.c
	 *
	 *	Complementary error function
	 *
	 *
	 *
	 * SYNOPSIS:
	 *
	 * double x, y, erfc();
	 *
	 * y = erfc( x );
	 *
	 *
	 *
	 * DESCRIPTION:
	 *
	 *
	 *  1 - erf(x) =
	 *
	 *                           inf. 
	 *                             -
	 *                  2         | |          2
	 *   erfc(x)  =  --------     |    exp( - t  ) dt
	 *               sqrt(pi)   | |
	 *                           -
	 *                            x
	 *
	 *
	 * For small x, erfc(x) = 1 - erf(x); otherwise rational
	 * approximations are computed.
	 *
	 *
	 *
	 * ACCURACY:
	 *
	 *                      Relative error:
	 * arithmetic   domain     # trials      peak         rms
	 *    DEC       0, 9.2319   12000       5.1e-16     1.2e-16
	 *    IEEE      0,26.6417   30000       5.7e-14     1.5e-14
	 *
	 *
	 * ERROR MESSAGES:
	 *
	 *   message         condition              value returned
	 * erfc underflow    x > 9.231948545 (DEC)       0.0
	 *
	 *
	 */
	public double erfc(double a)
	{
	double p,q,x,y,z;


	if( a < 0.0 )
		x = -a;
	else
		x = a;

	if( x < 1.0 ) {
		return (1.0 - erf(a));
	}

	z = -a * a;

	if( z < -MAXLOG )
		{
			System.err.println("Underflow in erfc()");
			if( a < 0 ) {
				return (2.0);
			}
			else {
				return (0.0);
			}
		
		} // if( z < -MAXLOG )

	z = Math.exp(z);

	if( x < 8.0 )
		{
		p = polevl( x, P, 8 );
		q = p1evl( x, Q, 8 );
		}
	else
		{
		p = polevl( x, R, 5 );
		q = p1evl( x, S, 6 );
		}
	y = (z * p)/q;

	if( a < 0 )
		y = 2.0 - y;

	if ( y == 0.0 ) {
		System.err.println("Underflow in erfc()");
		if( a < 0 ) {
			return (2.0);
		}
		else {
			return (0.0);
		}	
	} // if ( y == 0.0 )

	return y;
	}
	
	/*							ndtr.c
	 *
	 *	Normal distribution function
	 *
	 *
	 *
	 * SYNOPSIS:
	 *
	 * double x, y, ndtr();
	 *
	 * y = ndtr( x );
	 *
	 *
	 *
	 * DESCRIPTION:
	 *
	 * Returns the area under the Gaussian probability density
	 * function, integrated from minus infinity to x:
	 *
	 *                            x
	 *                             -
	 *                   1        | |          2
	 *    ndtr(x)  = ---------    |    exp( - t /2 ) dt
	 *               sqrt(2pi)  | |
	 *                           -
	 *                          -inf.
	 *
	 *             =  ( 1 + erf(z) ) / 2
	 *             =  erfc(z) / 2
	 *
	 * where z = x/sqrt(2). Computation is via the functions
	 * erf and erfc.
	 *
	 *
	 * ACCURACY:
	 *
	 *                      Relative error:
	 * arithmetic   domain     # trials      peak         rms
	 *    DEC      -13,0         8000       2.1e-15     4.8e-16
	 *    IEEE     -13,0        30000       3.4e-14     6.7e-15
	 *
	 *
	 * ERROR MESSAGES:
	 *
	 *   message         condition         value returned
	 * erfc underflow    x > 37.519379347       0.0
	 *
	 */
	public double ndtr(double a)
	{
	double x, y, z;

	x = a * SQRTH;
	z = Math.abs(x);

	if( z < SQRTH ) {
		y = 0.5 + 0.5 * erf(x);
	}

	else
		{
		y = 0.5 * erfc(z);

		if( x > 0 )
			y = 1.0 - y;
		}

	return y ;
	}
	
	/*							igami()
	 *
	 *      Inverse of complemented imcomplete gamma integral
	 *
	 *
	 *
	 * SYNOPSIS:
	 *
	 * double a, x, p, igami();
	 *
	 * x = igami( a, p );
	 *
	 * DESCRIPTION:
	 *
	 * Given p, the function finds x such that
	 *
	 *  igamc( a, x ) = p.
	 *
	 * Starting with the approximate value
	 *
	 *         3
	 *  x = a t
	 *
	 *  where
	 *
	 *  t = 1 - d - ndtri(p) sqrt(d)
	 * 
	 * and
	 *
	 *  d = 1/9a,
	 *
	 * the routine performs up to 10 Newton iterations to find the
	 * root of igamc(a,x) - p = 0.
	 *
	 * ACCURACY:
	 *
	 ** Tested at random a, p in the intervals indicated.
	 *
	 *                a        p                      Relative error:
	 * arithmetic   domain   domain     # trials      peak         rms
	 *    IEEE     0.5,100   0,0.5       100000       1.0e-14     1.7e-15
	 *    IEEE     0.01,0.5  0,0.5       100000       9.0e-14     3.4e-15
	 *    IEEE    0.5,10000  0,0.5        20000       2.3e-13     3.8e-14
	 */
	
	/*
	Cephes Math Library Release 2.3:  March, 1995
	Copyright 1984, 1987, 1995 by Stephen L. Moshier
	*/
	public double igami(double a, double y0) {
		double x0, x1, x, yl, yh, y, d, lgm, dithresh;
		int i, dir;

		/* bound the solution */
		x0 = MAXNUM;
		yl = 0;
		x1 = 0;
		yh = 1.0;
		dithresh = 5.0 * MACHEP;
		
		/* approximation to inverse function */
		d = 1.0/(9.0*a);
		y = ( 1.0 - d - ndtri(y0) * Math.sqrt(d) );
		x = a * y * y * y;
		
		lgm = lgam(a);

		for( i=0; i<10; i++ )
		{
		if( x > x0 || x < x1 )
			break;
		y = igamc(a,x);
		if( y < yl || y > yh )
			break;
		if( y < y0 )
			{
			x0 = x;
			yl = y;
			}
		else
			{
			x1 = x;
			yh = y;
			}
	/* compute the derivative of the function at this point */
		d = (a - 1.0) * Math.log(x) - x - lgm;
		if( d < -MAXLOG )
			break;
		d = -Math.exp(d);
	/* compute the step to the next approximation of x */
		d = (y - y0)/d;
		if( Math.abs(d/x) < MACHEP ) {
			return x;
		}
		x = x - d;
		} // for( i=0; i<10; i++ )
		
		/* Resort to interval halving if Newton iteration did not converge. */

		d = 0.0625;
		if( x0 == MAXNUM )
			{
			if( x <= 0.0 )
				x = 1.0;
			while( x0 == MAXNUM )
				{
				x = (1.0 + d) * x;
				y = igamc( a, x );
				if( y < y0 )
					{
					x0 = x;
					yl = y;
					break;
					}
				d = d + d;
				}
			}
		d = 0.5;
		dir = 0;

		for( i=0; i<400; i++ )
			{
			x = x1  +  d * (x0 - x1);
			y = igamc( a, x );
			lgm = (x0 - x1)/(x1 + x0);
			if( Math.abs(lgm) < dithresh )
				break;
			lgm = (y - y0)/y0;
			if( Math.abs(lgm) < dithresh )
				break;
			if( x <= 0.0 )
				break;
			if( y >= y0 )
				{
				x1 = x;
				yh = y;
				if( dir < 0 )
					{
					dir = 0;
					d = 0.5;
					}
				else if( dir > 1 )
					d = 0.5 * d + 0.5; 
				else
					d = (y0 - yl)/(yh - yl);
				dir += 1;
				}
			else
				{
				x0 = x;
				yl = y;
				if( dir > 0 )
					{
					dir = 0;
					d = 0.5;
					}
				else if( dir < -1 )
					d = 0.5 * d;
				else
					d = (y0 - yl)/(yh - yl);
				dir -= 1;
				}
			}
		if( x == 0.0 ) {
			MipavUtil.displayError( "igami UNDERFLOW ERROR");
			return (0.0);
		}
		return x;
	}
	
	/*							igamc()
	 *
	 *	Complemented incomplete gamma integral
	 *
	 *
	 *
	 * SYNOPSIS:
	 *
	 * double a, x, y, igamc();
	 *
	 * y = igamc( a, x );
	 *
	 * DESCRIPTION:
	 *
	 * The function is defined by
	 *
	 *
	 *  igamc(a,x)   =   1 - igam(a,x)
	 *
	 *                            inf.
	 *                              -
	 *                     1       | |  -t  a-1
	 *               =   -----     |   e   t   dt.
	 *                    -      | |
	 *                   | (a)    -
	 *                             x
	 *
	 *
	 * In this implementation both arguments must be positive.
	 * The integral is evaluated by either a power series or
	 * continued fraction expansion, depending on the relative
	 * values of a and x.
	 *
	 * ACCURACY:
	 *
	 * Tested at random a, x.
	 *                a         x                      Relative error:
	 * arithmetic   domain   domain     # trials      peak         rms
	 *    IEEE     0.5,100   0,100      200000       1.9e-14     1.7e-15
	 *    IEEE     0.01,0.5  0,100      200000       1.4e-13     1.6e-15
	 */
	
	/*
	Cephes Math Library Release 2.0:  April, 1987
	Copyright 1985, 1987 by Stephen L. Moshier
	Direct inquiries to 30 Frost Street, Cambridge, MA 02140
	*/

	public double igamc(double  a, double x )
	{
	double ans, ax, c, yc, r, t, y, z;
	double pk, pkm1, pkm2, qk, qkm1, qkm2;

	if( (x <= 0) || ( a <= 0) ) {
		return (1.0);
	}

	if( (x < 1.0) || (x < a) ) {
		return (1.0 - igam(a,x));
	}

	ax = a * Math.log(x) - x - lgam(a);
	if( ax < -MAXLOG )
		{
		MipavUtil.displayError("igamc UNDERFLOW");
		return (0.0);
		}
	ax = Math.exp(ax);

	/* continued fraction */
	y = 1.0 - a;
	z = x + y + 1.0;
	c = 0.0;
	pkm2 = 1.0;
	qkm2 = x;
	pkm1 = x + 1.0;
	qkm1 = z * x;
	ans = pkm1/qkm1;

	do
		{
		c += 1.0;
		y += 1.0;
		z += 2.0;
		yc = y * c;
		pk = pkm1 * z  -  pkm2 * yc;
		qk = qkm1 * z  -  qkm2 * yc;
		if( qk != 0 )
			{
			r = pk/qk;
			t = Math.abs( (ans - r)/r );
			ans = r;
			}
		else
			t = 1.0;
		pkm2 = pkm1;
		pkm1 = pk;
		qkm2 = qkm1;
		qkm1 = qk;
		if( Math.abs(pk) > big )
			{
			pkm2 *= biginv;
			pkm1 *= biginv;
			qkm2 *= biginv;
			qkm1 *= biginv;
			}
		}
	while( t > MACHEP );
     
	return (ans * ax);
	}

	/*							igam.c
	 *
	 *	Incomplete gamma integral
	 *
	 *
	 *
	 * SYNOPSIS:
	 *
	 * double a, x, y, igam();
	 *
	 * y = igam( a, x );
	 *
	 * DESCRIPTION:
	 *
	 * The function is defined by
	 *
	 *                           x
	 *                            -
	 *                   1       | |  -t  a-1
	 *  igam(a,x)  =   -----     |   e   t   dt.
	 *                  -      | |
	 *                 | (a)    -
	 *                           0
	 *
	 *
	 * In this implementation both arguments must be positive.
	 * The integral is evaluated by either a power series or
	 * continued fraction expansion, depending on the relative
	 * values of a and x.
	 *
	 * ACCURACY:
	 *
	 *                      Relative error:
	 * arithmetic   domain     # trials      peak         rms
	 *    IEEE      0,30       200000       3.6e-14     2.9e-15
	 *    IEEE      0,100      300000       9.9e-14     1.5e-14
	 */
	
	/* left tail of incomplete gamma function:
	 *
	 *          inf.      k
	 *   a  -x   -       x
	 *  x  e     >   ----------
	 *           -     -
	 *          k=0   | (a+k+1)
	 *
	 */

	public double igam(double a, double x)
	{
	double ans, ax, c, r;

	if( (x <= 0) || ( a <= 0) ) {
		return (0.0);
	}

	if( (x > 1.0) && (x > a ) ) {
		return (1.0 - igamc(a,x));
	}

	/* Compute  x**a * exp(-x) / gamma(a)  */
	ax = a * Math.log(x) - x - lgam(a);
	if( ax < -MAXLOG )
		{
		MipavUtil.displayError( "igam UNDERFLOW");
		return (0.0);
		}
	ax = Math.exp(ax);

	/* power series */
	r = a;
	c = 1.0;
	ans = 1.0;

	do
		{
		r += 1.0;
		c *= x/r;
		ans += c;
		}
	while( c/ans > MACHEP );

	return (ans * ax/a);
	}

	/*							ndtri.c
	 *
	 *	Inverse of Normal distribution function
	 *
	 *
	 *
	 * SYNOPSIS:
	 *
	 * double x, y, ndtri();
	 *
	 * x = ndtri( y );
	 *
	 *
	 *
	 * DESCRIPTION:
	 *
	 * Returns the argument, x, for which the area under the
	 * Gaussian probability density function (integrated from
	 * minus infinity to x) is equal to y.
	 *
	 *
	 * For small arguments 0 < y < exp(-2), the program computes
	 * z = sqrt( -2.0 * log(y) );  then the approximation is
	 * x = z - log(z)/z  - (1/z) P(1/z) / Q(1/z).
	 * There are two rational functions P/Q, one for 0 < y < exp(-32)
	 * and the other for y up to exp(-2).  For larger arguments,
	 * w = y - 0.5, and  x/sqrt(2pi) = w + w**3 R(w**2)/S(w**2)).
	 *
	 *
	 * ACCURACY:
	 *
	 *                      Relative error:
	 * arithmetic   domain        # trials      peak         rms
	 *    DEC      0.125, 1         5500       9.5e-17     2.1e-17
	 *    
 *    DEC      6e-39, 0.135     3500       5.7e-17     1.3e-17
 *    IEEE     0.125, 1        20000       7.2e-16     1.3e-16
 *    IEEE     3e-308, 0.135   50000       4.6e-16     9.8e-17
 *
 *
 * ERROR MESSAGES:
 *
 *   message         condition    value returned
 * ndtri domain       x <= 0        -MAXNUM
 * ndtri domain       x >= 1         MAXNUM
 *
 */
	public double ndtri(double y0) {
		double x, y, z, y2, x0, x1;
		int code;

		if( y0 <= 0.0 )
			{
			MipavUtil.displayError( "ndtri DOMAIN error");
			return (-MAXNUM);
			}
		if( y0 >= 1.0 )
			{
			MipavUtil.displayError( "ndtri DOMAIN error");
			return (MAXNUM);
			}
		code = 1;
		y = y0;
		if( y > (1.0 - 0.13533528323661269189) ) /* 0.135... = exp(-2) */
			{
			y = 1.0 - y;
			code = 0;
			}

		if( y > 0.13533528323661269189 )
			{
			y = y - 0.5;
			y2 = y * y;
			x = y + y * (y2 * polevl(y2, P0, 4)/p1evl(y2, Q0, 8));
			x = x * s2pi; 
			return x;
			}

		x = Math.sqrt( -2.0 * Math.log(y) );
		x0 = x - Math.log(x)/x;

		z = 1.0/x;
		if( x < 8.0 ) /* y > exp(-32) = 1.2664165549e-14 */ {
			x1 = z * polevl(z, P1, 8)/p1evl(z, Q1, 8);
		}
		else {
			x1 = z * polevl(z, P2, 8)/p1evl(z, Q2, 8);
		}
		x = x0 - x1;
		if( code != 0 )
			x = -x;
		return x;
	}
	
	/*							polevl.c
	 *							p1evl.c
	 *
	 *	Evaluate polynomial
	 *
	 *
	 *
	 * SYNOPSIS:
	 *
	 * int N;
	 * double x, y, coef[N+1], polevl[];
	 *
	 * y = polevl( x, coef, N );
	 *
	 *
	 *
	 * DESCRIPTION:
	 *
	 * Evaluates polynomial of degree N:
	 *
	 *                     2          N
	 * y  =  C  + C x + C x  +...+ C x
	 *        0    1     2          N
	 *
	 * Coefficients are stored in reverse order:
	 *
	 * coef[0] = C  , ..., coef[N] = C  .
	 *            N                   0
	 *
	 *  The function p1evl() assumes that coef[N] = 1.0 and is
	 * omitted from the array.  Its calling arguments are
	 * otherwise the same as polevl().
	 *
	 *
	 * SPEED:
	 * *
	 * In the interest of speed, there are no checks for out
	 * of bounds arithmetic.  This routine is used by most of
	 * the functions in the library.  Depending on available
	 * equipment features, the user may wish to rewrite the
	 * program in microcode or assembly language.
	 *
	 */
	
	/*
	Cephes Math Library Release 2.1:  December, 1988
	Copyright 1984, 1987, 1988 by Stephen L. Moshier
	Direct inquiries to 30 Frost Street, Cambridge, MA 02140
	*/
	
	public double polevl(double x, double coef[], int N ) {
		double ans;
		int i;
		int coefindex = 0;

		ans = coef[coefindex++];
		i = N;

		do {
			ans = ans * x  +  coef[coefindex++];
			--i;
		} while (i > 0);
		

		return ans;
	}
	
	public double p1evl(double x, double coef[], int N ) {
		double ans;
		int coefindex = 0;
		int i;

		ans = x + coef[coefindex++];
		i = N-1;

		do {
			ans = ans * x  + coef[coefindex++];
			--i;
		} while(i > 0);

		return ans;
	}
	
	/*							ellie.c
	 *
	 *	Incomplete elliptic integral of the second kind
	 *
	 *
	 *
	 * SYNOPSIS:
	 *
	 * double phi, m, y, ellie();
	 *
	 * y = ellie( phi, m );
	 *
	 *
	 *
	 * DESCRIPTION:
	 *
	 * Approximates the integral
	 *
	 *
	 *                phi
	 *                 -
	 *                | |
	 *                |                   2
	 * E(phi_\m)  =    |    sqrt( 1 - m sin t ) dt
	 *                |
	 *              | |    
	 *               -
	 *                0
	 *
	 * of amplitude phi and modulus m, using the arithmetic -
	 * geometric mean algorithm.
	 *
	 *
	 *
	 * ACCURACY:
	 *
	 * Tested at random arguments with phi in [-10, 10] and m in
	 * [0, 1].
	 *                      Relative error:
	 * arithmetic   domain     # trials      peak         rms
	 *    DEC        0,2         2000       1.9e-16     3.4e-17
	 *    IEEE     -10,10      150000       3.3e-15     1.4e-16
	 *
	 *
	 */
	

	/*
	Cephes Math Library Release 2.0:  April, 1987
	Copyright 1984, 1987, 1993 by Stephen L. Moshier
	Direct inquiries to 30 Frost Street, Cambridge, MA 02140
	*/

	/*	Incomplete elliptic integral of second kind	*/

	public double ellie(double phi, double m )
	{
	double a, b, c, e, temp;
	double lphi, t, E;
	int d, mod, npio2, sign;

	if( m == 0.0 ) {
		return phi;
	}
	lphi = phi;
	npio2 =(int)Math.floor( lphi/PIO2 );
	if(( npio2 & 1 ) != 0)
		npio2 += 1;
	lphi = lphi - npio2 * PIO2;
	if( lphi < 0.0 )
		{
		lphi = -lphi;
		sign = -1;
		}
	else
		{
		sign = 1;
		}
	// Changed to reflect change in ellpe
	a = 1.0 - m;
	E = ellpe(1.0 - a);
	if( a == 0.0 )
		{
		temp = Math.sin( lphi );
		if( sign < 0 )
			temp = -temp;
		temp += npio2 * E;
		return temp;
		}
	t = Math.tan( lphi );
	b = Math.sqrt(a);
	/* Thanks to Brian Fitzgerald <fitzgb@mml0.meche.rpi.edu>
	   for pointing out an instability near odd multiples of pi/2.  */
	if( Math.abs(t) > 10.0 )
		{
		/* Transform the amplitude */
		e = 1.0/(b*t);
		/* ... but avoid multiple recursions.  */
		if( Math.abs(e) < 10.0 )
			{
			e = Math.atan(e);
			temp = E + m * Math.sin( lphi ) * Math.sin( e ) - ellie(e,m);;
			if( sign < 0 )
				temp = -temp;
			temp += npio2 * E;
			return temp;
			}
		}
	c = Math.sqrt(m);
	a = 1.0;
	d = 1;
	e = 0.0;
	mod = 0;

	while( Math.abs(c/a) > MACHEP )
		{
		temp = b/a;
		lphi = lphi + Math.atan(t*temp) + mod * Math.PI;
		mod = (int)((lphi + PIO2)/Math.PI);
		t = t * ( 1.0 + temp )/( 1.0 - temp * t * t );
		c = ( a - b )/2.0;
		temp = Math.sqrt( a * b );
		a = ( a + b )/2.0;
		b = temp;
		d += d;
		e += c * Math.sin(lphi);
		}

	// Changed to reflect change in ellpk
	temp = E / ellpk(m);
	temp *= (Math.atan(t) + mod * Math.PI)/(d * a);
	temp += e;

	if( sign < 0 )
		temp = -temp;
	temp += npio2 * E;
	return temp;
	}
	
	/*							ellpe.c
	 *
	 *	Complete elliptic integral of the second kind
	 *
	 *
	 *
	 * SYNOPSIS:
	 *
	 * double m1, y, ellpe();
	 *
	 * y = ellpe( m1 );
	 *
	 *
	 *
	 * DESCRIPTION:
	 *
	 * Approximates the integral
	 *
	 *
	 *            pi/2
	 *             -
	 *            | |                 2
	 * E(m)  =    |    sqrt( 1 - m sin t ) dt
	 *          | |    
	 *           -
	 *            0
	 *
	 * Where m = 1 - m1, using the approximation
	 *
	 *      P(x)  -  x log x Q(x).
	 *
	 * Though there are no singularities, the argument m1 is used
	 * rather than m for compatibility with ellpk().
	 *
	 * E(1) = 1; E(0) = pi/2.
	 *
	 *
	 * ACCURACY:
	 *
	 *                      Relative error:
	 * arithmetic   domain     # trials      peak         rms
	 *    DEC        0, 1       13000       3.1e-17     9.4e-18
	 *    IEEE       0, 1       10000       2.1e-16     7.3e-17
	 *
	 *
	 * ERROR MESSAGES:
	 *
	 *   message         condition      value returned
	 * ellpe domain      x<0, x>1            0.0
	 *
	 */
	
	/*							ellpe.c		*/

	/* Elliptic integral of second kind */

	/*
	Cephes Math Library, Release 2.1:  February, 1989
	Copyright 1984, 1987, 1989 by Stephen L. Moshier
	Direct inquiries to 30 Frost Street, Cambridge, MA 02140
	*/
	
	public double ellpe(double x)
	{
    // hcephes_ellpe adds line by danilo x = 1.0 - x
	x = 1.0 - x;
	if( (x <= 0.0) || (x > 1.0) )
		{
		if( x == 0.0 ) {
			return (1.0);
		}
		MipavUtil.displayError("Domain error in ellpe()");
		return (0.0);
		}
	return (polevl(x,PELLPE,10) - Math.log(x) * (x * polevl(x,QELLPE,9)));
	}
	
	/*							ellpk.c
	 *
	 *	Complete elliptic integral of the first kind
	 *
	 *
	 *
	 * SYNOPSIS:
	 *
	 * double m1, y, ellpk();
	 *
	 * y = ellpk( m1 );
	 *
	 *
	 *
	 * DESCRIPTION:
	 *
	 * Approximates the integral
	 *
	 *
	 *
	 *            pi/2
	 *             -
	 *            | |
	 *            |           dt
	 * K(m)  =    |    ------------------
	 *            |                   2
	 *          | |    sqrt( 1 - m sin t )
	 *           -
	 *            0
	 *
	 * where m = 1 - m1, using the approximation
	 *
	 *     P(x)  -  log x Q(x).
	 *
	 * The argument m1 is used rather than m so that the logarithmic
	 * singularity at m = 1 will be shifted to the origin; this
	 * preserves maximum accuracy.
	 *
	 * K(0) = pi/2.
	 *
	 * ACCURACY:
	 *
	 *                      Relative error:
	 * arithmetic   domain     # trials      peak         rms
	 *    DEC        0,1        16000       3.5e-17     1.1e-17
	 *    IEEE       0,1        30000       2.5e-16     6.8e-17
	 *
	 * ERROR MESSAGES:
	 *
	 *   message         condition      value returned
	 * ellpk domain       x<0, x>1           0.0
	 *
	 */
	
	/*							ellpk.c */


	/*
	Cephes Math Library, Release 2.0:  April, 1987
	Copyright 1984, 1987 by Stephen L. Moshier
	Direct inquiries to 30 Frost Street, Cambridge, MA 02140
	*/

	public double ellpk(double x)
	{
		 // hcephes_ellpk adds line by danilo x = 1.0 - x
		x = 1.0 - x;

	if( (x < 0.0) || (x > 1.0) )
		{
		MipavUtil.displayError("Domain error in ellpk()");
		return (0.0);
		}

	if( x > MACHEP )
		{
		return (polevl(x,PELLPK,10) - Math.log(x) * polevl(x,QELLPK,10));
		}
	else
		{
		if( x == 0.0 )
			{
			MipavUtil.displayError("Singularity in ellpk()");
			return (MAXNUM);
			}
		else
			{
			return (C1 - 0.5 * Math.log(x));
			}
		}
	}
	
	/*							zeta.c
	 *
	 *	Riemann zeta function of two arguments
	 *
	 *
	 *
	 * SYNOPSIS:
	 *
	 * double x, q, y, zeta();
	 *
	 * y = zeta( x, q );
	 *
	 *
	 *
	 * DESCRIPTION:
	 *
	 *
	 *
	 *                 inf.
	 *                  -        -x
	 *   zeta(x,q)  =   >   (k+q)  
	 *                  -
	 *                 k=0
	 *
	 * where x > 1 and q is not a negative integer or zero.
	 * The Euler-Maclaurin summation formula is used to obtain
	 * the expansion
	 *
	 *                n         
	 *                -       -x
	 * zeta(x,q)  =   >  (k+q)  
	 *                -         
	 *               k=1        
	 *
	 *           1-x                 inf.  B   x(x+1)...(x+2j)
	 *      (n+q)           1         -     2j
	 *  +  ---------  -  -------  +   >    --------------------
	 *        x-1              x      -                   x+2j+1
	 *                   2(n+q)      j=1       (2j)! (n+q)
	 *
	 * where the B2j are Bernoulli numbers.  Note that (see zetac.c)
	 * zeta(x,1) = zetac(x) + 1.
	 *
	 *
	 *
	 * ACCURACY:
	 *
	 *
	 *
	 * REFERENCE:
	 *
	 * Gradshteyn, I. S., and I. M. Ryzhik, Tables of Integrals,
	 * Series, and Products, p. 1073; Academic Press, 1980.
	 *
	 */
	
	/*
	Cephes Math Library Release 2.0:  April, 1987
	Copyright 1984, 1987 by Stephen L. Moshier
	Direct inquiries to 30 Frost Street, Cambridge, MA 02140
	*/
	
	public double zeta(double x,double q) {
	int i;
	double a, b, k, s, t, w;

	if( x == 1.0 ) {
		return (MAXNUM);
	}
		

	if( x < 1.0 )
		{
		MipavUtil.displayError("Domain error in zeta");
		return (0.0);
		}

	if( q <= 0.0 )
		{
		if(q == Math.floor(q))
			{
			MipavUtil.displayError("Singularity error in zeta");
			return (MAXNUM);
			}
		if( x != Math.floor(x) )
			/* because q^-x not defined */
			MipavUtil.displayError("Domain error in zeta");
		    return (0.0);
		}

	/* Euler-Maclaurin summation formula */
	/*
	if( x < 25.0 )
	*/
	{
	/* Permit negative q but continue sum until n+q > +9 .
	 * This case should be handled by a reflection formula.
	 * If q<0 and x is an integer, there is a relation to
	 * the polygamma function.
	 */
	s = Math.pow( q, -x );
	a = q;
	i = 0;
	b = 0.0;
	while( (i < 9) || (a <= 9.0) )
		{
		i += 1;
		a += 1.0;
		b = Math.pow( a, -x );
		s += b;
		if( Math.abs(b/s) < MACHEP ) {
			return s;
		}
		}

	w = a;
	s += b*w/(x-1.0);
	s -= 0.5 * b;
	a = 1.0;
	k = 0.0;
	for( i=0; i<12; i++ )
		{
		a *= x + k;
		b /= w;
		t = a*b/AZETA[i];
		s = s + t;
		t = Math.abs(t/s);
		if( t < MACHEP ) {
		    return s;
		}
		k += 1.0;
		a *= x + k;
		b /= w;
		k += 1.0;
		}
	    return s;
	}



	/* Basic sum of inverse powers */
	/*
	pseres:

	s = pow( q, -x );
	a = q;
	do
		{
		a += 2.0;
		b = pow( a, -x );
		s += b;
		}
	while( b/s > MACHEP );

	b = pow( 2.0, -x );
	s = (s + b)/(1.0-b);
	return(s);
	*/
	}
	
	/* Gamma function computed by Stirling's formula.
	 * The polynomial STIR is valid for 33 <= x <= 172.
	 */
	public double stirf(double x) {
	double y, w, v;

	w = 1.0/x;
	w = 1.0 + w * polevl(w, STIR, 4);
	y = Math.exp(x);
	if( x > MAXSTIR )
		{ /* Avoid overflow in pow() */
		v = Math.pow( x, 0.5 * x - 0.25 );
		y = v * (v / y);
		}
	else
		{
		y = Math.pow( x, x - 0.5 ) / y;
		}
	y = SQTPI * y * w;
	return y;
	}
	
	public double true_gamma(double x) {
	double p, q, z;
	int i;

	int sgngam = 1;
	q = Math.abs(x);

	if( q > 33.0 )
		{
		if( x < 0.0 )
			{
			p = Math.floor(q);
			if( p == q ) {
				MipavUtil.displayError("OVERFLOW in true_gamma");
				return (sgngam * MAXNUM);
			}    
			i = (int)p;
			if( (i & 1) == 0 )
				sgngam = -1;
			z = q - p;
			if( z > 0.5 )
				{
				p += 1.0;
				z = q - p;
				}
			z = q * Math.sin(Math.PI * z );
			if( z == 0.0 )
				{
				MipavUtil.displayError("OVERFLOW in true_gamma");
				return (sgngam * MAXNUM);
				}
			z = Math.abs(z);
			z = Math.PI/(z * stirf(q) );
			}
		else
			{
			z = stirf(x);
			}
		return (sgngam * z);
		}

	z = 1.0;
	while( x >= 3.0 )
		{
		x -= 1.0;
		z *= x;
		}

	while( x < 0.0 )
		{
		if( x > -1.E-9 ) {
			if( x == 0.0 )
			{
			MipavUtil.displayError("Singularity in true_gamma");
			return (MAXNUM);
			}
		else {
			return ( z/((1.0 + 0.5772156649015329 * x) * x) );
		}
		}
		z /= x;
		x += 1.0;
		}

	while( x < 2.0 )
		{
		if( x < 1.e-9 ) {
			if( x == 0.0 )
			{
			MipavUtil.displayError("Singularity in true_gamma");
			return (MAXNUM);
			}
		else {
			return ( z/((1.0 + 0.5772156649015329 * x) * x) );
		}
		}
		z /= x;
		x += 1.0;
		}

	if( (x == 2.0) || (x == 3.0) ) {
		return z;
	}

	x -= 2.0;
	return (z * polevl( x, PGAMMA, 6 ) / polevl( x, QGAMMA, 7 ));
	    
	}
	
	/*							zetac.c
	 *
	 *	Riemann zeta function
	 *
	 *
	 *
	 * SYNOPSIS:
	 *
	 * double x, y, zetac();
	 *
	 * y = zetac( x );
	 *
	 *
	 *
	 * DESCRIPTION:
	 *
	 *
	 *
	 *                inf.
	 *                 -    -x
	 *   zetac(x)  =   >   k   ,   x > 1,
	 *                 -
	 *                k=2
	 *
	 * is related to the Riemann zeta function by
	 *
	 *	Riemann zeta(x) = zetac(x) + 1.
	 *
	 * Extension of the function definition for x < 1 is implemented.
	 * Zero is returned for x > log2(MAXNUM).
	 *
	 * An overflow error may occur for large negative x, due to the
	 * gamma function in the reflection formula.
	 *
	 * ACCURACY:
	 *
	 * Tabulated values have full machine accuracy.
	 *
	 *                      Relative error:
	 * arithmetic   domain     # trials      peak         rms
	 *    IEEE      1,50        10000       9.8e-16	    1.3e-16
	 *    DEC       1,50         2000       1.1e-16     1.9e-17
	 *
	 *
	 */
	
	/*
	Cephes Math Library Release 2.1:  January, 1989
	Copyright 1984, 1987, 1989 by Stephen L. Moshier
	Direct inquiries to 30 Frost Street, Cambridge, MA 02140
	*/
	
	/* Riemann zeta(x) - 1
	 * for integer arguments between 0 and 30.
	 */
	
	public double zetac(double x) {
	int i;
	double a, b, s, w;

	if( x < 0.0 )
		{
		if( x < -30.8148 )
			{
			MipavUtil.displayError("OVERFLOW error in zetac");
			return (0.0);
			}
		s = 1.0 - x;
		w = zetac( s );
		b = Math.sin(0.5*Math.PI*x) * Math.pow(2.0*Math.PI, x) * true_gamma(s) * (1.0 + w) / Math.PI;
		return (b - 1.0);
		}

	if( x >= MAXL2 ) {
		/* because first term is 2**-x */
		return (0.0);
	}

	/* Tabulated values for integer argument */
	w = Math.floor(x);
	if( w == x )
		{
		i = (int)x;
		if( i < 31 )
			{
	        return azetac[i];
			
			}
		}


	if( x < 1.0 )
		{
		w = 1.0 - x;
		a = polevl(x, RZETAC, 5) / ( w * p1evl(x, SZETAC, 5));
		return a;
		}

	if( x == 1.0 )
		{
		MipavUtil.displayError("SINGULARITY IN zetac");
		return (Double.POSITIVE_INFINITY);
		}

	if( x <= 10.0 )
		{
		b = Math.pow( 2.0, x ) * (x - 1.0);
		w = 1.0/x;
		s = (x * polevl(w, PZETAC, 8)) / (b * p1evl(w, QZETAC, 8));
		return s;
		}

	if( x <= 50.0 )
		{
		b = Math.pow( 2.0, -x );
		w = polevl(x, AZETAC, 10)/ p1evl(x, BZETAC, 10);
		w = Math.exp(w) + b;
		return w;
		}


	/* Basic sum of inverse powers */


	s = 0.0;
	a = 1.0;
	do
		{
		a += 2.0;
		b = Math.pow( a, -x );
		s += b;
		}
	while( b/s > MACHEP );

	b = Math.pow( 2.0, -x );
	s = (s + b)/(1.0-b);
	return s;
	}

	/*							struve.c
	 *
	 *      Struve function
	 *
	 *
	 *
	 * SYNOPSIS:
	 *
	 * double v, x, y, struve();
	 *
	 * y = struve( v, x );
	 *
	 *
	 *
	 * DESCRIPTION:
	 *
	 * Computes the Struve function Hv(x) of order v, argument x.
	 * Negative x is rejected unless v is an integer.
	 *
	 * This module also contains the hypergeometric functions 1F2
	 * and 3F0 and a routine for the Bessel function Yv(x) with
	 * noninteger v.
	 *
	 *
	 *
	 * ACCURACY:
	 *
	 * Not accurately characterized, but spot checked against tables.
	 *
	 */

	/*
	Cephes Math Library Release 2.1:  January, 1989
	Copyright 1984, 1987, 1989 by Stephen L. Moshier
	Direct inquiries to 30 Frost Street, Cambridge, MA 02140
	*/
	
	/* Crossover points between ascending series and asymptotic series
	 * for Struve function
	 *
	 *	 v	 x
	 * 
	 *	 0	19.2
	 *	 1	18.95
	 *	 2	19.15
	 *	 3	19.3
	 *	 5	19.7
	 *	10	21.35
	 *	20	26.35
	 *	30	32.31
	 *	40	40.0
	 */
	
	public double onef2(double a, double b, double c, double x, double err[] ) {
	double n, a0, sum, t;
	double an, bn, cn, max, z;

	an = a;
	bn = b;
	cn = c;
	a0 = 1.0;
	sum = 1.0;
	n = 1.0;
	t = 1.0;
	max = 0.0;

	do
		{
		if( an == 0 ) {
			err[0] = Math.abs( MACHEP*max /sum );

			if (DEBUG) {
				System.out.println(" onef2 cancellation error = " + err[0] );
			}
			
			if (DEBUG) {
			    System.out.println("onef2("+a+ " "+b+ " " + c + " "+ x+")");
			    System.out.println("n = " + n + " sum = " + sum);
			}
			return(sum);
		}
		if(( bn == 0 ) || (cn == 0) || (a0 > 1.0E34) || (n > 200)) {
			if (DEBUG) {
			    System.out.println("onef2 does not converge");
			}
			err[0] = 1.0e38;
			if (DEBUG) {
			    System.out.println("onef2("+a+ ", "+b+ ", " + c + ", "+ x+")");
			    System.out.println("n = " + n + " sum = " + sum);
			}
			return(sum);
		}
		
		a0 *= (an * x) / (bn * cn * n);
		sum += a0;
		an += 1.0;
		bn += 1.0;
		cn += 1.0;
		n += 1.0;
		z = Math.abs( a0 );
		if( z > max )
			max = z;
		if( sum != 0 )
			t = Math.abs( a0 / sum );
		else
			t = z;
		}
	while( t > stop );
	err[0] = Math.abs( MACHEP*max /sum );

	if (DEBUG) {
		System.out.println(" onef2 cancellation error = " + err[0] );
	}
	
	if (DEBUG) {
	    System.out.println("onef2("+a+ ", "+b+ ", " + c + ", "+ x+")");
	    System.out.println("n = " + n + " sum = " + sum);
	}
	return(sum);
	}

	public double threef0(double a, double b, double c, double x, double err[]) {
	double n, a0, sum, t, conv, conv1;
	double an, bn, cn, max, z;

	an = a;
	bn = b;
	cn = c;
	a0 = 1.0;
	sum = 1.0;
	n = 1.0;
	t = 1.0;
	max = 0.0;
	conv = 1.0e38;
	conv1 = conv;

	do
		{
		if(( an == 0.0 ) || (bn == 0.0) || (cn == 0.0)) {
			t = Math.abs( MACHEP*max/sum );
			if (DEBUG) {
				System.out.println("threef0 cancellation error = " + t );
			}

			max = Math.abs( conv/sum );
			if( max > t ) {
				t = max;
			}
			if (DEBUG) {
				System.out.println("threef0 convergence = " + max );
			}
			
			if (DEBUG) {
				System.out.println("threef0("+a+ ", "+b+ ", " + c + ", "+ x+")");
			    System.out.println("n = " + n + " sum = " + sum);
			}

			err[0] = t;
			return(sum);
		}
			
		if( (a0 > 1.0e34) || (n > 200) ) {
			if (DEBUG) {
			    System.out.println("threef0 does not converge");
			}
			t = 1.0e38;
			
			if (DEBUG) {
				System.out.println("threef0("+a+ ", "+b+ ", " + c + ", "+ x+")");
			    System.out.println("n = " + n + " sum = " + sum);
			}

			err[0] = t;
			return(sum);
		}
		a0 *= (an * bn * cn * x) / n;
		an += 1.0;
		bn += 1.0;
		cn += 1.0;
		n += 1.0;
		z = Math.abs( a0 );
		if( z > max )
			max = z;
		if( z >= conv )
			{
			if( (z < max) && (z > conv1) ) {
				t = Math.abs( MACHEP*max/sum );
				if (DEBUG) {
					System.out.println("threef0 cancellation error = " + t );
				}

				max = Math.abs( conv/sum );
				if( max > t ) {
					t = max;
				}
				if (DEBUG) {
					System.out.println("threef0 convergence = " + max );
				}
				
				if (DEBUG) {
					System.out.println("threef0("+a+ ", "+b+ ", " + c + ", "+ x+")");
				    System.out.println("n = " + n + " sum = " + sum);
				}

				err[0] = t;
				return(sum);	
			}
			}
				
		conv1 = conv;
		conv = z;
		sum += a0;
		if( sum != 0 )
			t = Math.abs( a0 / sum );
		else
			t = z;
		}
	while( t > stop );
	t = Math.abs( MACHEP*max/sum );
	if (DEBUG) {
		System.out.println("threef0 cancellation error = " + t );
	}

	max = Math.abs( conv/sum );
	if( max > t ) {
		t = max;
	}
	if (DEBUG) {
		System.out.println("threef0 convergence = " + max );
	}
	
	if (DEBUG) {
		System.out.println("threef0("+a+ ", "+b+ ", " + c + ", "+ x+")");
	    System.out.println("n = " + n + " sum = " + sum);
	}

	err[0] = t;
	return(sum);	
	}

	public double struve(double v, double x ) {
	double y, ya, f, g, h, t;
	double onef2err[] = new double[1];
	double threef0err[] = new double[1];
	double[] cyr = new double[1];
	double[] cyi = new double[1];
	int[] nz = new int[1];
	int[] errorFlag = new int[1];
	
	if ((v == 0.0) && (x == 0.0)) {
		return 0.0;
	}

	f = Math.floor(v);
	if( (v < 0) && ( v-f == 0.5 ) )
		{
		Bessel besj = new Bessel(Bessel.BESSEL_J,-v,0.0,x,Bessel.UNSCALED_FUNCTION,1,cyr,cyi,nz,errorFlag);
		besj.run();
		y = cyr[0];
		f = 1.0 - f;
		g =  2.0 * Math.floor(f/2.0);
		if( g != f )
			y = -y;
		return(y);
		}
	t = 0.25*x*x;
	f = Math.abs(x);
	g = 1.5 * Math.abs(v);
	if( (f > 30.0) && (f > g) )
		{
		onef2err[0] = 1.0e38;
		y = 0.0;
		}
	else
		{
		y = onef2( 1.0, 1.5, 1.5+v, -t, onef2err );
		}

	if( (f < 18.0) || (x < 0.0) )
		{
		threef0err[0] = 1.0e38;
		ya = 0.0;
		}
	else
		{
		ya = threef0( 1.0, 0.5, 0.5-v, -1.0/t, threef0err );
		}

	f = Math.sqrt( Math.PI );
	h = Math.pow( 0.5*x, v-1.0 );

	if( onef2err[0] <= threef0err[0] )
		{
		g = true_gamma( v + 1.5 );
		y = y * h * t / ( 0.5 * f * g );
		return(y);
		}
	else
		{
		g = true_gamma( v + 0.5 );
		ya = ya * h / ( f * g );
		Bessel besy = new Bessel(Bessel.BESSEL_Y,v,0.0,x,Bessel.UNSCALED_FUNCTION,1,cyr,cyi,nz,errorFlag);
		besy.run();
		ya = ya + cyr[0];
		return(ya);
		}
	}
	
	/* Logarithm of gamma function */

	public double lgam(double x) {
	double p, q, w, z;
	int i;

	sgngam = 1;

	if( x < -34.0 )
		{
		q = -x;
		w = lgam(q); /* note this modifies sgngam! */
		p = Math.floor(q);
		if( p == q ) {
			MipavUtil.displayError("OVERFLOW in lgam");
			return( sgngam * MAXNUM );
		}
		i = (int)p;
		if( (i & 1) == 0 )
			sgngam = -1;
		else
			sgngam = 1;
		z = q - p;
		if( z > 0.5 )
			{
			p += 1.0;
			z = p - q;
			}
		z = q * Math.sin( Math.PI * z );
		if( z == 0.0 ) {
			MipavUtil.displayError("OVERFLOW in lgam");
			return( sgngam * MAXNUM );	
		}
		z = LOGPI - Math.log( z ) - w;
		return( z );
		}

	if( x < 13.0 )
		{
		z = 1.0;
		while( x >= 3.0 )
			{
			x -= 1.0;
			z *= x;
			}
		while( x < 2.0 )
			{
			if( x == 0.0 ) {
				MipavUtil.displayError("OVERFLOW in lgam");
				return( sgngam * MAXNUM );		
			}
			z /= x;
			x += 1.0;
			}
		if( z < 0.0 )
			{
			sgngam = -1;
			z = -z;
			}
		else
			sgngam = 1;
		if( x == 2.0 )
			return( Math.log(z) );
		x -= 2.0;
		p = x * polevl( x, BLGAM, 5 ) / p1evl( x, CLGAM, 6);
		return( Math.log(z) + p );
		}

	if( x > MAXLGM )
		{
		MipavUtil.displayError("OVERFLOW in lgam");
		return( sgngam * MAXNUM );
		}

	q = ( x - 0.5 ) * Math.log(x) - x + LS2PI;
	if( x > 1.0e8 )
		return( q );

	p = 1.0/(x*x);
	if( x >= 1000.0 )
		q += ((   7.9365079365079365079365e-4 * p
			- 2.7777777777777777777778e-3) *p
			+ 0.0833333333333333333333) / x;
	else
		q += polevl( p, ALGAM, 4 ) / x;
	return( q );
	}


	
	/*							beta.c
	 *
	 *	Beta function
	 *
	 *
	 *
	 * SYNOPSIS:
	 *
	 * double a, b, y, beta();
	 *
	 * y = beta( a, b );
	 *
	 *
	 *
	 * DESCRIPTION:
	 *
	 *                   -     -
	 *                  | (a) | (b)
	 * beta( a, b )  =  -----------.
	 *                     -
	 *                    | (a+b)
	 *
	 * For large arguments the logarithm of the function is
	 * evaluated using lgam(), then exponentiated.
	 *
	 *
	 *
	 * ACCURACY:
	 *
	 *                      Relative error:
	 * arithmetic   domain     # trials      peak         rms
	 *    DEC        0,30        1700       7.7e-15     1.5e-15
	 *    IEEE       0,30       30000       8.1e-14     1.1e-14
	 *
	 * ERROR MESSAGES:
	 *
	 *   message         condition          value returned
	 * beta overflow    log(beta) > MAXLOG       0.0
	 *                  a or b <0 integer        0.0
	 *
	 */
	
	/*
	Cephes Math Library Release 2.0:  April, 1987
	Copyright 1984, 1987 by Stephen L. Moshier
	Direct inquiries to 30 Frost Street, Cambridge, MA 02140
	*/
	
	public double beta(double a, double b ) {
	double y;
	int sign;

	sign = 1;

	if( a <= 0.0 )
		{
		if( a == Math.floor(a) ) {
			MipavUtil.displayError("OVERFLOW in beta");
			return (sign * MAXNUM);
		}
		
		}
	if( b <= 0.0 )
		{
		if( b == Math.floor(b) ) {
			MipavUtil.displayError("OVERFLOW in beta");
			return (sign * MAXNUM);
		}
		}


	y = a + b;
	if( Math.abs(y) > MAXGAM )
		{
		y = lgam(y);
		sign *= sgngam; /* keep track of the sign */
		y = lgam(b) - y;
		sign *= sgngam;
		y = lgam(a) + y;
		sign *= sgngam;
		if( y > MAXLOG )
			{
		    MipavUtil.displayError("OVERFLOW in beta");
			return( sign * MAXNUM );
			}
		return( sign * Math.exp(y) );
		}

	y = true_gamma(y);
	if( y == 0.0 ) {
		MipavUtil.displayError("OVERFLOW in beta");
		return( sign * MAXNUM );	
	}

	if( a > b )
		{
		y = true_gamma(a)/y;
		y *= true_gamma(b);
		}
	else
		{
		y = true_gamma(b)/y;
		y *= true_gamma(a);
		}

	return(y);
	}

	/* Natural log of |beta|.  Return the sign of beta in sgngam.  */

	public double lbeta(double a, double b ) {
	double y;
	int sign;

	sign = 1;

	if( a <= 0.0 )
		{
		if( a == Math.floor(a) ) {
			MipavUtil.displayError("OVERFLOW in lbeta");
			return( sign * MAXNUM );
		}
		}
	if( b <= 0.0 )
		{
		if( b == Math.floor(b) ) {
			MipavUtil.displayError("OVERFLOW in lbeta");
			return( sign * MAXNUM );	
		}
		}


	y = a + b;
	if( Math.abs(y) > MAXGAM )
		{
		y = lgam(y);
		sign *= sgngam; /* keep track of the sign */
		y = lgam(b) - y;
		sign *= sgngam;
		y = lgam(a) + y;
		sign *= sgngam;
		sgngam = sign;
		return( y );
		}

	y = true_gamma(y);
	if( y == 0.0 )
		{
		MipavUtil.displayError("OVERFLOW in lbeta");
		return( sign * MAXNUM );
		}

	if( a > b )
		{
		y = true_gamma(a)/y;
		y *= true_gamma(b);
		}
	else
		{
		y = true_gamma(b)/y;
		y *= true_gamma(a);
		}

	if( y < 0 )
	  {
	    sgngam = -1;
	    y = -y;
	  }
	else
	  sgngam = 1;

	return( Math.log(y) );
	}

	/*							chbevl.c
	 *
	 *	Evaluate Chebyshev series
	 *
	 *
	 *
	 * SYNOPSIS:
	 *
	 * int N;
	 * double x, y, coef[N], chebevl();
	 *
	 * y = chbevl( x, coef, N );
	 *
	 *
	 *
	 * DESCRIPTION:
	 *
	 * Evaluates the series
	 *
	 *        N-1
	 *         - '
	 *  y  =   >   coef[i] T (x/2)
	 *         -            i
	 *        i=0
	 *
	 * of Chebyshev polynomials Ti at argument x/2.
	 *
	 * Coefficients are stored in reverse order, i.e. the zero
	 * order term is last in the array.  Note N is the number of
	 * coefficients, not the order.
	 *
	 * If coefficients are for the interval a to b, x must
	 * have been transformed to x -> 2(2x - b - a)/(b-a) before
	 * entering the routine.  This maps x from (a, b) to (-1, 1),
	 * over which the Chebyshev polynomials are defined.
	 *
	 * If the coefficients are for the inverted interval, in
	 * which (a, b) is mapped to (1/b, 1/a), the transformation
	 * required is x -> 2(2ab/x - b - a)/(b-a).  If b is infinity,
	 * this becomes x -> 4a/x - 1.
	 *
	 *
	 *
	 * SPEED:
	 *
	 * Taking advantage of the recurrence properties of the
	 * Chebyshev polynomials, the routine requires one more
	 * addition per loop than evaluating a nested polynomial of
	 * the same degree.
	 *
	 */
	
	/*
	Cephes Math Library Release 2.0:  April, 1987
	Copyright 1985, 1987 by Stephen L. Moshier
	Direct inquiries to 30 Frost Street, Cambridge, MA 02140
	*/
	
	public double chbevl(double x, double array[], int n ) {
	double b0, b1, b2, p[];
	int index = 0;
	int i;

	p = array;
	b0 = p[index++];
	b1 = 0.0;
	i = n - 1;

	do
		{
		b2 = b1;
		b1 = b0;
		b0 = x * b1  -  b2  + p[index++];
		}
	while( --i > 0);

	return( 0.5*(b0-b2) );
	}
	
	/*							dawsn.c
	 *
	 *	Dawson's Integral
	 *
	 *
	 *
	 * SYNOPSIS:
	 *
	 * double x, y, dawsn();
	 *
	 * y = dawsn( x );
	 *
	 *
	 *
	 * DESCRIPTION:
	 *
	 * Approximates the integral
	 *
	 *                             x
	 *                             -
	 *                      2     | |        2
	 *  dawsn(x)  =  exp( -x  )   |    exp( t  ) dt
	 *                          | |
	 *                           -
	 *                           0
	 *
	 * Three different rational approximations are employed, for
	 * the intervals 0 to 3.25; 3.25 to 6.25; and 6.25 up.
	 *
	 *
	 * ACCURACY:
	 *
	 *                      Relative error:
	 * arithmetic   domain     # trials      peak         rms
	 *    IEEE      0,10        10000       6.9e-16     1.0e-16
	 *    DEC       0,10         6000       7.4e-17     1.4e-17
	 *
	 *
	 */
	
	/*
	Cephes Math Library Release 2.1:  January, 1989
	Copyright 1984, 1987, 1989 by Stephen L. Moshier
	Direct inquiries to 30 Frost Street, Cambridge, MA 02140
	*/

	public double dawsn(double xx ) {
	double x, y;
	int sign;


	sign = 1;
	if( xx < 0.0 )
		{
		sign = -1;
		xx = -xx;
		}

	if( xx < 3.25 )
	{
	x = xx*xx;
	y = xx * polevl( x, AN, 9 )/polevl( x, AD, 10 );
	return( sign * y );
	}


	x = 1.0/(xx*xx);

	if( xx < 6.25 )
		{
		y = 1.0/xx + x * polevl( x, BN, 10) / (p1evl( x, BD, 10) * xx);
		return( sign * 0.5 * y );
		}


	if( xx > 1.0e9 )
		return( (sign * 0.5)/xx );

	/* 6.25 to infinity */
	y = 1.0/xx + x * polevl( x, CN, 4) / (p1evl( x, CD, 5) * xx);
	return( sign * 0.5 * y );
	}
	
	/*							ellik.c
	 *
	 *	Incomplete elliptic integral of the first kind
	 *
	 *
	 *
	 * SYNOPSIS:
	 *
	 * double phi, m, y, ellik();
	 *
	 * y = ellik( phi, m );
	 *
	 *
	 *
	 * DESCRIPTION:
	 *
	 * Approximates the integral
	 *
	 *
	 *
	 *                phi
	 *                 -
	 *                | |
	 *                |           dt
	 * F(phi_\m)  =    |    ------------------
	 *                |                   2
	 *              | |    sqrt( 1 - m sin t )
	 *               -
	 *                0
	 *
	 * of amplitude phi and modulus m, using the arithmetic -
	 * geometric mean algorithm.
	 *
	 *
	 *
	 *
	 * ACCURACY:
	 *
	 * Tested at random points with m in [0, 1] and phi as indicated.
	 *
	 *                      Relative error:
	 * arithmetic   domain     # trials      peak         rms
	 *    IEEE     -10,10       200000      7.4e-16     1.0e-16
	 *
	 *
	 */
	
	/*
	Cephes Math Library Release 2.0:  April, 1987
	Copyright 1984, 1987 by Stephen L. Moshier
	Direct inquiries to 30 Frost Street, Cambridge, MA 02140
	*/
	
	public double ellik(double phi, double m ) {
	double a, b, c, e, temp, t, K;
	int d, mod, sign, npio2;

	if( m == 0.0 )
		return( phi );
	a = 1.0 - m;
	if( a == 0.0 )
		{
		if( Math.abs(phi) >= PIO2 )
			{
			MipavUtil.displayError("SINGULARITY in ellik");
			return( MAXNUM );
			}
		return(  Math.log(  Math.tan( (PIO2 + phi)/2.0 )  )   );
		}
	npio2 = (int)Math.floor( phi/PIO2 );
	if( (npio2 & 1) != 0)
		npio2 += 1;
	if( npio2 != 0)
		{
		// Changed from K = ellpk(a)
		K = ellpk(1.0 - a );
		phi = phi - npio2 * PIO2;
		}
	else
		K = 0.0;
	if( phi < 0.0 )
		{
		phi = -phi;
		sign = -1;
		}
	else
		sign = 0;
	b = Math.sqrt(a);
	t = Math.tan( phi );
	if( Math.abs(t) > 10.0 )
		{
		/* Transform the amplitude */
		e = 1.0/(b*t);
		/* ... but avoid multiple recursions.  */
		if( Math.abs(e) < 10.0 )
			{
			e = Math.atan(e);
			if( npio2 == 0 )
				// Changed from K = ellpk(a)
				K = ellpk(1.0 - a );
			temp = K - ellik( e, m );
			if( sign < 0 )
				temp = -temp;
			temp += npio2 * K;
			return( temp );
			}
		}
	a = 1.0;
	c = Math.sqrt(m);
	d = 1;
	mod = 0;

	while( Math.abs(c/a) > MACHEP )
		{
		temp = b/a;
		phi = phi + Math.atan(t*temp) + mod * Math.PI;
		mod = (int)((phi + PIO2)/Math.PI);
		t = t * ( 1.0 + temp )/( 1.0 - temp * t * t );
		c = ( a - b )/2.0;
		temp = Math.sqrt( a * b );
		a = ( a + b )/2.0;
		b = temp;
		d += d;
		}

	temp = (Math.atan(t) + mod * Math.PI)/(d * a);

	if( sign < 0 )
		temp = -temp;
	temp += npio2 * K;
	return( temp );
	}
	
	/*							expn.c
	 *
	 *		Exponential integral En
	 *
	 *
	 *
	 * SYNOPSIS:
	 *
	 * int n;
	 * double x, y, expn();
	 *
	 * y = expn( n, x );
	 *
	 *
	 *
	 * DESCRIPTION:
	 *
	 * Evaluates the exponential integral
	 *
	 *                 inf.
	 *                   -
	 *                  | |   -xt
	 *                  |    e
	 *      E (x)  =    |    ----  dt.
	 *       n          |      n
	 *                | |     t
	 *                 -
	 *                  1
	 *
	 *
	 * Both n and x must be nonnegative.
	 *
	 * The routine employs either a power series, a continued
	 * fraction, or an asymptotic formula depending on the
	 * relative values of n and x.
	 *
	 * ACCURACY:
	 *
	 *                      Relative error:
	 * arithmetic   domain     # trials      peak         rms
	 *    DEC       0, 30        5000       2.0e-16     4.6e-17
	 *    IEEE      0, 30       10000       1.7e-15     3.6e-16
	 *
	 */
	
	/* Cephes Math Library Release 1.1:  March, 1985
	 * Copyright 1985 by Stephen L. Moshier
	 * Direct inquiries to 30 Frost Street, Cambridge, MA 02140 */

	public double expn(int n, double x ) {
	double ans, r, t, yk, xk;
	double pk, pkm1, pkm2, qk, qkm1, qkm2;
	double psi, z;
	int i, k;
	double big = BIG;

	if( n < 0 ) {
		MipavUtil.displayError("Domain error in expn n < 0");
		return (MAXNUM);
	}

	if( x < 0 )
		{
		MipavUtil.displayError("Domain error in expn x < 0");
		return (MAXNUM);
		}

	if( x > MAXLOG )
		return( 0.0 );

	if( x == 0.0 )
		{
		if( n < 2 )
			{
			MipavUtil.displayError("SINGULARITY error in expn");
			return( MAXNUM );
			}
		else
			return( 1.0/(n-1.0) );
		}

	if( n == 0 )
		return( Math.exp(-x)/x );
	
	/*							expn.c	*/
	/*		Expansion for large n		*/

	if( n > 5000 )
		{
		xk = x + n;
		yk = 1.0 / (xk * xk);
		t = n;
		ans = yk * t * (6.0 * x * x  -  8.0 * t * x  +  t * t);
		ans = yk * (ans + t * (t  -  2.0 * x));
		ans = yk * (ans + t);
		ans = (ans + 1.0) * Math.exp( -x ) / xk;
		return (ans);
		}

	if(x <=  1.0 ) {
	/*							expn.c	*/

	/*		Power series expansion		*/

	psi = -EUL - Math.log(x);
	for( i=1; i<n; i++ )
		psi = psi + 1.0/i;

	z = -x;
	xk = 0.0;
	yk = 1.0;
	pk = 1.0 - n;
	if( n == 1 )
		ans = 0.0;
	else
		ans = 1.0/pk;
	do
		{
		xk += 1.0;
		yk *= z/xk;
		pk += 1.0;
		if( pk != 0.0 )
			{
			ans += yk/pk;
			}
		if( ans != 0.0 )
			t = Math.abs(yk/ans);
		else
			t = 1.0;
		}
	while( t > MACHEP );
	k = (int)xk;
	t = n;
	r = n - 1;
	ans = (Math.pow(z, r) * psi / true_gamma(t)) - ans;
	return (ans);
	} // if (x <= 1.0)
	
	/*							expn.c	*/
	/*		continued fraction		*/
	k = 1;
	pkm2 = 1.0;
	qkm2 = x;
	pkm1 = 1.0;
	qkm1 = x + n;
	ans = pkm1/qkm1;

	do
		{
		k += 1;
		if( (k & 1) != 0 )
			{
			yk = 1.0;
			xk = n + (k-1)/2;
			}
		else
			{
			yk = x;
			xk = k/2;
			}
		pk = pkm1 * yk  +  pkm2 * xk;
		qk = qkm1 * yk  +  qkm2 * xk;
		if( qk != 0 )
			{
			r = pk/qk;
			t = Math.abs( (ans - r)/r );
			ans = r;
			}
		else
			t = 1.0;
		pkm2 = pkm1;
		pkm1 = pk;
		qkm2 = qkm1;
		qkm1 = qk;
	if( Math.abs(pk) > big )
			{
			pkm2 /= big;
			pkm1 /= big;
			qkm2 /= big;
			qkm1 /= big;
			}
		}
	while( t > MACHEP );

	ans *= Math.exp( -x );

	return( ans );
	}

	/*							fac.c
	 *
	 *	Factorial function
	 *
	 *
	 *
	 * SYNOPSIS:
	 *
	 * double y, fac();
	 * int i;
	 *
	 * y = fac( i );
	 *
	 *
	 *
	 * DESCRIPTION:
	 *
	 * Returns factorial of i  =  1 * 2 * 3 * ... * i.
	 * fac(0) = 1.0.
	 *
	 * Due to machine arithmetic bounds the largest value of
	 * i accepted is 33 in DEC arithmetic or 170 in IEEE
	 * arithmetic.  Greater values, or negative ones,
	 * produce an error message and return MAXNUM.
	 *
	 *
	 *
	 * ACCURACY:
	 *
	 * For i < 34 the values are simply tabulated, and have
	 * full machine accuracy.  If i > 55, fac(i) = true_gamma(i+1);
	 * see gamma.c.
	 *
	 *                      Relative error:
	 * arithmetic   domain      peak
	 *    IEEE      0, 170    1.4e-15
	 *    DEC       0, 33      1.4e-17
	 *
	 */
	
	/*
	Cephes Math Library Release 2.0:  April, 1987
	Copyright 1984, 1987 by Stephen L. Moshier
	Direct inquiries to 30 Frost Street, Cambridge, MA 02140
	*/
	
	public double fac(int i) {
	double x, f, n;
	int j;

	if( i < 0 )
		{
		MipavUtil.displayError("SINGULARITY in fac");
		return( MAXNUM );
		}

	if( i > MAXFAC )
		{
		MipavUtil.displayError("OVERFLOW in fac");
		return( MAXNUM );
		}

	/* Get answer from table for small i. */
	if( i < 34 )
		{
		return( factbl[i] );
		}
	/* Use gamma function for large i. */
	if( i > 55 )
		{
		x = i + 1;
		return( true_gamma(x) );
		}
	/* Compute directly for intermediate i. */
	n = 34.0;
	f = 34.0;
	for( j=35; j<=i; j++ )
		{
		n += 1.0;
		f *= n;
		}
		f *= factbl[33];
	return( f );
	}
	
	/*							incbet.c
	 *
	 *	Incomplete beta integral
	 *
	 *
	 * SYNOPSIS:
	 *
	 * double a, b, x, y, incbet();
	 *
	 * y = incbet( a, b, x );
	 *
	 *
	 * DESCRIPTION:
	 *
	 * Returns incomplete beta integral of the arguments, evaluated
	 * from zero to x.  The function is defined as
	 *
	 *                  x
	 *     -            -
	 *    | (a+b)      | |  a-1     b-1
	 *  -----------    |   t   (1-t)   dt.
	 *   -     -     | |
	 *  | (a) | (b)   -
	 *                 0
	 *
	 * The domain of definition is 0 <= x <= 1.  In this
	 * implementation a and b are restricted to positive values.
	 * The integral from x to 1 may be obtained by the symmetry
	 * relation
	 *
	 *    1 - incbet( a, b, x )  =  incbet( b, a, 1-x ).
	 *
	 * The integral is evaluated by a continued fraction expansion
	 * or, when b*x is small, by a power series.
	 *
	 * ACCURACY:
	 *
	 * Tested at uniformly distributed random points (a,b,x) with a and b
	 * in "domain" and x between 0 and 1.
	 *                                        Relative error
	 * arithmetic   domain     # trials      peak         rms
	 *    IEEE      0,5         10000       6.9e-15     4.5e-16
	 *    IEEE      0,85       250000       2.2e-13     1.7e-14
	 *    IEEE      0,1000      30000       5.3e-12     6.3e-13
	 *    IEEE      0,10000    250000       9.3e-11     7.1e-12
	 *    IEEE      0,100000    10000       8.7e-10     4.8e-11
	 * Outputs smaller than the IEEE gradual underflow threshold
	 * were excluded from these statistics.
	 *
	 * ERROR MESSAGES:
	 *   message         condition      value returned
	 * incbet domain      x<0, x>1          0.0
	 * incbet underflow                     0.0
	 */
	
	/*
	Cephes Math Library, Release 2.3:  March, 1995
	Copyright 1984, 1995 by Stephen L. Moshier
	*/
	
	public double incbet(double aa, double bb, double xx ) {
	double a, b, t, x, xc, w, y;
	int flag;

	if( aa <= 0.0 || bb <= 0.0 ) {
		MipavUtil.displayError("Domain error in incbet");
		return (0.0);
	}

	if( (xx <= 0.0) || ( xx >= 1.0) )
		{
		if( xx == 0.0 )
			return(0.0);
		if( xx == 1.0 )
			return( 1.0 );
		MipavUtil.displayError("Domain error in incbet");
		return( 0.0 );
		}

	flag = 0;
	if( (bb * xx) <= 1.0 && xx <= 0.95)
		{
		t = pseries(aa, bb, xx);
		if( flag == 1 )
		{
		if( t <= MACHEP )
			t = 1.0 - MACHEP;
		else
			t = 1.0 - t;
		}
	    return( t );
		}

	w = 1.0 - xx;

	/* Reverse a and b if x is greater than the mean. */
	if( xx > (aa/(aa+bb)) )
		{
		flag = 1;
		a = bb;
		b = aa;
		xc = xx;
		x = w;
		}
	else
		{
		a = aa;
		b = bb;
		xc = w;
		x = xx;
		}

	if( flag == 1 && (b * x) <= 1.0 && x <= 0.95)
		{
		t = pseries(a, b, x);
		if( flag == 1 )
		{
		if( t <= MACHEP )
			t = 1.0 - MACHEP;
		else
			t = 1.0 - t;
		}
	    return( t );
		}

	/* Choose expansion for better convergence. */
	y = x * (a+b-2.0) - (a-1.0);
	if( y < 0.0 )
		w = incbcf( a, b, x );
	else
		w = incbd( a, b, x ) / xc;

	/* Multiply w by the factor
	     a      b   _             _     _
	    x  (1-x)   | (a+b) / ( a | (a) | (b) ) .   */

	y = a * Math.log(x);
	t = b * Math.log(xc);
	if( (a+b) < MAXGAM && Math.abs(y) < MAXLOG && Math.abs(t) < MAXLOG )
		{
		t = Math.pow(xc,b);
		t *= Math.pow(x,a);
		t /= a;
		t *= w;
		t *= true_gamma(a+b) / (true_gamma(a) * true_gamma(b));
		if( flag == 1 )
		{
		if( t <= MACHEP )
			t = 1.0 - MACHEP;
		else
			t = 1.0 - t;
		}
	    return( t );
		}
	/* Resort to logarithms.  */
	y += t + lgam(a+b) - lgam(a) - lgam(b);
	y += Math.log(w/a);
	if( y < MINLOG )
		t = 0.0;
	else
		t = Math.exp(y);

	if( flag == 1 )
		{
		if( t <= MACHEP )
			t = 1.0 - MACHEP;
		else
			t = 1.0 - t;
		}
	return( t );
	}
	
	/* Continued fraction expansion #1
	 * for incomplete beta integral
	 */

	public double incbcf(double a, double b, double x ) {
	double xk, pk, pkm1, pkm2, qk, qkm1, qkm2;
	double k1, k2, k3, k4, k5, k6, k7, k8;
	double r, t, ans, thresh;
	int n;

	k1 = a;
	k2 = a + b;
	k3 = a;
	k4 = a + 1.0;
	k5 = 1.0;
	k6 = b - 1.0;
	k7 = k4;
	k8 = a + 2.0;

	pkm2 = 0.0;
	qkm2 = 1.0;
	pkm1 = 1.0;
	qkm1 = 1.0;
	ans = 1.0;
	r = 1.0;
	n = 0;
	thresh = 3.0 * MACHEP;
	do
		{
		
		xk = -( x * k1 * k2 )/( k3 * k4 );
		pk = pkm1 +  pkm2 * xk;
		qk = qkm1 +  qkm2 * xk;
		pkm2 = pkm1;
		pkm1 = pk;
		qkm2 = qkm1;
		qkm1 = qk;

		xk = ( x * k5 * k6 )/( k7 * k8 );
		pk = pkm1 +  pkm2 * xk;
		qk = qkm1 +  qkm2 * xk;
		pkm2 = pkm1;
		pkm1 = pk;
		qkm2 = qkm1;
		qkm1 = qk;

		if( qk != 0 )
			r = pk/qk;
		if( r != 0 )
			{
			t = Math.abs( (ans - r)/r );
			ans = r;
			}
		else
			t = 1.0;

		if( t < thresh )
			return (ans);

		k1 += 1.0;
		k2 += 1.0;
		k3 += 2.0;
		k4 += 2.0;
		k5 += 1.0;
		k6 -= 1.0;
		k7 += 2.0;
		k8 += 2.0;

		if( (Math.abs(qk) + Math.abs(pk)) > big )
			{
			pkm2 *= biginv;
			pkm1 *= biginv;
			qkm2 *= biginv;
			qkm1 *= biginv;
			}
		if( (Math.abs(qk) < biginv) || (Math.abs(pk) < biginv) )
			{
			pkm2 *= big;
			pkm1 *= big;
			qkm2 *= big;
			qkm1 *= big;
			}
		}
	while( ++n < 300 );

	return(ans);
	}
	
	/* Continued fraction expansion #2
	 * for incomplete beta integral
	 */

	public double incbd(double a, double b, double x) {
	double xk, pk, pkm1, pkm2, qk, qkm1, qkm2;
	double k1, k2, k3, k4, k5, k6, k7, k8;
	double r, t, ans, z, thresh;
	int n;

	k1 = a;
	k2 = b - 1.0;
	k3 = a;
	k4 = a + 1.0;
	k5 = 1.0;
	k6 = a + b;
	k7 = a + 1.0;;
	k8 = a + 2.0;

	pkm2 = 0.0;
	qkm2 = 1.0;
	pkm1 = 1.0;
	qkm1 = 1.0;
	z = x / (1.0-x);
	ans = 1.0;
	r = 1.0;
	n = 0;
	thresh = 3.0 * MACHEP;
	do
		{
		
		xk = -( z * k1 * k2 )/( k3 * k4 );
		pk = pkm1 +  pkm2 * xk;
		qk = qkm1 +  qkm2 * xk;
		pkm2 = pkm1;
		pkm1 = pk;
		qkm2 = qkm1;
		qkm1 = qk;

		xk = ( z * k5 * k6 )/( k7 * k8 );
		pk = pkm1 +  pkm2 * xk;
		qk = qkm1 +  qkm2 * xk;
		pkm2 = pkm1;
		pkm1 = pk;
		qkm2 = qkm1;
		qkm1 = qk;

		if( qk != 0 )
			r = pk/qk;
		if( r != 0 )
			{
			t = Math.abs( (ans - r)/r );
			ans = r;
			}
		else
			t = 1.0;

		if( t < thresh )
			return ans;

		k1 += 1.0;
		k2 -= 1.0;
		k3 += 2.0;
		k4 += 2.0;
		k5 += 1.0;
		k6 += 1.0;
		k7 += 2.0;
		k8 += 2.0;

		if( (Math.abs(qk) + Math.abs(pk)) > big )
			{
			pkm2 *= biginv;
			pkm1 *= biginv;
			qkm2 *= biginv;
			qkm1 *= biginv;
			}
		if( (Math.abs(qk) < biginv) || (Math.abs(pk) < biginv) )
			{
			pkm2 *= big;
			pkm1 *= big;
			qkm2 *= big;
			qkm1 *= big;
			}
		}
	while( ++n < 300 );
	
	return(ans);
	}
	
	/* Power series for incomplete beta integral.
	   Use when b*x is small and x not too close to 1.  */

	public double pseries(double a, double b, double x) {
	double s, t, u, v, n, t1, z, ai;

	ai = 1.0 / a;
	u = (1.0 - b) * x;
	v = u / (a + 1.0);
	t1 = v;
	t = u;
	n = 2.0;
	s = 0.0;
	z = MACHEP * ai;
	while( Math.abs(v) > z )
		{
		u = (n - b) * x / n;
		t *= u;
		v = t / (a + n);
		s += v; 
		n += 1.0;
		}
	s += t1;
	s += ai;

	u = a * Math.log(x);
	if( (a+b) < MAXGAM && Math.abs(u) < MAXLOG )
		{
		t = true_gamma(a+b)/(true_gamma(a)*true_gamma(b));
		s = s * t * Math.pow(x,a);
		}
	else
		{
		t = lgam(a+b) - lgam(a) - lgam(b) + u + Math.log(s);
		if( t < MINLOG )
			s = 0.0;
		else
		s = Math.exp(t);
		}
	return(s);
	}

	/*							incbi()
	 *
	 *      Inverse of imcomplete beta integral
	 *
	 *
	 *
	 * SYNOPSIS:
	 *
	 * double a, b, x, y, incbi();
	 *
	 * x = incbi( a, b, y );
	 *
	 *
	 *
	 * DESCRIPTION:
	 *
	 * Given y, the function finds x such that
	 *
	 *  incbet( a, b, x ) = y .
	 *
	 * The routine performs interval halving or Newton iterations to find the
	 * root of incbet(a,b,x) - y = 0.
	 *
	 *
	 * ACCURACY:
	 *
	 *                      Relative error:
	 *                x     a,b
	 * arithmetic   domain  domain  # trials    peak       rms
	 *    IEEE      0,1    .5,10000   50000    5.8e-12   1.3e-13
	 *    IEEE      0,1   .25,100    100000    1.8e-13   3.9e-15
	 *    IEEE      0,1     0,5       50000    1.1e-12   5.5e-15
	 *    VAX       0,1    .5,100     25000    3.5e-14   1.1e-15
	 * With a and b constrained to half-integer or integer values:
	 *    IEEE      0,1    .5,10000   50000    5.8e-12   1.1e-13
	 *    IEEE      0,1    .5,100    100000    1.7e-14   7.9e-16
	 * With a = .5, b constrained to half-integer or integer values:
	 *    IEEE      0,1    .5,10000   10000    8.3e-11   1.0e-11
	 */
	
	/*
	Cephes Math Library Release 2.4:  March,1996
	Copyright 1984, 1996 by Stephen L. Moshier
	*/
	
	double incbi(double aa, double bb, double yy0 ) {
	double d, x0, x1, lgm, yp, di, dithresh, yl, yh, xt;
	double a = 0.0;
	double b = 0.0;
	double y0 = 0.0;
	double y = 0.0;
	double x = 0.0;
	int i, dir, nflg;
	int rflg = 0;
	boolean doseg1 = true;
	boolean doseg2 = true;
    boolean doseg3 = true;

	i = 0;
	if( yy0 <= 0 )
		return(0.0);
	if( yy0 >= 1.0 )
		return(1.0);
	x0 = 0.0;
	yl = 0.0;
	x1 = 1.0;
	yh = 1.0;
	nflg = 0;

	if( aa <= 1.0 || bb <= 1.0 )
		{
		dithresh = 1.0e-6;
		rflg = 0;
		a = aa;
		b = bb;
		y0 = yy0;
		x = a/(a+b);
		y = incbet( a, b, x );
		doseg1 = false;;
		}
	else
		{
		dithresh = 1.0e-4;
		}
	/* approximation to inverse function */
    if (doseg1) {
	yp = -ndtri(yy0);

	if( yy0 > 0.5 )
		{
		rflg = 1;
		a = bb;
		b = aa;
		y0 = 1.0 - yy0;
		yp = -yp;
		}
	else
		{
		rflg = 0;
		a = aa;
		b = bb;
		y0 = yy0;
		}

	lgm = (yp * yp - 3.0)/6.0;
	x = 2.0/( 1.0/(2.0*a-1.0)  +  1.0/(2.0*b-1.0) );
	d = yp * Math.sqrt( x + lgm ) / x
		- ( 1.0/(2.0*b-1.0) - 1.0/(2.0*a-1.0) )
		* (lgm + 5.0/6.0 - 2.0/(3.0*x));
	d = 2.0 * d;
	if( d < MINLOG )
		{
		x = 1.0;
		MipavUtil.displayError("UNDERFLOW in incbi");
		x = 0.0;
		if( rflg != 0)
		{
		if( x <= MACHEP )
			x = 1.0 - MACHEP;
		else
			x = 1.0 - x;
		}
	    return( x );
		}
	x = a/( a + b * Math.exp(d) );
	y = incbet( a, b, x );
	yp = (y - y0)/y0;
	if( Math.abs(yp) < 0.2 )
		doseg2 = false;
    } // if (doseg1)
    
	/* Resort to interval halving if not close enough. */
	ihalve: while (true) {
    if (doseg2) {
	dir = 0;
	di = 0.5;
	for( i=0; i<100; i++ )
		{
		if( i != 0 )
			{
			x = x0  +  di * (x1 - x0);
			if( x == 1.0 )
				x = 1.0 - MACHEP;
			if( x == 0.0 )
				{
				di = 0.5;
				x = x0  +  di * (x1 - x0);
				if( x == 0.0 ) {
					x = 1.0;
					MipavUtil.displayError("UNDERFLOW in incbi");
					x = 0.0;
					if( rflg != 0)
					{
					if( x <= MACHEP )
						x = 1.0 - MACHEP;
					else
						x = 1.0 - x;
					}
				    return( x );	
				}
				}
			y = incbet( a, b, x );
			yp = (x1 - x0)/(x1 + x0);
			if( Math.abs(yp) < dithresh ) {
				doseg3 = false;
				break;
			}
			yp = (y-y0)/y0;
			if( Math.abs(yp) < dithresh ) {
				doseg3 = false;
				break;
			}
			}
		if( y < y0 )
			{
			x0 = x;
			yl = y;
			if( dir < 0 )
				{
				dir = 0;
				di = 0.5;
				}
			else if( dir > 3 )
				di = 1.0 - (1.0 - di) * (1.0 - di);
			else if( dir > 1 )
				di = 0.5 * di + 0.5; 
			else
				di = (y0 - y)/(yh - yl);
			dir += 1;
			if( x0 > 0.75 )
				{
				if( rflg == 1 )
					{
					rflg = 0;
					a = aa;
					b = bb;
					y0 = yy0;
					}
				else
					{
					rflg = 1;
					a = bb;
					b = aa;
					y0 = 1.0 - yy0;
					}
				x = 1.0 - x;
				y = incbet( a, b, x );
				x0 = 0.0;
				yl = 0.0;
				x1 = 1.0;
				yh = 1.0;
				continue ihalve;
				}
			}
		else
			{
			x1 = x;
			if( rflg == 1 && x1 < MACHEP )
				{
				x = 0.0;
				if( rflg != 0)
				{
				if( x <= MACHEP )
					x = 1.0 - MACHEP;
				else
					x = 1.0 - x;
				}
			    return( x );
				}
			yh = y;
			if( dir > 0 )
				{
				dir = 0;
				di = 0.5;
				}
			else if( dir < -3 )
				di = di * di;
			else if( dir < -1 )
				di = 0.5 * di;
			else
				di = (y - y0)/(yh - yl);
			dir -= 1;
			}
		}
	if (doseg3) {
	MipavUtil.displayError("PRECISION LOSS in incbi");
	if( x0 >= 1.0 )
		{
		x = 1.0 - MACHEP;
		if( rflg != 0)
		{
		if( x <= MACHEP )
			x = 1.0 - MACHEP;
		else
			x = 1.0 - x;
		}
	    return( x );
		}
	if( x <= 0.0 )
		{
		MipavUtil.displayError("UNDERFLOW in incbi");
		x = 0.0;
		if( rflg != 0)
		{
		if( x <= MACHEP )
			x = 1.0 - MACHEP;
		else
			x = 1.0 - x;
		}
	    return( x );
		}
	} // if (doseg3)
	doseg3 = true;
    } // if (doseg2)
    doseg2 = true;

	if( nflg != 0) {
		if( rflg != 0)
		{
		if( x <= MACHEP )
			x = 1.0 - MACHEP;
		else
			x = 1.0 - x;
		}
	    return( x );	
	}
	nflg = 1;
	lgm = lgam(a+b) - lgam(a) - lgam(b);

	for( i=0; i<8; i++ )
		{
		/* Compute the function at this point. */
		if( i != 0 )
			y = incbet(a,b,x);
		if( y < yl )
			{
			x = x0;
			y = yl;
			}
		else if( y > yh )
			{
			x = x1;
			y = yh;
			}
		else if( y < y0 )
			{
			x0 = x;
			yl = y;
			}
		else
			{
			x1 = x;
			yh = y;
			}
		if( x == 1.0 || x == 0.0 )
			break;
		/* Compute the derivative of the function at this point. */
		d = (a - 1.0) * Math.log(x) + (b - 1.0) * Math.log(1.0-x) + lgm;
		if( d < MINLOG ) {
			if( rflg != 0)
			{
			if( x <= MACHEP )
				x = 1.0 - MACHEP;
			else
				x = 1.0 - x;
			}
		    return( x );	
		}
		if( d > MAXLOG )
			break;
		d = Math.exp(d);
		/* Compute the step to the next approximation of x. */
		d = (y - y0)/d;
		xt = x - d;
		if( xt <= x0 )
			{
			y = (x - x0) / (x1 - x0);
			xt = x0 + 0.5 * y * (x - x0);
			if( xt <= 0.0 )
				break;
			}
		if( xt >= x1 )
			{
			y = (x1 - x) / (x1 - x0);
			xt = x1 - 0.5 * y * (x1 - x);
			if( xt >= 1.0 )
				break;
			}
		x = xt;
		if( Math.abs(d/x) < 128.0 * MACHEP ) {
			if( rflg != 0)
			{
			if( x <= MACHEP )
				x = 1.0 - MACHEP;
			else
				x = 1.0 - x;
			}
		    return( x );
		}
		}
	/* Did not converge.  */
	dithresh = 256.0 * MACHEP;
	} // ihalve: while (true)

	}
	
	/*							fdtr.c
	 *
	 *	F distribution
	 *
	 *
	 *
	 * SYNOPSIS:
	 *
	 * int df1, df2;
	 * double x, y, fdtr();
	 *
	 * y = fdtr( df1, df2, x );
	 *
	 * DESCRIPTION:
	 *
	 * Returns the area from zero to x under the F density
	 * function (also known as Snedcor's density or the
	 * variance ratio density).  This is the density
	 * of x = (u1/df1)/(u2/df2), where u1 and u2 are random
	 * variables having Chi square distributions with df1
	 * and df2 degrees of freedom, respectively.
	 *
	 * The incomplete beta integral is used, according to the
	 * formula
	 *
	 *	P(x) = incbet( df1/2, df2/2, (df1*x/(df2 + df1*x) ).
	 *
	 *
	 * The arguments a and b are greater than zero, and x is
	 * nonnegative.
	 *
	 * ACCURACY:
	 *
	 * Tested at random points (a,b,x).
	 *
	 *                x     a,b                     Relative error:
	 * arithmetic  domain  domain     # trials      peak         rms
	 *    IEEE      0,1    0,100       100000      9.8e-15     1.7e-15
	 *    IEEE      1,5    0,100       100000      6.5e-15     3.5e-16
	 *    IEEE      0,1    1,10000     100000      2.2e-11     3.3e-12
	 *    IEEE      1,5    1,10000     100000      1.1e-11     1.7e-13
	 * See also incbet.c.
	 *
	 *
	 * ERROR MESSAGES:
	 *
	 *   message         condition      value returned
	 * fdtr domain     a<0, b<0, x<0         0.0
	 *
	 */
	
	/*							fdtrc()
	 *
	 *	Complemented F distribution
	 *
	 *
	 *
	 * SYNOPSIS:
	 *
	 * int df1, df2;
	 * double x, y, fdtrc();
	 *
	 * y = fdtrc( df1, df2, x );
	 *
	 * DESCRIPTION:
	 *
	 * Returns the area from x to infinity under the F density
	 * function (also known as Snedcor's density or the
	 * variance ratio density).
	 *
	 *
	 *                      inf.
	 *                       -
	 *              1       | |  a-1      b-1
	 * 1-P(x)  =  ------    |   t    (1-t)    dt
	 *            B(a,b)  | |
	 *                     -
	 *                      x
	 *
	 *
	 * The incomplete beta integral is used, according to the
	 * formula
	 *
	 *	P(x) = incbet( df2/2, df1/2, (df2/(df2 + df1*x) ).
	 *
	 *
	 * ACCURACY:
	 *
	 * Tested at random points (a,b,x) in the indicated intervals.
	 *                x     a,b                     Relative error:
	 * arithmetic  domain  domain     # trials      peak         rms
	 *    IEEE      0,1    1,100       100000      3.7e-14     5.9e-16
	 *    IEEE      1,5    1,100       100000      8.0e-15     1.6e-15
	 *    IEEE      0,1    1,10000     100000      1.8e-11     3.5e-13
	 *    IEEE      1,5    1,10000     100000      2.0e-11     3.0e-12
	 * See also incbet.c.
	 *
	 * ERROR MESSAGES:
	 *
	 *   message         condition      value returned
	 * fdtrc domain    a<0, b<0, x<0         0.0
	 *
	 */
	
	/*							fdtri()
	 *
	 *	Inverse of complemented F distribution
	 *
	 *
	 *
	 * SYNOPSIS:
	 *
	 * int df1, df2;
	 * double x, p, fdtri();
	 *
	 * x = fdtri( df1, df2, p );
	 *
	 * DESCRIPTION:
	 *
	 * Finds the F density argument x such that the integral
	 * from x to infinity of the F density is equal to the
	 * given probability p.
	 *
	 * This is accomplished using the inverse beta integral
	 * function and the relations
	 *
	 *      z = incbi( df2/2, df1/2, p )
	 *      x = df2 (1-z) / (df1 z).
	 *
	 * Note: the following relations hold for the inverse of
	 * the uncomplemented F distribution:
	 *
	 *      z = incbi( df1/2, df2/2, p )
	 *      x = df2 z / (df1 (1-z)).
	 *
	 * ACCURACY:
	 *
	 * Tested at random points (a,b,p).
	 *
	 *              a,b                     Relative error:
	 * arithmetic  domain     # trials      peak         rms
	 *  For p between .001 and 1:
	 *    IEEE     1,100       100000      8.3e-15     4.7e-16
	 *    IEEE     1,10000     100000      2.1e-11     1.4e-13
	 *  For p between 10^-6 and 10^-3:
	 *    IEEE     1,100        50000      1.3e-12     8.4e-15
	 *    IEEE     1,10000      50000      3.0e-12     4.8e-14
	 * See also fdtrc.c.
	 *
	 * ERROR MESSAGES:
	 *
	 *   message         condition      value returned
	 * fdtri domain   p <= 0 or p > 1       0.0
	 *                     v < 1
	 *
	 */
	
	/*
	Cephes Math Library Release 2.3:  March, 1995
	Copyright 1984, 1987, 1995 by Stephen L. Moshier
	*/
	
	public double fdtrc(int ia, int ib, double x) {
	double a, b, w;

	if( (ia < 1) || (ib < 1) || (x < 0.0) )
		{
		MipavUtil.displayError("DOMAIN error in fdtrc");
		return( 0.0 );
		}
	a = ia;
	b = ib;
	w = b / (b + a * x);
	return( incbet( 0.5*b, 0.5*a, w ) );
	}
	
	public double fdtr(int ia, int ib, double x) {
	double a, b, w;

	if( (ia < 1) || (ib < 1) || (x < 0.0) )
		{
		MipavUtil.displayError("DOMAIN error in fdtr");
		return( 0.0 );
		}
	a = ia;
	b = ib;
	w = a * x;
	w = w / (b + w);
	return( incbet(0.5*a, 0.5*b, w) );
	}
	
	public double fdtri(int ia, int ib, double y) {
	double a, b, w, x;
	
	// added by danilo
    y = 1.0 - y;

	if( (ia < 1) || (ib < 1) || (y <= 0.0) || (y > 1.0) )
		{
		MipavUtil.displayError("DOMAIN error in fdtri");
		return( 0.0 );
		}
	a = ia;
	b = ib;
	/* Compute probability for x = 0.5.  */
	w = incbet( 0.5*b, 0.5*a, 0.5 );
	/* If that is greater than y, then the solution w < .5.
	   Otherwise, solve at 1-y to remove cancellation in (b - b*w).  */
	if( w > y || y < 0.001)
		{
		w = incbi( 0.5*b, 0.5*a, y );
		x = (b - b*w)/(a*w);
		}
	else
		{
		w = incbi( 0.5*a, 0.5*b, 1.0-y );
		x = b*w/(a*(1.0-w));
		}
	return(x);
	}
	
	/*							fresnl.c
	 *
	 *	Fresnel integral
	 *
	 *
	 *
	 * SYNOPSIS:
	 *
	 * double x, S, C;
	 * void fresnl();
	 *
	 * fresnl( x, _&S, _&C );
	 *
	 *
	 * DESCRIPTION:
	 *
	 * Evaluates the Fresnel integrals
	 *
	 *           x
	 *           -
	 *          | |
	 * C(x) =   |   cos(pi/2 t**2) dt,
	 *        | |
	 *         -
	 *          0
	 *
	 *           x
	 *           -
	 *          | |
	 * S(x) =   |   sin(pi/2 t**2) dt.
	 *        | |
	 *         -
	 *          0
	 *
	 *
	 * The integrals are evaluated by a power series for x < 1.
	 * For x >= 1 auxiliary functions f(x) and g(x) are employed
	 * such that
	 *
	 * C(x) = 0.5 + f(x) sin( pi/2 x**2 ) - g(x) cos( pi/2 x**2 )
	 * S(x) = 0.5 - f(x) cos( pi/2 x**2 ) - g(x) sin( pi/2 x**2 )
	 *
	 *
	 *
	 * ACCURACY:
	 *
	 *  Relative error.
	 *
	 * Arithmetic  function   domain     # trials      peak         rms
	 *   IEEE       S(x)      0, 10       10000       2.0e-15     3.2e-16
	 *   IEEE       C(x)      0, 10       10000       1.8e-15     3.3e-16
	 *   DEC        S(x)      0, 10        6000       2.2e-16     3.9e-17
	 *   DEC        C(x)      0, 10        5000       2.3e-16     3.9e-17
	 */
	
	/*
	Cephes Math Library Release 2.1:  January, 1989
	Copyright 1984, 1987, 1989 by Stephen L. Moshier
	Direct inquiries to 30 Frost Street, Cambridge, MA 02140
	*/
	
	public int fresnl(double xxa, double ssa[], double cca[]) {
	double f, g, cc, ss, c, s, t, u;
	double x, x2;

	x = Math.abs(xxa);
	x2 = x * x;
	if( x2 < 2.5625 )
		{
		t = x2 * x2;
		ss = x * x2 * polevl( t, sn, 5)/p1evl( t, sd, 6 );
		cc = x * polevl( t, cn, 5)/polevl(t, cd, 6 );
		if( xxa < 0.0 )
		{
		cc = -cc;
		ss = -ss;
		}

	    cca[0] = cc;
	    ssa[0] = ss;
	    return(0);
		}






	if( x > 36974.0 )
		{
		cc = 0.5;
		ss = 0.5;
		if( xxa < 0.0 )
		{
		cc = -cc;
		ss = -ss;
		}

	    cca[0] = cc;
	    ssa[0] = ss;
	    return(0);
		}


	/*		Asymptotic power series auxiliary functions
	 *		for large argument
	 */
		x2 = x * x;
		t = Math.PI * x2;
		u = 1.0/(t * t);
		t = 1.0/t;
		f = 1.0 - u * polevl( u, fn, 9)/p1evl(u, fd, 10);
		g = t * polevl( u, gn, 10)/p1evl(u, gd, 11);

		t = PIO2 * x2;
		c = Math.cos(t);
		s = Math.sin(t);
		t = Math.PI * x;
		cc = 0.5  +  (f * s  -  g * c)/t;
		ss = 0.5  -  (f * c  +  g * s)/t;

	if( xxa < 0.0 )
		{
		cc = -cc;
		ss = -ss;
		}

	cca[0] = cc;
	ssa[0] = ss;
	return(0);
	}
	
	/*							gdtr.c
	 *
	 *	Gamma distribution function
	 *
	 *
	 *
	 * SYNOPSIS:
	 *
	 * double a, b, x, y, gdtr();
	 *
	 * y = gdtr( a, b, x );
	 *
	 *
	 *
	 * DESCRIPTION:
	 *
	 * Returns the integral from zero to x of the gamma probability
	 * density function:
	 *
	 *
	 *                x
	 *        b       -
	 *       a       | |   b-1  -at
	 * y =  -----    |    t    e    dt
	 *       -     | |
	 *      | (b)   -
	 *               0
	 *
	 *  The incomplete gamma integral is used, according to the
	 * relation
	 *
	 * y = igam( b, ax ).
	 *
	 *
	 * ACCURACY:
	 *
	 * See igam().
	 *
	 * ERROR MESSAGES:
	 *
	 *   message         condition      value returned
	 * gdtr domain         x < 0            0.0
	 *
	 */
	
	/*							gdtrc.c
	 *
	 *	Complemented gamma distribution function
	 *
	 *
	 *
	 * SYNOPSIS:
	 *
	 * double a, b, x, y, gdtrc();
	 *
	 * y = gdtrc( a, b, x );
	 *
	 *
	 *
	 * DESCRIPTION:
	 *
	 * Returns the integral from x to infinity of the gamma
	 * probability density function:
	 *
	 *
	 *               inf.
	 *        b       -
	 *       a       | |   b-1  -at
	 * y =  -----    |    t    e    dt
	 *       -     | |
	 *      | (b)   -
	 *               x
	 *
	 *  The incomplete gamma integral is used, according to the
	 * relation
	 *
	 * y = igamc( b, ax ).
	 *
	 *
	 * ACCURACY:
	 *
	 * See igamc().
	 *
	 * ERROR MESSAGES:
	 *
	 *   message         condition      value returned
	 * gdtrc domain         x < 0            0.0
	 *
	 */
	
	/*
	Cephes Math Library Release 2.3:  March,1995
	Copyright 1984, 1987, 1995 by Stephen L. Moshier
	*/

	public double gdtr(double a, double b, double x) {

	if( x < 0.0 )
		{
		MipavUtil.displayError("DOMAIN error in gdtr");
		return( 0.0 );
		}
	return(  igam( b, a * x )  );
	}
	
	public double gdtrc(double a, double b, double x) {

	if( x < 0.0 )
		{
		MipavUtil.displayError("DOMAIN error in gdtrc");
		return( 0.0 );
		}
	return(  igamc( b, a * x )  );
	}
	
	/*							psi.c
	 *
	 *	Psi (digamma) function
	 *
	 *
	 * SYNOPSIS:
	 *
	 * double x, y, psi();
	 *
	 * y = psi( x );
	 *
	 *
	 * DESCRIPTION:
	 *
	 *              d      -
	 *   psi(x)  =  -- ln | (x)
	 *              dx
	 *
	 * is the logarithmic derivative of the gamma function.
	 * For integer x,
	 *                   n-1
	 *                    -
	 * psi(n) = -EUL  +   >  1/k.
	 *                    -
	 *                   k=1
	 *
	 * This formula is used for 0 < n <= 10.  If x is negative, it
	 * is transformed to a positive argument by the reflection
	 * formula  psi(1-x) = psi(x) + pi cot(pi x).
	 * For general positive x, the argument is made greater than 10
	 * using the recurrence  psi(x+1) = psi(x) + 1/x.
	 * Then the following asymptotic expansion is applied:
	 *
	 *                           inf.   B
	 *                            -      2k
	 * psi(x) = log(x) - 1/2x -   >   -------
	 *                            -        2k
	 *                           k=1   2k x
	 *
	 * where the B2k are Bernoulli numbers.
	 *
	 * ACCURACY:
	 *    Relative error (except absolute when |psi| < 1):
	 * arithmetic   domain     # trials      peak         rms
	 *    DEC       0,30         2500       1.7e-16     2.0e-17
	 *    IEEE      0,30        30000       1.3e-15     1.4e-16
	 *    IEEE      -30,0       40000       1.5e-15     2.2e-16
	 *
	 * ERROR MESSAGES:
	 *     message         condition      value returned
	 * psi singularity    x integer <=0      MAXNUM
	 */
	
	/*
	Cephes Math Library Release 2.2:  July, 1992
	Copyright 1984, 1987, 1992 by Stephen L. Moshier
	Direct inquiries to 30 Frost Street, Cambridge, MA 02140
	*/
	
	public double psi(double x) {
	double p, q, nz, s, w, y, z;
	int i, n, negative;

	negative = 0;
	nz = 0.0;

	if( x <= 0.0 )
		{
		negative = 1;
		q = x;
		p = Math.floor(q);
		if( p == q )
			{
			MipavUtil.displayError("SINGULARITY in psi");
			return( MAXNUM );
			}
	/* Remove the zeros of tan(PI x)
	 * by subtracting the nearest integer from x
	 */
		nz = q - p;
		if( nz != 0.5 )
			{
			if( nz > 0.5 )
				{
				p += 1.0;
				nz = q - p;
				}
			nz = Math.PI/Math.tan(Math.PI*nz);
			}
		else
			{
			nz = 0.0;
			}
		x = 1.0 - x;
		}

	/* check for positive integer up to 10 */
	if( (x <= 10.0) && (x == Math.floor(x)) )
		{
		y = 0.0;
		n = (int)x;
		for( i=1; i<n; i++ )
			{
			w = i;
			y += 1.0/w;
			}
		y -= EUL;
		if( negative != 0)
		{
		y -= nz;
		}

	    return(y);
		}

	s = x;
	w = 0.0;
	while( s < 10.0 )
		{
		w += 1.0/s;
		s += 1.0;
		}

	if( s < 1.0e17 )
		{
		z = 1.0/(s * s);
		y = z * polevl( z, APSI, 6 );
		}
	else
		y = 0.0;

	y = Math.log(s)  -  (0.5/s)  -  y  -  w;

	if( negative != 0)
		{
		y -= nz;
		}

	return(y);
	}

	/*							hyp2f1.c
	 *
	 *	Gauss hypergeometric function   F
	 *	                               2 1
	 *
	 *
	 * SYNOPSIS:
	 *
	 * double a, b, c, x, y, hyp2f1();
	 *
	 * y = hyp2f1( a, b, c, x );
	 *
	 *
	 * DESCRIPTION:
	 *
	 *
	 *  hyp2f1( a, b, c, x )  =   F ( a, b; c; x )
	 *                           2 1
	 *
	 *           inf.
	 *            -   a(a+1)...(a+k) b(b+1)...(b+k)   k+1
	 *   =  1 +   >   -----------------------------  x   .
	 *            -         c(c+1)...(c+k) (k+1)!
	 *          k = 0
	 *
	 *  Cases addressed are
	 *	Tests and escapes for negative integer a, b, or c
	 *	Linear transformation if c - a or c - b negative integer
	 *	Special case c = a or c = b
	 *	Linear transformation for  x near +1
	 *	Transformation for x < -0.5
	 *	Psi function expansion if x > 0.5 and c - a - b integer
	 *      Conditionally, a recurrence on c to make c-a-b > 0
	 *
	 * |x| > 1 is rejected.
	 *
	 * The parameters a, b, c are considered to be integer
	 * valued if they are within 1.0e-14 of the nearest integer
	 * (1.0e-13 for IEEE arithmetic).
	 *
	 * ACCURACY:
	 *
	 *
	 *               Relative error (-1 < x < 1):
	 * arithmetic   domain     # trials      peak         rms
	 *    IEEE      -1,7        230000      1.2e-11     5.2e-14
	 *
	 * Several special cases also tested with a, b, c in
	 * the range -7 to 7.
	 *
	 * ERROR MESSAGES:
	 *
	 * A "partial loss of precision" message is printed if
	 * the internally estimated relative error exceeds 1^-12.
	 * A "singularity" message is printed on overflow or
	 * in cases not addressed (such as x < -1).
	 */
	
	public double hyp2f1(double a, double b, double c, double x) {
		// hcephes version
		double d, d1, d2, e;
	    double p, q, r, s, y, ax;
	    double ia, ib, ic, id;
	    double t1;
	    int i, aid;
	    int neg_int_a = 0, neg_int_b = 0;
	    int neg_int_ca_or_cb = 0;

	    double err[] = new double[] {0.0};
	    ax = Math.abs(x);
	    s = 1.0 - x;
	    ia = Math.round(a); /* nearest integer to a */
	    ib = Math.round(b);

	    if (x == 0.0) {
	        return 1.0;
	    }

	    d = c - a - b;
	    id = Math.round(d);

	    if ((a == 0 || b == 0) && c != 0) {
	        return 1.0;
	    }

	    if (a <= 0 && Math.abs(a - ia) < EPS) { /* a is a negative integer */
	        neg_int_a = 1;
	    }

	    if (b <= 0 && Math.abs(b - ib) < EPS) { /* b is a negative integer */
	        neg_int_b = 1;
	    }

	    if ((d <= -1) && (!(Math.abs(d - id) > EPS && s < 0)) && (!((neg_int_a != 0)|| (neg_int_b != 0)))) {
	        return (Math.pow(s, d) * hyp2f1(c - a, c - b, c, x));
	    }
	    if (d <= 0 && x == 1 && !((neg_int_a != 0)|| (neg_int_b != 0))) {
	    	MipavUtil.displayError("OVERFLOW in hyp2f1");
	 	    return Double.POSITIVE_INFINITY;
	    }

	    if (ax < 1.0 || x == -1.0) {
	        /* 2F1(a,b;b;x) = (1-x)**(-a) */
	        if (Math.abs(b - c) < EPS) { /* b = c */
	            if (neg_int_b != 0) {
	                y = hcephes_hyp2f1_neg_c_equal_bc(a, b, x);
	            } else {
	                y = Math.pow(s, -a); /* s to the -a power */
	            }
	            if( err[0] > ETHRESH )
				{
				System.err.println("PRECISION LOSS in hyp2f1");
			    System.err.println( "Estimated err = " + err );
				}
			    return(y);
	        }
	        if (Math.abs(a - c) < EPS) { /* a = c */
	            y = Math.pow(s, -b);      /* s to the -b power */
	            if( err[0] > ETHRESH )
				{
				System.err.println("PRECISION LOSS in hyp2f1");
			    System.err.println( "Estimated err = " + err );
				}
			    return(y);
	        }
	    }

	    if (c <= 0.0) {
	        ic = Math.round(c);            /* nearest integer to c */
	        if (Math.abs(c - ic) < EPS) { /* c is a negative integer */
	            /* check if termination before explosion */
	            if ((neg_int_a != 0) && (ia > ic)) {
	            	y = hyt2f1(a, b, c, x, err);
            		if( err[0] > ETHRESH )
            		{
            		System.err.println("PRECISION LOSS in hyp2f1");
            	    System.err.println( "Estimated err = " + err );
            		}
            	    return(y);
	            }
	            if ((neg_int_b != 0) && (ib > ic)) {
	            	y = hyt2f1(a, b, c, x, err);
            		if( err[0] > ETHRESH )
            		{
            		System.err.println("PRECISION LOSS in hyp2f1");
            	    System.err.println( "Estimated err = " + err );
            		}
            	    return(y);	
	            }
	            MipavUtil.displayError("OVERFLOW in hyp2f1");
	    	    return Double.POSITIVE_INFINITY;
	        }
	    }

	    if ((neg_int_a != 0) || (neg_int_b != 0))  { /* function is a polynomial */
	    	y = hyt2f1(a, b, c, x, err);
    		if( err[0] > ETHRESH )
    		{
    		System.err.println("PRECISION LOSS in hyp2f1");
    	    System.err.println( "Estimated err = " + err );
    		}
    	    return(y);		
	    }

	    t1 = Math.abs(b - a);
	    if (x < -2.0 && Math.abs(t1 - Math.round(t1)) > EPS) {
	        /* This transform has a pole for b-a integer, and
	         * may produce large cancellation errors for |1/x| close 1
	         */
	        p = hyp2f1(a, 1 - c + a, 1 - b + a, 1.0 / x);
	        q = hyp2f1(b, 1 - c + b, 1 - a + b, 1.0 / x);
	        p *= Math.pow(-x, -a);
	        q *= Math.pow(-x, -b);
	        t1 = true_gamma(c);
	        s = t1 * true_gamma(b - a) / (true_gamma(b) * true_gamma(c - a));
	        y = t1 * true_gamma(a - b) / (true_gamma(a) * true_gamma(c - b));
	        return s * p + y * q;
	    } else if (x < -1.0) {
	        if (Math.abs(a) < Math.abs(b)) {
	            return Math.pow(s, -a) * hyp2f1(a, c - b, c, x / (x - 1));
	        } else {
	            return Math.pow(s, -b) * hyp2f1(b, c - a, c, x / (x - 1));
	        }
	    }

	    if (ax > 1.0) { /* series diverges  */
	        MipavUtil.displayError("OVERFLOW in hyp2f1");
	 	    return Double.POSITIVE_INFINITY;	
	    }

	    p = c - a;
	    ia = Math.round(p);                           /* nearest integer to c-a */
	    if ((ia <= 0.0) && (Math.abs(p - ia) < EPS)) /* negative int c - a */
	        neg_int_ca_or_cb = 1;

	    r = c - b;
	    ib = Math.round(r);                           /* nearest integer to c-b */
	    if ((ib <= 0.0) && (Math.abs(r - ib) < EPS)) /* negative int c - b */
	        neg_int_ca_or_cb = 1;

	    id = Math.round(d); /* nearest integer to d */
	    q = Math.abs(d - id);

	    /* Thanks to Christian Burger <BURGER@DMRHRZ11.HRZ.Uni-Marburg.DE>
	     * for reporting a bug here.  */
	    if (Math.abs(ax - 1.0) < EPS) { /* |x| == 1.0   */
	        if (x > 0.0) {
	            if (neg_int_ca_or_cb != 0) {
	                if (d >= 0.0) {
	                	y = Math.pow(s, d) * hys2f1(c - a, c - b, c, x, err);
	                	if( err[0] > ETHRESH )
	            		{
	            		System.err.println("PRECISION LOSS in hyp2f1");
	            	    System.err.println( "Estimated err = " + err );
	            		}
	            	    return(y);
	                }
	                else {
	                	MipavUtil.displayError("OVERFLOW in hyp2f1");
	            	    return Double.POSITIVE_INFINITY;	
	                }
	            }
	            if (d <= 0.0) {
	            	MipavUtil.displayError("OVERFLOW in hyp2f1");
	        	    return Double.POSITIVE_INFINITY;
	            }
	            y = true_gamma(c) * true_gamma(d) /
	                (true_gamma(p) * true_gamma(r));
	            if( err[0] > ETHRESH )
	    		{
	    		System.err.println("PRECISION LOSS in hyp2f1");
	    	    System.err.println( "Estimated err = " + err );
	    		}
	    	    return(y);
	        }
	        if (d <= -1.0) {
	        	MipavUtil.displayError("OVERFLOW in hyp2f1");
	     	    return Double.POSITIVE_INFINITY;	
	        }
	    }

	    /* Conditionally make d > 0 by recurrence on c
	     * AMS55 #15.2.27
	     */
	    if (d < 0.0) {
	        /* Try the power series first */
	        y = hyt2f1(a, b, c, x, err);
	        if (err[0] < ETHRESH) {
	    	    return(y);
	        }
	        /* Apply the recurrence if power series fails */
	        err[0] = 0.0;
	        aid = (int)(2 - id);
	        e = c + aid;
	        d2 = hyp2f1(a, b, e, x);
	        d1 = hyp2f1(a, b, e + 1.0, x);
	        q = a + b + 1.0;
	        for (i = 0; i < aid; i++) {
	            r = e - 1.0;
	            y = (e * (r - (2.0 * e - q) * x) * d2 + (e - a) * (e - b) * x * d1) /
	                (e * r * s);
	            e = r;
	            d1 = d2;
	            d2 = y;
	        }
	        if( err[0] > ETHRESH )
    		{
    		System.err.println("PRECISION LOSS in hyp2f1");
    	    System.err.println( "Estimated err = " + err );
    		}
	        return (y);
	    }

	    if (neg_int_ca_or_cb != 0) {
	    	/* negative integer c-a or c-b */
	    	y = Math.pow(s, d) * hys2f1(c - a, c - b, c, x, err);
	    	if( err[0] > ETHRESH )
			{
			System.err.println("PRECISION LOSS in hyp2f1");
		    System.err.println( "Estimated err = " + err );
			}
		    return (y);
	    }

	    y = hyt2f1(a, b, c, x, err);

		if( err[0] > ETHRESH )
		{
		System.err.println("PRECISION LOSS in hyp2f1");
	    System.err.println( "Estimated err = " + err );
		}
	    return (y);
	
	}
	
	/* Apply transformations for |x| near 1
	 * then call the power series
	 */
	public double hyt2f1(double a, double b, double c, double x, double loss[]) {
	double p, q, r, s, t, y, d;
	double ax, id, d1, d2, e, y1;
	int i, aid;

	double err[] = new double[] {0.0};
	double err1[] = new double[1];
	s = 1.0 - x;
	if( x < -0.5 )
		{
		if( b > a )
			y = Math.pow( s, -a ) * hys2f1( a, c-b, c, -x/s, err );

		else
			y = Math.pow( s, -b ) * hys2f1( c-a, b, c, -x/s, err );

		loss[0] = err[0];
		return(y);
		}

	d = c - a - b;
	id = Math.round(d);	/* nearest integer to d */

	if( x > 0.9 )
	{
	if( Math.abs(d-id) > EPS ) /* test for integer c-a-b */
		{
	/* Try the power series first */
		y = hys2f1( a, b, c, x, err );
		if( err[0] < ETHRESH ) {
			loss[0] = err[0];
			return(y);
		}
	/* If power series fails, then apply AMS55 #15.3.6 */
		q = hys2f1( a, b, 1.0-d, s, err );	
		q *= true_gamma(d) /(true_gamma(c-a) * true_gamma(c-b));
		r = Math.pow(s,d) * hys2f1( c-a, c-b, d+1.0, s, err1 );
		r *= true_gamma(-d)/(true_gamma(a) * true_gamma(b));
		y = q + r;

		q = Math.abs(q); /* estimate cancellation error */
		r = Math.abs(r);
		if( q > r )
			r = q;
		err[0] += err1[0] + (MACHEP*r)/y;

		y *= true_gamma(c);
		loss[0] = err[0];
		return(y);
		}
	else
		{
	/* Psi function expansion, AMS55 #15.3.10, #15.3.11, #15.3.12 */
		if( id >= 0.0 )
			{
			e = d;
			d1 = d;
			d2 = 0.0;
			aid = (int)id;
			}
		else
			{
			e = -d;
			d1 = 0.0;
			d2 = d;
			aid = (int)(-id);
			}

		ax = Math.log(s);

		/* sum for t = 0 */
		y = psi(1.0) + psi(1.0+e) - psi(a+d1) - psi(b+d1) - ax;
		y /= true_gamma(e+1.0);

		p = (a+d1) * (b+d1) * s / true_gamma(e+2.0);	/* Poch for t=1 */
		t = 1.0;
		do
			{
			r = psi(1.0+t) + psi(1.0+t+e) - psi(a+t+d1)
				- psi(b+t+d1) - ax;
			q = p * r;
			y += q;
			p *= s * (a+t+d1) / (t+1.0);
			p *= (b+t+d1) / (t+1.0+e);
			t += 1.0;
			}
		while( Math.abs(q/y) > EPS );


		if( id == 0.0 )
			{
			y *= true_gamma(c)/(true_gamma(a)*true_gamma(b));
			loss[0] = err[0];
			return(y);
			}

		y1 = 1.0;

		if( aid != 1 ) {

		t = 0.0;
		p = 1.0;
		for( i=1; i<aid; i++ )
			{
			r = 1.0-e+t;
			p *= s * (a+t+d2) * (b+t+d2) / r;
			t += 1.0;
			p /= t;
			y1 += p;
			}
		} // if (aid != 1)
	
		p = true_gamma(c);
		y1 *= true_gamma(e) * p / (true_gamma(a+d1) * true_gamma(b+d1));

		y *= p / (true_gamma(a+d2) * true_gamma(b+d2));
		if( (aid & 1) != 0 )
			y = -y;

		q = Math.pow( s, id );	/* s to the id power */
		if( id > 0.0 )
			y *= q;
		else
			y1 *= q;

		y += y1;
		loss[0] = err[0];
		return(y);
		}

	}

	/* Use defining power series if no special cases */
	y = hys2f1( a, b, c, x, err );

	loss[0] = err[0];
	return(y);
	}

	/* Defining power series expansion of Gauss hypergeometric function */

	public double hys2f1(double a, double b, double c, double x, double loss[]) {
    // hcephes version
	//double *loss; /* estimates loss of significance */
	double f, g, h, k, m, s, u, umax;
	int i;
	int ib, intflag = 0;

    if (Math.abs(b) > Math.abs(a)) {
        /* Ensure that |a| > |b| ... */
        f = b;
        b = a;
        a = f;
    }

    ib = (int)Math.round(b);

    if (Math.abs(b - ib) < EPS && ib <= 0 && Math.abs(b) < Math.abs(a)) {
        /* .. except when `b` is a smaller negative integer */
        f = b;
        b = a;
        a = f;
        intflag = 1;
    }

    if ((Math.abs(a) > Math.abs(c) + 1 || (intflag != 0)) && Math.abs(c - a) > 2 && Math.abs(a) > 2) {
        /* |a| >> |c| implies that large cancellation error is to be expected.
         *
         * We try to reduce it with the recurrence relations
         */
        return hcephes_hyp2f1ra(a, b, c, x, loss);
    }

    i = 0;
    umax = 0.0;
    f = a;
    g = b;
    h = c;
    s = 1.0;
    u = 1.0;
    k = 0.0;
    do {
        if (Math.abs(h) < EPS) {
            loss[0] = 1.0;
            return Double.POSITIVE_INFINITY;
        }
        m = k + 1.0;
        u = u * ((f + k) * (g + k) * x / ((h + k) * m));
        s += u;
        k = Math.abs(u); /* remember largest term summed */
        if (k > umax)
            umax = k;
        k = m;
        if (++i > MAX_ITERATIONS) { /* should never happen */
            loss[0] = 1.0;
            return (s);
        }
    } while (s == 0 || Math.abs(u / s) > MACHEP);

    /* return estimated relative error */
    loss[0] = (MACHEP * umax) / Math.abs(s) + (MACHEP * i);

    return (s);
	
	}
	
	/*
	 * Evaluate hypergeometric function by two-term recurrence in `a`.
	 *
	 * This avoids some of the loss of precision in the strongly alternating
	 * hypergeometric series, and can be used to reduce the `a` and `b` parameters
	 * to smaller values.
	 *
	 * AMS55 #15.2.10
	 */
	public double hcephes_hyp2f1ra(double a, double b, double c, double x, double loss[]) {
	    double f2, f1, f0;
	    int n;
	    double t, da;
	    double err[] = new double[1];

	    /* Don't cross c or zero */
	    if ((c < 0 && a <= c) || (c >= 0 && a >= c)) {
	        da = Math.round(a - c);
	    } else {
	        da = Math.round(a);
	    }
	    t = a - da;

	    loss[0] = 0;

	    if (Math.abs(da) > MAX_ITERATIONS) {
	        /* Too expensive to compute this value, so give up */
	       MipavUtil.displayError ("TOO EXPENSIVE TO COMPUTE hyp2f1");
	        loss[0] = 1.0;
	        return Double.NaN;
	    }

	    if (da < 0) {
	        /* Recurse down */
	        f2 = 0;
	        f1 = hys2f1(t, b, c, x, err);
	        loss[0] += err[0];
	        f0 = hys2f1(t - 1, b, c, x, err);
	        loss[0] += err[0];
	        t -= 1;
	        for (n = 1; n < -da; ++n) {
	            f2 = f1;
	            f1 = f0;
	            f0 =
	                -(2 * t - c - t * x + b * x) / (c - t) * f1 - t * (x - 1) / (c - t) * f2;
	            t -= 1;
	        }
	    } else {
	        /* Recurse up */
	        f2 = 0;
	        f1 = hys2f1(t, b, c, x, err);
	        loss[0] += err[0];
	        f0 = hys2f1(t + 1, b, c, x, err);
	        loss[0] += err[0];
	        t += 1;
	        for (n = 1; n < da; ++n) {
	            f2 = f1;
	            f1 = f0;
	            f0 = -((2 * t - c - t * x + b * x) * f1 + (c - t) * f2) / (t * (x - 1));
	            t += 1;
	        }
	    }

	    return f0;
	}
	
	/*
    15.4.2 Abramowitz & Stegun.
*/
public double hcephes_hyp2f1_neg_c_equal_bc(double a, double b, double x) {
    double k;
    double collector = 1;
    double sum = 1;
    double collector_max = 1;

    if (!(Math.abs(b) < 1e5)) {
        return Double.NaN;
    }

    for (k = 1; k <= -b; k++) {
        collector *= (a + k - 1) * x / k;
        collector_max = Math.max(Math.abs(collector), collector_max);
        sum += collector;
    }

    if (1e-16 * (1 + collector_max / Math.abs(sum)) > 1e-7) {
        return Double.NaN;
    }

    return sum;
}

}