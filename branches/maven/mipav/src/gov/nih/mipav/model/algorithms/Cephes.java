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
	
	public final static int CHDTR = 1;
	public final static int CHDTRC = 2;
	public final static int CHDTRI = 3;
	public final static int ELLIE = 4;
	public final static int ELLPE = 5;
	public final static int ELLPK = 6;
	public final static int ERF = 7;
	public final static int ERFC = 8;
	public final static int IGAM = 9;
	public final static int IGAMI = 10;
	public final static int IGAMC = 11;
	public final static int NDTR = 12;
	public final static int NDTRI = 13;
	public final static int POLEVL = 14;
	public final static int P1EVL = 15;
	public final static int STIRF = 16;
	public final static int STRUVE = 17;
	public final static int TRUE_GAMMA = 18;
	public final static int ZETA = 19;
	public final static int ZETAC = 20;
	// For IEEE arithmetic (IBMPC):
    private final static double MACHEP =  1.11022302462515654042E-16; // 2**-53
    private final static double MAXLOG =  7.09782712893383996843E2;   // log(2**1024)
    private final static double MINLOG = -7.08396418532264106224E2;   // log(2**-1022)
    private final static double MAXNUM =  Double.MAX_VALUE; // 1.7976931348623158E308 2**1024
    private final static double big = 4.503599627370496e15;
	private final static double biginv =  2.22044604925031308085e-16;
	/* sqrt(2pi) */
	private final static double s2pi = 2.50662827463100050242E0;
	private final static double PIO2   =  1.57079632679489661923; // pi/2
	private final static double SQRTH  =  7.07106781186547524401E-1; // sqrt(2)/2
	private final static int MAXL2 = 127;
	private final static double MAXSTIR = 143.01608;
	private final static double SQTPI = 2.50662827463100050242E0;
	private final static double stop = 1.37e-17;
	private final boolean DEBUG = false;
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
	
	private double result[];
	
	private int version;
	
	private double par1;
	
	private double par2;
	
	private double[] par3;
	
	private int par4;
	
	public void testCephes() {
		// The test for chdtr(4,5) passed
		// The test for chdtrc(4,5) passed
		// The test for chdtri(4,0.3) passed
		// The test for ellie(-5.3, 0.12) passed
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
		// The test for igam(0.5,0) passed
		// The test for igam(1,2) passed
		// lowerIncompleteGamma = 23.297935486152934
		// upperIncompleteGamma = 0.7020645138470673
		// regularizedGammaP = 0.9707473119230389
		// The test for igam(5,10) passed
		// lowerIncompleteGamma = 4.790423305785542E155
		// upperIncompleteGamma = 4.542198238608868E155
		// regularizedGammaP = 0.5132987856625221
		// Calculated answer for igam(100,100) = 0.5132987982791313
		// Correct answer for igam(100,100) = 0.5132987856625221
		// The test for igmac(2,1) passed
		// The test for igami(2,0.3) passed
		// The test for ndtr(0.0) passed
		// The test for ndtr(0.3) passed
		// The test for ndtr(1) passed
		// The test for ndtri(0.5) passed
		// The test for ndtri(0.6) passed
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
	    
	    result[0] = ellie(-5.3, 0.12);
	    if (Math.abs(result[0] + 5.12290521194) < 1.0E-7) {
	    	System.out.println("The test for ellie(-5.3, 0.12) passed");
	    }
	    else {
	    	System.out.println("The test for ellie(-5.3, 0.12) failed");
	    	System.out.println("Implemented ellie gave " + result[0]);
	    	System.out.println("Correct answer is -5.12290521194");
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
	    System.out.println("Calculated answer for igam(100,100) = " + result[0]);
	    System.out.println("Correct answer for igam(100,100) = " + regularizedGammaP[0]);
	    
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
	
	public Cephes(double par1, double par2, int version, double result[]) {
		this.par1 = par1;
		this.par2 = par2;
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
		if (version == CHDTR) {
			result[0] = chdtr(par1, par2);
		}
	    else if (version == CHDTRC) {
	    	result[0] = chdtrc(par1, par2);
		}
	    else if (version == CHDTRI) {
	    	result[0] = chdtri(par1, par2);
		}
	    else if (version == ELLIE) {
	    	result[0] = ellie(par1, par2);
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
		else if (version == IGAMI) {
			result[0] = igami(par1, par2);
		}
		else if (version == IGAMC) {
			result[0] = igamc(par1, par2);
		}
		else if (version == IGAM) {
			result[0] = igam(par1,par2);
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
		
		double ansG[] = new double[1];
		Gamma gam = new Gamma(a, 0, ansG);
		gam.run();
		lgm = ansG[0];

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

	double ansG[] = new double[1];
	Gamma gam = new Gamma(a, 0, ansG);
	gam.run();
	double lgm = ansG[0];
	ax = a * Math.log(x) - x - lgm;
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
	double ansG[] = new double[1];
	Gamma gam = new Gamma(a, 0, ansG);
	gam.run();
	double lgm = ansG[0];
	ax = a * Math.log(x) - x - lgm;
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


}