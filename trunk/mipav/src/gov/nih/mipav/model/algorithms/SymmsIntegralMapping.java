package gov.nih.mipav.model.algorithms;

import java.io.File;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.util.Scanner;

import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.jama.GeneralizedEigenvalue;
import gov.nih.mipav.model.structures.jama.LinearEquations2;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;

public class SymmsIntegralMapping extends AlgorithmBase  {
	String fileDir;
	
	// EPS returns the distance from 1.0 to the next larger double-precision number, that is, eps = 2^-52.
    private double EPS;
    // Filename to receive FORTRAN output code
    private String FORTFL = null;
    // Does the domain have any symmetry?
    private boolean SYMTY;
    // If SYMTY is true, are there any reflectional symmetries?
    private boolean REFLN;
    // If SYMTY is true, what are the coordinates of the center of symmetry?
    private final int MNARC = 100;
    private double CENSY[] = new double[2];
    // If SYMTY is true, number of arcs on the fundamental boundary section
    // If SYMTY is false, number of arcs on the boundary
    // NARCS <= MNARC-1 = 99
    private int NARCS;
    // NUMDER is of length MNARC
    // NUMDER is initially false
    private boolean NUMDER[] = new boolean[MNARC];
    // ARCTY is of length MNARC
    // Type of arc, 1 = LINE SEGMENT, 2 = CIRCULAR ARC SEGMENT, 3 = CARTESIAN PARAMETRIC FUNCTION
    // 4 = POLAR FUNCTION
    private int ARCTY[] = new int[MNARC];
    // STAPT is of length MNARC,2
    // Initial point on line, initial point on circle, or initial point on curve
    // IF (SYMTY) THEN
    // WRITE(*,*) 'COORDINATES OF FINAL POINT ON THIS ARC?'
    //STAPT[NARCS]=CMPLX(FINAL X point, FINAL Y point)
    //ELSE
    // STAPT[NARCS]=STAPT[1]
    //ENDIF
    private double STAPT[][] = new double[MNARC][2];
    // Start with GMCO = 0
    // For types 2, 3, and 4 increment GMCO and put
    //GMCO=GMCO+1
	//PGM[IA-1]=GMCO where IA is the number of the curve from 1 to NARCS
    // For type 2 circular arc:
    // RGM[GMCO-1]= X center of circle
	// GMCO=GMCO+1
	// RGM[GMCO-1]= Y center of circle
	//GMCO=GMCO+1
	//RGM[GMCO-1]=ALPHA*PI where alpha is the signed angle subtended at center in units of PI
    // ALPHA is positive for CCW and negative for CW.
    // For type 3 CARTESIAN PARAMETRIC FUNCTION:
    // RGM[GMCO-1]= initial parameter value
    // GMCO=GMCO+1
    // RGM[GMCO-1]= final parameter value
    // For type 4 polar function:
    // RGM[GMCO-1]= (initial polar angle in units of PI) *PI
    // GMCO=GMCO+1
    // RGM[GMCO-1]= (final polar angle in units of PI) *PI
    // PGM is of length MNARC
    private int PGM[] = new int[MNARC];
    // RGM is of length 3*MNARC
    private double RGM[] = new double[3*MNARC];
    // Start with TXCO = 0
    // For types 3 and 4
    // For each IA = 1 to NARCS do J = 1,2
    //TXCO=TXCO+1
    //PTX[IA-1+(J-1)*MNARC]=TXCO
    // DEFN[TXCO-1][0]= TXT for real part
    // DEFN[TXCO-1][1] = TXT for imaginary part
    // where TXT = A COMPLEX EXPRESSION for TYPE 3 and a REAL EXPRESSION FOR TYPE 4
    // for J = 1 and TYPE = 3 JAVA EXPRESSION FOR PARFUN
    // for J = 2 and TYPE = 3 JAVA EXPRESSION FOR DPARFN
    // for J = 1 and TYPE = 4 JAVA EXPRESSION FOR RADIUS
    // for J = 2 and TYPE = 4 JAVA EXPRESSION FOR RADIUS DERIVATIVE
    // PTX is of length 2*MNARC
    private int PTX[] = new int[2*MNARC];
    private int NTX[]= new int[2*MNARC];
    //CHARACTER DEFN(MNARC*2)*72
    // Holds text for real for types 3 and 4 and imaginary parts for type 3
    // Start imaginary text with ui.  All text following ui is imaginary.
    String DEFN[]= new String[2*MNARC];
    private boolean traditionalInput = false;
    Scanner input = new Scanner(System.in);
    private double zzset[][] = new double[400][2];
    private int IBNDS[] = new int[5];
    private int IGEOM[] = new int[4]; // Written in original JAPHYC
    private int PARNT[] = new int[IBNDS[0]]; // Written in original JAPHYC
    private double RGEOM[] = new double[2]; // Written in original JAPHYC
    private double HALEN[]= new double[IBNDS[0]]; // Written in original JAPHYC
	private double MIDPT[] = new double[IBNDS[0]]; // Written in original JAPHYC
	private double VTARG[] = new double[IBNDS[0]]; // Written in original JPAHYC
    private int ISNPH[] = new int[6]; // Written in original JAPHYC
    private int DGPOL[] = new int[IBNDS[0]]; // Written in original JAPHYC
	private int JATYP[] = new int[IBNDS[0]]; // Written in original JAPHYC
	private int LOSUB[] = new int[IBNDS[0]]; // Written in original JAPHYC
	// Parts of RSNPH
	private int NQPTS;
	private int NJIND = NARCS + 1;
	private int TNGQP = NQPTS * NJIND;
	private int MNEQN;
	private double ACOEF[] = new double[TNGQP]; // Written in original JAPHYC
	private double BCOEF[] = new double[TNGQP]; // Written in original JAPHYC
	private double AICOF[] = new double[TNGQP]; // Written in original JAPHYC
	private double BICOF[] = new double[TNGQP]; // Written in original JAPHYC
	private double QUPTS[] = new double[TNGQP]; // Written in original JAPHYC
	private double QUWTS[] = new double[TNGQP]; // Written in original JAPHYC
	private double H0VAL[] = new double[NJIND]; // Written in original JAPHYC
	private double HIVAL[] = new double[NJIND]; // Written in original JAPHYC
	private double JACIN[] = new double[NJIND]; // Written in original JAPHYC
	private double ERARC[] = new double[IBNDS[0]]; // Written in original JAPHYC
	private double BCFSN[] = new double[MNEQN]; // Written in original JAPHYC
	private double SOLUN[] = new double[MNEQN]; // Written in original JAPHYC
	// Parts of IWORK
	private int IPIVT[] = new int[MNEQN];
	private int LOQSB[] = new int[IBNDS[1]];
	private int NQUAD[] = new int[IBNDS[1]];
	private int HISUB[] = new int[IBNDS[0]];
	private int LOTES[] = new int[IBNDS[0]];
	private int HITES[] = new int[IBNDS[0]];
	private int AXION[] = new int[IBNDS[0]];
	private int NEWDG[] = new int[IBNDS[0]];
	private int ICOPY[] = new int[IBNDS[0]];
	private int LOOLD[] = new int[IBNDS[0]];
	private int HIOLD[] = new int[IBNDS[0]];
	// Parts of RWORK
	private double WORK2[] = new double[MNEQN];
	private double COLPR[] = new double[MNEQN];
	private double A1COF[] = new double[IBNDS[1]];
	private double B1COF[] = new double[IBNDS[1]];
	private double TOLOU[] = new double[IBNDS[1]];
	private double XIDST[] = new double[2*IBNDS[1]];
	private double XENPT[] = new double[IBNDS[2]];
	private double QCOMX[] = new double[IBNDS[3]];
	private double QCOMW[] = new double[IBNDS[3]];
	private double RCOPY[] = new double[IBNDS[0]];
	private double NEWHL[] = new double[IBNDS[0]];
	private double AQCOF[] = new double[TNGQP];
	private double BQCOF[] = new double[TNGQP];
	private double CQCOF[] = new double[TNGQP];
	private double COLSC[] = new double[TNGQP];
	private double RIGLL[] = new double[TNGQP];
	private double WORK[] = new double[2*NQPTS];
	private double DIAG[] = new double[NQPTS];
	private double SDIAG[] = new double[NQPTS];
	private double WORKT[] = new double[2*NQPTS*NQPTS];
	private double WORKQ[] = new double[NQPTS*NQPTS];
	// Parts of ZWORK
	private double ZCOLL[][] = new double[MNEQN][2];
	private double XIVAL[][] = new double[2*IBNDS[1]][2];
	// Parts of LWORK
	private boolean NEWQU[] = new boolean[IBNDS[1]];
	private boolean LCOPY[] = new boolean[IBNDS[0]];
	private boolean PNEWQ[] = new boolean[IBNDS[0]];
	private boolean LNSEG[] = new boolean[IBNDS[0]];
	
//  CENTR  - COMPLEX
//                     THE POINT IN THE PHYSICAL PLANE THAT IS TO BE
//                     MAPPED TO THE CENTRE OF THE UNIT DISC.  FOR
//                     EXTERIOR DOMAINS CENTR MUST BE SOME POINT IN THE
//                     COMPLEMENTARY INTERIOR  PHYSICAL DOMAIN.
//                     IN CASE ABS(ISYGP).GT.1 THEN CENTR MUST ALSO BE
//                     A CENTRE OF SYMMETRY FOR THE PHYSICAL DOMAIN.
	private double CENTR[] = new double[2]; // Written in original JAPHYC
//  INTER  - LOGICAL
//                     TRUE IF THE PHYSICAL DOMAIN IS INTERIOR, FALSE 
//                     OTHERWISE.
	private boolean INTER; // Written in in original JAPHYC
	
	//COMMON /FNDEF/
	private double BETA;
	private double A1;
	private double B1;
	private double P0VAL;
	private double SCALE;
	private int TYPE;
	
	// From GQPHYC:
	// IQUPH  - INTEGER ARRAY
	//                     AN INTEGER VECTOR OF SIZE AT LEAST 2*IBNDS(1) + 4,
	//                     WHERE IBNDS(1) (=IGEOM(4)) IS THE VALUE PREVIOUSLY
	//                     SUPPLIED TO JAPHYC; IQUPH STORES POINTERS TO 
	//                     ACCESS RQUPH AND ZQUPH.
	private int IQUPH[] = new int[4];
	private int LQSBF[] = new int[IBNDS[0]];
  	private int NPPQF[] = new int[IBNDS[0]];
//  MQUPH  - INTEGER
//                     THE MAXIMUM NUMBER OF QUADRATURE POINTS ALLOWED
//                     IN THE FINAL GLOBAL RULE.  (THE VALUE OF THIS
//                     ARGUMENT IS LINKED TO THOSE OF ARGUMENTS NQPTS
//                     AND IBNDS(1) PREVIOUSLY SUPPLIED TO JAPHYC VIA
//                     MQUPH <= (MQIN1-1)*NQPTS*IBNDS(1))
  	private int MQUPH;
  	// RQUPH  - REAL ARRAY
	//                     A REAL VECTOR OF SIZE AT LEAST 3*MQUPH + 1; STORES
	//                     THE REAL QUADRATURE DATA.
  	private double RQUPH[] = new double[1];
  	private double TPPQF[] = new double[MQUPH];
   	private double TRRAD[] = new double[MQUPH];
   	private double WPPQF[] = new double[MQUPH];
//  ZQUPH  - COMPLEX ARRAY
//                     A COMPLEX VECTOR OF SIZE AT LEAST MQUPH + 1; 
//                     STORES THE QUADRATURE POINTS ON THE PHYSICAL 
//                     BOUNDARY.
   	private double FACTR[] = new double[2]; // At first location of ZQUPH
  	private double ZPPQF[][] = new double[MQUPH][2];
	public SymmsIntegralMapping() {
		
	}
	
	public SymmsIntegralMapping(ModelImage destImg, ModelImage srcImg, String FORTFL, boolean SYMTY,
			boolean REFLN, double CENSY[], int NARCS, boolean NUMDER[], int ARCTY[], double STAPT[][],
			int PGM[], double RGM[], int PTX[], int NTX[], String DEFN[]) {
	    super(destImg, srcImg);
	    this.FORTFL = FORTFL;
	    this.SYMTY = SYMTY;
	    this.REFLN = REFLN;
	    this.CENSY = CENSY;
	    this.NARCS = NARCS;
	    this.NUMDER = NUMDER;
	    this.ARCTY = ARCTY;
	    this.STAPT = STAPT;
	    this.PGM = PGM;
	    this.RGM = RGM;
	    this.PTX = PTX;
	    this.NTX = NTX;
	    this.DEFN = DEFN;
	}
	
	public void runAlgorithm() {
	    fileDir = srcImage.getFileInfo(0).getFileDirectory();
	    
	 // eps returns the distance from 1.0 to the next larger double-precision number, that is, eps = 2^-52.
    	// epsilon = D1MACH(4)
        // Machine epsilon is the smallest positive epsilon such that
        // (1.0 + epsilon) != 1.0.
        // epsilon = 2**(1 - doubleDigits) = 2**(1 - 53) = 2**(-52)
        // epsilon = 2.2204460e-16
        // epsilon is called the largest relative spacing
        EPS = 1.0;
        double neweps = 1.0;


        while (true) {

            if (1.0 == (1.0 + neweps)) {
                break;
            } else {
                EPS = neweps;
                neweps = neweps / 2.0;
            }
        } // while(true)
	}
	
	public void PARGEN() {
	//.......................................................................
	//     AN INTERACTIVE PREPROCESSOR TO HELP THE  C O N F P A C K  USER 
	//     GENERATE THE FORTRAN CODE DEFINING THE BOUNDARY PARAMETRISATION 
	//     AND ITS DERIVATIVE.
	//
	//     THE FOLLOWING CONVENTIONS ARE ASSUMED:
	//
	//  ** IF A QUESTION REQUIRES A YES OR NO ANSWER, THE DETECTION OF 'Y' OR
	//     'y' IN THE FIRST 6 INPUT CHARACTERS IS TAKEN AS YES, ANYTHING ELSE
	//     AS NO.
	//
	//  ** WHEN ASKED FOR COORDINATES, THIS ALWAYS MEANS CARTESIAN COORDIN-
	//     ATES AND THESE SHOULD BE SUPPLIED AS TWO REAL NUMBERS, AS EITHER 
	//
	//                      X,Y     OR      X Y
	//
	//     WITHOUT PARENTHESES. 
	//
	//  ** FOUR TYPES OF ARCS ARE CURRENTLY TREATED, WITH NUMERICAL CODES TO 
	//     DENOTE THE TYPE AS FOLLOWS:
	//       1:= LINE SEGMENT.
	//       2:= CIRCULAR ARC SEGMENT.
	//           CONVENTIONS:
	//           1 - THE ANGLE SUBTENDED AT THE CENTRE IS POSITIVE FOR
	//               ANTICLOCKWISE TRAVERSAL OF THE ARC AND NEGATIVE FOR
	//               CLOCKWISE TRAVERSAL.
	//       3:= THE USER IS ASKED TO SUPPLY THE FORTRAN 77 ARITHMETIC 
	//           EXPRESSIONS WHICH DEFINE THE CARTESIAN PARAMETRIC FUNCTION  
	//           AND THE DERIVATIVE OF THIS FUNCTION.
	//           CONVENTIONS:
	//           1 - THE PARAMETER MUST BE DENOTED BY T.
	//           2 - THE REAL CONSTANT PI=3.14159.. AND THE COMPLEX CONSTANT
	//               UI=(0.0,1.0) MAY BE USED IN THE ARITHMETIC EXPRESSIONS.
	//       4:= THE USER IS ASKED TO SUPPLY THE FORTRAN 77 ARITHMETIC 
	//           EXPRESSIONS WHICH DEFINE THE POLAR COORDINATE AS A FUNCTION
	//           OF POLAR ANGLE AND THE DERIVATIVE OF THIS FUNCTION.
	//           CONVENTIONS:
	//           1 - THE POLAR ANGLE MUST BE DENOTED BY T.
	//           2 - THE REAL CONSTANT PI=3.14159.. MAY BE USED IN THE ARITH-
	//               METIC EXPRESSIONS. 
	//           3 - PARGEN ASSIGNS THE EXPRESSION FOR THE RADIUS TO THE 
	//               COMPLEX VARIABLE ZRAD;  IF REQUIRED THE USER MAY THERE- 
	//               FORE USE THE VARIABLE ZRAD IN THE EXPRESSION FOR THE 
	//               DERIVATIVE OF THE RADIUS WRT POLAR ANGLE
	//      IN ADDITION, FOR TYPES 3 AND 4, THE FOLLOWING CONVENTIONS HOLD:
	//           1 - ONLY USE UP TO 66 CHARACTERS PER LINE.
	//           2 - IF THE EXPRESSION OCCUPIES MORE THAN ONE LINE THEN THERE
	//               IS NO NEED TO SUPPLY ANY CONTINUATION CHARACTER.
	//           3 - ONLY USE THOSE FORTRAN 77 INTRINSIC MATHS FUNCTIONS  
	//               WHICH ACCEPT COMPLEX ARGUMENTS AND ARE ANALYTIC; I.E.,
	//               IN STANDARD FORTRAN,
	//                    SQRT, EXP, LOG, SIN, COS
	//           4 - THE WHOLE EXPRESSION SHOULD BE TERMINATED WITH A
	//               REPEATED DIVISION SIGN, I.E. //.
	//
	// ** THE CODE ISN'T VERY ROBUST IN THAT THERE IS LITTLE PROVISION FOR
	//    INTERACTIVE CORRECTION OF ERRORS.  HOWEVER, ALL THE USER'S INPUT
	//     IS AUTOMATICALLY OUTPUT TO A FILE NAMED pgenin.  IF THE USER
	//     REALISES THAT AN ITEM HAS BEEN INPUT INCORRECTLY, THE BEST POLICY
	//     IS TO CARRY ON TO THE END OF THE INPUT PHASE AND THEN TERMINATE 
	//     THE EXECUTION;  THE FILE pgenin CAN BE EDITED, RENAMED AND 
	//     SUBMITTED AS STANDARD INPUT FOR A SECOND RUN OF PARGEN.   
	//     (THE SUGGESTION TO RENAME IS TO AVOID ANY POSSIBLE DIFFICULTY 
	//    ARISING FROM READING THE FILE AS STANDARD INPUT WHILST ALSO 
	//     WRITING OUTPUT TO THE SAME FILE.) 
	//     IF THE USER REALISES ONLY AT A LATER STAGE (E.G. IN PLOTTING THE 
	//     BOUNDARY) THAT AN ITEM HAS BEEN INPUT INCORRECTLY THEN pgenin 
	//     MAY STILL BE AVAILABLE FOR RE-USE AS ABOVE.
	//  
	//     SUBROUTINES OR FUNCTIONS NEEDED
	//              - THE CONFPACK LIBRARY.
	//              - THE REAL FUNCTION R1MACH.
	//
	//.......................................................................
	//     AUTHOR: DAVID HOUGH, COVENTRY POLYTECHNIC, UK
	//     LAST UPDATE: 15 FEB 1991
	//.......................................................................
	//
	//     LOCAL VARIABLES
	//
	      int GMCO,IA,I,J,L,SW,
	          TXCO;
	      int TYPE = 1;
	      int IER[] = new int[1];
	      int ORDRG[] = new int[1];
	      int ORDSG[] = new int[1];
	      double ALPHA,PI,X,Y;
	      //COMPLEX CENSY,RTUNI,U2
	      double RTUNI[] = new double[2];
	      double U2[] = new double[2];
	     // CHARACTER TXT*72,TABC*6,FORTFL*72,CH*2,SIG(10)*2,WID(10)*2,REDD*6,
	     //+FMT1*8,FMT2*9
	      String TXT;
	      String CH;
	      String SIG[] = new String[]{"7","8","9","10","11","12","13","14","15","16"};
	      String WID[] = new String[]{"15","16","17","18","19","20","21","22","23","24"};
	      String REDD;
	      //String FMT1;
	      //String FMT2;
	
	      //PARAMETER (MNARC=100,TABC='     +',CHNL=20,CHIN=21)
	      final String TABC = "     +";
	      final int CHNL = 20;
	      //final int CHIN = 21;
	      
	      File file;
	      RandomAccessFile raFile = null;
	      boolean validInput;
	
	      //EXTERNAL CHRIN,HEADER,R1MACH,SYINF1,WRFUN1,WRFUN2,WRHEAD,WRSYM1,
	      //+WRSYM2,WRSYM3,WRTAIL
	
	      WRHEAD(6,0, null);
	
	      PI= Math.PI;
	      
	      //**** DETERMINE NUMBER OF SIGNIFICANT FIGURES REQUIRED TO MATCH MACHINE 
          //**** PRECISION AND SET UP POINTER SW TO SIG AND WID
	
	      SW=(int)(-Math.log10(EPS))+2;
	      if (SW <= 7) {
	        SW=1;
	      }
	      else if (SW >= 16) {
	        SW=10;
	      }
	      else {
	        SW=SW-6;
	      }
	      
	  	  //**** SET UP THE EDIT DESCRIPTOR AND FORMAT SPECIFICATION FOR FLOATING 
	  	  //**** POINT OUTPUT
	  	
	  	  REDD="E"+WID[SW-1]+"."+SIG[SW-1]; 
	  	  //FMT1="("+REDD+")";
	  	  //FMT2="(2"+REDD+")";
	  	  
	  	  if (traditionalInput) {
	  	      System.out.println("ENTER FILENAME TO RECEIVE OUTPUT JAVA CODE");  
	  	      FORTFL = input.next();
	  	  } // if (traditionalInput)
	      
	      //**** WRITE THE SOURCE CODE FOR PARFUN
	      file = new File(fileDir + FORTFL);
	      try {
	          raFile = new RandomAccessFile(file, "rw");
	      }
	      catch (IOException e) {
	    	  MipavUtil.displayError("IOException " + e + " on raFile = new RandomAccessFile(file, rw)");
	    	  System.exit(-1);
	      }
	      // Necessary so that if this is an overwritten file there isn't any
	      // junk at the end
	      try {
	          raFile.setLength(0);
	      }
	      catch (IOException e) {
	    	  MipavUtil.displayError("IOException " + e + " on raFile.setLength(0)");
	    	  System.exit(-1);  
	      }

	      //OPEN(CHNL,FILE=FORTFL)
	      
	      if (traditionalInput) {
	    	  validInput = false;
	    	  while (!validInput) {
	    	      System.out.println("DOES THE DOMAIN HAVE ANY SYMMETRY (Y/N)?");
	    	      String sym = input.next();
	    	      String firstSym = sym.substring(0,1);
	    	      if (firstSym.equalsIgnoreCase("Y")) {
	    	    	  SYMTY = true;
	    	    	  validInput = true;
	    	      }
	    	      else if (firstSym.equalsIgnoreCase("N")) {
	    	    	  SYMTY = false;
	    	    	  validInput = true;
	    	      }
	    	      else {
	    	    	  System.out.println(sym + " is not a valid response");
	    	      }
	    	  } // while (!validInput)
	    	  
	    	  if (SYMTY) {
	    		  validInput = false;
		    	  while (!validInput) {
		    	      System.out.println("ARE THERE ANY REFLECTIONAL SYMMETRIES (Y/N)?");
		    	      String ref = input.next();
		    	      String firstRef = ref.substring(0,1);
		    	      if (firstRef.equalsIgnoreCase("Y")) {
		    	    	  REFLN = true;
		    	    	  validInput = true;
		    	      }
		    	      else if (firstRef.equalsIgnoreCase("N")) {
		    	    	  REFLN = false;
		    	    	  validInput = true;
		    	      }
		    	      else {
		    	    	  System.out.println(ref + " is not a valid response");
		    	      }
		    	  } // while (!validInput)	
		    	  
		    	  System.out.println("What are the coordinates of the center of symmetry (X Y)?");
		    	  CENSY[0] = input.nextDouble();
		    	  CENSY[1] = input.nextDouble();
		    	  validInput = false;
		    	  while (!validInput) {
		    	      System.out.println("How many arcs are there on the fundamental boundary section?");
		    	      NARCS = input.nextInt();
		    	      if (NARCS <= MNARC-1) {
		    	    	  validInput = true;
		    	      }
		    	      else {
		    	    	  System.out.println("NARCS must be <= " + (MNARC-1));
		    	      }
		    	  } // while (!validInput)
	    	  } // if (SYMTY)
	    	  else { // !SYMTY
	    		  validInput = false;
		    	  while (!validInput) {
		    		  System.out.println("How many arcs are there on the boundary?");
		    		  NARCS = input.nextInt();
		    		  if (NARCS <= MNARC-1) {
		    	    	  validInput = true;
		    	      }
		    	      else {
		    	    	  System.out.println("NARCS must be <= " + (MNARC-1));
		    	      }
		    	  } // while (!validInput)
	    	  } // else !SYMTY
	    	  
	    	  GMCO = 0;
	    	  TXCO = 0;
	    	  
	    	  for (IA = 1; IA <= NARCS; IA++) {
	    		    NUMDER[IA-1] = false; 
	    		    validInput = false;
	    		    while (!validInput) {
	    		        System.out.println("ENTER THE TYPE OF ARC(1-4) for ARC NUMBER " + IA);
	    		        TYPE = input.nextInt();
	    		        if ((TYPE >= 1) && (TYPE <= 4)) {
	    		        	validInput = true;
	    		        }
	    		        else {
	    		        	System.out.println("TYPE MUST BE BETWEEN 1 and 4");
	    		        }
	    		    } // while (!validInput)
	    		    if (TYPE == 1) {
	    		        ARCTY[IA-1] = TYPE;	
	    		        System.out.println("What are the coordinates of the initial point on the line (X Y)?");
	    		        STAPT[IA-1][0] = input.nextDouble();
	    		        STAPT[IA-1][1] = input.nextDouble();
	    		    } // if (TYPE == 1)
	    		    else if (TYPE == 2) {
	    		    	ARCTY[IA-1] = TYPE;	
	    		        System.out.println("What are the coordinates of the initial point on the circle (X Y)?");
	    		        STAPT[IA-1][0] = input.nextDouble();
	    		        STAPT[IA-1][1] = input.nextDouble();
	    		        System.out.println("What are the coordinates of the center of the circle (X Y)?");
	    		        X = input.nextDouble();
	    		        Y = input.nextDouble();	
	    		        System.out.println("What is the signed angle subtended at center (in units of PI)?");
	    		        ALPHA = input.nextDouble();
	    		        GMCO=GMCO+1;
		                PGM[IA-1]=GMCO;
		                RGM[GMCO-1]=X;
		                GMCO=GMCO+1;
		                RGM[GMCO-1]=Y;
		                GMCO=GMCO+1;
		                RGM[GMCO-1]=ALPHA*PI;
	    		    } // else if (TYPE == 2)
	    		    else if ((TYPE == 3) || (TYPE == 4)) {
	    		    	ARCTY[IA-1] = TYPE;	
	    		        System.out.println("What are the coordinates of the initial point on the curve (X Y)?");
	    		        STAPT[IA-1][0] = input.nextDouble();
	    		        STAPT[IA-1][1] = input.nextDouble();
	    		        if (TYPE == 3) {
	    		        	System.out.println("Enter the initial and final parameter values (X Y)");
	    		        }
	    		        else {
	    		        	System.out.println("Enter the initial and final polar values (in angles of PI) (X Y)");
	    		        }
	    		        X = input.nextDouble();
	    		        Y = input.nextDouble();
	    		        GMCO=GMCO+1;
		                PGM[IA-1]=GMCO;
		                if (TYPE == 4) {
		                  RGM[GMCO-1]=X*PI;
		                  GMCO=GMCO+1;
		                  RGM[GMCO-1]=Y*PI;
		                }
		                else {
		                  RGM[GMCO-1]=X;
		                  GMCO=GMCO+1;
		                  RGM[GMCO-1]=Y;
		                }
		                for (J = 1; J <= 2; J++) {
		                	if (J == 1 && TYPE == 3) {
		                        System.out.println("ENTER JAVA EXPRESSION ENDING IN // FOR PARFUN");
		                        System.out.println("PUT REAL PART ui IMAGINARY PART");
		                	}
		                	else if (J == 2 && TYPE == 3) {          
		                        System.out.println("ENTER JAVA EXPRESSION ENDING IN // FOR DPARFN");
		                        System.out.println("PUT REAL PART ui IMAGINARY PART");
		                	}
		                	else if (J == 1 && TYPE == 4) {
		                        System.out.println("ENTER JAVA EXPRESSION ENDING IN // FOR RADIUS");
		                	}
		                	else {
		                        System.out.println("ENTER JAVA EXPRESSION ENDING IN // FOR RADIUS DERIVATIVE");
		                	}
		      
		                    TXCO=TXCO+1;
		                    PTX[IA-1+(J-1)*MNARC]=TXCO;
		                    I=1;
	
		                    TXT = input.next();
		                    L = -1;
		                    while (L == -1) {
			                    L = TXT.indexOf("//");
			                    if (L == -1) {
			                    	DEFN[TXCO-1]=TABC+TXT;
			                        I=I+1;
			                        TXCO=TXCO+1;
			                    } // if (L == -1)
		                    } // while (L == -1)
		                    NTX[IA-1+(J-1)*MNARC]=I;
		                    if (L == 0) {
		                        DEFN[TXCO-1]=TABC;
		                        NUMDER[IA-1] = true;
		                    }
		                    else {
		                        DEFN[TXCO-1]=TABC+TXT.substring(0,L);
		                    }
                            if ((J == 1) && (TYPE == 4)) {
                            	System.out.println("(... = ZRAD)");
                            }
		                } // for (J = 1; J <= 2; J++)
	    		    } // else if ((TYPE == 3) || (TYPE == 4))
	    	  } // for (IA = 1; IA <= NARCS; IA++)
	    	  
	    	  if (SYMTY) {
	              System.out.println("ENTER THE COORDINATES OF FINAL POINT ON THIS LAST ARC (X Y)");
	              STAPT[NARCS][0] = input.nextDouble();
	              STAPT[NARCS][1] = input.nextDouble();
	    	  }
	    	  else {
	              STAPT[NARCS][0]=STAPT[0][0];
	              STAPT[NARCS][1]=STAPT[0][1];
	    	  }
              
	    	  validInput = false;
  		      while (!validInput) {
	    	      System.out.println("END OF INPUT PHASE; CONTINUE WITH PROCESSING (Y/N)?");
	    	      String term = input.next();
	    	      String firstTerm = term.substring(0,1);
	    	      if (firstTerm.equalsIgnoreCase("Y")) {
	    	    	  validInput = true;
	    	      }
	    	      else if (firstTerm.equalsIgnoreCase("N")) {
	    	    	  validInput = true;
	    	    	  setCompleted(false);
	    	    	  return;
	    	      }
	    	      else {
	    	    	  System.out.println(term + " is not a valid response");
	    	      }
	    	  } // while (!validInput)
	      } // if (traditionalInput)
	      HEADER("PARFUN",REDD,raFile);
	      if (SYMTY) {
	        SYINF1(ORDRG,ORDSG,RTUNI,U2,REFLN,CENSY,STAPT[0],
	                   STAPT[NARCS],IER);
	        if (IER[0] > 0) {
	        	WRTAIL(6,0,IER[0],null);
	        	return;
	        }
	        System.out.println("\nN O T E : THE ORDER OF THE SYMMETRY GROUP IS " + ORDSG);
	        if (REFLN) {
	            System.out.println("          ISYGP = " + (-ORDSG[0]));	
	        }
	        else {
	        	System.out.println("          ISYGP = " + (ORDSG[0]));		
	        }
	        WRSYM1(NARCS,ORDRG[0],ORDSG[0],RTUNI,U2,CENSY,REFLN,true,REDD,CHNL,raFile);
	        if (REFLN) {
	          CH = "TS";
	        }
	        else {
	          CH = "TT";
	        }
	        WRFUN1(NARCS,STAPT,ARCTY,PGM,RGM,PTX,NTX,DEFN,CHNL,
	                 "IB",CH,"ZETA  ",REDD, raFile);
	        WRSYM2(NARCS,ORDRG[0],CENSY,REFLN,CHNL, raFile);
	      }
	      else {
	        WRFUN1(NARCS,STAPT,ARCTY,PGM,RGM,PTX,NTX,DEFN,CHNL,
	               "IA","TT","PARFUN",REDD,raFile);
	      }
	
	      try {
	          raFile.writeBytes("//\n");
	          raFile.writeBytes("}\n");
	  
	         // **** WRITE THE SOURCE CODE FOR DPARFN
	     
	         raFile.writeBytes("//...........................................\n");
	      }
	      catch (IOException e) {
	    	  MipavUtil.displayError("IOException " + e + " in PARGEN");
	    	  System.exit(-1);
	      }
	      HEADER("DPARFN",REDD,raFile);
	      if (SYMTY) {
	        WRSYM1(NARCS,ORDRG[0],ORDSG[0],RTUNI,U2,CENSY,REFLN,false,REDD,
	               CHNL, raFile);
	        if (REFLN) {
	          CH="TS";
	        }
	        else {
	          CH="TT";
	        }
	        WRFUN2(NARCS,MNARC,STAPT,ARCTY,PGM,RGM,PTX,NTX,DEFN,
	                   CHNL,"IB",CH,"ZETA  ",NUMDER,REDD, raFile);
	        WRSYM3(NARCS,ORDRG[0],REFLN,CHNL, raFile);
	      } // if (SYMTY)
	      else {
	          WRFUN2(NARCS,MNARC,STAPT,ARCTY,PGM,RGM,PTX,NTX,DEFN,
	                 CHNL,"IA","TT","DPARFN",NUMDER,REDD, raFile);
	      }
	
	      try {
		      raFile.writeBytes("//\n");
		      raFile.writeBytes("}\n");
		      raFile.close();
	      }
	      catch (IOException e) {
	    	  MipavUtil.displayError("IOException " + e + " in PARGEN");
	    	  System.exit(-1);
	      }
	      WRTAIL(6,0,IER[0], null);
	
	} // public void PARGEN
    private void HEADER(String TXT, String REDD, RandomAccessFile raFile) {

    String TAB6="      ";

    String LINE=TAB6+"private void "+TXT+"(int IA, double TT[]) {\n";
    try {
    raFile.writeBytes(LINE);

    LINE="//" + TAB6+"IMPLICIT REAL(A-H,O-S),INTEGER(I-N),COMPLEX(T-Z)\n";
    raFile.writeBytes(LINE);

    raFile.writeBytes("      double PI = "+Math.PI+ ";\n");
    raFile.writeBytes("      double UI[] = new double[]{0.0,1.0};\n");
    raFile.writeBytes("//\n");
    }
    catch (IOException e) {
    	MipavUtil.displayError("IOException " + e + " in HEADER");
    	System.exit(-1);
    }

    } // private void HEADER
    
    private void SYINF1(int ORDRG[],int ORDSG[],double RTUNI[], double U2[],boolean REFLN,
    		double Z0[], double Z1[], double Z2[], int IER[]) {
    //COMPLEX RTUNI,U2,Z0,Z1,Z2

//**** GIVEN Z0,THE CENTRE OF SYMMETRY, Z1 AND Z2, THE INITIAL AND FINAL
//**** POINTS ON THE FUNDAMENTAL BOUNDARY SECTION, REFLN, WHICH IS TRUE
//**** IF THE SYMMETRY GROUP HAS IMPROPER  ROTATIONAL ELEMENTS
//**** (I.E. REFLECTIONAL SYMMETRIES), THIS ROUTINE COMPUTES 
//**** ORDRG - THE ORDER OF THE SUBGROUP OF PROPER ROTATIONS (THIS IS THE
//****         ORDER OF THE SYMMETRY GROUP IF REFLN=.FALSE.)
//**** ORDSG - THE ORDER OF THE FULL SYMMETRY GROUP,  EITHER ORDRG OR 
//****         2*ORDRG DEPENDING ON WHETHER REFLN IS .FALSE. OR .TRUE.
//**** RTUNI - THE ROOT OF UNITY FROM WHICH THE PROPER ROTATIONAL SUBROUP
//****         IS GENERATED
//**** U2    - THE ADDITIONAL IN-PLANE ROTATION WHICH, WHEN COMBINED WITH
//****         CONJUGATION, DEFINES THE IMPROPER ROTATION FOR THE CASE
//****         REFLN=.TRUE.

//     LOCAL VARIABLES

    double ALPHA, PI,SQRTEPS;
    //COMPLEX CT,U
    double CT[] = new double[2];
    double U[] = new double[2];
    double cr[] = new double[1];
    double ci[] = new double[1];

    PI=Math.PI;
    SQRTEPS=Math.sqrt(EPS);
    CT[0]=Z2[0]-Z0[0];
    CT[1] = Z2[1] - Z0[1];
    double ABSCT = zabs(CT[0], CT[1]);
    if (ABSCT < SQRTEPS) {
      IER[0]=56;
      return;
    }
    U[0]=CT[0]/ABSCT;
    U[1]= CT[1]/ABSCT;
    zmlt(U[0],U[1],U[0],U[1], cr, ci);
    U2[0] = cr[0];
    U2[1] = ci[0];

    zmlt(Z1[0]-Z0[0],Z1[1]-Z0[1],U[0],-U[1],cr,ci);
    CT[0] = cr[0];
    CT[1] = ci[0];
    ABSCT = zabs(CT[0],CT[1]);
    if (ABSCT < SQRTEPS) {
      IER[0]=57;
      return;
    }
    ALPHA=Math.atan2(CT[1],CT[0]);
    ALPHA=Math.abs(ALPHA);

    if (REFLN) {
      ORDRG[0]=(int)Math.round(PI/ALPHA);
      ORDSG[0]=2*ORDRG[0];
    }
    else {
      ORDRG[0]=2*(int)Math.round(PI/ALPHA);
      ORDSG[0]=ORDRG[0];
    }

    ALPHA=2.0*PI/(double)(ORDRG[0]);
    RTUNI[0] = Math.cos(ALPHA);
    RTUNI[1] = Math.sin(ALPHA);

    // NORMAL EXIT

    IER[0]=0;
    return;
    } // private void SYINF1


	
      //COMPLEX FUNCTION PARFUN(I,T)
      //INTEGER I
      //COMPLEX T
      double[] PARFUN(int I, double T[]) {

          // DUMMY FUNCTION TO AID LINK-LOADING OF PARGEN

          double result[] =new double[]{1.0,0.0};
          return result;
      } // double[] PARFUN     
	
	  //COMPLEX FUNCTION DPARFN(I,T)
	  //INTEGER I
	  //COMPLEX T
      double[] DPARFN(int I, double T[]) {
	
	      // DUMMY FUNCTION TO AID LINK-LOADING OF PARGEN
	
    	  double result[] =new double[]{1.0,0.0};
          return result;
      } // double[] DPARFN

      private void WRHEAD(int I,int CHNL, RandomAccessFile raFile) {
   

      //**** WRITE A HEADING FOR THE MAIN CONFPACK MODULES JAPHYC (I=1), 
      //**** GQPHYC (I=2), JACANP (I=3), GQCANP (I=4), CNDPLT (I=5), THE
      //**** PARAMETRIC FUNCTION GENERATOR PARGEN (I=6),THE PARAMETRIC FUNCTION
      //**** TESTER TSTPLT (I=7) AND THE LEVEL CURVE ROUTINE LEVCUR (I=8).  IF 
      //**** CHNL=0 THEN WRITE ON THE STANDARD OUTPUT CHANNEL, OTHERWISE WRITE 
      //**** ON THE CHANNEL SPECIFIED BY CHNL.
      //
      //    LOCAL VARIABLES
      //
    	  String DOTS = ".................................................";
    	  String CPHEAD = ": C O N F P A C K    M O D U L E    ";
    	  String MOD[] = new String[]{"J A P H Y C :",
    	                              "G Q P H Y C :",
    	                              "J A C A N P :",
    	                              "G Q C A N P :",
    	                              "C N D P L T :",
    	                              "P A R G E N :",
    	                              "T S T P L T :",
    	                              "L E V C U R :"};
    	  String TXT = CPHEAD + MOD[I-1];
            
          if (CHNL == 0 || CHNL == 6) {
        	  System.out.println("\n\n      "+DOTS+"\n      "+TXT+"\n      "+DOTS);
          }
          else {
        	  try {
        	      raFile.writeBytes("\n\n      //"+DOTS+"\n      //"+TXT+"\n      //"+DOTS+"\n");
        	  }
        	  catch (IOException e) {
        		  MipavUtil.displayError("IOException " + e + " on raFile.writeBytes in WRHEAD");
        		  System.exit(-1);
        	  }
          }
          return;
      } // private void WRHEAD
      
      private void WRTAIL(int I, int CHNL, int IER, RandomAccessFile raFile) {
     
//
//**** WRITE A CLOSING MESSAGE FOR THE MAIN CONFPACK MODULES JAPHYC (I=1)
//**** GQPHYC (I=2), JACANP (I=3), GQCANP (I=4), CNDPLT (I=5), THE PARA-
//**** METRIC FUNCTION GENERATOR PARGEN (I=6), THE PARAMETRIC FUNCTION 
//**** TESTER TSTPLT (I=7) AND THE LEVEL CURVE ROUTINE LEVCUR (I=8).  IF 
//**** CHNL=0 THEN WRITE ON THE STANDARD OUTPUT CHANNEL, OTHERWISE WRITE 
//**** ON THE CHANNEL SPECIFIED BY CHNL.  THE TEXT OF THE MESSAGE IS 
//**** DETERMINED BY THE ERROR NUMBER IER VIA THE SUBROUTINE IERTXT.

//     LOCAL VARIABLES
     
      String MOD[] = new String[]{"J A P H Y C :",	  
                      "G Q P H Y C :","J A C A N P :",
                "G Q C A N P :","C N D P L T :","P A R G E N :",
                "T S T P L T :","L E V C U R :"};
     String GOOD = "  NORMAL EXIT";
     String BAD= "  ABNORMAL EXIT";
     String LINE = "__________________________________________________________________&";

     String TXT, TXT2;
     if (IER == 0) {
        TXT=MOD[I-1]+GOOD;
     }
     else{
        TXT=MOD[I-1]+BAD;
     }
     TXT2=IERTXT(IER);

      if ((CHNL == 0) || (CHNL == 6)) {
    	System.out.println("\n\n      "+TXT);
    	System.out.println("      "+TXT2);
        System.out.println(LINE);
     }
     else {
    	try {
            raFile.writeBytes("\n\n      //"+TXT+"\n");
            raFile.writeBytes("      //"+TXT2+"\n");
            raFile.writeBytes("//"+LINE+"\n");
    	}
    	catch(IOException e) {
    		MipavUtil.displayError("IOException " + e + " in WRTAIL");
    		System.exit(-1);
    	}
     }
     return;
      }
      
      private String IERTXT(int IER) {
    	  
//**** SUPPLY ERROR MESSAGE TEXT FOR ERROR NUMBER IER
      String result = null;
      if (IER == 0) {
          result = " ";
      }
      else if (IER == 1) {
          result="PARAMETER IBNDS[0] IS TOO SMALL AT START OF JAPHYC";
      }
      else if (IER == 2) {
          result = "PARAMETER IBNDS[1] IS TOO SMALL AT START OF JAPHYC";
      }
      else if (IER == 3) {
          result = "NQPTS < 1 AT START OF JAPHYC";
      }
      else if (IER == 4) {
          result="FAILURE TO CONVERGE IN EIGSYS; CAN''T SET UP BASIC QUADRATURE RULES";
      }
      else if (IER == 5) {
          result="PARAMETER MNQPT IN IGNLVL MUST BE INCREASED TO AT LEAST NQPTS";
      }
      else if (IER == 6) {
          result="FAILURE TO CONVERGE IN IMTQLH; CAN''T SET UP IGNORE LEVELS";
      }
      else if (IER == 7) {
          result="FAILURE TO CONVERGE IN IMTQLH; CAN''T SET UP COLLOCATION POINTS";
      }
      else if (IER == 8) {
          result="ARGUMENT MNEQN IS TOO SMALL AT START OF JAPHYC";
      }
      else if (IER == 9) {
          result="PARAMETER IBNDS[3] IS TOO SMALL AT START OF JAPHYC";
      }
      else if (IER == 10) {
          result="PARAMETER NMAX IN SUBIN7 MUST BE INCREASED TO AT LEAST 2*NQPTS";
      }
      else if (IER == 11) {
          result="PARAMETER IBNDS[2] IS TOO SMALL AT START OF JAPHYC";
      }
      else if (IER == 12) {
          result="PARAMETER NC IN DEJAC7 AND DELEG7 MUST BE INCREASED";
      }
      else if (IER == 13) {
          result="PARAMETER NR IN DEJAC7 AND DELEG7 MUST BE >= (NQPTS -1)";
      }
      else if (IER == 14) {
          result="A CORNER ANGLE IS TOO SMALL; MAY CAUSE OVERFLOW IN GAMMA FUNCTION";
      }
      else if (IER == 15) {
          result="SINGULAR COLLOCATION MATRIX";
      }
      else if (IER == 16) {
          result="COLLOCATION MATRIX IS EFFECTIVELY SINGULAR";
      }
      else if (IER == 17) {
          result="NUMBER OF SUBARCS EXCEEDS IBNDS[0] DURING REFINEMENT";
      }
      else if (IER == 18) {
          result="NUMBER OF EQUATIONS EXCEEDS MNEQN DURING REFINEMENT";
      }
      else if (IER == 19) {
          result="TOTAL NUMBER OF QUADRATURE PTS EXCEEDS IBNDS[3] DURING REFINEMENT";
      }
      else if (IER == 20) {
          result="NUMBER OF QUADRATURE PANELS EXCEEDS IBNDS[2] DURING REFINEMENT";
      }
      else if (IER == 21) {
          result="FAILURE TO CONVERGE IN IMTQLH; CAN''T SET UP TEST POINTS";
      }
      else if (IER == 22) {
          result="ARGUMENT MQUPH OF GQPHYC MUST BE INCREASED";
      }
      else if (IER == 23) {
          result="PARAMETER MNCOF IN POPQF1 MUST BE >= NQPTS";
      }
      else if (IER == 24) {
          result="NUMBER OF QUADRATURE PANELS EXCEEDS MQIN1 IN GQPHYC";
      }
      else if (IER == 25) {
          result="PARAMETER MNXI IN DEPPJ8 AND DEPPL8 MUST BE INCREASED";
      }
      else if (IER == 26) {
          result="PARAMETER MAXNZ IN DEPPJ9 AND DEPPL9 MUST BE INCREASED";
      }
      else if (IER == 27) {
          result="PARAMETER MXNQD IN PHTCA1 MUST BE INCREASED";
      }
      else if (IER == 28) {
          result="PARAMETER MXCOF IN PHTCA1 MUST BE INCREASED";
      }
      else if (IER == 29) {
          result="PARAMETER MQIN1 IN PHTCA1 MUST BE INCREASED";
      }
      else if (IER == 30) {
          result="PARAMETER MNDG IN JCFIM5 MUST BE INCREASED";
      }
      else if (IER == 31) {
          result="PARAMETER MNQD IN JCFIM5 MUST BE INCREASED";
      }
      else if (IER == 32) {
          result="ARGUMENT IBNDS[1] SUPPLIED TO JACANP MUST BE INCREASED";
      }
      else if (IER == 33) {
          result="ARGUMENT IBNDS[0] SUPPLIED TO JACANP MUST BE INCREASED";
      }
      else if (IER == 34) {
          result="FN HAS SAME SIGN AT INTERVAL ENDS IN BISNEW; CAN''T SOLVE BCF EQN";
      }
      else if (IER == 35) {
          result="DERIVATIVE OF BCF IS ZERO IN BISNEW; CAN''T SOLVE BCF EQN";
      }
      else if (IER == 36) {
          result="ELEMENT OF ARGUMENT ARRAY SVAL IN RHOFN IS +-1; CAN''T CONTINUE";
      }
      else if (IER == 37) {
          result="PARAMETER MXNQD IN CINRAD MUST BE INCREASED";
      }
      else if (IER == 38) {
          result="PARAMETER MXCOF IN CINRAD MUST BE INCREASED";
      }
      else if (IER == 39) {
          result="CENTRE POINT IS PATHOLOGICALLY CLOSE TO BOUNDARY;CAN''T CONTINUE";
      }
      else if (IER == 40) {
          result="PARAMETER MQIN1 IN CINRAD MUST BE INCREASED";
      }
      else if (IER == 41) {
          result="ARGUMENT MQUCA OF GQCANP MUST BE INCREASED";
      }
      else if (IER == 42) {
          result="PARAMETER MNCOF IN POPQG1 MUST BE >= NQPTS";
      }
      else if (IER == 43) {
          result="NUMBER OF QUADRATURE PANELS EXCEEDS MQIN1 IN GQCANP";
      }
      else if (IER == 44) {
          result="PARAMETER MNCOF IN BMPHC1 MUST BE >= NQPTS";
      }
      else if (IER == 45) {
          result="ARGUMENTS IARC, PHYPT OF BMPHYC DON''T DEFINE A BOUNDARY POINT";
      }
      else if (IER == 46) {
          result="PARAMETER MNCOF IN BMCAP1 MUST BE >= NQPTS";
      }
      else if (IER == 47) {
          result="PARAMETER MXNQD IN CATPH4 MUST BE INCREASED";
      }
      else if (IER == 48) {
          result="PARAMETER MNCOF IN CATPH4 MUST BE >= NQPTS";
      }
      else if (IER == 49) {
          result="PARAMETER MQIN1 IN CATPH4 MUST BE INCREASED";
      }
      else if (IER == 50) {
          result="PARAMETER MXCOF IN DIAGN3 MUST BE >= NQPTS";
      }
      else if (IER == 51) {
          result="NON-ANALYTIC ARC DETECTED IN DIAGN3";
      }
      else if (IER == 52) {
          result="PARAMETER MAXSA IN CNDPLT MUST BE INCREASED";
      }
      else if (IER == 53) {
          result="OVERFLOW EXPECTED IN IGNLVL; A CORNER ANGLE IS TOO SMALL";
      }
      else if (IER == 54) {
          result="PARAMETER MXCO IN AXION1 MUST BE INCREASED";
      }
      else if (IER == 55) {
          result="NARCS ISN''T AN INTEGER MULTIPLE OF THE ORDER OF THE SYMMETRY GROUP";
      }
      else if (IER == 56) {
          result="CENTRE OF SYMMETRY IS PATHOLOGICALLY CLOSE TO LAST POINT ON FBS";
      }
      else if (IER == 57) {
          result="CENTRE OF SYMMETRY IS PATHOLOGICALLY CLOSE TO FIRST POINT ON FBS";
      }
      else if (IER == 58) {
          result="NUMBER OF ARCS IS TOO BIG; INCREASE PARAMETER MNARC IN PARGEN";
      }
      else if (IER == 59) {
          result="NUMBER OF ARCS IS TOO BIG; INCREASE PARAMETER MNARC IN TSTPLT";
      }
      else if (IER == 60) {
          result="NON-ANALYTIC ARC (DPARFN=(0.,0.)) DETECTED IN TSTPLT";
      }
      else {
          result="UNRECOGNIZED ERROR NUMBER IN IERTXT ROUTINE !!";
      }
      return result;
}
      
      private void WRSYM1(int NARCS,int ORDRG,int ORDSG,double[] RTUNI,double[] U2,double[] CENSY,boolean REFLN,boolean PARFUN,
    		     String REDD,int CHNL, RandomAccessFile raFile) {
    		      //COMPLEX RTUNI,U2,CENSY
    		
    		//**** TO WRITE THE DIMENSION AND PARAMETER STATEMENTS AND THE CODE TO
    		//**** TO REDUCE A GIVEN ARC NUMBER TO ITS SYMMETRIC COUNTERPART ON THE
    		//**** FUNDAMENTAL BOUNDARY SECTION.
    		
    		//.......................................................................
    		//     AUTHOR: DAVID HOUGH, ETH, ZUERICH
    		//     LAST UPDATE: 4 AUG 1990
    		//.......................................................................C
    		//     LOCAL VARIABLES
    		
    		int I;
    		double R,A;
    		//COMPLEX ZT
    		double ZT[] = new double[2];
    		boolean NEEDC;
    		//String FMT;
    		double cr[] = new double[1];
    		double ci[] = new double[1];
    		
    		//FMT="(A12,"+REDD+",A1,"+REDD+",A2)";
    		
    		try {
	    		if (PARFUN) {
	    			NEEDC = ((CENSY[0] != 0.0) || (CENSY[1] != 0.0));
	    		    if (NEEDC || REFLN) {
	    		    	raFile.writeBytes("      PARAMETER (\n");
	    		        if (NEEDC && REFLN) {
	    		            R=U2[0];
	    		            A=U2[1];
	    		            raFile.writeBytes("U2[0] = " + R + ";\n");
	    		            raFile.writeBytes("U2[1] = " + A + ";\n");
	    		            R=CENSY[0];
	    		            A=CENSY[1];
	    		            raFile.writeBytes("ZCEN[0] = " + R +";\n");
	    		            raFile.writeBytes("ZCEN[1] = " + A + ";)\n");
	    		        } // if (NEEDC && REFLN)
	    		        else if (NEEDC && (!REFLN)) {
	    		            R=CENSY[0];
	    		            A=CENSY[1];
	    		            raFile.writeBytes("ZCEN[0] = " + R + ";\n");
	    		            raFile.writeBytes("ZCEN[1] = " + A + ";)\n");
	    		        } // else if (NEEDC && (!REFLN)) 
	    		        else {
	    		            R=U2[0];
	    		            A=U2[1];
	    		            raFile.writeBytes("U2[0] = " + R + ";\n");
	    		            raFile.writeBytes("U2[1] = " + A + ";)\n");
	    		        } // else
	    		        raFile.writeBytes("//\n");
	    		    } // if (NEEDC || REFLN)
	    		}
	    		else if (REFLN) {
	    		    R=U2[0];
	    		    A=U2[1];
	    		    raFile.writeBytes("      PARAMETER (\n");
	    		    raFile.writeBytes("U2[0] = " + R + ";\n");
	    		    raFile.writeBytes("U2[1] = " + A + ";)\n");
	    		    raFile.writeBytes("//\n");
	    		}
	    		
	    		//FMT="(A7,"+REDD+",A1,"+REDD+",A2)";
	    		
	    	    if (ORDRG >= 2) {
	    	    	raFile.writeBytes("double WW[] = new double["+(ORDRG-1)+"];\n");
	    	    	ZT[0] = 1.0;
	    	    	ZT[1] = 0.0;
	    		    for (I=0; I < ORDRG-2; I++) {
	    		    	zmlt(ZT[0],ZT[1],RTUNI[0],RTUNI[1],cr,ci);
	    		    	ZT[0] = cr[0];
	    		    	ZT[1] = ci[0];
	    		    	raFile.writeBytes("WW["+I+"][0] = " + ZT[0] + ";\n");
	    		    	raFile.writeBytes("WW["+I+"][1] = " + ZT[1] + ";\n");
	    		    }
	    		    zmlt(ZT[0],ZT[1],RTUNI[0],RTUNI[1],cr,ci);
			    	ZT[0] = cr[0];
			    	ZT[1] = ci[0];
			    	raFile.writeBytes("WW["+I+"][0] = " + ZT[0] + ";\n");
			    	raFile.writeBytes("WW["+I+"][1] = " + ZT[1] + ";)\n");
			    	raFile.writeBytes("//\n");
	    	    } // if (ORDRG >= 2)
	    	    
	    		if (ORDRG > 19) {
	    		    System.out.println("\n");
	    		    System.out.println("             ****WARNING****");
	    		    System.out.println("MORE THAN 19 CONTINUTATION LINES HAVE BEEN WRITTEN");
	    		}
	    		
	    		if (REFLN) {
	    	        if (ORDRG > 1) {
	    		        if (NARCS > 1) {
	    		            I=2*NARCS;
	    		            raFile.writeBytes("IB = IA%"+I+";\n");
	    		            raFile.writeBytes("if (IB == 0) IB = " + I + ";\n");
	    		            I=I+1;
	    		            raFile.writeBytes("if (IB > " + NARCS + ") {\n");
	    		            raFile.writeBytes("    IB = " + I + " - IB;\n");
	    		            raFile.writeBytes("    TS[0] = -TT[0];\n");
	    		            raFile.writeBytes("    TS[1] = TT[1]);\n");
	    		            raFile.writeBytes("}\n");
	    		            raFile.writeBytes("else {\n");
	    		            raFile.writeBytes("    TS[0] = TT[0];\n");
	    		            raFile.writeBytes("    TS[1] = TT[1];\n");
	    		            raFile.writeBytes("}\n");
	    		        } // if (NARCS > 1)
	    		        else {
	    		        	raFile.writeBytes("if ((IA%2) == 0) {\n");
	    		            raFile.writeBytes("    TS[0] = -TT[0];\n");
	    		            raFile.writeBytes("    TS[1] = TT[1];\n");
	    		            raFile.writeBytes("}\n");
	    		            raFile.writeBytes("else {\n");
	    		            raFile.writeBytes("    TS[0] = TT[0];\n");
	    		            raFile.writeBytes("    TS[1] = TT[1];\n");
	    		            raFile.writeBytes("}\n");
	    		        } // else
	    	        } // if (ORDRG > 1)
	    	        else {
	    		        if (NARCS > 1) {
	    		            I=2*NARCS+1;
	    		            raFile.writeBytes("if (IA > " + NARCS + "){\n");
	    		            raFile.writeBytes("    IB = " + I + " -IA;\n");
	    		            raFile.writeBytes("    TS[0] = -TT[0];\n");
	    		            raFile.writeBytes("    TS[1] = TT[1]);\n");
	    		            raFile.writeBytes("}\n");
	    		            raFile.writeBytes("else {\n");
	    		            raFile.writeBytes("    IB = IA;\n");
	    		            raFile.writeBytes("    TS[0] = TT[0];\n");
	    		            raFile.writeBytes("    TS[1] = TT[1];\n");
	    		            raFile.writeBytes("}\n");
	    		        } // if (NARCS)
	    		        else {
	    		        	raFile.writeBytes("if (IA == 2) {\n");
	    		        	raFile.writeBytes("    TS[0] = -TT[0];\n");
	    		            raFile.writeBytes("    TS[1] = TT[1]);\n");
	    		            raFile.writeBytes("}\n");
	    		            raFile.writeBytes("else {\n");
	    		            raFile.writeBytes("    TS[0] = TT[0];\n");
	    		            raFile.writeBytes("    TS[1] = TT[1];\n");
	    		            raFile.writeBytes("}\n");     
	    		        } // else
	    	        } // else
	    		} // if (REFLN)
	    		else if (NARCS > 1) {
	    			raFile.writeBytes("IB = IA%" + NARCS + ";\n");
	    		    raFile.writeBytes("if (IB == 0) IB = " + NARCS + ";\n");
	    		} // else if (NARCS > 1)
	    		
	    		raFile.writeBytes("//\n");
    		} // try
    		catch (IOException e) {
    			MipavUtil.displayError("IOException " + e + " in WRSYM1");
    			System.exit(-1);
    		}
    		
      }

      private void WRFUN1(int NARCS,double STAPT[][],int ARCTY[],int PGM[], 
    		  double RGM[], int PTX[], int NTX[], String DEFN[],
    		  int CHNL, String CHIA, String CHTT, String VAR, String REDD, RandomAccessFile raFile) {
          //COMPLEX STAPT(*)
          //CHARACTER DEFN(*)*72,CHIA*2,CHTT*2,VAR*6,REDD*6
    		
          //**** TO WRITE THE SOURCE CODE FOR PARFUN IN THE CASE WHERE NO
    	  //**** SYMMETRY IS INVOLVED.
    		
    	  //.......................................................................
    	  //     AUTHOR: DAVID HOUGH, ETH, ZUERICH
    	  //     LAST UPDATE: 4 AUG 1990
    	  //.......................................................................
    	
    	  //     LOCAL VARIABLES
    	
          int IA,I,J,K;
    	  //CHARACTER TX1*16,TX2*21,FMT1*11,FMT2*11
          String TX1,TX2;
          //String FMT1,FMT2;
    	  // EXTERNAL PTFUN1
    	  TX1 = "     if("+CHIA+ " == ";
    	  TX2 = "     else if("+CHIA+ " == ";
          //FMT1="(A16,I3,A6)";
          //FMT2="(A21,I3,A6)";
          double STAPT2[][];
          double RGM2[];
          String DEFN2[];
    		
    	  try {
	          for (IA=1; IA <= NARCS; IA++) {
	              I=PGM[IA-1];
	    		  J=PTX[IA-1];
	    		  STAPT2 = new double[STAPT.length-IA+1][2];
	    		  for (K = IA; K <= STAPT.length; K++) {
	    			  STAPT2[K-IA] = STAPT[K-1];
	    		  }
	    		  RGM2 = new double[RGM.length-I+1];
	    		  for (K = I; K <= RGM.length; K++) {
	    			  RGM2[K-I] = RGM[K-1];
	    		  }
	    		  DEFN2 = new String[DEFN.length-J+1];
	    		  for (K = J; K <= DEFN.length; K++) {
	    			  DEFN2[K-J] = DEFN[K-1];
	    		  }
	    		  if (NARCS == 1) {
	    		      PTFUN1(ARCTY[IA-1],STAPT2,RGM2,NTX[IA-1],DEFN2,
	    		                 CHNL,CHTT,VAR,REDD, raFile);
	    		  }
	    		  else {
		    		  if (IA == 1) {
		    			  raFile.writeBytes(TX1 + IA + ") {\n");
		    		  }
		    		  else if (IA == NARCS) {
		    			  raFile.writeBytes("      else {\n");
		    		  }
		    		  else {
		    			  raFile.writeBytes(TX2 + IA + ") {\n");
		    		  }
		    		  PTFUN1(ARCTY[IA-1],STAPT2,RGM2,NTX[IA-1],DEFN2,
		    		         CHNL,CHTT,VAR,REDD, raFile);
		    		  if (IA == NARCS) raFile.writeBytes("      }\n");
	    		  } // else
	    	  } // for (IA=1; IA <= NARCS; IA++)
    	  } // try
    	  catch (IOException e) {
    	      MipavUtil.displayError("IOException " + e + " in WRFUN1");
    	      System.exit(-1);
    	  }
    		
      } // private void WRFUN1

      private void PTFUN1(int TYPE, double STAPT[][], double RGM[],int NTX,
    		  String TXT[],int CHNL, String CHTT,String VAR, String REDD,
    		  RandomAccessFile raFile) {
      
          //COMPLEX STAPT(*)
          //CHARACTER TXT(*)*72,CHTT*2,VAR*6,REDD*6

          //.......................................................................
          // AUTHOR: DAVID HOUGH, ETH, ZUERICH
          // LAST UPDATE: 8 AUG 1990
          // .......................................................................C
          //**** LOCAL VARIABLES

      int I;
      double HA,MD,RAD;
      double C1[] = new double[2];
      double C2[] = new double[2];
      double CENTR[] = new double[2];
      //COMPLEX C1,C2,CENTR
      //String TX1, TX1B, TX2;
      //String TX2B, CTX1B, FMT1, FMT2, FMT3, FMT4, FMT5;
      //CHARACTER TX1*4,TX1B*5,TX2*13,TX2B*14,CTX1B*10,
      //+FMT1*25,FMT2*25,FMT3*14,FMT4*25,FMT5*24

      //TX1 = "+"+CHTT+"*";
      //TX1B = TX1 + "(";
      //CTX1B="     " + TX1B;
      //TX2="      "+VAR+" = ";
      //TX2B=TX2+"(";

      //FMT1='(A14,'//REDD//',A1,'//REDD//',A2)'
      //FMT2='(A10,'//REDD//',A1,'//REDD//',A1)'
      //FMT3='(A6,'//REDD//',A1)'
      //FMT4='(A14,'//REDD//',A5,'//REDD//',A3)'
      //FMT5='(A8,'//REDD//',A5,'//REDD//',A1)'

      try {
      if (TYPE == 1) {
    	C1[0] = 0.5*(STAPT[1][0] + STAPT[0][0]);
    	C1[1] = 0.5*(STAPT[1][1] + STAPT[0][1]);
        C2[0] = 0.5*(STAPT[1][0] - STAPT[0][0]);
        C2[1] = 0.5*(STAPT[1][1] - STAPT[0][1]);
        raFile.writeBytes("//\n");
        raFile.writeBytes(VAR+"[0] = " + C1[0] + "+" + CHTT+"[0]*"+C2[0]+" - "+CHTT+"[1]*"+C2[1]+";\n");
        raFile.writeBytes(VAR+"[1] = " + C1[1] + "+" + CHTT+"[0]*"+C2[1]+" + "+CHTT+"[1]*"+C2[0]+";\n");
        raFile.writeBytes("//\n");
      } // if (TYPE == 1)
      else if (TYPE == 2) {
    	CENTR[0] = RGM[0];
    	CENTR[1] = RGM[1];
        C1[0] = STAPT[0][0] - CENTR[0];
        C1[1] = STAPT[0][1] - CENTR[1];
        HA=0.5*RGM[2];
        MD = Math.atan2(C1[1], C1[0]) + HA;
        RAD=zabs(C1[0],C1[1]);
        raFile.writeBytes("//\n");
        raFile.writeBytes(VAR+"[0] = " + CENTR[0] + "+" + RAD + " * " + "Math.exp(-"+CHTT+"[1]*"+HA+")*"
        +"Math.cos("+MD+CHTT+"[0]*"+HA+");\n");
        raFile.writeBytes(VAR+"[1] = " + CENTR[1] + "+" + RAD + " * " + "Math.exp(-"+CHTT+"[1]*"+HA+")*"
                +"Math.sin("+MD+CHTT+"[0]*"+HA+");\n");
        raFile.writeBytes("//\n");
      } // else if (TYPE == 2)
      else if (TYPE == 3) {
        MD=0.5*(RGM[1]+RGM[0]);
        HA=0.5*(RGM[1]-RGM[0]);
        raFile.writeBytes("//\n");
        raFile.writeBytes("T[0] = " + MD + "+" + CHTT + "[0] * " + HA + ";\n");
        raFile.writeBytes("T[1] = " + MD + "+" + CHTT + "[1] * " + HA + ";\n");
        
        raFile.writeBytes(VAR + "[0] = ");
        // NTX = 1 if statements are entered without newlines for multiple lines
        for (I  = 1; I <= NTX; I++) {
        	int index = TXT[I-1].indexOf("ui");
        	String realString = null;
        	if (index == -1) {
        		realString = TXT[I-1];
        	}
        	else {
        		realString = TXT[I-1].substring(0, index);
        	}
        	if ((index == -1) || (index > 0)) {
        	    raFile.writeBytes(realString);
        	}
        	if (I == NTX) {
        		raFile.writeBytes(";\n");
        	}
        }
        raFile.writeBytes(VAR + "[1] = ");
        // NTX = 1 if statements are entered without newlines for multiple lines
        for (I  = 1; I <= NTX; I++) {
        	int index = TXT[I-1].indexOf("ui");
        	String imagString = null;
        	if ((index >= 0) && (index+2 < TXT[I-1].length())) {
        		imagString = TXT[I-1].substring(index+2);
        		raFile.writeBytes(imagString);
        	}
        	if (I == NTX) {
        		raFile.writeBytes(";\n");
        	}
        }
        raFile.writeBytes("//\n");
      } // else if (TYPE == 3)
      else {
    	MD=0.5*(RGM[1]+RGM[0]);
        HA=0.5*(RGM[1]-RGM[0]);
        raFile.writeBytes("//\n");
        raFile.writeBytes("T[0] = " + MD + "+" + CHTT + "[0] * " + HA + ";\n");
        raFile.writeBytes("T[1] = " + MD + "+" + CHTT + "[1] * " + HA + ";\n");
        raFile.writeBytes("     ZRAD = ");
        // NTX = 1 if statements are entered without newlines for multiple lines
        for (I  = 1; I <= NTX; I++) {
        	raFile.writeBytes(TXT[I-1]);
        }
        raFile.writeBytes(VAR + "[0] = Math.exp(-T[1])*(ZRAD * Math.cos(T[0]));\n");
        raFile.writeBytes(VAR + "[1] = Math.exp(-T[1])*(ZRAD * Math.sin(T[0]));\n");
        raFile.writeBytes("//\n");
      }
      } // try 
      catch (IOException e) {
    	  MipavUtil.displayError("IOException " + e + " in PTFUN1");
    	  System.exit(-1);
      }

      } // private void PTFUN1
      

      private void WRSYM2(int NARCS,int ORDRG, double CENSY[], boolean REFLN,int CHNL, RandomAccessFile raFile) {
     
          //COMPLEX CENSY

          // **** TO WRITE THE CODE TO RECOVER THE BOUNDARY POINT FROM ITS SYMMETRIC
          // **** COUNTERPART ON THE FUNDAMENTAL BOUNDARY SECTION.

          //.......................................................................
          // AUTHOR: DAVID HOUGH, ETH, ZUERICH
          // LAST UPDATE: 4 AUG 1990
          // .......................................................................C
          // LOCAL VARIABLES

          int I;
          boolean NEEDC;
  
          NEEDC = ((CENSY[0] != 0.0) || (CENSY[1] != 0.0));
          try {
        	  raFile.writeBytes("//\n");

              if (REFLN) {
                  if (ORDRG > 1) {
                      I=2*NARCS;
                      if (NARCS > 1) {
                    	  raFile.writeBytes("IS = (IA-IB)%"+I+";\n");
                          raFile.writeBytes("IR = (IA-IB-IS)/" + I + ";\n");
                      } // if (NARCS > 1)
                      else {
                    	  raFile.writeBytes("IS = (IA-1)%2;\n");
                          raFile.writeBytes("IR = (IA-1-IS)/2;\n");
                      }
                      raFile.writeBytes("if ((IR == 0) && (IS == 0)) {\n");
                      raFile.writeBytes("    PARFUN[0] = ZETA[0];\n");
                      raFile.writeBytes("    PARFUN[1] = ZETA[1];\n");
                      raFile.writeBytes("}\n");
                      raFile.writeBytes("else if ((IR > 0) && (IS == 0)) {\n");
                      if (NEEDC) {
                    	  raFile.writeBytes("    PARFUN[0] = ZCEN[0] + WW[IR-1][0]*(ZETA[0] - ZCEN[0]) - "
                    	  		+ "WW[IR-1][1]*(ZETA[1] - ZCEN[1]);\n");
                    	  raFile.writeBytes("    PARFUN[1] = ZCEN[1] + WW[IR-1][0]*(ZETA[1] - ZCEN[1]) + "
                      	  		+ "WW[IR-1][1]*(ZETA[0] - ZCEN[0]);\n");
                       } // if (NEEDC)
                      else {
                    	  raFile.writeBytes("PARFUN[0] = WW[IR-1][0]*ZETA[0] - WW[IR-1][1]*ZETA[1];\n");
                      }
                      raFile.writeBytes("else if ((IR == 0) && (IS > 0)) {\n");
                      if (NEEDC) {
                          raFile.writeBytes("    PARFUN[0] = ZCEN[0] + U2[0]*(ZETA[0]-ZCEN[0]) + " +
                        		  "U2[1]*(ZETA[1]-ZCEN[1]);\n");
                          raFile.writeBytes("    PARFUN[1] = ZCEN[1] - U2[0]*(ZETA[1]-ZCEN[1]) + " +
                        		  "U2[1]*(ZETA[0]-ZCEN[0]);\n");
                      }
                      else {
                    	  raFile.writeBytes("    PARFUN[0] = U2[0]*ZETA[0] + U2[1]*ZETA[1];\n");
                    	  raFile.writeBytes("    PARFUN[1] = -U2[0]*ZETA[1] + U2[1]*ZETA[0];\n");
                      }
                      raFile.writeBytes("}\n");
                      raFile.writeBytes("else {\n");
                      if (NEEDC) {
                          raFile.writeBytes("double realPart = U2[0]*WW[IR-1][0] - U2[1]*WW[IR-1][1];\n");
                          raFile.writeBytes("double imagPart = U2[0]*WW[IR-1][1] + U2[1]*WW[IR-1][0];\n");
                          raFile.writeBytes("PARFUN[0] = ZCEN[0] + realPart*(ZETA[0]-ZCEN[0]) + " +
                          		"imagPart*(ZETA[1]-ZCEN[1]);\n");
                          raFile.writeBytes("PARFUN[1] = ZCEN[1] - realPart*(ZETA[1]-ZCEN[1]) + " +
                            		"imagPart*(ZETA[0]-ZCEN[0]);\n");
                      }
                      else {
                    	  raFile.writeBytes("double realPart = U2[0]*WW[IR-1][0] - U2[1]*WW[IR-1][1];\n");
                          raFile.writeBytes("double imagPart = U2[0]*WW[IR-1][1] + U2[1]*WW[IR-1][0];\n");
                          raFile.writeBytes("PARFUN[0] = realPart * ZETA[0] + imagPart * ZETA[1];\n");
                          raFile.writeBytes("PARFUN[1] = -realPart * ZETA[1] + imagPart * ZETA[0];\n");
                      }
                      raFile.writeBytes("}\n");
                  } // if (ORDRG > 1)
                  else { // ORDRG <= 1
                      if (NARCS > 1) {
                    	  raFile.writeBytes("IS = IA - IB;\n");
                      }
                      else {
                    	  raFile.writeBytes("IS = IA - 1;\n");
                      }
                      raFile.writeBytes("if (IS == 0) {\n");
                      raFile.writeBytes("    PARFUN[0] = ZETA[0];\n");
                      raFile.writeBytes("    PARFUN[1] = ZETA[1];\n");
                      raFile.writeBytes("}\n");
                      raFile.writeBytes("else {\n");
                      if (NEEDC) {
                    	  raFile.writeBytes("    PARFUN[0] = ZCEN[0] + U2[0]*(ZETA[0]-ZCEN[0]) + " +
                        		  "U2[1]*(ZETA[1]-ZCEN[1]);\n");
                          raFile.writeBytes("    PARFUN[1] = ZCEN[1] - U2[0]*(ZETA[1]-ZCEN[1]) + " +
                        		  "U2[1]*(ZETA[0]-ZCEN[0]);\n");
                      }
                      else {
                    	  raFile.writeBytes("    PARFUN[0] = U2[0]*(ZETA[0]-ZCEN[0]) + " +
                        		  "U2[1]*(ZETA[1]-ZCEN[1]);\n");
                          raFile.writeBytes("    PARFUN[1] =  -U2[0]*(ZETA[1]-ZCEN[1]) + " +
                        		  "U2[1]*(ZETA[0]-ZCEN[0]);\n");
                      }
                      raFile.writeBytes("}\n");
                  } // else ORDRG <= 1
              } // if (REFLN)
              else { // !REFLN
                  if (NARCS > 1) {
                	  raFile.writeBytes("IR = (IA - IB)/" + NARCS + ";\n");
                  }
                  else {
                	  raFile.writeBytes("IR = IA - 1;\n");
                  }
                  raFile.writeBytes("if (IR == 0) {\n");
                  raFile.writeBytes("PARFUN[0] = ZETA[0]);\n");
                  raFile.writeBytes("PARFUN[1] = ZETA[1]);\n");
                  raFile.writeBytes("}\n");
                  raFile.writeBytes("else {\n");
                  if (NEEDC) {
                	  raFile.writeBytes("    PARFUN[0] = ZCEN[0] + WW[IR-1][0]*(ZETA[0] - ZCEN[0]) - "
                  	  		+ "WW[IR-1][1]*(ZETA[1] - ZCEN[1]);\n");
                  	  raFile.writeBytes("    PARFUN[1] = ZCEN[1] + WW[IR-1][0]*(ZETA[1] - ZCEN[1]) + "
                    	  		+ "WW[IR-1][1]*(ZETA[0] - ZCEN[0]);\n");
                  }
                  else {
                	  raFile.writeBytes("    PARFUN[0] = WW[IR-1][0]*(ZETA[0] - ZCEN[0]) - "
                    	  		+ "WW[IR-1][1]*(ZETA[1] - ZCEN[1]);\n");
                      raFile.writeBytes("    PARFUN[1] = WW[IR-1][0]*(ZETA[1] - ZCEN[1]) + "
                      	  		+ "WW[IR-1][1]*(ZETA[0] - ZCEN[0]);\n");
                  }
                  raFile.writeBytes("}\n");
              } // else !REFLN
          } // try
          catch(IOException e) {
        	  MipavUtil.displayError("IOException " + e + " in WRSYM2");
        	  System.exit(-1);
          }

      } // private void WRSYM2

      private void WRFUN2(int NARCS,int MNARC, double STAPT[][], int ARCTY[], int PGM[],double RGM[],
    		  int PTX[],int NTX[],String DEFN[],int CHNL,String CHIA,String CHTT, String VAR,
    		  boolean NUMDER[], String REDD, RandomAccessFile raFile) {
  
          // COMPLEX STAPT(*)
    	  // CHARACTER DEFN(*)*72,CHIA*2,CHTT*2,VAR*6,REDD*6
    		
          //**** TO WRITE THE SOURCE CODE FOR DPARFN IN THE CASE WHERE NO
    	  //**** SYMMETRY IS INVOLVED.
    		
    	  //.......................................................................
    	  //     AUTHOR: DAVID HOUGH, ETH, ZUERICH
    	  //     LAST UPDATE: 4 AUG 1990
    	  //.......................................................................
    		
    	  //     LOCAL VARIABLES
    		
    	  int IA,I,J1,J2,N1,N2;
    	  String TX1, TX2;
    	  //String FMT1, FMT2;
    	  //CHARACTER TX1*16,TX2*21,FMT1*11,FMT2*11
    	  //EXTERNAL PTFUN2
    	  double STAPT2[][];
    	  double RGM2[];
    	  String DEFN2[];
    	  String DEFN3[];
    	  int K;
    		
          TX1="      if ("+CHIA+" == ";
    	  TX2="      else if ("+CHIA+" == ";
    	  //FMT1="(A16,I3,A6)";
    	  //FMT2="(A21,I3,A6)";
    		
    	  try {
    	  for (IA=1; IA <= NARCS; IA++) {
    		        I=PGM[IA-1];
    		        J1=PTX[IA-1];
    		        J2=PTX[IA+MNARC-1];
    		        N1=NTX[IA-1];
    		        N2=NTX[IA+MNARC-1];
    		        STAPT2 = new double[STAPT.length-IA+1][2];
    		        for (K = IA; K <= STAPT.length; K++) {
    		        	STAPT2[K-IA][0] = STAPT[K-1][0];
    		        	STAPT2[K-IA][1] = STAPT[K-1][1];
    		        }
    		        RGM2 = new double[RGM.length-I+1];
    		        for (K = I; K <= RGM.length; K++) {
    		        	RGM2[K-I] = RGM[K-1];
    		        }
    		        DEFN2 = new String[DEFN.length-J1+1];
    		        for (K = J1; K <= DEFN.length; K++) {
    		        	DEFN2[K-J1] = DEFN[K-1];
    		        }
    		        DEFN3 = new String[DEFN.length-J2+1];
    		        for (K = J2; K <= DEFN.length; K++) {
    		        	DEFN3[K-J2] = DEFN[K-1];
    		        }
    		        if (NARCS == 1) {
    		          PTFUN2(ARCTY[IA-1],STAPT2,RGM2,N1,DEFN2,
    		                    N2,DEFN3,CHNL,CHTT,VAR," 1",NUMDER[IA-1],REDD,raFile);
    		        }
    		        else { 
    		          if (IA == 1) {
    		        	raFile.writeBytes(TX1 + IA + ") {\n");
    		          }
    		          else if (IA == NARCS) {
    		        	raFile.writeBytes("      else {\n");
    		          }
    		          else {
    		        	raFile.writeBytes(TX2 + IA + ") {\n");
    		          }
    		          PTFUN2(ARCTY[IA-1],STAPT2,RGM2,N1,DEFN2,
    		                 N2,DEFN3,CHNL,CHTT,VAR,CHIA,NUMDER[IA-1],REDD,raFile);
    		          if (IA == NARCS) {
    		        	  raFile.writeBytes("      }\n");
    		          }
    		        } // else
    	  } // for (IA=1; IA <= NARCS; IA++)
    	  } // try 
    	  catch (IOException e) {
    		  MipavUtil.displayError("IOException " + e + " in WRFUN2");
    		  System.exit(-1);
    	  }
    		
      } // private void WRFUN2
      
      private void PTFUN2(int TYPE,double STAPT[][], double RGM[],int NTX1, String TXT1[],int NTX2,
          String TXT2[],int CHNL, String CHTT, String VAR, String CHIA, boolean NUMDER, String REDD,
          RandomAccessFile raFile) {
          //COMPLEX STAPT(*)
          //CHARACTER TXT1(*)*72,TXT2(*)*72,CHTT*2,VAR*6,CHIA*2,REDD*6
    		
    	  //.......................................................................
    	  //     AUTHOR: DAVID HOUGH, ETH, ZUERICH
    	  //     LAST UPDATE: 8 AUG 1990
    	  //.......................................................................C
    	  // **** LOCAL VARIABLES
    		
    	  int I;
    	  double HA,MD,RAD,A,R;
    	  double C1[] = new double[2];
    	  double CENTR[] = new double[2];
    	  //COMPLEX C1,CENTR
    	  //String TX1, TX1B, TX2, TX2B, TX3, FMT1, FMT2, FMT3, FMT4, FMT5;
    	  //CHARACTER TX1*4,TX1B*5,TX2*13,TX2B*14,TX3*39,
          //FMT1*25,FMT2*15,FMT3*15,FMT4*25,FMT5*24
    	  //String TX3R, TX3I;
    		
    	  //TX1 = "+"+CHTT+"*";
    	  //TX1B = TX1 + "(";
    	  //TX2 = "      "+VAR+" = ";
    	  //TX2B = TX2 + "(";
    	  //TX3R = "      "+VAR+"[0] = (ZDER * Math.cos(T) - ZRAD * Math.sin(T))*(";
    	  //TX3I = "      "+VAR+"[1] = (ZDER * Math.sin(T) + ZRAD * Math.cos(T))*(";
          //TX3=TX2//'(ZDER+UI*ZRAD)*EXP(UI*T)*('
    	 
    	  //FMT1='(A14,'//REDD//',A1,'//REDD//',A2)'
    	  //FMT2='(A39,'//REDD//',A1)'
    	  //FMT3='(A13,'//REDD//',A2)'
    	  //FMT4='(A14,'//REDD//',A5,'//REDD//',A3)'
    	  //FMT5='(A8,'//REDD//',A5,'//REDD//',A1)'
    		
	     try {
    	    if (TYPE == 1) {
	    		 C1[0] = 0.5*(STAPT[1][0] - STAPT[0][0]);
	    		 C1[1] = 0.5*(STAPT[1][1] - STAPT[0][1]);
	    		 raFile.writeBytes("//\n");
	    		 R=C1[0];
	    		 A=C1[1];
	    		 raFile.writeBytes("      "+VAR+"[0] = " + R + ";\n");
	    		 raFile.writeBytes("      "+VAR+"[1] = " + A + ";\n");
	    		 raFile.writeBytes("//\n");
	    	 } // if (TYPE == 1)
	    	 else if (TYPE == 2) {
	    		 CENTR[0] = RGM[0];
	    		 CENTR[1] = RGM[1];
	    		 C1[0] = STAPT[0][0] - CENTR[0];
	    		 C1[1] = STAPT[0][1] - CENTR[1];
	    		 HA = 0.5 * RGM[2];
	    		 MD = Math.atan2(C1[1], C1[0]) + HA;
	    		 RAD = zabs(C1[0],C1[1]);
	    		 raFile.writeBytes("//\n");
	    		 raFile.writeBytes(VAR+"[0] = -" + RAD+"*"+HA+"*Math.sin("+MD+"+"+CHTT+"*"+HA+");\n");
	    		 raFile.writeBytes(VAR+"[1] = " + RAD+"*"+HA+"*Math.cos("+MD+"+"+CHTT+"*"+HA+");\n");
	    		 raFile.writeBytes("//\n");
	    	 } // else if (TYPE == 2)
	    	 else if (NUMDER) {
	    		 raFile.writeBytes("//\n");
	    		 raFile.writeBytes("      "+VAR+" = ZDPARF("+CHIA+","+CHTT+");\n");
	    		 raFile.writeBytes("//\n");  
	    	 } // else if (NUMDER)
	    	 else if (TYPE == 3) {
	    		 MD = 0.5*(RGM[1] + RGM[0]);
	    		 HA = 0.5*(RGM[1] - RGM[0]);
	    		 raFile.writeBytes("//\n");  
	    		 raFile.writeBytes("      T = " + MD+"+"+CHTT+"*"+"("+HA+");\n");
	    		 raFile.writeBytes("      "+VAR+" = " +HA + "*(");
	    		 for (I = 1; I <= NTX2; I++) {
	    			 raFile.writeBytes(TXT2[I-1]);
	    		 }
	    		 raFile.writeBytes(");\n");
	    		 raFile.writeBytes("//\n");      
	    	 } // else if (TYPE == 3)
	    	 else {
	    		 MD = 0.5*(RGM[1] + RGM[0]);
	    		 HA = 0.5*(RGM[1] - RGM[0]);
	    		 raFile.writeBytes("//\n");  
	    		 raFile.writeBytes("      T = " + MD+"+"+CHTT+"*"+"("+HA+");\n");
	    		 raFile.writeBytes("      ZRAD = ");
	    		 for (I = 1; I <= NTX1; I++) {
	    			 raFile.writeBytes(TXT1[I-1]);
	    		 }
	    		 raFile.writeBytes(";\n");
	    		 raFile.writeBytes("      ZDEr = ");
	    		 for (I = 1; I <= NTX2; I++) {
	    			 raFile.writeBytes(TXT2[I-1]);
	    		 }
	    		 raFile.writeBytes(";\n");      
	    		 raFile.writeBytes("      "+VAR+"[0] = (ZDER * Math.cos(T) - ZRAD * Math.sin(T))*("+HA+");\n");  
	    		 raFile.writeBytes("      "+VAR+"[1] = (ZDER * Math.sin(T) + ZRAD * Math.cos(T))*("+HA+");\n");
	    		 raFile.writeBytes("//\n");      
	    	 } // else
	     } // try
	     catch(IOException e) {
	    	 MipavUtil.displayError("IOException " + e + " in PTFUN2");
	    	 System.exit(-1);
	     }
    		
    }
      
   private void WRSYM3(int NARCS,int ORDRG, boolean REFLN, int CHN, RandomAccessFile raFile) {

    //**** TO WRITE THE CODE TO RECOVER THE DERIVATIVE FROM ITS SYMMETRIC
    //**** COUNTERPART ON THE FUNDAMENTAL BOUNDARY SECTION.

    //.......................................................................
    // AUTHOR: DAVID HOUGH, ETH, ZUERICH
    // LAST UPDATE: 4 AUG 1990
    //.......................................................................C
    //     LOCAL VARIABLES

    int I;


    try {
	    raFile.writeBytes("//\n"); 
	
	    if (REFLN) {
	        if (ORDRG > 1) {
	            I=2*NARCS;
	            if (NARCS > 1) {
	                raFile.writeBytes("      IS = (IA-IB)%"+I+";\n");
	                raFile.writeBytes("      IR=(IA-IB-IS)/"+I+";\n");
	            }
	            else {
	                raFile.writeBytes("      IS=(IA-1)%2;\n");
	                raFile.writeBytes("      IR=(IA-1-IS)/2;\n");
	            }
	            raFile.writeBytes("      if ((IR == 0) && (IS == 0)) {\n");
	            raFile.writeBytes("          DPARFN[0] =ZETA[0];\n");
	            raFile.writeBytes("          DPARFN[1] =ZETA[1];\n");
	            raFile.writeBytes("      }\n");
	            raFile.writeBytes("      else if ((IR > 0) && (IS == 0)) {\n");
	            raFile.writeBytes("          DPARFN[0] = WW[IR-1][0]*ZETA[0] - WW[IR-1][1]*ZETA[1];\n");
	            raFile.writeBytes("          DPARFN[1] = WW[IR-1][0]*ZETA[1] + WW[IR-1][1]*ZETA[0];\n");
	            raFile.writeBytes("      }\n");
	            raFile.writeBytes("       else if ((IR == 0 && (IS > 0)) {\n");
	            raFile.writeBytes("           DPARFN[0] = -U2[0]*ZETA[0] -U2[1]*ZETA[1];\n");
	            raFile.writeBytes("           DPARFN[1] = U2[0]*ZETA[1] -U2[1]*ZETA[0];\n");
	            raFile.writeBytes("       }\n");
	            raFile.writeBytes("       else {\n");
	            raFile.writeBytes("           double realPart = -U2[0]*WW[IR-1][0] + U2[1]*WW[IR-1][1]);\n");
	            raFile.writeBytes("           double imagPart = -U2[0]*WW[IR-1][1] + U2[1]*WW[IR-1][0]);\n");
	            raFile.writeBytes("           DPARFN[0] = realPart*ZETA[0] + imagPart*ZETA[1]);\n");
	            raFile.writeBytes("           DPARFN[1] = imagPart*ZETA[0] - realPart*ZETA[1]);\n"); 
	            raFile.writeBytes("       }\n");
	        } // if (ORDRG > 1)
	        else {
	            if (NARCS > 1) {
	                raFile.writeBytes("      IS=IA-IB;\n");
	            }
	            else {
	                raFile.writeBytes("      IS=IA-1;\n");
	            }
	            raFile.writeBytes("      if (IS == 0) {\n");
	            raFile.writeBytes("          DPARFN[0] = ZETA[0]\n");
	            raFile.writeBytes("          DPARFN[0] = ZETA[0]\n");
	            raFile.writeBytes("      }\n");
	            raFile.writeBytes("         else {\n");
	            raFile.writeBytes("             DPARFN[0] = -U2[0]*ZETA[0] + U2[1]*ZETA[1]);\n");
	            raFile.writeBytes("             DPARFN[1] = U2[0]*ZETA[1] - U2[1]*ZETA[0]);\n");
	            raFile.writeBytes("         }\n");
	        } // else
	    } // if (REFLN)
	    else {
	        if (NARCS > 1) {
	            raFile.writeBytes("      IR=(IA-IB)/"+NARCS+";\n");
	        }
	        else {
	            raFile.writeBytes("      IR=IA-1;\n");
	        }
	        raFile.writeBytes("      if (IR == 0) {\n");
	        raFile.writeBytes("          DPARFN[0] = ZETA[0];\n");
	        raFile.writeBytes("          DPARFN[1] = ZETA[1];\n");
	        raFile.writeBytes("      }\n");
	        raFile.writeBytes("      else {\n");
	        raFile.writeBytes("          DPARFN[0]= WW[IR-1][0]*ZETA[0] - WW[IR-1][1]*ZETA[1];\n");
	        raFile.writeBytes("          DPARFN[1]= WW[IR-1][0]*ZETA[1] + WW[IR-1][1]*ZETA[0];\n");
	        raFile.writeBytes("      }\n");
	    }
    } // try
    catch(IOException e) {
    	MipavUtil.displayError("IOException " + e + " in WRSYM3");
    	System.exit(-1);
    }

    } // private void WRSYM3
   
   private void TSTPLT(String JBNM, double MXMIS, double MXDIF, int NARCS, double PSD,
		   double MINPD, double MAXPD, int CHNL, int IER[]) {
   //CHARACTER*4 JBNM

    //......................................................................

    // 1.     TSTPLT
    //           TESTS THE PARAMETRIC FUNCTION ROUTINES PARFUN AND DPARFN
    //           FOR CONSISTENCY AND OUTPUTS BOUNDARY POINTS FOR PLOTTING.
           

    // 2.     PURPOSE
    //           THE ROUTINE FIRST CHECKS THAT THE PARAMETRIC FUNCTION
    //           ROUTINE PARFUN IS CONSISTENT WITH RESPECT TO ITS DEFINITION
    //           OF ANY CORNERS ON THE BOUNDARY.  THIS IS DONE BY CHECKING 
    //           THAT THE COMPUTED POINT AT THE END OF EACH ARC MATCHES THE 
    //           COMPUTED POINT AT THE START OF THE NEXT ONE.  IF ALL THE
    //           RELATIVE MISFIT ERRORS AT CORNERS ARE LESS THAN
    //           10*(UNIT ROUNDOFF) THEN ALL CORNERS ARE CONSIDERED TO FIT
    //           SATISFACTORILY, OTHERWISE THE MAXIMUM RELATIVE MISFIT
    //           ERROR IS REPORTED.

    //           THE SECOND PURPOSE OF THE ROUTINE IS TO OUTPUT TO A DATA 
    //           FILE THE COORDINATES OF A NUMBER OF POINTS ON THE BOUNDARY. 
    //           THE BOUNDARY POINTS ARE SELECTED ADAPTIVELY TO MEET THE 
    //           PLOTTING RESOLUTION SPECIFICATIONS DEFINED BY THE ARGUMENTS 
    //           PSD,MINPD,MAXPD (SEE BELOW).  THE HOPE IS THAT THE USER MAY 
    //           EASILY FEED THESE DATA POINTS TO HIS LOCAL GRAPH PLOTTING  
    //           ROUTINES SO AS TO CONSTRUCT A PLOT OF THE BOUNDARY.  THIS   
    //           WILL PROVIDE AN ESSENTIAL VISUAL CHECK ON THE VALIDITY OF 
    //           THE ROUTINE PARFUN.  THE OUTPUT DATA FILE IS AUTOMATICALLY 
    //           NAMED <JBNM>zz.
 
    //           THE THIRD PURPOSE OF THE ROUTINE IS TO CHECK PARFUN AND 
    //           DPARFN FOR MUTUAL CONSISTECY.  THIS IS DONE BY COMPUTING 
    //           TWO POINT FINITE DIFFERENCE APPROXIMATIONS TO DPARFN.  
    //           THESE DIFFERENCE APPROXIMATIONS ARE COMPUTED AT EACH BOUND- 
    //           ARY POINT THAT IS OUTPUT FOR PLOTTING AND ALSO AT NEARBY 
    //           POINTS WHICH LIE JUST O F F THE BOUNDARY.  THIS LATTER 
    //           COMPARISON ALSO TESTS PARFUN AND DPARFN FOR CORRECTNESS IN 
    //           ACCEPTING COMPLEX PARAMETER VALUES.   A RELATIVE ERROR IN 
    //           THE FINITE DIFFERENCE APPROXIMATION GREATER THAN 0.1 IS 
    //           REPORTED AS A POSSIBLE LOGICAL INCONSISTENCY BETWEEN PARFUN 
    //           AND DPARFN.  (THE CRITICAL RELATIVE ERROR VALUE OF 0.1 CAN 
    //           BE ALTERED BY ADJUSTING THE LOCAL PARAMETER DTOL).

    // 3.     CALLING SEQUENCE
    //           CALL TSTPLT(JBNM,MXMIS,MXDIF,NARCS,PSD,MINPD,MAXPD,CHNL,IER)

    //        PARAMETERS
    //         ON ENTRY
    //            JBNM   - CHARACTER*4
    //                     THE JOB NAME.  THIS IS USED TO CREATE THE OUTPUT 
    //                     FILE WITH FILENAME

    //                                <JBNM>zz ,

    //                     WHERE <JBNM> DENOTES THE VALUE OF VARIABLE JBNM
    //                     WITH ANY TRAILING SPACES DELETED.

    //            NARCS  - INTEGER
    //                     THE NUMBER OF ANALYTIC ARCS THAT MAKE UP THE 
    //                     W H O L E BOUNDARY OF THE PHYSICAL DOMAIN.

    //            PSD    - REAL
    //                     THE PLOTTING SIZE FOR THE DOMAIN IN ANY APPROPR-
    //                     IATE UNITS.  IF PSD .LE. 0.0 THEN IT IS ASSIGNED
    //                     THE DEFAULT VALUE OF 160.0 (A REASONBLE WIDTH IN
    //                     MM FOR PLOTTING ON A4 PAPER).

    //            MINPD  - REAL
    //                     THE MINIMUM SIGNIFICANT PLOTTING DISTANCE, IN THE
    //                     SAME UNITS AS PSD.  IF PSD .LE. 0.0 THEN IT IS
    //                     ASSIGNED THE DEFAULT VALUE OF 2.0.

    //            MAXPD  - REAL
    //                     THE MAXIMUM ALLOWED PLOTTING DISTANCE, IN THE
    //                     SAME UNITS AS PSD.  IF PSD .LE. 0.0 THEN IT IS
    //                     ASSIGNED THE DEFAULT VALUE OF 5.0.  THE LARGER
    //                     MAXPD, THE COARSER WILL BE THE RESOLUTION OF THE
    //                     BOUNDARY POINTS OUTPUT TO <JBNM>zz, BUT THE
    //                     QUICKER THEY WILL BE COMPUTED. 

    //            CHNL   - INTEGER
    //                     DEFINES AN OUTPUT CHANNEL THAT MAY BE USED FOR
    //                     WRITING THE FILE <JBNM>zz.
    //         ON EXIT
    //            MXMIS  - REAL
    //                     THE MAXIMUM RELATIVE MISFIT ERROR OVER ALL
    //                     CORNER POINTS

    //            MXDIF  - REAL
    //                     THE MAXIMUM RELATIVE ERROR IN FINITE DIFFERENCE
    //                     APPROXIMATIONS TO DPARFN OVER ALL BOUNDARY 
    //                     POINTS OUTPUT TO <JBNM>zz AND NEARBY POINTS OFF
    //                     THE BOUNDARY.

    //            PSD    - REAL
    //                     IF PSD .LE. 0.0 ON ENTRY THEN IT WILL HAVE
    //                     THE DEFAULT VALUE OF 160.0 ON  EXIT.

    //            MINPD  - REAL
    //                     IF PSD .LE. 0.0 ON ENTRY THEN MINPD WILL HAVE
    //                     THE DEFAULT VALUE OF 2.0 ON EXIT

    //            MAXPD  - REAL
    //                     IF PSD .LE. 0.0 ON ENTRY THEN MAXPD WILL HAVE
    //                     THE DEFAULT VALUE OF 5.0 ON EXIT

    //            IER    - INTEGER
    //                     IF IER > 0 THEN AN ABNORMAL EXIT HAS OCCURRED;
    //                     A MESSAGE TO DESCRIBE THE ERROR IS AUTOMATICALLY
    //                     WRITTEN ON THE STANDARD OUTPUT CHANNEL.
    //                     IER=0 - NORMAL EXIT.
    //                     IER>0 - ABNORMAL EXIT; THE ERROR MESSAGE SHOULD
    //                             BE SELF EXPLANATORY.


    // 4.     SUBROUTINES OR FUNCTIONS NEEDED
    //              - THE CONFPACK LIBRARY.
    //              - THE REAL FUNCTION R1MACH.
    //              - THE USER SUPPLIED COMPLEX FUNCTIONS PARFUN AND DPARFN.


    // 5.     FURTHER COMMENTS
    //              - A SUMMARY LISTING IS AUTOMATICALLY WRITTEN ON THE 
    //                STANDARD OUTPUT CHANNEL.
    //              - THE OUTPUT FILE <JBNM>zz CONTAINS COORDINATE PAIRS

    //                                 X Y

    //                FOR POINTS ON THE PHYSICAL BOUNDARY, WITH ONE PAIR
    //                PER LINE.

    // ......................................................................
    //     AUTHOR: DAVID HOUGH, ETH, ZUERICH
    //     LAST UPDATE: 6 JULY 1990
    // ......................................................................C
    //     LOCAL VARIABLES

    int I,IA;
    int IMX = 0;
    double TINC = 0.0;
    double TMX = 0.0;
    double A1,DIFF,ERR,HH,MINC,RMAX,RMEAN,RMIN,T,TOL1,TSD;
    double TT[] = new double[2];
    //REAL TT(2)
    double C1[] = new double[2];
    double C2[] = new double[2];
    double CENTR[] = new double[2];
    double ZZ0[] = new double[2];
    double DZZ[] = new double[2];
    double NDZZ[] = new double[2];
    //COMPLEX C1,C2,CENTR,ZZ0,DZZ,NDZZ;
    double ZZ[][] = new double[2][2];
    //COMPLEX ZZ(2)
    //CHARACTER OFL*6
    final int MNARC = 200;
    final double DTOL = 1.0E-1;
    final int NH = 4;
    boolean ATEND,FIRST,WARND;
    boolean LNSEG[] = new boolean[MNARC];
    double PIN[] = new double[2];
    double PAROUT[];
    int zzindex;
    double cr[] = new double[1];
    double ci[] = new double[1];
    //EXTERNAL DPARFN,LINSEG,PARFUN,R1MACH,WRHEAD,WRTAIL,ZDPARF
    //COMPLEX PARFUN,DPARFN,ZDPARF

    //**** WRITE CONFPACK HEADING

    WRHEAD(7,0,null);

    if (NARCS > MNARC) {
        IER[0]=59;
        WRTAIL(7,0,IER[0], null);
    }

    //1     FORMAT(A45)
    //2     FORMAT(A45,I4)
    //3     FORMAT(A45,E10.3)
    //4     FORMAT(//,T17,A)

    TOL1= 10.0*EPS;

    //**** CHECK THAT ALL ARCS MEET AT CORNER POINTS

    IER[0]=0;
    CENTR[0] = 0.0;
    CENTR[1] = 0.0;
    MXMIS=0.0;
    for (IA=1; IA <= NARCS; IA++) {
        if (IA == 1) {
            I=NARCS;
        }
        else {
            I=IA-1;
        }
        PIN[0] = -1.0;
        PIN[1] = 0.0;
        C1 = PARFUN(IA, PIN);
        CENTR[0] = CENTR[0] + C1[0];
        CENTR[1] = CENTR[1] + C1[1];
        A1 = zabs(C1[0],C1[1]);
        PIN[0] = 1.0;
        PIN[1] = 0.0;
        C2=PARFUN(I,PIN);
        ERR = zabs(C1[0]-C2[0],C1[1]-C2[1]);
        if (A1 >= 1.0) {
            ERR=ERR/A1;
        }
        if (ERR > MXMIS) {
            IMX=IA;
            MXMIS=ERR;
        }
    } // for (IA=1; IA <= NARCS; IA++)
	if (MXMIS >= TOL1) {
        System.out.println("MAXIMUM CORNER MISFIT: " + MXMIS);
        System.out.println("OCCURS AT CORNER: " + IMX);
    }
    else {
        System.out.println("ALL ARCS FIT AT CORNERS:");
    }	

    // **** ESTIMATE THE DIAMETER (TSD) OF THE PHYSICAL DOMAIN

    CENTR[0]=CENTR[0]/NARCS;
    CENTR[1]=CENTR[1]/NARCS;
    TSD=0.0;
    HH=2.0/(double)(NH);
    for (IA=1; IA <= NARCS; IA++) {
        T=-1.0;
        for (I=1; I <= NH; I++) {
            T=T+HH;
            PIN[0] = T;
            PIN[1] = 0;
            PAROUT = PARFUN(IA,PIN);
            C1[0] = PAROUT[0] - CENTR[0];
            C1[1] = PAROUT[1] - CENTR[1];
            A1=zabs(C1[0],C1[1]);
            TSD=Math.max(TSD,A1);
        } // for (I=1; I <= NH; I++)
    } // for (IA=1; IA <= NARCS; IA++)
   TSD=2.0*TSD;

   //**** DETERMINE WHICH ARCS (IF ANY) ARE LINE SEGMENTS

   LINSEG(LNSEG,NARCS);

   //**** OPEN FILE TO RECEIVE BOUNDARY DATA POINTS FOR PLOTTING

   //L=INDEX(JBNM,' ')-1
   //IF (L.EQ.-1) L=4
   //OFL=JBNM(1:L)//'zz'
   //OPEN(CHNL,FILE=OFL)
   // Use global zzset instead

   // **** SET DEFAULT PLOTTING DISTANCES, IF NECESSARY

   if (PSD <= 0.0) {
       PSD=1.6E+2;
       MINPD=2.0;
       MAXPD=5.0;
   }
   RMIN=MINPD*TSD/PSD;
   RMAX=MAXPD*TSD/PSD;
   RMEAN=0.5*(RMIN+RMAX);
   MINC=Math.sqrt(EPS);
   
   // **** START EVALUATING BOUNDARY POINTS AND DERIVATIVES FOR PLOTTING AND
   // **** TESTING

   MXDIF=0.0;
   zzindex = 0;
   for (IA=1; IA <= NARCS; IA++) {
       TT[0]=-1.0;
       PIN[0] = TT[0];
       PIN[1] = 0.0;
       ZZ[0]=PARFUN(IA,PIN);
       zzset[zzindex][0] = ZZ[0][0];
       zzset[zzindex++][1] = ZZ[0][1];
       if (IA == 1) {
    	   ZZ0[0] = ZZ[0][0];
    	   ZZ0[1] = ZZ[0][1];
       }
       FIRST = true;
       WARND= false;
       while (true) {
	   
           //****   TEST THE COMPATIBILTY OF PARFUN AND DPARFN BY ESTIMATING DPARFN
           //****   NUMERICALLY AT BOTH REAL AND COMPLEX PARAMETER VALUES.

	         for (I=1; I <= 2; I++) {
	             if (I == 1) {
	                 C1[0]=TT[0];
	                 C1[1] = 0.0;
	             }
	             else {
	                 C1[0]=TT[0];
                     C1[1] = MINC;
	             }
	             DZZ=DPARFN(IA,C1);
	             NDZZ=ZDPARF(IA,C1);
	             A1=zabs(DZZ[0],DZZ[1]);
	
	             if (A1 == 0.0) {
	                 IER[0]=60;
	                 System.out.println();
	                 System.out.println("              ***DPARFN=(0.,0.)***");
	                 System.out.println("                             ARC: " + IA); 
	                 System.out.println(" STANDARDISED PARAMETER VALUE: " + TT[0]);
	                 WRTAIL(7,0,IER[0],null);
	                 return;
	             } // if (A1 == 0.0)
	
	             if (A1 <=TOL1 && !WARND) {
	                 System.out.println("*** W A R N I N G  ***");
	                 System.out.println("PATHOLOGICALLY SMALL DERIVATIVE ON ARC" + IA);
	                 WARND=true;
	             } // if (A1 <=TOL1 && !WARND)
	
	             if (FIRST) {
	                 TINC=RMEAN/A1;
	                 TINC=Math.max(TINC,MINC);
	                 FIRST=false;
	             } // if (FIRST)
	
	             zdiv(1.0-NDZZ[0],-NDZZ[1],DZZ[0],DZZ[1],cr,ci);
	             ERR = zabs(cr[0],ci[0]);
	             if (ERR > MXDIF) {
	                 MXDIF=ERR;
	                 IMX=IA;
	                 TMX=TT[0];
	             } // if (ERR > MXDIF)
	         } // for (I=1; I <= 2; I++)
	
	     if (!LNSEG[IA-1]) {
	
	    	 //DETERMINE THE NEXT BOUNDARY POINT TO BE PLOTTED
	         while (true) {
	             TT[1]=TT[0]+TINC;
	             if (TT[1] >= 1.0) {
	                 TT[1]=1.0;
	                 ATEND=true;
	             }
	             else {
	                 ATEND=false;
	             }
	       
	             PIN[0] = TT[1];
	             PIN[1] = 0.0;
	             ZZ[1] = PARFUN(IA,PIN);
	             DIFF=zabs(ZZ[1][0]-ZZ[0][0],ZZ[1][1]-ZZ[0][1]);
	             if (DIFF == 0.0 && !ATEND) {
	                 TINC=Math.max(MINC,2*TINC);
	                 continue;
	             } // if (DIFF == 0.0 && !ATEND)
	
	             if (DIFF > RMAX || (DIFF < RMIN && !ATEND)) {
	                 TINC=RMEAN*TINC/DIFF;
	                 TINC=Math.max(TINC,MINC);
	                 continue;
	             } // if (DIFF > RMAX || (DIFF < RMIN && !ATEND))
	             break;
	         } // while (true)
	
	         zzset[zzindex][0] = ZZ[1][0];
	         zzset[zzindex++][1] = ZZ[1][1];
	         if (!ATEND) {
	             ZZ[0][0]=ZZ[1][0];
	             ZZ[0][1] = ZZ[1][1];
	             TT[0]=TT[1];
	       }
	       else {
	    	   break;
	       }
	     } // if (!LNSEG[IA-1])
	     else {
	    	 break;
	     }
       } // while (true)

   } // for (IA=1; IA <= NARCS; IA++)
   if (LNSEG[NARCS-1]) {
	   zzset[zzindex][0] = ZZ0[0];
       zzset[zzindex++][1] = ZZ0[1];
   }   

   if (MXDIF > DTOL) {
       System.out.println();
       System.out.println("POSSIBLE PARFUN/DPARFN INCONSISTECY ON ARC: " + IMX);
       System.out.println("OCCURS AT STANDARDISED PARAMETER VALUE: " + TMX);
       System.out.println("RELATIVE FINITE DIFF ERROR: " + MXDIF);
   }
   else {
       System.out.println();
       System.out.println("PARFUN AND DPARFN ARE CONSISTENT:");
   }

   WRTAIL(7,0,IER[0],null);

   } // private void TSTPLOT
   
   private void LINSEG(boolean LNSEG[], int NARCS) {

       //**** TO DETERMINE THE ARRAY LNSEG, WHERE LNSEG(I) IS SET TO TRUE IF THE
       //**** I'TH ARC IS A LINE SEGMENT, I=1,...,NARCS.

       //**** LOCAL VARIABLES

       final int NPTS = 9;
	   int IA,J,NINTS;
       double DIFF,HH,MXDIF,TOL;
       double SUM[] = new double[2];
       double TT[] = new double[2];
       // COMPLEX SUM,TT
       double DF[][] = new double[NPTS][2];
       //COMPLEX DF(NPTS)
       //EXTERNAL DPARFN,R1MACH
       // COMPLEX DPARFN

       NINTS=NPTS-1;
       HH=2.0/NINTS;
       TOL=10.0*EPS;

       for (IA=1; IA <= NARCS; IA++) {
    	   SUM[0] = 0.0;
    	   SUM[1] = 0.0;
           for (J=1; J <= NPTS; J++) {
               TT[0]=-1.0+(J-1)*HH;
               TT[1] = 0.0;
               DF[J-1]=DPARFN(IA,TT);
               SUM[0]=SUM[0]+DF[J-1][0];
               SUM[1]=SUM[1]+DF[J-1][1];
           } // for (J=1; J <= NPTS; J++)
           SUM[0]=SUM[0]/NPTS;
           SUM[1]=SUM[1]/NPTS;

           MXDIF=0.0;
           for (J=1; J <= NPTS; J++) {
               DIFF=zabs(SUM[0]-DF[J-1][0],SUM[1]-DF[J-1][1]);
               MXDIF = Math.max(MXDIF,DIFF);
           } // for (J=1; J <= NPTS; J++)

           if (MXDIF <= TOL) {
               LNSEG[IA-1] = true;
           }
           else {
               LNSEG[IA-1] = false;
           }
       } // for (IA=1; IA <= NARCS; IA++)

   } // private void LINSEG
   

   //COMPLEX FUNCTION ZDPARF(I,T)
   private double[] ZDPARF(int I, double T[]) {
       //COMPLEX T

       //**** NUMERICAL ESTIMATION OF THE DERIVATIVE OF THE PARAMETRIC FUNCTION
       //**** USING 2- OR 4-POINT TRAPEZOIDAL RULE ESTIMATES IN CAUCHY'S
       //**** FORMULA.  THE 2-POINT ESTIMATE IS THE STANDARD CENTRAL DIFFERENCE
       //**** IN THE REAL AXIS DIRECTION.

       double EPSZ;
       //final double IM[] = new double[]{0.0,1.0};
       final boolean FOUR = false;
       double SUM[] = new double[2];
       //COMPLEX IM,SUM
       double POUT1[];
       double POUT2[];
       double result[] = new double[2];
       double PIN[] = new double[2];

       //EXTERNAL PARFUN
       // COMPLEX PARFUN

       EPSZ = Math.pow(EPS, 0.3333);
       PIN[0] = T[0] + EPSZ;
       PIN[1] = T[1];
       POUT1 = PARFUN(I,PIN);
       PIN[0] = T[0] - EPSZ;
       PIN[1] = T[1];
       POUT2 = PARFUN(I,PIN);
       SUM[0] = (POUT1[0] - POUT2[0])/2.0/EPSZ;
       SUM[1] = (POUT1[1] - POUT2[1])/2.0/EPSZ;

       if (FOUR) {
    	    PIN[0] = T[0];
    	    PIN[1] = T[1]+EPSZ;
    	    POUT1 = PARFUN(I,PIN);
    	    PIN[0] = T[0];
    	    PIN[1] = T[1]-EPSZ;
    	    POUT2 = PARFUN(I,PIN);
            result[0] = SUM[0]/2.0 + (POUT1[1] - POUT2[1])/4.0/EPSZ;
            result[1] = SUM[1]/2.0 - (POUT1[0] - POUT2[0])/4.0/EPSZ;
       }
       else {
            result[0]=SUM[0];
            result[1] = SUM[1];
       }
       return result;
   }

   private void JAPHYC(double MAXER,
		      int ISYGP, boolean INCST,
		      int RFARC, double RFARG[], int TSTNG,int OULVL,
		      double MATRX[][][],
		      int IER[]) {
		
		      //INTEGER IBNDS(*),IGEOM(*),ISNPH(*),IWORK(*)
		      //REAL RGEOM(*),MATRX(MNEQN,MNEQN,2),RSNPH(*),RWORK(*)
		      //COMPLEX CENTR
		      //COMPLEX ZWORK(*)
		      //LOGICAL LWORK(*)
		      //CHARACTER JBNM*4,HEAD*72
		
		// ......................................................................
		
		// 1.     JAPHYC
		//           COMPUTATION OF PIECEWISE ORTHOGONAL JACOBI POLYNOMIAL 
		//           APPROXIMATIONS TO THE BOUNDARY CORRESPONDENCE DERIVATIVE FOR
		//           THE MAP:PHYSICAL --> CANONICAL.
		
		// 2.     PURPOSE
		//           THE MAIN PURPOSE IS TO CALCULATE THE COEFFICIENTS IN THE
		//           PIECEWISE ORTHOGONAL JACOBI POLYNOMIAL APPROXIMATIONS TO THE
		//           BOUNDARY CORRESPONDENCE DERIVATIVE FOR THE CONFORMAL MAP OF
		//           A GIVEN SIMPLY CONNECTED PHYSICAL DOMAIN (WITH PIECEWISE
		//           ANALYTIC BOUNDARY) ONTO A CANONICAL DOMAIN (WITH UNIT CIRCLE
		//           AS BOUNDARY).  AN INTERIOR PHYSICAL DOMAIN IS MAPPED TO THE 
		//           UNIT DISC, AN EXTERIOR PHYSICAL DOMAIN TO THE COMPLEMENT OF
		//           THE CLOSED UNIT DISC.
		//           THE METHOD USED IS AN ADAPTIVE COLLOCATION SOLUTION OF
		//           SYMM'S INTEGRAL EQUATION.  
		//           A NUMBER OF DATA ARRAYS ASSOCIATED WITH THE POLYNOMIAL
		//           APPROXIMATIONS ARE ALSO COMPUTED AND MAY BE USED FOR SUBSE-
		//           QUENT PROCESSING.  IN ADDITION TO BEING RETURNED AS 
		//           PARAMETERS OF THE SUBROUTINE THESE ARE ALSO AUTOMATICALLY
		//           OUTPUT TO DATA FILES.
		
		// 3.     CALLING SEQUENCE
		//           CALL JAPHYC(JBNM,HEAD,MAXER,INTER,NARCS,ISYGP,NQPTS,INCST,
		//                       RFARC,RFARG,CENTR,TSTNG,OULVL,IBNDS,MATRX,IWORK,
		//                       RWORK,ZWORK,LWORK,OCH,IGEOM,RGEOM,ISNPH,RSNPH,
		//                       IER)
		
		//        PARAMETERS
		//         ON ENTRY
		//            JBNM   - CHARACTER*4
		//                     THE JOB NAME.  THIS IS USED TO CREATE THREE OUT-
		//                     PUT FILES WITH FILENAMES
		
		//                         <JBNM>pl, <JBNM>gm, <JBNM>ph,
		
		//                     WHERE <JBNM> DENOTES THE VALUE OF VARIABLE JBNM
		//                     WITH ANY TRAILING SPACES DELETED.  THE FIRST OF
		//                     THESE IS A LISTING FILE RECORDING THE PROGRESS
		//                     AND RESULTS OF THE CALCULATION FOR LATER READING
		//                     BY THE USER.  THE TWO FILES <JBNM>gm AND <JBNM>ph
		//                     ARE DATA FILES, NOT REALLY INTENDED TO BE READ
		//                     BY THE USER.
		//                     THE VALUE OF JBNM IS ALSO THE ONLY ITEM IN A
		//                     FOURTH OUTPUT FILE NAMED (LITERALLY) jbnm.
		
		//            HEAD   - CHARACTER*72
		//                     A HEADING FOR THE PROBLEM, TO APPEAR ON THE
		//                     LISTING FILE <JBNM>pl.
		
		//            MAXER  - REAL
		//                     RELATIVE ACCURACY REQUESTED FOR THE CONFORMAL MAP;
		//                     THIS IS THE SAME AS THE ABSOLUTE ACCURACY ON THE
		//                     BOUNDARY OF THE PHYSICAL DOMAIN.
		
		//            INTER  - LOGICAL
		//                     TRUE IF THE PHYSICAL DOMAIN IS INTERIOR, FALSE 
		//                     OTHERWISE.
		
		//            NARCS  - INTEGER
		//                     THE NUMBER OF ANALYTIC ARCS THAT MAKE UP THE 
		//                     W H O L E BOUNDARY OF THE PHYSICAL DOMAIN.
		
		//            ISYGP  - INTEGER
		//                     THE MAGNITUDE OF ISYGP IS THE ORDER OF THE
		//                     SYMMETRY GROUP OF THE PHYSICAL DOMAIN.
		//                     ISYGP.EQ.1  -THE SYMMETRY GROUP HAS ONLY ONE ELE-
		//                                  MENT,THE IDENTITY TRANSFORMATION;  IN
		//                                  OTHER WORDS, THE DOMAIN HAS 'NO 
		//                                  SYMMETRY'.
		//                     ISYGP.GT.1  -THE SYMMETRY GROUP CONTAINS ONLY
		//                                  PROPER (IN-PLANE) ROTATIONS; IN OTHER
		//                                  WORDS, THE DOMAIN HAS ONLY ROTATIONAL
		//                                  SYMMETRIES.
		//                     ISYGP.LT.-1 -THE SYMMETRY GROUP CONTAINS IMPROPER
		//                                  (OUT-OF-PLANE) ROATIONS; IN OTHER
		//                                  WORDS, THE DOMAIN HAS REFLECTIONAL
		//                                  SYMMETRY AND MAY ALSO HAVE ROTATIONAL
		//                                  SYMMETRIES.
		//                     AN INPUT VALUE OF -1 OR 0 IS TREATED AS IF IT WERE
		//                     1.
		
		//            NQPTS  - INTEGER
		//                     PLAYS TWO ROLES.
		//                     1. THE NUMBER OF QUADRATURE POINTS TO BE USED IN 
		//                        AN ELEMENTARY GAUSS-JACOBI RULE;  COMPOSITE 
		//                        RULES ARE CONSTRUCTED FROM PANELS OF NQPTS-
		//                        POINT RULES.
		//                     2. THE MAXIMUM DEGREE OF POLYNOMIAL APPROXIMATION
		//                        IS FIXED AT NQPTS-1.
		//                     NQPTS SHOULD BE REASONABLY LARGE; A PRACTICAL RULE
		//                     OF THUMB IS THAT IF MACHINE PRECISION IS X*1E-N,
		//                    1<X<10, THEN NQPTS=N+1.
		
		//            INCST  - LOGICAL
		//                     IF INCST IS TRUE THEN AN INCREMENTAL STRATEGY IS
		//                     USED TO TRY TO ACHIEVE THE ACCURACY SPECIFIED BY
		//                     MAXER; VERY ROUGHLY SPEAKING, THIS MEANS THAT THE 
		//                     METHOD SUCCESSIVELY ACHIEVES THE TARGET ACCURACIES
		//                     1E-1,1E-2,...UNTIL MAXER HAS BEEN ACHIEVED.  IF 
		//                     THE PROBLEM IS THOUGHT TO BE EITHER PARTICULARLY
		//                     DIFFICULT OR PARTICULARLY SIMPLE, THEN INCST 
		//                     SHOULD BE SET TO .TRUE.  FOR PROBLEMS OF 'AVERAGE'
		//                     DIFFICULTY, SETTING INCST TO .FALSE. IS USUALLY 
		//                     MORE EFFICIENT.
		
		//            RFARC  - INTEGER
		//                     THE REFERENCE ARC USED TO DEFINE THE ORIENTATION 
		//                     THAT IS GIVEN TO THE MAP.  THE CONVENTION IS THAT 
		//                     THE POINT AT THE START OF ANALYTIC ARC NUMBER 
		//                     RFARC IS MAPPED TO THE POINT WITH ARGUMENT 
		//                     RFARG*PI ON THE UNIT DISC.
		
		//            RFARG  - REAL
		//                     THE REFERENCE ARGUMENT/PI USED TO DEFINE THE 
		//                     ORIENTATION THAT IS GIVEN TO THE MAP.  SEE RFARC 
		//                     ABOVE.
		
		//            CENTR  - COMPLEX
		//                     THE POINT IN THE PHYSICAL PLANE THAT IS TO BE
		//                     MAPPED TO THE CENTRE OF THE UNIT DISC.  FOR
		//                     EXTERIOR DOMAINS CENTR MUST BE SOME POINT IN THE
		//                     COMPLEMENTARY INTERIOR  PHYSICAL DOMAIN.
		//                     IN CASE ABS(ISYGP).GT.1 THEN CENTR MUST ALSO BE
		//                     A CENTRE OF SYMMETRY FOR THE PHYSICAL DOMAIN.
		
		//            TSTNG  - INTEGER
		//                     EITHER 0 OR 1.
		//                     ON SUCCESSFUL COMPLETION OF THE NUMERICAL SOLUTION
		//                     OF SYMM'S EQUATION, A MODULE IS PROVIDED FOR
		//                     TESTING THE ERROR IN THE MODULUS OF THE COMPUTED
		//                     MAP ON THE BOUNDARY OF THE DOMAIN.
		//                     TSTNG=0 - TEST ONLY AT SUB-ARC END POINTS
		//                     TSTNG=1 - IN ADDITION TO TESTING AT SUB-ARC END
		//                               POINTS TEST ALSO AT INTERIOR POINTS
		//                               ON EACH SUB-ARC.
		
		//            OULVL  - INTEGER
		//                     EITHER 0,1,2,3,4 OR 5.
		//                     CONTROLS THE AMOUNT OF OUTPUT IN THE LISTING FILE
		//                     <JBNM>pl.
		//                     OULVL=0 - OUTPUT A SOLUTION SUMMARY AT EACH STAGE
		//                               IN THE ADAPTIVE PROCESS AND A SHORT
		//                               SUMMARY OF THE ERRORS IN MODULUS.
		//                     OULVL=1 - AS 0, BUT ALSO OUTPUT A DETAILED LIST OF
		//                               THE ERRORS IN MODULUS.
		//                     OULVL=2 - AS 0, BUT ALSO OUTPUT FULL DETAILS OF 
		//                               THE FINAL COMPUTED JACOBI COEFFICIENTS 
		//                               ON SUCCESSFUL COMPLETION.
		//                     OULVL=3 - AS 2, BUT ALSO OUTPUT A DETAILED LIST OF
		//                               THE ERRORS IN MODULUS.
		//                     OULVL=4 - OUTPUT FULL DETAILS OF THE OF THE COMPU-
		//                               TED JACOBI COEFFICIENTS AT EVERY STAGE
		//                               IN THE ADAPTIVE PROCESS AND A SHORT
		//                               SUMMARY OF THE ERRORS IN MODULUS.
		//                     OULVL=5 - AS 4, BUT ALSO OUTPUT A DETAILED LIST OF
		//                               THE ERRORS IN MODULUS.
		
		//            IBNDS  - INTEGER ARRAY
		//                     INTEGER VECTOR OF SIZE AT LEAST 5.
		//                     IBNDS(K), K=1,2,3,4,5, DEFINE VARIOUS UPPER LIMITS
		//                     THAT HAVE BEEN SET IN THE CALLING PROGRAM AND 
		//                     WHICH CONTROL THE SIZES OF THE ARRAYS IGEOM,RGEOM,
		//                     MATRX,ISNPH,RSNPH,IWORK,RWORK,ZWORK,LWORK. 
		//                     THEIR MEANINGS ARE AS FOLLOWS:
		//                     IBNDS(1) - THE MAXIMUM NUMBER OF SUB-ARCS ALLOWED.
		//                     IBNDS(2) - THE MAXIMUM NUMBER OF JACOBI INDECES
		//                                ALLOWED (WHICH IS ALSO THE 1 + THE
		//                                MAXIMUM NUMBER OF CORNERS ALLOWED ON 
		//                                PHYSICAL BOUNDARY).
		//                     IBNDS(3) - 1 + THE MAXIMUM NUMBER OF PANELS
		//                                ALLOWED IN A SINGLE COMPOSITE GAUSSIAN
		//                                RULE.
		//                     IBNDS(4) - THE MAXIMUM TOTAL NUMBER OF QUADRATURE 
		//                                POINTS ALLOWED OVER ALL COMPOSITE 
		//                                GAUSSIAN RULES.
		//                                (IBNDS(4)<=(IBNDS(3)-1)*IBNDS(2)*NQPTS)
		
		//            MNEQN  - INTEGER
		//                     THE MAXIMUM NUMBER OF EQUATIONS ALLOWED IN THE 
		//                     LINEAR ALGEBRAIC SYSTEM RESULTING FROM THE 
		//                     COLLOCATION METHOD. (MNEQN <= 1+IBNDS(1)*NQPTS)
		
		//            MATRX  - REAL ARRAY
		//                     A 3-DIMENSIONAL MATRIX OF SIZE 
		//                          MNEQN X MNEQN X 2 .
		//                     (IN THE ADAPTIVE PROCESS, MATRX(*,*,2) WILL STORE 
		//                     THE COEFFICIENT MATRIX OF THE CURRENT COLLOCATION 
		//                     SYSTEM AND MATRX(*,*,1) WILL STORE THE COEFFICIENT
		//                     MATRIX OF THE PREVIOUS SYSTEM)
		
		//            IWORK  - INTEGER ARRAY
		//                     A WORKING VECTOR OF SIZE AT LEAST
		//                        8*IBNDS(1)+MNEQN+2*IBNDS(2) .
		
		//            RWORK  - REAL ARRAY
		//                     A WORKING VECTOR OF SIZE AT LEAST
		//                       (4 + 3*NQPTS + 5*IBNDS(2))*NQPTS + 2*IBNDS(1) +
		//                       2*MNEQN + IBNDS(3) + 5*IBNDS(2) + 2*IBNDS(4)
		
		//            ZWORK  - COMPLEX ARRAY
		//                     A WORKING VECTOR OF SIZE AT LEAST
		//                         MNEQN + 2*IBNDS(2)
		
		//            LWORK  - LOGICAL ARRAY
		//                     A WORKING VECTOR OF SIZE AT LEAST
		//                         3*IBNDS(1) + IBNDS(2)
		
		//            OCH    - INTEGER
		//                     DEFINES AN OUTPUT CHANNEL THAT MAY BE USED FOR
		//                     WRITING THE FILES <JBNM>pl, <JBNM>gm, <JBNM>ph.
		
		//         ON EXIT
		//            RFARG  - REAL
		//                     EXIT VALUE IS PI*(ENTRY VALUE)
		
		//            IGEOM  - INTEGER ARRAY
		//                     A VECTOR OF SIZE AT LEAST 
		//                          IBNDS(1) + 4;
		//                     STORES DATA RELATING TO THE ARC SUBDIVISIONS THAT
		//                     HAVE TAKEN PLACE.
		
		//            RGEOM  - REAL ARRAY
		//                     A VECTOR OF SIZE AT LEAST 
		//                          3*IBNDS(1)+2;
		//                     STORES DATA RELATING TO THE ARC SUBDIVISIONS THAT
		//                     HAVE TAKEN PLACE AND THE ARGUMENTS OF SUB-ARC END
		//                     POINTS ON THE UNIT DISC.
		
		//            ISNPH  - INTEGER ARRAY
		//                     A SOLUTION VECTOR OF SIZE AT LEAST 
		//                          3*IBNDS(1)+6;
		//                     STORES DATA DEFINING THE FINAL POLYNOMIAL DEGREES
		//                     ON THE SUB-ARCS, THE JACOBI INDEX FOR EACH SUB-ARC
		//                     AND POINTERS TO THE SOLUTIONS STORED IN RSNPH.
		
		//            RSNPH  - REAL ARRAY
		//                     A SOLUTION VECTOR OF SIZE AT LEAST
		//                         IBNDS(1)+2*MNEQN+3*IBNDS(2)*(1+2*NQPTS);
		//                     STORES DATA DEFINING THREE-TERM RECURRENCE
		//                     SCHEMES, ELEMENTARY GAUSS-JACOBI QUADRATURE RULES,
		//                     THE JACOBI COEFFICIENTS FOR THE BOUNDARY
		//                     CORRESPONDENCE FUNCTION AND ITS DERIVATIVE AND
		//                     THE ERRORS IN MODULUS ON EACH SUB-ARC.
		
		//            IER    - INTEGER
		//                     IF IER > 0 THEN AN ABNORMAL EXIT HAS OCCURRED;
		//                     A MESSAGE TO DESCRIBE THE ERROR IS AUTOMATICALLY
		//                     WRITTEN ON THE STANDARD OUTPUT CHANNEL AND THE
		//                     LISTING FILE <JBNM>pl.
		//                     IER=0 - NORMAL EXIT.
		//                     IER>0 - ABNORMAL EXIT; THE ERROR MESSAGE SHOULD
		//                             BE SELF EXPLANATORY.
		                                  
		
		// 4.     SUBROUTINES OR FUNCTIONS NEEDED
		//              - THE CONFPACK LIBRARY.
		//              - THE REAL FUNCTION R1MACH, WHICH IS A MACHINE CONSTANTS 
		//                ROUTINE OBTAINED FROM THE PORT LIBRARY.   
		//                IT MUST BE ADJUSTED TO SUIT EACH PARTICULAR MACHINE.  
		//                IF YOUR MACHINE ISN'T LISTED IN R1MACH THEN YOU'LL  
		//                HAVE TO WRITE YOUR OWN VERSION, BUT NOTE THAT CONFPACK 
		//                ONLY USES R1MACH(1), R1MACH(2) AND R1MACH(4).
		//              - THE FOLLOWING LINPACK ROUTINES:
		//                    ISAMAX   SASUM    SAXPY   SDOT    SGECO
		//                    SGEFA    SGEDI    SGESL   SSCAL   SSWAP
		//              - THE FOLLOWING QUADPACK ROUTINES:
		//                    QAWS     QAWSE    QC25S   QCHEB   QK15W
		//                    QMAC     QMOMO    QSORT   QWGTS
		//              - THE USER SUPPLIED COMPLEX FUNCTIONS PARFUN AND DPARFN
		//                WHICH DEFINE THE PARAMETRIC FUNCTION FOR THE PHYSICAL
		//                BOUNDARY AND THE DERIVATIVE OF THE PARAMETRIC FUNCTION.
		//                THE PARAMETRIC FUNCTION DEFINING THE K'TH ANALYTIC ARC
		//                SHOULD HAVE THE SUBROUTINE HEADING
		
		//                    COMPLEX FUNCTION PARFUN(K,T)
		//                    INTEGER K
		//                    COMPLEX T
		
		//                WITH THE REAL PARAMETER INTERVAL -1 < REAL(T) < +1
		//                BEING MAPPED TO THE PHYSICAL ARC.  A SIMILAR HEADING
		//                SHOULD BE GIVEN FOR THE DERIVATIVE DPARFN.  THE PRE-
		//                PROCESSING PROGRAM PARGEN IS AVAILABLE TO HELP WITH
		//                THE CREATION OF PARFUN AND DPARFN.
		
		
		// 5.     FURTHER COMMENTS
		//             A SUMMARY LISTING OF ACTIONS TAKEN IS AUTOMATICALLY
		//             WRITTEN ON THE STANDARD OUTPUT CHANNEL.
		
		// ......................................................................
		//     AUTHOR: DAVID HOUGH, ETH, ZUERICH
		//     LAST UPDATE: 15 JULY 1990
		// ......................................................................
		     
		//     LOCAL VARAIBLES
		
	   int TNSUA[] = new int[1];
	   int NTEST[] = new int[1];
	   int IMXER[] = new int[1];
	   int I,INDEG,J,MDGPO,MNJXS,MNQUA,MNSUA,MQIN1,NCOLL,
		     NEFF,NEQNS,NROWS,ORDSG;
		int SOLCO = 0;
		int QIERC[] = new int[7];
		int QIERR[] = new int[7];
		//int QIERC(0:6),QIERR(0:6)
		//DATA QIERC/7*0E+0/
		
		final double SFACT = 0.8;
		final double QFACT = 0.1;
		double MCQER[] = new double[1];
		double MQERR[] = new double[1];
		double RCOND[] = new double[1];
		double ESTOL[] = new double[1];
		double MXERM[] = new double[1];
		double AQTOL,CONST,GAQTL,GLGTL,GRQTL,GSUPE,GTGTE,LGTOL,
		     MCHEP,PI,RQTOL,SSUPE,
		     TGTER,TOLNR;
		double SOLUNB[][] = new double[MNEQN][1];
		double A[][] = new double[MNEQN][MNEQN];
		
		double ZMXER[] = new double[2];
		//COMPLEX ZMXER
		
		final boolean INIBT = true;
		boolean ACCPT[] = new boolean[1];
		boolean NUQTL[] = new boolean[1];
		boolean GACPT,REFLN;
	
		//EXTERNAL AXION1,ANGLE7,ASQUC7,BCFVTF,CPJAC3,CSCAL3,ICOQR1,IGNLVL,
		//         LINSEG,LNSY11,OPQUD1,OUPTGM,OUPTPH,R1MACH,RECON,RESCAL,RSLT80,
		//         RSLT71,RSLT72,RSLT83,RSLT84,SETIGL,SGECO,SGEDI,SGESL,TESMD9,
		//         TSJAC3,UPCOQ1,UPJAC1,WRHEAD,WRTAIL
		
		//**** OUTPUT CONFPACK HEADING
		
		WRHEAD(1,0,null);
		//WRHEAD(1,OCH)
		
		if (NQPTS < 1) {
		    IER[0]=3;
		    WRTAIL(1,0,IER[0],null);
		    return;
		} 
		
		//**** INITIALISE SOME VARIABLES
		
		if (ISYGP == 0 || ISYGP == -1) {
		    ORDSG=1;
		    REFLN=false;
		}
		else {
		    ORDSG=Math.abs(ISYGP);
		    REFLN=(ISYGP < -1);
		}
		
		if ((NARCS % ORDSG) != 0) {
		    IER[0]=55;
		    WRTAIL(1,0,IER[0],null);
		    return;
		}
		
		SOLCO=0;
		NEFF=0;
		MCHEP=EPS;
		TOLNR=Math.sqrt(MCHEP);
		NJIND=NARCS+1;
		TNGQP=NQPTS*NJIND;
		MDGPO=NQPTS-1;
		MNSUA=IBNDS[0];
		MNJXS=IBNDS[1];
		MQIN1=IBNDS[2];
		MNQUA=IBNDS[3];
		if (2*NARCS > MNSUA) {
		    IER[0]=1;
		    WRTAIL(1,0,IER[0],null);
		    return;
		}
		if (NARCS+1 > MNJXS) {
		    IER[0]=2;
		    WRTAIL(1,0,IER[0],null);
		    return;            
		}
		if (TSTNG != 1) {
		    TSTNG=0;
		}
		GSUPE=MAXER;
		GTGTE=GSUPE*SFACT;
		GAQTL=QFACT*GTGTE;
		if (GAQTL < 5*MCHEP) {
		    GAQTL=5*MCHEP;
		    GTGTE=GAQTL/QFACT;
		    GSUPE=GTGTE/SFACT;
		}
		GLGTL=Math.log(1+GTGTE);
		GRQTL=GAQTL;
		IGEOM[0]=NARCS;
		IGEOM[1]=NQPTS;
		IGEOM[3]=MNSUA;
		ISNPH[0]=NARCS;
		ISNPH[1]=NQPTS;
		ISNPH[4]=MNSUA;
		ISNPH[5]=MNEQN;
		RGEOM[0]=GSUPE;
		RGEOM[1]=GLGTL;
		
		//**** ASSIGN THE JACOBI INDECES FOR EACH ARC.
		
		ANGLE7(JACIN,NARCS,INTER);
		JACIN[NJIND-1]=0.0;
		
		//**** SET SUB-TOLERANCES AND INDEG
		
		if (INCST && GSUPE <= 3.16E-2) {
		
		    //****   FOLLOW INCREMENTAL STRATEGY
		
		    SSUPE=0.1;
		    TGTER=SSUPE*SFACT;
		    AQTOL=TGTER*QFACT;
		    LGTOL=Math.log(1.0+TGTER);
		    RQTOL=AQTOL;
		    INDEG=Math.min(3,NQPTS-1);
		}
		else {
		
		    //****   SUB-TOLERANCES SAME AS GLOBAL TOLERANCES, INDEG DETERMINED
		    // ****   ACCORDING TO ACCURACY REQUESTED
		
		    SSUPE=GSUPE;
		    TGTER=GTGTE;
		    AQTOL=GAQTL;
		    LGTOL=GLGTL;
		    RQTOL=GRQTL;
		    INDEG=(int)Math.round((-Math.log10(TGTER)))+2;
		    INDEG=Math.min(INDEG,NQPTS-1);
		}
		
		//**** ASSIGN THE LOGICAL LINE SEGMENT TYPE TO EACH ARC.
		
		LINSEG(LNSEG,NARCS);
		
		//**** LIST THE INPUT ARGUMENTS AND ASSOCIATED QUANTITIES
		
		RSLT80(GSUPE,MAXER,GAQTL,INTER,NARCS,ORDSG,NQPTS,
		     INCST,INDEG,RFARC,RFARG[0],CENTR,JACIN,LNSEG,
		     TSTNG,OULVL,IBNDS,MNEQN);
		PI=Math.PI;
		RFARG[0]=RFARG[0]*PI;
		
		//**** SET UP THE GAUSS-JACOBI AND GAUSS-LEGENDRE QUADRATURE DATA AND 
		//**** STORE IN ARRAYS QUPTS AND QUWTS.  SET UP THREE TERM RECURRENCE
		//**** COEFFICIENTS AND STORE IN ACOEF, BCOEF.  DETERMINE ZEROTH
		//**** MOMENTS OF JACOBI DISTRIBUTIONS AND STORE IN H0VAL. 
		//**** ALSO SET UP THREE TERM RECURRENCE COEFFICIENTS AND ZEROTH MOMENTS
		//**** FOR THE INTEGRATED POLYNOMIALS, STORING RESULTS IN AICOF,BICOF
		//**** AND HIVAL.
		      
		OPQUD1(NJIND,NQPTS,JACIN,ACOEF,BCOEF,
		     H0VAL,AICOF,BICOF,HIVAL,QUPTS,
		     QUWTS,WORK,IER);
		if (IER[0] > 0) {
			WRTAIL(1,0,IER[0],null);
		    return;            
		}
		J=1-NQPTS;
		for (I=1; I <= NJIND; I++) {
		    J=J+NQPTS;
		    A1COF[I-1]=ACOEF[J-1];
		    B1COF[I-1]=BCOEF[J-1];
		} // for (I=1; I <= NJIND; I++)
		System.out.println("BASIC GAUSS QUADRATURE DATA DONE:");
		
		//**** SET UP THE COEFFICIENTS IN THE THREE TERM RECURRENCE FORMULAE
		//**** FOR THE PRINCIPAL SINGULAR INTEGRALS ASSOCIATED WITH THE VARIOUS
		//**** JACOBI WEIGHT FUNCTIONS AND THEIR ORTHONORMAL POLYNOMIALS; STORE
		//**** THESE COEFFICIENTS IN AQCOF, BQCOF AND CQCOF
		
		ASQUC7(AQCOF,BQCOF,CQCOF,JACIN,NJIND,NQPTS);
		System.out.println("DATA FOR SINGULAR INTEGRALS DONE:");
		
		//**** SET UP THE A PRIORI COLUMN SCALE FACTORS, STORED IN COLSC.
		
		CSCAL3(COLSC,NQPTS,NJIND,ACOEF,BCOEF,
		     H0VAL,QUPTS,QUWTS,JACIN,WORK,
		     WORKT,WORKQ);
		
		// **** SET UP THE ARRAY RIGLL OF REFERENCE IGNORE LEVELS.
		
		IGNLVL(RIGLL,COLSC,ACOEF,BCOEF,H0VAL,JACIN,NJIND,NQPTS,IER);
		if (IER[0] > 0) {
			WRTAIL(1,0,IER[0],null);
		    return; 
		}
		
		// **** SET UP THE ARRAY OF COLLOCATION POINTS PARAMETER VALUES, COLPR,
		// **** THE ARRAY OF COLLOCATION POINTS ZCOLL AND THE ARRAYS LOSUB AND 
		//**** HISUB NEEDED TO ACCESS COLPR AND ZCOLL CORRECTLY.  INITIALISE
		//**** DGPOL AND UPDATE LNSEG FOR ARC HALVING. 
		
		CPJAC3(NARCS,NQPTS,INDEG,DGPOL,JACIN,
		     ACOEF,BCOEF,DIAG,SDIAG,TNSUA,
		     LOSUB,HISUB,JATYP,PARNT,MIDPT,
		     HALEN,COLPR,ZCOLL,LNSEG,LOOLD,
		     HIOLD,EPS,IER,INIBT);
		if (IER[0] > 0) {
			WRTAIL(1,0,IER[0],null);
		    return; 
		}
		NCOLL=HISUB[TNSUA[0]-1];
		NEQNS=NCOLL+1;
		NROWS=NCOLL/ORDSG+1;
		if (NEQNS > MNEQN) {
		    IER[0]=8;
		    WRTAIL(1,0,IER[0],null);
		    return;     
		}
		System.out.println("COLLOCATION POINT CHOICE DONE:");
		
		//**** SET UP THE COMPOSITE GAUSSIAN QUADRATURE RULES, STORING ABSCISSAE
		//**** AND WEIGHTS IN QCOMX AND QCOMW.  SET UP ARRAYS NQUAD,LOQSB
		//**** NEEDED TO ACCESS THESE DATA.  RECORD MAXIMUM QUADRATURE ERRORS
		//**** FOR COLUMN SCALED INTEGRALS IN ARRAY TOLOU.
		
		ICOQR1(NARCS,NJIND,NQPTS,MDGPO,MQIN1,AQTOL,QUPTS,
		     QUWTS,JACIN,MIDPT,HALEN,ACOEF,
		     BCOEF,H0VAL,COLSC,NQUAD,LOQSB,
		     QCOMX,QCOMW,MNQUA,TOLOU,MCQER,XENPT,
		     XIVAL,XIDST,IER);
		NUQTL[0]=false;
		if (IER[0] > 0) {
			WRTAIL(1,0,IER[0],null);
		    return;    
		}
		System.out.println("COMPOSITE GAUSSIAN RULES DONE:");
		
		//**** SET UP LINEAR ALGEBRAIC SYSTEM.
		
	    while (true) {
		    SOLCO=SOLCO+1;
		    System.out.println("********SOLUTION "+ SOLCO+ " ******** " + NROWS + " EQUATIONS");
		
		    LNSY11(MATRX,SOLUN,MNEQN,NCOLL,ORDSG,REFLN,NQPTS,
		     TNSUA[0],JATYP,PARNT,DGPOL,LOSUB,
		     HISUB,NQUAD,LOQSB,TOLNR,MIDPT,
		     HALEN,H0VAL,COLSC,ACOEF,BCOEF,
		     COLPR,QCOMX,QCOMW,CENTR,ZCOLL,INTER,
		     LNSEG,WORK,QIERR,MQERR,JACIN,A1COF,
		     B1COF,AQTOL,RQTOL,AQCOF,BQCOF,CQCOF,
		     LOOLD,HIOLD);
		
		    for (I=0; I <= 6; I++) {
		        QIERC[I]=QIERC[I]+QIERR[I];
		    }
		    System.out.println("LINEAR SYSTEM SET UP DONE:");
		
		    //**** SOLVE LINEAR SYSTEM BY GAUSSIAN ELIMINATION USING LINPACK
		    // LAPACK equivalent of LINPACK SGECO are DLANGE, DGETRF, and DGECON
		    // LU factorization and condition estimation of a general matrix.
		    for (I = 0; I < MNEQN; I++) {
		    	for (J = 0; J < MNEQN; J++) {
		    		A[I][J] = MATRX[I][J][1];
		    	}
		    }
		    LinearEquations2 le2 = new LinearEquations2();
    	    GeneralizedEigenvalue ge = new GeneralizedEigenvalue();
    	    double anorm;
    	    int iwork[] = new int[NROWS];
    	    int info[] = new int[1];
    	    anorm = ge.dlange('1', MNEQN, MNEQN, A, MNEQN, WORK2);
    	    le2.dgetrf(MNEQN, MNEQN, A, MNEQN, IPIVT, info);
    	    if (info[0] > 0) {
    	    	MipavUtil.displayError("In dgetrf factor U is exactly singular");
    	    }
    	    le2.dgecon('1', NROWS, A, MNEQN, anorm, RCOND, WORK2, iwork, info);
    	    // Reciprocal condition number of A in 1-norm.
		    //SGECO(MATRX(1,1,2),MNEQN,NROWS,IPIVT,RCOND,WORK2);
		    if (RCOND[0] == 0.0) {
		        IER[0]=15;
		        SOLCO=SOLCO-1;
		        if (SOLCO >= 1) {
				
				    // ****   COMPUTE THE BOUNDARY CORRESPONDENCE COEFFICIENTS BCFSN AND THE
				    // ****   ARGUMENTS OF ALL SUBARC END POINTS ON THE UNIT DISC,
				    // ****   AS REQUIRED BY SUBSEQUENT PROCESSING ROUTINES.
				
				    BCFVTF(BCFSN,VTARG,DGPOL,JATYP,
				        LOSUB,PARNT,RFARC,TNSUA[0],H0VAL,JACIN,
				        RFARG[0],SOLUN);
				
				    // ****   OUTPUT DATA REQUIRED FOR POST-PROCESSING.
				
				    IGEOM[2]=TNSUA[0];
				    ISNPH[2]=TNSUA[0];
				    ISNPH[3]=NEQNS;
		        } // if (SOLCO >= 1)
				  
		        WRTAIL(1,0,IER[0],null);
			    return;     
		    } // if (RCOND[0] == 0.0) 
		    // LINPACK SGESL is equivalent to LAPACK DGETRS
		    // Solves a general system of linear equations, after factorization by SGECO or SGEFA
		    // SOLUN has the solution vector on exit.
		    for (I = 0; I < MNEQN; I++) {
		    	SOLUNB[I][0] = SOLUN[I];
		    }
		    le2.dgetrs('N', NROWS, 1, A, MNEQN, IPIVT, SOLUNB, MNEQN, info);
		    for (I = 0; I < MNEQN; I++) {
		    	SOLUN[I] = SOLUNB[I][0];
		    }
		    //SGESL(MATRX(1,1,2),MNEQN,NROWS,IPIVT,SOLUN,0)
		    for (I = 0; I < MNEQN; I++) {
		    	for (J = 0; J < MNEQN; J++) {
		    		MATRX[I][J][1] = A[I][J];
		    	}
		    }
		    NEFF=NEFF+NROWS*NROWS*NROWS;
		    System.out.println("LINEAR SYSTEM SOLUTION DONE:");
		
		    // **** RECONSTITUTE FULL SOLUTION VECTOR
		
		    if (ORDSG > 1) {
		        RECON(ORDSG,REFLN,NCOLL,TNSUA[0],LOSUB,HISUB,SOLUN);
		    } // if (ORDSG > 1)
		    CONST=SOLUN[NEQNS-1];
		
		    // **** SET UP THE ARRAY WORK2 OF ACTUAL COEFFICIENT IGNORE LEVELS
		
		    SETIGL(WORK2,HISUB,JATYP,LOSUB,
		     NQPTS,RIGLL,TNSUA[0]);
		
		     // **** DETERMINE THE ACTIONS THAT HAVE TO BE TAKEN ON EACH ARC
		
		     AXION1(AXION,NEWDG,SOLUN,MDGPO,TNSUA[0],
		     DGPOL,LOSUB,HISUB,RIGLL,LGTOL,ACCPT,
		     JACIN,JATYP,NJIND,NEWHL,ESTOL,IER);
		     ESTOL[0]=ESTOL[0]/SFACT;
		     if (IER[0] > 0) {
		    	 if (SOLCO >= 1) {
						
					    // ****   COMPUTE THE BOUNDARY CORRESPONDENCE COEFFICIENTS BCFSN AND THE
					    // ****   ARGUMENTS OF ALL SUBARC END POINTS ON THE UNIT DISC,
					    // ****   AS REQUIRED BY SUBSEQUENT PROCESSING ROUTINES.
					
					    BCFVTF(BCFSN,VTARG,DGPOL,JATYP,
					        LOSUB,PARNT,RFARC,TNSUA[0],H0VAL,JACIN,
					        RFARG[0],SOLUN);
					
					    // ****   OUTPUT DATA REQUIRED FOR POST-PROCESSING.
					
					    IGEOM[2]=TNSUA[0];
					    ISNPH[2]=TNSUA[0];
					    ISNPH[3]=NEQNS;
			        } // if (SOLCO >= 1)
					  
			        WRTAIL(1,0,IER[0],null);
				    return;     
		     } // if (IER[0] > 1)
		     System.out.println("DECISIONS FOR EACH ARC DONE:");
		     System.out.println("EFFECTIVE STOPPING TOLERANCE: " + ESTOL[0]);
		     if (ACCPT[0] && ESTOL[0] <= GSUPE) {
		         GACPT=true;
		     }
		     else {
		         GACPT=false;
		     }
		
		      if (GACPT) {
		
		          // OUTPUT RESULTS
		
		          if (OULVL < 4) {
		              RSLT72(QIERC,RCOND[0],CONST,NROWS,DGPOL,JATYP,
		                     PARNT,TNSUA[0],INTER,MQERR[0],MCQER[0],AXION,
		                     NEWDG,NJIND,NQUAD,TOLOU,LGTOL,SOLCO);
		          }
		          else {
		              RSLT71(QIERC,RCOND[0],SOLUN,NEQNS,LOSUB,
		                     HISUB,COLSC,NQPTS,JATYP,
		                     PARNT,TNSUA[0],INTER,MQERR[0],MCQER[0],WORK2,
		                     AXION,NEWDG,NJIND,JACIN,
		                     NQUAD,TOLOU,LGTOL,SOLCO);
		          }
		          NEFF=(int)Math.round(Math.pow((double)(NEFF),3.3333333E-1));
		          System.out.println("\n****THE SOLUTION IS ACCEPTED****");
		          System.out.println("\nEFFECTIVE SIZE OF ALL SYSTEMS: " + NEFF);
		          if (INTER) {
		              System.out.println("ZERO " + CONST);
		          }
		          else {
		              System.out.println("CAPACITY: " + (Math.exp(-CONST)));
		          }
		
                  Preferences.debug("\n",Preferences.DEBUG_ALGORITHM);
                  Preferences.debug("****THE SOLUTION IS ACCEPTED****\n",Preferences.DEBUG_ALGORITHM);
		          Preferences.debug("EFFECTIVE SIZE OF ALL SYSTEMS: " + NEFF + "\n\n");
		          break;
		      } // if (GACPT)
		      else { // !GACPT
		          if (ACCPT[0] || ESTOL[0] <= SSUPE) {
		
		              // SOLUTION AT INTERMEDIATE ACCURACY IS ACCEPTED; SET TOLERANCES
		              // FOR GREATER ACCURACY AND RE-ASSESS UPDATING ACTIONS BEFORE 
		              // CONTINUING
		
		              SSUPE=0.1*Math.min(SSUPE,ESTOL[0]);
		              TGTER=SFACT*SSUPE;
		              if (TGTER <= 2.0*GTGTE) {
		                  TGTER=GTGTE;
		              }
		              AQTOL=TGTER*QFACT;
		              NUQTL[0]=true;
		              LGTOL=Math.log(1.0+TGTER);
		              RQTOL=AQTOL;
		              I=(int)Math.round(-Math.log10(TGTER))+2;
		              INDEG=Math.min(I,MDGPO);
		
		              // DETERMINE THE ACTIONS THAT HAVE TO BE TAKEN ON EACH ARC
		
		              AXION1(AXION,NEWDG,SOLUN,MDGPO,
		                     TNSUA[0],DGPOL,LOSUB,HISUB,
		                     RIGLL,LGTOL,ACCPT,JACIN,
		                     JATYP,NJIND,NEWHL,ESTOL,IER);
		              ESTOL[0]=ESTOL[0]/SFACT;
		              if (IER[0] > 0) {
		 		    	 if (SOLCO >= 1) {
		 						
		 					    // ****   COMPUTE THE BOUNDARY CORRESPONDENCE COEFFICIENTS BCFSN AND THE
		 					    // ****   ARGUMENTS OF ALL SUBARC END POINTS ON THE UNIT DISC,
		 					    // ****   AS REQUIRED BY SUBSEQUENT PROCESSING ROUTINES.
		 					
		 					    BCFVTF(BCFSN,VTARG,DGPOL,JATYP,
		 					        LOSUB,PARNT,RFARC,TNSUA[0],H0VAL,JACIN,
		 					        RFARG[0],SOLUN);
		 					
		 					    // ****   OUTPUT DATA REQUIRED FOR POST-PROCESSING.
		 					
		 					    IGEOM[2]=TNSUA[0];
		 					    ISNPH[2]=TNSUA[0];
		 					    ISNPH[3]=NEQNS;
		 			        } // if (SOLCO >= 1)
		 					  
		 			        WRTAIL(1,0,IER[0],null);
		 				    return;     
		 		     } // if (IER[0] > 1)
		             System.out.println("DECISIONS FOR EACH ARC RE-DONE:");
		          } //  if (ACCPT[0] || ESTOL[0] <= SSUPE)
		
		          // OUTPUT RESULTS
		
		          if (OULVL < 4) {
		              RSLT72(QIERC,RCOND[0],CONST,NROWS,DGPOL,JATYP,
		                     PARNT,TNSUA[0],INTER,MQERR[0],MCQER[0],AXION,
		                     NEWDG,NJIND,NQUAD,TOLOU,
		                     LGTOL,SOLCO);
		          }
		          else {
		              RSLT71(QIERC,RCOND[0],SOLUN,NEQNS,LOSUB,
		                     HISUB,COLSC,NQPTS,JATYP,
		                     PARNT,TNSUA[0],INTER,MQERR[0],MCQER[0],WORK2,
		                     AXION,NEWDG,NJIND,JACIN,
		                     NQUAD,TOLOU,LGTOL,SOLCO);
		          }
		          System.out.println("EFFECTIVE STOPPING TOLERANCE : " + ESTOL[0]);
		          if (RCOND[0] < 5.0*MCHEP) {
		              IER[0]=16;
		              if (SOLCO >= 1) {
	 						
	 					    // ****   COMPUTE THE BOUNDARY CORRESPONDENCE COEFFICIENTS BCFSN AND THE
	 					    // ****   ARGUMENTS OF ALL SUBARC END POINTS ON THE UNIT DISC,
	 					    // ****   AS REQUIRED BY SUBSEQUENT PROCESSING ROUTINES.
	 					
	 					    BCFVTF(BCFSN,VTARG,DGPOL,JATYP,
	 					        LOSUB,PARNT,RFARC,TNSUA[0],H0VAL,JACIN,
	 					        RFARG[0],SOLUN);
	 					
	 					    // ****   OUTPUT DATA REQUIRED FOR POST-PROCESSING.
	 					
	 					    IGEOM[2]=TNSUA[0];
	 					    ISNPH[2]=TNSUA[0];
	 					    ISNPH[3]=NEQNS;
	 			        } // if (SOLCO >= 1)
	 					  
	 			        WRTAIL(1,0,IER[0],null);
	 				    return;     
		          } // if (RCOND[0] < 5.0*MCHEP)
		          else if (RCOND[0] < AQTOL) {
		              NUQTL[0]=true;
		              AQTOL=0.1*RCOND[0];
		              if (AQTOL < 5.0*MCHEP) AQTOL=5.0*MCHEP;
		          }
		
		          // IMPLEMENT UPDATING PROCEDURES.
		          // FIRST UPDATE THE COLLOCATION PARAMETERS AND OTHER DATA
		          // RELATING TO SUB-ARC DEFINITIONS.
		
		          UPJAC1(NQPTS,NJIND,INDEG,AXION,DGPOL,
		          NEWDG,ACOEF,BCOEF,DIAG,
		          SDIAG,TNSUA[0],MNSUA,LOSUB,HISUB,
		          JATYP,PARNT,MIDPT,HALEN,
		          COLPR,ZCOLL,LNSEG,PNEWQ,EPS,IER,
		          WORK,NEWHL,RCOPY,ICOPY,
		          LCOPY,LOOLD,HIOLD);
		          if (IER[0] > 0) {
		        	  if (SOLCO >= 1) {
	 						
	 					    // ****   COMPUTE THE BOUNDARY CORRESPONDENCE COEFFICIENTS BCFSN AND THE
	 					    // ****   ARGUMENTS OF ALL SUBARC END POINTS ON THE UNIT DISC,
	 					    // ****   AS REQUIRED BY SUBSEQUENT PROCESSING ROUTINES.
	 					
	 					    BCFVTF(BCFSN,VTARG,DGPOL,JATYP,
	 					        LOSUB,PARNT,RFARC,TNSUA[0],H0VAL,JACIN,
	 					        RFARG[0],SOLUN);
	 					
	 					    // ****   OUTPUT DATA REQUIRED FOR POST-PROCESSING.
	 					
	 					    IGEOM[2]=TNSUA[0];
	 					    ISNPH[2]=TNSUA[0];
	 					    ISNPH[3]=NEQNS;
	 			        } // if (SOLCO >= 1)
	 					  
	 			        WRTAIL(1,0,IER[0],null);
	 				    return;     
		          } // if (IER[0] > 0)
		          System.out.println("ARC REFINEMENTS DONE:");
		          NCOLL=HISUB[TNSUA[0]-1];
		          NEQNS=NCOLL+1;
		          NROWS=NCOLL/ORDSG+1;
		          if (NEQNS > MNEQN) {
		              IER[0]=18;
		              if (SOLCO >= 1) {
	 						
	 					    // ****   COMPUTE THE BOUNDARY CORRESPONDENCE COEFFICIENTS BCFSN AND THE
	 					    // ****   ARGUMENTS OF ALL SUBARC END POINTS ON THE UNIT DISC,
	 					    // ****   AS REQUIRED BY SUBSEQUENT PROCESSING ROUTINES.
	 					
	 					    BCFVTF(BCFSN,VTARG,DGPOL,JATYP,
	 					        LOSUB,PARNT,RFARC,TNSUA[0],H0VAL,JACIN,
	 					        RFARG[0],SOLUN);
	 					
	 					    // ****   OUTPUT DATA REQUIRED FOR POST-PROCESSING.
	 					
	 					    IGEOM[2]=TNSUA[0];
	 					    ISNPH[2]=TNSUA[0];
	 					    ISNPH[3]=NEQNS;
	 			        } // if (SOLCO >= 1)
	 					  
	 			        WRTAIL(1,0,IER[0],null);
	 				    return;     
		          } // if (NEQNS > MNEQN)
		
		          // NEXT UPDATE THE COMPOSITE QUADRATURE RULES
		
		          UPCOQ1(NARCS,NJIND,NQPTS,MDGPO,MQIN1,AQTOL,QUPTS,
		          QUWTS,JACIN,MIDPT,HALEN,
		          ACOEF,BCOEF,H0VAL,COLSC,
		          NQUAD,LOQSB,QCOMX,QCOMW,
		          MNQUA,TOLOU,MCQER,XENPT,XIVAL,
		          XIDST,TNSUA[0],PNEWQ,NEWQU,JATYP,
		          PARNT,NUQTL,IER);
		          if (IER[0] > 0) {
		        	  if (SOLCO >= 1) {
	 						
	 					    // ****   COMPUTE THE BOUNDARY CORRESPONDENCE COEFFICIENTS BCFSN AND THE
	 					    // ****   ARGUMENTS OF ALL SUBARC END POINTS ON THE UNIT DISC,
	 					    // ****   AS REQUIRED BY SUBSEQUENT PROCESSING ROUTINES.
	 					
	 					    BCFVTF(BCFSN,VTARG,DGPOL,JATYP,
	 					        LOSUB,PARNT,RFARC,TNSUA[0],H0VAL,JACIN,
	 					        RFARG[0],SOLUN);
	 					
	 					    // ****   OUTPUT DATA REQUIRED FOR POST-PROCESSING.
	 					
	 					    IGEOM[2]=TNSUA[0];
	 					    ISNPH[2]=TNSUA[0];
	 					    ISNPH[3]=NEQNS;
	 			        } // if (SOLCO >= 1)
	 					  
	 			        WRTAIL(1,0,IER[0],null);
	 				    return;     
		          } // if (IER[0] > 0)
		          System.out.println("QUADRATURE UPDATES DONE:");
		          continue;
		      } // else !GACPT 
	    } // while (true)
		
		if (OULVL == 2 || OULVL == 3) {
		    RSLT71(QIERC,RCOND[0],SOLUN,NEQNS,LOSUB,
		     HISUB,COLSC,NQPTS,JATYP,PARNT,TNSUA[0],
		     INTER,MQERR[0],MCQER[0],WORK2,AXION,NEWDG,NJIND,
		     JACIN,NQUAD,TOLOU,LGTOL,SOLCO);
		} // if (OULVL == 2 || OULVL == 3)
		
		// ESTIMATE MAXIMUM ERROR IN MODULUS.
		
		System.out.println();
		System.out.println("ERRORS IN MODULUS STARTED:");
		
		TSJAC3(LOTES,HITES,COLPR,ZCOLL,
		  NQPTS,NTEST,ORDSG,TNSUA[0],TSTNG,DGPOL,JATYP,
		  PARNT,AICOF,BICOF,DIAG,HALEN,
		  JACIN,MIDPT,SDIAG,IER);
	    if (IER[0] > 0) {
			  if (SOLCO >= 1) {
					
				    // ****   COMPUTE THE BOUNDARY CORRESPONDENCE COEFFICIENTS BCFSN AND THE
				    // ****   ARGUMENTS OF ALL SUBARC END POINTS ON THE UNIT DISC,
				    // ****   AS REQUIRED BY SUBSEQUENT PROCESSING ROUTINES.
				
				    BCFVTF(BCFSN,VTARG,DGPOL,JATYP,
				        LOSUB,PARNT,RFARC,TNSUA[0],H0VAL,JACIN,
				        RFARG[0],SOLUN);
				
				    // ****   OUTPUT DATA REQUIRED FOR POST-PROCESSING.
				
				    IGEOM[2]=TNSUA[0];
				    ISNPH[2]=TNSUA[0];
				    ISNPH[3]=NEQNS;
		        } // if (SOLCO >= 1)
				  
		        WRTAIL(1,0,IER[0],null);
			    return;     
		  } // if (IER[0] > 0)
		
		  for (I = 0; I < MNEQN; I++) {
			  for (J = 0; J < MNEQN; J++) {
				  A[I][J] = MATRX[I][J][1];
			  }
		  }
		  TESMD9(WORK2,A,SOLUN,MNEQN,NCOLL,
		     NTEST[0],NQPTS,TNSUA[0],JATYP,PARNT,DGPOL,
		     LOSUB,HISUB,LOTES,HITES,NQUAD,
		     LOQSB,TOLNR,MIDPT,HALEN,H0VAL,
		     COLSC,ACOEF,BCOEF,COLPR,QCOMX,
		     QCOMW,CENTR,ZCOLL,INTER,LNSEG,WORK,
		     QIERR,MQERR,JACIN,A1COF,B1COF,AQTOL,RQTOL,
		     AQCOF,BQCOF,CQCOF,MXERM,IMXER,ZMXER,
		     ERARC,ORDSG,REFLN);
		  for (I = 0; I < MNEQN; I++) {
			  for (J = 0; J < MNEQN; J++) {
				  MATRX[I][J][1] = A[I][J];
			  }
		  }
		
		  System.out.println("ERRORS IN MODULUS DONE:");
		  System.out.println("MAXIMUM ERROR AT TEST POINTS: " + MXERM[0]);
		  for (I=0; I <= 6; I++) {
		      QIERC[I]=QIERC[I]+QIERR[I];
		  }
		  if ((OULVL%2) == 1) {
		      RSLT84(WORK2,TNSUA[0],MXERM[0],ZMXER,IMXER[0],LOTES,
		      HITES,QIERC,PARNT,ORDSG);
		  }
		  else {
		      RSLT83(ERARC,TNSUA[0],MXERM[0],ZMXER,IMXER[0],QIERC,
		      PARNT,ORDSG);
		  }
		
		  // RESCALE SOLUTIONS TO OBTAIN STANDARD JACOBI COEFFICIENTS
		  RESCAL(NQPTS,TNSUA[0],LOSUB,HISUB,JATYP,SOLUN,COLSC);
		  
		  if (SOLCO >= 1) {
				
			    // ****   COMPUTE THE BOUNDARY CORRESPONDENCE COEFFICIENTS BCFSN AND THE
			    // ****   ARGUMENTS OF ALL SUBARC END POINTS ON THE UNIT DISC,
			    // ****   AS REQUIRED BY SUBSEQUENT PROCESSING ROUTINES.
			
			    BCFVTF(BCFSN,VTARG,DGPOL,JATYP,
			        LOSUB,PARNT,RFARC,TNSUA[0],H0VAL,JACIN,
			        RFARG[0],SOLUN);
			
			    // ****   OUTPUT DATA REQUIRED FOR POST-PROCESSING.
			
			    IGEOM[2]=TNSUA[0];
			    ISNPH[2]=TNSUA[0];
			    ISNPH[3]=NEQNS;
	        } // if (SOLCO >= 1)
			  
	        WRTAIL(1,0,IER[0],null);
		    return;     
		
    } // private void JAPHYC

    private void ANGLE7(double BE[], int NA, boolean IN) {
        // REAL BE(*)

        // **** BE=JACIN,NA=NARCS,IN=INTER

        // **** TO COMPUTE THE ARRAY OF JACOBI INDECES CORRESPONDING TO THE
        // **** CORNER ANGLES ON THE BOUNDARY

        // **** LOCAL VARIABLES
        int K,B0,B1,B2;
        double X,Y,ANG,PI,EPSA,XI,APP;
        double U[] = new double[2];
        double V[] = new double[2];
        double DIN[] = new double[2];
        double absu;
        double absv;
        double cr[] = new double[1];
        double ci[] = new double[1];
        //COMPLEX U,V,DPARFN
        //EXTERNAL DPARFN

        PI=Math.PI;
        EPSA=Math.sqrt(EPS);
        for (K=1; K <= NA; K++) {
        	DIN[0] = 1.0;
        	DIN[1] = 0.0;
            U=DPARFN(K,DIN);
            absu = zabs(U[0],U[1]);
            U[0] = -U[0]/absu;
            U[1] = -U[1]/absu;
            DIN[0] = -1.0;
        	DIN[1] = 0.0;
            if (K == NA) {
                V=DPARFN(1,DIN);
            }
            else {
                V=DPARFN(K+1,DIN);
            }
            absv = zabs(V[0],V[1]);
            V[0] = V[0]/absv;
            V[1] = V[1]/absv;
            zdiv(U[0],U[1],V[0],V[1],cr,ci);
            V[0]= cr[0];
            V[1] = ci[0];
            X = V[0];
            Y = V[1];
            ANG=Math.atan2(Y,X);
            if (ANG < 0) {
                ANG=ANG+2.0*PI;
            }
            ANG=ANG/PI;
            if (!IN) {
                ANG=2.0-ANG;
            }
            ANG=-1.0+1.0/ANG;

            // ****   TRY TO DETECT SIMPLE RATIONAL INDECES AND FORCE BEST REAL
            // ****   APPROXIMATIONS

            if (Math.abs(ANG) < EPSA) {
                ANG=0.0;
            }
            else {
                XI=Math.abs(ANG);
                B0=(int)(XI);
                XI=XI-(double)(B0);
                if (Math.abs(XI) < EPSA) {
                    APP=(double)(B0);
                }
                else {
                    XI=1.0/XI;
                    B1=(int)(XI);
                    XI=XI-(double)(B1);
                    if (Math.abs(XI) < EPSA) {
                        APP=(double)(1+B0*B1)/(double)(B1);
                    }
                    else {
                        XI=1.0/XI;
                        B2=(int)(XI);
                        APP=(double)(B0*(1+B1*B2)+B2)/(double)(1+B1*B2);   
                    } // else
                } // else
                if (ANG < 0.0) {
                	APP = -APP;
                }
                if (Math.abs(ANG-APP) < EPSA) {
                	ANG=APP;
                }
            } // else

            if (K == NA) {
                BE[0]=ANG;
            }
            else {
                BE[K]=ANG;
            }
  
        } // for (K=1; K <= NA; K++)

    } // private void ANGLE7
    
    private void RSLT80(double SUPER, double MAXER, double AQTOL, boolean INTER, int NARCS, int ORDSG,
        int NQPTS, boolean INCST, int INDEG, int RFARC, double RFARG,double CENTR[], double BETA[], boolean LINEAR[],
        int TSTNG, int OULVL, int IBNDS[], int MNEQN) {
    	
    	//INTEGER IBNDS(*)
    	//REAL BETA(*)
    	//COMPLEX CENTR
    	//LOGICAL LINEAR(*)
    	//CHARACTER JBNM*4,HEAD*72
    	
    	//**** WRITE THE MAIN ARGUMENTS OF JAPHYC AND ASSOCIATED QUANTITIES ON  
    	//**** THE LISTING FILE.
    	
    	// LOCAL VARIABLES
    	
    	int I;
    	final String TXT1 = "REQUESTED ACCURACY UNREALISTIC";
    	
    	if (INTER) {
    	    Preferences.debug("INTERIOR DOMAIN WITH " + NARCS + " ARCS\n",Preferences.DEBUG_ALGORITHM);
    	}
    	else {
    	    Preferences.debug("EXTERIOR DOMAIN WITH " + NARCS + " ARCS\n",Preferences.DEBUG_ALGORITHM);
    	}
    	if (ORDSG > 1) {
    	    Preferences.debug("\n",Preferences.DEBUG_ALGORITHM);
    	    Preferences.debug("ORDER OF SYMMETRY GROUP IS : " + ORDSG + "\n",Preferences.DEBUG_ALGORITHM);
    	    Preferences.debug("NUMBER OF ARCS ON FBS IS   : " + (NARCS/ORDSG)  + "\n",Preferences.DEBUG_ALGORITHM);
    	}
    	
    	Preferences.debug("ACCURACY REQUESTED            : " + MAXER  + "\n",Preferences.DEBUG_ALGORITHM);
    	if (MAXER < SUPER) {
    	    Preferences.debug("WORKING ACCURACY              : " + SUPER + "  " + TXT1 + "\n",Preferences.DEBUG_ALGORITHM);
    	}
    	Preferences.debug("ABSOLUTE QUADRATURE TOLERENCE : " + AQTOL + "\n",Preferences.DEBUG_ALGORITHM);
    	
    	Preferences.debug("\n",Preferences.DEBUG_ALGORITHM);
    	Preferences.debug("MAXIMUM NUMBER OF SUBARCS           : " + IBNDS[0] + "\n",Preferences.DEBUG_ALGORITHM);
    	Preferences.debug("MAXIMUM NUMBER OF EQUATIONS         : " + MNEQN + "\n",Preferences.DEBUG_ALGORITHM);
    	Preferences.debug("MAXIMUM NUMBER OF QUADRATURE PANELS : " + (IBNDS[2]-1) + "\n",Preferences.DEBUG_ALGORITHM);
    	Preferences.debug("MAXIMUM TOTAL  OF QUADRATURE POINTS : " + IBNDS[3] + "\n",Preferences.DEBUG_ALGORITHM);
    	
    	Preferences.debug("\n",Preferences.DEBUG_ALGORITHM);
    	Preferences.debug("MINIMUM NUMBER OF QUADRATURE POINTS : " + NQPTS + "\n",Preferences.DEBUG_ALGORITHM);
    	Preferences.debug("MAXIMUM DEGREE OF POLYNOMIAL        : " + (NQPTS-1) + "\n",Preferences.DEBUG_ALGORITHM);
    	Preferences.debug("INITIAL DEGREE OF POLYNOMIAL        : " + INDEG + (NQPTS-1) + "\n",Preferences.DEBUG_ALGORITHM);
    	Preferences.debug("INCREMENTAL STRATEGY                : " + INCST + "\n",Preferences.DEBUG_ALGORITHM);
    	
    	Preferences.debug("\n",Preferences.DEBUG_ALGORITHM);
    	Preferences.debug("REFERENCE ARC         : " + RFARC + "\n",Preferences.DEBUG_ALGORITHM);
    	Preferences.debug("REFERENCE ARGUMENT/PI : " + RFARG + "\n",Preferences.DEBUG_ALGORITHM);
    	Preferences.debug("CENTRE POINT          : " + CENTR[0] + ", " + CENTR[1] + "\n",Preferences.DEBUG_ALGORITHM);
    	
    	Preferences.debug("\n",Preferences.DEBUG_ALGORITHM);
    	Preferences.debug("CORNER        ANGLE/PI          JACOBI INDEX      LINEAR\n",Preferences.DEBUG_ALGORITHM);
    	for (I = 1; I <= NARCS; I++) {
    		Preferences.debug("  " + I + "     " + (1.0/(1.0 + BETA[I-1])) + "    " + BETA[I-1] + "      " + LINEAR[I-1] + "\n",Preferences.DEBUG_ALGORITHM);
    	}
    	
    	
    	Preferences.debug("\n",Preferences.DEBUG_ALGORITHM);
    	Preferences.debug("TESTING LEVEL : " + TSTNG + "\n",Preferences.DEBUG_ALGORITHM);
    	Preferences.debug("OUTPUT  LEVEL : " + OULVL + "\n",Preferences.DEBUG_ALGORITHM);
    } // private void RSLT80
    
    private void OPQUD1(int NJIND, int NQPTS,double JACIN[], double ACOEF[], double BCOEF[],
        double H0VAL[], double AICOF[], double BICOF[], double HIVAL[], double QUPTS[], 
        double QUWTS[], double WORK[], int IER[]) {
    	//REAL JACIN(*),ACOEF(*),BCOEF(*),H0VAL(*),QUPTS(*),QUWTS(*),
    	//+WORK(*),AICOF(*),BICOF(*),HIVAL(*)
    	
    	//**** TO SET UP THE THREE TERM RECURRENCE COEFFICIENTS (ACOEF AND BCOEF)
    	//**** FOR THE ON JACOBI POLYNOMIALS (UP TO DEGREE NQPTS) ASSOCIATED WITH
    	//**** THE JACOBI INDECES GIVEN IN JACIN AND TO STORE THE ZEROTH MOMENTS
    	//**** OF THE JACOBI DISTRIBUTIONS IN H0VAL.  ALSO TO REPEAT THESE
    	//**** CALCULATIONS FOR THE INCREMENTED JACOBI INDECES ARISING IN THE
    	//**** EXPRESSIONS FOR THE BOUNDARY CORRESPONDENCE FUNCTION, STORING
    	//**** RESULTS IN AICOF, BICOF AND HIVAL.
    	
    	//**** ALSO TO SET UP THE NQPT POINT GAUSS JACOBI QUADRATURE RULES,
    	//**** STORING THE ABSCISSAE IN QUPTS AND THE WEIGHTS IN QUWTS.
    	
    	//**** IER=0 - NORMAL EXIT
    	//**** IER=4 - FAILURE TO CONVERGE IN EIGSYS; CAN'T SET UP BASIC
    	//****         QUADRATURE RULES 
    	
    	//     LOCAL VARIABLES
    	
    	int I,J,K,LOSUB,M;
    	int IFAIL[] = new int[1];
    	double BETA,BETA1,C,PROD,S,T;
    	//EXTERNAL ASONJ7,EIGSYS
    	
    	for (I=1; I <= NJIND; I++) {
    	    BETA=JACIN[I-1];
    	    BETA1=BETA+1.0;
    	    PROD=BETA*BETA;
    	
    	    // CALCULATE THE ZEROTH MOMENT FOR THIS BETA
    	
    	    H0VAL[I-1]=Math.pow(2.0,BETA1)/BETA1;
    	
    	    // START ON THE 3-TERM ORTHONORMAL RECURRENCE COEFFICIENTS ACOEF
    	    // AND BCOEF FOR THIS BETA
    	
    	    T=2.0+BETA;
    	    S=T*T;
    	    C=4.0*BETA1/S/(T+1.0);
    	    LOSUB=(I-1)*NQPTS+1;
    	    ACOEF[LOSUB-1]=Math.sqrt(C);
    	    BCOEF[LOSUB-1]=BETA/T;
    	
    	    for (K=2; K <= NQPTS; K++) {
    	        J=LOSUB+K-1;
    	        BCOEF[J-1]=PROD/T/(T+2.0);
    	        T=2.0*K+BETA;
    	        S=T*T;
    	        C=4.0*K*K*(BETA+K)*(BETA+K)/S/(S-1.0);
    	        ACOEF[J-1]=Math.sqrt(C);
    	    } // for (K=2; K <= NQPTS; K++)
    	
    	    // START ON THE QUADRATURE POINTS AND WEIGHTS FOR THIS BETA
    	
    	    for (K=1; K <= NQPTS; K++) {
    	        J=LOSUB+K-1;
    	        QUPTS[J-1]=BCOEF[J-1];
    	        QUWTS[J-1]=ACOEF[J-1];
    	        WORK[K-1]=0.0;
    	    } // for (K=1; K <= NQPTS; K++)
    	    WORK[0]=1.0;
    	
    	    // AT THIS POINT THE LOCAL SEGMENTS OF QUPTS AND QUWTS ARE THE 
    	    // DIAGONAL AND SUBDIAGONAL OF A SYMMETRIC TRIDIAGONAL MATRIX 
    	    // WHOSE EIGENVALUES ARE THE QUADRATURE POINTS AND WHOSE 
    	    // EIGENVECTORS GIVE THE QUADRATURE WEIGHTS.
    	
    	    double D[] = new double[NQPTS];
    	    double E[] = new double[NQPTS];
    	    for (M = 0; M < NQPTS; M++) {
    	    	D[M] = QUPTS[LOSUB+M-1];
    	        E[M] = QUWTS[LOSUB+M-1];
    	    }
    	    EIGSYS(NQPTS,EPS,D,E,WORK,IFAIL);
    	    for (M = 0; M < NQPTS; M++) {
    	    	QUPTS[LOSUB+M-1] = D[M];
    	        QUWTS[LOSUB+M-1] = E[M];
    	    }
    	    if (IFAIL[0] > 0) {
    	        IER[0]=4;
    	        return;
    	    }
    	
    	    for (K=1; K <= NQPTS; K++) {
    	        QUWTS[LOSUB+K-2]=WORK[K-1]*WORK[K-1]*H0VAL[I-1];
    	    } // for (K=1; K <= NQPTS; K++)
    	
    	    // SET UP THE THREE TERM RECURRENCE COEFFICIENTS AIVAL,BIVAL AND
    	    // THE ZEROTH MOMENT HIVAL FOR THE INTEGRATED POLYNOMIALS.
    	
    	    double A[] = new double[NQPTS];
    	    double B[] = new double[NQPTS];
    	    double H[] = new double[1];
    	    ASONJ7(1.0,BETA1,A,B,H,NQPTS);
    	    for (M = 0; M < NQPTS; M++) {
    	    	AICOF[LOSUB+M-1] = A[M];
    	    	BICOF[LOSUB+M-1] = B[M];
    	    }
    	    HIVAL[I-1] = H[0];
    	
    	} // for (I=1; I <= NJIND; I++)
    	
    	//     NORMAL TERMINATION
    	
    	IER[0]=0;
    	
    } // private void OPQUD1
    
    private void EIGSYS(int N, double EPS, double D[], double E[],
    		double Z[],int IER[]) { 
        // REAL D(N),E(N),Z(N)      
      
        // THIS IS A MODIFIED VERSION OF THE EISPACK ROUTINE IMTQL2.     
        // IT FINDS THE EIGENVALUES AND FIRST COMPONENTS OF THE
        // EIGENVECTORS OF A SYMMETRIC TRIDIAGONAL MATRIX BY THE IMPLICIT QL
        // METHOD.     

        // **** LOCAL VARIABLES

        int L,J,M ,MML,II,K,I;
        double P,G,R,S,C,F,B;  
  
        IER[0] = 0;    

        if (N == 1) {
        	return;
        }

        E[N-1] = 0.0;  
        loopL: for (L = 1; L <= N; L++) {       
            J = 0;    

            //****    LOOK FOR SMALL SUB-DIAGONAL ELEMENT     

            while (true) {
		        for (M = L; M <= N; M++) {    
		            if (M == N) break;   
		            if (Math.abs(E[M-1]) <= EPS * (Math.abs(D[M-1]) + Math.abs(D[M])))  break;
		        } // for (M = L; M <= N; M++) 
		
		       P = D[L-1]; 
		       if (M == L) continue loopL;      
		       if (J == 30) {
		    	   // **** SET ERROR -- NO CONVERGENCE TO AN EIGENVALUE AFTER 30 ITERATIONS
		    	   IER[0] = L;
		    	   return;
		       }
		       J = J + 1;
		
		       // ****    FORM SHIFT
		
		       G = (D[L] - P) / (2. * E[L-1]);
		       R = Math.sqrt(G*G+1.0);
		       double sig;
		       if (G >= 0) {
		    	   sig = Math.abs(R);
		       }
		       else {
		    	   sig = -Math.abs(R);
		       }
		       G = D[M-1] - P + E[L-1] / (G + sig); 
		       S = 1.0;  
		       C = 1.0 ;  
		       P = 0.0;  
		       MML = M - L;
		       
		       for (II = 1; II <= MML; II++) { 
		           I = M - II;      
		           F = S * E[I-1];    
		           B = C * E[I-1];    
		           if (Math.abs(F) >= Math.abs(G)) {   
		               C = G / F;       
		               R = Math.sqrt(C*C+1.0);
		               E[I] = F * R;  
		               S = 1.0 / R;     
		               C = C * S;       
		           } // if (Math.abs(F) >= Math.abs(G))
		           else {
		               S = F / G;       
		               R = Math.sqrt(S*S+1.0);
		               E[I] = G * R;  
		               C = 1.0 / R;      
		               S = S * C; 
		           } // else
		           G = D[I] - P;  
		           R = (D[I-1] - G) * S + 2. * C * B;     
		           P = S * R;       
		           D[I] = G + P;  
		           G = C * R - B;  
		 
		           //****       FORM FIRST COMPONENT OF VECTOR
		
		           F = Z[I];      
		           Z[I] = S * Z[I-1] + C * F; 
		           Z[I-1] = C * Z[I-1] - S * F;  
		       } // for (II = 1; II <= MML; II++)
		
		       D[L-1] = D[L-1] - P;    
		       E[L-1] = G; 
		       E[M-1] = 0.0;
            } // while (true)
        } // loopL: for (L = 1; L <= N; L++)   
       
        // **** ORDER EIGENVALUES AND EIGENVECTORS      

        for (II = 2; II <= N; II++) {      
            I = II - 1; 
            K = I;    
            P = D[I-1]; 
            for (J = II; J <= N; J++) {   
                if (D[J-1] >= P) continue;
                K = J; 
                P = D[J-1];
            } // for (J = II; J <= N; J++)
       
            if (K == I) continue;      
            D[K-1] = D[I-1];
            D[I-1] = P; 
            P = Z[I-1]; 
            Z[I-1] = Z[K-1];
            Z[K-1] = P; 
        } // for (II = 2; II <= N; II++) 
        return;

    } // private void EIGSYS

    private void ASONJ7(double ALFA,double BETA, double A[], double B[],
    		double H[], int N) {
        //REAL A(*),B(*),H,ALFA,BETA

        // ..TO ASSIGN THE COEFFICIENTS A(K) AND B(K) , K=1(1)N, IN THE
        // ..3-TERM RECURRENCE FORMULA FOR THE ORTHONORMAL JACOBI POLYNOMIALS
        // ..WHERE
        // ..
        // ..     A(K)P (X) = (X - B(K))P   (X) - A(K-1)P   (X) , K=1,2,..,N,
        // ..          K                 K-1             K-2
        // ..
        // ..         P  (X) = 0 , P (X) = 1/SQRT(H)
        // ..          -1           0 
        // .. 
        // ..AND H IS THE ZEROTH MOMENT OF THE JACOBI WEIGHT FUNCTION
        // ..(1-X)**ALFA*(1+X)**BETA ON [-1,1].

        // **** AUTHOR: DAVID HOUGH
        // **** LAST UPDATE: 15.09.89

        // **** ..LOCAL VARIABLES..
        double SUM,DIFF,PROD,TC,T,SC,S,C;
        int K;
        // EXTERNAL GAMMA
        // double GAMMA;

        SUM=ALFA+BETA;
        DIFF=BETA-ALFA;
        PROD=SUM*DIFF;

        // ..CALCULATE H.
        TC=SUM+1.0;
        SC=Math.pow(2.0,TC);
        S=GAMMA(ALFA+1.0);
        T=GAMMA(BETA+1.0);
        C=GAMMA(TC+1.0);
        H[0]=SC*S*T/C;

        // ..START ON A,B ARRAYS.
        if (N > 0) {
            T=2.0+SUM;
            S=T*T;
            C=4.0*(ALFA+1.0)*(BETA+1.0)/S/(T+1.0);
            A[0]=Math.sqrt(C);
            B[0]=DIFF/T;

            for (K=2; K<= N; K++) {
                B[K-1]=PROD/T/(T+2.0);
                T=2.0*K+SUM;
                S=T*T;
                C=4.0*K*(ALFA+K)*(BETA+K)*(SUM+K)/S/(S-1.0);
                A[K-1]=Math.sqrt(C);
            } // for (K=2; K<= N; K++) 
        } // if (N > 0)

    } // private void ASONJ7
    
    private double GAMMA( double U) {

        // TO COMPUTE THE GAMMA FUNCTION FOR REAL ARGUMENT U BY USING
        // THE CHEBYSHEV EXPANSION GIVEN IN TABLE 1.3 OF "MATHEMATICAL
        // FUNCTIONS AND THEIR APPROXIMATION" BY Y.L. LUKE ,ACADEMIC PRESS,
        // NEW YORK, 1975.
        // SINCE GAMMA HAS POLES AT U=0,-1,-2,-3,... DIVISION BY ZERO WILL
        // OCCUR FOR THESE ARGUMENT VALUES.

        // LOCAL VARIABLES

        int N;
        double UWORK,FACTOR;
        double B0 = 0.0;
        double X,B1,B2;
        double A[] = new double[]{3.65738772508338243850,
                              1.95754345666126826928,
                              0.33829711382616038916,0.4208951276557549199E-1,
                              0.42876504821290877E-2,0.36521216929461767E-3,
                              0.27400642226422E-4,0.181240233365124E-5,
                              0.10965775865997E-6,0.598718404552E-8,
                              0.30769080535E-9,0.143179303E-10,
                              0.65108773E-12,0.259585E-13,0.110789E-14,
                              0.3547E-16,0.169E-17,0.3E-19};  
        double result;

        UWORK=U;
        FACTOR=1.0;

	    while (true) {
	        if (UWORK > 4.0) {
	            UWORK=UWORK-1.0;
	            FACTOR=FACTOR*UWORK;
	            continue;
	        }
	        else if (UWORK < 3.0) {
	            FACTOR=FACTOR/UWORK;
	            UWORK=UWORK+1.0;
	            continue;
	        }
	        else {
	        	break;
	        }
	    } // while (true)
	
	    X=UWORK-3.0;      
	    X=4.0*X-2.0;
	    B2=0.0;
	    B1=0.0;
	    for (N=17; N >= 0; N--) {
	        B0=X*B1-B2+A[N];
	        if (N > 0) {
	            B2=B1;
	            B1=B0;
	        }
	    } // for (N=17; N >= 0; N--)
	
	    result=5E-1*(B0+A[0]-B2)*FACTOR;
	    return result;
    } // private double GAMMA
    
    private void ASQUC7(double AQCOF[], double BQCOF[], double CQCOF[],
    		double JACIN[], int NJIND, int NQPTS) {
        //REAL AQCOF(*),BQCOF(*),CQCOF(*),JACIN(*)

        // ..TO ASSIGN THE COEFFICIENTS A(J), B(J) AND C(J) ,J=1,MN IN THE
        // ..3-TERM RECURRENCE FORMULA 
        // ..
        // ..  Q   (Z) = (A(J)Z - B(J))Q (Z) - C(J)Q   (Z) , J=2,...,MN
        // ..   J+1                     J           J-1
        // ..
        // ..    Q (Z) = (A(1)Z - B(1))Q (Z) - C(1)
        // ..     2                     1 
        // ..
        // ..WHERE Q (Z):=<P ,LOG(Z-.)>, P  IS THE ORTHONORMAL JACOBI POLY
        // ..       J       J             J
        // ..OF DEGREE J AND THE INNER PRODUCT IS WITH RESPECT TO THE JACOBI
        // ..DISTRIBUTION OVER (-1,1).  HERE A(J,I) STORES "A(J)" FOR THE ITH
        // ..ARC ON THE BOUNDARY (WITH SIMILAR ROLE FOR ARRAYS B AND C) AND
        // ..THE JACOBI WEIGHT FUNCTION FOR THE ITH ARC IS 
        // ..(1-X)**AB(I,1)*(1+X)**AB(I,2), J=1,MN, I=1,NA.  

        // **** AUTHOR: DAVID HOUGH
        // **** LAST UPDATE: 15.09.89

        // **** LOCAL VARIABLES        
        int I,J,K,LOSUB;
        double BE,D,N,N1,N2,F;
        double H[] = new double[1];
        // EXTERNAL ASONJ7

      for (I=1; I <= NJIND; I++) {
          LOSUB=(I-1)*NQPTS+1;
          BE=JACIN[I-1];
          double A[] = new double[NQPTS];
          double B[] = new double[NQPTS];
          ASONJ7(1.0,BE+1.0,A,B,H,NQPTS);
          for (K = 0; K < NQPTS; K++) {
        	  AQCOF[LOSUB+K-1] = A[K];
        	  BQCOF[LOSUB+K-1] = B[K];
          }
          for (K=1; K <= NQPTS; K++) {
              J=LOSUB+K-1;
              N=(double)(K);
              D=(N+1.0)*(N+BE+2.0);
              N1=N*(N+BE+1.0);
              N2=(N-1.0)*(N+BE);
              F=Math.sqrt(N1/D);
              AQCOF[J-1]=F/AQCOF[J-1];
              BQCOF[J-1]=BQCOF[J-1]*AQCOF[J-1];
              if (K > 1) {
                  CQCOF[J-1]=AQCOF[J-1]*N2/AQCOF[J-2]/N1;
              }
              else {
                  CQCOF[J-1]=-AQCOF[J-1]*Math.sqrt(H[0]/N1);
              }
          } // for (K=1; K <= NQPTS; K++)
      } // for (I=1; I <= NJIND; I++)

    } // private void ASQUC7
    
    private void CSCAL3(double COLSC[], int NQPTS,int NJIND, double ACOEF[], double BCOEF[],
    		double H0VAL[], double QUPTS[], double QUWTS[], double JACIN[], double MU[],
    		double TT[], double QQ[]) {
        // REAL COLSC(*),ACOEF(*),BCOEF(*),H0VAL(*),QUPTS(*),QUWTS(*),
    	// +JACIN(*),MU(*),TT(*),QQ(*)
    	
    	// TO SET UP THE A PRIORI COLUMN SCALE FACTORS USING TRUNCATED
    	// CHEBYSHEV EXAPANSIONS FOR THE LOGARITHMIC KERNEL, GAUSS-JACOBI
    	// QUADRATURE AND GAUSS-JACOBI TEST POINTS.
    	
    	// LOCAL VARIABLES
    	
        int I,J,J1,JI,K,K1,KMAX,LO,LO1,M,N;
    	double ROOTH,P0SCL,X,MAXVL,SUM1,SUM2;
    	// EXTERNAL JAPAR7
    	
    	K1=0;
    	MU[0]=-Math.log(2.0);
    	for (I=2; I <= 2*NQPTS; I++) {
    	    MU[I-1]=-2.0/(double)(I-1);
    	}
    	
    	for (JI=1; JI <= NJIND; JI++) {
    	    //BETA=JACIN[JI-1];
    	    ROOTH=Math.sqrt(H0VAL[JI-1]);
    	    P0SCL=1.0/ROOTH;
    	    LO=(JI-1)*NQPTS;
    	    LO1=LO+1;
    	
    	    for (J=1; J <= NQPTS; J++) {
    	        X=QUPTS[LO+J-1];
    	        QQ[(J-1)*NQPTS]=P0SCL;
    	        double PP[] = new double[NQPTS-1];
    	        double AA[] = new double[NQPTS-1];
    	        double BB[] = new double[NQPTS-1];
    	        for (N = 0; N < NQPTS-1; N++) {
    	        	PP[N] = QQ[(J-1)*NQPTS+N];
    	        	AA[N] = ACOEF[LO1+N-1];
    	        	BB[N] = BCOEF[LO1+N-1];
    	        }
    	        JAPAR7(PP,X,AA,BB,NQPTS-1);
    	        for (N = 0; N < NQPTS-1; N++) {
    	        	QQ[(J-1)*NQPTS+N] = PP[N];
    	        }
    	        TT[J-1]=1.0;
    	        TT[J+NQPTS-1]=X;
    	        for (K=3; K <= 2*NQPTS; K++) {
    	            TT[J+(K-1)*NQPTS-1]=2.0*X*TT[J+(K-2)*NQPTS-1]-TT[J+(K-3)*NQPTS-1];
    	        }
    	    } // for (J=1; J <= NQPTS; J++)
    	
    	    for (K=1; K <= NQPTS; K++) {
    	        MAXVL=0.0;
    	        for (I=1; I <= NQPTS; I++) {
    	            SUM2=0.0;
    	            KMAX=2*NQPTS+1-K;
    	            for (M=K; M <= KMAX; M++) {
    	                SUM1=0.0;
    	                for (J=1; J <= NQPTS; J++) {
    	                    J1=LO+J;
    	                    SUM1=SUM1+QUWTS[J1-1]*QQ[K+(J-1)*NQPTS-1]*TT[J+(M-1)*NQPTS-1];
    	                } // for (J=1; J <= NQPTS; J++)
    	                SUM2=SUM2+MU[M-1]*TT[I+(M-1)*NQPTS-1]*SUM1;
    	            } // for (M=K; M <= KMAX; M++)
    	            MAXVL=Math.max(MAXVL,Math.abs(SUM2));
    	        } // for (I=1; I <= NQPTS; I++)
    	        K1=K1+1;
    	        COLSC[K1-1]=1.0/MAXVL;
    	    } // for (K=1; K <= NQPTS; K++)
    	
    	} // for (JI=1; JI <= NJIND; JI++)
    	
    }

    private void JAPAR7(double PP[] ,double X, double AA[], double BB[], int N) {
        // REAL X,PP(*),AA(*),BB(*)

        // ..................................................................

        // 1.  JAPAR7
        //     EVALUATES ORTHONORMAL JACOBI POLYNOMIALS.


       // 2.  PURPOSE
       //     TO COMPUTE PP(I), I=1,..,N+1, GIVEN PP(1) ON ENTRY, WHERE PP(I)
       //     STORES THE VALUE OF THE ORTHONORMAL JACOBI POLYNOMIAL OF DEGREE
       //     I-1 AT THE GIVEN PARAMETER VALUE X. 


       // 3.  CALLING SEQUENCE
       //     CALL JAPAR7(PP,X,AA,BB,N)

       // PARAMETERS (SEE DECLARATIONS ABOVE FOR TYPES)
       //  ON ENTRY:
       //  PP(1)  - THE VALUE OF THE POLYNOMIAL OF DEGREE ZERO.

       //  X      - THE REAL NUMBER AT WHICH THE POLYNOMIALS ARE TO BE
       //           EVALUATED.

       //  AA     - ARRAY OF COEFFICIENTS IN THE 3-TERM RECURRENCE
       //           AA(I)*PP(I+1)=(X-BB(I))*PP(I)-AA(I-1)*PP(I-1),
       //           I=1,..,N, WITH "PP(0)" = 0.

       //  BB     - ARRAY OF COEFFICIENTS  IN THE 3-TERM RECURRENCE ABOVE.

       //  N      - THE LARGEST DEGREE OF POLYNOMIAL REQUIRING EVALUATION.

       //  ON EXIT:
       //  PP     - PP(I) IS THE VALUE OF THE POLYNOMIAL OF DEGREE I-1 AT
       //           X, I=1,..,N+1.


       // 4.  NO SUBROUTINES OR FUNCTIONS NEEDED
        
       // ..................................................................
   
       int I;

       if (N > 0) {
           PP[1]=(X-BB[0])*PP[0]/AA[0];
           for (I=2; I <= N; I++) {
               PP[I]=((X-BB[I-1])*PP[I-1]-AA[I-2]*PP[I-2])/AA[I-1];
           }
       }

    } // private void JAPAR7

    private void IGNLVL(double RIGLL[], double COLSC[], double ACOEF[], double BCOEF[],
    		double H0VAL[], double JACIN[], int NJIND, int NQPTS, int IER[]) {
        // REAL ACOEF(*),BCOEF(*),H0VAL(*),JACIN(*),RIGLL(*),COLSC(*)
    	
    	// TO SET UP THE A PRIORI COLUMN SCALE FACTORS USING THE COEFFICIENT
    	// IGNORE LEVELS OBTAINED FROM BOUNDARY CORRESPONDENCE FUNCTION 
    	// REPRESENTATION FOR THE BOUNDARY MAP; SEE RB #50 P141 ET SEQ.
    	// COMBINE WITH A PRIORI COLUMN SCALE FACTORS COLSC IN THE CASE OF
    	// SOLVING SYMM'S EQUATION.
    	
    	// IER=0 - NORMAL EXIT
    	// IER=5 - LOCAL PARAMETER MNQPT MUST BE INCREASED TO AT LEAST THE 
    	//         VALUE OF NQPTS
    	// IER=6 - FAILURE OF IMTQLH EIGENVALUE ROUTINE
    	// IER=53- A CORNER ANGLE IS SO SMALL THAT IT MAY CAUSE OVERFLOW
    	
    	// LOCAL VARIABLES
    	
    	final int MNQPT = 20;
    	int JT,K,K1,LO,N;
    	double H1VAL[] = new double[1];
    	double B1,B2,BETA,EXPON,LGTWO,OFLOW,PI,RH,RH1,SUP,T;
    	boolean SYMM;
    	double A1COF[] = new double[MNQPT];
    	double B1COF[] = new double[MNQPT];
    	double DIAG[] = new double[MNQPT];
    	double PP[] = new double[MNQPT];
    	double SDIAG[] = new double[MNQPT];
    	// EXTERNAL ASONJ7,IMTQLH,JAPAR7
    	
    	if (NQPTS > MNQPT) {
    	    IER[0]=5;
    	    return;
    	}
    	
    	if (COLSC[0] <= 0.0) {
    	    SYMM= false;
    	}
    	else {
    	    SYMM= true;
    	}
    	
    	K1=0;
    	PI= Math.PI;
    	LGTWO=Math.log(2.0);
    	OFLOW=Math.log(Double.MAX_VALUE);
    	for (JT=1; JT <= NJIND; JT++) {
    	    BETA=JACIN[JT-1];
    	    B1=BETA+1.0;
    	    B2=BETA+2.0;
    	    RH=Math.sqrt(H0VAL[JT-1]);
    	    ASONJ7(1.0,B1,A1COF,B1COF,H1VAL,NQPTS);
    	    LO=(JT-1)*NQPTS;
    	    K1=K1+1;
    	
    	    // ****   COMPUTE THE QUANTITY
    	    // ****       PI*2E+0**B2/B1/RH
    	    // ****   BUT CHECK FOR POSSIBLE OVERFLOW
    	
    	    EXPON=B2*LGTWO+Math.log(PI/B1/RH);
    	    if (EXPON >= OFLOW) {
    	        IER[0]=53;
    	        return;
    	    }
    	    else {
    	        if (SYMM) {
    	            RIGLL[K1-1]=Math.exp(EXPON)*COLSC[K1-1];
    	        }
    	        else {
    	            RIGLL[K1-1]=Math.exp(EXPON);
    	        }
    	    }
    	
    	    if (NQPTS == 1) {
    	        continue;
    	    }
    	
    	    RH1=Math.sqrt(H1VAL[0]);
    	    K1=K1+1;
    	
    	    // ****   COMPUTE THE QUANTITY
    	    // ****       PI*2E+0**(B2+1E+0)*B1**B1/RH1/B2**(B2+5E-1)
    	    // ****   BUT CHECK FOR POSSIBLE OVERFLOW
    	
    	    EXPON=(B2+1.0)*LGTWO+B1*Math.log(B1)-(B2+0.5)*Math.log(B2)+Math.log(PI/RH1);
    	    if (EXPON >= OFLOW) {
    	        IER[0]=53;
    	        return;
    	    }
    	    else {
    	        if (SYMM) {
    	            RIGLL[K1-1]=Math.exp(EXPON)*COLSC[K1-1];
    	        }
    	        else {
    	            RIGLL[K1-1]=Math.exp(EXPON);
    	        }
    	    }
    	
    	    for (N=2; N <= NQPTS-1; N++) {
    	
    	        // FIND THE ZEROES OF THE JACOBI POLYNOMIAL OF DEGREE N FOR
    	        // WEIGHT (1+X)**BETA
    	
    	        for (K=1; K <= N; K++) {
    	            DIAG[K-1]=BCOEF[LO+K-1];
    	            if (K == 1) {
    	                SDIAG[K-1]=0.0;
    	            }
    	            else {
    	                SDIAG[K-1]=ACOEF[LO+K-2];
    	            }
    	        } // for (K=1; K <= N; K++)
    	
    	        IMTQLH(N,DIAG,SDIAG,IER);
    	        if (IER[0] > 0) {
    	            IER[0]=6;
    	            return;
    	        }
    	
    	        SUP=0.0;
    	        for (K=1; K <= N; K++) {
    	            T=DIAG[K-1];
    	            PP[0]=1.0/RH1;
    	            JAPAR7(PP,T,A1COF,B1COF,N);
    	            T=(1.0-T)*Math.pow((1.0+T),B1)*PP[N-1];
    	            SUP=Math.max(SUP,Math.abs(T));
    	        } // for (K=1; K <= N; K++)
    	
    	        K1=K1+1;
    	        if (SYMM) {
    	            RIGLL[K1-1]=2.0*PI*SUP*COLSC[K1-1]/Math.sqrt(N*(N+B1));
    	        }
    	        else {
    	            RIGLL[K1-1]=2.0*PI*SUP/Math.sqrt(N*(N+B1));
    	        }
    	    } // for (N=2; N <= NQPTS-1; N++)
    	} // for (JT=1; JT <= NJIND; JT++)
    	
    	// NORMAL EXIT
    	
    	IER[0]=0;
    	
    } // private void IGNLVL
    
    private void IMTQLH(int N, double D[], double E[], int IERR[]) {
    
        int I,J,L,M,II,MML;
        //REAL D(N),E(N)
        double B,C,F,G,P,R,S,TST1,TST2;
    
        // THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE IMTQL1,
        // NUM. MATH. 12, 377-383(1968) BY MARTIN AND WILKINSON,
        // AS MODIFIED IN NUM. MATH. 15, 450(1970) BY DUBRULLE.
        // HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 241-248(1971).
    
        // THIS SUBROUTINE FINDS THE EIGENVALUES OF A SYMMETRIC
        // TRIDIAGONAL MATRIX BY THE IMPLICIT QL METHOD.
    
        // ON INPUT
    
        //     N IS THE ORDER OF THE MATRIX.
    
        //     D CONTAINS THE DIAGONAL ELEMENTS OF THE INPUT MATRIX.
    
        //     E CONTAINS THE SUBDIAGONAL ELEMENTS OF THE INPUT MATRIX
        //       IN ITS LAST N-1 POSITIONS.  E(1) IS ARBITRARY.
    
        // ON OUTPUT
    
        //     D CONTAINS THE EIGENVALUES IN ASCENDING ORDER.  IF AN
        //       ERROR EXIT IS MADE, THE EIGENVALUES ARE CORRECT AND
        //       ORDERED FOR INDICES 1,2,...IERR-1, BUT MAY NOT BE
        //       THE SMALLEST EIGENVALUES.
    
        //    E HAS BEEN DESTROYED.
    
        //    IERR IS SET TO
        //           ZERO       FOR NORMAL RETURN,
        //           J          IF THE J-TH EIGENVALUE HAS NOT BEEN
        //                      DETERMINED AFTER 30 ITERATIONS.
    
        // CALLS PYTHAG FOR  SQRT(A*A + B*B) .
    
        // QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO BURTON S. GARBOW,
        // MATHEMATICS AND COMPUTER SCIENCE DIV, ARGONNE NATIONAL LABORATORY
    
        // THIS VERSION ORIGINALLY DATED AUGUST 1983; RENDERED INACCURATE 
        // AND TRANSLATED INTO SINGLE PRECISION BY DAVID HOUGH, ETH, ZUERICH
        // OCTOBER, 1989.
    
        // ------------------------------------------------------------------
    
        IERR[0] = 0;
        if (N == 1) {
            return;	
        }
    
        for (I = 1; I <= N-1; I++) {
            E[I-1] = E[I];
        }
        E[N-1] = 0.0;
    
        for (L = 1; L <= N; L++) {
            J = 0;
            // .......... LOOK FOR SMALL SUB-DIAGONAL ELEMENT ..........
            loopW: while (true) {
                for (M = L; M <= N; M++) {
                    if (M == N) break;
                    TST1 = Math.abs(D[M-1]) + Math.abs(D[M]);
                    TST2 = TST1 + Math.abs(E[M-1]);
                    if (TST2 == TST1) break;
                } // for (M = L; M <= N; M++)
    
             P = D[L-1];
             if (M != L) {
                 if (J == 30) {
                     // .......... SET ERROR -- NO CONVERGENCE TO AN
                	 //                EIGENVALUE AFTER 30 ITERATIONS ..........
                	 IERR[0] = L;
                	 return;
                 } // if (J == 30)
                 J = J + 1;
                 // .......... FORM SHIFT ..........
                 G = (D[L] - P) / (2.0 * E[L-1]);
                 R = PYTHAG(G,1.0);
                 double sigR;
                 if (G >= 0) {
                	 sigR = Math.abs(R);
                 }
                 else {
                	 sigR = -Math.abs(R);
                 }
                 G = D[M-1] - P + E[L-1] / (G + sigR);
                 S = 1.0;
                 C = 1.0;
                 P = 0.0;
                 MML = M - L;
                 sect: {
                     // .......... FOR I=M-1 STEP -1 UNTIL L DO -- ..........
                     for (II = 1; II <= MML; II++) {
                         I = M - II;
                         F = S * E[I-1];
                         B = C * E[I-1];
                         R = PYTHAG(F,G);
                         E[I] = R;
                         if (R == 0.0) break sect;
                         S = F / R;
                         C = G / R;
                         G = D[I] - P;
                         R = (D[I-1] - G) * S + 2.0 * C * B;
                         P = S * R;
                         D[I] = G + P;
                         G = C * R - B;
                     } // for (II = 1; II <= MML; II++)
    
                     D[L-1] = D[L-1] - P;
                     E[L-1] = G;
                     E[M-1] = 0.0;
                     continue loopW;
                 } // sect
                 //.......... RECOVER FROM UNDERFLOW ..........
                 D[I] = D[I] - P;
                 E[M-1] = 0.0;
                 continue loopW;
             } // if (M != L)
             else { // if (M == L)
             // .......... ORDER EIGENVALUES ..........
             secb: {
             if (L != 1) {
                 // .......... FOR I=L STEP -1 UNTIL 2 DO -- ..........
                 for (II = 2; II <= L; II++) {
                     I = L + 2 - II;
                     if (P >= D[I-2]) break secb;
                     D[I-1] = D[I-2];
                 } // for (II = 2; II <= L; II++)
             } // if (L != 1)
    
             I = 1;
             } // secb:
             D[I-1] = P;
             break loopW;
             } // else if (M == L)
            } // loopW: while (true)
        } // for (L = 1; L <= N; L++)
        return;
   
    } // private void IMTQLH
    
    private double PYTHAG(double A, double B) {

        // FINDS SQRT(A**2+B**2) WITHOUT OVERFLOW OR DESTRUCTIVE UNDERFLOW

        double P,R,S,T,U;
        P = Math.max(Math.abs(A),Math.abs(B));
        if (P == 0.0E0) {
            return P;	
        }
        R = (Math.min(Math.abs(A),Math.abs(B))/P);
        R = R*R;
        while (true) {
           T = 4.0 + R;
           if (T == 4.0) {
        	   return P;
           }
           S = R/T;
           U = 1.0 + 2.0*S;
           P = U*P;
           double div = (S/U);
           R = div*div * R;
        } // while (true)
    } // private double PYTHAG

    private void CPJAC3(int NARCS,int NQPTS,int INDEG, int DGPOL[], double JACIN[],
    		double ACOEF[], double BCOEF[], double DIAG[],
            double SDIAG[],int TNSUA[], int LOSUB[], int HISUB[], int JATYP[], int PARNT[],
            double MIDPT[], double HALEN[], double COLPR[], double ZCOLL[][], boolean LNSEG[],
            int LOOLD[], int HIOLD[], double EPS,int IER[], boolean INIBT) {
        // INTEGER NARCS,NQPTS,INDEG,TNSUA,IER
        // INTEGER DGPOL(*),LOSUB(*),HISUB(*),JATYP(*),PARNT(*)
        // REAL EPS,JACIN(*),ACOEF(*),BCOEF(*),DIAG(*),SDIAG(*),
        // +MIDPT(*),HALEN(*),COLPR(*),LOOLD(*),HIOLD(*)
    	// JAPHYC call has INT LOOLD(*) and HIOLD(*).
        // COMPLEX ZCOLL(*)
        // LOGICAL LNSEG(*),INIBT

        // **** TO MAKE THE INITIAL ASSIGNMENT OF THE COLLOCATION PARAMETERS 
        // **** (STORED IN COLPR), THE COLLOCATION POINTS ON THE PHYSICAL 
        // **** BOUNDARY (STORED IN ZCOLL) AND THE ARRAYS LOSUB AND HISUB
        // **** NEEDED TO ACCESS THIS DATA CORRECTLY.  ALSO TO SET UP THE
        // **** ARRAYS
        // **** 	JATYP - THE JACOBI INDEX TYPE OF EACH SUBARC
        // ****	PARNT - THE ORIGINAL PARENT ARC OF EACH SUBARC
        // ****	MIDPT - THE GLOBAL PARAMETRIC MIDPOINT OF EACH SUBARC
        // ****	HALEN - THE GLOBAL PARAMETRIC HALF-LENGTH OF EACH SUBARC
        // ****   DGPOL - THE INITIAL POLYNOMIAL DEGREE ON EACH SUBARC
        // ****   LNSEG - THE INITIAL LINE SEGMENT TYPE OF EACH SUBARC.

        // **** IER=0 - NORMAL EXIT
        // **** IER=7 - FAILURE TO CONVERGE IN EIGENVALUE ROUTINE IMTQLH

        // LOCAL VARIABLES

        int IFAIL[] = new int[1];
    	int D,D1,FIRST,I,J,K,K1,K2,P,PREV;
        double S,TC,ALFA,BETA,MED,MDNBT;
        double PIN[] = new double[2];
        // COMPLEX PARFUN
        // EXTERNAL PARFUN,IMTQLH,MDNBT
 
        TNSUA[0]=2*NARCS;
        for (I=1; I <= NARCS; I++) {
            BETA=JACIN[I-1];
            if (I == NARCS) {
                ALFA=JACIN[0];
            }
            else {
                ALFA=JACIN[I];
            }
            if (INIBT) {
                MED=MDNBT(ALFA,BETA);
            }
            else {
                MED=0.0;
            }
            J=2*I-1;
            MIDPT[J-1]=0.5*(MED-1.0);
            HALEN[J-1]=0.5*(MED+1.0);
            PARNT[J-1]=I;
            JATYP[J-1]=I;
            J=J+1;
            MIDPT[J-1]=0.5*(MED+1.0);
            HALEN[J-1]=0.5*(1.0-MED);
            PARNT[J-1]=I;
            if (I == NARCS) {
                JATYP[J-1]=-1;
            }
            else {
                JATYP[J-1]=-I-1;
            }
        } // for (I=1; I <= NARCS; I++)

        for (I=NARCS; I >= 1; I--) {
            J=2*I;
            LNSEG[J-1]=LNSEG[I-1];
            LNSEG[J-2]=LNSEG[I-1];
        } // for (I=NARCS; I >= 1; I--)

        for (I=1; I <= TNSUA[0]; I++) {
            DGPOL[I-1]=INDEG;
        }

        LOSUB[0]=1;
        HISUB[0]=1+DGPOL[0];
        for (I=2; I <= TNSUA[0]; I++) {
            LOSUB[I-1]=HISUB[I-2]+1;
            HISUB[I-1]=LOSUB[I-1]+DGPOL[I-1];
        } // for (I=2; I <= TNSUA[0]; I++)

        for (I=1; I <= TNSUA[0]; I++) {
            LOOLD[I-1]=0;
            HIOLD[I-1]=-1;
        } // for (I=1; I <= TNSUA[0]; I++)

        for (I=1; I <= TNSUA[0]; I++) {
            J=JATYP[I-1];
            P=PARNT[I-1];
            D=DGPOL[I-1];
            D1=D+1;
            if (J > 0) {
                S=1.0;
            }
            else {
                S=-1.0;
                J=-J;
            }
            PREV=(J-1)*NQPTS;
            FIRST=LOSUB[I-1];
            for (K=1; K <= D1; K++) {
                K1=PREV+K;
                DIAG[K-1]=BCOEF[K1-1];
                if (K == 1) {
                    SDIAG[K-1]=0.0;
                }
                else {
                    SDIAG[K-1]=ACOEF[K1-2];
                }
            } // for (K=1; K <= D1; K++)
            IMTQLH(D1,DIAG,SDIAG,IFAIL);
            if (IFAIL[0] > 0) {
                IER[0]=7;
                return;
            }
            for (K=1; K <= D1; K++) {
                TC=S*DIAG[K-1];
                K2=FIRST+K-1;
                COLPR[K2-1]=TC;
                TC=MIDPT[I-1]+HALEN[I-1]*TC;
                PIN[0] = TC;
                PIN[1] = 0.0;
                ZCOLL[K2-1]=PARFUN(P,PIN);
            } // for (K=1; K <= D1; K++)
        } // for (I=1; I <= TNSUA[0]; I++)

        // NORMAL EXIT

        IER[0]=0;

    } // private void CPJAC3

    private double MDNBT(double ALFA, double BETA) {

        // MDNBT IS THE MEDIAN OF THE BETA DISTRIBUTION DEFINED BY THE
        // DENSITY (1-X)**ALFA * (1+X)**BETA ON (-1,1).

        // LOCAL VARIABLES..

        int N;
        double TOL,CC,CONST,SOLD,SNEW,SMNAB,GAMMA,FF,AA,BB,SS;
        // EXTERNAL SMNAB,GAMMA

        TOL=100.0*EPS;

        if (Math.abs(ALFA-BETA) <= TOL) {
            return 0.0;
        }

        if (ALFA > BETA) {
            CC=BETA+1.0;
        }
        else {
            CC=ALFA+1.0;
        }

        FF=1.0;
        AA=ALFA+1.0;
        BB=BETA+1.0;
        SS=ALFA+BETA+2.0;

    while (AA > 30.0) {
        FF=(AA-1.0)*FF/(SS-1.0);
        AA=AA-1.0;
        SS=SS-1.0;
    }

    while (BB > 30.0) {
        FF=(BB-1.0)*FF/(SS-1.0);
        BB=BB-1.0;
        SS=SS-1.0;
    }

    CONST=0.5*FF*GAMMA(AA)/GAMMA(SS);
    CONST=CONST*GAMMA(BB);

    N=0;
    SOLD=Math.pow((CC*CONST),(1.0/CC));

    while (true) {
        N=N+1;
        if (ALFA > BETA) {
            SNEW=Math.pow((CONST/SMNAB(N,ALFA,BETA,SOLD)),(1.0/CC));
        }
        else {
            SNEW=Math.pow((CONST/SMNAB(N,BETA,ALFA,SOLD)),(1.0/CC));
        }

        if (Math.abs(1.0-SOLD/SNEW) >= TOL) {
            SOLD=SNEW;
            continue;
        }
        else {
        	break;
        }
    } // while (true)

    double result;
    if (ALFA > BETA) {
        result=2.0*SNEW-1.0;
    }
    else {
        result=1.0-2.0*SNEW;
    }
    return result;
    } // private double MDNBT
    
    private double SMNAB(int N, double AL, double BE, double X) {

        // EVALUATES A SUM NEEDED TO DETERMINE THE MEDIAN OF THE BETA
        // DISTRIBUTION.

        // LOCAL VARIABLES..

        int I;
        double SUM,TERM;

        TERM=1.0/(1.0+BE);
        SUM=TERM;
        for (I=1; I <= N; I++) {
            TERM=-X*TERM*(AL-I+1)*(I+BE)/I/(I+BE+1);
            SUM=SUM+TERM;
        } // for (I=1; I <= N; I++)

        return SUM;

    } // private double SMNAB
    
    private void ICOQR1(int NARCS,int NJIND,int NQPTS,int MDGPO,int MQIN1, double AQTOL, double QUPTS[], double QUWTS[],
        double JACIN[], double MIDPT[], double HALEN[], double ACOEF[], double BCOEF[], double H0VAL[], double COLSC[],
        int NQUAD[], int LOQSB[], double QCOMX[], double QCOMW[], int MNQUA, double TOLOU[], double MCQER[], double XENPT[],
        double XIVAL[][], double XIDST[], int IER[]) {
        //INTEGER NARCS,NJIND,NQPTS,MDGPO,MQIN1,IER,NQUAD(*),LOQSB(*),
    	//+MNQUA
    	//REAL AQTOL,QUPTS(*),QUWTS(*),JACIN(*),MIDPT(*),HALEN(*),ACOEF(*),
    	//+BCOEF(*),H0VAL(*),COLSC(*),QCOMX(*),QCOMW(*),TOLOU(*),XENPT(*),
    	//+XIDST(*),MCQER
    	//COMPLEX XIVAL(*)
    	
    	// THE MAIN PURPOSE OF THIS ROUTINE IS TO SET UP THE ABSCISSAE 
    	// (QCOMX) AND WEIGHTS (QCOMW) FOR THE COMPOSITE GAUSSIAN RULES 
    	//  FOR THE ESTIMATION OF
    	
    	//      INTEGRAL  [(1+X)**BETA*P(X,I)*LOG|ZZ-X|*dX], I=0,1,...,MDGPO.
    	//     -1<=X<=1                                      J=1,NZZ
    	
    	//     HERE P(.,I) IS THE ORTHONORMAL JACOBI POLYNOMIAL OF DEGREE I
    	//     ASSOCIATED WITH THE WEIGHT (1+X)**BETA AND ZZ IS ANY COLLOCATION
    	//     POINT PREIMAGE NOT ON [-1,1].  BETA TAKES ON THE VARIOUS VALUES
    	//     DEFINED BY ARRAY JACIN.  THE ROUTINE ALSO COMPUTES
    	
    	//     NQUAD - NQUAD(I) IS THE NUMBER OF QUADRATURE POINTS IN THE
    	//             COMPOSITE RULE FOR BETA=JACIN(I).
    	//     LOQSB - THE ABSCISSAE AND WEIGHTS OF THE COMPOSITE RULE FOR
    	//             BETA=JACIN(I) ARE STORED IN ARRAYS QCOMX AND QCOMW IN
    	//             THE POSITIONS LOQSB(I) TO LOQSB(I)+NQUAD(I)-1 INCLUSIVE.
    	//     XIDST,
    	//     XIVAL - XIVAL(2*I-1) STORES THE COLLOCATION PREIMAGE THOUGHT
    	//             TO BE NEAREST TO -1 AND XIDST(2*I-1) STORES ITS DISTANCE
    	//             FROM -1; SIMILARLY, XIVAL(2*I) STORES THE PREIMAGE
    	//             THOUGHT TO BE NEAREST TO +1 AND XIDST(2*I) ITS DISTANCE
    	//             FROM +1. THE PREIMAGES ARE WITH RESPECT TO
    	//             THE PARAMETRIC FUNCTIONS DEFINING THE SUBARCS WHICH
    	//             MEET AT THE PHYSICAL CORNER WHERE BETA=JACIN(I).
    	//     TOLOU - TOLOU(I) IS THE ESTIMATED MAXIMUM ERROR OVER ALL
    	//             COLLOCATION POINTS IN USING THE COMPOSITE RULE
    	//             FOR BETA=JACIN(I).
    	//     MCQRE - THE INFINITY NORM OF TOLOU.
    	//     IER   - IER=0 FOR NORMAL TERMINATION.
    	//             IER=9 THE REQUIRED TOTAL NUMBER OF COMPOSITE QUADRATURE
    	//                   POINTS EXCEEDS THE LIMIT MNQUA.
    	
    	
    	//     LOCAL VARIABLES
    	
    	int QINTS[] = new int[1];
    	int I,J,K,J0,J1,J2,J3,JI,JI0,JI1,HI,LO;
    	double DST[] = new double[4];
    	double BETA,H1,H2,T0,T1,T2,T3,SUM1,RR,RRB,MEAN,RXI,IXI;
    	final double ONE[] = new double[]{1.0,0.0};
    	double Z0[] = new double[2];
    	double Z1[] = new double[2];
    	double Z2[] = new double[2];
    	double Z3[] = new double[2];
    	double XI[][] = new double[4][2];
    	double GT[] = new double[2];
    	//COMPLEX ONE,Z0,Z1,Z2,Z3,XI(4),PARFUN,DPARFN,GT
    	//PARAMETER (ONE=(1E+0,0E+0))
    	//EXTERNAL DPARFN,PARFUN,SUBIN7
    	double PIN[] = new double[2];
    	double POUT[];
    	double DOUT[];
    	double cr[] = new double[1];
    	double ci[] = new double[1];
    	
    	HI=0;
    	for (JI=1; JI <= NARCS; JI++) {
    	
    	    // AT THE JI'TH CORNER, THE ANALYTIC ARC IN THE BACKWARDS DIRECTION
    	    // IS THE JI0'TH, IN THE FORWARDS DIRECTION IT IS THE JI'TH AND THE
    	    // ONE BEYOND THAT IS THE JI1'TH.  THE FOUR SUBARCS ON THE JI0'TH
    	    // AND JI'TH ANALYTIC ARCS ARE THE J0'TH, J1'TH, J2'TH AND J3'TH,
    	    // STARTING AT THE BEGINING OF ARC JI0 AND ENDING AT THE END OF ARC
    	    // JI.
    	
    	    BETA=JACIN[JI-1];
    	    LO=(JI-1)*NQPTS+1;
    	    J2=2*JI-1;
    	    J3=J2+1;
    	    if (JI == 1) {
    	        JI0=NARCS;
    	        J1=2*NARCS;
    	    }
    	    else {
    	        JI0=JI-1;
    	        J1=J2-1;
    	    }
    	    J0=J1-1;
    	    if (JI == NARCS) {
    	        JI1=1;
    	    }
    	    else {
    	        JI1=JI+1;
    	    }
    	
    	    // NEXT WE FIX THE LOCAL PARAMETER VALUES OF THE COLLOCATION
    	    // POINTS NEAREST TO SUBARCS J1 AND J2.  FOR SUBARC J1 THE NEAR 
    	    // POINTS HAVE LOCAL PARAMETER VALUES T0 ON J0 AND T2 ON J2.  FOR
    	    // SUBARC J2 THE NEAR POINTS HAVE LOCAL PARAMETER VALUES T1 ON J1
    	    // AND T3 ON J3.
    	
    	    T0=QUPTS[JI0*NQPTS-1];
    	    T2=QUPTS[LO-1];
    	    T1=-T2;
    	    T3=-QUPTS[JI1*NQPTS-1];
    	
    	    // NOW CONVERT THESE LOCAL PARAMETER VALUES FOR THE SUBARCS TO
    	    // GLOBAL PARMETER VALUES FOR THE MAIN ARCS JI0 AND JI.
    	
    	    T0=MIDPT[J0-1]+HALEN[J0-1]*T0;
    	    H1=HALEN[J1-1];
    	    T1=MIDPT[J1-1]+H1*T1;
    	    H2=HALEN[J2-1];
    	    T2=MIDPT[J2-1]+H2*T2;
    	    T3=MIDPT[J3-1]+HALEN[J3-1]*T3;
    	
    	    // NOW COMPUTE THE POSITIONS OF THE FOUR NEAR POINTS ON THE
    	    // PHYSICAL BOUNDARY.
    	    PIN[0] = T0;
    	    PIN[1] = 0;
    	    Z0=PARFUN(JI0,PIN);
    	    PIN[0] = T1;
    	    PIN[1] = 0;
    	    Z1=PARFUN(JI0,PIN);
    	    PIN[0] = T2;
    	    PIN[1] = 0;
    	    Z2=PARFUN(JI,PIN);
    	    PIN[0] = T3;
    	    PIN[1] = 0;
    	    Z3=PARFUN(JI,PIN);
    	
            // FIND THE APPROXIMATE PARAMETRIC PREIMAGE OF Z0 WRT SUBARC J1.
    	    GT[0] = MIDPT[J1-1]-H1;
    	    GT[1] = 0.0;
    	    POUT = PARFUN(JI0,GT);
    	    DOUT = DPARFN(JI0,GT);
    	    zdiv(Z0[0] - POUT[0],Z0[1] - POUT[1],DOUT[0],DOUT[1],cr,ci);
    	    XI[0][0] = -1.0 + cr[0]/H1;
    	    XI[0][1] = ci[0]/H1;
    	
    	    // CONVERT TO  PARAMETRIC PREIMAGE WRT SUBARC J2.
    	
    	    XI[0][0]=-XI[0][0]; 
    	    XI[0][1]=-XI[0][1];
    	
    	    // FIND THE APPROXIMATE PARAMETRIC PREIMAGE OF Z1 WRT SUBARC J2.
    	
    	    PIN[0] = -1.0;
    	    PIN[1] = 0.0;
    	    POUT = PARFUN(JI,PIN);
    	    DOUT = DPARFN(JI,PIN);
    	    zdiv(Z1[0] - POUT[0],Z1[1] - POUT[1], DOUT[0], DOUT[1], cr, ci);
    	    XI[1][0] = -1.0 + cr[0]/H2;
    	    XI[1][1] = ci[0]/H2;
    	
    	    // FIND THE APPROXIMATE PARAMETRIC PREIMAGE OF Z2 WRT SUBARC J1.
    	    POUT = PARFUN(JI0,ONE);
    	    DOUT = DPARFN(JI0,ONE);
    	    zdiv(Z2[0] - POUT[0],Z2[1] - POUT[1],DOUT[0],DOUT[1],cr,ci);
    	    XI[2][0] = 1.0 + cr[0]/H1;
    	    XI[2][1]= ci[0]/H1; 
    	
    	    // CONVERT TO  PARAMETRIC PREIMAGE WRT SUBARC J2.
    	
    	    XI[2][0]=-XI[2][0]; 
    	    XI[2][1]=-XI[2][1]; 
    	
    	    // FIND THE APPROXIMATE PARAMETRIC PREIMAGE OF Z3 WRT SUBARC J2.
    	
    	        GT[0]=MIDPT[J2-1]+H2;
    	        GT[1] = 0.0;
    	        POUT = PARFUN(JI,GT);
    	        DOUT = DPARFN(JI,GT);
    	        zdiv(Z3[0]-POUT[0],Z3[1]-POUT[1],DOUT[0],DOUT[1],cr,ci);
    	        XI[3][0] = 1.0 + cr[0]/H2;
    	        XI[3][1] = ci[0]/H2;
    	
    	        // SELECT THE PREIMAGE NEAREST -1 AND THE ONE NEAREST +1.
    	
    	        for (J=1; J <= 4; J++) {
    	            RXI=XI[J-1][0];
    	            IXI=XI[J-1][1];
    	            if (-1.0 <= RXI && RXI <= 1.0) {
    	                DST[J-1]=Math.abs(IXI);
    	            }
    	            else if (RXI < -1.0) {
    	                DST[J-1]=zabs(XI[J-1][0]+1.0,XI[J-1][1]);
    	            }
    	            else {
    	                DST[J-1]=zabs(XI[J-1][0]-1.0,XI[J-1][1]);
    	            }
    	        } // for (J=1; J <= 4; J++)
    	
    	        if (DST[1] < DST[2]) {
    	            XIVAL[J2-1][0]=XI[1][0];
    	            XIVAL[J2-1][1]=XI[1][1];
    	            XIDST[J2-1]=DST[1];
    	        }
    	        else {
    	            XIVAL[J2-1][0]=XI[2][0];
    	            XIVAL[J2-1][1]=XI[2][1];
    	            XIDST[J2-1]=DST[2];
    	        }
    	
    	        if (DST[0] < DST[3]) {
    	            XIVAL[J3-1][0]=XI[0][0];
    	            XIVAL[J3-1][1]=XI[0][1];
    	            XIDST[J3-1]=DST[0];
    	        }
    	        else {
    	            XIVAL[J3-1][0]=XI[3][0];
    	            XIVAL[J3-1][1]=XI[3][1];
    	            XIDST[J3-1]=DST[3];
    	        }
    	
    	        // NOW DETERMINE THE NUMBER AND LOCATION OF THE  QUADRATURE 
    	        // SUBINTERVALS NEEDED TO MEET THE TOLERANCE AT XIVAL(J2) AND 
                // XIVAL(J3).
    	
    	        double ZZ[][] = new double[2][2];
    	        for (I = 0; I < 2; I++) {
    	        	ZZ[I][0] = XIVAL[J2-1+I][0];
    	        	ZZ[I][1] = XIVAL[J2-1+I][1];
    	        }
    	        double AJAC[] = new double[MDGPO];
    	        double BJAC[] = new double[MDGPO];
    	        for (I = 0; I < MDGPO; I++) {
    	        	AJAC[I] = ACOEF[LO-1+I];
    	        	BJAC[I] = BCOEF[LO-1+I];
    	        }
    	        double CSCAL[] = new double[MDGPO+1];
    	        for (I = 0; I < MDGPO+1; I++) {
    	        	CSCAL[I] = COLSC[LO-1+I];
    	        }
    	        double TOLU[] =new double[1];
    	        TOLU[0] = TOLOU[JI-1];
    	        SUBIN7(ZZ,2,BETA,MDGPO,NQPTS,AJAC,BJAC,
    	            H0VAL[JI-1],CSCAL,AQTOL,TOLU,XENPT,QINTS,MQIN1,IER);
    	        TOLOU[JI-1] = TOLU[0];
    	        if (IER[0] > 0) {
    	            return;
    	        }
    	
                // SET UP THE COMPOSITE RULE ABSCISSAE AND WEIGHTS FOR THIS
    	        // JACOBI INDEX.
    	
    	        NQUAD[JI-1]=QINTS[0]*NQPTS;
    	        LOQSB[JI-1]=HI+1;
    	        if (HI+NQUAD[JI-1] > MNQUA) {
    	            IER[0]=9;
    	            return;
    	        }
    	        SUM1=BETA+1.0;
    	        K=HI;
    	        for (I=1; I <= QINTS[0]; I++) {
    	            RR=(XENPT[I]-XENPT[I-1])*0.5;
    	            MEAN=(XENPT[I]+XENPT[I-1])*0.5; 
    	            if (I == 1) {
    	                RRB=Math.pow(RR,SUM1);
    	                LO=LO-1;
    	                for (J=1; J <= NQPTS; J++) {
    	                    K=K+1;
    	                    QCOMX[K-1]=MEAN+RR*QUPTS[LO+J-1];
    	                    QCOMW[K-1]=RRB*QUWTS[LO+J-1];
    	                } // for (J=1; J <= NQPTS; J++)
    	            } // if (I == 1)
    	            else {
    	                LO=NARCS*NQPTS;
    	                for (J=1; J <= NQPTS; J++) {
    	                    K=K+1;
    	                    QCOMX[K-1]=MEAN+RR*QUPTS[LO+J-1];
    	                    QCOMW[K-1]=RR*QUWTS[LO+J-1]*Math.pow((1.0+QCOMX[K-1]),BETA);
    	                } // for (J=1; J <= NQPTS; J++)
    	            }
    	        } // for (I=1; I <= QINTS[0]; I++)
    	        HI=HI+NQUAD[JI-1];
    	} // for (JI=1; JI <= NARCS; JI++)
    	
    	// ASSIGN INITIAL DATA FOR LEGENDRE ARCS 
    	
    	I=2*NJIND;
    	RR=Double.MAX_VALUE;
    	XIDST[I-1]=RR;
    	XIDST[I-2]=RR;
    	XIVAL[I-1][0]=RR;
    	XIVAL[I-1][1] = 0.0;
    	XIVAL[I-2][0]=RR;
    	XIVAL[I-2][1] = 0.0;
    	LOQSB[NJIND-1]=HI+1;
    	NQUAD[NJIND-1]=0;
    	
    	// FIND THE MAXIMUM OF THE TOLOU ELEMENTS
    	
    	MCQER[0]=0.0;
        for (I=1; I <= NARCS; I++) {
    	    MCQER[0]=Math.max(MCQER[0],TOLOU[I-1]);
        } 
    	
    	// NORMAL TERMINATION
    	
    	IER[0]=0;
    	
    } // private void ICOQR1


    private void SUBIN7(double ZZ[][], int NZZ, double BETA, int MAXDG, int NQUAD, double AJAC[], double BJAC[],
        double H0JAC, double CSCAL[], double TOLIN, double TOLOU[], double XENPT[], int QINTS[], int MQIN1,int IER[]) {
    	//INTEGER MAXDG,NQUAD,QINTS,NZZ,IER,MQIN1
    	//REAL BETA,AJAC(*),BJAC(*),H0JAC,CSCAL(*),TOLIN,TOLOU,XENPT(*)
    	//COMPLEX ZZ(*)
    	
    	//     CALCULATES THE NUMBER OF QUADRATURE INTERVALS (QINTS) REQUIRED
    	//     FOR THE COMPOSITE GAUSS-JACOBI/GAUSS-LEGENDRE ESTIMATION OF
    	
    	//       INTEGRAL  [(1+X)**BETA*P(X,I)*LOG|ZZ(J)-X|*dX], I=0,1,...,MAXDG
    	//      -1<=X<=1                                         J=1,NZZ
    	
    	//     WHERE P(.,I) IS THE ORTHONORMAL JACOBI POLYNOMIAL OF DEGREE I
    	//     ASSOCIATED WITH THE WEIGHT (1+X)**BETA AND ZZ(J),J=1,..,NZZ, ARE 
    	//     GIVEN POINTS CLOSE TO [-1,1].  
    	
    	//     THE ENDPOINTS OF THESE INTERVALS ARE RETURNED IN VECTOR XENPT, 
    	//     WITH XENPT(1)=-1<XENPT(2)<...<1=XENPT(QINTS+1).
    	
    	//     IF Q(I,J) DENOTES THE ABSOLUTE QUADRATURE ERROR FOR THE INTEGRAL
    	//     ASSOCIATED WITH P(.,I) AND ZZ(J) THEN WE REQUIRE THAT
    	
    	//                 MAX       Q(I,J)*CSCAL(I) < TOLIN,
    	//              I=0,MAXDG
    	//               J=1,NZZ
    	
    	//     WITH THE MAXIMUM ON THE LEFT BEING REASONABLY CLOSE TO TOLIN.
    	//     TOLOU RETURNS THE COMPUTED VALUE OF THE ABOVE MAXIMUM.
    	 
    	//     IER=0 - NORMAL EXIT
    	//     IER=10- PARAMETER NMAX LOCAL TO THIS ROUTINE NEEDS INCREASING TO
    	//             BE AT LEAST NZZ*(MAXDG+1)
    	//     IER=11- REQUESTED NUMBER OF QUADRATURE PANELS EXCEEDS THAT DEFINED
    	//             BY MQIN1   
    	
    	//     LOCAL VARIABLES
    	
    	final int NMAX = 100;
    	double TAU[] = new double[1];
    	double MAXRM[] = new double[1];
    	double TOL;
    	double RIGHT[] = new double[1];
    	boolean T1FXD;
    	
    	double REMND[][] = new double[NMAX][2];
    	//COMPLEX REMND(NMAX)
    	
    	// EXTERNAL DEJAC7,DELEG7
    	
    	if (NZZ*(MAXDG+1) > NMAX) {
    	    IER[0]=10;
    	    return;
    	}
    	
    	QINTS[0]=1;
    	XENPT[0]=-1.0;
    	TOL=TOLIN;
    	DEJAC7(ZZ,NZZ,BETA,TAU,MAXDG,NQUAD,AJAC,BJAC,H0JAC,REMND,CSCAL,TOL,MAXRM,IER);
    	if (IER[0] > 0) {
    	    return;
    	}
    	TOLOU[0]=MAXRM[0];
    	XENPT[1]=TAU[0];
    	
    	if (XENPT[1] < 1.0) {
    	    QINTS[0]=2;
    	    T1FXD=false;
    	    TAU[0]=1.0;
    	    RIGHT[0]=-1.0;
    	    DELEG7(ZZ,NZZ,BETA,RIGHT,TAU,T1FXD,MAXDG,NQUAD,AJAC,
                   BJAC,H0JAC,REMND,CSCAL,TOL,MAXRM,IER);
    	    if (IER[0] > 0) {
    	        return;
    	    }
    	    TOLOU[0]=TOLOU[0]+MAXRM[0];
    	    T1FXD=true;
    	
    	    while (true) {
    	
    	        if (XENPT[QINTS[0]-1] > RIGHT[0]) {
    	            XENPT[QINTS[0]-1]=0.5*(XENPT[QINTS[0]-1]+RIGHT[0]);
    	            XENPT[QINTS[0]]=1.0;
    	            break;
    	        }
    	        else {
    	          TAU[0]=1.0;
    	          double TAU1[] = new double[1];
    	          TAU1[0] = XENPT[QINTS[0]-1];
    	          DELEG7(ZZ,NZZ,BETA,TAU1,TAU,T1FXD,MAXDG,
    	                   NQUAD,AJAC,BJAC,H0JAC,REMND,CSCAL,TOL,MAXRM,IER);
    	          XENPT[QINTS[0]-1] = TAU1[0];
    	          if (IER[0] > 0) {
    	              return;
    	          }
    	          TOLOU[0]=TOLOU[0]+MAXRM[0];
    	          QINTS[0]=QINTS[0]+1;
    	          if (QINTS[0] >= MQIN1) {
    	              IER[0]=11;
    	              return;
    	          }
    	          XENPT[QINTS[0]-1]=TAU[0];
    	          continue;
    	        } // else
    	} // while (true)
    } // if (XENPT[1] < 1.0)
    	
    // NORMAL TERMINATION
    	
    IER[0]=0;
    	
    } // private void SUBIN7
    
    private void DEJAC7(double ZZ[][], int NZZ, double BETA, double TAU[], int MAXDG, int NQUAD,
        double ACOEF[], double BCOEF[], double H0VAL,
    	double REMND[][], double CSCAL[], double TOL, double MAXRM[],int IER[]) {
    	//INTEGER MAXDG,NQUAD,NZZ,IER
    	//REAL BETA,TAU,H0VAL,TOL,MAXRM
    	//REAL ACOEF(*),BCOEF(*),CSCAL(*)
    	//COMPLEX ZZ(*),REMND(*)
    	
    	//     WE COMPUTE THE DONALDSON-ELLIOTT ESTIMATES FOR THE REMAINDERS IN
    	//     USING AN NQUAD-POINT GAUSS-JACOBI RULE TO ESTIMATE THE INTEGRALS
    	
    	//       INTEGRAL  [(1+X)**BETA*P(X,I)*LOG(ZZ(J)-X)*dX], I=0,MAXDG
    	//      -1<=X<=TAU                                       J=1,NZZ
    	
    	//     WHERE P(.,I) IS THE ORTHONORMAL JACOBI POLYNOMIAL OF DEGREE I
    	//     ASSOCIATED WITH THE WEIGHT (1+X)**BETA.  THE REMAINDER 
    	//     CORRESPONDING TO P(.,I) AND ZZ(J) IS STORED IN 
    	//     REMND(I+J+MAXDG*(J-1)), I=0,MAXDG, J=1,NZZ.  THIS ROUTINE USES
    	//     THE SIMPLEST POSSIBLE ESTIMATES; I.E. THE LEADING TERM ONLY IN
    	//     THE ASYMPTOTIC EXPANSION AND THE WATSON-DOETSCH ESTIMATE FOR ANY
    	//     INTEGRALS.
    	
    	//     THE PURPOSE OF THIS ROUTINE IS THEN TO DETERMINE A VALUE FOR TAU
    	//     SUCH THAT
    	
    	//         ABS( REAL(REMND(I)) )*CSCAL(I) < TOL , I=1,NZZ*(MAXDG+1)
    	
    	//     AND THAT, IF POSSIBLE, 
    	
    	//        0.5*TOL <= ABS( REAL(REMND(I)) )*CSCAL(I) < TOL
    	
    	//     FOR AT LEAST ONE VALUE OF I.
    	
    	//     IER=0 - NORMAL EXIT
    	//     IER=12- LOCAL PARAMETER NC NEEDS INCREASING TO AT LEAST NZZ
    	//             (THIS ERROR CAN'T ARISE IN THE PRESENT VERSION, SINCE
    	//              NZZ IS FIXED AT 2)
    	//     IER=13- LOCAL PARAMETER NR NEEDS INCREASING TO AT LEAST MAXDG
    	//             (AT PRESENT MAXDG=NQPTS-1) 
    	//     IER=14- A JACOBI INDEX MAY BE LARGE ENOUGH TO CAUSE OVERFLOW IN
    	//             THE GAMMA FUNCTION (AN ANGLE ON THE PHYSICAL BOUNDARY
    	//             MUST BE LESS THAN ABOUT 6 DEGREES)
    	
    	//     LOCAL VARIABLES..
    	
    	final int NC = 8;
    	final int NR = 30;
    	int I,J,K,LIM;
    	double S,KK,SUM1,RI,TURI,RN,OFLOW,P0SCL,TUK,LOWER,UPPER,TERM,HTOL;
    	double XI[] = new double[2];
    	double Z1[] = new double[2];
    	double XI1[] = new double[2];
    	double FF[] = new double[2];
    	double PRE[] = new double[2];
    	double CUR[] = new double[2];
    	double NXT[] = new double[2];
    	//COMPLEX XI,Z1,XI1,FF,PRE,CUR,NXT
    	double GG[][] = new double[NC][2];
    	double CONST[][][] = new double[NR][NC][2];
    	//COMPLEX GG(NC),CONST(NR,NC)
    	//REAL GAMMA, LGGAM
    	//EXTERNAL GAMMA,LGGAM
    	double cr[] = new double[1];
    	double ci[] = new double[1];
    	double r;
    	double theta;
    	double mag;
    	double arg;
    	double absXI1;
    	double var[] = new double[2];
    	double cr2[] = new double[1];
    	double ci2[] = new double[1];
    	
    	if (NZZ > NC ) {
    	    IER[0]=12;
    	    return;
    	}
    	
    	if (MAXDG >= NR) {
    	    IER[0]=13;
    	    return;
    	}
    	
    	S=BETA+4.0;
    	if (S > 20.0) {
    	
    	    // TEST FOR POSSIBLE OVERFLOW IN GAMMA FUNCTION
    	
    	    OFLOW=Math.log(Double.MAX_VALUE);
    	    KK=LGGAM(S);
    	    if (KK > OFLOW) {
    	        IER[0]=14;
    	        return;
    	    }
    	    else {
    	        KK=Math.exp(-KK);
    	    }
    	} // if (S > 20.0)
    	else {
    	    KK=1.0/GAMMA(S);
    	}
    	
    	// FIRST WE COMPUTE THE FACTORS WHICH ARE INDEPENDENT OF TAU
    	
    	S=S-1.0;
    	KK=Math.pow(4.0,S)*KK*GAMMA(BETA+2.0)/(S-1.0);
    	SUM1=BETA+1.0;
    	for (I=2; I <= NQUAD; I++) {
    	    RI=(double)(I);
    	    TURI=2.0*RI;
    	    KK=KK*16.0*(RI+BETA)/(TURI+SUM1);
    	    KK=KK*RI/(TURI+BETA);
    	    KK=KK*(RI+BETA)/(TURI+BETA);
    	    KK=KK*RI/(TURI+BETA-1.0);
    	} // for (I=2; I <= NQUAD; I++)
    	RN=(double)(NQUAD);
    	TUK=2.0*RN+SUM1;
    	KK=-KK/TUK/2.0;
    	
    	for (I=1; I <= NZZ; I++) {
    		r = zabs(1.0 + ZZ[I-1][0], ZZ[I-1][1]);
    		theta = Math.atan2(ZZ[I-1][1],1.0+ZZ[I-1][0]);
    		mag = Math.pow(r, BETA);
    		arg = BETA * theta;
    		GG[I-1][0] = mag * Math.cos(arg)*KK;
    		GG[I-1][1] = mag * Math.sin(arg)*KK;
    	} // for (I=1; I <= NZZ; I++)
    	
    	// NOW GIVE THE JACOBI POLYNOMIALS THE SCALING CORRESPONDING TO
        // [-1,1] AS STANDARD INTERVAL
    	
    	P0SCL=1.0/Math.sqrt(H0VAL);
    	for (J=1; J <= NZZ; J++) {
    	    PRE[0]=P0SCL;
    	    PRE[1] = 0.0;
    	    CUR[0] = PRE[0];
    	    CUR[1] = PRE[1];
    	    zmlt(CUR[0], CUR[1], GG[J-1][0],GG[J-1][1], cr, ci);
    	    CONST[0][J-1][0] = cr[0] * CSCAL[0];
    	    CONST[0][J-1][1] = ci[0] * CSCAL[0];
    	    if (MAXDG >= 1) {
    	    	zmlt(ZZ[J-1][0] - BCOEF[0], ZZ[J-1][1], PRE[0]/ACOEF[0], PRE[1]/ACOEF[0], cr, ci);
    	    	CUR[0] = cr[0];
    	    	CUR[1] = ci[0];
    	        zmlt(CUR[0], CUR[1], GG[J-1][0]*CSCAL[1], GG[J-1][1]*CSCAL[1], cr, ci);
    	        CONST[1][J-1][0] = cr[0];
    	        CONST[1][J-1][1] = ci[0];
    	        for (I=2; I <= MAXDG; I++) {
    	        	zmlt(ZZ[J-1][0] - BCOEF[I-1], ZZ[J-1][1], CUR[0], CUR[1], cr, ci);
    	        	NXT[0] = (cr[0] - ACOEF[I-2]*PRE[0])/ACOEF[I-1];
    	        	NXT[1] = (ci[0] - ACOEF[I-2]*PRE[1])/ACOEF[I-1];
    	            PRE[0] = CUR[0];
    	            PRE[1] = CUR[1];
    	            CUR[0]=NXT[0];
    	            CUR[1]=NXT[1];
    	            zmlt(CUR[0],CUR[1],GG[J-1][0]*CSCAL[I],GG[J-1][1]*CSCAL[I], cr, ci);
    	            CONST[I][J-1][0] = cr[0];
    	            CONST[I][J-1][1] = ci[0];
    	        } // for (I=2; I <= MAXDG; I++)
    	    } // if (MAXDG >= 1)
    	} // for (J=1; J <= NZZ; J++)
    	
    	// NOW COME THE FACTORS DEPENDENT ON TAU
    	
    	TAU[0]=1.0;
    	LOWER=-1.0;
    	UPPER=1.0;
    	LIM=NZZ*(MAXDG+1);
    	
    	while (true) {
    	
    	    HTOL=0.5*TOL;
    	    K=0;
    	    for (J=1; J <= NZZ; J++) {
    	        XI[0]=(2.0*ZZ[J-1][0]+1.0-TAU[0])/(1.0+TAU[0]);
    	        XI[1]= (2.0*ZZ[J-1][1])/(1.0 + TAU[0]);
    	        zmlt(XI[0],XI[1],XI[0],XI[1],cr,ci);
    	        r = zabs(cr[0]-1.0,ci[0]);
    	        theta = Math.atan2(ci[0],cr[0]-1.0);
    	        mag = Math.sqrt(r);
    	        arg = 0.5*theta;
    	        Z1[0] = mag * Math.cos(arg);
    	        Z1[1] = mag * Math.sin(arg);
    	        XI1[0]=XI[0]+Z1[0];
    	        XI1[1]=XI[1]+Z1[1];
    	        absXI1 = zabs(XI1[0],XI1[1]);
    	        if (absXI1 < 1.0) {
    	            XI1[0]=XI[0]-Z1[0];
    	            XI1[1]=XI[1]-Z1[1];
    	        }
    	        r = zabs(XI1[0],XI1[1]);
    	        theta = Math.atan2(XI1[1], XI1[0]);
    	        mag = Math.pow(r, -TUK-1.0);
    	        arg = theta * (-TUK-1.0);
    	        var[0] = mag * Math.cos(arg);
    	        var[1] = mag * Math.sin(arg);
    	        zmlt(XI1[0],XI1[1],XI1[0],XI1[1],cr,ci);
    	        zmlt(var[0],var[1],cr[0]-1.0,ci[0],cr2,ci2);
    	        FF[0] = cr2[0]*(1.0+TAU[0])*0.5;
    	        FF[1] = ci2[0]*(1.0+TAU[0])*0.5;
    	        for (I=0; I <= MAXDG; I++) {
    	          K=K+1;
    	          zmlt(CONST[I][J-1][0],CONST[I][J-1][1],FF[0],FF[1],cr,ci);
    	          REMND[K-1][0]= cr[0];
    	          REMND[K-1][1] = ci[0];
    	        } // for (I=0; I <= MAXDG; I++)
    	    } // for (J=1; J <= NZZ; J++)
    	
    	    MAXRM[0]=0.0;
    	    for (I=1; I <= LIM; I++) {
    	        TERM=Math.abs(REMND[I-1][0]);
    	        MAXRM[0]=Math.max(MAXRM[0],TERM);
    	    } // for (I=1; I <= LIM; I++)
    	
    	    if (MAXRM[0] < TOL) {
    	
    	        // ACCURACY IS ACHIEVED, BUT MAYBE TAU COULD BE INCREASED.
    	
    	        if (MAXRM[0] < HTOL) {
    	
    	            // TAU NEEDS INCREASING, BUT THIS IS ONLY POSSIBLE IF TAU<1.
    	
    	            if (TAU[0] < 1.0) {
    	                LOWER=TAU[0];
    	                TAU[0]=0.5*(LOWER+UPPER);
    	                continue;
    	            } // if (TAU[0 < 1.0)
    	            else {
    	            	break;
    	            }
    	        } // if (MAXRM[0] < HTOL)
    	        else {
    	        	break;
    	        }
    	    }
    	    else { // MAXRM[0] >= TOL
    	
                // ACCURACY NOT ACHIEVED AND TAU NEEDS DECREASING.
    	
    	        if (TAU[0] == 1.0) {
    	            TOL=HTOL;
    	        }
    	        UPPER=TAU[0];
    	        TAU[0]=0.5*(LOWER+UPPER);
    	        continue;
    	    } // else MAXRM[0] >= TOL
    	} // while (true)
    	
    	// NORMAL TERMINATION
    	
    	IER[0]=0;
    	
    } // private void DEJAC7
    

    private double LGGAM(double X) {

        // **** TO ESTIMATE THE LOGARITHM OF THE GAMMA FUNCTION FOR  L A R G E
        // **** POSITIVE VALUES OF X USING THE ASYMPTOTIC FORMULA FROM ABRAMOWITZ
        // **** AND STEGUN, SECTION 6.1.41

        double PI,W,result;

        PI=Math.PI;
        W=1.0/X/X;
        result=
                ((((W/9.9E+1-1E+0/1.4E+2)*W+1E+0/1.05E+2)*W-1E+0/3E+1)*W+1E+0)/
                (1.2E+1*X) + 5E-1*Math.log(2E+0*PI) - X + (X-5E-1)*Math.log(X);
        return result;
    } // private double LGGAM

    private void DELEG7(double ZZ[][], int NZZ, double BETA, double TAU1[], double TAU2[], boolean T1FXD,
        int MAXDG, int NQUAD, double ACOEF[], double BCOEF[], double H0VAL, double REMND[][], 
        double CSCAL[], double TOL, double MAXRM[], int IER[]) {
        //INTEGER MAXDG,NQUAD,IER,NZZ
    	//REAL BETA,TAU1,TAU2,H0VAL,TOL,MAXRM
    	//REAL ACOEF(*),BCOEF(*),CSCAL(*)
    	//LOGICAL T1FXD
    	//COMPLEX ZZ(*),REMND(*)
    	
    	// WE COMPUTE THE DONALDSON-ELLIOTT ESTIMATES FOR THE REMAINDERS IN
    	// USING AN NQUAD-POINT GAUSS-LEGENDRE RULE TO ESTIMATE THE INTEGRALS
    
    	//   INTEGRAL  [(1+X)**BETA*P(X,I)*LOG(ZZ(J)-X)*dX], I=0,1,...,MAXDG
    	// TAU1<=X<=TAU2                                     J=1,2
    	//     FOR AT LEAST ONE VALUE OF I.
    	//     IER=0 - NORMAL EXIT
    	//     IER=12- LOCAL PARAMETER NC NEEDS INCREASING TO AT LEAST NZZ
    	//             (THIS ERROR CAN'T ARISE IN THE PRESENT VERSION, SINCE
    	//              NZZ IS FIXED AT 2)
    	//     IER=13- LOCAL PARAMETER NR NEEDS INCREASING TO AT LEAST MAXDG
    	//             (AT PRESENT MAXDG=NQPTS-1) 
    	//
    	//     LOCAL VARIABLES..
    	
    	final int NC = 8;
    	final int NR = 30;
    	int I,J,K;
    	double KK,RI,TURI,RN,P0SCL,TUK,LOWER,HTOL,UFLOW,EXPON,
    	     UPPER,TERM,HCO,PI,FORK,SUM,FAC1,FAC2,PVAL,RR,MEAN,BB,BB1,BB2,FFH;
    	double XI[] = new double[2];
    	double Z1[] = new double[2];
    	double XI1[] = new double[2];
    	double FFG[] = new double[2];
    	double PRE[] = new double[2];
    	double CUR[] = new double[2];
    	double NXT[] = new double[2];
    	// COMPLEX XI,Z1,XI1,FFG,PRE,CUR,NXT
    	boolean FIRST;
    	double GG[][] = new double[NC][2];
    	double HH[][] = new double[NC][2];
    	double CONGG[][][] = new double[NR][NC][2];
    	double CONHH[][][] = new double[NR][NC][2]; 
    	// COMPLEX GG(NC),HH(NC),CONGG(NR,NC),CONHH(NR,NC)
    	// EXTERNAL GAMMA,LGGAM
    	double r;
    	double mag;
    	double theta;
    	double ang;
    	double cr[] = new double[1];
    	double ci[] = new double[1];
    	double absXI1;
    	double var[] = new double[2];
    	double cr2[] = new double[1];
    	double ci2[] = new double[1];
    	
    	// FIRST WE COMPUTE THE FACTORS WHICH ARE INDEPENDENT OF TAU1,TAU2
    	
    	if (NZZ > NC ) {
    	    IER[0]=12;
    	    return;
    	}
    	
    	if (MAXDG >= NR) {
    	    IER[0]=13;
    	    return;
    	}
    	
    	// **** SET THE LOGARITHMIC UNDERFLOW LIMIT
    	// B**(EMIN-1)
    	// EMIN = -1022;
    	UFLOW = Math.log(Math.pow(2.0, -1023.0));    
    	
    	PI=Math.PI;
    	KK=32.0/6.0;
    	for (I=2; I <= NQUAD; I++) {
    	    RI=(double)(I);
    	    TURI=2.0*RI;
    	    KK=KK*4.0*RI/(TURI+1.0);
    	    KK=KK*RI/(TURI-1.0);
    	} // for (I=2; I <= NQUAD; I++)
    	RN=(double)(NQUAD);
    	TUK=2.0*RN+1.0;
    	FORK=2.0*TUK;
    	KK=KK/FORK;
    	
    	if (BETA >= 20.0) {
    	    EXPON=LGGAM(BETA+1.0)-BETA*Math.log(FORK);
    	    if (EXPON <= UFLOW) {
    	        HCO=0.0;
    	    }
    	    else {
    	        HCO=Math.sin(PI*BETA)*Math.exp(EXPON)/PI;
    	    }
    	} // if (BETA >= 20.0)
    	else {
    	    HCO=Math.sin(PI*BETA)*GAMMA(BETA+1.0)/PI/Math.pow(FORK,BETA);
    	}
    	for (I=1; I <= NZZ; I++) {
    		r = zabs(1.0+ZZ[I-1][0],ZZ[I-1][1]);
    		mag = Math.pow(r, BETA);
    		theta = Math.atan2(ZZ[I-1][1], 1.0 + ZZ[I-1][0]);
    		ang = BETA * theta;
    		GG[I-1][0] = mag * Math.cos(ang) * KK;
    		GG[I-1][1] = mag * Math.sin(ang) * KK;
    		HH[I-1][0] = -Math.log(r)*HCO*KK;
    		HH[I-1][1] = -theta * HCO * KK;
    	} // for (I=1; I <= NZZ; I++)
    	
    	// NOW GIVE THE JACOBI POLYNOMIALS THE SCALING CORRESPONDING TO
        // [-1,1] AS STANDARD INTERVAL
    	
    	P0SCL=1.0/Math.sqrt(H0VAL);
    	for (J=1; J <= NZZ; J++) {
    	    PRE[0]=P0SCL;
    	    PRE[1] = 0.0;
    	    CUR[0]=PRE[0];
    	    CUR[1]=PRE[1];
    	    zmlt(CUR[0],CUR[1],GG[J-1][0]*CSCAL[0],GG[J-1][1]*CSCAL[0],cr,ci);
    	    CONGG[0][J-1][0] = cr[0];
    	    CONGG[0][J-1][1] = ci[0];
    	    zmlt(CUR[0],CUR[1],HH[J-1][0]*CSCAL[0],HH[J-1][1]*CSCAL[0],cr,ci);
    	    CONHH[0][J-1][0] = cr[0];
    	    CONHH[0][J-1][1] = ci[0];
    	    if (MAXDG >= 1) {
    	    	zmlt(ZZ[J-1][0] - BCOEF[0],ZZ[J-1][1],PRE[0]/ACOEF[0],PRE[1]/ACOEF[0],cr,ci);
    	    	CUR[0] = cr[0];
    	    	CUR[1] = ci[0];
    	        zmlt(CUR[0],CUR[1],GG[J-1][0]*CSCAL[1],GG[J-1][1]*CSCAL[1],cr,ci);
    	        CONGG[1][J-1][0] = cr[0];
    	        CONGG[1][J-1][1] = ci[0];
    	        for (I=2; I <= MAXDG; I++) {
    	        	zmlt(ZZ[J-1][0]-BCOEF[I-1],ZZ[J-1][1],CUR[0],CUR[1],cr,ci);
    	        	NXT[0] = (cr[0] - ACOEF[I-2]*PRE[0])/ACOEF[I-1];
    	        	NXT[1] = (ci[0] - ACOEF[I-2]*PRE[1])/ACOEF[I-1];
    	            PRE[0]=CUR[0];
    	            PRE[1]=CUR[1];
    	            CUR[0]=NXT[0];
    	            CUR[1]=NXT[1];
    	            zmlt(CUR[0],CUR[1],GG[J-1][0]*CSCAL[I],GG[J-1][1]*CSCAL[I],cr,ci);
    	            CONGG[I][J-1][0] = cr[0];
    	            CONGG[I][J-1][1] = ci[0];
    	        } // for (I=2; I <= MAXDG; I++)
    	    } // if (MAXDG >= 1)
    	} // for (J=1; J <= NZZ; J++)
    	
    	// NOW COMPUTE THE POLYNOMIAL VALUES AT -1, SCALE AND ACCUMULATE INTO
    	// CONHH
    	
    	SUM=BETA+1.0;
    	FAC1=1.0/Math.sqrt(Math.pow(2.0,SUM));
    	FAC2=1.0;
    	for (I=1; I <= MAXDG; I++) {
    	    SUM=SUM+2.0;
    	    FAC1=-FAC1;
    	    FAC2=(I+BETA)*FAC2/I;
    	    PVAL=Math.sqrt(SUM)*FAC1*FAC2;
    	    for (J=1; J <= 2; J++) {
    	        CONHH[I][J-1][0]=PVAL*HH[J-1][0]*CSCAL[I];
    	        CONHH[I][J-1][1]=PVAL*HH[J-1][1]*CSCAL[I];
    	    } // for (J=1; J <= 2; J++)
    	} // for (I=1; I <= MAXDG; I++)
    	
        // NOW COME THE FACTORS DEPENDENT ON TAU1 AND TAU2.
    	
    	LOWER=TAU1[0];
    	UPPER=TAU2[0];
    	FIRST= true;
    	
    	while (true) {
    	
    	    HTOL=0.5*TOL;
    	    RR=(TAU2[0]-TAU1[0])*0.5;
    	    MEAN=(TAU1[0]+TAU2[0])*0.5;
    	    BB=(1.0+MEAN)/RR;
    	    BB1=BB+Math.sqrt(BB*BB-1.0);
    	
    	    // **** NOW COMPUTE THE QUANTITY
    	    // ****
    	    //****      FFH=(RR*(BB1-1E+0/BB1))**(BETA+1E+0)/BB1**TUK
    	    //****
    	    // **** BUT CHECK FOR POSSIBLE UNDERFLOW
    	
    	    BB2=BB1-1.0/BB1;
    	    if (BB2 <= 0.0) {
    	        FFH=0.0;
    	    }
    	    else {
    	        EXPON=(BETA+1.0)*Math.log(RR*BB2)-TUK*Math.log(BB1);
    	        if (EXPON <= UFLOW) {
    	            FFH=0.0;
    	        }
    	        else {
    	            FFH=Math.exp(EXPON);
    	        }
    	    }
    	    K=0;
    	    for (J=1; J <= NZZ; J++) {
    	        XI[0]=(ZZ[J-1][0]-MEAN)/RR;
    	        XI[1] = ZZ[J-1][1]/RR;
    	        zmlt(XI[0],XI[0],XI[1],XI[1],cr,ci);
    	        r = zabs(cr[0]-1.0,ci[0]);
    	        mag = Math.sqrt(r);
    	        theta = Math.atan2(ci[0], cr[0]-1.0);
    	        ang = 0.5 * theta;
    	        Z1[0] = mag;
    	        Z1[1] = ang;
    	        XI1[0]=XI[0]+Z1[0];
    	        XI1[1]=XI[1]+Z1[1];
    	        absXI1 = zabs(XI1[0],XI1[1]);
    	        if (absXI1 < 1.0) {
    	          XI1[0]=XI[0]-Z1[0];
    	          XI1[1]=XI[1]-Z1[1];
    	        }
    	        r = zabs(XI1[0],XI1[1]);
    	        mag = Math.pow(r, -TUK-1.0);
    	        theta = Math.atan2(XI1[1],XI1[0]);
    	        ang = theta *(-TUK - 1.0);
    	        var[0] = mag * Math.cos(ang);
    	        var[1] = mag * Math.sin(ang);
    	        zmlt(XI1[0],XI1[1],XI1[0],XI1[1],cr,ci);
    	        zmlt(var[0],var[1],cr[0]-1.0,ci[0],cr2,ci2);
    	        FFG[0] = cr2[0] * RR;
    	        FFG[1] = ci2[0] * RR;
    	        for (I=0; I <= MAXDG; I++) {
    	            K=K+1;
    	            zmlt(CONGG[I][J-1][0],CONGG[I][J-1][1],FFG[0],FFG[1],cr,ci);
    	            REMND[K-1][0] = cr[0] + CONHH[I][J-1][0]*FFH;
    	            REMND[K-1][1] = cr[0] + CONHH[I][J-1][1]*FFH;
    	        } // for (I=0; I <= MAXDG; I++)
    	    } // for (J=1; J <= NZZ; J++)
    	
    	      MAXRM[0]=0.0;
    	      for (I=1; I <= 2*MAXDG+2; I++) {
    	        TERM=Math.abs(REMND[I-1][0]);
    	        MAXRM[0]=Math.max(MAXRM[0],TERM);
    	      } // for (I=1; I <= 2*MAXDG+2; I++)
    	
    	      if (MAXRM[0] < TOL) {
    	
    	          // ACCURACY IS ACHIEVED, BUT MAYBE TAU2 COULD BE INCREASED OR
    	          // TAU1 DECREASED
    	
    	          if (MAXRM[0] < HTOL) {
    	
    	              // TAU2 NEEDS INCREASING IF T1FXD (BUT THIS IS ONLY POSSIBLE IF 
    	              // TAU2<1) OR TAU1 NEED INCREASING OTHERWISE (BUT THIS IS ONLY
                      // POSSIBLE IF TAU1>-1)
    	
    	              if (T1FXD && TAU2[0] < 1.0) {
    	                  LOWER=TAU2[0];
    	                  TAU2[0]=0.5*(LOWER+UPPER);
    	                  continue;
    	              }
    	              else if (!T1FXD && TAU1[0] > -1.0) {
    	                  UPPER=TAU1[0];
    	                  TAU1[0]=0.5*(LOWER+UPPER);
    	                  continue;
    	              }
    	              else {
    	            	  break;
    	              }
    	          } // if (MAXRM[0] < HTOL
    	          else {
    	        	  break;
    	          }
    	      } // if (MAXRM[0] < TOL)
    	      else { // MAXRM[0] >= TOL
    	
    	          // ACCURACY NOT ACHIEVED AND TAU2 NEEDS DECREASING OR TAU1 NEEDS
    	          // INCREASING.
    	
    	          if (FIRST) {
    	              TOL=HTOL;
    	              FIRST=false;
    	          }
    	          if (T1FXD) {
    	              UPPER=TAU2[0];
    	              TAU2[0]=0.5*(LOWER+UPPER);
    	          }
    	          else {
    	              LOWER=TAU1[0];
    	              TAU1[0]=0.5*(LOWER+UPPER);
    	          }
    	          continue;
    	      } // else MAXRM[0] >= TOL
    	} // while (true)
    	
    	// NORMAL TERMINATION
    	
    	IER[0]=0;
    	
    } // private void DELEG7


    private void RSLT71(int QIERC[], double RCOND,double SOLUN[], int NEQNS, int LOSUB[],
        int HISUB[], double COLSC[], int NQPTS, int JATYP[], int PARNT[], int TNSUA, boolean INTER,
        double MQERR, double MCQER, double CINFN[], int ACTIN[], int NEWDG[], int NJIND,
        double JACIN[], int NQUAD[], double TOLOU[], double LGTOL, int SOLCO) {
    	//INTEGER NEQNS,TNSUA,OUCH1,NQPTS,NJIND,NEWDG(*),NQUAD(*),LOSUB(*),
    	//+HISUB(*),QIERC(0:6),JATYP(*),PARNT(*),ACTIN(*),SOLCO
    	//REAL SOLUN(*),RCOND,COLSC(*),MQERR,MCQER,LGTOL,
    	//+CINFN(*),JACIN(*),TOLOU(*)
    	//LOGICAL INTER
    	String QTEXT[] = new String[7];
    	final String LINE = "_________________________________________________________________";
    	//CHARACTER QTEXT(0:6)*22,LINE*72
    	      
    	// LOCAL VARIABLES
    	
    	int I,J,JI,K,L,LOD,N,H;
    	double S,CAP;
    	
    	QTEXT[0]="...........NORMAL EXIT";
    	QTEXT[1]=".....MAX. SUBDIVISIONS";
    	QTEXT[2]="....ROUNDOFF DETECTION";
    	QTEXT[3]=".........BAD INTEGRAND";
    	QTEXT[6]=".........INVALID INPUT";

    	Preferences.debug(LINE+"\n", Preferences.DEBUG_ALGORITHM);
    	Preferences.debug("             SOLUTION NUMBER = " + SOLCO + "\n", Preferences.DEBUG_ALGORITHM);
    	Preferences.debug("                       NEQNS = " + NEQNS + "\n", Preferences.DEBUG_ALGORITHM); 
    	Preferences.debug("RECIPROCAL COND NO. ESTIMATE = " + RCOND + "\n", Preferences.DEBUG_ALGORITHM);
    	Preferences.debug("   CONDITION NO. LOWER BOUND = " + (1.0/RCOND) + "\n", Preferences.DEBUG_ALGORITHM);
    	
    	Preferences.debug("\n", Preferences.DEBUG_ALGORITHM);
    	Preferences.debug("JACOBI INDEX           POINTS          TOLERANCE ACHIEVED\n", Preferences.DEBUG_ALGORITHM);
    	for (I=1; I <= NJIND; I++) {
    	    Preferences.debug("     "+I+"                 "+NQUAD[I-1]+"           "+TOLOU[I-1]+"\n", Preferences.DEBUG_ALGORITHM);
    	} // for (I=1; I <= NJIND; I++)+
    	
    	
    	Preferences.debug("\n", Preferences.DEBUG_ALGORITHM);
    	Preferences.debug("QAWS TERMINATIONS WITH......\n", Preferences.DEBUG_ALGORITHM);
    	for (I=0; I <= 6; I++) {
    	    if (QIERC[I] > 0) {
    	        Preferences.debug(QTEXT[I] + " " + QIERC[I] + "\n", Preferences.DEBUG_ALGORITHM);
    	    }
    	}      
    	
    	Preferences.debug("\n", Preferences.DEBUG_ALGORITHM);
    	Preferences.debug("              MAXIMUM QAWS ERROR = " + MQERR + "\n", Preferences.DEBUG_ALGORITHM);
    	Preferences.debug("MAXIMUM COMPOSITE GAUSSIAN ERROR = " + MCQER + "\n", Preferences.DEBUG_ALGORITHM);
    	Preferences.debug("\n", Preferences.DEBUG_ALGORITHM);
    	      
    	for (I=1; I <= TNSUA; I++) {
    		Preferences.debug("\n", Preferences.DEBUG_ALGORITHM);
    	    Preferences.debug("SUB ARC = " + I + " ON PARENT ARC " + PARNT[I-1] + "\n", Preferences.DEBUG_ALGORITHM);
    	    Preferences.debug("N     SCALED SOLUN       UNSCALED SOLUN    IGNORE LEVEL\n", Preferences.DEBUG_ALGORITHM);
    	    L=LOSUB[I-1];
    	    H=HISUB[I-1];
    	    JI=Math.abs(JATYP[I-1]);
    	    LOD=(JI-1)*NQPTS+1;
    	    for (J=L; J <= H; J++) {
    	        N=J-L;
    	        K=LOD+N;
    	        S=SOLUN[J-1];
    	        Preferences.debug(N+"  "+S+"    "+(S*COLSC[K-1])+"    "+(LGTOL/CINFN[J-1])+ "\n", Preferences.DEBUG_ALGORITHM);
    	    } // for (J=L; J <= H; J++)
    	    if (ACTIN[I-1] == -1) {
    	        Preferences.debug("ACTION: REDUCE DEGREE TO " + NEWDG[I-1] + " ***\n", Preferences.DEBUG_ALGORITHM);
    	    }
    	    else if (ACTIN[I-1] == 0) {
    	        Preferences.debug("ACTION: NONE            ***\n", Preferences.DEBUG_ALGORITHM);
    	    }
    	    else if (ACTIN[I-1] == 1) {
    	        Preferences.debug("ACTION: INCREASE DEGREE TO " + NEWDG[I-1]+ "\n", Preferences.DEBUG_ALGORITHM);
    	    }
    	    else {
    	        Preferences.debug("ACTION: SUBDIVIDE THIS ARC\n", Preferences.DEBUG_ALGORITHM);
    	    }
    	} // for (I=1; I <= TNSUA; I++)
    	
    	Preferences.debug("KAPPA = " + SOLUN[NEQNS-1] + "\n", Preferences.DEBUG_ALGORITHM);
    	if (!INTER) {
    	    CAP=Math.exp(-SOLUN[NEQNS-1]);
    	    Preferences.debug("CAPACITY = " + CAP + "\n", Preferences.DEBUG_ALGORITHM);
    	}
    	
    } // private void RSLT71
    
    private void RSLT72(int QIERC[], double RCOND, double GAMMA, int NEQNS, int DGPOL[],
        int JATYP[], int PARNT[], int TNSUA, boolean INTER, double MQERR, double MCQER,
        int ACTIN[], int NEWDG[], int NJIND, int NQUAD[], double TOLOU[], double LGTOL,
    	int SOLCO) {
    	// INTEGER NEQNS,TNSUA,OUCH1,NJIND,NEWDG(*),NQUAD(*),QIERC(0:6),
    	// PARNT(*),ACTIN(*),DGPOL(*),JATYP(*),SOLCO
    	// REAL GAMMA,RCOND,MQERR,MCQER,LGTOL,TOLOU(*)
    	// LOGICAL INTER
    	 
    	// LOCAL VARIABLES
    	
    	int I;
    	double CAP;
    	// CHARACTER QTEXT(0:6)*22,LINE*72
    	String QTEXT[] = new String[7];
    	final String LINE="_________________________________________________________________";
    	
    	QTEXT[0]="...........NORMAL EXIT";
    	QTEXT[1]=".....MAX. SUBDIVISIONS";
    	QTEXT[2]="....ROUNDOFF DETECTION";
    	QTEXT[3]=".........BAD INTEGRAND";
    	QTEXT[6]=".........INVALID INPUT";
    	
    	Preferences.debug(LINE+"\n", Preferences.DEBUG_ALGORITHM);
    	Preferences.debug("             SOLUTION NUMBER = " + SOLCO + "\n", Preferences.DEBUG_ALGORITHM);
    	Preferences.debug("                       NEQNS = " + NEQNS + "\n", Preferences.DEBUG_ALGORITHM); 
    	Preferences.debug("RECIPROCAL COND NO. ESTIMATE = " + RCOND + "\n", Preferences.DEBUG_ALGORITHM);
    	Preferences.debug("   CONDITION NO. LOWER BOUND = " + (1.0/RCOND) + "\n", Preferences.DEBUG_ALGORITHM);
    	
    	Preferences.debug("\n", Preferences.DEBUG_ALGORITHM);
    	Preferences.debug("JACOBI INDEX           POINTS          TOLERANCE ACHIEVED\n", Preferences.DEBUG_ALGORITHM);
    	for (I=1; I <= NJIND; I++) {
    	    Preferences.debug("     "+I+"                 "+NQUAD[I-1]+"           "+TOLOU[I-1]+"\n", Preferences.DEBUG_ALGORITHM);
    	} // for (I=1; I <= NJIND; I++)+
    	
    	
    	Preferences.debug("\n", Preferences.DEBUG_ALGORITHM);
    	Preferences.debug("QAWS TERMINATIONS WITH......\n", Preferences.DEBUG_ALGORITHM);
    	for (I=0; I <= 6; I++) {
    	    if (QIERC[I] > 0) {
    	        Preferences.debug(QTEXT[I] + " " + QIERC[I] + "\n", Preferences.DEBUG_ALGORITHM);
    	    }
    	}
    	
    	Preferences.debug("\n", Preferences.DEBUG_ALGORITHM);
    	Preferences.debug("              MAXIMUM QAWS ERROR = " + MQERR + "\n", Preferences.DEBUG_ALGORITHM);
    	Preferences.debug("MAXIMUM COMPOSITE GAUSSIAN ERROR = " + MCQER + "\n", Preferences.DEBUG_ALGORITHM);
    	Preferences.debug("\n", Preferences.DEBUG_ALGORITHM);
    	Preferences.debug("SUB ARC   PARENT ARC   TYPE     CURRENT DEGREE     ACTION\n", Preferences.DEBUG_ALGORITHM);
    	for (I=1; I <= TNSUA; I++) {
    	    if (ACTIN[I-1] == -1) {
    	          Preferences.debug("  "+I+"         "+PARNT[I-1]+"        "+JATYP[I-1]+"            "+
    	                            DGPOL[I-1]+" REDUCE TO " + NEWDG[I-1] + "\n", Preferences.DEBUG_ALGORITHM);
    	    }
    	    else if (ACTIN[I-1] == 0) {
    	    	Preferences.debug("  "+I+"         "+PARNT[I-1]+"        "+JATYP[I-1]+"            "+
                        DGPOL[I-1]+" NONE " + "\n", Preferences.DEBUG_ALGORITHM);
    	    }
    	    else if (ACTIN[I-1] == 1) {
    	    	Preferences.debug("  "+I+"         "+PARNT[I-1]+"        "+JATYP[I-1]+"            "+
                        DGPOL[I-1]+" INCREASE TO " + NEWDG[I-1] + "\n", Preferences.DEBUG_ALGORITHM);
    	    }
    	    else {
    	    	Preferences.debug("  "+I+"         "+PARNT[I-1]+"        "+JATYP[I-1]+"            "+
                        DGPOL[I-1]+" SUBDIVIDE " + "\n", Preferences.DEBUG_ALGORITHM);
    	    }
    	} // for (I=1; I <= TNSUA; I++)
    	
    	Preferences.debug("KAPPA = " + GAMMA + "\n", Preferences.DEBUG_ALGORITHM);
    	if (!INTER) {
    	    CAP=Math.exp(-GAMMA);
    	    Preferences.debug("CAPACITY = " + CAP + "\n", Preferences.DEBUG_ALGORITHM);
    	}
    } // private void RSLT72

    
    private void LNSY11(double MATRX[][][], double RGTSD[], int MNEQN, int NCOLL, int ORDSG, boolean REFLN,
        int NQPTS, int TNSUA, int JATYP[], int PARNT[], int DGPOL[], int LOSUB[], int HISUB[], int NQUAD[],
        int LOQSB[], double TOLNR, double MIDPT[], double HALEN[], double H0VAL[], double COLSC[], double ACOEF[],
        double BCOEF[], double COLPR[], double QCOMX[], double QCOMW[], double CENTR[], double ZCOLL[][],
        boolean INTER, boolean LNSEG[], double WORK[], int QIERR[], double MQERR[], double JACIN[], double A1COF[],
        double B1COF[], double AQTOL, double RQTOL, double AQCOF[], double BQCOF[], double CQCOF[],
        int LOOLD[], int HIOLD[]) {
    	
        // INTEGER MNEQN,NCOLL,ORDSG,NQPTS,TNSUA,JATYP(*),PARNT(*),
    	//+DGPOL(*),LOSUB(*),HISUB(*),NQUAD(*),LOQSB(*),QIERR(0:6),LOOLD(*),
    	//+HIOLD(*)
    	
    	//REAL MATRX(MNEQN,MNEQN,2),RGTSD(*),TOLNR,MIDPT(*),HALEN(*),
    	//+H0VAL(*),COLSC(*),ACOEF(*),BCOEF(*),COLPR(*),QCOMX(*),QCOMW(*),
    	//+MQERR,WORK(*),A1COF(*),B1COF(*),AQTOL,
    	//+JACIN(*),RQTOL,AQCOF(*),BQCOF(*),CQCOF(*)
    	
    	//COMPLEX CENTR,ZCOLL(*)
    	
    	//LOGICAL INTER,LNSEG(*),REFLN
    	
    	// TO SET UP THE INITIAL COLLOCATION MATRIX (MATRX) AND THE RIGHT
    	// HAND SIDE VECTOR (RGTSD).  WE ASSUME COLLOCATION DOES NOT TAKE 
    	// PLACE AT END POINTS OF ARCS.
    	
    	//.......................................................................
    	//     THIS ROUTINE IS ADAPTED FROM LNSY10 AND IS DESIGNED TO FORM ONLY
    	//     THOSE LINEAR EQUATIONS THAT ARISE FROM COLLOCATING AT POINTS ON
    	//     THE FUNDAMENTAL PART OF THE BOUNDARY
    	//.......................................................................
    	
    	//     LOCAL VARIABLES
    	
        int IER[] = new int[1];
    	int I,J,I1,J1,J2,J3,JO,IA,IQ,IS,JI,AJI,PT,DG,LOM,HIM,LOQ,LOD,
    	    NQ,NCFBS,NROWS,NCOLS,ORDRG,ISAMAX,NVAL,IC,LOC,HIC,TSFBS;
    	double MD,HL,RH,TQ,WQ,DD,LDD,SG,SJI,TC,C0,
    	       FNVAL,U0,U1,CURR,PREV,NEXT,ABER0,ABER1;
    	double GTQ[] = new double[2];
    	double ZQ[] = new double[2];
    	double ZC[] = new double[2];
    	// COMPLEX GTQ,ZQ,ZC,PARFUN,DPARFN
    	boolean OLDIA;
    	//COMMON /FNDEF/BETA,A1,B1,P0VAL,SCALE,TYPE
    	// EXTERNAL DPARFN,FNVAL,ISAMAX,JAPAR7,PARFUN,QAWS
    	qaws qmod;
    	int routine = Integration2.DQAWSE;
    	double DOUT[];
    	double DIN[] = new double[2];
    	/**
         * Gives an upper bound on the number of subintervals in the partition of lower, upper.
         */
        int limit = 100;
    	
    	NCFBS=NCOLL/ORDSG;
    	TSFBS=TNSUA/ORDSG;
    	NROWS=NCFBS+1;
    	NCOLS=NCOLL+1;
    	
    	for (J=1; J <= NCOLS; J++) {
    	    for (I=1; I <= NROWS; I++) {
    	        MATRX[I-1][J-1][1]=0.0;
    	    }
    	} // for (J=1; J <= NCOLS; J++)
    	for (I=0; I <= 6; I++) {
    	    QIERR[I]=0;
    	}
    	MQERR[0]=0.0;
    	
    	// NOW SELECT THE INTEGRATION ARC
    	
    	for (IA=1; IA <= TNSUA; IA++) {

    	
    	    // INITIALISE DATA FOR THIS ARC
    	
    	    PT=PARNT[IA-1];
    	    MD=MIDPT[IA-1];
    	    HL=HALEN[IA-1];
    	    DG=DGPOL[IA-1];
    	    LOM=LOSUB[IA-1];
    	    HIM=HISUB[IA-1];
    	    JI=JATYP[IA-1];
    	    if (JI < 0) {
    	        SJI=-1.0;
    	    }
    	    else {
    	        SJI=1.0;
    	    }
    	    AJI=Math.abs(JI);
    	    LOD=(AJI-1)*NQPTS+1;
    	    RH=Math.sqrt(H0VAL[AJI-1]);
    	    BETA=JACIN[AJI-1];
    	    A1=A1COF[AJI-1];
    	    B1=B1COF[AJI-1];
    	    P0VAL=1.0/RH;
    	
    	    // ASSIGN THE NON-ZERO ELEMENT FOR THE SIDE EQUATION FOR THIS ARC
    	
    	    MATRX[NROWS-1][LOM-1][1]=RH*COLSC[LOD-1];
    	
    	    // SET UP THE DIAGONAL BLOCK OF SCALED PRINCIPAL SINGULAR INTEGRALS
    	    // IF THIS DOESN'T ALREADY EXIST.
    	
    	    JO=LOOLD[IA-1];
    	    OLDIA=(JO > 0);
    	    if (!OLDIA && HIM <= NCFBS) {
    	        for (I1=LOM; I1 <= HIM; I1++) {
    	            TC=SJI*COLPR[I1-1];
    	            SCALE=COLSC[LOD-1];
    	            TYPE=1;
    	            // Use dqawse which is the same as (quadpack/dqaws) but provides more
    	            // information and control
    	            qmod = new qaws(-1.0,TC,routine,BETA,0.0,3,AQTOL,RQTOL,limit);
    	            qmod.driver();
    	            U0 = qmod.getIntegral();
    	            ABER0 = qmod.getAbserr();
    	            NVAL = qmod.getNeval();
    	            IER[0] = qmod.getErrorStatus();
    	            QIERR[IER[0]]=QIERR[IER[0]]+1;
    	            TYPE=2;
    	            qmod = new qaws(TC,1.0,routine,0.0,0.0,2,AQTOL,RQTOL,limit);
    	            qmod.driver();
    	            U1 = qmod.getIntegral();
    	            ABER1 = qmod.getAbserr();
    	            NVAL = qmod.getNeval();
    	            IER[0] = qmod.getErrorStatus();
    	            QIERR[IER[0]]=QIERR[IER[0]]+1;
    	            WORK[0]=U0+U1;
    	            MQERR[0]=Math.max(MQERR[0],ABER0+ABER1);
    	            if (DG > 0) {
    	                SCALE=COLSC[LOD];
    	                TYPE=3;
    	                qmod = new qaws(-1.0,TC,routine,BETA,0.0,3,AQTOL,RQTOL,limit);
    	                qmod.driver();
        	            U0 = qmod.getIntegral();
        	            ABER0 = qmod.getAbserr();
        	            NVAL = qmod.getNeval();
        	            IER[0] = qmod.getErrorStatus();
        	            QIERR[IER[0]]=QIERR[IER[0]]+1;
    	                TYPE=4;
    	                qmod = new qaws(TC,1.0,routine,0.0,0.0,2,AQTOL,RQTOL,limit);
    	                qmod.driver();
        	            U1 = qmod.getIntegral();
        	            ABER1 = qmod.getAbserr();
        	            NVAL = qmod.getNeval();
        	            IER[0] = qmod.getErrorStatus();
        	            QIERR[IER[0]]=QIERR[IER[0]]+1;
    	                WORK[1]=U0+U1;
    	                MQERR[0]=Math.max(MQERR[0],ABER0+ABER1);
    	
    	                // NOW USE THE (WEAKLY) STABLE FORWARD RECURRENCE SCHEME  
    	                // FOR WORK(I),I=3,NQPTS (WITH SCALE FACTOR FOR WORK(2))
    	
    	                CURR=WORK[1];
    	                PREV=SCALE;
    	                for (I=1; I <= DG-1; I++) {
    	                    J=LOD+I-1;
    	                    NEXT=(AQCOF[J-1]*TC-BQCOF[J-1])*CURR-CQCOF[J-1]*PREV;
    	                    WORK[I+1]=NEXT;
    	                    PREV=CURR;
    	                    CURR=NEXT;
    	                } // for (I=1; I <= DG-1; I++)
    	
    	                // ASSIGN CORRECT SCALE FACTORS.
    	
    	                for (I=3; I <= DG+1; I++) {
    	                    J=LOD+I-1;
    	                    WORK[I-1]=WORK[I-1]*COLSC[J-1]/SCALE;
    	                } // for (I=3; I <= DG+1; I++)
    	            } // if (DG > 0)
    	
    	            SG=1.0;
    	            for (J=LOM; J <= HIM; J++) {
    	                MATRX[I1-1][J-1][1]=MATRX[I1-1][J-1][1]+SG*WORK[J-LOM];
    	                SG=SG*SJI;
    	            } // for (J=LOM; J <= HIM; J++)
    	
    	        } // for (I1=LOM; I1 <= HIM; I1++)
    	    } // if (!OLDIA && HIM <= NCFBS)

    	
    	    // INITIALISE SOME DATA FOR THE NON-SINGULAR INTEGRALS
    	
    	    WORK[0]=1.0/RH;
    	    NQ=NQUAD[AJI-1];
    	    LOQ=LOQSB[AJI-1];
    	
    	    for (IQ=1; IQ <= NQ; IQ++) {
    	        I=LOQ+IQ-1;
    	        TQ=QCOMX[I-1];
    	        WQ=QCOMW[I-1];
    	        double AA[] = new double[DG];
    	        double BB[] = new double[DG];
    	        for (I = 0; I < DG; I++) {
    	        	AA[I] = ACOEF[LOD-1+I];
    	        	BB[I] = BCOEF[LOD-1+I];
    	        }
    	        JAPAR7(WORK,TQ,AA,BB,DG);
    	        if (JI < 0) {
    	            TQ=-TQ;
    	        }
    	        GTQ[0] = MD+HL*TQ;
    	        GTQ[1] = 0.0;
    	        ZQ=PARFUN(PT,GTQ);
    	
    	        // ACCUMULATE ALL NEW ELEMENTS NOT ON THE DIAGONAL BLOCK
    	
    	        for (IC=1; IC <= TSFBS; IC++) {
    	            J2=LOOLD[IC-1];
    	            if (IC != IA && (J2 == 0 || !OLDIA)) {
    	                LOC=LOSUB[IC-1];
    	                HIC=HISUB[IC-1];
    	                for (I1=LOC; I1 <= HIC; I1++) {
    	                    ZC[0]=ZCOLL[I1-1][0];
    	                    ZC[1]=ZCOLL[I1-1][1];
    	                    DD=zabs(ZC[0]-ZQ[0],ZC[1]-ZQ[1]);
    	                    LDD=Math.log(DD)*WQ;
    	                    SG=1.0;
    	                    for (J1=LOM; J1 <= HIM; J1++) {
    	                        J=J1-LOM+1;
    	                        I=J1-LOM+LOD;
    	                        MATRX[I1-1][J1-1][1]=MATRX[I1-1][J1-1][1]+SG*WORK[J-1]*LDD*COLSC[I-1];
    	                        SG=SG*SJI;
    	                    } // for (J1=LOM; J1 <= HIM; J1++)
    	                } // for (I1=LOC; I1 <= HIC; I1++)
    	            } // if (IC != IA && (J2 == 0 || !OLDIA))
    	        } // for (IC=1; IC <= TSFBS; IC++)
    	
    	        // ACCUMULATE THE RESIDUAL NON-SINGULAR CONTRIBUTIONS INTO
    	        // ANY NEW DIAGONAL BLOCK FOR THE NON-LINE-SEGMENT CASE.
    	
    	        if (!LNSEG[IA-1] && !OLDIA && HIM <= NCFBS) {
    	            for (I1=LOM; I1 <= HIM; I1++) {
    	                TC=COLPR[I1-1];
    	                DD=Math.abs(TC-TQ);
    	                if (DD <= TOLNR) {
    	                	DOUT = DPARFN(PT,GTQ);
    	                	DD = zabs(DOUT[0],DOUT[1])*HL;
    	                }
    	                else {
    	                    ZC[0]=ZCOLL[I1-1][0];
    	                    ZC[1]=ZCOLL[I1-1][1];
    	                    DD=zabs(ZC[0]-ZQ[0],ZC[1]-ZQ[1])/DD;
    	                    if (DD < TOLNR) {
    	                    	DOUT = DPARFN(PT,GTQ);
        	                	DD = zabs(DOUT[0],DOUT[1])*HL;
    	                    }
    	                }
    	                LDD=Math.log(DD)*WQ;
    	                SG=1.0;
    	                for (J1=LOM; J1 <= HIM; J1++) {
    	                    J=J1-LOM+1;
    	                    I=J1-LOM+LOD;
    	                    MATRX[I1-1][J1-1][1]=MATRX[I1-1][J1-1][1]+SG*WORK[J-1]*LDD*COLSC[I-1];
    	                    SG=SG*SJI;
    	                } // for (J1=LOM; J1 <= HIM; J1++)
    	            } // for (I1=LOM; I1 <= HIM; I1++)
    	        } // if (!LNSEG[IA-1] && !OLDIA && HIM <= NCFBS)
    	    } // for (IQ=1; IQ <= NQ; IQ++)
    	
    	    // ACCUMULATE THE RESIDUAL NON-SINGULAR CONTRIBUTIONS INTO
    	    // ANY NEW DIAGONAL BLOCK FOR THE LINE-SEGMENT CASE.
    	
    	    if (LNSEG[IA-1] && !OLDIA && HIM <= NCFBS) {
    	    	  DIN[0] = MD;
    	    	  DIN[1] = 0.0;
    	    	  DOUT = DPARFN(PT,DIN);
    	    	  ZC[0] = DOUT[0]*HL;
    	    	  ZC[1] = DOUT[1]*HL;
    	          C0=zabs(ZC[0],ZC[1]);
    	          C0=RH*Math.log(C0)*COLSC[LOD-1];
    	          for (I1=LOM; I1 <= HIM; I1++) {
    	              MATRX[I1-1][LOM-1][1]=MATRX[I1-1][LOM-1][1]+C0;
    	          }
    	    } // if (LNSEG[IA-1] && !OLDIA && HIM <= NCFBS)
    	
    	    // NOW COPY OVER ANY RELEVANT ELEMENTS ALREADY AVAILABLE IN
    	    // MATRX(*,*,1)
    	
    	    if (OLDIA) {
    	        for (IC=1; IC <= TSFBS; IC++) {
    	            J2=LOOLD[IC-1];
    	            if (J2 > 0) {
    	                LOC=LOSUB[IC-1];
    	                HIC=HISUB[IC-1];
    	                J2=J2-1;
    	                for (I1=LOC; I1 <= HIC; I1++) {
    	                    J2=J2+1;
    	                    for (J1=LOM; J1 <= HIM; J1++) {
    	                        J=J1+JO-LOM;
    	                        MATRX[I1-1][J1-1][1]=MATRX[J2-1][J-1][0];
    	                    } // for (J1=LOM; J1 <= HIM; J1++)
    	                } // for (I1=LOC; I1 <= HIC; I1++)
    	            } // if (J2 > 0)
    	        } // for (IC=1; IC <= TSFBS; IC++)
    	    } // if (OLDIA)
    	
    	} // for (IA=1; IA <= TNSUA; IA++)
    	
    	// SET UP THE LAST COLUMN
    	
    	for (I=1; I <= NCFBS; I++) {
    	    MATRX[I-1][NCOLS-1][1]=1.0;
    	}
    	
        // FINALLY SET UP THE RIGHT HAND SIDE VECTOR
    	
    	if (INTER) {
    	    for (I=1; I <= NCFBS; I++) {
    	        ZC[0]=ZCOLL[I-1][0]-CENTR[0];
    	        ZC[1]=ZCOLL[I-1][1]-CENTR[1];
    	        RGTSD[I-1]=Math.log(zabs(ZC[0],ZC[1]));
    	    }
    	}
    	else {
    	    for (I=1; I <= NCFBS; I++) {
    	        RGTSD[I-1]=0.0;
    	    }
    	}
    	RGTSD[NROWS-1]=1.0;
    	
    	// COPY MATRX(*,*,2) ONTO MATRX(*,*,1)
    	
    	for (J=1; J <= NCOLS; J++) {
    	    for (I=1; I <= NROWS; I++) {
    	        MATRX[I-1][J-1][0]=MATRX[I-1][J-1][1];
    	    }
    	} // for (J=1; J <= NCOLS; J++)
    	
    	// COMBINE COLUMNS OF MATRX(*,*,2) TOGETHER ACCORDING TO SYMMETRY
    	// SPECIFICATIONS
    	
    	if (ORDSG > 1) {
    	    if (REFLN) {
    	        ORDRG=ORDSG/2;
    	        for (IS=2; IS <= ORDRG; IS++) {
    	            LOM=(IS-1)*NCFBS*2;
    	            for (I=1; I <= NROWS; I++) {
    	                for (J=1; J <= NCFBS*2; J++) {
    	                    J1=LOM+J;
    	                    MATRX[I-1][J-1][1]=MATRX[I-1][J-1][1]+MATRX[I-1][J1-1][1];
    	                } // for (J=1; J <= NCFBS*2; J++)
    	            } // for (I=1; I <= NROWS; I++)
    	        } // for (IS=2; IS <= ORDRG; IS++)
    	        for (IA=1; IA <= TSFBS; IA++) {
    	            J1=LOSUB[IA-1];
    	            J2=HISUB[IA-1];
    	            LOM=LOSUB[2*TSFBS-IA]-J1;
    	            SG=-1.0;
    	            for (J=J1; J <= J2; J++) {
    	                SG=-SG;
    	                J3=LOM+J;
    	                for (I=1; I <= NROWS; I++) {
    	                    MATRX[I-1][J-1][1]=MATRX[I-1][J-1][1]+SG*MATRX[I-1][J3-1][1];
    	                }
    	            } // for (J=J1; J <= J2; J++)
    	        } // for (IA=1; IA <= TSFBS; IA++)
    	    } // if (REFLN)
    	    else {
    	        for (IS=2; IS <= ORDSG; IS++) {
    	            LOM=(IS-1)*NCFBS;
    	            for (I=1; I <= NROWS; I++) {
    	                for (J=1; J <= NCFBS; J++) {
    	                    J1=LOM+J;
    	                    MATRX[I-1][J-1][1]=MATRX[I-1][J-1][1]+MATRX[I-1][J1-1][1];
    	                } // for (J=1; J <= NCFBS; J++)
    	            } // for (I=1; I <= NROWS; I++)
    	        } // for (IS=2; IS <= ORDSG; IS++)
    	    } // else
    	
    	    for (I=1; I <= NROWS; I++) {
    	        MATRX[I-1][NROWS-1][1]=MATRX[I-1][NCOLS-1][1];
    	    }
    	} // if (ORDSG > 1)
    	
    } // private void LNSY11
    
    class qaws extends Integration2 {
    	
    	public qaws(double lower, double upper, int routine, double alfa,
                double beta, int integr, double epsabs, double epsrel, int limit) {
	        super(lower, upper, routine, alfa, beta, integr, epsabs, epsrel, limit);
    	}
    	
    	public void driver() {
			super.driver();
		}
    	
    	public double intFunc(double X) {
    		double result;

            if (TYPE == 1) {
                result=P0VAL*SCALE;
            }
            else if (TYPE == 2) {
                result=Math.pow((1.0+X),BETA)*P0VAL*SCALE;
            }
            else if (TYPE == 3) {
                result=(X-B1)*P0VAL*SCALE/A1;
            }
            else {
                result=Math.pow((1.0+X),BETA)*(X-B1)*P0VAL*SCALE/A1;
            }
            return result; 	
    	}
    }

    private double FNVAL(double X) {

        // LOCAL VARIABLES.

        // INTEGER TYPE
        // REAL BETA,A1,B1,P0VAL,SCALE
        // COMMON /FNDEF/BETA,A1,B1,P0VAL,SCALE,TYPE
    	double result;

        if (TYPE == 1) {
            result=P0VAL*SCALE;
        }
        else if (TYPE == 2) {
            result=Math.pow((1.0+X),BETA)*P0VAL*SCALE;
        }
        else if (TYPE == 3) {
            result=(X-B1)*P0VAL*SCALE/A1;
        }
        else {
            result=Math.pow((1.0+X),BETA)*(X-B1)*P0VAL*SCALE/A1;
        }
        return result;
    } // private double FNVAL

    private void BCFVTF(double BCFSN[], double VTARG[], int DGPOL[], int JATYP[], int LOSUB[], int PARNT[],
        int RFARC, int TNSUA, double H0VAL[], double JACIN[], double RFARG, double SOLUN[]) {
    	// INTEGER RFARC,TNSUA
    	// INTEGER DGPOL(*),JATYP(*),LOSUB(*),PARNT(*)
    	// REAL RFARG
    	// REAL BCFSN(*),H0VAL(*),JACIN(*),SOLUN(*),VTARG(*)
    	
    	//     PERFORMS VARIOUS PRELIMINARY TASKS TO PREPARE FOR THE POST-
    	//     PROCESSING QUADRATURE CALCULATIONS.
    	
    	//     SETS UP THE ARRAY OF COEFFICIENTS BCFSN NEEDED FOR THE 
    	//     CALCULATION OF THE BOUNDARY CORRESPONDENCE FUNCTIONS FOR THE MAP
    	//     PHYSICAL --> CANONICAL; THESE ARE SIMPLY RELATED TO THE SOLUTION 
    	//     COEFFICIENT ARRAY SOLUN.
    	
    	//     CALCULATES THE ARGUMENTS VTARG OF THE IMAGES ON THE UNIT CIRCLE
    	//     OF THE END POINTS OF ALL SUBARCS, ENFORCING THE USER'S ROTATION
    	//     CONDITION THAT PARFUN(RFARC,(-1.0,0.0)) BE MAPPED TO THE POINT
    	//     WITH ARGUMENT RFARG.
    	
    	//     LOCAL VARIABLES
    	
    	int AJT,DEG,I,J,J1,JT,LO;
    	double B1,RTH0,THET0,TUPI;
    	
        TUPI=2.0*Math.PI;
    	VTARG[0]=0.0;
    	
    	for (I=1; I <= TNSUA; I++) {
    	    JT=JATYP[I-1];
    	    AJT=Math.abs(JT);
    	    RTH0=Math.sqrt(H0VAL[AJT-1]);
    	    B1=JACIN[AJT-1]+1.0;
    	    DEG=DGPOL[I-1];
    	    LO=LOSUB[I-1];
    	    VTARG[I]=VTARG[I-1]+SOLUN[LO-1]*TUPI*RTH0;
    	
    	    BCFSN[LO-1]=TUPI*SOLUN[LO-1]/(B1*RTH0);
    	    for (J=1; J <= DEG; J++) {
    	        J1=LO+J;
    	        BCFSN[J1-1]=TUPI*SOLUN[J1-1]/Math.sqrt(J*(J+B1));
    	    } // for (J=1; J <= DEG; J++)
    	
    	    if (JT < 0) {
    	        for (J=1; J <= DEG; J += 2) {
    	            J1=LO+J;
    	            BCFSN[J1-1]=-BCFSN[J1-1];
    	        } // for (J=1; J <= DEG; J += 2)
    	    } // if (JT < 0)
    	
    	} // for (I=1; I <= TNSUA; I++)
    	
    	I=1;
    	while (PARNT[I-1] != RFARC) {
    		I = I+1;
    	}
    	THET0=RFARG-VTARG[I-1];
    	
    	for (I = 1; I <= TNSUA; I++) {
    	    VTARG[I-1]=VTARG[I-1]+THET0;
    	}
    	VTARG[TNSUA]=VTARG[0]+TUPI;
    	
    } // private void BCFVTF

    private void RECON(int ORDSG, boolean REFLN, int NCOLL,int TNSUA, int LOSUB[],
    		int HISUB[], double SOLUN[]) {
        // INTEGER ORDSG,NCOLL,TNSUA
        // INTEGER LOSUB(*),HISUB(*)
        // REAL SOLUN(*)
        // LOGICAL REFLN

        // TO RECONSTITUTE THE FULL SOLUTION VECTOR FOR THE WHOLE BOUNDARY
        // FROM THE SOLUTION ON THE FUNDAMENTAL BOUNDARY SECTION.

        // LOCAL VARIABLES

        int IA,IS,J,J1,J2,J3,LOM,NCFBS,ORDRG,TSFBS;
        double SG;

        if (ORDSG == 1) return;

        TSFBS=TNSUA/ORDSG;
        NCFBS=NCOLL/ORDSG;

        SOLUN[NCOLL]=SOLUN[NCFBS];

        if (REFLN) {
            for (IA=1; IA <= TSFBS; IA++) {
                J1=LOSUB[IA-1];
                J2=HISUB[IA-1];
                LOM=LOSUB[2*TSFBS-IA]-J1;
                SG=-1.0;
                for (J=J1; J <= J2; J++) {
                    SG=-SG;
                    J3=LOM+J;
                    SOLUN[J3-1]=SG*SOLUN[J-1];
                } // for (J=J1; J <= J2; J++)
            } // for (IA=1; IA <= TSFBS; IA++)
            ORDRG=ORDSG/2;
            for (IS=2; IS <= ORDRG; IS++) {
                LOM=(IS-1)*NCFBS*2;
                for (J=1; J <= NCFBS*2; J++) {
                    J1=LOM+J;
                    SOLUN[J1-1]=SOLUN[J-1];
                } // for (J=1; J <= NCFBS*2; J++)
            } // for (IS=2; IS <= ORDRG; IS++)
        } // if (REFLN)
        else {
            for (IS=2; IS <= ORDSG; IS++) {
                LOM=(IS-1)*NCFBS;
                for (J=1; J <= NCFBS; J++) {
                    J1=LOM+J;
                    SOLUN[J1-1]=SOLUN[J-1];
                } // for (J=1; J <= NCFBS; J++)
            } // for (IS=2; IS <= ORDSG; IS++)
        } // else

    } // private void RECON

    private void SETIGL(double AIGLL[], int HISUB[], int JATYP[], int LOSUB[],
    		int NQPTS, double RIGLL[], int TNSUA) {
        // INTEGER NQPTS,TNSUA
        // INTEGER HISUB(*),JATYP(*),LOSUB(*)
        // REAL AIGLL(*),RIGLL(*)

        // **** COPY THE REFERENCE IGNORE LEVELS *RIGLL* INTO THE ACTUAL IGNORE
        // **** IGNORE LEVEL ARRAY *AIGLL*

        // LOCAL VARIABLES

        int AJT,HIM,I,IA,LOD,LOM;

        for (IA=1; IA <= TNSUA; IA++) {
            AJT=Math.abs(JATYP[IA-1]);
            LOD=(AJT-1)*NQPTS+1;
            LOM=LOSUB[IA-1];
            HIM=HISUB[IA-1];
            for (I=LOM; I <= HIM; I++) {
                AIGLL[I-1]=RIGLL[LOD+I-LOM-1];
            } // for (I=LOM; I <= HIM; I++)
        } // for (IA=1; IA <= TNSUA; IA++)

    } // private void SETIGL

    private void AXION1(int AXION[], int NEWDG[], double SOLUN[], int MDGPO,int TNSUA,
        int DGPOL[], int LOSUB[], int HISUB[], double RIGLL[], double LGTOL, boolean ACCPT[],
        double JACIN[], int JATYP[], int NJIND, double NEWHL[], double ESTOL[], int IER[]) {
    	// INTEGER AXION(*),NEWDG(*),MDGPO,TNSUA,DGPOL(*),LOSUB(*),HISUB(*),
    	//+JATYP(*),NJIND,IER
    	//REAL SOLUN(*),RIGLL(*),LGTOL,JACIN(*),NEWHL(*),ESTOL
    	//LOGICAL ACCPT
    	
    	//     TO DETERMINE THE ARRAY "AXION" WHICH SPECIFIES THE ACTIONS THAT 
    	//     ARE TO TAKEN ON EACH SUBARC, AS FOLLOWS:
    	//        AXION(I)=-1 - REDUCE THE DEGREE ON ARC I
    	//        AXION(I)=0  - LEAVE THE DEGREE ON ARC I UNCHANGED
    	//        AXION(I)=1  - INCREASE THE DEGREE ON ARC I
    	//        AXION(I)=2  - SUBDIVIDE ARC I.
    	
    	//     IN CASE ABS(AXION(I))=1, NEWDG(I) RECORDS THE NEW DEGREE TO BE
    	//     USED ON ARC I.
    	
    	//     IN CASE AXION(I)<=0 FOR ALL I=1,TNSUA, THE SOLUTION IS DEEMED TO
    	//     BE ACCEPTABLE TO THE REQUIRED ACCURACY AND WE SET ACCPT=.TRUE.;
    	//     ACCPT=.FALSE. OTHERWISE.
    	
    	//     ALSO TO DETERMINE THE EFFECTIVE STOPPING TOLERANCE ESTOL; IF THE
    	//     USER HAD INPUT ESTOL AS THE VALUE FOR THE ARGUMENT MAXER IN
    	//     JAPHYC, THEN THE CURRENT SOLUTION WOULD BE ACCEPTED.  
    	    
    	//     IER=0  - NORMAL EXIT
    	//     IER=54 - LOCAL PARAMETER MXCO MUST BE INCREASED
    	
    	//     LOCAL VARIABLES
    	
    	final int MINDG = 0;
    	final int MXCO = 32;
    	int IFL[] = new int[1];
    	int AJT,CI,DG,I,IA,J,LOD,LOM,PNDG;
    	final double SAFEF = 5.0;
    	double AA[] = new double[1];
    	double POW[] = new double[1];
    	double EXA[] = new double[1];
    	double CONF[] = new double[1];
    	double BETA,LAM,TERM,THSLN,XX,VAR;
    	double COVAR[][] = new double[2][2];
    	final boolean CONSV = false;
    	// INTEGER CRITCO
    	// EXTERNAL CRITCO,STATS1
    	double POSCO[] = new double[MXCO];
    	
    	ESTOL[0]=0.0;
    	for (IA=1; IA <= TNSUA; IA++) {
    	    DG=DGPOL[IA-1];
    	    if (DG+1 > MXCO) {
    	        IER[0]=54; 
    	        return;
    	    }
    	    LOM=LOSUB[IA-1];
    	    AJT=Math.abs(JATYP[IA-1]);
    	    LOD=(AJT-1)*(MDGPO+1)+1;
    	    for (I=0; I <= DG; I++) {
    	        J=LOM+I;
    	        POSCO[I]=Math.abs(SOLUN[J-1])*RIGLL[LOD+I-1]/LGTOL;
    	    } // for (I= 0; I <= DG; I++)   
    	    CI=CRITCO(DG+1,POSCO)-1;  
    	    PNDG=CI+1+MINDG;
    	    if (CI == -1) {
    	
    	        // ALL COEFFICIENTS ARE ACCEPTABLE.
    	
    	        if (DG == MINDG) {
    	            AXION[IA-1]=0;
    	            NEWDG[IA-1]=DG;
    	        } // if (DG == MINDG)
    	        else {
    	
    	            // PROBABLY DECREASE THE DEGREE, BUT ONLY IGNORE THOSE
    	            // COEFFICIENTS WHICH ARE 'SAFELY' BELOW THE TOLERANCE
    	            // LIMIT.
    	
    	            for (I=0; I <= DG; I++) {
    	                POSCO[I]=POSCO[I]*SAFEF;
    	            } // for (I=0; I <= DG; I++)
    	            PNDG=CRITCO(DG+1,POSCO)+MINDG;
    	            if (PNDG >= DG) {
    	                AXION[IA-1]=0;
    	                NEWDG[IA-1]=DG;
    	            }
    	            else if (PNDG <= MINDG) {
    	                AXION[IA-1]=-1;
    	                NEWDG[IA-1]=MINDG;
    	            }
    	            else {
    	                AXION[IA-1]=-1;
    	                NEWDG[IA-1]=PNDG;
    	            }
    	        } // else
    	    } // if (CI == -1)
    	    else if (PNDG == DG) {
    	        AXION[IA-1]=0;
    	        NEWDG[IA-1]=DG;
    	    } // else if (PNDG == DG)
    	    else if (PNDG < DG) {
    	
    	        // PROBABLY DECREASE THE DEGREE, BUT ONLY IGNORE THOSE
    	        // COEFFICIENTS WHICH ARE 'SAFELY' BELOW THE TOLERANCE
    	        // LIMIT.
    	        for (I=0; I <= DG; I++) {
    	            POSCO[I]=POSCO[I]*SAFEF;
    	        }
    	        PNDG=CRITCO(DG+1,POSCO)+MINDG;
    	        if (PNDG >= DG) {
    	            AXION[IA-1]=0;
    	            NEWDG[IA-1]=DG;
    	        }
    	        else if (PNDG <= MINDG) {
    	            AXION[IA-1]=-1;
    	            NEWDG[IA-1]=MINDG;
    	        }
    	        else {
    	            AXION[IA-1]=-1;
    	            NEWDG[IA-1]=PNDG;
    	        }
    	    } // else if (PNDG < DG)
    	    else if (DG == MDGPO) {
    	
    	        // ARC SUBDIVISION IS REQUIRED AND ASSIGN NEW HALF-LENGTH.
    	
    	        AXION[IA-1]=2;
    	        BETA=JACIN[AJT-1];
    	        LAM=Math.min(1.0,1.0+BETA);
    	        if (AJT != NJIND) {
    	            NEWHL[IA-1]=Math.pow((1.0/POSCO[CI]),(1.0/(1.0+LAM+BETA)));
    	        }
    	        else {
    	            NEWHL[IA-1]=0.5;
    	        }
    	    } // else if (DG == MDGPO)
    	    else {
    	
    	        // MUST DECIDE BETWEEN ARC SUBDIVISION ARC DEGREE INCREASE.
    	        // FIRST MAKE CONSERVATIVE ESTIMATES FOR THE COEFFICIENTS
    	        // FOR DEGREES DG+1 TO MDGPO. 
    	
    	        double SN[] = new double[Math.min(32,DG+1)];
    	        for (I = 0; I < Math.min(32, DG+1); I++) {
    	        	SN[I] = SOLUN[LOM+I-1];
    	        }
    	    	STATS1(SN,DG+1,AA,POW,EXA,COVAR,CONF,IFL);
    	        if (IFL[0] == 1) {
    	
                    // NOT ENOUGH DATA TO MAKE ESTIMATES, WHICH PRESUMES DG
    	            // IS RATHER SMALL; THEREFORE INCREASE THE DEGREE.
    	
    	            AXION[IA-1]=1;
    	            NEWDG[IA-1]=Math.min(DG+2,MDGPO);
    	        } // if (IFL == 1)
    	        else {
    	            for (I=DG+1; I <= MDGPO; I++) {
    	                if (CONSV) {
    	                    XX=Math.log(1.0+I);
    	                    VAR=COVAR[0][0]+XX*XX*COVAR[1][1]+2.0*XX*COVAR[0][1];
    	                    THSLN=EXA[0]*Math.pow((I+1),POW[0])*Math.exp(CONF[0]*Math.sqrt(VAR));
    	                } // if (CONSV)
    	                else {
    	                    THSLN=EXA[0]*Math.pow((I+1),POW[0]);
    	                }
    	                POSCO[I]=Math.abs(THSLN)*RIGLL[LOD+I-1]/LGTOL;
    	            } // for (I=DG+1; I <= MDGPO; I++)
    	            PNDG=CRITCO(MDGPO+1,POSCO)+MINDG;
    	            if (PNDG <= MDGPO) {
    	
    	                // INCREASE DEGREE
    	
    	                AXION[IA-1]=1;
    	                NEWDG[IA-1]=PNDG;
    	            }
    	            else {
    	
    	                // SUBDIVIDE ARC AND ASSIGN NEW HALF-LENGTH.
    	
    	                AXION[IA-1]=2;
    	                BETA=JACIN[AJT-1];
    	                LAM=Math.min(1.0,1.0+BETA);
    	                if (AJT != NJIND) {
    	                    NEWHL[IA-1]=Math.pow((1.0/POSCO[CI]),(1.0/(1.0+LAM+BETA)));
    	                }
    	                else {
    	                    NEWHL[IA-1]= 0.5;
    	                }
    	            }
    	        }
    	    } // else 
    	
    	    // NOW UPDATE THE EFFECTIVE STOPPING TOLERANCE
    	
    	    J=HISUB[IA-1]-MINDG;
    	    for (I=J; I <= HISUB[IA-1]; I++) {
    	        TERM=Math.abs(SOLUN[I-1])*RIGLL[LOD+I-LOM-1];
    	        ESTOL[0]=Math.max(ESTOL[0],Math.exp(TERM)-1.0);
    	    } // for (I=J; I <= HISUB[IA-1]; I++)
    	} // for (IA=1; IA <= TNSUA; IA++)
    	
    	ACCPT[0]=true;
    	for (I=1; I <= TNSUA; I++) {
    	    if (AXION[I-1] > 0) {
    	        ACCPT[0]=false;
    	        IER[0] = 0;
    	        return;
    	    }
    	} // for (I=1; I <= TNSUA; I++)
    	
        IER[0]=0;
    	
    } // private void AXION1
    
    private int CRITCO(int N, double POSCO[]) {
        // INTEGER N
        // REAL POSCO(*)

        // GIVEN THE NON-NEGATIVE NUMBERS POSCO(I), I=1,...,N,  TO FIND THE
        // INDEX CRITCO SUCH THAT POSCO(CRITCO) > 1 AND POSCO(I) <=1 FOR
        // I=CRITCO+1,...,N.  IN CASE POSCO(I) <=1 FOR ALL I=1,...,N THEN
        // POSCO=0.

        // **** LOCAL VARIABLE
      
        int I;

        I=N;
        while (true) {
            if (I == 0) {
                return 0;
            }
            else if (POSCO[I-1] > 1.0) {
            	return I;
            }
            else {
                I=I-1;
                continue;
            }
        } // while true

    } // private int CRITCO

    private void STATS1(double SN[], int M, double A[], double B[], double EA[], double COV[][],
    		double CONF[], int IER[]) {
        // INTEGER M,IER
        // REAL A,B,CONF,EA
        // REAL SN(*),COV(2,2)

        // TO FIND THE LEAST SQUARES ESTIMATES A, B IN THE RELATIONSHIP

        // LOG(SN(I)) = A + B*LOG(I) + ERROR, I=1,..,M, (M<=NDAT)

        // AND ALSO TO GIVE EA=EXP(A) AND THE COVARIANCE MATRIX COV FOR
        // ESTIMATES A,B.

        // IER IS SET TO 1 IF LESS THAN 3 DATA PAIRS ARE COMPATIBLE WITH THE
        // ABOVE MODEL AND NOTHING IS DONE.

        // LOCAL VARIABLES..

        final int NDAT = 32;
    	int I,N,N1;
        double SX1,SX2,SY1,SXY,SE2,DET,DIFF,VAR;
        double X[] = new double[NDAT];
        double Y[] = new double[NDAT];
        double par;

        if (M < 3) {
            IER[0]=1;
            return;
        }

        N1=Math.min(M,NDAT);
        N=N1;

        for (I=1; I <= N1; I++) {
            SY1=Math.abs(SN[I-1]);
            if (SY1 > 0.0) {
                Y[I-1]=Math.log(SY1);
                X[I-1]=Math.log((double)(I));
            }
            else {
                N=N-1;
            }
        } // for (I=1; I <= N1; I++)

        if (N < 3) {
            IER[0]=1;
            return;
        }

        SX1=0.0;
        SX2=0.0;
        SY1=0.0;
        SXY=0.0;
        SE2=0.0;

        for (I=1; I <= N; I++) {
            SX1=SX1+X[I-1];
            SX2=SX2+X[I-1]*X[I-1];
            SY1=SY1+Y[I-1];
            SXY=SXY+X[I-1]*Y[I-1];
        } // for (I=1; I <= N; I++)

        DET=N*SX2-SX1*SX1;
        A[0]=(SX2*SY1-SX1*SXY)/DET;
        EA[0]=Math.exp(A[0]);
        B[0]=(N*SXY-SX1*SY1)/DET;

        for (I=1; I <= N; I++) {
        	par = (Y[I-1]-A[0]-B[0]*X[I-1]);
            SE2=SE2+par*par;
        } // for (I=1; I <= N; I++)

        SE2=SE2/(N-2)/DET;
        COV[0][0]=SX2*SE2;
        COV[0][1]=-SX1*SE2;
        COV[1][0]=COV[0][1];
        COV[1][1]=N*SE2;

        CONF[0]=0.0;
        I=N;
	    while (true) {
	        DIFF=Y[I-1]-A[0]-B[0]*X[I-1];
	        if (DIFF > 0.0) {
	            VAR=COV[0][0]+X[I-1]*X[I-1]*COV[1][1]+2.0*X[I-1]*COV[0][1];
	            CONF[0]=DIFF/Math.sqrt(VAR);
	            break;
	        }
	        else {
	            I=I-1;
	            continue;
	        }
	    } // while (true)
        IER[0]=0;

    } // private void STATS1
    
    private void UPJAC1(int NQPTS,int NJIND,int INDEG,int AXION[],int DGPOL[],
        int NEWDG[],double ACOEF[],double BCOEF[],double DIAG[],double SDIAG[],
        int TNSUA,int MNSUA,int LOSUB[],int HISUB[],int JATYP[],int PARNT[],
        double MIDPT[],double HALEN[],double COLPR[], double ZCOLL[][],
        boolean LNSEG[],boolean PNEWQ[],double EPS,int IER[],double WORK[],
        double NEWHL[],double RCOPY[],int ICOPY[],boolean LCOPY[], int LOOLD[],
    	int HIOLD[]) {
    	//INTEGER NQPTS,INDEG,TNSUA,MNSUA,IER,NJIND
    	//INTEGER DGPOL(*),LOSUB(*),HISUB(*),JATYP(*),PARNT(*),ICOPY(*),
    	//+AXION(*),NEWDG(*),LOOLD(*),HIOLD(*)
    	//REAL EPS,ACOEF(*),BCOEF(*),DIAG(*),SDIAG(*),WORK(*),MIDPT(*),
    	//+HALEN(*),COLPR(*),RCOPY(*),NEWHL(*)
    	//COMPLEX ZCOLL(*)
    	//LOGICAL LNSEG(*),LCOPY(*),PNEWQ(*)
    	
    	// TO UPDATE THE COLLOCATION PARAMETERS (STORED IN COLPR), THE 
    	// COLLOCATION POINTS ON THE PHYSICAL BOUNDARY (STORED IN ZCOLL) 
    	// AND THE ARRAYS LOSUB AND HISUB NEEDED TO ACCESS THIS DATA 
    	// CORRECTLY.  
    	// ALSO TO UPDATE/DETERMINE THE ARRAYS
    	//   JATYP - THE JACOBI INDEX TYPE OF EACH SUBARC
    	//   PARNT - THE PARENT ARC OF EACH SUBARC
    	//   MIDPT - THE GLOBAL PARAMETRIC MIDPOINT OF EACH SUBARC
    	//   HALEN - THE GLOBAL PARAMETRIC HALF-LENGTH OF EACH SUBARC
    	//   DGPOL - THE POLYNOMIAL DEGREE ON EACH SUBARC
    	//   LNSEG - THE LINE SEGMENT BOOLEAN FOR EACH SUBARC
    	//   PNEWQ - BOOLEAN INDICATING POSSIBLE NEW QUADRATURE FOR SUBARC
    	//   IER=0 - NORMAL TERMINATION
    	//   IER=7 - FAILURE IN IMTQLH
    	//   IER=17- NUMBER OF SUBARCS REQUIRED EXCEEDS MNSUA
    	
    	// LOCAL VARIABLES
    	
    	int IFAIL[] = new int[1];
    	int D,D1,FIRST,I,J,K,K1,K2,P,PREV,JT,NTNSA,J1,J2;
    	double S,TC,MD,HH,F1,F2;
    	double PIN[] = new double[2];
    	double POUT[];
    	//COMPLEX PARFUN
    	final boolean USEIN = true;
    	boolean LS;
    	// EXTERNAL PARFUN,IMTQLH
    	
    	J=0;
    	for (I=1; I <= TNSUA; I++) {
    	    MD=MIDPT[I-1];
    	    if (AXION[I-1] < 2) {
    	        J=J+1;
    	        if (J > MNSUA) {
    	            IER[0]=17;
    	            return;
    	        }
    	        RCOPY[J-1]=MD;
    	    } // if (AXION[I-1] < 2)
    	    else { // AXION[I-1] >= 2
    	        JT=JATYP[I-1];
    	        if (JT > 0) {
    	            F1=1.0-NEWHL[I-1];
    	        }
    	        else {
    	            F1=NEWHL[I-1];
    	        }
    	        F2=1.0-F1;
    	        HH=HALEN[I-1];
    	        J=J+1;
    	        if (J > MNSUA) {
    	            IER[0]=17;
    	            return;
    	        }
    	        RCOPY[J-1]=MD-F1*HH;
    	        J=J+1;
    	        if (J > MNSUA) {
    	            IER[0]=17;
    	            return;
    	        }
    	        RCOPY[J-1]=MD+F2*HH;
    	    } // else AXION[I-1] >= 2
    	} // for (I=1; I <= TNSUA; I++)
    	NTNSA=J;
    	for (I=1; I <= NTNSA; I++) {
    	    MIDPT[I-1]=RCOPY[I-1];
    	}
    	     
    	J=0;
    	for (I=1; I <= TNSUA; I++) {
    	    HH=HALEN[I-1];      
    	    if (AXION[I-1] < 2) {
    	        J=J+1;
    	        RCOPY[J-1]=HH;
    	    }
    	    else {
    	        JT=JATYP[I-1];
    	        if (JT > 0) {
    	            F1=NEWHL[I-1];
    	        }
    	        else {
    	            F1=1.0-NEWHL[I-1];
    	        }
    	        F2=1.0-F1;
    	        J=J+1;
    	        RCOPY[J-1]=F1*HH;
    	        J=J+1;
    	        RCOPY[J-1]=F2*HH;
    	    }
    	} // for (I=1; I <= TNSUA; I++)
    	for (I=1; I <= NTNSA; I++) {
    	    HALEN[I-1]=RCOPY[I-1];
    	} // for (I=1; I <= NTNSA; I++)
    	      
    	J=0;
    	for (I=1; I <= TNSUA; I++) {
    	    JT=JATYP[I-1];
    	    if (AXION[I-1] < 2) {
    	        J=J+1;
    	        ICOPY[J-1]=JT;
    	    }
    	    else { 
    	        if (JT < NJIND) {
    	            if (JT > 0) {
    	                J1=JT;
    	                J2=NJIND;
    	            }
    	            else {
    	                J1=NJIND;
    	                J2=JT;
    	            }
    	        }
    	        else {
    	            J1=NJIND;
    	            J2=J1;
    	        }
    	        J=J+1;
    	        ICOPY[J-1]=J1;
    	        J=J+1;
    	        ICOPY[J-1]=J2;
    	    }
    	} // for (I=1; I <= TNSUA; I++)
    	for (I=1; I <= NTNSA; I++) {
    	    JATYP[I-1]=ICOPY[I-1];
    	}
    	
    	J=0;
    	for (I=1; I <= TNSUA; I++) {
    	    if (AXION[I-1] < 2) {
    	        J=J+1;
    	        ICOPY[J-1]=PARNT[I-1];
    	    }
    	    else {
    	        J=J+1;
    	        ICOPY[J-1]=PARNT[I-1];
    	        J=J+1;
    	        ICOPY[J-1]=PARNT[I-1];
    	    }
    	} // for (I=1; I <= TNSUA; I++)
    	for (I=1; I <= NTNSA; I++) {
    	    PARNT[I-1]=ICOPY[I-1];
    	}
    	      
    	J=0;
    	for (I=1; I <= TNSUA; I++) {
    	    LS=LNSEG[I-1];       
    	    if (AXION[I-1] < 2) {
    	        J=J+1;
    	        LCOPY[J-1]=LS;
    	    }
    	    else {
    	        J=J+1;
    	        LCOPY[J-1]=LS;
    	        J=J+1;
    	        LCOPY[J-1]=LS;
    	    }
    	} // for (I=1; I <= TNSUA; I++)
    	for (I=1; I <= NTNSA; I++) {
    	    LNSEG[I-1]=LCOPY[I-1];
    	}
    	  
    	if (USEIN) { 
    	
    	    // USE INDEG ON SUBDIVIDED ARCS
    	   
    	    J=0;
    	    for (I=1; I <= TNSUA; I++) {
    	        if (AXION[I-1] < 2) {
    	            J=J+1;
    	            DGPOL[J-1]=NEWDG[I-1];
    	        }
    	        else {
    	            J=J+1;
    	            DGPOL[J-1]=INDEG;
    	            J=J+1;
    	            DGPOL[J-1]=INDEG;
    	        }
    	    } // for (I=1; I <= TNSUA; I++)
    	} // if (USEIN)
    	else {
    	
    	    // USE CURRENT DEGREE ON SUBDIVIDED ARCS
    	
    	    J=0;
    	    for (I=1; I <= TNSUA; I++) {
    	        if (AXION[I-1] < 2) {
    	            J=J+1;
    	            ICOPY[J-1]=NEWDG[I-1];
    	        }
    	        else {
    	            J=J+1;
    	            ICOPY[J-1]=DGPOL[I-1];
    	            J=J+1;
    	            ICOPY[J-1]=DGPOL[I-1];
    	        }
    	    } // for (I=1; I <= TNSUA; I++) 
    	    for (I=1; I <= NTNSA; I++) {
    	        DGPOL[I-1]=ICOPY[I-1];
    	    }
    	} // else !USEIN
    	
    	J=0;
    	for (I=1; I <= TNSUA; I++) {
    	    if (AXION[I-1] == 2) {
    	        J=J+1;
    	        LOOLD[J-1]=0;
    	        HIOLD[J-1]=-1;     
    	        J=J+1;
    	        LOOLD[J-1]=0;
    	        HIOLD[J-1]=-1;
    	    }
    	    else if (AXION[I-1] == 0) {
    	        J=J+1;
    	        LOOLD[J-1]=LOSUB[I-1];
    	        HIOLD[J-1]=HISUB[I-1];
    	    }
    	    else {     
    	        J=J+1;
    	        LOOLD[J-1]=0;
    	        HIOLD[J-1]=-1;
    	    }
    	} // for (I=1; I <= TNSUA; I++)
    	
    	J=0;
    	for (I=1; I <= TNSUA; I++) {
    	    if (AXION[I-1] < 2) {
    	        J=J+1;
    	        LCOPY[J-1]=true;
    	    }
    	    else {
    	        J=J+1;
    	        LCOPY[J-1]=false;
    	        J=J+1;
    	        LCOPY[J-1]=false;
    	    }
    	} // for (I=1; I <= TNSUA; I++)
    	
    	if (LCOPY[0] && LCOPY[NTNSA-1]) {
    	    if (LCOPY[1]) {
    	        PNEWQ[0]=false;
    	    }
    	    else {
    	        PNEWQ[0]=true;
    	    }
    	    if (LCOPY[NTNSA-2]) {
    	        PNEWQ[NTNSA-1]=false;
    	    }
    	    else {
    	        PNEWQ[NTNSA-1]=true;
    	    }
    	}
    	else {
    	    PNEWQ[0]=true;
    	    PNEWQ[NTNSA-1]=true;
    	}
    	
    	J=NTNSA-1;
    	for (I=2; I <= J; I++) {
    	    if (LCOPY[I-2] && LCOPY[I-1] && LCOPY[I]) {
    	        PNEWQ[I-1]=false;
    	    }
    	    else {
    	        PNEWQ[I-1]=true;
    	    }
    	} // for (I=2; I <= J; I++)
    	
    	TNSUA=NTNSA;
    	LOSUB[0]=1;
    	HISUB[0]=1+DGPOL[0];
    	for (I=2; I <= TNSUA; I++) {
    	    LOSUB[I-1]=HISUB[I-2]+1;
    	    HISUB[I-1]=LOSUB[I-1]+DGPOL[I-1];
    	} // for (I=2; I <= TNSUA; I++)
    	
    	for (I=1; I <= TNSUA; I++) {
    	    J=JATYP[I-1];
    	    P=PARNT[I-1];
    	    D=DGPOL[I-1];
    	    D1=D+1;
    	    if (J > 0) {
    	        S=1.0;
    	    }
    	    else {
    	        S=-1.0;
    	        J=-J;
    	    }
    	    PREV=(J-1)*NQPTS;
    	    FIRST=LOSUB[I-1];
    	    for (K=1; K <= D1; K++) {
    	        WORK[K-1]=0.0;
    	        K1=PREV+K;
    	        DIAG[K-1]=BCOEF[K1-1];
    	        if (K == 1) {
    	            SDIAG[K-1]=0.0;
    	        }
    	        else {
    	            SDIAG[K-1]=ACOEF[K1-2];
    	        }
    	    } // for (K=1; K <= D1; K++) 
    	    WORK[0]=1.0;
    	    IMTQLH(D1,DIAG,SDIAG,IFAIL);
    	    if (IFAIL[0] > 0) {
    	        IER[0]=7;
    	        return;
    	    }
    	    for (K=1; K <= D1; K++) {
    	        TC=S*DIAG[K-1];
    	        K2=FIRST+K-1;
    	        COLPR[K2-1]=TC;
    	        TC=MIDPT[I-1]+HALEN[I-1]*TC;
    	        PIN[0] = TC;
    	        PIN[1] = 0.0;
    	        POUT = PARFUN(P,PIN);
    	        ZCOLL[K2-1][0] = POUT[0];
    	        ZCOLL[K2-1][1] = POUT[1];
    	    } // for (K=1; K <= D1; K++)
    	} // for (I=1; I <= TNSUA; I++)
    	
    	// NORMAL EXIT
    	
        IER[0]=0;
    	
    } // private void UPJAC1
    
    private void UPCOQ1(int NARCS,int NJIND,int NQPTS,int MDGPO,int MQIN1,double AQTOL,
        double QUPTS[], double QUWTS[],double JACIN[],double MIDPT[],double HALEN[],
        double ACOEF[],double BCOEF[],double H0VAL[],double COLSC[],int NQUAD[],
        int LOQSB[],double QCOMX[],double QCOMW[],int MNQUA,double TOLOU[], double MCQER[],
        double XENPT[], double XIVAL[][], double XIDST[],int TNSUA, boolean PNEWQ[],
        boolean NEWQU[],int JATYP[],int PARNT[],boolean NUQTL[],int IER[]) {
    	//INTEGER NARCS,NQPTS,MDGPO,MQIN1,TNSUA,IER,NQUAD(*),LOQSB(*),
    	//+NJIND,JATYP(*),PARNT(*),MNQUA
    	//REAL AQTOL,QUPTS(*),QUWTS(*),JACIN(*),MIDPT(*),HALEN(*),ACOEF(*),
    	//+BCOEF(*),H0VAL(*),COLSC(*),QCOMX(*),QCOMW(*),TOLOU(*),XENPT(*),
    	//+XIDST(*),MCQER
    	//LOGICAL NUQTL
    	//LOGICAL PNEWQ(*),NEWQU(*)
    	//COMPLEX XIVAL(*)
    	
    	//     THE PURPOSE OF THIS ROUTINE IS TO UPDATE THE ABSCISSAE 
    	//     (QCOMX) AND WEIGHTS (QCOMW) FOR THE COMPOSITE GAUSSIAN RULES 
    	//     FOR THE ESTIMATION OF
    	
    	//       INTEGRAL  [(1+X)**BETA*P(X,I)*LOG|ZZ-X|*dX], I=0,1,...,MDGPO.
    	//      -1<=X<=1                                      J=1,NZZ
    	
    	//     HERE P(.,I) IS THE ORTHONORMAL JACOBI POLYNOMIAL OF DEGREE I
    	//     ASSOCIATED WITH THE WEIGHT (1+X)**BETA AND ZZ IS ANY COLLOCATION
    	//     POINT PREIMAGE NOT ON [-1,1].  BETA TAKES ON THE VARIOUS VALUES
    	//     DEFINED BY ARRAY JACIN.  THE ROUTINE ALSO COMPUTES
    	
    	//     NQUAD - NQUAD(I) IS THE NUMBER OF QUADRATURE POINTS IN THE
    	//             COMPOSITE RULE FOR BETA=JACIN(I).
    	//     LOQSB - THE ABSCISSAE AND WEIGHTS OF THE COMPOSITE RULE FOR
    	//             BETA=JACIN(I) ARE STORED IN ARRAYS QCOMX AND QCOMW IN
    	//             THE POSITIONS LOQSB(I) TO LOQSB(I)+NQUAD(I)-1 INCLUSIVE.
    	//     XIDST,
    	//     XIVAL - XIVAL(2*I-1) STORES THE COLLOCATION PREIMAGE THOUGHT
    	//             TO BE NEAREST TO -1 AND XIDST(2*I-1) STORES ITS DISTANCE
    	//             FROM -1; SIMILARLY, XIVAL(2*I) STORES THE PREIMAGE
    	//             THOUGHT TO BE NEAREST TO +1 AND XIDST(2*I) ITS DISTANCE
    	//             FROM +1. THE PREIMAGES ARE WITH RESPECT TO
    	//             THE PARAMETRIC FUNCTIONS DEFINING THE SUBARCS WHICH
    	//             MEET AT THE PHYSICAL CORNER WHERE BETA=JACIN(I).
    	//     TOLOU - TOLOU(I) IS THE ESTIMATED MAXIMUM ERROR OVER ALL
    	//             COLLOCATION POINTS IN USING THE COMPOSITE RULE
    	//             FOR BETA=JACIN(I).
    	//     IER  - IER=0 FOR NORMAL TERMINATION.
    	//            IER=19 THE REQUIRED TOTAL NUMBER OF COMPOSITE QUADRATURE
    	//                   POINTS EXCEEDS THE LIMIT MNQUA.
    	//            IER=20 THE PARAMETER MQIN1 NEEDS INCREASING; MQIN1-1 IS 
    	//                   THE MAXIMUM ALLOWED NUMBER OF SUBINTERVALS IN OUR
    	//                   COMPOSITE GAUSSIAN RULE. (SAME AS IER=11, BUT 20
    	//                   IDICATES DURING REFINEMENT PROCESS)
    	
    	//     ALL THE ABOVE (APART FROM IER) SHOULD HAVE EXISTING VALUES ON 
    	//     INPUT WHICH ARE UPDATED BY THE NEW VERSIONS ON OUTPUT.
    	
    	//     LOCAL VARIABLES
    	
    	int QINTS[] = new int[1];
    	int I,I0,I1,I2,J,K,JI,JI0,JI1,JI2,P0,P1,P2,HI,LO,NQ,DIFF,TNCQP;
    	double DST[] = new double[2];
    	double BETA,H1,M1,T0,T2,SUM1,RR,RRB,MEAN,RXI,IXI;
    	final double ONE[] = new double[]{1.0,0.0};
    	double ZZ[] = new double[2];
    	double Z0[] = new double[2];
    	double Z2[] = new double[2];
    	double XI[][] = new double[2][2];
    	//COMPLEX ONE,ZZ,Z0,Z2,XI(2),PARFUN,DPARFN
    	//PARAMETER (ONE=(1E+0,0E+0))
        //EXTERNAL PARFUN,DPARFN,SUBIN7
    	double PIN[] = new double[2];
    	double POUT[];
    	double DOUT[];
    	double cr[] = new double[1];
    	double ci[] = new double[1];
    	
    	//**** NEWQU(J) IS TRUE IF THE QUADRATURE RULE FOR THE J'TH JACOBI INDEX
    	//**** NEEDS UPDATING.  FIRST SET NEWQU FOR THE CASE WHERE A NEW
    	//**** QUADRATURE TOLERANCE HAS BEEN FIXED; THIS IS INDEPENDENT OF ANY
    	//**** POSSIBLE ARC SUBDIVISIONS.  IF THE PURE LEGENDRE RULE DOESN'T
    	//**** ALREADY EXIST THEN IT DOESN'T HAVE TO BE UPDATED.
    	
    	for (J=1; J <= NARCS; J++) {
    	    NEWQU[J-1]=NUQTL[0];
    	}
    	NEWQU[NJIND-1]=(NUQTL[0] && (NQUAD[NJIND-1] > 0));
    	
    	//**** NEXT OVERWRITE NEWQU TO PICK UP THOSE CASES WHERE A BOUNDARY
    	//**** SUBDIVISION HAS OCCURRED AND UPDATE THE NEAR POINT VECTOR XIVAL.
    	
    	for (I1=1; I1 <= TNSUA; I1++) {
    	    if (PNEWQ[I1-1]) {
    	        if (I1 == 1) {
    	            I0=TNSUA;
    	        }
    	        else { 
    	            I0=I1-1;
    	        }
    	
    	        if (I1 == TNSUA) {
    	            I2=1;
    	        }
    	        else {
    	            I2=I1+1;
    	        }
    	
    	        JI0=JATYP[I0-1];
    	        JI1=JATYP[I1-1];
    	        JI2=JATYP[I2-1];
    	
    	        if (JI0 > 0) {
    	            T0=QUPTS[JI0*NQPTS-1];
    	        }
    	        else {
    	            JI0=-JI0;
    	            T0=-QUPTS[(JI0-1)*NQPTS];
    	        }
    	
    	        if (JI2 > 0) {
    	            T2=QUPTS[(JI2-1)*NQPTS];
    	        }
    	        else {
    	            JI2=-JI2;
    	            T2=-QUPTS[JI2*NQPTS-1];
    	        }
    	
    	        T0=MIDPT[I0-1]+T0*HALEN[I0-1];
    	        T2=MIDPT[I2-1]+T2*HALEN[I2-1];
    	        P0=PARNT[I0-1];
    	        P1=PARNT[I1-1];
    	        P2=PARNT[I2-1];
    	        PIN[0] = T0;
    	        PIN[1] = 0.0;
    	        Z0=PARFUN(P0,PIN);
    	        PIN[0] = T2;
    	        PIN[1] = 0.0;
    	        Z2=PARFUN(P2,PIN);
    	        H1=HALEN[I1-1];
    	        M1=MIDPT[I1-1];
    	        ZZ[0]=M1-H1;
    	        ZZ[1] = 0.0;
    	        POUT = PARFUN(P1,ZZ);
    	        DOUT = DPARFN(P1,ZZ);
    	        zdiv(POUT[0] - Z0[0], POUT[1] - Z0[1],DOUT[0],DOUT[1],cr,ci);
    	        XI[0][0] = -1.0 - cr[0]/H1;
    	        XI[0][1] = -ci[0]/H1;
    	        ZZ[0] = M1+H1;
    	        ZZ[1] = 0.0;
    	        POUT = PARFUN(P1,ZZ);
    	        DOUT = DPARFN(P1,ZZ);
    	        zdiv(POUT[0]-Z2[0], POUT[1]-Z2[1],DOUT[0],DOUT[1],cr,ci);
    	        XI[1][0] = 1.0 - cr[0]/H1;
    	        XI[1][1] = -ci[0]/H1;
    	
    	        if (JI1 < 0) {
    	            Z0[0]=XI[0][0];
    	            Z0[1]=XI[0][1];
    	            XI[0][0]=-XI[1][0];
    	            XI[0][1]=-XI[1][1];
    	            XI[1][0]=-Z0[0];
    	            XI[1][1]=-Z0[1];
    	            JI1=-JI1;
    	        } // if (JI1 < 0)
    	
    	        for (J=1; J <= 2; J++) {
    	            RXI=XI[J-1][0];
    	            IXI=XI[J-1][1];
    	            if (-1.0 <= RXI && RXI <= 1.0) {
    	                DST[J-1]=Math.abs(IXI);
    	            }
    	            else if (RXI < -1.0) {
    	                DST[J-1]=zabs(XI[J-1][0]+1.0,XI[J-1][1]);
    	            }
    	            else {
    	                DST[J-1]=zabs(XI[J-1][0]-1.0,XI[J-1][1]);
    	            }
    	        } // for (J=1; J <= 2; J++)
    	
    	        J=2*JI1-2;
    	        for (I=1; I <= 2; I++) {
    	            J=J+1;
    	            if (DST[I-1] < XIDST[J-1]) {
    	                NEWQU[JI1-1]=true;
    	                XIVAL[J-1][0]=XI[I-1][0];
    	                XIVAL[J-1][1]=XI[I-1][1];
    	                XIDST[J-1]=DST[I-1];
    	            }
    	        } // for (I=1; I <= 2; I++) 
    	    } // if (PNEWQ[I1-1])
    	} // for (I1=1; I1 <= TNSUA; I1++)
    	
    	//     FOR THOSE INDECES FOR WHICH NEWQU IS TRUE WE NOW SET UP THE NEW
    	//     COMPOSITE GAUSSIAN QUADRATURE DATA.
    	
    	TNCQP=LOQSB[NJIND-1]+NQUAD[NJIND-1]-1;
    	HI=0;
    	for (JI=1; JI <= NJIND; JI++) {
    	    NQ=NQUAD[JI-1];
    	    if (NEWQU[JI-1]) {
    	        LO=(JI-1)*NQPTS+1;
    	        BETA=JACIN[JI-1];
    	        I2=2*JI-1;
    	        double ZZin[][] = new double[2][2];
    	        for (K = 0; K < 2; K++) {
    	        	ZZin[K][0] = XIVAL[I2-1+K][0];
    	        	ZZin[K][1] = XIVAL[I2-1+K][1];
    	        }
    	        double AJAC[] = new double[MDGPO];
    	        double BJAC[] = new double[MDGPO];
    	        for (K = 0; K < MDGPO; K++) {
    	        	AJAC[K] = ACOEF[LO-1+K];
    	        	BJAC[K] = BCOEF[LO-1+K];
    	        }
    	        double CSCAL[]= new double[MDGPO+1];
    	        for (K = 0; K < MDGPO+1; K++) {
    	        	CSCAL[K] = COLSC[LO-1+K];
    	        }
    	        double TOLIO[] = new double[1];
    	        TOLIO[0] = TOLOU[JI-1];
    	        SUBIN7(ZZin,2,BETA,MDGPO,NQPTS,AJAC,BJAC,
    	              H0VAL[JI-1],CSCAL,AQTOL,TOLIO,XENPT,QINTS,MQIN1,
    	              IER);
    	        TOLOU[JI-1] = TOLIO[0];
    	        if (IER[0] > 0) {
    	            if (IER[0] == 11) {
    	                IER[0]=20;
    	            }
    	            return;
    	        } // if (IER[0] > 0)
    	
    	        DIFF=QINTS[0]*NQPTS-NQ;
    	        if (TNCQP+DIFF > MNQUA) {
    	            IER[0]=19;
    	            return;
    	        }
    	        I1=HI+NQ+1;
    	 
    	        // IF DIFF IS NON-ZERO WE MUST MAKE SPACE IN ARRAYS QCOMX AND
                // QCOMW TO RECEIVE THE NEW DATA.
    	
    	        if (DIFF > 0) {
    	            for (I=TNCQP; I >= I1; I--) {
    	                J=I+DIFF;
    	                QCOMX[J-1]=QCOMX[I-1];
    	                QCOMW[J-1]=QCOMW[I-1];
    	            } // for (I=TNCQP; I >= I1; I--)
    	        } // if (DIFF > 0)
    	        else if (DIFF < 0) {
    	            for (I=I1; I <= TNCQP; I++) {
    	                J=I+DIFF;
    	                QCOMX[J-1]=QCOMX[I-1];
    	                QCOMW[J-1]=QCOMW[I-1];
    	            } // for (I=I1; I <= TNCQP; I++)
    	        } // else if (DIFF < 0)
    	
    	        // NOW SET UP THE NEW RULE AND STORE DATA IN QCOMX, QCOMW
    	
    	        TNCQP=TNCQP+DIFF;
    	        NQUAD[JI-1]=NQ+DIFF;
    	        LOQSB[JI-1]=HI+1;
    	        SUM1=BETA+1.0;
    	        K=HI;
    	        for (I=1; I <= QINTS[0]; I++) {
    	            RR=(XENPT[I]-XENPT[I-1])*0.5;
    	            MEAN=(XENPT[I]+XENPT[I-1])*0.5; 
    	            if (I == 1) {
    	                RRB=Math.pow(RR,SUM1);
    	                LO=LO-1;
    	                for (J=1; J <= NQPTS; J++) {
    	                    K=K+1;
    	                    QCOMX[K-1]=MEAN+RR*QUPTS[LO+J-1];
    	                    QCOMW[K-1]=RRB*QUWTS[LO+J-1];
    	                } // for (J=1; J <= NQPTS; J++)
    	            } // if (I == 1)
    	            else {
    	                LO=NARCS*NQPTS;
    	                for (J=1; J <= NQPTS; J++) {
    	                    K=K+1;
    	                    QCOMX[K-1]=MEAN+RR*QUPTS[LO+J-1];
    	                    QCOMW[K-1]=RR*QUWTS[LO+J-1]*Math.pow((1.0+QCOMX[K-1]),BETA);
    	                } // for (J=1; J <= NQPTS; J++)
    	            } // else
    	        } // for (I=1; I <= QINTS[0]; I++)
    	        HI=HI+NQUAD[JI-1];
    	    } // if (NEWQU[JI-1])
    	    else {
    	
    	        // HERE WE DO NOTHING OTHER THAN UPDATE SOME SUBSCRIPTS.
    	
    	        LOQSB[JI-1]=HI+1;
    	        HI=HI+NQ;
    	    }
    	
    	} // for (JI=1; JI <= NJIND; JI++)
    	
    	MCQER[0]=0.0;
    	for (I=1; I <= NJIND; I++) {
    	    MCQER[0]=Math.max(MCQER[0],TOLOU[I-1]);
    	}
    	
    	NUQTL[0]=false;
    	
    	//     NORMAL TERMINATION
    	
    	IER[0]=0;
    	
    } // private void UPOCOQ1


    private void TSJAC3(int LOTES[], int HITES[], double TESPR[], double ZTEST[][], int NQPTS,
        int NTEST[], int ORDSG, int TNSUA, int TSTNG, int DGPOL[], int JATYP[], int PARNT[],
        double AICOF[], double BICOF[], double DIAG[], double HALEN[], double JACIN[],
        double MIDPT[], double SDIAG[], int IER[]) {
    	
        // INTEGER IER,NQPTS,NTEST,ORDSG,TNSUA,TSTNG
    	// INTEGER DGPOL(*),HITES(*),JATYP(*),LOTES(*),PARNT(*)
    	// REAL AICOF(*),BICOF(*),DIAG(*),HALEN(*),JACIN(*),MIDPT(*),
    	// +SDIAG(*),TESPR(*)
        // COMPLEX ZTEST(*)
    	
    	// TO ASSIGN THE TEST PARAMETERS (STORED IN TESPR),THE TEST POINTS ON
    	// THE PHYSICAL BOUNDARY (STORED IN ZTEST) AND THE ARRAYS LOTES AND 
    	// HITES NEEDED TO ACCESS THIS DATA CORRECTLY. 
    	
    	// IER=0 - NORMAL EXIT
    	// IER=21 - FAILURE IN IMTQLH
    	
    	// LOCAL VARIABLES
    	
    	int IFAIL[] = new int[1];
    	int D,FIRST,I,J,K,K1,P,PREV,TSFBS;
    	double S,TT;
    	double PIN[] = new double[2];
    	double POUT[];
    	// COMPLEX PARFUN
    	// EXTERNAL ASONJ7,IMTQLH,PARFUN
    	
    	TSFBS=TNSUA/ORDSG;
        if (TSTNG != 1) {
    	    for (I=1; I <= TSFBS; I++) {
    	        TESPR[I-1]=-1.0;
    	        P=PARNT[I-1];
    	        TT=MIDPT[I-1]-HALEN[I-1];
    	        PIN[0] = TT;
    	        PIN[1] = 0.0;
    	        POUT = PARFUN(P,PIN);
    	        ZTEST[I-1][0] = POUT[0];
    	        ZTEST[I-1][1] = POUT[1];
    	        LOTES[I-1]=I;
    	        HITES[I-1]=I;
    	    } // for (I=1; I <= TSFBS; I++) 
        } // if (TSTNG != 1)
        else { // TSTNG == 1           
    	    LOTES[0]=1;
    	    HITES[0]=1+DGPOL[0];
    	    for (I=2; I <= TSFBS; I++) {
    	        LOTES[I-1]=HITES[I-2]+1;
    	        HITES[I-1]=LOTES[I-1]+DGPOL[I-1];
    	    } // for (I=2; I <= TSFBS; I++)
    	
    	    for (I=1; I <= TSFBS; I++) {
    	        D=DGPOL[I-1];
    	        P=PARNT[I-1];
    	        FIRST=LOTES[I-1];
    	        TESPR[FIRST-1]=-1.0;
    	        TT=MIDPT[I-1]-HALEN[I-1];
    	        PIN[0] = TT;
    	        PIN[1] = 0.0;
    	        POUT = PARFUN(P,PIN);
    	        ZTEST[FIRST-1][0] = POUT[0];
    	        ZTEST[FIRST-1][1] = POUT[1];
    	        if (D > 0) {
    	            J=JATYP[I-1];
    	            if (J > 0) {
    	                S=1.0;
    	            }
    	            else {
    	                S=-1.0;
    	                J=-J;
    	            }
    	            PREV=(J-1)*NQPTS;
    	            for (K=1; K <= D; K++) {
    	                K1=PREV+K;
    	                DIAG[K-1]=BICOF[K1-1];
    	                if (K == 1) {
    	                    SDIAG[K-1]=0.0;
    	                }
    	                else {
    	                    SDIAG[K-1]=AICOF[K1-2];
    	                }
    	            } // for (K=1; K <= D; K++)
    	            IMTQLH(D,DIAG,SDIAG,IFAIL);
    	            if (IFAIL[0] > 0) {
    	                IER[0]=21;
    	                return;
    	            }
    	            for (K=1; K <= D; K++) {
    	                TT=S*DIAG[K-1];
    	                K1=FIRST+K;
    	                TESPR[K1-1]=TT;
    	                TT=MIDPT[I-1]+HALEN[I-1]*TT;
    	                PIN[0] = TT;
    	                PIN[1] = 0.0;
    	                POUT = PARFUN(P,PIN);
    	                ZTEST[K1-1][0] = POUT[0];
    	                ZTEST[K1-1][1] = POUT[1];
    	            } // for (K=1; K <= D; K++)
    	        } // if (D > 0)
    	    } // for (I=1; I <= TSFBS; I++) 
        } // else TSTNG == 1
    	NTEST[0]=HITES[TSFBS-1];
    	if (ORDSG > 1) {
    	    NTEST[0]=NTEST[0]+1;
    	    LOTES[TSFBS]=NTEST[0];
    	    HITES[TSFBS]=NTEST[0];
    	    TESPR[NTEST[0]-1]=-1.0;
    	    P=PARNT[TSFBS];
    	    PIN[0] = -1.0;
    	    PIN[1] = 0.0;
    	    POUT = PARFUN(P,PIN);
    	    ZTEST[NTEST[0]-1][0] = POUT[0];
    	    ZTEST[NTEST[0]-1][1] = POUT[1];
    	}
    	
    	IER[0]=0;
    	
    } // private void TSJAC3 
    
    private void TESMD9(double ERMOD[], double MATRX[][], double SOLUN[], int MNEQN, int NCOLL,
        int NTEST, int NQPTS, int TNSUA, int JATYP[], int PARNT[], int DGPOL[], int LOSUB[],
        int HISUB[], int LOTES[], int HITES[], int NQUAD[], int LOQSB[], double TOLNR,
        double MIDPT[], double HALEN[], double H0VAL[], double COLSC[], double ACOEF[], 
        double BCOEF[], double TESPR[], double QCOMX[], double QCOMW[], double CENTR[], 
        double ZTEST[][], boolean INTER, boolean LNSEG[], double WORK[], int QIERR[],
        double MQERR[], double JACIN[], double A1COF[], double B1COF[], double AQTOL,
        double RQTOL, double AQCOF[], double BQCOF[], double CQCOF[], double MXERM[],
        int IMXER[], double ZMXER[], double ERARC[], int ORDSG, boolean REFLN) {
    	
    	//INTEGER MNEQN,NCOLL,NTEST,NQPTS,TNSUA,JATYP(*),PARNT(*),DGPOL(*),
    	//+LOSUB(*),HISUB(*),LOTES(*),HITES(*),NQUAD(*),LOQSB(*),QIERR(0:6),
    	//+IMXER,ORDSG
    	
    	//REAL MATRX(MNEQN,*),SOLUN(*),TOLNR,MIDPT(*),HALEN(*),H0VAL(*),
    	//+COLSC(*),ACOEF(*),BCOEF(*),TESPR(*),QCOMX(*),QCOMW(*),MQERR,
    	//+WORK(*),A1COF(*),B1COF(*),AQTOL,JACIN(*),ERARC(*),
    	//+RQTOL,AQCOF(*),BQCOF(*),CQCOF(*),ERMOD(*),MXERM
    	
    	//COMPLEX CENTR,ZMXER,ZTEST(*)
    	
    	//LOGICAL INTER,LNSEG(*),REFLN
    	
    	// TO COMPUTE THE ERROR IN MODULUS AT THE VECTOR OF TEST POINTS
    	// ZTEST (PARAMETER VALUES IN TESPR) STORING RESULTS IN ERMOD.
    	
        // LOCAL VARIABLES
    	
    	int IER[] = new int[1];
    	int I,J,I1,J1,IA,IQ,JI,AJI,PT,DG,LOCLM,HICLM,LOQ,LOD,NQ,IT,
    	    NVAL,FIRST,LAST,NEQNS,ROW,TSFBS,ORDRG,IB;
    	double MD,HL,RH,TQ,WQ,DD,LDD,SG,SJI,TT,C0,U0,U1,CURR,PREV,NEXT,ABER0,ABER1,RLIM,LLIM,SUM;
    	// REAL FNVAL
    	double GTQ[] = new double[2];
    	double ZQ[] = new double[2];
    	double ZT[] = new double[2];
    	// COMPLEX GTQ,ZQ,ZT,PARFUN,DPARFN
    	// COMMON /FNDEF/BETA,A1,B1,P0VAL,SCALE,TYPE
    	// EXTERNAL DPARFN,FNVAL,JAPAR7,PARFUN,QAWS,R1MACH
    	double PIN[] = new double[2];
    	double POUT[];
    	qaws qmod;
    	int routine = Integration2.DQAWSE;
    	/**
         * Gives an upper bound on the number of subintervals in the partition of lower, upper.
         */
        int limit = 100;
    	
    	TSFBS=TNSUA/ORDSG;
    	NEQNS=NCOLL+1;
    	RLIM=1.0-5.0*EPS;
    	LLIM=-RLIM;
    	for (J=1; J <= NCOLL; J++) {
    	    for (I=1; I <= NTEST; I++) {
    	        MATRX[I-1][J-1]=0.0;
    	    }
    	} // for (J=1; J <= NCOLL; J++)
    	for (I=0; I <= 6; I++) {
    	    QIERR[I]=0;
    	}
    	MQERR[0]=0.0;
    	
    	// NOW SELECT THE INTEGRATION ARC
    	
    	for (IA=1; IA <= TNSUA; IA++) {

    	
    	    // INITIALISE DATA FOR THIS ARC
    	
    	    PT=PARNT[IA-1];
    	    MD=MIDPT[IA-1];
    	    HL=HALEN[IA-1];
    	    DG=DGPOL[IA-1];
    	    LOCLM=LOSUB[IA-1];
    	    HICLM=HISUB[IA-1];
    	    if (ORDSG == 1 || (ORDSG > 1 && IA <= (TSFBS+1))) {
    	        FIRST=LOTES[IA-1];
    	        LAST=HITES[IA-1]+1;
    	    }
    	    else if (IA == TNSUA && ORDSG > 1) {
    	        FIRST=NTEST+1;
    	        LAST=FIRST;
    	    }
    	    else {
    	        FIRST=NTEST+1;
    	        LAST=NTEST;
    	    }
    	    JI=JATYP[IA-1];
    	    if (JI < 0) {
    	        SJI=-1.0;
    	    }
    	    else {
    	        SJI=1.0;
    	    }
    	    AJI=Math.abs(JI);
    	    LOD=(AJI-1)*NQPTS+1;
    	    RH=Math.sqrt(H0VAL[AJI-1]);
    	    BETA=JACIN[AJI-1];
    	    A1=A1COF[AJI-1];
    	    B1=B1COF[AJI-1];
    	    P0VAL=1.0/RH;
    	
    	    // SET UP THE DIAGONAL BLOCK OF SCALED PRINCIPAL SINGULAR INTEGRALS
    	
    	    for (IT=FIRST; IT <= LAST; IT++) {
    	        if (ORDSG > 1 && IA == (TSFBS+1) && IT == LAST) continue;
    	        if (IT == LAST) {
    	            TT=SJI;
    	        }
    	        else {
    	            TT=SJI*TESPR[IT-1];
    	        }
    	
    	        if (IT > NTEST) {
    	            ROW=1;
    	        }
    	        else {
    	            ROW=IT;
    	        }
    	
    	        SCALE=COLSC[LOD-1];
    	        if (TT < LLIM) {
    	            U0=0.0;
    	            ABER0=0.0;
    	            TYPE=1;
    	            // Use dqawse which is the same as (quadpack/dqaws) but provides more
    	            // information and control
    	            qmod = new qaws(-1.0,1.0,routine,BETA,0.0,3,AQTOL,RQTOL,limit);
    	            qmod.driver();
    	            U1 = qmod.getIntegral();
    	            ABER1 = qmod.getAbserr();
    	            NVAL = qmod.getNeval();
    	            IER[0] = qmod.getErrorStatus();
    	            QIERR[IER[0]]=QIERR[IER[0]]+1;
    	        }
    	        else if (TT >= RLIM) {
    	            TYPE=1;
    	            qmod = new qaws(-1.0,1.0,routine,BETA,0.0,3,AQTOL,RQTOL,limit);
    	            qmod.driver();
    	            U0 = qmod.getIntegral();
    	            ABER0 = qmod.getAbserr();
    	            NVAL = qmod.getNeval();
    	            IER[0] = qmod.getErrorStatus();
    	            QIERR[IER[0]]=QIERR[IER[0]]+1;
    	            U1=0.0;
    	            ABER1=0.0;
    	        }
    	        else {
    	            TYPE=1;
    	            qmod = new qaws(-1.0,TT,routine,BETA,0.0,3,AQTOL,RQTOL,limit);
    	            qmod.driver();
    	            U0 = qmod.getIntegral();
    	            ABER0 = qmod.getAbserr();
    	            NVAL = qmod.getNeval();
    	            IER[0] = qmod.getErrorStatus();
    	            QIERR[IER[0]]=QIERR[IER[0]]+1;
    	            TYPE=2;
    	            qmod = new qaws(TT,1.0,routine,0.0,0.0,2,AQTOL,RQTOL,limit);
    	            qmod.driver();
    	            U1 = qmod.getIntegral();
    	            ABER1 = qmod.getAbserr();
    	            NVAL = qmod.getNeval();
    	            IER[0] = qmod.getErrorStatus();
    	            QIERR[IER[0]]=QIERR[IER[0]]+1;
    	        }
    	        WORK[0]=U0+U1;
    	        MQERR[0]=Math.max(MQERR[0],ABER0+ABER1);
    	
    	        if (DG > 0) {
    	            SCALE=COLSC[LOD];
    	            if (TT < LLIM) {
    	                U0=0.0;
    	                ABER0=0.0;
    	                TYPE=3;
    	                qmod = new qaws(-1.0,1.0,routine,BETA,0.0,2,AQTOL,RQTOL,limit);
        	            qmod.driver();
        	            U1 = qmod.getIntegral();
        	            ABER1 = qmod.getAbserr();
        	            NVAL = qmod.getNeval();
        	            IER[0] = qmod.getErrorStatus();
        	            QIERR[IER[0]]=QIERR[IER[0]]+1;
    	            }
    	            else if (TT > RLIM) {
    	                TYPE=3;
    	                qmod = new qaws(-1.0,1.0,routine,BETA,0.0,3,AQTOL,RQTOL,limit);
        	            qmod.driver();
        	            U0 = qmod.getIntegral();
        	            ABER0 = qmod.getAbserr();
        	            NVAL = qmod.getNeval();
        	            IER[0] = qmod.getErrorStatus();
        	            QIERR[IER[0]]=QIERR[IER[0]]+1;
    	                U1=0.0;
    	                ABER1=0.0;
    	            }
    	            else {
    	                TYPE=3;
    	                qmod = new qaws(-1.0,TT,routine,BETA,0.0,3,AQTOL,RQTOL,limit);
        	            qmod.driver();
        	            U0 = qmod.getIntegral();
        	            ABER0 = qmod.getAbserr();
        	            NVAL = qmod.getNeval();
        	            IER[0] = qmod.getErrorStatus();
        	            QIERR[IER[0]]=QIERR[IER[0]]+1;
    	                TYPE=4;
    	                qmod = new qaws(TT,1.0,routine,0.0,0.0,2,AQTOL,RQTOL,limit);
        	            qmod.driver();
        	            U1 = qmod.getIntegral();
        	            ABER1 = qmod.getAbserr();
        	            NVAL = qmod.getNeval();
        	            IER[0] = qmod.getErrorStatus();
        	            QIERR[IER[0]]=QIERR[IER[0]]+1;
    	            }
    	            WORK[1]=U0+U1;
    	            MQERR[0]=Math.max(MQERR[0],ABER0+ABER1);
    	
    	            // NOW USE THE (WEAKLY) STABLE FORWARD RECURRENCE SCHEME FOR 
    	            // WORK(I),I=3,NQPTS (WITH SCALE FACTOR FOR WORK(2))
    	
    	            CURR=WORK[1];
    	            PREV=SCALE;
    	            for (I=1; I <= DG-1; I++) {
    	                J=LOD+I-1;
    	                NEXT=(AQCOF[J-1]*TT-BQCOF[J-1])*CURR-CQCOF[J-1]*PREV;
    	                WORK[I+1]=NEXT;
    	                PREV=CURR;
    	                CURR=NEXT;
    	            } // for (I=1; I <= DG-1; I++)
    	
    	            // ASSIGN CORRECT SCALE FACTORS.
    	
    	            for (I=3; I <= DG+1; I++) {
    	                J=LOD+I-1;
    	                WORK[I-1]=WORK[I-1]*COLSC[J-1]/SCALE;
    	            } // for (I=3; I <= DG+1; I++)
    	        } // if (DG > 0)
    	
    	        SG=1.0;
    	        for (J=LOCLM; J <= HICLM; J++) {
    	            MATRX[ROW-1][J-1]=MATRX[ROW-1][J-1]+SG*WORK[J-LOCLM];
    	            SG=SG*SJI;
    	        } // for (J=LOCLM; J <= HICLM; J++)
    	
    	    } // for (IT=FIRST; IT <= LAST; IT++)
    	
    	    // INITIALISE SOME DATA FOR THE NON-SINGULAR INTEGRALS
    	
    	    WORK[0]=1.0/RH;
    	    NQ=NQUAD[AJI-1];
    	    LOQ=LOQSB[AJI-1];
    	    if (IA == TNSUA) {
    	        I1=2;
    	    }
    	    else {
    	        I1=1;
    	    }
    	
    	    for (IQ=1; IQ <= NQ; IQ++) {
    	        I=LOQ+IQ-1;
    	        TQ=QCOMX[I-1];
    	        WQ=QCOMW[I-1];
    	        double AA[] = new double[DG];
    	        double BB[] = new double[DG];
    	        for (I = 0; I < DG; I++) {
    	        	AA[I] = ACOEF[LOD+I-1];
    	        	BB[I] = BCOEF[LOD+I-1];
    	        }
    	        JAPAR7(WORK,TQ,AA,BB,DG);
    	        if (JI < 0) {
    	            TQ=-TQ;
    	        }
    	        GTQ[0] = MD+HL*TQ;
    	        GTQ[1] = 0.0;
    	        ZQ=PARFUN(PT,GTQ);
    	
    	        // ACCUMULATE THE ELEMENTS ABOVE THE DIAGONAL BLOCK
    	
    	
    	        for (IT=I1; IT <= FIRST-1; IT++) {
    	            ZT[0]=ZTEST[IT-1][0];
    	            ZT[1]=ZTEST[IT-1][1];
    	            DD=zabs(ZT[0]-ZQ[0],ZT[1]-ZQ[1]);
    	            LDD=Math.log(DD)*WQ;
    	            SG=1.0;
    	            for (J1=LOCLM; J1 <= HICLM; J1++) {
    	                J=J1-LOCLM+1;
    	                I=J1-LOCLM+LOD;
    	                MATRX[IT-1][J1-1]=MATRX[IT-1][J1-1]+SG*WORK[J-1]*LDD*COLSC[I-1];
    	                SG=SG*SJI;
    	            } // for (J1=LOCLM; J1 <= HICLM; J1++)
    	        } // for (IT=I1; IT <= FIRST-1; IT++)
    	
    	        // ACCUMULATE THE ELEMENTS BELOW THE DIAGONAL BLOCK
    	
    	        for (IT=LAST+1; IT <= NTEST; IT++) {
    	            ZT[0]=ZTEST[IT-1][0];
    	            ZT[1]=ZTEST[IT-1][1];
    	            DD=zabs(ZT[0]-ZQ[0],ZT[1]-ZQ[1]);
    	            LDD=Math.log(DD)*WQ;
    	            SG=1.0;
    	            for (J1=LOCLM; J1 <= HICLM; J1++) {
    	                J=J1-LOCLM+1;
    	                I=J1-LOCLM+LOD;
    	                MATRX[IT-1][J1-1]=MATRX[IT-1][J1-1]+SG*WORK[J-1]*LDD*COLSC[I-1];
    	                SG=SG*SJI;
    	            } // for (J1=LOCLM; J1 <= HICLM; J1++)
    	        } // for (IT=LAST+1; IT <= NTEST; IT++)
    	
                // ACCUMULATE THE RESIDUAL NON-SINGULAR CONTRIBUTIONS INTO
    	        // THE DIAGONAL BLOCK FOR THE NON-LINE-SEGMENT CASE.
    	
    	        if (!LNSEG[IA-1]) {
    	            for (IT=FIRST; IT <= LAST; IT++) {
    	                if (ORDSG > 1 && IA == (TSFBS+1) && IT == LAST) { 
    	                    continue;
    	                }
    	                if (IT == LAST) {
    	                    TT=1.0;
    	                }
    	                else {
    	                    TT=TESPR[IT-1];
    	                }
    	
    	                if (IT > NTEST) {
    	                    ROW=1;
    	                }
    	                else {
    	                    ROW=IT;
    	                }
    	
    	                DD=Math.abs(TT-TQ);
    	                if (DD <= TOLNR) {
    	                	POUT = DPARFN(PT,GTQ);
    	                    DD=zabs(POUT[0],POUT[1])*HL;
    	                }
    	                else {
    	                    if (IT > NTEST) {
    	                        ZT[0]=ZTEST[0][0];
    	                        ZT[1]=ZTEST[0][1];
    	                    }
    	                    else {
    	                        ZT[0]=ZTEST[IT-1][0];
    	                        ZT[1]=ZTEST[IT-1][1];
    	                    }
    	                    DD=zabs(ZT[0]-ZQ[0],ZT[1]-ZQ[1])/DD;
    	                    if (DD < TOLNR) {
    	                    	POUT = DPARFN(PT,GTQ);
    	                    	DD=zabs(POUT[0],POUT[1])*HL;
    	                    }
    	                }
    	                LDD=Math.log(DD)*WQ;
    	                SG=1.0;
    	                for (J1=LOCLM; J1 <= HICLM; J1++) {
    	                    J=J1-LOCLM+1;
    	                    I=J1-LOCLM+LOD;
    	                    MATRX[ROW-1][J1-1]=MATRX[ROW-1][J1-1]+SG*WORK[J-1]*LDD*COLSC[I-1];
    	                    SG=SG*SJI;
    	                } // for (J1=LOCLM; J1 <= HICLM; J1++)
    	            } // for (IT=FIRST; IT <= LAST; IT++)
    	        } // if (!LNSEG[IA-1])
    	    } // for (IQ=1; IQ <= NQ; IQ++)
    	
    	    // ACCUMULATE THE RESIDUAL NON-SINGULAR CONTRIBUTIONS INTO
    	    // THE DIAGONAL BLOCK FOR THE LINE-SEGMENT CASE.
    	
    	    if (LNSEG[IA-1]) {
    	    	PIN[0] = MD;
    	    	PIN[1] = 0.0;
    	    	POUT = DPARFN(PT,PIN);
    	    	ZT[0] = POUT[0]*HL;
    	    	ZT[1] = POUT[1]*HL;
    	        C0=zabs(ZT[0],ZT[1]);
    	        C0=RH*Math.log(C0)*COLSC[LOD-1];
    	        for (IT=FIRST; IT <= LAST; IT++) {
    	            if (ORDSG > 1 && IA == (TSFBS+1) && IT == LAST) {
    	                continue;
    	            }
    	            if (IT > NTEST) {
    	                ROW=1;
    	            }
    	            else {
    	                ROW=IT;
    	            }
    	            MATRX[ROW-1][LOCLM-1]=MATRX[ROW-1][LOCLM-1]+C0;
    	        } // for (IT=FIRST; IT <= LAST; IT++)
    	    } // if (LNSEG[IA-1])
    	} // for (IA=1; IA <= TNSUA; IA++)
    	
    	// SET UP THE LAST COLUMN
    	
    	for (I=1; I <= NTEST; I++) {
    	    MATRX[I-1][NEQNS-1]=1.0;
    	}
    	
    	// COMPUTE MATRIX-VECTOR PRODUCT
    	
    	for (I=1; I <= NTEST; I++) {
    	    SUM=0.0;
    	    for (J=1; J <= NEQNS; J++) {
    	          SUM=SUM+MATRX[I-1][J-1]*SOLUN[J-1];
    	    }
    	    ERMOD[I-1]=SUM;
    	} // for (I=1; I <= NTEST; I++)
    	
    	// FORM THE ERROR IN MODULUS
    	
    	if (INTER) {
    	    for (I=1; I <= NTEST; I++) {
    	        SUM=Math.exp(ERMOD[I-1]);
    	        ERMOD[I-1]=Math.abs(1.0-zabs(ZTEST[I-1][0]-CENTR[0],ZTEST[I-1][1]-CENTR[1])/SUM);
    	    }
    	}
    	else {
    	    for (I=1; I <= NTEST; I++) {
    	        SUM=Math.exp(ERMOD[I-1]);
    	        ERMOD[I-1]=Math.abs(1.0-SUM);
    	    }
    	}
    	
    	// FIND MAXIMUM ERROR IN MODULUS AND THE POINT AND THE ARC AT WHICH 
    	// IT OCCURS
    	
    	MXERM[0]=0.0;
    	    for (IA=1; IA <= TSFBS; IA++) {
    	        FIRST=LOTES[IA-1];
    	        LAST=HITES[IA-1];
    	        MD=0.0;
    	        for (IT=FIRST; IT <= LAST; IT++) {
    	            if (ERMOD[IT-1] > MD) {
    	                MD=ERMOD[IT-1];
    	            }
    	            if (MD > MXERM[0]) {
    	                MXERM[0]=MD;
    	                IMXER[0]=IA;
    	                ZMXER[0]=ZTEST[IT-1][0];
    	                ZMXER[1]=ZTEST[IT-1][1];
    	            }
    	        } // for (IT=FIRST; IT <= LAST; IT++)
    	        if (IA == TSFBS && ORDSG == 1) {
    	            IT=1;
    	        }
    	        else {
    	            IT=LAST+1;
    	        }
    	        if (ERMOD[IT-1] > MD) {
    	            MD=ERMOD[IT-1];
    	        }
    	        if (MD > MXERM[0]) {
    	            MXERM[0]=MD;
    	            IMXER[0]=IA;
    	            ZMXER[0]=ZTEST[IT-1][0];
    	            ZMXER[1]=ZTEST[IT-1][1];
    	        } // if (MD > MXERM[0])
    	        ERARC[IA-1]=MD;
    	    } // for (IA=1; IA <= TSFBS; IA++)
    	
    	    // IF REGION IS SYMMETRIC, FILL UP THE WHOLE ERARC VECTOR USING
    	    // SYMMETRY
    	
    	    if (ORDSG > 1) {
    	        if (REFLN) {
    	            for (IA=1; IA <= TSFBS; IA++) {
    	                IB=2*TSFBS+1-IA;
    	                ERARC[IB-1]=ERARC[IA-1];
    	            } // for (IA=1; IA <= TSFBS; IA++)
    	            ORDRG=ORDSG/2;
    	            for (I=2; I <= ORDRG; I++) {
    	                I1=(I-1)*TSFBS*2;
    	                for (IA=1; IA <= TSFBS*2; IA++) {
    	                    IB=I1+IA;
    	                    ERARC[IB-1]=ERARC[IA-1];
    	                } // for (IA=1; IA <= TSFBS*2; IA++)
    	            } //  for (I=2; I <= ORDRG; I++)
    	        } // if (REFLN)
    	        else {
    	            for (I=2; I <= ORDSG; I++) {
    	                I1=(I-1)*TSFBS;
    	                for (IA=1; IA <= TSFBS; IA++) {
    	                    IB=I1+IA;
    	                    ERARC[IB-1]=ERARC[IA-1];
    	                } // for (IA=1; IA <= TSFBS; IA++)
    	            } // for (I=2; I <= ORDSG; I++)
    	        } // else
    	    } // if (ORDSG > 1)

    } // private void TESMD9
    
    private void RSLT83(double ERARC[], int TNSUA, double MXERM, double ZMXER[], int IMXER,
        int QIERC[], int PARNT[], int ORDSG) {
    	// INTEGER TNSUA,IMXER,ORDSG,OC
    	// INTEGER PARNT(*),QIERC(0:6)
    	// REAL MXERM,ERARC(*)
    	// COMPLEX ZMXER
    	
    	// LOCAL VARIABLES
    	
    	int I,TSFBS;
    	String QTEXT[] = new String[7];
        //CHARACTER QTEXT(0:6)*22,LINE*72
    	final String LINE="_________________________________________________________________";
    	
    	QTEXT[0]="...........NORMAL EXIT";
    	QTEXT[1]=".....MAX. SUBDIVISIONS";
    	QTEXT[2]="....ROUNDOFF DETECTION";
    	QTEXT[3]=".........BAD INTEGRAND";
    	QTEXT[6]=".........INVALID INPUT";
    	
    	Preferences.debug("\n",Preferences.DEBUG_ALGORITHM);
    	Preferences.debug(LINE+"\n",Preferences.DEBUG_ALGORITHM);
    	Preferences.debug("RESULTS FROM ERROR IN MODULUS TESTS",Preferences.DEBUG_ALGORITHM);
    	Preferences.debug("\n",Preferences.DEBUG_ALGORITHM);
    	Preferences.debug("QAWS TERMINATIONS WITH......\n",Preferences.DEBUG_ALGORITHM); 
    	for (I=0; I <= 6; I++) {
    	    if (QIERC[I] > 0) {
    	        Preferences.debug(QTEXT[I]+ " " + QIERC[I] + "\n",Preferences.DEBUG_ALGORITHM); 
    	    }
    	} // for (I=0; I <= 6; I++)
    	
    	Preferences.debug("\n",Preferences.DEBUG_ALGORITHM);
    	Preferences.debug("SUB ARC  PARENT ARC     MAX ERROR IN MODULUS\n",Preferences.DEBUG_ALGORITHM); 
    	TSFBS=TNSUA/ORDSG;
    	for (I=1; I <= TSFBS; I++) {
    		Preferences.debug(" " + I + "       " + PARNT[I-1] + "          " + ERARC[I-1] + "\n",Preferences.DEBUG_ALGORITHM);
    	}
    	
    	Preferences.debug("MAXIMUM ERROR IN MODULUS IS " + MXERM + "\n",Preferences.DEBUG_ALGORITHM);
    	Preferences.debug("THIS OCCURS AT ("+ZMXER[0]+","+ZMXER[1]+") ON ARC " + IMXER + "\n",Preferences.DEBUG_ALGORITHM);
    	
    } // private void RSLT83


    private void RSLT84(double ERMOD[], int TNSUA, double MXERM, double ZMXER[], int IMXER,
        int LOTES[], int HITES[], int QIERC[], int PARNT[], int ORDSG) {
    	// INTEGER TNSUA,IMXER,ORDSG,OC
    	// INTEGER LOTES(*),HITES(*),PARNT(*),QIERC(0:6)
    	// REAL MXERM,ERMOD(*)
    	// COMPLEX ZMXER
    	
    	// LOCAL VARIABLES
    	 
    	int I,J,L,H,TSFBS;
    	double S;
        String QTEXT[] = new String[7];
        //CHARACTER QTEXT(0:6)*22,LINE*72
    	final String LINE="_________________________________________________________________";
    	
    	QTEXT[0]="...........NORMAL EXIT";
    	QTEXT[1]=".....MAX. SUBDIVISIONS";
    	QTEXT[2]="....ROUNDOFF DETECTION";
    	QTEXT[3]=".........BAD INTEGRAND";
    	QTEXT[6]=".........INVALID INPUT";
    	
    	Preferences.debug("\n",Preferences.DEBUG_ALGORITHM);
    	Preferences.debug(LINE+"\n",Preferences.DEBUG_ALGORITHM);
    	Preferences.debug("RESULTS FROM ERROR IN MODULUS TESTS",Preferences.DEBUG_ALGORITHM);
    	Preferences.debug("\n",Preferences.DEBUG_ALGORITHM);
    	Preferences.debug("QAWS TERMINATIONS WITH......\n",Preferences.DEBUG_ALGORITHM); 
    	for (I=0; I <= 6; I++) {
    	    if (QIERC[I] > 0) {
    	        Preferences.debug(QTEXT[I]+ " " + QIERC[I] + "\n",Preferences.DEBUG_ALGORITHM); 
    	    }
    	} // for (I=0; I <= 6; I++)
    	
    	Preferences.debug("\n",Preferences.DEBUG_ALGORITHM);
    	Preferences.debug("ERROR IN MODULI AT TEST POINTS\n",Preferences.DEBUG_ALGORITHM);
    	TSFBS=TNSUA/ORDSG;
    	for (I=1; I <= TSFBS; I++) {
    	    Preferences.debug("SUB ARC = " + I + " ON PARENT ARC " + PARNT[I-1] + "\n",Preferences.DEBUG_ALGORITHM);
    	    Preferences.debug("N         ERROR IN MOD\n",Preferences.DEBUG_ALGORITHM);
    	    L=LOTES[I-1];
    	    H=HITES[I-1];
    	    for (J=L; J <= H; J++) {
    	        S=ERMOD[J-1];
    	        Preferences.debug(J + "   " + S + "\n",Preferences.DEBUG_ALGORITHM);
    	    } // for (J=L; J <= H; J++)
    	    if (I == TSFBS && ORDSG == 1) {
    	        J=1;
    	    }
    	    else {
    	        J=H+1;
    	    }
    	    S=ERMOD[J-1];
    	    Preferences.debug(J + "   " + S + "\n",Preferences.DEBUG_ALGORITHM);
    	} // for (I=1; I <= TSFBS; I++)
    	
    	Preferences.debug("MAXIMUM ERROR IN MODULUS IS " + MXERM + "\n",Preferences.DEBUG_ALGORITHM);
    	Preferences.debug("THIS OCCURS AT ("+ZMXER[0]+","+ZMXER[1]+") ON ARC " + IMXER + "\n",Preferences.DEBUG_ALGORITHM);
    	
    } // private void RSLT84
    
    private void RESCAL(int NQPTS,int TNSUA, int LOSUB[], int HISUB[], int JATYP[],
    		double SOLUN[], double COLSC[]) {
        // INTEGER NQPTS,TNSUA
        // INTEGER LOSUB(*),HISUB(*),JATYP(*)
        // REAL SOLUN(*),COLSC(*)

        // TO RESCALE THE SOLUTION VECTOR SOLUN SO AS TO OBTAIN THE 
        // STANDARD JACOBI COEFFICIENTS.

        // LOCAL VARIABLES

        int H,I,J,J1,JT,L,LOD;

        for (I=1; I <= TNSUA; I++) {
            L=LOSUB[I-1];
            H=HISUB[I-1];
            JT=Math.abs(JATYP[I-1]);
            LOD=(JT-1)*NQPTS+1;
            for (J=L; J <= H; J++) {
                J1=LOD+J-L;
                SOLUN[J-1]=SOLUN[J-1]*COLSC[J1-1];
            } // for (J=L; J <= H; J++)
        } // for (I=1; I <= TNSUA; I++)

    } // private void RESCAL

    private void GQPHYC(int MQIN1, int IER[]) {
    	
    	// INTEGER MQIN1,MQUPH,CHNL,IER
    	// INTEGER IQUPH(*),IGEOM(*),ISNPH(*)
    	// REAL RQUPH(*),RGEOM(*),RSNPH(*),RWORK(*)
    	// COMPLEX CENTR
    	// COMPLEX ZQUPH(*)
    	// LOGICAL INTER
    	
    	// ......................................................................
    	//
    	// 1.     GQPHYC
    	//           COMPUTES A GLOBAL QUADRATURE RULE FOR APPROXIMATING THE
    	//           BOUNDARY INTEGRAL REPRESENTATION OF THE MAP : PHYSICAL -->
    	//           CANONICAL.
    	
    	
    	// 2.     PURPOSE
    	//           THE ROUTINE SETS UP THE BOUNDARY QUADRATURE POINTS AND
    	//           CORRESPONDING WEIGHTS FOR A COMPOSITE GAUSS-JACOBI/GAUSS-
    	//           LEGENDRE RULE FOR ESTIMATING THE BOUNDARY INTEGRAL THAT
    	//           APPEARS IN THE RESPRESENTATION FOR THE CONFORMAL MAP OF THE
    	//           PHYSICAL DOMAIN ONTO THE CANONICAL DOMAIN.  THIS QUADRATURE
    	//           RULE IS USED IN THE STANDARD NON-SINGULAR CASE WHEN THE
    	//           FIELD POINT IN THE PHYSICAL DOMAIN DOES NOT LIE CLOSE TO THE
    	//           BOUNDARY. 
    	           
    	
    	// 3.     CALLING SEQUENCE
    	//           CALL GQPHYC(MQIN1,MQUPH,INTER,CENTR,IGEOM,RGEOM,ISNPH,RSNPH,
    	//                       RWORK,CHNL,IQUPH,RQUPH,ZQUPH,IER)
    	
    	//        PARAMETERS
    	//         ON ENTRY
    	//            MQIN1  - INTEGER
    	//                     DEFINES THE NUMBER OF PANELS ALLOWED IN A 
    	//                     COMPOSITE RULE.  SPECIFICALLY, MQIN1 = 1 + (THE
    	//                     MAXIMUM NUMBER OF PANELS IN A COMPOSITE RULE FOR
    	//                     A SINGLE SUB-ARC ON THE BOUNDARY)
    	
    	//            MQUPH  - INTEGER
    	//                     THE MAXIMUM NUMBER OF QUADRATURE POINTS ALLOWED
    	//                     IN THE FINAL GLOBAL RULE.  (THE VALUE OF THIS
    	//                     ARGUMENT IS LINKED TO THOSE OF ARGUMENTS NQPTS
    	//                     AND IBNDS(1) PREVIOUSLY SUPPLIED TO JAPHYC VIA
    	//                     MQUPH <= (MQIN1-1)*NQPTS*IBNDS(1))
    	
    	//            INTER  - LOGICAL
    	//                     TRUE IF THE PHYSICAL DOMAIN IS INTERIOR, FALSE 
    	//                     OTHERWISE.
    	
    	//            CENTR  - COMPLEX
    	//                     THE POINT IN THE PHYSICAL PLANE THAT IS TO BE
    	//                     MAPPED TO THE CENTRE OF THE UNIT DISC.  FOR
    	//                     EXTERIOR DOMAINS CENTR MUST BE SOME POINT IN THE
    	//                     COMPLEMENTARY INTERIOR  PHYSICAL DOMAIN.
    	
    	//            IGEOM  - INTEGER ARRAY
    	//                     THE INTEGER VECTOR IGEOM PREVIOUSLY SET UP BY 
    	//                     JAPHYC.
    	
    	//            RGEOM  - REAL ARRAY
    	//                     THE REAL VECTOR RGEOM PREVIOUSLY SET UP BY JAPHYC.
    	
    	//            ISNPH  - INTEGER ARRAY
    	//                     THE INTEGER VECTOR ISNPH PREVIOUSLY SET UP BY 
    	//                     JAPHYC.
    	
    	//            RSNPH  - REAL ARRAY
    	//                     THE REAL VECTOR RSNPH PREVIOUSLY SET UP BY JAPHYC.
    	
    	//            RWORK  - REAL ARRAY
    	//                     A WORKING VECTOR OF SIZE AT LEAST MQIN1.
    	
    	//             CHNL  - INTEGER
    	//                     DEFINES AN OUTPUT CHANNEL THAT MAY BE USED FOR
    	//                     WRITING THE FILE <JBNM>pq.
    	
    	//         ON EXIT
    	//            IQUPH  - INTEGER ARRAY
    	//                     AN INTEGER VECTOR OF SIZE AT LEAST 2*IBNDS(1) + 4,
    	//                     WHERE IBNDS(1) (=IGEOM(4)) IS THE VALUE PREVIOUSLY
    	//                     SUPPLIED TO JAPHYC; IQUPH STORES POINTERS TO 
    	//                     ACCESS RQUPH AND ZQUPH.
    	
    	//            RQUPH  - REAL ARRAY
    	//                     A REAL VECTOR OF SIZE AT LEAST 3*MQUPH + 1; STORES
    	//                     THE REAL QUADRATURE DATA.
    	
    	//            ZQUPH  - COMPLEX ARRAY
    	//                     A COMPLEX VECTOR OF SIZE AT LEAST MQUPH + 1; 
    	//                     STORES THE QUADRATURE POINTS ON THE PHYSICAL 
    	//                     BOUNDARY.
    	                     
    	//            IER    - INTEGER
    	//                     IF IER > 0 THEN AN ABNORMAL EXIT HAS OCCURRED;
    	//                     A MESSAGE TO DESCRIBE THE ERROR IS AUTOMATICALLY
    	//                     WRITTEN ON THE STANDARD OUTPUT CHANNEL.
    	//                     IER=0 - NORMAL EXIT.
    	//                     IER>0 - ABNORMAL EXIT; THE ERROR MESSAGE SHOULD
    	//                             BE SELF EXPLANATORY.
    	
    	                    
    	// 4.     SUBROUTINES OR FUNCTIONS NEEDED
    	//              - THE CONFPACK LIBRARY.
    	//              - THE REAL FUNCTION R1MACH.
    	//              - THE USER SUPPLIED COMPLEX FUNCTIONS PARFUN AND DPARFN.
    	
    	
    	// 5.     FURTHER COMMENTS
    	//           - NOTE THAT THIS ROUTINE CAN ONLY BE USED  A F T E R  THE  
    	//             ROUTINE JAPHYC HAS SUCCESSFULLY EXECUTED, AND THAT SOME  
    	//             INPUT ARGUMENTS FOR GQPHYC ARE OUTPUT VALUES FROM JAPHYC.
    	//           - THE GLOBAL QUADRATURE DATA ARE AUTOMATICALLY OUTPUT ON THE
    	//             FILE <JBNM>pq, WHERE <JBNM> IS COLLECTED FROM THE FILE 
    	//             jbnm PREVIOUSLY CREATED BY JAPHYC.
    	//           - A SUMMARY LISTING OF ACTIONS TAKEN IS AUTOMATICALLY
    	//             WRITTEN ON THE STANDARD OUTPUT CHANNEL.
    	
    	// ......................................................................
    	//     AUTHOR: DAVID HOUGH, ETH, ZUERICH
    	//     LAST UPDATE: 3 JULY 1990
    	// ......................................................................
    	     
    	//     LOCAL VARAIBLES
    	
    	final int NINTS = 5;
    	int ACOEF,BCOEF,DGPOL,FACTR,H0VAL,HALEN,JACIN,JATYP,LOSUB,
    	     LQSBF,MIDPT,NPPQF,PARNT,QUPTS,QUWTS,SOLUN,TPPQF,TRRAD,VTARG,WPPQF,
    	     ZPPQF;
    	int MNEQN,MNSUA,NARCS,NEQNS,NJIND,NQPTS,TNGQP,TNPQP,
    	     TNSUA;
    	final double DLETA = 0.2;
    	double LGTOL,SUPER,THET0,TOLOU;
    	double CT[] = new double[2];
    	//COMPLEX CT
    	
        // EXTERNAL POPQF1,OUPTPQ,WRHEAD,WRTAIL
    	
    	// WRITE HEADING TO STANDARD OUTPUT CHANNEL
    	
    	WRHEAD(2,0,null);
    	
    	NARCS=ISNPH[0];
    	NQPTS=ISNPH[1];
    	TNSUA=ISNPH[2];
    	NEQNS=ISNPH[3];
    	MNSUA=ISNPH[4];
    	MNEQN=ISNPH[5];
    	NJIND=NARCS+1;
    	TNGQP=NJIND*NQPTS;
    	SUPER=RGEOM[0];
    	LGTOL=RGEOM[1];
    	
    	IQUPH[1]=TNSUA;
    	IQUPH[2]=MNSUA;
    	IQUPH[3]=MQUPH;
    	
    	// COPY POINTERS FROM JAPHYC
    	
    	// SET UP POINTERS FOR QUADRATURE DATA.
    	
    	
    	/*RQUPH[0]=SOLUN[NEQNS-1];
    	
    	System.out.println("QUADRATURE RULES STARTED:");
    	POPQF1(NPPQF,LQSBF,TNPQP,TOLOU,TPPQF,
    	     TRRAD,WPPQF,ZPPQF,MQUPH,MQIN1,NARCS,NINTS,
    	     NQPTS,TNSUA,DGPOL,JATYP,LOSUB,PARNT,
    	     DELTA,LGTOL,ACOEF,BCOEF,H0VAL,HALEN,
    	     JACIN,MIDPT,QUPTS,QUWTS,SOLUN,
    	     RWORK,IER);
    	      WRITE(*,10) 'QUADRATURE RULES DONE:'
    	C
    	      IF (IER .GT. 0) THEN
    	        GOTO 999
    	      ENDIF
    	C
    	      IQUPH(1)=TNPQP
    	C
    	C**** SET UP THE CONSTANT FACTOR FOR THE MAPPING FORMULA
    	C
    	      THET0=RGEOM(VTARG)
    	      IF (INTER) THEN
    	        ZQUPH(FACTR)=CMPLX(COS(THET0),SIN(THET0))
    	      ELSE
    	        ZQUPH(FACTR)=(1E+0,0E+0)
    	        CALL DMPHYC(1,CENTR,CT,INTER,CENTR,IGEOM,RGEOM,ISNPH,RSNPH,
    	     +              IQUPH,RQUPH,ZQUPH,.TRUE.,IER)
    	        IF (IER .GT. 0) THEN
    	          GOTO 999
    	        ENDIF
    	        CT=CMPLX(RQUPH(1),THET0-AIMAG(LOG(CT)))
    	        ZQUPH(FACTR)=CEXP(CT)
    	      ENDIF
    	C
    	      CALL OUPTPQ(IQUPH,RQUPH,ZQUPH,CHNL)
    	999   CONTINUE
    	C
    	C**** WRITE CLOSING MESSAGE TO STANDARD OUTPUT CHANNEL
    	C
    	      CALL WRTAIL(2,0,IER)
    	C*/
    } // private void GQPHYC

    private void POPQF1(int NPPQF[], int LPQSB[], int TNPQP[], double TOLOU, double TPPQF[],
        double TRRAD[], double WPPQF[], double ZPPQF[][], int MNQUA, int MQIN1, int NARCS,
        int NINTS, int NQPTS,int TNSUA, int DGPOL[], int JATYP[], int LOSUB[], int PARNT[],
    	double DELTA, double LGTOL, double ACOEF[], double BCOEF[], double H0VAL[], double HALEN[],
    	double JACIN[], double MIDPT[], double QUPTS[], double QUWTS[], double SOLUN[],
        double XENPT[], int IER[]) {
    	// INTEGER IER,MQIN1,MNQUA,NARCS,NINTS,NQPTS,TNPQP,TNSUA
    	// INTEGER DGPOL(*),JATYP(*),LOSUB(*),LPQSB(*),NPPQF(*),
    	// +PARNT(*)
    	// REAL DELTA,LGTOL,TOLOU
    	// REAL ACOEF(*),BCOEF(*),HALEN(*),H0VAL(*),JACIN(*),MIDPT(*),
    	// +QUPTS(*),QUWTS(*),SOLUN(*),TPPQF(*),TRRAD(*),WPPQF(*),XENPT(*)
    	// COMPLEX ZPPQF(*)
    	
    	//     THE MAIN PURPOSE OF THIS ROUTINE IS TO SET UP THE REAL ARRAY
    	//     WPPQF OF QUADRATURE WEIGHTS, THE COMPLEX ARRAY ZPPQF OF
    	//     QUADRATURE POINTS ON THE PHYSICAL BOUNDARY AND THE REAL ARRAY
    	//     TPPQF OF LOCAL PARAMETER VALUES CORRESPONDING TO ZPPQF; ALL THESE
    	//     DATA DEFINE THE POST-PROCESSING COMPOSITE GAUSSIAN
    	//     QUADRATURE RULES FOR THE ESTIMATE OF THE MAP F (PHYSICAL ONTO
    	//     CANONICAL) WHEN Z IS NOT TOO CLOSE TO THE BOUNDARY.
    	
    	//     THIS ROUTINE ALSO RETURNS THE ARRAY TRRAD OF SO-CALLED TRANSITION
    	//     RADII;  THE IDEA IS THAT IF A PHYSICAL FIELD POINT Z SATISFIES
    	
    	//                 ABS(Z-ZPPQF(I)) < TRRAD(I)
    	
    	//     THEN THE QUADRATURE RULE PRODUCED BY THIS ROUTINE IS PROBABLY
    	//     NOT SUFFICIENTLY ACCURATE ON THE PARTICULAR ARC ON WHICH
    	//     ZPPQF(I) LIES.  THIS ROUTINE ALSO ASSIGNS FICTICIOUS QUADRATURE 
    	//     POINTS WITH WEIGHTS SET TO ZERO.  THESE POINTS ARE INSERTED TO 
    	//     ENSURE THAT IF A FIELD POINT Z IS CLOSE TO THE PHYSICAL BOUNDARY
    	//     THEN THE ABOVE INEQUALITY SHOULD BE SATISFIED FOR AT LEAST ONE
    	//     "GENERALISED" QUADRATURE POINT.
    	
    	//     THE ARRAY ELEMENT NPPQF(I) RECORDS THE NUMBER OF QUADRATURE
    	//     POINTS FOR THE SUBARC NUMBER I, I=1,...,TNSUA.  THE WEIGHTS,
    	//     QUADRATURE POINTS AND LOCAL QUADRATURE POINT PARAMETERS FOR ARC 
    	//     NUMBER I ARE STORED IN WPPQF, ZPPQF AND TPPQF STARTING AT THE 
    	//     ELEMENT WITH INDEX LPQSB(I).
    	
    	//     IER=0 - NORMAL EXIT 
    	//     IER=22- THE TOTAL NUMBER OF QUADRATURE POINTS REQUESTED FOR 
    	//             THE WHOLE BOUNDARY EXCEEDS THE LIMIT DEFINED BY THE
    	//             INPUT PARAMETER MNQUA; MNQUA MUST BE INCREASED IN A
    	//             HIGHER LEVEL ROUTINE.
    	//     IER=23- THE LOCAL PARAMETER MNCOF SHOULD BE INCREASED
    	
    	//     LOCAL VARIABLES
    	
    	final int MNCOF = 32;
    	int AJT,DEG,HI,HI1,I,I1,J,J1,J2,JT,K,LIM,LOD,LOL,LOM,PT,QINTS;
    	final double RHO = 0.13;
    	double BETA,DIST,HL,JACSUM,MD,MEAN,MXDIS,RR,RRB,SS,SUM1,TT,SCO;
    	double CTT[] = new double[2];
    	// COMPLEX CTT,DPARFN,PARFUN
    	double JACOF[] = new double[MNCOF];
    	double JCOFC[][] = new double[MNCOF][2];
    	// COMPLEX JCOFC(MNCOF)
    	// EXTERNAL DPARFN,JACSUM,PARFUN,PPSBI7
    	
    	MXDIS=DELTA;
    	HI=0;
    	LOL=NARCS*NQPTS;
    	/*for (I1=1; I1 <= TNSUA; I1++) {
    	    JT=JATYP[I1-1];
    	    if (JT > 0) {
    	        SS=1.0;
    	    }
    	    else {
    	        SS=-1.0;
    	    }
    	    AJT=Math.abs(JT);
    	    BETA=JACIN[AJT-1];
    	    DEG=DGPOL[I1-1];
    	    if (DEG+1 > MNCOF) {
    	        IER[0]=23;
    	        return;
    	    }
    	    LOM=LOSUB[I1-1];
    	    LOD=(AJT-1)*NQPTS+1;
    	    PT=PARNT[I1-1];
    	    HL=HALEN[I1-1];
    	    MD=MIDPT[I1-1];
    	
    	    SCO=SS;
    	    for (J=1; J <= DEG+1; J++) {
    	        J1=LOM+J-1;
    	        SCO=SCO*SS;
    	        JACOF[J-1]=SOLUN[J1-1]*SCO;
    	        JCOFC[J-1][0]=SOLUN[J1-1]*SCO;
    	        JCOFC[J-1][1] = 0.0;
    	    } // for (J=1; J <= DEG+1; J++)
    	
    	    PPSBI7(DELTA,NINTS,BETA,NQPTS,DEG,ACOEF(LOD),
    	     BCOEF(LOD),H0VAL(AJT),JCOFC,LGTOL,TOLOU,XENPT,
    	     QINTS,MQIN1,IER);
    	        IF (IER .GT. 0) THEN
    	          RETURN
    	        ENDIF
    	        NPPQF(I1)=QINTS*NQPTS
    	        LPQSB(I1)=HI+1
    	        HI1=HI+NPPQF(I1)
    	        IF (HI1 .GT. MNQUA) THEN
    	          IER=22
    	          RETURN
    	        ENDIF
    	        K=HI
    	        SUM1=BETA+1E+0
    	        DO 30 I=1,QINTS
    	          RR=(XENPT(I+1)-XENPT(I))*5E-1
    	          MEAN=(XENPT(I+1)+XENPT(I))*5E-1
    	          IF (I .EQ. 1) THEN
    	            RRB=RR**SUM1
    	            DO 10 J=1,NQPTS
    	              J1=LOD+J-1
    	              K=K+1
    	              TT=(MEAN+RR*QUPTS(J1))
    	              WPPQF(K)=RRB*QUWTS(J1)*JACSUM(TT,DEG,ACOEF(LOD),
    	     +                 BCOEF(LOD),H0VAL(AJT),JACOF)
    	              TT=TT*SS
    	              TPPQF(K)=TT
    	              CTT=CMPLX(MD+TT*HL)
    	              ZPPQF(K)=PARFUN(PT,CTT)
    	              TRRAD(K)=HL*DELTA*ABS(DPARFN(PT,CTT))
    	10          CONTINUE
    	          ELSE
    	            DO 20 J=1,NQPTS
    	              J1=LOL+J
    	              K=K+1
    	              TT=(MEAN+RR*QUPTS(J1))
    	              WPPQF(K)=RR*QUWTS(J1)*(1E+0+TT)**BETA*JACSUM(TT,DEG,
    	     +                 ACOEF(LOD),BCOEF(LOD),H0VAL(AJT),JACOF)
    	              TT=TT*SS
    	              TPPQF(K)=TT
    	              CTT=CMPLX(MD+TT*HL)
    	              ZPPQF(K)=PARFUN(PT,CTT)
    	              TRRAD(K)=HL*DELTA*ABS(DPARFN(PT,CTT))
    	20          CONTINUE
    	          ENDIF
    	30      CONTINUE
    	        IF (SS .LT. 0E+0) THEN
    	          LIM=NPPQF(I1)
    	          IF (MOD(LIM,2) .EQ. 0) THEN
    	            LIM=LIM/2
    	          ELSE
    	            LIM=(LIM-1)/2
    	          ENDIF
    	          J1=LPQSB(I1)-1
    	          J2=HI1+1
    	          DO 40 J=1,LIM
    	            J1=J1+1
    	            J2=J2-1
    	            TT=WPPQF(J1)
    	            WPPQF(J1)=WPPQF(J2)
    	            WPPQF(J2)=TT
    	            TT=TRRAD(J1)
    	            TRRAD(J1)=TRRAD(J2)
    	            TRRAD(J2)=TT
    	            TT=TPPQF(J1)
    	            TPPQF(J1)=TPPQF(J2)
    	            TPPQF(J2)=TT
    	            CTT=ZPPQF(J1)
    	            ZPPQF(J1)=ZPPQF(J2)
    	            ZPPQF(J2)=CTT
    	40        CONTINUE
    	        ENDIF
    	C
    	C       NEXT WE INSERT ANY NECESSARY FICTICIOUS QUADRATURE POINTS
    	C
    	        J1=LPQSB(I1)
    	        IF (TPPQF(J1)+1E+0 .GT. RHO*MXDIS) THEN
    	          J2=HI1
    	          DO 50 I=J2,J1,-1
    	            WPPQF(I+1)=WPPQF(I)
    	            TRRAD(I+1)=TRRAD(I)
    	            TPPQF(I+1)=TPPQF(I)
    	            ZPPQF(I+1)=ZPPQF(I)
    	50        CONTINUE
    	          HI1=J2+1
    	          NPPQF(I1)=NPPQF(I1)+1
    	          WPPQF(J1)=0E+0
    	          TPPQF(J1)=-1E+0
    	          CTT=CMPLX(MD-HL)
    	          ZPPQF(J1)=PARFUN(PT,CTT)
    	          TRRAD(J1)=HL*DELTA*ABS(DPARFN(PT,CTT))
    	        ENDIF
    	C
    	60      CONTINUE
    	        J1=J1+1
    	        IF (J1 .LT. HI1) THEN
    	          DIST=TPPQF(J1+1)-TPPQF(J1)
    	          IF (DIST .GT. MXDIS) THEN
    	            J2=HI1
    	            DO 70 I=J2,J1+1,-1
    	              WPPQF(I+1)=WPPQF(I)
    	              TRRAD(I+1)=TRRAD(I)
    	              TPPQF(I+1)=TPPQF(I)
    	              ZPPQF(I+1)=ZPPQF(I)
    	70          CONTINUE
    	            HI1=J2+1
    	            NPPQF(I1)=NPPQF(I1)+1
    	            WPPQF(J1+1)=0E+0
    	            TPPQF(J1+1)=(TPPQF(J1)+TPPQF(J1+2))*5E-1
    	            CTT=CMPLX(MD+HL*TPPQF(J1+1))
    	            ZPPQF(J1+1)=PARFUN(PT,CTT)
    	            TRRAD(J1+1)=HL*DELTA*ABS(DPARFN(PT,CTT))
    	          ENDIF
    	          GOTO 60
    	        ELSE IF (1E+0-TPPQF(J1) .GT. RHO*MXDIS) THEN
    	          J1=J1+1
    	          HI1=J1
    	          NPPQF(I1)=NPPQF(I1)+1
    	          WPPQF(J1)=0E+0
    	          TPPQF(J1)=1E+0
    	          CTT=CMPLX(MD+HL)
    	          ZPPQF(J1)=PARFUN(PT,CTT)
    	          TRRAD(J1)=HL*DELTA*ABS(DPARFN(PT,CTT))
    	        ENDIF
    	        HI=HI1
    	} // for (I1=1; I1 <= TNSUA; I1++)*/
    	
    	TNPQP[0]=HI;
    	
    	IER[0]=0;
    	
    } // private void POPQF1
    
    private void PPSBI7(double DELTA, int NINTS, double BETA, int NQUAD, int DGPOL, double ACOEF[],
        double BCOEF[], double H0VAL, double SOLUN[][], double TOLIN, double TOLOU, double XENPT[],
        int QINTS[], int MQIN1, int IER[]) {
    	// INTEGER DGPOL,MQIN1,NQUAD,QINTS,IER,NINTS
    	// REAL BETA,H0VAL,TOLIN,TOLOU,DELTA
    	// REAL ACOEF(*),BCOEF(*),XENPT(*)
    	// COMPLEX SOLUN(*)
    	
    	//     CALCULATES THE NUMBER OF QUADRATURE INTERVALS (QINTS) REQUIRED
    	//     FOR THE COMPOSITE GAUSS-JACOBI/GAUSS-LEGENDRE ESTIMATION OF
    	
    	//          INTEGRAL  [(1+X)**BETA*FNPHI(X)*LOG(ZZ(J)-X)*dX], 
    	//         -1<=X<=1                                         
    	
    	//     WHERE FNPHI IS A POLYNOMIAL APPROXIMATION TO THE BOUNDARY 
    	//     CORRESPONDENCE DERIVATIVE / JACOBI WEIGHT QUOTIENT. 
    	 
    	//     ZZ IS ANY POINT ON A "DELTA-CONTOUR" IN THE UPPER HALF PLANE,
    	//     THIS CONTOUR BEING DEFINED BY THE PARAMETER DELTA.  TEST VALUES 
    	//     FOR ZZ ARE ASSIGNED IN THE SUBROUTINES DEPPJ8 AND DEPPL8.
    	
    	//     THE PARAMETERS DGPOL,ACOEF,BCOEF,H0VAL AND SOLUN ARE USED TO 
    	//     DEFINE FNPHI AND ARE PASSED TO DEPPJ8 AND DEPPL8 FOR THIS PURPOSE.
    	
    	//     THE ENDPOINTS OF THE QUADRATURE INTERVALS ARE RETURNED IN VECTOR 
    	//     XENPT, WITH XENPT(1)=-1<XENPT(2)<...<1=XENPT(QINTS+1).
    	
    	//     TOLOU RECORDS OUR ESTIMATE FOR THE MAXIMUM OF THE ABSOLUTE VALUES
    	//     OF THE REMAINDER ESTIMATES ON THE DELTA-CONTOUR.  WE REQUIRE THAT
    	
    	//                  TOLOU <= TOLIN
    	
    	//     WITH THE TOLOU BEING REASONABLY CLOSE TO TOLIN.
    	
    	//     IER=0 - NORMAL EXIT
    	//     IER=24- THE REQUIRED NUMBER OF QUADRATURE INTERVALS EXCEEDS THAT
    	//             SPECIFIED BY THE GLOBAL PARAMETER MQIN1; MQIN1 MUST BE
    	//             INCREASED (ERROR NUMBER 43 IF CALLED FROM POPQG1)
    	   
    	//     LOCAL VARIABLES
    	
    	int INTS;
    	double TAU[] = new double[1];
    	double MAXRM[] = new double[1];
    	double TOL,RIGHT;
    	boolean T1FXD;
    	// EXTERNAL DEPPJ8,DEPPL8
    	
    	QINTS[0]=1;
    	XENPT[0]=-1.0;
    	TOL=TOLIN;
    	INTS=NINTS;
    	DEPPJ8(BETA,TAU,NQUAD,DGPOL,ACOEF,BCOEF,H0VAL,
    	     SOLUN,TOL,MAXRM,INTS,DELTA,IER);
    	/*      IF (IER .GT. 0) THEN
    	        RETURN
    	      ENDIF
    	      TOLOU=MAXRM
    	      XENPT(2)=TAU
    	C
    	      IF (XENPT(2) .LT. 1E+0) THEN
    	        QINTS=2
    	        T1FXD=.FALSE.
    	        TAU=1E+0
    	        RIGHT=-1E+0
    	        INTS=NINTS
    	        CALL DEPPL8(BETA,RIGHT,TAU,T1FXD,NQUAD,DGPOL,ACOEF,
    	     +              BCOEF,H0VAL,SOLUN,TOL,MAXRM,INTS,DELTA,IER)
    	        IF (IER .GT. 0) THEN
    	          RETURN
    	        ENDIF
    	        TOLOU=TOLOU+MAXRM
    	        T1FXD=.TRUE.
    	C
    	100     CONTINUE
    	C
    	        IF (XENPT(QINTS) .GT. RIGHT) THEN
    	          XENPT(QINTS)=5E-1*(XENPT(QINTS)+RIGHT)
    	          XENPT(QINTS+1)=1E+0
    	        ELSE
    	          TAU=1E+0
    	          INTS=NINTS
    	          CALL DEPPL8(BETA,XENPT(QINTS),TAU,T1FXD,NQUAD,DGPOL,
    	     +                ACOEF,BCOEF,H0VAL,SOLUN,TOL,MAXRM,INTS,DELTA,IER) 
    	          IF (IER .GT. 0) THEN
    	            RETURN
    	          ENDIF
    	          TOLOU=TOLOU+MAXRM
    	          QINTS=QINTS+1
    	          IF (QINTS .GE. MQIN1) THEN
    	            IER=24
    	            RETURN
    	          ENDIF
    	          XENPT(QINTS)=TAU
    	          GOTO 100
    	        ENDIF
    	      ENDIF
    	C
    	      IER=0
    	C */
    } // private void PPSBI7
    
    private void DEPPJ8(double BETA, double TAU[], int NQUAD, int DGPOL, double ACOEF[],
        double BCOEF[], double H0VAL, double SOLUN[][], double TOL, double MAXRM[], int NINTS,
        double DELTA,int IER[]) {
        //INTEGER NQUAD,IER,DGPOL,NINTS
    	//REAL BETA,TAU,TOL,MAXRM,ACOEF(*),BCOEF(*),H0VAL,DELTA
    	//COMPLEX SOLUN(*)
    	
    	// WE COMPUTE THE DONALDSON-ELLIOTT ESTIMATES FOR THE REMAINDERS IN
    	//     USING AN NQUAD-POINT GAUSS-JACOBI RULE TO ESTIMATE THE INTEGRALS
    	
    	//            INTEGRAL  [(1+X)**BETA*FNPHI(X)*LOG(ZZ-X)*dX]
    	//           -1<=X<=TAU                                       
    	
    	//     WHERE FNPHI IS A POLYNOMIAL APPROXIMATION TO THE BOUNDARY 
    	//     CORRESPONDENCE DERIVATIVE / JACOBI WEIGHT QUOTIENT. 
    	 
    	//     ZZ IS ANY POINT ON A "DELTA-CONTOUR" IN THE UPPER HALF PLANE,
    	//     THIS CONTOUR BEING DEFINED BY THE PARAMETE DELTA.  TEST VALUES 
    	//     FOR ZZ ARE ASSIGNED IN THE BODY OF THE ROUTINE AND THE LOCAL ARRAY
    	//     XIVAL IS USED FOR STORING THESE TEST VALUES.
    	
    	//     THE PARAMETERS DGPOL,ACOEF,BCOEF,H0VAL AND SOLUN ARE USED TO 
    	//     DEFINE FNPHI AND ARE PASSED TO DEPPJ9 FOR THIS PURPOSE.
    	
    	//     MAXRM RECORDS THE MAXIMUM OF THE ABSOLUTE VALUES OF THE REMAINDER
    	//     ESTIMATES ASSOCIATED WITH THE DELTA-CONTOUR.
    	
    	//     ON ENTRY NINTS IS THE NUMBER OF INITIAL TEST INTERVALS TO BE USED
    	//     ON THE UPPER HALF DELTA-CONTOUR; ON EXIT NINTS GIVES THE FINAL
    	//     NUMBEROF INTERVALS REQUIRED FOR CONVERGENCE TO TAU.
    	
    	//     THE PURPOSE OF THIS ROUTINE IS TO DETERMINE A VALUE FOR TAU
    	//     SUCH THAT
    	
    	//                         MAXRM < TOL 
    	
    	//     AND THAT, IF POSSIBLE, 
    	
    	//                   0.5*TOL <= MAXRM < TOL.
    	
    	//     IER=0 - NORMAL EXIT
    	//     IER=25- THE LOCAL ARRAY BOUND PARAMETER MNXI NEEDS INCREASING.
    	             
    	
    	//     LOCAL VARIABLES
    	
    	final int MNXI = 100;
    	int NXI;
    	double SS,PI,SS1,SS2,SS3,DP,LEN,TAUI,SINC,STAR,RXI,DT,HDP;
    	double XI[] = new double[2];
    	//COMPLEX XI
    	double XIVAL[][] = new double[MNXI][2];
    	// COMPLEX XIVAL(MNXI)
    	// EXTERNAL DEPPJ9
    	
    	//     INITIALISATION
    	
    	PI=Math.PI;
    	      /*TAUI=1E+0
    	      DP=DELTA*PI
    	      DT=DELTA*2E+0
    	      HDP=5E-1*DP
    	      LEN=2E+0+DP
    	      SINC=LEN/NINTS
    	      STAR=SINC
    	C
    	C     START OF LOOP FOR INTERVAL HALVING
    	C
    	10    CONTINUE
    	      NXI=0
    	      SS1=-STAR+SINC
    	      SS2=LEN
    	      DO 30 SS=SS1,SS2,SINC
    	        IF (SS .LT. HDP) THEN
    	            SS3=SS/DELTA
    	            XI=1E+0+DELTA*CMPLX(COS(SS3),SIN(SS3))
    	        ELSE IF (SS .GT. 2E+0+HDP) THEN
    	            SS3=(SS-2E+0-HDP)/DELTA
    	            XI=-1E+0+DELTA*(0E+0,1E+0)*CMPLX(COS(SS3),SIN(SS3))
    	        ELSE
    	            SS3=SS-HDP
    	            XI=CMPLX(1E+0-SS3,DELTA)
    	        ENDIF
    	        RXI=REAL(XI)
    	        IF (RXI .LT. (TAUI+DT)) THEN
    	          NXI=NXI+1
    	          IF (NXI .GT. MNXI) THEN
    	            IER=25
    	            RETURN
    	          ENDIF
    	          XIVAL(NXI)=XI
    	        ENDIF
    	30    CONTINUE
    	C
    	      IF (NXI .EQ. 0) THEN
    	        SINC=STAR
    	        STAR=5E-1*STAR
    	        NINTS=NINTS*2
    	        GOTO 10
    	      ENDIF
    	C      
    	      TAU=TAUI
    	      CALL DEPPJ9(XIVAL,NXI,BETA,TAU,NQUAD,DGPOL,ACOEF,BCOEF,H0VAL,
    	     +            SOLUN(1),TOL,MAXRM,IER) 
    	      IF (IER .GT. 0) THEN
    	        RETURN
    	      ENDIF
    	C
    	      IF (TAU .NE. TAUI) THEN
    	        TAUI=TAU
    	        SINC=STAR
    	        STAR=5E-1*STAR
    	        NINTS=NINTS*2
    	        GOTO 10
    	      ENDIF
    	C
    	      IER=0
    	C
    	      END*/
    } // private void DEPPJ8


      /**
       * zabs computes the absolute value or magnitude of a double precision complex variable zr + j*zi.
       * 
       * @param zr double
       * @param zi double
       * 
       * @return double
       */
      public double zabs(final double zr, final double zi) {
          double u, v, q, s;
          u = Math.abs(zr);
          v = Math.abs(zi);
          s = u + v;

          // s * 1.0 makes an unnormalized underflow on CDC machines into a true
          // floating zero
          s = s * 1.0;

          if (s == 0.0) {
              return 0.0;
          } else if (u > v) {
              q = v / u;

              return (u * Math.sqrt(1.0 + (q * q)));
          } else {
              q = u / v;

              return (v * Math.sqrt(1.0 + (q * q)));
          }
      }
      
      /**
       * complex multiply c = a * b.
       * 
       * @param ar double
       * @param ai double
       * @param br double
       * @param bi double
       * @param cr double[]
       * @param ci double[]
       */
      public void zmlt(final double ar, final double ai, final double br, final double bi, final double[] cr,
              final double[] ci) {
          double ca, cb;

          ca = (ar * br) - (ai * bi);
          cb = (ar * bi) + (ai * br);
          cr[0] = ca;
          ci[0] = cb;

          return;
      }
      
      /**
       * complex divide c = a/b.
       * 
       * @param ar double
       * @param ai double
       * @param br double
       * @param bi double
       * @param cr double[]
       * @param ci double[]
       */
      public void zdiv(final double ar, final double ai, final double br, final double bi, final double[] cr,
              final double[] ci) {
          double bm, cc, cd, ca, cb;

          bm = 1.0 / zabs(br, bi);
          cc = br * bm;
          cd = bi * bm;
          ca = ( (ar * cc) + (ai * cd)) * bm;
          cb = ( (ai * cc) - (ar * cd)) * bm;
          cr[0] = ca;
          ci[0] = cb;

          return;
      }

}