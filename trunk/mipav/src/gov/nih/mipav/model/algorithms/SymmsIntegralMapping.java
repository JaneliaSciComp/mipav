package gov.nih.mipav.model.algorithms;

import java.io.File;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.util.Scanner;

import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.MipavUtil;

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

    int I,IA,L;
    int IMX = 0;
    double A1,DIFF,ERR,HH,MINC,R1MACH,RMAX,RMEAN,RMIN,T,TINC,TOL1,TMX,TSD;
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
    String OFL;
    //CHARACTER OFL*6
    final int MNARC = 200;
    final double DTOL = 1.0E-1;
    final int NH = 4;
    boolean ATEND,FIRST,WARND;
    boolean LNSEG[] = new boolean[MNARC];
    double PIN[] = new double[2];
    double PAROUT[];
    int zzindex;
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
   /*for (IA=1; IA <= NARCS; IA++) {
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
20      CONTINUE
C
C****   TEST THE COMPATIBILTY OF PARFUN AND DPARFN BY ESTIMATING DPARFN
C****   NUMERICALLY AT BOTH REAL AND COMPLEX PARAMETER VALUES.
C
     DO 30 I=1,2
       IF (I.EQ.1) THEN
         C1=CMPLX(TT(1))
       ELSE
         C1=CMPLX(TT(1),MINC)
       ENDIF
       DZZ=DPARFN(IA,C1)
       NDZZ=ZDPARF(IA,C1)
       A1=ABS(DZZ)
C
       IF (A1.EQ.0E+0) THEN
         IER=60
         WRITE(*,*)
         WRITE(*,*) '              ***DPARFN=(0.,0.)***'
         WRITE(*,*) '                             ARC:',IA 
         WRITE(*,*) '    STANDARDISED PARAMETER VALUE:',TT(1) 
         GOTO 999
       ENDIF
C
       IF (A1.LE.TOL1 .AND. .NOT.WARND) THEN
         WRITE(*,4) '*** W A R N I N G  ***'
         WRITE(*,2) 'PATHOLOGICALLY SMALL DERIVATIVE ON ARC',IA
         WARND=.TRUE.
       ENDIF
C
       IF (FIRST) THEN
         TINC=RMEAN/A1
         TINC=MAX(TINC,MINC)
         FIRST=.FALSE.
       ENDIF
C
       ERR=ABS(1E+0-NDZZ/DZZ)
       IF (ERR.GT.MXDIF) THEN
         MXDIF=ERR
         IMX=IA
         TMX=TT(1)
       ENDIF
30      CONTINUE
C
     IF (.NOT.LNSEG(IA)) THEN
C
C****     DETERMINE THE NEXT BOUNDARY POINT TO BE PLOTTED
C
40        CONTINUE
       TT(2)=TT(1)+TINC
       IF (TT(2) .GE. 1E+0) THEN
         TT(2)=1E+0
         ATEND=.TRUE.
       ELSE
         ATEND=.FALSE.
       ENDIF
C
       ZZ(2)=PARFUN(IA,CMPLX(TT(2)))
       DIFF=ABS(ZZ(2)-ZZ(1))
       IF (DIFF.EQ.0E+0 .AND. .NOT.ATEND) THEN
         TINC=MAX(MINC,2*TINC)
         GOTO 40
       ENDIF
C
       IF (DIFF.GT.RMAX .OR. (DIFF.LT.RMIN .AND. .NOT.ATEND)) THEN
         TINC=RMEAN*TINC/DIFF
         TINC=MAX(TINC,MINC)
         GOTO 40
       ENDIF
C
       WRITE(CHNL,'(2E16.7)') ZZ(2)
       IF (.NOT. ATEND) THEN
         ZZ(1)=ZZ(2)
         TT(1)=TT(2)
         GOTO 20
       ENDIF
     ENDIF

   } // for (IA=1; IA <= NARCS; IA++)
   IF (LNSEG(NARCS)) WRITE(CHNL,'(2E16.7)') ZZ0
   CLOSE(CHNL)   
C
   IF (MXDIF .GT. DTOL) THEN
     WRITE(*,*)
     WRITE(*,2) 'POSSIBLE PARFUN/DPARFN INCONSISTECY ON ARC:',IMX
     WRITE(*,3) 'OCCURS AT STANDARDISED PARAMETER VALUE:',TMX
     WRITE(*,3) 'RELATIVE FINITE DIFF ERROR:',MXDIF 
   ELSE
     WRITE(*,*)
     WRITE(*,1) 'PARFUN AND DPARFN ARE CONSISTENT:'
   ENDIF
C
999   CALL WRTAIL(7,0,IER)*/

   } // private void TSTPLOT
   
   private void LINSEG(boolean LNSEG[], int NARCS) {

       //**** TO DETERMINE THE ARRAY LNSEG, WHERE LNSEG(I) IS SET TO TRUE IF THE
       //**** I'TH ARC IS A LINE SEGMENT, I=1,...,NARCS.

       //**** LOCAL VARIABLES

       final int NPTS = 9;
	   int IA,J,NINTS;
       double DIFF,HH,MXDIF,TOL,R1MACH;
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

}