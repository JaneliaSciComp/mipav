package gov.nih.mipav.model.algorithms;

import java.io.File;
import java.io.IOException;
import java.io.RandomAccessFile;

import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.MipavUtil;

public class SymmsIntegralMapping extends AlgorithmBase  {
	String fileDir;
	
	// EPS returns the distance from 1.0 to the next larger double-precision number, that is, eps = 2^-52.
    private double EPS;
    // Filename to receive FORTRAN output code
    private String FORTFL;
    // Does the domain have any symmetry?
    private boolean SYMTY;
    // If SYMTY is true, are there any reflectional symmetries?
    private boolean REFLN;
    // If SYMTY is true, what are the coordinates of the center of symmetry?
    private double CENSY[] = new double[2];
    // If SYMTY is true, number of arcs on the fundamental boundary section
    // If SYMTY is false, number of arcs on the boundary
    // NARCS <= MNARC-1 = 99
    private int NARCS;
    // NUMDER is of length MNARC
    // NUMDER is initially false
    private boolean NUMDER[];
    // ARCTY is of length MNARC
    // Type of arc, 1 = LINE SEGMENT, 2 = CIRCULAR ARC SEGMENT, 3 = CARTESIAN PARAMETRIC FUNCTION
    // 4 = POLAR FUNCTION
    private int ARCTY[];
    // STAPT is of length MNARC,2
    // Initial point on line, initial point on circle, or initial point on curve
    // IF (SYMTY) THEN
    // WRITE(*,*) 'COORDINATES OF FINAL POINT ON THIS ARC?'
    //STAPT[NARCS]=CMPLX(FINAL X point, FINAL Y point)
    //ELSE
    // STAPT[NARCS]=STAPT[1]
    //ENDIF
    private double STAPT[][];
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
    private int PGM[];
    // RGM is of length 3*MNARC
    private double RGM[];
    // Start with TXCO = 0
    // For types 3 and 4
    // For each IA = 1 to NARCS do J = 1,2
    //TXCO=TXCO+1
    //PTX[IA-1+(J-1)*MNARC]=TXCO
    // DEFN[TXCO-1]= TXT  
    // where TXT =
    // for J = 1 and TYPE = 3 JAVA EXPRESSION FOR PARFUN
    // for J = 2 and TYPE = 3 JAVA EXPRESSION FOR DPARFN
    // for J = 1 and TYPE = 4 JAVA EXPRESSION FOR RADIUS
    // for J = 2 and TYPE = 4 JAVA EXPRESSION FOR RADIUS DERIVATIVE
    // PTX is of length 2*MNARC
    private int PTX[];
    
	public SymmsIntegralMapping() {
		
	}
	
	public SymmsIntegralMapping(ModelImage destImg, ModelImage srcImg, String FORTFL, boolean SYMTY,
			boolean REFLN, double CENSY[], int NARCS, boolean NUMDER[], double STAPT[][],
			int PGM[]) {
	    super(destImg, srcImg);
	    this.FORTFL = FORTFL;
	    this.SYMTY = SYMTY;
	    this.REFLN = REFLN;
	    this.CENSY = CENSY;
	    this.NARCS = NARCS;
	    this.NUMDER = NUMDER;
	    this.STAPT = STAPT;
	    this.PGM = PGM;
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
	      int GMCO,IA,I,IER,J,L,ORDRG,ORDSG,SW,
	          TXCO,TYPE;
	      double ALPHA,PI,R1MACH,X,Y;
	      //COMPLEX CENSY,RTUNI,U2
	      double RTUNI[] = new double[2];
	      double U2[] = new double[2];
	      boolean CHRIN,CHCK;
	     // CHARACTER TXT*72,TABC*6,FORTFL*72,CH*2,SIG(10)*2,WID(10)*2,REDD*6,
	     //+FMT1*8,FMT2*9
	      String TXT;
	      String FORTFL;
	      String CH;
	      String SIG[] = new String[]{"7","8","9","10","11","12","13","14","15","16"};
	      String WID[] = new String[]{"15","16","17","18","19","20","21","22","23","24"};
	      String REDD;
	      String FMT1;
	      String FMT2;
	
	      //PARAMETER (MNARC=100,TABC='     +',CHNL=20,CHIN=21)
	      final int MNARC = 100;
	      final String TABC = "     +";
	      final int CHNL = 20;
	      final int CHIN = 21;
	      int PTX[] = new int[2*MNARC];
	      int NTX[] = new int[2*MNARC];
	      //CHARACTER DEFN(MNARC*2)*72
	      String DEFN[] = new String[2*MNARC];
	      File file;
	      RandomAccessFile raFile = null;
	
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
	
	      
	/*C**** WRITE THE SOURCE CODE FOR PARFUN
	C
	      OPEN(CHNL,FILE=FORTFL)
	      CALL HEADER('PARFUN',REDD,CHNL)
	      IF (SYMTY) THEN
	        CALL SYINF1(ORDRG,ORDSG,RTUNI,U2,REFLN,CENSY,STAPT(1),
	     +              STAPT(NARCS+1),IER)
	        IF (IER.GT.0) GOTO 999
	        WRITE(*,'(/A,I3)') 'N O T E : THE ORDER OF THE SYMMETRY GROUP IS
	     +',ORDSG
	        IF (REFLN) THEN
	           WRITE(*,*) '          ISYGP = ',-ORDSG
	        ELSE
	           WRITE(*,*) '          ISYGP = ',ORDSG
	        ENDIF
	        CALL WRSYM1(NARCS,ORDRG,ORDSG,RTUNI,U2,CENSY,REFLN,.TRUE.,REDD,
	     +              CHNL)
	        IF (REFLN) THEN
	          CH='TS'
	        ELSE
	          CH='TT'
	        ENDIF
	        CALL WRFUN1(NARCS,STAPT,ARCTY,PGM,RGM,PTX,NTX,DEFN,CHNL,
	     +             'IB',CH,'ZETA  ',REDD)
	        CALL WRSYM2(NARCS,ORDRG,CENSY,REFLN,CHNL)
	      ELSE
	        CALL WRFUN1(NARCS,STAPT,ARCTY,PGM,RGM,PTX,NTX,DEFN,CHNL,
	     +             'IA','TT','PARFUN',REDD)
	      ENDIF
	C
	      WRITE(CHNL,'(A1)') 'C'
	      WRITE(CHNL,'(A9)') '      END'
	C
	C**** WRITE THE SOURCE CODE FOR DPARFN
	C
	      WRITE(CHNL,'(A40)') 'C...........................................'
	      CALL HEADER('DPARFN',REDD,CHNL)
	      IF (SYMTY) THEN
	        CALL WRSYM1(NARCS,ORDRG,ORDSG,RTUNI,U2,CENSY,REFLN,.FALSE.,REDD,
	     +              CHNL)
	        IF (REFLN) THEN
	          CH='TS'
	        ELSE
	          CH='TT'
	        ENDIF
	        CALL WRFUN2(NARCS,MNARC,STAPT,ARCTY,PGM,RGM,PTX,NTX,DEFN,
	     +              CHNL,'IB',CH,'ZETA  ',NUMDER,REDD)
	        CALL WRSYM3(NARCS,ORDRG,REFLN,CHNL)
	      ELSE
	        CALL WRFUN2(NARCS,MNARC,STAPT,ARCTY,PGM,RGM,PTX,NTX,DEFN,
	     +              CHNL,'IA','TT','DPARFN',NUMDER,REDD)
	      ENDIF
	C
	      WRITE(CHNL,'(A1)') 'C'
	      WRITE(CHNL,'(A9)') '      END'
	      CLOSE(CHNL)
	C
	999   CONTINUE
	      CALL WRTAIL(6,0,IER)
	C*/
	} // public void PARGEN
	
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
        	  System.out.println("\n\n      "+DOTS+"\n      "+TXT+"\n      "+DOTS+"\n");
          }
          else {
        	  try {
        	      raFile.writeBytes("\n\n      "+DOTS+"\n      "+TXT+"\n      "+DOTS+"\n");
        	  }
        	  catch (IOException e) {
        		  MipavUtil.displayError("IOException " + e + " on raFile.writeBytes in WRHEAD");
        		  System.exit(-1);
        	  }
          }
          return;
      } // private void WRHEAD

}