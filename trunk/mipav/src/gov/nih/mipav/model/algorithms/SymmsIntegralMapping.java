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
	public SymmsIntegralMapping() {
		
	}
	
	public SymmsIntegralMapping(ModelImage destImg, ModelImage srcImg) {
	    super(destImg, srcImg);	
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
	      int GMCO,IA,I,IER,J,L,NARCS,ORDRG,ORDSG,SW,
	          TXCO,TYPE;
	      double ALPHA,PI,R1MACH,X,Y;
	      //COMPLEX CENSY,RTUNI,U2
	      double CENSY[] = new double[2];
	      double RTUNI[] = new double[2];
	      double U2[] = new double[2];
	      boolean CHRIN,REFLN,SYMTY,CHCK;
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
	      int ARCTY[] = new int[MNARC];
	      int PGM[] = new int[MNARC];
	      int PTX[] = new int[2*MNARC];
	      int NTX[] = new int[2*MNARC];
	      double RGM[] = new double[3*MNARC];
	      //COMPLEX STAPT(MNARC)
	      double STAPT[][] = new double[MNARC][2];
	      boolean NUMDER[] = new boolean[MNARC];
	      //CHARACTER DEFN(MNARC*2)*72
	      String DEFN[] = new String[2*MNARC];
	      File file;
	      RandomAccessFile raFile = null;
	
	      //EXTERNAL CHRIN,HEADER,R1MACH,SYINF1,WRFUN1,WRFUN2,WRHEAD,WRSYM1,
	      //+WRSYM2,WRSYM3,WRTAIL
	
	      WRHEAD(6,0, null);
	
	      PI= Math.PI;
	      file = new File(fileDir + "pgenin");
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
	      //OPEN(CHIN,FILE='pgenin')
	
	      //**** DETERMINE NUMBER OF SIGNIFICANT FIGURES REQUIRED TO MATCH MACHINE 
          //**** PRECISION AND SET UP POINTER SW TO SIG AND WID
	
	      SW=(int)(-Math.log10(EPS))+2;
	      /*IF (SW.LE.7) THEN
	        SW=1
	      ELSE IF (SW.GE.16) THEN
	        SW=10
	      ELSE
	        SW=SW-6
	      ENDIF
	C
	C**** SET UP THE EDIT DESCRIPTOR AND FORMAT SPECIFICATION FOR FLOATING 
	C**** POINT OUTPUT
	C
	      REDD='E'//WID(SW)//'.'//SIG(SW) 
	      FMT1='('//REDD//')'
	      FMT2='(2'//REDD//')'   
	C
	      WRITE(*,*) 'FILENAME TO RECEIVE OUTPUT FORTRAN CODE?'
	      READ(*,'(A)') FORTFL
	      WRITE(CHIN,'(A)') FORTFL
	C
	      WRITE(*,*) 'DOES THE DOMAIN HAVE ANY SYMMETRY?'
	      SYMTY=CHRIN('Y','y')
	      IF (SYMTY) THEN
	        WRITE(CHIN,*) 'Y','      ..SYMMETRY'
	      ELSE
	        WRITE(CHIN,*) 'N','      ..SYMMETRY'
	      ENDIF
	      IF (SYMTY) THEN
	        WRITE(*,*) 'ARE THERE ANY REFLECTIONAL SYMMETRIES?'
	        REFLN=CHRIN('Y','y')
	        IF (REFLN) THEN
	          WRITE(CHIN,*) 'Y','      ..REFLECTIONAL SYMMETRY'
	        ELSE
	          WRITE(CHIN,*) 'N','      ..REFLECTIONAL SYMMETRY'
	        ENDIF
	        WRITE(*,*) 'WHAT ARE THE COORDINATES OF THE CENTRE OF SYMMETRY?'
	        READ(*,*) X,Y
	        WRITE(CHIN,FMT2) X,Y
	        CENSY=CMPLX(X,Y)
	        WRITE(*,*) 'HOW MANY ARCS ARE THERE ON THE FUNDAMENTAL BOUNDARY
	     +SECTION?'
	        READ(*,*) NARCS
	        WRITE(CHIN,*) NARCS,'      ..NUMBER OF ARCS ON FBS'
	      ELSE
	        WRITE(*,*) 'HOW MANY ARCS ARE THERE ON THE BOUNDARY?'
	        READ(*,*) NARCS
	        WRITE(CHIN,*) NARCS,'      ..NUMBER OF ARCS ON BOUNDARY'
	      ENDIF
	C
	      IF (NARCS.GT.MNARC-1) THEN
	        IER=58
	        GOTO 999 
	      ENDIF 
	C
	      GMCO=0
	      TXCO=0
	C
	      DO 100 IA=1,NARCS
	        NUMDER(IA)=.FALSE.
	5       CONTINUE
	        WRITE(*,'(//A,I2,A)') 'TYPE OF ARC ',IA,'?'
	        READ(*,*) TYPE
	        WRITE(CHIN,*) TYPE,'      ..TYPE FOR ARC',IA
	        IF (TYPE.EQ.1) THEN
	          ARCTY(IA)=TYPE
	          WRITE(*,*) 'COORDINATES OF INITIAL POINT ON LINE?'
	          READ(*,*) X,Y
	          WRITE(CHIN,FMT2) X,Y
	          STAPT(IA)=CMPLX(X,Y)
	        ELSE IF (TYPE.EQ.2) THEN
	          ARCTY(IA)=TYPE
	          WRITE(*,*) 'COORDINATES OF INITIAL POINT ON CIRCLE?'
	          READ(*,*) X,Y
	          WRITE(CHIN,FMT2) X,Y
	          STAPT(IA)=CMPLX(X,Y)
	          WRITE(*,*) 'COORDINATES OF CENTRE OF CIRCLE?'
	          READ(*,*) X,Y
	          WRITE(CHIN,FMT2) X,Y
	          WRITE(*,*)'SIGNED ANGLE SUBTENDED AT CENTRE (IN UNITS OF PI)?'
	          READ(*,*) ALPHA
	          WRITE(CHIN,FMT1) ALPHA
	          GMCO=GMCO+1
	          PGM(IA)=GMCO
	          RGM(GMCO)=X
	          GMCO=GMCO+1
	          RGM(GMCO)=Y
	          GMCO=GMCO+1
	          RGM(GMCO)=ALPHA*PI
	        ELSE IF (TYPE.EQ.3 .OR. TYPE.EQ.4) THEN
	          ARCTY(IA)=TYPE
	          WRITE(*,*) 'COORDINATES OF INITIAL POINT ON CURVE?'
	          READ(*,*) X,Y
	          WRITE(CHIN,FMT2) X,Y
	          STAPT(IA)=CMPLX(X,Y)
	          IF (TYPE.EQ.3) THEN
	            WRITE(*,*) 'INITIAL AND FINAL PARAMETER VALUES?'
	          ELSE
	            WRITE(*,*)'INITIAL AND FINAL POLAR ANGLES (IN UNITS OF PI)?'
	          ENDIF
	          READ(*,*) X,Y
	          WRITE(CHIN,FMT2) X,Y
	          GMCO=GMCO+1
	          PGM(IA)=GMCO
	          IF (TYPE.EQ.4) THEN
	            RGM(GMCO)=X*PI
	            GMCO=GMCO+1
	            RGM(GMCO)=Y*PI
	          ELSE
	            RGM(GMCO)=X
	            GMCO=GMCO+1
	            RGM(GMCO)=Y
	          ENDIF
	          DO 50 J=1,2
	            IF (J.EQ.1 .AND. TYPE.EQ.3) THEN
	              WRITE(*,*) 'FORTRAN EXPRESSION FOR PARFUN?'
	            ELSE IF (J.EQ.2 .AND. TYPE.EQ.3) THEN          
	              WRITE(*,*) 'FORTRAN EXPRESSION FOR DPARFN?'
	            ELSE IF (J.EQ.1 .AND. TYPE.EQ.4) THEN
	              WRITE(*,*) 'FORTRAN EXPRESSION FOR RADIUS?'
	            ELSE
	              WRITE(*,*) 'FORTRAN EXPRESSION FOR RADIUS DERIVATIVE?'
	            ENDIF
	C
	            TXCO=TXCO+1
	            PTX(IA+(J-1)*MNARC)=TXCO
	            I=1
	C
	10          CONTINUE
	            READ(*,'(A72)') TXT
	            WRITE(CHIN,'(A72)') TXT
	            L=INDEX(TXT,'//')
	            IF (L.EQ.0) THEN
	              IF (TXT(67:72) .NE. '      ') THEN
	                WRITE(*,*) 'THIS LINE IS MORE THAN 66CH LONG.'
	                WRITE(*,*) 'PLEASE SPLIT AND ENTER AS SEPERATE LINES.'
	                GOTO 10
	              ENDIF
	              DEFN(TXCO)=TABC//TXT
	              I=I+1
	              TXCO=TXCO+1
	              GOTO 10
	            ELSE
	              IF (L.GT.67) THEN
	                WRITE(*,*) 'THIS LINE IS MORE THAN 66CH LONG.'
	                WRITE(*,*) 'PLEASE SPLIT AND ENTER AS SEPERATE LINES.'
	                GOTO 10
	              ENDIF
	              NTX(IA+(J-1)*MNARC)=I
	              IF (L.EQ.1) THEN
	                DEFN(TXCO)=TABC
	                NUMDER(IA)=.TRUE.
	              ELSE 
	                DEFN(TXCO)=TABC//TXT(1:L-1)
	              ENDIF
	            ENDIF
	            IF (J.EQ.1 .AND. TYPE.EQ.4) THEN
	              WRITE(*,*) '(... = ZRAD)'
	            ENDIF
	50        CONTINUE
	        ELSE
	          WRITE(*,*) 'TYPE MUST BE EITHER 1,2,3 OR 4'
	          GOTO 5
	        ENDIF
	100   CONTINUE  
	C
	      IF (SYMTY) THEN
	        WRITE(*,*) 'COORDINATES OF FINAL POINT ON THIS ARC?'
	        READ(*,*) X,Y
	        WRITE(CHIN,FMT2) X,Y
	        STAPT(NARCS+1)=CMPLX(X,Y)
	      ELSE
	        STAPT(NARCS+1)=STAPT(1)
	      ENDIF
	C
	C**** CHECK WITH THE USER THAT THE INPUT DATA IS OK
	C
	      IER=0
	      WRITE(*,*)
	      WRITE(*,*) 'END OF INPUT PHASE; CONTINUE WITH PROCESSING?'
	      CHCK=CHRIN('Y','y')
	      IF (.NOT.CHCK) THEN
	        WRITE(CHIN,*) 'N','      ..CONTINUE PROCESSING'
	        GOTO 999
	      ELSE
	        WRITE(CHIN,*) 'Y','      ..CONTINUE PROCESSING'
	      ENDIF
	      CLOSE(CHIN) 
	C
	C**** WRITE THE SOURCE CODE FOR PARFUN
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