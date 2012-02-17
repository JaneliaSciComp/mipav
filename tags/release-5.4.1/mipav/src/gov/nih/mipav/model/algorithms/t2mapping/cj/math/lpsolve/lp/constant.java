/* $Header: /brain/craig/work/CVS/java/cj/math/lpsolve/lp/constant.java,v 1.4 2002/04/19 16:32:43 craig Exp $ */
/* $Log: constant.java,v $
/* Revision 1.4  2002/04/19 16:32:43  craig
/* MAJOR CHANGES TO DIRECTORY STRUCTURE AND RENAMING
/*
/* Revision 1.3  2002/03/19 04:38:32  craig
/* Converted all short representations of true/false values to boolean. Still
/* need to test this all...
/*
/* Revision 1.2  2002/03/17 05:31:10  craig
/* Cleaned up the LPSolve.
/* Added makefiles.
/*
/* Revision 1.1.1.1  2001/09/11 21:52:39  craig
/*
/*
# Revision 1.2  1996/06/06  19:45:34  hma
# added package lp;
#
# Revision 1.1  1996/05/21  02:01:48  hma
# Initial revision
# */

package gov.nih.mipav.model.algorithms.t2mapping.cj.math.lpsolve.lp;

public interface constant {
  final static short FAIL = -1;
  
  final static short NULL = 0;
//  final static short FALSE = 0;
//  final static short TRUE = 1;

  final static short DEFNUMINV = 50;

/* solve status values */
  final static short OPTIMAL = 0;
  final static short MILP_FAIL = 1;
  final static short INFEASIBLE = 2;
  final static short UNBOUNDED = 3;
  final static short FAILURE = 4;
  final static short RUNNING = 5;

/* lag_solve extra status values */
  final static short FEAS_FOUND = 6;
  final static short NO_FEAS_FOUND = 7;
  final static short BREAK_BB = 8;

  final static boolean FIRST_NI =	false;
  final static boolean RAND_NI = true;

  final static short LE = 0;
  final static short EQ = 1;
  final static short GE = 2;
  final static short OF = 3;

  final static short MAX_WARN_COUNT = 20;

  final static double DEF_INFINITE = 1e24; /* limit for dynamic range */
  final static double DEF_EPSB = 5.01e-7; /* for rounding RHS values to 0 determine	
				      infeasibility basis */
  final static double DEF_EPSEL = 1e-8; /* for rounding other values (vectors) to 0 */
  final static double DEF_EPSD  = 1e-6; /* for rounding reduced costs to zero */
  final static double DEF_EPSILON = 1e-3; /* to determine if a float value is integer */
 
  final static double PREJ = 1e-3;  /* pivot reject (try others first) */

  final static int HASHSIZE = 10007; /* prime number is better, MB */
  final static int ETA_START_SIZE = 10000; /* start size of array Eta. Realloced if needed */
  final static String STD_ROW_NAME_PREFIX = "r_";

} // end of interface constant
