/* $Header: /brain/craig/work/CVS/java/cj/math/lpsolve/lp/lprec.java,v 1.4 2002/04/19 16:32:43 craig Exp $ */
/* $Log: lprec.java,v $
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
# Revision 1.3  1996/06/07  01:31:45  hma
# changed some member functions to be public
#
# Revision 1.2  1996/06/06  19:46:40  hma
# added package statement
#
# Revision 1.1  1996/05/21  02:02:54  hma
# Initial revision
# */

/* fields indicated with ## may be modified directly */
/* pointers will have there size in the comments */

package gov.nih.mipav.model.algorithms.t2mapping.cj.math.lpsolve.lp;

public class lprec implements constant
{
  String   lp_name;		/* the name of the lp */

  public boolean active;	        /*true if the globals point to this structure*/
  public boolean verbose;         /* ## Verbose flag */
  public boolean print_duals;     /* ## PrintDuals flag for PrintSolution */
  public boolean print_sol;       /* ## used in lp_solve */
  public boolean debug;           /* ## Print B&B information */
  public boolean print_at_invert; /* ## Print information at every reinversion */
  public boolean trace;           /* ## Print information on pivot selection */
  public boolean anti_degen;	/* ## Do perturbations */
  
  int	    rows;               /* Nr of constraint rows in the problem */
  int       rows_alloc;      	/* The allocated memory for Rows sized data */
  int       columns;            /* The number of columns (= variables) */
  int       columns_alloc;  
  int       sum;                /* The size of the variables + the slacks */
  int       sum_alloc;

  boolean     names_used;         /* Flag to indecate if names for rows and
				   columns are used */
  String[]  row_name;		/* rows_alloc+1 */
  String[]  col_name;		/* columns_alloc+1 */

 /* Row[0] of the sparce matrix is the objective function */

  int       non_zeros;          /* The number of elements in the sparce matrix*/
  int       mat_alloc;		/* The allocated size for matrix sized 
				   structures */
  matrec[]  mat;                /* mat_alloc :The sparse matrix */
  int[]     col_end;            /* columns_alloc+1 :Cend[i] is the index of the
		 		   first element after column i.
				   column[i] is stored in elements 
				   col_end[i-1] to col_end[i]-1 */
  int[]     col_no;             /* mat_alloc :From Row 1 on, col_no contains the
				   column nr. of the
                                   nonzero elements, row by row */
  boolean     row_end_valid;	/* true if row_end & col_no are valid */
  int[]     row_end;            /* rows_alloc+1 :row_end[i] is the index of the 
				   first element in Colno after row i */
  double[]  orig_rh;            /* rows_alloc+1 :The RHS after scaling & sign 
				   changing, but before `Bound transformation' */
  double[]  rh;		        /* rows_alloc+1 :As orig_rh, but after Bound 
				   transformation */
  double[]  rhs;		/* rows_alloc+1 :The RHS of the curent simplex 	
				   tableau */
  boolean[]   must_be_int;        /* sum_alloc+1 :true if variable must be 
				   Integer */
  double[]  orig_upbo;          /* sum_alloc+1 :Bound before transformations */
  double[]  orig_lowbo;	        /*  "       "                   */
  double[]  upbo;               /*  "       "  :Upper bound after transformation 
				   & B&B work*/
  double[]  lowbo;              /*  "       "  :Lower bound after transformation
				   & B&B work */

  boolean     basis_valid;        /* true is the basis is still valid */
  int[]     bas;                /* rows_alloc+1 :The basis column list */
  boolean[]   basis;              /* sum_alloc+1 : basis[i] is true if the column
				   is in the basis */
  boolean[]   lower;              /*  "       "  :true is the variable is at its 
				   lower bound (or in the basis), it is false
				   if the variable is at its upper bound */

  boolean     eta_valid;          /* true if current Eta structures are valid */
  int       eta_alloc;          /* The allocated memory for Eta */
  int       eta_size;           /* The number of Eta columns */
  int       num_inv;            /* The number of double pivots */
  int       max_num_inv;        /* ## The number of double pivots between 
				   reinvertions */
  double[]  eta_value;          /* eta_alloc :The Structure containing the
				   values of Eta */
  int[]     eta_row_nr;         /*  "     "  :The Structure containing the Row
				   indexes of Eta */
  int[]     eta_col_end;        /* rows_alloc + MaxNumInv : eta_col_end[i] is
				   the start index of the next Eta column */

  boolean	    bb_rule;		/* what rule for selecting B&B variables */

  boolean     break_at_int;       /* true if stop at first integer better than
                                   break_value */
  double    break_value;        

  double    obj_bound;          /* ## Objective function bound for speedup of 
				   B&B */
  int       iter;               /* The number of iterations in the simplex
				   solver (LP) */
  int       total_iter;         /* The total number of iterations (B&B) (ILP)*/ 
  int       max_level;          /* The Deepest B&B level of the last solution */
  int	    total_nodes;	/* total number of nodes processed in b&b */
  double[]  solution;           /* sum_alloc+1 :The Solution of the last LP, 
				   0 = The Optimal Value, 
                                   1..rows The Slacks, 
				   rows+1..sum The Variables */
  double[]  best_solution;      /*  "       "  :The Best 'Integer' Solution */
  double[]  duals;              /* rows_alloc+1 :The dual variables of the
				   last LP */
  
  boolean     maximise;           /* true if the goal is to maximise the 
				   objective function */
  boolean     floor_first;        /* true if B&B does floor bound first */
  boolean[]   ch_sign;            /* rows_alloc+1 :true if the Row in the matrix
				   has changed sign 
                                   (a`x > b, x>=0) is translated to 
				   s + -a`x = -b with x>=0, s>=0) */ 

  boolean     scaling_used;	/* true if scaling is used */
  boolean     columns_scaled;     /* true is the columns are scaled too, Only use
		 		   if all variables are non-integer */
  double[]  scale;              /* sum_alloc+1 :0..Rows the scaling of the Rows,
				   Rows+1..Sum the scaling of the columns */

  int	    nr_lagrange;	/* Nr. of Langrangian relaxation constraints */
  double[][]lag_row;	        /* NumLagrange, columns+1:Pointer to pointer of 
				   rows */
  double[]  lag_rhs;	        /* NumLagrange :Pointer to pointer of Rhs */
  double[]  lambda;		/* NumLagrange :Lambda Values */
  short[]   lag_con_type;       /* NumLagrange :true if constraint type EQ */
  double    lag_bound;		/* the lagrangian lower bound */

  boolean     valid;		/* Has this lp pased the 'test' */
  double    infinite;           /* ## numercal stuff */
  double    epsilon;            /* ## */
  double    epsb;               /* ## */
  double    epsd;               /* ## */
  double    epsel;              /* ## */


public lprec (int nrows, int ncolumns)
{
  int i, nsum;  

  nsum=nrows+ncolumns;
  if(rows < 0 || columns < 0)
    System.err.print("rows < 0 or columns < 0");

  lp_name = new String("unnamed");
  active=false;
  verbose=false;
  print_duals=false;
  print_sol=false;
  debug=false;
  print_at_invert=false;
  trace=false;

  rows=nrows;
  columns=ncolumns;
  sum=nsum;
  rows_alloc=rows;
  columns_alloc=columns;
  sum_alloc=sum;
  names_used=false;

  obj_bound=DEF_INFINITE;
  infinite=DEF_INFINITE;
  epsilon=DEF_EPSILON;
  epsb=DEF_EPSB;
  epsd=DEF_EPSD;
  epsel=DEF_EPSEL;
  non_zeros=0;
  mat_alloc=1;

  mat = new matrec[mat_alloc];
  for (i = 0; i < mat_alloc; i++)
    mat[i] = new matrec(0, 0);

  col_no = new int[mat_alloc];
  for (i = 0; i < mat_alloc; i++)
    col_no[i] = 0;

  col_end = new int[columns + 1];
  for (i = 0; i < columns + 1; i++)
    col_end[i] = 0;

  row_end = new int[rows + 1];
  for (i = 0; i < rows + 1; i++)
    row_end[i] = 0;

  row_end_valid=false;

  orig_rh = new double[rows + 1];
  for (i = 0; i < rows + 1; i++)
    orig_rh[i] = 0;

  rh = new double[rows + 1];
  for (i = 0; i < rows + 1; i++)
    rh[i] = 0;

  rhs = new double[rows + 1];
  for (i = 0; i < rows + 1; i++)
    rhs[i] = 0;

  must_be_int = new boolean[sum + 1];
  for (i = 0; i < sum + 1; i++)
    must_be_int[i]=false;

  orig_upbo = new double[sum + 1];
  for(i = 0; i <= sum; i++)
    orig_upbo[i]=infinite;

  upbo = new double[sum + 1];
  for (i = 0; i < sum + 1; i++)
    upbo[i] = 0;

  orig_lowbo = new double[sum + 1];
  for (i = 0; i < sum + 1; i++)
    orig_lowbo[i] = 0;

  lowbo = new double[sum + 1];
  for (i = 0; i < sum + 1; i++)
    lowbo[i] = 0;

  basis_valid=true;

  bas = new int[rows+1];
  for (i = 0; i <= rows; i++)
    bas[i] = 0;

  basis = new boolean[sum + 1];
  for (i = 0; i <= sum; i++)
    basis[i] = false;

  lower = new boolean[sum + 1];
  for(i = 0; i <= rows; i++)
    {
      bas[i]=i;
      basis[i]=true;
    }
  for(i = rows + 1; i <= sum; i++)
    basis[i]=false;
  for(i = 0 ; i <= sum; i++)
    lower[i]=true;
 
  eta_valid=true;
  eta_size=0;
  eta_alloc=10000;
  max_num_inv=DEFNUMINV;

  nr_lagrange=0;

  eta_value = new double[eta_alloc];
  for (i = 0; i < eta_alloc; i++)
    eta_value[i] = 0;

  eta_row_nr = new int[eta_alloc];
  for (i = 0; i < eta_alloc; i++)
    eta_row_nr[i] = 0;

  eta_col_end = new int[rows_alloc + max_num_inv];
  for (i = 0; i < rows_alloc + max_num_inv; i++)
    eta_col_end[i] = 0;

  bb_rule=FIRST_NI;
  break_at_int=false;
  break_value=0;

  iter=0;
  total_iter=0;

  solution = new double[sum + 1];
  for (i = 0; i <= sum; i++)
    solution[i] = 0;

  best_solution = new double[sum + 1];
  for (i = 0; i <= sum; i++)
    best_solution[i] = 0;

  duals = new double[rows + 1];
  for (i = 0; i <= rows; i++)
    duals[i] = 0;

  maximise = false;
  floor_first = true;

  scaling_used = false;
  columns_scaled = false;

  ch_sign = new boolean[rows + 1];
  for(i = 0; i <= rows; i++)
    ch_sign[i] = false;

  valid = false; 
} // end of constructor from row and column

//***************************************
// return the ith member of the best_solution[]
//
public double getBestSolution(int i) {
  return best_solution[i];
}

//***************************************
// get the number of rows
//
public int getRows() {
  return rows;
}

//***************************************
// get the number of columns
//
public int getColumns() {
  return columns;
}

} // end of class lprec

