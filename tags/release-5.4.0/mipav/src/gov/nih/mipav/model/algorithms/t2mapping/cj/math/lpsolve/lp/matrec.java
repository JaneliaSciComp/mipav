/* $Header: /brain/craig/work/CVS/java/cj/math/lpsolve/lp/matrec.java,v 1.3 2002/04/19 16:32:43 craig Exp $ */
/* $Log: matrec.java,v $
/* Revision 1.3  2002/04/19 16:32:43  craig
/* MAJOR CHANGES TO DIRECTORY STRUCTURE AND RENAMING
/*
/* Revision 1.2  2002/03/17 05:31:10  craig
/* Cleaned up the LPSolve.
/* Added makefiles.
/*
/* Revision 1.1.1.1  2001/09/11 21:52:39  craig
/*
/*
# Revision 1.2  1996/06/06  19:47:20  hma
# added package statement
#
# Revision 1.1  1996/05/21  02:04:15  hma
# Initial revision
# */

package gov.nih.mipav.model.algorithms.t2mapping.cj.math.lpsolve.lp;

public class matrec
{
  int row_nr;
  double value;
  public matrec(int r, double v) {
    row_nr = r;
    value = v;
  }
}
