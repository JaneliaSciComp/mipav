package gov.nih.mipav.model.algorithms.t2mapping.com.kutsyy.util;

import java.io.*;
import java.util.*;
/**
 * Contains input/output functions for arrays<BR>
 * Created by <A href="http://www.kutsyy.com">Vadim Kutsyy</A><BR>
 * @author <A href="http://www.kutsyy.com">Vadim Kutsyy</A>
 */
public final class LaIo {



    /**
         * Print array a
         * @param a array a
         */
    public static void print(double[] a) {
        int n = a.length;
        for (int i = 0; i < n; i++) {
            System.out.print(a[i]);
            //if (i != n - 1)
            System.out.print("  ");
        }
        //System.out.println("");
    }



    /**
         * Print array a
         * @param a array a
         */
    public static void print(int[] a) {
        int n = a.length;
        for (int i = 0; i < n; i++) {
            System.out.print(a[i]);
            //if (i != n - 1)
            System.out.print("  ");
        }
        //System.out.println("");
    }



    /**
         * Print array a
         * @param a array a
         */
    public static void println(double[][] a) {
        int n = a.length;
        int m = a[0].length;
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < m; j++) {
                System.out.print(a[i][j]);
                if (j != n - 1)
                    System.out.print("  ");
            }
            System.out.print("\n");
        }
        System.out.print("\n");
    }



    /**
         * Print array a
         * @param a array a
         */
    public static void println(int[][] a) {
        int n = a.length;
        int m = a[0].length;
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < m; j++) {
                System.out.print(a[i][j]);
                if (j != n - 1)
                    System.out.print("  ");
            }
            System.out.print("\n");
        }
        System.out.print("\n");
    }



    /**
         * Print array a
         * @param a array a
         */
    public static void println(double[] a) {
        int n = a.length;
        for (int i = 0; i < n; i++) {
            System.out.print(a[i]);
            if (i != n - 1)
                System.out.print("  ");
        }
        System.out.println("");
    }



    /**
         * Print array a
         * @param a array a
         */
    public static void println(int[] a) {
        int n = a.length;
        for (int i = 0; i < n; i++) {
            System.out.print(a[i]);
            if (i != n - 1)
                System.out.print("  ");
        }
        System.out.println("");
    }



    /**
         * Read in array of double from text file
         * @return array
         * @param filename java.lang.String - name of input file
         */
    public static double[][] read(String filename) {
        Vector vec = new Vector();
        int col = 0;
        try {
            File f = new File(filename);
            if (!f.exists()) {
                System.err.println("File " + filename + " could not be found.");
                return null;
            }
            FileReader fr = new FileReader(f);
            BufferedReader br = new BufferedReader(fr);
            String ln;
            boolean first = true;
            int jj = 1;
            while ((ln = br.readLine()) != null) {
                StringTokenizer st = new StringTokenizer(ln);
                int nt = st.countTokens();
                if (nt == 0)
                    break;
                if (first) {
                    col = nt;
                    first = false;
                } else {
                    if (nt != col) {
                        System.err.println("Problem reading line " + jj + " of " + filename + ".");
                        return null;
                    }
                }
                double[] ar = new double[col];
                for (int i = 0; i < col; ++i) {
                    ar[i] = Double.valueOf(st.nextToken()).doubleValue();
                }
                vec.addElement(ar);
                ++jj;
            }
        } catch (IOException e) {
            System.err.println(e);
            return null;
        }
        double[][] A = new double[col][vec.size()];
        for (int i = 0; i < vec.size(); ++i) {
            double[] ar = (double[]) vec.elementAt(i);
            for (int j = 0; j < col; ++j) {
                A[j][i] = ar[j];
            }
        }
        return A;
    }
}