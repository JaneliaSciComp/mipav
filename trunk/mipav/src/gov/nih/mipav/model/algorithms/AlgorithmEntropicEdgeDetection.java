package gov.nih.mipav.model.algorithms;

import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;

    // Reference: "A New Algorithm Based Entropic Threshold for Edge Detection in Images"
    // by Mohamed A. El-Sayed, International Journal of Computer Science Issues, Vol. 8,
    // Issue 5, No. 1, Spetember, 2011

public class AlgorithmEntropicEdgeDetection extends AlgorithmBase {
    
  //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * AlgorithmEntropicEdgeDetection - default constructor.
     */
    public AlgorithmEntropicEdgeDetection() { }
    
    /**
     * AlgorithmEntropicEdgeDetection.
     *
     * @param  destImg  DOCUMENT ME!
     * @param  srcImg   DOCUMENT ME!
     */
    public AlgorithmEntropicEdgeDetection(ModelImage destImg, ModelImage srcImg) {
        super(destImg, srcImg);
    }


    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * finalize -
     */
    public void finalize() {
        super.finalize();
    }

    /**
     * Starts the program.
     */
    public void runAlgorithm() {
        
        // Image must only have gray levels from 0 to 255

        int xDim;
        int yDim;
        int sliceSize;
        short buffer[];
        int p[][];
        int i;
        double pmod[][];
        int zeroCount;
        int index;
        int totalCount;
        double Max1;
        int t;
        double PA;
        double PB;
        double p1[];
        double p2[];
        double Sa;
        double Sb;
        double Sab;
        int T1 = 0;
        int Loc = 0;
        double pLow[][];
        double pHigh[][];
        int j;
        int T2;
        int T3;
        byte f[];
        byte g[];
        int x;
        int y;
        int sum1;
        
        if (srcImage == null) {
            displayError("Source Image is null");
            finalize();

            return;
        }
        
        if (srcImage.getMax() > 255) {
            displayError("Aglrithm requires maximum value not exceed 255");
            finalize();

            return;    
        }
        
        if (srcImage.getMin() < 0) {
            displayError("Aglrithm requires minimum value not be less than zero");
            finalize();

            return;        
        }
        
        fireProgressStateChanged(srcImage.getImageName(), "Running Entropic Edge Detection ...");

        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        sliceSize = xDim * yDim;
        buffer = new short[sliceSize];
        try {
            srcImage.exportData(0, sliceSize, buffer);
        }
        catch (IOException e) {
            MipavUtil.displayError("IOException " + e + " on srcImage.exportdata(0, sliceSize, buffer)");
            setCompleted(false);
            return;
        }
        
        p = new int[256][2];
        for (i = 0; i <= 255; i++) {
            p[i][0] = i;
        }
        // Create histogram
        for (i = 0; i < sliceSize; i++) {
            p[buffer[i]][1]++;
        }
        // Remove 0 count entries in histogram
        zeroCount = 0;
        totalCount = 0;
        for (i = 0; i <= 255; i++) {
            if (p[i][1] == 0) {
                zeroCount++;
            }
            else {
                totalCount += p[i][1];
            }
        }
        pmod = new double[256 - zeroCount][3];
        index = 0;
        for (i = 0; i <= 255; i++) {
            if (p[i][1] != 0) {
                pmod[index][0] = p[i][0];
                pmod[index][1] = p[i][1];
                index++;
            }
        }
        
        // Call the Shannon procedure, return t1 value and its location in p2
        
        // Normalize so that p2[i][2] sum to 1.0
        for (i = 0; i <= 255 - zeroCount; i++) {
            pmod[i][2] = pmod[i][1]/totalCount;   
        }
        Max1 = 0.0;
        for (t = 0; t <= 255 - zeroCount; t++) {
            PA = 0.0;
            for (i = 0; i <= t; i++) {
                PA += pmod[i][2];
            }
            PB = 1.0 - PA;
            p1 = new double[t+1];
            p2 = new double[256 - zeroCount - t - 1]; 
            for (i = 0; i <= t; i++) {
                p1[i] = pmod[i][2]/PA; // p1 is a probability in PA
            }
            for (i = t+1; i <= 255 - zeroCount; i++) {
                p2[i - t - 1] = pmod[i][2]/PB; // p2 is a probability in PB
            }
            Sa = 0.0;
            for (i = 0; i <= t; i++) {
                // log2(x) = ln(x)/ln(2)
                Sa -= (p1[i] * Math.log(p1[i])/Math.log(2.0));
            }
            Sb = 0.0;
            for (i = t+1; i <= 255 - zeroCount; i++) {
                Sb -= (p2[i-t-1] * Math.log(p2[i-t-1])/Math.log(2.0));
            }
            Sab = Sa + Sb;
            if (Sab > Max1) {
                T1 = (int)Math.round(pmod[t][0]);
                Loc = t;
                Max1 = Sab;
            }
        } // for (t = 0; t <= 255 - zeroCount; t++)
        
        // Call Tsallis procedure of Part1
        pLow = new double[Loc+1][3];
        for (i = 0; i <= Loc; i++) {
            for (j = 0; j <= 2; j++) {
                pLow[i][j] = pmod[i][j];
            }
        }
        T2 = Tsallis_Sqrt(pLow);
        
        // Call Tsallis procedure of Part2
        pHigh = new double[256 - zeroCount - Loc - 1][3];
        for (i = Loc + 1; i <= 255 - zeroCount; i++) {
            for (j = 0; j <= 2; j++) {
                pHigh[i - Loc - 1][j] = pmod[i][j];
            }
        }
        T3 = Tsallis_Sqrt(pHigh);
        Preferences.debug("T1 = " + T1 + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("T2 = " + T2 + "\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("T3 = " + T3 + "\n", Preferences.DEBUG_ALGORITHM);
        
        f = new byte[sliceSize];
        
        for (i = 0; i < sliceSize; i++) {
            if (((buffer[i] >= T2) && (buffer[i] < T1)) || (buffer[i] >= T3)) {
                f[i] = 1;
            }
        }
        
        g = new byte[sliceSize];
        for (y = 1; y < yDim - 1; y++) {
            for (x = 1; x < xDim - 1; x++) {
                sum1 = 0;
                for (j = -1; j <= 1; j++) {
                    for (i = -1; i <= 1; i++) {
                        if (f[x + y*xDim] == f[x + i + (y + j)*xDim]) {
                            sum1++;
                        }
                    }
                }
                if (sum1 <= 6) {
                    g[x + y * xDim] = 1;
                }
            }
        }
        
        try {
            destImage.importData(0, g, true);
        }
        catch (IOException e) {
            MipavUtil.displayError("IOException " + e + " on destImage.importData(0, g, true)");
            setCompleted(false);
            return;
        }
        
        setCompleted(true);
        return;
    }
    
    private int Tsallis_Sqrt(double[][] p) {
        double totalSum = 0.0;
        int i;
        int t;
        double Max1 = 0.0;
        double PA;
        double PB;
        double p1[];
        double p2[];
        double sum1 = 0.0;
        double sum2 = 0.0;
        double Tab;
        int T = 0;
        
        for (i = 0; i < p.length; i++) {
            totalSum += p[i][1];    
        }
        
        for (i = 0; i < p.length; i++) {
            p[i][2] = p[i][1]/totalSum;
        }
        
        for (t = 0; t < p.length; t++) {
            PA = 0.0;
            for (i = 0; i <= t; i++) {
                PA += p[i][2];
            }
            PB = 1.0 - PA;
            p1 = new double[t+1];
            sum1 = 0.0;
            for (i = 0; i <= t; i++) {
                p1[i] = p[i][2]/PA;
                sum1 += Math.sqrt(p1[i]);
            }
            p2 = new double[p.length - t - 1];
            sum2 = 0.0;
            for (i = t + 1; i < p.length; i++) {
                p2[i-t-1] = p[i][2]/PB;
                sum2 += Math.sqrt(p2[i-t-1]);
            }
            Tab = sum1 * sum2 - 1.0;
            if (Tab > Max1) {
                T = (int)Math.round(p[t][0]);
                Max1 = Tab;
            }
        } // for (t = 0; t < p.length; t++)
        return T;
    }
    
}