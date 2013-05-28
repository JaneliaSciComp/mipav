package gov.nih.mipav.model.structures;


import gov.nih.mipav.model.algorithms.StatisticsTable;
import gov.nih.mipav.view.*;

import java.awt.Color;
import java.io.*;
import java.net.URL;

import WildMagic.LibFoundation.Mathematics.Vector2f;


/**
 * Model of a Lookup Table (LUT). At present, one can construct 10 different LUTs: GRAY, SPECTRUM, HOTMETAL, COOLHOT,
 * SKIN, STRIPED, RED, GREEN, BLUE, GRAY_BR, and BONE. This class allows the programmer to specify the number of colors
 * for the LUT. The LUT extents should be 4 x 256 or problems may arise with the differing colormaps (This class can be
 * extended to handle arbitrary size LUTs but 256 is almost universal). There are 256 locations with alpha, red, green,
 * and blue values as floats in order to store both RGB or HSI or etc. The LUTs are calculated from the corresponding
 * transfer functions for each of a, r, g, and b.
 * 
 * @version 1.0
 * @author Matthew J. McAuliffe, Ph.D.
 */
public class ModelLUT extends ModelStorageBase {

    // ~ Static fields/initializers
    // -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 5061105716563814614L;

    /** Sets up the transfer function colors to be gray. */
    public static final int GRAY = 0;

    /** Sets up the transfer function colors to be colours of . */
    public static final int SPECTRUM = 1;

    /** Sets up the transfer function colors to be hotmetal. */
    public static final int HOTMETAL = 2;

    /** Sets up the transfer function colors to be cool-hot. */
    public static final int COOLHOT = 3;

    /** Sets up the transfer function colors to be skin-colors. */
    public static final int SKIN = 4;

    /** Sets up the transfer function colors to be striped. */
    public static final int STRIPED = 5;

    /** Sets up the transfer function colors to be monochrome-red. */
    public static final int RED = 6;

    /** Sets up the transfer function colors to be monochrome-green. */
    public static final int GREEN = 7;

    /** Sets up the transfer function colors to be monochrome-blue. */
    public static final int BLUE = 8;

    /** Sets up the transfer function colors to be gray, with blue in the first location, red in the last location. */
    public static final int GRAY_BR = 9;

    /** Sets up the transfer function to be yellow-ish orange which is supposed to make bones look good. */
    public static final int BONE = 10;

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** Fucntion that attenuates image values. */
    private TransferFunction alphaLine = new TransferFunction();

    /** Function that maps the blue function of the LUT. */
    private TransferFunction blueLine = new TransferFunction();

    /** Function that maps the green function of the LUT. */
    private TransferFunction greenLine = new TransferFunction();

    /**
     * Special int array where the LUT is packed with alpha in the most significant byte of the int, followed red,
     * green, and blue.
     */
    private int[] indexedLUT = null;

    /** Number of colors in the LUT. */
    private int nColors = 256;

    /** Function that maps the red function of the LUT. */
    private TransferFunction redLine = new TransferFunction();

    /** DOCUMENT ME! */
    private int[] remappedLUT = null;

    /** Function that maps image values into the LUT. The x coord. ranges [image.min, image.max]; */
    private TransferFunction transferLine = new TransferFunction();

    /** DOCUMENT ME! */
    private int type;

    /** The X coordinates of the transfer functions. */
    private float[] x = new float[256]; // I don't expect tranfer function to have > 256 points

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * Default Constructor.
     */
    public ModelLUT()
    {
        this(ModelLUT.GRAY, 256, new int[]{4,256});
    }
    
    /**
     * Constructor.
     * 
     * @param _type indicates type of lut (ie. GRAY, SPECTRUM ...)
     * @param _nColors number of colors defined in LUT
     * @param dimExtents array indicating LUT extent in each dimension (e.g. 4x256)
     */
    public ModelLUT(int _type, int _nColors, int[] dimExtents) {
        super(ModelStorageBase.FLOAT, dimExtents);

        type = _type;
        resetAlphaLine();
        resetTransferLine(0, dimExtents[1]);

        if ( (_nColors > 0) && (_nColors <= 256)) {
            nColors = _nColors;
        } else {
            nColors = 256;
        }

        switch (type) {

            case GRAY:
                makeGrayTransferFunctions();
                makeLUT(nColors);
                break;

            case SPECTRUM:
                makeSpectrumTransferFunctions();
                makeLUT(nColors);
                break;

            case HOTMETAL:
                makeHotMetalTransferFunctions();
                makeLUT(nColors);
                break;

            case COOLHOT:
                makeCoolHotTransferFunctions();
                makeLUT(nColors);
                break;

            case SKIN:
                makeSkinTransferFunctions();
                makeLUT(nColors);
                break;

            case STRIPED:
                makeStripedLUT();
                break;

            case RED:
                makeRedTransferFunctions();
                makeLUT(nColors);
                break;

            case GREEN:
                makeGreenTransferFunctions();
                makeLUT(nColors);
                break;

            case BLUE:
                makeBlueTransferFunctions();
                makeLUT(nColors);
                break;

            case GRAY_BR:
                makeGrayBRTransferFunctions();
                makeLUT(nColors);
                break;

            case BONE:
                makeBoneTransferFunctions();
                makeLUT(nColors);
                break;

            default:
                break;
        }
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------
    
    /**
     * Build LUT consisting of a student t-distribution at the specified level os significance for the given
     * degrees of freedom.
     * 
     * @param dof degrees of freedom
     * @param sig level of significance 
     * @param image 
     * 
     * @return
     */
    public static ModelLUT buildTDistLUT(int dof, double sig, ModelImage image) {
    	
    	double studentT = StatisticsTable.getOneTailInvTStatsitic(dof, sig);
    	
    	float max = (float) image.getMax();
    	
    	ModelLUT lut = new ModelLUT(ModelLUT.GRAY, 256, new int[]{4, 256});
		lut.makeCustomizedLUT("Rainbow2");
		TransferFunction t = new TransferFunction();
		t.addPoint((float) image.getMin(),  255);
		t.addPoint((float) studentT, 255);
		if(max > 14) {
			t.addPoint(14, 0);
		} else {
			t.addPoint(max-1, 0);
		}
		t.addPoint(max, 0);
		lut.setTransferFunction(t);

		return lut;
	}
    
    /**
     * This is a method to export a special int array where the alpha is stored in the most significant byte, then red,
     * green and blue. Note that the transfer function has been applied to it.
     * 
     * @return remappedLUT location where indexLUT will be exported to
     */
    public final int[] exportIndexedLUT() {

        int i;
        int lutHeight;
        int j;
        int nPts = 0;
        float iNew;

        if (type == STRIPED) {
            lutHeight = getExtents()[1];

            for (i = 0; i < lutHeight; i++) {
                remappedLUT[i] = indexedLUT[255 - i];
            }
        } else {
            nPts = transferLine.size();
            lutHeight = getExtents()[1];

            for (j = 0; j < transferLine.size(); j++) {
                x[j] = ((Vector2f) (transferLine.getPoint(j))).X;

            }

            for (i = 0; i < lutHeight; i++) {
                iNew = (float) (x[0] + ( ((float) i / (lutHeight - 1)) * (x[nPts - 1] - x[0])));
                remappedLUT[i] = indexedLUT[(int) (transferLine.getRemappedValue(iNew, lutHeight) + 0.5f)];
            }
        }

        return (remappedLUT);
    }

    /**
     * @param kLUT model lut
     * @param kTransferLine transfer function
     * @param iHeight lut height
     * @param iTable
     */
    public static void exportIndexedLUTMin(ModelLUT kLut, byte[] remappedLUTMin ) {
        TransferFunction kTransferLine = kLut.getTransferFunction();

        //byte[] remappedLUTMin = null;
        int remappedValue;
        int count = 0;
        int nPts = kTransferLine.size();
        float xMax = ((Vector2f) (kTransferLine.getPoint(nPts - 1))).X;
        float xMin = ((Vector2f) (kTransferLine.getPoint(0))).X;
        float fNew;

        int lutHeight = remappedLUTMin.length/4; //kLut.getExtents()[1];
        //remappedLUTMin = new byte[lutHeight * 4];
        for (int i = 0; i < lutHeight; i++) {
            fNew = (float) (xMin + ( ((float) i / (lutHeight - 1)) * (xMax - xMin)));
            remappedValue = kLut.indexedLUT[(int) (kTransferLine.getRemappedValue(fNew, lutHeight) + 0.5f)];
            remappedLUTMin[count++] = (byte) ( (remappedValue & 0x00ff0000) >> 16);
            remappedLUTMin[count++] = (byte) ( (remappedValue & 0x0000ff00) >> 8);
            remappedLUTMin[count++] = (byte) ( (remappedValue & 0x000000ff));
            //remappedLUTMin[count++] = (byte) ( (remappedValue & 0xff000000) >> 24);
            count++;
        }
        //return remappedLUTMin;
    }

    /**
     */
    public static void exportIndexedLUTMin(ModelRGB kRGBT, byte[] remappedLUTMin ) {
        TransferFunction kTransferLineR = kRGBT.getRedFunction();
        TransferFunction kTransferLineG = kRGBT.getGreenFunction();
        TransferFunction kTransferLineB = kRGBT.getBlueFunction();
        int[] iTable = kRGBT.exportIndexedRGB();

        int nPtsR = kTransferLineR.size();
        int nPtsG = kTransferLineG.size();
        int nPtsB = kTransferLineB.size();
        float xMaxR = kTransferLineR.getPoint(nPtsR - 1).X;
        float xMinR = kTransferLineR.getPoint(0).X;
        
        float xMaxG = kTransferLineG.getPoint(nPtsG - 1).X;
        float xMinG = kTransferLineG.getPoint(0).X;

        float xMaxB = kTransferLineB.getPoint(nPtsB - 1).X;
        float xMinB = kTransferLineB.getPoint(0).X;

        int remappedValue;
        int count = 0;
        float fNewR, fNewG, fNewB;

        int lutHeight = remappedLUTMin.length/4;
        //byte[] remappedLUTMin = new byte[lutHeight * 4];
        for (int i = 0; i < lutHeight; i++) {
            fNewR = (float) (xMinR + ( ((float) i / (lutHeight - 1)) * (xMaxR - xMinR)));
            fNewG = (float) (xMinG + ( ((float) i / (lutHeight - 1)) * (xMaxG - xMinG)));
            fNewB = (float) (xMinB + ( ((float) i / (lutHeight - 1)) * (xMaxB - xMinB)));

            remappedValue = iTable[(int) (kTransferLineR.getRemappedValue(fNewR, lutHeight) + 0.5f)];
            remappedLUTMin[count++] = (byte) ( (remappedValue & 0x00ff0000) >> 16);

            remappedValue = iTable[(int) (kTransferLineG.getRemappedValue(fNewG, lutHeight) + 0.5f)];
            remappedLUTMin[count++] = (byte) ( (remappedValue & 0x0000ff00) >> 8);

            remappedValue = iTable[(int) (kTransferLineB.getRemappedValue(fNewB, lutHeight) + 0.5f)];
            remappedLUTMin[count++] = (byte) ( (remappedValue & 0x000000ff));

            // Alpha
            //remappedLUTMin[count++] = (byte) (255);
            count++;
        }
        //return remappedLUTMin;
    }

    /**
     * This is a method to export a special int array where the alpha is stored in the most significant byte, then red,
     * green and blue. Without the transfer function applied to the LUT
     * 
     * @param arrayLUT location where indexLUT will be exported.
     * 
     * @return int 0 indictes error, and 1 indicates successful completion
     */
    public final int exportIndexedLUT(int[] arrayLUT) {

        if (arrayLUT.length >= indexedLUT.length) {
            System.arraycopy(indexedLUT, 0, arrayLUT, 0, indexedLUT.length);

            return 1;
        } else {
            return 0;
        }
    }

    /**
     * This is a method to export a 2D float array of the LUT. This method is used by the ViewJComponent show methods to
     * blend between two images.
     * 
     * @param applyAlpha flag indicating if the LUT's alpha values should be applied
     * 
     * @return float returns the RGB LUT a 2D array
     */
    public final float[][] exportRGB_LUT(boolean applyAlpha) {

        int i;
        int lutHeight;
        float[][] lut = null;

        lutHeight = getExtents()[1];

        try {
            lut = new float[3][getExtents()[1]];
        } catch (OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("Out of memory: ModelLUT: exportRGB_LUT");

            return null;
        }

        lutHeight = getExtents()[1];

        for (i = 0; i < lutHeight; i++) {
            lut[0][i] = getFloat(1, i);
            lut[1][i] = getFloat(2, i);
            lut[2][i] = getFloat(3, i);
        }

        return (lut);
    }

    /**
     * Accessor that returns the alpha transfer function.
     * 
     * @return the transfer function that describes how to map the alpha values
     */
    public TransferFunction getAlphaFunction() {
        return alphaLine;
    }

    /**
     * Accessor that returns the blue transfer function.
     * 
     * @return the transfer function that describes how to map the blue values
     */
    public TransferFunction getBlueFunction() {
        return blueLine;
    }

    /**
     * Gets a specific index of the LUT.
     * 
     * @param index index of the LUT, normally 0-255
     * 
     * @return LUTcolor color at index
     */
    public Color getColor(int index) {
        int r, g, b;

        r = getInt(1, index); // get red channel
        g = getInt(2, index); // get green channel
        b = getInt(3, index); // get blue channel

        return (new Color(r, g, b));
    }

    /**
     * Accessor that returns the green transfer function.
     * 
     * @return the transfer function that describes how to map the green values
     */
    public TransferFunction getGreenFunction() {
        return greenLine;
    }

    /**
     * Accessor to get LUT type.
     * 
     * @return LUT type
     */
    public int getLUTType() {
        return type;
    }

    /**
     * Accessor that returns the red transfer function.
     * 
     * @return the transfer function that describes how to map the red values
     */
    public TransferFunction getRedFunction() {
        return redLine;
    }

    /**
     * Accessor that returns the transfer function.
     * 
     * @return the transfer function that describes how to remap the image intensities into display values from the LUT.
     */
    public TransferFunction getTransferFunction() {
        return transferLine;
    }

    /**
     * Inverts the LUT using the values stored in the band arrays and not the transfer functions.
     */
    public void invertLUT() {

        int index, index2, height;
        int[] extents = null;
        int[] tmpIndexLUT = null;
        float[] tmpLUT = null;
        float[] tmp2LUT = null;

        extents = getExtents();
        height = 4 * extents[1]; // Get the height of LUT array;

        try {
            tmpIndexLUT = new int[extents[1]];
            tmpLUT = new float[height];
            tmp2LUT = new float[height];
        } catch (OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("Out of memory: ModelLUT: invertLUT");

            return;
        }

        try {
            exportData(0, height, tmpLUT);
        } catch (IOException error) {
            System.out.println("ModelLUT.invertLUT export: " + error);

            return;
        }

        for (index = 0, index2 = height - 4; index < height; index += 4, index2 -= 4) {
            tmp2LUT[index2] = tmpLUT[index]; // alpha
            tmp2LUT[index2 + 1] = tmpLUT[index + 1]; // R
            tmp2LUT[index2 + 2] = tmpLUT[index + 2]; // G
            tmp2LUT[index2 + 3] = tmpLUT[index + 3]; // B
        }

        try {
            importData(0, tmp2LUT, true);
        } catch (IOException error) {
            System.out.println("ModelLUT.invertLUT import: " + error);

            return;
        }

        height = extents[1];

        for (index = 0; index < height; index++) {
            tmpIndexLUT[height - 1 - index] = indexedLUT[index];
        }

        for (index = 0; index < height; index++) {
            indexedLUT[index] = tmpIndexLUT[index];
        }
    }

    /**
     * Creates the R, G, and B transfer functions to produce a blue scale LUT.
     */
    public void makeBlueTransferFunctions() {

        float[] x = null;
        float[] y = null;
        float[] z = null;

        try {
            x = new float[3];
            y = new float[3];
            z = new float[3];
        } catch (OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("Out of memory: ModelLUT:makeBlueTransferFunctions");

            return;
        }

        int height;
        height = getExtents()[1];

        x[0] = 0;
        y[0] = height - 1;
        z[0] = 0;
        x[1] = (height - 1) / 2.0f;
        y[1] = (height - 1) / 2.0f;
        z[1] = 0;
        x[2] = height - 1;
        y[2] = 0;
        z[2] = 0;

        blueLine.importArrays(x, y, 3);

        x[0] = 0;
        y[0] = height - 1;
        z[0] = 0;
        x[1] = 0;
        y[1] = height - 1;
        z[1] = 0;
        x[2] = 0;
        y[2] = height - 1;
        z[2] = 0;

        redLine.importArrays(x, y, 3);
        greenLine.importArrays(x, y, 3);
        resetAlphaLine();

        type = BLUE;
    }

    /**
     * Creates the R, G, and B transfer functions to produce a bone scale LUT.
     */
    public void makeBoneTransferFunctions() {
        float[] x = null;
        float[] y = null;
        float[] z = null;

        try {
            x = new float[6];
            y = new float[6];
            z = new float[6];
        } catch (OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("Out of memory: ModelLUT: makeBoneTransferFunctions");

            return;
        }

        x[0] = 0;
        y[0] = 255;
        z[0] = 0;
        x[1] = 241;
        y[1] = 45;
        z[1] = 0;
        x[2] = 255;
        y[2] = 0;
        z[2] = 0;

        blueLine.importArrays(x, y, 3);

        x[0] = 0;
        y[0] = 255;
        z[0] = 0;
        x[1] = 169;
        y[1] = 45;
        z[1] = 0;
        x[2] = 255;
        y[2] = 0;
        z[2] = 0;

        greenLine.importArrays(x, y, 3);

        x[0] = 0;
        y[0] = 255;
        z[0] = 0;
        x[1] = 139;
        y[1] = 2;
        z[1] = 0;
        x[2] = 255;
        y[2] = 0;
        z[2] = 0;
        x[3] = 255;
        y[3] = 0;
        z[3] = 0;

        redLine.importArrays(x, y, 4);
        resetAlphaLine();
        type = BONE;
    }

    /**
     * Creates the R, G, and B transfer functions to produce a "Cool hot" scale LUT. (blue -> pink -> red -> orange ->
     * yellow -> white)
     */
    public void makeCoolHotTransferFunctions() {

        float[] x = null;
        float[] y = null;
        float[] z = null;

        try {
            x = new float[6];
            y = new float[6];
            z = new float[6];
        } catch (OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("Out of memory: ModelLUT: makeCoolHotTransferFunctions");

            return;
        }

        x[0] = 0;
        y[0] = 255;
        z[0] = 0;
        x[1] = 48;
        y[1] = 0;
        z[1] = 0;
        x[2] = 112;
        y[2] = 255;
        z[2] = 0;
        x[3] = 184;
        y[3] = 255;
        z[3] = 0;
        x[4] = 255;
        y[4] = 0;
        z[4] = 0;

        blueLine.importArrays(x, y, 5);

        x[0] = 0;
        y[0] = 255;
        z[0] = 0;
        x[1] = 112;
        y[1] = 255;
        z[1] = 0;
        x[2] = 184;
        y[2] = 0;
        z[2] = 0;
        x[3] = 255;
        y[3] = 0;
        z[3] = 0;

        greenLine.importArrays(x, y, 4);

        x[0] = 0;
        y[0] = 255;
        z[0] = 0;
        x[1] = 48;
        y[1] = 255;
        z[1] = 0;
        x[2] = 112;
        y[2] = 0;
        z[2] = 0;
        x[3] = 255;
        y[3] = 0;
        z[3] = 0;

        redLine.importArrays(x, y, 4);
        resetAlphaLine();
        type = COOLHOT;
    }

    /**
     * Makes a LUT specifically for viewing CT thigh images
     * 
     */
    public void makeCTThighTransferFunctions() {
        float[] x = null;
        float[] y = null;
        float[] z = null;

        try {
            x = new float[7];
            y = new float[7];
            z = new float[7];
        } catch (OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("Out of memory: ModelLUT: makeGrayBRTransferFunctions");

            return;
        }

        int height;
        height = getExtents()[1];

        x[0] = 0;
        y[0] = height - 1;
        z[0] = 0;

        x[1] = 1;
        y[1] = 0;
        z[1] = 0;

        x[2] = 2;
        y[2] = 0;
        z[2] = 0;

        x[3] = 2;
        y[3] = height - 1;
        z[3] = 0;

        x[4] = 254;
        y[4] = 0;
        z[4] = 0;

        x[5] = 254;
        y[5] = height - 1;
        z[5] = 0;

        x[6] = 255;
        y[6] = height - 1;
        z[6] = 0;

        blueLine.importArrays(x, y, 7);

        x[0] = 0;
        y[0] = height - 1;
        z[0] = 0;

        x[1] = 2;
        y[1] = height - 1;
        z[1] = 0;

        x[2] = 254;
        y[2] = 0;
        z[2] = 0;

        x[3] = 254;
        y[3] = height - 1;
        z[3] = 0;

        x[4] = 255;
        y[4] = height - 1;
        z[4] = 0;

        greenLine.importArrays(x, y, 5);

        x[0] = 0;
        y[0] = height - 1;
        z[0] = 0;

        x[1] = 2;
        y[1] = height - 1;
        z[1] = 0;

        x[2] = 254;
        y[2] = 0;
        z[2] = 0;

        x[3] = 255;
        y[3] = 0;
        z[3] = 0;

        redLine.importArrays(x, y, 4);
        resetAlphaLine();
        type = GRAY_BR;
    }

    /**
     * Creates the R, G, and B transfer functions to produce a gray scale LUT.
     */
    public void makeGrayBRTransferFunctions() {

        float[] x = null;
        float[] y = null;
        float[] z = null;

        try {
            x = new float[6];
            y = new float[6];
            z = new float[6];
        } catch (OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("Out of memory: ModelLUT: makeGrayBRTransferFunctions");

            return;
        }

        int height;
        height = getExtents()[1];

        x[0] = 0;
        y[0] = 0;
        z[0] = 0;

        x[1] = 1;
        y[1] = 0;
        z[1] = 0;

        x[2] = 1;
        y[2] = height - 1;
        z[2] = 0;

        x[3] = 254;
        y[3] = 0;
        z[3] = 0;

        x[4] = 254;
        y[4] = height - 1;
        z[4] = 0;

        x[5] = 255;
        y[5] = height - 1;
        z[5] = 0;

        blueLine.importArrays(x, y, 6);

        x[0] = 0;
        y[0] = height - 1;
        z[0] = 0;

        x[1] = 1;
        y[1] = height - 1;
        z[1] = 0;

        x[2] = 254;
        y[2] = 0;
        z[2] = 0;

        x[3] = 254;
        y[3] = height - 1;
        z[3] = 0;

        x[4] = 255;
        y[4] = height - 1;
        z[4] = 0;

        greenLine.importArrays(x, y, 5);

        x[0] = 0;
        y[0] = height - 1;
        z[0] = 0;

        x[1] = 1;
        y[1] = height - 1;
        z[1] = 0;

        x[2] = 254;
        y[2] = 0;
        z[2] = 0;

        x[3] = 255;
        y[3] = 0;
        z[3] = 0;

        redLine.importArrays(x, y, 4);
        resetAlphaLine();
        type = GRAY_BR;
    }

    /**
     * Creates the R, G, and B transfer functions to produce a gray scale LUT.
     */
    public void makeGrayTransferFunctions() {

        float[] x = null;
        float[] y = null;
        float[] z = null;

        try {
            x = new float[3];
            y = new float[3];
            z = new float[3];
        } catch (OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("Out of memory: ModelLUT:makeGrayTransferFunctions");

            return;
        }

        int height;
        height = getExtents()[1];

        x[0] = 0;
        y[0] = height - 1;
        z[0] = 0;
        x[1] = (height - 1) / 2.0f;
        y[1] = (height - 1) / 2.0f;
        z[1] = 0;
        x[2] = height - 1;
        y[2] = 0;
        z[2] = 0;

        redLine.importArrays(x, y, 3);
        greenLine.importArrays(x, y, 3);
        blueLine.importArrays(x, y, 3);

        resetAlphaLine();
        type = GRAY;
    }

    /**
     * Creates the R, G, and B transfer functions to produce a green scale LUT.
     */
    public void makeGreenTransferFunctions() {

        float[] x = null;
        float[] y = null;
        float[] z = null;

        try {
            x = new float[3];
            y = new float[3];
            z = new float[3];
        } catch (OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("Out of memory: ModelLUT:makeGreenTransferFunctions");

            return;
        }

        int height;
        height = getExtents()[1];

        x[0] = 0;
        y[0] = height - 1;
        z[0] = 0;
        x[1] = (height - 1) / 2.0f;
        y[1] = (height - 1) / 2.0f;
        z[1] = 0;
        x[2] = height - 1;
        y[2] = 0;
        z[2] = 0;

        greenLine.importArrays(x, y, 3);

        x[0] = 0;
        y[0] = height - 1;
        z[0] = 0;
        x[1] = 0;
        y[1] = height - 1;
        z[1] = 0;
        x[2] = 0;
        y[2] = height - 1;
        z[2] = 0;

        redLine.importArrays(x, y, 3);
        blueLine.importArrays(x, y, 3);

        resetAlphaLine();

        type = GREEN;
    }

    /**
     * Creates the R, G, and B transfer functions to produce a spectrum scale LUT. (black -> red -> orange -> yellow ->
     * white)
     */
    public void makeHotMetalTransferFunctions() {

        float[] x = null;
        float[] y = null;
        float[] z = null;

        try {
            x = new float[6];
            y = new float[6];
            z = new float[6];
        } catch (OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("Out of memory: ModelLUT: makeHotMetalTransferFunctions");

            return;
        }

        x[0] = 0;
        y[0] = 255;
        z[0] = 0;
        x[1] = 190;
        y[1] = 255;
        z[1] = 0;
        x[2] = 255;
        y[2] = 0;
        z[2] = 0;

        blueLine.importArrays(x, y, 3);

        x[0] = 0;
        y[0] = 255;
        z[0] = 0;
        x[1] = 95;
        y[1] = 255;
        z[1] = 0;
        x[2] = 190;
        y[2] = 0;
        z[2] = 0;
        x[3] = 255;
        y[3] = 0;
        z[3] = 0;

        greenLine.importArrays(x, y, 4);

        x[0] = 0;
        y[0] = 255;
        z[0] = 0;
        x[1] = 95;
        y[1] = 0;
        z[1] = 0;
        x[2] = 255;
        y[2] = 0;
        z[2] = 0;

        redLine.importArrays(x, y, 3);
        resetAlphaLine();
        type = HOTMETAL;
    }

    /**
     * Special LUT to be used to display java image. Assumes RGB values that range between (0 and 255) are stored in the
     * LUT;
     * 
     * @param opacityArray DOCUMENT ME!
     */
    public void makeIndexedLUT(int[] opacityArray) {
        int index;
        int height;
        float alpha;
        int r, g, b;

        height = getExtents()[1]; // Get the height of LUT array;

        if ( (indexedLUT == null) || (indexedLUT.length != height)) {

            try {
                indexedLUT = new int[height]; // Special LUT to be used to display image
                remappedLUT = new int[height];
            } catch (OutOfMemoryError error) {
                System.gc();
                MipavUtil.displayError("Out of memory: ModelLUT: makeIndexedLUT");

                return;
            }
        }

        for (index = 0; index < height; index++) {

            if (opacityArray != null) {
                alpha = opacityArray[index] / 255.0f;
            } else {
                alpha = getInt(0, index);
            }

            r = (int) ( (alpha * getFloat(1, index)) + 0.5f);
            g = (int) ( (alpha * getFloat(2, index)) + 0.5f);
            b = (int) ( (alpha * getFloat(3, index)) + 0.5f);

            // indexedLUT[index] = (255 << 24) | //set Java's alpha value always to 255
            // (getInt(1,index) << 16) |
            // (getInt(2,index) << 8) |
            // getInt(3,index);
            indexedLUT[index] = (255 << 24) | (r << 16) | (g << 8) | b; // set Java's alpha value always to 255
        }
    }

    /**
     * This method uses the A, R, G, B transfer functions to build the desired LUT.
     * 
     * @param _nColors indicates the number of colors to used in the LUT.
     */

    public void makeLUT(int _nColors) {

        int nPts;
        float alpha, red, green, blue;
        int i, j;
        int height;
        float step;
        float[] x;
        float[] a, r, g, b;

        if ( (_nColors > 0) && (_nColors <= 256)) {
            nColors = _nColors;
        } else {
            nColors = 256;
        }

        height = getExtents()[1]; // number of entries in the LUT (i.e. 256)

        try {
            a = new float[height];
            r = new float[height];
            g = new float[height];
            b = new float[height];

            indexedLUT = new int[height];
            remappedLUT = new int[height];

            // Calculate LUT values per band from its corresponding transfer function.
            calcBand(redLine, r);
            calcBand(greenLine, g);
            calcBand(blueLine, b);

            nPts = alphaLine.size();
            x = new float[nPts];
        } catch (OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("Out of memory: ModelLUT: makeLUT");

            return;
        }

        for (j = 0; j < alphaLine.size(); j++) {
            x[j] = ((Vector2f) (alphaLine.getPoint(j))).X;
        }

        for (i = 0; i < height; i++) {
            a[i] = alphaLine.getRemappedValue(i, height) / (height - 1);
        }

        // remap LUT based on the number of colors
        // if (height >= 256) {
        step = (float) height / nColors;
        j = 0;
        alpha = a[0];
        red = r[0];
        green = g[0];
        blue = b[0];

        for (i = 0; i < height; i++) {

            if (i > (j * step)) {
                alpha = a[i];
                red = r[i];
                green = g[i];
                blue = b[i];
                j++;
            }

            set(0, i, alpha);
            set(1, i, red);
            set(2, i, green);
            set(3, i, blue);
        }

        // make special Java LUT that is an int array where MSB is alpha, and then red, green
        // and blue follow;
        makeIndexedLUT(null);
    }

    /**
     * Creates the R, G, and B transfer functions to produce a red scale LUT.
     */
    public void makeRedTransferFunctions() {

        float[] x = null;
        float[] y = null;
        float[] z = null;

        try {
            x = new float[3];
            y = new float[3];
            z = new float[3];
        } catch (OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("Out of memory: ModelLUT:makeRedTransferFunctions");

            return;
        }

        int height;
        height = getExtents()[1];

        x[0] = 0;
        y[0] = height - 1;
        z[0] = 0;
        x[1] = (height - 1) / 2.0f;
        y[1] = (height - 1) / 2.0f;
        z[1] = 0;
        x[2] = height - 1;
        y[2] = 0;
        z[2] = 0;

        redLine.importArrays(x, y, 3);

        x[0] = 0;
        y[0] = height - 1;
        z[0] = 0;
        x[1] = 0;
        y[1] = height - 1;
        z[1] = 0;
        x[2] = 0;
        y[2] = height - 1;
        z[2] = 0;

        greenLine.importArrays(x, y, 3);
        blueLine.importArrays(x, y, 3);
        resetAlphaLine();

        type = RED;
    }

    /**
     * Creates the R, G, and B transfer functions to produce a skin scale LUT. (blue -> pink -> red -> orange -> yellow ->
     * white)
     */
    public void makeSkinTransferFunctions() {

        float[] x = null;
        float[] y = null;
        float[] z = null;

        try {
            x = new float[6];
            y = new float[6];
            z = new float[6];
        } catch (OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("Out of memory: ModelLUT: makeSkinTransferFunctions");

            return;
        }

        x[0] = 0;
        y[0] = 255;
        z[0] = 0;
        x[1] = 128;
        y[1] = 128;
        z[1] = 0;
        x[2] = 255;
        y[2] = 0;
        z[2] = 0;

        blueLine.importArrays(x, y, 3);

        x[0] = 0;
        y[0] = 255;
        z[0] = 0;
        x[1] = 126;
        y[1] = 121;
        z[1] = 0;
        x[2] = 255;
        y[2] = 0;
        z[2] = 0;

        greenLine.importArrays(x, y, 3);

        x[0] = 0;
        y[0] = 255;
        z[0] = 0;
        x[1] = 109;
        y[1] = 84;
        z[1] = 0;
        x[2] = 255;
        y[2] = 0;
        z[2] = 0;

        redLine.importArrays(x, y, 3);
        resetAlphaLine();
        type = SKIN;
    }

    /**
     * Creates the R, G, and B transfer functions to produce a spectrum scale LUT. (blue -> light blue -> green ->
     * yellow -> orange -> red)
     */
    public void makeSpectrumTransferFunctions() {

        float[] x = null;
        float[] y = null;
        float[] z = null;

        try {
            x = new float[6];
            y = new float[6];
            z = new float[6];
        } catch (OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("Out of memory: ModelLUT: makeSpectrumTransferFunctions");

            return;
        }

        x[0] = 0;
        y[0] = 127;
        z[0] = 0;
        x[1] = 32;
        y[1] = 0;
        z[1] = 0;
        x[2] = 96;
        y[2] = 0;
        z[2] = 0;
        x[3] = 160;
        y[3] = 255;
        z[3] = 0;
        x[4] = 255;
        y[4] = 255;
        z[4] = 0;

        blueLine.importArrays(x, y, 5);

        x[0] = 0;
        y[0] = 255;
        z[0] = 0;
        x[1] = 32;
        y[1] = 255;
        z[1] = 0;
        x[2] = 96;
        y[2] = 0;
        z[2] = 0;
        x[3] = 160;
        y[3] = 0;
        z[3] = 0;
        x[4] = 224;
        y[4] = 255;
        z[4] = 0;
        x[5] = 255;
        y[5] = 255;
        z[5] = 0;

        greenLine.importArrays(x, y, 6);

        x[0] = 0;
        y[0] = 255;
        z[0] = 0;
        x[1] = 96;
        y[1] = 255;
        z[1] = 0;
        x[2] = 160;
        y[2] = 0;
        z[2] = 0;
        x[3] = 224;
        y[3] = 0;
        z[3] = 0;
        x[4] = 255;
        y[4] = 127;
        z[4] = 0;

        redLine.importArrays(x, y, 5);
        resetAlphaLine();
        type = SPECTRUM;
    }

    /**
     * makeStripedLUT -
     */
    public void makeStripedLUT() {

        int i;
        Color color;
        int height;

        nColors = 256;
        height = getExtents()[1]; // number of entries in the LUT (i.e. 256)

        try {
            indexedLUT = new int[height];
            remappedLUT = new int[height];
        } catch (OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("Out of memory: ModelLUT: makeLUT");

            return;
        }

        set(0, 0, 1);
        set(1, 0, 0);
        set(2, 0, 0);
        set(3, 0, 0);

        float sat = 1.0f;
        float bri = 1.0f;
        int m = 53;

        for (i = 0; i < (height - 1); i++) {
            color = Color.getHSBColor( ( (i * m) % 360) / 360.0f, sat, bri);
            set(0, i + 1, 1);
            set(1, i + 1, color.getRed());
            set(2, i + 1, color.getGreen());
            set(3, i + 1, color.getBlue());

            if (i != 0) {

                if ( (i % (360 / m)) == 0) {

                    if ( (sat > 0.5f) && (bri > 0.5f)) {
                        sat = sat - 0.25f;
                        bri = bri - 0.25f;
                    } else {
                        sat = 1.0f;
                        bri = 1.0f;
                    }
                }
            }
        }

        type = STRIPED;

        // make special Java LUT that is an int array where MSB is alpha, and then red, green
        // and blue follow;
        makeIndexedLUT(null);
    }

    /**
     * makeVR with customized LUT
     */
    public void makeCustomizedLUT(String name) {
        nColors = 256;
        int height = getExtents()[1]; // number of entries in the LUT (i.e. 256)

        try {
            indexedLUT = new int[height];
            remappedLUT = new int[height];
        } catch (OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("Out of memory: ModelLUT: makeLUT");

            return;
        }

        try {
            BufferedReader in = openLUTFile(name);
            String str;
            int i = 0;

            while ( (str = in.readLine()) != null) {
                // System.err.print(str + " ==> " );
                set(0, i, 1);
                java.util.StringTokenizer st = new java.util.StringTokenizer(str);
                if (st.hasMoreTokens()) {
                    int iValue = Integer.valueOf(st.nextToken()).intValue();
                    // System.err.print(iValue + " " );
                    set(1, i, iValue);
                }
                if (st.hasMoreTokens()) {
                    int iValue = Integer.valueOf(st.nextToken()).intValue();
                    // System.err.print(iValue + " " );
                    set(2, i, iValue);
                }
                if (st.hasMoreTokens()) {
                    int iValue = Integer.valueOf(st.nextToken()).intValue();
                    // System.err.println(iValue + " " );
                    set(3, i, iValue);
                }
                i++;
            }
            in.close();
        } catch (IOException e) {}

        // make special Java LUT that is an int array where MSB is alpha, and then red, green
        // and blue follow;
        makeIndexedLUT(null);
    }
    
    

    /**
     * The purpose of this method is to adjust the zero index of the LUT from (1, 1, 1) to (0, 0, 0) The reason is so
     * image A and image B are not blended throughout by default.
     */
    public void oneToZeroLUTAdjust() {
        Color zeroIndexColor = getColor(0);

        // test to see if the color is R == 0, G == 0, B == 0
        boolean zeroIndexColorIs000 = ( (zeroIndexColor.getRed() == 1) && (zeroIndexColor.getGreen() == 1) && (zeroIndexColor
                .getBlue() == 1));

        // only change index 0 to 1's if index 0 is currently R == 0, G == 0, B == 0.
        if (zeroIndexColorIs000 == true) {
            setColor(0, new Color(0, 0, 0));
        }
    }

    /**
     * Resets the alpha function to be linear.
     */
    public void resetAlphaLine() {
        int height;
        float[] x = null;
        float[] y = null;
        float[] z = null;

        try {
            x = new float[3];
            y = new float[3];
            z = new float[3];
        } catch (OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("Out of memory: ModelLUT: resetAlphaLine");

            return;
        }

        height = getExtents()[1];

        x[0] = 0;
        y[0] = 0;
        z[0] = 0;

        x[1] = (height - 1) / 2.0f;
        y[1] = 0;
        z[1] = 0;

        x[2] = height - 1;
        y[2] = 0;
        z[2] = 0;

        alphaLine.importArrays(x, y, 3);
    }

    /**
     * Resets the transfer function to be linear.
     * 
     * @param min DOCUMENT ME!
     * @param max DOCUMENT ME!
     */
    public void resetTransferLine(float min, float max) {
        this.resetTransferLine(min, min, max, max);
    }

    /**
     * Resets the transfer function to be linear.
     * 
     * @param min float the minimum for the data range of this image type
     * @param imgMin float the actual minimum data value for the image
     * @param max float the maximum for the data range of this image type
     * @param imgMax float the actual maximum data value for the image
     */
    public void resetTransferLine(float min, float imgMin, float max, float imgMax) {
        int height;
        float[] x = null;
        float[] y = null;
        float[] z = null;

        try {
            x = new float[4];
            y = new float[4];
            z = new float[4];
        } catch (OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("Out of memory: ModelLUT: resetTransferLine");

            return;
        }

        height = getExtents()[1];

        x[0] = min;
        y[0] = height - 1;
        z[0] = 0;

        // if both min == imgMin and max==imgMax, then
        // divide the 2, and 3rd points by 3rds
        if ( (min == imgMin) && (max == imgMax)) {
            x[1] = min + ( (max - min) / 3.0f);
            y[1] = (height - 1) - ( (height - 1) / 3.0f);
            z[1] = 0;

            x[2] = min + ( (max - min) * 2.0f / 3.0f);
            y[2] = (height - 1) - ( ( (height - 1) * 2.0f) / 3.0f);
            z[2] = 0;
        } // else if only min = imgMin and max != imgMax, then pt 3

        // should be set for imgMax, and pt 2 should be 1/2 between pts 1 and 3
        //
        else if (min == imgMin) {
            x[1] = (imgMin + imgMax) / 2.0f;
            y[1] = (height - 1) / 2.0f;
            z[1] = 0;

            x[2] = imgMax;
            y[2] = 0;
            z[2] = 0;
        } // else if only max = imgMax and min != imgMin, then pt 2

        // should be set for imgMin, and pt 3 should be 1/2 between pts 2 and 4
        //
        else if (max == imgMax) {
            x[1] = imgMin;
            y[1] = height - 1;
            z[1] = 0;

            x[2] = (imgMin + imgMax) / 2.0f;
            y[2] = (height - 1) / 2.0f;
            z[2] = 0;
        } // else, neither imgMin nor imgMax equals min or max.. so use

        // these values for points 2 and 3.
        else {
            x[1] = imgMin;
            y[1] = height - 1;
            z[1] = 0;

            x[2] = imgMax;
            y[2] = 0;
            z[2] = 0;
        }

        // last point is always the actual type range
        x[3] = max; // - (max-min)/255.0f;
        y[3] = 0;
        z[3] = 0;

        transferLine.importArrays(x, y, 4);
    }

    /**
     * Sets a specific index of the LUT with the given color.
     * 
     * @param index index of the LUT, normally 0-255
     * @param LUTcolor color to be placed at the LUT
     */
    public void setColor(int index, Color LUTcolor) {

        set(0, index, 1.0f); // set alpha value to 1.0
        set(1, index, LUTcolor.getRed()); // set red channel
        set(2, index, LUTcolor.getGreen()); // set green channel
        set(3, index, LUTcolor.getBlue()); // set blue channel
        indexedLUT[index] = (255 << 24) | (LUTcolor.getRed() << 16) | (LUTcolor.getGreen() << 8) | LUTcolor.getBlue();

    }

    /**
     * Sets a specific index of the LUT with the given color and updates the compressed LUT.
     * 
     * @param index int index of the LUT
     * @param alpha int 0-255 alpha
     * @param red int 0-255 red
     * @param green int 0-255 green
     * @param blue int 0-255 blue
     */
    public void setColor(int index, int alpha, int red, int green, int blue) {
        set(0, index, alpha); // set alpha value to 1.0
        set(1, index, red); // set red channel
        set(2, index, green); // set green channel
        set(3, index, blue); // set blue channel
        indexedLUT[index] = (255 << 24) | (red << 16) | (green << 8) | blue;
    }

    /**
     * Sets the LUTs transfer function.
     * 
     * @param txFunction DOCUMENT ME!
     */
    public void setTransferFunction(TransferFunction txFunction) {
        transferLine = txFunction;
    }

    /**
     * displays the LUT colours and alpha values in hex.
     * 
     * @return DOCUMENT ME!
     */
    public String toString() {
        return toString(true, true);
    }

    /**
     * Presents the Lookuptable by.
     * 
     * @param displayInHex DOCUMENT ME!
     * @param displayAlphaValues DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    public String toString(boolean displayInHex, boolean displayAlphaValues) {
        StringBuffer outStr = new StringBuffer("ModelLUT: ");
        int height = getExtents()[1];

        for (int i = 0; i < height; i++) {

            // if (i%0x08 == 0) {
            // outStr.append("\n");
            // }
            String alphaValue = "";

            if (displayInHex) {

                if (displayAlphaValues) {
                    alphaValue = Integer.toHexString(get(0, i).intValue()) + ", ";
                }

                outStr.append("[" + alphaValue + Integer.toHexString(get(1, i).intValue()) + ", "
                        + Integer.toHexString(get(2, i).intValue()) + ", " + Integer.toHexString(get(3, i).intValue())
                        + "]");
            } else { // display as Decimal

                if (displayAlphaValues) {
                    alphaValue = Integer.toString(get(0, i).intValue()) + ", ";
                }

                outStr.append("[" + alphaValue + Integer.toString(get(1, i).intValue()) + ", "
                        + Integer.toString(get(2, i).intValue()) + ", " + Integer.toString(get(3, i).intValue()) + "]");
            }

            outStr.append("\n");
        }

        return outStr.toString();
    }

    /**
     * The purpose of this method is to adjust the zero index of the LUT from (0, 0, 0) to (1, 1, 1) The reason is so
     * image A and image B are blended throughout by default.
     */
    public void zeroToOneLUTAdjust() {

        Color zeroIndexColor = getColor(0);

        // test to see if the color is R == 0, G == 0, B == 0
        boolean zeroIndexColorIs000 = ( (zeroIndexColor.getRed() == 0) && (zeroIndexColor.getGreen() == 0) && (zeroIndexColor
                .getBlue() == 0));

        // only change index 0 to 1's if index 0 is currently R == 0, G == 0, B == 0.
        if (zeroIndexColorIs000 == true) {
            setColor(0, new Color(1, 1, 1));
        }
    }

    /**
     * Calculates the color band (i.e. red, green, blue) for the LUT using the the corresponding transfer function
     * 
     * @param function the band's transfer function
     * @param band storage location after conversion from transfer function to the band
     */
    private void calcBand(TransferFunction function, float[] band) {
        int i, j;
        int height = getExtents()[1]; // number of entries in the LUT (i.e. 256)

        for (j = 0; j < function.size(); j++) {
            x[j] = ((Vector2f) (function.getPoint(j))).X;
        }

        for (i = 0; i < height; i++) {
            band[i] = function.getRemappedValue(i, height) / height * 255;
        }
    }

    public int[] getIndexedLUT() {
        return indexedLUT;
    }
    

    /**
     * Opens and returns a buffered reader for a given custom LUT name.
     * 
     * @param lutName The name of the LUT file (without the extension).
     * 
     * @return A LUT file buffered reader.
     */
    public static final String customLUTsLocation = "WildMagic/Shaders/LUTs";
    public static final BufferedReader openLUTFile(String lutName) throws IOException {
        String filename = customLUTsLocation + "/" + lutName + ".txt";

        // use this long call instead of ClassLoader.getSystemResource() to work properly from a jnlp launch
        URL fileURL = Thread.currentThread().getContextClassLoader().getResource(filename);

        if (fileURL == null) {
            Preferences.debug("Unable to open " + filename + ".\n", Preferences.DEBUG_MINOR);
            return null;
        }

        // use buffering this implementation reads one line at a time
        return new BufferedReader(new InputStreamReader(fileURL.openStream()));
    }
}
