package gov.nih.mipav.model.algorithms.utilities;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.dialogs.JDialogReplaceValue.*;

import java.util.*;


/**
 * <p>Title: AlgorithmReplaceValue</p>
 *
 * <p>Description: Replaces the "input" value with the specified "output" value</p>
 *
 * <p>Copyright: Copyright (c) 2004</p>
 *
 * <p>Company:</p>
 *
 * @author   not attributable
 * @version  1.0
 */
public class AlgorithmReplaceValue extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private int colorFactor = 1;

    /** DOCUMENT ME! */
    private Vector<Values> inputRanges = null;

    /** DOCUMENT ME! */
    private double outputVal;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Default contructor.
     *
     * @param  destImage  ModelImage destination image
     * @param  srcImage   ModelImage source image
     * @param  input      double input value to replace
     * @param  output     double value to replace with
     */
    public AlgorithmReplaceValue(ModelImage destImage, ModelImage srcImage, Vector<Values> input, double output) {
        super(destImage, srcImage);

        // this.inputVal = input;
        this.outputVal = output;
        // System.out.println(" input = " + input );

        inputRanges = input;

        if (srcImage.isComplexImage()) {
        	colorFactor = 2;
        }
        else if (srcImage.isColorImage()) {
            colorFactor = 4;
        } 

    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Gets the result image.
     *
     * @return  ModelImage result image
     */
    public ModelImage getResultImage() {
        return this.destImage;
    }


    /**
     * Actually runs the algorithm.
     *
     * @todo  Implement this gov.nih.mipav.model.algorithms.AlgorithmBase method
     */
    public void runAlgorithm() {

        if (this.destImage == null) {
            calcStoreInPlace();
        } else {
            calcStoreInDest();
        }

    }

    /**
     * Replace the values and store into a new image.
     */
    private void calcStoreInDest() {

        int length;
        double[] buffer;
        int z = 1;
        int t = 1;

        int volume = 0;

        length = srcImage.getSliceSize() * colorFactor;
        buffer = new double[length];

        int counter = 0;

        if (srcImage.getExtents().length > 2) {
            z = srcImage.getExtents()[2];
        }

        if (srcImage.getExtents().length > 3) {
            t = srcImage.getExtents()[3];
            volume = length * z;
        }


        int mod = (length * t * z) / 10; // mod is 10 percent of total length

        int tIndex;
        int zIndex;

        int buffIndex;

        int start;

        fireProgressStateChanged(srcImage.getImageName(), "Replacing values...");
        


        int len = inputRanges.size();
        int rangeCounter = 0;

        Values values = null;


        for (tIndex = 0; tIndex < t; tIndex++) {

            for (zIndex = 0; zIndex < z; zIndex++) {
                start = (tIndex * volume) + (zIndex * length);

                try {
                    srcImage.exportData(start, length, buffer);

                    for (buffIndex = 0; buffIndex < length; buffIndex++) {

                        if ((counter % mod) == 0) {
                            fireProgressStateChanged(Math.round((float) counter / ((length * t * z) - 1) * 100));
                        }

                        for (rangeCounter = 0; rangeCounter < len; rangeCounter++) {
                            values = (Values) inputRanges.elementAt(rangeCounter);

                            if (values.isRange) {

                                if ((buffer[buffIndex] >= values.firstVal) && (buffer[buffIndex] <= values.secondVal)) {
                                    buffer[buffIndex] = outputVal;
                                }
                            } else {

                                if (buffer[buffIndex] == values.firstVal) {
                                    buffer[buffIndex] = outputVal;
                                } else if (Double.isNaN(buffer[buffIndex]) && Double.isNaN(values.firstVal)) {
                                    buffer[buffIndex] = outputVal;
                                } else if ((buffer[buffIndex] == Double.POSITIVE_INFINITY) &&
                                               (values.firstVal == Double.POSITIVE_INFINITY)) {
                                    buffer[buffIndex] = outputVal;
                                } else if ((buffer[buffIndex] == Double.NEGATIVE_INFINITY) &&
                                               (values.firstVal == Double.NEGATIVE_INFINITY)) {
                                    buffer[buffIndex] = outputVal;
                                }

                            }
                        }

                        counter++;
                    }

                    destImage.importData(start, buffer, false);

                } catch (Exception e) { }
            }

        }

        destImage.calcMinMax();

        
        setCompleted(true);
    }

    /**
     * Replace the values in place.
     */
    private void calcStoreInPlace() {
        int length;
        double[] buffer;
        int z = 1;
        int t = 1;

        int volume = 0;

        length = srcImage.getSliceSize() * colorFactor;
        buffer = new double[length];


        if (srcImage.getExtents().length > 2) {
            z = srcImage.getExtents()[2];
        }

        if (srcImage.getExtents().length > 3) {
            t = srcImage.getExtents()[3];
            volume = length * z;
        }


        int tIndex;
        int zIndex;

        int buffIndex;

        int start;

        fireProgressStateChanged(srcImage.getImageName(), "Replacing values...");
        

        int mod = (length * t * z) / 10; // mod is 10 percent of total length

        int counter = 0;

        int len = inputRanges.size();
        int rangeCounter = 0;

        Values values = null;

        for (tIndex = 0; tIndex < t; tIndex++) {

            for (zIndex = 0; zIndex < z; zIndex++) {


                start = (tIndex * volume) + (zIndex * length);

                try {
                    srcImage.exportData(start, length, buffer);

                    for (buffIndex = 0; buffIndex < length; buffIndex++) {

                        if ((counter % mod) == 0) {
                            fireProgressStateChanged(Math.round((float) counter / ((length * t * z) - 1) * 100));
                        }

                        for (rangeCounter = 0; rangeCounter < len; rangeCounter++) {
                            values = (Values) inputRanges.elementAt(rangeCounter);

                            if (values.isRange) {

                                if ((buffer[buffIndex] >= values.firstVal) && (buffer[buffIndex] <= values.secondVal)) {
                                    buffer[buffIndex] = outputVal;
                                }
                            } else {

                                if (buffer[buffIndex] == values.firstVal) {
                                    buffer[buffIndex] = outputVal;
                                } else if (Double.isNaN(buffer[buffIndex]) && Double.isNaN(values.firstVal)) {
                                    buffer[buffIndex] = outputVal;
                                } else if ((buffer[buffIndex] == Double.POSITIVE_INFINITY) &&
                                               (values.firstVal == Double.POSITIVE_INFINITY)) {
                                    buffer[buffIndex] = outputVal;
                                } else if ((buffer[buffIndex] == Double.NEGATIVE_INFINITY) &&
                                               (values.firstVal == Double.NEGATIVE_INFINITY)) {
                                    buffer[buffIndex] = outputVal;
                                }

                            }
                        }

                        counter++;
                    }

                    srcImage.importData(start, buffer, false);

                } catch (Exception e) { }
            }
        }

        srcImage.calcMinMax();

        
        setCompleted(true);

    }

}
