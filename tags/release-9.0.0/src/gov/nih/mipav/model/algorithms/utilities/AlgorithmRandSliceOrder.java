package gov.nih.mipav.model.algorithms.utilities;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import java.io.*;

import java.util.*;


/**
 * Algorithm that randomizes the order of 3D dataset.
 *
 * @version  1.0 April 1, 2002
 * @author   Matthew J. McAuliffe, Ph.D.
 */
public class AlgorithmRandSliceOrder extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private int[] randomOrder = null;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructs an algorithm object that randomizes the order of 3D image dataset.
     *
     * @param  srcImg  source image model
     */
    public AlgorithmRandSliceOrder(ModelImage srcImg) {
        super(null, srcImg);
    }

    /**
     * Constructs an algorithm object that randomizes the order of 3D image dataset.
     *
     * @param  srcImg    source image model
     * @param  progress  Progress mode - see AlgorithmBase.
     */
    public AlgorithmRandSliceOrder(ModelImage srcImg, int progress) {
        this(srcImg);
    //    progressMode = progress;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        randomOrder = null;
        super.finalize();
    }

    /**
     * Accessor returns randomOrder.
     *
     * @return  int[]
     */
    public int[] getRandomOrder() {
        return randomOrder;
    }

    /**
     * Starts the program.
     */
    public void runAlgorithm() {

        if (srcImage == null) {
            displayError("RandSliceOrder.run(): Source Image is null");

            return;
        }

        if ((srcImage.getNDims() != 3)) {
            displayError("RandSliceOrder.run(): Source Image is not 3D");

            return;
        }

        
        calcInPlace();
    }

    /**
     * Forms the reversed order image and places the result in original image.
     */
    private void calcInPlace() {

        int i, idx, s;
        int length, threeDLength;
        float[] buffer;
        float[] sliceBuffer;
        int sliceNumber;
        Vector<Integer> vectRand;
        RandomNumberGen randomGen;

        try {
            randomGen = new RandomNumberGen();
            vectRand = new Vector<Integer>();
            sliceNumber = srcImage.getExtents()[2];
            randomOrder = new int[sliceNumber];

            for (int r = 0; r < sliceNumber; r++) {
                vectRand.add(new Integer(r));
            }

            if (srcImage.isColorImage()) {
                length = 4 * srcImage.getSliceSize();
            } else if (srcImage.isComplexImage()) {
            	length = 2 * srcImage.getSliceSize();
            } else {
                length = srcImage.getSliceSize();
            }

            threeDLength = length * srcImage.getExtents()[2];
            buffer = new float[threeDLength];
            sliceBuffer = new float[length];
            fireProgressStateChanged(srcImage.getImageName(), "Reordering image ...");
        } catch (OutOfMemoryError e) {
            buffer = null;
            sliceBuffer = null;
            errorCleanUp("Algorithm Randomize slice order: Out of memory", true);

            return;
        }

        

        for (s = 0; (s < sliceNumber) && !threadStopped; s++) {
            fireProgressStateChanged(Math.round((float) (s) / (sliceNumber - 1) * 100));

            try {
                srcImage.exportData(s * length, length, sliceBuffer); // locks and releases lock
            } catch (IOException error) {
                buffer = null;
                sliceBuffer = null;
                errorCleanUp("Algorithm Randomize slice order: Image(s) locked", true);

                return;
            }

            int randNum = randomGen.genUniformRandomNum(0, vectRand.size() - 1);
            Integer randSlice = (Integer) vectRand.elementAt(randNum);
            vectRand.removeElementAt(randNum);
            randomOrder[randSlice.intValue()] = s;

            for (i = 0, idx = randSlice.intValue() * length; (i < length) && !threadStopped; i++, idx++) {
                buffer[idx] = sliceBuffer[i];
            }
        } // for (s = 0; s < sliceNumber; s++)

        if (threadStopped) {
            buffer = null;
            sliceBuffer = null;
            finalize();

            return;
        }

        // Reorder fileinfo stuff
        FileInfoBase[] fileInfo;
        FileInfoBase[] fileInfoNew = new FileInfoBase[sliceNumber];
        fileInfo = srcImage.getFileInfo();

        int orient = FileInfoBase.oppositeOrient(fileInfo[0].getAxisOrientation(2));

        for (s = 0, idx = sliceNumber - 1; (s < sliceNumber) && !threadStopped; s++, idx--) {
            fileInfoNew[s] = fileInfo[idx];
            fileInfoNew[s].setAxisOrientation(orient, 2);
        }

        for (s = 0; (s < sliceNumber) && !threadStopped; s++) {
            srcImage.setFileInfo(fileInfoNew[s], s);
        }

        if (threadStopped) {
            srcImage.setFileInfo(fileInfo);
            buffer = null;
            sliceBuffer = null;
            finalize();

            return;
        }

        try {
            srcImage.importData(0, buffer, true);
        } catch (IOException error) {
            buffer = null;
            sliceBuffer = null;
            errorCleanUp("Algorithm Randomize slice order: Image(s) locked", false);

            return;
        }

        if (threadStopped) {
            buffer = null;
            sliceBuffer = null;
            finalize();

            return;
        }

        srcImage.calcMinMax();
        
        setCompleted(true);
    }
}
