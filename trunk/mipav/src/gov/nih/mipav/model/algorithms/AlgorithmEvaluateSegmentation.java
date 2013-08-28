package gov.nih.mipav.model.algorithms;


import java.util.BitSet;
import java.text.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.*;


/**
 * Compares segmentation results of a test image to segmentation results of an ideal gold standard true image.
 * Comparisons are made for contour or polyline vois having the same ids in the 2 images. For each id number, the false
 * negative volume fraction, the false positive volume fraction, and the positive volume fraction are output to the
 * global data text.
 */

public class AlgorithmEvaluateSegmentation extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** total number of id voxels in true image. */
    private int absoluteTrue;

    /** where trueImage has id but test image does not. */
    private int falseNegative;

    /** where trueImage does not have id but test image does. */
    private int falsePositive;

    /** false negative volume fraction. */
    private float fnvf;

    /** false positive volume fraction. */
    private float fpvf;

    /** DOCUMENT ME! */
    private int length;

    /** DOCUMENT ME! */
    private int nTestVOIs;

    /** DOCUMENT ME! */
    private int nTrueVOIs;

    /** DOCUMENT ME! */
    private int testID;

    /** DOCUMENT ME! */
    private ModelImage testImage;

    /** DOCUMENT ME! */
    private int testLength;

    /** DOCUMENT ME! */
    private short[] testMask;

    /** DOCUMENT ME! */
    private ViewVOIVector testVOIs;

    /** positive volume fraction. */
    private float tpvf;

    /** number of the absoluteTrue found in the test image. */
    private int trueFound;

    /** DOCUMENT ME! */
    private int trueID;

    /** DOCUMENT ME! */
    private short[] trueMask;

    /** DOCUMENT ME! */
    private ViewVOIVector trueVOIs;


    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new AlgorithmEvaluateSegmentation object.
     *
     * @param  trueImage  image model used as a ideal gold standard
     * @param  testImage  image model tested against the trueImage
     */
    public AlgorithmEvaluateSegmentation(ModelImage trueImage, ModelImage testImage) {
        super(null, trueImage);
        this.testImage = testImage;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {

        testImage = null;
        srcImage = null;
        super.finalize();
    }

    /**
     * Starts the program.
     */
    public void runAlgorithm() {
        int i, j, k;

        if (srcImage == null) {
            displayError("Source Image is null");
            finalize();

            return;
        }

        if (threadStopped) {
            finalize();

            return;
        }

        trueVOIs = srcImage.getVOIs();
        nTrueVOIs = trueVOIs.size();
        testVOIs = testImage.getVOIs();
        nTestVOIs = testVOIs.size();
        length = srcImage.getExtents()[0];

        for (i = 1; i < srcImage.getNDims(); i++) {
            length *= srcImage.getExtents()[i];
        }

        testLength = testImage.getExtents()[0];

        for (i = 1; i < testImage.getNDims(); i++) {
            testLength *= testImage.getExtents()[i];
        }

        if (length != testLength) {
            MipavUtil.displayError(srcImage.getImageName() + " and " + testImage.getImageName() +
                                   " are unequal in dimensions");
            setCompleted(false);

            return;
        }

        trueMask = new short[length];
        testMask = new short[testLength];
        ViewUserInterface.getReference().setGlobalDataText(srcImage.getImageName() + " = true\n");
        ViewUserInterface.getReference().setGlobalDataText(testImage.getImageName() + " = test\n");
        
        //*******************************************************************
        //New additions for Dice's Coefficient and area functionality
        //
        //Displays VOI areas in each slice, the intersected area, and Dice's Coefficient.
        //
        //By Victor Wang
        //
        //*******************************************************************
        
        int testArea, trueArea; //stores the areas of the contours per slice
        int testAreaCumm, trueAreaCumm; //stores the areas of contours per VOI grouping
        int iArea, iAreaCumm; //stores the intersection areas of the contours per slice/VOI grouping
        int testVOIsize, trueVOIsize; //find the total number of contours per VOI grouping
        float dice, diceCumm; //Dice's Coefficient
        String formattedDice; //Dice's Coefficient formatted to 3 decimal points
        boolean truePresent, testPresent; //Checks to see if contours are present in the slice
        VOIContour trueContour, testContour; 
        
        int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];
        int zDim = srcImage.getExtents()[2];
        float[] resolutions = srcImage.getResolutions(0); //image resolution (in units)
        float ratio = resolutions[0]*resolutions[1]; //conversion from pixel to real area
		String units = srcImage.getFileInfo()[0].getAreaUnitsOfMeasureStr(); //image units of measure
        BitSet trueBit = new BitSet(length); //Mask of pixels inside the contours
        BitSet testBit = new BitSet(length);
        DecimalFormat formatter = new DecimalFormat("#.###");
        
        for (i = 0; i < nTrueVOIs; i++) {
     	
        	//Reset cumulative values for each new VOI grouping
        	iAreaCumm = 0;
            testAreaCumm = 0;
            trueAreaCumm = 0;
        	
            //Check to see if VOIs are closed contours, and how many are present in the VOI grouping
        	if (trueVOIs.VOIAt(i).getCurveType() == VOI.CONTOUR)
        		trueVOIsize = trueVOIs.VOIAt(i).getSize();	
        	else trueVOIsize = 0;
        	
        	if (testVOIs.VOIAt(i).getCurveType() == VOI.CONTOUR)
    			testVOIsize = testVOIs.VOIAt(i).getSize();
        	else testVOIsize = 0;

        	//Go slice by slice to determine intersected regions
        	for (int depth = 0; depth < zDim; depth++){
        	
        		trueBit.clear();
            	testBit.clear();
            	iArea = 0;
            	testArea = 0;
            	trueArea = 0;
            	truePresent = false;
            	testPresent = false;
        		
            	//If the contour is in the current slice, add its pixels to the mask
	        	for (int w=0; w<trueVOIsize;w++){	
	        		trueContour = (VOIContour) (trueVOIs.VOIAt(i).getCurves().elementAt(w));
	        		if (trueContour.slice(4) == depth){
	        			trueContour.setMask(trueBit, xDim, yDim , false, 1);
	        			truePresent = true; //At least one contour is present in this slice
	        		}
	        	}
	        	for (int w=0; w<testVOIsize;w++){
	        		testContour = (VOIContour) (testVOIs.VOIAt(i).getCurves().elementAt(w));
	        		if (testContour.slice(4) == depth){
	        			testContour.setMask(testBit, xDim, yDim , false, 1);  
	        			testPresent = true;
	        		}
	        	}

	        	//If no contours are present in this slice, do not calculate anything
	        	if (testPresent && truePresent){
	        		//calculate areas based on the masks
		        	for (int w=0; w<length; w++){	
		        		if (testBit.get(w) == true) 
		        			testArea++;
		        		if (trueBit.get(w) == true)
		        			trueArea++;
		        		//Calculates the intersection of the two masks
		        		if ((testBit.get(w) == true) && (trueBit.get(w) == true)) 
		        			iArea++;
		        	}
	        		
	        		dice = 2*(float)iArea/(float)(testArea+trueArea);
	        		formattedDice = formatter.format(dice); //format to 3 decimal places
	        		
		        	iAreaCumm += iArea;
		        	testAreaCumm += testArea;
		        	trueAreaCumm += trueArea;
		        	
		        	ViewUserInterface.getReference().setGlobalDataText("\nStatistics for VOIs with ID = " 
		        			+ String.valueOf(trueID) + ", Slice = "+ String.valueOf(depth) + "\n");
		        	ViewUserInterface.getReference().setGlobalDataText("     True VOI Area =\n\t" + String.valueOf(trueArea) +
		                    " pixels\n\t" + String.valueOf(trueArea*ratio) + " " + units + "\n");
		        	ViewUserInterface.getReference().setGlobalDataText("     Test VOI Area =\n\t" + String.valueOf(testArea) +
		        			" pixels\n\t" + String.valueOf(testArea*ratio) + " " + units + "\n");
		        	ViewUserInterface.getReference().setGlobalDataText("     Intersection Area =\n\t" + String.valueOf(iArea) +
		        			" pixels\n\t" + String.valueOf(iArea*ratio) + " " + units + "\n");
		        	ViewUserInterface.getReference().setGlobalDataText("     Dice's Coefficient = " + formattedDice +
		        			"\n");
	        	}
        	
        	}//for (int depth = 0; depth < zDim; depth++)
        	
        	//*******************************************************************
        	//end of new additions for Dice's Coefficient
        	//*******************************************************************
        	
            if ((trueVOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) ||
                    (trueVOIs.VOIAt(i).getCurveType() == VOI.POLYLINE)) {
                trueID = trueVOIs.VOIAt(i).getID();

                for (j = 0; j < nTestVOIs; j++) {
                    testID = testVOIs.VOIAt(j).getID();

                    if (trueID == testID) {

                        for (k = 0; k < length; k++) {
                            trueMask[k] = -1;
                            testMask[k] = -1;
                        }

                        trueMask = srcImage.generateVOIMask(trueMask, i);
                        testMask = testImage.generateVOIMask(testMask, j);
                        absoluteTrue = 0;
                        trueFound = 0;
                        falseNegative = 0;
                        falsePositive = 0;

                        for (k = 0; k < length; k++) {

                            if (trueMask[k] == trueID) {
                                absoluteTrue++;

                                if (testMask[k] == trueID) {
                                    trueFound++;
                                } else {
                                    falseNegative++;
                                }
                            } // if (trueMask[k] == trueID)
                            else { // trueMask[k] != trueID

                                if (testMask[k] == trueID) {
                                    falsePositive++;
                                }
                            } // else trueMask[k] != trueID
                        } // for (k = 0; k < length; k++)

                        ViewUserInterface.getReference().setGlobalDataText("\nStatistics for VOIs with ID = " + String.valueOf(trueID) +
                                                        "\n");
                        fnvf = (float) falseNegative / (float) absoluteTrue;
                        ViewUserInterface.getReference().setGlobalDataText("     False negative volume fraction = " +
                                                        String.valueOf(fnvf) + "\n");
                        fpvf = (float) falsePositive / (float) absoluteTrue;
                        ViewUserInterface.getReference().setGlobalDataText("     False positive volume fraction = " +
                                                        String.valueOf(fpvf) + "\n");
                        tpvf = (float) trueFound / (float) absoluteTrue;
                        ViewUserInterface.getReference().setGlobalDataText("     True Positive volume fraction = " + String.valueOf(tpvf) +
                                                        "\n");
                    } // if (trueID == testID)
                } // for (j = 0; j < nTestVOIs; j++)
            } // if ((trueVOIs.VOIAt(i).getCurveType() == VOI.CONTOUR)
            
            //Display cumulative area information from the VOI grouping
            diceCumm = 2*(float)iAreaCumm / (float)(trueAreaCumm + testAreaCumm);
            formattedDice = formatter.format(diceCumm);

            ViewUserInterface.getReference().setGlobalDataText("     True Area (Cumulative) =\n\t" + String.valueOf(trueAreaCumm) +
            		" pixels\n\t" + String.valueOf(trueAreaCumm*ratio) + " " + units + "\n");
        	ViewUserInterface.getReference().setGlobalDataText("     Test Area (Cumulative) =\n\t" + String.valueOf(testAreaCumm) +
        			" pixels\n\t" + String.valueOf(testAreaCumm*ratio) + " " + units + "\n");
        	ViewUserInterface.getReference().setGlobalDataText("     Intersection Area (Cumulative) =\n\t" + String.valueOf(iAreaCumm) +
        			" pixels\n\t" + String.valueOf(iAreaCumm*ratio) + " " + units + "\n");
        	ViewUserInterface.getReference().setGlobalDataText("     Dice's Coefficient (Cumulative) = " + formattedDice +
                    "\n");
        	
        } // for (i = 0; i < nTrueVOIs; i++)

        setCompleted(true);
    }

}
