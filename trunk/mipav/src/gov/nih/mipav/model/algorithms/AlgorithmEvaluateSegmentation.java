package gov.nih.mipav.model.algorithms;

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
    private double absoluteTrue;

    /** where trueImage has id but test image does not. */
    private double falseNegative;

    /** where trueImage does not have id but test image does. */
    private double falsePositive;

    /** false negative volume fraction. */
    private double fnvf;

    /** false positive volume fraction. */
    private double fpvf;

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
    private ViewVOIVector testVOIs;

    /** positive volume fraction. */
    private double tpvf;

    /** number of the absoluteTrue found in the test image. */
    private double trueFound;

    /** DOCUMENT ME! */
    private int trueID;

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
        int i, j, m;

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
        
        double testAreaCumm, trueAreaCumm; //stores the areas of contours per VOI grouping
        double iAreaCumm;
        int testVOIsize, trueVOIsize; //find the total number of contours per VOI grouping
        double dice, diceCumm; //Dice's Coefficient
        String formattedDice; //Dice's Coefficient formatted to 3 decimal points
        boolean truePresent, testPresent; //Checks to see if contours are present in the slice
        VOIContour trueContour, testContour, intersectionContour; 
        
        int zDim = srcImage.getExtents()[2];
        float[] resolutions = srcImage.getResolutions(0); //image resolution (in units)
        float ratio = resolutions[0]*resolutions[1]; //conversion from pixel to real area
		String units = srcImage.getFileInfo()[0].getAreaUnitsOfMeasureStr(); //image units of measure
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
        	
            	truePresent = false;
            	testPresent = false;
            	double exactPartialTruePixelCount = 0.0;
            	double exactTotalTruePixelCount = 0.0;
            	double exactPartialTestPixelCount = 0.0;
            	double exactTotalTestPixelCount = 0.0;
            	double exactPartialIntersectionPixelCount = 0.0;
            	double exactTotalIntersectionPixelCount = 0.0;
            	double term;
            	short subjID = 0;
                String subjName = "subjName";
                VOI subj = new VOI(subjID, subjName, VOI.CONTOUR, -1.0f); 
                short clipID = 1;
                String clipName = "clipName";
                VOI clip = new VOI(clipID, clipName, VOI.CONTOUR, -1.0f);
                short intersectionID = 2;
                String intersectionName = "intersectionName";
                VOI intersection = new VOI(intersectionID, intersectionName, VOI.CONTOUR, -1.0f);
                int intersectionVOISize;
        		
            	//If the contour is in the current slice, add its pixels to the mask
	        	for (int w=0; w<trueVOIsize;w++){	
	        		trueContour = (VOIContour) (trueVOIs.VOIAt(i).getCurves().elementAt(w));
	        		if (trueContour.slice(4) == depth){
	        		    exactPartialTruePixelCount = 0.0;
	        		    for (m = 0; m < trueContour.size()-1; m++) {
	        		        term = (trueContour.elementAt(m).X*trueContour.elementAt(m+1).Y 
	        		                - trueContour.elementAt(m+1).X*trueContour.elementAt(m).Y);
	        	            exactPartialTruePixelCount += term;        
	        		    }
	        		    term =  (trueContour.elementAt(trueContour.size()-1).X*trueContour.elementAt(0).Y 
	        		            - trueContour.elementAt(0).X*trueContour.elementAt(trueContour.size()-1).Y);
	        	        exactPartialTruePixelCount += term;
	        	        exactTotalTruePixelCount += 0.5*Math.abs(exactPartialTruePixelCount);
	        			truePresent = true; //At least one contour is present in this slice
	        			subj.importCurve(trueContour);
	        		}
	        	}
	        	for (int w=0; w<testVOIsize;w++){
	        		testContour = (VOIContour) (testVOIs.VOIAt(i).getCurves().elementAt(w));
	        		if (testContour.slice(4) == depth){
	        		    exactPartialTestPixelCount = 0.0;
	        		    for (m = 0; m < testContour.size()-1; m++) {
                            term = (testContour.elementAt(m).X*testContour.elementAt(m+1).Y 
                                    - testContour.elementAt(m+1).X*testContour.elementAt(m).Y);
                            exactPartialTestPixelCount += term;        
                        }
                        term =  (testContour.elementAt(testContour.size()-1).X*testContour.elementAt(0).Y 
                                - testContour.elementAt(0).X*testContour.elementAt(testContour.size()-1).Y);
                        exactPartialTestPixelCount += term;
                        exactTotalTestPixelCount += 0.5*Math.abs(exactPartialTestPixelCount); 
	        			testPresent = true;
	        			clip.importCurve(testContour);
	        		}
	        	}

	        	//If no contours are present in this slice, do not calculate anything
	        	if (testPresent && truePresent){
	        	    new GenericPolygonClipper(GenericPolygonClipper.gpc_op.GPC_INT, subj, clip, intersection);
	        	    intersectionVOISize = intersection.getSize();
	        	    for (int w=0; w<intersectionVOISize;w++){
	                    intersectionContour = (VOIContour) (intersection.getCurves().elementAt(w));
	                    if (intersectionContour.slice(4) == depth){
	                        exactPartialIntersectionPixelCount = 0.0;
	                        for (m = 0; m < intersectionContour.size()-1; m++) {
	                            term = (intersectionContour.elementAt(m).X*intersectionContour.elementAt(m+1).Y 
	                                    - intersectionContour.elementAt(m+1).X*intersectionContour.elementAt(m).Y);
	                            exactPartialIntersectionPixelCount += term;        
	                        }
	                        term =  (intersectionContour.elementAt(intersectionContour.size()-1).X*intersectionContour.elementAt(0).Y 
	                                - intersectionContour.elementAt(0).X*intersectionContour.elementAt(intersectionContour.size()-1).Y);
	                        exactPartialIntersectionPixelCount += term;
	                        exactTotalIntersectionPixelCount += 0.5*Math.abs(exactPartialIntersectionPixelCount);
	                    }
	                }
	        		
	        		dice = 2*exactTotalIntersectionPixelCount/(exactTotalTestPixelCount+exactTotalTruePixelCount);
	        		formattedDice = formatter.format(dice); //format to 3 decimal places
	        		
		        	iAreaCumm += exactTotalIntersectionPixelCount;
		        	testAreaCumm += exactTotalTestPixelCount;
		        	trueAreaCumm += exactTotalTruePixelCount;
		        	
		        	ViewUserInterface.getReference().setGlobalDataText("\nStatistics for VOIs with ID = " 
		        			+ String.valueOf(trueID) + ", Slice = "+ String.valueOf(depth) + "\n");
		        	ViewUserInterface.getReference().setGlobalDataText("     True VOI Area =\n\t" + String.valueOf(exactTotalTruePixelCount) +
		                    " pixels\n\t" + String.valueOf(exactTotalTruePixelCount*ratio) + " " + units + "\n");
		        	ViewUserInterface.getReference().setGlobalDataText("     Test VOI Area =\n\t" + String.valueOf(exactTotalTestPixelCount) +
		        			" pixels\n\t" + String.valueOf(exactTotalTestPixelCount*ratio) + " " + units + "\n");
		        	ViewUserInterface.getReference().setGlobalDataText("     Intersection Area =\n\t" 
		        			+ String.valueOf(exactTotalIntersectionPixelCount) +
		        			" pixels\n\t" + String.valueOf(exactTotalIntersectionPixelCount*ratio) + " " + units + "\n");
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
                        
                        absoluteTrue = 0;
                        trueFound = 0;
                        falseNegative = 0;
                        falsePositive = 0;
                        
                      //Go slice by slice to determine intersected regions
                        for (int depth = 0; depth < zDim; depth++){
                        
                            truePresent = false;
                            testPresent = false;
                            double exactPartialTruePixelCount = 0.0;
                            double exactTotalTruePixelCount = 0.0;
                            double exactPartialTestPixelCount = 0.0;
                            double exactTotalTestPixelCount = 0.0;
                            double exactPartialIntersectionPixelCount = 0.0;
                            double exactTotalIntersectionPixelCount = 0.0;
                            double term;
                            short subjID = 0;
                            String subjName = "subjName";
                            VOI subj = new VOI(subjID, subjName, VOI.CONTOUR, -1.0f); 
                            short clipID = 1;
                            String clipName = "clipName";
                            VOI clip = new VOI(clipID, clipName, VOI.CONTOUR, -1.0f);
                            short intersectionID = 2;
                            String intersectionName = "intersectionName";
                            VOI intersection = new VOI(intersectionID, intersectionName, VOI.CONTOUR, -1.0f);
                            int intersectionVOISize;
                            
                            //If the contour is in the current slice, add its pixels to the mask
                            for (int w=0; w<trueVOIsize;w++){   
                                trueContour = (VOIContour) (trueVOIs.VOIAt(i).getCurves().elementAt(w));
                                if (trueContour.slice(4) == depth){
                                    exactPartialTruePixelCount = 0.0;
                                    for (m = 0; m < trueContour.size()-1; m++) {
                                        term = (trueContour.elementAt(m).X*trueContour.elementAt(m+1).Y 
                                                - trueContour.elementAt(m+1).X*trueContour.elementAt(m).Y);
                                        exactPartialTruePixelCount += term;        
                                    }
                                    term =  (trueContour.elementAt(trueContour.size()-1).X*trueContour.elementAt(0).Y 
                                            - trueContour.elementAt(0).X*trueContour.elementAt(trueContour.size()-1).Y);
                                    exactPartialTruePixelCount += term;
                                    exactTotalTruePixelCount += 0.5*Math.abs(exactPartialTruePixelCount);
                                    truePresent = true; //At least one contour is present in this slice
                                    subj.importCurve(trueContour);
                                }
                            }
                            for (int w=0; w<testVOIsize;w++){
                                testContour = (VOIContour) (testVOIs.VOIAt(j).getCurves().elementAt(w));
                                if (testContour.slice(4) == depth){
                                    exactPartialTestPixelCount = 0.0;
                                    for (m = 0; m < testContour.size()-1; m++) {
                                        term = (testContour.elementAt(m).X*testContour.elementAt(m+1).Y 
                                                - testContour.elementAt(m+1).X*testContour.elementAt(m).Y);
                                        exactPartialTestPixelCount += term;        
                                    }
                                    term =  (testContour.elementAt(testContour.size()-1).X*testContour.elementAt(0).Y 
                                            - testContour.elementAt(0).X*testContour.elementAt(testContour.size()-1).Y);
                                    exactPartialTestPixelCount += term;
                                    exactTotalTestPixelCount += 0.5*Math.abs(exactPartialTestPixelCount);
                                    testPresent = true;
                                    clip.importCurve(testContour);
                                }
                            }

                            //If no contours are present in this slice, do not calculate anything
                            if (testPresent && truePresent){
                                new GenericPolygonClipper(GenericPolygonClipper.gpc_op.GPC_INT, subj, clip, intersection);
                                intersectionVOISize = intersection.getSize();
                                for (int w=0; w<intersectionVOISize;w++){
                                    intersectionContour = (VOIContour) (intersection.getCurves().elementAt(w));
                                    if (intersectionContour.slice(4) == depth){
                                        exactPartialIntersectionPixelCount = 0.0;
                                        for (m = 0; m < intersectionContour.size()-1; m++) {
                                            term = (intersectionContour.elementAt(m).X*intersectionContour.elementAt(m+1).Y 
                                                    - intersectionContour.elementAt(m+1).X*intersectionContour.elementAt(m).Y);
                                            exactPartialIntersectionPixelCount += term;        
                                        }
                                        term =  (intersectionContour.elementAt(intersectionContour.size()-1).X*intersectionContour.elementAt(0).Y 
                                                - intersectionContour.elementAt(0).X*intersectionContour.elementAt(intersectionContour.size()-1).Y);
                                        exactPartialIntersectionPixelCount += term;
                                        exactTotalIntersectionPixelCount += 0.5*Math.abs(exactPartialIntersectionPixelCount);
                                    }
                                }
                            }
                            
                            trueFound += exactTotalIntersectionPixelCount;
                            falsePositive += (exactTotalTestPixelCount - exactTotalIntersectionPixelCount);
                            absoluteTrue += exactTotalTruePixelCount;
                            falseNegative += (exactTotalTruePixelCount - exactTotalIntersectionPixelCount);
                        
                        }//for (int depth = 0; depth < zDim; depth++)

                        ViewUserInterface.getReference().setGlobalDataText("\nStatistics for VOIs with ID = " + String.valueOf(trueID) +
                                                        "\n");
                        fnvf = falseNegative / absoluteTrue;
                        ViewUserInterface.getReference().setGlobalDataText("     False negative volume fraction = " +
                                                        String.valueOf(fnvf) + "\n");
                        fpvf = falsePositive / absoluteTrue;
                        ViewUserInterface.getReference().setGlobalDataText("     False positive volume fraction = " +
                                                        String.valueOf(fpvf) + "\n");
                        tpvf = trueFound / absoluteTrue;
                        ViewUserInterface.getReference().setGlobalDataText("     True Positive volume fraction = " + String.valueOf(tpvf) +
                                                        "\n");
                    } // if (trueID == testID)
                } // for (j = 0; j < nTestVOIs; j++)
            } // if ((trueVOIs.VOIAt(i).getCurveType() == VOI.CONTOUR)
            
            //Display cumulative area information from the VOI grouping
            diceCumm = 2*iAreaCumm /(trueAreaCumm + testAreaCumm);
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
