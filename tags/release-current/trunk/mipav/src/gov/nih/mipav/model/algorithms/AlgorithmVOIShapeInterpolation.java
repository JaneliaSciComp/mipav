package gov.nih.mipav.model.algorithms;

import java.util.Vector;

import WildMagic.LibFoundation.Mathematics.Vector3f;

import gov.nih.mipav.model.algorithms.utilities.AlgorithmExtractSlices;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmImageCalculator;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmImageMath;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIBase;
import gov.nih.mipav.model.structures.VOIContour;
import gov.nih.mipav.view.ViewVOIVector;
/**
 * 
 * @author pandyan
 *
 * This algorithm calculates the VOI Shape Based Interpolation
 * algorithm works the following way:  Once 2 closed voi contour shapes are selected in non-contiguous 
 * slices, both slices are extracted out of the 3d dataset.   Each one has its voi moved to the center of the image
 * to obtain maximum overlap.  Then distance maps are created and then interpolated depending on num slices in between...then 
 * they the shapes are moved to their proper position.
 */
public class AlgorithmVOIShapeInterpolation extends AlgorithmBase implements AlgorithmInterface {
	
	/** src image **/
	private ModelImage srcImage;
	
	/** slice index for voi1 **/
	private int sliceIndex1;
	
	/** slice index for voi2 **/
	private int sliceIndex2;
	
	/** num slice in between voi1 and voi2 **/
	private int numSlicesInBetween;
	
	/** voi1 **/
	private VOIContour VOI1;
	
	/** voi2 **/
	private VOIContour VOI2;
	
	/** 2d image containing voi1 **/
	private ModelImage imageSlice1;
	
	/** 2d image containing voi2 **/
	private ModelImage imageSlice2;
	
	/** mask image 1 **/
	private ModelImage maskImage1;
	
	/** mask imag 2 **/
	private ModelImage maskImage2;
	
	/** distance map 1 **/
	private ModelImage distanceMap1;
	
	/** distance map 2 **/
	private ModelImage distanceMap2;
	
	/** temp image **/
	private ModelImage tempA;
	
	/** temp image **/
	private ModelImage tempB;
	
	/** distance maps **/
	private ModelImage[] averageDistanceMaps;
	
	/** tween shape s**/
	private ModelImage[] inBetweenBooleanShapes;
	
	/** final voi contour shapes **/
	private VOIContour[] finalContours;

	/** geom center voi1 **/
	private Vector3f geomCenter1;
	
	/** geom center voi2 **/
	private Vector3f geomCenter2;
	
	/** image center **/
	private Vector3f imageCenter;
	
	/** handle to src image's VOI **/
	private VOI VOIHandle;
	
	/** 
	 * constructor
	 */
	public AlgorithmVOIShapeInterpolation() {
		
	}
	
	/**
	 * constructor
	 * @param srcImage
	 * @param sliceIndex1
	 * @param VOI1
	 * @param sliceIndex2
	 * @param VOI2
	 */
	public AlgorithmVOIShapeInterpolation(ModelImage srcImage,int sliceIndex1,VOIContour VOI1, int sliceIndex2,VOIContour VOI2, VOI VOIHandle) {
		this.srcImage = srcImage;
		if(sliceIndex1 < sliceIndex2) {
			this.sliceIndex1 = sliceIndex1;
			this.sliceIndex2 = sliceIndex2;
			this.VOI1 = VOI1;
			this.VOI2 = VOI2;
		}else {
			this.sliceIndex1 = sliceIndex2;
			this.sliceIndex2 = sliceIndex1;
			this.VOI1 = VOI2;
			this.VOI2 = VOI1;
		}
		this.VOIHandle = VOIHandle;
		numSlicesInBetween = this.sliceIndex2 - this.sliceIndex1 - 1;
		
		averageDistanceMaps = new ModelImage[numSlicesInBetween];
		inBetweenBooleanShapes = new ModelImage[numSlicesInBetween];
		finalContours = new VOIContour[numSlicesInBetween];
	}

	/**
	 * run algorithm
	 */
	public void runAlgorithm() {
        fireProgressStateChanged(10);
		
        VOI1.translate( 0, 0, -sliceIndex1 ); VOI1.update();
        VOI2.translate( 0, 0, -sliceIndex2 ); VOI2.update();
        
		//extract 2D slices...and place voi contours in them
		int[] destExtents = new int[2];
        destExtents[0] = srcImage.getExtents()[0];
        destExtents[1] = srcImage.getExtents()[1];

        imageSlice1 = new ModelImage(srcImage.getType(), destExtents, srcImage.getImageName() + "_1");
        imageSlice2 = new ModelImage(srcImage.getType(), destExtents, srcImage.getImageName() + "_2");
        
        String[] extractSlices1 = {String.valueOf(sliceIndex1)};
        String[] extractSlices2 = {String.valueOf(sliceIndex2)};
		
		AlgorithmExtractSlices extractSlicesAlg1 = new AlgorithmExtractSlices(srcImage, imageSlice1, extractSlices1);
		extractSlicesAlg1.run();
		VOI newVOI1 = new VOI((short)0,"voi1");
		newVOI1.getCurves().add(VOI1);
		imageSlice1.registerVOI(newVOI1);
		//lets get original center for VOI1
		geomCenter1 = VOI1.getGeometricCenter();

		AlgorithmExtractSlices extractSlicesAlg2 = new AlgorithmExtractSlices(srcImage, imageSlice2, extractSlices2);
		extractSlicesAlg2.run();
		VOI newVOI2 = new VOI((short)0,"voi2");
		newVOI2.getCurves().add(VOI2);
		imageSlice2.registerVOI(newVOI2);
		//lets get original center for VOI2
		geomCenter2 = VOI2.getGeometricCenter();
		
		//System.err.println( geomCenter1 + " " + geomCenter2 );

		//get center of image...since imageSlice1 and imageSlice2 are slices from same image, get it from either
		imageCenter = imageSlice1.getImageCenter();

		//reposition VOIs to center
		float transX1 = imageCenter.X - geomCenter1.X;
		float transY1 = imageCenter.Y - geomCenter1.Y;
		float transX2 = imageCenter.X - geomCenter2.X;
		float transY2 = imageCenter.Y - geomCenter2.Y;
		VOI1.translate(transX1, transY1, 0); VOI1.update();
		VOI2.translate(transX2, transY2, 0); VOI2.update();

		fireProgressStateChanged(20);
		
		//generate binary mask images
		maskImage1 = imageSlice1.generateBinaryImage(false, true);
		maskImage1.setImageName(imageSlice1.getImageName() + "_mask1");
		maskImage2 = imageSlice2.generateBinaryImage(false, true);
		maskImage2.setImageName(imageSlice2.getImageName() + "_mask2");

        //new ViewJFrameImage((ModelImage)imageSlice1.clone());
        //new ViewJFrameImage((ModelImage)imageSlice2.clone());

        //new ViewJFrameImage((ModelImage)maskImage1.clone());
        //new ViewJFrameImage((ModelImage)maskImage2.clone());
		
		
        //generate distance map image for shape interpolation
        distanceMap1 = (ModelImage) maskImage1.clone();
        distanceMap1.setImageName(imageSlice1.getImageName() + "_dMap1");
        distanceMap2 = (ModelImage) maskImage2.clone();
        distanceMap2.setImageName(imageSlice2.getImageName() + "_dMap2");

        AlgorithmMorphology2D distanceMapAlgo1 = new AlgorithmMorphology2D(distanceMap1, 0, 0, AlgorithmMorphology2D.DISTANCE_MAP_FOR_SHAPE_INTERPOLATION, 0, 0, 0, 0, true);
        distanceMapAlgo1.run();
        AlgorithmMorphology2D distanceMapAlgo2 = new AlgorithmMorphology2D(distanceMap2, 0, 0, AlgorithmMorphology2D.DISTANCE_MAP_FOR_SHAPE_INTERPOLATION, 0, 0, 0, 0, true);
        distanceMapAlgo2.run();

        
        fireProgressStateChanged(30);
        //now depending on #of image slices in between imageSlice 1 and imageSlice2, we average accordingly
        //if numSlicesInBetween is 1...just call MIPAV's average function
        if(numSlicesInBetween == 1) {
        	averageDistanceMaps[0] = new ModelImage(distanceMap1.getType(), distanceMap1.getExtents(), "average_0");
        	fireProgressStateChanged(40);
        	AlgorithmImageCalculator mathAlgo = new AlgorithmImageCalculator(averageDistanceMaps[0], distanceMap1, distanceMap2, AlgorithmImageCalculator.AVERAGE, AlgorithmImageMath.CLIP, true, null);

        	mathAlgo.run();

        	inBetweenBooleanShapes[0] = new ModelImage(ModelStorageBase.BOOLEAN, distanceMap1.getExtents(), "booleanShape_0");
        	int length = distanceMap1.getExtents()[0] * distanceMap1.getExtents()[1];
        	float[] avgDistMapBuff = new float[length];
        	boolean[] boolBuff = new boolean[length];
        	try {
        		averageDistanceMaps[0].exportData(0,length,avgDistMapBuff);
        	}catch(Exception e) {
        		
        	}
        	for(int i=0;i<avgDistMapBuff.length;i++) {
        		if(avgDistMapBuff[i] >= 0) {
        			boolBuff[i] = true;
        		}else {
        			boolBuff[i] = false;
        		}
        	}
        	try {
        		inBetweenBooleanShapes[0].importData(0, boolBuff, true);
        	}catch(Exception e) {
        		
        	}

        	fireProgressStateChanged(60);
            AlgorithmVOIExtraction VOIExtractionAlgo = new AlgorithmVOIExtraction(inBetweenBooleanShapes[0]);
            VOIExtractionAlgo.run();
            
            fireProgressStateChanged(80);
            //we are only working with one contour on a 2d slice here
            ViewVOIVector VOIs = (ViewVOIVector) inBetweenBooleanShapes[0].getVOIs();
            VOI tempVOI = (VOI)(VOIs.VOIAt(0).clone());
        	tempVOI.setUID(tempVOI.hashCode());
            Vector<VOIBase> contours = tempVOI.getCurves();
            finalContours[0] = (VOIContour)contours.elementAt(0);
            
            boolean isLineVertical = false;
            float diffX = Math.abs(geomCenter1.X - geomCenter2.X);
            float stepX = (diffX/2);

            float newX = 0;
            float newY = 0;
            if(geomCenter1.X < geomCenter2.X) {
            	newX = geomCenter1.X + stepX;
            }else if(geomCenter1.X > geomCenter2.X) {
            	newX = geomCenter2.X + stepX;
            }else {
            	isLineVertical = true;
            }

            if(!isLineVertical) {
            	//get newY coordinate
            	newY = linearInterpGetY(newX);
            }
            if(isLineVertical) {
            	newX = geomCenter1.X;
            	float diffY = Math.abs(geomCenter1.Y - geomCenter2.Y);
                float stepY = (diffY/2);
                if(geomCenter1.Y < geomCenter2.Y) {
                	newY = geomCenter1.Y + stepY;
                }else if(geomCenter1.Y > geomCenter2.Y) {
                	newY = geomCenter2.Y + stepY;
                }else {
                	newY = geomCenter1.Y;
                }
            }
            

            float transX = newX - imageCenter.X;
    		float transY = newY - imageCenter.Y;
    		finalContours[0].translate(transX, transY, sliceIndex1 + 1);
            
            VOIHandle.importCurve(finalContours[0]);
    		fireProgressStateChanged(100);

            
        	
        }else { 
        	fireProgressStateChanged(40);
        	/**
        	 * example....if #of slices in between are 3
        	 * then...averages are obtained where 1st image slice is (3/4)A + (1/4)B
        	 * 2nd one is (2/4)A + (2/4)B
        	 * and 3rd one is (1/4)A + ((3/4)B
        	 */
        	int progVal = 45;
        	for(int i=1,k=numSlicesInBetween;i<=numSlicesInBetween;i++,k--) {
        		if(progVal > 90) {
        			progVal = 90;
        		}
        		fireProgressStateChanged(progVal);
        		progVal = progVal + (progVal/numSlicesInBetween);
        		int denominator = numSlicesInBetween + 1;
        		double numerator_A = k;
        		double numerator_B = i;

        		double factor_A = (numerator_A/denominator);
        		double factor_B = (numerator_B/denominator);

        		int index = i - 1;
        		tempA = new ModelImage(distanceMap1.getType(), distanceMap1.getExtents(), "tempA_" + index);
        		tempB = new ModelImage(distanceMap2.getType(), distanceMap2.getExtents(), "tempB_" + index);
        		
        		averageDistanceMaps[index] = new ModelImage(distanceMap1.getType(), distanceMap1.getExtents(), "average_" + index);
        		
        		AlgorithmImageMath mathAlgoA = new AlgorithmImageMath(tempA, distanceMap1, AlgorithmImageMath.MULTIPLY, factor_A, 0, 0, AlgorithmImageMath.CLIP, true);
        		mathAlgoA.run();

        		AlgorithmImageMath mathAlgoB = new AlgorithmImageMath(tempB, distanceMap2, AlgorithmImageMath.MULTIPLY, factor_B, 0, 0, AlgorithmImageMath.CLIP, true);
        		mathAlgoB.run();

            	AlgorithmImageCalculator mathAlgoAdd = new AlgorithmImageCalculator(averageDistanceMaps[index], tempA, tempB, AlgorithmImageCalculator.ADD, AlgorithmImageMath.CLIP, true, null);
            	mathAlgoAdd.run();

            	inBetweenBooleanShapes[index] = new ModelImage(ModelStorageBase.BOOLEAN, distanceMap1.getExtents(), "booleanShape_" + index);
            	int length = distanceMap1.getExtents()[0] * distanceMap1.getExtents()[1];
            	float[] avgDistMapBuff = new float[length];
            	boolean[] boolBuff = new boolean[length];
            	try {
            		averageDistanceMaps[index].exportData(0,length,avgDistMapBuff);
            	}catch(Exception e) {
            		
            		finalize();
            		return;
            	}
            	for(int m=0;m<avgDistMapBuff.length;m++) {
            		if(avgDistMapBuff[m] >= 0) {
            			boolBuff[m] = true;
            		}else {
            			boolBuff[m] = false;
            		}
            	}
            	try {
            		inBetweenBooleanShapes[index].importData(0, boolBuff, true);
            	}catch(Exception e) {
            		finalize();
            		return;
            	}

                //new ViewJFrameImage( (ModelImage)averageDistanceMaps[index].clone() );
                //new ViewJFrameImage( (ModelImage)inBetweenBooleanShapes[index].clone() );

            	AlgorithmVOIExtraction VOIExtractionAlgo = new AlgorithmVOIExtraction(inBetweenBooleanShapes[index]);
                VOIExtractionAlgo.run();
            	
                //we are only working with one contour on a 2d slice here
                ViewVOIVector VOIs = inBetweenBooleanShapes[index].getVOIs();
                VOI tempVOI = new VOI(VOIs.VOIAt(0));
            	tempVOI.setUID(tempVOI.hashCode());
                Vector<VOIBase> contours = tempVOI.getCurves();
                finalContours[index] = (VOIContour)contours.elementAt(0);
                
                boolean isLineVertical = false;
                float diffX = Math.abs(geomCenter1.X - geomCenter2.X);
                float stepX = diffX/(denominator);
                float newX = 0;
                float newY = 0;
                if(geomCenter1.X < geomCenter2.X) {
                	newX = (int)geomCenter1.X + (stepX * i);
                }else if(geomCenter1.X > geomCenter2.X) {
                    //newX = (int)geomCenter2.X + (stepX * i);
                    newX = (int)geomCenter1.X - (stepX * i);
                }else {
                	//line is vertical...to do later
                	isLineVertical = true;
                }

                if(!isLineVertical) {
                	//get newY coordinate
                	newY = linearInterpGetY(newX);
                }
                if(isLineVertical) {
                	newX = geomCenter1.X;
                	float diffY = Math.abs(geomCenter1.Y - geomCenter2.Y);
                    float stepY = (diffY/denominator);
                    if(geomCenter1.Y < geomCenter2.Y) {
                    	newY = geomCenter1.Y + (stepY * i);
                    }else if(geomCenter1.Y > geomCenter2.Y) {
                        //newY = geomCenter2.Y + (stepY * i);
                        newY = geomCenter1.Y - (stepY * i);
                    }else {
                    	newY = geomCenter1.Y;
                    }
                }
                

                float transX = newX - imageCenter.X;
        		float transY = newY - imageCenter.Y;
        		finalContours[index].translate(transX, transY, sliceIndex1 + i);
        		finalContours[index].update();
        		if ( finalContours[index].size() > 0 )
        		{
        		    VOIHandle.importCurve(finalContours[index]);
        		}
                if(tempA != null) {
                	tempA.disposeLocal();
                	tempA = null;
                }
                if(tempB != null) {
                	tempB.disposeLocal();
                	tempB = null;
                }
                
                
        	}
        	
        }
        fireProgressStateChanged(95);
        finalize();
        fireProgressStateChanged(100);
        srcImage.notifyImageDisplayListeners();
        setCompleted(true);
        
        
	}

	/** alg performed **/
	public void algorithmPerformed(AlgorithmBase algorithm) {
		// TODO Auto-generated method stub

	}
	
	/** get y based on equation of linear interpolation **/
	public float linearInterpGetY(float x) {
		float y = 0;
		y = ((geomCenter1.Y) + (x - geomCenter1.X)*(geomCenter2.Y - geomCenter1.Y)/(geomCenter2.X - geomCenter1.X));
		return y;
	}
	
	
	
	/** 
	 * finalize
	 */
	public void finalize() {		
		if(imageSlice1 != null) {
			imageSlice1.disposeLocal();
			imageSlice1 = null;
		}
		if(imageSlice2 != null) {
			imageSlice2.disposeLocal();
			imageSlice2 = null;
		}
		if(maskImage1 != null) {
			maskImage1.disposeLocal();
			maskImage1 = null;
		}
		if(maskImage2 != null) {
			maskImage2.disposeLocal();
			maskImage2 = null;
		}
		if(distanceMap1 != null) {
			distanceMap1.disposeLocal();
			distanceMap1 = null;
		}
		if(distanceMap2 != null) {
			distanceMap2.disposeLocal();
			distanceMap2 = null;
		}
		if(tempA != null) {
			tempA.disposeLocal();
			tempA = null;
		}
		if(tempB != null) {
			tempB.disposeLocal();
			tempB = null;
		}
		if(averageDistanceMaps != null) {
			for(int i=0;i<averageDistanceMaps.length;i++) {
				if(averageDistanceMaps[i] != null) {
					averageDistanceMaps[i].disposeLocal();
					averageDistanceMaps[i] = null;
				}
			}
			averageDistanceMaps = null;
		}
		if(inBetweenBooleanShapes != null) {
			for(int i=0;i<inBetweenBooleanShapes.length;i++) {
				if(inBetweenBooleanShapes[i] != null) {
					inBetweenBooleanShapes[i].disposeLocal();
					inBetweenBooleanShapes[i] = null;
				}
			}
			inBetweenBooleanShapes = null;
		}
	}

}
