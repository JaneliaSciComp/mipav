import java.awt.Frame;
import java.io.File;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.Vector;

import WildMagic.LibFoundation.Mathematics.Vector3f;

import gov.nih.mipav.MipavMath;
import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmTPSpline;
import gov.nih.mipav.model.algorithms.AlgorithmTransform;
import gov.nih.mipav.model.algorithms.registration.AlgorithmRegLeastSquares;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmRGBtoGray;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.model.structures.TransMatrix;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIBase;
import gov.nih.mipav.model.structures.VOIPoint;
import gov.nih.mipav.model.structures.VOIVector;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewJFrameImage;


public class PlugInAlgorithmDrosophilaStandardColumnRegistration extends AlgorithmBase {
	
	/** model images **/
	private ModelImage standardColumnImage, neuronImage, neuronImage_grey, resultImage1, resultImage2;
	
	/** resolutions **/
	private float[] resols;
	
	 /** Dimensions of match image and base image. */
    private int xdimA, ydimA, zdimA;
	
	 /** Resolutions of match image and base image. */
    private float xresA, yresA, zresA, xresB, yresB, zresB;
    /** least squared alg **/
    private AlgorithmRegLeastSquares LSMatch;
    
    /** num dims **/
    private int DIM = 3;
    
    /** fill value **/
    private float fillValue = 0.0f;
    
    /** points to remove **/
    private ArrayList<String> removePointsLabels = new ArrayList<String>();
    
    /** points collection **/
    private TreeMap<Integer, float[]> pointsMap;
    
    /** DOCUMENT ME! */
    private double[] xSource;

    /** DOCUMENT ME! */
    private double[] xTar;

    /** DOCUMENT ME! */
    private double[] ySource;

    /** DOCUMENT ME! */
    private double[] yTar;

    /** DOCUMENT ME! */
    private double[] zSource;

    /** DOCUMENT ME! */
    private double[] zTar;

    /** tp spline alg **/
    private AlgorithmTPSpline spline;
    
    /** file info **/
    private FileInfoBase fInfoBase;
    
    private float redValue = 1.0f / 3.0f;
    
    private float greenValue = 1.0f / 3.0f;
    
    private float blueValue = 1.0f / 3.0f;
    
    private boolean thresholdAverage = false;
    
    private boolean intensityAverage = false;
    
    private float threshold = 0.0f;
    
    /** dir where neuron image is **/
    private String dir;
    
    /** matrix for least squares **/
    private String leastSquaresMatrixFileName;
    
    /** matrix for thin plate spline **/
    private String thinPlateSplineMatrixFileName;
    
    
    /**
     * constuctor
     * @param neuronImage
     * @param pointsMap
     */
	public PlugInAlgorithmDrosophilaStandardColumnRegistration(ModelImage neuronImage, TreeMap<Integer, float[]> pointsMap) {
		this.neuronImage = neuronImage;
		dir = neuronImage.getImageDirectory();
		//create neuron grey image
		createGreyImage();
		this.pointsMap = pointsMap;
		resols = neuronImage.getResolutions(0);
		
	}
	
	

	/**
	 * run algorithm
	 */
	public void runAlgorithm() {
		 int[] extents = {512,512,512};
		 standardColumnImage = new ModelImage(ModelImage.UBYTE, extents,"standardColumnImage");
		 for(int i=0;i<standardColumnImage.getExtents()[2];i++) {
			 standardColumnImage.setResolutions(i, resols);
		 }
		 VOI newPtVOI = null;
		 float[] x = new float[1];
         float[] y = new float[1];
         float[] z = new float[1];
         
		 //STANDARD COLUMN IMAGE
		 newPtVOI = new VOI((short) 0, "point3D.voi",512, VOI.POINT, -1.0f);
		 newPtVOI.setUID(newPtVOI.hashCode());
		 standardColumnImage.registerVOI(newPtVOI);
		 Vector<VOIBase>[] curves = newPtVOI.getCurves();

		 //std column vals
		 //top
		 //-7.637	0		0
		 //-3.819	-3.819	0
		 //0		-7.637	0
		 //3.819	-3.819	0
		 //7.637	0		0
		 //3.819	3.819	0
		 //0		7.637	0
		 //-3.819	3.819	0
		 //0		0		0
		 //r8
		 //-7.637	0		12.7
		 //-3.819	-3.819	12.7
		 //0		-7.637	12.7
		 //3.819	-3.819	12.7
		 //7.637	0		12.7
		 //3.819	3.819	12.7
		 //0		7.637	12.7
		 //-3.819	3.819	12.7
		 //0		0		12.7
		 //r7
		 //-7.637	0		19.9
		 //-3.819	-3.819	19.9
		 //0		-7.637	19.9
		 //3.819	-3.819	19.9
		 //7.637	0		19.9
		 //3.819	3.819	19.9
		 //0		7.637	19.9
		 //-3.819	3.819	19.9
		 //0		0		19.9

		 int neurLen = Math.round((float)(19.9/resols[2]));
		 int diff = 512 - neurLen;
		 int zStart = Math.round(diff/2);
		 int zEnd = zStart + neurLen;
		 int r8Len = Math.round((float)(12.7/resols[2]));
		 int zMiddle = zStart + r8Len;

		 
		 //top
		 standardColumnImage.set(188,256,40,100);
		 x[0] = 188;
         y[0] = 256;
         z[0] = zStart;
         newPtVOI.importCurve(x, y, z, (int)z[0]);

         standardColumnImage.set(222,290,40,100);
          x[0] = 222;
         y[0] = 290;
         z[0] = zStart;
         newPtVOI.importCurve(x, y, z, (int)z[0]);
         
		 standardColumnImage.set(256,324,40,100);
		 x[0] = 256;
         y[0] = 324;
         z[0] = zStart;
         newPtVOI.importCurve(x, y, z, (int)z[0]);
         
		 standardColumnImage.set(290,290,40,100);
		 x[0] = 290;
         y[0] = 290;
         z[0] = zStart;
         newPtVOI.importCurve(x, y, z, (int)z[0]);
         
		 standardColumnImage.set(324,256,40,100);
		 x[0] = 324;
         y[0] = 256;
         z[0] = zStart;
         newPtVOI.importCurve(x, y, z, (int)z[0]);
         
		 standardColumnImage.set(290,222,40,100);
		 x[0] = 290;
         y[0] = 222;
         z[0] = zStart;
         newPtVOI.importCurve(x, y, z, (int)z[0]);
         
		 standardColumnImage.set(256,188,40,100);
		 x[0] = 256;
         y[0] = 188;
         z[0] = zStart;
         newPtVOI.importCurve(x, y, z, (int)z[0]);
         
         standardColumnImage.set(222,222,40,100);
		 x[0] = 222;
         y[0] = 222;
         z[0] = zStart;
         newPtVOI.importCurve(x, y, z, (int)z[0]);
         
		 standardColumnImage.set(256,256,40,100);
		 x[0] = 256;
         y[0] = 256;
         z[0] = zStart;
         newPtVOI.importCurve(x, y, z, (int)z[0]);
         
         //r8
         standardColumnImage.set(188,256,316,100);
		 x[0] = 188;
         y[0] = 256;
         z[0] = zMiddle;
         newPtVOI.importCurve(x, y, z, (int)z[0]);

		 standardColumnImage.set(222,290,316,100);
		 x[0] = 222;
         y[0] = 290;
         z[0] = zMiddle;
         newPtVOI.importCurve(x, y, z, (int)z[0]);
         
		 standardColumnImage.set(256,324,316,100);
		 x[0] = 256;
         y[0] = 324;
         z[0] = zMiddle;
         newPtVOI.importCurve(x, y, z, (int)z[0]);
         
		 standardColumnImage.set(290,290,316,100);
		 x[0] = 290;
         y[0] = 290;
         z[0] = zMiddle;
         newPtVOI.importCurve(x, y, z, (int)z[0]);
         
		 standardColumnImage.set(324,256,316,100);
		 x[0] = 324;
         y[0] = 256;
         z[0] = zMiddle;
         newPtVOI.importCurve(x, y, z, (int)z[0]);
         
		 standardColumnImage.set(290,222,316,100);
		 x[0] = 290;
         y[0] = 222;
         z[0] = zMiddle;
         newPtVOI.importCurve(x, y, z, (int)z[0]);
         
		 standardColumnImage.set(256,188,316,100);
		 x[0] = 256;
         y[0] = 188;
         z[0] = zMiddle;
         newPtVOI.importCurve(x, y, z, (int)z[0]);
         
         standardColumnImage.set(222,222,316,100);
		 x[0] = 222;
         y[0] = 222;
         z[0] = zMiddle;
         newPtVOI.importCurve(x, y, z, (int)z[0]);
         
		 standardColumnImage.set(256,256,316,100);
		 x[0] = 256;
         y[0] = 256;
         z[0] = zMiddle;
         newPtVOI.importCurve(x, y, z, (int)z[0]);

         //r7
         standardColumnImage.set(188,256,472,100);
		 x[0] = 188;
         y[0] = 256;
         z[0] = zEnd;
         newPtVOI.importCurve(x, y, z, (int)z[0]);

		 standardColumnImage.set(222,290,472,100);
		 x[0] = 222;
         y[0] = 290;
         z[0] = zEnd;
         newPtVOI.importCurve(x, y, z, (int)z[0]);
         
		 standardColumnImage.set(256,324,472,100);
		 x[0] = 256;
         y[0] = 324;
         z[0] = zEnd;
         newPtVOI.importCurve(x, y, z, (int)z[0]);
         
		 standardColumnImage.set(290,290,472,100);
		 x[0] = 290;
         y[0] = 290;
         z[0] = zEnd;
         newPtVOI.importCurve(x, y, z, (int)z[0]);
         
		 standardColumnImage.set(324,256,472,100);
		 x[0] = 324;
         y[0] = 256;
         z[0] = zEnd;
         newPtVOI.importCurve(x, y, z, (int)z[0]);
         
		 standardColumnImage.set(290,222,472,100);
		 x[0] = 290;
         y[0] = 222;
         z[0] = zEnd;
         newPtVOI.importCurve(x, y, z, (int)z[0]);
         
		 standardColumnImage.set(256,188,472,100);
		 x[0] = 256;
         y[0] = 188;
         z[0] = zEnd;
         newPtVOI.importCurve(x, y, z, (int)z[0]);
         
         standardColumnImage.set(222,222,472,100);
		 x[0] = 222;
         y[0] = 222;
         z[0] = zEnd;
         newPtVOI.importCurve(x, y, z, (int)z[0]);
         
		 standardColumnImage.set(256,256,472,100);
		 x[0] = 256;
         y[0] = 256;
         z[0] = zEnd;
         newPtVOI.importCurve(x, y, z, (int)z[0]);

		 standardColumnImage.calcMinMax();
		 

		 //NEURON IMAGE
		 newPtVOI = new VOI((short) 0, "point3D.voi",512, VOI.POINT, -1.0f);
		 newPtVOI.setUID(newPtVOI.hashCode());
		 neuronImage_grey.registerVOI(newPtVOI);
		 curves = newPtVOI.getCurves();
		 int key;
		 for(int i=0;i<pointsMap.size();i++) {
			 key = i + 1;
			 if(pointsMap.get(new Integer(key)) != null) {
				 x[0] = Math.round(pointsMap.get(new Integer(key))[0]/resols[0]);
		         y[0] = Math.round(pointsMap.get(new Integer(key))[1]/resols[1]);
		         z[0] = Math.round(pointsMap.get(new Integer(key))[2]/resols[2]);
			 }else{
				 removePointsLabels.add(String.valueOf(key));
				 x[0] = 10;
		         y[0] = 10;
		         z[0] = 10; 
			 }
	         newPtVOI.importCurve(x, y, z, (int)z[0]);
		 }
		 
		 
		//remove whatever points from both std column and neuron image that were not available in the points file
		 VOIVector stdColumnVOIs = standardColumnImage.getVOIs();
		 int nCurves;
		 String label;
         VOI voi = (VOI) stdColumnVOIs.elementAt(0);
         curves = voi.getCurves();
         ArrayList<Integer> indexAL_std = new ArrayList<Integer>();
         ArrayList<Integer> sliceAL_std = new ArrayList<Integer>();
         for (int s = 0; s < standardColumnImage.getExtents()[2]; s++) {
        	nCurves = curves[s].size();
			for (int j = 0; j < nCurves; j++) {
				VOIPoint voiPoint = (VOIPoint)(curves[s].elementAt(j));
				label = voiPoint.getLabel();
				for(int k=0;k<removePointsLabels.size();k++) {
			    	String removeLabel = removePointsLabels.get(k);
			    	if(label.equals(removeLabel)) {
			    		indexAL_std.add(new Integer(j));
			    		sliceAL_std.add(new Integer(s));
					}
			    }
			} 
         }	
         if(indexAL_std.size() > 0) {
        	 for(int i=indexAL_std.size(); i > 0; i--) {
        		 int ind = indexAL_std.get(i-1);
        		 int slice = sliceAL_std.get(i-1);
        		 voi.removeCurve(ind, slice);
        	 }
         }
     	standardColumnImage.notifyImageDisplayListeners();

		new ViewJFrameImage(standardColumnImage);

         VOIVector neuronVOIs = neuronImage_grey.getVOIs();
         voi = (VOI) neuronVOIs.elementAt(0);
         curves = voi.getCurves();
         ArrayList<Integer> indexAL_neuron = new ArrayList<Integer>();
         ArrayList<Integer> sliceAL_neuron = new ArrayList<Integer>();
         for (int s = 0; s < neuronImage_grey.getExtents()[2]; s++) {
        	nCurves = curves[s].size();
			for (int j = 0; j < nCurves; j++) {
				VOIPoint voiPoint = (VOIPoint)(curves[s].elementAt(j));
			    label = voiPoint.getLabel();
			    for(int k=0;k<removePointsLabels.size();k++) {
			    	String removeLabel = removePointsLabels.get(k);
			    	if(label.equals(removeLabel)) {
						indexAL_neuron.add(new Integer(j));
						sliceAL_neuron.add(new Integer(s));
					}
			    }
				
			} 
         }	
         if(indexAL_neuron.size() > 0) {
        	 for(int i=indexAL_neuron.size(); i > 0; i--) {
        		 int ind = indexAL_neuron.get(i-1);
        		 int slice = sliceAL_neuron.get(i-1);
        		 voi.removeCurve(ind, slice);
        	 }
         }

         neuronImage_grey.notifyImageDisplayListeners();
		 
         new ViewJFrameImage(neuronImage_grey);
         
         
         //call rigid least squares alg
         leastSquaredAlg();
         leastSquaredAlgorithmPerformed();
         
         //call non linear thin plate spline alg
         thinPlateSplineAlg();
         thinPlateSplineAlgorithmPerformed();
         
         //apply transformation matrices to the original color image
         //applyMatricesOnColorImage();
		 
         
         
		 setCompleted(true);
	}
	
	
	/**
	 * applies the patrices to the original color image to produce the final image
	 */
	private void applyMatricesOnColorImage() {
		System.out.println("Applying transformation matrices to original color image");
	}
	
	/**
	 * calls the non-linear thin plate spline alg
	 */
	private void thinPlateSplineAlg() {
		System.out.println("Calling Non Linear Thin Plate Spline Registration");
		int nPtsA = 0; // = standardColumnImage.getVOIs().size();
        int nPtsB = 0; // = resultImage1.getVOIs().size()
        Vector3f[] tmpptA = null;
        Vector3f[] tmpptB = null;
        Vector3f[] ptA = null; // new Vector3f[nPtsA];
        Vector3f[] ptB = null; // new Vector3f[nPtsB];
        int i, s;
        int ptNum = 0;
        Vector[] curvesB;
        Vector[] curvesM;


            curvesB = standardColumnImage.getVOIs().VOIAt(0).getCurves(); // curves[s] holds all VOIs in slice s

            for (s = 0; s < standardColumnImage.getExtents()[2]; s++) {
                nPtsA += curvesB[s].size();
            }

            Preferences.debug("thin plate spline - nPtsA = " + nPtsA);

            curvesM = resultImage1.getVOIs().VOIAt(0).getCurves(); // curves[s] holds all VOIs in slice s

            for (s = 0; s < resultImage1.getExtents()[2]; s++) {
                nPtsB += curvesM[s].size();
            }

            Preferences.debug("thin plate spline nPtsB = " + nPtsB);

            try {
                ptA = new Vector3f[nPtsA];
                ptB = new Vector3f[nPtsB];
            } catch (OutOfMemoryError error) {
                ptA = null;
                ptB = null;
                System.gc();
                MipavUtil.displayError("JDialogRegistrationTPSpline: Out of memory on ptA");

                return;
            }

            for (s = 0; s < standardColumnImage.getExtents()[2]; s++) {
                tmpptA = standardColumnImage.getVOIs().VOIAt(0).exportPoints(s);

                for (i = 0; i < tmpptA.length; i++) {
                    ptNum = (int) (Short.valueOf(((VOIPoint) curvesB[s].elementAt(i)).getLabel()).shortValue()) - 1;
                    ptA[ptNum] = tmpptA[i];
                }
            }

            for (s = 0; s < resultImage1.getExtents()[2]; s++) {
                tmpptB = resultImage1.getVOIs().VOIAt(0).exportPoints(s);

                for (i = 0; i < tmpptB.length; i++) {
                    ptNum = (int) (Short.valueOf(((VOIPoint) curvesM[s].elementAt(i)).getLabel()).shortValue()) - 1;

                    // ptNum = (int)(Short.valueOf(((VOIPoint)tmpptB[i]).getLabel()).shortValue());
                    ptB[ptNum] = tmpptB[i];
                }
            }



        // DIM == 3

            // Calculate the reverse direction to find the values of the grid positions in x',y',z' space in
            // terms of x, y, z values in the original space
            try {
                xSource = new double[nPtsA];
                ySource = new double[nPtsA];
                zSource = new double[nPtsA];

                xTar = new double[nPtsB];
                yTar = new double[nPtsB];
                zTar = new double[nPtsB];
            } catch (OutOfMemoryError error) {
                xSource = null;
                ySource = null;
                zSource = null;

                xTar = null;
                yTar = null;
                zTar = null;

                System.gc();
                MipavUtil.displayError("JDialogRegistrationTPSpline: Out of memory.");

                return;
            }

            for (i = 0; i < nPtsA; i++) {
                xSource[i] = ptA[i].X;
                ySource[i] = ptA[i].Y;
                zSource[i] = ptA[i].Z;
            }

            for (i = 0; i < nPtsB; i++) {
                xTar[i] = ptB[i].X;
                yTar[i] = ptB[i].Y;
                zTar[i] = ptB[i].Z;
            }

            // 0.0f for no smoothing, with smoothing interpolation is not exact
            try {
                spline = new AlgorithmTPSpline(xSource, ySource, zSource, xTar, yTar, zTar, 0.0f, standardColumnImage,
                                               resultImage1);
            } catch (OutOfMemoryError error) {
                spline = null;
                System.gc();
                MipavUtil.displayError("JDialogRegistrationTPSpline: Out of memory on spline");

                return;
            }

            spline.run();


	}
	
	/**
	 * create a grey image from the input color image
	 */
	private void createGreyImage() {
		if (neuronImage.getType() == ModelStorageBase.ARGB) {
            neuronImage_grey = new ModelImage(ModelImage.UBYTE, neuronImage.getExtents(),
                                         (neuronImage.getImageName() + "Gray"));
        } else if (neuronImage.getType() == ModelStorageBase.ARGB_USHORT) {
        	neuronImage_grey = new ModelImage(ModelImage.USHORT, neuronImage.getExtents(),
                                         (neuronImage.getImageName() + "Gray"));
        } else if (neuronImage.getType() == ModelStorageBase.ARGB_FLOAT) {
        	neuronImage_grey = new ModelImage(ModelImage.FLOAT, neuronImage.getExtents(),
                                         (neuronImage.getImageName() + "Gray"));
        }
		
		
		for (int n = 0; n < neuronImage.getFileInfo().length; n++) {
            fInfoBase = (FileInfoBase) (neuronImage.getFileInfo(n).clone());
            fInfoBase.setDataType(neuronImage_grey.getType());
            neuronImage_grey.setFileInfo(fInfoBase, n);
        }

        // Make algorithm
		AlgorithmRGBtoGray RGBAlgo = new AlgorithmRGBtoGray(neuronImage_grey, neuronImage, redValue, greenValue, blueValue, thresholdAverage,
                                         threshold, intensityAverage);
        
		RGBAlgo.run();

	}
	
	
	/**
	 * alg performed for thin plate spline
	 */
	private void thinPlateSplineAlgorithmPerformed() {
		resultImage2 = spline.getResultImage();
		thinPlateSplineMatrixFileName = "nonlinearThinPlateSpline-" + resultImage1.getImageName() + "_To_" + standardColumnImage.getImageName() + ".tps";
		spline.saveMatrix(dir + thinPlateSplineMatrixFileName, null);
		System.out.println("Saving nonlinear thin plate spline transformation matrix as " + dir + thinPlateSplineMatrixFileName);
		for (int i = 0; i < resultImage2.getExtents()[2]; i++) {
            resultImage2.getFileInfo(i).setOrigin(standardColumnImage.getFileInfo(i).getOrigin());
        }
		
		new ViewJFrameImage(resultImage2);
		
	}
	
	/**
	 * calls rigid least squared alg
	 */
	private void leastSquaredAlg() {
		System.out.println("Calling Rigid Least Squared Registration");
		int nPtsA = 0; // = standardColumnImage.getVOIs().size();
        int nPtsB = 0; // = neuronImage.getVOIs().size()
        Vector3f[] tmpptA = null;
        Vector3f[] tmpptB = null;
        Vector3f[] ptA = null; // new Vector3f[nPtsA];
        Vector3f[] ptB = null; // new Vector3f[nPtsB];
        int i, s, ptNum;
        Vector[] curves;
        
		curves = standardColumnImage.getVOIs().VOIAt(0).getCurves(); // curves[s] holds all VOIs in slice s

        for (s = 0; s < standardColumnImage.getExtents()[2]; s++) {
            nPtsA += curves[s].size();
        }

        Preferences.debug("nPtsA = " + nPtsA + "\n");
        ptA = new Vector3f[nPtsA];
        for (s = 0; s < standardColumnImage.getExtents()[2]; s++) {
            tmpptA = standardColumnImage.getVOIs().VOIAt(0).exportPoints(s);

            for (i = 0; i < tmpptA.length; i++) {
                ptNum = (int) (Short.valueOf(((VOIPoint) curves[s].elementAt(i)).getLabel()).shortValue()) -
                        1;
                ptA[ptNum] = tmpptA[i];
            }
        }

        curves = neuronImage_grey.getVOIs().VOIAt(0).getCurves();

        for (s = 0; s < neuronImage_grey.getExtents()[2]; s++) {
            nPtsB += curves[s].size();
        }

        if (nPtsA != nPtsB) {
            MipavUtil.displayError("Both images must have the same number of points");

            return;
        }

        Preferences.debug("nPtsB = " + nPtsB + "\n");
        ptB = new Vector3f[nPtsB];
        for (s = 0; s < neuronImage_grey.getExtents()[2]; s++) {
            tmpptB = neuronImage_grey.getVOIs().VOIAt(0).exportPoints(s);

            for (i = 0; i < tmpptB.length; i++) {
                ptNum = (int) (Short.valueOf(((VOIPoint) curves[s].elementAt(i)).getLabel()).shortValue()) -
                        1;

                // ptNum = (int)(Short.valueOf(((VOIPoint)tmpptB[i]).getLabel()).shortValue());
                ptB[ptNum] = tmpptB[i];
            }
        }
        
        
        //for(int w=0;w<ptA.length;w++) {
        	//System.out.println(ptA[w].X + "," + ptA[w].Y + "," + ptA[w].Z + "     "  + ptB[w].X + "," + ptB[w].Y + "," + ptB[w].Z);
        //}
        
        Vector3f[] ptAmm = new Vector3f[nPtsA];
        Vector3f[] ptBmm = new Vector3f[nPtsB];
        zresA = 1;
        xresA = standardColumnImage.getFileInfo(0).getResolutions()[0];
        yresA = standardColumnImage.getFileInfo(0).getResolutions()[1];
        xresB = neuronImage_grey.getFileInfo(0).getResolutions()[0];
        yresB = neuronImage_grey.getFileInfo(0).getResolutions()[1];

        if (standardColumnImage.getNDims() == 3) {
            zresA = standardColumnImage.getFileInfo(0).getResolutions()[2];
        }
        if (neuronImage_grey.getNDims() == 3) {
            zresB = neuronImage_grey.getFileInfo(0).getResolutions()[2];
        }

        for (i = 0; i < nPtsA; i++) {
        	 Preferences.debug(ptA[i].X + ", " + ptA[i].Y + ", " + ptA[i].Z + "   ");
             Preferences.debug(ptB[i].X + ", " + ptB[i].Y + ", " + ptB[i].Z + "\n");
            ptAmm[i] = new Vector3f((ptA[i].X * xresA), (ptA[i].Y * yresA),
                                    (ptA[i].Z * zresA));
            ptBmm[i] = new Vector3f((ptB[i].X * xresB), (ptB[i].Y * yresB),
                    (ptB[i].Z * zresB));
           
        }

        LSMatch = new AlgorithmRegLeastSquares(ptAmm, ptBmm, DIM);
        LSMatch.run();
        
	}

	/**
	 * alg performed for least squared alg
	 */
	private void leastSquaredAlgorithmPerformed() {
		TransMatrix matrix = LSMatch.getTransformBtoA();
		leastSquaresMatrixFileName = "rigidLeastSquares-" + neuronImage_grey.getImageName() +"_To_" + standardColumnImage.getImageName() + ".mtx";
		matrix.saveMatrix(dir + leastSquaresMatrixFileName);
		System.out.println("Saving rigid least squares transformation matrix as " + dir + leastSquaresMatrixFileName);
		LSMatch.calculateResiduals();
		xdimA = standardColumnImage.getExtents()[0];
		ydimA = standardColumnImage.getExtents()[1];
		zdimA = standardColumnImage.getExtents()[2];

        int[] extents = new int[] { xdimA, ydimA, zdimA };
        float[] resolutions = new float[] { xresA, yresA, zresA };
        resultImage1 = new ModelImage(neuronImage_grey.getType(), extents, "LS Transformed image");

        for (int i = 0; i < zdimA; i++) {
            resultImage1.getFileInfo(i).setResolutions(resolutions);
            resultImage1.getFileInfo(i).setUnitsOfMeasure(neuronImage_grey.getFileInfo()[0].getUnitsOfMeasure());
        }

        if (neuronImage_grey.isColorImage() == false) {
            AlgorithmTransform.transformTrilinear(neuronImage_grey, resultImage1, LSMatch.getTransformBtoA(),
                                                  null, true, fillValue);
        } else {
            AlgorithmTransform.transformTrilinearC(neuronImage_grey, resultImage1, LSMatch.getTransformBtoA(),
                                                   xdimA, ydimA, zdimA, xresA, yresA, zresA, fillValue);
        }
        
        
        resultImage1.calcMinMax();
        resultImage1.setImageName("LS Transformed image");
        

        //need to transform points if there were any
        VOIVector srcVOIs = neuronImage_grey.getVOIs();
        VOI newVOI;
        Vector[] curves;
        Vector[] newCurves;
        int nCurves;
        Vector3f pt, tPt;
        
        TransMatrix kTMInverse = matrix.clone();
        kTMInverse.Inverse();
        String label = "";
        VOIPoint point;
        ArrayList<Integer> indexAL_std = new ArrayList<Integer>();
        ArrayList<Integer> sliceAL_std = new ArrayList<Integer>();
        ArrayList<String> labelAL_std = new ArrayList<String>();
        TreeMap<Integer, AddVals> addCurvesMap = new TreeMap<Integer, AddVals>();
        VOI voi = (VOI) srcVOIs.elementAt(0);
		newVOI = new VOI((short) 0, "point3D.voi",resultImage1.getExtents()[2], VOI.POINT, -1.0f);
		newVOI.setUID(newVOI.hashCode());
		curves = voi.getCurves();
		newCurves = newVOI.getCurves();

		Integer labelInt;
		for (int s = 0; s < neuronImage_grey.getExtents()[2]; s++) {
			nCurves = curves[s].size();
			 for (int j = 0; j < nCurves; j++) {
				 float[] xPt = new float[1];
			     float[] yPt = new float[1];
			     float[] zPt = new float[1];
				 VOIPoint voiPoint = (VOIPoint)(curves[s].elementAt(j));
				 pt = voiPoint.exportPoint();
				 label = voiPoint.getLabel();
				 Vector3f pt2 = new Vector3f();
				 pt2.X = pt.X * neuronImage_grey.getResolutions(0)[0];
				 pt2.Y = pt.Y * neuronImage_grey.getResolutions(0)[1];
				 pt2.Z = pt.Z * neuronImage_grey.getResolutions(0)[2];
				 tPt = new Vector3f();
				 matrix.transformAsPoint3Df(pt2, tPt);
				 xPt[0] = (float)MipavMath.round(tPt.X/neuronImage_grey.getResolutions(0)[0]);
				 yPt[0] = (float)MipavMath.round(tPt.Y/neuronImage_grey.getResolutions(0)[1]);
				 zPt[0] = (float)MipavMath.round(tPt.Z/neuronImage_grey.getResolutions(0)[2]);
				 if(xPt[0] < 0 || yPt[0] < 0 || zPt[0] < 0 || xPt[0] > neuronImage_grey.getExtents()[0]-1 || yPt[0] > neuronImage_grey.getExtents()[1]-1 || zPt[0] > neuronImage_grey.getExtents()[2]-1) {
					 System.out.println("neuron image point after rigid least squares alg is out of bounds - " + label);
					 indexAL_std.add(new Integer(j));
					 sliceAL_std.add(new Integer(s));
					 labelAL_std.add(label);
				 }else {
					 AddVals vals = new AddVals(xPt, yPt, zPt);
					 labelInt = new Integer(label);
					 //System.out.println(labelInt + " " + vals.getXPt()[0] + " " + vals.getYPt()[0] + " " + vals.getZPt()[0]);
					 addCurvesMap.put(labelInt, vals);
				 }
			 }
		}
		
        //need to add curves to result image...treemap is ascending order...so should be straightforward
		Set keySet = addCurvesMap.keySet();
		Iterator iter = keySet.iterator();
		Integer key;
		AddVals v;
        while (iter.hasNext()) {
        	key = (Integer)iter.next();
        	//System.out.println("key is " + key);
        	v = addCurvesMap.get(key);
        	//System.out.println(" " + v.getXPt()[0] + " " + v.getYPt()[0] + " " + v.getZPt()[0]);
        	newVOI.importCurve(v.getXPt(), v.getYPt(), v.getZPt(), (int)(v.getZPt()[0]));
        }
            
		
        //need to remove the corresponding out of bounds points from the standard column image
        if(labelAL_std.size() > 0) {
	        VOIVector stdVOIs = standardColumnImage.getVOIs();
	        voi = (VOI) stdVOIs.elementAt(0);
	        curves = voi.getCurves();
	        String removeLabel;
	        for (int s = standardColumnImage.getExtents()[2]-1; s >= 0; s--) {
	        	nCurves = curves[s].size();
	        	 for (int j = nCurves-1; j >= 0; j--) {
					 VOIPoint voiPoint = (VOIPoint)(curves[s].elementAt(j));
					 label = voiPoint.getLabel();
					 for(int i=0;i<labelAL_std.size();i++) {
						 removeLabel = labelAL_std.get(i);
						 if(label.equals(removeLabel)) {
							 System.out.println("removing point from standard column image - " + label);
							 voi.removeCurve(j, s); 
						 }
					 }
	        	 }
	        }
        }
        

        standardColumnImage.notifyImageDisplayListeners();
        
        resultImage1.registerVOI(newVOI);

        resultImage1.notifyImageDisplayListeners();

        new ViewJFrameImage(resultImage1);
	}
	
	
	
	//Inner class that has the three coordinates for the point
	
	private class AddVals {
		private float[] xPt;
		private float[] yPt;
		private float[] zPt;
		
		private AddVals(float[] xPt, float[] yPt, float[] zPt) {
			this.xPt = xPt;
			this.yPt = yPt;
			this.zPt = zPt;
		}
		public float[] getXPt() {
			return xPt;
		}
		public void setXPt(float[] pt) {
			xPt = pt;
		}
		public float[] getYPt() {
			return yPt;
		}
		public void setYPt(float[] pt) {
			yPt = pt;
		}
		public float[] getZPt() {
			return zPt;
		}
		public void setZPt(float[] pt) {
			zPt = pt;
		}
		
	}
	
}
