import gov.nih.mipav.util.MipavMath;

import gov.nih.mipav.model.algorithms.*;

import gov.nih.mipav.model.algorithms.registration.AlgorithmRegLeastSquares;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmRGBtoGray;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.RegionGrowDialog;

import java.io.*;
import java.util.*;

import javax.swing.JTextArea;

import WildMagic.LibFoundation.Mathematics.Vector3f;


/**
 * This plugin was done for Dr Chi-hon Lee of NICHD
 * 
 * This plugin basically standardizes the output from the DrosophilaRetinalRegistration to a standard model
 * 
 * It also standardizes an input surface file
 * 
 * It also allows for inverting the output surface file
 * 
 * The plugin also creates the SWC file
 * 
 * @author pandyan
 */
public class PlugInAlgorithmDrosophilaStandardColumnRegistration extends AlgorithmBase {

    /** model images * */
    private ModelImage standardColumnImage, neuronImage, neuronImage_grey, resultImage1, finalImage, cityBlockImage;

    /** resolutions * */
    private final float[] resols;

    /** Dimensions of match image and base image. */
    private int xdimA, ydimA, zdimA;

    /** Resolutions of match image and base image. */
    private float xresA, yresA, zresA, xresB, yresB, zresB;

    /** least squared alg * */
    private AlgorithmRegLeastSquares LSMatch;

    /** num dims * */
    private final int DIM = 3;

    /** fill value * */
    private final float fillValue = 0.0f;

    /** points to remove * */
    private final ArrayList<String> removePointsLabels = new ArrayList<String>();

    /** points collection * */
    private final TreeMap<Integer, float[]> pointsMap;

    /** xSource */
    private double[] xSource;

    /** xTar */
    private double[] xTar;

    /** ySource */
    private double[] ySource;

    /** yTar */
    private double[] yTar;

    /** zSource */
    private double[] zSource;

    /** zTar! */
    private double[] zTar;

    /** tp spline alg * */
    private AlgorithmTPSpline spline;

    /** file info * */
    private FileInfoBase fInfoBase;

    /** red value weighting * */
    private final float redValue = 1.0f / 3.0f;

    /** green value weighting * */
    private final float greenValue = 1.0f / 3.0f;

    /** blue value weighting * */
    private final float blueValue = 1.0f / 3.0f;

    /** threshold average * */
    private final boolean thresholdAverage = false;

    /** intensity average * */
    private final boolean intensityAverage = false;

    /** threshold * */
    private final float threshold = 0.0f;

    /** dir where neuron image is * */
    private final String dir;

    /** least squares - rigid matrix * */
    private TransMatrix lsMatrix;

    /** coords of filament * */
    private final ArrayList<ArrayList<float[]>> allFilamentCoords;

    /** new coords of filment * */
    private final ArrayList<ArrayList<float[]>> allFilamentCoords_newCoords;
    
    
    

    /** tolerances used * */
    private final double tolerance, toleranceSq;

    /** old surface file * */
    private final File oldSurfaceFile;

    /** sampling rate * */
    private final float samplingRate;

    /** neuraon image extents * */
    private final int[] neuronImageExtents;

    /** r7 center coord * */
    private float[] r7_27Coord;

    /** r7 center coord transformed * */
    private float[] r7_27Coord_transformed;

    /** if r7 center point was found * */
    private boolean r7CenterPointFound = false;

    /** input points * */
    private final ArrayList<float[]> inputPointsList = new ArrayList<float[]>();

    /** transformed points * */
    private final ArrayList<float[]> transformedPointsList = new ArrayList<float[]>();

    /** points file * */
    private final File pointsFile;

    /** output text area * */
    private final JTextArea outputTextArea;

    /** whether to flip or not * */
    private  boolean flipX, flipY, flipZ;
    
    private String standardizedFilamentFileName;
    
    private String filamentFileParentDir;
    
    //SWC...following vars are needed for creation of swc file format
    
    private float subsamplingDistance;
	
	private float greenThresold;
	
	//private String outputFilename, outputFilename_auto, outputFilename_regionGrow;

	
	/** coords of filament **/
    private ArrayList <ArrayList<float[]>> allFilamentCoords_swc = new ArrayList <ArrayList<float[]>>();

    
    private ArrayList <ArrayList<float[]>> newFilamentCoords_swc = new ArrayList<ArrayList<float[]>>();

    
	/** Storage location of the second derivative of the Gaussian in the X direction. */
    private float[] GxxData;

    /** Storage location of the second derivative of the Gaussian in the Y direction. */
    private float[] GyyData;

    /** Storage location of the second derivative of the Gaussian in the Z direction. */
    private float[] GzzData;

    /** Dimensionality of the kernel. */
    private int[] kExtents;
    
    /** An amplification factor greater than 1.0 causes this filter to act like a highpass filter. */
    private float amplificationFactor = 1.0f;
    
    /** images showing the swc spheres/radii **/
    //private ModelImage maskImage, maskImageAuto;
    
    //private ModelImage maskImageRegionGrow;
    
    private boolean doRigidOnly = false;
    
    
    private ModelImage greenImage;
    
    //private ModelImage greenRegionGrowMaskImage;
    

    private BitSet paintBitmap;
    
    private boolean doSWC;
    
    private boolean rvld;
    
    private String numPointsString;
    
    private int numPoints;
    
    
    
    //private boolean doLeftEye;
	
	
	

    /**
     * constuctor
     * 
     * @param neuronImage
     * @param pointsMap
     */
    public PlugInAlgorithmDrosophilaStandardColumnRegistration(final ModelImage neuronImage,
            final TreeMap<Integer, float[]> pointsMap, final ArrayList<ArrayList<float[]>> allFilamentCoords,
            final File oldSurfaceFile, final float samplingRate, final ModelImage cityBlockImage,
            final File pointsFile, final JTextArea outputTextArea, final boolean flipX, final boolean flipY,
            final boolean flipZ,float greenThreshold, float subsamplingDistance, boolean rigidOnly,boolean doSWC, boolean rvld, String numPointsString) {
    	
    	
        this.neuronImage = neuronImage;
        this.neuronImageExtents = neuronImage.getExtents();
        dir = neuronImage.getImageDirectory();
        // create neuron grey image
        createGreyImage();
        this.pointsMap = pointsMap;
        resols = neuronImage.getResolutions(0);
        this.allFilamentCoords = allFilamentCoords;
        allFilamentCoords_newCoords = new ArrayList<ArrayList<float[]>>();
        for (int i = 0; i < allFilamentCoords.size(); i++) {
            final ArrayList<float[]> al = allFilamentCoords.get(i);
            final int size = al.size();
            final ArrayList<float[]> al_new = new ArrayList<float[]>();


            for (int k = 0; k < size; k++) {
                al_new.add(k, null);
            }


            allFilamentCoords_newCoords.add(al_new);
        }

        final double sqrRtThree = Math.sqrt(3);

        
        tolerance = (sqrRtThree / 2) * ( ( (resols[0] / resols[0]) + (resols[1] / resols[0]) + (resols[2] / resols[0])) / 3);

        toleranceSq = tolerance * tolerance;

        this.oldSurfaceFile = oldSurfaceFile;
        this.samplingRate = samplingRate;
        this.cityBlockImage = cityBlockImage;
        this.pointsFile = pointsFile;
        this.outputTextArea = outputTextArea;
        this.flipX = flipX;
        this.flipY = flipY;
        this.flipZ = flipZ;
        this.doSWC = doSWC;
	    if(doSWC) {
	        this.greenThresold = greenThreshold;
			this.subsamplingDistance = subsamplingDistance;
			//this.outputFilename = outputFilename;
			//this.outputFilename_auto = outputFilename_auto;
			//this.outputFilename_regionGrow = outputFilename_regionGrow;

	    }
		this.doRigidOnly = rigidOnly;
		this.rvld = rvld;
		
		this.numPointsString = numPointsString;
		if(numPointsString.equals(PlugInDialogDrosophilaStandardColumnRegistration._27POINTS) || numPointsString.equals(PlugInDialogDrosophilaStandardColumnRegistration._27APOINTS)) {
			this.numPoints = 27;
		}else if(numPointsString.equals(PlugInDialogDrosophilaStandardColumnRegistration._75POINTS) || numPointsString.equals(PlugInDialogDrosophilaStandardColumnRegistration._75APOINTS)) {
			this.numPoints = 75;
		}else {
			this.numPoints = 147;
		}
		
		

    }
    

    /**
     * run algorithm
     */
    public void runAlgorithm() {
    	if(outputTextArea != null) {
    		outputTextArea.append("Running Algorithm v6.4" + "\n");
    	}else {
    		System.out.println("Running Algorithm v6.4");
    	}
    	
    	
        
        //outputTextArea.append("Standard Column : RV/LD (in to out); RD/LV(out to in)" + "\n");
        /*String text = "";
        if(rvld) {
        	text = "RV/LD";
        }else {
        	text = "LD/RV";
        }
        
        outputTextArea.append("Standard Column : " + text + "\n");*/

        final long begTime = System.currentTimeMillis();

        final int[] extents = {512, 512, 512};
        standardColumnImage = new ModelImage(ModelStorageBase.UBYTE, extents, "standardColumnImage");
        for (int i = 0; i < standardColumnImage.getExtents()[2]; i++) {
            standardColumnImage.setResolutions(i, resols);
        }

        final FileInfoImageXML[] fileInfoBases = new FileInfoImageXML[standardColumnImage.getExtents()[2]];
        for (int i = 0; i < fileInfoBases.length; i++) {
            fileInfoBases[i] = new FileInfoImageXML(standardColumnImage.getImageName(), null, FileUtility.XML);
            fileInfoBases[i].setEndianess(neuronImage.getFileInfo()[0].getEndianess());
            fileInfoBases[i].setUnitsOfMeasure(neuronImage.getFileInfo()[0].getUnitsOfMeasure());
            fileInfoBases[i].setResolutions(neuronImage.getFileInfo()[0].getResolutions());
            fileInfoBases[i].setExtents(neuronImage.getExtents());
            fileInfoBases[i].setOrigin(neuronImage.getFileInfo()[0].getOrigin());

        }

        standardColumnImage.setFileInfo(fileInfoBases);

        VOI newPtVOI = null;
        final float[] x = new float[1];
        final float[] y = new float[1];
        final float[] z = new float[1];

        // STANDARD COLUMN IMAGE
        newPtVOI = new VOI((short) 0, "point3D.voi", VOI.POINT, -1.0f);
        newPtVOI.setUID(newPtVOI.hashCode());
        standardColumnImage.registerVOI(newPtVOI);

        // std column vals
        // top
        // -7.637 0 0
        // -3.819 -3.819 0
        // 0 -7.637 0
        // 3.819 -3.819 0
        // 7.637 0 0
        // 3.819 3.819 0
        // 0 7.637 0
        // -3.819 3.819 0
        // 0 0 0
        // r8
        // -7.637 0 12.7
        // -3.819 -3.819 12.7
        // 0 -7.637 12.7
        // 3.819 -3.819 12.7
        // 7.637 0 12.7
        // 3.819 3.819 12.7
        // 0 7.637 12.7
        // -3.819 3.819 12.7
        // 0 0 12.7
        // r7
        // -7.637 0 19.9
        // -3.819 -3.819 19.9
        // 0 -7.637 19.9
        // 3.819 -3.819 19.9
        // 7.637 0 19.9
        // 3.819 3.819 19.9
        // 0 7.637 19.9
        // -3.819 3.819 19.9
        // 0 0 19.9
        
        
        
        
        //NEW STUFF
        
        //Top A	-8.74	0	-21.37
    	//B	-4.37	-4.37	-21.37
    	//C	0	-8.74	-21.37
    	//D	4.37	-4.37	-21.37
    	//E	8.74	0	-21.37
    	//F	4.37	4.37	-21.37
    	//G	0	8.74	-21.37
    	//H	-4.37	4.37	-21.37
    	//I	0	0	-21.37
    //R8	A	-8.74	0	-6.93
    	//B	-4.37	-4.37	-6.93
    	//C	0	-8.74	-6.93
    	//D	4.37	-4.37	-6.93
    	//E	8.74	0	-6.93
    	//F	4.37	4.37	-6.93
    	//G	0	8.74	-6.93
    	//H	-4.37	4.37	-6.93
    	//I	0	0	-6.93
    //R7	A	-8.74	0	0
    	//B	-4.37	-4.37	0
    	//C	0	-8.74	0
    	//D	4.37	-4.37	0
    	//E	8.74	0	0
    	//F	4.37	4.37	0
    	//G	0	8.74	0
    	//H	-4.37	4.37	0
    	//I	0	0	0
        
        
        //NEW NEW STUFF    25 points per layer! ....still -21.37  -6.93, 0 for z
        
    
        //A -17.48	0
        //B -13.11	-4.37
        //C -8.74	-8.74
        //D -4.37	-13.11
        //E 0	-17.48
        //F 4.37	-13.11
        //G 8.74	-8.74
        //H 13.11	-4.37
        //I 17.48	0
        //J 13.11	4.37
        //K 8.74	8.74
        //L 4.37	13.11
        //M 0	17.48
        //N -4.37	13.11
        //0 -8.74	8.74
        //P -13.11	4.37
        //Q -8.74	0
        //R -4.37	-4.37
        //S 0	-8.74
        //T 4.37	-4.37
        //U 8.74	0
        //V 4.37	4.37
        //W 0	8.74
        //X -4.37	4.37
        //Y 0	0



        
        
        
        //21.37 is the length of neuron
        //14.44 is the diff between top and r8
        
        
        

        final int neurLen = Math.round((float) (21.37 / resols[2]));
        final int diff = 512 - neurLen;
        final int zStart = Math.round(diff / 2);
        final int zEnd = zStart + neurLen;
        
        //****** FOR LV, WE ARE FLIPPING THE NEURON....SO THE ZMIDDLE USES A LENGTH OF 6.93 INSTEAD OF THE NORMAL 14.44
        //final int r8Len = Math.round((float) (6.93 / resols[2]));
        final int r8Len = Math.round((float) (14.44 / resols[2]));
        
        final int zMiddle = zStart + r8Len;
        
        
  
        
        //27 Points: 9 per layer
        //8.74*2 = 17.48
        //17.48 / res[0] =  155.4
        //512 - 155.4 = 356.6
        //356.6/2 = 178 
        //512-178 = 334  

        //4.37*2 = 8.74
        //8.74 / res[0] = 77.7
        //512 - 77.7 = 434.3
        //434.3/2 = 217    
        //512 - 217 =  295    
        
        
        
        //75 points: 25 per layer
        //17.48 * 2 = 34.96
        //34.96 / res[0] = 310.8
        //512 - 310.8 = 201.2
        //201.2/2 = 100.6
        //512 - 100.6 = 411.4

        //256 - 100.6 = 155.4   155.4/4 = 38.85
        
        //rvld points
        
        //101, 256
        //139, 295
        //178, 334
        //217, 373
        //256, 411
        
        //295, 373
        //334, 334
        //373, 295
        //411, 256
        
        //373, 217
        //334, 178
        //295, 139
        //256, 101
        
        //217, 139
        //178, 178
        //139, 217
      
        //178, 256
        //217, 295
        //256, 334
        
        //295, 295
        //334, 256
        
        //295, 217
        //256, 178
        
        //217, 217
        
        //256, 256
        
        
        
        
        //147 points: 49 per layer
        //26.22 * 2 = 52.44
        //52.44 / res[0] = 466.2
        //512 - 466.2 - 45.8
        //45.8/2 = 22.9
        //
        //466.2 / 2 = 233.1    233.1/6 = 38.85
        
        //rvld points

        //23, 256
        //62, 295
        //101, 334
        //139, 373
        //178, 411
        //217, 450
        //256, 489
        
        //295, 450
        //334, 411
        //373, 373
        //411, 334
        //450, 295
        //489, 256
        
        //450, 217
        //411, 178
        //373, 139
        //334, 101
        //295, 62
        //256, 23
        
        //217, 62
        //178, 101
        //139, 139
        //101, 178
        //62, 217
        
        //101, 256
        //139, 295
        //178, 334
        //217, 373
        //256, 411
        
        //295, 373
        //334, 334
        //373, 295
        //411, 256
        
        //373, 217
        //334, 178
        //295, 139
        //256, 101
        
        //217, 139
        //178, 178
        //139, 217
      
        //178, 256
        //217, 295
        //256, 334
        
        //295, 295
        //334, 256
        
        //295, 217
        //256, 178
        
        //217, 217
        
        //256, 256
        
        
        
        
        //new 10/2/2012
        //27 points per layer (L1-R8-R7)  RV!
        //Top  A: 1	-6.743075	-21.957
                                          
        // B: -2.9715375	-2.7715375	-21.957

        //C: -6.943075	1.2	-21.957

        //D: -2.9715375	5.1715375	-21.957

        //E: 1	9.143075	-21.957

        //F: 4.9715375	5.1715375	-21.957

        //G: 8.943075	1.2	-21.957

        //H: 4.9715375	-2.7715375	-21.957

        //I: 1	1.2	-21.957

        //R8 A: 0	-7.943075	-6.634
        //B: 3.9715375	-3.9715375	-6.634
        //C: 7.943075	0	-6.634
        //D: 3.9715375	3.9715375	-6.634
        //E: 0	7.943075	-6.634
        //F: -3.9715375	3.9715375	-6.634
        //G: -7.943075	0	-6.634
        //H: -3.9715375	-3.9715375	-6.634
        //I: 0	0	-6.634
        //R7 A: 0	-7.943075	0
        //B: 3.9715375	-3.9715375	0
        //C: 7.943075	0	0
        //D: 3.9715375	3.9715375	0
        //E: 0	7.943075	0
        //F: -3.9715375	3.9715375	0
        //G: -7.943075	0	0
        //H: -3.9715375	-3.9715375	0
        //I: 0	0	0
        
        
        
        
        
        
        
        
        //21.957 is the length of neuron
        //15.323 is the diff between top and r8
        
        final int neurLen2 = Math.round((float) (21.957/ resols[2]));
        final int diff2 = 512 - neurLen2;
        final int zStart2 = Math.round(diff2 / 2);
        final int zEnd2 = zStart2 + neurLen2;
        
        //****** FOR LV, WE ARE FLIPPING THE NEURON....SO THE ZMIDDLE USES A LENGTH OF 6.93 INSTEAD OF THE NORMAL 14.44
        //final int r8Len = Math.round((float) (6.93 / resols[2]));
        final int r8Len2 = Math.round((float) (15.323 / resols[2]));
        
        final int zMiddle2 = zStart2 + r8Len2;
        
        
        
        //10/4/2012
        //notice the top layer has been shifted in x direction by 1 and in y direction by 1.2
        //so we need to add this delta to the first layer
        //in mipav space, we prob need to ADD the x direction delta and SUBTRACT the y direction delta
        final int xDelta = Math.round((float) (1/ resols[0]));
        final int yDelta = Math.round((float) (1.2/ resols[1]));
        
        

        //27 Points: 9 per layer
        //7.94*2 = 15.88
        //15.88 / res[0] =  141.75
        //512 - 141.75 = 370.25
        
        
        
        //370.25/2 = 185.125
        
        
        
        //512-185.125=326.875 


        //7.94 / res[0] = 70.58
        
        
        //512 - 70.58 = 441.42
        
        //441.42/2 = 220.71    
        //512 - 220.71 -   291.3

        
        
        
        //2/28/2014
        //for 25 points per layer
        //15.88615*2 = 31.7723
        //31.7723/res[0 = 283.61
        //512 - 283.61 = 228.39
        //228.39/2=114.195
        
        
        
        
        
        
        
        
        
        
        
        
        
        

        //27 Points: 9 per layer///NISH
        //8.74*2 = 17.48
        //17.48 / res[0] =  155.4
        //512 - 155.4 = 356.6
        //356.6/2 = 178 
        //512-178 = 334  

        //4.37*2 = 8.74
        //8.74 / res[0] = 77.7
        //512 - 77.7 = 434.3
        //434.3/2 = 217    
        //512 - 217 =  295 
        
        
        
        
        
        
        
        
        if(numPointsString.equals(PlugInDialogDrosophilaStandardColumnRegistration._147POINTS)) {
        	
        	if(rvld) {
			        	// top
				        //A
				        standardColumnImage.set(23, 256, zStart, 100);
				        x[0] = 23;
				        y[0] = 256;
				        z[0] = zStart;
				        newPtVOI.importCurve(x, y, z);
				        
				        //B
				        standardColumnImage.set(62, 295, zStart, 100);
				        x[0] = 62;
				        y[0] = 295;
				        z[0] = zStart;
				        newPtVOI.importCurve(x, y, z);
				        
				        //C
				        standardColumnImage.set(101, 334, zStart, 100);
				        x[0] = 101;
				        y[0] = 334;
				        z[0] = zStart;
				        newPtVOI.importCurve(x, y, z);
				        
				        //D
				        standardColumnImage.set(139, 373, zStart, 100);
				        x[0] = 139;
				        y[0] = 373;
				        z[0] = zStart;
				        newPtVOI.importCurve(x, y, z);
				        
				        //E
				        standardColumnImage.set(178, 411, zStart, 100);
				        x[0] = 178;
				        y[0] = 411;
				        z[0] = zStart;
				        newPtVOI.importCurve(x, y, z);
				        
				        //F
				        standardColumnImage.set(217, 450, zStart, 100);
				        x[0] = 217;
				        y[0] = 450;
				        z[0] = zStart;
				        newPtVOI.importCurve(x, y, z);
				        
				        //G
				        standardColumnImage.set(256, 489, zStart, 100);
				        x[0] = 256;
				        y[0] = 489;
				        z[0] = zStart;
				        newPtVOI.importCurve(x, y, z);
				        
			
				        
				        
				        //H
				        standardColumnImage.set(295, 450, zStart, 100);
				        x[0] = 295;
				        y[0] = 450;
				        z[0] = zStart;
				        newPtVOI.importCurve(x, y, z);
				        
				        //I
				        standardColumnImage.set(334, 411, zStart, 100);
				        x[0] = 334;
				        y[0] = 411;
				        z[0] = zStart;
				        newPtVOI.importCurve(x, y, z);
				        
				        //J
				        standardColumnImage.set(373, 373, zStart, 100);
				        x[0] = 373;
				        y[0] = 373;
				        z[0] = zStart;
				        newPtVOI.importCurve(x, y, z);
				        
				        //K
				        standardColumnImage.set(411, 334, zStart, 100);
				        x[0] = 411;
				        y[0] = 334;
				        z[0] = zStart;
				        newPtVOI.importCurve(x, y, z);
				        
				        //L
				        standardColumnImage.set(450, 295, zStart, 100);
				        x[0] = 450;
				        y[0] = 295;
				        z[0] = zStart;
				        newPtVOI.importCurve(x, y, z);
				        
				        //M
				        standardColumnImage.set(489, 256, zStart, 100);
				        x[0] = 489;
				        y[0] = 256;
				        z[0] = zStart;
				        newPtVOI.importCurve(x, y, z);
				        
				        
				        
				        //N
				        standardColumnImage.set(450, 217, zStart, 100);
				        x[0] = 450;
				        y[0] = 217;
				        z[0] = zStart;
				        newPtVOI.importCurve(x, y, z);
				        
				        //O
				        standardColumnImage.set(411, 178, zStart, 100);
				        x[0] = 411;
				        y[0] = 178;
				        z[0] = zStart;
				        newPtVOI.importCurve(x, y, z);
				        
				        //P
				        standardColumnImage.set(373, 139, zStart, 100);
				        x[0] = 373;
				        y[0] = 139;
				        z[0] = zStart;
				        newPtVOI.importCurve(x, y, z);
				        
				        //Q
				        standardColumnImage.set(334, 101, zStart, 100);
				        x[0] = 334;
				        y[0] = 101;
				        z[0] = zStart;
				        newPtVOI.importCurve(x, y, z);
				        
				        //R
				        standardColumnImage.set(295, 62, zStart, 100);
				        x[0] = 295;
				        y[0] = 62;
				        z[0] = zStart;
				        newPtVOI.importCurve(x, y, z);
				        
				        //S
				        standardColumnImage.set(256, 23, zStart, 100);
				        x[0] = 256;
				        y[0] = 23;
				        z[0] = zStart;
				        newPtVOI.importCurve(x, y, z);
				        
				        
				        
				        //T
				        standardColumnImage.set(217, 62, zStart, 100);
				        x[0] = 217;
				        y[0] = 62;
				        z[0] = zStart;
				        newPtVOI.importCurve(x, y, z);
				        
				        //U
				        standardColumnImage.set(178, 101, zStart, 100);
				        x[0] = 178;
				        y[0] = 101;
				        z[0] = zStart;
				        newPtVOI.importCurve(x, y, z);
				        
				        //V
				        standardColumnImage.set(139, 139, zStart, 100);
				        x[0] = 139;
				        y[0] = 139;
				        z[0] = zStart;
				        newPtVOI.importCurve(x, y, z);
				        
				        //W
				        standardColumnImage.set(101, 178, zStart, 100);
				        x[0] = 101;
				        y[0] = 178;
				        z[0] = zStart;
				        newPtVOI.importCurve(x, y, z);
				        
				        //X
				        standardColumnImage.set(62, 217, zStart, 100);
				        x[0] = 62;
				        y[0] = 217;
				        z[0] = zStart;
				        newPtVOI.importCurve(x, y, z);
				        
				        
				        
				        //A
				        standardColumnImage.set(101, 256, zStart, 100);
				        x[0] = 101;
				        y[0] = 256;
				        z[0] = zStart;
				        newPtVOI.importCurve(x, y, z);
				        
				        //B
				        standardColumnImage.set(139, 295, zStart, 100);
				        x[0] = 139;
				        y[0] = 295;
				        z[0] = zStart;
				        newPtVOI.importCurve(x, y, z);
				        
				        //C
				        standardColumnImage.set(178, 334, zStart, 100);
				        x[0] = 178;
				        y[0] = 334;
				        z[0] = zStart;
				        newPtVOI.importCurve(x, y, z);
				        
				        //D
				        standardColumnImage.set(217, 373, zStart, 100);
				        x[0] = 217;
				        y[0] = 373;
				        z[0] = zStart;
				        newPtVOI.importCurve(x, y, z);
				        
				        //E
				        standardColumnImage.set(256, 411, zStart, 100);
				        x[0] = 256;
				        y[0] = 411;
				        z[0] = zStart;
				        newPtVOI.importCurve(x, y, z);
				        
				        //F
				        standardColumnImage.set(295, 373, zStart, 100);
				        x[0] = 295;
				        y[0] = 373;
				        z[0] = zStart;
				        newPtVOI.importCurve(x, y, z);
				        
				        //G
				        standardColumnImage.set(334, 334, zStart, 100);
				        x[0] = 334;
				        y[0] = 334;
				        z[0] = zStart;
				        newPtVOI.importCurve(x, y, z);
				        
				        //H
				        standardColumnImage.set(373, 295, zStart, 100);
				        x[0] = 373;
				        y[0] = 295;
				        z[0] = zStart;
				        newPtVOI.importCurve(x, y, z);
				        
				        //I
				        standardColumnImage.set(411, 256, zStart, 100);
				        x[0] = 411;
				        y[0] = 256;
				        z[0] = zStart;
				        newPtVOI.importCurve(x, y, z);
				        
				        //J
				        standardColumnImage.set(373, 217, zStart, 100);
				        x[0] = 373;
				        y[0] = 217;
				        z[0] = zStart;
				        newPtVOI.importCurve(x, y, z);
				        
				        //K
				        standardColumnImage.set(334, 178, zStart, 100);
				        x[0] = 334;
				        y[0] = 178;
				        z[0] = zStart;
				        newPtVOI.importCurve(x, y, z);
				        
				        //L
				        standardColumnImage.set(295, 139, zStart, 100);
				        x[0] = 295;
				        y[0] = 139;
				        z[0] = zStart;
				        newPtVOI.importCurve(x, y, z);
				        
				        //M
				        standardColumnImage.set(256, 101, zStart, 100);
				        x[0] = 256;
				        y[0] = 101;
				        z[0] = zStart;
				        newPtVOI.importCurve(x, y, z);
				        
				        //N
				        standardColumnImage.set(217, 139, zStart, 100);
				        x[0] = 217;
				        y[0] = 139;
				        z[0] = zStart;
				        newPtVOI.importCurve(x, y, z);
				        
				        //O
				        standardColumnImage.set(178, 178, zStart, 100);
				        x[0] = 178;
				        y[0] = 178;
				        z[0] = zStart;
				        newPtVOI.importCurve(x, y, z);
				        
				        //P
				        standardColumnImage.set(139, 217, zStart, 100);
				        x[0] = 139;
				        y[0] = 217;
				        z[0] = zStart;
				        newPtVOI.importCurve(x, y, z);
				        
				        //Q
				        standardColumnImage.set(178, 256, zStart, 100);
				        x[0] = 178;
				        y[0] = 256;
				        z[0] = zStart;
				        newPtVOI.importCurve(x, y, z);
				        
				        //R
				        standardColumnImage.set(217, 295, zStart, 100);
				        x[0] = 217;
				        y[0] = 295;
				        z[0] = zStart;
				        newPtVOI.importCurve(x, y, z);
				        
				        //S
				        standardColumnImage.set(256, 334, zStart, 100);
				        x[0] = 256;
				        y[0] = 334;
				        z[0] = zStart;
				        newPtVOI.importCurve(x, y, z);
				        
				        //T
				        standardColumnImage.set(295, 295, zStart, 100);
				        x[0] = 295;
				        y[0] = 295;
				        z[0] = zStart;
				        newPtVOI.importCurve(x, y, z);
				        
				        //U
				        standardColumnImage.set(334, 256, zStart, 100);
				        x[0] = 334;
				        y[0] = 256;
				        z[0] = zStart;
				        newPtVOI.importCurve(x, y, z);
				        
				        //V
				        standardColumnImage.set(295, 217, zStart, 100);
				        x[0] = 295;
				        y[0] = 217;
				        z[0] = zStart;
				        newPtVOI.importCurve(x, y, z);
				        
				        //W
				        standardColumnImage.set(256, 178, zStart, 100);
				        x[0] = 256;
				        y[0] = 178;
				        z[0] = zStart;
				        newPtVOI.importCurve(x, y, z);
				        
				        //X
				        standardColumnImage.set(217, 217, zStart, 100);
				        x[0] = 217;
				        y[0] = 217;
				        z[0] = zStart;
				        newPtVOI.importCurve(x, y, z);
				        
				        //Y
				        standardColumnImage.set(256, 256, zStart, 100);
				        x[0] = 256;
				        y[0] = 256;
				        z[0] = zStart;
				        newPtVOI.importCurve(x, y, z);
				        
				        
			
				        //r8
				        //A
				        standardColumnImage.set(23, 256, zMiddle, 100);
				        x[0] = 23;
				        y[0] = 256;
				        z[0] = zMiddle;
				        newPtVOI.importCurve(x, y, z);
				        
				        //B
				        standardColumnImage.set(62, 295, zMiddle, 100);
				        x[0] = 62;
				        y[0] = 295;
				        z[0] = zMiddle;
				        newPtVOI.importCurve(x, y, z);
				        
				        //C
				        standardColumnImage.set(101, 334, zMiddle, 100);
				        x[0] = 101;
				        y[0] = 334;
				        z[0] = zMiddle;
				        newPtVOI.importCurve(x, y, z);
				        
				        //D
				        standardColumnImage.set(139, 373, zMiddle, 100);
				        x[0] = 139;
				        y[0] = 373;
				        z[0] = zMiddle;
				        newPtVOI.importCurve(x, y, z);
				        
				        //E
				        standardColumnImage.set(178, 411, zMiddle, 100);
				        x[0] = 178;
				        y[0] = 411;
				        z[0] = zMiddle;
				        newPtVOI.importCurve(x, y, z);
				        
				        //F
				        standardColumnImage.set(217, 450, zMiddle, 100);
				        x[0] = 217;
				        y[0] = 450;
				        z[0] = zMiddle;
				        newPtVOI.importCurve(x, y, z);
				        
				        //G
				        standardColumnImage.set(256, 489, zMiddle, 100);
				        x[0] = 256;
				        y[0] = 489;
				        z[0] = zMiddle;
				        newPtVOI.importCurve(x, y, z);
				        
			
				        
				        
				        //H
				        standardColumnImage.set(295, 450, zMiddle, 100);
				        x[0] = 295;
				        y[0] = 450;
				        z[0] = zMiddle;
				        newPtVOI.importCurve(x, y, z);
				        
				        //I
				        standardColumnImage.set(334, 411, zMiddle, 100);
				        x[0] = 334;
				        y[0] = 411;
				        z[0] = zMiddle;
				        newPtVOI.importCurve(x, y, z);
				        
				        //J
				        standardColumnImage.set(373, 373, zMiddle, 100);
				        x[0] = 373;
				        y[0] = 373;
				        z[0] = zMiddle;
				        newPtVOI.importCurve(x, y, z);
				        
				        //K
				        standardColumnImage.set(411, 334, zMiddle, 100);
				        x[0] = 411;
				        y[0] = 334;
				        z[0] = zMiddle;
				        newPtVOI.importCurve(x, y, z);
				        
				        //L
				        standardColumnImage.set(450, 295, zMiddle, 100);
				        x[0] = 450;
				        y[0] = 295;
				        z[0] = zMiddle;
				        newPtVOI.importCurve(x, y, z);
				        
				        //M
				        standardColumnImage.set(489, 256, zMiddle, 100);
				        x[0] = 489;
				        y[0] = 256;
				        z[0] = zMiddle;
				        newPtVOI.importCurve(x, y, z);
				        
				        //N
				        standardColumnImage.set(450, 217, zMiddle, 100);
				        x[0] = 450;
				        y[0] = 217;
				        z[0] = zMiddle;
				        newPtVOI.importCurve(x, y, z);
				        
				        //O
				        standardColumnImage.set(411, 178, zMiddle, 100);
				        x[0] = 411;
				        y[0] = 178;
				        z[0] = zMiddle;
				        newPtVOI.importCurve(x, y, z);
				        
				        //P
				        standardColumnImage.set(373, 139, zMiddle, 100);
				        x[0] = 373;
				        y[0] = 139;
				        z[0] = zMiddle;
				        newPtVOI.importCurve(x, y, z);
				        
				        //Q
				        standardColumnImage.set(334, 101, zMiddle, 100);
				        x[0] = 334;
				        y[0] = 101;
				        z[0] = zMiddle;
				        newPtVOI.importCurve(x, y, z);
				        
				        //R
				        standardColumnImage.set(295, 62, zMiddle, 100);
				        x[0] = 295;
				        y[0] = 62;
				        z[0] = zMiddle;
				        newPtVOI.importCurve(x, y, z);
				        
				        //S
				        standardColumnImage.set(256, 23, zMiddle, 100);
				        x[0] = 256;
				        y[0] = 23;
				        z[0] = zMiddle;
				        newPtVOI.importCurve(x, y, z);
				        
				        //T
				        standardColumnImage.set(217, 62, zMiddle, 100);
				        x[0] = 217;
				        y[0] = 62;
				        z[0] = zMiddle;
				        newPtVOI.importCurve(x, y, z);
				        
				        //U
				        standardColumnImage.set(178, 101, zMiddle, 100);
				        x[0] = 178;
				        y[0] = 101;
				        z[0] = zMiddle;
				        newPtVOI.importCurve(x, y, z);
				        
				        //V
				        standardColumnImage.set(139, 139, zMiddle, 100);
				        x[0] = 139;
				        y[0] = 139;
				        z[0] = zMiddle;
				        newPtVOI.importCurve(x, y, z);
				        
				        //W
				        standardColumnImage.set(101, 178, zMiddle, 100);
				        x[0] = 101;
				        y[0] = 178;
				        z[0] = zMiddle;
				        newPtVOI.importCurve(x, y, z);
				        
				        //X
				        standardColumnImage.set(62, 217, zMiddle, 100);
				        x[0] = 62;
				        y[0] = 217;
				        z[0] = zMiddle;
				        newPtVOI.importCurve(x, y, z);
				        
				        //A
				        standardColumnImage.set(101, 256, zMiddle, 100);
				        x[0] = 101;
				        y[0] = 256;
				        z[0] = zMiddle;
				        newPtVOI.importCurve(x, y, z);
				        
				        //B
				        standardColumnImage.set(139, 295, zMiddle, 100);
				        x[0] = 139;
				        y[0] = 295;
				        z[0] = zMiddle;
				        newPtVOI.importCurve(x, y, z);
				        
				        //C
				        standardColumnImage.set(178, 334, zMiddle, 100);
				        x[0] = 178;
				        y[0] = 334;
				        z[0] = zMiddle;
				        newPtVOI.importCurve(x, y, z);
				        
				        //D
				        standardColumnImage.set(217, 373, zMiddle, 100);
				        x[0] = 217;
				        y[0] = 373;
				        z[0] = zMiddle;
				        newPtVOI.importCurve(x, y, z);
				        
				        //E
				        standardColumnImage.set(256, 411, zMiddle, 100);
				        x[0] = 256;
				        y[0] = 411;
				        z[0] = zMiddle;
				        newPtVOI.importCurve(x, y, z);
				        
				        //F
				        standardColumnImage.set(295, 373, zMiddle, 100);
				        x[0] = 295;
				        y[0] = 373;
				        z[0] = zMiddle;
				        newPtVOI.importCurve(x, y, z);
				        
				        //G
				        standardColumnImage.set(334, 334, zMiddle, 100);
				        x[0] = 334;
				        y[0] = 334;
				        z[0] = zMiddle;
				        newPtVOI.importCurve(x, y, z);
				        
				        //H
				        standardColumnImage.set(373, 295, zMiddle, 100);
				        x[0] = 373;
				        y[0] = 295;
				        z[0] = zMiddle;
				        newPtVOI.importCurve(x, y, z);
				        
				        //I
				        standardColumnImage.set(411, 256, zMiddle, 100);
				        x[0] = 411;
				        y[0] = 256;
				        z[0] = zMiddle;
				        newPtVOI.importCurve(x, y, z);
				        
				        //J
				        standardColumnImage.set(373, 217, zMiddle, 100);
				        x[0] = 373;
				        y[0] = 217;
				        z[0] = zMiddle;
				        newPtVOI.importCurve(x, y, z);
				        
				        //K
				        standardColumnImage.set(334, 178, zMiddle, 100);
				        x[0] = 334;
				        y[0] = 178;
				        z[0] = zMiddle;
				        newPtVOI.importCurve(x, y, z);
				        
				        //L
				        standardColumnImage.set(295, 139, zMiddle, 100);
				        x[0] = 295;
				        y[0] = 139;
				        z[0] = zMiddle;
				        newPtVOI.importCurve(x, y, z);
				        
				        //M
				        standardColumnImage.set(256, 101, zMiddle, 100);
				        x[0] = 256;
				        y[0] = 101;
				        z[0] = zMiddle;
				        newPtVOI.importCurve(x, y, z);
				        
				        //N
				        standardColumnImage.set(217, 139, zMiddle, 100);
				        x[0] = 217;
				        y[0] = 139;
				        z[0] = zMiddle;
				        newPtVOI.importCurve(x, y, z);
				        
				        //O
				        standardColumnImage.set(178, 178, zMiddle, 100);
				        x[0] = 178;
				        y[0] = 178;
				        z[0] = zMiddle;
				        newPtVOI.importCurve(x, y, z);
				        
				        //P
				        standardColumnImage.set(139, 217, zMiddle, 100);
				        x[0] = 139;
				        y[0] = 217;
				        z[0] = zMiddle;
				        newPtVOI.importCurve(x, y, z);
				        
				        //Q
				        standardColumnImage.set(178, 256, zMiddle, 100);
				        x[0] = 178;
				        y[0] = 256;
				        z[0] = zMiddle;
				        newPtVOI.importCurve(x, y, z);
				        
				        //R
				        standardColumnImage.set(217, 295, zMiddle, 100);
				        x[0] = 217;
				        y[0] = 295;
				        z[0] = zMiddle;
				        newPtVOI.importCurve(x, y, z);
				        
				        //S
				        standardColumnImage.set(256, 334, zMiddle, 100);
				        x[0] = 256;
				        y[0] = 334;
				        z[0] = zMiddle;
				        newPtVOI.importCurve(x, y, z);
				        
				        //T
				        standardColumnImage.set(295, 295, zMiddle, 100);
				        x[0] = 295;
				        y[0] = 295;
				        z[0] = zMiddle;
				        newPtVOI.importCurve(x, y, z);
				        
				        //U
				        standardColumnImage.set(334, 256, zMiddle, 100);
				        x[0] = 334;
				        y[0] = 256;
				        z[0] = zMiddle;
				        newPtVOI.importCurve(x, y, z);
				        
				        //V
				        standardColumnImage.set(295, 217, zMiddle, 100);
				        x[0] = 295;
				        y[0] = 217;
				        z[0] = zMiddle;
				        newPtVOI.importCurve(x, y, z);
				        
				        //W
				        standardColumnImage.set(256, 178, zMiddle, 100);
				        x[0] = 256;
				        y[0] = 178;
				        z[0] = zMiddle;
				        newPtVOI.importCurve(x, y, z);
				        
				        //X
				        standardColumnImage.set(217, 217, zMiddle, 100);
				        x[0] = 217;
				        y[0] = 217;
				        z[0] = zMiddle;
				        newPtVOI.importCurve(x, y, z);
				        
				        //Y
				        standardColumnImage.set(256, 256, zMiddle, 100);
				        x[0] = 256;
				        y[0] = 256;
				        z[0] = zMiddle;
				        newPtVOI.importCurve(x, y, z);
				        
				        
				        
				        
				        //--
				     // r7
				        //A
				        standardColumnImage.set(23, 256, zEnd, 100);
				        x[0] = 23;
				        y[0] = 256;
				        z[0] = zEnd;
				        newPtVOI.importCurve(x, y, z);
				        
				        //B
				        standardColumnImage.set(62, 295, zEnd, 100);
				        x[0] = 62;
				        y[0] = 295;
				        z[0] = zEnd;
				        newPtVOI.importCurve(x, y, z);
				        
				        //C
				        standardColumnImage.set(101, 334, zEnd, 100);
				        x[0] = 101;
				        y[0] = 334;
				        z[0] = zEnd;
				        newPtVOI.importCurve(x, y, z);
				        
				        //D
				        standardColumnImage.set(139, 373, zEnd, 100);
				        x[0] = 139;
				        y[0] = 373;
				        z[0] = zEnd;
				        newPtVOI.importCurve(x, y, z);
				        
				        //E
				        standardColumnImage.set(178, 411, zEnd, 100);
				        x[0] = 178;
				        y[0] = 411;
				        z[0] = zEnd;
				        newPtVOI.importCurve(x, y, z);
				        
				        //F
				        standardColumnImage.set(217, 450, zEnd, 100);
				        x[0] = 217;
				        y[0] = 450;
				        z[0] = zEnd;
				        newPtVOI.importCurve(x, y, z);
				        
				        //G
				        standardColumnImage.set(256, 489, zEnd, 100);
				        x[0] = 256;
				        y[0] = 489;
				        z[0] = zEnd;
				        newPtVOI.importCurve(x, y, z);
				        
			
				        
				        
				        //H
				        standardColumnImage.set(295, 450, zEnd, 100);
				        x[0] = 295;
				        y[0] = 450;
				        z[0] = zEnd;
				        newPtVOI.importCurve(x, y, z);
				        
				        //I
				        standardColumnImage.set(334, 411, zEnd, 100);
				        x[0] = 334;
				        y[0] = 411;
				        z[0] = zEnd;
				        newPtVOI.importCurve(x, y, z);
				        
				        //J
				        standardColumnImage.set(373, 373, zEnd, 100);
				        x[0] = 373;
				        y[0] = 373;
				        z[0] = zEnd;
				        newPtVOI.importCurve(x, y, z);
				        
				        //K
				        standardColumnImage.set(411, 334, zEnd, 100);
				        x[0] = 411;
				        y[0] = 334;
				        z[0] = zEnd;
				        newPtVOI.importCurve(x, y, z);
				        
				        //L
				        standardColumnImage.set(450, 295, zEnd, 100);
				        x[0] = 450;
				        y[0] = 295;
				        z[0] = zEnd;
				        newPtVOI.importCurve(x, y, z);
				        
				        //M
				        standardColumnImage.set(489, 256, zEnd, 100);
				        x[0] = 489;
				        y[0] = 256;
				        z[0] = zEnd;
				        newPtVOI.importCurve(x, y, z);
				        
				        //N
				        standardColumnImage.set(450, 217, zEnd, 100);
				        x[0] = 450;
				        y[0] = 217;
				        z[0] = zEnd;
				        newPtVOI.importCurve(x, y, z);
				        
				        //O
				        standardColumnImage.set(411, 178, zEnd, 100);
				        x[0] = 411;
				        y[0] = 178;
				        z[0] = zEnd;
				        newPtVOI.importCurve(x, y, z);
				        
				        //P
				        standardColumnImage.set(373, 139, zEnd, 100);
				        x[0] = 373;
				        y[0] = 139;
				        z[0] = zEnd;
				        newPtVOI.importCurve(x, y, z);
				        
				        //Q
				        standardColumnImage.set(334, 101, zEnd, 100);
				        x[0] = 334;
				        y[0] = 101;
				        z[0] = zEnd;
				        newPtVOI.importCurve(x, y, z);
				        
				        //R
				        standardColumnImage.set(295, 62, zEnd, 100);
				        x[0] = 295;
				        y[0] = 62;
				        z[0] = zEnd;
				        newPtVOI.importCurve(x, y, z);
				        
				        //S
				        standardColumnImage.set(256, 23, zEnd, 100);
				        x[0] = 256;
				        y[0] = 23;
				        z[0] = zEnd;
				        newPtVOI.importCurve(x, y, z);
				        
				        //T
				        standardColumnImage.set(217, 62, zEnd, 100);
				        x[0] = 217;
				        y[0] = 62;
				        z[0] = zEnd;
				        newPtVOI.importCurve(x, y, z);
				        
				        //U
				        standardColumnImage.set(178, 101, zEnd, 100);
				        x[0] = 178;
				        y[0] = 101;
				        z[0] = zEnd;
				        newPtVOI.importCurve(x, y, z);
				        
				        //V
				        standardColumnImage.set(139, 139, zEnd, 100);
				        x[0] = 139;
				        y[0] = 139;
				        z[0] = zEnd;
				        newPtVOI.importCurve(x, y, z);
				        
				        //W
				        standardColumnImage.set(101, 178, zEnd, 100);
				        x[0] = 101;
				        y[0] = 178;
				        z[0] = zEnd;
				        newPtVOI.importCurve(x, y, z);
				        
				        //X
				        standardColumnImage.set(62, 217, zEnd, 100);
				        x[0] = 62;
				        y[0] = 217;
				        z[0] = zEnd;
				        newPtVOI.importCurve(x, y, z);
				        
				        //A
				        standardColumnImage.set(101, 256, zEnd, 100);
				        x[0] = 101;
				        y[0] = 256;
				        z[0] = zEnd;
				        newPtVOI.importCurve(x, y, z);
				        
				        //B
				        standardColumnImage.set(139, 295, zEnd, 100);
				        x[0] = 139;
				        y[0] = 295;
				        z[0] = zEnd;
				        newPtVOI.importCurve(x, y, z);
				        
				        //C
				        standardColumnImage.set(178, 334, zEnd, 100);
				        x[0] = 178;
				        y[0] = 334;
				        z[0] = zEnd;
				        newPtVOI.importCurve(x, y, z);
				        
				        //D
				        standardColumnImage.set(217, 373, zEnd, 100);
				        x[0] = 217;
				        y[0] = 373;
				        z[0] = zEnd;
				        newPtVOI.importCurve(x, y, z);
				        
				        //E
				        standardColumnImage.set(256, 411, zEnd, 100);
				        x[0] = 256;
				        y[0] = 411;
				        z[0] = zEnd;
				        newPtVOI.importCurve(x, y, z);
				        
				        //F
				        standardColumnImage.set(295, 373, zEnd, 100);
				        x[0] = 295;
				        y[0] = 373;
				        z[0] = zEnd;
				        newPtVOI.importCurve(x, y, z);
				        
				        //G
				        standardColumnImage.set(334, 334, zEnd, 100);
				        x[0] = 334;
				        y[0] = 334;
				        z[0] = zEnd;
				        newPtVOI.importCurve(x, y, z);
				        
				        //H
				        standardColumnImage.set(373, 295, zEnd, 100);
				        x[0] = 373;
				        y[0] = 295;
				        z[0] = zEnd;
				        newPtVOI.importCurve(x, y, z);
				        
				        //I
				        standardColumnImage.set(411, 256, zEnd, 100);
				        x[0] = 411;
				        y[0] = 256;
				        z[0] = zEnd;
				        newPtVOI.importCurve(x, y, z);
				        
				        //J
				        standardColumnImage.set(373, 217, zEnd, 100);
				        x[0] = 373;
				        y[0] = 217;
				        z[0] = zEnd;
				        newPtVOI.importCurve(x, y, z);
				        
				        //K
				        standardColumnImage.set(334, 178, zEnd, 100);
				        x[0] = 334;
				        y[0] = 178;
				        z[0] = zEnd;
				        newPtVOI.importCurve(x, y, z);
				        
				        //L
				        standardColumnImage.set(295, 139, zEnd, 100);
				        x[0] = 295;
				        y[0] = 139;
				        z[0] = zEnd;
				        newPtVOI.importCurve(x, y, z);
				        
				        //M
				        standardColumnImage.set(256, 101, zEnd, 100);
				        x[0] = 256;
				        y[0] = 101;
				        z[0] = zEnd;
				        newPtVOI.importCurve(x, y, z);
				        
				        //N
				        standardColumnImage.set(217, 139, zEnd, 100);
				        x[0] = 217;
				        y[0] = 139;
				        z[0] = zEnd;
				        newPtVOI.importCurve(x, y, z);
				        
				        //O
				        standardColumnImage.set(178, 178, zEnd, 100);
				        x[0] = 178;
				        y[0] = 178;
				        z[0] = zEnd;
				        newPtVOI.importCurve(x, y, z);
				        
				        //P
				        standardColumnImage.set(139, 217, zEnd, 100);
				        x[0] = 139;
				        y[0] = 217;
				        z[0] = zEnd;
				        newPtVOI.importCurve(x, y, z);
				        
				        //Q
				        standardColumnImage.set(178, 256, zEnd, 100);
				        x[0] = 178;
				        y[0] = 256;
				        z[0] = zEnd;
				        newPtVOI.importCurve(x, y, z);
				        
				        //R
				        standardColumnImage.set(217, 295, zEnd, 100);
				        x[0] = 217;
				        y[0] = 295;
				        z[0] = zEnd;
				        newPtVOI.importCurve(x, y, z);
				        
				        //S
				        standardColumnImage.set(256, 334, zEnd, 100);
				        x[0] = 256;
				        y[0] = 334;
				        z[0] = zEnd;
				        newPtVOI.importCurve(x, y, z);
				        
				        //T
				        standardColumnImage.set(295, 295, zEnd, 100);
				        x[0] = 295;
				        y[0] = 295;
				        z[0] = zEnd;
				        newPtVOI.importCurve(x, y, z);
				        
				        //U
				        standardColumnImage.set(334, 256, zEnd, 100);
				        x[0] = 334;
				        y[0] = 256;
				        z[0] = zEnd;
				        newPtVOI.importCurve(x, y, z);
				        
				        //V
				        standardColumnImage.set(295, 217, zEnd, 100);
				        x[0] = 295;
				        y[0] = 217;
				        z[0] = zEnd;
				        newPtVOI.importCurve(x, y, z);
				        
				        //W
				        standardColumnImage.set(256, 178, zEnd, 100);
				        x[0] = 256;
				        y[0] = 178;
				        z[0] = zEnd;
				        newPtVOI.importCurve(x, y, z);
				        
				        //X
				        standardColumnImage.set(217, 217, zEnd, 100);
				        x[0] = 217;
				        y[0] = 217;
				        z[0] = zEnd;
				        newPtVOI.importCurve(x, y, z);
				        
				        //Y
				        standardColumnImage.set(256, 256, zEnd, 100);
				        x[0] = 256;
				        y[0] = 256;
				        z[0] = zEnd;
				        newPtVOI.importCurve(x, y, z);
        	}else {
        		// top
		        //A
		        standardColumnImage.set(23, 256, zStart, 100);
		        x[0] = 23;
		        y[0] = 256;
		        z[0] = zStart;
		        newPtVOI.importCurve(x, y, z);
		        
		      //X
		        standardColumnImage.set(62, 217, zStart, 100);
		        x[0] = 62;
		        y[0] = 217;
		        z[0] = zStart;
		        newPtVOI.importCurve(x, y, z);
		        

		        
		        //W
		        standardColumnImage.set(101, 178, zStart, 100);
		        x[0] = 101;
		        y[0] = 178;
		        z[0] = zStart;
		        newPtVOI.importCurve(x, y, z);
		        
		      //V
		        standardColumnImage.set(139, 139, zStart, 100);
		        x[0] = 139;
		        y[0] = 139;
		        z[0] = zStart;
		        newPtVOI.importCurve(x, y, z);
		        
		      //U
		        standardColumnImage.set(178, 101, zStart, 100);
		        x[0] = 178;
		        y[0] = 101;
		        z[0] = zStart;
		        newPtVOI.importCurve(x, y, z);
		        
		        //T
		        standardColumnImage.set(217, 62, zStart, 100);
		        x[0] = 217;
		        y[0] = 62;
		        z[0] = zStart;
		        newPtVOI.importCurve(x, y, z);

		        //S
		        standardColumnImage.set(256, 23, zStart, 100);
		        x[0] = 256;
		        y[0] = 23;
		        z[0] = zStart;
		        newPtVOI.importCurve(x, y, z);
		        
		        //R
		        standardColumnImage.set(295, 62, zStart, 100);
		        x[0] = 295;
		        y[0] = 62;
		        z[0] = zStart;
		        newPtVOI.importCurve(x, y, z);
		        
		        //Q
		        standardColumnImage.set(334, 101, zStart, 100);
		        x[0] = 334;
		        y[0] = 101;
		        z[0] = zStart;
		        newPtVOI.importCurve(x, y, z);
		        
		      //P
		        standardColumnImage.set(373, 139, zStart, 100);
		        x[0] = 373;
		        y[0] = 139;
		        z[0] = zStart;
		        newPtVOI.importCurve(x, y, z);

		        //O
		        standardColumnImage.set(411, 178, zStart, 100);
		        x[0] = 411;
		        y[0] = 178;
		        z[0] = zStart;
		        newPtVOI.importCurve(x, y, z);
		        
		        //N
		        standardColumnImage.set(450, 217, zStart, 100);
		        x[0] = 450;
		        y[0] = 217;
		        z[0] = zStart;
		        newPtVOI.importCurve(x, y, z);
		        
		        //M
		        standardColumnImage.set(489, 256, zStart, 100);
		        x[0] = 489;
		        y[0] = 256;
		        z[0] = zStart;
		        newPtVOI.importCurve(x, y, z);

		        //L
		        standardColumnImage.set(450, 295, zStart, 100);
		        x[0] = 450;
		        y[0] = 295;
		        z[0] = zStart;
		        newPtVOI.importCurve(x, y, z);
		        
		        //K
		        standardColumnImage.set(411, 334, zStart, 100);
		        x[0] = 411;
		        y[0] = 334;
		        z[0] = zStart;
		        newPtVOI.importCurve(x, y, z);
		        
		      //J
		        standardColumnImage.set(373, 373, zStart, 100);
		        x[0] = 373;
		        y[0] = 373;
		        z[0] = zStart;
		        newPtVOI.importCurve(x, y, z);

		        //I
		        standardColumnImage.set(334, 411, zStart, 100);
		        x[0] = 334;
		        y[0] = 411;
		        z[0] = zStart;
		        newPtVOI.importCurve(x, y, z);
		        
		      //H
		        standardColumnImage.set(295, 450, zStart, 100);
		        x[0] = 295;
		        y[0] = 450;
		        z[0] = zStart;
		        newPtVOI.importCurve(x, y, z);
		        
		      //G
		        standardColumnImage.set(256, 489, zStart, 100);
		        x[0] = 256;
		        y[0] = 489;
		        z[0] = zStart;
		        newPtVOI.importCurve(x, y, z);
		        
		      
		        
		        
		        
		        //F
		        standardColumnImage.set(217, 450, zStart, 100);
		        x[0] = 217;
		        y[0] = 450;
		        z[0] = zStart;
		        newPtVOI.importCurve(x, y, z);
		        
		      //E
		        standardColumnImage.set(178, 411, zStart, 100);
		        x[0] = 178;
		        y[0] = 411;
		        z[0] = zStart;
		        newPtVOI.importCurve(x, y, z);
		        
		      //D
		        standardColumnImage.set(139, 373, zStart, 100);
		        x[0] = 139;
		        y[0] = 373;
		        z[0] = zStart;
		        newPtVOI.importCurve(x, y, z);
		        
		      //C
		        standardColumnImage.set(101, 334, zStart, 100);
		        x[0] = 101;
		        y[0] = 334;
		        z[0] = zStart;
		        newPtVOI.importCurve(x, y, z);
		        
		        
		        //B
		        standardColumnImage.set(62, 295, zStart, 100);
		        x[0] = 62;
		        y[0] = 295;
		        z[0] = zStart;
		        newPtVOI.importCurve(x, y, z);
		        
		        
		        //A
    	        standardColumnImage.set(101, 256, zStart, 100);
    	        x[0] = 101;
    	        y[0] = 256;
    	        z[0] = zStart;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //P
    	        standardColumnImage.set(139, 217, zStart, 100);
    	        x[0] = 139;
    	        y[0] = 217;
    	        z[0] = zStart;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //O
    	        standardColumnImage.set(178, 178, zStart, 100);
    	        x[0] = 178;
    	        y[0] = 178;
    	        z[0] = zStart;
    	        newPtVOI.importCurve(x, y, z);

    	        //N
    	        standardColumnImage.set(217, 139, zStart, 100);
    	        x[0] = 217;
    	        y[0] = 139;
    	        z[0] = zStart;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //M
    	        standardColumnImage.set(256, 101, zStart, 100);
    	        x[0] = 256;
    	        y[0] = 101;
    	        z[0] = zStart;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //L
    	        standardColumnImage.set(295, 139, zStart, 100);
    	        x[0] = 295;
    	        y[0] = 139;
    	        z[0] = zStart;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //K
    	        standardColumnImage.set(334, 178, zStart, 100);
    	        x[0] = 334;
    	        y[0] = 178;
    	        z[0] = zStart;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //J
    	        standardColumnImage.set(373, 217, zStart, 100);
    	        x[0] = 373;
    	        y[0] = 217;
    	        z[0] = zStart;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //I
    	        standardColumnImage.set(411, 256, zStart, 100);
    	        x[0] = 411;
    	        y[0] = 256;
    	        z[0] = zStart;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //H
    	        standardColumnImage.set(373, 295, zStart, 100);
    	        x[0] = 373;
    	        y[0] = 295;
    	        z[0] = zStart;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //G
    	        standardColumnImage.set(334, 334, zStart, 100);
    	        x[0] = 334;
    	        y[0] = 334;
    	        z[0] = zStart;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //F
    	        standardColumnImage.set(295, 373, zStart, 100);
    	        x[0] = 295;
    	        y[0] = 373;
    	        z[0] = zStart;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //E
    	        standardColumnImage.set(256, 411, zStart, 100);
    	        x[0] = 256;
    	        y[0] = 411;
    	        z[0] = zStart;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //D
    	        standardColumnImage.set(217, 373, zStart, 100);
    	        x[0] = 217;
    	        y[0] = 373;
    	        z[0] = zStart;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //C
    	        standardColumnImage.set(178, 334, zStart, 100);
    	        x[0] = 178;
    	        y[0] = 334;
    	        z[0] = zStart;
    	        newPtVOI.importCurve(x, y, z);

    	        //B
    	        standardColumnImage.set(139, 295, zStart, 100);
    	        x[0] = 139;
    	        y[0] = 295;
    	        z[0] = zStart;
    	        newPtVOI.importCurve(x, y, z);

    	        //Q
    	        standardColumnImage.set(178, 256, zStart, 100);
    	        x[0] = 178;
    	        y[0] = 256;
    	        z[0] = zStart;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //X
    	        standardColumnImage.set(217, 217, zStart, 100);
    	        x[0] = 217;
    	        y[0] = 217;
    	        z[0] = zStart;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //W
    	        standardColumnImage.set(256, 178, zStart, 100);
    	        x[0] = 256;
    	        y[0] = 178;
    	        z[0] = zStart;
    	        newPtVOI.importCurve(x, y, z);

    	        //V
    	        standardColumnImage.set(295, 217, zStart, 100);
    	        x[0] = 295;
    	        y[0] = 217;
    	        z[0] = zStart;
    	        newPtVOI.importCurve(x, y, z);

    	        //U
    	        standardColumnImage.set(334, 256, zStart, 100);
    	        x[0] = 334;
    	        y[0] = 256;
    	        z[0] = zStart;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //T
    	        standardColumnImage.set(295, 295, zStart, 100);
    	        x[0] = 295;
    	        y[0] = 295;
    	        z[0] = zStart;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //S
    	        standardColumnImage.set(256, 334, zStart, 100);
    	        x[0] = 256;
    	        y[0] = 334;
    	        z[0] = zStart;
    	        newPtVOI.importCurve(x, y, z);

    	        //R
    	        standardColumnImage.set(217, 295, zStart, 100);
    	        x[0] = 217;
    	        y[0] = 295;
    	        z[0] = zStart;
    	        newPtVOI.importCurve(x, y, z);

    	        //Y
    	        standardColumnImage.set(256, 256, zStart, 100);
    	        x[0] = 256;
    	        y[0] = 256;
    	        z[0] = zStart;
    	        newPtVOI.importCurve(x, y, z);
		        
		        
		        
		        
		        //--
    	     // r8
		        //A
		        standardColumnImage.set(23, 256, zMiddle, 100);
		        x[0] = 23;
		        y[0] = 256;
		        z[0] = zMiddle;
		        newPtVOI.importCurve(x, y, z);
		        
		      //X
		        standardColumnImage.set(62, 217, zMiddle, 100);
		        x[0] = 62;
		        y[0] = 217;
		        z[0] = zMiddle;
		        newPtVOI.importCurve(x, y, z);
		        

		        
		        //W
		        standardColumnImage.set(101, 178, zMiddle, 100);
		        x[0] = 101;
		        y[0] = 178;
		        z[0] = zMiddle;
		        newPtVOI.importCurve(x, y, z);
		        
		      //V
		        standardColumnImage.set(139, 139, zMiddle, 100);
		        x[0] = 139;
		        y[0] = 139;
		        z[0] = zMiddle;
		        newPtVOI.importCurve(x, y, z);
		        
		      //U
		        standardColumnImage.set(178, 101, zMiddle, 100);
		        x[0] = 178;
		        y[0] = 101;
		        z[0] = zMiddle;
		        newPtVOI.importCurve(x, y, z);
		        
		        //T
		        standardColumnImage.set(217, 62, zMiddle, 100);
		        x[0] = 217;
		        y[0] = 62;
		        z[0] = zMiddle;
		        newPtVOI.importCurve(x, y, z);

		        //S
		        standardColumnImage.set(256, 23, zMiddle, 100);
		        x[0] = 256;
		        y[0] = 23;
		        z[0] = zMiddle;
		        newPtVOI.importCurve(x, y, z);
		        
		        //R
		        standardColumnImage.set(295, 62, zMiddle, 100);
		        x[0] = 295;
		        y[0] = 62;
		        z[0] = zMiddle;
		        newPtVOI.importCurve(x, y, z);
		        
		        //Q
		        standardColumnImage.set(334, 101, zMiddle, 100);
		        x[0] = 334;
		        y[0] = 101;
		        z[0] = zMiddle;
		        newPtVOI.importCurve(x, y, z);
		        
		      //P
		        standardColumnImage.set(373, 139, zMiddle, 100);
		        x[0] = 373;
		        y[0] = 139;
		        z[0] = zMiddle;
		        newPtVOI.importCurve(x, y, z);

		        //O
		        standardColumnImage.set(411, 178, zMiddle, 100);
		        x[0] = 411;
		        y[0] = 178;
		        z[0] = zMiddle;
		        newPtVOI.importCurve(x, y, z);
		        
		        //N
		        standardColumnImage.set(450, 217, zMiddle, 100);
		        x[0] = 450;
		        y[0] = 217;
		        z[0] = zMiddle;
		        newPtVOI.importCurve(x, y, z);
		        
		        //M
		        standardColumnImage.set(489, 256, zMiddle, 100);
		        x[0] = 489;
		        y[0] = 256;
		        z[0] = zMiddle;
		        newPtVOI.importCurve(x, y, z);

		        //L
		        standardColumnImage.set(450, 295, zMiddle, 100);
		        x[0] = 450;
		        y[0] = 295;
		        z[0] = zMiddle;
		        newPtVOI.importCurve(x, y, z);
		        
		        //K
		        standardColumnImage.set(411, 334, zMiddle, 100);
		        x[0] = 411;
		        y[0] = 334;
		        z[0] = zMiddle;
		        newPtVOI.importCurve(x, y, z);
		        
		      //J
		        standardColumnImage.set(373, 373, zMiddle, 100);
		        x[0] = 373;
		        y[0] = 373;
		        z[0] = zMiddle;
		        newPtVOI.importCurve(x, y, z);

		        //I
		        standardColumnImage.set(334, 411, zMiddle, 100);
		        x[0] = 334;
		        y[0] = 411;
		        z[0] = zMiddle;
		        newPtVOI.importCurve(x, y, z);
		        
		      //H
		        standardColumnImage.set(295, 450, zMiddle, 100);
		        x[0] = 295;
		        y[0] = 450;
		        z[0] = zMiddle;
		        newPtVOI.importCurve(x, y, z);
		        
		      //G
		        standardColumnImage.set(256, 489, zMiddle, 100);
		        x[0] = 256;
		        y[0] = 489;
		        z[0] = zMiddle;
		        newPtVOI.importCurve(x, y, z);
		        
		      
		        
		        
		        
		        //F
		        standardColumnImage.set(217, 450, zMiddle, 100);
		        x[0] = 217;
		        y[0] = 450;
		        z[0] = zMiddle;
		        newPtVOI.importCurve(x, y, z);
		        
		      //E
		        standardColumnImage.set(178, 411, zMiddle, 100);
		        x[0] = 178;
		        y[0] = 411;
		        z[0] = zMiddle;
		        newPtVOI.importCurve(x, y, z);
		        
		      //D
		        standardColumnImage.set(139, 373, zMiddle, 100);
		        x[0] = 139;
		        y[0] = 373;
		        z[0] = zMiddle;
		        newPtVOI.importCurve(x, y, z);
		        
		      //C
		        standardColumnImage.set(101, 334, zMiddle, 100);
		        x[0] = 101;
		        y[0] = 334;
		        z[0] = zMiddle;
		        newPtVOI.importCurve(x, y, z);
		        
		        
		        //B
		        standardColumnImage.set(62, 295, zMiddle, 100);
		        x[0] = 62;
		        y[0] = 295;
		        z[0] = zMiddle;
		        newPtVOI.importCurve(x, y, z);
		        
		        
		        //A
    	        standardColumnImage.set(101, 256, zMiddle, 100);
    	        x[0] = 101;
    	        y[0] = 256;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //P
    	        standardColumnImage.set(139, 217, zMiddle, 100);
    	        x[0] = 139;
    	        y[0] = 217;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //O
    	        standardColumnImage.set(178, 178, zMiddle, 100);
    	        x[0] = 178;
    	        y[0] = 178;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);

    	        //N
    	        standardColumnImage.set(217, 139, zMiddle, 100);
    	        x[0] = 217;
    	        y[0] = 139;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //M
    	        standardColumnImage.set(256, 101, zMiddle, 100);
    	        x[0] = 256;
    	        y[0] = 101;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //L
    	        standardColumnImage.set(295, 139, zMiddle, 100);
    	        x[0] = 295;
    	        y[0] = 139;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //K
    	        standardColumnImage.set(334, 178, zMiddle, 100);
    	        x[0] = 334;
    	        y[0] = 178;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //J
    	        standardColumnImage.set(373, 217, zMiddle, 100);
    	        x[0] = 373;
    	        y[0] = 217;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //I
    	        standardColumnImage.set(411, 256, zMiddle, 100);
    	        x[0] = 411;
    	        y[0] = 256;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //H
    	        standardColumnImage.set(373, 295, zMiddle, 100);
    	        x[0] = 373;
    	        y[0] = 295;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //G
    	        standardColumnImage.set(334, 334, zMiddle, 100);
    	        x[0] = 334;
    	        y[0] = 334;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //F
    	        standardColumnImage.set(295, 373, zMiddle, 100);
    	        x[0] = 295;
    	        y[0] = 373;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //E
    	        standardColumnImage.set(256, 411, zMiddle, 100);
    	        x[0] = 256;
    	        y[0] = 411;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //D
    	        standardColumnImage.set(217, 373, zMiddle, 100);
    	        x[0] = 217;
    	        y[0] = 373;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //C
    	        standardColumnImage.set(178, 334, zMiddle, 100);
    	        x[0] = 178;
    	        y[0] = 334;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);

    	        //B
    	        standardColumnImage.set(139, 295, zMiddle, 100);
    	        x[0] = 139;
    	        y[0] = 295;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);

    	        //Q
    	        standardColumnImage.set(178, 256, zMiddle, 100);
    	        x[0] = 178;
    	        y[0] = 256;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //X
    	        standardColumnImage.set(217, 217, zMiddle, 100);
    	        x[0] = 217;
    	        y[0] = 217;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //W
    	        standardColumnImage.set(256, 178, zMiddle, 100);
    	        x[0] = 256;
    	        y[0] = 178;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);

    	        //V
    	        standardColumnImage.set(295, 217, zMiddle, 100);
    	        x[0] = 295;
    	        y[0] = 217;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);

    	        //U
    	        standardColumnImage.set(334, 256, zMiddle, 100);
    	        x[0] = 334;
    	        y[0] = 256;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //T
    	        standardColumnImage.set(295, 295, zMiddle, 100);
    	        x[0] = 295;
    	        y[0] = 295;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //S
    	        standardColumnImage.set(256, 334, zMiddle, 100);
    	        x[0] = 256;
    	        y[0] = 334;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);

    	        //R
    	        standardColumnImage.set(217, 295, zMiddle, 100);
    	        x[0] = 217;
    	        y[0] = 295;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);

    	        //Y
    	        standardColumnImage.set(256, 256, zMiddle, 100);
    	        x[0] = 256;
    	        y[0] = 256;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        
    	        
    	        
    	        
    	        
    	        //--
    	     // r7
		        //A
		        standardColumnImage.set(23, 256, zEnd, 100);
		        x[0] = 23;
		        y[0] = 256;
		        z[0] = zEnd;
		        newPtVOI.importCurve(x, y, z);
		        
		      //X
		        standardColumnImage.set(62, 217, zEnd, 100);
		        x[0] = 62;
		        y[0] = 217;
		        z[0] = zEnd;
		        newPtVOI.importCurve(x, y, z);
		        

		        
		        //W
		        standardColumnImage.set(101, 178, zEnd, 100);
		        x[0] = 101;
		        y[0] = 178;
		        z[0] = zEnd;
		        newPtVOI.importCurve(x, y, z);
		        
		      //V
		        standardColumnImage.set(139, 139, zEnd, 100);
		        x[0] = 139;
		        y[0] = 139;
		        z[0] = zEnd;
		        newPtVOI.importCurve(x, y, z);
		        
		      //U
		        standardColumnImage.set(178, 101, zEnd, 100);
		        x[0] = 178;
		        y[0] = 101;
		        z[0] = zEnd;
		        newPtVOI.importCurve(x, y, z);
		        
		        //T
		        standardColumnImage.set(217, 62, zEnd, 100);
		        x[0] = 217;
		        y[0] = 62;
		        z[0] = zEnd;
		        newPtVOI.importCurve(x, y, z);

		        //S
		        standardColumnImage.set(256, 23, zEnd, 100);
		        x[0] = 256;
		        y[0] = 23;
		        z[0] = zEnd;
		        newPtVOI.importCurve(x, y, z);
		        
		        //R
		        standardColumnImage.set(295, 62, zEnd, 100);
		        x[0] = 295;
		        y[0] = 62;
		        z[0] = zEnd;
		        newPtVOI.importCurve(x, y, z);
		        
		        //Q
		        standardColumnImage.set(334, 101, zEnd, 100);
		        x[0] = 334;
		        y[0] = 101;
		        z[0] = zEnd;
		        newPtVOI.importCurve(x, y, z);
		        
		      //P
		        standardColumnImage.set(373, 139, zEnd, 100);
		        x[0] = 373;
		        y[0] = 139;
		        z[0] = zEnd;
		        newPtVOI.importCurve(x, y, z);

		        //O
		        standardColumnImage.set(411, 178, zEnd, 100);
		        x[0] = 411;
		        y[0] = 178;
		        z[0] = zEnd;
		        newPtVOI.importCurve(x, y, z);
		        
		        //N
		        standardColumnImage.set(450, 217, zEnd, 100);
		        x[0] = 450;
		        y[0] = 217;
		        z[0] = zEnd;
		        newPtVOI.importCurve(x, y, z);
		        
		        //M
		        standardColumnImage.set(489, 256, zEnd, 100);
		        x[0] = 489;
		        y[0] = 256;
		        z[0] = zEnd;
		        newPtVOI.importCurve(x, y, z);

		        //L
		        standardColumnImage.set(450, 295, zEnd, 100);
		        x[0] = 450;
		        y[0] = 295;
		        z[0] = zEnd;
		        newPtVOI.importCurve(x, y, z);
		        
		        //K
		        standardColumnImage.set(411, 334, zEnd, 100);
		        x[0] = 411;
		        y[0] = 334;
		        z[0] = zEnd;
		        newPtVOI.importCurve(x, y, z);
		        
		      //J
		        standardColumnImage.set(373, 373, zEnd, 100);
		        x[0] = 373;
		        y[0] = 373;
		        z[0] = zEnd;
		        newPtVOI.importCurve(x, y, z);

		        //I
		        standardColumnImage.set(334, 411, zEnd, 100);
		        x[0] = 334;
		        y[0] = 411;
		        z[0] = zEnd;
		        newPtVOI.importCurve(x, y, z);
		        
		      //H
		        standardColumnImage.set(295, 450, zEnd, 100);
		        x[0] = 295;
		        y[0] = 450;
		        z[0] = zEnd;
		        newPtVOI.importCurve(x, y, z);
		        
		      //G
		        standardColumnImage.set(256, 489, zEnd, 100);
		        x[0] = 256;
		        y[0] = 489;
		        z[0] = zEnd;
		        newPtVOI.importCurve(x, y, z);
		        
		      
		        
		        
		        
		        //F
		        standardColumnImage.set(217, 450, zEnd, 100);
		        x[0] = 217;
		        y[0] = 450;
		        z[0] = zEnd;
		        newPtVOI.importCurve(x, y, z);
		        
		      //E
		        standardColumnImage.set(178, 411, zEnd, 100);
		        x[0] = 178;
		        y[0] = 411;
		        z[0] = zEnd;
		        newPtVOI.importCurve(x, y, z);
		        
		      //D
		        standardColumnImage.set(139, 373, zEnd, 100);
		        x[0] = 139;
		        y[0] = 373;
		        z[0] = zEnd;
		        newPtVOI.importCurve(x, y, z);
		        
		      //C
		        standardColumnImage.set(101, 334, zEnd, 100);
		        x[0] = 101;
		        y[0] = 334;
		        z[0] = zEnd;
		        newPtVOI.importCurve(x, y, z);
		        
		        
		        //B
		        standardColumnImage.set(62, 295, zEnd, 100);
		        x[0] = 62;
		        y[0] = 295;
		        z[0] = zEnd;
		        newPtVOI.importCurve(x, y, z);
		        
		        
		        //A
    	        standardColumnImage.set(101, 256, zEnd, 100);
    	        x[0] = 101;
    	        y[0] = 256;
    	        z[0] = zEnd;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //P
    	        standardColumnImage.set(139, 217, zEnd, 100);
    	        x[0] = 139;
    	        y[0] = 217;
    	        z[0] = zEnd;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //O
    	        standardColumnImage.set(178, 178, zEnd, 100);
    	        x[0] = 178;
    	        y[0] = 178;
    	        z[0] = zEnd;
    	        newPtVOI.importCurve(x, y, z);

    	        //N
    	        standardColumnImage.set(217, 139, zEnd, 100);
    	        x[0] = 217;
    	        y[0] = 139;
    	        z[0] = zEnd;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //M
    	        standardColumnImage.set(256, 101, zEnd, 100);
    	        x[0] = 256;
    	        y[0] = 101;
    	        z[0] = zEnd;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //L
    	        standardColumnImage.set(295, 139, zEnd, 100);
    	        x[0] = 295;
    	        y[0] = 139;
    	        z[0] = zEnd;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //K
    	        standardColumnImage.set(334, 178, zEnd, 100);
    	        x[0] = 334;
    	        y[0] = 178;
    	        z[0] = zEnd;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //J
    	        standardColumnImage.set(373, 217, zEnd, 100);
    	        x[0] = 373;
    	        y[0] = 217;
    	        z[0] = zEnd;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //I
    	        standardColumnImage.set(411, 256, zEnd, 100);
    	        x[0] = 411;
    	        y[0] = 256;
    	        z[0] = zEnd;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //H
    	        standardColumnImage.set(373, 295, zEnd, 100);
    	        x[0] = 373;
    	        y[0] = 295;
    	        z[0] = zEnd;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //G
    	        standardColumnImage.set(334, 334, zEnd, 100);
    	        x[0] = 334;
    	        y[0] = 334;
    	        z[0] = zEnd;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //F
    	        standardColumnImage.set(295, 373, zEnd, 100);
    	        x[0] = 295;
    	        y[0] = 373;
    	        z[0] = zEnd;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //E
    	        standardColumnImage.set(256, 411, zEnd, 100);
    	        x[0] = 256;
    	        y[0] = 411;
    	        z[0] = zEnd;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //D
    	        standardColumnImage.set(217, 373, zEnd, 100);
    	        x[0] = 217;
    	        y[0] = 373;
    	        z[0] = zEnd;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //C
    	        standardColumnImage.set(178, 334, zEnd, 100);
    	        x[0] = 178;
    	        y[0] = 334;
    	        z[0] = zEnd;
    	        newPtVOI.importCurve(x, y, z);

    	        //B
    	        standardColumnImage.set(139, 295, zEnd, 100);
    	        x[0] = 139;
    	        y[0] = 295;
    	        z[0] = zEnd;
    	        newPtVOI.importCurve(x, y, z);

    	        //Q
    	        standardColumnImage.set(178, 256, zEnd, 100);
    	        x[0] = 178;
    	        y[0] = 256;
    	        z[0] = zEnd;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //X
    	        standardColumnImage.set(217, 217, zEnd, 100);
    	        x[0] = 217;
    	        y[0] = 217;
    	        z[0] = zEnd;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //W
    	        standardColumnImage.set(256, 178, zEnd, 100);
    	        x[0] = 256;
    	        y[0] = 178;
    	        z[0] = zEnd;
    	        newPtVOI.importCurve(x, y, z);

    	        //V
    	        standardColumnImage.set(295, 217, zEnd, 100);
    	        x[0] = 295;
    	        y[0] = 217;
    	        z[0] = zEnd;
    	        newPtVOI.importCurve(x, y, z);

    	        //U
    	        standardColumnImage.set(334, 256, zEnd, 100);
    	        x[0] = 334;
    	        y[0] = 256;
    	        z[0] = zEnd;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //T
    	        standardColumnImage.set(295, 295, zEnd, 100);
    	        x[0] = 295;
    	        y[0] = 295;
    	        z[0] = zEnd;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //S
    	        standardColumnImage.set(256, 334, zEnd, 100);
    	        x[0] = 256;
    	        y[0] = 334;
    	        z[0] = zEnd;
    	        newPtVOI.importCurve(x, y, z);

    	        //R
    	        standardColumnImage.set(217, 295, zEnd, 100);
    	        x[0] = 217;
    	        y[0] = 295;
    	        z[0] = zEnd;
    	        newPtVOI.importCurve(x, y, z);

    	        //Y
    	        standardColumnImage.set(256, 256, zEnd, 100);
    	        x[0] = 256;
    	        y[0] = 256;
    	        z[0] = zEnd;
    	        newPtVOI.importCurve(x, y, z);

        	}
	
        }else if(numPointsString.equals(PlugInDialogDrosophilaStandardColumnRegistration._75POINTS)) {
        	System.out.println("75 points");
        	
        	if(rvld) {
            	// top
    	        //A
    	        standardColumnImage.set(101, 256, zStart, 100);
    	        x[0] = 101;
    	        y[0] = 256;
    	        z[0] = zStart;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //B
    	        standardColumnImage.set(139, 295, zStart, 100);
    	        x[0] = 139;
    	        y[0] = 295;
    	        z[0] = zStart;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //C
    	        standardColumnImage.set(178, 334, zStart, 100);
    	        x[0] = 178;
    	        y[0] = 334;
    	        z[0] = zStart;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //D
    	        standardColumnImage.set(217, 373, zStart, 100);
    	        x[0] = 217;
    	        y[0] = 373;
    	        z[0] = zStart;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //E
    	        standardColumnImage.set(256, 411, zStart, 100);
    	        x[0] = 256;
    	        y[0] = 411;
    	        z[0] = zStart;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //F
    	        standardColumnImage.set(295, 373, zStart, 100);
    	        x[0] = 295;
    	        y[0] = 373;
    	        z[0] = zStart;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //G
    	        standardColumnImage.set(334, 334, zStart, 100);
    	        x[0] = 334;
    	        y[0] = 334;
    	        z[0] = zStart;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //H
    	        standardColumnImage.set(373, 295, zStart, 100);
    	        x[0] = 373;
    	        y[0] = 295;
    	        z[0] = zStart;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //I
    	        standardColumnImage.set(411, 256, zStart, 100);
    	        x[0] = 411;
    	        y[0] = 256;
    	        z[0] = zStart;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //J
    	        standardColumnImage.set(373, 217, zStart, 100);
    	        x[0] = 373;
    	        y[0] = 217;
    	        z[0] = zStart;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //K
    	        standardColumnImage.set(334, 178, zStart, 100);
    	        x[0] = 334;
    	        y[0] = 178;
    	        z[0] = zStart;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //L
    	        standardColumnImage.set(295, 139, zStart, 100);
    	        x[0] = 295;
    	        y[0] = 139;
    	        z[0] = zStart;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //M
    	        standardColumnImage.set(256, 101, zStart, 100);
    	        x[0] = 256;
    	        y[0] = 101;
    	        z[0] = zStart;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //N
    	        standardColumnImage.set(217, 139, zStart, 100);
    	        x[0] = 217;
    	        y[0] = 139;
    	        z[0] = zStart;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //O
    	        standardColumnImage.set(178, 178, zStart, 100);
    	        x[0] = 178;
    	        y[0] = 178;
    	        z[0] = zStart;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //P
    	        standardColumnImage.set(139, 217, zStart, 100);
    	        x[0] = 139;
    	        y[0] = 217;
    	        z[0] = zStart;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //Q
    	        standardColumnImage.set(178, 256, zStart, 100);
    	        x[0] = 178;
    	        y[0] = 256;
    	        z[0] = zStart;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //R
    	        standardColumnImage.set(217, 295, zStart, 100);
    	        x[0] = 217;
    	        y[0] = 295;
    	        z[0] = zStart;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //S
    	        standardColumnImage.set(256, 334, zStart, 100);
    	        x[0] = 256;
    	        y[0] = 334;
    	        z[0] = zStart;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //T
    	        standardColumnImage.set(295, 295, zStart, 100);
    	        x[0] = 295;
    	        y[0] = 295;
    	        z[0] = zStart;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //U
    	        standardColumnImage.set(334, 256, zStart, 100);
    	        x[0] = 334;
    	        y[0] = 256;
    	        z[0] = zStart;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //V
    	        standardColumnImage.set(295, 217, zStart, 100);
    	        x[0] = 295;
    	        y[0] = 217;
    	        z[0] = zStart;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //W
    	        standardColumnImage.set(256, 178, zStart, 100);
    	        x[0] = 256;
    	        y[0] = 178;
    	        z[0] = zStart;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //X
    	        standardColumnImage.set(217, 217, zStart, 100);
    	        x[0] = 217;
    	        y[0] = 217;
    	        z[0] = zStart;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //Y
    	        standardColumnImage.set(256, 256, zStart, 100);
    	        x[0] = 256;
    	        y[0] = 256;
    	        z[0] = zStart;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        

    	        
    	        // r8
    	        //A
    	        standardColumnImage.set(101, 256, zMiddle, 100);
    	        x[0] = 101;
    	        y[0] = 256;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //B
    	        standardColumnImage.set(139, 295, zMiddle, 100);
    	        x[0] = 139;
    	        y[0] = 295;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //C
    	        standardColumnImage.set(178, 334, zMiddle, 100);
    	        x[0] = 178;
    	        y[0] = 334;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //D
    	        standardColumnImage.set(217, 373, zMiddle, 100);
    	        x[0] = 217;
    	        y[0] = 373;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //E
    	        standardColumnImage.set(256, 411, zMiddle, 100);
    	        x[0] = 256;
    	        y[0] = 411;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //F
    	        standardColumnImage.set(295, 373, zMiddle, 100);
    	        x[0] = 295;
    	        y[0] = 373;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //G
    	        standardColumnImage.set(334, 334, zMiddle, 100);
    	        x[0] = 334;
    	        y[0] = 334;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //H
    	        standardColumnImage.set(373, 295, zMiddle, 100);
    	        x[0] = 373;
    	        y[0] = 295;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //I
    	        standardColumnImage.set(411, 256, zMiddle, 100);
    	        x[0] = 411;
    	        y[0] = 256;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //J
    	        standardColumnImage.set(373, 217, zMiddle, 100);
    	        x[0] = 373;
    	        y[0] = 217;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //K
    	        standardColumnImage.set(334, 178, zMiddle, 100);
    	        x[0] = 334;
    	        y[0] = 178;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //L
    	        standardColumnImage.set(295, 139, zMiddle, 100);
    	        x[0] = 295;
    	        y[0] = 139;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //M
    	        standardColumnImage.set(256, 101, zMiddle, 100);
    	        x[0] = 256;
    	        y[0] = 101;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //N
    	        standardColumnImage.set(217, 139, zMiddle, 100);
    	        x[0] = 217;
    	        y[0] = 139;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //O
    	        standardColumnImage.set(178, 178, zMiddle, 100);
    	        x[0] = 178;
    	        y[0] = 178;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //P
    	        standardColumnImage.set(139, 217, zMiddle, 100);
    	        x[0] = 139;
    	        y[0] = 217;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //Q
    	        standardColumnImage.set(178, 256, zMiddle, 100);
    	        x[0] = 178;
    	        y[0] = 256;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //R
    	        standardColumnImage.set(217, 295, zMiddle, 100);
    	        x[0] = 217;
    	        y[0] = 295;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //S
    	        standardColumnImage.set(256, 334, zMiddle, 100);
    	        x[0] = 256;
    	        y[0] = 334;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //T
    	        standardColumnImage.set(295, 295, zMiddle, 100);
    	        x[0] = 295;
    	        y[0] = 295;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //U
    	        standardColumnImage.set(334, 256, zMiddle, 100);
    	        x[0] = 334;
    	        y[0] = 256;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //V
    	        standardColumnImage.set(295, 217, zMiddle, 100);
    	        x[0] = 295;
    	        y[0] = 217;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //W
    	        standardColumnImage.set(256, 178, zMiddle, 100);
    	        x[0] = 256;
    	        y[0] = 178;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //X
    	        standardColumnImage.set(217, 217, zMiddle, 100);
    	        x[0] = 217;
    	        y[0] = 217;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //Y
    	        standardColumnImage.set(256, 256, zMiddle, 100);
    	        x[0] = 256;
    	        y[0] = 256;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        

    	        // r7
    	        //A
    	        standardColumnImage.set(101, 256, zEnd, 100);
    	        x[0] = 101;
    	        y[0] = 256;
    	        z[0] = zEnd;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //B
    	        standardColumnImage.set(139, 295, zEnd, 100);
    	        x[0] = 139;
    	        y[0] = 295;
    	        z[0] = zEnd;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //C
    	        standardColumnImage.set(178, 334, zEnd, 100);
    	        x[0] = 178;
    	        y[0] = 334;
    	        z[0] = zEnd;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //D
    	        standardColumnImage.set(217, 373, zEnd, 100);
    	        x[0] = 217;
    	        y[0] = 373;
    	        z[0] = zEnd;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //E
    	        standardColumnImage.set(256, 411, zEnd, 100);
    	        x[0] = 256;
    	        y[0] = 411;
    	        z[0] = zEnd;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //F
    	        standardColumnImage.set(295, 373, zEnd, 100);
    	        x[0] = 295;
    	        y[0] = 373;
    	        z[0] = zEnd;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //G
    	        standardColumnImage.set(334, 334, zEnd, 100);
    	        x[0] = 334;
    	        y[0] = 334;
    	        z[0] = zEnd;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //H
    	        standardColumnImage.set(373, 295, zEnd, 100);
    	        x[0] = 373;
    	        y[0] = 295;
    	        z[0] = zEnd;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //I
    	        standardColumnImage.set(411, 256, zEnd, 100);
    	        x[0] = 411;
    	        y[0] = 256;
    	        z[0] = zEnd;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //J
    	        standardColumnImage.set(373, 217, zEnd, 100);
    	        x[0] = 373;
    	        y[0] = 217;
    	        z[0] = zEnd;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //K
    	        standardColumnImage.set(334, 178, zEnd, 100);
    	        x[0] = 334;
    	        y[0] = 178;
    	        z[0] = zEnd;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //L
    	        standardColumnImage.set(295, 139, zEnd, 100);
    	        x[0] = 295;
    	        y[0] = 139;
    	        z[0] = zEnd;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //M
    	        standardColumnImage.set(256, 101, zEnd, 100);
    	        x[0] = 256;
    	        y[0] = 101;
    	        z[0] = zEnd;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //N
    	        standardColumnImage.set(217, 139, zEnd, 100);
    	        x[0] = 217;
    	        y[0] = 139;
    	        z[0] = zEnd;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //O
    	        standardColumnImage.set(178, 178, zEnd, 100);
    	        x[0] = 178;
    	        y[0] = 178;
    	        z[0] = zEnd;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //P
    	        standardColumnImage.set(139, 217, zEnd, 100);
    	        x[0] = 139;
    	        y[0] = 217;
    	        z[0] = zEnd;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //Q
    	        standardColumnImage.set(178, 256, zEnd, 100);
    	        x[0] = 178;
    	        y[0] = 256;
    	        z[0] = zEnd;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //R
    	        standardColumnImage.set(217, 295, zEnd, 100);
    	        x[0] = 217;
    	        y[0] = 295;
    	        z[0] = zEnd;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //S
    	        standardColumnImage.set(256, 334, zEnd, 100);
    	        x[0] = 256;
    	        y[0] = 334;
    	        z[0] = zEnd;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //T
    	        standardColumnImage.set(295, 295, zEnd, 100);
    	        x[0] = 295;
    	        y[0] = 295;
    	        z[0] = zEnd;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //U
    	        standardColumnImage.set(334, 256, zEnd, 100);
    	        x[0] = 334;
    	        y[0] = 256;
    	        z[0] = zEnd;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //V
    	        standardColumnImage.set(295, 217, zEnd, 100);
    	        x[0] = 295;
    	        y[0] = 217;
    	        z[0] = zEnd;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //W
    	        standardColumnImage.set(256, 178, zEnd, 100);
    	        x[0] = 256;
    	        y[0] = 178;
    	        z[0] = zEnd;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //X
    	        standardColumnImage.set(217, 217, zEnd, 100);
    	        x[0] = 217;
    	        y[0] = 217;
    	        z[0] = zEnd;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //Y
    	        standardColumnImage.set(256, 256, zEnd, 100);
    	        x[0] = 256;
    	        y[0] = 256;
    	        z[0] = zEnd;
    	        newPtVOI.importCurve(x, y, z);
    	        
            	
            }else {
            	
            	
            	// top
    	        //A
    	        standardColumnImage.set(101, 256, zStart, 100);
    	        x[0] = 101;
    	        y[0] = 256;
    	        z[0] = zStart;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //P
    	        standardColumnImage.set(139, 217, zStart, 100);
    	        x[0] = 139;
    	        y[0] = 217;
    	        z[0] = zStart;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //O
    	        standardColumnImage.set(178, 178, zStart, 100);
    	        x[0] = 178;
    	        y[0] = 178;
    	        z[0] = zStart;
    	        newPtVOI.importCurve(x, y, z);

    	        //N
    	        standardColumnImage.set(217, 139, zStart, 100);
    	        x[0] = 217;
    	        y[0] = 139;
    	        z[0] = zStart;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //M
    	        standardColumnImage.set(256, 101, zStart, 100);
    	        x[0] = 256;
    	        y[0] = 101;
    	        z[0] = zStart;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //L
    	        standardColumnImage.set(295, 139, zStart, 100);
    	        x[0] = 295;
    	        y[0] = 139;
    	        z[0] = zStart;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //K
    	        standardColumnImage.set(334, 178, zStart, 100);
    	        x[0] = 334;
    	        y[0] = 178;
    	        z[0] = zStart;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //J
    	        standardColumnImage.set(373, 217, zStart, 100);
    	        x[0] = 373;
    	        y[0] = 217;
    	        z[0] = zStart;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //I
    	        standardColumnImage.set(411, 256, zStart, 100);
    	        x[0] = 411;
    	        y[0] = 256;
    	        z[0] = zStart;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //H
    	        standardColumnImage.set(373, 295, zStart, 100);
    	        x[0] = 373;
    	        y[0] = 295;
    	        z[0] = zStart;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //G
    	        standardColumnImage.set(334, 334, zStart, 100);
    	        x[0] = 334;
    	        y[0] = 334;
    	        z[0] = zStart;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //F
    	        standardColumnImage.set(295, 373, zStart, 100);
    	        x[0] = 295;
    	        y[0] = 373;
    	        z[0] = zStart;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //E
    	        standardColumnImage.set(256, 411, zStart, 100);
    	        x[0] = 256;
    	        y[0] = 411;
    	        z[0] = zStart;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //D
    	        standardColumnImage.set(217, 373, zStart, 100);
    	        x[0] = 217;
    	        y[0] = 373;
    	        z[0] = zStart;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //C
    	        standardColumnImage.set(178, 334, zStart, 100);
    	        x[0] = 178;
    	        y[0] = 334;
    	        z[0] = zStart;
    	        newPtVOI.importCurve(x, y, z);

    	        //B
    	        standardColumnImage.set(139, 295, zStart, 100);
    	        x[0] = 139;
    	        y[0] = 295;
    	        z[0] = zStart;
    	        newPtVOI.importCurve(x, y, z);

    	        //Q
    	        standardColumnImage.set(178, 256, zStart, 100);
    	        x[0] = 178;
    	        y[0] = 256;
    	        z[0] = zStart;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //X
    	        standardColumnImage.set(217, 217, zStart, 100);
    	        x[0] = 217;
    	        y[0] = 217;
    	        z[0] = zStart;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //W
    	        standardColumnImage.set(256, 178, zStart, 100);
    	        x[0] = 256;
    	        y[0] = 178;
    	        z[0] = zStart;
    	        newPtVOI.importCurve(x, y, z);

    	        //V
    	        standardColumnImage.set(295, 217, zStart, 100);
    	        x[0] = 295;
    	        y[0] = 217;
    	        z[0] = zStart;
    	        newPtVOI.importCurve(x, y, z);

    	        //U
    	        standardColumnImage.set(334, 256, zStart, 100);
    	        x[0] = 334;
    	        y[0] = 256;
    	        z[0] = zStart;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //T
    	        standardColumnImage.set(295, 295, zStart, 100);
    	        x[0] = 295;
    	        y[0] = 295;
    	        z[0] = zStart;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //S
    	        standardColumnImage.set(256, 334, zStart, 100);
    	        x[0] = 256;
    	        y[0] = 334;
    	        z[0] = zStart;
    	        newPtVOI.importCurve(x, y, z);

    	        //R
    	        standardColumnImage.set(217, 295, zStart, 100);
    	        x[0] = 217;
    	        y[0] = 295;
    	        z[0] = zStart;
    	        newPtVOI.importCurve(x, y, z);

    	        //Y
    	        standardColumnImage.set(256, 256, zStart, 100);
    	        x[0] = 256;
    	        y[0] = 256;
    	        z[0] = zStart;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        
    	        
    	        
    	        
    	        
    	        // r8
    	        //A
    	        standardColumnImage.set(101, 256, zMiddle, 100);
    	        x[0] = 101;
    	        y[0] = 256;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //P
    	        standardColumnImage.set(139, 217, zMiddle, 100);
    	        x[0] = 139;
    	        y[0] = 217;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //O
    	        standardColumnImage.set(178, 178, zMiddle, 100);
    	        x[0] = 178;
    	        y[0] = 178;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);

    	        //N
    	        standardColumnImage.set(217, 139, zMiddle, 100);
    	        x[0] = 217;
    	        y[0] = 139;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //M
    	        standardColumnImage.set(256, 101, zMiddle, 100);
    	        x[0] = 256;
    	        y[0] = 101;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //L
    	        standardColumnImage.set(295, 139, zMiddle, 100);
    	        x[0] = 295;
    	        y[0] = 139;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //K
    	        standardColumnImage.set(334, 178, zMiddle, 100);
    	        x[0] = 334;
    	        y[0] = 178;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //J
    	        standardColumnImage.set(373, 217, zMiddle, 100);
    	        x[0] = 373;
    	        y[0] = 217;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //I
    	        standardColumnImage.set(411, 256, zMiddle, 100);
    	        x[0] = 411;
    	        y[0] = 256;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //H
    	        standardColumnImage.set(373, 295, zMiddle, 100);
    	        x[0] = 373;
    	        y[0] = 295;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //G
    	        standardColumnImage.set(334, 334, zMiddle, 100);
    	        x[0] = 334;
    	        y[0] = 334;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //F
    	        standardColumnImage.set(295, 373, zMiddle, 100);
    	        x[0] = 295;
    	        y[0] = 373;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //E
    	        standardColumnImage.set(256, 411, zMiddle, 100);
    	        x[0] = 256;
    	        y[0] = 411;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //D
    	        standardColumnImage.set(217, 373, zMiddle, 100);
    	        x[0] = 217;
    	        y[0] = 373;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //C
    	        standardColumnImage.set(178, 334, zMiddle, 100);
    	        x[0] = 178;
    	        y[0] = 334;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);

    	        //B
    	        standardColumnImage.set(139, 295, zMiddle, 100);
    	        x[0] = 139;
    	        y[0] = 295;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);

    	        //Q
    	        standardColumnImage.set(178, 256, zMiddle, 100);
    	        x[0] = 178;
    	        y[0] = 256;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //X
    	        standardColumnImage.set(217, 217, zMiddle, 100);
    	        x[0] = 217;
    	        y[0] = 217;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //W
    	        standardColumnImage.set(256, 178, zMiddle, 100);
    	        x[0] = 256;
    	        y[0] = 178;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);

    	        //V
    	        standardColumnImage.set(295, 217, zMiddle, 100);
    	        x[0] = 295;
    	        y[0] = 217;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);

    	        //U
    	        standardColumnImage.set(334, 256, zMiddle, 100);
    	        x[0] = 334;
    	        y[0] = 256;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //T
    	        standardColumnImage.set(295, 295, zMiddle, 100);
    	        x[0] = 295;
    	        y[0] = 295;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //S
    	        standardColumnImage.set(256, 334, zMiddle, 100);
    	        x[0] = 256;
    	        y[0] = 334;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);

    	        //R
    	        standardColumnImage.set(217, 295, zMiddle, 100);
    	        x[0] = 217;
    	        y[0] = 295;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);

    	        //Y
    	        standardColumnImage.set(256, 256, zMiddle, 100);
    	        x[0] = 256;
    	        y[0] = 256;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        
    	        
    	        
    	        
    	        
    	        
    	        // r7
    	        //A
    	        standardColumnImage.set(101, 256, zEnd, 100);
    	        x[0] = 101;
    	        y[0] = 256;
    	        z[0] = zEnd;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //P
    	        standardColumnImage.set(139, 217, zEnd, 100);
    	        x[0] = 139;
    	        y[0] = 217;
    	        z[0] = zEnd;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //O
    	        standardColumnImage.set(178, 178, zEnd, 100);
    	        x[0] = 178;
    	        y[0] = 178;
    	        z[0] = zEnd;
    	        newPtVOI.importCurve(x, y, z);

    	        //N
    	        standardColumnImage.set(217, 139, zEnd, 100);
    	        x[0] = 217;
    	        y[0] = 139;
    	        z[0] = zEnd;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //M
    	        standardColumnImage.set(256, 101, zEnd, 100);
    	        x[0] = 256;
    	        y[0] = 101;
    	        z[0] = zEnd;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //L
    	        standardColumnImage.set(295, 139, zEnd, 100);
    	        x[0] = 295;
    	        y[0] = 139;
    	        z[0] = zEnd;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //K
    	        standardColumnImage.set(334, 178, zEnd, 100);
    	        x[0] = 334;
    	        y[0] = 178;
    	        z[0] = zEnd;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //J
    	        standardColumnImage.set(373, 217, zEnd, 100);
    	        x[0] = 373;
    	        y[0] = 217;
    	        z[0] = zEnd;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //I
    	        standardColumnImage.set(411, 256, zEnd, 100);
    	        x[0] = 411;
    	        y[0] = 256;
    	        z[0] = zEnd;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //H
    	        standardColumnImage.set(373, 295, zEnd, 100);
    	        x[0] = 373;
    	        y[0] = 295;
    	        z[0] = zEnd;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //G
    	        standardColumnImage.set(334, 334, zEnd, 100);
    	        x[0] = 334;
    	        y[0] = 334;
    	        z[0] = zEnd;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //F
    	        standardColumnImage.set(295, 373, zEnd, 100);
    	        x[0] = 295;
    	        y[0] = 373;
    	        z[0] = zEnd;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //E
    	        standardColumnImage.set(256, 411, zEnd, 100);
    	        x[0] = 256;
    	        y[0] = 411;
    	        z[0] = zEnd;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //D
    	        standardColumnImage.set(217, 373, zEnd, 100);
    	        x[0] = 217;
    	        y[0] = 373;
    	        z[0] = zEnd;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //C
    	        standardColumnImage.set(178, 334, zEnd, 100);
    	        x[0] = 178;
    	        y[0] = 334;
    	        z[0] = zEnd;
    	        newPtVOI.importCurve(x, y, z);

    	        //B
    	        standardColumnImage.set(139, 295, zEnd, 100);
    	        x[0] = 139;
    	        y[0] = 295;
    	        z[0] = zEnd;
    	        newPtVOI.importCurve(x, y, z);

    	        //Q
    	        standardColumnImage.set(178, 256, zEnd, 100);
    	        x[0] = 178;
    	        y[0] = 256;
    	        z[0] = zEnd;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //X
    	        standardColumnImage.set(217, 217, zEnd, 100);
    	        x[0] = 217;
    	        y[0] = 217;
    	        z[0] = zEnd;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //W
    	        standardColumnImage.set(256, 178, zEnd, 100);
    	        x[0] = 256;
    	        y[0] = 178;
    	        z[0] = zEnd;
    	        newPtVOI.importCurve(x, y, z);

    	        //V
    	        standardColumnImage.set(295, 217, zEnd, 100);
    	        x[0] = 295;
    	        y[0] = 217;
    	        z[0] = zEnd;
    	        newPtVOI.importCurve(x, y, z);

    	        //U
    	        standardColumnImage.set(334, 256, zEnd, 100);
    	        x[0] = 334;
    	        y[0] = 256;
    	        z[0] = zEnd;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //T
    	        standardColumnImage.set(295, 295, zEnd, 100);
    	        x[0] = 295;
    	        y[0] = 295;
    	        z[0] = zEnd;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //S
    	        standardColumnImage.set(256, 334, zEnd, 100);
    	        x[0] = 256;
    	        y[0] = 334;
    	        z[0] = zEnd;
    	        newPtVOI.importCurve(x, y, z);

    	        //R
    	        standardColumnImage.set(217, 295, zEnd, 100);
    	        x[0] = 217;
    	        y[0] = 295;
    	        z[0] = zEnd;
    	        newPtVOI.importCurve(x, y, z);

    	        //Y
    	        standardColumnImage.set(256, 256, zEnd, 100);
    	        x[0] = 256;
    	        y[0] = 256;
    	        z[0] = zEnd;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        
            	
            	
            	
            }
        	
        }else if(numPointsString.equals(PlugInDialogDrosophilaStandardColumnRegistration._27POINTS)) {
        	if(rvld) {
    	        // top
    	        //A
    	        standardColumnImage.set(178, 256, zStart, 100);
    	        x[0] = 178;
    	        y[0] = 256;
    	        z[0] = zStart;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //B
    	    	standardColumnImage.set(217, 295, zStart, 100);
    	        x[0] = 217;
    	        y[0] = 295;
    	        z[0] = zStart;
    	        newPtVOI.importCurve(x, y, z);
    	
    	        //C
    	    	standardColumnImage.set(256, 334, zStart, 100);
    	        x[0] = 256;
    	        y[0] = 334;
    	        z[0] = zStart;
    	        newPtVOI.importCurve(x, y, z);
    	
    	        //D
    	    	standardColumnImage.set(295, 295, zStart, 100);
    	        x[0] = 295;
    	        y[0] = 295;
    	        z[0] = zStart;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //E
    	        standardColumnImage.set(334, 256, zStart, 100);
    	        x[0] = 334;
    	        y[0] = 256;
    	        z[0] = zStart;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //F
    	    	standardColumnImage.set(295, 217, zStart, 100);
    	        x[0] = 295;
    	        y[0] = 217;
    	        z[0] = zStart;
    	        newPtVOI.importCurve(x, y, z);
    	
    	        //G
    	    	standardColumnImage.set(256, 178, zStart, 100);
    	        x[0] = 256;
    	        y[0] = 178;
    	        z[0] = zStart;
    	        newPtVOI.importCurve(x, y, z);
    	
    	        //H
    			standardColumnImage.set(217, 217, zStart, 100);
    	        x[0] = 217;
    	        y[0] = 217;
    	        z[0] = zStart;
    	        newPtVOI.importCurve(x, y, z);
    	
    		    //I
    	        standardColumnImage.set(256, 256, zStart, 100);
    	        x[0] = 256;
    	        y[0] = 256;
    	        z[0] = zStart;
    	        newPtVOI.importCurve(x, y, z);
    	
    	        
    	        
    	        // r8
    	        //A
    	        standardColumnImage.set(178, 256, zMiddle, 100);
    	        x[0] = 178;
    	        y[0] = 256;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	    	//B
    	    	standardColumnImage.set(217, 295, zMiddle, 100);
    	        x[0] = 217;
    	        y[0] = 295;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);
    	
    	        //C
    	    	standardColumnImage.set(256, 334, zMiddle, 100);
    	        x[0] = 256;
    	        y[0] = 334;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);
    	
    	        //D
    			standardColumnImage.set(295, 295, zMiddle, 100);
    	        x[0] = 295;
    	        y[0] = 295;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //E
    	        standardColumnImage.set(334, 256, zMiddle, 100);
    	        x[0] = 334;
    	        y[0] = 256;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	    	//F
    	    	standardColumnImage.set(295, 217, zMiddle, 100);
    	        x[0] = 295;
    	        y[0] = 217;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);
    	
    	        //G
    	    	standardColumnImage.set(256, 178, zMiddle, 100);
    	        x[0] = 256;
    	        y[0] = 178;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);
    	
    	        //H
    	    	standardColumnImage.set(217, 217, zMiddle, 100);
    	        x[0] = 217;
    	        y[0] = 217;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);
    	
    	        //I
    	        standardColumnImage.set(256, 256, zMiddle, 100);
    	        x[0] = 256;
    	        y[0] = 256;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);
    	
    	        
    	        
    	        
    	        // r7
    	        //A
    	        standardColumnImage.set(178, 256, zEnd, 100);
    	        x[0] = 178;
    	        y[0] = 256;
    	        z[0] = zEnd;
    	        newPtVOI.importCurve(x, y, z);
    	
    	    	//B
    	    	standardColumnImage.set(217, 295, zEnd, 100);
    	        x[0] = 217;
    	        y[0] = 295;
    	        z[0] = zEnd;
    	        newPtVOI.importCurve(x, y, z);
    	
    	        //C
    	    	standardColumnImage.set(256, 334, zEnd, 100);
    	        x[0] = 256;
    	        y[0] = 334;
    	        z[0] = zEnd;
    	        newPtVOI.importCurve(x, y, z);
    	
    	        //D
    	    	standardColumnImage.set(295, 295, zEnd, 100);
    	        x[0] = 295;
    	        y[0] = 295;
    	        z[0] = zEnd;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //E
    	        standardColumnImage.set(334, 256, zEnd, 100);
    	        x[0] = 334;
    	        y[0] = 256;
    	        z[0] = zEnd;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	    	//F
    	    	standardColumnImage.set(295, 217, zEnd, 100);
    	        x[0] = 295;
    	        y[0] = 217;
    	        z[0] = zEnd;
    	        newPtVOI.importCurve(x, y, z);
    	
    	        //G
    	    	standardColumnImage.set(256, 178, zEnd, 100);
    	        x[0] = 256;
    	        y[0] = 178;
    	        z[0] = zEnd;
    	        newPtVOI.importCurve(x, y, z);
    	
    	        //H
    	    	standardColumnImage.set(217, 217, zEnd, 100);
    	        x[0] = 217;
    	        y[0] = 217;
    	        z[0] = zEnd;
    	        newPtVOI.importCurve(x, y, z);
    	
    	        //I
    	        standardColumnImage.set(256, 256, zEnd, 100);
    	        x[0] = 256;
    	        y[0] = 256;
    	        z[0] = zEnd;
    	        newPtVOI.importCurve(x, y, z);
            }else {
            	// top
    	        //A
    	        standardColumnImage.set(178, 256, zStart, 100);
    	        x[0] = 178;
    	        y[0] = 256;
    	        z[0] = zStart;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //H
    			standardColumnImage.set(217, 217, zStart, 100);
    	        x[0] = 217;
    	        y[0] = 217;
    	        z[0] = zStart;
    	        newPtVOI.importCurve(x, y, z);

    	        //G
    	    	standardColumnImage.set(256, 178, zStart, 100);
    	        x[0] = 256;
    	        y[0] = 178;
    	        z[0] = zStart;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //F
    	    	standardColumnImage.set(295, 217, zStart, 100);
    	        x[0] = 295;
    	        y[0] = 217;
    	        z[0] = zStart;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //E
    	        standardColumnImage.set(334, 256, zStart, 100);
    	        x[0] = 334;
    	        y[0] = 256;
    	        z[0] = zStart;
    	        newPtVOI.importCurve(x, y, z);

    	        //D
    	    	standardColumnImage.set(295, 295, zStart, 100);
    	        x[0] = 295;
    	        y[0] = 295;
    	        z[0] = zStart;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //C
    	    	standardColumnImage.set(256, 334, zStart, 100);
    	        x[0] = 256;
    	        y[0] = 334;
    	        z[0] = zStart;
    	        newPtVOI.importCurve(x, y, z);

    	        //B
    	    	standardColumnImage.set(217, 295, zStart, 100);
    	        x[0] = 217;
    	        y[0] = 295;
    	        z[0] = zStart;
    	        newPtVOI.importCurve(x, y, z);

    		    //I
    	        standardColumnImage.set(256, 256, zStart, 100);
    	        x[0] = 256;
    	        y[0] = 256;
    	        z[0] = zStart;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        
    	        // r8
    	        //A
    	        standardColumnImage.set(178, 256, zMiddle, 100);
    	        x[0] = 178;
    	        y[0] = 256;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //H
    			standardColumnImage.set(217, 217, zMiddle, 100);
    	        x[0] = 217;
    	        y[0] = 217;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);

    	        //G
    	    	standardColumnImage.set(256, 178, zMiddle, 100);
    	        x[0] = 256;
    	        y[0] = 178;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //F
    	    	standardColumnImage.set(295, 217, zMiddle, 100);
    	        x[0] = 295;
    	        y[0] = 217;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //E
    	        standardColumnImage.set(334, 256, zMiddle, 100);
    	        x[0] = 334;
    	        y[0] = 256;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);

    	        //D
    	    	standardColumnImage.set(295, 295, zMiddle, 100);
    	        x[0] = 295;
    	        y[0] = 295;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //C
    	    	standardColumnImage.set(256, 334, zMiddle, 100);
    	        x[0] = 256;
    	        y[0] = 334;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);

    	        //B
    	    	standardColumnImage.set(217, 295, zMiddle, 100);
    	        x[0] = 217;
    	        y[0] = 295;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);

    		    //I
    	        standardColumnImage.set(256, 256, zMiddle, 100);
    	        x[0] = 256;
    	        y[0] = 256;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        
    	        // r7
    	        //A
    	        standardColumnImage.set(178, 256, zEnd, 100);
    	        x[0] = 178;
    	        y[0] = 256;
    	        z[0] = zEnd;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //H
    			standardColumnImage.set(217, 217, zEnd, 100);
    	        x[0] = 217;
    	        y[0] = 217;
    	        z[0] = zEnd;
    	        newPtVOI.importCurve(x, y, z);

    	        //G
    	    	standardColumnImage.set(256, 178, zEnd, 100);
    	        x[0] = 256;
    	        y[0] = 178;
    	        z[0] = zEnd;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //F
    	    	standardColumnImage.set(295, 217, zEnd, 100);
    	        x[0] = 295;
    	        y[0] = 217;
    	        z[0] = zEnd;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //E
    	        standardColumnImage.set(334, 256, zEnd, 100);
    	        x[0] = 334;
    	        y[0] = 256;
    	        z[0] = zEnd;
    	        newPtVOI.importCurve(x, y, z);

    	        //D
    	    	standardColumnImage.set(295, 295, zEnd, 100);
    	        x[0] = 295;
    	        y[0] = 295;
    	        z[0] = zEnd;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //C
    	    	standardColumnImage.set(256, 334, zEnd, 100);
    	        x[0] = 256;
    	        y[0] = 334;
    	        z[0] = zEnd;
    	        newPtVOI.importCurve(x, y, z);

    	        //B
    	    	standardColumnImage.set(217, 295, zEnd, 100);
    	        x[0] = 217;
    	        y[0] = 295;
    	        z[0] = zEnd;
    	        newPtVOI.importCurve(x, y, z);

    		    //I
    	        standardColumnImage.set(256, 256, zEnd, 100);
    	        x[0] = 256;
    	        y[0] = 256;
    	        z[0] = zEnd;
    	        newPtVOI.importCurve(x, y, z);
    	
    	        
            }
        }else if(numPointsString.equals(PlugInDialogDrosophilaStandardColumnRegistration._27APOINTS)) {
        	if(rvld) {
    	        // top
        		
        		//A
    	    	standardColumnImage.set(256 + xDelta, 327 - yDelta, zStart2, 100);
    	        x[0] = 256 + xDelta;
    	        y[0] = 327 - yDelta;
    	        z[0] = zStart2;
    	        newPtVOI.importCurve(x, y, z);
    	
    	        //B
    	    	standardColumnImage.set(291 + xDelta, 291 - yDelta, zStart2, 100);
    	        x[0] = 291 + xDelta;
    	        y[0] = 291 - yDelta;
    	        z[0] = zStart2;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //C
    	        standardColumnImage.set(327 + xDelta, 256 - yDelta, zStart2, 100);
    	        x[0] = 327 + xDelta;
    	        y[0] = 256 - yDelta;
    	        z[0] = zStart2;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //D
    	    	standardColumnImage.set(291 + xDelta, 221 - yDelta, zStart2, 100);
    	        x[0] = 291 + xDelta;
    	        y[0] = 221 - yDelta;
    	        z[0] = zStart2;
    	        newPtVOI.importCurve(x, y, z);

        		 //E
    	    	standardColumnImage.set(256 + xDelta, 185 - yDelta, zStart2, 100);
    	        x[0] = 256 + xDelta;
    	        y[0] = 185 - yDelta;
    	        z[0] = zStart2;
    	        newPtVOI.importCurve(x, y, z);
    	
    	        //F
    			standardColumnImage.set(221 + xDelta, 221 - yDelta, zStart2, 100);
    	        x[0] = 221 + xDelta;
    	        y[0] = 221 - yDelta;
    	        z[0] = zStart2;
    	        newPtVOI.importCurve(x, y, z);

    	        //G
    	        standardColumnImage.set(185 + xDelta, 256 - yDelta, zStart2, 100);
    	        x[0] = 185 + xDelta;
    	        y[0] = 256 - yDelta;
    	        z[0] = zStart2;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //H
    	    	standardColumnImage.set(221 + xDelta, 291 - yDelta, zStart2, 100);
    	        x[0] = 221 + xDelta;
    	        y[0] = 291 - yDelta;
    	        z[0] = zStart2;
    	        newPtVOI.importCurve(x, y, z);

    		    //I
    	        standardColumnImage.set(256 + xDelta, 256 - yDelta, zStart2, 100);
    	        x[0] = 256 + xDelta;
    	        y[0] = 256 - yDelta;
    	        z[0] = zStart2;
    	        newPtVOI.importCurve(x, y, z);
    	
    	        
    	        
    	        // r8
    	        //A
    	    	standardColumnImage.set(256, 327, zMiddle, 100);
    	        x[0] = 256;
    	        y[0] = 327;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);
    	
    	        //B
    			standardColumnImage.set(291, 291, zMiddle, 100);
    	        x[0] = 291;
    	        y[0] = 291;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //C
    	        standardColumnImage.set(327, 256, zMiddle, 100);
    	        x[0] = 327;
    	        y[0] = 256;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	    	//D
    	    	standardColumnImage.set(291, 221, zMiddle, 100);
    	        x[0] = 291;
    	        y[0] = 221;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //E
    	    	standardColumnImage.set(256, 185, zMiddle, 100);
    	        x[0] = 256;
    	        y[0] = 185;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);
    	
    	        //F
    	    	standardColumnImage.set(221, 221, zMiddle, 100);
    	        x[0] = 221;
    	        y[0] = 221;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //G
    	        standardColumnImage.set(185, 256, zMiddle, 100);
    	        x[0] = 185;
    	        y[0] = 256;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	    	//H
    	    	standardColumnImage.set(221, 291, zMiddle, 100);
    	        x[0] = 221;
    	        y[0] = 291;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);

    	        //I
    	        standardColumnImage.set(256, 256, zMiddle, 100);
    	        x[0] = 256;
    	        y[0] = 256;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);
    	
    	        
    	        
    	        
    	        // r7
    	        //A
    	    	standardColumnImage.set(256, 327, zEnd2, 100);
    	        x[0] = 256;
    	        y[0] = 327;
    	        z[0] = zEnd2;
    	        newPtVOI.importCurve(x, y, z);
    	
    	        //B
    	    	standardColumnImage.set(291, 291, zEnd2, 100);
    	        x[0] = 291;
    	        y[0] = 291;
    	        z[0] = zEnd2;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //C
    	        standardColumnImage.set(327, 256, zEnd2, 100);
    	        x[0] = 327;
    	        y[0] = 256;
    	        z[0] = zEnd2;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	    	//D
    	    	standardColumnImage.set(291, 221, zEnd2, 100);
    	        x[0] = 291;
    	        y[0] = 221;
    	        z[0] = zEnd2;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //E
    	    	standardColumnImage.set(256, 185, zEnd2, 100);
    	        x[0] = 256;
    	        y[0] = 185;
    	        z[0] = zEnd2;
    	        newPtVOI.importCurve(x, y, z);
    	
    	        //F
    	    	standardColumnImage.set(221, 221, zEnd2, 100);
    	        x[0] = 221;
    	        y[0] = 221;
    	        z[0] = zEnd2;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //G
    	        standardColumnImage.set(185, 256, zEnd2, 100);
    	        x[0] = 185;
    	        y[0] = 256;
    	        z[0] = zEnd2;
    	        newPtVOI.importCurve(x, y, z);
    	
    	    	//H
    	    	standardColumnImage.set(221, 291, zEnd2, 100);
    	        x[0] = 221;
    	        y[0] = 291;
    	        z[0] = zEnd2;
    	        newPtVOI.importCurve(x, y, z);

    	
    	        //I
    	        standardColumnImage.set(256, 256, zEnd2, 100);
    	        x[0] = 256;
    	        y[0] = 256;
    	        z[0] = zEnd2;
    	        newPtVOI.importCurve(x, y, z);
            }else {
            	// top
            	//A
    	    	standardColumnImage.set(256 + xDelta, 327 - yDelta, zStart2, 100);
    	        x[0] = 256 + xDelta;
    	        y[0] = 327 - yDelta;
    	        z[0] = zStart2;
    	        newPtVOI.importCurve(x, y, z);

    	        //B
    	    	standardColumnImage.set(221 + xDelta, 291 - yDelta, zStart2, 100);
    	        x[0] = 221 + xDelta;
    	        y[0] = 291 - yDelta;
    	        z[0] = zStart2;
    	        newPtVOI.importCurve(x, y, z);
            	
    	        //C
    	        standardColumnImage.set(185 + xDelta, 256 - yDelta, zStart2, 100);
    	        x[0] = 185 + xDelta;
    	        y[0] = 256 - yDelta;
    	        z[0] = zStart2;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //D
    			standardColumnImage.set(221 + xDelta, 221 - yDelta, zStart2, 100);
    	        x[0] = 221 + xDelta;
    	        y[0] = 221 - yDelta;
    	        z[0] = zStart2;
    	        newPtVOI.importCurve(x, y, z);

    	        //E
    	    	standardColumnImage.set(256 + xDelta, 185 - yDelta, zStart2, 100);
    	        x[0] = 256 + xDelta;
    	        y[0] = 185 - yDelta;
    	        z[0] = zStart2;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //F
    	    	standardColumnImage.set(291 + xDelta, 221 - yDelta, zStart2, 100);
    	        x[0] = 291 + xDelta;
    	        y[0] = 221 - yDelta;
    	        z[0] = zStart2;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //G
    	        standardColumnImage.set(327 + xDelta, 256 - yDelta, zStart2, 100);
    	        x[0] = 327 + xDelta;
    	        y[0] = 256 - yDelta;
    	        z[0] = zStart2;
    	        newPtVOI.importCurve(x, y, z);

    	        //H
    	    	standardColumnImage.set(291 + xDelta, 291 - yDelta, zStart2, 100);
    	        x[0] = 291 + xDelta;
    	        y[0] = 291 - yDelta;
    	        z[0] = zStart2;
    	        newPtVOI.importCurve(x, y, z);

    		    //I
    	        standardColumnImage.set(256 + xDelta, 256 - yDelta, zStart2, 100);
    	        x[0] = 256 + xDelta;
    	        y[0] = 256 - yDelta;
    	        z[0] = zStart2;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        
    	        // r8
    	        //A
    	    	standardColumnImage.set(256, 327, zMiddle2, 100);
    	        x[0] = 256;
    	        y[0] = 327;
    	        z[0] = zMiddle2;
    	        newPtVOI.importCurve(x, y, z);

    	        //B
    	    	standardColumnImage.set(221, 291, zMiddle2, 100);
    	        x[0] = 221;
    	        y[0] = 291;
    	        z[0] = zMiddle2;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //C
    	        standardColumnImage.set(185, 256, zMiddle2, 100);
    	        x[0] = 185;
    	        y[0] = 256;
    	        z[0] = zMiddle2;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //D
    			standardColumnImage.set(221, 221, zMiddle2, 100);
    	        x[0] = 221;
    	        y[0] = 221;
    	        z[0] = zMiddle2;
    	        newPtVOI.importCurve(x, y, z);

    	        //E
    	    	standardColumnImage.set(256, 185, zMiddle2, 100);
    	        x[0] = 256;
    	        y[0] = 185;
    	        z[0] = zMiddle2;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //F
    	    	standardColumnImage.set(291, 221, zMiddle2, 100);
    	        x[0] = 291;
    	        y[0] = 221;
    	        z[0] = zMiddle2;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //G
    	        standardColumnImage.set(327, 256, zMiddle2, 100);
    	        x[0] = 327;
    	        y[0] = 256;
    	        z[0] = zMiddle2;
    	        newPtVOI.importCurve(x, y, z);

    	        //H
    	    	standardColumnImage.set(291, 291, zMiddle2, 100);
    	        x[0] = 291;
    	        y[0] = 291;
    	        z[0] = zMiddle2;
    	        newPtVOI.importCurve(x, y, z);

    		    //I
    	        standardColumnImage.set(256, 256, zMiddle2, 100);
    	        x[0] = 256;
    	        y[0] = 256;
    	        z[0] = zMiddle2;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        
    	        // r7
    	        //A
    	    	standardColumnImage.set(256, 327, zEnd2, 100);
    	        x[0] = 256;
    	        y[0] = 327;
    	        z[0] = zEnd2;
    	        newPtVOI.importCurve(x, y, z);

    	        //B
    	    	standardColumnImage.set(221, 291, zEnd2, 100);
    	        x[0] = 221;
    	        y[0] = 291;
    	        z[0] = zEnd2;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //C
    	        standardColumnImage.set(185, 256, zEnd2, 100);
    	        x[0] = 185;
    	        y[0] = 256;
    	        z[0] = zEnd2;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //D
    			standardColumnImage.set(221, 221, zEnd2, 100);
    	        x[0] = 221;
    	        y[0] = 221;
    	        z[0] = zEnd2;
    	        newPtVOI.importCurve(x, y, z);

    	        //E
    	    	standardColumnImage.set(256, 185, zEnd2, 100);
    	        x[0] = 256;
    	        y[0] = 185;
    	        z[0] = zEnd2;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //F
    	    	standardColumnImage.set(291, 221, zEnd2, 100);
    	        x[0] = 291;
    	        y[0] = 221;
    	        z[0] = zEnd2;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //G
    	        standardColumnImage.set(327, 256, zEnd2, 100);
    	        x[0] = 327;
    	        y[0] = 256;
    	        z[0] = zEnd2;
    	        newPtVOI.importCurve(x, y, z);

    	        //H
    	    	standardColumnImage.set(291, 291, zEnd2, 100);
    	        x[0] = 291;
    	        y[0] = 291;
    	        z[0] = zEnd2;
    	        newPtVOI.importCurve(x, y, z);

    		    //I
    	        standardColumnImage.set(256, 256, zEnd2, 100);
    	        x[0] = 256;
    	        y[0] = 256;
    	        z[0] = zEnd2;
    	        newPtVOI.importCurve(x, y, z);
    	
    	        
            }
        }else if (numPointsString.equals(PlugInDialogDrosophilaStandardColumnRegistration._75APOINTS)) {
        	if(rvld) {
        		// top
        		
        		//A
    	    	standardColumnImage.set(256 + xDelta, 398 - yDelta, zStart2, 100);
    	        x[0] = 256 + xDelta;
    	        y[0] = 398 - yDelta;
    	        z[0] = zStart2;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //B
    	    	standardColumnImage.set(291 + xDelta, 362 - yDelta, zStart2, 100);
    	        x[0] = 291 + xDelta;
    	        y[0] = 362 - yDelta;
    	        z[0] = zStart2;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //C
    	    	standardColumnImage.set(327 + xDelta, 327 - yDelta, zStart2, 100);
    	        x[0] = 327 + xDelta;
    	        y[0] = 327 - yDelta;
    	        z[0] = zStart2;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //D
    	    	standardColumnImage.set(362 + xDelta, 291 - yDelta, zStart2, 100);
    	        x[0] = 362 + xDelta;
    	        y[0] = 291 - yDelta;
    	        z[0] = zStart2;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //E
    	    	standardColumnImage.set(398 + xDelta, 256 - yDelta, zStart2, 100);
    	        x[0] = 398 + xDelta;
    	        y[0] = 256 - yDelta;
    	        z[0] = zStart2;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //F
    	    	standardColumnImage.set(362 + xDelta, 221 - yDelta, zStart2, 100);
    	        x[0] = 362 + xDelta;
    	        y[0] = 221 - yDelta;
    	        z[0] = zStart2;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //G
    	    	standardColumnImage.set(327 + xDelta, 185 - yDelta, zStart2, 100);
    	        x[0] = 327 + xDelta;
    	        y[0] = 185 - yDelta;
    	        z[0] = zStart2;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //H
    	    	standardColumnImage.set(291 + xDelta, 150 - yDelta, zStart2, 100);
    	        x[0] = 291 + xDelta;
    	        y[0] = 150 - yDelta;
    	        z[0] = zStart2;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //I
    	    	standardColumnImage.set(256 + xDelta, 114 - yDelta, zStart2, 100);
    	        x[0] = 256 + xDelta;
    	        y[0] = 114 - yDelta;
    	        z[0] = zStart2;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //J
    	    	standardColumnImage.set(221 + xDelta, 150 - yDelta, zStart2, 100);
    	        x[0] = 221 + xDelta;
    	        y[0] = 150 - yDelta;
    	        z[0] = zStart2;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //K
    	    	standardColumnImage.set(185 + xDelta, 185 - yDelta, zStart2, 100);
    	        x[0] = 185 + xDelta;
    	        y[0] = 185 - yDelta;
    	        z[0] = zStart2;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //L
    	    	standardColumnImage.set(150 + xDelta, 221 - yDelta, zStart2, 100);
    	        x[0] = 150 + xDelta;
    	        y[0] = 221 - yDelta;
    	        z[0] = zStart2;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //M
    	    	standardColumnImage.set(114 + xDelta, 256 - yDelta, zStart2, 100);
    	        x[0] = 114 + xDelta;
    	        y[0] = 256 - yDelta;
    	        z[0] = zStart2;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //N
    	    	standardColumnImage.set(150 + xDelta, 291 - yDelta, zStart2, 100);
    	        x[0] = 150 + xDelta;
    	        y[0] = 291 - yDelta;
    	        z[0] = zStart2;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //O
    	    	standardColumnImage.set(185 + xDelta, 327 - yDelta, zStart2, 100);
    	        x[0] = 185 + xDelta;
    	        y[0] = 327 - yDelta;
    	        z[0] = zStart2;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //P
    	    	standardColumnImage.set(221 + xDelta, 362 - yDelta, zStart2, 100);
    	        x[0] = 221 + xDelta;
    	        y[0] = 362 - yDelta;
    	        z[0] = zStart2;
    	        newPtVOI.importCurve(x, y, z);

        		//Q
    	    	standardColumnImage.set(256 + xDelta, 327 - yDelta, zStart2, 100);
    	        x[0] = 256 + xDelta;
    	        y[0] = 327 - yDelta;
    	        z[0] = zStart2;
    	        newPtVOI.importCurve(x, y, z);
    	
    	        //R
    	    	standardColumnImage.set(291 + xDelta, 291 - yDelta, zStart2, 100);
    	        x[0] = 291 + xDelta;
    	        y[0] = 291 - yDelta;
    	        z[0] = zStart2;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //S
    	        standardColumnImage.set(327 + xDelta, 256 - yDelta, zStart2, 100);
    	        x[0] = 327 + xDelta;
    	        y[0] = 256 - yDelta;
    	        z[0] = zStart2;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //T
    	    	standardColumnImage.set(291 + xDelta, 221 - yDelta, zStart2, 100);
    	        x[0] = 291 + xDelta;
    	        y[0] = 221 - yDelta;
    	        z[0] = zStart2;
    	        newPtVOI.importCurve(x, y, z);

        		 //U
    	    	standardColumnImage.set(256 + xDelta, 185 - yDelta, zStart2, 100);
    	        x[0] = 256 + xDelta;
    	        y[0] = 185 - yDelta;
    	        z[0] = zStart2;
    	        newPtVOI.importCurve(x, y, z);
    	
    	        //V
    			standardColumnImage.set(221 + xDelta, 221 - yDelta, zStart2, 100);
    	        x[0] = 221 + xDelta;
    	        y[0] = 221 - yDelta;
    	        z[0] = zStart2;
    	        newPtVOI.importCurve(x, y, z);

    	        //W
    	        standardColumnImage.set(185 + xDelta, 256 - yDelta, zStart2, 100);
    	        x[0] = 185 + xDelta;
    	        y[0] = 256 - yDelta;
    	        z[0] = zStart2;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //X
    	    	standardColumnImage.set(221 + xDelta, 291 - yDelta, zStart2, 100);
    	        x[0] = 221 + xDelta;
    	        y[0] = 291 - yDelta;
    	        z[0] = zStart2;
    	        newPtVOI.importCurve(x, y, z);

    		    //Y
    	        standardColumnImage.set(256 + xDelta, 256 - yDelta, zStart2, 100);
    	        x[0] = 256 + xDelta;
    	        y[0] = 256 - yDelta;
    	        z[0] = zStart2;
    	        newPtVOI.importCurve(x, y, z);
    	
    	        
    	        
    	        // r8
    	        //A
    	    	standardColumnImage.set(256, 398, zMiddle, 100);
    	        x[0] = 256;
    	        y[0] = 398;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //B
    	    	standardColumnImage.set(291, 362, zMiddle, 100);
    	        x[0] = 291;
    	        y[0] = 362;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //C
    	    	standardColumnImage.set(327, 327, zMiddle, 100);
    	        x[0] = 327;
    	        y[0] = 327;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //D
    	    	standardColumnImage.set(362, 291, zMiddle, 100);
    	        x[0] = 362;
    	        y[0] = 291;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //E
    	    	standardColumnImage.set(398, 256, zMiddle, 100);
    	        x[0] = 398;
    	        y[0] = 256;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //F
    	    	standardColumnImage.set(362, 221, zMiddle, 100);
    	        x[0] = 362;
    	        y[0] = 221;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //G
    	    	standardColumnImage.set(327, 185, zMiddle, 100);
    	        x[0] = 327;
    	        y[0] = 185;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //H
    	    	standardColumnImage.set(291, 150, zMiddle, 100);
    	        x[0] = 291;
    	        y[0] = 150;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //I
    	    	standardColumnImage.set(256, 114, zMiddle, 100);
    	        x[0] = 256;
    	        y[0] = 114;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //J
    	    	standardColumnImage.set(221, 150, zMiddle, 100);
    	        x[0] = 221;
    	        y[0] = 150;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //K
    	    	standardColumnImage.set(185, 185, zMiddle, 100);
    	        x[0] = 185;
    	        y[0] = 185;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //L
    	    	standardColumnImage.set(150, 221, zMiddle, 100);
    	        x[0] = 150;
    	        y[0] = 221;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //M
    	    	standardColumnImage.set(114, 256, zMiddle, 100);
    	        x[0] = 114;
    	        y[0] = 256;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //N
    	    	standardColumnImage.set(150, 291, zMiddle, 100);
    	        x[0] = 150;
    	        y[0] = 291;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //O
    	    	standardColumnImage.set(185, 327, zMiddle, 100);
    	        x[0] = 185;
    	        y[0] = 327;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //P
    	    	standardColumnImage.set(221, 362, zMiddle, 100);
    	        x[0] = 221;
    	        y[0] = 362;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);
   
    	        //Q
    	    	standardColumnImage.set(256, 327, zMiddle, 100);
    	        x[0] = 256;
    	        y[0] = 327;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);
    	
    	        //R
    			standardColumnImage.set(291, 291, zMiddle, 100);
    	        x[0] = 291;
    	        y[0] = 291;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //S
    	        standardColumnImage.set(327, 256, zMiddle, 100);
    	        x[0] = 327;
    	        y[0] = 256;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	    	//T
    	    	standardColumnImage.set(291, 221, zMiddle, 100);
    	        x[0] = 291;
    	        y[0] = 221;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //U
    	    	standardColumnImage.set(256, 185, zMiddle, 100);
    	        x[0] = 256;
    	        y[0] = 185;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);
    	
    	        //V
    	    	standardColumnImage.set(221, 221, zMiddle, 100);
    	        x[0] = 221;
    	        y[0] = 221;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //W
    	        standardColumnImage.set(185, 256, zMiddle, 100);
    	        x[0] = 185;
    	        y[0] = 256;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	    	//X
    	    	standardColumnImage.set(221, 291, zMiddle, 100);
    	        x[0] = 221;
    	        y[0] = 291;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);

    	        //Y
    	        standardColumnImage.set(256, 256, zMiddle, 100);
    	        x[0] = 256;
    	        y[0] = 256;
    	        z[0] = zMiddle;
    	        newPtVOI.importCurve(x, y, z);
    	
    	        
    	        
    	        
    	        // r7
    	        //A
    	    	standardColumnImage.set(256, 398, zEnd2, 100);
    	        x[0] = 256;
    	        y[0] = 398;
    	        z[0] = zEnd2;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //B
    	    	standardColumnImage.set(291, 362, zEnd2, 100);
    	        x[0] = 291;
    	        y[0] = 362;
    	        z[0] = zEnd2;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //C
    	    	standardColumnImage.set(327, 327, zEnd2, 100);
    	        x[0] = 327;
    	        y[0] = 327;
    	        z[0] = zEnd2;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //D
    	    	standardColumnImage.set(362, 291, zEnd2, 100);
    	        x[0] = 362;
    	        y[0] = 291;
    	        z[0] = zEnd2;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //E
    	    	standardColumnImage.set(398, 256, zEnd2, 100);
    	        x[0] = 398;
    	        y[0] = 256;
    	        z[0] = zEnd2;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //F
    	    	standardColumnImage.set(362, 221, zEnd2, 100);
    	        x[0] = 362;
    	        y[0] = 221;
    	        z[0] = zEnd2;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //G
    	    	standardColumnImage.set(327, 185, zEnd2, 100);
    	        x[0] = 327;
    	        y[0] = 185;
    	        z[0] = zEnd2;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //H
    	    	standardColumnImage.set(291, 150, zEnd2, 100);
    	        x[0] = 291;
    	        y[0] = 150;
    	        z[0] = zEnd2;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //I
    	    	standardColumnImage.set(256, 114, zEnd2, 100);
    	        x[0] = 256;
    	        y[0] = 114;
    	        z[0] = zEnd2;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //J
    	    	standardColumnImage.set(221, 150, zEnd2, 100);
    	        x[0] = 221;
    	        y[0] = 150;
    	        z[0] = zEnd2;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //K
    	    	standardColumnImage.set(185, 185, zEnd2, 100);
    	        x[0] = 185;
    	        y[0] = 185;
    	        z[0] = zEnd2;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //L
    	    	standardColumnImage.set(150, 221, zEnd2, 100);
    	        x[0] = 150;
    	        y[0] = 221;
    	        z[0] = zEnd2;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //M
    	    	standardColumnImage.set(114, 256, zEnd2, 100);
    	        x[0] = 114;
    	        y[0] = 256;
    	        z[0] = zEnd2;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //N
    	    	standardColumnImage.set(150, 291, zEnd2, 100);
    	        x[0] = 150;
    	        y[0] = 291;
    	        z[0] = zEnd2;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //O
    	    	standardColumnImage.set(185, 327, zEnd2, 100);
    	        x[0] = 185;
    	        y[0] = 327;
    	        z[0] = zEnd2;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //P
    	    	standardColumnImage.set(221, 362, zEnd2, 100);
    	        x[0] = 221;
    	        y[0] = 362;
    	        z[0] = zEnd2;
    	        newPtVOI.importCurve(x, y, z);
   
    	        //Q
    	    	standardColumnImage.set(256, 327, zEnd2, 100);
    	        x[0] = 256;
    	        y[0] = 327;
    	        z[0] = zEnd2;
    	        newPtVOI.importCurve(x, y, z);
    	
    	        //R
    	    	standardColumnImage.set(291, 291, zEnd2, 100);
    	        x[0] = 291;
    	        y[0] = 291;
    	        z[0] = zEnd2;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //S
    	        standardColumnImage.set(327, 256, zEnd2, 100);
    	        x[0] = 327;
    	        y[0] = 256;
    	        z[0] = zEnd2;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	    	//T
    	    	standardColumnImage.set(291, 221, zEnd2, 100);
    	        x[0] = 291;
    	        y[0] = 221;
    	        z[0] = zEnd2;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //U
    	    	standardColumnImage.set(256, 185, zEnd2, 100);
    	        x[0] = 256;
    	        y[0] = 185;
    	        z[0] = zEnd2;
    	        newPtVOI.importCurve(x, y, z);
    	
    	        //V
    	    	standardColumnImage.set(221, 221, zEnd2, 100);
    	        x[0] = 221;
    	        y[0] = 221;
    	        z[0] = zEnd2;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //W
    	        standardColumnImage.set(185, 256, zEnd2, 100);
    	        x[0] = 185;
    	        y[0] = 256;
    	        z[0] = zEnd2;
    	        newPtVOI.importCurve(x, y, z);
    	
    	    	//X
    	    	standardColumnImage.set(221, 291, zEnd2, 100);
    	        x[0] = 221;
    	        y[0] = 291;
    	        z[0] = zEnd2;
    	        newPtVOI.importCurve(x, y, z);

    	        //Y
    	        standardColumnImage.set(256, 256, zEnd2, 100);
    	        x[0] = 256;
    	        y[0] = 256;
    	        z[0] = zEnd2;
    	        newPtVOI.importCurve(x, y, z);
        		
        	}else {

            	// top
    	        //A
    	    	standardColumnImage.set(256 + xDelta, 398 - yDelta, zStart2, 100);
    	        x[0] = 256 + xDelta;
    	        y[0] = 398 - yDelta;
    	        z[0] = zStart2;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        
    	        //P
    	    	standardColumnImage.set(221 + xDelta, 362 - yDelta, zStart2, 100);
    	        x[0] = 221 + xDelta;
    	        y[0] = 362 - yDelta;
    	        z[0] = zStart2;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //O
    	    	standardColumnImage.set(185 + xDelta, 327 - yDelta, zStart2, 100);
    	        x[0] = 185 + xDelta;
    	        y[0] = 327 - yDelta;
    	        z[0] = zStart2;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //N
    	    	standardColumnImage.set(150 + xDelta, 291 - yDelta, zStart2, 100);
    	        x[0] = 150 + xDelta;
    	        y[0] = 291 - yDelta;
    	        z[0] = zStart2;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //M
    	    	standardColumnImage.set(114 + xDelta, 256 - yDelta, zStart2, 100);
    	        x[0] = 114 + xDelta;
    	        y[0] = 256 - yDelta;
    	        z[0] = zStart2;
    	        newPtVOI.importCurve(x, y, z);

    	        //L
    	    	standardColumnImage.set(150 + xDelta, 221 - yDelta, zStart2, 100);
    	        x[0] = 150 + xDelta;
    	        y[0] = 221 - yDelta;
    	        z[0] = zStart2;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //K
    	    	standardColumnImage.set(185 + xDelta, 185 - yDelta, zStart2, 100);
    	        x[0] = 185 + xDelta;
    	        y[0] = 185 - yDelta;
    	        z[0] = zStart2;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //J
    	    	standardColumnImage.set(221 + xDelta, 150 - yDelta, zStart2, 100);
    	        x[0] = 221 + xDelta;
    	        y[0] = 150 - yDelta;
    	        z[0] = zStart2;
    	        newPtVOI.importCurve(x, y, z);
    	        
      	        //I
      	    	standardColumnImage.set(256 + xDelta, 114 - yDelta, zStart2, 100);
      	        x[0] = 256 + xDelta;
      	        y[0] = 114 - yDelta;
      	        z[0] = zStart2;
      	        newPtVOI.importCurve(x, y, z);
      	        
      	        //H
    	    	standardColumnImage.set(291 + xDelta, 150 - yDelta, zStart2, 100);
    	        x[0] = 291 + xDelta;
    	        y[0] = 150 - yDelta;
    	        z[0] = zStart2;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //G
    	    	standardColumnImage.set(327 + xDelta, 185 - yDelta, zStart2, 100);
    	        x[0] = 327 + xDelta;
    	        y[0] = 185 - yDelta;
    	        z[0] = zStart2;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //F
    	    	standardColumnImage.set(362 + xDelta, 221 - yDelta, zStart2, 100);
    	        x[0] = 362 + xDelta;
    	        y[0] = 221 - yDelta;
    	        z[0] = zStart2;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //E
    	    	standardColumnImage.set(398 + xDelta, 256 - yDelta, zStart2, 100);
    	        x[0] = 398 + xDelta;
    	        y[0] = 256 - yDelta;
    	        z[0] = zStart2;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //D
    	    	standardColumnImage.set(362 + xDelta, 291 - yDelta, zStart2, 100);
    	        x[0] = 362 + xDelta;
    	        y[0] = 291 - yDelta;
    	        z[0] = zStart2;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //C
    	    	standardColumnImage.set(327 + xDelta, 327 - yDelta, zStart2, 100);
    	        x[0] = 327 + xDelta;
    	        y[0] = 327 - yDelta;
    	        z[0] = zStart2;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //B
    	    	standardColumnImage.set(291 + xDelta, 362 - yDelta, zStart2, 100);
    	        x[0] = 291 + xDelta;
    	        y[0] = 362 - yDelta;
    	        z[0] = zStart2;
    	        newPtVOI.importCurve(x, y, z);
      	       

            	//Q
    	    	standardColumnImage.set(256 + xDelta, 327 - yDelta, zStart2, 100);
    	        x[0] = 256 + xDelta;
    	        y[0] = 327 - yDelta;
    	        z[0] = zStart2;
    	        newPtVOI.importCurve(x, y, z);

    	        //R
    	    	standardColumnImage.set(221 + xDelta, 291 - yDelta, zStart2, 100);
    	        x[0] = 221 + xDelta;
    	        y[0] = 291 - yDelta;
    	        z[0] = zStart2;
    	        newPtVOI.importCurve(x, y, z);
            	
    	        //S
    	        standardColumnImage.set(185 + xDelta, 256 - yDelta, zStart2, 100);
    	        x[0] = 185 + xDelta;
    	        y[0] = 256 - yDelta;
    	        z[0] = zStart2;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //T
    			standardColumnImage.set(221 + xDelta, 221 - yDelta, zStart2, 100);
    	        x[0] = 221 + xDelta;
    	        y[0] = 221 - yDelta;
    	        z[0] = zStart2;
    	        newPtVOI.importCurve(x, y, z);

    	        //U
    	    	standardColumnImage.set(256 + xDelta, 185 - yDelta, zStart2, 100);
    	        x[0] = 256 + xDelta;
    	        y[0] = 185 - yDelta;
    	        z[0] = zStart2;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //V
    	    	standardColumnImage.set(291 + xDelta, 221 - yDelta, zStart2, 100);
    	        x[0] = 291 + xDelta;
    	        y[0] = 221 - yDelta;
    	        z[0] = zStart2;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //W
    	        standardColumnImage.set(327 + xDelta, 256 - yDelta, zStart2, 100);
    	        x[0] = 327 + xDelta;
    	        y[0] = 256 - yDelta;
    	        z[0] = zStart2;
    	        newPtVOI.importCurve(x, y, z);

    	        //X
    	    	standardColumnImage.set(291 + xDelta, 291 - yDelta, zStart2, 100);
    	        x[0] = 291 + xDelta;
    	        y[0] = 291 - yDelta;
    	        z[0] = zStart2;
    	        newPtVOI.importCurve(x, y, z);

    		    //Y
    	        standardColumnImage.set(256 + xDelta, 256 - yDelta, zStart2, 100);
    	        x[0] = 256 + xDelta;
    	        y[0] = 256 - yDelta;
    	        z[0] = zStart2;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        
    	        // r8
    	        //A
    	    	standardColumnImage.set(256, 398, zMiddle2, 100);
    	        x[0] = 256;
    	        y[0] = 398;
    	        z[0] = zMiddle2;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        
    	        //P
    	    	standardColumnImage.set(221, 362, zMiddle2, 100);
    	        x[0] = 221;
    	        y[0] = 362;
    	        z[0] = zMiddle2;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //O
    	    	standardColumnImage.set(185, 327, zMiddle2, 100);
    	        x[0] = 185;
    	        y[0] = 327;
    	        z[0] = zMiddle2;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //N
    	    	standardColumnImage.set(150, 291, zMiddle2, 100);
    	        x[0] = 150;
    	        y[0] = 291;
    	        z[0] = zMiddle2;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //M
    	    	standardColumnImage.set(114, 256, zMiddle2, 100);
    	        x[0] = 114;
    	        y[0] = 256;
    	        z[0] = zMiddle2;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //L
    	    	standardColumnImage.set(150, 221, zMiddle2, 100);
    	        x[0] = 150;
    	        y[0] = 221;
    	        z[0] = zMiddle2;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //K
    	    	standardColumnImage.set(185, 185, zMiddle2, 100);
    	        x[0] = 185;
    	        y[0] = 185;
    	        z[0] = zMiddle2;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //J
    	    	standardColumnImage.set(221, 150, zMiddle2, 100);
    	        x[0] = 221;
    	        y[0] = 150;
    	        z[0] = zMiddle2;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //I
    	    	standardColumnImage.set(256, 114, zMiddle2, 100);
    	        x[0] = 256;
    	        y[0] = 114;
    	        z[0] = zMiddle2;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //H
    	    	standardColumnImage.set(291, 150, zMiddle2, 100);
    	        x[0] = 291;
    	        y[0] = 150;
    	        z[0] = zMiddle2;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //G
    	    	standardColumnImage.set(327, 185, zMiddle2, 100);
    	        x[0] = 327;
    	        y[0] = 185;
    	        z[0] = zMiddle2;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //F
    	    	standardColumnImage.set(362, 221, zMiddle2, 100);
    	        x[0] = 362;
    	        y[0] = 221;
    	        z[0] = zMiddle2;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //E
    	    	standardColumnImage.set(398, 256, zMiddle2, 100);
    	        x[0] = 398;
    	        y[0] = 256;
    	        z[0] = zMiddle2;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //D
    	    	standardColumnImage.set(362, 291, zMiddle2, 100);
    	        x[0] = 362;
    	        y[0] = 291;
    	        z[0] = zMiddle2;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //C
    	    	standardColumnImage.set(327, 327, zMiddle2, 100);
    	        x[0] = 327;
    	        y[0] = 327;
    	        z[0] = zMiddle2;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //B
    	    	standardColumnImage.set(291, 362, zMiddle2, 100);
    	        x[0] = 291;
    	        y[0] = 362;
    	        z[0] = zMiddle2;
    	        newPtVOI.importCurve(x, y, z);

    	        
    	        //Q
    	    	standardColumnImage.set(256, 327, zMiddle2, 100);
    	        x[0] = 256;
    	        y[0] = 327;
    	        z[0] = zMiddle2;
    	        newPtVOI.importCurve(x, y, z);

    	        //R
    	    	standardColumnImage.set(221, 291, zMiddle2, 100);
    	        x[0] = 221;
    	        y[0] = 291;
    	        z[0] = zMiddle2;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //S
    	        standardColumnImage.set(185, 256, zMiddle2, 100);
    	        x[0] = 185;
    	        y[0] = 256;
    	        z[0] = zMiddle2;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //T
    			standardColumnImage.set(221, 221, zMiddle2, 100);
    	        x[0] = 221;
    	        y[0] = 221;
    	        z[0] = zMiddle2;
    	        newPtVOI.importCurve(x, y, z);

    	        //U
    	    	standardColumnImage.set(256, 185, zMiddle2, 100);
    	        x[0] = 256;
    	        y[0] = 185;
    	        z[0] = zMiddle2;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //V
    	    	standardColumnImage.set(291, 221, zMiddle2, 100);
    	        x[0] = 291;
    	        y[0] = 221;
    	        z[0] = zMiddle2;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //W
    	        standardColumnImage.set(327, 256, zMiddle2, 100);
    	        x[0] = 327;
    	        y[0] = 256;
    	        z[0] = zMiddle2;
    	        newPtVOI.importCurve(x, y, z);

    	        //X
    	    	standardColumnImage.set(291, 291, zMiddle2, 100);
    	        x[0] = 291;
    	        y[0] = 291;
    	        z[0] = zMiddle2;
    	        newPtVOI.importCurve(x, y, z);

    		    //Y
    	        standardColumnImage.set(256, 256, zMiddle2, 100);
    	        x[0] = 256;
    	        y[0] = 256;
    	        z[0] = zMiddle2;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        
    	        // r7
    	        //A
    	    	standardColumnImage.set(256, 398, zEnd2, 100);
    	        x[0] = 256;
    	        y[0] = 398;
    	        z[0] = zEnd2;
    	        newPtVOI.importCurve(x, y, z);

    	        //P
    	    	standardColumnImage.set(221, 362, zEnd2, 100);
    	        x[0] = 221;
    	        y[0] = 362;
    	        z[0] = zEnd2;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //O
    	    	standardColumnImage.set(185, 327, zEnd2, 100);
    	        x[0] = 185;
    	        y[0] = 327;
    	        z[0] = zEnd2;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //N
    	    	standardColumnImage.set(150, 291, zEnd2, 100);
    	        x[0] = 150;
    	        y[0] = 291;
    	        z[0] = zEnd2;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //M
    	    	standardColumnImage.set(114, 256, zEnd2, 100);
    	        x[0] = 114;
    	        y[0] = 256;
    	        z[0] = zEnd2;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //L
    	    	standardColumnImage.set(150, 221, zEnd2, 100);
    	        x[0] = 150;
    	        y[0] = 221;
    	        z[0] = zEnd2;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //K
    	    	standardColumnImage.set(185, 185, zEnd2, 100);
    	        x[0] = 185;
    	        y[0] = 185;
    	        z[0] = zEnd2;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //J
    	    	standardColumnImage.set(221, 150, zEnd2, 100);
    	        x[0] = 221;
    	        y[0] = 150;
    	        z[0] = zEnd2;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //I
    	    	standardColumnImage.set(256, 114, zEnd2, 100);
    	        x[0] = 256;
    	        y[0] = 114;
    	        z[0] = zEnd2;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //H
    	    	standardColumnImage.set(291, 150, zEnd2, 100);
    	        x[0] = 291;
    	        y[0] = 150;
    	        z[0] = zEnd2;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //G
    	    	standardColumnImage.set(327, 185, zEnd2, 100);
    	        x[0] = 327;
    	        y[0] = 185;
    	        z[0] = zEnd2;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //F
    	    	standardColumnImage.set(362, 221, zEnd2, 100);
    	        x[0] = 362;
    	        y[0] = 221;
    	        z[0] = zEnd2;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //E
    	    	standardColumnImage.set(398, 256, zEnd2, 100);
    	        x[0] = 398;
    	        y[0] = 256;
    	        z[0] = zEnd2;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //D
    	    	standardColumnImage.set(362, 291, zEnd2, 100);
    	        x[0] = 362;
    	        y[0] = 291;
    	        z[0] = zEnd2;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //C
    	    	standardColumnImage.set(327, 327, zEnd2, 100);
    	        x[0] = 327;
    	        y[0] = 327;
    	        z[0] = zEnd2;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //B
    	    	standardColumnImage.set(291, 362, zEnd2, 100);
    	        x[0] = 291;
    	        y[0] = 362;
    	        z[0] = zEnd2;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        
    	        
    	        //Q
    	    	standardColumnImage.set(256, 327, zEnd2, 100);
    	        x[0] = 256;
    	        y[0] = 327;
    	        z[0] = zEnd2;
    	        newPtVOI.importCurve(x, y, z);

    	        //R
    	    	standardColumnImage.set(221, 291, zEnd2, 100);
    	        x[0] = 221;
    	        y[0] = 291;
    	        z[0] = zEnd2;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //S
    	        standardColumnImage.set(185, 256, zEnd2, 100);
    	        x[0] = 185;
    	        y[0] = 256;
    	        z[0] = zEnd2;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //T
    			standardColumnImage.set(221, 221, zEnd2, 100);
    	        x[0] = 221;
    	        y[0] = 221;
    	        z[0] = zEnd2;
    	        newPtVOI.importCurve(x, y, z);

    	        //U
    	    	standardColumnImage.set(256, 185, zEnd2, 100);
    	        x[0] = 256;
    	        y[0] = 185;
    	        z[0] = zEnd2;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //V
    	    	standardColumnImage.set(291, 221, zEnd2, 100);
    	        x[0] = 291;
    	        y[0] = 221;
    	        z[0] = zEnd2;
    	        newPtVOI.importCurve(x, y, z);
    	        
    	        //W
    	        standardColumnImage.set(327, 256, zEnd2, 100);
    	        x[0] = 327;
    	        y[0] = 256;
    	        z[0] = zEnd2;
    	        newPtVOI.importCurve(x, y, z);

    	        //X
    	    	standardColumnImage.set(291, 291, zEnd2, 100);
    	        x[0] = 291;
    	        y[0] = 291;
    	        z[0] = zEnd2;
    	        newPtVOI.importCurve(x, y, z);

    		    //Y
    	        standardColumnImage.set(256, 256, zEnd2, 100);
    	        x[0] = 256;
    	        y[0] = 256;
    	        z[0] = zEnd2;
    	        newPtVOI.importCurve(x, y, z);
        		
        	}
        }
        
        
    

        
        
        
        
        

        
        /**/

        
        
        
        
        
        
        
        
        standardColumnImage.calcMinMax();

        // NEURON IMAGE
        newPtVOI = new VOI((short) 0, "point3D.voi", VOI.POINT, -1.0f);
        newPtVOI.setUID(newPtVOI.hashCode());
        neuronImage_grey.registerVOI(newPtVOI);
        int key;
        float[] f;
        for (int i = 0; i < pointsMap.size(); i++) {
            key = i + 1;
            if (pointsMap.get(new Integer(key)) != null) {
                x[0] = Math.round(pointsMap.get(new Integer(key))[0] / resols[0]);
                y[0] = Math.round(pointsMap.get(new Integer(key))[1] / resols[1]);
                z[0] = Math.round(pointsMap.get(new Integer(key))[2] / resols[2]);
                if (i == pointsMap.size() - 1) {
                    // this is the r7 center point!!
                    r7_27Coord = new float[3];
                    r7_27Coord[0] = x[0];
                    r7_27Coord[1] = y[0];
                    r7_27Coord[2] = z[0];
                }
                f = new float[3];
                f[0] = x[0];
                f[1] = y[0];
                f[2] = z[0];
                inputPointsList.add(f);
            } else {
                removePointsLabels.add(String.valueOf(key));
                x[0] = 10;
                y[0] = 10;
                z[0] = 10;
                inputPointsList.add(null);
            }
            transformedPointsList.add(null);
            newPtVOI.importCurve(x, y, z);
        }

        // remove whatever points from both std column and neuron image that were not available in the points file
        final VOIVector stdColumnVOIs = standardColumnImage.getVOIs();
        int nCurves;
        String label;
        VOI voi = stdColumnVOIs.elementAt(0);
        Vector<VOIBase> curves = voi.getCurves();
        nCurves = curves.size();
        for (int j = nCurves-1; j >= 0; j--) {
            final VOIPoint voiPoint = (VOIPoint) (curves.elementAt(j));
            label = voiPoint.getLabel();
            for (int k = 0; k < removePointsLabels.size(); k++) {
                final String removeLabel = removePointsLabels.get(k);
                if (label.equals(removeLabel)) {
                    curves.removeElement(voiPoint);
                }
            }
        }
        standardColumnImage.notifyImageDisplayListeners();

        VOIVector neuronVOIs = neuronImage_grey.getVOIs();
        voi = neuronVOIs.elementAt(0);
        curves = voi.getCurves();
        nCurves = curves.size();

        for (int j = nCurves-1; j >= 0; j--) {
            final VOIPoint voiPoint = (VOIPoint) (curves.elementAt(j));
            label = voiPoint.getLabel();
            for (int k = 0; k < removePointsLabels.size(); k++) {
                final String removeLabel = removePointsLabels.get(k);
                if (label.equals(removeLabel)) {
                    curves.removeElement(voiPoint);
                }
            }

        }

        neuronImage_grey.notifyImageDisplayListeners();
        
        
        if(doRigidOnly) {
        	// call rigid least squares alg
            leastSquaredAlg();
            leastSquaredAlgorithmPerformed();
        }else {
        	// call rigid least squares alg
            leastSquaredAlg();
            leastSquaredAlgorithmPerformed();

            // call non linear thin plate spline alg
            thinPlateSplineAlg();
            thinPlateSplineAlgorithmPerformed();
        }

        

        createFinalImage();
        
        //remove any null points from 
        ArrayList<float[]> al;
        int size = allFilamentCoords_newCoords.size();
        for(int i=0;i<size;i++) {
			 al = allFilamentCoords_newCoords.get(i);
			 float[] coords;
			 int size2 = al.size();
			 for(int k=size2-1;k>=0;k--) {
				 coords = al.get(k);
				 if(coords == null) {
					 System.out.println("removing " + k + " from " + i);
					 al.remove(k);
				 }
			 }
			 
        }
			 
			 
        System.gc();
        
      
        //create SWC file
        if(doSWC) {
        	createSWCFile();
        }
        


        if (standardColumnImage != null) {
            standardColumnImage.disposeLocal();
            standardColumnImage = null;
        }

        if (cityBlockImage != null) {
            cityBlockImage.disposeLocal();
            cityBlockImage = null;
        }

        if (neuronImage != null) {
            neuronImage.disposeLocal();
            neuronImage = null;
        }

        if (neuronImage_grey != null) {
            neuronImage_grey.disposeLocal();
            neuronImage_grey = null;
        }

        if (resultImage1 != null) {
            resultImage1.disposeLocal();
            resultImage1 = null;
        }
        

        
        
        if (greenImage!= null) {
        	greenImage.disposeLocal();
        	greenImage = null;
        }
        
        if (finalImage!= null) {
        	finalImage.disposeLocal();
        	finalImage = null;
        }
        
        
       /* if (greenRegionGrowMaskImage!= null) {
        	greenRegionGrowMaskImage.disposeLocal();
        	greenRegionGrowMaskImage = null;
        }
        */
        
        System.gc();

        final long endTime = System.currentTimeMillis();
        final long diffTime = endTime - begTime;
        final float seconds = ((float) diffTime) / 1000;
        if(outputTextArea != null) {
        	outputTextArea.append("** Algorithm took " + seconds + " seconds \n");
    	}else {
    		System.out.println("** Algorithm took " + seconds + " seconds \n");
    	}
        

        setCompleted(true);
    }
    
    
    /**
     * creates the swc file
     * 
     * www.neuronland.org/NLMorphologyConverter/MorphologyFormats/SWC/Spec.html
     * 
     */
    private void createSWCFile() {
    	
    	if(outputTextArea != null) {
    		outputTextArea.append("Creating SWC file..." + "\n");
            outputTextArea.append("\n");
    	}else {
    		System.out.println("Creating SWC file...");
    		System.out.println();
    	}
    	
    	
        
        
    	File filamentFile = new File(filamentFileParentDir + File.separator + standardizedFilamentFileName);
    	readFilamentFile_swc(filamentFile);
    	determineConnectivity1_swc(allFilamentCoords_swc);
    	determineAxon_swc(allFilamentCoords_swc);
		determineDistances_swc(allFilamentCoords_swc);
		
		if(outputTextArea != null) {
			outputTextArea.append("SWC - subsampling..." + "\n");
	        outputTextArea.append("\n");
    	}else {
    		System.out.println("SWC - subsampling...");
    		System.out.println();
    	}
		
		subsample_swc(allFilamentCoords_swc,newFilamentCoords_swc);
		
		if(outputTextArea != null) {
			outputTextArea.append("SWC - determining connectivity..." + "\n");
	        outputTextArea.append("\n");
    	}else {
    		System.out.println("SWC - determining connectivity...");
    		System.out.println(); 
    	}
		
		determineConnectivity2_swc(newFilamentCoords_swc);
		
		createGreenImage();
		
		/*outputTextArea.append("SWC - determining radii via region grow/distance map..." + "\n");
        outputTextArea.append("\n");
        determineRadiiRegionGrow_swc(newFilamentCoords_swc);
        
        output_swc(outputFilename_regionGrow,newFilamentCoords_swc);
		outputTextArea.append("Saving region grow SWC file to " + filamentFileParentDir + File.separator + outputFilename_auto + "\n");
        outputTextArea.append("\n");
        
        
		outputTextArea.append("SWC - determining radii autolatically..." + "\n");
        outputTextArea.append("\n");
		determineRadiiAutomatically_swc(newFilamentCoords_swc);
		
		output_swc(outputFilename_auto,newFilamentCoords_swc);
		outputTextArea.append("Saving automatic SWC file to " + filamentFileParentDir + File.separator + outputFilename_auto + "\n");
        outputTextArea.append("\n");*/
		if(outputTextArea != null) {
			outputTextArea.append("SWC - determining radd via threshold..." + "\n");
	        outputTextArea.append("\n");
    	}else {
    		System.out.println("SWC - determining radd via threshold..." ); 
    		System.out.println(); 
    	}
		
		determineRadiiThreshold_swc(newFilamentCoords_swc);
		
		String outputFilename = oldSurfaceFile.getName().substring(0, oldSurfaceFile.getName().indexOf(".")) + ".swc";
		output_swc(outputFilename,newFilamentCoords_swc);
		
		if(outputTextArea != null) {
			outputTextArea.append("Saving threshold SWC file to " + filamentFileParentDir + File.separator + outputFilename + "\n");
	        outputTextArea.append("\n");
    	}else {
    		System.out.println("Saving threshold SWC file to " + filamentFileParentDir + File.separator + outputFilename);
    		System.out.println(); 
    	}
		
        
        
		
		//new ViewJFrameImage(maskImageAuto);
		//new ViewJFrameImage(maskImage);
        //new ViewJFrameImage(maskImageRegionGrow);
    }
    
    
    /**
     * Determines initial conenctivity...how one block is connected to the previous blocks
	 * while we are at it...lets set the type value to axon (2) for the 0th block
	 * @return
	 */
	private void determineConnectivity1_swc(ArrayList <ArrayList<float[]>> filamentCoords) {
		//first block of code stays at -1
		try {
			int allFilamentsSize = filamentCoords.size();
			int alMatchSize;
			ArrayList<float[]> al;
			ArrayList<float[]> al2;
			ArrayList<float[]> alMatch;
			float[] coords = new float[6];
			float[] coords2;
			float[] coordsMatch = new float[6];
			al = filamentCoords.get(0);

			for(int m=0;m<al.size();m++) {
				coords = al.get(m);
				if(m==0) {
					coords[4] = -1;
				}
			    coords[5] = 2;
			    al.set(m, coords);
				
			}
			
			//HACK
			//I am using the allFilamnetCoords to determine connectivity b/c for some reason some
			// of the transformed filament points are not exact
			//
			//MAY BE DELETING THE ABOVE COMMENT SOON   5/13/2011
			
			
			
			/*for(int i=1;i<allFilamentsSize;i++) {
				 al = filamentCoords.get(i);
				 coords = al.get(0);
				 al2 = allFilamentCoords_newCoords.get(i);
				 coords2 = al2.get(0);

				 int k;
				 
				 for(k=0;k<i;k++) {
					 alMatch = allFilamentCoords_newCoords.get(k);
					 alMatchSize = alMatch.size();
					 coordsMatch[0] = alMatch.get(alMatchSize-1)[0];
					 coordsMatch[1] = alMatch.get(alMatchSize-1)[1];
					 coordsMatch[2] = alMatch.get(alMatchSize-1)[2];
					 if(coords2[0]==coordsMatch[0] && coords2[1]==coordsMatch[1] && coords2[2]==coordsMatch[2]) {
						 //set the connectivity of coords[4] to k+1
						 coords[4] = k+1;
						 al.set(0, coords);

						 break;
					 }
					 
				 }
				 
		   }*/
			
			
			
			
			
			for(int i=1;i<allFilamentsSize;i++) {
				
				
				 al = filamentCoords.get(i);
				 coords = al.get(0);
				
				int k;
				 
				 for(k=0;k<i;k++) {
					 alMatch = filamentCoords.get(k);
					 alMatchSize = alMatch.size();
					 
					 coordsMatch[0] = alMatch.get(alMatchSize-1)[0];
					 coordsMatch[1] = alMatch.get(alMatchSize-1)[1];
					 coordsMatch[2] = alMatch.get(alMatchSize-1)[2];
					 
					 if(coords[0]==coordsMatch[0] && coords[1]==coordsMatch[1] && coords[2]==coordsMatch[2]) {

						 coords[4] = k+1;
						 al.set(0, coords);


						 break;
					 }
					 
					 
					 
				 }
				
				
			}
			
			
			
			
			
			
			
			
			
			
			//make sure all are connected
			for(int i=1;i<allFilamentsSize;i++) {
				 al = filamentCoords.get(i);
				 coords = al.get(0);
				 if(coords[4] == 0) {
					 //this means this block is not connected!
					 String coordsString = coords[0] + "," + coords[1] + "," + coords[2];
					 System.out.println("Standardized IV file is not properly connecte: the block of points starting with " + coordsString + " is not connected to anything");
					 
					 break;
					 
				 }
				 
			}
			
			

		}catch(Exception e) {
			e.printStackTrace();
		}

	}
	
	
	/**
	 * Determines connectivity for all points
	 */
	private void determineConnectivity2_swc(ArrayList <ArrayList<float[]>> newFilamentCoords) {
		
		int newFilamentsSize = newFilamentCoords.size();
		int alSize;
		ArrayList<float[]> al,al2;
		float[] coords;
		float c;
		int count = 1;
		c=0;
		for(int i=0;i<newFilamentsSize;i++) {
			 al = newFilamentCoords.get(i);
			 alSize = al.size();
			
			 
			 for(int k=0;k<alSize;k++) {
				 
				 coords = al.get(k);
				 if(k==0) {
					 c = coords[4];
					 continue;
				 }else if(k==1) {
					 
					 if(i==0) {
						 coords[4] = count;
						 al.set(k, coords);
					 }else{
						 //get the last emements connectivity of c-1
						 al2 = newFilamentCoords.get((int)c-1);
						 //System.out.println("********" + al2.size());
						 coords[4] = al2.get(al2.size()-1)[4] + 1;
						 al.set(k, coords);
						 //counter = al2.size() + 1;
						 //counter = al2.size() +1;
						 
					 }
				 }else{
					 
					 coords[4] = count;
					 al.set(k, coords);
					 
					 
				 }

				 count = count + 1;
				
			 }
			 
		}
		
		
	}
	
	/**
	 * creates green channel image
	 */
	private void createGreenImage() {
		int extents[] = finalImage.getExtents();
		int length = finalImage.getExtents()[0] * finalImage.getExtents()[1] * finalImage.getExtents()[2];
		float[] greenBuffer = new float[length];
		try {
			finalImage.exportRGBData(2, 0, length, greenBuffer);
		}catch (Exception e) {
			e.printStackTrace();
		}
		/////////////////////////////////////////////////////////////////////////////////////////////////////////
		//We need to pass the green channel through a median filter or Coherence-Enhancing Diffusion
		greenImage = new ModelImage(ModelStorageBase.FLOAT, extents, "greenImage");
		try {
            greenImage.importData(0, greenBuffer, true);
        } catch (final IOException error) {
            System.out.println("IO exception");
            error.printStackTrace();
            return;
        }
		FileInfoImageXML[] fileInfoBases = new FileInfoImageXML[finalImage.getExtents()[2]];
		float[] finalImageResols = new float[3];
        finalImageResols[0] = neuronImage.getResolutions(0)[0];
        finalImageResols[1] = neuronImage.getResolutions(0)[1];
        finalImageResols[2] = neuronImage.getResolutions(0)[2];
        float[] f = null;
        if (r7_27Coord_transformed != null) {
            f = new float[3];
            f[0] = -r7_27Coord_transformed[0];
            f[1] = -r7_27Coord_transformed[1];
            f[2] = -r7_27Coord_transformed[2];
        }
		for (int i = 0; i < fileInfoBases.length; i++) {
            fileInfoBases[i] = new FileInfoImageXML(greenImage.getImageName(), null, FileUtility.XML);
            fileInfoBases[i].setUnitsOfMeasure(neuronImage.getFileInfo()[0].getUnitsOfMeasure());
            fileInfoBases[i].setResolutions(finalImageResols);
            fileInfoBases[i].setExtents(neuronImage.getExtents());
            if (r7_27Coord_transformed != null) {

                fileInfoBases[i].setOrigin(f);
            } else {
                fileInfoBases[i].setOrigin(neuronImage.getFileInfo()[0].getOrigin());
            }

            fileInfoBases[i].setDataType(ModelStorageBase.ARGB);

        }
		greenImage.setFileInfo(fileInfoBases);
        greenImage.calcMinMax();
        //new ViewJFrameImage(greenImage);
	}
    
	
	/**
	 * This determines radii via creating region grow and then a distance map
	 * and then walking along the value to get the radius
	 * @param newFilamentCoords
	 */
	/*private void determineRadiiRegionGrow_swc(ArrayList <ArrayList<float[]>> newFilamentCoords) {
		int[] extents;
		
		
		extents = {512, 512, 512};
        maskImageRegionGrow = new ModelImage(ModelStorageBase.UBYTE, extents, "maskImage_swc_regionGrow");
        for (int i = 0; i < maskImageRegionGrow.getExtents()[2]; i++) {
        	maskImageRegionGrow.setResolutions(i, resols);
        }
        FileInfoImageXML[] fileInfoBases = new FileInfoImageXML[maskImageRegionGrow.getExtents()[2]];
        for (int i = 0; i < fileInfoBases.length; i++) {
            fileInfoBases[i] = new FileInfoImageXML(maskImageRegionGrow.getImageName(), null, FileUtility.XML);
            fileInfoBases[i].setEndianess(finalImage.getFileInfo()[0].getEndianess());
            fileInfoBases[i].setUnitsOfMeasure(finalImage.getFileInfo()[0].getUnitsOfMeasure());
            fileInfoBases[i].setResolutions(finalImage.getFileInfo()[0].getResolutions());
            fileInfoBases[i].setExtents(finalImage.getExtents());
            fileInfoBases[i].setOrigin(finalImage.getFileInfo()[0].getOrigin());

        }
        maskImageRegionGrow.setFileInfo(fileInfoBases);
		

		paintBitmap = greenImage.getMask();
		int newFilamentsSize = newFilamentCoords.size();
		int alSize;
		ArrayList<float[]> al;
		float[] coords;
		int xCenterPixel,yCenterPixel,zCenterPixel;
		int[] imageExtents = greenImage.getExtents();
		BitSet seedPaintBitmap;
		float fuzzyThreshold = -1;
		boolean useVOI = false;
		boolean displayFuzzy = false;
		RegionGrowDialog growDialog = null;
		float upperBound = (float)greenImage.getMax();
		float lowerBound = 20;
		float value = 0;
		int sizeLimit = -1;
		float maxDistance = -1;
		boolean variableThresholds = false;
		AlgorithmRegionGrow regionGrowAlgo = new AlgorithmRegionGrow(greenImage, 1.0f, 1.0f);
		int timeSlice = 0;
		CubeBounds regionGrowBounds = new CubeBounds(imageExtents[0], 0, imageExtents[1], 0, imageExtents[2], 0);
		
		for(int i=0;i<newFilamentsSize;i++) {
			 al = newFilamentCoords.get(i);
			 alSize = al.size();
			 for(int k=0;k<alSize;k++) {
				 coords = al.get(k);
				 if(r7CenterPointFound) {
					 xCenterPixel = (int)Math.floor((coords[0]+r7_27Coord_transformed[0])/resols[0]);
					 yCenterPixel = (int)Math.floor((coords[1]+r7_27Coord_transformed[1])/resols[1]);
					 zCenterPixel = (int)Math.floor((coords[2]+r7_27Coord_transformed[2])/resols[2]);
				 }else {
					 xCenterPixel = (int)Math.floor(coords[0]/resols[0]);
					 yCenterPixel = (int)Math.floor(coords[1]/resols[1]);
					 zCenterPixel = (int)Math.floor(coords[2]/resols[2]);
				 }
				 value = greenImage.getFloat(xCenterPixel, yCenterPixel, zCenterPixel);
				 seedPaintBitmap = new BitSet();
				 regionGrowAlgo.regionGrow3D(seedPaintBitmap, new Point3D(xCenterPixel, yCenterPixel, zCenterPixel), fuzzyThreshold,
	                        useVOI, displayFuzzy, growDialog, lowerBound, upperBound, sizeLimit, maxDistance,
	                        variableThresholds, timeSlice, regionGrowBounds);

				 paintBitmap.or(seedPaintBitmap);
 
			 }
			 
		}
		
		greenImage.notifyImageDisplayListeners(null, true);
		
		
		//now create mask image
		extents = finalImage.getExtents();

		greenRegionGrowMaskImage = new ModelImage(ModelStorageBase.UBYTE, extents, "greenImage");

        boolean on = false;
        byte val;
        for(int i=0;i<paintBitmap.length();i++) {
        	on = paintBitmap.get(i);
        	if(on) {
        		val = (byte)1.0;
        	}else {
        		val = (byte)0;
        	}
        	greenRegionGrowMaskImage.set(i, val);
        	
        }
        
        FileInfoImageXML[] fileInfoBases2 = new FileInfoImageXML[finalImage.getExtents()[2]];
		float[] finalImageResols = new float[3];
        finalImageResols[0] = neuronImage.getResolutions(0)[0];
        finalImageResols[1] = neuronImage.getResolutions(0)[1];
        finalImageResols[2] = neuronImage.getResolutions(0)[2];
        float[] f = null;
        if (r7_27Coord_transformed != null) {
            f = new float[3];
            f[0] = -r7_27Coord_transformed[0];
            f[1] = -r7_27Coord_transformed[1];
            f[2] = -r7_27Coord_transformed[2];
        }
		for (int i = 0; i < fileInfoBases2.length; i++) {
			fileInfoBases2[i] = new FileInfoImageXML(greenImage.getImageName(), null, FileUtility.XML);
			fileInfoBases2[i].setUnitsOfMeasure(neuronImage.getFileInfo()[0].getUnitsOfMeasure());
			fileInfoBases2[i].setResolutions(finalImageResols);
			fileInfoBases2[i].setExtents(neuronImage.getExtents());
            if (r7_27Coord_transformed != null) {

            	fileInfoBases2[i].setOrigin(f);
            } else {
            	fileInfoBases2[i].setOrigin(neuronImage.getFileInfo()[0].getOrigin());
            }

            fileInfoBases2[i].setDataType(ModelStorageBase.ARGB);

        }
		greenRegionGrowMaskImage.setFileInfo(fileInfoBases2);
		greenRegionGrowMaskImage.calcMinMax();
		
		//create distance map
		int kernel = 0;
		AlgorithmMorphology3D distanceMapAlgo3D = new AlgorithmMorphology3D(greenRegionGrowMaskImage, kernel, 0, AlgorithmMorphology3D.DISTANCE_MAP,
                0, 0, 0, 0, true);
		distanceMapAlgo3D.run();
		
		greenRegionGrowMaskImage.calcMinMax();

        //new ViewJFrameImage(greenRegionGrowMaskImage);
		
		int numIter = 7;
		int xPix,yPix,zPix;
		int xPix1,yPix1,zPix1;
		float value1=0;
		float highestValue = 0;
		//now set radius based on distance map
        for(int i=0;i<newFilamentsSize;i++) {
			 al = newFilamentCoords.get(i);
			 alSize = al.size();
			 for(int k=0;k<alSize;k++) {
				 coords = al.get(k);
				 if(r7CenterPointFound) {
					 xCenterPixel = (int)Math.floor((coords[0]+r7_27Coord_transformed[0])/resols[0]);
					 yCenterPixel = (int)Math.floor((coords[1]+r7_27Coord_transformed[1])/resols[1]);
					 zCenterPixel = (int)Math.floor((coords[2]+r7_27Coord_transformed[2])/resols[2]);
				 }else {
					 xCenterPixel = (int)Math.floor(coords[0]/resols[0]);
					 yCenterPixel = (int)Math.floor(coords[1]/resols[1]);
					 zCenterPixel = (int)Math.floor(coords[2]/resols[2]);
				 }
				 value = greenRegionGrowMaskImage.getFloat(xCenterPixel, yCenterPixel, zCenterPixel);
				 
				 //now we need to look at the immediate neighbors of it to see
				 //if we are truly at the center (center will have highest value)
				 //if neighbor is higher...walk to it.   do this iteration 7 times
				 
				 xPix = xCenterPixel;
				 yPix = yCenterPixel;
				 zPix = zCenterPixel;
				 
				 for(int m=0;m<numIter;m++) {

					xPix1 = xPix;
					yPix1 = yPix+1;
					zPix1 = zPix;
					if(!(xPix1<0 || xPix1>=greenRegionGrowMaskImage.getExtents()[0] || yPix1<0 || yPix1>=greenRegionGrowMaskImage.getExtents()[1] || zPix1<0 || zPix1>=greenRegionGrowMaskImage.getExtents()[2])) {
						 value1 = greenRegionGrowMaskImage.getFloat(xPix1, yPix1, zPix1);
						 if(value1 > value) {
							 highestValue = value1;
							 xPix = xPix1;
							 yPix = yPix1;
							 zPix = zPix1;
							
						 }
					 } 
					
					xPix1 = xPix;
					yPix1 = yPix;
					zPix1 = zPix+1;
					if(!(xPix1<0 || xPix1>=greenRegionGrowMaskImage.getExtents()[0] || yPix1<0 || yPix1>=greenRegionGrowMaskImage.getExtents()[1] || zPix1<0 || zPix1>=greenRegionGrowMaskImage.getExtents()[2])) {
						 value1 = greenRegionGrowMaskImage.getFloat(xPix1, yPix1, zPix1);
						 if(value1 > value) {
							 if(value1 > highestValue) {
								 highestValue = value1;
								 xPix = xPix1;
								 yPix = yPix1;
								 zPix = zPix1;
							 }
							
						 }
					 } 
					
					
					xPix1 = xPix;
					yPix1 = yPix+1;
					zPix1 = zPix+1;
					if(!(xPix1<0 || xPix1>=greenRegionGrowMaskImage.getExtents()[0] || yPix1<0 || yPix1>=greenRegionGrowMaskImage.getExtents()[1] || zPix1<0 || zPix1>=greenRegionGrowMaskImage.getExtents()[2])) {
						 value1 = greenRegionGrowMaskImage.getFloat(xPix1, yPix1, zPix1);
						 if(value1 > value) {
							 if(value1 > highestValue) {
								 highestValue = value1;
								 xPix = xPix1;
								 yPix = yPix1;
								 zPix = zPix1;
							 }
					
						 }
					 } 
					
					xPix1 = xPix;
					yPix1 = yPix-1;
					zPix1 = zPix;
					if(!(xPix1<0 || xPix1>=greenRegionGrowMaskImage.getExtents()[0] || yPix1<0 || yPix1>=greenRegionGrowMaskImage.getExtents()[1] || zPix1<0 || zPix1>=greenRegionGrowMaskImage.getExtents()[2])) {
						 value1 = greenRegionGrowMaskImage.getFloat(xPix1, yPix1, zPix1);
						 if(value1 > value) {
							 if(value1 > highestValue) {
								 highestValue = value1;
								 xPix = xPix1;
								 yPix = yPix1;
								 zPix = zPix1;
							 }
						 }
					 } 
					
					xPix1 = xPix;
					yPix1 = yPix;
					zPix1 = zPix-1;
					if(!(xPix1<0 || xPix1>=greenRegionGrowMaskImage.getExtents()[0] || yPix1<0 || yPix1>=greenRegionGrowMaskImage.getExtents()[1] || zPix1<0 || zPix1>=greenRegionGrowMaskImage.getExtents()[2])) {
						 value1 = greenRegionGrowMaskImage.getFloat(xPix1, yPix1, zPix1);
						 if(value1 > value) {
							 if(value1 > highestValue) {
								 highestValue = value1;
								 xPix = xPix1;
								 yPix = yPix1;
								 zPix = zPix1;
							 }
						 }
					 } 
					
					xPix1 = xPix;
					yPix1 = yPix-1;
					zPix1 = zPix-1;
					if(!(xPix1<0 || xPix1>=greenRegionGrowMaskImage.getExtents()[0] || yPix1<0 || yPix1>=greenRegionGrowMaskImage.getExtents()[1] || zPix1<0 || zPix1>=greenRegionGrowMaskImage.getExtents()[2])) {
						 value1 = greenRegionGrowMaskImage.getFloat(xPix1, yPix1, zPix1);
						 if(value1 > value) {
							 if(value1 > highestValue) {
								 highestValue = value1;
								 xPix = xPix1;
								 yPix = yPix1;
								 zPix = zPix1;
							 }
						 }
					 }
					
					xPix1 = xPix;
					yPix1 = yPix+1;
					zPix1 = zPix-1;
					if(!(xPix1<0 || xPix1>=greenRegionGrowMaskImage.getExtents()[0] || yPix1<0 || yPix1>=greenRegionGrowMaskImage.getExtents()[1] || zPix1<0 || zPix1>=greenRegionGrowMaskImage.getExtents()[2])) {
						 value1 = greenRegionGrowMaskImage.getFloat(xPix1, yPix1, zPix1);
						 if(value1 > value) {
							 if(value1 > highestValue) {
								 highestValue = value1;
								 xPix = xPix1;
								 yPix = yPix1;
								 zPix = zPix1;
							 }
						 }
					 }
					
					xPix1 = xPix;
					yPix1 = yPix-1;
					zPix1 = zPix+1;
					if(!(xPix1<0 || xPix1>=greenRegionGrowMaskImage.getExtents()[0] || yPix1<0 || yPix1>=greenRegionGrowMaskImage.getExtents()[1] || zPix1<0 || zPix1>=greenRegionGrowMaskImage.getExtents()[2])) {
						 value1 = greenRegionGrowMaskImage.getFloat(xPix1, yPix1, zPix1);
						 if(value1 > value) {
							 if(value1 > highestValue) {
								 highestValue = value1;
								 xPix = xPix1;
								 yPix = yPix1;
								 zPix = zPix1;
							 }
						 }
					 }
					
					
					
					xPix1 = xPix+1;
					yPix1 = yPix;
					zPix1 = zPix;
					if(!(xPix1<0 || xPix1>=greenRegionGrowMaskImage.getExtents()[0] || yPix1<0 || yPix1>=greenRegionGrowMaskImage.getExtents()[1] || zPix1<0 || zPix1>=greenRegionGrowMaskImage.getExtents()[2])) {
						 value1 = greenRegionGrowMaskImage.getFloat(xPix1, yPix1, zPix1);
						 if(value1 > value) {
							 if(value1 > highestValue) {
								 highestValue = value1;
								 xPix = xPix1;
								 yPix = yPix1;
								 zPix = zPix1;
							 }
						 }
					 }
					
					xPix1 = xPix+1;
					yPix1 = yPix+1;
					zPix1 = zPix;
					if(!(xPix1<0 || xPix1>=greenRegionGrowMaskImage.getExtents()[0] || yPix1<0 || yPix1>=greenRegionGrowMaskImage.getExtents()[1] || zPix1<0 || zPix1>=greenRegionGrowMaskImage.getExtents()[2])) {
						 value1 = greenRegionGrowMaskImage.getFloat(xPix1, yPix1, zPix1);
						 if(value1 > value) {
							 if(value1 > highestValue) {
								 highestValue = value1;
								 xPix = xPix1;
								 yPix = yPix1;
								 zPix = zPix1;
							 }
						 }
					 }
					
					xPix1 = xPix+1;
					yPix1 = yPix;
					zPix1 = zPix+1;
					if(!(xPix1<0 || xPix1>=greenRegionGrowMaskImage.getExtents()[0] || yPix1<0 || yPix1>=greenRegionGrowMaskImage.getExtents()[1] || zPix1<0 || zPix1>=greenRegionGrowMaskImage.getExtents()[2])) {
						 value1 = greenRegionGrowMaskImage.getFloat(xPix1, yPix1, zPix1);
						 if(value1 > value) {
							 if(value1 > highestValue) {
								 highestValue = value1;
								 xPix = xPix1;
								 yPix = yPix1;
								 zPix = zPix1;
							 }
						 }
					 }
					
					xPix1 = xPix+1;
					yPix1 = yPix+1;
					zPix1 = zPix+1;
					if(!(xPix1<0 || xPix1>=greenRegionGrowMaskImage.getExtents()[0] || yPix1<0 || yPix1>=greenRegionGrowMaskImage.getExtents()[1] || zPix1<0 || zPix1>=greenRegionGrowMaskImage.getExtents()[2])) {
						 value1 = greenRegionGrowMaskImage.getFloat(xPix1, yPix1, zPix1);
						 if(value1 > value) {
							 if(value1 > highestValue) {
								 highestValue = value1;
								 xPix = xPix1;
								 yPix = yPix1;
								 zPix = zPix1;
							 }
						 }
					 }
					
					xPix1 = xPix+1;
					yPix1 = yPix-1;
					zPix1 = zPix;
					if(!(xPix1<0 || xPix1>=greenRegionGrowMaskImage.getExtents()[0] || yPix1<0 || yPix1>=greenRegionGrowMaskImage.getExtents()[1] || zPix1<0 || zPix1>=greenRegionGrowMaskImage.getExtents()[2])) {
						 value1 = greenRegionGrowMaskImage.getFloat(xPix1, yPix1, zPix1);
						 if(value1 > value) {
							 if(value1 > highestValue) {
								 highestValue = value1;
								 xPix = xPix1;
								 yPix = yPix1;
								 zPix = zPix1;
							 }
						 }
					 }
					
					 
					xPix1 = xPix+1;
					yPix1 = yPix;
					zPix1 = zPix-1;
					if(!(xPix1<0 || xPix1>=greenRegionGrowMaskImage.getExtents()[0] || yPix1<0 || yPix1>=greenRegionGrowMaskImage.getExtents()[1] || zPix1<0 || zPix1>=greenRegionGrowMaskImage.getExtents()[2])) {
						 value1 = greenRegionGrowMaskImage.getFloat(xPix1, yPix1, zPix1);
						 if(value1 > value) {
							 if(value1 > highestValue) {
								 highestValue = value1;
								 xPix = xPix1;
								 yPix = yPix1;
								 zPix = zPix1;
							 }
						 }
					 }
					
					xPix1 = xPix+1;
					yPix1 = yPix-1;
					zPix1 = zPix-1;
					if(!(xPix1<0 || xPix1>=greenRegionGrowMaskImage.getExtents()[0] || yPix1<0 || yPix1>=greenRegionGrowMaskImage.getExtents()[1] || zPix1<0 || zPix1>=greenRegionGrowMaskImage.getExtents()[2])) {
						 value1 = greenRegionGrowMaskImage.getFloat(xPix1, yPix1, zPix1);
						 if(value1 > value) {
							 if(value1 > highestValue) {
								 highestValue = value1;
								 xPix = xPix1;
								 yPix = yPix1;
								 zPix = zPix1;
							 }
						 }
					 }
					
					xPix1 = xPix+1;
					yPix1 = yPix+1;
					zPix1 = zPix-1;
					if(!(xPix1<0 || xPix1>=greenRegionGrowMaskImage.getExtents()[0] || yPix1<0 || yPix1>=greenRegionGrowMaskImage.getExtents()[1] || zPix1<0 || zPix1>=greenRegionGrowMaskImage.getExtents()[2])) {
						 value1 = greenRegionGrowMaskImage.getFloat(xPix1, yPix1, zPix1);
						 if(value1 > value) {
							 if(value1 > highestValue) {
								 highestValue = value1;
								 xPix = xPix1;
								 yPix = yPix1;
								 zPix = zPix1;
							 }
						 }
					 }
					
					xPix1 = xPix+1;
					yPix1 = yPix-1;
					zPix1 = zPix+1;
					if(!(xPix1<0 || xPix1>=greenRegionGrowMaskImage.getExtents()[0] || yPix1<0 || yPix1>=greenRegionGrowMaskImage.getExtents()[1] || zPix1<0 || zPix1>=greenRegionGrowMaskImage.getExtents()[2])) {
						 value1 = greenRegionGrowMaskImage.getFloat(xPix1, yPix1, zPix1);
						 if(value1 > value) {
							 if(value1 > highestValue) {
								 highestValue = value1;
								 xPix = xPix1;
								 yPix = yPix1;
								 zPix = zPix1;
							 }
						 }
					 }
					
					
					
					
					
					
					
					xPix1 = xPix-1;
					yPix1 = yPix;
					zPix1 = zPix;
					if(!(xPix1<0 || xPix1>=greenRegionGrowMaskImage.getExtents()[0] || yPix1<0 || yPix1>=greenRegionGrowMaskImage.getExtents()[1] || zPix1<0 || zPix1>=greenRegionGrowMaskImage.getExtents()[2])) {
						 value1 = greenRegionGrowMaskImage.getFloat(xPix1, yPix1, zPix1);
						 if(value1 > value) {
							 if(value1 > highestValue) {
								 highestValue = value1;
								 xPix = xPix1;
								 yPix = yPix1;
								 zPix = zPix1;
							 }
						 }
					 }
					
					xPix1 = xPix-1;
					yPix1 = yPix+1;
					zPix1 = zPix;
					if(!(xPix1<0 || xPix1>=greenRegionGrowMaskImage.getExtents()[0] || yPix1<0 || yPix1>=greenRegionGrowMaskImage.getExtents()[1] || zPix1<0 || zPix1>=greenRegionGrowMaskImage.getExtents()[2])) {
						 value1 = greenRegionGrowMaskImage.getFloat(xPix1, yPix1, zPix1);
						 if(value1 > value) {
							 if(value1 > highestValue) {
								 highestValue = value1;
								 xPix = xPix1;
								 yPix = yPix1;
								 zPix = zPix1;
							 }
						 }
					 }
					
					xPix1 = xPix-1;
					yPix1 = yPix;
					zPix1 = zPix+1;
					if(!(xPix1<0 || xPix1>=greenRegionGrowMaskImage.getExtents()[0] || yPix1<0 || yPix1>=greenRegionGrowMaskImage.getExtents()[1] || zPix1<0 || zPix1>=greenRegionGrowMaskImage.getExtents()[2])) {
						 value1 = greenRegionGrowMaskImage.getFloat(xPix1, yPix1, zPix1);
						 if(value1 > value) {
							 if(value1 > highestValue) {
								 highestValue = value1;
								 xPix = xPix1;
								 yPix = yPix1;
								 zPix = zPix1;
							 }
						 }
					 }
					
					xPix1 = xPix-1;
					yPix1 = yPix+1;
					zPix1 = zPix+1;
					if(!(xPix1<0 || xPix1>=greenRegionGrowMaskImage.getExtents()[0] || yPix1<0 || yPix1>=greenRegionGrowMaskImage.getExtents()[1] || zPix1<0 || zPix1>=greenRegionGrowMaskImage.getExtents()[2])) {
						 value1 = greenRegionGrowMaskImage.getFloat(xPix1, yPix1, zPix1);
						 if(value1 > value) {
							 if(value1 > highestValue) {
								 highestValue = value1;
								 xPix = xPix1;
								 yPix = yPix1;
								 zPix = zPix1;
							 }
						 }
					 }
					
					xPix1 = xPix-1;
					yPix1 = yPix-1;
					zPix1 = zPix;
					if(!(xPix1<0 || xPix1>=greenRegionGrowMaskImage.getExtents()[0] || yPix1<0 || yPix1>=greenRegionGrowMaskImage.getExtents()[1] || zPix1<0 || zPix1>=greenRegionGrowMaskImage.getExtents()[2])) {
						 value1 = greenRegionGrowMaskImage.getFloat(xPix1, yPix1, zPix1);
						 if(value1 > value) {
							 if(value1 > highestValue) {
								 highestValue = value1;
								 xPix = xPix1;
								 yPix = yPix1;
								 zPix = zPix1;
							 }
						 }
					 }
					
					 
					xPix1 = xPix-1;
					yPix1 = yPix;
					zPix1 = zPix-1;
					if(!(xPix1<0 || xPix1>=greenRegionGrowMaskImage.getExtents()[0] || yPix1<0 || yPix1>=greenRegionGrowMaskImage.getExtents()[1] || zPix1<0 || zPix1>=greenRegionGrowMaskImage.getExtents()[2])) {
						 value1 = greenRegionGrowMaskImage.getFloat(xPix1, yPix1, zPix1);
						 if(value1 > value) {
							 if(value1 > highestValue) {
								 highestValue = value1;
								 xPix = xPix1;
								 yPix = yPix1;
								 zPix = zPix1;
							 }
						 }
					 }
					
					xPix1 = xPix-1;
					yPix1 = yPix-1;
					zPix1 = zPix-1;
					if(!(xPix1<0 || xPix1>=greenRegionGrowMaskImage.getExtents()[0] || yPix1<0 || yPix1>=greenRegionGrowMaskImage.getExtents()[1] || zPix1<0 || zPix1>=greenRegionGrowMaskImage.getExtents()[2])) {
						 value1 = greenRegionGrowMaskImage.getFloat(xPix1, yPix1, zPix1);
						 if(value1 > value) {
							 if(value1 > highestValue) {
								 highestValue = value1;
								 xPix = xPix1;
								 yPix = yPix1;
								 zPix = zPix1;
							 }
						 }
					 }
					
					xPix1 = xPix-1;
					yPix1 = yPix+1;
					zPix1 = zPix-1;
					if(!(xPix1<0 || xPix1>=greenRegionGrowMaskImage.getExtents()[0] || yPix1<0 || yPix1>=greenRegionGrowMaskImage.getExtents()[1] || zPix1<0 || zPix1>=greenRegionGrowMaskImage.getExtents()[2])) {
						 value1 = greenRegionGrowMaskImage.getFloat(xPix1, yPix1, zPix1);
						 if(value1 > value) {
							 if(value1 > highestValue) {
								 highestValue = value1;
								 xPix = xPix1;
								 yPix = yPix1;
								 zPix = zPix1;
							 }
						 }
					 }
					
					xPix1 = xPix-1;
					yPix1 = yPix-1;
					zPix1 = zPix+1;
					if(!(xPix1<0 || xPix1>=greenRegionGrowMaskImage.getExtents()[0] || yPix1<0 || yPix1>=greenRegionGrowMaskImage.getExtents()[1] || zPix1<0 || zPix1>=greenRegionGrowMaskImage.getExtents()[2])) {
						 value1 = greenRegionGrowMaskImage.getFloat(xPix1, yPix1, zPix1);
						 if(value1 > value) {
							 if(value1 > highestValue) {
								 highestValue = value1;
								 xPix = xPix1;
								 yPix = yPix1;
								 zPix = zPix1;
							 }
						 }
					 }
					 
					 
					 
				 }
				 
				 if(xPix == xCenterPixel && yPix == yCenterPixel && zPix == zCenterPixel) {
					 //this means we didnt walk at all
					 coords[3] = value;
					 
				 }else {
					 coords[3] = highestValue;
				 }

				 al.set(k, coords);
				 
			 }
        }
        
        
        
        
        
        float resX = resols[0];
		float resY = resols[1];
		float resZ = resols[2];
		
		float increaseRadiusBy = resZ;
		

		float radius;
		float radiusSquared;
		float xDist,yDist,zDist;
		float distance;
		int xStart,yStart,zStart;
		int xEnd,yEnd,zEnd;

        for(int i=0;i<newFilamentsSize;i++) {
			 al = newFilamentCoords.get(i);
			 alSize = al.size();
			 for(int k=0;k<alSize;k++) {
				 coords = al.get(k);
				 
				 if(r7CenterPointFound) {
					 xCenterPixel = (int)Math.floor((coords[0]+r7_27Coord_transformed[0])/resols[0]);
					 yCenterPixel = (int)Math.floor((coords[1]+r7_27Coord_transformed[1])/resols[1]);
					 zCenterPixel = (int)Math.floor((coords[2]+r7_27Coord_transformed[2])/resols[2]);
				 }else {
					 xCenterPixel = (int)Math.floor(coords[0]/resols[0]);
					 yCenterPixel = (int)Math.floor(coords[1]/resols[1]);
					 zCenterPixel = (int)Math.floor(coords[2]/resols[2]);
				 }
				 
				 //expand radius..start with increaseRadiusBy and increse
				 loop:		for(radius=increaseRadiusBy;radius<=coords[3];radius=radius+increaseRadiusBy) {
								
								 radiusSquared = radius * radius;  //we will work with radius squared...that way we dont have to do SqrRt down in the for loops
								 
								 xStart = xCenterPixel - Math.round(radius/resX);
								 xStart = xStart - 1;
								 yStart = yCenterPixel - Math.round(radius/resY);
								 yStart = yStart - 1;
								 zStart = zCenterPixel - Math.round(radius/resZ);
								 zStart = zStart - 1;
								 
								 xEnd = xCenterPixel + Math.round(radius/resX);
								 xEnd = xEnd + 1;
								 yEnd = yCenterPixel + Math.round(radius/resY);
								 yEnd = yEnd + 1;
								 zEnd = zCenterPixel + Math.round(radius/resZ);
								 zEnd = zEnd + 1;
								 
								 for(int z=zStart;z<=zEnd;z++) {
									 zDist = ((z-zCenterPixel)*(resZ)) * ((z-zCenterPixel)*(resZ));
									 for(int y=yStart;y<=yEnd;y++) {
										 yDist = ((y-yCenterPixel)*(resY)) * ((y-yCenterPixel)*(resY));
										 for(int x=xStart;x<=xEnd;x++) {
											 xDist = ((x-xCenterPixel)*(resX)) * ((x-xCenterPixel)*(resX));
											 distance = xDist + yDist + zDist;
											 if(distance <= radiusSquared) {
												 //this means we have a valid pixel in the sphere
												 //first check to see of x,y,z are in bounds
												 if(x<0 || x>=finalImage.getExtents()[0] || y<0 || y>=finalImage.getExtents()[1] || z<0 || z>=finalImage.getExtents()[2]) {
													 continue;
												 }
												 
													maskImageRegionGrow.set(x, y, z, 100);
												 
											 }
											 
										 }
										 
									 }
								 }
							 } //end loop:
			 }
		}
        
	
	}*/
	
	
    
    
    /**
     * Dtermeines radius based upon a user-input threshold
     * Sphere keeps growing till threshold is hit
     */
    private void determineRadiiThreshold_swc(ArrayList <ArrayList<float[]>> newFilamentCoords) {
		
		///////////////////////////  
		/*int[] extents = {512, 512, 512};
        maskImage = new ModelImage(ModelStorageBase.UBYTE, extents, "maskImage_swc_threshold");
        for (int i = 0; i < maskImage.getExtents()[2]; i++) {
        	maskImage.setResolutions(i, resols);
        }
        final FileInfoImageXML[] fileInfoBases = new FileInfoImageXML[maskImage.getExtents()[2]];
        for (int i = 0; i < fileInfoBases.length; i++) {
            fileInfoBases[i] = new FileInfoImageXML(maskImage.getImageName(), null, FileUtility.XML);
            fileInfoBases[i].setEndianess(finalImage.getFileInfo()[0].getEndianess());
            fileInfoBases[i].setUnitsOfMeasure(finalImage.getFileInfo()[0].getUnitsOfMeasure());
            fileInfoBases[i].setResolutions(finalImage.getFileInfo()[0].getResolutions());
            fileInfoBases[i].setExtents(finalImage.getExtents());
            fileInfoBases[i].setOrigin(finalImage.getFileInfo()[0].getOrigin());

        }
        maskImage.setFileInfo(fileInfoBases);*/
        /////////////////////////////////////
		

		float resX = resols[0];
		float resY = resols[1];
		float resZ = resols[2];
		
		float increaseRadiusBy = resZ;
		
		int newFilamentsSize = newFilamentCoords.size();
		int alSize;
		ArrayList<float[]> al;
		float[] coords;
		int xCenterPixel,yCenterPixel,zCenterPixel;
		float radius;
		float radiusSquared;
		float xDist,yDist,zDist;
		float distance;
		int xStart,yStart,zStart;
		int xEnd,yEnd,zEnd;
		float greenValue;
		
		
		//intitilaize all raii to 0 first
		for(int i=0;i<newFilamentsSize;i++) {
			 al = newFilamentCoords.get(i);
			 alSize = al.size();
			 for(int k=0;k<alSize;k++) {
				 coords = al.get(k);
				 coords[3] = 0;
				 al.set(k, coords);
				 
			 }
		}
		
		
		
		
		
		for(int i=0;i<newFilamentsSize;i++) {
			 al = newFilamentCoords.get(i);
			 alSize = al.size();
			 for(int k=0;k<alSize;k++) {
				 coords = al.get(k);
				 if(r7CenterPointFound) {
					 xCenterPixel = (int)Math.floor((coords[0]+r7_27Coord_transformed[0])/resols[0]);
					 yCenterPixel = (int)Math.floor((coords[1]+r7_27Coord_transformed[1])/resols[1]);
					 zCenterPixel = (int)Math.floor((coords[2]+r7_27Coord_transformed[2])/resols[2]);
				 }else {
					 xCenterPixel = (int)Math.floor(coords[0]/resols[0]);
					 yCenterPixel = (int)Math.floor(coords[1]/resols[1]);
					 zCenterPixel = (int)Math.floor(coords[2]/resols[2]);
				 }
				 
				 //expand radius..start with increaseRadiusBy and increse
				 loop:		for(radius=increaseRadiusBy;;radius=radius+increaseRadiusBy) {
								
								 radiusSquared = radius * radius;  //we will work with radius squared...that way we dont have to do SqrRt down in the for loops
								 
								 xStart = xCenterPixel - Math.round(radius/resX);
								 xStart = xStart - 1;
								 yStart = yCenterPixel - Math.round(radius/resY);
								 yStart = yStart - 1;
								 zStart = zCenterPixel - Math.round(radius/resZ);
								 zStart = zStart - 1;
								 
								 xEnd = xCenterPixel + Math.round(radius/resX);
								 xEnd = xEnd + 1;
								 yEnd = yCenterPixel + Math.round(radius/resY);
								 yEnd = yEnd + 1;
								 zEnd = zCenterPixel + Math.round(radius/resZ);
								 zEnd = zEnd + 1;
								 
								 for(int z=zStart;z<=zEnd;z++) {
									 zDist = ((z-zCenterPixel)*(resZ)) * ((z-zCenterPixel)*(resZ));
									 for(int y=yStart;y<=yEnd;y++) {
										 yDist = ((y-yCenterPixel)*(resY)) * ((y-yCenterPixel)*(resY));
										 for(int x=xStart;x<=xEnd;x++) {
											 xDist = ((x-xCenterPixel)*(resX)) * ((x-xCenterPixel)*(resX));
											 distance = xDist + yDist + zDist;
											 if(distance <= radiusSquared) {
												 //this means we have a valid pixel in the sphere
												 //first check to see of x,y,z are in bounds
												 /*if(x<0 || x>=finalImage.getExtents()[0] || y<0 || y>=finalImage.getExtents()[1] || z<0 || z>=finalImage.getExtents()[2]) {
													 continue;
												 }
												 greenValue = finalImage.getFloatC(x, y, z, 2);*/
												 
												 
												 if(x<0 || x>=greenImage.getExtents()[0] || y<0 || y>=greenImage.getExtents()[1] || z<0 || z>=greenImage.getExtents()[2]) {
													 continue;
												 }
												 greenValue = greenImage.getFloat(x, y, z);
												 
												 
												 
												 
												 if(greenValue <= greenThresold) {
													 //this means we have exceeded the radius
													 //break the loop:  and move on to next point
													 //store the radius....but make sure you store radius-increaseRadiusBy since this one has exceeded
													 coords[3] = radius-increaseRadiusBy;
													 al.set(k, coords);
													 break loop;
												 }else {
													 ///////////////////////////////////////////////////////////
													//maskImage.set(x, y, z, 100);
												 }
											 }
											 
										 }
										 
									 }
								 }
							 } //end loop:
			 }
		}
		
		
		//some radii might still be at 0 because the threshold was already met...in this case set the
		//radius to the "increaseRadiusBy" step size
		float r;
		for(int i=0;i<newFilamentsSize;i++) {
			 al = newFilamentCoords.get(i);
			 alSize = al.size();
			 for(int k=0;k<alSize;k++) {
				 coords = al.get(k);
				 r = coords[3];
				 if(r == 0) {
					 coords[3] = increaseRadiusBy;
					 al.set(k, coords);
				 }
			 }
		}
		
		
		
		
	}
    
    
    /**
	 * outputs the swc file
	 */
	private void output_swc(String filename,ArrayList <ArrayList<float[]>> newFilamentCoords) {
		try {
			
	        final File newSurfaceFile = new File(filamentFileParentDir + File.separator  + filename);
	        final FileWriter fw = new FileWriter(newSurfaceFile);
	        final BufferedWriter bw = new BufferedWriter(fw);

			int newFilamentsSize = newFilamentCoords.size();
			int alSize;
			ArrayList<float[]> al;
			float[] coords;
			float x,y,z,r,c,a;
			int cInt,aInt;
			int counter = 1;
			for(int i=0;i<newFilamentsSize;i++) {
				 al = newFilamentCoords.get(i);
				 alSize = al.size();
				 for(int k=0;k<alSize;k++) {
					 if(k==0 && i!=0) {
						 continue;
					 }
					 coords = al.get(k);
	
					 x = coords[0];
					 y = coords[1];
					 z = coords[2];
					 r = coords[3];
					 c = coords[4];
					 a = coords[5];
					 cInt = (int)c;
					 aInt = (int)a;
					 //System.out.println(counter + " " + aInt + " " + x + " " + y + " " + z + " " + r + " " + cInt) ;
					 bw.write(counter + " " + aInt + " " + x + " " + y + " " + z + " " + r + " " + cInt);
					 bw.newLine();
					 counter++;
					 //System.out.println("   " + Math.abs(Math.round(x/resols[0])) + " " + Math.abs(Math.round(y/resols[1])) + " " + Math.abs(Math.round(z/resols[2])));
				 }
				 
			}
			
		
			bw.close();
		}catch(Exception e) {
			e.printStackTrace();
		}
	}
    
    
    
    /**
     * Dteremines radius automtaicalluy
     * 
     * This is done by convolving an increasing sized laplacian kernel to the center point.
     * The max value returned reflects the radius size
     */
    private void determineRadiiAutomatically_swc(ArrayList <ArrayList<float[]>> newFilamentCoords) {
		int newFilamentsSize = newFilamentCoords.size();
		int alSize;
		ArrayList<float[]> al;
		float[] coords;
		int xCenterPixel,yCenterPixel,zCenterPixel;
		float lap;
		float[] sigs = new float[3];
		int extents[] = finalImage.getExtents();
		int index;
		
		//System.out.println("i size is " + newFilamentCoords_swc.size());
		//System.out.println();
		
		int length = finalImage.getExtents()[0] * finalImage.getExtents()[1] * finalImage.getExtents()[2];
		float[] greenBuffer = new float[length];
		try {
			finalImage.exportRGBData(2, 0, length, greenBuffer);
		}catch (Exception e) {
			e.printStackTrace();
		}
		/////////////////////////////////////////////////////////////////////////////////////////////////////////
		//We need to pass the green channel through a median filter or Coherence-Enhancing Diffusion
		/*greenImage = new ModelImage(ModelStorageBase.FLOAT, extents, "greenImage");
		try {
            greenImage.importData(0, greenBuffer, true);
        } catch (final IOException error) {
            System.out.println("IO exception");
            error.printStackTrace();
            return;
        }
		FileInfoImageXML[] fileInfoBases = new FileInfoImageXML[finalImage.getExtents()[2]];
		float[] finalImageResols = new float[3];
        finalImageResols[0] = neuronImage.getResolutions(0)[0];
        finalImageResols[1] = neuronImage.getResolutions(0)[1];
        finalImageResols[2] = neuronImage.getResolutions(0)[2];
        float[] f = null;
        if (r7_27Coord_transformed != null) {
            f = new float[3];
            f[0] = -r7_27Coord_transformed[0];
            f[1] = -r7_27Coord_transformed[1];
            f[2] = -r7_27Coord_transformed[2];
        }
		for (int i = 0; i < fileInfoBases.length; i++) {
            fileInfoBases[i] = new FileInfoImageXML(greenImage.getImageName(), null, FileUtility.XML);
            fileInfoBases[i].setUnitsOfMeasure(neuronImage.getFileInfo()[0].getUnitsOfMeasure());
            fileInfoBases[i].setResolutions(finalImageResols);
            fileInfoBases[i].setExtents(neuronImage.getExtents());
            if (r7_27Coord_transformed != null) {

                fileInfoBases[i].setOrigin(f);
            } else {
                fileInfoBases[i].setOrigin(neuronImage.getFileInfo()[0].getOrigin());
            }

            fileInfoBases[i].setDataType(ModelStorageBase.ARGB);

        }
		greenImage.setFileInfo(fileInfoBases);
        greenImage.calcMinMax();*/
        //now we send it through the median filter
        /*int iters = 1;
        int kernelSize = 3;
        int kernelShape = 0;
        float stdDev = 1;
        boolean adaptiveSize = false;
        int maximumSize = 5;
        boolean wholeImage = true;
        medianAlgo = new AlgorithmMedian(greenImage, iters, kernelSize, kernelShape, stdDev, adaptiveSize,maximumSize, wholeImage);
        medianAlgo.run();
        //ok...now export buffer back to greenBuffer
		try {
			greenImage.exportData(0, length, greenBuffer);
		}catch (Exception e) {
			e.printStackTrace();
		}*/
        
        //send through coherence-enhancing diffusion filter
       /* cohResultImage = new ModelImage(ModelStorageBase.FLOAT, greenImage.getExtents(), "coherence");
        int numIterations = 1;
        float diffusitivityDenom = 0.001f;
        float derivativeScale = 0.5f;
        float gaussianScale = 2.0f;
        boolean do25D = true;
        boolean entireImage = true;
        coherenceAlg = new AlgorithmCoherenceEnhancingDiffusion(cohResultImage, greenImage, numIterations,diffusitivityDenom,derivativeScale, gaussianScale,do25D, entireImage);
        coherenceAlg.run();
        //ok...now export buffer back to greenBuffer
		try {
			cohResultImage.exportData(0, length, greenBuffer);
		}catch (Exception e) {
			e.printStackTrace();
		}*/
		////////////////////////////////////////////////////////////////////////////////////////////////////////////////
        
        
        
        
		
		
		
		//intitilaize all radii to -1 first
		for(int i=0;i<newFilamentsSize;i++) {
			 al = newFilamentCoords.get(i);
			 alSize = al.size();
			 for(int k=0;k<alSize;k++) {
				 coords = al.get(k);
				 coords[3] = -1;
				 al.set(k, coords);
				 
			 }
		}
		
		
	   int sigmaMax = 7;

		for(int i=0;i<newFilamentsSize;i++) {
			 al = newFilamentCoords.get(i);
			 alSize = al.size();
			 //System.out.println("k size is " + alSize);
			 for(int k=0;k<alSize;k++) {
				 coords = al.get(k);

				 
				 if(r7CenterPointFound) {
					 xCenterPixel = (int)Math.floor((coords[0]+r7_27Coord_transformed[0])/resols[0]);
					 yCenterPixel = (int)Math.floor((coords[1]+r7_27Coord_transformed[1])/resols[1]);
					 zCenterPixel = (int)Math.floor((coords[2]+r7_27Coord_transformed[2])/resols[2]);
				 }else {
					 xCenterPixel = (int)Math.floor(coords[0]/resols[0]);
					 yCenterPixel = (int)Math.floor(coords[1]/resols[1]);
					 zCenterPixel = (int)Math.floor(coords[2]/resols[2]);
				 }
				 
				 index = (zCenterPixel * (extents[0] * extents[1])) + (yCenterPixel * extents[0]) + xCenterPixel;
				 
				 //System.out.println(i + "," + k + ": "+ xCenterPixel + "," + yCenterPixel + "," + zCenterPixel);
				 if(outputTextArea != null) {
					 outputTextArea.append(i + "," + k + ": "+ xCenterPixel + "," + yCenterPixel + "," + zCenterPixel + "\n");	
			     }else {
			    	System.out.println(i + "," + k + ": "+ xCenterPixel + "," + yCenterPixel + "," + zCenterPixel);
			     }
				 
				 
				 System.gc();
				 float lapMax = -100;
				 float radiusPixelSpace = -1;
				 float radius = -1;
				 float increment = .5f;
				 
				 for (float s = 1; (s <= sigmaMax); s=s+increment) {
			            sigs[0] = s;
			            sigs[1] = s;
			            sigs[2] = s;
			            makeKernels3D(sigs);

			            lap = AlgorithmConvolver.convolve3DPtMed(index, extents, greenBuffer, kExtents, GxxData);
			            //System.out.println("   " + s + "   " + lap);	
			            if(outputTextArea != null) {
			            	outputTextArea.append("   " + s + "   " + lap + "\n");
			        	}else {
			        		System.out.println("   " + s + "   " + lap ); 
			        	}
			            
			            if(lap > lapMax) {
			            	lapMax = lap;
			            	radiusPixelSpace = s;
			            }else {
			            	break;
			            } 
			     }	
				 if(radiusPixelSpace != sigmaMax) {  //if its sigmaMax, then it kep getting bigger and bigger
					 //convert to micron space!!!!!!!!!!!!!!!!!!!!!!
					 radiusPixelSpace = radiusPixelSpace - increment;
					 radius = radiusPixelSpace * resols[0];
					 //add to coords
					 coords[3] = radius;
					 al.set(k, coords);

				 } 
			 } 
		}

		//for all radii that havent been set..(-1), becasue the convoultion process kept increqaasing, then set
		//the radius to the one of the previous one
		/*float r;
		float[] coords2;
		for(int i=0;i<newFilamentsSize;i++) {
			 al = newFilamentCoords.get(i);
			 alSize = al.size();
			 for(int k=0;k<alSize;k++) {
				 coords = al.get(k);
				 r = coords[3];
				 if(r == -1) {
					 //System.out.println("here" + k);
					 if(k!=0) {
						 coords2 = al.get(k-1);
						 coords[3] = coords2[3];
						 al.set(k, coords);
					 }else {
						 coords2 = al.get(k+1);
						 coords[3] = coords2[3];
						 al.set(k, coords);
					 }
				 }
			 }
		}*/
		
		
		
		
		/*int[] exts = {512, 512, 512};
        maskImageAuto = new ModelImage(ModelStorageBase.UBYTE, exts, "maskImage_swc_auto");
        for (int i = 0; i < maskImageAuto.getExtents()[2]; i++) {
        	maskImageAuto.setResolutions(i, resols);
        }
        final FileInfoImageXML[] fileInfoBases2 = new FileInfoImageXML[maskImageAuto.getExtents()[2]];
        for (int i = 0; i < fileInfoBases2.length; i++) {
        	fileInfoBases2[i] = new FileInfoImageXML(maskImageAuto.getImageName(), null, FileUtility.XML);
        	fileInfoBases2[i].setEndianess(finalImage.getFileInfo()[0].getEndianess());
        	fileInfoBases2[i].setUnitsOfMeasure(finalImage.getFileInfo()[0].getUnitsOfMeasure());
        	fileInfoBases2[i].setResolutions(finalImage.getFileInfo()[0].getResolutions());
        	fileInfoBases2[i].setExtents(finalImage.getExtents());
            fileInfoBases2[i].setOrigin(finalImage.getFileInfo()[0].getOrigin());

        }
        maskImageAuto.setFileInfo(fileInfoBases2);*/
        


		
        //may not need the following
        /*for(int i=0;i<newFilamentsSize;i++) {
			 al = newFilamentCoords.get(i);
			 alSize = al.size();
			 for(int k=0;k<alSize;k++) {
				 coords = al.get(k);
				 
				 if(r7CenterPointFound) {
					 xCenterPixel = (int)Math.floor((coords[0]+r7_27Coord_transformed[0])/resols[0]);
					 yCenterPixel = (int)Math.floor((coords[1]+r7_27Coord_transformed[1])/resols[1]);
					 zCenterPixel = (int)Math.floor((coords[2]+r7_27Coord_transformed[2])/resols[2]);
				 }else {
					 xCenterPixel = (int)Math.floor(coords[0]/resols[0]);
					 yCenterPixel = (int)Math.floor(coords[1]/resols[1]);
					 zCenterPixel = (int)Math.floor(coords[2]/resols[2]);
				 }
				 
				 //expand radius..start with increaseRadiusBy and increse
				 loop:		for(radius=increaseRadiusBy;radius<=coords[3];radius=radius+increaseRadiusBy) {
								
								 radiusSquared = radius * radius;  //we will work with radius squared...that way we dont have to do SqrRt down in the for loops
								 
								 xStart = xCenterPixel - Math.round(radius/resX);
								 xStart = xStart - 1;
								 yStart = yCenterPixel - Math.round(radius/resY);
								 yStart = yStart - 1;
								 zStart = zCenterPixel - Math.round(radius/resZ);
								 zStart = zStart - 1;
								 
								 xEnd = xCenterPixel + Math.round(radius/resX);
								 xEnd = xEnd + 1;
								 yEnd = yCenterPixel + Math.round(radius/resY);
								 yEnd = yEnd + 1;
								 zEnd = zCenterPixel + Math.round(radius/resZ);
								 zEnd = zEnd + 1;
								 
								 for(int z=zStart;z<=zEnd;z++) {
									 zDist = ((z-zCenterPixel)*(resZ)) * ((z-zCenterPixel)*(resZ));
									 for(int y=yStart;y<=yEnd;y++) {
										 yDist = ((y-yCenterPixel)*(resY)) * ((y-yCenterPixel)*(resY));
										 for(int x=xStart;x<=xEnd;x++) {
											 xDist = ((x-xCenterPixel)*(resX)) * ((x-xCenterPixel)*(resX));
											 distance = xDist + yDist + zDist;
											 if(distance <= radiusSquared) {
												 //this means we have a valid pixel in the sphere
												 //first check to see of x,y,z are in bounds
												 if(x<0 || x>=finalImage.getExtents()[0] || y<0 || y>=finalImage.getExtents()[1] || z<0 || z>=finalImage.getExtents()[2]) {
													 continue;
												 }
												 
													maskImageAuto.set(x, y, z, 100);
												 
											 }
											 
										 }
										 
									 }
								 }
							 } //end loop:
			 }
		}*/
        
        
        
      //some radii might still be at 0 because the threshold was already met...in this case set the
	  //radius to the radius after it
		float rad,rad2;
		float[] coords2;
		for(int i=0;i<newFilamentsSize;i++) {
			 al = newFilamentCoords.get(i);
			 alSize = al.size();
			 for(int k=0;k<alSize;k++) {
				 coords = al.get(k);
				 rad = coords[3];
				 if(rad == 0) {
					 //find next non-zero radius and set
					 for(int m=k;m<alSize;m++) {
						coords2 = al.get(m);
						rad2 = coords2[3];
						if(rad2 != 0) {
							coords[3] = rad2;
							al.set(k, coords);
							break;
						}
					 } 
				 }
			 }
		}
        
        

	}
    
    
    
    
    
    /**
     * Creates Gaussian derivative kernels.
     *
     * @param  sigmas  DOCUMENT ME!
     */
    private void makeKernels3D(float[] sigmas) {
        int xkDim, ykDim, zkDim;
        int[] derivOrder = new int[3];

        kExtents = new int[3];
        derivOrder[0] = 2;
        derivOrder[1] = 0;
        derivOrder[2] = 0;

        xkDim = Math.round(8 * sigmas[0]);

        if ((xkDim % 2) == 0) {
            xkDim++;
        }

        if (xkDim < 3) {
            xkDim = 3;
        }

        kExtents[0] = xkDim;

        ykDim = Math.round(8 * sigmas[1]);

        if ((ykDim % 2) == 0) {
            ykDim++;
        }

        if (ykDim < 3) {
            ykDim = 3;
        }

        kExtents[1] = ykDim;

        float scaleFactor = sigmas[2];

        sigmas[2] = sigmas[1];
        zkDim = Math.round(8 * sigmas[2]);

        if ((zkDim % 2) == 0) {
            zkDim++;
        }

        if (zkDim < 3) {
            zkDim = 3;
        }

        kExtents[2] = zkDim;

        GxxData = new float[xkDim * ykDim * zkDim];

        GenerateGaussian Gxx = new GenerateGaussian(GxxData, kExtents, sigmas, derivOrder);

        Gxx.calc(false);

        derivOrder[0] = 0;
        derivOrder[1] = 2;
        derivOrder[2] = 0;
        GyyData = new float[xkDim * ykDim * zkDim];

        GenerateGaussian Gyy = new GenerateGaussian(GyyData, kExtents, sigmas, derivOrder);

        Gyy.calc(false);

        derivOrder[0] = 0;
        derivOrder[1] = 0;
        derivOrder[2] = 2;
        GzzData = new float[xkDim * ykDim * zkDim];

        GenerateGaussian Gzz = new GenerateGaussian(GzzData, kExtents, sigmas, derivOrder);

        Gzz.calc(false);

        float tmp;

        for (int i = 0; i < GyyData.length; i++) {
            tmp = -(GxxData[i] + GyyData[i] + (GzzData[i] * scaleFactor));

            if (tmp > 0) {
                tmp *= amplificationFactor;
            }

            GxxData[i] = tmp;
        }
    }
    
    

    
    
    /**
     * Dtermines is block of points is axon or dendritic brach
     */
    private void determineAxon_swc(ArrayList <ArrayList<float[]>> filamentCoords) {
		//first find block of points that has highest z-value
		int allFilamentsSize = filamentCoords.size();
		ArrayList<float[]> al;
		int alSize;
		float[] coords = new float[6];
		float zVal = 0;
		float z;
		int highestZBlockIndex = 0;
		float connectedTo = 0;  //This is 1-based!
		 
		for(int i=0,m=1;i<allFilamentsSize;i++,m++) {
			 al = filamentCoords.get(i);
			 alSize = al.size();
			 float c = 0;
			 for(int k=0;k<alSize;k++) {
				 coords = al.get(k);
				 if(k==0) {
					 c = coords[4];
				 }
				 z = coords[2];
				 if(z>zVal) {
					 zVal=z;
					 highestZBlockIndex=i;
					 connectedTo = c;
				 }
			 } 
		}
		
		
		//next...set the highestZBlockIndex block to axon
		al = filamentCoords.get(highestZBlockIndex);
		alSize = al.size();
		for(int k=0;k<alSize;k++) {
			 coords = al.get(k);
			 coords[5] = 2;
			 al.set(k, coords);
		}
		
		//now traverse back until we get to soma (connectedTo=-1) and set to axon (2)
		while(connectedTo != -1) {
			int ind = (int)connectedTo - 1;
			al = filamentCoords.get(ind);
			alSize = al.size();
			for(int k=0;k<alSize;k++) {
				 coords = al.get(k);
				 if(k==0) {
					 connectedTo = coords[4];
				 }
				 coords[5] = 2;
				 al.set(k, coords);
			}
		}

	}
    

    
    
    
    /**
	 * subsample
	 */
	private void subsample_swc(ArrayList <ArrayList<float[]>> filamentCoords, ArrayList <ArrayList<float[]>> newFilamentCoords) {
		 int allFilamentsSize = filamentCoords.size();
		 float x,y,z,d,c,a;
		 float xb,yb,zb,db;
		 float xn,yn,zn;
		 float dDiff1,dDiff2;
		 float ratio;
		 int alSize;
		 ArrayList<float[]> al;
		 ArrayList<float[]> newAl;
		 float[] newCoords;
		 float totalDistance;

		 //go through each segment
		 for(int i=0;i<allFilamentsSize;i++) {
			 al = filamentCoords.get(i);
			 alSize = al.size();
			 newAl = new ArrayList<float[]>();
			 //the 0th one stays as is
			 x = al.get(0)[0];
			 y = al.get(0)[1];
			 z = al.get(0)[2];
			 c = al.get(0)[4];
			 a = al.get(0)[5];
			 newCoords = new float[6];
			 newCoords[0] = x;
			 newCoords[1] = y;
			 newCoords[2] = z;
			 //in newCoords, this is now radius
			 newCoords[3] = -1;
			 newCoords[4] = c;
			 newCoords[5] = a;
			 newAl.add(newCoords);
			 
			 
			 
			 
			 
			 //first get total distance of the segment
			 totalDistance = al.get(alSize-1)[3];
			 loop:	for(float k=subsamplingDistance;k<=totalDistance;k=k+subsamplingDistance) {
						 for(int m=1;m<alSize-1;m++) {
							 x = al.get(m)[0];
							 y = al.get(m)[1];
							 z = al.get(m)[2];
							 d = al.get(m)[3];
							 c = al.get(m)[4];
							 a = al.get(m)[5];
							 if(k==d) {
								 //highly unlikely...but just in case
								 newCoords = new float[6];
								 newCoords[0] = x;
								 newCoords[1] = y;
								 newCoords[2] = z;
								 newCoords[3] = 0;
								 newCoords[4] = c;
								 newCoords[5] = a;
								 newAl.add(newCoords);
								 continue loop;
							 }else if(k<d) {
								 xb = al.get(m-1)[0];
								 yb = al.get(m-1)[1];
								 zb = al.get(m-1)[2];
								 db = al.get(m-1)[3];
								 dDiff1 = k-db;
								 dDiff2 = d-db;
								 ratio = dDiff1/dDiff2;
								 xn = xb + (ratio*(x-xb));
								 yn = yb + (ratio*(y-yb));
								 zn = zb + (ratio*(z-zb));
								 newCoords = new float[6];
								 newCoords[0] = xn;
								 newCoords[1] = yn;
								 newCoords[2] = zn;
								 newCoords[3] = -1;
								 newCoords[4] = c;
								 newCoords[5] = a;
								 newAl.add(newCoords);
								 continue loop;
							 }
						 } 
					 } //end loop:
			 
			//the last one stays as is
			 x = al.get(alSize-1)[0];
			 y = al.get(alSize-1)[1];
			 z = al.get(alSize-1)[2];
			 c = al.get(alSize-1)[4];
			 a = al.get(alSize-1)[5];
			 newCoords = new float[6];
			 newCoords[0] = x;
			 newCoords[1] = y;
			 newCoords[2] = z;
			 newCoords[3] = -1;
			 newCoords[4] = c;
			 newCoords[5] = a;
			 newAl.add(newCoords);
			 
			 
			 newFilamentCoords.add(newAl);
		 }
		 
	}
	
	
    /**
	 * reads filament file
	 * @param filamaneFile
	 * @return
	 */
	private boolean readFilamentFile_swc(File surfaceFile) {
		boolean success = true;
		RandomAccessFile raFile = null;
		try {

			raFile = new RandomAccessFile(surfaceFile, "r");
			
			String line;
			
			
			while((line=raFile.readLine())!= null) {
				line = line.trim();
				if(line.startsWith("Translate1Dragger")) {
					break;
				}
				if(line.contains("Coordinate3")) {
					ArrayList<float[]> filamentCoords = new ArrayList<float[]>();
					while(!((line=raFile.readLine()).endsWith("}"))) {
						line = line.trim();
						if(!line.equals("")) {
							if(line.startsWith("point [")) {
								line = line.substring(line.indexOf("point [") + 7, line.length()).trim();
								if(line.equals("")) {
									continue;
								}
							}
							if(line.endsWith("]")) {
								line = line.substring(0, line.indexOf("]")).trim();
								if(line.equals("")) {
									continue;
								}
							}
							if(line.endsWith(",")) {
								line = line.substring(0, line.indexOf(",")).trim();
								if(line.equals("")) {
									continue;
								}
							}
							String[] splits = line.split("\\s+");
							splits[0] = splits[0].trim();
							splits[1] = splits[1].trim();
							splits[2] = splits[2].trim();
							float coord_x = new Float(splits[0]).floatValue();
							float coord_y = new Float(splits[1]).floatValue();
							float coord_z = new Float(splits[2]).floatValue();
							float x = coord_x;
							float y = coord_y;
							float z = coord_z;
							//x,y,z are the coordinates
							//next one is distance from beginning of segment to the point///for now initialize at 0
							//next one is connectivity...initialize at 0
							//next one is axon...2=axon   3=dendrite    initialize at 3
							float[] coords = {x,y,z,0,0,3};
							
							filamentCoords.add(coords);
						}
					}
					allFilamentCoords_swc.add(filamentCoords);
				}
				
				
				
				
				
			}
			raFile.close();
			
		}catch(Exception e) {
			try {
				if(raFile != null) {
					raFile.close();
				}
			}catch(Exception ex) {
				
			}
			e.printStackTrace();
			return false;
		}
		
		return success;
	}
    
    
    
    
    
    /**
	 * calculate distances of each point fron beginning of line segment
	 */
	private void determineDistances_swc(ArrayList <ArrayList<float[]>> filamentCoords) {
		 int allFilamentsSize = filamentCoords.size();

		 int x1Pix=0,y1Pix=0,z1Pix=0;
		 int x2Pix,y2Pix,z2Pix;
		 float d;
		 int alSize;
		 ArrayList<float[]> al;
		 float[] coords;
		 for(int i=0;i<allFilamentsSize;i++) {
			 al = filamentCoords.get(i);
			 alSize = al.size();
			 for(int k=0;k<alSize;k++) {
				 coords = al.get(k);
				 if (r7CenterPointFound) {
					 x2Pix = (int)Math.floor((coords[0]+r7_27Coord_transformed[0])/resols[0]);
					 y2Pix = (int)Math.floor((coords[1]+r7_27Coord_transformed[1])/resols[1]);
					 z2Pix = (int)Math.floor((coords[2]+r7_27Coord_transformed[2])/resols[2]);
				 }else {
					 x2Pix = (int)Math.floor(coords[0]/resols[0]);
					 y2Pix = (int)Math.floor(coords[1]/resols[1]);
					 z2Pix = (int)Math.floor(coords[2]/resols[2]);
				 }

				 if(k==0) {
					 x1Pix = x2Pix;
					 y1Pix = y2Pix;
					 z1Pix = z2Pix;
					 coords[3] = 0;
					 al.set(k, coords);
				 }else {
					d  = (float)MipavMath.distance(x1Pix, y1Pix, z1Pix, x2Pix, y2Pix, z2Pix, resols);
					coords[3] = d;
					al.set(k, coords);
				 }
			 }
		 }
	}
    
    
    
    
    
    
    
    
    
    
    
    
    
    

    /**
     * calls the non-linear thin plate spline alg
     */
    private void thinPlateSplineAlg() {
    	if(outputTextArea != null) {
    		outputTextArea.append("Calling Non Linear Thin Plate Spline Registration" + "\n");
    	}else {
    		System.out.println("Calling Non Linear Thin Plate Spline Registration"); 
    	}
        
        int nPtsA = 0; // = standardColumnImage.getVOIs().size();
        int nPtsB = 0; // = resultImage1.getVOIs().size()

        Vector3f[] ptA = null; // new Vector3f[nPtsA];
        Vector3f[] ptB = null; // new Vector3f[nPtsB];
        int i;

        Vector<VOIBase> curvesB;
        Vector<VOIBase> curvesM;

        curvesB = standardColumnImage.getVOIs().VOIAt(0).getCurves(); // curves[s] holds all VOIs in slice s
        nPtsA = curvesB.size();

        Preferences.debug("thin plate spline - nPtsA = " + nPtsA);

        curvesM = resultImage1.getVOIs().VOIAt(0).getCurves(); // curves[s] holds all VOIs in slice s
        nPtsB = curvesM.size();

        Preferences.debug("thin plate spline nPtsB = " + nPtsB);

        try {
            ptA = new Vector3f[nPtsA];
            ptB = new Vector3f[nPtsB];
        } catch (final OutOfMemoryError error) {
            ptA = null;
            ptB = null;
            System.gc();
            MipavUtil.displayError("JDialogRegistrationTPSpline: Out of memory on ptA");

            return;
        }

        for (i = 0; i < nPtsA; i++) {
            ptA[i] = ((VOIPoint) curvesB.elementAt(i)).exportPoint();
        }

        for (i = 0; i < nPtsB; i++) {
            ptB[i] = ((VOIPoint) curvesM.elementAt(i)).exportPoint();
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
        } catch (final OutOfMemoryError error) {
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
                    resultImage1, true);
        } catch (final OutOfMemoryError error) {
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
            neuronImage_grey = new ModelImage(ModelStorageBase.UBYTE, neuronImage.getExtents(), (neuronImage
                    .getImageName() + "Gray"));
        } else if (neuronImage.getType() == ModelStorageBase.ARGB_USHORT) {
            neuronImage_grey = new ModelImage(ModelStorageBase.USHORT, neuronImage.getExtents(), (neuronImage
                    .getImageName() + "Gray"));
        } else if (neuronImage.getType() == ModelStorageBase.ARGB_FLOAT) {
            neuronImage_grey = new ModelImage(ModelStorageBase.FLOAT, neuronImage.getExtents(), (neuronImage
                    .getImageName() + "Gray"));
        }

        for (int n = 0; n < neuronImage.getFileInfo().length; n++) {
            fInfoBase = (FileInfoBase) (neuronImage.getFileInfo(n).clone());
            fInfoBase.setDataType(neuronImage_grey.getType());
            neuronImage_grey.setFileInfo(fInfoBase, n);
        }

        // Make algorithm
        final AlgorithmRGBtoGray RGBAlgo = new AlgorithmRGBtoGray(neuronImage_grey, neuronImage, redValue, greenValue, blueValue, thresholdAverage, threshold, intensityAverage,false,0,255,0,255,0,255);

        RGBAlgo.run();

    }

    /**
     * alg performed for thin plate spline
     */
    private void thinPlateSplineAlgorithmPerformed() {
        if (resultImage1 != null) {
            resultImage1.disposeLocal();
            resultImage1 = null;
        }
        if (standardColumnImage != null) {
            standardColumnImage.disposeLocal();
            standardColumnImage = null;
        }
        System.gc();
    }

    /**
     * calls rigid least squared alg
     */
    private void leastSquaredAlg() {
    	if(outputTextArea != null) {
    		outputTextArea.append("Calling Rigid Least Squared Registration" + "\n");
    	}else {
    		System.out.println("Calling Rigid Least Squared Registration"); 
    	}
        
        int nPtsA = 0; // = standardColumnImage.getVOIs().size();
        int nPtsB = 0; // = neuronImage.getVOIs().size()

        Vector3f[] ptA = null; // new Vector3f[nPtsA];
        Vector3f[] ptB = null; // new Vector3f[nPtsB];
        int i;
        Vector<VOIBase> curves;

        curves = standardColumnImage.getVOIs().VOIAt(0).getCurves(); // curves[s] holds all VOIs in slice s
        nPtsA = curves.size();
System.out.println(nPtsA);
        Preferences.debug("nPtsA = " + nPtsA + "\n");
        ptA = new Vector3f[nPtsA];

        for (i = 0; i < nPtsA; i++) {
            ptA[i] = ((VOIPoint)curves.elementAt(i)).exportPoint();
        }

        curves = neuronImage_grey.getVOIs().VOIAt(0).getCurves();
        nPtsB = curves.size();
System.out.println(nPtsB);
        if (nPtsA != nPtsB) {
            MipavUtil.displayError("Both images must have the same number of points");

            return;
        }

        Preferences.debug("nPtsB = " + nPtsB + "\n");
        ptB = new Vector3f[nPtsB];
        for (i = 0; i < nPtsB; i++) {
            // ptNum = (int)(Short.valueOf(((VOIPoint)tmpptB[i]).getLabel()).shortValue());
            ptB[i] = ((VOIPoint)curves.elementAt(i)).exportPoint();
        }

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
            ptAmm[i] = new Vector3f( (ptA[i].X * xresA), (ptA[i].Y * yresA), (ptA[i].Z * zresA));
            ptBmm[i] = new Vector3f( (ptB[i].X * xresB), (ptB[i].Y * yresB), (ptB[i].Z * zresB));

        }

        LSMatch = new AlgorithmRegLeastSquares(ptAmm, ptBmm, DIM);
        LSMatch.run();

        ptA = null; // new Vector3f[nPtsA];
        ptB = null;
        ptAmm = null;
        ptBmm = null;

    }

    /**
     * alg performed for least squared alg
     */
    private void leastSquaredAlgorithmPerformed() {
        lsMatrix = LSMatch.getTransformBtoA();
        LSMatch.calculateResiduals();
        xdimA = standardColumnImage.getExtents()[0];
        ydimA = standardColumnImage.getExtents()[1];
        zdimA = standardColumnImage.getExtents()[2];

        final int[] extents = new int[] {xdimA, ydimA, zdimA};
        final float[] resolutions = new float[] {xresA, yresA, zresA};
        resultImage1 = new ModelImage(neuronImage_grey.getType(), extents, "LS Transformed image");

        for (int i = 0; i < zdimA; i++) {
            resultImage1.getFileInfo(i).setResolutions(resolutions);
            resultImage1.getFileInfo(i).setUnitsOfMeasure(neuronImage_grey.getFileInfo()[0].getUnitsOfMeasure());
        }

        if (neuronImage_grey.isColorImage() == false) {
            AlgorithmTransform.transformTrilinear(neuronImage_grey, resultImage1, LSMatch.getTransformBtoA(), null,
                    true, fillValue);
        } else {
            AlgorithmTransform.transformTrilinearC(neuronImage_grey, resultImage1, LSMatch.getTransformBtoA(), xdimA,
                    ydimA, zdimA, xresA, yresA, zresA, fillValue);
        }

        LSMatch.finalize();
        LSMatch = null;

        resultImage1.calcMinMax();
        resultImage1.setImageName("LS Transformed image");

        // need to transform points if there were any
        final VOIVector srcVOIs = neuronImage_grey.getVOIs();
        VOI newVOI;
        int nCurves;
        Vector3f pt, tPt;

        String label = "";

        ArrayList<Integer> indexAL_std = new ArrayList<Integer>();
        ArrayList<Integer> sliceAL_std = new ArrayList<Integer>();
        ArrayList<String> labelAL_std = new ArrayList<String>();
        TreeMap<Integer, AddVals> addCurvesMap = new TreeMap<Integer, AddVals>();
        VOI voi = srcVOIs.elementAt(0);
        newVOI = new VOI((short) 0, "point3D.voi", VOI.POINT, -1.0f);
        newVOI.setUID(newVOI.hashCode());
        Vector<VOIBase>[] curves = voi.getSortedCurves(neuronImage_grey.getExtents()[2]);
        
        Vector<VOIBase> curves3;
        curves3 = neuronImage_grey.getVOIs().VOIAt(0).getCurves(); //
        int nPtsC = curves3.size();

        int testTotal = 0;
        Integer labelInt;
        for (int s = 0; s < neuronImage_grey.getExtents()[2]; s++) {
            nCurves = curves[s].size();
            testTotal = testTotal + nCurves;
            for (int j = 0; j < nCurves; j++) {
                final float[] xPt = new float[1];
                final float[] yPt = new float[1];
                final float[] zPt = new float[1];
                final VOIPoint voiPoint = (VOIPoint) (curves[s].elementAt(j));
                pt = voiPoint.exportPoint();
                label = voiPoint.getLabel();
                final Vector3f pt2 = new Vector3f();
                pt2.X = pt.X * neuronImage_grey.getResolutions(0)[0];
                pt2.Y = pt.Y * neuronImage_grey.getResolutions(0)[1];
                pt2.Z = pt.Z * neuronImage_grey.getResolutions(0)[2];
                tPt = new Vector3f();
                lsMatrix.transformAsPoint3Df(pt2, tPt);
                xPt[0] = MipavMath.round(tPt.X / neuronImage_grey.getResolutions(0)[0]);
                yPt[0] = MipavMath.round(tPt.Y / neuronImage_grey.getResolutions(0)[1]);
                zPt[0] = MipavMath.round(tPt.Z / neuronImage_grey.getResolutions(0)[2]);
                if (xPt[0] < 0 || yPt[0] < 0 || zPt[0] < 0 || xPt[0] > neuronImage_grey.getExtents()[0] - 1
                        || yPt[0] > neuronImage_grey.getExtents()[1] - 1
                        || zPt[0] > neuronImage_grey.getExtents()[2] - 1) {
                	
                	if(outputTextArea != null) {
                		outputTextArea.append("neuron image point after rigid least squares alg is out of bounds - "
                                + label + "\n");
                	}else {
                		System.out.println("neuron image point after rigid least squares alg is out of bounds - "
                                + label); 
                	}
                    
                    indexAL_std.add(new Integer(j));
                    sliceAL_std.add(new Integer(s));
                    labelAL_std.add(label);
                } else {
                    final AddVals vals = new AddVals(xPt, yPt, zPt);
                    labelInt = new Integer(label);
                    addCurvesMap.put(labelInt, vals);
                }
            }
        }
        
        //the getSortedCurves does upperbounds checking and it removes some points..we need to add these to the labelAL_std list
        for(int i=0;i<nPtsC;i++) {
           Integer ig = new Integer(i);
           String l = String.valueOf(i);
           if(addCurvesMap.get(ig) == null) {
        	   boolean found = false;
        	   for(int k=0;k<labelAL_std.size();k++) {
        		   if(l.equals(labelAL_std.get(k))) {
        			   found = true;
        			   break;
        		   }
        	   }
        	   if(!found) {
        		   labelAL_std.add(l);
        	   }
        	   
        	   
           }
        }
        

        // need to add curves to result image...treemap is ascending order...so should be straightforward
        final Set<Integer> keySet = addCurvesMap.keySet();
        final Iterator<Integer> iter = keySet.iterator();
        Integer key;
        AddVals v;
        while (iter.hasNext()) {
            key = (Integer) iter.next();
            v = addCurvesMap.get(key);
            newVOI.importCurve(v.getXPt(), v.getYPt(), v.getZPt());
        }

        // need to remove the corresponding out of bounds points from the standard column image
        if (labelAL_std.size() > 0) {
            VOIVector stdVOIs = standardColumnImage.getVOIs();
            voi = stdVOIs.elementAt(0);
            curves = voi.getSortedCurves(standardColumnImage.getExtents()[2]);
            String removeLabel;
            for (int s = standardColumnImage.getExtents()[2] - 1; s >= 0; s--) {
                nCurves = curves[s].size();
                for (int j = nCurves - 1; j >= 0; j--) {
                    final VOIPoint voiPoint = (VOIPoint) (curves[s].elementAt(j));
                    label = voiPoint.getLabel();
                    for (int i = 0; i < labelAL_std.size(); i++) {
                        removeLabel = labelAL_std.get(i);
                        if (label.equals(removeLabel)) {
                        	if(outputTextArea != null) {
                        		outputTextArea.append("removing point from standard column image - " + label + "\n");
                        	}else {
                        		System.out.println("removing point from standard column image - " + label); 
                        	}
                            
                            voi.getCurves().removeElement(voiPoint);
                        }
                    }
                }
            }
        }

        standardColumnImage.notifyImageDisplayListeners();

        resultImage1.registerVOI(newVOI);

        resultImage1.notifyImageDisplayListeners();
        

        if (neuronImage_grey != null) {
            neuronImage_grey.disposeLocal();
            neuronImage_grey = null;
        }

        indexAL_std = null;
        sliceAL_std = null;
        labelAL_std = null;
        addCurvesMap = null;

    }

    /**
     * creates final image
     */
    private void createFinalImage() {
    	String neuronImageName = neuronImage.getImageFileName();
    	String finalImageName = neuronImageName.substring(0, neuronImageName.lastIndexOf(".")) + "_Standardized";
        // make LS MAtrix into inverse
        lsMatrix.Inverse();

        final int[] extents = {512, 512, 512};
        finalImage = new ModelImage(ModelStorageBase.ARGB, extents, finalImageName);

        final float[] finalImageResols = new float[3];
        finalImageResols[0] = neuronImage.getResolutions(0)[0];
        finalImageResols[1] = neuronImage.getResolutions(0)[1];
        finalImageResols[2] = neuronImage.getResolutions(0)[2];
        for (int i = 0; i < finalImage.getExtents()[2]; i++) {
            finalImage.setResolutions(i, finalImageResols);
        }

        final byte[] finalBuffer = new byte[512 * 512 * 512 * 4];
        int index = 0; // index into finalBuffer

        float[] tPt1 = new float[3];
        float[] tPt2 = new float[3];
        //float[] tPtR = new float[3];  //transformedm points only after rigd body

        float xmm, ymm, zmm;

        final short[] rgb_short = new short[3];
        byte[] rgb = new byte[3];

        byte[] neuronImageBuffer;
        final int length2 = neuronImageExtents[0] * neuronImageExtents[1] * neuronImageExtents[2] * 4;
        neuronImageBuffer = new byte[length2];
        try {
            neuronImage.exportData(0, length2, neuronImageBuffer);
        } catch (final IOException error) {
            System.out.println("IO exception");
            return;
        }

        byte r, g, b;


        final int[] extents2 = neuronImage.getExtents();
        final float[] finalImageRes = finalImage.getResolutions(0);
        final int[] finalImageExts = finalImage.getExtents();
        float diffX, diffY, diffZ;
        float diffTotal;

        // loop through each point in result image
        for (float z = 0; z < 512; z = z + samplingRate) {
            if ((float) Math.floor(z) == z) {
            	if(outputTextArea != null) {
            		outputTextArea.append("z is " + z + "\n");
            	}else {
            		System.out.println("z is " + z ); 
            	}
                

                if (z % 5 == 0) {
                    System.gc();
                }
            }

            for (float y = 0; y < 512; y = y + samplingRate) {

                for (float x = 0; x < 512; x = x + samplingRate) {

                    final float xFloor = (float) Math.floor(x);
                    final float yFloor = (float) Math.floor(y);
                    final float zFloor = (float) Math.floor(z);

                    if(doRigidOnly) {
                    	//xmm = xFloor * finalImageRes[0];
                        //ymm = yFloor * finalImageRes[1];
                        //zmm = zFloor * finalImageRes[2];
                        
                        xmm = x * finalImageRes[0];
                        ymm = y * finalImageRes[1];
                        zmm = z * finalImageRes[2];

                        lsMatrix.transform(xmm, ymm, zmm, tPt2);
                    }else {
                    	tPt1 = spline.getCorrespondingPoint(x, y, z);

                        xmm = tPt1[0] * finalImageRes[0];
                        ymm = tPt1[1] * finalImageRes[1];
                        zmm = tPt1[2] * finalImageRes[2];
                    	
                    	lsMatrix.transform(xmm, ymm, zmm, tPt2);
                    }
                    
                    


                    tPt2[0] = tPt2[0] / finalImageRes[0];
                    tPt2[1] = tPt2[1] / finalImageRes[1];
                    tPt2[2] = tPt2[2] / finalImageRes[2];
                    
                    //tPtR[0] = tPtR[0] / finalImageRes[0];
                    //tPtR[1] = tPtR[1] / finalImageRes[1];
                    //tPtR[2] = tPtR[2] / finalImageRes[2];

                    if (tPt2[0] < 0 || tPt2[1] < 0 || tPt2[2] < 0 || tPt2[0] > finalImageExts[0] - 1
                            || tPt2[1] > finalImageExts[1] - 1 || tPt2[2] > finalImageExts[2] - 1) {
                        rgb_short[0] = 0;
                        rgb_short[1] = 0;
                        rgb_short[2] = 0;

                    } else {
                        int floorPointIndex2 = 0;
                        final double tX2_floor = Math.floor(tPt2[0]);
                        final double tY2_floor = Math.floor(tPt2[1]);
                        final double tZ2_floor = Math.floor(tPt2[2]);
                        final float dx2 = (float) (tPt2[0] - tX2_floor);
                        final float dy2 = (float) (tPt2[1] - tY2_floor);
                        final float dz2 = (float) (tPt2[2] - tZ2_floor);

                        floorPointIndex2 = (int) ( ( (tZ2_floor * (extents2[0] * extents2[1]))
                                + (tY2_floor * extents2[0]) + tX2_floor) * 4);
                        if (floorPointIndex2 < neuronImageBuffer.length) {

                            if (xFloor == x && yFloor == y && zFloor == z) {
                                rgb = AlgorithmConvolver.getTrilinearC(floorPointIndex2, dx2, dy2, dz2, extents2,
                                        neuronImageBuffer);
                                rgb_short[0] = (short) (rgb[0] & 0xff);
                                rgb_short[1] = (short) (rgb[1] & 0xff);
                                rgb_short[2] = (short) (rgb[2] & 0xff);

                            }

                            diffX = Math.abs(tPt2[0] - r7_27Coord[0]);
                            diffY = Math.abs(tPt2[1] - r7_27Coord[1]);
                            diffZ = Math.abs(tPt2[2] - r7_27Coord[2]);

                            diffTotal = (diffX * diffX) + (diffY * diffY) + (diffZ * diffZ);

                            if (diffTotal < toleranceSq) {
                                if (r7_27Coord_transformed == null
                                        || (r7_27Coord_transformed != null && r7_27Coord_transformed[3] > diffTotal)) {
                                    r7_27Coord_transformed = new float[4];
                                    r7_27Coord_transformed[0] = x * finalImage.getResolutions(0)[0];
                                    r7_27Coord_transformed[1] = y * finalImage.getResolutions(0)[1];
                                    r7_27Coord_transformed[2] = z * finalImage.getResolutions(0)[2];
                                    r7_27Coord_transformed[3] = diffTotal;
                                    r7CenterPointFound = true;
                                }
                            }

                            for (int i = 0; i < inputPointsList.size(); i++) {
                                final float[] f = inputPointsList.get(i);
                                if (f != null) {

                                    diffX = Math.abs(tPt2[0] - f[0]);
                                    diffY = Math.abs(tPt2[1] - f[1]);
                                    diffZ = Math.abs(tPt2[2] - f[2]);

                                    diffTotal = (diffX * diffX) + (diffY * diffY) + (diffZ * diffZ);

                                    if (diffTotal < toleranceSq) {
                                        final float[] ft = transformedPointsList.get(i);
                                        if (ft == null || (ft != null && ft[3] > diffTotal)) {
                                            final float[] fTrans = new float[4];
                                            fTrans[0] = x * finalImage.getResolutions(0)[0];
                                            fTrans[1] = y * finalImage.getResolutions(0)[1];
                                            fTrans[2] = z * finalImage.getResolutions(0)[2];
                                            fTrans[3] = diffTotal;
                                            transformedPointsList.set(i, fTrans);
                                        }
                                    }

                                }
                            }

                            // Calculating new surface file points!!!!!
                            /*if (cityBlockImage.getByte((int) (tPt2[0] + 0.5f), (int) (tPt2[1] + 0.5f),
                                    (int) (tPt2[2] + 0.5f)) != 100) {*/
                            	
                            if (cityBlockImage.getByte(Math.round(tPt2[0]), Math.round(tPt2[1]),Math.round(tPt2[2])) != 100) {	
                                ArrayList<float[]> al;
                                ArrayList<float[]> al_new;
                                float[] coords;
                                final int allFilamentsSize = allFilamentCoords.size();
                                int alSize;
                                for (int i = 0; i < allFilamentsSize; i++) {
                                    al = allFilamentCoords.get(i);
                                    alSize = al.size();
                                    for (int k = 0; k < alSize; k++) {
                                        coords = al.get(k);
                                        diffX = Math.abs(tPt2[0] - coords[0]);
                                        diffY = Math.abs(tPt2[1] - coords[1]);
                                        diffZ = Math.abs(tPt2[2] - coords[2]);
                                        
                                        //5/30/2011
                                       /* diffX = Math.abs(Math.round(tPt2[0]) - coords[0]);
                                        diffY = Math.abs(Math.round(tPt2[1]) - coords[1]);
                                        diffZ = Math.abs(Math.round(tPt2[2]) - coords[2]);*/


                                        diffTotal = (diffX * diffX) + (diffY * diffY) + (diffZ * diffZ);

                                        if (diffTotal < toleranceSq) {
                                            final float[] nCoords = {x, y, z, diffTotal};
 
                                            al_new = allFilamentCoords_newCoords.get(i);
                                            final float[] ft = al_new.get(k);
                                            if (ft == null || (ft != null && ft[3] > diffTotal)) {
                                                al_new.set(k, nCoords);
                                            }

                                            // coords[3] = 1;
                                        }
                                    }

                                }
                            }

                        } else {
                            rgb_short[0] = 0;
                            rgb_short[1] = 0;
                            rgb_short[2] = 0;
                        }
                    }


                    if (xFloor == x && yFloor == y && zFloor == z) {
                        r = (byte) rgb_short[0];
                        g = (byte) rgb_short[1];
                        b = (byte) rgb_short[2];

                        // alpha
                        finalBuffer[index] = (byte) 255;
                        // r
                        index = index + 1;
                        finalBuffer[index] = r;
                        // g
                        index = index + 1;
                        finalBuffer[index] = g;
                        // b
                        index = index + 1;
                        finalBuffer[index] = b;
                        index = index + 1;
                    }
                }
            }

        }

        
        if(outputTextArea != null) {
        	outputTextArea.append("\n");
    	}else {
    		System.out.println(); 
    	}
        

        if (cityBlockImage != null) {
            cityBlockImage.disposeLocal();
            cityBlockImage = null;
        }

        try {
            finalImage.importData(0, finalBuffer, true);
        } catch (final IOException error) {
            System.out.println("IO exception");
            error.printStackTrace();
            return;
        }

        final FileInfoImageXML[] fileInfoBases = new FileInfoImageXML[finalImage.getExtents()[2]];
        float[] f = null;
        ;
        if (r7_27Coord_transformed != null) {
            f = new float[3];
            f[0] = -r7_27Coord_transformed[0];
            f[1] = -r7_27Coord_transformed[1];
            f[2] = -r7_27Coord_transformed[2];
        }
        for (int i = 0; i < fileInfoBases.length; i++) {
            fileInfoBases[i] = new FileInfoImageXML(finalImage.getImageName(), null, FileUtility.XML);
            fileInfoBases[i].setUnitsOfMeasure(neuronImage.getFileInfo()[0].getUnitsOfMeasure());
            fileInfoBases[i].setResolutions(finalImageResols);
            fileInfoBases[i].setExtents(neuronImage.getExtents());
            if (r7_27Coord_transformed != null) {

                fileInfoBases[i].setOrigin(f);
            } else {
                fileInfoBases[i].setOrigin(neuronImage.getFileInfo()[0].getOrigin());
            }

            fileInfoBases[i].setDataType(ModelStorageBase.ARGB);

        }

        if (r7_27Coord_transformed != null) {
        	if(outputTextArea != null) {
        		outputTextArea.append("Setting new origin of standardized image to transformed r7 center point" + "\n");
                outputTextArea.append("\n"); 
        	}else {
        		System.out.println("Setting new origin of standardized image to transformed r7 center point" );
        		System.out.println();
        	}
            
        }

        finalImage.setFileInfo(fileInfoBases);
        finalImage.calcMinMax();

        final FileIO fileIO = new FileIO();
        fileIO.setQuiet(true);
        final FileWriteOptions opts = new FileWriteOptions(true);
        opts.setFileType(FileUtility.ICS);
        opts.setFileDirectory(dir);
        //final String finalImageFileName = "finalStandardizedImage";
        opts.setFileName(finalImageName + ".ics");
        opts.setBeginSlice(0);
        opts.setEndSlice(511);
        opts.setOptionsSet(true);
        fileIO.writeImage(finalImage, opts);
        if(outputTextArea != null) {
        	outputTextArea.append("Saving final standardized image as: \n");
            outputTextArea.append(dir + finalImageName + ".ics" + "\n");
            outputTextArea.append("\n");
    	}else {
    		System.out.println("Saving final standardized image as: ");
    		System.out.println(dir + finalImageName + ".ics");
    		System.out.println(); 
    	}
        

        writeSurfaceFile();
        //if we do a flipx or flipy or flipz ...lets also write surface file without being flipped
        if(flipX || flipY || flipZ) {
        	flipX = false;
        	flipY = false;
        	flipZ = false;
        	writeSurfaceFile();
        }
        writeNewPointsFile();
        
        
        //TEST
        
    }

    /**
     * writes new points file
     * 
     * @return
     */
    private boolean writeNewPointsFile() {


        final String parentDir = pointsFile.getParent();
        final String newName = pointsFile.getName().substring(0, pointsFile.getName().indexOf("."))
                + "_transformedCoordinates.txt";
        if(outputTextArea != null) {
        	outputTextArea.append("Saving new transformed coordinates file as: \n");
            outputTextArea.append(parentDir + File.separator + newName + "\n");
            outputTextArea.append("\n");
    	}else {
    		System.out.println("Saving new transformed coordinates file as: ");
    		System.out.println(parentDir + File.separator + newName);
    		System.out.println(); 
    	}
        

        try {

            final File newPointsCoordinatesFile = new File(parentDir + File.separator + newName);
            final FileWriter fw = new FileWriter(newPointsCoordinatesFile);
            final BufferedWriter bw = new BufferedWriter(fw);
            for (int i = 0; i < transformedPointsList.size(); i++) {
                final float[] f = transformedPointsList.get(i);
                final String ind = String.valueOf(i + 1);
                /*if (i == 0) {
                    bw.write("top");
                    bw.newLine();
                }

                if (i == 9) {
                    bw.write("r8");
                    bw.newLine();
                }

                if (i == 18) {
                    bw.write("r7");
                    bw.newLine();
                }*/

                bw.write(ind + ":");
                if (f != null) {
                    bw.write( (f[0] - r7_27Coord_transformed[0]) + "," + (f[1] - r7_27Coord_transformed[1]) + ","
                            + (f[2] - r7_27Coord_transformed[2]));
                    bw.newLine();
                } else {
                    bw.newLine();
                }
            }
            bw.close();
            fw.close();

        } catch (final Exception e) {

        }

        return true;
    }

    /**
     * writes new surface file
     * 
     * @return
     */
    private boolean writeSurfaceFile() {

        final boolean success = true;
        int index = 0;

        filamentFileParentDir = oldSurfaceFile.getParent();
        standardizedFilamentFileName = oldSurfaceFile.getName().substring(0, oldSurfaceFile.getName().indexOf(".")) + "_standardized";
        if (flipX) {
        	standardizedFilamentFileName = standardizedFilamentFileName + "_flipX";
        }
        if (flipY) {
        	standardizedFilamentFileName = standardizedFilamentFileName + "_flipY";
        }
        if (flipZ) {
        	standardizedFilamentFileName = standardizedFilamentFileName + "_flipZ";
        }
        standardizedFilamentFileName = standardizedFilamentFileName + ".iv";
        if(outputTextArea != null) {
        	outputTextArea.append("Saving new filament file as: \n");
            outputTextArea.append(filamentFileParentDir + File.separator + standardizedFilamentFileName + "\n");
            outputTextArea.append("\n");
    	}else {
    		System.out.println("Saving new filament file as: ");
    		System.out.println(filamentFileParentDir + File.separator + standardizedFilamentFileName);
    		System.out.println(); 
    	}
        

        try {
            final RandomAccessFile raFile = new RandomAccessFile(oldSurfaceFile, "r");
            final File newSurfaceFile = new File(filamentFileParentDir + File.separator + standardizedFilamentFileName);
            final FileWriter fw = new FileWriter(newSurfaceFile);
            final BufferedWriter bw = new BufferedWriter(fw);

            String line;

            while ( (line = raFile.readLine()) != null) {
                line = line.trim();

                if (line.startsWith("point [")) {
                    if (index < allFilamentCoords_newCoords.size()) {

                        final float tPt3[] = new float[3];

                        final ArrayList<float[]> filCoords = allFilamentCoords_newCoords.get(index);
                        bw.write("point [");
                        bw.newLine();
                        for (int k = 0; k < filCoords.size(); k++) {

                            final float[] filCoord = filCoords.get(k);

                            if (filCoord != null) {
                                if (r7CenterPointFound) {
                                    tPt3[0] = (filCoord[0] * finalImage.getResolutions(0)[0])
                                            - r7_27Coord_transformed[0];
                                    tPt3[1] = (filCoord[1] * finalImage.getResolutions(0)[1])
                                            - r7_27Coord_transformed[1];
                                    tPt3[2] = (filCoord[2] * finalImage.getResolutions(0)[2])
                                            - r7_27Coord_transformed[2];

                                } else {
                                    tPt3[0] = filCoord[0] * finalImage.getResolutions(0)[0];
                                    tPt3[1] = filCoord[1] * finalImage.getResolutions(0)[1];
                                    tPt3[2] = filCoord[2] * finalImage.getResolutions(0)[2];
                                }

                                if (flipX) {
                                    tPt3[0] = tPt3[0] * -1;
                                }
                                if (flipY) {
                                    tPt3[1] = tPt3[1] * -1;
                                }
                                if (flipZ) {
                                    tPt3[2] = tPt3[2] * -1;
                                }

                                bw.write(tPt3[0] + " " + tPt3[1] + " " + tPt3[2] + ",");
                                bw.newLine();
                            }else {
                            	System.out.println("index is " + index);
                            	System.out.println(k + " is null");
                            	System.out.println();
                            }

                        }

                        bw.write("]");
                        bw.newLine();

                        while ( ! (line = raFile.readLine()).endsWith("]")) {

                        }
                        raFile.readLine();
                        index = index + 1;
                    } else {
                        bw.write(line);
                        bw.newLine();
                    }

                } else {
                    bw.write(line);
                    bw.newLine();

                }

            }
            //System.out.println("R7: " + r7_27Coord_transformed[0] + " " + r7_27Coord_transformed[1] + " " + r7_27Coord_transformed[2]);
            raFile.close();
            bw.close();
            fw.close();
        } catch (final Exception e) {
            e.printStackTrace();
            return false;
        }

        return success;
    }

    // /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Inner class that has the three coordinates for the point

    private class AddVals {
        private float[] xPt;

        
        private float[] yPt;

        private float[] zPt;

        private AddVals(final float[] xPt, final float[] yPt, final float[] zPt) {
            this.xPt = xPt;
            this.yPt = yPt;
            this.zPt = zPt;
        }

        public float[] getXPt() {
            return xPt;
        }


        public float[] getYPt() {
            return yPt;
        }


        public float[] getZPt() {
            return zPt;
        }


    }

}
