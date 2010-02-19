package gov.nih.mipav.model.algorithms;

import java.awt.Frame;
import java.io.IOException;

import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewUserInterface;

/** Swiss army knife for calculating the T1 relaxation time from any number of EFGRE (SPGR) images. 
 *  written: May 29, 2007 : S. Deoni
 *  status : in development
 * 
 **/

public class AlgorithmDespotT1 extends AlgorithmBase {

    private ModelImage t1ResultStack;
    private ModelImage moResultStack;
    private ModelImage r1ResultStack;
    private ModelImage b1ResultStack;

    private double despotTR = 5.00;
    private double irspgrTR = 5.00;
    private double irspgrKy = 96.00;
    private double irspgrFA = 5.00;
    private double maxT1 = 5000;
    private double maxMo = 10000;
    private double[] despotFA;
    private double[] irspgrTr;
    private double[] irspgrTI;
    
    private double[] spgrData;
    private double[] irspgrData;
    private double scale, pointScale, scaleIncrement;
    private double[] estimates, residuals;
    private int[] direction;
    
    private int[] spgrImageIndex;
    private int[] irspgrImageIndex;
    private int b1ImageIndex;
    private double angleIncrement;
    private int Nsa = 2;
    private int Nti = 1;
    private double maxAngle = 20;
    
    private boolean smoothB1Field = true;
    private boolean performStraightDESPOT1 = true;
    private boolean performDESPOT1withPreCalculatedB1Map = false;
    private boolean performDESPOT1HIFI = false;
    private boolean doubleInversion = true;
    private boolean singleInversion = false;
    private boolean geScanner = true;
    private boolean siemensScanner = false;
    private boolean threeTField = true;
    private boolean onefiveTField = false;
    
    private boolean calculateT1 = true;
    private boolean showB1Map = true;
    private boolean calculateMo = false;
    private boolean invertT1toR1 = false;
    
    private boolean useWeights = true;
    
    private boolean uniformAngleSpacing = true;
    
    private boolean upperLeftCorner = true;
    private boolean upperRightCorner = false;
    private boolean lowerLeftCorner = false;
    private boolean lowerRightCorner = false;
    
    private boolean useSmartThresholding = true;
    private boolean useHardThresholding = false;
    private float noiseScale = (float) 4.00;
    private float hardNoiseThreshold = (float) 0.00;
    
    private String[] wList;
    private String[] titles;
    private boolean completed;
    
    /** The frames for result images (if null at end of algorithm src ModelImage is destroyed) */
    private ViewJFrameImage t1ResultWindow = null;
    private ViewJFrameImage moResultWindow = null;
    private ViewJFrameImage r1ResultWindow = null;
    private ViewJFrameImage b1ResultWindow = null;
    
    public AlgorithmDespotT1(double despotTR, double irspgrTR,
            double irspgrKy, double irspgrFA, double maxT1, double maxMo,
            double[] despotFA, double[] irspgrTr2, double[] irspgrTI,
            double[] spgrData, double[] irspgrData, double scale,
            double pointScale, double scaleIncrement, double[] estimates,
            double[] residuals, int[] direction, int[] spgrImageIndex,
            int[] irspgrImageIndex, int b1ImageIndex, double angleIncrement,
            int nsa, int nti, double maxAngle, boolean smoothB1Field,
            boolean performStraightDESPOT1,
            boolean performDESPOT1withPreCalculatedB1Map,
            boolean performDESPOT1HIFI, boolean doubleInversion,
            boolean singleInversion, boolean geScanner, boolean siemensScanner,
            boolean threeTField, boolean onefiveTField, boolean calculateT1,
            boolean showB1Map, boolean calculateMo, boolean invertT1toR1,
            boolean useWeights, boolean uniformAngleSpacing,
            boolean upperLeftCorner, boolean upperRightCorner,
            boolean lowerLeftCorner, boolean lowerRightCorner,
            boolean useSmartThresholding, boolean useHardThresholding,
            float noiseScale, float hardNoiseThreshold, String[] wList,
            String[] titles) {
        super();
        this.despotTR = despotTR;
        this.irspgrTR = irspgrTR;
        this.irspgrKy = irspgrKy;
        this.irspgrFA = irspgrFA;
        this.maxT1 = maxT1;
        this.maxMo = maxMo;
        this.despotFA = despotFA;
        irspgrTr = irspgrTr2;
        this.irspgrTI = irspgrTI;
        this.spgrData = spgrData;
        this.irspgrData = irspgrData;
        this.scale = scale;
        this.pointScale = pointScale;
        this.scaleIncrement = scaleIncrement;
        this.estimates = estimates;
        this.residuals = residuals;
        this.direction = direction;
        this.spgrImageIndex = spgrImageIndex;
        this.irspgrImageIndex = irspgrImageIndex;
        this.b1ImageIndex = b1ImageIndex;
        this.angleIncrement = angleIncrement;
        Nsa = nsa;
        Nti = nti;
        this.maxAngle = maxAngle;
        this.smoothB1Field = smoothB1Field;
        this.performStraightDESPOT1 = performStraightDESPOT1;
        this.performDESPOT1withPreCalculatedB1Map = performDESPOT1withPreCalculatedB1Map;
        this.performDESPOT1HIFI = performDESPOT1HIFI;
        this.doubleInversion = doubleInversion;
        this.singleInversion = singleInversion;
        this.geScanner = geScanner;
        this.siemensScanner = siemensScanner;
        this.threeTField = threeTField;
        this.onefiveTField = onefiveTField;
        this.calculateT1 = calculateT1;
        this.showB1Map = showB1Map;
        this.calculateMo = calculateMo;
        this.invertT1toR1 = invertT1toR1;
        this.useWeights = useWeights;
        this.uniformAngleSpacing = uniformAngleSpacing;
        this.upperLeftCorner = upperLeftCorner;
        this.upperRightCorner = upperRightCorner;
        this.lowerLeftCorner = lowerLeftCorner;
        this.lowerRightCorner = lowerRightCorner;
        this.useSmartThresholding = useSmartThresholding;
        this.useHardThresholding = useHardThresholding;
        this.noiseScale = noiseScale;
        this.hardNoiseThreshold = hardNoiseThreshold;
        this.wList = wList;
        this.titles = titles;
    }

    public AlgorithmDespotT1(ModelImage destImage, ModelImage srcImage,
            double despotTR, double irspgrTR, double irspgrKy, double irspgrFA,
            double maxT1, double maxMo, double[] despotFA, double[] irspgrTr2,
            double[] irspgrTI, double[] spgrData, double[] irspgrData,
            double scale, double pointScale, double scaleIncrement,
            double[] estimates, double[] residuals, int[] direction,
            int[] spgrImageIndex, int[] irspgrImageIndex, int b1ImageIndex,
            double angleIncrement, int nsa, int nti, double maxAngle,
            boolean smoothB1Field, boolean performStraightDESPOT1,
            boolean performDESPOT1withPreCalculatedB1Map,
            boolean performDESPOT1HIFI, boolean doubleInversion,
            boolean singleInversion, boolean geScanner, boolean siemensScanner,
            boolean threeTField, boolean onefiveTField, boolean calculateT1,
            boolean showB1Map, boolean calculateMo, boolean invertT1toR1,
            boolean useWeights, boolean uniformAngleSpacing,
            boolean upperLeftCorner, boolean upperRightCorner,
            boolean lowerLeftCorner, boolean lowerRightCorner,
            boolean useSmartThresholding, boolean useHardThresholding,
            float noiseScale, float hardNoiseThreshold, String[] wList,
            String[] titles) {
        super(destImage, srcImage);
        this.despotTR = despotTR;
        this.irspgrTR = irspgrTR;
        this.irspgrKy = irspgrKy;
        this.irspgrFA = irspgrFA;
        this.maxT1 = maxT1;
        this.maxMo = maxMo;
        this.despotFA = despotFA;
        irspgrTr = irspgrTr2;
        this.irspgrTI = irspgrTI;
        this.spgrData = spgrData;
        this.irspgrData = irspgrData;
        this.scale = scale;
        this.pointScale = pointScale;
        this.scaleIncrement = scaleIncrement;
        this.estimates = estimates;
        this.residuals = residuals;
        this.direction = direction;
        this.spgrImageIndex = spgrImageIndex;
        this.irspgrImageIndex = irspgrImageIndex;
        this.b1ImageIndex = b1ImageIndex;
        this.angleIncrement = angleIncrement;
        Nsa = nsa;
        Nti = nti;
        this.maxAngle = maxAngle;
        this.smoothB1Field = smoothB1Field;
        this.performStraightDESPOT1 = performStraightDESPOT1;
        this.performDESPOT1withPreCalculatedB1Map = performDESPOT1withPreCalculatedB1Map;
        this.performDESPOT1HIFI = performDESPOT1HIFI;
        this.doubleInversion = doubleInversion;
        this.singleInversion = singleInversion;
        this.geScanner = geScanner;
        this.siemensScanner = siemensScanner;
        this.threeTField = threeTField;
        this.onefiveTField = onefiveTField;
        this.calculateT1 = calculateT1;
        this.showB1Map = showB1Map;
        this.calculateMo = calculateMo;
        this.invertT1toR1 = invertT1toR1;
        this.useWeights = useWeights;
        this.uniformAngleSpacing = uniformAngleSpacing;
        this.upperLeftCorner = upperLeftCorner;
        this.upperRightCorner = upperRightCorner;
        this.lowerLeftCorner = lowerLeftCorner;
        this.lowerRightCorner = lowerRightCorner;
        this.useSmartThresholding = useSmartThresholding;
        this.useHardThresholding = useHardThresholding;
        this.noiseScale = noiseScale;
        this.hardNoiseThreshold = hardNoiseThreshold;
        this.wList = wList;
        this.titles = titles;
    }

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        if(t1ResultWindow == null && t1ResultStack != null) {
            t1ResultStack.disposeLocal();
        }
        
        if(moResultWindow == null && moResultStack != null) {
            moResultStack.disposeLocal();
        }

        if(r1ResultWindow == null && r1ResultStack != null) {
            r1ResultStack.disposeLocal();
        }

        if(b1ResultWindow == null && b1ResultStack != null) {
            b1ResultStack.disposeLocal();
        }
    }
    
    public void runAlgorithm() {
        if (performDESPOT1HIFI) {
            completed = calculateT1UsingDESPOT1HIFI();
        }
        else {
            completed = calculateT1UsingConventionalDESPOT1();
        }
        
        setCompleted(completed);
    }

    /**
     * This method gives <code>clo</code> most of the image attributes of <code>orig</code> besides
     * data type to minimize rounding errors.
     * 
     * @param orig Original image
     * @param clo Near clone image
     */
    public ModelImage nearCloneImage(ModelImage orig, ModelImage clo) {
        FileInfoBase[] oArr = orig.getFileInfo();
        FileInfoBase[] cArr = clo.getFileInfo();
        if(cArr.length != oArr.length) {
            MipavUtil.displayError("Images are not same length");
            return clo;
        }
        for(int i=0; i<cArr.length; i++) {
            if(cArr == null) {
                return clo;
            }
            cArr[i].setOffset(oArr[i].getOffset());
            cArr[i].setEndianess(oArr[i].getEndianess());
            cArr[i].setResolutions(oArr[i].getResolutions().clone());
            cArr[i].setUnitsOfMeasure(oArr[i].getUnitsOfMeasure().clone());
            cArr[i].setOrigin(oArr[i].getOrigin().clone());
            cArr[i].setImageOrientation(cArr[i].getImageOrientation());
            cArr[i].setAxisOrientation(oArr[i].getAxisOrientation().clone());
            cArr[i].setDataType(ModelImage.DOUBLE);
        }
        
        return clo;
    }
    
    public double signalResiduals(double x, double[] spgrData, double[] irspgrData, double Inversion, int Nfa, int Nti, double[] despotFA, double despotTR, double[] irspgrTr, double[] irspgrTI, double irspgrFA) {
        
        double sumX, sumY, sumXY, sumXX, slope, intercept, residuals;
        double t1Guess, moGuess;
        double[] despotGuess, irspgrGuess, guess;
        
        double MoScale;
        
        int p, pulse, i,j;
        
        despotGuess = new double[Nfa];
        irspgrGuess = new double[Nti];
        
        sumX = 0.00;
        sumY = 0.00;
        sumXY = 0.00;
        sumXX = 0.00;
        for (p=0; p<Nfa; p++) {
            sumX += spgrData[p]/Math.tan(x*despotFA[p]*3.14159265/180.00);
            sumY += spgrData[p]/Math.sin(x*despotFA[p]*3.14159265/180.00);
            sumXY += spgrData[p]/Math.tan(x*despotFA[p]*3.14159265/180.00) * spgrData[p]/Math.sin(x*despotFA[p]*3.14159265/180.00);
            sumXX += spgrData[p]/Math.tan(x*despotFA[p]*3.14159265/180.00) * spgrData[p]/Math.tan(x*despotFA[p]*3.14159265/180.00);
        }
        slope = (Nfa*sumXY - sumX*sumY) / (Nfa*sumXX - sumX*sumX);
        intercept = (sumY - slope*sumX)/Nfa;
        
        if (slope > 0.00 && slope < 1.00) {
            t1Guess = -despotTR/Math.log(slope);
            moGuess = intercept/(1.00-slope);
        }
        else {
            t1Guess = 0.00;
            moGuess = 0.00;
        }
        
        if (t1Guess > 0) {
            for (p=0; p<Nfa; p++) {
                despotGuess[p] = moGuess*(1.00-Math.exp(-despotTR/t1Guess))*Math.sin(x*despotFA[p]*3.14159265/180.00)/(1.00-Math.exp(-despotTR/t1Guess)*Math.cos(x*despotFA[p]*3.14159265/180.00));
            }
        }
        else {
            for (p=0; p<Nfa; p++) {
                despotGuess[p] = 0.00;
            }
        }
        
        if (t1Guess > 0) {
            
            MoScale = 0.975;
            
            for (p=0; p<Nti; p++) {
                irspgrGuess[p] = Math.abs( MoScale*moGuess*Math.sin(x*irspgrFA*3.14159265/180.00) * (1.00-Inversion*Math.exp(-irspgrTI[p]/t1Guess) + Math.exp(-irspgrTr[p]/t1Guess)) );
            }
        }
        else {
            for (p=0; p<Nti; p++) {
                irspgrGuess[p] = 0.00;
            }
        }
        
        residuals = 0.00;
        for (p=0; p<Nfa; p++) residuals += Math.pow( (spgrData[p]-despotGuess[p]), 2.00);
        for (p=0; p<Nti; p++) residuals += Math.pow( (irspgrData[p]-irspgrGuess[p]), 2.00);
        
        return residuals;
    }

    public boolean calculateT1UsingDESPOT1HIFI() {
        fireProgressStateChanged("Prepping data..hang on");
        fireProgressStateChanged(5);
        
        ModelImage image, irspgrImage;
        float[] ctable;
        double Inversion, ax, bx, cx, fax, fbx, fcx, R, C, precision, x0, x1, x2, x3, f1, f2, xmin;
        double smoothedB1;
        
        Inversion = 1+Math.cos(0.98*3.14159265/180.00);
        R = 0.61803399;
        C = 1.00-R;
        precision = 0.003;
        
        double[] fa;
        double[] scaledFA;
        double[][] spgrPixelValues;
        double[][] irspgrPixelValues;
        float[][] t1Values, moValues, r1Values, b1field;
        double[][][] b1Values;
        double[][] Gaussian;
        double[][][] smoothedB1Values;
        double sumX, sumY, sumXY, sumXX, slope, intercept, lnslope, t1, e1, mo, r1, d, a, b, b1;
        double ernstAngle, ernstSignal, collectedSignal, weight, sumWeights;
        float noiseSum, threshold;
        
        int width, height, nSlices, tSeries, irspgrSlices, irspgrSeries;
        int irwidth, irheight;
        int x,y,i,j,k,t,angle, p, ti, p1, p2, pixelIndex, noiseIndex;
        int calculateB1 = 1;
        
        image = ViewUserInterface.getReference().getRegisteredImageByName(wList[spgrImageIndex[0]]);
        width = image.getExtents()[0];
        height = image.getExtents()[1];
        if(image.getNDims() > 2) {
            nSlices = image.getExtents()[2];
        } else {
            nSlices = 1;
        }
        
        boolean do4D = false;
        int dimT = 0;
        for (angle=0; angle<Nsa; angle++) {
            image = ViewUserInterface.getReference().getRegisteredImageByName(wList[spgrImageIndex[angle]]);
            if(image.getNDims() > 3) {
            	do4D = true;
                dimT = image.getExtents()[3];
            }
        }
        
        if(do4D) {
            tSeries = dimT;
        } else {
            tSeries = 1;
        }
        
        
        irspgrImage = ViewUserInterface.getReference().getRegisteredImageByName(wList[irspgrImageIndex[0]]);
        if(image.getNDims() > 2) {
            irspgrSlices = irspgrImage.getExtents()[2];
        } else {
            irspgrSlices = 1;
        }
        
        do4D = false;
        dimT = 0;
        for (ti=0; ti<Nti; ti++) {
            image = ViewUserInterface.getReference().getRegisteredImageByName(wList[irspgrImageIndex[ti]]);  
            if(image.getNDims() > 3) {
                do4D = true;
                dimT = image.getExtents()[3];
            }
        }
        
        if(do4D) {
            irspgrSeries = dimT;
        } else {
            irspgrSeries = 1;
        }
        
        // check to make sure the IR-SPGR and SPGR data have the same size
        irwidth = irspgrImage.getExtents()[0];
        irheight = irspgrImage.getExtents()[1];
        
        if (irwidth != width || irheight != height || irspgrSlices != nSlices) {
            MipavUtil.displayError("IR-SPGR and SPGR data must have the same image dimensions.");
            return false;
        }
        
        Gaussian = new double[5][5];
        
        // define the Gaussian kernel
        Gaussian[0][0] = 0;
        Gaussian[0][1] = 0;
        Gaussian[0][2] = 1;
        Gaussian[0][3] = 0;
        Gaussian[0][4] = 0;
        Gaussian[1][0] = 0;
        Gaussian[1][1] = 2;
        Gaussian[1][2] = 4;
        Gaussian[1][3] = 2;
        Gaussian[1][4] = 0;
        Gaussian[2][0] = 1;
        Gaussian[2][1] = 4;
        Gaussian[2][2] = 6;
        Gaussian[2][3] = 4;
        Gaussian[2][4] = 1;
        Gaussian[3][0] = 0;
        Gaussian[3][1] = 2;
        Gaussian[3][2] = 4;
        Gaussian[3][3] = 2;
        Gaussian[3][4] = 0;
        Gaussian[0][0] = 0;
        Gaussian[4][1] = 0;
        Gaussian[4][2] = 1;
        Gaussian[4][3] = 0;
        Gaussian[4][4] = 0;
        
        t1ResultStack = new ModelImage(ModelImage.DOUBLE, image.getExtents(), "t1_results");
        moResultStack = new ModelImage(ModelImage.DOUBLE, image.getExtents(), "mo_results");
        r1ResultStack = new ModelImage(ModelImage.DOUBLE, image.getExtents(), "r1_results");
        b1ResultStack = new ModelImage(ModelImage.DOUBLE, image.getExtents(), "b1_results");
        
        t1ResultStack = nearCloneImage(image, t1ResultStack);
        moResultStack = nearCloneImage(image, moResultStack);
        r1ResultStack = nearCloneImage(image, r1ResultStack);
        b1ResultStack = nearCloneImage(image, b1ResultStack);
        
        // start by calculaing the B1 field
        String prefix = new String();
        for(t=0; t<irspgrSeries; t++) {
            if(image.getNDims() > 3) {
                prefix = "Series "+t+": ";
            } else {
                prefix = "";
            }
            
            spgrPixelValues = new double[Nsa][width*height];
            irspgrPixelValues = new double[Nsa][width*height];
            if (calculateT1) { 
                t1Values = new float[irspgrSlices][width*height];
            }
            else { 
                t1Values = new float[1][1];
            }
            if (calculateMo) { 
                moValues = new float[irspgrSlices][width*height];
            }
            else { 
                moValues = new float[1][1];
            }
            if (invertT1toR1) { 
                r1Values = new float[irspgrSlices][width*height];
            }
            else { 
                r1Values = new float[1][1];
            }
            
            b1field = new float[irspgrSlices][width*height];
            b1Values = new double[irspgrSlices][height][width];
            
            fa = new double[Nsa];
            scaledFA = new double[Nsa];
            for (angle=0; angle<Nsa;angle++) {
                fa[angle] = Math.toRadians(despotFA[angle]);
            }
            double irFA = Math.toRadians(irspgrFA);
            
            for (k=0; k<irspgrSlices; k++) {
            
                noiseSum = (float) 0.00;
                image = ViewUserInterface.getReference().getRegisteredImageByName(wList[spgrImageIndex[0]]);
                
                threshold = hardNoiseThreshold;
                if (useSmartThresholding) {
                    noiseSum = (float) 0.00;
                    noiseIndex = 0;
                    if (upperLeftCorner) {
                        for (y=20; y<30; y++) {
                            for (x=20; x<30; x++) {
                                noiseSum += image.getFloat(x, y, k);
                                noiseIndex++;
                            }
                        }
                    }
                    if (upperRightCorner) {
                        for (y=20; y<30; y++) {
                            for (x=width-30; x<width-20; x++) {
                                noiseSum += image.getFloat(x, y, k);
                                noiseIndex++;
                            }
                        }
                    }
                    if (lowerLeftCorner) {
                        for (y=height-30; y<height-20; y++) {
                            for (x=20; x<30; x++) {
                                noiseSum += image.getFloat(x, y, k);
                                noiseIndex++;
                            }
                        }
                    }
                    if (lowerRightCorner) {
                        for (y=height-30; y<height-20; y++) {
                            for (x=width-30; x<width-20; x++) {
                                noiseSum += image.getFloat(x, y, k);
                                noiseIndex++;
                            }
                        }
                    }
                    
                    threshold = (float) ( (noiseSum/noiseIndex + 1)*noiseScale );
                }
                else {
                    threshold = (float) hardNoiseThreshold;
                }
                fireProgressStateChanged(prefix+"calculating B1 field for slice: "+k+" of "+(nSlices-1));
                fireProgressStateChanged(5+(int)(((float)k+1.0)/(float)nSlices*60.0));
                if(interrupted()) {
                    return false;
                }
                // grab the ir-spgr pixel values
                for (ti=0; ti<Nti; ti++) {
                    image = ViewUserInterface.getReference().getRegisteredImageByName(wList[irspgrImageIndex[ti]]);             
                    pixelIndex = 0;
                    for (y=0; y<height; y++) {
                        for (x=0; x<width; x++) {
                            if(image.getNDims() < 4) {
                                irspgrPixelValues[ti][pixelIndex] = image.getFloat(x, y, k);
                            } else {
                                irspgrPixelValues[ti][pixelIndex] = image.getFloat(x, y, k, t);
                            }
                            pixelIndex++;
                        }
                    }
                }
                // grab the spgr pixel values 
                for (angle=0; angle<Nsa; angle++) {
                    image = ViewUserInterface.getReference().getRegisteredImageByName(wList[spgrImageIndex[angle]]);
                    pixelIndex = 0;
                    for (y=0; y<height; y++) {
                        for (x=0; x<width; x++) {
                            if(image.getNDims() < 4) {
                                spgrPixelValues[angle][pixelIndex] = image.getFloat(x, y, k);
                            } else {
                                spgrPixelValues[angle][pixelIndex] = image.getFloat(x, y, k, t);
                            }
                            pixelIndex++;
                        }
                    }
                }
                pixelIndex = 0;
                for (y=0; y<height; y++) {
                    for (x=0; x<width; x++) {
                        if (spgrPixelValues[0][pixelIndex] > (threshold)) {
                            
                            // group the spgr and ir-spagr data
                            for (p=0; p<Nsa; p++) spgrData[p] = spgrPixelValues[p][pixelIndex];
                            for (p=0; p<Nti; p++) irspgrData[p] = irspgrPixelValues[p][pixelIndex];
                            
                            // define the initial fitting points
                            ax = 0.3; // lower bound
                            cx = 1.5; // upper bound
                            fax = signalResiduals(ax, spgrData, irspgrData, Inversion, Nsa, Nti, despotFA, despotTR, irspgrTr, irspgrTI, irspgrFA);
                            fcx = signalResiduals(cx, spgrData, irspgrData, Inversion, Nsa, Nti, despotFA, despotTR, irspgrTr, irspgrTI, irspgrFA);
                            
                            // choose bx such that ax < bx < cx and f(bx) < f(ax) and f(bx) < f(cx)
                            if (fax < fcx) bx = ax + 0.2;
                            else bx = cx - 0.2;
                            fbx = signalResiduals(bx, spgrData, irspgrData, Inversion, Nsa, Nti, despotFA, despotTR, irspgrTr, irspgrTI, irspgrFA);
                            
                            x0 = ax;
                            x3 = cx;
                            if ( Math.abs(cx-bx) > Math.abs(bx-ax) ) {
                                x1 = bx;
                                x2 = bx + C*(cx-bx);
                            }
                            else {
                                x2 = bx;
                                x1 = bx - C*(bx-ax);
                            }
                            
                            f1 = signalResiduals(x1, spgrData, irspgrData, Inversion, Nsa, Nti, despotFA, despotTR, irspgrTr, irspgrTI, irspgrFA);
                            f2 = signalResiduals(x2, spgrData, irspgrData, Inversion, Nsa, Nti, despotFA, despotTR, irspgrTr, irspgrTI, irspgrFA);
                            
                            while ( Math.abs(x3-x0) > precision*(Math.abs(x1)+Math.abs(x2)) ) {
                                if (f2 < f1) {
                                    x0 = x1;
                                    x1 = x2;
                                    x2 = R*x1 + C*x3;
                                    f1 = f2;
                                    f2 = signalResiduals(x2, spgrData, irspgrData, Inversion, Nsa, Nti, despotFA, despotTR, irspgrTr, irspgrTI, irspgrFA);
                                }
                                else {
                                    x3 = x2;
                                    x2 = x1;
                                    x1 = R*x2 + C*x0;
                                    f2 = f1;
                                    f1 = signalResiduals(x1, spgrData, irspgrData, Inversion, Nsa, Nti, despotFA, despotTR, irspgrTr, irspgrTI, irspgrFA);
                                }
                            }
                            
                            if (f1 < f2) xmin = x1;
                            else xmin = x2;
                            
                            b1Values[k][y][x] = xmin;
                        }
                        pixelIndex ++;
                    }
                }
            }
        
            // go back through and apply a single gaussian kernel to smooth the calculated B1 field
            if (smoothB1Field) {
                int baseVal = getProgressChangeListener().getValue();
                for (k=0; k<irspgrSlices; k++) {
                    fireProgressStateChanged(prefix+"smoothing B1 field on slice: "+k+" of "+(nSlices-1));
                    fireProgressStateChanged(baseVal+(int)(((float)k+1.0)/(float)nSlices*10.0));
                    pixelIndex = 0;
                    for (y=0; y<height; y++) {
                        for (x=0; x<width; x++) {
                            
                            if (y>2 && y<height-2 && x>2 && x<width-2) {
                                
                                smoothedB1 = 0.00;
                                for (p1=0; p1<5; p1++) {
                                    for (p2=0; p2<5; p2++) smoothedB1 += b1Values[k][y-2+p2][x-2+p1]*Gaussian[p1][p2];
                                }
                                smoothedB1 = smoothedB1 / 34.00;
                                
                                b1field[k][pixelIndex] = (float) smoothedB1;
                            }
                            else b1field[k][pixelIndex] = (float) b1Values[k][y][x];
                            
                            pixelIndex ++;
                        }
                    }
                }
            }
            else { // no smoothing option
                for (k=0; k<irspgrSlices; k++) {
                    pixelIndex = 0;
                    for (y=0; y<height; y++) {
                        for (x=0; x<width; x++) {
                            b1field[k][pixelIndex] = (float) b1Values[k][y][x];
                            pixelIndex ++;
                        }
                    }
                }
                
            }
        
            if(interrupted()) {
                return false;
            }
        
            // clear the b1Values matrix
            
            // finally, calculate the corrected T1 estimates, changed slice start 
            int baseVal = getProgressChangeListener().getValue();
            for (k=0; k<irspgrSlices; k++) {
                
                // first, recalculate the noise threshold
                noiseSum = (float) 0.00;
                image = ViewUserInterface.getReference().getRegisteredImageByName(wList[spgrImageIndex[0]]);
                
                threshold = hardNoiseThreshold;
                if (useSmartThresholding) {
                    noiseSum = (float) 0.00;
                    noiseIndex = 0;
                    if (upperLeftCorner) {
                        for (y=20; y<30; y++) {
                            for (x=20; x<30; x++) {
                                noiseSum += image.getFloat(x, y, k);
                                noiseIndex++;
                            }
                        }
                    }
                    if (upperRightCorner) {
                        for (y=20; y<30; y++) {
                            for (x=width-30; x<width-20; x++) {
                                noiseSum += image.getFloat(x, y, k);
                                noiseIndex++;
                            }
                        }
                    }
                    if (lowerLeftCorner) {
                        for (y=height-30; y<height-20; y++) {
                            for (x=20; x<30; x++) {
                                noiseSum += image.getFloat(x, y, k);
                                noiseIndex++;
                            }
                        }
                    }
                    if (lowerRightCorner) {
                        for (y=height-30; y<height-20; y++) {
                            for (x=width-30; x<width-20; x++) {
                                noiseSum += image.getFloat(x, y, k);
                                noiseIndex++;
                            }
                        }
                    }
                    
                    threshold = (float) ( (noiseSum/noiseIndex + 1)*noiseScale );
                }
                else {
                    threshold = (float) hardNoiseThreshold;
                }
            
            
                fireProgressStateChanged(prefix+"calculating T1 values on slice: "+k+" of "+(nSlices-1));
                fireProgressStateChanged(baseVal+(int)(((float)k+1.0)/(float)nSlices*20.0));
                if(interrupted()) {
                    return false;
                }
                // grab the spgr pixel values 
                for (angle=0; angle<Nsa; angle++) {
                    image = ViewUserInterface.getReference().getRegisteredImageByName(wList[spgrImageIndex[angle]]);
                    
                    pixelIndex = 0;
                    for (y=0; y<height; y++) {
                        for (x=0; x<width; x++) {
                            if(image.getNDims() < 4) {
                                spgrPixelValues[angle][pixelIndex] = image.getFloat(x, y, k);
                            } else {
                                spgrPixelValues[angle][pixelIndex] = image.getFloat(x, y, k, t);
                            }
                            pixelIndex++;
                        }
                    }
                }
                
                pixelIndex = 0;
                //for (x=0; x<width; x++) {
                //  for (y=0; y<height; y++) {
                for (y=0; y<height; y++) {
                    for (x=0; x<width; x++) {
                        
                    
                        if (b1field[k][pixelIndex] > 0.00 && spgrPixelValues[0][pixelIndex] > threshold) {
                            
                            // scale up (or down) the flip angles based on the calculated B1
                            for (p=0; p<Nsa; p++) scaledFA[p] = fa[p]*b1field[k][pixelIndex];
                            
                            // grab the SPGR values for this pixel
                            for (p=0; p<Nsa; p++) spgrData[p] = spgrPixelValues[p][pixelIndex];
                            
                            // calculate T1
                            sumX = 0.00;
                            sumY = 0.00;
                            sumXY = 0.00;
                            sumXX = 0.00;
                        
                            for (p=0; p<Nsa; p++) {
                                sumX += spgrData[p]/Math.tan(scaledFA[p]);
                                sumY += spgrData[p]/Math.sin(scaledFA[p]);
                                sumXY += (spgrData[p]/Math.tan(scaledFA[p]))*(spgrData[p]/Math.sin(scaledFA[p]));
                                sumXX += (spgrData[p]/Math.tan(scaledFA[p]))*(spgrData[p]/Math.tan(scaledFA[p]));
                            }
                            
                            d = (Nsa*sumXX) - sumX*sumX;
                            a = (Nsa*sumXY) - (sumX*sumY);
                            
                            if (d != 0) {
                                slope = a/d;
                                intercept = (sumY-slope*sumX)/Nsa;
                                lnslope = -1.00*Math.log(slope);
                                if (lnslope > 0.00 && lnslope < 1.00) {
                                    t1 = despotTR/lnslope;
                                    mo = intercept/(1.00-Math.exp(-despotTR/t1));
                                }
                                else {
                                    mo = maxMo;
                                    t1 = maxT1;
                                }
                            }
                            else {
                                mo = maxMo;
                                t1 = maxT1;
                            }
                        
    
                            if (t1 < 0.00 || t1 > maxT1) {
                                t1 = maxT1;
                            }
                            if (mo < 0.00 || mo > maxMo) {
                                mo = maxMo;
                            }
                            
                        
                            if (t1 != 0.00) {
                                r1 = 1.00/t1;
                            }
                            else {
                                r1 = 0.00;
                            }
                        
                            // if they wish to use weights, re-calculate using our non-linear weighting scheme
                            if (useWeights || Nsa > 3) {
                                sumX = 0.00;
                                sumY = 0.00;
                                sumXY = 0.00;
                                sumXX = 0.00;
                                sumWeights = 0.00;
                                if (t1 == 0) {
                                    t1 = 0;
                                    mo = 0;
                                    r1 = 0;
                                }
                                else {
                                    e1 = Math.exp(-1.00*despotTR/t1);
                                    ernstAngle = Math.acos(e1);
                                    ernstSignal = (1.00-e1)*Math.sin(ernstAngle)/(1.00-e1*Math.cos(ernstAngle));
                                    for (angle=0; angle<Nsa; angle++) {
                                        collectedSignal = (1.00-e1)*Math.sin(scaledFA[angle])/(1.00-e1*Math.cos(scaledFA[angle]));
                                        weight = collectedSignal/ernstSignal;
                                        sumX+=weight * spgrPixelValues[angle][pixelIndex]/Math.tan(scaledFA[angle]);
                                        sumY+=weight * spgrPixelValues[angle][pixelIndex]/Math.sin(scaledFA[angle]);
                                        sumXY+=weight * spgrPixelValues[angle][pixelIndex]/Math.tan(scaledFA[angle]) * spgrPixelValues[angle][pixelIndex]/Math.sin(scaledFA[angle]);
                                        sumXX+=weight* spgrPixelValues[angle][pixelIndex]/Math.tan(scaledFA[angle]) * spgrPixelValues[angle][pixelIndex]/Math.tan(scaledFA[angle]);
                                        sumWeights+=weight;
                                    }
                                    d = (sumWeights*sumXX) - sumX*sumX;
                                    a = (sumWeights*sumXY)-(sumX*sumY);
                                    
                                    if (d != 0) {
                                        slope = a/d;
                                        intercept = (sumY - slope*sumX)/sumWeights;
                                        lnslope = -1.00*Math.log(slope);
                                        if (lnslope > 0.00 && lnslope < 1.00) {
                                            t1 = despotTR/lnslope;
                                            mo = intercept/(1.00-Math.exp(-despotTR/t1));
                                        }
                                        else {
                                            mo = maxMo;
                                            t1 = maxT1;
                                        }
                                    }
                                    else {
                                        mo = maxMo;
                                        t1 = maxT1;
                                    }
                                    
                                    
                                    if (t1 < 0.00 || t1 > maxT1) {
                                        t1 = maxT1;
                                    } 
                                    if (mo < 0.00 || mo > maxMo) {
                                        mo = maxMo;
                                    }
                                    if (t1 != 0.00) {
                                        r1 = 1.00/t1;
                                    }
                                    else {
                                        r1 = 0.00;
                                    }
                                }
                            }
                            if (calculateT1) {
                                t1Values[k][pixelIndex] = (float) t1;
                            }
                            if (calculateMo) {
                                moValues[k][pixelIndex] = (float) mo;
                            }
                            if (invertT1toR1) {
                                r1Values[k][pixelIndex] = (float) r1;
                            }
                        }
                        else {
                            if (calculateT1) {
                                t1Values[k][pixelIndex] = 0;
                            }
                            if (calculateMo) {
                                moValues[k][pixelIndex] = 0;
                            }
                            if (invertT1toR1) {
                                r1Values[k][pixelIndex] = 0;
                            }
                        }
                        pixelIndex++;
                    }
                }
            
                //adds data to the end of an image
                try {
                    int startVal = image.getSliceSize()*nSlices*t + image.getSliceSize()*k;
                    if (calculateT1) {
                        t1ResultStack.importData(startVal, t1Values[k], true);
                    }
                    if (calculateMo) {
                        moResultStack.importData(startVal, moValues[k], true);
                    }
                    if (invertT1toR1) {
                        r1ResultStack.importData(startVal, r1Values[k], true);
                    }
                    if (showB1Map) {
                        b1ResultStack.importData(startVal, b1field[k], true);
                    }
                } catch(IOException e) {
                    e.printStackTrace();
                    MipavUtil.displayError("Data could not be imported into result image");
                }
            }
        }
        
        if (calculateT1) {
            t1ResultWindow = new ViewJFrameImage(t1ResultStack);
            t1ResultWindow.setTitle("DESPOT1-HIFI_T1_Map");
            t1ResultWindow.setVisible(true);
        } 
        
        if (calculateMo) {
            moResultWindow = new ViewJFrameImage(moResultStack);
            moResultWindow.setTitle("DESPOT1-HIFI_Mo_Map");
            moResultWindow.setVisible(true);
        } 
        
        if (invertT1toR1) {
            r1ResultWindow = new ViewJFrameImage(r1ResultStack);
            r1ResultWindow.setTitle("DESPOT1-HIFI_R1_Map");
            r1ResultWindow.setVisible(true);
        } 
        
        if (showB1Map) {
            b1ResultWindow = new ViewJFrameImage(b1ResultStack);
            b1ResultWindow.setTitle("DESPOT1-HIFI_B1_Map");
            b1ResultWindow.setVisible(true);
        } 
        
        return true;
    }

    public boolean calculateT1UsingConventionalDESPOT1() {
        ModelImage image, b1FieldImage = null;
        float[] ctable, b1ctable;
         
        double b1;
        double[] fa, b1Values;
        double[][] pixelValues;
        float[][] t1Values, moValues, r1Values;
        double sumX, sumY, sumXY, sumXX, slope, intercept, lnslope, t1, e1, mo, r1, d, a, b;
        double ernstAngle, ernstSignal, collectedSignal, weight, sumWeights;
        float noiseSum, threshold;
        
        int width, height, nSlices, tSeries;
        int x,y,i,j,k,t,angle, pixelIndex, noiseIndex;
        
        image = ViewUserInterface.getReference().getRegisteredImageByName(wList[spgrImageIndex[0]]);
        width = image.getExtents()[0];
        height = image.getExtents()[1];
        if(image.getNDims() > 2) {
            nSlices = image.getExtents()[2];
        } else {
            nSlices = 1;
        }
        
        boolean do4D = false;
        int dimT = 0;
        for (angle=0; angle<Nsa; angle++) {
            image = ViewUserInterface.getReference().getRegisteredImageByName(wList[spgrImageIndex[angle]]);
            if(image.getNDims() > 3) {
            	do4D = true;
                dimT = image.getExtents()[3];
            }
        }
        
        if(do4D) {
            tSeries = dimT;
        } else {
            tSeries = 1;
        }
        
        t1ResultStack = new ModelImage(ModelImage.DOUBLE, image.getExtents(), "t1_results");
        moResultStack = new ModelImage(ModelImage.DOUBLE, image.getExtents(), "mo_results");
        r1ResultStack = new ModelImage(ModelImage.DOUBLE, image.getExtents(), "r1_results");
        
        t1ResultStack = nearCloneImage(image, t1ResultStack);
        moResultStack = nearCloneImage(image, moResultStack);
        r1ResultStack = nearCloneImage(image, r1ResultStack);
        
        String prefix = new String();
        for(t=0; t<tSeries; t++) {
            if(image.getNDims() > 3) {
                prefix = "Series "+t+": ";
            } else {
                prefix = "";
            }
        
            pixelValues = new double[Nsa][width*height];
            t1Values = new float[nSlices][width*height];
            moValues = new float[nSlices][width*height];
            r1Values = new float[nSlices][width*height];
            
            fa = new double[Nsa];
            for (angle=0; angle<Nsa;angle++) {
                fa[angle] = Math.toRadians(despotFA[angle]);
            }
            
            for (k=0; k<nSlices; k++) { //changed slice size
                noiseSum = (float) 0.00;
                image = ViewUserInterface.getReference().getRegisteredImageByName(wList[spgrImageIndex[0]]);
                
                threshold = hardNoiseThreshold;
                if (useSmartThresholding) {
                    noiseSum = (float) 0.00;
                    noiseIndex = 0;
                    if (upperLeftCorner) {
                        for (y=20; y<30; y++) {
                            for (x=20; x<30; x++) {
                                noiseSum += image.getFloat(x, y, k);
                                noiseIndex++;
                            }
                        }
                    }
                    if (upperRightCorner) {
                        for (y=20; y<30; y++) {
                            for (x=width-30; x<width-20; x++) {
                                noiseSum += image.getFloat(x, y, k);
                                noiseIndex++;
                            }
                        }
                    }
                    if (lowerLeftCorner) {
                        for (y=height-30; y<height-20; y++) {
                            for (x=20; x<30; x++) {
                                noiseSum += image.getFloat(x, y, k);
                                noiseIndex++;
                            }
                        }
                    }
                    if (lowerRightCorner) {
                        for (y=height-30; y<height-20; y++) {
                            for (x=width-30; x<width-20; x++) {
                                noiseSum += image.getFloat(x, y, k);
                                noiseIndex++;
                            }
                        }
                    }
                
                    threshold = (float) ( (noiseSum/noiseIndex)*noiseScale );
                }
                else {
                    threshold = (float) hardNoiseThreshold;
                }
                fireProgressStateChanged(prefix+"working on slice: "+k+" of "+(nSlices-1)+". Noise Threshold = "+threshold);
                fireProgressStateChanged(5+(int)((float)k+1.0/(float)nSlices*80.0));
                
                if(interrupted()) {
                    return false;
                }
                
                if (performDESPOT1withPreCalculatedB1Map) {
                    b1FieldImage = ViewUserInterface.getReference().getRegisteredImageByName(wList[b1ImageIndex]);
                    
                    b1Values = new double[width*height]; 
                }
                else { // need to initialize something
                    image = ViewUserInterface.getReference().getRegisteredImageByName(wList[spgrImageIndex[0]]);
                    
                    b1Values = new double[1];
                }
            
                for (angle=0; angle<Nsa; angle++) {
                    image = ViewUserInterface.getReference().getRegisteredImageByName(wList[spgrImageIndex[angle]]);
                    
                    pixelIndex = 0;
                    for (y=0; y<height; y++) {
                        for (x=0; x<width; x++) {
                            if(image.getNDims() < 4) {
                                pixelValues[angle][pixelIndex] = image.getDouble(x, y, k);
                            } else {
                                pixelValues[angle][pixelIndex] = image.getDouble(x, y, k, t);
                            }
                            if (performDESPOT1withPreCalculatedB1Map) {
                                b1Values[pixelIndex] = b1FieldImage.getDouble(x, y, k, t); 
                            }
                            pixelIndex++;
                        }
                    }
                }
            
                for (pixelIndex=0; pixelIndex<width*height; pixelIndex++) {
                    if (pixelValues[0][pixelIndex] > (threshold)) {
                        sumX = 0.00;
                        sumY = 0.00;
                        sumXY = 0.00;
                        sumXX = 0.00;
                        
                        if (performDESPOT1withPreCalculatedB1Map) {
                            b1 = b1Values[pixelIndex];
                            if (b1 == 0) b1 = 1.00;
                        }
                        else b1 = 1.00;
                        
                        for (angle=0; angle<Nsa; angle++) {
                            sumX+=(pixelValues[angle][pixelIndex]/Math.tan(b1*fa[angle]));
                            sumY+=(pixelValues[angle][pixelIndex]/Math.sin(b1*fa[angle]));
                            sumXY+=(pixelValues[angle][pixelIndex]/Math.tan(b1*fa[angle])) * (pixelValues[angle][pixelIndex]/Math.sin(b1*fa[angle]));
                            sumXX+=(pixelValues[angle][pixelIndex]/Math.tan(b1*fa[angle])) * (pixelValues[angle][pixelIndex]/Math.tan(b1*fa[angle]));
                        }
                        
                        d = (Nsa*sumXX) - (sumX*sumX);
                        a = (Nsa*sumXY) - (sumX*sumY);
                        
                        if (d != 0) {
                            slope = a/d;
                            intercept = (sumY-slope*sumX)/Nsa;
                            lnslope = -1.00*Math.log(slope);
                            if (lnslope > 0.00 && lnslope < 1.00) {
                                t1 = despotTR/lnslope;
                                mo = intercept/(1.00-Math.exp(-despotTR/t1));
                            } 
                            else {
                                mo = maxMo;
                                t1 = maxT1;
                            }
                        }
                        else {
                            mo = maxMo;
                            t1 = maxT1;
                        }
                        
                        if (t1 < 0 || t1 > maxT1) {
                            t1 = maxT1;
                        }
                        if (mo < 0 || mo > maxMo) {
                            mo = maxMo;
                        }
                        if (t1 != 0) {
                            r1 = 1/t1;
                        }
                        else {
                            r1 = 0;
                        }
                    
                        if (useWeights) {
                            sumX = 0.00;
                            sumY = 0.00;
                            sumXY = 0.00;
                            sumXX = 0.00;
                            sumWeights = 0.00;
                            if (t1 == 0) {
                                t1 = 0;
                                mo = 0;
                                r1 = 0;
                            }
                            else {
                                e1 = Math.exp(-1.00*despotTR/t1);
                                ernstAngle = Math.acos(e1);
                                ernstSignal = (1.00-e1)*Math.sin(ernstAngle)/(1.00-e1*Math.cos(ernstAngle));
                                for (angle=0; angle<Nsa; angle++) {
                                    collectedSignal = (1.00-e1)*Math.sin(b1*fa[angle])/(1.00-e1*Math.cos(b1*fa[angle]));
                                    weight = collectedSignal/ernstSignal;
                                    sumX+=weight * pixelValues[angle][pixelIndex]/Math.tan(b1*fa[angle]);
                                    sumY+=weight * pixelValues[angle][pixelIndex]/Math.sin(b1*fa[angle]);
                                    sumXY+=weight * pixelValues[angle][pixelIndex]/Math.tan(b1*fa[angle]) * pixelValues[angle][pixelIndex]/Math.sin(b1*fa[angle]);
                                    sumXX+=weight* pixelValues[angle][pixelIndex]/Math.tan(b1*fa[angle]) * pixelValues[angle][pixelIndex]/Math.tan(b1*fa[angle]);
                                    sumWeights+=weight;
                                }
                                d = (sumWeights*sumXX) - (sumX*sumX);
                                a = (sumWeights*sumXY)-(sumX*sumY);
                                
                                if (d != 0) {
                                    slope = a/d;
                                    intercept = (sumY - slope*sumX)/sumWeights;
                                    lnslope = -1.00*Math.log(slope);
                                    if (lnslope > 0.00 && lnslope < 1.00) {
                                        t1 = despotTR/lnslope;
                                        mo = intercept/(1.00-Math.exp(-despotTR/t1));
                                    }
                                    else {
                                        mo = maxMo;
                                        t1 = maxT1;
                                    }
                                }
                                else {
                                    mo = maxMo;
                                    t1 = maxT1;
                                }
                            
                                if (t1 < 0 || t1 > maxT1) {
                                    t1 = maxT1;
                                } 
                                if (mo < 0 || mo > maxMo) {
                                    mo = maxMo;
                                }
                                if (t1 != 0) {
                                    r1 = 1/t1;
                                }
                                else {
                                    r1 = 0;
                                }
                            }
                        }
                        
                        t1Values[k][pixelIndex] = (float) t1;
                        moValues[k][pixelIndex] = (float) mo;
                        r1Values[k][pixelIndex] = (float) r1;
                    }
                    else {
                        t1Values[k][pixelIndex] = 0;
                        moValues[k][pixelIndex] = 0;
                        r1Values[k][pixelIndex] = 0;
                    }
                }
                try {
                    int startVal = image.getSliceSize()*nSlices*t + image.getSliceSize()*k;
                    t1ResultStack.importData(startVal, t1Values[k], true);
                    moResultStack.importData(startVal, moValues[k], true);
                    r1ResultStack.importData(startVal, r1Values[k], true);
                } catch (IOException e) {
                    e.printStackTrace();
                    MipavUtil.displayError("Could not import result image data.");
                }
            } 
        }
        
        if (calculateT1) {
            t1ResultWindow = new ViewJFrameImage(t1ResultStack);
            t1ResultWindow.setTitle("DESPOT1_T1_Map");
            t1ResultWindow.setVisible(true);
        } 
         
        if (calculateMo) {
            moResultWindow = new ViewJFrameImage(moResultStack);
            moResultWindow.setTitle("DESPOT1_Mo_Map");
            moResultWindow.setVisible(true);
        } 
        
        if (invertT1toR1) {
            r1ResultWindow = new ViewJFrameImage(r1ResultStack);
            r1ResultWindow.setTitle("DESPOT1_R1_Map");
            r1ResultWindow.setVisible(true);
        } 
        
        return true;
    }

	public ModelImage getT1ResultStack() {
		return t1ResultStack;
	}

	public ModelImage getMoResultStack() {
		return moResultStack;
	}

	public ModelImage getR1ResultStack() {
		return r1ResultStack;
	}

	public ModelImage getB1ResultStack() {
		return b1ResultStack;
	}
}
