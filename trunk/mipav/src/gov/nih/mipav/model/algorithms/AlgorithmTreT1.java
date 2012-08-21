package gov.nih.mipav.model.algorithms;

import java.io.File;
import java.io.IOException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewJFrameMessage;
import gov.nih.mipav.view.ViewUserInterface;

/** 
 * This class is adapted from the ImageJ version originally written by Sean Deoni while working at the
 * National Institutes of Health. This class performs T1 calculations given any number of
 * SPGR and IRSPGR images.
 * 
 **/

public class AlgorithmTreT1 extends AlgorithmTProcess {

    private ModelImage t1ResultStack = null;
    private ModelImage m0ResultStack = null;
    private ModelImage r1ResultStack = null;
    private ModelImage b1ResultStack = null;
    
    private double[][][] t1ResultData = null;
    private double[][][] m0ResultData = null;
    private double[][][] r1ResultData = null;
    private double[][][] b1ResultData = null;
    
    private ModelImage largestImage = null;
    
    private double treTR = 5.00;
    private double irspgrTR = 5.00;
    private double irspgrKy = 96.00;
    private double irspgrFA = 5.00;
    private double maxT1 = 5000;
    private double maxM0 = 10000;
    private double[] treFA;
    private double[] irspgrTr;
    private double[] irspgrTI;
    
    private double[] spgrData;
    private double[] irspgrData;
    private double scale;
    private double pointScale;
    private double scaleIncrement;
    private double[] estimates;
    private double[] residuals;
    private int[] direction;
    
    private int[] spgrImageIndex;
    private int[] irspgrImageIndex;
    private int b1ImageIndex;
    private double angleIncrement;
    private int Nsa = 2;
    private int Nti = 1;
    private double maxAngle = 20;
    
    private boolean smoothB1Field = true;
    private boolean performStraightTreT1 = true;
    private boolean performTreT1withPreCalculatedB1Map = false;
    private boolean performTreT1HIFI = false;
    private boolean doubleInversion = true;
    private boolean singleInversion = false;
    private boolean geScanner = true;
    private boolean siemensScanner = false;
    private boolean threeTField = true;
    private boolean onefiveTField = false;
    
    private boolean calculateT1 = true;
    private boolean showB1Map = false;
    private boolean calculateM0 = false;
    private boolean invertT1toR1 = false;
    
    private boolean useWeights = true;
    private boolean uniformAngleSpacing = true;
    
    
    
    private String[] wList;
    private String[] titles;
    private boolean completed;
    
    /** The frames for result images (if null at end of algorithm src ModelImage is destroyed) */
    private ViewJFrameImage t1ResultWindow = null;
    private ViewJFrameImage m0ResultWindow = null;
    private ViewJFrameImage r1ResultWindow = null;
    private ViewJFrameImage b1ResultWindow = null;
    
    public AlgorithmTreT1(double treTR, double irspgrTR,
            double irspgrKy, double irspgrFA, double maxT1, double maxM0,
            double[] treFA, double[] irspgrTr2, double[] irspgrTI,
            double[] spgrData, double[] irspgrData, double scale,
            double pointScale, double scaleIncrement, double[] estimates,
            double[] residuals, int[] direction, int[] spgrImageIndex,
            int[] irspgrImageIndex, int b1ImageIndex, double angleIncrement,
            int nsa, int nti, double maxAngle, boolean smoothB1Field,
            boolean performStraightTreT1,
            boolean performTreT1withPreCalculatedB1Map,
            boolean performTreT1HIFI, boolean doubleInversion,
            boolean singleInversion, boolean geScanner, boolean siemensScanner,
            boolean threeTField, boolean onefiveTField, boolean calculateT1,
            boolean showB1Map, boolean calculateM0, boolean invertT1toR1,
            boolean useWeights, boolean uniformAngleSpacing,
            boolean upperLeftCorner, boolean upperRightCorner,
            boolean lowerLeftCorner, boolean lowerRightCorner,
            boolean useSmartThresholding, boolean useHardThresholding,
            float noiseScale, float hardNoiseThreshold, String[] wList,
            String[] titles) {
        super();
        this.treTR = treTR;
        this.irspgrTR = irspgrTR;
        this.irspgrKy = irspgrKy;
        this.irspgrFA = irspgrFA;
        this.maxT1 = maxT1;
        this.maxM0 = maxM0;
        this.treFA = treFA;
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
        this.performStraightTreT1 = performStraightTreT1;
        this.performTreT1withPreCalculatedB1Map = performTreT1withPreCalculatedB1Map;
        this.performTreT1HIFI = performTreT1HIFI;
        this.doubleInversion = doubleInversion;
        this.singleInversion = singleInversion;
        this.geScanner = geScanner;
        this.siemensScanner = siemensScanner;
        this.threeTField = threeTField;
        this.onefiveTField = onefiveTField;
        this.calculateT1 = calculateT1;
        this.showB1Map = showB1Map;
        this.calculateM0 = calculateM0;
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
        
        init();
    }

    public AlgorithmTreT1(ModelImage destImage, ModelImage srcImage,
            double treTR, double irspgrTR, double irspgrKy, double irspgrFA,
            double maxT1, double maxM0, double[] treFA, double[] irspgrTr2,
            double[] irspgrTI, double[] spgrData, double[] irspgrData,
            double scale, double pointScale, double scaleIncrement,
            double[] estimates, double[] residuals, int[] direction,
            int[] spgrImageIndex, int[] irspgrImageIndex, int b1ImageIndex,
            double angleIncrement, int nsa, int nti, double maxAngle,
            boolean smoothB1Field, boolean performStraightTreT1,
            boolean performTreT1withPreCalculatedB1Map,
            boolean performTreT1HIFI, boolean doubleInversion,
            boolean singleInversion, boolean geScanner, boolean siemensScanner,
            boolean threeTField, boolean onefiveTField, boolean calculateT1,
            boolean showB1Map, boolean calculateM0, boolean invertT1toR1,
            boolean useWeights, boolean uniformAngleSpacing,
            boolean upperLeftCorner, boolean upperRightCorner,
            boolean lowerLeftCorner, boolean lowerRightCorner,
            boolean useSmartThresholding, boolean useHardThresholding,
            float noiseScale, float hardNoiseThreshold, String[] wList,
            String[] titles) {
        super(destImage, srcImage);
        this.treTR = treTR;
        this.irspgrTR = irspgrTR;
        this.irspgrKy = irspgrKy;
        this.irspgrFA = irspgrFA;
        this.maxT1 = maxT1;
        this.maxM0 = maxM0;
        this.treFA = treFA;
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
        this.performStraightTreT1 = performStraightTreT1;
        this.performTreT1withPreCalculatedB1Map = performTreT1withPreCalculatedB1Map;
        this.performTreT1HIFI = performTreT1HIFI;
        this.doubleInversion = doubleInversion;
        this.singleInversion = singleInversion;
        this.geScanner = geScanner;
        this.siemensScanner = siemensScanner;
        this.threeTField = threeTField;
        this.onefiveTField = onefiveTField;
        this.calculateT1 = calculateT1;
        this.showB1Map = showB1Map;
        this.calculateM0 = calculateM0;
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
        
        init();
    }
    
    /**
     * Initializes local variables that are internal to tre.
     */
    private void init() {
        computeProcessors();
    }
    
    protected void computeProcessors() {
        int processors = Runtime.getRuntime().availableProcessors();
        processors = processors > MAX_PROCESS ? MAX_PROCESS : processors;
        this.processDataThreads = 1;
        this.computeDataThreads = 1;
        this.loadDataThreads = 1;
        if(Preferences.isMultiThreadingEnabled() && processors > 2) {
            if(calculateT1 && processors-2 > processDataThreads + computeDataThreads) {
                processDataThreads++;
            }
            if(showB1Map && processors-2 > processDataThreads + computeDataThreads) {
                processDataThreads++;
            }
            if(calculateM0 && processors-2 > processDataThreads + computeDataThreads) {
                processDataThreads++;
            }
            if(invertT1toR1 && processors-2 > processDataThreads + computeDataThreads) {
                processDataThreads++;
            }
            
            ViewUserInterface.getReference().getMessageFrame().append("ComputeData: "+computeDataThreads+"\tProcessData: "+processDataThreads+"\n", ViewJFrameMessage.DEBUG);
            
            if(processors-2 > processDataThreads + computeDataThreads) {
                processDataThreads = (int)(((double)processDataThreads/(processDataThreads+computeDataThreads))*(processors-2));
                computeDataThreads = (processors-2-processDataThreads);
            }
            
            if(processDataThreads < 1) {
                processDataThreads = 1;
            }
            if(computeDataThreads < 1) {
                computeDataThreads = 1;
            } 
            
            loadDataThreads = processors-2;
            int divisor = ((calculateT1 ? 1 : 0) + (showB1Map ? 1 : 0) + (calculateM0 ? 1 : 0) + (invertT1toR1 ? 1 : 0));
            if(divisor < 1) {
                divisor = 1;
            }
            loadDataThreads = (int)((double)loadDataThreads/divisor);
            if(loadDataThreads < 1) {
                loadDataThreads = 1;
            }
            
            ViewUserInterface.getReference().getMessageFrame().append("ComputeData: "+computeDataThreads+"\tProcessData: "+processDataThreads+"\t loadData: "+loadDataThreads+"\n", ViewJFrameMessage.DEBUG);
            
            if(largestImage != null) {
                if(largestImage.getNDims() > 3) {
                    processDataThreads = processDataThreads > largestImage.getExtents()[3] ? largestImage.getExtents()[3] : processDataThreads;
                    computeDataThreads = computeDataThreads > largestImage.getExtents()[3] ? largestImage.getExtents()[3] : computeDataThreads;
                    loadDataThreads = loadDataThreads > largestImage.getExtents()[3]*divisor ? largestImage.getExtents()[3]*divisor : loadDataThreads;
                }
            }
            
            if(processDataThreads < 1) {
                processDataThreads = 1;
            }
            if(computeDataThreads < 1) {
                computeDataThreads = 1;
            } 
            if(loadDataThreads < 1) {
                loadDataThreads = 1;
            }
        }
        
        ViewUserInterface.getReference().getMessageFrame().append("ComputeData: "+computeDataThreads+"\tProcessData: "+processDataThreads+"\t loadData: "+loadDataThreads+"\n", ViewJFrameMessage.DEBUG);
    }

    /**
     * This method is used in both the conventional and HIFI case to display result images
     */
    protected void displayImages() {
        String prefix = new String();
        if(performTreT1HIFI) {
            prefix = "-HIFI";
        }
        
        if (calculateT1) {
            t1ResultWindow = new ViewJFrameImage(t1ResultStack);
            t1ResultWindow.setTitle("treT1"+prefix+"_T1_Map");
            t1ResultWindow.setVisible(true);
        } 
         
        if (calculateM0) {
            m0ResultWindow = new ViewJFrameImage(m0ResultStack);
            m0ResultWindow.setTitle("treT1"+prefix+"_M0_Map");
            m0ResultWindow.setVisible(true);
        } 
        
        if (invertT1toR1) {
            r1ResultWindow = new ViewJFrameImage(r1ResultStack);
            r1ResultWindow.setTitle("treT1"+prefix+"_R1_Map");
            r1ResultWindow.setVisible(true);
        } 
        
        if (performTreT1HIFI && showB1Map) {
            b1ResultWindow = new ViewJFrameImage(b1ResultStack);
            b1ResultWindow.setTitle("treT1"+prefix+"_B1_Map");
            b1ResultWindow.setVisible(true);
        } 
    }

    public boolean calculateT1UsingTreT1HIFI() {
        fireProgressStateChanged("Prepping data..hang on");
        fireProgressStateChanged(5);
        
        ModelImage image, irspgrImage;
        
        int width, height, tSeries, irspgrSlices;
        int irwidth, irheight;
        
        image = ViewUserInterface.getReference().getRegisteredImageByName(wList[spgrImageIndex[0]]);
        width = image.getExtents()[0];
        height = image.getExtents()[1];
        if(image.getNDims() > 2) {
            nSlices = image.getExtents()[2];
        } else {
            nSlices = 1;
        }
        
        do4D = false;
        tSeries = 1;
        for (int angle=0; angle<Nsa; angle++) {
            image = ViewUserInterface.getReference().getRegisteredImageByName(wList[spgrImageIndex[angle]]);
            if(image.getNDims() > 3 && !do4D) { //clause is only entered once per program operation
                largestImage = image;
                do4D = true;
                tSeries = image.getExtents()[3];
                
            }
        }
        
        if(!do4D) {
            for (int ti=0; ti<Nti; ti++) {
                image = ViewUserInterface.getReference().getRegisteredImageByName(wList[irspgrImageIndex[ti]]);  
                if(image.getNDims() > 3 && !do4D) { //clause is only entered once per program operation
                    largestImage = image;
                    do4D = true;
                    tSeries = image.getExtents()[3];
                }
            }
        }
        
        if(!do4D) {
            largestImage = image;
        }
        
        irspgrImage = ViewUserInterface.getReference().getRegisteredImageByName(wList[irspgrImageIndex[0]]);
        if(image.getNDims() > 2) {
            irspgrSlices = irspgrImage.getExtents()[2];
        } else {
            irspgrSlices = 1;
        }
        
        // check to make sure the IR-SPGR and SPGR data have the same size
        irwidth = irspgrImage.getExtents()[0];
        irheight = irspgrImage.getExtents()[1];
        
        if (irwidth != width || irheight != height || irspgrSlices != nSlices) {
            MipavUtil.displayError("IR-SPGR and SPGR data must have the same image dimensions.");
            return false;
        }
        computeProcessors();
        
        ExecutorService exec = Executors.newFixedThreadPool(computeDataThreads); 
        // start by calculaing the B1 field
        for(int t=0; t<tSeries; t++) {     
            exec.execute(new CalculteT1UsingTreT1HIFIInner(largestImage, width, height, irspgrSlices, t));    
        }
        exec.shutdown();
        
        try {
            exec.awaitTermination(1, TimeUnit.DAYS);
        } catch (InterruptedException e) {
            MipavUtil.displayError("Program did not execute correctly");
            e.printStackTrace();
        }
        
        fireProgressStateChanged("Computation finished..loading data");
        fireProgressStateChanged(85);
        
        initializeDisplayImages(largestImage);
        
        return true;
    }

    public boolean calculateT1UsingConventionalTreT1() {
        ModelImage image;
         
        int width, height, tSeries;
        
        image = ViewUserInterface.getReference().getRegisteredImageByName(wList[spgrImageIndex[0]]);
        width = image.getExtents()[0];
        height = image.getExtents()[1];
        if(image.getNDims() > 2) {
            nSlices = image.getExtents()[2];
        } else {
            nSlices = 1;
        }
        
        do4D = false;
        tSeries = 1;
        for (int angle=0; angle<Nsa; angle++) {
            image = ViewUserInterface.getReference().getRegisteredImageByName(wList[spgrImageIndex[angle]]);
            if(image.getNDims() > 3 && !do4D) { //clause is only entered once
                largestImage = image;
            	do4D = true;
                tSeries = image.getExtents()[3];
            }
        }
        
        if(!do4D) {
            largestImage = image;
        }
        computeProcessors();
        
        ExecutorService exec = Executors.newFixedThreadPool(computeDataThreads); 
    
        for(int t=0; t<tSeries; t++) {     
            exec.submit(new CalculateT1UsingConventionalTreT1Inner(largestImage, width, height, t));    
        }
        exec.shutdown();
        
        try {
            exec.awaitTermination(1, TimeUnit.DAYS);
        } catch (InterruptedException e) {
            MipavUtil.displayError("Program did not execute correctly");
            e.printStackTrace();
        }
        
        fireProgressStateChanged("Computation finished..loading data");
        fireProgressStateChanged(85);
        
        initializeDisplayImages(largestImage);
        
        return true;
    }

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        if(t1ResultWindow == null && t1ResultStack != null) {
            t1ResultStack.disposeLocal();
        }
        
        if(m0ResultWindow == null && m0ResultStack != null) {
            m0ResultStack.disposeLocal();
        }

        if(r1ResultWindow == null && r1ResultStack != null) {
            r1ResultStack.disposeLocal();
        }

        if(b1ResultWindow == null && b1ResultStack != null) {
            b1ResultStack.disposeLocal();
        }
    }
    
    public void runAlgorithm() {
        if (performTreT1HIFI) {
            completed = calculateT1UsingTreT1HIFI();
        }
        else {
            completed = calculateT1UsingConventionalTreT1();
        }
        
        setCompleted(completed);
    }

    public double signalResiduals(double x, double[] spgrData, double[] irspgrData, double Inversion, int Nfa, int Nti, double[] treFA, double treTR, double[] irspgrTr, double[] irspgrTI, double irspgrFA) {
        
        double sumX, sumY, sumXY, sumXX, slope, intercept, residuals;
        double t1Guess, m0Guess;
        double[] treGuess, irspgrGuess;
        
        double m0Scale;
        
        int p;
        
        treGuess = new double[Nfa];
        irspgrGuess = new double[Nti];
        
        sumX = 0.00;
        sumY = 0.00;
        sumXY = 0.00;
        sumXX = 0.00;
        for (p=0; p<Nfa; p++) {
            sumX += spgrData[p]/Math.tan(x*treFA[p]*3.14159265/180.00);
            sumY += spgrData[p]/Math.sin(x*treFA[p]*3.14159265/180.00);
            sumXY += spgrData[p]/Math.tan(x*treFA[p]*3.14159265/180.00) * spgrData[p]/Math.sin(x*treFA[p]*3.14159265/180.00);
            sumXX += spgrData[p]/Math.tan(x*treFA[p]*3.14159265/180.00) * spgrData[p]/Math.tan(x*treFA[p]*3.14159265/180.00);
        }
        slope = (Nfa*sumXY - sumX*sumY) / (Nfa*sumXX - sumX*sumX);
        intercept = (sumY - slope*sumX)/Nfa;
        
        if (slope > 0.00 && slope < 1.00) {
            t1Guess = -treTR/Math.log(slope);
            m0Guess = intercept/(1.00-slope);
        }
        else {
            t1Guess = 0.00;
            m0Guess = 0.00;
        }
        
        if (t1Guess > 0) {
            for (p=0; p<Nfa; p++) {
                treGuess[p] = m0Guess*(1.00-Math.exp(-treTR/t1Guess))*Math.sin(x*treFA[p]*3.14159265/180.00)/(1.00-Math.exp(-treTR/t1Guess)*Math.cos(x*treFA[p]*3.14159265/180.00));
            }
        }
        else {
            for (p=0; p<Nfa; p++) {
                treGuess[p] = 0.00;
            }
        }
        
        if (t1Guess > 0) {
            
            m0Scale = 0.975;
            
            for (p=0; p<Nti; p++) {
                irspgrGuess[p] = Math.abs( m0Scale*m0Guess*Math.sin(x*irspgrFA*3.14159265/180.00) * (1.00-Inversion*Math.exp(-irspgrTI[p]/t1Guess) + Math.exp(-irspgrTr[p]/t1Guess)) );
            }
        }
        else {
            for (p=0; p<Nti; p++) {
                irspgrGuess[p] = 0.00;
            }
        }
        
        residuals = 0.00;
        for (p=0; p<Nfa; p++) residuals += Math.pow( (spgrData[p]-treGuess[p]), 2.00);
        for (p=0; p<Nti; p++) residuals += Math.pow( (irspgrData[p]-irspgrGuess[p]), 2.00);
        
        return residuals;
    }

    public ModelImage getT1ResultStack() {
		return t1ResultStack;
	}

	public ModelImage getM0ResultStack() {
		return m0ResultStack;
	}

	public ModelImage getR1ResultStack() {
		return r1ResultStack;
	}

	public ModelImage getB1ResultStack() {
		return b1ResultStack;
	}
	
	/**
	 * This method initializes the final images as near-clones of the largest image in the set
	 * 
	 * @param image
	 */
	private void initializeDisplayImages(ModelImage image) {
	    if(calculateT1) {
            t1ResultStack = new ModelImage(ModelImage.DOUBLE, image.getExtents(), "t1_results");
            cloneFileInfo(image, t1ResultStack);
            
            loadFinalData(t1ResultStack, t1ResultData);   
        }
        
        if(calculateM0) {
            m0ResultStack = new ModelImage(ModelImage.DOUBLE, image.getExtents(), "m0_results");
            cloneFileInfo(image, m0ResultStack);
            
            loadFinalData(m0ResultStack, m0ResultData);   
        }
         
        if(invertT1toR1) {
            r1ResultStack = new ModelImage(ModelImage.DOUBLE, image.getExtents(), "r1_results");
            cloneFileInfo(image, r1ResultStack);
            
            loadFinalData(r1ResultStack, r1ResultData);   
        }
         
        if(performTreT1HIFI && showB1Map) {
            b1ResultStack = new ModelImage(ModelImage.DOUBLE, image.getExtents(), "b1_results");
            cloneFileInfo(image, b1ResultStack);
            
            loadFinalData(b1ResultStack, b1ResultData);   
        }
        
        displayImages();
	}

    private void loadFinalData(ModelImage image, double[][][] data) {
        int volumeSize = image.getVolumeSize();
        int sliceSize = image.getSliceSize();
        
        for(int i=0; i<data.length; i++) {
            for(int j=0; j<data[i].length; j++) {
                try {
                    image.importData(volumeSize*i + sliceSize*j, data[i][j], false);
                } catch (IOException e) {
                    e.printStackTrace();
                }
            }
        }
        
        image.calcMinMax();
    }

    private abstract class CalculateT1 extends CalculateT {
        
        
        public CalculateT1(ModelImage image, int t) {
            super(t);
            initializeLocalImages(image);
        }
        
        /**
         * This method initializes the intermediate images as a near-clone of the largest image in the set.
         * 
         * @param image
         */
        protected void initializeLocalImages(ModelImage image) {
            int tSlice = 1;
            if(image.getNDims() > 3) {
                tSlice = image.getExtents()[3];
            }
            
            if(calculateT1) {
                t1ResultData = new double[tSlice][][];
            }
            
            if(calculateM0) {
                m0ResultData = new double[tSlice][][];
            }
             
            if(invertT1toR1) {
                r1ResultData = new double[tSlice][][];
            }
             
            if(performTreT1HIFI && showB1Map) {
                b1ResultData = new double[tSlice][][];
            }
        }
    }

    private class CalculteT1UsingTreT1HIFIInner extends CalculateT1 implements Runnable {
    
        
        private int irspgrSlices;
    
        public CalculteT1UsingTreT1HIFIInner(ModelImage image, int width, int height, int irspgrSlices, int t) {
            super(image, t);
            this.width = width;
            this.height = height;
            this.irspgrSlices = irspgrSlices;
        }
        
        public void run() {
            dataBar.setVisible(true);
            String prefix;
            @SuppressWarnings("unused")
            double fbx;
            double Inversion, ax, bx, cx, fax, fcx, R, C, precision, x0, x1, x2, x3, f1, f2, xmin;
            double smoothedB1;
            ModelImage image;
            
            Inversion = 1+Math.cos(0.98*3.14159265/180.00);
            R = 0.61803399;
            C = 1.00-R;
            precision = 0.003;
            
            double ernstAngle, ernstSignal, collectedSignal, weight, sumWeights;
            double sumX, sumY, sumXY, sumXX, slope, intercept, lnslope, t1, e1, m0, r1, d, a;
            float noiseSum, threshold;
            int noiseIndex;
            int x,y,k,p, ti, angle, p1, p2, pixelIndex;
            
            double[] fa;
            double[] scaledFA;
            double[][] spgrPixelValues;
            double[][] irspgrPixelValues;
            double[][] t1Values, m0Values, r1Values, b1field;
            double[][][] b1Values;
            
            if(do4D) {
                prefix = "Series "+t+": ";
            } else {
                prefix = "";
            }
            
            spgrPixelValues = new double[Nsa][width*height];
            irspgrPixelValues = new double[Nsa][width*height];
            if (calculateT1) { 
                t1Values = new double[irspgrSlices][width*height];
            }
            else { 
                t1Values = new double[1][1];
            }
            if (calculateM0) { 
                m0Values = new double[irspgrSlices][width*height];
            }
            else { 
                m0Values = new double[1][1];
            }
            if (invertT1toR1) { 
                r1Values = new double[irspgrSlices][width*height];
            }
            else { 
                r1Values = new double[1][1];
            }
            
            b1field = new double[irspgrSlices][width*height];
            b1Values = new double[irspgrSlices][height][width];
            
            fa = new double[Nsa];
            scaledFA = new double[Nsa];
            for (angle=0; angle<Nsa;angle++) {
                fa[angle] = Math.toRadians(treFA[angle]);
            }
            //double irFA = Math.toRadians(irspgrFA);
            
            for (k=0; k<irspgrSlices; k++) {
            
                noiseSum = (float) 0.00;
                image = ViewUserInterface.getReference().getRegisteredImageByName(wList[spgrImageIndex[0]]);
                
                threshold = hardNoiseThreshold;
                if (useSmartThresholding) {
                    threshold = calculateThreshold(image, k);
                }
                else {
                    threshold = (float) hardNoiseThreshold;
                }
                dataBar.setMessage(prefix+"calculating B1 field for slice: "+k+" of "+(nSlices-1));
                dataBar.updateValue(5+(int)((float)k/(float)nSlices*60.0));
                if(interrupted()) {
                    return;
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
                            fax = signalResiduals(ax, spgrData, irspgrData, Inversion, Nsa, Nti, treFA, treTR, irspgrTr, irspgrTI, irspgrFA);
                            fcx = signalResiduals(cx, spgrData, irspgrData, Inversion, Nsa, Nti, treFA, treTR, irspgrTr, irspgrTI, irspgrFA);
                            
                            // choose bx such that ax < bx < cx and f(bx) < f(ax) and f(bx) < f(cx)
                            if (fax < fcx) bx = ax + 0.2;
                            else bx = cx - 0.2;
                            fbx = signalResiduals(bx, spgrData, irspgrData, Inversion, Nsa, Nti, treFA, treTR, irspgrTr, irspgrTI, irspgrFA);
                            
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
                            
                            f1 = signalResiduals(x1, spgrData, irspgrData, Inversion, Nsa, Nti, treFA, treTR, irspgrTr, irspgrTI, irspgrFA);
                            f2 = signalResiduals(x2, spgrData, irspgrData, Inversion, Nsa, Nti, treFA, treTR, irspgrTr, irspgrTI, irspgrFA);
                            
                            while ( Math.abs(x3-x0) > precision*(Math.abs(x1)+Math.abs(x2)) ) {
                                if (f2 < f1) {
                                    x0 = x1;
                                    x1 = x2;
                                    x2 = R*x1 + C*x3;
                                    f1 = f2;
                                    f2 = signalResiduals(x2, spgrData, irspgrData, Inversion, Nsa, Nti, treFA, treTR, irspgrTr, irspgrTI, irspgrFA);
                                }
                                else {
                                    x3 = x2;
                                    x2 = x1;
                                    x1 = R*x2 + C*x0;
                                    f2 = f1;
                                    f1 = signalResiduals(x1, spgrData, irspgrData, Inversion, Nsa, Nti, treFA, treTR, irspgrTr, irspgrTI, irspgrFA);
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
                for (k=0; k<irspgrSlices; k++) {
                    dataBar.setMessage(prefix+"smoothing B1 field on slice: "+k+" of "+(nSlices-1));
                    dataBar.updateValue(80+(int)((float)k/(float)nSlices*10.0));
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
                return;
            }
        
            // clear the b1Values matrix
            
            // finally, calculate the corrected T1 estimates, changed slice start 
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
                                if(image.getNDims() < 4) {
                                    noiseSum += image.getFloat(x, y, k);
                                } else {
                                    noiseSum += image.getFloat(x, y, k, t);
                                }
                            }
                        }
                    }
                    if (upperRightCorner) {
                        for (y=20; y<30; y++) {
                            for (x=width-30; x<width-20; x++) {
                                if(image.getNDims() < 4) {
                                    noiseSum += image.getFloat(x, y, k);
                                } else {
                                    noiseSum += image.getFloat(x, y, k, t);
                                }
                            }
                        }
                    }
                    if (lowerLeftCorner) {
                        for (y=height-30; y<height-20; y++) {
                            for (x=20; x<30; x++) {
                                if(image.getNDims() < 4) {
                                    noiseSum += image.getFloat(x, y, k);
                                } else {
                                    noiseSum += image.getFloat(x, y, k, t);
                                }
                            }
                        }
                    }
                    if (lowerRightCorner) {
                        for (y=height-30; y<height-20; y++) {
                            for (x=width-30; x<width-20; x++) {
                                if(image.getNDims() < 4) {
                                    noiseSum += image.getFloat(x, y, k);
                                } else {
                                    noiseSum += image.getFloat(x, y, k, t);
                                }
                            }
                        }
                    }
                    
                    threshold = (float) ( (noiseSum/noiseIndex + 1)*noiseScale );
                }
                else {
                    threshold = (float) hardNoiseThreshold;
                }
            
            
                dataBar.setMessage(prefix+"calculating T1 values on slice: "+k+" of "+(nSlices-1));
                dataBar.updateValue(10+(int)((float)k/(float)nSlices*20.0));
                if(interrupted()) {
                    return;
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
                            for (p=0; p<Nsa; p++) {
                                scaledFA[p] = fa[p]*b1field[k][pixelIndex];
                            }
                            
                            // grab the SPGR values for this pixel
                            for (p=0; p<Nsa; p++) {
                                spgrData[p] = spgrPixelValues[p][pixelIndex];
                            }
                            
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
                                    t1 = treTR/lnslope;
                                    m0 = intercept/(1.00-Math.exp(-treTR/t1));
                                }
                                else {
                                    m0 = maxM0;
                                    t1 = maxT1;
                                }
                            }
                            else {
                                m0 = maxM0;
                                t1 = maxT1;
                            }
                        
        
                            if (t1 < 0.00 || t1 > maxT1) {
                                t1 = maxT1;
                            }
                            if (m0 < 0.00 || m0 > maxM0) {
                                m0 = maxM0;
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
                                    m0 = 0;
                                    r1 = 0;
                                }
                                else {
                                    e1 = Math.exp(-1.00*treTR/t1);
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
                                            t1 = treTR/lnslope;
                                            m0 = intercept/(1.00-Math.exp(-treTR/t1));
                                        }
                                        else {
                                            m0 = maxM0;
                                            t1 = maxT1;
                                        }
                                    }
                                    else {
                                        m0 = maxM0;
                                        t1 = maxT1;
                                    }
                                    
                                    
                                    if (t1 < 0.00 || t1 > maxT1) {
                                        t1 = maxT1;
                                    } 
                                    if (m0 < 0.00 || m0 > maxM0) {
                                        m0 = maxM0;
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
                                t1Values[k][pixelIndex] = t1;
                            }
                            if (calculateM0) {
                                m0Values[k][pixelIndex] = (float) m0;
                            }
                            if (invertT1toR1) {
                                r1Values[k][pixelIndex] = (float) r1;
                            }
                        }
                        else {
                            if (calculateT1) {
                                t1Values[k][pixelIndex] = 0;
                            }
                            if (calculateM0) {
                                m0Values[k][pixelIndex] = 0;
                            }
                            if (invertT1toR1) {
                                r1Values[k][pixelIndex] = 0;
                            }
                        }
                        pixelIndex++;
                    }
                }
            }
            
            if(calculateT1) {
                t1ResultData[t] = t1Values;
            }
            if(calculateM0) {
                m0ResultData[t] = m0Values;
            }
            if(invertT1toR1) {
                r1ResultData[t] = r1Values;
            }
            if(showB1Map) {
                b1ResultData[t] = b1field;
            }
            dataBar.setVisible(false);
        }
        
    }

    private class CalculateT1UsingConventionalTreT1Inner extends CalculateT1 implements Runnable {
    
        public CalculateT1UsingConventionalTreT1Inner(ModelImage image, int width, int height, int t) {
            super(image, t);
            this.width = width;
            this.height = height;
        }
        
        public void run() {
            dataBar.setVisible(true);
            
            int x,y,k,angle, pixelIndex;
            
            double b1;
            double[] fa, b1Values;
            double[][] pixelValues;
            double[][] t1Values, m0Values, r1Values;
            
            double sumX, sumY, sumXY, sumXX, slope, intercept, lnslope, t1, e1, m0, r1, d, a;
            double ernstAngle, ernstSignal, collectedSignal, weight, sumWeights;
            
            float threshold;
            
            ModelImage image, b1FieldImage = null;
            
            String prefix = new String();
            
            if(do4D) {
                prefix = "Series "+t+": ";
            } else {
                prefix = "";
            }
        
            pixelValues = new double[Nsa][width*height];
            t1Values = new double[nSlices][width*height];
            m0Values = new double[nSlices][width*height];
            r1Values = new double[nSlices][width*height];
            
            fa = new double[Nsa];
            for (angle=0; angle<Nsa;angle++) {
                fa[angle] = Math.toRadians(treFA[angle]);
            }
            
            for (k=0; k<nSlices; k++) { //changed slice size
                image = ViewUserInterface.getReference().getRegisteredImageByName(wList[spgrImageIndex[0]]);
                
                threshold = hardNoiseThreshold;
                if (useSmartThresholding) {
                    threshold = calculateThreshold(image, k);
                } else {
                    threshold = (float) hardNoiseThreshold;
                }
                dataBar.setMessage(prefix+"working on slice: "+k+" of "+(nSlices-1)+". Noise Threshold = "+threshold);
                dataBar.updateValue(5+(int)((float)k/(float)nSlices*80.0));
                
                if(interrupted()) {
                    return;
                }
                
                if (performTreT1withPreCalculatedB1Map) {
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
                            if (performTreT1withPreCalculatedB1Map) {
                                if(b1FieldImage.getNDims() < 4) {
                                    b1Values[pixelIndex] = b1FieldImage.getDouble(x, y, k); 
                                } else {
                                    b1Values[pixelIndex] = b1FieldImage.getDouble(x, y, k, t); 
                                }
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
                        
                        if (performTreT1withPreCalculatedB1Map) {
                            b1 = b1Values[pixelIndex];
                            if (b1 == 0) { 
                                b1 = 1.00;
                            }
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
                                t1 = treTR/lnslope;
                                m0 = intercept/(1.00-Math.exp(-treTR/t1));
                            } 
                            else {
                                m0 = maxM0;
                                t1 = maxT1;
                            }
                        }
                        else {
                            m0 = maxM0;
                            t1 = maxT1;
                        }
                        
                        if (t1 < 0 || t1 > maxT1) {
                            t1 = maxT1;
                        }
                        if (m0 < 0 || m0 > maxM0) {
                            m0 = maxM0;
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
                                m0 = 0;
                                r1 = 0;
                            }
                            else {
                                e1 = Math.exp(-1.00*treTR/t1);
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
                                        t1 = treTR/lnslope;
                                        m0 = intercept/(1.00-Math.exp(-treTR/t1));
                                    }
                                    else {
                                        m0 = maxM0;
                                        t1 = maxT1;
                                    }
                                }
                                else {
                                    m0 = maxM0;
                                    t1 = maxT1;
                                }
                            
                                if (t1 < 0 || t1 > maxT1) {
                                    t1 = maxT1;
                                } 
                                if (m0 < 0 || m0 > maxM0) {
                                    m0 = maxM0;
                                }
                                if (t1 != 0) {
                                    r1 = 1/t1;
                                }
                                else {
                                    r1 = 0;
                                }
                            }
                        }
                        
                        if(calculateT1) {
                            t1Values[k][pixelIndex] = (float) t1;
                        }
                        if(calculateM0) {
                            m0Values[k][pixelIndex] = (float) m0;
                        }
                        if(invertT1toR1) {
                            r1Values[k][pixelIndex] = (float) r1;
                        }
                    }
                    else {
                        if(calculateT1) {
                            t1Values[k][pixelIndex] = 0;
                        }
                        if(calculateM0) {
                            m0Values[k][pixelIndex] = 0;
                        }
                        if(invertT1toR1) {
                            r1Values[k][pixelIndex] = 0;
                        }
                    }
                }
            }
            
            dataBar.updateValue(87);
            
            if(calculateT1) {
                t1ResultData[t] = t1Values;
            }
            if(calculateM0) {
                m0ResultData[t] = m0Values;
            }
            if(invertT1toR1) {
                r1ResultData[t] = r1Values;
            }
            
            dataBar.setVisible(false);
        }
    }
	
	
}
