package niaid.tumorSim.createMap;
//MIPAV is freely available from http://mipav.cit.nih.gov

//THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, 
//EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES 
//OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND 
//NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT 
//HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, 
//WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING 
//FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE 
//OR OTHER DEALINGS IN THE SOFTWARE. 

/*****************************************************************
******************************************************************

The MIPAV application is intended for research use only.
This application has NOT been approved for ANY diagnostic use 
by the Food and Drug Administration. There is currently no 
approval process pending. 

This software may NOT be used for diagnostic purposes.

******************************************************************
******************************************************************/

import java.awt.event.ActionEvent;
import java.io.File;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Random;

import WildMagic.LibFoundation.Mathematics.Vector3d;

import niaid.tumorSim.createMap.PlugInDialogCreateTumorMap543a.NoiseMode;


import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmMorphology3D;
import gov.nih.mipav.model.algorithms.AlgorithmVOIExtraction;
import gov.nih.mipav.model.algorithms.filters.AlgorithmGaussianBlur;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmNoise;
import gov.nih.mipav.model.file.FileInfoBase.Unit;
import gov.nih.mipav.model.file.FileInfoImageXML;
import gov.nih.mipav.model.file.FileInfoXML;
import gov.nih.mipav.model.file.FileUtility;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase.DataType;
import gov.nih.mipav.util.CircleUtil;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewJProgressBar;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.dialogs.JDialogSubsample;

/**
 * This class implements a basic algorithm that performs operations on 2D and 3D images. 
 * By extending AlgorithmBase, it has no more functionality than any other algorithm in MIPAV.
 * No functionality specifically makes it a plug-in.
 * 
 * @author Justin Senseney (SenseneyJ@mail.nih.gov)
 * @see http://mipav.cit.nih.gov
 */

public class PlugInAlgorithmCreateTumorMap543a extends AlgorithmBase {

    public static final String INTENSITY1 = "Intensity Pre: ";
    public static final String INTENSITY2 = "Intensity Post: ";
    public static final String RADIUS1 = "Radius Pre: ";
    public static final String RADIUS2 = "Radius Post: ";
    public static final String STD_DEV = "std dev: ";
    public static final String NOISE_LEVEL = "Adding noise parameter: ";
    public static final String NORMAL_TISSUE = "Normal tissue: ";
    public static final String PARTIAL = "Partial volume pixels: ";
    public static final String TOTAL = "Total voxels in tumor";
    
    /** Image dimensions */
    private int xyDim, zDim;
    /** Image resolutions */
    private double xyRes, zRes;
    /** Tumor radius in units of image */
    private double initRadius;
    private double tumorChange;
    private PlugInDialogCreateTumorMap543a.TumorSimMode simMode;
    /** Center of created sphere */
    private int xCenter, yCenter, zCenter;
    private double intensity1, intensity2;
    /** Result images */
    private ModelImage image1a, image2a;
    /** Tumor only images */
    private ModelImage image1aTumor, image2aTumor;
    private int[][] sphere;
    /** Amount of subsampling to occur */
    private int subsampleAmount;
    /** Whether boundary checking is necessary during sphere population */
    private boolean doBoundCheck;
    /** Whether sphere should be populated entirely within field of view */
    private boolean doCenter;
    /** Maximum of Rician distributed noise */
    private double noiseMax;
    /** Standard deviation of Gaussian distributed noise */
    private double stdDevGaussian;
    /** Normal tissue value, assummed same for image 1 and 2 */
    private double normalTissue;
    /** Standard deviation of normal tissue intensity and tumor intensities*/
    private double stdDevIntensity1, stdDevIntensity2, stdDevNormal;
    /** Noise mode */
    private NoiseMode noise;
    /** Alg iteration number */
    private int iter = -1;
   
    public PlugInAlgorithmCreateTumorMap543a() {
        // TODO Auto-generated constructor stub
    }
    
    /**
     * Constructor.
     * @param intensity1 
     * @param intensity2 
     * @param stdDevIntensity2 
     * @param noise 
     * @param intensity22 
     * @param noiseParam either rician or gaussian noise parameter
     * @param normalTissue 
     * @param stdDevNormal 
     * @param subsample 
     *
     */
	public PlugInAlgorithmCreateTumorMap543a(int xyDim, int zDim, double xyRes,
            double zRes, double initRadius, double tumorChange,
            PlugInDialogCreateTumorMap543a.TumorSimMode simMode, 
            double intensity1, double stdDevIntensity1, double intensity2, double stdDevIntensity2, 
            int subsampleAmount, boolean doCenter, NoiseMode noise, double noiseParam, double normalTissue, double stdDevNormal) {
        this.xyDim = xyDim;
        this.zDim = zDim;
        
        this.xyRes = xyRes;
        this.zRes = zRes;
        
        this.doCenter = doCenter;
        
        this.initRadius = initRadius;
        if(tumorChange >= 2) {
            tumorChange /= 100;
        } else if(tumorChange >= 1) {
            tumorChange = tumorChange-1;
        }
        this.tumorChange = tumorChange;
        this.simMode = simMode;
        this.intensity1 = intensity1;
        this.stdDevIntensity1 = stdDevIntensity1;
        this.intensity2 = intensity2;
        this.stdDevIntensity2 = stdDevIntensity2;
        
        this.subsampleAmount = subsampleAmount;
        
        this.noise = noise;
        
        switch(noise) {
        case gaussian:
            this.stdDevGaussian = noiseParam;
            break;
        default:
            this.noiseMax = noiseParam;
            break;
        }
        
        
        this.normalTissue = normalTissue;
        this.stdDevNormal = stdDevNormal;
    }
    
	//  ~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        image1a = null;
        image2a = null;
        image1aTumor = null;
        image2aTumor = null;
        super.finalize();
    }
    
    /**
     * Starts the algorithm.  At the conclusion of this method, AlgorithmBase reports to any
     * algorithm listeners that this algorithm has completed.  This method is not usually called explicitly by
     * a controlling dialog.  Instead, see AlgorithmBase.run() or start().
     */
    public void runAlgorithm() {
        FileInfoImageXML fileInfoImage1 = new FileInfoImageXML("image1a", null, FileUtility.RAW);
        FileInfoImageXML fileInfoImage2 = new FileInfoImageXML("image2a", null, FileUtility.RAW);
        
        FileInfoImageXML fileInfoImage1TumorOnly = new FileInfoImageXML("image1a_tumor", null, FileUtility.RAW);
        FileInfoImageXML fileInfoImage2TumorOnly = new FileInfoImageXML("image2a_tumor", null, FileUtility.RAW);
        
        if(iter != -1) {
            fileInfoImage1.setFileName(fileInfoImage1.getFileName()+"_iter"+iter);
            fileInfoImage1TumorOnly.setFileName(fileInfoImage1TumorOnly.getFileName()+"_iter"+iter);
            fileInfoImage2.setFileName(fileInfoImage2.getFileName()+"_iter"+iter);
            fileInfoImage2TumorOnly.setFileName(fileInfoImage2TumorOnly.getFileName()+"_iter"+iter);
        }
        
        setBasicInfo(fileInfoImage1, DataType.FLOAT);
        setBasicInfo(fileInfoImage2, DataType.FLOAT);
        
        setBasicInfo(fileInfoImage1TumorOnly, DataType.FLOAT);
        setBasicInfo(fileInfoImage2TumorOnly, DataType.FLOAT);
        
        image1a = ViewUserInterface.getReference().createBlankImage(fileInfoImage1, isRunningInSeparateThread());
        image1a.setImageName("image1a");
        image2a = ViewUserInterface.getReference().createBlankImage(fileInfoImage2, isRunningInSeparateThread());
        image2a.setImageName("image2a");
        
        image1aTumor = ViewUserInterface.getReference().createBlankImage(fileInfoImage1TumorOnly, isRunningInSeparateThread());
        image1aTumor.setImageName("image1a_tumor");
        image2aTumor = ViewUserInterface.getReference().createBlankImage(fileInfoImage2TumorOnly, isRunningInSeparateThread());
        image2aTumor.setImageName("image2a_tumor");
        
        if(isRunningInSeparateThread()) {
            image1a.getParentFrame().setVisible(false);
            image2a.getParentFrame().setVisible(false);
            image1aTumor.getParentFrame().setVisible(false);
            image2aTumor.getParentFrame().setVisible(false);
        }
        
        if(iter != -1) {
            image1a.setImageName(image1a.getImageName()+"_iter"+iter);
            image1aTumor.setImageName(image1aTumor.getImageName()+"_iter"+iter);
            image2a.setImageName(image2a.getImageName()+"_iter"+iter);
            image2aTumor.setImageName(image2aTumor.getImageName()+"_iter"+iter);
        }
        
        setNormalTissue(image1a, stdDevIntensity1);
        setNormalTissue(image2a, stdDevIntensity2);
        
        int xyLargerRadius = (int)Math.ceil(defineLargerRadius()/xyRes);
        
        doBoundCheck = false;
        Random r = new Random(); 
        if(!doCenter || (xyDim-2*xyLargerRadius) <= 0) {
        	xCenter = r.nextInt(xyDim);
            yCenter = r.nextInt(xyDim);
            doBoundCheck = true;
            if((xyDim-1-2*xyLargerRadius) < 0) {
                Preferences.data("Unable to enclose tumor within FOV due to large radius. \n");
            }
        } else { //ensures entire tumor is inside image
        	xCenter = r.nextInt(xyDim-1-2*xyLargerRadius)+xyLargerRadius;
            yCenter = r.nextInt(xyDim-1-2*xyLargerRadius)+xyLargerRadius;
        }
        
        int zLargerRadius = (int)Math.ceil(defineLargerRadius()/zRes);
        
        if(!doCenter || (zDim-1-2*zLargerRadius) <= 0) {
        	zCenter = r.nextInt(zDim);
        } else {
        	zCenter = r.nextInt(zDim-1-2*zLargerRadius)+zLargerRadius;
        	doBoundCheck = true;
        }
        
        Preferences.debug("Center of tumor: "+xCenter+", "+yCenter+", "+zCenter+"\n");
        Preferences.data("Center of tumor: "+xCenter+", "+yCenter+", "+zCenter+"\n");
        
        Preferences.data(INTENSITY1+intensity1+"\t"+STD_DEV+stdDevIntensity1+";\n");
        Preferences.data(INTENSITY2+intensity2+"\t"+STD_DEV+stdDevIntensity2+";\n");
        Preferences.data(NORMAL_TISSUE+normalTissue+"\t"+STD_DEV+stdDevNormal+";\n");
        
        fireProgressStateChanged("Populating spheres");
        
        populateSphere(initRadius, intensity1, stdDevIntensity1, image1a);      
        populateSphere(getChangedRadius(), intensity2, stdDevIntensity2, image2a);
                
        populateSphere(initRadius, intensity1, 0, image1aTumor); //use to find subsampling effect     
        populateSphere(getChangedRadius(), intensity2, 0, image2aTumor); //use to find subsampling effect
        
        addPartialVoluming(initRadius, image1a, image1aTumor);
        addPartialVoluming(getChangedRadius(), image2a, image2aTumor);
        
        if(subsampleAmount != 0) {
            image1a = subsample(image1a);       
            image2a = subsample(image2a);
            image1aTumor = subsample(image1aTumor);
            image2aTumor = subsample(image2aTumor);
            image1aTumor.setImageName("image1aTumor");
            image2aTumor.setImageName("image2aTumor");
            image1a.getParentFrame().setVisible(false);
            image2a.getParentFrame().setVisible(false);
            image1aTumor.getParentFrame().setVisible(false);
            image2aTumor.getParentFrame().setVisible(false);
        }
        
        createVOI(image1aTumor, 0+Double.MIN_VALUE, Double.MAX_VALUE);
        createVOI(image2aTumor, 0+Double.MIN_VALUE, Double.MAX_VALUE);
        
        countPixels(image1aTumor, intensity1);
        countPixels(image2aTumor, intensity2);
        
        image1a.setImageName("image1a");
        for(int i=0; i<image1a.getFileInfo().length; i++) {
            image1a.getFileInfo(i).setFileDirectory(Preferences.getPreferencesDir());
        }
        image2a.setImageName("image2a");
        for(int i=0; i<image2a.getFileInfo().length; i++) {
            image2a.getFileInfo(i).setFileDirectory(Preferences.getPreferencesDir());
        }
        
        
        if(subsampleAmount != 0) {
            xCenter = (int) Math.round(((double)xCenter)/subsampleAmount);
            yCenter = (int) Math.round(((double)yCenter)/subsampleAmount);
            zCenter = (int) Math.round(((double)zCenter)/subsampleAmount);
        }
        
        Preferences.debug("Center of tumor subsampled: "+xCenter+", "+yCenter+", "+zCenter+";\n");
        Preferences.data("Center of tumor subsampled: "+xCenter+", "+yCenter+", "+zCenter+";\n");
         
        if(noiseMax != 0 && stdDevGaussian != 0) {
            generateNoise(image1a);
            generateNoise(image2a);
        }
        
        Preferences.data(RADIUS1+initRadius+"\n");
        Preferences.data(RADIUS2+getChangedRadius()+"\n");
        
        image1a.calcMinMax();
        image2a.calcMinMax();

        image1aTumor.calcMinMax();
        image2aTumor.calcMinMax();
        
    	setCompleted(true); //indicating to listeners that the algorithm completed successfully

    } // end runAlgorithm()

    /**
     * Adds partial voluming pixels to edge of sphere by multiplying edge pixels by their percent coverage of the radial line
     * from the origin of the tumor to the tumor line.
     * @param tumorImage image with normal tissue intensity == 0
     */
    private void addPartialVoluming(double radius, ModelImage image, ModelImage tumorImage) {
        Vector3d center = new Vector3d(xCenter*xyRes, yCenter*xyRes, zCenter*zRes);
        Vector3d point = new Vector3d();
        double distance = 0.0, relevantRes = 0.0, percent = 0.0;
        for(int x=0; x<image.getExtents()[0]; x++) {
            point.X = x*xyRes;
            for(int y=0; y<image.getExtents()[1]; y++) {
                point.Y = y*xyRes;
                for(int z=0; z<image.getExtents()[2]; z++) {
                    point.Z = z*zRes;
                    if(tumorImage.getDouble(x, y, z) != 0) {
                        distance = center.Distance(point);
                        if(distance > radius) {
                            relevantRes = xyRes;
                            if(zCenter - z > xCenter - x || 
                                    zCenter - z > yCenter - y) {
                                relevantRes = zRes;
                            }
                            percent = 1-((distance - radius)/relevantRes);  //tumor contribution to pixel
                            tumorImage.set(x, y, z, percent*tumorImage.getDouble(x, y, z));
                            //double old = image.getDouble(x, y, z);
                            image.set(x, y, z, percent*image.getDouble(x, y, z) + ((1-percent)*normalTissue));
                            //System.out.println("At: "+x+", "+y+", "+z+": "+distance+"\tvs "+radius+": "+image.getDouble(x, y, z)+"\tvs "+old);
                        }
                    }
                }
            }
        }
    }

    public static void createVOI(ModelImage image, double lowerBound, double upperBound) {

        FileInfoImageXML fileInfo = (FileInfoImageXML) image.getFileInfo()[0].clone();
        fileInfo.setDataType(DataType.BOOLEAN.getLegacyNum());
        
        ModelImage imageBin = ViewUserInterface.getReference().createBlankImage(fileInfo);
        imageBin.getParentFrame().setVisible(false);
        
        double intensityValue = 0.0;
        for(int i=0; i<image.getDataSize(); i++) {
            intensityValue = image.getDouble(i);
            if(intensityValue >= lowerBound && intensityValue <= upperBound) {
                imageBin.set(i, true);
            }
        }
        
        AlgorithmMorphology3D idObjectsAlgo3D;
        int method = AlgorithmMorphology3D.ID_OBJECTS;
        
        idObjectsAlgo3D = new AlgorithmMorphology3D(imageBin, 0, 0, method, 0, 0, 0, 0, true);
        idObjectsAlgo3D.setMinMax(1, Integer.MAX_VALUE);
        idObjectsAlgo3D.run();
        idObjectsAlgo3D.finalize();
        idObjectsAlgo3D = null;

        imageBin.calcMinMax();
        final AlgorithmVOIExtraction VOIExtractionAlgo = new AlgorithmVOIExtraction(imageBin);
        VOIExtractionAlgo.setRunningInSeparateThread(false);
        VOIExtractionAlgo.run();
        
//        ViewJFrameImage imageFrame = new ViewJFrameImage(imageBin);
//        imageFrame.setVisible(true);
        
        image.registerVOI(imageBin.getVOIs().get(0));
        
        ViewUserInterface.getReference().unRegisterImage(imageBin);
    }

    private void countPixels(ModelImage image, double intensity) {
        int numPixelsTumor = 0;
        int numPixelsSubsampled = 0;
        for(int i=0; i<image.getDataSize(); i++) {
            if(image.getDouble(i) != 0) {
                if(image.getDouble(i) != intensity) {
                    numPixelsSubsampled++;
                } 
                numPixelsTumor++;
            }
        }
        
        Preferences.data("For "+image.getImageName()+" "+TOTAL+" "+numPixelsTumor+"\n");
        Preferences.data("For "+image.getImageName()+" "+PARTIAL+" "+numPixelsSubsampled+"\n");
    }

    private void setNormalTissue(ModelImage image, double stdDevIntensity) {
        Random r = new Random();
        for(int i=0; i<image.getDataSize(); i++) {
            image.set(i, normalTissue+r.nextGaussian()*stdDevIntensity);
        }
    }

    /**
     * @param iter the iter to set
     */
    public void setIter(int iter) {
        this.iter = iter;
    }

    private void generateNoise(ModelImage image) {
        AlgorithmNoise noiseAlg = null;
        switch(noise) {
        case gaussian:
            noiseAlg = new AlgorithmNoise(image, AlgorithmNoise.GAUSSIAN, stdDevGaussian*4.0, 5, 1, 0, 1); //alg uses 4*std_dev to define min/max
            Preferences.data(NOISE_LEVEL+stdDevGaussian+";\n");
            break;
        default:
            noiseAlg = new AlgorithmNoise(image, AlgorithmNoise.RICIAN, noiseMax, 5, 1, 0, 1);
            Preferences.data(NOISE_LEVEL+noiseMax+";\n");
            break;
        }
        noiseAlg.setRunningInSeparateThread(false);
        noiseAlg.run();
    }


    private ModelImage subsample(ModelImage image) {
        JDialogSubsample subsample = new JDialogSubsample(image.getParentFrame(), image);
        subsample.setVisible(false);
        subsample.setProcessIndep(false);
        subsample.setDoVOI(false);
        subsample.setSubsamplingRate(subsampleAmount);
        subsample.setSeparateThread(false);
        subsample.actionPerformed(new ActionEvent(this, 0, "OK"));

        subsample.getResultImage().getParentFrame().setVisible(false);
        ViewUserInterface.getReference().unRegisterImage(image);
        return subsample.getResultImage();
    }

    private void populateSphere(double radius, double intensity, double stdDev, ModelImage image) {
        Random r = new Random();
        sphere = CircleUtil.get3DPointsInSphere(xCenter, yCenter, zCenter, xyRes, xyRes, zRes, radius);
        int xyDimBound = xyDim-1;
        int zDimBound = zDim-1;
        
        for(int i=0; i<sphere.length; i++) {
            if(doBoundCheck) {
                if(sphere[i][0] > xyDimBound || sphere[i][0] < 0 ||
                        sphere[i][1] > xyDimBound || sphere[i][1] < 0 || 
                        sphere[i][2] > zDimBound || sphere[i][2] < 0) {
                    continue;
                }
            }
            image.set(sphere[i][0], sphere[i][1], sphere[i][2], intensity+r.nextGaussian()*stdDev);
        }
    }
    
    private double getChangedRadius() {
        
        double newRadius = 0;
        switch(simMode) {
        case grow:
            newRadius = (initRadius*(1+tumorChange));
            break;
            
        case shrink:
            newRadius = (initRadius*(1-tumorChange));
            break;
            
        case none:
        //case intensify:
        //case deintensify:
        default:
            newRadius = initRadius;
            
        }
        
        return newRadius;
    }
    
    /*private double getChangedIntensity() {
        double newIntensity = intensity1;
        
        switch(simMode) {     
        case intensify:
            newIntensity = intensity1*(1+tumorChange);
            break;
        case deintensify:
            newIntensity = intensity1*(1-tumorChange);
            break;
        case none:
        case grow:
        case shrink:
        default:
            newIntensity = intensity1;
            
        }
        
        return newIntensity;
    }*/

    private double defineLargerRadius() {
        
        double largerRadius = 0;
        switch(simMode) {
        case grow:
            largerRadius = (initRadius*(1+tumorChange));
            break;
            
        case shrink:
            largerRadius = initRadius;
            break;
            
        case none:
        //case intensify:
        //case deintensify:
        default:
            largerRadius = initRadius;
            
        }
        
        return largerRadius;
    }

    private void setBasicInfo(FileInfoImageXML fileInfo, DataType type) {
        fileInfo.setDataType(type.getLegacyNum());
        fileInfo.setExtents(new int[]{xyDim, xyDim, zDim});
        fileInfo.setUnitsOfMeasure(new Unit[]{Unit.MILLIMETERS, Unit.MILLIMETERS, Unit.MILLIMETERS});
        fileInfo.setResolutions(new float[]{(float) xyRes, (float) xyRes, (float) zRes});
        fileInfo.setEndianess(false);
        fileInfo.setOffset(0);
        fileInfo.setFileDirectory(Preferences.getPreferencesDir());
    }
    
    public ModelImage getImage1a() {
        return image1a;
    }

    public ModelImage getImage2a() {
        return image2a;
    }
    
public ModelImage getImage1aTumor() {
        return image1aTumor;
    }

    public ModelImage getImage2aTumor() {
        return image2aTumor;
    }

private class RayleighRandom extends Random {
        
        /**
         * Outputs next number of rayleigh distribution with given scale and minimum of 0, the use of nextNext
         * and synchronized helps thread performance and is based of java.util.Random source code.
         * 
         * @param scale
         * @param min
         * @return
         */
        synchronized public double nextRayleigh(double scale, double min) {
            double x = nextDouble();
            
            double sub = x-min;
            
            scale = scale*scale;
            
            double f = (sub/scale)*Math.exp(-(sub*sub)/(2*scale*scale));
            
            return f;
        }
        
        public double gaussian(double x, double mean, double sigma) {
            return (1.0/(Math.sqrt(2*Math.PI)*sigma))*Math.exp(-(((x-sigma)*(x-sigma)))/(2.0*sigma*sigma));
        }
        
        synchronized public double nextGaussianSimple() {
            double x = nextDouble()*2-1;
            System.out.println("X: "+x);
            double sigma = 1;
            double mean = 0;
            return (1.0/(Math.sqrt(2*Math.PI)*sigma))*Math.exp(-Math.pow(x-mean, 2)/(2.0*Math.pow(sigma, 2)));
        }
    }
    
    public static void main(String[] args) {
        PlugInAlgorithmCreateTumorMap543a p = new PlugInAlgorithmCreateTumorMap543a();
        
        long time = System.currentTimeMillis();
        System.out.println("Start");
        DecimalFormat dec = new DecimalFormat("0.##");
        RayleighRandom r = p.new RayleighRandom();
        int[] rHit = new int[1000];
        for(int i=0; i<196608; i++) {
            double ray = r.nextGaussianSimple();
            //System.out.println(ray);
            long loc = Math.round((ray*100)+500);
            if(ray < -3 || ray > 3) {
                System.out.println(loc+", "+ray);
            }
            //System.out.println(r.nextRayleigh(.32, 0.0));
            rHit[(int) loc]++;
        }
        System.out.println("Start");
        for(int i=0; i<rHit.length; i++) {
            System.out.println(rHit[i]);
        }
        
        System.out.println("Time: "+(System.currentTimeMillis() - time));
    }
}
