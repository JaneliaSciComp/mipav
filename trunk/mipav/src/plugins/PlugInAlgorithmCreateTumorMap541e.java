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

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.filters.AlgorithmGaussianBlur;
import gov.nih.mipav.model.file.FileInfoBase.Unit;
import gov.nih.mipav.model.file.FileInfoImageXML;
import gov.nih.mipav.model.file.FileInfoXML;
import gov.nih.mipav.model.file.FileUtility;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase.DataType;
import gov.nih.mipav.util.CircleUtil;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
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

public class PlugInAlgorithmCreateTumorMap541e extends AlgorithmBase {

    public static final String INTENSITY1 = "Intensity1: ";
    public static final String INTENSITY2 = "Intensity2: ";
    public static final String NOISE_LEVEL = "Adding noise level: ";
    
    /** Image dimensions */
    private int xyDim, zDim;
    /** Image resolutions */
    private double xyRes, zRes;
    /** Tumor radius in units of image */
    private double initRadius;
    private double tumorChange;
    private PlugInDialogCreateTumorMap541e.TumorSimMode simMode;
    /** Center of created sphere */
    private int xCenter, yCenter, zCenter;
    private double intensity1, intensity2;
    /** Result images */
    private ModelImage image1a, image2a;
    private int[][] sphere;
    private int subsampleAmount;
    /** Whether boundary checking is necessary during sphere population */
    private boolean doBoundCheck;
    /** Whether sphere should be populated entirely within field of view */
    private boolean doCenter;
    /** Percent Rayleigh-distributed noise */
    private double noisePercent;
    
    
    public PlugInAlgorithmCreateTumorMap541e() {
        
    }
    
    
    /**
     * Constructor.
     * @param intensity1 
     * @param intensity2 
     * @param noisePercent 
     * @param subsample 
     *
     */
	public PlugInAlgorithmCreateTumorMap541e(int xyDim, int zDim, double xyRes,
            double zRes, double initRadius, double tumorChange,
            PlugInDialogCreateTumorMap541e.TumorSimMode simMode, double intensity1, double intensity2, int subsampleAmount, boolean doCenter, double noisePercent) {
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
        this.intensity2 = intensity2;
        
        this.subsampleAmount = subsampleAmount;
        
        this.noisePercent = noisePercent;
    }
    
	//  ~ Methods --------------------------------------------------------------------------------------------------------

        
    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        destImage = null;
        srcImage = null;
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
        
        setBasicInfo(fileInfoImage1);
        setBasicInfo(fileInfoImage2);
        
        image1a = ViewUserInterface.getReference().createBlankImage(fileInfoImage1);
        image1a.getParentFrame().setVisible(false);
        image1a.setImageName("image1a");
        image2a = ViewUserInterface.getReference().createBlankImage(fileInfoImage2);
        image2a.getParentFrame().setVisible(false);
        image2a.setImageName("image2a");
        
        int xyLargerRadius = (int)Math.ceil(defineLargerRadius()/xyRes);
        
        doBoundCheck = false;
        Random r = new Random(); 
        if(!doCenter) {
        	xCenter = r.nextInt(xyDim);
            yCenter = r.nextInt(xyDim);
            doBoundCheck = true;
        } else { //ensures entire tumor is inside image
        	xCenter = r.nextInt(xyDim-2*xyLargerRadius)+xyLargerRadius;
            yCenter = r.nextInt(xyDim-2*xyLargerRadius)+xyLargerRadius;
        }
        
        int zLargerRadius = (int)Math.ceil(defineLargerRadius()/zRes);
        
        if(!doCenter) {
        	zCenter = r.nextInt(zDim);
        } else {
        	zCenter = r.nextInt(zDim-2*zLargerRadius)+zLargerRadius;
        	doBoundCheck = true;
        }
        
        Preferences.debug("Center of tumor: "+xCenter+", "+yCenter+", "+zCenter+"\n");
        Preferences.data("Center of tumor: "+xCenter+", "+yCenter+", "+zCenter+"\n");
        
        Preferences.data(INTENSITY1+intensity1+";\n");
        Preferences.data(INTENSITY2+intensity2+";\n");
        
        populateSphere(initRadius, intensity1, image1a);      
        populateSphere(getChangedRadius(), intensity2, image2a);
        
        if(subsampleAmount != 0) {
            image1a = subsample(image1a);       
            image2a = subsample(image2a);
        }
        
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
        
        if(noisePercent != 0) {
            generateNoise(image1a, intensity1);
            generateNoise(image2a, intensity2);
        }
        Preferences.data(NOISE_LEVEL+noisePercent+";\n");
        
        image1a.calcMinMax();
        image2a.calcMinMax();
        
    	setCompleted(true); //indicating to listeners that the algorithm completed successfully

    } // end runAlgorithm()

    private void generateNoise(ModelImage image, double intensity) {
        Random r = new Random();
        double threshold = intensity*noisePercent;
        for(int i=0; i<image.getDataSize(); i++) {
            if(image.get(i).doubleValue() < threshold) {
                image.set(i, r.nextGaussian()*intensity*noisePercent);
            } else {
                image.set(i, image.get(i).doubleValue() + image.get(i).doubleValue()*(r.nextGaussian()*noisePercent));                //TODO: make Rayleigh distributed
            }
        }
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

    private void populateSphere(double radius, double intensity, ModelImage image) {
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
            image.set(sphere[i][0], sphere[i][1], sphere[i][2], intensity);
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

    private void setBasicInfo(FileInfoImageXML fileInfo) {
        fileInfo.setDataType(DataType.FLOAT.getLegacyNum());
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
        PlugInAlgorithmCreateTumorMap541e p = new PlugInAlgorithmCreateTumorMap541e();
        
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
