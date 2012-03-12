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

public class PlugInAlgorithmCreateTumorMap541c extends AlgorithmBase {

    /** Image dimensions */
    private int xyDim, zDim;
    /** Image resolutions */
    private double xyRes, zRes;
    /** Tumor radius in units of image */
    private double initRadius;
    private double tumorChange;
    private PlugInDialogCreateTumorMap541c.TumorSimMode simMode;
    /** Center of created sphere */
    private int xCenter, yCenter, zCenter;
    private double intensity;
    /** Result images */
    private ModelImage image1a, image2a;
    private int[][] sphere;
    private int subsampleAmount;
    /** Whether boundary checking is necessary during sphere population */
    private boolean doBoundCheck;
    /** Whether sphere should be populated entirely within field of view */
    private boolean doCenter;
    
    

    /**
     * Constructor.
     * @param intensity 
     * @param subsample 
     *
     */
	public PlugInAlgorithmCreateTumorMap541c(int xyDim, int zDim, double xyRes,
            double zRes, double initRadius, double tumorChange,
            PlugInDialogCreateTumorMap541c.TumorSimMode simMode, double intensity, int subsampleAmount, boolean doCenter) {
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
        this.intensity = intensity;
        this.subsampleAmount = subsampleAmount;
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
        
        populateSphere(initRadius, intensity, image1a);
        populateSphere(getChangedRadius(), getChangedIntensity(), image2a);
        
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
        
        image1a.calcMinMax();
        image2a.calcMinMax();
        
        if(subsampleAmount != 0) {
            xCenter = (int) Math.round(((double)xCenter)/subsampleAmount);
            yCenter = (int) Math.round(((double)yCenter)/subsampleAmount);
            zCenter = (int) Math.round(((double)zCenter)/subsampleAmount);
        }
        
        Preferences.debug("Center of tumor subsampled: "+xCenter+", "+yCenter+", "+zCenter+"\n");
        Preferences.data("Center of tumor subsampled: "+xCenter+", "+yCenter+", "+zCenter+"\n");
        
    	setCompleted(true); //indicating to listeners that the algorithm completed successfully

    } // end runAlgorithm()

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
            newRadius = (int) (initRadius*(1+tumorChange));
            break;
            
        case shrink:
            newRadius = (int) (initRadius*(tumorChange));
            break;
            
        case none:
        case intensify:
        case deintensify:
        default:
            newRadius = initRadius;
            
        }
        
        return newRadius;
    }
    
    private double getChangedIntensity() {
        double newIntensity = intensity;
        
        switch(simMode) {     
        case intensify:
            newIntensity = intensity*(1+tumorChange);
            break;
        case deintensify:
            newIntensity = intensity*tumorChange;
            break;
        case none:
        case grow:
        case shrink:
        default:
            newIntensity = intensity;
            
        }
        
        return newIntensity;
    }

    private double defineLargerRadius() {
        
        double largerRadius = 0;
        switch(simMode) {
        case grow:
            largerRadius = (int) (initRadius*(1+tumorChange));
            break;
            
        case shrink:
            largerRadius = initRadius;
            break;
            
        case none:
        case intensify:
        case deintensify:
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
}
