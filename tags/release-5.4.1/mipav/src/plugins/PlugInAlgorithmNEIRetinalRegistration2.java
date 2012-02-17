import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmCenterOfMass;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.file.FileVOI;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.VOI;

import gov.nih.mipav.view.MipavUtil;

import java.awt.Point;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.BitSet;

import javax.swing.JTextArea;

/**
 * This algorithm takes in a image and voi location. Using the VOI the center of mass is found. From that point an azimutal 
 * average is taken for each pixel (as a radius) until the radius is 475. The values are saved in a .txt file as CSV
 * with the first value as radius times pixels per degrees and the second as the average intensity.
 * @author morseaj
 *
 */
public class PlugInAlgorithmNEIRetinalRegistration2 extends AlgorithmBase {

	
    /** mpMap File **/
    private File mpMapFile;
    
    /** VOI File **/
    private File voiFile;
    
    
    /** dots per pixel value **/
    private float dPerp;
    
    /** output box **/
    private JTextArea outputbox;
    
    /** IO **/
    private FileIO fileIO = new FileIO();
    
    
    /** center of mass **/
    private Point cm;
    
    /** finds center of mass **/
    private AlgorithmCenterOfMass algoFindCM;
    
    /** sees if value has been used in calculating data already **/
    private BitSet check;
    
    
    public  PlugInAlgorithmNEIRetinalRegistration2(){   
    }
    /**
     * mpMapFile is where the MPmap is
     *voiLoc is where the VOI of the fovea is
     *dPerp is degree per pixel and is used as a multiplication factor for the radius value
     *
     */
    public PlugInAlgorithmNEIRetinalRegistration2(String mpMapLoc, String voiLoc, float dPerp, JTextArea outbox2){
        
        //save user input
        mpMapFile = new File(mpMapLoc);
        voiFile = new File(voiLoc);
        this.outputbox = outbox2;
        this.dPerp = dPerp;
        
    }
    @Override
    /**
     * Load mpMap and register VOI to it.
     *
     *Using VOi find center of mass of voi, save this as Point cm
     *
     *check is a bitset used to see if the pixel was already included in the average
     *
     *for each radius which is from 1 to 475
     *    for each value of theta (0 - 360 with step = .1)
     *        calculate the location of the pixel in rect coords using cm as origin
     *        see if the point (current) has already be used using bitset
     *        if not then add it to the total and add one to the denominator
     *                if it is then ignore it and increase step
     *    save the average of the perimeter to file data.txt with radius as radius time dPerp
     */
    public void runAlgorithm() {

        outputbox.append("*Beginning MPmap Analysis* \n");
        fileIO.setQuiet(true);
        
        //read in mpMap and VOI
        ModelImage image = fileIO.readImage(mpMapFile.getAbsolutePath());
        FileVOI voiIO;
        try {
            voiIO = new FileVOI(voiFile.getName(), voiFile.getParent() + File.separator, image);

        outputbox.append("****Loading image w/ fovea VOI \n");
        VOI voi[] = voiIO.readVOI(false);
        
        for (int i = 0; i < voi.length; i++) {
            image.registerVOI(voi[i]);
        }
        
        } catch (IOException e) {
            MipavUtil.displayError("Error loading VOI to MPmap!");
            setCompleted(true);
            notifyListeners(this);
            return;
        }
        
        //calculate center of mass of VOI
        float threshold[] = new float[2];
        threshold[0] = (float)image.getMin();
        threshold[1] = (float)image.getMax();
        outputbox.append("****Calculating Center of Mass \n");
        algoFindCM = new AlgorithmCenterOfMass(image, threshold, false);
        algoFindCM.run();
        
        //save center
        cm = new Point((int)((float)algoFindCM.getCenterOfMass()[0]/image.getResolutions(0)[0]),(int)((float)algoFindCM.getCenterOfMass()[1]/image.getResolutions(0)[1]));
        Point current;
        
        
        float[] buffer = new float[image.getExtents()[0] * image.getExtents()[1]];
        int xDim = image.getExtents()[0], pos = 0;
        check = new BitSet(image.getExtents()[0] * image.getExtents()[1]);
        
        //get image data
        try {
            image.exportData(0, image.getExtents()[0] * image.getExtents()[1], buffer);
        } catch (IOException e) {
        }
        
        
        float total = 0, denom = 0;
        File data = new File(mpMapFile.getParent() + File.separatorChar + "data.txt");
        try {
            data.createNewFile();
            FileWriter out = new FileWriter(data);
            
            outputbox.append("****Calculating Azimuthal Average \n");
            
            
                for (int r = 1; r <=475; r++)  {
                    check.clear();
                        total = 0;
                    denom = 0;
                for(float theta = 0; theta < 360; theta=theta + (float).1){
                    
                    if (isThreadStopped()) {
                        return;
                    }
                    
                    //get point in relation to center of mass
                    current = new Point((int)(r*Math.sin(theta)), (int)(r*Math.cos(theta)));
                    current.translate((int)cm.getX(), (int)cm.getY());
                    
                    pos = (int)(current.getY()*xDim + current.getX());
                    
                    //if NaN dont add it
                    if (!check.get(pos) && !Float.isNaN(buffer[pos])){
                        total = total + buffer[pos];
                        denom++;
                        check.set(pos, true);
                    }
    
                }
                //divide by total values added
                total = total / denom;
                
                //print to file
                out.write((r*dPerp) + "," + total + "\r\n");
                    
        
        }
        outputbox.append("****Saving data in " + data.getPath() + " \n");
        out.close();
        
        if (image != null){
            image.disposeLocal();
            image = null;
        }
        
    } catch (IOException e) {
        MipavUtil.displayError("FileIO Error!");
    }
        outputbox.append("*End MPmap Analysis* \n");
        setCompleted(true);
        return;
        
    }
    

}
