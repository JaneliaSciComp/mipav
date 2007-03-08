import gov.nih.mipav.model.file.FileCheshire;
import gov.nih.mipav.model.file.FileCheshireVOI;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.plugins.*; // needed to load PlugInAlgorithm / PlugInView / PlugInFile interface
import gov.nih.mipav.view.*;
import java.awt.*;
import java.io.File;
import java.io.IOException;
import java.util.Vector;


/**
 * Converts cheshire overlays in the given file to VOIs.
 * 
 * @see  PlugInAlgorithm
 */

// This is a Generic type of PlugIn which does not require a source image to run.
public class PlugInCheshireVOI implements PlugInGeneric {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Default x dimension for modelImage. */
    public static final int DEFAULT_X_DIM = 2;
    
    /** Default y dimension for modelImage. */
    public static final int DEFAULT_Y_DIM = 2;
    
    /** Default z dimension for modelImage. */
    public static final int DEFAULT_Z_DIM = 2;

    //~ Instance fields ------------------------------------------------------------------------------------------------
    
    /**Cheshire overlay files to process. */
    private Vector cheshireFiles;
    
    /** Image where converted cheshire overlays are stored. */
    private ModelImage cheshireComposite;
    
    /**Dialog for this plugin. */
    private PlugInDialogCheshireVOI cheshireDialog;
    
    /** The user interface */
    private ViewUserInterface UI;
    
    /** Number of cheshire files loaded. */
    private int filesLoaded;
    
    
    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Defines body of run method, which was declared in the interface. Run method converts cheshire overlays in
     * the given file to VOIs.
     *
     *
     * @see    ViewUserInterface
     * @see    ModelImage
     * @see    ViewJFrameImage
     */
    public void run() {

        UI = ViewUserInterface.getReference();
        cheshireDialog = new PlugInDialogCheshireVOI(false, this);
        cheshireFiles = cheshireDialog.getCheshireFiles();
        filesLoaded = 0;
        int[] dimExtents = new int[3];
        dimExtents[0] = DEFAULT_X_DIM;
        dimExtents[1] = DEFAULT_Y_DIM;
        dimExtents[2] = DEFAULT_Z_DIM;
        cheshireComposite = new ModelImage(ModelImage.FLOAT, dimExtents, "Cheshire Composite", UI);
        
    }
    
    /**
     * Runs the plugin
     *
     */
    
    public void runPlugin() {
        if(cheshireDialog.isSuccessfulExit()) {
            FileCheshireVOI[] cheshireArray = new FileCheshireVOI[cheshireFiles.size()];
            String[] cheshireNames = new String[cheshireFiles.size()];
            ViewJProgressBar progressBar = new ViewJProgressBar("Cheshire Overlay Loading", "loading cheshire files...", 0, 100, true, null, null);
            progressBar.setLocation((int) Toolkit.getDefaultToolkit().getScreenSize().getWidth() / 2, 50);
            VOIVector voiListVec = new VOIVector();
            float highX = 0, highY = 0, highZ = 0;
            if(cheshireFiles.size() == 0) {
                MipavUtil.displayError("No cheshire overlay files found in directory.");
                progressBar.setVisible(false);
                return;
            }
            for(int i=0; i<cheshireFiles.size(); i++) {
               File tempFile = ((File)cheshireFiles.get(i));
               File tryFile = new File(tempFile.getAbsolutePath().substring(0, tempFile.getAbsolutePath().lastIndexOf("."))+".imc");
               File secondTry = new File(tempFile.getAbsolutePath().substring(0, tempFile.getAbsolutePath().lastIndexOf("."))+".img");
               VOI[] voiListArr = null;
               ModelImage tempImage;// = new ModelImage(ModelImage.BYTE, dimExtentsTemp, "Cheshire Pretend", UI);
               try {
                   if(tryFile.exists()) {
                       FileCheshire tempCheshire = new FileCheshire(UI, tryFile.getName(), tryFile.getParent()+tryFile.separatorChar, false);
                       tempImage = tempCheshire.readImage();
                   }
                   else if(secondTry.exists()) {
                       FileCheshire tempCheshire = new FileCheshire(UI, secondTry.getName(), secondTry.getParent()+secondTry.separatorChar, false);
                       tempImage = tempCheshire.readImage();
                   }
                   else {
                       tempImage = cheshireComposite;
                   }
                   cheshireArray[i] = new FileCheshireVOI(tempFile.getName(), tempFile.getParent()+"\\", tempImage);
                   cheshireNames[i] = tempFile.getName();    
                   voiListArr = cheshireArray[i].readVOI();
               }
               catch (IOException e) {
                   MipavUtil.displayError("Error reading VOIs for image "+tempFile.getName());
                   return;
               }
               if(voiListArr != null) {
                   for(int j=0; j<voiListArr.length; j++) {
                       Point3Df[] extrema = voiListArr[j].maxWidth();
                       for(int k=0; k<extrema.length; k++) {
                           if(extrema[k].x > highX) {
                               highX = extrema[k].x;
                           }
                           if(extrema[k].y > highY) {
                               highY = extrema[k].y;
                           }
                           if(extrema[k].z > highZ) {
                               highZ = extrema[k].z;
                           }
                       }
                   }
                   for(int j=0; j<voiListArr.length; j++) {
                       voiListVec.add(voiListArr[j]);
                   }
               }
               
            }
            int[] dimExtents = new int[3];
            dimExtents[0] = ((int)(highX*1.5));
            dimExtents[1] = ((int)(highY*1.5));
            dimExtents[2] = ((int)(highZ*1.5));
            
            ModelImage newImage = new ModelImage(ModelImage.BYTE, dimExtents, "Cheshire Composite", UI);
            new ViewJFrameImage(newImage);
           
            for(int i=0; i< voiListVec.size(); i++) {
                VOI temp = new VOI((short)i, cheshireNames[i], (int)(highZ*1.5));
                VOI oldVOI = ((VOI)voiListVec.get(i));
                Vector[] vec = oldVOI.getCurves();
                for(int j=0; j<vec.length; j++) {
                    for(int k=0; k<vec[j].size(); k++) {
                        temp.importCurve(((VOIContour)vec[j].get(k)), j);
                    }
                }
                newImage.registerVOI(temp);
            }
            
            progressBar.setVisible(false);
            
        }
        
        else {
            //Do nothing since individual error is already displayed.
        }
    }
}