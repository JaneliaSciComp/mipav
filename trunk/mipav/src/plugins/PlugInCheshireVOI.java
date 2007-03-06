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
    public static final int DEFAULT_X_DIM = 600;
    
    /** Default y dimension for modelImage. */
    public static final int DEFAULT_Y_DIM = 600;
    
    /** Default z dimension for modelImage. */
    public static final int DEFAULT_Z_DIM = 150;

    //~ Instance fields ------------------------------------------------------------------------------------------------
    
    /**Cheshire overlay files to process. */
    private Vector cheshireFiles;
    
    /** Image where converted cheshire overlays are stored. */
    private ModelImage cheshireComposite;
    
    /**Frame for cheshireImage. */
    private ViewJFrameImage cheshireFrame;
    
    /**Dialog for this plugin. */
    private PlugInDialogCheshireVOI cheshireDialog;
    
    /** The user interface */
    private ViewUserInterface UI;
    
    
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
        int[] dimExtents = new int[3];
        dimExtents[0] = DEFAULT_X_DIM;
        dimExtents[1] = DEFAULT_Y_DIM;
        dimExtents[2] = DEFAULT_Z_DIM;
        cheshireComposite = new ModelImage(ModelImage.IMAGE_A, dimExtents, "Cheshire Composite", UI);
        
    }
    
    /**
     * Runs the plugin
     *
     */
    
    public void runPlugin() {
        if(cheshireDialog.isSuccessfulExit()) {
            FileCheshireVOI[] cheshireArray = new FileCheshireVOI[cheshireFiles.size()];
            VOIVector voiListVec = new VOIVector();
            float highX = 0, highY = 0, highZ = 0;
            for(int i=0; i<cheshireFiles.size(); i++) {
               File tempFile = ((File)cheshireFiles.get(i));
               File tryFile = new File(tempFile.getAbsolutePath().substring(0, tempFile.getAbsolutePath().lastIndexOf("."))+".imc");
               System.out.println(tryFile.getName());
               System.out.println(tryFile.getParent());
               VOI[] voiListArr = null;
               ModelImage tempImage;
               try {
                   if(tryFile.exists()) {
                       FileCheshire tempCheshire = new FileCheshire(UI, tryFile.getName(), tryFile.getParent()+tryFile.separatorChar, false);
                       tempImage = tempCheshire.readImage();
                   }
                   else {
                       tempImage = cheshireComposite;
                   }
                   cheshireArray[i] = new FileCheshireVOI(tempFile.getName(), tempFile.getParent()+"\\", tempImage);
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
                               System.out.println("HighX:\t"+highX);
                           }
                           if(extrema[k].y > highY) {
                               highY = extrema[k].y;
                               System.out.println("HighY:\t"+highY);
                           }
                           if(extrema[k].z > highZ) {
                               highZ = extrema[k].z;
                               System.out.println("HighZ:\t"+highZ);
                           }
                       }
                   }
                   for(int j=0; j<voiListArr.length; j++) {
                       voiListVec.add(voiListArr[j]);
                   }
               }
               cheshireArray[i].fireProgressStateChanged(100);
               
            }
            int[] dimExtents = new int[3];
            dimExtents[0] = ((int)(highX+2));
            dimExtents[1] = ((int)(highY+2));
            dimExtents[2] = ((int)(highZ+2));
            
            cheshireComposite.setExtents(dimExtents);
            cheshireFrame = new ViewJFrameImage(cheshireComposite);
            
            cheshireComposite.addVOIs(voiListVec);
            
        }
        
        else {
            //Do nothing since individual error is already displayed.
        }
           
    
    
    }
}