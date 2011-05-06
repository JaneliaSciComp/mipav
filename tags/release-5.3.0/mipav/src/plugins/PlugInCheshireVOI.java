import WildMagic.LibFoundation.Mathematics.Vector3f;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.plugins.*; // needed to load PlugInAlgorithm / PlugInView / PlugInFile interface

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.ActionEvent;

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

    /** Image where converted cheshire overlays are stored. */
    private ModelImage cheshireComposite;

    /** Dialog for this plugin. */
    private PlugInDialogCheshireVOI cheshireDialog;

    /** Cheshire overlay files to process. */
    private Vector<File> cheshireFiles;

    /** JFrameImage for the collected VOIs. */
    private ViewJFrameImage imageFrame;

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Defines body of run method, which was declared in the interface. Run method converts cheshire overlays in the
     * given file to VOIs.
     *
     * @see  ViewUserInterface
     * @see  ModelImage
     * @see  ViewJFrameImage
     */
    @SuppressWarnings("unchecked")
    public void run() {
        cheshireDialog = new PlugInDialogCheshireVOI(false, this);
        cheshireFiles = cheshireDialog.getCheshireFiles();
        int[] dimExtents = new int[3];
        dimExtents[0] = DEFAULT_X_DIM;
        dimExtents[1] = DEFAULT_Y_DIM;
        dimExtents[2] = DEFAULT_Z_DIM;
        cheshireComposite = new ModelImage(ModelStorageBase.FLOAT, dimExtents, "Cheshire Composite");

    }

    /**
     * Runs the plugin.
     */

    public void runPlugin() {

        if (cheshireDialog.isSuccessfulExit()) {
            FileCheshireVOI[] cheshireArray = new FileCheshireVOI[cheshireFiles.size()];
            String[] cheshireNames = new String[cheshireFiles.size()];
            ViewJProgressBar progressBar = new ViewJProgressBar("Cheshire Overlay Loading", "loading cheshire files...",
                                                                0, 100, true, null, null);
            progressBar.setLocation((int) Toolkit.getDefaultToolkit().getScreenSize().getWidth() / 2, 50);

            VOIVector voiListVec = new VOIVector();
            float highX = 0, highY = 0, highZ = 0;

            if (cheshireFiles.size() == 0) {
                MipavUtil.displayError("No cheshire overlay files found in directory.");
                progressBar.setVisible(false);

                return;
            }

            ModelImage tempImage = null; // = new ModelImage(ModelImage.BYTE, dimExtentsTemp, "Cheshire Pretend", UI);

            for (int i = 0; i < cheshireFiles.size(); i++) {
                File tempFile = cheshireFiles.get(i);
                File tryFile = new File(tempFile.getAbsolutePath().substring(0,
                                              tempFile.getAbsolutePath().lastIndexOf(".")) +".imc");
                File secondTry = new File(tempFile.getAbsolutePath().substring(0,
                                               tempFile.getAbsolutePath().lastIndexOf(".")) +".img");
                VOI[] voiListArr = null;

                try {

                    if (tryFile.exists()) {
                        FileCheshire tempCheshire = new FileCheshire(tryFile.getName(),
                                                                     tryFile.getParent() + File.separatorChar,
                                                                     false);
                        tempImage = tempCheshire.readImage();
                    } else if (secondTry.exists()) {
                        FileCheshire tempCheshire = new FileCheshire(secondTry.getName(),
                                                                     secondTry.getParent() + File.separatorChar,
                                                                     false);
                        tempImage = tempCheshire.readImage();
                    } else {
                        tempImage = cheshireComposite;
                    }

                    cheshireArray[i] = new FileCheshireVOI(tempFile.getName(), tempFile.getParent() + File.separatorChar, tempImage);
                    cheshireNames[i] = tempFile.getName().substring(0, tempFile.getName().lastIndexOf("."));
                    voiListArr = cheshireArray[i].readVOI();
                } catch (IOException e) {
                    Preferences.debug("Error reading VOIs for image " + tempFile.getName());

                    return;
                }

                if (voiListArr != null) {

                    for (int j = 0; j < voiListArr.length; j++) {
                        Vector3f[] extrema = voiListArr[j].maxWidth(false);

                        for (int k = 0; k < extrema.length; k++) {

                            if (extrema[k].X > highX) {
                                highX = extrema[k].X;
                            }

                            if (extrema[k].Y > highY) {
                                highY = extrema[k].Y;
                            }

                            if (extrema[k].Z > highZ) {
                                highZ = extrema[k].Z;
                            }
                        }
                    }

                    for (int j = 0; j < voiListArr.length; j++) {
                        voiListVec.add(voiListArr[j]);
                    }
                }
            }

            int[] dimExtents = new int[3];
            dimExtents[0] = ((int) (highX * 1.5));
            dimExtents[1] = ((int) (highY * 1.5));
            dimExtents[2] = ((int) (highZ * 1.5));

            ModelImage newImage = new ModelImage(ModelStorageBase.BYTE, dimExtents, "Cheshire Composite");
            imageFrame = new ViewJFrameImage(newImage);

            for (int i = 0; i < voiListVec.size(); i++) {
                VOI temp = new VOI((short) i, cheshireNames[i]);
                VOI oldVOI = voiListVec.get(i);
                Vector vec = oldVOI.getCurves();

                for (int k = 0; k < vec.size(); k++) {
                    temp.importCurve(((VOIContour) vec.get(k)));
                }

                newImage.registerVOI(temp);
            }

            newImage.setFileInfo(tempImage.getFileInfo());

            imageFrame.actionPerformed(new ActionEvent(this, 0, "Save all VOIs"));

            String imageName = newImage.getImageName();
            String fileDir = newImage.getFileInfo(0).getFileDirectory();

            imageFrame.close();

            progressBar.updateValue(100);

            MipavUtil.displayInfo("VOIs saved in folder\n " + fileDir + "defaultVOIs_" + imageName);

            progressBar.setVisible(false);

        } else {
            // Do nothing since individual error is already displayed.
        }
    }
}
