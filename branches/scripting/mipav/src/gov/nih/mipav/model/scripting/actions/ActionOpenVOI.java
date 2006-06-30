package gov.nih.mipav.model.scripting.actions;


import java.io.File;
import java.util.Vector;

import gov.nih.mipav.model.file.FileVOI;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewVOIVector;
import gov.nih.mipav.view.dialogs.AlgorithmParameters;


/**
 * A script action which opens a VOI from disk and adds it to an image.
 */
public class ActionOpenVOI implements ScriptableActionInterface {
    /**
     * The label to use for the input image parameter.
     */
    protected static final String INPUT_IMAGE_LABEL = AlgorithmParameters.getInputImageLabel(1);
    
    /**
     * The image into which the VOI was loaded should be recorded in the script.  The actual opening/adding must be done elsewhere.
     */
    private ModelImage recordingInputImage;
    
    /**
     * Constructor for the dynamic instantiation and execution of the OpenVOI script action.
     */
    public ActionOpenVOI() {}
    
    /**
     * Constructor used to record the OpenVOI script action line.
     * @param input  The image to which the VOI was added.
     */
    public ActionOpenVOI(ModelImage input) {
        recordingInputImage = input;
    }
    
    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * {@inheritDoc}
     */
    public void insertScriptLine() {
        ParameterTable parameters = new ParameterTable();
        try {
            parameters.put(ParameterFactory.newImage(INPUT_IMAGE_LABEL, recordingInputImage.getImageName()));
        } catch (ParserException pe) {
            MipavUtil.displayError("Error encountered creating parameters while recording OpenVOI script action:\n" + pe);
            return;
        }
        
        ScriptRecorder.getReference().addLine("OpenVOI", parameters);
        
        /*String imageName = getActiveImage().getImageName();

        if (userInterface.getScriptDialog().getImgTableVar(imageName) == null) {

            if (userInterface.getScriptDialog().getActiveImgTableVar(imageName) == null) {
                userInterface.getScriptDialog().putActiveVar(imageName);
            }
        }

        String imageVarName = (String) userInterface.getScriptDialog().getVar(imageName);

        userInterface.getScriptDialog().append("OpenVOI " + imageVarName);

        ViewVOIVector VOIs = (ViewVOIVector) getActiveImage().getVOIs();

        int nVOI = VOIs.size();
        int index = 0, i = 0;
        boolean foundActive = false;

        while (!foundActive && (i != nVOI)) {

            if (VOIs.VOIAt(i).isActive() == true) {
                index = i;
                foundActive = true;
            } else {
                i++;
            }
        }

        VOI currVOI = VOIs.VOIAt(index);
        String voiName = currVOI.getName();

        userInterface.getScriptDialog().putVoiVar(voiName);

        String voiVarName = (String) userInterface.getScriptDialog().getVoiVar(voiName);

        userInterface.getScriptDialog().append(" " + voiVarName + "\n");*/
    }

    /**
     * {@inheritDoc}
     */
    public void scriptRun(ParameterTable parameters) {
        parameters.getImage(INPUT_IMAGE_LABEL);
        
        /*try {
            String var1 = getNextString();
            String imgName = (String) variableTable.get(var1);
            ModelImage image = UI.getRegisteredImageByName(imgName);
            String fileName;
            String directory;
            String fileNameIn;
            int index = 0;

            currVoiIndex++;

            // Ruida
            //ViewOpenVOIUI voiUI = new ViewOpenVOIUI(UI);
            //VOI vois[], firstVoi;
            //vois = voiUI.open(image);
            //firstVoi = vois[0];
            //String voiName = firstVoi.getName();
            key = getNextString();

            // System.out.println("key = " + key + "  voiName = "  + voiName);
            // / System.out.println("key = " + key + " voiName = " + voiNames[currVoiIndex].elementAt(0));
            // voiTable.put(key, voiNames[currVoiIndex].elementAt(0));
            VOI[] voi;
            FileVOI fileVOI;

            //System.out.println("ruida voi"); for ( int j =0; j < voiNames.size(); j++ ) {
            //System.out.println("voiNames = " + voiNames.elementAt(j)); }
            
            if (parseType == VERTICAL_PARSE) {

                for (int i = 0; i < voiNames.size(); i++) {

                    for (int x = 0; x < ((Vector) (voiNames.elementAt(i))).size(); x++) {

                        // fileNameIn = (String) (voiNames.elementAt(i));
                        fileNameIn = (String) (((Vector) (voiNames.elementAt(i))).elementAt(x));
                        index = fileNameIn.lastIndexOf(File.separatorChar);
                        directory = fileNameIn.substring(0, index + 1);
                        fileName = fileNameIn.substring(index + 1, fileNameIn.length());
                        fileVOI = new FileVOI(fileName, directory, image);
                        voi = fileVOI.readVOI();

                        // image.registerVOI(voiNames[currVoiIndex].elementAt(i));
                        for (int j = 0; j < voi.length; j++) {
                            image.registerVOI(voi[j]);
                        }
                    }
                }
            } else if (parseType == HORIZONTAL_PARSE) {
                int k = 0;

                for (k = 0; k < fileNames.size(); k++) {
                    String fName = (String) (fileNames.elementAt(k));
                    int idx = fName.lastIndexOf(".");

                    if (fName.substring(0, idx).equals(imgName)) {
                        break;
                    }
                }

                for (int x = 0; x < ((Vector) (voiNames.elementAt(k))).size(); x++) {
                    fileNameIn = (String) (((Vector) (voiNames.elementAt(k))).elementAt(x));
                    index = fileNameIn.lastIndexOf(File.separatorChar);
                    directory = fileNameIn.substring(0, index + 1);
                    fileName = fileNameIn.substring(index + 1, fileNameIn.length());
                    fileVOI = new FileVOI(fileName, directory, image);
                    voi = fileVOI.readVOI();

                    // image.registerVOI(voiNames[currVoiIndex].elementAt(i));
                    for (int j = 0; j < voi.length; j++) {
                        image.registerVOI(voi[j]);
                    }
                }

            }

            image.notifyImageDisplayListeners();
        } catch (Exception e) {
            e.printStackTrace();
            MipavUtil.displayError("Error in script file near \"OpenVOI\"\n");
            isRunning = false;

            return false;
        }*/
    }
    
    /**
     * Changes the which had a VOI added to it.
     * @param inputImage  The image that had a VOI added.
     */
    public void setInputImage(ModelImage inputImage) {
        recordingInputImage = inputImage;
    }
}
