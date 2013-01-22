package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.file.FileVOI;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.scripting.parameters.ParameterDouble;
import gov.nih.mipav.model.scripting.parameters.ParameterString;
import gov.nih.mipav.model.scripting.parameters.ParameterExternalImage;
import gov.nih.mipav.model.scripting.parameters.ParameterFactory;
import gov.nih.mipav.model.scripting.parameters.ParameterImage;
import gov.nih.mipav.model.scripting.parameters.ParameterTable;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.io.File;
import java.io.IOException;
import java.io.FileNotFoundException;
import java.io.FileInputStream;
import java.io.InputStreamReader;
import java.io.BufferedReader;

import javax.swing.*;


/**
 * Dialog to get user input for 3 parameter dynamic (contrast) enhanced MRI model or SM2 model
 */
public class JDialogSM2 extends JDialogScriptableBase implements AlgorithmInterface, WindowListener, ActionDiscovery {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    //private static final long serialVersionUID;

    //~ Instance fields ------------------------------------------------------------------------------------------------
    
    private JLabel labelParamsToFit;
    
    private JButton buttonTissueFile;
    
    private JTextField textTissueFile;
    
    private String directoryTissue;
    
    private String fileNameTissue;
    
    private ModelImage tissueImage = null;
    
    private JLabel labelVOI;
    
    private JButton buttonVOIFile;
    
    private JTextField textVOIFile;
    
    private String directoryVOI;
    
    private String fileNameVOI;
    
    private JButton buttonVOIFile2;
    
    private JTextField textVOIFile2;
    
    private String directoryVOI2 = null;
    
    private String fileNameVOI2 = null;
    
    private JButton buttonTimesFile;
    
    private JTextField textTimesFile;
    
    private String directoryTimes;
    
    private String fileNameTimes;
    
    private File fileTimes;
    
    private int numTimes;
    
    private double timeVals[];
    
    private ViewVOIVector VOIs;


    /** DOCUMENT ME! */
    private AlgorithmSM2 sm2Algo = null;

    /** DOCUMENT ME! */
    private ModelImage image;
    
    private ModelImage resultImage[] = null;
    
    private JLabel labelMinConstr0;
    
    private JTextField textMinConstr0;
    
    private JLabel labelMaxConstr0;
    
    private JTextField textMaxConstr0;
    
    private JLabel labelInitial0;
    
    private JTextField textInitial0;
    
    private JLabel labelMinConstr1;
    
    private JTextField textMinConstr1;
    
    private JLabel labelMaxConstr1;
    
    private JLabel labelInitial1;
    
    private JTextField textInitial1;
    
    private JTextField textMaxConstr1;
    
    private JLabel labelMinConstr2;
    
    private JTextField textMinConstr2;
    
    private JLabel labelMaxConstr2;
    
    private JTextField textMaxConstr2;
    
    private JLabel labelInitial2;
    
    private JTextField textInitial2;
    
    private double min_constr[] = new double[3];
    private double max_constr[] = new double[3];
    private double initial[] = new double[3];
    
    private JLabel labelHematocrit;
    private JTextField textHematocrit;
    private double hematocrit = 0.4;
    
    private FileVOI fileVOI;
    private VOI[] voi;
    private FileVOI fileVOI2;
    private VOI[] voi2;
    private BufferedReader br;
    
    private ButtonGroup VOIGroup;
    private JRadioButton sagittalSinusButton;
    private JRadioButton processRegionButton;
    private int sagittalSinusIndex;
    private int processRegionIndex;
    
    private ViewJComponentEditImage componentImage;
    
    /** DOCUMENT ME! */
    private ViewUserInterface UI;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new JDialogSM2 object.
     *
     * @param  image  DOCUMENT ME!
     */
    public JDialogSM2(ModelImage image) {
        super();
        this.UI = ViewUserInterface.getReference();
        this.image = image;
        parentFrame = image.getParentFrame();
        componentImage = ((ViewJFrameImage) parentFrame).getComponentImage();
    }

    /**
     * Creates new dialog.
     *
     * @param  theParentFrame  Parent frame
     * @param  im              Source image
     */
    public JDialogSM2(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        image = im;
        componentImage = ((ViewJFrameImage) theParentFrame).getComponentImage();
        UI = ViewUserInterface.getReference();
        init();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Closes dialog box when the OK button is pressed and calls the algorithm.
     *
     * @param  event  Event that triggers function.
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();
        Object source = event.getSource();

        if (command.equals("OK")) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Script")) {
            callAlgorithm();
        } else if (command.equals("Help")) {
            //MipavUtil.showHelp("");
        } else if ((source == sagittalSinusButton) || (source == processRegionButton)) {
            if (sagittalSinusButton.isSelected()) {
                componentImage.getVOIHandler().newVOI(0.0f); // red
                //componentImage.setCursorMode(ViewJComponentEditImage.NEW_VOI);
                //componentImage.getVOIHandler().setPresetHue(0.0f); // red
            } else if (processRegionButton.isSelected()) {
                componentImage.getVOIHandler().newVOI(1.0f / 3.0f); // green
                //componentImage.setCursorMode(ViewJComponentEditImage.NEW_VOI);
                //componentImage.getVOIHandler().setPresetHue(1.0f / 3.0f); // green
            } 
        } else if (command.equals("TissueFile")) {

                try {
                    JFileChooser chooser = new JFileChooser();

                    if (UI.getDefaultDirectory() != null) {
                        File file = new File(UI.getDefaultDirectory());

                        if (file != null) {
                            chooser.setCurrentDirectory(file);
                        } else {
                            chooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
                        }
                    } else {
                        chooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
                    }

                    chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.GEN));
                    chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.TECH));
                    chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.MICROSCOPY));
                    chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.MISC));

                    chooser.setDialogTitle("Open 3D tissue instrinsic realxivity rate image file");
                    directoryTissue = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;

                    int returnValue = chooser.showOpenDialog(UI.getMainFrame());

                    if (returnValue == JFileChooser.APPROVE_OPTION) {
                        fileNameTissue = chooser.getSelectedFile().getName();
                        directoryTissue = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;
                        UI.setDefaultDirectory(directoryTissue);
                    } else {
                        fileNameTissue = null;

                        return;
                    }

                    if (fileNameTissue != null) {
                        textTissueFile.setText(fileNameTissue);
                    }
                } catch (OutOfMemoryError e) {
                    MipavUtil.displayError("Out of memory in JDialogSM2.");

                    return;
                }
        } else if (command.equals("VOIFile")) {

            try {
                JFileChooser chooser = new JFileChooser();

                if (UI.getDefaultDirectory() != null) {
                    File file = new File(UI.getDefaultDirectory());

                    if (file != null) {
                        chooser.setCurrentDirectory(file);
                    } else {
                        chooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
                    }
                } else {
                    chooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
                }

                chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.GEN));
                chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.TECH));
                chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.MICROSCOPY));
                chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.MISC));

                chooser.setDialogTitle("Open sagittal sinus VOI file");
                directoryVOI = String.valueOf(chooser.getCurrentDirectory()) + File.separator;

                int returnValue = chooser.showOpenDialog(UI.getMainFrame());

                if (returnValue == JFileChooser.APPROVE_OPTION) {
                    fileNameVOI = chooser.getSelectedFile().getName();
                    directoryVOI = String.valueOf(chooser.getCurrentDirectory()) + File.separator;
                    UI.setDefaultDirectory(directoryVOI);
                } else {
                    fileNameVOI = null;

                    return;
                }

                if (fileNameVOI != null) {

                    try {
                        fileVOI = new FileVOI(fileNameVOI, directoryVOI, image);
                    }
                    catch (IOException e) {
                        MipavUtil.displayError("IOException on new FileVOI(fileNameVOI, directoryVOI, image)");
                        return;
                    }

                    try {
                        voi = fileVOI.readVOI(false);
                    }
                    catch (IOException e) {
                        MipavUtil.displayError("IOException on fileVOI.readVOI(false)");
                        return;
                    }
                    
                    if (voi.length > 1) {
                        MipavUtil.displayError("Found " + voi.length + " vois in file instead of 1");
                        return;
                    }
                    
                    textVOIFile.setText(fileNameVOI);
                    voi[0].setColor(0.0f);
                    image.registerVOI(voi[0]);

                    //  when everything's done, notify the image listeners
                    image.notifyImageDisplayListeners();   
                }

                
            } catch (OutOfMemoryError e) {
                MipavUtil.displayError("Out of memory in JDialogSM2.");

                return;
            }
        } else if (command.equals("VOIFile2")) {

            try {
                JFileChooser chooser = new JFileChooser();

                if (UI.getDefaultDirectory() != null) {
                    File file = new File(UI.getDefaultDirectory());

                    if (file != null) {
                        chooser.setCurrentDirectory(file);
                    } else {
                        chooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
                    }
                } else {
                    chooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
                }

                chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.GEN));
                chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.TECH));
                chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.MICROSCOPY));
                chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.MISC));

                chooser.setDialogTitle("Open process region VOI file");
                directoryVOI2 = String.valueOf(chooser.getCurrentDirectory()) + File.separator;

                int returnValue = chooser.showOpenDialog(UI.getMainFrame());

                if (returnValue == JFileChooser.APPROVE_OPTION) {
                    fileNameVOI2 = chooser.getSelectedFile().getName();
                    directoryVOI2 = String.valueOf(chooser.getCurrentDirectory()) + File.separator;
                    UI.setDefaultDirectory(directoryVOI2);
                } else {
                    fileNameVOI2 = null;

                    return;
                }

                if (fileNameVOI2 != null) {

                    try {
                        fileVOI2 = new FileVOI(fileNameVOI2, directoryVOI2, image);
                    }
                    catch (IOException e) {
                        MipavUtil.displayError("IOException on new FileVOI(fileNameVOI2, directoryVOI2, image)");
                        return;
                    }

                    try {
                        voi2 = fileVOI2.readVOI(false);
                    }
                    catch (IOException e) {
                        MipavUtil.displayError("IOException on fileVOI2.readVOI(false)");
                        return;
                    }
                    
                    if (voi2.length > 1) {
                        MipavUtil.displayError("Found " + voi2.length + " vois in file instead of 1");
                        return;
                    }
                    
                    textVOIFile2.setText(fileNameVOI2);
                    
                    voi2[0].setColor((float)(1.0/3.0));
                    image.registerVOI(voi2[0]);

                    //  when everything's done, notify the image listeners
                    image.notifyImageDisplayListeners();   
                }

                
            } catch (OutOfMemoryError e) {
                MipavUtil.displayError("Out of memory in JDialogSM2.");

                return;
            }
        } else if (command.equals("TimesFile")) {

            try {
                JFileChooser chooser = new JFileChooser();

                if (UI.getDefaultDirectory() != null) {
                    File file = new File(UI.getDefaultDirectory());

                    if (file != null) {
                        chooser.setCurrentDirectory(file);
                    } else {
                        chooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
                    }
                } else {
                    chooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
                }

                chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.GEN));
                chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.TECH));
                chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.MICROSCOPY));
                chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.MISC));

                chooser.setDialogTitle("Open file of volume center times");
                directoryTimes = String.valueOf(chooser.getCurrentDirectory()) + File.separator;

                int returnValue = chooser.showOpenDialog(UI.getMainFrame());

                if (returnValue == JFileChooser.APPROVE_OPTION) {
                    fileNameTimes = chooser.getSelectedFile().getName();
                    directoryTimes = String.valueOf(chooser.getCurrentDirectory()) + File.separator;
                    UI.setDefaultDirectory(directoryTimes);
                } else {
                    fileNameTimes = null;

                    return;
                }

                if (fileNameTimes != null) {
                	fileTimes = new File(directoryTimes + fileNameTimes);
                    
                    try {
                        br = new BufferedReader(new InputStreamReader(new FileInputStream(fileTimes)));
                    }
                    catch (FileNotFoundException e) {
                        MipavUtil.displayError((directoryTimes + fileNameTimes) + " was not found");
                        return;
                    }
                    
                    // Read lines until first character is not blank and not #
                    int ii = 0;
                    String line = null;
                    do {
                        try {
                            // Contains the contents of the line not including line termination characters
                            line = br.readLine();  
                        }
                        catch(IOException e) {
                            MipavUtil.displayError("IOException on br.readLine");
                            br.close();
                            return;
                        }
                        // have reached end of stream
                        if (line == null) {
                            MipavUtil.displayError("Have reached end of stream on br.readLine");
                            br.close();
                            return;
                        }
                        for (ii = 0; ((ii < line.length()) && (Character.isSpaceChar(line.charAt(ii)))); ii++);
                    } while ((ii == line.length()) || (line.charAt(ii) == '#'));
                    
                    int start = 0;
                    int end = 0;
                    for (; ((start < line.length()) && (Character.isSpaceChar(line.charAt(start)))); start++);
                    end = start;
                    for (; ((end < line.length()) && (Character.isDigit(line.charAt(end)))); end++);
                    if (start == end) {
                        MipavUtil.displayError("No digit starts line which should contain number of times");
                        br.close();
                        return;
                    }
                    numTimes = Integer.valueOf(line.substring(start, end)).intValue();
                    if (numTimes != image.getExtents()[3]) {
                    	MipavUtil.displayError("Number of times in file = " + numTimes + " != tDim of source image = " + image.getExtents()[3]);
                    	br.close();
                    	return;
                    }
                    timeVals = new double[numTimes];
                    int nval = 0;
                    while (true) {
                    	try {
                            // Contains the contents of the line not including line termination characters
                            line = br.readLine();  
                        }
                        catch(IOException e) {
                            MipavUtil.displayError("IOException on br.readLine");
                            br.close();
                            return;
                        }
                        // have reached end of stream
                        if (line == null) {
                            MipavUtil.displayError("Have reached end of stream on br.readLine");
                            break;
                        }
                    	start = 0;
                    	end = 0;
                    	for (; ((start < line.length()) && (Character.isSpaceChar(line.charAt(start)))); start++);
                        end = start;
                        for (; ((end < line.length()) && ((Character.isDigit(line.charAt(end))) || (line.charAt(end) == '.') || 
                                       (line.charAt(end) == 'e') || (line.charAt(end) == 'E') ||
                                       (line.charAt(end) == '+') || (line.charAt(end) == '-'))); end++);
                        if (start == end) {
                            continue;
                        }
                        timeVals[nval++] = Double.valueOf(line.substring(start, end)).doubleValue();
                        if (nval ==  numTimes) {
                            break;
                        }
                    } // while (true)
                    br.close();
                    if (nval < 1) {
                        MipavUtil.displayError("No double values found in " + fileNameTimes);
                        return;
                    }
                    if (nval < numTimes) {
                    	MipavUtil.displayError("Only " + nval + " of " + numTimes + " required times found");
                    	return;
                    }

                	textTimesFile.setText(fileNameTimes);
                }
            } catch (OutOfMemoryError e) {
                MipavUtil.displayError("Out of memory in JDialogSM2.");

                return;
            } catch (IOException e) {
            	MipavUtil.displayError("IOException on BufferedReader");
            	return;
            }

    }   else if (command.equals("Cancel")) {
    	    componentImage.getVOIHandler().setPresetHue(-1.0f);
            dispose();
        } else {
            super.actionPerformed(event);
        }
    }


    // ************************************************************************
    // ************************** Algorithm Events ****************************
    // ************************************************************************

    /**
     * This method is required if the AlgorithmPerformed interface is implemented. It is called by the algorithm when it
     * has completed or failed to to complete, so that the dialog can be display the result image and/or clean up.
     *
     * @param  algorithm  Algorithm that caused the event.
     */
    public void algorithmPerformed(AlgorithmBase algorithm) {
    	int i;

        if (algorithm instanceof AlgorithmSM2) {
            Preferences.debug("SM2 elapsed: " + algorithm.getElapsedTime());
            image.clearMask();

            if ((sm2Algo.isCompleted() == true) && (resultImage != null)) {
            	
            	for (i = 0; i < 5; i++) {

	                resultImage[i].clearMask();
	
	                try {
	                	openNewFrame(resultImage[i]);
	                 //   openNewFrame(resultImage);
	                } catch (OutOfMemoryError error) {
	                    System.gc();
	                    MipavUtil.displayError("Out of memory: unable to open new frame");
	                }
            	}
            } else if (resultImage != null) {
            	
            	for (i = 0; i < 5; i++) {

	                // algorithm failed but result image still has garbage
	                resultImage[i].disposeLocal(); // clean up memory
	                resultImage[i] = null;
            	}
            	resultImage = null;
                System.gc();

            }

            if (algorithm.isCompleted()) {
                insertScriptLine();
            }
        }
    }
    
    /**
     * Saves the default settings into the Preferences file.
     */
    public void legacySaveDefaults() {
        String delim = ",";
        String defaultsString = tissueImage.getImageFileName() + delim;
        defaultsString = min_constr[0] + delim;
        defaultsString += max_constr[0] + delim;
        defaultsString += initial[0] + delim;
        defaultsString += min_constr[1] + delim;
        defaultsString += max_constr[1] + delim;
        defaultsString += initial[1] + delim;
        defaultsString += min_constr[2] + delim;
        defaultsString += max_constr[2] + delim;
        defaultsString += initial[2] + delim;
        defaultsString += hematocrit;
        

        Preferences.saveDialogDefaults(getDialogName(), defaultsString);
    }
    
    /**
     * Set up the dialog GUI based on the parameters before running the algorithm as part of a script.
     */
    protected void setGUIFromParams() {
        image = scriptParameters.retrieveInputImage();
        tissueImage = scriptParameters.retrieveImage("tissue_image");
        if (tissueImage == null) {
            MipavUtil.displayError("Tissue image is null.");
            return;
        } else if (tissueImage.getNDims() != 3) {
            MipavUtil.displayError("Tissue image must be 3D");
            return;
        }

        for (int i = 0; i < 3; i++) {

            if (image.getExtents()[i] != tissueImage.getExtents()[i]) {
                MipavUtil.displayError("First 3 dimensions of source image must match the tissue image.");
                return;
            }
        }
        textTissueFile.setText(tissueImage.getImageFileName());
        UI = ViewUserInterface.getReference();
        parentFrame = image.getParentFrame();

        
        min_constr[0] = scriptParameters.getParams().getDouble("min_constr0");
        textMinConstr0.setText(String.valueOf(min_constr[0]));
        max_constr[0] = scriptParameters.getParams().getDouble("max_constr0");
        textMaxConstr0.setText(String.valueOf(max_constr[0]));
        initial[0] = scriptParameters.getParams().getDouble("initial0");
        textInitial0.setText(String.valueOf(initial[0]));
        min_constr[1] = scriptParameters.getParams().getDouble("min_constr1");
        textMinConstr1.setText(String.valueOf(min_constr[1]));
        max_constr[1] = scriptParameters.getParams().getDouble("max_constr1");
        textMaxConstr1.setText(String.valueOf(max_constr[1]));
        initial[1] = scriptParameters.getParams().getDouble("initial1");
        textInitial1.setText(String.valueOf(initial[1]));
        min_constr[2] = scriptParameters.getParams().getDouble("min_constr2");
        textMinConstr2.setText(String.valueOf(min_constr[2]));
        max_constr[2] = scriptParameters.getParams().getDouble("max_constr2");
        textMaxConstr2.setText(String.valueOf(max_constr[2]));
        initial[2] = scriptParameters.getParams().getDouble("initial2");
        textInitial2.setText(String.valueOf(initial[2]));
        hematocrit = scriptParameters.getParams().getDouble("hematocrit_");
        textHematocrit.setText(String.valueOf(hematocrit));
        fileNameTimes = scriptParameters.getParams().getString("file_name_times");
        directoryTimes = scriptParameters.getParams().getString("directory_times");
        if (fileNameTimes != null) {
        	try {
	        	fileTimes = new File(directoryTimes + fileNameTimes);
	            
	            try {
	                br = new BufferedReader(new InputStreamReader(new FileInputStream(fileTimes)));
	            }
	            catch (FileNotFoundException e) {
	                MipavUtil.displayError((directoryTimes + fileNameTimes) + " was not found");
	                return;
	            }
	            
	            // Read lines until first character is not blank and not #
	            int ii = 0;
	            String line = null;
	            do {
	                try {
	                    // Contains the contents of the line not including line termination characters
	                    line = br.readLine();  
	                }
	                catch(IOException e) {
	                    MipavUtil.displayError("IOException on br.readLine");
	                    br.close();
	                    return;
	                }
	                // have reached end of stream
	                if (line == null) {
	                    MipavUtil.displayError("Have reached end of stream on br.readLine");
	                    br.close();
	                    return;
	                }
	                for (ii = 0; ((ii < line.length()) && (Character.isSpaceChar(line.charAt(ii)))); ii++);
	            } while ((ii == line.length()) || (line.charAt(ii) == '#'));
	            
	            int start = 0;
	            int end = 0;
	            for (; ((start < line.length()) && (Character.isSpaceChar(line.charAt(start)))); start++);
	            end = start;
	            for (; ((end < line.length()) && (Character.isDigit(line.charAt(end)))); end++);
	            if (start == end) {
	                MipavUtil.displayError("No digit starts line which should contain number of times");
	                br.close();
	                return;
	            }
	            numTimes = Integer.valueOf(line.substring(start, end)).intValue();
	            if (numTimes != image.getExtents()[3]) {
	            	MipavUtil.displayError("Number of times in file = " + numTimes + " != tDim of source image = " + image.getExtents()[3]);
	            	br.close();
	            	return;
	            }
	            timeVals = new double[numTimes];
	            int nval = 0;
	            while (true) {
	            	try {
	                    // Contains the contents of the line not including line termination characters
	                    line = br.readLine();  
	                }
	                catch(IOException e) {
	                    MipavUtil.displayError("IOException on br.readLine");
	                    br.close();
	                    return;
	                }
	                // have reached end of stream
	                if (line == null) {
	                    MipavUtil.displayError("Have reached end of stream on br.readLine");
	                    break;
	                }
	            	start = 0;
	            	end = 0;
	            	for (; ((start < line.length()) && (Character.isSpaceChar(line.charAt(start)))); start++);
	                end = start;
	                for (; ((end < line.length()) && ((Character.isDigit(line.charAt(end))) || (line.charAt(end) == '.') || 
	                               (line.charAt(end) == 'e') || (line.charAt(end) == 'E') ||
	                               (line.charAt(end) == '+') || (line.charAt(end) == '-'))); end++);
	                if (start == end) {
	                    continue;
	                }
	                timeVals[nval++] = Double.valueOf(line.substring(start, end)).doubleValue();
	                if (nval ==  numTimes) {
	                    break;
	                }
	            } // while (true)
	            br.close();
	            if (nval < 1) {
	                MipavUtil.displayError("No double values found in " + fileNameTimes);
	                return;
	            }
	            if (nval < numTimes) {
	            	MipavUtil.displayError("Only " + nval + " of " + numTimes + " required times found");
	            	return;
	            }
	
	        	textTimesFile.setText(fileNameTimes);
        	} // try
        	catch (IOException e) {
            	MipavUtil.displayError("IOException on BufferedReader");
            	return;
            }
        	
        } // if (fileNameTimes != null)
        else {
        	MipavUtil.displayError("fileNameTimes is null");
        	return;
        }
        fileNameVOI = scriptParameters.getParams().getString("file_name_voi");
        directoryVOI = scriptParameters.getParams().getString("directory_voi");
        try {
            fileVOI = new FileVOI(fileNameVOI, directoryVOI, image);
        }
        catch (IOException e) {
            MipavUtil.displayError("IOException on new FileVOI(fileNameVOI, directoryVOI, image)");
            return;
        }

        try {
            voi = fileVOI.readVOI(false);
        }
        catch (IOException e) {
            MipavUtil.displayError("IOException on fileVOI.readVOI(false)");
            return;
        }
        
        if (voi.length > 1) {
            MipavUtil.displayError("Found " + voi.length + " vois in file instead of 1");
            return;
        }
        
        textVOIFile.setText(fileNameVOI);
        
        voi[0].setColor(0.0f);
        image.registerVOI(voi[0]);

        //  when everything's done, notify the image listeners
        image.notifyImageDisplayListeners();
        
        fileNameVOI2 = scriptParameters.getParams().getString("file_name_voi2");
        if (fileNameVOI2 != null) {
	        directoryVOI2 = scriptParameters.getParams().getString("directory_voi2");
	        try {
	            fileVOI2 = new FileVOI(fileNameVOI2, directoryVOI2, image);
	        }
	        catch (IOException e) {
	            MipavUtil.displayError("IOException on new FileVOI(fileNameVOI2, directoryVOI2, image)");
	            return;
	        }
	
	        try {
	            voi2 = fileVOI2.readVOI(false);
	        }
	        catch (IOException e) {
	            MipavUtil.displayError("IOException on fileVOI2.readVOI(false)");
	            return;
	        }
	        
	        if (voi2.length > 1) {
	            MipavUtil.displayError("Found " + voi2.length + " vois in file instead of 1");
	            return;
	        }
	        
	        textVOIFile2.setText(fileNameVOI2);
	        
	        voi2[0].setColor((float)(1.0/3.0));
	        image.registerVOI(voi2[0]);
	
	        //  when everything's done, notify the image listeners
	        image.notifyImageDisplayListeners(); 
        } // if (fileNameVOI2 != null)
    }

    /**
     * Store the parameters from the dialog to record the execution of this algorithm.
     *
     * @throws  ParserException  If there is a problem creating one of the new parameters.
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        try {
    	    scriptParameters.storeImage(tissueImage, "tissue_image");
    	}
    	catch (ParserException e) {
    		MipavUtil.displayError("Parser exception on scriptParameters.storeImage(tissueImage");
    		return;
    	}
        
        for (int i = 0; i < 5; i++) {
            scriptParameters.storeImageInRecorder(getResultImage()[i]);
        }
        
        scriptParameters.getParams().put(ParameterFactory.newParameter("min_constr0", min_constr[0]));
        scriptParameters.getParams().put(ParameterFactory.newParameter("max_constr0", max_constr[0]));
        scriptParameters.getParams().put(ParameterFactory.newParameter("initial0", initial[0]));
        scriptParameters.getParams().put(ParameterFactory.newParameter("min_constr1", min_constr[1]));
        scriptParameters.getParams().put(ParameterFactory.newParameter("max_constr1", max_constr[1]));
        scriptParameters.getParams().put(ParameterFactory.newParameter("initial1", initial[1]));
        scriptParameters.getParams().put(ParameterFactory.newParameter("min_constr2", min_constr[2]));
        scriptParameters.getParams().put(ParameterFactory.newParameter("max_constr2", max_constr[2]));
        scriptParameters.getParams().put(ParameterFactory.newParameter("initial2", initial[2]));
        scriptParameters.getParams().put(ParameterFactory.newParameter("hematocrit_",hematocrit));
        scriptParameters.getParams().put(ParameterFactory.newParameter("file_name_times", fileNameTimes));
        scriptParameters.getParams().put(ParameterFactory.newParameter("directory_times", directoryTimes));
        scriptParameters.getParams().put(ParameterFactory.newParameter("file_name_voi", fileNameVOI));
        scriptParameters.getParams().put(ParameterFactory.newParameter("directory_voi", directoryVOI));
        scriptParameters.getParams().put(ParameterFactory.newParameter("file_name_voi2", fileNameVOI2));
        scriptParameters.getParams().put(ParameterFactory.newParameter("directory_voi2", directoryVOI2));
    }

    // ************************* Item Events ****************************
    // *******************************************************************

    /**
     * itemStateChanged.
     *
     * @param  event  DOCUMENT ME!
     */
    public void itemStateChanged(ItemEvent event) {

    }

    

    /**
     * Disposes of error dialog, then frame. Sets cancelled to <code>true</code>.
     *
     * @param  event  DOCUMENT ME!
     */
    public void windowClosing(WindowEvent event) {
    	componentImage.getVOIHandler().setPresetHue(-1.0f);
        cancelFlag = true;
        dispose();
    }

    /**
     * DOCUMENT ME!
     */
    protected void callAlgorithm() {
        int i;
        try {

            // Make algorithm
        	resultImage = new ModelImage[5];
            int resultExtents[] = new int[3];
            for (i = 0; i < 3; i++) {
            	resultExtents[i] = image.getExtents()[i];
            }
            resultImage[0] = new ModelImage(ModelStorageBase.FLOAT, resultExtents, image.getImageName() + "_ktrans");
            resultImage[1] = new ModelImage(ModelStorageBase.FLOAT, resultExtents, image.getImageName() + "_ve");
            resultImage[2] = new ModelImage(ModelStorageBase.FLOAT, resultExtents, image.getImageName() + "_vp");
            resultImage[3] = new ModelImage(ModelStorageBase.FLOAT, resultExtents, image.getImageName() + "_chi_squared");
            resultImage[4] = new ModelImage(ModelStorageBase.INTEGER, resultExtents, image.getImageName() + 
            		                        "_exit_status");
            
            componentImage.getVOIHandler().setPresetHue(-1.0f);
            
            sm2Algo = new AlgorithmSM2(resultImage, image, min_constr, max_constr, initial, tissueImage, timeVals,
            		                   hematocrit, sagittalSinusIndex, processRegionIndex);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed of failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            sm2Algo.addListener(this);

            createProgressBar(image.getImageName(), sm2Algo);

            // Hide dialog
            setVisible(false);

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (sm2Algo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                sm2Algo.run();
            }
        } catch (OutOfMemoryError x) {

            System.gc();
            MipavUtil.displayError("Dialog SM2: unable to allocate enough memory");

            return;
        }
    }

    


    /**
     * Initializes GUI components and displays dialog.
     */
    private void init() {
        

        setForeground(Color.black);
        setTitle("3 - parameter SM2 model");

        JPanel mainPanel = new JPanel(new GridBagLayout());
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.insets = new Insets(3, 3, 3, 3);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridx = 0;
        
        labelParamsToFit = new JLabel("3 model parameters to fit for each voxel in 3D:");
        labelParamsToFit.setForeground(Color.black);
        labelParamsToFit.setFont(serif12);
        mainPanel.add(labelParamsToFit, gbc);
        
        labelMinConstr0 = new JLabel("K_trans minimum allowed value (1.0E-5 - 5.0)in /min");
        labelMinConstr0.setForeground(Color.black);
        labelMinConstr0.setFont(serif12);
        gbc.gridy = 1;
        mainPanel.add(labelMinConstr0, gbc);
        
        textMinConstr0 = new JTextField(10);
        textMinConstr0.setText("1.0E-5");
        textMinConstr0.setForeground(Color.black);
        textMinConstr0.setFont(serif12);
        gbc.gridx = 1;
        mainPanel.add(textMinConstr0, gbc);
        
        labelMaxConstr0 = new JLabel("K_trans maximum allowed value (1.0E-5 - 5.0)in /min");
        labelMaxConstr0.setForeground(Color.black);
        labelMaxConstr0.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = 2;
        mainPanel.add(labelMaxConstr0, gbc);
        
        textMaxConstr0 = new JTextField(10);
        textMaxConstr0.setText("5.0");
        textMaxConstr0.setForeground(Color.black);
        textMaxConstr0.setFont(serif12);
        gbc.gridx = 1;
        mainPanel.add(textMaxConstr0, gbc);
        
        labelInitial0 = new JLabel("K_trans initial guess value (1.0E-5 - 5.0)in /min");
        labelInitial0.setForeground(Color.black);
        labelInitial0.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = 3;
        mainPanel.add(labelInitial0, gbc);
        
        textInitial0 = new JTextField(10);
        textInitial0.setText("0.02");
        textInitial0.setForeground(Color.black);
        textInitial0.setFont(serif12);
        gbc.gridx = 1;
        mainPanel.add(textInitial0, gbc);
        
        labelMinConstr1 = new JLabel("ve minimum allowed value (1.0E-5 - 0.99)");
        labelMinConstr1.setForeground(Color.black);
        labelMinConstr1.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = 4;
        mainPanel.add(labelMinConstr1, gbc);
        
        textMinConstr1 = new JTextField(10);
        textMinConstr1.setText("1.0E-5");
        textMinConstr1.setForeground(Color.black);
        textMinConstr1.setFont(serif12);
        gbc.gridx = 1;
        mainPanel.add(textMinConstr1, gbc);
        
        labelMaxConstr1 = new JLabel("ve maximum allowed value (1.0E-5 - 0.99)");
        labelMaxConstr1.setForeground(Color.black);
        labelMaxConstr1.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = 5;
        mainPanel.add(labelMaxConstr1, gbc);
        
        textMaxConstr1 = new JTextField(10);
        textMaxConstr1.setText("0.99");
        textMaxConstr1.setForeground(Color.black);
        textMaxConstr1.setFont(serif12);
        gbc.gridx = 1;
        mainPanel.add(textMaxConstr1, gbc);
        
        labelInitial1 = new JLabel("ve initial guess value (1.0E-5 - 0.99)");
        labelInitial1.setForeground(Color.black);
        labelInitial1.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = 6;
        mainPanel.add(labelInitial1, gbc);
        
        textInitial1 = new JTextField(10);
        textInitial1.setText("0.4");
        textInitial1.setForeground(Color.black);
        textInitial1.setFont(serif12);
        gbc.gridx = 1;
        mainPanel.add(textInitial1, gbc);
        
        labelMinConstr2 = new JLabel("vp minimum allowed value (0 - 0.99)");
        labelMinConstr2.setForeground(Color.black);
        labelMinConstr2.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = 7;
        mainPanel.add(labelMinConstr2, gbc);
        
        textMinConstr2 = new JTextField(10);
        textMinConstr2.setText("0.0");
        textMinConstr2.setForeground(Color.black);
        textMinConstr2.setFont(serif12);
        gbc.gridx = 1;
        mainPanel.add(textMinConstr2, gbc);
        
        labelMaxConstr2 = new JLabel("vp maximum allowed value (0 - 0.99)");
        labelMaxConstr2.setForeground(Color.black);
        labelMaxConstr2.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = 8;
        mainPanel.add(labelMaxConstr2, gbc);
        
        textMaxConstr2 = new JTextField(10);
        textMaxConstr2.setText("0.99");
        textMaxConstr2.setForeground(Color.black);
        textMaxConstr2.setFont(serif12);
        gbc.gridx = 1;
        mainPanel.add(textMaxConstr2, gbc);
        
        labelInitial2 = new JLabel("vp initial guess value (0.0 - 0.99)");
        labelInitial2.setForeground(Color.black);
        labelInitial2.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = 9;
        mainPanel.add(labelInitial2, gbc);
        
        textInitial2 = new JTextField(10);
        textInitial2.setText("0.02");
        textInitial2.setForeground(Color.black);
        textInitial2.setFont(serif12);
        gbc.gridx = 1;
        mainPanel.add(textInitial2, gbc);
        
        labelHematocrit = new JLabel("Hematocrit (0.0 - 1.0)");
        labelHematocrit.setForeground(Color.black);
        labelHematocrit.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = 10;
        mainPanel.add(labelHematocrit, gbc);
        
        textHematocrit = new JTextField(10);
        textHematocrit.setText("0.4");
        textHematocrit.setForeground(Color.black);
        textHematocrit.setFont(serif12);
        gbc.gridx = 1;
        mainPanel.add(textHematocrit, gbc);
        
        buttonTissueFile = new JButton("Choose 3D tissue R1 map");
        buttonTissueFile.setForeground(Color.black);
        buttonTissueFile.setFont(serif12B);
        buttonTissueFile.addActionListener(this);
        buttonTissueFile.setActionCommand("TissueFile");
        buttonTissueFile.setPreferredSize(new Dimension(235, 30));
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridx = 0;
        gbc.gridy = 11;
        mainPanel.add(buttonTissueFile, gbc);

        textTissueFile = new JTextField();
        textTissueFile.setFont(serif12);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 1;
        mainPanel.add(textTissueFile, gbc);
        
        labelVOI = new JLabel("Open a VOI file or draw a VOI");
        labelVOI.setForeground(Color.black);
        labelVOI.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = 12;
        mainPanel.add(labelVOI, gbc);
        
        buttonVOIFile = new JButton("Open a mandatory sagittal sinus VOI file");
        buttonVOIFile.setForeground(Color.black);
        buttonVOIFile.setFont(serif12B);
        buttonVOIFile.addActionListener(this);
        buttonVOIFile.setActionCommand("VOIFile");
        buttonVOIFile.setPreferredSize(new Dimension(205, 30));
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridy = 13;
        mainPanel.add(buttonVOIFile, gbc);
        
        textVOIFile = new JTextField();
        textVOIFile.setFont(serif12);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 1;
        mainPanel.add(textVOIFile, gbc);
        
        buttonVOIFile2 = new JButton("Open an optional process region VOI file");
        buttonVOIFile2.setForeground(Color.black);
        buttonVOIFile2.setFont(serif12B);
        buttonVOIFile2.addActionListener(this);
        buttonVOIFile2.setActionCommand("VOIFile2");
        buttonVOIFile2.setPreferredSize(new Dimension(205, 30));
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 0;
        gbc.gridy = 14;
        mainPanel.add(buttonVOIFile2, gbc);
        
        textVOIFile2 = new JTextField();
        textVOIFile2.setFont(serif12);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 1;
        mainPanel.add(textVOIFile2, gbc);
        
        VOIGroup = new ButtonGroup();

        sagittalSinusButton = new JRadioButton("Draw required sagittal sinus VOI", true);
        sagittalSinusButton.setForeground(Color.red);
        sagittalSinusButton.setFont(serif12);
        sagittalSinusButton.addActionListener(this);
        VOIGroup.add(sagittalSinusButton);
        gbc.gridy = 15;
        gbc.gridx = 0;
        mainPanel.add(sagittalSinusButton, gbc);
        componentImage.getVOIHandler().newVOI(0.0f); // red
        //componentImage.setCursorMode(ViewJComponentEditImage.NEW_VOI);
        //componentImage.getVOIHandler().setPresetHue(0.0f); // red

        processRegionButton = new JRadioButton("Draw optional region VOI", false);
        processRegionButton.setForeground(Color.green.darker());
        processRegionButton.setFont(serif12);
        processRegionButton.addActionListener(this);
        VOIGroup.add(processRegionButton);
        gbc.gridy = 16;
        mainPanel.add(processRegionButton, gbc);
        
        buttonTimesFile = new JButton("Open a file of volume center times in seconds");
        buttonTimesFile.setForeground(Color.black);
        buttonTimesFile.setFont(serif12B);
        buttonTimesFile.addActionListener(this);
        buttonTimesFile.setActionCommand("TimesFile");
        buttonTimesFile.setPreferredSize(new Dimension(225, 30));
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridx = 0;
        gbc.gridy = 17;
        mainPanel.add(buttonTimesFile, gbc);
        
        textTimesFile = new JTextField();
        textTimesFile.setFont(serif12);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 1;
        mainPanel.add(textTimesFile, gbc);
        
        
        getContentPane().add(mainPanel, BorderLayout.CENTER);
        getContentPane().add(buildButtons(), BorderLayout.SOUTH);

        pack();
        setVisible(true);
    }

    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {
        String tmpStr;
        float[] hsb;
        float hue;
        sagittalSinusIndex = -1;
        processRegionIndex = -1;
        
        tmpStr = textMinConstr0.getText();
        min_constr[0] = Double.parseDouble(tmpStr);
        
        if (min_constr[0] < 1.0E-5) {
        	MipavUtil.displayError("Minimum K_trans must be at least 1.0E-5");
        	textMinConstr0.requestFocus();
        	textMinConstr0.selectAll();
        	return false;
        }
        
        tmpStr = textMaxConstr0.getText();
        max_constr[0] = Double.parseDouble(tmpStr);
        
        if (max_constr[0] < min_constr[0]) {
        	MipavUtil.displayError("Maximum K_trans must be at least " + min_constr[0]);
        	textMaxConstr0.requestFocus();
        	textMaxConstr0.selectAll();
        	return false;
        }
        else if (max_constr[0] > 5.0) {
        	MipavUtil.displayError("Maximum K_trans must not exceed 5.0");
        	textMaxConstr0.requestFocus();
        	textMaxConstr0.selectAll();
        	return false;	
        }
        
        tmpStr = textInitial0.getText();
        initial[0] = Double.parseDouble(tmpStr);
        
        if (initial[0] < min_constr[0]) {
        	MipavUtil.displayError("Initial K_trans must be at least " + min_constr[0]);
        	textInitial0.requestFocus();
        	textInitial0.selectAll();
        	return false;
        }
        else if (initial[0] > max_constr[0]) {
        	MipavUtil.displayError("Initial K_trans must not exceed " + max_constr[0]);
        	textInitial0.requestFocus();
        	textInitial0.selectAll();
        	return false;	
        }
        
        tmpStr = textMinConstr1.getText();
        min_constr[1] = Double.parseDouble(tmpStr);
        
    	if (min_constr[1] < 1.0E-5) {
        	MipavUtil.displayError("Minimum ve must be at least 1.0E-5");
        	textMinConstr1.requestFocus();
        	textMinConstr1.selectAll();
        	return false;
        }
        
        tmpStr = textMaxConstr1.getText();
        max_constr[1] = Double.parseDouble(tmpStr);
        
        if (max_constr[1] < min_constr[1]) {
        	MipavUtil.displayError("Maximum ve must be at least " + min_constr[1]);
        	textMaxConstr1.requestFocus();
        	textMaxConstr1.selectAll();
        	return false;
        }
        else if (max_constr[1] > 0.99) {
        	MipavUtil.displayError("Maximum ve must not exceed 0.99");
        	textMaxConstr1.requestFocus();
        	textMaxConstr1.selectAll();
        	return false;	
        }
        
        tmpStr = textInitial1.getText();
        initial[1] = Double.parseDouble(tmpStr);
        
        if (initial[1] < min_constr[1]) {
        	MipavUtil.displayError("Initial ve must be at least " + min_constr[1]);
        	textInitial1.requestFocus();
        	textInitial1.selectAll();
        	return false;
        }
        else if (initial[1] > max_constr[1]) {
        	MipavUtil.displayError("Initial ve must not exceed " + max_constr[1]);
        	textInitial1.requestFocus();
        	textInitial1.selectAll();
        	return false;	
        }
        
        tmpStr = textMinConstr2.getText();
        min_constr[2] = Double.parseDouble(tmpStr);
        
        if (min_constr[2] < 0.0) {
        	MipavUtil.displayError("Minimum vp must be at least 0.0");
        	textMinConstr2.requestFocus();
        	textMinConstr2.selectAll();
        	return false;
        }
        
        tmpStr = textMaxConstr2.getText();
        max_constr[2] = Double.parseDouble(tmpStr);
        
        if (max_constr[2] < min_constr[2]) {
        	MipavUtil.displayError("Maximum vp must be at least " + min_constr[2]);
        	textMaxConstr2.requestFocus();
        	textMaxConstr2.selectAll();
        	return false;
        }
        else if (max_constr[2] > 0.99) {
        	MipavUtil.displayError("Maximum vp must not exceed 0.99");
        	textMaxConstr2.requestFocus();
        	textMaxConstr2.selectAll();
        	return false;	
        }
        
        tmpStr = textInitial2.getText();
        initial[2] = Double.parseDouble(tmpStr);
        
        if (initial[2] < min_constr[2]) {
        	MipavUtil.displayError("Initial vp must be at least " + min_constr[2]);
        	textInitial2.requestFocus();
        	textInitial2.selectAll();
        	return false;
        }
        else if (initial[2] > max_constr[2]) {
        	MipavUtil.displayError("Initial vp must not exceed " + max_constr[2]);
        	textInitial2.requestFocus();
        	textInitial2.selectAll();
        	return false;	
        }
        
        tmpStr = textHematocrit.getText();
        hematocrit = Double.parseDouble(tmpStr);
        
        if (hematocrit < 0.0) {
        	MipavUtil.displayError("Hematocrit must be at least 0.0");
        	textHematocrit.requestFocus();
        	textHematocrit.selectAll();
        	return false;
        }
        else if (hematocrit > 1.0) {
        	MipavUtil.displayError("Hematocrit must not exceed 1.0");
        	textHematocrit.requestFocus();
        	textHematocrit.selectAll();
        	return false;	
        }
        
        fileNameTissue = textTissueFile.getText();  
        try {
            FileIO fileIO = new FileIO();
            tissueImage = fileIO.readImage(fileNameTissue, directoryTissue, false, null);

            if (tissueImage == null) {
                MipavUtil.displayError("Tissue image is not valid.");

                return false;
            } else if (tissueImage.getNDims() != 3) {
                MipavUtil.displayError("Tissue image must be 3D");

                return false;
            }

            for (int i = 0; i < 3; i++) {

                if (image.getExtents()[i] != tissueImage.getExtents()[i]) {
                    MipavUtil.displayError("First 3 dimensions of source image must match the tissue image.");

                    return false;
                }
            }

        } catch (OutOfMemoryError e) {
            MipavUtil.displayError("Out of memory in JDialogSM2");

            return false;
        }
        
        VOIs = image.getVOIs();
        int nVOIs = VOIs.size();
        int nBoundingVOIs = 0;
        
        for (int i = 0; i < nVOIs; i++) {

            if ((VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) || (VOIs.VOIAt(i).getCurveType() == VOI.POLYLINE)) {
                nBoundingVOIs++;
                hsb = Color.RGBtoHSB(VOIs.VOIAt(i).getColor().getRed(), VOIs.VOIAt(i).getColor().getGreen(),
                                     VOIs.VOIAt(i).getColor().getBlue(), null);
                hue = hsb[0];

                if ((Math.abs(hue - 0.0f)) < 0.0001f) {

                    if (sagittalSinusIndex == -1) {
                        sagittalSinusIndex = i;
                        VOIs.VOIAt(i).setName("SagittalSinus");
                    } else {
                        MipavUtil.displayError("Cannot have more than 1 sagittal sinus VOI");

                        return false;
                    }
                } else if ((Math.abs(hue - (1.0f / 3.0f))) < 0.0001f) {

                    if (processRegionIndex == -1) {
                        processRegionIndex = i;
                        VOIs.VOIAt(i).setName("ProcessRegion");
                    } else {
                        MipavUtil.displayError("Cannot have more than 1 process region VOI");

                        return false;
                    }
                }  else {
                    MipavUtil.displayError("VOI hue = " + hue + " Must be 0 for red or " +
                                           "1/3 for green");

                    return false;
                }
            }
        } // for (i = 0; i < nVOIs; i++)

        if (sagittalSinusIndex == -1) {
            MipavUtil.displayError("Must specify a sagittal sinus VOI");

            return false;
        }
        
        
        if (timeVals == null) {
        	MipavUtil.displayError("Array of time values was not created");
        	return false;
        }
        
        if (timeVals.length < image.getExtents()[3]) {
            MipavUtil.displayError("Array of time values only has " + timeVals.length + " of " + image.getExtents()[3] + " required values");
            return false;
        }
        
        return true;
    }
    
    /**
     * Accessor that returns the image.
     *
     * @return  The result image.
     */
    public ModelImage[] getResultImage() {
        return resultImage;
    }
    
    /**
     * Store the result image in the script runner's image table now that the action execution is finished.
     */
    protected void doPostAlgorithmActions() {

        for (int i = 0; i < 5; i++) {
            AlgorithmParameters.storeImageInRunner(getResultImage()[i]);
        }
    }
    
    /**
     * Returns a table listing the input parameters of this algorithm (which should match up with the scripting
     * parameters used in {@link #setGUIFromParams()}).
     * 
     * @return A parameter table listing the inputs of this algorithm.
     */
    public ParameterTable createInputParameters() {
        final ParameterTable table = new ParameterTable();

        try {
        	//System.out.println("beginning input params");
            table.put(new ParameterExternalImage(AlgorithmParameters.getInputImageLabel(1)));
            table.put(new ParameterExternalImage(AlgorithmParameters.getInputImageLabel(2)));
            table.put(new ParameterDouble("min_constr0", 1.0E-5));
            table.put(new ParameterDouble("max_constr0", 5.0));
            table.put(new ParameterDouble("initial0", 0.02));
            table.put(new ParameterDouble("min_constr1", 1.0E-5));
            table.put(new ParameterDouble("max_constr1", 0.99));
            table.put(new ParameterDouble("initial1", 0.4));
            table.put(new ParameterDouble("min_constr2", 0.0));
            table.put(new ParameterDouble("max_constr2", 0.99));
            table.put(new ParameterDouble("initial2", 0.02));
            table.put(new ParameterDouble("hematocrit_", 0.4));
            table.put(new ParameterString("file_name_times"));
            table.put(new ParameterString("directory_times"));
            table.put(new ParameterString("file_name_voi"));
            table.put(new ParameterString("directory_voi"));
            table.put(new ParameterString("file_name_voi2"));
            table.put(new ParameterString("directory_voi2"));
            //System.out.println("ending input params");
        } catch (final ParserException e) {
            // this shouldn't really happen since there isn't any real parsing going on...
            e.printStackTrace();
        }

        return table;
    }
    
    /**
     * Returns a table listing the output parameters of this algorithm (usually just labels used to obtain output image
     * names later).
     * 
     * @return A parameter table listing the outputs of this algorithm.
     */
    public ParameterTable createOutputParameters() {
        final ParameterTable table = new ParameterTable();

        try {
            table.put(new ParameterImage(AlgorithmParameters.RESULT_IMAGE+"1"));
            table.put(new ParameterImage(AlgorithmParameters.RESULT_IMAGE+"2"));
            table.put(new ParameterImage(AlgorithmParameters.RESULT_IMAGE+"3"));
            table.put(new ParameterImage(AlgorithmParameters.RESULT_IMAGE+"4"));
            table.put(new ParameterImage(AlgorithmParameters.RESULT_IMAGE+"5"));
        } catch (final ParserException e) {
            // this shouldn't really happen since there isn't any real parsing going on...
            e.printStackTrace();
        }

        return table;
    }


    /**
     * Returns the name of an image output by this algorithm, the image returned depends on the parameter label given
     * (which can be used to retrieve the image object from the image registry).
     * 
     * @param imageParamName The output image parameter label for which to get the image name.
     * @return The image name of the requested output image parameter label.
     */
    public String getOutputImageName(final String imageParamName) {
        if (imageParamName.equals(AlgorithmParameters.RESULT_IMAGE+"1")) {
        	return resultImage[0].getImageName();
        }
        else if (imageParamName.equals(AlgorithmParameters.RESULT_IMAGE+"2")) {
        	return resultImage[1].getImageName();
        }
        else if (imageParamName.equals(AlgorithmParameters.RESULT_IMAGE+"3")) {
        	return resultImage[2].getImageName();
        }
        else if (imageParamName.equals(AlgorithmParameters.RESULT_IMAGE+"4")) {
        	return resultImage[3].getImageName();
        }
        else if (imageParamName.equals(AlgorithmParameters.RESULT_IMAGE+"5")) {
        	return resultImage[4].getImageName();
        }

        Preferences.debug("Unrecognized output image parameter: " + imageParamName + "\n", Preferences.DEBUG_SCRIPTING);

        return null;
    }
    
    /**
     * Returns whether the action has successfully completed its execution.
     * 
     * @return True, if the action is complete. False, if the action failed or is still running.
     */
    public boolean isActionComplete() {
        return isComplete();
    }
    
    /**
     * Return meta-information about this discoverable action for categorization and labeling purposes.
     * 
     * @return Metadata for this action.
     */
    public ActionMetadata getActionMetadata() {
        return new MipavActionMetadata() {
            public String getCategory() {
                return new String("Algorithms.MRI");
            }

            public String getDescription() {
                return new String("Calculates SM2 Model Parameters.");
            }

            public String getDescriptionLong() {
                return new String("Calculates ktrans, ve, and vp Parameters for SM2 model.");
            }

            public String getShortLabel() {
                return new String("SM2");
            }

            public String getLabel() {
                return new String("SM2");
            }

            public String getName() {
                return new String("SM2");
            }
        };
    }
    
}
