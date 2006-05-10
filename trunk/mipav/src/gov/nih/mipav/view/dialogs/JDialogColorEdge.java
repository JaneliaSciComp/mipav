package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.filters.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.util.*;

import javax.swing.*;


/**
 
 */
public class JDialogColorEdge extends JDialogBase
        implements AlgorithmInterface, ScriptableInterface, DialogDefaultsInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    
    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private AlgorithmColorEdge algoColorEdge;

    
    /** DOCUMENT ME! */
    private int displayLoc; // Flag indicating if a new image is to be generated

    // or if the source image is to be replaced

    /** DOCUMENT ME! */
    private ModelImage resultImage;

    /** DOCUMENT ME! */
    private ModelImage sourceImage;

    /** DOCUMENT ME! */
    private String[] titles; // used to save image names when replacing an image

    /** DOCUMENT ME! */
    private JTextField red1Text, green1Text, blue1Text;
    
    private JTextField red2Text, green2Text, blue2Text;
    
    private int red1, green1, blue1;
    
    private int red2, green2, blue2;

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogColorEdge() { }

    /**
     * Creates an modal extension of JDialogBase, using the title, "Local Normalization". Creates an options panel; this
     * contains: the inputs for the unsharp masking; the inputs for blurring; and the inputs for choosing which colour
     * channels to process. This last set of options are not selectable on images which are not colour images. It
     * creates the OKAY and CANCEL buttons on a panel, to b be placed at the bottom of the dialog.
     *
     * <p>The panel is then pack()'d and then setVisible(true).</p>
     *
     * @param  owner  DOCUMENT ME!
     * @param  mi     DOCUMENT ME!
     */
    public JDialogColorEdge(JFrame owner, ModelImage mi) {
        super(owner, false);
        setTitle("Color Edge");
        userInterface = ((ViewJFrameBase) (owner)).getUserInterface();
        sourceImage = mi;
        if (!mi.isColorImage()) {
            MipavUtil.displayError("Must be a color image");
            return;
        }
        displayLoc = NEW; // currently replace is not supported

        getContentPane().setLayout(new BorderLayout());
        getContentPane().add(buildColorPanel(), BorderLayout.CENTER);
        getContentPane().add(buildOkayCancelPanel(), BorderLayout.SOUTH);

        pack();
        loadDefaults();
        setVisible(true);
    }

    /**
     * Used primarily for the script to store variables and run the algorithm. No actual dialog will appear but the set
     * up info and result image will be stored here.
     *
     * <p>Replace is currently not supported.</p>
     *
     * @param  UI  The user interface, needed to create the image frame.
     * @param  im  Source image.
     */
    public JDialogColorEdge(ViewUserInterface UI, ModelImage im) {
        super();
        userInterface = UI;
        sourceImage = im;
        if (!im.isColorImage()) {
            MipavUtil.displayError("Must be a color image");
            return;
        }
        displayLoc = NEW; // currently replace is not supported
        parentFrame = im.getParentFrame();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    // ************************************************************************
    // ************************** Action Events *******************************
    // ************************************************************************

    /**
     * a button has been clicked! Cancel will dispose of the dialog, OK sets the variables and calls the algorithm; any
     * errors in setting the variables upon OK are written to the debug pane. Help brings up the help text.
     *
     * @param  ae  DOCUMENT ME!
     */
    public void actionPerformed(ActionEvent ae) {
        String command = ae.getActionCommand();

        if (command.equalsIgnoreCase("cancel")) {
            dispose();
        } else if (command.equalsIgnoreCase("ok")) {

            if (setVariables()) {
                callAlgorithm();

            } else {
                Preferences.debug("JDialogColorEdge: " + "error setting variables.");
            }
        } else if (command.equals("Help")) {
            MipavUtil.showHelp("");
        }
    }

    // ************************************************************************
    // ************************** Algorithm Events ****************************
    // ************************************************************************

    /**
     * This method is required if the AlgorithmPerformed interface is implemented. It is called by the algorithms when
     * it has completed or failed to to complete, so that the dialog can be display the result image and/or clean up.
     *
     * @param  algorithm  Algorithm that caused the event.
     */
    public void algorithmPerformed(AlgorithmBase algorithm) {

        if (Preferences.is(Preferences.PREF_SAVE_DEFAULTS) && (this.getOwner() != null) && !isScriptRunning()) {
            saveDefaults();
        }

        if (algorithm instanceof AlgorithmColorEdge) {
            sourceImage.clearMask();

            if ((algoColorEdge.isCompleted() == true) && (resultImage != null)) {

                // The algorithm has completed and produced a new image to be displayed.
                updateFileInfo(sourceImage, resultImage);
                resultImage.clearMask();

                try {
                    new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
                } catch (OutOfMemoryError error) {
                    System.gc();
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }
            } else if (resultImage == null) {

                // These next lines set the titles in all frames where the source image is displayed to
                // image name so as to indicate that the image is now unlocked!
                // The image frames are enabled and then registered to the userinterface.
                Vector imageFrames = sourceImage.getImageFrameVector();
                titles = new String[imageFrames.size()];

                for (int i = 0; i < imageFrames.size(); i++) {
                    ((Frame) (imageFrames.elementAt(i))).setTitle(titles[i]);
                    ((Frame) (imageFrames.elementAt(i))).setEnabled(true);

                    if (((Frame) (imageFrames.elementAt(i))) != parentFrame) {
                        userInterface.registerFrame((Frame) (imageFrames.elementAt(i)));
                    }
                }

                if (parentFrame != null) {
                    userInterface.registerFrame(parentFrame);
                }

                sourceImage.notifyImageDisplayListeners(null, true);
            } else if (resultImage != null) {

                // algorithm failed but result image still has garbage
                resultImage.disposeLocal(); // clean up memory
                resultImage = null;
                System.gc();
            }

            insertScriptLine(algorithm);
        } // else not a Color Edge algorithm
        else {
            Preferences.debug("JDialogColorEdge caught algorithm performed for: " +
                              algorithm.getClass().getName(), Preferences.DEBUG_ALGORITHM);
        }

        algoColorEdge.finalize();
        algoColorEdge = null;
        dispose();
    }

    /**
     * Construct a delimited string that contains the parameters to this algorithm.
     *
     * @param   delim  the parameter delimiter (defaults to " " if empty)
     *
     * @return  the parameter string
     */
    public String getParameterString(String delim) {

        if (delim.equals("")) {
            delim = " ";
        }

        String str = new String();
        str += red1 + delim;
        str += green1 + delim;
        str += blue1 + delim;
        str += red2 + delim;
        str += green2 + delim;
        str += blue2;

        return str;
    }

    // ************************************************************************
    // ************************** Access Methods*******************************
    // ************************************************************************

    /**
     * Accessor that returns the image.
     *
     * @return  The result image.
     */
    public ModelImage getResultImage() {
        return resultImage;
    }

    /**
     * If a script is being recorded and the algorithm is done, add an entry for this algorithm.
     *
     * @param  algo  the algorithm to make an entry for
     */
    public void insertScriptLine(AlgorithmBase algo) {

        if (algo.isCompleted()) {

            if (userInterface.isScriptRecording()) {

                // check to see if the  image is already in the ImgTable
                if (userInterface.getScriptDialog().getImgTableVar(sourceImage.getImageName()) == null) {

                    if (userInterface.getScriptDialog().getActiveImgTableVar(sourceImage.getImageName()) == null) {
                        userInterface.getScriptDialog().putActiveVar(sourceImage.getImageName());
                    }
                }

                userInterface.getScriptDialog().append("ColorEdge " +
                                                       userInterface.getScriptDialog().getVar(sourceImage.getImageName()) +
                                                       " ");

                if (displayLoc == NEW) {
                    userInterface.getScriptDialog().putVar(resultImage.getImageName());
                    userInterface.getScriptDialog().append(userInterface.getScriptDialog().getVar(resultImage.getImageName()) +
                                                           " " + getParameterString(" ") + "\n");
                } else {
                    userInterface.getScriptDialog().append(userInterface.getScriptDialog().getVar(sourceImage.getImageName()) +
                                                           " " + getParameterString(" ") + "\n");
                }
            }
        }
    }

    /**
     * Loads the default settings from Preferences to set up the dialog.
     */
    public void loadDefaults() {
        String defaultsString = Preferences.getDialogDefaults(getDialogName());

        if ((defaultsString != null) && (red1Text != null)) {
            StringTokenizer st = new StringTokenizer(defaultsString, ",");

            try {
                red1Text.setText("" + MipavUtil.getInt(st));
                green1Text.setText("" + MipavUtil.getInt(st));
                blue1Text.setText("" + MipavUtil.getInt(st));
                red2Text.setText("" + MipavUtil.getInt(st));
                green2Text.setText("" + MipavUtil.getInt(st));
                blue2Text.setText("" + MipavUtil.getInt(st));
            } catch (Exception ex) {

                // since there was a problem parsing the defaults string, start over with the original defaults
                Preferences.debug("Resetting defaults for dialog: " + getDialogName());
                Preferences.removeProperty(getDialogName());
                ex.printStackTrace();
            }
        }
    }

    /**
     * Saves the default settings into the Preferences file.
     */
    public void saveDefaults() {
        String defaultsString = new String(getParameterString(","));
        Preferences.saveDialogDefaults(getDialogName(), defaultsString);
    }

    /**
     * Run this algorithm from a script.
     *
     * @param   parser  the script parser we get the state from
     *
     * @throws  IllegalArgumentException  if there is something wrong with the arguments in the script
     */
    public void scriptRun(AlgorithmScriptParser parser) throws IllegalArgumentException {
        setScriptRunning(true);

        String srcImageKey = null;
        String destImageKey = null;

        try {
            srcImageKey = parser.getNextString();
        } catch (Exception e) {
            throw new IllegalArgumentException();
        }

        ModelImage im = parser.getImage(srcImageKey);

        sourceImage = im;
        if (!im.isColorImage()) {
            MipavUtil.displayError("Must be a color image");
            return;
        }
        displayLoc = NEW;
        userInterface = sourceImage.getUserInterface();
        parentFrame = sourceImage.getParentFrame();

        // the result image
        try {
            destImageKey = parser.getNextString();
        } catch (Exception e) {
            throw new IllegalArgumentException();
        }

        try {
            setRed1(parser.getNextInteger());
            setGreen1(parser.getNextInteger());
            setBlue1(parser.getNextInteger());
            setRed2(parser.getNextInteger());
            setGreen2(parser.getNextInteger());
            setBlue2(parser.getNextInteger());
        } catch (Exception e) {
            throw new IllegalArgumentException();
        }

        setActiveImage(parser.isActiveImage());
        setSeparateThread(false);
        callAlgorithm();

        if (!srcImageKey.equals(destImageKey)) {
            parser.putVariable(destImageKey, getResultImage().getImageName());
        }
    }

    
    /**
     * 
     * @param red1
     */
    public void setRed1(int red1) {
        this.red1 = red1;
    }
    
    /**
     * 
     * @param green1
     */
    public void setGreen1(int green1) {
        this.green1 = green1;
    }
    
    /**
     * 
     * @param blue1
     */
    public void setBlue1(int blue1) {
        this.blue1 = blue1;
    }
    
    /**
     * 
     * @param red2
     */
    public void setRed2(int red2) {
        this.red2 = red2;
    }
    
    /**
     * 
     * @param green2
     */
    public void setGreen2(int green2) {
        this.green2 = green2;
    }
    
    /**
     * 
     * @param blue2
     */
    public void setBlue2(int blue2) {
        this.blue2 = blue2;
    }
    
    /**
     * Accessor that sets the display loc variable to new, so that a new image is created once the algorithm completes.
     */
    public void setDisplayLocNew() {
        displayLoc = NEW;
    }

    /**
     * Accessor that sets the display loc variable to replace, so the current image is replaced once the algorithm
     * completes.
     */
    public void setDisplayLocReplace() {

        // eventually this will set this... for now, only NEW is supported
        // displayLoc = REPLACE;
        displayLoc = NEW;
    }

    
    

    
    /**
     * creates the planel which contains the OKAY and Cancel buttons. sets their sizes, colours and listeners.
     *
     * @return  DOCUMENT ME!
     */
    private JPanel buildOkayCancelPanel() {
        return buildButtons();
    }

    

    /**
     * part of the algorithm rests on finding the original image minus an estimation of the local mean.
     *
     * <p>This is the same as using as unsharp-mask filter; so, this panel is a recreation of the inputs made in the
     * JDialogUnsharpMask.</p>
     *
     * <p>The panel is returned to the caller.</p>
     *
     * @see  JDialogUnsharpMask
     */
    private JPanel buildColorPanel() {
        JPanel colorPanel = new JPanel();
        Insets spacer = new Insets(0, 10, 0, 0);
        Insets nospace = new Insets(0, 0, 0, 0);

        GridBagLayout gbl = new GridBagLayout();
        GridBagConstraints gbc = new GridBagConstraints();

        colorPanel.setLayout(gbl);
        colorPanel.setBorder(buildTitledBorder("Colors at edge"));

        gbc.gridwidth = gbc.RELATIVE;
        gbc.insets = nospace;
        gbc.anchor = gbc.WEST;
        colorPanel.add(createLabel("Red 1:"), gbc);

        gbc.gridwidth = gbc.REMAINDER;
        gbc.insets = spacer;
        gbc.anchor = gbc.EAST;
        red1Text = createEntryField("0");
        MipavUtil.makeNumericsOnly(red1Text, true);
        colorPanel.add(red1Text, gbc);

        gbc.gridwidth = gbc.RELATIVE;
        gbc.insets = nospace;
        gbc.anchor = gbc.WEST;
        colorPanel.add(createLabel("Green 1:"), gbc);

        gbc.gridwidth = gbc.REMAINDER;
        gbc.insets = spacer;
        gbc.anchor = gbc.EAST;
        green1Text = createEntryField("0");
        MipavUtil.makeNumericsOnly(green1Text, true);
        colorPanel.add(green1Text, gbc);

        gbc.gridwidth = gbc.RELATIVE;
        gbc.insets = nospace;
        gbc.anchor = gbc.WEST;
        colorPanel.add(createLabel("Blue 1:"), gbc);

        gbc.gridwidth = gbc.REMAINDER;
        gbc.insets = spacer;
        gbc.anchor = gbc.EAST;
        blue1Text = createEntryField("0");
        MipavUtil.makeNumericsOnly(blue1Text, true);
        colorPanel.add(blue1Text, gbc);

        gbc.gridwidth = gbc.RELATIVE;
        gbc.insets = nospace;
        gbc.anchor = gbc.WEST;
        colorPanel.add(createLabel("Red 2:"), gbc);

        gbc.gridwidth = gbc.REMAINDER;
        gbc.insets = spacer;
        gbc.anchor = gbc.EAST;
        red2Text = createEntryField("0");
        MipavUtil.makeNumericsOnly(red2Text, true);
        colorPanel.add(red2Text, gbc);

        gbc.gridwidth = gbc.RELATIVE;
        gbc.insets = nospace;
        gbc.anchor = gbc.WEST;
        colorPanel.add(createLabel("Green 2:"), gbc);

        gbc.gridwidth = gbc.REMAINDER;
        gbc.insets = spacer;
        gbc.anchor = gbc.EAST;
        green2Text = createEntryField("0");
        MipavUtil.makeNumericsOnly(green2Text, true);
        colorPanel.add(green2Text, gbc);

        gbc.gridwidth = gbc.RELATIVE;
        gbc.insets = nospace;
        gbc.anchor = gbc.WEST;
        colorPanel.add(createLabel("Blue 2:"), gbc);

        gbc.gridwidth = gbc.REMAINDER;
        gbc.insets = spacer;
        gbc.anchor = gbc.EAST;
        blue2Text = createEntryField("0");
        MipavUtil.makeNumericsOnly(blue2Text, true);
        colorPanel.add(blue2Text, gbc);

        return colorPanel;
    }

    /**
     * Once all the necessary variables are set, call the local normalization algorithm based on what type of image this
     * is and whether or not there is a separate destination image.
     */
    private void callAlgorithm() {
        String name = makeImageName(sourceImage.getImageName(), "_ColorEdge");

        
        // if (displayLoc == NEW) {
        try {

            // Make result image of UBYTE type
            resultImage     = new ModelImage(ModelStorageBase.UBYTE, sourceImage.getExtents(), name, userInterface);

            /*if ((resultImage.getFileInfo()[0]).getFileFormat() == FileBase.DICOM) {
                ((FileInfoDicom) (resultImage.getFileInfo(0))).setValue("0002,0002", "1.2.840.10008.5.1.4.1.1.7 ",
                                                                        26); // Secondary Capture SOP UID
                ((FileInfoDicom) (resultImage.getFileInfo(0))).setValue("0008,0016", "1.2.840.10008.5.1.4.1.1.7 ",
                                                                        26);
                ((FileInfoDicom) (resultImage.getFileInfo(0))).setValue("0002,0012", "1.2.840.34379.17", 16); // bogus Implementation UID made up by Matt
                ((FileInfoDicom) (resultImage.getFileInfo(0))).setValue("0002,0013", "MIPAV--NIH", 10); //
            }*/

            // Make algorithm
            algoColorEdge = new AlgorithmColorEdge(resultImage, sourceImage, red1, green1, blue1,
                                                   red2, green2, blue2);
            algoColorEdge.setUserInterface(userInterface);

            // This is very important. Adding this object as a listener
            // allows the algorithm to notify this object when it
            // has completed or failed. See algorithm performed event.
            // This is made possible by implementing
            // AlgorithmedPerformed interface
            algoColorEdge.addListener(this);
            setVisible(false); // Hide dialog

            if (runInSeparateThread) {

                // Start the thread as a low priority because we wish
                // to still have user interface work fast.
                if (algoColorEdge.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                algoColorEdge.setActiveImage(isActiveImage);

                if (!userInterface.isAppFrameVisible()) {
                    algoColorEdge.setProgressBarVisible(false);
                }

                algoColorEdge.run();
            }
        } catch (OutOfMemoryError x) {
            MipavUtil.displayError("Dialog LocalNormalization: unable to allocate enough memory");

            if (resultImage != null) {
                resultImage.disposeLocal(); // Clean up memory of result image
                resultImage = null;
            }

            return;
        }
        // }
        /*else {  // displayLoc == REPLACE        (2D)
         */
        
    } // end callAlgorithm()

    


    /**
     * Builds a new JTextField, with the given String, sets its font (to MipavUtil.font12), sets the foreground colour
     * (to Color.black), sets column width to 7, then returns the newly made JTextField.
     *
     * @param   presetText  the String to have the JTextField display.
     *
     * @return  a black-text, font12'd, JTextField displaying presetText.
     */
    private JTextField createEntryField(String presetText) {
        JTextField jtf = new JTextField(presetText);
        jtf.setFont(MipavUtil.font12);
        jtf.setForeground(Color.black);
        jtf.setColumns(7);
        jtf.setHorizontalAlignment(JTextField.RIGHT);

        return jtf;
    }

    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {
        int imageMax = 255;
        if (sourceImage.getType() == ModelStorageBase.ARGB_USHORT) {
            imageMax = 65535;
        }
        
        try {
            red1 = Integer.parseInt(red1Text.getText());
        } catch (NullPointerException npe) {
            MipavUtil.displayError("No value!  Value must be >= 0"); 
            red1Text.requestFocus();
            red1Text.selectAll(); // doesn't really do anything since the object is empty

            return false;
        }

        if (red1 < 0) {
            MipavUtil.displayError("Value must be >= 0"); 
            red1Text.requestFocus();
            red1Text.selectAll(); 

            return false;
        }
        
        if (red1 > imageMax) {
            MipavUtil.displayError("Value must be <= " + imageMax); 
            red1Text.requestFocus();
            red1Text.selectAll(); 

            return false;
        }
        
        try {
            green1 = Integer.parseInt(green1Text.getText());
        } catch (NullPointerException npe) {
            MipavUtil.displayError("No value!  Value must be >= 0"); 
            green1Text.requestFocus();
            green1Text.selectAll(); // doesn't really do anything since the object is empty

            return false;
        }

        if (green1 < 0) {
            MipavUtil.displayError("Value must be >= 0"); 
            green1Text.requestFocus();
            green1Text.selectAll(); 

            return false;
        }
        
        if (green1 > imageMax) {
            MipavUtil.displayError("Value must be <= " + imageMax); 
            green1Text.requestFocus();
            green1Text.selectAll(); 

            return false;
        }
        
        try {
            blue1 = Integer.parseInt(blue1Text.getText());
        } catch (NullPointerException npe) {
            MipavUtil.displayError("No value!  Value must be >= 0"); 
            blue1Text.requestFocus();
            blue1Text.selectAll(); // doesn't really do anything since the object is empty

            return false;
        }

        if (blue1 < 0) {
            MipavUtil.displayError("Value must be >= 0"); 
            blue1Text.requestFocus();
            blue1Text.selectAll(); 

            return false;
        }
        
        if (blue1 > imageMax) {
            MipavUtil.displayError("Value must be <= " + imageMax); 
            blue1Text.requestFocus();
            blue1Text.selectAll(); 

            return false;
        }
        
        try {
            red2 = Integer.parseInt(red2Text.getText());
        } catch (NullPointerException npe) {
            MipavUtil.displayError("No value!  Value must be >= 0"); 
            red2Text.requestFocus();
            red2Text.selectAll(); // doesn't really do anything since the object is empty

            return false;
        }

        if (red2 < 0) {
            MipavUtil.displayError("Value must be >= 0"); 
            red2Text.requestFocus();
            red2Text.selectAll(); 

            return false;
        }
        
        if (red2 > imageMax) {
            MipavUtil.displayError("Value must be <= " + imageMax); 
            red2Text.requestFocus();
            red2Text.selectAll(); 

            return false;
        }
        
        try {
            green2 = Integer.parseInt(green2Text.getText());
        } catch (NullPointerException npe) {
            MipavUtil.displayError("No value!  Value must be >= 0"); 
            green2Text.requestFocus();
            green2Text.selectAll(); // doesn't really do anything since the object is empty

            return false;
        }

        if (green2 < 0) {
            MipavUtil.displayError("Value must be >= 0"); 
            green2Text.requestFocus();
            green2Text.selectAll(); 

            return false;
        }
        
        if (green2 > imageMax) {
            MipavUtil.displayError("Value must be <= " + imageMax); 
            green2Text.requestFocus();
            green2Text.selectAll(); 

            return false;
        }
        
        try {
            blue2 = Integer.parseInt(blue2Text.getText());
        } catch (NullPointerException npe) {
            MipavUtil.displayError("No value!  Value must be >= 0"); 
            blue2Text.requestFocus();
            blue2Text.selectAll(); // doesn't really do anything since the object is empty

            return false;
        }

        if (blue2 < 0) {
            MipavUtil.displayError("Value must be >= 0"); 
            blue2Text.requestFocus();
            blue2Text.selectAll(); 

            return false;
        }
        
        if (blue2 > imageMax) {
            MipavUtil.displayError("Value must be <= " + imageMax); 
            blue2Text.requestFocus();
            blue2Text.selectAll(); 

            return false;
        }
        
        return true;
    }
}
