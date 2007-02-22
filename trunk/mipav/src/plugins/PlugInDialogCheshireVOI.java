import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.*;

import java.awt.*;
import java.awt.event.*;
import java.io.File;
import java.util.Vector;
import javax.swing.*;




/**
 * @version  February 22, 2007
 * @see      JDialogBase
 * @see      AlgorithmInterface
 *
 *           
 */
public class PlugInDialogCheshireVOI extends JDialogScriptableBase implements AlgorithmInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    public static final String BROWSE = "BROWSE";
    
    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private Vector files = new Vector();
    
    /** Result image. */
    private ModelImage resultImage = null;

    /** Source image */
    private ModelImage image; // source image
    
    /** Text field for file name of chesire overlay file. */
    private JTextField textName;
    
    /** Button to browse for cheshire overlay file. */
    private JButton browseButton;

    /** The implemented plugin algorithm */
    private PlugInAlgorithmCheshireVOI cheshireVoiAlgo = null;
    

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor used for instantiation during script execution (required for dynamic loading).
     */
    public PlugInDialogCheshireVOI() { }

    /**
     * Creates new dialog for converting a cheshire overlay file to VOIs.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public PlugInDialogCheshireVOI(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        image = im;
        init();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    // ************************************************************************
    // ************************** Event Processing ****************************
    // ************************************************************************

    /**
     * Closes dialog box when the OK button is pressed and calls the algorithm.
     *
     * @param  event  Event that triggers function.
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();
        Object source = event.getSource();
        if (command.equals(BROWSE)) {

            try {
                //Define .oly filter
                String[] extensions = new String[1];
                extensions[0] = "oly";
                ViewImageFileFilter oly = new ViewImageFileFilter(extensions);
                
                
                JFileChooser chooser = new JFileChooser();
                chooser.setDialogTitle("Select Cheshire File(s)");
                chooser.addChoosableFileFilter(oly); // shows overlay files only
                chooser.setCurrentDirectory(new File(image.getImageDirectory()));
                chooser.setMultiSelectionEnabled(true);
                

                int returnValue = chooser.showOpenDialog(this);

                if (returnValue == JFileChooser.APPROVE_OPTION) {
                    files.removeAllElements();
                    textName.setText("");
                    textName.setToolTipText(null);

                    File[] fileArray = chooser.getSelectedFiles();
                    String fileNames = new String();

                    for (int i = 0; i < fileArray.length; i++) {
                        files.add(fileArray[i]);
                        fileNames += fileArray[i].getName() + " ";
                    }

                    if (files.size() > 0) {
                        textName.setText(fileNames);

                        if (fileNames.length() > 100) {
                            textName.setToolTipText(fileNames.substring(0, 99) + "...");
                        } else {
                            textName.setToolTipText(fileNames);
                        }
                    }

                } else {
                    return;
                }
            } catch (OutOfMemoryError e) {
                MipavUtil.displayError("Out of memory");

                return;
            }

        }
        else if (command.equals("OK")) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Script")) {
            callAlgorithm();
        } else if (command.equals("Cancel")) {
            dispose();
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
        
        

        if (algorithm instanceof PlugInAlgorithmCheshireVOI) {
            Preferences.debug("Cheshire to VOI Elapsed: " + algorithm.getElapsedTime());
            image.clearMask();
            
            if (algorithm.isCompleted()) {
                insertScriptLine();
            }

            if (algorithm != null) {
                algorithm.finalize();
                algorithm = null;
            }
            image.notifyImageDisplayListeners(null, true);
            dispose();
        }

    } // end AlgorithmPerformed()

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
        //str += fileName;    //necessry parameter?

        return str;
    }

    /**
     * Accessor that sets the file name to be used
     * @param fileName
     */
    public void setFileName(String[] fileName) {
        //this.fileName = fileName;
    }

    
    /**
     * Once all the necessary variables are set, call the cheshire to voi algorithm
     */
    protected void callAlgorithm() {

        try {
            String name = makeImageName(image.getImageName(), "_toVoi");
            resultImage = (ModelImage) image.clone();
            resultImage.setImageName(name);
            cheshireVoiAlgo = new PlugInAlgorithmCheshireVOI(resultImage, image, files);

            // This is very important. Adding this object as a listener allows
            // the algorithm to
            // notify this object when it has completed or failed. See algorithm
            // performed event.
            // This is made possible by implementing AlgorithmedPerformed
            // interface
            cheshireVoiAlgo.addListener(this);
            createProgressBar(image.getImageName(), " ...", cheshireVoiAlgo);

            setVisible(false); // Hide dialog

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still
                // have user interface work fast.
                if (cheshireVoiAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                cheshireVoiAlgo.run();
            }
        } catch (OutOfMemoryError x) {
            if (resultImage != null) {
                resultImage.disposeLocal(); // Clean up memory of result image
                resultImage = null;
            }

            MipavUtil.displayError("Cheshire to VOI: unable to allocate enough memory");

            return;
        }

    } // end callAlgorithm()
    
    /**
     * Accessor that returns the image.
     *
     * @return  the result image
     */
    public ModelImage getResultImage() {
        return resultImage;
    }

    /**
     * Store the result image in the script runner's image table now that the action execution is finished.
     */
    protected void doPostAlgorithmActions() {
        AlgorithmParameters.storeImageInRunner(getResultImage());
    }

    /**
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {
        image = scriptParameters.retrieveInputImage();
        parentFrame = image.getParentFrame();

        //setFileName(scriptParameters.getParams().getString("file_name"));
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        scriptParameters.storeOutputImageParams(resultImage, true);

        //scriptParameters.getParams().put(ParameterFactory.newParameter("file_name", fileName));
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setForeground(Color.black);
        addNotify();
        setTitle("Cheshire to VOI");

        JPanel mainPanel = new JPanel(new GridBagLayout());
        mainPanel.setForeground(Color.black);
        mainPanel.setBorder(buildTitledBorder("Select Cheshire overlay file(s)"));

        JLabel labelType = new JLabel("Cheshire Overlay Files");
        labelType.setForeground(Color.black);
        labelType.setFont(serif12);

        textName = new JTextField(30);
        textName.setText("Cheshire overlay file");
        textName.setFont(serif12);
        textName.setEnabled(false);

        browseButton = new JButton("Browse");
        browseButton.setPreferredSize(MipavUtil.defaultButtonSize);
        browseButton.setFont(serif12B);
        browseButton.setActionCommand(BROWSE);
        browseButton.addActionListener(this);
        
        
        Insets insets = new Insets(0, 2, 0, 2);
        GridBagConstraints gbc = new GridBagConstraints();

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.insets = insets;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        gbc.anchor = GridBagConstraints.WEST;

        mainPanel.add(browseButton, gbc);
        gbc.gridx = 1;
        gbc.gridy = 0;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        mainPanel.add(textName, gbc);

        
        getContentPane().add(mainPanel, BorderLayout.CENTER);

        // Build the Panel that holds the OK and CANCEL Buttons
        JPanel OKCancelPanel = new JPanel();

        // size and place the OK button
        buildOKButton();
        OKCancelPanel.add(OKButton, BorderLayout.WEST);

        // size and place the CANCEL button
        buildCancelButton();
        OKCancelPanel.add(cancelButton, BorderLayout.EAST);
        getContentPane().add(OKCancelPanel, BorderLayout.SOUTH);

        pack();
        setVisible(true);
        setResizable(false);
        System.gc();

    } // end init()

    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {
        

        return true;
    } // end setVariables()

}
