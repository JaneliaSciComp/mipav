package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.utilities.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.JDialogSwapSlicesVolumes.SwapMode;

import java.awt.*;
import java.awt.event.*;

import java.util.*;

import javax.swing.*;
import javax.swing.table.DefaultTableModel;


/**
 * Creates the dialog for swapping slices/volumes. Allows 3D or 4D images.
 *
 * @author   Justin Senseney
 * @version  v1 2012
 */
public class JDialogSwapSlicesVolumes extends JDialogScriptableBase implements AlgorithmInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    public enum SwapMode {
        ThreeD("Slice", 2),
        FourD("Volume", 3);
        
        private String title;
        private int dimLoc;

        SwapMode(String title, int dimLoc) {
            this.title = title;
            this.dimLoc = dimLoc;
        }
        
        public String getTitle() {
            return title;
        }
        
        public int getDim() {
            return dimLoc;
        }
    }
    
    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private JCheckBox[] checkboxList;

    /** DOCUMENT ME! */
    private boolean[] checkListExtract;

    /** DOCUMENT ME! */
    private ModelImage[] extractedImages;

    /** DOCUMENT ME! */
    private AlgorithmSwapSlicesVolume extractSlicesAlgo;

    /** DOCUMENT ME! */
    private int numChecked;

    /** Swap mode, either 3D or 4D */
    private SwapMode mode;

    /** Panel for displaying JTable */
    private JPanel tablePanel;
    
    /** Number of slices in mode */
    private int nSlices; // number of slices in image
    
    /** srcImage for keeping track of slices */
    private ModelImage srcImage;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogSwapSlicesVolumes() { }

    /**
     * Creates new dialog for removing slices.
     *
     * @param  theParentFrame  Parent frame
     * @param  im              Source image
     */
    public JDialogSwapSlicesVolumes(Frame theParentFrame, ModelImage im, SwapMode mode) {
        super(theParentFrame, false);
        this.srcImage = im;
        this.mode = mode;
        init();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Closes dialog box when the OK button is pressed and calls the algorithm.
     *
     * @param  event  Event that triggers function
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();
        int i;

        if (command.equals("Extract")) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Cancel")) {
            dispose();
        } else if (command.equals("Help")) {
            MipavUtil.showHelp("U4051");
        } else if (command.equals("Check")) {

            for (i = 0; i < nSlices; i++) {
                (checkboxList[i]).setSelected(true);
            }
        } else if (command.equals("UnCheck")) {

            for (i = 0; i < nSlices; i++) {
                (checkboxList[i]).setSelected(false);
            }
        } else if (command.equals("CheckEven")) {

            for (i = 0; i < nSlices; i += 2) {
                (checkboxList[i]).setSelected(true);
            }
        } else if (command.equals("CheckOdd")) {

            for (i = 1; i < nSlices; i += 2) {
                (checkboxList[i]).setSelected(true);
            }
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

        if (algorithm instanceof AlgorithmExtractSlicesVolumes) {
            extractedImages = extractSlicesAlgo.getExtractedImages();

            for (int i = 0; i < extractedImages.length; i++) {
                new ViewJFrameImage(extractedImages[i]);
            }

            if (Preferences.debugLevel(Preferences.DEBUG_ALGORITHM)) {
                int numExtracted = 0;
                Preferences.debug("\nHave extracted slices:\n");

                for (int i = 0; i < checkListExtract.length; i++) {

                    if (checkListExtract[i]) {
                        Preferences.debug("\t" + (i));

                        if (((i % 5) == 4) || (numExtracted == (numChecked - 1))) {
                            Preferences.debug("\n");
                        }

                        numExtracted++;
                    }
                }

                if (srcImage.getNDims() == 3) {
                    Preferences.debug("from " + srcImage.getFileInfo(0).getExtents()[2] + " slice 3D " +
                                      srcImage.getImageName() + "\n");
                } else {
                    Preferences.debug("from " + srcImage.getFileInfo(0).getExtents()[2] + " slice " +
                                      srcImage.getFileInfo(0).getExtents()[3] + " volume 4D " +
                                      srcImage.getImageName() + "\n");
                }

                Preferences.debug("to create:\n");

                if (srcImage.getNDims() == 3) {

                    if (numExtracted > 1) {
                        Preferences.debug(numExtracted + " 2D images\n");
                    } else {
                        Preferences.debug(numExtracted + " 2D image\n");
                    }
                } else {

                    if (numExtracted > 1) {
                        Preferences.debug(numExtracted + " " + srcImage.getFileInfo(0).getExtents()[3] +
                                          " slice 3D images\n");
                    } else {
                        Preferences.debug(numExtracted + " " + srcImage.getFileInfo(0).getExtents()[3] +
                                          " slice 3D image\n");
                    }
                }
            } // if (Preferences.debugLevel(Preferences.DEBUG_ALGORITHM))

            if (algorithm.isCompleted()) {
                insertScriptLine();
            }
        } // if ( algorithm instanceof AlgorithmExtractSlicesVolumes )

        extractSlicesAlgo.finalize();
        extractSlicesAlgo = null;
        dispose();
    }

    /**
     * Once all the necessary variables are set, call the Remove Slices algorithm based on what type of image this is
     * and whether or not there is a separate destination image.
     */
    protected void callAlgorithm() {
        System.gc();
        nSlices = srcImage.getExtents()[2];
        numChecked = 0;

        for (int i = 0; i < nSlices; i++) {

            if (checkListExtract[i]) {
                numChecked++;
            }
        }

        if (numChecked != 0) {
            extractSlicesAlgo = new AlgorithmSwapSlicesVolume(srcImage, checkListExtract);
            extractSlicesAlgo.addListener(this);

            createProgressBar(srcImage.getImageName(), extractSlicesAlgo);

            setVisible(false); // Hide dialog

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (extractSlicesAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                extractSlicesAlgo.run();
            }

        } else if (numChecked == 0) {
            MipavUtil.displayError("No slices were selected!  Select some slices.");
        } 
    }

    /**
     * Store the result image in the script runner's image table now that the action execution is finished.
     */
    protected void doPostAlgorithmActions() {

        for (int i = 0; i < extractedImages.length; i++) {
            AlgorithmParameters.storeImageInRunner(extractedImages[i]);
        }
    }

    /**
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {
        srcImage = scriptParameters.retrieveInputImage();
        parentFrame = srcImage.getParentFrame();

        if (srcImage.getNDims() < 3) {
            throw new ParameterException(AlgorithmParameters.getInputImageLabel(1), "3D or 4D image required.");
        }

        checkListExtract = null;

        if (checkListExtract == null) {
            throw new ParameterException("slices",
                                         "A problem was encountered while parsing the list of slices to extract.");
        }
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(srcImage);

        for (int i = 0; i < extractedImages.length; i++) {
            scriptParameters.storeImageInRecorder(extractedImages[i]);
        }

        scriptParameters.getParams().put(ParameterFactory.newParameter("slices",
                                                                       nSlices));
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {

        // make sure that this is a 3D image first
        // make sure this image, im, is not 2D, for removing an image's only slice makes no sense...
        if ((srcImage.getNDims() == 2) || (srcImage.getExtents()[2] == 1)) {
            MipavUtil.displayError("Extract Individual Slices does not make sense for single-slice (2-D)\n" +
                                   "images.  No operation may be performed.");

            return; // the wrong kind of image gets sent back before wasting anymore time.
        }

        nSlices = srcImage.getExtents()[mode.getDim()];

        JPanel mainPanel = new JPanel(new BorderLayout()); // everything gets placed on this panel

        setTitle("Swap "+mode.getTitle());
        setForeground(Color.black);

        String[] columnName = new String[]{"Index", mode.getTitle()};
        
        DefaultTableModel d = new DefaultTableModel() {
            public boolean isCellEditable(int row, int column) {
                return false;
            }
        };
        d.setColumnCount(2);
        d.setColumnIdentifiers(columnName);
        
        for(int i=0; i<nSlices; i++) {
            Vector<String> v = new Vector<String>();
            v.add(String.valueOf(i));
            v.add(mode.getTitle()+" "+i);
            
            d.addRow(v);
        }
        
        JTable table = new JTable(d);
        
        table.setRequestFocusEnabled(true);
        table.setFocusable(true);
        
        JScrollPane scroll = new JScrollPane(table);
        scroll.setPreferredSize(new Dimension(340, 450));
        
        mainPanel.add(scroll, BorderLayout.CENTER);
        JPanel buttonPanel = new JPanel(new FlowLayout());
        buttonPanel.add(buildButtons());
        OKButton.setText("Swap");

        mainPanel.add(buttonPanel, BorderLayout.SOUTH);
        mainPanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
        getContentPane().add(mainPanel);
        pack();
        setSize(350, 474);
        setVisible(true); // let someone see the dialog.

    }

    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {

        return true;
    }
    
}
