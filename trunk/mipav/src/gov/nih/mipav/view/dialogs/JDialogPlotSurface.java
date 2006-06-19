package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.io.*;

import javax.swing.*;


/**
 * Plots a surface of an image as a function as height surface. If the image is 3D, the current slice is plotted.
 *
 * @version  0.1 June, 2001
 * @author   Matthew J. McAuliffe, Ph.D.
 * @see      ModelTriangleMesh
 * @see      ModelQuadMesh
 */
public class JDialogPlotSurface extends JDialogBase implements AlgorithmInterface, ScriptableInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 1362109646657351750L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private String directory = null;

    /** DOCUMENT ME! */
    private AlgorithmHeightFunction extractSurAlgo;

    /** DOCUMENT ME! */
    private JButton fileButton;

    /** DOCUMENT ME! */
    private String fileName = null;

    /** DOCUMENT ME! */
    private JTextField fileTF;

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** DOCUMENT ME! */
    private int mesh;

    /** DOCUMENT ME! */
    private int slice;

    /** DOCUMENT ME! */
    private int subSample;

    /** DOCUMENT ME! */
    private JComboBox subSampleCBox;

    /** DOCUMENT ME! */
    private JComboBox typeComboBox;

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogPlotSurface() { }

    /**
     * Used primarily for the script to store variables and run the algorithm. No actual dialog will appear but the set
     * up info and result image will be stored here.
     *
     * @param  UI  The user interface, needed to create the image frame.
     * @param  im  Source image.
     */
    public JDialogPlotSurface(ViewUserInterface UI, ModelImage im) {
        super();
        userInterface = UI;
        image = im;
        this.slice = slice;
    }

    /**
     * Creates a new JDialogPlotSurface object.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     * @param  slice           Slice on the image.
     */
    public JDialogPlotSurface(JFrame theParentFrame, ModelImage im, int slice) {
        super(theParentFrame, true);
        image = im;
        userInterface = ((ViewJFrameBase) (parentFrame)).getUserInterface();
        this.slice = slice;
        init();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Takes the following actions:
     *
     * <ul>
     *   <li>Choose file - calls the open file dialog so the user can choose where to save the file</li>
     *   <li>OK Button - gets the file name from the text box and the subsample and mesh parameters from their
     *     respective combo boxes and calls the algorithm</li>
     *   <li>Cancel Button - closes the dialog without doing anything</li>
     * </ul>
     *
     * @param  event  Event that triggers function.
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();

        if (command.equals("Choose")) {

            try {

                JFileChooser chooser = new JFileChooser();

                if (userInterface.getDefaultDirectory() != null) {
                    chooser.setCurrentDirectory(new File(userInterface.getDefaultDirectory()));
                } else {
                    chooser.setCurrentDirectory(new File(System.getProperties().getProperty("user.dir")));
                }

                chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.TECH));

                int returnVal = chooser.showOpenDialog(this);

                if (returnVal == JFileChooser.APPROVE_OPTION) {
                    fileName = chooser.getSelectedFile().getName();
                    directory = chooser.getCurrentDirectory().getName() + File.separatorChar;
                    fileTF.setText(fileName);
                    userInterface.setDefaultDirectory("" + chooser.getCurrentDirectory() + File.separatorChar);
                }

                fileTF.setPreferredSize(new Dimension(fileTF.getHorizontalVisibility().getMaximum() + 5,
                                                      fileTF.getPreferredSize().height));
                pack();
            } catch (OutOfMemoryError e) {
                MipavUtil.displayError("Out of memory!");

                return;
            }
        } else if (command.equals("OK")) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Cancel")) {
            dispose();
        }
    }

    // ************************************************************************
    // ************************** Algorithm Events ****************************
    // ************************************************************************

    /**
     * This method is required if the AlgorithmPerformed interface is implemented. It is called by the algorithms when
     * it has completed or failed to to complete, so that the dialog can be display the result image and/or clean up.
     *
     * @param  algorithm  algorithm that caused the event.
     */
    public void algorithmPerformed(AlgorithmBase algorithm) {

        if (algorithm instanceof AlgorithmHeightFunction) {
            image.clearMask();

            if (extractSurAlgo.isCompleted()) {

                // The algorithm has completed .
                System.gc();
            } else {

                // algorithm failed but result image still has garbage
                System.gc();
            }
        }

        insertScriptLine(algorithm);
        dispose();
    }

    /**
     * If a script is being recorded and the algorithm is done, add an entry for this algorithm.
     *
     * @param  algo  the algorithm to make an entry for
     */
    public void insertScriptLine(AlgorithmBase algo) {

        if (algo.isCompleted()) {

            if (userInterface.isScriptRecording()) {

                // check to see if the match image is already in the ImgTable
                if (userInterface.getScriptDialog().getImgTableVar(image.getImageName()) == null) {

                    if (userInterface.getScriptDialog().getActiveImgTableVar(image.getImageName()) == null) {
                        userInterface.getScriptDialog().putActiveVar(image.getImageName());
                    }
                }

                userInterface.getScriptDialog().append("PlotSurface " +
                                                       userInterface.getScriptDialog().getVar(image.getImageName()) +
                                                       " " + fileName + " " + slice + " " + mesh + " " + subSample +
                                                       "\n");
            }
        }
    }

    /**
     * Run this algorithm from a script.
     *
     * @param   parser  the script parser we get the state from
     *
     * @throws  IllegalArgumentException  if there is something wrong with the arguments in the script
     */
    public void scriptRun(AlgorithmScriptParser parser) throws IllegalArgumentException {
        String srcImageKey = null;

        try {
            srcImageKey = parser.getNextString();
        } catch (Exception e) {
            throw new IllegalArgumentException();
        }

        ModelImage im = parser.getImage(srcImageKey);

        image = im;
        userInterface = image.getUserInterface();
        parentFrame = image.getParentFrame();

        try {
            setFileName(parser.getNextString());
            setSlice(parser.getNextInteger());
            setMeshType(parser.getNextInteger());
            setSubSample(parser.getNextInteger());
        } catch (Exception e) {
            throw new IllegalArgumentException();
        }

        setSeparateThread(false);
        callAlgorithm();
    }

    /**
     * Accessor that sets the file name. Only the file name and should end in ".sur"
     *
     * @param  name  Name of the file where the surface is to be saved.
     */
    public void setFileName(String name) {
        fileName = name;
    }

    /**
     * Accessor that sets the mesh type, quad or triangle.
     *
     * @param  type  The mesh type (Quad or Triangle).
     */
    public void setMeshType(int type) {
        mesh = type;
    }

    /**
     * Accessor that sets the slice number where the surface is to be taken from.
     *
     * @param  type  The slice number.
     */
    public void setSlice(int type) {
        slice = type;
    }

    /**
     * Accessor that sets the subsample size.
     *
     * @param  type  The subsample size.
     */
    public void setSubSample(int type) {
        subSample = type;
    }

    /**
     * Once all the necessary variables are set, call the Plot surface algorithm.
     */
    private void callAlgorithm() {

        try {

            // Make algorithm
            extractSurAlgo = new AlgorithmHeightFunction(image, subSample, fileName, slice, mesh);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed of failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            extractSurAlgo.addListener(this);

            // Hide dialog
            setVisible(false);

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still have user interface work fast
                if (extractSurAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                if (!userInterface.isAppFrameVisible()) {
                    extractSurAlgo.setProgressBarVisible(false);
                }

                extractSurAlgo.run();
            }
        } catch (OutOfMemoryError x) {
            MipavUtil.displayError("Dialog extract surface: unable to allocate enough memory");

            return;
        }
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setForeground(Color.black);
        setTitle("Extract Height Surface");

        JPanel subSamplePanel = new JPanel();
        subSamplePanel.setLayout(new BorderLayout());
        subSamplePanel.setForeground(Color.black);
        subSamplePanel.setBorder(buildTitledBorder("Subsample image by:"));

        subSampleCBox = new JComboBox();
        subSampleCBox.setFont(MipavUtil.font12);
        subSampleCBox.setToolTipText("Subsample by");
        subSampleCBox.addItem("1");
        subSampleCBox.addItem("2");
        subSampleCBox.addItem("3");
        subSampleCBox.addItem("4");
        subSampleCBox.addItem("8");

        subSamplePanel.add(subSampleCBox);

        JPanel filePanel = new JPanel();
        filePanel.setLayout(new GridBagLayout());
        filePanel.setForeground(Color.black);
        filePanel.setBorder(buildTitledBorder("Save surface"));

        fileButton = new JButton("Choose...");
        fileButton.setFont(serif12B);
        fileButton.setMinimumSize(MipavUtil.defaultButtonSize);
        fileButton.setPreferredSize(MipavUtil.defaultButtonSize);
        fileButton.addActionListener(this);
        fileButton.setActionCommand("Choose");

        fileTF = new JTextField();
        fileTF.setText(makeImageName(image.getImageName(), ".sur"));
        fileTF.setMinimumSize(new Dimension(100, 30));
        fileTF.setPreferredSize(new Dimension(fileTF.getHorizontalVisibility().getMaximum() + 5,
                                              fileTF.getPreferredSize().height));
        fileTF.setFont(serif12);

        JLabel saveLabel = new JLabel("Type of mesh");
        saveLabel.setFont(serif12B);
        saveLabel.setForeground(Color.black);

        typeComboBox = new JComboBox();
        typeComboBox.setFont(MipavUtil.font12);
        typeComboBox.setMinimumSize(new Dimension(100, 30));
        typeComboBox.setPreferredSize(new Dimension(100, 30));
        typeComboBox.setToolTipText("Type of mesh to save");
        typeComboBox.addItem("Quad");
        typeComboBox.addItem("Triangle");

        Insets insets = new Insets(0, 2, 0, 2);
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.insets = insets;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        filePanel.add(fileButton, gbc);
        gbc.gridx = 1;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        filePanel.add(fileTF, gbc);
        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        filePanel.add(saveLabel, gbc);
        gbc.gridx = 1;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        filePanel.add(typeComboBox, gbc);

        JPanel buttonPanel = new JPanel();
        buildOKButton();
        buttonPanel.add(OKButton);

        buildCancelButton();
        buttonPanel.add(cancelButton);

        JPanel mainPanel = new JPanel();
        mainPanel.setLayout(new BorderLayout());
        mainPanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
        mainPanel.add(subSamplePanel, BorderLayout.NORTH);
        mainPanel.add(filePanel, BorderLayout.CENTER);
        mainPanel.add(buttonPanel, BorderLayout.SOUTH);

        getContentPane().add(mainPanel);

        pack();
        setVisible(true);
        System.gc();
    }

    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {

        fileName = fileTF.getText();
        subSample = Integer.valueOf((String) subSampleCBox.getSelectedItem()).intValue();
        mesh = typeComboBox.getSelectedIndex();

        return true;
    }

}
