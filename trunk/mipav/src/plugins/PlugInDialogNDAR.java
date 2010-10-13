import gov.nih.mipav.plugins.JDialogStandalonePlugin;

import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.utilities.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.components.WidgetFactory;


import gov.nih.ndar.ws.accession.VToolSimpleAccessionClient;
import gov.nih.ndar.ws.client.Startup;

import java.awt.*;
import java.awt.event.*;
import java.awt.image.MemoryImageSource;
import java.io.*;
import java.text.NumberFormat;
import java.util.*;
import java.util.List;
import java.util.zip.*;

import javax.swing.*;
import javax.swing.event.*;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.TableCellEditor;
import javax.xml.namespace.QName;

import org.apache.axiom.om.*;
import org.apache.axis2.AxisFault;

import WildMagic.LibFoundation.Mathematics.*;

import com.sun.jimi.core.*;


public class PlugInDialogNDAR extends JDialogStandalonePlugin implements ActionListener, ChangeListener, ItemListener,
        TreeSelectionListener, MouseListener, PreviewImageContainer {

    /** Scrolling text area for log output */
    private WidgetFactory.ScrollTextArea logOutputArea;

    private JScrollPane listPane;

    private JTable sourceTable;

    private JLabel outputDirLabel;

    private JButton addSourceButton, finishButton, removeSourceButton, completeDataElementsButton, outputDirButton;

    private JPanel outputDirPanel;

    private JPanel previewPanel, leftPanel;

    private JTextField outputDirTextField;

    private ViewTableModel sourceTableModel;

    private String outputDirBase = System.getProperty("user.home") + File.separator + "mipav" + File.separator
            + "NDAR_Imaging_Submission" + File.separator;

    /** Length of the NDAR GUID */
    private static final int GUID_LENGTH = 12;

    private Hashtable<File, Boolean> multiFileTable = null;

    /** this is the hashtabele of file and associated dataStructure(s) actual user data that gets saved to the xml file**/
    private Hashtable<File, Hashtable<String, LinkedHashMap<String, String>>> infoTable = null;
    
    /** this is a hastable of file and arraylist of selected data structure names **/
    private Hashtable<File, ArrayList<String>> selectedFileDataStructureNames = null;
    
    /** this is an arraylist of selected DataStruct Objects **/
    private ArrayList<DataStruct> selectedFileDataStructures = null;

    private Hashtable<File, String> outputFileNameBaseTable = null;

    //private DataStruct imageDataStruct;

    /** tab level counter for writing xml header. */
    protected int tabLevel = 0;

    /** Buffered writer for writing to XML file */
    protected BufferedWriter bw;

    protected FileWriter fw;

    protected static final String TAB = "\t";

    /** XML encoding string. */
    protected static final String XML_ENCODING = "UTF-8";

    private OMElement dataStructElement, documentElement;
    
    /** hash table of OMElements **/
    private Hashtable<String, OMElement> documentElements = new Hashtable<String, OMElement>();

    private String namespace;

    private final ArrayList<ViewJComponentPreviewImage> previewImages = new ArrayList<ViewJComponentPreviewImage>();

    private final ArrayList<float[]> previewImagesWinLevs = new ArrayList<float[]>();

    /** DOCUMENT ME! */
    private final int origBrightness = 0;

    private JLabel current, current2;

    private JSlider brightnessSlider, contrastSlider;

    /** DOCUMENT ME! */
    private NumberFormat nfc;

    /** DOCUMENT ME! */
    private final float origContrast = 1;

    private JPanel brightnessContrastPanel;

    private ViewJComponentPreviewImage previewImg;

    /** DOCUMENT ME! */
    private float contrast = 1;

    /** DOCUMENT ME! */
    private int brightness = 0;
    
    protected VToolSimpleAccessionClient client;
    
    private OMElement publishedDataStructs = null;

    /** Text of the NDAR privacy notice displayed to the user before the plugin can be used. */
    public static final String NDAR_PRIVACY_NOTICE = "MIPAV is a collaborative environment with privacy rules that pertain to the collection\n"
            + "and display of imaging data. Before accessing and using MIPAV, please ensure that you\n"
            + "familiarize yourself with our privacy rules, available through the NDAR Rules of Behavior\n"
            + "document and supporting documentation.\n"
            + "\n"
            + "Collection of this information is authorized under 42 U.S.C. 241, 242, 248, 281(a)(b)(1)(P)\n"
            + "and 44 U.S.C. 3101. The primary use of this information is to facilitate medical research\n"
            + "around autism and autism treatment. This information may be disclosed to researchers for\n"
            + "research purposes, and to system administrators for evaluation and data normalization.\n"
            + "\n"
            + "Rules governing submission of this information are based on the data sharing rules defined\n"
            + "in the Notice of Grant Award (NOGA). If you do not have a grant defining data sharing\n"
            + "requirements, data submission is voluntary.  Data entered into NDAR will be used solely for\n"
            + "scientific and research purposes and is designed to further the understanding of autism and\n"
            + "autism treatments.  Modification of NDAR information may be addressed by contacting your NDAR\n"
            + "system administrator at ndarhelp@nih.gov. Significant system update information may be posted\n"
            + "on the NDAR site as required.";

    private enum NDARServer {
        PROD("PROD"),
        DEMO("DEMO");

        public String name;

        NDARServer(final String s) {
            name = s;
        }
    }

    public PlugInDialogNDAR() {
        super(false);
        Icon icon = null;
        try {
            icon = new ImageIcon(MipavUtil.getIconImage(Preferences.getIconName()));
        } catch (final Exception e) {

        }
        final int response = JOptionPane.showConfirmDialog(this, PlugInDialogNDAR.NDAR_PRIVACY_NOTICE,
                "NDAR Image Submission Package Creation Tool", JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE);

        if (response == JOptionPane.YES_OPTION) {
            init();
            setVisible(true);
            validate();
        } else {
            if (JDialogStandalonePlugin.isExitRequired()) {
                System.exit(0);
                // ViewUserInterface.getReference().windowClosing(new WindowEvent(this, WindowEvent.WINDOW_CLOSING));
            } else {
                return;
            }
        }

        final Thread thread = new WebServiceThread(this);
        thread.start();

    }

    public void actionPerformed(final ActionEvent e) {

        /*
         * @todo Implement this java.awt.event.ActionListener abstract method
         */

        final String command = e.getActionCommand();

        // System.err.println("size : " + this.getSize());

        if (command.equals("AddSource")) {
            final ViewFileChooserBase fileChooser = new ViewFileChooserBase(true, false);
            fileChooser.setMulti(ViewUserInterface.getReference().getLastStackFlag());

            final JFileChooser chooser = fileChooser.getFileChooser();
            chooser.setCurrentDirectory(new File(ViewUserInterface.getReference().getDefaultDirectory()));

            // default to TECH filter
            int filter = ViewImageFileFilter.TECH;

            try {
                filter = Integer.parseInt(Preferences.getProperty(Preferences.PREF_FILENAME_FILTER));
            } catch (final NumberFormatException nfe) {

                // an invalid value was set in preferences -- so don't use it!
                filter = -1;
            }

            chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.GEN));
            chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.TECH));
            chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.MICROSCOPY));
            chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.MISC));

            if (filter != -1) {
                // it seems that the set command adds the filter again...
                // chooser.addChoosableFileFilter(new ViewImageFileFilter(filter));

                // if filter is something we already added, then remove it before
                // setting it..... (kludgy, kludgy....)
                final javax.swing.filechooser.FileFilter found = ViewOpenFileUI.findFilter(chooser, filter);

                if (found != null) {
                    chooser.removeChoosableFileFilter(found);
                }

                // initially set to the preferences
                chooser.setFileFilter(new ViewImageFileFilter(filter));
            }

            final int returnVal = chooser.showOpenDialog(this);

            if (returnVal == JFileChooser.APPROVE_OPTION) {
                final boolean isMultiFile = fileChooser.isMulti();

                final File[] files = chooser.getSelectedFiles();
                ViewUserInterface.getReference().setDefaultDirectory(files[0].getParent());
                for (int i = 0; i < files.length; i++) {

                    // if ( !sourceModel.contains(files[i])) {
                    if ( !contains(files[i])) {
                    	
                    	
                    	

	                    	FileIO fileIO = new FileIO();
	                        fileIO.setQuiet(true);
	                        ModelImage srcImage = fileIO.readImage(files[i].getName(), files[i].getParent()
	                                + File.separator, isMultiFile, null);
	                        
	                        if(srcImage != null) {
	                        	new ChooseDataStructDialog(this, files[i], isMultiFile, srcImage);
	                        }


                        //new InfoDialog(this, files[i], false);
                    }
                }
            }
            removeSourceButton.setEnabled(sourceTableModel.getRowCount() > 0);
            completeDataElementsButton.setEnabled(sourceTableModel.getRowCount() > 0);
            listPane.setBorder(buildTitledBorder(sourceTableModel.getRowCount() + " image(s) "));

        } else if (command.equals("RemoveSource")) {
            final int selected = sourceTable.getSelectedRow();

            final File f = (File) sourceTableModel.getValueAt(selected, 0);
            sourceTableModel.removeRow(selected);
            multiFileTable.remove(f);
            infoTable.remove(f);
            selectedFileDataStructureNames.remove(f);
            selectedFileDataStructures.remove(f);
            outputFileNameBaseTable.remove(f);

            previewImages.remove(selected);
            previewPanel.removeAll();
            previewPanel.repaint();

            removeSourceButton.setEnabled(sourceTableModel.getRowCount() > 0);
            completeDataElementsButton.setEnabled(sourceTableModel.getRowCount() > 0);
            if (sourceTableModel.getRowCount() > 0) {
                enableDisableFinishButton();

                if (selected >= sourceTableModel.getRowCount()) {
                    sourceTable.setRowSelectionInterval(sourceTableModel.getRowCount() - 1, sourceTableModel
                            .getRowCount() - 1);
                } else {
                    sourceTable.setRowSelectionInterval(selected, selected);
                }
                previewPanel.add(previewImages.get(sourceTable.getSelectedRow()));
                previewImages.get(sourceTable.getSelectedRow()).setSliceBrightness(brightness, contrast);
                previewPanel.validate();
                previewPanel.repaint();
            } else {
                finishButton.setEnabled(false);

            }
            listPane.setBorder(buildTitledBorder(sourceTableModel.getRowCount() + " image(s) "));
        } else if (command.equals("Help")) {

            // MipavUtil.showHelp("ISPImages01");

        } else if (command.equals("Finish")) {
            final SwingWorker<Object, Object> worker = new SwingWorker<Object, Object>() {
                public Object doInBackground() {
                    createSubmissionFiles();

                    return null;
                }
            };
            final int response = JOptionPane.showConfirmDialog(this, "Done adding image datasets?",
                    "Done adding image datasets?", JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE);

            final ArrayList<String> incolmpleteFileNames = new ArrayList<String>();
            for (int i = 0; i < sourceTableModel.getRowCount(); i++) {
                final File f1 = (File) sourceTableModel.getValueAt(i, 0);
                // see if there is
                final Set keySet = infoTable.keySet();
                final Iterator iter = keySet.iterator();
                boolean found = false;
                while (iter.hasNext()) {
                    final File f2 = (File) iter.next();

                    if (f1.getName().equals(f2.getName())) {
                        found = true;
                        break;
                    }
                }
                if ( !found) {
                    incolmpleteFileNames.add(f1.getName());
                }
            }
            if (incolmpleteFileNames.size() > 0) {
                final StringBuffer names = new StringBuffer();
                for (int i = 0; i < incolmpleteFileNames.size(); i++) {
                    names.append(" - " + incolmpleteFileNames.get(i) + "\n");
                }
                MipavUtil.displayError("Please complete required fields for the following images: \n"
                        + names.toString());
                return;
            }

            if (response == JOptionPane.YES_OPTION) {
                worker.execute();
                removeSourceButton.setEnabled(false);
                finishButton.setEnabled(false);
                outputDirButton.setEnabled(false);
                addSourceButton.setEnabled(false);
                completeDataElementsButton.setEnabled(false);

            }

        } else if (command.equals("completeDataElements")) {
            final File f = (File) sourceTableModel.getValueAt(sourceTable.getSelectedRow(), 0);
            final String completed = (String) sourceTableModel.getValueAt(sourceTable.getSelectedRow(), 1);
            if (completed.equals("No")) {
                new InfoDialog(this, f, false);
            } else {
                new InfoDialog(this, f, true);
            }
        } else if (command.equalsIgnoreCase("outputDirBrowse")) {
            final JFileChooser chooser = new JFileChooser();

            chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
            chooser.setDialogTitle("Choose output directory");
            final int returnValue = chooser.showOpenDialog(this);
            if (returnValue == JFileChooser.APPROVE_OPTION) {
                outputDirTextField.setText(chooser.getSelectedFile().getAbsolutePath() + File.separator);

                outputDirBase = chooser.getSelectedFile().getAbsolutePath() + File.separator;

            }
        }
    }

    /**
     * Sets values based on knob along slider.
     * 
     * @param e Event that triggered this function
     */
    public void stateChanged(final ChangeEvent e) {
        final Object source = e.getSource();

        if (source == brightnessSlider) {
            brightness = brightnessSlider.getValue();
            current.setText(String.valueOf(brightness));

            // Change only the brightness and contrast of the current slice
            if (previewImg != null) {
                previewImages.get(sourceTable.getSelectedRow()).setSliceBrightness(brightness, contrast);
            }
        } else if (source == contrastSlider) {
            contrast = (float) Math.pow(10.0, contrastSlider.getValue() / 200.0);
            current2.setText(String.valueOf(nfc.format(contrast)));

            // Change only the brightness and contrast of the current slice
            if (previewImg != null) {
                previewImages.get(sourceTable.getSelectedRow()).setSliceBrightness(brightness, contrast);
            }
        }
    }

    public void itemStateChanged(final ItemEvent e) {

    }

    private void init() {
        setTitle("NDAR Image Submission Package Creation Tool v1.0");

        multiFileTable = new Hashtable<File, Boolean>();
        
        infoTable = new Hashtable<File, Hashtable<String, LinkedHashMap<String, String>>>();
        selectedFileDataStructureNames = new Hashtable<File, ArrayList<String>>();
        selectedFileDataStructures = new ArrayList<DataStruct>();
        
        outputFileNameBaseTable = new Hashtable<File, String>();
        final JPanel topPanel = new JPanel(new GridBagLayout());
        final GridBagConstraints gbc2 = new GridBagConstraints();
        final GridBagConstraints gbc3 = new GridBagConstraints();
        gbc2.anchor = GridBagConstraints.NORTHWEST;

        buildBrightnessContrastPanel();

        /*
         * leftPanel = new JPanel(new GridBagLayout()); leftPanel.setBorder(buildTitledBorder("Preview image"));
         * leftPanel.setPreferredSize(new Dimension(200, 350));
         */

        previewPanel = new JPanel();
        previewPanel.setBorder(buildTitledBorder("Preview image"));
        previewPanel.setPreferredSize(new Dimension(200, 300));
        /*
         * gbc3.gridy = 0; gbc3.gridx = 0; gbc3.fill = GridBagConstraints.BOTH; leftPanel.add(previewPanel, gbc3);
         * gbc3.gridy = 1; leftPanel.add(brightnessContrastPanel, gbc3);
         */

        gbc2.gridy = 0;
        gbc2.gridx = 0;
        gbc2.fill = GridBagConstraints.BOTH;
        gbc2.weightx = 0;
        gbc2.weighty = 0;
        topPanel.add(previewPanel, gbc2);
        gbc2.gridy = 1;
        topPanel.add(brightnessContrastPanel, gbc2);

        gbc2.gridy = 0;
        gbc2.gridx = 1;
        gbc2.weightx = 1;
        gbc2.weighty = 1;
        gbc2.gridheight = 2;
        topPanel.add(buildSourcePanel(), gbc2);

        getContentPane().add(topPanel, BorderLayout.NORTH);

        getContentPane().add(buildLogPanel(), BorderLayout.CENTER);
        getContentPane().add(buildButtonPanel(), BorderLayout.SOUTH);
        pack();
        validate();
        this.setMinimumSize(this.getSize());
        // this.setSize(new Dimension(610, 537));
    }

    /**
     * Build a panel for the zip and metadata file creation log.
     */
    private JPanel buildLogPanel() {
        final JPanel destPanel = new JPanel(new GridBagLayout());

        final GridBagConstraints gbc2 = new GridBagConstraints();
        gbc2.anchor = GridBagConstraints.CENTER;

        gbc2.gridy = 0;
        gbc2.gridx = 0;

        logOutputArea = WidgetFactory.buildScrollTextArea(Color.white);
        logOutputArea.setBorder(buildTitledBorder("Output log"));
        logOutputArea.getTextArea().setEditable(false);
        logOutputArea.getTextArea().setRows(10);
        outputDirPanel = new JPanel();
        outputDirLabel = new JLabel("Output Directory ");
        outputDirTextField = new JTextField(30);
        outputDirTextField.setEditable(false);
        outputDirTextField.setToolTipText(outputDirBase);
        outputDirTextField.setText(outputDirBase);
        outputDirButton = WidgetFactory.buildTextButton("Browse", "Choose Output Directory", "outputDirBrowse", this);
        outputDirButton.setPreferredSize(MipavUtil.defaultButtonSize);
        outputDirPanel.add(outputDirLabel);
        outputDirPanel.add(outputDirTextField);
        outputDirPanel.add(outputDirButton);

        destPanel.add(outputDirPanel, gbc2);

        gbc2.gridy = 1;
        gbc2.fill = GridBagConstraints.BOTH;
        gbc2.weightx = 1;
        gbc2.weighty = 1;
        gbc2.anchor = GridBagConstraints.NORTHWEST;
        destPanel.add(logOutputArea, gbc2);

        return destPanel;
    }

    public void valueChanged(final TreeSelectionEvent e) {

    }

    private JScrollPane buildSourcePanel() {
        // final GridBagConstraints gbc = new GridBagConstraints();

        // gbc.anchor = GridBagConstraints.NORTHWEST;
        // gbc.weightx = 1;
        // gbc.weighty = 1;
        // gbc.fill = GridBagConstraints.BOTH;

        sourceTableModel = new ViewTableModel();
        sourceTableModel.addColumn("Image Name");
        sourceTableModel.addColumn("Completed?");

        sourceTable = new JTable(sourceTableModel);
        sourceTable.addMouseListener(this);
        sourceTable.setPreferredScrollableViewportSize(new Dimension(500, 350));
        sourceTable.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);

        sourceTable.setAutoResizeMode(JTable.AUTO_RESIZE_ALL_COLUMNS);
        sourceTable.getColumn("Completed?").setMinWidth(100);
        sourceTable.getColumn("Completed?").setMaxWidth(100);

        sourceTable.getColumn("Completed?").setCellRenderer(new MyRightCellRenderer());

        listPane = WidgetFactory.buildScrollPane(sourceTable);
        listPane.setBorder(buildTitledBorder(0 + " image(s) "));

        return listPane;
    }

    /**
     * Checks to see if the given string is a valid NDAR GUID
     * 
     * @param checkString the string to check
     * @return whether this is a valid guid
     */
    private boolean isValidGUID(final String checkString) {
        if (checkString.length() != PlugInDialogNDAR.GUID_LENGTH) {
            return false;
        }

        if (isValidChar(checkString.charAt(4)) && isValidChar(checkString.charAt(5))
                && isNumChar(checkString.charAt(6)) && isNumChar(checkString.charAt(7))
                && isNumChar(checkString.charAt(8)) && isValidChar(checkString.charAt(9))
                && isValidChar(checkString.charAt(10))
                && (isNumChar(checkString.charAt(11)) || isValidChar(checkString.charAt(11)))) {
            return true;
        }
        return false;
    }

    /**
     * Is the char a valid number character
     * 
     * @param checkChar char to check
     * @return whether is a number
     */
    private boolean isNumChar(final char checkChar) {
        return (checkChar >= '0' && checkChar <= '9');
    }

    /**
     * Check if this is a valid NDAR character ( no I, O, Q, or S)
     * 
     * @param checkChar char to check
     * @return is the char valid
     */
    private boolean isValidChar(final char checkChar) {
        if ( (checkChar >= 'a' && checkChar <= 'z') || (checkChar >= 'A' && checkChar <= 'Z')) {
            if (checkChar != 'i' && checkChar != 'I' && checkChar != 'o' && checkChar != 'O' && checkChar != 'q'
                    && checkChar != 'Q' && checkChar != 's' && checkChar != 'S') {
                return true;
            }
        }

        return false;
    }

    private ModelImage createThumbnailImage(final ModelImage origImage) {
        ModelImage thumbnailImage = null;

        // create a thumbnail image...4 colums, 2 rows
        // grab the middle 8 slices from the image for the thumbnail
        // need to determine by what percentage...so...need to figure out by what percebtahe the xdim will go down
        // to 128
        // startSLice will be 3 less than middle slice
        // endSlice will be 4 more than middle slixe
        final int xDim = origImage.getExtents()[0];
        int percentage = 100;
        if (xDim > 128) {
            final float perc = 128f / xDim * 100;
            percentage = (int) Math.floor(perc);
        }
        final int columns = 4;
        final int rows = 2;
        final int rBorderVal = 255;
        final int gBorderVal = 0;
        final int bBorderVal = 0;
        final int borderThick = 1;
        int startSlice = 0;
        int endSlice = 0;
        int numSlices = 0;
        int middleSlice = 0;
        LightboxGenerator lightGen;

        if (origImage.is2DImage()) {
            // Creating a blank TransMatrix for resampling
            final TransMatrix percentSizer = new TransMatrix(4);
            percentSizer.Set(1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1);

            // Resample image size based on percent inputted
            final AlgorithmTransform transformer = new AlgorithmTransform(origImage, percentSizer, 1,
                    (float) (origImage.getResolutions(0)[0] / (percentage * .01)),
                    (float) (origImage.getResolutions(0)[1] / (percentage * .01)), (int) (origImage.getExtents()[0]
                            * percentage * .01), (int) (origImage.getExtents()[1] * percentage * .01), origImage
                            .getUnitsOfMeasure(), false, true, false, true, origImage.getImageCentermm(false));
            transformer.runAlgorithm();
            thumbnailImage = transformer.getTransformedImage();
            thumbnailImage.calcMinMax();
            // convert this image to color image if it is not
            if ( !thumbnailImage.isColorImage()) {
                final ModelImage newRGB = new ModelImage(ModelStorageBase.ARGB, thumbnailImage.getExtents(),
                        thumbnailImage.getImageName());
                final AlgorithmRGBConcat mathAlgo = new AlgorithmRGBConcat(thumbnailImage, thumbnailImage,
                        thumbnailImage, newRGB, true, true, 255.0f, true);
                mathAlgo.run();
                thumbnailImage.disposeLocal();
                thumbnailImage = null;
                thumbnailImage = newRGB;
            }
        } else if (origImage.is3DImage()) {
            numSlices = origImage.getExtents()[2];
            numSlices = numSlices - 1; // its 0 based
            middleSlice = numSlices / 2;
            startSlice = middleSlice - 3;
            if (startSlice < 0) {
                startSlice = 0;
            }
            endSlice = middleSlice + 4;
            if (endSlice > numSlices - 1) {
                endSlice = numSlices - 1;
            }

            try {
                // Make algorithm
                lightGen = new LightboxGenerator(origImage, startSlice, endSlice, percentage, rows, columns,
                        rBorderVal, gBorderVal, bBorderVal, false, borderThick);
                lightGen.run();
                thumbnailImage = lightGen.getImage();
                thumbnailImage.calcMinMax();
            } catch (final Exception e) {
                e.printStackTrace();
            }
        } else if (origImage.is4DImage()) {
            // get middle time volume
            final int[] destExtents = new int[3];
            final int xSlices = origImage.getExtents()[0];
            final int ySlices = origImage.getExtents()[1];
            final int zSlices = origImage.getExtents()[2];
            destExtents[0] = xSlices;
            destExtents[1] = ySlices;
            destExtents[2] = zSlices;

            ModelImage timeImage = new ModelImage(origImage.getType(), destExtents, "");

            final int tSlices = origImage.getExtents()[3];
            int middleVol = (int) Math.floor(tSlices / 2);
            if (middleVol > 0) {
                middleVol = middleVol - 1; // 0 based
            }
            final AlgorithmSubset subsetAlgo = new AlgorithmSubset(origImage, timeImage, AlgorithmSubset.REMOVE_T,
                    middleVol);
            subsetAlgo.run();

            numSlices = timeImage.getExtents()[2];
            numSlices = numSlices - 1; // its 0 based
            middleSlice = numSlices / 2;
            startSlice = middleSlice - 3;
            if (startSlice < 0) {
                startSlice = 0;
            }
            endSlice = middleSlice + 4;
            if (endSlice > numSlices - 1) {
                endSlice = numSlices - 1;
            }
            try {
                // Make algorithm
                lightGen = new LightboxGenerator(timeImage, startSlice, endSlice, percentage, rows, columns,
                        rBorderVal, gBorderVal, bBorderVal, false, borderThick);
                lightGen.run();
                thumbnailImage = lightGen.getImage();
                thumbnailImage.calcMinMax();
                if (timeImage != null) {
                    timeImage.disposeLocal();
                    timeImage = null;
                }
            } catch (final Exception e) {

            }
        }

        return thumbnailImage;
    }

    /**
     * Create the ZIP(s) containing the original image files and the XML meta-data for each image dataset.
     */
    private void createSubmissionFiles() {

        final File outputDirFile = new File(outputDirBase);
        if ( !outputDirFile.exists()) {
            outputDirFile.mkdirs();

        }
        final int numImages = sourceTableModel.getRowCount();
        for (int i = 0; i < numImages; i++) {
            final File imageFile = (File) sourceTableModel.getValueAt(i, 0);
            final String guid = outputFileNameBaseTable.get(imageFile);
            printlnToLog("Opening: " + imageFile + ", multifile: " + multiFileTable.get(imageFile));

            final FileIO fileIO = new FileIO();
            fileIO.setQuiet(true);
            final ModelImage origImage = fileIO.readImage(imageFile.getName(), imageFile.getParent() + File.separator,
                    multiFileTable.get(imageFile), null);

            final List<String> origFiles = FileUtility.getFileNameList(origImage);

            final int modality = origImage.getFileInfo(0).getModality();
            final String modalityString = FileInfoBase.getModalityStr(modality).replaceAll("\\s+", "");

            String outputFileNameBase;
            if (modality == FileInfoBase.UNKNOWN_MODALITY) {
                outputFileNameBase = guid + "_" + System.currentTimeMillis();
            } else {
                outputFileNameBase = guid + "_" + modalityString + "_" + System.currentTimeMillis();
            }

            final String zipFilePath = outputDirBase + outputFileNameBase + ".zip";

            ModelImage thumbnailImage = createThumbnailImage(origImage);

            // write out thumbnail image
            final FileWriteOptions opts = new FileWriteOptions(outputFileNameBase + ".jpg", outputDirBase, true);
            writeThumbnailJIMI(thumbnailImage, opts);
            if (thumbnailImage != null) {
                thumbnailImage.disposeLocal();
                thumbnailImage = null;
            }
            printlnToLog("Creating thumbnail image:\t" + outputDirBase + outputFileNameBase + ".jpg");
            try {
                printlnToLog("Creating ZIP file:\t" + zipFilePath);
                for (final String file : origFiles) {
                    printlnToLog("Adding file to ZIP:\t" + file);
                }

                makeZipFile(zipFilePath, origFiles);
            } catch (final IOException ioe) {
                ioe.printStackTrace();
                MipavUtil.displayError("Unable to write original image dataset files to ZIP package:\n"
                        + ioe.getMessage());
                continue;
            }

            // now we need to write out the xml...nish
            writeXMLFile(outputDirBase, outputFileNameBase, imageFile, origImage);

            origImage.disposeLocal();

            printlnToLog("");
        }

        printlnToLog("*** Submission package processing complete. ***");

    }

    /**
     * writes out xml file
     * 
     * @param outputDirBase
     * @param outputFileNameBase
     * @param imageFile
     * @param origImage
     */
    private void writeXMLFile(final String outputDirBase, final String outputFileNameBase, final File imageFile,
            final ModelImage origImage) {
        final String xmlFileName = outputFileNameBase + ".xml";
        final String xmlHeader = "<?xml version=\"1.0\" ?>";
        final String xmlSchema = "http://www.w3.org/2001/XMLSchema-instance";
        final String xsd = "schema.xsd";

        try {
            final File xmlFile = new File(outputDirBase + xmlFileName);
            fw = new FileWriter(xmlFile);
            bw = new BufferedWriter(fw);
            bw.write(xmlHeader);
            bw.newLine();
            openTag("data_set xmlns:xsi=\"" + xmlSchema + "\" xsi:noNamespaceSchemaLocation=\"" + xsd + "\"", true);
            // TODO: temporarily changed data structure name to lower case, since that's what the Validation Tool
            // expects
            
            ArrayList<String> selectedDSNames = selectedFileDataStructureNames.get(imageFile);
            Hashtable<String, LinkedHashMap<String, String>> infoMaps = infoTable.get(imageFile);
            
            for(int i=0;i<selectedDSNames.size();i++) {
            	  for(int k=0;k<selectedFileDataStructures.size();k++) {
            		  DataStruct ds = selectedFileDataStructures.get(k);
            		  String shortname = ds.getShortname();
            		  if(selectedDSNames.get(i).equals(shortname)) {
            			  LinkedHashMap<String, String> infoMap = infoMaps.get(shortname);
            			  String n = selectedFileDataStructures.get(i).getName().toLowerCase();
            			  String v = selectedFileDataStructures.get(i).getVersion().replaceFirst("^0", "");
            			  openTag("data_structure name=\"" + n + "\" version=\"" + v + "\"", true);
            			  parse(ds, imageFile, outputFileNameBase, infoMap);
            			  openTag("data_structure", false);
            			  break;
            		  }
            		  
            	  }
            	
            }
            
            openTag("data_set", false);
            
            
            /*for(int i=0;i<selectedFileDataStructures.size();i++) {
            	String n = selectedFileDataStructures.get(i).getName().toLowerCase();
                // TODO: temporarily changed data structure version to remove leading zero, which was also causing
                // Validation Tool problems
                String v = selectedFileDataStructures.get(i).getVersion().replaceFirst("^0", "");
                openTag("data_structure name=\"" + n + "\" version=\"" + v + "\"", true);
                parse(selectedFileDataStructures.get(i), imageFile, outputFileNameBase);
                openTag("data_structure", false);
                openTag("data_set", false);
            }
            */
            
            bw.close();
        } catch (final Exception e) {
            e.printStackTrace();
        }
    }

    /**
     * 
     * @param ds
     */
    private void parse(final DataStruct ds2, final File imageFile, final String outputFileNameBase, LinkedHashMap<String, String> infoMap) {
        Vector<XMLAttributes> attr;
        XMLAttributes xmlAttributes;


        for (int k = 0; k < ds2.size(); k++) {

            final Object o1 = ds2.get(k);
            if (o1 instanceof DataElement) {
                // data element
                final DataElement de = (DataElement) o1;
                final String name = de.getName();
                String value = "";
                String v;
                if (name.equals("image_file")) {
                    value = outputFileNameBase + ".zip";
                } else if (name.equals("image_thumbnail_file")) {
                    value = outputFileNameBase + ".jpg";
                } else {
                    // need to get appropriat value

                    final Set keySet = infoMap.keySet();
                    final Iterator iter = keySet.iterator();
                    String key;
                    while (iter.hasNext()) {
                        key = (String) iter.next();
                        if (key.equals(name)) {
                            v = infoMap.get(key);
                            value = v;
                            break;
                        }
                    }
                }
                if ( !value.trim().equals("")) {
                    attr = new Vector<XMLAttributes>();
                    xmlAttributes = new XMLAttributes("name", name);
                    attr.add(xmlAttributes);
                    xmlAttributes = new XMLAttributes("value", value);
                    attr.add(xmlAttributes);
                    closedTag("data_element", attr);
                }
            }/* else {
                final DataStruct ds3 = (DataStruct) o1;
                final String n = ds3.getName();
                final String v = ds3.getVersion();
                openTag("data_structure name=\"" + n + "\" version=\"" + v + "\"", true);
                parse(ds3, imageFile, outputFileNameBase);
                openTag("data_structure", false);
            }*/

        }
    }

    /**
     * Simple function to write an xml formatted open ended tag (value not included).
     * 
     * @param bw writer to use
     * @param tag tag name
     * @param start is this a start or end tag
     */
    public final void openTag(final String tag, boolean start) {

        try {

            if ( !start) {

                // done with this container
                tabLevel--;
            }

            for (int i = 0; i < tabLevel; i++) {
                bw.write(PlugInDialogNDAR.TAB);
            }

            if (start) {
                bw.write("<" + tag + ">");

                // indent the contained tags
                tabLevel++;
            } else {
                bw.write("</" + tag + ">");
            }

            bw.newLine();
        } catch (final IOException ex) {}
    }

    /**
     * Simple function to write an xml formatted closed tag including the tag value.
     * 
     * @param bw write to use
     * @param tag tag name
     * @param val tag value
     */
    protected final void closedTag(final String tag, final String val) {

        try {

            for (int i = 0; i < tabLevel; i++) {
                bw.write(PlugInDialogNDAR.TAB);
            }

            // entity-ize some xml-unfriendly characters and convert to the XML charset
            String writeVal = val.trim().replaceAll("&", "&amp;");
            writeVal = writeVal.trim().replaceAll("\"", "&quot;");
            writeVal = writeVal.trim().replaceAll("<", "&lt;");
            writeVal = writeVal.trim().replaceAll(">", "&gt;");
            writeVal = new String(writeVal.getBytes(PlugInDialogNDAR.XML_ENCODING));

            bw.write("<" + tag + ">" + writeVal + "</" + tag + ">");
            bw.newLine();
        } catch (final IOException ex) {}
    }

    /**
     * Writes a closed tag where no value is specified, only attributes.
     */
    public final void closedTag(final String tag, final Vector<XMLAttributes> attr) {

        try {

            for (int i = 0; i < tabLevel; i++) {
                bw.write(PlugInDialogNDAR.TAB);
            }

            bw.write("<" + tag);

            String attrStr;
            for (int i = 0; i < attr.size(); i++) {

                attrStr = attr.elementAt(i).getValue().trim().replaceAll("&", "&amp;");
                attrStr = attrStr.trim().replaceAll("\"", "&quot;");
                attrStr = attrStr.trim().replaceAll("<", "&lt;");
                attrStr = attrStr.trim().replaceAll(">", "&gt;");
                attrStr = new String(attrStr.getBytes(PlugInDialogNDAR.XML_ENCODING));

                bw.write(" " + attr.elementAt(i).getName() + "=\"" + attrStr + "\"");
            }

            bw.write("/>");

            bw.newLine();
        } catch (final IOException ex) {}

        attr.clear();
    }

    /**
     * Adds a set of files to a ZIP archive.
     * 
     * @param destZipFile The full path to the ZIP archive to create.
     * @param srcFiles A list of files (full paths) to include in the ZIP archive.
     * @throws IOException If there is a problem reading the srcFiles or writing to the ZIP file.
     */
    private void makeZipFile(final String destZipFile, final List<String> srcFiles) throws IOException {
        // Create a buffer for reading the files
        final byte[] buf = new byte[1024];

        // Create the ZIP file
        final ZipOutputStream out = new ZipOutputStream(new FileOutputStream(destZipFile));

        // Compress the files
        for (final String file : srcFiles) {
            final FileInputStream in = new FileInputStream(file);

            // Add ZIP entry to output stream.
            out.putNextEntry(new ZipEntry(FileUtility.getFileName(file)));

            // Transfer bytes from the file to the ZIP file
            int len;
            while ( (len = in.read(buf)) > 0) {
                out.write(buf, 0, len);
            }

            // Complete the entry
            out.closeEntry();
            in.close();
        }

        // Complete the ZIP file
        out.close();
    }

    /**
     * Append a line to the log output area in the Log tab.
     * 
     * @param line The line to append (do not include the trailing newline).
     */
    private void printlnToLog(final String line) {
        logOutputArea.getTextArea().append(line + "\n");
    }

    private JPanel buildButtonPanel() {

        final JPanel buttonPanel1 = new JPanel(new GridBagLayout());

        final GridBagConstraints gbc = new GridBagConstraints();
        gbc.weightx = 1;
        gbc.weighty = 1;
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.fill = GridBagConstraints.BOTH;

        addSourceButton = WidgetFactory.buildTextButton("Add images", "Add image datasets", "AddSource", this);
        
        removeSourceButton = WidgetFactory.buildTextButton("Remove image", "Remove the selected image dataset",
                "RemoveSource", this);
        finishButton = WidgetFactory.buildTextButton("Finish", "Finish", "Finish", this);
        completeDataElementsButton = WidgetFactory.buildTextButton("Edit Data Elements",
                "Edit data elements for selected image dataset", "completeDataElements", this);
        // helpButton = WidgetFactory.buildTextButton("Help", "Show MIPAV help", "Help", this);

        addSourceButton.setPreferredSize(MipavUtil.defaultButtonSize);
        removeSourceButton.setPreferredSize(MipavUtil.defaultButtonSize);
        finishButton.setPreferredSize(MipavUtil.defaultButtonSize);
        completeDataElementsButton.setPreferredSize(MipavUtil.defaultButtonSize);
        // helpButton.setPreferredSize(MipavUtil.defaultButtonSize);

        addSourceButton.setEnabled(false);
        removeSourceButton.setEnabled(false);
        completeDataElementsButton.setEnabled(false);
        finishButton.setEnabled(false);

        gbc.gridx = 0;
        buttonPanel1.add(addSourceButton, gbc);
        gbc.gridx = 1;
        buttonPanel1.add(removeSourceButton, gbc);
        gbc.gridx = 2;
        buttonPanel1.add(completeDataElementsButton, gbc);
        gbc.gridx = 3;
        buttonPanel1.add(finishButton, gbc);

        return buttonPanel1;
    }

    public Dimension getPanelSize() {
        return new Dimension(previewPanel.getBounds().width, previewPanel.getBounds().height);
    }

    /**
     * Writes a JIMI file to store the image.
     * 
     * @param image The image to write.
     * @param options The options to use to write the image.
     * 
     * @return Flag indicating that this was a successful write.
     */
    private boolean writeThumbnailJIMI(final ModelImage image, final FileWriteOptions options) {
        final int imageSize = image.getExtents()[0] * image.getExtents()[1];
        final int[] paintBuffer = new int[imageSize];
        final ColorRGBA colorMappedA = new ColorRGBA();
        final float[] imageBufferA = new float[image.getExtents()[0] * image.getExtents()[1] * 4];
        final int length = imageBufferA.length;
        final ColorRGB[] m_akOffset = {new ColorRGB(0.0f, 0.0f, 0.0f), new ColorRGB(0.0f, 0.0f, 0.0f)};
        float fMaxColor = 255;
        final float[] m_afNormColor = {1, 1};

        if (image.getMinR() < 0.0) {
            fMaxColor = (float) (image.getMaxR() - image.getMinR());
            m_akOffset[0].R = (float) ( -image.getMinR());
        } else {
            fMaxColor = (float) image.getMaxR();
        }

        if (image.getMinG() < 0.0) {
            fMaxColor = Math.max((float) (image.getMaxG() - image.getMinG()), fMaxColor);
            m_akOffset[0].G = (float) ( -image.getMinG());
        } else {
            fMaxColor = Math.max((float) image.getMaxG(), fMaxColor);
        }

        if (image.getMinB() < 0.0) {
            fMaxColor = Math.max((float) (image.getMaxB() - image.getMinB()), fMaxColor);
            m_akOffset[0].B = (float) ( -image.getMinB());
        } else {
            fMaxColor = Math.max((float) image.getMaxB(), fMaxColor);
        }
        m_afNormColor[0] = 255 / fMaxColor;

        try {
            image.exportData(0, length, imageBufferA);
        } catch (final Exception e) {

        }
        for (int j = 0; j < image.getExtents()[1]; j++) {

            for (int i = 0; i < image.getExtents()[0]; i++) {
                final int ind4 = (j * image.getExtents()[0]) + i;
                final int index = 4 * ind4;
                int pixValue;

                colorMappedA.R = 0;
                colorMappedA.G = 0;
                colorMappedA.B = 0;
                colorMappedA.A = imageBufferA[index];

                colorMappedA.R = (imageBufferA[index + 1] + m_akOffset[0].R) * m_afNormColor[0];
                colorMappedA.G = (imageBufferA[index + 2] + m_akOffset[0].G) * m_afNormColor[0];
                colorMappedA.B = (imageBufferA[index + 3] + m_akOffset[0].B) * m_afNormColor[0];

                pixValue = 0xff000000 | ((int) (colorMappedA.R) << 16) | ((int) (colorMappedA.G) << 8)
                        | ((int) (colorMappedA.B));

                paintBuffer[ind4] = pixValue;
            }
        }

        final MemoryImageSource memImageA = new MemoryImageSource(image.getExtents()[0], image.getExtents()[1],
                paintBuffer, 0, image.getExtents()[0]);

        final int extIndex = options.getFileName().indexOf(".");
        final String prefix = options.getFileName().substring(0, extIndex); // Used for setting file name
        final String fileSuffix = options.getFileName().substring(extIndex);
        String name;

        final Image img = createImage(memImageA);

        name = options.getFileDirectory() + prefix + fileSuffix;

        try {
            Jimi.putImage(img, name);
        } catch (final JimiException jimiException) {
            Preferences.debug("JIMI write error: " + jimiException + "\n", Preferences.DEBUG_FILEIO);

            jimiException.printStackTrace();

            return false;
        }

        return true;
    }

    public void mouseClicked(final MouseEvent e) {
        final Component c = e.getComponent();
        if (c instanceof JTable) {
            if (sourceTable.getSelectedRow() == -1) {
                completeDataElementsButton.setEnabled(false);
                removeSourceButton.setEnabled(false);
                previewPanel.removeAll();
                previewPanel.repaint();
                return;
            } else {
                completeDataElementsButton.setEnabled(true);
                removeSourceButton.setEnabled(true);
            }
            final File f = (File) sourceTableModel.getValueAt(sourceTable.getSelectedRow(), 0);

            previewPanel.removeAll();
            previewPanel.repaint();

            previewPanel.add(previewImages.get(sourceTable.getSelectedRow()));
            previewImages.get(sourceTable.getSelectedRow()).setSliceBrightness(brightness, contrast);
            previewPanel.validate();
            previewPanel.repaint();

            if (e.getClickCount() == 2) {
                final String completed = (String) sourceTableModel.getValueAt(sourceTable.getSelectedRow(), 1);
                if (completed.equals("No")) {
                    new InfoDialog(this, f, false);
                } else {
                    new InfoDialog(this, f, true);
                }
            }
        }
    }

    public void mouseEntered(final MouseEvent e) {
    // TODO Auto-generated method stub

    }

    public void mouseExited(final MouseEvent e) {
    // TODO Auto-generated method stub

    }

    public void mousePressed(final MouseEvent e) {
    // TODO Auto-generated method stub

    }

    public void mouseReleased(final MouseEvent e) {
    // TODO Auto-generated method stub

    }

    public boolean contains(final File f) {
        boolean contains = false;

        for (int i = 0; i < sourceTableModel.getRowCount(); i++) {
            final File f1 = (File) sourceTableModel.getValueAt(i, 0);
            if (f1.getAbsolutePath().equals(f.getAbsolutePath())) {
                contains = true;
                break;
            }
        }
        return contains;
    }

    public void enableDisableFinishButton() {
        boolean allCompleted = true;

        for (int i = 0; i < sourceTableModel.getRowCount(); i++) {
            final String completed = (String) sourceTableModel.getValueAt(i, 1);
            if (completed.equals("No")) {
                allCompleted = false;
                break;
            }
        }

        if (allCompleted) {
            finishButton.setEnabled(true);
        } else {
            finishButton.setEnabled(false);
        }

    }

    private void buildBrightnessContrastPanel() {
        brightnessSlider = new JSlider(SwingConstants.HORIZONTAL, -255, 255, origBrightness);

        brightnessSlider.setMajorTickSpacing(102);
        brightnessSlider.setPaintTicks(true);
        brightnessSlider.setEnabled(true);
        brightnessSlider.addChangeListener(this);

        final JLabel maximum = new JLabel(String.valueOf(255));

        maximum.setForeground(Color.black);
        maximum.setFont(serif12);

        current = new JLabel(String.valueOf(origBrightness));
        current.setForeground(Color.black);
        current.setFont(serif12B);

        final JLabel minimum = new JLabel(String.valueOf( -255));

        minimum.setForeground(Color.black);
        minimum.setFont(serif12);

        final JPanel sliderPanel = new JPanel(new GridBagLayout());
        final GridBagConstraints gbc = new GridBagConstraints();

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 3;
        gbc.weightx = 1;
        gbc.gridheight = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;

        sliderPanel.add(brightnessSlider, gbc);

        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.gridwidth = 1;
        gbc.weightx = 0;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.fill = GridBagConstraints.NONE;

        sliderPanel.add(minimum, gbc);

        gbc.gridx = 1;
        gbc.anchor = GridBagConstraints.CENTER;
        gbc.weightx = .5;

        sliderPanel.add(current, gbc);

        gbc.gridx = 2;
        gbc.anchor = GridBagConstraints.EAST;
        gbc.weightx = 0;

        sliderPanel.add(maximum, gbc);
        sliderPanel.setBorder(buildTitledBorder("Level"));

        contrastSlider = new JSlider(SwingConstants.HORIZONTAL, -200, 200, (int) (Math.round(86.85889638 * Math
                .log(origContrast))));

        contrastSlider.setMajorTickSpacing(80);
        contrastSlider.setPaintTicks(true);
        contrastSlider.setEnabled(true);
        contrastSlider.addChangeListener(this);

        final JLabel maximum2 = new JLabel(String.valueOf(10));

        maximum2.setForeground(Color.black);
        maximum2.setFont(serif12);

        nfc = NumberFormat.getNumberInstance();
        nfc.setMaximumFractionDigits(3);

        current2 = new JLabel(String.valueOf(nfc.format(origContrast)));
        current2.setForeground(Color.black);
        current2.setFont(serif12B);

        final JLabel minimum2 = new JLabel(String.valueOf(0.100));

        minimum2.setForeground(Color.black);
        minimum2.setFont(serif12);

        final JPanel sliderPanel2 = new JPanel(new GridBagLayout());

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 3;
        gbc.weightx = 1;
        gbc.gridheight = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;

        sliderPanel2.add(contrastSlider, gbc);

        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.gridwidth = 1;
        gbc.weightx = 0;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.fill = GridBagConstraints.NONE;

        sliderPanel2.add(minimum2, gbc);

        gbc.gridx = 1;
        gbc.anchor = GridBagConstraints.CENTER;
        gbc.weightx = .5;

        sliderPanel2.add(current2, gbc);

        gbc.gridx = 2;
        gbc.anchor = GridBagConstraints.EAST;
        gbc.weightx = 0;

        sliderPanel2.add(maximum2, gbc);
        sliderPanel2.setBorder(buildTitledBorder("Window"));

        final JPanel centerPanel = new JPanel(new GridBagLayout());

        final GridBagConstraints gbc2 = new GridBagConstraints();

        gbc2.gridx = 0;
        gbc2.gridy = 0;
        gbc2.fill = GridBagConstraints.BOTH;
        gbc2.weightx = 1;
        gbc2.gridheight = 2;
        centerPanel.add(sliderPanel2, gbc2);

        gbc2.gridy = 2;
        centerPanel.add(sliderPanel, gbc2);

        gbc2.gridheight = 1;
        gbc2.gridy = 4;

        brightnessContrastPanel = new JPanel(new BorderLayout());
        brightnessContrastPanel.add(centerPanel);
        brightnessContrastPanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));

    }

    /**
     * 
     * Inner class Right Renderer
     * 
     * @author pandyan
     * 
     */
    private class MyRightCellRenderer extends DefaultTableCellRenderer {

        public Component getTableCellRendererComponent(final JTable table, final Object value,
                final boolean isSelected, final boolean hasFocus, final int row, final int column) {
            final Component comp = super.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column);
            setHorizontalAlignment(SwingConstants.CENTER);

            if (column == 1 && value.equals("No")) {
                setForeground(Color.red);
            } else {
                setForeground(Color.black);
            }

            return comp;
        }

    }
    
    
    private class ChooseDataStructDialog extends JDialog implements ActionListener {
    	private final PlugInDialogNDAR owner;
    	 private final File file;
    	 private ViewTableModel structsModel;
    	 private JTable structsTable;
    	 private QName qName = null;
    	 private QName qStatus = null;
    	 private QName qDataType = null;
    	 private QName qDescription = null;
    	 private QName qParentDataStructure = null;
    	 private QName qVersion = null;
    	 
    	 private ArrayList<String>  descAL = new ArrayList<String>();
    	 private ArrayList<String>  shortNameAL = new ArrayList<String>();
    	 private ArrayList<String>  versionAL = new ArrayList<String>();
    	 private ArrayList<String>  statusAL = new ArrayList<String>();
    	 private JScrollPane structsScrollPane;
    	 private boolean isMultiFile;
    	 private ModelImage srcImage;
    	 
    	public ChooseDataStructDialog(PlugInDialogNDAR owner, final File file, boolean isMultiFile, ModelImage srcImage) {
    		super(owner, true);

            this.owner = owner;
            this.file = file;
            this.isMultiFile = isMultiFile;
            this.srcImage = srcImage;
            
            
            
            

	       qName    = new QName(publishedDataStructs.getNamespace().getNamespaceURI(), "short_name");
	       qStatus    = new QName(publishedDataStructs.getNamespace().getNamespaceURI(), "status");
	       qDataType    = new QName(publishedDataStructs.getNamespace().getNamespaceURI(), "type");
	       qDescription   = new QName(publishedDataStructs.getNamespace().getNamespaceURI(), "desc");
	       qParentDataStructure  = new QName(publishedDataStructs.getNamespace().getNamespaceURI(), "parent");
	       qVersion    = new QName(publishedDataStructs.getNamespace().getNamespaceURI(), "version");
            
           init();
            
            
    	}
    	

        /**
         * init
         */
        private void init() {
            setTitle("Choose Data Structure(s) for " + file.getName());
            int numColumns = 5;
            String[] columnNames = {" ", "Short Name" , "Description", "Version", "Status"};
            structsModel = new ViewTableModel() {
                public boolean isCellEditable(final int row, final int column) {
                    if (column == 0) {
                        return true;
                    }
                    return false;
                }
            };
            structsTable = new JTable(structsModel);
            
            for (int i = 0; i < columnNames.length; i++) {
            	structsModel.addColumn(columnNames[i]);
            }
            
            
            
            structsTable.getColumn(" ").setMinWidth(50);
            structsTable.getColumn(" ").setMaxWidth(50);
            structsTable.getColumn("Short Name").setMinWidth(150);
            structsTable.getColumn("Description").setMinWidth(300);
            
            structsTable.getColumn("Version").setMinWidth(50);
            structsTable.getColumn("Version").setMaxWidth(50);
            structsTable.getColumn(" ").setCellRenderer(new CheckBoxRenderer());
            structsTable.getColumn(" ").setCellEditor(new CheckBoxEditor());

            
            
            
            
            Iterator<OMElement> iter2 = publishedDataStructs.getChildElements();
            while(iter2.hasNext()) {
            	OMElement e = iter2.next();
            	
            	if (e.getLocalName().equals("data_structure"))
            	 {
            	  String shortname = e.getAttributeValue(qName);
            	  String version = e.getAttributeValue(qVersion); // This can also be obtained using the last two characters of short name
            	  String status = e.getAttributeValue(qStatus);
            	  String dataType = e.getAttributeValue(qDataType);
            	  String desc = e.getAttributeValue(qDescription);
            	  String parent = e.getAttributeValue(qParentDataStructure);
            	if(dataType.equals("Imaging")) {
            		if(parent.trim().equals("")) {
                	  descAL.add(desc);
                	  shortNameAL.add(shortname);
                	  versionAL.add(version);
                	  statusAL.add(status);
            		}
                	  
            	 }
            	  
            	 
            	  // TODO: Store or throw away the information about this data structure
            	 }

            	
            }
            
            //numColumns = 5
            Object[] rowData = new Object[numColumns];
            for(int i=0;i<descAL.size();i++) {
            	rowData[0] = false;
            	
            	rowData[1] = shortNameAL.get(i);
            	rowData[2] = descAL.get(i);
            	rowData[3] = versionAL.get(i);
            	rowData[4] = statusAL.get(i);
            	structsModel.addRow(rowData);
            }
            
            
            
            
            structsScrollPane = new JScrollPane(structsTable, ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED,
                    ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
            
            
            structsScrollPane.setPreferredSize(new Dimension(600, 300));
            
            
            
            
            
            
            
            
            
            
            final JPanel OKPanel = new JPanel();
            buildOKButton();
            buildCancelButton();
            OKButton.setText("OK");
            OKButton.setActionCommand("ok4");
            OKButton.addActionListener(this);
            OKPanel.add(OKButton);

            
            
            getContentPane().add(structsScrollPane,BorderLayout.CENTER);
            
            getContentPane().add(OKPanel,BorderLayout.SOUTH);
            
            pack();
            
            MipavUtil.centerInWindow(owner, this);
            this.setMinimumSize(this.getSize());
            
            setVisible(true);
            
            
        }
    	
        
        
        

        /**
         * action performed
         */
        public void actionPerformed(final ActionEvent e) {
            final String command = e.getActionCommand();
            if(command.equals("ok4")) {
            	int numRows = structsModel.getRowCount();
            	boolean hasSelected = false;
            	for(int i=0;i<numRows;i++) {
            		
            		if((Boolean)structsModel.getValueAt(i, 0)) {
            			hasSelected = true;
            		}
            	}
            	if(!hasSelected) {
            		MipavUtil.displayError("Please select at least 1 Data Structure for this image");
            		return;
            	}
            	
            	Vector rowData = new Vector();
                rowData.add(file);
                rowData.add("No");
                sourceTableModel.addRow(rowData);
                sourceTable.setRowSelectionInterval(sourceTableModel.getRowCount() - 1, sourceTableModel
                        .getRowCount() - 1);
                multiFileTable.put(file, new Boolean(isMultiFile));

                //FileIO fileIO = new FileIO();
                //fileIO.setQuiet(true);
                final File imageFile = file;
                //ModelImage srcImage = fileIO.readImage(imageFile.getName(), imageFile.getParent()
                        //+ File.separator, multiFileTable.get(imageFile), null);

                final int[] extents = new int[] {srcImage.getExtents()[0], srcImage.getExtents()[1]};

                previewImg = new ViewJComponentPreviewImage(srcImage, extents, owner);
                int slice = 0;
                if ( !srcImage.is2DImage()) {
                    slice = (srcImage.getExtents()[2] / 2);
                }
                previewImg.createImg(slice);

                previewImages.add(previewImg);

                previewPanel.removeAll();
                previewPanel.repaint();

                previewPanel.add(previewImg);

                previewImages.get(sourceTable.getSelectedRow()).setSliceBrightness(brightness, contrast);

                previewPanel.validate();
                previewPanel.repaint();

                srcImage.disposeLocal();
                srcImage = null;
                
                ArrayList<String> selectedDataStructsAL = new ArrayList<String>();
                for(int i=0;i<numRows;i++) {
            		if((Boolean)structsModel.getValueAt(i, 0)) {
            			//get all selected shortnames
            			String shortName = (String)structsModel.getValueAt(i, 1);
            			selectedDataStructsAL.add(shortName);
            		}
            	}
                
                selectedFileDataStructureNames.put(file, selectedDataStructsAL);
                
                this.dispose();
                
                
                
                
                new InfoDialog(owner, file, false);
            	
            	
            }
            
        }
    	
    	
        
        private class CheckBoxRenderer extends DefaultTableCellRenderer {
            JCheckBox checkBox = new JCheckBox();

            public Component getTableCellRendererComponent(final JTable table, final Object value,
                    final boolean isSelected, final boolean hasFocus, final int row, final int column) {
                if (value instanceof Boolean) { // Boolean
                    checkBox.setSelected( ((Boolean) value).booleanValue());
                    checkBox.setHorizontalAlignment(SwingConstants.CENTER);
                    return checkBox;
                }

                return null;
            }
        }
        
        
        private class CheckBoxEditor implements TableCellEditor {
            private final static int BOOLEAN = 1;

            DefaultCellEditor cellEditor;

            int flg;

            public CheckBoxEditor() {
                final JCheckBox checkBox = new JCheckBox();
                cellEditor = new DefaultCellEditor(checkBox);
                checkBox.setHorizontalAlignment(SwingConstants.CENTER);
            }

            public Component getTableCellEditorComponent(final JTable table, final Object value, final boolean isSelected,
                    final int row, final int column) {
                if (value instanceof Boolean) { // Boolean
                    flg = CheckBoxEditor.BOOLEAN;
                    return cellEditor.getTableCellEditorComponent(table, value, isSelected, row, column);
                }
                return null;
            }

            public Object getCellEditorValue() {
                switch (flg) {
                    case BOOLEAN:
                        return cellEditor.getCellEditorValue();
                    default:
                        return null;
                }
            }

            public Component getComponent() {
                return cellEditor.getComponent();
            }

            public boolean stopCellEditing() {
                return cellEditor.stopCellEditing();
            }

            public void cancelCellEditing() {
                cellEditor.cancelCellEditing();
            }

            public boolean isCellEditable(final EventObject anEvent) {
                return cellEditor.isCellEditable(anEvent);
            }

            public boolean shouldSelectCell(final EventObject anEvent) {
                return cellEditor.shouldSelectCell(anEvent);
            }

            public void addCellEditorListener(final CellEditorListener l) {
                cellEditor.addCellEditorListener(l);
            }

            public void removeCellEditorListener(final CellEditorListener l) {
                cellEditor.removeCellEditorListener(l);
            }

            public void setClickCountToStart(final int n) {
                cellEditor.setClickCountToStart(n);
            }

            public int getClickCountToStart() {
                return cellEditor.getClickCountToStart();
            }
        }
        
        
    }

    /**
     * launches the dialog to add info
     * 
     * @author pandyan
     * 
     */
    private class InfoDialog extends JDialog implements ActionListener, WindowListener {
        private final Dialog owner;

        private final File file;

        private Hashtable<String, TreeMap> labelsAndComponentsTable = new Hashtable<String,TreeMap>();

        private final JTabbedPane tabbedPane = new JTabbedPane();

        private JPanel mainPanel;

        private GridBagConstraints gbc;

        private JScrollPane tabScrollPane;

        private String guid = "";

        private ModelImage origImage;

        private final FileIO fileIO;

        private int gridYCounter = 0;

        private boolean launchedFromCompletedState = false;

        private JLabel requiredLabel;
        
        private JMenu addMenu;
        
        private JMenu removeMenu;

        /**
         * constructor
         * 
         * @param owner
         * @param file
         * @param launchedFromCompletedState
         */
        public InfoDialog(final Dialog owner, final File file, final boolean launchedFromCompletedState) {

            super(owner, true);

            this.owner = owner;
            this.file = file;
            this.launchedFromCompletedState = launchedFromCompletedState;

            fileIO = new FileIO();
            fileIO.setQuiet(true);
            origImage = fileIO.readImage(file.getName(), file.getParent() + File.separator, multiFileTable.get(file),
                    null);
            
            //create tabs for all the data structures
            Set keySet = selectedFileDataStructureNames.keySet();
            Iterator iter = keySet.iterator();
            while (iter.hasNext()) {
                File f2 = (File) iter.next();
                if (file.getName().equals(f2.getName())) {
                    ArrayList<String> dataStructsAL = selectedFileDataStructureNames.get(f2);
                	for(int i=0;i<dataStructsAL.size();i++) {
                		String dsName = dataStructsAL.get(i);
                		JPanel panel = new JPanel(new GridBagLayout());
                        tabScrollPane = new JScrollPane(panel, ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED,
                                ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
                        tabScrollPane.setPreferredSize(new Dimension(600, 200));
                        tabbedPane.addTab(dsName, tabScrollPane);	
                	}
                }
            }
            
            init();
            if (origImage != null) {
                origImage.disposeLocal();
                origImage = null;
            }

        }

        /**
         * init
         */
        private void init() {
            setTitle("Edit Data Elements for " + file.getName());
            addWindowListener(this);
            mainPanel = new JPanel(new GridBagLayout());

            gbc = new GridBagConstraints();

            try {
                setIconImage(MipavUtil.getIconImage(Preferences.getIconName()));
            } catch (final Exception e) {
                // setIconImage() is not part of the Java 1.5 API - catch any runtime error on those systems
            }

            ArrayList<String> selectedDataStuctsAl = null;
            Set keySet = selectedFileDataStructureNames.keySet();
            Iterator iter = keySet.iterator();

            while (iter.hasNext()) {
                File f = (File) iter.next();
                if(f.getName().equals(file.getName())) {
                	selectedDataStuctsAl = selectedFileDataStructureNames.get(f);
                	break;
                }    
            }
            
            
            
            for(int i=0;i<selectedDataStuctsAl.size();i++) {
            	//get the OMElement for each selected Data Structure
            	
            	
            	
            	 keySet = documentElements.keySet();
                 iter = keySet.iterator();

                 while (iter.hasNext()) {
                     String shortname  = (String) iter.next();
                     if(shortname.equals(selectedDataStuctsAl.get(i))) {
                    	 documentElement = documentElements.get(shortname);
                    	 Iterator<OMElement> iter2 = documentElement.getChildElements();
                         //should only be 1 top level Data_Structure tag
                         dataStructElement = iter2.next();
                         namespace = dataStructElement.getNamespace().getNamespaceURI();
                         
                         
                         
                         try {
                             OMAttribute attr;
                             QName qname;

                             qname = new QName(namespace, "name");
                             attr = dataStructElement.getAttribute(qname);
                             final String n = attr.getAttributeValue();

                             qname = new QName(namespace, "short_name");
                             attr = dataStructElement.getAttribute(qname);
                             final String s = attr.getAttributeValue();

                             qname = new QName(namespace, "desc");
                             attr = dataStructElement.getAttribute(qname);
                             final String d = attr.getAttributeValue();

                             qname = new QName(namespace, "version");
                             attr = dataStructElement.getAttribute(qname);
                             final String v = attr.getAttributeValue();

                             qname = new QName(namespace, "type");
                             attr = dataStructElement.getAttribute(qname);
                             final String t = attr.getAttributeValue();

                             DataStruct dataStruct = new DataStruct(n, s, d, v, t);
                             selectedFileDataStructures.add(dataStruct);
                             
                             TreeMap<JLabel, JComponent> labelsAndComps = new TreeMap<JLabel, JComponent>(new JLabelComparator());
                             
                             parse(dataStructElement, dataStruct, shortname, labelsAndComps);
                             
                             //parseForInitLabelsAndComponents(selectedFileDataStructures.get(i), labelsAndComps);
                             parseForInitLabelsAndComponents(dataStruct, labelsAndComps);
                             
                             if (launchedFromCompletedState) {
                            	 //LinkedHashMap<String, String> infoMap2 = infoTable.get(file);
                            	 Hashtable<String, LinkedHashMap<String, String>> hTable = infoTable.get(file);
                            	 
                            	 keySet = hTable.keySet();
                                 iter = keySet.iterator();
                                 while (iter.hasNext()) {

                                     String sname = (String) iter.next();
                                     LinkedHashMap<String, String> infoMap = hTable.get(sname);

                                     populateFieldsFromCompletedState(labelsAndComps, infoMap);

                                     
                                 }
                            	 
                                 
                             } else {
                                 populateFields(labelsAndComps);
                             }
                             labelsAndComponentsTable.put(shortname, labelsAndComps);
                             
                             

                         } catch (final Exception e) {
                             e.printStackTrace();
                         }
                         
                         
                         
                         
                    	 
                    	 
                     	break;
                     }    
                 }
            	
            	
            	
            	
            }
            
            
            
            
            

            gbc.gridx = 0;
            gbc.gridy = 0;
            gbc.insets = new Insets(10, 5, 10, 25);
            gbc.gridwidth = 1;
            
            /*for(int i=0;i<selectedFileDataStructures.size();i++) {
            	parseForInitLabelsAndComponents(selectedFileDataStructures.get(i));
            }*/



            final JPanel OKPanel = new JPanel();
            buildOKButton();
            buildCancelButton();
            OKButton.setText("Save");
            OKButton.setActionCommand("ok3");
            OKButton.addActionListener(this);
            cancelButton.setActionCommand("cancel3");
            cancelButton.addActionListener(this);
            OKPanel.add(OKButton);
            OKPanel.add(cancelButton);

            
            requiredLabel = new JLabel("<html>* Required data elements are in <font color=\"red\">red</font></html>");

            gbc.fill = GridBagConstraints.BOTH;
            gbc.anchor = GridBagConstraints.EAST;
            gbc.weightx = 0;
            gbc.gridx = 0;
            gbc.gridy = 0;
            mainPanel.add(requiredLabel, gbc);
            gbc.gridy = 1;
            gbc.weightx = 1;
            gbc.weighty = 1;
            mainPanel.add(tabbedPane, gbc);
            gbc.weightx = 0;
            gbc.weighty = 0;
            gbc.gridy = 2;
            mainPanel.add(OKPanel, gbc);


            getContentPane().add(mainPanel);
            
            addMenu = new JMenu("Add");
            
            keySet = documentElements.keySet();
            iter = keySet.iterator();
            while (iter.hasNext()) {
                String shortname  = (String) iter.next();
                boolean found = false;
                for(int i=0;i<selectedDataStuctsAl.size();i++) {
                	String selectedDS = selectedDataStuctsAl.get(i);
                	if(shortname.equals(selectedDS)) {
                		found = true;
                		break;
                	}
                }
                if(!found) {
                	JMenuItem itemOpen = new JMenuItem(shortname);
                    itemOpen.addActionListener(this);
                    itemOpen.setActionCommand("add");
                    itemOpen.setFont(MipavUtil.font12B);
                    addMenu.add(itemOpen);
                }
                
            }
            
            
            
            
            
            
            
            
            removeMenu = new JMenu("Remove");
            for(int i=0;i<selectedDataStuctsAl.size();i++) {
            	JMenuItem itemOpen = new JMenuItem(selectedDataStuctsAl.get(i));
                itemOpen.addActionListener(this);
                itemOpen.setActionCommand("remove");
                itemOpen.setFont(MipavUtil.font12B);
                removeMenu.add(itemOpen);
            }
            JMenuBar menuBar = new JMenuBar();
            menuBar.add(addMenu);
            menuBar.add(removeMenu);
            setJMenuBar(menuBar);

            pack();
            MipavUtil.centerInWindow(owner, this);
            this.setMinimumSize(this.getSize());
            setVisible(true);
        }



        /**
         * displays the labels and components
         * 
         * @param ds2
         */
        private void parseForInitLabelsAndComponents(DataStruct ds2,TreeMap<JLabel, JComponent> labelsAndComps) {
            JPanel panel;
            JScrollPane sp;
            final Set keySet = labelsAndComps.keySet();
            final Iterator iter = keySet.iterator();
            while (iter.hasNext()) {
                final JLabel l = (JLabel) iter.next();
                final JComponent t = labelsAndComps.get(l);
                for (int k = 0; k < ds2.size(); k++) {
                    final Object o1 = ds2.get(k);
                    if (o1 instanceof DataElement) {
                        final String parentDataStructShortname = ((DataElement) o1).getParentDataStructShortname();

                            for (int i = 0; i < tabbedPane.getTabCount(); i++) {
                                final String title = tabbedPane.getTitleAt(i);
                                if (title.toLowerCase().startsWith(parentDataStructShortname.toLowerCase())) {
                                    sp = (JScrollPane) (tabbedPane.getComponentAt(i));
                                    panel = (JPanel) (sp.getViewport().getComponent(0));
                                    if (l.getName().equals( ((DataElement) o1).getName())) {
                                        gbc.fill = GridBagConstraints.HORIZONTAL;
                                        gbc.anchor = GridBagConstraints.EAST;
                                        gbc.weightx = 0;
                                        panel.add(l, gbc);
                                        gbc.weightx = 1;
                                        gbc.gridx = 1;
                                        gbc.anchor = GridBagConstraints.WEST;
                                        panel.add(t, gbc);
                                        gridYCounter = gridYCounter + 1;
                                        gbc.gridy = gridYCounter;
                                        gbc.gridx = 0;
                                        break;
                                    }
                                }
                            }
                    }
                }
            }
        }

        /**
         * parses the OMElement
         * 
         * @param ds
         */
        private void parse(OMElement omElement, DataStruct ds2, String shortname,TreeMap<JLabel, JComponent> labelsAndComps) {
        	Iterator iter = omElement.getChildElements();
            OMElement childElement;
            OMAttribute attr;
            QName qname;
            String childElementName;
            while (iter.hasNext()) {
                childElement = (OMElement) iter.next();
                childElementName = childElement.getLocalName();
                if (childElementName.equals("data_element")) {
                    qname = new QName(namespace, "name");
                    attr = childElement.getAttribute(qname);
                    String n = attr.getAttributeValue();

                    qname = new QName(namespace, "desc");
                    attr = childElement.getAttribute(qname);
                    String d = attr.getAttributeValue();

                    qname = new QName(namespace, "short_desc");
                    attr = childElement.getAttribute(qname);
                    String sh = attr.getAttributeValue();

                    qname = new QName(namespace, "type");
                    attr = childElement.getAttribute(qname);
                    String t = attr.getAttributeValue();

                    qname = new QName(namespace, "size");
                    attr = childElement.getAttribute(qname);
                    String s = attr.getAttributeValue();

                    qname = new QName(namespace, "required");
                    attr = childElement.getAttribute(qname);
                    String r = attr.getAttributeValue();

                    qname = new QName(namespace, "value_range");
                    attr = childElement.getAttribute(qname);
                    String v = attr.getAttributeValue();

                    String parentDataStruct = ds2.getName();
                    String parentDataStructShortName = ds2.getShortname();
                    DataElement de = new DataElement(n, d, sh, t, s, r, v, parentDataStruct,parentDataStructShortName);
                    ds2.add(de);

                    if ( ! (n.equals("image_file")
                            || n.equals("image_thumbnail_file")
                            || (origImage.is2DImage() && n.equals("image_extent3"))
                            || ( (origImage.is2DImage() || origImage.is3DImage()) && n.equals("image_extent4"))
                            || ( (origImage.is2DImage() || origImage.is3DImage() || origImage.is4DImage()) && n
                                    .equals("image_extent5"))
                            || (origImage.is2DImage() && n.equals("image_resolution3"))
                            || ( (origImage.is2DImage() || origImage.is3DImage()) && n.equals("image_resolution4"))
                            || ( (origImage.is2DImage() || origImage.is3DImage() || origImage.is4DImage()) && n
                                    .equals("image_resolution5")) || (origImage.is2DImage() && n.equals("image_unit3"))
                            || ( (origImage.is2DImage() || origImage.is3DImage()) && n.equals("image_unit4")) || ( (origImage
                            .is2DImage()
                            || origImage.is3DImage() || origImage.is4DImage()) && n.equals("image_unit5")))) {

                        JLabel l;
                        if (sh == null || sh.equals("")) {
                            l = new JLabel(n);
                        } else {
                            l = new JLabel(sh);
                        }

                        l.setName(n);
                        // if valuerange is enumeration, create a combo box...otherwise create a textfield
                        if (v.contains(";") && !t.equalsIgnoreCase("DATE")) {
                            final JComboBox cb = new JComboBox();
                            cb.setName(n);
                            final String[] items = v.split(";");
                            for (final String element : items) {
                                final String item = element.trim();
                                cb.addItem(item);
                            }
                            if (r.equals("Required")) {
                                l.setForeground(Color.red);
                            }
                            labelsAndComps.put(l, cb);
                        } else {
                            final JTextField tf = new JTextField(30);
                            tf.setName(n);

                            String tooltip = "Type: " + t;
                            if (t.equalsIgnoreCase("String")) {
                                tooltip += " (" + s + ")";
                            }
                            if ( !v.trim().equals("")) {
                                tooltip += ".  Value range: " + v;
                            }
                            tf.setToolTipText(tooltip);

                            if (n.equals("image_num_dimensions")) {
                                tf.setEnabled(false);
                            } else if (n.equals("image_extent1")) {
                                tf.setEnabled(false);
                            } else if (n.equals("image_extent2")) {
                                tf.setEnabled(false);
                            } else if (n.equals("image_extent3")) {
                                tf.setEnabled(false);
                            } else if (n.equals("image_extent4")) {
                                tf.setEnabled(false);
                            } else if (n.equals("image_extent5")) {
                                tf.setEnabled(false);
                            }
                            if (r.equals("Required")) {
                                l.setForeground(Color.red);
                            }
                            labelsAndComps.put(l, tf);
                        }
                    }
                }
            }
        }

        /**
         * populates dialog from completed state
         */
        public void populateFieldsFromCompletedState(TreeMap<JLabel, JComponent> labelsAndComps,LinkedHashMap<String, String> infoMap2) {
            
            final Set keySet = infoMap2.keySet();
            final Iterator iter = keySet.iterator();
            String key;
            String value;
            while (iter.hasNext()) {
                key = (String) iter.next();
                value = infoMap2.get(key);

                final Set keySet2 = labelsAndComps.keySet();
                final Iterator iter2 = keySet2.iterator();
                while (iter2.hasNext()) {
                    final JLabel l = (JLabel) iter2.next();
                    final Component comp = labelsAndComps.get(l);
                    final String name = comp.getName();
                    if (name.equals(key)) {
                        if (comp instanceof JTextField) {
                            final JTextField t = (JTextField) comp;
                            t.setText(value);

                        } else if (comp instanceof JComboBox) {
                            final JComboBox c = (JComboBox) comp;

                            for (int k = 0; k < c.getItemCount(); k++) {
                                final String item = (String) c.getItemAt(k);
                                if (value.equals(item)) {
                                    c.setSelectedIndex(k);
                                }
                            }
                        }
                        break;
                    }
                }
            }
        }

        /**
         * prepopulates some of the fields with info from image header
         */
        public void populateFields(TreeMap<JLabel, JComponent> labelsAndComps) {
            final float[] res = origImage.getResolutions(0);
            final int[] units = origImage.getUnitsOfMeasure();
            final int exts[] = origImage.getExtents();
            final int nDims = origImage.getNDims();
            final int modality = origImage.getFileInfo(0).getModality();
            final String modalityString = FileInfoBase.getModalityStr(modality);
            final float sliceThickness = origImage.getFileInfo(0).getSliceThickness();
            final int orient = origImage.getFileInfo(0).getImageOrientation();
            final String orientation = FileInfoBase.getImageOrientationStr(orient);
            // get index for extents
            final Set keySet = labelsAndComps.keySet();
            final Iterator iter = keySet.iterator();
            while (iter.hasNext()) {
                final JLabel label = (JLabel) iter.next();
                final String l = label.getName();
                final JComponent comp = labelsAndComps.get(label);
                if (l.equals("image_num_dimensions")) {
                    ((JTextField) comp).setText(String.valueOf(nDims));
                } else if (l.equals("image_extent1")) {
                    ((JTextField) comp).setText(String.valueOf(exts[0]));
                } else if (l.equals("image_extent2")) {
                    ((JTextField) comp).setText(String.valueOf(exts[1]));
                } else if (l.equals("image_extent3")) {
                    ((JTextField) comp).setText(String.valueOf(exts[2]));
                } else if (l.equals("image_extent4")) {
                    ((JTextField) comp).setText(String.valueOf(exts[3]));
                } else if (l.equals("image_extent5")) {
                    // for now...nothing
                } else if (l.equals("image_unit1")) {
                    final JComboBox jc = (JComboBox) comp;
                    for (int k = 0; k < jc.getItemCount(); k++) {
                        final String item = (String) jc.getItemAt(k);
                        if (FileInfoBase.getUnitsOfMeasureStr(units[0]).equals(item)) {
                            jc.setSelectedIndex(k);
                        }
                    }
                } else if (l.equals("image_unit2")) {
                    final JComboBox jc = (JComboBox) comp;
                    for (int k = 0; k < jc.getItemCount(); k++) {
                        final String item = (String) jc.getItemAt(k);
                        if (FileInfoBase.getUnitsOfMeasureStr(units[1]).equals(item)) {
                            jc.setSelectedIndex(k);
                        }
                    }
                } else if (l.equals("image_unit3")) {
                    final JComboBox jc = (JComboBox) comp;
                    for (int k = 0; k < jc.getItemCount(); k++) {
                        final String item = (String) jc.getItemAt(k);
                        if (FileInfoBase.getUnitsOfMeasureStr(units[2]).equals(item)) {
                            jc.setSelectedIndex(k);
                        }
                    }
                } else if (l.equals("image_unit4")) {
                    final JComboBox jc = (JComboBox) comp;
                    for (int k = 0; k < jc.getItemCount(); k++) {
                        final String item = (String) jc.getItemAt(k);
                        if (FileInfoBase.getUnitsOfMeasureStr(units[3]).equals(item)) {
                            jc.setSelectedIndex(k);
                        }
                    }
                } else if (l.equals("image_unit5")) {
                    // for now...nothing
                } else if (l.equals("image_resolution1")) {
                    ((JTextField) comp).setText(String.valueOf(res[0]));
                } else if (l.equals("image_resolution2")) {
                    ((JTextField) comp).setText(String.valueOf(res[1]));
                } else if (l.equals("image_resolution3")) {
                    ((JTextField) comp).setText(String.valueOf(res[2]));
                } else if (l.equals("image_resolution4")) {
                    ((JTextField) comp).setText(String.valueOf(res[3]));
                } else if (l.equals("image_resolution5")) {
                    // for now...nothing
                } else if (l.equals("image_modality")) {
                    final JComboBox jc = (JComboBox) comp;
                    for (int k = 0; k < jc.getItemCount(); k++) {
                        final String item = (String) jc.getItemAt(k);
                        if (modalityString.equals(item)) {
                            jc.setSelectedIndex(k);
                        }
                    }
                } else if (l.equals("image_slice_thickness")) {
                    if (sliceThickness == 0) {
                        ((JTextField) comp).setText("");
                    } else {
                        ((JTextField) comp).setText(String.valueOf(sliceThickness));
                    }
                } else if (l.equals("image_orientation")) {
                    final JComboBox jc = (JComboBox) comp;
                    for (int k = 0; k < jc.getItemCount(); k++) {
                        final String item = (String) jc.getItemAt(k);
                        if (orientation.equals(item)) {
                            jc.setSelectedIndex(k);
                        }
                    }
                }
            }
        }

        /**
         * action performed
         */
        public void actionPerformed(final ActionEvent e) {
            final String command = e.getActionCommand();
            ArrayList<String> errs;
            final StringBuffer errors = new StringBuffer();
            ;
            if (command.equals("ok3")) {
                errs = validateFields();
                if (errs.size() == 0) {
                	
                	
                	
                	
                	Set keySet = selectedFileDataStructureNames.keySet();
                    Iterator iter = keySet.iterator();
                    while (iter.hasNext()) {
                        File f2 = (File) iter.next();
                        if (file.getName().equals(f2.getName())) {
                            ArrayList<String> dataStructsAL = selectedFileDataStructureNames.get(f2);
                        	for(int i=0;i<dataStructsAL.size();i++) {
                        		String dsName = dataStructsAL.get(i);
                        		
                        		
                        		for(int k=0;k<selectedFileDataStructures.size();k++) {
                                	String sName = selectedFileDataStructures.get(k).getShortname();
                                	
                                	if(dsName.equals(sName)) {
                                	
	                                	TreeMap<JLabel, JComponent> labelsAndComps = labelsAndComponentsTable.get(sName);
	                                	
	                                	
	                                    complete(labelsAndComps, sName);	
                                	}
                                	
                                	
                                	
                                }
                        		
                        		
                        	}
                        }
                    }

                    enableDisableFinishButton();
                    dispose();
                } else {
                    for (int i = 0; i < errs.size(); i++) {
                        errors.append(" - " + errs.get(i) + "\n");
                    }
                    MipavUtil.displayError("Please correct the following errors: \n" + errors.toString());
                }
            } else if (command.equals("cancel3")) {
                // enableDisableCompleteDataElementsButton();
                enableDisableFinishButton();
                dispose();
            }else if(command.equals("add")) {
            	String shortname = ((JMenuItem)e.getSource()).getText();
            	
            	ArrayList<String> selectedDataStructsAL = new ArrayList<String>();
            	selectedDataStructsAL = selectedFileDataStructureNames.get(file);
            	selectedDataStructsAL.add(shortname);
            	selectedFileDataStructureNames.put(file, selectedDataStructsAL);
            	
            	
            	//create tab
            	JPanel panel = new JPanel(new GridBagLayout());
                tabScrollPane = new JScrollPane(panel, ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED,
                        ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
                tabScrollPane.setPreferredSize(new Dimension(600, 200));
                tabbedPane.addTab(shortname, tabScrollPane);

            	documentElement = documentElements.get(shortname);
           	    Iterator<OMElement> iter2 = documentElement.getChildElements();
                //should only be 1 top level Data_Structure tag
                dataStructElement = iter2.next();
                namespace = dataStructElement.getNamespace().getNamespaceURI();
                try {
                    OMAttribute attr;
                    QName qname;

                    qname = new QName(namespace, "name");
                    attr = dataStructElement.getAttribute(qname);
                    final String n = attr.getAttributeValue();

                    qname = new QName(namespace, "short_name");
                    attr = dataStructElement.getAttribute(qname);
                    final String s = attr.getAttributeValue();

                    qname = new QName(namespace, "desc");
                    attr = dataStructElement.getAttribute(qname);
                    final String d = attr.getAttributeValue();

                    qname = new QName(namespace, "version");
                    attr = dataStructElement.getAttribute(qname);
                    final String v = attr.getAttributeValue();

                    qname = new QName(namespace, "type");
                    attr = dataStructElement.getAttribute(qname);
                    final String t = attr.getAttributeValue();

                    DataStruct dataStruct = new DataStruct(n, s, d, v, t);
                    selectedFileDataStructures.add(dataStruct);

                    TreeMap<JLabel, JComponent> labelsAndComps = new TreeMap<JLabel, JComponent>(new JLabelComparator());
                    
                    parse(dataStructElement, dataStruct, shortname, labelsAndComps);

                    parseForInitLabelsAndComponents(dataStruct, labelsAndComps);
                    
                    populateFields(labelsAndComps);

                    labelsAndComponentsTable.put(shortname, labelsAndComps);
                }catch(Exception ex) {
                	ex.printStackTrace();
                }

                JMenuItem itemOpen = new JMenuItem(shortname);
                itemOpen.addActionListener(this);
                itemOpen.setActionCommand("remove");
                itemOpen.setFont(MipavUtil.font12B);
                removeMenu.add(itemOpen);
                
                int numItems = addMenu.getItemCount();
                for(int i=0;i<numItems;i++) {
                	JMenuItem item = addMenu.getItem(i);
                	String itemText = item.getText();
                	if(shortname.equals(itemText)) {
                		addMenu.remove(i);
                		break;
                	}
                	
                }

            	this.repaint();
            	sourceTableModel.setValueAt("No", sourceTable.getSelectedRow(), 1);

            }else if(command.equals("remove")) {
            	
            	int numTabs = tabbedPane.getTabCount();
            	
            	if(numTabs < 2) {
            		MipavUtil.displayError("There must be at least 1 Data Structure associated with each file");
            		return;
            	}

            	String shortname = ((JMenuItem)e.getSource()).getText();

            	ArrayList<String> selectedDataStructsAL = new ArrayList<String>();
            	selectedDataStructsAL = selectedFileDataStructureNames.get(file);
            	selectedDataStructsAL.remove(shortname);
            	selectedFileDataStructureNames.put(file, selectedDataStructsAL);

            for(int i=0;i<numTabs;i++) {
            	String tabName = tabbedPane.getTitleAt(i);
            	if(tabName.equals(shortname)) {
            		tabbedPane.removeTabAt(i);
            		break;
            	}
            }
            
            JMenuItem itemOpen = new JMenuItem(shortname);
            itemOpen.addActionListener(this);
            itemOpen.setActionCommand("add");
            itemOpen.setFont(MipavUtil.font12B);
            addMenu.add(itemOpen);
            
            int numItems = removeMenu.getItemCount();
            for(int i=0;i<numItems;i++) {
            	JMenuItem item = removeMenu.getItem(i);
            	String itemText = item.getText();
            	if(shortname.equals(itemText)) {
            		removeMenu.remove(i);
            		break;
            	}
            	
            }

            this.repaint();

            Hashtable<String, LinkedHashMap<String, String>> hTable;
            if(infoTable.get(file) != null) {

            	hTable = infoTable.get(file);
            	
            	hTable.remove(shortname);
            	
            	infoTable.put(file, hTable);
            	
            }

            
            
            
            
            
            
            	
            	
            }

        }

        /**
         * validates fields
         * 
         * @return
         */
        public ArrayList<String> validateFields() {
            ArrayList<String> errs = new ArrayList<String>();
            
            
            
            
            Set keySet = selectedFileDataStructureNames.keySet();
            Iterator iter = keySet.iterator();
            while (iter.hasNext()) {
                File f2 = (File) iter.next();
                if (file.getName().equals(f2.getName())) {
                    ArrayList<String> dataStructsAL = selectedFileDataStructureNames.get(f2);
                	for(int i=0;i<dataStructsAL.size();i++) {
                		String dsName = dataStructsAL.get(i);
                		
                		for(int k=0;k<selectedFileDataStructures.size();k++) {
                        	String sName = selectedFileDataStructures.get(k).getShortname();
                        	
                        	if(dsName.equals(sName)) {
	                        	TreeMap<JLabel, JComponent> labelsAndComps = labelsAndComponentsTable.get(sName);
	                        	parseDataStructForValidation(selectedFileDataStructures.get(k), file, errs, labelsAndComps);
                        	}
                        }
                	}
                }
            }

            return errs;

        }

        /**
         * validates fields
         * 
         * @param ds2
         * @param imageFile
         * @param errs
         */
        public void parseDataStructForValidation(final DataStruct ds2, final File imageFile,
                final ArrayList<String> errs,TreeMap<JLabel, JComponent> labelsAndComps) {
            String value = "";
            String key = "";
            String labelText = "";
            String required = "";
            String valuerange = "";
            String type = "";
            String size = "";
            boolean found = false;
            for (int k = 0; k < ds2.size(); k++) {
                final Object o1 = ds2.get(k);
                if (o1 instanceof DataElement) {
                    // data element
                    final DataElement de = (DataElement) o1;
                    final String name = de.getName();

                    // need to get appropriat value
                    final Set keySet = labelsAndComps.keySet();
                    final Iterator iter = keySet.iterator();
                    while (iter.hasNext()) {
                        final JLabel label = (JLabel) iter.next();
                        final JComponent comp = labelsAndComps.get(label);
                        key = label.getName();
                        labelText = label.getText();
                        if (comp instanceof JTextField) {
                            value = ((JTextField) comp).getText().trim();
                        } else if (comp instanceof JComboBox) {
                            value = (String) ( ((JComboBox) comp).getSelectedItem());
                        }
                        if (key.equals(name)) {
                            found = true;
                            break;
                        }
                    }

                    if (found) {
                        // now we need to validate
                        required = de.getRequired();
                        type = de.getType();
                        size = de.getSize();
                        valuerange = de.getValuerange();
                        if (required.equals("Required")) {
                            if (value.trim().equals("")) {
                                errs.add(labelText + " is a required field");
                            } else {
                                if (key.equals("subjectkey")) {
                                    if ( !value.trim().startsWith("NDAR")) {
                                        errs.add(labelText + " must begin with NDAR");
                                    }
                                }
                            }
                        }
                        if (type.equals("Integer")) {
                            if ( !value.trim().equals("")) {
                                try {
                                    final int intValue = Integer.valueOf(value.trim()).intValue();
                                    if (valuerange.contains("+")) {
                                        // test int if its in valuerange
                                        final int min = Integer.valueOf(
                                                valuerange.substring(0, valuerange.indexOf("+")).trim()).intValue();
                                        if (min == 0) {
                                            if (intValue <= min) {
                                                errs.add(labelText + " must be greater than 0");
                                            }
                                        } else {
                                            if (intValue < min) {
                                                errs.add(labelText + " must be greater than " + min);
                                            }
                                        }

                                    } else if (valuerange.contains(" to ")) {
                                        final int min = Integer.valueOf(
                                                valuerange.substring(0, valuerange.indexOf(" to ")).trim()).intValue();
                                        final int max = Integer.valueOf(
                                                valuerange.substring(valuerange.indexOf(" to ") + 4,
                                                        valuerange.length()).trim()).intValue();
                                        if (intValue < min || intValue > max) {
                                            errs.add(labelText + " must be in the range of " + min + " to " + max);
                                        }
                                    }
                                } catch (final NumberFormatException e) {
                                    errs.add(labelText + " must be an Integer");
                                }
                            }
                        }
                        if (type.equals("Float")) {
                            if ( !value.trim().equals("")) {
                                try {
                                    final float floatValue = Float.valueOf(value.trim()).floatValue();
                                    if (valuerange.contains("+")) {
                                        // test int if its in valuerange
                                        final float min = Float.valueOf(
                                                valuerange.substring(0, valuerange.indexOf("+")).trim()).floatValue();
                                        if (min == 0) {
                                            if (floatValue <= min) {
                                                errs.add(labelText + " must be greater than 0");
                                            }
                                        } else {
                                            if (floatValue < min) {
                                                errs.add(labelText + " must be greater than " + min);
                                            }
                                        }
                                    } else if (valuerange.contains(" to ")) {
                                        final float min = Float.valueOf(
                                                valuerange.substring(0, valuerange.indexOf(" to ")).trim())
                                                .floatValue();
                                        final float max = Float.valueOf(
                                                valuerange.substring(valuerange.indexOf(" to ") + 4,
                                                        valuerange.length()).trim()).floatValue();
                                        if (floatValue < min || floatValue > max) {
                                            errs.add(labelText + " must be in the range of " + min + " to " + max);
                                        }
                                    }

                                } catch (final NumberFormatException e) {
                                    errs.add(labelText + " must be an Float");
                                }
                            }
                        }
                        if ( !size.equals("")) {
                            final int intValue = Integer.valueOf(size.trim()).intValue();
                            if ( !value.trim().equals("")) {
                                if (value.length() > intValue) {
                                    errs.add(labelText + " must not exceed " + intValue + " in length");
                                }
                            }

                        }
                        found = false;
                    }
                } /*else {
                    final DataStruct ds3 = (DataStruct) o1;
                    final String n = ds3.getName();
                    final String v = ds3.getVersion();
                    parseDataStructForValidation(ds3, imageFile, errs);
                }*/
            }
        }

        /**
         * called after validation is done
         */
        public void complete(TreeMap<JLabel, JComponent> labelsAndComps, String dataStructShortname) {
        	LinkedHashMap<String, String> infoMap = new LinkedHashMap<String, String>();
            String value = "";
            final Set keySet = labelsAndComps.keySet();
            final Iterator iter = keySet.iterator();
            while (iter.hasNext()) {
                final JLabel label = (JLabel) iter.next();
                final JComponent comp = labelsAndComps.get(label);
                if (label.getName().equals("subjectkey")) {
                    guid = ((JTextField) comp).getText().trim();
                    outputFileNameBaseTable.put(file, guid);
                }
                final String key = label.getName();
                if (comp instanceof JTextField) {
                    value = ((JTextField) comp).getText().trim();
                } else if (comp instanceof JComboBox) {
                    value = (String) ( ((JComboBox) comp).getSelectedItem());
                }
                infoMap.put(key, value);
            }
            Hashtable<String, LinkedHashMap<String, String>> hTable;
            if(infoTable.get(file) == null) {
            	hTable = new Hashtable<String, LinkedHashMap<String, String>>();
            }else {
            	hTable = infoTable.get(file);
            }
            hTable.put(dataStructShortname, infoMap);
            
            
            infoTable.put(file, hTable);
            sourceTableModel.setValueAt("Yes", sourceTable.getSelectedRow(), 1);
        }

        public void windowActivated(final WindowEvent e) {
        // TODO Auto-generated method stub

        }

        public void windowClosed(final WindowEvent e) {
        // TODO Auto-generated method stub

        }

        public void windowClosing(final WindowEvent e) {
            //System.out.println("windowClosing");
            // enableDisableCompleteDataElementsButton();
            enableDisableFinishButton();

        }

        public void windowDeactivated(final WindowEvent e) {
        // TODO Auto-generated method stub

        }

        public void windowDeiconified(final WindowEvent e) {
        // TODO Auto-generated method stub

        }

        public void windowIconified(final WindowEvent e) {
        // TODO Auto-generated method stub

        }

        public void windowOpened(final WindowEvent e) {
        // TODO Auto-generated method stub

        }

        /**
         * This inner class is used to sort the list by instance number
         */
        private class JLabelComparator implements Comparator {
            public int compare(final Object oA, final Object oB) {
                final JLabel lA = (JLabel) oA;
                final JLabel lB = (JLabel) oB;
                final String aText = lA.getText();
                final String bText = lB.getText();
                return aText.compareTo(bText);
            }
        }

    }

    // inner class
    /**
     * represents the DataStructure of the xml
     */
    public class DataStruct extends Vector {
        private final String name;

        private String shortname;

        private String desc;

        private final String version;

        private String type;

        public DataStruct(final String name, final String version) {
            super();
            this.name = name;
            this.version = version;
        }

        public DataStruct(final String name, final String shortname, final String desc, final String version,
                final String type) {
            super();
            this.name = name;
            this.shortname = shortname;
            this.desc = desc;
            this.version = version;
            this.shortname = shortname;
            this.type = type;
        }

        public String getName() {
            return name;
        }

        public String getVersion() {
            return version;
        }

        public String getShortname() {
            return shortname;
        }

        public String getType() {
            return type;
        }

        public String getDesc() {
            return desc;
        }

    }

    /**
     * represents the DataElement of the XML
     * 
     * @author pandyan
     * 
     */
    public class DataElement {
        private final String name;

        private final String desc;

        private final String shortDesc;

        private final String type;

        private final String size;

        private final String required;

        private final String valuerange;

        private final String parentDataStruct;
        
        private final String parentDataStructShortname;

        public DataElement( String name,  String desc,  String shortDesc,  String type,
                 String size,  String required,  String valuerange,  String parentDataStruct, String parentDataStructShortname) {
            this.name = name;
            this.desc = desc;
            this.shortDesc = shortDesc;
            this.type = type;
            this.size = size;
            this.required = required;
            this.valuerange = valuerange;
            this.parentDataStruct = parentDataStruct;
            this.parentDataStructShortname = parentDataStructShortname;
        }

        public String getName() {
            return name;
        }

        public String getType() {
            return type;
        }

        public String getSize() {
            return size;
        }

        public String getRequired() {
            return required;
        }

        public String getValuerange() {
            return valuerange;
        }

        public String getDesc() {
            return desc;
        }

        public String getShortDesc() {
            return shortDesc;
        }

        public String getParentDataStruct() {
            return parentDataStruct;
        }
        
        public String getParentDataStructShortname() {
            return parentDataStructShortname;
        }

    }

    /**
     * Class used to store an xml tag's attribute (name and value)
     * 
     */
    public class XMLAttributes {

        private final String name;

        private final String value;

        public XMLAttributes(final String n, final String v) {
            name = n;
            value = v;
        }

        public String getName() {
            return name;
        }

        public String getValue() {
            return value;
        }

    }

    /**
     * Class that connects to NDAR data dictionary web service
     * 
     * @author pandyan
     * 
     */
    public class WebServiceThread extends Thread {

        PlugInDialogNDAR dial;

        /** Should be either NDAR_SERVER_PROD or NDAR_SERVER_DEMO. Removed storage/retrieval from preferences. */
        String ndarServer = NDARServer.PROD.name;

        String ndarDataStructName;
        
        

        WebServiceThread(final PlugInDialogNDAR dial) {
            super();
            this.dial = dial;
        }

        public void run() {
        	
            try {
                // TODO
                ndarDataStructName = Preferences.getProperty(Preferences.PREF_NDAR_PLUGIN_DATASTRUCT_NAME);
                if (ndarDataStructName == null) {
                    ndarDataStructName = "image01";
                    Preferences.setProperty(Preferences.PREF_NDAR_PLUGIN_DATASTRUCT_NAME, "image01");
                }

                // get OMElement from web service
                progressBar = new ViewJProgressBar("NDAR", "Connecting to NDAR data dictionary web service...", 0, 100,
                        true);
                progressBar.setVisible(true);
                progressBar.updateValue(20);
                client = Startup.getClient(ndarServer);
               
                
                
                
                
                try {
    	            publishedDataStructs = client.getPublishedStructures();
                }catch(AxisFault e) {
                	e.printStackTrace();
                }
                Iterator<OMElement> iter2 = publishedDataStructs.getChildElements();
                QName qDataType    = new QName(publishedDataStructs.getNamespace().getNamespaceURI(), "type");
                QName qName    = new QName(publishedDataStructs.getNamespace().getNamespaceURI(), "short_name");
                QName qParentDataStructure  = new QName(publishedDataStructs.getNamespace().getNamespaceURI(), "parent");
                progressBar.updateValue(60);
                while(iter2.hasNext()) {
                	OMElement e = iter2.next();
                	
                	if (e.getLocalName().equals("data_structure"))
                	 {
                	  String dataType = e.getAttributeValue(qDataType);
                	  String parent = e.getAttributeValue(qParentDataStructure);
                	  String shortname = e.getAttributeValue(qName);
                	if(dataType.equals("Imaging")) {
                		if(parent.trim().equals("")) {
                			//set up all the OMElements
                			OMElement ome = client.getDataDictionary(shortname);
                			documentElements.put(shortname, ome);
                			
                			
                		}
                	 }
                	  
                	 
                	  // TODO: Store or throw away the information about this data structure
                	 }

                	
                }
                

                progressBar.updateValue(80);
                
                progressBar.updateValue(100);
                progressBar.setVisible(false);
                progressBar.dispose();
                printlnToLog("Successful connection to NDAR data dictionary web service");
                addSourceButton.setEnabled(true);
            } catch (final Exception e) {
                e.printStackTrace();
                if (progressBar != null) {
                    progressBar.setVisible(false);
                    progressBar.dispose();
                    MipavUtil.displayError("Error in connecting to web service");
                    dial.dispose();
                }
            }
        }
    }

}
