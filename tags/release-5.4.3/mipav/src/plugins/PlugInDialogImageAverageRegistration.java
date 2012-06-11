import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.file.FileUtility;
import gov.nih.mipav.model.file.FileWriteOptions;
import gov.nih.mipav.model.structures.ModelImage;

import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewTableModel;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.dialogs.JDialogBase;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.WindowEvent;

import java.io.File;

import java.util.ArrayList;
import java.util.Vector;

import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JFileChooser;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.table.DefaultTableModel;


/**
 * DOCUMENT ME!
 *
 * @author  pandyan
 *
 *          <p>This class is the main dialog for this plugin that registers multiple source images to a target image and
 *          then averages them to get a final registered image</p>
 */
public class PlugInDialogImageAverageRegistration extends JDialogBase implements AlgorithmInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -5202342171083965017L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** handle to the algorithm. */
    private PlugInAlgorithmImageAverageRegistration alg;

    /** directory path to image file. */
    private String directory;

    /** boolean that determines if image is colr or not. */
    private boolean doColor;

    /** checkbox for including target image in final average calc. */
    private JCheckBox includeTargetCheckBox;

    /** boolean to determine if target image will be used in final average calculation. */
    private boolean includeTargetImageinCalc;

    /** Document Me. */
    private boolean isTargetDICOM;

    /** button that brings up registration options dialog. */
    private JButton registrationOptionsButton;

    /** handle to the registration options dialog. */
    private PlugInImageAverageRegistration_RegOptionsDialog regOptions;

    /** result image name. */
    private String resultImageName = "";

    /** checkbox for saving result image. */
    private JCheckBox saveAsCheckBox;

    /** input box for populating resultImageName. */
    private JTextField saveAsTextField;

    /** boolean to determine if intermediate images should be saved. */
    private boolean saveIntermediateRegImages;

    /** checkbox for saving intermediate registered images. */
    private JCheckBox saveIntermRegCheckBox;

    /** boolean to determine if the result image should be saved. */
    private boolean saveResultImage;

    /** List of Source filenames. */
    private ArrayList<String> srcFilenamesArrList = new ArrayList<String>();

    /** array to store the srcImage files. */
    private File[] srcFiles;

    /** table to display the src image names. */
    private JTable srcImagesTable;

    /** table model for the srcimages. */
    private ViewTableModel srcTableModel;

    /** Target Browse Button. */
    private JButton targetBrowseButton;

    /** Document me. */
    private String targetDirectory;

    /** array to store the target image file. */
    private File targetFile;

    /** model image for target and result images. */
    private ModelImage targetImage, resultImage;

    /** table to store the target image name. */
    private JTable targetImageTable;

    /** table model for the targetImage. */
    private ViewTableModel targetTableModel;

    /** handle to ViewUserInterface. */
    private ViewUserInterface UI;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * constructor.
     *
     * @param  modal  DOCUMENT ME!
     */
    public PlugInDialogImageAverageRegistration(boolean modal) {
        super(modal);
        UI = ViewUserInterface.getReference();
        init();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * action performed method.
     *
     * @param  e  DOCUMENT ME!
     */
    public void actionPerformed(ActionEvent e) {
        String command = e.getActionCommand();
        String filename = "";
        String fullPath = "";



        if (command.equalsIgnoreCase("srcBrowse")) {
            JFileChooser srcFileChooser = new JFileChooser();

            if (UI.getDefaultDirectory() != null) {
                srcFileChooser.setCurrentDirectory(new File(UI.getDefaultDirectory()));
            } else {
                srcFileChooser.setCurrentDirectory(new File(System.getProperties().getProperty("user.dir")));
            }

            srcFileChooser.setMultiSelectionEnabled(true);

            int srcReturnVal = srcFileChooser.showOpenDialog(this);

            if (srcReturnVal == JFileChooser.APPROVE_OPTION) {

                srcFiles = srcFileChooser.getSelectedFiles();
                directory = String.valueOf(srcFileChooser.getCurrentDirectory()) + File.separatorChar;
                UI.setDefaultDirectory(directory);

                for (int k = 0; k < srcFiles.length; k++) {

                    try {
                        filename = srcFiles[k].getName();

                        fullPath = directory + filename;

                        // we will display the full path
                        Vector<String> rowData = new Vector<String>();
                        rowData.add(fullPath);
                        srcTableModel.addRow(rowData);

                        // we will put the whole path and filename in this list that will then go to the alg
                        srcFilenamesArrList.add(fullPath);
                    } catch (OutOfMemoryError err) {
                        MipavUtil.displayError("Out of memory!");

                        return;
                    }


                }
            }
        } else if (command.equalsIgnoreCase("targetBrowse")) {
            JFileChooser targetFileChooser = new JFileChooser();

            if (UI.getDefaultDirectory() != null) {
                targetFileChooser.setCurrentDirectory(new File(UI.getDefaultDirectory()));
            } else {
                targetFileChooser.setCurrentDirectory(new File(System.getProperties().getProperty("user.dir")));
            }

            targetFileChooser.setMultiSelectionEnabled(false);

            int targetReturnVal = targetFileChooser.showOpenDialog(this);

            if (targetReturnVal == JFileChooser.APPROVE_OPTION) {

                try {
                    isTargetDICOM = false;

                    boolean isMultifile = false;
                    targetFile = targetFileChooser.getSelectedFile();

                    FileIO fileIO = new FileIO();
                    filename = targetFile.getName();
                    directory = String.valueOf(targetFileChooser.getCurrentDirectory()) + File.separatorChar;
                    UI.setDefaultDirectory(directory);

                    boolean zerofunused[] = new boolean[1];
                    int fileType = FileUtility.getFileType(filename, directory, false, false, zerofunused);

                    if (fileType == FileUtility.DICOM) {
                        isMultifile = true;
                        isTargetDICOM = true;
                        targetDirectory = directory;
                    }

                    fullPath = directory + filename;
                    targetImage = fileIO.readImage(filename, directory, isMultifile, null);

                    if (targetImage == null) {
                        System.err.println("Error loading file");

                        return;
                    }

                    Vector<String> rowData = new Vector<String>();
                    rowData.add(fullPath);

                    if (targetTableModel.getRowCount() == 0) {
                        targetTableModel.addRow(rowData);
                    } else {
                        targetTableModel.setValueAt(fullPath, 0, 0);
                    }

                    // need to test if target image is color or not
                    if (targetImage.isColorImage()) {
                        doColor = true;
                    } else {
                        doColor = false;
                    }

                    // instantiate the dialog...but it is not visible yet....in case user just wants defaults
                    regOptions = new PlugInImageAverageRegistration_RegOptionsDialog(doColor);

                    // enable the Registration Options Button
                    registrationOptionsButton.setEnabled(true);

                    // disable the target Browse Button
                    targetBrowseButton.setEnabled(false);

                } catch (OutOfMemoryError err) {
                    MipavUtil.displayError("Out of memory!");

                    return;
                }
            }
        } else if (command.equalsIgnoreCase("cancel")) {

            if (regOptions != null) {
                regOptions.dispose();
            }

            finalize();
            dispose();
        } else if (command.equalsIgnoreCase("ok")) {

            if ((srcFilenamesArrList.size() == 0) || (targetImage == null)) {
                MipavUtil.displayError("Both Source Image(s) and Target Image are required");

                return;
            }

            if (targetImage.getNDims() < 3) {
                MipavUtil.displayError("2D registration not supported");

                return;
            }

            if (regOptions.setVariables()) {
                includeTargetImageinCalc = includeTargetCheckBox.isSelected();
                saveIntermediateRegImages = saveIntermRegCheckBox.isSelected();
                saveResultImage = saveAsCheckBox.isSelected();

                if (saveResultImage) {
                    resultImageName = saveAsTextField.getText();

                    if (resultImageName.trim().equals("")) {
                        MipavUtil.displayError("Result Image Name is required");

                        return;
                    }
                }

                callAlgorithm();

                if (regOptions != null) {
                    regOptions.dispose();
                }

            }

        } else if (command.equalsIgnoreCase("options")) {
            regOptions.setVisible(true);
        } else if (command.equalsIgnoreCase("saveAs")) {

            if (saveAsCheckBox.isSelected()) {
                saveAsTextField.setEnabled(true);
            } else {
                saveAsTextField.setText("");
                saveAsTextField.setEnabled(false);
            }
        }


    }

    /**
     * algorithmPerformed Method This is called after the algorithm is finished.
     *
     * @param  algorithm  DOCUMENT ME!
     */
    public void algorithmPerformed(AlgorithmBase algorithm) {

        // grab the result image
        resultImage = alg.getResultImage();

        if (resultImage == null) {
            MipavUtil.displayError("Result Image is null");
            finalize();
            dispose();

            return;
        }

        // display result image if they did not select save

        if (!saveAsCheckBox.isSelected()) {

            try {
                new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
            } catch (OutOfMemoryError error) {
                MipavUtil.displayError("Out of memory: unable to open new frame");
            }
        }


        // need to save image here if they selected the checkbox
        if (saveAsCheckBox.isSelected()) {

            if (!resultImageName.equals("")) {
                String dir = directory;

                // if the target was a dicom, we want to save the result image 1 level up
                if (isTargetDICOM) {
                    int sep = targetDirectory.lastIndexOf(File.separatorChar);
                    String directory2 = targetDirectory.substring(0, sep);
                    sep = directory2.lastIndexOf(File.separatorChar);
                    dir = directory2.substring(0, sep + 1);
                }

                FileWriteOptions options = new FileWriteOptions(resultImageName, dir, true);
                FileIO io = new FileIO();
                io.writeImage(resultImage, options);
                Preferences.debug("\n", Preferences.DEBUG_ALGORITHM);
                Preferences.debug("*** Final Registered Image saved to " + dir + resultImageName + "\n",
                                  Preferences.DEBUG_ALGORITHM);

                if (resultImage != null) {
                    resultImage.disposeLocal();
                    resultImage = null;
                }
            }

        }

        Preferences.debug("\n", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("*** Completed Image Average Registration ***", Preferences.DEBUG_ALGORITHM);
        Preferences.debug("\n", Preferences.DEBUG_ALGORITHM);
        finalize();
        dispose();


    }


    /**
     * DOCUMENT ME!
     */
    public void finalize() {

        if (targetImage != null) {
            targetImage.disposeLocal();
            targetImage = null;
        }

    }

    /**
     * init method.
     */
    public void init() {
        setForeground(Color.black);
        setTitle("Image Average Registration");

        GridBagLayout mainPanelGridBagLayout = new GridBagLayout();
        GridBagConstraints mainPanelConstraints = new GridBagConstraints();
        JPanel mainPanel = new JPanel(mainPanelGridBagLayout);


        JPanel srcPanel = new JPanel();
        srcTableModel = new ViewTableModel();
        srcTableModel.addColumn("Source Images");
        srcImagesTable = new JTable(srcTableModel);
        srcImagesTable.setPreferredScrollableViewportSize(new Dimension(300, 200));

        JScrollPane srcImagesScrollPane = new JScrollPane(srcImagesTable);
        srcImagesTable.addMouseListener(new MouseHandler());
        srcPanel.add(srcImagesScrollPane);

        JButton srcBrowseButton = new JButton("Browse");
        srcBrowseButton.addActionListener(this);
        srcBrowseButton.setActionCommand("srcBrowse");
        srcPanel.add(srcBrowseButton);

        JPanel targetPanel = new JPanel();
        targetTableModel = new ViewTableModel();
        targetTableModel.addColumn("Target Image");
        targetImageTable = new JTable(targetTableModel);
        targetImageTable.addMouseListener(new MouseHandler());
        targetImageTable.setPreferredScrollableViewportSize(new Dimension(300, 30));

        JScrollPane targetImageScrollPane = new JScrollPane(targetImageTable);
        targetPanel.add(targetImageScrollPane);
        targetBrowseButton = new JButton("Browse");
        targetBrowseButton.addActionListener(this);
        targetBrowseButton.setActionCommand("targetBrowse");
        targetPanel.add(targetBrowseButton);

        GridBagLayout prefsPanelGridBagLayout = new GridBagLayout();
        GridBagConstraints prefsPanelConstraints = new GridBagConstraints();
        JPanel prefsPanel = new JPanel(prefsPanelGridBagLayout);
        registrationOptionsButton = new JButton("Registration Options");
        registrationOptionsButton.addActionListener(this);
        registrationOptionsButton.setActionCommand("options");
        registrationOptionsButton.setEnabled(false);
        prefsPanelConstraints.gridx = 0;
        prefsPanelConstraints.gridy = 0;
        prefsPanelConstraints.gridwidth = 2;
        prefsPanelConstraints.anchor = GridBagConstraints.CENTER;
        prefsPanelConstraints.insets = new Insets(15, 0, 15, 0);
        prefsPanelGridBagLayout.setConstraints(registrationOptionsButton, prefsPanelConstraints);
        includeTargetCheckBox = new JCheckBox("Include target image in averaging calculation");
        prefsPanelConstraints.gridx = 0;
        prefsPanelConstraints.gridy = 1;
        prefsPanelConstraints.gridwidth = 2;
        prefsPanelConstraints.anchor = GridBagConstraints.WEST;
        prefsPanelConstraints.insets = new Insets(0, 0, 0, 0);
        prefsPanelGridBagLayout.setConstraints(includeTargetCheckBox, prefsPanelConstraints);
        saveIntermRegCheckBox = new JCheckBox("Save intermediate registered images");
        prefsPanelConstraints.gridx = 0;
        prefsPanelConstraints.gridy = 2;
        prefsPanelConstraints.gridwidth = 2;
        prefsPanelConstraints.anchor = GridBagConstraints.WEST;
        prefsPanelConstraints.insets = new Insets(0, 0, 0, 0);
        prefsPanelGridBagLayout.setConstraints(saveIntermRegCheckBox, prefsPanelConstraints);
        saveAsCheckBox = new JCheckBox("Save result image as  ");
        saveAsCheckBox.addActionListener(this);
        saveAsCheckBox.setActionCommand("saveAs");
        prefsPanelConstraints.gridx = 0;
        prefsPanelConstraints.gridy = 3;
        prefsPanelConstraints.gridwidth = 1;
        prefsPanelConstraints.anchor = GridBagConstraints.WEST;
        prefsPanelConstraints.insets = new Insets(0, 0, 10, 0);
        prefsPanelGridBagLayout.setConstraints(saveAsCheckBox, prefsPanelConstraints);
        saveAsTextField = new JTextField(15);
        saveAsTextField.setEnabled(false);
        prefsPanelConstraints.gridx = 1;
        prefsPanelConstraints.gridy = 3;
        prefsPanelConstraints.gridwidth = 1;
        prefsPanelConstraints.insets = new Insets(0, 0, 10, 0);
        prefsPanelGridBagLayout.setConstraints(saveAsTextField, prefsPanelConstraints);
        prefsPanel.add(registrationOptionsButton);
        prefsPanel.add(includeTargetCheckBox);
        prefsPanel.add(saveIntermRegCheckBox);
        prefsPanel.add(saveAsCheckBox);
        prefsPanel.add(saveAsTextField);

        mainPanelConstraints.gridx = 0;
        mainPanelConstraints.gridy = 0;
        mainPanelGridBagLayout.setConstraints(srcPanel, mainPanelConstraints);
        mainPanelConstraints.gridx = 0;
        mainPanelConstraints.gridy = 1;
        mainPanelGridBagLayout.setConstraints(targetPanel, mainPanelConstraints);
        mainPanelConstraints.gridx = 0;
        mainPanelConstraints.gridy = 2;
        mainPanelGridBagLayout.setConstraints(prefsPanel, mainPanelConstraints);
        mainPanel.add(srcPanel);
        mainPanel.add(targetPanel);
        mainPanel.add(prefsPanel);

        JPanel OKCancelPanel = new JPanel();
        buildOKButton();
        OKButton.setActionCommand("ok");
        OKCancelPanel.add(OKButton, BorderLayout.WEST);
        buildCancelButton();
        cancelButton.setActionCommand("cancel");
        OKCancelPanel.add(cancelButton, BorderLayout.EAST);

        getContentPane().add(mainPanel, BorderLayout.CENTER);
        getContentPane().add(OKCancelPanel, BorderLayout.SOUTH);

        pack();
        setResizable(false);
        setVisible(true);
    }


    /**
     * DOCUMENT ME!
     *
     * @param  event  DOCUMENT ME!
     */
    public void windowClosing(WindowEvent event) {
        super.windowClosing(event);

        if (regOptions != null) {
            regOptions.dispose();
        }

        finalize();
        dispose();
    }

    /**
     * callAlgorithm method.
     */
    protected void callAlgorithm() {

        // call the algorithm
        alg = new PlugInAlgorithmImageAverageRegistration(srcFilenamesArrList, targetImage, regOptions.getCost(),
                                                          regOptions.getDOF(), regOptions.getInterp(),
                                                          regOptions.getInterp2(), regOptions.getRotateBeginX(),
                                                          regOptions.getRotateEndX(), regOptions.getCoarseRateX(),
                                                          regOptions.getFineRateX(), regOptions.getRotateBeginY(),
                                                          regOptions.getRotateEndY(), regOptions.getCoarseRateY(),
                                                          regOptions.getFineRateY(), regOptions.getRotateBeginZ(),
                                                          regOptions.getRotateEndZ(), regOptions.getCoarseRateZ(),
                                                          regOptions.getFineRateZ(), regOptions.isMaxOfMinResol(),
                                                          includeTargetImageinCalc, saveIntermediateRegImages, doColor,
                                                          isTargetDICOM);

        alg.addListener(this);


        setVisible(false);

        if (alg.startMethod(Thread.MIN_PRIORITY) == false) {
            MipavUtil.displayError("A thread is already running on this object");
        }


    }

    //~ Inner Classes --------------------------------------------------------------------------------------------------

    /**
     * This is an inner class that is used by the tables within the dialogs to delete src and target images from the
     * tables.
     */
    private class MouseHandler extends MouseAdapter implements ActionListener {

        /** column name of the corresponding table. */
        String columnName;

        /** name of the image file. */
        String imageName;

        /** menu item in the popup menu. */
        JMenuItem menuItem;

        /** DefaultTableModel for the corresponding table. */
        DefaultTableModel model;

        /** point representing where the mouse event took place. */
        Point p;

        /** popup menu for removing files. */
        JPopupMenu popupMenu;

        /** rowIndex. */
        int rowIndex;

        /**
         * actionPerformed.
         *
         * @param  event  DOCUMENT ME!
         */
        public void actionPerformed(ActionEvent event) {
            String command = event.getActionCommand();

            if (command.equals("remove")) {

                if (columnName.equals("Source Images")) {

                    // need to remove from the table and the ArrayList
                    PlugInDialogImageAverageRegistration.this.srcTableModel.removeRow(rowIndex);

                    // need to remove that entry from the ArrList
                    PlugInDialogImageAverageRegistration.this.srcFilenamesArrList.remove(rowIndex);
                } else if (columnName.equals("Target Image")) {

                    // need to remove from table, make targetImage to null, set the regOptions instance to null, and
                    // disable regOptions button
                    PlugInDialogImageAverageRegistration.this.targetTableModel.removeRow(rowIndex);

                    if (PlugInDialogImageAverageRegistration.this.targetImage != null) {
                        PlugInDialogImageAverageRegistration.this.targetImage.disposeLocal();
                        PlugInDialogImageAverageRegistration.this.targetImage = null;
                    }

                    PlugInDialogImageAverageRegistration.this.registrationOptionsButton.setEnabled(false);
                    PlugInDialogImageAverageRegistration.this.regOptions = null;

                    PlugInDialogImageAverageRegistration.this.targetBrowseButton.setEnabled(true);


                }

            }

        }

        /**
         * mouseClicked.
         *
         * @param  e  DOCUMENT ME!
         */
        public void mouseClicked(MouseEvent e) {

            if (e.getButton() == MouseEvent.BUTTON3) {
                p = new Point(e.getX(), e.getY());
                popupMenu = new JPopupMenu();
                rowIndex = ((JTable) e.getSource()).rowAtPoint(p);
                model = (DefaultTableModel) ((JTable) e.getSource()).getModel();
                imageName = (String) model.getValueAt(rowIndex, 0);
                menuItem = new JMenuItem("Remove " + imageName);
                menuItem.addActionListener(this);
                menuItem.setActionCommand("remove");
                popupMenu.add(menuItem);
                popupMenu.show((JTable) e.getSource(), e.getX(), e.getY());
                columnName = ((JTable) e.getSource()).getColumnName(0);
            }
        }


    }


}
