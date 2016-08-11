import gov.nih.mipav.plugins.JDialogStandalonePlugin;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.file.FileInfoBase.Unit;
import gov.nih.mipav.model.file.FileInfoBase.UnitType;

import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewUserInterface;

import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.WindowEvent;
import java.io.*;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;

import javax.swing.*;
import javax.swing.border.TitledBorder;
import javax.swing.filechooser.FileFilter;
import javax.swing.text.*;

import org.apache.commons.csv.CSVFormat;
import org.apache.commons.csv.CSVRecord;


/**
 * New plugin for Akanni Clarke of the Giniger Lab. Conceptually similar to the DrosophilaCreateSWC plugin written by
 * Nish Pandya, this one also includes keeping track of statistics. These statistics are: branch order, number, and
 * length as well as the distance along the axon/parent that the branch originates from.
 * 
 * @author wangvg
 * 
 */
public class PlugInDialog3DSWCStats extends JDialogStandalonePlugin implements AlgorithmInterface {

    /**
	 * 
	 */
    private static final long serialVersionUID = 2942624971551641398L;

    private JTextField textField;

    // private JTextField imageField;

    private JFileChooser ivFileChooser;

    private JFileChooser csvFileChooser;

    @SuppressWarnings("rawtypes")
    private JComboBox resolutionUnits;

    private ArrayList<ArrayList<ArrayList<float[]>>> allTimeCoordinates; // **ADDED**

    private JTextPane textArea;

    private JRadioButton axonRB;

    private JRadioButton customRB;

    private JRadioButton densityRB;

    private boolean chooseIV;

    private PlugInAlgorithm3DSWCViewer alg;

    private boolean writeStep;

    private boolean locked;

    private File[] densityFiles;

    private int densityCount;

    private int timePtCounter;

    private boolean is3DVol;

    private File ivFile;

    private JTextField splitField;

    private JButton splitBrowse;

    private float[] growthConeLenList;

    public static final SimpleAttributeSet BLACK_TEXT;

    public static final SimpleAttributeSet BLACK_BOLD_TEXT;

    public static final SimpleAttributeSet GREEN_TEXT;

    public static final SimpleAttributeSet RED_TEXT;

    static {
        BLACK_TEXT = new SimpleAttributeSet();
        StyleConstants.setFontFamily(BLACK_TEXT, "Serif");
        StyleConstants.setFontSize(BLACK_TEXT, 12);

        BLACK_BOLD_TEXT = new SimpleAttributeSet(BLACK_TEXT);
        StyleConstants.setBold(BLACK_BOLD_TEXT, true);

        RED_TEXT = new SimpleAttributeSet(BLACK_TEXT);
        StyleConstants.setForeground(RED_TEXT, Color.red.darker());

        GREEN_TEXT = new SimpleAttributeSet(BLACK_TEXT);
        StyleConstants.setForeground(GREEN_TEXT, Color.green.darker());
    }

    public PlugInDialog3DSWCStats() {
        super();

        writeStep = false;
        locked = false;

        init();

        final String version = "2.0";
        final String lastUpdate = "8/5/2016";

        final String message = "Initializing v " + version + "\n" + "Last updated: " + lastUpdate + "\n" + "-----------------------------------------";

        append(message, BLACK_TEXT);
    }

    @Override
    public void actionPerformed(final ActionEvent e) {
        final String command = e.getActionCommand();
        final Object source = e.getSource();

        if (command.equals("ok")) {
            if ( !locked && (alg == null || !alg.isViewerOpen())) {
                if ( !writeStep) {
                    locked = true;
                    callAlgorithm();
                }
            }
        } else if (command.equals("cancel")) {
            if (isExitRequired()) {
                ViewUserInterface.getReference().windowClosing(new WindowEvent(this, WindowEvent.WINDOW_CLOSING));
            } else {
                dispose();
            }
        } else if (source == ivFileChooser && command.equals(JFileChooser.APPROVE_SELECTION)) {
            final File selected = ivFileChooser.getSelectedFile();
            final String name = selected.getAbsolutePath();
            Preferences.setImageDirectory(selected);
            if (chooseIV) {
                textField.setText(name);
            } else {
                // imageField.setText(name);
            }
        } else if (command.equals("Browse")) {
            chooseIV = true;
            chooseDir();
            // } else if (command.equals("BrowseImage") && !densityRB.isSelected()) {
            // chooseIV = false;
            // chooseDir();
        } else if (command.equals("density")) {
            // imageField.setEditable(false);
            splitField.setEditable(false);
        } else if (command.startsWith("notDensity")) {
            // imageField.setEditable(true);
            splitField.setEditable(true);
        } else if (command.equals("BrowseGrowthConeLen")) {
            chooseCSVFile();
        } else if (source == csvFileChooser && command.equals(JFileChooser.APPROVE_SELECTION)) {
            final File selected = csvFileChooser.getSelectedFile();
            growthConeLenList = readGrowthConeLenCSV(selected);
            if (growthConeLenList != null) {
                splitField.setEditable(false);
            }
        } else {
            super.actionPerformed(e);
        }
    }

    @Override
    public void algorithmPerformed(final AlgorithmBase algorithm) {
        if (algorithm instanceof PlugInAlgorithm3DSWCViewer) {
            if (algorithm.isCompleted()) {
                // For branch density, iterate through all the files
                if (densityRB.isSelected() && writeStep) {
                    densityCount++;
                    if (densityCount < densityFiles.length || !is3DVol) {
                        if (is3DVol) {
                            ivFile = densityFiles[densityCount];
                            if ( !readSurfaceFile(ivFile)) {
                                MipavUtil.displayError("Error reading IV file: " + ivFile);
                                locked = false;
                                return;
                            }
                        }

                        timePtCounter++;
                        ArrayList<ArrayList<float[]>> TimePoint;
                        for (; timePtCounter < allTimeCoordinates.size(); timePtCounter++) {
                            TimePoint = allTimeCoordinates.get(timePtCounter);
                            if (TimePoint == null || TimePoint.size() == 0) {
                                is3DVol = true;
                                continue;
                            }

                            if (allTimeCoordinates.size() == 1) {
                                is3DVol = true;
                            }

                            String baseName = ivFile.getName().substring(0, ivFile.getName().lastIndexOf("."));
                            if ( !is3DVol) {
                                baseName += "_T=" + timePtCounter;

                            }
                            alg = new PlugInAlgorithm3DSWCViewer(TimePoint, ivFile.getParent(), baseName, textArea, (String) resolutionUnits.getSelectedItem());
                            alg.addListener(this);
                            new PlugInDialog3DSWCViewer(textArea, (String) resolutionUnits.getSelectedItem(), alg);

                            if (isRunInSeparateThread()) {
                                if (alg.startMethod(Thread.MIN_PRIORITY) == false) {
                                    MipavUtil.displayError("A thread is already running on this object");
                                }
                            } else {
                                alg.run();
                            }

                            break;
                        }
                    } else {
                        locked = false;
                    }
                } else {
                    locked = false;

                }
                writeStep ^= true;

                // If the 3D viewer isn't being used, go straight to the write step
                if ( !densityRB.isSelected() && !alg.isViewerOpen() && writeStep) {
                    locked = true;
                    alg.write();

                    timePtCounter++;
                    ArrayList<ArrayList<float[]>> TimePoint;
                    for (; timePtCounter < allTimeCoordinates.size(); timePtCounter++) {
                        TimePoint = allTimeCoordinates.get(timePtCounter);
                        if (TimePoint == null || TimePoint.size() == 0) {
                            is3DVol = true;
                            continue;
                        }

                        if (allTimeCoordinates.size() == 1) {
                            is3DVol = true;
                        }

                        String baseName = ivFile.getName().substring(0, ivFile.getName().lastIndexOf("."));
                        if ( !is3DVol) {
                            baseName += "_T=" + timePtCounter;

                        }

                        // alg = new PlugInAlgorithm3DSWCViewer(imageField.getText(), file, textArea, (String)
                        // resolutionUnits.getSelectedItem(), axonRB.isSelected(),
                        // false);
                        alg = new PlugInAlgorithm3DSWCViewer(TimePoint, ivFile.getParent(), baseName, textArea, (String) resolutionUnits.getSelectedItem(),
                                axonRB.isSelected(), false);
                        float splitDist;
                        try {
                            splitDist = Float.valueOf(splitField.getText());
                        } catch (final NumberFormatException e) {
                            MipavUtil.displayError("Enter the length of the growth cone");
                            locked = false;
                            return;
                        }
                        alg.setSplit(splitDist);
                        alg.addListener(this);

                        if (isRunInSeparateThread()) {
                            if (alg.startMethod(Thread.MIN_PRIORITY) == false) {
                                MipavUtil.displayError("A thread is already running on this object");
                            }
                        } else {
                            alg.run();
                        }

                        break;
                    }
                }

                if ( !densityRB.isSelected() && !writeStep && !alg.isViewerOpen()) {
                    timePtCounter++;
                    ArrayList<ArrayList<float[]>> TimePoint;
                    for (; timePtCounter < allTimeCoordinates.size(); timePtCounter++) {
                        TimePoint = allTimeCoordinates.get(timePtCounter);
                        if (TimePoint == null || TimePoint.size() == 0) {
                            is3DVol = true;
                            continue;
                        }

                        if (allTimeCoordinates.size() == 1) {
                            is3DVol = true;
                        }

                        String baseName = ivFile.getName().substring(0, ivFile.getName().lastIndexOf("."));
                        if ( !is3DVol) {
                            baseName += "_T=" + timePtCounter;

                        }

                        // Open the viewer if the a custom axon is to be chosen

                        // alg = new PlugInAlgorithm3DSWCViewer(imageField.getText(), file, textArea, (String)
                        // resolutionUnits.getSelectedItem(), false, true);
                        alg = new PlugInAlgorithm3DSWCViewer(TimePoint, ivFile.getParent(), baseName, textArea, (String) resolutionUnits.getSelectedItem(),
                                false, true);
                        new PlugInDialog3DSWCViewer(textArea, (String) resolutionUnits.getSelectedItem(), alg);

                        float splitDist;
                        try {
                            splitDist = Float.valueOf(splitField.getText());
                        } catch (final NumberFormatException e) {
                            MipavUtil.displayError("Enter the length of the growth cone");
                            locked = false;
                            return;
                        }
                        alg.setSplit(splitDist);

                        alg.addListener(this);

                        if (isRunInSeparateThread()) {
                            if (alg.startMethod(Thread.MIN_PRIORITY) == false) {
                                MipavUtil.displayError("A thread is already running on this object");
                            }
                        } else {
                            alg.run();
                        }

                        break;
                    }
                }

            } else {
                locked = false;
                writeStep = false;
            }

        }
    }

    protected void callAlgorithm() {

        final String fileName = textField.getText();
        final File file = new File(fileName);

        if (densityRB.isSelected()) {

            final File parent = file.getParentFile();
            final File[] list = parent.listFiles(new FilenameFilter() {

                @Override
                public boolean accept(final File dir, final String name) {
                    final int ind = name.lastIndexOf(".");
                    if (ind < 0) {
                        return false;
                    }
                    // skip hidden files created on Akanni's Mac
                    if (name.startsWith("._")) {
                        return false;
                    }
                    final String ext = name.substring(ind);
                    if (ext.equalsIgnoreCase(".iv")) {
                        return true;
                    } else {
                        return false;
                    }
                }
            });

            // Sort list into correct order

            Arrays.sort(list, new Comparator<File>() {

                @Override
                public int compare(final File o1, final File o2) {
                    final String s1 = o1.getName();
                    final String s2 = o2.getName();

                    final String s1NoNum = s1.replaceAll("[0-9]", "");
                    final String s2NoNum = s2.replaceAll("[0-9]", "");

                    final int compare = s1NoNum.compareTo(s2NoNum);

                    if (compare == 0) {
                        // Without numbers, the two are the same
                        final String s1Num = s1.replaceAll("[^0-9]", "");
                        final String s2Num = s2.replaceAll("[^0-9]", "");
                        final String s1NumFinal;
                        final String s2NumFinal;

                        // Truncate so that you aren't growing too large
                        final int length = String.valueOf(Integer.MAX_VALUE).length() - 1;
                        if (s1Num.length() > length) {
                            s1NumFinal = s1Num.substring(s1Num.length() - length);
                        } else {
                            s1NumFinal = s1Num;
                        }

                        if (s2Num.length() > length) {
                            s2NumFinal = s2Num.substring(s2Num.length() - length);
                        } else {
                            s2NumFinal = s2Num;
                        }

                        // Compare the left over numbers
                        final int s1Int = Integer.valueOf(s1NumFinal);
                        final int s2Int = Integer.valueOf(s2NumFinal);

                        return Integer.valueOf(s1Int).compareTo(Integer.valueOf(s2Int));
                    } else {
                        return compare;
                    }
                }

            });

            final String parentStr = parent.getAbsolutePath();
            final File csvFile = new File(parentStr + File.separator + "branch_density.csv");
            try {
                final FileWriter fw = new FileWriter(csvFile);
                fw.append("Branch Density Statistics\n");
                fw.close();
            } catch (final IOException e) {
                final String message = "Could not create a CSV file for writing. Make sure to close any open CSVs.";
                append(message, RED_TEXT);
                locked = false;
                return;
            }

            densityFiles = list;
            densityCount = 0;

            ivFile = list[densityCount];
            if ( !readSurfaceFile(ivFile)) {
                MipavUtil.displayError("Error reading IV file: " + ivFile);
                locked = false;
                return;
            }

            ArrayList<ArrayList<float[]>> TimePoint;
            for (timePtCounter = 0; timePtCounter < allTimeCoordinates.size(); timePtCounter++) {
                TimePoint = allTimeCoordinates.get(timePtCounter);
                if (TimePoint == null || TimePoint.size() == 0) {
                    is3DVol = true;
                    continue;
                }

                if (allTimeCoordinates.size() == 1) {
                    is3DVol = true;
                }

                String baseName = ivFile.getName().substring(0, ivFile.getName().lastIndexOf("."));
                if ( !is3DVol) {
                    baseName += "_T=" + timePtCounter;

                }
                alg = new PlugInAlgorithm3DSWCViewer(TimePoint, ivFile.getParent(), baseName, textArea, (String) resolutionUnits.getSelectedItem());
                alg.addListener(this);
                new PlugInDialog3DSWCViewer(textArea, (String) resolutionUnits.getSelectedItem(), alg);

                break;
            }
        } else {

            if ( !fileName.endsWith(".iv")) {
                MipavUtil.displayError("This file is not the correct format");
                locked = false;
                return;
            }

            if ( !file.exists()) {
                MipavUtil.displayError("This file does not exist");
                locked = false;
                return;
            }

            // final String imageStr = imageField.getText();
            //
            // if (imageStr.length() == 0) {
            // MipavUtil.displayError("Please input an image");
            // locked = false;
            // return;
            // }
            //
            // final File imageFile = new File(imageStr);
            // if ( !imageFile.exists()) {
            // MipavUtil.displayError("This image does not exist");
            // locked = false;
            // return;
            // }

            float splitDist;

            if (growthConeLenList != null && growthConeLenList.length > 0) {
                if (timePtCounter > growthConeLenList.length) {
                    MipavUtil.displayError("Growth cone length CSV value missing for time point " + timePtCounter);
                    locked = false;
                    return;
                }
                splitDist = growthConeLenList[timePtCounter];
            } else {
                try {
                    splitDist = Float.valueOf(splitField.getText());
                } catch (final NumberFormatException e) {
                    MipavUtil.displayError("Enter the length of the growth cone");
                    locked = false;
                    return;
                }
            }

            append("Reading " + file.getName(), BLACK_BOLD_TEXT);

            ivFile = file;
            if ( !readSurfaceFile(ivFile)) {
                MipavUtil.displayError("Error reading IV file: " + ivFile);
                locked = false;
                return;
            }

            is3DVol = false;

            ArrayList<ArrayList<float[]>> TimePoint;
            for (timePtCounter = 0; timePtCounter < allTimeCoordinates.size(); timePtCounter++) {
                TimePoint = allTimeCoordinates.get(timePtCounter);
                if (TimePoint == null || TimePoint.size() == 0) {
                    is3DVol = true;
                    continue;
                }

                if (allTimeCoordinates.size() == 1) {
                    is3DVol = true;
                }

                String baseName = ivFile.getName().substring(0, ivFile.getName().lastIndexOf("."));
                if ( !is3DVol) {
                    baseName += "_T=" + timePtCounter;

                }

                // Open the viewer if the a custom axon is to be chosen
                if (customRB.isSelected()) {
                    // alg = new PlugInAlgorithm3DSWCViewer(imageField.getText(), file, textArea, (String)
                    // resolutionUnits.getSelectedItem(), false, true);
                    alg = new PlugInAlgorithm3DSWCViewer(TimePoint, ivFile.getParent(), baseName, textArea, (String) resolutionUnits.getSelectedItem(), false,
                            true);
                    new PlugInDialog3DSWCViewer(textArea, (String) resolutionUnits.getSelectedItem(), alg);
                } else {
                    // alg = new PlugInAlgorithm3DSWCViewer(imageField.getText(), file, textArea, (String)
                    // resolutionUnits.getSelectedItem(), axonRB.isSelected(),
                    // false);
                    alg = new PlugInAlgorithm3DSWCViewer(TimePoint, ivFile.getParent(), baseName, textArea, (String) resolutionUnits.getSelectedItem(),
                            axonRB.isSelected(), false);
                }

                break;
            }

            alg.setSplit(splitDist);
            alg.addListener(this);

        }

        if (isRunInSeparateThread()) {
            if (alg.startMethod(Thread.MIN_PRIORITY) == false) {
                MipavUtil.displayError("A thread is already running on this object");
                locked = false;
            }
        } else {
            alg.run();
        }
    }

    @SuppressWarnings({"rawtypes", "unchecked"})
    private void init() {

        setTitle("Imaris to 3D SWC with stats");

        getContentPane().removeAll();
        getContentPane().setLayout(new BoxLayout(getContentPane(), BoxLayout.PAGE_AXIS));

        final JPanel mainPanel = new JPanel(new GridBagLayout());
        mainPanel.setForeground(Color.black);

        final JLabel fileLabel = new JLabel("Input Imaris filament");
        fileLabel.setFont(serif12B);

        textField = new JTextField(30);
        textField.setFont(serif12);

        final JButton browseButton = new JButton("Browse");
        browseButton.setFont(serif12);
        browseButton.addActionListener(this);

        // final JLabel imLabel = new JLabel("Input Imaris image");
        // imLabel.setFont(serif12B);
        //
        // imageField = new JTextField(30);
        // imageField.setFont(serif12);
        //
        // final JButton browseImage = new JButton("Browse");
        // browseImage.setFont(serif12);
        // browseImage.setActionCommand("BrowseImage");
        // browseImage.addActionListener(this);

        final JLabel splitLabel = new JLabel("Growth Cone Length");
        splitLabel.setFont(serif12B);

        splitField = new JTextField(7);
        splitField.setFont(serif12);
        splitField.setHorizontalAlignment(JTextField.RIGHT);

        splitBrowse = new JButton("Read from CSV");
        splitBrowse.setFont(serif12);
        splitBrowse.setActionCommand("BrowseGrowthConeLen");
        splitBrowse.addActionListener(this);

        final JPanel rbPanel = new JPanel(new GridLayout(0, 2));
        rbPanel.setForeground(Color.black);
        rbPanel.setBorder(new TitledBorder(BorderFactory.createLineBorder(Color.black), "Axon determination"));

        final ButtonGroup group = new ButtonGroup();

        axonRB = new JRadioButton("Use absolute length");
        axonRB.setFont(serif12);
        axonRB.setActionCommand("notDensity_Length");
        axonRB.addActionListener(this);
        axonRB.setSelected(true);
        group.add(axonRB);

        final JRadioButton imarisRB = new JRadioButton("Infer from file");
        imarisRB.setFont(serif12);
        imarisRB.setActionCommand("notDensity_File");
        imarisRB.addActionListener(this);
        group.add(imarisRB);

        customRB = new JRadioButton("Choose filament");
        customRB.setFont(serif12);
        customRB.setActionCommand("notDensity_Choose");
        customRB.addActionListener(this);
        group.add(customRB);

        densityRB = new JRadioButton("Branch Density");
        densityRB.setFont(serif12);
        densityRB.setActionCommand("density");
        densityRB.addActionListener(this);
        group.add(densityRB);

        rbPanel.add(axonRB);
        rbPanel.add(imarisRB);
        rbPanel.add(customRB);
        rbPanel.add(densityRB);

        final JLabel resLabel = new JLabel("SWC Resolution Units");
        resLabel.setFont(serif12);

        final Unit[] allSame = UnitType.getUnitsOfType(UnitType.LENGTH);
        final int[] allSameMeasure = new int[allSame.length];
        for (int i = 0; i < allSameMeasure.length; i++) {
            allSameMeasure[i] = allSame[i].getLegacyNum();
        }
        final String[] unitArr = new String[allSameMeasure.length];
        for (int i = 0; i < allSameMeasure.length; i++) {
            final Unit unit = Unit.getUnitFromLegacyNum(allSameMeasure[i]);
            unitArr[i] = unit.getAbbrev();
        }

        resolutionUnits = new JComboBox(unitArr);
        resolutionUnits.setSelectedItem("um");
        resolutionUnits.setFont(serif12);

        final GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 0;
        gbc.weighty = 0;
        gbc.insets = new Insets(5, 5, 5, 5);
        gbc.anchor = GridBagConstraints.WEST;
        gbc.fill = GridBagConstraints.HORIZONTAL;

        mainPanel.add(fileLabel, gbc);

        gbc.gridy = 1;
        gbc.weightx = 1;
        gbc.gridwidth = 2;

        mainPanel.add(textField, gbc);

        gbc.gridx = 2;
        gbc.weightx = 0;
        gbc.gridwidth = 1;

        mainPanel.add(browseButton, gbc);

        // gbc.gridx = 0;
        // gbc.gridy++;
        //
        // mainPanel.add(imLabel, gbc);
        //
        // gbc.gridy++;
        // gbc.weightx = 1;
        // gbc.gridwidth = 2;
        //
        // mainPanel.add(imageField, gbc);
        //
        // gbc.gridx = 2;
        // gbc.weightx = 0;
        // gbc.gridwidth = 1;
        //
        // mainPanel.add(browseImage, gbc);

        gbc.gridx = 0;
        gbc.gridy++;

        mainPanel.add(splitLabel, gbc);

        gbc.gridx = 1;
        gbc.fill = GridBagConstraints.NONE;
        mainPanel.add(splitField, gbc);

        gbc.gridx = 2;
        gbc.fill = GridBagConstraints.NONE;
        mainPanel.add(splitBrowse, gbc);

        gbc.gridx = 0;
        gbc.gridy++;
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        mainPanel.add(rbPanel, gbc);

        getContentPane().add(mainPanel);

        final JPanel resPanel = new JPanel();
        resPanel.setForeground(Color.black);

        resPanel.add(resLabel);
        resPanel.add(resolutionUnits);

        getContentPane().add(resPanel);

        buildOKCancelButtons();

        // OKButton.addActionListener(this);
        OKButton.setActionCommand("ok");
        // cancelButton.addActionListener(this);
        cancelButton.setActionCommand("cancel");

        final JPanel buttonPanel = new JPanel();
        buttonPanel.setForeground(Color.black);
        buttonPanel.add(OKButton, BorderLayout.WEST);
        buttonPanel.add(cancelButton, BorderLayout.EAST);

        getContentPane().add(buttonPanel);

        final JPanel debugPanel = new JPanel();
        debugPanel.setLayout(new BoxLayout(debugPanel, BoxLayout.PAGE_AXIS));
        ;
        debugPanel.setForeground(Color.black);
        debugPanel.setBorder(new TitledBorder(BorderFactory.createEmptyBorder(), "Debugging Output"));

        final JPanel textPanel = new JPanel(new BorderLayout());
        textArea = new JTextPane();
        textPanel.add(textArea, BorderLayout.CENTER);
        final JScrollPane scrollPane = new JScrollPane(textPanel);
        scrollPane.setPreferredSize(new Dimension(100, 200));
        scrollPane.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);
        scrollPane.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        debugPanel.add(scrollPane);

        getContentPane().add(debugPanel);

        pack();
        setVisible(true);
        System.gc();
    }

    private void chooseDir() {
        final String dirText = Preferences.getImageDirectory();

        final FileFilter ivFilter = new FileFilter() {

            @Override
            public boolean accept(final File pathname) {
                if (pathname.isDirectory()) {
                    return true;
                }
                final String name = pathname.getName();
                final int index = name.lastIndexOf(".");
                if (index < 0) {
                    return false;
                }
                final String fileExt = name.substring(index);
                if (fileExt.equalsIgnoreCase(".iv")) {
                    return true;
                } else {
                    return false;
                }
            }

            @Override
            public String getDescription() {
                return "Imaris Filaments (.iv)";
            }

        };

        final FileFilter filter = ivFilter;

        ivFileChooser = new JFileChooser(dirText);
        ivFileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
        ivFileChooser.addActionListener(this);
        ivFileChooser.addChoosableFileFilter(filter);
        ivFileChooser.setFileFilter(filter);
        ivFileChooser.showOpenDialog(this);
    }

    private void chooseCSVFile() {
        final String dirText = Preferences.getImageDirectory();

        final FileFilter csvFilter = new FileFilter() {

            @Override
            public boolean accept(final File pathname) {
                if (pathname.isDirectory()) {
                    return true;
                }
                final String name = pathname.getName();
                final int index = name.lastIndexOf(".");
                if (index < 0) {
                    return false;
                }
                final String fileExt = name.substring(index);
                if (fileExt.equalsIgnoreCase(".csv")) {
                    return true;
                } else {
                    return false;
                }
            }

            @Override
            public String getDescription() {
                return "Comma separated value files (.csv)";
            }

        };

        final FileFilter filter = csvFilter;

        csvFileChooser = new JFileChooser(dirText);
        csvFileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
        csvFileChooser.addActionListener(this);
        csvFileChooser.addChoosableFileFilter(filter);
        csvFileChooser.setFileFilter(filter);
        csvFileChooser.showOpenDialog(this);
    }

    private float[] readGrowthConeLenCSV(final File csvFile) {
        final ArrayList<Float> gcLenList = new ArrayList<Float>();

        Iterable<CSVRecord> records;

        try {
            final Reader in = new FileReader(csvFile);
            records = CSVFormat.EXCEL.parse(in);
        } catch (final FileNotFoundException e) {
            MipavUtil.displayError("File not found: " + csvFile);
            return null;
        } catch (final IOException e) {
            MipavUtil.displayError("Error reading file: " + csvFile);
            return null;
        }

        for (final CSVRecord record : records) {
            try {
                gcLenList.add(Float.valueOf(record.get(0)));
            } catch (final NumberFormatException e) {
                MipavUtil.displayError("Error parsing growth cone length on CSV line " + record.getRecordNumber());
                return null;
            }
        }

        final float[] arr = new float[gcLenList.size()];
        for (int i = 0; i < gcLenList.size(); i++) {
            arr[i] = gcLenList.get(i).floatValue();
        }
        return arr;
    }

    /**
     * Reads surface file. Taken from drosophila registration dialog written by Nish Pandya.
     * 
     * @param surfaceFile
     * @return
     */
    private boolean readSurfaceFile(final File surfaceFile) {
        final boolean success = true;
        RandomAccessFile raFile = null;
        ArrayList<ArrayList<float[]>> curTimeCoordinates = null;

        allTimeCoordinates = new ArrayList<ArrayList<ArrayList<float[]>>>();
        try {

            raFile = new RandomAccessFile(surfaceFile, "r");

            String line;

            while ( (line = raFile.readLine()) != null) {
                line = line.trim();
                if (line.startsWith("Translate1Dragger")) {
                    break;
                }
                if (line.contains("Coordinate3")) {
                    final ArrayList<float[]> filamentCoords = new ArrayList<float[]>();
                    while ( ! ( (line = raFile.readLine()).endsWith("}"))) {
                        line = line.trim();
                        if ( !line.equals("")) {
                            if (line.startsWith("point [")) {
                                line = line.substring(line.indexOf("point [") + 7, line.length()).trim();
                                if (line.equals("")) {
                                    continue;
                                }
                            }
                            if (line.endsWith("]")) {
                                line = line.substring(0, line.indexOf("]")).trim();
                                if (line.equals("")) {
                                    continue;
                                }
                            }
                            if (line.endsWith(",")) {
                                line = line.substring(0, line.indexOf(",")).trim();
                                if (line.equals("")) {
                                    continue;
                                }
                            }
                            final String[] splits = line.split("\\s+");
                            splits[0] = splits[0].trim();
                            splits[1] = splits[1].trim();
                            splits[2] = splits[2].trim();
                            final float coord_x = new Float(splits[0]).floatValue();
                            final float coord_y = new Float(splits[1]).floatValue();
                            final float coord_z = new Float(splits[2]).floatValue();

                            /**
                             * Changing from previous versions. Order is now: X, Y, Z coordinates (0, 1, 2) Distance (3)
                             * Backwards connection (4) Branch order (5) Radius (6) Include in Volume Calculations
                             * (1=Include, -1=Exclude) (7)
                             */
                            final float[] coords = {coord_x, coord_y, coord_z, 0, Float.NEGATIVE_INFINITY, 0f, -1.0f, -1.0f};

                            filamentCoords.add(coords);
                        }
                    }
                    curTimeCoordinates.add(filamentCoords);
                } else if (line.startsWith("Group {")) {
                    if (curTimeCoordinates != null) {
                        allTimeCoordinates.add(curTimeCoordinates);
                    }
                    curTimeCoordinates = new ArrayList<ArrayList<float[]>>();
                }
            }
            raFile.close();
        } catch (final Exception e) {
            try {
                if (raFile != null) {
                    raFile.close();
                }
            } catch (final Exception ex) {

            }
            e.printStackTrace();
            return false;
        }

        return success;
    }

    /**
     * Used to write messages with certain fonts to the text area that can be used to monitor progress and errors in
     * lieu of using the java console.
     * 
     * @param message
     * @param a
     */
    private void append(final String message, final AttributeSet a) {
        final Document doc = textArea.getDocument();
        try {
            doc.insertString(doc.getLength(), message + "\n", a);
        } catch (final BadLocationException e) {
            e.printStackTrace();
        }

        textArea.setCaretPosition(doc.getLength());
    }
}
