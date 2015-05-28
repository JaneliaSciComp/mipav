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
import java.io.File;
import java.io.FileWriter;
import java.io.FilenameFilter;
import java.io.IOException;
import java.util.Arrays;
import java.util.Comparator;

import javax.swing.*;
import javax.swing.border.TitledBorder;
import javax.swing.filechooser.FileFilter;
import javax.swing.text.BadLocationException;
import javax.swing.text.Document;
import javax.swing.text.SimpleAttributeSet;
import javax.swing.text.StyleConstants;


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

    private JTextField imageField;

    private JFileChooser fileChooser;

    @SuppressWarnings("rawtypes")
    private JComboBox resolutionUnits;

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

    private JTextField splitField;

    public PlugInDialog3DSWCStats() {
        super();

        writeStep = false;
        locked = false;

        init();

        final SimpleAttributeSet blackText = new SimpleAttributeSet();
        StyleConstants.setFontFamily(blackText, "Serif");
        StyleConstants.setFontSize(blackText, 12);

        final String version = "1.5.1";
        final String lastUpdate = "4/20/15";

        final String message = "Initializing v " + version + "\n" + "Last updated: " + lastUpdate + "\n" + "-----------------------------------------";

        final Document doc = textArea.getDocument();
        try {
            doc.insertString(doc.getLength(), message + "\n", blackText);
        } catch (final BadLocationException ex) {
            ex.printStackTrace();
        }
        textArea.setCaretPosition(doc.getLength());
    }

    @Override
    public void actionPerformed(final ActionEvent e) {
        final String command = e.getActionCommand();
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
        } else if (command.equals(JFileChooser.APPROVE_SELECTION)) {
            final File selected = fileChooser.getSelectedFile();
            final String name = selected.getAbsolutePath();
            Preferences.setImageDirectory(selected);
            if (chooseIV) {
                textField.setText(name);
            } else {
                imageField.setText(name);
            }
        } else if (command.equals("Browse")) {
            chooseIV = true;
            chooseDir();
        } else if (command.equals("BrowseImage") && !densityRB.isSelected()) {
            chooseIV = false;
            chooseDir();
        } else if (command.equals("density")) {
            imageField.setEditable(false);
            splitField.setEditable(false);
        } else if (command.startsWith("notDensity")) {
            imageField.setEditable(true);
            splitField.setEditable(true);
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
                    if (densityCount < densityFiles.length) {
                        alg = new PlugInAlgorithm3DSWCViewer(densityFiles[densityCount], textArea, (String) resolutionUnits.getSelectedItem());
                        alg.addListener(this);
                        new PlugInDialog3DSWCViewer(textArea, (String) resolutionUnits.getSelectedItem(), alg);
                        if (isRunInSeparateThread()) {
                            if (alg.startMethod(Thread.MIN_PRIORITY) == false) {
                                MipavUtil.displayError("A thread is already running on this object");
                            }
                        } else {
                            alg.run();
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
                final SimpleAttributeSet redText = new SimpleAttributeSet();
                StyleConstants.setFontFamily(redText, "Serif");
                StyleConstants.setFontSize(redText, 12);
                StyleConstants.setForeground(redText, Color.red.darker());
                final Document doc = textArea.getDocument();
                try {
                    doc.insertString(doc.getLength(), message + "\n", redText);
                } catch (final BadLocationException ex) {
                    ex.printStackTrace();
                }
                textArea.setCaretPosition(doc.getLength());
                locked = false;
                return;
            }

            densityFiles = list;
            densityCount = 0;

            alg = new PlugInAlgorithm3DSWCViewer(list[densityCount], textArea, (String) resolutionUnits.getSelectedItem());
            alg.addListener(this);
            new PlugInDialog3DSWCViewer(textArea, (String) resolutionUnits.getSelectedItem(), alg);

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

            final String imageStr = imageField.getText();

            if (imageStr.length() == 0) {
                MipavUtil.displayError("Please input an image");
                locked = false;
                return;
            }

            final File imageFile = new File(imageStr);
            if ( !imageFile.exists()) {
                MipavUtil.displayError("This image does not exist");
                locked = false;
                return;
            }

            float splitDist;

            try {
                splitDist = Float.valueOf(splitField.getText());
            } catch (final NumberFormatException e) {
                MipavUtil.displayError("Enter the length of the growth cone");
                locked = false;
                return;
            }

            // Open the viewer if the a custom axon is to be chosen
            if (customRB.isSelected()) {
                alg = new PlugInAlgorithm3DSWCViewer(imageField.getText(), file, textArea, (String) resolutionUnits.getSelectedItem(), false, true);
                new PlugInDialog3DSWCViewer(textArea, (String) resolutionUnits.getSelectedItem(), alg);
            } else {
                alg = new PlugInAlgorithm3DSWCViewer(imageField.getText(), file, textArea, (String) resolutionUnits.getSelectedItem(), axonRB.isSelected(),
                        false);
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

        final JLabel imLabel = new JLabel("Input Imaris image");
        imLabel.setFont(serif12B);

        imageField = new JTextField(30);
        imageField.setFont(serif12);

        final JButton browseImage = new JButton("Browse");
        browseImage.setFont(serif12);
        browseImage.setActionCommand("BrowseImage");
        browseImage.addActionListener(this);

        final JLabel splitLabel = new JLabel("Growth Cone Length");
        splitLabel.setFont(serif12B);

        splitField = new JTextField(7);
        splitField.setFont(serif12);
        splitField.setHorizontalAlignment(JTextField.RIGHT);

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

        gbc.gridx = 0;
        gbc.gridy++;

        mainPanel.add(imLabel, gbc);

        gbc.gridy++;
        gbc.weightx = 1;
        gbc.gridwidth = 2;

        mainPanel.add(imageField, gbc);

        gbc.gridx = 2;
        gbc.weightx = 0;
        gbc.gridwidth = 1;

        mainPanel.add(browseImage, gbc);

        gbc.gridx = 0;
        gbc.gridy++;

        mainPanel.add(splitLabel, gbc);

        gbc.gridx = 1;
        gbc.fill = GridBagConstraints.NONE;
        mainPanel.add(splitField, gbc);

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

        final FileFilter imFilter = new FileFilter() {
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
                if (fileExt.equalsIgnoreCase(".ics")) {
                    return true;
                } else {
                    return false;
                }
            }

            @Override
            public String getDescription() {
                return "Imaris Image (.ics)";
            }
        };

        final FileFilter filter = chooseIV ? ivFilter : imFilter;

        fileChooser = new JFileChooser(dirText);
        fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
        fileChooser.addActionListener(this);
        fileChooser.addChoosableFileFilter(filter);
        fileChooser.setFileFilter(filter);
        fileChooser.showOpenDialog(this);
    }

}
