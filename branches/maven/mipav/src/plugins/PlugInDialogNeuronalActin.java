import gov.nih.mipav.plugins.JDialogStandalonePlugin;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.structures.ModelImage;

import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewUserInterface;

import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.WindowEvent;
import java.io.File;
import java.io.FilenameFilter;
import java.util.Arrays;
import java.util.Comparator;
import java.util.Vector;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.swing.*;
import javax.swing.border.TitledBorder;
import javax.swing.filechooser.FileFilter;
import javax.swing.text.*;


public class PlugInDialogNeuronalActin extends JDialogStandalonePlugin implements AlgorithmInterface {
    private static final long serialVersionUID = 7054211453483441130L;

    private JTextField swcField;

    private Vector<File> swcFileList;

    private JTextField imageField;

    private JTextPane textArea;

    private JTextField actinField;

    private int channel = -1;
    
    private JTextField sensitivityField;
    
    private float sensitivity = 0.01f;

    private final String[] acceptedImTypes = new String[] {".ics"};

    private ModelImage srcImage;

    private int curTimePt;

    public static final SimpleAttributeSet BLACK_TEXT;

    public static final SimpleAttributeSet BLACK_BOLD_TEXT;

    public static final SimpleAttributeSet GREEN_TEXT;

    public static final SimpleAttributeSet RED_TEXT;

    private static final String LAST_SWC_FILE_PREF = "PlugInNeuronalActinStats_LastSWCFile";

    private static final String LAST_ACTIN_FILE_PREF = "PlugInNeuronalActinStats_LastActinFile";
    
    private static final String THRESHOLD_SENSITIVITY = "PlugInNeuronalActinStats_ThresholdSensitivity";

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

    public PlugInDialogNeuronalActin() {
        super();

        init();

        final String version = "$Rev$";
        final String lastUpdate = "$Date$";

        append("Version:\t" + MipavUtil.getSVNRevisionNum(version), BLACK_TEXT);
        append("Date:\t" + MipavUtil.getSVNChangedDate(lastUpdate), BLACK_TEXT);
        append("-----------------------------------------", BLACK_TEXT);
    }

    @Override
    public void actionPerformed(final ActionEvent e) {
        final String command = e.getActionCommand();
        if (command.equals("ok")) {
            callAlgorithm();
        } else if (command.equals("BrowseSWC")) {
            chooseSWC();
        } else if (command.equals("BrowseImage")) {
            chooseICS();
        } else if (command.equals("cancel")) {
            if (isExitRequired()) {
                ViewUserInterface.getReference().windowClosing(new WindowEvent(this, WindowEvent.WINDOW_CLOSING));
            } else {
                dispose();
            }
        }
    }

    @Override
    public void algorithmPerformed(final AlgorithmBase algorithm) {
        if (algorithm.isCompleted()) {
            append("-----------------------------------------", BLACK_TEXT);

            // move on to next time point, if there is one
            curTimePt++;
            if (curTimePt < swcFileList.size()) {
                final PlugInAlgorithmNeuronalActin alg = new PlugInAlgorithmNeuronalActin(srcImage, curTimePt, swcFileList.get(curTimePt).getAbsolutePath(),
                        textArea);
                alg.addListener(this);
                if (channel != -1) {
                    alg.setActinChannel(channel);
                }
                alg.setSensitivity(sensitivity);

                if (isRunInSeparateThread()) {
                    if (alg.startMethod(Thread.MIN_PRIORITY) == false) {
                        append("A thread is already running on this object", RED_TEXT);
                    }
                } else {
                    alg.run();
                }
            }
        }
    }

    protected void callAlgorithm() {
        final String swcFile = swcField.getText();
        final String imgFile = imageField.getText();

        final File chosenSWCPath = new File(swcFile);
        if ( ! (chosenSWCPath.exists())) {
            append("The provided SWC file does not exist.", RED_TEXT);
            return;
        }

        swcFileList = new Vector<File>();
        if (chosenSWCPath.isFile()) {
            swcFileList.add(chosenSWCPath);
        } else if (chosenSWCPath.isDirectory()) {
            final FilenameFilter swcFilter = new FilenameFilter() {

                @Override
                public boolean accept(final File dir, final String name) {
                    if ( (new File(dir.getPath() + File.separator + name)).isDirectory()) {
                        return false;
                    }

                    // skip hidden files created on Akanni's Mac
                    if (name.startsWith("._")) {
                        return false;
                    }

                    return name.toLowerCase().endsWith(".swc");
                }
            };

            final File[] files = chosenSWCPath.listFiles(swcFilter);

            // Sort list into correct order
            Arrays.sort(files, new Comparator<File>() {

                @Override
                public int compare(final File o1, final File o2) {
                    final String s1 = o1.getName();
                    final String s2 = o2.getName();

                    final String s1NoNum = s1.replaceAll("[0-9]", "");
                    final String s2NoNum = s2.replaceAll("[0-9]", "");

                    final int compare = s1NoNum.compareTo(s2NoNum);

                    if (compare == 0) {
                        // Without numbers, the two are the same
                        String s1Num = "";
                        String s2Num = "";
                        
                        final Pattern p = Pattern.compile("(\\d+)\\.\\w+$");
                        Matcher m = p.matcher(s1);
                        if (m.find()) {
                            s1Num = m.group(1);
                        }
                        m = p.matcher(s2);
                        if (m.find()) {
                            s2Num = m.group(1);
                        }

                        // Compare the left over numbers
                        final int s1Int = Integer.valueOf(s1Num);
                        final int s2Int = Integer.valueOf(s2Num);

                        return Integer.valueOf(s1Int).compareTo(Integer.valueOf(s2Int));
                    } else {
                        return compare;
                    }
                }
            });

            for (final File f : files) {
                swcFileList.add(f);
            }
        }

        if ( ! (new File(imgFile).exists())) {
            append("The provided image file does not exist.", RED_TEXT);
            return;
        }

        if (actinField.isEnabled()) {
            try {
                channel = Integer.valueOf(actinField.getText());
            } catch (final NumberFormatException e) {
                e.printStackTrace();
                append("The input channel is not an integer", RED_TEXT);
                return;
            }
            if (channel < 1 && channel >= 4) {
                append("The input channel is not between 1 and 3, inclusive", RED_TEXT);
                return;
            }
        }
        
        try {
            sensitivity = Float.valueOf(sensitivityField.getText());
            Preferences.setProperty(THRESHOLD_SENSITIVITY, "" + sensitivity);
        } catch (final NumberFormatException e) {
            e.printStackTrace();
            append("The threshold sensitivity is not a floating point number", RED_TEXT);
            return;
        }

        srcImage = readImage(imgFile);
        if (srcImage.is4DImage()) {
            if (swcFileList.size() != srcImage.getExtents()[3]) {
                MipavUtil.displayError("The number of image timepoints doesn't match the number of SWC files provided.");
                return;
            }
        }

        curTimePt = 0;
        final PlugInAlgorithmNeuronalActin alg = new PlugInAlgorithmNeuronalActin(srcImage, curTimePt, swcFileList.get(curTimePt).getAbsolutePath(), textArea);
        alg.addListener(this);
        if (channel != -1) {
            alg.setActinChannel(channel);
        }
        alg.setSensitivity(sensitivity);

        if (isRunInSeparateThread()) {
            if (alg.startMethod(Thread.MIN_PRIORITY) == false) {
                append("A thread is already running on this object", RED_TEXT);
            }
        } else {
            alg.run();
        }
    }

    private void append(final String message, final AttributeSet a) {
        final Document doc = textArea.getDocument();
        try {
            doc.insertString(doc.getLength(), message + "\n", a);
        } catch (final BadLocationException e) {
            e.printStackTrace();
        }

        textArea.setCaretPosition(doc.getLength());
    }

    private void chooseICS() {
        final String dirText = Preferences.getImageDirectory();

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

                for (int i = 0; i < acceptedImTypes.length; i++) {
                    if (fileExt.equalsIgnoreCase(acceptedImTypes[i])) {
                        return true;
                    }
                }

                return false;
            }

            @Override
            public String getDescription() {
                return "Imaris Image (.ics)";
            }
        };

        final JFileChooser fileChooser = new JFileChooser(dirText);
        fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
        fileChooser.addChoosableFileFilter(imFilter);
        fileChooser.setFileFilter(imFilter);
        final int returnVal = fileChooser.showOpenDialog(this);

        if (returnVal == JFileChooser.APPROVE_OPTION) {
            final String selection = fileChooser.getSelectedFile().getAbsolutePath();
            imageField.setText(selection);
            // Preferences.setImageDirectory(new File(selection));
            Preferences.setProperty(LAST_ACTIN_FILE_PREF, selection);
        }
    }

    private void chooseSWC() {
        final String dirText = Preferences.getImageDirectory();

        final FileFilter swcFilter = new FileFilter() {

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
                if (fileExt.equalsIgnoreCase(".swc")) {
                    return true;
                } else {
                    return false;
                }
            }

            @Override
            public String getDescription() {
                return "SWC File (.swc)";
            }

        };

        final JFileChooser fileChooser = new JFileChooser(dirText);
        fileChooser.setFileSelectionMode(JFileChooser.FILES_AND_DIRECTORIES);
        fileChooser.addChoosableFileFilter(swcFilter);
        fileChooser.setFileFilter(swcFilter);
        final int returnVal = fileChooser.showOpenDialog(this);

        if (returnVal == JFileChooser.APPROVE_OPTION) {
            final String selection = fileChooser.getSelectedFile().getAbsolutePath();
            swcField.setText(selection);
            // Preferences.setImageDirectory(new File(selection));
            Preferences.setProperty(LAST_SWC_FILE_PREF, selection);
        }
    }

    private void init() {

        setTitle("Neuronal Actin Stats");

        getContentPane().removeAll();
        setLayout(new BoxLayout(getContentPane(), BoxLayout.Y_AXIS));

        final JPanel mainPanel = new JPanel(new GridBagLayout());
        mainPanel.setForeground(Color.black);

        final JLabel fileLabel = new JLabel("Input SWC File or Directory");
        fileLabel.setFont(serif12B);

        swcField = new JTextField(30);
        swcField.setFont(serif12);

        final String swcFileDefault = Preferences.getProperty(LAST_SWC_FILE_PREF);
        if (swcFileDefault != null) {
            swcField.setText(swcFileDefault);
        }

        final JButton browseButton = new JButton("Browse");
        browseButton.setFont(serif12);
        browseButton.setActionCommand("BrowseSWC");
        browseButton.addActionListener(this);

        final JLabel imLabel = new JLabel("Input Actin Image");
        imLabel.setFont(serif12B);

        imageField = new JTextField(30);
        imageField.setFont(serif12);

        final String imageFileDefault = Preferences.getProperty(LAST_ACTIN_FILE_PREF);
        if (imageFileDefault != null) {
            imageField.setText(imageFileDefault);
        }

        final JButton browseImage = new JButton("Browse");
        browseImage.setFont(serif12);
        browseImage.setActionCommand("BrowseImage");
        browseImage.addActionListener(this);

        final JLabel actinLabel = new JLabel("Actin Channel (1-3)");
        actinLabel.setFont(serif12B);

        actinField = new JTextField(3);
        actinField.setFont(serif12);
        actinField.setText("1");
        actinField.setHorizontalAlignment(JTextField.RIGHT);
        
        final JLabel sensitivityLabel = new JLabel("Threshold sensitivity (0.01 default; 0 for no thresholding)");
        sensitivityLabel.setFont(serif12B);

        if (Preferences.isPreferenceSet(THRESHOLD_SENSITIVITY)) {
            try {
                sensitivity = Float.parseFloat(Preferences.getProperty(THRESHOLD_SENSITIVITY));
            } catch (final NumberFormatException e) {
                e.printStackTrace();
                append("The threshold sensitivity preference is not a floating point number. Defaulting to 0.01.", RED_TEXT);
                sensitivity = 0.01f;
                return;
            }
        }
        
        sensitivityField = new JTextField(5);
        sensitivityField.setFont(serif12);
        sensitivityField.setText("" + sensitivity);
        sensitivityField.setHorizontalAlignment(JTextField.RIGHT);

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

        mainPanel.add(swcField, gbc);

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

        mainPanel.add(actinLabel, gbc);

        gbc.gridx = 1;
        gbc.fill = GridBagConstraints.NONE;
        mainPanel.add(actinField, gbc);
        
        gbc.gridx = 0;
        gbc.gridy++;

        mainPanel.add(sensitivityLabel, gbc);

        gbc.gridx = 1;
        gbc.fill = GridBagConstraints.NONE;
        mainPanel.add(sensitivityField, gbc);

        getContentPane().add(mainPanel);

        buildOKCancelButtons();

        OKButton.setActionCommand("ok");
        cancelButton.setActionCommand("cancel");

        final JPanel buttonPanel = new JPanel();
        buttonPanel.setForeground(Color.black);
        buttonPanel.add(OKButton, BorderLayout.WEST);
        buttonPanel.add(cancelButton, BorderLayout.EAST);

        getContentPane().add(buttonPanel);

        final JPanel debugPanel = new JPanel();
        debugPanel.setLayout(new BoxLayout(debugPanel, BoxLayout.PAGE_AXIS));
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

    private ModelImage readImage(final String file) {
        final ModelImage img;
        final FileIO reader = new FileIO();
        try {
            append("Opening image...", PlugInDialogNeuronalActin.BLACK_TEXT);
            img = reader.readImage(file);
        } catch (final Exception e) {
            e.printStackTrace();
            append("Could not open image file", PlugInDialogNeuronalActin.RED_TEXT);
            return null;
        }

        return img;
    }

}
