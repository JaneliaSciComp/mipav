import gov.nih.mipav.plugins.JDialogStandalonePlugin;

import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.components.WidgetFactory;
import gov.nih.mipav.view.dialogs.JDialogBase;

import java.io.IOException;
import java.io.InputStream;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.Properties;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.awt.*;
import java.awt.event.*;
import java.io.File;

import javax.swing.*;


public class PlugInDialogStrokeSegmentationListenerPWI extends JFrame implements ActionListener, WindowListener {
    private JTextField aeField;
    private JTextField portField;
    private JTextField outputDirField;
    private JTextField reportDirField;
//    private JTextField emailField;
    private JTextField minExpectedSlicesField;
    private JTextField minExpectedSlicesPWIField;
    private JTextField maxWaitTimeField;
    
    private JCheckBox emailCheckbox;
    
    private WidgetFactory.ScrollTextArea logOutputArea;
    
    private String ipAddress;
    
    private String ae = "MIPAV-stroke";
    
    private int port = 11115;

    private String outputDir = new String(System.getProperty("user.home") + File.separator + "mipav" + File.separator + "dicom_catcher" + File.separator);
    
    private String reportDir = outputDir;
    
    private boolean doEmailReport = false;
    
    private boolean doAutoStart = false;
    
    private int minExpectedSlices = 30;
    
    private int[] minExpectedSlicesPWI = new int[] {1600, 2400, 2640};
    
    private int maxWaitTime = 15;
    
    private StrokeSegmentationDicomReceiverPWI dicomReceiver;
    
    private static final String svnVersion = "$Rev: 15722 $";

    private static final String svnLastUpdate = "$Date: 2018-11-07 16:19:05 -0500 (Wed, 07 Nov 2018) $";
    
    private static final String pluginVersion = MipavUtil.getSVNChangedDate(svnLastUpdate);
    
    private static final String configFileName = "stroke_seg_listener.properties";

    public PlugInDialogStrokeSegmentationListenerPWI() {
        if (JDialogStandalonePlugin.isExitRequired()) {
            setDefaultCloseOperation(JDialog.DO_NOTHING_ON_CLOSE);
        } else {
            setDefaultCloseOperation(JDialog.DISPOSE_ON_CLOSE);
        }

        try {
            setIconImage(MipavUtil.getIconImage(Preferences.getIconName()));
        } catch (final Exception e) {
            // setIconImage() is not part of the Java 1.5 API - catch any
            // runtime error on those systems
        }
        
        readListenerConfig();
        
        init();
        
        if (doAutoStart) {
            if (setVariables()) {
                callAlgorithm();
            }
        }
    }
    
    private void init() {
        setForeground(Color.black);
        setTitle("Stroke DICOM receiver - IP: " + ipAddress + " - " + pluginVersion);
        
        setTitle("Stroke Segmentation " + pluginVersion);

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.insets = new Insets(3, 3, 3, 3);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 0;
        gbc.gridy = 0;

        JPanel mainPanel = new JPanel(new GridBagLayout());
        mainPanel.setForeground(Color.black);
        mainPanel.setBorder(JDialogBase.buildTitledBorder("DICOM receiver parameters"));
        
        JLabel labelAE = new JLabel("AE title");
        labelAE.setForeground(Color.black);
        labelAE.setFont(MipavUtil.font12);
        mainPanel.add(labelAE, gbc);
        
        aeField = new JTextField(20);
        aeField.setText("" + ae);
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridx++;
        mainPanel.add(aeField, gbc);
        
        gbc.fill = GridBagConstraints.HORIZONTAL;
        
        gbc.gridy++;
        gbc.gridx = 0;
        
        JLabel labelPort = new JLabel("Port");
        labelPort.setForeground(Color.black);
        labelPort.setFont(MipavUtil.font12);
        mainPanel.add(labelPort, gbc);
        
        portField = new JTextField(20);
        portField.setText("" + port);
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridx++;
        mainPanel.add(portField, gbc);
        
        gbc.fill = GridBagConstraints.HORIZONTAL;
        
        gbc.gridy++;
        gbc.gridx = 0;
        
        JLabel labelOutput = new JLabel("Output dir");
        labelOutput.setForeground(Color.black);
        labelOutput.setFont(MipavUtil.font12);
        mainPanel.add(labelOutput, gbc);
        
        outputDirField = new JTextField(40);
        outputDirField.setText("" + outputDir);
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridx++;
        mainPanel.add(outputDirField, gbc);
        
        JButton dirFileButton = new JButton("Browse");
        dirFileButton.setActionCommand("BrowseDir");
        dirFileButton.addActionListener(this);
        dirFileButton.setForeground(Color.black);
        dirFileButton.setFont(MipavUtil.font12B);
        gbc.gridx++;
        mainPanel.add(dirFileButton, gbc);
        
        gbc.gridy++;
        gbc.gridx = 0;
        
        JLabel labelReport = new JLabel("HTML report dir");
        labelReport.setForeground(Color.black);
        labelReport.setFont(MipavUtil.font12);
        mainPanel.add(labelReport, gbc);
        
        reportDirField = new JTextField(40);
        reportDirField.setText("" + reportDir);
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridx++;
        mainPanel.add(reportDirField, gbc);
        
        JButton reportFileButton = new JButton("Browse");
        reportFileButton.setActionCommand("BrowseDirReport");
        reportFileButton.addActionListener(this);
        reportFileButton.setForeground(Color.black);
        reportFileButton.setFont(MipavUtil.font12B);
        gbc.gridx++;
        mainPanel.add(reportFileButton, gbc);
        
        gbc.gridy++;
        gbc.gridx = 0;
        
        emailCheckbox = new JCheckBox("Email report to pre-defined address", doEmailReport);
        emailCheckbox.setForeground(Color.black);
        emailCheckbox.setFont(MipavUtil.font12);
        mainPanel.add(emailCheckbox, gbc);
        
        gbc.gridy++;
        gbc.gridx = 0;
        
        JLabel labelNumSlices = new JLabel("Minimum number of expected slices per ADC/DWI volume");
        labelNumSlices.setForeground(Color.black);
        labelNumSlices.setFont(MipavUtil.font12);
        mainPanel.add(labelNumSlices, gbc);
        
        minExpectedSlicesField = new JTextField(20);
        minExpectedSlicesField.setText("" + minExpectedSlices);
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridx++;
        mainPanel.add(minExpectedSlicesField, gbc);
        
        gbc.gridy++;
        gbc.gridx = 0;
        
        JLabel labelNumSlicesPWI = new JLabel("Number of expected slices per PWI volume");
        labelNumSlicesPWI.setForeground(Color.black);
        labelNumSlicesPWI.setFont(MipavUtil.font12);
        mainPanel.add(labelNumSlicesPWI, gbc);
        
        minExpectedSlicesPWIField = new JTextField(20);
        minExpectedSlicesPWIField.setText(intArrayToStr(minExpectedSlicesPWI));
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridx++;
        mainPanel.add(minExpectedSlicesPWIField, gbc);
        
        gbc.gridy++;
        gbc.gridx = 0;
        
        JLabel labelMaxWait = new JLabel("Maximum number of minutes to wait for PWI data before processing only ADC/DWI");
        labelMaxWait.setForeground(Color.black);
        labelMaxWait.setFont(MipavUtil.font12);
        mainPanel.add(labelMaxWait, gbc);
        
        maxWaitTimeField = new JTextField(20);
        maxWaitTimeField.setText("" + maxWaitTime);
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridx++;
        mainPanel.add(maxWaitTimeField, gbc);
        
//        
//        JLabel labelEmail = new JLabel("Send email report to");
//        labelEmail.setForeground(Color.black);
//        labelEmail.setFont(MipavUtil.font12);
//        mainPanel.add(labelEmail, gbc);
        
//        emailField = new JTextField(40);
//        emailField.setText("" + emailAddress);
//        gbc.fill = GridBagConstraints.NONE;
//        gbc.gridx++;
//        mainPanel.add(emailField, gbc);
  
        getContentPane().add(mainPanel, BorderLayout.NORTH);
        
        logOutputArea = WidgetFactory.buildScrollTextArea(Color.white);
        logOutputArea.setBorder(JDialogBase.buildTitledBorder("DICOM receiver log"));
        logOutputArea.getTextArea().setEditable(false);
        logOutputArea.getTextArea().setRows(10);
        
        getContentPane().add(logOutputArea, BorderLayout.CENTER);

        JPanel buttonPanel = new JPanel(new GridLayout());
        
        JButton startButton = new JButton("Start");
        startButton.setActionCommand("Start");
        startButton.addActionListener(this);
        startButton.setForeground(Color.black);
        startButton.setFont(MipavUtil.font12B);
        buttonPanel.add(startButton);
        
        JButton stopButton = new JButton("Stop");
        stopButton.setActionCommand("Stop");
        stopButton.addActionListener(this);
        stopButton.setForeground(Color.black);
        stopButton.setFont(MipavUtil.font12B);
        buttonPanel.add(stopButton);
        
        JButton exitButton = new JButton("Exit");
        exitButton.setActionCommand("Exit");
        exitButton.addActionListener(this);
        exitButton.setForeground(Color.black);
        exitButton.setFont(MipavUtil.font12B);
        buttonPanel.add(exitButton);

        getContentPane().add(buttonPanel, BorderLayout.SOUTH);

        pack();
        setResizable(true);
        System.gc();
        
        MipavUtil.centerOnScreen(this);
        setVisible(true);
    }
    
    public void actionPerformed(final ActionEvent event) {
        if (event.getActionCommand().equalsIgnoreCase("Start")) {
            if (setVariables()) {
                callAlgorithm();
            }
        } else if (event.getActionCommand().equalsIgnoreCase("Stop")) {
            stopAlgorithm();
        } else if (event.getActionCommand().equalsIgnoreCase("Exit")) {
            this.windowClosing(new WindowEvent(this, WindowEvent.WINDOW_CLOSING));
        } else if (event.getActionCommand().equalsIgnoreCase("BrowseDir")) {
            browseOutputDir();
        } else if (event.getActionCommand().equalsIgnoreCase("BrowseDirReport")) {
            browseReportDir();
        }
    }

    private boolean setVariables() {
        ae = aeField.getText();
        port = Integer.parseInt(portField.getText());
        outputDir = outputDirField.getText();
        reportDir = reportDirField.getText();
        doEmailReport = emailCheckbox.isSelected();
//        emailAddress = emailField.getText();
        minExpectedSlices = Integer.parseInt(minExpectedSlicesField.getText());
        minExpectedSlicesPWI = strToIntArray(minExpectedSlicesPWIField.getText());
        maxWaitTime = Integer.parseInt(maxWaitTimeField.getText());
        
        return true;
    }
    
    protected void callAlgorithm() {
        try {
            log("Starting DICOM receiver: " + ae + " @ " + ipAddress + ":" + port);
            
            dicomReceiver = new StrokeSegmentationDicomReceiverPWI(ipAddress, port, ae, outputDir, reportDir, minExpectedSlices, minExpectedSlicesPWI, maxWaitTime, doEmailReport, logOutputArea);
        } catch (IOException e) {
            e.printStackTrace();
            
            stopAlgorithm();
        }
    }
    
    private void stopAlgorithm() {
        if (dicomReceiver != null) {
            log("Stopping DICOM receiver: " + ae + " @ " + ipAddress + ":" + port);
            if (!dicomReceiver.shutdownReceiver()) {
                MipavUtil.displayError("Unable to shutdown DICOM receiver thread.");
            }
        }
    }
    
    public void windowClosing(WindowEvent event) {
        stopAlgorithm();
        
        if (JDialogStandalonePlugin.isExitRequired()) {
            ViewUserInterface.getReference().windowClosing(event);
        }
    }
    
    @Override
    public void windowActivated(WindowEvent arg0) {}

    @Override
    public void windowClosed(WindowEvent arg0) {}

    @Override
    public void windowDeactivated(WindowEvent arg0) {}

    @Override
    public void windowDeiconified(WindowEvent arg0) {}

    @Override
    public void windowIconified(WindowEvent arg0) {}

    @Override
    public void windowOpened(WindowEvent arg0) {}
    
    private boolean browseOutputDir() {
        String initDir = outputDirField.getText();
        if (initDir.equals("") || !(new File(initDir).exists())) {
            initDir = ViewUserInterface.getReference().getDefaultDirectory();
        }
        
        final JFileChooser chooser = new JFileChooser(initDir);

        chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
        chooser.setDialogTitle("Choose directory to output received DICOM files");
        final int returnValue = chooser.showOpenDialog(this);
        if (returnValue == JFileChooser.APPROVE_OPTION) {
            outputDirField.setText(chooser.getSelectedFile().getAbsolutePath() + File.separator);

            outputDir = chooser.getSelectedFile().getAbsolutePath() + File.separator;
            
            return true;
        }
        
        return false;
    }
    
    private boolean browseReportDir() {
        String initDir = reportDirField.getText();
        if (initDir.equals("") || !(new File(initDir).exists())) {
            initDir = ViewUserInterface.getReference().getDefaultDirectory();
        }
        
        final JFileChooser chooser = new JFileChooser(initDir);

        chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
        chooser.setDialogTitle("Choose directory to output HTML report");
        final int returnValue = chooser.showOpenDialog(this);
        if (returnValue == JFileChooser.APPROVE_OPTION) {
            reportDirField.setText(chooser.getSelectedFile().getAbsolutePath() + File.separator);

            reportDir = chooser.getSelectedFile().getAbsolutePath() + File.separator;
            
            return true;
        }
        
        return false;
    }
    
    /**
     * Append a line to the log output area in the Log tab.
     * 
     * @param line The line to append (do not include the trailing newline).
     */
    public void log(final String line) {
        logOutputArea.getTextArea().append(line + "\n");
        System.out.println("*****\t" + line);
    }
    
    private boolean readListenerConfig() {
        try {
            final InputStream in = getClass().getResourceAsStream(configFileName);
            if (in != null) {
                final Properties prop = new Properties();
                try {
                    prop.load(in);
                } catch (final IOException e) {
                    Preferences.debug("Unable to load stroke segementation listener plugin preferences file: " + configFileName + "\n", Preferences.DEBUG_MINOR);
                    e.printStackTrace();
                    if (in != null) {
                        in.close();
                    }
                    return false;
                }
                
                ae = prop.getProperty("listenerAETitle", ae);
                
                final String prefIP = prop.getProperty("listenerIP");
                if (prefIP != null && !prefIP.equals("")) {
                    ipAddress = prefIP;
                } else {
                    try {
                        ipAddress = InetAddress.getLocalHost().getHostAddress();
                    } catch (UnknownHostException e) {
                        e.printStackTrace();
                    }
                }
                
                port = Integer.parseInt(prop.getProperty("listenerPort", "" + port));
                
                outputDir = prop.getProperty("listenerOutputDir", outputDir);
                
                reportDir = prop.getProperty("listenerReportDir", outputDir);
                
                doEmailReport = Boolean.parseBoolean(prop.getProperty("listenerDoEmail", "" + doEmailReport));
                
                doAutoStart = Boolean.parseBoolean(prop.getProperty("listenerAutoStart", "" + doAutoStart));
                
                minExpectedSlices = Integer.parseInt(prop.getProperty("listenerMinExpectedSlices", "" + minExpectedSlices));
                
                int[] intArr = strToIntArray(prop.getProperty("listenerMinExpectedSlicesPWI", "" + intArrayToStr(minExpectedSlicesPWI)));
                if (intArr != null) {
                    minExpectedSlicesPWI = intArr;
                }
                
                maxWaitTime = Integer.parseInt(prop.getProperty("listenerMaxWaitTime", "" + maxWaitTime));
                
                if (in != null) {
                    in.close();
                }
                return true;
            } else {
                // couldn't load file
                return false;
            }
        } catch (IOException e) {
            e.printStackTrace();
            log("Error loading listener properties file: " + configFileName);
            return false;
        }
    }
    
    public static final String intArrayToStr(int[] intArr) {
        return IntStream.of(intArr).mapToObj(Integer::toString).collect(Collectors.joining(", "));
    }
    
    public static final int[] strToIntArray(String intListString) {
        int[] intArr = null;
        
        String[] strArr = intListString.split(",\\w*");
        if (strArr.length > 0) {
            intArr = new int[strArr.length];
            for (int i = 0; i < strArr.length; i++) {
                intArr[i] = Integer.parseInt(strArr[i].trim());
            }
        }
        
        return intArr;
    }
}