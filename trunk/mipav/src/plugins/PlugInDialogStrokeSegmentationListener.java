import gov.nih.mipav.plugins.JDialogStandaloneScriptablePlugin;

import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.scripting.parameters.ParameterFactory;

import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.components.WidgetFactory;
import gov.nih.mipav.view.dialogs.JDialogBase;

import java.io.IOException;
import java.net.InetAddress;
import java.net.UnknownHostException;

import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.WindowEvent;
import java.io.File;

import javax.swing.*;


public class PlugInDialogStrokeSegmentationListener extends JDialogStandaloneScriptablePlugin {
    private JTextField aeField;
    private JTextField portField;
    private JTextField outputDirField;
    private JTextField emailField;
    
    private WidgetFactory.ScrollTextArea logOutputArea;
    
    private String ae = "MIPAV-stroke";

    private String ipAddress;
    
    private int port = 11115;

    private String outputDir = new String(System.getProperty("user.home") + File.separator + "mipav" + File.separator + "dicom_catcher" + File.separator);
    
    private String emailAddress = "";
    
    private StrokeSegmentationDicomReceiver dicomReceiver;
    
    private static final String svnVersion = "$Rev$";

    private static final String svnLastUpdate = "$Date$";
    
    private static final String pluginVersion = MipavUtil.getSVNChangedDate(svnLastUpdate);

    public PlugInDialogStrokeSegmentationListener() {
        try {
            ipAddress = InetAddress.getLocalHost().getHostAddress();
        } catch (UnknownHostException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
        
        init();
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
        mainPanel.setBorder(buildTitledBorder("DICOM receiver parameters"));
        
        JLabel labelAE = new JLabel("AE title");
        labelAE.setForeground(Color.black);
        labelAE.setFont(serif12);
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
        labelPort.setFont(serif12);
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
        labelOutput.setFont(serif12);
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
        dirFileButton.setFont(serif12B);
        gbc.gridx++;
        mainPanel.add(dirFileButton, gbc);
        
        gbc.gridy++;
        gbc.gridx = 0;
        
        JLabel labelEmail = new JLabel("Send email report to");
        labelEmail.setForeground(Color.black);
        labelEmail.setFont(serif12);
        mainPanel.add(labelEmail, gbc);
        
        emailField = new JTextField(40);
        emailField.setText("" + emailAddress);
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridx++;
        mainPanel.add(emailField, gbc);
  
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
        startButton.setFont(serif12B);
        buttonPanel.add(startButton);
        
        JButton stopButton = new JButton("Stop");
        stopButton.setActionCommand("Stop");
        stopButton.addActionListener(this);
        stopButton.setForeground(Color.black);
        stopButton.setFont(serif12B);
        buttonPanel.add(stopButton);
        
        JButton exitButton = new JButton("Exit");
        exitButton.setActionCommand("Exit");
        exitButton.addActionListener(this);
        exitButton.setForeground(Color.black);
        exitButton.setFont(serif12B);
        buttonPanel.add(exitButton);

        getContentPane().add(buttonPanel, BorderLayout.SOUTH);

        pack();
        setVisible(true);
        setResizable(true);
        System.gc();
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
            browseDir();
        }
    }
    
    protected void setGUIFromParams() {
        ae = scriptParameters.getParams().getString("AE_title");
        port = scriptParameters.getParams().getInt("port");
        outputDir = scriptParameters.getParams().getString("output_dir");
        emailAddress = scriptParameters.getParams().getString("email_address");
    }

    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.getParams().put(ParameterFactory.newParameter("AE_title", ae));
        scriptParameters.getParams().put(ParameterFactory.newParameter("port", port));
        scriptParameters.getParams().put(ParameterFactory.newParameter("output_dir", outputDir));
        scriptParameters.getParams().put(ParameterFactory.newParameter("email_address", emailAddress));
    }
    
    /**
     * Perform any actions required after the running of the algorithm is complete.
     */
    protected void doPostAlgorithmActions() {
        // nothing to do
    }
    
    private boolean setVariables() {
        ae = aeField.getText();
        port = Integer.parseInt(portField.getText());
        outputDir = outputDirField.getText();
        emailAddress = emailField.getText();
        
        return true;
    }
    
    protected void callAlgorithm() {
        try {
            log("Starting DICOM receiver: " + ae + " @ " + ipAddress + ":" + port);
            
            dicomReceiver = new StrokeSegmentationDicomReceiver(ipAddress, port, ae, outputDir, emailAddress, logOutputArea);
        } catch (IOException e) {
            // TODO Auto-generated catch block
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
        
        super.windowClosing(event);
    }
    
    private boolean browseDir() {
        String initDir = outputDirField.getText();
        if (initDir.equals("") || !(new File(initDir).exists())) {
            initDir = ViewUserInterface.getReference().getDefaultDirectory();
        }
        
        final JFileChooser chooser = new JFileChooser(initDir);

        chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
        chooser.setDialogTitle("Choose directory containing ADC and DWI volumes");
        final int returnValue = chooser.showOpenDialog(this);
        if (returnValue == JFileChooser.APPROVE_OPTION) {
            outputDirField.setText(chooser.getSelectedFile().getAbsolutePath() + File.separator);

            outputDir = chooser.getSelectedFile().getAbsolutePath() + File.separator;
            
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
    }
}