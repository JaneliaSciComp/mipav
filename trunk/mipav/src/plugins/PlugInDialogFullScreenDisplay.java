import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.structures.ModelImage;

import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ScrollCorrector;
import gov.nih.mipav.view.dialogs.JDialogBase;

import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;

import javax.swing.*;
import javax.swing.border.LineBorder;


public class PlugInDialogFullScreenDisplay extends JDialogBase implements AlgorithmInterface, ActionListener {

    private JPanel mainPanel;

    private GridBagConstraints gbc;

    private JLabel fileLabel;

    private JTextField filePathTextField;

    private JButton fileBrowseButton;

    private JTextArea outputTextArea;

    private JScrollPane scrollPane;

    private String fileName;

    String directory;

    private String currDir;

    private PlugInAlgorithmFullScreenDisplay alg;

    public PlugInDialogFullScreenDisplay() {

    }

    public PlugInDialogFullScreenDisplay(final boolean modal) {
        super(modal);
        init();
    }

    private void init() {
        setForeground(Color.black);
        setTitle("Full Screen Display v1.0");
        mainPanel = new JPanel(new GridBagLayout());
        gbc = new GridBagConstraints();

        fileLabel = new JLabel("File ");
        filePathTextField = new JTextField(35);
        filePathTextField.setEditable(false);
        filePathTextField.setBackground(Color.white);
        fileBrowseButton = new JButton("Browse");
        fileBrowseButton.addActionListener(this);
        fileBrowseButton.setActionCommand("FileBrowse");

        outputTextArea = new JTextArea();
        outputTextArea.setRows(15);
        outputTextArea.setEditable(false);
        outputTextArea.setBackground(Color.lightGray);
        outputTextArea.setBorder(new LineBorder(Color.black));
        outputTextArea.setForeground(Color.black);
        scrollPane = new JScrollPane(outputTextArea, ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED, ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);

        scrollPane.getVerticalScrollBar().addAdjustmentListener(new ScrollCorrector());

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.insets = new Insets(15, 5, 5, 15);
        gbc.gridwidth = 1;
        gbc.anchor = GridBagConstraints.EAST;
        mainPanel.add(fileLabel, gbc);
        gbc.gridx = 1;
        mainPanel.add(filePathTextField, gbc);
        gbc.gridx = 2;
        mainPanel.add(fileBrowseButton, gbc);

        gbc.anchor = GridBagConstraints.CENTER;
        gbc.fill = GridBagConstraints.BOTH;

        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.gridwidth = 3;
        mainPanel.add(scrollPane, gbc);

        final JPanel OKCancelPanel = new JPanel();
        buildOKButton();
        OKButton.setActionCommand("ok");
        OKCancelPanel.add(OKButton, BorderLayout.WEST);
        buildCancelButton();
        cancelButton.setActionCommand("cancel");
        OKCancelPanel.add(cancelButton, BorderLayout.EAST);

        getContentPane().add(mainPanel, BorderLayout.CENTER);
        getContentPane().add(OKCancelPanel, BorderLayout.SOUTH);

        pack();
        setMinimumSize(getSize());

        setVisible(true);
        setResizable(false);

    }

    private void callAlgorithm() {
        Image cornerImage;
        try {
            cornerImage = MipavUtil.getIconImage("WhiteCircle.png");
        } catch (final FileNotFoundException e) {
            MipavUtil.displayError("Eye tracker landmark image not found.");
            e.printStackTrace();
            return;
        }

        final FileIO fileIO = new FileIO();
        final boolean multiFile = false;
        final ModelImage image = fileIO.readImage(fileName, directory, multiFile, null);
        final int xDim = image.getExtents()[0];
        final int yDim = image.getExtents()[1];
        final int length = xDim * yDim;
        BufferedImage inputImage = null;
        inputImage = new BufferedImage(xDim, yDim, BufferedImage.TYPE_INT_ARGB);
        if (image.isColorImage()) {
            final int[] imageData = new int[length * 4];
            try {
                image.exportData(0, length * 4, imageData);
            } catch (final IOException e) {
                MipavUtil.displayError("IOException " + e + " on image.exportData(0, length*4, imageData)");
                return;
            }
            final int[] bufferData = new int[length * 4];
            for (int i = 0; i < length; i++) {
                bufferData[i * 4 + 0] = imageData[i * 4 + 1];
                bufferData[i * 4 + 1] = imageData[i * 4 + 2];
                bufferData[i * 4 + 2] = imageData[i * 4 + 3];
                bufferData[i * 4 + 3] = 255;
            }
            inputImage.getRaster().setPixels(0, 0, xDim, yDim, bufferData);
        } else {
            final int[] imageData = new int[length];
            try {
                image.exportData(0, length, imageData);
            } catch (final IOException e) {
                MipavUtil.displayError("IOException " + e + " on image.exportData(0, length, imageData)");
                return;
            }
            final int[] bufferData = new int[length * 4];
            for (int i = 0; i < length; i++) {
                bufferData[i * 4 + 0] = imageData[i];
                bufferData[i * 4 + 1] = imageData[i];
                bufferData[i * 4 + 2] = imageData[i];
                bufferData[i * 4 + 3] = 255;
            }
            inputImage.getRaster().setPixels(0, 0, xDim, yDim, bufferData);

        }
        /*
         * try { inputImage = ImageIO.read( new File(directory + fileName) ); } catch (IOException e) {
         * MipavUtil.displayError("IOException " + e + " on ImageIO.read( new File(directory + fileName)"); return; }
         */
        alg = new PlugInAlgorithmFullScreenDisplay(inputImage, cornerImage, outputTextArea);

        alg.addListener(this);

        if (isRunInSeparateThread()) {

            // Start the thread as a low priority because we wish to still
            // have user interface work fast.
            if (alg.startMethod(Thread.MIN_PRIORITY) == false) {
                MipavUtil.displayError("A thread is already running on this object");
            }
        } else {
            alg.run();
        }
    }

    @Override
    public void algorithmPerformed(final AlgorithmBase algorithm) {
        if (alg.isCompleted()) {
            setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
            OKButton.setEnabled(false);
            cancelButton.setText("Close");

            outputTextArea.append("Finished" + "\n");

        }

    }

    @Override
    public void actionPerformed(final ActionEvent e) {
        final String command = e.getActionCommand();
        if (command.equalsIgnoreCase("FileBrowse")) {
            final JFileChooser chooser = new JFileChooser(Preferences.getImageDirectory());
            /*
             * if (currDir != null) { chooser.setCurrentDirectory(new File(currDir)); }
             */
            chooser.setDialogTitle("Choose File");
            chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
            final int returnValue = chooser.showOpenDialog(this);
            if (returnValue == JFileChooser.APPROVE_OPTION) {
                currDir = chooser.getSelectedFile().getAbsolutePath();
                Preferences.setImageDirectory(new File(currDir));
                fileName = chooser.getSelectedFile().getName();
                directory = chooser.getCurrentDirectory() + File.separator;
                filePathTextField.setText(currDir);
            }
        } else if (command.equalsIgnoreCase("ok")) {
            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equalsIgnoreCase("cancel")) {

            dispose();
        } else {
            super.actionPerformed(e);
        }

    }

    private boolean setVariables() {

        if (filePathTextField.getText().trim().equals("")) {
            MipavUtil.displayError("File is required");
            return false;

        }

        return true;
    }

}
