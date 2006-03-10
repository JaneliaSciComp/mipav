package gov.nih.mipav.view.renderer;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.media.*;
import javax.media.format.*;
import com.sun.media.codec.video.vcm.*;

import gov.nih.mipav.model.algorithms.AlgorithmTranscode;
import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.*;

/**
 *
 * <p>Title: JDialogAVIChoice</p>
 * <p>Description: Confirmation Dialog giving user the choice to choose what type
 * of compression to use when saving to AVI format</p>
 * <p>Copyright: Copyright (c) 2003</p>
 * <p>Company: </p>
 * @author not attributable
 * @version 1.0
 */
public class JPanelAVIChoice
    extends JDialogBase {

    /**
     * Text Field for M-JPEG quality
     */
    private JTextField qualityField;
    /**
     *  Combo box to select compression
     */
    private JComboBox compressionBox;

    private boolean okayPressed = false;
    private int operating_system = 0; // 0 = cross platform... default

    /**
     *   Creates new dialog.
     *   @param theParentFrame   Parent frame of dialog.
     */
    public JPanelAVIChoice(Frame theParentFrame) {
        super(theParentFrame, true);
        init(false);
    }

    public JPanelAVIChoice(Frame theParentFrame, boolean isColor) {
        super(theParentFrame, true);
        init(isColor);
    }

    /**
     * Creates and displays dialog
     * @param isColor is the image color
     */
    private void init(boolean isColor) {
        setTitle("Choose type of AVI file");

        String os = System.getProperty("os.name");
        if (os.startsWith("Windows")) {
            operating_system = 2; //windows
        }
        else if (!os.startsWith("Mac")) {
            operating_system = 1; //unix
        }
        //otherwise cross platform (mac)

        JPanel createPanel = new JPanel(new GridBagLayout());
        createPanel.setBorder(buildTitledBorder("Write file as"));

        compressionBox = new JComboBox();
        compressionBox.addItem("24 bit uncompressed RGB");
        if (!isColor) {
            compressionBox.addItem("8 bit RLE with LUT");
          //  compressionBox.addItem("8 bit with LUT");
        }
        compressionBox.addItem("Quicktime movie");

        //if using Windows OS, check to see which compression formats are installed
        if (operating_system > 1) {
             //build compression options
            compressionBox.addItem("M-JPEG");

            Format[] formats = null;
            try {
                formats = new NativeEncoder().getSupportedOutputFormats(
                    new RGBFormat());
                for (int i = 0; i < formats.length; i++) {
                    if (formats[i].getEncoding().equals("IV32")) {
                        compressionBox.addItem("IR32");
                    }
                    else if (formats[i].getEncoding().equals("IV41")) {
                        compressionBox.addItem("IR41");
                    }
                    else if (formats[i].getEncoding().equals("IV50")) {
                        compressionBox.addItem("Indeo Video 5");
                    }
                    else if (formats[i].getEncoding().equals("MPG4")) {
                        compressionBox.addItem("MS-MPEG4 V1");
                    }
                    else if (formats[i].getEncoding().equals("MP42")) {
                        compressionBox.addItem("MS-MPEG4 V2");
                    }
                    else if (formats[i].getEncoding().equals("DIVX")) {
                        compressionBox.addItem("DivX");
                    }
                }

            }
            catch (UnsatisfiedLinkError ex) {
                System.err.println("JMF library not installed");
            }
            catch (NoClassDefFoundError ex) {
                System.err.println("JMF library not installed");
            }
        }
        compressionBox.addItemListener(this);

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.anchor = gbc.WEST;
        gbc.insets = new Insets(0, 20, 0, 0);
        createPanel.add(compressionBox);

        if (operating_system > 1) {
            gbc.gridy++;
            gbc.insets = new Insets(0, 20, 0, 0);
            JLabel compLabel = new JLabel("M-JPEG quality (0.1 - 1.0):");
            compLabel.setFont(serif12B);
            createPanel.add(compLabel, gbc);
            gbc.gridx++;
            gbc.insets = new Insets(0, 0, 0, 20);
            qualityField = new JTextField(3);
            qualityField.setText(".80");
            MipavUtil.makeNumericsOnly(qualityField, true);
            qualityField.setEnabled(false);
            createPanel.add(qualityField, gbc);

        }


        JPanel buttonPanel = new JPanel();
        buildOKButton();
        buttonPanel.add(OKButton);

        getContentPane().add(createPanel);
        getContentPane().add(buttonPanel, BorderLayout.SOUTH);
        pack();
        setVisible(true);
    }

    /**
     * Check to see if M-JPEG was selected so that the quality field
     * will be enabled
     * @param event (combo box event)
     */
    public void itemStateChanged(ItemEvent event) {
        Object source = event.getSource();

        if (source == compressionBox) {
            if (((String)compressionBox.getSelectedItem()).equals("M-JPEG")) {
                qualityField.setEnabled(true);
            }
            else {
                qualityField.setEnabled(false);
            }
        }
    }

    /**
     * Accessor for m-jpeg quality
     * @return float m-jpeg quality ranging from .01 to 1.0
     */
    public float getMJPEGQuality() {
        if (qualityField != null) {
            try {
                return Float.parseFloat(qualityField.getText());
            }
            catch (Exception ex) {
                return 0.8f;
            }
        }
        else {
            return 0.8f;
        }
    }

    /**
     * Returns an int (based on AlgorithmTranscode's static variables)
     * representing the compression type to be used
     * @return int compression type
     */
    public int getCompression() {
       String compressionStr = (String)compressionBox.getSelectedItem();

       if (compressionStr.equals("24 bit uncompressed RGB")) {
           return AlgorithmTranscode.TRANSCODE_RGB;
       }
       else if (compressionStr.equals("8 bit RLE with LUT")) {
           return AlgorithmTranscode.TRANSCODE_8_BIT_RLE;
       }
       else if (compressionStr.equals("M-JPEG")) {
           return AlgorithmTranscode.TRANSCODE_MJPG;
       }
       else if (compressionStr.equals("Quicktime movie")) {
           return AlgorithmTranscode.TRANSCODE_QT;
       }
       else if (compressionStr.equals("IR32")) {
           return AlgorithmTranscode.TRANSCODE_IV32;
       }
       else if (compressionStr.equals("IR41")) {
           return AlgorithmTranscode.TRANSCODE_IV41;
       }
       else if (compressionStr.equals("Indeo Video 5")) {
           return AlgorithmTranscode.TRANSCODE_IV50;
       }
       else if (compressionStr.equals("MS-MPEG4 V1")) {
           return AlgorithmTranscode.TRANSCODE_MPG4;
       }
       else if (compressionStr.equals("MS-MPEG4 V2")) {
           return AlgorithmTranscode.TRANSCODE_MP42;
       }
       else if (compressionStr.equals("DivX")) {
           return AlgorithmTranscode.TRANSCODE_DIVX;
       }

       return 0;
    }

    /**
     * Was the okay button pressed
     * @return boolean was okay pressed
     */
    public boolean okayPressed() {
        return okayPressed;
    }

    /**
     *   Checks to see if the OK or Cancel buttons were pressed
     *   @param event    Event that triggered this function.
     */
    public void actionPerformed(ActionEvent event) {
        if (event.getSource() == OKButton) {
            okayPressed = true;
        }
        dispose();
    }
}
