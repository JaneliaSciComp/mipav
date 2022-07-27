package gov.nih.mipav.view.dialogs;

import gov.nih.mipav.model.file.FileInfoBase.Unit;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.scripting.ScriptableActionInterface;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewJColorChooser;
import gov.nih.mipav.view.ViewJComponentEditImage;
import gov.nih.mipav.view.ViewJFrameImage;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;

public class JDialogGenerateIsolines extends JDialogScriptableBase {
	//~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    //private static final long serialVersionUID;

    
    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // result image
    
    private JTextField textThreshold;
    
    private double threshold;
	
	 /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
	public JDialogGenerateIsolines() {}
	
	public JDialogGenerateIsolines(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        image = im;
        init();
	}
	
	/**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setTitle("Generate isolines");

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.weightx = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.fill = GridBagConstraints.HORIZONTAL;

        JPanel paramPanel = new JPanel(new GridBagLayout());
        paramPanel.setBorder(buildTitledBorder("Parameters"));
        double maxValue = image.getMax();
        double minValue = image.getMin();
        double defaultThreshold = (minValue + maxValue)/2.0;
        
        JLabel labelThreshold = new JLabel(String.valueOf(minValue) + " <= Threshold <= " + String.valueOf(maxValue)+")");
        labelThreshold.setForeground(Color.black);
        labelThreshold.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = 0;
        paramPanel.add(labelThreshold, gbc);

        textThreshold = new JTextField(10);
        textThreshold.setText(String.valueOf(defaultThreshold));
        textThreshold.setFont(serif12);
        gbc.gridx = 1;
        paramPanel.add(textThreshold, gbc);
    }
	
	protected void callAlgorithm() {
		
	}
	
	protected void setGUIFromParams() {
        image = scriptParameters.retrieveInputImage();
        threshold = scriptParameters.getParams().getDouble("thresh");
	}
	
	 protected void storeParamsFromGUI() throws ParserException {
	        scriptParameters.storeInputImage(image);
	        scriptParameters.storeOutputImageParams(resultImage, true);
	        scriptParameters.getParams().put(ParameterFactory.newParameter("thresh", threshold));
	 }
}