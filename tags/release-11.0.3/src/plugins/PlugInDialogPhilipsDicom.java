import gov.nih.mipav.model.algorithms.*;

import gov.nih.mipav.model.file.FileInfoDicom;

import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.JDialogScriptableBase;

import java.awt.*;
import java.awt.event.ActionEvent;


import javax.swing.*;

/**
 * @author 	 senseneyj
 * @version  0.1
 * @see      JDialogBase
 * @see      AlgorithmInterface
 *
 */
public class PlugInDialogPhilipsDicom extends JDialogScriptableBase implements AlgorithmInterface {
    
    
    //~ Static fields/initializers -------------------------------------------------------------------------------------

	/**Action command for browse button*/
    public static final String OPEN = "Open";
    
    /**Action command for OK button*/
    public static final String OK = "OK";

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Source image for algorithm */
    private ModelImage image = null; // source image
    
    /** Browse button to load compatible image*/
    private JButton openButton;
    
    /** Name of file to load*/
    private JTextField textName;
    
    /** The algorithm to perform */
    private PlugInAlgorithmPhilipsDicom philipsAlgo = null;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor used for instantiation during script execution (required for dynamic loading).
     */
    public PlugInDialogPhilipsDicom() { }

    /**
     * Creates new dialog for kidney segmentation from an abdominal cavity image using a plugin.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public PlugInDialogPhilipsDicom(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);

        image = im;
        init();
        setVisible(false);
        
        if(image == null || !setVariables()) {
        	MipavUtil.displayError("Image not a valid DICOM image, reload image");
        } else {
        	OKButton.doClick();
        }
    }
    
//  ~ Methods --------------------------------------------------------------------------------------------------------

    // ************************************************************************
    // ************************** Event Processing ****************************
    // ************************************************************************

    /**
     * Closes dialog box when the OK button is pressed and calls the algorithm.
     *
     * @param  event  Event that triggers function.
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();

        if (command.equals(OPEN)) {
        	ViewUserInterface.getReference().openImageFrame();
        	image = ViewUserInterface.getReference().getActiveImageFrame().getImageA();
        	if(image != null) {
	        	textName.setText(image.getImageDirectory());
	        	validate();
        	}
        } else if (command.equals(OK)) {
            if (setVariables()) {
                callAlgorithm();
            } else {
            	MipavUtil.displayError("Image not a valid DICOM image, reload image");
            }
        } else if (command.equals("Script")) {
            callAlgorithm();
        } else if (command.equals("Cancel")) {
            dispose();
        } else {
            super.actionPerformed(event);
        }
    }

    // ************************************************************************
    // ************************** Algorithm Events ****************************
    // ************************************************************************

    /**
     * This method is required if the AlgorithmPerformed interface is implemented. It is called by the algorithm when it
     * has completed or failed to to complete, so that the dialog can be display the result image and/or clean up.
     *
     * @param  algorithm  Algorithm that caused the event.
     */
    public void algorithmPerformed(AlgorithmBase algorithm) {

        if (algorithm instanceof PlugInAlgorithmPhilipsDicom) {
            ViewJFrameImage resultFrame = new ViewJFrameImage(philipsAlgo.getDestImage());
            resultFrame.setTitle(philipsAlgo.getDestImage().getImageName());
            resultFrame.setVisible(true);

            if (algorithm.isCompleted()) {
                insertScriptLine();
            }

            if (algorithm != null) {
                algorithm.finalize();
                algorithm = null;
            }

            dispose();
        }

    } // end AlgorithmPerformed()

	/**
     * Once all the necessary variables are set, call the kidney segmentation algorithm
     */
    protected void callAlgorithm() {

        try {
            philipsAlgo = new PlugInAlgorithmPhilipsDicom(null, image);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed or failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            philipsAlgo.addListener(this);
            createProgressBar(image.getImageName(), " ...", philipsAlgo);

            setVisible(false); // Hide dialog

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still
                // have user interface work fast.
                if (philipsAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                philipsAlgo.run();
            }
        } catch (OutOfMemoryError x) {

            MipavUtil.displayError("Kidney segmentation: unable to allocate enough memory");

            return;
        }

    } // end callAlgorithm()

    protected void setGUIFromParams() {
    // TODO Auto-generated method stub, no params yet
    }

    protected void storeParamsFromGUI() throws ParserException {
    // TODO Auto-generated method stub, no params yet
    }
   
    /**
     * Should this plugin need to load the particular image
     */
    private void init() {
    	setForeground(Color.black);
        addNotify();
        setTitle("Load DICOM image");

        JPanel mainPanel = new JPanel(new GridBagLayout());
        mainPanel.setForeground(Color.black);
        mainPanel.setBorder(buildTitledBorder("Load DICOM image"));

        JLabel labelType = new JLabel("<html>The open image is not a valid Philips DICOM image file.  Click the <br>\""+OPEN+"\" button to load a Philips DICOM image.  Click the \""+OK+"\" button <br>to run the plugin.</html>");
        labelType.setForeground(Color.black);
        labelType.setFont(serif12);

        textName = new JTextField(30);
        textName.setText("DICOM image location");
        textName.setFont(serif12);
        textName.setEnabled(false);

        openButton = new JButton(OPEN);
        openButton.setPreferredSize(MipavUtil.defaultButtonSize);
        openButton.setFont(serif12B);
        openButton.setActionCommand(OPEN);
        openButton.addActionListener(this);
        
        
        Insets insets = new Insets(2, 10, 10, 10);
        GridBagConstraints gbc = new GridBagConstraints();

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 2;
        gbc.gridheight = 1;
        gbc.insets = insets;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.BOTH;
        gbc.anchor = GridBagConstraints.WEST;
        mainPanel.add(labelType, gbc);
        
        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.weightx = 0;
        gbc.insets = new Insets(0, 2, 0, 2);
        gbc.fill = GridBagConstraints.NONE;
        gbc.anchor = GridBagConstraints.WEST;
        mainPanel.add(openButton, gbc);
        
        gbc.gridx = 1;
        gbc.gridy = 1;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.BOTH;
        mainPanel.add(textName, gbc);

        getContentPane().add(mainPanel, BorderLayout.CENTER);

        // Build the Panel that holds the OK and CANCEL Buttons
        JPanel OKCancelPanel = new JPanel();

        // size and place the OK button
        buildOKButton();
        OKCancelPanel.add(OKButton, BorderLayout.WEST);

        // size and place the CANCEL button
        buildCancelButton();
        OKCancelPanel.add(cancelButton, BorderLayout.EAST);
        getContentPane().add(OKCancelPanel, BorderLayout.SOUTH);

        pack();
        setVisible(true);
        setResizable(false);
        System.gc();

    } // end init()
    
    private boolean setVariables() {
    	Object fileInfo = image.getFileInfo()[0];
    	if(fileInfo != null && fileInfo instanceof FileInfoDicom) {
    	
	    	Object obj1 = ((FileInfoDicom)fileInfo).getTagTable().getValue("2005,100E");
	    	Object obj2 = ((FileInfoDicom)fileInfo).getTagTable().getValue("0028,1052");
	    	Object obj3 = ((FileInfoDicom)fileInfo).getTagTable().getValue("0028,1053");
	    
	    	if(obj1 != null && obj2 != null && obj3 != null)
	    		return true;
    	}
    	
    	return false;
    }
}
