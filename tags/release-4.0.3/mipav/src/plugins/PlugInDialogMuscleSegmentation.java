import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.JDialogFaceAnonymizerBET;
import gov.nih.mipav.view.dialogs.JDialogScriptableBase;

import java.awt.*;
import java.awt.event.ActionEvent;
import java.util.ArrayList;
import java.util.Iterator;

import javax.swing.*;

/**
 * @author senseneyj
 * @version  June 4, 2007
 * @see      JDialogBase
 * @see      AlgorithmInterface
 *
 */
public class PlugInDialogMuscleSegmentation extends JDialogScriptableBase implements AlgorithmInterface {
    
    
    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /**declare UID */

    //~ Instance fields ------------------------------------------------------------------------------------------------
    
    private JRadioButton twoThighRadio;
    
    private JRadioButton abdomenRadio;
    
    private PlugInMuscleImageDisplay.ImageType imageType;
    
    /** Result image. */
    private ModelImage resultImage = null;

    /** DOCUMENT ME! */
    private ModelImage image; // source image
    
    /** DOCUMENT ME! */
    private PlugInAlgorithmMuscleSegmentation muscleSegAlgo = null;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor used for instantiation during script execution (required for dynamic loading).
     */
    public PlugInDialogMuscleSegmentation() { }

    /**
     * Creates new dialog for kidney segmentation from an abdominal cavity image using a plugin.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public PlugInDialogMuscleSegmentation(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);

        image = im;
        imageType = detectImageType(im);
        if(imageType == PlugInMuscleImageDisplay.ImageType.Unknown)
        	init();
        else
        	callAlgorithm();
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

        if (command.equals("OK")) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Script")) {
            callAlgorithm();
        } else if (command.equals("Cancel")) {
            dispose();
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
        
        

        if (algorithm instanceof PlugInAlgorithmMuscleSegmentation) {
            Preferences.debug("Muscle segmentation, Elapsed time: " + algorithm.getElapsedTime());
            image.clearMask();
           
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
     * Once all the necessary variables are set, call the NIA muscle segmentation algorithm
     */
    protected void callAlgorithm() {

        try {
            //FileInfoBase[] info = image.getFileInfo();
            //info[0].displayAboutInfo(this); //expecting a 2D image
           
            muscleSegAlgo = new PlugInAlgorithmMuscleSegmentation(image, imageType, parentFrame);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed or failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            muscleSegAlgo.addListener(this);

            setVisible(false); // Hide dialog

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still
                // have user interface work fast.
                if (muscleSegAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                muscleSegAlgo.run();
            }
        } catch (OutOfMemoryError x) {
            if (resultImage != null) {
                resultImage.disposeLocal(); // Clean up memory of result image
                resultImage = null;
            }

            MipavUtil.displayError("Muscle segmentation: unable to allocate enough memory");

            return;
        }

    } // end callAlgorithm()

    protected void setGUIFromParams() {
    // TODO Auto-generated method stub, no params yet
    }

    protected void storeParamsFromGUI() throws ParserException {
    // TODO Auto-generated method stub, no params yet
    }
   
    private void init() {
        
        
        setForeground(Color.black);
        setTitle("NIA Muscle Segmentation");

        GridBagConstraints gbc = new GridBagConstraints();
        int yPos = 0;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.insets = new Insets(3, 3, 3, 3);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 0;
        gbc.gridy = yPos++;

        JPanel mainPanel = new JPanel(new GridBagLayout());
        mainPanel.setForeground(Color.black);
        mainPanel.setBorder(buildTitledBorder("What kind of image?"));

        twoThighRadio = new JRadioButton("Two thighs");
        twoThighRadio.setFont(MipavUtil.font12);

        abdomenRadio = new JRadioButton("Abdomen");
        abdomenRadio.setFont(MipavUtil.font12);
        
        if (true) {
            twoThighRadio.setSelected(true);
        } else {
            abdomenRadio.setSelected(true);
        }
        
        ButtonGroup group = new ButtonGroup();
        group.add(twoThighRadio);
        group.add(abdomenRadio);
        
        mainPanel.add(twoThighRadio);
        mainPanel.add(abdomenRadio);

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
        OKButton.requestFocus();
        setVisible(true);
        setResizable(false);
        System.gc();

    } // end init()
    
    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {

        if (twoThighRadio.isSelected()) {
            imageType = PlugInMuscleImageDisplay.ImageType.Thigh;
        } else if (abdomenRadio.isSelected()) {
            imageType = PlugInMuscleImageDisplay.ImageType.Abdomen;
        } else {
            MipavUtil.displayWarning("You have selected an unsupported image type.");
            return false;
        }
        return true;
    }
    /**
     * Detects what type of image is being dealt with.  Basically looks for two thighs; if
     * not found, look for abdomen; if not found, return ImageType.UNKNOWN_TYPE
     * 
     * @param im
     * @return
     */
    
    private PlugInMuscleImageDisplay.ImageType detectImageType(ModelImage im) {
    	int xBound = im.getFileInfo()[0].getExtents()[0];
    	int yBound = im.getFileInfo()[0].getExtents()[1];
    	
    	//comparing each row to an expected two thigh profile
    	int qualifiedRows = 0;
    	int boneCountLeft = 0;
		int boneCountRight = 0;
		int airCountCenter = 0;
		int muscleCountLeftFar = 0;
		int muscleCountLeftNear = 0;
		int muscleCountRightNear = 0;
		int muscleCountRightFar = 0;
    	
		boolean foundBoneLeft = false;
		boolean foundBoneRight = false;
		
		ArrayList boneRowHigh = new ArrayList();
		ArrayList bonePercent = new ArrayList();
		
		for(int y=0; y<yBound; y++) {
			int boneNumber = 0;
			double percent = 0;
			for(int x=0; x<xBound; x++) {
				if(im.getDouble(x, y) > 400) {
					boneNumber++;
				}
			}
			if((percent = ((double)boneNumber)/((double)xBound)) > 0.05) {
				boneRowHigh.add(y);
				bonePercent.add(percent);
			}
		}
		
		double test = 0.0;
		Iterator itrBone = boneRowHigh.iterator();
		Iterator itrPercent = bonePercent.iterator();
		while(itrBone.hasNext()) {
			int y = ((Integer)itrBone.next()).intValue();
    		boneCountLeft = 0;
    		boneCountRight = 0;
    		airCountCenter = 0;
    		muscleCountLeftFar = 0;
    		muscleCountLeftNear = 0;
    		muscleCountRightNear = 0;
    		muscleCountRightFar = 0;
    		
    		foundBoneLeft = false;
    		foundBoneRight = false;
    		
    		double boneTest = ((Double)itrPercent.next()).doubleValue();
    		
    		for(int x=0; x<xBound; x++){
    			test = im.getDouble(x, y);
    			if(!foundBoneLeft) {
    				if(test > 0 && test < 100)
    					muscleCountLeftFar++;
    				else if(test > 400)
    					boneCountLeft++;
    			} else if(foundBoneLeft && !foundBoneRight) {
    				if(test > 0 && test < 100) {
    					if(((double)airCountCenter)/((double)xBound) > 0.001)
    						muscleCountRightNear++;
    					else
    						muscleCountLeftNear++;
    				}
    				else if(test > 400)
    					boneCountRight++;
    				else if(test < -900) 
    					airCountCenter++;
    			} else if(foundBoneRight && test > 0 && test < 100) {
    					muscleCountRightFar++;
    			}
    			
    			if(!foundBoneLeft && ((double)boneCountLeft)/((double)xBound) > boneTest/2-.01) {
    				foundBoneLeft = true;
    			}
    			if(!foundBoneRight && xBound-x < xBound/2 && ((double)boneCountRight)/((double)xBound) > boneTest/2-.01) {
    				foundBoneRight = true;
    			}
    		}
    		
    		if(foundBoneLeft && foundBoneRight) {
    			if(((double)airCountCenter)/((double)xBound) > 0.001) {
    				qualifiedRows++;
    			} else if(muscleCountLeftNear > .75*muscleCountLeftFar + .75*muscleCountRightFar) {
    				qualifiedRows++;
    			}
    		}
    	}
		
		//compares rows where bone was found to rows which matched two thigh description
		if(((double)qualifiedRows)/((double)boneRowHigh.size()) > .75) {
			return PlugInMuscleImageDisplay.ImageType.Thigh;
		} 
		
		return PlugInMuscleImageDisplay.ImageType.Abdomen;
    }
}
