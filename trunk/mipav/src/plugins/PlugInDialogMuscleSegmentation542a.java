//MIPAV is freely available from http://mipav.cit.nih.gov

//THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, 
//EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES 
//OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND 
//NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT 
//HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, 
//WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING 
//FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE 
//OR OTHER DEALINGS IN THE SOFTWARE. 

/*****************************************************************
******************************************************************

The MIPAV application is intended for research use only.
This application has NOT been approved for ANY diagnostic use 
by the Food and Drug Administration. There is currently no 
approval process pending. 

This software may NOT be used for diagnostic purposes.

******************************************************************
******************************************************************/

import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.JDialogScriptableBase;

import java.awt.*;
import java.awt.event.ActionEvent;
import java.io.File;
import java.io.FilenameFilter;
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
public class PlugInDialogMuscleSegmentation542a extends JDialogScriptableBase implements AlgorithmInterface {

    //~ Instance fields ------------------------------------------------------------------------------------------------
    
	/**Radio button to denote image is two thigh CT*/
    private JRadioButton twoThighRadio;
    
    /**Radio button to denote image is abdomen CT*/
    private JRadioButton abdomenRadio;
    
    /**Alternate configs in user-dir*/
    private JRadioButton[] extraRadio;
    
    /**Radio button to denote user would like to build custom plugin.*/
    private JRadioButton customRadio;
    
    private PlugInMuscleImageDisplay542a.ImageType imageType;
    
    private boolean multipleSlices = false;
    
    /** Result image. */
    private ModelImage resultImage = null;

    /** The source image */
    private ModelImage image; 
    
    /** String defined by RunTime image type*/
    private String fileName = new String();
    
    /** Segmentation algorithm */
    private PlugInAlgorithmMuscleSegmentation542a muscleSegAlgo = null;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor used for instantiation during script execution (required for dynamic loading).
     */
    public PlugInDialogMuscleSegmentation542a() { }

    /**
     * Creates new dialog for kidney segmentation from an abdominal cavity image using a plugin.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public PlugInDialogMuscleSegmentation542a(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);

        image = im;
        imageType = detectImageType(im);
        if(image.getNDims() < 3) {
        	multipleSlices = false;
        } else
        	multipleSlices = true;
        //if(imageType == PlugInMuscleImageDisplay.ImageType.Unknown)
        	init();
        //else
        //	callAlgorithm();
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
        
        

        if (algorithm instanceof PlugInAlgorithmMuscleSegmentation542a) {
            Preferences.debug("Muscle segmentation, Elapsed time: " + algorithm.getElapsedTime());
            image.clearMask();
           
            if (algorithm.isCompleted()) {
                insertScriptLine();
            }

            algorithm.finalize();
            algorithm = null;

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
            muscleSegAlgo = new PlugInAlgorithmMuscleSegmentation542a(image, imageType, multipleSlices, fileName);

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
   
    private class NiaAcceptable implements FilenameFilter {

		public boolean accept(File dir, String name) {
			if(name.indexOf('.') != -1) {
				if(name.substring(name.lastIndexOf('.')+1).equals("nia"))
					return true;
			}
			return false;
		}
    }
    
    private void init() {
        
        String fileName = image.getFileInfo()[0].getFileDirectory()+PlugInMuscleImageDisplay542a.VOI_DIR+File.separator;
        ArrayList<String> validConfig = new ArrayList<String>();
    	if(new File(fileName).exists()) {
        	String[] allFiles = new File(fileName).list(new NiaAcceptable());
        	if(allFiles != null) {
	        	for(int i=0; i<allFiles.length; i++) {
	        		if(!allFiles[i].equals(imageType.toString()+".nia")) 
	        			validConfig.add(allFiles[i]);
	        	}
        	}
    	}
        setForeground(Color.black);
        setTitle("NIA Muscle Segmentation 5.4.0");

        
        
        twoThighRadio = new JRadioButton("Two thighs");
        twoThighRadio.setFont(MipavUtil.font12);

        abdomenRadio = new JRadioButton("Abdomen");
        abdomenRadio.setFont(MipavUtil.font12);
        
        extraRadio = new JRadioButton[validConfig.size()];
        for(int i=0; i<validConfig.size(); i++) {
        	extraRadio[i] = new JRadioButton(validConfig.get(i));
        	extraRadio[i].setFont(MipavUtil.font12);
        }
        
        customRadio = new JRadioButton("Custom");
        customRadio.setFont(MipavUtil.font12);

        if (imageType.equals(PlugInMuscleImageDisplay542a.ImageType.Thigh)) {
            twoThighRadio.setSelected(true);
        } else if (imageType.equals(PlugInMuscleImageDisplay542a.ImageType.Abdomen)) {
            abdomenRadio.setSelected(true);
        } else {
        	customRadio.setSelected(true);
        }
        
        ButtonGroup group = new ButtonGroup();
        group.add(twoThighRadio);
        group.add(abdomenRadio);
        for(int i=0; i<extraRadio.length; i++) {
        	group.add(extraRadio[i]);
        }
        group.add(customRadio);
        
        JPanel mainPanel = new JPanel(new GridBagLayout());
        mainPanel.setForeground(Color.black);
        mainPanel.setBorder(buildTitledBorder("What kind of image?"));
       
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.insets = new Insets(3, 3, 3, 3);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 0;
        gbc.gridy = 0;
        mainPanel.add(twoThighRadio, gbc);
        gbc.gridx = 1;
        mainPanel.add(abdomenRadio, gbc);
        for(int i=0; i<extraRadio.length; i++) {
        	if((gbc.gridx+1)%3 == 0) {
        		gbc.gridy++;
        		gbc.gridx = 0;
        	} else
        		gbc.gridx++;
        	mainPanel.add(extraRadio[i], gbc);
        }
        gbc.gridx++;
        mainPanel.add(customRadio, gbc);
        
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
     *.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {

        if (twoThighRadio.isSelected()) {
            imageType = PlugInMuscleImageDisplay542a.ImageType.Thigh;
        } else if (abdomenRadio.isSelected()) {
            imageType = PlugInMuscleImageDisplay542a.ImageType.Abdomen;
        } else if (customRadio.isSelected()) {
        	imageType = PlugInMuscleImageDisplay542a.ImageType.Custom;
        } else {
        	//load custom
            imageType = PlugInMuscleImageDisplay542a.ImageType.RunTimeDefined;
            for(int i=0; i<extraRadio.length; i++) {
            	if(extraRadio[i].isSelected())
            		fileName = extraRadio[i].getText();
            }
            System.out.println("File name: "+fileName);
        }
        return true;
    }
    
    /**
     * Creates the image based on an array of files greater than one.  When dealing with single image, the ModelImage
     * is already loaded.  FileInfo is transferred from 1st slice.
     * 
     * @param fileAr
     * @return constructed image
     */
    @SuppressWarnings("unused")
    private ModelImage createImage(File[] fileAr) {
    	FileIO fileIO = new FileIO();
    	fileIO.setQuiet(true);
    	
    	ModelImage resultImage = fileIO.readOrderedGrayscale(fileAr, true, null, false);
    	
    	FileInfoBase[] fb = new FileInfoBase[fileAr.length];
    	for(int i=0; i<fileAr.length; i++) {
    		fb[i] = image.getFileInfo(0);
    	}
    	resultImage.setFileInfo(fb);
    	
    	return resultImage;
    }
    
    /**
     * Detects what type of image is being dealt with.  Basically looks for two thighs; if
     * not found, look for abdomen; if not found, return ImageType.UNKNOWN_TYPE
     * 
     * @param im
     * @return
     */
    
    private PlugInMuscleImageDisplay542a.ImageType detectImageType(ModelImage im) {
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
		
		ArrayList<Integer> boneRowHigh = new ArrayList<Integer>();
		ArrayList<Double> bonePercent = new ArrayList<Double>();
		
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
		Iterator<Integer> itrBone = boneRowHigh.iterator();
		Iterator<Double> itrPercent = bonePercent.iterator();
		while(itrBone.hasNext()) {
			int y = (itrBone.next()).intValue();
    		boneCountLeft = 0;
    		boneCountRight = 0;
    		airCountCenter = 0;
    		muscleCountLeftFar = 0;
    		muscleCountLeftNear = 0;
    		muscleCountRightNear = 0;
    		muscleCountRightFar = 0;
    		
    		foundBoneLeft = false;
    		foundBoneRight = false;
    		
    		double boneTest = (itrPercent.next()).doubleValue();
    		
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
			return PlugInMuscleImageDisplay542a.ImageType.Thigh;
		} 
		
		return PlugInMuscleImageDisplay542a.ImageType.Abdomen;
    }
    
    @SuppressWarnings("unused")
    private File[] detectImageSequence(ModelImage im) {
    	try {
    		File dir = new File(im.getFileInfo()[0].getFileDirectory());
	    	String name = im.getFileInfo()[0].getFileName();
	    	boolean likelyMultiple = false;
	    	if(name.indexOf('.') > name.length() - 5) {
	    		int offset = 0;
	    		if(name.indexOf('.') > 2)
	    			offset = 2;
	    		name = name.substring(0, name.lastIndexOf('.')-offset);
	    	} else	
	    		name = name.substring(0, im.getFileInfo()[0].getFileName().length()-2);
	    	File[] contain = dir.listFiles();
	    	int size = 0;
	    	ArrayList<File> fileList = new ArrayList<File>();
	    	for(int i=0; i<contain.length; i++) {
	    		if(contain[i].getName().contains(name)) {
	    			size++;
	    			fileList.add(contain[i]);
	    		}
	    	}
	    	if(size > 1)
	    		likelyMultiple = true;
	    	
	    	if(!likelyMultiple) {
	    		return new File[1];
	    	}
	    	
	    	String[] commonExt = new String[fileList.size()];
	    	int[] commonCount = new int[fileList.size()];
	    	for(int i=0; i<fileList.size(); i++) {
	    		if(fileList.get(i).getName().lastIndexOf(".") != -1) 
	    			commonExt[i] = fileList.get(i).getName().substring(fileList.get(i).getName().lastIndexOf("."));
	    		else
	    			commonExt[i] = "";
	    		commonCount[i] = 0;
	    	}
	    	
	    	//find most frequent extension
	    	String ext = "";
	    	for(int i=0; i<fileList.size(); i++) {
	    		for(int j=0; j<fileList.size(); j++) {
	    			if(fileList.get(j).getName().lastIndexOf(".") != -1) 
	    				ext = fileList.get(j).getName().substring(fileList.get(j).getName().lastIndexOf("."));
	    			else
	    				ext = "";
		    			if(commonExt[i].equals(ext)) {
		        			commonCount[i]++;
		        		} 
	    		}
	    	}
	    	
	    	int bestCount = 0, numFound = 1;
	    	for(int i=0; i<commonCount.length; i++) {
	    		if(commonCount[i] > numFound) {
	    			numFound = commonCount[i];
	    			bestCount = i;
	    		}
	    	}
	    	
	    	File[] imageSet = new File[numFound];
	    	int imageIndex = 0;
	    	ext = "";
	    	for(int i=0; i<fileList.size(); i++) {
	    		if(fileList.get(i).getName().lastIndexOf(".") != -1) 
					ext = fileList.get(i).getName().substring(fileList.get(i).getName().lastIndexOf("."));
				else
					ext = "";
	    		if(commonExt[bestCount].equals(ext)) {
	    			imageSet[imageIndex] = fileList.get(i);
	    			imageIndex++;
	    		}
	    	}
	    	
	    	if(imageSet.length == 1 && likelyMultiple) {
	    		MipavUtil.displayInfo("This image is likely a 3D CT image file, but the algorithm was unable to extract the image.\n"+
	    								"Try reopening the image as a multi-slice image and reload the algorithm");
	    		return null;
	    	}
	    	
	    	return imageSet;
    	} catch(Exception e) {
    		MipavUtil.displayInfo("Opening image as a single slice.  If this is a multi-slice image, please reload the image");
    		e.printStackTrace();
    		return null;
    	}
    }
}
