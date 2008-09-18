import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.file.FileInfoDicom;
import gov.nih.mipav.model.file.FileUtility;
import gov.nih.mipav.model.file.FileWriteOptions;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.JDialogScriptableBase;

import java.awt.*;
import java.awt.event.ActionEvent;
import java.io.File;
import java.util.ArrayList;
import java.util.Vector;

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
    public static final String BROWSE = "Browse";
    
    /**Action command for OK button*/
    public static final String OK = "OK";

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Result image. */
    private ModelImage resultImage = null;

    /** Dource image for algorithm */
    private ModelImage image = null; // source image
    
    /** Browse button to load compatible image*/
    private JButton browseButton;
    
    /** Name of file to load*/
    private JTextField textName;
    
    /**File directory where image is stored.*/
    private File fileDir;
    
    /**Whether to perform image loading when OK button is pressed. */
    private boolean performImageLoad = false;
    
    /** The algorithm to perform */
    private PlugInAlgorithmPhilipsDicom philipsAlgo = null;
    
    /**The array making up the new Philips DICOM image */
    private File imageFile;

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
        	performImageLoad = true;
        	setVisible(true);
        } else {
        	performImageLoad = false;
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

        if (command.equals(BROWSE)) {

        	imageFile = setImageToOpen();
        	if(imageFile != null) {
        		textName.setText(imageFile.getAbsolutePath());
        		validate();
        	}
            
        } if (command.equals(OK)) {
        	if(performImageLoad) {
        		try {
        			loadImage();
        		} catch(Exception e) {
        			MipavUtil.displayError("Not a valid Philips DICOM image file, try another image.");
        		}
        	}

            if (setVariables()) {
                callAlgorithm();
            } else {
            	MipavUtil.displayError("Image not a valid DICOM image, reload image");
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

        if (algorithm instanceof PlugInAlgorithmPhilipsDicom) {
            Preferences.debug("DICOM Process Time Elapsed: " + algorithm.getElapsedTime());
            image.clearMask();
            
            if ((philipsAlgo.isCompleted() == true) && (resultImage != null)) {

                progressBar.setMessage("Saving image...");
            	
            	// The algorithm has completed and produced a new image to be displayed.
                updateFileInfo(image, resultImage);

                resultImage.clearMask();

                FileIO fileIO = new FileIO();
                FileWriteOptions opts = new FileWriteOptions(true);
                opts.setFileType(FileUtility.XML);
                opts.setFileDirectory(image.getImageDirectory()+"corrected"+File.separator);
                opts.setFileName(image.getImageName()+"_corrected");
                if(image.getNDims() > 2) {
	                opts.setBeginSlice(0);
	                opts.setEndSlice(resultImage.getExtents()[2]-1);
                }
                opts.setIsScript(true);
                opts.setOptionsSet(true); 
                fileIO.writeImage(resultImage, opts);
                
                progressBar.dispose();
                
                try {
                    new ViewJFrameImage(resultImage);
                    
                } catch (OutOfMemoryError error) {
                    System.gc();
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }
                
                MipavUtil.displayInfo("Image saved as "+image.getImageName()+"_corrected\n"+
                						"in "+image.getImageDirectory()+"corrected"+File.separator);
            } else if (resultImage != null) {

                // algorithm failed but result image still has garbage
                resultImage.disposeLocal(); // clean up memory
                resultImage = null;
                System.gc();

            }

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
            String name = makeImageName(image.getImageName(), "_philpsconvert");
            resultImage = (ModelImage) image.clone();
            resultImage.setImageName(name);
            philipsAlgo = new PlugInAlgorithmPhilipsDicom(resultImage, image);

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
            if (resultImage != null) {
                resultImage.disposeLocal(); // Clean up memory of result image
                resultImage = null;
            }

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

        JLabel labelType = new JLabel("<html>The open image is not a valid Philips DICOM image file.  Click the \"Browse\" <br>button to find a Philips DICOM image to load.</html>");
        labelType.setForeground(Color.black);
        labelType.setFont(serif12);

        textName = new JTextField(30);
        textName.setText("DICOM image location");
        textName.setFont(serif12);
        textName.setEnabled(false);

        browseButton = new JButton("Browse");
        browseButton.setPreferredSize(MipavUtil.defaultButtonSize);
        browseButton.setFont(serif12B);
        browseButton.setActionCommand(BROWSE);
        browseButton.addActionListener(this);
        
        
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
        mainPanel.add(browseButton, gbc);
        
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
    
    private File setImageToOpen() {
        JFileChooser chooser = new JFileChooser();
        int filterType = -1;

        // lets set the user defined file filters
        ViewImageFileFilter.setUserDefinedExtensions();

        // address of TIFF header of second image in file if present
        // for LSM510 image files

        // set the filter type to the preferences saved filter
        int filter = 0;

        try {
            filter = Integer.parseInt(Preferences.getProperty(Preferences.PREF_FILENAME_FILTER));
        } catch (NumberFormatException nfe) {

            // an invalid value was set in preferences -- so don't use it!
            filter = -1;
        }
        
        try {

            // chooser = new JFileChooser();
            if (ViewUserInterface.getReference().getDefaultDirectory() != null) {
                File file = new File(ViewUserInterface.getReference().getDefaultDirectory());

                if (file != null) {
                    chooser.setCurrentDirectory(file);
                } else {
                    chooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
                }
            } else {
                chooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
            }

            chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.GEN));
            chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.TECH));
            chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.MICROSCOPY));
            chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.MISC));

            if (filter != -1) {
                // it seems that the set command adds the filter again...
                // chooser.addChoosableFileFilter(new ViewImageFileFilter(filter));

                // if filter is something we already added, then remove it before
                // setting it..... (kludgy, kludgy....)
                javax.swing.filechooser.FileFilter found = ViewOpenFileUI.findFilter(chooser, filter);

                if (found != null) {
                    chooser.removeChoosableFileFilter(found);
                }

                // initially set to the preferences
                chooser.setFileFilter(new ViewImageFileFilter(filter));
            }

            // but if the filterType was set, then use that instead
            // set the current filter to filterType
            if (filterType != -1) { // filterType has been set

                // don't add this filter twice --- if it's there, then remove it
                javax.swing.filechooser.FileFilter found2 = ViewOpenFileUI.findFilter(chooser, filterType);

                if (found2 != null) {
                    chooser.removeChoosableFileFilter(found2);
                }

                // set the current file filter
                chooser.setFileFilter(new ViewImageFileFilter(filterType));
            }

            chooser.setDialogTitle("Open Image");

            int returnValue = chooser.showOpenDialog(ViewUserInterface.getReference().getMainFrame());

            if (returnValue == JFileChooser.APPROVE_OPTION) {
                imageFile = chooser.getSelectedFile();
                String directory = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;
                ViewUserInterface.getReference().setDefaultDirectory(directory);
                return imageFile;
            } 
            	
            return null;
        } catch (OutOfMemoryError e) {
            MipavUtil.displayError("Out of memory!");

            return null;
        }
    }
    
    private void loadImage() throws Exception {
    	ModelRGB modelRGB = null;
    	String fileName = new String();
    	String directory = new String();
    	ModelLUT LUT = null;
    	try {
            FileIO fileIO = new FileIO();
            fileName = imageFile.getName();
            directory = imageFile.getParent();
            image = fileIO.readImage(fileName, imageFile.getParent(), false, null);

            LUT = fileIO.getModelLUT();
            modelRGB = fileIO.getModelRGB();

        } catch (OutOfMemoryError e) {
            MipavUtil.displayError("Out of memory!");
        }

        try {

        	ViewJFrameImage imageFrame = new ViewJFrameImage(image, LUT, ViewUserInterface.getReference().getNewFrameLocation());
        	if (modelRGB != null) {
                imageFrame.setRGBTA(modelRGB);
            }
        } catch (OutOfMemoryError e) {
            MipavUtil.displayError("Out of memory!");
        }

        Preferences.setLastImage(directory + fileName, image.getFileInfo()[0].getMultiFile(), image.getNDims());

        // updates menubar for each image
        Vector<Frame> imageFrames = ViewUserInterface.getReference().getImageFrameVector();

        if (imageFrames.size() < 1) {
        	ViewUserInterface.getReference().buildMenu();
        	ViewUserInterface.getReference().setControls();
        } else {
        	ViewUserInterface.getReference().buildMenu();

            for (int i = 0; i < imageFrames.size(); i++) {

                if (imageFrames.elementAt(i) instanceof ViewJFrameImage) {
                    ((ViewJFrameImage) (imageFrames.elementAt(i))).updateMenubar();
                }
            }

            ViewUserInterface.getReference().getActiveImageFrame().setControls();
        } 
    }
}
