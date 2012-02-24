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

import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.ParameterFactory;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.GuiBuilder;
import gov.nih.mipav.view.dialogs.JDialogScriptableBase;

import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Enumeration;

import javax.swing.*;

/**
 * Class for performing image fusion based on reference image and transformation matrix.  Option to output geometric/arithmetic mean.
 * 
 * @version  June 4, 2010
 * @see      JDialogBase
 * @see      AlgorithmInterface
 *
 * @author Justin Senseney (SenseneyJ@mail.nih.gov)
 * @see http://mipav.cit.nih.gov
 */
public class PlugInDialogGenerateFusion541a extends JDialogScriptableBase implements AlgorithmInterface {
    
    
    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /**declare UID */

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Result image. */
    private ModelImage resultImage = null;

    /** This source image is typically set by the constructor */
    private ModelImage image; // 
    
    /** This is your algorithm */
    private PlugInAlgorithmGenerateFusion541a generateFusionAlgo = null;

    private JCheckBox doInterImagesCheckBox;

    private JTextField mtxFileLocText;

    private JTextField spimAFileLocText;

    private JCheckBox geometricMeanBox;

    private JCheckBox arithmeticMeanBox;

    private JCheckBox interImagesBox;

    private JTextField middleSliceText;

    private JPanel okCancelPanel;

    private JTextField transformImageText;

    private JTextField baseImageText;

    private JCheckBox doSubsampleBox;

    private String mtxFileLoc;

    private File[] baseImageAr;

    private File[] transformImageAr;

    private boolean doSubsample;

    private boolean doInterImages;

    private boolean doGeoMean;

    private boolean doAriMean;

    private int middleSlice;

    private String spimFileDir;

    private String baseImage;

    private JTextField spimBFileLocText;

    private JTextField resXText;

    private Component resYText;

    private Component resZText;

    private JTextField concurrentNumText;

    private JCheckBox doThresholdBox;

  //~ Constructors ---------------------------------------------------------------------------------------------------
    
    /**
     * Constructor used for instantiation during script execution (required for dynamic loading).
     */
    public PlugInDialogGenerateFusion541a() { }

    /**
     * Creates new dialog for kidney segmentation from an abdominal cavity image using a plugin.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public PlugInDialogGenerateFusion541a(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);

        image = im;
        init();
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
    } // end actionPerformed()

    /**
     * This method is required if the AlgorithmPerformed interface is implemented. It is called by the algorithm when it
     * has completed or failed to to complete, so that the dialog can be display the result image and/or clean up.
     *
     * @param  algorithm  Algorithm that caused the event.
     */
    public void algorithmPerformed(AlgorithmBase algorithm) {
        if (algorithm instanceof PlugInAlgorithmGenerateFusion541a) {
            Preferences.debug("Elapsed: " + algorithm.getElapsedTime());
            
            if ((generateFusionAlgo.isCompleted() == true)) {
                if(doInterImages) {
                    
                }
                
            } 

            if (generateFusionAlgo.isCompleted()) {                
                insertScriptLine();
            }

            if (generateFusionAlgo != null) {
                generateFusionAlgo.finalize();
                generateFusionAlgo = null;
            }

            dispose();
        }

    } // end algorithmPerformed()

    
    /**
     * Once all the necessary variables are set, call the kidney segmentation algorithm
     */
    protected void callAlgorithm() {

        try {
            
            generateFusionAlgo = new PlugInAlgorithmGenerateFusion541a(image, doSubsample, doInterImages, doGeoMean, doAriMean, middleSlice, baseImageAr, transformImageAr);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed or failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            generateFusionAlgo.addListener(this);
            createProgressBar(image.getImageName(), " ...", generateFusionAlgo);

            setVisible(false); // Hide dialog

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still
                // have user interface work fast.
                if (generateFusionAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                generateFusionAlgo.run();
            }
        } catch (OutOfMemoryError x) {
            if (resultImage != null) {
                resultImage.disposeLocal(); // Clean up memory of result image
                resultImage = null;
            }

            MipavUtil.displayError("Generic algorithm: unable to allocate enough memory");

            return;
        }

    } // end callAlgorithm()

    /**
     * Used in turning your plugin into a script
     */
    protected void setGUIFromParams() {
    	image = scriptParameters.retrieveInputImage();

    	doAriMean = scriptParameters.getParams().getBoolean("do_arithmetic");
    	doGeoMean = scriptParameters.getParams().getBoolean("do_geometric");
    	doInterImages = scriptParameters.getParams().getBoolean("do_interImages");
    	doSubsample = scriptParameters.getParams().getBoolean("do_subsample");
    	
    	middleSlice = scriptParameters.getParams().getInt("middleSlice");
    	mtxFileLoc = scriptParameters.getParams().getFile("mtxFileLoc");
    	spimFileDir = scriptParameters.getParams().getFile("spimFileDir");
    	baseImage = scriptParameters.getParams().getString("baseImage");
    	
    	populateFileLists();
    	
    } //end setGUIFromParams()

    /**
     * Used in turning your plugin into a script
     */
    protected void storeParamsFromGUI() throws ParserException {
    	scriptParameters.storeInputImage(image);
   
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_arithmetic", doAriMean));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_geometric", doGeoMean));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_interImages", doInterImages));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_subsample", doSubsample));
        scriptParameters.getParams().put(ParameterFactory.newParameter("middleSlice", middleSlice));
        scriptParameters.getParams().put(ParameterFactory.newParameter("mtxFileLoc", mtxFileLoc));
        scriptParameters.getParams().put(ParameterFactory.newParameter("spimFileDir", spimFileDir));
        scriptParameters.getParams().put(ParameterFactory.newParameter("baseImageText", baseImageText));
    } //end storeParamsFromGUI()
   
    private void init() {
        setForeground(Color.black);
        setTitle("Generate fusion 541a");
        try {
            setIconImage(MipavUtil.getIconImage("divinci.gif"));
        } catch (FileNotFoundException e) {
            Preferences.debug("Failed to load default icon", Preferences.DEBUG_MINOR);
        }
        
        GuiBuilder gui = new GuiBuilder(this);

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

        JPanel mtxPanel = new JPanel(new GridBagLayout());
        mtxPanel.setForeground(Color.black);
        mtxPanel.setBorder(buildTitledBorder("File information"));
        
        mtxFileLocText = gui.buildFileField("Matrix file location: ", "", false, JFileChooser.FILES_ONLY);
        mtxPanel.add(mtxFileLocText.getParent(), gbc);
        gbc.gridy++;
        
        spimAFileLocText = gui.buildFileField("Directory containing SPIMA: ", "", false, JFileChooser.DIRECTORIES_ONLY);
        mtxPanel.add(spimAFileLocText.getParent(), gbc);
        gbc.gridy++;
        
        spimBFileLocText = gui.buildFileField("Directory containing SPIMB: ", "", false, JFileChooser.DIRECTORIES_ONLY);
        mtxPanel.add(spimBFileLocText.getParent(), gbc);
        gbc.gridy++;
        
        transformImageText = gui.buildField("Transform image ", "SPIMA");
        mtxPanel.add(transformImageText.getParent(), gbc);
        gbc.gridx++;
        
        baseImageText = gui.buildField(" to image ", "SPIMB");
        mtxPanel.add(baseImageText.getParent(), gbc);
        gbc.gridx = 0;
        gbc.gridy = 0;
        
        mtxFileLocText.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                try {
                    File f = new File(mtxFileLocText.getText());
                    if(f.getParentFile().isDirectory() && !(spimAFileLocText.getText().length() > 0) && !(spimBFileLocText.getText().length() > 0)) {
                        spimAFileLocText.setText(f.getParent());
                        spimBFileLocText.setText(f.getParent());
                    }
                } catch(Exception e1) {
                    e1.printStackTrace();
                }
                
            }
        });
        
        mainPanel.add(mtxPanel, gbc);
        
        JPanel algOptionPanel = new JPanel(new GridBagLayout());
        algOptionPanel.setForeground(Color.black);
        algOptionPanel.setBorder(MipavUtil.buildTitledBorder("Algorithm options"));
        
        JLabel resLabel = new JLabel("Initial resolutions (um): ");
        algOptionPanel.add(resLabel, gbc);
        gbc.gridy++;
        
        resXText = gui.buildDecimalField("X: ", .1625);
        algOptionPanel.add(resXText.getParent(), gbc);
        gbc.gridx++;
        
        resYText = gui.buildDecimalField("Y: ", .1625);
        algOptionPanel.add(resYText.getParent(), gbc);
        gbc.gridx++;
        
        resZText = gui.buildDecimalField("Z: ", .5);
        algOptionPanel.add(resZText.getParent(), gbc);
        gbc.gridy++;
        gbc.gridx = 0;
        
        concurrentNumText = gui.buildIntegerField("Number of concurrent fusions: ", 
                                                    (Runtime.getRuntime().availableProcessors() - 2) > 1 ? Runtime.getRuntime().availableProcessors() : 1);
        algOptionPanel.add(concurrentNumText.getParent(), gbc);
        gbc.gridy++;
        
        doThresholdBox = gui.buildCheckBox("Threshold noise", false);
        algOptionPanel.add(doThresholdBox.getParent(), gbc);
        gbc.gridy++;
        
        JPanel thresholdPanel = new JPanel(new GridBagLayout());
        GridBagConstraints gbcThreshold = new GridBagConstraints();
        thresholdPanel.setForeground(Color.black);
        
        doThresholdBox.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                if(doThresholdBox.isSelected()) {
                    
                }
            }
        });
        
        
        gbc.gridy = 1;
        gbc.gridx = 0;
        mainPanel.add(algOptionPanel, gbc);
        
        JPanel outputPanel = new JPanel(new GridBagLayout());
        outputPanel.setForeground(Color.black);
        outputPanel.setBorder(MipavUtil.buildTitledBorder("Output options"));
        
        gbc.gridy = 0;
        middleSliceText = gui.buildIntegerField("Middle slice of transformation: ", 0);
        outputPanel.add(middleSliceText.getParent(), gbc);
        gbc.gridy++;  
        
        gbc.gridy++;
        doSubsampleBox = gui.buildCheckBox("Do subsampling to match images", false);
        outputPanel.add(doSubsampleBox.getParent(), gbc);
        gbc.gridy++;
      
        arithmeticMeanBox = gui.buildCheckBox("Show arithmetic mean", false);
        outputPanel.add(arithmeticMeanBox.getParent(), gbc);
        gbc.gridy++;
        
        geometricMeanBox = gui.buildCheckBox("Show geometric mean", true);
        outputPanel.add(geometricMeanBox.getParent(), gbc);
        gbc.gridy++;
        
        
        
        interImagesBox = gui.buildCheckBox("Show transformed images", true);
        outputPanel.add(interImagesBox.getParent(), gbc);
        
        gbc.gridy = 2;
        mainPanel.add(outputPanel, gbc);
        
        gbc.gridy++;
        okCancelPanel = gui.buildOKCancelPanel();
        mainPanel.add(okCancelPanel, gbc);

        getContentPane().add(mainPanel, BorderLayout.CENTER);

        pack();
        setVisible(true);
        setResizable(false);
        System.gc();
        
    } // end init()
    

    /**
     * This method could ensure everything in your dialog box has been set correctly
     * 
     * @return
     */
	private boolean setVariables() {
	    doGeoMean = geometricMeanBox.isSelected();
        doAriMean = arithmeticMeanBox.isSelected();
        doInterImages = interImagesBox.isSelected();
        doSubsample = doSubsampleBox.isSelected();
	    
	    try {
		    middleSlice = Integer.valueOf(middleSliceText.getText()).intValue();
		} catch(NumberFormatException nfe) {
            MipavUtil.displayError("Input error, enter numerical values only.");
            return false;
        }
	      
	    try {
	        mtxFileLoc = mtxFileLocText.getText();
	        File f = new File(mtxFileLoc);
	        if(!f.exists()) {
	            MipavUtil.displayError("Matrix file could not be found.");
	            return false;
	        }
	    } catch(Exception e) {
	        MipavUtil.displayError("Invalid matrix file.");
	        return false;
	    }
	    
	    spimFileDir = spimAFileLocText.getText();
	    baseImage = baseImageText.getText();
	    
	    if(!populateFileLists()) {
	        return false;
	    }
		
	    StringBuffer transformMessage = new StringBuffer();
	    for(int i=0; i<baseImageAr.length; i++) {
	        transformMessage.append("Image ").append(transformImageAr[i].getName()).append(" transformed to ").append(baseImageAr[i].getName()).append("\n");
	    }
	    int returnOption = JOptionPane.showConfirmDialog(this, "Proceed with the following operations?\n"+transformMessage, "Algorithm run confirm", JOptionPane.YES_NO_OPTION);
	    if(returnOption == JOptionPane.NO_OPTION) {
	        return false;
	    }
	    
		return true;
	} //end setVariables()

    private boolean populateFileLists() {
        ArrayList<File> baseImageList = new ArrayList<File>();
        ArrayList<File> transformImageList = new ArrayList<File>();
        try {
            File f = new File(spimFileDir);
            if(!f.exists() || !f.isDirectory()) {
                MipavUtil.displayError("Spim file directory could not be found");
                return false;
            }
    search:   for(File fTry : f.listFiles()) {
                 String s = fTry.getName(); 
                 if(!s.contains(".mtx")) {
                    if(s.contains(baseImage)) {
                        String[] subSection = s.split(baseImage);
            searchSub:  for(File fTrySub : f.listFiles()) {
                            String sSub = fTrySub.getName();
                            for(String subParts : subSection) {
                                if(!sSub.contains(subParts)) {
                                    continue searchSub;
                                }
                            }
                            //matching subsections have been found
                            baseImageList.add(fTry);
                            transformImageList.add(fTrySub);
                            continue search;
                        }
                    }
                }
            }
        } catch(Exception e) {
            MipavUtil.displayError("Invalid spim directory");
            return false;
        }
        
        baseImageAr = baseImageList.toArray(new File[baseImageList.size()]);
        transformImageAr = transformImageList.toArray(new File[transformImageList.size()]);
        
        return true;
    }
}
