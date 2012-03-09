package nibib.spim;
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
import java.util.Collection;
import java.util.Collections;
import java.util.Enumeration;
import java.util.Iterator;

import javax.swing.*;

import nibib.spim.PlugInAlgorithmGenerateFusion541b.SampleMode;


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
public class PlugInDialogGenerateFusion541b extends JDialogScriptableBase implements AlgorithmInterface {
    
    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /**declare UID */

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Result image. */
    private ModelImage resultImage = null;

    /** This source image is typically set by the constructor */
    private ModelImage image; // 
    
    /** This is your algorithm */
    private PlugInAlgorithmGenerateFusion541b generateFusionAlgo = null;

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

    private String spimAFileDir;

    private String baseImage;

    private JTextField spimBFileLocText;

    private JTextField resXText, resYText, resZText;

    private JTextField concurrentNumText;

    private JCheckBox doThresholdBox;

    private JTextField thresholdIntensityText;

    private boolean doThreshold;

    private double resX;

    private double resY;

    private double resZ;

    private int concurrentNum;

    private String spimBFileDir;

    private double thresholdIntensity;

    private JTextField xMovementText;

    private JTextField yMovementText;

    private JTextField zMovementText;

    private int xMovement;

    private int yMovement;

    private int zMovement;

    private SampleMode mode;
    
    private JComboBox modeOption;

  //~ Constructors ---------------------------------------------------------------------------------------------------
    
    /**
     * Constructor used for instantiation during script execution (required for dynamic loading).
     */
    public PlugInDialogGenerateFusion541b() { }

    /**
     * Creates new dialog for kidney segmentation from an abdominal cavity image using a plugin.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public PlugInDialogGenerateFusion541b(Frame theParentFrame, ModelImage im) {
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
        if (algorithm instanceof PlugInAlgorithmGenerateFusion541b) {
            Preferences.debug("Elapsed: " + algorithm.getElapsedTime());
            
            if ((generateFusionAlgo.isCompleted() == true)) {
                Collection<ModelImage> list = generateFusionAlgo.getResultImageList();
                synchronized(list) {
                    Iterator<ModelImage> itr = list.iterator();
                    while(itr.hasNext()) {
                        new ViewJFrameImage(itr.next());
                    }
                }
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
            
            generateFusionAlgo = new PlugInAlgorithmGenerateFusion541b(image, doSubsample, doInterImages, doGeoMean, doAriMean, doThreshold, 
                                                                         resX, resY, resZ, concurrentNum, thresholdIntensity,
                                                                                mtxFileLoc, middleSlice, baseImageAr, transformImageAr, 
                                                                                xMovement, yMovement, zMovement, mode);

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
    	doThreshold = scriptParameters.getParams().getBoolean("do_threshold");
    	
    	middleSlice = scriptParameters.getParams().getInt("middleSlice");
    	
    	mtxFileLoc = scriptParameters.getParams().getFile("mtxFileLoc");
    	spimAFileDir = scriptParameters.getParams().getFile("spimAFileDir");
    	spimBFileDir = scriptParameters.getParams().getFile("spimBFileDir");
    	
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
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_threshold", doThreshold));
        
        scriptParameters.getParams().put(ParameterFactory.newParameter("middleSlice", middleSlice));
       
        scriptParameters.getParams().put(ParameterFactory.newParameter("mtxFileLoc", mtxFileLoc));
        scriptParameters.getParams().put(ParameterFactory.newParameter("spimAFileDir", spimAFileDir));
        scriptParameters.getParams().put(ParameterFactory.newParameter("spimBFileDir", spimBFileDir));
        
        scriptParameters.getParams().put(ParameterFactory.newParameter("baseImageText", baseImageText));
    } //end storeParamsFromGUI()
   
    private void init() {
        setForeground(Color.black);
        setTitle("Generate fusion 541b");
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
        
        JPanel transformPanel = new JPanel();
        FlowLayout transformFlow = new FlowLayout(FlowLayout.LEFT);
        transformPanel.setLayout(transformFlow);
        
        transformImageText = gui.buildField("Transform image ", "SPIMA");
        transformPanel.add(transformImageText.getParent());
        
        baseImageText = gui.buildField(" to image ", "SPIMB");
        transformPanel.add(baseImageText.getParent());
        
        mtxPanel.add(transformPanel, gbc);
        gbc.gridy++;
        
        JLabel dirMove = new JLabel("Enter post-transformation translation (needed since padding occurs)");
        mtxPanel.add(dirMove, gbc);
        gbc.gridy++;
        
        JPanel movementPanel = new JPanel();
        FlowLayout movementFlow = new FlowLayout(FlowLayout.LEFT);
        movementPanel.setLayout(movementFlow);
        
        xMovementText = gui.buildIntegerField("X: ", 0);
        movementPanel.add(xMovementText.getParent());
        
        yMovementText = gui.buildIntegerField("Y: ", 0);
        movementPanel.add(yMovementText.getParent());
        
        zMovementText = gui.buildIntegerField("Z: ", 0);
        movementPanel.add(zMovementText.getParent());
        
        mtxPanel.add(movementPanel, gbc);
        
        gbc.gridx = 0;
        gbc.gridy = 0;
        
        mainPanel.add(mtxPanel, gbc);
        
        JPanel algOptionPanel = new JPanel(new GridBagLayout());
        algOptionPanel.setForeground(Color.black);
        algOptionPanel.setBorder(MipavUtil.buildTitledBorder("Algorithm options"));
        
        JLabel resLabel = new JLabel("Initial resolutions (um): ");
        algOptionPanel.add(resLabel, gbc);
        gbc.gridy++;
        
        JPanel resPanel = new JPanel(new GridBagLayout());
        resPanel.setForeground(Color.black);
        FlowLayout flow = new FlowLayout(FlowLayout.LEFT);
        resPanel.setLayout(flow);
        
        resXText = gui.buildDecimalField("X: ", .1625);
        resPanel.add(resXText.getParent());
        
        resYText = gui.buildDecimalField("Y: ", .1625);
        resPanel.add(resYText.getParent());
        
        resZText = gui.buildDecimalField("Z: ", .5);
        resPanel.add(resZText.getParent());
        
        algOptionPanel.add(resPanel, gbc);
        gbc.gridy++;
        gbc.gridx = 0;
        
        modeOption =  gui.buildComboBox("Sampling mode", SampleMode.values());
        algOptionPanel.add(modeOption.getParent(), gbc);
        gbc.gridy++;
        
        concurrentNumText = gui.buildIntegerField("Number of concurrent fusions: ", 
                                                    (Runtime.getRuntime().availableProcessors() - 2) > 1 ? Runtime.getRuntime().availableProcessors()-2 : 1);
        algOptionPanel.add(concurrentNumText.getParent(), gbc);
        gbc.gridy++;
        
        doThresholdBox = gui.buildCheckBox("Threshold noise", false);
        algOptionPanel.add(doThresholdBox.getParent(), gbc);
        gbc.gridy++;
        
        final JPanel thresholdPanel = new JPanel(new GridBagLayout());
        GridBagConstraints gbcThreshold = new GridBagConstraints();
        thresholdPanel.setForeground(Color.black);
        
        gbcThreshold.gridx = 0;
        gbcThreshold.gridy = 0;
        
        thresholdIntensityText = gui.buildDecimalField("Threshold value", 10);
        thresholdPanel.add(thresholdIntensityText.getParent(), gbcThreshold);
        
        algOptionPanel.add(thresholdPanel, gbc);
        
        doThresholdBox.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                thresholdPanel.setVisible(doThresholdBox.isSelected());
            }
        });
        
        thresholdPanel.setVisible(doThresholdBox.isSelected());
        
        
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
      
        arithmeticMeanBox = gui.buildCheckBox("Show arithmetic mean", true);
        outputPanel.add(arithmeticMeanBox.getParent(), gbc);
        gbc.gridy++;
        
        geometricMeanBox = gui.buildCheckBox("Show geometric mean", false);
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
        doThreshold = doThresholdBox.isSelected();
	    
	    try {
		    middleSlice = Integer.valueOf(middleSliceText.getText()).intValue();
		    
		    concurrentNum = Integer.valueOf(concurrentNumText.getText()).intValue();
		    
		    thresholdIntensity = Double.valueOf(thresholdIntensityText.getText()).doubleValue();
		    
		    resX = Double.valueOf(resXText.getText()).doubleValue();
		    resY = Double.valueOf(resYText.getText()).doubleValue();
		    resZ = Double.valueOf(resZText.getText()).doubleValue();
		    
		    xMovement = Integer.valueOf(xMovementText.getText()).intValue();
		    yMovement = Integer.valueOf(yMovementText.getText()).intValue();
		    zMovement = Integer.valueOf(zMovementText.getText()).intValue();
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
	    
	    spimAFileDir = spimAFileLocText.getText();
	    spimBFileDir = spimBFileLocText.getText();
	    baseImage = baseImageText.getText();
	    
	    mode = (SampleMode) modeOption.getSelectedItem();
	    
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
            File fA = new File(spimAFileDir);
            File fB = new File(spimBFileDir);
            boolean containsFiles = false;
            for(File fTry : fA.listFiles()) {
                if(fTry.getName().contains(baseImage)) {
                    containsFiles = true;
                    continue;
                }
            }
            if(!containsFiles) {
                fB = new File(spimAFileDir);
                fA = new File(spimBFileDir);
            }
            
            if(!fA.exists() || !fA.isDirectory()) {
                MipavUtil.displayError("Spim file directories could not be found");
                return false;
            }
    search:   for(File fTry : fA.listFiles()) {
                 String s = fTry.getName(); 
                 if(!s.contains(".mtx")) {
                    if(s.contains(baseImage)) {
                        String[] subSection = s.split(baseImage);
            searchSub:  for(File fTrySub : fB.listFiles()) {
                            String sSub = fTrySub.getName();
                            if(!s.contains(".mtx")) {
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
            }
        } catch(Exception e) {
            MipavUtil.displayError("Invalid spim directories");
            return false;
        }
        
        baseImageAr = baseImageList.toArray(new File[baseImageList.size()]);
        transformImageAr = transformImageList.toArray(new File[transformImageList.size()]);
        
        return true;
    }
}
