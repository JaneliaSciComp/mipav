package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.filters.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.util.*;

import javax.swing.*;


/**
 * Dialog to get user input, then call the algorithm.
 *
 * @version  0.4 April 14, 2009
 * @author   William Gandler
 * @see      AlgorithmHaralickTexture
 */
public class JDialogHaralickTexture extends JDialogScriptableBase
        implements AlgorithmInterface, DialogDefaultsInterface, ActionDiscovery, ScriptableActionInterface
     {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 2408406330754526954L;
    
    /** Red channel. */
    private static final int RED_OFFSET = 1;

    /** Green channel. */
    private static final int GREEN_OFFSET = 2;

    /** Blue channel. */
    private static final int BLUE_OFFSET = 3;

    //~ Instance fields ------------------------------------------------------------------------------------------------
    
    /** DOCUMENT ME! */
    private JPanel colorPanel;
    
    /** DOCUMENT ME! */
    private ButtonGroup colorGroup;
    
    /** DOCUMENT ME! */
    private JRadioButton redButton;
    
    /** DOCUMENT ME! */
    private JRadioButton greenButton;
    
    /** DOCUMENT ME! */
    private JRadioButton blueButton;
    
    /** DOCUMENT ME! */
    private int RGBOffset = RED_OFFSET;

    /** DOCUMENT ME! */
    private boolean asm = false;

    /** DOCUMENT ME! */
    private JCheckBox asmCheckBox;

    /** DOCUMENT ME! */
    private boolean contrast = true;

    /** DOCUMENT ME! */
    private JCheckBox contrastCheckBox;

    /** DOCUMENT ME! */
    private boolean correlation = false;

    /** DOCUMENT ME! */
    private JCheckBox correlationCheckBox;

    /** DOCUMENT ME! */
    private boolean dissimilarity = false;

    /** DOCUMENT ME! */
    private JCheckBox dissimilarityCheckBox;

    /** DOCUMENT ME! */
    private boolean energy = false;

    /** DOCUMENT ME! */
    private JCheckBox energyCheckBox;

    /** DOCUMENT ME! */
    private boolean entropy = false;

    /** DOCUMENT ME! */
    private JCheckBox entropyCheckBox;

    /** DOCUMENT ME! */
    private boolean ew = false;

    /** DOCUMENT ME! */
    private JCheckBox ewCheckBox;

    /** DOCUMENT ME! */
    private boolean homogeneity = false;

    /** DOCUMENT ME! */
    private JCheckBox homogeneityCheckBox;

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** DOCUMENT ME! */
    private JCheckBox invariantCheckBox;

    /** DOCUMENT ME! */
    private boolean invariantDir = false;

    /** DOCUMENT ME! */
    private boolean inverseOrder1 = false;

    /** DOCUMENT ME! */
    private JCheckBox inverseOrder1CheckBox;

    /** DOCUMENT ME! */
    private JLabel labelOffsetDistance;

    /** DOCUMENT ME! */
    private JLabel labelWindowSize;

    /** DOCUMENT ME! */
    private boolean maxProbability = false;

    /** DOCUMENT ME! */
    private JCheckBox maxProbabilityCheckBox;

    /** DOCUMENT ME! */
    private boolean mean = false;

    /** DOCUMENT ME! */
    private JCheckBox meanCheckBox;

    /** DOCUMENT ME! */
    private boolean nesw = false;

    /** DOCUMENT ME! */
    private JCheckBox neswCheckBox;

    /** DOCUMENT ME! */
    private boolean ns = true;

    /** DOCUMENT ME! */
    private JCheckBox nsCheckBox;

    /** DOCUMENT ME! */
    private int numDirections = 0;

    /** DOCUMENT ME! */
    private int numOperators = 0;

    /** DOCUMENT ME! */
    private int offsetDistance;

    /** DOCUMENT ME! */
    private ModelImage[] resultImage = null; // result image

    /** DOCUMENT ME! */
    private int resultNumber;

    /** DOCUMENT ME! */
    private JPanel scalePanel;

    /** DOCUMENT ME! */
    private boolean senw = false;

    /** DOCUMENT ME! */
    private JCheckBox senwCheckBox;

    /** DOCUMENT ME! */
    private boolean standardDeviation = false;

    /** DOCUMENT ME! */
    private JCheckBox standardDeviationCheckBox;

    /** DOCUMENT ME! */
    private JTextField textOffsetDistance;

    /** DOCUMENT ME! */
    private AlgorithmHaralickTexture textureAlgo;

    /** DOCUMENT ME! */
    private JTextField textWindowSize;

    /** DOCUMENT ME! */
    private boolean variance = false;

    /** DOCUMENT ME! */
    private JCheckBox varianceCheckBox;
    
    private boolean shade = false;
    
    private JCheckBox shadeCheckBox;
    
    private boolean promenance = false;
    
    private JCheckBox promenanceCheckBox;

    /** DOCUMENT ME! */
    private int windowSize;
    
    private JLabel labelRescaling;
    
    private JTextField textRescaling;
    
    /** Number of grey levels used if data must be rescaled */
    private int greyLevels;
    
    private JCheckBox concatenateCheckBox;
    
    /** If true, only one result image with the original source concatenated with the
     *  calculated features.
     */
    private boolean concatenate = false;
    
    private JCheckBox zscoreCheckBox;
    
    /** If true, produce z score = (value - mean)/(standard deviation) output of Haralick features
     */
    private boolean zscore = false;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation.
     */
    public JDialogHaralickTexture() { }


    /**
     * Creates a new JDialogHaralickTexture object.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogHaralickTexture(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        image = im;
        init();
        loadDefaults();
        setVisible(true);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

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
        } else if (command.equals("Cancel")) {
            dispose();
        } else if (command.equals("Help")) {
            //MipavUtil.showHelp("Haral1001");
            MipavUtil.showWebHelp("Filters_(Spatial):_Haralick_Texture");
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
        int i;
        ViewJFrameImage[] imageFrame = new ViewJFrameImage[resultNumber];

        if (algorithm instanceof AlgorithmHaralickTexture) {
            image.clearMask();

            if ((textureAlgo.isCompleted() == true) && (resultImage != null)) {

                // The algorithm has completed and produced a new image to be displayed.
                // Take resultImage out of array form or null pointer errors can
                // result in one of the resultImages after another of the resultImages
                // has been deleted.
            	
            	
            	// save the completion status for later
            	setComplete(textureAlgo.isCompleted());

                for (i = 0; i < resultNumber; i++) {
                    updateFileInfo(image, resultImage[i]);
                    resultImage[i].clearMask();
                    
                    try {
                        imageFrame[i] = new ViewJFrameImage(resultImage[i], null, new Dimension(610, 200));
                    } catch (OutOfMemoryError error) {
                        System.gc();
                        JOptionPane.showMessageDialog(null, "Out of memory: unable to open new resultImage frame",
                                                      "Error", JOptionPane.ERROR_MESSAGE);
                    }

                }
            } else if (resultImage != null) {

                // algorithm failed but result image still has garbage
                for (i = 0; i < resultNumber; i++) {

                    if (resultImage[i] != null) {
                        resultImage[i].disposeLocal(); // Clean up memory of result image
                        resultImage[i] = null;
                    }
                }

                resultImage = null;
                System.gc();
            }
        }

        if (algorithm.isCompleted()) {
            insertScriptLine();
        }

        dispose();

    }

    /**
     * Construct a delimited string that contains the parameters to this algorithm.
     *
     * @param   delim  the parameter delimiter (defaults to " " if empty)
     *
     * @return  the parameter string
     */
    public String getParameterString(String delim) {

        if (delim.equals("")) {
            delim = " ";
        }

        String str = new String();
        if (image.isColorImage()) {
            str += RGBOffset + delim;
        }
        str += windowSize + delim;
        str += offsetDistance + delim;
        str += greyLevels + delim;
        str += ns + delim;
        str += nesw + delim;
        str += ew + delim;
        str += senw + delim;
        str += invariantDir + delim;
        str += contrast + delim;
        str += dissimilarity + delim;
        str += homogeneity + delim;
        str += inverseOrder1 + delim;
        str += asm + delim;
        str += energy + delim;
        str += maxProbability + delim;
        str += entropy + delim;
        str += mean + delim;
        str += variance + delim;
        str += standardDeviation + delim;
        str += correlation + delim;
        str += shade + delim;
        str += promenance + delim;
        str += concatenate + delim;
        str += zscore;

        return str;
    }

    /**
     * Accessor that returns the image.
     *
     * @return  The result image.
     */
    public ModelImage[] getResultImage() {
        return resultImage;
    }

    /**
     * Loads the default settings from Preferences to set up the dialog.
     */
    public void loadDefaults() {
        String defaultsString = Preferences.getDialogDefaults(getDialogName());

        if (defaultsString != null) {

            try {
                StringTokenizer st = new StringTokenizer(defaultsString, ",");
                textWindowSize.setText("" + MipavUtil.getInt(st));
                textOffsetDistance.setText("" + MipavUtil.getInt(st));
                textRescaling.setText("" + MipavUtil.getInt(st));
                nsCheckBox.setSelected(MipavUtil.getBoolean(st));
                neswCheckBox.setSelected(MipavUtil.getBoolean(st));
                ewCheckBox.setSelected(MipavUtil.getBoolean(st));
                senwCheckBox.setSelected(MipavUtil.getBoolean(st));
                invariantCheckBox.setSelected(MipavUtil.getBoolean(st));
                contrastCheckBox.setSelected(MipavUtil.getBoolean(st));
                dissimilarityCheckBox.setSelected(MipavUtil.getBoolean(st));
                homogeneityCheckBox.setSelected(MipavUtil.getBoolean(st));
                inverseOrder1CheckBox.setSelected(MipavUtil.getBoolean(st));
                asmCheckBox.setSelected(MipavUtil.getBoolean(st));
                energyCheckBox.setSelected(MipavUtil.getBoolean(st));
                maxProbabilityCheckBox.setSelected(MipavUtil.getBoolean(st));
                entropyCheckBox.setSelected(MipavUtil.getBoolean(st));
                meanCheckBox.setSelected(MipavUtil.getBoolean(st));
                varianceCheckBox.setSelected(MipavUtil.getBoolean(st));
                standardDeviationCheckBox.setSelected(MipavUtil.getBoolean(st));
                correlationCheckBox.setSelected(MipavUtil.getBoolean(st));
                shadeCheckBox.setSelected(MipavUtil.getBoolean(st));
                promenanceCheckBox.setSelected(MipavUtil.getBoolean(st));
                concatenateCheckBox.setSelected(MipavUtil.getBoolean(st));
                zscoreCheckBox.setSelected(MipavUtil.getBoolean(st));
            } catch (Exception ex) {

                // since there was a problem parsing the defaults string, start over with the original defaults
                Preferences.debug("Resetting defaults for dialog: " + getDialogName());
                Preferences.removeProperty(getDialogName());
            }
        }
    }

    /**
     * Saves the default settings into the Preferences file.
     */
    public void saveDefaults() {
        String defaultsString = new String(getParameterString(","));
        Preferences.saveDialogDefaults(getDialogName(), defaultsString);
    }

    /**
     * Accessor that sets if asm operator is calculated.
     *
     * @param  asm  boolean
     */
    public void setASM(boolean asm) {
        this.asm = asm;
    }

    /**
     * Accessor that sets if the contrast operator is calculated.
     *
     * @param  contrast  boolean
     */
    public void setContrast(boolean contrast) {
        this.contrast = contrast;
    }

    /**
     * Accessor that sets if gray level coordinate matrix correlation is calculated.
     *
     * @param  correlation  boolean
     */
    public void setCorrelation(boolean correlation) {
        this.correlation = correlation;
    }

    /**
     * Accessor that sets if dissimilarity operator is calculated.
     *
     * @param  dissimilarity  boolean
     */
    public void setDissimilarity(boolean dissimilarity) {
        this.dissimilarity = dissimilarity;
    }

    /**
     * Accessor that sets if energy operator is calculated.
     *
     * @param  energy  boolean
     */
    public void setEnergy(boolean energy) {
        this.energy = energy;
    }

    /**
     * Accessor that sets if entropy operator is calculated.
     *
     * @param  entropy  boolean
     */
    public void setEntropy(boolean entropy) {
        this.entropy = entropy;
    }

    /**
     * Accessor that sets if east west offset direction is calculated.
     *
     * @param  ew  boolean
     */
    public void setEW(boolean ew) {
        this.ew = ew;
    }

    /**
     * Accessor that sets if homogeneity operator is calculated.
     *
     * @param  homogeneity  boolean
     */
    public void setHomogeneity(boolean homogeneity) {
        this.homogeneity = homogeneity;
    }

    /**
     * Accessor that sets if spatially invariant offset direction is performed.
     *
     * @param  invariantDir  boolean
     */
    public void setInvariant(boolean invariantDir) {
        this.invariantDir = invariantDir;
    }

    /**
     * Accessor that sets if the inverse difference moment of order 1 operator is called.
     *
     * @param  inverseOrder1  boolean
     */
    public void setInverseOrder1(boolean inverseOrder1) {
        this.inverseOrder1 = inverseOrder1;
    }

    /**
     * Accessor that sets if maximum probability operator is calculated.
     *
     * @param  maxProbability  boolean
     */
    public void setMaxProbability(boolean maxProbability) {
        this.maxProbability = maxProbability;
    }

    /**
     * Accessor that set if the gray level coordinate matrix mean is calculated.
     *
     * @param  mean  boolean
     */
    public void setMean(boolean mean) {
        this.mean = mean;
    }

    /**
     * Accessor that sets if northeast-southest offset direction is calculated.
     *
     * @param  nesw  boolean
     */
    public void setNESW(boolean nesw) {
        this.nesw = nesw;
    }

    /**
     * Accessor that sets if north south offset direction is calculated.
     *
     * @param  ns  boolean
     */
    public void setNS(boolean ns) {
        this.ns = ns;
    }

    /**
     * Accessor that sets the offset distance.
     *
     * @param  offsetDistance  int
     */
    public void setOffsetDistance(int offsetDistance) {
        this.offsetDistance = offsetDistance;
    }

    /**
     * Accessor that sets if southeast-northwest offset direction is calculated.
     *
     * @param  senw  boolean
     */
    public void setSENW(boolean senw) {
        this.senw = senw;
    }

    /**
     * Accessor that sets if gray level coordinate matrix standard deviation is calculated.
     *
     * @param  standardDeviation  boolean
     */
    public void setStandardDeviation(boolean standardDeviation) {
        this.standardDeviation = standardDeviation;
    }

    /**
     * Accessor that sets if the gray level coordinate matrix variance is calculated.
     *
     * @param  variance  boolean
     */
    public void setVariance(boolean variance) {
        this.variance = variance;
    }
    
    /**
     * Accessor that sets if the cluster shade is calculated
     * @param shade
     */
    public void setShade(boolean shade) {
        this.shade = shade;
    }
    
    
    /**
     * Accessor that sets if the cluster promenance is calculated
     * @param promenance
     */
    public void setPromenance(boolean promenance) {
        this.promenance = promenance;
    }
    /**
     * Accessor that sets if there is only 1 result image with the original
     * source concatenated with the calculated features
     * @param concatenate
     */
    
    public void setConcatenate(boolean concatenate) {
        this.concatenate = concatenate;
    }
    
    public void setZscore(boolean zscore) {
        this.zscore = zscore;
    }

    /**
     * Accessor that sets the window size.
     *
     * @param  windowSize  int
     */
    public void setWindowSize(int windowSize) {
        this.windowSize = windowSize;
    }
    
    /**
     * Accessor that sets the number of grey levels if rescaling used
     * @param greyLevels
     */
    public void setGreyLevels(int greyLevels) {
        this.greyLevels = greyLevels;
    }
    
    /**
     * Accessor that sets the RGBOffset.
     *
     * @param  RGBoffset  DOCUMENT ME!
     */
    public void setRGBOffset(int RGBoffset) {
        this.RGBOffset = RGBoffset;
    }

    /**
     * Once all the necessary variables are set, call the Gaussian Haralick feature algorithm.
     */
    protected void callAlgorithm() {
        int i, j, k, index;
        if (concatenate) {
            resultNumber = 1;
        }
        else {
            resultNumber = numDirections * numOperators;
        }

        String[] name = null;
        String[] imageNameArray = null;
        String dirString = null;
        String opString = null;
        boolean doneNS = false;
        boolean doneNESW = false;
        boolean doneEW = false;
        boolean doneSENW = false;
        boolean doneInvariant = false;
        boolean doneContrast;
        boolean doneDissimilarity;
        boolean doneHomogeneity;
        boolean doneInverseOrder1;
        boolean doneASM;
        boolean doneEnergy;
        boolean doneMaxProbability;
        boolean doneEntropy;
        boolean doneMean;
        boolean doneVariance;
        boolean doneStandardDeviation;
        boolean doneCorrelation;
        boolean doneShade;
        boolean donePromenance;
        int zDim;
        int tDim;
        int newExtents[];

        try {
            resultImage = new ModelImage[resultNumber];
            name = new String[resultNumber];
            
            if (concatenate) {
                if (image.getNDims() == 2) {
                    name[0] = makeImageName(image.getImageName(), "_Haralick");
                    zDim = 1 + numDirections * numOperators;
                    newExtents = new int[3];
                    newExtents[0] = image.getExtents()[0];
                    newExtents[1] = image.getExtents()[1];
                    newExtents[2] = zDim;
                    resultImage[0] = new ModelImage(ModelStorageBase.FLOAT, newExtents, name[0]);
                    imageNameArray = new String[zDim];
                    imageNameArray[0] = name[0];
                } // if (image.getNDims() == 2)
                else { // image.getNDims() == 3
                    name[0] = makeImageName(image.getImageName(), "_Haralick");
                    tDim = 1 + numDirections * numOperators;
                    newExtents = new int[4];
                    newExtents[0] = image.getExtents()[0];
                    newExtents[1] = image.getExtents()[1];
                    newExtents[2] = image.getExtents()[2];
                    newExtents[3] = tDim;
                    resultImage[0] = new ModelImage(ModelStorageBase.FLOAT, newExtents, name[0]);
                    imageNameArray = new String[newExtents[2]*newExtents[3]];
                    for (i = 0; i < image.getExtents()[2]; i++) {
                        imageNameArray[i] = name[0];
                    }
                }
            } // if (concatenate)
            for (i = 0; i < numDirections; i++) {

                if (ns && (!doneNS)) {
                    dirString = "_ns";
                    doneNS = true;
                } else if (nesw && (!doneNESW)) {
                    dirString = "_nesw";
                    doneNESW = true;
                } else if (ew && (!doneEW)) {
                    dirString = "_ew";
                    doneEW = true;
                } else if (senw && (!doneSENW)) {
                    dirString = "_senw";
                    doneSENW = true;
                } else if (invariantDir && (!doneInvariant)) {
                    dirString = "_invariantDir";
                    doneInvariant = true;
                }
    
                doneContrast = false;
                doneDissimilarity = false;
                doneHomogeneity = false;
                doneInverseOrder1 = false;
                doneASM = false;
                doneEnergy = false;
                doneMaxProbability = false;
                doneEntropy = false;
                doneMean = false;
                doneVariance = false;
                doneStandardDeviation = false;
                doneCorrelation = false;
                doneShade = false;
                donePromenance = false;
    
                for (j = 0; j < numOperators; j++) {
                    index = j + (i * numOperators);
    
                    if (contrast && (!doneContrast)) {
                        opString = "_contrast";
                        doneContrast = true;
                    } else if (dissimilarity && (!doneDissimilarity)) {
                        opString = "_dissimilarity";
                        doneDissimilarity = true;
                    } else if (homogeneity && (!doneHomogeneity)) {
                        opString = "_homogeneity";
                        doneHomogeneity = true;
                    } else if (inverseOrder1 && (!doneInverseOrder1)) {
                        opString = "_inverseOrder1";
                        doneInverseOrder1 = true;
                    } else if (asm && (!doneASM)) {
                        opString = "_asm";
                        doneASM = true;
                    } else if (energy && (!doneEnergy)) {
                        opString = "_energy";
                        doneEnergy = true;
                    } else if (maxProbability && (!doneMaxProbability)) {
                        opString = "_maxProbability";
                        doneMaxProbability = true;
                    } else if (entropy && (!doneEntropy)) {
                        opString = "_entropy";
                        doneEntropy = true;
                    } else if (mean && (!doneMean)) {
                        opString = "_mean";
                        doneMean = true;
                    } else if (variance && (!doneVariance)) {
                        opString = "_variance";
                        doneVariance = true;
                    } else if (standardDeviation && (!doneStandardDeviation)) {
                        opString = "_standardDeviation";
                        doneStandardDeviation = true;
                    } else if (correlation && (!doneCorrelation)) {
                        opString = "_correlation";
                        doneCorrelation = true;
                    } else if (shade && (!doneShade)) {
                        opString = "_shade";
                        doneShade = true;
                    } else if (promenance && (!donePromenance)) {
                        opString = "_promenance";
                        donePromenance = true;
                    }
    
                    if (concatenate) {
                        if (image.getNDims() == 2) {
                            imageNameArray[index+1] = makeImageName(image.getImageName(), dirString + opString);
                        }
                        else {
                            for (k = 0; k < image.getExtents()[2]; k++) {
                                imageNameArray[(index+1)*image.getExtents()[2] + k] = 
                                    makeImageName(image.getImageName(), dirString + opString);
                            }
                        }
                    }
                    else {
                        name[index] = makeImageName(image.getImageName(), dirString + opString);
                        resultImage[index] = new ModelImage(ModelStorageBase.FLOAT, image.getExtents(), name[index]);
                    }

            } // for (j = 0; j < numOperators; j++)
        } // for (i = 0; i < numDirections; i++)
                
        if (concatenate) {
            resultImage[0].setImageNameArray(imageNameArray);
        }
        
        //System.out.println("This is " + numOperators);
        //System.out.println("That is " + numDirections);

            if (image.isColorImage()) {
                textureAlgo = new AlgorithmHaralickTexture(resultImage, image, RGBOffset, windowSize, offsetDistance,
                        greyLevels, ns, nesw, ew,
                        senw, invariantDir, contrast, dissimilarity, homogeneity,
                        inverseOrder1, asm, energy, maxProbability, entropy, mean,
                        variance, standardDeviation, correlation, shade, promenance,
                        concatenate, zscore);    
            }
            else {
                textureAlgo = new AlgorithmHaralickTexture(resultImage, image, windowSize, offsetDistance, greyLevels, ns, nesw, ew,
                                                           senw, invariantDir, contrast, dissimilarity, homogeneity,
                                                           inverseOrder1, asm, energy, maxProbability, entropy, mean,
                                                           variance, standardDeviation, correlation, shade, promenance,
                                                           concatenate, zscore);
            }

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed of failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            textureAlgo.addListener(this);
            createProgressBar(image.getImageName(), textureAlgo);

            // Hide dialog
            setVisible(false);

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (textureAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                textureAlgo.run();
            }
            

        } catch (OutOfMemoryError x) {

            if (resultImage != null) {

                for (i = 0; i < resultNumber; i++) {

                    if (resultImage[i] != null) {
                        resultImage[i].disposeLocal(); // Clean up memory of result image
                        resultImage[i] = null;
                    }
                }

                resultImage = null;
            }
            
         // save the completion status for later
            setComplete(textureAlgo.isCompleted());

            System.gc();
            MipavUtil.displayError("Dialog Haralick Texture: unable to allocate enough memory");

            return;
        }

    }

    /**
     * Store the result image in the script runner's image table now that the action execution is finished.
     */
    protected void doPostAlgorithmActions() {

        for (int i = 0; i < resultNumber; i++) {
            AlgorithmParameters.storeImageInRunner(getResultImage()[i]);
        }
    }

    /**
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {
        image = scriptParameters.retrieveInputImage();
        parentFrame = image.getParentFrame();

        if (image.isColorImage() && scriptParameters.getParams().containsParameter("RGB_offset")) {
            RGBOffset = scriptParameters.getParams().getInt("RGB_offset");
        } else if (image.isColorImage()) {
            throw new ParameterException("RGB_offset",
                                         "This parameter (RGB_offset) is required for the processing of color images.  Please re-record this script using a color image.");
        }
        setWindowSize(scriptParameters.getParams().getInt("window_size"));
        setOffsetDistance(scriptParameters.getParams().getInt("offset_distance"));
        setGreyLevels(scriptParameters.getParams().getInt("grey_levels"));
        setNS(scriptParameters.getParams().getBoolean("do_calc_north_south_dir"));
        setNESW(scriptParameters.getParams().getBoolean("do_calc_NE_SW_dir"));
        setEW(scriptParameters.getParams().getBoolean("do_calc_east_west_dir"));
        setSENW(scriptParameters.getParams().getBoolean("do_calc_SE_NW_dir"));
        setInvariant(scriptParameters.getParams().getBoolean("do_calc_invariant_dir"));
        setContrast(scriptParameters.getParams().getBoolean("do_calc_contrast"));
        setDissimilarity(scriptParameters.getParams().getBoolean("do_calc_dissimilarity"));
        setHomogeneity(scriptParameters.getParams().getBoolean("do_calc_homogeneity"));
        setInverseOrder1(scriptParameters.getParams().getBoolean("do_inverse_order_1"));
        setASM(scriptParameters.getParams().getBoolean("do_calc_ASM"));
        setEnergy(scriptParameters.getParams().getBoolean("do_calc_energy"));
        setMaxProbability(scriptParameters.getParams().getBoolean("do_calc_max_probability"));
        setEntropy(scriptParameters.getParams().getBoolean("do_calc_entropy"));
        setMean(scriptParameters.getParams().getBoolean("do_calc_mean"));
        setVariance(scriptParameters.getParams().getBoolean("do_calc_variance"));
        setStandardDeviation(scriptParameters.getParams().getBoolean("do_calc_standard_deviation"));
        setCorrelation(scriptParameters.getParams().getBoolean("do_calc_correlation"));
        setShade(scriptParameters.getParams().getBoolean("do_calc_shade"));
        setPromenance(scriptParameters.getParams().getBoolean("do_calc_promenance"));
        setConcatenate(scriptParameters.getParams().getBoolean("do_concatenate"));
        setZscore(scriptParameters.getParams().getBoolean("do_zscore"));

        if ((windowSize % 2) == 0) {
            throw new ParameterException("window_size", "Window size must not be even");
        }

        if (windowSize > image.getExtents()[0]) {
            throw new ParameterException("window_size",
                                         "Window size must not excced image width of " + image.getExtents()[0]);
        }

        if (windowSize > image.getExtents()[1]) {
            throw new ParameterException("window_size",
                                         "Window size must not excced image height of " + image.getExtents()[1]);
        }

        numDirections = getNumDirections();
        numOperators = getNumOperators();
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);

        for (int i = 0; i < resultNumber; i++) {
            scriptParameters.storeImageInRecorder(getResultImage()[i]);
        }

        if (image.isColorImage()) {
            scriptParameters.getParams().put(ParameterFactory.newParameter("RGB_offset", RGBOffset));
        }
        scriptParameters.getParams().put(ParameterFactory.newParameter("window_size", windowSize));
        scriptParameters.getParams().put(ParameterFactory.newParameter("offset_distance", offsetDistance));
        scriptParameters.getParams().put(ParameterFactory.newParameter("grey_levels", greyLevels));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_calc_north_south_dir", ns));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_calc_NE_SW_dir", nesw));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_calc_east_west_dir", ew));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_calc_SE_NW_dir", senw));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_calc_invariant_dir", invariantDir));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_calc_contrast", contrast));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_calc_dissimilarity", dissimilarity));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_calc_homogeneity", homogeneity));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_inverse_order_1", inverseOrder1));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_calc_ASM", asm));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_calc_energy", energy));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_calc_max_probability", maxProbability));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_calc_entropy", entropy));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_calc_mean", mean));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_calc_variance", variance));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_calc_standard_deviation",
                                                                       standardDeviation));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_calc_correlation", correlation));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_calc_shade", shade));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_calc_promenance", promenance));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_concatenate", concatenate));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_zscore", zscore));
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private int getNumDirections() {
        int numDirs = 0;

        if (ns) {
            numDirs++;
        }

        if (nesw) {
            numDirs++;
        }

        if (ew) {
            numDirs++;
        }

        if (senw) {
            numDirs++;
        }

        if (invariantDir) {
            numDirs++;
        }

        return numDirs;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private int getNumOperators() {
        int numOps = 0;

        if (contrast) {
            numOps++;
        }

        if (dissimilarity) {
            numOps++;
        }

        if (homogeneity) {
            numOps++;
        }

        if (inverseOrder1) {
            numOps++;
        }

        if (asm) {
            numOps++;
        }

        if (energy) {
            numOps++;
        }

        if (maxProbability) {
            numOps++;
        }

        if (entropy) {
            numOps++;
        }

        if (mean) {
            numOps++;
        }

        if (variance) {
            numOps++;
        }

        if (standardDeviation) {
            numOps++;
        }

        if (correlation) {
            numOps++;
        }
        
        if (shade) {
            numOps++;
        }
        
        if (promenance) {
            numOps++;
        }

        return numOps;
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        int ypos = 0;
        setForeground(Color.black);

        setTitle("Haralick Texture");
        getContentPane().setLayout(new BorderLayout());

        JPanel mainPanel;

        mainPanel = new JPanel();
        mainPanel.setBorder(BorderFactory.createEmptyBorder(3, 3, 3, 3));
        mainPanel.setLayout(new GridBagLayout());

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.insets = new Insets(3, 3, 3, 3);
        gbc.fill = GridBagConstraints.BOTH;
        
        if (image.isColorImage()) {
            colorPanel = new JPanel(new GridLayout(3, 1));
            colorPanel.setForeground(Color.black);

            colorGroup = new ButtonGroup();
            redButton = new JRadioButton("Red", true);
            redButton.setFont(serif12);
            redButton.setForeground(Color.black);
            redButton.addActionListener(this);
            colorGroup.add(redButton);
            colorPanel.add(redButton);

            greenButton = new JRadioButton("Green", false);
            greenButton.setFont(serif12);
            greenButton.setForeground(Color.black);
            greenButton.addActionListener(this);
            colorGroup.add(greenButton);
            colorPanel.add(greenButton);

            blueButton = new JRadioButton("Blue", false);
            blueButton.setFont(serif12);
            blueButton.setForeground(Color.black);
            blueButton.addActionListener(this);
            colorGroup.add(blueButton);
            colorPanel.add(blueButton);
            gbc.gridx = 0;
            gbc.gridy = ypos++;
            gbc.weighty = .1;
            JScrollPane colorScroll = new JScrollPane(colorPanel);
            colorScroll.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
            colorScroll.setBorder(buildTitledBorder("Colors"));
            mainPanel.add(colorScroll, gbc);   
        } // if (image.isColorImage())

        scalePanel = new JPanel(new GridBagLayout()); //3 rows x 2 columns
        scalePanel.setForeground(Color.black);
        gbc.gridx = 0;
        gbc.gridy = ypos++;
        gbc.weighty = .1;
        JScrollPane scaleScroll = new JScrollPane(scalePanel);
        scaleScroll.setBorder(buildTitledBorder("Sizes"));
        scaleScroll.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
        mainPanel.add(scaleScroll, gbc);

        GridBagConstraints gbcScale = new GridBagConstraints();
        labelWindowSize = new JLabel("Window size - odd: ");
        labelWindowSize.setForeground(Color.black);
        labelWindowSize.setFont(serif12);
        gbcScale.gridx = 0;
        gbcScale.gridy = 0;
        gbcScale.fill = GridBagConstraints.HORIZONTAL;
        gbcScale.anchor = GridBagConstraints.WEST;
        gbcScale.insets = new Insets(0, 0, 2, 0);
        scalePanel.add(labelWindowSize, gbcScale);
        textWindowSize = new JTextField(10);
        textWindowSize.setText("7");
        textWindowSize.setFont(serif12);
        textWindowSize.setForeground(Color.black);
        gbcScale.gridx++;
        gbcScale.insets = new Insets(0, 4, 2, 0);
        scalePanel.add(textWindowSize, gbcScale);

        //second row
        labelOffsetDistance = new JLabel("Offset distance: ");
        labelOffsetDistance.setForeground(Color.black);
        labelOffsetDistance.setFont(serif12);
        gbcScale.gridx = 0;
        gbcScale.gridy++;
        gbcScale.insets = new Insets(0, 0, 2, 0);
        scalePanel.add(labelOffsetDistance, gbcScale);
        textOffsetDistance = new JTextField(10);
        textOffsetDistance.setText("1");
        textOffsetDistance.setFont(serif12);
        textOffsetDistance.setForeground(Color.black);
        gbcScale.gridx++;
        gbcScale.insets = new Insets(0, 4, 2, 0);
        scalePanel.add(textOffsetDistance, gbcScale);
        
        //third row
        labelRescaling = new JLabel("Grey levels if rescaled (8-64): ");
        labelRescaling.setForeground(Color.black);
        labelRescaling.setFont(serif12);
        gbcScale.gridx = 0;
        gbcScale.gridy++;
        gbcScale.insets = new Insets(0, 0, 2, 0);
        scalePanel.add(labelRescaling, gbcScale);
        textRescaling = new JTextField(10);
        textRescaling.setText("32");
        textRescaling.setFont(serif12);
        textRescaling.setForeground(Color.black);
        gbcScale.gridx++;
        gbcScale.insets = new Insets(0, 4, 2, 0);
        scalePanel.add(textRescaling, gbcScale);

        JPanel directionPanel = new JPanel(new GridBagLayout());
        GridBagConstraints gbc2 = new GridBagConstraints();
        gbc2.gridwidth = 1;
        gbc2.gridheight = 1;
        gbc2.anchor = GridBagConstraints.WEST;
        gbc2.weightx = 1;
        gbc2.insets = new Insets(3, 3, 3, 3);
        gbc2.fill = GridBagConstraints.HORIZONTAL;

        nsCheckBox = new JCheckBox("North-South");
        nsCheckBox.setFont(serif12);
        gbc2.gridx = 0;
        gbc2.gridy = 0;
        directionPanel.add(nsCheckBox, gbc2);
        nsCheckBox.setSelected(true);

        neswCheckBox = new JCheckBox("NE-SW");
        neswCheckBox.setFont(serif12);
        gbc2.gridx = 0;
        gbc2.gridy = 1;
        directionPanel.add(neswCheckBox, gbc2);
        neswCheckBox.setSelected(false);

        ewCheckBox = new JCheckBox("East-West");
        ewCheckBox.setFont(serif12);
        gbc2.gridx = 0;
        gbc2.gridy = 2;
        directionPanel.add(ewCheckBox, gbc2);
        ewCheckBox.setSelected(false);

        senwCheckBox = new JCheckBox("SE-NW");
        senwCheckBox.setFont(serif12);
        gbc2.gridx = 0;
        gbc2.gridy = 3;
        directionPanel.add(senwCheckBox, gbc2);
        senwCheckBox.setSelected(false);

        invariantCheckBox = new JCheckBox("Spatially invariant");
        invariantCheckBox.setFont(serif12);
        gbc2.gridx = 0;
        gbc2.gridy = 4;
        directionPanel.add(invariantCheckBox, gbc2);
        invariantCheckBox.setSelected(false);

        gbc.gridx = 0;
        gbc.gridy = ypos++;
        gbc.weighty = .2;  
        JScrollPane directionScroll = new JScrollPane(directionPanel);
        directionScroll.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
        directionScroll.setBorder(buildTitledBorder("Offset directions"));
        mainPanel.add(directionScroll, gbc);

        JPanel operatorPanel = new JPanel(new GridBagLayout());
        GridBagConstraints gbc3 = new GridBagConstraints();
        gbc3.gridwidth = 1;
        gbc3.gridheight = 1;
        gbc3.anchor = GridBagConstraints.WEST;
        gbc3.weightx = 1;
        gbc3.insets = new Insets(2, 3, 2, 3);
        gbc3.fill = GridBagConstraints.HORIZONTAL;

        contrastCheckBox = new JCheckBox("Contrast (Inertia) (Sum of Squares Variance)");
        contrastCheckBox.setFont(serif12);
        gbc3.gridx = 0;
        gbc3.gridy = 0;
        operatorPanel.add(contrastCheckBox, gbc3);
        contrastCheckBox.setSelected(true);

        dissimilarityCheckBox = new JCheckBox("Dissimilarity");
        dissimilarityCheckBox.setFont(serif12);
        gbc3.gridx = 0;
        gbc3.gridy = 1;
        operatorPanel.add(dissimilarityCheckBox, gbc3);
        dissimilarityCheckBox.setSelected(false);

        homogeneityCheckBox = new JCheckBox("Homogeneity (Inverse Difference Moment of Order 2)");
        homogeneityCheckBox.setFont(serif12);
        gbc3.gridx = 0;
        gbc3.gridy = 2;
        operatorPanel.add(homogeneityCheckBox, gbc3);
        homogeneityCheckBox.setSelected(false);

        inverseOrder1CheckBox = new JCheckBox("Inverse Difference Moment of Order 1");
        inverseOrder1CheckBox.setFont(serif12);
        gbc3.gridx = 0;
        gbc3.gridy = 3;
        operatorPanel.add(inverseOrder1CheckBox, gbc3);
        inverseOrder1CheckBox.setSelected(false);

        asmCheckBox = new JCheckBox("Angular Second Moment (ASM)");
        asmCheckBox.setFont(serif12);
        gbc3.gridx = 0;
        gbc3.gridy = 4;
        operatorPanel.add(asmCheckBox, gbc3);
        asmCheckBox.setSelected(false);

        energyCheckBox = new JCheckBox("Energy (Uniformity)");
        energyCheckBox.setFont(serif12);
        gbc3.gridx = 0;
        gbc3.gridy = 5;
        operatorPanel.add(energyCheckBox, gbc3);
        energyCheckBox.setSelected(false);

        maxProbabilityCheckBox = new JCheckBox("Maximum Probability (MAX)");
        maxProbabilityCheckBox.setFont(serif12);
        gbc3.gridx = 0;
        gbc3.gridy = 6;
        operatorPanel.add(maxProbabilityCheckBox, gbc3);
        maxProbabilityCheckBox.setSelected(false);

        entropyCheckBox = new JCheckBox("Entropy (ENT)");
        entropyCheckBox.setFont(serif12);
        gbc3.gridx = 0;
        gbc3.gridy = 7;
        operatorPanel.add(entropyCheckBox, gbc3);
        entropyCheckBox.setSelected(false);

        meanCheckBox = new JCheckBox("Gray Level Co-occurrence Matrix Mean");
        meanCheckBox.setFont(serif12);
        gbc3.gridx = 0;
        gbc3.gridy = 8;
        operatorPanel.add(meanCheckBox, gbc3);
        meanCheckBox.setSelected(false);

        varianceCheckBox = new JCheckBox("Gray Level Co-occurrence Matrix Variance");
        varianceCheckBox.setFont(serif12);
        gbc3.gridx = 0;
        gbc3.gridy = 9;
        operatorPanel.add(varianceCheckBox, gbc3);
        varianceCheckBox.setSelected(false);

        standardDeviationCheckBox = new JCheckBox("Gray Level Co-occurrence Matrix Standard Deviation");
        standardDeviationCheckBox.setFont(serif12);
        gbc3.gridx = 0;
        gbc3.gridy = 10;
        operatorPanel.add(standardDeviationCheckBox, gbc3);
        standardDeviationCheckBox.setSelected(false);

        correlationCheckBox = new JCheckBox("Gray Level Co-occurrence Matrix Correlation");
        correlationCheckBox.setFont(serif12);
        gbc3.gridx = 0;
        gbc3.gridy = 11;
        operatorPanel.add(correlationCheckBox, gbc3);
        correlationCheckBox.setSelected(false);
        
        shadeCheckBox = new JCheckBox("Cluster shade");
        shadeCheckBox.setFont(serif12);
        gbc3.gridx = 0;
        gbc3.gridy = 12;
        operatorPanel.add(shadeCheckBox, gbc3);
        shadeCheckBox.setSelected(false);
        
        promenanceCheckBox = new JCheckBox("Cluster promenance");
        promenanceCheckBox.setFont(serif12);
        gbc3.gridx = 0;
        gbc3.gridy = 13;
        operatorPanel.add(promenanceCheckBox, gbc3);
        promenanceCheckBox.setSelected(false);

        gbc.gridx = 0;
        gbc.gridy = ypos++;
        gbc.weighty = .55;  
        JScrollPane operatorScroll = new JScrollPane(operatorPanel);
        operatorScroll.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
        operatorScroll.setBorder(buildTitledBorder("Texture operators"));
        mainPanel.add(operatorScroll, gbc);
        
        JPanel outputPanel = new JPanel(new GridBagLayout());
        gbc3 = new GridBagConstraints();
        gbc3.gridwidth = 1;
        gbc3.gridheight = 1;
        gbc3.anchor = GridBagConstraints.WEST;
        gbc3.weightx = 1;
        gbc3.insets = new Insets(3, 3, 3, 3);
        gbc3.fill = GridBagConstraints.HORIZONTAL;
        
        concatenateCheckBox = new JCheckBox("Concatenate source and calculated features together");
        concatenateCheckBox.setFont(serif12);
        gbc3.gridx = 0;
        gbc3.gridy = 0;
        outputPanel.add(concatenateCheckBox, gbc3);
        concatenateCheckBox.setSelected(false);
        
        zscoreCheckBox = new JCheckBox("zscore output");
        zscoreCheckBox.setFont(serif12);
        gbc3.gridx = 0;
        gbc3.gridy = 1;
        outputPanel.add(zscoreCheckBox, gbc3);
        zscoreCheckBox.setSelected(false);
        
        gbc.gridx = 0;
        gbc.gridy = ypos++;
        gbc.weighty = .1;  
        JScrollPane outputScroll = new JScrollPane(outputPanel);
        outputScroll.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
        outputScroll.setBorder(buildTitledBorder("Output options"));
        mainPanel.add(outputScroll, gbc);

        getContentPane().add(mainPanel, BorderLayout.CENTER);
        getContentPane().add(buildButtons(), BorderLayout.SOUTH);
        pack();
        setResizable(true);

        // setVisible( true );

        System.gc();
    }

    // *******************************************************************
    // ************************* Item Events ****************************
    // *******************************************************************


    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {
        String tmpStr;
        numDirections = 0;
        numOperators = 0;
        
        if (image.isColorImage()) {

            if (redButton.isSelected()) {
                RGBOffset = RED_OFFSET;
            } else if (greenButton.isSelected()) {
                RGBOffset = GREEN_OFFSET;
            } else {
                RGBOffset = BLUE_OFFSET;
            }
        } // if (image.isColorImage())

        tmpStr = textWindowSize.getText();

        if (testParameter(tmpStr, 3, Integer.MAX_VALUE)) {
            windowSize = Integer.valueOf(tmpStr).intValue();
        } else {
            textWindowSize.requestFocus();
            textWindowSize.selectAll();

            return false;
        }

        if ((windowSize % 2) == 0) {
            MipavUtil.displayError("Window size must not be even");
            textWindowSize.requestFocus();
            textWindowSize.selectAll();

            return false;
        }

        if (windowSize > image.getExtents()[0]) {
            MipavUtil.displayError("Window size must not excced image width of " + image.getExtents()[0]);
            textWindowSize.requestFocus();
            textWindowSize.selectAll();

            return false;
        }

        if (windowSize > image.getExtents()[1]) {
            MipavUtil.displayError("Window size must not excced image height of " + image.getExtents()[1]);
            textWindowSize.requestFocus();
            textWindowSize.selectAll();

            return false;
        }

        tmpStr = textOffsetDistance.getText();

        if (testParameter(tmpStr, 1, windowSize - 1)) {
            offsetDistance = Integer.valueOf(tmpStr).intValue();
        } else {
            textOffsetDistance.requestFocus();
            textOffsetDistance.selectAll();

            return false;
        }
        
        tmpStr = textRescaling.getText();
        
        if (testParameter(tmpStr, 8, 64)) {
            greyLevels = Integer.valueOf(tmpStr).intValue();
        } else {
            textRescaling.requestFocus();
            textRescaling.selectAll();
            
            return false;
        }

        ns = nsCheckBox.isSelected();

        nesw = neswCheckBox.isSelected();

        ew = ewCheckBox.isSelected();

        senw = senwCheckBox.isSelected();

        invariantDir = invariantCheckBox.isSelected();

        numDirections = getNumDirections();

        if (numDirections == 0) {
            MipavUtil.displayError("At least one direction must be checked");

            return false;
        }

        contrast = contrastCheckBox.isSelected();

        dissimilarity = dissimilarityCheckBox.isSelected();

        homogeneity = homogeneityCheckBox.isSelected();

        inverseOrder1 = inverseOrder1CheckBox.isSelected();

        asm = asmCheckBox.isSelected();

        energy = energyCheckBox.isSelected();

        maxProbability = maxProbabilityCheckBox.isSelected();

        entropy = entropyCheckBox.isSelected();

        mean = meanCheckBox.isSelected();

        variance = varianceCheckBox.isSelected();

        standardDeviation = standardDeviationCheckBox.isSelected();

        correlation = correlationCheckBox.isSelected();
        
        shade = shadeCheckBox.isSelected();
        
        promenance = promenanceCheckBox.isSelected();

        numOperators = getNumOperators();

        if (numOperators == 0) {
            MipavUtil.displayError("At least 1 texture operator must be selected");

            return false;
        }
        
        concatenate = concatenateCheckBox.isSelected();
        
        zscore = zscoreCheckBox.isSelected();

        return true;


    }
    
    /**
     * Return meta-information about this discoverable action for categorization and labeling purposes.
     * 
     * @return Metadata for this action.
     */
    public ActionMetadata getActionMetadata() {
        return new MipavActionMetadata() {
            public String getCategory() {
                return new String("Algorithms.Filters (spatial)");
            }

            public String getDescription() {
                return new String("Applies a Haralick texture to the image.");
            }

            public String getDescriptionLong() {
                return new String("Applies a Haralick texture to the image.");
            }

            public String getShortLabel() {
                return new String("HaralickTexture");
            }

            public String getLabel() {
                return new String("Haralick Texture");
            }

            public String getName() {
                return new String("Haralick Texture");
            }
        };
    }
    
    /**
     * Returns a table listing the input parameters of this algorithm (which should match up with the scripting
     * parameters used in {@link #setGUIFromParams()}).
     * 
     * @return A parameter table listing the inputs of this algorithm.
     */
    public ParameterTable createInputParameters() {
        final ParameterTable table = new ParameterTable();

        try {
        	
            table.put(new ParameterExternalImage(AlgorithmParameters.getInputImageLabel(1)));
            table.put(new ParameterInt("RGB_offset",1));
            table.put(new ParameterInt("window_size", 7));
            table.put(new ParameterInt("offset_distance", 1));
            table.put(new ParameterInt("grey_levels", 32));
            table.put(new ParameterBoolean("do_calc_north_south_dir", true));
            table.put(new ParameterBoolean("do_calc_NE_SW_dir", false));
            table.put(new ParameterBoolean("do_calc_east_west_dir", false));
            table.put(new ParameterBoolean("do_calc_SE_NW_dir", false));
            table.put(new ParameterBoolean("do_calc_invariant_dir", false));
            table.put(new ParameterBoolean("do_calc_contrast", true));
            table.put(new ParameterBoolean("do_calc_dissimilarity", false));
            table.put(new ParameterBoolean("do_calc_homogeneity", false));
            table.put(new ParameterBoolean("do_inverse_order_1", false));
            table.put(new ParameterBoolean("do_calc_ASM", false));
            table.put(new ParameterBoolean("do_calc_energy", false));
            table.put(new ParameterBoolean("do_calc_max_probability", false));
            table.put(new ParameterBoolean("do_calc_entropy", false));
            table.put(new ParameterBoolean("do_calc_mean", false));
            table.put(new ParameterBoolean("do_calc_variance", false));
            table.put(new ParameterBoolean("do_calc_standard_deviation", false));
            table.put(new ParameterBoolean("do_calc_correlation", false));
            table.put(new ParameterBoolean("do_calc_shade", false));
            table.put(new ParameterBoolean("do_calc_promenance", false));
            table.put(new ParameterBoolean("do_concatenate", true));
            table.put(new ParameterBoolean("do_zscore", false));
            } catch (final ParserException e) {
            // this shouldn't really happen since there isn't any real parsing going on...
            e.printStackTrace();
        }

        return table;
    }

    /**
     * Returns a table listing the output parameters of this algorithm (usually just labels used to obtain output image
     * names later).
     * 
     * @return A parameter table listing the outputs of this algorithm.
     */
    public ParameterTable createOutputParameters() {
        final ParameterTable table = new ParameterTable();

        try {
            table.put(new ParameterImage(AlgorithmParameters.RESULT_IMAGE));
        } catch (final ParserException e) {
            // this shouldn't really happen since there isn't any real parsing going on...
            e.printStackTrace();
        }

        return table;
    }
    
    /**
     * Returns the name of an image output by this algorithm, the image returned depends on the parameter label given
     * (which can be used to retrieve the image object from the image registry).
     * 
     * @param imageParamName The output image parameter label for which to get the image name.
     * @return The image name of the requested output image parameter label.
     */
    public String getOutputImageName(final String imageParamName) {
    		//System.out.println(resultImage.length);
                return resultImage[0].getImageName();
        }

    /**
     * Returns whether the action has successfully completed its execution.
     * 
     * @return True, if the action is complete. False, if the action failed or is still running.
     */
    public boolean isActionComplete() {
        return isComplete();
    }

}
