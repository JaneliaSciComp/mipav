package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.utilities.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.util.*;

import javax.swing.*;


/**
 * Simple dialog to convert the type of an image.
 *
 * @version  1.0 Jan 25, 1999
 * @author   Matthew J. McAuliffe, Ph.D.
 */
public class JDialogConvertType extends JDialogScriptableBase
        implements AlgorithmInterface, ItemListener, DialogDefaultsInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -6815107682654273857L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private JRadioButton bigEnd;

    /** DOCUMENT ME! */
    private AlgorithmChangeType changeTypeAlgo;

    /** DOCUMENT ME! */
    private int dataType;

    /** DOCUMENT ME! */
    private int displayLoc; // Flag indicating if a new image is to be generated

    /** DOCUMENT ME! */
    private boolean endianess;

    /** DOCUMENT ME! */
    private JRadioButton fullRangeRadio;

    /** DOCUMENT ME! */
    private ModelImage image;

    /** DOCUMENT ME! */
    private JLabel inEnd;

    /** DOCUMENT ME! */
    private double inMin, inMax;

    /** DOCUMENT ME! */
    private ButtonGroup inputRangeGroup;

    /** DOCUMENT ME! */
    private JLabel inStart;

    /** DOCUMENT ME! */
    private double inTempMin, inTempMax;

    /** DOCUMENT ME! */
    private JRadioButton littleEnd;

    /** DOCUMENT ME! */
    private JRadioButton newImage;

    /** DOCUMENT ME! */
    private JLabel outEnd;

    /** DOCUMENT ME! */
    private double outMin, outMax;

    /** DOCUMENT ME! */
    private JLabel outStart;

    /** DOCUMENT ME! */
    private double outTempMin, outTempMax;

    /** DOCUMENT ME! */
    private boolean processIndep = false; // for 2.5D processing each slice independently

    /** DOCUMENT ME! */
    private JCheckBox processIndepBox;

    /** DOCUMENT ME! */
    private JRadioButton radioARGB;

    /** DOCUMENT ME! */
    private JRadioButton radioARGB_FLOAT;

    /** DOCUMENT ME! */
    private JRadioButton radioARGB_USHORT;
    
    private JRadioButton radioComplex;
    
    private JRadioButton radioDComplex;

    /** DOCUMENT ME! */
    private JRadioButton radioBool;

    /** DOCUMENT ME! */
    private JRadioButton radioByte;

    /** DOCUMENT ME! */
    private JRadioButton radioDouble;

    /** DOCUMENT ME! */
    private JRadioButton radioFloat;

    /** DOCUMENT ME! */
    private JRadioButton radioInt;

    /** DOCUMENT ME! */
    private JRadioButton radioLong;

    /** DOCUMENT ME! */
    private JRadioButton radioShort;

    /** DOCUMENT ME! */
    private JRadioButton radioUByte;

    /** DOCUMENT ME! */
    private JRadioButton radioUInt;

    /** DOCUMENT ME! */
    private JRadioButton radioUShort;

    /** DOCUMENT ME! */
    private JRadioButton replaceImage;

    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // result image

    /** DOCUMENT ME! */
    private JTextField textInEnd;

    /** DOCUMENT ME! */
    private JTextField textInStart;

    /** DOCUMENT ME! */
    private JTextField textOutEnd;

    /** DOCUMENT ME! */
    private JTextField textOutStart;

    /** DOCUMENT ME! */
    private String[] titles;

    /** DOCUMENT ME! */
    private boolean useDefaultRanges = true; // only applies for input ranges

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;

    /** DOCUMENT ME! */
    private JRadioButton userRangeRadio;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogConvertType() { }

    // or if the source image is to be replaced

    /**
     * Creates new dialog for converting type of image.
     *
     * @param  theParentFrame  Parent frame.
     * @param  _image          Source image.
     */
    public JDialogConvertType(Frame theParentFrame, ModelImage _image) {
        super(theParentFrame, false);
        image = _image;
        userInterface = ViewUserInterface.getReference();
        init();
        loadDefaults();
        setVisible(true);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Closes dialog box when the OK button is pressed and sets the variables.
     *
     * @param  event  Event that triggers this function
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();

        if (command.equals("OK")) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Cancel")) {
            cancelFlag = true;
            dispose();
        } else if (command.equals("Help")) {
            //MipavUtil.showHelp("U4013");
            MipavUtil.showWebHelp("Converting_image_datasets_to_different_data_types");
        } else if (command.equals("FullRange")) {

            // if using full range, then reset the
            // input values to their defaults
            // and disable the text fields and labels
            // for the user defined range.
            useDefaultRanges = true;
            this.setDefaultRanges();

            inStart.setEnabled(false);
            inEnd.setEnabled(false);
            textInStart.setEnabled(false);
            textInEnd.setEnabled(false);
        } else if (command.equals("UserRange")) {
            // set the values to the last settings of the text fields
            // and enable the user defined labels and text fields

            useDefaultRanges = false;
            inStart.setEnabled(true);
            inEnd.setEnabled(true);
            textInStart.setEnabled(true);
            textInEnd.setEnabled(true);

            String tmpStr = textInStart.getText();

            if (testParameter(tmpStr, image.getMin(), image.getMax())) {
                inTempMin = Double.valueOf(tmpStr).doubleValue();
            } else {
                textInStart.requestFocus();
                textInStart.selectAll();
            }

            tmpStr = textInEnd.getText();

            if (testParameter(tmpStr, image.getMin(), image.getMax())) {
                inTempMax = Double.valueOf(tmpStr).doubleValue();
            } else {
                textInEnd.requestFocus();
                textInEnd.selectAll();
            } 
 
        } else {
            super.actionPerformed(event);
        }

    } // end actionPerformed()

    // ************************************************************************
    // ************************** Algorithm Events ****************************
    // ************************************************************************

    /**
     * This method is required if the AlgorithmPerformed interface is implemented. It is called by the algorithms when
     * it has completed or failed to to complete, so that the dialog can be display the result image and/or clean up.
     *
     * @param  algorithm  Algorithm that caused the event.
     */
    public void algorithmPerformed(AlgorithmBase algorithm) {

        if (Preferences.is(Preferences.PREF_SAVE_DEFAULTS) && (this.getOwner() != null) && !isScriptRunning()) {
            saveDefaults();
        }

        if (algorithm instanceof AlgorithmChangeType) {

            if ((changeTypeAlgo.isCompleted() == true) && (resultImage != null)) {
                updateFileInfo(image, resultImage);

                if (resultImage.getNDims() == 3) {
                	for (int n = 0; n < resultImage.getExtents()[2]; n++) {
                        resultImage.getFileInfo(n).setEndianess(endianess);
                	}
                } 

                // The algorithm has completed and produced a new image to be displayed.
                try {
                    new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
                } catch (OutOfMemoryError error) {
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }
            } else if (resultImage == null) {

                // These next lines set the titles in all frames where the source image is displayed to
                // image name so as to indicate that the image is now unlocked!
                // The image frames are enabled and then registed to the userinterface.
                Vector<ViewImageUpdateInterface> imageFrames = image.getImageFrameVector();

                for (int i = 0; i < imageFrames.size(); i++) {
                    ((Frame) (imageFrames.elementAt(i))).setTitle(titles[i]);
                    ((Frame) (imageFrames.elementAt(i))).setEnabled(true);

                    if (((Frame) (imageFrames.elementAt(i))) != parentFrame) {
                        userInterface.registerFrame((Frame) (imageFrames.elementAt(i)));
                    }
                }

                if (parentFrame != null) {
                    userInterface.registerFrame(parentFrame);
                }

                if (image.isColorImage()) {
                    image.notifyImageDisplayListeners(true, 0, image.getParentFrame().getComponentImage().getRGBTA());    
                }
                else {
                    image.notifyImageDisplayListeners(null, true);
                }
            } else if (resultImage != null) {

                // algorithm failed but result image still has garbage
                resultImage.disposeLocal(); // clean up memory
                resultImage = null;
            }
        }

        insertScriptLine();

        // Update frame
        // ((ViewJFrameBase)parentFrame).updateImages(true);
        changeTypeAlgo.finalize();
        changeTypeAlgo = null;
        dispose();
        System.gc();
    }

    /**
     * Accessor that returns the data type.
     *
     * @return  the data type
     */
    public int getDataType() {
        return dataType;
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
        str += dataType + delim;
        str += endianess + delim;
        str += useDefaultRanges + delim;

        if (!useDefaultRanges) {
            str += inTempMin + delim;
            str += inTempMax + delim;
        }

        str += outTempMin + delim;
        str += outTempMax;

        return str;
    }

    /**
     * Accessor that returns the image.
     *
     * @return  The result image.
     */
    public ModelImage getResultImage() {
        return resultImage;
    }

    /**
     * Sets the flags for the checkboxes and resets labels.
     *
     * @param  event  Event that triggered this function.
     */
    public synchronized void itemStateChanged(ItemEvent event) {

        outStart.setEnabled(true);
        outEnd.setEnabled(true);
        textOutStart.setEnabled(true);
        textOutEnd.setEnabled(true);

        if (image.isColorImage()) {

            if (radioARGB.isSelected()) {
                outStart.setText("Starting range (0 to 255).");
                outEnd.setText("Ending range (0 to 255).");
                //textOutStart.setText("0");
                //textOutEnd.setText("255");
                
                if(image.getMin() <= 0 || image.getMin() > 255) {
                	textOutStart.setText("0");
                }else {
                	Integer i = new Integer((int)image.getMin());
                	textOutStart.setText(i.toString());
                }
                
                if(image.getMax() > 255) {
                	textOutEnd.setText("255");
                }else {
                	Integer i = new Integer((int)image.getMax());
                	textOutEnd.setText(i.toString());
                }
                inMin = 0;
                inMax = 255;
                outMin = 0;
                outMax = 255;
            } else if (radioARGB_USHORT.isSelected()) {
            	//System.out.println("selected");
                outStart.setText("Starting range (0 to 65535).");
                outEnd.setText("Ending range (0 to 65535).");
                //textOutStart.setText("0");
                //textOutEnd.setText("4095");
                
                if(image.getMin() <= 0 || image.getMin() > 65535) {
                	textOutStart.setText("0");
                }else {
                	Integer i1 = new Integer((int)image.getMin());
                	textOutStart.setText(i1.toString());
                }
                
                if(image.getMax() > 65535) {
                	textOutEnd.setText("65535");
                }else {
                	Integer i2 = new Integer((int)image.getMax());
                	textOutEnd.setText(i2.toString());
                }
                inMin = 0;
                inMax = 65535;
                outMin = 0;
                outMax = 65535;
            } else if (radioARGB_FLOAT.isSelected()) {
                outStart.setText("Starting range (-3.40 E+38 to 3.40 E+38).");
                outEnd.setText("Ending range (-3.40 E+38 to 3.40 E+38).");
                //textOutStart.setText("0");
                //textOutEnd.setText("1023");

                Float f1 = new Float((int)image.getMin());
                textOutStart.setText(f1.toString());
                
                
                if(image.getMax() > Float.MAX_VALUE) {
                	textOutEnd.setText(String.valueOf(Float.MAX_VALUE));
                }else {
                	Float f2 = new Float((float)image.getMax());
                	textOutEnd.setText(f2.toString());
                }
                
                inMin = -Float.MAX_VALUE;
                inMax = Float.MAX_VALUE;
                outMin = -Float.MAX_VALUE;
                outMax = Float.MAX_VALUE;
            }
        } // if (image.isColorImage())
        else if (image.isComplexImage()) {
            if (radioComplex.isSelected()) {
            	//outStart.setText("Starting range (0.0 to 3.40 E+38).");
                outEnd.setText("Ending maximum magnitude (0.0 to 3.40 E+38).");
                //textOutStart.setText("0");
                //textOutEnd.setText("1023");

                //Float f1 = new Float((int)image.getMin());
                //textOutStart.setText(f1.toString());
                
                
                if(image.getMax() > Float.MAX_VALUE) {
                	textOutEnd.setText(String.valueOf(Float.MAX_VALUE));
                }else {
                	Float f2 = new Float((float)image.getMax());
                	textOutEnd.setText(f2.toString());
                }
                
                inMin = 0.0;
                inMax = Float.MAX_VALUE;
                outMin = 0.0;
                outMax = Float.MAX_VALUE;	
            }
            else if (radioDComplex.isSelected()) {
            	//outStart.setText("Starting range (0.0 to 1.8 E+308).");
                outEnd.setText("Ending maximum magnitude (0.0 to 1.8 E+308).");
                //textOutStart.setText("0");
                //textOutEnd.setText("1023");
                
                //Double d1 = new Double((double)image.getMin());
                //textOutStart.setText(d1.toString());

                     
                if(image.getMax() > Double.MAX_VALUE) {
                	textOutEnd.setText(String.valueOf(Double.MAX_VALUE));
                }else {
                	Double d2 = new Double((double)image.getMax());
                	textOutEnd.setText(d2.toString());
                }
                inMin = 0.0;
                inMax = Double.MAX_VALUE;
                outMin = 0.0;
                outMax = Double.MAX_VALUE;	
            }
        } // else if (image.isComplexImage())
        else { // black and white image

            if (radioBool.isSelected()) {
                outStart.setText(" ");
                outEnd.setText("Pixels != 0 are set to 1. ");
                textOutStart.setText("0");
                textOutStart.setEnabled(false);
                textOutEnd.setText("1");
                textOutEnd.setEnabled(false);
                inMin = 0;
                inMax = 1;
                outMin = 0;
                outMax = 1;
            } else if (radioByte.isSelected()) {
                outStart.setText("Starting range (-128 to 127).");
                outEnd.setText("Ending range (-128 to 127).");
                //textOutStart.setText("-128");
                //textOutEnd.setText("127");
                
                if(image.getMin() > 127) {
                	textOutStart.setText("0");
                }else {
                	Byte b1 = new Byte((byte)image.getMin());
                	textOutStart.setText(b1.toString());
                }

                
                if(image.getMax() > Byte.MAX_VALUE) {
                	textOutEnd.setText(String.valueOf(Byte.MAX_VALUE));
                }else {
                	Byte b2 = new Byte((byte)image.getMax());
                	textOutEnd.setText(b2.toString());
                }
                
                
                inMin = -128;
                inMax = 127;
                outMin = -128;
                outMax = 127;
            } else if (radioUByte.isSelected()) {
                outStart.setText("Starting range (0 to 255).");
                outEnd.setText("Ending range (0 to 255).");
                //textOutStart.setText("0");
                //textOutEnd.setText("255");
                
                if(image.getMin() <= 0 || image.getMin() > 255) {
                	textOutStart.setText("0");
                }else {
                	Short s1 = new Short((short)image.getMin());
                	textOutStart.setText(s1.toString());
                }
                
                if(image.getMax() > 255) {
                	textOutEnd.setText("255");
                }else {
                	Short s2 = new Short((short)image.getMax());
                	textOutEnd.setText(s2.toString());
                }
                inMin = 0;
                inMax = 255;
                outMin = 0;
                outMax = 255;
            } else if (radioShort.isSelected()) {
                outStart.setText("Starting range (-32768 to 32767).");
                outEnd.setText("Ending range (-32768 to 32767).");
                //textOutStart.setText("0");
                //textOutEnd.setText("3071");
                

                Short s1 = new Short((short)image.getMin());
                textOutStart.setText(s1.toString());
                
                if(image.getMax() > Short.MAX_VALUE) {
                	textOutEnd.setText("32767");
                }else {
                	Short s2 = new Short((short)image.getMax());
                	textOutEnd.setText(s2.toString());
                }
                inMin = -32768;
                inMax = 32767;
                outMin = -32768;
                outMax = 32767;
            } else if (radioUShort.isSelected()) {
                outStart.setText("Starting range (0 to 65535).");
                outEnd.setText("Ending range (0 to 65535).");
                //textOutStart.setText("0");
                //textOutEnd.setText("4095");
                
                if(image.getMin() <= 0) {
                	textOutStart.setText("0");
                }else {
                	Integer i1 = new Integer((int)image.getMin());
                	textOutStart.setText(i1.toString());
                }
                
                if(image.getMax() > 65535) {
                	textOutEnd.setText("65535");
                }else {
                	Integer i2 = new Integer((int)image.getMax());
                	textOutEnd.setText(i2.toString());
                }
                inMin = 0;
                inMax = 65535;
                outMin = 0;
                outMax = 65535;
            } else if (radioInt.isSelected()) {
                outStart.setText("Starting range (-2.147 E+9  to 2.147 E+9).");
                outEnd.setText("Ending range (-2.147 E+9  to 2.147 E+9).");
                //textOutStart.setText("0");
                //textOutEnd.setText("4095");
                

                Integer i1 = new Integer((int)image.getMin());
                textOutStart.setText(i1.toString());
                
                if(image.getMax() > Integer.MAX_VALUE) {
                	textOutEnd.setText(String.valueOf(Integer.MAX_VALUE));
                }else {
                	Integer i2 = new Integer((int)image.getMax());
                	textOutEnd.setText(i2.toString());
                }
                inMin = Integer.MIN_VALUE;
                inMax = Integer.MAX_VALUE;
                outMin = Integer.MIN_VALUE;
                outMax = Integer.MAX_VALUE;
            } else if (radioUInt.isSelected()) {
                outStart.setText("Starting range (0 to 4.29 E+9).");
                outEnd.setText("Ending range (0 to 4.29 E+9).");
                //textOutStart.setText("0");
                //textOutEnd.setText("4095");
                
                if(image.getMin() <= 0) {
                	textOutStart.setText("0");
                }else {
                	Long l1 = new Long((long)image.getMin());
                	textOutStart.setText(l1.toString());
                }
                
                if(image.getMax() > 4294967295L) {
                	textOutEnd.setText("4294967295");
                }else {
                	Long l2 = new Long((long)image.getMax());
                	textOutEnd.setText(l2.toString());
                }
                inMin = 0;
                inMax = 4294967295L;
                outMin = 0;
                outMax = 4294967295L;
            } else if (radioLong.isSelected()) {
                outStart.setText("Starting range (-9.22 E+18 to 9.22 E+18).");
                outEnd.setText("Ending range (-9.22 E+18 to 9.22 E+18).");
                //textOutStart.setText("0");
                //textOutEnd.setText("4095");
                

                Long l1 = new Long((long)image.getMin());
                textOutStart.setText(l1.toString());

                if(image.getMax() > Long.MAX_VALUE) {
                	textOutEnd.setText(String.valueOf(Long.MAX_VALUE));
                }else {
                	Long l2 = new Long((long)image.getMax());
                	textOutEnd.setText(l2.toString());
                }
                inMin = Long.MIN_VALUE;
                inMax = Long.MAX_VALUE;
                outMin = Long.MIN_VALUE;
                outMax = Long.MAX_VALUE;
            } else if (radioFloat.isSelected()) {
                outStart.setText("Starting range (-3.40 E+38 to 3.40 E+38).");
                outEnd.setText("Ending range (-3.40 E+38 to 3.40 E+38).");
                //textOutStart.setText("0");
                //textOutEnd.setText("1023");
                

                Float f1 = new Float((float)image.getMin());
                textOutStart.setText(f1.toString());
                
                if(image.getMax() > Float.MAX_VALUE) {
                	textOutEnd.setText(String.valueOf(Float.MAX_VALUE));
                }else {
                	Float f2 = new Float((float)image.getMax());
                	textOutEnd.setText(f2.toString());
                }
                inMin = -Float.MAX_VALUE;
                inMax = Float.MAX_VALUE;
                outMin = -Float.MAX_VALUE;
                outMax = Float.MAX_VALUE;
            } else if (radioDouble.isSelected()) {
                outStart.setText("Starting range (-1.8 E+308 to 1.8 E+308).");
                outEnd.setText("Ending range (-1.8 E+308 to 1.8 E+308).");
                //textOutStart.setText("0");
                //textOutEnd.setText("1023");
                
                Double d1 = new Double((double)image.getMin());
                textOutStart.setText(d1.toString());

                     
                if(image.getMax() > Double.MAX_VALUE) {
                	textOutEnd.setText(String.valueOf(Double.MAX_VALUE));
                }else {
                	Double d2 = new Double((double)image.getMax());
                	textOutEnd.setText(d2.toString());
                }
                inMin = -Double.MAX_VALUE;
                inMax = Double.MAX_VALUE;
                outMin = -Double.MAX_VALUE;
                outMax = Double.MAX_VALUE;
            }
        } // else black and white image
    }

    /**
     * Loads the default settings from Preferences to set up the dialog.
     */
    public void loadDefaults() {
        String defaultsString = Preferences.getDialogDefaults(getDialogName());

        if ((defaultsString != null) && (newImage != null)) {

            try {
                StringTokenizer st = new StringTokenizer(defaultsString, ",");

                dataType = MipavUtil.getInt(st);
                endianess = MipavUtil.getBoolean(st);
                useDefaultRanges = MipavUtil.getBoolean(st);

                if (!useDefaultRanges) {
                    inTempMin = MipavUtil.getFloat(st);
                    inTempMax = MipavUtil.getFloat(st);
                }

                outTempMin = MipavUtil.getFloat(st);
                outTempMax = MipavUtil.getFloat(st);

                if (endianess) {
                    bigEnd.setSelected(true);
                } else {
                    littleEnd.setSelected(true);
                }

                if (useDefaultRanges) {
                    fullRangeRadio.setSelected(true);
                } else {
                    actionPerformed(new ActionEvent(userRangeRadio, 0, "UserRange"));
                    userRangeRadio.setSelected(true);
                }

                if (new Boolean(st.nextToken()).booleanValue()) {
                    newImage.setSelected(true);
                } else {
                    replaceImage.setSelected(true);
                }

                // check to see if image type goes with grayscale or color
                // only sets defaults when last was color->color
                // and las was gray->gray align
                if (image.isColorImage()) {

                    if (dataType == ModelStorageBase.ARGB) {
                        radioARGB.setSelected(true);
                    } else if (dataType == ModelStorageBase.ARGB_FLOAT) {
                        radioARGB_FLOAT.setSelected(true);
                    } else if (dataType == ModelStorageBase.ARGB_USHORT) {
                        radioARGB_USHORT.setSelected(true);
                    }
                } else if (image.isComplexImage()) {
                	if (dataType == ModelStorageBase.COMPLEX) {
                		radioComplex.setSelected(true);
                	} else if (dataType == ModelStorageBase.DCOMPLEX) {
                		radioDComplex.setSelected(true);
                	}
                } else {

                    if (dataType == ModelStorageBase.BOOLEAN) {
                        radioBool.setSelected(true);
                    } else if (dataType == ModelStorageBase.BYTE) {
                        radioByte.setSelected(true);
                    } else if (dataType == ModelStorageBase.UBYTE) {
                        radioUByte.setSelected(true);
                    } else if (dataType == ModelStorageBase.SHORT) {
                        radioShort.setSelected(true);
                    } else if (dataType == ModelStorageBase.USHORT) {
                        radioUShort.setSelected(true);
                    } else if (dataType == ModelStorageBase.INTEGER) {
                        radioInt.setSelected(true);
                    } else if (dataType == ModelStorageBase.UINTEGER) {
                        radioUInt.setSelected(true);
                    } else if (dataType == ModelStorageBase.LONG) {
                        radioLong.setSelected(true);
                    } else if (dataType == ModelStorageBase.FLOAT) {
                        radioFloat.setSelected(true);
                    } else if (dataType == ModelStorageBase.DOUBLE) {
                        radioDouble.setSelected(true);
                    }
                }
            } catch (Exception ex) {

                // since there was a problem parsing the defaults string, start over with the original defaults
                Preferences.debug("Resetting defaults for dialog: " + getDialogName());
                Preferences.removeProperty(getDialogName());
                ex.printStackTrace();
            }
        }
    }

    /**
     * Saves the default settings into the Preferences file.
     */
    public void saveDefaults() {
        String defaultsString = new String(getParameterString(",") + "," + newImage.isSelected());
        Preferences.saveDialogDefaults(getDialogName(), defaultsString);
    }


    /**
     * Accessor that sets the data type for what the converted image is to be.
     *
     * @param  type  New data type.
     */
    public void setDataType(int type) {
        dataType = type;
    }

    /**
     * Sets the default values for the input and output range.
     */
    public void setDefaultRanges() {
        inTempMin = (float) image.getMin();
        inTempMax = (float) image.getMax();

        /*  Only the input values need to be reset for a particular image
         * if (dataType == ModelStorageBase.BOOLEAN) { outTempMin= 0; outTempMax= 1; } else if (dataType ==
         * ModelStorageBase.BYTE)    { outTempMin = -128; outTempMax = 127; } else if (dataType ==
         * ModelStorageBase.UBYTE)  { outTempMin = 0; outTempMax = 255; } else if (dataType == ModelStorageBase.SHORT) {
         * outTempMin = -1024; outTempMax = 3071; } else if (dataType == ModelStorageBase.USHORT) { outTempMin = 0;
         * outTempMax = 4095; } else if (dataType == ModelStorageBase.INTEGER) { outTempMin = 0; outTempMax = 4095; }
         * else if (dataType == ModelStorageBase.LONG) { outTempMin = 0; outTempMax = 4095; } else if (dataType ==
         * ModelStorageBase.FLOAT) { outTempMin = 0; outTempMax = 1023; } else if (dataType == ModelStorageBase.DOUBLE)
         * { outTempMin = 0; outTempMax = 1023; }
         */
        // inTempMinDefault = inTempMin;
        // inTempMaxDefault = inTempMax;
        // outTempMinDefault = outTempMin;
        // outTempMaxDefault = outTempMax;

    }

    /**
     * Accessor that sets the display loc variable to new, so that a new image is created once the algorithm completes.
     */
    public void setDisplayLocNew() {
        displayLoc = NEW;
    }

    /**
     * Accessor that sets the display loc variable to replace, so the current image is replaced once the algorithm
     * completes.
     */
    public void setDisplayLocReplace() {
        displayLoc = REPLACE;
    }

    /**
     * Accessor that sets the endianess.
     *
     * @param  endns  Endianess.
     */
    public void setEndianess(boolean endns) {
        endianess = endns;
    }

    /**
     * Accessor that sets the maximum input range to the parameter.
     *
     * @param  max  Maximum input range.
     */
    public void setInputRangeMax(double max) {
        inTempMax = max;
    }

    /**
     * Accessor that sets the minimum input range to the parameter.
     *
     * @param  min  Minimum input range.
     */
    public void setInputRangeMin(double min) {
        inTempMin = min;
    }

    /**
     * Accessor that sets the maximum output range to the parameter.
     *
     * @param  max  Maximum output range.
     */
    public void setOutputRangeMax(double max) {
        outTempMax = max;
    }

    /**
     * Accessor that sets the minimum output range to the parameter.
     *
     * @param  min  Minimum output range.
     */
    public void setOutputRangeMin(double min) {
        outTempMin = min;
    }

    /**
     * Accessor that sets the useDefaultRanges to the parameter.
     *
     * @param  useDefault  Value for useDefaultRanges variable.
     */
    public void setUseDefaultRanges(boolean useDefault) {
        useDefaultRanges = useDefault;
    }

    /**
     * Once all the necessary variables are set, call the Change Type algorithm based on what type of image this is and
     * whether or not there is a separate destination image.
     */
    protected void callAlgorithm() {

    	if (image.getNDims() == 2) {
    	    image.getFileInfo(0).setOriginalEndianess(image.getFileInfo(0).getEndianess());
    	}
    	else if (image.getNDims() == 3) {
    		for (int i = 0; i < image.getExtents()[2]; i++) {
    			image.getFileInfo(i).setOriginalEndianess(image.getFileInfo(i).getEndianess());	
    		}
     	}
    	else {
    		for (int i = 0; i < image.getExtents()[2]*image.getExtents()[3]; i++) {
    			image.getFileInfo(i).setOriginalEndianess(image.getFileInfo(i).getEndianess());	
    		}	
    	}
        if (image.getNDims() == 2) { // source image is 2D

            int[] destExtents = new int[2];
            destExtents[0] = image.getExtents()[0]; // X dim
            destExtents[1] = image.getExtents()[1]; // Y dim

            image.getFileInfo(0).setEndianess(endianess);

            if (displayLoc == NEW) {

                try {

                    // Make result image of the new data type
                    resultImage = new ModelImage(dataType, destExtents,
                                                 makeImageName(image.getImageName(), "_changed"));

                    resultImage.getFileInfo(0).setEndianess(endianess);


                    // Make algorithm
                    changeTypeAlgo = new AlgorithmChangeType(resultImage, image, inTempMin, inTempMax, outTempMin,
                                                             outTempMax, processIndep);

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed of failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    changeTypeAlgo.addListener(this);

                    createProgressBar(image.getImageName(), changeTypeAlgo);

                    // Hide dialog
                    setVisible(false);

                    if (isRunInSeparateThread()) {

                        // Start the thread as a low priority because we wish to still have user interface work fast.
                        if (changeTypeAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {

                        changeTypeAlgo.run();
                    }
                } catch (OutOfMemoryError x) {
                    MipavUtil.displayError("Dialog convert image: unable to allocate enough memory");

                    if (resultImage != null) {
                        resultImage.disposeLocal(); // Clean up memory of result image
                        resultImage = null;
                    }

                    return;
                }
            } else {

                try {

                    image.getFileInfo(0).setEndianess(endianess);

                    // No need to make new image space because the user has choosen to replace the source image
                    // Make the algorithm class
                    changeTypeAlgo = new AlgorithmChangeType(image, dataType, inTempMin, inTempMax, outTempMin,
                                                             outTempMax, processIndep);

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed of failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    changeTypeAlgo.addListener(this);

                    createProgressBar(image.getImageName(), changeTypeAlgo);

                    // Hide the dialog since the algorithm is about to run.
                    setVisible(false);

                    // These next lines set the titles in all frames where the source image is displayed to
                    // "locked - " image name so as to indicate that the image is now read/write locked!
                    // The image frames are disabled and then unregisted from the userinterface until the
                    // algorithm has completed.
                    Vector<ViewImageUpdateInterface> imageFrames = image.getImageFrameVector();
                    titles = new String[imageFrames.size()];

                    for (int i = 0; i < imageFrames.size(); i++) {
                        titles[i] = ((Frame) (imageFrames.elementAt(i))).getTitle();
                        ((Frame) (imageFrames.elementAt(i))).setTitle("Locked: " + titles[i]);
                        ((Frame) (imageFrames.elementAt(i))).setEnabled(false);
                        userInterface.unregisterFrame((Frame) (imageFrames.elementAt(i)));
                    }

                    if (isRunInSeparateThread()) {

                        // Start the thread as a low priority because we wish to still have user interface.
                        if (changeTypeAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {

                        changeTypeAlgo.run();
                    }
                } catch (OutOfMemoryError x) {
                    MipavUtil.displayError("Dialog change type: unable to allocate enough memory");

                    return;
                }
            }
        } else if ((image.getNDims() == 3) || (image.getNDims() == 4)) {
            int[] destExtents;

            if (image.getNDims() == 3) {
                destExtents = new int[3];
                destExtents[0] = image.getExtents()[0];
                destExtents[1] = image.getExtents()[1];
                destExtents[2] = image.getExtents()[2];

                // for (int n = 0; n < image.getExtents()[2]; n++) {
                // image.getFileInfo(n).setEndianess(endianess);
                // }


            } else {
                destExtents = new int[4];
                destExtents[0] = image.getExtents()[0];
                destExtents[1] = image.getExtents()[1];
                destExtents[2] = image.getExtents()[2];
                destExtents[3] = image.getExtents()[3];

                // for (int n = 0; n < (image.getExtents()[2] * image.getExtents()[3]); n++) {
                // image.getFileInfo(n).setEndianess(endianess);
                // }
            }

            if (displayLoc == NEW) {

                try {

                    // Make result image of the new data type
                    resultImage = new ModelImage(dataType, destExtents,
                                                 makeImageName(image.getImageName(), "_changed"));

                    if (image.getNDims() == 3) {
                    for (int n = 0; n < resultImage.getExtents()[2]; n++) {
                        resultImage.getFileInfo(n).setEndianess(endianess);
                    }
                    }
                    else {
                    	for (int n = 0; n < resultImage.getExtents()[2]*resultImage.getExtents()[3]; n++) {
	                        resultImage.getFileInfo(n).setEndianess(endianess);
	                    }	
                    }

                    // Make algorithm
                    changeTypeAlgo = new AlgorithmChangeType(resultImage, image, inTempMin, inTempMax, outTempMin,
                                                             outTempMax, processIndep);

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed or failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    changeTypeAlgo.addListener(this);

                    createProgressBar(image.getImageName(), changeTypeAlgo);

                    // Hide dialog
                    setVisible(false);

                    if (isRunInSeparateThread()) {

                        // Start the thread as a low priority because we wish to still have user interface work fast
                        if (changeTypeAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {

                        changeTypeAlgo.run();
                    }
                } catch (OutOfMemoryError x) {
                    MipavUtil.displayError("Dialog change type: unable to allocate enough memory");

                    if (resultImage != null) {
                        resultImage.disposeLocal(); // Clean up image memory
                        resultImage = null;
                    }

                    return;
                }
            } else {

                try {

                    if (image.getNDims() == 3) {
                    for (int n = 0; n < image.getExtents()[2]; n++) {
                        image.getFileInfo(n).setEndianess(endianess);
                    }
                    }
                    else {
                    	for (int n = 0; n < image.getExtents()[2]*image.getExtents()[3]; n++) {
	                        image.getFileInfo(n).setEndianess(endianess);
	                    }	
                    }

                    // Make algorithm
                    changeTypeAlgo = new AlgorithmChangeType(image, dataType, inTempMin, inTempMax, outTempMin,
                                                             outTempMax, processIndep);

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed or failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    changeTypeAlgo.addListener(this);

                    createProgressBar(image.getImageName(), changeTypeAlgo);

                    // Hide dialog
                    setVisible(false);

                    // These next lines set the titles in all frames where the source image is displayed to
                    // "locked - " image name so as to indicate that the image is now read/write locked!
                    // The image frames are disabled and then unregistered from the userinterface until the
                    // algorithm has completed.
                    Vector<ViewImageUpdateInterface> imageFrames = image.getImageFrameVector();
                    titles = new String[imageFrames.size()];

                    for (int i = 0; i < imageFrames.size(); i++) {
                        titles[i] = ((Frame) (imageFrames.elementAt(i))).getTitle();
                        ((Frame) (imageFrames.elementAt(i))).setTitle("Locked: " + titles[i]);
                        ((Frame) (imageFrames.elementAt(i))).setEnabled(false);
                        userInterface.unregisterFrame((Frame) (imageFrames.elementAt(i)));
                    }

                    if (isRunInSeparateThread()) {

                        // Start the thread as a low priority because we wish to still have user interface work fast
                        if (changeTypeAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {

                        changeTypeAlgo.run();
                    }
                } catch (OutOfMemoryError x) {
                    MipavUtil.displayError("Dialog change type: unable to allocate enough memory");

                    return;
                }
            }
        }
    }

    /**
     * Used to perform actions after the execution of the algorithm is completed (e.g., put the result image in the
     * image table). Defaults to no action, override to actually have it do something.
     */
    protected void doPostAlgorithmActions() {

        if (displayLoc == NEW) {
            AlgorithmParameters.storeImageInRunner(getResultImage());
        }
    }

    /**
     * Set the dialog GUI using the script parameters while running this algorithm as part of a script.
     */
    protected void setGUIFromParams() {
        image = scriptParameters.retrieveInputImage();
        userInterface = ViewUserInterface.getReference();
        parentFrame = image.getParentFrame();

        if (scriptParameters.doOutputNewImage()) {
            displayLoc = NEW;
        } else {
            displayLoc = REPLACE;
        }

        processIndep = scriptParameters.doProcess3DAs25D();

        dataType = scriptParameters.getParams().getInt("data_type");
        endianess = scriptParameters.getParams().getBoolean("is_big_endian");

        useDefaultRanges = scriptParameters.getParams().getBoolean("do_use_default_input_ranges");

        if (useDefaultRanges) {
            setDefaultRanges();
        } else {
            double[] inRange = scriptParameters.getParams().getList("input_range").getAsDoubleArray();
            inTempMin = inRange[0];
            inTempMax = inRange[1];
        }

        double[] outRange = scriptParameters.getParams().getList("output_range").getAsDoubleArray();
        outTempMin = outRange[0];
        outTempMax = outRange[1];

        if (!image.isComplexImage()) {
	        if (inTempMin >= inTempMax) {
	            MipavUtil.displayError("Input minimum must be less than the input maximum (" + inTempMin + " >= " +
	                                   inTempMax + ").");
	
	            return;
	        }
	
	        if (outTempMin >= outTempMax) {
	            MipavUtil.displayError("Output minimum must be less than the output maximum (" + outTempMin + " >= " +
	                                   outTempMax + ").");
	
	            return;
	        }
	
	        if (inTempMin < image.getMin()) {
	            MipavUtil.displayError("Input minimum must be within the range of the input image (" + inTempMin + " < " +
	                                   image.getMin() + ").");
	
	            return;
	        }
        } // if (!image.isComplexImage())

        if (inTempMax > image.getMax()) {
            MipavUtil.displayError("Input maximum must be within the range of the input image (" + inTempMax + " > " +
                                   image.getMax() + ").");

            return;
        }
    }

    /**
     * Record the parameters just used to run this algorithm in a script.
     *
     * @throws  ParserException  If there is a problem creating/recording the new parameters.
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        scriptParameters.storeOutputImageParams(resultImage, (displayLoc == NEW));

        scriptParameters.storeProcess3DAs25D(processIndep);

        scriptParameters.getParams().put(ParameterFactory.newParameter("data_type", dataType));
        scriptParameters.getParams().put(ParameterFactory.newParameter("is_big_endian", endianess));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_use_default_input_ranges",
                                                                       useDefaultRanges));

        if (!useDefaultRanges) {
            scriptParameters.getParams().put(ParameterFactory.newParameter("input_range",
                                                                           new double[] { inTempMin, inTempMax }));
        }

        scriptParameters.getParams().put(ParameterFactory.newParameter("output_range",
                                                                       new double[] { outTempMin, outTempMax }));
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setForeground(Color.black);
        setTitle("Convert Image Type");

        GridBagLayout gblay = new GridBagLayout();
        JPanel panelImageType = new JPanel(new GridBagLayout());
        panelImageType.setForeground(Color.black);
        panelImageType.setBorder(buildTitledBorder("Image Type"));

        ButtonGroup group1 = new ButtonGroup();

        if (image.isColorImage()) {
            radioARGB = new JRadioButton("ARGB", false);
            radioARGB.setFont(serif12);
            radioARGB.addItemListener(this);
            group1.add(radioARGB);

            radioARGB_USHORT = new JRadioButton("ARGB_USHORT", false);
            radioARGB_USHORT.setFont(serif12);
            radioARGB_USHORT.addItemListener(this);
            group1.add(radioARGB_USHORT);

            radioARGB_FLOAT = new JRadioButton("ARGB_FLOAT", false);
            radioARGB_FLOAT.setFont(serif12);
            radioARGB_FLOAT.addItemListener(this);
            group1.add(radioARGB_FLOAT);
        } // if (image.isColorImage())
        else if (image.isComplexImage()) {
        	image.calcMinMaxMag(false);
            radioComplex = new JRadioButton("Float complex", false);
            radioComplex.setFont(serif12);
            radioComplex.addItemListener(this);
            group1.add(radioComplex);
            
            radioDComplex = new JRadioButton("Double complex", false);
            radioDComplex.setFont(serif12);
            radioDComplex.addItemListener(this);
            group1.add(radioDComplex);
        } // else if (image.isComplexImage())
        else { // black and white image
            radioBool = new JRadioButton("Boolean", false);
            radioBool.setFont(serif12);
            radioBool.addItemListener(this);
            group1.add(radioBool);

            radioByte = new JRadioButton("Byte", false);
            radioByte.setFont(serif12);
            radioByte.addItemListener(this);
            group1.add(radioByte);

            radioUByte = new JRadioButton("Unsigned Byte", false);
            radioUByte.setFont(serif12);
            radioUByte.addItemListener(this);
            group1.add(radioUByte);

            radioShort = new JRadioButton("Short", false);
            radioShort.setFont(serif12);
            radioShort.addItemListener(this);
            group1.add(radioShort);

            radioUShort = new JRadioButton("Unsigned Short", false);
            radioUShort.setFont(serif12);
            radioUShort.addItemListener(this);
            group1.add(radioUShort);

            radioInt = new JRadioButton("Integer", false);
            radioInt.setFont(serif12);
            radioInt.addItemListener(this);
            group1.add(radioInt);

            radioUInt = new JRadioButton("Unsigned Integer", false);
            radioUInt.setFont(serif12);
            radioUInt.addItemListener(this);
            group1.add(radioUInt);

            radioLong = new JRadioButton("Long", false);
            radioLong.setFont(serif12);
            radioLong.addItemListener(this);
            group1.add(radioLong);

            radioFloat = new JRadioButton("Float", false);
            radioFloat.setFont(serif12);
            radioFloat.addItemListener(this);
            group1.add(radioFloat);

            radioDouble = new JRadioButton("Double", false);
            radioDouble.setFont(serif12);
            radioDouble.addItemListener(this);
            group1.add(radioDouble);
        } // else black and white image

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.weightx = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.insets = new Insets(0, 5, 0, 5);

        if (image.isColorImage()) {
            panelImageType.add(radioARGB, gbc);
            gbc.gridy = 1;
            panelImageType.add(radioARGB_USHORT, gbc);
            gbc.gridy = 2;
            panelImageType.add(radioARGB_FLOAT, gbc);
        } // if (image.isColorImage())
        else if (image.isComplexImage()) {
        	panelImageType.add(radioComplex, gbc);
        	gbc.gridy = 1;
        	panelImageType.add(radioDComplex, gbc);
        } // else if (image.isComplexImage())
        else { // black and white image
            panelImageType.add(radioBool, gbc);
            gbc.gridy = 1;
            panelImageType.add(radioByte, gbc);
            gbc.gridy = 2;
            panelImageType.add(radioUByte, gbc);
            gbc.gridy = 3;
            panelImageType.add(radioShort, gbc);
            gbc.gridy = 4;
            panelImageType.add(radioUShort, gbc);
            gbc.gridy = 5;
            panelImageType.add(radioInt, gbc);
            gbc.gridy = 6;
            panelImageType.add(radioUInt, gbc);
            gbc.gridy = 7;
            panelImageType.add(radioLong, gbc);
            gbc.gridy = 8;
            panelImageType.add(radioFloat, gbc);
            gbc.gridy = 9;
            panelImageType.add(radioDouble, gbc);
        } // else black and white image

        JPanel panelInRange = new JPanel(gblay);
        panelInRange.setForeground(Color.black);
        panelInRange.setBorder(buildTitledBorder("Range of input values"));

        inputRangeGroup = new ButtonGroup();
        fullRangeRadio = new JRadioButton("Use entire image range.", true);
        fullRangeRadio.setFont(serif12);
        fullRangeRadio.setActionCommand("FullRange");
        fullRangeRadio.addActionListener(this);
        inputRangeGroup.add(fullRangeRadio);
        useDefaultRanges = true;

        userRangeRadio = new JRadioButton("Use user defined range.", false);
        userRangeRadio.setFont(serif12);
        userRangeRadio.setActionCommand("UserRange");
        userRangeRadio.addActionListener(this);
        inputRangeGroup.add(userRangeRadio);

        
        inMin = (float) image.getMin();
        inMax = (float) image.getMax();

        String tempStr = new String("  Start input range ( " + makeString((float) inMin, 12) + " - " +
                                    makeString((float) inMax, 12) + " ).");

        inStart = new JLabel(tempStr);
        inStart.setFont(serif12);
        inStart.setForeground(Color.black);
        inStart.setEnabled(false);

        textInStart = new JTextField();
        textInStart.setText(makeString((float) inMin, 12));
        textInStart.setFont(serif12);
        textInStart.addFocusListener(this);
        textInStart.setEnabled(false);

        if (image.isComplexImage()) {
        	tempStr = new String(" Maximum input magnitude ("+ makeString((float) inMax, 12) + " ).");
        }
        else {
            tempStr = new String("  End input range ( " + makeString((float) inMin, 12) + " - " +
                             makeString((float) inMax, 12) + " ).");
        }

        inEnd = new JLabel(tempStr);
        inEnd.setFont(serif12);
        inEnd.setForeground(Color.black);
        inEnd.setEnabled(false);

        textInEnd = new JTextField();
        textInEnd.setText(makeString((float) inMax, 12));
        textInEnd.setFont(serif12);
        textInEnd.addFocusListener(this);
        textInEnd.setEnabled(false);

        // check to see if HistoLUT frame is open and threshold values selected
        if ((image.getHistoLUTFrame() != null) && image.getHistoLUTFrame().isThresholding()) {
            userRangeRadio.setSelected(true);
            fullRangeRadio.setSelected(false);

            textInStart.setText(makeString(image.getHistoLUTFrame().getLowerThreshold(), 12));
            textInEnd.setText(makeString(image.getHistoLUTFrame().getUpperThreshold(), 12));
            inStart.setEnabled(true);
            inEnd.setEnabled(true);
            textInStart.setEnabled(true);
            textInEnd.setEnabled(true);
        }

        // position radio buttons for full range or user range
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        panelInRange.add(fullRangeRadio, gbc);
        gbc.gridx = 1;
        panelInRange.add(userRangeRadio, gbc);

        // position user defined range
        Insets normal = gbc.insets;
        Insets indented = new Insets(normal.bottom, 15, normal.right, normal.top);
        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.insets = indented;
        if (!image.isComplexImage()) {
            panelInRange.add(inStart, gbc);
            gbc.gridy++;
        }
        panelInRange.add(inEnd, gbc);
        gbc.gridx = 1;
        gbc.gridy = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.insets = normal;
        if (!image.isComplexImage()) {
        	panelInRange.add(textInStart, gbc);
            gbc.gridy++;
        }
        panelInRange.add(textInEnd, gbc);

        JPanel panelOutRange = new JPanel(gblay);
        panelOutRange.setForeground(Color.black);
        panelOutRange.setBorder(buildTitledBorder("Range of output values"));

        outStart = new JLabel();
        outStart.setFont(serif12);
        outStart.setForeground(Color.black);

        outEnd = new JLabel();
        outEnd.setFont(serif12);
        outEnd.setForeground(Color.black);

        textOutStart = new JTextField(7);
        textOutStart.setFont(serif12);

        textOutEnd = new JTextField(7);
        textOutEnd.setFont(serif12);

        processIndepBox = new JCheckBox("Process each slice independently (2.5D)");
        processIndepBox.setFont(serif12);
        processIndepBox.setEnabled(image.getNDims() > 2);

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        if (!image.isComplexImage()) {
            panelOutRange.add(outStart, gbc);
            gbc.gridy++;
        }
        panelOutRange.add(outEnd, gbc);
        gbc.gridx = 1;
        gbc.gridy = 0;
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        if (!image.isComplexImage()) {
        	panelOutRange.add(textOutStart, gbc);
            gbc.gridy++;
        }
        panelOutRange.add(textOutEnd, gbc);
        gbc.gridx = 0;
        gbc.gridy++;
        gbc.insets = new Insets(0, 0, 0, 0);
        panelOutRange.add(processIndepBox, gbc);

        outStart.setText("Starting range (-2.147 E+9 to 2.147 E+9).");
        outEnd.setText("Ending range (-2.147 E+9 to 2.147 E+9).");
        textOutStart.setText("0");
        textOutEnd.setText("4095");
        processIndepBox.setSelected(false);
        // processIndepBox.addItemListener(this);

        JPanel destinationPanel = new JPanel(new GridBagLayout());
        destinationPanel.setForeground(Color.black);
        destinationPanel.setBorder(buildTitledBorder("Destination"));

        ButtonGroup destinationGroup = new ButtonGroup();
        newImage = new JRadioButton("New image", true);
        newImage.setFont(serif12);
        destinationGroup.add(newImage);

        replaceImage = new JRadioButton("Replace image", false);
        replaceImage.setFont(serif12);
        destinationGroup.add(replaceImage);

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.gridheight = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.weightx = 1;
        gbc.anchor = GridBagConstraints.WEST;
        destinationPanel.add(newImage, gbc);
        gbc.gridy = 1;
        destinationPanel.add(replaceImage, gbc);

        if (image.getLockStatus() == ModelStorageBase.UNLOCKED) { // Only if the image is unlocked can it be replaced.
            replaceImage.setEnabled(true);
        } else {
            replaceImage.setEnabled(false);
        }

        JPanel endianessPanel = new JPanel(new GridBagLayout());
        endianessPanel.setForeground(Color.black);
        endianessPanel.setBorder(buildTitledBorder("Endianess"));

        boolean lFlag, bFlag;

        if (image.getFileInfo(0).getEndianess() == FileBase.LITTLE_ENDIAN) {
            lFlag = true;
            bFlag = false;
        } else {
            lFlag = false;
            bFlag = true;
        }

        ButtonGroup endianessGroup = new ButtonGroup();
        littleEnd = new JRadioButton("Little endian", lFlag);
        littleEnd.setFont(serif12);
        endianessGroup.add(littleEnd);

        bigEnd = new JRadioButton("Big endian", bFlag);
        bigEnd.setFont(serif12);
        endianessGroup.add(bigEnd);

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.gridheight = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.weightx = 1;
        gbc.anchor = GridBagConstraints.WEST;
        endianessPanel.add(littleEnd, gbc);
        gbc.gridy = 1;
        endianessPanel.add(bigEnd, gbc);

        JPanel buttonPanel = new JPanel();

        /*
         * buildOKButton(); buildCancelButton(); buttonPanel.add(OKButton); buttonPanel.add(cancelButton);
         */
        buttonPanel.add(buildButtons());

        JPanel mainPanel = new JPanel();
        mainPanel.setLayout(new BoxLayout(mainPanel, BoxLayout.Y_AXIS));
        mainPanel.add(destinationPanel);
        mainPanel.add(panelInRange);
        mainPanel.add(panelOutRange);
        mainPanel.add(endianessPanel);

        getContentPane().add(mainPanel);
        getContentPane().add(panelImageType, BorderLayout.WEST);
        getContentPane().add(buttonPanel, BorderLayout.SOUTH);
        pack();

        if (image.getType() == ModelStorageBase.BOOLEAN) {
            outStart.setText(" ");
            outEnd.setText("Pixels != 0 are set to 1. ");
            textOutStart.setText("0");
            textOutStart.setEnabled(false);
            textOutEnd.setText("1");
            textOutEnd.setEnabled(false);
            inMin = 0;
            inMax = 1;
            outMin = 0;
            outMax = 1;
            radioBool.setSelected(true);
        } else if (image.getType() == ModelStorageBase.BYTE) {
            outStart.setText("Starting range (-128 to 127).");
            outEnd.setText("Ending range (-128 to 127).");
            //textOutStart.setText("-128");
            //textOutEnd.setText("127");

            if(image.getMin() > 127) {
            	textOutStart.setText("0");
            }else {
            	Byte b1 = new Byte((byte)image.getMin());
            	textOutStart.setText(b1.toString());
            }
            
            if(image.getMax() > Byte.MAX_VALUE) {
            	textOutEnd.setText(String.valueOf(Byte.MAX_VALUE));
            }else {
            	Byte b2 = new Byte((byte)image.getMax());
            	textOutEnd.setText(b2.toString());
            }
            inMin = -128;
            inMax = 127;
            outMin = -128;
            outMax = 127;
            radioByte.setSelected(true);
        } else if (image.getType() == ModelStorageBase.UBYTE) {
            outStart.setText("Starting range (0 to 255).");
            outEnd.setText("Ending range (0 to 255).");
            //textOutStart.setText("0");
            //textOutEnd.setText("255");
            if(image.getMin() <= 0 || image.getMin() > 255) {
            	textOutStart.setText("0");
            }else {
            	Short s1 = new Short((short)image.getMin());
            	textOutStart.setText(s1.toString());
            }
            
            if(image.getMax() > 255) {
            	textOutEnd.setText("255");
            }else {
            	Short s2 = new Short((short)image.getMax());
            	textOutEnd.setText(s2.toString());
            }
            inMin = 0;
            inMax = 255;
            outMin = 0;
            outMax = 255;
            radioUByte.setSelected(true);
        } else if (image.getType() == ModelStorageBase.SHORT) {
            outStart.setText("Starting range (-32768 to 32767).");
            outEnd.setText("Ending range (-32768 to 32767).");
            //textOutStart.setText("0");
            //textOutEnd.setText("3071");

            Short s1 = new Short((short)image.getMin());
            textOutStart.setText(s1.toString());
            
            if(image.getMax() > Short.MAX_VALUE) {
            	textOutEnd.setText("32767");
            }else {
            	Short s2 = new Short((short)image.getMax());
            	textOutEnd.setText(s2.toString());
            }
            inMin = -32768;
            inMax = 32767;
            outMin = -32768;
            outMax = 32767;
            radioShort.setSelected(true);
        } else if (image.getType() == ModelStorageBase.USHORT) {
            outStart.setText("Starting range (0 to 65535).");
            outEnd.setText("Ending range (0 to 65535).");
            //textOutStart.setText("0");
            //textOutEnd.setText("4095");
            if(image.getMin() <= 0) {
            	textOutStart.setText("0");
            }else {
            	Integer i1 = new Integer((int)image.getMin());
            	textOutStart.setText(i1.toString());
            }
            
            if(image.getMax() > 65535) {
            	textOutEnd.setText("65535");
            }else {
            	Integer i2 = new Integer((int)image.getMax());
            	textOutEnd.setText(i2.toString());
            }
            inMin = 0;
            inMax = 65535;
            outMin = 0;
            outMax = 65535;
            radioUShort.setSelected(true);
        } else if (image.getType() == ModelStorageBase.INTEGER) {
            outStart.setText("Starting range (-2.147 E+9 to 2.147 E+9).");
            outEnd.setText("Ending range (-2.147 E+9 to 2.147 E+9).");
            //textOutStart.setText("0");
            //textOutEnd.setText("4095");
            Integer i1 = new Integer((int)image.getMin());
            textOutStart.setText(i1.toString());
            
            if(image.getMax() > Integer.MAX_VALUE) {
            	textOutEnd.setText(String.valueOf(Integer.MAX_VALUE));
            }else {
            	Integer i2 = new Integer((int)image.getMax());
            	textOutEnd.setText(i2.toString());
            }
            inMin = Integer.MIN_VALUE;
            inMax = Integer.MAX_VALUE;
            outMin = Integer.MIN_VALUE;
            outMax = Integer.MAX_VALUE;
            radioInt.setSelected(true);
        } else if (image.getType() == ModelStorageBase.UINTEGER) {
            outStart.setText("Starting range (0 to 4.29 E+9).");
            outEnd.setText("Ending range (0 to 4.29 E+9).");
            //textOutStart.setText("0");
            //textOutEnd.setText("4095");
            if(image.getMin() <= 0) {
            	textOutStart.setText("0");
            }else {
            	Long l1 = new Long((long)image.getMin());
            	textOutStart.setText(l1.toString());
            }
            
            if(image.getMax() > 4294967295L) {
            	textOutEnd.setText("4294967295");
            }else {
            	Long l2 = new Long((long)image.getMax());
            	textOutEnd.setText(l2.toString());
            }
            inMin = 0;
            inMax = 4294967295L;
            outMin = 0;
            outMax = 4294967295L;
            radioUInt.setSelected(true);
        } else if (image.getType() == ModelStorageBase.LONG) {
            outStart.setText("Starting range (-9.22 E+18 to 9.22 E+18).");
            outEnd.setText("Ending range (-9.22 E+18 to 9.22 E+18).");
            //textOutStart.setText("0");
            //textOutEnd.setText("4095");
            Long l1 = new Long((long)image.getMin());
            textOutStart.setText(l1.toString());

            if(image.getMax() > Long.MAX_VALUE) {
            	textOutEnd.setText(String.valueOf(Long.MAX_VALUE));
            }else {
            	Long l2 = new Long((long)image.getMax());
            	textOutEnd.setText(l2.toString());
            }
            inMin = Long.MIN_VALUE;
            inMax = Long.MAX_VALUE;
            outMin = Long.MIN_VALUE;
            outMax = Long.MAX_VALUE;
            radioLong.setSelected(true);
        } else if (image.getType() == ModelStorageBase.FLOAT) {
            outStart.setText("Starting range (-3.40 E+38  to 3.40 E+38).");
            outEnd.setText("Ending range (-3.40 E+38 to 3.40 E+38).");
            //textOutStart.setText("0");
            //textOutEnd.setText("1023");
            Float f1 = new Float((float)image.getMin());
            textOutStart.setText(f1.toString());
            
            if(image.getMax() > Float.MAX_VALUE) {
            	textOutEnd.setText(String.valueOf(Float.MAX_VALUE));
            }else {
            	Float f2 = new Float((float)image.getMax());
            	textOutEnd.setText(f2.toString());
            }
            inMin = -Float.MAX_VALUE;
            inMax = Float.MAX_VALUE;
            outMin = -Float.MAX_VALUE;
            outMax = Float.MAX_VALUE;
            radioFloat.setSelected(true);
        } else if (image.getType() == ModelStorageBase.DOUBLE) {
            outStart.setText("Starting range (-1.8 E+308 to 1.8 E+308).");
            outEnd.setText("Ending range (-1.8 E+308 to 1.8 E+308).");
            //textOutStart.setText("0");
            //textOutEnd.setText("1023");
            Double d1 = new Double((double)image.getMin());
            textOutStart.setText(d1.toString());

                 
            if(image.getMax() > Double.MAX_VALUE) {
            	textOutEnd.setText(String.valueOf(Double.MAX_VALUE));
            }else {
            	Double d2 = new Double((double)image.getMax());
            	textOutEnd.setText(d2.toString());
            }
            inMin = -Double.MAX_VALUE;
            inMax = Double.MAX_VALUE;
            outMin = -Double.MAX_VALUE;
            outMax = Double.MAX_VALUE;
            radioDouble.setSelected(true);
        } else if (image.getType() == ModelStorageBase.ARGB) {
            outStart.setText("Starting range (0 to 255).");
            outEnd.setText("Ending range (0 to 255).");
            //textOutStart.setText("0");
            //textOutEnd.setText("255");
            if(image.getMin() <= 0 || image.getMin() > 255) {
            	textOutStart.setText("0");
            }else {
            	Integer i = new Integer((int)image.getMin());
            	textOutStart.setText(i.toString());
            }
            
            if(image.getMax() > 255) {
            	textOutEnd.setText("255");
            }else {
            	Integer i = new Integer((int)image.getMax());
            	textOutEnd.setText(i.toString());
            }
            inMin = 0;
            inMax = 255;
            outMin = 0;
            outMax = 255;
            radioARGB.setSelected(true);
        } else if (image.getType() == ModelStorageBase.ARGB_USHORT) {
            outStart.setText("Starting range (0 to 65535).");
            outEnd.setText("Ending range (0 to 65535).");
            //textOutStart.setText("0");
            //textOutEnd.setText("4095");
            if(image.getMin() <= 0) {
            	textOutStart.setText("0");
            }else {
            	Integer i1 = new Integer((int)image.getMin());
            	textOutStart.setText(i1.toString());
            }
            
            if(image.getMax() > 65535) {
            	textOutEnd.setText("65535");
            }else {
            	Integer i2 = new Integer((int)image.getMax());
            	textOutEnd.setText(i2.toString());
            }
            inMin = 0;
            inMax = 65535;
            outMin = 0;
            outMax = 65535;
            radioARGB_USHORT.setSelected(true);
        } else if (image.getType() == ModelStorageBase.ARGB_FLOAT) {
            outStart.setText("Starting range (-3.40 E+38  to 3.40 E+38).");
            outEnd.setText("Ending range (-3.40 E+38 to 3.40 E+38).");
            //textOutStart.setText("0");
            //textOutEnd.setText("1023");
            Float f1 = new Float((int)image.getMin());
            textOutStart.setText(f1.toString());
            
            
            if(image.getMax() > Float.MAX_VALUE) {
            	textOutEnd.setText(String.valueOf(Float.MAX_VALUE));
            }else {
            	Float f2 = new Float((float)image.getMax());
            	textOutEnd.setText(f2.toString());
            }
            inMin = -Float.MAX_VALUE;
            inMax = Float.MAX_VALUE;
            outMin = -Float.MAX_VALUE;
            outMax = Float.MAX_VALUE;
            radioARGB_FLOAT.setSelected(true);
        } else if (image.getType() == ModelStorageBase.COMPLEX) {
        	// outStart.setText("Starting maximum magnitude ( 0.0 to 3.40 E+38).");
            outEnd.setText("Ending maximum magnitude (0.0 to 3.40 E+38).");
            //textOutStart.setText("0");
            //textOutEnd.setText("1023");
            //Float f1 = new Float((int)image.getMin());
            //textOutStart.setText(f1.toString());
            
            
            if(image.getMax() > Float.MAX_VALUE) {
            	textOutEnd.setText(String.valueOf(Float.MAX_VALUE));
            }else {
            	Float f2 = new Float((float)image.getMax());
            	textOutEnd.setText(f2.toString());
            }
            inMin = 0.0;
            inMax = Float.MAX_VALUE;
            outMin = 0.0;
            outMax = Float.MAX_VALUE;
            radioComplex.setSelected(true);	
        } else if (image.getType() == ModelStorageBase.DCOMPLEX) {
        	//outStart.setText("Starting maximum magnitude (0 to 1.8 E+308).");
            outEnd.setText("Ending maximum magnitude (0.0 to 1.8 E+308).");
            //textOutStart.setText("0");
            //textOutEnd.setText("1023");
            //Double d1 = new Double((double)image.getMin());
            //textOutStart.setText(d1.toString());

                 
            if(image.getMax() > Double.MAX_VALUE) {
            	textOutEnd.setText(String.valueOf(Double.MAX_VALUE));
            }else {
            	Double d2 = new Double((double)image.getMax());
            	textOutEnd.setText(d2.toString());
            }
            inMin = 0.0;
            inMax = Double.MAX_VALUE;
            outMin = 0.0;
            outMax = Double.MAX_VALUE;
            radioDComplex.setSelected(true);
        }

        setVisible(true);
    }

    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {
        String tmpStr;

        System.gc();

        if (image.isColorImage()) {

            if (radioARGB.isSelected()) {
                dataType = ModelStorageBase.ARGB;
            } else if (radioARGB_USHORT.isSelected()) {
                dataType = ModelStorageBase.ARGB_USHORT;
            } else if (radioARGB_FLOAT.isSelected()) {
                dataType = ModelStorageBase.ARGB_FLOAT;
            } else {
                dataType = ModelStorageBase.ARGB;
            }
        } // if (image.isColorImage())
        else if (image.isComplexImage()) {
        	if (radioComplex.isSelected()) {
        		dataType = ModelStorageBase.COMPLEX;
        	} else if (radioDComplex.isSelected()) {
        		dataType = ModelStorageBase.DCOMPLEX;
        	}
        } // else if (image.isComplexImage())
        else { // black and white image

            if (radioBool.isSelected()) {
                dataType = ModelStorageBase.BOOLEAN;
            } else if (radioByte.isSelected()) {
                dataType = ModelStorageBase.BYTE;
            } else if (radioUByte.isSelected()) {
                dataType = ModelStorageBase.UBYTE;
            } else if (radioShort.isSelected()) {
                dataType = ModelStorageBase.SHORT;
            } else if (radioUShort.isSelected()) {
                dataType = ModelStorageBase.USHORT;
            } else if (radioInt.isSelected()) {
                dataType = ModelStorageBase.INTEGER;
            } else if (radioUInt.isSelected()) {
                dataType = ModelStorageBase.UINTEGER;
            } else if (radioLong.isSelected()) {
                dataType = ModelStorageBase.LONG;
            } else if (radioFloat.isSelected()) {
                dataType = ModelStorageBase.FLOAT;
            } else if (radioDouble.isSelected()) {
                dataType = ModelStorageBase.DOUBLE;
            } else {
                dataType = ModelStorageBase.UBYTE;
            }
        } // else black and white image

        if (replaceImage.isSelected()) {
            displayLoc = REPLACE;
        } else if (newImage.isSelected()) {
            displayLoc = NEW;
        }

        if (littleEnd.isSelected()) {
            endianess = FileBase.LITTLE_ENDIAN;
        } else if (bigEnd.isSelected()) {
            endianess = FileBase.BIG_ENDIAN;
        }

        // check to see if default ranges should be used
        if (useDefaultRanges == true) {
            this.setDefaultRanges();
        } // otherwise, get the values from the user defined input ranges
        else {
        	if (!image.isComplexImage()) {
	            tmpStr = textInStart.getText();
	
	            if (testParameter(tmpStr, image.getMin(), image.getMax())) {
	                inTempMin = Double.valueOf(tmpStr).doubleValue();
	            } else {
	                textInStart.requestFocus();
	                textInStart.selectAll();
	
	                return false;
	            }
        	}

            tmpStr = textInEnd.getText();

            if (testParameter(tmpStr, image.getMin(), image.getMax())) {
                inTempMax = Double.valueOf(tmpStr).doubleValue();
            } else {
                textInEnd.requestFocus();
                textInEnd.selectAll();

                return false;
            }
        }

        if (!image.isComplexImage()) {
	        tmpStr = textOutStart.getText();
	
	        if (testParameter(tmpStr, outMin, outMax)) {
	            outTempMin = Double.valueOf(tmpStr).doubleValue();
	        } else {
	            textOutStart.requestFocus();
	            textOutStart.selectAll();
	
	            return false;
	        }
        }

        tmpStr = textOutEnd.getText();

        if (testParameter(tmpStr, outMin, outMax)) {
            outTempMax = Double.valueOf(tmpStr).doubleValue();
        } else {
            textOutEnd.requestFocus();
            textOutEnd.selectAll();

            return false;
        }

        processIndep = processIndepBox.isSelected();

        return true;
    }

}
