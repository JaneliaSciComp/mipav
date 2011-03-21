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
import gov.nih.mipav.model.scripting.parameters.ParameterBoolean;
import gov.nih.mipav.model.scripting.parameters.ParameterDouble;
import gov.nih.mipav.model.scripting.parameters.ParameterExternalImage;
import gov.nih.mipav.model.scripting.parameters.ParameterFactory;
import gov.nih.mipav.model.scripting.parameters.ParameterImage;
import gov.nih.mipav.model.scripting.parameters.ParameterInt;
import gov.nih.mipav.model.scripting.parameters.ParameterTable;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.ActionDiscovery;
import gov.nih.mipav.view.dialogs.ActionMetadata;
import gov.nih.mipav.view.dialogs.AlgorithmParameters;
import gov.nih.mipav.view.dialogs.JDialogBase;
import gov.nih.mipav.view.dialogs.JDialogScriptableBase;
import gov.nih.mipav.view.dialogs.MipavActionMetadata;
import gov.nih.mipav.view.dialogs.JDialogTreT1.ExitStatus;

import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.Enumeration;

import javax.swing.*;

/**
 * This class displays a basic dialog for a MIPAV plug-in.  The dialog has been made scriptable, 
 * meaning it can be executed and recorded as part of a script.  It implements AlgorithmInterface,
 * meaning it has methods for listening to and recording the progress of an algorithm.
 * 
 * @version  June 4, 2010
 * @see      JDialogBase
 * @see      AlgorithmInterface
 *
 * @author Justin Senseney (SenseneyJ@mail.nih.gov)
 * @see http://mipav.cit.nih.gov
 */
public class PlugInDialogSWI extends JDialogScriptableBase implements AlgorithmInterface, ActionDiscovery {
    
    
    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /**declare UID */

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Result image. */
    private ModelImage resultImage = null;

    /** This source image is typically set by the constructor */
    private ModelImage image; // 
    
    /** This is the SWIs algorithm */
    private PlugInAlgorithmSWI swiAlgo = null;

	/** The variable representing whether the blur should be performed. */
	private boolean doErnst;

    private String[] titles;
    
    private GuiBuilder guiBuilder;

    private JTextField maskThresholdField;

    private JTextField roFilterSizeField;

    private JTextField peFilterSizeField;

    private double maskThreshold;

    private int roFilterSize;

    private int peFilterSize;

    private JComboBox magnitudeCombo;

    private JComboBox phaseCombo;
    
    private JCheckBox showInterImagesBox;

    private JTextField multFactorField;

    private int multFactor;

    private ModelImage magImage;

    private ModelImage phaseImage;

    private boolean showInterImages;
    
    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor used for instantiation during script execution (required for dynamic loading).
     */
    public PlugInDialogSWI() { }

    /**
     * Creates new dialog for kidney segmentation from an abdominal cavity image using a plugin.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public PlugInDialogSWI(Frame theParentFrame, ModelImage im) {
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
       if (algorithm instanceof PlugInAlgorithmSWI) {
            Preferences.debug("Elapsed: " + algorithm.getElapsedTime());
            image.clearMask();
            resultImage = algorithm.getDestImage();
            if ((swiAlgo.isCompleted() == true) && (resultImage != null)) {

                // The algorithm has completed and produced a new image to be displayed.
                updateFileInfo(image, resultImage);

                resultImage.clearMask();

                try {
                    new ViewJFrameImage(resultImage);
                } catch (OutOfMemoryError error) {
                    System.gc();
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }
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

    } // end algorithmPerformed()

    
    /**
     * Once all the necessary variables are set, call the kidney segmentation algorithm
     */
    protected void callAlgorithm() {

        try {
            String name = makeImageName(image.getImageName(), "_T1Map");
            resultImage = (ModelImage) image.clone();
            resultImage.setImageName(name);
            
        
            
            swiAlgo = new PlugInAlgorithmSWI(isScriptRunning(), resultImage, magImage, phaseImage, 
                                                    maskThreshold, roFilterSize, peFilterSize, multFactor, showInterImages);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed or failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            swiAlgo.addListener(this);
            createProgressBar(image.getImageName(), " ...", swiAlgo);

            setVisible(false); // Hide dialog

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still
                // have user interface work fast.
                if (swiAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                swiAlgo.run();
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
     * Used in turning the plugin into a script
     */
    protected void setGUIFromParams() {
    	roFilterSize = scriptParameters.getParams().getInt("roFilterSize");
    	peFilterSize = scriptParameters.getParams().getInt("peFilterSize");
    	multFactor = scriptParameters.getParams().getInt("multFactor");
    	showInterImages = scriptParameters.getParams().getBoolean("showInterImages");
    	maskThreshold = scriptParameters.getParams().getDouble("maskThreshold");
    	
    	magImage = scriptParameters.retrieveImage("MagnitudeImage");
    	image = magImage;
    	phaseImage = scriptParameters.retrieveImage("PhaseImage");
    } //end setGUIFromParams()

    /**
     * Used in turning the plugin into a script
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeImage(magImage, "MagnitudeImage");
        scriptParameters.storeImage(phaseImage, "PhaseImage");
        
        scriptParameters.getParams().put(ParameterFactory.newParameter("roFilterSize", roFilterSize));
        scriptParameters.getParams().put(ParameterFactory.newParameter("peFilterSize", peFilterSize));
        scriptParameters.getParams().put(ParameterFactory.newParameter("multFactor", multFactor));
        scriptParameters.getParams().put(ParameterFactory.newParameter("showInterImages", showInterImages));
        scriptParameters.getParams().put(ParameterFactory.newParameter("maskThreshold", maskThreshold));
        
        if(swiAlgo != null && showInterImages) {
            scriptParameters.storeOutputImageParams(swiAlgo.getkImage(), true);
            scriptParameters.storeOutputImageParams(swiAlgo.getiImage(), true);
            scriptParameters.storeOutputImageParams(swiAlgo.getBrainMask(), true);
            scriptParameters.storeOutputImageParams(swiAlgo.getkCenterImage(), true);
            scriptParameters.storeOutputImageParams(swiAlgo.getiFinal(), true);
            scriptParameters.storeOutputImageParams(swiAlgo.getPhaseMask(), true);
            scriptParameters.storeOutputImageParams(swiAlgo.getiCenter(), true);
        }
        scriptParameters.storeOutputImageParams(swiAlgo.getDestImage(), true);  //magEnhanced image
        
        //algorithm shows inter images, but referencing them here allows JIST to see them for possible post-processing
    } //end storeParamsFromGUI()
   
    private void init() {
        setForeground(Color.black);
        setTitle("SWI Plugin");
        guiBuilder = new GuiBuilder(this);
        
        try {
			setIconImage(MipavUtil.getIconImage("divinci.gif"));
		} catch (FileNotFoundException e) {
			Preferences.debug("Failed to load default icon", Preferences.DEBUG_MINOR);
		}
        
        JPanel imagePanel = new JPanel(new GridBagLayout());
        imagePanel.setBorder(MipavUtil.buildTitledBorder("Select images"));
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.insets = new Insets(3, 3, 3, 3);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 0;
        gbc.gridy = 0;
        
        titles = new String[ViewUserInterface.getReference().getRegisteredImagesNum()];
        Enumeration<String> imageStr = ViewUserInterface.getReference().getRegisteredImageNames();
        int index = 0;
        int phaseDefault = 0, magDefault = 0;
        String imageEl = null;
        while(imageStr.hasMoreElements()) {
            imageEl = imageStr.nextElement();
            if(imageEl.matches(".*(p|P)ha.*")) {
                phaseDefault = index;
            } else if(imageEl.matches(".*(m|M)ag.*")) {
                magDefault = index;
            }
            titles[index] = imageEl;
            index++;
        }
        
        
        magnitudeCombo = guiBuilder.buildComboBox("Magnitude:", titles, magDefault);
        imagePanel.add(magnitudeCombo.getParent(), gbc);
        gbc.gridy++;
        
        phaseCombo = guiBuilder.buildComboBox("Phase:", titles, phaseDefault);
        imagePanel.add(phaseCombo.getParent(), gbc);
        gbc.gridy++;
        
        JPanel paramPanel = new JPanel(new GridBagLayout());
        paramPanel.setBorder(MipavUtil.buildTitledBorder("SWI Parameters"));
        gbc = new GridBagConstraints();
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.insets = new Insets(3, 3, 3, 3);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 0;
        gbc.gridy = 0;
        
        maskThresholdField = guiBuilder.buildDecimalField("Brain Mask Threshold", .5e4);
        roFilterSizeField = guiBuilder.buildIntegerField("Ro filter size", 64);
        peFilterSizeField = guiBuilder.buildIntegerField("Pe filter size", 64);
        multFactorField = guiBuilder.buildIntegerField("Multiplication factor", 4);
        showInterImagesBox = guiBuilder.buildCheckBox("Show intermediate images", true);
        
        paramPanel.add(maskThresholdField.getParent(), gbc);
        gbc.gridy++;
        paramPanel.add(roFilterSizeField.getParent(), gbc);
        gbc.gridy++;
        paramPanel.add(peFilterSizeField.getParent(), gbc);
        gbc.gridy++;
        paramPanel.add(multFactorField.getParent(), gbc);
        gbc.gridy++;
        paramPanel.add(showInterImagesBox.getParent(), gbc);
        gbc.gridy++;
        
        JPanel mainPanel = new JPanel(new GridBagLayout());
        mainPanel.setForeground(Color.black);
        gbc = new GridBagConstraints();
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.insets = new Insets(3, 3, 3, 3);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 0;
        gbc.gridy = 0;
        
        mainPanel.add(imagePanel, gbc);
        gbc.gridy++;
        mainPanel.add(paramPanel, gbc);

        getContentPane().add(mainPanel, BorderLayout.CENTER);

        // Build the Panel that holds the OK and CANCEL Buttons
        JPanel OKCancelPanel = guiBuilder.buildOKCancelPanel();

        getContentPane().add(OKCancelPanel, BorderLayout.SOUTH);

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
	    
	    maskThreshold = Double.valueOf(maskThresholdField.getText()).doubleValue();
	    roFilterSize = Integer.valueOf(roFilterSizeField.getText()).intValue();
	    peFilterSize = Integer.valueOf(peFilterSizeField.getText()).intValue();
	    multFactor = Integer.valueOf(multFactorField.getText()).intValue();
	    showInterImages = showInterImagesBox.isSelected();
	    
	    try {
	        magImage = ViewUserInterface.getReference().getRegisteredImageByName(magnitudeCombo.getSelectedItem().toString());
            
            phaseImage = ViewUserInterface.getReference().getRegisteredImageByName(phaseCombo.getSelectedItem().toString());
	    } catch(Exception e) {
	        MipavUtil.displayError("Please select magnitude and phase images.");
	        return false;
	    }
        
		return true;
	} //end setVariables()
	
	/**
     * Provides methods for quickly building panel components. I can think of many other (better)
     * ways to do this, but for the ImageJ port this works well for now.
     * 
     * @author senseneyj
     *
     */
    private class GuiBuilder implements ActionListener {
        
        @SuppressWarnings("unused")
    	public static final int GUI_BUILDER_OK_ID = ActionEvent.RESERVED_ID_MAX + 20;

        private ArrayList<ActionListener> listenerList;
        
        private boolean passedListeners;

        private ExitStatus exit;
        
        private JDialogBase parent;
        
        public GuiBuilder(JDialogBase parent) {
            this.parent = parent;
            this.listenerList = new ArrayList<ActionListener>();
            this.exit = ExitStatus.INCOMPLETE;
        }
        
        @SuppressWarnings("unused")
        public ExitStatus getExitStatus() {
            return exit;
        }
        
        @SuppressWarnings("unused")
        public ActionListener[] getListenerList() {
            ActionListener[] list = new ActionListener[listenerList.size()];
            for(int i=0; i<listenerList.size(); i++) {
                list[i] = listenerList.get(i);
            }
            return list;
        }
        
        @SuppressWarnings("unused")
        public JRadioButton buildRadioButton(String label, boolean selected) {
            FlowLayout f = new FlowLayout();
            f.setAlignment(FlowLayout.LEFT);
            JPanel radioPanel = new JPanel(f);
            JRadioButton radioButton = new JRadioButton(label);
            radioButton.setSelected(selected);
            radioPanel.add(radioButton);
            return radioButton;
        }
        
        @SuppressWarnings("unused")
        public JCheckBox buildCheckBox(String label, boolean selected) {
            FlowLayout f = new FlowLayout();
            f.setAlignment(FlowLayout.LEFT);
            JPanel checkPanel = new JPanel(f);
            JCheckBox checkBox = new JCheckBox(label);
            checkBox.setSelected(selected);
            checkPanel.add(checkBox);
            return checkBox;
        }
        
        public JTextField buildField(String labelText, String initText) {
            FlowLayout f = new FlowLayout();
            f.setAlignment(FlowLayout.LEFT);
            JPanel panel = new JPanel(f);
            JLabel label = new JLabel(labelText);
            JTextField text = new JTextField(initText);
            text.setColumns(8);
            panel.add(label);
            panel.add(text);
            return text;
        }
        
        public JTextField buildIntegerField(final String labelText, int initNum) {
            final JTextField genericField = buildField(labelText, String.valueOf(initNum));
            ActionListener listener = new ActionListener() {
                public void actionPerformed(ActionEvent e) {
                    if(e.getSource().equals(OKButton)) {
                        try {
                            Integer.valueOf(genericField.getText());
                        } catch(NumberFormatException e1) {
                            try {
                                double d = Double.valueOf(genericField.getText());
                                if(((int)d) == d) {
                                    genericField.setText(Integer.valueOf((int)d).toString());
                                    return;
                                } else {
                                    MipavUtil.displayInfo(labelText+" must be an integer.");
                                    passedListeners = false;
                                }
                            } catch(NumberFormatException e2) {
                                MipavUtil.displayInfo(labelText+" must be an integer.");
                                passedListeners = false;
                            }
                        }
                    }
                }
            };
            genericField.addActionListener(listener);
            listenerList.add(listener);
            return genericField;
        }
        
        public JTextField buildDecimalField(final String labelText, double initNum) {
            final JTextField genericField = buildField(labelText, String.valueOf(initNum));
            ActionListener listener = new ActionListener() {
                public void actionPerformed(ActionEvent e) {
                    if(e.getSource().equals(OKButton)) {
                        try {
                            Double.valueOf(genericField.getText());
                        } catch(NumberFormatException e1) {
                            MipavUtil.displayInfo(labelText+" must be a number.");
                            passedListeners = false;
                        }
                    }
                }
            };
            genericField.addActionListener(listener);
            listenerList.add(listener);
            return genericField;
        }
        
        public JComboBox buildComboBox(String labelText, Object[] options) {
            FlowLayout f = new FlowLayout();
            f.setAlignment(FlowLayout.LEFT);
            JPanel panel = new JPanel(f);
            JLabel label = new JLabel(labelText);
            JComboBox comboBox = null;
            if(options != null) {
                
                comboBox = new JComboBox(options);
            } else {
                comboBox = new JComboBox(new String[]{"a", "B"});
            }
         
            panel.add(label);
            panel.add(comboBox);
            return comboBox;
        }
        
        public JComboBox buildComboBox(String labelText, Object[] options, int numDefault) {
            JComboBox comboBox = buildComboBox(labelText, options); //call default
            if(numDefault > comboBox.getItemCount()-1) {
                numDefault = 0;
            } else {
                comboBox.setSelectedIndex(numDefault);
            }
            //TODO: get renderer to truncate long names
            /*comboBox.setRenderer(new ListCellRenderer() {
                DefaultListCellRenderer defaultRenderer = new DefaultListCellRenderer();
                
                public Component getListCellRendererComponent(JList list,
                        Object value, int index, boolean isSelected,
                        boolean cellHasFocus) {
                    JLabel renderer = (JLabel) defaultRenderer.getListCellRendererComponent(list, value, index, isSelected, cellHasFocus);
                    if(index == -1 && value.toString().length() > 23) {
                        renderer.setText(value.toString().substring(0, 23)+"...");
                    } else {
                        //renderer.setBounds(0, 0, 300, 20);
                    }
                    System.out.println(value+" "+index);
                    return renderer;
                
                }   
            });*/
            return comboBox;
        }
        
        public JPanel buildOKCancelPanel() {
            JPanel panel = new JPanel();
            OKButton = new JButton("OK");
            OKButton.addActionListener(this);
            OKButton.setMinimumSize(MipavUtil.defaultButtonSize);
            OKButton.setPreferredSize(MipavUtil.defaultButtonSize);
            OKButton.setFont(serif12B);
            
            cancelButton = new JButton("Cancel");
            cancelButton.addActionListener(this);
            cancelButton.setMinimumSize(MipavUtil.defaultButtonSize);
            cancelButton.setPreferredSize(MipavUtil.defaultButtonSize);
            cancelButton.setFont(serif12B);
            
            panel.add(OKButton);
            panel.add(cancelButton);
            return panel;
        }

        public void actionPerformed(ActionEvent e) {
            passedListeners = true;
            if(e.getSource().equals(OKButton)) {
                for(int i=0; i<listenerList.size(); i++) {
                    if(passedListeners) {
                        listenerList.get(i).actionPerformed(e);
                    } else {
                        exit = ExitStatus.OK_FAIL;
                        return;
                    }
                }
                if(passedListeners) {
                    exit = ExitStatus.OK_SUCCESS;
                    parent.actionPerformed(e);
                } else {    
                    exit = ExitStatus.OK_FAIL;
                    return;
                }
            } else if(e.getSource().equals(cancelButton)) {
                exit = ExitStatus.CANCEL;
                parent.dispose();
            }
        }
        
    }

    /**
     * Return meta-information about this discoverable action for categorization and labeling purposes.
     * 
     * @return Metadata for this action.
     */
    public ActionMetadata getActionMetadata() {
        return new MipavActionMetadata() {
            public String getCategory() {
                return new String("Algorithms.MRI");
            }

            public String getDescription() {
                return new String("Creates suscptibility weighted image.");
            }

            public String getDescriptionLong() {
                return new String("Models magnetic susceptibility of anatomy in MRI.");
            }

            public String getShortLabel() {
                return new String("SWI");
            }

            public String getLabel() {
                return new String("SWI");
            }

            public String getName() {
                return new String("SWI");
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
            table.put(new ParameterInt("roFilterSize", roFilterSize));
            table.put(new ParameterInt("peFilterSize", peFilterSize));
            table.put(new ParameterInt("multFactor", multFactor));
            table.put(new ParameterBoolean("showInterImages", showInterImages));
            table.put(new ParameterDouble("maskThreshold", maskThreshold));
            
            magImage = scriptParameters.retrieveImage("magnitudeImage");
            phaseImage = scriptParameters.retrieveImage("phaseImage");
            
            table.put(new ParameterBoolean(AlgorithmParameters.DO_OUTPUT_NEW_IMAGE, true));
            
            table.put(new ParameterExternalImage("magnitudeImage"));
            table.put(new ParameterExternalImage("phaseImage"));
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
            table.put(new ParameterImage("kImage"));
            table.put(new ParameterImage("iImage"));
            table.put(new ParameterImage("brainMask"));
            table.put(new ParameterImage("kCenterImage"));
            table.put(new ParameterImage("iFinal"));
            table.put(new ParameterImage("phaseMask"));
            table.put(new ParameterImage("iCenter"));
            table.put(new ParameterImage("magEnhancedFINAL"));
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
    public String getOutputImageName(String imageParamName) {
        return "magEnhanced";
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
