package mtry;
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
import gov.nih.mipav.model.file.FileDicomKey;
import gov.nih.mipav.model.file.FileInfoBRUKER;
import gov.nih.mipav.model.file.FileInfoDicom;


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
public class PlugInDialogMTry extends JDialogScriptableBase implements AlgorithmInterface, ActionDiscovery {
    
    
    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /**declare UID */

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Result image. */
    private ModelImage resultImage = null;

    /** This source image is typically set by the constructor */
    private ModelImage image; // 
    
    /** This is your algorithm */
    private PlugInAlgorithmMTry mTryAlgo = null;

    private String[] titles;

    private JComboBox[] imageCombo;
    
    private GuiBuilder guiBuilder;

    private JTextField[] inversionTimeField;

    private JTextField t1MinField;

    private JTextField t1MaxField;

    private JTextField precisionField;

    private ModelImage minImage;

    private ModelImage medImage;

    private ModelImage maxImage;

    private double t1Min;

    private double t1Max;

    private double precision;

    private double invTimeMin;

    private double invTimeMed;

    private double invTimeMax;
    
    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor used for instantiation during script execution (required for dynamic loading).
     */
    public PlugInDialogMTry() { }

    /**
     * Creates new dialog for kidney segmentation from an abdominal cavity image using a plugin.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public PlugInDialogMTry(Frame theParentFrame, ModelImage im) {
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
       if (algorithm instanceof PlugInAlgorithmMTry) {
            Preferences.debug("Elapsed: " + algorithm.getElapsedTime());
            
            if (algorithm.isCompleted() == true) {

                // The algorithm has completed and produced a new image to be displayed.
                resultImage = mTryAlgo.getDestImage();
                ViewJFrameImage resultFrame = new ViewJFrameImage(resultImage);
                resultFrame.setVisible(true);
                
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
            
            resultImage = new ModelImage(ModelImage.FLOAT, new int[]{64,64,44}, "T1Map");
            
            mTryAlgo = new PlugInAlgorithmMTry(resultImage, minImage, medImage, maxImage, 
                                                    t1Min, t1Max, precision, invTimeMin, invTimeMed, invTimeMax);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed or failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            mTryAlgo.addListener(this);
            createProgressBar("Reconstructing image", " ...", mTryAlgo);

            setVisible(false); // Hide dialog

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still
                // have user interface work fast.
                if (mTryAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                mTryAlgo.run();
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
        invTimeMin = scriptParameters.getParams().getDouble("invTimeMin");
        invTimeMed = scriptParameters.getParams().getDouble("invTimeMed");
        invTimeMax = scriptParameters.getParams().getDouble("invTimeMax");
        precision = scriptParameters.getParams().getDouble("precision");
        t1Min = scriptParameters.getParams().getDouble("t1Min");
        t1Max = scriptParameters.getParams().getDouble("t1Max");

        minImage = scriptParameters.retrieveImage("minImageInvTime"+Double.valueOf(invTimeMin).intValue());
        medImage = scriptParameters.retrieveImage("medImageInvTime"+Double.valueOf(invTimeMed).intValue());
        maxImage = scriptParameters.retrieveImage("maxImageInvTime"+Double.valueOf(invTimeMax).intValue());
    } //end setGUIFromParams()

    /**
     * Used in turning your plugin into a script
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeImage(minImage, "minImageInvTime"+Double.valueOf(invTimeMin).intValue());
        scriptParameters.storeImage(medImage, "medImageInvTime"+Double.valueOf(invTimeMed).intValue());
        scriptParameters.storeImage(maxImage, "maxImageInvTime"+Double.valueOf(invTimeMax).intValue());
        
        scriptParameters.getParams().put(ParameterFactory.newParameter("invTimeMin", invTimeMin));
        scriptParameters.getParams().put(ParameterFactory.newParameter("invTimeMed", invTimeMed));
        scriptParameters.getParams().put(ParameterFactory.newParameter("invTimeMax", invTimeMax));
        scriptParameters.getParams().put(ParameterFactory.newParameter("precision", precision));
        scriptParameters.getParams().put(ParameterFactory.newParameter("t1Min", t1Min));
        scriptParameters.getParams().put(ParameterFactory.newParameter("t1Max", t1Max));
    } //end storeParamsFromGUI()
   
    private void init() {
        setForeground(Color.black);
        setTitle("MDEFT Plugin");
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
        while(imageStr.hasMoreElements()) {
            titles[index] = imageStr.nextElement();
            index++;
        }
        
        imageCombo = new JComboBox[3];
        inversionTimeField = new JTextField[3];
        int numDefault = 0;
        for (int i=0; i<3; i++) {
            if(titles.length >= 3) {
                numDefault = i;
            }
            imageCombo[i] = guiBuilder.buildComboBox("MDEFT Scan #"+(i+1), titles, numDefault);
            imageCombo[i].addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent e) {
                    for(int i=0; i<imageCombo.length; i++) {
                        if(e.getSource().equals(imageCombo[i])) {
                            Double invTime = getInvTime(imageCombo[i].getSelectedItem().toString());
                            if(invTime != null) {
                                inversionTimeField[i].setText(invTime.toString());
                            }
                        }
                    }
                }
                
            });
            imagePanel.add(imageCombo[i].getParent(), gbc);
            gbc.gridy++;
            
            Double invTime = getInvTime(titles[numDefault]);
            if(invTime == null) {
                invTime = Double.valueOf(0.0);
            }

            inversionTimeField[i] = guiBuilder.buildDecimalField("Inversion time", invTime.doubleValue());
            imagePanel.add(inversionTimeField[i].getParent(), gbc);
            gbc.gridy++;
        }
        
        JPanel paramPanel = new JPanel(new GridBagLayout());
        paramPanel.setBorder(MipavUtil.buildTitledBorder("T1 Parameters"));
        gbc = new GridBagConstraints();
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.insets = new Insets(3, 3, 3, 3);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 0;
        gbc.gridy = 0;
        
        t1MinField = guiBuilder.buildDecimalField("Minimum T1", 500);
        t1MaxField = guiBuilder.buildDecimalField("Maximum T1", 2500);
        precisionField = guiBuilder.buildDecimalField("Precision", 0.5);
        
        paramPanel.add(t1MinField.getParent(), gbc);
        gbc.gridy++;
        paramPanel.add(t1MaxField.getParent(), gbc);
        gbc.gridy++;
        paramPanel.add(precisionField.getParent(), gbc);
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

    private Double getInvTime(String string) {
        Double invTime = null;
        
        if(ViewUserInterface.getReference().getRegisteredImageByName(string).getFileInfo()[0] instanceof FileInfoDicom) {
            FileInfoDicom f = (FileInfoDicom) ViewUserInterface.getReference().getRegisteredImageByName(string).getFileInfo()[0];
            Object obj = f.getTagTable().getValue(new FileDicomKey("0018,0082"));
            if(obj != null) {
                try {
                    invTime = Double.valueOf(obj.toString());
                } catch(NumberFormatException e) {
                    
                }
            }
        } else if(ViewUserInterface.getReference().getRegisteredImageByName(string).getFileInfo()[0] instanceof FileInfoBRUKER) {
            FileInfoBRUKER f = (FileInfoBRUKER) ViewUserInterface.getReference().getRegisteredImageByName(string).getFileInfo()[0];
            invTime = Double.valueOf(f.getInversionTime());
        }
        return invTime;
    }

    /**
     * This method initializes internal dialog variables based on inputs to the GUI.
     * 
     * @return
     */
	private boolean setVariables() {
	   
	    titles = new String[imageCombo.length];
	    for(int i=0; i<titles.length; i++) {
	        titles[i] = imageCombo[i].getSelectedItem().toString();
	    }
	    
	    double invTime1, invTime2, invTime3;
	    
	    try {
	        
	    t1Min = Double.valueOf(t1MinField.getText()).doubleValue();
	    t1Max = Double.valueOf(t1MaxField.getText()).doubleValue();
	    precision = Double.valueOf(precisionField.getText()).doubleValue();
	    
	    invTime1 = Double.valueOf(inversionTimeField[0].getText()).doubleValue();
	    invTime2 = Double.valueOf(inversionTimeField[1].getText()).doubleValue();
	    invTime3 = Double.valueOf(inversionTimeField[2].getText()).doubleValue();
	    
	    } catch(NumberFormatException nfe) {
	        MipavUtil.displayError("All values must be numerical.");
	        return false;
	    }
	    
	    double minTime = Math.min(invTime1, invTime2);
	    minTime = Math.min(minTime, invTime3);
	    
	    double maxTime = Math.max(invTime1, invTime2);
        maxTime = Math.max(maxTime, invTime3);
	    
        boolean i1Placed = false, i2Placed = false, i3Placed = false;
        
        if(minTime == invTime1) {
            invTimeMin = invTime1;
            minImage = ViewUserInterface.getReference().getRegisteredImageByName(titles[0]);
            i1Placed = true;
        } else if(minTime == invTime2) {
            invTimeMin = invTime2;
            minImage = ViewUserInterface.getReference().getRegisteredImageByName(titles[1]);
            i2Placed = true;
        } else if(minTime == invTime3) {
            invTimeMin = invTime3;
            minImage = ViewUserInterface.getReference().getRegisteredImageByName(titles[2]);
            i3Placed = true;
        }
        
        if(maxTime == invTime1) {
            invTimeMax = invTime1;
            maxImage = ViewUserInterface.getReference().getRegisteredImageByName(titles[0]);
            i1Placed = true;
        } else if(maxTime == invTime2) {
            invTimeMax = invTime2;
            maxImage = ViewUserInterface.getReference().getRegisteredImageByName(titles[1]);
            i2Placed = true;
        } else if(maxTime == invTime3) {
            invTimeMax = invTime3;
            maxImage = ViewUserInterface.getReference().getRegisteredImageByName(titles[2]);
            i3Placed = true;
        }
        
        if(!i1Placed) {
            invTimeMed = invTime1;
            medImage = ViewUserInterface.getReference().getRegisteredImageByName(titles[0]);
            i1Placed = true;
        } else if(!i2Placed) {
            invTimeMed = invTime2;
            medImage = ViewUserInterface.getReference().getRegisteredImageByName(titles[1]);
            i2Placed = true;
        } else if(!i3Placed) {
            invTimeMed = invTime3;
            medImage = ViewUserInterface.getReference().getRegisteredImageByName(titles[2]);
            i3Placed = true;
        }
        
        if(!i1Placed || !i2Placed || !i3Placed) {
            MipavUtil.displayError("Sorting failed, please enter unique inversion times");
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
        
        @SuppressWarnings("unused")
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
                return new String("Reconstructs complex images into T1 map.");
            }

            public String getDescriptionLong() {
                return new String("Uses scanner data from three inversion times to model T1 map.");
            }

            public String getShortLabel() {
                return new String("MTry");
            }

            public String getLabel() {
                return new String("MTry");
            }

            public String getName() {
                return new String("MTry");
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
            
            table.put(new ParameterDouble("invTimeMin", invTimeMin));
            table.put(new ParameterDouble("invTimeMed", invTimeMed));
            table.put(new ParameterDouble("invTimeMax", invTimeMax));
            table.put(new ParameterDouble("precision", precision));
            table.put(new ParameterDouble("t1Min", t1Min));
            table.put(new ParameterDouble("t1Max", t1Max));
            
            if(scriptParameters != null) {
                minImage = scriptParameters.retrieveImage("minImage");
                medImage = scriptParameters.retrieveImage("medImage");
                maxImage = scriptParameters.retrieveImage("maxImage");
            }
            
            table.put(new ParameterBoolean(AlgorithmParameters.DO_OUTPUT_NEW_IMAGE, true));
            
            table.put(new ParameterExternalImage("minImage"));
            table.put(new ParameterExternalImage("medImage"));
            table.put(new ParameterExternalImage("maxImage"));
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
            table.put(new ParameterImage("MTryImage"));
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
        return "MTryImage";
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
