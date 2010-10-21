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
import gov.nih.mipav.view.dialogs.JDialogScriptableBase;
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
public class PlugInDialogMTry extends JDialogScriptableBase implements AlgorithmInterface {
    
    
    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /**declare UID */

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Result image. */
    private ModelImage resultImage = null;

    /** This source image is typically set by the constructor */
    private ModelImage image; // 
    
    /** This is your algorithm */
    private PlugInAlgorithmMTry genericAlgo = null;

    /** The check box for whether a blur should be performed. */
	private JCheckBox check;

	/** The variable representing whether the blur should be performed. */
	private boolean doErnst;

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
       if (algorithm instanceof PlugInAlgorithmNewGeneric2) {
            Preferences.debug("Elapsed: " + algorithm.getElapsedTime());
            image.clearMask();
            
            if ((genericAlgo.isCompleted() == true) && (resultImage != null)) {

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
            
        
            
            genericAlgo = new PlugInAlgorithmMTry(resultImage, minImage, medImage, maxImage, 
                                                    t1Min, t1Max, precision, invTimeMin, invTimeMed, invTimeMax);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed or failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            genericAlgo.addListener(this);
            createProgressBar(image.getImageName(), " ...", genericAlgo);

            setVisible(false); // Hide dialog

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still
                // have user interface work fast.
                if (genericAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                genericAlgo.run();
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

    	doErnst = scriptParameters.getParams().getBoolean("do_gaussian");
    } //end setGUIFromParams()

    /**
     * Used in turning your plugin into a script
     */
    protected void storeParamsFromGUI() throws ParserException {
    	scriptParameters.storeInputImage(image);
   
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_gaussian", doErnst));
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
        mainPanel.setBorder(buildTitledBorder("Image Parameters"));
        
        titles = new String[ViewUserInterface.getReference().getRegisteredImagesNum()];
        Enumeration<String> imageStr = ViewUserInterface.getReference().getRegisteredImageNames();
        int index = 0;
        while(imageStr.hasMoreElements()) {
            titles[index] = imageStr.nextElement();
            index++;
        }
        
        imageCombo = new JComboBox[3];
        inversionTimeField = new JTextField[3];
        for (int i=0; i<3; i++) {
            imageCombo[i] = guiBuilder.buildComboBox("MDEFT Scan #"+(i+1), titles, 0);
            mainPanel.add(imageCombo[i].getParent(), gbc);
            gbc.gridy++;
            inversionTimeField[i] = guiBuilder.buildDecimalField("Inversion time", 0.0);
            mainPanel.add(inversionTimeField[i].getParent(), gbc);
            gbc.gridy++;
        }
        
        t1MinField = guiBuilder.buildDecimalField("Minimum T1", 500);
        t1MaxField = guiBuilder.buildDecimalField("Maximum T1", 2500);
        precisionField = guiBuilder.buildDecimalField("Precision", 0.5);
        
        mainPanel.add(t1MinField.getParent(), gbc);
        gbc.gridy++;
        mainPanel.add(t1MaxField.getParent(), gbc);
        gbc.gridy++;
        mainPanel.add(precisionField.getParent(), gbc);
        gbc.gridy++;

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
	   
	    
	    
	    
	    medImage = ViewUserInterface.getReference().getRegisteredImageByName(titles[1]);
	    maxImage = ViewUserInterface.getReference().getRegisteredImageByName(titles[2]);
	    
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
        
        public static final int GUI_BUILDER_OK_ID = ActionEvent.RESERVED_ID_MAX + 20;

        private ArrayList<ActionListener> listenerList;
        
        private boolean passedListeners;

        private ExitStatus exit;
        
        private JButton yes, no;
        
        private JDialog parent;
        
        public GuiBuilder(JDialog parent) {
            this.parent = parent;
            this.listenerList = new ArrayList<ActionListener>();
            this.exit = ExitStatus.INCOMPLETE;
        }
        
        public ExitStatus getExitStatus() {
            return exit;
        }
        
        public ActionListener[] getListenerList() {
            ActionListener[] list = new ActionListener[listenerList.size()];
            for(int i=0; i<listenerList.size(); i++) {
                list[i] = listenerList.get(i);
            }
            return list;
        }
        
        public JRadioButton buildRadioButton(String label, boolean selected) {
            FlowLayout f = new FlowLayout();
            f.setAlignment(FlowLayout.LEFT);
            JPanel radioPanel = new JPanel(f);
            JRadioButton radioButton = new JRadioButton(label);
            radioButton.setSelected(selected);
            radioPanel.add(radioButton);
            return radioButton;
        }
        
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
                            MipavUtil.displayInfo(labelText+" must be an integer.");
                            passedListeners = false;
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
            OKButton = buildOKButton();
            cancelButton = buildCancelButton();
            cancelButton.addActionListener(this);
            panel.add(OKButton);
            OKButton.addActionListener(this);
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
                    parent.dispose();
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
}
