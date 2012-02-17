package gov.nih.mipav.view;



import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.dialogs.*;

import java.awt.*;
import java.awt.event.*;

import java.io.*;

import javax.swing.*;


/**
 * This class produces a frame where the user can specify a specific LUT, the number of colors of the LUT or dynamically
 * edit the LUT. The frame with histogram of the image data is displayed using the color map. All frames using the color
 * map are dynamically updated with the new color map.
 *
 * @version  1.0
 * @author   Matthew J. McAuliffe, Ph.D.
 * @author   Harman Singh
 * @author   Lynne M. Pusanik
 * @see      ViewJFrameBase
 * @see      ViewJComponentHistoLUT
 */
public class ViewJFrameHistoLUT extends ViewJFrameBase implements WindowListener, ViewImageUpdateInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -5323312688163424096L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private boolean calcThresholdVolume = true;

    /** DOCUMENT ME! */
    private ViewJFrameColocalizationEM colEMFrame = null;

    /** DOCUMENT ME! */
    private ViewJFrameColocalizationRegression colRegFrame = null;

    /** Parent frame */
    private Frame parentFrame = null;
    
    /** DOCUMENT ME! */
    private boolean entireFlag = true;

    /** DOCUMENT ME! */
    private ViewJPanelLUT lutPanel;

    /** DOCUMENT ME! */
    private ViewMenuBuilder menuObj;

    /** DOCUMENT ME! */
    private ViewJComponentRegistration regComponent = null;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Makes a frame of the histogram and LUT.
     * 
     * @param  _imageA      Model of imageA
     * @param  _imageB      Model of imageB
     * @param  _LUTa        Model of LUT for image A
     * @param  _LUTb        Model of LUT for image B
     * @param  _entireFlag  Flag indicating if histogram should be done on all of image.
     */
    public ViewJFrameHistoLUT(ModelImage _imageA, ModelImage _imageB, ModelLUT _LUTa, ModelLUT _LUTb,
                              boolean _entireFlag) {
        super(_imageA, _imageB);
        setLUTs(_LUTa, _LUTb);

        buildMenu();

        entireFlag = _entireFlag;
        setTitle("Lookup Table: " + _imageA.getImageName());


        try {
            setIconImage(MipavUtil.getIconImage("histolut.gif"));
        } catch (FileNotFoundException error) {
            Preferences.debug("Exception ocurred while getting <" + error.getMessage() +
                              ">.  Check that this file is available.\n");
            System.err.println("Exception ocurred while getting <" + error.getMessage() +
                               ">.  Check that this file is available.\n");
        }

        setLocation(200, 200);
        setBackground(new Color(160, 160, 160));

        lutPanel = new ViewJPanelLUT(this);
        getContentPane().add(lutPanel, BorderLayout.NORTH);
        pack();
        addWindowListener(this);
        setResizable(false);
        setVisible(true);

    }
    
    /**
     * Makes a frame of the histogram and LUT.
     *
     * @param  _parentFrame  parent frame to hold the image. 
     * @param  _imageA      Model of imageA
     * @param  _imageB      Model of imageB
     * @param  _LUTa        Model of LUT for image A
     * @param  _LUTb        Model of LUT for image B
     * @param  _entireFlag  Flag indicating if histogram should be done on all of image.
     */
    public ViewJFrameHistoLUT(Frame _parentFrame, ModelImage _imageA, ModelImage _imageB, ModelLUT _LUTa, ModelLUT _LUTb,
                              boolean _entireFlag) {
        super(_imageA, _imageB);
        setLUTs(_LUTa, _LUTb);
        this.parentFrame = _parentFrame;
        buildMenu();

        entireFlag = _entireFlag;
        setTitle("Lookup Table: " + _imageA.getImageName());

        try {
            setIconImage(MipavUtil.getIconImage("histolut.gif"));
        } catch (FileNotFoundException error) {
            Preferences.debug("Exception ocurred while getting <" + error.getMessage() +
                              ">.  Check that this file is available.\n");
            System.err.println("Exception ocurred while getting <" + error.getMessage() +
                               ">.  Check that this file is available.\n");
        }

        setLocation(200, 200);
        setBackground(new Color(160, 160, 160));

        lutPanel = new ViewJPanelLUT(this);
        getContentPane().add(lutPanel, BorderLayout.NORTH);
        pack();
        addWindowListener(this);
        setResizable(false);
        setVisible(true);

    }

    /**
     * Makes a frame of the histogram and LUT.
     *
     * @param  _imageA      Model of imageA
     * @param  _imageB      Model of imageB
     * @param  _LUTa        Model of LUT for image A
     * @param  _LUTb        Model of LUT for image B
     * @param  _entireFlag  Flag indicating if histogram should be done on all of image.
     * @param  visible      Frame visible or not.
     */
    public ViewJFrameHistoLUT(ModelImage _imageA, ModelImage _imageB, ModelLUT _LUTa, ModelLUT _LUTb,
                              boolean _entireFlag, boolean visible) {
        super(_imageA, _imageB);
        // setLUTs( _LUTa, _LUTb );

        // buildMenu();

        entireFlag = _entireFlag;
        setTitle("Lookup Table: " + _imageA.getImageName());

        try {
            setIconImage(MipavUtil.getIconImage("histolut.gif"));
        } catch (FileNotFoundException error) {
            Preferences.debug("Exception ocurred while getting <" + error.getMessage() +
                              ">.  Check that this file is available.\n");
            System.err.println("Exception ocurred while getting <" + error.getMessage() +
                               ">.  Check that this file is available.\n");
        }

        addWindowListener(this);
        setResizable(false);
    }

    /**
     * Makes a frame of the histogram and LUT.
     *
     * @param  colRegFrame  DOCUMENT ME!
     * @param  _imageA      Model of imageA
     * @param  _imageB      Model of imageB
     * @param  _LUTa        Model of LUT for image A
     * @param  _LUTb        Model of LUT for image B
     * @param  _entireFlag  Flag indicating if histogram should be done on all of image.
     */
    public ViewJFrameHistoLUT(ViewJFrameColocalizationRegression colRegFrame, ModelImage _imageA, ModelImage _imageB,
                              ModelLUT _LUTa, ModelLUT _LUTb, boolean _entireFlag) {
        super(_imageA, _imageB);
        setLUTs(_LUTa, _LUTb);
        this.colRegFrame = colRegFrame;

        buildMenu();

        entireFlag = _entireFlag;
        setTitle("Lookup Table: " + _imageA.getImageName());

        try {
            setIconImage(MipavUtil.getIconImage("histolut.gif"));
        } catch (FileNotFoundException error) {
            Preferences.debug("Exception ocurred while getting <" + error.getMessage() +
                              ">.  Check that this file is available.\n");
            System.err.println("Exception ocurred while getting <" + error.getMessage() +
                               ">.  Check that this file is available.\n");
        }

        setLocation(200, 200);
        setBackground(new Color(160, 160, 160));

        lutPanel = new ViewJPanelLUT(this);
        getContentPane().add(lutPanel, BorderLayout.NORTH);

        pack();
        addWindowListener(this);
        setResizable(false);
        setVisible(true);
    }

    /**
     * Makes a frame of the histogram and LUT.
     *
     * @param  colEMFrame   DOCUMENT ME!
     * @param  _imageA      Model of imageA
     * @param  _imageB      Model of imageB
     * @param  _LUTa        Model of LUT for image A
     * @param  _LUTb        Model of LUT for image B
     * @param  _entireFlag  Flag indicating if histogram should be done on all of image.
     */
    public ViewJFrameHistoLUT(ViewJFrameColocalizationEM colEMFrame, ModelImage _imageA, ModelImage _imageB,
                              ModelLUT _LUTa, ModelLUT _LUTb, boolean _entireFlag) {
        super(_imageA, _imageB);
        setLUTs(_LUTa, _LUTb);
        this.colEMFrame = colEMFrame;

        buildMenu();

        entireFlag = _entireFlag;
        setTitle("Lookup Table: " + _imageA.getImageName());

        try {
            setIconImage(MipavUtil.getIconImage("histolut.gif"));
        } catch (FileNotFoundException error) {
            Preferences.debug("Exception ocurred while getting <" + error.getMessage() +
                              ">.  Check that this file is available.\n");
            System.err.println("Exception ocurred while getting <" + error.getMessage() +
                               ">.  Check that this file is available.\n");
        }

        setLocation(200, 200);
        setBackground(new Color(160, 160, 160));

        lutPanel = new ViewJPanelLUT(this);
        getContentPane().add(lutPanel, BorderLayout.NORTH);

        pack();
        addWindowListener(this);
        setResizable(false);
        setVisible(true);
    }

    /**
     * Makes a frame of the histogram and LUT.
     *
     * @param  _regComponent  component to which information is passed in manual registration
     * @param  _imageA        Model of imageA
     * @param  _imageB        Model of imageB
     * @param  _LUTa          Model of LUT for image A
     * @param  _LUTb          Model of LUT for image B
     * @param  _entireFlag    DOCUMENT ME!
     */
    public ViewJFrameHistoLUT(ViewJComponentRegistration _regComponent, ModelImage _imageA, ModelImage _imageB,
                              ModelLUT _LUTa, ModelLUT _LUTb, boolean _entireFlag) {

        this(_imageA, _imageB, _LUTa, _LUTb, _entireFlag);
        regComponent = _regComponent;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Creates a label in the proper font and color.
     *
     * @param   title  The title of the label.
     *
     * @return  The new label.
     */
    public static final JLabel createSliderLabel(String title) {
        JLabel label = new JLabel(title);
        label.setFont(MipavUtil.font12);
        label.setForeground(Color.black);

        return label;
    }

    /**
     * ActionListener.
     *
     * @param  event  DOCUMENT ME!
     */

    /**
     * Calls various methods depending on the action.
     *
     * @param  event  event that triggered function
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();

        if (command.equals("invertLUT")) {
            lutPanel.actionPerformed(event);
        } else if (command.equals("SaveLUT")) {

            // save both the LUT and the transfer functions
            saveLUTAs(true, null, null);

            if (getDisplayMode() == IMAGE_A) {
                setLUTA(getLUTa());
            } else {
                setLUTB(getLUTb());
            }
        } else if (command.equals("SaveUDLUT")) {
            // save both the LUT and the transfer functions
            String fName = "userdefine.lut";
            String dName = Preferences.getPreferencesDir();
            saveLUTandTransferFunction(fName, dName);

            if (getDisplayMode() == IMAGE_A) {
                setLUTA(getLUTa());
            } else {
                setLUTB(getLUTb());
            }
        } else if (command.equals("OpenLUT")) {
        	loadLUTandTransferFunctionFrom(true, null, null, false);

            if (getDisplayMode() == IMAGE_A) {
                setLUTA(getLUTa());
            } else {
                setLUTB(getLUTb());
            }
        } else if (command.equals("OpenUDLUT")) {
        	if ( ViewUserInterface.getReference().getActiveImageFrame().getActiveImage() != imageA ) {
        		ViewUserInterface.getReference().setActiveFrame(this.parentFrame);
        	}
           
        	String fName = "userdefine.lut";
            String dName = Preferences.getPreferencesDir();
            loadLUTandTransferFunctionFrom( true, fName, dName, false );
            
            if (getDisplayMode() == IMAGE_A) {
                setLUTA(getLUTa());
            } else {
                setLUTB(getLUTb());
            }
            
        } else if (command.equals("CloseLUT") || command.equals("Close")) {
            setVisible(false);

            // imageA.unregisterHistoLUTFrame();
            getImageA().removeImageDisplayListener(this);

            if (getImageB() != null) {
                getImageB().removeImageDisplayListener(this);
            }

            dispose();
        } else if (command.equals("SaveFuncts")) {

            // save only the transfer functions
            saveLUTAs(false, null, null);

            if (getDisplayMode() == IMAGE_A) {
                setLUTA(getLUTa());
            } else {
                setLUTB(getLUTb());
            }
        } else if (command.equals("OpenFuncts")) {

            // open only the transfer functions
        	loadLUTandTransferFunctionFrom(false, null, null, false);

            if (getDisplayMode() == IMAGE_A) {
                setLUTA(getLUTa());
            } else {
                setLUTB(getLUTb());
            }
        } else if (command.equals("ChangeNColors")) {
            JDialogNColors dialog = new JDialogNColors(this);

            if (!dialog.isCancelled()) {

                if (lutPanel.isImageASelected()) {
                    lutPanel.setNColors(dialog.getNColors());
                    getLUTa().makeLUT(dialog.getNColors());
                    setLUTA(getLUTa());
                } else if (lutPanel.isImageBSelected()) {
                    lutPanel.setNColors(dialog.getNColors());
                    getLUTb().makeLUT(dialog.getNColors());
                    setLUTB(getLUTb());
                }

                lutPanel.updateFrames(false);
            }
        } else if (command.equals("UpdateA")) {
            lutPanel.updateHistoLUT(getImageA(), getLUTa(), null, null, false);
            menuObj.setMenuItemEnabled("Reset histogram & LUT A", false);
        } else if (command.equals("UpdateB")) {
            lutPanel.updateHistoLUT(null, null, getImageB(), getLUTb(), false);
            menuObj.setMenuItemEnabled("Reset histogram & LUT B", false);
        } else if (command.equals("ctPresetsLUT")) {
            lutPanel.actionPerformed(event);
        } else if (command.equals("calcThresholdVolume")) {
            calcThresholdVolume = menuObj.isMenuItemSelected("Calculate threshold volume");

            if (!calcThresholdVolume) {
                lutPanel.clearVoxelLabel();
            }

        } else {
            JComboBox cb = (JComboBox)event.getSource();
            String lutName = (String)cb.getSelectedItem();
            if (getDisplayMode() == IMAGE_A) {
                LUTa.makeCustomizedLUT(lutName);
                setLUTA(LUTa);
                updateFrames(false);
            } else {
          	  LUTa.makeCustomizedLUT(lutName); 
                setLUTB(LUTb);
                updateFrames(false);
            }
         }
    }

    /**
     * Disposes of components and frame.
     */
    public void dispose() {
        super.dispose();
    }

    /**
     * Whether or not to calculate the threshold volume.
     *
     * @return  boolean do calc?
     */
    public boolean doCalcThresholdVolume() {
        return this.calcThresholdVolume;
    }

    /**
     * Enables the thresholding menu item(s).
     *
     * @param  flag  whether to enable the items
     */
    public void enableThresholdingItems(boolean flag) {
        // menuObj.setMenuItemEnabled( "Threshold image", flag );
    }

    /**
     * DOCUMENT ME!
     *
     * @param   imageAorB  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public ViewJComponentHLUTBase getComponentHistoLUT(int imageAorB) {

        if (imageAorB == IMAGE_A) {
            return getHistoLUTComponentA();
        } else {
            return getHistoLUTComponentB();
        }
    }

    /**
     * Gets the control widgets for the frame.
     *
     * @return  DOCUMENT ME!
     */
    public ViewControlsImage getControls() {
        return null;
    }

    /**
     * Get the histogram component for imageA.
     *
     * @return  the imageA histogram component
     */
    public ViewJComponentHLUTBase getHistoLUTComponentA() {
        return lutPanel.getHistoLUTComponentA();
    }

    /**
     * Get the histogram component for imageB.
     *
     * @return  the imageB histogram component
     */
    public ViewJComponentHLUTBase getHistoLUTComponentB() {
        return lutPanel.getHistoLUTComponentB();
    }

    /**
     * Accessor that returns the imageA.
     *
     * @return  image
     */
    public ModelImage getImageA() {
        return imageA;
    }

    /**
     * Accessor that returns the imageB.
     *
     * @return  imageB
     */
    public ModelImage getImageB() {
        return imageB;
    }

    /**
     * Returns the lower threshold value.
     *
     * @return  float lower thresh
     */
    public float getLowerThreshold() {
        return lutPanel.getLowerThreshold();
    }

    /**
     * This method returns the current LUT.
     *
     * @return  LUT for Image A if selected, otherwise for Image B
     */
    public ModelLUT getLUT() {

        if (lutPanel.isImageASelected()) {
            return LUTa;
        } else {
            return LUTb;
        }

    }

    /**
     * Get the imageA LUT.
     *
     * @return  the imageA LUT component
     */
    public final ViewJComponentLUT getLUTComponentA() {
        return lutPanel.getLUTComponentA();
    }

    /**
     * Get the imageB LUT.
     *
     * @return  the imageB LUT component
     */
    public final ViewJComponentLUT getLUTComponentB() {
        return lutPanel.getLUTComponentB();
    }

    /**
     * Get the JDialogRecordLUT reference from ViewJPanelLUT.
     *
     * @return  JDialogRecordLUT
     */
    public JDialogRecordLUT getLUTRecorder() {
        return lutPanel.getLUTRecorder();
    }

    /**
     * This method is used to obtain the VOIBase Single Transfer Line from ViewJFrameComponentHistoLut.
     *
     * @return  LUTb.getTransferFunction() - VOIBase of Single Transfer Line for Image B
     */
    public TransferFunction getTransferLine() {

        if (lutPanel.isImageASelected()) {
            return LUTa.getTransferFunction();
        } else {
            return LUTb.getTransferFunction();
        }
    }

    /**
     * Returns the upper threshold value.
     *
     * @return  float upper thresh
     */
    public float getUpperThreshold() {
        return lutPanel.getUpperThreshold();
    }

    /**
     * Returns whether the whole image is being examined or just the VOI region.
     *
     * @return  whether the whole image is being examined or just the VOI region
     */
    public boolean getWholeImageFlag() {
        return entireFlag;
    }

    /**
     * Returns true if comphist A or B is in dual threshold inverse mode (for JDialogConvertType input ranges).
     *
     * @return  boolean is dual threshold inversing
     */
    public boolean isThresholding() {

        if (lutPanel.isImageASelected() && (getHistoLUTComponentA() != null)) {

            getHistoLUTComponentA();
			getHistoLUTComponentA();
			if ((getHistoLUTComponentA().getMode() == ViewJComponentHLUTBase.DUAL_THRESHOLD_INV) ||
                    (getHistoLUTComponentA().getMode() == ViewJComponentHLUTBase.DUAL_THRESHOLD)) {
                return true;
            }
        } else if (lutPanel.isImageBSelected() && (getHistoLUTComponentB() != null)) {

            getHistoLUTComponentB();
			getHistoLUTComponentB();
			if ((getHistoLUTComponentB().getMode() == ViewJComponentHLUTBase.DUAL_THRESHOLD_INV) ||
                    (getHistoLUTComponentB().getMode() == ViewJComponentHLUTBase.DUAL_THRESHOLD)) {
                return true;
            }
        }

        return false;
    }

    /**
     * Enable button to indicate image has changed and the histogram should be recalculated.
     *
     * @param  LUT        new Lookup table
     * @param  imageAorB  indicates which histogram needs to be recalculated
     */
    public void notifyOfUpdate(ModelLUT LUT, int imageAorB) {

        if (imageAorB == ModelImage.IMAGE_A) {
            menuObj.setMenuItemEnabled("Reset histogram & LUT A", true);
        } else {
            menuObj.setMenuItemEnabled("Reset histogram & LUT B", true);
        }
    }

    /**
     * Removes the menu and controls of the main frame so that a new frame can load the main frame with the proper
     * controls.Abstract and must be extended.
     */
    public void removeControls() { }

    /**
     * Removes the tabbed pane for the histogram of image B.
     */
    public void removeHistoLUTb() {
        lutPanel.removeHistoLUTb();
    }

    /**
     * Sets the active image for drawing VOIs. VOIs are only drawn in the active image. In addition, algorithms are
     * executed on the active window.
     *
     * @param  active  IMAGE_A or IMAGE_B
     */
    public void setActiveImage(int active) {

        if (active == IMAGE_A) {
            displayMode = IMAGE_A;
            setTitle();
        } else {
            displayMode = IMAGE_B;
            setTitle();
        }

        updateImages(false);
    }

    /**
     * Sets the alpha blending of parameter for two image displaying.
     *
     * @param  value  amount [0,100] that is the percentage of Image A to be displayed and (1-percentage) of Image B to
     *                be displayed
     */
    public void setAlphaBlend(int value) { }

    /**
     * Sets the menu and controls (i.e. toolbars) of the main frame! This puts only the menus and controls needed to
     * controls the operations of this frame. Different image frames have different menu and controls.
     */
    public void setControls() { }

    /**
     * This method is used to set Computed Tomography (CT) presets for CT images.
     *
     * @param  st   starting preset of the window
     * @param  end  ending preset of the window
     *
     * @see    JDialogCT
     */
    public void setCTMode(int st, int end) {

        if (lutPanel.isImageASelected()) {
            ((ViewJComponentHistoLUT) getHistoLUTComponentA()).ctMode(st, end);
        } else {
            ((ViewJComponentHistoLUT) getHistoLUTComponentB()).ctMode(st, end);
        }
    }

    /**
     * Change the image display mode.
     *
     * @param  mode  IMAGE_A or IMAGE_B or IMAGE_A_B
     */
    public void setDisplayMode(int mode) {
        displayMode = mode;
    }

    /**
     * Controls whether or not the images/VOIs of the frame can be modified.
     *
     * @param  flag  if true the image/VOIs can be modified; if false image/VOIs can NOT be modified
     */
    public void setEnabled(boolean flag) { }

    /**
     * Accessor that sets the imageB.
     *
     * @param  image  image to set frame to
     */
    public void setImageB(ModelImage image) {
        imageB = image;
        menuObj.setMenuItemEnabled("Reset histogram & LUT B", imageB != null);
    }

    /**
     * This method sets the histogram to Linear Transfer Function Mode.
     */
    public void setLinearLUT() {

        if (lutPanel.isImageASelected()) {

            if (getHistoLUTComponentA() != null) {
                getHistoLUTComponentA();
				getHistoLUTComponentA().setMode(ViewJComponentHLUTBase.LINEAR);
            }
        } else {

            if (getHistoLUTComponentB() != null) {
                getHistoLUTComponentB();
				getHistoLUTComponentB().setMode(ViewJComponentHLUTBase.LINEAR);
            }
        }
    }

    /**
     * Change the panel's LUT.
     *
     * @param  newLUT  the new LUT
     */
    public void setLUT(ModelLUT newLUT) {

        if (lutPanel.isImageASelected()) {
            setLUTA(newLUT);
        } else {
            setLUTB(newLUT);
        }
    }

    /**
     * Replaces the LUT A component.
     *
     * @param  LUT  new LUT
     */
    public void setLUTA(ModelLUT LUT) {
        setLUTa(LUT);

        if (lutPanel.isImageASelected()) {
            getLUTComponentA().show(LUT);
            getHistoLUTComponentA().showHistogram(LUT);
            getLUTRecorder().updateLUT(LUT);
        }
        
        if (regComponent != null) {
            regComponent.setLUTa(LUT);
        }

        if (colRegFrame != null) {
            colRegFrame.setLUTdest(LUT);
        }

        if (colEMFrame != null) {
            colEMFrame.setLUTdest(LUT);
        }
        
        updateFrames(false);
    }

    /**
     * Replaces the LUT B component.
     *
     * @param  LUT  new LUT
     */
    public void setLUTB(ModelLUT LUT) {

        setLUTb(LUT);

        if (lutPanel.isImageBSelected()) {
            getLUTComponentB().show(LUT);
            getHistoLUTComponentB().showHistogram(LUT);
        }

        if (regComponent != null) {
            regComponent.setLUTb(LUT);
        }

        updateFrames(false);
    }

    /**
     * DOCUMENT ME!
     *
     * @param  flag  DOCUMENT ME!
     */
    public void setPaintBitmapSwitch(boolean flag) { }

    /**
     * The following 2 functions set the RGB tables for images A and B.
     *
     * @param  RGBT  DOCUMENT ME!
     */
    public void setRGBTA(ModelRGB RGBT) { }

    /**
     * DOCUMENT ME!
     *
     * @param  RGBT  DOCUMENT ME!
     */
    public void setRGBTB(ModelRGB RGBT) { }

    /**
     * Does nothing in this class.
     *
     * @param  slice  DOCUMENT ME!
     */
    public void setSlice(int slice) { }

    // ===============Abstract Methods from ViewJFrameBase===========================

    /**
     * Does nothing in this class.
     *
     * @param  tSlice  DOCUMENT ME!
     */
    public void setTimeSlice(int tSlice) { }

    /**
     * setTitle.
     */
    public void setTitle() { }

    /**
     * Redisplays histoLUT.
     */
    public void update() {

        if (lutPanel.isImageASelected()) {
            getHistoLUTComponentA().showHistogram();
            getLUTComponentA().show(null);
        } else if (lutPanel.isImageBSelected()) {
            getHistoLUTComponentB().showHistogram();
            getLUTComponentB().show(null);
        }

    }

    /**
     * Update registration frames.
     *
     * @param  flag  whether to display a reloading of the image in the frames
     */
    public void updateFrames(boolean flag) {

        if (colRegFrame != null) {
            colRegFrame.setLUTdest(getLUTa());
        } else if (colEMFrame != null) {
            colEMFrame.setLUTdest(getLUTa());
        } else if (regComponent == null) {

            if (getImageA() != null) {
                getImageA().notifyImageDisplayListeners(getLUTa(), flag);
            }

            if (getImageB() != null) {
                getImageB().notifyImageDisplayListeners(getLUTb(), flag);
            }
        } // end of if (regComponent == null)
        else {

            if (getImageA() != null) {
                regComponent.setLUTa(getLUTa());

            }

            if (regComponent != null) {
                regComponent.setLUTb(getLUTb());
            }
        }
    }

    /**
     * end ActionListener.
     *
     * @param  _imageA       DOCUMENT ME!
     * @param  _LUTa         DOCUMENT ME!
     * @param  _imageB       DOCUMENT ME!
     * @param  _LUTb         DOCUMENT ME!
     * @param  progressFlag  DOCUMENT ME!
     */

    /**
     * This method is called to update the histogram(s) displayed in each tabbed pane of the frame.
     *
     * @param  _imageA       image A
     * @param  _LUTa         lookup table for image A
     * @param  _imageB       image B
     * @param  _LUTb         lookup table for image B
     * @param  progressFlag  passed to calculateHistogram algorithm. If false progress bar is not displayed
     */
    public void updateHistoLUT(ModelImage _imageA, ModelLUT _LUTa, ModelImage _imageB, ModelLUT _LUTb,
                               boolean progressFlag) {
        lutPanel.updateHistoLUT(_imageA, _LUTa, _imageB, _LUTb, progressFlag);
    }

    /**
     * This methods calls the componentImage's REPAINT method to redraw the screen. The extents on this image have
     * changed, so the extents need to be read in again and menus, panes and slide bars adjusted accordingly.
     *
     * @return  DOCUMENT ME!
     */
    public boolean updateImageExtents() {
        return false;
    }

    // **** Methods here only because ViewImageUpdateInterface is implemented. They do
    // nothing here.

    /**
     * This methods calls the componentImage's REPAINT method to redraw the screen. Without LUT changes or image changes
     *
     * @return  DOCUMENT ME!
     */
    public boolean updateImages() {
        return false;
    }

    /**
     * This methods calls the componentImage's update method to redraw the screen. Without LUT changes. Does nothing in
     * this class.
     *
     * @param   flag  forces show to re import image and calc. java image
     *
     * @return  boolean confirming successful update
     */
    public boolean updateImages(boolean flag) {
        return false;
    }

    /**
     * This methods calls the componentImage's update method to redraw the screen. Does nothing in this class.
     *
     * @param   LUTa        LUT used to update imageA
     * @param   LUTb        LUT used to update imageB
     * @param   flag        forces show to re import image and calc. java image
     * @param   interpMode  image interpolation method (Nearest or Smooth)
     *
     * @return  boolean confirming a successful update
     */
    public boolean updateImages(ModelLUT LUTa, ModelLUT LUTb, boolean flag, int interpMode) {
        return false;
    }

    /**
     * {@inheritDoc}
     */
    public void updateLUTPositionString(String str) {
        lutPanel.updateLUTPositionString(str);
    }

    /**
     * Unchanged.
     *
     * @param  event  DOCUMENT ME!
     */
    public void windowActivated(WindowEvent event) { }

    /**
     * Unchanged.
     *
     * @param  event  DOCUMENT ME!
     */
    public void windowClosed(WindowEvent event) { }

    /**
     * Calls close.
     *
     * @param  event  event that triggered function
     */
    public void windowClosing(WindowEvent event) {
        setVisible(false);
        imageA.removeImageDisplayListener(this);

        if (imageB != null) {
            imageB.removeImageDisplayListener(this);
        }

        dispose();
    }

    /**
     * Unchanged.
     *
     * @param  event  DOCUMENT ME!
     */
    public void windowDeactivated(WindowEvent event) { }

    /**
     * Unchanged.
     *
     * @param  event  DOCUMENT ME!
     */
    public void windowDeiconified(WindowEvent event) { }

    /**
     * Unchanged.
     *
     * @param  event  DOCUMENT ME!
     */
    public void windowIconified(WindowEvent event) { }

    // ************************************************************************
    // **************************** Window Events *****************************
    // ************************************************************************

    /**
     * Unchanged.
     *
     * @param  event  DOCUMENT ME!
     */
    public void windowOpened(WindowEvent event) { }

    /**
     * This method builds a menu which contains the options for opening/saving a LUT or set of transfer functions,
     * closing the LUT, and utilities such as CT presets.
     */
    private void buildMenu() {
        JMenuBar menuBar;

        try {
            menuObj = new ViewMenuBuilder(this);
            menuBar = new JMenuBar();

            JSeparator separator = new JSeparator();

            menuBar.add(menuObj.makeMenu("File", 'F', false,
                                         new JComponent[] {
                                             menuObj.buildMenuItem("Open LUT and transfer function", "OpenLUT", 0, "open.gif", true),
                                             menuObj.buildMenuItem("Save LUT and transfer function", "SaveLUT", 0, "save.gif", true),
                                             new JSeparator(),
                                             menuObj.buildMenuItem("Open default LUT and transfer function", "OpenUDLUT", 0,
                                                                   "defaultlutopen.gif", true),
                                             menuObj.buildMenuItem("Save default LUT and transfer function", "SaveUDLUT", 0,
                                                                   "defaultlutsave.gif", true), new JSeparator(),
                                             menuObj.buildMenuItem("Open transfer function", "OpenFuncts", 0,
                                                                   "open.gif", true),
                                             menuObj.buildMenuItem("Save transfer function", "SaveFuncts", 0,
                                                                   "save.gif", true), separator,
                                             menuObj.buildMenuItem("Close LUT", "CloseLUT", 0, null, true)
                                         }));
            menuBar.add(menuObj.makeMenu("Utilities", 'U', false,
                                         new JComponent[] {
                                             menuObj.buildMenuItem("Change number of colors", "ChangeNColors", 0, null,
                                                                   true),
                                             menuObj.buildMenuItem("CT function", "ctPresetsLUT", 0, "ctwindow.gif",
                                                                   true),
                                             menuObj.buildMenuItem("Invert LUT", "invertLUT", 0, null, true),
                                             menuObj.buildMenuItem("Reset histogram & LUT A", "UpdateA", 0, null,
                                                                   true),
                                             menuObj.buildMenuItem("Reset histogram & LUT B", "UpdateB", 0, null,
                                                                   true),
                                             menuObj.buildCheckBoxMenuItem("Calculate threshold volume",
                                                                           "calcThresholdVolume", true),
            // menuObj.buildMenuItem("Threshold Image", "Threshold", null, null)
                                         }));
            menuObj.setMenuItemEnabled("Reset histogram & LUT A", false);
            menuObj.setMenuItemEnabled("Reset histogram & LUT B", false);

        } catch (OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory: ViewJFrameHistoLUT.buildMenu");

            return;
        }

        setJMenuBar(menuBar);
    }

    
}
