package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.filters.*;
import gov.nih.mipav.model.algorithms.itk.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.components.*;

import java.awt.*;
import java.awt.event.*;

import java.util.*;

import javax.swing.*;


/**
 * Dialog to get user input, then call the algorithm. Algorithm is a
 * dynamically selected Itk Filter. The user has the option to generate a new
 * image or replace the source image. In addition the user can indicate if
 * he/she wishes to have the algorithm applied to whole image or to the VOI
 * regions. It should be noted that the algorithms are executed in their own
 * thread.
 *
 * @see  AlgorithmItkFiler
 */
public class JDialogItkFilter extends JDialogScriptableBase
        implements AlgorithmInterface, LegacyDialogDefaultsInterface {

    //~ Static fields/initializers --------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -4057161307906161677L;

    //~ Instance fields -------------------------------------------------------------------------

    /** filter algorithm */
    private AlgorithmItkFilter m_itkFilterAlgo = null;

    /** itk filter container, 2D */
    private PItkFilter m_itkFilter2D = null;
    /** itk filter container, 3D */
    private PItkFilter m_itkFilter3D = null;

    /** source image */
    private ModelImage m_srcImage = null;

    /** result image */
    private ModelImage m_resultImage = null;

    /** flag indicating if slices should be blurred independently */
    private boolean m_image25D = false;  

    /** gui for image25D */
    //private JCheckBox image25DCheckbox;

    /** widgets for all the filter's parameters. */
    private JPanelItkFilterParams m_filterParamPanel = null;
 
    /** Which color channels to filter */
    private JPanelColorChannels m_colorChannelPanel = null;

    /** new image or replace current, option to apply algorithm only to VOI regions. */
    private JPanelAlgorithmOutputOptions m_outputOptionsPanel = null;

    /** saved window titles */
    private String[] m_titles = null;

    /** UI to hang from */
    private ViewUserInterface m_userInterface = null;

    //~ Constructors -------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation.
     */
    public JDialogItkFilter() { }

    /**
     * Creates a new JDialogItkFilter object.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     * @param  fr              FilterRecordItk, info about the Itk filter, i/o types.
     */
    public JDialogItkFilter(Frame theParentFrame, ModelImage im, 
                            FilterRecordItk fr) {
        super(theParentFrame, false);
        m_srcImage = im;
        m_userInterface = ViewUserInterface.getReference();
        if (init(fr)) {
            setVisible(true);
        }
    }

    /** @return true if the constructor completed successfully. */
    public boolean isOK() { 
        return m_itkFilter2D != null; 
    }

    //~ Methods -------------------------------------------------------------------------------

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
            // TODO correct help item.
            //MipavUtil.showHelp("10009");
            MipavUtil.showWebHelp("Filters_(Spatial):_Gaussian_Blur#Applying_the_Gaussian_Blur_algorithm");
        } else {
            super.actionPerformed(event);
        }
    }

    // ************************************************************************
    // ************************** Algorithm Events ****************************
    // ************************************************************************

    /**
     * This method is required if the AlgorithmPerformed interface is
     * implemented. It is called by the algorithm when it has completed or
     * failed to to complete, so that the dialog can be display the result
     * image and/or clean up.
     *
     * @param  algorithm  Algorithm that caused the event.
     */
    public void algorithmPerformed(AlgorithmBase algorithm) {

        m_srcImage.clearMask();

        if ((m_itkFilterAlgo.isCompleted() == true) && (m_resultImage != null)) {

            // The algorithm has completed and produced a new image to be displayed.
            if (m_resultImage.isColorImage()) {
                updateFileInfo(m_srcImage, m_resultImage);
            }

            m_resultImage.clearMask();

            try {
                openNewFrame(m_resultImage);
            } catch (OutOfMemoryError error) {
                System.gc();
                MipavUtil.displayError("Out of memory: unable to open new frame");
            }
        } else if (m_resultImage == null) {

            // These next lines set the titles in all frames where the source image is displayed to
            // image name so as to indicate that the image is now unlocked!
            // The image frames are enabled and then registered to the userinterface.
            Vector<ViewImageUpdateInterface> imageFrames = m_srcImage.getImageFrameVector();

            for (int i = 0; i < imageFrames.size(); i++) {
                ((Frame) (imageFrames.elementAt(i))).setTitle(m_titles[i]);
                ((Frame) (imageFrames.elementAt(i))).setEnabled(true);

                if (((Frame) (imageFrames.elementAt(i))) != parentFrame) {
                    m_userInterface.registerFrame((Frame) (imageFrames.elementAt(i)));
                }
            }

            if (parentFrame != null) {
                m_userInterface.registerFrame(parentFrame);
            }

            m_srcImage.notifyImageDisplayListeners(null, true);
        } else if (m_resultImage != null) {

            // algorithm failed but result image still has garbage
            m_resultImage.disposeLocal(); // clean up memory
            m_resultImage = null;
            System.gc();

        }

        if (algorithm.isCompleted()) {
            insertScriptLine();
        }

        if (m_itkFilterAlgo != null) {
            m_itkFilterAlgo.finalize();
            m_itkFilterAlgo = null;
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
        str += m_outputOptionsPanel.isProcessWholeImageSet() + delim;
        str += m_image25D + delim;
        str += m_colorChannelPanel.isRedProcessingRequested() + delim;
        str += m_colorChannelPanel.isGreenProcessingRequested() + delim;
        str += m_colorChannelPanel.isBlueProcessingRequested();

        return str;
    }

    /**
     * Accessor that returns the image.
     *
     * @return  The result image.
     */
    public ModelImage getResultImage() {
        return m_resultImage;
    }

    // *******************************************************************
    // ************************* Item Events ****************************
    // *******************************************************************

    /**
     * Loads the default settings from Preferences to set up the dialog.
     */
    public void legacyLoadDefaults() {
        String defaultsString = Preferences.getDialogDefaults(getDialogName());

        if ((defaultsString != null) && (m_outputOptionsPanel != null)) {

            try {
                // Preferences.debug(defaultsString);

                StringTokenizer st = new StringTokenizer(defaultsString, ",");

                m_outputOptionsPanel.setProcessWholeImage(MipavUtil.getBoolean(st));

                m_colorChannelPanel.setRedProcessingRequested(MipavUtil.getBoolean(st));
                m_colorChannelPanel.setGreenProcessingRequested(MipavUtil.getBoolean(st));
                m_colorChannelPanel.setBlueProcessingRequested(MipavUtil.getBoolean(st));

                m_outputOptionsPanel.setOutputNewImage(MipavUtil.getBoolean(st));
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
    public void legacySaveDefaults() {
        String defaultsString = new String(getParameterString(",") + "," 
                                           + m_outputOptionsPanel.isOutputNewImageSet());

        Preferences.saveDialogDefaults(getDialogName(), defaultsString);
    }

    /**
     * Accessor that sets the slicing flag.
     *
     * @param  flag  <code>true</code> indicates slices should be blurred independently.
     */
    public void setImage25D(boolean flag) {
        m_image25D = flag;
    }

    /**
     * Once all the necessary variables are set, call the algorithm based on
     * what type of image this is and whether or not there is a separate
     * destination image.
     */
    protected void callAlgorithm() {
        String result_name = makeImageName(m_srcImage.getImageName(), "_itk_filter");

        if (m_srcImage.getNDims() == 2) { // source image is 2D and kernel not separable

            if (m_outputOptionsPanel.isOutputNewImageSet()) {

                try {

                    // Make result image
                    if (m_srcImage.getType() == ModelImage.ARGB) {
                        m_resultImage = new ModelImage(ModelImage.ARGB, m_srcImage.getExtents(), result_name);
                    } else if (m_srcImage.getType() == ModelImage.ARGB_USHORT) {
                        m_resultImage = new ModelImage(ModelImage.ARGB_USHORT, m_srcImage.getExtents(), result_name);
                    } else if (m_srcImage.getType() == ModelImage.ARGB_FLOAT) {
                        m_resultImage = new ModelImage(ModelImage.ARGB_FLOAT, m_srcImage.getExtents(), result_name);
                    } else {

                        // m_resultImage     = new ModelImage(ModelImage.FLOAT, destExtents, name, m_userInterface);
                        m_resultImage = (ModelImage) m_srcImage.clone();
                        m_resultImage.setImageName(result_name);

                        if ((m_resultImage.getFileInfo()[0]).getFileFormat() == FileUtility.DICOM) {
                            ((FileInfoDicom) (m_resultImage.getFileInfo(0))).setSecondaryCaptureTags();
                        }
                    }

                    // Make algorithm
                    m_itkFilterAlgo = new AlgorithmItkFilter(m_resultImage, m_srcImage, m_itkFilter2D);

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed of failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    m_itkFilterAlgo.addListener(this);

                    createProgressBar(m_srcImage.getImageName(), m_itkFilterAlgo);

                    m_itkFilterAlgo.setRed(m_colorChannelPanel.isRedProcessingRequested());
                    m_itkFilterAlgo.setGreen(m_colorChannelPanel.isGreenProcessingRequested());
                    m_itkFilterAlgo.setBlue(m_colorChannelPanel.isBlueProcessingRequested());

                    if (!m_outputOptionsPanel.isProcessWholeImageSet()) {
                        m_itkFilterAlgo.setMask(m_srcImage.generateVOIMask());
                    }

                    // Hide dialog
                    setVisible(false);

                    if (isRunInSeparateThread()) {

                        // Start the thread as a low priority because we wish to still have user interface work fast.
                        if (m_itkFilterAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {
                        m_itkFilterAlgo.run();
                    }
                } catch (OutOfMemoryError x) {

                    if (m_resultImage != null) {
                        m_resultImage.disposeLocal(); // Clean up memory of result image
                        m_resultImage = null;
                    }

                    System.gc();
                    MipavUtil.displayError("Dialog Itk Filter: unable to allocate enough memory");

                    return;
                }
            } else {

                try {

                    // No need to make new image space because the user has choosen to replace the source image
                    // Make the algorithm class
                    m_itkFilterAlgo = new AlgorithmItkFilter(null, m_srcImage, m_itkFilter2D);

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed of failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    m_itkFilterAlgo.addListener(this);
                    createProgressBar(m_srcImage.getImageName(), m_itkFilterAlgo);

                    m_itkFilterAlgo.setRed(m_colorChannelPanel.isRedProcessingRequested());
                    m_itkFilterAlgo.setGreen(m_colorChannelPanel.isGreenProcessingRequested());
                    m_itkFilterAlgo.setBlue(m_colorChannelPanel.isBlueProcessingRequested());

                    if (!m_outputOptionsPanel.isProcessWholeImageSet()) {
                        m_itkFilterAlgo.setMask(m_srcImage.generateVOIMask());
                    }

                    // Hide the dialog since the algorithm is about to run.
                    setVisible(false);

                    // These next lines set the titles in all frames where the source image is displayed to
                    // "locked - " image name so as to indicate that the image is now read/write locked!
                    // The image frames are disabled and then unregisted from the userinterface until the
                    // algorithm has completed.
                    Vector<ViewImageUpdateInterface> imageFrames = m_srcImage.getImageFrameVector();

                    m_titles = new String[imageFrames.size()];

                    for (int i = 0; i < imageFrames.size(); i++) {
                        m_titles[i] = ((Frame) (imageFrames.elementAt(i))).getTitle();
                        ((Frame) (imageFrames.elementAt(i))).setTitle("Locked: " + m_titles[i]);
                        ((Frame) (imageFrames.elementAt(i))).setEnabled(false);
                        m_userInterface.unregisterFrame((Frame) (imageFrames.elementAt(i)));
                    }

                    if (isRunInSeparateThread()) {

                        // Start the thread as a low priority because we wish to still have user interface work fast.
                        if (m_itkFilterAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {
                        m_itkFilterAlgo.run();
                    }
                } catch (OutOfMemoryError x) {
                    System.gc();
                    MipavUtil.displayError("Dialog Itk Filter: unable to allocate enough memory");

                    return;
                }
            }
        } else if (m_srcImage.getNDims() >= 3) { 

            if (m_outputOptionsPanel.isOutputNewImageSet()) {

                try {

                    // Make result image
                    if (m_srcImage.getType() == ModelImage.ARGB) {
                        m_resultImage = new ModelImage(ModelImage.ARGB, m_srcImage.getExtents(), result_name);
                    } else if (m_srcImage.getType() == ModelImage.ARGB_USHORT) {
                        m_resultImage = new ModelImage(ModelImage.ARGB_USHORT, m_srcImage.getExtents(), result_name);
                    } else if (m_srcImage.getType() == ModelImage.ARGB_FLOAT) {
                        m_resultImage = new ModelImage(ModelImage.ARGB_FLOAT, m_srcImage.getExtents(), result_name);
                    } else {

                        // m_resultImage     = new ModelImage(ModelImage.FLOAT, destExtents, name, m_userInterface);
                        m_resultImage = (ModelImage) m_srcImage.clone();
                        m_resultImage.setImageName(result_name);

                        if ((m_resultImage.getFileInfo()[0]).getFileFormat() == FileUtility.DICOM) {

                            for (int i = 0; i < m_resultImage.getExtents()[2]; i++) {
                                ((FileInfoDicom) (m_resultImage.getFileInfo(i))).setSecondaryCaptureTags();
                            }
                        }
                    }

                    // Make algorithm
                    if (m_image25D || m_itkFilter3D == null) {
                        m_itkFilterAlgo = new AlgorithmItkFilter(m_resultImage, m_srcImage, m_itkFilter2D);
                    } else {
                        m_itkFilterAlgo = new AlgorithmItkFilter(m_resultImage, m_srcImage, m_itkFilter3D);
                    }

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed of failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    m_itkFilterAlgo.addListener(this);

                    createProgressBar(m_srcImage.getImageName(), m_itkFilterAlgo);

                    m_itkFilterAlgo.setRed(m_colorChannelPanel.isRedProcessingRequested());
                    m_itkFilterAlgo.setGreen(m_colorChannelPanel.isGreenProcessingRequested());
                    m_itkFilterAlgo.setBlue(m_colorChannelPanel.isBlueProcessingRequested());

                    if (!m_outputOptionsPanel.isProcessWholeImageSet()) {
                        m_itkFilterAlgo.setMask(m_srcImage.generateVOIMask());
                    }

                    // Hide dialog
                    setVisible(false);

                    if (isRunInSeparateThread()) {

                        // Start the thread as a low priority because we wish to still have user interface work fast.
                        if (m_itkFilterAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {
                        m_itkFilterAlgo.run();
                    }
                } catch (OutOfMemoryError x) {

                    if (m_resultImage != null) {
                        m_resultImage.disposeLocal(); // Clean up image memory
                        m_resultImage = null;
                    }

                    System.gc();
                    MipavUtil.displayError("Dialog Itk Filter: unable to allocate enough memory");

                    return;
                }
            } else {

                try {

                    // Make algorithm
                    if (m_image25D || m_itkFilter3D == null) {
                        m_itkFilterAlgo = new AlgorithmItkFilter(null, m_srcImage, m_itkFilter2D);
                    } else {
                        m_itkFilterAlgo = new AlgorithmItkFilter(null, m_srcImage, m_itkFilter3D);
                    }

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed of failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    m_itkFilterAlgo.addListener(this);

                    createProgressBar(m_srcImage.getImageName(), m_itkFilterAlgo);

                    m_itkFilterAlgo.setRed(m_colorChannelPanel.isRedProcessingRequested());
                    m_itkFilterAlgo.setGreen(m_colorChannelPanel.isGreenProcessingRequested());
                    m_itkFilterAlgo.setBlue(m_colorChannelPanel.isBlueProcessingRequested());

                    if (!m_outputOptionsPanel.isProcessWholeImageSet()) {
                        m_itkFilterAlgo.setMask(m_srcImage.generateVOIMask());
                    }

                    // Hide dialog
                    setVisible(false);

                    // These next lines set the titles in all frames where the source image is displayed to
                    // "locked - " image name so as to indicate that the image is now read/write locked!
                    // The image frames are disabled and then unregisted from the userinterface until the
                    // algorithm has completed.
                    Vector<ViewImageUpdateInterface> imageFrames = m_srcImage.getImageFrameVector();

                    m_titles = new String[imageFrames.size()];

                    for (int i = 0; i < imageFrames.size(); i++) {
                        m_titles[i] = ((Frame) (imageFrames.elementAt(i))).getTitle();
                        ((Frame) (imageFrames.elementAt(i))).setTitle("Locked: " + m_titles[i]);
                        ((Frame) (imageFrames.elementAt(i))).setEnabled(false);
                        m_userInterface.unregisterFrame((Frame) (imageFrames.elementAt(i)));
                    }

                    if (isRunInSeparateThread()) {

                        // Start the thread as a low priority because we wish to still have user interface work fast.
                        if (m_itkFilterAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {
                        m_itkFilterAlgo.run();
                    }
                } catch (OutOfMemoryError x) {
                    System.gc();
                    MipavUtil.displayError("Dialog Itk Filter: unable to allocate enough memory");

                    return;
                }
            }
        }
    }

    /**
     * Perform any actions required after the running of the algorithm is
     * complete.
     */
    protected void doPostAlgorithmActions() {

        if (m_outputOptionsPanel.isOutputNewImageSet()) {
            AlgorithmParameters.storeImageInRunner(getResultImage());
        }
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(m_srcImage);
        // First stores which filter to run:
        String name = (m_itkFilter2D != null ? m_itkFilter2D.getName() : m_itkFilter3D.getName());
        scriptParameters.storeItkFilterName(name);

        scriptParameters.storeOutputImageParams(m_resultImage, m_outputOptionsPanel.isOutputNewImageSet());

        scriptParameters.storeProcessingOptions(m_outputOptionsPanel.isProcessWholeImageSet(), m_image25D);
        scriptParameters.storeColorOptions(m_colorChannelPanel);
        // Store these, which might be edited, last.
        scriptParameters.storeItkMethods(m_filterParamPanel);
    }

    /**
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {
        m_srcImage = scriptParameters.retrieveInputImage();
        m_userInterface = ViewUserInterface.getReference();
        parentFrame = m_srcImage.getParentFrame();

        m_outputOptionsPanel = new JPanelAlgorithmOutputOptions(m_srcImage);

        m_colorChannelPanel = new JPanelColorChannels(m_srcImage);

        scriptParameters.setOutputOptionsGUI(m_outputOptionsPanel);
        setImage25D(scriptParameters.doProcess3DAs25D());

        scriptParameters.setColorOptionsGUI(m_colorChannelPanel);

        // construct the input filter.
        String base_name = scriptParameters.getItkFilterName();
        if (base_name == null) return;

        int pixel_type = m_srcImage.getType();
        String pixel_type_str = AutoItkLoader.getItkModelImageString(pixel_type);
        String data_type_2D = pixel_type_str + "2";
        String data_type_3D = pixel_type_str + "3";

        // only one filter created.
        if (m_image25D) {
            m_itkFilter2D = new PItkFilter(base_name, data_type_2D, data_type_2D);
        } else {
            m_itkFilter3D = new PItkFilter(base_name, data_type_3D, data_type_3D);
        }
        m_filterParamPanel = new JPanelItkFilterParams(m_srcImage, 
                                        (m_itkFilter2D == null ? null : m_itkFilter2D.filter()), 
                                        (m_itkFilter3D == null ? null : m_itkFilter3D.filter()),
                                                       m_image25D);
        
        scriptParameters.setItkFilterParamGUI(m_filterParamPanel);
    }


    /**
     * Set up the Itk filters, if successful then
     * sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private boolean init(FilterRecordItk fr) {

        // try to create filters first, fail if not available. 
        int pixel_type = m_srcImage.getType();
        String pixel_type_str = AutoItkLoader.getItkModelImageString(pixel_type);
        int model_dims = m_srcImage.getNDims();

        if (model_dims != 2 && model_dims != 3) {
            JOptionPane.showMessageDialog(parentFrame,
                                          "Itk filters only support images of 2 or 3 dimensions. " +
                                          "Please convert the image first.", 
                                          "Filter class input mismatch",
                                          JOptionPane.PLAIN_MESSAGE);
            return false;
            
        }

        // A few filter use "F2" as shorthand for "F2F2", so separate input/output types.
        String data_type_2D = pixel_type_str + "2";
        String data_type_3D = pixel_type_str + "3";
            
        PItkFilter filter_obj_2D = new PItkFilter(fr.m_Name, data_type_2D, data_type_2D);
        PItkFilter filter_obj_3D = null;
        if (model_dims == 3) {
            filter_obj_3D = new PItkFilter(fr.m_Name, data_type_3D, data_type_3D);
        }
        // one or the other needs to be OK.
        if ((filter_obj_2D == null || filter_obj_2D.filter() == null) && 
            (filter_obj_3D == null || filter_obj_3D.filter() == null)) {
            // TODO user friendly available types.
            String msg = "The filter " + fr.m_Name + " does not accept input/output data of type: " + 
                ModelImage.getBufferTypeStr(pixel_type) + " " + model_dims + "D.\n"
                + "Please convert the image first to one of these types: \n" +
                fr.m_IOType;
            JOptionPane.showMessageDialog(parentFrame,
                                          msg, "Filter class input mismatch",
                                          JOptionPane.PLAIN_MESSAGE);
            return false;
        }
        // We have filters, 2D, 3D or both.
        m_itkFilter2D = filter_obj_2D;
        m_itkFilter3D = filter_obj_3D;
        
        setForeground(Color.black);

        setTitle(fr.m_Name + " Itk Filter");
        getContentPane().setLayout(new BorderLayout());

        boolean select_25D = false;
        if (m_srcImage.getNDims() == 3) {
            select_25D = m_srcImage.getFileInfo()[0].getIs2_5D();
        }

        m_filterParamPanel = new JPanelItkFilterParams(m_srcImage, 
                                        (m_itkFilter2D == null ? null : m_itkFilter2D.filter()), 
                                        (m_itkFilter3D == null ? null : m_itkFilter3D.filter()),
                                        select_25D);
        m_colorChannelPanel = new JPanelColorChannels(m_srcImage);
        m_outputOptionsPanel = new JPanelAlgorithmOutputOptions(m_srcImage);

        PanelManager paramPanelManager = new PanelManager();
        JScrollPane jsp = new JScrollPane(m_filterParamPanel, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                                                 JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
        // some adjustments so filter params expand to available space:
        GridBagConstraints constraints = paramPanelManager.getConstraints();
        constraints.fill = GridBagConstraints.BOTH;
        constraints.weighty = 1;
        paramPanelManager.add(jsp);
        // reset:
        constraints.weighty = 0;
        constraints.fill = GridBagConstraints.HORIZONTAL;

        paramPanelManager.addOnNextLine(m_colorChannelPanel);
        paramPanelManager.addOnNextLine(m_outputOptionsPanel);

        getContentPane().add(paramPanelManager.getPanel(), BorderLayout.CENTER);
        getContentPane().add(buildButtons(), BorderLayout.SOUTH);
        pack();
        setResizable(true);

        System.gc();
        return true;
    }

    /**
     * Use the GUI results to set up the variables needed to run the
     * algorithm.
     *
     * @return <code>true</code> if parameters set successfully,
     * <code>false</code> otherwise.
     */
    private boolean setVariables() {

        // set all the parameters that were changed by the user.
        m_image25D = m_filterParamPanel.is2DActive();
        m_filterParamPanel.runSetMethods();

        return true;
    }
}
