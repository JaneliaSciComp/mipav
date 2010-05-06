package gov.nih.mipav.view;


import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import java.awt.Color;
import java.awt.Dimension;

import java.io.*;

import java.util.Enumeration;
import java.util.Hashtable;
import java.util.Vector;

import javax.swing.*;


/**
 * A frame which can initialize, store, and switch between multiple images.
 *
 * <p>This frame can be extended to create a wizard-like frame (see <b>ViewJFrameWizard</b> in the RFAST project. Here a
 * a number of steps that one would probably need to do to create a wizard:</p>
 *
 * <ul>
 *   <li>Create a number of static strings to identify the stages of the wizard (these will be used to save and retrieve
 *     the step state).</li>
 *   <li>Implement <code>initGUI()</code> to setup the wizard interface (ie - a tabbed pane or card layout). Should also
 *     setup the menubar, frame title, frame size and load the initial step state.</li>
 *   <li>Might need to override the <b>ViewJFrameImage</b> <code>updateImages()</code> methods (there have been problems
 *     with repainting controls).</li>
 *   <li>Override <code>initControls()</code> to change the toolbar controls for different images.</li>
 *   <li>Create parameter panels for actions performed within each step (see <b>JPanelLiver</b> and <b>
 *     JPanelVOISnake</b> in the RFAST project).</li>
 *   <li>Methods to save / retrieve project parameters.</li>
 *   <li>Might want to add in a function to save all of the wizard state (<code>saveAll()</code>).</li>
 *   <li>Implement <code>finalize()</code> (to clean up allocated memory) and <code>close()</code> (for actions required
 *     when closing the frame).</li>
 * </ul>
 *
 * @author   Evan McCreedy
 * @version  1.0 August 9, 2004
 */
public abstract class ViewJFrameMultiple extends ViewJFrameImage {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -1374582208019835548L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** The state keys used to store/retrieve the image states, (optionally) in order. */
    public StateList states;

    /**
     * Table of ComponentImages to be displayed on various frames. The current component image is stored in <b>
     * componentImage</b>.
     */
    protected Hashtable componentTable;

    /** Table of toolbar image controls. The current controls are stored in <b>controls</b>. */
    protected Hashtable controlsTable;

    /** The last key used to access one of the images of this frame. */
    protected String currentKey;

    /**
     * Table of images being used in this frame. The key for each image indicates which tab it should be used for.
     * Stored in <b>imageA</b>.
     */
    protected Hashtable imageATable;

    /**
     * Table of images being used in this frame. The key for each image indicates which tab it should be used for.
     * Stored in <b>imageB</b>.
     */
    protected Hashtable imageBTable;

    /** Table of LUTs to accompany the image table. Stored in <b>LUTa</b>. */
    protected Hashtable LUTaTable;

    /** Table of LUTs to accompany the image table. Stored in <b>LUTb</b>. */
    protected Hashtable LUTbTable;

    /** Contains parameters and images to be used by the frame. */
    protected FileInfoProject projectInfo;

    /**
     * Table of the scroll panes, which hold the component images. The current scroll pane is stored in <b>
     * scrollPane</b>.
     */
    protected Hashtable scrollPaneTable;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new frame.
     *
     * @param  info  information about the files and parameters the frame should work with
     * @param  loc   location where the frame should be placed initially
     */
    public ViewJFrameMultiple(FileInfoProject info, Dimension loc) {
        super(null, null, loc, false);

        states = new StateList();
        initStates();

        projectInfo = info;

        // create and build the menus and toolbar
        menuBuilder = new ViewMenuBuilder(this);

        initImageTable(info);

        initGUI();

        initLocation(loc);

        if (userInterface.isAppFrameVisible()) {
            setVisible(true);
        } else {
            setVisible(false);
        }

        userInterface.registerFrame(this);

        setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Creates a deep copy of an image.
     *
     * @param   img   the image to copy
     * @param   name  text to append to the image's name for use as the clone's name
     *
     * @return  the new image copy
     */
    public static final ModelImage makeImageClone(ModelImage img, String name) {
        return (ModelImage) img.clone(newImageName(img, name));
    }

    /**
     * Takes the name from an image (&quot;name&quot;) and appends an underscore and some other text to it
     * (&quot;text&quot;), then returns the result (&quot;imageName_text&quot;).
     *
     * @param   img   the image to base the new name on
     * @param   text  the text to append to the image name
     *
     * @return  the new image name
     */
    public static final String newImageName(ModelImage img, String text) {
        return img.getImageName() + "_" + text;
    }

    /**
     * Close the frame.
     */
    public void close() {
        super.close();
    }

    /**
     * Copy all VOIs from the images of one state type to the images of another state type.
     *
     * @param  from  the key to copy the VOIs from
     * @param  to    the key to copy the VOIs into
     */
    public void copyVOIs(String from, String to) {
        getImageA(to).setVOIs(getImageA(from).getVOIs());

        if (getImageB(to) != null) {
            getImageB(to).setVOIs(getImageA(from).getVOIs());
        }
    }

    /**
     * Clean up the frame's memory.
     *
     * @throws  Throwable  if a problem is encountered freeing the memory
     */
    public void finalize() throws Throwable {
        super.finalize();

        if (projectInfo != null) {
            projectInfo.finalize();
        }

        projectInfo = null;

        if (imageATable != null) {
            Enumeration e = imageATable.elements();

            while (e.hasMoreElements()) {
                ModelImage img = (ModelImage) e.nextElement();

                img.disposeLocal();
            }

            imageATable.clear();
        }

        imageATable = null;

        if (imageBTable != null) {
            Enumeration e = imageBTable.elements();

            while (e.hasMoreElements()) {
                ModelImage img = (ModelImage) e.nextElement();

                img.disposeLocal();
            }

            imageBTable.clear();
        }

        imageBTable = null;

        if (LUTaTable != null) {
            Enumeration e = LUTaTable.elements();

            while (e.hasMoreElements()) {
                ModelLUT lut = (ModelLUT) e.nextElement();

                lut.disposeLocal();
            }

            LUTaTable.clear();
        }

        LUTaTable = null;

        if (LUTbTable != null) {
            Enumeration e = LUTbTable.elements();

            while (e.hasMoreElements()) {
                ModelLUT lut = (ModelLUT) e.nextElement();

                lut.disposeLocal();
            }

            LUTbTable.clear();
        }

        LUTbTable = null;

        if (componentTable != null) {
            Enumeration e = componentTable.elements();

            while (e.hasMoreElements()) {
                ViewJComponentEditImage compImg = (ViewJComponentEditImage) e.nextElement();

                compImg.dispose(false);
            }

            componentTable.clear();
        }

        componentTable = null;

        if (scrollPaneTable != null) {
            scrollPaneTable.clear();
        }

        scrollPaneTable = null;

        if (controlsTable != null) {
            Enumeration e = controlsTable.elements();

            while (e.hasMoreElements()) {
                ViewControlsImage cont = (ViewControlsImage) e.nextElement();

                cont.finalize();
            }

            controlsTable.clear();
        }

        controlsTable = null;

        if (states != null) {
            states.removeAllElements();
        }

        states = null;

        currentKey = null;
    }

    /**
     * Gets the component image with a specified image type.
     *
     * @param   type  type of image to try to get
     *
     * @return  a component image
     */
    public ViewJComponentEditImage getComponentImage(String type) {

        if (componentTable == null) {
            Preferences.debug("Tried to get image, but component image table was null\n");

            return null;
        }

        if (componentTable.containsKey(type)) {
            return (ViewJComponentEditImage) componentTable.get(type);
        } else {

            // MipavUtil.displayError("Tried to access a component image which doesn't exist yet");
            return null;
        }
    }

    /**
     * Gets the controls of a specified image type.
     *
     * @param   type  type of image to try to get
     *
     * @return  a image's controls
     */
    public ViewControlsImage getControls(String type) {

        if (controlsTable == null) {
            Preferences.debug("Tried to get image, but image controls table was null\n");

            return null;
        }

        if (componentTable.containsKey(type)) {
            return (ViewControlsImage) controlsTable.get(type);
        } else {

            // MipavUtil.displayError("Tried to access image controls which don't exist yet");
            return null;
        }
    }

    /**
     * Returns the toolbar of the current image (same as <code>getControls()</code>).
     *
     * @return  the current toolbar
     */
    public ViewControlsImage getCurrentControls() {
        return controls;
    }

    /**
     * Return the last accessed image key.
     *
     * @return  the key of the currently loaded image state
     */
    public String getCurrentKey() {
        return currentKey;
    }

    /**
     * Returns the scroll pane of the component image currently being worked on.
     *
     * @return  the current scrollPane
     */
    public JScrollPane getCurrentScrollPane() {
        return scrollPane;
    }

    /**
     * Gets the image with a specified image type (or one of the unclassified images if no image with that type).
     *
     * @param   type  type of image to try to get
     *
     * @return  an image (hopefully of the requested type)
     */
    public synchronized ModelImage getImageA(String type) {

        if (imageATable == null) {
            Preferences.debug("Tried to get image, but imageA table was null\n");

            return null;
        }

        // check to see if we've seen this type before (either in initImageTable() or this function)
        if (imageATable.containsKey(type)) {
            return (ModelImage) imageATable.get(type);
        } else {
            boolean needCopy = states.getInfo(type).uniqueImage;

            // use a copy of any image
            Enumeration e = imageATable.keys();

            if (e.hasMoreElements()) {
                String temp = (String) e.nextElement();

                ModelImage tempImg = (ModelImage) imageATable.get(temp);

                ModelImage newImg;

                if (needCopy) {
                    newImg = makeImageClone(tempImg, type);
                } else {
                    newImg = tempImg;
                }

                initImage(type, IMAGE_A, newImg, null);

                if (needCopy) {
                    saveImageCopy(newImg);
                }

                return newImg;
            } else {
                MipavUtil.displayError("Unable to find an image to display in the wizard");

                return null;
            }
        }
    }

    /**
     * Gets the image with a specified image type (or one of the unclassified images if no image with that type).
     *
     * @param   type  type of image to try to get
     *
     * @return  an image (hopefully of the requested type)
     */
    public synchronized ModelImage getImageB(String type) {

        if (imageBTable == null) {
            Preferences.debug("Tried to get image, but imageB table was null\n");

            return null;
        }

        // check to see if we've seen this type before (either in initImageTable() or this function)
        if (imageBTable.containsKey(type)) {
            return (ModelImage) imageBTable.get(type);
        } else {
            boolean needCopy = states.getInfo(type).uniqueImage;

            // use a copy of any image
            Enumeration e = imageBTable.keys();

            if (e.hasMoreElements()) {
                String temp = (String) e.nextElement();

                ModelImage tempImg = (ModelImage) imageBTable.get(temp);

                ModelImage newImg;

                if (needCopy) {
                    newImg = makeImageClone(tempImg, type);
                } else {
                    newImg = tempImg;
                }

                initImage(type, IMAGE_B, newImg, null);

                if (needCopy) {
                    saveImageCopy(newImg);
                }

                return newImg;
            } else {

                // MipavUtil.displayError("Unable to find an image to display in the wizard");
                return null;
            }
        }
    }

    /**
     * Gets the lut of an image with a specified image type (or of the last unclassified image used if no lut with that
     * type). The image corressponding to the requested lut should be accessed before the lut.
     *
     * @param   type  type of image whose lut to try to get
     *
     * @return  a lut (hopefully of the requested type)
     */
    public ModelLUT getLUTa(String type) {

        if (LUTaTable == null) {
            Preferences.debug("Tried to get lut, but lut table was null\n");

            return null;
        }

        if (LUTaTable.containsKey(type)) {
            return (ModelLUT) LUTaTable.get(type);
        } else {

            // MipavUtil.displayError( "Tried to access the lut for a type before accessing the image" );
            return null;
        }
    }

    /**
     * Gets the lut of an image with a specified image type (or of the last unclassified image used if no lut with that
     * type). The image corressponding to the requested lut should be accessed before the lut.
     *
     * @param   type  type of image whose lut to try to get
     *
     * @return  a lut (hopefully of the requested type)
     */
    public ModelLUT getLUTb(String type) {

        if (LUTbTable == null) {
            Preferences.debug("Tried to get lut, but lut table was null\n");

            return null;
        }

        if (LUTbTable.containsKey(type)) {
            return (ModelLUT) LUTbTable.get(type);
        } else {

            // MipavUtil.displayError( "Tried to access the lut for a type before accessing the image" );
            return null;
        }
    }

    /**
     * Get the project information for the frame.
     *
     * @return  the project info
     */
    public FileInfoProject getProjectInfo() {
        return projectInfo;
    }

    /**
     * Gets the scroll pane with a specified image type.
     *
     * @param   type  type of image to try to get
     *
     * @return  a scroll pane
     */
    public JScrollPane getScrollPane(String type) {

        if (scrollPaneTable == null) {
            Preferences.debug("Tried to get image, but scroll pane table was null\n");

            return null;
        }

        if (scrollPaneTable.containsKey(type)) {
            return (JScrollPane) scrollPaneTable.get(type);
        } else {

            // MipavUtil.displayError("Tried to access a scroll pane which doesn't exist yet");
            return null;
        }
    }

    /**
     * Load state into the frame for a particular component image that we want to work with.
     *
     * @param  compImg  the component image to load the state for
     */
    public synchronized void initComponentImageVars(ViewJComponentEditImage compImg) {
        ModelImage img = compImg.getActiveImage();

        int[] slices = initSlicePositions(img);
        int[] numSlices = initNumSlices(img);

        compImg.setSlice(slices[0]);
        compImg.setTimeSlice(slices[1]);
        
        nImage = numSlices[0];
        nTImage = numSlices[1];

        resols = initResolutions(img);
        units = initUnits(img);

        float[] resFactor = initResFactor(resols, units);

        zoom = initZoom(img, resFactor[0], resFactor[1], xScreen, yScreen);

        imageBufferA = compImg.getImageBufferA();
        imageBufferB = compImg.getImageBufferB();
        pixBuffer = compImg.getPixBuffer();
        pixBufferB = compImg.getPixBufferB();
    }

    /**
     * Initialize an image for use in the wizard.
     *
     * @param  type      which step the image is to be used in
     * @param  loadInto  where to load in image into (IMAGE_A or IMAGE_B)
     * @param  img       the image to initialize
     * @param  imgLUT    the image's LUT (or null if it doesn't have one yet)
     */
    public synchronized void initImage(String type, int loadInto, ModelImage img, ModelLUT imgLUT) {
        img.setProjectInfo(projectInfo);
        ///img.getFileInfo()[0].setModality( FileInfoBase.UNKNOWN_MODALITY );

        if (img.getNDims() == 3) {

            for (int i = 0; i < img.getExtents()[2]; i++) {
                img.getFileInfo()[i].setFileDirectory(userInterface.getDefaultDirectory() + File.separator);
                img.getFileInfo()[i].setFileName(img.getImageName() + ".raw");
                img.getFileInfo()[i].setFileSuffix(".xml");
            }
        }

        // add image
        if (loadInto == IMAGE_B) {
            imageBTable.put(type, img);
        } else {
            imageATable.put(type, img);
        }

        // MUST register frame to image models
        img.addImageDisplayListener(this);

        // replace the lut for the current image type with the lut of the result image
        if (imgLUT == null) {
            imgLUT = initLUT(img);
        }

        if (loadInto == IMAGE_B) {
            LUTbTable.put(type, imgLUT);
        } else {
            LUTaTable.put(type, imgLUT);
        }

        ViewJComponentEditImage compImg;

        if (loadInto == IMAGE_A) {
            compImg = initComponentImage(this, img, imgLUT);
            componentTable.put(type, compImg);
        } else {
            compImg = getComponentImage(type);
            setImageB(compImg, img);
        }

        // take special action if replacing the current image
        if (type.equals(getCurrentKey())) {

            if (loadInto == IMAGE_B) {
                setCurrentImageB(img);
                setCurrentLUTb(imgLUT);
            } else {
                setCurrentImageA(img);
                setCurrentLUTa(imgLUT);
            }

            setCurrentComponentImage(compImg);

            initScrollPane(type);
            initControls(type);
            setCurrentScrollPane(getScrollPane(type));
            setCurrentControls(getControls(type));
        }
    }

    /**
     * Gets and categorizes by imageType the images associated with a project.
     *
     * @param  info  information about the project to be worked on
     */
    public synchronized void initImageTable(FileInfoProject info) {
        imageATable = new Hashtable();
        imageBTable = new Hashtable();
        LUTaTable = new Hashtable();
        LUTbTable = new Hashtable();
        componentTable = new Hashtable();
        scrollPaneTable = new Hashtable();
        controlsTable = new Hashtable();

        Enumeration e = info.getImageKeys();

        Vector imageBs = new Vector();

        // init the imageAs and hold off on any imageBs
        while (e.hasMoreElements()) {
            String file = (String) e.nextElement();
            FileInfoProject.ImageInfo infoItem = info.getImage(file).getInfo("imageType");
            FileInfoProject.ImageInfo displayItem = info.getImage(file).getInfo("loadInto");

            if ((displayItem != null) && displayItem.getValue().equalsIgnoreCase("imageB")) {
                imageBs.addElement(file);
            }

            if (infoItem != null) {
                String type = infoItem.getValue();

                if ((type != null) && !type.equals("")) {
                    loadImageFromFile(type, file, IMAGE_A);
                } else {
                    Preferences.debug("Image found without a type in project file.\n");
                }
            } else {
                Preferences.debug("Image found without any associated info in project file.\n");
            }
        }

        for (int i = 0; i < imageBs.size(); i++) {
            String file = (String) imageBs.elementAt(i);
            FileInfoProject.ImageInfo infoItem = info.getImage(file).getInfo("imageType");

            if (infoItem != null) {
                String type = infoItem.getValue();

                if ((type != null) && !type.equals("")) {
                    loadImageFromFile(type, file, IMAGE_B);
                } else {
                    Preferences.debug("Image found without a type in project file.\n");
                }
            } else {
                Preferences.debug("Image found without any associated info in project file.\n");
            }
        }
    }

    /**
     * Set the initial location of the wizard frame.
     *
     * @param  loc  the desired initial location
     */
    public void initLocation(Dimension loc) {

        if (loc != null) {

            if ((loc.width + getSize().width) > xScreen) {
                loc.width = xScreen - getSize().width - 100;
            }

            if ((loc.height + getSize().height) > yScreen) {
                loc.height = yScreen - getSize().height - 100;
            }

            setLocation(loc.width, loc.height);
        }
    }

    /**
     * Save an xml copy of an image.
     *
     * @param  img  ModelImage
     */
    public void saveImageCopy(ModelImage img) {

        if (img == getActiveImage()) {

            // if we're saving the current image, make sure any image info changes have been saved
            saveImageInfo();
        }

        // only want to save the image here (not the VOIs, LUT, etc.)
        boolean temp = Preferences.is(Preferences.PREF_SAVE_ALL_ON_SAVE);
        Preferences.setProperty(Preferences.PREF_SAVE_ALL_ON_SAVE, "false");

        FileWriteOptions options = new FileWriteOptions(img.getImageName() + ".xml",
                                                        userInterface.getDefaultDirectory() + File.separator, true);

        options.setFileType(FileUtility.XML);

        if (img.getNDims() == 3) {
            options.setBeginSlice(0);
            options.setEndSlice(img.getExtents()[2] - 1);
        }

        options.setOptionsSet(true);

        save(img, options, -1);

        Preferences.setProperty(Preferences.PREF_SAVE_ALL_ON_SAVE, Boolean.toString(temp));
    }

    /**
     * Saves the images and other info to the frame's FileInfoProject object.
     *
     * @return  true if all state saved successfully, false otherwise.
     */
    public synchronized boolean saveProjectInfo() {

        // remove all images from the project
        projectInfo.getImages().clear();

        // save the paths to images being worked on
        Enumeration ie = imageATable.keys();

        while (ie.hasMoreElements()) {
            String type = (String) (ie.nextElement());

            int i = getImageA(type).getImageFileName().lastIndexOf(".");

            if (i < 0) {
                i = getImageA(type).getImageFileName().length();
            }

            String imagePath = getImageA(type).getFileInfo()[0].getFileDirectory() +
                               getImageA(type).getImageFileName().substring(0, i) +
                               getImageA(type).getFileInfo()[0].getFileSuffix();

            projectInfo.addImage(imagePath);
            projectInfo.getImage(imagePath).addInfo("imageType");
            projectInfo.getImage(imagePath).getInfo("imageType").setDescription("determines how/where this image is used by programs that use it");
            projectInfo.getImage(imagePath).getInfo("imageType").setValueType("string");
            projectInfo.getImage(imagePath).getInfo("imageType").setValue(type);
            projectInfo.getImage(imagePath).addInfo("display");
            projectInfo.getImage(imagePath).getInfo("display").setDescription("Whether to display this image in a frame when the project is loaded.");
            projectInfo.getImage(imagePath).getInfo("display").setValueType("boolean");
            projectInfo.getImage(imagePath).getInfo("display").setValue("true");
            projectInfo.getImage(imagePath).addInfo("frameType");
            projectInfo.getImage(imagePath).getInfo("frameType").setDescription("What type of frame to load this image in.");
            projectInfo.getImage(imagePath).getInfo("frameType").setValueType("string");
            projectInfo.getImage(imagePath).getInfo("frameType").setValue("multiple");
            projectInfo.getImage(imagePath).addInfo("loadInto");
            projectInfo.getImage(imagePath).getInfo("loadInto").setDescription("Where to put the image (imageA or imageB).");
            projectInfo.getImage(imagePath).getInfo("loadInto").setValueType("string");
            projectInfo.getImage(imagePath).getInfo("loadInto").setValue("imageA");
        }

        ie = imageBTable.keys();

        while (ie.hasMoreElements()) {
            String type = (String) (ie.nextElement());

            int i = getImageB(type).getImageFileName().lastIndexOf(".");

            if (i < 0) {
                i = getImageB(type).getImageFileName().length();
            }

            String imagePath = getImageB(type).getFileInfo()[0].getFileDirectory() +
                               getImageB(type).getImageFileName().substring(0, i) +
                               getImageB(type).getFileInfo()[0].getFileSuffix();

            projectInfo.addImage(imagePath);
            projectInfo.getImage(imagePath).addInfo("imageType");
            projectInfo.getImage(imagePath).getInfo("imageType").setDescription("determines how/where this image is used by programs that use it");
            projectInfo.getImage(imagePath).getInfo("imageType").setValueType("string");
            projectInfo.getImage(imagePath).getInfo("imageType").setValue(type);
            projectInfo.getImage(imagePath).addInfo("display");
            projectInfo.getImage(imagePath).getInfo("display").setDescription("Whether to display this image in a frame when the project is loaded.");
            projectInfo.getImage(imagePath).getInfo("display").setValueType("boolean");
            projectInfo.getImage(imagePath).getInfo("display").setValue("true");
            projectInfo.getImage(imagePath).addInfo("frameType");
            projectInfo.getImage(imagePath).getInfo("frameType").setDescription("What type of frame to load this image in.");
            projectInfo.getImage(imagePath).getInfo("frameType").setValueType("string");
            projectInfo.getImage(imagePath).getInfo("frameType").setValue("multiple");
            projectInfo.getImage(imagePath).addInfo("loadInto");
            projectInfo.getImage(imagePath).getInfo("loadInto").setDescription("Where to put the image (imageA or imageB).");
            projectInfo.getImage(imagePath).getInfo("loadInto").setValueType("string");
            projectInfo.getImage(imagePath).getInfo("loadInto").setValue("imageB");
        }

        return true;
    }

    /**
     * Changes the reference to the component image currently being worked on.
     *
     * @param  component  the new current component image
     */
    public synchronized void setCurrentComponentImage(ViewJComponentEditImage component) {
        componentImage = component;
    }

    /**
     * Set the current component image reference to the component image image from the step <code>type</code>.
     *
     * @param  type  the step to load the current compoent image from
     */
    public final synchronized void setCurrentComponentImage(String type) {
        setCurrentComponentImage(getComponentImage(type));
    }

    /**
     * Changes the reference to the toolbar of the current tab.
     *
     * @param  tool  the new current toolbar
     */
    public synchronized void setCurrentControls(ViewControlsImage tool) {
        controls = tool;
    }

    /**
     * Set the current image controls reference to the controls with key <code>type</code>.
     *
     * @param  key  the step to load the current image controls from
     */
    public synchronized void setCurrentControls(String key) {
        setCurrentControls(getControls(key));
    }

    /**
     * Changes the reference to the image currently being worked on.
     *
     * @param  img  the new current image
     */
    public synchronized void setCurrentImageA(ModelImage img) {
        imageA = img;
    }

    /**
     * Set the current imageA reference to the image from the step <code>type</code>.
     *
     * @param  type  the step to load the current image from
     */
    public final synchronized void setCurrentImageA(String type) {
        setCurrentImageA(getImageA(type));
    }

    /**
     * Changes the reference to the image currently being worked on.
     *
     * @param  img  the new current image
     */
    public synchronized void setCurrentImageB(ModelImage img) {
        imageB = img;
    }

    /**
     * Set the current imageB reference to the image from the step <code>type</code>.
     *
     * @param  type  the step to load the current image from
     */
    public final synchronized void setCurrentImageB(String type) {
        setCurrentImageB(getImageB(type));
    }

    /**
     * Changes the reference to the imageA lut currently being worked on.
     *
     * @param  lut  the new current lut
     */
    public synchronized void setCurrentLUTa(ModelLUT lut) {
        LUTa = lut;
    }

    /**
     * Set the current LUT reference to the LUTa from the step <code>type</code>.
     *
     * @param  type  the step to load the current LUT from
     */
    public final synchronized void setCurrentLUTa(String type) {
        setCurrentLUTa(getLUTa(type));
    }

    /**
     * Changes the reference to the imageB lut of currently being worked on.
     *
     * @param  lut  the new current lut
     */
    public synchronized void setCurrentLUTb(ModelLUT lut) {
        LUTb = lut;
    }

    /**
     * Set the current LUT reference to the LUTb from the step <code>type</code>.
     *
     * @param  type  the step to load the current LUT from
     */
    public final synchronized void setCurrentLUTb(String type) {
        setCurrentLUTb(getLUTb(type));
    }

    /**
     * Changes the reference to the scroll pane of the component image currently being worked on.
     *
     * @param  scroll  the new current component image
     */
    public synchronized void setCurrentScrollPane(JScrollPane scroll) {
        scrollPane = scroll;
    }

    /**
     * Set the current scroll pane reference to the scroll pane with key <code>type</code>.
     *
     * @param  key  the step to load the current scroll pane from
     */
    public synchronized void setCurrentScrollPane(String key) {
        setCurrentScrollPane(getScrollPane(key));
    }

    /**
     * Sets the reference to imageB.
     *
     * @param  compImg  the component image to load imageB into
     * @param  _imageB  image to set the frame to
     */
    public void setImageB(ViewJComponentEditImage compImg, ModelImage _imageB) {

        Vector frameList = compImg.getImageA().getImageFrameVector();
        float min, max;

        if (frameList == null) {
            return;
        }

        for (int i = 0; i < frameList.size(); i++) {

            if ((frameList.elementAt(i) instanceof ViewJFrameBase) &&
                    (((ViewJFrameBase) frameList.elementAt(i)) != this)) {
                ((ViewJFrameBase) frameList.elementAt(i)).setImageB(_imageB);
            }
        }

        if ((compImg.getImageB() != null) && (!compImg.getImageB().equals(_imageB))) {
            compImg.getImageB().disposeLocal(); // Dispose of the memory of the old image
        }

        compImg.setImageB(_imageB);

        setZoomB(compImg);

        compImg.getImageB().addImageDisplayListener(this);

        if ((compImg.getImageA().isColorImage() == false) && (compImg.getImageB().isColorImage() == false)) {
            int[] dimExtentsLUT = new int[2];

            dimExtentsLUT[0] = 4;
            dimExtentsLUT[1] = 256;

            ModelLUT LUT = new ModelLUT(ModelLUT.GRAY, 256, dimExtentsLUT);

            if (compImg.getImageB().getType() == ModelStorageBase.UBYTE) {
                min = 0;
                max = 255;
            } else if (compImg.getImageB().getType() == ModelStorageBase.BYTE) {
                min = -128;
                max = 127;
            } else {
                min = (float) compImg.getImageB().getMin();
                max = (float) compImg.getImageB().getMax();
            }

            float imgMin = (float) compImg.getImageB().getMin();
            float imgMax = (float) compImg.getImageB().getMax();

            LUT.resetTransferLine(min, imgMin, max, imgMax);

            compImg.setLUTb(LUT);
            // updateImages(null, LUT, true);
            // compImg.getImageB().notifyImageDisplayListeners( LUT, true );

            if (compImg.getImageA().getHistoLUTFrame() != null) {

                // compImg.getImageB().registerHistoLUTFrame(compImg.getImageA().getHistoLUTFrame());
                compImg.getImageB().addImageDisplayListener(compImg.getImageA().getHistoLUTFrame());
                updateHistoLUTFrame(IMAGE_B, compImg);
            }
        } else if (compImg.getImageA().isColorImage() && compImg.getImageB().isColorImage()) {
            compImg.getImageB().notifyImageDisplayListeners(null, true);

            if (compImg.getImageA().getHistoRGBFrame() != null) {
                compImg.getImageB().addImageDisplayListener(compImg.getImageA().getHistoRGBFrame());
                updateHistoRGBFrame(IMAGE_B, compImg);
            }
        }
    }

    /**
     * Sets the magnification of Image B. Allocates the image buffers so that it can be displayed properly.
     *
     * @param   compImg  the component image to allocate buffers for
     *
     * @return  true if setting the zoom on imageB was successful
     */
    public boolean setZoomB(ViewJComponentEditImage compImg) {
        int xDim = compImg.getImageA().getExtents()[0];
        int yDim = compImg.getImageA().getExtents()[1];

        float[] imageTempB = null;
        int[] tempPixBufferB = null;

        try {

            if (compImg.getImageB() != null) {
                tempPixBufferB = new int[compImg.getPixBuffer().length];

                int bufferFactor = 1;

                if (compImg.getImageA().isColorImage()) {
                    bufferFactor = 4;
                }

                imageTempB = new float[bufferFactor * xDim * yDim];
            }
        } catch (OutOfMemoryError error) {
            imageTempB = null;
            System.gc();
            throw (error);
        }

        compImg.setBuffers(compImg.getImageBufferA(), imageTempB, compImg.getPixBuffer(), tempPixBufferB);
        initComponentImageVars(compImg);

        return true;
    }

    /**
     * This methods calls the componentImage's REPAINT method to redraw the screen. The extents on this image have
     * changed, so the extents need to be read in again and menus, panes and slide bars adjusted accordingly.
     *
     * @return  whether the image extents update was successful
     */
    public synchronized boolean updateImageExtents() {
        ViewJComponentEditImage compImg = initComponentImage(this, getImageA(), getLUTa());
        initComponentImageVars(compImg);

        setCurrentComponentImage(compImg);
        componentTable.put(getCurrentKey(), compImg);

        setSlice(compImg.getSlice());

        return true;

    } // end updateImageExtents()

    /**
     * Initialize the frame's user interface.
     */
    protected abstract void initGUI();

    /**
     * Initialize the state identification keys.
     */
    protected abstract void initStates();

    /**
     * Creates and initializes the component image for the given image.
     *
     * @param   frame   the frame to attach the component image to
     * @param   img     the image to create a component image for
     * @param   imgLUT  the lut of the image
     *
     * @return  the new component image
     *
     * @throws  OutOfMemoryError  if enough memory cannot be allocated for this method
     */
    protected static final synchronized ViewJComponentEditImage initComponentImage(ViewJFrameBase frame, ModelImage img,
                                                                                   ModelLUT imgLUT)
            throws OutOfMemoryError {

        // create buffers for the new component image
        int[] extents = initExtents(img);
        float[] imageBuf = initImageBuffer(extents, img.isColorImage());
        int[] pixBuf = initPixelBuffer(extents);

        float[] imgResols = initResolutions(img);
        int[] imgUnits = initUnits(img);

        float[] resFactor = initResFactor(imgResols, imgUnits);

        float imgZoom = initZoom(img, resFactor[0], resFactor[1], xScreen, yScreen);

        ViewJComponentEditImage imgComp = new ViewJComponentEditImage(frame, img, imgLUT, imageBuf, null, null, null,
                                                                      pixBuf, imgZoom, extents, img.getLogMagDisplay(),
                                                                      FileInfoBase.UNKNOWN_ORIENT );

        imgComp.setBuffers(imageBuf, null, pixBuf, null);

        if (imgResols[1] >= imgResols[0]) {
            imgComp.setResolutions(1, resFactor[0]);
        } else {
            imgComp.setResolutions(resFactor[1], 1);
        }

        // if this is a color image, then update the RGB info in the component
        if (img.isColorImage()) {

            if (imgComp.getRGBTA() == null) {
                int[] RGBExtents = new int[2];

                RGBExtents[0] = 4;
                RGBExtents[1] = 256;

                ModelRGB rgb = new ModelRGB(RGBExtents);

                imgComp.setRGBTA(rgb);
            }
        } // end if image is an RGB type

        return imgComp;
    }

    /**
     * Overridden because the component image initialization was moved into the building of the RFAST image table.
     * Creates and initializes the component image for the given image.
     *
     * @param       extents  the image dimensionality.
     *
     * @throws      OutOfMemoryError  if enough memory cannot be allocated for this method
     *
     * @deprecated  DOCUMENT ME!
     */
    protected synchronized void initComponentImage(int[] extents) throws OutOfMemoryError { }

    /**
     * Initialize the key's image controls. Override to change the image toolbars that are displayed.
     *
     * @param  key  the key indicating the image controls to initialize
     */
    protected synchronized void initControls(String key) {
        ViewControlsImage newControls = new ViewControlsImage(this);

        boolean showImage = menuBuilder.isMenuItemSelected("Image toolbar");
        boolean showVOI = menuBuilder.isMenuItemSelected("VOI toolbar");
        boolean showPaint = menuBuilder.isMenuItemSelected("Paint toolbar");
        boolean showScript = menuBuilder.isMenuItemSelected("Scripting toolbar");

        newControls.buildToolbar(showImage, null, showPaint, showScript);

        controlsTable.put(key, newControls);
    }

    /**
     * Initialize the key's scroll pane with its associated component image.
     *
     * @param  key  the key indicating the scroll pane to initialize
     */
    protected synchronized void initScrollPane(String key) {

        if (getScrollPane(key) == null) {
            JScrollPane newScrollPane = new JScrollPane(getComponentImage(key));
            newScrollPane.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
            newScrollPane.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED);
            newScrollPane.setBackground(Color.black);

            newScrollPane.setSize(getScrollPaneSize());

            scrollPaneTable.put(key, newScrollPane);
        } else {
            getScrollPane(key).setViewportView(getComponentImage(key));
        }
    }

    /**
     * Open an image file and initialize it for use in the frame.
     *
     * @param  type      the image type
     * @param  file      the path to the image file
     * @param  loadInto  whether this image is an imageA or an imageB
     */
    protected synchronized void loadImageFromFile(String type, String file, int loadInto) {
        ViewOpenFileUI openFile = new ViewOpenFileUI(false);

        openFile.setPutInFrame(false);

        String name = openFile.open(file, false, null);

        if (name == null) {
            MipavUtil.displayError("Unable to open image listed in project file: " + file);

            return;
        }

        ModelImage img = userInterface.getRegisteredImageByName(name);

        ModelLUT lut = openFile.getLUT();

        String oldKey = getCurrentKey();

        currentKey = type;
        initImage(type, loadInto, img, lut);

        currentKey = oldKey;

        if (loadInto == IMAGE_B) {
            setCurrentImageB((ModelImage) null);
            setCurrentLUTb((ModelLUT) null);
        } else {
            setCurrentImageA((ModelImage) null);
            setCurrentLUTa((ModelLUT) null);
        }

        setCurrentComponentImage((ViewJComponentEditImage) null);
        setCurrentScrollPane((JScrollPane) null);
        setCurrentControls((ViewControlsImage) null);
    }

    /**
     * Load the state for a key into the &quot;current&quot; variables.
     *
     * @param  key      the key to load the state of
     * @param  display  whether to initiate a repaint of the image once the loading is complete
     */
    protected synchronized void loadState(String key, boolean display) {
        currentKey = key;

        setCurrentImageA(key);
        setCurrentLUTa(key);
        setCurrentImageB(key);
        setCurrentLUTb(key);
        setCurrentComponentImage(key);
        setCurrentScrollPane(key);
        setCurrentControls(key);

        if (getComponentImage() != null) {
            initComponentImageVars(getComponentImage());
            setTitle();
        }

        if (display) {
            updateImages(true);
        }
    }

    //~ Inner Classes --------------------------------------------------------------------------------------------------

    /**
     * Information about the image states of the frame.
     *
     * @author   Evan McCreedy
     * @version  1.0
     */
    protected class StateInfo {

        /** The name of the step. */
        public String name;

        /** Whether the this step should have it's own copy of it. */
        public boolean uniqueImage;

        /**
         * Create a new step information object.
         *
         * @param  stepName  the name of the step
         * @param  unique    whether the step needs a copy of the original image all to itself
         */
        public StateInfo(String stepName, boolean unique) {
            name = stepName;
            uniqueImage = unique;
        }
    }

    /**
     * The list of wizard step information.
     *
     * @author   Evan McCreedy
     * @version  1.0
     */
    protected class StateList extends Vector {

        /** Use serialVersionUID for interoperability. */
        private static final long serialVersionUID = 7118784852919366358L;

        /**
         * Set up the wizard labels list.
         */
        public StateList() { }

        /**
         * Get the ith step's information.
         *
         * @param   i  the number of the step info to retrieve
         *
         * @return  the step information
         */
        public StateInfo getInfo(int i) {
            return (StateInfo) this.elementAt(i);
        }

        /**
         * Get a step's information.
         *
         * @param   name  the name of the step to get
         *
         * @return  the step information
         */
        public StateInfo getInfo(String name) {

            for (int i = 0; i < size(); i++) {
                StateInfo info = getInfo(i);

                if (info.name.equals(name)) {
                    return info;
                }
            }

            return null;
        }
    }
}
