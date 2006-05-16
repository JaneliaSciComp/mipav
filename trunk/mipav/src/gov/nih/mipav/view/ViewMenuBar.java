package gov.nih.mipav.view;


import gov.nih.mipav.model.structures.*;

import javax.swing.*;


/**
 * Build the menus for an image loaded into MIPAV.
 *
 * @see      ViewJFrameImage
 * @see      ViewMenuBuilder
 * @author   Evan McCreedy
 * @version  1.0 July 12, 2004
 */
public class ViewMenuBar {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Separator that can be added in between menu items. */
    protected static final JSeparator separator = new JSeparator();

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Menu and menu item generator. */
    protected ViewMenuBuilder menuBuilder;

    /** The menu listing installed plugins in the user's home directory. */
    protected JMenu plugInMenu;

    /** The user interface for MIPAV. */
    protected ViewUserInterface userInterface;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Set up the menu bar builder.
     *
     * @param  menuObj  the helper class used to make the menus and menu items
     */
    public ViewMenuBar(ViewMenuBuilder menuObj) {
        userInterface = ViewUserInterface.getReference();
        menuBuilder = menuObj;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Clean up memory used by this class.
     */
    public void finalize() {
        menuBuilder = null;
        userInterface = null;
    }

    /**
     * Creates general menus based on dimensionality and data type (byte, short, argb, etc) of image and attaches them
     * to a menu bar.
     *
     * @param   numberOfDimensions  Dimensionality of image (2D, 3D, 4D...)
     * @param   type                Data type from <code>ModelStorageBase</code>, byte, short, argb, etc.
     * @param   isDicomImage        Whether the image this menu bar is for is a dicom image.
     *
     * @return  Menu bar with the menus attached.
     */
    public JMenuBar getMenuBar(int numberOfDimensions, int type, boolean isDicomImage) {
        JMenuBar menuBar;

        try {
            menuBar = new JMenuBar();
            plugInMenu = userInterface.buildPlugInsMenu(userInterface.getActiveImageFrame());
            menuBar.add(makeFileMenu(true));
            menuBar.add(makeEditMenu());
            menuBar.add(makeVOIMenu());
            menuBar.add(makeLUTMenu());
            menuBar.add(makeAlgorithmsMenu());
            menuBar.add(makeUtilitiesMenu());
            menuBar.add(plugInMenu);
            menuBar.add(makeScriptingMenu());
            menuBar.add(makeImageMenu(isDicomImage));
            menuBar.add(makeToolbarsMenu());
            menuBar.add(makeHelpMenu());
        } catch (OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("Out of memory: unable to build menu.");
            throw error;
        }

        setEnabledMenuItems(menuBar, numberOfDimensions, type);
        menuBar.validate();

        return menuBar;
    }

    /**
     * Return the menu containing the list of installed plugins.
     *
     * @return  the plugin menu
     */
    public JMenu getPlugInMenu() {
        return this.plugInMenu;
    }

    /**
     * Sets the menu containing the list of installed plugins.
     *
     * @param  menu  the new plugin menu
     */
    public void setPlugInMenu(JMenu menu) {
        this.plugInMenu = menu;
    }

    /**
     * Construct the algorithms menu.
     *
     * @return  the algorithms menu
     */
    protected JMenu makeAlgorithmsMenu() {
        return menuBuilder.makeMenu("Algorithms", 'A', false,
                                    new JComponent[] {
                                        menuBuilder.buildMenuItem("Autocorrelation coefficients", "ACC", 0, null,
                                                                  false),
                                        menuBuilder.buildMenuItem("Autocovariance coefficients", "ACCOV", 0, null,
                                                                  false),
                                        menuBuilder.makeMenu("Brain tools", false,
                                                             new JMenuItem[] {
                                                                 menuBuilder.buildMenuItem("Face de-identification",
                                                                                           "Anonymize face (BET)", 0,
                                                                                           null, false),
                                                                 menuBuilder.buildMenuItem("Extract brain surface (BET)",
                                                                                           "extractBrainSurfaceBET", 0,
                                                                                           null, false),
                                                                 menuBuilder.buildMenuItem("Extract brain surface (BSE)",
                                                                                           "extractBrainSurfaceBSE", 0,
                                                                                           null, false),
                                                                 menuBuilder.buildMenuItem("Midsagittal line alignment",
                                                                                           "Midsagittal", 0, null,
                                                                                           false),
                                                             }),
                                        menuBuilder.makeMenu("Edge detection", false,
                                                             new JMenuItem[] {
                                                                 menuBuilder.buildMenuItem("Zero X laplacian", null, 0,
                                                                                           null, false),
                                                                 menuBuilder.buildMenuItem("Zero X non-maximum suppression",
                                                                                           "zxsuppression", 0, null,
                                                                                           false)
                                                             }),
                                        menuBuilder.buildMenuItem("Extract surface (marching cubes)",
                                                                  "extractSurfaceCubes", 0, null, false),
                                        menuBuilder.buildMenuItem("FFT", null, 0, null, false),
                                        menuBuilder.buildMenuItem("Filters (frequency)", null, 0, null, false),
                                        menuBuilder.buildMenuItem("Filters (Gabor)", "gFilter", 0, null, false),
                                        menuBuilder.buildMenuItem("Filters (homomorphic)", "hFilter", 0, null, false),
                                        menuBuilder.makeMenu("Filters (spatial)", false,
                                                             new JMenuItem[] {
                                                                 menuBuilder.buildMenuItem("Adaptive noise reduction",
                                                                                           "ANR", 0, null, false),
                                                                 menuBuilder.buildMenuItem("Adaptive path smooth",
                                                                                           "adaptivePathSmooth", 0,
                                                                                           null, false),

                                                                 // menuBuilder.buildMenuItem("Adaptive smooth",
                                                                 // "adaptiveSmooth", 0, null, false),
                                                                 menuBuilder.buildMenuItem("Anisotropic diffusion",
                                                                                           null, 0, null, false),
                                                                 menuBuilder.buildMenuItem("Boundary attenuation",
                                                                                           "BoundaryAttenuation", 0,
                                                                                           null, false),
                                                                 menuBuilder.buildMenuItem("Coherence-enhancing diffusion",
                                                                                           "CoherDiff", 0, null,
                                                                                           false),
                                                                 menuBuilder.buildMenuItem("Color edge", "ColorEdge", 0,
                                                                                           null, false),
                                                                 menuBuilder.buildMenuItem("Gaussian blur", null, 0,
                                                                                           null, false),
                                                                 menuBuilder.buildMenuItem("Gradient magnitude", null,
                                                                                           0, null, false),
                                                                 menuBuilder.buildMenuItem("Haralick texture",
                                                                                           "Haralick", 0, null, false),
                                                                 menuBuilder.buildMenuItem("Laplacian", null, 0, null,
                                                                                           false),
                                                                 menuBuilder.buildMenuItem("Local normalization", null,
                                                                                           0, null, false),
                                                                 menuBuilder.buildMenuItem("Mean", null, 0, null,
                                                                                           false),
                                                                 menuBuilder.buildMenuItem("Median", null, 0, null,
                                                                                           false),
                                                                 menuBuilder.buildMenuItem("Mode", null, 0, null,
                                                                                           false),
                                                                 menuBuilder.buildMenuItem("Nonlinear noise reduction",
                                                                                           "NLNR", 0, null, false),
                                                                 menuBuilder.buildMenuItem("Nonmaximum suppression",
                                                                                           "nmsuppression", 0, null,
                                                                                           false),
                                                                 menuBuilder.buildMenuItem("Regularized isotropic diffusion",
                                                                                           "RegIsoDiff", 0, null,
                                                                                           false),
                                                                 menuBuilder.buildMenuItem("Slice averaging",
                                                                                           "sliceAveraging", 0, null,
                                                                                           false),
                                                                 menuBuilder.buildMenuItem("Unsharp mask", null, 0,
                                                                                           null, false),
                                                             }),
                                        menuBuilder.makeMenu("Filters (wavelet)", false,
                                                             new JMenuItem[] { // menuBuilder.buildMenuItem("Maxima",
                                                                               // null,

                                                                 // null, null),
                                                                 menuBuilder.buildMenuItem("Thresholding",
                                                                                           "waveletThreshold", 0, null,
                                                                                           false),
                                                             }),
                                        menuBuilder.makeMenu("Histogram tools", false,
                                                             new JMenuItem[] {
                                                                 menuBuilder.buildMenuItem("2D Histogram", "histTwoDim",
                                                                                           0, null, false),
                                                                 menuBuilder.makeMenu("Histogram equalization", false,
                                                                                      new JMenuItem[] {
                                                                                          menuBuilder.buildMenuItem("Neighborhood adaptive",
                                                                                                                    "performNAHE",
                                                                                                                    0,
                                                                                                                    null,
                                                                                                                    false),
                                                                                          menuBuilder.buildMenuItem("Regional adaptive",
                                                                                                                    "performRAHE",
                                                                                                                    0,
                                                                                                                    null,
                                                                                                                    false)
                                                                                      }),
                                                                 menuBuilder.buildMenuItem("Histogram matching",
                                                                                           "histMatch", 0, null,
                                                                                           false),
                                                                 menuBuilder.buildMenuItem("Histogram summary",
                                                                                           "HistoSummary", 0, null,
                                                                                           false),
                                                             }),
                                        menuBuilder.makeMenu("Insight toolkit (ITK)", false,
                                                             new JMenuItem[] {
                                                                 menuBuilder.buildMenuItem("Gaussian blur",
                                                                                           "GaussianBlurITK", 0, null,
                                                                                           false)
                                                             }),
                                        menuBuilder.makeMenu("Microscopy", false,
                                                             new JMenuItem[] {
                                                                 menuBuilder.makeMenu("Colocalization", false,
                                                                                      new JMenuItem[] {

                                                                                          // menuBuilder.buildMenuItem("DENCLUE","colocDENCLUE",null,null),
                                                                                          menuBuilder.buildMenuItem("Expectation maximization",
                                                                                                                    "colocEM",
                                                                                                                    0,
                                                                                                                    null,
                                                                                                                    false),
                                                                                          menuBuilder.buildMenuItem("Orthogonal regression",
                                                                                                                    "colocRegression",
                                                                                                                    0,
                                                                                                                    null,
                                                                                                                    false)
                                                                                      }),
                                                                 menuBuilder.buildMenuItem("FRAP", "doFRAP", 0, null,
                                                                                           false),
                                                                 menuBuilder.buildMenuItem("FRET", "doFRET", 0, null,
                                                                                           false),
                                                                 menuBuilder.buildMenuItem("FRET bleed thru",
                                                                                           "doFRETBleed", 0, null,
                                                                                           false),
                                                                 menuBuilder.buildMenuItem("FRET efficiency",
                                                                                           "doFRETEfficiency", 0, null,
                                                                                           false),
                                                                 menuBuilder.buildMenuItem("Subtract VOI background",
                                                                                           "SubVOI", 0, null, false),
                                                                 menuBuilder.makeMenu("Restoration", false,
                                                                                      new JMenuItem[] {
                                                                                          menuBuilder.buildMenuItem("XCOSM_EM",
                                                                                                                    "xcosmEM",
                                                                                                                    0,
                                                                                                                    null,
                                                                                                                    false),

                                                                                          //
                                                                                          // menuBuilder.buildMenuItem("Iterative
                                                                                          // blind deconvolution",
                                                                                          // null,
                                                                                          // 0,
                                                                                          // null,
                                                                                          // false),
                                                                                          menuBuilder.buildMenuItem("Maximum Likelihood Iterative blind deconvolution",
                                                                                                                    null,
                                                                                                                    0,
                                                                                                                    null,
                                                                                                                    false)
                                                                                      }),
                                                             }),
                                        menuBuilder.makeMenu("Morphological", false,
                                                             new JMenuItem[] {
                                                                 menuBuilder.buildMenuItem("Bg. distance map", null, 0,
                                                                                           null, false),
                                                                 menuBuilder.buildMenuItem("Close", null, 0, null,
                                                                                           false),
                                                                 menuBuilder.buildMenuItem("Delete objects", null, 0,
                                                                                           null, false),
                                                                 menuBuilder.buildMenuItem("Dilate", null, 0, null,
                                                                                           false),
                                                                 menuBuilder.buildMenuItem("Distance map", null, 0,
                                                                                           null, false),
                                                                 menuBuilder.buildMenuItem("Erode", null, 0, null,
                                                                                           false),
                                                                 menuBuilder.buildMenuItem("Evaluate segmentation",
                                                                                           "evalSegMask", 0, null,
                                                                                           false),
                                                                 menuBuilder.buildMenuItem("Fill holes", null, 0, null,
                                                                                           false),
                                                                 menuBuilder.buildMenuItem("Find edges", null, 0, null,
                                                                                           false),
                                                                 menuBuilder.buildMenuItem("ID objects", null, 0, null,
                                                                                           false),
                                                                 menuBuilder.buildMenuItem("Morphological filter",
                                                                                           "morFilter", 0, null,
                                                                                           false),
                                                                 menuBuilder.buildMenuItem("Open", null, 0, null,
                                                                                           false),
                                                                 menuBuilder.buildMenuItem("Particle analysis", null, 0,
                                                                                           null, false),
                                                                 menuBuilder.buildMenuItem("Skeletonize3D pot field",
                                                                                           "Skeletonize3D", 0, null,
                                                                                           false),
                                                                 menuBuilder.buildMenuItem("Skeletonize3D Voronoi",
                                                                                           "SkelGeom3D", 0, null,
                                                                                           false),
                                                                 menuBuilder.buildMenuItem("Ultimate erode", null, 0,
                                                                                           null, false),
                                                             }),

                                        // menuBuilder.buildMenuItem("Plot surface", null, 0, null, false),
                                        // menuBuilder.buildMenuItem("Point area average intensities", "Point area", 0,
                                        // null, false),
                                        menuBuilder.buildMenuItem("Principal component", "Principal components", 0,
                                                                  null, false),
                                        menuBuilder.makeMenu("Registration", false,
                                                             new JMenuItem[] {

                                                                 // menuBuilder.buildMenuItem("AFNI - Shear",
                                                                 // "MRIShear", 0, null, false),
                                                                 // menuBuilder.buildMenuItem("AIR linear", null, 0,
                                                                 // null, false),
                                                                 // menuBuilder.buildMenuItem("Chamfer", null, 0, null,
                                                                 // false),
                                                                 // menuBuilder.buildMenuItem("AIR nonlinear", null, 0,
                                                                 // null, false),
                                                                 menuBuilder.buildMenuItem("Align patient position",
                                                                                           "Patient Position", 0, null,
                                                                                           false),
                                                                 menuBuilder.buildMenuItem("B-Spline automatic registration 2D/3D",
                                                                                           "BSplineReg", 0, null,
                                                                                           false),
                                                                 menuBuilder.buildMenuItem("B-Spline automatic registration 2.5D",
                                                                                           "BSplineReg25D", 0, null,
                                                                                           false),
                                                                 menuBuilder.buildMenuItem("Constrained optimized automatic registration",
                                                                                           "COAR", 0, null, false),
                                                                 menuBuilder.buildMenuItem("Display pixel similarity cost functions",
                                                                                           "COSTS", 0, null, false),
                                                                 menuBuilder.makeMenu("Landmark", false,
                                                                                      new JMenuItem[] {
                                                                                          menuBuilder.buildMenuItem("Least squares",
                                                                                                                    "LeastSquares",
                                                                                                                    0,
                                                                                                                    "reglsq.gif",
                                                                                                                    false),
                                                                                          menuBuilder.buildMenuItem("Thin plate spline",
                                                                                                                    "TPSpline",
                                                                                                                    0,
                                                                                                                    "regtsp.gif",
                                                                                                                    false),
                                                                                      }),
                                                                 menuBuilder.buildMenuItem("Manual 2D series", "Manual",
                                                                                           0, null, false),
                                                                 menuBuilder.buildMenuItem("Mosaic", "Mosaic", 0, null,
                                                                                           false),
                                                                 menuBuilder.buildMenuItem("Optimized automatic registration",
                                                                                           "OAR", 0, null, false),
                                                                 menuBuilder.buildMenuItem("Optimized automatic registration 2.5D",
                                                                                           "OAR25D", 0, null, false),
                                                                 menuBuilder.buildMenuItem("Optimized automatic registration 3.5D",
                                                                                           "OAR35D", 0, null, false),
                                                                 menuBuilder.buildMenuItem("Time series optimized automatic registration",
                                                                                           "TSOAR", 0, null, false),

                                                                 // menuBuilder.buildMenuItem("Turbo", "Turbo", 0,
                                                                 // null, false),
                                                                 menuBuilder.buildMenuItem("VOI landmark",
                                                                                           "VOILandmark", 0, null,
                                                                                           false),
                                                             }),
                                        menuBuilder.makeMenu("Segmentation", false,
                                                             new JMenuItem[] {
                                                                 menuBuilder.buildMenuItem("Evaluate VOI segmentation",
                                                                                           "evalSeg", 0, null, false),

                                                                 // menuBuilder.buildMenuItem("Extract object surface",
                                                                 // "extractObjectSurface", 0, null, false),
                                                                 menuBuilder.makeMenu("Fuzzy C-means", false,
                                                                                      new JMenuItem[] {
                                                                                          menuBuilder.buildMenuItem("Multispectral",
                                                                                                                    null,
                                                                                                                    0,
                                                                                                                    null,
                                                                                                                    false),
                                                                                          menuBuilder.buildMenuItem("Single channel",
                                                                                                                    null,
                                                                                                                    0,
                                                                                                                    null,
                                                                                                                    false),
                                                                                      }),
                                                                 menuBuilder.buildMenuItem("Graph based",
                                                                                           "graphBasedSeg", 0, null,
                                                                                           false),
                                                                 menuBuilder.buildMenuItem("Levelset", "Levelset", 0,
                                                                                           null, false),
                                                                 menuBuilder.buildMenuItem("Levelset diffusion",
                                                                                           "LevelsetDiffusion", 0, null,
                                                                                           false),
                                                                 menuBuilder.buildMenuItem("Threshold", null, 0, null,
                                                                                           false),
                                                                 menuBuilder.buildMenuItem("Watershed", null, 0, null,
                                                                                           false),
                                                             }),

                                        // menuBuilder.buildMenuItem("Non-parametric", "nonparametric", 0, null,
                                        // false)}),
                                        // menuBuilder.buildMenuItem("Stereo depth", "StereoDepth", 0, null, false),
                                        menuBuilder.makeMenu("Shading correction", false,
                                                             new JMenuItem[] {
                                                                 menuBuilder.buildMenuItem("Entropy minimization",
                                                                                           "entropyMin", 0, null,
                                                                                           false),
                                                                 menuBuilder.buildMenuItem("Inhomogeneity N3 correction",
                                                                                           "N3Correction", 0, null,
                                                                                           false),
                                                                 menuBuilder.buildMenuItem("MRI combined info",
                                                                                           "MRICorrection", 0, null,
                                                                                           false)
                                                             }),
                                        menuBuilder.makeMenu("Transformation tools", false,
                                                             new JMenuItem[] {
                                                                 menuBuilder.buildMenuItem("Reslice - isotropic voxels",
                                                                                           null, 0, null, false),
                                                                 menuBuilder.buildMenuItem("Subsample", "subsample", 0,
                                                                                           null, false),
                                                                 menuBuilder.buildMenuItem("Transform", null, 0, null,
                                                                                           false),
                                                                 menuBuilder.buildMenuItem("Transform nonlinear",
                                                                                           "TransformNL", 0, null,
                                                                                           false),
                                                                 menuBuilder.buildMenuItem("Transform to power of 2",
                                                                                           null, 0, null, false),
                                                             }),
                                    });
    }

    /**
     * Create the image capture/print File submenu.
     *
     * @param   isAnImageOpen  Whether MIPAV has an image open (determines whether the menu is enabled).
     *
     * @return  The new submenu
     *
     * @see     #makeFileMenu(boolean)
     */
    protected JMenu makeCaptureMenu(boolean isAnImageOpen) {
        JMenu captureMenu = menuBuilder.makeMenu("Capture/print image", true,
                                                 new JComponent[] {
                                                     menuBuilder.buildMenuItem("Capture and print image", "PrintImage",
                                                                               0, "printer.gif", true),
                                                     menuBuilder.buildMenuItem("Capture image region/slice to TIFF(RGB)",
                                                                               "CaptureTiff", 0, "camera.gif", true),
                                                     menuBuilder.buildMenuItem("Capture image slices to new frame",
                                                                               "CaptureTiffs", 0, "camera.gif", true),
                                                 });

        menuBuilder.setMenuItemEnabled("Capture/print image", isAnImageOpen);

        return captureMenu;
    }


    /**
     * Create the Dicom File submenu.
     *
     * @return  The new submenu
     *
     * @see     #makeFileMenu(boolean)
     */
    protected JMenu makeDicomMenu() {
        return menuBuilder.makeMenu("DICOM", true,
                                    new JComponent[] {
                                        menuBuilder.buildMenuItem("DICOM browser", "BrowseDICOM", 0, null, true),
                                        menuBuilder.buildMenuItem("Anonymize DICOM directory", "AnonymizeDirectory", 0,
                                                                  null, true),
                                        menuBuilder.buildMenuItem("DICOM database access", "QueryDatabase", 0,
                                                                  "database.gif", true),
                                        menuBuilder.buildCheckBoxMenuItem("Enable DICOM receiver", "Dicom",
                                                                          Preferences.is(Preferences.PREF_AUTOSTART_DICOM_RECEIVER)),
                                    });
    }

    /**
     * Construct the edit menu.
     *
     * @return  the edit menu
     */
    protected JMenu makeEditMenu() {
        return menuBuilder.makeMenu("Edit", 'E', false,
                                    new JComponent[] {
                                        menuBuilder.buildMenuItem("Undo VOI", "undoVOI", 0, "undopaint.gif", true),
                                        separator,
                                        menuBuilder.buildMenuItem("Cut VOI", "cutVOI", 0, "cutpaint.gif", true),
                                        menuBuilder.buildMenuItem("Copy VOI", "copyVOI", 0, "copypaint.gif", true),
                                        menuBuilder.buildMenuItem("Paste VOI", "pasteVOI", 0, "pastepaint.gif", true),
                                        separator,
                                        menuBuilder.buildMenuItem("Select all VOIs", "selectAllVOIs", 0, null, true)
                                        // menuBuilder.buildMenuItem("Delete VOI", "deleteVOI", null, "delete.gif"), separator,
                                        // menuBuilder.buildMenuItem("Edit image attributes", "EditImageInfo", null, null),
                                    });
    }

    /**
     * Construct the file menu.
     *
     * @param   isAnImageOpen  indicates whether an image has been opened in MIPAV yet (false if calling from
     *                         ViewUserInterface, true otherwise)
     *
     * @return  the file menu
     */
    protected JMenu makeFileMenu(boolean isAnImageOpen) {
        JMenu loadMenu = makeLoadBMenu(isAnImageOpen);

        JMenu captureMenu = makeCaptureMenu(isAnImageOpen);

        JMenu srbMenu = makeSRBMenu();

        JMenu dicomMenu = makeDicomMenu();

        JMenuItem closeImageBItem = menuBuilder.buildMenuItem("Close image(B)", "CloseImageB", 0, null, true);
        menuBuilder.setMenuItemEnabled("Close image(B)", isAnImageOpen);

        JMenuItem saveImageItem = menuBuilder.buildMenuItem("Save image", "SaveImage", 0, "save.gif", true);
        JMenuItem saveImageAsItem = menuBuilder.buildMenuItem("Save image as", "SaveImageAs", 0, "save.gif", true);
        menuBuilder.setMenuItemEnabled("Save image", isAnImageOpen);
        menuBuilder.setMenuItemEnabled("Save image as", isAnImageOpen);

        return menuBuilder.makeMenu("File", 'F', false,
                                    new JComponent[] {
                                        menuBuilder.buildMenuItem("Open image (A) from disk", "OpenNewImage", 0,
                                                                  "open.gif", true),
                                        menuBuilder.makeMenu("Open image (A)...", true,
                                                             new JMenuItem[] {
                                                                 menuBuilder.buildMenuItem("Leica series", "loadLeica",
                                                                                           0, "open.gif", true),
                                                                 menuBuilder.buildMenuItem("Image sequence",
                                                                                           "openImgSeq", 0, "open.gif",
                                                                                           true),
                                                                 menuBuilder.buildMenuItem("Create blank image",
                                                                                           "CreateBlankImage", 0,
                                                                                           "open.gif", true),
                                                                 menuBuilder.buildMenuItem("Image browser",
                                                                                           "BrowseImages", 0, null,
                                                                                           true),
                                                             }), separator, loadMenu, closeImageBItem, separator,
                                        saveImageItem, saveImageAsItem, captureMenu, separator, dicomMenu, separator,
                                        srbMenu, separator, menuBuilder.buildQuickList(), separator,
                                        menuBuilder.buildMenuItem("DCCIE image conversion", "dccieconvert", 0, null,
                                                                  true),

                                        // menuBuilder.buildMenuItem("Convert old XML", "convertXML", 0, null, true),
                                        separator, menuBuilder.buildMenuItem("Exit", "Exit", 'x', null, true)
                                    });
    }

    /**
     * Construct the help menu.
     *
     * @return  the help menu
     */
    protected JMenu makeHelpMenu() {
        return menuBuilder.makeMenu("Help", 'H', false,
                                    new JComponent[] {
                                        menuBuilder.buildMenuItem("About MIPAV", "About", 0, null, true),
                                        menuBuilder.buildMenuItem("JVM information", "AboutJava", 0, null, true),
                                        menuBuilder.buildMenuItem("MIPAV license", "License", 0, null, true), separator,
                                        menuBuilder.buildMenuItem("MIPAV help topics", "Help", 0, null, true),
                                        separator,
                                        menuBuilder.buildMenuItem("Memory usage", "MemoryUsage", 0, null, true),
                                        menuBuilder.buildMenuItem("Memory allocation", "MemoryAdjust", 0, null, true),
                                        menuBuilder.buildMenuItem("Image registry monitor", "ImageRegistryMonitor", 0,
                                                                  null, true), separator,
                                        menuBuilder.buildMenuItem("MIPAV options", "Options", 0, null, true),
                                        menuBuilder.buildMenuItem("Shortcut editor", "Shortcuts", 0, null, true),
                                    });
    }

    /**
     * Construct the image menu.
     *
     * @param   isDicomImage  Whether the image this menu is attached to is a dicom image.
     *
     * @return  the image menu
     */
    protected JMenu makeImageMenu(boolean isDicomImage) {

        // grabs the show overlay from prefs
        boolean showOverlay = false;

        if (isDicomImage) {
            showOverlay = Preferences.is(Preferences.PREF_SHOW_DICOM_OVERLAYS);
        } else {
            showOverlay = Preferences.is(Preferences.PREF_SHOW_IMAGE_OVERLAYS);
        }

        return menuBuilder.makeMenu("Image", 'I', false,
                                    new JComponent[] {
                                        menuBuilder.makeMenu("Views", true,
                                                             new JMenuItem[] {
                                                                 menuBuilder.buildMenuItem("Animate", "Animate", 0,
                                                                                           "movie.gif", true),
                                                                 menuBuilder.buildMenuItem("Cine (movie)",
                                                                                           "Cine (movie)", 0,
                                                                                           "movie.gif", true),
                                                                 menuBuilder.buildMenuItem("Light box", "Light box", 0,
                                                                                           "lightbox_16x16.gif", true),
                                                                 menuBuilder.buildMenuItem("Link to another image",
                                                                                           "LinkFrame", 0, null, true),
                                                                 menuBuilder.buildMenuItem("Surface plotter",
                                                                                           "Surface plotter", 0, null,
                                                                                           true),
                                                                 menuBuilder.buildMenuItem("Triplanar", "Tri-planar", 0,
                                                                                           "3plane_16x16.gif", true),
                                                                 menuBuilder.buildMenuItem("Volume renderers",
                                                                                           "VolTriplanar", 0,
                                                                                           "4plane_16x16.gif", true)
                                                             }),
                                        menuBuilder.makeMenu("Attributes", true,
                                                             new JMenuItem[] {
                                                                 menuBuilder.buildMenuItem("View header", "AboutImage",
                                                                                           0, null, false),
                                                                 menuBuilder.buildMenuItem("Edit attributes",
                                                                                           "EditImageInfo", 0, null,
                                                                                           false)
                                                             }),
                                        menuBuilder.makeMenu("Zoom", true,
                                                             new JMenuItem[] {
                                                                 menuBuilder.buildMenuItem("0.5X", "UnMagImage", 0,
                                                                                           "zoomout.gif", true),
                                                                 menuBuilder.buildMenuItem("1X", "ZoomOne", 0,
                                                                                           "zoom1.gif", true),
                                                                 menuBuilder.buildMenuItem("2X", "MagImage", 0,
                                                                                           "zoomin.gif", true),
                                                                 menuBuilder.buildMenuItem("Custom", "MagControls", 0,
                                                                                           null, true)
                                                             }),
                                        menuBuilder.buildMenuItem("Magnifying glass settings", "MagSettings", 0, null,
                                                                  true), separator,
                                        menuBuilder.buildMenuItem("Histogram - LUT", "DisplayLUT", 0, "histolut.gif",
                                                                  true), separator,
                                        menuBuilder.buildCheckBoxMenuItem("Show slice number overlay", "ShowSliceNum",
                                                                          true),
                                        menuBuilder.buildCheckBoxMenuItem("Show image/DICOM overlay", "ShowOverlay",
                                                                          showOverlay),
                                        menuBuilder.buildCheckBoxMenuItem("Show overlay grid", "ShowGrid", false),
                                        menuBuilder.buildMenuItem("Grid options", "GridOptions", 0, null, true),
                                        menuBuilder.buildMenuItem("DICOM overlay options", "DICOMOverlayOptions", 0,
                                                                  null, true),
                                        menuBuilder.buildMenuItem("Image overlay options", "ImageOverlayOptions", 0,
                                                                  null, true),
                                    });
    }

    /**
     * Create the Load image B File submenu.
     *
     * @param   isAnImageOpen  Whether MIPAV has an image open (determines whether the menu is enabled).
     *
     * @return  The new submenu
     *
     * @see     #makeFileMenu(boolean)
     */
    protected JMenu makeLoadBMenu(boolean isAnImageOpen) {
        JMenu loadMenu = menuBuilder.makeMenu("Load image (B)", true,
                                              new JMenuItem[] {
                                                  menuBuilder.buildMenuItem("From frame", "ComponentLoadB", 0,
                                                                            "frame.gif", true),
                                                  menuBuilder.buildMenuItem("From file", "LoadB", 0, "open.gif", true),
                                                  menuBuilder.buildMenuItem("Create blank image", "LoadBlankB", 0,
                                                                            "open.gif", true),
                                              });

        menuBuilder.setMenuItemEnabled("Load image (B)", isAnImageOpen);

        return loadMenu;
    }

    /**
     * Construct the LUT menu.
     *
     * @return  the LUT menu
     */
    protected JMenu makeLUTMenu() {
        return menuBuilder.makeMenu("LUT", 'L', false,
                                    new JComponent[] {
                                        menuBuilder.buildMenuItem("Open LUT", null, 0, "open.gif", true),
                                        menuBuilder.buildMenuItem("Open functions", null, 0, "open.gif", true),
                                        menuBuilder.buildMenuItem("Open LUT from...", null, 0, "open.gif", true),
                                        menuBuilder.buildMenuItem("Open functions from...", null, 0, "open.gif", true),
                                        separator, menuBuilder.buildMenuItem("Save LUT", null, 0, "save.gif", true),
                                        menuBuilder.buildMenuItem("Save functions", null, 0, "save.gif", true),
                                        menuBuilder.buildMenuItem("Save LUT as...", null, 0, "save.gif", true),
                                        menuBuilder.buildMenuItem("Save functions as...", null, 0, "save.gif", true),
                                        separator,
                                        menuBuilder.buildMenuItem("Histogram - LUT...", "DisplayLUT", 0, "histolut.gif",
                                                                  true),
                                    });
    }

    /**
     * Create the Scripting menu.
     *
     * @return  The new scripting menu
     */
    protected JMenu makeScriptingMenu() {
        return menuBuilder.makeMenu("Scripts", false,
                                    new JMenuItem[] {
                                        menuBuilder.buildMenuItem("Record script", "RecordScript", 0, null, false),
                                        menuBuilder.buildMenuItem("Run script", "RunScript", 0, null, false)
                                    });
    }

    /**
     * Create the srb File submenu.
     *
     * @return  The new submenu
     *
     * @see     #makeFileMenu(boolean)
     */
    protected JMenu makeSRBMenu() {
        boolean isAutoUploadEnabled = ((userInterface != null) && (userInterface.getNDARPipeline() != null));

        return menuBuilder.makeMenu("SRB-BIRN", true,
                                    new JMenuItem[] {
                                        menuBuilder.buildMenuItem("Open XCEDE schema", "OpenXCEDESchema", 0, "open.gif",
                                                                  true),
                                        menuBuilder.buildMenuItem("Open image from SRB", "OpenSRBFile", 0, null, true),
                                        menuBuilder.buildMenuItem("Save image to SRB", "SaveSRBFile", 0, null, true),
                                        menuBuilder.buildMenuItem("SRB transfer", "TransferSRBFiles", 0, null, true),
                                        menuBuilder.buildCheckBoxMenuItem("Enable auto SRB upload", "AutoUploadToSRB",
                                                                          isAutoUploadEnabled)
                                    });
    }

    /**
     * Construct the toolbar menu.
     *
     * @return  the toolbar menu
     */
    protected JMenu makeToolbarsMenu() {
        boolean showVOIToolbar = Preferences.is(Preferences.PREF_VOI_TOOLBAR_ON);
        boolean showPaintToolbar = Preferences.is(Preferences.PREF_PAINT_TOOLBAR_ON);
        boolean showScriptingToolbar = Preferences.is(Preferences.PREF_SCRIPTING_TOOLBAR_ON);
        boolean showImageToolbar = Preferences.is(Preferences.PREF_IMAGE_TOOLBAR_ON);

        // default the VOI and Image toolbars to on if the user hasn't explicitly turned them off
        if (!showVOIToolbar && !Preferences.isPreferenceSet(Preferences.PREF_VOI_TOOLBAR_ON)) {
            showVOIToolbar = true;
        }

        if (!showImageToolbar && !Preferences.isPreferenceSet(Preferences.PREF_IMAGE_TOOLBAR_ON)) {
            showImageToolbar = true;
        }

        return menuBuilder.makeMenu("Toolbars", 'T', false,
                                    new JMenuItem[] {
                                        menuBuilder.buildCheckBoxMenuItem("Image toolbar", "ImageToolbar",
                                                                          showImageToolbar),
                                        menuBuilder.buildCheckBoxMenuItem("Paint toolbar", "PaintToolbar",
                                                                          showPaintToolbar),
                                        menuBuilder.buildCheckBoxMenuItem("Scripting toolbar", "ScriptToolbar",
                                                                          showScriptingToolbar),
                                        menuBuilder.buildCheckBoxMenuItem("VOI toolbar", "VOIToolbar", showVOIToolbar)
                                    });
    }

    /**
     * Construct the utilities menu.
     *
     * @return  the utilities menu
     */
    protected JMenu makeUtilitiesMenu() {
        return menuBuilder.makeMenu("Utilities", 'U', false,
                                    new JMenuItem[] {
                                        menuBuilder.makeMenu("4D tools", false,
                                                             new JMenuItem[] {
                                                                 menuBuilder.buildMenuItem("Convert 3D to 4D",
                                                                                           "Convert3Dto4D", 0, null,
                                                                                           false),
                                                                 menuBuilder.buildMenuItem("Convert 4D to 3D",
                                                                                           "Convert4Dto3D", 0, null,
                                                                                           false),
                                                                 menuBuilder.buildMenuItem("Extract 3D subset from 4D",
                                                                                           "Subset", 0, null, false),
                                                                 menuBuilder.buildMenuItem("Remove time volumes", null,
                                                                                           0, null, false),
                                                                 menuBuilder.buildMenuItem("Swap dims 3<->4", "Swap34",
                                                                                           0, null, false),
                                                             }),
                                        menuBuilder.buildMenuItem("Add image margins", "Add margins", 0, null, false),
                                        menuBuilder.buildMenuItem("Clone (copy)", "Clone", 0, null, false),
                                        menuBuilder.makeMenu("Conversion tools", false,
                                                             new JMenuItem[] {
                                                                 menuBuilder.buildMenuItem("Convert type", null, 0,
                                                                                           null, false),
                                                                 menuBuilder.buildMenuItem("Grays -> RGB", null, 0,
                                                                                           null, false),
                                                                 menuBuilder.buildMenuItem("RGB -> Gray", null, 0, null,
                                                                                           false),
                                                                 menuBuilder.buildMenuItem("RGB -> Grays", null, 0,
                                                                                           null, false),
                                                                 menuBuilder.buildMenuItem("RGB -> HSB", null, 0, null,
                                                                                           false),
                                                             }),
                                        menuBuilder.buildMenuItem("Correct image spacing", "CorrectSpace", 0, null,
                                                                  false),
                                        menuBuilder.makeMenu("Crop", false,
                                                             new JMenuItem[] {
                                                                 menuBuilder.buildMenuItem("Using parameters",
                                                                                           "CropParam", 0, null,
                                                                                           false),
                                                                 menuBuilder.buildMenuItem("Using VOI", "Crop", 0, null,
                                                                                           false)
                                                             }),

                                        // menuBuilder.buildMenuItem("Dicom order", "DicomOrder", 0, null, false),
                                        menuBuilder.makeMenu("Flip", false,
                                                             new JMenuItem[] {
                                                                 menuBuilder.buildMenuItem("Horizontal", "FlipY", 0,
                                                                                           "fliphoriz.gif", true),
                                                                 menuBuilder.buildMenuItem("Vertical", "FlipX", 0,
                                                                                           "flipvert.gif", true)
                                                             }),
                                        menuBuilder.buildMenuItem("Image calculator", "Calculator", 0, null, false),
                                        menuBuilder.buildMenuItem("Image math", null, 0, null, false),
                                        menuBuilder.buildMenuItem("Invert", null, 0, null, false),
                                        menuBuilder.buildMenuItem("Mask", null, 0, null, false),
                                        menuBuilder.buildMenuItem("Match images", "matchImages", 0, null, false),
                                        menuBuilder.buildMenuItem("Noise", null, 0, null, false),
                                        menuBuilder.buildMenuItem("Quantify using mask", "Quantify", 0, null, false),
                                        menuBuilder.buildMenuItem("Replace pixel/voxel value", "ReplaceValue", 0, null,
                                                                  false),
                                        menuBuilder.makeMenu("Rotate", false,
                                                             new JMenuItem[] {
                                                                 menuBuilder.buildMenuItem("X axis 180", "RotateX180",
                                                                                           0, null, false),
                                                                 menuBuilder.buildMenuItem("X axis +90", "RotateXPlus",
                                                                                           0, null, false),
                                                                 menuBuilder.buildMenuItem("X axis -90", "RotateXMinus",
                                                                                           0, null, false),
                                                                 menuBuilder.buildMenuItem("Y axis 180", "RotateY180",
                                                                                           0, null, false),
                                                                 menuBuilder.buildMenuItem("Y axis +90", "RotateYPlus",
                                                                                           0, null, false),
                                                                 menuBuilder.buildMenuItem("Y axis -90", "RotateYMinus",
                                                                                           0, null, false),
                                                                 menuBuilder.buildMenuItem("Z axis 180", "RotateZ180",
                                                                                           0, null, false),
                                                                 menuBuilder.buildMenuItem("Z axis +90", "RotateZPlus",
                                                                                           0, null, false),
                                                                 menuBuilder.buildMenuItem("Z axis -90", "RotateZMinus",
                                                                                           0, null, false)
                                                             }),
                                        menuBuilder.makeMenu("Slice tools", false,
                                                             new JMenuItem[] {
                                                                 menuBuilder.buildMenuItem("Concatenate", "Concat", 0,
                                                                                           null, false),
                                                                 menuBuilder.buildMenuItem("Extract slices / volumes",
                                                                                           null, 0, null, false),
                                                                 menuBuilder.buildMenuItem("Insert slice", null, 0,
                                                                                           null, false),
                                                                 menuBuilder.buildMenuItem("Inverse slice order",
                                                                                           "InverseOrder", 0, null,
                                                                                           false),
                                                                 menuBuilder.buildMenuItem("Pad with slices", "padding",
                                                                                           0, null, false),
                                                                 menuBuilder.buildMenuItem("Randomize slice order",
                                                                                           "RandOrder", 0, null,
                                                                                           false),
                                                                 menuBuilder.buildMenuItem("Remove slices", null, 0,
                                                                                           null, false),
                                                                 menuBuilder.buildMenuItem("Replace slice", null, 0,
                                                                                           null, false),
                                                             }),
                                        menuBuilder.buildMenuItem("Subtract VOI background", "SubVOI", 0, null, false),
                                    });
    }

    /**
     * Construct the VOI menu.
     *
     * @return  the VOI menu
     */
    protected JMenu makeVOIMenu() {
        return menuBuilder.makeMenu("VOI", 'V', false,
                                    new JComponent[] {
                                        menuBuilder.makeMenu("New VOI", true,
                                                             new JMenuItem[] {
                                                                 menuBuilder.buildMenuItem("Point", "Point", 0,
                                                                                           "pointROI.gif", true),
                                                                 menuBuilder.buildMenuItem("Line", "Line", 0,
                                                                                           "linear.gif", true),
                                                                 menuBuilder.buildMenuItem("Rectangle", "RectVOI", 0,
                                                                                           "rect.gif", true),
                                                                 menuBuilder.buildMenuItem("Ellipse", "EllipseVOI", 0,
                                                                                           "circle.gif", true),
                                                                 menuBuilder.buildMenuItem("Polygon", "PolygonVOI", 0,
                                                                                           "polygon.gif", true),
                                                                 menuBuilder.buildMenuItem("Level set", "LevelSetVOI",
                                                                                           0, "contour.gif", true),
                                                                 menuBuilder.buildMenuItem("3D rectangle", "Rect3DVOI",
                                                                                           0, "cube.gif", true)
                                                             }), separator,
                                        menuBuilder.makeMenu("Open VOI", true,
                                                             new JMenuItem[] {
                                                                 menuBuilder.buildMenuItem("Open VOI", "Open VOI", 0,
                                                                                           "open.gif", true),
                                                                 menuBuilder.buildMenuItem("Open all VOIs",
                                                                                           "Open all VOIs", 0,
                                                                                           "open.gif", true),
                                                                 menuBuilder.buildMenuItem("Open all VOIs from...",
                                                                                           "Open all VOIs from...", 0,
                                                                                           "open.gif", true)
                                                             }), separator,
                                        menuBuilder.makeMenu("Save VOI", true,
                                                             new JMenuItem[] {
                                                                 menuBuilder.buildMenuItem("Save selected contours",
                                                                                           "SaveSelectedContours", 0,
                                                                                           "save.gif", true),
                                                                 menuBuilder.buildMenuItem("Save selected contours as",
                                                                                           "SaveSelectedContoursAs", 0,
                                                                                           "save.gif", true),
                                                                 menuBuilder.buildMenuItem("Save VOI", "Save VOI", 0,
                                                                                           "save.gif", true),
                                                                 menuBuilder.buildMenuItem("Save VOI as", "Save VOI as",
                                                                                           0, "save.gif", true),
                                                                 menuBuilder.buildMenuItem("Save all VOIs",
                                                                                           "Save all VOIs", 0,
                                                                                           "save.gif", true),
                                                                 menuBuilder.buildMenuItem("Save all VOIs to...",
                                                                                           "Save all VOIs to...", 0,
                                                                                           "save.gif", true)
                                                             }), separator,
                                        menuBuilder.buildMenuItem("Open VOI intensity graph", "OpenNewGraph", 0,
                                                                  "open.gif", true), separator,
                                        menuBuilder.buildMenuItem("Group VOIs", "GroupVOIs", 0, null, true),
                                        menuBuilder.makeMenu("VOI order", true,
                                                             new JMenuItem[] {
                                                                 menuBuilder.buildMenuItem("Bring VOI to front",
                                                                                           "BringToFront", 0,
                                                                                           "front.gif", true),
                                                                 menuBuilder.buildMenuItem("Send VOI to back",
                                                                                           "SendToBack", 0, "back.gif",
                                                                                           true),
                                                                 menuBuilder.buildMenuItem("Bring VOI forward",
                                                                                           "BringForward", 0,
                                                                                           "forward.gif", true),
                                                                 menuBuilder.buildMenuItem("Send VOI backward",
                                                                                           "SendBackward", 0,
                                                                                           "backward.gif", true)
                                                             }),
                                        menuBuilder.makeMenu("Contour order", true,
                                                             new JMenuItem[] {
                                                                 menuBuilder.buildMenuItem("Bring contour to front",
                                                                                           "BringContourToFront", 0,
                                                                                           "front.gif", true),
                                                                 menuBuilder.buildMenuItem("Send contour to back",
                                                                                           "SendContourToBack", 0,
                                                                                           "back.gif", true),
                                                                 menuBuilder.buildMenuItem("Bring contour forward",
                                                                                           "BringContourForward", 0,
                                                                                           "forward.gif", true),
                                                                 menuBuilder.buildMenuItem("Send contour backward",
                                                                                           "SendContourBackward", 0,
                                                                                           "backward.gif", true)
                                                             }),
                                        menuBuilder.makeMenu("Propagate", true,
                                                             new JMenuItem[] {
                                                                 menuBuilder.buildMenuItem("To next slice", "PropVOIUp",
                                                                                           0, "voipropu.gif", true),
                                                                 menuBuilder.buildMenuItem("To previous slice",
                                                                                           "PropVOIDown", 0,
                                                                                           "voipropd.gif", true),
                                                                 menuBuilder.buildMenuItem("To all slices",
                                                                                           "PropVOIAll", 0,
                                                                                           "voipropall.gif", true)
                                                             }),
                                        menuBuilder.buildMenuItem("Ungroup VOIs", "UngroupVOIs", 0, null, true),
                                        separator,
                                        menuBuilder.makeMenu("VOI -> Mask", true,
                                                             new JMenuItem[] {
                                                                 menuBuilder.buildMenuItem("VOI(s) to binary mask",
                                                                                           "BinaryMask", 0, null,
                                                                                           false),
                                                                 menuBuilder.buildMenuItem("VOI(s) to short mask",
                                                                                           "ShortMask", 0, null,
                                                                                           false),
                                                                 menuBuilder.buildMenuItem("VOI(s) to paint mask",
                                                                                           "PaintMask", 0, null,
                                                                                           false),
                                                                 menuBuilder.buildMenuItem("VOI(s) to ubyte mask",
                                                                                           "UnsignedByteMask", 0, null,
                                                                                           false)
                                                             }),

                                        // separator,
                                        menuBuilder.buildMenuItem("Mask -> VOI", "MaskToVOI", 0, null, true),
                                        menuBuilder.buildMenuItem("Mask -> Paint", "MaskToPaint", 0, null, true),
                                        menuBuilder.buildMenuItem("Paint -> VOI", "PaintToVOI", 0, null, true),
                                        menuBuilder.buildMenuItem("Paint -> Short mask", "PaintToShortMask", 0, null,
                                                                  true),
                                        menuBuilder.buildCheckBoxMenuItem("Allow VOI holes (XOR)", "XOR",
                                                                          Preferences.is(Preferences.PREF_USE_VOI_XOR)),
                                        separator, menuBuilder.buildMenuItem("Smooth VOI", "SmoothVOI", 0, null, true),
                                        separator, menuBuilder.buildMenuItem("Trim parameter", "Trim", 0, null, true),
                                        separator,
                                        menuBuilder.makeMenu("Evolve boundary 2D", true,
                                                             new JMenuItem[] {
                                                                 menuBuilder.buildMenuItem("Active contour", "Snake", 0,
                                                                                           null, true),
                                                                 menuBuilder.buildMenuItem("Active GVF", "AGVF", 0,
                                                                                           null, true),
                                                                 menuBuilder.buildMenuItem("Spline active contour",
                                                                                           "BSnake", 0, null, true),
                                                                 menuBuilder.buildMenuItem("Spline GVF", "GVF", 0, null,
                                                                                           true)
                                                             }), separator,
                                        menuBuilder.buildMenuItem("Cardiology VOI", "Cardio", 0, null, true), separator,
                                        menuBuilder.makeMenu("Graph", true,
                                                             new JMenuItem[] {
                                                                 menuBuilder.buildMenuItem("Boundary intensity",
                                                                                           "boundaryIntensity", 0, null,
                                                                                           true),
                                                                 menuBuilder.buildMenuItem("2.5D total intensity",
                                                                                           "totalIntensity", 0, null,
                                                                                           true),
                                                                 menuBuilder.buildMenuItem("2.5D average intensity",
                                                                                           "avgIntensity", 0, null,
                                                                                           true)
                                                             }),

                                        // menuBuilder.buildMenuItem("Graph", "GraphVOI", null, null),
                                        menuBuilder.buildMenuItem("Properties...", "VOIProperties", 'P', null, true),
                                        menuBuilder.buildMenuItem("Statistics generator...", "VOIStatistics", 'G', null,
                                                                  true),
                                    });
    }

    /**
     * Enable or disable items in a menubar based on the image dimensionality and storage type.
     *
     * @param  menu                the menubar with elements to enable / disable
     * @param  numberOfDimensions  Dimensionality of image (2D, 3D, 4D...)
     * @param  type                Data type from <code>ModelStorageBase</code>, byte, short, argb, etc.
     */
    protected void setEnabledMenuItems(JMenuBar menu, int numberOfDimensions, int type) {

        // if (!InsightToolkitSupport.isLibraryPresent()) {
        // menuBuilder.setMenuItemEnabled("Insight toolkit (ITK)", false);
        // }
        menuBuilder.setMenuItemEnabled("Close image(B)", false);

        if (numberOfDimensions == 4) {
            menuBuilder.setMenuItemEnabled("Edge detection", false);
            menuBuilder.setMenuItemEnabled("Evaluate segmentation", false); // vois
            menuBuilder.setMenuItemEnabled("Entropy minimization", false);
            menuBuilder.setMenuItemEnabled("Evolve surface", false);
            menuBuilder.setMenuItemEnabled("Extract surface (marching cubes)", false);
            menuBuilder.setMenuItemEnabled("FFT", false);
            menuBuilder.setMenuItemEnabled("Filters (wavelet)", false);
            menuBuilder.setMenuItemEnabled("Anisotropic diffusion", false);
            menuBuilder.setMenuItemEnabled("Boundary attenuation", false);
            menuBuilder.setMenuItemEnabled("Laplacian", false);
            menuBuilder.setMenuItemEnabled("Mean", false);
            menuBuilder.setMenuItemEnabled("Median", false);
            menuBuilder.setMenuItemEnabled("Mode", false);
            menuBuilder.setMenuItemEnabled("Zero X non-maximum suppression", false);
            menuBuilder.setMenuItemEnabled("Slice averaging", false);
            menuBuilder.setMenuItemEnabled("Unsharp mask", false);
            menuBuilder.setMenuItemEnabled("Filters (frequency)", false);
            menuBuilder.setMenuItemEnabled("FRAP", false);
            menuBuilder.setMenuItemEnabled("FRET", false);
            menuBuilder.setMenuItemEnabled("FRET bleed thru", false);
            menuBuilder.setMenuItemEnabled("FRET efficiency", false);
            menuBuilder.setMenuItemEnabled("Graph based", false);
            menuBuilder.setMenuItemEnabled("Multispectral", false);
            menuBuilder.setMenuItemEnabled("Image calculator", false);
            menuBuilder.setMenuItemEnabled("Image math", false);
            menuBuilder.setMenuItemEnabled("Inhomogeneity N3 correction", false);
            menuBuilder.setMenuItemEnabled("Levelset", false);
            menuBuilder.setMenuItemEnabled("Levelset diffusion", false);
            menuBuilder.setMenuItemEnabled("Mask", false);
            menuBuilder.setMenuItemEnabled("Morphological", false);
            menuBuilder.setMenuItemEnabled("MRI combined info", false);
            menuBuilder.setMenuItemEnabled("Noise", false);
            menuBuilder.setMenuItemEnabled("Adaptive noise reduction", false);
            menuBuilder.setMenuItemEnabled("Adaptive path smooth", false);
            menuBuilder.setMenuItemEnabled("Nonlinear noise reduction", false);
            menuBuilder.setMenuItemEnabled("Point area average intensities", false);
            menuBuilder.setMenuItemEnabled("Principal component", false);
            menuBuilder.setMenuItemEnabled("Quantify using mask", false);
            menuBuilder.setMenuItemEnabled("Shading correction", false);
            menuBuilder.setMenuItemEnabled("Skeletonize 3D", false);

            // menuBuilder.setMenuItemEnabled("AFNI - Shear", false);
            menuBuilder.setMenuItemEnabled("Optimized automatic registration", false);
            menuBuilder.setMenuItemEnabled("Constrained optimized automatic registration", false);
            menuBuilder.setMenuItemEnabled("Optimized automatic registration 2.5D", false);
            menuBuilder.setMenuItemEnabled("B-Spline automatic registration 2D/3D", false);
            menuBuilder.setMenuItemEnabled("B-Spline automatic registration 2.5D", false);
            menuBuilder.setMenuItemEnabled("Least squares", false);
            menuBuilder.setMenuItemEnabled("Thin plate spline", false);
            menuBuilder.setMenuItemEnabled("Manual 2D series", false);
            menuBuilder.setMenuItemEnabled("Align patient position", false);
            menuBuilder.setMenuItemEnabled("VOI landmark", false);
            menuBuilder.setMenuItemEnabled("Reslice - isotropic voxels", false);
            menuBuilder.setMenuItemEnabled("Subtract VOI background", false);
            menuBuilder.setMenuItemEnabled("Single channel", false);
            menuBuilder.setMenuItemEnabled("Watershed", false);
            menuBuilder.setMenuItemEnabled("Clone (copy)", false);
            menuBuilder.setMenuItemEnabled("Histogram summary", false);
            menuBuilder.setMenuItemEnabled("RGB -> Gray", false);
            menuBuilder.setMenuItemEnabled("RGB -> Grays", false);
            menuBuilder.setMenuItemEnabled("RGB -> HSB", false);
            menuBuilder.setMenuItemEnabled("Surface plotter", false);
            menuBuilder.setMenuItemEnabled("Evolve boundary 2D", false);
            menuBuilder.setMenuItemEnabled("Convert 3D to 4D", false);
            menuBuilder.setMenuItemEnabled("Capture images to new frame", false);
            menuBuilder.setMenuItemEnabled("Haralick texture", false);
            menuBuilder.setMenuItemEnabled("Display pixel similarity cost functions", false);
            menuBuilder.setMenuItemEnabled("Pad with slices", false);
        } else if (numberOfDimensions == 3) {
            menuBuilder.setMenuItemEnabled("Adaptive noise reduction", false);
            menuBuilder.setMenuItemEnabled("Convert 4D to 3D", false);
            menuBuilder.setMenuItemEnabled("Extract 3D subset from 4D", false);
            menuBuilder.setMenuItemEnabled("Graph based", false);
            menuBuilder.setMenuItemEnabled("Optimized automatic registration 3.5D", false);
            menuBuilder.setMenuItemEnabled("Remove time volumes", false);
            menuBuilder.setMenuItemEnabled("Swap dims 3<->4", false);
            menuBuilder.setMenuItemEnabled("Time series optimized automatic registration", false);
            menuBuilder.setMenuItemEnabled("Haralick texture", false);
        } else if (numberOfDimensions == 2) {
            menuBuilder.setMenuItemEnabled("3D rectangle", false);
            menuBuilder.setMenuItemEnabled("Animate", false);
            menuBuilder.setMenuItemEnabled("Anonymize face", false);
            menuBuilder.setMenuItemEnabled("Cine (movie)", false);
            menuBuilder.setMenuItemEnabled("Convert 4D to 3D", false);
            menuBuilder.setMenuItemEnabled("Evolve surface", false);
            menuBuilder.setMenuItemEnabled("Extract slices / volumes", false);
            menuBuilder.setMenuItemEnabled("Replace slice", false);
            menuBuilder.setMenuItemEnabled("Extract 3D subset from 4D", false);
            menuBuilder.setMenuItemEnabled("Extract surface (marching cubes)", false);
            menuBuilder.setMenuItemEnabled("Extract brain", false);
            menuBuilder.setMenuItemEnabled("FRAP", false);
            menuBuilder.setMenuItemEnabled("Inverse slice order", false);
            menuBuilder.setMenuItemEnabled("Light box", false);
            menuBuilder.setMenuItemEnabled("Propagate", false);
            menuBuilder.setMenuItemEnabled("Triplanar", false);
            menuBuilder.setMenuItemEnabled("Volume renderers", false);
            menuBuilder.setMenuItemEnabled("Insert slice", false);
            menuBuilder.setMenuItemEnabled("Remove slices", false);
            menuBuilder.setMenuItemEnabled("Pad with slices", false);
            menuBuilder.setMenuItemEnabled("Remove time volumes", false);
            menuBuilder.setMenuItemEnabled("2.5D total intensity", false);
            menuBuilder.setMenuItemEnabled("2.5D average intensity", false);
            menuBuilder.setMenuItemEnabled("Manual 2D series", true);
            menuBuilder.setMenuItemEnabled("Optimized automatic registration 2.5D", false);
            menuBuilder.setMenuItemEnabled("Optimized automatic registration 3.5D", false);
            menuBuilder.setMenuItemEnabled("B-Spline automatic registration 2.5D", false);
            menuBuilder.setMenuItemEnabled("Time series optimized automatic registration", false);
            menuBuilder.setMenuItemEnabled("X axis 180", false);
            menuBuilder.setMenuItemEnabled("X axis +90", false);
            menuBuilder.setMenuItemEnabled("X axis -90", false);
            menuBuilder.setMenuItemEnabled("Y axis 180", false);
            menuBuilder.setMenuItemEnabled("Y axis +90", false);
            menuBuilder.setMenuItemEnabled("Y axis -90", false);
            menuBuilder.setMenuItemEnabled("Randomize slice order", false);
            menuBuilder.setMenuItemEnabled("Swap dims 3<->4", false);
            menuBuilder.setMenuItemEnabled("Convert 3D to 4D", false);
            menuBuilder.setMenuItemEnabled("Reslice - isotropic voxels", false);
            menuBuilder.setMenuItemEnabled("Slice averaging", false);
            menuBuilder.setMenuItemEnabled("Link to another image", false);
            menuBuilder.setMenuItemEnabled("Point area average intensities", false);
            menuBuilder.setMenuItemEnabled("Correct image spacing", false);
            menuBuilder.setMenuItemEnabled("Capture images to new frame", false);
            menuBuilder.setMenuItemSelected("ShowSliceNum", false);
            menuBuilder.setMenuItemEnabled("Show slice number overlay", false);
            menuBuilder.setMenuItemEnabled("Skeletonize 3D", false);
        }

        if (ModelImage.isColorImage(type)) {
            menuBuilder.setMenuItemEnabled("Anisotropic diffusion", false);
            menuBuilder.setMenuItemEnabled("Boundary attenuation", false);
            menuBuilder.setMenuItemEnabled("Edge detection", false);
            menuBuilder.setMenuItemEnabled("Evolve surface", false);
            menuBuilder.setMenuItemEnabled("Extract brain", false);
            menuBuilder.setMenuItemEnabled("FFT", false);
            menuBuilder.setMenuItemEnabled("Filters (frequency)", false);
            menuBuilder.setMenuItemEnabled("Filters (Gabor)", false);
            menuBuilder.setMenuItemEnabled("Filters (homomorphic)", false);
            menuBuilder.setMenuItemEnabled("Filters (wavelet)", false);
            menuBuilder.setMenuItemEnabled("Grays -> RGB", false);
            menuBuilder.setMenuItemEnabled("Image math", false);
            menuBuilder.setMenuItemEnabled("Inhomogeneity N3 correction", false);
            menuBuilder.setMenuItemEnabled("Laplacian", false);
            menuBuilder.setMenuItemEnabled("Levelset", false);
            menuBuilder.setMenuItemEnabled("Levelset diffusion", false);
            menuBuilder.setMenuItemEnabled("Local normalization", false);
            menuBuilder.setMenuItemEnabled("Morphological", false);
            menuBuilder.setMenuItemEnabled("MRI combined info", false);
            menuBuilder.setMenuItemEnabled("Noise", false);
            menuBuilder.setMenuItemEnabled("Nonlinear noise reduction", false);
            menuBuilder.setMenuItemEnabled("Nonmaximum suppression", false);
            menuBuilder.setMenuItemEnabled("Zero X non-maximum suppression", false);
            menuBuilder.setMenuItemEnabled("Quantify using mask", false);
            menuBuilder.setMenuItemEnabled("Single channel", false);
            menuBuilder.setMenuItemEnabled("Unsharp mask", false);
            menuBuilder.setMenuItemEnabled("Watershed", false);
            menuBuilder.setMenuItemEnabled("VOI landmark", false);
            menuBuilder.setMenuItemEnabled("Evolve boundary 2D", false);
            menuBuilder.setMenuItemEnabled("Correct image spacing", false);
            menuBuilder.setMenuItemEnabled("Surface plotter", false);
            menuBuilder.setMenuItemEnabled("Haralick texture", false);
            menuBuilder.setMenuItemEnabled("Display pixel similarity cost functions", false);
            menuBuilder.setMenuItemEnabled("Skeletonize 3D", false);
        } else {
            menuBuilder.setMenuItemEnabled("RGB -> Gray", false);
            menuBuilder.setMenuItemEnabled("RGB -> Grays", false);
            menuBuilder.setMenuItemEnabled("RGB -> HSB", false);
            menuBuilder.setMenuItemEnabled("Color edge", false);

            if (numberOfDimensions == 2) {
                menuBuilder.setMenuItemEnabled("Principal component", false);
            }
        }

        menuBuilder.setMenuItemEnabled("DICOM database access",
                                       Preferences.is(Preferences.PREF_AUTOSTART_DICOM_RECEIVER));
    }
}
