package gov.nih.mipav.view;


import gov.nih.mipav.model.structures.ModelImage;

import java.awt.event.ActionListener;
import java.util.Vector;

import javax.swing.*;


/**
 * Build the menus for an image loaded into MIPAV.
 * 
 * @see ViewJFrameImage
 * @see ViewMenuBuilder
 * @author Evan McCreedy
 * @version 1.0 July 12, 2004
 */
public class ViewMenuBar {

    // ~ Static fields/initializers
    // -------------------------------------------------------------------------------------

    /** Separator that can be added in between menu items. */
    protected static final JSeparator separator = new JSeparator();

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** Menu and menu item generator. */
    protected ViewMenuBuilder menuBuilder;

    /** The menu listing installed plugins in the user's home directory. */
    protected JMenu plugInMenu;

    /** The user interface for MIPAV. */
    protected ViewUserInterface userInterface;

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * Set up the menu bar builder.
     * 
     * @param menuObj the helper class used to make the menus and menu items
     */
    public ViewMenuBar(final ViewMenuBuilder menuObj) {
        userInterface = ViewUserInterface.getReference();
        menuBuilder = menuObj;
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

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
     * @param listener The class that wants to listen to plugin menu actions.
     * @param numberOfDimensions Dimensionality of image (2D, 3D, 4D...)
     * @param type Data type from <code>ModelStorageBase</code>, byte, short, argb, etc.
     * @param isDicomImage Whether the image this menu bar is for is a dicom image.
     * 
     * @return Menu bar with the menus attached.
     */
    public JMenuBar getMenuBar(final ActionListener listener, final int numberOfDimensions, final int type,
            final boolean isDicomImage) {
        JMenuBar menuBar;

        try {
            menuBar = new JMenuBar();
            plugInMenu = userInterface.buildPlugInsMenu(listener);
            menuBar.add(makeFileMenu(true));
            menuBar.add(makeVOIMenu());
            menuBar.add(makeLUTMenu());
            menuBar.add(makeAlgorithmsMenu());
            menuBar.add(makeUtilitiesMenu());
            menuBar.add(makeSystemsAnalysisMenu());
            menuBar.add(plugInMenu);
            menuBar.add(makeScriptingMenu());
            menuBar.add(makeImageMenu(isDicomImage));
            menuBar.add(makeToolbarsMenu());
            menuBar.add(makeHelpMenu());
        } catch (final OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("Out of memory: unable to build menu.");
            throw error;
        }

        setEnabledMenuItems(numberOfDimensions, type);
        menuBar.validate();

        return menuBar;
    }

    /**
     * Return the menu containing the list of installed plugins.
     * 
     * @return the plugin menu
     */
    public JMenu getPlugInMenu() {
        return this.plugInMenu;
    }

    /**
     * Sets the menu containing the list of installed plugins.
     * 
     * @param menu the new plugin menu
     */
    public void setPlugInMenu(final JMenu menu) {
        this.plugInMenu = menu;
    }

    /**
     * Construct the algorithms menu.
     * 
     * @return the algorithms menu
     */
    public JMenu makeAlgorithmsMenu() {
        return menuBuilder.makeMenu("Algorithms", 'A', false, new JComponent[] {
                menuBuilder.buildMenuItem("Autocorrelation coefficients", "ACC", 0, null, false),
                menuBuilder.buildMenuItem("Autocovariance coefficients", "ACCOV", 0, null, false),
                menuBuilder.makeMenu("Brain tools", false, new JMenuItem[] {
                        menuBuilder.buildMenuItem("Face de-identification", "Anonymize face (BET)", 0, null, false),
                        menuBuilder.buildMenuItem("Extract brain surface (BET)", "extractBrainSurfaceBET", 0, null,
                                false),
                        menuBuilder.buildMenuItem("Extract brain surface (BSE)", "extractBrainSurfaceBSE", 0, null,
                                false),
                        menuBuilder.buildMenuItem("Midsagittal line alignment", "Midsagittal", 0, null, false),
                        menuBuilder.buildMenuItem("Talairach transform", "talairachTransform", 0, null, false),}),
                menuBuilder.buildMenuItem("Density based clustering", "DENCLUE", 0, null, false),
                menuBuilder.makeMenu("Edge detection", false, new JMenuItem[] {
                        menuBuilder.buildMenuItem("Zero X laplacian", null, 0, null, false),
                        menuBuilder.buildMenuItem("Zero X non-maximum suppression", "zxsuppression", 0, null, false)}),
                menuBuilder.buildMenuItem("Extract surface (marching cubes)", "extractSurfaceCubes", 0, null, false),
                menuBuilder.buildMenuItem("FFT", null, 0, null, false),
                menuBuilder.buildMenuItem("Filters (frequency)", null, 0, null, false),
                menuBuilder.buildMenuItem("Filters (Gabor)", "gFilter", 0, null, false),
                menuBuilder.buildMenuItem("Filters (homomorphic)", "hFilter", 0, null, false),
                menuBuilder.makeMenu("Filters (spatial)", false, new JMenuItem[] {
                        menuBuilder.buildMenuItem("Adaptive noise reduction", "ANR", 0, null, false),
                        menuBuilder.buildMenuItem("Adaptive path smooth", "adaptivePathSmooth", 0, null, false),

                        // menuBuilder.buildMenuItem("Adaptive smooth",
                        // "adaptiveSmooth", 0, null, false),
                        menuBuilder.buildMenuItem("Anisotropic diffusion", null, 0, null, false),
                        menuBuilder.buildMenuItem("Bilateral filter", null, 0, null, false),
                        menuBuilder.buildMenuItem("Boundary attenuation", "BoundaryAttenuation", 0, null, false),
                        menuBuilder.buildMenuItem("Coherence-enhancing diffusion", "CoherDiff", 0, null, false),
                        menuBuilder.buildMenuItem("Color edge", "ColorEdge", 0, null, false),
                        menuBuilder.buildMenuItem("Gaussian blur", null, 0, null, false),
                        menuBuilder.buildMenuItem("Gradient magnitude", null, 0, null, false),
                        menuBuilder.buildMenuItem("Haralick texture", "Haralick", 0, null, false),
                        menuBuilder.buildMenuItem("Kernel regression", "kernelRegression", 0, null, false),
                        menuBuilder.buildMenuItem("Laplacian", null, 0, null, false),
                        menuBuilder.buildMenuItem("Local normalization", null, 0, null, false),
                        menuBuilder.buildMenuItem("Mean", null, 0, null, false),
                        menuBuilder.buildMenuItem("Median", null, 0, null, false),
                        menuBuilder.buildMenuItem("Mode", null, 0, null, false),
                        menuBuilder.buildMenuItem("Nonlinear noise reduction", "NLNR", 0, null, false),
                        menuBuilder.buildMenuItem("Nonlocal means filter", "nlMeansFilter", 0, null, false),
                        menuBuilder.buildMenuItem("Nonmaximum suppression", "nmsuppression", 0, null, false),
                        menuBuilder.buildMenuItem("Regularized isotropic diffusion", "RegIsoDiff", 0, null, false),
                        menuBuilder.buildMenuItem("Slice averaging", "sliceAveraging", 0, null, false),
                        menuBuilder.buildMenuItem("Trilateral filter", null, 0, null, false),
                        menuBuilder.buildMenuItem("Unsharp mask", null, 0, null, false),}),
                menuBuilder.makeMenu("Filters (wavelet)", false, new JMenuItem[] { // menuBuilder.buildMenuItem("Maxima",
                                // null, null, null),

                                menuBuilder.buildMenuItem("Denoising BLS GSM", "blsGSM", 0, null, false),
                                menuBuilder.buildMenuItem("Thresholding", "waveletThreshold", 0, null, false),}),
                menuBuilder.makeMenu("Histogram tools", false, new JMenuItem[] {
                        menuBuilder.buildMenuItem("2D histogram", "histTwoDim", 0, null, false),
                        menuBuilder.buildMenuItem("Cumulative histogram", "cumHistogram", 0, null, false),
                        menuBuilder.makeMenu("Histogram equalization", false, new JMenuItem[] {
                                menuBuilder.buildMenuItem("Neighborhood adaptive", "performNAHE", 0, null, false),
                                menuBuilder.buildMenuItem("Regional adaptive", "performRAHE", 0, null, false)}),
                        menuBuilder.buildMenuItem("Histogram image matching", "histImageMatch", 0, null, false),
                        menuBuilder.buildMenuItem("Histogram slice matching", "histSliceMatch", 0, null, false),
                        menuBuilder.buildMenuItem("Histogram summary", "HistoSummary", 0, null, false),}),
                menuBuilder.makeMenu("Hough transform", false, new JMenuItem[] {
                        menuBuilder.buildMenuItem("Circle detection", "HoughCircle", 0, null, false),
                        menuBuilder.buildMenuItem("Create R-table", "Rtable", 0, null, false),
                        menuBuilder.buildMenuItem("Ellipse detection", "HoughEllipse", 0, null, false),
                        menuBuilder.buildMenuItem("Find R-table object", "FindRObject", 0, null, false),
                        menuBuilder.buildMenuItem("Line filling", "HoughLine", 0, null, false),
                        menuBuilder.buildMenuItem("Parabola detection", "HoughParabola", 0, null, false)}),
                menuBuilder.makeMenu("Insight toolkit (auto ITK)", false, new JMenuItem[] {}),
                menuBuilder.makeMenu("Microscopy", false,
                        new JMenuItem[] {
                                menuBuilder.makeMenu("Colocalization", false, new JMenuItem[] {

                                        // menuBuilder.buildMenuItem("DENCLUE","colocDENCLUE",null,null),
                                        menuBuilder
                                                .buildMenuItem("Expectation maximization", "colocEM", 0, null, false),
                                        menuBuilder.buildMenuItem("Orthogonal regression", "colocRegression", 0, null,
                                                false)}),
                                menuBuilder.buildMenuItem("FRAP", "doFRAP", 0, null, false),
                                menuBuilder.buildMenuItem("FRET", "doFRET", 0, null, false),
                                menuBuilder.buildMenuItem("FRET bleed thru", "doFRETBleed", 0, null, false),
                                menuBuilder.buildMenuItem("FRET efficiency", "doFRETEfficiency", 0, null, false),
                                menuBuilder.buildMenuItem("Subtract VOI background", "SubVOI", 0, null, false),
                                menuBuilder.makeMenu("Restoration", false, new JMenuItem[] {
                                        menuBuilder.buildMenuItem("XCOSM_EM", "xcosmEM", 0, null, false),

                                        //
                                        // menuBuilder.buildMenuItem("Iterative
                                        // blind deconvolution",
                                        // null,
                                        // 0,
                                        // null,
                                        // false),
                                        menuBuilder.buildMenuItem("Maximum likelihood iterative blind deconvolution",
                                                null, 0, null, false)}),}),
                menuBuilder.makeMenu("Morphological", false, new JMenuItem[] {

                        menuBuilder.buildMenuItem("Close", null, 0, null, false),
                        menuBuilder.buildMenuItem("Delete objects", null, 0, null, false),
                        menuBuilder.buildMenuItem("Dilate", null, 0, null, false),
                        menuBuilder.makeMenu("Distance Maps", false, new JMenuItem[] {
                                menuBuilder.buildMenuItem("BG distance map", null, 0, null, false),
                                menuBuilder.buildMenuItem("FG distance map", null, 0, null, false),
                                menuBuilder.buildMenuItem("BG + FG distance map", null, 0, null, false)}),
                        menuBuilder.buildMenuItem("Bottom hat", "bottomHat", 0, null, false),
                        menuBuilder.buildMenuItem("Erode", null, 0, null, false),
                        menuBuilder.buildMenuItem("Evaluate segmentation", "evalSegMask", 0, null, false),
                        menuBuilder.buildMenuItem("Fill holes", null, 0, null, false),
                        menuBuilder.buildMenuItem("Find edges", null, 0, null, false),
                        menuBuilder.buildMenuItem("ID objects", null, 0, null, false),
                        menuBuilder.buildMenuItem("Morphological filter", "morFilter", 0, null, false),
                        menuBuilder.buildMenuItem("Morphological gradient", "morGradient", 0, null, false),
                        menuBuilder.buildMenuItem("Morphological laplacian", "morLaplacian", 0, null, false),
                        menuBuilder.buildMenuItem("Open", null, 0, null, false),
                        menuBuilder.buildMenuItem("Particle analysis", null, 0, null, false),
                        menuBuilder.buildMenuItem("Skeletonize", null, 0, null, false),
                        menuBuilder.buildMenuItem("Skeletonize3D pot field", "Skeletonize3D", 0, null, false),
                        menuBuilder.buildMenuItem("Top hat", "topHat", 0, null, false),
                        menuBuilder.buildMenuItem("Ultimate erode", null, 0, null, false),}),

                // menuBuilder.buildMenuItem("Plot surface", null, 0, null, false),
                // menuBuilder.buildMenuItem("Point area average intensities", "Point area", 0,
                // null, false),
                menuBuilder.buildMenuItem("Principal component", "Principal components", 0, null, false),
                menuBuilder.makeMenu("Registration", false, new JMenuItem[] {

                        // menuBuilder.buildMenuItem("AFNI - shear",
                        // "MRIShear", 0, null, false),
                        // menuBuilder.buildMenuItem("AIR linear", null, 0,
                        // null, false),
                        // menuBuilder.buildMenuItem("Chamfer", null, 0, null,
                        // false),
                        // menuBuilder.buildMenuItem("AIR nonlinear", null, 0,
                        // null, false),
                        menuBuilder.buildMenuItem("Align patient position", "Patient Position", 0, null, false),
                        menuBuilder
                                .buildMenuItem("B-Spline automatic registration 2D/3D", "BSplineReg", 0, null, false),
                        menuBuilder.buildMenuItem("B-Spline automatic registration 2.5D", "BSplineReg25D", 0, null,
                                false),
                        menuBuilder.buildMenuItem("Constrained optimized automatic registration", "COAR", 0, null,
                                false),
                        menuBuilder.buildMenuItem("Diffeomorphic demons", "diffDemons", 0, null, false),
                        menuBuilder.buildMenuItem("Display pixel similarity cost functions", "COSTS", 0, null, false),
                        menuBuilder.makeMenu("Landmark", false, new JMenuItem[] {
                                menuBuilder.buildMenuItem("Least squares", "LeastSquares", 0, "reglsq.gif", false),
                                menuBuilder.buildMenuItem("Thin plate spline", "TPSpline", 0, "regtsp.gif", false),}),
                        menuBuilder.buildMenuItem("Manual 2D series", "Manual", 0, null, false),
                        menuBuilder.buildMenuItem("Mosaic", "Mosaic", 0, null, false),
                        menuBuilder.buildMenuItem("Optimized automatic registration", "OAR", 0, null, false),
                        menuBuilder.buildMenuItem("Optimized automatic registration 2.5D", "OAR25D", 0, null, false),
                        // Doesn't seem to work at the moment - should handle wider search range than TSOAR.
                        // menuBuilder.buildMenuItem("Optimized automatic registration 3.5D", "OAR35D", 0, null, false),
                        menuBuilder.buildMenuItem("Time series optimized automatic registration", "TSOAR", 0, null,
                                false),
                        menuBuilder.buildMenuItem("Registration validation", "RegValidation", 0, null, false),

                        // menuBuilder.buildMenuItem("Turbo", "Turbo", 0,
                        // null, false),
                        menuBuilder.buildMenuItem("VOI landmark", "VOILandmark", 0, null, false),}),
                menuBuilder.makeMenu("Segmentation", false, new JMenuItem[] {
                        menuBuilder.buildMenuItem("Evaluate VOI segmentation", "evalSeg", 0, null, false),

                        // menuBuilder.buildMenuItem("Extract object surface",
                        // "extractObjectSurface", 0, null, false),
                        menuBuilder.makeMenu("Fuzzy C-means", false, new JMenuItem[] {
                                menuBuilder.buildMenuItem("Multispectral", null, 0, null, false),
                                menuBuilder.buildMenuItem("Single channel", null, 0, null, false),}),
                        menuBuilder.buildMenuItem("Graph based", "graphBasedSeg", 0, null, false),
                        menuBuilder.buildMenuItem("Levelset", "Levelset", 0, null, false),
                        menuBuilder.buildMenuItem("Levelset diffusion", "LevelsetDiffusion", 0, null, false),

                        menuBuilder.makeMenu("Threshold", false, new JMenuItem[] {
                                menuBuilder.buildMenuItem("Threshold using min/max", "threshMinMax", 0, null, false),
                                menuBuilder.buildMenuItem("Theshold using standard deviation", "threshStdDev", 0, null,
                                        false),}),
                        menuBuilder.buildMenuItem("Watershed", null, 0, null, false),
                        menuBuilder.makeMenu("ITK", false, new JMenuItem[] {
                                menuBuilder.buildMenuItem("Diffusion levelset 2D", "DiffusionLevelsetITK2", 0, null,
                                        false),
                                menuBuilder.buildMenuItem("Diffusion levelset 3D", "DiffusionLevelsetITK3", 0, null,
                                        false),}),}),

                // menuBuilder.buildMenuItem("Non-parametric", "nonparametric", 0, null,
                // false)}),
                // menuBuilder.buildMenuItem("Stereo depth", "StereoDepth", 0, null, false),
                menuBuilder.makeMenu("Shading correction", false, new JMenuItem[] {
                        // Of 3 shading correction methods only entropy minimization works on color
                        menuBuilder.buildMenuItem("Entropy minimization", "entropyMin", 0, null, false),
                        menuBuilder.buildMenuItem("Inhomogeneity N3 correction", "N3Correction", 0, null, false)}),
                // menuBuilder.buildMenuItem("MRI combined info", "MRICorrection", 0, null, false)}),
                menuBuilder.makeMenu("SNR", false, new JMenuItem[] {
                        menuBuilder.buildMenuItem("Single MRI image", "SMRISNR", 0, null, false),
                        menuBuilder.buildMenuItem("Two MRI images", "DMRISNR", 0, null, false),}),
                menuBuilder.makeMenu("Spatial statistics", false, new JMenuItem[] {
                        menuBuilder.buildMenuItem("Circle pattern generation", "CircleGen", 0, null, false),
                        menuBuilder.buildMenuItem("Sphere pattern generation", "SphereGen", 0, null, false),
                        menuBuilder.buildMenuItem("Two class segregation/association generation", "TwoClass", 0, null,
                                false),
                        menuBuilder.buildMenuItem("Three class segregation/association generation", "ThreeClass", 0,
                                null, false)}),
                menuBuilder.makeMenu("Transformation tools", false, new JMenuItem[] {
                        menuBuilder.buildMenuItem("Barrel distortion correction", "Barrel", 0, null, false),
                        menuBuilder.buildMenuItem("Circle to rectangle", "CircleToRec", 0, null, false),
                        menuBuilder.buildMenuItem("Circular sector to rectangle", "CirToRec", 0, null, false),
                        menuBuilder.buildMenuItem("Ellipse to circle", "EllipseToCircle", 0, null, false),
                        menuBuilder.buildMenuItem("Nearly circle to circle", "NearlyCircleToCircle", 0, null, false),
                        menuBuilder.buildMenuItem("Reslice - isotropic voxels", null, 0, null, false),
                        menuBuilder.buildMenuItem("Subsample", "subsample", 0, null, false),
                        menuBuilder.buildMenuItem("Transform", null, 0, null, false),
                        menuBuilder.buildMenuItem("Transform nonlinear", "TransformNL", 0, null, false),
                        menuBuilder.buildMenuItem("Transform to power of 2", null, 0, null, false),}),});
    }

    /**
     * Create the image capture/print File submenu.
     * 
     * @param isAnImageOpen Whether MIPAV has an image open (determines whether the menu is enabled).
     * 
     * @return The new submenu
     * 
     * @see #makeFileMenu(boolean)
     */
    public JMenu makeCaptureMenu(final boolean isAnImageOpen) {
        final JMenu captureMenu = menuBuilder
                .makeMenu("Capture/print image", true, new JComponent[] {
                        menuBuilder.buildMenuItem("Capture and print image", "PrintImage", 0, "printer.gif", true),
                        menuBuilder.buildMenuItem("Capture image region/slice to TIFF(RGB)", "CaptureTiff", 0,
                                "camera.gif", true),
                        menuBuilder.buildMenuItem("Capture image slices to new frame", "CaptureTiffs", 0, "camera.gif",
                                true),});

        menuBuilder.setMenuItemEnabled("Capture/print image", isAnImageOpen);

        return captureMenu;
    }

    /**
     * Creates a custom menu with a given Vector of UIParams
     * 
     * @param customMenuName the name of the menu
     * @param paramVector a vector of UIParams objects (can be lifted from CustomUIBuilder
     * @return the custom menu
     */
    public JMenu makeCustomMenu(final String customMenuName, final Vector<CustomUIBuilder.UIParams> paramVector) {
        boolean iconPadding = false;
        final JComponent[] comps = new JComponent[paramVector.size()];
        for (int i = 0; i < comps.length; i++) {
            if (paramVector.elementAt(i).getIconBase() != null) {
                iconPadding = true;
            }
            comps[i] = menuBuilder.buildMenuItem(paramVector.elementAt(i), iconPadding);
        }

        final JMenu customMenu = menuBuilder.makeMenu(customMenuName, false, comps);

        return customMenu;
    }

    /**
     * Create the Dicom File submenu.
     * 
     * @return The new submenu
     * 
     * @see #makeFileMenu(boolean)
     */
    public JMenu makeDicomMenu() {
        return menuBuilder.makeMenu("DICOM", true, new JComponent[] {
                menuBuilder.buildMenuItem("DICOM browser", "BrowseDICOM", 0, null, true),
                menuBuilder.buildMenuItem("DICOMDIR browser", "BrowseDICOMDIR", 0, null, true),
                menuBuilder.buildMenuItem("Anonymize DICOM directory", "AnonymizeDirectory", 0, null, true),
                menuBuilder.buildMenuItem("DICOM database access", "QueryDatabase", 0, "database.gif", true),
                menuBuilder.buildCheckBoxMenuItem("Activate DICOM receiver", "Dicom", Preferences
                        .is(Preferences.PREF_AUTOSTART_DICOM_RECEIVER)),});
    }

    /**
     * Create the Diffusion Tensor File submenu.
     * 
     * @return The new submenu
     * 
     * @see #makeFileMenu(boolean)
     */
    public JMenu makeDiffusionTensorMenu() {
        return menuBuilder.makeMenu("Diffusion tensor imaging", true, new JComponent[] {
                menuBuilder.buildMenuItem("Create list file", "createListFile", 0, "open.gif", true),
                menuBuilder.buildMenuItem("Open DTI framework", "loadDTIFrame", 0, "open.gif", true),
                menuBuilder.buildMenuItem("Open diffusion weighted images", "loadDWI", 0, "open.gif", true),
                menuBuilder.buildMenuItem("Open diffusion tensor image", "loadDTI", 0, "open.gif", true),
                menuBuilder.buildMenuItem("Open eigenvector and functional analysis images", "loadEG_FA", 0,
                        "open.gif", true)});
    }

    /**
     * Construct the file menu.
     * 
     * @param isAnImageOpen indicates whether an image has been opened in MIPAV yet (false if calling from
     *            ViewUserInterface, true otherwise)
     * 
     * @return the file menu
     */
    public JMenu makeFileMenu(final boolean isAnImageOpen) {
        final JMenu loadMenu = makeLoadBMenu(isAnImageOpen);

        final JMenu captureMenu = makeCaptureMenu(isAnImageOpen);

        final JMenu dicomMenu = makeDicomMenu();

        // final JMenu dtiMenu = makeDiffusionTensorMenu();

        final JMenuItem closeImageBItem = menuBuilder.buildMenuItem("Close image(B)", "CloseImageB", 0, null, true);
        menuBuilder.setMenuItemEnabled("Close image(B)", false);

        final JMenuItem extractImageBItem = menuBuilder.buildMenuItem("Extract image(B)", "ExtractImageB", 0, null,
                true);
        menuBuilder.setMenuItemEnabled("Extract image(B)", false);

        final JMenuItem saveImageItem = menuBuilder.buildMenuItem("Save image", "SaveImage", 0, "save.gif", true);
        final JMenuItem saveImageAsItem = menuBuilder
                .buildMenuItem("Save image as", "SaveImageAs", 0, "save.gif", true);
        menuBuilder.setMenuItemEnabled("Save image", isAnImageOpen);
        menuBuilder.setMenuItemEnabled("Save image as", isAnImageOpen);

        return menuBuilder.makeMenu("File", 'F', false, new JComponent[] {
                menuBuilder.buildMenuItem("Open image (A) from disk", "OpenNewImage", 0, "open.gif", true),
                menuBuilder.makeMenu("Open image (A)...", true, new JMenuItem[] {
                        menuBuilder.buildMenuItem("Leica series", "loadLeica", 0, "open.gif", true),
                        menuBuilder.buildMenuItem("Image sequence", "openImgSeq", 0, "open.gif", true),
                        menuBuilder.buildMenuItem("Create blank image", "CreateBlankImage", 0, "open.gif", true),
                        menuBuilder.buildMenuItem("Image browser", "BrowseImages", 0, null, true),}),
                ViewMenuBar.separator, loadMenu, extractImageBItem, closeImageBItem, ViewMenuBar.separator,
                saveImageItem, saveImageAsItem, captureMenu, ViewMenuBar.separator, dicomMenu, ViewMenuBar.separator,
                menuBuilder.buildQuickList(), ViewMenuBar.separator,
                menuBuilder.buildMenuItem("DCCIE image conversion", "dccieconvert", 0, null, true),

                ViewMenuBar.// menuBuilder.buildMenuItem("Convert old XML", "convertXML", 0, null, true),
                separator, menuBuilder.buildMenuItem("Exit", "Exit", 'x', null, true)});
    }

    /**
     * Construct the help menu.
     * 
     * @return the help menu
     */
    public JMenu makeHelpMenu() {
        return menuBuilder.makeMenu("Help", 'H', false, new JComponent[] {
                menuBuilder.buildMenuItem("About MIPAV", "About", 0, null, false),
                menuBuilder.buildMenuItem("JVM information", "AboutJava", 0, null, false),
                menuBuilder.buildMenuItem("MIPAV license", "License", 0, null, false), ViewMenuBar.separator,
                menuBuilder.buildMenuItem("MIPAV help topics", "Help", 0, null, false), ViewMenuBar.separator,
                menuBuilder.buildMenuItem("Memory usage", "MemoryUsage", 0, null, false),
                menuBuilder.buildMenuItem("Memory allocation", "MemoryAdjust", 0, null, false),
                menuBuilder.buildMenuItem("Image registry monitor", "ImageRegistryMonitor", 0, null, false),
                menuBuilder.buildMenuItem("MIPAV data provenance", "DataProvenance", 0, null, false),
                ViewMenuBar.separator, menuBuilder.buildMenuItem("MIPAV options", "Options", 0, null, false),
                menuBuilder.buildMenuItem("Shortcut editor", Preferences.PREF_SHORTCUTS, 0, null, false),});
    }

    public JMenu makeSystemsAnalysisMenu() {
        return menuBuilder.makeMenu("Systems analysis", 'S', false, new JComponent[] {
                menuBuilder.makeMenu("DTI", false, new JMenuItem[] {
                        menuBuilder.buildMenuItem("Estimate tensor", "estimateTensor", 0, null, false),
                        menuBuilder.buildMenuItem("Fiber tracking / Statistics", "fiberTracking", 0, null, false),
                        menuBuilder.buildMenuItem("Visualization", "dtiVisualization", 0, null, false)}),
                menuBuilder.buildMenuItem("Log slope mapping", "LogSlope", 0, null, false),
                menuBuilder.makeMenu("Quantitative MRI", false, new JMenuItem[] {
                        menuBuilder.makeMenu("T1", false, new JMenuItem[] {menuBuilder.buildMenuItem("DespotT1",
                                "despotT1", 0, null, false)}),
                        menuBuilder.makeMenu("T2", false, new JMenuItem[] {menuBuilder.buildMenuItem("DespotT2",
                                "despotT2", 0, null, false)}),
                        menuBuilder.makeMenu("DCE", false, new JMenuItem[] {
                                menuBuilder.buildMenuItem("T2M2", "t2m2", 0, null, false),
                                menuBuilder.buildMenuItem("SM2", "sm2", 0, null, false)})})});
    }

    /**
     * Construct the image menu.
     * 
     * @param isDicomImage Whether the image this menu is attached to is a dicom image.
     * 
     * @return the image menu
     */
    public JMenu makeImageMenu(final boolean isDicomImage) {

        // grabs the show overlay from prefs
        boolean showOverlay = false;

        if (isDicomImage) {
            showOverlay = Preferences.is(Preferences.PREF_SHOW_DICOM_OVERLAYS);
        } else {
            showOverlay = Preferences.is(Preferences.PREF_SHOW_IMAGE_OVERLAYS);
        }

        return menuBuilder.makeMenu("Image", 'I', false, new JComponent[] {
                menuBuilder.makeMenu("Views", true, new JMenuItem[] {
                        menuBuilder.buildMenuItem("Animate", "Animate", 0, "movie.gif", true),
                        menuBuilder.buildMenuItem("Cine (movie)", "Cine (movie)", 0, "movie.gif", true),
                        menuBuilder.buildMenuItem("Light box", "Light box", 0, "lightbox_16x16.gif", true),
                        menuBuilder.buildMenuItem("Link to another image", "LinkFrame", 0, null, true),
                        menuBuilder.buildMenuItem("Surface plotter", "Surface plotter", 0, null, true),
                        menuBuilder.buildMenuItem("Triplanar", "Tri-planar", 0, "3plane_16x16.gif", true),
                        menuBuilder.buildMenuItem("Volume renderers", "VolTriplanar", 0, "4plane_16x16.gif", true)}),
                menuBuilder.makeMenu("Attributes", true, new JMenuItem[] {
                        menuBuilder.buildMenuItem("View header", "AboutImage", 0, "header.gif", false),
                        menuBuilder.buildMenuItem("Edit attributes", "EditImageInfo", 0, "attributes.gif", false),
                        menuBuilder.buildMenuItem("Data provenance", "ImageDataProvenance", 0, null, true)}),
                menuBuilder.makeMenu("Zoom", true, new JMenuItem[] {
                        menuBuilder.buildMenuItem("0.5X", "UnMagImage", 0, "zoomout.gif", true),
                        menuBuilder.buildMenuItem("1X", "ZoomOne", 0, "zoom1.gif", true),
                        menuBuilder.buildMenuItem("2X", "MagImage", 0, "zoomin.gif", true),
                        menuBuilder.buildMenuItem("Custom", "MagControls", 0, null, true)}),
                menuBuilder.buildMenuItem("Magnifying glass settings", "MagSettings", 0, null, true),
                menuBuilder.buildMenuItem("Lightbox generator", "LightboxGenerator", 0, null, true),
                ViewMenuBar.separator,
                menuBuilder.buildMenuItem("Histogram - LUT", "DisplayLUT", 0, "histolut.gif", true),
                ViewMenuBar.separator,
                menuBuilder.buildCheckBoxMenuItem("Show slice number overlay", "ShowSliceNum", true),
                menuBuilder.buildCheckBoxMenuItem("Show image/DICOM overlay", "ShowOverlay", showOverlay),
                menuBuilder.buildCheckBoxMenuItem("Show overlay grid", "ShowGrid", false),
                menuBuilder.buildMenuItem("Grid overlay options", "GridOptions", 0, null, true),
                menuBuilder.buildMenuItem("DICOM overlay options", "DICOMOverlayOptions", 0, null, true),
                menuBuilder.buildMenuItem("Image overlay options", "ImageOverlayOptions", 0, null, true),});
    }

    /**
     * Create the Load image B File submenu.
     * 
     * @param isAnImageOpen Whether MIPAV has an image open (determines whether the menu is enabled).
     * 
     * @return The new submenu
     * 
     * @see #makeFileMenu(boolean)
     */
    public JMenu makeLoadBMenu(final boolean isAnImageOpen) {
        final JMenu loadMenu = menuBuilder.makeMenu("Load image (B)", true, new JMenuItem[] {
                menuBuilder.buildMenuItem("From frame", "ComponentLoadB", 0, "frame.gif", true),
                menuBuilder.buildMenuItem("From file", "LoadB", 0, "open.gif", true),
                menuBuilder.buildMenuItem("Create blank image", "LoadBlankB", 0, "open.gif", true),});

        menuBuilder.setMenuItemEnabled("Load image (B)", isAnImageOpen);

        return loadMenu;
    }

    /**
     * Construct the LUT menu.
     * 
     * @return the LUT menu
     */
    public JMenu makeLUTMenu() {
        return menuBuilder.makeMenu("LUT", 'L', false, new JComponent[] {
                menuBuilder.buildMenuItem("Open LUT", null, 0, "open.gif", true),
                menuBuilder.buildMenuItem("Open functions", null, 0, "open.gif", true),
                menuBuilder.buildMenuItem("Open LUT from...", null, 0, "open.gif", true),
                menuBuilder.buildMenuItem("Open functions from...", null, 0, "open.gif", true), ViewMenuBar.separator,
                menuBuilder.buildMenuItem("Save LUT", null, 0, "save.gif", true),
                menuBuilder.buildMenuItem("Save functions", null, 0, "save.gif", true),
                menuBuilder.buildMenuItem("Save LUT as...", null, 0, "save.gif", true),
                menuBuilder.buildMenuItem("Save functions as...", null, 0, "save.gif", true), ViewMenuBar.separator,
                menuBuilder.buildMenuItem("Histogram - LUT...", "DisplayLUT", 0, "histolut.gif", true),});
    }

    /**
     * Create the Scripting menu.
     * 
     * @return The new scripting menu
     */
    public JMenu makeScriptingMenu() {
        return menuBuilder.makeMenu("Scripts", false, new JMenuItem[] {
                menuBuilder.buildMenuItem("Record script", "RecordScript", 0, null, false),
                menuBuilder.buildMenuItem("Run script", "RunScript", 0, null, false)});
    }

    /**
     * Construct the toolbar menu.
     * 
     * @return the toolbar menu
     */
    public JMenu makeToolbarsMenu() {
        boolean showVOIToolbar = Preferences.is(Preferences.PREF_VOI_TOOLBAR_ON);
        final boolean showPaintToolbar = Preferences.is(Preferences.PREF_PAINT_TOOLBAR_ON);
        final boolean showScriptingToolbar = Preferences.is(Preferences.PREF_SCRIPTING_TOOLBAR_ON);
        boolean showImageToolbar = Preferences.is(Preferences.PREF_IMAGE_TOOLBAR_ON);

        // default the VOI and Image toolbars to on if the user hasn't explicitly turned them off
        if ( !showVOIToolbar && !Preferences.isPreferenceSet(Preferences.PREF_VOI_TOOLBAR_ON)) {
            showVOIToolbar = true;
        }

        if ( !showImageToolbar && !Preferences.isPreferenceSet(Preferences.PREF_IMAGE_TOOLBAR_ON)) {
            showImageToolbar = true;
        }

        return menuBuilder.makeMenu("Toolbars", 'T', false, new JMenuItem[] {
                menuBuilder.buildCheckBoxMenuItem("Image toolbar", "ImageToolbar", showImageToolbar),
                menuBuilder.buildCheckBoxMenuItem("Paint toolbar", "PaintToolbar", showPaintToolbar),
                menuBuilder.buildCheckBoxMenuItem("Scripting toolbar", "ScriptToolbar", showScriptingToolbar),
                menuBuilder.buildCheckBoxMenuItem("VOI toolbar", "VOIToolbar", showVOIToolbar)});
    }

    /**
     * Construct the utilities menu.
     * 
     * @return the utilities menu
     */
    public JMenu makeUtilitiesMenu() {
        return menuBuilder.makeMenu("Utilities", 'U', false, new JMenuItem[] {
                menuBuilder.makeMenu("4D tools", false, new JMenuItem[] {
                        menuBuilder.buildMenuItem("Convert 3D to 4D", "Convert3Dto4D", 0, null, false),
                        menuBuilder.buildMenuItem("Convert 4D to 3D", "Convert4Dto3D", 0, null, false),
                        menuBuilder.buildMenuItem("Convert 4D to RGB", "Convert4DtoRGB", 0, null, false),
                        menuBuilder.buildMenuItem("Extract 3D subset from 4D", "Subset", 0, null, false),
                        menuBuilder.buildMenuItem("Remove time volumes", null, 0, null, false),
                        menuBuilder.buildMenuItem("Swap dims 3<->4", "Swap34", 0, null, false),
                        menuBuilder.buildMenuItem("Swap dims 1<->4", "Swap14", 0, null, false),}),
                menuBuilder.buildMenuItem("Center of mass", "COM", 0, null, false),
                menuBuilder.buildMenuItem("Clone (copy)", "Clone", 0, null, false),
                menuBuilder.makeMenu("Conversion tools", false, new JMenuItem[] {
                        menuBuilder.buildMenuItem("Convert type", null, 0, null, false),
                        menuBuilder.buildMenuItem("Grays -> RGB", null, 0, null, false),
                        menuBuilder.buildMenuItem("RGB -> Gray", null, 0, null, false),
                        menuBuilder.buildMenuItem("RGB -> Grays", null, 0, null, false),
                        menuBuilder.buildMenuItem("RGB -> HSB", null, 0, null, false),}),
                menuBuilder.buildMenuItem("Correct image spacing", "CorrectSpace", 0, null, false),
                menuBuilder.makeMenu("Crop", false, new JMenuItem[] {
                        menuBuilder.buildMenuItem("Using boundary parameters", "CropBoundaryParam", 0, null, false),
                        menuBuilder.buildMenuItem("Using point parameters", "CropPointParam", 0, null, false),
                        menuBuilder.buildMenuItem("Using VOI", "Crop", 0, null, false)}),

                // menuBuilder.buildMenuItem("Dicom order", "DicomOrder", 0, null, false),
                menuBuilder.buildMenuItem("Fill image", null, 0, null, false),
                menuBuilder.makeMenu("Flip image", false, new JMenuItem[] {
                        menuBuilder.buildMenuItem("Horizontal", "ImageFlipY", 0, "fliphoriz.gif", true),
                        menuBuilder.buildMenuItem("Vertical", "ImageFlipX", 0, "flipvert.gif", true),
                        menuBuilder.buildMenuItem("Depth", "ImageFlipZ", 0, "flipvert.gif", true)}),
                menuBuilder.buildMenuItem("Generate grid", "writeGrid", 0, null, false),
                menuBuilder.buildMenuItem("Image calculator", "Calculator", 0, null, false),
                menuBuilder.buildMenuItem("Image calculator - Bulk Images", "BulkCalculator", 0, null, false),
                menuBuilder.buildMenuItem("Image math", null, 0, null, false),
                menuBuilder.buildMenuItem("Invert", null, 0, null, false),
                menuBuilder.buildMenuItem("Match images", "matchImages", 0, null, false),
                menuBuilder.buildMenuItem("Maximum intensity projection (MIP)", "maximumIntensityProjection", 0, null,
                        false),
                menuBuilder.buildMenuItem("Noise", null, 0, null, false),
                menuBuilder.buildMenuItem("Pad", null, 0, null, false),
                menuBuilder.makeMenu("Quantify", false, new JMenuItem[] {
                        menuBuilder.buildMenuItem("Quantify using mask", "Quantify", 0, null, false),
                        menuBuilder.buildMenuItem("Quantify mask(s)", "QuantifyMasks", 0, null, false),

                }),
                menuBuilder.buildMenuItem("Reorientation", "Reorientation", 0, null, false),
                menuBuilder.buildMenuItem("Replace pixel/voxel value", "ReplaceValue", 0, null, false),
                menuBuilder.makeMenu("Rotate", false, new JMenuItem[] {
                        menuBuilder.buildMenuItem("X axis 180", "RotateX180", 0, null, false),
                        menuBuilder.buildMenuItem("X axis +90", "RotateXPlus", 0, null, false),
                        menuBuilder.buildMenuItem("X axis -90", "RotateXMinus", 0, null, false),
                        menuBuilder.buildMenuItem("Y axis 180", "RotateY180", 0, null, false),
                        menuBuilder.buildMenuItem("Y axis +90", "RotateYPlus", 0, null, false),
                        menuBuilder.buildMenuItem("Y axis -90", "RotateYMinus", 0, null, false),
                        menuBuilder.buildMenuItem("Z axis 180", "RotateZ180", 0, null, false),
                        menuBuilder.buildMenuItem("Z axis +90", "RotateZPlus", 0, null, false),
                        menuBuilder.buildMenuItem("Z axis -90", "RotateZMinus", 0, null, false)}),
                menuBuilder.makeMenu("Slice tools", false, new JMenuItem[] {
                        menuBuilder.buildMenuItem("Concatenate", "Concat", 0, null, false),
                        menuBuilder.buildMenuItem("Extract slices", null, 0, null, false),
                        menuBuilder.buildMenuItem("Insert missing slices", "InsertMSlices", 0, null, false),
                        menuBuilder.buildMenuItem("Insert slice", null, 0, null, false),
                        menuBuilder.buildMenuItem("Pad slices to power of 2", "padding", 0, null, false),
                        menuBuilder.buildMenuItem("Randomize slice order", "RandOrder", 0, null, false),
                        menuBuilder.buildMenuItem("Remove slices", null, 0, null, false),
                        menuBuilder
                                .buildMenuItem("Replace blanks with averages", "ReplaceBlankWithAvg", 0, null, false),
                        menuBuilder.buildMenuItem("Replace slice", null, 0, null, false),}),
                menuBuilder.buildMenuItem("Subtract VOI background", "SubVOI", 0, null, false),});
    }

    /**
     * Construct the VOI menu.
     * 
     * @return the VOI menu
     */
    public JMenu makeVOIMenu() {
        return menuBuilder
                .makeMenu(
                        "VOI",
                        'V',
                        false,
                        new JComponent[] {

                                menuBuilder.makeMenu("New VOI", true, new JMenuItem[] {
                                        menuBuilder.buildMenuItem(CustomUIBuilder.PARAM_VOI_POINT, true),
                                        menuBuilder.buildMenuItem(CustomUIBuilder.PARAM_VOI_LINE, true),
                                        menuBuilder.buildMenuItem(CustomUIBuilder.PARAM_VOI_RECTANGLE, true),
                                        menuBuilder.buildMenuItem(CustomUIBuilder.PARAM_VOI_ELLIPSE, true),
                                        menuBuilder.buildMenuItem(CustomUIBuilder.PARAM_VOI_POLYGON, true),
                                        menuBuilder.buildMenuItem(CustomUIBuilder.PARAM_VOI_LEVELSET, true),
                                        menuBuilder.buildMenuItem(CustomUIBuilder.PARAM_VOI_3D_RECTANGLE, true)}),
                                ViewMenuBar.separator,
                                menuBuilder.makeMenu("Open VOI", true, new JMenuItem[] {
                                        menuBuilder.buildMenuItem("Open VOI", "Open VOI", 0, "open.gif", true),
                                        menuBuilder
                                                .buildMenuItem("Open all VOIs", "Open all VOIs", 0, "open.gif", true),
                                        menuBuilder.buildMenuItem("Open all VOIs from...", "Open all VOIs from...", 0,
                                                "open.gif", true),
                                        menuBuilder.buildMenuItem("Open label(s)...", "Open labels", 0, "open.gif",
                                                true)}),
                                ViewMenuBar.separator,
                                menuBuilder.makeMenu("Save VOI", true, new JMenuItem[] {
                                        menuBuilder.buildMenuItem("Save selected contours", "SaveSelectedContours", 0,
                                                "save.gif", true),
                                        menuBuilder.buildMenuItem("Save selected contours as",
                                                "SaveSelectedContoursAs", 0, "save.gif", true),
                                        menuBuilder.buildMenuItem("Save VOI", "Save VOI", 0, "save.gif", true),
                                        menuBuilder.buildMenuItem("Save VOI as", "Save VOI as", 0, "save.gif", true),
                                        menuBuilder
                                                .buildMenuItem("Save all VOIs", "Save all VOIs", 0, "save.gif", true),
                                        menuBuilder.buildMenuItem("Save all VOIs to...", "Save all VOIs to...", 0,
                                                "save.gif", true),
                                        menuBuilder.buildMenuItem("Save intensities in VOI to...",
                                                "SaveVOIIntensities", 0, "save.gif", true),
                                        menuBuilder.buildMenuItem("Save selected label to...",
                                                "SaveSelectedAnnotation", 0, "save.gif", true),
                                        menuBuilder.buildMenuItem("Save all labels to...", "SaveAllAnnotations", 0,
                                                "save.gif", true)}),
                                ViewMenuBar.separator,
                                menuBuilder
                                        .makeMenu("Edit VOI", true,
                                                new JMenuItem[] {
                                                        menuBuilder.buildMenuItem("Undo VOI", "undoVOI", 0,
                                                                "undopaint.gif", true),
                                                        menuBuilder.buildMenuItem("Cut VOI", "cutVOI", 0,
                                                                "cutpaint.gif", true),
                                                        menuBuilder.buildMenuItem("Copy VOI", "copyVOI", 0,
                                                                "copypaint.gif", true),
                                                        menuBuilder.buildMenuItem("Paste VOI", "pasteVOI", 0,
                                                                "pastepaint.gif", true),}),
                                menuBuilder.makeMenu("Select VOI", true, new JMenuItem[] {
                                        menuBuilder.buildMenuItem("Select all VOIs", "selectAllVOIs", 0, null, false),
                                        menuBuilder.buildMenuItem("Select none", "voiSelectNone", 0, null, false),}),
                                ViewMenuBar.separator,
                                menuBuilder.makeMenu("VOI grouping", true, new JMenuItem[] {
                                        menuBuilder.buildMenuItem("Group VOIs", "GroupVOIs", 0, null, false),
                                        menuBuilder.buildMenuItem("Ungroup VOIs", "UngroupVOIs", 0, null, false),}),
                                menuBuilder.makeMenu("VOI order", true, new JMenuItem[] {
                                        menuBuilder.buildMenuItem("Bring VOI to front", "BringToFront", 0, "front.gif",
                                                true),
                                        menuBuilder
                                                .buildMenuItem("Send VOI to back", "SendToBack", 0, "back.gif", true),
                                        menuBuilder.buildMenuItem("Bring VOI forward", "BringForward", 0,
                                                "forward.gif", true),
                                        menuBuilder.buildMenuItem("Send VOI backward", "SendBackward", 0,
                                                "backward.gif", true)}),
                                menuBuilder.makeMenu("Contour order", true, new JMenuItem[] {
                                        menuBuilder.buildMenuItem("Bring contour to front", "BringContourToFront", 0,
                                                "front.gif", true),
                                        menuBuilder.buildMenuItem("Send contour to back", "SendContourToBack", 0,
                                                "back.gif", true),
                                        menuBuilder.buildMenuItem("Bring contour forward", "BringContourForward", 0,
                                                "forward.gif", true),
                                        menuBuilder.buildMenuItem("Send contour backward", "SendContourBackward", 0,
                                                "backward.gif", true)}),
                                ViewMenuBar.separator,
                                menuBuilder.makeMenu("VOI conversion", true, new JMenuItem[] {
                                        menuBuilder.buildMenuItem("VOI -> Paint", "PaintMask", 0, null, false),
                                        menuBuilder.buildMenuItem("VOI -> All to Binary mask", "BinaryMask", 0, null,
                                                false),
                                        menuBuilder.buildMenuItem("VOI -> All to Short mask", "ShortMask", 0, null,
                                                false),
                                        menuBuilder.buildMenuItem("VOI -> All to Unsigned byte mask",
                                                "UnsignedByteMask", 0, null, false),
                                        menuBuilder.buildMenuItem("VOI -> Selected to Binary mask",
                                                "BinaryMaskSelected", 0, null, false),
                                        menuBuilder.buildMenuItem("VOI -> Selected to Short mask", "ShortMaskSelected",
                                                0, null, false),
                                        menuBuilder.buildMenuItem("VOI -> Selected to Unsigned byte mask",
                                                "UnsignedByteMaskSelected", 0, null, false)}),
                                menuBuilder.makeMenu("Mask conversion", true, new JMenuItem[] {
                                        menuBuilder.buildMenuItem("Mask -> VOI", "MaskToVOI", 0, null, false),
                                        menuBuilder.buildMenuItem("Mask -> Paint", "MaskToPaint", 0, null, false),}),
                                menuBuilder.makeMenu("Paint conversion", true, new JMenuItem[] {
                                        menuBuilder.buildMenuItem("Paint -> VOI", "PaintToVOI", 0, null, false),
                                        menuBuilder.buildMenuItem("Paint -> Unsigned byte mask", "PaintToUbyteMask", 0,
                                                null, false),
                                        menuBuilder.buildMenuItem("Paint -> Short mask", "PaintToShortMask", 0, null,
                                                false),}),
                                menuBuilder.buildCheckBoxMenuItem("Allow VOI holes (XOR)", "XOR", Preferences
                                        .is(Preferences.PREF_USE_VOI_XOR)),
                                ViewMenuBar.separator,
                                menuBuilder.makeMenu("Propagate", true, new JMenuItem[] {
                                        menuBuilder
                                                .buildMenuItem("To next slice", "PropVOIUp", 0, "voipropu.gif", true),
                                        menuBuilder.buildMenuItem("To previous slice", "PropVOIDown", 0,
                                                "voipropd.gif", true),
                                        menuBuilder.buildMenuItem("To all slices", "PropVOIAll", 0, "voipropall.gif",
                                                true)}),
                                menuBuilder.makeMenu("Evolve boundary 2D", true, new JMenuItem[] {
                                        menuBuilder.buildMenuItem("Active contour", "Snake", 0, null, false),
                                        menuBuilder.buildMenuItem("Active GVF", "AGVF", 0, null, false),
                                        menuBuilder.buildMenuItem("Spline active contour", "BSnake", 0, null, false),
                                        menuBuilder.buildMenuItem("Spline GVF", "GVF", 0, null, false)}),
                                menuBuilder.makeMenu("Flip VOI", true, new JMenuItem[] {
                                        menuBuilder.buildMenuItem("Horizontal", "VOIFlipY", 0, "fliphoriz.gif", true),
                                        menuBuilder.buildMenuItem("Vertical", "VOIFlipX", 0, "flipvert.gif", true),
                                        menuBuilder.buildMenuItem("Depth", "VOIFlipZ", 0, "flipvert.gif", true)}),
                                menuBuilder.buildMenuItem("Interpolate VOIs", "interpolateVOIs", 0, null, true),
                                ViewMenuBar.separator,
                                menuBuilder.buildMenuItem("Smooth VOI", "SmoothVOI", 0, null, true),
                                menuBuilder.buildMenuItem("Trim parameter", "Trim", 0, null, true),
                                ViewMenuBar.separator,
                                menuBuilder.buildMenuItem("Cardiology VOI", "Cardio", 0, null, true),
                                menuBuilder.makeMenu("Prostate VOI", true,
                                        new JMenuItem[] {
                                                menuBuilder.buildMenuItem("Merge VOIs", "ProstateMergedVOIs", 0, null,
                                                        false),
                                                menuBuilder.buildMenuItem("Surface Reconstruction",
                                                        "ProstateReconstruct", 0, null, false),
                                                menuBuilder.buildMenuItem("Save Features", "ProstateFeaturesSave", 0,
                                                        null, false),
                                                menuBuilder.buildMenuItem("Save Test Features", "ProstateFeaturesTest",
                                                        0, null, false),
                                                menuBuilder.buildMenuItem("Training", "ProstateFeaturesTrain", 0, null,
                                                        false),
                                                menuBuilder.buildMenuItem("Classification",
                                                        "ProstateFeaturesClassification", 0, null, false),
                                                menuBuilder.buildMenuItem("Load Mask", "LoadProstateMask", 0, null,
                                                        false),
                                                menuBuilder.buildMenuItem("Extract Surface", "ProstateExtract", 0,
                                                        null, false)}),
                                ViewMenuBar.separator,
                                menuBuilder.makeMenu("Graph", true, new JMenuItem[] {
                                        menuBuilder.buildMenuItem("Open VOI intensity graph", "OpenNewGraph", 0,
                                                "open.gif", true),
                                        menuBuilder.buildMenuItem("Boundary intensity", "boundaryIntensity", 0, null,
                                                true),
                                        menuBuilder.buildMenuItem("Total intensity", "totalIntensity", 0, null, true),
                                        menuBuilder.buildMenuItem("Average intensity", "avgIntensity", 0, null, true),
                                        menuBuilder.buildMenuItem("Total intensity with threshold",
                                                "totalIntensityThreshold", 0, null, true),
                                        menuBuilder.buildMenuItem("Average intensity with threshold",
                                                "avgIntensityThreshold", 0, null, true)}), ViewMenuBar.separator,
                                menuBuilder.buildMenuItem("Properties...", "VOIProperties", 'P', null, true),
                                menuBuilder.buildMenuItem("Statistics generator...", "VOIStatistics", 'G', null, true),});
    }

    /**
     * Enable or disable items in a menubar based on the image dimensionality and storage type.
     * 
     * @param numberOfDimensions Dimensionality of image (2D, 3D, 4D...)
     * @param type Data type from <code>ModelStorageBase</code>, byte, short, argb, etc.
     */
    public void setEnabledMenuItems(final int numberOfDimensions, final int type) {

        // if (!InsightToolkitSupport.isLibraryPresent()) {
        // menuBuilder.setMenuItemEnabled("Insight toolkit (ITK)", false);
        // }
        menuBuilder.setMenuItemEnabled("Close image(B)", false);

        if (numberOfDimensions == 4) {
            menuBuilder.setMenuItemEnabled("Center of mass", false);
            menuBuilder.setMenuItemEnabled("Density based clustering", false);
            menuBuilder.setMenuItemEnabled("Edge detection", false);
            menuBuilder.setMenuItemEnabled("Evaluate segmentation", false); // vois
            menuBuilder.setMenuItemEnabled("Entropy minimization", false);
            menuBuilder.setMenuItemEnabled("Extract surface (marching cubes)", false);
            menuBuilder.setMenuItemEnabled("FFT", false);
            menuBuilder.setMenuItemEnabled("Filters (wavelet)", false);
            menuBuilder.setMenuItemEnabled("Anisotropic diffusion", false);
            menuBuilder.setMenuItemEnabled("Boundary attenuation", false);
            menuBuilder.setMenuItemEnabled("Laplacian", false);
            menuBuilder.setMenuItemEnabled("Maximum intensity projection (MIP)", false);
            menuBuilder.setMenuItemEnabled("Mean", false);
            menuBuilder.setMenuItemEnabled("Median", false);
            menuBuilder.setMenuItemEnabled("Mode", false);
            menuBuilder.setMenuItemEnabled("Zero X non-maximum suppression", false);
            menuBuilder.setMenuItemEnabled("Slice averaging", false);
            menuBuilder.setMenuItemEnabled("Unsharp mask", false);
            menuBuilder.setMenuItemEnabled("Fill image", false);
            menuBuilder.setMenuItemEnabled("Filters (frequency)", false);
            menuBuilder.setMenuItemEnabled("Trilateral filter", false);
            menuBuilder.setMenuItemEnabled("FRAP", false);
            menuBuilder.setMenuItemEnabled("FRET", false);
            menuBuilder.setMenuItemEnabled("FRET bleed thru", false);
            menuBuilder.setMenuItemEnabled("FRET efficiency", false);
            menuBuilder.setMenuItemEnabled("Graph based", false);
            menuBuilder.setMenuItemEnabled("Multispectral", false);
            menuBuilder.setMenuItemEnabled("Inhomogeneity N3 correction", false);
            menuBuilder.setMenuItemEnabled("Levelset", false);
            menuBuilder.setMenuItemEnabled("Levelset diffusion", false);
            menuBuilder.setMenuItemEnabled("Morphological", false);
            menuBuilder.setMenuItemEnabled("MRI combined info", false);
            menuBuilder.setMenuItemEnabled("Adaptive noise reduction", false);
            menuBuilder.setMenuItemEnabled("Adaptive path smooth", false);
            menuBuilder.setMenuItemEnabled("Kernel regression", false);
            menuBuilder.setMenuItemEnabled("Nonlinear noise reduction", false);
            menuBuilder.setMenuItemEnabled("Nonlocal means filter", false);
            menuBuilder.setMenuItemEnabled("Point area average intensities", false);
            menuBuilder.setMenuItemEnabled("Principal component", false);
            menuBuilder.setMenuItemEnabled("Quantify using mask", false);
            menuBuilder.setMenuItemEnabled("Shading correction", false);
            menuBuilder.setMenuItemEnabled("Skeletonize3D pot field", false);
            menuBuilder.setMenuItemEnabled("Skeletonize", false);

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
            menuBuilder.setMenuItemEnabled("Histogram summary", false);
            menuBuilder.setMenuItemEnabled("Surface plotter", false);
            menuBuilder.setMenuItemEnabled("Evolve boundary 2D", false);
            menuBuilder.setMenuItemEnabled("Convert 3D to 4D", false);
            menuBuilder.setMenuItemEnabled("Capture image slices to new frame", false);
            menuBuilder.setMenuItemEnabled("Haralick texture", false);
            menuBuilder.setMenuItemEnabled("Display pixel similarity cost functions", false);
            menuBuilder.setMenuItemEnabled("Pad slices to power of 2", false);
            menuBuilder.setMenuItemEnabled("Replace blanks with averages", false);
            menuBuilder.setMenuItemEnabled("Replace slice", false);
            menuBuilder.setMenuItemEnabled("Insert missing slices", false);
            menuBuilder.setMenuItemEnabled("Randomize slice order", false);
            menuBuilder.setMenuItemEnabled("Barrel distortion correction", false);
            menuBuilder.setMenuItemEnabled("Circular sector to rectangle", false);
            menuBuilder.setMenuItemEnabled("Circle to rectangle", false);
            menuBuilder.setMenuItemEnabled("Ellipse to circle", false);
            menuBuilder.setMenuItemEnabled("Nearly circle to circle", false);
            menuBuilder.setMenuItemEnabled("Hough transform", false);
            if (ModelImage.isColorImage(type)) {
                menuBuilder.setMenuItemEnabled("Convert 4D to RGB", false);
            }
        } else if (numberOfDimensions == 3) {
            menuBuilder.setMenuItemEnabled("Adaptive noise reduction", false);
            menuBuilder.setMenuItemEnabled("Convert 4D to 3D", false);
            menuBuilder.setMenuItemEnabled("Convert 4D to RGB", false);
            menuBuilder.setMenuItemEnabled("Denoising BLS GSM", false);
            menuBuilder.setMenuItemEnabled("Extract 3D subset from 4D", false);
            menuBuilder.setMenuItemEnabled("Graph based", false);
            // menuBuilder.setMenuItemEnabled("Optimized automatic registration 3.5D", false);
            menuBuilder.setMenuItemEnabled("Remove time volumes", false);
            menuBuilder.setMenuItemEnabled("Swap dims 3<->4", false);
            menuBuilder.setMenuItemEnabled("Swap dims 1<->4", false);
            menuBuilder.setMenuItemEnabled("Time series optimized automatic registration", false);
            menuBuilder.setMenuItemEnabled("Barrel distortion correction", false);
            menuBuilder.setMenuItemEnabled("Circular sector to rectangle", false);
            menuBuilder.setMenuItemEnabled("Circle to rectangle", false);
            menuBuilder.setMenuItemEnabled("Ellipse to circle", false);
            menuBuilder.setMenuItemEnabled("Nearly circle to circle", false);
            menuBuilder.setMenuItemEnabled("Hough transform", false);
            menuBuilder.setMenuItemEnabled("Trilateral filter", false);
        } else if (numberOfDimensions == 2) {
            menuBuilder.setMenuItemEnabled("Align patient position", false);
            menuBuilder.setMenuItemEnabled("Draw 3D rectangle VOI", false);
            menuBuilder.setMenuItemEnabled("Animate", false);
            menuBuilder.setMenuItemEnabled("Boundary attenuation", false);
            menuBuilder.setMenuItemEnabled("Cine (movie)", false);
            menuBuilder.setMenuItemEnabled("Convert 4D to 3D", false);
            menuBuilder.setMenuItemEnabled("Convert 4D to RGB", false);
            menuBuilder.setMenuItemEnabled("Brain tools", false);
            menuBuilder.setMenuItemEnabled("Extract slices", false);
            menuBuilder.setMenuItemEnabled("Replace slice", false);
            menuBuilder.setMenuItemEnabled("Extract 3D subset from 4D", false);
            menuBuilder.setMenuItemEnabled("Extract surface (marching cubes)", false);
            menuBuilder.setMenuItemEnabled("FRAP", false);
            menuBuilder.setMenuItemEnabled("Light box", false);
            menuBuilder.setMenuItemEnabled("Maximum intensity projection (MIP)", false);
            menuBuilder.setMenuItemEnabled("Propagate", false);
            menuBuilder.setMenuItemEnabled("Triplanar", false);
            menuBuilder.setMenuItemEnabled("Volume renderers", false);
            menuBuilder.setMenuItemEnabled("Insert slice", false);
            menuBuilder.setMenuItemEnabled("Remove slices", false);
            menuBuilder.setMenuItemEnabled("Pad slices to power of 2", false);
            menuBuilder.setMenuItemEnabled("Remove time volumes", false);
            menuBuilder.setMenuItemEnabled("Manual 2D series", true);
            menuBuilder.setMenuItemEnabled("Constrained optimized automatic registration", false);
            menuBuilder.setMenuItemEnabled("Optimized automatic registration 2.5D", false);
            // menuBuilder.setMenuItemEnabled("Optimized automatic registration 3.5D", false);
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
            menuBuilder.setMenuItemEnabled("Swap dims 1<->4", false);
            menuBuilder.setMenuItemEnabled("Convert 3D to 4D", false);
            menuBuilder.setMenuItemEnabled("Reslice - isotropic voxels", false);
            menuBuilder.setMenuItemEnabled("Slice averaging", false);
            menuBuilder.setMenuItemEnabled("Link to another image", false);
            menuBuilder.setMenuItemEnabled("Point area average intensities", false);
            menuBuilder.setMenuItemEnabled("Correct image spacing", false);
            menuBuilder.setMenuItemEnabled("Capture image slices to new frame", false);
            menuBuilder.setMenuItemSelected("Show slice number overlay", false);
            menuBuilder.setMenuItemEnabled("Show slice number overlay", false);
            menuBuilder.setMenuItemEnabled("Skeletonize3D pot field", false);
            menuBuilder.setMenuItemEnabled("Replace blanks with averages", false);
            menuBuilder.setMenuItemEnabled("Insert missing slices", false);
            menuBuilder.setMenuItemEnabled("4D tools", false);
            menuBuilder.setMenuItemEnabled("Depth", false);
            menuBuilder.setMenuItemEnabled("VOI landmark", false);
            menuBuilder.setMenuItemEnabled("Reorientation / Resampling", false);
            menuBuilder.setMenuItemEnabled("Histogram slice matching", false);
        }

        if (ModelImage.isColorImage(type)) {
            menuBuilder.setMenuItemEnabled("Anisotropic diffusion", false);
            menuBuilder.setMenuItemEnabled("Boundary attenuation", false);
            menuBuilder.setMenuItemEnabled("Density based clustering", false);
            menuBuilder.setMenuItemEnabled("Diffeomorphic demons", false);
            menuBuilder.setMenuItemEnabled("Edge detection", false);
            menuBuilder.setMenuItemEnabled("Brain tools", false);
            menuBuilder.setMenuItemEnabled("FFT", false);
            menuBuilder.setMenuItemEnabled("Filters (frequency)", false);
            menuBuilder.setMenuItemEnabled("Filters (Gabor)", false);
            menuBuilder.setMenuItemEnabled("Filters (homomorphic)", false);
            menuBuilder.setMenuItemEnabled("Filters (wavelet)", false);
            menuBuilder.setMenuItemEnabled("Grays -> RGB", false);
            menuBuilder.setMenuItemEnabled("Inhomogeneity N3 correction", false);
            menuBuilder.setMenuItemEnabled("Laplacian", false);
            menuBuilder.setMenuItemEnabled("Levelset", false);
            menuBuilder.setMenuItemEnabled("Levelset diffusion", false);
            menuBuilder.setMenuItemEnabled("Local normalization", false);
            menuBuilder.setMenuItemEnabled("Mode", false);
            menuBuilder.setMenuItemEnabled("Morphological", false);
            menuBuilder.setMenuItemEnabled("MRI combined info", false);
            menuBuilder.setMenuItemEnabled("Noise", false);
            menuBuilder.setMenuItemEnabled("Nonlinear noise reduction", false);
            menuBuilder.setMenuItemEnabled("Nonlocal means filter", false);
            menuBuilder.setMenuItemEnabled("Nonmaximum suppression", false);
            menuBuilder.setMenuItemEnabled("Zero X non-maximum suppression", false);
            menuBuilder.setMenuItemEnabled("Single channel", false);
            menuBuilder.setMenuItemEnabled("Unsharp mask", false);
            menuBuilder.setMenuItemEnabled("Watershed", false);
            menuBuilder.setMenuItemEnabled("VOI landmark", false);
            menuBuilder.setMenuItemEnabled("Evolve boundary 2D", false);
            menuBuilder.setMenuItemEnabled("Correct image spacing", false);
            menuBuilder.setMenuItemEnabled("Surface plotter", false);
            menuBuilder.setMenuItemEnabled("Display pixel similarity cost functions", false);
            menuBuilder.setMenuItemEnabled("Skeletonize3D pot field", false);
            menuBuilder.setMenuItemEnabled("Skeletonize", false);
            menuBuilder.setMenuItemEnabled("Single MRI image", false);
            menuBuilder.setMenuItemEnabled("Two MRI images", false);
            menuBuilder.setMenuItemEnabled("Hough transform", false);
        } else {
            menuBuilder.setMenuItemEnabled("RGB -> Gray", false);
            menuBuilder.setMenuItemEnabled("RGB -> Grays", false);
            menuBuilder.setMenuItemEnabled("RGB -> HSB", false);
            menuBuilder.setMenuItemEnabled("Color edge", false);

            if (numberOfDimensions == 2) {
                menuBuilder.setMenuItemEnabled("Principal component", false);
            }
        }

        menuBuilder.setMenuItemEnabled("DICOM database access", true);
    }

    /**
     * Enable all items in a menubar.
     * 
     */
    protected void enableAllMenuItems() {

        // if (!InsightToolkitSupport.isLibraryPresent()) {
        // menuBuilder.setMenuItemEnabled("Insight toolkit (ITK)", false);
        // }

        menuBuilder.setMenuItemEnabled("Density based clustering", true);
        menuBuilder.setMenuItemEnabled("Edge detection", true);
        menuBuilder.setMenuItemEnabled("Evaluate segmentation", true); // vois
        menuBuilder.setMenuItemEnabled("Entropy minimization", true);
        menuBuilder.setMenuItemEnabled("Extract surface (marching cubes)", true);
        menuBuilder.setMenuItemEnabled("FFT", true);
        menuBuilder.setMenuItemEnabled("Filters (wavelet)", true);
        menuBuilder.setMenuItemEnabled("Anisotropic diffusion", true);
        menuBuilder.setMenuItemEnabled("Boundary attenuation", true);
        menuBuilder.setMenuItemEnabled("Laplacian", true);
        menuBuilder.setMenuItemEnabled("Mean", true);
        menuBuilder.setMenuItemEnabled("Median", true);
        menuBuilder.setMenuItemEnabled("Mode", true);
        menuBuilder.setMenuItemEnabled("Zero X non-maximum suppression", true);
        menuBuilder.setMenuItemEnabled("Slice averaging", true);
        menuBuilder.setMenuItemEnabled("Unsharp mask", true);
        menuBuilder.setMenuItemEnabled("Fill image", true);
        menuBuilder.setMenuItemEnabled("Filters (frequency)", true);
        menuBuilder.setMenuItemEnabled("FRAP", true);
        menuBuilder.setMenuItemEnabled("FRET", true);
        menuBuilder.setMenuItemEnabled("FRET bleed thru", true);
        menuBuilder.setMenuItemEnabled("FRET efficiency", true);
        menuBuilder.setMenuItemEnabled("Graph based", true);
        menuBuilder.setMenuItemEnabled("Multispectral", true);
        menuBuilder.setMenuItemEnabled("Inhomogeneity N3 correction", true);
        menuBuilder.setMenuItemEnabled("Levelset", true);
        menuBuilder.setMenuItemEnabled("Levelset diffusion", true);
        menuBuilder.setMenuItemEnabled("Morphological", true);
        menuBuilder.setMenuItemEnabled("MRI combined info", true);
        menuBuilder.setMenuItemEnabled("Noise", true);
        menuBuilder.setMenuItemEnabled("Adaptive noise reduction", true);
        menuBuilder.setMenuItemEnabled("Adaptive path smooth", true);
        menuBuilder.setMenuItemEnabled("Nonlinear noise reduction", true);
        menuBuilder.setMenuItemEnabled("Point area average intensities", true);
        menuBuilder.setMenuItemEnabled("Principal component", true);
        menuBuilder.setMenuItemEnabled("Quantify using mask", true);
        menuBuilder.setMenuItemEnabled("Shading correction", true);
        menuBuilder.setMenuItemEnabled("Skeletonize3D pot field", true);
        menuBuilder.setMenuItemEnabled("Skeletonize", true);

        // menuBuilder.setMenuItemEnabled("AFNI - Shear", true);
        menuBuilder.setMenuItemEnabled("Optimized automatic registration", true);
        menuBuilder.setMenuItemEnabled("Constrained optimized automatic registration", true);
        menuBuilder.setMenuItemEnabled("Optimized automatic registration 2.5D", true);
        menuBuilder.setMenuItemEnabled("B-Spline automatic registration 2D/3D", true);
        menuBuilder.setMenuItemEnabled("B-Spline automatic registration 2.5D", true);
        menuBuilder.setMenuItemEnabled("Least squares", true);
        menuBuilder.setMenuItemEnabled("Thin plate spline", true);
        menuBuilder.setMenuItemEnabled("Manual 2D series", true);
        menuBuilder.setMenuItemEnabled("Align patient position", true);
        menuBuilder.setMenuItemEnabled("VOI landmark", true);
        menuBuilder.setMenuItemEnabled("Reslice - isotropic voxels", true);
        menuBuilder.setMenuItemEnabled("Subtract VOI background", true);
        menuBuilder.setMenuItemEnabled("Single channel", true);
        menuBuilder.setMenuItemEnabled("Watershed", true);
        menuBuilder.setMenuItemEnabled("Histogram summary", true);
        menuBuilder.setMenuItemEnabled("RGB -> Gray", true);
        menuBuilder.setMenuItemEnabled("RGB -> Grays", true);
        menuBuilder.setMenuItemEnabled("RGB -> HSB", true);
        menuBuilder.setMenuItemEnabled("Surface plotter", true);
        menuBuilder.setMenuItemEnabled("Evolve boundary 2D", true);
        menuBuilder.setMenuItemEnabled("Convert 3D to 4D", true);
        menuBuilder.setMenuItemEnabled("Capture image slices to new frame", true);
        menuBuilder.setMenuItemEnabled("Haralick texture", true);
        menuBuilder.setMenuItemEnabled("Display pixel similarity cost functions", true);
        menuBuilder.setMenuItemEnabled("Pad slices to power of 2", true);
        menuBuilder.setMenuItemEnabled("Replace blanks with averages", true);
        menuBuilder.setMenuItemEnabled("Insert missing slices", true);
        menuBuilder.setMenuItemEnabled("Barrel distortion correction", true);
        menuBuilder.setMenuItemEnabled("Circular sector to rectangle", true);
        menuBuilder.setMenuItemEnabled("Circle to rectangle", true);
        menuBuilder.setMenuItemEnabled("Ellipse to circle", true);
        menuBuilder.setMenuItemEnabled("Nearly circle to circle", true);
        menuBuilder.setMenuItemEnabled("Hough transform", true);
        menuBuilder.setMenuItemEnabled("Convert 4D to RGB", true);
        menuBuilder.setMenuItemEnabled("Convert 4D to 3D", true);
        menuBuilder.setMenuItemEnabled("Denoising BLS GSM", true);
        menuBuilder.setMenuItemEnabled("Extract 3D subset from 4D", true);
        // menuBuilder.setMenuItemEnabled("Optimized automatic registration 3.5D", true);
        menuBuilder.setMenuItemEnabled("Remove time volumes", true);
        menuBuilder.setMenuItemEnabled("Swap dims 3<->4", true);
        menuBuilder.setMenuItemEnabled("Swap dims 1<->4", true);
        menuBuilder.setMenuItemEnabled("Time series optimized automatic registration", true);
        menuBuilder.setMenuItemEnabled("3D rectangle", true);
        menuBuilder.setMenuItemEnabled("Animate", true);
        menuBuilder.setMenuItemEnabled("Anonymize face (BET)", true);
        menuBuilder.setMenuItemEnabled("Cine (movie)", true);
        menuBuilder.setMenuItemEnabled("Extract slices", true);
        menuBuilder.setMenuItemEnabled("Replace slice", true);
        menuBuilder.setMenuItemEnabled("Brain tools", true);
        menuBuilder.setMenuItemEnabled("Light box", true);
        menuBuilder.setMenuItemEnabled("Propagate", true);
        menuBuilder.setMenuItemEnabled("Triplanar", true);
        menuBuilder.setMenuItemEnabled("Volume renderers", true);
        menuBuilder.setMenuItemEnabled("Insert slice", true);
        menuBuilder.setMenuItemEnabled("Remove slices", true);
        menuBuilder.setMenuItemEnabled("X axis 180", true);
        menuBuilder.setMenuItemEnabled("X axis +90", true);
        menuBuilder.setMenuItemEnabled("X axis -90", true);
        menuBuilder.setMenuItemEnabled("Y axis 180", true);
        menuBuilder.setMenuItemEnabled("Y axis +90", true);
        menuBuilder.setMenuItemEnabled("Y axis -90", true);
        menuBuilder.setMenuItemEnabled("Randomize slice order", true);
        menuBuilder.setMenuItemEnabled("Slice averaging", true);
        menuBuilder.setMenuItemEnabled("Link to another image", true);
        menuBuilder.setMenuItemEnabled("Correct image spacing", true);
        menuBuilder.setMenuItemSelected("ShowSliceNum", true);
        menuBuilder.setMenuItemEnabled("Show slice number overlay", true);
        menuBuilder.setMenuItemEnabled("4D tools", true);
        menuBuilder.setMenuItemEnabled("Depth", true);
        menuBuilder.setMenuItemEnabled("Filters (Gabor)", true);
        menuBuilder.setMenuItemEnabled("Filters (homomorphic)", true);
        menuBuilder.setMenuItemEnabled("Grays -> RGB", true);
        menuBuilder.setMenuItemEnabled("Image math", true);
        menuBuilder.setMenuItemEnabled("Lightbox generator", true);
        menuBuilder.setMenuItemEnabled("Local normalization", true);
        menuBuilder.setMenuItemEnabled("Nonmaximum suppression", true);
        menuBuilder.setMenuItemEnabled("Single MRI image", true);
        menuBuilder.setMenuItemEnabled("Two MRI images", true);
        menuBuilder.setMenuItemEnabled("Hough transform", true);
        menuBuilder.setMenuItemEnabled("Color edge", true);
        menuBuilder.setMenuItemEnabled("DICOM database access", true);
    }
}
