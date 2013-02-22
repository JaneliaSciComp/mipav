package gov.nih.mipav.view;


import gov.nih.mipav.model.file.FileUtility;

import java.io.File;


/**
 * A simple file filter to display only files with the appropriate extension in the file dialog.
 * 
 * @author Harman Singh
 */
public class ViewImageFileFilter extends javax.swing.filechooser.FileFilter implements java.io.FileFilter {
    // ~ Static fields/initializers
    // -------------------------------------------------------------------------------------

    /** Display all files. */
    public static final int ALL = 0;

    /** Filter avi files (*.avi). */
    public static final int AVI = 1;

    /** Class Files (*.class). */
    public static final int CLASS = 2;

    /** FreeSurfer ascii files (*.asc). */
    public static final int FREESURFER = 3;

    /** General( *.gif; *.jpeg; *.jpg; *.pict; *.psd; *.tif; *.tiff). */
    public static final int GEN = 4;

    /** Graphs (*.plt). */
    public static final int PLOT = 5;

    /** Look-Up-Table Files (*.lut). */
    public static final int LUT = 6;

    /** Matrix Files (*.mtx; *.mat; *.xfm; *.tps; *.1D). */
    /** .tps contains int N and float C[][] for thin plate spline */
    public static final int MATRIX = 7;

    /** Medical Formats(*.dcm; *.ima; *.img; *.mnc; *.sig; *.xml; *.head; *.nii, *.rec, *.frec, *.nrrd). */
    public static final int TECH = 8;

    /** Microscopy files (*.avi; *.ics; *.liff; *.lsm; *.pic; *.stk; *.tif; *.tiff; *.xml). */
    public static final int MICROSCOPY = 9;

    /** Misc.(*.bmp; *.pcx; *.png; *.tga; *.xbm; *.xpm). */
    public static final int MISC = 10;

    /** Optical Files (for ophthalmology) (*.avi; *.bmp; *.jpg; *.pict; *.tif; *.tiff; *.img; *.psd). */
    public static final int OPTICAL = 11;

    /** Script Files (*.sct). */
    public static final int SCRIPT = 12;

    /** Surface Files (*.sur). */
    public static final int SURFACE = 13;

    /** Transfer Function Files (*.fun). */
    public static final int FUNCT = 14;

    /** VOI Files (*.voi). */
    public static final int VOI = 15;

    /** B-Spline Nonlinear Transformation Files (*.nlt). */
    public static final int NLT = 16;

    /** Filter made up on-the-fly. */
    public static final int DYNAMIC = 17;

    /** All plugin related files (.class, .jar, .zip, .tar, .tar.gz). */
    public static final int PLUGIN = 18;

    /** User defined */
    public static final int UDEF = 19;

    /** R-table (*.rtb) */
    public static final int RTABLE = 20;

    /** mipav data provenance (*.xmp) files */
    public static final int DATA_PROVENANCE = 21;

    public static final int TIFF = 22;

    /** Dicom Matrix info Files (*.dicomMatrix). */
    public static final int DICOMMATRIX = 23;

    /** Fiber track files */
    public static final int FIBER = 24;

    /** description strings for each filterType. */
    // note that the order must match the order of filterType definitions above!!
    private static String[] descriptions = {
            "All Files", // ALL
            "AVI (*.avi)", // AVI
            "Class Files (*.class)", // CLASS
            "FreeSurfer (*.asc)", // FREESURFER
            "General (*.gif; *.jpeg; *.jpg; *.pict; *.psd; *.tif; *.tiff)", // GEN
            "Graphs (*.plt)", // PLOT
            "Look-Up-Table Files (*.lut)", // LUT
            "Matrix Files (*.mtx; *.mat; *.xfm; *.tps; *.1D)", // MATRIX
            "Medical (*.dcm; *.xml, *ima; *.img; *.mnc; *.sig; *.head; *.nii; *.rec; *.frec; *.nrrd; *.gz; *.bz2)", // TECH
            "Microscopy (*.avi; *.ics; *.liff; *.lsm; *.pic; *.stk; *.tif; *.tiff; *.xml)", // MICROSCOPY
            "Misc.(*.avi; *.bmp; *.pcx; *.png; *.tga; *.xbm; *.xpm)", // MISC
            "Optical (*.avi; *.bmp; *.img; *.jpg; *.pict; *.psd; *.tif; *.tiff; *.xml)", // OPTICAL
            "Script Files (*.sct)", // SCRIPT
            "Surface Files (*.sur; *.wrl; *.xml; *.vtk; *.vtp; *.stl; *.ply; *.gii)", // SURFACE
            "Transfer Function Files (*.fun)", // FUNCT
            "VOI Files (*.voi)", // VOI
            "Nonlinear Transformation Files (*.nlt)", "Dynamic", "Plugin Files", "User Defined", "R-table (*.rtb)",
            "Data provenance (*.xmp)", // NLT
            "TIFF files (*.tif; *.tiff)", "DicomMatrix file (*.dicomMatrix)",
            "Fiber Files (*.dat; *.vtk; *.vtp)"}; // FIBER

    /** short description strings for each filterType. */
    // note that the order must match the order of filterType definitions above!!
    private static String[] shortDescriptions = {"All", // ALL
            "AVI", // AVI
            "Class Files", // CLASS
            "FreeSurfer", // FREESURFER
            "General", // GEN
            "Graphs", // PLOT
            "Look-Up-Table Files", // LUT
            "Matrix Files", // MATRIX
            "Medical", // TECH
            "Microscopy", // MICROSCOPY
            "Misc.", // MISC
            "Optical", // OPTICAL
            "Script Files", // SCRIPT
            "Surface Files", // SURFACE
            "Transfer Function Files", // FUNCT
            "VOI Files", // VOI
            "Nonlinear Transformation Files", // NLT
            "Dynamic", "Plugin", "User Defined", "R-table", "Data provenance", "TIFF", "DicomMatrix"};

    /** array of user defined extensions */
    protected static String[] userDefinedExtensions;

    /** This is the user defined file extensions String */
    protected static String udefExtsString;

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------
    /** Description of dynamic filter. */
    protected String dynamicDescription = null;

    /** Extensions to look at for the dynamic filter. */
    protected String[] dynamicExts = null;

    /** Filter type of this filter. */
    protected final int filterType;

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------
    /**
     * Constructs new filter of given type.
     * 
     * @param filterType Type of filter (GEN, TECH, etc.)
     */
    public ViewImageFileFilter(final int filterType) {
        this.filterType = filterType;
    }

    /**
     * Constructs a dynamic filter, with the given extensions.
     * 
     * @param exts Extensions to accept for dynamic filter.
     */
    public ViewImageFileFilter(final String[] exts) {
        this(ViewImageFileFilter.DYNAMIC);
        dynamicExts = exts;
        dynamicDescription = "Specific files (";
        for (int i = 0; i < exts.length; i++) {
            dynamicDescription += "*" + exts[i];
            if (i < (exts.length - 1)) {
                dynamicDescription += "; ";
            } else {
                dynamicDescription += ")";
            }
        }
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------
    /**
     * Returns the description of the given file filter.
     * 
     * @param filter Filter to check.
     * 
     * @return Description of filter.
     */
    public static String getDescription(final int filter) {
        // make sure filter type is valid
        if ( (filter < 0) || (filter >= ViewImageFileFilter.descriptions.length)) {
            return " ";
        }
        return ViewImageFileFilter.descriptions[filter];
    }

    /**
     * Returns the list of descriptions.
     * 
     * @return The list of descriptions.
     */
    public static String[] getDescriptions() {
        ViewImageFileFilter.setUserDefinedExtensions();
        return ViewImageFileFilter.descriptions;
    }

    /**
     * DOCUMENT ME!
     * 
     * @param filter DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    public static int getFilterIndex(final String filter) {
        for (int i = 0; i < ViewImageFileFilter.descriptions.length; i++) {
            if (filter.equalsIgnoreCase(ViewImageFileFilter.descriptions[i])) {
                return i;
            }
        }
        return 0;
    }

    /**
     * Returns the dshort escription of the given file filter.
     * 
     * @param filter Filter to check.
     * 
     * @return Short description of filter.
     */
    public static String getShortDescription(final int filter) {
        // make sure filter type is valid
        if ( (filter < 0) || (filter >= ViewImageFileFilter.shortDescriptions.length)) {
            return " ";
        }
        return ViewImageFileFilter.shortDescriptions[filter];
    }

    /**
     * Determines if the given file filter's description matches the description associated with the given filterType.
     * 
     * @param filter The filter to be tested
     * @param filterType The filter type to test for a match
     * 
     * @return <code>true</code> if and only if the filter's description matches the filterType description.
     */
    public static boolean matches(final javax.swing.filechooser.FileFilter filter, final int filterType) {
        final String desc = filter.getDescription();
        // if desc is null, then it can't match
        if (desc == null) {
            return false;
        }
        // make sure that filterType is valid
        if ( (filterType < 0) || (filterType >= ViewImageFileFilter.descriptions.length)) {
            return false;
        }
        // if descriptions match then return true
        if (desc.equalsIgnoreCase(ViewImageFileFilter.descriptions[filterType])) {
            return true;
        }
        // otherwise, no match
        return false;
    } // end matches()

    /**
     * One of the overrides to FileFilter. This function is called for each file in the current directory of the file
     * dialog to test if it is a file with the appropriate extension. If so, it is accepted for display in the file
     * dialog.
     * 
     * @param f The file whose name is to be tested for extension.
     * 
     * @return <code>true</code> if and only if the file has an appropriate extension.
     */
    public boolean accept(final File f) {
        if (f.isDirectory()) {
            return true;
        }

        final String extension = FileUtility.getExtension(f.getAbsolutePath()).toLowerCase();
        return accept(extension);
    }

    /**
     * Checks if extension matches what is accepted for the filter type. If so, returns <code>true</code>; if not,
     * returns <code>false</code>.
     * 
     * @param extension Extension of file to check.
     * 
     * @return <code>true</code> if extension is in file filter.
     */
    public boolean accept(String extension) {
        
    	extension = extension.toLowerCase();
    	
    	if (filterType == ViewImageFileFilter.ALL) {
            return true;
        }
        if ( (filterType == ViewImageFileFilter.MISC)
                && ( (extension.equals(".bmp")) || (extension.equals(".pcx")) || (extension.equals(".png"))
                        || (extension.equals(".tga")) || (extension.equals(".xbm")) || (extension.equals(".xpm")) || (extension
                        .equals(".avi")))) {
            return true;
        } else if ( (filterType == ViewImageFileFilter.GEN)
                && ( (extension.equals(".gif")) || (extension.equals(".jpeg")) || (extension.equals(".jpg"))
                        || (extension.equals(".pict")) || (extension.equals(".psd")) || (extension.equals(".tif")) || (extension
                        .equals(".tiff")))) {
            return true;
        } else if ( (filterType == ViewImageFileFilter.TIFF)
                && ( (extension.equals(".tif")) || (extension.equals(".tiff")))) {
            return true;
        } else if ( (filterType == ViewImageFileFilter.TECH)
                && ( (extension.equals(".img")) || (extension.equals(".ima")) || (extension.equals(".dcm"))
                        || (extension.equals(".mnc")) || (extension.equals(".sig")) || (extension.equals(".xml"))
                        || (extension.equals(".head")) || (extension.equals(".nii")) || (extension.equals(".rec"))
                        || (extension.equals(".frec")) || (extension.equals(".nrrd")) || (extension.equals(".gz")) || (extension
                        .equals(".bz2")))) {
            return true;
        } else if ( (filterType == ViewImageFileFilter.VOI)
                && ( (extension.equals(".voi")) || (extension.equals(".oly")))) {
            return true;
        } else if ( (filterType == ViewImageFileFilter.FUNCT) && (extension.equals(".fun"))) {
            return true;
        } else if ( (filterType == ViewImageFileFilter.LUT) && (extension.equals(".lut"))) {
            return true;
        } else if ( (filterType == ViewImageFileFilter.PLOT) && (extension.equals(".plt"))) {
            return true;
        } else if ( (filterType == ViewImageFileFilter.MATRIX)
                && ( (extension.equals(".mtx")) || (extension.equals(".mat")) || (extension.equals(".xfm")) || (extension
                        .equals(".tps")) || extension.equals(".1d"))) {
            return true;
        } else if ( (filterType == ViewImageFileFilter.CLASS) && (extension.equals(".class"))) {
            return true;
        } else if ( (filterType == ViewImageFileFilter.SCRIPT) && (extension.equals(".sct"))) {
            return true;
        } else if ( (filterType == ViewImageFileFilter.SURFACE)
                && (extension.equals(".sur") || extension.equals(".wrl") || extension.equals(".xml")
                        || extension.equals(".vtk") || extension.equals(".vtp") || extension.equals(".stl")
                        || extension.equals(".ply") || extension.equals(".gii"))) {
            return true;
        } else if ( (filterType == ViewImageFileFilter.FIBER)
                && (extension.equals(".dat") || extension.equals(".vtk") || extension.equals(".vtp"))) {
            return true;
        } else if ( (filterType == ViewImageFileFilter.OPTICAL)
                && ( (extension.equals(".avi")) || (extension.equals(".xml")) || (extension.equals(".bmp"))
                        || (extension.equals(".img")) || (extension.equals(".jpeg")) || (extension.equals(".jpg"))
                        || (extension.equals(".pict")) || (extension.equals(".psd")) || (extension.equals(".tif")) || (extension
                        .equals(".tiff")))) {
            return true;
        } else if ( (filterType == ViewImageFileFilter.FREESURFER) && (extension.equals(".asc"))) {
            return true;
        } else if ( (filterType == ViewImageFileFilter.AVI) && (extension.equals(".avi"))) {
            return true;
        } else if ( (filterType == ViewImageFileFilter.MICROSCOPY)
                && ( (extension.equals(".avi")) || (extension.equals(".ics")) || (extension.equals(".liff"))
                        || (extension.equals(".lsm")) || (extension.equals(".pic")) || (extension.equals(".stk"))
                        || (extension.equals(".tif")) || (extension.equals(".tiff")) || (extension.equals(".xml")))) {
            return true;
        } else if ( (filterType == ViewImageFileFilter.NLT) && (extension.equals(".nlt"))) {
            return true;
        } else if ( (filterType == ViewImageFileFilter.PLUGIN)
                && ( (extension.equals(".class")) || (extension.equals(".jar")) || (extension.equals(".zip"))
                        || (extension.equals(".tar")) || (extension.equals(".gz")))) {
            return true;
        } else if (filterType == ViewImageFileFilter.DYNAMIC) {
            for (final String element : dynamicExts) {
                if (extension.equals(element)) {
                    return true;
                }
            }
        } else if (filterType == ViewImageFileFilter.UDEF) {
            if (ViewImageFileFilter.userDefinedExtensions != null) {
                for (String element : ViewImageFileFilter.userDefinedExtensions) {
                    element = element.toLowerCase();
                	if (extension.equals("." + element.split("\\.")[1])) {
                        return true;
                    }
                }
            }
        } else if (filterType == ViewImageFileFilter.RTABLE && extension.equals(".rtb")) {
            return true;
        } else if (filterType == ViewImageFileFilter.DATA_PROVENANCE && extension.equals(".xmp")) {
            return true;
        } else if (filterType == ViewImageFileFilter.DICOMMATRIX && extension.equals(".dicommatrix")) {
            return true;
        }
        return false;
    }

    /**
     * Determines if this file filter is equal (in value) to another.
     * 
     * @param filter The filter to be tested for equality
     * 
     * @return <code>true</code> if and only if the filter is the same type as this filter.
     */
    public boolean equals(final javax.swing.filechooser.FileFilter filter) {
        // if filter is not a ViewImageFileFilter, then it doesn't match
        if ( ! (filter instanceof ViewImageFileFilter)) {
            return false;
        }
        if (this.filterType == ((ViewImageFileFilter) filter).filterType) {
            return true;
        } else {
            return false;
        }
    } // end equals()

    /**
     * One of the overrides to FileFilter. This function is called just to access the description of the filter.
     * 
     * @return The description for the filter.
     */
    public String getDescription() {
        // if dynamic, return dynamic description
        if (filterType == ViewImageFileFilter.DYNAMIC) {
            return dynamicDescription;
        }
        // make sure filter type is valid
        if ( (filterType < 0) || (filterType >= ViewImageFileFilter.descriptions.length)) {
            return " ";
        }
        return ViewImageFileFilter.descriptions[filterType];
    }

    /**
     * Returns a list of the files in the directory that satisfy the file filter.
     * 
     * @param file Directory
     * 
     * @return List of files in directory that satisfy file filter.
     */
    public String[] listFiles(final File file) {
        final File[] allFiles = file.listFiles();
        int count = 0;
        for (int i = 0; i < allFiles.length; i++) {
            if ( !allFiles[i].isDirectory() && accept(allFiles[i])) {
                count++;
            }
        }
        final String[] files = new String[count];
        count = 0;
        for (int i = 0; i < allFiles.length; i++) {
            if ( !allFiles[i].isDirectory() && accept(allFiles[i])) {
                files[count] = allFiles[i].toString();
                count++;
            }
        }
        return files;
    }

    /**
     * This sets the udef description becasue as the user edits the file extensions, the description changes
     * 
     * @param desc Description String
     */
    public static void setUdefDescription(final String desc) {
        ViewImageFileFilter.descriptions[21] = desc;
    }

    /** This sets the user defined Exts String as well as the Arraay of exts from the Preferences */
    public static void setUserDefinedExtensions() {
        ViewImageFileFilter.udefExtsString = Preferences.getProperty(Preferences.PREF_USER_FILETYPES);
        if (ViewImageFileFilter.udefExtsString != null && ( ! (ViewImageFileFilter.udefExtsString.trim().equals("")))) {
            final String desc = "User Defined (" + ViewImageFileFilter.udefExtsString + ")";
            ViewImageFileFilter.setUdefDescription(desc);
            ViewImageFileFilter.userDefinedExtensions = ViewImageFileFilter.udefExtsString.split(";");
        } else {
            final String desc = "User Defined";
            ViewImageFileFilter.setUdefDescription(desc);
            ViewImageFileFilter.userDefinedExtensions = null;
        }
    }
}
