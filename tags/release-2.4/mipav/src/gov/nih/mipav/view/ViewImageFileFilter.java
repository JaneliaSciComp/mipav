package gov.nih.mipav.view;

import java.io.File;

/**
 *   A simple file filter to display only files with the appropriate
 *	extension in the file dialog.
 *
 *		@author     Harman Singh
 *
 */

public class ViewImageFileFilter
    extends javax.swing.filechooser.FileFilter {

    /** Display all files. */
    public static final int ALL = 0;

    /** Filter avi files (*.avi) */
    public static final int AVI = 1;

    /** Class Files (*.class) */
    public static final int CLASS = 2;

    /** FreeSurfer ascii files (*.asc) */
    public static final int FREESURFER = 3;

    /** General( *.gif; *.jpeg; *.jpg; *.pict; *.psd; *.tif; *.tiff) */
    public static final int GEN = 4;

    /** Graphs (*.plt)*/
    public static final int PLOT = 5;

    /** Look-Up-Table Files (*.lut) */
    public static final int LUT = 6;

    /** Matrix Files (*.mtx; *.mat) */
    public static final int MATRIX = 7;

    /** Medical Formats(*.dcm; *.ima; *.img; *.mnc; *.sig; *.xml; *.head; *.nii) */
    public static final int TECH = 8;

    /** Microscopy files (*.avi; *.ics; *.lsm; *.pic; *.stk; *.tif; *.tiff; *.xml) */
    public static final int MICROSCOPY = 9;

    /** Misc.(*.bmp; *.pcx; *.png; *.tga; *.xbm; *.xpm) */
    public static final int MISC = 10;

    /** Optical Files (for ophthalmology) (*.avi; *.bmp; *.jpg; *.pict; *.tif; *.tiff; *.img; *.psd) */
    public static final int OPTICAL = 11;

    /** Filter project files (*.xml) */
    public static final int PROJECT = 12;

    /** Script Files (*.sct) */
    public static final int SCRIPT = 13;

    /** Surface Files (*.sur) */
    public static final int SURFACE = 14;

    /** Transfer Function Files (*.fun) */
    public static final int FUNCT = 15;

    /** VOI Files (*.voi) */
    public static final int VOI = 16;

    /** B-Spline Nonlinear Transformation Files (*.nlt) */
    public static final int NLT = 17;

    /** Filter made up on-the-fly.*/
    public static final int DYNAMIC = 18;

    /** All plugin related files (.class, .jar, .zip)*/
    public static final int PLUGIN = 19;

    /** description strings for each filterType */
    // note that the order must match the order of filterType definitions above!!
    private static String[] descriptions = {
        "All (*.*)", // ALL
        "AVI (*.avi)", // AVI
        "Class Files (*.class)", // CLASS
        "FreeSurfer (*.asc)", // FREESURFER
        "General (*.gif; *.jpeg; *.jpg; *.pict; *.psd; *.tif; *.tiff)", // GEN
        "Graphs (*.plt)", // PLOT
        "Look-Up-Table Files (*.lut)", // LUT
        "Matrix Files (*.mtx; *.mat)", // MATRIX
        "Medical (*.dcm; *.xml, *ima; *.img; *.mnc; *.sig; *.head; *.nii)", // TECH
        "Microscopy (*.avi; *.ics; *.lsm; *.pic; *.stk; *.tif; *.tiff; *.xml)", // MICROSCOPY
        "Misc.(*.avi; *.bmp; *.pcx; *.png; *.tga; *.xbm; *.xpm)", // MISC
        "Optical (*.avi; *.bmp; *.img; *.jpg; *.pict; *.psd; *.tif; *.tiff; *.xml)", // OPTICAL
        "Project (*.xml)", // PROJECT
        "Script Files (*.sct)", // SCRIPT
        "Surface Files (*.sur; *.wrl; *.xml)", // SURFACE
        "Transfer Function Files (*.fun)", // FUNCT
        "VOI Files (*.voi)", // VOI
        "Nonlinear Transformation Files (*.nlt)",
        "Dynamic",
        "Plugin Files"}; // NLT

    /** short description strings for each filterType */
    // note that the order must match the order of filterType definitions above!!
    private static String[] shortDescriptions = {
        "All", // ALL
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
        "Project", // PROJECT
        "Script Files", // SCRIPT
        "Surface Files", // SURFACE
        "Transfer Function Files", // FUNCT
        "VOI Files", // VOI
        "Nonlinear Transformation Files", // NLT
        "Dynamic",
        "Plugin"};

    /** Filter type of this filter. */
    private int filterType;

    /** Extensions to look at for the dynamic filter. */
    private String[] dynamicExts = null;

    /** Description of dynamic filter. */
    private String dynamicDescription = null;

    /**
     *	Constructs new filter of given type.
     *	@param filterType	Type of filter (GEN, TECH, etc.)
     */
    public ViewImageFileFilter(int filterType) {
        this.filterType = filterType;
    }

    /**
     *   Constructs a dynamic filter, with the given extensions.
     *   @param exts     Extensions to accept for dynamic filter.
     */
    public ViewImageFileFilter(String[] exts) {
        this(DYNAMIC);
        dynamicExts = exts;
        dynamicDescription = "Specific files (";
        for (int i = 0; i < exts.length; i++) {
            dynamicDescription += "*." + exts[i];
            if (i < exts.length - 1)
                dynamicDescription += "; ";
            else
                dynamicDescription += ")";
        }
    }

    /**
     *	One of the overrides to FileFilter.  This function is called for
     *	each file in the current directory of the file dialog to test if it
     *	is a file with the appropriate extension.  If so, it is accepted for
     *	display in the file dialog.
     *
     *	@param f The file whose name is to be tested for extension.
     *	@return <code>true</code> if and only if the file has an appropriate extension.
     */
    public boolean accept(File f) {
        if (f.isDirectory()) {
            return true;
        }

        String extension = getExtension(f);
        return accept(extension);
    }

    /**
     *   Checks if extension matches what is accepted for the filter type.
     *   If so, returns <code>true</code>; if not, returns <code>false</code>.
     *   @param extension    Extension of file to check.
     *   @return             <code>true</code> if extension is in file filter.
     */
    public boolean accept(String extension) {
        if (filterType == ALL) {
            return true;
        }

        if (filterType == MISC && ( (extension.equals("bmp")) || (extension.equals("pcx"))
                                   || (extension.equals("png")) || (extension.equals("tga"))
                                   || (extension.equals("xbm")) || (extension.equals("xpm"))
                                   || (extension.equals("avi")))) {
            return true;
        }

        else if (filterType == GEN && ( (extension.equals("gif"))
                                       || (extension.equals("jpeg"))
                                       || (extension.equals("jpg"))
                                       || (extension.equals("pict"))
                                       || (extension.equals("psd"))
                                       || (extension.equals("tif"))
                                       || (extension.equals("tiff")))) {
            return true;
        }

        else if (filterType == TECH && ( (extension.equals("img"))     || (extension.equals("ima"))
                                         || (extension.equals("dcm"))  || (extension.equals("mnc"))
                                         || (extension.equals("sig"))  || (extension.equals("xml"))
                                         || (extension.equals("head")) || (extension.equals("nii")))) {
            return true;
        }

        else if (filterType == VOI && ( (extension.equals("voi")) ||
                                       (extension.equals("oly")))) {
            return true;
        }

        else if (filterType == FUNCT && ( (extension.equals("fun")))) {
            return true;
        }

        else if (filterType == LUT && ( (extension.equals("lut")))) {
            return true;
        }

        else if (filterType == PLOT && ( (extension.equals("plt")))) {
            return true;
        }
        else if (filterType == MATRIX && ( (extension.equals("mtx"))
                                          || (extension.equals("mat")))) {
            return true;
        }
        else if (filterType == CLASS && ( (extension.equals("class")))) {
            return true;
        }
        else if (filterType == SCRIPT && ( (extension.equals("sct")))) {
            return true;
        }
        else if (filterType == SURFACE && ( (extension.equals("sur")) ||
                                            (extension.equals("wrl")) ||
                                            (extension.equals("xml")))) {
            return true;
        }

        else if (filterType == OPTICAL && ( (extension.equals("avi"))
                                           || (extension.equals("xml"))
                                           || (extension.equals("bmp"))
                                           || (extension.equals("img"))
                                           || (extension.equals("jpeg"))
                                           || (extension.equals("jpg"))
                                           || (extension.equals("pict"))
                                           || (extension.equals("psd"))
                                           || (extension.equals("tif"))
                                           || (extension.equals("tiff")))) {
            return true;
        }

        else if (filterType == FREESURFER && (extension.equals("asc"))) {
            return true;
        }
        else if (filterType == AVI && (extension.equals("avi"))) {
            return true;
        }
        else if (filterType == PROJECT && (extension.equals("xml"))) {
            return true;
        }
        else if (filterType == MICROSCOPY && ( (extension.equals("avi"))
                                              || (extension.equals("ics"))
                                              || (extension.equals("lsm"))
                                              || (extension.equals("pic"))
                                              || (extension.equals("stk"))
                                              || (extension.equals("tif"))
                                              || (extension.equals("tiff"))
                                              || (extension.equals("xml")))) {
            return true;
        }
        else if (filterType == NLT && (extension.equals("nlt"))) {
            return true;
        }
        else if (filterType == PLUGIN && ( (extension.equals("class"))
                                           || (extension.equals("jar"))
                                           || (extension.equals("zip")))) {
            return true;
        }
        else if (filterType == DYNAMIC) {
            for (int i = 0; i < dynamicExts.length; i++) {
                if (extension.equals(dynamicExts[i]))
                    return true;
            }
        }
        return false;

    }

    /**
     *	One of the overrides to FileFilter.  This function is called just
     *	to access the description of the filter.
     *	@return The description for the filter.
     */
    public String getDescription() {
        // if dynamic, return dynamic description
        if (filterType == DYNAMIC)
            return dynamicDescription;
        // make sure filter type is valid
        if (filterType < 0 || filterType >= descriptions.length)
            return " ";

        return descriptions[filterType];

    }

    /**
     *	Parses the name of the file and returns the extension.
     *	@param f	File.
     *	@return		The extension of the file.
     */
    private String getExtension(File f) {
        String s = f.getName();
        int i = s.lastIndexOf('.');
        if (i >= 0 && i < s.length() - 1)
            return s.substring(i + 1).toLowerCase();
        return " ";
    }

    /**
     *	Determines if this file filter is equal (in value) to another.
     *
     *	@param filter   The filter to be tested for equality
     *	@return <code>true</code> if and only if the filter is the same type as this filter.
     */
    public boolean equals(javax.swing.filechooser.FileFilter filter) {

        // if filter is not a ViewImageFileFilter, then it doesn't match
        if (! (filter instanceof ViewImageFileFilter))
            return false;

        if (this.filterType == ( (ViewImageFileFilter) filter).filterType)
            return true;

        else
            return false;

    } // end equals()

    /**
     *	Determines if the given file filter's description matches the description
     *   associated with the given filterType.
     *
     *	@param filter       The filter to be tested
     *   @param filterType   The filter type to test for a match
     *	@return <code>true</code> if and only if the filter's description matches the filterType
     *                               description.
     */
    public static boolean matches(javax.swing.filechooser.FileFilter filter, int filterType) {

        String desc = filter.getDescription();

        // if desc is null, then it can't match
        if (desc == null)
            return false;

        // make sure that filterType is valid
        if (filterType < 0 || filterType >= descriptions.length)
            return false;

        // if descriptions match then return true
        if (desc.equalsIgnoreCase(descriptions[filterType]))
            return true;

        // otherwise, no match
        return false;

    } // end matches()

    /**
     *   Returns the description of the given file filter.
     *   @param filter   Filter to check.
     *   @return         Description of filter.
     */
    public static String getDescription(int filter) {
        // make sure filter type is valid
        if (filter < 0 || filter >= descriptions.length)
            return " ";

        return descriptions[filter];
    }

    /**
     *   Returns the dshort escription of the given file filter.
     *   @param filter   Filter to check.
     *   @return         Short description of filter.
     */
    public static String getShortDescription(int filter) {
        // make sure filter type is valid
        if (filter < 0 || filter >= shortDescriptions.length)
            return " ";

        return shortDescriptions[filter];
    }

    public static int getFilterIndex(String filter) {
        for (int i = 0; i < descriptions.length; i++) {
            if (filter.equalsIgnoreCase(descriptions[i])) {
                return i;
            }
        }
        return 0;
    }

    /**
     *   Returns the list of descriptions.
     *   @return The list of descriptions.
     */
    public static String[] getDescriptions() {
        return descriptions;
    }

    /**
     *   Returns a list of the files in the directory
     *   that satisfy the file filter.
     *   @param file Directory
     *   @return     List of files in directory that satisfy file filter.
     */
    public String[] listFiles(File file) {
        File[] allFiles = file.listFiles();
        int count = 0;
        for (int i = 0; i < allFiles.length; i++) {
            if (!allFiles[i].isDirectory() && accept(allFiles[i]))
                count++;
        }

        String[] files = new String[count];
        count = 0;
        for (int i = 0; i < allFiles.length; i++) {
            if (!allFiles[i].isDirectory() && accept(allFiles[i])) {
                files[count] = allFiles[i].toString();
                count++;
            }
        }
        return files;
    }

}
