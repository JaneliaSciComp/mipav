package gov.nih.mipav.view.renderer.flythroughview;


import java.io.File;

import java.util.*;

import javax.swing.filechooser.FileFilter;


/**
 * A simple file filter to select only files with the specified extensions for display in a JFileChooser instance.
 */
public class FileFilterExt extends FileFilter {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** The description for this filter. */
    private String m_kDescription = new String();

    /** An array of possible extensions. */
    private Vector m_kExtensionsVector = new Vector();

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Create file filter given no initial extensions. Until at least one extension is added (see the addExtension
     * method), the accept method will always return false.
     *
     * @param  kDescription  String containing the filter description
     */
    public FileFilterExt(String kDescription) {
        setDescription(kDescription);
    }


    /**
     * Create file filter given the initial single extension.
     *
     * @param  kExtension    String containing the initial extension
     * @param  kDescription  String containing the filter description
     */
    public FileFilterExt(String kExtension, String kDescription) {
        addExtension(kExtension);
    }

    /**
     * Create file filter given the initial array of extensions.
     *
     * @param  akExtensions  String array containing the initial extensions
     * @param  kDescription  String containing the filter description
     */
    public FileFilterExt(String[] akExtensions, String kDescription) {

        for (int iExt = 0; iExt < akExtensions.length; ++iExt) {
            addExtension(akExtensions[iExt]);
        }

        setDescription(kDescription);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * FileFilter override. This function is called for each file in the current directory of the file dialog to test if
     * it is a file with a valid extension. If so, it is accepted for display in the file dialog.
     *
     * @param   kFile  the file whose name is to be tested for extension
     *
     * @return  true if and only if the file has a valid extension
     */
    public boolean accept(File kFile) {

        if (null != kFile) {

            if (kFile.isDirectory()) {
                return true;
            }

            // Extract just the file name portion of path for this file.
            // Loop through the array of possible valid extensions to see if
            // this file name ends with that extension.
            String kName = kFile.getName();
            int i = kName.lastIndexOf('.');

            if ((i > 0) && (i < (kName.length() - 1))) {

                for (int iString = 0; iString < m_kExtensionsVector.size(); ++iString) {
                    String kExtension = (String) m_kExtensionsVector.get(iString);

                    if (kName.endsWith(kExtension)) {
                        return true;
                    }
                }
            }
        }

        return false;
    }

    /**
     * Add an extension to the list of possible extensions valid for this filter.
     *
     * @param  kExtension  String containing the valid file extension which is case-sensitive
     */
    public void addExtension(String kExtension) {
        m_kExtensionsVector.add(kExtension);
    }

    /**
     * FileFilter override. This function is called just to access the description of the filter.
     *
     * @return  the description for the possible files
     */
    public String getDescription() {
        return m_kDescription;
    }

    /**
     * Set the description of this filter as it should be returned by the getDescription override method.
     *
     * @param  kDescription  String containing the description
     */
    public void setDescription(String kDescription) {
        m_kDescription = kDescription;
    }
}
