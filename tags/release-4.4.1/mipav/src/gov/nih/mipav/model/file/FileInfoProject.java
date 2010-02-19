package gov.nih.mipav.model.file;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.*;

import java.awt.*;

import java.io.*;

import java.util.*;

import javax.swing.*;


/**
 * This structure contains the information about a project including preferences and an image list (code based off of
 * FileInfoXML).
 *
 * @version  0.1 June 12, 2003
 * @author   Evan McCreedy
 * @see      FileIO
 * @see      FileProject
 */
public class FileInfoProject extends FileInfoBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -8242251068336660670L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Used to keep track of the current image. */
    private String currentImageFileName;

    /** Used to keep track of the current parameter name. */
    private String currentPName;

    /** String for the current PSet description (for key). */
    private String currentPSetDesc;

    /** Type of Data. */
    private int dataType;

    /** List of images attached to this project. */
    private Hashtable images;

    /** DOCUMENT ME! */
    private boolean[] invest;

    /** Investigators (up to 3). Optional XML tag. */
    private Investigator[] investigators;

    /** Brief description of the image. */
    private String projectDescription;

    /** Name (not path) of the project file. */
    private String projectFileName;

    /** Flag to indicate whether the project info has been changed and should be saved. */
    private boolean saveNeededFlag;

    /** Hashtable for holding sets of parameters. */
    private Hashtable setTable;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new FileInfoProject object.
     *
     * @param  name       file name
     * @param  directory  file directory
     * @param  format     file format (RAW)
     */
    public FileInfoProject(String name, String directory, int format) {
        super(name, directory, format);
        setTable = new Hashtable();
        images = new Hashtable();
        investigators = new Investigator[3];
        invest = new boolean[3];
        invest[0] = false;
        invest[1] = false;
        invest[2] = false;
        currentPSetDesc = new String("");
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Brings up a dialog containing the parameters and attached images for a project.
     *
     * @param  parent  the parent frame of the project dialog
     */
    public final void aboutProject(Frame parent) {
        JDialogFileInfoProject dialog = new JDialogFileInfoProject(parent, "Project information");
        displayAboutInfo(dialog);
    }

    /**
     * Adds an image to this project.
     *
     * @param  file  image file name
     */
    public void addImage(String file) {
        currentImageFileName = file;
        images.put(file, new ProjectImage(file));
    }

    /**
     * Adds (or updates) a parameter set (with parameters) to the information about the project. For any i &lt;
     * paramNames.size() there should be an entry in the other 3 parameter Vectors.
     *
     * @param  setDesc          description of the parameter set
     * @param  paramNames       list of parameter names
     * @param  paramDescs       list of parameter descriptions
     * @param  paramValueTypes  list of parameter value types
     * @param  paramValues      list of parameter values
     */
    public final void addPSet(String setDesc, Vector paramNames, Vector paramDescs, Vector paramValueTypes,
                              Vector paramValues) {
        Hashtable pset = getPSetHashtable();

        if (pset.containsKey(setDesc)) {

            // update pset
            for (int i = 0; i < paramNames.size(); i++) {
                String name = (String) paramNames.elementAt(i);
                String desc = (String) paramDescs.elementAt(i);
                String valType = (String) paramValueTypes.elementAt(i);
                String val = (String) paramValues.elementAt(i);

                if (getPSet(setDesc).containsKey(name)) {
                    getPSet(setDesc).getParameter(name).setValue(val);
                } else {
                    getPSet(setDesc).addParameter(name);
                    getPSet(setDesc).getParameter(name).setDescription(desc);
                    getPSet(setDesc).getParameter(name).setValueType(valType);
                    getPSet(setDesc).getParameter(name).setValue(val);
                }
            }
        } else {

            // create the pset and all the parameters
            createPSet(setDesc);

            for (int i = 0; i < paramNames.size(); i++) {
                String name = (String) paramNames.elementAt(i);
                String desc = (String) paramDescs.elementAt(i);
                String valType = (String) paramValueTypes.elementAt(i);
                String val = (String) paramValues.elementAt(i);

                getPSet(setDesc).addParameter(name);
                getPSet(setDesc).getParameter(name).setDescription(desc);
                getPSet(setDesc).getParameter(name).setValueType(valType);
                getPSet(setDesc).getParameter(name).setValue(val);
            }
        }
    }

    /**
     * Appends a string to the project description.
     *
     * @param  appendDescription  a brief description of the project to be appended.
     */
    public void appendProjectDescription(String appendDescription) {
        projectDescription = projectDescription + appendDescription;
    }

    /**
     * Creates a new parameter set with a description.
     *
     * @param  description  descrition of new parameter set
     */
    public void createPSet(String description) {
        this.currentPSetDesc = description;
        setTable.put(description, new PSet(description));
    }

    /**
     * Displays the file information.
     *
     * @param  dlog  dialog box that is written to
     */
    public void displayAboutInfo(JDialogBase dlog) {

        JDialogFileInfoProject dialog = (JDialogFileInfoProject) dlog;
        int i;
        int[] editorChoice = new int[1];

        editorChoice[0] = JDialogEditor.STRING;

        dialog.displayAboutInfo(this); // setup layout in the dialog

        editorChoice[0] = JDialogEditor.STRING;

        if (this.projectDescription != null) {
            dialog.appendPrimaryData("Description", this.projectDescription, editorChoice);
        } else {
            dialog.appendPrimaryData("Description", "", editorChoice);
        }

        String emptyString = new String("");

        // Investigator Data
        for (int j = 0; j < 3; j++) {
            editorChoice[0] = JDialogEditor.STRING;

            boolean isNull = (investigators[j] == null);

            // name is required if investigator data exists.
            // everything else is optional
            if (!isNull) {
                dialog.appendInvestigatorData((j + 1) + " Name", investigators[j].getName(), editorChoice);
            } else {
                dialog.appendInvestigatorData((j + 1) + " Name", emptyString, editorChoice);
            }

            if (!isNull && (investigators[j].getTitle() != null)) {
                dialog.appendInvestigatorData((j + 1) + " Title", investigators[j].getTitle(), editorChoice);
            } else {
                dialog.appendInvestigatorData((j + 1) + " Title", emptyString, editorChoice);
            }

            if (!isNull && (investigators[j].getAffiliation() != null)) {
                dialog.appendInvestigatorData((j + 1) + " Affiliation", investigators[j].getAffiliation(),
                                              editorChoice);
            } else {
                dialog.appendInvestigatorData((j + 1) + " Affiliation", emptyString, editorChoice);
            }

            if (!isNull && (investigators[j].getEmail() != null)) {
                dialog.appendInvestigatorData((j + 1) + " Email", investigators[j].getEmail(), editorChoice);
            } else {
                dialog.appendInvestigatorData((j + 1) + " Email", emptyString, editorChoice);
            }

            if (!isNull && (investigators[j].getPhone() != null)) {
                dialog.appendInvestigatorData((j + 1) + " Phone", investigators[j].getPhone(), editorChoice);
            } else {
                dialog.appendInvestigatorData((j + 1) + " Phone", emptyString, editorChoice);
            }

        }

        Enumeration e = getPSetKeys();

        while (e.hasMoreElements()) {
            PSet temp = getPSet((String) e.nextElement());
            String desc = temp.getDescription();
            Enumeration pe = temp.getParameterKeys();

            while (pe.hasMoreElements()) {
                Parameter tp = temp.getParameter((String) pe.nextElement());

                dialog.appendParameter(desc, tp.getName(), tp.getDescription(), tp.getValueType(), tp.getValue(),
                                       tp.getDate(), tp.getTime());
            }
        }

        Enumeration ie = getImageKeys();

        while (ie.hasMoreElements()) {
            ProjectImage temp = getImage((String) ie.nextElement());
            String file = temp.getFileName();
            Enumeration iie = temp.getInfoKeys();

            while (iie.hasMoreElements()) {
                ImageInfo ii = temp.getInfo((String) iie.nextElement());

                dialog.appendInfo(file, ii.getName(), ii.getDescription(), ii.getValueType(), ii.getValue());
            }
        }

        dialog.setVisible(true);
    }

    /**
     * empty placeholder because abstract method in FileInfoBase.
     *
     * @param  dlog  DOCUMENT ME!
     * @param  t     DOCUMENT ME!
     */
    public void displayAboutInfo(JDialogBase dlog, TransMatrix t) { }

    /**
     * Prepares this class for cleanup.
     */
    public void finalize() {

        if (setTable != null) {
            setTable.clear();
        }

        setTable = null;
        investigators = null;
        invest = null;
        super.finalize();
    }

    /**
     * Returns the current image.
     *
     * @return  current image
     */
    public ProjectImage getCurrentImage() {
        return (ProjectImage) images.get(currentImageFileName);
    }

    /**
     * Gets the current parameter set.
     *
     * @return  current parameter set
     */
    public PSet getCurrentPSet() {
        return (PSet) setTable.get(currentPSetDesc);
    }

    /**
     * Gets an image from the hashtable using a file name.
     *
     * @param   file  image file name
     *
     * @return  DOCUMENT ME!
     */
    public ProjectImage getImage(String file) {
        return (ProjectImage) images.get(file);
    }

    /**
     * Gets the info item table associated with an image with the given file name.
     *
     * @param   file  name
     *
     * @return  image infomation hashtable
     */
    public Hashtable getImageInfoTable(String file) {
        return ((ProjectImage) images.get(file)).getTable();
    }

    /**
     * Gets an enumeration for the list of image file names.
     *
     * @return  keys for image hashtable (file names)
     */
    public Enumeration getImageKeys() {
        return images.keys();
    }

    /**
     * Gets the images in the project.
     *
     * @return  list images
     */
    public Hashtable getImages() {
        return images;
    }

    /**
     * Get an investigator using an index.
     *
     * @param   index  DOCUMENT ME!
     *
     * @return  investigator
     */
    public Investigator getInvestigator(int index) {

        if (index < 3) {
            return investigators[index];
        } else {
            return null;
        }
    }

    /**
     * Gets an array (3) of Investigator data.
     *
     * @return  array of investigator data
     */
    public Investigator[] getInvestigators() {
        return this.investigators;
    }

    /**
     * Get an array (3) of booleans that tell whether the investigator data for that index is complete.
     *
     * @return  boolean array of investigator completeness
     */
    public boolean[] getInvestigatorsComplete() {
        return this.invest;
    }

    /**
     * Gets the # of images in the project.
     *
     * @return  number of images
     */
    public int getNumImages() {
        return images.size();
    }

    /**
     * Gets the # of parameter sets stored.
     *
     * @return  number of parameter sets
     */
    public int getNumPSets() {
        return setTable.size();
    }

    /**
     * Gets the value of a parameter set in the project information (use isSaveNeeded() to determine if a default was
     * used).
     *
     * @param   setDesc     parameter set the parameter should be in
     * @param   paramName   name of the parameter to retreive
     * @param   defaultVal  value to return if parameter is not found or is invalid
     *
     * @return  the retrieved parameter
     */
    public final Object getParameter(String setDesc, String paramName, Object defaultVal) {
        FileInfoProject.Parameter param;
        FileInfoProject.PSet set;

        set = getPSet(setDesc);

        if (set != null) {
            param = set.getParameter(paramName);

            if ((param != null) && !param.getValue().equals("")) {
                String val = param.getValue();
                String valType = param.getValueType();

                if (valType.equalsIgnoreCase("int")) {

                    try {
                        return Integer.valueOf(val);
                    } catch (NumberFormatException nfe) {
                        Preferences.debug("Error trying to parse int: " + setDesc + "->" + paramName + "\n");
                        setSaveNeeded(true);

                        return defaultVal;
                    }
                } else if (valType.equalsIgnoreCase("float")) {

                    try {
                        return Float.valueOf(val);
                    } catch (NumberFormatException nfe) {
                        Preferences.debug("Error trying to parse float: " + setDesc + "->" + paramName + "\n");
                        setSaveNeeded(true);

                        return defaultVal;
                    }
                } else if (valType.equalsIgnoreCase("string")) {
                    return val;
                } else if (valType.equalsIgnoreCase("boolean")) {
                    return Boolean.valueOf(val);
                } else if (valType.equalsIgnoreCase("double")) {

                    try {
                        return Double.valueOf(val);
                    } catch (NumberFormatException nfe) {
                        Preferences.debug("Error trying to parse double: " + setDesc + "->" + paramName + "\n");
                        setSaveNeeded(true);

                        return defaultVal;
                    }
                } else if (valType.equalsIgnoreCase("long")) {

                    try {
                        return Long.valueOf(val);
                    } catch (NumberFormatException nfe) {
                        Preferences.debug("Error trying to parse long: " + setDesc + "->" + paramName + "\n");
                        setSaveNeeded(true);

                        return defaultVal;
                    }
                } else if (valType.equalsIgnoreCase("ushort")) {

                    try {
                        return Short.valueOf(val);
                    } catch (NumberFormatException nfe) {
                        Preferences.debug("Error trying to parse ushort: " + setDesc + "->" + paramName + "\n");
                        setSaveNeeded(true);

                        return defaultVal;
                    }
                } else if (valType.equalsIgnoreCase("short")) {

                    try {
                        return Short.valueOf(val);
                    } catch (NumberFormatException nfe) {
                        Preferences.debug("Error trying to parse short: " + setDesc + "->" + paramName + "\n");
                        setSaveNeeded(true);

                        return defaultVal;
                    }
                } else if (valType.equalsIgnoreCase("ubyte")) {

                    try {
                        return Byte.valueOf(val);
                    } catch (NumberFormatException nfe) {
                        Preferences.debug("Error trying to parse ubyte: " + setDesc + "->" + paramName + "\n");
                        setSaveNeeded(true);

                        return defaultVal;
                    }
                } else if (valType.equalsIgnoreCase("byte")) {

                    try {
                        return Byte.valueOf(val);
                    } catch (NumberFormatException nfe) {
                        Preferences.debug("Error trying to parse byte: " + setDesc + "->" + paramName + "\n");
                        setSaveNeeded(true);

                        return defaultVal;
                    }
                } else {
                    Preferences.debug("Unknown value type " + valType + " for " + setDesc + "->" + paramName + "\n");
                    setSaveNeeded(true);

                    return defaultVal;
                }
            } else {
                Preferences.debug("No value for " + setDesc + "->" + paramName + "\n");
                setSaveNeeded(true);

                return defaultVal;
            }
        } else {
            Preferences.debug("Parameter set not found: " + setDesc + "\n");
            setSaveNeeded(true);

            return defaultVal;
        }
    }

    /**
     * Gets the parameter table associated with a Parameter set with the given description.
     *
     * @param   description  parameter set description
     *
     * @return  parameter hashtable
     */
    public Hashtable getParameterTable(String description) {
        return ((PSet) setTable.get(description)).getTable();
    }

    /**
     * Returns description of the project.
     *
     * @return  a brief description of the project.
     */
    public String getProjectDescription() {
        return projectDescription;
    }

    /**
     * Returns the name of the project file.
     *
     * @return  name of the project file
     */
    public String getProjectFileName() {
        return projectFileName;
    }

    /**
     * Gets a parameter set from the hashtable using a description.
     *
     * @param   description  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public PSet getPSet(String description) {
        return (PSet) setTable.get(description);
    }

    /**
     * Gets the hashtable that holds all sets of parameters.
     *
     * @return  set hashtable
     */
    public Hashtable getPSetHashtable() {
        return this.setTable;
    }

    /**
     * Gets an enumeration for the list of parameter set keys.
     *
     * @return  keys for parameter set hashtable
     */
    public Enumeration getPSetKeys() {
        return setTable.keys();
    }

    /**
     * Updates the FileInfo with new image info contained within the vector passed in [0] is the file name of the image
     * to be changed [1] is the name of the image info item [2] is the description of the image info item [3] is the
     * value type of the image info item [4] is the value of the image info item.
     *
     * @param  pData  Vector of changed image info data + image file name
     */
    public void infoChanged(Vector pData) {
        String fileName = (String) pData.elementAt(0);
        String name = (String) pData.elementAt(1);
        String desc = (String) pData.elementAt(2);
        String vt = (String) pData.elementAt(3);
        String val = (String) pData.elementAt(4);

        getImage(fileName).getInfo(name).setDescription(desc);
        getImage(fileName).getInfo(name).setValueType(vt);
        getImage(fileName).getInfo(name).setValue(val);
    }

    /**
     * Get whether the project info should be saved.
     *
     * @return  if the project info has been altered
     */
    public boolean isSaveNeeded() {
        return saveNeededFlag;
    }

    /**
     * Updates the FileInfo with new parameter data contained within the vector passed in [0] is the set description,
     * [1] is the name of the parameter to be changed [2] is the description of the parameter [3] is the value type of
     * the parameter [4] is the date for the parameter [5] is the time for the parameter.
     *
     * @param  pData  Vector of changed parameter data + set description
     */
    public void parameterChanged(Vector pData) {
        String setDesc = (String) pData.elementAt(0);
        String name = (String) pData.elementAt(1);
        String paramdesc = (String) pData.elementAt(2);
        String vt = (String) pData.elementAt(3);
        String val = (String) pData.elementAt(4);
        String date = (String) pData.elementAt(5);
        String time = (String) pData.elementAt(6);

        getPSet(setDesc).getParameter(name).setDescription(paramdesc);
        getPSet(setDesc).getParameter(name).setValueType(vt);
        getPSet(setDesc).getParameter(name).setValue(val);
        getPSet(setDesc).getParameter(name).setDate(date);
        getPSet(setDesc).getParameter(name).setTime(time);
    }

    /**
     * Removes an image from this project.
     *
     * @param  file  image file name
     */
    public void removeImage(String file) {
        images.remove(file);
    }

    /**
     * Removes a specific Parameter Set using it's description.
     *
     * @param  description  description of parameter set
     */
    public void removePSet(String description) {
        setTable.remove(description);
    }

    /**
     * Creates save dialog so that the project can be saved.
     *
     * @param  frame    DOCUMENT ME!
     * @param  options  the file writing options
     */
    public void saveProject(ViewJFrameBase frame, FileWriteOptions options) {
        ViewUserInterface userInterface = frame.getUserInterface();
        String fileName = null;
        String directory = null;
        String extension = null;
        String suffix = null;
        int fileType = FileUtility.PROJECT;
        int filterType = ViewImageFileFilter.PROJECT;
        ViewImageFileFilter vFilter = null;
        int i;

        if (options.isSaveAs()) {

            if (options.isSet()) {
                fileName = options.getFileName();
                directory = options.getFileDirectory();
            } else {

                try {
                    JFileChooser chooser = new JFileChooser();

                    if (userInterface.getDefaultDirectory() != null) {
                        chooser.setCurrentDirectory(new File(userInterface.getDefaultDirectory()));
                    } else {
                        chooser.setCurrentDirectory(new File(System.getProperties().getProperty("user.dir")));
                    }

                    chooser.addChoosableFileFilter(new ViewImageFileFilter(filterType));

                    int returnVal = chooser.showSaveDialog(frame);

                    if (returnVal == JFileChooser.APPROVE_OPTION) {
                        fileName = chooser.getSelectedFile().getName();

                        if (filterType >= 0) {
                            i = fileName.lastIndexOf('.');

                            if ((i > 0) && (i < (fileName.length() - 1))) {
                                extension = fileName.substring(i + 1).toLowerCase();
                                vFilter = new ViewImageFileFilter(filterType);

                                if (!vFilter.accept(extension)) {
                                    MipavUtil.displayError("Extension does not match filter type");

                                    return;
                                }
                            } // if ( i > 0 && i < fileName.length() - 1 )
                            else if (i < 0) {
                                fileName = fileName + ".xml";
                            } // else if (i < 0)
                        } // if (filterType >= 0)

                        directory = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;
                        userInterface.setDefaultDirectory(directory);
                        options.setFileName(fileName);
                        options.setFileDirectory(directory + File.separator);
                    } else {
                        return;
                    }
                } catch (OutOfMemoryError error) {
                    MipavUtil.displayError("Out of memory: ViewJFrameBase.save");
                    Preferences.debug("Out of memory: ViewJFrameBase.save\n", 3);

                    return;
                }
            }
        } else {
            options.setFileName(getFileName());
            options.setFileDirectory(getFileDirectory() + File.separator);
        }

        FileIO fileIO = new FileIO();

        fileIO.writeProject(this, options);
    }

    /**
     * Set the investigator's affiliation using an index.
     *
     * @param  affiliation  Investigator Affiliation
     * @param  num          index for investigator array
     */
    public void setAffiliation(String affiliation, int num) {
        investigators[num - 1].setAffiliation(affiliation);
    }

    /**
     * Set the investigator's email using an index.
     *
     * @param  email  Investigator Email
     * @param  num    index for investigator array
     */
    public void setEmail(String email, int num) {
        investigators[num - 1].setEmail(email);
    }

    /**
     * Sets the images list for the project.
     *
     * @param  _images  list of images
     */
    public void setImages(Hashtable _images) {
        images = _images;
    }

    /**
     * Create a new investigator using his/her name.
     *
     * @param  investigatorName  Investigator Name
     * @param  num               investigator # (not index)
     */
    public void setInvestigatorName(String investigatorName, int num) {
        num--;

        if (num < 3) {
            investigators[num] = new Investigator(investigatorName);
            invest[num] = true;
        }
    }

    /**
     * Sets the array (3) of Investigator data.
     *
     * @param  investigators  array of investigator data
     */
    public void setInvestigators(Investigator[] investigators) {
        this.investigators = investigators;
    }

    /**
     * Sets the array (3) of booleans that tell whether the investigator data for that index is complete.
     *
     * @param  invest  boolean array of investigator completeness
     */
    public void setInvestigatorsComplete(boolean[] invest) {
        this.invest = invest;
    }

    /**
     * Set the investigators phone # using an index.
     *
     * @param  phone  investigator phone #
     * @param  num    index for investigator array
     */
    public void setPhone(String phone, int num) {
        investigators[num - 1].setPhone(phone);
    }

    /**
     * Sets the project description.
     *
     * @param  newDescription  a brief description of the project.
     */
    public void setProjectDescription(String newDescription) {
        projectDescription = newDescription;
    }

    /**
     * Sets the name of the project file.
     *
     * @param  fileName  project file name (not path)
     */
    public void setProjectFileName(String fileName) {
        projectFileName = fileName;
    }

    /**
     * Sets the hashtable that holds all sets of parameters.
     *
     * @param  setTable  parameter hashtable
     */
    public void setPSetHashtable(Hashtable setTable) {
        this.setTable = setTable;
    }

    /**
     * Set whether a save is needed.
     *
     * @param  flag  whether to save
     */
    public void setSaveNeeded(boolean flag) {
        saveNeededFlag = flag;
    }

    /**
     * Set the investigator's title using an index.
     *
     * @param  title  Investigator Title
     * @param  num    index for investigator array
     */
    public void setTitle(String title, int num) {
        investigators[num - 1].setTitle(title);
    }

    /**
     * Updates the fileinfo with changes made within the JDialogs.
     *
     * @param  ce  Vector of new data
     */
    public void stateChanged(Vector ce) {
        String tname = (String) ce.elementAt(2); // [t]able [name]
        Vector tcvalue = (Vector) ce.elementAt(3); // [t]able [c]ode [value]
        String tvalue = (String) ce.elementAt(4); // [t]able [value]

        if (tname.equalsIgnoreCase("Description")) {
            setProjectDescription(tvalue);
        } else if (tname.endsWith(" Name")) {
            int index = Integer.valueOf(tname.substring(0, 1)).intValue();

            setInvestigatorName(tvalue, index);
        } else if (tname.endsWith(" Title")) {
            int index = Integer.valueOf(tname.substring(0, 1)).intValue();

            setTitle(tvalue, index);
        } else if (tname.endsWith(" Affiliation")) {
            int index = Integer.valueOf(tname.substring(0, 1)).intValue();

            setAffiliation(tvalue, index);
        } else if (tname.endsWith(" Email")) {
            int index = Integer.valueOf(tname.substring(0, 1)).intValue();

            setEmail(tvalue, index);
        } else if (tname.endsWith(" Phone")) {
            int index = Integer.valueOf(tname.substring(0, 1)).intValue();

            setPhone(tvalue, index);
        } else {
            System.out.println("FileInfoXML.stateChanged: " + tvalue + " not yet supported!");
        }
    }

    /**
     * Used to propogate all fileInfoProject private variables to other fileinfos.
     *
     * @param  fInfo  fileinfo to be copied into
     */
    public void updateFileInfos(FileInfoProject fInfo) {

        if (this == fInfo) {
            return;
        }

        fInfo.setProjectDescription(this.getProjectDescription());
        fInfo.setInvestigators(this.getInvestigators());
        fInfo.setInvestigatorsComplete(this.getInvestigatorsComplete());
        fInfo.setPSetHashtable(this.getPSetHashtable());
        fInfo.setImages(this.getImages());
    }

    //~ Inner Classes --------------------------------------------------------------------------------------------------

    /**
     * Class to hold a piece of information about an image.
     */
    public class ImageInfo implements Serializable {

        /** Use serialVersionUID for interoperability. */
        private static final long serialVersionUID = 1057184447893258773L;

        /** DOCUMENT ME! */
        private String description;

        /** DOCUMENT ME! */
        private String name;

        /** DOCUMENT ME! */
        private String value;

        /** DOCUMENT ME! */
        private String valueType;

        /**
         * Creates a new ImageInfo object.
         *
         * @param  _name  DOCUMENT ME!
         */
        public ImageInfo(String _name) {
            name = _name;
        }

        /**
         * DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        public String getDescription() {
            return description;
        }

        /**
         * DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        public String getName() {
            return name;
        }

        /**
         * DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        public String getValue() {
            return value;
        }

        /**
         * DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        public String getValueType() {
            return valueType;
        }

        /**
         * DOCUMENT ME!
         *
         * @param  _description  DOCUMENT ME!
         */
        public void setDescription(String _description) {
            description = _description;
        }

        /**
         * DOCUMENT ME!
         *
         * @param  _name  DOCUMENT ME!
         */
        public void setName(String _name) {
            name = _name;
        }

        /**
         * DOCUMENT ME!
         *
         * @param  _value  DOCUMENT ME!
         */
        public void setValue(String _value) {
            value = _value;
        }

        /**
         * DOCUMENT ME!
         *
         * @param  _valueType  DOCUMENT ME!
         */
        public void setValueType(String _valueType) {
            valueType = _valueType;
        }
    }

    /**
     * Public class to hold several attributes for an investigator associated with the project (up to three per).
     */
    public class Investigator implements Serializable {

        /** Use serialVersionUID for interoperability. */
        private static final long serialVersionUID = -2326251180154056395L;

        /** DOCUMENT ME! */
        private String affiliation;

        /** DOCUMENT ME! */
        private String email;

        /** DOCUMENT ME! */
        private String name;

        /** DOCUMENT ME! */
        private String phone;

        /** DOCUMENT ME! */
        private String title;

        /**
         * Creates a new Investigator with given name.
         *
         * @param  name  DOCUMENT ME!
         */
        public Investigator(String name) {
            this.name = name;
        }

        /**
         * Gets affiliation.
         *
         * @return  affiliation
         */
        public String getAffiliation() {
            return affiliation;
        }

        /**
         * Gets email.
         *
         * @return  email
         */
        public String getEmail() {
            return email;
        }

        /**
         * Gets name.
         *
         * @return  name
         */
        public String getName() {
            return name;
        }

        /**
         * Gets phone number.
         *
         * @return  phone number
         */
        public String getPhone() {
            return phone;
        }

        /**
         * Gets title.
         *
         * @return  title
         */
        public String getTitle() {
            return title;
        }

        /**
         * Sets affiliation.
         *
         * @param  affiliation  DOCUMENT ME!
         */
        public void setAffiliation(String affiliation) {
            this.affiliation = affiliation;
        }

        /**
         * Sets email.
         *
         * @param  email  DOCUMENT ME!
         */
        public void setEmail(String email) {
            this.email = email;
        }

        /**
         * Sets phone number.
         *
         * @param  phone  number
         */
        public void setPhone(String phone) {
            this.phone = phone;
        }

        /**
         * Sets title.
         *
         * @param  title  DOCUMENT ME!
         */
        public void setTitle(String title) {
            this.title = title;
        }
    }


    /**
     * Public class to store information for a parameter associated with the project (infinite parameters allowed per).
     */
    public class Parameter implements Serializable {

        /** Use serialVersionUID for interoperability. */
        private static final long serialVersionUID = 7867185997827250638L;

        /** DOCUMENT ME! */
        private String date;

        /** DOCUMENT ME! */
        private String description;

        /** DOCUMENT ME! */
        private String name;

        /** DOCUMENT ME! */
        private String time;

        /** DOCUMENT ME! */
        private String value;

        /** DOCUMENT ME! */
        private String valueType;

        /**
         * Creates a new parameter by name.
         *
         * @param  name  parameter name
         */
        public Parameter(String name) {
            this.name = name;
        }

        /**
         * Gets the parameter's date.
         *
         * @return  date
         */
        public String getDate() {
            return this.date;
        }

        /**
         * Gets the parameter's description.
         *
         * @return  description
         */
        public String getDescription() {
            return this.description;
        }

        /**
         * Gets the parameter's name.
         *
         * @return  parameter name
         */
        public String getName() {
            return this.name;
        }

        /**
         * Gets the parameter's time.
         *
         * @return  time
         */
        public String getTime() {
            return this.time;
        }

        /**
         * Gets the parameter's value.
         *
         * @return  value
         */
        public String getValue() {
            return this.value;
        }

        /**
         * Gets the parameter's value-type.
         *
         * @return  value-type
         */
        public String getValueType() {
            return this.valueType;
        }

        /**
         * Sets the date for the parameter.
         *
         * @param  date  DOCUMENT ME!
         */
        public void setDate(String date) {
            this.date = date;
        }

        /**
         * Sets the date + T + time for the parameter.
         *
         * @param  dateTime  date-time
         */
        public void setDateTime(String dateTime) {
            StringTokenizer dt = new StringTokenizer(dateTime, "T");

            if (dt.hasMoreElements()) {
                date = dt.nextToken();
                time = dt.nextToken();
            }
        }

        /**
         * Sets the description for the parameter.
         *
         * @param  description  DOCUMENT ME!
         */
        public void setDescription(String description) {
            this.description = description;
        }

        /**
         * Sets the time for the parameter.
         *
         * @param  time  DOCUMENT ME!
         */
        public void setTime(String time) {
            this.time = time;
        }

        /**
         * Sets the value for the parameter.
         *
         * @param  value  value
         */
        public void setValue(String value) {
            this.value = value;
        }

        /**
         * Sets the value type for the parameter.
         *
         * @param  valueType  value type
         */
        public void setValueType(String valueType) {
            this.valueType = valueType;
        }
    }


    /**
     * Class to hold the image file name and a table of information about the image.
     */
    public class ProjectImage implements Serializable {

        /** Use serialVersionUID for interoperability. */
        private static final long serialVersionUID = -3382758798506490222L;

        /** DOCUMENT ME! */
        private String currentInfoName;

        /** DOCUMENT ME! */
        private String fileName;

        /** DOCUMENT ME! */
        private Hashtable infoTable;

        /**
         * Creates a new ProjectImage object.
         *
         * @param  file  DOCUMENT ME!
         */
        public ProjectImage(String file) {
            fileName = file;
            infoTable = new Hashtable();
        }

        /**
         * Adds a new image info to the set.
         *
         * @param  name  DOCUMENT ME!
         */
        public void addInfo(String name) {
            this.currentInfoName = name;
            infoTable.put(name, new ImageInfo(name));
        }

        /**
         * Determines if the image has a info item with the given name.
         *
         * @param   key  information name (key)
         *
         * @return  boolean contains parameter
         */
        public boolean containsKey(String key) {
            return infoTable.containsKey(key);
        }

        /**
         * Returns the current info item to be modified.
         *
         * @return  current info item
         */
        public ImageInfo getCurrentInfo() {
            return (ImageInfo) infoTable.get(currentInfoName);
        }

        /**
         * DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        public String getFileName() {
            return fileName;
        }

        /**
         * Gets the info item with the given name.
         *
         * @param   name  DOCUMENT ME!
         *
         * @return  info item
         */
        public ImageInfo getInfo(String name) {
            return (ImageInfo) infoTable.get(name);
        }

        /**
         * Get an enumeration for the list of info item names.
         *
         * @return  enumeration for info name list
         */
        public Enumeration getInfoKeys() {
            return infoTable.keys();
        }

        /**
         * Gets the hashtable of info items.
         *
         * @return  infomation hashtable
         */
        public Hashtable getTable() {
            return this.infoTable;
        }

        /**
         * Removes the info item with the given name from the hashtable.
         *
         * @param  name  info item name
         */
        public void removeInfo(String name) {
            infoTable.remove(name);
        }

        /**
         * DOCUMENT ME!
         *
         * @param  file  DOCUMENT ME!
         */
        public void setFileName(String file) {
            fileName = file;
        }
    }


    /**
     * Public class to store up to an infinite number of parameters... which will be in a Hashtable with name as the key
     * Note: there must be at least one parameter associated with each parameter set per XSD (XML Schema)
     */
    public class PSet implements Serializable {

        /** Use serialVersionUID for interoperability. */
        private static final long serialVersionUID = 524931106461981593L;

        /** DOCUMENT ME! */
        private String currentParameterName;

        /** DOCUMENT ME! */
        private String description;

        /** DOCUMENT ME! */
        private Hashtable parameterTable;

        /**
         * Create a new parameter set with the given description.
         *
         * @param  description  DOCUMENT ME!
         */
        public PSet(String description) {
            this.description = description;
            parameterTable = new Hashtable();
        }

        /**
         * Adds a new parameter to the set.
         *
         * @param  name  DOCUMENT ME!
         */
        public void addParameter(String name) {
            this.currentParameterName = name;
            parameterTable.put(name, new Parameter(name));
        }

        /**
         * Determines if the set contains a parameter with the given name.
         *
         * @param   key  parameter name (key)
         *
         * @return  true if the table has an entry for the key
         */
        public boolean containsKey(String key) {
            return parameterTable.containsKey(key);
        }

        /**
         * Returns the current parameter to be modified.
         *
         * @return  current parameter
         */
        public Parameter getCurrentParameter() {
            return (Parameter) parameterTable.get(currentParameterName);
        }

        /**
         * Get the parameter set description.
         *
         * @return  description
         */
        public String getDescription() {
            return this.description;
        }

        /**
         * Gets the parameter with the given name.
         *
         * @param   name  DOCUMENT ME!
         *
         * @return  parameter
         */
        public Parameter getParameter(String name) {
            return (Parameter) parameterTable.get(name);
        }

        /**
         * Get an enumeration for the list of parameter names.
         *
         * @return  enumeration for parameter name list
         */
        public Enumeration getParameterKeys() {
            return parameterTable.keys();
        }

        /**
         * Gets the hashtable of parameters.
         *
         * @return  parameter hashtable
         */
        public Hashtable getTable() {
            return this.parameterTable;
        }

        /**
         * Removes the parameter with the given name from the hashtable.
         *
         * @param  name  parameter name
         */
        public void removeParameter(String name) {
            parameterTable.remove(name);
        }
    }
}
