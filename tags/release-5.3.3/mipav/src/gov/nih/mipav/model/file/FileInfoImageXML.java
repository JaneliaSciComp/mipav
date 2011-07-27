package gov.nih.mipav.model.file;


import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.model.structures.TransMatrix;

import gov.nih.mipav.view.dialogs.JDialogBase;
import gov.nih.mipav.view.dialogs.JDialogEditor;
import gov.nih.mipav.view.dialogs.JDialogFileInfoXML;

import java.io.Serializable;
import java.util.Enumeration;
import java.util.Hashtable;
import java.util.StringTokenizer;
import java.util.Vector;


/**
 * This structure contains the information that describes how an XML image is stored on disk.
 * 
 * <p>
 * Notes:
 * </p>
 * 
 * <ol>
 * <li>Should the matrix XML tag have a field to apply transformation matrix.</li>
 * <li>toString should be added near the end of development ? or in FileXML.</li>
 * </ol>
 * 
 * @version 0.1 Sept 19, 2002
 * @author Neva Cherniavsky
 * @author Matthew J. McAuliffe, Ph.D.
 * @see FileIO
 * @see FileInfoXML
 */
public class FileInfoImageXML extends FileInfoXML {

    // ~ Static fields/initializers
    // -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -6296437302928259711L;

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private String bodyPart;

    /** String for the current PSet description (for key). */
    private String currentPSetDesc;

    /** Used to keep track of the current surface. */
    private String currentSurfacePath;

    /** Used to keep track of the current VOI. */
    private String currentVOIPath;

    /** DOCUMENT ME! */
    private String diagnosis;

    /** DOCUMENT ME! */
    private String DOB;

    /** Scan attributes. Optional XML tag. */
    private String equipmentName;

    /** DOCUMENT ME! */
    private int height = 0;

    /** DOCUMENT ME! */
    private boolean[] invest;

    /** Investigators (up to 3). Optional XML tag. */
    private Investigator[] investigators;

    /** linked image path (optional). */
    private String linkedImagePath;

    /** Transformation matrix. */
    private TransMatrix matrix;

    /** DOCUMENT ME! */
    private String protocol;

    /** DOCUMENT ME! */
    private String race;

    /** DOCUMENT ME! */
    private String scanDate;

    /** DOCUMENT ME! */
    private String scanID;

    /** DOCUMENT ME! */
    private String scanTime;

    /** Hashtable for holding sets of parameters. */
    private Hashtable<String,XMLPSet> setTable;

    /** DOCUMENT ME! */
    private String sex;

    /** DOCUMENT ME! */
    private String subjectID;

    /** Subject's information. */
    private String subjectName;

    /** List of files which describe surfaces attached to this image. */
    private Hashtable<String,SurfaceLink> surfaces;

    /** List of files which describe VOIs attached to this image. */
    private Hashtable<String,VOILink> VOIs;

    /** DOCUMENT ME! */
    private int weight = 0;

    /**
     * The image history. Used by the NDAR meta-information generation dialog to write out the name of the zip file
     * where the original data files are stored.
     */
    private String history;

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * Main constructor for FileInfoXML.
     * 
     * @param name String file name
     * @param directory String file directory
     * @param format int file format (data type)
     */
    public FileInfoImageXML(String name, String directory, int format) {
        super(name, directory, format);
        setTable = new Hashtable<String,XMLPSet>();
        investigators = new Investigator[3];
        invest = new boolean[3];
        invest[0] = false;
        invest[1] = false;
        invest[2] = false;
        currentPSetDesc = new String("");
        VOIs = new Hashtable<String,VOILink>();
        currentVOIPath = new String("");
        surfaces = new Hashtable<String,SurfaceLink>();
        currentSurfacePath = new String("");
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * Adds a surface path to the image's surface list.
     * 
     * @param path String path to a surface file
     */
    public void addSurface(String path) {
        currentSurfacePath = path;
        surfaces.put(path, new SurfaceLink(path));
    }

    /**
     * Adds a voi path to the image's VOI list.
     * 
     * @param path String path to a VOI file
     */
    public void addVOI(String path) {
        currentVOIPath = path;
        VOIs.put(path, new VOILink(path));
    }

    /**
     * Creates a new parameter set with a description.
     * 
     * @param description String desription
     */
    public void createPSet(String description) {
        this.currentPSetDesc = description;
        setTable.put(description, new XMLPSet(description));
    }
    
    public void addPset(String description, XMLPSet pset) {
    	this.currentPSetDesc = description;
    	setTable.put(description, pset);
    }
    
    

    /**
     * Displays the file information.
     * 
     * @param dlog JDialogBase dialog box that is written to
     * @param matrix transformation matrix
     */
    @SuppressWarnings("unchecked")
    public void displayAboutInfo(JDialogBase dlog, TransMatrix matrix) {
        JDialogFileInfoXML dialog = (JDialogFileInfoXML) dlog;
        int[] extents;
        int i;
        int[] editorChoice = new int[1];

        editorChoice[0] = JDialogEditor.STRING;

        dialog.displayAboutInfo(this); // setup layout in the dialog

        editorChoice[0] = JDialogEditor.STRING;

        if (this.imageDescription != null) {
            dialog.appendPrimaryData("Description", this.imageDescription, editorChoice);
        } else {
            dialog.appendPrimaryData("Description", "", editorChoice);
        }

        editorChoice[0] = JDialogEditor.XML_LINKEDIMAGE;

        if (this.linkedImagePath != null) {
            dialog.appendPrimaryData("Linked-image", this.linkedImagePath, editorChoice);
        } else {
            dialog.appendPrimaryData("Linked-image", "", editorChoice);
        }

        dialog.appendPrimaryData("Image-offset", Integer.toString(getOffset()));

        extents = super.getExtents();

        for (i = 0; i < extents.length; i++) {
            dialog.appendPrimaryData("Dimension " + i, Integer.toString(extents[i]));
        }

        dialog.appendPrimaryData("Type", ModelStorageBase.getBufferTypeStr(getDataType()));

        if (ModelImage.isColorImage(getDataType())) {
            dialog.appendPrimaryData("Min red", Double.toString(getMinR()));
            dialog.appendPrimaryData("Max red", Double.toString(getMaxR()));
            dialog.appendPrimaryData("Min green", Double.toString(getMinG()));
            dialog.appendPrimaryData("Max green", Double.toString(getMaxG()));
            dialog.appendPrimaryData("Min blue", Double.toString(getMinB()));
            dialog.appendPrimaryData("Max blue", Double.toString(getMaxB()));

        } else {
            dialog.appendPrimaryData("Min", Double.toString(getMin()));
            dialog.appendPrimaryData("Max", Double.toString(getMax()));
        }

        dialog.appendPrimaryData("Orientation", getImageOrientationStr(getImageOrientation()));

        dialog.appendPrimaryData("Axis X Orientation", getAxisOrientationStr(getAxisOrientation(0)));
        dialog.appendPrimaryData("Axis Y Orientation", getAxisOrientationStr(getAxisOrientation(1)));
        dialog.appendPrimaryData("Axis Z Orientation", getAxisOrientationStr(getAxisOrientation(2)));

        float[] resolutions; // = new float[5];

        resolutions = getResolutions();

        int[] measure; // = new int[5];

        measure = getUnitsOfMeasure();

        for (i = 0; i < extents.length; i++) {

            if (resolutions[i] > 0.0) {
                String pixelRes = "Pixel resolution " + i;

                dialog.appendPrimaryData(pixelRes, Float.toString(resolutions[i]) + " "
                        + (Unit.getUnitFromLegacyNum(measure[i])).toString());
            } // end of if (resolutions[i] > 0.0)
        } // for (i=0; i < 5; i++)

        if ( (extents.length > 2) && (extents[2] > 0)) {
            dialog.appendPrimaryData("Slice thickness ", Float.toString(getSliceThickness()) + " "
                    + (Unit.getUnitFromLegacyNum(measure[2])).toString());
        }

        float[] origin; // = new float[4];

        origin = getOrigin();

        for (i = 0; (i < extents.length) && (i < 4); i++) {
            String originStr = "Origin " + i;

            dialog.appendPrimaryData(originStr, Float.toString(origin[i]));
        } // for (i=0; i < 4; i++)

        if (getEndianess() == FileBase.LITTLE_ENDIAN) {
            dialog.appendPrimaryData("Endianess", "Little Endian");
        } else {
            dialog.appendPrimaryData("Endianess", "Big Endian");
        }

        if (matrix != null) {

            // when using displayAboutInfo(dialog) this doesn't appear
            // calling prg might use an editing panel to adjust this matrix

            // BEN: CHANGE the following...
            // dialog.appendPrimaryData("Transform ID", TransMatrix.getTransformIDStr(getTransformID()));
            dialog.appendPrimaryData("Matrix", matrix.matrixToString(10, 4));
        }

        editorChoice[0] = JDialogEditor.XML_MODALITY;
        dialog.appendPrimaryData("Modality", getModalityStr(getModality()), editorChoice);

        editorChoice[0] = JDialogEditor.STRING;
        dialog.appendPrimaryData("History", this.history, editorChoice);

        // now subject information
        String emptyString = new String("");

        editorChoice[0] = JDialogEditor.STRING;

        if (subjectName != null) {
            dialog.appendSubjectData("Subject Name", subjectName, editorChoice);
        } else {
            dialog.appendSubjectData("Subject Name", emptyString, editorChoice);
        }

        if (subjectID != null) {
            dialog.appendSubjectData("Subject ID", subjectID, editorChoice);
        } else {
            dialog.appendSubjectData("Subject ID", emptyString, editorChoice);
        }

        editorChoice[0] = JDialogEditor.XML_RACE;

        if (race != null) {
            dialog.appendSubjectData("Race", race, editorChoice);
        } else {
            dialog.appendSubjectData("Race", emptyString, editorChoice);
        }

        editorChoice[0] = JDialogEditor.STRING;

        if (diagnosis != null) {
            dialog.appendSubjectData("Diagnosis", diagnosis, editorChoice);
        } else {
            dialog.appendSubjectData("Diagnosis", emptyString, editorChoice);
        }

        editorChoice[0] = JDialogEditor.XML_DOB;

        if (DOB != null) {
            dialog.appendSubjectData("Date of Birth", DOB, editorChoice);
        } else {
            dialog.appendSubjectData("Date of Birth", "0000-01-01", editorChoice);
        }

        editorChoice[0] = JDialogEditor.STRING;
        dialog.appendSubjectData("Height", (new Integer(height)).toString(), editorChoice);
        dialog.appendSubjectData("Weight", (new Integer(weight)).toString(), editorChoice);
        editorChoice[0] = JDialogEditor.XML_SEX;

        if (sex != null) {
            dialog.appendSubjectData("Sex", sex, editorChoice);
        } else {
            dialog.appendSubjectData("Sex", "Unknown", editorChoice);
        }

        editorChoice[0] = JDialogEditor.STRING;

        if (bodyPart != null) {
            dialog.appendSubjectData("Body Part", bodyPart, editorChoice);
        } else {
            dialog.appendSubjectData("Body Part", emptyString, editorChoice);
        }

        // Scan Attributes
        editorChoice[0] = JDialogEditor.STRING;

        if (equipmentName != null) {
            dialog.appendScanData("Equipment Model Name", equipmentName, editorChoice);
        } else {
            dialog.appendScanData("Equipment Model Name", emptyString, editorChoice);
        }

        if (scanID != null) {
            dialog.appendScanData("Scan ID", scanID, editorChoice);
        } else {
            dialog.appendScanData("Scan ID", emptyString, editorChoice);
        }

        if (protocol != null) {
            dialog.appendScanData("Protocol", protocol, editorChoice);
        } else {
            dialog.appendScanData("Protocol", emptyString, editorChoice);
        }

        editorChoice[0] = JDialogEditor.XML_DATE;

        if (scanDate != null) {
            dialog.appendScanData("Scan Date", scanDate, editorChoice);
        } else {
            dialog.appendScanData("Scan Date", "0000-01-01", editorChoice);
        }

        editorChoice[0] = JDialogEditor.XML_TIME;

        if (scanTime != null) {
            dialog.appendScanData("Scan Time", scanTime, editorChoice);
        } else {
            dialog.appendScanData("Scan Time", "00:00:00-00:00", editorChoice);
        }

        // Investigator Data
        for (int j = 0; j < 3; j++) {
            editorChoice[0] = JDialogEditor.STRING;

            boolean isNull = (investigators[j] == null);

            // name is required if investigator data exists.
            // everything else is optional
            if ( !isNull) {
                dialog.appendInvestigatorData( (j + 1) + " Name", investigators[j].getName(), editorChoice);
            } else {
                dialog.appendInvestigatorData( (j + 1) + " Name", emptyString, editorChoice);
            }

            if ( !isNull && (investigators[j].getTitle() != null)) {
                dialog.appendInvestigatorData( (j + 1) + " Title", investigators[j].getTitle(), editorChoice);
            } else {
                dialog.appendInvestigatorData( (j + 1) + " Title", emptyString, editorChoice);
            }

            if ( !isNull && (investigators[j].getAffiliation() != null)) {
                dialog.appendInvestigatorData( (j + 1) + " Affiliation", investigators[j].getAffiliation(),
                        editorChoice);
            } else {
                dialog.appendInvestigatorData( (j + 1) + " Affiliation", emptyString, editorChoice);
            }

            if ( !isNull && (investigators[j].getEmail() != null)) {
                dialog.appendInvestigatorData( (j + 1) + " Email", investigators[j].getEmail(), editorChoice);
            } else {
                dialog.appendInvestigatorData( (j + 1) + " Email", emptyString, editorChoice);
            }

            if ( !isNull && (investigators[j].getPhone() != null)) {
                dialog.appendInvestigatorData( (j + 1) + " Phone", investigators[j].getPhone(), editorChoice);
            } else {
                dialog.appendInvestigatorData( (j + 1) + " Phone", emptyString, editorChoice);
            }

        }

        Enumeration<String> e = getPSetKeys();

        while (e.hasMoreElements()) {
            XMLPSet temp = getPSet(e.nextElement());
            String desc = temp.getDescription();
            Enumeration<String> pe = temp.getParameterKeys();

            while (pe.hasMoreElements()) {
                XMLParameter tp = temp.getParameter(pe.nextElement());
                editorChoice[0] = JDialogEditor.STRING;
                int loc = tp.getDescription().indexOf('[')+1;
                int value = Integer.parseInt(tp.getDescription().substring(loc, loc+1));
                if(value > 0)
                	dialog.appendTagData(tp.getDescription().substring(0, tp.getDescription().indexOf('[')), desc+" [" + (value+1)+"]" , tp.getValue(), editorChoice);
                else
                	dialog.appendTagData(tp.getDescription().substring(0, tp.getDescription().indexOf('[')), desc , tp.getValue(), editorChoice);
             //   dialog.appendParameter(desc, tp.getName(), tp.getDescription(), tp.getValueType(), tp.getValue(), tp
              //          .getDate(), tp.getTime());
                
            }
        }
    }

    /**
     * Prepares the class for cleanup.
     */
    public void finalize() {

        if (setTable != null) {
            setTable.clear();
        }

        setTable = null;
        matrix = null;
        investigators = null;
        invest = null;

        if (VOIs != null) {
            VOIs.clear();
        }

        VOIs = null;

        if (surfaces != null) {
            surfaces.clear();
        }

        surfaces = null;

        // if (thumbnail != null) {
        //
        // }
        super.finalize();
    }

    /**
     * Get subject's body part (optional).
     * 
     * @return bodyPart subject's body part
     */
    public String getBodyPart() {
        return this.bodyPart;
    }

    /**
     * Gets current parameter set.
     * 
     * @return PSet current parameter set
     */
    public XMLPSet getCurrentPSet() {
        return setTable.get(currentPSetDesc);
    }

    /**
     * Returns the SurfaceLink object for the current surface.
     * 
     * @return SurfaceLink surfacelink object
     */
    public SurfaceLink getCurrentSurface() {
        return surfaces.get(currentSurfacePath);
    }

    /**
     * Get's the VOILink for the current VOI.
     * 
     * @return VOILink current voi link
     */
    public VOILink getCurrentVOI() {
        return VOIs.get(currentVOIPath);
    }

    /**
     * Get diagnosis for subject (optional).
     * 
     * @return diagnosis subject's diagnosis
     */
    public String getDiagnosis() {
        return this.diagnosis;
    }

    /**
     * Get subject's date of birth (optional).
     * 
     * @return DOB date of birth
     */
    public String getDOB() {
        return this.DOB;
    }

    /**
     * Get the scan Equipment Model Name associated with image.
     * 
     * @return equipmentName equipment model name
     */
    public String getEquipmentName() {
        return this.equipmentName;
    }

    /**
     * Get subject's height (optional).
     * 
     * @return height subject's height
     */
    public int getHeight() {
        return this.height;
    }

    /**
     * Gets the specified investigator.
     * 
     * @param index int index of investigator
     * 
     * @return Investigator investigator
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
     * @return Investigator[] array of investigator data
     */
    public Investigator[] getInvestigators() {
        return this.investigators;
    }

    /**
     * Get an array (3) of booleans that tell whether the investigator data for that index is complete.
     * 
     * @return boolean[] array of investigator completeness
     */
    public boolean[] getInvestigatorsComplete() {
        return this.invest;
    }

    /**
     * Gets the full path + name of the linked image.
     * 
     * @return full path of the linked image
     */
    public String getLinkedImagePath() {
        return this.linkedImagePath;
    }

    /**
     * Gets the matrix associated with the FileInfo.
     * 
     * @return TransMatrix the image orientation matrix
     */
    public TransMatrix getMatrix() {
        return this.matrix;
    }

    /**
     * Gets the # of parameter sets stored.
     * 
     * @return int number of parameter sets
     */
    public int getNumPSets() {
        return setTable.size();
    }

    /**
     * Gets the number of surfaces attached to the image.
     * 
     * @return int number of surfaces
     */
    public int getNumSurfaces() {
        return surfaces.size();
    }

    /**
     * Get number of VOIs.
     * 
     * @return int number of VOIs
     */
    public int getNumVOIs() {
        return VOIs.size();
    }

    /**
     * Gets the parameter table associated with a Parameter set with the given description.
     * 
     * @param description String parameter set description
     * 
     * @return Hashtable parameter hashtable
     */
    public Hashtable<?,?> getParameterTable(String description) {
        return (setTable.get(description)).getTable();
    }

    /**
     * Get scan Protocol for equipment associated with image.
     * 
     * @return protocol protocol
     */
    public String getProtocol() {
        return this.protocol;
    }

    /**
     * Gets a parameter set from the hashtable using a description.
     * 
     * @param description PSet description of parameter set
     * 
     * @return DOCUMENT ME!
     */
    public XMLPSet getPSet(String description) {
        return setTable.get(description);
    }

    /**
     * Gets the hashtable that holds all sets of parameters.
     * 
     * @return Hashtable set hashtable
     */
    public Hashtable<String,XMLPSet> getPSetHashtable() {
        return this.setTable;
    }

    /**
     * Gets an enumeration for the list of parameter set keys.
     * 
     * @return Enumeration keys for parameter set hashtable
     */
    public Enumeration<String> getPSetKeys() {
        return setTable.keys();
    }

    /**
     * Gets the subject's race.
     * 
     * @return String subject's race
     */
    public String getRace() {
        return this.race;
    }

    /**
     * Get the date the image was taken.
     * 
     * @return scanDate scan date
     */
    public String getScanDate() {
        return this.scanDate;
    }

    /**
     * Gets the scan ID.
     * 
     * @return String scan ID
     */
    public String getScanID() {
        return this.scanID;
    }

    /**
     * Get the time the image was taken.
     * 
     * @return scanTime scan time
     */
    public String getScanTime() {
        return this.scanTime;
    }

    /**
     * Get subject's sex (optional).
     * 
     * @return sex subject's sex
     */
    public String getSex() {
        return this.sex;
    }

    /**
     * Get's the subject's ID.
     * 
     * @return String subject's ID
     */
    public String getSubjectID() {
        return this.subjectID;
    }

    /**
     * Get's the subject's name.
     * 
     * @return String subject's name
     */
    public String getSubjectName() {
        return this.subjectName;
    }

    /**
     * Returns a SurfaceLink to a surface attached to this image.
     * 
     * @param path String filepath to the surface to be returned
     * 
     * @return SurfaceLink Surfacelink for the given path
     */
    public SurfaceLink getSurface(String path) {
        return surfaces.get(path);
    }

    /**
     * Gets paths to surface files for this image.
     * 
     * @return Enumeration Enumeration of paths to surface files
     */
    public Enumeration<String> getSurfaceKeys() {
        return surfaces.keys();
    }

    /**
     * Returns the surface hashtable for this image.
     * 
     * @return Hashtable the surface hashtable
     */
    public Hashtable<String,SurfaceLink> getSurfaces() {
        return surfaces;
    }

    /**
     * Returns a VOILink to a VOI attached to this image.
     * 
     * @param path String filepath to the VOI to be returned
     * 
     * @return VOILink VOILink for the given path
     */
    public VOILink getVOI(String path) {
        return VOIs.get(path);
    }

    /**
     * Gets paths to VOI files for this image.
     * 
     * @return Enumeration Enumeration of paths to VOI files
     */
    public Enumeration<String> getVOIKeys() {
        return VOIs.keys();
    }

    /**
     * Returns the VOI hashtable for this image.
     * 
     * @return Hashtable VOI hashtable
     */
    public Hashtable<String,VOILink> getVOIs() {
        return VOIs;
    }

    /**
     * Gets the subject's weight.
     * 
     * @return int subject's weight
     */
    public int getWeight() {
        return this.weight;
    }

    /**
     * Gets the image history string. Used by the NDAR meta-information generation dialog to write out the name of the
     * zip file where the original data files are stored.
     * 
     * @return The image history string.
     */
    public String getHistory() {
        return this.history;
    }

    /**
     * Updates the FileInfo with new parameter data contained within the vector passed in [0] is the set description,
     * [1] is the name of the parameter to be changed [2] is the description of the parameter [3] is the value type of
     * the parameter [4] is the date for the parameter [5] is the time for the parameter.
     * 
     * @param pData Vector Vector of changed parameter data + set description
     */
    public void parameterChanged(Vector<String> pData) {
        String setDesc = pData.elementAt(0);
        String name = pData.elementAt(1);
        String paramdesc = pData.elementAt(2);
        String vt = pData.elementAt(3);
        String val = pData.elementAt(4);
        String date = pData.elementAt(5);
        String time = pData.elementAt(6);

        getPSet(setDesc).getParameter(name).setDescription(paramdesc);
        getPSet(setDesc).getParameter(name).setValueType(vt);
        getPSet(setDesc).getParameter(name).setValue(val);
        getPSet(setDesc).getParameter(name).setDate(date);
        getPSet(setDesc).getParameter(name).setTime(time);

    }

    /**
     * Removes a specific Parameter Set using it's description.
     * 
     * @param description String description of parameter set
     */
    public void removePSet(String description) {
        setTable.remove(description);
    }

    /**
     * Removes a surface from the image's surface list.
     * 
     * @param path String path to the surface to be removed from the image
     */
    public void removeSurface(String path) {
        surfaces.remove(path);
    }

    /**
     * Removes a voi from the image's VOI list.
     * 
     * @param path String path to the VOI to be removed from the image
     */
    public void removeVOI(String path) {
        VOIs.remove(path);
    }

    /**
     * Set the investigator's affiliation using an index.
     * 
     * @param affiliation String Investigator Affiliation
     * @param num int index for investigator array
     */
    public void setAffiliation(String affiliation, int num) {
        investigators[num - 1].setAffiliation(affiliation);
    }

    /**
     * Set the subject's body part.
     * 
     * @param bodyPart String subject's body part
     */
    public void setBodyPart(String bodyPart) {
        this.bodyPart = bodyPart;
    }

    /**
     * Sets the subject's diagonosis.
     * 
     * @param diagnosis String subject's diagnosis
     */
    public void setDiagnosis(String diagnosis) {
        this.diagnosis = diagnosis;
    }

    /**
     * Set subject's date of birth.
     * 
     * @param DOB String date of birth
     */
    public void setDOB(String DOB) {
        this.DOB = DOB;
    }

    /**
     * Set the investigator's email using an index.
     * 
     * @param email String Investigator Email
     * @param num int index for investigator array
     */
    public void setEmail(String email, int num) {
        investigators[num - 1].setEmail(email);
    }

    /**
     * Set the scan Equipment Model Name associated with image.
     * 
     * @param equipmentName String equipment model name
     */
    public void setEquipmentName(String equipmentName) {
        this.equipmentName = equipmentName;
    }

    /**
     * Sets the subject's height.
     * 
     * @param height int subject's height
     */
    public void setHeight(int height) {
        this.height = height;
    }

    /**
     * Create a new investigator using his/her name.
     * 
     * @param investigatorName String Investigator Name
     * @param num int investigator # (not index)
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
     * @param investigators Investigator[] array of investigator data
     */
    public void setInvestigators(Investigator[] investigators) {
        this.investigators = investigators;
    }

    /**
     * Sets the array (3) of booleans that tell whether the investigator data for that index is complete.
     * 
     * @param invest boolean[] boolean array for investigator completeness
     */
    public void setInvestigatorsComplete(boolean[] invest) {
        this.invest = invest;
    }

    /**
     * Sets the path for the linked image.
     * 
     * @param linkedImagePath path of linked image
     */
    public void setLinkedImagePath(String linkedImagePath) {
        this.linkedImagePath = linkedImagePath;
    }

    /**
     * Sets the matrix associated with the FileInfo.
     * 
     * @param matrix TransMatrix the image orientation matrix
     */
    public void setMatrix(TransMatrix matrix) {
        this.matrix = matrix;
    }

    /**
     * Set the investigators phone # using an index.
     * 
     * @param phone String investigator phone #
     * @param num int index for investigator array
     */
    public void setPhone(String phone, int num) {
        investigators[num - 1].setPhone(phone);
    }

    /**
     * Sets scan Protocol for equipment associated with image.
     * 
     * @param protocol String protocol
     */
    public void setProtocol(String protocol) {
        this.protocol = protocol;
    }

    /**
     * Sets the hashtable that holds all sets of parameters.
     * 
     * @param setTable Hashtable set hashtable
     */
    public void setPSetHashtable(Hashtable<String,XMLPSet> setTable) {
        this.setTable = setTable;
    }

    /**
     * Sets the subject's race.
     * 
     * @param race String subject's race
     */
    public void setRace(String race) {
        this.race = race;
    }

    /**
     * Set the scan date.
     * 
     * @param scanDate String scan date
     */
    public void setScanDate(String scanDate) {
        this.scanDate = scanDate;
    }

    /**
     * Set the date & time (date + T + time).
     * 
     * @param scanDateTime String scan date & time
     */
    public void setScanDateTime(String scanDateTime) {

        // separate into date and time
        StringTokenizer dt = new StringTokenizer(scanDateTime, "T");

        if (dt.hasMoreElements()) {
            this.scanDate = dt.nextToken();
            this.scanTime = dt.nextToken();
        }
    }

    /**
     * Sets the scan ID.
     * 
     * @param scanID String scan ID
     */
    public void setScanID(String scanID) {
        this.scanID = scanID;
    }

    /**
     * Set the scan time.
     * 
     * @param scanTime String scan time
     */
    public void setScanTime(String scanTime) {
        this.scanTime = scanTime;
    }

    /**
     * Set subject's sex.
     * 
     * @param sex String subject's sex
     */
    public void setSex(String sex) {
        this.sex = sex;
    }

    /**
     * Set the subject's ID.
     * 
     * @param subjectID String subject's ID
     */
    public void setSubjectID(String subjectID) {
        this.subjectID = subjectID;
    }

    /**
     * Sets the subject's name.
     * 
     * @param subjectName String subject's name
     */
    public void setSubjectName(String subjectName) {
        this.subjectName = subjectName;
    }

    /**
     * Sets the Surface hashtable for this image.
     * 
     * @param _surfaces Hashtable new surface hashtable
     */
    public void setSurfaces(Hashtable<String,SurfaceLink> _surfaces) {
        surfaces = _surfaces;
    }

    /**
     * Set the investigator's title using an index.
     * 
     * @param title String Investigator Title
     * @param num int index for investigator array
     */
    public void setTitle(String title, int num) {
        investigators[num - 1].setTitle(title);
    }

    /**
     * Sets the VOI hashtable for this image.
     * 
     * @param _VOIs Hashtable new VOI hashtable
     */
    public void setVOIs(Hashtable<String,VOILink> _VOIs) {
        VOIs = _VOIs;
    }

    /**
     * Sets the subject's weight.
     * 
     * @param weight int subject's weight
     */
    public void setWeight(int weight) {
        this.weight = weight;
    }

    /**
     * Sets the history string. Used by the NDAR meta-information generation dialog to write out the name of the zip
     * file where the original data files are stored.
     * 
     * @param hist The history string for this image.
     */
    public void setHistory(String hist) {
        this.history = hist;
    }

    /**
     * Updates the fileinfo with changes made within the JDialogs.
     * 
     * @param ce Vector Vector of new data
     */
    @SuppressWarnings("unchecked")
    public void stateChanged(Vector ce) {
        String tname = (String) ce.elementAt(2); // [t]able [name]
        //Vector tcvalue = (Vector) ce.elementAt(3); // [t]able [c]ode [value]
        String tvalue = (String) ce.elementAt(4); // [t]able [value]

        if (tname.equalsIgnoreCase("Description")) {
            setImageDescription(tvalue);
        } else if (tname.equalsIgnoreCase("Linked-image")) {
            setLinkedImagePath(tvalue);
        } else if (tname.equalsIgnoreCase("Modality")) {
            setModality(FileInfoBase.getModalityFromStr(tvalue));
        } else if (tname.equalsIgnoreCase("Subject Name")) {
            setSubjectName(tvalue);
        } else if (tname.equalsIgnoreCase("Subject ID")) {
            setSubjectID(tvalue);
        } else if (tname.equalsIgnoreCase("Race")) {
            setRace(tvalue);
        } else if (tname.equalsIgnoreCase("Diagnosis")) {
            setDiagnosis(tvalue);
        } else if (tname.equalsIgnoreCase("Date of Birth")) {
            setDOB(tvalue);
        } else if (tname.equalsIgnoreCase("Height")) {
            setHeight(Integer.valueOf(tvalue).intValue());
        } else if (tname.equalsIgnoreCase("Weight")) {
            setWeight(Integer.valueOf(tvalue).intValue());
        } else if (tname.equalsIgnoreCase("History")) {
            setHistory(tvalue);
        } else if (tname.equalsIgnoreCase("Sex")) {
            setSex(tvalue);
        } else if (tname.equalsIgnoreCase("Body Part")) {
            setBodyPart(tvalue);
        } else if (tname.equalsIgnoreCase("Equipment Model Name")) {
            setEquipmentName(tvalue);
        } else if (tname.equalsIgnoreCase("Scan ID")) {
            setScanID(tvalue);
        } else if (tname.equalsIgnoreCase("Protocol")) {
            setProtocol(tvalue);
        } else if (tname.equalsIgnoreCase("Scan Date")) {
            setScanDate(tvalue);
        } else if (tname.equalsIgnoreCase("Scan Time")) {
            setScanTime(tvalue);
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
     * Used to propogate all fileInfoXML private variables to other fileinfos.
     * 
     * @param fInfo FileInfoXML file info to be copied into
     */
    public void updateFileInfos(FileInfoXML fInfo) {

        if (this == fInfo) {
            return;
        }

        ((FileInfoImageXML) fInfo).setImageDescription(this.getImageDescription());
        ((FileInfoImageXML) fInfo).setLinkedImagePath(this.getLinkedImagePath());
        ((FileInfoImageXML) fInfo).setModality(this.getModality());
        ((FileInfoImageXML) fInfo).setSubjectName(this.getSubjectName());
        ((FileInfoImageXML) fInfo).setSubjectID(this.getSubjectID());
        ((FileInfoImageXML) fInfo).setRace(this.getRace());
        ((FileInfoImageXML) fInfo).setDiagnosis(this.getDiagnosis());
        ((FileInfoImageXML) fInfo).setDOB(this.getDOB());
        ((FileInfoImageXML) fInfo).setHeight(this.getHeight());
        ((FileInfoImageXML) fInfo).setWeight(this.getWeight());
        ((FileInfoImageXML) fInfo).setHistory(this.getHistory());
        ((FileInfoImageXML) fInfo).setSex(this.getSex());
        ((FileInfoImageXML) fInfo).setBodyPart(this.getBodyPart());
        ((FileInfoImageXML) fInfo).setEquipmentName(this.getEquipmentName());
        ((FileInfoImageXML) fInfo).setScanID(this.getScanID());
        ((FileInfoImageXML) fInfo).setProtocol(this.getProtocol());
        ((FileInfoImageXML) fInfo).setScanDate(this.getScanDate());
        ((FileInfoImageXML) fInfo).setScanTime(this.getScanTime());
        ((FileInfoImageXML) fInfo).setInvestigators(this.getInvestigators());
        ((FileInfoImageXML) fInfo).setInvestigatorsComplete(this.getInvestigatorsComplete());
        ((FileInfoImageXML) fInfo).setPSetHashtable(this.getPSetHashtable());
        ((FileInfoImageXML) fInfo).setSurfaces(this.getSurfaces());
    }

    // ~ Inner Classes
    // --------------------------------------------------------------------------------------------------

    /**
     * <p>
     * Title: Investigator
     * </p>
     * 
     * <p>
     * Description: Public class to hold several attributes for an investigator associated with the image (up to three
     * per)
     * </p>
     * 
     * <p>
     * Copyright: Copyright (c) 2004
     * </p>
     * 
     * <p>
     * Company:
     * </p>
     * 
     * @author not attributable
     * @version 1.0
     */
    public static class Investigator implements Serializable {

        /** Use serialVersionUID for interoperability. */
        private static final long serialVersionUID = -5267494343773142072L;

        /** Affiliation of investigator. */
        private String affiliation;

        /** Investigator's email. */
        private String email;

        /** Name of investigator. */
        private String name;

        /** Investigator's phone number. */
        private String phone;

        /** Title of investigator. */
        private String title;

        /**
         * Creates a new Investigator with given name.
         * 
         * @param name String name
         */
        public Investigator(String name) {
            this.name = name;
        }

        /**
         * Gets affiliation.
         * 
         * @return String affiliation
         */
        public String getAffiliation() {
            return affiliation;
        }

        /**
         * Gets email.
         * 
         * @return String email
         */
        public String getEmail() {
            return email;
        }

        /**
         * Gets name.
         * 
         * @return String name
         */
        public String getName() {
            return name;
        }

        /**
         * Gets phone number.
         * 
         * @return String phone number
         */
        public String getPhone() {
            return phone;
        }

        /**
         * Gets title.
         * 
         * @return String title
         */
        public String getTitle() {
            return title;
        }

        /**
         * Sets affiliation.
         * 
         * @param affiliation String affiliation
         */
        public void setAffiliation(String affiliation) {
            this.affiliation = affiliation;
        }

        /**
         * Sets email.
         * 
         * @param email String email
         */
        public void setEmail(String email) {
            this.email = email;
        }

        /**
         * Sets phone number.
         * 
         * @param phone String phone number
         */
        public void setPhone(String phone) {
            this.phone = phone;
        }

        /**
         * Sets title.
         * 
         * @param title String title
         */
        public void setTitle(String title) {
            this.title = title;
        }
    }

   

    

    /**
     * <p>
     * Title: SurfaceLink
     * </p>
     * 
     * <p>
     * Description: Link to a surface associated with the image
     * </p>
     * 
     * <p>
     * Copyright: Copyright (c) 2004
     * </p>
     * 
     * <p>
     * Company:
     * </p>
     * 
     * @author not attributable
     * @version 1.0
     */
    public class SurfaceLink implements Serializable {

        /** Use serialVersionUID for interoperability. */
        private static final long serialVersionUID = -3088783811309623463L;

        /** Should the surface be displayed. */
        boolean display;

        /** Opacity of surface. */
        float opacity;

        /** Path to surface. */
        String path;

        /**
         * Create a new surface link with the given path.
         * 
         * @param _path String path to surface
         */
        public SurfaceLink(String _path) {
            path = _path;

            // defaults
            display = true;
            opacity = 0.5f;
        }

        /**
         * Gets whether the surface should be displayed.
         * 
         * @return boolean whether the surface should be displayed
         */
        public boolean getDisplay() {
            return display;
        }

        /**
         * Gets the opacity of the surface.
         * 
         * @return float opacity of the surface
         */
        public float getOpacity() {
            return opacity;
        }

        /**
         * Gets the path for the surface.
         * 
         * @return String path
         */
        public String getPath() {
            return path;
        }

        /**
         * Sets whether the surface should be displayed.
         * 
         * @param flag boolean should the surface be displayed
         */
        public void setDisplay(boolean flag) {
            display = flag;
        }

        /**
         * Sets the opacity for the surface.
         * 
         * @param val float opacity for the surface
         */
        public void setOpacity(float val) {
            opacity = val;
        }
    }

    /**
     * <p>
     * Title: VOILink
     * </p>
     * 
     * <p>
     * Description: Link to VOI associated with image
     * </p>
     * 
     * <p>
     * Copyright: Copyright (c) 2004
     * </p>
     * 
     * <p>
     * Company:
     * </p>
     * 
     * @author not attributable
     * @version 1.0
     */
    public class VOILink implements Serializable {

        /** Use serialVersionUID for interoperability. */
        private static final long serialVersionUID = 3856425069094626835L;

        /** Display VOI with image. */
        boolean display;

        /** Path to VOI. */
        String path;

        /**
         * Create a new VOILink with the given path.
         * 
         * @param _path String path of VOI
         */
        public VOILink(String _path) {
            path = _path;
        }

        /**
         * Gets whether or not the VOI should be displayed.
         * 
         * @return boolean should the VOI be displayed
         */
        public boolean getDisplay() {
            return display;
        }

        /**
         * Gets the path for the VOI.
         * 
         * @return String path
         */
        public String getPath() {
            return path;
        }

        /**
         * Sets whether or not the VOI should be displayed.
         * 
         * @param flag boolean should the VOI be displayed
         */
        public void setDisplay(boolean flag) {
            display = flag;
        }
    }
}
