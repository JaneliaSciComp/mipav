package gov.nih.mipav.model.file;


import gov.nih.mipav.view.dialogs.*;
import gov.nih.mipav.model.structures.*;

import java.util.Hashtable;
import java.util.Enumeration;
import java.util.Vector;
import java.util.StringTokenizer;
import java.io.Serializable;


/**
 * This structure contains the information that describes how
 * an XML image is stored on disk.
 *
 * Notes:
 * <ol>
 *  <li>Should the matrix XML tag have a field to apply transformation matrix.</li>
 *  <li>toString should be added near the end of development ? or in FileXML.</li>
 * </ol>
 *
 * @version 0.1 Sept 19, 2002
 * @author  Neva Cherniavsky
 * @author  Matthew J. McAuliffe, Ph.D.
 * @see     FileIO
 * @see     FileInfoXML
 */
public class FileInfoImageXML extends FileInfoXML {
    /**
     *   Full path to an image that is linked to this image file. It
     *   is typically used to load a second image into the same frame.
     *   Optional XML tag.
     */
    private String linkedFilePath;

    /** Transformation matrix */
    private TransMatrix matrix;

    /** linked image path (optional) */
    private String linkedImagePath;

    /** Subject's information */
    private String subjectName;
    private String race;
    private String subjectID;
    private String diagnosis;
    private String bodyPart;
    private String DOB;
    private int height = 0;
    private int weight = 0;
    private String sex;

    /** Scan attributes.
     *   Optional XML tag.
     */
    private String equipmentName;
    private String scanID;
    private String protocol;
    private String scanDate;
    private String scanTime;

    /** Investigators (up to 3).
     *   Optional XML tag.
     */
    private Investigator[] investigators;
    private boolean[] invest;

    /** Hashtable for holding sets of parameters */
    private Hashtable setTable;

    /** Used to keep track of the current parameter name */
    private String currentPName;

    /** String for the current PSet description (for key) */
    private String currentPSetDesc;

    /** List of files which describe VOIs attached to this image */
    private Hashtable VOIs;

    /** Used to keep track of the current VOI */
    private String currentVOIPath;

    /** List of files which describe surfaces attached to this image */
    private Hashtable surfaces;

    /** Used to keep track of the current surface */
    private String currentSurfacePath;

    /**
     * Main constructor for FileInfoXML
     * @param name String file name
     * @param directory String file directory
     * @param format int file format (data type)
     */
    public FileInfoImageXML( String name, String directory, int format ) {
        super( name, directory, format );
        setTable = new Hashtable();
        investigators = new Investigator[3];
        invest = new boolean[3];
        invest[0] = false;
        invest[1] = false;
        invest[2] = false;
        currentPSetDesc = new String( "" );
        VOIs = new Hashtable();
        currentVOIPath = new String( "" );
        surfaces = new Hashtable();
        currentSurfacePath = new String( "" );
    }

    /**
     * Prepares the class for cleanup
     */
    public void finalize() {

        if ( setTable != null ) {
            setTable.clear();
        }

        setTable = null;
        matrix = null;
        investigators = null;
        invest = null;
        if ( VOIs != null ) {
            VOIs.clear();
        }

        VOIs = null;
        if ( surfaces != null ) {
            surfaces.clear();
        }

        surfaces = null;
        //if (thumbnail != null) {
        //
        //   }
        super.finalize();
    }

    /**
     *   Gets the full path + name of the linked image
     *   @return full path of the linked image
     */
    public String getLinkedImagePath() {
        return this.linkedImagePath;
    }

    /**
     *   Sets the path for the linked image
     *   @param linkedImagePath  path of linked image
     */
    public void setLinkedImagePath( String linkedImagePath ) {
        this.linkedImagePath = linkedImagePath;
    }

    /**
     * Gets the matrix associated with the FileInfo
     * @return TransMatrix the image orientation matrix
     */
    public TransMatrix getMatrix() {
        return this.matrix;
    }

    /**
     * Sets the matrix associated with the FileInfo
     * @param matrix TransMatrix the image orientation matrix
     */
    public void setMatrix( TransMatrix matrix ) {
        this.matrix = matrix;
    }

    /**
     * Get's the subject's name
     * @return String subject's name
     */
    public String getSubjectName() {
        return this.subjectName;
    }

    /**
     * Sets the subject's name
     * @param subjectName String subject's name
     */
    public void setSubjectName( String subjectName ) {
        this.subjectName = subjectName;
    }

    /**
     * Gets the subject's race
     * @return String subject's race
     */
    public String getRace() {
        return this.race;
    }

    /**
     * Sets the subject's race
     * @param race String subject's race
     */
    public void setRace( String race ) {
        this.race = race;
    }

    /**
     * Get's the subject's ID
     * @return String subject's ID
     */
    public String getSubjectID() {
        return this.subjectID;
    }

    /**
     *   Set the subject's ID
     *   @param subjectID String subject's ID
     */
    public void setSubjectID( String subjectID ) {
        this.subjectID = subjectID;
    }

    /**
     *   Get diagnosis for subject (optional)
     *   @return diagnosis subject's diagnosis
     */
    public String getDiagnosis() {
        return this.diagnosis;
    }

    /**
     *   Sets the subject's diagonosis
     *   @param diagnosis String subject's diagnosis
     */
    public void setDiagnosis( String diagnosis ) {
        this.diagnosis = diagnosis;
    }

    /**
     *   Get subject's date of birth (optional)
     *   @return DOB date of birth
     */
    public String getDOB() {
        return this.DOB;
    }

    /**
     *   Set subject's date of birth
     *   @param DOB String date of birth
     */
    public void setDOB( String DOB ) {
        this.DOB = DOB;
    }

    /**
     *   Get subject's height (optional)
     *   @return height subject's height
     */
    public int getHeight() {
        return this.height;
    }

    /**
     * Sets the subject's height
     * @param height int subject's height
     */
    public void setHeight( int height ) {
        this.height = height;
    }

    /**
     * Gets the subject's weight
     * @return int subject's weight
     */
    public int getWeight() {
        return this.weight;
    }

    /**
     * Sets the subject's weight
     * @param weight int subject's weight
     */
    public void setWeight( int weight ) {
        this.weight = weight;
    }

    /**
     *   Get subject's sex (optional)
     *   @return sex subject's sex
     */
    public String getSex() {
        return this.sex;
    }

    /**
     *   Set subject's sex
     *   @param sex String subject's sex
     */
    public void setSex( String sex ) {
        this.sex = sex;
    }

    /**
     *   Get subject's body part (optional)
     *   @return bodyPart subject's body part
     */
    public String getBodyPart() {
        return this.bodyPart;
    }

    /**
     *   Set the subject's body part
     *   @param bodyPart String subject's body part
     */
    public void setBodyPart( String bodyPart ) {
        this.bodyPart = bodyPart;
    }

    /**
     *   Get the scan Equipment Model Name associated with image
     *   @return equipmentName equipment model name
     */
    public String getEquipmentName() {
        return this.equipmentName;
    }

    /**
     *   Set the scan Equipment Model Name associated with image
     *   @param equipmentName String equipment model name
     */
    public void setEquipmentName( String equipmentName ) {
        this.equipmentName = equipmentName;
    }

    /**
     * Gets the scan ID
     * @return String scan ID
     */
    public String getScanID() {
        return this.scanID;
    }

    /**
     * Sets the scan ID
     * @param scanID String scan ID
     */
    public void setScanID( String scanID ) {
        this.scanID = scanID;
    }

    /**
     *   Get scan Protocol for equipment associated with image
     *   @return protocol protocol
     */
    public String getProtocol() {
        return this.protocol;
    }

    /**
     *   Sets scan Protocol for equipment associated with image
     *   @param protocol String protocol
     */
    public void setProtocol( String protocol ) {
        this.protocol = protocol;
    }

    /**
     *   Get the date the image was taken
     *   @return scanDate scan date
     */
    public String getScanDate() {
        return this.scanDate;
    }

    /**
     *   Get the time the image was taken
     *   @return scanTime scan time
     */
    public String getScanTime() {
        return this.scanTime;
    }

    /**
     *   Set the scan date
     *   @param scanDate String scan date
     */
    public void setScanDate( String scanDate ) {
        this.scanDate = scanDate;
    }

    /**
     *   Set the scan time
     *   @param scanTime String scan time
     */
    public void setScanTime( String scanTime ) {
        this.scanTime = scanTime;
    }

    /**
     *   Set the date & time (date + T + time)
     *   @param scanDateTime String scan date & time
     */
    public void setScanDateTime( String scanDateTime ) {

        //separate into date and time
        StringTokenizer dt = new StringTokenizer( scanDateTime, "T" );

        if ( dt.hasMoreElements() ) {
            this.scanDate = dt.nextToken();
            this.scanTime = dt.nextToken();
        }
    }

    /**
     * Gets the specified investigator
     * @param index int index of investigator
     * @return Investigator investigator
     */
    public Investigator getInvestigator( int index ) {
        if ( index < 3 ) {
            return investigators[index];
        } else {
            return null;
        }
    }

    /**
     *   Get an array (3) of booleans that tell whether
     *   the investigator data for that index is complete
     *   @return boolean[] array of investigator completeness
     */
    public boolean[] getInvestigatorsComplete() {
        return this.invest;
    }

    /**
     * Sets the array (3) of booleans that tell whether
     *   the investigator data for that index is complete
     * @param invest boolean[] boolean array for investigator completeness
     */
    public void setInvestigatorsComplete( boolean[] invest ) {
        this.invest = invest;
    }

    /**
     *   Gets an array (3) of Investigator data
     *   @return Investigator[] array of investigator data
     */
    public Investigator[] getInvestigators() {
        return this.investigators;
    }

    /**
     *   Sets the array (3) of Investigator data
     *   @param investigators Investigator[] array of investigator data
     */
    public void setInvestigators( Investigator[] investigators ) {
        this.investigators = investigators;
    }

    /**
     *   Create a new investigator using his/her name
     *   @param investigatorName String Investigator Name
     *   @param num int investigator # (not index)
     */
    public void setInvestigatorName( String investigatorName, int num ) {
        num--;
        if ( num < 3 ) {
            investigators[num] = new Investigator( investigatorName );
            invest[num] = true;
        }
    }

    /**
     *   Set the investigator's title using an index
     *   @param title String Investigator Title
     *   @param num int index for investigator array
     */
    public void setTitle( String title, int num ) {
        investigators[num - 1].setTitle( title );
    }

    /**
     *   Set the investigator's affiliation using an index
     *   @param affiliation String Investigator Affiliation
     *   @param num int index for investigator array
     */
    public void setAffiliation( String affiliation, int num ) {
        investigators[num - 1].setAffiliation( affiliation );
    }

    /**
     *   Set the investigator's email using an index
     *   @param email String Investigator Email
     *   @param num int index for investigator array
     */
    public void setEmail( String email, int num ) {
        investigators[num - 1].setEmail( email );
    }

    /**
     *   Set the investigators phone # using an index
     *   @param phone String investigator phone #
     *   @param num int index for investigator array
     */
    public void setPhone( String phone, int num ) {
        investigators[num - 1].setPhone( phone );
    }

    /**
     *   Gets the hashtable that holds all sets of parameters
     *   @return Hashtable set hashtable
     */
    public Hashtable getPSetHashtable() {
        return this.setTable;
    }

    /**
     *   Sets the hashtable that holds all sets of parameters
     *   @param setTable Hashtable set hashtable
     */
    public void setPSetHashtable( Hashtable setTable ) {
        this.setTable = setTable;
    }

    /**
     *   Removes a specific Parameter Set using it's description
     *   @param description String description of parameter set
     */
    public void removePSet( String description ) {
        setTable.remove( description );
    }

    /**
     *   Creates a new parameter set with a description
     *   @param description String desription
     */
    public void createPSet( String description ) {
        this.currentPSetDesc = description;
        setTable.put( description, new PSet( description ) );
    }

    /**
     * Gets current parameter set
     * @return PSet current parameter set
     */
    public PSet getCurrentPSet() {
        return (PSet) setTable.get( currentPSetDesc );
    }

    /**
     *   Gets a parameter set from the hashtable using a description
     *   @param description PSet description of parameter set
     */
    public PSet getPSet( String description ) {
        return (PSet) setTable.get( description );
    }

    /**
     *   Gets the parameter table associated with a Parameter set
     *   with the given description
     *   @param description String parameter set description
     *   @return Hashtable parameter hashtable
     */
    public Hashtable getParameterTable( String description ) {
        return ( (PSet) setTable.get( description ) ).getTable();
    }

    /**
     *   Gets the # of parameter sets stored
     *   @return int number of parameter sets
     */
    public int getNumPSets() {
        return setTable.size();
    }

    /**
     *   Gets an enumeration for the list of parameter set keys
     *   @return Enumeration keys for parameter set hashtable
     */
    public Enumeration getPSetKeys() {
        return setTable.keys();
    }

    /**
     *	Returns the VOI hashtable for this image.
     *	@return Hashtable VOI hashtable
     */
    public Hashtable getVOIs() {
        return VOIs;
    }

    /**
     *	Sets the VOI hashtable for this image.
     *	@param _VOIs Hashtable new VOI hashtable
     */
    public void setVOIs( Hashtable _VOIs ) {
        VOIs = _VOIs;
    }

    /**
     *	Gets paths to VOI files for this image.
     *	@return Enumeration Enumeration of paths to VOI files
     */
    public Enumeration getVOIKeys() {
        return VOIs.keys();
    }

    /**
     *	Adds a voi path to the image's VOI list.
     *	@param path String path to a VOI file
     */
    public void addVOI( String path ) {
        currentVOIPath = path;
        VOIs.put( path, new VOILink( path ) );
    }

    /**
     *	Removes a voi from the image's VOI list.
     *	@param path String path to the VOI to be removed from the image
     */
    public void removeVOI( String path ) {
        VOIs.remove( path );
    }

    /**
     *	Returns a VOILink to a VOI attached to this image.
     *	@param path String filepath to the VOI to be returned
     *	@return VOILink VOILink for the given path
     */
    public VOILink getVOI( String path ) {
        return (VOILink) VOIs.get( path );
    }

    /**
     * Get number of VOIs
     * @return int number of VOIs
     */
    public int getNumVOIs() {
        return VOIs.size();
    }

    /**
     * Get's the VOILink for the current VOI
     * @return VOILink current voi link
     */
    public VOILink getCurrentVOI() {
        return (VOILink) VOIs.get( currentVOIPath );
    }

    /**
     *	Returns the surface hashtable for this image.
     *	@return Hashtable the surface hashtable
     */
    public Hashtable getSurfaces() {
        return surfaces;
    }

    /**
     *	Sets the Surface hashtable for this image.
     *	@param _surfaces Hashtable new surface hashtable
     */
    public void setSurfaces( Hashtable _surfaces ) {
        surfaces = _surfaces;
    }

    /**
     *	Gets paths to surface files for this image.
     *	@return Enumeration Enumeration of paths to surface files
     */
    public Enumeration getSurfaceKeys() {
        return surfaces.keys();
    }

    /**
     *	Adds a surface path to the image's surface list.
     *	@param path String path to a surface file
     */
    public void addSurface( String path ) {
        currentSurfacePath = path;
        surfaces.put( path, new SurfaceLink( path ) );
    }

    /**
     *	Removes a surface from the image's surface list.
     *	@param path String path to the surface to be removed from the image
     */
    public void removeSurface( String path ) {
        surfaces.remove( path );
    }

    /**
     *	Returns a SurfaceLink to a surface attached to this image.
     *	@param path String filepath to the surface to be returned
     *	@return SurfaceLink Surfacelink for the given path
     */
    public SurfaceLink getSurface( String path ) {
        return (SurfaceLink) surfaces.get( path );
    }

    /**
     * Gets the number of surfaces attached to the image
     * @return int number of surfaces
     */
    public int getNumSurfaces() {
        return surfaces.size();
    }

    /**
     * Returns the SurfaceLink object for the current surface.
     * @return SurfaceLink surfacelink object
     */
    public SurfaceLink getCurrentSurface() {
        return (SurfaceLink) surfaces.get( currentSurfacePath );
    }

    /**
     *
     * <p>Title: Investigator </p>
     *
     * <p>Description: Public class to hold several attributes for an investigator
     *  associated with the image (up to three per) </p>
     *
     * <p>Copyright: Copyright (c) 2004</p>
     *
     * <p>Company: </p>
     *
     * @author not attributable
     * @version 1.0
     */
    public class Investigator
        implements Serializable {
        /** Name of investigator*/
        private String name;
        /** Title of investigator*/
        private String title;
        /** Affiliation of investigator*/
        private String affiliation;
        /** Investigator's email */
        private String email;
        /** Investigator's phone number*/
        private String phone;

        /**
         *   Creates a new Investigator with given name
         *   @param name String name
         */
        public Investigator( String name ) {
            this.name = name;
        }

        /**
         *   Sets title
         *   @param title String title
         */
        public void setTitle( String title ) {
            this.title = title;
        }

        /**
         *   Sets affiliation
         *   @param affiliation String affiliation
         */
        public void setAffiliation( String affiliation ) {
            this.affiliation = affiliation;
        }

        /**
         *   Sets email
         *   @param email String email
         */
        public void setEmail( String email ) {
            this.email = email;
        }

        /**
         *   Sets phone number
         *   @param phone String phone number
         */
        public void setPhone( String phone ) {
            this.phone = phone;
        }

        /**
         *   Gets name
         *   @return String name
         */
        public String getName() {
            return name;
        }

        /**
         *   Gets title
         *   @return String title
         */
        public String getTitle() {
            return title;
        }

        /**
         *   Gets affiliation
         *   @return String affiliation
         */
        public String getAffiliation() {
            return affiliation;
        }

        /**
         *   Gets email
         *   @return String email
         */
        public String getEmail() {
            return email;
        }

        /**
         *   Gets phone number
         *   @return String phone number
         */
        public String getPhone() {
            return phone;
        }
    }


    /**
     *
     * <p>Title: PSet</p>
     *
     * <p>Description: Public class to store up to an infinite number of
     *  parameters... which will be in a Hashtable with name as the key
     *  Note: there must be at least one parameter associated with each parameter set
     *  per XSD (XML Schema)</p>
     *
     * <p>Copyright: Copyright (c) 2004</p>
     *
     * <p>Company: </p>
     *
     * @author not attributable
     * @version 1.0
     */
    public class PSet
        implements Serializable {
        /** Description of parameter*/
        private String description;
        /** Current parameter name */
        private String currentParameterName;
        /** Parameter hashtable */
        private Hashtable parameterTable;

        /**
         * Create a new parameter set with the given description
         * @param description String description of parameter set
         */
        public PSet( String description ) {
            this.description = description;
            parameterTable = new Hashtable();
        }

        /**
         *   Get the parameter set description
         *   @return String description
         */
        public String getDescription() {
            return this.description;
        }

        /**
         *   Adds a new parameter to the set
         *   @param name String name
         */
        public void addParameter( String name ) {
            this.currentParameterName = name;
            parameterTable.put( name, new Parameter( name ) );
        }

        /**
         *   Determines if the set contains a parameter with the given name
         *   @param key String parameter name (key)
         *   @return boolean set contains parameter
         */
        public boolean containsKey( String key ) {
            return parameterTable.containsKey( key );
        }

        /**
         *   Get an enumeration for the list of parameter names
         *   @return Enumeration enumeration for parameter name list
         */
        public Enumeration getParameterKeys() {
            return parameterTable.keys();
        }

        /**
         *   Returns the current parameter to be modified
         *   @return Parameter current parameter
         */
        public Parameter getCurrentParameter() {
            return (Parameter) parameterTable.get( currentParameterName );
        }

        /**
         *   Gets the parameter with the given name
         *   @return Parameter parameter
         */
        public Parameter getParameter( String name ) {
            return (Parameter) parameterTable.get( name );
        }

        /**
         *   Gets the hashtable of parameters
         *   @return Hashtable parameter hashtable
         */
        public Hashtable getTable() {
            return this.parameterTable;
        }

        /**
         *   Removes the parameter with the given name from the hashtable
         *   @param name String parameter name
         */
        public void removeParameter( String name ) {
            parameterTable.remove( name );
        }

        /**
         * Returns a String representation of the Set
         * @return String string representation
         */
        public String toString() {
            Enumeration e = getTable().elements();
            StringBuffer set = new StringBuffer( "<Sets>" );

            set.append( "<Set-description>" );
            set.append( getDescription() );
            set.append( "</Set-description>" );
            while ( e.hasMoreElements() ) {
                try {
                    Parameter p = (Parameter) e.nextElement();

                    //                set.append("<Parameters>");
                    set.append( p.toString() );
                    //                set.append("</Parameters>");
                } catch ( ClassCastException cce ) {
                    System.err.println( "This element was not a Parameter." );
                }
            }
            set.append( "</Sets>" );

            return set.toString();
        }

    }


    /**
     *
     * <p>Title: Parameter</p>
     *
     * <p>Description: Public class to store information for a parameter associated with
     *  the image (infinite parameters allowed per)</p>
     * <p>Copyright: Copyright (c) 2004</p>
     *
     * <p>Company: </p>
     *
     * @author not attributable
     * @version 1.0
     */
    public class Parameter
        implements Serializable {
        /** Parameter name */
        private String name;
        /** Parameter description */
        private String description;
        /** Parameter value type */
        private String valueType;
        /** Parameter value */
        private String value;
        /** Parameter date */
        private String date;
        /** Parameter time */
        private String time;

        /**
         *   Creates a new parameter by name
         *   @param name String parameter name
         */
        public Parameter( String name ) {
            this.name = name;
        }

        /**
         *   Sets the description for the parameter
         *   @param description String description
         */
        public void setDescription( String description ) {
            this.description = description;
        }

        /**
         *   Sets the value type for the parameter
         *   @param valueType String value type
         */
        public void setValueType( String valueType ) {
            this.valueType = valueType;
        }

        /**
         *   Sets the value for the parameter
         *   @param value String value
         */
        public void setValue( String value ) {
            this.value = value;
        }

        /**
         *   Sets the date + T + time for the parameter
         *   @param dateTime String date-time
         */
        public void setDateTime( String dateTime ) {
            StringTokenizer dt = new StringTokenizer( dateTime, "T" );

            if ( dt.hasMoreElements() ) {
                date = dt.nextToken();
                time = dt.nextToken();
            }
        }

        /**
         *   Sets the date for the parameter
         *   @param date String date
         */
        public void setDate( String date ) {
            this.date = date;
        }

        /**
         *   Sets the time for the parameter
         *   @param time String time
         */
        public void setTime( String time ) {
            this.time = time;
        }

        /**
         *   Gets the parameter's name
         *   @return String parameter name
         */
        public String getName() {
            return this.name;
        }

        /**
         *   Gets the parameter's description
         *   @return String description
         */
        public String getDescription() {
            return this.description;
        }

        /**
         *   Gets the parameter's value-type
         *   @return String value-type
         */
        public String getValueType() {
            return this.valueType;
        }

        /**
         *   Gets the parameter's value
         *   @return String value
         */
        public String getValue() {
            return this.value;
        }

        /**
         *   Gets the parameter's date
         *   @return String date
         */
        public String getDate() {
            return this.date;
        }

        /**
         *   Gets the parameter's time
         *   @return String time
         */
        public String getTime() {
            return this.time;
        }

        /**
         * String representation of parameter
         * @return String string representation
         */
        public String toString() {
            String p = new String( "<Parameters>" );

            p += "<Parameter-name>" + getName() + "</Parameter-name>";
            p += "<Parameter-description>" + getDescription() + "</Parameter-description>";
            p += "<Value-type>" + getValueType() + "</Value-type>";
            p += "<Value>" + getValue() + "</Value>";
            p += "<Parameter-date-time>" + getDate() + "T" + getTime() + "</Parameter-date-time>";
            p += "</Parameters>";

            return p;
        }
    }


    /**
     *
     * <p>Title: VOILink</p>
     *
     * <p>Description: Link to VOI associated with image</p>
     *
     * <p>Copyright: Copyright (c) 2004</p>
     *
     * <p>Company: </p>
     *
     * @author not attributable
     * @version 1.0
     */
    public class VOILink
        implements Serializable {
        /** Path to VOI */
        String path;
        /** Display VOI with image */
        boolean display;

        /**
         * Create a new VOILink with the given path
         * @param _path String path of VOI
         */
        public VOILink( String _path ) {
            path = _path;
        }

        /**
         * Gets the path for the VOI
         * @return String path
         */
        public String getPath() {
            return path;
        }

        /**
         * Gets whether or not the VOI should be displayed
         * @return boolean should the VOI be displayed
         */
        public boolean getDisplay() {
            return display;
        }

        /**
         * Sets whether or not the VOI should be displayed
         * @param flag boolean should the VOI be displayed
         */
        public void setDisplay( boolean flag ) {
            display = flag;
        }
    }


    /**
     *
     * <p>Title: SurfaceLink</p>
     *
     * <p>Description: Link to a surface associated with the image</p>
     *
     * <p>Copyright: Copyright (c) 2004</p>
     *
     * <p>Company: </p>
     *
     * @author not attributable
     * @version 1.0
     */
    public class SurfaceLink
        implements Serializable {
        /** Path to surface */
        String path;
        /** Should the surface be displayed */
        boolean display;
        /** Opacity of surface */
        float opacity;

        /**
         * Create a new surface link with the given path
         * @param _path String path to surface
         */
        public SurfaceLink( String _path ) {
            path = _path;

            // defaults
            display = true;
            opacity = 0.5f;
        }

        /**
         * Gets the path for the surface
         * @return String path
         */
        public String getPath() {
            return path;
        }

        /**
         * Gets whether the surface should be displayed
         * @return boolean whether the surface should be displayed
         */
        public boolean getDisplay() {
            return display;
        }

        /**
         * Gets the opacity of the surface
         * @return float opacity of the surface
         */
        public float getOpacity() {
            return opacity;
        }

        /**
         * Sets whether the surface should be displayed
         * @param flag boolean should the surface be displayed
         */
        public void setDisplay( boolean flag ) {
            display = flag;
        }

        /**
         * Sets the opacity for the surface
         * @param val float opacity for the surface
         */
        public void setOpacity( float val ) {
            opacity = val;
        }
    }

    /**
     *  Displays the file information
     *  @param dlog    JDialogBase dialog box that is written to
     *  @param matrix  transformation matrix
     */
    public void displayAboutInfo( JDialogBase dlog, TransMatrix matrix ) {
        JDialogFileInfoXML dialog = (JDialogFileInfoXML) dlog;
        int[] extents;
        int i;
        int[] editorChoice = new int[1];

        editorChoice[0] = JDialogEditor.STRING;

        dialog.displayAboutInfo( this ); // setup layout in the dialog

        editorChoice[0] = JDialogEditor.STRING;
        if ( this.imageDescription != null ) {
            dialog.appendPrimaryData( "Description", this.imageDescription, editorChoice );
        } else {
            dialog.appendPrimaryData( "Description", "", editorChoice );
        }

        editorChoice[0] = JDialogEditor.XML_LINKEDIMAGE;
        if ( this.linkedImagePath != null ) {
            dialog.appendPrimaryData( "Linked-image", this.linkedImagePath, editorChoice );
        } else {
            dialog.appendPrimaryData( "Linked-image", "", editorChoice );
        }

        dialog.appendPrimaryData( "Image-offset", Integer.toString( getOffset() ) );

        extents = super.getExtents();
        for ( i = 0; i < extents.length; i++ ) {
            dialog.appendPrimaryData( "Dimension " + i, Integer.toString( extents[i] ) );
        }

        dialog.appendPrimaryData( "Type", ModelStorageBase.getBufferTypeStr( getDataType() ) );
        if ( ModelImage.isColorImage( getDataType() ) ) {
            dialog.appendPrimaryData( "Min red", Double.toString( getMinR() ) );
            dialog.appendPrimaryData( "Max red", Double.toString( getMaxR() ) );
            dialog.appendPrimaryData( "Min green", Double.toString( getMinG() ) );
            dialog.appendPrimaryData( "Max green", Double.toString( getMaxG() ) );
            dialog.appendPrimaryData( "Min blue", Double.toString( getMinB() ) );
            dialog.appendPrimaryData( "Max blue", Double.toString( getMaxB() ) );

        } else {
            dialog.appendPrimaryData( "Min", Double.toString( getMin() ) );
            dialog.appendPrimaryData( "Max", Double.toString( getMax() ) );
        }
        dialog.appendPrimaryData( "Orientation", getImageOrientationStr( getImageOrientation() ) );

        dialog.appendPrimaryData( "Axis X Orientation", getAxisOrientationStr( getAxisOrientation( 0 ) ) );
        dialog.appendPrimaryData( "Axis Y Orientation", getAxisOrientationStr( getAxisOrientation( 1 ) ) );
        dialog.appendPrimaryData( "Axis Z Orientation", getAxisOrientationStr( getAxisOrientation( 2 ) ) );

        float[] resolutions; // = new float[5];

        resolutions = getResolutions();
        int[] measure; // = new int[5];

        measure = getUnitsOfMeasure();
        for ( i = 0; i < extents.length; i++ ) {
            if ( resolutions[i] > 0.0 ) {
                String pixelRes = "Pixel resolution " + i;

                dialog.appendPrimaryData( pixelRes,
                        Float.toString( resolutions[i] ) + " " + getUnitsOfMeasureStr( measure[i] ) );
            } // end of if (resolutions[i] > 0.0)
        } // for (i=0; i < 5; i++)

        if ( extents.length > 2 && extents[2] > 0 ) {
            float sliceSpacing;

            sliceSpacing = getSliceSpacing();
            dialog.appendPrimaryData( "Slice Spacing ",
                    Float.toString( sliceSpacing ) + " " + getUnitsOfMeasureStr( measure[2] ) );
        }

        float[] origin; // = new float[4];

        origin = getOrigin();
        for ( i = 0; i < extents.length && i < 4; i++ ) {
            String originStr = "Origin " + i;

            dialog.appendPrimaryData( originStr, Float.toString( origin[i] ) );
        } // for (i=0; i < 4; i++)

        if ( getEndianess() == FileBase.LITTLE_ENDIAN ) {
            dialog.appendPrimaryData( "Endianess", "Little Endian" );
        } else {
            dialog.appendPrimaryData( "Endianess", "Big Endian" );
        }

        if ( matrix != null ) {
            // when using displayAboutInfo(dialog) this doesn't appear
            // calling prg might use an editing panel to adjust this matrix
            dialog.appendPrimaryData( "Transform ID", getTransformIDStr( getTransformID() ) );
            dialog.appendPrimaryData( "Matrix", matrix.matrixToString( 10, 4 ) );
        }

        editorChoice[0] = JDialogEditor.XML_MODALITY;
        dialog.appendPrimaryData( "Modality", getModalityStr( getModality() ), editorChoice );

        // now subject information
        String emptyString = new String( "" );

        editorChoice[0] = JDialogEditor.STRING;
        if ( subjectName != null ) {
            dialog.appendSubjectData( "Subject Name", subjectName, editorChoice );
        } else {
            dialog.appendSubjectData( "Subject Name", emptyString, editorChoice );
        }
        if ( subjectID != null ) {
            dialog.appendSubjectData( "Subject ID", subjectID, editorChoice );
        } else {
            dialog.appendSubjectData( "Subject ID", emptyString, editorChoice );
        }
        editorChoice[0] = JDialogEditor.XML_RACE;
        if ( race != null ) {
            dialog.appendSubjectData( "Race", race, editorChoice );
        } else {
            dialog.appendSubjectData( "Race", emptyString, editorChoice );
        }
        editorChoice[0] = JDialogEditor.STRING;
        if ( diagnosis != null ) {
            dialog.appendSubjectData( "Diagnosis", diagnosis, editorChoice );
        } else {
            dialog.appendSubjectData( "Diagnosis", emptyString, editorChoice );
        }
        editorChoice[0] = JDialogEditor.XML_DOB;
        if ( DOB != null ) {
            dialog.appendSubjectData( "Date of Birth", DOB, editorChoice );
        } else {
            dialog.appendSubjectData( "Date of Birth", "0000-01-01", editorChoice );
        }

        editorChoice[0] = JDialogEditor.STRING;
        dialog.appendSubjectData( "Height", ( new Integer( height ) ).toString(), editorChoice );
        dialog.appendSubjectData( "Weight", ( new Integer( weight ) ).toString(), editorChoice );
        editorChoice[0] = JDialogEditor.XML_SEX;
        if ( sex != null ) {
            dialog.appendSubjectData( "Sex", sex, editorChoice );
        } else {
            dialog.appendSubjectData( "Sex", "Unknown", editorChoice );
        }
        editorChoice[0] = JDialogEditor.STRING;
        if ( bodyPart != null ) {
            dialog.appendSubjectData( "Body Part", bodyPart, editorChoice );
        } else {
            dialog.appendSubjectData( "Body Part", emptyString, editorChoice );
        }

        // Scan Attributes
        editorChoice[0] = JDialogEditor.STRING;
        if ( equipmentName != null ) {
            dialog.appendScanData( "Equipment Model Name", equipmentName, editorChoice );
        } else {
            dialog.appendScanData( "Equipment Model Name", emptyString, editorChoice );
        }
        if ( scanID != null ) {
            dialog.appendScanData( "Scan ID", scanID, editorChoice );
        } else {
            dialog.appendScanData( "Scan ID", emptyString, editorChoice );
        }
        if ( protocol != null ) {
            dialog.appendScanData( "Protocol", protocol, editorChoice );
        } else {
            dialog.appendScanData( "Protocol", emptyString, editorChoice );
        }
        editorChoice[0] = JDialogEditor.XML_DATE;
        if ( scanDate != null ) {
            dialog.appendScanData( "Scan Date", scanDate, editorChoice );
        } else {
            dialog.appendScanData( "Scan Date", "0000-01-01", editorChoice );
        }
        editorChoice[0] = JDialogEditor.XML_TIME;
        if ( scanTime != null ) {
            dialog.appendScanData( "Scan Time", scanTime, editorChoice );
        } else {
            dialog.appendScanData( "Scan Time", "00:00:00-00:00", editorChoice );
        }

        // Investigator Data
        for ( int j = 0; j < 3; j++ ) {
            editorChoice[0] = JDialogEditor.STRING;
            boolean isNull = ( investigators[j] == null );

            //name is required if investigator data exists.
            //  everything else is optional
            if ( !isNull ) {
                dialog.appendInvestigatorData( ( j + 1 ) + " Name", investigators[j].getName(), editorChoice );
            } else {
                dialog.appendInvestigatorData( ( j + 1 ) + " Name", emptyString, editorChoice );
            }
            if ( !isNull && investigators[j].getTitle() != null ) {
                dialog.appendInvestigatorData( ( j + 1 ) + " Title", investigators[j].getTitle(), editorChoice );
            } else {
                dialog.appendInvestigatorData( ( j + 1 ) + " Title", emptyString, editorChoice );
            }

            if ( !isNull && investigators[j].getAffiliation() != null ) {
                dialog.appendInvestigatorData( ( j + 1 ) + " Affiliation", investigators[j].getAffiliation(),
                        editorChoice );
            } else {
                dialog.appendInvestigatorData( ( j + 1 ) + " Affiliation", emptyString, editorChoice );
            }

            if ( !isNull && investigators[j].getEmail() != null ) {
                dialog.appendInvestigatorData( ( j + 1 ) + " Email", investigators[j].getEmail(), editorChoice );
            } else {
                dialog.appendInvestigatorData( ( j + 1 ) + " Email", emptyString, editorChoice );
            }

            if ( !isNull && investigators[j].getPhone() != null ) {
                dialog.appendInvestigatorData( ( j + 1 ) + " Phone", investigators[j].getPhone(), editorChoice );
            } else {
                dialog.appendInvestigatorData( ( j + 1 ) + " Phone", emptyString, editorChoice );
            }

        }

        Enumeration e = getPSetKeys();

        while ( e.hasMoreElements() ) {
            PSet temp = getPSet( (String) e.nextElement() );
            String desc = temp.getDescription();
            Enumeration pe = temp.getParameterKeys();

            while ( pe.hasMoreElements() ) {
                Parameter tp = temp.getParameter( (String) pe.nextElement() );

                dialog.appendParameter( desc, tp.getName(), tp.getDescription(), tp.getValueType(), tp.getValue(),
                        tp.getDate(), tp.getTime() );
            }
        }
    }


    /**
     *   Used to propogate all fileInfoXML private variables
     *   to other fileinfos
     *   @param fInfo FileInfoXML file info to be copied into
     */
    public void updateFileInfos( FileInfoXML fInfo ) {
        if ( this == fInfo ) {
            return;
        }

        ((FileInfoImageXML)fInfo).setImageDescription( this.getImageDescription() );
        ((FileInfoImageXML)fInfo).setLinkedImagePath( this.getLinkedImagePath() );
        ((FileInfoImageXML)fInfo).setModality( this.getModality() );
        ((FileInfoImageXML)fInfo).setSubjectName( this.getSubjectName() );
        ((FileInfoImageXML)fInfo).setSubjectID( this.getSubjectID() );
        ((FileInfoImageXML)fInfo).setRace( this.getRace() );
        ((FileInfoImageXML)fInfo).setDiagnosis( this.getDiagnosis() );
        ((FileInfoImageXML)fInfo).setDOB( this.getDOB() );
        ((FileInfoImageXML)fInfo).setHeight( this.getHeight() );
        ((FileInfoImageXML)fInfo).setWeight( this.getWeight() );
        ((FileInfoImageXML)fInfo).setSex( this.getSex() );
        ((FileInfoImageXML)fInfo).setBodyPart( this.getBodyPart() );
        ((FileInfoImageXML)fInfo).setEquipmentName( this.getEquipmentName() );
        ((FileInfoImageXML)fInfo).setScanID( this.getScanID() );
        ((FileInfoImageXML)fInfo).setProtocol( this.getProtocol() );
        ((FileInfoImageXML)fInfo).setScanDate( this.getScanDate() );
        ((FileInfoImageXML)fInfo).setScanTime( this.getScanTime() );
        ((FileInfoImageXML)fInfo).setInvestigators( this.getInvestigators() );
        ((FileInfoImageXML)fInfo).setInvestigatorsComplete( this.getInvestigatorsComplete() );
        ((FileInfoImageXML)fInfo).setPSetHashtable( this.getPSetHashtable() );
        ((FileInfoImageXML)fInfo).setSurfaces( this.getSurfaces() );
    }

    /**
     *   Updates the FileInfo with new parameter data
     *   contained within the vector passed in
     *   [0] is the set description,
     *   [1] is the name of the parameter to be changed
     *   [2] is the description of the parameter
     *   [3] is the value type of the parameter
     *   [4] is the date for the parameter
     *   [5] is the time for the parameter
     *   @param pData Vector Vector of changed parameter data + set description
     */
    public void parameterChanged( Vector pData ) {
        String setDesc = (String) pData.elementAt( 0 );
        String name = (String) pData.elementAt( 1 );
        String paramdesc = (String) pData.elementAt( 2 );
        String vt = (String) pData.elementAt( 3 );
        String val = (String) pData.elementAt( 4 );
        String date = (String) pData.elementAt( 5 );
        String time = (String) pData.elementAt( 6 );

        getPSet( setDesc ).getParameter( name ).setDescription( paramdesc );
        getPSet( setDesc ).getParameter( name ).setValueType( vt );
        getPSet( setDesc ).getParameter( name ).setValue( val );
        getPSet( setDesc ).getParameter( name ).setDate( date );
        getPSet( setDesc ).getParameter( name ).setTime( time );

    }

    /**
     *   Updates the fileinfo with changes made within the JDialogs
     *   @param ce Vector Vector of new data
     */
    public void stateChanged( Vector ce ) {
        String tname = (String) ce.elementAt( 2 ); // [t]able [name]
        Vector tcvalue = (Vector) ce.elementAt( 3 ); // [t]able [c]ode [value]
        String tvalue = (String) ce.elementAt( 4 ); // [t]able [value]

        if ( tname.equalsIgnoreCase( "Description" ) ) {
            setImageDescription( tvalue );
        } else if ( tname.equalsIgnoreCase( "Linked-image" ) ) {
            setLinkedImagePath( tvalue );
        } else if ( tname.equalsIgnoreCase( "Modality" ) ) {
            setModality( FileInfoBase.getModalityFromStr( tvalue ) );
        } else if ( tname.equalsIgnoreCase( "Subject Name" ) ) {
            setSubjectName( tvalue );
        } else if ( tname.equalsIgnoreCase( "Subject ID" ) ) {
            setSubjectID( tvalue );
        } else if ( tname.equalsIgnoreCase( "Race" ) ) {
            setRace( tvalue );
        } else if ( tname.equalsIgnoreCase( "Diagnosis" ) ) {
            setDiagnosis( tvalue );
        } else if ( tname.equalsIgnoreCase( "Date of Birth" ) ) {
            setDOB( tvalue );
        } else if ( tname.equalsIgnoreCase( "Height" ) ) {
            setHeight( Integer.valueOf( tvalue ).intValue() );
        } else if ( tname.equalsIgnoreCase( "Weight" ) ) {
            setWeight( Integer.valueOf( tvalue ).intValue() );
        } else if ( tname.equalsIgnoreCase( "Sex" ) ) {
            setSex( tvalue );
        } else if ( tname.equalsIgnoreCase( "Body Part" ) ) {
            setBodyPart( tvalue );
        } else if ( tname.equalsIgnoreCase( "Equipment Model Name" ) ) {
            setEquipmentName( tvalue );
        } else if ( tname.equalsIgnoreCase( "Scan ID" ) ) {
            setScanID( tvalue );
        } else if ( tname.equalsIgnoreCase( "Protocol" ) ) {
            setProtocol( tvalue );
        } else if ( tname.equalsIgnoreCase( "Scan Date" ) ) {
            setScanDate( tvalue );
        } else if ( tname.equalsIgnoreCase( "Scan Time" ) ) {
            setScanTime( tvalue );
        } else if ( tname.endsWith( " Name" ) ) {
            int index = Integer.valueOf( tname.substring( 0, 1 ) ).intValue();

            setInvestigatorName( tvalue, index );
        } else if ( tname.endsWith( " Title" ) ) {
            int index = Integer.valueOf( tname.substring( 0, 1 ) ).intValue();

            setTitle( tvalue, index );
        } else if ( tname.endsWith( " Affiliation" ) ) {
            int index = Integer.valueOf( tname.substring( 0, 1 ) ).intValue();

            setAffiliation( tvalue, index );
        } else if ( tname.endsWith( " Email" ) ) {
            int index = Integer.valueOf( tname.substring( 0, 1 ) ).intValue();

            setEmail( tvalue, index );
        } else if ( tname.endsWith( " Phone" ) ) {
            int index = Integer.valueOf( tname.substring( 0, 1 ) ).intValue();

            setPhone( tvalue, index );
        } else {
            System.out.println( "FileInfoXML.stateChanged: " + tvalue + " not yet supported!" );
        }
    }
}
