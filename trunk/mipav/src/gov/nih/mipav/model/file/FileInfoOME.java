package gov.nih.mipav.model.file;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.dialogs.*;

import java.net.*;

import java.util.*;


/**
 * <p>Title:</p>
 *
 * <p>Description:</p>
 *
 * <p>Copyright: Copyright (c) 2003</p>
 *
 * <p>Company:</p>
 *
 * @author   not attributable
 * @version  1.0
 */

public class FileInfoOME extends FileInfoBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -4798053431303160065L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private String headerFileName;

    /** DOCUMENT ME! */
    private TransMatrix matrix;

    /** DOCUMENT ME! */
    private OME ome;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new FileInfoOME object.
     *
     * @param  name       DOCUMENT ME!
     * @param  directory  DOCUMENT ME!
     * @param  format     DOCUMENT ME!
     */
    public FileInfoOME(String name, String directory, int format) {
        super(name, directory, format);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------


    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public OME createOME() {
        return new OME();
    }

    /**
     * DOCUMENT ME!
     *
     * @param  dialog  DOCUMENT ME!
     * @param  matrix  DOCUMENT ME!
     */
    public void displayAboutInfo(JDialogBase dialog, TransMatrix matrix) {

        /**
         * @todo  Implement this gov.nih.mipav.model.file.FileInfoBase abstract method
         */
    }

    /**
     * Gets the transition matrix.
     *
     * @return  matrix
     */
    public TransMatrix getMatrix() {
        return this.matrix;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public OME getOME() {
        return ome;
    }

    /**
     * Sets the name of the header file.
     *
     * @param  headerFileName  header file name (not path(
     */
    public void setHeaderFileName(String headerFileName) {
        this.headerFileName = headerFileName;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  ome  DOCUMENT ME!
     */
    public void setOME(OME ome) {
        this.ome = ome;
    }

    //~ Inner Classes --------------------------------------------------------------------------------------------------

    /**
     * DOCUMENT ME!
     */
    public class ExperimenterRef {

        /** DOCUMENT ME! */
        private Integer documentRef;

        /** DOCUMENT ME! */
        private Integer experimentID;

        /**
         * Creates a new ExperimenterRef object.
         *
         * @param  documentRef  DOCUMENT ME!
         * @param  expID        DOCUMENT ME!
         */
        public ExperimenterRef(Integer documentRef, Integer expID) {
            this.documentRef = documentRef;
            this.experimentID = expID;
        }
    }

    /**
     * DOCUMENT ME!
     */
    public class ExperimentRef {

        /** DOCUMENT ME! */
        private Integer documentRef;

        /** DOCUMENT ME! */
        private String experimentID;

        /**
         * Creates a new ExperimentRef object.
         *
         * @param  docRef  DOCUMENT ME!
         * @param  expID   DOCUMENT ME!
         */
        public ExperimentRef(Integer docRef, String expID) {
            this.documentRef = docRef;
            this.experimentID = expID;
        }
    }

    /**
     * DOCUMENT ME!
     */
    public class OME {

        /** DOCUMENT ME! */
        private Vector datasets; // Vector of Datasets

        /** DOCUMENT ME! */
        private Vector documentGroup; // Vector of Includes

        /** DOCUMENT ME! */
        private Vector experimenters; // Vector of Experimenters

        /** DOCUMENT ME! */
        private Vector experiments; // Vector of Experiments

        /** DOCUMENT ME! */
        private Vector groups; // Vector of Groups

        /** DOCUMENT ME! */
        private Vector images; // Vector of images

        /** DOCUMENT ME! */
        private Vector instruments; // Vector of instruments

        /** DOCUMENT ME! */
        private Vector plates; // Vector of Plates

        /** DOCUMENT ME! */
        private Vector projects; // Vector of Projects

        /** DOCUMENT ME! */
        private Vector screens; // Vector of Screens

        /**
         * DOCUMENT ME!
         *
         * @param  name    DOCUMENT ME!
         * @param  id      DOCUMENT ME!
         * @param  locked  DOCUMENT ME!
         */
        public void addDataset(String name, Integer id, Boolean locked) {

            if (datasets == null) {
                datasets = new Vector();
            }

            datasets.add(new Dataset(name, id, locked));
        }

        /**
         * DOCUMENT ME!
         *
         * @param  id  DOCUMENT ME!
         */
        public void addExperiment(Integer id) {

            if (experiments == null) {
                experiments = new Vector();
            }

            experiments.add(new Experiment(id));
        }

        /**
         * DOCUMENT ME!
         *
         * @param  id  DOCUMENT ME!
         */
        public void addExperimenter(Integer id) {

            if (experimenters == null) {
                experimenters = new Vector();
            }

            experimenters.add(new Experimenter(id));
        }

        /**
         * DOCUMENT ME!
         *
         * @param  name  DOCUMENT ME!
         * @param  id    DOCUMENT ME!
         */
        public void addGroup(String name, Integer id) {

            if (groups == null) {
                groups = new Vector();
            }

            groups.add(new Group(name, id));
        }

        /**
         * DOCUMENT ME!
         *
         * @param  guid         DOCUMENT ME!
         * @param  type         DOCUMENT ME!
         * @param  name         DOCUMENT ME!
         * @param  sizeX        DOCUMENT ME!
         * @param  sizeY        DOCUMENT ME!
         * @param  sizeZ        DOCUMENT ME!
         * @param  numChannels  DOCUMENT ME!
         * @param  numTimes     DOCUMENT ME!
         * @param  pSizeX       DOCUMENT ME!
         * @param  pSizeY       DOCUMENT ME!
         * @param  pSizeZ       DOCUMENT ME!
         * @param  timeInc      DOCUMENT ME!
         * @param  waveS        DOCUMENT ME!
         * @param  waveInc      DOCUMENT ME!
         */
        public void addImage(String guid, String type, String name, Integer sizeX, Integer sizeY, Integer sizeZ,
                             Integer numChannels, Integer numTimes, Float pSizeX, Float pSizeY, Float pSizeZ,
                             Float timeInc, Integer waveS, Integer waveInc) {

            if (images == null) {
                images = new Vector();
            }

            images.add(new Image(guid, type, name, sizeX, sizeY, sizeZ, numChannels, numTimes, pSizeX, pSizeY, pSizeZ,
                                 timeInc, waveS, waveInc));
        }

        /**
         * need these functions to create the classes within..
         *
         * @param  id    DOCUMENT ME!
         * @param  href  DOCUMENT ME!
         * @param  sha1  DOCUMENT ME!
         */
        public void addInclude(Integer id, URI href, String sha1) {

            if (documentGroup == null) {
                documentGroup = new Vector();
            }

            documentGroup.add(new Include(id, href, sha1));
        }

        /**
         * DOCUMENT ME!
         *
         * @param  id  DOCUMENT ME!
         */
        public void addInstrument(Integer id) {

            if (instruments == null) {
                instruments = new Vector();
            }

            instruments.add(new Instrument(id));
        }

        /**
         * DOCUMENT ME!
         *
         * @param  id    DOCUMENT ME!
         * @param  name  DOCUMENT ME!
         * @param  ref   DOCUMENT ME!
         */
        public void addPlate(Integer id, String name, String ref) {

            if (plates == null) {
                plates = new Vector();
            }

            plates.add(new Plate(id, name, ref));
        }

        /**
         * DOCUMENT ME!
         *
         * @param  name  DOCUMENT ME!
         * @param  id    DOCUMENT ME!
         */
        public void addProject(String name, Integer id) {

            if (projects == null) {
                projects = new Vector();
            }

            projects.add(new Project(name, id));
        }

        /**
         * DOCUMENT ME!
         *
         * @param  id    DOCUMENT ME!
         * @param  name  DOCUMENT ME!
         * @param  ref   DOCUMENT ME!
         */
        public void addScreen(Integer id, String name, String ref) {

            if (screens == null) {
                screens = new Vector();
            }

            screens.add(new Screen(id, name, ref));
        }

        /**
         * DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        public Dataset getCurrentDataset() {
            return (Dataset) datasets.lastElement();
        }

        /**
         * DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        public Experiment getCurrentExperiment() {
            return (Experiment) experiments.lastElement();
        }

        /**
         * DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        public Experimenter getCurrentExperimenter() {
            return (Experimenter) experimenters.lastElement();
        }

        /**
         * DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        public Group getCurrentGroup() {
            return (Group) groups.lastElement();
        }

        /**
         * DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        public Image getCurrentImage() {
            return (Image) images.lastElement();
        }

        /**
         * DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        public Include getCurrentInclude() {
            return (Include) documentGroup.lastElement();
        }

        /**
         * DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        public Instrument getCurrentInstrument() {
            return (Instrument) instruments.lastElement();
        }

        /**
         * DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        public Plate getCurrentPlate() {
            return (Plate) plates.lastElement();
        }

        /**
         * DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        public Project getCurrentProject() {
            return (Project) projects.lastElement();
        }

        /**
         * DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        public Screen getCurrentScreen() {
            return (Screen) screens.lastElement();
        }

        /**
         * DOCUMENT ME!
         */
        public class Dataset {

            /** DOCUMENT ME! */
            private String customAttributes;

            /** DOCUMENT ME! */
            private Integer datasetID;

            /** Elements. */
            private String description;

            /** DOCUMENT ME! */
            private ExperimenterRef experimenterRef;

            /** DOCUMENT ME! */
            private GroupRef groupRef;

            /** DOCUMENT ME! */
            private Boolean locked;

            /** Attributes. */
            private String name;

            /** DOCUMENT ME! */
            private Vector projectRefs; // Vector of ProjectsRefs

            /**
             * Creates a new Dataset object.
             *
             * @param  name    DOCUMENT ME!
             * @param  id      DOCUMENT ME!
             * @param  locked  DOCUMENT ME!
             */
            public Dataset(String name, Integer id, Boolean locked) {
                this.name = name;
                this.datasetID = id;
                this.locked = locked;
            }

            /**
             * DOCUMENT ME!
             *
             * @param  docRef  DOCUMENT ME!
             * @param  id      DOCUMENT ME!
             */
            public void addProjectRef(Integer docRef, Integer id) {

                if (projectRefs == null) {
                    projectRefs = new Vector();
                }

                projectRefs.add(new ProjectRef(docRef, id));
            }

            /**
             * DOCUMENT ME!
             *
             * @param  desc  DOCUMENT ME!
             */
            public void setDescription(String desc) {
                this.description = desc;
            }

            /**
             * DOCUMENT ME!
             *
             * @param  docRef  DOCUMENT ME!
             * @param  expID   DOCUMENT ME!
             */
            public void setExperimenterRef(Integer docRef, Integer expID) {
                this.experimenterRef = new ExperimenterRef(docRef, expID);
            }

            /**
             * DOCUMENT ME!
             *
             * @param  id  DOCUMENT ME!
             */
            public void setGroupRef(Integer id) {
                this.groupRef = new GroupRef(id);
            }
        }

        /**
         * DOCUMENT ME!
         */
        public class Experiment {

            /** Elements. */
            private String description;

            /** DOCUMENT ME! */
            private ExperimenterRef experimenterRef;

            /** Attributes. */
            private Integer experimentID;

            /**
             * Creates a new Experiment object.
             *
             * @param  id  DOCUMENT ME!
             */
            public Experiment(Integer id) {
                this.experimentID = id;
            }

            /**
             * DOCUMENT ME!
             *
             * @param  desc  DOCUMENT ME!
             */
            public void setDescription(String desc) {
                this.description = desc;
            }

            /**
             * DOCUMENT ME!
             *
             * @param  docRef  DOCUMENT ME!
             * @param  expID   DOCUMENT ME!
             */
            public void setExperimenterRef(Integer docRef, Integer expID) {
                this.experimenterRef = new ExperimenterRef(docRef, expID);
            }
        }

        /**
         * DOCUMENT ME!
         */
        public class Experimenter {

            /** DOCUMENT ME! */
            private String email;

            /** Attributes. */
            private Integer experimenterID;

            /** Elements. */
            private String firstName;

            /** DOCUMENT ME! */
            private Vector groupRefs; // Vector of GroupRefs

            /** DOCUMENT ME! */
            private String institution;

            /** DOCUMENT ME! */
            private String lastName;

            /** DOCUMENT ME! */
            private String omeName;

            /**
             * Creates a new Experimenter object.
             *
             * @param  id  DOCUMENT ME!
             */
            public Experimenter(Integer id) {
                this.experimenterID = id;
            }

            /**
             * DOCUMENT ME!
             *
             * @param  id  DOCUMENT ME!
             */
            public void addGroupRef(Integer id) {

                if (groupRefs == null) {
                    groupRefs = new Vector();
                }

                groupRefs.add(new GroupRef(id));
            }

            /**
             * DOCUMENT ME!
             *
             * @param  email  DOCUMENT ME!
             */
            public void setEmail(String email) {
                this.email = email;
            }

            /**
             * DOCUMENT ME!
             *
             * @param  first  DOCUMENT ME!
             */
            public void setFirstName(String first) {
                this.firstName = first;
            }

            /**
             * DOCUMENT ME!
             *
             * @param  inst  DOCUMENT ME!
             */
            public void setInstitution(String inst) {
                this.institution = inst;
            }

            /**
             * DOCUMENT ME!
             *
             * @param  last  DOCUMENT ME!
             */
            public void setLastName(String last) {
                this.lastName = last;
            }

            /**
             * DOCUMENT ME!
             *
             * @param  oName  DOCUMENT ME!
             */
            public void setOMEName(String oName) {
                this.omeName = oName;
            }
        }

        /**
         * DOCUMENT ME!
         */
        public class Group {

            /** DOCUMENT ME! */
            private String contactEmail;

            /** DOCUMENT ME! */
            private ExperimenterRef contactExperimenterRef;

            /** DOCUMENT ME! */
            private String contactFirstName;

            /** DOCUMENT ME! */
            private String contactInstitution;

            /** DOCUMENT ME! */
            private Boolean contactIsPerson; // is the contact data a person or a ref

            /** DOCUMENT ME! */
            private String contactLastName;

            /** DOCUMENT ME! */
            private Integer groupID;

            /** DOCUMENT ME! */
            private String leaderEmail;

            /** DOCUMENT ME! */
            private ExperimenterRef leaderExperimenterRef;

            /** DOCUMENT ME! */
            private String leaderFirstName;

            /** DOCUMENT ME! */
            private String leaderInstitution;

            /** DOCUMENT ME! */
            private Boolean leaderIsPerson; // is the leader data a person or a ref

            /** DOCUMENT ME! */
            private String leaderLastName;

            /** Attributes. */
            private String name;

            /**
             * Creates a new Group object.
             *
             * @param  name  DOCUMENT ME!
             * @param  id    DOCUMENT ME!
             */
            public Group(String name, Integer id) {
                this.name = name;
                this.groupID = id;
            }

            /**
             * DOCUMENT ME!
             *
             * @param  email  DOCUMENT ME!
             */
            public void setContactEmail(String email) {
                this.contactEmail = email;
            }

            /**
             * DOCUMENT ME!
             *
             * @param  docRef  DOCUMENT ME!
             * @param  expID   DOCUMENT ME!
             */
            public void setContactExperimenterRef(Integer docRef, Integer expID) {
                this.contactExperimenterRef = new ExperimenterRef(docRef, expID);
            }

            /**
             * DOCUMENT ME!
             *
             * @param  first  DOCUMENT ME!
             */
            public void setContactFirstName(String first) {
                this.contactIsPerson = new Boolean("true");
                this.contactFirstName = first;
            }

            /**
             * DOCUMENT ME!
             *
             * @param  inst  DOCUMENT ME!
             */
            public void setContactInstitution(String inst) {
                this.contactInstitution = inst;
            }

            /**
             * DOCUMENT ME!
             *
             * @param  last  DOCUMENT ME!
             */
            public void setContactLastName(String last) {
                this.contactLastName = last;
            }

            /**
             * DOCUMENT ME!
             *
             * @param  email  DOCUMENT ME!
             */
            public void setLeaderEmail(String email) {
                this.leaderEmail = email;
            }

            /**
             * DOCUMENT ME!
             *
             * @param  docRef  DOCUMENT ME!
             * @param  expID   DOCUMENT ME!
             */
            public void setLeaderExperimenterRef(Integer docRef, Integer expID) {
                this.leaderExperimenterRef = new ExperimenterRef(docRef, expID);
            }

            /**
             * DOCUMENT ME!
             *
             * @param  first  DOCUMENT ME!
             */
            public void setLeaderFirstName(String first) {
                leaderIsPerson = new Boolean("true");
                this.leaderFirstName = first;
            }

            /**
             * DOCUMENT ME!
             *
             * @param  inst  DOCUMENT ME!
             */
            public void setLeaderInstitution(String inst) {
                this.leaderInstitution = inst;
            }

            /**
             * DOCUMENT ME!
             *
             * @param  last  DOCUMENT ME!
             */
            public void setLeaderLastName(String last) {
                this.leaderLastName = last;
            }
        }

        /**
         * DOCUMENT ME!
         */
        public class GroupRef {

            /** Attributes. */
            private Integer groupID;

            /**
             * Creates a new GroupRef object.
             *
             * @param  id  DOCUMENT ME!
             */
            public GroupRef(Integer id) {
                this.groupID = id;
            }
        }

        /**
         * DOCUMENT ME!
         */
        public class Image {

            /** DOCUMENT ME! */
            private Float airPressure;

            /** DOCUMENT ME! */
            private Vector channelInfos; // Vector of channel infos

            /** DOCUMENT ME! */
            private Float cO2Percent; // percentage

            /** Elements. */
            private String creationDate;

            /** DOCUMENT ME! */
            private Data data;

            /** DOCUMENT ME! */
            private Vector datasetRefs; // Vector for DatasetRefs (Integers)

            /** DOCUMENT ME! */
            private String derivedImage;

            /** DOCUMENT ME! */
            private String description;

            /** DOCUMENT ME! */
            private DisplayOptions displayOptions;

            /** DOCUMENT ME! */
            private ExperimentRef experimentRef;

            /** DOCUMENT ME! */
            private GroupRef groupRef;

            /** Attributes. */
            private String guid; // ?

            /** DOCUMENT ME! */
            private Float humidity; // percentage

            /** DOCUMENT ME! */
            private String imageType;

            /** DOCUMENT ME! */
            private InstrumentRef instrumentRef;

            /** DOCUMENT ME! */
            private String mimeType; // for thumbnail

            /** DOCUMENT ME! */
            private String name;

            /** DOCUMENT ME! */
            private Integer numChannels;

            /** DOCUMENT ME! */
            private Integer numTimes;

            /** DOCUMENT ME! */
            private Float pixelSizeX;

            /** DOCUMENT ME! */
            private Float pixelSizeY;

            /** DOCUMENT ME! */
            private Float pixelSizeZ;

            /** DOCUMENT ME! */
            private PlateInfo plateInfo;

            /** DOCUMENT ME! */
            private Integer sizeX;

            /** DOCUMENT ME! */
            private Integer sizeY;

            /** DOCUMENT ME! */
            private Integer sizeZ;

            /** DOCUMENT ME! */
            private StageLabel stageLabel;

            /** For Imaging Environment. */
            private Float temperature;

            /** DOCUMENT ME! */
            private URI thumbnail; // web location of thumbnail pic

            /** DOCUMENT ME! */
            private Float timeIncrement;

            /** DOCUMENT ME! */
            private Integer waveIncrement;

            /** DOCUMENT ME! */
            private Integer waveStart;

            /**
             * Creates a new Image object.
             *
             * @param  guid         DOCUMENT ME!
             * @param  type         DOCUMENT ME!
             * @param  name         DOCUMENT ME!
             * @param  sizeX        DOCUMENT ME!
             * @param  sizeY        DOCUMENT ME!
             * @param  sizeZ        DOCUMENT ME!
             * @param  numChannels  DOCUMENT ME!
             * @param  numTimes     DOCUMENT ME!
             * @param  pSizeX       DOCUMENT ME!
             * @param  pSizeY       DOCUMENT ME!
             * @param  pSizeZ       DOCUMENT ME!
             * @param  ti           DOCUMENT ME!
             * @param  ws           DOCUMENT ME!
             * @param  wi           DOCUMENT ME!
             */
            public Image(String guid, String type, String name, Integer sizeX, Integer sizeY, Integer sizeZ,
                         Integer numChannels, Integer numTimes, Float pSizeX, Float pSizeY, Float pSizeZ, Float ti,
                         Integer ws, Integer wi) {
                this.guid = guid;
                this.imageType = type;
                this.name = name;
                this.sizeX = sizeX;
                this.sizeY = sizeY;
                this.sizeZ = sizeZ;
                this.numChannels = numChannels;
                this.numTimes = numTimes;
                this.pixelSizeX = pSizeX;
                this.pixelSizeY = pSizeY;
                this.pixelSizeZ = pSizeZ;
                this.timeIncrement = ti;
                this.waveStart = ws;
                this.waveIncrement = wi;

                // Set the extents based on sizeX, sizeY, and sizeZ
                int[] extents = null;

                if (sizeZ.intValue() > 1) {

                    if (numTimes.intValue() > 1) {
                        extents = new int[4];
                        extents[3] = numTimes.intValue();
                    } else {
                        extents = new int[3];
                    }

                    extents[2] = sizeZ.intValue();
                } else {

                    if (numTimes.intValue() > 1) {
                        extents = new int[3];
                        extents[2] = numTimes.intValue();
                    } else {
                        extents = new int[2];
                    }
                }

                extents[0] = sizeX.intValue();
                extents[1] = sizeY.intValue();
                setExtents(extents);

                // Set the resolutions based on pixelSizeX,Y,Z and timeIncrement
                float[] res = new float[extents.length];

                if (res.length == 4) {
                    res[2] = pixelSizeZ.floatValue();

                    if (timeIncrement != null) {
                        res[3] = timeIncrement.floatValue();
                    } else {
                        res[3] = 1.0f;
                    }
                } else if (res.length == 3) {

                    if (!(numTimes.intValue() > 1)) {
                        res[2] = pixelSizeZ.floatValue();
                    } else if (timeIncrement != null) {
                        res[2] = timeIncrement.floatValue();
                    } else {
                        res[2] = 1.0f;
                    }
                }

                res[0] = pixelSizeX.floatValue();
                res[1] = pixelSizeY.floatValue();
            }

            /**
             * DOCUMENT ME!
             *
             * @param  name            DOCUMENT ME!
             * @param  spp             DOCUMENT ME!
             * @param  illType         DOCUMENT ME!
             * @param  phs             DOCUMENT ME!
             * @param  photoInt        DOCUMENT ME!
             * @param  mode            DOCUMENT ME!
             * @param  contrastMethod  DOCUMENT ME!
             * @param  exWave          DOCUMENT ME!
             * @param  emWave          DOCUMENT ME!
             * @param  fluor           DOCUMENT ME!
             * @param  ndFilter        DOCUMENT ME!
             */
            public void addChannelInfo(String name, Integer spp, String illType, Integer phs, String photoInt,
                                       String mode, String contrastMethod, Integer exWave, Integer emWave, String fluor,
                                       Float ndFilter) {

                if (channelInfos == null) {
                    channelInfos = new Vector();
                }

                channelInfos.add(new ChannelInfo(name, spp, illType, phs, photoInt, mode, contrastMethod, exWave,
                                                 emWave, fluor, ndFilter));
            }

            /**
             * DOCUMENT ME!
             *
             * @param  datasetID  DOCUMENT ME!
             */
            public void addDatasetRef(Integer datasetID) {

                if (datasetRefs == null) {
                    datasetRefs = new Vector();
                }

                datasetRefs.add(datasetID);
            }

            /**
             * DOCUMENT ME!
             *
             * @return  DOCUMENT ME!
             */
            public ChannelInfo getCurrentChannelInfo() {
                return (ChannelInfo) channelInfos.lastElement();
            }

            /**
             * DOCUMENT ME!
             *
             * @return  DOCUMENT ME!
             */
            public Data getData() {
                return this.data;
            }

            /**
             * DOCUMENT ME!
             *
             * @return  DOCUMENT ME!
             */
            public DisplayOptions getDisplayOptions() {
                return displayOptions;
            }

            /**
             * DOCUMENT ME!
             *
             * @return  DOCUMENT ME!
             */
            public PlateInfo getPlateInfo() {
                return plateInfo;
            }

            /**
             * DOCUMENT ME!
             *
             * @param  ap  DOCUMENT ME!
             */
            public void setAirPressure(Float ap) {
                this.airPressure = ap;
            }

            /**
             * DOCUMENT ME!
             *
             * @param  cO2  DOCUMENT ME!
             */
            public void setCO2Percent(Float cO2) {
                this.cO2Percent = cO2;
            }

            /**
             * DOCUMENT ME!
             *
             * @param  date  DOCUMENT ME!
             */
            public void setCreationDate(String date) {
                this.creationDate = date;
            }

            /**
             * DOCUMENT ME!
             *
             * @param  dimOrd  DOCUMENT ME!
             * @param  pt      DOCUMENT ME!
             * @param  bigEnd  DOCUMENT ME!
             */
            public void setData(String dimOrd, String pt, Boolean bigEnd) {
                data = new Data(dimOrd, pt, bigEnd);
            }

            /**
             * DOCUMENT ME!
             *
             * @param  derImage  DOCUMENT ME!
             */
            public void setDerivedImage(String derImage) {
                this.derivedImage = derImage;
            }

            /**
             * DOCUMENT ME!
             *
             * @param  desc  DOCUMENT ME!
             */
            public void setDescription(String desc) {
                this.description = desc;
            }

            /**
             * DOCUMENT ME!
             *
             * @param  zoom  DOCUMENT ME!
             */
            public void setDisplayOptions(Float zoom) {
                displayOptions = new DisplayOptions(zoom);
            }

            /**
             * DOCUMENT ME!
             *
             * @param  docRef        DOCUMENT ME!
             * @param  experimentID  DOCUMENT ME!
             */
            public void setExperimentRef(Integer docRef, String experimentID) {
                this.experimentRef = new ExperimentRef(docRef, experimentID);
            }

            /**
             * DOCUMENT ME!
             *
             * @param  id  DOCUMENT ME!
             */
            public void setGroupRef(Integer id) {
                this.groupRef = new GroupRef(id);
            }

            /**
             * DOCUMENT ME!
             *
             * @param  hum  DOCUMENT ME!
             */
            public void setHumidity(Float hum) {
                this.humidity = hum;
            }

            /**
             * DOCUMENT ME!
             *
             * @param  docRef  DOCUMENT ME!
             * @param  instID  DOCUMENT ME!
             * @param  objID   DOCUMENT ME!
             */
            public void setInstrumentRef(Integer docRef, Integer instID, Integer objID) {
                this.instrumentRef = new InstrumentRef(docRef, instID, objID);
            }

            /**
             * DOCUMENT ME!
             *
             * @param  mime  DOCUMENT ME!
             */
            public void setMIMEType(String mime) {
                this.mimeType = mime;
            }

            /**
             * DOCUMENT ME!
             *
             * @param  id  DOCUMENT ME!
             */
            public void setPlateInfo(Integer id) {
                plateInfo = new PlateInfo(id);
            }

            /**
             * DOCUMENT ME!
             *
             * @param  name  DOCUMENT ME!
             * @param  x     DOCUMENT ME!
             * @param  y     DOCUMENT ME!
             * @param  z     DOCUMENT ME!
             */
            public void setStageLabel(String name, Float x, Float y, Float z) {
                stageLabel = new StageLabel(name, x, y, z);
            }

            /**
             * DOCUMENT ME!
             *
             * @param  temp  DOCUMENT ME!
             */
            public void setTemperature(Float temp) {
                this.temperature = temp;
            }

            /**
             * DOCUMENT ME!
             *
             * @param  tn  DOCUMENT ME!
             */
            public void setThumbnail(URI tn) {
                this.thumbnail = tn;
            }

            /**
             * DOCUMENT ME!
             */
            public class ChannelInfo {

                /** DOCUMENT ME! */
                private Vector channelComponents; // Vector of channel components

                /** DOCUMENT ME! */
                private String contrastMethod;

                /** DOCUMENT ME! */
                private Integer detectorRefDetectorID;

                /** DetectorRef. */
                private Integer detectorRefDocumentRef;

                /** DOCUMENT ME! */
                private Float detectorRefGain;

                /** DOCUMENT ME! */
                private Float detectorRefOffset;

                /** DOCUMENT ME! */
                private Integer emWave;

                /** DOCUMENT ME! */
                private Integer exWave;

                /** FilterRef. */
                private Integer filterRefDocumentRef;

                /** DOCUMENT ME! */
                private Integer filterRefFilterID;

                /** DOCUMENT ME! */
                private String fluor;

                /** DOCUMENT ME! */
                private String illuminationType;

                /** Elements. */
                private Vector lightSourceRefs; // Vector of light source refs

                /** DOCUMENT ME! */
                private String mode;

                /** attributes. */
                private String name;

                /** DOCUMENT ME! */
                private Float ndFilter;

                /** OTF Ref. */
                private Integer otfRefDocumentRef;

                /** DOCUMENT ME! */
                private Integer otfRefOTFID;

                /** DOCUMENT ME! */
                private String photometricInterpretation;

                /** DOCUMENT ME! */
                private Integer pinholeSize;

                /** DOCUMENT ME! */
                private Integer samplesPerPixel;

                /**
                 * Creates a new ChannelInfo object.
                 *
                 * @param  name            DOCUMENT ME!
                 * @param  spp             DOCUMENT ME!
                 * @param  illType         DOCUMENT ME!
                 * @param  phs             DOCUMENT ME!
                 * @param  photoInt        DOCUMENT ME!
                 * @param  mode            DOCUMENT ME!
                 * @param  contrastMethod  DOCUMENT ME!
                 * @param  exWave          DOCUMENT ME!
                 * @param  emWave          DOCUMENT ME!
                 * @param  fluor           DOCUMENT ME!
                 * @param  ndFilter        DOCUMENT ME!
                 */
                public ChannelInfo(String name, Integer spp, String illType, Integer phs, String photoInt, String mode,
                                   String contrastMethod, Integer exWave, Integer emWave, String fluor,
                                   Float ndFilter) {
                    this.name = name;
                    this.samplesPerPixel = spp;
                    this.illuminationType = illType;
                    this.pinholeSize = phs;
                    this.photometricInterpretation = photoInt;
                    this.mode = mode;
                    this.contrastMethod = contrastMethod;
                    this.exWave = exWave;
                    this.emWave = emWave;
                    this.fluor = fluor;
                    this.ndFilter = ndFilter;
                }

                /**
                 * DOCUMENT ME!
                 *
                 * @param  cd   DOCUMENT ME!
                 * @param  ccn  DOCUMENT ME!
                 */
                public void addChannelComponent(String cd, Integer ccn) {

                    if (channelComponents == null) {
                        channelComponents = new Vector();
                    }

                    channelComponents.add(new ChannelComponent(cd, ccn));
                }

                /**
                 * DOCUMENT ME!
                 *
                 * @param  docRef   DOCUMENT ME!
                 * @param  id       DOCUMENT ME!
                 * @param  auxTech  DOCUMENT ME!
                 * @param  atten    DOCUMENT ME!
                 * @param  wl       DOCUMENT ME!
                 */
                public void addLightSourceRef(Integer docRef, Integer id, String auxTech, Float atten, Integer wl) {

                    if (lightSourceRefs == null) {
                        lightSourceRefs = new Vector();
                    }

                    lightSourceRefs.add(new LightSourceRef(docRef, id, auxTech, atten, wl));
                }

                /**
                 * DOCUMENT ME!
                 *
                 * @param  id  DOCUMENT ME!
                 */
                public void setDetectorRefDetectorID(Integer id) {
                    this.detectorRefDetectorID = id;
                }

                /**
                 * DOCUMENT ME!
                 *
                 * @param  docRef  DOCUMENT ME!
                 */
                public void setDetectorRefDocumentRef(Integer docRef) {
                    this.detectorRefDocumentRef = docRef;
                }

                /**
                 * DOCUMENT ME!
                 *
                 * @param  gain  DOCUMENT ME!
                 */
                public void setDetectorRefGain(Float gain) {
                    this.detectorRefGain = gain;
                }

                /**
                 * DOCUMENT ME!
                 *
                 * @param  offset  DOCUMENT ME!
                 */
                public void setDetectorRefOffset(Float offset) {
                    this.detectorRefOffset = offset;
                }

                /**
                 * DOCUMENT ME!
                 *
                 * @param  docRef  DOCUMENT ME!
                 */
                public void setFilterRefDocumentRef(Integer docRef) {
                    this.filterRefDocumentRef = docRef;
                }

                /**
                 * DOCUMENT ME!
                 *
                 * @param  id  DOCUMENT ME!
                 */
                public void setFilterRefFilterID(Integer id) {
                    this.filterRefFilterID = id;
                }

                /**
                 * DOCUMENT ME!
                 *
                 * @param  docRef  DOCUMENT ME!
                 */
                public void setOTFRefDocumentRef(Integer docRef) {
                    this.otfRefDocumentRef = docRef;
                }

                /**
                 * DOCUMENT ME!
                 *
                 * @param  id  DOCUMENT ME!
                 */
                public void setOTFRefOTFID(Integer id) {
                    this.otfRefOTFID = id;
                }

                /**
                 * DOCUMENT ME!
                 */
                public class ChannelComponent {

                    /** DOCUMENT ME! */
                    private Integer channelComponentNumber;

                    /** DOCUMENT ME! */
                    private String colorDomain;

                    /**
                     * Creates a new ChannelComponent object.
                     *
                     * @param  cd   DOCUMENT ME!
                     * @param  ccn  DOCUMENT ME!
                     */
                    public ChannelComponent(String cd, Integer ccn) {
                        this.colorDomain = cd;
                        this.channelComponentNumber = ccn;
                    }
                }

                /**
                 * DOCUMENT ME!
                 */
                public class LightSourceRef {

                    /** DOCUMENT ME! */
                    private Float attenuation; // percent fraction

                    /** DOCUMENT ME! */
                    private String auxTechnique;

                    /** attributes. */
                    private Integer documentRef;

                    /** DOCUMENT ME! */
                    private Integer lightSourceID;

                    /** DOCUMENT ME! */
                    private Integer wavelength;

                    /**
                     * Creates a new LightSourceRef object.
                     *
                     * @param  docRef   DOCUMENT ME!
                     * @param  id       DOCUMENT ME!
                     * @param  auxTech  DOCUMENT ME!
                     * @param  atten    DOCUMENT ME!
                     * @param  wl       DOCUMENT ME!
                     */
                    public LightSourceRef(Integer docRef, Integer id, String auxTech, Float atten, Integer wl) {
                        this.documentRef = docRef;
                        this.lightSourceID = id;
                        this.auxTechnique = auxTech;
                        this.attenuation = atten;
                        this.wavelength = wl;
                    }
                }
            }

            /**
             * DOCUMENT ME!
             */
            public class Data {

                /** DOCUMENT ME! */
                private Boolean bigEndian;

                /** Elements. */
                private BinExternal binExt;

                /** Attributes. */
                private String dimensionOrder;

                /** DOCUMENT ME! */
                private String pixelType;

                /**
                 * Creates a new Data object.
                 *
                 * @param  dimOrd  DOCUMENT ME!
                 * @param  pt      DOCUMENT ME!
                 * @param  be      DOCUMENT ME!
                 */
                public Data(String dimOrd, String pt, Boolean be) {
                    this.dimensionOrder = dimOrd;
                    this.pixelType = pt;
                    this.bigEndian = be;
                }

                /**
                 * DOCUMENT ME!
                 *
                 * @param  comp    DOCUMENT ME!
                 * @param  s1      DOCUMENT ME!
                 * @param  offset  DOCUMENT ME!
                 * @param  hRef    DOCUMENT ME!
                 */
                public void setBinExternal(String comp, String s1, Integer offset, URI hRef) {
                    this.binExt = new BinExternal(comp, s1, offset, hRef);
                }

                /**
                 * DOCUMENT ME!
                 */
                public class BinExternal {

                    /** DOCUMENT ME! */
                    private String compression;

                    /** DOCUMENT ME! */
                    private URI hRef;

                    /** DOCUMENT ME! */
                    private Integer offset;

                    /** DOCUMENT ME! */
                    private String sha1;

                    /**
                     * Creates a new BinExternal object.
                     *
                     * @param  comp    DOCUMENT ME!
                     * @param  s1      DOCUMENT ME!
                     * @param  offset  DOCUMENT ME!
                     * @param  hRef    DOCUMENT ME!
                     */
                    public BinExternal(String comp, String s1, Integer offset, URI hRef) {
                        this.compression = comp;
                        this.sha1 = s1;
                        this.offset = offset;
                        this.hRef = hRef;
                    }
                }
            }

            /**
             * DOCUMENT ME!
             */
            public class DisplayOptions {

                /** DOCUMENT ME! */
                private Integer blueBlackLevel;

                /** DOCUMENT ME! */
                private Integer blueChannelNumber;

                /** DOCUMENT ME! */
                private Float blueGamma;

                /** DOCUMENT ME! */
                private Integer blueWhiteLevel;

                /** DOCUMENT ME! */
                private Integer greenBlackLevel;

                /** DOCUMENT ME! */
                private Integer greenChannelNumber;

                /** DOCUMENT ME! */
                private Float greenGamma;

                /** DOCUMENT ME! */
                private Integer greenWhiteLevel;

                /** DOCUMENT ME! */
                private Integer greyBlackLevel;

                /** for Grey:. */
                private Integer greyChannelNumber;

                /** DOCUMENT ME! */
                private String greyColorMap;

                /** DOCUMENT ME! */
                private Float greyGamma;

                /** DOCUMENT ME! */
                private Integer greyWhiteLevel;

                /** DOCUMENT ME! */
                private Integer redBlackLevel;

                /** either RGB channels or Grey Channel... for RGB: */
                private Integer redChannelNumber;

                /** DOCUMENT ME! */
                private Float redGamma;

                /** DOCUMENT ME! */
                private Integer redWhiteLevel;

                /** DOCUMENT ME! */
                private Vector rois; // Vector of ROIs

                /** Time. */
                private Integer tStart;

                /** DOCUMENT ME! */
                private Integer tStop;

                /** DOCUMENT ME! */
                private Float zoom;

                /** for Projection. */
                private Integer zStart;

                /** DOCUMENT ME! */
                private Integer zStop;

                /**
                 * Creates a new DisplayOptions object.
                 *
                 * @param  zoom  DOCUMENT ME!
                 */
                public DisplayOptions(Float zoom) {
                    this.zoom = zoom;
                }

                /**
                 * DOCUMENT ME!
                 *
                 * @param  x0  DOCUMENT ME!
                 * @param  y0  DOCUMENT ME!
                 * @param  z0  DOCUMENT ME!
                 * @param  x1  DOCUMENT ME!
                 * @param  y1  DOCUMENT ME!
                 * @param  z1  DOCUMENT ME!
                 * @param  t0  DOCUMENT ME!
                 * @param  t1  DOCUMENT ME!
                 */
                public void addROI(Integer x0, Integer y0, Integer z0, Integer x1, Integer y1, Integer z1, String t0,
                                   String t1) {

                    if (rois == null) {
                        rois = new Vector();
                    }

                    rois.add(new ROI(x0, y0, z0, x1, y1, z1, t0, t1));
                }

                /**
                 * DOCUMENT ME!
                 *
                 * @param  bbl  DOCUMENT ME!
                 */
                public void setBlueBlackLevel(Integer bbl) {
                    this.blueBlackLevel = bbl;
                }

                /**
                 * DOCUMENT ME!
                 *
                 * @param  bcn  DOCUMENT ME!
                 */
                public void setBlueChannelNumber(Integer bcn) {
                    this.blueChannelNumber = bcn;
                }

                /**
                 * DOCUMENT ME!
                 *
                 * @param  gamma  DOCUMENT ME!
                 */
                public void setBlueGamma(Float gamma) {
                    this.blueGamma = gamma;
                }

                /**
                 * DOCUMENT ME!
                 *
                 * @param  bwl  DOCUMENT ME!
                 */
                public void setBlueWhiteLevel(Integer bwl) {
                    this.blueWhiteLevel = bwl;
                }

                /**
                 * DOCUMENT ME!
                 *
                 * @param  gbl  DOCUMENT ME!
                 */
                public void setGreenBlackLevel(Integer gbl) {
                    this.greenBlackLevel = gbl;
                }

                /**
                 * DOCUMENT ME!
                 *
                 * @param  gcn  DOCUMENT ME!
                 */
                public void setGreenChannelNumber(Integer gcn) {
                    this.greenChannelNumber = gcn;
                }

                /**
                 * DOCUMENT ME!
                 *
                 * @param  gg  DOCUMENT ME!
                 */
                public void setGreenGamma(Float gg) {
                    this.greenGamma = gg;
                }

                /**
                 * DOCUMENT ME!
                 *
                 * @param  gwl  DOCUMENT ME!
                 */
                public void setGreenWhiteLevel(Integer gwl) {
                    this.greenWhiteLevel = gwl;
                }

                /**
                 * DOCUMENT ME!
                 *
                 * @param  gbl  DOCUMENT ME!
                 */
                public void setGreyBlackLevel(Integer gbl) {
                    this.greyBlackLevel = gbl;
                }

                /**
                 * DOCUMENT ME!
                 *
                 * @param  gcn  DOCUMENT ME!
                 */
                public void setGreyChannelNumber(Integer gcn) {
                    this.greyChannelNumber = gcn;
                }

                /**
                 * DOCUMENT ME!
                 *
                 * @param  gcm  DOCUMENT ME!
                 */
                public void setGreyColorMap(String gcm) {
                    this.greyColorMap = gcm;
                }

                /**
                 * DOCUMENT ME!
                 *
                 * @param  gg  DOCUMENT ME!
                 */
                public void setGreyGamma(Float gg) {
                    this.greyGamma = gg;
                }

                /**
                 * DOCUMENT ME!
                 *
                 * @param  gwl  DOCUMENT ME!
                 */
                public void setGreyWhiteLevel(Integer gwl) {
                    this.greyWhiteLevel = gwl;
                }

                /**
                 * DOCUMENT ME!
                 *
                 * @param  rbl  DOCUMENT ME!
                 */
                public void setRedBlackLevel(Integer rbl) {
                    this.redBlackLevel = rbl;
                }

                /**
                 * DOCUMENT ME!
                 *
                 * @param  rcn  DOCUMENT ME!
                 */
                public void setRedChannelNumber(Integer rcn) {
                    this.redChannelNumber = rcn;
                }

                /**
                 * DOCUMENT ME!
                 *
                 * @param  rg  DOCUMENT ME!
                 */
                public void setRedGamma(Float rg) {
                    this.redGamma = rg;
                }

                /**
                 * DOCUMENT ME!
                 *
                 * @param  rwl  DOCUMENT ME!
                 */
                public void setRedWhiteLevel(Integer rwl) {
                    this.redWhiteLevel = rwl;
                }

                /**
                 * DOCUMENT ME!
                 *
                 * @param  tStart  DOCUMENT ME!
                 */
                public void setTStart(Integer tStart) {
                    this.tStart = tStart;
                }

                /**
                 * DOCUMENT ME!
                 *
                 * @param  tStop  DOCUMENT ME!
                 */
                public void setTStop(Integer tStop) {
                    this.tStop = tStop;
                }

                /**
                 * DOCUMENT ME!
                 *
                 * @param  zStart  DOCUMENT ME!
                 */
                public void setZStart(Integer zStart) {
                    this.zStart = zStart;
                }

                /**
                 * DOCUMENT ME!
                 *
                 * @param  zStop  DOCUMENT ME!
                 */
                public void setZStop(Integer zStop) {
                    this.zStop = zStop;
                }

                /**
                 * DOCUMENT ME!
                 */
                public class ROI {

                    /** DOCUMENT ME! */
                    private String t0;

                    /** DOCUMENT ME! */
                    private String t1;

                    /** DOCUMENT ME! */
                    private Integer x0;

                    /** DOCUMENT ME! */
                    private Integer x1;

                    /** DOCUMENT ME! */
                    private Integer y0;

                    /** DOCUMENT ME! */
                    private Integer y1;

                    /** DOCUMENT ME! */
                    private Integer z0;

                    /** DOCUMENT ME! */
                    private Integer z1;

                    /**
                     * Creates a new ROI object.
                     *
                     * @param  x0  DOCUMENT ME!
                     * @param  y0  DOCUMENT ME!
                     * @param  z0  DOCUMENT ME!
                     * @param  x1  DOCUMENT ME!
                     * @param  y1  DOCUMENT ME!
                     * @param  z1  DOCUMENT ME!
                     * @param  t0  DOCUMENT ME!
                     * @param  t1  DOCUMENT ME!
                     */
                    public ROI(Integer x0, Integer y0, Integer z0, Integer x1, Integer y1, Integer z1, String t0,
                               String t1) {
                        this.x0 = x0;
                        this.y0 = y0;
                        this.z0 = z0;
                        this.x1 = x1;
                        this.y1 = y1;
                        this.z1 = z1;
                        this.t0 = t0;
                        this.t1 = t1;
                    }
                }
            }

            /**
             * DOCUMENT ME!
             */
            public class Feature {

                /** DOCUMENT ME! */
                private String name;

                /** DOCUMENT ME! */
                private String tag;

                /**
                 * Creates a new Feature object.
                 *
                 * @param  tag  DOCUMENT ME!
                 */
                public Feature(String tag) {
                    this.tag = tag;
                }
            }

            /**
             * DOCUMENT ME!
             */
            public class InstrumentRef {

                /** Attributes. */
                private Integer documentRef;

                /** DOCUMENT ME! */
                private Integer instrumentID;

                /** DOCUMENT ME! */
                private Integer objectiveID;

                /**
                 * Creates a new InstrumentRef object.
                 *
                 * @param  docRef  DOCUMENT ME!
                 * @param  instID  DOCUMENT ME!
                 * @param  objID   DOCUMENT ME!
                 */
                public InstrumentRef(Integer docRef, Integer instID, Integer objID) {
                    this.documentRef = docRef;
                    this.instrumentID = instID;
                    this.objectiveID = objID;
                }
            }

            /**
             * DOCUMENT ME!
             */
            public class PlateInfo {

                /** attributes. */
                private Integer plateID;

                /** elements. */
                private String sampleExternRef;

                /** DOCUMENT ME! */
                private Integer sampleNumber;

                /** DOCUMENT ME! */
                private String wellAddress;

                /** DOCUMENT ME! */
                private String wellExternRef;

                /** DOCUMENT ME! */
                private String wellString;

                /**
                 * Creates a new PlateInfo object.
                 *
                 * @param  id  DOCUMENT ME!
                 */
                public PlateInfo(Integer id) {
                    this.plateID = id;
                }

                /**
                 * DOCUMENT ME!
                 *
                 * @param  ref  DOCUMENT ME!
                 */
                public void setSampleExternRef(String ref) {
                    this.sampleExternRef = ref;
                }

                /**
                 * DOCUMENT ME!
                 *
                 * @param  sn  DOCUMENT ME!
                 */
                public void setSampleNumber(Integer sn) {
                    this.sampleNumber = sn;
                }

                /**
                 * DOCUMENT ME!
                 *
                 * @param  add  DOCUMENT ME!
                 */
                public void setWellAddress(String add) {
                    this.wellAddress = add;
                }

                /**
                 * DOCUMENT ME!
                 *
                 * @param  ref  DOCUMENT ME!
                 */
                public void setWellExternRef(String ref) {
                    this.wellExternRef = ref;
                }

                /**
                 * DOCUMENT ME!
                 *
                 * @param  ws  DOCUMENT ME!
                 */
                public void setWellString(String ws) {
                    this.wellString = ws;
                }
            }

            /**
             * DOCUMENT ME!
             */
            public class StageLabel {

                /** DOCUMENT ME! */
                private String name;

                /** DOCUMENT ME! */
                private Float x;

                /** DOCUMENT ME! */
                private Float y;

                /** DOCUMENT ME! */
                private Float z;

                /**
                 * Creates a new StageLabel object.
                 *
                 * @param  name  DOCUMENT ME!
                 * @param  x     DOCUMENT ME!
                 * @param  y     DOCUMENT ME!
                 * @param  z     DOCUMENT ME!
                 */
                public StageLabel(String name, Float x, Float y, Float z) {
                    this.name = name;
                    this.x = x;
                    this.y = y;
                    this.z = z;
                }
            }

        }

        /**
         * DOCUMENT ME!
         */
        public class Include {

            /** attributes. */
            private Integer documentID;

            /** DOCUMENT ME! */
            private URI hRef;

            /** elements. */
            private String includeString;

            /** DOCUMENT ME! */
            private String SHA1; // hex40

            /**
             * Creates a new Include object.
             *
             * @param  docID  DOCUMENT ME!
             * @param  ref    DOCUMENT ME!
             * @param  sha1   DOCUMENT ME!
             */
            public Include(Integer docID, URI ref, String sha1) {
                this.documentID = docID;
                this.hRef = ref;
                this.SHA1 = sha1;
            }

            /**
             * DOCUMENT ME!
             *
             * @param  str  DOCUMENT ME!
             */
            public void setIncludeString(String str) {
                this.includeString = str;
            }
        }

        /**
         * DOCUMENT ME!
         */
        public class Instrument {

            /** DOCUMENT ME! */
            private Vector detectors; // Vector of dectectors

            /** DOCUMENT ME! */
            private Vector filters; // Vector of filters

            /** Attributes. */
            private Integer instrumentID;

            /** DOCUMENT ME! */
            private Vector lightsources; // Vector of lightsources

            /** Elements. */
            private Microscope microscope;

            /** DOCUMENT ME! */
            private Vector objectives; // Vector of objectives

            /** DOCUMENT ME! */
            private Vector otfs; // Vector of OTFs

            /**
             * Creates a new Instrument object.
             *
             * @param  id  DOCUMENT ME!
             */
            public Instrument(Integer id) {
                this.instrumentID = id;
            }

            /**
             * DOCUMENT ME!
             *
             * @param  man      DOCUMENT ME!
             * @param  mod      DOCUMENT ME!
             * @param  sn       DOCUMENT ME!
             * @param  gain     DOCUMENT ME!
             * @param  voltage  DOCUMENT ME!
             * @param  offset   DOCUMENT ME!
             * @param  id       DOCUMENT ME!
             * @param  type     DOCUMENT ME!
             */
            public void addDetector(String man, String mod, String sn, Float gain, Float voltage, Float offset,
                                    Integer id, String type) {

                if (detectors == null) {
                    detectors = new Vector();
                }

                detectors.add(new Detector(man, mod, sn, gain, voltage, offset, id, type));
            }

            /**
             * DOCUMENT ME!
             *
             * @param  id  DOCUMENT ME!
             */
            public void addFilter(Integer id) {

                if (filters == null) {
                    filters = new Vector();
                }

                filters.add(new Filter(id));
            }

            /**
             * DOCUMENT ME!
             *
             * @param  man  DOCUMENT ME!
             * @param  mod  DOCUMENT ME!
             * @param  sn   DOCUMENT ME!
             * @param  id   DOCUMENT ME!
             */
            public void addLightSource(String man, String mod, String sn, Integer id) {

                if (lightsources == null) {
                    lightsources = new Vector();
                }

                lightsources.add(new LightSource(man, mod, sn, id));
            }

            /**
             * DOCUMENT ME!
             *
             * @param  man  DOCUMENT ME!
             * @param  mod  DOCUMENT ME!
             * @param  sn   DOCUMENT ME!
             * @param  id   DOCUMENT ME!
             */
            public void addObjective(String man, String mod, String sn, Integer id) {

                if (objectives == null) {
                    objectives = new Vector();
                }

                objectives.add(new Objective(man, mod, sn, id));
            }

            /**
             * DOCUMENT ME!
             *
             * @param  otfID  DOCUMENT ME!
             * @param  objID  DOCUMENT ME!
             * @param  fID    DOCUMENT ME!
             * @param  pt     DOCUMENT ME!
             * @param  oaa    DOCUMENT ME!
             * @param  sizeX  DOCUMENT ME!
             * @param  sizeY  DOCUMENT ME!
             */
            public void addOTF(Integer otfID, Integer objID, Integer fID, String pt, Boolean oaa, Integer sizeX,
                               Integer sizeY) {

                if (otfs == null) {
                    otfs = new Vector();
                }

                otfs.add(new OTF(otfID, objID, fID, pt, oaa, sizeX, sizeY));
            }

            /**
             * DOCUMENT ME!
             *
             * @return  DOCUMENT ME!
             */
            public Filter getCurrentFilter() {
                return (Filter) filters.lastElement();
            }

            /**
             * DOCUMENT ME!
             *
             * @return  DOCUMENT ME!
             */
            public LightSource getCurrentLightSource() {
                return (LightSource) lightsources.lastElement();
            }

            /**
             * DOCUMENT ME!
             *
             * @return  DOCUMENT ME!
             */
            public Objective getCurrentObjective() {
                return (Objective) objectives.lastElement();
            }

            /**
             * DOCUMENT ME!
             *
             * @return  DOCUMENT ME!
             */
            public OTF getCurrentOTF() {
                return (OTF) otfs.lastElement();
            }

            /**
             * DOCUMENT ME!
             *
             * @param  man   DOCUMENT ME!
             * @param  mod   DOCUMENT ME!
             * @param  sn    DOCUMENT ME!
             * @param  type  DOCUMENT ME!
             */
            public void setMicroscope(String man, String mod, String sn, String type) {
                this.microscope = new Microscope(man, mod, sn, type);
            }

            /**
             * DOCUMENT ME!
             */
            public class Detector {

                /** DOCUMENT ME! */
                private Integer detectorID;

                /** DOCUMENT ME! */
                private Float gain;

                /** attributes. */
                private String manufacturer;

                /** DOCUMENT ME! */
                private String model;

                /** DOCUMENT ME! */
                private Float offset;

                /** DOCUMENT ME! */
                private String serialNumber;

                /** DOCUMENT ME! */
                private String type;

                /** DOCUMENT ME! */
                private Float voltage;

                /**
                 * Creates a new Detector object.
                 *
                 * @param  man      DOCUMENT ME!
                 * @param  mod      DOCUMENT ME!
                 * @param  sn       DOCUMENT ME!
                 * @param  gain     DOCUMENT ME!
                 * @param  voltage  DOCUMENT ME!
                 * @param  offset   DOCUMENT ME!
                 * @param  id       DOCUMENT ME!
                 * @param  type     DOCUMENT ME!
                 */
                public Detector(String man, String mod, String sn, Float gain, Float voltage, Float offset, Integer id,
                                String type) {
                    this.manufacturer = man;
                    this.model = mod;
                    this.serialNumber = sn;
                    this.gain = gain;
                    this.voltage = voltage;
                    this.offset = offset;
                    this.detectorID = id;
                    this.type = type;
                }
            }


            /**
             * DOCUMENT ME!
             */
            public class Filter {

                /** DOCUMENT ME! */
                private FilterType dichroic;

                /** DOCUMENT ME! */
                private FilterType emFilter;

                /** must have these 3:. */
                private FilterType exFilter;

                /** DOCUMENT ME! */
                private Integer filterID;

                /** or this one:. */
                private FilterType filterSet;

                /** DOCUMENT ME! */
                private Boolean isFilterSet; // is this a filterSet?

                /**
                 * constructor.
                 *
                 * @param  id  DOCUMENT ME!
                 */
                public Filter(Integer id) {
                    this.filterID = id;
                }

                /**
                 * DOCUMENT ME!
                 *
                 * @param  man   DOCUMENT ME!
                 * @param  mod   DOCUMENT ME!
                 * @param  ln    DOCUMENT ME!
                 * @param  type  DOCUMENT ME!
                 */
                public void setDichroic(String man, String mod, String ln, String type) {
                    dichroic = new FilterType(man, mod, ln, type);
                }

                /**
                 * DOCUMENT ME!
                 *
                 * @param  man   DOCUMENT ME!
                 * @param  mod   DOCUMENT ME!
                 * @param  ln    DOCUMENT ME!
                 * @param  type  DOCUMENT ME!
                 */
                public void setEmFilter(String man, String mod, String ln, String type) {
                    emFilter = new FilterType(man, mod, ln, type);
                }

                /**
                 * DOCUMENT ME!
                 *
                 * @param  man   DOCUMENT ME!
                 * @param  mod   DOCUMENT ME!
                 * @param  ln    DOCUMENT ME!
                 * @param  type  DOCUMENT ME!
                 */
                public void setExFilter(String man, String mod, String ln, String type) {
                    exFilter = new FilterType(man, mod, ln, type);
                    isFilterSet = Boolean.FALSE;
                }

                /**
                 * DOCUMENT ME!
                 *
                 * @param  man  DOCUMENT ME!
                 * @param  mod  DOCUMENT ME!
                 * @param  ln   DOCUMENT ME!
                 */
                public void setFilterSet(String man, String mod, String ln) {
                    filterSet = new FilterType(man, mod, ln, null);
                    isFilterSet = Boolean.TRUE;
                }

                /**
                 * DOCUMENT ME!
                 */
                public class FilterType {

                    /** DOCUMENT ME! */
                    private String lotNumber;

                    /** DOCUMENT ME! */
                    private String manufacturer;

                    /** DOCUMENT ME! */
                    private String model;

                    /** DOCUMENT ME! */
                    private String type; // optional

                    /**
                     * Creates a new FilterType object.
                     *
                     * @param  man   DOCUMENT ME!
                     * @param  mod   DOCUMENT ME!
                     * @param  ln    DOCUMENT ME!
                     * @param  type  DOCUMENT ME!
                     */
                    public FilterType(String man, String mod, String ln, String type) {
                        this.manufacturer = man;
                        this.model = mod;
                        this.lotNumber = ln;
                        this.type = type;
                    }
                }
            }

            /**
             * DOCUMENT ME!
             */
            public class LightSource {

                /** DOCUMENT ME! */
                Integer lightSourceID;

                /** Attributes. */
                String manufacturer;

                /** DOCUMENT ME! */
                String model;

                /** DOCUMENT ME! */
                String serialNumber;

                /** DOCUMENT ME! */
                private FilamentArc filarc;

                /** Elements has one of the following...laser, filament, or arc. */
                private Laser laser;

                /**
                 * Creates a new LightSource object.
                 *
                 * @param  man  DOCUMENT ME!
                 * @param  mod  DOCUMENT ME!
                 * @param  sn   DOCUMENT ME!
                 * @param  id   DOCUMENT ME!
                 */
                public LightSource(String man, String mod, String sn, Integer id) {
                    this.manufacturer = man;
                    this.model = mod;
                    this.serialNumber = sn;
                    this.lightSourceID = id;
                }

                /**
                 * DOCUMENT ME!
                 *
                 * @return  DOCUMENT ME!
                 */
                public Laser getLaser() {
                    return this.laser;
                }

                /**
                 * DOCUMENT ME!
                 *
                 * @param  type   DOCUMENT ME!
                 * @param  power  DOCUMENT ME!
                 */
                public void setArc(String type, Float power) {
                    this.filarc = new FilamentArc(type, power, Boolean.FALSE);
                }

                /**
                 * DOCUMENT ME!
                 *
                 * @param  type   DOCUMENT ME!
                 * @param  power  DOCUMENT ME!
                 */
                public void setFilament(String type, Float power) {
                    this.filarc = new FilamentArc(type, power, Boolean.TRUE);
                }

                /**
                 * DOCUMENT ME!
                 *
                 * @param  type        DOCUMENT ME!
                 * @param  medium      DOCUMENT ME!
                 * @param  wavelength  DOCUMENT ME!
                 * @param  freqDb      DOCUMENT ME!
                 * @param  tunable     DOCUMENT ME!
                 * @param  pulse       DOCUMENT ME!
                 * @param  power       DOCUMENT ME!
                 */
                public void setLaser(String type, String medium, Integer wavelength, Boolean freqDb, Boolean tunable,
                                     String pulse, Float power) {
                    this.laser = new Laser(type, medium, wavelength, freqDb, tunable, pulse, power);
                }

                /**
                 * DOCUMENT ME!
                 */
                public class FilamentArc {

                    /** DOCUMENT ME! */
                    Boolean isFilament;

                    /** DOCUMENT ME! */
                    Float power;

                    /** Attributes. */
                    String type;

                    /**
                     * Creates a new FilamentArc object.
                     *
                     * @param  type   DOCUMENT ME!
                     * @param  power  DOCUMENT ME!
                     * @param  isFil  DOCUMENT ME!
                     */
                    public FilamentArc(String type, Float power, Boolean isFil) {
                        this.type = type;
                        this.power = power;
                        this.isFilament = isFil;
                    }
                }

                /**
                 * DOCUMENT ME!
                 */
                public class Laser {

                    /** DOCUMENT ME! */
                    private Boolean frequencyDoubled;

                    /** DOCUMENT ME! */
                    private String medium;

                    /** DOCUMENT ME! */
                    private Float power;

                    /** DOCUMENT ME! */
                    private String pulse;

                    /** elements. */
                    private Pump pump;

                    /** DOCUMENT ME! */
                    private Boolean tunable;

                    /** attributes. */
                    private String type;

                    /** DOCUMENT ME! */
                    private Integer wavelength;

                    /**
                     * Creates a new Laser object.
                     *
                     * @param  type    DOCUMENT ME!
                     * @param  medium  DOCUMENT ME!
                     * @param  wl      DOCUMENT ME!
                     * @param  fq      DOCUMENT ME!
                     * @param  tun     DOCUMENT ME!
                     * @param  pulse   DOCUMENT ME!
                     * @param  power   DOCUMENT ME!
                     */
                    public Laser(String type, String medium, Integer wl, Boolean fq, Boolean tun, String pulse,
                                 Float power) {
                        this.type = type;
                        this.medium = medium;
                        this.wavelength = wl;
                        this.frequencyDoubled = fq;
                        this.tunable = tun;
                        this.pulse = pulse;
                        this.power = power;
                    }

                    /**
                     * DOCUMENT ME!
                     *
                     * @param  docRef  DOCUMENT ME!
                     * @param  id      DOCUMENT ME!
                     */
                    public void setPump(Integer docRef, Integer id) {
                        this.pump = new Pump(docRef, id);
                    }

                    /**
                     * DOCUMENT ME!
                     */
                    public class Pump {

                        /** DOCUMENT ME! */
                        private Integer documentRef;

                        /** DOCUMENT ME! */
                        private Integer lightSourceID;

                        /**
                         * Creates a new Pump object.
                         *
                         * @param  docRef  DOCUMENT ME!
                         * @param  id      DOCUMENT ME!
                         */
                        public Pump(Integer docRef, Integer id) {
                            this.documentRef = docRef;
                            this.lightSourceID = id;
                        }
                    }
                }
            }

            /**
             * DOCUMENT ME!
             */
            public class Microscope {

                /** Attributes. */
                private String manufacturer;

                /** DOCUMENT ME! */
                private String model;

                /** DOCUMENT ME! */
                private String serialNumber;

                /** DOCUMENT ME! */
                private String type;

                /**
                 * Creates a new Microscope object.
                 *
                 * @param  manufacturer  DOCUMENT ME!
                 * @param  model         DOCUMENT ME!
                 * @param  serialNumber  DOCUMENT ME!
                 * @param  type          DOCUMENT ME!
                 */
                public Microscope(String manufacturer, String model, String serialNumber, String type) {
                    this.manufacturer = manufacturer;
                    this.model = model;
                    this.serialNumber = serialNumber;
                    this.type = type;
                }
            }

            /**
             * DOCUMENT ME!
             */
            public class Objective {

                /** DOCUMENT ME! */
                private Float lensNA;

                /** DOCUMENT ME! */
                private Float magnification;

                /** DOCUMENT ME! */
                private String manufacturer;

                /** DOCUMENT ME! */
                private String model;

                /** DOCUMENT ME! */
                private Integer objectiveID;

                /** DOCUMENT ME! */
                private String serialNumber;

                /**
                 * Creates a new Objective object.
                 *
                 * @param  man  DOCUMENT ME!
                 * @param  mod  DOCUMENT ME!
                 * @param  sn   DOCUMENT ME!
                 * @param  id   DOCUMENT ME!
                 */
                public Objective(String man, String mod, String sn, Integer id) {
                    this.manufacturer = man;
                    this.model = mod;
                    this.serialNumber = sn;
                    this.objectiveID = id;
                }

                /**
                 * DOCUMENT ME!
                 *
                 * @param  lna  DOCUMENT ME!
                 */
                public void setLensNA(Float lna) {
                    this.lensNA = lna;
                }

                /**
                 * DOCUMENT ME!
                 *
                 * @param  mag  DOCUMENT ME!
                 */
                public void setMagnification(Float mag) {
                    this.magnification = mag;
                }

            }

            /**
             * DOCUMENT ME!
             */
            public class OTF {

                /** must have either: Bin:External or Bin:BinData. */
                private BinExternal binExternal;

                /** DOCUMENT ME! */
                private Integer filterID;

                /** DOCUMENT ME! */
                private Integer objectiveID;

                /** DOCUMENT ME! */
                private Boolean opticalAxisAvrg;

                /** DOCUMENT ME! */
                private Integer otfID;

                /** DOCUMENT ME! */
                private String pixelType;

                /** DOCUMENT ME! */
                private Integer sizeX;

                /** DOCUMENT ME! */
                private Integer sizeY;

                /**
                 * Bin:BinData base64binary??
                 *
                 * @param  otfID  DOCUMENT ME!
                 * @param  objID  DOCUMENT ME!
                 * @param  fID    DOCUMENT ME!
                 * @param  pt     DOCUMENT ME!
                 * @param  oaa    DOCUMENT ME!
                 * @param  sizeX  DOCUMENT ME!
                 * @param  sizeY  DOCUMENT ME!
                 */
                public OTF(Integer otfID, Integer objID, Integer fID, String pt, Boolean oaa, Integer sizeX,
                           Integer sizeY) {
                    this.otfID = otfID;
                    this.objectiveID = objID;
                    this.filterID = fID;
                    this.pixelType = pt;
                    this.opticalAxisAvrg = oaa;
                    this.sizeX = sizeX;
                    this.sizeY = sizeY;
                }

                /**
                 * DOCUMENT ME!
                 *
                 * @param  comp    DOCUMENT ME!
                 * @param  sha1    DOCUMENT ME!
                 * @param  offset  DOCUMENT ME!
                 * @param  href    DOCUMENT ME!
                 */
                public void setBinExternal(String comp, String sha1, Integer offset, URI href) {
                    binExternal = new BinExternal(comp, sha1, offset, href);
                }

                /**
                 * DOCUMENT ME!
                 */
                public class BinExternal {

                    /** DOCUMENT ME! */
                    private String compression;

                    /** DOCUMENT ME! */
                    private URI hRef;

                    /** DOCUMENT ME! */
                    private Integer offset;

                    /** DOCUMENT ME! */
                    private String SHA1;

                    /**
                     * Creates a new BinExternal object.
                     *
                     * @param  comp    DOCUMENT ME!
                     * @param  sha1    DOCUMENT ME!
                     * @param  offset  DOCUMENT ME!
                     * @param  href    DOCUMENT ME!
                     */
                    public BinExternal(String comp, String sha1, Integer offset, URI href) {
                        this.compression = comp;
                        this.SHA1 = sha1;
                        this.offset = offset;
                        this.hRef = href;
                    }
                }

            }
        }

        /**
         * DOCUMENT ME!
         */
        public class Plate {

            /** DOCUMENT ME! */
            private String externRef;

            /** DOCUMENT ME! */
            private String name;

            /** Attributes. */
            private Integer plateID;

            /** Elements. */
            private Vector screenRefs; // Vector of Screen Refs

            /**
             * Creates a new Plate object.
             *
             * @param  id    DOCUMENT ME!
             * @param  name  DOCUMENT ME!
             * @param  ref   DOCUMENT ME!
             */
            public Plate(Integer id, String name, String ref) {
                this.name = name;
                this.plateID = id;
                this.externRef = ref;
            }

            /**
             * DOCUMENT ME!
             *
             * @param  docRef  DOCUMENT ME!
             * @param  id      DOCUMENT ME!
             */
            public void addScreenRef(Integer docRef, Integer id) {

                if (screenRefs == null) {
                    screenRefs = new Vector();
                }

                screenRefs.add(new ScreenRef(docRef, id));
            }

            /**
             * DOCUMENT ME!
             */
            public class ScreenRef {

                /** DOCUMENT ME! */
                private Integer documentRef;

                /** DOCUMENT ME! */
                private Integer screenID;

                /**
                 * Creates a new ScreenRef object.
                 *
                 * @param  docRef  DOCUMENT ME!
                 * @param  id      DOCUMENT ME!
                 */
                public ScreenRef(Integer docRef, Integer id) {
                    this.documentRef = docRef;
                    this.screenID = id;
                }
            }
        }

        /**
         * DOCUMENT ME!
         */
        public class Project {

            /** Elements. */
            private String description;

            /** DOCUMENT ME! */
            private ExperimenterRef experimenterRef;

            /** DOCUMENT ME! */
            private GroupRef groupRef;

            /** Attributes. */
            private String name;

            /** DOCUMENT ME! */
            private Integer projectID;

            /**
             * Creates a new Project object.
             *
             * @param  name  DOCUMENT ME!
             * @param  id    DOCUMENT ME!
             */
            public Project(String name, Integer id) {
                this.name = name;
                this.projectID = id;
            }

            /**
             * DOCUMENT ME!
             *
             * @param  desc  DOCUMENT ME!
             */
            public void setDescription(String desc) {
                this.description = desc;
            }

            /**
             * DOCUMENT ME!
             *
             * @param  docRef  DOCUMENT ME!
             * @param  expID   DOCUMENT ME!
             */
            public void setExperimenterRef(Integer docRef, Integer expID) {
                this.experimenterRef = new ExperimenterRef(docRef, expID);
            }

            /**
             * DOCUMENT ME!
             *
             * @param  id  DOCUMENT ME!
             */
            public void setGroupRef(Integer id) {
                this.groupRef = new GroupRef(id);
            }

        }

        /**
         * DOCUMENT ME!
         */
        public class Screen {

            /** Elements. */
            private String description;

            /** DOCUMENT ME! */
            private String externRef;

            /** DOCUMENT ME! */
            private String name;

            /** Attributes. */
            private Integer screenID;

            /**
             * Creates a new Screen object.
             *
             * @param  screenID  DOCUMENT ME!
             * @param  name      DOCUMENT ME!
             * @param  ref       DOCUMENT ME!
             */
            public Screen(Integer screenID, String name, String ref) {
                this.screenID = screenID;
                this.name = name;
                this.externRef = ref;
            }

            /**
             * DOCUMENT ME!
             *
             * @param  desc  DOCUMENT ME!
             */
            public void setDescription(String desc) {
                this.description = desc;
            }
        }

    }


    /**
     * DOCUMENT ME!
     */
    public class Person {

        /** DOCUMENT ME! */
        private String email;

        /** Attributes. */
        private String firstName;

        /** DOCUMENT ME! */
        private String institution;

        /** DOCUMENT ME! */
        private String lastName;

        /**
         * Creates a new Person object.
         *
         * @param  f  DOCUMENT ME!
         * @param  l  DOCUMENT ME!
         * @param  e  DOCUMENT ME!
         * @param  i  DOCUMENT ME!
         */
        public Person(String f, String l, String e, String i) {
            this.firstName = f;
            this.lastName = l;
            this.email = e;
            this.institution = i;
        }
    }

    /**
     * DOCUMENT ME!
     */
    public class ProjectRef {

        /** DOCUMENT ME! */
        private Integer documentRef;

        /** DOCUMENT ME! */
        private Integer projectID;

        /**
         * Creates a new ProjectRef object.
         *
         * @param  docRef  DOCUMENT ME!
         * @param  id      DOCUMENT ME!
         */
        public ProjectRef(Integer docRef, Integer id) {
            this.documentRef = docRef;
            this.projectID = id;
        }
    }

}
