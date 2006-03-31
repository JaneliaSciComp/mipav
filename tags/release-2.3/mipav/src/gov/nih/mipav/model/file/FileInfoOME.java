package gov.nih.mipav.model.file;

import java.util.Vector;
import java.net.URI;

import gov.nih.mipav.model.file.*;
import gov.nih.mipav.view.dialogs.*;
import gov.nih.mipav.model.structures.*;

/**
 * <p>Title: </p>
 * <p>Description: </p>
 * <p>Copyright: Copyright (c) 2003</p>
 * <p>Company: </p>
 * @author not attributable
 * @version 1.0
 */

public class FileInfoOME
    extends FileInfoBase {

  private String headerFileName;

  private TransMatrix matrix;

  private OME ome;


  public OME createOME() {
    return new OME();
  }
  public void setOME(OME ome) {
    this.ome = ome;
  }
  public OME getOME() {
    return ome;
  }

  /**
   * Sets the name of the header file
   * @param headerFileName       header file name (not path(
   */
  public void setHeaderFileName(String headerFileName) {
    this.headerFileName = headerFileName;
  }

  /**
   * Gets the transition matrix
   * @return matrix
   */
  public TransMatrix getMatrix() {
    return this.matrix;
  }



  public class Person {
    // Attributes
    private String firstName;
    private String lastName;
    private String email;
    private String institution;

    public Person(String f, String l, String e, String i) {
      this.firstName = f;
      this.lastName = l;
      this.email = e;
      this.institution = i;
    }
  }

  public class OME {
    private Vector documentGroup; //Vector of Includes
    private Vector projects; //Vector of Projects
    private Vector datasets; //Vector of Datasets
    private Vector experiments; //Vector of Experiments
    private Vector plates; //Vector of Plates
    private Vector screens; //Vector of Screens
    private Vector experimenters; //Vector of Experimenters
    private Vector groups; //Vector of Groups
    private Vector instruments; //Vector of instruments
    private Vector images; //Vector of images

    //need these functions to create the classes within..

    public void addInclude(Integer id, URI href, String sha1) {
      if (documentGroup == null) {
        documentGroup = new Vector();
      }
      documentGroup.add(new Include(id, href, sha1));
    }
    public Include getCurrentInclude() {
      return (Include) documentGroup.lastElement();
    }
    public void addProject(String name, Integer id) {
      if (projects == null) {
        projects = new Vector();
      }
      projects.add(new Project(name, id));
    }

    public Project getCurrentProject() {
      return (Project) projects.lastElement();
    }

    public void addDataset(String name, Integer id, Boolean locked) {
      if (datasets == null) {
        datasets = new Vector();
      }
      datasets.add(new Dataset(name, id, locked));
    }

    public Dataset getCurrentDataset() {
      return (Dataset) datasets.lastElement();
    }

    public void addExperiment(Integer id) {
      if (experiments == null) {
        experiments = new Vector();
      }
      experiments.add(new Experiment(id));
    }

    public Experiment getCurrentExperiment() {
      return (Experiment) experiments.lastElement();
    }

    public void addPlate(Integer id, String name, String ref) {
      if (plates == null) {
        plates = new Vector();
      }
      plates.add(new Plate(id, name, ref));
    }

    public Plate getCurrentPlate() {
      return (Plate) plates.lastElement();
    }

    public void addScreen(Integer id, String name, String ref) {
      if (screens == null) {
        screens = new Vector();
      }
      screens.add(new Screen(id, name, ref));
    }

    public Screen getCurrentScreen() {
      return (Screen) screens.lastElement();
    }

    public void addExperimenter(Integer id) {
      if (experimenters == null) {
        experimenters = new Vector();
      }
      experimenters.add(new Experimenter(id));
    }

    public Experimenter getCurrentExperimenter() {
      return (Experimenter) experimenters.lastElement();
    }

    public void addGroup(String name, Integer id) {
      if (groups == null) {
        groups = new Vector();
      }
      groups.add(new Group(name, id));
    }

    public Group getCurrentGroup() {
      return (Group) groups.lastElement();
    }

    public void addInstrument(Integer id) {
      if (instruments == null) {
        instruments = new Vector();
      }
      instruments.add(new Instrument(id));
    }

    public Instrument getCurrentInstrument() {
      return (Instrument) instruments.lastElement();
    }

    public void addImage(String guid, String type, String name, Integer sizeX, Integer sizeY, Integer sizeZ,
                         Integer numChannels, Integer numTimes, Float pSizeX, Float pSizeY, Float pSizeZ,
                         Float timeInc, Integer waveS, Integer waveInc) {
      if (images == null) {
        images = new Vector();
      }
      images.add(new Image(guid, type, name, sizeX, sizeY, sizeZ, numChannels,
                           numTimes, pSizeX, pSizeY, pSizeZ, timeInc, waveS, waveInc));
    }

    public Image getCurrentImage() {
      return (Image) images.lastElement();
    }

    public class Include {
      //attributes
      private Integer documentID;
      private URI hRef;
      private String SHA1; //hex40

      //elements
      private String includeString;

      public Include(Integer docID, URI ref, String sha1) {
        this.documentID = docID;
        this.hRef = ref;
        this.SHA1 = sha1;
      }

      public void setIncludeString(String str) {
        this.includeString = str;
      }
    }

    public class Project {
      //Attributes
      private String name;
      private Integer projectID;

      //Elements
      private String description;
      private ExperimenterRef experimenterRef;

      private GroupRef groupRef;

      public Project(String name, Integer id) {
        this.name = name;
        this.projectID = id;
      }

      public void setDescription(String desc) {
        this.description = desc;
      }
      public void setExperimenterRef(Integer docRef, Integer expID) {
        this.experimenterRef = new ExperimenterRef(docRef, expID);
      }
      public void setGroupRef(Integer id) {
        this.groupRef = new GroupRef(id);
      }

    }

    public class Dataset {
      //Attributes
      private String name;
      private Integer datasetID;
      private Boolean locked;

      //Elements
      private String description;
      private ExperimenterRef experimenterRef;
      private GroupRef groupRef;
      private Vector projectRefs; // Vector of ProjectsRefs
      private String customAttributes;

      public Dataset(String name, Integer id, Boolean locked) {
        this.name = name;
        this.datasetID = id;
        this.locked = locked;
      }

      public void setDescription(String desc) {
        this.description = desc;
      }
      public void setExperimenterRef(Integer docRef, Integer expID) {
        this.experimenterRef = new ExperimenterRef(docRef, expID);
      }
      public void setGroupRef(Integer id) {
        this.groupRef = new GroupRef(id);
      }
      public void addProjectRef(Integer docRef, Integer id) {
        if (projectRefs == null) {
          projectRefs = new Vector();
        }
        projectRefs.add(new ProjectRef(docRef, id));
      }
    }

    public class Experiment {
      //Attributes
      private Integer experimentID;

      //Elements
      private String description;
      private ExperimenterRef experimenterRef;

      public Experiment(Integer id) {
        this.experimentID = id;
      }

      public void setDescription(String desc) {
        this.description = desc;
      }
      public void setExperimenterRef(Integer docRef, Integer expID) {
        this.experimenterRef = new ExperimenterRef(docRef, expID);
      }
    }

    public class Plate {
      //Attributes
      private Integer plateID;
      private String name;
      private String externRef;

      //Elements
      private Vector screenRefs; //Vector of Screen Refs

      public Plate(Integer id, String name, String ref) {
        this.name = name;
        this.plateID = id;
        this.externRef = ref;
      }

      public void addScreenRef(Integer docRef, Integer id) {
        if (screenRefs == null) {
          screenRefs = new Vector();
        }
        screenRefs.add(new ScreenRef(docRef, id));
      }

      public class ScreenRef {
        private Integer documentRef;
        private Integer screenID;

        public ScreenRef(Integer docRef, Integer id) {
          this.documentRef = docRef;
          this.screenID = id;
        }
      }
    }

    public class Screen {
      //Attributes
      private Integer screenID;
      private String name;
      private String externRef;

      //Elements
      private String description;

      public Screen(Integer screenID, String name, String ref) {
        this.screenID = screenID;
        this.name = name;
        this.externRef = ref;
      }

      public void setDescription(String desc) {
        this.description = desc;
      }
    }

    public class Experimenter {
      //Attributes
      private Integer experimenterID;

      //Elements
      private String firstName;
      private String lastName;
      private String email;
      private String institution;

      private String omeName;
      private Vector groupRefs; //Vector of GroupRefs

      public Experimenter(Integer id) {
        this.experimenterID = id;
      }

      public void setFirstName(String first) {
        this.firstName = first;
      }
      public void setLastName(String last) {
        this.lastName = last;
      }
      public void setEmail(String email) {
        this.email = email;
      }
      public void setInstitution(String inst) {
        this.institution = inst;
      }
      public void setOMEName(String oName) {
        this.omeName = oName;
      }
      public void addGroupRef(Integer id) {
        if (groupRefs == null) {
          groupRefs = new Vector();
        }
        groupRefs.add(new GroupRef(id));
      }
    }

    public class Group {
      //Attributes
      private String name;
      private Integer groupID;

      private String leaderFirstName;
      private String leaderLastName;
      private String leaderEmail;
      private String leaderInstitution;
      private ExperimenterRef leaderExperimenterRef;
      private Boolean leaderIsPerson; // is the leader data a person or a ref

      private String contactFirstName;
      private String contactLastName;
      private String contactEmail;
      private String contactInstitution;
      private ExperimenterRef contactExperimenterRef;
      private Boolean contactIsPerson; // is the contact data a person or a ref

      public Group(String name, Integer id) {
        this.name = name;
        this.groupID = id;
      }

      public void setLeaderFirstName(String first) {
        leaderIsPerson = new Boolean("true");
        this.leaderFirstName = first;
      }
      public void setLeaderLastName(String last) {
        this.leaderLastName = last;
      }
      public void setLeaderEmail(String email) {
        this.leaderEmail = email;
      }
      public void setLeaderInstitution(String inst) {
        this.leaderInstitution = inst;
      }
      public void setLeaderExperimenterRef(Integer docRef, Integer expID) {
        this.leaderExperimenterRef = new ExperimenterRef(docRef, expID);
      }
      public void setContactFirstName(String first) {
        this.contactIsPerson = new Boolean("true");
        this.contactFirstName = first;
      }
      public void setContactLastName(String last) {
        this.contactLastName = last;
      }
      public void setContactEmail(String email) {
        this.contactEmail = email;
      }
      public void setContactInstitution(String inst) {
        this.contactInstitution = inst;
      }
      public void setContactExperimenterRef(Integer docRef, Integer expID) {
        this.contactExperimenterRef = new ExperimenterRef(docRef, expID);
      }
    }

    public class GroupRef {
      //Attributes
      private Integer groupID;

      public GroupRef(Integer id) {
        this.groupID = id;
      }
    }

    public class Instrument {
      //Attributes
      private Integer instrumentID;

      //Elements
      private Microscope microscope;
      private Vector lightsources; //Vector of lightsources
      private Vector detectors; //Vector of dectectors
      private Vector objectives; //Vector of objectives
      private Vector filters; //Vector of filters
      private Vector otfs; //Vector of OTFs

      public Instrument(Integer id) {
        this.instrumentID = id;
      }

      public void setMicroscope(String man, String mod, String sn, String type) {
        this.microscope = new Microscope(man, mod, sn, type);
      }

      public void addLightSource(String man, String mod, String sn, Integer id) {
        if (lightsources == null) {
          lightsources = new Vector();
        }
        lightsources.add(new LightSource(man, mod, sn, id));
      }
      public LightSource getCurrentLightSource() {
        return (LightSource) lightsources.lastElement();
      }

      public void addDetector(String man, String mod, String sn, Float gain,
                              Float voltage, Float offset, Integer id, String type) {
        if (detectors == null) {
          detectors = new Vector();
        }
        detectors.add(new Detector(man, mod, sn, gain, voltage, offset, id, type));
      }

      public void addObjective(String man, String mod, String sn, Integer id) {
        if (objectives == null) {
          objectives = new Vector();
        }
        objectives.add(new Objective(man, mod, sn, id));
      }

      public Objective getCurrentObjective() {
        return (Objective) objectives.lastElement();
      }

      public void addFilter(Integer id) {
        if (filters == null) {
          filters = new Vector();
        }
        filters.add(new Filter(id));
      }

      public Filter getCurrentFilter() {
        return (Filter) filters.lastElement();
      }

      public void addOTF(Integer otfID, Integer objID, Integer fID, String pt, Boolean oaa, Integer sizeX, Integer sizeY) {
        if (otfs == null) {
          otfs = new Vector();
        }
        otfs.add(new OTF(otfID, objID, fID, pt, oaa, sizeX, sizeY));
      }

      public OTF getCurrentOTF() {
        return (OTF) otfs.lastElement();
      }

      public class Microscope {
        //Attributes
        private String manufacturer;
        private String model;
        private String serialNumber;
        private String type;

        public Microscope(String manufacturer, String model,
                          String serialNumber,
                          String type) {
          this.manufacturer = manufacturer;
          this.model = model;
          this.serialNumber = serialNumber;
          this.type = type;
        }
      }

      public class LightSource {
        //Attributes
        String manufacturer;
        String model;
        String serialNumber;
        Integer lightSourceID;

        //Elements
        // has one of the following...laser, filament, or arc
        private Laser laser;
        private FilamentArc filarc;

        public LightSource(String man, String mod, String sn, Integer id) {
          this.manufacturer = man;
          this.model = mod;
          this.serialNumber = sn;
          this.lightSourceID = id;
        }

        public void setLaser(String type, String medium, Integer wavelength, Boolean freqDb,
                             Boolean tunable, String pulse, Float power) {
          this.laser = new Laser(type, medium, wavelength, freqDb, tunable, pulse, power);
        }
        public Laser getLaser() {
          return this.laser;
        }

        public void setFilament(String type, Float power) {
          this.filarc = new FilamentArc(type, power, Boolean.TRUE);
        }

        public void setArc(String type, Float power) {
          this.filarc = new FilamentArc(type, power, Boolean.FALSE);
        }

        public class Laser {
          //attributes
          private String type;
          private String medium;
          private Integer wavelength;
          private Boolean frequencyDoubled;
          private Boolean tunable;
          private String pulse;
          private Float power;

          //elements
          private Pump pump;

          public Laser(String type, String medium, Integer wl, Boolean fq, Boolean tun, String pulse, Float power) {
            this.type = type;
            this.medium = medium;
            this.wavelength = wl;
            this.frequencyDoubled = fq;
            this.tunable = tun;
            this.pulse = pulse;
            this.power = power;
          }

          public void setPump(Integer docRef, Integer id) {
            this.pump = new Pump(docRef, id);
          }

          public class Pump {
            private Integer documentRef;
            private Integer lightSourceID;

            public Pump(Integer docRef, Integer id) {
              this.documentRef = docRef;
              this.lightSourceID = id;
            }
          }
        }

        public class FilamentArc {
          //Attributes
          String type;
          Float power;
          Boolean isFilament;

          public FilamentArc(String type, Float power, Boolean isFil) {
            this.type = type;
            this.power = power;
            this.isFilament = isFil;
          }
        }
      }

      public class Detector {
        //attributes
        private String manufacturer;
        private String model;
        private String serialNumber;
        private Float gain;
        private Float voltage;
        private Float offset;
        private Integer detectorID;
        private String type;

        public Detector(String man, String mod, String sn, Float gain,
                        Float voltage, Float offset, Integer id, String type) {
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

      public class Objective {
        private String manufacturer;
        private String model;
        private String serialNumber;
        private Integer objectiveID;

        private Float lensNA;
        private Float magnification;

        public Objective(String man, String mod, String sn, Integer id) {
          this.manufacturer = man;
          this.model = mod;
          this.serialNumber = sn;
          this.objectiveID = id;
        }

        public void setLensNA(Float lna) {
          this.lensNA = lna;
        }

        public void setMagnification(Float mag) {
          this.magnification = mag;
        }

      }



      public class Filter {
        private Integer filterID;

        //must have these 3:
        private FilterType exFilter;
        private FilterType dichroic;
        private FilterType emFilter;

        // or this one:
        private FilterType filterSet;

        private Boolean isFilterSet;  //is this a filterSet?

        //constructor
        public Filter(Integer id) {
          this.filterID = id;
        }

        public void setExFilter(String man, String mod, String ln, String type) {
          exFilter = new FilterType(man, mod, ln, type);
          isFilterSet = Boolean.FALSE;
        }

        public void setDichroic(String man, String mod, String ln, String type) {
          dichroic = new FilterType(man, mod, ln, type);
        }

        public void setEmFilter(String man, String mod, String ln , String type) {
          emFilter = new FilterType(man, mod, ln, type);
        }

        public void setFilterSet(String man, String mod, String ln) {
          filterSet = new FilterType(man, mod, ln, null);
          isFilterSet = Boolean.TRUE;
        }

        public class FilterType {
          private String manufacturer;
          private String model;
          private String lotNumber;
          private String type; //optional

          public FilterType(String man, String mod, String ln, String type) {
            this.manufacturer = man;
            this.model = mod;
            this.lotNumber = ln;
            this.type = type;
          }
        }
      }

      public class OTF {
        private Integer otfID;
        private Integer objectiveID;
        private Integer filterID;
        private String pixelType;
        private Boolean opticalAxisAvrg;
        private Integer sizeX;
        private Integer sizeY;

        //must have either: Bin:External or Bin:BinData

        private BinExternal binExternal;

        //Bin:BinData
        //base64binary??

        public OTF(Integer otfID, Integer objID, Integer fID, String pt, Boolean oaa, Integer sizeX, Integer sizeY) {
          this.otfID = otfID;
          this.objectiveID = objID;
          this.filterID = fID;
          this.pixelType = pt;
          this.opticalAxisAvrg = oaa;
          this.sizeX = sizeX;
          this.sizeY = sizeY;
        }

        public void setBinExternal(String comp, String sha1, Integer offset, URI href) {
          binExternal = new BinExternal(comp, sha1, offset, href);
        }

        public class BinExternal {
          private String compression;
          private String SHA1;
          private Integer offset;
          private URI hRef;

          public BinExternal(String comp, String sha1, Integer offset, URI href) {
            this.compression = comp;
            this.SHA1 = sha1;
            this.offset = offset;
            this.hRef = href;
          }
        }

      }
    }

    public class Image {
      //Attributes
      private String guid; //?
      private String imageType;
      private String name;
      private Integer sizeX;
      private Integer sizeY;
      private Integer sizeZ;
      private Integer numChannels;
      private Integer numTimes;
      private Float pixelSizeX;
      private Float pixelSizeY;
      private Float pixelSizeZ;
      private Float timeIncrement;
      private Integer waveStart;
      private Integer waveIncrement;

      //Elements
      private String creationDate;
      private String description;
      private ExperimentRef experimentRef;
      private String derivedImage;

      private GroupRef groupRef;
      private Vector datasetRefs; // Vector for DatasetRefs (Integers)
      private InstrumentRef instrumentRef;

      //For Imaging Environment
      private Float temperature;
      private Float airPressure;
      private Float humidity; // percentage
      private Float cO2Percent; // percentage

      private URI thumbnail; // web location of thumbnail pic
      private String mimeType; // for thumbnail
      private Vector channelInfos; //Vector of channel infos

      private DisplayOptions displayOptions;
      private StageLabel stageLabel;
      private PlateInfo plateInfo;
      private Data data;

      public Image(String guid, String type, String name, Integer sizeX, Integer sizeY,
                   Integer sizeZ, Integer numChannels, Integer numTimes,
                   Float pSizeX, Float pSizeY, Float pSizeZ, Float ti, Integer ws, Integer wi) {
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

        //Set the extents based on sizeX, sizeY, and sizeZ
        int [] extents = null;
        if (sizeZ.intValue() > 1 ) {
          if (numTimes.intValue() > 1) {
            extents = new int[4];
            extents[3] = numTimes.intValue();
          }
          else {
            extents = new int[3];
          }
          extents[2] = sizeZ.intValue();
        }
        else {
          if (numTimes.intValue() > 1) {
            extents = new int[3];
            extents[2] = numTimes.intValue();
          }
          else {
            extents = new int[2];
          }
        }
        extents[0] = sizeX.intValue();
        extents[1] = sizeY.intValue();
        setExtents(extents);

        //Set the resolutions based on pixelSizeX,Y,Z and timeIncrement
        float [] res = new float[extents.length];

        if (res.length == 4) {
          res[2] = pixelSizeZ.floatValue();
          if (timeIncrement != null) {
            res[3] = timeIncrement.floatValue();
          }
          else {
            res[3] = 1.0f;
          }
        }
        else if (res.length == 3) {
          if (!(numTimes.intValue() > 1 )) {
              res[2] = pixelSizeZ.floatValue();
          }
          else if (timeIncrement != null){
            res[2] = timeIncrement.floatValue();
          }
          else {
            res[2] = 1.0f;
          }
        }
        res[0] = pixelSizeX.floatValue();
        res[1] = pixelSizeY.floatValue();
      }

      public void setCreationDate(String date) {
        this.creationDate = date;
      }
      public void setDescription(String desc) {
        this.description = desc;
      }
      public void setExperimentRef(Integer docRef, String experimentID) {
        this.experimentRef = new ExperimentRef(docRef, experimentID);
      }
      public void setDerivedImage(String derImage) {
        this.derivedImage = derImage;
      }
      public void setGroupRef(Integer id) {
        this.groupRef = new GroupRef(id);
      }
      public void addDatasetRef(Integer datasetID) {
        if (datasetRefs == null) {
          datasetRefs = new Vector();
        }
        datasetRefs.add(datasetID);
      }
      public void setInstrumentRef(Integer docRef, Integer instID, Integer objID) {
        this.instrumentRef = new InstrumentRef(docRef, instID, objID);
      }
      public void setTemperature(Float temp) {
        this.temperature = temp;
      }
      public void setAirPressure(Float ap) {
        this.airPressure = ap;
      }
      public void setHumidity(Float hum) {
        this.humidity = hum;
      }
      public void setCO2Percent(Float cO2) {
        this.cO2Percent = cO2;
      }
      public void setThumbnail(URI tn) {
        this.thumbnail = tn;
      }
      public void setMIMEType(String mime) {
        this.mimeType = mime;
      }
      public void addChannelInfo(String name, Integer spp, String illType, Integer phs, String photoInt,
                                 String mode, String contrastMethod, Integer exWave, Integer emWave,
                                 String fluor, Float ndFilter) {
        if (channelInfos == null) {
          channelInfos = new Vector();
        }
        channelInfos.add(new ChannelInfo(name, spp, illType, phs, photoInt,
                                         mode, contrastMethod, exWave, emWave,
                                         fluor, ndFilter));
      }
      public ChannelInfo getCurrentChannelInfo() {
        return (ChannelInfo) channelInfos.lastElement();
      }
      public void setDisplayOptions(Float zoom) {
        displayOptions = new DisplayOptions(zoom);
      }
      public DisplayOptions getDisplayOptions() {
        return displayOptions;
      }
      public void setStageLabel(String name, Float x, Float y, Float z) {
        stageLabel = new StageLabel(name, x, y, z);
      }
      public void setPlateInfo(Integer id) {
        plateInfo = new PlateInfo(id);
      }
      public PlateInfo getPlateInfo() {
        return plateInfo;
      }
      public void setData(String dimOrd, String pt, Boolean bigEnd) {
        data = new Data(dimOrd, pt, bigEnd);
      }
      public Data getData() {
        return this.data;
      }

      public class InstrumentRef {
        //Attributes
        private Integer documentRef;
        private Integer instrumentID;
        private Integer objectiveID;

        public InstrumentRef(Integer docRef, Integer instID, Integer objID) {
          this.documentRef = docRef;
          this.instrumentID = instID;
          this.objectiveID = objID;
        }
      }

      public class ChannelInfo {
        //attributes
        private String name;
        private Integer samplesPerPixel;
        private String illuminationType;
        private Integer pinholeSize;
        private String photometricInterpretation;
        private String mode;
        private String contrastMethod;
        private Integer exWave;
        private Integer emWave;
        private String fluor;
        private Float ndFilter;

        //Elements
        private Vector lightSourceRefs; //Vector of light source refs

        //OTF Ref
        private Integer otfRefDocumentRef;
        private Integer otfRefOTFID;

        // DetectorRef
        private Integer detectorRefDocumentRef;
        private Integer detectorRefDetectorID;
        private Float detectorRefOffset;
        private Float detectorRefGain;

        // FilterRef
        private Integer filterRefDocumentRef;
        private Integer filterRefFilterID;

        private Vector channelComponents; //Vector of channel components

        public ChannelInfo(String name, Integer spp, String illType, Integer phs, String photoInt, String mode,
                           String contrastMethod, Integer exWave, Integer emWave, String fluor, Float ndFilter) {
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

        public void addLightSourceRef(Integer docRef, Integer id, String auxTech,
                                      Float atten, Integer wl) {
          if (lightSourceRefs == null) {
            lightSourceRefs = new Vector();
          }
          lightSourceRefs.add(new LightSourceRef(docRef, id, auxTech, atten, wl));
        }
        public void setOTFRefDocumentRef(Integer docRef) {
          this.otfRefDocumentRef = docRef;
        }
        public void setOTFRefOTFID(Integer id) {
          this.otfRefOTFID = id;
        }
        public void setDetectorRefDocumentRef(Integer docRef) {
          this.detectorRefDocumentRef = docRef;
        }
        public void setDetectorRefDetectorID(Integer id) {
          this.detectorRefDetectorID = id;
        }
        public void setDetectorRefOffset(Float offset) {
          this.detectorRefOffset = offset;
        }
        public void setDetectorRefGain(Float gain) {
          this.detectorRefGain = gain;
        }
        public void setFilterRefDocumentRef(Integer docRef) {
          this.filterRefDocumentRef = docRef;
        }
        public void setFilterRefFilterID(Integer id) {
          this.filterRefFilterID = id;
        }
        public void addChannelComponent(String cd, Integer ccn) {
          if (channelComponents == null) {
            channelComponents = new Vector();
          }
          channelComponents.add(new ChannelComponent(cd, ccn));
        }

        public class LightSourceRef {
          //attributes
          private Integer documentRef;
          private Integer lightSourceID;
          private String auxTechnique;
          private Float attenuation; //percent fraction
          private Integer wavelength;

          public LightSourceRef(Integer docRef, Integer id, String auxTech,
                                Float atten, Integer wl) {
            this.documentRef = docRef;
            this.lightSourceID = id;
            this.auxTechnique = auxTech;
            this.attenuation = atten;
            this.wavelength = wl;
          }
        }

        public class ChannelComponent {
          private String colorDomain;
          private Integer channelComponentNumber;

          public ChannelComponent(String cd, Integer ccn) {
            this.colorDomain = cd;
            this.channelComponentNumber = ccn;
          }
        }
      }

      public class DisplayOptions {
        private Float zoom;

        //either RGB channels or Grey Channel...
        //for RGB:
        private Integer redChannelNumber;
        private Integer redBlackLevel;
        private Integer redWhiteLevel;
        private Float redGamma;
        private Integer greenChannelNumber;
        private Integer greenBlackLevel;
        private Integer greenWhiteLevel;
        private Float greenGamma;
        private Integer blueChannelNumber;
        private Integer blueBlackLevel;
        private Integer blueWhiteLevel;
        private Float blueGamma;

        //for Grey:
        private Integer greyChannelNumber;
        private Integer greyBlackLevel;
        private Integer greyWhiteLevel;
        private Float greyGamma;
        private String greyColorMap;

        //for Projection
        private Integer zStart;
        private Integer zStop;

        //Time
        private Integer tStart;
        private Integer tStop;

        private Vector rois; //Vector of ROIs

        public DisplayOptions(Float zoom) {
          this.zoom = zoom;
        }

        public void setRedChannelNumber(Integer rcn) {
          this.redChannelNumber = rcn;
        }
        public void setRedBlackLevel(Integer rbl) {
          this.redBlackLevel = rbl;
        }
        public void setRedWhiteLevel(Integer rwl) {
          this.redWhiteLevel = rwl;
        }
        public void setRedGamma(Float rg) {
          this.redGamma = rg;
        }
        public void setGreenChannelNumber(Integer gcn) {
          this.greenChannelNumber = gcn;
        }
        public void setGreenBlackLevel(Integer gbl) {
          this.greenBlackLevel = gbl;
        }
        public void setGreenWhiteLevel(Integer gwl) {
          this.greenWhiteLevel = gwl;
        }
        public void setGreenGamma(Float gg) {
          this.greenGamma = gg;
        }
        public void setBlueChannelNumber(Integer bcn) {
          this.blueChannelNumber = bcn;
        }
        public void setBlueBlackLevel(Integer bbl) {
          this.blueBlackLevel = bbl;
        }
        public void setBlueWhiteLevel(Integer bwl) {
          this.blueWhiteLevel = bwl;
        }
        public void setBlueGamma(Float gamma) {
          this.blueGamma = gamma;
        }
        public void setGreyChannelNumber(Integer gcn) {
          this.greyChannelNumber = gcn;
        }
        public void setGreyBlackLevel(Integer gbl) {
          this.greyBlackLevel = gbl;
        }
        public void setGreyWhiteLevel(Integer gwl) {
          this.greyWhiteLevel = gwl;
        }
        public void setGreyGamma(Float gg) {
          this.greyGamma = gg;
        }
        public void setGreyColorMap(String gcm) {
          this.greyColorMap = gcm;
        }
        public void setZStart(Integer zStart) {
          this.zStart = zStart;
        }
        public void setZStop(Integer zStop) {
          this.zStop = zStop;
        }
        public void setTStart(Integer tStart) {
          this.tStart = tStart;
        }
        public void setTStop(Integer tStop) {
          this.tStop = tStop;
        }
        public void addROI(Integer x0, Integer y0, Integer z0,
                           Integer x1, Integer y1, Integer z1, String t0, String t1) {
          if (rois == null) {
            rois = new Vector();
          }
          rois.add(new ROI(x0, y0, z0, x1, y1, z1, t0, t1));
        }

        public class ROI {
          private Integer x0;
          private Integer y0;
          private Integer z0;
          private Integer x1;
          private Integer y1;
          private Integer z1;
          private String t0;
          private String t1;

          public ROI(Integer x0, Integer y0, Integer z0,
                     Integer x1, Integer y1, Integer z1, String t0, String t1) {
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

      public class StageLabel {
        private String name;
        private Float x;
        private Float y;
        private Float z;

        public StageLabel(String name, Float x, Float y, Float z) {
          this.name = name;
          this.x = x;
          this.y = y;
          this.z = z;
        }
      }

      public class PlateInfo {
        //attributes
        private Integer plateID;

        //elements
        private String sampleExternRef;
        private Integer sampleNumber;

        private String wellAddress;
        private String wellExternRef;
        private String wellString;

        public PlateInfo(Integer id) {
          this.plateID = id;
        }

        public void setSampleExternRef(String ref) {
          this.sampleExternRef = ref;
        }
        public void setSampleNumber(Integer sn) {
          this.sampleNumber = sn;
        }
        public void setWellAddress(String add) {
          this.wellAddress = add;
        }
        public void setWellExternRef(String ref) {
          this.wellExternRef = ref;
        }
        public void setWellString(String ws) {
          this.wellString = ws;
        }
      }

      public class Data {
        //Attributes
        private String dimensionOrder;
        private String pixelType;
        private Boolean bigEndian;

        //Elements
        private BinExternal binExt;

        public Data(String dimOrd, String pt, Boolean be) {
          this.dimensionOrder = dimOrd;
          this.pixelType = pt;
          this.bigEndian = be;
        }

        public void setBinExternal(String comp, String s1, Integer offset, URI hRef) {
          this.binExt = new BinExternal(comp, s1, offset, hRef);
        }

        public class BinExternal {
          private String compression;
          private String sha1;
          private Integer offset;
          private URI hRef;

          public BinExternal(String comp, String s1, Integer offset, URI hRef) {
            this.compression = comp;
            this.sha1 = s1;
            this.offset = offset;
            this.hRef = hRef;
          }
        }
      }

      public class Feature {
        private String tag;
        private String name;

        public Feature(String tag) {
          this.tag = tag;
        }
      }

    }

  }

  public class ExperimenterRef {
    private Integer documentRef;
    private Integer experimentID;

    public ExperimenterRef(Integer documentRef, Integer expID) {
      this.documentRef = documentRef;
      this.experimentID = expID;
    }
  }

  public class ExperimentRef {
    private Integer documentRef;
    private String experimentID;

    public ExperimentRef(Integer docRef, String expID) {
      this.documentRef = docRef;
      this.experimentID = expID;
    }
  }

  public class ProjectRef {
    private Integer documentRef;
    private Integer projectID;

    public ProjectRef(Integer docRef, Integer id) {
      this.documentRef = docRef;
      this.projectID = id;
    }
  }

  public FileInfoOME(String name, String directory, int format) {
    super(name, directory, format);
  }

  public void displayAboutInfo(JDialogBase dialog, TransMatrix matrix) {
        /**@todo Implement this gov.nih.mipav.model.file.FileInfoBase abstract method*/
  }

}
