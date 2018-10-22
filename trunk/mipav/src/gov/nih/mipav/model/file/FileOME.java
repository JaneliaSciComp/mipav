package gov.nih.mipav.model.file;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import org.apache.xerces.jaxp.*;

import org.xml.sax.*;
import org.xml.sax.helpers.*;

import java.io.*;

import java.net.*;

// JAXP packages
import javax.xml.parsers.*;


/**
 * DOCUMENT ME!
 */
public class FileOME extends FileBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** The W3C XML schema. */
    private static final String W3C_XML_SCHEMA = "http://www.w3.org/2001/XMLSchema";

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** The directory where the xml header and raw image data file are located. */
    private String fileDir;

    /** The info for the image file. */
    private FileInfoOME fileInfo;

    /** The name of the xml image header file. */
    private String fileName;

    /** The image to read or write. */
    private ModelImage image;

    /** The name of the raw image data file. */
    private String imageFileName;

    /** The image's lut. */
    private ModelLUT LUT;

    /** Whether to show a progress bar while processing the file. */
    @SuppressWarnings("unused")
    private boolean showProgress;

    /** Whether the image has a lut already. One is generated if this variable is false. */
    private boolean usesLUT = false;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructs new file object.
     *
     * @param  fName  File name.
     * @param  fDir   File directory.
     * @param  show   Flag for showing the progress bar.
     */
    public FileOME(String fName, String fDir, boolean show) {
        fileName = fName;
        fileDir = fDir;
        showProgress = show;
        fileInfo = new FileInfoOME(fName, fDir, FileUtility.XML);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Read the image XML header.
     *
     * @param   headerFileName  the header file name
     * @param   headerFileDir   the directory the header file is in
     *
     * @return  true if successful, false otherwise
     *
     * @throws  IOException  if there is an error reading the header file from disk
     */
    public boolean readHeader(String headerFileName, String headerFileDir) throws IOException {

        fileInfo.setHeaderFileName(headerFileName);

        SAXParserFactory spf = SAXParserFactory.newInstance();

        spf.setNamespaceAware(true);

        spf.setValidating(true);

        try {

            // Create a JAXP SAXParser
            SAXParser saxParser = spf.newSAXParser();

            // Validation part 2a: set the schema language if necessary
            saxParser.setProperty(SAXParserImpl.JAXP_SCHEMA_LANGUAGE, W3C_XML_SCHEMA);

            // Validation part 2b: Set the schema source, if any. See the JAXP
            // 1.2 maintenance update specification for more complex usages of
            // this feature.
            URL xsdURL = getClass().getClassLoader().getResource("ome.xsd");

            if (xsdURL == null) {
                MipavUtil.displayError("Unable to find OME XML schema.");

                return false;
            }

            saxParser.setProperty(SAXParserImpl.JAXP_SCHEMA_SOURCE, xsdURL.toExternalForm());

            // Get the encapsulated SAX XMLReader
            XMLReader xmlReader = saxParser.getXMLReader();

            // Set the ContentHandler of the XMLReader
            xmlReader.setContentHandler(new MyXMLHandler());

            // Set an ErrorHandler before parsing
            xmlReader.setErrorHandler(new XMLErrorHandler());

            // Tell the XMLReader to parse the XML document
            xmlReader.parse(MipavUtil.convertToFileURL(headerFileDir + headerFileName));

            if (LUT != null) {

                if (!usesLUT) {
                    LUT.makeLUT(256);
                }

                LUT.makeIndexedLUT(null);
            }

        } catch (Exception error) {
            MipavUtil.displayError("Error: " + error.getMessage());

            return false;
        }

        return true;
    }

    /**
     * Reads an XML image file by reading the XML header then making a FileRaw to read the image for all filenames in
     * the file list. Only the one file directory (currently) supported.
     *
     * @exception  IOException       if there is an error reading the file
     * @exception  OutOfMemoryError  if there is not enough memory available to read in the image
     *
     * @return     The image.
     *
     * @see        FileRaw
     */
    public ModelImage readImage() throws IOException, OutOfMemoryError {
        fileInfo = new FileInfoOME(fileName, fileDir, FileUtility.XML);

        if (!readHeader(fileInfo.getFileName(), fileInfo.getFileDirectory())) {
            throw (new IOException("XML file error"));
        }

        int[] extents = null;

        try {

            extents = new int[fileInfo.getExtents().length];

            for (int i = 0; i < extents.length; i++) {
                extents[i] = fileInfo.getExtents()[i];
            }

            image = new ModelImage(fileInfo.getDataType(), new int[] { extents[0], extents[1] },
                                   fileInfo.getFileName());

        } catch (OutOfMemoryError error) {
            throw (error);
        }

        image.setMatrix(fileInfo.getMatrix());

        try { // Construct a FileRaw to actually read the image.

            FileRaw rawFile;

            // imageFileName was parsed from the "image".xml file.
            rawFile = new FileRaw(fileDir + imageFileName, fileInfo, FileBase.READ);

            long offset = 0L;

            rawFile.readImage(image, offset);

            FileInfoOME[] nFileInfos;

            if (fileInfo.getExtents().length > 2) { // Set file info

                int length = fileInfo.getExtents()[2];

                if (fileInfo.getExtents().length == 4) {
                    length *= fileInfo.getExtents()[3];
                }

                if (fileInfo.getExtents().length == 5) {
                    length *= fileInfo.getExtents()[4];
                }

                nFileInfos = new FileInfoOME[length];

                for (int i = 0; i < length; i++) {
                    nFileInfos[i] = (FileInfoOME) (fileInfo.clone());
                }

                image.setFileInfo(nFileInfos);
                // updateStartInfo(nFileInfos);
            } else {
                image.setFileInfo(fileInfo, 0);
            }

        } catch (IOException error) {
            throw new IOException("FileXML: " + error);
        } catch (OutOfMemoryError e) {
            throw (e);
        }

        return image;
    }

    //~ Inner Classes --------------------------------------------------------------------------------------------------

    /**
     * Private class used by the parser to parse the XML file.
     */
    private class MyXMLHandler extends DefaultHandler {

        /** The current XML tag we are parsing. */
        String currentKey;

        /** The data for the current element being parsed. */
        String elementBuffer = new String("");

        /** The parent XML tag of the element we are currently parsing. */
        String superKey;

        /**
         * Create a handler to fill out the image info in the parent class from the xml header data.
         */
        public MyXMLHandler() { }

        /**
         * Text data callback from parser. If the parser is not validating, this method can report whitespace. We ignore
         * strings that are entirely whitespace.
         *
         * @param  ch      Character array
         * @param  start   Start of data in array.
         * @param  length  Length of data in array.
         */
        public void characters(char[] ch, int start, int length) {
            String s = new String(ch, start, length);

            if (s.trim().length() != 0) {
                elementBuffer += s;
            }
        }

        /**
         * Handle the end of the XML document. Does nothing.
         *
         * @throws  SAXException  if there is a parser error
         */
        public void endDocument() throws SAXException { }

        /**
         * Parser calls this when the end tag of each element is reached. Data collected in the elementbuffer is
         * generally saved to the image info.
         *
         * @param   namespaceURI  the namespace (not used)
         * @param   localName     the current tag we are parsing (not used)
         * @param   qName         ? (not used)
         *
         * @throws  SAXException  if there is a problem with the parser
         */
        public void endElement(String namespaceURI, String localName, String qName) throws SAXException {

            if (currentKey.equals("Include")) {
                fileInfo.getOME().getCurrentInclude().setIncludeString(elementBuffer);
            } else if (currentKey.equals("Description")) {

                if (superKey.equals("Project")) {
                    fileInfo.getOME().getCurrentProject().setDescription(elementBuffer);
                } else if (superKey.equals("Dataset")) {
                    fileInfo.getOME().getCurrentDataset().setDescription(elementBuffer);
                } else if (superKey.equals("Experiment")) {
                    fileInfo.getOME().getCurrentExperiment().setDescription(elementBuffer);
                } else if (superKey.equals("Screen")) {
                    fileInfo.getOME().getCurrentScreen().setDescription(elementBuffer);
                } else if (superKey.equals("Image")) {
                    fileInfo.getOME().getCurrentImage().setDescription(elementBuffer);
                }
            } else if (currentKey.equals("FirstName")) {

                if (superKey.equals("Experimenter")) {
                    fileInfo.getOME().getCurrentExperimenter().setFirstName(elementBuffer);
                } else if (superKey.equals("Leader")) {
                    fileInfo.getOME().getCurrentGroup().setLeaderFirstName(elementBuffer);
                } else if (superKey.equals("Contact")) {
                    fileInfo.getOME().getCurrentGroup().setContactFirstName(elementBuffer);
                }
            } else if (currentKey.equals("LastName")) {

                if (superKey.equals("Experimenter")) {
                    fileInfo.getOME().getCurrentExperimenter().setLastName(elementBuffer);
                } else if (superKey.equals("Leader")) {
                    fileInfo.getOME().getCurrentGroup().setLeaderLastName(elementBuffer);
                } else if (superKey.equals("Contact")) {
                    fileInfo.getOME().getCurrentGroup().setContactLastName(elementBuffer);
                }

            } else if (currentKey.equals("email")) {

                if (superKey.equals("Experimenter")) {
                    fileInfo.getOME().getCurrentExperimenter().setEmail(elementBuffer);
                } else if (superKey.equals("Leader")) {
                    fileInfo.getOME().getCurrentGroup().setLeaderEmail(elementBuffer);
                } else if (superKey.equals("Contact")) {
                    fileInfo.getOME().getCurrentGroup().setContactEmail(elementBuffer);
                }

            } else if (currentKey.equals("Institution")) {

                if (superKey.equals("Experimenter")) {
                    fileInfo.getOME().getCurrentExperimenter().setInstitution(elementBuffer);
                } else if (superKey.equals("Leader")) {
                    fileInfo.getOME().getCurrentGroup().setLeaderInstitution(elementBuffer);
                } else if (superKey.equals("Contact")) {
                    fileInfo.getOME().getCurrentGroup().setContactInstitution(elementBuffer);
                }

            } else if (currentKey.equals("OMEName")) {

                if (superKey.equals("Experimenter")) {
                    fileInfo.getOME().getCurrentExperimenter().setOMEName(elementBuffer);
                }
            } else if (currentKey.equals("LensNA")) {
                fileInfo.getOME().getCurrentInstrument().getCurrentObjective().setLensNA(new Float(elementBuffer));
            } else if (currentKey.equals("Magnification")) {
                fileInfo.getOME().getCurrentInstrument().getCurrentObjective().setMagnification(new Float(elementBuffer));
            } else if (currentKey.equals("CreationDate")) {
                fileInfo.getOME().getCurrentImage().setCreationDate(elementBuffer);
            } else if (currentKey.equals("Sample")) {
                fileInfo.getOME().getCurrentImage().getPlateInfo().setSampleNumber(Integer.valueOf(elementBuffer));
            } else if (currentKey.equals("Well")) {
                fileInfo.getOME().getCurrentImage().getPlateInfo().setWellString(elementBuffer);
            }

        }

        /**
         * Handle any skipped entities by writing them out to the debug window.
         *
         * @param  name  the skipped entity
         */
        public void skippedEntity(String name) {
            Preferences.debug("OME: skipped entity: " + name + "\n", Preferences.DEBUG_FILEIO);
        }

        /**
         * Parser calls this for each element in a document.
         *
         * @param   namespaceURI  the namespace (not used)
         * @param   localName     the current tag we are parsing
         * @param   qName         ? (not used)
         * @param   atts          attributes for the current tag
         *
         * @throws  SAXException  if there is a problem with the parser
         */
        public void startElement(String namespaceURI, String localName, String qName, Attributes atts)
                throws SAXException {

            currentKey = localName;

            if (currentKey.equals("OME")) {

                // create the OME object
                fileInfo.setOME(fileInfo.createOME());
            } else if (currentKey.equals("Include")) {
                Integer documentID;
                String sha1;
                URI href = null;

                documentID = Integer.valueOf(atts.getValue("DocumentID"));

                try {
                    href = new URI(atts.getValue("href"));
                } catch (java.net.URISyntaxException e) {
                    Preferences.debug("OME: " + e + "\n", Preferences.DEBUG_FILEIO);
                }

                sha1 = atts.getValue("SHA1");

                fileInfo.getOME().addInclude(documentID, href, sha1);
            } else if (currentKey.equals("Project")) {
                String name;
                Integer id;

                name = atts.getValue("Name");
                id = Integer.valueOf(atts.getValue("ProjectID"));

                fileInfo.getOME().addProject(name, id);

                superKey = currentKey;
            } else if (currentKey.equals("ExperimenterRef")) {
                Integer documentRef, experimenterID;

                documentRef = Integer.valueOf(atts.getValue("DocumentRef"));
                experimenterID = Integer.valueOf(atts.getValue("ExperimenterID"));

                if (superKey.equals("Project")) {
                    fileInfo.getOME().getCurrentProject().setExperimenterRef(documentRef, experimenterID);
                } else if (superKey.equals("Dataset")) {
                    fileInfo.getOME().getCurrentDataset().setExperimenterRef(documentRef, experimenterID);
                } else if (superKey.equals("Experiment")) {
                    fileInfo.getOME().getCurrentExperiment().setExperimenterRef(documentRef, experimenterID);
                } else if (superKey.equals("Leader")) {
                    fileInfo.getOME().getCurrentGroup().setLeaderExperimenterRef(documentRef, experimenterID);
                } else if (superKey.equals("Contact")) {
                    fileInfo.getOME().getCurrentGroup().setContactExperimenterRef(documentRef, experimenterID);
                }
            } else if (currentKey.equals("GroupRef")) {
                Integer groupID;

                groupID = Integer.valueOf(atts.getValue("GroupID"));

                if (superKey.equals("Project")) {
                    fileInfo.getOME().getCurrentProject().setGroupRef(groupID);
                } else if (superKey.equals("Dataset")) {
                    fileInfo.getOME().getCurrentDataset().setGroupRef(groupID);
                } else if (superKey.equals("Experimenter")) {
                    fileInfo.getOME().getCurrentExperimenter().addGroupRef(groupID);
                } else if (superKey.equals("Image")) {
                    fileInfo.getOME().getCurrentImage().setGroupRef(groupID);
                }
            } else if (currentKey.equals("Dataset")) {
                String name;
                Integer datasetID;
                Boolean locked;

                name = atts.getValue("Name");
                datasetID = Integer.valueOf(atts.getValue("DatasetID"));

                try {
                    locked = Boolean.valueOf(atts.getValue("Locked"));
                } catch (NullPointerException e) {
                    locked = Boolean.FALSE;
                }

                fileInfo.getOME().addDataset(name, datasetID, locked);
                superKey = currentKey;
            } else if (currentKey.equals("Experiment")) {
                Integer experimentID;

                experimentID = Integer.valueOf(atts.getValue("ExperimentID"));

                fileInfo.getOME().addExperiment(experimentID);
                superKey = currentKey;
            } else if (currentKey.equals("Plate")) {
                Integer plateID;
                String name, ref;

                plateID = Integer.valueOf(atts.getValue("PlateID"));
                name = atts.getValue("Name");
                ref = atts.getValue("ExternRef");

                fileInfo.getOME().addPlate(plateID, name, ref);
            } else if (currentKey.equals("ScreenRef")) {
                Integer docRef;
                Integer screenID;

                docRef = Integer.valueOf(atts.getValue("DocumentRef"));
                screenID = Integer.valueOf(atts.getValue("ScreenID"));

                fileInfo.getOME().getCurrentPlate().addScreenRef(docRef, screenID);
            } else if (currentKey.equals("Screen")) {
                Integer screenID;
                String name, externRef;

                screenID = Integer.valueOf(atts.getValue("ScreenID"));
                name = atts.getValue("Name");
                externRef = atts.getValue("ExternRef");

                fileInfo.getOME().addScreen(screenID, name, externRef);
                superKey = currentKey;
            } else if (currentKey.equals("Experimenter")) {
                Integer id;

                id = Integer.valueOf(atts.getValue("ExperimenterID"));

                fileInfo.getOME().addExperimenter(id);
                superKey = currentKey;
            } else if (currentKey.equals("Group")) {
                String name;
                Integer id;

                name = atts.getValue("Name");
                id = Integer.valueOf(atts.getValue("GroupID"));

                fileInfo.getOME().addGroup(name, id);
            } else if (currentKey.equals("Leader")) {
                superKey = currentKey;
            } else if (currentKey.equals("Contact")) {
                superKey = currentKey;
            } else if (currentKey.equals("Instrument")) {
                Integer id;

                id = Integer.valueOf(atts.getValue("InstrumentID"));

                fileInfo.getOME().addInstrument(id);
            } else if (currentKey.equals("Microscrope")) {
                String man, mod, sn, type;

                man = atts.getValue("Manufacturer");
                mod = atts.getValue("Model");
                sn = atts.getValue("SerialNumber");
                type = atts.getValue("Type");

                fileInfo.getOME().getCurrentInstrument().setMicroscope(man, mod, sn, type);
            } else if (currentKey.equals("LightSource")) {
                String man, mod, sn;
                Integer id;

                man = atts.getValue("Manufacturer");
                mod = atts.getValue("Model");
                sn = atts.getValue("SerialNumber");
                id = Integer.valueOf(atts.getValue("LightSourceID"));

                fileInfo.getOME().getCurrentInstrument().addLightSource(man, mod, sn, id);
            } else if (currentKey.equals("Laser")) {
                String type, medium, pulse;
                Integer wavelength = null;
                Boolean fd = null, tunable = null;
                Float power = null;

                type = atts.getValue("Type");
                medium = atts.getValue("Medium");

                String temp = null;
                temp = atts.getValue("Wavelength");

                if (temp != null) {
                    wavelength = Integer.valueOf(temp);
                    temp = null;
                }

                temp = atts.getValue("FrequencyDoubled");

                if (temp != null) {
                    fd = Boolean.valueOf(temp);
                    temp = null;
                }

                temp = atts.getValue("Tunable");

                if (temp != null) {
                    tunable = Boolean.valueOf(temp);
                    temp = null;
                }

                pulse = atts.getValue("Pulse");

                temp = atts.getValue("Power");

                if (temp != null) {
                    power = new Float(temp);
                }

                fileInfo.getOME().getCurrentInstrument().getCurrentLightSource().setLaser(type, medium, wavelength, fd,
                                                                                          tunable, pulse, power);
            } else if (currentKey.equals("Pump")) {
                Integer ref = null, id;

                String temp = atts.getValue("DocumentRef");

                if (temp != null) {
                    ref = Integer.valueOf(temp);
                }

                id = Integer.valueOf(atts.getValue("LightSourceID"));

                fileInfo.getOME().getCurrentInstrument().getCurrentLightSource().getLaser().setPump(ref, id);
            } else if (currentKey.equals("Filament")) {
                String type;
                Float power = null;

                type = atts.getValue("Type");

                String temp = atts.getValue("Power");

                if (temp != null) {
                    power = new Float(temp);
                }

                fileInfo.getOME().getCurrentInstrument().getCurrentLightSource().setFilament(type, power);
            } else if (currentKey.equals("Arc")) {
                String type;
                Float power = null;

                type = atts.getValue("Type");

                String temp = atts.getValue("Power");

                if (temp != null) {
                    power = new Float(temp);
                }

                fileInfo.getOME().getCurrentInstrument().getCurrentLightSource().setArc(type, power);
            } else if (currentKey.equals("Detector")) {
                String man, mod, sn, type;
                Float gain = null, voltage = null, offset = null;
                Integer id;

                man = atts.getValue("Manufacturer");
                mod = atts.getValue("Model");
                sn = atts.getValue("SerialNumber");

                String temp = atts.getValue("Gain");

                if (temp != null) {
                    gain = new Float(temp);
                    temp = null;
                }

                temp = atts.getValue("Voltage");

                if (temp != null) {
                    voltage = new Float(temp);
                    temp = null;
                }

                temp = atts.getValue("Offset");

                if (temp != null) {
                    offset = new Float(temp);
                }

                id = Integer.valueOf(atts.getValue("DetectorID"));
                type = atts.getValue("Type");

                fileInfo.getOME().getCurrentInstrument().addDetector(man, mod, sn, gain, voltage, offset, id, type);
            } else if (currentKey.equals("Objective")) {
                String man, mod, sn;
                Integer id;

                man = atts.getValue("Manufacturer");
                mod = atts.getValue("Model");
                sn = atts.getValue("SerialNumber");
                id = Integer.valueOf(atts.getValue("ObjectiveID"));

                fileInfo.getOME().getCurrentInstrument().addObjective(man, mod, sn, id);
            } else if (currentKey.equals("Filter")) {
                Integer id;

                id = Integer.valueOf(atts.getValue("FilterID"));

                fileInfo.getOME().getCurrentInstrument().addFilter(id);
            } else if (currentKey.equals("ExFilter")) {
                String man, mod, ln, type;

                man = atts.getValue("Manufacturer");
                mod = atts.getValue("Model");
                ln = atts.getValue("LotNumber");
                type = atts.getValue("Type");

                fileInfo.getOME().getCurrentInstrument().getCurrentFilter().setExFilter(man, mod, ln, type);
            } else if (currentKey.equals("Dichroic")) {
                String man, mod, ln, type;

                man = atts.getValue("Manufacturer");
                mod = atts.getValue("Model");
                ln = atts.getValue("LotNumber");
                type = atts.getValue("Type");

                fileInfo.getOME().getCurrentInstrument().getCurrentFilter().setDichroic(man, mod, ln, type);
            } else if (currentKey.equals("EmFilter")) {
                String man, mod, ln, type;

                man = atts.getValue("Manufacturer");
                mod = atts.getValue("Model");
                ln = atts.getValue("LotNumber");
                type = atts.getValue("Type");

                fileInfo.getOME().getCurrentInstrument().getCurrentFilter().setEmFilter(man, mod, ln, type);
            } else if (currentKey.equals("FilterSet")) {
                String man, mod, ln;

                man = atts.getValue("Manufacturer");
                mod = atts.getValue("Model");
                ln = atts.getValue("LotNumber");

                fileInfo.getOME().getCurrentInstrument().getCurrentFilter().setFilterSet(man, mod, ln);
            } else if (currentKey.equals("OTF")) {
                Integer otfid, objid, fid, sizex, sizey;
                String pt;
                Boolean oaa;

                otfid = Integer.valueOf(atts.getValue("OTFID"));
                objid = Integer.valueOf(atts.getValue("ObjectiveID"));
                fid = Integer.valueOf(atts.getValue("FilterID"));
                pt = atts.getValue("PixelType");
                oaa = Boolean.valueOf(atts.getValue("OpticalAxisAvrg"));
                sizex = Integer.valueOf(atts.getValue("SizeX"));
                sizey = Integer.valueOf(atts.getValue("SizeY"));

                fileInfo.getOME().getCurrentInstrument().addOTF(otfid, objid, fid, pt, oaa, sizex, sizey);
                superKey = currentKey;
            } else if (currentKey.equals("Bin:External")) {
                String compression, sha1;
                Integer offset = null;
                URI href = null;

                compression = atts.getValue("Compression");
                sha1 = atts.getValue("SHA1");

                String temp = atts.getValue("Offset");

                if (temp != null) {
                    offset = Integer.valueOf(temp);
                }

                try {
                    href = new URI(atts.getValue("href"));
                } catch (java.net.URISyntaxException e) {
                    Preferences.debug("OME: " + e + "\n", Preferences.DEBUG_FILEIO);
                }

                if (superKey.equals("OTF")) {
                    fileInfo.getOME().getCurrentInstrument().getCurrentOTF().setBinExternal(compression, sha1, offset,
                                                                                            href);
                } else if (superKey.equals("Data")) {
                    fileInfo.getOME().getCurrentImage().getData().setBinExternal(compression, sha1, offset, href);
                }
            } else if (currentKey.equals("Bin:BinData")) {

                // what do we do?
                Preferences.debug("OME: Got a Bin:BinData\n", Preferences.DEBUG_FILEIO);
            } else if (currentKey.equals("Image")) {
                String guid, imageType, name;
                Integer sizeX, sizeY, sizeZ, numChannels, numTimes, waveStart = null, waveIncrement = null;
                Float pixelSizeX, pixelSizeY, pixelSizeZ = null, timeIncrement = null;

                guid = atts.getValue("GUID"); // optional
                imageType = atts.getValue("ImageType");
                name = atts.getValue("Name");
                sizeX = Integer.valueOf(atts.getValue("SizeX"));
                sizeY = Integer.valueOf(atts.getValue("SizeY"));
                sizeZ = Integer.valueOf(atts.getValue("SizeZ"));
                numChannels = Integer.valueOf(atts.getValue("NumChannels"));
                numTimes = Integer.valueOf(atts.getValue("NumTimes"));
                pixelSizeX = new Float(atts.getValue("PixelSizeX"));
                pixelSizeY = new Float(atts.getValue("PixelSizeY"));

                String temp = null;
                temp = atts.getValue("PixelSizeZ");

                if (temp != null) {
                    pixelSizeZ = new Float(temp);
                    temp = null;
                }

                temp = atts.getValue("TimeIncrement");

                if (temp != null) {
                    timeIncrement = new Float(temp);
                    temp = null;
                }

                temp = atts.getValue("WaveStart");

                if (temp != null) {
                    waveStart = Integer.valueOf(atts.getValue(temp));
                    temp = null;
                }

                temp = atts.getValue("WaveIncrement");

                if (temp != null) {
                    waveIncrement = Integer.valueOf(temp);
                }

                // Create the image object
                fileInfo.getOME().addImage(guid, imageType, name, sizeX, sizeY, sizeZ, numChannels, numTimes,
                                           pixelSizeX, pixelSizeY, pixelSizeZ, timeIncrement, waveStart, waveIncrement);
                superKey = currentKey;
            } else if (currentKey.equals("ExperimentRef")) {
                Integer docRef = null;
                String expID;

                String temp = atts.getValue("DocumentRef");

                if (temp != null) {
                    docRef = Integer.valueOf(atts.getValue(temp));
                }

                expID = atts.getValue("ExperimentID");

                fileInfo.getOME().getCurrentImage().setExperimentRef(docRef, expID);
            } else if (currentKey.equals("DerivedImage")) {
                String method;

                method = atts.getValue("Method");

                fileInfo.getOME().getCurrentImage().setDerivedImage(method);
            } else if (currentKey.equals("DatasetRef")) {
                Integer datasetID;

                datasetID = Integer.valueOf(atts.getValue("DatasetID"));

                fileInfo.getOME().getCurrentImage().addDatasetRef(datasetID);
            } else if (currentKey.equals("InstrumentRef")) {
            	@SuppressWarnings("unused")
                Integer docRef = null, instID, objID;

                String temp = atts.getValue("DocumentRef");

                if (temp != null) {
                    docRef = Integer.valueOf(temp);
                }

                instID = Integer.valueOf(atts.getValue("InstrumentID"));
                objID = Integer.valueOf(atts.getValue("ObjectiveID"));
            } else if (currentKey.equals("ImagingEnvironment")) {
                Float temperature = null, ap = null, hum = null, CO2 = null;

                String temp = atts.getValue("Temperature");

                if (temp != null) {
                    temperature = new Float(temp);
                    temp = null;
                }

                temp = atts.getValue("AirPressure");

                if (temp != null) {
                    ap = new Float(temp);
                    temp = null;
                }

                temp = atts.getValue("Humidity");

                if (temp != null) {
                    hum = new Float(temp);
                    temp = null;
                }

                temp = atts.getValue("CO2Percent");

                if (temp != null) {
                    CO2 = new Float(temp);
                }

                fileInfo.getOME().getCurrentImage().setTemperature(temperature);
                fileInfo.getOME().getCurrentImage().setAirPressure(ap);
                fileInfo.getOME().getCurrentImage().setHumidity(hum);
                fileInfo.getOME().getCurrentImage().setCO2Percent(CO2);
            } else if (currentKey.equals("Thumbnail")) {
                String mimeType;

                String temp = atts.getValue("href");

                if (temp != null) {

                    try {
                        fileInfo.getOME().getCurrentImage().setThumbnail(new URI(temp));
                    } catch (Exception e) {
                        Preferences.debug("OME: " + e + "\n", Preferences.DEBUG_FILEIO);
                    }
                }

                mimeType = atts.getValue("MIMEtype");
                fileInfo.getOME().getCurrentImage().setMIMEType(mimeType);
            } else if (currentKey.equals("ChannelInfo")) {
                String name, illType, photoInt, mode, contrastMethod, fluor;
                Integer spp = null, phs = null, exWave = null, emWave = null;
                Float ndFilter = null;

                name = atts.getValue("Name");

                String temp = atts.getValue("SamplesPerPixel");

                if (temp != null) {
                    spp = Integer.valueOf(temp);
                    temp = null;
                }

                illType = atts.getValue("IlluminationType");
                temp = atts.getValue("PinHoleSize");

                if (temp != null) {
                    phs = Integer.valueOf(temp);
                    temp = null;
                }

                photoInt = atts.getValue("PhotometricInterpretation");
                mode = atts.getValue("Mode");
                contrastMethod = atts.getValue("ContrastMethod");
                temp = atts.getValue("ExWave");

                if (temp != null) {
                    exWave = Integer.valueOf(temp);
                    temp = null;
                }

                temp = atts.getValue("EmWave");

                if (temp != null) {
                    emWave = Integer.valueOf(temp);
                    temp = null;
                }

                fluor = atts.getValue("Fluor");
                temp = atts.getValue("NDfilter");

                if (temp != null) {
                    ndFilter = new Float(temp);
                }

                fileInfo.getOME().getCurrentImage().addChannelInfo(name, spp, illType, phs, photoInt, mode,
                                                                   contrastMethod, exWave, emWave, fluor, ndFilter);
            } else if (currentKey.equals("LightSourceRef")) {
                Integer docRef = null, lightSourceID, wavelength = null;
                String auxTech;
                Float atten = null;

                String temp = atts.getValue("DocumentRef");

                if (temp != null) {
                    docRef = Integer.valueOf(temp);
                    temp = null;
                }

                lightSourceID = Integer.valueOf(atts.getValue("LightSourceID"));
                auxTech = atts.getValue("AuxTechnique");
                temp = atts.getValue("Attenuation");

                if (temp != null) {
                    atten = new Float(temp);
                    temp = null;
                }

                temp = atts.getValue("Wavelength");

                if (temp != null) {
                    wavelength = Integer.valueOf(temp);
                }

                fileInfo.getOME().getCurrentImage().getCurrentChannelInfo().addLightSourceRef(docRef, lightSourceID,
                                                                                              auxTech, atten,
                                                                                              wavelength);
            } else if (currentKey.equals("OTFRef")) {
                Integer docRef = null, otfID;

                String temp = atts.getValue("DocumentRef");

                if (temp != null) {
                    docRef = Integer.valueOf(temp);
                }

                otfID = Integer.valueOf(atts.getValue("OTFID"));

                fileInfo.getOME().getCurrentImage().getCurrentChannelInfo().setOTFRefDocumentRef(docRef);
                fileInfo.getOME().getCurrentImage().getCurrentChannelInfo().setOTFRefOTFID(otfID);
            } else if (currentKey.equals("DetectorRef")) {
            	@SuppressWarnings("unused")
                Integer docRef = null, id;

                String temp = atts.getValue("DocumentRef");

                if (temp != null) {
                    fileInfo.getOME().getCurrentImage().getCurrentChannelInfo().setDetectorRefDocumentRef(Integer.valueOf(temp));
                    temp = null;
                }

                id = Integer.valueOf(atts.getValue("DetectorID"));
                fileInfo.getOME().getCurrentImage().getCurrentChannelInfo().setDetectorRefDetectorID(id);
                temp = atts.getValue("Offset");

                if (temp != null) {
                    fileInfo.getOME().getCurrentImage().getCurrentChannelInfo().setDetectorRefOffset(new Float(temp));
                    temp = null;
                }

                temp = atts.getValue("Gain");

                if (temp != null) {
                    fileInfo.getOME().getCurrentImage().getCurrentChannelInfo().setDetectorRefGain(new Float(temp));
                }
            } else if (currentKey.equals("FilterRef")) {
                Integer filterID;

                String temp = atts.getValue("DocumentRef");

                if (temp != null) {
                    fileInfo.getOME().getCurrentImage().getCurrentChannelInfo().setFilterRefDocumentRef(Integer.valueOf(temp));
                }

                filterID = Integer.valueOf(atts.getValue("FilterID"));
                fileInfo.getOME().getCurrentImage().getCurrentChannelInfo().setFilterRefFilterID(filterID);
            } else if (currentKey.equals("ChannelComponent")) {
                String cd = null;
                Integer ccn;

                cd = atts.getValue("ColorDomain");
                ccn = Integer.valueOf(atts.getValue("ChannelComponentNumber"));

                fileInfo.getOME().getCurrentImage().getCurrentChannelInfo().addChannelComponent(cd, ccn);
            } else if (currentKey.equals("DisplayOptions")) {
                Float zoom = null;

                String temp = atts.getValue("Zoom");

                if (temp != null) {
                    zoom = new Float(temp);
                }

                fileInfo.getOME().getCurrentImage().setDisplayOptions(zoom);
            } else if (currentKey.equals("RedChannel")) {
                Integer cn, bl, wl;
                Float gamma = null;

                cn = Integer.valueOf(atts.getValue("ChannelNumber"));
                bl = Integer.valueOf(atts.getValue("BlackLevel"));
                wl = Integer.valueOf(atts.getValue("WhiteLevel"));

                String temp = atts.getValue("Gamma");

                if (temp != null) {
                    gamma = new Float(temp);
                }

                fileInfo.getOME().getCurrentImage().getDisplayOptions().setRedChannelNumber(cn);
                fileInfo.getOME().getCurrentImage().getDisplayOptions().setRedBlackLevel(bl);
                fileInfo.getOME().getCurrentImage().getDisplayOptions().setRedWhiteLevel(wl);
                fileInfo.getOME().getCurrentImage().getDisplayOptions().setRedGamma(gamma);
            } else if (currentKey.equals("GreenChannel")) {
                Integer cn, bl, wl;
                Float gamma = null;

                cn = Integer.valueOf(atts.getValue("ChannelNumber"));
                bl = Integer.valueOf(atts.getValue("BlackLevel"));
                wl = Integer.valueOf(atts.getValue("WhiteLevel"));

                String temp = atts.getValue("Gamma");

                if (temp != null) {
                    gamma = new Float(temp);
                }

                fileInfo.getOME().getCurrentImage().getDisplayOptions().setGreenChannelNumber(cn);
                fileInfo.getOME().getCurrentImage().getDisplayOptions().setGreenBlackLevel(bl);
                fileInfo.getOME().getCurrentImage().getDisplayOptions().setGreenWhiteLevel(wl);
                fileInfo.getOME().getCurrentImage().getDisplayOptions().setGreenGamma(gamma);
            } else if (currentKey.equals("BlueChannel")) {
                Integer cn, bl, wl;
                Float gamma = null;

                cn = Integer.valueOf(atts.getValue("ChannelNumber"));
                bl = Integer.valueOf(atts.getValue("BlackLevel"));
                wl = Integer.valueOf(atts.getValue("WhiteLevel"));

                String temp = atts.getValue("Gamma");

                if (temp != null) {
                    gamma = new Float(temp);
                }

                fileInfo.getOME().getCurrentImage().getDisplayOptions().setBlueChannelNumber(cn);
                fileInfo.getOME().getCurrentImage().getDisplayOptions().setBlueBlackLevel(bl);
                fileInfo.getOME().getCurrentImage().getDisplayOptions().setBlueWhiteLevel(wl);
                fileInfo.getOME().getCurrentImage().getDisplayOptions().setBlueGamma(gamma);
            } else if (currentKey.equals("GreyChannel")) {
                Integer cn, bl, wl;
                Float gamma = null;
                String cm;

                cn = Integer.valueOf(atts.getValue("ChannelNumber"));
                bl = Integer.valueOf(atts.getValue("BlackLevel"));
                wl = Integer.valueOf(atts.getValue("WhiteLevel"));

                String temp = atts.getValue("Gamma");

                if (temp != null) {
                    gamma = new Float(temp);
                }

                cm = atts.getValue("ColorMap");

                fileInfo.getOME().getCurrentImage().getDisplayOptions().setGreyChannelNumber(cn);
                fileInfo.getOME().getCurrentImage().getDisplayOptions().setGreyBlackLevel(bl);
                fileInfo.getOME().getCurrentImage().getDisplayOptions().setGreyWhiteLevel(wl);
                fileInfo.getOME().getCurrentImage().getDisplayOptions().setGreyGamma(gamma);
                fileInfo.getOME().getCurrentImage().getDisplayOptions().setGreyColorMap(cm);
            } else if (currentKey.equals("Projection")) {
                String temp = atts.getValue("Zstart");

                if (temp != null) {
                    fileInfo.getOME().getCurrentImage().getDisplayOptions().setZStart(Integer.valueOf(temp));
                    temp = null;
                }

                temp = atts.getValue("Zstop");

                if (temp != null) {
                    fileInfo.getOME().getCurrentImage().getDisplayOptions().setZStop(Integer.valueOf(temp));
                }
            } else if (currentKey.equals("Time")) {
                String temp = atts.getValue("Tstart");

                if (temp != null) {
                    fileInfo.getOME().getCurrentImage().getDisplayOptions().setTStart(Integer.valueOf(temp));
                    temp = null;
                }

                temp = atts.getValue("Tstop");

                if (temp != null) {
                    fileInfo.getOME().getCurrentImage().getDisplayOptions().setTStop(Integer.valueOf(temp));
                }
            } else if (currentKey.equals("ROI")) {
                Integer x0, y0, z0 = null, x1, y1, z1 = null;
                String t0, t1;

                x0 = Integer.valueOf(atts.getValue("X0"));
                y0 = Integer.valueOf(atts.getValue("Y0"));

                String temp = atts.getValue("Z0");

                if (temp != null) {
                    z0 = Integer.valueOf(temp);
                    temp = null;
                }

                x1 = Integer.valueOf(atts.getValue("X1"));
                y1 = Integer.valueOf(atts.getValue("Y1"));
                temp = atts.getValue("Z1");

                if (temp != null) {
                    z1 = Integer.valueOf(temp);
                }

                t0 = atts.getValue("T0");
                t1 = atts.getValue("T1");

                fileInfo.getOME().getCurrentImage().getDisplayOptions().addROI(x0, y0, z0, x1, y1, z1, t0, t1);
            } else if (currentKey.equals("StageLabel")) {
                String name;
                Float x = null, y = null, z = null;

                name = atts.getValue("Name");

                String temp = atts.getValue("X");

                if (temp != null) {
                    x = new Float(temp);
                    temp = null;
                }

                temp = atts.getValue("Y");

                if (temp != null) {
                    y = new Float(temp);
                    temp = null;
                }

                temp = atts.getValue("Z");

                if (temp != null) {
                    z = new Float(temp);
                }

                fileInfo.getOME().getCurrentImage().setStageLabel(name, x, y, z);
            } else if (currentKey.equals("PlateInfo")) {
                Integer id;

                id = Integer.valueOf(atts.getValue("PlateID"));

                fileInfo.getOME().getCurrentImage().setPlateInfo(id);
            } else if (currentKey.equals("Sample")) {
                String externRef;

                externRef = atts.getValue("ExternRef");
                fileInfo.getOME().getCurrentImage().getPlateInfo().setSampleExternRef(externRef);
            } else if (currentKey.equals("Well")) {
                String address, externRef;

                address = atts.getValue("Address");
                externRef = atts.getValue("ExternRef");

                fileInfo.getOME().getCurrentImage().getPlateInfo().setWellAddress(address);
                fileInfo.getOME().getCurrentImage().getPlateInfo().setWellExternRef(externRef);
            } else if (currentKey.equals("Data")) {
                String dimOrd, pt;
                Boolean bigEndian;

                dimOrd = atts.getValue("DimensionOrder");
                pt = atts.getValue("PixelType");
                bigEndian = Boolean.valueOf(atts.getValue("BigEndian"));

                fileInfo.getOME().getCurrentImage().setData(dimOrd, pt, bigEndian);
            }

            // clear the element buffer
            elementBuffer = "";
        }
    }
}
