import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.plugins.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.components.*;
import gov.nih.mipav.view.srb.*;

import edu.sdsc.grid.io.*;
import edu.sdsc.grid.io.local.*;
import edu.sdsc.grid.io.srb.*;

import java.awt.*;
import java.awt.event.*;

import java.io.*;

import java.net.*;

import java.util.*;

import javax.swing.*;


/**
 * Demo plugin to read/write image files using SRB.
 *
 * @author  mccreedy
 */
public class PlugInFileSRB implements PlugInFile, ScriptableInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /**
     * Location of the SRB Zone Authority. When given a zone name the zone authority returns a xml document with the
     * host and port information for that zone. see also, http://www.sdsc.edu/srb/cgi-bin/zoneList.cgi?zone=ZoneName
     */
    static String ZONE_AUTHORITY = "http://www.sdsc.edu/srb/cgi-bin/zoneList.cgi?zone=";


    /** DOCUMENT ME! */
    private static final String LOCAL_TEMP_DIR = "file:/C:/temp/srb-temp/";

    /** Used to indicate whether it is the source or the destination. */
    private static final int SOURCE = 0;

    /** DOCUMENT ME! */
    private static final int DESTINATION = 1;

    /** Three allowed dialog type of getting URI. */
    private static final int OPEN_DIALOG = 0;

    /** DOCUMENT ME! */
    private static final int SAVE_DIALOG = 1;

    /** DOCUMENT ME! */
    private static final int TRANSFER_DIALOG = 2;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Used to hold the srb file system information. */
    private SRBFileSystem srbFileSystem = null;

    /** The local temporary directory which is used by this class. */
    private String srbTempDir = "file:/C:/temp/srb-temp/";

    /** DOCUMENT ME! */
    private JDialog uriDialog;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new PlugInFileSRB object.
     */
    public PlugInFileSRB() { }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Creates the local file list according to the local directory and the file name list.
     *
     * @param   localDir      the target local directory.
     * @param   fileNameList  the file name list.
     *
     * @return  the local file list which are under the local directory.
     */
    public static Vector createLocalFileList(LocalFile localDir, Vector fileNameList) {

        if ((localDir == null) || (fileNameList == null)) {
            return null;
        }

        Vector fileList = new Vector();

        for (int i = 0; i < fileNameList.size(); i++) {
            Object obj = fileNameList.get(i);

            if (obj instanceof String) {
                fileList.add(new LocalFile(localDir, (String) fileNameList.get(i)));
            } else if (obj instanceof SRBFile) {
                fileList.add(new LocalFile(localDir, ((SRBFile) fileNameList.get(i)).getName()));
            }
        }

        return fileList;
    }

    /**
     * Creates the srb file list according to the srb directory and the file name list.
     *
     * @param   srbDir        the target srb directory.
     * @param   fileNameList  the file name list.
     *
     * @return  the srb file list which are under the srb directory.
     */
    public static Vector createSRBFileList(SRBFile srbDir, Vector fileNameList) {

        if ((srbDir == null) || (fileNameList == null)) {
            return null;
        }

        Vector fileList = new Vector();

        for (int i = 0; i < fileNameList.size(); i++) {
            fileList.add(new SRBFile(srbDir, (String) fileNameList.get(i)));
        }

        return fileList;
    }

    /**
     * Creates the srb file system from the given uri, if it fails, then use the srb login dialog to get the needed
     * information to create srb file systme.
     *
     * @param   uri  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public static SRBFileSystem createSRBFileSystem(URI uri) {

        if (uri == null) {
            return null;
        }

        if (!isSRBURI(uri)) {
            return null;
        }

        try {
            SRBAccount srbAccount = uriInitialAccount(uri);

            if (srbAccount == null) {
                JDialogLoginSRB loginDialog = new JDialogLoginSRB("Connect to");
                SRBFileSystem srbFileSystem = loginDialog.getSRBFileSystem();

                return srbFileSystem;
            }
        } catch (IOException e) {
            e.printStackTrace(System.err);
            MipavUtil.displayError("Error encountered creating SRBAccount from URI(" + uri.toString() + "): " +
                                   e.getMessage());

            return null;
        }

        return null;
    }

    /**
     * DOCUMENT ME!
     *
     * @param   srbFile  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public static Vector getFileList(SRBFile srbFile) {

        if (srbFile == null) {
            return null;
        }

        Vector fileNameList = new Vector();

        /**
         * First adds the selected file name into the list.
         */
        fileNameList.add(srbFile);

        /**
         * Gets the extension of the selected file.
         */
        String extension = getExtension(srbFile.getName());

        /**
         * According to the file type, determines what other files should be added into the file list.
         */
        if (extension.toLowerCase().equals(".xml")) {
            fileNameList.add(new SRBFile((SRBFile) srbFile.getParentFile(),
                                         srbFile.getName().toLowerCase().replaceFirst(".xml", ".raw")));
        } else if (extension.toLowerCase().equals(".img")) {
            fileNameList.add(new SRBFile((SRBFile) srbFile.getParentFile(),
                                         srbFile.getName().toLowerCase().replaceFirst(".img", ".hdr")));
        } else if (extension.toLowerCase().equals(".head")) {
            fileNameList.add(new SRBFile((SRBFile) srbFile.getParentFile(),
                                         srbFile.getName().toLowerCase().replaceFirst(".head", ".brik")));
        }

        return fileNameList;
    }

    /**
     * Gets the file name list that the current image contains according to the file info.
     *
     * @param   fileInfoList  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public static Vector getFileNameList(FileInfoBase[] fileInfoList) {

        if ((fileInfoList == null) || (fileInfoList.length == 0)) {
            return null;
        }

        FileInfoBase fileInfo = fileInfoList[0];
        int fileFormat = fileInfo.getFileFormat();
        Vector fileNameList = null;

        switch (fileFormat) {

            /** Ill defined file type.              */
            case FileBase.ERROR:
                return null;

            /** Undefined file type.                */
            case FileBase.UNDEFINED:
                return null;

            /** Not presently implemented.          */
            case FileBase.MIPAV:
                return null;

            /** RAW image data, no header.          */
            case FileBase.RAW:
                return null;

            /** TIFF file; tagged header            */
            case FileBase.TIFF:
                return null;

            /** VOI file, used to read VOIs.        */
            case FileBase.VOI_FILE:
                return null;

            /** Analyze format (Mayo).              */
            case FileBase.ANALYZE:

            /** NIFTI format */
            case FileBase.NIFTI:
                fileNameList = new Vector();

                String imgFileName = fileInfo.getFileName();
                String hdrFileName = imgFileName.replaceFirst(".img", ".hdr");
                fileNameList.add(hdrFileName);
                fileNameList.add(imgFileName);

                return fileNameList;

            /** Digital Imaging and COmmunications in Medicine file type.
             * Fully implemented versions 2 & 3.   */
            case FileBase.DICOM:
                fileNameList = new Vector();
                for (int i = 0; i < fileInfoList.length; i++) {
                    fileNameList.add(fileInfoList[i].getFileName());
                }

                return fileNameList;

            /** Medvision file type.                */
            case FileBase.MEDVISION:
                return null;

            /** Benes Trus special file type.       */
            case FileBase.MAP:
                return null;

            /** Java Image Manangement Interface file type. */
            case FileBase.JIMI:
                return null;

            /** Multiple files of TIFF images.      */
            case FileBase.TIFF_MULTIFILE:
                return null;

            /** MINC file type.  MINC is a medical imaging oriented extension
             * of the NetCDF file format. NetCDF stands for `Network Common Data Form'.  */
            case FileBase.MINC:
                fileNameList = new Vector();
                fileNameList.add(fileInfo.getFileName());

                return fileNameList;

            /** AVI file type.  Windows Media.*/
            case FileBase.AVI:
                return null;

            /** Multiple files of type analyze.     */
            case FileBase.ANALYZE_MULTIFILE:
                return null;

            /** Quicktime file type.            */
            case FileBase.QT:
                return null;

            /** Cheshire file type (a kind of Analyze).*/
            case FileBase.CHESHIRE:
                return null;

            /** Cheshire overlay file type.  Contains VOIs. */
            case FileBase.CHESHIRE_OVERLAY:
                return null;

            /** AFNI file type. */
            case FileBase.AFNI:
                fileNameList = new Vector();

                String headFileName = fileInfo.getFileName();
                String brikFileName = headFileName.replaceFirst(".HEAD", ".BRIK");
                fileNameList.add(headFileName);
                fileNameList.add(brikFileName);

                return fileNameList;

            /** FITS file type. */
            case FileBase.FITS:
                return null;

            /** MetaMorph Stack (STK) file type. */
            case FileBase.STK:
                return null;

            /** Siemens MAGNETOM VISION */
            case FileBase.MAGNETOM_VISION:
                return null;

            /** GE Genesis 5X and LX */
            case FileBase.GE_GENESIS:
                return null;

            /** MRC file format used by IMOD */
            case FileBase.MRC:
                return null;

            /** Interfile file format used in Nuclear Medicine */
            case FileBase.INTERFILE:
                return null;

            /** Micro CT format for small animal imaging */
            case FileBase.MICRO_CAT:
                return null;

            /** RAW MULTIFLE image data, no header. */
            case FileBase.RAW_MULTIFILE:
                return null;

            /** Used by the Zeiss LSM 510 Dataserver */
            case FileBase.LSM:
                return null;

            /** Used by the Zeiss LSM 510 Dataserver */
            case FileBase.LSM_MULTIFILE:
                return null;

            /** Used by the Bio-Rad Pic format */
            case FileBase.BIORAD:
                return null;

            /** Used by FreeSurfer software */
            case FileBase.COR:
                return null;

            /** Bruker file format */
            case FileBase.BRUKER:
                return null;

            /** MIPAV XML file format */
            case FileBase.XML:
                fileNameList = new Vector();

                String rawFileName = fileInfo.getFileName();
                String xmlFileName = rawFileName.replaceFirst(".raw", ".xml");
                fileNameList.add(xmlFileName);
                fileNameList.add(rawFileName);

                return fileNameList;

            /** MIPAV XML file format */
            case FileBase.XML_MULTIFILE:
                return null;

            /** SPM file format */
            case FileBase.SPM:
                return null;

            /** MIPAV project format */
            case FileBase.PROJECT:
                return null;

            /** NIFTI multi-file format */
            case FileBase.NIFTI_MULTIFILE:
                return null;

            /* Image Cytometry Standard */
            case FileBase.ICS:
                return null;

            /* Optical coherence tomography */
            case FileBase.TMG:
                return null;

            /* Washington University OSM dataset structure */
            case FileBase.OSM:
                return null;

            /** MIPAV Surface XML file format */
            case FileBase.SURFACE_XML:
                return null;

            /** Gatan's Digital Micrograph version 3 file format */
            case FileBase.DM3: /** Image modality unknown. */
                return null;
        }

        return null;
    }

    /**
     * Tests whether this uri is the local file uri.
     *
     * @param   uri  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public static boolean isFileURI(URI uri) {

        if (uri.getScheme().toLowerCase().equals("file")) {
            return true;
        }

        return false;
    }

    /**
     * Tests whether this uri is the SRB uri.
     *
     * @param   uri  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public static boolean isSRBURI(URI uri) {

        if (uri.getScheme().toLowerCase().equals("srb")) {
            return true;
        }

        return false;
    }

    /**
     * Copied from edu.sdsc.grid.io.srb.SRBFile.
     *
     * @param   uri  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     *
     * @throws  IOException            DOCUMENT ME!
     * @throws  MalformedURLException  DOCUMENT ME!
     * @throws  ConnectException       DOCUMENT ME!
     */
    public static SRBAccount uriInitialAccount(URI uri) throws IOException {

        //
        // I guess eventually, move all this to some Handler.java for SRB URIs.
        //
        String host = uri.getHost();
        int port = uri.getPort();
        String path = uri.getPath();
        String userInfo = uri.getUserInfo();
        String userName = null, mdasDomain = null, password = null;
        String homeDirectory = null;
        int index = -1;

        if ((userInfo == null) || (userInfo == "")) {

            // anon. login
            userName = "public";
            mdasDomain = "npaci";
            password = "CANDO";
            homeDirectory = "/home/public.npaci/";
        } else {
            index = userInfo.indexOf(".");

            if (index < 0) {
                throw new MalformedURLException();
            }

            userName = userInfo.substring(0, index);

            if (index < 0) {
                throw new MalformedURLException();
            }

            userInfo = userInfo.substring(index + 1);

            index = userInfo.indexOf(":");

            if (index > 0) {
                mdasDomain = userInfo.substring(0, index);
                password = userInfo.substring(index + 1);
            } else {
                mdasDomain = userInfo;
            }

            // set the home directory to the local zone
            homeDirectory = "/home/" + userName + "." + mdasDomain;
        }

        index = host.indexOf(".");

        if (index < 0) {

            // use zone authority
            URL url = new URL(ZONE_AUTHORITY + host);
            InputStream in = url.openConnection().getInputStream();

            // not the best, doesn't use the xml.
            int index2 = -1;
            String result = null;
            byte[] data = new byte[1000];

            in.read(data);
            result = new String(data);

            // ? first read always stops at the 26th byte
            in.read(data);
            result += new String(data);

            index = result.indexOf("ns1:server");
            index2 = result.indexOf("/ns1:server", index + 11);

            if ((index < 0) || (index2 < 0)) {
                throw new ConnectException("Invalid zone name.");
            }

            host = result.substring(index + 11, index2 - 1);

            index = result.indexOf("ns1:port");
            index2 = result.indexOf("/ns1:port", index + 9);
            result = result.substring(index + 9, index2 - 1);

            if ((result != null) && (result.length() > 0)) {
                port = Integer.parseInt(result);
            }
        }

        if (port < 0) {
            port = 5544;
        }

        // Have to find a storage resource after connection.
        return new SRBAccount(host, port, userName, password, homeDirectory, mdasDomain, "");
    }

    /**
     * DOCUMENT ME!
     *
     * @return      DOCUMENT ME!
     *
     * @inheritDoc  DOCUMENT ME!
     */
    public boolean canReadImages() {
        return true;
    }

    /**
     * DOCUMENT ME!
     *
     * @return      DOCUMENT ME!
     *
     * @inheritDoc  DOCUMENT ME!
     */
    public boolean canWriteImages() {
        return true;
    }

    /**
     * Copies the selected srb file and relevant files to the local temporary directory.
     *
     * @param   srbFile  the selected srb file
     *
     * @return  the local file list which was copied from the srb server.
     */
    public Vector copyFilesFromSRB(SRBFile srbFile) {

        if (srbFile == null) {
            return null;
        }

        String extension = getExtension(srbFile);
        Vector srbFileList = new Vector();

        if (extension.equalsIgnoreCase("")) {
            // TODO: maybe assume multifile? get all in sequence?
        } else if (extension.equalsIgnoreCase(".xml")) {
            SRBFile f = new SRBFile((SRBFile) srbFile.getParentFile(),
                                    srbFile.getName().replaceFirst("\\.xml$", ".raw"));
            srbFileList.add(f);
        } else if (extension.equalsIgnoreCase(".img")) {
            SRBFile f = new SRBFile((SRBFile) srbFile.getParentFile(),
                                    srbFile.getName().replaceFirst("\\.img$", ".hdr"));
            srbFileList.add(f);
        }

        srbFileList.add(srbFile);

        GeneralFile localTempFile = null;
        Vector localFileList = new Vector();

        try {
            GeneralFile localTempDir = edu.sdsc.grid.io.FileFactory.newFile(new URI(srbTempDir));

            if (!localTempDir.exists()) {
                localTempDir.mkdirs();
            }

            for (int i = 0; i < srbFileList.size(); i++) {
                SRBFile f = (SRBFile) srbFileList.elementAt(i);

                if (!f.isFile()) {

                    // TODO handle retrieval of directories from srb
                    MipavUtil.displayError(f.toString() + " is not a file and cannot be copied locally.");

                    return null;
                }

                localTempFile = new LocalFile(new File(localTempDir.getPath() + File.separator + f.getName()));

                if (!localTempFile.exists()) {
                    f.copyTo(localTempFile, true);
                    localFileList.add(localTempFile);
                }
            }
        } catch (IOException ioe) {
            ioe.printStackTrace(System.err);
            MipavUtil.displayError("Error encountered copying file: " + ioe.getMessage());

            return null;
        } catch (URISyntaxException urie) {
            urie.printStackTrace(System.err);
            MipavUtil.displayError("SRB temp dir URI is malformed: " + urie.getMessage());

            return null;
        }

        // The local temporary file will deleted when MIPAV exits.
        for (int i = 0; i < localFileList.size(); i++) {
            ((LocalFile) localFileList.get(i)).deleteOnExit();
        }

        return localFileList;
    }

    /**
     * Creates the temporary local file which has the same file name as <code>fileName</code>.
     *
     * @param   parent    the parent directory.
     * @param   fileName  the file name which will be created under the parent directory.
     *
     * @return  the new local file.
     */
    public LocalFile createLocalTempFile(File parent, String fileName) {
        return new LocalFile(new File(parent, fileName));
    }

    /**
     * Creates the srb file which has the same file name as <code>fileName</code>.
     *
     * @param   srbFile   the parent directory.
     * @param   fileName  the file name which will be created under the parent directory.
     *
     * @return  the new srb file.
     */
    public SRBFile createSRBFile(SRBFile srbFile, String fileName) {
        return new SRBFile(srbFile, fileName);
    }

    /**
     * @see  InheritDoc
     */
    public void insertScriptLine(AlgorithmBase algo) { }

    /**
     * DOCUMENT ME!
     *
     * @param       ext  DOCUMENT ME!
     *
     * @return      DOCUMENT ME!
     *
     * @inheritDoc  DOCUMENT ME!
     */
    public boolean isExtensionSupported(String ext) {
        return true;
    }

    /**
     * DOCUMENT ME!
     *
     * @inheritDoc  DOCUMENT ME!
     */
    public void readImage() {

        /**
         * If the srbFileSystem was not set up, then uses the JDialogLoginSRB dialog to retrieve information to
         * construct SRBFileSystem instance.
         */
        if (!JDialogLoginSRB.hasValidSRBFileSystem()) {
            new JDialogLoginSRB("Connect to");
        }

        if (!JDialogLoginSRB.hasValidSRBFileSystem()) {
            return;
        }

        /**
         * Uses the JargonFileChooser to retrieve the file that the user wants to open.
         */
        JargonFileChooser chooser = null;

        try {
            chooser = new JargonFileChooser(JDialogLoginSRB.srbFileSystem);
        } catch (OutOfMemoryError e) {
            e.printStackTrace(System.err);
            MipavUtil.displayError("Out of memory!");

            return;
        } catch (IOException e) {
            e.printStackTrace(System.err);
            MipavUtil.displayError(e.getMessage());

            return;
        }

        chooser.setFileSelectionMode(JFileChooser.FILES_AND_DIRECTORIES);
        chooser.setMultiSelectionEnabled(true);

        int returnValue = chooser.showDialog(ViewUserInterface.getReference().getMainFrame(), "Open");

        if (returnValue == JargonFileChooser.APPROVE_OPTION) {

            /**
             * According to the files selected by user, tries to create the srb file list.
             */
            SRBFile[] files = chooser.getSelectedFiles();
            Vector sourceFileList = null;

            if (files.length == 1) {
                sourceFileList = getFileList(files[0]);
            } else {
                sourceFileList = new Vector();

                for (int i = 0; i < files.length; i++) {
                    sourceFileList.add(files[i]);
                }
            }

            /**
             * Gets the local temporary diretory, if it doesn't exist, then it will be created.
             */
            LocalFile localTempDir = null;

            try {
                localTempDir = (LocalFile) edu.sdsc.grid.io.FileFactory.newFile(new URI(LOCAL_TEMP_DIR));

                if (!localTempDir.exists()) {
                    localTempDir.mkdirs();
                }
            } catch (IOException e) {
                e.printStackTrace(System.err);
                MipavUtil.displayError("I/O Errors encountered creating LocalFile: " + LOCAL_TEMP_DIR);
            } catch (URISyntaxException e) {

                // Should not happen.
                e.printStackTrace(System.err);
            }

            /**
             * Creates the target file list.
             */

            Vector targetFileList = createLocalFileList(localTempDir, sourceFileList);

            /**
             * After MIPAV exits, the local temporary files will be deleted.
             */
            for (int i = 0; i < targetFileList.size(); i++) {
                ((LocalFile) targetFileList.get(i)).deleteOnExit();
            }

            FileTransferSRB fileTransfer = new FileTransferSRB(sourceFileList, targetFileList);
            fileTransfer.setThreadSeperated(false);
            fileTransfer.run();

            boolean multiFile = false;

            if (targetFileList.size() > 1) {
                multiFile = true;
            }

            ViewUserInterface.getReference().openImageFrame(((LocalFile) targetFileList.get(0)).getPath(), multiFile);

        }
    }

    /**
     * Reads the image data from srb server and open the image.
     *
     * @param  srbFile  DOCUMENT ME!
     */
    public void readImageFromSRBFile(SRBFile srbFile) {

        if (srbFile == null) {
            return;
        }

        String extension = getExtension(srbFile);
        Vector srbFileList = new Vector();

        if (extension.equalsIgnoreCase("")) {
            // TODO: maybe assume multifile? get all in sequence?
        } else if (extension.equalsIgnoreCase(".xml")) {
            SRBFile f = new SRBFile((SRBFile) srbFile.getParentFile(),
                                    srbFile.getName().replaceFirst("\\.xml$", ".raw"));
            srbFileList.add(f);
        } else if (extension.equalsIgnoreCase(".img")) {
            SRBFile f = new SRBFile((SRBFile) srbFile.getParentFile(),
                                    srbFile.getName().replaceFirst("\\.img$", ".hdr"));
            srbFileList.add(f);
        }

        srbFileList.add(srbFile);

        GeneralFile localTempFile = null;
        Vector localFileList = new Vector();

        try {
            GeneralFile localTempDir = edu.sdsc.grid.io.FileFactory.newFile(new URI(srbTempDir));

            if (!localTempDir.exists()) {
                localTempDir.mkdirs();
            }

            for (int i = 0; i < srbFileList.size(); i++) {
                SRBFile f = (SRBFile) srbFileList.elementAt(i);

                if (!f.isFile()) {

                    // TODO handle retrieval of directories from srb
                    MipavUtil.displayError(f.toString() + " is not a file and cannot be copied locally.");

                    return;
                }

                localTempFile = new LocalFile(new File(localTempDir.getPath() + File.separator + f.getName()));

                if (!localTempFile.exists()) {
                    f.copyTo(localTempFile, true);
                    localFileList.add(localTempFile);
                }
            }
        } catch (IOException ioe) {
            ioe.printStackTrace(System.err);
            MipavUtil.displayError("Error encountered copying file: " + ioe.getMessage());

            return;
        } catch (URISyntaxException urie) {
            urie.printStackTrace(System.err);
            MipavUtil.displayError("SRB temp dir URI is malformed: " + urie.getMessage());

            return;
        }

        // Open the local file and diplay the image.
        ViewUserInterface.getReference().openImageFrame(localTempFile.getPath());

        // The local temporary file will deleted when MIPAV exits.
        for (int i = 0; i < localFileList.size(); i++) {
            ((LocalFile) localFileList.get(i)).deleteOnExit();
        }

    }

    /**
     * Retrieves an image located at an URI, saves it in a local temp directory, and opens the image with MIPAV.
     *
     * @param  uri  the URI of the image to copy locally and open
     */
    public void readImageFromURI(URI uri) {

        if (uri == null) {
            return;
        }

        if (isSRBURI(uri)) {

            Vector fileList = new Vector();
            fileList.add(uri);

            String extension = getExtension(uri);

            try {

                if (extension.equalsIgnoreCase("")) {
                    // TODO: maybe assume multifile? get all in sequence?
                } else if (extension.equalsIgnoreCase(".xml")) {
                    fileList.add(new URI(uri.toASCIIString().replaceFirst("\\.xml$", ".raw")));
                } else if (extension.equalsIgnoreCase(".img")) {
                    fileList.add(new URI(uri.toASCIIString().replaceFirst("\\.img$", ".hdr")));
                }
            } catch (URISyntaxException urie) {
                urie.printStackTrace(System.err);
                MipavUtil.displayError("URI is malformed: " + urie.getMessage());

                return;
            }

            /**
             * If there is no srb file system information, then try to use the uri to create the srb file system. If the
             * srb file system can't be created by uri, then use the srb server login dialog to get the login
             * information.
             */
            if ((srbFileSystem == null) && (JDialogLoginSRB.srbFileSystem == null)) {
                srbFileSystem = createSRBFileSystem(uri);
            }


            /**
             * Copies the srb files to the local temporary directory.
             */
            GeneralFile firstLocalTempFile = null;
            GeneralFile localTempFile = null;

            try {
                GeneralFile localTempDir = edu.sdsc.grid.io.FileFactory.newFile(new URI(srbTempDir));

                if (!localTempDir.exists()) {
                    localTempDir.mkdirs();
                }

                for (int i = 0; i < fileList.size(); i++) {
                    SRBFile srbFile = (SRBFile) fileList.elementAt(i);

                    if (!srbFile.isFile()) {

                        // TODO handle retrieval of directories from srb
                        MipavUtil.displayError(uri.toASCIIString() + " is not a file and cannot be copied locally.");

                        return;
                    }

                    localTempFile = new LocalFile(new File(localTempDir.getPath() + File.separator +
                                                           srbFile.getName()));

                    if (i == 0) {
                        firstLocalTempFile = localTempFile;
                    }

                    /*
                     * if (localTempFile.exists()) { if (Preferences.is(Preferences.PREF_SAVE_PROMPT_OVERWRITE)) { int
                     * response = JOptionPane.showConfirmDialog(uriDialog, localTempFile + " exists. Overwrite?", "File
                     * exists", JOptionPane.YES_NO_OPTION); if (response == JOptionPane.NO_OPTION) { continue; } } } //
                     * force overwriting of temp files file.copyTo(localTempFile, true);
                     */
                    if (!localTempFile.exists()) {
                        srbFile.copyTo(localTempFile, true);
                    }
                }
            } catch (IOException ioe) {
                ioe.printStackTrace(System.err);
                MipavUtil.displayError("Error encountered copying file: " + ioe.getMessage());

                return;
            } catch (URISyntaxException urie) {
                urie.printStackTrace(System.err);
                MipavUtil.displayError("SRB temp dir URI is malformed: " + urie.getMessage());

                return;
            }

            ViewUserInterface.getReference().openImageFrame(firstLocalTempFile.getPath());

            // The local temporary file will deleted when MIPAV exits.
            localTempFile.deleteOnExit();
            firstLocalTempFile.deleteOnExit();
        } else if (isFileURI(uri)) {

            try {
                LocalFile localFile = (LocalFile) edu.sdsc.grid.io.FileFactory.newFile(uri);
                ViewUserInterface.getReference().openImageFrame(localFile.getPath());
            } catch (IOException e) {
                e.printStackTrace(System.err);
                MipavUtil.displayError("Error encountered opening file: " + e.getMessage());

                return;
            }
        }

    }

    /**
     * @see  inheritDoc
     */
    public void scriptRun(AlgorithmScriptParser parser) throws IllegalArgumentException { }

    /**
     * Transfers the images between the source URI and the destination URI, which could be local or SRB file.
     *
     * @param  sourceURI       the source URI.
     * @param  destinationURI  the destinationURI.
     */
    public void transferImageBetweenURI(URI sourceURI, URI destinationURI) {
        Vector fileList = new Vector();
        fileList.add(sourceURI);

        String extension = getExtension(sourceURI);

        try {

            if (extension.equalsIgnoreCase("")) {
                // TODO: maybe assume multifile? get all in sequence?
            } else if (extension.equalsIgnoreCase(".xml")) {
                fileList.add(new URI(sourceURI.toASCIIString().replaceFirst("\\.xml$", ".raw")));
            } else if (extension.equalsIgnoreCase(".img")) {
                fileList.add(new URI(sourceURI.toASCIIString().replaceFirst("\\.img$", ".hdr")));
            }
        } catch (URISyntaxException urie) {
            urie.printStackTrace(System.err);
            MipavUtil.displayError("URI is malformed: " + urie.getMessage());

            return;
        }

        try {

            for (int i = 0; i < fileList.size(); i++) {
                GeneralFile sourceFile = null;

                if (isFileURI(sourceURI)) {
                    sourceFile = edu.sdsc.grid.io.FileFactory.newFile((URI) fileList.elementAt(i));
                } else {
                    sourceFile = new SRBFile(srbFileSystem, ((URI) fileList.elementAt(i)).getPath());
                }

                if (!sourceFile.isFile()) {

                    // TODO handle retrieval of directories from srb
                    MipavUtil.displayError(sourceURI.toASCIIString() +
                                           " is not a file and cannot be copied locally or remotely.");

                    return;
                }

                GeneralFile destinationDirectory = null;

                if (isFileURI(destinationURI)) {
                    destinationDirectory = edu.sdsc.grid.io.FileFactory.newFile(destinationURI);
                } else {
                    destinationDirectory = new SRBFile(srbFileSystem, destinationURI.getPath());
                }

                if (destinationDirectory.isFile()) {
                    destinationDirectory = destinationDirectory.getParentFile();
                }

                GeneralFile destinationFile = null;

                if (isFileURI(destinationURI)) {
                    destinationFile = new LocalFile(getLocalPathFromURI(destinationDirectory.toURI()) + "\\" +
                                                    sourceFile.getName());
                } else {
                    destinationFile = new SRBFile(srbFileSystem,
                                                  destinationDirectory.getPath() + "/" + sourceFile.getName());
                }

                FileTransferSRB.copy(sourceFile, destinationFile);
            }
        } catch (IOException ioe) {
            ioe.printStackTrace(System.err);
            MipavUtil.displayError("Error encountered copying file: " + ioe.getMessage());

            return;
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param       image  DOCUMENT ME!
     *
     * @inheritDoc  DOCUMENT ME!
     */
    public void writeImage(ModelImage image) {

        if (!JDialogLoginSRB.hasValidSRBFileSystem()) {
            new JDialogLoginSRB("Connect to");
        }

        if (JDialogLoginSRB.srbFileSystem == null) {
            return;
        }

        try {
            JargonFileChooser chooser = new JargonFileChooser(JDialogLoginSRB.srbFileSystem);
            chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);

            int returnValue = chooser.showDialog(ViewUserInterface.getReference().getMainFrame(), "Save");

            if (returnValue == JargonFileChooser.APPROVE_OPTION) {
                SRBFile f = chooser.getSelectedFile();
                writeImageToSRBFile(f);
            }
        } catch (OutOfMemoryError e) {
            MipavUtil.displayError("Out of memory!");
        } catch (IOException e) {
            MipavUtil.displayError(e.getMessage());
        }
    }

    /**
     * Writes the current opened images to the srb file.
     *
     * @param  srbFile  the srb file.
     */
    public void writeImageToSRBFile(SRBFile srbFile) {

        /**
         * Gets the active ViewJFrameImage instance.
         */
        ViewJFrameImage currentImageFrame = ViewUserInterface.getReference().getActiveImageFrame();

        /**
         * Gets the current image file opened inside the active ViewJFrameImage.
         */
        if (currentImageFrame == null) {
            return;
        }

        ModelImage currentImage = currentImageFrame.getActiveImage();

        if (currentImage == null) {
            return;
        }

        /**
         * Gets the file informations for the current opened images.
         */
        FileInfoBase[] currentFileInfoList = currentImage.getFileInfo();

        if (currentFileInfoList == null) {
            return;
        }

        /**
         * Gets the local temporary diretory, if it doesn't exist, then it will be created.
         */
        LocalFile localTempDir = null;

        try {
            localTempDir = (LocalFile) edu.sdsc.grid.io.FileFactory.newFile(new URI(LOCAL_TEMP_DIR));

            if (!localTempDir.exists()) {
                localTempDir.mkdirs();
            }
        } catch (IOException e) {
            e.printStackTrace(System.err);
            MipavUtil.displayError("I/O Errors encountered creating LocalFile: " + LOCAL_TEMP_DIR);
        } catch (URISyntaxException e) {

            // Should not happen.
            e.printStackTrace(System.err);
        }

        /**
         * Saves the current directory for recovery at the end of function.
         *
         * The idea is try to use the save function save the files to different directory.
         */
        String savedDir = currentFileInfoList[0].getFileDirectory();

        /**
         * Sets the new directory which we want the files saved to.
         */
        for (int i = 0; i < currentFileInfoList.length; i++) {
            currentFileInfoList[i].setFileDirectory(localTempDir.getPath() + "\\");
        }

        /**
         * Creates the local temporary file list.
         */
        Vector sourceFileList = createLocalFileList(localTempDir, getFileNameList(currentFileInfoList));

        /**
         * Constructs the FileWriteOptions to prepare the file name for save.
         */
        FileWriteOptions opts = new FileWriteOptions(((LocalFile) sourceFileList.get(0)).getName(),
                                                     localTempDir.getPath() + "//", false);


        if (currentImage.getNDims() == 3) {
            opts.setBeginSlice(0);
            opts.setEndSlice(currentImage.getExtents()[2] - 1);
        }

        opts.setOptionsSet(true);

        /**
         * Saves the opened images to the local temporary directory.
         */
        currentImageFrame.saveSRB(opts, -1);

        /**
         * Recovers the original directory which these files belong to.
         */
        for (int i = 0; i < currentFileInfoList.length; i++) {
            currentFileInfoList[i].setFileDirectory(savedDir);
        }

        /**
         * Copies the local temporary files to the directory of the SRB server.
         */
        Vector targetFileList = new Vector();

        for (int i = 0; i < sourceFileList.size(); i++) {
            LocalFile lf = (LocalFile) sourceFileList.get(i);
            targetFileList.add(createSRBFile(srbFile, lf.getName()));
            lf.deleteOnExit();
        }

        FileTransferSRB fileTransferThread = new FileTransferSRB(sourceFileList, targetFileList);
        fileTransferThread.start();
    }

    /**
     * DOCUMENT ME!
     *
     * @param   srbFile  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private static String getExtension(SRBFile srbFile) {

        if (srbFile == null) {
            return null;
        }

        return getExtension(srbFile.getName());

    }

    /**
     * DOCUMENT ME!
     *
     * @param   fileName  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private static String getExtension(String fileName) {
        int index = fileName.lastIndexOf(".");

        if (index == -1) {
            return new String("");
        }

        return fileName.substring(index);
    }

    /**
     * DOCUMENT ME!
     *
     * @param   uri  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private static String getExtension(URI uri) {

        if (uri == null) {
            return null;
        }

        return getExtension(uri.toString());
    }

    /**
     * DOCUMENT ME!
     *
     * @param   uri  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private String getLocalPathFromURI(URI uri) {
        return uri.getPath().substring(1);
    }


    /**
     * DOCUMENT ME!
     *
     * @param   uri  DOCUMENT ME!
     * @param   use  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private URI getURI(String uri, int use) {

        if (uri.startsWith("file:/")) {

            // local browser
            try {
                JFileChooser chooser = new JFileChooser();
                chooser.setFont(MipavUtil.defaultMenuFont);

                Dimension d = new Dimension(700, 400);
                chooser.setMinimumSize(d);
                chooser.setPreferredSize(d);

                MipavUtil.setFonts(chooser.getComponents());

                chooser.setLocation(new Point(ViewUserInterface.getReference().getNewFrameLocation().width,
                                              ViewUserInterface.getReference().getNewFrameLocation().height));

                // TODO default to current dir in uri field

                if (ViewUserInterface.getReference().getDefaultDirectory() != null) {
                    File file = new File(ViewUserInterface.getReference().getDefaultDirectory());

                    if (file != null) { }
                    else {
                        file = new File(System.getProperty("user.dir"));
                    }

                    LocalFile lf = new LocalFile(file);
                    chooser.setCurrentDirectory(file);
                } else {
                    chooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
                }

                if (use == SOURCE) {
                    chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
                } else {
                    chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
                }

                chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.GEN));
                chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.MICROSCOPY));
                chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.MISC));
                chooser.setFileFilter(new ViewImageFileFilter(ViewImageFileFilter.TECH));

                chooser.setDialogTitle("Select Image");

                int returnValue = chooser.showOpenDialog(ViewUserInterface.getReference().getMainFrame());

                if (returnValue == JFileChooser.APPROVE_OPTION) {
                    return chooser.getSelectedFile().toURI();
                } else {
                    return (URI) null;
                }
            } catch (OutOfMemoryError e) {
                MipavUtil.displayError("Out of memory!");

                return (URI) null;
            }
        } else if (uri.startsWith("srb://")) {

            // srb browser
            try {
                JargonFileChooser chooser = null;

                if (uri.equals("srb://")) {
                    chooser = new JargonFileChooser((SRBAccount) srbFileSystem.getAccount(), "/");

                } else {
                    URI uri2 = new URI(uri);
                    chooser = new JargonFileChooser((SRBAccount) srbFileSystem.getAccount(), uri2.getPath());
                }

                chooser.setFileSelectionMode(JFileChooser.FILES_AND_DIRECTORIES);

                int returnValue = chooser.showDialog(ViewUserInterface.getReference().getMainFrame(), null);

                if (returnValue == JargonFileChooser.APPROVE_OPTION) {
                    SRBFile f = chooser.getSelectedFile();

                    if (f != null) {
                        srbFileSystem = (SRBFileSystem) f.getFileSystem();
                    }
                }
            } catch (OutOfMemoryError e) {
                MipavUtil.displayError("Out of memory!");

                return (URI) null;
            } catch (IOException e) {
                MipavUtil.displayError(e.getMessage());

                return (URI) null;
            } catch (URISyntaxException urie) {
                urie.printStackTrace(System.err);
                MipavUtil.displayError("URI is malformed: " + urie.getMessage());

                return (URI) null;
            }
        } else {
            MipavUtil.displayError("URI is malformed: " + uri);

            return (URI) null;
        }

        return (URI) null;
    }

    //~ Inner Classes --------------------------------------------------------------------------------------------------

    /**
     * DOCUMENT ME!
     */
    private class JDialogGetURI extends JDialog implements ActionListener {

        /** DOCUMENT ME! */
        private String defaultDialogTitle;

        /** DOCUMENT ME! */
        private JTextField destinationURIField = null;

        /** Indicates the dialog type. */
        private int dialogType = OPEN_DIALOG;

        /** Instance variables. */
        private JTextField sourceURIField = null;

        /**
         * Creates a new JDialogGetURI object.
         */
        public JDialogGetURI() {
            this(OPEN_DIALOG);
        }

        /**
         * Creates a new JDialogGetURI object.
         *
         * @param  dialogType  DOCUMENT ME!
         */
        public JDialogGetURI(int dialogType) {
            this(dialogType, null);
        }

        /**
         * Creates a new JDialogGetURI object.
         *
         * @param  dialogTitle  DOCUMENT ME!
         */
        public JDialogGetURI(String dialogTitle) {
            this(OPEN_DIALOG, dialogTitle);
        }

        /**
         * Set up the URI input GUI.
         *
         * @param  dialogType   DOCUMENT ME!
         * @param  dialogTitle  DOCUMENT ME!
         */
        public JDialogGetURI(int dialogType, String dialogTitle) {
            super(ViewUserInterface.getReference().getMainFrame(), false);

            /**
             * If the dialog type is not supported, the OPEN_DIALOG type will be used.
             */
            if ((dialogType != OPEN_DIALOG) || (dialogType != SAVE_DIALOG) || (dialogType != TRANSFER_DIALOG)) {
                dialogType = OPEN_DIALOG;
            }

            this.dialogType = dialogType;

            PanelManager manager = new PanelManager();
            manager.getConstraints().insets = new Insets(3, 3, 3, 3);

            if (dialogType == OPEN_DIALOG) {
                manager.add(WidgetFactory.buildLabel("Enter Source URI "));
                sourceURIField = WidgetFactory.buildTextField("srb://");
                sourceURIField.setColumns(40);
                manager.add(sourceURIField);
                manager.add(WidgetFactory.buildTextButton("Browse",
                                                          "Browse srb or local filesystem (determined by which is in URI field).",
                                                          "sourceBrowse", this));
                manager.add(WidgetFactory.buildTextButton("Open", "Get file indicated by URI", "Open", this));
            } else if (dialogType == SAVE_DIALOG) {
                manager.add(WidgetFactory.buildLabel("Enter Destination URI "));
                sourceURIField = WidgetFactory.buildTextField("srb://");
                sourceURIField.setColumns(40);
                manager.add(sourceURIField);
                manager.add(WidgetFactory.buildTextButton("Browse",
                                                          "Browse srb or local filesystem (determined by which is in URI field).",
                                                          "destinationBrowse", this));
                manager.add(WidgetFactory.buildTextButton("Save", "Save the active imagefile indicated by URI", "Save",
                                                          this));
            } else {
                manager.add(WidgetFactory.buildLabel("Enter Source URI "));
                sourceURIField = WidgetFactory.buildTextField("srb://");
                sourceURIField.setColumns(40);
                manager.add(sourceURIField);
                manager.add(WidgetFactory.buildTextButton("Browse",
                                                          "Browse srb or local filesystem (determined by which is in URI field).",
                                                          "sourceBrowse", this));
                manager.addOnNextLine(WidgetFactory.buildLabel("Enter Destination URI "));
                destinationURIField = WidgetFactory.buildTextField("srb://");
                destinationURIField.setColumns(40);
                manager.add(destinationURIField);
                manager.add(WidgetFactory.buildTextButton("Browse",
                                                          "Browse srb or local filesystem (determined by which is in URI field).",
                                                          "destinationBrowse", this));
                manager.add(WidgetFactory.buildTextButton("Transfer", "Push file from source URI to destination URI",
                                                          "Transfer", this));
            }

            if (dialogTitle == null) {
                dialogTitle = getDefaultDialogTitle(dialogType);
            }

            this.setTitle(dialogTitle);

            getContentPane().add(manager.getPanel());
            pack();
            setVisible(true);
            MipavUtil.centerOnScreen(this);
        }

        /**
         * DOCUMENT ME!
         *
         * @param       event  DOCUMENT ME!
         *
         * @inheritDoc  DOCUMENT ME!
         */
        public void actionPerformed(ActionEvent event) {
            String command = event.getActionCommand();

            if (command.equals("Open")) {
                readImageFromURI(getSourceURI());
                dispose();
            } else if (command.equals("sourceBrowse")) {
                URI sourceURI = getURI(sourceURIField.getText(), SOURCE);
                setSourceURI(sourceURI);
            } else if (command.equals("destinationBrowse")) {
                URI destinationURI = getURI(destinationURIField.getText(), SOURCE);
                setSourceURI(destinationURI);
            }
        }

        /**
         * DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        public URI getDestinationURI() {

            try {
                return new URI(destinationURIField.getText());
            } catch (URISyntaxException urie) {
                urie.printStackTrace(System.err);
                MipavUtil.displayError("URI is malformed: " + urie.getMessage());

                return null;
            }
        }

        /**
         * Return the current uri that is in the uri text field.
         *
         * @return  the current uri in the uri text field, or null if it is malformed
         */
        public URI getSourceURI() {

            try {
                return new URI(sourceURIField.getText());
            } catch (URISyntaxException urie) {
                urie.printStackTrace(System.err);
                MipavUtil.displayError("SRB URI is malformed: " + urie.getMessage());

                return null;
            }
        }

        /**
         * DOCUMENT ME!
         *
         * @param  uri  DOCUMENT ME!
         */
        public void setDestinationURI(URI uri) {

            if (uri == null) {
                return;
            }

            String strURI = uri.toString();
            int index = strURI.indexOf("?");

            if (index >= 0) {
                strURI = strURI.substring(0, strURI.indexOf("?"));
            }

            destinationURIField.setText(strURI);
        }

        /**
         * Changes the URI displayed in the URI field.
         *
         * @param  uri  the url to put into the URI text field
         */
        public void setSourceURI(URI uri) {

            if (uri == null) {
                return;
            }

            String strURI = uri.toString();
            int index = strURI.indexOf("?");

            if (index >= 0) {
                strURI = strURI.substring(0, index);
            }

            sourceURIField.setText(strURI);
        }

        /**
         * DOCUMENT ME!
         *
         * @param   dialogType  DOCUMENT ME!
         *
         * @return  DOCUMENT ME!
         */
        private String getDefaultDialogTitle(int dialogType) {
            String title = null;

            if (dialogType == OPEN_DIALOG) {
                title = "Open File";
            } else if (dialogType == SAVE_DIALOG) {
                title = "Save File";
            } else if (dialogType == TRANSFER_DIALOG) {
                title = "Transfer File";
            } else {
                dialogType = OPEN_DIALOG;
                title = "Open File";
            }

            return title;
        }
    }
}
