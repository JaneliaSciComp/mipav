package gov.nih.mipav.model.file;


import java.util.Enumeration;
import java.util.Hashtable;
import java.util.Vector;


public class FileTypeTable {
    private static Hashtable<Integer, FileTypeInfo> FILE_TYPE_TABLE;

    private static Hashtable<String, Integer> FILE_EXT_TABLE;

    static {
        FILE_TYPE_TABLE = new Hashtable<Integer, FileTypeInfo>(100);
        FILE_EXT_TABLE = new Hashtable<String, Integer>(200);

        FILE_TYPE_TABLE.put(FileUtility.UNDEFINED, new FileTypeInfo("Undefined", ""));

        FILE_TYPE_TABLE.put(FileUtility.AFNI, new FileTypeInfo("AFNI", ".afni,.head,.brik"));
        FILE_EXT_TABLE.put(".afni", FileUtility.AFNI);
        FILE_EXT_TABLE.put(".head", FileUtility.AFNI);
        FILE_EXT_TABLE.put(".brik", FileUtility.AFNI);
        FILE_EXT_TABLE.put(".AFNI", FileUtility.AFNI);
        FILE_EXT_TABLE.put(".HEAD", FileUtility.AFNI);
        FILE_EXT_TABLE.put(".BRIK", FileUtility.AFNI);

        FILE_TYPE_TABLE.put(FileUtility.ANALYZE, new FileTypeInfo("Analyze", ".img"));
        FILE_TYPE_TABLE.put(FileUtility.ANALYZE_MULTIFILE, new FileTypeInfo("Analyze multifile", ".img"));

        // analyze, nifti and interfile use .img
        FILE_EXT_TABLE.put(".img", FileUtility.UNDEFINED);
        FILE_EXT_TABLE.put(".IMG", FileUtility.UNDEFINED);

        FILE_TYPE_TABLE.put(FileUtility.AVI, new FileTypeInfo("Avi", ".avi"));
        FILE_EXT_TABLE.put(".avi", FileUtility.AVI);
        FILE_EXT_TABLE.put(".AVI", FileUtility.AVI);

        FILE_TYPE_TABLE.put(FileUtility.BIORAD, new FileTypeInfo("Bio-Rad", ".pic"));

        // both Bio-rad and JIMI use .pic
        FILE_EXT_TABLE.put(".pic", FileUtility.UNDEFINED);
        FILE_EXT_TABLE.put(".PIC", FileUtility.UNDEFINED);

        FILE_TYPE_TABLE.put(FileUtility.BMP, new FileTypeInfo("BMP", ".bmp"));
        FILE_EXT_TABLE.put(".bmp", FileUtility.BMP);
        FILE_EXT_TABLE.put(".BMP", FileUtility.BMP);

        FILE_TYPE_TABLE.put(FileUtility.BRUKER, new FileTypeInfo("BRUKER", ".2dseq"));

        FILE_TYPE_TABLE.put(FileUtility.CHESHIRE, new FileTypeInfo("Chesire", ".imc"));
        FILE_EXT_TABLE.put(".imc", FileUtility.CHESHIRE);
        FILE_EXT_TABLE.put(".IMC", FileUtility.CHESHIRE);

        FILE_TYPE_TABLE.put(FileUtility.CHESHIRE_OVERLAY, new FileTypeInfo("Chesire Overlay", ".oly"));
        FILE_EXT_TABLE.put(".oly", FileUtility.CHESHIRE_OVERLAY);
        FILE_EXT_TABLE.put(".OLY", FileUtility.CHESHIRE_OVERLAY);

        FILE_TYPE_TABLE.put(FileUtility.COR, new FileTypeInfo("COR", ".info,.info~"));
        FILE_EXT_TABLE.put(".info", FileUtility.COR);
        FILE_EXT_TABLE.put(".info~", FileUtility.COR);
        FILE_EXT_TABLE.put(".INFO", FileUtility.COR);
        FILE_EXT_TABLE.put(".INFO~", FileUtility.COR);

        FILE_TYPE_TABLE.put(FileUtility.CUR, new FileTypeInfo("CUR", ".cur"));

        FILE_TYPE_TABLE.put(FileUtility.DIB, new FileTypeInfo("DIB", ".dib"));

        FILE_TYPE_TABLE.put(FileUtility.DICOM, new FileTypeInfo("DICOM", ".dcm,.ima"));
        // Don't put ".IMA" in FILE_EXT_TABLE; could be MAGNETOM_VISION
        FILE_EXT_TABLE.put(".dcm", FileUtility.DICOM);
        FILE_EXT_TABLE.put(".DCM", FileUtility.DICOM);

        FILE_TYPE_TABLE.put(FileUtility.DM3, new FileTypeInfo("DM3", ".dm3"));
        FILE_EXT_TABLE.put(".dm3", FileUtility.DM3);
        FILE_EXT_TABLE.put(".DM3", FileUtility.DM3);

        FILE_TYPE_TABLE.put(FileUtility.FITS, new FileTypeInfo("FITS", ".fits,.fts"));
        FILE_EXT_TABLE.put(".fits", FileUtility.FITS);
        FILE_EXT_TABLE.put(".FITS", FileUtility.FITS);

        FILE_TYPE_TABLE.put(FileUtility.GE_GENESIS, new FileTypeInfo("GE Genesis", ".sig"));
        FILE_EXT_TABLE.put(".sig", FileUtility.GE_GENESIS);
        FILE_EXT_TABLE.put(".SIG", FileUtility.GE_GENESIS);

        FILE_TYPE_TABLE.put(FileUtility.GE_SIGNA4X, new FileTypeInfo("GE Signa4x", ".gedno"));
        FILE_EXT_TABLE.put(".gedno", FileUtility.GE_SIGNA4X);
        FILE_EXT_TABLE.put(".GEDNO", FileUtility.GE_SIGNA4X);

        FILE_TYPE_TABLE.put(FileUtility.GIF, new FileTypeInfo("GIF", ".gif"));

        FILE_TYPE_TABLE.put(FileUtility.ICO, new FileTypeInfo("ICO", ".ico"));

        FILE_TYPE_TABLE.put(FileUtility.ICS, new FileTypeInfo("ICS", ".ics,.ids"));
        FILE_EXT_TABLE.put(".ics", FileUtility.ICS);
        FILE_EXT_TABLE.put(".ids", FileUtility.ICS);
        FILE_EXT_TABLE.put(".ICS", FileUtility.ICS);
        FILE_EXT_TABLE.put(".IDS", FileUtility.ICS);

        FILE_TYPE_TABLE.put(FileUtility.INTERFILE, new FileTypeInfo("Interfile", ".hdr"));
        // .hdr used by analyze, interfile and nifti
        FILE_EXT_TABLE.put(".hdr", FileUtility.UNDEFINED);
        FILE_EXT_TABLE.put(".HDR", FileUtility.UNDEFINED);


        FILE_TYPE_TABLE.put(FileUtility.JIMI, new FileTypeInfo("JIMI",
                ".jpg,.jpeg,.bmp,.gif,.pict,.png,.psd,.dib,.tga,.xbm,.xpm,.pcx,.ico,.cur"));
        FILE_EXT_TABLE.put(".jpeg", FileUtility.JIMI);
        FILE_EXT_TABLE.put(".jpg", FileUtility.JIMI);
        FILE_EXT_TABLE.put(".bmp", FileUtility.JIMI);
        FILE_EXT_TABLE.put(".gif", FileUtility.JIMI);
        FILE_EXT_TABLE.put(".pict", FileUtility.JIMI);
        FILE_EXT_TABLE.put(".png", FileUtility.JIMI);
        FILE_EXT_TABLE.put(".tga", FileUtility.JIMI);
        FILE_EXT_TABLE.put(".xbm", FileUtility.JIMI);
        FILE_EXT_TABLE.put(".xpm", FileUtility.JIMI);
        FILE_EXT_TABLE.put(".pcx", FileUtility.JIMI);
        FILE_EXT_TABLE.put(".ico", FileUtility.JIMI);
        FILE_EXT_TABLE.put(".cur", FileUtility.JIMI);
        FILE_EXT_TABLE.put(".JPEG", FileUtility.JIMI);
        FILE_EXT_TABLE.put(".JPG", FileUtility.JIMI);
        FILE_EXT_TABLE.put(".BMP", FileUtility.JIMI);
        FILE_EXT_TABLE.put(".GIF", FileUtility.JIMI);
        FILE_EXT_TABLE.put(".PICT", FileUtility.JIMI);
        FILE_EXT_TABLE.put(".PNG", FileUtility.JIMI);
        FILE_EXT_TABLE.put(".TGA", FileUtility.JIMI);
        FILE_EXT_TABLE.put(".XBM", FileUtility.JIMI);
        FILE_EXT_TABLE.put(".XPM", FileUtility.JIMI);
        FILE_EXT_TABLE.put(".PCX", FileUtility.JIMI);
        FILE_EXT_TABLE.put(".ICO", FileUtility.JIMI);
        FILE_EXT_TABLE.put(".CUR", FileUtility.JIMI);

        FILE_TYPE_TABLE.put(FileUtility.JPEG, new FileTypeInfo("JPEG", ".jpg,.jpeg"));
        FILE_TYPE_TABLE.put(FileUtility.JP2, new FileTypeInfo("JPEG2000", ".jp2"));

        FILE_TYPE_TABLE.put(FileUtility.LSM, new FileTypeInfo("LSM", ".lsm"));
        FILE_TYPE_TABLE.put(FileUtility.LSM_MULTIFILE, new FileTypeInfo("LSM multifile", ".lsm"));
        FILE_EXT_TABLE.put(".lsm", FileUtility.LSM);
        FILE_EXT_TABLE.put(".LSM", FileUtility.LSM);

        FILE_TYPE_TABLE.put(FileUtility.MAGNETOM_VISION, new FileTypeInfo("Magnetom Vision", ".ima"));

        FILE_TYPE_TABLE.put(FileUtility.MAP, new FileTypeInfo("Map", ".map"));
        FILE_EXT_TABLE.put(".map", FileUtility.MAP);
        FILE_EXT_TABLE.put(".MAP", FileUtility.MAP);

        FILE_TYPE_TABLE.put(FileUtility.MEDIVISION, new FileTypeInfo("Medvision", ".bin"));
        FILE_EXT_TABLE.put(".bin", FileUtility.MEDIVISION);
        FILE_EXT_TABLE.put(".BIN", FileUtility.MEDIVISION);

        FILE_TYPE_TABLE.put(FileUtility.MGH, new FileTypeInfo("MGH", ".mgh,.mgz,.mgh.gz"));
        FILE_EXT_TABLE.put(".mgh", FileUtility.MGH);
        FILE_EXT_TABLE.put(".mgz", FileUtility.MGH);
        FILE_EXT_TABLE.put(".mgh.gz", FileUtility.MGH);
        FILE_EXT_TABLE.put(".MGH", FileUtility.MGH);
        FILE_EXT_TABLE.put(".MGZ", FileUtility.MGH);
        FILE_EXT_TABLE.put(".MGH.GZ", FileUtility.MGH);
        

        FILE_TYPE_TABLE.put(FileUtility.MICRO_CAT, new FileTypeInfo("Micro CAT", ".log,.ct"));
        FILE_EXT_TABLE.put(".log", FileUtility.MICRO_CAT);
        FILE_EXT_TABLE.put(".ct", FileUtility.MICRO_CAT);
        FILE_EXT_TABLE.put(".LOG", FileUtility.MICRO_CAT);
        FILE_EXT_TABLE.put(".CT", FileUtility.MICRO_CAT);

        FILE_TYPE_TABLE.put(FileUtility.MINC, new FileTypeInfo("MINC", ".mnc"));
        // TODO: should this be undefined since .mnc can be both minc1 and minc2?
        FILE_EXT_TABLE.put(".mnc", FileUtility.MINC);
        FILE_EXT_TABLE.put(".MNC", FileUtility.MINC);

        FILE_TYPE_TABLE.put(FileUtility.MIPAV, new FileTypeInfo("MIPAV", ""));

        FILE_TYPE_TABLE.put(FileUtility.MRC, new FileTypeInfo("MRC", ".mrc"));
        FILE_EXT_TABLE.put(".mrc", FileUtility.MRC);
        FILE_EXT_TABLE.put(".MRC", FileUtility.MRC);

        FILE_TYPE_TABLE.put(FileUtility.NIFTI, new FileTypeInfo("NIFTI", ".nii,.img"));
        FILE_TYPE_TABLE.put(FileUtility.NIFTI_MULTIFILE, new FileTypeInfo("NIFTI multifile", ".nii,.img"));
        FILE_EXT_TABLE.put(".nii", FileUtility.NIFTI);
        FILE_EXT_TABLE.put(".NII", FileUtility.NIFTI);

        FILE_TYPE_TABLE.put(FileUtility.NRRD, new FileTypeInfo("NRRD", ".nrrd,.nhdr"));
        FILE_EXT_TABLE.put(".nrrd", FileUtility.NRRD);
        FILE_EXT_TABLE.put(".nhdr", FileUtility.NRRD);
        FILE_EXT_TABLE.put(".NRRD", FileUtility.NRRD);
        FILE_EXT_TABLE.put(".NHDR", FileUtility.NRRD);

        FILE_TYPE_TABLE.put(FileUtility.OSM, new FileTypeInfo("OSM", ".wu"));
        FILE_EXT_TABLE.put(".wu", FileUtility.OSM);
        FILE_EXT_TABLE.put(".WU", FileUtility.OSM);

        FILE_TYPE_TABLE.put(FileUtility.PCX, new FileTypeInfo("PCX", ".pcx"));

        FILE_TYPE_TABLE.put(FileUtility.PIC, new FileTypeInfo("PIC", ""));

        FILE_TYPE_TABLE.put(FileUtility.PICT, new FileTypeInfo("PICT", ".pict"));

        FILE_TYPE_TABLE.put(FileUtility.PNG, new FileTypeInfo("PNG", ".png"));

        FILE_TYPE_TABLE.put(FileUtility.PROJECT, new FileTypeInfo("Project", ""));

        FILE_TYPE_TABLE.put(FileUtility.PSD, new FileTypeInfo("PSD", ".psd"));

        FILE_TYPE_TABLE.put(FileUtility.QT, new FileTypeInfo("QT", ".mov,.qt"));
        FILE_EXT_TABLE.put(".mov", FileUtility.QT);
        FILE_EXT_TABLE.put(".qt", FileUtility.QT);
        FILE_EXT_TABLE.put(".MOV", FileUtility.QT);
        FILE_EXT_TABLE.put(".QT", FileUtility.QT);


        FILE_TYPE_TABLE.put(FileUtility.RAW, new FileTypeInfo("Raw", ".raw"));
        FILE_TYPE_TABLE.put(FileUtility.RAW_MULTIFILE, new FileTypeInfo("Raw multifile", ".raw"));
        FILE_EXT_TABLE.put(".raw", FileUtility.RAW);
        FILE_EXT_TABLE.put(".RAW", FileUtility.RAW);

        // SPM does not by official specification have a .spm extension
        // SPM99 and SPM2 are slight variants of the Mayo Analyze 7.5 file format
        // with the same .hdr and .img extensions
        // However, user could change the .img extension to .SPM
        // The .hdr extension would remain unchanged
        FILE_TYPE_TABLE.put(FileUtility.SPM, new FileTypeInfo("SPM", ".spm"));
        FILE_EXT_TABLE.put(".spm", FileUtility.SPM);
        FILE_EXT_TABLE.put(".SPM", FileUtility.SPM);

        FILE_TYPE_TABLE.put(FileUtility.STK, new FileTypeInfo("STK", ".stk"));
        FILE_EXT_TABLE.put(".stk", FileUtility.STK);
        FILE_EXT_TABLE.put(".STK", FileUtility.STK);

        FILE_TYPE_TABLE.put(FileUtility.SURFACE_XML, new FileTypeInfo("Surface XML", ""));

        FILE_TYPE_TABLE.put(FileUtility.TGA, new FileTypeInfo("TGA", ".tga"));

        FILE_TYPE_TABLE.put(FileUtility.TIFF, new FileTypeInfo("Tiff", ".tif,.tiff"));
        FILE_TYPE_TABLE.put(FileUtility.TIFF_MULTIFILE, new FileTypeInfo("Tiff multifile", ".tif,.tiff"));
        FILE_EXT_TABLE.put(".tif", FileUtility.TIFF);
        FILE_EXT_TABLE.put(".tiff", FileUtility.TIFF);
        FILE_EXT_TABLE.put(".TIF", FileUtility.TIFF);
        FILE_EXT_TABLE.put(".TIFF", FileUtility.TIFF);

        FILE_TYPE_TABLE.put(FileUtility.TMG, new FileTypeInfo("TMG", ".tmg"));
        FILE_EXT_TABLE.put(".tmg", FileUtility.TMG);
        FILE_EXT_TABLE.put(".TMG", FileUtility.TMG);

        FILE_TYPE_TABLE.put(FileUtility.VOI_FILE, new FileTypeInfo("VOI", ".voi"));
        FILE_EXT_TABLE.put(".voi", FileUtility.VOI_FILE);
        FILE_EXT_TABLE.put(".VOI", FileUtility.VOI_FILE);

        FILE_TYPE_TABLE.put(FileUtility.XBM, new FileTypeInfo("XBM", ".xbm"));

        FILE_TYPE_TABLE.put(FileUtility.XML, new FileTypeInfo("XML", ".xml"));
        FILE_TYPE_TABLE.put(FileUtility.XML_MULTIFILE, new FileTypeInfo("XML multifile", ".xml"));
        FILE_EXT_TABLE.put(".xml", FileUtility.XML);
        FILE_EXT_TABLE.put(".XML", FileUtility.XML);

        FILE_TYPE_TABLE.put(FileUtility.XPM, new FileTypeInfo("XPM", ".xpm"));

        FILE_TYPE_TABLE.put(FileUtility.PARREC, new FileTypeInfo("Philips PARREC", ".par,.rec,.parv2,.frec"));
        FILE_EXT_TABLE.put(".par", FileUtility.PARREC);
        FILE_EXT_TABLE.put(".parv2", FileUtility.PARREC);
        FILE_EXT_TABLE.put(".rec", FileUtility.PARREC);
        FILE_EXT_TABLE.put(".frec", FileUtility.PARREC);
        FILE_EXT_TABLE.put(".PAR", FileUtility.PARREC);
        FILE_EXT_TABLE.put(".PARV2", FileUtility.PARREC);
        FILE_EXT_TABLE.put(".PARv2", FileUtility.PARREC);
        FILE_EXT_TABLE.put(".REC", FileUtility.PARREC);
        FILE_EXT_TABLE.put(".FREC", FileUtility.PARREC);
        FILE_EXT_TABLE.put(".fREC", FileUtility.PARREC);

        FILE_TYPE_TABLE.put(FileUtility.SURFACEREF_XML, new FileTypeInfo("Surface Reference XML", ""));

        FILE_TYPE_TABLE.put(FileUtility.MINC_HDF, new FileTypeInfo("MINC 2.0", ".mnc"));

        FILE_TYPE_TABLE.put(FileUtility.LIFF, new FileTypeInfo("LIFF", ".liff"));
        FILE_EXT_TABLE.put(".liff", FileUtility.LIFF);
        FILE_EXT_TABLE.put(".LIFF", FileUtility.LIFF);

        FILE_TYPE_TABLE.put(FileUtility.BFLOAT, new FileTypeInfo("BFLOAT", ".bfloat"));
        FILE_EXT_TABLE.put(".bfloat", FileUtility.BFLOAT);
        FILE_EXT_TABLE.put(".BFLOAT", FileUtility.BFLOAT);
        
        FILE_EXT_TABLE.put(".jp2", FileUtility.JP2);
        FILE_EXT_TABLE.put(".JP2", FileUtility.JP2);
    }

    /**
     * Gets the file type based upon the suffix.
     * 
     * @param ext The suffix (including the period -- e.g., ".xml")
     * 
     * @return The file type for the given suffix (or UNDEFINED if the suffix is null, unrecognized, or ambiguous).
     */
    public static final int getFileTypeFromSuffix(final String ext) {
        // unable to determine file type for that suffix
        if (ext == null || !FILE_EXT_TABLE.containsKey(FileTypeTable.standardizeExtensionFormat(ext))) {
            return FileUtility.UNDEFINED;
        }

        // may return UNDEFINED if the suffix can be handled by more than one file format
        return FILE_EXT_TABLE.get(FileTypeTable.standardizeExtensionFormat(ext));
    }

    public static final FileTypeInfo getFileTypeInfo(final int fileType) {
        return FILE_TYPE_TABLE.get(fileType);
    }

    public static final void addExtensionAssociation(final String ext, final int fileType) {
        FILE_TYPE_TABLE.get(fileType).addExtension(FileTypeTable.standardizeExtensionFormat(ext));
        FILE_EXT_TABLE.put(FileTypeTable.standardizeExtensionFormat(ext), fileType);
    }

    /**
     * Standardizes an file extension string to conform with how they should be stored in the file type table.
     * 
     * @param ext The extension to standardize.
     * 
     * @return The standardized extension.
     */
    private static final String standardizeExtensionFormat(final String ext) {
        if ( !ext.startsWith(".")) {
            return "." + ext;
        } else {
            return ext;
        }
    }

    public static final String[] getAllFileTypeDescriptions() {
        Enumeration<FileTypeInfo> fileTypes = FILE_TYPE_TABLE.elements();

        String[] fileTypeDescriptions = new String[FILE_TYPE_TABLE.size()];

        int i = 0;
        while (fileTypes.hasMoreElements()) {
            fileTypeDescriptions[i] = fileTypes.nextElement().getExtendedDescription();
            i++;
        }

        return fileTypeDescriptions;
    }

    public static final Vector<String> getAllFileTypeExtensions() {
        Enumeration<FileTypeInfo> fileTypes = FILE_TYPE_TABLE.elements();

        Vector<String> allExts = new Vector<String>(FILE_TYPE_TABLE.size() * 2);
        Vector<String> typeExts;

        while (fileTypes.hasMoreElements()) {
            typeExts = fileTypes.nextElement().getExtensionList();
            for (String ext : typeExts) {
                if ( !ext.equals("")) {
                    allExts.add(ext);
                }
            }
        }

        return allExts;
    }

    public static final String[] getAllFileTypeDefaultExtensions() {
        Enumeration<FileTypeInfo> fileTypes = FILE_TYPE_TABLE.elements();

        String[] fileTypeExtensions = new String[FILE_TYPE_TABLE.size()];

        int i = 0;
        while (fileTypes.hasMoreElements()) {
            fileTypeExtensions[i] = fileTypes.nextElement().getDefaultExtension();
            i++;
        }

        return fileTypeExtensions;
    }

    public static final int[] getAllFileTypes() {
        Enumeration<Integer> fileTypes = FILE_TYPE_TABLE.keys();

        int[] fileTypeInts = new int[FILE_TYPE_TABLE.size()];

        int i = 0;
        while (fileTypes.hasMoreElements()) {
            fileTypeInts[i] = fileTypes.nextElement().intValue();
        }

        return fileTypeInts;
    }

    public static final Hashtable<Integer, FileTypeInfo> getFileTypeTable() {
        return (Hashtable<Integer, FileTypeInfo>) FILE_TYPE_TABLE.clone();
    }
}
