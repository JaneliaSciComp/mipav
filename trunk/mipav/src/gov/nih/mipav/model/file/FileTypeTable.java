package gov.nih.mipav.model.file;


import java.util.*;


public class FileTypeTable {
    private static Hashtable<Integer, FileTypeInfo> FILE_TYPE_TABLE;

    private static Hashtable<String, Integer> FILE_EXT_TABLE;

    static {
        FileTypeTable.FILE_TYPE_TABLE = new Hashtable<Integer, FileTypeInfo>(100);
        FileTypeTable.FILE_EXT_TABLE = new Hashtable<String, Integer>(200);

        FileTypeTable.FILE_TYPE_TABLE.put(FileUtility.UNDEFINED, new FileTypeInfo("Undefined", ""));

        FileTypeTable.FILE_TYPE_TABLE.put(FileUtility.AFNI, new FileTypeInfo("AFNI", ".afni,.head,.brik"));
        FileTypeTable.FILE_EXT_TABLE.put(".afni", FileUtility.AFNI);
        FileTypeTable.FILE_EXT_TABLE.put(".head", FileUtility.AFNI);
        FileTypeTable.FILE_EXT_TABLE.put(".brik", FileUtility.AFNI);
        FileTypeTable.FILE_EXT_TABLE.put(".AFNI", FileUtility.AFNI);
        FileTypeTable.FILE_EXT_TABLE.put(".HEAD", FileUtility.AFNI);
        FileTypeTable.FILE_EXT_TABLE.put(".BRIK", FileUtility.AFNI);

        FileTypeTable.FILE_TYPE_TABLE.put(FileUtility.ANALYZE, new FileTypeInfo("Analyze", ".img"));
        FileTypeTable.FILE_TYPE_TABLE.put(FileUtility.ANALYZE_MULTIFILE, new FileTypeInfo("Analyze multifile", ".img"));

        // analyze, nifti and interfile use .img
        FileTypeTable.FILE_EXT_TABLE.put(".img", FileUtility.UNDEFINED);
        FileTypeTable.FILE_EXT_TABLE.put(".IMG", FileUtility.UNDEFINED);

        FileTypeTable.FILE_TYPE_TABLE.put(FileUtility.AVI, new FileTypeInfo("Avi", ".avi"));
        FileTypeTable.FILE_EXT_TABLE.put(".avi", FileUtility.AVI);
        FileTypeTable.FILE_EXT_TABLE.put(".AVI", FileUtility.AVI);

        FileTypeTable.FILE_TYPE_TABLE.put(FileUtility.BIORAD, new FileTypeInfo("Bio-Rad", ".pic"));

        // both Bio-rad and JIMI use .pic
        FileTypeTable.FILE_EXT_TABLE.put(".pic", FileUtility.UNDEFINED);
        FileTypeTable.FILE_EXT_TABLE.put(".PIC", FileUtility.UNDEFINED);

        FileTypeTable.FILE_TYPE_TABLE.put(FileUtility.BMP, new FileTypeInfo("BMP", ".bmp"));
        FileTypeTable.FILE_EXT_TABLE.put(".bmp", FileUtility.BMP);
        FileTypeTable.FILE_EXT_TABLE.put(".BMP", FileUtility.BMP);

        FileTypeTable.FILE_TYPE_TABLE.put(FileUtility.BRUKER, new FileTypeInfo("BRUKER", ".2dseq"));

        FileTypeTable.FILE_TYPE_TABLE.put(FileUtility.CHESHIRE, new FileTypeInfo("Chesire", ".imc"));
        FileTypeTable.FILE_EXT_TABLE.put(".imc", FileUtility.CHESHIRE);
        FileTypeTable.FILE_EXT_TABLE.put(".IMC", FileUtility.CHESHIRE);

        FileTypeTable.FILE_TYPE_TABLE.put(FileUtility.CHESHIRE_OVERLAY, new FileTypeInfo("Chesire Overlay", ".oly"));
        FileTypeTable.FILE_EXT_TABLE.put(".oly", FileUtility.CHESHIRE_OVERLAY);
        FileTypeTable.FILE_EXT_TABLE.put(".OLY", FileUtility.CHESHIRE_OVERLAY);

        FileTypeTable.FILE_TYPE_TABLE.put(FileUtility.COR, new FileTypeInfo("COR", ".info,.info~"));
        FileTypeTable.FILE_EXT_TABLE.put(".info", FileUtility.COR);
        FileTypeTable.FILE_EXT_TABLE.put(".info~", FileUtility.COR);
        FileTypeTable.FILE_EXT_TABLE.put(".INFO", FileUtility.COR);
        FileTypeTable.FILE_EXT_TABLE.put(".INFO~", FileUtility.COR);

        FileTypeTable.FILE_TYPE_TABLE.put(FileUtility.CUR, new FileTypeInfo("CUR", ".cur"));

        FileTypeTable.FILE_TYPE_TABLE.put(FileUtility.DIB, new FileTypeInfo("DIB", ".dib"));

        FileTypeTable.FILE_TYPE_TABLE.put(FileUtility.DICOM, new FileTypeInfo("DICOM", ".dcm,.ima"));
        // Don't put ".IMA" in FILE_EXT_TABLE; could be MAGNETOM_VISION
        FileTypeTable.FILE_EXT_TABLE.put(".dcm", FileUtility.DICOM);
        FileTypeTable.FILE_EXT_TABLE.put(".DCM", FileUtility.DICOM);

        FileTypeTable.FILE_TYPE_TABLE.put(FileUtility.DM3, new FileTypeInfo("DM3", ".dm3"));
        FileTypeTable.FILE_EXT_TABLE.put(".dm3", FileUtility.DM3);
        FileTypeTable.FILE_EXT_TABLE.put(".DM3", FileUtility.DM3);

        FileTypeTable.FILE_TYPE_TABLE.put(FileUtility.FITS, new FileTypeInfo("FITS", ".fits,.fts"));
        FileTypeTable.FILE_EXT_TABLE.put(".fits", FileUtility.FITS);
        FileTypeTable.FILE_EXT_TABLE.put(".FITS", FileUtility.FITS);

        FileTypeTable.FILE_TYPE_TABLE.put(FileUtility.GE_GENESIS, new FileTypeInfo("GE Genesis", ".sig"));
        FileTypeTable.FILE_EXT_TABLE.put(".sig", FileUtility.GE_GENESIS);
        FileTypeTable.FILE_EXT_TABLE.put(".SIG", FileUtility.GE_GENESIS);

        FileTypeTable.FILE_TYPE_TABLE.put(FileUtility.GE_SIGNA4X, new FileTypeInfo("GE Signa4x", ".gedno"));
        FileTypeTable.FILE_EXT_TABLE.put(".gedno", FileUtility.GE_SIGNA4X);
        FileTypeTable.FILE_EXT_TABLE.put(".GEDNO", FileUtility.GE_SIGNA4X);

        FileTypeTable.FILE_TYPE_TABLE.put(FileUtility.GIF, new FileTypeInfo("GIF", ".gif"));

        FileTypeTable.FILE_TYPE_TABLE.put(FileUtility.ICO, new FileTypeInfo("ICO", ".ico"));

        FileTypeTable.FILE_TYPE_TABLE.put(FileUtility.ICS, new FileTypeInfo("ICS", ".ics,.ids"));
        FileTypeTable.FILE_EXT_TABLE.put(".ics", FileUtility.ICS);
        FileTypeTable.FILE_EXT_TABLE.put(".ids", FileUtility.ICS);
        FileTypeTable.FILE_EXT_TABLE.put(".ICS", FileUtility.ICS);
        FileTypeTable.FILE_EXT_TABLE.put(".IDS", FileUtility.ICS);

        FileTypeTable.FILE_TYPE_TABLE.put(FileUtility.INTERFILE, new FileTypeInfo("Interfile", ".hdr"));
        // .hdr used by analyze, interfile  and nifti
        FileTypeTable.FILE_EXT_TABLE.put(".hdr", FileUtility.UNDEFINED);
        FileTypeTable.FILE_EXT_TABLE.put(".HDR", FileUtility.UNDEFINED);
        
        FileTypeTable.FILE_TYPE_TABLE.put(FileUtility.BMP, new FileTypeInfo("BMP", ".bmp"));
        FileTypeTable.FILE_EXT_TABLE.put(".bmp", FileUtility.BMP);
        FileTypeTable.FILE_EXT_TABLE.put(".BMP", FileUtility.BMP);

        FileTypeTable.FILE_TYPE_TABLE.put(FileUtility.JIMI, new FileTypeInfo("JIMI",
                ".jpg,.jpeg,.gif,.pict,.png,.psd,.dib,.tga,.xbm,.xpm,.pcx,.ico,.cur"));
        FileTypeTable.FILE_EXT_TABLE.put(".jpeg", FileUtility.JIMI);
        FileTypeTable.FILE_EXT_TABLE.put(".jpg", FileUtility.JIMI); 
        FileTypeTable.FILE_EXT_TABLE.put(".gif", FileUtility.JIMI);
        FileTypeTable.FILE_EXT_TABLE.put(".pict", FileUtility.JIMI);
        FileTypeTable.FILE_EXT_TABLE.put(".png", FileUtility.JIMI);
        FileTypeTable.FILE_EXT_TABLE.put(".psd", FileUtility.JIMI);
        FileTypeTable.FILE_EXT_TABLE.put(".tga", FileUtility.JIMI);
        FileTypeTable.FILE_EXT_TABLE.put(".xbm", FileUtility.JIMI);
        FileTypeTable.FILE_EXT_TABLE.put(".xpm", FileUtility.JIMI);
        FileTypeTable.FILE_EXT_TABLE.put(".pcx", FileUtility.JIMI);
        FileTypeTable.FILE_EXT_TABLE.put(".ico", FileUtility.JIMI);
        FileTypeTable.FILE_EXT_TABLE.put(".cur", FileUtility.JIMI);
        FileTypeTable.FILE_EXT_TABLE.put(".JPEG", FileUtility.JIMI);
        FileTypeTable.FILE_EXT_TABLE.put(".JPG", FileUtility.JIMI);
        FileTypeTable.FILE_EXT_TABLE.put(".GIF", FileUtility.JIMI);
        FileTypeTable.FILE_EXT_TABLE.put(".PICT", FileUtility.JIMI);
        FileTypeTable.FILE_EXT_TABLE.put(".PNG", FileUtility.JIMI);
        FileTypeTable.FILE_EXT_TABLE.put(".PSD", FileUtility.JIMI);
        FileTypeTable.FILE_EXT_TABLE.put(".TGA", FileUtility.JIMI);
        FileTypeTable.FILE_EXT_TABLE.put(".XBM", FileUtility.JIMI);
        FileTypeTable.FILE_EXT_TABLE.put(".XPM", FileUtility.JIMI);
        FileTypeTable.FILE_EXT_TABLE.put(".PCX", FileUtility.JIMI);
        FileTypeTable.FILE_EXT_TABLE.put(".ICO", FileUtility.JIMI);
        FileTypeTable.FILE_EXT_TABLE.put(".CUR", FileUtility.JIMI);

        FileTypeTable.FILE_TYPE_TABLE.put(FileUtility.JPEG, new FileTypeInfo("JPEG", ".jpg,.jpeg"));
        FileTypeTable.FILE_TYPE_TABLE.put(FileUtility.JP2, new FileTypeInfo("JPEG2000", ".jp2"));

        FileTypeTable.FILE_TYPE_TABLE.put(FileUtility.LSM, new FileTypeInfo("LSM", ".lsm"));
        FileTypeTable.FILE_TYPE_TABLE.put(FileUtility.LSM_MULTIFILE, new FileTypeInfo("LSM multifile", ".lsm"));
        FileTypeTable.FILE_EXT_TABLE.put(".lsm", FileUtility.LSM);
        FileTypeTable.FILE_EXT_TABLE.put(".LSM", FileUtility.LSM);

        FileTypeTable.FILE_TYPE_TABLE.put(FileUtility.MAGNETOM_VISION, new FileTypeInfo("Magnetom Vision", ".ima"));

        FileTypeTable.FILE_TYPE_TABLE.put(FileUtility.MAP, new FileTypeInfo("Map", ".map"));
        FileTypeTable.FILE_EXT_TABLE.put(".map", FileUtility.MAP);
        FileTypeTable.FILE_EXT_TABLE.put(".MAP", FileUtility.MAP);

        FileTypeTable.FILE_TYPE_TABLE.put(FileUtility.MEDIVISION, new FileTypeInfo("Medvision", ".bin"));
        FileTypeTable.FILE_EXT_TABLE.put(".bin", FileUtility.MEDIVISION);
        FileTypeTable.FILE_EXT_TABLE.put(".BIN", FileUtility.MEDIVISION);

        FileTypeTable.FILE_TYPE_TABLE.put(FileUtility.MGH, new FileTypeInfo("MGH", ".mgh,.mgz,.mgh.gz"));
        FileTypeTable.FILE_EXT_TABLE.put(".mgh", FileUtility.MGH);
        FileTypeTable.FILE_EXT_TABLE.put(".mgz", FileUtility.MGH);
        FileTypeTable.FILE_EXT_TABLE.put(".mgh.gz", FileUtility.MGH);
        FileTypeTable.FILE_EXT_TABLE.put(".MGH", FileUtility.MGH);
        FileTypeTable.FILE_EXT_TABLE.put(".MGZ", FileUtility.MGH);
        FileTypeTable.FILE_EXT_TABLE.put(".MGH.GZ", FileUtility.MGH);
        
        FileTypeTable.FILE_TYPE_TABLE.put(FileUtility.MICRO_CAT, new FileTypeInfo("Micro CAT", ".log,.ct"));
        FileTypeTable.FILE_EXT_TABLE.put(".log", FileUtility.MICRO_CAT);
        FileTypeTable.FILE_EXT_TABLE.put(".ct", FileUtility.MICRO_CAT);
        FileTypeTable.FILE_EXT_TABLE.put(".LOG", FileUtility.MICRO_CAT);
        FileTypeTable.FILE_EXT_TABLE.put(".CT", FileUtility.MICRO_CAT);

        FileTypeTable.FILE_TYPE_TABLE.put(FileUtility.MINC, new FileTypeInfo("MINC", ".mnc"));
        // TODO: should this be undefined since .mnc can be both minc1 and minc2?
        FileTypeTable.FILE_EXT_TABLE.put(".mnc", FileUtility.MINC);
        FileTypeTable.FILE_EXT_TABLE.put(".MNC", FileUtility.MINC);

        FileTypeTable.FILE_TYPE_TABLE.put(FileUtility.MIPAV, new FileTypeInfo("MIPAV", ""));

        FileTypeTable.FILE_TYPE_TABLE.put(FileUtility.MRC, new FileTypeInfo("MRC", ".mrc"));
        FileTypeTable.FILE_EXT_TABLE.put(".mrc", FileUtility.MRC);
        FileTypeTable.FILE_EXT_TABLE.put(".MRC", FileUtility.MRC);

        FileTypeTable.FILE_TYPE_TABLE.put(FileUtility.NIFTI, new FileTypeInfo("NIFTI", ".nii,.img"));
        FileTypeTable.FILE_TYPE_TABLE
                .put(FileUtility.NIFTI_MULTIFILE, new FileTypeInfo("NIFTI multifile", ".nii,.img"));
        FileTypeTable.FILE_EXT_TABLE.put(".nii", FileUtility.NIFTI);
        FileTypeTable.FILE_EXT_TABLE.put(".NII", FileUtility.NIFTI);
        
        FileTypeTable.FILE_TYPE_TABLE.put(FileUtility.METAIMAGE, new FileTypeInfo("MetaImage", ".mhd,.mha,.raw"));
        FileTypeTable.FILE_EXT_TABLE.put(".mhd", FileUtility.METAIMAGE);
        FileTypeTable.FILE_EXT_TABLE.put(".MHD", FileUtility.METAIMAGE);
        FileTypeTable.FILE_EXT_TABLE.put(".mha", FileUtility.METAIMAGE);
        FileTypeTable.FILE_EXT_TABLE.put(".MHA", FileUtility.METAIMAGE);

        FileTypeTable.FILE_TYPE_TABLE.put(FileUtility.NRRD, new FileTypeInfo("NRRD", ".nrrd,.nhdr"));
        FileTypeTable.FILE_TYPE_TABLE.put(FileUtility.NRRD_MULTIFILE, new FileTypeInfo("NRRD multifile", ".nrrd,.nhdr"));
        FileTypeTable.FILE_EXT_TABLE.put(".nrrd", FileUtility.NRRD);
        FileTypeTable.FILE_EXT_TABLE.put(".nhdr", FileUtility.NRRD);
        FileTypeTable.FILE_EXT_TABLE.put(".NRRD", FileUtility.NRRD);
        FileTypeTable.FILE_EXT_TABLE.put(".NHDR", FileUtility.NRRD);

        FileTypeTable.FILE_TYPE_TABLE.put(FileUtility.PCX, new FileTypeInfo("PCX", ".pcx"));

        FileTypeTable.FILE_TYPE_TABLE.put(FileUtility.PIC, new FileTypeInfo("PIC", ""));

        FileTypeTable.FILE_TYPE_TABLE.put(FileUtility.PICT, new FileTypeInfo("PICT", ".pict"));

        FileTypeTable.FILE_TYPE_TABLE.put(FileUtility.PNG, new FileTypeInfo("PNG", ".png"));

        FileTypeTable.FILE_TYPE_TABLE.put(FileUtility.PSD, new FileTypeInfo("PSD", ".psd"));

        FileTypeTable.FILE_TYPE_TABLE.put(FileUtility.QT, new FileTypeInfo("QT", ".mov,.qt"));
        FileTypeTable.FILE_EXT_TABLE.put(".mov", FileUtility.QT);
        FileTypeTable.FILE_EXT_TABLE.put(".qt", FileUtility.QT);
        FileTypeTable.FILE_EXT_TABLE.put(".MOV", FileUtility.QT);
        FileTypeTable.FILE_EXT_TABLE.put(".QT", FileUtility.QT);

        FileTypeTable.FILE_TYPE_TABLE.put(FileUtility.RAW, new FileTypeInfo("Raw", ".raw"));
        FileTypeTable.FILE_TYPE_TABLE.put(FileUtility.RAW_MULTIFILE, new FileTypeInfo("Raw multifile", ".raw"));
        FileTypeTable.FILE_EXT_TABLE.put(".raw", FileUtility.RAW);
        FileTypeTable.FILE_EXT_TABLE.put(".RAW", FileUtility.RAW);

        // SPM does not by official specification have a .spm extension
        // SPM99 and SPM2 are slight variants of the Mayo Analyze 7.5 file format
        // with the same .hdr and .img extensions
        // However, user could change the .img extension to .SPM
        // The .hdr extension would remain unchanged
        FileTypeTable.FILE_TYPE_TABLE.put(FileUtility.SPM, new FileTypeInfo("SPM", ".spm"));
        FileTypeTable.FILE_EXT_TABLE.put(".spm", FileUtility.SPM);
        FileTypeTable.FILE_EXT_TABLE.put(".SPM", FileUtility.SPM);

        FileTypeTable.FILE_TYPE_TABLE.put(FileUtility.STK, new FileTypeInfo("STK", ".stk"));
        FileTypeTable.FILE_EXT_TABLE.put(".stk", FileUtility.STK);
        FileTypeTable.FILE_EXT_TABLE.put(".STK", FileUtility.STK);

        FileTypeTable.FILE_TYPE_TABLE.put(FileUtility.SURFACE_XML, new FileTypeInfo("Surface XML", ""));

        FileTypeTable.FILE_TYPE_TABLE.put(FileUtility.TGA, new FileTypeInfo("TGA", ".tga"));

        FileTypeTable.FILE_TYPE_TABLE.put(FileUtility.TIFF, new FileTypeInfo("Tiff", ".tif,.tiff"));
        FileTypeTable.FILE_TYPE_TABLE.put(FileUtility.TIFF_MULTIFILE, new FileTypeInfo("Tiff multifile", ".tif,.tiff"));
        FileTypeTable.FILE_EXT_TABLE.put(".tif", FileUtility.TIFF);
        FileTypeTable.FILE_EXT_TABLE.put(".tiff", FileUtility.TIFF);
        FileTypeTable.FILE_EXT_TABLE.put(".TIF", FileUtility.TIFF);
        FileTypeTable.FILE_EXT_TABLE.put(".TIFF", FileUtility.TIFF);

        FileTypeTable.FILE_TYPE_TABLE.put(FileUtility.TMG, new FileTypeInfo("TMG", ".tmg"));
        FileTypeTable.FILE_EXT_TABLE.put(".tmg", FileUtility.TMG);
        FileTypeTable.FILE_EXT_TABLE.put(".TMG", FileUtility.TMG);

        FileTypeTable.FILE_TYPE_TABLE.put(FileUtility.VOI_FILE, new FileTypeInfo("VOI", ".voi"));
        FileTypeTable.FILE_EXT_TABLE.put(".voi", FileUtility.VOI_FILE);
        FileTypeTable.FILE_EXT_TABLE.put(".VOI", FileUtility.VOI_FILE);

        FileTypeTable.FILE_TYPE_TABLE.put(FileUtility.XBM, new FileTypeInfo("XBM", ".xbm"));

        FileTypeTable.FILE_TYPE_TABLE.put(FileUtility.XML, new FileTypeInfo("XML", ".xml"));
        FileTypeTable.FILE_TYPE_TABLE.put(FileUtility.XML_MULTIFILE, new FileTypeInfo("XML multifile", ".xml"));
        FileTypeTable.FILE_EXT_TABLE.put(".xml", FileUtility.XML);
        FileTypeTable.FILE_EXT_TABLE.put(".XML", FileUtility.XML);

        FileTypeTable.FILE_TYPE_TABLE.put(FileUtility.XPM, new FileTypeInfo("XPM", ".xpm"));

        FileTypeTable.FILE_TYPE_TABLE.put(FileUtility.PARREC, new FileTypeInfo("Philips PARREC",
                ".par,.rec,.parv2,.frec"));
        FileTypeTable.FILE_TYPE_TABLE.put(FileUtility.PARREC_MULTIFILE, new FileTypeInfo("PARREC multifile",
                ".par,.rec,.parv2,.frec"));
        FileTypeTable.FILE_EXT_TABLE.put(".par", FileUtility.PARREC);
        FileTypeTable.FILE_EXT_TABLE.put(".parv2", FileUtility.PARREC);
        FileTypeTable.FILE_EXT_TABLE.put(".rec", FileUtility.PARREC);
        FileTypeTable.FILE_EXT_TABLE.put(".frec", FileUtility.PARREC);
        FileTypeTable.FILE_EXT_TABLE.put(".PAR", FileUtility.PARREC);
        FileTypeTable.FILE_EXT_TABLE.put(".PARV2", FileUtility.PARREC);
        FileTypeTable.FILE_EXT_TABLE.put(".PARv2", FileUtility.PARREC);
        FileTypeTable.FILE_EXT_TABLE.put(".REC", FileUtility.PARREC);
        FileTypeTable.FILE_EXT_TABLE.put(".FREC", FileUtility.PARREC);
        FileTypeTable.FILE_EXT_TABLE.put(".fREC", FileUtility.PARREC);
        FileTypeTable.FILE_EXT_TABLE.put(".SPAR", FileUtility.SPAR);
        FileTypeTable.FILE_EXT_TABLE.put(".SDAT", FileUtility.SPAR);

        FileTypeTable.FILE_TYPE_TABLE.put(FileUtility.SURFACEREF_XML, new FileTypeInfo("Surface Reference XML", ""));

        FileTypeTable.FILE_TYPE_TABLE.put(FileUtility.MINC_HDF, new FileTypeInfo("MINC 2.0", ".mnc"));

        FileTypeTable.FILE_TYPE_TABLE.put(FileUtility.LIFF, new FileTypeInfo("LIFF", ".liff"));
        FileTypeTable.FILE_EXT_TABLE.put(".liff", FileUtility.LIFF);
        FileTypeTable.FILE_EXT_TABLE.put(".LIFF", FileUtility.LIFF);

        FileTypeTable.FILE_TYPE_TABLE.put(FileUtility.BFLOAT, new FileTypeInfo("BFLOAT", ".bfloat"));
        FileTypeTable.FILE_EXT_TABLE.put(".bfloat", FileUtility.BFLOAT);
        FileTypeTable.FILE_EXT_TABLE.put(".BFLOAT", FileUtility.BFLOAT);
        
        FileTypeTable.FILE_EXT_TABLE.put(".jp2", FileUtility.JP2);
        FileTypeTable.FILE_EXT_TABLE.put(".JP2", FileUtility.JP2);
        
        FileTypeTable.FILE_TYPE_TABLE.put(FileUtility.ZVI, new FileTypeInfo("ZVI", ".zvi"));
        FileTypeTable.FILE_EXT_TABLE.put(".zvi", FileUtility.ZVI);
        FileTypeTable.FILE_EXT_TABLE.put(".ZVI", FileUtility.ZVI);
        
        FileTypeTable.FILE_TYPE_TABLE.put(FileUtility.MATLAB, new FileTypeInfo("MATLAB", ".mat"));
        FileTypeTable.FILE_EXT_TABLE.put(".mat", FileUtility.MATLAB);
        FileTypeTable.FILE_EXT_TABLE.put(".MAT", FileUtility.MATLAB);
        
        FileTypeTable.FILE_EXT_TABLE.put(".v", FileUtility.VISTA);
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
        if (ext == null || !FileTypeTable.FILE_EXT_TABLE.containsKey(FileTypeTable.standardizeExtensionFormat(ext))) {
            return FileUtility.UNDEFINED;
        }

        // may return UNDEFINED if the suffix can be handled by more than one file format
        return FileTypeTable.FILE_EXT_TABLE.get(FileTypeTable.standardizeExtensionFormat(ext));
    }

    public static final FileTypeInfo getFileTypeInfo(final int fileType) {
        return FileTypeTable.FILE_TYPE_TABLE.get(fileType);
    }

    public static final void addExtensionAssociation(final String ext, final int fileType) {
        FileTypeTable.FILE_TYPE_TABLE.get(fileType).addExtension(FileTypeTable.standardizeExtensionFormat(ext));
        FileTypeTable.FILE_EXT_TABLE.put(FileTypeTable.standardizeExtensionFormat(ext), fileType);
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
        final Enumeration<FileTypeInfo> fileTypes = FileTypeTable.FILE_TYPE_TABLE.elements();

        final String[] fileTypeDescriptions = new String[FileTypeTable.FILE_TYPE_TABLE.size()];

        int i = 0;
        while (fileTypes.hasMoreElements()) {
            fileTypeDescriptions[i] = fileTypes.nextElement().getExtendedDescription();
            i++;
        }

        return fileTypeDescriptions;
    }

    public static final Vector<String> getAllFileTypeExtensions() {
        final Enumeration<FileTypeInfo> fileTypes = FileTypeTable.FILE_TYPE_TABLE.elements();

        final Vector<String> allExts = new Vector<String>(FileTypeTable.FILE_TYPE_TABLE.size() * 2);
        Vector<String> typeExts;

        while (fileTypes.hasMoreElements()) {
            typeExts = fileTypes.nextElement().getExtensionList();
            for (final String ext : typeExts) {
                if ( !ext.equals("")) {
                    allExts.add(ext);
                }
            }
        }

        return allExts;
    }

    public static final String[] getAllFileTypeDefaultExtensions() {
        final Enumeration<FileTypeInfo> fileTypes = FileTypeTable.FILE_TYPE_TABLE.elements();

        final String[] fileTypeExtensions = new String[FileTypeTable.FILE_TYPE_TABLE.size()];

        int i = 0;
        while (fileTypes.hasMoreElements()) {
            fileTypeExtensions[i] = fileTypes.nextElement().getDefaultExtension();
            i++;
        }

        return fileTypeExtensions;
    }

    public static final int[] getAllFileTypes() {
        final Enumeration<Integer> fileTypes = FileTypeTable.FILE_TYPE_TABLE.keys();

        final int[] fileTypeInts = new int[FileTypeTable.FILE_TYPE_TABLE.size()];

        final int i = 0;
        while (fileTypes.hasMoreElements()) {
            fileTypeInts[i] = fileTypes.nextElement().intValue();
        }

        return fileTypeInts;
    }

    public static final Hashtable<Integer, FileTypeInfo> getFileTypeTable() {
        return (Hashtable<Integer, FileTypeInfo>) FileTypeTable.FILE_TYPE_TABLE.clone();
    }
}
