package gov.nih.mipav.model.file;


import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.*;

import java.awt.*;
import java.io.*;

public class FilePGM extends FileBase {
	
	/** DOCUMENT ME! */
    private File file;

    /** DOCUMENT ME! */
    private String fileDir;

    /** DOCUMENT ME! */
    private FileInfoPGM fileInfo;
    
    /** DOCUMENT ME! */
    private String fileName;

    /** DOCUMENT ME! */
    private ModelImage image;

    /** DOCUMENT ME! */
    private ViewJProgressBar progressBar;
    
    /**
     * FileMedVision - MedVision reader/writer constructor.
     *
     * @param  fName  file name
     * @param  fDir   file directory
     */
    public FilePGM(String fName, String fDir) {
        fileName = fName;
        fileDir = fDir;
    }
    
    public ModelImage readImage() throws IOException {
        int i;
        int[] extents;
        FileRawChunk reader;
        boolean endianess;

        fileInfo = new FileInfoPGM(fileName, fileDir, FileUtility.PGM);
        file = new File(fileDir + fileName);

        try {
            raFile = new RandomAccessFile(file, "rw");
        } catch (IOException e) {
            raFile = new RandomAccessFile(file, "r");
        }

        fileInfo.setEndianess(BIG_ENDIAN);
        endianess = BIG_ENDIAN;
        
        return null;
    }
}