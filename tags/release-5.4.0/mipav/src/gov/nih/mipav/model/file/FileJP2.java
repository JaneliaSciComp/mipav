package gov.nih.mipav.model.file;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
//import gov.nih.mipav.view.dialogs.JDialogLoadLeica.*;

import java.awt.BorderLayout;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.*;

import java.text.DecimalFormat;

import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JTabbedPane;
import javax.swing.JTextField;

//import pluginJP2.rawimg.BEByteArrayInputStream;
//import pluginJP2.rawimg.ImgWriterRAW;

import jj2000.j2k.encoder.Encoder;
import jj2000.j2k.image.*;
import jj2000.j2k.util.ParameterList;
import gov.nih.mipav.model.file.rawjp2.*;


/**
 * Jpeg2K reader/ writer for 3D images. 
 * @version  0.1, Mar 2007
 * @author   Dzung Nguyen, Nam Nguyen
 * @see      FileIO
 * @see      FileInfoJP2
 * @see      FileRaw
 */
public class FileJP2 extends FileBase implements ActionListener{
	
	/** The identifier for the data type, as signed 8 bits. */
	//private final static int TYPE_BYTE = 0;
	
	/** The identifier for the data type, as signed 16 bits. */
	//private final static int TYPE_SHORT = 1;
	
	/** The identifier for the data type, as signed 32 bits. */
	//private final static int TYPE_INT = 3;
	
	/** The identifier for the data type, as float */
	//private final static int TYPE_FLOAT = 4;

    /** DOCUMENT ME! */
    private boolean endianess;

    /** File directory. */
    private String fileDir;

    /** File Info. */
    private FileInfoJP2 fileInfo;

    /** Model Image. */
    private ModelImage image;
    
    /** File name. */
    private String fileName;

    /** DOCUMENT ME! */
    private int[] intBuffer = null;
    
    /** The width of image */
    //private int w;
    
    /** The height of image */
    //private int h;
    private JDialog advDecOptDialog;
    private JTextField decRateField;   
    public JButton decOkBut;
    private JButton decDefBut, decCancelBut;
    private JCheckBox showDecInf, decResChkBox, csIgnoreBox;    
    private JRadioButton decParsOn, decParsOff;
    private JRadioButton codeStrInfoOn, codeStrInfoOff;    
    private JRadioButton cerOn, cerOff, cverberOn;
    private JRadioButton cverberOff, noRoiOn, noRoiOff;
    private JTextField decResTf;//,selectedKeyTF;
    private DecimalFormat f = new DecimalFormat("##0.000");
    int nStartFrameDec , nEndFrameDec; 
    private JTextField decFrameStart = null,decFrameEnd = null ;    
    
    private ViewJProgressBar progressBar;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Tiff reader/writer constructor.
     *
     * @param      fileName  file name
     * @param      fileDir   file directory
     *
     * @exception  IOException  if there is an error making the file
     */
    public FileJP2(String fileName, String fileDir, ViewJProgressBar progressBar) throws IOException {
        this.fileName = fileName;
        this.fileDir = fileDir;
        this.progressBar = progressBar;
    }
    
    
    /**
     * Tiff reader/writer constructor.
     *
     * @param      fileName  file name
     * @param      fileDir   file directory
     *
     * @exception  IOException  if there is an error making the file
     */
    public FileJP2() throws IOException {

    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for cleanup. Calls the <code>finalize</code> method for existing elements, closes any open
     * files and sets other elements to <code>null</code>.
     */
    public void finalize() {

        intBuffer = null;

        try {
            super.finalize();
        } catch (Throwable er) { }
    }

    /**
     * Accessor that returns the file info.
     *
     * @return  FileInfoBase containing the file info
     */
    public FileInfoBase getFileInfo() {
        return fileInfo;
    }

    /**
     * Accessor that returns the image buffer.
     *
     * @return  buffer of image.
     */
    public int[] getImageBuffer() {
        return intBuffer;
    }
    
    
    
    public int[] decodeImageData(byte[] imageData, int imageType) {
    	BEByteArrayInputStream inStream;
        BlkImgDataSrc slc;
        //int dataType;
        //DecoderRAW dec;
        ParameterList defpl = new ParameterList();
        
        //createDecGui();
        //advDecOptDialog.setVisible(true);
        //if (advDecOptDialog==null) {
        //	return null;
        //}
        
	  	String[][] param = DecoderRAW.getAllParameters();
	    for (int i=param.length-1; i>=0; i--) {
	  	    if(param[i][3]!=null){
	  		defpl.put(param[i][0],param[i][3]);
	  		}
	  	}
	    ParameterList pl = new ParameterList(defpl);
	    //pl.put("nocolorspace","on");
	    pl.put("verbose","off");
	    //dec = new DecoderRAW(pl);
	    DecoderRAWColor dec = new DecoderRAWColor(pl);
	    inStream = new BEByteArrayInputStream(imageData);
	    slc = dec.run1Slice(inStream);
	    Coord nT = slc.getNumTiles(null);
	    DataBlkInt db = new DataBlkInt();
	    /*dataType = db.getDataType();
	    switch (dataType) {
	        case TYPE_BYTE:
	    	    Preferences.debug("decodeImageData BYTE\n", Preferences.DEBUG_FILEIO);
	    	    break;
	        case TYPE_SHORT:
	        	Preferences.debug("decodeImageData SHORT\n", Preferences.DEBUG_FILEIO);
	        	break;
	        case TYPE_INT:
	        	Preferences.debug("decodeImageData INT\n", Preferences.DEBUG_FILEIO);
	        	break;
	        case TYPE_FLOAT:
	        	Preferences.debug("decodeImageData FLOAT\n", Preferences.DEBUG_FILEIO);
	        	break;
	        default:
	        	Preferences.debug("decodeImageData unrecognized data type\n", Preferences.DEBUG_FILEIO);
	    }*/
	    
	    int w = slc.getImgWidth();
        int h = slc.getImgHeight();
	 // Loop on vertical tiles
        DataBlkInt db0= null;
        DataBlkInt db1= null;
        DataBlkInt db2= null;
        int[] levShift = null;
        int fb[] = null;
        int cFactor=4;
        if(imageType == ModelStorageBase.ARGB) {
        	db0 = new DataBlkInt();
        	db1 = new DataBlkInt();
        	db2 = new DataBlkInt();
        	levShift = new int[3];
        	fb = new int[3];
        	fb[0] = slc.getFixedPoint(0);
            fb[1] = slc.getFixedPoint(1);
            fb[2] = slc.getFixedPoint(2);
        	levShift[0] = 1<< (slc.getNomRangeBits(0)-1);
            levShift[1] = 1<< (slc.getNomRangeBits(1)-1);
            levShift[2] = 1<< (slc.getNomRangeBits(2)-1);
        }
        else {
        	fb = new int[1];
        	fb[0] = slc.getFixedPoint(0);
        	levShift = new int[1];
        	levShift[0] = 1 << (slc.getNomRangeBits(0)-1);
        }
        int[] barr = null;
        for(int y=0; y<nT.y; y++){
            // Loop on horizontal tiles
            for(int x=0; x<nT.x; x++){
            	slc.setTile(x,y);
            	if(imageType == ModelStorageBase.ARGB) {
            		db.ulx = 0;
            		db.uly = 0;
            		db.w = w;
            		db.h = h;
            		if(db.data!=null && db.data.length<w*h) {
            			// A new one will be allocated by getInternCompData()
            			db.data = null;
            		}
            		// Request the data and make sure it is not
            		// progressive
//            		do {
//            			db = (DataBlkInt) src.getInternCompData(db,0);
//            		} while (db.progressive);
            		
            		if(db0.data!=null && db0.data.length<w*h) {
            			// A new one will be allocated by getInternCompData()
            			db0.data = null;
            		}
            		if(db1.data!=null && db1.data.length<w*h) {
            			// A new one will be allocated by getInternCompData()
            			db1.data = null;
            		}		
            		if(db2.data!=null && db2.data.length<w*h) {
            			// A new one will be allocated by getInternCompData()
            			db2.data = null;
            		}		
            		int dataLength;

            		
            		db0 = (DataBlkInt) slc.getInternCompData(db,0);
            		dataLength = db0.data.length;
            		barr = new int[cFactor * dataLength];
            		
            		if (fb[0]==0) {
            			for(int i=0;i<dataLength;i++){
            				barr[cFactor*i+1]=db0.data[i]+ levShift[0];	
            				db0.data[i] = db0.data[i] + levShift[0];
            			
            			}
            		}else{
            			for(int i=0;i<dataLength;i++){
            				barr[cFactor*i+1] = db0.data[i]>>>fb[0] + levShift[0];
            			}
            		}
            		
            		db1 = (DataBlkInt) slc.getInternCompData(db,1);
            		if (fb[1]==0) {
            			for(int i=0;i<dataLength;i++){
            				barr[cFactor*i+2]=db1.data[i]+ levShift[1];				
            				db1.data[i] = db1.data[i] + levShift[1];
            			}
            		}else{
            			for(int i=0;i<dataLength;i++){
            				barr[cFactor*i+2] = db1.data[i]>>>fb[1] + levShift[1];
            			}
            		}
            	
            		db2 = (DataBlkInt) slc.getInternCompData(db,2);
            		if (fb[2]==0) {
            			for(int i=0;i<dataLength;i++){
            				barr[cFactor*i+3]=db2.data[i]+ levShift[2];				
            				db2.data[i] = db2.data[i] + levShift[2];
            			}
            		}else{
            			for(int i=0;i<dataLength;i++){
            				barr[cFactor*i+3] = db2.data[i]>>>fb[2] + levShift[2];
            			}
            		}		
            	}else {
            	
	            	db.ulx = 0;
	                db.uly = 0;
	                db.w = w;
	                db.h = h;
	            
	                if(db.data!=null && db.data.length<w*h) {
	                    // A new one will be allocated by getInternCompData()
	                    db.data = null;
	                }
	                // Request the data and make sure it is not
	                // progressive
	                do {
	                    db = (DataBlkInt) slc.getInternCompData(db,0);
	                } while (db.progressive);
	                int dataLength = db.data.length;
	                if (fb[0]==0) {
            			for(int i=0;i<dataLength;i++){	
            				db.data[i] = db.data[i] + levShift[0];
            			
            			}
            		}else{
            			for(int i=0;i<dataLength;i++){
            				db.data[i] = db.data[i]>>>fb[0] + levShift[0];
            			}
            		}
            	}
	
            } // End loop on horizontal tiles            
        } // End loop on vertical tiles
	    
	    
	    
	    
	    
	    
	    //db = (DataBlkInt) slc.getInternCompData(db,0);
        int[] vals = null;
        if(imageType == ModelStorageBase.ARGB) {
        	vals = barr;
        }else {
        	vals = db.data;
        }
	
	    return vals;
    }

    /**
     * Reads the JP2 header which indicates the number of slices and each slice's size.
     * It then reads all the encoded slice. This method then opens a Model of an image and imports the the images one slice at a
     * time. Image slices are separated by an IFD.
     *
     * @param      multiFile  <code>true</code> if a set of files each containing a separate 2D image is present <code>
     *                        false</code> if one file with either a 2D image or a stack of 2D images
     * @param      one        <code>true</code> if only want to read in one image of the 3D set
     *
     * @return     returns the image
     *
     * @exception  IOException  if there is an error reading the file
     */
    public ModelImage readImage(boolean multiFile, boolean one) throws IOException {
    	
//    	int[] dimExtents = {256,256,124};
    	int[] dimExtents = {256,256,124,1};
    	int[] dimExtents_2d = new int[2];
    	int[] dimExtents_3d = new int[3];
    	// Get input file
        String infile = fileDir + fileName;
//        System.out.println("Hello, world!");
        
        // Get output files
//        outfile = pl.getParameter("o");    	
        RAWJP2Header rawhd = new RAWJP2Header();
        FileInputStream f = rawhd.readRawJP2Header(infile);

        BEByteArrayInputStream inStream;
 //       BlkImgDataSrc slc;
//        DataBlkInt blk = new DataBlkInt();
        
        
        ImgWriterRAW wr = null;

        if (f == null) {
        	System.out.println("Error opening input file!");
        	return null;
        }
        //int nslice = rawhd.getNumOfSlices();

        boolean is2D = rawhd.getIs2D();
        
        
        dimExtents = rawhd.getImgExtents();
        
        if(is2D) {
        	dimExtents_2d[0] = rawhd.getImgExtents()[0];
        	dimExtents_2d[1] = rawhd.getImgExtents()[1];
        }
        if(rawhd.getImgExtents()[3] == 1) {
        	dimExtents_3d[0] = rawhd.getImgExtents()[0];
        	dimExtents_3d[1] = rawhd.getImgExtents()[1];
        	dimExtents_3d[2] = rawhd.getImgExtents()[2];
        }

        int imgType = rawhd.getImgType();
        int imgModality = rawhd.getImgModality();
        int imgOrientation= rawhd.getImgOrientation();
        //float[] imgRes = rawhd.getImgResolution();
        float[] imgRes_2d = new float[2];
        
        if(is2D) {
        	imgRes_2d[0] = rawhd.getImgResolution()[0];
        	imgRes_2d[1] = rawhd.getImgResolution()[1];
        }
        
        
        
        
        
//        dimExtents[2] = nslice;

        nStartFrameDec = 0;
        if(is2D) {
        	nEndFrameDec = 0;
        }else {
        	nEndFrameDec = dimExtents[2]-1;
        }
    
//---        
    //    createDecGui();
   //     advDecOptDialog.setVisible(true);
    //    if (advDecOptDialog==null) {
     //   	return null;
     //   }
    //    nStartFrameDec = Integer.parseInt(decFrameStart.getText());
    //    nEndFrameDec = Integer.parseInt(decFrameEnd.getText());
//---
        fileInfo = new FileInfoJP2(fileName, fileDir, FileUtility.JP2);
        fileInfo.setEndianess(endianess);
        if(is2D) {
        	fileInfo.setExtents(dimExtents_2d);
        }else {
        	if(rawhd.getImgExtents()[3] == 1) {
        		fileInfo.setExtents(dimExtents_3d); 
        	}else {
        		fileInfo.setExtents(dimExtents);
        	}
        }
//        fileInfo.s


        //DecoderRAW dec;
        ParameterList defpl = new ParameterList();
        
	  	String[][] param = DecoderRAW.getAllParameters();
	    for (int i=param.length-1; i>=0; i--) {
	  	    if(param[i][3]!=null){
	  		defpl.put(param[i][0],param[i][3]);
	  		}
	  	}
        
	    ParameterList pl = new ParameterList(defpl);
        pl.put("i", infile);
        pl.put("o", "outfile.raw");

	    //pl.put("nocolorspace","off");
        //pl.put("colorspace","on");  //color test
	    pl.put("verbose","off");
	    
	    //dec = new DecoderRAW(pl);
	    DecoderRAWColor dec = new DecoderRAWColor(pl);
	    if(is2D) {
	    	image= new ModelImage(imgType, dimExtents_2d,infile);
	    }else {
	    	if(rawhd.getImgExtents()[3] == 1) {
	    		image= new ModelImage(imgType, dimExtents_3d,infile);
	    	}else {
	    		image= new ModelImage(imgType, dimExtents,infile);
	    	}
	    }
	    
	    
	    image.setType(imgType);
	    if(is2D) {
	    	image.setExtents(dimExtents_2d);
	    }else {
	    	if(rawhd.getImgExtents()[3] == 1) {
	    		image.setExtents(dimExtents_3d);
	    	}else{
	    		image.setExtents(dimExtents);
	    	}
	    }
//	    image.setResolutions(0, imgRes);
	    image.setImageModality(imgModality);
	    image.setImageOrientation(imgOrientation);
	    
	    byte[] b;
	    int si;
        try {
        	b =new byte[1];
        	//File outf = new File(outfile); 
        	// skip frames, jump to the start frame.

        	long nBytesToSkip = 0;
        	
        	for (si = 0; si<nStartFrameDec;si++){

        		nBytesToSkip += rawhd.getSize(si);
        	}
        	f.skip(nBytesToSkip);
        	BlkImgDataSrc slc;
            for (si=nStartFrameDec;si<=nEndFrameDec;si++){
            	b=new byte[rawhd.getSize(si)];

            	f.read(b);
            	inStream = new BEByteArrayInputStream(b);
//            	JOptionPane.showMessageDialog(null, "Size of slice:" + Integer.toString(inStream.length()), "Debug", JOptionPane.INFORMATION_MESSAGE);
            	slc = dec.run1Slice(inStream);

            	if (si==nStartFrameDec){
            		if (imgType==ModelStorageBase.ARGB){  
            			wr = new ImgWriterRAWColor(image,slc,false);
            		}else{
            			wr = new ImgWriterRAW(image,slc,true);            			
            		}
            		wr.setImgType(imgType);
            	}else {
            		wr.setOffset(si-nStartFrameDec);
            		wr.setSrc(slc);
            	}
        		wr.writeSlice();
            }
            f.close();
        } catch(IOException e) {
        }
	    
	    /* OLD try-catch
	    try {
        	b =new byte[1];
        	//File outf = new File(outfile); 
        	
            for (int si=0;si<nslice;si++){

            	b=new byte[rawhd.getSize(si)];
            	f.read(b);
            	inStream = new BEByteArrayInputStream(b);
            	System.out.println("Size of slice:" + Integer.toString(inStream.length()));
            	slc = dec.run1Slice(inStream);
            	if (si==0){
            		wr = new ImgWriterRAW(image,slc ,1,false);
            	}else {
            		wr.setOffset(si);
            		wr.setSrc(slc);
            	}
        		wr.writeSlice();
            }
        } catch(IOException e) {
        }
*/
 //   	image.importData(0, buf, /*mmFlag*/ false);
        image.calcMinMax();
//		image.setType(imgType);
    	
    	return image;
    }
    
    /**
     * Accessor to set the file dir (used when reading TIFF multiFile).
     *
     * @param  fDir  file dir of image to read.
     */
    public void setFileDir(String fDir) {
        fileDir = fDir;
    }

    /**
     * Accessor to set the file name (used when reading TIFF multiFile).
     *
     * @param  fName  file name of image to read.
     */
    public void setFileName(String fName) {
        fileName = fName;
    }

    
    public void writeImage(ModelImage image){
		
    	int[] imgExtents;
    	int imgType;

    	String outfile = fileDir + fileName;

        ParameterList defpl = new ParameterList();
        String[][] param = Encoder.getAllParameters();
        for (int i=param.length-1; i>=0; i--) {
        	if(param[i][3]!=null)
        		defpl.put(param[i][0],param[i][3]);
        }

        imgType = image.getType();
        
        // Create parameter list using defaults
        ParameterList pl = new ParameterList(defpl);           
        pl.put("i", "something.raw"); // to make the ncoder happy
        pl.put("o", outfile);
        pl.put("lossless","on");
        pl.put("verbose","off");
        
        if (imgType==ModelStorageBase.ARGB){
        	pl.put("Mct","on");
        } else {
        	pl.put("Mct","off");	
        }

        pl.put("file_format", "on");            
        // Get the dimension of image and number of slice
        imgExtents = image.getExtents();    

        if (imgType==ModelStorageBase.ARGB){        
        	EncoderRAWColor encRAW = new EncoderRAWColor(pl,image);
        	if(image.getNDims() == 2) {
        		encRAW.runAllSlices(0, 0,true, progressBar);//full mode: 0 -> imgExtents[2]-1  
        	}else {
        		encRAW.runAllSlices(0, imgExtents[2]-1,true, progressBar);//full mode: 0 -> imgExtents[2]-1  
        	}
        } else {       	
        	EncoderRAW encRAW = new EncoderRAW(pl,image);
        	if(image.getNDims() == 2) {
        		encRAW.runAllSlices(0, 0,true, progressBar);
        	}else {
        		encRAW.runAllSlices(0, imgExtents[2]-1,true, progressBar);
        	}
        }
	}
    
    
    
    
    
    
    
    
    
    
    
    
    

    /**
     * This function is not used. Instead, use the RAWJP2Header class.
     * Writes JP2 starting file header.
     *
     * @throws  IOException  DOCUMENT ME!
     */
    
    @SuppressWarnings("unused")
    private void writeHeader() throws IOException {
/*        boolean endianess = image.getFileInfo(0).getEndianess();
        byte[] hdr = new byte[8];

        if (endianess == true) {
            hdr[0] = 77; // Big endian.
            hdr[1] = 77;
            hdr[2] = 0; // 42 magic number
            hdr[3] = 42;
            hdr[4] = 0; // 8 (offset to first IFD)
            hdr[5] = 0;
            hdr[6] = 0;
            hdr[7] = 8;
        } else {
            hdr[0] = 73; // "49" little endian
            hdr[1] = 73;
            hdr[2] = 42; // 42 magic number
            hdr[3] = 0;
            hdr[4] = 8; // 8 (offset to first IFD)
            hdr[5] = 0;
            hdr[6] = 0;
            hdr[7] = 0;
        }

        raFile.write(hdr);
*/    }
    
    public ModelLUT getModelLUT() {
        return null;
    }
    
    @SuppressWarnings("unused")
    private void createDecGui() {
    	
    	
//    	advDecOptDialog = new JDialog(mainFrame,"Decoding Parameters");
    	advDecOptDialog = new JDialog();
    	
            // Panel for dialog box
    	JPanel decDialogPane = new JPanel(new BorderLayout());
    	advDecOptDialog.setContentPane(decDialogPane);
    	
    	JPanel buttonsPane = new JPanel();
    	decOkBut = new JButton("OK");
    	decOkBut.addActionListener(this);
    	buttonsPane.add(decOkBut);

    	decDefBut = new JButton("Default");
    	decDefBut.addActionListener(this);
    	buttonsPane.add(decDefBut);

    	decCancelBut = new JButton("Cancel");
    	decCancelBut.addActionListener(this);
    	buttonsPane.add(decCancelBut);

        decRateField = new JTextField(f.format(100),5);
        decRateField.addActionListener(this);
        
        decFrameStart = new JTextField("0",2);
        decFrameEnd = new JTextField("0",3);
        
    	decDialogPane.add(buttonsPane,BorderLayout.SOUTH);
    	
    	//****** General Options Tab ***********
    	JPanel genTab = new JPanel(new BorderLayout());
    	JPanel genTabSuper = new JPanel(new GridLayout(3,0));

            // Show decoding info
            JPanel showInfPan = new JPanel();
     
            showDecInf = new JCheckBox("Display decoding info",false);
            showDecInf.setToolTipText("Show decoding info and warning messages");
            showInfPan.add(showDecInf, null);
    	// Rate option
    	JPanel genTabPane2 = new JPanel();

    	genTabPane2.add(new JLabel("Decoding rate:"));
    	genTabPane2.add(decRateField);	
    	genTabPane2.add(new JLabel("bpp (per frame)"));
    	JPanel genTabPane3 = new JPanel();
    	genTabPane3.add(new JLabel("Start from frame #:"), null);	
    	genTabPane3.add(decFrameStart);
    	genTabPane3.add(new JLabel("to frame #:"));
    	genTabPane3.add(decFrameEnd);
        decFrameStart.setText(Integer.toString(nStartFrameDec));
        decFrameEnd.setText(Integer.toString(nEndFrameDec));
    	
//    	genTabPane3.add(getDecFrameStart(), null);
    	
    	genTabSuper.add(genTabPane2);
    	genTabSuper.add(genTabPane3);
    	genTabSuper.add(showInfPan);
    	genTab.add(genTabSuper,BorderLayout.CENTER);
    	
    	// ****** Advanced Otions Tab *********
    	JPanel advTab = new JPanel(new BorderLayout());
    	JPanel advTabSuper = new JPanel(new GridLayout(3,1));

    	// Parsing option
    	JPanel advTab2 = new JPanel();
    	advTab2.add(new JLabel("Parsing:"));
    	decParsOn = new JRadioButton("on",true);
    	decParsOff = new JRadioButton("off",false);
    	ButtonGroup parsingGrp = new ButtonGroup();
    	parsingGrp.add(decParsOn);
    	parsingGrp.add(decParsOff);
    	advTab2.add(decParsOn);
    	advTab2.add(decParsOff);
    	advTabSuper.add(advTab2);
    	// CodeStream Info
    	JPanel advTab3 = new JPanel();
    	advTab3.add(new JLabel("Codestream Info:"));
    	codeStrInfoOn = new JRadioButton("on",false);
    	codeStrInfoOff = new JRadioButton("off",true);
    	ButtonGroup codeStrInfoGrp = new ButtonGroup();
    	codeStrInfoGrp.add(codeStrInfoOn);
    	codeStrInfoGrp.add(codeStrInfoOff);
    	advTab3.add(codeStrInfoOn);
    	advTab3.add(codeStrInfoOff);
    	advTabSuper.add(advTab3);
    	//resolution level option
    	JPanel advTab4 = new JPanel();
    	decResChkBox = new JCheckBox("Resolution level:",false);
    	advTab4.add(decResChkBox);
    	decResTf = new JTextField("",5);
    	advTab4.add(decResTf);
    	advTabSuper.add(advTab4);

    	advTab.add(advTabSuper,BorderLayout.NORTH);
    	

    	//******* Entropy Decoding option *******
    	JPanel entropyDecTab = new JPanel(new BorderLayout());
    	JPanel entrDecSuper = new JPanel(new GridLayout(2,1));
    	
    	//error detecttion option
    	JPanel entrDec1 = new JPanel();
    	entrDec1.add(new JLabel("Detect synchronization errors:"));
    	cerOn = new JRadioButton("on",true);
    	cerOff = new JRadioButton("off",false);

    	ButtonGroup cerGrp = new ButtonGroup();
    	cerGrp.add(cerOn);
    	cerGrp.add(cerOff);
    	entrDec1.add(cerOn);
    	entrDec1.add(cerOff);
    	entrDecSuper.add(entrDec1);

    	//cverber option
    	JPanel entrDec2 = new JPanel();
    	entrDec2.add(new JLabel("Display synchronization errors:"));
    	cverberOn = new JRadioButton("on",true);
    	cverberOff = new JRadioButton("off",false);
    	ButtonGroup cverberGrp = new ButtonGroup();
    	cverberGrp.add(cverberOn);
    	cverberGrp.add(cverberOff);
    	entrDec2.add(cverberOn);
    	entrDec2.add(cverberOff);
    	entrDecSuper.add(entrDec2);
    	
    	entropyDecTab.add(entrDecSuper,BorderLayout.NORTH);

    	//********* Dec ROI ********
    	JPanel decROITab = new JPanel(new BorderLayout());
    	JPanel noRoiPan = new JPanel();
    	noRoiPan.add(new JLabel("Process ROI : "));
//    	noRoiOn = new JRadioButton("on",true);
//    	noRoiOff = new JRadioButton("off",false);
    	noRoiOn = new JRadioButton("on",false);
    	noRoiOff = new JRadioButton("off",true);

    	ButtonGroup noRoiGrp = new ButtonGroup();
    	noRoiGrp.add(noRoiOn);
    	noRoiGrp.add(noRoiOff);
    	noRoiPan.add(noRoiOn);
    	noRoiPan.add(noRoiOff);
    	decROITab.add(noRoiPan,BorderLayout.CENTER);

    	// *************** Color space ***************
    	JPanel csTab = new JPanel();
    	csIgnoreBox = new JCheckBox("Ignore color space information");
    	csTab.add(csIgnoreBox);
    /*
    	// *************** Security ***************
    	JPanel secTab = new JPanel();
    	secTab.add(new JLabel("Public key"));
    	selectedKeyTF = new JTextField("",10);

    	rsaPubKey = rsaSupport.getRawRSAPubKey();
    	selectedKeyTF.setText(rsaSupport.getPubKeyName());
    	selectedKeyTF.setEnabled(false);
    	secTab.add(selectedKeyTF);
    	JButton selectPubKey = new JButton("Select Key");
    	selectPubKey.addActionListener(new ActionListener() {
    		public void actionPerformed(ActionEvent e) {
    		    rsaSupport.createMngKeyUI(mainFrame,RSASupport.
    					      DISPLAY_PUBLIC);
    		    rsaPubKey = rsaSupport.getRawRSAPubKey();
    		    selectedKeyTF.setText(rsaSupport.getPubKeyName());
    		}
    	    });
    	secTab.add(selectPubKey);
//     	JPanel secTab = new JPanel();
//     	secTab.add(new JLabel("Public key:"));
//     	secSeedTF = new JTextField("-1",5);
//     	secTab.add(secSeedTF);
    */	
    	// *************** Tabbed pane ***************
    	JTabbedPane tabPane = new JTabbedPane();
    		
    	tabPane.add("General",genTab);
    	tabPane.add("Advanced",advTab);
    	tabPane.add("Entropy Coder",entropyDecTab);
    	tabPane.add("Region of Interest",decROITab);
    	tabPane.add("Colorspace",csTab);
//    	tabPane.add("Security",secTab);

    	decDialogPane.add(tabPane,BorderLayout.CENTER);

    	advDecOptDialog.setModal(true);
    	advDecOptDialog.setTitle("Decode Parameters");
    	advDecOptDialog.addWindowListener(new java.awt.event.WindowAdapter() {   
    		public void windowClosing(java.awt.event.WindowEvent e) {    
    			System.out.println("windowClosing()"); // TODO Auto-generated Event stub windowClosing()
    		}
    		public void windowOpened(java.awt.event.WindowEvent e) {
    			System.out.println("windowOpened()"); // TODO Auto-generated Event stub windowOpened()
    		}
    	});
    	advDecOptDialog.addWindowListener(new java.awt.event.WindowAdapter() {
    		public void windowOpened(java.awt.event.WindowEvent e) {
    			System.out.println("windowOpened()"); // TODO Auto-generated Event stub windowOpened()
    		}
    	});
    	advDecOptDialog.addWindowListener(new java.awt.event.WindowAdapter() {
    		public void windowOpened(java.awt.event.WindowEvent e) {
    			System.out.println("windowOpened()"); // TODO Auto-generated Event stub windowOpened()
    		}
    	});
    	advDecOptDialog.addWindowListener(new java.awt.event.WindowAdapter() {
    		public void windowOpened(java.awt.event.WindowEvent e) {
    			System.out.println("windowOpened()"); // TODO Auto-generated Event stub windowOpened()
    		}
    	});
    	advDecOptDialog.addWindowListener(new java.awt.event.WindowAdapter() {
    		public void windowOpened(java.awt.event.WindowEvent e) {
    			System.out.println("windowOpened()"); // TODO Auto-generated Event stub windowOpened()
    		}
    	});
    	advDecOptDialog.addWindowListener(new java.awt.event.WindowAdapter() {
    		public void windowOpened(java.awt.event.WindowEvent e) {
    			System.out.println("windowOpened()"); // TODO Auto-generated Event stub windowOpened()
    		}
    	});
    	advDecOptDialog.pack();
     //       advDecOptDialog.setLocationRelativeTo(mainFrame);
        }
    public void actionPerformed(ActionEvent e){
    	Object o = e.getSource();

    	if(o==decDefBut) { // Default values for advanced options
            showDecInf.setSelected(false);
            cerOn.setSelected(true);
            cverberOn.setSelected(true);
            codeStrInfoOff.setSelected(true);
            decParsOn.setSelected(true);
            noRoiOn.setSelected(true);
		    decRateField.setText("8.0000");
		    decResChkBox.setSelected(false);
		    decResTf.setText("");
        } else if(o==decCancelBut) { // Cancel advanced decoding
            advDecOptDialog.setVisible(false);
            advDecOptDialog = null;
        } else if(o==decOkBut){
            advDecOptDialog.setVisible(false);        	
        }
    }
    
}
