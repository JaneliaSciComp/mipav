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
import java.util.*;

import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
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

    /** DOCUMENT ME! */
    private byte[] byteBuffer = null;

    /** DOCUMENT ME! */
    private byte[] dateTime;

    /** doTile: true if tiles are used. */
    private boolean doTile = false;

    /** DOCUMENT ME! */
    private double[] doubleBuffer;

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
    private byte[] hostComputer;

    /** DOCUMENT ME! */
    private byte[] imageDescription;

    /** DOCUMENT ME! */
    //private int imageSlice = 0;

    /** DOCUMENT ME! */
    private float[] imgBuffer = null;

    /** DOCUMENT ME! */
    private int[] intBuffer = null;

    /** DOCUMENT ME! */
    private float[] imgResols;

    /** DOCUMENT ME! */
    private byte[] software;

    
    /** DOCUMENT ME! */
    private double tRes = 1.0;

    /** DOCUMENT ME! */
    private ViewUserInterface UI;

    /** DOCUMENT ME! */
    private int xDim = 0;

    /** DOCUMENT ME! */
    private int yDim = 0;

    /** DOCUMENT ME! */
    private double zRes = 1.0;
    
    /** Save the compressed image */
    private File outFile;
    
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

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Tiff reader/writer constructor.
     *
     * @param      fileName  file name
     * @param      fileDir   file directory
     *
     * @exception  IOException  if there is an error making the file
     */
    public FileJP2(String fileName, String fileDir) throws IOException {
        UI = ViewUserInterface.getReference();
        this.fileName = fileName;
        this.fileDir = fileDir;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for cleanup. Calls the <code>finalize</code> method for existing elements, closes any open
     * files and sets other elements to <code>null</code>.
     */
    public void finalize() {

        imgBuffer = null;
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
    	// Get input file
        String infile = fileDir + fileName;
//        System.out.println("Hello, world!");
        
        // Get output files
//        outfile = pl.getParameter("o");    	
        RAWJP2Header rawhd = new RAWJP2Header();
        FileInputStream f = rawhd.readRawJP2Header(infile);

        BEByteArrayInputStream inStream;
        BlkImgDataSrc slc;
//        DataBlkInt blk = new DataBlkInt();
        
        
        ImgWriterRAW wr = null;

        if (f == null) {
        	System.out.println("Error opening input file!");
        	return null;
        }
        int nslice = rawhd.getNumOfSlices();

        dimExtents = rawhd.getImgExtents();
        int imgType = rawhd.getImgType();
        int imgModality = rawhd.getImgModality();
        int imgOrientation= rawhd.getImgOrientation();
        float[] imgRes = rawhd.getImgResolution(); 
        
        
        
//        dimExtents[2] = nslice;

        nStartFrameDec = 0;
        nEndFrameDec = dimExtents[2]-1;
        System.out.println(String.valueOf(nslice));        
//---        
        createDecGui();
        advDecOptDialog.setVisible(true);
        if (advDecOptDialog==null) {
        	return null;
        }
        nStartFrameDec = Integer.parseInt(decFrameStart.getText());
        nEndFrameDec = Integer.parseInt(decFrameEnd.getText());
//---
        fileInfo = new FileInfoJP2(fileName, fileDir, FileUtility.JP2);
        fileInfo.setEndianess(endianess);
        fileInfo.setExtents(dimExtents);
//        fileInfo.s

        System.out.println("Num of slices:" + Integer.toString(nslice)); 
        DecoderRAW dec;
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

	    pl.put("nocolorspace","on");
	    pl.put("verbose","off");
	    
	    dec = new DecoderRAW(pl);

	    image= new ModelImage(ModelStorageBase.INTEGER, dimExtents,infile);
	    
	    image.setType(imgType);
	    image.setExtents(dimExtents);
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
            for (si=nStartFrameDec;si<=nEndFrameDec;si++){

            	b=new byte[rawhd.getSize(si)];
            	f.read(b);
            	inStream = new BEByteArrayInputStream(b);
            	System.out.println("Size of slice:" + Integer.toString(inStream.length()));
//            	JOptionPane.showMessageDialog(null, "Size of slice:" + Integer.toString(inStream.length()), "Debug", JOptionPane.INFORMATION_MESSAGE);
            	slc = dec.run1Slice(inStream);

            	if (si==nStartFrameDec){
            		wr = new ImgWriterRAW(image,slc ,0,true);
            	}else {
            		wr.setOffset(si-nStartFrameDec);
            		wr.setSrc(slc);
            	}
        		wr.writeSlice();
            }
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
		image.setType(imgType);
    	
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

    /**
     * This method writes a tiff image file.
     *
     * @param      image    image model where the data is stored.
     * @param      LUT      LUT to be saved with image if not null.
     * @param      options  options to be used to write out the image
     *
     * @exception  IOException  if there is an error writing the file.
     */
    public void writeImage(ModelImage image, ModelLUT LUT, FileWriteOptions options) throws IOException {
          //raFile.close();
    	EncoderRAW encRAW;        
    	int[] imgExtents;
    	String outfile = fileDir + fileName;
        this.image = image;
        
        ParameterList defpl = new ParameterList();
    	String[][] param = Encoder.getAllParameters();
    	for (int i=param.length-1; i>=0; i--) {
    	    if(param[i][3]!=null)
    		defpl.put(param[i][0],param[i][3]);
            }

    	// Create parameter list using defaults
            ParameterList pl = new ParameterList(defpl);           
            pl.put("i", "something.raw"); // to make the EncoderRAW happy
            pl.put("o", outfile);
            pl.put("lossless","on");
            pl.put("verbose","off");
            pl.put("Mct","off");
            pl.put("file_format", "on");            
        // Get the dimension of image and number of slice
        imgExtents = image.getExtents();    
        //this.w = imgExtents[0];
        //this.h = imgExtents[1];
        //this.imageSlice = imgExtents[2];
        
        //img = new ImgReaderRAW(image);
        
       encRAW = new EncoderRAW(pl,image);     
       
       // Compress image from BeginSlice to EndSlice
       encRAW.runAllSlices(options.getBeginSlice(), options.getEndSlice(),true);
       JOptionPane.showMessageDialog(null, "Done.", "Results", JOptionPane.INFORMATION_MESSAGE);       
    }

    /**
     * This function is not used. Instead, use the RAWJP2Header class.
     * Writes JP2 starting file header.
     *
     * @throws  IOException  DOCUMENT ME!
     */
    
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
