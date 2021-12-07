import java.io.BufferedWriter;

import java.io.File;

import java.io.FileWriter;
import java.io.IOException;

import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.HashMap;

import java.util.Vector;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.file.FileUtility;
import gov.nih.mipav.model.file.FileXML;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.view.MipavUtil;

public class PlugInAlgorithmCreateXML extends AlgorithmBase {

	// ~ Instance fields ---------------------------------------------------------------------------------------
	/** Files and folders selected by the user */	
	private File[] selectedFiles;
	
	/**All the XML files that have been written by this algorithm. */
	private ArrayList<String> xmlList;

	/**Map keyed to exact xml tags for easy provenance referencing.*/
	private HashMap<String, String> textFieldsMap;

	private String imageCopyDir;

	//	~ Constructors -----------------------------------------------------------------------------------------
	public PlugInAlgorithmCreateXML(File[] inputFiles, HashMap<String, String> textFieldsMap, String imageCopyDir) {
		this.selectedFiles = inputFiles;
		this.xmlList = new ArrayList<String>();
		this.textFieldsMap = textFieldsMap;
		this.imageCopyDir = imageCopyDir;
	}
	
	//  ~ Methods ----------------------------------------------------------------------------------------------
	
	/**
     * Prepares this class for destruction.
     */
    public void finalize() {
        selectedFiles = null;      
    }
	
	@Override
	public void runAlgorithm() {
		if (selectedFiles == null) {
    		displayError("Selected file is null.");
    		return;
    	}
		
		fireProgressStateChanged(70, null, "Processing files");
		writeXML(selectedFiles);
		
		System.out.println("Finished reading files");
		
	}
	
	public ArrayList<String> getXmlList() {
		return xmlList;
	}

	/**
	 * Method for writing files to XML using dataset schema.
	 * @param f is not a directory
	 * @return
	 */
	private boolean writeXML(File[] f) {
		FileIO io = new FileIO();
		ModelImage m = io.readImage(f[0].getName(), f[0].getParentFile().getAbsolutePath()+File.separator, f.length > 1, null);
		FileImageXML im = new FileImageXML(m, f.length > 1, f[0]);
		
		try {
			boolean success =  im.writeXML();
			if(m != null) {
				m.disposeLocal();
				m = null;
			}
			return success;
		} catch (Exception e) {
			e.printStackTrace();
			if(m != null) {
				m.disposeLocal();
				m = null;
			}
			return false;
		}
		
	}
	
	private class FileImageXML extends FileXML {

		//~ Static fields/initializers -------------------------------------------------------------------------------------
		
		/** The W3C XML schema. */
	    private static final String W3C_XML_SCHEMA = "http://www.w3.org/2001/XMLSchema";

	    /** The charset the XML file is written in. */
	    private static final String XML_ENCODING = "UTF-8";

	    /** The XML version description header. */
	    private static final String XML_HEADER = "<?xml version=\"1.0\" encoding=\"" + XML_ENCODING + "\"?>";
	    
	    /** The XML header comment. */
	    private static final String IMAGE_HEADER = "<!-- Image file description-->";
	    
	    /** The XML file name*/
	    private static final String XML_NAME = "imageInfo.xml";
	    
	    private ModelImage image;

		private boolean isDir;
		
		private String pattern;
	    
		public FileImageXML(ModelImage m, boolean isDir, File f) {
			super(m.getFileInfo()[0].getFileName(), m.getFileInfo()[0].getFileDirectory());
			
			this.image = m;
			this.isDir = isDir;
			
			if(isDir)
				pattern = constructPattern();
		}
		
		private String constructPattern() {
			File[] fileList = selectedFiles;
			//only consider the files
			int fileCount = 0;	
			for(int i=0; i<fileList.length; i++) {
				if(fileList[i].isFile()) {
					fileCount++;
				}
			}
			String[] strList = new String[fileCount];
			int index=0;
			for(int i=0; i<fileList.length; i++) {
				if(fileList[i].isFile()) {
					strList[index] = fileList[i].getName();
					index++;
				}
			}
			
			if(strList.length == 0) 
				return null;
			
			boolean allExt = true;
			for(int i=0; i<strList.length; i++) {
				if(strList[i].indexOf(".") == -1) {
					allExt = false;
				}
			}
			if(allExt) {
				String extCompare = strList[0].substring(strList[0].indexOf(".")+1);
				boolean allAgree = true;
				for(int i=1; i<strList.length; i++) {
					if(!extCompare.equals(strList[i].substring(strList[i].indexOf(".")+1))) {
						allAgree = false;
					}
				}
				if(allAgree) {
					//files are matched by extension
					System.out.println("Found the following pattern: *."+extCompare);
					return "*."+extCompare;
				}
			}
			//at this point, either the images had no extension or their extensions didn't match
			//will instead return string with common elements plus a "?" for each possibly filled
			//character (up to length of the largest filename in the series)
			String toCompare = strList[0];
			int smallestMatch = strList[0].length(), largestRegion = strList[0].length();
			try {
				for(int i=1; i<strList.length; i++) {
					if(strList[i].length() > largestRegion) {
						largestRegion = strList[i].length();
					}
					int largestMatch = 0;
					for(int j=1; j<strList[i].length(); j++) {
						if(toCompare.substring(0, j).equals(strList[i].substring(0, j))) {
							largestMatch = j;
						}
					}
					//if the largest match possible for these two strings was less than the previous
					//largest match possible, then set the smallest match over all strings to the current 
					//largest match, since the smallest match over all strings should be the smallest largest 
					//set of characters.
					if(largestMatch < smallestMatch) {
						smallestMatch = largestMatch;
					}
				}
			} catch(StringIndexOutOfBoundsException e) {
				//No discernable pattern
				return "*.*";
			}
			
			//if next statement is true, then all files had in common points from 0 to
			//largest index, else no pattern could be found, files will be matched by name;
			if(smallestMatch > 0) {
				String toReturn = strList[0].substring(0, smallestMatch);
				for(int i=smallestMatch; i<largestRegion; i++) {
					toReturn += "?";
				}
				System.out.println("Found the following pattern: "+toReturn);
				return toReturn;
				
			} 
			
			return null;
		}
		
		public boolean writeXML() {
			try {
			bw = new BufferedWriter(new FileWriter(new File(imageCopyDir+File.separator+XML_NAME)));
			
			bw.write(XML_HEADER);
			bw.newLine();
			bw.write(IMAGE_HEADER);
			bw.newLine();
			
			openTag("dataset-description xmlns:xsi=\"" + W3C_XML_SCHEMA + "-instance\"", true);
			
			writeImageAttributes();
			
			writeDatasetFiles();
			
			writeProvenance();
			
			writeExtInfo();
			
			openTag("dataset-description", false);
			
			bw.flush();
			bw.close();
			
			xmlList.add(imageCopyDir+File.separator+XML_NAME);
			
			} catch(IOException e) {
				MipavUtil.displayError("Error writing XML file for "+image.getImageDirectory());
				e.printStackTrace();
			}
			
			return true;
			
		}
		
		private void writeImageAttributes() {
			openTag("image-attr", true);
			closedTag("ndims", Integer.toString(image.getNDims()));
			
			openTag("extents", true);
			closedTag("x", Integer.toString(image.getExtents()[0]));
			closedTag("y", Integer.toString(image.getExtents()[1]));
			if(image.getNDims() > 2)
				closedTag("z", Integer.toString(image.getExtents()[2]));
			if(image.getNDims() > 3)
				closedTag("t", Integer.toString(image.getExtents()[3]));
			openTag("extents", false);
			
			openTag("resolution", true);
			closedTag("xr", Float.toString(image.getResolutions(0)[0]));
			closedTag("yr", Float.toString(image.getResolutions(0)[1]));
			if(image.getNDims() > 2)
				closedTag("zr", Float.toString(image.getResolutions(0)[2]));
			if(image.getNDims() > 3)
				closedTag("tr", Float.toString(image.getResolutions(0)[3]));
			openTag("resolution", false);

			closedTag("orientation", FileInfoBase.getImageOrientationStr(image.getImageOrientation()));
			closedTag("modality", FileInfoBase.getModalityStr(image.getImageModality()));
			closedTag("file-format", FileUtility.getFileTypeStr(image.getFileInfo()[0].getFileFormat()));
			closedTag("data-type", ModelStorageBase.getBufferTypeStr(image.getFileInfo()[0].getDataType()));
			closedTag("anatomical-area", textFieldsMap.get("anatomical-area"));
			openTag("image-attr", false);
		}
		
		private void writeDatasetFiles() {
			openTag("dataset-files", true);
			Vector<XMLAttributes> att = new Vector<XMLAttributes>();
			if(isDir) {
				att.add(new XMLAttributes("path", image.getImageDirectory()));
				if(pattern != null)
					att.add(new XMLAttributes("pattern", pattern));
				closedTag("fileset", att);
			} else {
				if(image.getImageDirectory() != null) {
					att.add(new XMLAttributes("path", image.getImageDirectory()));
				}
				att.add(new XMLAttributes("name", image.getImageFileName()));
				closedTag("file", att);
			}
			openTag("dataset-files", false);
		}
		
		private void writeProvenance() {
			Calendar cal = Calendar.getInstance();
			DecimalFormat dec = new DecimalFormat("00");
			//uses 0 for January
			String dateStr = cal.get(Calendar.YEAR)+"-"+dec.format(cal.get(Calendar.MONTH)+1)+"-"+dec.format(cal.get(Calendar.DAY_OF_MONTH));
			openTag("provenance", true);
			closedTag("anonymized-by", System.getProperty("user.name"));
			closedTag("date-added", dateStr);
			closedTag("source-name", textFieldsMap.get("source-name"));
			closedTag("source-org", textFieldsMap.get("source-org"));
			closedTag("source-project", textFieldsMap.get("source-project"));
			openTag("provenance", false);
		}
		
		private void writeExtInfo() throws IOException {
			openTag("testing-information", true);
			bw.write(textFieldsMap.get("testing-information"));
			bw.newLine();
			openTag("testing-information", false);
			
			openTag("details", true);
			bw.write(textFieldsMap.get("details"));
			bw.newLine();
			openTag("details", false);
		}
		
	}
	

}
